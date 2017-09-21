unit Ugpu; // SpyroTAS is licensed under WTFPL

interface

uses
  Windows, SysUtils, Forms, Classes, Utas;

const
  GpuCommand_none = 0;
  GpuCommand_updateLace = 1;
  GpuCommand_dmaChain = 2;
  GpuCommand_freeze = 3;
  GpuCommand_writeDataMem = 4;
  GpuCommand_writeData = 5;
  GpuCommand_writeStatus = 6;
  GpuCommand_setMode = 7;

type
  TGpuCommand = Integer;

procedure GpuRecord(Start: Boolean);

procedure DumpGpuCommand(Command: TGpuCommand; A: Integer = 0; B: Integer = 0);

function GpuReplay(Start: Boolean): Boolean;

function GpuPlayFrame(Back: Boolean = False): Boolean;

procedure GpuMovieDelete();

var
  IsGpuRecording: Boolean = False;
  IsGpuPlaying: Boolean = False;
  GpuMovieLimit: Integer = 16 * 1024 * 1024;

implementation

uses
  Uroute, Umisc, UFover, Uglob, Umain;

type
  PGpuBufferData = ^TGpuBufferData;

  TGpuBufferData = record
    X, A, B, C: Integer;
  end;

const
  MaxTrustedSize = 1 * 1024 * 1024;
  SizeOfGpuBuffer = MaxTrustedSize + 1024 * 1024 * 2;

const
  SizeOfVideoStruct = (1024 * 512 * 2) + (256 + 2) * 4;

var
  GpuBuffer: PChar = nil;
  GpuCompress: PChar = nil;
  GpuBufferPosition: Integer = 0;
  GpuBufferSize: Integer = 0;
  GpuMovie: TFileStream = nil;
  GpuCopyRam: PChar = nil;
  GpuCopyVideoStruct: PChar = nil;
  LastRamTop: Integer = 0;
  LaceList: TList = nil;
  LaceListIndex: Integer = 0;
  TempGpuStruct: Pointer = nil;
  HaveLace: Boolean;
  MovieList: TList = nil;
  MovieListIndex: Integer;
  MovieListFile: Integer;
  MovieListSize: Integer;

function GetNameOfMovie(Index: Pointer = nil): string;
begin
  if Index = nil then
    Result := PathToSpyroTAS + SpyroTASForHistory + 'tmp_gpu_*.tmp'
  else
    Result := PathToSpyroTAS + SpyroTASForHistory + 'tmp_gpu_' + IntToStr(Integer
      (Index)) + '.tmp';
end;

function SmartCompare(Old, Current, Target: Pinteger; Size: Integer = 0): Integer;
var
  Similar, Different, This: Integer;
  Meta: PInteger;
label
  Next;
begin
  if Old = nil then
  begin   {
    Size := Target^;
    Result := Size + 4;
    if Size = 0 then
      Exit;
    Size := (Size shr 2);
    Last := Target;
    Inc(Target, Size);
    Inc(Current, Size);
    repeat
      Dec(Current);
      Current^ := Current^ xor Target^;
      Dec(Target);
    until Target = Last;
    Exit;
    }
    Result := Target^;
    Inc(Target);
    Move(Target^, Current^, Result);
    Inc(Result, 4);
  end
  else
  begin    {
    Target^ := Size;
    Result := Size + 4;
    if Size = 0 then
      Exit;
    Size := (Size shr 2);
    Last := Target;
    Inc(Target, Size);
    Inc(Current, Size);
    Inc(Old, Size);
    repeat
      Dec(Current);
      Dec(Old);
      Target^ := Current^ xor Old^;
      Old^ := Current^;
      Dec(Target);
    until Target = Last;
    Exit;  }
    Target^ := Size;
    Inc(Target);
    Move(Current^, Target^, Size);
    Result := Size + 4;
  end;
  Exit;

  Result := 4;
  if Old = nil then
  begin
    while Target^ <> 0 do
    begin
      Similar := Target^ shr 16;
      Different := Target^ and $ffff;
      Inc(Target);
      Inc(Current, Similar);
      Inc(Result, (Different shl 2) + 4);
      Move(Target^, Current^, Different shl 2);
      Inc(Target, Different);
      Inc(Current, Different);
    end;
  end
  else
  begin
    while Size > 0 do
    begin
      Similar := 0;
      Different := 0;
      Meta := Target;
      Inc(Target);
      Inc(Result, 4);
      while Size > 0 do
      begin
        This := Current^;
        if Old^ = This then
          Inc(Similar)
        else
          goto Next;
        Inc(Old);
        Inc(Current);
        Dec(Size, 4);
        if Similar = $ffff then
          Break;
      end;
      while Size > 0 do
      begin
        This := Current^;
        if Old^ <> This then
        begin
Next:
          Old^ := This;
          Target^ := This;
          Inc(Different);
          Inc(Target);
          Inc(Result, 4);
        end
        else
          Break;
        Inc(Old);
        Inc(Current);
        Dec(Size, 4);
        if Different = $ffff then
          Break;
      end;
      Meta^ := Different or (Similar shl 16);
    end;
  end;
  Target^ := 0;
end;

function FlushBuffer(Back: Boolean = False): Boolean;
var
  Size: Cardinal;
  Name: string;
begin
  Result := True;
  if IsGpuRecording then
  begin
    if GpuBufferPosition = 0 then
      Exit;
    Size := GpuBufferPosition;
    GpuMovie.WriteBuffer(Size, 4);
    GpuMovie.WriteBuffer(GpuBuffer^, Size);
    GpuMovie.WriteBuffer(Size, 4);
    GpuBufferPosition := 0;
    if (GpuMovie.Size > (GpuMovieLimit shr 3)) and (GpuMovie.Size > 1024 * 1024) then
    begin
      Inc(MovieListSize, GpuMovie.Size);
      GpuMovie.Free();
      Inc(MovieListFile);
      if (MovieListSize > GpuMovieLimit) and (MovieList.Count > 4) then
      begin
        Name := GetNameOfMovie(MovieList[0]);
        GpuMovie := TFileStream.Create(Name, fmOpenRead or fmShareDenyNone);
        Dec(MovieListSize, GpuMovie.Size);
        GpuMovie.Free();
        DeleteFile(Name);
        MovieList.Delete(0);
      end;
      MovieListIndex := MovieList.Count;
      MovieList.Add(Pointer(MovieListFile));
      GpuMovie := TFileStream.Create(GetNameOfMovie(MovieList[MovieListIndex]), fmCreate);
    end;
  end;
  if IsGpuPlaying then
    while True do
    begin
      if Back then
      begin
        if GpuMovie.Position <= 4 then
        begin
          if (MovieListIndex <= 0) then
          begin
            Inc(LaceListIndex);
            GpuMovie.Seek(4, soFromBeginning);
            Exit;
          end;
          Dec(MovieListIndex);
          GpuMovie.Free();
          GpuMovie := TFileStream.Create(GetNameOfMovie(MovieList[MovieListIndex]),
            fmOpenRead or fmShareDenyWrite);
          GpuMovie.Seek(0, soFromEnd);
          Continue;
        end;
        if GpuMovie.Position + 4 >= GpuMovie.Size then
          GpuMovie.Seek(-4, soFromEnd)
        else
          GpuMovie.Seek(-8, soFromCurrent);
        GpuMovie.ReadBuffer(GpuBufferSize, 4);
        GpuMovie.Seek(-GpuBufferSize - 4, soFromCurrent);
        GpuMovie.ReadBuffer(GpuBuffer^, GpuBufferSize);
        GpuMovie.Seek(-GpuBufferSize, soFromCurrent);
      end
      else
      begin
        if (GpuMovie.Position >= 4) and (GpuMovie.Position + 4 < GpuMovie.Size) then
        begin
          GpuMovie.Seek(-4, soFromCurrent);
          GpuMovie.ReadBuffer(GpuBufferSize, 4);
          GpuMovie.Seek(GpuBufferSize + 4, soFromCurrent);
        end;
        if GpuMovie.Position + 4 >= GpuMovie.Size then
        begin
          if (MovieListIndex >= MovieList.Count - 1) then
          begin
            Dec(LaceListIndex);
            if GpuMovie.Size > 4 then
            begin
              GpuMovie.Seek(-4, soFromEnd);
              GpuMovie.ReadBuffer(GpuBufferSize, 4);
              GpuMovie.Seek(-GpuBufferSize - 4, soFromCurrent);
            end;
            Exit;
          end;
          Inc(MovieListIndex);
          GpuMovie.Free();
          GpuMovie := TFileStream.Create(GetNameOfMovie(MovieList[MovieListIndex]),
            fmOpenRead or fmShareDenyWrite);
          Continue;
        end;
        GpuMovie.ReadBuffer(GpuBufferSize, 4);
        GpuMovie.ReadBuffer(GpuBuffer^, GpuBufferSize);
        GpuMovie.Seek(-GpuBufferSize, soFromCurrent);
      end;
      LaceList.Clear();
      GpuBufferPosition := 0;
      while GpuBufferPosition < GpuBufferSize do
      begin
        if TGpuCommand(GpuBuffer[GpuBufferPosition + 3]) = GpuCommand_updateLace then
          LaceList.Add(Pointer(GpuBufferPosition));
        Inc(GpuBufferPosition, PInteger(Pointer(GpuBuffer + GpuBufferPosition))^
          and $ffffff);
      end;
      if LaceList.Count < 1 then
        Abort;
      if Back then
        LaceListIndex := LaceList.Count - 1
      else
        LaceListIndex := 0;
      Break;
    end;
  Result := False;
end;

function ProcessDma(Base: PChar; Offset: Integer; Target: PInteger; DoSave:
  Boolean): Integer;
var
  Size: Integer;
begin
  Result := 0;
  repeat
    if DoSave then
    begin
      Size := (Ord(Base[Offset + 3]) shl 2) + 4;
      if Offset + Size >= SizeOfRAM then
        Abort;
      Size := SmartCompare(Pointer(GpuCopyRam + Offset), Pointer(Base + Offset),
        Target, Size)
    end
    else
      Size := SmartCompare(nil, Pointer(Base + Offset), Target);

    Inc(Target, Size shr 2);
    Inc(Result, Size);
    Offset := PInteger(Base + Offset)^ and $ffffff;
    if Offset > SizeOfRam then
      Break;
  until Offset = $ffffff;
end;

function GpuPlayFrame(Back: Boolean = False): Boolean;
var
  BufferData: PGpuBufferData;
begin
  Result := True;
  if not IsGpuPlaying then
    Exit;
  if Back then
  begin
    Dec(LaceListIndex);
    if LaceListIndex < 0 then
      if FlushBuffer(True) then
        Exit;
  end
  else
  begin
    Inc(LaceListIndex);
    if LaceListIndex >= LaceList.Count then
      if FlushBuffer(False) then
        Exit;
  end;
  Result := False;
  HaveLace := False;
  if (LaceListIndex < 0) or (LaceList.Count < 0) then
    Abort;
  GpuBufferPosition := Integer(LaceList[LaceListIndex]);
  while True do
  begin
    if GpuBufferPosition >= GpuBufferSize then
      Break;
    BufferData := @GpuBuffer[GpuBufferPosition];
    Inc(GpuBufferPosition, BufferData.X and $ffffff);
    case TGpuCommand(BufferData.X shr 24) of
      GpuCommand_updateLace:
        begin
          if HaveLace then
          begin
            Exit;
          end;
          if BufferData.B <> -2 then
          begin
            VOID(GPUupdateLace_)();
            if (BufferData.B > 0) and (BufferData.B < History.GetSize()) then
            begin
              HistoryPosition := BufferData.B;
              RequestOverlayRedraw(BufferData.A, HistoryPosition, History.Read(HistoryRrec,
                HistoryPosition));
            end;
          end;
          HaveLace := True;
        end;
      GpuCommand_dmaChain:
        begin
          ProcessDma(PointerToRamStart, BufferData.A, @BufferData.B, False);
          GPUdmaChain(PointerToRamStart, BufferData.A);
        end;
      GpuCommand_freeze:
        begin
          SmartCompare(nil, Pointer(GpuCopyVideoStruct), @BufferData.A);
          GPUfreeze(0, Integer(GpuCopyVideoStruct));
        end;
      GpuCommand_writeDataMem:
        begin
          SmartCompare(nil, Pointer(PointerToRamStart + BufferData.A), @BufferData.C);
          GPUwriteDataMem(Integer(PointerToRamStart + BufferData.A), BufferData.B);
        end;
      GpuCommand_writeData:
        begin
          GPUwriteData(BufferData.A);
        end;
      GpuCommand_writeStatus:
        begin
          GPUwriteStatus(BufferData.A);
        end;
      GpuCommand_setMode:
        begin
          GPUsetMode(BufferData.A);
        end;
    end;
  end;
end;

procedure GpuRecord(Start: Boolean);
var
  Name: string;
begin
  if IsRunDLL then
    Exit;
  try
    if Start then
    begin
      if IsGpuRecording then
        Exit;
      IsGpuRecording := True;
      if MovieList = nil then
      begin
        MovieList := TList.Create();
        MovieListFile := 1;
        MovieList.Add(Pointer(MovieListFile));
        MovieListIndex := 0;
        MovieListSize := 0;
      end
      else
      begin
        MovieListIndex := MovieList.Count - 1;
        MovieListFile := Integer(MovieList[MovieListIndex]);
      end;
      Name := GetNameOfMovie(MovieList[MovieListIndex]);
      GetMem(GpuBuffer, SizeOfGpuBuffer);
      GetMem(GpuCompress, SizeOfGpuBuffer);
      GetMem(GpuCopyRam, SizeOfRAM);
      GetMem(GpuCopyVideoStruct, SizeOfVideoStruct);
      ZeroMemory(GpuCopyRam, SizeOfRAM);
      ZeroMemory(GpuCopyVideoStruct, SizeOfVideoStruct);
      if not FileExists(Name) then
        GpuMovie := TFileStream.Create(Name, fmCreate)
      else
        GpuMovie := TFileStream.Create(Name, fmOpenReadWrite or fmShareDenyWrite);
      GpuMovie.Seek(0, soFromEnd);
      GpuBufferPosition := 0;
      DumpGpuCommand(GpuCommand_updateLace, 0, -2);
      HaveLace := True;
                               {
    GetMem(TempGpuStruct, SizeOfVideoStruct);
    PInteger(TempGpuStruct)^ := 1;
    GPUfreeze(1, Integer(TempGpuStruct));
    DumpGpuCommand(GpuCommand_freeze, 0, Integer(TempGpuStruct));
    FreeMem(TempGpuStruct); }
    end
    else
    begin
      if not IsGpuRecording then
        Exit;
      try
        if AllowWarp then
        begin
          DumpGpuCommand(GpuCommand_updateLace, 0, -2);
          FlushBuffer();
        end;
      finally
        FreeAndNil(GpuMovie);
        FreeMem(GpuBuffer);
        FreeMem(GpuCompress);
        FreeMem(GpuCopyRam);
        FreeMem(GpuCopyVideoStruct);
        if MovieList.Count <= 0 then
          GpuMovieDelete();
        IsGpuRecording := False;
      end;
    end;
  except
    on E: Exception do
      Report(SpyroTASName + ' GPU movie record error:', E.Message);
  end;
end;

var
  Search: TSearchRec;

procedure GpuMovieDelete();
var
  Name: string;
begin
  if IsGpuRecording or IsGpuPlaying then
    Exit;
  FreeAndNil(MovieList);
  MovieListIndex := 0;
  MovieListFile := 0;
  MovieListSize := 0;
  Name := GetNameOfMovie();
  if FindFirst(Name, faAnyFile, Search) = 0 then
  begin
    Name := PathFixBackslash(ExtractFilePath((Name)));
    repeat
      DeleteFile(Name + Search.Name);
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
end;

function GpuReplay(Start: Boolean): Boolean;
begin
  Result := False;
  if IsGpuRecording then
    Exit;
  if IsRunDLL then
    Exit;
  if MovieList = nil then
    Exit;
  try
    if Start then
    begin
      if IsGpuPlaying then
        Exit;
      IsGpuPlaying := True;
      GetMem(GpuBuffer, SizeOfGpuBuffer);
      GetMem(GpuCompress, SizeOfGpuBuffer);
      GetMem(GpuCopyRam, SizeOfRAM);
      GetMem(GpuCopyVideoStruct, SizeOfVideoStruct);
      GetMem(TempGpuStruct, SizeOfVideoStruct);
      PInteger(TempGpuStruct)^ := 1;
      GPUfreeze(1, Integer(TempGpuStruct));
      Move(PointerToRamStart^, GpuCopyRam^, SizeOfRAM);
      ZeroMemory(PointerToRamStart, SizeOfRAM);
      ZeroMemory(GpuCopyVideoStruct, SizeOfVideoStruct);
      LaceList := TList.Create();
      GpuBufferPosition := 0;
      GpuBufferSize := 0;
      LaceListIndex := 0;
//      RequestOverlayRedraw(0, 0, 0);
      GpuMovie := TFileStream.Create(GetNameOfMovie(MovieList[MovieListIndex]),
        fmOpenRead or fmShareDenyNone);
      GpuMovie.Seek(0, soFromEnd);
    end
    else
    begin
      if not IsGpuPlaying then
        Exit;
      FreeAndNil(LaceList);
      Move(GpuCopyRam^, PointerToRamStart^, SizeOfRAM);
      GPUfreeze(0, Integer(TempGpuStruct));
      FreeMem(GpuBuffer);
      FreeMem(GpuCompress);
      FreeMem(GpuCopyRam);
      FreeMem(GpuCopyVideoStruct);
      FreeMem(TempGpuStruct);
      FreeAndNil(GpuMovie);
//      RequestOverlayRedraw(0, 0, 0);
      IsGpuPlaying := False;
    end;
  except
    on E: Exception do
      Report(SpyroTASName + ' GPU movie replay error:', E.Message);
  end;
  Result := True;
end;

procedure DumpGpuCommand(Command: TGpuCommand; A: Integer = 0; B: Integer = 0);
var
  BufferData: PGpuBufferData;
  Size: Integer;
begin
  if not IsGpuRecording then
    Exit;
  CWPush(); // careful below
  try
    if not AllowWarp then
    begin
      GpuRecord(False);
      CWPop(); //
      Exit;
    end;
    Size := -1;
    BufferData := @GpuBuffer[GpuBufferPosition];
    case Command of
      GpuCommand_updateLace:
        begin
          if HaveLace then
          begin
            HaveLace := False;
            CWPop(); //
            Exit;
          end;
          Size := 8;
          BufferData.A := A;
          BufferData.B := B;
          if (GpuBufferPosition > MaxTrustedSize) then
            FlushBuffer();
          BufferData := @GpuBuffer[GpuBufferPosition];
        end;
      GpuCommand_dmaChain:
        begin
          LastRamTop := A;
          BufferData.A := B;
          Size := 4 + ProcessDma(Pointer(A), B, @BufferData.B, True);
        end;
      GpuCommand_freeze:
        begin
          if A = 0 then
          begin
            Size := SmartCompare(Pointer(GpuCopyVideoStruct), Pointer(B), @BufferData.A,
              SizeOfVideoStruct);
          end;
        end;
      GpuCommand_writeDataMem:
        begin
          if LastRamTop <> 0 then
          begin
            BufferData.A := A - LastRamTop;
            BufferData.B := B;
            Size := 8 + SmartCompare(Pointer(GpuCopyRam + BufferData.A), Pointer
              (A), @BufferData.C, B shl 2);
          end;
        end;
      GpuCommand_writeData, GpuCommand_writeStatus, GpuCommand_setMode:
        begin
          Size := 4;
          BufferData.A := A;
        end;
    end;
    if Size >= 0 then
    begin
      Inc(Size, 4);
      BufferData.X := Size or (Command shl 24);
      Inc(GpuBufferPosition, Size);
    end;
  except
    on E: Exception do
    begin
      Report(SpyroTASName + ' GPU movie record error:', E.Message);
      GpuRecord(False);
    end;
  end;
  CWPop();
end;

end.

// EOF


