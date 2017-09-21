unit Umisc; // SpyroTAS is licensed under WTFPL

interface

uses
  Windows, Forms, Classes, SysUtils, Graphics, Math;

  // TCustomIntegerArray class:
type
  TCustomIntegerArray = class(TObject)
  public
    procedure SetSize(NewSize: Integer);
    function GetSize(): Integer;
    procedure ClearFrom(Index: Integer);
    procedure Write(Index, Value: Integer);
    function Read(Index: Integer): Integer;
    function Addr(Index: Integer = 0): PInteger;
    procedure Push(Value: Integer);
  private
    Data: array of Integer; // managed type, no need in destructor
    Count: Integer; // user assigned
    Size: Integer; // internal allocated
    procedure RaiseError(Index: Integer);
  end;

type
  TCustomIntegerArrays = class(TObject)
    constructor Create(Count: Integer);
    destructor Destroy(); override;
  public
    function GetDim(): Integer;
    procedure SetSize(NewSize: Integer);
    function GetSize(): Integer;
    procedure ClearFrom(Index: Integer);
    procedure Write(Arr, Index, Value: Integer);
    function Read(Arr, Index: Integer): Integer;
    function Addr(Arr: Integer; Index: Integer = 0): PInteger;
    procedure Assign(Other: TCustomIntegerArrays);
    procedure FromStream(Arr: Integer; Values: Integer; Stream: TStream; NoFatal:
      Boolean = False);
    procedure ToStream(Arr: Integer; Stream: TStream; Last: Integer);
  private
    procedure RaiseError(Arr, Index: Integer);
  private
    Data: array of TCustomIntegerArray;
    Size: Integer;
    Count: Integer;
  end;

function Ignores(const a): Pointer; overload;

function Ignores(const a; const b): Pointer; overload;

function Ignores(const a; const b; const c): Pointer; overload;

function Ignores(const a; const b; const c; const d): Pointer; overload;

function Initializes(out a): Pointer;

procedure Report(Caption, Text: string; Hand: HWND); overload;

procedure Report(Caption, Text: string; Form: TForm = nil); overload;

function FormatNumberWithZero(Value: Integer; StrLength: Integer; Zero: Char =
  '0'): string;

function PathFixBackslash(const Path: string): string;

function HashCRC(DataPtr: Pointer; DataSize: Integer): Integer;

function TimeToString(Time: TDateTime): string;

function IntegerToBase26(Number, Size: Integer): string;

function PluginTitleFromNameAndVersion(Name: string; version: Integer): string;

function SquaredDistance(X, Y: Integer; Z: Integer = 0): Real;

function SwapWords(Value: Integer): Integer;

procedure BitmapDataAndSize(Bitmap: TBitmap; out Data: PChar; out Size: Integer);

function AnyBaseToInt(Text: string; Base: Integer): Int64;

function StringToRamOffset(Text: string): Integer;

function IsOurWindow(Wind: HWND): Boolean;

function IsAbsolutePath(FileName: string): Boolean;

procedure SaveBitmapToPng(Bitmap: TBitmap; PngFilename: string; Compression: Integer);

procedure LoadPngAsBitmap(Bitmap: TBitmap; PngFilename: string);

function ArrayCalc(Start: PInteger; Size: Integer; out Dev: Double): Double;

function HashFile(Path: string): Integer;

procedure CryptData(Start: PInteger; Words: Integer; Key: Integer; UseShift: Boolean);

function Print8087CW(): string;

{$IFDEF SpyroTAS_no_Libs}
const
  PngIsAvailable = False;
{$ELSE}

const
  PngIsAvailable = True;
{$ENDIF}

implementation

uses
  DateUtils, StrUtils, Uglob
  {$IFNDEF SpyroTAS_no_Libs}
    , PNGImage
   {$ENDIF}
;

// one entry point for errors:
procedure TCustomIntegerArray.RaiseError(Index: Integer);
begin
  raise Exception.Create('TCustomIntegerArray wrong index (' + IntToStr(Index) + ')');
end;

// zerofill from target index till available memory:
procedure TCustomIntegerArray.ClearFrom(Index: Integer);
begin
  if Size <= 0 then // nothing to clear
    Exit;
  if (Index < 0) or (Index > Size) then // if called with wrong index
    RaiseError(Index);
  FillChar(Addr(Index)^, (Size - Index) * SizeOf(Integer), 0); // get the tail
end;

// change length of array;
procedure TCustomIntegerArray.SetSize(NewSize: Integer);
const
  MaxSize: Integer = $0FFFFFFF; // hard limit ~1Gb
var
  OldCount: Integer;
begin
  if NewSize < 0 then // protection
    RaiseError(NewSize);
  if NewSize > MaxSize then
    NewSize := MaxSize;
  OldCount := Count; // don't forget old
  Size := (NewSize + 16) * 2; // add overhead
  SetLength(Data, Size + 64); // resize
  Count := NewSize;
  if OldCount < Size then // if became larger
    ClearFrom(OldCount); // clear new memory
end;

// simple getter or length:
function TCustomIntegerArray.GetSize(): Integer;
begin
  Result := Count; // public Size, not internal
end;

// set new value, but array will grow automatically if next index was pushed:
procedure TCustomIntegerArray.Write(Index, Value: Integer);
begin
  if (Index < 0) or (Index > Count) then
    RaiseError(Index); // don't allow to write at arbitrary index, only sequental
  if Index >= Size then
    SetSize(Index); // resize memory
  if Index = Count then
    Inc(Count); // increment user length
  Data[Index] := Value;
end;

// get from array:
function TCustomIntegerArray.Read(Index: Integer): Integer;
begin
  if (Index < 0) or (Index > Count) then
    RaiseError(Index); // don't allow resizing at reads
  Result := Data[Index];
end;

// retrieve a pointer to raw data:
function TCustomIntegerArray.Addr(Index: Integer = 0): PInteger;
begin
  if (Index < 0) or (Index > Size) then
    RaiseError(Index); // like in Read()
  Result := PInteger(@Data[Index]);
end;

procedure TCustomIntegerArray.Push(Value: Integer);
begin
  Write(Count, Value);
end;


// for ignoring arguments, to make Lazarus silent:
function Ignores(const a): Pointer; overload;
begin
  Result := @a;
end;

function Ignores(const a; const b): Pointer; overload;
begin
  Ignores(a);
  Result := Ignores(b);
end;

function Ignores(const a; const b; const c): Pointer; overload;
begin
  Ignores(a, b);
  Result := Ignores(c);
end;

function Ignores(const a; const b; const c; const d): Pointer; overload;
begin
  Ignores(a, b, c);
  Result := Ignores(d);
end;

function Initializes(out a): Pointer;
begin
  Result := @a;
end;

//TODO
procedure Report(Caption, Text: string; Hand: HWND); overload;
begin
  if GuiClosing then
    Exit;
  Caption := Caption + #0#0#0#0;
  Text := Text + #0#0#0#0;
  MessageBox(Hand, PChar(Text), PChar(Caption), MB_SYSTEMMODAL);
end;

procedure Report(Caption, Text: string; Form: TForm = nil); overload;
begin
  if Form = nil then
    Report(Caption, Text, 0)
  else
    Report(Caption, Text, Form.Handle);
end;

// get hash of data, Size must be dword-aligned:
function HashCRC(DataPtr: Pointer; DataSize: Integer): Integer;
const
  CRC32Table: array[0..255] of Cardinal = ($00000000, $77073096, $ee0e612c,
    $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3, $0edb8832, $79dcb8a4,
    $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91, $1db71064,
    $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
    $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63,
    $8d080df5, $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447,
    $d20d85fd, $a50ab56b, $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3,
    $45df5c75, $dcd60dcf, $abd13d59, $26d930ac, $51de003a, $c8d75180, $bfd06116,
    $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f, $2802b89e, $5f058808, $c60cd9b2,
    $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d, $76dc4190, $01db7106,
    $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433, $7807c9a2,
    $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
    $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1,
    $f50fc457, $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49,
    $8cd37cf3, $fbd44c65, $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541,
    $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a, $346ed9fc, $ad678846, $da60b8d0,
    $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9, $5005713c, $270241aa, $be0b1010,
    $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f, $5edef90e, $29d9c998,
    $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad, $edb88320,
    $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
    $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27,
    $7d079eb1, $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb,
    $196c3671, $6e6b06e7, $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f,
    $8ebeeff9, $17b7be43, $60b08ed5, $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252,
    $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b, $d80d2bda, $af0a1b4c, $36034af6,
    $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79, $cb61b38c, $bc66831a,
    $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f, $c5ba3bbe,
    $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
    $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785,
    $05005713, $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d,
    $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd,
    $f6b9265b, $6fb077e1, $18b74777, $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
    $8f659eff, $f862ae69, $616bffd3, $166ccf45, $a00ae278, $d70dd2ee, $4e048354,
    $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db, $aed16a4a, $d9d65adc,
    $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9, $bdbdf21c,
    $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
    $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b,
    $2d02ef8d);
var
  EndOfData, Source: PChar;
  Value: Integer;
begin
  Result := -1; // $FFFFFFFF
  Source := PChar(DataPtr);
  EndOfData := Source + (DataSize and not 3); // mod 4
  while Source < EndOfData do
  begin
    Value := PInteger(Source)^; // do for each byte:
    Result := (Result shr 8) xor Integer(CRC32Table[(Result xor (Value and 255))
      and $FF]);
    Value := Value shr 8;
    Result := (Result shr 8) xor Integer(CRC32Table[(Result xor (Value and 255))
      and $FF]);
    Value := Value shr 8;
    Result := (Result shr 8) xor Integer(CRC32Table[(Result xor (Value and 255))
      and $FF]);
    Value := Value shr 8;
    Result := (Result shr 8) xor Integer(CRC32Table[(Result xor (Value and 255))
      and $FF]);
    Inc(Source, 4);
  end;
  Result := Result xor  - 1;
end;

// represent a number as a letter string:
function IntegerToBase26(Number, Size: Integer): string;
var
  Modulo: Integer;
begin
  Result := StringOfChar('A', Size); // pad "zeroes" from left
  repeat
    if Size = 0 then // need more space
    begin
      Size := 1;
      Result := '_' + Result; // one digit
    end;
    Modulo := Number mod 26;
    Result[Size] := Chr(Ord('A') + Modulo); // letter
    Number := Number div 26;
    Dec(Size); // next position
  until Number = 0;
end;

// encode timestamp to alphanumeric:
function TimeToString(Time: TDateTime): string;
begin
  Result := IntegerToBase26(DateTimeToUnix(Time) - DateTimeToUnix(EncodeDate(2017,
    1, 1)), 6); // count from 2017 year to get shorter strings
end;

// $12345678 -> $56781234

function SwapWords(Value: Integer): Integer;
begin
  Result := (Value shl 16) or (Value shr 16);
end;

// returns 2D or 3D distance without sqrt():
function SquaredDistance(X, Y: Integer; Z: Integer = 0): Real;
begin
  Result := X * X + Y * Y + Z * Z; // use Real to ensure not overflow
end;

// pad a value with zeroes from left to desired length
function FormatNumberWithZero(Value: Integer; StrLength: Integer; Zero: Char =
  '0'): string;
begin
  Result := RightStr(StringOfChar(Zero, StrLength) + IntToStr(Value), StrLength);
end;

// name in ePSXe's manner:
function PluginTitleFromNameAndVersion(Name: string; version: Integer): string;
begin
  Result := Name + ' ' + IntToStr(((version shr 8) and 255)) + '.' + IntToStr((version
    and 255));
end;

// fixes a directory name to always have "\" at the end:
// ( also replaces "/", "\\" and "\.\" to "\")
function PathFixBackslash(const Path: string): string;
var
  Share: Boolean;
begin
  Share := False;
  Result := Trim(StringReplace(Path, '/', '\', [rfReplaceAll]));
  while (Result <> '') and (Result[Length(Result)] = '\') do
    Delete(Result, Length(Result), 1);
  if Result = '' then
    Exit;
  Result := Result + '\';
  if Result[1] = '\' then
  begin
    Share := True;
    Delete(Result, 1, 1);
  end;
  Result := StringReplace(Result, '\.\', '\', [rfReplaceAll]);
  Result := StringReplace(Result, '\\', '\', [rfReplaceAll]);
  if Share then
    Result := '\' + Result;
end;

procedure BitmapDataAndSize(Bitmap: TBitmap; out Data: PChar; out Size: Integer);
var
  A0, A1, Ah: PChar;
  H: Integer;
begin
  Data := nil;
  Size := 0;
  if (Bitmap = nil) or (Bitmap.Height < 2) then
    Exit;
  H := Bitmap.Height;
  A0 := Bitmap.{%H-}ScanLine[0];
  A1 := Bitmap.{%H-}ScanLine[1];
  Ah := Bitmap.{%H-}ScanLine[H - 1];
  if A0 < Ah then
  begin
    Data := A0;
    Size := (A1 - A0) * H;
  end
  else
  begin
    Data := Ah;
    Size := (A0 - A1) * H;
  end;
end;

function AnyBaseToInt(Text: string; Base: Integer): Int64;
var
  Index, Value: Integer;
begin
  Result := 0;
  for Index := 1 to Length(Text) do
  begin
    Value := Ord(Text[Index]) - Ord('0');
    if Value > 9 then
      Value := 10 + Ord(Text[Index]) - Ord('A');
    if (Value >= 0) and (Value < Base) then
      Result := Result * Base + Value
    else
    begin
      Result := -1;
      Exit;
    end;
  end;
end;

function StringToRamOffset(Text: string): Integer;
var
  Line: string;
  Size, Index: Integer;
  Ch: Byte;
begin
  Result := -1;
  Text := UpperCase(Trim(Text));
  SetLength(Line, Length(Text));
  Size := 0;
  for Index := 1 to Length(Text) do
  begin
    Ch := Ord(Text[Index]);
    if (Ch >= Ord('A')) and (Ch <= Ord('Z')) or ((Ch >= Ord('0')) and (Ch <= Ord
      ('9'))) then
    begin
      Inc(Size);
      Line[Size] := Text[Index];
    end;
  end;
  if Size = 0 then
    Exit;
  if Line[1] = '0' then
    if Size > 1 then
      Delete(Line, 1, 1)
    else
    begin
      Result := 0;
      Exit;
    end;
  case Line[1] of
    'X':
      Result := AnyBaseToInt(Copy(Line, 2, Length(Line)), 16);
    'O':
      Result := AnyBaseToInt(Copy(Line, 2, Length(Line)), 8);
    'B':
      Result := AnyBaseToInt(Copy(Line, 2, Length(Line)), 2);
  else
    case Line[Length(Line)] of
      'H':
        Result := AnyBaseToInt(Copy(Line, 1, Length(Line) - 1), 16);
      'O':
        Result := AnyBaseToInt(Copy(Line, 1, Length(Line) - 1), 8);
      'B':
        Result := AnyBaseToInt(Copy(Line, 1, Length(Line) - 1), 2);
    else
      Result := AnyBaseToInt(Line, 10)
    end;
  end;
end;

function IsOurWindow(Wind: HWND): Boolean;
var
  Pid: Cardinal;
begin
  Result := False;
  if (Wind = 0) or (Wind = INVALID_HANDLE_VALUE) then
    Exit;
  if not IsWindow(Wind) then
    Exit;
  Pid := 0;
  GetWindowThreadProcessId(Wind, Pid);
  Result := (Pid = GetCurrentProcessId());
end;

function IsAbsolutePath(FileName: string): Boolean;
begin
  Result := False;
  FileName := PathFixBackslash(FileName);
  if Length(FileName) < 3 then
    Exit;
  if (FileName[2] = ':') or ((FileName[1] = '\') and (FileName[2] = '\')) then
    Result := True;
end;

{$IFDEF SpyroTAS_no_Libs}
procedure SaveBitmapToPng(Bitmap: TBitmap; PngFilename: string; Compression: Integer);
begin
  Ignores(Bitmap, PngFilename, Compression);
end;

procedure LoadPngAsBitmap(Bitmap: TBitmap; PngFilename: string);
begin
  Ignores(Bitmap, PngFilename);
end;
{$ELSE}

procedure SaveBitmapToPng(Bitmap: TBitmap; PngFilename: string; Compression: Integer);
var
  Png: TPNGObject;
begin
  DeleteFile(PngFilename);
  Png := TPNGObject.Create{%H-};
  Png.Assign(Bitmap);
  Png.CompressionLevel := Compression;
  Png.SaveToFile(PngFilename);
  Png.Free();
end;

procedure LoadPngAsBitmap(Bitmap: TBitmap; PngFilename: string);
var
  Png: TPNGObject;
begin
  Png := TPNGObject.Create{%H-};
  Png.LoadFromFile(PngFilename);
  if (Png.Width mod 4) = 0 then // choose bit depth
    Bitmap.PixelFormat := pf24bit
  else
    Bitmap.PixelFormat := pf32bit;
  Bitmap.Assign(Png);
  Png.Free();
end;
{$ENDIF}

constructor TCustomIntegerArrays.Create(Count: Integer);
var
  Arr: Integer;
begin
  inherited Create();
  if Count > 0 then
  begin
    Size := Count;
    SetLength(Data, Size);
    Dec(Size);
    for Arr := 0 to Size do
      Data[Arr] := TCustomIntegerArray.Create();
    SetSize(1);
  end;
end;

destructor TCustomIntegerArrays.Destroy();
var
  Arr: Integer;
begin
  for Arr := 0 to Size do
    Data[Arr].Free();
end;

function TCustomIntegerArrays.GetDim(): Integer;
begin
  Result := Size + 1;
end;

procedure TCustomIntegerArrays.SetSize(NewSize: Integer);
var
  Arr: Integer;
begin
  Count := NewSize;
  for Arr := 0 to Size do
    Data[Arr].SetSize(NewSize);
end;

procedure TCustomIntegerArrays.RaiseError(Arr, Index: Integer);
begin
  raise Exception.Create('TCustomIntegerArrays wrong index (' + IntToStr(Arr) +
    ',' + IntToStr(Index) + ')');
end;

function TCustomIntegerArrays.GetSize(): Integer;
begin
  Result := Count;
end;

procedure TCustomIntegerArrays.ClearFrom(Index: Integer);
var
  Arr: Integer;
begin
  for Arr := 0 to Size do
    Data[Arr].ClearFrom(Index);
end;

procedure TCustomIntegerArrays.Write(Arr, Index, Value: Integer);
var
  I: Integer;
begin
  if (Arr < 0) or (Arr > Size) or (Index < 0) or (Index > Count) then
    RaiseError(Arr, Index);
  if Index = Count then
  begin
    Inc(Count);
    for I := 0 to Size do
      Data[I].Write(Index, 0);
  end;
  Data[Arr].Write(Index, Value);
end;

function TCustomIntegerArrays.Read(Arr, Index: Integer): Integer;
begin
  if (Arr < 0) or (Arr > Size) or (Index < 0) or (Index > Count) then
    RaiseError(Arr, Index);
  Result := Data[Arr].Read(Index);
end;

function TCustomIntegerArrays.Addr(Arr: Integer; Index: Integer = 0): PInteger;
begin
  if (Arr < 0) or (Arr > Size) then
    RaiseError(Arr, Index);
  Result := Data[Arr].Addr(Index);
end;

procedure TCustomIntegerArrays.Assign(Other: TCustomIntegerArrays);
var
  Arr, Cnt: Integer;
begin
  SetSize(0);
  Size := Other.GetDim();
  SetLength(Data, Size);
  if Size > 0 then
  begin
    Dec(Size);
    Cnt := Other.GetSize();
    SetSize(Cnt);
    if Cnt > 0 then
      for Arr := 0 to Size do
        CopyMemory(Addr(Arr), Other.Addr(Arr), Cnt * 4);
  end;
end;

procedure TCustomIntegerArrays.FromStream(Arr: Integer; Values: Integer; Stream:
  TStream; NoFatal: Boolean = False);
begin
  if (Arr < 0) or (Arr > Size) then
    RaiseError(Arr, 0);
  if NoFatal then
  begin
    Data[Arr].ClearFrom(0);
    Stream.Read(Data[Arr].Addr()^, Values * 4);
  end
  else
    Stream.ReadBuffer(Data[Arr].Addr()^, Values * 4);
end;

procedure TCustomIntegerArrays.ToStream(Arr: Integer; Stream: TStream; Last: Integer);
begin
  if (Arr < 0) or (Arr > Size) or (Last < 0) or (Last > Count) then
    RaiseError(Arr, Size);
  Stream.WriteBuffer(Data[Arr].Addr()^, Last * 4);
end;

function ArrayCalc(Start: PInteger; Size: Integer; out Dev: Double): Double;
var
  s, m: Integer;
  a: PInteger;
  r: Real;
begin
  Result := 0;
  if Size < 0 then
    Exit;
  s := Size;
  a := Start;
  m := 0;
  while s > 0 do
  begin
    Inc(m, a^);
    Inc(a);
    Dec(s);
  end;
  s := Size;
  a := Start;
  Result := m / s;
  Dev := 0;
  while s > 0 do
  begin
    r := a^ - Result;
    Dev := Dev + r * r;
    Inc(a);
    Dec(s);
  end;
  Dev := Sqrt(Dev / Size);
end;

function HashFile(Path: string): Integer;
var
  Stream: TFileStream;
  Size: Integer;
  Data: PChar;
begin
  Result := 0;
  if not FileExists(Path) then
    Exit;
  Stream := nil;
  Data := nil;
  try
    Stream := TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
    Size := Stream.Size;
    if (Size < 16) or (Size > 16 * 1024 * 1024) then
      Abort;
    GetMem(Data, Size + 4);
    ZeroMemory(Data, Size + 4);
    Stream.ReadBuffer(Data^, Size);
  except
    if Data = nil then
      FreeMem(Data);
    Stream.Free();
    Exit;
  end;
  Stream.Free();
  Result := HashCRC(Data, Size) + Size;
  if Result = 0 then
    Dec(Result, Size);
  FreeMem(Data);
end;

procedure CryptData(Start: PInteger; Words: Integer; Key: Integer; UseShift: Boolean);
var
  Seed: Integer;
begin
  if UseShift then
    Seed := Key
  else
    Seed := 0;
  while Words > 0 do
  begin
    if UseShift then
    begin
      Seed := Seed xor (Seed shl 13);
      Seed := Seed xor (Seed shr 17);
      Seed := Seed xor (Seed shl 5);
    end;
    Start^ := Start^ xor Seed xor Key;
    Inc(Start);
    Dec(Words);
  end;
end;

function Print8087CW(): string;
var
  m: TFPUExceptionMask;
  p: TFPUPrecisionMode;
  r: TFPURoundingMode;
begin
  Result := '8087CW(';
  m := GetExceptionMask();
  Result := Result + 'MASK:';
  if exInvalidOp in m then
    Result := Result + 'InvalidOp,';
  if exDenormalized in m then
    Result := Result + 'Denormalized,';
  if exZeroDivide in m then
    Result := Result + 'ZeroDivide,';
  if exOverflow in m then
    Result := Result + 'Overflow,';
  if exUnderflow in m then
    Result := Result + 'Underflow,';
  if exPrecision in m then
    Result := Result + 'Precision,';
  p := GetPrecisionMode();
  Result := Result + ' PREC:';
  case p of
    pmSingle:
      Result := Result + 'Single';
    pmReserved:
      Result := Result + 'Reserved';
    pmDouble:
      Result := Result + 'Double';
    pmExtended:
      Result := Result + 'Extended';
  end;
  r := GetRoundMode();
  Result := Result + ', ROUND:';
  case r of
    rmNearest:
      Result := Result + 'Nearest';
    rmDown:
      Result := Result + 'Down';
    rmUp:
      Result := Result + 'Up';
    rmTruncate:
      Result := Result + 'Truncate';
  end;
  Result := Result + ')';
end;

end.

// EOF


