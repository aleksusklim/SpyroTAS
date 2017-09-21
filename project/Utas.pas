unit Utas; // SpyroTAS is licensed under WTFPL

// shared functions and variables

interface

uses
  Windows, Forms, Messages, Classes, Graphics, SysUtils, StrUtils, DateUtils,
  IniFiles, Math;

const
  // customization for other developers / builds:
  SpyroTASGlobalName = 'SpyroTAS'; // change here in soruce, but read only by SpyroTASName
  SpyroTASVersionMajor: Integer = 2; // don't forget to change these
  SpyroTASVersionMinor: Integer = 5; // version constants in .RC also!
  SpyroTASVestionString: string = ' v2.5!'; // for strings, according to above
  SpyroTASDirectory: string = '.\SpyroTAS\'; // rename here to resolve path collisions
  // other constans:
  {$IFDEF FPC} // name only for strings and messages:
  SpyroTASName: string = SpyroTASGlobalName + ' (Laz)'; // to visually distinguish
  SpyroTASHelper: string = 'padSpyroTAShelper_Laz.';
  SpyroTASHelperRes: string = 'PADSPYROTASHELPERLAZ';
  {$ELSE}
  SpyroTASName: string = SpyroTASGlobalName; // Delphi is primary
  SpyroTASHelper: string = 'padSpyroTAShelper.';
  SpyroTASHelperRes: string = 'PADSPYROTASHELPER';
  {$ENDIF}
  SpyroTASGlobalIni: string = 'SpyroTAS.ini'; // main/hotkey settings
  ExtSaveState = '.SpyroTasSaveState'; // only for export
  ExtKeyHistory = '.SpyroTasHistory'; // used when import
  ExtSprintHist = '.SpyroTasSprint'; // for sprint mode
  ExtForArchive = '.SpyroTAS';
  SpyroTASFilters: string = '*' + ExtSaveState + ';*' + ExtKeyHistory + ';*' +
    ExtSprintHist + ';*' + ExtForArchive;
  SpyroTASForScreenshots: string = 'shots\';
  SpyroTASForHistory: string = 'hist\';
  SectionSpyrotas: string = 'spyrotas'; // for ini files
  SectionLastpath: string = 'lastpath';
  SectionSettings: string = 'settings';
  HistoryOffsetInRAM: Integer = 348;
  SizeOfRAM = 2 * 1024 * 1024; // 2 Mb RAM
  PadAlphaDefault = 196;
  DefaultActionSleep: Integer = 100; // for several user actions
  DefaultFrameWait: Integer = 5000; // 5 sec wait for screenshot and overlay redraw
  PaddingForLastest: Integer = 4;
  MY80807CW = $27F; // Masked, Double, Nearest
  SprintValue: Integer = -3;

var
  HistoryPosition: Integer = -1; // current frame in history; -1 in free modes
  LastTimepoint: Integer = 0; // position from which to record new history or overwrite
  ThresholdTimepoint: Integer = 0; // left position for semi-keys
  EmulatorWindow: HWND = 0; // handle of the game window
  PointerToRamStart: PChar = nil; // points to game memory, in current process space
  SavestateDirectory: string = '.\sstates\'; // will be changed according to .ini
  SavestateID: string = ''; // name of default used savestate file, like GAME_XYZ.000
  EmulatorRoot: string; // full path to emualtor directory, populated as soon as possible
  PathToSpyroTAS: string; // = EmulatorRoot + SpyroTASDirectory
  LoadInFree: Boolean; // when should load a keystate without activating the history
  StopOnBlur: Boolean; // when history is active to prevant the game to lose focus
  GameIsRunning: Boolean; // notification, for when game thread is not in a wait state (TODO)
  SwitchRequested: Boolean; // when gui needs to resume the game
  AviConfigured: Boolean = False;
  AviFrameRate: Integer;
  PngCompression: Integer;
  ShotNeed: Boolean;
  ShotFreeMode: Boolean;
  ShotSkipSame: Boolean;
  ShotCount: Integer;
  AutofireRandom: array[0..15] of Integer;
  AutofireButton: array[0..15] of ByteBool;
  KeystateList: array of Integer;
  KeystateUsedToStart: Integer;
  KeystateToLoad: Integer;
  SaveWaitForNone: Integer;
  SaveWaitForDone: Integer;
  SaveWaitAfter: Integer;
  SaveInEffect: Boolean;
  SaveLoadFailed: Integer;
  LoadWaitForNone: Integer;
  LoadWaitForDone: Integer;
  LoadWaitAfter: Integer;
  LoadInEffect: Boolean;
  SaveSettingsScheduled: Boolean;
  ArchivesList: TStringList;
  GlobalPadMask: Integer;
  KeyAutofireMask: Integer;
  SemiKeysUsed: Boolean;
  IsRunDLL: Boolean;
  MainIsAlreadyInited: Boolean;
  TimeOfStart: TDateTime;
  HistoryWasAltered: Boolean;
  DontShowHints: Boolean;
  DebugRandomKeys: Boolean;
  HaveNewScreen: Boolean;
  StartFromRecord: Boolean = False;
  LiveSaveRequest: Boolean = False;
  SpecSaveRequest: Boolean = False;
  AllowNextFrame: Boolean = False;
  PadToggleFrame: Boolean;
  PadShadowFrame: Boolean;
  OldToggleFrame: Boolean;
  LastFrameKeys: Integer = 0;
  FrameAdvanceHold: Integer = 0;
  AdvanceRequested: Boolean = False;
  AdvanceWaitRelease: Boolean = False;
  SelfDllTime: Integer; // assigned with DLL first init, for self-check
  IsOneCleanup: Integer = 1;
  IsOneMain: Integer = 1;
  GpuWatching: Boolean = False;
  StoredKeys: Integer = 0;
  AllowWarp: Boolean = False;
  DoWarpNow: Boolean = False;
  WarpRequested: Boolean = False;
  WatchFlipRequested: Boolean = False;
  WatchNextRequested: Boolean = False;
  SpyroOffsetText: string = '';
  SpyroOffset: Integer = 0;
  MoveLimitMb: Integer = 16;
  PathToIni: string;
  SprintMode: Boolean = False; // TODO
  SprintFile: string = '';
  SprintState: string = '';
  SprintSave: Integer = 0;
  SprintHashBegin: Integer = 0;
  SprintHashEnd: Integer = 0;
  SprintMiss: Boolean = False;
  SprintCalcM: Integer = 0;
  SprintCalcD: Integer = 0;
  ThisIsRestart: Boolean = False;
  FireDeadlock: Integer = 0;

var // for coordinate measurment
  SpyroX1, SpyroY1, SpyroZ1, SpyroX2, SpyroY2, SpyroZ2, SpyroTime2: Integer;
  SpyroDist, SpyroDist1Prev, SpyroDist2Prev, SpyroDistMax: Real;
  SpyroAway, SpyroApproach, SpyroUse1, SpyroUse2: Boolean;
  SpyroOffsetX, SpyroOffsetY, SpyroOffsetZ: Integer;

function UpdateHistoryPosition(): Boolean;

procedure SetHistoryPosition(NewPosition: Integer);

procedure SetLastestPosition(NewPos: Integer);

procedure SetThresholdPosition(NewThres: Integer);

procedure SavestateCopyFrom(CopyFromWhere: string);

procedure SavestateMoveTo(WhereToMove: string);

procedure SavestateDelete();

function SavestateExist(): Boolean;

function ImportExportHistory(Filename: string; DoImport: Boolean): Boolean;

procedure SwitchToEmulator(Intent: Boolean);

function ImportSpyroTAS(Filename: string): Boolean;

procedure MakeGameScreenshot(OnlyForAviOptions: Boolean);

procedure ResetLoadSave();

procedure LoadSettings();

procedure SaveSettings();

function EnsureAbsolutePath(const AnyPath: string): string;

procedure EnterSptintMode(Start: Boolean);

function HashRAM(): Integer;

procedure SprintFileSave();

function SprintFileLoad(): Boolean;

procedure SprintCompute();

implementation

uses
  MMSystem, UFgui, UFext, Uforms, UFover, Uavi, UFview, UFshot, Uglob, Controls,
  Types, Uini, Ukey, Umain, Umisc, Uroute, UFedit;

var
  OldCaption: string = '';

function PrintWindow(hwnd: HWND; hdcBlt: HDC; nFlags: UINT): BOOL; stdcall;
  external 'user32.dll';

// increment history number in game RAM
function UpdateHistoryPosition(): Boolean;
var
  MemPointer: PInteger;
  NewPosition: Integer;
begin
  Result := False;
  if ThisIsRestart then
    Exit;
  if PointerToRamStart = nil then
  begin
    if RamStartRequest = nil then
    begin
      if (HistoryPosition <> -1) and (HistoryPosition <> SprintValue) then
        Inc(HistoryPosition);
      Exit;
    end;
    HistoryWasAltered := False;
    PointerToRamStart := RamStartRequest;
    SetHistoryPosition(HistoryPosition);
  end;
  if IsRunDLL or HistoryWasAltered then
  begin
    HistoryPosition := -1;
    Exit;
  end;
  MemPointer := PInteger(PointerToRamStart + HistoryOffsetInRAM);
  NewPosition := MemPointer^;
  if (HistoryPosition <> -1) and (HistoryPosition <> SprintValue) and (NewPosition
    <> HistoryPosition) then
    Result := True;
  HistoryPosition := NewPosition;
  if (HistoryPosition = -1) or (HistoryPosition = SprintValue) then // (-1) is a special "don't update"
    Exit;
  Inc(HistoryPosition); // +1 and put back
  MemPointer^ := HistoryPosition;
end;

// force new history positon to game
procedure SetHistoryPosition(NewPosition: Integer);
begin
  HistoryPosition := NewPosition; // update global too
  if IsRunDLL or HistoryWasAltered or (PointerToRamStart = nil) then
    Exit;
  PInteger(PointerToRamStart + HistoryOffsetInRAM)^ := HistoryPosition;
end;

// change last saved history point
procedure SetLastestPosition(NewPos: Integer);
begin
  LastTimepoint := NewPos;
  SendAction(caPositions); // signal GUI
end;

// TODO
procedure SetThresholdPosition(NewThres: Integer);
begin
  ThresholdTimepoint := NewThres;
  SendAction(caPositions); // signal GUI too
end;

// copy savestate from any file to proper location:
procedure SavestateCopyFrom(CopyFromWhere: string);
begin
  if SavestateID = '' then
    Exit;
  CopyFileA(PChar(CopyFromWhere), PChar(SavestateDirectory + SavestateID), False);
end;

// move (rename) savestate to any filename:
procedure SavestateMoveTo(WhereToMove: string);
begin
  DeleteFile(WhereToMove); // replacing
  if SavestateID = '' then
    Exit;
  MoveFileA(PChar(SavestateDirectory + SavestateID), PChar(WhereToMove));
end;

// delete savestate file by index
procedure SavestateDelete();
begin
  if SavestateID = '' then
    Exit;
  DeleteFile(SavestateDirectory + SavestateID);
  DeleteFile(SavestateDirectory + SavestateID + '.pic'); // picture also
end;

// test for savestate file by index
function SavestateExist(): Boolean;
var
  TestStream: TFileStream;
begin
  Result := True;
  try
    if SavestateID = '' then
      Abort;
    if not FileExists(SavestateDirectory + SavestateID) then
      Abort;
    TestStream := TFileStream.Create(SavestateDirectory + SavestateID,
      fmOpenRead or fmShareDenyNone);
    if TestStream.Size = 0 then
      Result := False; // treat empty as non-existed
    TestStream.Free;
  except
    Result := False; // couldn't open
  end;
end;

function ImportExportHistory(Filename: string; DoImport: Boolean): Boolean;
var
  Size: Integer;
  Stream: TFileStream;
  Header: array[0..3] of Integer;
begin
  Result := False;
  Stream := nil;
  try
    if DoImport then
    begin
      Header[0] := 0;
      Header[1] := 0;
      Header[2] := 0;
      Header[3] := 0;
      Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
      Stream.ReadBuffer(Header, SizeOf(Header));
      Size := Header[2];
      if (Header[0] <> 1) or (Header[3] <> 0) or (Size <= 0) then // wrong file
      begin
        Report(SpyroTASName + ' history import error:', Filename, Fgui);
        Exit;
      end;
      History.SetSize(Size + 10);
      SetLength(KeystateList, 2);
      KeystateList[0] := Header[1];
      KeystateList[1] := Size;
      SendAction(caListDirty);
      History.FromStream(HistoryKeys, Size, Stream);
      History.FromStream(HistoryHash, Size, Stream, True);
      History.FromStream(HistoryRrec, Size, Stream, True);
      SetLastestPosition(Size);
    end
    else
    begin
      Size := KeystateList[Length(KeystateList) - 1];
      History.SetSize(Size + 10);
      Header[0] := 1; // format version
      Header[1] := KeystateList[0]; // beginning
      Header[2] := Size; // ending
      Header[3] := 0; // reserved
      Stream := TFileStream.Create(Filename, fmCreate);
      Stream.WriteBuffer(Header, SizeOf(Header));
      History.ToStream(HistoryKeys, Stream, Size);
      History.ToStream(HistoryHash, Stream, Size);
      History.ToStream(HistoryRrec, Stream, Size);
    end;
    Result := True;
  except
    on e: Exception do
      Report(SpyroTASName + ' ImportExportHistory() exception:', e.Message);
  end;
  Stream.Free();
end;

function ImportSpyroTAS(Filename: string): Boolean;
var
  Name, State: string;
begin
  Result := False;
  if not FileExists(Filename) then
    Exit;
  if SprintMode then
    EnterSptintMode(False);
  Name := LowerCase(ExtractFileExt(Filename));
  if Name = LowerCase(ExtSprintHist) then
  begin
    Name := ChangeFileExt(ChangeFileExt(Filename, ''), ExtSaveState);
    if not FileExists(Name) then
      Exit;
    SprintFile := Filename;
    SprintState := Name;
    Fgui.b_free.Click();
    EnterSptintMode(True);
    Exit;
  end;
  SwitchToEmulator(True);
  State := Filename;
  Name := ChangeFileExt(Filename, ExtKeyHistory);
  if FileExists(Name) then
  begin
    Result := ImportExportHistory(Name, True);
    if LowerCase(Name) = LowerCase(Filename) then
      State := ChangeFileExt(Filename, ExtSaveState);
  end;
  if FileExists(State) then
    CopyFile(PChar(State), PChar(NameOfKeystate(0)), False);
  if Fgui.lst_history.Items.Count > 0 then
    Fgui.lst_history.ItemIndex := 0;
  KeystateToLoad := 0;
  LoadInFree := not Result;
  LoadRequsted := True;
  WaitBeforeLoading := 20;
  SendAction(caListDirty);
  SwitchToEmulator(False);
end;


// called from gui thread
procedure SwitchToEmulator(Intent: Boolean);
begin
  SwitchingEffect := True;
  SkipLocks();

  if Intent and (EmulatorWindow <> 0) then
  begin
    ResetEvent(EventEmulatorTrap);
    TrapRequested := True;
    while WaitForSingleObject(EventEmulatorTrap, 50) = WAIT_TIMEOUT do
      SkipLocks();
  end
  else
  begin
    SwitchRequested := False;

    if ThisIsRestart then
      SetColorStatus(StatusSaving)
    else
    begin
      if HistoryPosition <> -1 then
      begin
        if HistoryPosition >= LastTimepoint then
          SetColorStatus(StatusRecord)
        else
          SetColorStatus(StatusPlay);
      end
      else
        SetColorStatus(StatusFree);
    end;

    StopOnBlur := False;
    ClearCatchedKeys();
    LastFrameTimestamp := timeGetTime();
    AllFormsAction(afaGhost);

    ResetEvent(EventEmulatorTrap);
    SetEvent(EventContinueGame);
    SetEvent(EventEmulatorWait);
    SkipLocks();
    if EmulatorWindow <> 0 then
    begin
      FireDeadlock := 1;
      BringWindowToTop(EmulatorWindow);
      SetForegroundWindow(EmulatorWindow);
      FireDeadlock := 0;
    end;
    SwitchingEffect := False;
  end;
  PadToggleFrame := False;
  SendAction(caPopup);
end;

//
procedure MakeGameScreenshot(OnlyForAviOptions: Boolean);
const
  CAPTUREBLT = $40000000; // winapi constant
  PW_CLIENTONLY = $00000001;
var
  TargetRect: TRect;
  TargetWidth, TargetHeight, ImageSize: Integer;
  ShotBitmap: TBitmap;
  Rop: Cardinal;
  TargetDC, PreviewDC: HDC;
  ImageName: string;
  ImageData: PChar;
begin
  if IsRunDLL or (EmulatorWindow = 0) then
    Exit;
  if ShotSkipSame and (not OnlyForAviOptions) and (not HaveNewScreen) then
    Exit;
  if AreOverlaysVisible() then
    Rop := SRCCOPY or CAPTUREBLT
  else
    Rop := SRCCOPY;
//  TargetRect := Rect(0, 0, 0, 0);
//  GetClientRect(EmulatorWindow, TargetRect);
  TargetRect := TFSpyroTAS(nil).GetSizeClient(EmulatorWindow);
  TargetWidth := TargetRect.Right - TargetRect.Left;
  TargetHeight := TargetRect.Bottom - TargetRect.Top;
  if (TargetWidth < 4) or (TargetHeight < 4) then
    Exit;
  ShotBitmap := TBitmap.Create();
  if (TargetWidth mod 4) = 0 then
    ShotBitmap.PixelFormat := pf24bit
  else
    ShotBitmap.PixelFormat := pf32bit;
  ShotBitmap.Width := TargetWidth;
  ShotBitmap.Height := TargetHeight;
  {$IFDEF FPC}
  ShotBitmap.Canvas.Changed();
  {$ENDIF}

//  TargetDC := GetDC(EmulatorWindow);
//  BitBlt(ShotBitmap.Canvas.Handle, 0, 0, TargetWidth, TargetHeight, TargetDC, 0, 0, Rop);
//  PrintWindow(EmulatorWindow, ShotBitmap.Canvas.Handle,0);
  TargetDC := GetDC(0);
  BitBlt(ShotBitmap.Canvas.Handle, 0, 0, TargetWidth, TargetHeight, TargetDC,
    TargetRect.Left, TargetRect.Top, Rop);

  {$IFDEF FPC}
  ShotBitmap.Canvas.Changed();
  {$ENDIF}

  BitmapDataAndSize(ShotBitmap, ImageData, ImageSize);

  if OnlyForAviOptions then
  begin
    ReleaseDC(EmulatorWindow, TargetDC);
    if not AviConfigured then
    begin
      if avi_settings(Fshot, ShotBitmap, AviFrameRate) then
        AviConfigured := True;
    end
    else
      avi_settings(Fshot, ShotBitmap, AviFrameRate);
    ShotBitmap.Free();
    Exit;
  end;

  ReleaseDC(EmulatorWindow, TargetDC);

  if Fview.Visible then
  begin
    PreviewDC := GetDC(Fview.Handle);
    StretchBlt(PreviewDC, 0, 0, Fview.ClientWidth, Fview.ClientHeight,
      ShotBitmap.Canvas.Handle, 0, 0, TargetWidth, TargetHeight, SRCCOPY);
    ReleaseDC(Fview.Handle, PreviewDC);
  end;

  if ShotFreeMode then
    ImageName := FormatNumberWithZero(ShotCount, 6)
  else
    ImageName := FormatNumberWithZero(HistoryPosition, 6);

  ImageName := ShotPath + 'TAS_' + ImageName;
  if ShotType = stAvi then
  begin

    if not avi_write(ShotBitmap) then
    begin
      if Fgui.c_shot.Checked then
      begin
        Fgui.c_shot.Checked := False;
        Report(SpyroTASName + ' screenshots:', 'AVI error!', Fgui);
      end;
    end;
  end
  else if ShotType = stPng then
  begin
    ImageName := ImageName + '.png';
    SaveBitmapToPng(ShotBitmap, ImageName, PngCompression);
  end
  else if ShotType = stBmp then
  begin
    ImageName := ImageName + '.bmp';
    DeleteFile(ImageName);
    ShotBitmap.SaveToFile(ImageName);
  end;
  Inc(ShotCount);
  ShotBitmap.Free();
end;

procedure ResetLoadSave();
begin
  SaveWaitForNone := 0;
  SaveWaitForDone := 0;
  SaveWaitAfter := 0;
  SaveInEffect := False;
  LoadWaitForNone := 0;
  LoadWaitForDone := 0;
  LoadWaitAfter := 0;
  LoadInEffect := False;
  WaitBeforeLoading := 0;
  LiveSaveRequest := False;
  SpecSaveRequest := False;
end;

//
procedure LoadSettings();
var
  Scr: Integer;
  Ini: TTasIni;
begin
  if SprintMode then
    Exit;
  SaveSettingsScheduled := False;
  Ini := TTasIni.Create();
  Ini.Put('gui_type', 0);
  Ini.Put('gui_fire', False);
  Ini.Put('gui_2nd', False);
  Ini.Put('gui_hash', True);
  Ini.Put('gui_png', 6);
  Ini.Put('gui_avi', 30);
  Ini.Put('gui_semi', 0);
  Ini.Put('gui_auto', 0);
  Ini.Put('gui_diff', True);
  Ini.Put('gui_ever', False);
  Ini.Put('gui_fps', 60);
  Ini.Put('gui_alpha', PadAlphaDefault);
  Ini.Put('gui_warp', False);
  Ini.Put('gui_spyro', '0x00000000');
  Ini.ReadFrom(SectionSettings);
  Ini.Get(Scr);
  Ini.Get(UseAutofire);
  Ini.Get(UseSecond);
  Ini.Get(UseHashing);
  Ini.Get(PngCompression);
  Ini.Get(AviFrameRate);
  Ini.Get(GlobalPadMask);
  Ini.Get(KeyAutofireMask);
  Ini.Get(ShotSkipSame);
  Ini.Get(ShotFreeMode);
  Ini.Get(TargetEmulationFps);
  Ini.Get(PadWindowsAlpha);
  Ini.Get(AllowWarp);
  Ini.Get(SpyroOffsetText);
  Ini.Free();
  AviConfigured := True; // suppress AVI setting display
  ShotType := stBmp;
  if Scr = 2 then
  begin
    Fshot.r_avi.Checked := True;
    Fgui.c_shot.Enabled := False;
    ShotType := stAvi;
  end
  else if Scr = 1 then
  begin
    Fshot.r_png.Checked := True;
    ShotType := stPng;
  end
  else
    Fshot.r_bmp.Checked := True;
  AviConfigured := False;
  Fgui.c_auto.Checked := UseAutofire;
  Fgui.c_2nd.Checked := UseSecond;
  Fgui.c_hash.Checked := UseHashing;
  Fshot.se_png.Value := PngCompression;
  Fshot.se_avi.Value := AviFrameRate;

  Fshot.c_diff.Checked := ShotSkipSame;
  Fshot.c_ever.Checked := ShotFreeMode;
  Fgui.se_fps.Value := TargetEmulationFps;
  Fshot.tr_alpha.Position := PadWindowsAlpha;
  Fgui.c_warp.Checked := AllowWarp;
  Fext.e_spyro.Text := SpyroOffsetText;
  Fext.e_spyroChange(nil);
  AdjustAllAlpha(PadWindowsAlpha);
  SaveSettingsScheduled := False;
end;

procedure SaveSettings();
var
  Scr: Integer;
  Ini: TTasIni;
begin
  if SprintMode then
    Exit;
  if not SaveSettingsScheduled then
    Exit;
  SaveSettingsScheduled := False;
  if ShotType = stAvi then
    Scr := 2
  else if ShotType = stPng then
    Scr := 1
  else
    Scr := 0;
  Ini := TTasIni.Create();
  Ini.Put('gui_type', Scr);
  Ini.Put('gui_fire', UseAutofire);
  Ini.Put('gui_2nd', UseSecond);
  Ini.Put('gui_hash', UseHashing);
  Ini.Put('gui_png', PngCompression);
  Ini.Put('gui_avi', AviFrameRate);
  Ini.Put('gui_semi', GlobalPadMask);
  Ini.Put('gui_auto', KeyAutofireMask);
  Ini.Put('gui_diff', ShotSkipSame);
  Ini.Put('gui_ever', ShotFreeMode);
  Ini.Put('gui_fps', TargetEmulationFps);
  Ini.Put('gui_alpha', PadWindowsAlpha);
  Ini.Put('gui_warp', AllowWarp);
  Ini.Put('gui_spyro', SpyroOffsetText);
  Ini.WriteTo(SectionSettings);
  Ini.Free();
end;

function EnsureAbsolutePath(const AnyPath: string): string;
begin
  Result := PathFixBackslash(AnyPath);
  if not IsAbsolutePath(AnyPath) then
    Result := EmulatorRoot + Result;
end;

procedure EnterSptintMode(Start: Boolean);
begin
  ChooseSemiAuto();
  SprintSave := 0;
  SprintHashBegin := 0;
  SprintHashEnd := 0;
  SprintMiss := False;
  SprintCalcM := 0;
  SprintCalcD := 0;
  if Start then
  begin
    if SprintMode then
      EnterSptintMode(False);
    SaveSettings();
    SprintMode := True;
    Fgui.lst_history.Enabled := False;
    Fgui.b_delete.Enabled := False;
    Fgui.c_hash.Checked := False;
    Fgui.c_hash.Enabled := False;
    Fgui.c_semi.Checked := False;
    Fgui.c_semi.Enabled := False;
    Fgui.c_warp.Checked := False;
    Fgui.c_warp.Enabled := False;
    Fgui.c_skip.Checked := False;
    Fgui.c_skip.Enabled := False;
    Fext.c_spyro2.Checked := False;
    Fext.c_spyro.Checked := False;
    Fext.b_clear.Click();
    Fext.c_spyro.Enabled := False;
    Fext.e_spyro.Enabled := False;
    Fext.b_clear.Enabled := False;
    Fext.b_unpack.Enabled := False;
    Fext.b_store.Enabled := False;
    Fext.b_remove.Enabled := False;
    Fext.lst_arch.Enabled := False;
    Fedit.Close();
    Fgui.se_threshold.Enabled := False;
    Fgui.e_position.Enabled := False;
    Fgui.se_timepoint.Enabled := False;
    Fgui.b_load.Enabled := True;
    Fgui.b_save.Enabled := True;
    Fgui.b_restart.Enabled := True;
    Fgui.b_free.Enabled := True;
    Fgui.b_current.Enabled := True;
    OldCaption := Fgui.Caption;
    Fgui.Caption := 'SPRINT! - ' + OldCaption;
    SetLastestPosition(0);
    SetThresholdPosition(0);
    SprintFileLoad();
    Fgui.b_load.Click();
  end
  else
  begin
    SprintMode := False;
    Fgui.lst_history.Enabled := True;
    Fgui.b_delete.Enabled := True;
    Fgui.c_hash.Enabled := True;
    Fgui.c_semi.Enabled := True;
    Fgui.c_warp.Enabled := True;
    Fgui.c_skip.Enabled := True;
    Fext.e_spyro.Enabled := True;
    Fext.e_spyroChange(nil);
    Fext.b_clear.Enabled := True;
    Fext.lst_arch.Enabled := True;
    Fgui.se_threshold.Enabled := True;
    Fgui.e_position.Enabled := True;
    Fgui.se_timepoint.Enabled := True;
    LoadSettings();
    Fgui.Caption := OldCaption;
    Fgui.timer_mainTimer(nil);
  end;
  FoverF.FormShow(nil);
  FoverR.FormShow(nil);
end;

procedure SprintFileSave();
var
  Stream: TFileStream;
  Hash, Size: Integer;
  Data, From: PInteger;
const
  Head = 4;
begin
  if not FileExists(SprintState) then
    Exit;
  if LastTimepoint < 1 then
    Exit;
  Size := (LastTimepoint * 2 + Head) * 4;
  Hash := HashFile(SprintState);
  GetMem(Data, Size);
  From := Data;
  Inc(From);
  From^ := SprintHashBegin;
  Inc(From);
  From^ := SprintHashEnd;
  Inc(From);
  From^ := 1;
  Inc(From);
  Move(History.Addr(HistoryKeys)^, From^, LastTimepoint * 4);
  Inc(From, LastTimepoint);
  Move(History.Addr(HistoryRrec)^, From^, LastTimepoint * 4);
  From := Data;
  Inc(From);
  Data^ := HashCRC(From, Size - 4);
  CryptData(Data, LastTimepoint * 2 + Head, Hash, True);
  From := Data;
  Inc(From);
  Hash := From^;
  Inc(From);
  Hash := Hash xor From^;
  CryptData(Data, LastTimepoint * 2 + Head, Hash, False);
  Stream := nil;
  SprintFile := ChangeFileExt(SprintState, '.' + TimeToString(Now())) + ExtSprintHist;
  try
    Stream := TFileStream.Create(SprintFile, fmCreate);
    Stream.WriteBuffer(Data^, Size);
  except
  end;
  Stream.Free();
  FreeMem(Data);
end;

function SprintFileLoad(): Boolean;
var
  Stream: TFileStream;
  Hash, Size, Last: Integer;
  Data, From: PInteger;
const
  Head = 4;
begin
  Result := False;
  Stream := nil;
  Data := nil;
  try
    if (not FileExists(SprintState)) or (not FileExists(SprintFile)) then
      Abort;
    Stream := TFileStream.Create(SprintFile, fmOpenRead or fmShareDenyNone);
    Size := Stream.Size;
    if Size < (4 + 60) * 4 then
      Abort;
    if Size > 100 * 1024 * 1024 then
      Abort;
    if (Size mod 8) <> 0 then
      Abort;
    Last := (Size - Head * 4) div 8;
    GetMem(Data, Size);
    Stream.ReadBuffer(Data^, Size);
    From := Data;
    Inc(From);
    Hash := From^;
    Inc(From);
    Hash := Hash xor From^;
    CryptData(Data, Last * 2 + Head, Hash, False);
    Hash := HashFile(SprintState);
    if Hash = 0 then
      Abort;
    CryptData(Data, Last * 2 + Head, Hash, True);
    From := Data;
    Inc(From);
    if Data^ <> HashCRC(From, Size - 4) then
      Abort;
    History.SetSize(Last);
    From := Data;
    Inc(From);
    SprintHashBegin := From^;
    Inc(From);
    SprintHashEnd := From^;
    Inc(From);
    Inc(From);
    Move(From^, History.Addr(HistoryKeys)^, Last * 4);
    Inc(From, Last);
    Move(From^, History.Addr(HistoryRrec)^, Last * 4);
    LastTimepoint := Last;
    SprintCompute();
    Result := True;
  except
  end;
  Stream.Free();
  if Data <> nil then
    FreeMem(Data);
end;

function HashRAM(): Integer;
var
  Cur: Integer;
  Off: PInteger;
begin
  Result := 0;
  if IsRunDLL or (PointerToRamStart = nil) then
    Exit;
  Off := PInteger(PointerToRamStart + HistoryOffsetInRAM);
  Cur := Off^;
  Off^ := 0;
  Result := HashCRC(PointerToRamStart, SizeOfRAM);
  Off^ := Cur;
end;

procedure SprintCompute();
var
  M, D: Double;
begin
  if not SprintMode then
    Exit;
  if LastTimepoint < 60 then
    Exit;
  M := ArrayCalc(History.Addr(HistoryRrec, 30), LastTimepoint - 60, D);
  SprintCalcM := Round(M);
  SprintCalcD := Round(D);
end;

end.

