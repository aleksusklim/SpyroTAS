unit Umain;

interface

uses
  Utas, Forms, Windows, Classes, Controls;

procedure MAIN();

function PadPollHandler(Value: Integer; Routed: Integer; Start: Boolean): Integer;

procedure PadReadHandler(Data: PChar; Second: Boolean);

procedure RequestSwitch(Focus: Boolean = False);

procedure SkipLocks();

const
  HistoryKeys = 0;
  HistoryHash = 1;
  HistoryRrec = 2;
  HistoriesCount = 3; // must be last + 1

var
  IsGuiInvoked: Boolean = False;
  SwitchingEffect: Boolean = False;
  TimeFromFps: Integer = 0; // fps limit converted to milliseconds
  LastFrameTimestamp: Integer = 0; // for proper frame limiting
  IsFreeMode: Boolean; // when history is not active
  SaveRequsted: Boolean = False; // when need to run keystate saving mode
  LoadRequsted: Boolean = False; // same for keystate loading
  TrapRequested: Boolean = False; // when gui needs to temporary pause the game thread
  WaitBeforeLoading: Integer; // timeout to ensure focus for game
  NoFrameLimitHold: Boolean; // notification when this hotkey is hold

implementation

uses
  SysUtils, Graphics, Math, DateUtils, MMSystem, UFbind, Uroute, Uforms, UFplug,
  Uglob, Ugpu, Uini, Ukey, UFgui, Umisc, UFover;

var
  SavedKeys: Integer;
  CurrentKeys: Integer;
  PressedKeys: Integer;
  SwitchFocused: Boolean; //

procedure SkipLocks();
begin
  PadToggleFrame := False;
  AdvanceRequested := False;
  GpuWatching := False;
  SetEvent(EventContinueGame);
  SetEvent(EventFrame);
end;

procedure RequestSwitch(Focus: Boolean = False);
begin
  AdvanceRequested := False;
  SwitchRequested := True;
  PadToggleFrame := False;
  if Focus then
    SwitchFocused := True;
  if not GameIsRunning then
    AllFormsAction(afaFocus);
end;

// stop emulation and focus to plugin window;
// must be called at the very beginning or very end of main loop:
procedure SwitchToGui();
begin
  ThisIsRestart := False;
  SwitchRequested := False;
  HashWait := 0;
  SetColorStatus(StatusGui); // visual signal
  ResetEvent(EventContinueGame); // hold game
  // restore save-load and free modes
  StopOnBlur := False;
  ResetLoadSave();
  if SwitchFocused then
    SendAction(caFocus);
  SwitchFocused := False;
end;

// pause to achieve needed FPS
procedure WaitAndCatch();
var
  UntilTime: Cardinal;
  TimeDiff: Integer; // index
begin
  UntilTime := LastFrameTimestamp + TimeFromFps;
  ClearCatchedKeys();
  while True do
  begin
    CatchKeysNow();
    if NoFrameLimitHold or PadToggleFrame or not UseLimit then // don't wait
      Break;
    TimeDiff := UntilTime - timeGetTime();
    if TimeDiff < 2 then //
      Break;
    Sleep(TimeDiff div 2);
  end;
  LastFrameTimestamp := timeGetTime(); // update timestamp
end;

function LoadKeystateHandler(IntentCall: Boolean = False): Boolean;
begin
  Result := False;
  if SaveInEffect then
    Exit;
  if SprintSave > 0 then
    Exit;
  if IntentCall then
  begin
    if SprintMode then
    begin
      if not FileExists(SprintState) then
        Exit;
      if LastTimepoint < 1 then
        History.ClearFrom(1);
    end
    else
    begin
      if not FileExists(NameOfKeystate(0)) then
        Exit; // no corresponding file
      if (KeystateToLoad >= 0) and (KeystateList[KeystateToLoad] > History.GetSize()) then
        Exit; // messed up indexes
    end;
    HistoryWasAltered := False;
    IsFreeMode := True;
    LoadRequsted := False;
    LoadWaitForDone := 0;
    LoadWaitAfter := 0;
    HashWait := 0;
    SetColorStatus(StatusLoading);
    if SprintMode or (KeystateToLoad = -1) then
      SavedKeys := 0
    else
      SavedKeys := History.Read(HistoryKeys, KeystateList[KeystateToLoad]);
    if SprintMode then
    begin
      SetHistoryPosition(SprintValue);
      SavestateCopyFrom(SprintState);
    end
    else
    begin
      SetHistoryPosition(-1);
      SavestateCopyFrom(NameOfKeystate(Max(0, KeystateToLoad)));
    end;
    KeystateUsedToStart := KeystateToLoad;
    LoadWaitForDone := 3;
  end;
  if LoadWaitForDone > 0 then
  begin
    Result := True;
    Dec(LoadWaitForDone);
    if LoadWaitForDone = 1 then
    begin
      PressKeyDown(GetAssignedHotkey(EmulatorHotkeyLoadstate));
      LoadWaitForDone := 35;
    end;
    if (HistoryPosition <> -1) and (HistoryPosition <> SprintValue) then
    begin
      HashWait := 0;
      SendAction(caPopup);
      LoadWaitForDone := 0;
      IsFreeMode := LoadInFree;
      StopOnBlur := not IsFreeMode;
      if IsFreeMode then
      begin
        SetHistoryPosition(-1);
        SetColorStatus(StatusFree);
      end
      else
      begin
        if SprintMode then
          SetHistoryPosition(1);
        if HistoryPosition >= LastTimepoint then
          SetColorStatus(StatusRecord)
        else
          SetColorStatus(StatusPlay);
      end;
      SavestateDelete();
      if AllowWarp then
      begin
        GpuRecord(False);
        GpuMovieDelete();
        GpuRecord(True);
      end;
      if ThisIsRestart then
        SetHistoryPosition(1);
    end;
  end;
end;

// must be called from main loop with false; and true to invoke savestate
// returns whether to ignore buttons
function SaveKeystateHandler(IntentCall: Boolean = False): Boolean;
var
  Index, TargetKeystate: Integer;
begin
  Result := False;
  if LoadInEffect then
    Exit;
  if SprintMode and ((SprintSave > 0) or (HistoryPosition < 60)) then
    Exit;
  if IntentCall then
  begin
    SaveRequsted := False;
    if SprintMode then
    begin
      if (HistoryPosition < 1) or (LastTimepoint > 0) then
        Exit;
      SprintSave := 1;
      SetColorStatus(StatusSaving);
      Exit;
    end;
    SaveWaitAfter := 0;
    SetColorStatus(StatusSaving);
    SavestateDelete();
    if LiveSaveRequest then
    begin
      if HistoryPosition >= LastTimepoint - PaddingForLastest then
      begin
        SetLastestPosition(HistoryPosition - 1);
        LiveSaveRequest := False;
      end
      else
      begin
        PressKeyDown(GetAssignedHotkey(EmulatorHotkeySavestate));
        Exit;
      end;
    end;
    SavedKeys := CurrentKeys;
    SaveWaitForDone := 3;
    if ThisIsRestart then
    begin
      SetLastestPosition(0);
      SetHistoryPosition(1);
    end;
  end;
  if SprintMode then
    Exit;
  if LiveSaveRequest then
  begin
    if SavestateExist() then
    begin
      HashWait := 0;
      LiveSaveRequest := False;
      StopOnBlur := True;
      for TargetKeystate := 0 to Length(KeystateList) - 1 do
      begin  // TODO!
        if KeystateList[TargetKeystate] > HistoryPosition then
        begin
          SetLength(KeystateList, Length(KeystateList) + 1);
          for Index := Length(KeystateList) - 2 downto TargetKeystate do
          begin
            KeystateList[Index + 1] := KeystateList[Index];
            RenameFile(NameOfKeystate(Index), NameOfKeystate(Index + 1));
          end;
          KeystateList[TargetKeystate] := HistoryPosition;
          SavestateMoveTo(NameOfKeystate(TargetKeystate));
          SetColorStatus(StatusPlay);
          KeystateToLoad := TargetKeystate;
          KeystateUsedToStart := TargetKeystate;
          SendAction(caListDirty);
          Exit;
        end;
      end;
      TargetKeystate := Length(KeystateList);
      SetLength(KeystateList, TargetKeystate + 1);
      KeystateList[TargetKeystate] := HistoryPosition;
      SavestateMoveTo(NameOfKeystate(TargetKeystate));
      SetColorStatus(StatusRecord);
      KeystateToLoad := TargetKeystate;
      KeystateUsedToStart := TargetKeystate;
      SendAction(caListDirty);
    end;
    Exit;
  end;
  if SaveWaitForDone > 0 then
  begin
    Result := True;
    if SaveWaitForDone > 1 then
    begin
      Dec(SaveWaitForDone);
    end
    else
      PressKeyDown(GetAssignedHotkey(EmulatorHotkeySavestate));
    CurrentKeys := SavedKeys;
    if SavestateExist() then
    begin
      HashWait := 0;
      StopOnBlur := True;
      SaveWaitForDone := 0;
      TargetKeystate := Length(KeystateList);
      while TargetKeystate > 0 do
        if KeystateList[TargetKeystate - 1] > LastTimepoint then
          Dec(TargetKeystate)
        else
          Break;
      History.ClearFrom(HistoryPosition);
      SetLength(KeystateList, TargetKeystate + 1);
      SavestateMoveTo(NameOfKeystate(TargetKeystate));
      KeystateUsedToStart := TargetKeystate;
      KeystateToLoad := TargetKeystate;
      KeystateList[TargetKeystate] := HistoryPosition;
      SetLastestPosition(HistoryPosition);
      SendAction(caListDirty);
      SetColorStatus(StatusPlay);
      if AllowWarp and not IsGpuRecording then
      begin
        GpuMovieDelete();
        GpuRecord(True);
      end;
      if ThisIsRestart then
        LoadRequsted := True;
      ThisIsRestart := False;
    end;
  end;
end;

// initial run of main():
function InitIfFirstTime(): Boolean;
begin
  Result := not IsGuiInvoked;
  if MainIsAlreadyInited then
    Exit;
  MainIsAlreadyInited := True;
  IsFreeMode := True;
  SavestateID := '';
  if IniValueUpdate(PathToIni, SectionSpyrotas, 'boot_play') = '1' then
  begin
    IniValueUpdate(PathToIni, SectionSpyrotas, 'boot_play', '0');
    StartFromRecord := True;
    IsFreeMode := False;
  end
  else if IniValueUpdate(PathToIni, SectionSpyrotas, 'tas_freeze') = '1' then
    AlwaysAutoinvoke := True;
  if MoveLimitMb < 4 then
    MoveLimitMb := 4
  else if MoveLimitMb > 256 then
    MoveLimitMb := 256;
  GpuMovieLimit := MoveLimitMb * 1024 * 1024;
  if not DirectoryExists(SavestateDirectory) or ((not Assigned(GPUupdateLace_))
    and not Assigned(GPUdmaChain_) and not Assigned(GPUopen_)) then
  begin
    IsRunDLL := True;
    Fplug := TFplug.CreateUsual();
    Screen.Cursor := crArrow;
    Fplug.Cursor := crArrow;
    ShowCursor(True);
    Report(SpyroTASName + ' error:',
      'SpyroTAS is not configured! You were expected to select your real video plugin in SpyroTAS configuration, at emulator config window.'#13#13 +
      'But you can do it now. Then close the next window to exit, or click "OK" there (in SpyroTAS configuration window) to restart the emulator automatically.');
    Fplug.ShowModal();
    ProcessRestart(); // does not return
  end;
  HistoryWasAltered := False;
  SetHistoryPosition(0);
  MainInited();
  SetEvent(EventContinueGame);
end;

// game coordinates measure:
procedure DoSpyro();
var
  X, Y, Z: Integer; // current coords
  Dist1, Dist2: Real;
begin
  if IsRunDLL or (not SpyroUse1) or (PointerToRamStart = nil) then // out if not needed
    Exit;
  X := PInteger(PointerToRamStart + SpyroOffsetX)^;
  Y := PInteger(PointerToRamStart + SpyroOffsetY)^;
  Z := PInteger(PointerToRamStart + SpyroOffsetZ)^;
  if not SpyroUse2 then // first mode, for height
  begin
    Dist1 := SquaredDistance(X - SpyroX1, Y - SpyroY1); // 2D dist to target
    if (Dist1 > SpyroDist) and not SpyroAway then // first time moved out
    begin
      SendAction(caSpyro, Z); // send height
      SpyroAway := True; // to discard next time
    end
    else if Dist1 < SpyroDist then // any move towards
      SpyroAway := False; // unlock
    SpyroDist := Dist1; // old position
  end
  else
  begin  // second mode, for time
    Inc(SpyroTime2); // frame counter
    Dist1 := SquaredDistance(X - SpyroX1, Y - SpyroY1, Z - SpyroZ1); // to point A
    Dist2 := SquaredDistance(X - SpyroX2, Y - SpyroY2, Z - SpyroZ2); // to point B
    if SpyroApproach then // moving towards ??
    begin
      if (Dist1 > SpyroDist) and not SpyroAway then // first time moved out A
      begin
        SpyroTime2 := 0; // reset counter
        SpyroAway := True;
        SpyroApproach := False; //
        SpyroDist := Dist2; // ?
        Exit;
      end
      else if Dist1 < SpyroDist then // moded towards A
        SpyroAway := False;
      SpyroDist := Dist1; // old distance
    end
    else
    begin //
      if (Dist1 < SpyroDist1Prev) and (Dist2 < SpyroDist2Prev) and (Dist2 >
        SpyroDistMax) then // reset time while close to A
        SpyroTime2 := 0;
      SpyroDist1Prev := Dist1;
      SpyroDist2Prev := Dist2;
      if (Dist2 > SpyroDist) and (Dist1 >= SpyroDistMax) and not SpyroAway then
      begin
        SendAction(caSpyro, SpyroTime2);
        SpyroTime2 := 0;
        SpyroAway := True;
        SpyroApproach := True;
        SpyroDist := Dist1;
        Exit;
      end
      else if Dist2 < SpyroDist then
        SpyroAway := False;
      SpyroDist := Dist2;
    end;
  end;
end;

type
  TSomeHotkeys = set of (AHotkeySave, AHotkeyLoad, AHotkeyCurrent);

function CatchSomeHotkeys(Hotkeys: TSomeHotkeys): Boolean;
begin
  Result := False;
  if AHotkeyCurrent in Hotkeys then
    if IsHotkeyPressed(HotkeyCurrent, True) then
    begin
      if SprintMode then
      begin
        Result := False;
      end
      else
      begin
        if not IsFreeMode then
        begin
          if SemiKeysUsed and (LastTimepoint > 0) then
            SetThresholdPosition(HistoryPosition)
          else
            SetLastestPosition(HistoryPosition);
        end
        else
          Result := True;
      end;
    end;
  if AHotkeyLoad in Hotkeys then
    if IsHotkeyPressed(HotkeyLoad, True) or LoadRequsted then
    begin
      Result := True;
      LoadRequsted := True;
    end;
  if AHotkeySave in Hotkeys then
    if (not IsFreeMode) and (IsHotkeyPressed(HotkeySave, True) or SaveRequsted) then
    begin
      Result := True;
      SaveRequsted := True;
      if not SprintMode then
      begin
        if SpecSaveRequest and (HistoryPosition <> -1) then
        begin
          SetLastestPosition(HistoryPosition - 1);
          SpecSaveRequest := False;
        end
        else if (HistoryPosition <> -1) and (HistoryPosition < LastTimepoint) then
          LiveSaveRequest := True;
      end;
    end;
end;

procedure GpuMovieHandler();
var
  FramePlayed: Boolean;
  Rec, Play, Back, Oldnew, Yes: Boolean;
  Stat: TColorStatus;
  KeyWait, Key, Key2: Integer;
  OldHistory: Integer;
const
  WaitDone: Integer = 7;
begin
  if GuiClosing or not AllowWarp or not IsGuiInvoked then
    Exit;
  Rec := False;
  OldHistory := HistoryPosition;

  GpuWatching := True;
  if IsGpuRecording then
  begin
    Rec := True;
    GpuRecord(False);
  end;
  try
    Back := True;
    Play := WatchFlipRequested;
    KeyWait := 0;
    Oldnew := HaveNewScreen;
    Stat := CurrentStatus;
    HashWait := 0;
    SetColorStatus(StatusGpuMovie);
    GpuReplay(True);
    Yes := True;
    WatchFlipRequested := False;
    WatchNextRequested := False;
    repeat
      WaitAndCatch();
      FramePlayed := False;
      if Back then
      begin
        Key := GetAssignedHotkey(HotkeyWarp);
        Key2 := GetAssignedHotkey(HotkeyFrame);
      end
      else
      begin
        Key := GetAssignedHotkey(HotkeyFrame);
        Key2 := GetAssignedHotkey(HotkeyWarp);
      end;
      if WatchFlipRequested and not Play then
      begin
        Play := True;
        WatchFlipRequested := False;
      end;
      if IsKeyJustPressed(Key2) or WatchFlipRequested then
      begin
        if WatchFlipRequested then
          Play := True;
        WatchFlipRequested := False;
        Back := not Back;
        KeyWait := 0;
        if not Play then
          Yes := True;
      end;
      if IsKeyJustPressed(Key) or WatchNextRequested then
      begin
        KeyWait := 0;
        if Play then
          Play := False
        else
        begin
          FramePlayed := True;
          repeat
            HaveNewScreen := False;
            if GpuPlayFrame(Back) then
            begin
              if Back then
                Back := False
              else
              begin
                GpuWatching := False;
                AdvanceRequested := not Play;
              end;
              Break;
            end;
          until HaveNewScreen;
        end;
        WatchNextRequested := False;
      end;
      if IsKeyCatched(Key) or Yes then
      begin
        Inc(KeyWait);
        if KeyWait > WaitDone then
          Play := True;
      end
      else
        KeyWait := 0;
      if Play then
      begin
        FramePlayed := True;
        if GpuPlayFrame(Back) then
          if Back then
          begin
            Back := False;
            Play := False;
          end
          else
            GpuWatching := False;
      end;
      if FramePlayed then
      begin
        HaveNewScreen := False;
        ResetEvent(EventFrame);
        if ShotNeed and ShotFreeMode then // only in ever-mode
          SendAction(caFrame, 1)
        else
          SendAction(caFrame, 0);
        WaitForSingleObject(EventFrame, DefaultFrameWait);
      end
      else
        Sleep(35);

      if IsHotkeyPressed(HotkeyFast, False) then
        GpuWatching := False;

      if CatchSomeHotkeys([AHotkeyLoad, AHotkeyCurrent]) then
        GpuWatching := False;

      Yes := False;
    until not GpuWatching;
    GpuReplay(False);
    if Rec then
      GpuRecord(True);
    HaveNewScreen := Oldnew;
    HashWait := 0;
    SetColorStatus(Stat);
  except
    on E: Exception do
      Report(SpyroTASName + ' GPU movie error:', E.Message);
  end;
  ClearJustPressed();
  ReleaseAllPressedKeys();
  HistoryPosition := OldHistory;
end;

var
  HoldPrevKey: Integer = 0;
  HoldCurrKey: Integer = 0;
  LastRerecord: Integer; // keeps re-record conter to display in overlay

// every frame logic

procedure MAIN();
var
  InputDisabled: Boolean;
  Index, OldHash, CurrentHash, PadMask, Sum, Counter: Integer;
  SemiHashFix: Boolean;
label
  movie;
begin
  if InitIfFirstTime() then // works only once
    if not StartFromRecord then
      Exit; // really until invoke
  StoredKeys := 0;
  if SwitchRequested or StopOnBlur and (GetActiveWindow() <> EmulatorWindow) and
    not IsPadRouted then
    SwitchToGui(); // stop game when switching during recording
  if WaitForSingleObject(EventContinueGame, 0) <> WAIT_TIMEOUT then
    GameIsRunning := True // execution allowed
  else
  begin
    GameIsRunning := False; // wait release
    while not TrapRequested do
      if WaitForSingleObject(EventContinueGame, 100) <> WAIT_TIMEOUT then
        Break;
    GameIsRunning := True;
  end;

  if TrapRequested then
  begin
    TrapRequested := False;
    ResetEvent(EventEmulatorWait);
    SetEvent(EventEmulatorTrap);
    WaitForSingleObject(EventEmulatorWait, INFINITE);
  end;

  if ((CurrentStatus = StatusSaving) and (SaveWaitForNone = 0)) or (CurrentStatus
    = StatusLoading) then
  begin
    Inc(SaveLoadFailed);
    if SaveLoadFailed > 80 then
    begin
      if SprintMode then
      begin
        SetHistoryPosition(-1);
        IsFreeMode := True;
        HashWait := 0;
        SetColorStatus(StatusFree);
      end
      else
        RequestSwitch();
    end;
  end
  else
    SaveLoadFailed := 0;

  ReleaseAllPressedKeys(); // from previous frame

  if (IsHotkeyPressed(HotkeyWarp, True) or WarpRequested) and not SprintMode then
  begin
movie:
    WarpRequested := False;
    GpuMovieHandler();
  end;

  if HaveNewScreen and AllowNextFrame then
    AllowNextFrame := False;

  if (IsHotkeyPressed(HotkeyFrame, True) or AdvanceRequested) and not
    SaveInEffect and not LoadInEffect and not SprintMode then
  begin
    PadToggleFrame := True;
    FrameAdvanceHold := 0;
    AdvanceRequested := False;
    JustPressedKeys(); // ignore already pressed
  end;

  if not GuiClosing then
    repeat
      WaitAndCatch(); // framelimit and polling keyboard
      NoFrameLimitHold := IsHotkeyPressed(HotkeyFast, False);
      if NoFrameLimitHold then
      begin
        if AdvanceWaitRelease then
          NoFrameLimitHold := False;
      end
      else
        AdvanceWaitRelease := False;
      if AllowNextFrame or (not PadToggleFrame) or NoFrameLimitHold then
        Break;
      GameIsRunning := False;
      PressedKeys := JustPressedKeys();
      if UseSecond then
        LastFrameKeys := LastFrameKeys xor (PressedKeys shl 16)
      else
        LastFrameKeys := LastFrameKeys xor PressedKeys;

      if IsHotkeyPressed(HotkeyFrame, False) and not SprintMode then
      begin
        AdvanceRequested := False;
        Inc(FrameAdvanceHold);
        if FrameAdvanceHold > 30 then
        begin
          FrameAdvanceHold := 25;
          Break;
        end;
      end
      else
        FrameAdvanceHold := 0;
      if IsHotkeyPressed(HotkeySwap, True) then
        UseSecond := not UseSecond;
      if IsHotkeyPressed(HotkeyAuto, True) then
        UseAutofire := not UseAutofire;

      if (IsHotkeyPressed(HotkeyWarp, True) or WarpRequested) and not SprintMode then
      begin
        PadToggleFrame := False;
        goto movie;
      end;

      if CatchSomeHotkeys([AHotkeySave, AHotkeyLoad, AHotkeyCurrent]) then
      begin
        PadShadowFrame := True;
        PadToggleFrame := False;
      end;

      RequestOverlayRedraw(LastFrameKeys, HistoryPosition, LastRerecord);
      ResetEvent(EventFrame);
      SendAction(caFrame, 0);
      WaitForSingleObject(EventFrame, DefaultFrameWait);

      Sleep(20);
    until IsHotkeyPressed(HotkeyFrame, True) or AdvanceRequested or not PadToggleFrame;

  GameIsRunning := True;
  CurrentKeys := LastFrameKeys; // for SavedKeys

  if NoFrameLimitHold then
    PadToggleFrame := False;
  if PadToggleFrame then
    AllowNextFrame := True
  else if not PadShadowFrame then
    LastFrameKeys := 0;

  if UpdateHistoryPosition() then
    HistoryPosition := -2;

  if DoWarpNow and AllowWarp and not IsGpuRecording then
  begin
    DoWarpNow := False;
    GpuMovieDelete();
    GpuRecord(True);
  end;

  if CatchSomeHotkeys([AHotkeySave]) then
    SaveKeystateHandler(True);

  InputDisabled := False;
  // check save and load operations
  InputDisabled := LoadKeystateHandler() or InputDisabled;
  LoadInEffect := (LoadWaitForNone > 0) or (LoadWaitForDone > 0) or (LoadWaitAfter > 0);
  InputDisabled := SaveKeystateHandler() or InputDisabled;
  SaveInEffect := (SaveWaitForNone > 0) or (SaveWaitForDone > 0) or (SaveWaitAfter > 0);

  if LoadInEffect and (CurrentStatus <> StatusLoading) then
    SetColorStatus(StatusLoading);
  if SaveInEffect and (CurrentStatus <> StatusSaving) then
    SetColorStatus(StatusSaving);

  if (Length(KeystateList) > 0) and (HistoryPosition > KeystateList[Length(KeystateList)
    - 1]) and (HistoryPosition > History.GetSize() + 1) then
  begin
    IsFreeMode := True;
    SetHistoryPosition(-1);
  end;

  if (HistoryPosition = -2) or (IsFreeMode and (HistoryPosition <> -1) and (HistoryPosition
    <> SprintValue) and not ThisIsRestart) then  // increment current position
  begin
    IsFreeMode := True;
    SetColorStatus(StatusWrong);
    ResetLoadSave();
  end;

  if UseAutofire then // autofire feature checkbox
    for Index := 0 to 15 do
    begin
      Dec(AutofireRandom[Index]); // random delay
      if AutofireRandom[Index] < 0 then
      begin
        AutofireButton[Index] := True; // it fired
        AutofireRandom[Index] := RandomRange(1, 4); // new delay
      end
      else
        AutofireButton[Index] := False; // released
    end
  else
    FillChar(AutofireButton, 16, #1); // without autofire - all pressed

  DoSpyro(); // game coordinates measure

  if (CurrentStatus = StatusKeys) and (ThresholdTimepoint > 0) and (HistoryPosition
    < ThresholdTimepoint) then
  begin
    if (HistoryPosition >= LastTimepoint) then
      SetColorStatus(StatusRecord)
    else
      SetColorStatus(StatusPlay);
  end;

  PadMask := -1; // default use all keys
  if SemiKeysUsed and (ThresholdTimepoint <> 0) and (HistoryPosition >=
    ThresholdTimepoint) and (HistoryPosition < LastTimepoint) and not IsFreeMode
    then // compute mask from checkboxes
  begin
    SemiHashFix := True;
    if UseSecond then
      PadMask := not (GlobalPadMask shl 16)
    else
      PadMask := not GlobalPadMask;
    if (CurrentStatus = StatusPlay) or (CurrentStatus = StatusHashMiss) then
      SetColorStatus(StatusKeys);
  end
  else
    SemiHashFix := False;

  CurrentHash := 0; // todo
  CurrentKeys := 0;
  OldHash := 0; // stays zero in free mode

  if not IsFreeMode then  // recording or playing
  begin

    if NoFrameLimitHold and (HistoryPosition = LastTimepoint - PaddingForLastest) then
    begin
      AdvanceRequested := True;
      AdvanceWaitRelease := True;
    end;

    if UseHashing and (PointerToRamStart <> nil) then // ram CRC needed
      CurrentHash := HashRAM(HistoryPosition);

    if (History.GetSize() <= HistoryPosition) then  // less by one
    begin
      History.Write(HistoryKeys, HistoryPosition, 0);
      History.Write(HistoryHash, HistoryPosition, 0);
    end;
    if HistoryPosition >= 0 then
    begin
      CurrentKeys := History.Read(HistoryKeys, HistoryPosition); // from history
      if HistoryPosition < LastTimepoint then
      begin
        if UseHashing then // stored hash
          OldHash := History.Read(HistoryHash, HistoryPosition);
      end
      else
      begin
        if UseSecond then
        begin // zero current keys if beyond last point
          CurrentKeys := CurrentKeys and $0000ffff;
          if PadToggleFrame or PadShadowFrame then
            CurrentKeys := CurrentKeys or (LastFrameKeys and Integer($ffff0000));
        end
        else
        begin
          CurrentKeys := CurrentKeys and Integer($ffff0000);
          if PadToggleFrame or PadShadowFrame then
            CurrentKeys := CurrentKeys or (LastFrameKeys and $0000ffff);
        end;
      end;
    end;
  end;

  if ThisIsRestart then
    SetColorStatus(StatusSaving)
  else if (HistoryPosition >= LastTimepoint) and (HistoryPosition <=
    LastTimepoint + 1) then // going beyond last save point
  begin
    if SprintMode and (LastTimepoint > 0) then
    begin
      IsFreeMode := True;
      SetHistoryPosition(-1);
      if CurrentStatus <> StatusPlay then
        SetColorStatus(StatusFree)
      else if SprintMiss then
        SetColorStatus(StatusHashMiss)
      else
        SetColorStatus(StatusKeys);
    end
    else
      SetColorStatus(StatusRecord);
  end;

  PadShadowFrame := False;
  PressedKeys := 0;
  if PadToggleFrame or PadShadowFrame then
  begin
    if UseSecond then
      PressedKeys := LastFrameKeys shr 16
    else
      PressedKeys := LastFrameKeys and $ffff;
  end
  else
    ReadFromKeys(PressedKeys, KeyAutofireMask); // fill with new data
  if UseSecond then
    PressedKeys := PressedKeys shl 16;

  if IsFreeMode or (HistoryPosition >= LastTimepoint) then
  begin // pure new reciording
    if UseSecond then
      CurrentKeys := (CurrentKeys and $ffff) or PressedKeys
    else
      CurrentKeys := (CurrentKeys and Integer($ffff0000)) or PressedKeys;
  end
  else
  begin
    CurrentKeys := (CurrentKeys and PadMask) or (PressedKeys and not PadMask);
  end;

  if InputDisabled then  // don't move during saving or loading
    CurrentKeys := SavedKeys;

  if SprintMode and (HistoryPosition > 0) then
  begin
    if HistoryPosition <= 30 then
    begin
      SprintMiss := False;
      CurrentKeys := SavedKeys;
      if HistoryPosition = 20 then
      begin
        if LastTimepoint = 0 then
        begin
          SprintHashBegin := HashRAM(-1);
        end
        else
        begin
          if HashRAM(-1) <> SprintHashBegin then
            SetColorStatus(StatusHashMiss);
        end;
      end;
    end
    else
    begin
      if HistoryPosition = LastTimepoint - 10 then
        if HashRAM(-1) <> SprintHashEnd then
          SprintMiss := True;
    end;
    if SprintSave > 0 then
    begin
      CurrentKeys := SavedKeys;
      if SprintSave = 20 then
        SprintHashEnd := HashRAM(-1);
      if SprintSave = 30 then
      begin
        SprintSave := 0;
        IsFreeMode := True;
        SetLastestPosition(HistoryPosition);
        SetHistoryPosition(-1);
        SetColorStatus(StatusFree);
        SprintFileSave();
        SprintCompute();
      end
      else
        Inc(SprintSave);
    end;
  end;

  if IsGuiInvoked then
  begin
    PressThoseKeys(CurrentKeys); // simulate
    StoredKeys := CurrentKeys;
  end;
  LastFrameKeys := CurrentKeys;

  if (HistoryPosition < LastTimepoint) and (not IsFreeMode) and not SemiHashFix then
  begin
    if CurrentHash <> OldHash then
      HashWait := 33;
    if HashWait > 0 then  // if hashing is used
    begin
      Dec(HashWait);
      if CurrentStatus <> StatusHashMiss then  // wrong hash
        SetColorStatus(StatusHashMiss);
    end
    else if (CurrentStatus = StatusHashMiss) and not SprintMode then
      SetColorStatus(StatusPlay); // sudden restore
  end;

  if CatchSomeHotkeys([AHotkeyLoad]) then
  begin
    if WaitBeforeLoading > 0 then
      Dec(WaitBeforeLoading)
    else
      LoadKeystateHandler(True);
  end;
  LastRerecord := 0;
  if not IsFreeMode then
  begin
    if (HistoryPosition <> -1) and (HistoryPosition <> SprintValue) then
    begin
      History.Write(HistoryKeys, HistoryPosition, CurrentKeys); // save keys
      if not SprintMode then
        LastRerecord := History.Read(HistoryRrec, HistoryPosition);
    end;
    if (HistoryPosition >= LastTimepoint) or SemiHashFix then
    begin
      if SprintMode then
      begin
        LastRerecord := timeGetTime();
        History.Write(HistoryHash, HistoryPosition, LastRerecord);
        if HistoryPosition >= 30 then
        begin
          Sum := History.Read(HistoryHash, HistoryPosition) - History.Read(HistoryHash,
            HistoryPosition - 30);
          LastRerecord := Round(30 * 1000000 / Sum);
        end;
        History.Write(HistoryRrec, HistoryPosition, LastRerecord);
      end
      else
      begin
        if UseHashing then
          History.Write(HistoryHash, HistoryPosition, CurrentHash); // save hash
        Inc(LastRerecord);
        History.Write(HistoryRrec, HistoryPosition, LastRerecord);
      end;
    end;
  end;
  if IsHotkeyPressed(HotkeyPrevious, SprintMode) then
  begin
    if SprintMode then
    begin
      LoadRequsted := True;
      SetLastestPosition(0);
    end
    else
    begin
      Inc(HoldPrevKey);
      if (HoldPrevKey = 40) and (Length(KeystateList) > 1) then
      begin
        KeystateToLoad := Length(KeystateList) - 1;
        LoadKeystateHandler(True);
        SendAction(caListDirty, 1);
      end;
    end;
  end
  else
    HoldPrevKey := 0;
  if IsHotkeyPressed(HotkeyCurrent, False) and not SprintMode then
  begin
    Inc(HoldCurrKey);
    if HoldCurrKey = 50 then
      SendAction(caReload);
  end
  else
    HoldCurrKey := 0;

  if IsHotkeyPressed(HotkeyPrevious, True) and (KeystateToLoad > 0) then
  begin
    Dec(KeystateToLoad);
    LoadKeystateHandler(True);
    SendAction(caListDirty, 1);
  end;

  if IsHotkeyPressed(HotkeySwap, True) then // swap controllers hotkeys
    UseSecond := not UseSecond;
  if IsHotkeyPressed(HotkeyAuto, True) then
    UseAutofire := not UseAutofire;

  Counter := HistoryPosition;
  if SprintMode then
  begin
    if (LastTimepoint > 0) then
    begin
      if (HistoryPosition > 0) and (HistoryPosition < LastTimepoint) and not
        IsFreeMode then
      begin
        if HistoryPosition >= 30 then
          LastRerecord := History.Read(HistoryRrec, HistoryPosition)
        else
          LastRerecord := 0;
      end
      else
      begin
        Counter := -SprintCalcD;
        LastRerecord := -SprintCalcM;
        if Counter > -5 then
          Counter := -5;
        if LastRerecord > -5 then
          LastRerecord := -5;
      end;
    end
    else
    begin
      if HistoryPosition < 30 then
        LastRerecord := 0;
    end;
  end;
  RequestOverlayRedraw(CurrentKeys, Counter, LastRerecord);

  ResetEvent(EventFrame);
  if ShotNeed and (ShotFreeMode or not IsFreeMode) and not TrapRequested then
    SendAction(caFrame, 1) // call shot every frame
  else
    SendAction(caFrame, 0); // frame done
  if not SwitchingEffect then
    WaitForSingleObject(EventFrame, DefaultFrameWait); // wait until completed

  HaveNewScreen := False;
  if CatchSomeHotkeys([AHotkeyCurrent]) then
    SwitchToGui();
end;

// TODO!

var
  CommandPad, CommandNum, CommandType: Integer;

function PadPollHandler(Value: Integer; Routed: Integer; Start: Boolean): Integer;
begin
  Result := Routed;
  if GuiClosing then
    Exit;
  if not IsPadRouted then
  begin
    IsPadRouted := True;
    if Fbind <> nil then
      Fbind.c_pad_routed.Checked := True;
  end;
  if Start then
  begin
    CommandPad := Value;
    CommandNum := 0;
  end
  else
  begin
    Inc(CommandNum);
    if CommandNum = 1 then
      CommandType := Value;
    case CommandType of
      $43:
        ;
      $44:
        ;
      $45:
        ;
      $46:
        ;
      $47:
        ;
      $4C:
        ;
      $4D:
        ;
    else
      begin
        if CommandNum = 3 then
        begin
          if CommandPad = 1 then
            Result := (not (GlobalKeysForPad shr 0)) and 255
          else
            Result := (not (GlobalKeysForPad shr 16)) and 255
        end;
        if CommandNum = 4 then
        begin
          if CommandPad = 1 then
            Result := (not (GlobalKeysForPad shr 8)) and 255
          else
            Result := (not (GlobalKeysForPad shr 24)) and 255
        end;
      end;
    end;
  end;
end;

procedure PadReadHandler(Data: PChar; Second: Boolean);
begin
  if GuiClosing then
    Exit;
  if not IsPadRouted then
  begin
    IsPadRouted := True;
    if Fbind <> nil then
      Fbind.c_pad_routed.Checked := True;
  end;
  Inc(Data, 2);
  if Second then
  begin
    Data^ := Chr(not ((GlobalKeysForPad shr 16)) and 255);
    Inc(Data);
    Data^ := Chr(not ((GlobalKeysForPad shr 24)) and 255);
  end
  else
  begin
    Data^ := Chr(not ((GlobalKeysForPad shr 0)) and 255);
    Inc(Data);
    Data^ := Chr(not ((GlobalKeysForPad shr 8)) and 255);
  end;
end;

end.

