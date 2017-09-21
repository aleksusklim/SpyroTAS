unit Uglob; // SpyroTAS is licensed under WTFPL

interface

uses
  Windows, SysUtils, Forms, Classes, Utas, Umisc;

var
  History: TCustomIntegerArrays = nil;
  EventContinueGame: THandle = 0;
  EventEmulatorTrap: THandle = 0;
  EventEmulatorWait: THandle = 0;
  EventFrame: THandle = 0;
  EventGuiCreated: THandle = 0;
  EventExtinguishFire: THandle = 0;
  EventMainDone: THandle = 0;
  HThreadFire: THandle = 0;
  HThreadGui: THandle = 0;
  IThreadFire: Cardinal = 0;
  IThreadGui: Cardinal = 0;
  IThreadMain: Cardinal = 0;
  GuiClosing: Boolean = False;
  SavestateDirectory: string = '.\sstates\';

procedure ThisDllInit(Reason: Integer);

procedure InitPlugin(StandAlone: Boolean = False);

procedure MainInited();

procedure GlobalCleanUp();

procedure ProcessRestart();

implementation

uses
  UFgui, UFbind, Uforms, Uroute, UFhide, Uavi, Ugpu, Uini, Ukey, UFshot, Umain,
  Ulang;

var
  PluginAlreadyInited: Boolean = False; 

// emergency thread!
procedure ThreadFire(Ignored: Integer); stdcall;
var
  Loops: Integer;
begin
  Ignores(Ignored);
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);
  while WaitForSingleObject(EventExtinguishFire, 0) = WAIT_TIMEOUT do
  begin
    while GetAsyncKeyState(GetAssignedHotkey(HotkeyExit)) = 0 do
      if WaitForSingleObject(EventExtinguishFire, 200) <> WAIT_TIMEOUT then
        Break; // idle when released
    if IsGuiInvoked then
      SendAction(caToggle);
    for Loops := 0 to 6 do
    begin  // wait for 200*5=1 sec to hold
      if Loops > 5 then
      begin
        GlobalCleanUp();
        TerminateProcess(GetCurrentProcess(), 0); // self-destruct!
      end;
      if WaitForSingleObject(EventExtinguishFire, 200) <> WAIT_TIMEOUT then
        Break; // todo
      if GetAsyncKeyState(GetAssignedHotkey(HotkeyExit)) = 0 then
        Break; // restore if not hold enough
    end;
    if FireDeadlock > 0 then
    begin
      Inc(FireDeadlock);
      if (FireDeadlock mod 4) = 0 then
        SkipLocks();
    end;
  end;
  SetEvent(EventGuiCreated); // reuse this
  TerminateThread(GetCurrentThread(), 0);
end;

// thread that owns plugin GUI
procedure ThreadGui(Ignored: Integer); stdcall;
begin
  Ignores(Ignored);
  try
    MainThreadID := GetCurrentThreadId(); // for Lazarus to make it happy
    if not ApplicationInitialized then
      Application.Initialize;
    ApplicationInitialized := True;
    // creating base forms
    Fhide := TFHide.Create(nil);
    Fgui := TFgui.Create(nil);
    with Fgui do
    begin
      if not IsRunDLL then
      begin
        SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or
          WS_EX_NOACTIVATE or WS_EX_TOPMOST);
        ShowWindow(Handle, SW_SHOWNOACTIVATE); // to not steal focus here
      end;
      Popup();
      Fgui.StopVisibleChanging := True;
      Show();
      Visible := False;
      SetSizeClient(0, 0, b_invoke.Width + b_invoke.Left * 2, b_invoke.Height +
        b_invoke.Top * 2, True); // calculate initial size
      ForceSize();
    end;
    Fgui.ShowModal(); // message loop here
  except
    on e: Exception do
      Report(SpyroTASName + ' ThreadGui() exception:', e.Message);
  end;
  SetEvent(EventGuiCreated);
  TerminateThread(GetCurrentThread(), 0);
end;

procedure MainInited();
begin
  History := TCustomIntegerArrays.Create(HistoriesCount);
  EventFrame := CreateEvent(nil, False, False, nil);
  EventContinueGame := CreateEvent(nil, True, False, nil);
  EventEmulatorTrap := CreateEvent(nil, True, False, nil);
  EventEmulatorWait := CreateEvent(nil, True, False, nil);
  EventGuiCreated := CreateEvent(nil, True, False, nil);
  HThreadGui := CreateThread(nil, 0, @ThreadGui, nil, 0, IThreadGui);
  WaitForSingleObject(EventGuiCreated, INFINITE);
end;

procedure GlobalCleanUp();
const
  TimeToWait: Integer = 1000;
var
  ExitCode: Cardinal;
begin
  if InterlockedDecrement(IsOneCleanup) < 0 then
    Exit;
  GpuWatching := False;
  GuiClosing := True;
  ResetEvent(EventGuiCreated);
  ExitCode := 0;
  if (HThreadGui <> 0) and GetExitCodeThread(HThreadGui, ExitCode) and (ExitCode
    = STILL_ACTIVE) then
  begin
    SendAction(caExit);
    if GetCurrentThreadId() <> IThreadGui then
    begin
      WaitForSingleObject(EventGuiCreated, TimeToWait);
      TerminateThread(HThreadGui, 0);
    end;
  end;
  if (HThreadFire <> 0) and GetExitCodeThread(HThreadFire, ExitCode) and (ExitCode
    = STILL_ACTIVE) then
  begin
    ResetEvent(EventGuiCreated);
    SetEvent(EventExtinguishFire);
    if GetCurrentThreadId() <> IThreadFire then
    begin
      WaitForSingleObject(EventGuiCreated, TimeToWait);
      TerminateThread(HThreadFire, 0);
    end;
  end;
  SetEvent(EventMainDone);
  InterlockedDecrement(IsOneMain);
  TrapRequested := False;
  SetEvent(EventEmulatorWait);
  SkipLocks();
  WaitForSingleObject(EventMainDone, TimeToWait);
  GpuRecord(False);

  SaveSettings();

  if History <> nil then
    History.Free();
  if EventContinueGame <> 0 then
    CloseHandle(EventContinueGame);
  if EventEmulatorTrap <> 0 then
    CloseHandle(EventEmulatorTrap);
  if EventEmulatorWait <> 0 then
    CloseHandle(EventEmulatorWait);
  if EventFrame <> 0 then
    CloseHandle(EventFrame);
  if EventGuiCreated <> 0 then
    CloseHandle(EventGuiCreated);
  if EventExtinguishFire <> 0 then
    CloseHandle(EventExtinguishFire);
  if EventMainDone <> 0 then
    CloseHandle(EventMainDone);
  if HThreadFire <> 0 then
    CloseHandle(HThreadFire);
  if HThreadGui <> 0 then
    CloseHandle(HThreadGui);

  if ShotPath <> '' then
    RemoveDir(ShotPath);
  ShotPath := '';
  avi_close();
end;

procedure ProcessRestart();
var
  Cmd: WideString;
  {$IFDEF FPC}
  Si: STARTUPINFOW;
  {$ELSE}
  Si: STARTUPINFO;
  {$ENDIF}
  Pi: PROCESS_INFORMATION;
begin
  Cmd := GetCommandLineW();
  ZeroMemory(@Si, SizeOf(Si));
  ZeroMemory(@Pi, SizeOf(Pi));
  CreateProcessW(nil, PWideChar(Cmd), nil, nil, False, 0, nil, nil, Si, Pi);
  TerminateProcess(GetCurrentProcess(), 0);
end;

procedure InitPlugin(StandAlone: Boolean = False);
var
  TargetGpu, TargetPad: string;
  Data: TPairsArray;
  Lang: Integer;
begin
  if PluginAlreadyInited then
    Exit;
  PluginAlreadyInited := True;
  CWPush();
  try
    IsRunDLL := StandAlone;
    if StandAlone then
      EmulatorRoot := PathFixBackslash(GetCurrentDir())
    else
      EmulatorRoot := PathFixBackslash(ExtractFilePath(ParamStr(0)));
    PathToSpyroTAS := PathFixBackslash(EmulatorRoot + SpyroTASDirectory); // todo
    CreateDir(PathToSpyroTAS); // if not exists!
    CreateDir(PathToSpyroTAS + SpyroTASForScreenshots);
    CreateDir(PathToSpyroTAS + SpyroTASForHistory);
    PathToIni := PathToSpyroTAS + SpyroTASGlobalIni;
    AssignHotkey(HotkeyExit, StrToIntDef(IniValueUpdate(PathToIni, PortSections[3],
      HotkeyNames[HotkeyExit]), HotkeyExitDefault));
    SetLength(Data, 5);
    Data[0] := MakePair('gpu_plugin', ''); // todo make separate func
    Data[1] := MakePair('dir_states', '.\sstates\');
    Data[2] := MakePair('warp_size', 16);
    Data[3] := MakePair('pad_plugin', '');
    Data[4] := MakePair('tas_lang', 0);
    IniSectionUpdate(PathToIni, SectionSpyrotas, Data, False);
    GetPair(Data[0], TargetGpu);
    GetPair(Data[1], SavestateDirectory);
    GetPair(Data[2], MoveLimitMb);
    GetPair(Data[3], TargetPad);
    GetPair(Data[4], Lang);
    LanguageCurrent := TLanguage(Lang);
    IniValueUpdate(PathToIni, SectionSpyrotas, 'dll_path', GetModuleName(HInstance));
    SavestateDirectory := EnsureAbsolutePath(SavestateDirectory);
    DoDllLoad(TargetGpu, TargetPad); // load real plugin if there is one
  except
    on e: Exception do
      Report(SpyroTASName + ' real plugin load exception:', e.Message);
  end;
  CWPop();
end;

// called from DLL initialization:
procedure ThisDllInit(Reason: Integer);
begin
  if Reason = DLL_PROCESS_DETACH then
  begin
    GlobalCleanUp();
    Exit;
  end;
  if Reason <> DLL_PROCESS_ATTACH then
    Exit;
  if (SizeOf(Integer) <> 4) or (SizeOf(Pointer) <> 4) then
  {%H-}  begin
    Report(SpyroTASName,
      'Internal error, this DLL must be compiled with 32-bit Integer and Pointer type!');
    GlobalCleanUp();
    Exit;
  end;
  PluginCorrectVersion := (1 shl 16) or (SpyroTASVersionMajor shl 8) or
    SpyroTASVersionMinor; // craft our version
  IsMultiThread := True; // delphi rtl
  EventExtinguishFire := CreateEvent(nil, True, False, nil);
  EventMainDone := CreateEvent(nil, True, False, nil);
  AssignHotkey(HotkeyExit, HotkeyExitDefault);
  HThreadFire := CreateThread(nil, 0, @ThreadFire, nil, 0, IThreadFire); // create an emergency thread
  SelfDllTime := {%H-}GetTickCount(); // to detect self DLL
end;

end.

// EOF


