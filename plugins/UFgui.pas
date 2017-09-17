unit UFgui; // SpyroTAS is licensed under WTFPL

// main form, initial also

interface

uses
  Utas, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, Spin, ShellAPI, Uforms;

// here will be one panel that is used as a target for dropping files:
type
  TPanel = class(ExtCtrls.TPanel)
  protected
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
  public
  end;

const
  WM_USER_SendAction = WM_USER + 10; // for SendAction()

type
  TFgui = class(TFSpyroTAS)
    pnl_main: TPanel;
    timer_main: TTimer;
    e_position: TEdit;
    se_fps: TSpinEdit;
    b_halt: TButton;
    b_load: TButton;
    lst_history: TListBox;
    c_limit: TCheckBox;
    b_delete: TButton;
    b_free: TButton;
    b_restart: TButton;
    se_timepoint: TSpinEdit;
    b_current: TButton;
    c_hash: TCheckBox;
    b_ext: TButton;
    se_threshold: TSpinEdit;
    c_semi: TCheckBox;
    c_shot: TCheckBox;
    pnl_drop: TPanel;
    shp_status: TShape;
    c_2nd: TCheckBox;
    c_auto: TCheckBox;
    b_keys: TButton;
    b_save: TButton;
    b_capt: TButton;
    c_skip: TCheckBox;
    pnl_init: TPanel;
    b_invoke: TButton;
    b_hints: TButton;
    c_warp: TCheckBox;
    procedure ShowExceptions(Sender: TObject; E: Exception);
    procedure FormCreate(Sender: TObject);
    procedure b_invokeClick(Sender: TObject);
    procedure timer_mainTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure se_fpsChange(Sender: TObject);
    procedure b_haltClick(Sender: TObject);
    procedure b_loadClick(Sender: TObject);
    procedure c_limitClick(Sender: TObject);
    procedure b_deleteClick(Sender: TObject);
    procedure b_restartClick(Sender: TObject);
    procedure shp_statusMouseDown(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure b_freeClick(Sender: TObject);
    procedure lst_historyDblClick(Sender: TObject);
    procedure se_timepointChange(Sender: TObject);
    procedure b_currentClick(Sender: TObject);
    procedure c_hashClick(Sender: TObject);
    procedure b_extClick(Sender: TObject);
    procedure se_thresholdChange(Sender: TObject);
    procedure c_semiClick(Sender: TObject);
    procedure c_shotClick(Sender: TObject);
    procedure c_autoClick(Sender: TObject);
    procedure c_2ndClick(Sender: TObject);
    procedure b_keysClick(Sender: TObject);
    procedure SendActionHandler(var MSG: TMessage); message WM_USER_SendAction;
    procedure b_saveClick(Sender: TObject);
    procedure b_captClick(Sender: TObject);
    procedure c_skipClick(Sender: TObject);
    procedure b_captContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure b_freeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure b_loadContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cb_anyDropDown(Sender: TObject);
    procedure lst_historyClick(Sender: TObject);
    procedure b_extContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure b_hintsClick(Sender: TObject);
    procedure b_keysContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure lst_historyContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
    procedure b_restartContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
    procedure b_saveContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure b_hintsContextPopup(Sender: TObject; MousePos: TPoint; var Handled:
      Boolean);
    procedure e_positionDblClick(Sender: TObject);
    procedure b_haltContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure b_invokeContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
    procedure c_warpClick(Sender: TObject);
    procedure b_currentContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
    procedure b_deleteContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
  private
    WidthAfterInvoke: Integer;
    HeightAfterInvoke: Integer;
    StopPoping: Boolean;
    WaitForSavestate: Boolean;
    ToldGuiCreated: Boolean;
    InvokeTime: Integer;
  public
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShowHint(var HintStr: string; var CanShow: Boolean; var
      HintInfo: THintInfo);
    procedure Invocation();
  end;

var
  Fgui: TFgui = nil;
  LastHint: TSpyroTASHint;

type
  TCustomAction = (caExit, caFrame, caSetStatus, caPositions, caFreeze,
    caCaption, caSpyro, caListDirty, caFocus, caGuiInit, caToggle, caPopup, caReload);

type
  TColorStatus = (StatusFree, StatusGui, StatusPlay, StatusRecord,
    StatusHashMiss, StatusLoading, StatusSaving, StatusKeys, StatusLost,
    StatusWrong, StatusGpuMovie);

const
  KeystatePoslist = -1;
  KeystateHistory = -2;
  KeystateHashes = -3;
  KeystateRerecords = -4;
  KeystateMinimalIndex = KeystateRerecords;

var
  CurrentStatus: TColorStatus; // displayed in panel; also sometimes directly checked
  TargetEmulationFps: Integer; // frame limit, as entered in the form
  UseAutofire: Boolean; // "auto" state
  UseHashing: Boolean; // "hash" state
  UseSecond: Boolean; // "2-nd" state
  UseLimit: Boolean; // "limit" state
  UseSkip: Boolean; // "skip" state

procedure SendAction(Action: TCustomAction; Value: Integer = 0);

procedure ReloadKeystateList();

function NameOfKeystate(Index: Integer): string;

procedure SetColorStatus(NewStatus: TColorStatus);

implementation

{$R *.dfm}

uses
  Ulang, UFhide, UFbind, UFview, UFext, UFover, Uavi, UFshot, UFedit, Uglob,
  Ugpu, UFplug, Uini, Ukey, Umain, Umisc, Uroute, Math;


// update for colored rect:
procedure SetColorStatus(NewStatus: TColorStatus);
var
  Color: TColor;
begin
  Color := clNone;
//  if ThisIsRestart then    Color := clRed  else
  case NewStatus of
    StatusPlay:
      Color := clBlue; // replay already recorded history
    StatusLoading:
      Color := clYellow; // loadstate started but not done yet
    StatusSaving:
      Color := clRed; // savestate started but not done yet
    StatusRecord:
      Color := clLime; // recording history
    StatusFree:
      Color := clBlack; // free mode active
    StatusHashMiss:
      Color := clFuchsia; // hash mismatch during replaying
    StatusKeys:
      Color := clAqua; // some semi-keys re-recorded while replaying
    StatusGui:
      Color := clWhite; // game is frozen
    StatusLost:
      Color := clLtGray; // emulator window not found
    StatusWrong:
      begin
        Color := clDkGray; // something wrong with frame counter in memory
        HistoryWasAltered := True;
      end;
    StatusGpuMovie:
      Color := RGB(255, 128, 0);
  end;
  SendAction(caSetStatus, Color); // post color update
  CurrentStatus := NewStatus;
end;

// Path to a keystate file by index:
function NameOfKeystate(Index: Integer): string;
begin
  Result := PathToSpyroTAS + SpyroTASForHistory + 'tmp_' + IntToStr(Index) + '.tmp';
end;

// TODO
function GetLastSavestateName(out LastTime: Integer): string;
var
  Search: TSearchRec;
  Ext: string;
  Index: Integer;
  CurrentTime: Integer;
  SstatesDir: string;
begin
  Result := '';
  LastTime := 0;
  SstatesDir := PathFixBackslash(SavestateDirectory);
  if not DirectoryExists(SstatesDir) then
    Exit;
  if FindFirst(SstatesDir + '*.0??', faAnyFile, Search) = 0 then
    repeat
      Ext := ExtractFileExt(Search.Name);
      if Length(Ext) > 3 then
      begin
        Delete(Ext, 1, 1);
        if TryStrToInt(Ext, Index) then
        begin
          CurrentTime := Search.Time;
          if (LastTime = 0) or (CurrentTime - LastTime > 0) then
          begin
            LastTime := CurrentTime;
            Result := Search.Name;
          end;
        end;
      end;
    until FindNext(Search) <> 0;
  FindClose(Search);
end;
  
// seve current history to files and refresh list on form:
procedure RewriteKeystateList();
var
  Index, Size: Integer;
  FilePos: TextFile; // plaint text
  Stream: TFileStream; // binary
begin
  AssignFile(FilePos, NameOfKeystate(KeystatePoslist));
  Stream := nil;
  try
    Rewrite(FilePos); // saving keystate list
    try
      Fgui.lst_history.Items.BeginUpdate();
      Fgui.lst_history.Items.Clear(); // we'll fill visual list from scratch
      Write(FilePos, Length(KeystateList), ' '); // first store count
      for Index := 0 to Length(KeystateList) - 1 do
      begin
        Fgui.lst_history.Items.Add(IntToStr(KeystateList[Index]));
        Write(FilePos, KeystateList[Index], ' '); // space as separator
      end;
    finally
      CloseFile(FilePos);
    end;
    Fgui.lst_history.Items.EndUpdate();
    if KeystateToLoad >= Fgui.lst_history.Items.Count then
      KeystateToLoad := Fgui.lst_history.Items.Count - 1;
    Fgui.lst_history.ItemIndex := KeystateToLoad; // highlight target line
    if Length(KeystateList) > 0 then
      Size := KeystateList[Length(KeystateList) - 1]
    else
      Size := 0;
    Stream := TFileStream.Create(NameOfKeystate(KeystateHistory), fmCreate);
    History.ToStream(HistoryKeys, Stream, Size); // save keys
    FreeAndNil(Stream);
    Stream := TFileStream.Create(NameOfKeystate(KeystateHashes), fmCreate);
    History.ToStream(HistoryHash, Stream, Size); // save hashes
    FreeAndNil(Stream);
    Stream := TFileStream.Create(NameOfKeystate(KeystateRerecords), fmCreate);
    History.ToStream(HistoryRrec, Stream, Size);
    FreeAndNil(Stream);
  except
    on E: Exception do
    begin
      FreeAndNil(Stream);
      Report(SpyroTASName + ' save error:', E.message);
    end;
  end;
end;

// read current saved history into memory:
procedure ReloadKeystateList();
var
  FilePos: TextFile; // positions stored as text
  Stream: TFileStream; // to read key and hash history
  Index, Position, Count, Cur: Integer;
begin
  if not FileExists(NameOfKeystate(KeystateHistory)) or not FileExists(NameOfKeystate
    (KeystatePoslist)) then
    Exit;
  if StartFromRecord then
    Cur := History.GetSize()
  else
    Cur := 0;
  Stream := nil;
  try
    Stream := TFileStream.Create(NameOfKeystate(KeystateHistory), fmOpenRead);
    History.SetSize(Max(Cur, Stream.Size div 4));
    History.FromStream(HistoryKeys, Stream.Size div 4, Stream);
    FreeAndNil(Stream);
    if FileExists(NameOfKeystate(KeystateHashes)) then
    begin  // read hashes
      Stream := TFileStream.Create(NameOfKeystate(KeystateHashes), fmOpenRead);
      History.FromStream(HistoryHash, Stream.Size div 4, Stream);
      FreeAndNil(Stream);
    end;
    if FileExists(NameOfKeystate(KeystateRerecords)) then
    begin
      Stream := TFileStream.Create(NameOfKeystate(KeystateRerecords), fmOpenRead);
      try
        History.FromStream(HistoryRrec, Stream.Size div 4, Stream);
      except // TODO
      end;
      FreeAndNil(Stream);
    end;
    AssignFile(FilePos, NameOfKeystate(KeystatePoslist));
    Reset(FilePos); // read keystate positions
    try
      Fgui.lst_history.Items.BeginUpdate();
      Fgui.lst_history.Items.Clear(); // list on form
      Read(FilePos, Count);
      if Count > 0 then // first number is count
      begin
        SetLength(KeystateList, Count);
        for Index := 0 to Count - 1 do
        begin
          Read(FilePos, Position);
          KeystateList[Index] := Position; // one keystate
          Fgui.lst_history.Items.Add(IntToStr(Position));
        end;
        History.SetSize(Max(History.GetSize(), KeystateList[Count - 1]) + 100);
            // +100 is just in case
      end;
    finally
      CloseFile(FilePos);
      Fgui.lst_history.Items.EndUpdate(); // list filled
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(Stream);
//      CloseFile(FilePos);
      Report(SpyroTASName + ' load error:', E.message);
    end;
  end;
end;
  
// signal to GUI thread:
procedure SendAction(Action: TCustomAction; Value: Integer = 0);
begin
  if Fgui <> nil then
    PostMessage(Fgui.Handle, WM_USER_SendAction, Integer(Action), Value);
end;

// for Delphi
procedure TPanel.CreateWnd;
begin
  inherited;
  {$IFNDEF FPC}
  if Tag = -1 then // only target panel
    DragAcceptFiles(Handle, True);
  {$ENDIF}
end;

// not really necessary:
procedure TPanel.DestroyWnd;
begin
  {$IFNDEF FPC}
  if Tag = -1 then
    DragAcceptFiles(Handle, False);
  {$ENDIF}
  inherited;
end;

// for Delphi, dropping files:
procedure TPanel.WMDropFiles(var Message: TWMDropFiles);
var
  Name: array[0..MAX_PATH - 1] of Char;
begin
  if DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0) > 0 then
  begin
    Name[0] := #0;
    if DragQueryFile(Message.Drop, 0, Name, MAX_PATH) <> 0 then
      ImportSpyroTAS(Name);
  end;
  DragFinish(Message.Drop);
end;

// for Lazarus, dropping files:
procedure TFgui.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if Length(FileNames) > 0 then
    ImportSpyroTAS(FileNames[0]);
end;

// reciever of SendAction() calls:
procedure TFgui.SendActionHandler(var MSG: TMessage);
var
  Action: TCustomAction; // requested action
  Value: Integer; // optional value
  Hand: Boolean; //ignored
begin
  Action := TCustomAction(MSG.WPARAM);
  Value := MSG.LPARAM;
  case Action of // all registred types
    caFrame: // one frame done - update counter, from MAIN()
      begin
        if (HistoryPosition = -1) or IsFreeMode then
          e_position.Text := '--' // show as not available
        else
          e_position.Text := IntToStr(HistoryPosition); // print number
        UpdateOverlays();
        if Value = 1 then
          MakeGameScreenshot(False);
        SetEvent(EventFrame); // release caller
      end;
    caSetStatus: // colored rectangle, from SetColorStatus()
      begin
        shp_status.Brush.Color := Value; // this is the color itself
        shp_status.Refresh(); // redraw
      end;
    caFreeze: // stop the game and switch to gui, called from MAIN()
      begin
        RequestSwitch();
      end;
    caPositions: // by hotkey from MAIN()
      begin
        se_timepoint.Value := LastTimepoint;
        if ThresholdTimepoint <> -1 then // both values
          se_threshold.Value := ThresholdTimepoint;
      end;
    caCaption: // todo
      begin
        Caption := '  ' + IntToStr(Value); // one integer value
      end;
    caPopup: // todo
      begin
        AllFormsAction(afaPopup);
      end;
    caSpyro: // coordinates measured, from DoSpyro()
      begin
        Fext.lst_spyro.items.Add(IntToStr(Value));
        Fext.lst_spyro.ItemIndex := Fext.lst_spyro.Count - 1; // highlight last one
      end;
    caListDirty: // need recreate visual KeystateList
      begin
        if Value = 1 then
          Fgui.lst_history.ItemIndex := KeystateToLoad // only update selected
        else
          RewriteKeystateList();
      end;
    caFocus: // focus requested, from SwitchToGui()
      begin
        BringWindowToTop(Handle);
        SetForegroundWindow(Handle);
        AllFormsAction(afaFocus);
      end;
    caGuiInit: // after invoke button, to activate black painting
      begin
        Fhide.Visible := True;
        AllFormsAction(afaGhost);
        AllFormsAction(afaHide);
        AllFormsAction(afaShow); // make sure the GUI is always on-top
      end;
    caExit: // todo
      begin
        Application.ShowHint := False;
        AllFormsAction(afaExit);
        Application.Terminate();
      end;
    caToggle: // from emergency thread, first press
      begin
        if Fhide.VisibleState then
        begin
          Fhide.FormMouseDown(nil, mbRight, [], 0, 0); // simulate
          ShowWindow(Fhide.Handle, SW_HIDE);
        end
        else
        begin
          ShowWindow(Fhide.Handle, SW_SHOW); // restore
          Fhide.FormMouseDown(nil, mbRight, [], 0, 0);
        end;
      end;
    caReload:
      begin // hotkey asks to right-click Current
        Hand := False;
        b_currentContextPopup(nil, Point(0, 0), Hand);
      end;
  end;
end;

// top level exception handler
procedure TFgui.ShowExceptions(Sender: TObject; E: Exception);
begin
  Report(SpyroTASName + ' GUI exception:', E.Message);
end;

// first call from gui thread:
procedure TFgui.FormCreate(Sender: TObject);
begin
  Fgui := Self; // initialize from variable since starting in modal
  SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST); // stronger than game
  Application.OnException := ShowExceptions; // redirect exceptions
  OnClose := FormClose;
  Application.OnShowHint := FormShowHint; // track of hints
  TimeOfStart := Now(); // for dll self-check
  timer_main.Interval := 25; // gui timer, also for bridge
  timer_main.Enabled := True;
  InitHints(True);
  PutHints1(); // hints for initial screen
  if StartFromRecord then
  begin
    Caption := '  ' + SpyroTASName + '...';
    b_invoke.Caption := 'Autoinvoke ' + SpyroTASName + SpyroTASVestionString;
    SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not
      WS_SYSMENU); // remove close button
  end
  else
  begin
    if IsRunDLL then
      Caption := '  ' + SpyroTASName + ' [rundll32]:'
    else
      Caption := '  ' + SpyroTASName + ':';
    b_invoke.Caption := 'Invoke ' + SpyroTASName + SpyroTASVestionString;
  end;
  pnl_main.Visible := False; // no general gui
  pnl_init.Visible := True; // initial controls
  pnl_init.Left := 0;
  pnl_init.Top := 0;
  WidthAfterInvoke := Width; // target Size
  HeightAfterInvoke := Height;
  pnl_init.Align := alClient; // entire form
  ActiveControl := b_invoke;
  b_invoke.Enabled := IsRunDLL;
  b_invoke.Font.Style := [fsItalic];
  SpyroUse1 := False; // initialize several variables
  SpyroUse2 := False;
  Randomize();
  if IniValueUpdate(PathToIni, SectionSpyrotas, 'hide_hints') = '1' then // when disabled global
  begin
    b_hints.Caption := '!!'; // value "!!" is also checked later
    DontShowHints := True;
  end
  else
  begin
    b_hints.Caption := '?';
    DontShowHints := False;
  end;
  GetLastSavestateName(InvokeTime);
end;

procedure TFgui.Invocation();
var
  Name: string;
  Rect: TRect;
  Diff: Integer;
begin
  if GuiClosing then
    Exit;
  MakeKeyUp(GetAssignedHotkey(EmulatorHotkeySavestate));
  // create rest of forms
  Fshot := TFshot.Create(nil);
  Fext := TFext.Create(nil);
  Fview := TFview.Create(nil);
  Fedit := TFedit.Create(nil);
  Fover1 := TFover.Create(nil);
  Fover2 := TFover.Create(nil);
  FoverF := TFover.Create(nil);
  FoverR := TFover.Create(nil);
  Fover2.IsSecondController := True;
  FoverF.InitCounter(False);
  FoverR.InitCounter(True);
  {$IFDEF FPC}
  AllowDropFiles := True; // form dropping files for Lazarus
  DragAcceptFiles(Handle, False);
  DragAcceptFiles(pnl_drop.Handle, True);
  OnDropFiles := FormDropFiles;
  {$ENDIF}
  pnl_init.Visible := False; // show general controls
  Width := WidthAfterInvoke; // correct Size
  Height := HeightAfterInvoke;
  c_semi.Checked := False;
  c_semiClick(nil); // force
  se_threshold.Value := 0;
   // need to hide window close button; "BorderIcons:=[];" have side-effects, so change style
  SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not WS_SYSMENU);
  PutHints2();
  PutHints3(); // hints to gui
  LoadSettings(); // components state
  LoadPadKeys();
  ReloadKeystateList(); // read history from disk
  if Length(KeystateList) > 0 then // if have some hitory
    SetLastestPosition(KeystateList[Length(KeystateList) - 1])
  else
    SetLastestPosition(0); // clean
  if IsRunDLL then
    Name := '[rundll32]' // standalone caption stub
  else
    Name := SavestateID; // correct
  Caption := Name + ' - ' + SpyroTASName + SpyroTASVestionString;
  Fhide.Enabled := True; // all forms initial positions
  Fhide.Show();
  Rect := Fgui.GetSizeSystem();
  Fshot.Left := Rect.Left;
  Fshot.Top := Rect.Bottom;
  Fshot.ForceSize();
  Fext.Left := Rect.Right;
  Fext.Top := Rect.Top;
  Fext.ForceSize();
  Fbind.Left := Rect.Left + ((Rect.Right - Rect.Left) div 4);
  Fbind.Top := Fgui.Top + ((Rect.Bottom - Rect.Top) div 2);
  Fbind.ForceSize();
  Rect := Fshot.GetSizeSystem();
  Fover1.Left := Rect.Right;
  Fover1.Top := Rect.Top;
  Fover1.ForceSize();
  Rect := Fover1.GetSizeSystem();
  Diff := Rect.Bottom - Rect.Top;
  Fover2.Left := Rect.Left;
  Fover2.Top := Rect.Bottom;
  Fover2.ForceSize();
  Rect := Fshot.GetSizeSystem();
  Fview.SetSizeSystem(Rect.Left, Rect.Bottom, Rect.Right - Rect.Left, Rect.Bottom
    - Rect.Top);
  Fview.ForceSize();
  Rect := Fview.GetSizeSystem();
  Fedit.Left := Fgui.Left + 4;
  Fedit.Top := Fgui.Top + 4;
  Fedit.Height := Rect.bottom - Fedit.Top;
  Fedit.ForceSize();
  Fhide.VisibleState := True;
  Rect := Fext.GetSizeSystem();
  Rect.Top := Rect.Bottom;
  Rect.Bottom := Rect.Top + Diff;
  FoverF.SetSizeSystem(Rect);
  Rect.Top := Rect.Bottom;
  Rect.Bottom := Rect.Top + Diff;
  FoverR.SetSizeSystem(Rect);
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Fhide.Handle, GWL_EXSTYLE)
    and not WS_EX_APPWINDOW);
  MoveWindow(Fhide.Handle, -32, -2, 64, 3, True); // line at very top-left
  shp_status.Brush.Color := clBlack;
  Repaint();
  pnl_main.Visible := True;
  IsGuiInvoked := True; // now we have everything
  WaitForSavestate := False;
  ChooseSemiAuto(); // just in case
  SendAction(caGuiInit); // all ok
  if StartFromRecord then
  begin // boot record mode
    if Length(KeystateList) = 0 then // start new history
      b_restartClick(nil) // with savestate preventions
    else
      b_loadClick(nil); // with loadstate prevention
    StopOnBlur := True; // set manually
  end
  else
    b_free.Click(); // normal run, start free mode
  StartFromRecord := False;
  timer_mainTimer(nil); // force
  if IsRunDLL then
    AllFormsAction(afaFocus);
end;

// "INVOKE" handler:
procedure TFgui.b_invokeClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if Fbind = nil then
    Fbind := TFbind.Create(nil);
  LoadPadKeys(); // hotkeys
  if IsRunDLL then
  begin
    Invocation();
    Exit;
  end;
  MakeKeyDown(GetAssignedHotkey(EmulatorHotkeySavestate));
  WaitForSavestate := True;
end;

// timer for bridge and control disabling:
procedure TFgui.timer_mainTimer(Sender: TObject);
var
  TopMost: TFSpyroTAS; // will be Fhide or Fgui before invoking
  Time: Integer;
begin
//Caption:=TimeToString(Now());
  if GuiClosing then
    Exit;
  TopMost := nil;
  if IsGuiInvoked then
  begin // main state
    if ((CurrentStatus <> StatusLost) and ((EmulatorWindow = 0) or (not IsWindow
      (EmulatorWindow)))) or ((EmulatorWindow <> 0) and (not IsWindowVisible(EmulatorWindow)))
      then
    begin
      SetColorStatus(StatusLost); // no emulator window?
      RequestSwitch();
      EmulatorWindow := 0;
      AllFormsAction(afaFocus);
    end;
    // maybe better is to filter those to only unexpected ones,
    // others would be forced with timer_mainTimer(nil) anyway
    if not SprintMode then
    begin
      Fext.b_store.Enabled := Length(KeystateList) > 0;
      Fext.b_export.Enabled := Fext.b_store.Enabled;
      Fext.b_unpack.Enabled := Fext.lst_arch.ItemIndex >= 0;
      Fext.b_remove.Enabled := Fext.b_unpack.Enabled;
      b_save.Enabled := (GameIsRunning or PadToggleFrame) and (not IsFreeMode)
        and not GpuWatching;
      b_restart.Enabled := ((Length(KeystateList) = 0) or not (GameIsRunning or
        PadToggleFrame)) and (SavestateID <> '');
      b_current.Enabled := (HistoryPosition <> -1);
      b_delete.Enabled := (lst_history.ItemIndex >= 0);
      b_load.Enabled := b_delete.Enabled and (SavestateID <> '');
      if not c_semi.Checked then
        ThresholdTimepoint := -1; // dirty fix
    end;
    c_2nd.Checked := UseSecond;
    TopMost := Fhide; // always in normal mode
  end
  else if Sender <> nil then // prevent internal calls
  begin
    if not ToldGuiCreated then
    begin
      SetEvent(EventGuiCreated);
      ToldGuiCreated := True;
    end;
    if not StopPoping then
      TopMost := Self; // early moment, for fullscreen emulation
    if (RamStartRequest <> nil) and not WaitForSavestate then
    begin
      b_invoke.Font.Style := [fsBold, fsUnderline];
      if StartFromRecord then
      begin
        b_invoke.Enabled := True;
        b_invoke.Click(); // can be commented out to make a robus test
      end
      else if not IsRunDLL then
        b_invoke.Enabled := IsOurWindow(GetForegroundWindow());
    end;
    if WaitForSavestate and not IsRunDLL then
    begin
      SavestateID := GetLastSavestateName(Time);
      if (SavestateID <> '') and (Time > InvokeTime) then
        Invocation();
    end;
  end;
  if TopMost <> nil then
    TopMost.Popup(); // very bad practice, but quite effective!..
  if (LastHint <> nil) and (not IsWindowVisible(LastHint.Handle)) then
  begin
    LastHint.DoWipe(); // black painting for closed hints
    LastHint := nil;
  end;
end;

// additional form closing prevention
procedure TFgui.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not IsGuiInvoked then
  begin
    CanClose := True; // can be closed before invoking
    Exit;
  end;
  CanClose := False; // no way to close after
end;

// change game fps handler:
procedure TFgui.se_fpsChange(Sender: TObject);
begin
  TargetEmulationFps := se_fps.Value;
  if TargetEmulationFps > 0 then
    TimeFromFps := Round(1000 / TargetEmulationFps);
  SaveSettingsScheduled := True;
end;

// "HALT" handler:
procedure TFgui.b_haltClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  GlobalCleanUp();
  if Sender <> nil then // to allow controlled, for boot record restarting
    TerminateProcess(GetCurrentProcess(), 0); // hard way
end;

// "LOAD" handler:
procedure TFgui.b_loadClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  SaveSettings();
  SprintSave := 0;
  if SprintMode then
  begin
    if not FileExists(SprintState) then
      Exit;
  end
  else if Sender <> nil then // skip if called from boot record time
  begin
    if lst_history.ItemIndex < 0 then
      Exit;
    if not FileExists(NameOfKeystate(lst_history.ItemIndex)) then
      Exit;
  end;
  // magic stuff #1
  TemporaryDisable(Sender);
  SwitchToEmulator(True);
  ResetLoadSave();
  HistoryWasAltered := False;
  KeystateToLoad := Self.lst_history.ItemIndex;
  LoadInFree := False;
  IsFreeMode := True;
  if Sender <> nil then
  begin
    LoadRequsted := True; // if normal call
    SetHistoryPosition(-1);
  end
  else
  begin
    IsFreeMode := False; // for boot time
  end;
  if SprintMode then
  begin
    KeystateToLoad := SprintValue;
    SetHistoryPosition(SprintValue);
  end;
  SwitchToEmulator(False);
  TemporaryDisable();
  timer_mainTimer(nil);
end;

// "LIMIT" handler:
procedure TFgui.c_limitClick(Sender: TObject);
begin
  se_fpsChange(Sender);
  UseLimit := c_limit.Checked;
  timer_mainTimer(nil);
end;

// "DELETE" handler:
procedure TFgui.b_deleteClick(Sender: TObject);
var
  Index, Next: Integer;
  NeedGame: Boolean;
begin
  if SprintMode then
    Exit;
  if GuiClosing then
    Exit;
  if lst_history.ItemIndex < 0 then
    Exit; // nothing to delete anyway
  NeedGame := GameIsRunning;
  if NeedGame then // allow deleting while running
    SwitchToEmulator(True);
  if Sender = nil then
  begin // call from restart, delete all keystates
    for Index := Length(KeystateList) - 1 downto lst_history.ItemIndex do
      DeleteFile(NameOfKeystate(Index));
    SetLength(KeystateList, 0);
  end
  else
  begin // normal press
    Index := lst_history.ItemIndex; // selected
    DeleteFile(NameOfKeystate(Index));
    for Next := Index to lst_history.Count - 2 do
    begin // wave renaming
      RenameFile(NameOfKeystate(Next + 1), NameOfKeystate(Next));
      KeystateList[Next] := KeystateList[Next + 1];
    end;
    SetLength(KeystateList, lst_history.Count - 1); // one less
    KeystateToLoad := Index;
  end;
  SendAction(caListDirty); // update
  if NeedGame then
    SwitchToEmulator(False); // paired
  timer_mainTimer(nil);
end;

// "RESTART" handler:
procedure TFgui.b_restartClick(Sender: TObject);
begin
  if SprintMode then
  begin
    SprintSave := 0;
    SetLastestPosition(0);
    b_load.Click();
    Exit;
  end;
  if GuiClosing then
    Exit;
  SaveSettings();
  if lst_history.Count > 0 then
  begin // delete all keystates
    lst_history.ItemIndex := 0;
    b_deleteClick(nil);
  end;
  // magic stuff #2
  TemporaryDisable(Sender);
  SwitchToEmulator(True);
  HistoryWasAltered := False;
  ResetLoadSave();
  KeystateToLoad := 0;
  SetLastestPosition(0);
  SetThresholdPosition(0);
  LoadInFree := False;
  IsFreeMode := False;
  if Sender <> nil then
  begin
    SetColorStatus(StatusSaving);
    SaveRequsted := True; // normal
    SetHistoryPosition(0);
    History.SetSize(1);
    ThisIsRestart := True;
  end;
  SwitchToEmulator(False);
  TemporaryDisable();
  timer_mainTimer(nil);
end;

// status panel left click handler:
procedure TFgui.shp_statusMouseDown(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  if GuiClosing then
    Exit;
  Ignores(Button, Shift, X, Y);
  SaveSettings();
  if Button = mbLeft then
  begin
    if GpuWatching then
    begin
      GpuWatching := False;
      AdvanceRequested := False;
      Exit;
    end;
    if PadToggleFrame then
      PadToggleFrame := False
    else
      RequestSwitch(True);
  end
  else if Button = mbRight then
  begin
    if GpuWatching then
    begin
      WatchNextRequested := True;
      Exit;
    end
    else
      AdvanceRequested := True
  end
  else if Button = mbMiddle then
  begin
    WatchFlipRequested := True;
    WarpRequested := True;
    Exit;
  end;
  GpuWatching := False;
end;

// "FREE" handler:
procedure TFgui.b_freeClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  SaveSettings();
  SprintSave := 0;
  // magic stuff #3
  TemporaryDisable(Sender);
  SwitchToEmulator(True);
  ResetLoadSave();
  SaveRequsted := False;
  IsFreeMode := True;
  SetHistoryPosition(-1);
  ThisIsRestart := False;
  SwitchToEmulator(False);
  timer_mainTimer(nil);
  TemporaryDisable();
end;

// double click to history:
procedure TFgui.lst_historyDblClick(Sender: TObject);
begin
  if Length(KeystateList) < 0 then
    Exit; // no keystates
  SetLastestPosition(KeystateList[Length(KeystateList) - 1]);
  b_load.Click(); // loading immediately
end;

// bottom editbox handler:
procedure TFgui.se_timepointChange(Sender: TObject);
begin
  if SprintMode then
    Exit;
  LastTimepoint := se_timepoint.Value;
end;

// "CURRENT" handler:
procedure TFgui.b_currentClick(Sender: TObject);
var
  NeedGame: Boolean;
begin
  if SprintMode then
  begin
    EnterSptintMode(False);
    ReloadKeystateList();
    b_free.Click();
    Exit;
  end;
  if GuiClosing then
    Exit;
  if HistoryPosition = -1 then
    Exit;
  TemporaryDisable(Sender);
  NeedGame := GameIsRunning; // allow changing during play
  if NeedGame then
    SwitchToEmulator(True);
  if c_semi.Checked then
    SetThresholdPosition(HistoryPosition) // upper
  else
    SetLastestPosition(HistoryPosition); // bottom
  if NeedGame then
    SwitchToEmulator(False);
  TemporaryDisable();
  timer_mainTimer(nil);
end;

// "HASH" handler:
procedure TFgui.c_hashClick(Sender: TObject);
begin
  UseHashing := c_hash.checked;
  SaveSettingsScheduled := True;
  timer_mainTimer(nil);
end;

// "[EXT]" handler:
procedure TFgui.b_extClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if Fext.Visible then // toggle
    Fext.Hide()
  else
    Fext.Show();
end;

// upper editbox handler:
procedure TFgui.se_thresholdChange(Sender: TObject);
begin
  if Enabled then // only in semi mode
    ThresholdTimepoint := se_threshold.Value;
end;

// "SEMI" handler:
procedure TFgui.c_semiClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  SemiKeysUsed := c_semi.Checked; // new state
  if SemiKeysUsed then
    ThresholdTimepoint := se_threshold.Value // read current
  else
    ThresholdTimepoint := -1; // store special
  ChooseSemiAuto();
  timer_mainTimer(nil);
end;

// "SHOT" handler:
procedure TFgui.c_shotClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if c_shot.Checked then
  begin // start capture
    if (ShotType = stAvi) and not AviConfigured then
    begin // no codec
      Fshot.b_codec.Click(); // try to get one now
      if not AviConfigured then
      begin // still none
        c_shot.Checked := False;
        Exit;
      end;
    end;
    with Fshot do
    begin // disable some capture settings while recording
      b_codec.Enabled := False;
      r_bmp.Enabled := False;
      r_png.Enabled := False;
      r_avi.Enabled := False;
      se_avi.Enabled := False;
      c_ever.Enabled := False;
      b_combine.Enabled := False;
    end;
    ShotPath := TimeToString(Now()); // capture name
    if ShotType = stAvi then
    begin // video
      ShotPath := SpyroTASForScreenshots + 'AVI_' + ShotPath + '.avi';
      Fview.Caption := '\' + ShotPath;
      ShotPath := PathToSpyroTAS + ShotPath; // target filename
      if not avi_create(ShotPath) then
      begin
        c_shot.Checked := False;
        Report(SpyroTASName + ' AVI codec error!', PChar(ShotPath), Fgui.Handle);
      end;
    end
    else if ShotType = stPng then
    begin // png
      ShotPath := SpyroTASForScreenshots + 'PNG_' + ShotPath + '\';
      Fview.Caption := '\' + ShotPath + '*.png';
      ShotPath := PathToSpyroTAS + ShotPath;
      CreateDir(ShotPath);
    end
    else if ShotType = stBmp then
    begin // bmp, almost as above
      ShotPath := SpyroTASForScreenshots + 'BMP_' + ShotPath + '\';
      Fview.Caption := '\' + ShotPath + '*.bmp';
      ShotPath := PathToSpyroTAS + ShotPath;
      CreateDir(ShotPath);
    end;
    ShotCount := 0; // initial
    ShotNeed := True; // request capture
  end
  else
  begin // stop capture
    ShotNeed := False; // disable screnshoting
    with Fshot do
    begin // enable settings back
      b_codec.Enabled := True;
      r_bmp.Enabled := True;
      r_png.Enabled := True;
      r_avi.Enabled := True;
      se_avi.Enabled := True;
      c_ever.Enabled := True;
      b_combine.Enabled := True;
    end;
    if ShotType = stAvi then
      avi_close() // finalize video
    else if ShotPath <> '' then
      RemoveDir(ShotPath); // only if empty!
    ShotPath := '';
    Fview.Repaint(); // clear preview
    Fview.Caption := '';
  end;
  timer_mainTimer(nil);
end;

// "AUTO" handler:
procedure TFgui.c_autoClick(Sender: TObject);
begin
  UseAutofire := c_auto.Checked;
  ChooseSemiAuto();
  timer_mainTimer(nil);
end;

// "2-ND" hander:
procedure TFgui.c_2ndClick(Sender: TObject);
begin
  UseSecond := c_2nd.Checked;
  SaveSettingsScheduled := True;
  timer_mainTimer(nil);
end;

// "[KEYS]" handler:
procedure TFgui.b_keysClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if Fbind.Visible then // toggle
    Fbind.Hide()
  else
    Fbind.Show();
end;
     
// "SAVE" handler:
procedure TFgui.b_saveClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  SaveSettings();
  TemporaryDisable(Sender);
  SwitchToEmulator(True);
  ResetLoadSave();
  HistoryWasAltered := False;
  SaveRequsted := True; // no magic, just a request
  SwitchToEmulator(False);
  TemporaryDisable();
  timer_mainTimer(nil);
end;

// "[CAPT] handler:
procedure TFgui.b_captClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if Fshot.Visible then // toggle
    Fshot.Hide()
  else
    Fshot.Show();
end;

procedure TFgui.c_skipClick(Sender: TObject);
begin
  UseSkip := c_skip.Checked;
  timer_mainTimer(nil);
end;

procedure TFgui.b_captContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Fshot.b_overContextPopup(Sender, MousePos, Handled);
end;

procedure TFgui.b_freeContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  if SprintMode then
    Exit;
  if GuiClosing then
    Exit;
  Ignores(MousePos);
  Handled := True;
  SaveSettings();
  if FileExists(NameOfKeystate(0)) then
  begin
    TemporaryDisable(Sender);
    SwitchToEmulator(True);
    HistoryWasAltered := False;
    KeystateToLoad := -1; // !!
    LoadRequsted := True;
    LoadInFree := True;
    IsFreeMode := True;
    SetHistoryPosition(-1);
    SwitchToEmulator(False);
    TemporaryDisable();
  end;
  timer_mainTimer(nil);
end;

procedure TFgui.b_loadContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  if SprintMode then
    Exit;
  if GuiClosing then
    Exit;
  if lst_history.ItemIndex < 0 then
    Exit;
  Ignores(MousePos);
  Handled := True;
  if FileExists(NameOfKeystate(lst_history.ItemIndex)) then
  begin
    TemporaryDisable(Sender);
    SaveSettings();
    SwitchToEmulator(True);
    HistoryWasAltered := False;
    KeystateToLoad := Self.lst_history.ItemIndex;
    LoadRequsted := True;
    LoadInFree := True;
    SetHistoryPosition(-1);
    KeystateUsedToStart := KeystateToLoad;
    IsFreeMode := True;
    SwitchToEmulator(False);
    TemporaryDisable();
  end;
  timer_mainTimer(nil);
end;

procedure TFgui.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Ignores(Action);
  if IsRunDLL then
    ExitProcess(0);
end;

procedure TFgui.cb_anyDropDown(Sender: TObject);
begin
  StopPoping := True;
end;

procedure TFgui.FormShowHint(var HintStr: string; var CanShow: Boolean; var
  HintInfo: THintInfo);
begin
  Ignores(HintStr, HintInfo);
  if DontShowHints then
    CanShow := False;
  StopPoping := True;
end;

procedure TFgui.lst_historyClick(Sender: TObject);
begin
  timer_mainTimer(nil);
end;

procedure TFgui.b_extContextPopup(Sender: TObject; MousePos: TPoint; var Handled:
  Boolean);
begin
  if SprintMode then
    Exit;
  Ignores(MousePos);
  Handled := True;
  TemporaryDisable(Sender);
  Fext.b_storeClick(nil);
  TemporaryDisable();
end;

procedure TFgui.b_hintsClick(Sender: TObject);
begin
  DontShowHints := not DontShowHints;
  if DontShowHints then
    b_hints.Caption := '!'
  else
  begin
    if b_hints.Caption = '!!' then // if globally disabled
      IniValueUpdate(PathToIni, SectionSpyrotas, 'hide_hints', '0');
    b_hints.Caption := '?';
  end;
end;

procedure TFgui.b_keysContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
  Fshot.b_over.Click;
end;

procedure TFgui.lst_historyContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
var
  Item: Integer;
begin
  Item := lst_history.ItemAtPos(MousePos, True);
  if Item >= 0 then
    lst_history.ItemIndex := Item;
  b_load.OnContextPopup(Sender, MousePos, Handled);
end;

procedure TFgui.b_restartContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  if SprintMode then
    Exit;
  if GuiClosing then
    Exit;
  Ignores(MousePos);
  Handled := True;
  if IsRunDLL then
    Exit;
  IniValueUpdate(PathToIni, SectionSpyrotas, 'boot_play', '1');
  b_haltClick(nil);
  ProcessRestart();
end;

procedure TFgui.b_saveContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  if SprintMode then
    Exit;
  Ignores(MousePos);
  Handled := True;
  if GuiClosing then
    Exit;
  if IsRunDLL then
    Exit;
  if LastTimepoint > 0 then
  begin
    TemporaryDisable(Sender);
    SwitchToEmulator(True);
    SaveRequsted := True;
    SpecSaveRequest := True;
    SwitchToEmulator(False);
    TemporaryDisable();
  end;
end;

procedure TFgui.b_hintsContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
  if DontShowHints then
  begin // set global option
    IniValueUpdate(PathToIni, SectionSpyrotas, 'hide_hints', '1');
    b_hints.Caption := '!!'; // tested when enabling hints
  end;
end;

procedure TFgui.e_positionDblClick(Sender: TObject);
begin
  if Fedit.Visible then
    Fedit.SetFocus()
  else
  begin
    Fedit.m_history.Lines.Clear();
    Fedit.Show();
    Fedit.b_reload.Click();
  end;
end;

procedure TFgui.b_haltContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
  if IsRunDLL then
    Exit;
  GlobalCleanUp();
end;

procedure TFgui.b_invokeContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  if GuiClosing or StartFromRecord or IsRunDLL then
    Exit;
  Ignores(MousePos);
  Handled := True;
  Fplug := TFplug.CreateUsual();
  Fplug.ShowModal();
  FreeAndNil(Fplug);
  PutHints1();
end;

procedure TFgui.c_warpClick(Sender: TObject);
begin
  SaveSettingsScheduled := True;
  AllowWarp := c_warp.Checked;
  GpuWatching := False;
  if AllowWarp then
    DoWarpNow := True;
end;

// TODO
procedure TFgui.b_currentContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  if SprintMode then
    Exit;
  Ignores(MousePos);
  Handled := True;
  ReloadKeystateList();
  if lst_history.Count > 0 then
  begin
    lst_history.ItemIndex := 0;
    lst_historyDblClick(nil);
  end;
end;

procedure TFgui.b_deleteContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  if SprintMode then
    Exit;
  Ignores(MousePos);
  Handled := True;
  if GuiClosing then
    Exit;
  if lst_history.ItemIndex < 0 then
    Exit;
  SwitchToEmulator(True);
  RequestSwitch(True);
  SwitchToEmulator(False);
  Fext.b_exportClick(Sender);
  if SprintMode then
  begin
    b_delete.Enabled := False;
    b_restart.Click();
  end;
end;

end.

