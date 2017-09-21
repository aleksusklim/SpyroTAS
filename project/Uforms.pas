unit Uforms; // SpyroTAS is licensed under WTFPL

// overrides form class

interface

uses
  Utas, Forms, Windows, Messages, Controls, StdCtrls, ExtCtrls, Graphics, Grids,
  Classes, SysUtils
  {$IFDEF FPC}
    , LCLType
  {$ELSE} // fix for TCreateParams
    , Types
  {$ENDIF};

const
  CUSTOM_MESSAGE_DOWIPE = WM_USER + 1;

type
  TFSpyroTAS = class(TForm)
    constructor Create(Owner: TComponent); override;
    constructor CreateUsual();
    procedure SetZOrder(Topmost: Boolean); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Hide(StoreState: Boolean = False);
    procedure Show(RetoreState: Boolean = False);
    procedure WipeThisForm();
    procedure DoWipe(var MSG: TMessage); message CUSTOM_MESSAGE_DOWIPE;
    procedure WMpos(var MSG: TMessage); message WM_WINDOWPOSCHANGING;
    procedure DefOnClose(Sender: TObject; var Action: TCloseAction);
    procedure SetPermanentHint(Control: TControl; Text: string);
    procedure SetPermanentGridHint(Grid: TStringGrid; Col, Row: Integer; Text: string);
  public
    procedure SetSizeClient(ALeft, Atop, AWidth, AHeight: Integer; OnlySize:
      Boolean); overload;
    procedure SetSizeSystem(ALeft, Atop, AWidth, AHeight: Integer); overload;
    procedure SetSizeSystem(Rect: TRect); overload;
    procedure SetSizeClient(Rect: TRect; OnlySize: Boolean); overload;
    function GetSizeClient(TargetWindow: Integer = 0): TRect;
    function GetSizeSystem(): TRect;
    procedure ForceSize();
    procedure TemporaryDisable(Sender: TObject = nil);
    procedure Popup();
    procedure TopMost(OnTop: Boolean);
    procedure Report(Caption, Text: string); overload;
    procedure Report(Caption, Text: string; Handle: HWND); overload;
  public
    StopVisibleChanging: Boolean;
    StopWipingHappen: Boolean;
    ThisIsOverlay: Boolean;
  private
    DoNotMessAround: Boolean; // to disable most of this class
    TemporaryDisabled: TControl;
    TemporaryDisableTime: Integer;
    WipeCounter: Integer;
    OldPos: TRect; // form position before move/resize
    OldVisible: Boolean;
  end;

  // customizing hints:
type
  TSpyroTASHint = class(THintWindow)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  private
    OldPos: TRect; // for wiping
    WasWiped: Boolean;
  public
    procedure Paint; override;
    procedure DoWipe();
  end;

var
  TSpyroTASHintOkay: Boolean = False;

type
  TAllFormsAction = (afaShow, afaHide, afaFocus, afaGhost, afaEnable, afaDisable,
    afaExit, afaPopup);

procedure AllFormsAction(Action: TAllFormsAction); // called from GUI

var
  HintsFont: string = 'Tahoma';
  HintsCharset: TFontCharset = ANSI_CHARSET;
  HintsSize: Integer = 10;

implementation

uses
  UFgui, UFhide, UFbind, UFview, UFext, UFover, UFshot, UFedit, Umain, UFplug,
  Umisc;

var
  InFocusedMode: Boolean; // for internal branching

// to have one function instead of many:

procedure AllFormsAction(Action: TAllFormsAction);

  procedure Process(OneForm: TFSpyroTAS); // (inner scope), called for each form
  begin
    if OneForm = nil then
      Exit;
    if Action = afaExit then
    begin
      OneForm.DoNotMessAround := True;
      OneForm.OnCloseQuery := nil;
      OneForm.OnClose := nil;
      OneForm.Close();
      OneForm.Release();
      Exit;
    end;
    if (Action <> afaEnable) and not OneForm.Enabled then
      Exit; // skipping disabled
    case Action of
      afaShow: // show all
        OneForm.Show(True);
      afaHide: // hide all
        if not OneForm.ThisIsOverlay then
          OneForm.Hide(True); // pad overlays are special cases
      afaFocus: // allow recieve focus
        begin
          SetWindowLong(OneForm.Handle, GWL_EXSTYLE, GetWindowLong(OneForm.Handle,
            GWL_EXSTYLE) and not WS_EX_NOACTIVATE);
          if OneForm.Visible then
            OneForm.Popup();
        end;
      afaGhost: // deny recieving focus
        begin
          SetWindowLong(OneForm.Handle, GWL_EXSTYLE, GetWindowLong(OneForm.Handle,
            GWL_EXSTYLE) or WS_EX_NOACTIVATE);
          if OneForm.Visible then
            OneForm.Popup();
        end;
      afaDisable: // disable and put to background
        begin
          OneForm.TopMost(False);
          OneForm.Enabled := False;
        end;
      afaEnable: // enable and restore topmost
        begin
          OneForm.Enabled := True;
          OneForm.TopMost(True);
        end;
      afaPopup: //
        begin
          OneForm.Popup();
          OneForm.TopMost(True);
        end;
    end;
  end;

begin // (body of AllFormsAction)
  if Action = afaFocus then
  begin
    if InFocusedMode then
      Exit;
    InFocusedMode := True; // store action
  end;
  if Action = afaGhost then
  begin
    if not InFocusedMode then
      Exit;
    InFocusedMode := False;
  end;
// here must be all forms (except Fhide)
  Process(Fbind);
  Process(Fext);
  Process(Fover1);
  Process(Fover2);
  Process(FoverF);
  Process(FoverR);
  Process(Fview);
  Process(Fedit);
  Process(Fshot);
  Process(Fplug);
  Process(Fgui);
end;

constructor TFSpyroTAS.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  HandleNeeded();
  Left := -9999;
  Top := -9999;
  ForceSize();
end;

constructor TFSpyroTAS.CreateUsual();
begin
  DoNotMessAround := True;
  inherited Create(nil);
end;

// schedule black painting:
procedure TFSpyroTAS.WipeThisForm();
begin
  if not IsGuiInvoked then // not initialized yet
    Exit;
  if Handle = Fhide.Handle then // don't wipe the brush
    Exit;
  // rect to fill
  OldPos := GetSizeSystem();
  // post wiping
  Inc(WipeCounter);
  PostMessage(Handle, CUSTOM_MESSAGE_DOWIPE, 0, WipeCounter);
end;

// paint black in old posion:
procedure TFSpyroTAS.DoWipe(var MSG: TMessage);
begin
  if (Fhide = nil) or (Self.Handle = Fhide.Handle) then // don't do for brush itself
    Exit;
  if WipeCounter <> MSG.LPARAM then // react only on last call
    Exit;
  if StopWipingHappen then // denied by form itself
    Exit;
  Fhide.OldPos := Fhide.GetSizeSystem(); // store old position
  Fhide.SetSizeSystem(Self.OldPos); // move brush to old position of current window
  Fhide.Repaint();
  Fhide.SetSizeSystem(Fhide.OldPos);  // restore
end;

// hide form with black painting:
procedure TFSpyroTAS.Hide(StoreState: Boolean = False);
begin
  if DoNotMessAround then
  begin
    inherited Hide();
    Exit;
  end;
  if StoreState then // to show only already visible forms later
    OldVisible := Visible;
  if not Visible then
    Exit;
  WipeThisForm(); // draw black
  inherited Hide();
  ShowWindow(Handle, SW_HIDE); // hide always!
end;

// show form, maybe if needed to:
procedure TFSpyroTAS.Show(RetoreState: Boolean = False);
begin
  if DoNotMessAround then
  begin
    inherited Show();
    Exit;
  end;
  if RetoreState and not OldVisible then
    Exit; // it wasn't visible during special Hide();
  if Visible then
    Exit;
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or
    WS_EX_TOOLWINDOW or WS_EX_CONTEXTHELP); // explicit for Lazarus
  ShowWindow(Handle, SW_SHOWNOACTIVATE); // without stealing focus anyway
  Popup();
  inherited Show;
end;

procedure TFSpyroTAS.SetZOrder(Topmost: Boolean);
begin
  if (not StopVisibleChanging) or DoNotMessAround then
    inherited SetZOrder(Topmost);
end;

// set base window styles at first:
procedure TFSpyroTAS.CreateParams(var Params: TCreateParams);
begin
  if DoNotMessAround then
  begin
    inherited CreateParams(Params);
    Exit;
  end;
  Visible := False;
  Position := poDesigned; // stop the system to screw window creation
  inherited CreateParams(Params);
  Params.ExStyle := (Params.ExStyle or WS_EX_NOACTIVATE or WS_EX_TOPMOST or
    WS_EX_TOOLWINDOW or WS_EX_CONTEXTHELP) and not WS_EX_APPWINDOW;
  OnClose := DefOnClose; // see below
end;

// to make additional windows wipe when closed:
procedure TFSpyroTAS.DefOnClose(Sender: TObject; var Action: TCloseAction);
begin
  Ignores(Action);
  Hide();
end;

// early resizing and moving:
procedure TFSpyroTAS.WMpos(var MSG: TMessage);
begin
  if DoNotMessAround then
    Exit;
  Ignores(MSG);
  if (not Enabled) or InFocusedMode then // skip when not in ghost mode
    Exit;
  if Visible then
    WipeThisForm(); // draw black in old position
end;

// assign a hint to a control that will be shown even when it is disabled:
procedure TFSpyroTAS.SetPermanentHint(Control: TControl; Text: string);
var
  Lab: TLabel;
  Name: string;
begin
  Name := Control.Name + '_hint';
  Lab := TLabel(FindComponent(Name));
  if Lab = nil then
    Lab := TLabel.Create(Self);
  Lab.Name := Name;
  Lab.Parent := Control.Parent;
  Lab.ParentShowHint := True;
  Lab.AutoSize := False; // will be invisible
  Lab.SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
  Lab.Caption := '';
  Lab.TRANSPARENT := True;
  Lab.Hint := StringReplace(Text, '|', 'l', [rfReplaceAll]);
  Control.Hint := Lab.Hint; // hint when enabled, also could be different, but we don't need it
end;

// same as above, but for grid cells:
procedure TFSpyroTAS.SetPermanentGridHint(Grid: TStringGrid; Col, Row: Integer;
  Text: string);
var
  Lab: TLabel;
  Rect: TRect;
  Name: string;
begin
  Name := StringReplace(Grid.Name + IntToStr(Col) + '_' + IntToStr(Row) +
    '_hint', '-', '_', [rfReplaceAll]);
  Lab := TLabel(FindComponent(Name));
  if Lab = nil then
    Lab := TLabel.Create(Self);
  Lab.Name := Name;
  Lab.Parent := Grid;
  Lab.ParentShowHint := True;
  Lab.AutoSize := False;
  if Col < 0 then
  begin
    Rect := Grid.CellRect(0, Row);
    Rect.Right := Grid.ClientWidth;
  end
  else if Row < 0 then
  begin
    Rect := Grid.CellRect(Col, 0);
    Rect.Bottom := Grid.ClientHeight;
  end
  else
    Rect := Grid.CellRect(Col, Row); // target area
  Lab.SetBounds(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
  Lab.Caption := '';
  Lab.TRANSPARENT := True;
  Lab.Hint := StringReplace(StringReplace(Text, '|', 'l', [rfReplaceAll]), '#',
    #13, [rfReplaceAll]);
end;

procedure TFSpyroTAS.SetSizeClient(ALeft, Atop, AWidth, AHeight: Integer;
  OnlySize: Boolean);
var
  Temp: TRect;
begin
  Temp := Rect(ALeft, Atop, ALeft + AWidth, Atop + AHeight);
  AdjustWindowRectEx(Temp, GetWindowLong(Handle, GWL_STYLE), False,
    GetWindowLong(Handle, GWL_EXSTYLE));
  {$IFDEF FPC}
  if OnlySize then
    SetBounds(ALeft, Atop, AWidth, AHeight)
  else
    SetBounds(Temp.Left, Temp.Top, AWidth, AHeight);
  {$ELSE}
  if OnlySize then
    SetBounds(ALeft, Atop, Temp.Right - Temp.Left, Temp.Bottom - Temp.Top)
  else
    SetBounds(Temp.Left, Temp.Top, Temp.Right - Temp.Left, Temp.Bottom - Temp.Top);
  {$ENDIF}
end;

procedure TFSpyroTAS.SetSizeSystem(ALeft, Atop, AWidth, AHeight: Integer);
var
  Temp: TRect;
begin
  Temp := Rect(ALeft, Atop, ALeft + AWidth, Atop + AHeight);
  AdjustWindowRectEx(Temp, GetWindowLong(Handle, GWL_STYLE), False,
    GetWindowLong(Handle, GWL_EXSTYLE));
  {$IFDEF FPC}
  SetBounds(ALeft, Atop, AWidth * 2 - (Temp.Right - Temp.Left), AHeight * 2 - (Temp.Bottom
    - Temp.Top));
  {$ELSE}
  SetBounds(ALeft, Atop, AWidth, AHeight);
  {$ENDIF}
end;

procedure TFSpyroTAS.SetSizeSystem(Rect: TRect);
begin
  SetSizeSystem(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
end;

procedure TFSpyroTAS.SetSizeClient(Rect: TRect; OnlySize: Boolean);
begin
  SetSizeClient(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top,
    OnlySize);
end;

function TFSpyroTAS.GetSizeClient(TargetWindow: Integer = 0): TRect;
var
  Temp: TRect;
begin
  Result.Left := 0;
  if TargetWindow = 0 then
    TargetWindow := Handle;
  GetWindowRect(TargetWindow, Result);
  Temp := Result;
  AdjustWindowRectEx(Temp, GetWindowLong(TargetWindow, GWL_STYLE), False,
    GetWindowLong(TargetWindow, GWL_EXSTYLE));
  Result.Top := Result.Top * 2 - Temp.Top;
  Result.Left := Result.Left * 2 - Temp.Left;
  Result.Right := Result.Right * 2 - Temp.Right;
  Result.Bottom := Result.Bottom * 2 - Temp.Bottom;
end;

function TFSpyroTAS.GetSizeSystem(): TRect;
begin
  Result.Left := 0;
  GetWindowRect(Handle, Result);
end;

procedure TFSpyroTAS.ForceSize();
var
  Rect: TRect;
begin
  Rect.Left := Left;
  Rect.Top := Top;
  Rect.Right := Left + Width;
  Rect.Bottom := Top + Height;
  {$IFDEF FPC}
  AdjustWindowRectEx(Rect, GetWindowLong(Handle, GWL_STYLE), False,
    GetWindowLong(Handle, GWL_EXSTYLE));
  {$ENDIF}
  MoveWindow(Handle, Left, Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top, False);
end;

procedure TFSpyroTAS.TemporaryDisable(Sender: TObject = nil);
begin
  if TemporaryDisabled <> nil then
  begin
    TemporaryDisableTime := DefaultActionSleep - (Integer({%H-}GetTickCount()) -
      TemporaryDisableTime);
    if TemporaryDisableTime > 0 then
      Sleep(TemporaryDisableTime);
    TemporaryDisabled.Enabled := True;
    TemporaryDisabled := nil;
  end;
  if Sender <> nil then
  begin
    TemporaryDisabled := TControl(Sender);
    TemporaryDisabled.Enabled := False;
    TemporaryDisableTime := Integer({%H-}GetTickCount());
  end;
end;

procedure TFSpyroTAS.Popup();
begin
  SetWindowPos(Handle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TFSpyroTAS.TopMost(OnTop: Boolean);
begin
  if OnTop then
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
      SWP_NOACTIVATE)
  else
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
      SWP_NOACTIVATE)
end;

procedure TFSpyroTAS.Report(Caption, Text: string);
begin
  Umisc.Report(Caption, Text, Self);
end;

procedure TFSpyroTAS.Report(Caption, Text: string; Handle: HWND);
begin
  Umisc.Report(Caption, Text, Handle);
end;

// remove shadow from hints:
procedure TSpyroTASHint.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params); // since shadow could glitch and stick detached forever
  Params.WindowClass.Style := Params.WindowClass.style and not CS_DROPSHADOW;
  LastHint := Self;
  Canvas.Font.Name := HintsFont;
  Canvas.Font.Charset := HintsCharset;
  Canvas.Font.Size := HintsSize;
end;

// wipe a hint, just like forms' DoWipe(); also called from gui timer:
procedure TSpyroTASHint.DoWipe();
begin
  if (Fhide = nil) or (not IsGuiInvoked) or WasWiped or not TSpyroTASHintOkay then
    Exit;
  Fhide.OldPos := Fhide.GetSizeSystem();
  Fhide.SetSizeSystem(Self.OldPos);
  Fhide.Repaint();
  Fhide.SetSizeSystem(Fhide.OldPos);
  WasWiped := True; // don't wipe twice
end;

// paint event on a hint:
procedure TSpyroTASHint.Paint();
begin
  if not TSpyroTASHintOkay then
  begin
    inherited Paint();
    Exit;
  end;
  DoWipe(); // wipe in old position
  inherited Paint();
  GetWindowRect(Handle, OldPos); // store new position
  LastHint := Self; // activete monitoring by timer
  WasWiped := False;
end;

end.

// EOF


