unit UFbind; // SpyroTAS is licensed under WTFPL

// hotkey settings form

interface

uses
  Utas, Windows, Classes, Controls, Forms, Grids, ExtCtrls, StdCtrls, ComCtrls,
  Uforms;

type
  TFbind = class(TFSpyroTAS)
    timer_key: TTimer;
    page_all: TPageControl;
    tab_pad: TTabSheet;
    tab_func: TTabSheet;
    grd_pad: TStringGrid;
    grd_hot: TStringGrid;
    tab_help: TTabSheet;
    m_help: TMemo;
    grd_emu: TStringGrid;
    c_pad_routed: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure grd_anyGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure timer_keyTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ch_allClick(Sender: TObject);
    procedure grd_anyContextPopup(Sender: TObject; MousePos: TPoint; var Handled:
      Boolean);
    procedure c_pad_routedClick(Sender: TObject);
  end;

var
  Fbind: TFbind = nil;

const
  PortSections: array[0..3] of string = ('virtual_pad', 'port_1', 'port_2',
    'control_keys');

procedure LoadPadKeys(); // called from gui-init

implementation

{$R *.dfm}

uses
  SysUtils, Uini, Ukey, Umisc, Uglob, Umain;

var
  CurrentColumn, CurrentRow: Integer; // for both tables
  grd_cur: TStringGrid;
  InternalChange: Boolean; // avoid recursion

// load variables from INI and fill tables

function GetKeysArray(Hotkeys: Boolean): TPairsArray;
var
  Index: Integer;
begin
  if Hotkeys then
  begin
    SetLength(Result, HotkeysLast + 1);
    for Index := 0 to HotkeysLast do
      Result[Index].Key := HotkeyNames[Index];
  end
  else
  begin
    SetLength(Result, 16);
    for Index := 0 to 15 do
      Result[Index].Key := ButtonNames[Index];
  end;
  for Index := 0 to Length(Result) - 1 do
    Result[Index].Value := '*';
end;

procedure LoadPadKeys();
var
  Data: TPairsArray;
  Index, Port, Key: Integer;
begin
  SetLength(Data, 0);
  if GuiClosing then
    Exit;
  if InternalChange then // disable self events triggering
    Exit;
  InternalChange := True;
  for Port := 0 to 2 do
  begin
    Data := GetKeysArray(False);
    IniSectionUpdate(PathToIni, PortSections[Port], Data, False);
    for Index := 0 to 15 do
    begin
      Key := StrToIntDef(Data[Index].Value, DefaultButtons[Port, Index]);
      AssignButton(Port, Index, Key);
      Fbind.grd_pad.Cells[Port + 1, Index + 1] := VkKeyNames[Key];
    end;
  end;

  Data := GetKeysArray(True);
  IniSectionUpdate(PathToIni, PortSections[3], Data, False);
  for Index := 0 to HotkeysLast do
  begin
    Key := StrToIntDef(Data[Index].Value, DefaultHotkeys[Index]);
    AssignHotkey(Index, Key);
    if (Index = EmulatorHotkeySavestate) or (Index = EmulatorHotkeyLoadstate) then
      Fbind.grd_emu.Cells[1, Index + 1 - EmulatorHotkeySavestate] := VkKeyNames[Key]
    else
      Fbind.grd_hot.Cells[1, Index + 1] := VkKeyNames[Key];
  end;

  InternalChange := False;
end;

//
procedure SavePadKeys();
var
  Data: TPairsArray;
  Index, Port: Integer;
begin
  SetLength(Data, 0);
  if GuiClosing then
    Exit;
  if InternalChange then // protection
    Exit;
  InternalChange := True;
  KeyAutofireMask := 0;

  for Port := 0 to 2 do
  begin
    Data := GetKeysArray(False);
    for Index := 0 to 15 do
      Data[Index].Value := IntToStr(GetAssignedButton(Port, Index));
    IniSectionUpdate(PathToIni, PortSections[Port], Data, True);
  end;

  Data := GetKeysArray(True);
  for Index := 0 to HotkeysLast do
    Data[Index].Value := IntToStr(GetAssignedHotkey(Index));

  IniSectionUpdate(PathToIni, PortSections[3], Data, True);

  InternalChange := False;
end;

// table captions
procedure TFbind.FormCreate(Sender: TObject);
begin
  Fbind := Self;
  m_help.Font.Name := HintsFont;
  m_help.Font.Charset := HintsCharset;
  Caption := 'Keyborad mappings + general help:';
  grd_pad.DefaultRowHeight := 18;
  {$IFDEF FPC}
  grd_pad.DefaultRowHeight := grd_pad.DefaultRowHeight + grd_pad.GridLineWidth; // Lazarus border width fix
  {$ENDIF}
  grd_hot.DefaultRowHeight := grd_pad.DefaultRowHeight;
  page_all.ActivePage := tab_pad; // default first tab
  with grd_pad do
  begin
    RowCount := 17;
    ColCount := 4;
    Height := RowCount * (DefaultRowHeight + 1) + 4;
    Cells[0, 0] := ' Action:';
    Cells[1, 0] := ' Virtual for SpyroTAS:';
    Cells[2, 0] := ' Emulator port 1:';
    Cells[3, 0] := ' Emulator port 2:';
    Cells[0, 1] := ' Up';
    Cells[0, 2] := ' Right';
    Cells[0, 3] := ' Down';
    Cells[0, 4] := ' Left';
    Cells[0, 5] := ' Triangle';
    Cells[0, 6] := ' Circle';
    Cells[0, 7] := ' Cross';
    Cells[0, 8] := ' Square';
    Cells[0, 9] := ' L1';
    Cells[0, 10] := ' L2';
    Cells[0, 11] := ' R1';
    Cells[0, 12] := ' R2';
    Cells[0, 13] := ' Start';
    Cells[0, 14] := ' Select';
    Cells[0, 15] := ' L3';
    Cells[0, 16] := ' R3';
  end;
  with grd_hot do // second tab
  begin
    RowCount := 10;
    ColCount := 2;
    Height := RowCount * (DefaultRowHeight + 1) + 4;
    Cells[0, 0] := '      [For SpyroTAS]';
    Cells[1, 0] := '          Hotkey:';
    Cells[0, 1] := ' Save keystate';
    Cells[0, 2] := ' Load keystate';
    Cells[0, 3] := ' Change keystate';
    Cells[0, 4] := ' Set last timepoint';
    Cells[0, 5] := ' Swap controllers';
    Cells[0, 6] := ' No frame limit (hold)';
    Cells[0, 7] := ' Frame advance, toggle';
    Cells[0, 8] := ' Toggle GUI (exit on hold)';
    Cells[0, 9] := ' Warp back via GPU';
  end;
  with grd_emu do
  begin
    RowCount := 3;
    ColCount := 2;
    Height := RowCount * (DefaultRowHeight + 1) + 4;
    Cells[0, 0] := '          [For emulator]';
    Cells[1, 0] := '     Standard hotkey:';
    Cells[0, 1] := ' Do savestate';
    Cells[0, 2] := ' Do loadstate';
  end;
  c_pad_routed.Checked := IsPadRouted;
end;

// timer for key polling
procedure TFbind.timer_keyTimer(Sender: TObject);
var
  KeyCode: Integer;
begin
  if GuiClosing then
    Exit;
  if not grd_cur.EditorMode then // only when editing
    Exit;
  for KeyCode := 254 downto 3 do // ignore 0xff and mouse buttons
    if GetAsyncKeyState(KeyCode) <> 0 then
    begin  // something is pressed
      grd_cur.EditorMode := False; // done
      grd_cur.Cells[CurrentColumn, CurrentRow] := VkKeyNames[KeyCode]; // string
      if grd_cur = grd_hot then
        AssignHotkey(CurrentRow - 1, KeyCode) // value
      else if grd_cur = grd_pad then
        AssignButton(CurrentColumn - 1, CurrentRow - 1, KeyCode)
      else
        AssignHotkey(EmulatorHotkeySavestate + CurrentRow - 1, KeyCode); // value
      timer_key.Enabled := False; // disable this timer
      KeyPreview := False; // allow navigation
      SavePadKeys(); // save everything after any change
      Break; // ignore others
    end;
end;

// disable default keypress:
procedure TFbind.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Ignores(Shift);
  Key := 0; // if previewed
end;

// same as above:
procedure TFbind.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
end;

// disable right click:
procedure TFbind.grd_anyContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
end;

// autofire checkboxes:
procedure TFbind.ch_allClick(Sender: TObject);
begin
  if InternalChange then // only at user clicks
    Exit;
  SavePadKeys(); // save everything after any change
end;

// start editing any cell:
procedure TFbind.grd_anyGetEditText(Sender: TObject; ACol, ARow: Integer; var
  Value: string);
var
  Key: Integer;
  Released: Boolean;
begin
  Ignores(Value);
  if GuiClosing then
    Exit;
  if GameIsRunning then
    RequestSwitch();
  repeat  // poll all keys until nothing is pressed
    Released := True;
    for Key := 254 downto 5 do
      if GetAsyncKeyState(Key) <> 0 then
      begin
        MakeKeyUp(Key);
        Released := False;
      end;
    Sleep(10);
  until Released;
  grd_cur := TStringGrid(Sender);
  CurrentColumn := ACol;
  CurrentRow := ARow;
  KeyPreview := True; // disable table navigation
  timer_key.Enabled := True; // start polling timer
end;

procedure TFbind.c_pad_routedClick(Sender: TObject);
begin
  c_pad_routed.Checked := IsPadRouted;
end;

end.

// EOF


