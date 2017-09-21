unit Ukey; // SpyroTAS is licensed under WTFPL

// keyboard and controller related stuff

interface

uses
  Utas, Windows;

const // from PlayStation spec actually
  pad_Select = $0001;
  pad_L3 = $0002;
  pad_R3 = $0004;
  pad_Start = $0008;
  pad_Up = $0010;
  pad_Right = $0020;
  pad_Down = $0040;
  pad_Left = $0080;
  pad_L2 = $0100;
  pad_R2 = $0200;
  pad_L1 = $0400;
  pad_R1 = $0800;
  pad_Triangle = $1000;
  pad_Circle = $2000;
  pad_Cross = $4000;
  pad_Square = $8000;

const // arbitrary sequence to refer buttons
  PadButton: array[0..15] of Integer = (pad_Up, pad_Right, pad_Down, pad_Left,
    pad_Triangle, pad_Circle, pad_Cross, pad_Square, pad_L1, pad_L2, pad_R1,
    pad_R2, pad_Start, pad_Select, pad_L3, pad_R3);

const // for ini
  ButtonNames: array[0..15] of string = ('pad_up', 'pad_right', 'pad_down',
    'pad_left', 'pad_triangle', 'pad_circle', 'pad_cross', 'pad_square',
    'pad_l1', 'pad_l2', 'pad_r1', 'pad_r2', 'pad_start', 'pad_select', 'pad_l3',
    'pad_r3');

const // controller defaults
  DefaultButtons: array[0..2, 0..15] of Byte = ((VK_UP, VK_RIGHT, VK_DOWN,
    VK_LEFT, VK_END, VK_NUMPAD0, VK_RCONTROL, VK_RSHIFT, VK_DELETE, VK_INSERT,
    VK_NEXT, VK_PRIOR, VK_SPACE, 191, 0, 0), (Ord('W'), Ord('D'), Ord('S'), Ord('A'),
    Ord('T'), Ord('H'), Ord('G'), Ord('F'), Ord('E'), Ord('Q'), Ord('R'), Ord('Y'),
    Ord('C'), Ord('X'), Ord('Z'), Ord('V')), (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0));

const // hotkey sequence
  HotkeySave = 0;
  HotkeyLoad = 1;
  HotkeyPrevious = 2;
  HotkeyCurrent = 3;
  HotkeySwap = 4;
  HotkeyFast = 5;
  HotkeyFrame = 6;
  HotkeyExit = 7;
  HotkeyWarp = 8;
  EmulatorHotkeySavestate = 9;
  EmulatorHotkeyLoadstate = 10;
  HotkeysLast = EmulatorHotkeyLoadstate; // must point to previous!

type
  THotkey = 0..HotkeysLast;

const // for ini
  HotkeyNames: array[0..HotkeysLast] of string = ('hot_save', 'hot_load',
    'hot_prev', 'hot_last', 'hot_swap', 'hot_fast', 'hot_advance', 'hot_exit',
    'hot_warp', 'emu_save', 'emu_load');

const
  HotkeyExitDefault = VK_F10; // explicit
  DefaultHotkeys: array[0..HotkeysLast] of Byte = (VK_APPS, VK_RETURN, VK_BACK,
    VK_DIVIDE, VK_TAB, VK_ADD, VK_SUBTRACT, HotkeyExitDefault, VK_MULTIPLY, VK_F1, VK_F3);

const // keycode names
  VkKeyNames: array[0..255] of string = ('', 'Left Mouse', 'Right Mouse',
    '[CANCEL]', 'Middle Mouse', 'X-1 Mouse', 'X-2 Mouse', '(7)', 'Backspasce',
    'Tab', '(10)', '(11)', 'Clear (num 5)', 'Enter', '(14)', '(15)', '(16)',
    '(17)', '(18)', 'Pause Break', 'Caps Lock', '[KANA]', '(22)', '[JUNJA]',
    '[FINAL]', '[KANJI]', '(26)', '', '[CONVERT]', '[NONCONVERT]', '[ACCEPT]',
    '[MODECHANGE]', 'Space', 'Page Up', 'Page Down', 'End', 'Home', 'Left', 'Up',
    'Right', 'Down', '[SELECT]', '[PRINT]', '[EXECUTE]', 'Print Screen',
    'Insert', 'Delete', '[HELP]', '0', '1', '2', '3', '4', '5', '6', '7', '8',
    '9', '(58)', '(59)', '(60)', '(61)', '(62)', '(63)', '(64)', 'A', 'B', 'C',
    'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
    'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'Left Windows', 'Right Windows',
    'Application (menu)', '(94)', '[SLEEP]', 'Num 0', 'Num 1', 'Num 2', 'Num 3',
    'Num 4', 'Num 5', 'Num 6', 'Num 7', 'Num 8', 'Num 9', 'Num Multiply',
    'Num Add', '[SEPARATOR]', 'Num Substract', 'Num Dot', 'Num Divide', 'F1',
    'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12', 'F13',
    'F14', 'F15', 'F16', 'F17', 'F18', 'F19', 'F20', 'F21', 'F22', 'F23', 'F24',
    '(136)', '(137)', '(138)', '(139)', '(140)', '(141)', '(142)', '(143)',
    'Num Lock', 'Scroll Lock', '[FJ_JISHO]', '[FJ_MASSHOU]', '[FJ_TOUROKU]',
    '[FJ_LOYA]', '[ROYA]', '(151)', '(152)', '(153)', '(154)', '(155)', '(156)',
    '(157)', '(158)', '(159)', 'Left Shift', 'Right Shift', 'Left Control',
    'Right Control', 'Left Alt', 'Right Alt', '[BROWSER_BACK]',
    '[BROWSER_FORWARD]', '[BROWSER_REFRESH]', '[BROWSER_STOP]',
    '[BROWSER_SEARCH]', '[BROWSER_FAVORITES]', '[BROWSER_HOME]', '[VOLUME_MUTE]',
    '[VOLUME_DOWN]', '[VOLUME_UP]', '[MEDIA_NEXT_TRACK]', '[MEDIA_PREV_TRACK]',
    '[MEDIA_STOP]', '[MEDIA_PLAY_PAUSE]', '[LAUNCH_MAIL]', '[MEDIA_SELECT]',
    '[LAUNCH_APP1]', '[LAUNCH_APP2]', '(184)', '(185)', '  ;    :', '  =    +',
    '  ,    <', '  -    _', '  .    >', '  /    ?', '  `    ~', '[ABNT_C1]',
    '[ABNT_C2]', '(195)', '(196)', '(197)', '(198)', '(199)', '(200)', '(201)',
    '(202)', '(203)', '(204)', '(205)', '(206)', '(207)', '(208)', '(209)',
    '(210)', '(211)', '(212)', '(213)', '(214)', '(215)', '(216)', '(217)',
    '(218)', '  [    {', '  \    |', '  ]    }', '  ''    "', '[OEM_8]', '(224)',
    '[OEM_AX]', '  \    /', '[ICO_HELP]', '[ICO_00]', '[PROCESSKEY]',
    '[ICO_CLEAR]', '[PACKET]', '(232)', '[OEM_RESET]', '[OEM_JUMP]', '[OEM_PA1]',
    '[OEM_PA2]', '[OEM_PA3]', '[OEM_WSCTRL]', '[OEM_CUSEL]', '[OEM_ATTN]',
    '[OEM_FINISH]', '[OEM_COPY]', '[OEM_AUTO]', '[OEM_ENLW]', '[OEM_BACKTAB]',
    '[ATTN]', '[CRSEL]', '[EXSEL]', '[EREOF]', '[PLAY]', '[ZOOM]', '[NONAME]',
    '[PA1]', '[OEM_CLEAR]', '0xFF');

var
  GlobalKeysForPad: Integer = 0;
  IsPadRouted: Boolean = False;

procedure PressKeyDown(KeyCode: Byte);

procedure ReleaseAllPressedKeys();

function IsKeyJustPressed(KeyCode: Byte): Boolean;

procedure MakeKeyDown(KeyCode: Byte);

procedure MakeKeyUp(KeyCode: Byte);

procedure ReadFromKeys(var Keys: Integer; Mask: Integer = 0);

procedure PressThoseKeys(Keys: Integer);

function JustPressedKeys(): Integer;

function IsKeyCatched(KeyCode: Byte): Boolean;

procedure CatchKeysNow();

procedure ClearCatchedKeys();

procedure ClearJustPressed();

function IsHotkeyPressed(Hotkey: THotkey; JustPress: Boolean): Boolean;

procedure AssignHotkey(Hotkey: THotkey; KeyCode: Byte);

function GetAssignedHotkey(Hotkey: THotkey): Byte;

procedure AssignButton(Port, Button: Integer; KeyCode: Byte);

function GetAssignedButton(Port, Button: Integer): Byte;

implementation

uses
  Messages;

var
  KeyMonitorQueue: array[0..255] of Integer; // to know which to release
  KeyMonitorQueueCounter: Integer = 0;
  JustPressArray: array[0..255] of Boolean; // to test for exact press moment
  PressedKeys: array[0..255] of Boolean; // that catched
  AssignedHotkeys: array[0..HotkeysLast] of Byte; // all hotkeys
  AssignedButtons: array[0..2, 0..15] of Byte; // virtual, 1 and 2 controllers

// press and store key:

procedure PressKeyDown(KeyCode: Byte);
begin
  Inc(KeyMonitorQueueCounter);
  KeyMonitorQueue[KeyMonitorQueueCounter] := KeyCode; // store
  MakeKeyDown(KeyCode); // push key
end;

// pull up all PressKeyDown'ed:
procedure ReleaseAllPressedKeys();
begin
  while KeyMonitorQueueCounter > 0 do
  begin
    MakeKeyUp(KeyMonitorQueue[KeyMonitorQueueCounter]); // release every key
    Dec(KeyMonitorQueueCounter);
  end;
end;

// check against press event, not hold
function IsKeyJustPressed(KeyCode: Byte): Boolean;
begin
  Result := False;
  if KeyCode = AssignedHotkeys[HotkeyExit] then // don't touch exit key
    Exit;
  if not JustPressArray[KeyCode] then // was released
  begin
    if 0 <> GetAsyncKeyState(KeyCode) then // but pressed now
    begin
      JustPressArray[KeyCode] := True; // will be down
      Result := True;
    end;
  end
  else // was hold
  begin
    if 0 = GetAsyncKeyState(KeyCode) then // but released now
      JustPressArray[KeyCode] := False; // will be up
  end;
end;

// simulate key down event
procedure MakeKeyDown(KeyCode: Byte);
var
  ScanCode: Integer;
begin
  if IsRunDLL then // no events when standalone
    Exit;
  ScanCode := MapVirtualKey(KeyCode, 0); // might be need
  if EmulatorWindow <> 0 then
  begin // actually when there's no window, no events should be sent at all, hmm
    PostMessage(EmulatorWindow, WM_KEYDOWN, KeyCode, 1 or Integer(ScanCode shl 16));
    PostMessage(EmulatorWindow, WM_KEYDOWN, KeyCode, 1 or Integer(ScanCode shl
      16) or Integer(1 shl 24));
  end; // abuse every method
  keybd_event(KeyCode, ScanCode, 0, 0);
  keybd_event(KeyCode, ScanCode, KEYEVENTF_EXTENDEDKEY, 0);
end;

// simulate key up event, everything alike MakeKeyDown:
procedure MakeKeyUp(KeyCode: Byte);
var
  ScanCode: Integer;
begin
  if IsRunDLL then
    Exit;
  ScanCode := MapVirtualKey(KeyCode, 0);
  if EmulatorWindow <> 0 then
  begin
    PostMessage(EmulatorWindow, WM_KEYUP, KeyCode, 1 or Integer(ScanCode shl 16)
      or Integer(3 shl 30));
    PostMessage(EmulatorWindow, WM_KEYUP, KeyCode, 1 or Integer(ScanCode shl 16)
      or Integer(3 shl 30) or Integer(1 shl 24));
  end;
  keybd_event(KeyCode, ScanCode, KEYEVENTF_KEYUP, 0);
  keybd_event(KeyCode, ScanCode, KEYEVENTF_KEYUP or KEYEVENTF_EXTENDEDKEY, 0);
end;

// updates history keys with current pressed, supporting semi-keys mask:
procedure ReadFromKeys(var Keys: Integer; Mask: Integer = 0);
var
  Index: Integer;
begin
  for Index := 0 to 15 do // checking virtual
    if PressedKeys[AssignedButtons[0, Index]] and (((Mask and PadButton[Index])
      = 0) or AutofireButton[Index]) then
      Keys := Keys or PadButton[Index];
end;

// do press all keys from history:
procedure PressThoseKeys(Keys: Integer);
var
  Index: Integer;
begin
  GlobalKeysForPad := Keys;
  if IsPadRouted then
    Exit;
  for Index := 0 to 15 do // first controller
    if (Keys and PadButton[Index]) <> 0 then
      PressKeyDown(AssignedButtons[1, Index]);
  Keys := Keys shr 16; // second
  for Index := 0 to 15 do
    if (Keys and PadButton[Index]) <> 0 then
      PressKeyDown(AssignedButtons[2, Index]);
end;

// get only just-pressed virtuals as history keys:
function JustPressedKeys(): Integer;
var
  Index: Integer;
begin
  Result := 0; // no by default
  for Index := 0 to 15 do
    if IsKeyJustPressed(AssignedButtons[0, Index]) then
      Result := Result or PadButton[Index]; // combine
end;

// getter for array:
function IsKeyCatched(KeyCode: Byte): Boolean;
begin
  Result := PressedKeys[KeyCode];
end;

// poll keyboard and update array:
procedure CatchKeysNow();
var
  Index: Integer;
begin
  for Index := 1 to 255 do // gather pressed keys
    if (Index <> AssignedHotkeys[HotkeyExit]) and (0 <> GetAsyncKeyState(Index)) then
      PressedKeys[Index] := True; // ignoring exit hotkey
end;

// refresh array of pressed:
procedure ClearCatchedKeys();
var
  Index: Integer;
begin
  for Index := 0 to 255 do // can be done my ZeroMemory, hmm
    PressedKeys[Index] := False;
end;

// refresh just-press array:
procedure ClearJustPressed();
var
  Index: Integer;
begin
  for Index := 0 to 255 do
    JustPressArray[Index] := False;
end;

// checker for hotkeys:
function IsHotkeyPressed(Hotkey: THotkey; JustPress: Boolean): Boolean;
begin
  if JustPress then
    Result := IsKeyJustPressed(AssignedHotkeys[Hotkey])
  else
    Result := IsKeyCatched(AssignedHotkeys[Hotkey]);
end;

// setter for hotkeys:
procedure AssignHotkey(Hotkey: THotkey; KeyCode: Byte);
begin
  if KeyCode = VK_ESCAPE then
    KeyCode := 0;
  AssignedHotkeys[Hotkey] := KeyCode;
  if (Hotkey = HotkeyExit) and (KeyCode = 0) then
    AssignedHotkeys[Hotkey] := HotkeyExitDefault;
end;

// getter for hotkeys:
function GetAssignedHotkey(Hotkey: THotkey): Byte;
begin
  Result := AssignedHotkeys[Hotkey];
end;

// setter for controllers:
procedure AssignButton(Port, Button: Integer; KeyCode: Byte);
begin
  if KeyCode = VK_ESCAPE then
    KeyCode := 0;
  AssignedButtons[Port][Button] := KeyCode;
end;

// getter for controllers:
function GetAssignedButton(Port, Button: Integer): Byte;
begin
  Result := AssignedButtons[Port][Button];
end;

end.

// EOF


