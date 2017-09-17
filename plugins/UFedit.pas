unit UFedit; // SpyroTAS is licensed under WTFPL

interface

uses
  Utas, Windows, Classes, Forms, Uforms, Controls, Messages, StdCtrls, ExtCtrls;

type
  TFedit = class(TFSpyroTAS)
    m_history: TMemo;
    pnl_editor: TPanel;
    b_accept: TButton;
    b_reload: TButton;
    b_select: TButton;
    procedure b_reloadClick(Sender: TObject);
    procedure b_selectClick(Sender: TObject);
    procedure b_acceptClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure m_historyKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
  end;

var
  Fedit: TFedit = nil;

implementation

{$R *.dfm}

uses
  SysUtils, Ukey, Uglob, UFgui, Umisc, Umain, Math;

//
procedure HistoryToText(Save: TStrings);
var
  Index, Sorter, Keys, Min, Len: Integer;
  Addr, Hash, Rrec: PInteger; // to faster access
  Line: AnsiString;
  States, List: array of Integer;

  function OnePad(Keys: Integer): AnsiString;
  // "321 UDLR ES TXQC 123"
  // "--- ---- -- ---- ---"
  //  12345678901234567890
  begin
    Result := StringOfChar('-', 20);
    if (Keys and pad_L3) <> 0 then
      Result[1] := '3';
    if (Keys and pad_L2) <> 0 then
      Result[2] := '2';
    if (Keys and pad_L1) <> 0 then
      Result[3] := '1';
    Result[4] := ' ';
    if (Keys and pad_Up) <> 0 then
      Result[5] := 'U';
    if (Keys and pad_Down) <> 0 then
      Result[6] := 'D';
    if (Keys and pad_Left) <> 0 then
      Result[7] := 'L';
    if (Keys and pad_Right) <> 0 then
      Result[8] := 'R';
    Result[9] := ' ';
    if (Keys and pad_Select) <> 0 then
      Result[10] := 'E';
    if (Keys and pad_Start) <> 0 then
      Result[11] := 'S';
    Result[12] := ' ';
    if (Keys and pad_Triangle) <> 0 then
      Result[13] := 'T';
    if (Keys and pad_Cross) <> 0 then
      Result[14] := 'X';
    if (Keys and pad_Square) <> 0 then
      Result[15] := 'Q';
    if (Keys and pad_Circle) <> 0 then
      Result[16] := 'C';
    Result[17] := ' ';
    if (Keys and pad_R1) <> 0 then
      Result[18] := '1';
    if (Keys and pad_R2) <> 0 then
      Result[19] := '2';
    if (Keys and pad_R3) <> 0 then
      Result[20] := '3';
  end;

begin
  Save.Clear();
  Len := Length(KeystateList);
  SetLength(List, Len);
  SetLength(States, Len + 1);
  for Index := 0 to Len - 1 do
    List[Index] := KeystateList[Index];
  for Sorter := 0 to Len - 1 do
  begin
    Min := 0;
    for Index := 1 to Len - 1 do
      if List[Index] < List[Min] then
        Min := Index;
    States[Sorter] := List[Min];
    List[Min] := MaxInt;
  end;
  Addr := History.Addr(HistoryKeys);
  Hash := History.Addr(HistoryHash);
  Rrec := History.Addr(HistoryRrec);
  Rrec^ := 0;
  Hash^ := 0;
  Addr^ := 0;
  States[Len] := 0;
  Sorter := 0;
  for Index := 0 to History.GetSize() do
  begin
    Keys := Addr^;
    if Index = States[Sorter] then
    begin
      Line := IntToStr(Sorter);
      Line := Line + ' ' + StringOfChar('-', Max(1, 6 - Length(Line)));
      Inc(Sorter);
    end
    else
      Line := '';                               
    {(*}
    Line := '| ' + UpperCase(IntToHex(Hash^, 8)) +
           ' | ' + FormatNumberWithZero(Rrec^, 3, ' ') +
           ' | ' + OnePad(Keys) +
           ' | ' + OnePad(Keys shr 16) +
           ' | ' + FormatNumberWithZero(Index, 6, ' ') + ' | ' + Line;
    {*)}
    Save.Add(Line);
    Inc(Addr);
    Inc(Hash);
    Inc(Rrec);
  end;

end;

//
function HistoryFromText(Load: TStrings): string;
var
  Line, Part: AnsiString;
  i, j, Index, Value, Code, Keys: Integer;
  Stat: TCustomIntegerArray;
  Hist: TCustomIntegerArrays;

  function Iterate(var Str: AnsiString): AnsiString;
  begin
    if (Str = '') or (Str[1] <> '|') then
      Abort;
    Delete(Str, 1, 1);
    Result := Copy(Str, 1, Pos('|', Str + '|') - 1);
    Delete(Str, 1, Length(Result));
    Result := Trim(Result);
  end;

  function GetPad(Str: AnsiString): Integer;
  // "321UDLRESTXQC123"
  // "----------------"
  //  1234567890123456
  begin
    if Length(Str) <> 16 then
      Abort;
    Result := 0;
    if Str[1] <> '-' then
      Result := Result or pad_L3;
    if Str[2] <> '-' then
      Result := Result or pad_L2;
    if Str[3] <> '-' then
      Result := Result or pad_L1;
    if Str[4] <> '-' then
      Result := Result or pad_Up;
    if Str[5] <> '-' then
      Result := Result or pad_Down;
    if Str[6] <> '-' then
      Result := Result or pad_Left;
    if Str[7] <> '-' then
      Result := Result or pad_Right;
    if Str[8] <> '-' then
      Result := Result or pad_Select;
    if Str[9] <> '-' then
      Result := Result or pad_Start;
    if Str[10] <> '-' then
      Result := Result or pad_Triangle;
    if Str[11] <> '-' then
      Result := Result or pad_Cross;
    if Str[12] <> '-' then
      Result := Result or pad_Square;
    if Str[13] <> '-' then
      Result := Result or pad_Circle;
    if Str[14] <> '-' then
      Result := Result or pad_R1;
    if Str[15] <> '-' then
      Result := Result or pad_R2;
    if Str[16] <> '-' then
      Result := Result or pad_R3;
  end;

begin
  Result := '';
  Hist := TCustomIntegerArrays.Create(HistoriesCount);
  Stat := TCustomIntegerArray.Create();
  Index := 0;
  try
    j := 0;
    for i := 0 to Load.Count - 1 do
    begin
      Index := i;
      Line := Trim(StringReplace(StringReplace(Load.Strings[Index], ' ', '', [rfReplaceAll]),
        #9, '', [rfReplaceAll]));
      if Line = '' then
        Continue;
      Part := Iterate(Line);
      if Part = '' then
        Hist.Write(HistoryHash, j, 0)
      else
      begin
        Val('$' + Part, Value, Code);
        if Code <> 0 then
          Abort;
        Hist.Write(HistoryHash, j, Value);
      end;
      Part := Iterate(Line);
      Hist.Write(HistoryRrec, j, StrToInt64Def(Part, 0));
      Part := Iterate(Line);
      Keys := GetPad(Part);
      Part := Iterate(Line);
      Keys := Keys or (GetPad(Part) shl 16);
      Part := Iterate(Line);
      if Part = '' then
        Abort;
      Part := Iterate(Line);
      Hist.Write(HistoryKeys, j, Keys);
      if Part <> '' then
        Stat.Push(Index);
      Inc(j);
    end;
    History.Free();
    History := Hist;
    Hist := nil;
    SetLength(KeystateList, Stat.GetSize());
    Move(Stat.Addr()^, KeystateList[0], Stat.GetSize() * 4);
    SendAction(caListDirty);
  except
    Line := '';
    if Index > 0 then
      Line := Load.Strings[Index - 1] + #13;
    Line := Line + Load.Strings[Index];
    if Index < Load.Count - 1 then
      Line := Line + #13 + Load.Strings[Index + 1];
    Result := Line;
  end;
  Hist.Free();
  Stat.Free();
end;

procedure TFedit.b_reloadClick(Sender: TObject);
var
  Up: Integer;
begin
  if GuiClosing then
    Exit;
  if SprintMode then
    Exit;
  TemporaryDisable(Sender);
  Up := m_history.Perform(EM_GETFIRSTVISIBLELINE, 0, 0);
  m_history.Lines.BeginUpdate();
  HistoryToText(m_history.Lines);
  m_history.Lines.EndUpdate();
  SendMessage(m_history.Handle, EM_LINESCROLL, 0, Up);
  TemporaryDisable();
end;

procedure TFedit.b_selectClick(Sender: TObject);
begin
  m_history.SelectAll();
  m_history.SetFocus();
end;

procedure TFedit.b_acceptClick(Sender: TObject);
var
  Res: string;
begin
  if GuiClosing then
    Exit;
  if SprintMode then
    Exit;
  TemporaryDisable(Sender);
  Res := HistoryFromText(m_history.Lines);
  if Res <> '' then
    Report(SpyroTASName + ' Text history error, near to:', Res);
  TemporaryDisable();
end;

procedure TFedit.FormResize(Sender: TObject);
begin
  if Height < 130 then
    Height := 130;
end;

procedure TFedit.m_historyKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^A then
    b_select.Click();
end;

procedure TFedit.FormCreate(Sender: TObject);
begin
  Caption := 'Textual key history:';
end;

end.

