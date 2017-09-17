unit UFext; // SpyroTAS is licensed under WTFPL

// extended form with archives, semi-keys and spyro-stuff

interface

uses
  Utas, Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Uforms;

type
  TFext = class(TFSpyroTAS)
    lst_arch: TListBox;
    b_store: TButton;
    b_remove: TButton;
    b_unpack: TButton;
    b_export: TButton;
    pnl_keys: TPanel;
    c_pad_Left: TCheckBox;
    c_pad_Right: TCheckBox;
    c_pad_Up: TCheckBox;
    c_pad_Down: TCheckBox;
    c_pad_L3: TCheckBox;
    c_pad_Select: TCheckBox;
    c_pad_Start: TCheckBox;
    c_pad_R3: TCheckBox;
    c_pad_Cross: TCheckBox;
    c_pad_Square: TCheckBox;
    c_pad_Circle: TCheckBox;
    c_pad_Triangle: TCheckBox;
    c_pad_R1: TCheckBox;
    c_pad_R2: TCheckBox;
    c_pad_L2: TCheckBox;
    c_pad_L1: TCheckBox;
    file_save: TSaveDialog;
    lst_spyro: TListBox;
    c_spyro: TCheckBox;
    c_spyro2: TCheckBox;
    b_clear: TButton;
    file_open: TOpenDialog;
    e_spyro: TEdit;
    l_keys: TLabel;
    b_rename: TButton;
    e_rename: TEdit;
    procedure FormShow(Sender: TObject);
    procedure b_removeClick(Sender: TObject);
    procedure b_unpackClick(Sender: TObject);
    procedure b_storeClick(Sender: TObject);
    procedure b_exportClick(Sender: TObject);
    procedure lst_archDblClick(Sender: TObject);
    procedure c_padClick(Sender: TObject);
    procedure c_spyroClick(Sender: TObject);
    procedure b_clearClick(Sender: TObject);
    procedure c_spyro2Click(Sender: TObject);
    procedure b_exportContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure e_spyroChange(Sender: TObject);
    procedure lst_archContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
    procedure lst_archClick(Sender: TObject);
    procedure b_renameClick(Sender: TObject);
  end;

var
  Fext: TFext = nil;

procedure ChooseSemiAuto();

implementation

{$R *.dfm}

uses
  Math, UFgui, UFbind, Uini, Ukey, Umisc, Uglob, Umain;

var
  SemiInternal: Boolean = False;
  CheckboxArray: array[0..15] of TCheckBox;


// internal sorting for ReloadArchivesList():
function ReloadArchivesList_sort(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
end;


// import an archive to current history
procedure LoadFromArchive(Filename: string);
var
  Search: TSearchRec;
  Index, Size: Integer;
  Archive, Store: TFileStream;
begin
  Index := 0;
  Size := 0;
  if FindFirst(PathToSpyroTAS + 'tmp*.tmp', faAnyFile, Search) = 0 then
    repeat // delete everything from current history
      DeleteFile(PathToSpyroTAS + Search.Name);
    until FindNext(Search) <> 0;
  FindClose(Search);
  Archive := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  while Archive.Position < Archive.Size do // read sequentel
  begin
    Archive.ReadBuffer(Index, 4); // special or keystate number
    Archive.ReadBuffer(Size, 4); // Size
    Store := TFileStream.Create(NameOfKeystate(Index), fmCreate);
    if Size > 0 then // maybe need more validation...
      Store.CopyFrom(Archive, Size);
    Store.Free;
  end;
  Archive.Free;
  ReloadKeystateList(); // update currents immediately
  if Length(KeystateList) > 0 then
  begin // set last saved position to max available
    SetLastestPosition(KeystateList[Length(KeystateList) - 1]);
  end;
end;

// search for available Archives:
procedure ReloadArchivesList();
var
  Search: TSearchRec;
  Index: Integer;
begin
  if ArchivesList = nil then // create a list first time
    ArchivesList := TStringList.Create();
  ArchivesList.Clear; // clear old
  if FindFirst(PathToSpyroTAS + '*' + ExtForArchive, faAnyFile, Search) <> 0 then
  begin
    FindClose(Search);
    Exit; // nothing is found
  end;
  repeat
    ArchivesList.AddObject(ChangeFileExt(Search.Name, ''), {%H-}Pointer(Search.Time));
      // add
  until FindNext(Search) <> 0; // all founded
  ArchivesList.CustomSort(ReloadArchivesList_sort); // sort by modification time
  FindClose(Search);
  Fext.lst_arch.Items.BeginUpdate();
  Fext.lst_arch.Clear(); // fill the visual list
  for Index := 0 to ArchivesList.Count - 1 do
    Fext.lst_arch.Items.AddObject(ArchivesList[Index], {%H-}Pointer(Index)); // TODO
  Fext.lst_arch.Items.EndUpdate();
end;

// store current history as new archive:
procedure SaveToArchive();
var
  Index, Size: Integer;
  Source, Archive: TFileStream;
  Name: string;
begin
  Name := PathToSpyroTAS + 'SAVE_' + TimeToString(Now()) + ExtForArchive; // unique title
  Archive := TFileStream.Create(Name, fmCreate);
  Index := KeystateMinimalIndex; // minimal special internal name
  Name := NameOfKeystate(Index);
  while FileExists(Name) do
  begin
    Source := TFileStream.Create(Name, fmOpenRead or fmShareDenyNone);
    Archive.WriteBuffer(Index, 4); // store number
    Size := Source.Size;
    Archive.WriteBuffer(Size, 4); // store Size
    if Size > 0 then
      Archive.CopyFrom(Source, Size); // store content
    Source.Free();
    Inc(Index); // don't forget go increment and get name
    Name := NameOfKeystate(Index);
  end;
  Archive.Free();
  ReloadArchivesList(); // also update list
end;

procedure ChooseSemiAuto();
var
  Index: Integer;
begin
  Fext.lst_archClick(nil);
  SemiInternal := True;
  if Fgui.c_semi.Checked then
  begin
    Fext.l_keys.Caption := 'SEMI';
    for Index := 0 to 15 do
    begin
      CheckboxArray[Index].Checked := (GlobalPadMask and PadButton[Index]) <> 0;
      CheckboxArray[Index].Enabled := True;
    end;
  end
  else if Fgui.c_auto.Checked then
  begin
    Fext.l_keys.Caption := 'AUTO';
    for Index := 0 to 15 do
    begin
      CheckboxArray[Index].Checked := (KeyAutofireMask and PadButton[Index]) <> 0;
      CheckboxArray[Index].Enabled := True;
    end;
  end
  else
  begin
    Fext.l_keys.Caption := 'NONE';
    for Index := 0 to 15 do
    begin
      CheckboxArray[Index].Checked := False;
      CheckboxArray[Index].Enabled := False;
    end;
  end;
  SemiInternal := False;
end;

// update list at showing:
procedure TFext.FormShow(Sender: TObject);
begin
  ReloadArchivesList();
end;

// "REMOVE" handler:
procedure TFext.b_removeClick(Sender: TObject);
var
  Index: Integer;
begin
  if GuiClosing then
    Exit;
  Index := lst_arch.ItemIndex;
  if Index < 0 then
    Exit;
  TemporaryDisable(Sender);
  DeleteFile(PathToSpyroTAS + lst_arch.Items[Index] + '.SpyroTAS');
  lst_arch.Items.Delete(Index);
  if lst_arch.Count > Index then // preserve selection
    lst_arch.ItemIndex := Index
  else
    lst_arch.ItemIndex := lst_arch.Count - 1;
  Sleep(DefaultActionSleep);
  TemporaryDisable();
end;

// "UNPACK" handler:
procedure TFext.b_unpackClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if lst_arch.ItemIndex < 0 then
    Exit;
  Fgui.b_free.Click();
  if Sender <> nil then
    RequestSwitch(); // stop the game
  TemporaryDisable(Sender);
  LoadFromArchive(PathToSpyroTAS + lst_arch.Items[lst_arch.ItemIndex] +
    '.SpyroTAS'); // actual code there
  TemporaryDisable();
end;

// "STORE" handler:
procedure TFext.b_storeClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if Sender <> nil then
    TemporaryDisable(Sender);
  SaveToArchive();
  lst_arch.ItemIndex := lst_arch.Count - 1;
  TemporaryDisable();
end;

// "EXPORT" handler:
procedure TFext.b_exportClick(Sender: TObject);
const
  ThisKey: string = 'export_save';
var
  Name, Ext: string;
  Sprint: Boolean;
  Num: Integer;
begin
  if GuiClosing then
    Exit;
  if Length(KeystateList) < 1 then
    Exit;
  if SprintMode then
    Exit;
  Sprint := (Sender = Fgui.b_delete);
  if Sprint then
    Num := Fgui.lst_history.ItemIndex
  else
    Num := 0;
  Fgui.b_free.Click();
  Fgui.timer_main.Enabled := False;
  TemporaryDisable(Sender);
  RequestSwitch(); // stop the game during dialogue
  AllFormsAction(afaDisable); // release gui top-most
  file_save.InitialDir := IniValueUpdate(PathToIni, SectionLastpath, ThisKey);
  if Sprint then
    file_save.Filter := 'SpyroTAS savestate for sprint|' + SpyroTASFilters
  else
    file_save.Filter := 'SpyroTAS history and savestate|' + SpyroTASFilters;
  file_save.DefaultExt := ExtSaveState;
  if file_save.Execute then
  begin
    Name := file_save.FileName;
    IniValueUpdate(PathToIni, SectionLastpath, ThisKey, ExtractFilePath(Name));
      // store directory
    Ext := LowerCase(ExtractFileExt(Name));
    if (Ext = LowerCase(ExtKeyHistory)) or (Ext = LowerCase(ExtSaveState)) then
      Name := ChangeFileExt(Name, '')
    else if Ext = LowerCase(ExtSprintHist) then
      Name := ChangeFileExt(ChangeFileExt(Name, ''), '');
    file_save.FileName := Name;
    CopyFile(PChar(NameOfKeystate(Num)), PChar(Name + ExtSaveState), False);
    if Sprint then
    begin
      SprintState := Name + ExtSaveState;
      SprintFile := '';
      EnterSptintMode(True);
    end
    else
      ImportExportHistory(Name + ExtKeyHistory, False); // actual code there
  end;
  AllFormsAction(afaEnable); // restore top-most
  TemporaryDisable();
  Fgui.timer_main.Enabled := True;
end;

// double click to list - load and run:
procedure TFext.lst_archDblClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  b_unpackClick(nil); // without requesting switch
  if Fgui.lst_history.Items.Count > 0 then
  begin // when have something
    Fgui.lst_history.ItemIndex := 0;
    Fgui.timer_mainTimer(nil);
    SwitchRequested := False; // prevent switching
    ResetLoadSave();
    Fgui.b_load.Click() // call for loading immediately
  end;
end;

// click to any of semi-checkboxes:
procedure TFext.c_padClick(Sender: TObject);
var
  Index: Integer;
begin
  if GuiClosing then
    Exit;
  if SemiInternal then
    Exit;
  if Fgui.c_semi.Checked then
  begin
    GlobalPadMask := 0;
    for Index := 0 to 15 do
      if CheckboxArray[Index].Checked then
        GlobalPadMask := GlobalPadMask or PadButton[Index];
  end
  else if Fgui.c_auto.Checked then
  begin
    KeyAutofireMask := 0;
    for Index := 0 to 15 do
      if CheckboxArray[Index].Checked then
        KeyAutofireMask := KeyAutofireMask or PadButton[Index];
  end;
  SaveSettingsScheduled := True;
end;

// "CLEAR" hander:
procedure TFext.b_clearClick(Sender: TObject);
begin
  lst_spyro.Clear();
end;

// point A checkbox:
procedure TFext.c_spyroClick(Sender: TObject);
begin
  if IsRunDLL then
    Exit;
  SaveSettingsScheduled := True;
  SpyroUse1 := c_spyro.Checked;
  if SpyroUse1 and (SpyroOffset <> 0) then
  begin
    SpyroOffsetX := SpyroOffset;
    SpyroOffsetY := SpyroOffset + 4;
    SpyroOffsetZ := SpyroOffset + 8;
    e_spyro.Enabled := False;
  end
  else
  begin
    SpyroOffsetX := 0;
    SpyroOffsetY := 0;
    SpyroOffsetZ := 0;
    e_spyro.Enabled := True;
    c_spyro.Checked := False;
  end;
  SpyroDist := 0;
  SpyroAway := True;
  c_spyro2.Enabled := SpyroUse1; // allow B only when A is checked
  if SpyroOffset <> 0 then
  begin  // get current coordinates
    SpyroX1 := PInteger(PointerToRamStart + SpyroOffsetX)^;
    SpyroY1 := PInteger(PointerToRamStart + SpyroOffsetY)^;
    SpyroZ1 := PInteger(PointerToRamStart + SpyroOffsetZ)^;
  end;
end;

// point B checkbox:
procedure TFext.c_spyro2Click(Sender: TObject);
begin
  SpyroUse2 := c_spyro2.Checked;
  SpyroTime2 := 0;
  SpyroDist := 0;
  SpyroDist1Prev := 0;
  SpyroDist2Prev := 0;
  SpyroApproach := True;
  if IsRunDLL then
    Exit;
  // more coordinates
  SpyroX2 := PInteger(PointerToRamStart + SpyroOffsetX)^;
  SpyroY2 := PInteger(PointerToRamStart + SpyroOffsetY)^;
  SpyroZ2 := PInteger(PointerToRamStart + SpyroOffsetZ)^;
  SpyroDistMax := SquaredDistance(SpyroX2 - SpyroX1, SpyroY2 - SpyroY1, SpyroZ2
    - SpyroZ1); // distance between points
end;

procedure TFext.b_exportContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
const
  ThisKey: string = 'export_load';
begin
  if GuiClosing then
    Exit;
  Ignores(MousePos);
  Handled := True;
  RequestSwitch();
  Fgui.timer_main.Enabled := False;
  TemporaryDisable(Sender);
  AllFormsAction(afaDisable);
  file_open.InitialDir := IniValueUpdate(PathToIni, SectionLastpath, ThisKey);
  file_open.Filter := 'Import SpyroTAS savestate /  history / sprint|' +
    SpyroTASFilters + ';*.00?';
  if file_open.Execute then
  begin
    IniValueUpdate(PathToIni, SectionLastpath, ThisKey, ExtractFilePath(file_open.FileName));
    AllFormsAction(afaEnable);
    ImportSpyroTAS(file_open.FileName);
    Fgui.timer_mainTimer(nil);
  end
  else
    AllFormsAction(afaEnable);
  TemporaryDisable();
  Fgui.timer_main.Enabled := True;
end;

procedure TFext.FormCreate(Sender: TObject);
begin
  CheckboxArray[0] := c_pad_Up;
  CheckboxArray[1] := c_pad_Right;
  CheckboxArray[2] := c_pad_Down;
  CheckboxArray[3] := c_pad_Left;
  CheckboxArray[4] := c_pad_Triangle;
  CheckboxArray[5] := c_pad_Circle;
  CheckboxArray[6] := c_pad_Cross;
  CheckboxArray[7] := c_pad_Square;
  CheckboxArray[8] := c_pad_L1;
  CheckboxArray[9] := c_pad_L2;
  CheckboxArray[10] := c_pad_R1;
  CheckboxArray[11] := c_pad_R2;
  CheckboxArray[12] := c_pad_Start;
  CheckboxArray[13] := c_pad_Select;
  CheckboxArray[14] := c_pad_L3;
  CheckboxArray[15] := c_pad_R3;
end;

procedure TFext.e_spyroChange(Sender: TObject);
begin
  SpyroOffset := (StringToRamOffset(e_spyro.Text) and $7fffffff);
  if (SpyroOffset < 0) or (SpyroOffset > SizeOfRAM - 16) then
    SpyroOffset := 0;
  c_spyro.Enabled := (SpyroOffset > 0);
  if SpyroOffset > 0 then
    SpyroOffsetText := e_spyro.Text
  else
    SpyroOffsetText := '0x00000000';
end;

procedure TFext.lst_archContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  if GuiClosing then
    Exit;
  Handled := True;
  Ignores(MousePos);
  if lst_arch.ItemIndex < 0 then
    Exit;
  e_rename.Visible := True;
  b_rename.Visible := True;
  pnl_keys.Visible := False;
  e_rename.Text := lst_arch.Items[lst_arch.ItemIndex];
end;

procedure TFext.lst_archClick(Sender: TObject);
begin
  e_rename.Visible := False;
  b_rename.Visible := False;
  pnl_keys.Visible := True;
end;

procedure TFext.b_renameClick(Sender: TObject);
var
  Index: Integer;
begin
  if RenameFile(SpyroTASDirectory + lst_arch.Items[lst_arch.ItemIndex] +
    ExtForArchive, SpyroTASDirectory + e_rename.Text + ExtForArchive) then
  begin
    ReloadArchivesList();
    for Index := 0 to lst_arch.Count - 1 do
      if lst_arch.Items[Index] = e_rename.Text then
        lst_arch.ItemIndex := Index;
    ChooseSemiAuto();
  end;
end;

end.

// EOF


