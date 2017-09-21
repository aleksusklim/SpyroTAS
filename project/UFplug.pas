unit UFplug; // SpyroTAS is licensed under WTFPL

// plugin config window

interface

uses
  Utas, Windows, Forms, Controls, StdCtrls, Classes, Uforms, Spin;

type
  TFplug = class(TFSpyroTAS)
    cb_pad: TComboBox;
    b_test: TButton;
    b_configure: TButton;
    b_about: TButton;
    b_ok: TButton;
    l_pad: TLabel;
    b_refresh: TButton;
    e_states: TEdit;
    b_padkeys: TButton;
    cb_language: TComboBox;
    se_warp: TSpinEdit;
    l_gpu: TLabel;
    cb_gpu: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure cb_padChange(Sender: TObject);
    procedure b_refreshClick(Sender: TObject);
    procedure b_aboutClick(Sender: TObject);
    procedure b_configureClick(Sender: TObject);
    procedure b_testClick(Sender: TObject);
    procedure b_okClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure b_padkeysClick(Sender: TObject);
    procedure cb_languageChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cb_gpuChange(Sender: TObject);
    procedure b_configureContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
    procedure b_aboutContextPopup(Sender: TObject; MousePos: TPoint; var Handled:
      Boolean);
    procedure b_testContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  public
    function CombineDllList(): TStringList;
    procedure InitConfigureForm();
    function JustLoadAndCall(DllName, FuncName: string; ThisIsTest: Boolean): Integer;
  private
    AllowContinue: Boolean; // for standalone call
  end;

var
  Fplug: TFplug = nil;

implementation

{$R *.dfm}

uses
  Ulang, SysUtils, Dialogs, Uroute, UFbind, Ugpu, Uini, Umisc;

 // TODO:
procedure ExportHelper();
var
  Name: string;
  Stream: TFileStream;
  Resource: TResourceStream;
begin
  Name := GetModuleName(HInstance);
  Name := PathFixBackslash(ExtractFilePath(Name)) + SpyroTASHelper +
    ExtractFileName(Name);
  if FileExists(Name) then
    Exit;
  Stream := TFileStream.Create(Name, fmCreate);
  Resource := TResourceStream.Create(HInstance, SpyroTASHelperRes, RT_RCDATA);
  Stream.CopyFrom(Resource, Resource.Size);
  Resource.Free();
  Stream.Free();
end;

// seek for real GPU plugins:
function TFplug.CombineDllList(): TStringList;
var
  Search: TSearchRec;
  Index: Integer;
  DirectoryList: TStringList; // temporary
begin
  DirectoryList := TStringList.Create();
  Result := TStringList.Create(); // owned by caller
  if FindFirst(EmulatorRoot + '*', faDirectory, Search) = 0 then
  begin  // find all directories (including "." and "..")
    repeat
      DirectoryList.Add(Search.Name + '\');
    until FindNext(Search) <> 0;
    FindClose(Search);
  end;
  for Index := 0 to DirectoryList.Count - 1 do
  begin // find all DLLs in them
    if FindFirst(EmulatorRoot + DirectoryList[Index] + '*.dll', faAnyFile,
      Search) = 0 then
    begin
      repeat // one big list
        Result.Add(DirectoryList[Index] + Search.Name);
      until FindNext(Search) <> 0;
      FindClose(Search);
    end;
  end;
  DirectoryList.Free(); // since temp
end;

// picking a plugin from list - update the name of selected DLL
// actually this can crash everything, so it's not called default first time:
procedure TFplug.cb_gpuChange(Sender: TObject);
var
  TestDllHandle: Integer;
  OurSpyroTAS: Pointer;
  GoodAndNotOur: Boolean;
begin
  l_gpu.Caption := ''; // this will we readdable name + version
  if cb_gpu.ItemIndex < 0 then
  begin
    // we can't do anything if no plugins
    b_ok.Enabled := False;
    b_test.Enabled := False;
    b_configure.Enabled := False;
    Exit;
  end;
  GoodAndNotOur := False;
  TestDllHandle := LoadLibrary(PChar(EmulatorRoot + cb_gpu.Items[cb_gpu.ItemIndex]));
  if (TestDllHandle <> 0) and (TestDllHandle <> -1) then
  begin
    try
      // if it is GPU plugin type
      if (PINT(GetProcAddress(TestDllHandle, PChar('PSEgetLibType'))) and 2) <> 0 then
      begin
        // construct name title:
        l_gpu.Caption := PluginTitleFromNameAndVersion(string(PSTR(GetProcAddress
          (TestDllHandle, PChar('PSEgetLibName')))), PINT(GetProcAddress(TestDllHandle,
          PChar('PSEgetLibVersion'))));
        // but that may be our own instance! Flag it otherwise:
        OurSpyroTAS := GetProcAddress(TestDllHandle, PChar('Spyro_TAS'));
        if (OurSpyroTAS = nil) or (PINT(OurSpyroTAS) <> SelfDllTime) then
          GoodAndNotOur := True;
      end;
    except // no need to report error
      FreeLibrary(TestDllHandle);
    end;
  end;
  // enable buttons if all ok:
  b_ok.Enabled := GoodAndNotOur;
  b_configure.Enabled := GoodAndNotOur;
  b_test.Enabled := GoodAndNotOur;
end;

// refreshing plugin list
procedure TFplug.InitConfigureForm();
var
  AllDllsList: TStringList;
  Index, ItIsOurGpu, ItIsOurPad: Integer;
  TestDllHandle: Integer;
  CurrentGpuFromIni, CurrentPadFromIni: string;
  Data: TPairsArray;
begin
  // clear UI:
  cb_gpu.ItemIndex := -1;
  cb_pad.ItemIndex := -1;
  l_gpu.Caption := '';
  l_pad.Caption := '';
  AllDllsList := nil;
  ItIsOurGpu := -1; // it will be index of current plugin in the list
  ItIsOurPad := -1;
  cb_gpu.Items.BeginUpdate();
  cb_pad.Items.BeginUpdate();
  try
    SetLength(Data, 4);
    Data[0] := MakePair('gpu_plugin', '');
    Data[1] := MakePair('dir_states', '.\sstates\');
    Data[2] := MakePair('warp_size', 16);
    Data[3] := MakePair('pad_plugin', '');
    IniSectionUpdate(PathToIni, SectionSpyrotas, Data, False);
    GetPair(Data[0], CurrentGpuFromIni);
    GetPair(Data[1], SavestateDirectory);
    GetPair(Data[2], MoveLimitMb);
    GetPair(Data[3], CurrentPadFromIni);
    e_states.Text := SavestateDirectory;
    se_warp.Value := MoveLimitMb;
    // get all DLLs filelist:
    AllDllsList := CombineDllList(); // we'll free it here later
    cb_pad.Items.Clear();
    cb_gpu.Items.Clear();
    // filter to only GPU callable DLLs:
    for Index := 0 to AllDllsList.Count - 1 do
    begin
      // Load DLL without executing anything:
      SetErrorMode(Cardinal(-1));
      TestDllHandle := LoadLibraryExA(PChar(EmulatorRoot + AllDllsList[Index]),
        0, DONT_RESOLVE_DLL_REFERENCES);
      SetErrorMode(0); // better would be restore previous...
      if (TestDllHandle = 0) or (TestDllHandle = -1) then
        Continue; // skip wrong
      try
        // Make sure some important functions existed:
        if (GetProcAddress(TestDllHandle, PChar('PSEgetLibName')) <> nil) and (GetProcAddress
          (TestDllHandle, PChar('PSEgetLibVersion')) <> nil) and (GetProcAddress
          (TestDllHandle, PChar('PSEgetLibType')) <> nil) then
        begin
          if (GetProcAddress(TestDllHandle, PChar('GPUupdateLace')) <> nil) then
          begin
            cb_gpu.Items.Add(AllDllsList[Index]);
            // If it is our current active plugin, store its index:
            if AllDllsList[Index] = CurrentGpuFromIni then
              ItIsOurGpu := cb_gpu.Items.Count - 1;
          end;
          if ((GetProcAddress(TestDllHandle, PChar('PADstartPoll')) <> nil) or (GetProcAddress
            (TestDllHandle, PChar('PADreadPort1')) <> nil)) and (GetProcAddress(TestDllHandle,
            PChar('Spyro_TAS')) = nil) then
          begin
            cb_pad.Items.Add(AllDllsList[Index]);
            if AllDllsList[Index] = CurrentPadFromIni then
              ItIsOurPad := cb_pad.Items.Count - 1;
          end;
        end;
      finally
        FreeLibrary(TestDllHandle); // free it anyway
      end;
    end;
  except
    on E: Exception do
      Report(SpyroTASName + ' get DLL list exception:', E.Message);
  end;
  AllDllsList.Free();
  cb_pad.Items.EndUpdate();
  cb_gpu.Items.EndUpdate();
  cb_pad.ItemIndex := ItIsOurPad;
  if ItIsOurPad = -1 then
    cb_padChange(nil);
  cb_gpu.ItemIndex := ItIsOurGpu;
  if ItIsOurGpu = -1 then
    cb_gpuChange(nil); // disable UI if no selected
end;

// call About, Configure or Test:
function TFplug.JustLoadAndCall(DllName, FuncName: string; ThisIsTest: Boolean): Integer;
var
  DllHandle: Integer;
  FunctionPtr: Pointer;
begin
  Result := -1;
  DllName := EmulatorRoot + DllName;
  DllHandle := LoadLibrary(PChar(DllName)); // just load
  if (DllHandle <> 0) and (DllHandle <> -1) then
  begin
    try
      FunctionPtr := GetProcAddress(DllHandle, PChar(FuncName));
      if FunctionPtr <> nil then  // and call
      begin
        if ThisIsTest then
          Result := PINT(FunctionPtr)  // Test() returns 0 or -1
        else
          VOID(FunctionPtr); // About() and Configure() are procedures
      end;
    except
      on E: Exception do
        Report(SpyroTASName + ' routed ' + DllName + ' ' + FuncName +
          '() exception:', E.Message);
    end;
    FreeLibrary(DllHandle);
  end;
end;

// "ABOUT" (for real plugin) handler:
procedure TFplug.b_aboutClick(Sender: TObject);
begin
  if cb_gpu.ItemIndex >= 0 then
    JustLoadAndCall(cb_gpu.Items[cb_gpu.ItemIndex], 'GPUabout', False);
end;

// "CONFIGURE" (for real plugin) handler:
procedure TFplug.b_configureClick(Sender: TObject);
begin
  JustLoadAndCall(cb_gpu.Items[cb_gpu.ItemIndex], 'GPUconfigure', False);
end;

procedure TFplug.b_aboutContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
  if cb_pad.ItemIndex >= 0 then
    JustLoadAndCall(cb_pad.Items[cb_pad.ItemIndex], 'PADabout', False);
end;

procedure TFplug.b_configureContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
  if cb_pad.ItemIndex >= 0 then
    JustLoadAndCall(cb_pad.Items[cb_pad.ItemIndex], 'PADconfigure', False);
end;

procedure TFplug.b_testContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
  b_testClick(Sender);
end;

// "TEST" (for real plugin) handler:
procedure TFplug.b_testClick(Sender: TObject);
var
  Res: string; // to show user
begin
  Res := 'GPU: "' + cb_gpu.Items[cb_gpu.ItemIndex] + '"'#13#10#13#10;
  if JustLoadAndCall(cb_gpu.Items[cb_gpu.ItemIndex], 'GPUtest', True) = 0 then
    Res := Res + 'Should work.' // library OK, GPUtest returned 0
  else
    Res := Res + 'MAY NOT work!';
  if cb_pad.ItemIndex >= 0 then
  begin
    Res := Res + #13#10#13#10'PAD: "' + cb_pad.Items[cb_pad.ItemIndex] + '"'#13#10#13#10;
    if JustLoadAndCall(cb_pad.Items[cb_pad.ItemIndex], 'PADtest', True) = 0 then
      Res := Res + 'Should work.' // library OK, GPUtest returned 0
    else
      Res := Res + 'MAY NOT work!';
  end;
  Report(SpyroTASName + ' testing real plugins:', Res);
end;

// "REFRESH" (refill plugins list) handler:
procedure TFplug.b_refreshClick(Sender: TObject);
begin
  InitConfigureForm();
  cb_padChange(nil);
  cb_gpuChange(nil); // invoke selected file anyway
end;

// "OK" (save and close) handler:
procedure TFplug.b_okClick(Sender: TObject);
var
  SelectedGpu, SelectedPad: string;
  Data: TPairsArray;
begin
  try
    ExportHelper();
    SelectedGpu := cb_gpu.Items[cb_gpu.ItemIndex];
    SelectedPad := cb_pad.Items[cb_pad.ItemIndex];
    SetLength(Data, 5);
    SavestateDirectory := PathFixBackslash(e_states.Text);
    MoveLimitMb := se_warp.Value;
    Data[0] := MakePair('gpu_plugin', SelectedGpu);
    Data[1] := MakePair('dir_states', SavestateDirectory);
    Data[2] := MakePair('warp_size', MoveLimitMb);
    Data[3] := MakePair('pad_plugin', SelectedPad);
    Data[4] := MakePair('tas_lang', cb_language.ItemIndex);
    IniSectionUpdate(PathToIni, SectionSpyrotas, Data, True);
    if MoveLimitMb < 4 then
      MoveLimitMb := 4
    else if MoveLimitMb > 256 then
      MoveLimitMb := 256;
    GpuMovieLimit := MoveLimitMb * 1024 * 1024;
    SavestateDirectory := EnsureAbsolutePath(SavestateDirectory);
    if not DirectoryExists(SavestateDirectory) then
      Report(SpyroTASName + ' warning:', 'Savestate directory "' +
        SavestateDirectory + '" not found!');
    if not MainIsAlreadyInited then // when current real plugin wasn't activated,
      DoDllLoad(SelectedGpu, SelectedPad); //  we still can switch to a selected one
  except
    on E: Exception do
      Report(SpyroTASName + ' save and reload exception:', E.Message);
  end;
  AllowContinue := True;
  Self.Close; // "OK" must close the window and return control to the emulator
end;

procedure TFplug.FormCreate(Sender: TObject);
begin
  Fplug := Self;
  InitConfigureForm(); // populate the list first time
  InitHints(False);
  PutHints0(); // context help
  Caption := SpyroTASName + SpyroTASVestionString +
    ' - configuration; select the real GPU plugin:';
  OnClose := FormClose;
  cb_language.Items.BeginUpdate();
  cb_language.Items.Clear();
  cb_language.Items.Add('English');
  cb_language.Items.Add('Russian');
  cb_language.Items.EndUpdate();
  cb_language.ItemIndex := Integer(LanguageCurrent);
end;

procedure TFplug.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Ignores(Action);
  if IsRunDLL and not AllowContinue then
    TerminateProcess(GetCurrentProcess(), 0); // if window was closed, but OK isn't pressed
end;

procedure TFplug.b_padkeysClick(Sender: TObject);
begin
  Fbind := TFbind.CreateUsual();
  PutHints2();
  LoadPadKeys();
  Fbind.ShowModal();
  FreeAndNil(Fbind);
end;

procedure TFplug.cb_languageChange(Sender: TObject);
var
  LanguageNew: TLanguage;
begin
  case cb_language.ItemIndex of
    0:
      LanguageNew := LanguageEnglish;
    1:
      LanguageNew := LanguageRussian;
  else
    Exit;
  end;
  if LanguageNew = LanguageCurrent then
    Exit;
  LanguageCurrent := LanguageNew;
  PutHints0();
end;

procedure TFplug.FormShow(Sender: TObject);
begin
  TopMost(True);
  Popup();
  TopMost(False);
end;

procedure TFplug.cb_padChange(Sender: TObject);
var
  TestDllHandle: Integer;
begin
  l_pad.Caption := '';
  if cb_pad.ItemIndex < 0 then
    Exit;
  TestDllHandle := LoadLibrary(PChar(EmulatorRoot + cb_pad.Items[cb_pad.ItemIndex]));
  if (TestDllHandle <> 0) and (TestDllHandle <> -1) then
  begin
    try
      if (PINT(GetProcAddress(TestDllHandle, PChar('PSEgetLibType'))) and 8) <> 0 then
      begin
        l_pad.Caption := PluginTitleFromNameAndVersion(string(PSTR(GetProcAddress
          (TestDllHandle, PChar('PSEgetLibName')))), PINT(GetProcAddress(TestDllHandle,
          PChar('PSEgetLibVersion'))));
      end;
    except
      FreeLibrary(TestDllHandle);
    end;
  end;
end;

end.

// EOF


