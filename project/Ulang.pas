unit Ulang; // SpyroTAS is licensed under WTFPL

// context hints on gui

interface

uses
  Windows, Graphics, IniFiles, Classes, SysUtils;

{$IFDEF FPC}
  {$R SpyroTASlangs.rc}
{$ELSE}
  {$R SpyroTASlangs.res}
{$ENDIF}

procedure InitHints(Okay: Boolean); // global settings for hints

procedure PutHints0(); // plugin search dialog

procedure PutHints1(); // before main initialization

procedure PutHints2(); // keybindings window

procedure PutHints3(); // all gui windows

type
  TLanguage = (LanguageNone = -1, LanguageEnglish = 0, LanguageRussian = 1);

{$IFNDEF FPC}
type
  TSystemCodePage = Word;

  RawByteString = string;
{$ENDIF}

var
  LanguageCurrent: TLanguage = LanguageEnglish;
  LanguageOld: TLanguage = LanguageNone;
  LanguageTable: THashedStringList = nil;
  LanguageCP: TSystemCodePage;

implementation

uses
  Utas, Forms, UFgui, UFhide, UFbind, UFext, UFshot, UFplug, UFedit, Uforms,
  Umisc;

procedure SetHintFont(Charset: TFontCharset; Name: string = 'Tahoma'; Size: Integer = 10);
begin
  HintsFont := Name;
  HintsCharset := Charset;
  HintsSize := Size;
  if LastHint <> nil then
    with LastHint.Canvas do
    begin
      Font.Name := HintsFont;
      Font.Charset := HintsCharset;
      Font.Size := HintsSize;
    end;
  if Fbind <> nil then
    with Fbind.m_help do
    begin
      Font.Name := HintsFont;
      Font.Charset := HintsCharset;
    end;
end;

procedure AssignLanguage();
var
  Stream: TResourceStream;
  Lang: string;
begin
  if LanguageOld = LanguageCurrent then
    Exit;
  if LanguageTable = nil then
  begin
    LanguageTable := THashedStringList.Create();
    LanguageTable.CaseSensitive := False;

  end;
  LanguageTable.Clear();
  LanguageOld := LanguageCurrent;
  Stream := nil;
  Lang := '';
  try
    case LanguageCurrent of
      LanguageEnglish:
        begin
          SetHintFont(EASTEUROPE_CHARSET);
          Lang := 'SPYROTASLANG_EN';
          LanguageCP := 1250;
        end;
      LanguageRussian:
        begin
          SetHintFont(RUSSIAN_CHARSET);
          Lang := 'SPYROTASLANG_RU';
          LanguageCP := 1251;
        end;
    end;
    if Lang <> '' then
    begin
      Stream := TResourceStream.Create(HInstance, Lang, RT_RCDATA);
      LanguageTable.LoadFromStream(Stream);
    end
    else
      LanguageTable.Clear();
  except
  end;
  Stream.Free();
end;

procedure InitHints(Okay: Boolean);
begin // can be called multiple times
  TSpyroTASHintOkay := Okay;
  HintWindowClass := TSpyroTASHint;
  Application.HintPause := 200; // 1/5 sec.
  Application.HintShortPause := 0; // mainly for spin-edits
  Application.HintHidePause := 180000; // 2 minutes
end;

function GetText(ID: string): RawByteString;
var
  Index, Last: Integer;
  Line: string;
begin
  Result := '<UNDEFINED TEXT> $' + ID;
  if LanguageTable = nil then
    Exit;
  Index := LanguageTable.IndexOf('$' + ID);
  Last := LanguageTable.Count - 1;
  if (Index < 0) or (Index > Last) then
    Exit;
  Result := '';
  while Index < Last do
  begin
    Inc(Index);
    Line := LanguageTable[Index];
    if (Line <> '') and (Line[1] = '$') then
      Break;
    Result := Result + Line + #13;
  end;
  Result := Trim(Result);
  {$IFDEF FPC}
  SetCodePage(Result, LanguageCP, False);
  {$ENDIF}
end;

procedure PutHints0();
begin
  AssignLanguage();
  with Fplug do
  begin
    SetPermanentHint(cb_language, GetText('cb_language'));
    SetPermanentHint(b_ok, GetText('b_ok'));
    SetPermanentHint(b_test, GetText('b_test'));
    SetPermanentHint(b_configure, GetText('b_configure'));
    SetPermanentHint(b_about, GetText('b_about'));
    SetPermanentHint(b_refresh, GetText('b_refresh'));
    SetPermanentHint(cb_gpu, GetText('cb_gpu'));
    SetPermanentHint(l_gpu, GetText('l_gpu'));
    SetPermanentHint(b_padkeys, GetText('b_padkeys'));
    SetPermanentHint(e_states, GetText('e_states'));
    SetPermanentHint(se_warp, GetText('se_warp'));
    SetPermanentHint(cb_pad, GetText('cb_pad'));
    SetPermanentHint(l_pad, GetText('l_pad'));
  end;
end;

procedure PutHints1();
begin
  AssignLanguage();
  with Fgui do
  begin
    SetPermanentHint(b_invoke, GetText('b_invoke'));
  end;
end;

procedure PutHints2();
begin
  AssignLanguage();
  with Fbind do
  begin
    SetPermanentGridHint(grd_hot, 0, 1, GetText('grd_hot_0_1'));
    SetPermanentGridHint(grd_hot, 0, 2, GetText('grd_hot_0_2'));
    SetPermanentGridHint(grd_hot, 0, 3, GetText('grd_hot_0_3'));
    SetPermanentGridHint(grd_hot, 0, 4, GetText('grd_hot_0_4'));
    SetPermanentGridHint(grd_hot, 0, 5, GetText('grd_hot_0_5'));
    SetPermanentGridHint(grd_hot, 0, 6, GetText('grd_hot_0_6'));
    SetPermanentGridHint(grd_hot, 0, 7, GetText('grd_hot_0_7'));
    SetPermanentGridHint(grd_hot, 0, 8, GetText('grd_hot_0_8'));
    SetPermanentGridHint(grd_hot, 0, 9, GetText('grd_hot_0_9'));
    SetPermanentGridHint(grd_hot, 0, 10, GetText('grd_hot_0_10'));
    SetPermanentGridHint(grd_pad, 0, 0, GetText('grd_pad_0_0'));
    SetPermanentGridHint(grd_pad, 1, 0, GetText('grd_pad_1_0'));
    SetPermanentGridHint(grd_pad, 2, 0, GetText('grd_pad_2_0'));
    SetPermanentGridHint(grd_pad, 3, 0, GetText('grd_pad_3_0'));
    SetPermanentGridHint(grd_hot, -1, 0, GetText('grd_hot_-1_0'));
    SetPermanentGridHint(grd_emu, -1, 0, GetText('grd_emu_-1_0'));
    SetPermanentGridHint(grd_emu, 0, 1, GetText('grd_emu_0_1'));
    SetPermanentGridHint(grd_emu, 0, 2, GetText('grd_emu_0_2'));
    SetPermanentHint(c_pad_routed, GetText('c_pad_routed'));
    SetPermanentHint(c_autofreeze, GetText('c_autofreeze'));
    m_help.Text := StringReplace(GetText('m_help'), #13, #13#10, [rfReplaceAll]);
  end;
end;

procedure PutHints3();
begin
  AssignLanguage();
  Fhide.Hint := GetText('fhide');
  with Fgui do
  begin
    SetPermanentHint(lst_history, GetText('lst_history'));
    SetPermanentHint(b_restart, GetText('b_restart'));
    SetPermanentHint(b_load, GetText('b_load'));
    SetPermanentHint(b_current, GetText('b_current'));
    SetPermanentHint(b_free, GetText('b_free'));
    SetPermanentHint(b_save, GetText('b_save'));
    SetPermanentHint(b_delete, GetText('b_delete'));
    SetPermanentHint(b_keys, GetText('b_keys'));
    SetPermanentHint(b_ext, GetText('b_ext'));
    SetPermanentHint(b_capt, GetText('b_capt'));
    SetPermanentHint(b_halt, GetText('b_halt'));
    SetPermanentHint(se_fps, GetText('se_fps'));
    SetPermanentHint(c_limit, GetText('c_limit'));
    SetPermanentHint(c_skip, GetText('c_skip'));
    SetPermanentHint(c_semi, GetText('c_semi'));
    SetPermanentHint(c_2nd, GetText('c_2nd'));
    SetPermanentHint(c_hash, GetText('c_hash'));
    SetPermanentHint(c_auto, GetText('c_auto'));
    SetPermanentHint(c_shot, GetText('c_shot'));
    SetPermanentHint(se_threshold, GetText('se_threshold'));
    SetPermanentHint(e_position, GetText('e_position'));
    SetPermanentHint(se_timepoint, GetText('se_timepoint'));
    SetPermanentHint(pnl_drop, GetText('pnl_drop'));
    SetPermanentHint(b_hints, GetText('b_hints'));
    SetPermanentHint(c_warp, GetText('c_warp'));
  end;
  with Fext do
  begin
    SetPermanentHint(b_unpack, GetText('b_unpack'));
    SetPermanentHint(b_store, GetText('b_store'));
    SetPermanentHint(b_remove, GetText('b_remove'));
    SetPermanentHint(b_export, GetText('b_export'));
    SetPermanentHint(lst_arch, GetText('lst_arch'));
    SetPermanentHint(pnl_keys, GetText('pnl_keys'));
    SetPermanentHint(lst_spyro, GetText('lst_spyro'));
    SetPermanentHint(b_clear, GetText('b_clear'));
    SetPermanentHint(c_spyro, GetText('c_spyro'));
    SetPermanentHint(c_spyro2, GetText('c_spyro2'));
    SetPermanentHint(e_spyro, GetText('e_spyro'));
    SetPermanentHint(b_rename, GetText('b_rename'));
    SetPermanentHint(e_rename, GetText('e_rename'));
  end;
  with Fshot do
  begin
    SetPermanentHint(r_avi, GetText('r_avi'));
    SetPermanentHint(r_png, GetText('r_png'));
    SetPermanentHint(r_bmp, GetText('r_bmp'));
    SetPermanentHint(b_codec, GetText('b_codec'));
    SetPermanentHint(b_combine, GetText('b_combine'));
    SetPermanentHint(se_avi, GetText('se_avi'));
    SetPermanentHint(se_png, GetText('se_png'));
    SetPermanentHint(c_diff, GetText('c_diff'));
    SetPermanentHint(c_ever, GetText('c_ever'));
    SetPermanentHint(b_see, GetText('b_see'));
    SetPermanentHint(b_over, GetText('b_over'));
  end;
  with Fedit do
  begin
    SetPermanentHint(b_select, GetText('b_select'));
    SetPermanentHint(b_reload, GetText('b_reload'));
    SetPermanentHint(b_accept, GetText('b_accept'));
  end;
end;

end.

// EOF


