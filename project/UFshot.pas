unit UFshot; // SpyroTAS is licensed under WTFPL

// screenshots settings form

interface

uses
  Utas, {%H-}Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, ComCtrls, ExtDlgs, Uforms;

type
  TFshot = class(TFSpyroTAS)
    b_codec: TButton;
    c_diff: TCheckBox;
    c_ever: TCheckBox;
    r_avi: TRadioButton;
    r_bmp: TRadioButton;
    r_png: TRadioButton;
    se_avi: TSpinEdit;
    se_png: TSpinEdit;
    tr_alpha: TTrackBar;
    b_over: TButton;
    b_see: TButton;
    b_combine: TButton;
    dlg_img: TOpenPictureDialog;
    procedure se_pngChange(Sender: TObject);
    procedure c_diffClick(Sender: TObject);
    procedure c_everClick(Sender: TObject);
    procedure tr_alphaChange(Sender: TObject);
    procedure b_overClick(Sender: TObject);
    procedure r_aviClick(Sender: TObject);
    procedure r_pngClick(Sender: TObject);
    procedure r_bmpClick(Sender: TObject);
    procedure b_codecClick(Sender: TObject);
    procedure se_aviChange(Sender: TObject);
    procedure b_seeClick(Sender: TObject);
    procedure b_combineClick(Sender: TObject);
    procedure tr_alphaContextPopup(Sender: TObject; MousePos: TPoint; var
      Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure b_overContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
  end;

var
  Fshot: TFshot = nil;

type
  TScreenshotType = (stAvi, stPng, stBmp);

var
  ShotType: TScreenshotType;
  ShotPath: string = '';

implementation

{$R *.dfm}

uses
  Umisc, UFgui, UFbind, UFview, UFover, Uavi, Uglob, Umain;

// set compression:
procedure TFshot.se_pngChange(Sender: TObject);
begin
  PngCompression := se_png.Value;
  SaveSettingsScheduled := True;
end;

// set fps:
procedure TFshot.se_aviChange(Sender: TObject);
begin
  AviFrameRate := se_avi.Value;
  SaveSettingsScheduled := True;
end;

// "DIFF" handler:
procedure TFshot.c_diffClick(Sender: TObject);
begin
  ShotSkipSame := c_diff.Checked;
  SaveSettingsScheduled := True;
end;

// "EVER" handler:
procedure TFshot.c_everClick(Sender: TObject);
begin
  ShotFreeMode := c_ever.Checked;
  SaveSettingsScheduled := True;
end;

// set alpha for overlays:
procedure TFshot.tr_alphaChange(Sender: TObject);
begin
  SaveSettingsScheduled := True;
  AdjustAllAlpha(tr_alpha.Position);
  tr_alpha.Hint := IntToStr(PadWindowsAlpha);
  if Fshot.Visible then
    tr_alpha.SetFocus();
end;

// right click to restore alpha:
procedure TFshot.tr_alphaContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
  PadWindowsAlpha := PadAlphaDefault;
  tr_alpha.Position := PadWindowsAlpha;
end;

// "[OVER]" handler:
procedure TFshot.b_overClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if Fover2.Visible then
  begin
    Fover1.Hide();
    Fover2.Hide();
  end
  else // toggle pad overlays
  begin
    if UseSecond or Fover1.Visible then
      Fover2.Show();
    Fover1.Show();
  end;
end;

// mode "AVI" handler:
procedure TFshot.r_aviClick(Sender: TObject);
begin
  if not AviIsAvailable then
  {%H-}begin
    r_bmp.Checked := True;
    r_avi.Enabled := False;
    Exit;
  end;
  SaveSettingsScheduled := True;
  ShotType := stAvi;
  if not AviConfigured then // show codec options first time
    b_codec.Click;
end;

// mode "PNG" handler:
procedure TFshot.r_pngClick(Sender: TObject);
begin
  if not PngIsAvailable then
  {%H-}begin
    r_bmp.Checked := True;
    r_png.Enabled := False;
    Exit;
  end;
  SaveSettingsScheduled := True;
  ShotType := stPng;
  Fgui.c_shot.Enabled := True;
end;

// mode "BMP" handler:
procedure TFshot.r_bmpClick(Sender: TObject);
begin
  SaveSettingsScheduled := True;
  ShotType := stBmp;
  Fgui.c_shot.Enabled := True;
end;

// "CODEC..." handler:
procedure TFshot.b_codecClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if not AviIsAvailable then
  {%H-}begin
    b_codec.Enabled := False;
    Exit;
  end;
  Fgui.timer_main.Enabled := False;
  TemporaryDisable(Sender);
  RequestSwitch();
  Fgui.c_shot.Enabled := False; // disbale screenshoting
  Fgui.b_capt.Enabled := False;
  MakeGameScreenshot(True); // special call for settings, see there
  Fgui.b_capt.Enabled := True;
  if ShotType = stAvi then
    Fgui.c_shot.Enabled := AviConfigured // checkbox respects success
  else
    Fgui.c_shot.Enabled := True;
  TemporaryDisable();
  Fgui.timer_main.Enabled := True;
end;

// "[PREV]" handler:
procedure TFshot.b_seeClick(Sender: TObject);
begin
  if GuiClosing then
    Exit;
  if Fview.Visible then // toggle preview
    Fview.Hide()
  else
    Fview.Show();
end;

// "COMBINE..." handler:
procedure TFshot.b_combineClick(Sender: TObject);

  function Load(Name: string): TBitmap; // (inner scope) read one image:
  var
    Ext: string;
  begin
    Result := nil;
    Ext := UpperCase(ExtractFileExt(Name));
    if Ext = '.BMP' then // allowed only .bmp and .png files
    begin
      Result := TBitmap.Create;
      Result.LoadFromFile(Name); // as is
    end;
    if Ext = '.PNG' then
    begin
      Result := TBitmap.Create;
      LoadPngAsBitmap(Result, Name);
    end;
  end;

var
  Serach: TSearchRec;
  Name, Ext: string;
  List: TStringList;
  Index: Integer;
  Bitmap: TBitmap;
begin // (body)
  if GuiClosing then
    Exit;
  if not AviIsAvailable then
  {%H-}begin
    b_combine.Enabled := False;
    Exit;
  end;
  Fgui.timer_main.Enabled := False;
  TemporaryDisable(Sender);
  RequestSwitch(); // stop the game
  dlg_img.InitialDir := ExpandFileName(PathToSpyroTAS + SpyroTASForScreenshots);
  dlg_img.Filter := '*.bmp;*.png|*.bmp;*.png;*.avi'; // AVI as a hint
  AllFormsAction(afaDisable); // release on-top
  List := TStringList.Create;
  try
    Caption := 'COMBINE';
    if dlg_img.Execute then
    begin
      Ext := UpperCase(ExtractFileExt(dlg_img.FileName));
      if (Ext <> '.BMP') and (Ext <> '.PNG') then
      begin // he clicked an AVI
        Report('Combine error:',
          'Only .BMP and .PNG source images are supported!', Handle);
        Abort;
      end;
      Name := ExtractFilePath(dlg_img.FileName);
      Bitmap := Load(dlg_img.FileName);
      if Bitmap = nil then
        Abort;
      AviConfigured := False; // we must request new compression settings
      if not avi_settings(Fshot, Bitmap, AviFrameRate) then
      begin
        Bitmap.Free();
        Abort;
      end;
      if FindFirst(Name + '*', faAnyFile, Serach) <> 0 then
        Abort;
      repeat // building sequence
        Ext := UpperCase(ExtractFileExt(Serach.Name));
        if (Ext = '.BMP') or ((Ext = '.PNG') and PngIsAvailable) then
          List.Add(Name + Serach.Name);
      until FindNext(Serach) <> 0;
      FindClose(Serach);
      Delete(Name, Length(Name), 1); // backslash
      Ext := ExtractFileName(Name); // parent folder
      Caption := Ext + '.avi';
      try
        Name := Name + '\..\' + Ext + '.avi';
        if not avi_create(Name) then
          Abort;
        for Index := 0 to List.Count - 1 do // process all
        begin
          Caption := IntToStr(Index + 1) + '/' + IntToStr(List.Count); // show progress
          Bitmap := Load(List.Strings[Index]);
          if Bitmap = nil then
          begin
            Bitmap.Free();
            Abort;
          end;
          if not avi_write(Bitmap) then
          begin
            Bitmap.Free();
            Abort;
          end;
          Bitmap.Free();
        end;
      except
        Report('Combine error:', 'Could not save to "' + Name + '"', Handle);
      end;
      avi_close();
    end;
    Caption := 'DONE';
  except
    Caption := 'CANCELED';
  end;
  List.Free();
  AllFormsAction(afaEnable); // restore on-top
  TemporaryDisable();
  Fgui.timer_main.Enabled := True;
end;

// restore window title:
procedure TFshot.FormShow(Sender: TObject);
begin
  Caption := 'Capture:';
end;

procedure TFshot.b_overContextPopup(Sender: TObject; MousePos: TPoint; var
  Handled: Boolean);
begin
  Ignores(MousePos);
  Handled := True;
  if FoverF.Visible then
  begin
    if FoverR.Visible then
    begin
      FoverF.Visible := False;
      FoverR.Visible := False;
    end
    else
      FoverR.Visible := True;
  end
  else
    FoverF.Visible := True;
end;

end.

// EOF



