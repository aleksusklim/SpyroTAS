object Fshot: TFshot
  Left = 834
  Top = 140
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 115
  ClientWidth = 177
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefault
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object c_diff: TCheckBox
    Left = 5
    Top = 65
    Width = 34
    Height = 17
    Caption = 'Diff'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = c_diffClick
  end
  object c_ever: TCheckBox
    Left = 47
    Top = 64
    Width = 40
    Height = 17
    Caption = 'Ever'
    TabOrder = 5
    OnClick = c_everClick
  end
  object tr_alpha: TTrackBar
    Left = 2
    Top = 85
    Width = 172
    Height = 26
    Max = 255
    Min = 1
    ParentShowHint = False
    Position = 196
    ShowHint = True
    TabOrder = 0
    OnContextPopup = tr_alphaContextPopup
    OnChange = tr_alphaChange
  end
  object b_over: TButton
    Left = 136
    Top = 58
    Width = 37
    Height = 23
    Caption = '[Over]'
    TabOrder = 11
    OnClick = b_overClick
    OnContextPopup = b_overContextPopup
  end
  object b_see: TButton
    Left = 95
    Top = 58
    Width = 37
    Height = 23
    Caption = '[See]'
    TabOrder = 8
    OnClick = b_seeClick
  end
  object b_combine: TButton
    Left = 112
    Top = 31
    Width = 61
    Height = 23
    Caption = 'Combine...'
    TabOrder = 9
    OnClick = b_combineClick
  end
  object r_avi: TRadioButton
    Left = 8
    Top = 8
    Width = 35
    Height = 14
    Caption = 'AVI'
    TabOrder = 2
    OnClick = r_aviClick
  end
  object r_png: TRadioButton
    Left = 8
    Top = 24
    Width = 41
    Height = 17
    Caption = 'PNG'
    TabOrder = 3
    OnClick = r_pngClick
  end
  object r_bmp: TRadioButton
    Left = 8
    Top = 43
    Width = 41
    Height = 17
    Caption = 'BMP'
    Checked = True
    TabOrder = 4
    TabStop = True
    OnClick = r_bmpClick
  end
  object se_png: TSpinEdit
    Left = 57
    Top = 33
    Width = 52
    Height = 22
    MaxValue = 9
    MinValue = 0
    TabOrder = 7
    Value = 6
    OnChange = se_pngChange
  end
  object se_avi: TSpinEdit
    Left = 117
    Top = 4
    Width = 55
    Height = 22
    MaxValue = 9999
    MinValue = 1
    TabOrder = 10
    Value = 30
    OnChange = se_aviChange
  end
  object b_codec: TButton
    Left = 54
    Top = 5
    Width = 56
    Height = 23
    Caption = 'Codec...'
    TabOrder = 6
    OnClick = b_codecClick
  end
  object dlg_img: TOpenPictureDialog
    Options = [ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 66
    Top = 88
  end
end
