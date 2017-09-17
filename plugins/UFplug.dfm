object Fplug: TFplug
  Left = 541
  Top = 325
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 161
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    443
    161)
  PixelsPerInch = 96
  TextHeight = 16
  object l_pad: TLabel
    Left = 8
    Top = 62
    Width = 104
    Height = 16
    Caption = ':                        :'
    Color = clBtnFace
    ParentColor = False
  end
  object l_gpu: TLabel
    Left = 8
    Top = 112
    Width = 104
    Height = 16
    Caption = ':                        :'
    Color = clBtnFace
    ParentColor = False
  end
  object cb_pad: TComboBox
    Left = 3
    Top = 34
    Width = 436
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    TabOrder = 0
    OnChange = cb_padChange
    Items.Strings = (
      '0'
      '1'
      '2'
      '3')
  end
  object b_test: TButton
    Left = 78
    Top = 132
    Width = 71
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = 'Test'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsItalic, fsUnderline]
    ParentFont = False
    TabOrder = 2
    OnClick = b_testClick
    OnContextPopup = b_testContextPopup
  end
  object b_configure: TButton
    Left = 153
    Top = 132
    Width = 71
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = 'Configure'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsItalic, fsUnderline]
    ParentFont = False
    TabOrder = 3
    OnClick = b_configureClick
    OnContextPopup = b_configureContextPopup
  end
  object b_about: TButton
    Left = 228
    Top = 132
    Width = 71
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = 'About'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsItalic, fsUnderline]
    ParentFont = False
    TabOrder = 4
    OnClick = b_aboutClick
    OnContextPopup = b_aboutContextPopup
  end
  object b_ok: TButton
    Left = 3
    Top = 132
    Width = 71
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = b_okClick
  end
  object b_refresh: TButton
    Left = 303
    Top = 132
    Width = 71
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = 'Refresh'
    TabOrder = 5
    OnClick = b_refreshClick
  end
  object e_states: TEdit
    Left = 162
    Top = 4
    Width = 182
    Height = 24
    TabOrder = 6
    Text = '.\sstates\'
  end
  object b_padkeys: TButton
    Left = 378
    Top = 132
    Width = 63
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = '[Keys]'
    TabOrder = 7
    OnClick = b_padkeysClick
  end
  object cb_language: TComboBox
    Left = 3
    Top = 4
    Width = 155
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    TabOrder = 8
    OnChange = cb_languageChange
    Items.Strings = (
      '0'
      '1'
      '2'
      '3')
  end
  object se_warp: TSpinEdit
    Left = 349
    Top = 3
    Width = 89
    Height = 26
    MaxValue = 256
    MinValue = 4
    TabOrder = 9
    Value = 16
  end
  object cb_gpu: TComboBox
    Left = 3
    Top = 84
    Width = 436
    Height = 24
    Style = csDropDownList
    ItemHeight = 16
    TabOrder = 10
    OnChange = cb_gpuChange
    Items.Strings = (
      '0'
      '1'
      '2'
      '3')
  end
end
