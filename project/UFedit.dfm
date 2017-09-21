object Fedit: TFedit
  Left = 289
  Top = 255
  Width = 700
  Height = 400
  BorderIcons = [biSystemMenu]
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object m_history: TMemo
    Left = 0
    Top = 0
    Width = 631
    Height = 373
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      
        '| 00000000 |   0 | --- ---- -- ---- --- | --- ---- -- ---- --- |' +
        '      0 | 0 -----')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
    WordWrap = False
    OnKeyPress = m_historyKeyPress
  end
  object pnl_editor: TPanel
    Left = 631
    Top = 0
    Width = 61
    Height = 373
    Align = alRight
    TabOrder = 1
    object b_accept: TButton
      Left = 6
      Top = 60
      Width = 51
      Height = 24
      Caption = 'Accept'
      TabOrder = 0
      OnClick = b_acceptClick
    end
    object b_reload: TButton
      Left = 6
      Top = 32
      Width = 51
      Height = 24
      Caption = 'Reload'
      TabOrder = 1
      OnClick = b_reloadClick
    end
    object b_select: TButton
      Left = 6
      Top = 4
      Width = 51
      Height = 24
      Caption = 'Select'
      TabOrder = 2
      OnClick = b_selectClick
    end
  end
end
