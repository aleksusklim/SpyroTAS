object Fbind: TFbind
  Left = 204
  Top = 231
  BorderStyle = bsDialog
  Caption = 'KEYBINDINGS'
  ClientHeight = 366
  ClientWidth = 542
  Color = clBtnHighlight
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  ShowHint = True
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object page_all: TPageControl
    Left = 3
    Top = 5
    Width = 535
    Height = 357
    ActivePage = tab_help
    TabOrder = 0
    TabStop = False
    object tab_pad: TTabSheet
      Caption = '   Pad mappings   '
      object grd_pad: TStringGrid
        Left = 0
        Top = 0
        Width = 521
        Height = 329
        TabStop = False
        ColCount = 16
        DefaultColWidth = 128
        DefaultRowHeight = 18
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing]
        ParentFont = False
        ScrollBars = ssNone
        TabOrder = 0
        OnContextPopup = grd_anyContextPopup
        OnGetEditText = grd_anyGetEditText
        ColWidths = (
          128
          128
          128
          128
          128
          128
          128
          128
          128
          128
          128
          128
          128
          128
          128
          128)
        RowHeights = (
          18
          18
          18
          18
          18)
      end
    end
    object tab_func: TTabSheet
      Caption = '   Functional hotkeys   '
      ImageIndex = 1
      object grd_hot: TStringGrid
        Left = 0
        Top = 0
        Width = 262
        Height = 166
        TabStop = False
        ColCount = 2
        DefaultColWidth = 128
        DefaultRowHeight = 18
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing]
        ScrollBars = ssNone
        TabOrder = 0
        OnContextPopup = grd_anyContextPopup
        OnGetEditText = grd_anyGetEditText
        ColWidths = (
          128
          128)
        RowHeights = (
          18
          18
          18
          18
          18)
      end
      object grd_emu: TStringGrid
        Left = 262
        Top = 0
        Width = 262
        Height = 166
        TabStop = False
        ColCount = 2
        DefaultColWidth = 128
        DefaultRowHeight = 18
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing]
        ScrollBars = ssNone
        TabOrder = 1
        OnContextPopup = grd_anyContextPopup
        OnGetEditText = grd_anyGetEditText
        ColWidths = (
          128
          128)
        RowHeights = (
          18
          18
          18
          18
          18)
      end
      object c_pad_routed: TCheckBox
        Left = 264
        Top = 60
        Width = 257
        Height = 17
        Caption = 'PAD plugin is used instead of Windows messages'
        TabOrder = 2
        OnClick = c_pad_routedClick
      end
    end
    object tab_help: TTabSheet
      Caption = '   SpyroTAS general help!  '
      ImageIndex = 2
      object m_help: TMemo
        Left = 0
        Top = 0
        Width = 527
        Height = 329
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object timer_key: TTimer
    Enabled = False
    Interval = 10
    OnTimer = timer_keyTimer
    Left = 504
    Top = 8
  end
end
