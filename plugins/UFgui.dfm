object Fgui: TFgui
  Left = 219
  Top = 31
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 175
  ClientWidth = 315
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = True
  Position = poDefault
  ShowHint = True
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object pnl_main: TPanel
    Left = 0
    Top = 0
    Width = 315
    Height = 175
    Align = alClient
    TabOrder = 1
    DesignSize = (
      315
      175)
    object c_shot: TCheckBox
      Left = 200
      Top = 154
      Width = 47
      Height = 20
      Caption = 'Shot'
      TabOrder = 15
      OnClick = c_shotClick
    end
    object e_position: TEdit
      Left = 100
      Top = 118
      Width = 79
      Height = 24
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
      ReadOnly = True
      TabOrder = 5
      Text = '--'
      OnDblClick = e_positionDblClick
    end
    object se_fps: TSpinEdit
      Left = 263
      Top = 31
      Width = 50
      Height = 26
      MaxValue = 999
      MinValue = 1
      TabOrder = 21
      Value = 60
      OnChange = se_fpsChange
    end
    object b_halt: TButton
      Left = 263
      Top = 3
      Width = 48
      Height = 25
      Caption = 'HALT'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 20
      OnClick = b_haltClick
      OnContextPopup = b_haltContextPopup
    end
    object b_load: TButton
      Left = 100
      Top = 31
      Width = 52
      Height = 25
      Caption = 'Load'
      TabOrder = 2
      OnClick = b_loadClick
      OnContextPopup = b_loadContextPopup
    end
    object lst_history: TListBox
      Left = 3
      Top = 2
      Width = 94
      Height = 169
      Align = alCustom
      ItemHeight = 16
      TabOrder = 0
      OnClick = lst_historyClick
      OnContextPopup = lst_historyContextPopup
      OnDblClick = lst_historyDblClick
    end
    object c_limit: TCheckBox
      Left = 264
      Top = 58
      Width = 48
      Height = 20
      Caption = 'Limit'
      TabOrder = 22
      OnClick = c_limitClick
    end
    object b_delete: TButton
      Left = 156
      Top = 59
      Width = 48
      Height = 25
      Caption = 'Delete'
      TabOrder = 9
      OnClick = b_deleteClick
      OnContextPopup = b_deleteContextPopup
    end
    object b_free: TButton
      Left = 156
      Top = 3
      Width = 48
      Height = 25
      Caption = 'Free'
      TabOrder = 7
      OnClick = b_freeClick
      OnContextPopup = b_freeContextPopup
    end
    object b_restart: TButton
      Left = 100
      Top = 3
      Width = 52
      Height = 25
      Caption = 'Restart'
      TabOrder = 1
      OnClick = b_restartClick
      OnContextPopup = b_restartContextPopup
    end
    object se_timepoint: TSpinEdit
      Left = 100
      Top = 146
      Width = 97
      Height = 26
      MaxValue = 99999
      MinValue = 0
      TabOrder = 6
      Value = 0
      OnChange = se_timepointChange
    end
    object b_current: TButton
      Left = 100
      Top = 59
      Width = 52
      Height = 25
      Caption = 'Current'
      TabOrder = 3
      OnClick = b_currentClick
      OnContextPopup = b_currentContextPopup
    end
    object c_hash: TCheckBox
      Left = 200
      Top = 120
      Width = 50
      Height = 20
      Caption = 'Hash'
      TabOrder = 13
      OnClick = c_hashClick
    end
    object b_ext: TButton
      Left = 208
      Top = 3
      Width = 51
      Height = 25
      Caption = '[Ext]'
      TabOrder = 17
      OnClick = b_extClick
      OnContextPopup = b_extContextPopup
    end
    object se_threshold: TSpinEdit
      Left = 100
      Top = 88
      Width = 97
      Height = 26
      MaxValue = 99999
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = se_thresholdChange
    end
    object c_semi: TCheckBox
      Left = 200
      Top = 86
      Width = 50
      Height = 20
      Caption = 'Semi'
      TabOrder = 11
      OnClick = c_semiClick
    end
    object pnl_drop: TPanel
      Tag = -1
      Left = 255
      Top = 112
      Width = 56
      Height = 60
      Anchors = [akRight, akBottom]
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 19
      object shp_status: TShape
        Left = 0
        Top = 0
        Width = 56
        Height = 60
        OnMouseDown = shp_statusMouseDown
      end
    end
    object c_2nd: TCheckBox
      Left = 200
      Top = 103
      Width = 45
      Height = 20
      Caption = '2-nd'
      TabOrder = 12
      OnClick = c_2ndClick
    end
    object c_auto: TCheckBox
      Left = 200
      Top = 137
      Width = 47
      Height = 20
      Caption = 'Auto'
      TabOrder = 14
      OnClick = c_autoClick
    end
    object b_keys: TButton
      Left = 208
      Top = 31
      Width = 51
      Height = 25
      Hint = 'Hotkey window'
      Caption = '[Keys]'
      TabOrder = 16
      OnClick = b_keysClick
      OnContextPopup = b_keysContextPopup
    end
    object b_save: TButton
      Left = 156
      Top = 31
      Width = 48
      Height = 25
      Caption = 'Save'
      TabOrder = 8
      OnClick = b_saveClick
      OnContextPopup = b_saveContextPopup
    end
    object b_capt: TButton
      Left = 208
      Top = 59
      Width = 51
      Height = 25
      Caption = '[Capt]'
      TabOrder = 18
      OnClick = b_captClick
      OnContextPopup = b_captContextPopup
    end
    object c_skip: TCheckBox
      Left = 264
      Top = 74
      Width = 46
      Height = 20
      Caption = 'Skip'
      TabOrder = 23
      OnClick = c_skipClick
    end
    object b_hints: TButton
      Left = 179
      Top = 118
      Width = 16
      Height = 23
      Caption = '?'
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 10
      OnClick = b_hintsClick
      OnContextPopup = b_hintsContextPopup
    end
    object c_warp: TCheckBox
      Left = 258
      Top = 92
      Width = 51
      Height = 20
      Caption = 'Warp'
      TabOrder = 24
      OnClick = c_warpClick
    end
  end
  object pnl_init: TPanel
    Left = 0
    Top = 0
    Width = 45
    Height = 39
    TabOrder = 0
    object b_invoke: TButton
      Left = 3
      Top = 3
      Width = 211
      Height = 32
      Caption = 'INVOKE'
      TabOrder = 0
      OnClick = b_invokeClick
      OnContextPopup = b_invokeContextPopup
    end
  end
  object timer_main: TTimer
    Enabled = False
    Interval = 25
    OnTimer = timer_mainTimer
    Left = 268
    Top = 132
  end
end
