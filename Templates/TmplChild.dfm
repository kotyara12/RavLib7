inherited ChildTemplate: TChildTemplate
  Caption = 'ChildTemplate'
  Menu = MainMenu
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object CoolBar: TCoolBar
    Left = 0
    Top = 0
    Width = 500
    Height = 27
    AutoSize = True
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        Width = 500
      end>
    EdgeBorders = [ebTop]
    object ToolBar: TToolBar
      Left = 9
      Top = 0
      Width = 487
      Height = 25
      AutoSize = True
      Caption = #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      EdgeBorders = []
      Flat = True
      ShowCaptions = True
      TabOrder = 0
    end
  end
  object ActionList: TActionList
    Left = 12
    Top = 64
    object CloseSelect: TAction
      Category = #1060#1072#1081#1083
      Caption = #1042#1099#1073#1086#1088
      Enabled = False
      Hint = #1042#1099#1073#1088#1072#1090#1100' '#1079#1072#1087#1080#1089#1100' '#1080' '#1079#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
      ImageIndex = 0
      ShortCut = 13
      Visible = False
    end
    object CloseCancel: TAction
      Category = #1060#1072#1081#1083
      Caption = #1047#1072#1082#1088#1099#1090#1100
      Enabled = False
      Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
      ImageIndex = 1
      ShortCut = 16499
    end
  end
  object MainMenu: TMainMenu
    Left = 40
    Top = 64
  end
end
