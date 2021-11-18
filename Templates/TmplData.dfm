inherited DataTemplate: TDataTemplate
  Left = 567
  Top = 391
  Width = 847
  Height = 489
  Caption = 'DataTemplate'
  Menu = MainMenu
  OldCreateOrder = True
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 411
    Width = 831
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object CoolBar: TCoolBar
    Left = 0
    Top = 0
    Width = 831
    Height = 29
    AutoSize = True
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        Width = 831
      end>
    EdgeBorders = [ebTop, ebBottom]
    object ToolBar: TToolBar
      Left = 9
      Top = 0
      Width = 818
      Height = 25
      AutoSize = True
      Caption = #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      EdgeBorders = []
      Flat = True
      Images = BaseData.ImageList
      ShowCaptions = True
      TabOrder = 0
    end
  end
  object ActionList: TActionList
    Images = BaseData.ImageList
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
      OnExecute = CloseSelectExecute
      OnUpdate = CloseSelectUpdate
    end
    object CloseCancel: TAction
      Category = #1060#1072#1081#1083
      Caption = #1047#1072#1082#1088#1099#1090#1100
      Enabled = False
      Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
      ImageIndex = 1
      ShortCut = 16499
      OnExecute = CloseCancelExecute
      OnUpdate = CloseCancelUpdate
    end
    object ShowHelp: TAction
      Category = #1055#1086#1084#1086#1097#1100
      Caption = #1057#1087#1088#1072#1074#1082#1072
      Enabled = False
      Hint = #1042#1099#1079#1086#1074' '#1089#1087#1088#1072#1074#1086#1095#1085#1086#1081' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1080
      ImageIndex = 3
      ShortCut = 112
      OnExecute = ShowHelpExecute
      OnUpdate = ShowHelpUpdate
    end
    object AboutBox: TAction
      Category = #1055#1086#1084#1086#1097#1100
      Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
      Enabled = False
      Hint = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
      ImageIndex = 2
      OnExecute = AboutBoxExecute
      OnUpdate = AboutBoxUpdate
    end
    object Refresh: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1054#1073#1085#1086#1074#1080#1090#1100
      Enabled = False
      Hint = #1055#1077#1088#1077#1095#1080#1090#1072#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080#1079' '#1073#1072#1079#1099' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 12
      ShortCut = 116
      OnExecute = RefreshExecute
      OnUpdate = RefreshUpdate
    end
    object Find: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082
      Enabled = False
      Hint = #1055#1086#1080#1089#1082' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 13
      ShortCut = 118
    end
  end
  object PopupMenu: TPopupMenu
    Images = BaseData.ImageList
    Left = 68
    Top = 64
    object itemFindP: TMenuItem
      Action = Find
    end
    object divPopupSubmenus: TMenuItem
      Caption = '-'
      Visible = False
    end
    object menuOperationsP: TMenuItem
      Caption = #1054#1087#1077#1088#1072#1094#1080#1080
      Hint = #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1076#1072#1085#1085#1099#1093' '#1080' '#1076#1086#1089#1090#1091#1087' '#1082' '#1076#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1086#1081' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1080
      ImageIndex = 24
      Visible = False
    end
    object menuReportsP: TMenuItem
      Caption = #1054#1090#1095#1077#1090#1099
      Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1086#1090#1095#1077#1090#1086#1074', '#1087#1077#1095#1072#1090#1100' '#1080' '#1101#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 23
      Visible = False
    end
    object divPopupRefresh: TMenuItem
      Caption = '-'
    end
    object itemRefreshP: TMenuItem
      Action = Refresh
    end
    object divPopupEnd: TMenuItem
      Caption = '-'
    end
    object itemCloseSelectP: TMenuItem
      Action = CloseSelect
    end
    object itemCloseCancelP: TMenuItem
      Action = CloseCancel
    end
  end
  object MainMenu: TMainMenu
    Images = BaseData.ImageList
    Left = 40
    Top = 64
    object menuEdit: TMenuItem
      Caption = #1055#1088#1072#1074#1082#1072
      GroupIndex = 10
      object itemRefresh: TMenuItem
        Action = Refresh
      end
    end
    object menuData: TMenuItem
      Caption = #1044#1072#1085#1085#1099#1077
      GroupIndex = 20
      object itemFind: TMenuItem
        Action = Find
      end
    end
    object menuOperations: TMenuItem
      Caption = #1054#1087#1077#1088#1072#1094#1080#1080
      GroupIndex = 50
      Visible = False
    end
    object menuReports: TMenuItem
      Caption = #1054#1090#1095#1077#1090#1099
      GroupIndex = 180
    end
    object menuHelp: TMenuItem
      Caption = #1055#1086#1084#1086#1097#1100
      GroupIndex = 220
      object itemHelpContext: TMenuItem
        Action = ShowHelp
      end
      object divHelp1: TMenuItem
        Caption = '-'
      end
      object itemAboutBox: TMenuItem
        Action = AboutBox
      end
    end
  end
  object DataPopupMenu: TPopupMenu
    Images = BaseData.ImageList
    Left = 96
    Top = 64
  end
  object OperationsPopupMenu: TPopupMenu
    Images = BaseData.ImageList
    Left = 124
    Top = 64
  end
  object ReportsPopupMenu: TPopupMenu
    Images = BaseData.ImageList
    Left = 152
    Top = 64
  end
end
