inherited QueryCustomTemplate: TQueryCustomTemplate
  Left = 451
  Top = 266
  Height = 522
  Caption = 'QueryCustomTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Top = 444
    Panels = <
      item
        Text = #1053#1077#1090' '#1076#1072#1085#1085#1099#1093
        Width = 160
      end
      item
        Text = #1053#1072#1073#1086#1088' '#1076#1072#1085#1085#1099#1093' '#1079#1072#1082#1088#1099#1090
        Width = 160
      end
      item
        Width = 1000
      end>
  end
  inherited CoolBar: TCoolBar
    Height = 40
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 786
      end>
    inherited ToolBar: TToolBar
      Height = 36
    end
  end
  inherited DataPanel: TPanel
    Top = 69
    Height = 375
    inherited DbGrid: TRDbStyledGrid
      Height = 278
    end
    inherited InfoPanel: TRDbInfoPanel
      Top = 299
      Height = 76
    end
    inherited TabViews: TTabSet
      Top = 278
    end
  end
  inherited FindPanel: TPanel
    Top = 40
  end
  inherited ActionList: TActionList
    inherited Find: TAction [9]
    end
    inherited DataSetCopy: TAction [13]
    end
    inherited ColumnLeft: TAction [15]
    end
    inherited ColumnCenter: TAction [16]
    end
    inherited ColumnRight: TAction [17]
    end
    object FilterUser: TAction [18]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103'...'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1087#1088#1086#1080#1079#1074#1086#1083#1100#1085#1086#1075#1086' '#1092#1080#1083#1100#1090#1088#1072' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 15
      ShortCut = 119
      OnExecute = FilterUserExecute
      OnUpdate = FilterUserUpdate
    end
    object FilterDefault: TAction [19]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1092#1080#1083#1100#1090#1088#1072' '#1076#1072#1085#1085#1099#1093' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      ImageIndex = 16
      ShortCut = 16503
      OnExecute = FilterDefaultExecute
      OnUpdate = FilterDefaultUpdate
    end
    object FilterSelected: TAction [20]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' "'#1087#1086' '#1074#1099#1076#1077#1083#1077#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' '#1087#1086' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1084' '#1079#1072#1087#1080#1089#1103#1084
      ImageIndex = 16
      ShortCut = 49271
      OnExecute = FilterSelectedExecute
      OnUpdate = FilterSelectedUpdate
    end
    object FilterNone: TAction [21]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1041#1077#1079' '#1092#1080#1083#1100#1090#1088#1072
      Enabled = False
      Hint = #1054#1090#1082#1083#1102#1095#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 17
      ShortCut = 32887
      OnExecute = FilterNoneExecute
      OnUpdate = FilterNoneUpdate
    end
    object SortUser: TAction [22]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072'...'
      Enabled = False
      Hint = #1042#1099#1073#1086#1088' '#1089#1090#1086#1083#1073#1094#1086#1074' '#1076#1083#1103' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 18
      ShortCut = 120
      OnExecute = SortUserExecute
      OnUpdate = SortUserUpdate
    end
    object SortDefault: TAction [23]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      ImageIndex = 18
      ShortCut = 16504
      OnExecute = SortDefaultExecute
      OnUpdate = SortDefaultUpdate
    end
    inherited DbGridSetup: TAction [24]
    end
    inherited ExportToFileCsv: TAction [25]
    end
    inherited DataSetReportList: TAction [26]
    end
    inherited DbGridDefault: TAction [27]
    end
    inherited Refresh: TAction [28]
    end
    inherited CreateDynamicReport: TAction [29]
    end
    inherited ExportToExcel: TAction [30]
    end
    inherited DbLocate: TAction [31]
    end
    inherited MultiSelectOnOff: TAction [32]
    end
    inherited Attachments: TAction [33]
    end
    inherited FindFast: TAction [34]
    end
    inherited DataSetStatistic: TAction
      OnExecute = DataSetStatisticExecute
      OnUpdate = DataSetStatisticUpdate
    end
    inherited ColumnStatistic: TAction
      OnExecute = ColumnStatisticExecute
      OnUpdate = ColumnStatisticUpdate
    end
    object SetCurrOrderAsc: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1087#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102' '#1074' '#1076#1072#1085#1085#1086#1084' '#1089#1090#1086#1083#1073#1094#1077
      OnExecute = SetCurrOrderAscExecute
      OnUpdate = SetCurrOrderAscUpdate
    end
    object SetCurrOrderDesc: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1091#1073#1099#1074#1072#1085#1080#1102
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1087#1086' '#1091#1073#1099#1074#1072#1085#1080#1102' '#1074' '#1076#1072#1085#1085#1086#1084' '#1089#1090#1086#1083#1073#1094#1077
      OnExecute = SetCurrOrderDescExecute
      OnUpdate = SetCurrOrderDescUpdate
    end
  end
  object RDbFilter: TRDbFilter [5]
    AutoActivate = False
    DateFormatWhere = 'mm.dd.yyyy'
    DateFormatFilter = 'dd.MM.yyyy'
    Options = [foStoreItemsState, foChangeOptions, foItemActivateOnChange, foItemsDisabledActive]
    Left = 312
    Top = 88
  end
  object RDbOrder: TRDbOrder [6]
    AutoActivate = False
    Options = [ooStoreItemsState, ooChangeOptions]
    Left = 340
    Top = 88
  end
  inherited RDbGridTuner: TRDbGridTuner
    StoreInIniFile = True
  end
  inherited RDbFilterStatus: TRDbFilterStatus
    RDbFilter = RDbFilter
  end
  inherited PopupMenu: TPopupMenu
    object menuFilterP: TMenuItem [9]
      Caption = #1060#1080#1083#1100#1090#1088' '#1076#1072#1085#1085#1099#1093
      Hint = #1042#1099#1073#1086#1088' '#1088#1077#1078#1080#1084#1072' '#1086#1090#1073#1086#1088#1072' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 15
      object itemFilterUserP: TMenuItem
        Action = FilterUser
      end
      object itemFilterDefaultP: TMenuItem
        Action = FilterDefault
      end
      object itemFilterSelectedP: TMenuItem
        Action = FilterSelected
      end
      object itemFilterNoneP: TMenuItem
        Action = FilterNone
      end
    end
    object menuSortP: TMenuItem [10]
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 18
      object itemSortUserP: TMenuItem
        Action = SortUser
      end
      object itemSortDefaultP: TMenuItem
        Action = SortDefault
      end
    end
  end
  inherited MainMenu: TMainMenu
    inherited menuData: TMenuItem
      object divDataFilter: TMenuItem [7]
        Caption = '-'
      end
      object itemFilterUser: TMenuItem [8]
        Action = FilterUser
      end
      object itemFilterDefault: TMenuItem [9]
        Action = FilterDefault
      end
      object itemFilterSelected: TMenuItem [10]
        Action = FilterSelected
      end
      object itemFilterNone: TMenuItem [11]
        Action = FilterNone
      end
      object divDataSort: TMenuItem [12]
        Caption = '-'
      end
      object itemSortUser: TMenuItem [13]
        Action = SortUser
      end
      object itemSortDefault: TMenuItem [14]
        Action = SortDefault
      end
    end
  end
  inherited DataPopupMenu: TPopupMenu
    object itemFilterUserD: TMenuItem [0]
      Action = FilterUser
    end
    object itemFilterDefaultD: TMenuItem [1]
      Action = FilterDefault
    end
    object itemFilterSelectedD: TMenuItem [2]
      Action = FilterSelected
    end
    object itemFilterNoneD: TMenuItem [3]
      Action = FilterNone
    end
    object divDataPFilter: TMenuItem [4]
      Caption = '-'
    end
    object itemSortUserD: TMenuItem [5]
      Action = SortUser
    end
    object itemSortDefaultD: TMenuItem [6]
      Action = SortDefault
    end
    object divDataPSort: TMenuItem [7]
      Caption = '-'
    end
  end
  inherited RDbEditor: TRDbExportEditor
    OnGetExcelComment = RDbEditorGetExcelComment
  end
  inherited TitleGridPopupMenu: TPopupMenu
    Left = 396
    object itemSetCurrOrderAsc: TMenuItem [2]
      Action = SetCurrOrderAsc
    end
    object itemSetCurrOrderDesc: TMenuItem [3]
      Action = SetCurrOrderDesc
    end
    object itemSortUserT: TMenuItem [4]
      Action = SortUser
    end
    object divGridTitle: TMenuItem [5]
      Caption = '-'
    end
  end
  inherited RDbLocate: TRDbFind
    Left = 368
  end
  inherited RDbUpdater: TRDbUpdater
    Left = 452
  end
  inherited InfoPanelPopupMenu: TPopupMenu
    Left = 424
  end
end
