inherited DbTemplate: TDbTemplate
  Left = 380
  Top = 187
  Width = 869
  Height = 510
  Caption = 'DbTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Top = 432
    Width = 853
  end
  inherited CoolBar: TCoolBar
    Width = 853
    Height = 40
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 853
      end>
    inherited ToolBar: TToolBar
      Width = 840
      Height = 36
      ButtonWidth = 58
      object DataSetInsertToolButton: TToolButton
        Left = 0
        Top = 0
        Action = DataSetInsert
      end
      object DataSetEditToolButton: TToolButton
        Left = 58
        Top = 0
        Action = DataSetEdit
      end
      object DataSetDeleteToolButton: TToolButton
        Left = 116
        Top = 0
        Action = DataSetDelete
      end
      object SeparatorEdit: TToolButton
        Left = 174
        Top = 0
        Width = 8
        Caption = 'SeparatorEdit'
        ImageIndex = 8
        Style = tbsSeparator
      end
      object DataSetFindToolButton: TToolButton
        Left = 182
        Top = 0
        Action = Find
      end
      object SeparatorFind: TToolButton
        Left = 240
        Top = 0
        Width = 8
        Caption = 'SeparatorFind'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object DataToolButton: TToolButton
        Left = 248
        Top = 0
        Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1076#1072#1085#1085#1099#1084#1080
        Caption = #1044#1072#1085#1085#1099#1077
        DropdownMenu = DataPopupMenu
        ImageIndex = 14
      end
      object OpersToolButton: TToolButton
        Left = 306
        Top = 0
        Hint = #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1076#1072#1085#1085#1099#1093
        Caption = #1054#1087#1077#1088#1072#1094#1080#1080
        DropdownMenu = OperationsPopupMenu
        ImageIndex = 24
        Visible = False
      end
      object ReportsToolButton: TToolButton
        Left = 364
        Top = 0
        Hint = #1055#1077#1095#1072#1090#1100' '#1080' '#1101#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093
        Caption = #1054#1090#1095#1077#1090#1099
        DropdownMenu = ReportsPopupMenu
        ImageIndex = 23
      end
      object SeparatorRefresh: TToolButton
        Left = 422
        Top = 0
        Width = 8
        Caption = 'SeparatorRefresh'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object RefreshToolButton: TToolButton
        Left = 430
        Top = 0
        Action = Refresh
      end
      object SeparatorEnd: TToolButton
        Left = 488
        Top = 0
        Width = 8
        Caption = 'SeparatorEnd'
        ImageIndex = 10
        Style = tbsSeparator
      end
      object CloseSelectToolButton: TToolButton
        Left = 496
        Top = 0
        Action = CloseSelect
      end
      object CloseCancelToolButton: TToolButton
        Left = 554
        Top = 0
        Action = CloseCancel
      end
    end
  end
  inherited DataPanel: TPanel
    Top = 69
    Width = 853
    Height = 363
    inherited DbGrid: TRDbStyledGrid
      Width = 853
      Height = 298
    end
    inherited InfoPanel: TRDbInfoPanel
      Top = 319
      Width = 853
    end
    inherited TabViews: TTabSet
      Top = 298
      Width = 853
    end
  end
  inherited FindPanel: TPanel
    Top = 40
    Width = 853
  end
  inherited ActionList: TActionList
    inherited DataSetStatistic: TAction
      OnExecute = DataSetStatisticExecute
      OnUpdate = DataSetStatisticUpdate
    end
    inherited ColumnStatistic: TAction
      OnExecute = ColumnStatisticExecute
      OnUpdate = ColumnStatisticUpdate
    end
  end
end
