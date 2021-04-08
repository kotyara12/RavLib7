inherited QueryTemplate: TQueryTemplate
  Left = 460
  Top = 399
  Width = 872
  Height = 516
  Caption = 'QueryTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Top = 438
    Width = 856
  end
  inherited CoolBar: TCoolBar
    Width = 856
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 856
      end>
    inherited ToolBar: TToolBar
      Width = 843
      ButtonWidth = 57
      object DataSetFirstToolButton: TToolButton
        Left = 0
        Top = 0
        Action = DataSetFirst
      end
      object DataSetPriorToolButton: TToolButton
        Left = 57
        Top = 0
        Action = DataSetPrior
      end
      object DataSetNextToolButton: TToolButton
        Left = 114
        Top = 0
        Action = DataSetNext
      end
      object DataSetLastToolButton: TToolButton
        Left = 171
        Top = 0
        Action = DataSetLast
      end
      object SeparatorNav: TToolButton
        Left = 228
        Top = 0
        Width = 8
        Caption = 'SeparatorNav'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object DataSetInsertToolButton: TToolButton
        Left = 236
        Top = 0
        Action = DataSetInsert
      end
      object DataSetEditToolButton: TToolButton
        Left = 293
        Top = 0
        Action = DataSetEdit
      end
      object DataSetDeleteToolButton: TToolButton
        Left = 350
        Top = 0
        Action = DataSetDelete
      end
      object SeparatorEdit: TToolButton
        Left = 407
        Top = 0
        Width = 8
        Caption = 'SeparatorEdit'
        ImageIndex = 8
        Style = tbsSeparator
      end
      object DataSetFindToolButton: TToolButton
        Left = 415
        Top = 0
        Action = Find
      end
      object SeparatorFind: TToolButton
        Left = 472
        Top = 0
        Width = 8
        Caption = 'SeparatorFind'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object DataToolButton: TToolButton
        Left = 480
        Top = 0
        Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1076#1072#1085#1085#1099#1084#1080
        Caption = #1044#1072#1085#1085#1099#1077
        DropdownMenu = DataPopupMenu
        ImageIndex = 14
      end
      object OpersToolButton: TToolButton
        Left = 537
        Top = 0
        Hint = #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1076#1072#1085#1085#1099#1093
        Caption = #1054#1087#1077#1088#1072#1094#1080#1080
        DropdownMenu = OperationsPopupMenu
        ImageIndex = 24
        Visible = False
      end
      object ReportsToolButton: TToolButton
        Left = 594
        Top = 0
        Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1086#1090#1095#1077#1090#1086#1074', '#1087#1077#1095#1072#1090#1100' '#1080' '#1101#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093
        Caption = #1054#1090#1095#1077#1090#1099
        DropdownMenu = ReportsPopupMenu
        ImageIndex = 23
      end
      object SeparatorRefresh: TToolButton
        Left = 651
        Top = 0
        Width = 8
        Caption = 'SeparatorRefresh'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object RefreshToolButton: TToolButton
        Left = 659
        Top = 0
        Action = Refresh
      end
      object SeparatorEnd: TToolButton
        Left = 716
        Top = 0
        Width = 8
        Caption = 'SeparatorEnd'
        ImageIndex = 10
        Style = tbsSeparator
      end
      object CloseSelectToolButton: TToolButton
        Left = 724
        Top = 0
        Action = CloseSelect
      end
      object CloseCancelToolButton: TToolButton
        Left = 781
        Top = 0
        Action = CloseCancel
      end
    end
  end
  inherited DataPanel: TPanel
    Width = 856
    Height = 369
    inherited DbGrid: TRDbStyledGrid
      Width = 856
      Height = 272
    end
    inherited InfoPanel: TRDbInfoPanel
      Top = 293
      Width = 856
    end
    inherited TabViews: TTabSet
      Top = 272
      Width = 856
    end
  end
  inherited FindPanel: TPanel
    Width = 856
  end
  inherited TitleGridPopupMenu: TPopupMenu
    inherited divGridTitleFind: TMenuItem [2]
    end
    inherited itemSetCurrOrderAsc: TMenuItem [3]
    end
    inherited itemSetCurrOrderDesc: TMenuItem [4]
    end
    inherited itemSortUserT: TMenuItem [5]
    end
    inherited divGridTitle: TMenuItem [6]
    end
  end
end
