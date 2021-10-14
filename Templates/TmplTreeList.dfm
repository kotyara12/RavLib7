inherited TreeListTemplate: TTreeListTemplate
  Left = 359
  Top = 255
  Width = 821
  ActiveControl = ListView
  Caption = 'TreeListTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter: TSplitter
    Top = 40
    Height = 413
  end
  inherited StatusBar: TStatusBar
    Width = 805
    Panels = <
      item
        Width = 221
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  inherited CoolBar: TCoolBar
    Width = 805
    Height = 40
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 805
      end>
    inherited ToolBar: TToolBar
      Width = 792
      Height = 36
      ButtonHeight = 36
      ButtonWidth = 57
      object NewToolButton: TToolButton
        Left = 0
        Top = 0
        Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100
        Caption = #1057#1086#1079#1076#1072#1090#1100
        DropdownMenu = NewPopupMenu
        ImageIndex = 8
      end
      object PropertiesToolButton: TToolButton
        Left = 57
        Top = 0
        Action = Properties
      end
      object DeleteToolButton: TToolButton
        Left = 114
        Top = 0
        Action = Delete
      end
      object SeparatorEdit: TToolButton
        Left = 171
        Top = 0
        Width = 8
        Caption = 'SeparatorEdit'
        ImageIndex = 3
        Style = tbsSeparator
      end
      object FindToolButton: TToolButton
        Left = 179
        Top = 0
        Action = Find
      end
      object SeparatorFind: TToolButton
        Left = 236
        Top = 0
        Width = 8
        Caption = 'SeparatorFind'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object DataToolButton: TToolButton
        Left = 244
        Top = 0
        Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1076#1072#1085#1085#1099#1084#1080
        Caption = #1044#1072#1085#1085#1099#1077
        DropdownMenu = DataPopupMenu
        ImageIndex = 14
      end
      object OperToolButton: TToolButton
        Left = 301
        Top = 0
        Hint = #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1076#1072#1085#1085#1099#1093' '#1080' '#1076#1086#1089#1090#1091#1087' '#1082' '#1076#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1086#1081' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1080
        Caption = #1054#1087#1077#1088#1072#1094#1080#1080
        DropdownMenu = OperationsPopupMenu
        ImageIndex = 24
        Visible = False
      end
      object ReportsToolButton: TToolButton
        Left = 358
        Top = 0
        Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1086#1090#1095#1077#1090#1086#1074', '#1087#1077#1095#1072#1090#1100' '#1080' '#1101#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093
        Caption = #1054#1090#1095#1077#1090#1099
        DropdownMenu = ReportsPopupMenu
        ImageIndex = 23
      end
      object ToolsSeparator: TToolButton
        Left = 415
        Top = 0
        Width = 8
        Caption = 'ToolsSeparator'
        ImageIndex = 8
        Style = tbsSeparator
      end
      object RefreshToolButton: TToolButton
        Left = 423
        Top = 0
        Action = Refresh
      end
      object ToolButton13: TToolButton
        Left = 480
        Top = 0
        Width = 8
        Caption = 'ToolButton13'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object CloseSelectToolButton: TToolButton
        Left = 488
        Top = 0
        Action = CloseSelect
      end
      object CloseCancelToolButton: TToolButton
        Left = 545
        Top = 0
        Action = CloseCancel
      end
    end
  end
  inherited TreePanel: TPanel
    Top = 40
    Height = 413
    inherited TreeView: TRTreeView
      Height = 390
    end
  end
  inherited DataPanel: TPanel
    Top = 40
    Width = 581
    Height = 413
    object ListView: TRSortListView
      Left = 0
      Top = 29
      Width = 581
      Height = 384
      Align = alClient
      Columns = <>
      FlatScrollBars = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenu
      SortType = stData
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = ListViewColumnClick
      OnDblClick = ListViewDblClick
      SortDirection = sdAscending
    end
    object FindPanel: TPanel
      Left = 0
      Top = 0
      Width = 581
      Height = 29
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 1
      OnResize = FindPanelResize
      object edFastFind: TEdit
        Left = 0
        Top = 4
        Width = 461
        Height = 21
        Hint = 
          #1042#1074#1077#1076#1080#1090#1077' '#1090#1077#1082#1089#1090' '#1076#1083#1103' '#1087#1086#1080#1089#1082#1072' '#1080' '#1085#1072#1078#1084#1080#1090#1077' [ Enter ] ('#1080#1083#1080' '#1082#1085#1086#1087#1082#1091' "'#1055#1086#1080#1089#1082'"' +
          ')'#13#10#1054#1095#1080#1089#1090#1080#1090#1100' '#1087#1086#1083#1077' - '#1082#1083#1072#1074#1080#1096#1072' [ Esc ]'
        TabOrder = 0
        OnEnter = edFastFindEnter
        OnExit = edFastFindExit
        OnKeyPress = edFastFindKeyPress
      end
      object btnFastFind: TBitBtn
        Left = 468
        Top = 4
        Width = 107
        Height = 21
        Action = FindFast
        Caption = #1055#1086#1080#1089#1082
        TabOrder = 1
        Glyph.Data = {
          36080000424D3608000000000000360000002800000020000000100000000100
          2000000000000008000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0000445A00006F90007FAFC400FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00444444006E6E6E00B1B1B100FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0000445A00006F900000AEDF0063D6F500FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00444444006E6E6E00ABABAB00D4D4D400FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0000445A00006F900000AEDF007FDDF700077FC000FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00444444006E6E6E00ABABAB00DBDBDB0087878700FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000044
          5A00006F900000AEDF009CE5F900077FC000FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF004444
          44006E6E6E00ABABAB00E4E4E40087878700FF00FF00FF00FF00FF00FF00FF00
          FF00F3EEE300CDB48100B48D4300B1862F00BC954700D4BA8500184E5500006F
          900000AEDF00B8ECFB00077FC000FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00EBEBEB00A6A6A600797979006E6E6E0080808000ABABAB004A4A4A006E6E
          6E00ABABAB00EBEBEB0087878700FF00FF00FF00FF00FF00FF00FF00FF00E7DB
          C300B28B3A00D0BB9100E4DED100E9E8E800DFD9CC00D0BA8E00B794430036A7
          B100D5F4FC00077FC000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00D4D4
          D40075757500AFAFAF00DADADA00E8E8E800D5D5D500AEAEAE007D7D7D009E9E
          9E00F3F3F30087878700FF00FF00FF00FF00FF00FF00FF00FF00F4EEE300B28B
          3A00E3DAC700F7F7F700EDEDED00E6E6E600E2E2E200DFDFDF00D6CCB700BE9C
          4A001F83B000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00EBEBEB007575
          7500D5D5D500F7F7F700EDEDED00E6E6E600E2E2E200DFDFDF00C6C6C6008585
          850087878700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CDB38000D1BC
          9100F8F8F800FEFEFE00E9E9E900E2E2E200DFDFDF00DBDBDB00D8D8D800D0B9
          8B00E1C68F00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00A5A5A500B0B0
          B000F8F8F800FEFEFE00E9E9E900E2E2E200DFDFDF00DBDBDB00D8D8D800ACAC
          AC00B7B7B700FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00B7914600E5DF
          D300ECECEC00E9E9E900E2E2E200DFDFDF00DBDBDB00D9D9D900D4D4D400D1CA
          BD00D5AF5C00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF007C7C7C00DCDC
          DC00ECECEC00E9E9E900E2E2E200DFDFDF00DBDBDB00D9D9D900D4D4D400C6C6
          C60098989800FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00B68B3300E9E8
          E800E6E6E600E2E2E200DFDFDF00DBDBDB00EDEDED00FEFEFE00FBFBFB00CFCF
          CF00D7A94A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0073737300E8E8
          E800E6E6E600E2E2E200DFDFDF00DBDBDB00EDEDED00FEFEFE00FBFBFB00CFCF
          CF008F8F8F00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00BE994D00E0DA
          CD00E2E2E200DFDFDF00DBDBDB00EEEEEE00FFFFFF00FFFFFF00FFFFFF00D2CB
          BF00DCB66100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0084848400D6D6
          D600E2E2E200DFDFDF00DBDBDB00EEEEEE00FFFFFF00FFFFFF00FFFFFF00C8C8
          C8009E9E9E00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00D8BE8800D2BC
          8F00DFDFDF00DBDBDB00DADADA00FFFFFF00FFFFFF00FFFFFF00EAEAEA00D4BE
          8E00EBD09600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00AFAFAF00B0B0
          B000DFDFDF00DBDBDB00DADADA00FFFFFF00FFFFFF00FFFFFF00EAEAEA00B1B1
          B100C0C0C000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00F6F1E600C9A0
          4B00D7CDB800D8D8D800D5D5D500FBFBFB00FDFDFD00E8E8E800D1C6B100E0B6
          5800FAF4E800FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00EEEEEE008888
          8800C7C7C700D8D8D800D5D5D500FBFBFB00FDFDFD00E8E8E800C0C0C0009C9C
          9C00F1F1F100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00F0E3
          C900CCA35100D3BD8F00D1CBBD00CDCCCB00D1CABC00D5BF9000E0B75D00F6E9
          CE00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00DCDC
          DC008C8C8C00B0B0B000C7C7C700CCCCCC00C6C6C600B2B2B2009E9E9E00E2E2
          E200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00F9F3E800E6CB9300DBB45E00DCAA5000E2B96300EDD19800FAF5E900FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00F0F0F000BBBBBB009C9C9C0092929200A1A1A100C1C1C100F2F2F200FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        NumGlyphs = 2
      end
    end
  end
  inherited ActionList: TActionList
    inherited Find: TAction
      OnExecute = FindExecute
      OnUpdate = FindUpdate
    end
    object FindFast: TAction [2]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082
      Hint = #1041#1099#1089#1090#1088#1099#1081' '#1087#1086#1080#1089#1082
      ImageIndex = 13
      OnExecute = FindFastExecute
      OnUpdate = FindFastUpdate
    end
    object ListSortAsc: TAction [19]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102
      Enabled = False
      GroupIndex = 10
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1087#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102
      ShortCut = 16449
      OnExecute = ListSortAscExecute
      OnUpdate = ListSortAscUpdate
    end
    object ListSortDesc: TAction [20]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1091#1073#1099#1074#1072#1085#1080#1102
      Enabled = False
      GroupIndex = 10
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1087#1086' '#1091#1073#1099#1074#1072#1085#1080#1102
      ShortCut = 16452
      OnExecute = ListSortDescExecute
      OnUpdate = ListSortDescUpdate
    end
    object ListGrid: TAction [21]
      Category = #1042#1080#1076
      Caption = #1051#1080#1085#1080#1080' '#1089#1077#1090#1082#1080
      Enabled = False
      Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1083#1080#1085#1080#1080' '#1089#1077#1090#1082#1080
      ShortCut = 49228
      OnExecute = ListGridExecute
      OnUpdate = ListGridUpdate
    end
    object ListExportToExcel: TAction [22]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' Excel'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' Microsoft Excel'
      ImageIndex = 22
      ShortCut = 16453
      OnExecute = ListExportToExcelExecute
      OnUpdate = ListExportToExcelUpdate
    end
    object ListExportToFile: TAction [23]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' '#1092#1072#1081#1083
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' '#1092#1072#1081#1083' CSV ('#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083' '#1089' '#1088#1072#1079#1076#1077#1083#1080#1090#1077#1083#1103#1084#1080')'
      ImageIndex = 30
      ShortCut = 16454
      OnExecute = ListExportToFileExecute
      OnUpdate = ListExportToFileUpdate
    end
    object ListCreateDynamicReport: TAction [24]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1055#1077#1095#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
      Enabled = False
      Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1090#1077#1082#1089#1090#1086#1074#1086#1075#1086' '#1086#1090#1095#1077#1090#1072' '#1076#1083#1103' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 23
      ShortCut = 16466
      OnExecute = ListCreateDynamicReportExecute
      OnUpdate = ListCreateDynamicReportUpdate
    end
    object MultiSelectOnOff: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1052#1091#1083#1100#1090#1080#1086#1073#1088#1072#1073#1086#1090#1082#1072
      Enabled = False
      Hint = 
        #1055#1077#1088#1077#1082#1083#1102#1095#1077#1085#1080#1077' '#1088#1077#1078#1080#1084#1072' '#1074#1099#1076#1077#1083#1077#1085#1080#1103' '#1080' '#1086#1073#1088#1072#1073#1086#1090#1082#1080' '#1085#1077#1089#1082#1086#1083#1100#1082#1080#1093' '#1079#1072#1087#1080#1089#1077#1081' '#1086#1076#1085 +
        #1086#1081' '#1082#1086#1084#1072#1085#1076#1086#1081
      ShortCut = 49229
      OnExecute = MultiSelectOnOffExecute
      OnUpdate = MultiSelectOnOffUpdate
    end
    object SelectAll: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077
      Enabled = False
      Hint = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077' '#1089#1090#1088#1086#1082#1080
      ShortCut = 16449
      OnExecute = SelectAllExecute
      OnUpdate = SelectAllUpdate
    end
    object CheckAll: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1054#1090#1084#1077#1090#1080#1090#1100' '#1074#1089#1077
      Enabled = False
      Hint = #1054#1090#1084#1077#1090#1080#1090#1100' '#1074#1089#1077' '#1089#1090#1088#1086#1082#1080
      OnExecute = CheckAllExecute
      OnUpdate = CheckAllUpdate
    end
    object CheckNone: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1085#1103#1090#1100' '#1074#1089#1077
      Enabled = False
      Hint = #1057#1085#1103#1090#1100' '#1086#1090#1084#1077#1090#1082#1080' '#1089#1086' '#1074#1089#1077#1093' '#1089#1090#1088#1086#1082
      OnExecute = CheckNoneExecute
      OnUpdate = CheckNoneUpdate
    end
    object CheckInverse: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1048#1085#1074#1077#1088#1090#1080#1088#1086#1074#1072#1090#1100
      Enabled = False
      Hint = 
        #1048#1085#1074#1077#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1086#1090#1084#1077#1090#1082#1080' ('#1086#1090#1084#1077#1095#1077#1085#1085#1099#1077' - '#1089#1085#1103#1090#1100', '#1085#1077' '#1086#1090#1084#1077#1095#1077#1085#1085#1099#1077' - '#1091#1089#1090#1072#1085 +
        #1086#1074#1080#1090#1100')'
      OnExecute = CheckInverseExecute
      OnUpdate = CheckInverseUpdate
    end
  end
  inherited PopupMenu: TPopupMenu
    inherited menuReportsP: TMenuItem
      object itemListCreateDynamicReportP: TMenuItem
        Action = ListCreateDynamicReport
      end
      object itemListExportToExcelP: TMenuItem
        Action = ListExportToExcel
      end
      object itemListExportToFileP: TMenuItem
        Action = ListExportToFile
      end
    end
    inherited divPopupFolders: TMenuItem [10]
    end
    object divPopupList: TMenuItem [13]
      Caption = '-'
    end
    object menuSortListP: TMenuItem [14]
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1087#1080#1089#1082#1072
      Hint = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1087#1080#1089#1082#1072
      ImageIndex = 18
      object divSortMenuP: TMenuItem
        Caption = '-'
      end
      object itemListSortAscP: TMenuItem
        Action = ListSortAsc
      end
      object itemListSortDescP: TMenuItem
        Action = ListSortDesc
      end
    end
    object itemCheckAllP: TMenuItem [15]
      Action = CheckAll
    end
    object itemCheckNoneP: TMenuItem [16]
      Action = CheckNone
    end
    object itemCheckInverseP: TMenuItem [17]
      Action = CheckInverse
    end
    object divListMultiedit: TMenuItem [18]
      Caption = '-'
    end
    object itemMultiSelectOnOffP: TMenuItem [19]
      Action = MultiSelectOnOff
    end
    object itemSelectAllP: TMenuItem [20]
      Action = SelectAll
    end
    inherited divPopupRefresh: TMenuItem [21]
    end
  end
  inherited MainMenu: TMainMenu
    inherited menuEdit: TMenuItem
      object divEditMulti: TMenuItem [10]
        Caption = '-'
      end
      object itemMultiSelectOnOff: TMenuItem [11]
        Action = MultiSelectOnOff
      end
      object itemSelectAll: TMenuItem [12]
        Action = SelectAll
      end
    end
    inherited menuData: TMenuItem
      object divDataList: TMenuItem
        Caption = '-'
      end
      object menuSortList: TMenuItem
        Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1087#1080#1089#1082#1072
        Hint = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1087#1080#1089#1082#1072
        ImageIndex = 18
        object divSortList: TMenuItem
          Caption = '-'
        end
        object itemListSortAsc: TMenuItem
          Action = ListSortAsc
        end
        object itemListSortDesc: TMenuItem
          Action = ListSortDesc
        end
      end
      object itemCheckAll: TMenuItem
        Action = CheckAll
      end
      object itemCheckNone: TMenuItem
        Action = CheckNone
      end
      object itemCheckInverse: TMenuItem
        Action = CheckInverse
      end
    end
    inherited menuReports: TMenuItem
      object itemListCreateDynamicReport: TMenuItem
        Action = ListCreateDynamicReport
      end
      object itemListExportToExcel: TMenuItem
        Action = ListExportToExcel
      end
      object itemListExportToFile: TMenuItem
        Action = ListExportToFile
      end
    end
  end
  inherited DataPopupMenu: TPopupMenu
    object menuSortListD: TMenuItem [0]
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1087#1080#1089#1082#1072
      Hint = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1087#1080#1089#1082#1072
      ImageIndex = 18
      object divSortListD: TMenuItem
        Caption = '-'
      end
      object itemListSortAscD: TMenuItem
        Action = ListSortAsc
      end
      object itemListSortDescD: TMenuItem
        Action = ListSortDesc
      end
    end
    object divDataTreeD: TMenuItem [1]
      Caption = '-'
    end
    object divDataDCheck: TMenuItem
      Caption = '-'
    end
    object itemCheckAllD: TMenuItem
      Action = CheckAll
    end
    object itemCheckNoneD: TMenuItem
      Action = CheckNone
    end
    object itemCheckInverseD: TMenuItem
      Action = CheckInverse
    end
  end
  inherited ReportsPopupMenu: TPopupMenu
    object itemListCreateDynamicReportR: TMenuItem
      Action = ListCreateDynamicReport
    end
    object itemListExportToExcelR: TMenuItem
      Action = ListExportToExcel
    end
    object itemListExportToFileR: TMenuItem
      Action = ListExportToFile
    end
  end
end
