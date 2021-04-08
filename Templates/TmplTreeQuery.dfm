inherited TreeQueryTemplate: TTreeQueryTemplate
  Left = 531
  Top = 397
  Width = 867
  Height = 538
  ActiveControl = DbGrid
  Caption = 'TreeQueryTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  inherited Splitter: TSplitter
    Top = 40
    Height = 420
  end
  inherited StatusBar: TStatusBar
    Top = 460
    Width = 851
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
    SimplePanel = False
    SimpleText = #1053#1077#1090' '#1076#1072#1085#1085#1099#1093'; '#1056#1077#1078#1080#1084': "'#1053#1072#1073#1086#1088' '#1076#1072#1085#1085#1099#1093' '#1079#1072#1082#1088#1099#1090'"'
  end
  inherited CoolBar: TCoolBar
    Width = 851
    Height = 40
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 851
      end>
    inherited ToolBar: TToolBar
      Width = 838
      Height = 36
      ButtonHeight = 36
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
      object NewToolButton: TToolButton
        Left = 236
        Top = 0
        Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100
        Caption = #1057#1086#1079#1076#1072#1090#1100
        DropdownMenu = NewPopupMenu
        ImageIndex = 8
        PopupMenu = NewPopupMenu
      end
      object PropertiesToolButton: TToolButton
        Left = 293
        Top = 0
        Action = Properties
      end
      object DeleteItemToolButton: TToolButton
        Left = 350
        Top = 0
        Action = Delete
      end
      object SeparatorEdit: TToolButton
        Left = 407
        Top = 0
        Width = 8
        Caption = 'SeparatorEdit'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object FindRecordToolButton: TToolButton
        Left = 415
        Top = 0
        Action = Find
      end
      object SeparatorFind: TToolButton
        Left = 472
        Top = 0
        Width = 8
        Caption = 'SeparatorFind'
        ImageIndex = 13
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
        Hint = #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1076#1072#1085#1085#1099#1093' '#1080' '#1076#1086#1089#1090#1091#1087' '#1082' '#1076#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1086#1081' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1080
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
        ImageIndex = 2
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
  inherited TreePanel: TPanel
    Top = 40
    Height = 420
    inherited TreeView: TRTreeView
      Height = 397
    end
  end
  inherited DataPanel: TPanel
    Top = 40
    Width = 627
    Height = 420
    object InfoPanel: TRDbInfoPanel
      Left = 0
      Top = 379
      Width = 627
      Height = 41
      Align = alBottom
      BevelOuter = bvNone
      BorderStyle = bsSingle
      PopupMenu = InfoPanelPopupMenu
      TabOrder = 2
    end
    object DbGrid: TRDbStyledGrid
      Left = 0
      Top = 29
      Width = 627
      Height = 329
      Align = alClient
      DataSource = RDbEditor
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
      PopupMenu = PopupMenu
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnColumnMoved = DbGridColumnMoved
      OnDblClick = DbGridDblClick
      OnMouseUp = DbGridMouseUp
      Images = BaseData.ImageList
      AttachImage = 38
      AttachField = 'ATTACHS_CNT'
      FieldFontStyle = 'FONT_STYLE'
      FieldFontColor = 'FONT_COLOR'
      FieldCellColor = 'CELL_COLOR'
    end
    object FindPanel: TPanel
      Left = 0
      Top = 0
      Width = 627
      Height = 29
      Align = alTop
      BevelOuter = bvNone
      ParentBackground = False
      ParentColor = True
      TabOrder = 0
      OnResize = FindPanelResize
      object edFastFind: TEdit
        Left = 0
        Top = 4
        Width = 509
        Height = 21
        Hint = 
          #1042#1074#1077#1076#1080#1090#1077' '#1090#1077#1082#1089#1090' '#1076#1083#1103' '#1087#1086#1080#1089#1082#1072' '#1080' '#1085#1072#1078#1084#1080#1090#1077' [ Enter ] ('#1080#1083#1080' '#1082#1085#1086#1087#1082#1091' "'#1055#1086#1080#1089#1082'"' +
          ')'#13#10#1054#1090#1084#1077#1085#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' - '#1082#1083#1072#1074#1080#1096#1072' [ Esc ] '#1080#1083#1080' '#1086#1095#1080#1089#1090#1080#1090#1100' '#1087#1086#1083#1077' '#1080' [ Enter' +
          ' ]'
        TabOrder = 0
        OnEnter = edFastFindEnter
        OnExit = edFastFindExit
        OnKeyPress = edFastFindKeyPress
      end
      object btnFastFind: TBitBtn
        Left = 516
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
    object TabViews: TTabSet
      Left = 0
      Top = 358
      Width = 627
      Height = 21
      Align = alBottom
      DitherBackground = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentBackground = True
      PopupMenu = ViewsPopupMenu
      SelectedColor = clHighlight
      SoftTop = True
      Tabs.Strings = (
        #1055#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102)
      TabIndex = 0
      OnChange = TabViewsChange
    end
  end
  inherited ActionList: TActionList
    object DataSetReportList: TAction [0]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1053#1072#1089#1090#1088#1072#1080#1074#1072#1077#1084#1099#1077' '#1086#1090#1095#1077#1090#1099
      Enabled = False
      Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1089#1087#1080#1089#1082#1086#1084' '#1086#1090#1095#1077#1090#1086#1074
      ImageIndex = 21
      ShortCut = 16464
      OnExecute = DataSetReportListExecute
      OnUpdate = DataSetReportListUpdate
    end
    inherited Find: TAction
      OnExecute = FindExecute
      OnUpdate = FindUpdate
    end
    object FindColumn: TAction [5]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082'...'
      Enabled = False
      Hint = #1055#1086#1080#1089#1082' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1089#1090#1086#1083#1073#1094#1091
      ImageIndex = 13
      OnExecute = FindColumnExecute
      OnUpdate = FindColumnUpdate
    end
    object DbLocate: TAction [6]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082'...'
      Enabled = False
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1091#1082#1072#1079#1072#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 13
      ShortCut = 16455
      OnExecute = DbLocateExecute
      OnUpdate = DbLocateUpdate
    end
    object FindFast: TAction [7]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082
      Hint = #1041#1099#1089#1090#1088#1099#1081' '#1087#1086#1080#1089#1082
      ImageIndex = 13
      OnExecute = FindFastExecute
      OnUpdate = FindFastUpdate
    end
    object DataSetFirst: TDataSetFirst [8]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042' '#1085#1072#1095#1072#1083#1086
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1085#1072#1095#1072#1083#1086' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 4
      ShortCut = 36
      DataSource = RDbEditor
    end
    object DataSetPrior: TDataSetPrior [9]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1053#1072#1079#1072#1076
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1085#1072' '#1087#1088#1077#1076#1099#1076#1091#1097#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 5
      DataSource = RDbEditor
    end
    object DataSetNext: TDataSetNext [10]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042#1087#1077#1088#1077#1076
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1085#1072' '#1089#1083#1077#1076#1091#1102#1097#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 6
      DataSource = RDbEditor
    end
    object DataSetLast: TDataSetLast [11]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042' '#1082#1086#1085#1077#1094
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1082#1086#1085#1077#1094' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 7
      ShortCut = 35
      DataSource = RDbEditor
    end
    object DbGridSetup: TAction [12]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1080#1103'...'
      Enabled = False
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1089#1090#1086#1083#1073#1094#1086#1074' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 19
      OnExecute = DbGridSetupExecute
      OnUpdate = DbGridSetupUpdate
    end
    object DbGridDefault: TAction [13]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1090#1086#1083#1073#1094#1099' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1089#1090#1086#1083#1073#1094#1099' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'" ('#1074#1089#1077' '#1074#1080#1076#1080#1084#1099#1077')'
      ImageIndex = 20
      OnExecute = DbGridDefaultExecute
      OnUpdate = DbGridDefaultUpdate
    end
    object ColumnLeft: TAction [14]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1083#1077#1074#1086#1084#1091' '#1082#1088#1072#1102
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1083#1077#1074#1086#1084#1091' '#1082#1088#1072#1102
      OnExecute = ColumnLeftExecute
      OnUpdate = ColumnLeftUpdate
    end
    object ColumnCenter: TAction [15]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1094#1077#1085#1090#1088#1091
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1094#1077#1085#1090#1088#1091
      OnExecute = ColumnCenterExecute
      OnUpdate = ColumnCenterUpdate
    end
    object ColumnRight: TAction [16]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1087#1088#1072#1074#1086#1084#1091' '#1082#1088#1072#1102
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1087#1088#1072#1074#1086#1084#1091' '#1082#1088#1072#1102
      OnExecute = ColumnRightExecute
      OnUpdate = ColumnRightUpdate
    end
    inherited NewGroup: TAction [17]
    end
    inherited NewSubGroup: TAction [18]
    end
    inherited NewItem: TAction [19]
    end
    inherited NewRecord: TAction [20]
      OnExecute = NewRecordExecute
      OnUpdate = NewRecordUpdate
    end
    inherited Properties: TAction [21]
    end
    object FilterUser: TAction [22]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103'...'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1087#1088#1086#1080#1079#1074#1086#1083#1100#1085#1086#1075#1086' '#1092#1080#1083#1100#1090#1088#1072' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 15
      ShortCut = 119
      OnExecute = FilterUserExecute
      OnUpdate = FilterUserUpdate
    end
    object FilterDefault: TAction [23]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1092#1080#1083#1100#1090#1088#1072' '#1076#1072#1085#1085#1099#1093' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      ImageIndex = 16
      ShortCut = 16503
      OnExecute = FilterDefaultExecute
      OnUpdate = FilterDefaultUpdate
    end
    object FilterSelected: TAction [24]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' "'#1087#1086' '#1074#1099#1076#1077#1083#1077#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' '#1087#1086' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1084' '#1079#1072#1087#1080#1089#1103#1084
      ImageIndex = 16
      ShortCut = 49271
      OnExecute = FilterSelectedExecute
      OnUpdate = FilterSelectedUpdate
    end
    object FilterNone: TAction [25]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1041#1077#1079' '#1092#1080#1083#1100#1090#1088#1072
      Enabled = False
      Hint = #1054#1090#1082#1083#1102#1095#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 17
      ShortCut = 32887
      OnExecute = FilterNoneExecute
      OnUpdate = FilterNoneUpdate
    end
    object SortUser: TAction [26]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072'...'
      Enabled = False
      Hint = #1042#1099#1073#1086#1088' '#1089#1090#1086#1083#1073#1094#1086#1074' '#1076#1083#1103' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 18
      ShortCut = 120
      OnExecute = SortUserExecute
      OnUpdate = SortUserUpdate
    end
    object SortDefault: TAction [27]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      ImageIndex = 18
      ShortCut = 16504
      OnExecute = SortDefaultExecute
      OnUpdate = SortDefaultUpdate
    end
    object SetCurrOrderAsc: TAction [28]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1087#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102' '#1074' '#1076#1072#1085#1085#1086#1084' '#1089#1090#1086#1083#1073#1094#1077
      OnExecute = SetCurrOrderAscExecute
      OnUpdate = SetCurrOrderAscUpdate
    end
    object SetCurrOrderDesc: TAction [29]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1091#1073#1099#1074#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1087#1086' '#1091#1073#1099#1074#1072#1085#1080#1102' '#1074' '#1076#1072#1085#1085#1086#1084' '#1089#1090#1086#1083#1073#1094#1077
      OnExecute = SetCurrOrderDescExecute
      OnUpdate = SetCurrOrderDescUpdate
    end
    inherited Move: TAction [30]
    end
    inherited TreeSort_None: TAction [31]
    end
    inherited AboutBox: TAction [32]
    end
    inherited TreeSort_Id: TAction [33]
    end
    inherited TreeSort_TypeId: TAction [34]
    end
    inherited TreeSort_Name: TAction [35]
    end
    inherited TreeSort_TypeName: TAction [36]
    end
    inherited EM_None: TAction [37]
    end
    inherited EM_Root: TAction [38]
    end
    object ImportDS: TAction [39]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072'...'
      Enabled = False
      Hint = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072' '#1076#1072#1085#1085#1099#1093' '#1080#1079' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
      ImageIndex = 31
      OnExecute = ImportDSExecute
      OnUpdate = ImportDSUpdate
    end
    inherited TreeVisible: TAction [40]
    end
    inherited EM_Groups: TAction [41]
    end
    inherited EM_All: TAction [42]
    end
    inherited SelectSaveEM: TAction [43]
    end
    inherited SaveTreePosition: TAction [44]
    end
    inherited ExpandAll: TAction [45]
    end
    inherited CollapseAll: TAction [46]
    end
    inherited ShowHelp: TAction [47]
    end
    inherited CopyRecord: TAction [48]
    end
    inherited TreeSort_IndexSort: TAction [49]
    end
    inherited ExpandNode: TAction [50]
    end
    object DataSetExportToExcel: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' Excel'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' Microsoft Excel'
      ImageIndex = 22
      ShortCut = 16453
      OnExecute = DataSetExportToExcelExecute
      OnUpdate = DataSetExportToExcelUpdate
    end
    object DataSetExportToFileCsv: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' '#1092#1072#1081#1083' CSV'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' '#1092#1072#1081#1083' CSV ('#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083' '#1089' '#1088#1072#1079#1076#1077#1083#1080#1090#1077#1083#1103#1084#1080')'
      ImageIndex = 30
      ShortCut = 16467
      OnExecute = DataSetExportToFileCsvExecute
      OnUpdate = DataSetExportToFileCsvUpdate
    end
    object DataSetCreateDynamicReport: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1055#1077#1095#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
      Enabled = False
      Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1090#1077#1082#1089#1090#1086#1074#1086#1075#1086' '#1086#1090#1095#1077#1090#1072' '#1076#1083#1103' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 23
      ShortCut = 16466
      OnExecute = DataSetCreateDynamicReportExecute
      OnUpdate = DataSetCreateDynamicReportUpdate
    end
    object DataSetStatistic: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1079#1072#1087#1080#1089#1077#1081', '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1099#1084' '#1087#1086#1083#1103#1084
      ImageIndex = 45
      ShortCut = 16469
      OnExecute = DataSetStatisticExecute
      OnUpdate = DataSetStatisticUpdate
    end
    object Attachments: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1055#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1077' '#1092#1072#1081#1083#1099
      Enabled = False
      Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1087#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1084#1080' '#1092#1072#1081#1083#1072#1084#1080
      ImageIndex = 38
      ShortCut = 123
      Visible = False
      OnExecute = AttachmentsExecute
      OnUpdate = AttachmentsUpdate
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
    object ColumnStatistic: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1079#1072#1087#1080#1089#1077#1081', '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1087#1086#1083#1102
      ImageIndex = 45
      OnExecute = ColumnStatisticExecute
      OnUpdate = ColumnStatisticUpdate
    end
    object SelectAll: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077
      Enabled = False
      Hint = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077' '#1079#1072#1087#1080#1089#1080' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ShortCut = 16449
      OnExecute = SelectAllExecute
      OnUpdate = SelectAllUpdate
    end
    object SelectDefaultValues: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1047#1085#1072#1095#1077#1085#1080#1103' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = 
        #1042#1099#1073#1088#1072#1090#1100' '#1079#1085#1072#1095#1077#1085#1080#1103' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'" '#1076#1083#1103' '#1085#1086#1074#1099#1093' '#1079#1072#1087#1080#1089#1077#1081' '#1074' '#1090#1077#1082#1091#1097#1077#1084' '#1089#1077#1072#1085 +
        #1089#1077' '#1088#1072#1073#1086#1090#1099
      ShortCut = 49220
      OnExecute = SelectDefaultValuesExecute
      OnUpdate = SelectDefaultValuesUpdate
    end
    object DateSetGrouping: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1043#1088#1091#1087#1087#1080#1088#1086#1074#1082#1072
      Enabled = False
      Hint = 
        #1043#1088#1091#1087#1087#1080#1088#1086#1074#1082#1072' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1099#1084' '#1087#1086#1083#1103#1084' '#1089' '#1074#1086#1079#1084#1086#1078#1085#1086#1089#1090#1100#1102' '#1087#1086#1076#1089#1095#1077#1090#1072' '#1087#1086' '#1076#1088#1091#1075#1080#1084 +
        ' '#1087#1086#1083#1103#1084
      ImageIndex = 45
      ShortCut = 16473
      OnExecute = DateSetGroupingExecute
      OnUpdate = DateSetGroupingUpdate
    end
  end
  inherited PopupMenu: TPopupMenu
    AutoPopup = False
    object itemDataSetFirstP: TMenuItem [0]
      Action = DataSetFirst
    end
    object itemDataSetPriorP: TMenuItem [1]
      Action = DataSetPrior
    end
    object itemDataSetNextP: TMenuItem [2]
      Action = DataSetNext
    end
    object itemDataSetLastP: TMenuItem [3]
      Action = DataSetLast
    end
    object divPopupNav: TMenuItem [4]
      Caption = '-'
    end
    inherited itemPropertiesP: TMenuItem
      Default = True
    end
    object itemDbLocateP: TMenuItem [13]
      Action = DbLocate
    end
    inherited divPopupSubmenus: TMenuItem
      Visible = True
    end
    object itemAttachmentsP: TMenuItem [15]
      Action = Attachments
    end
    object divAttachP: TMenuItem [16]
      Caption = '-'
      Visible = False
    end
    inherited menuDataP: TMenuItem
      Visible = True
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
      object divPopupFilter: TMenuItem
        Caption = '-'
      end
      object itemSortUserP: TMenuItem
        Action = SortUser
      end
      object itemSortDefaultP: TMenuItem
        Action = SortDefault
      end
      object divPopupSort: TMenuItem
        Caption = '-'
      end
      object itemDbGridSetupP: TMenuItem
        Action = DbGridSetup
      end
      object itemDbGridDefaultP: TMenuItem
        Action = DbGridDefault
      end
    end
    inherited menuReportsP: TMenuItem
      Visible = True
      object itemDataSetReportListP: TMenuItem
        Action = DataSetReportList
      end
      object divRepExpP: TMenuItem
        Caption = '-'
      end
      object itemDataSetCreateDynamicReportP: TMenuItem
        Action = DataSetCreateDynamicReport
      end
      object itemDataSetExportToExcelP: TMenuItem
        Action = DataSetExportToExcel
      end
      object itemDataSetExportToFileP: TMenuItem
        Action = DataSetExportToFileCsv
      end
      object divRepStatP: TMenuItem
        Caption = '-'
      end
      object itemDateSetGroupingP: TMenuItem
        Action = DateSetGrouping
      end
      object itemStatisticP: TMenuItem
        Action = DataSetStatistic
      end
    end
    object itemSelectDefaultValuesP: TMenuItem [21]
      Action = SelectDefaultValues
    end
    object itemMultiSelectOnOffP: TMenuItem [22]
      Action = MultiSelectOnOff
    end
    object itemSelectAllP: TMenuItem [23]
      Action = SelectAll
    end
  end
  inherited MainMenu: TMainMenu
    inherited menuEdit: TMenuItem
      object itemAttachments: TMenuItem [11]
        Action = Attachments
      end
      object divAttach: TMenuItem [12]
        Caption = '-'
        Visible = False
      end
      object itemImportDS: TMenuItem [13]
        Action = ImportDS
      end
      object divEditMulti: TMenuItem [14]
        Caption = '-'
        Visible = False
      end
      object itemSelectDefaultValues: TMenuItem [15]
        Action = SelectDefaultValues
      end
      object itemMultiSelectOnOff: TMenuItem [16]
        Action = MultiSelectOnOff
      end
      object itemSelectAll: TMenuItem [17]
        Action = SelectAll
      end
      object divEditImport: TMenuItem [18]
        Caption = '-'
      end
    end
    inherited menuData: TMenuItem
      object itemDataSetFirst: TMenuItem [0]
        Action = DataSetFirst
      end
      object itemDataSetPrior: TMenuItem [1]
        Action = DataSetPrior
      end
      object itemDataSetNext: TMenuItem [2]
        Action = DataSetNext
      end
      object itemDataSetLast: TMenuItem [3]
        Action = DataSetLast
      end
      inherited divDataFind: TMenuItem [4]
      end
      inherited itemFind: TMenuItem [5]
      end
      inherited itemFindTree: TMenuItem [6]
      end
      object itemDbLocate: TMenuItem [7]
        Action = DbLocate
      end
      object divDataFilter: TMenuItem [8]
        Caption = '-'
      end
      object itemFilterUser: TMenuItem [9]
        Action = FilterUser
      end
      object itemFilterDefault: TMenuItem [10]
        Action = FilterDefault
      end
      object itemFilterSelected: TMenuItem [11]
        Action = FilterSelected
      end
      object itemFilterNone: TMenuItem [12]
        Action = FilterNone
      end
      object divDataSort: TMenuItem [13]
        Caption = '-'
      end
      object itemSortUser: TMenuItem [14]
        Action = SortUser
      end
      object itemSortDefault: TMenuItem [15]
        Action = SortDefault
      end
      object divDataGrid: TMenuItem [16]
        Caption = '-'
      end
      object itemDbGridSetup: TMenuItem [17]
        Action = DbGridSetup
      end
      object itemDbGridDefault: TMenuItem [18]
        Action = DbGridDefault
      end
      object divDataTree: TMenuItem [19]
        Caption = '-'
      end
    end
    inherited menuReports: TMenuItem
      object itemDataSetReportList: TMenuItem
        Action = DataSetReportList
      end
      object divRepExp: TMenuItem
        Caption = '-'
      end
      object itemDataSetCreateDynamicReport: TMenuItem
        Action = DataSetCreateDynamicReport
      end
      object itemDataSetExportToExcel: TMenuItem
        Action = DataSetExportToExcel
      end
      object itemDataSetExportToFile: TMenuItem
        Action = DataSetExportToFileCsv
      end
      object divRepStat: TMenuItem
        Caption = '-'
      end
      object itemDateSetGrouping: TMenuItem
        Action = DateSetGrouping
      end
      object itemStatistic: TMenuItem
        Action = DataSetStatistic
      end
    end
  end
  object RDbFind: TRDbSearch [9]
    AutoActivate = False
    Options = [olUseHistory, olStoreLastFind, olStoreHistory]
    Left = 312
    Top = 104
  end
  object RDbFilter: TRDbFilter [10]
    AutoActivate = False
    AddBrackets = True
    DateFormatWhere = 'mm.dd.yyyy'
    DateFormatFilter = 'dd.MM.yyyy'
    Options = [foStoreItemsState, foChangeOptions, foItemActivateOnChange]
    Left = 340
    Top = 104
  end
  object RDbOrder: TRDbOrder [11]
    AutoActivate = False
    Options = [ooStoreItemsState, ooChangeOptions]
    Left = 368
    Top = 104
  end
  object RDbGridTuner: TRDbGridTuner [12]
    AutoActivate = False
    DbGrid = DbGrid
    Options = [ogStoreItemsState, ogChangeOptions]
    OnViewChange = RDbGridTunerViewChange
    Left = 396
    Top = 104
  end
  object RDbFilterStatus: TRDbFilterStatus [13]
    DataSource = RDbEditor
    StatusBar = StatusBar
    Options = [soRecordCount, soRecordNum, soShowCaptions]
    RDbFilter = RDbFilter
    Left = 424
    Top = 104
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
    object divDataDFilter: TMenuItem [4]
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
    object itemDbGridSetupD: TMenuItem [8]
      Action = DbGridSetup
    end
    object itemDbGridDefaultD: TMenuItem [9]
      Action = DbGridDefault
    end
    object divDataTreeD: TMenuItem [10]
      Caption = '-'
    end
  end
  inherited ReportsPopupMenu: TPopupMenu
    object itemDataSetReportListR: TMenuItem
      Action = DataSetReportList
    end
    object divRepExpR: TMenuItem
      Caption = '-'
    end
    object itemDataSetCreateDynamicReportR: TMenuItem
      Action = DataSetCreateDynamicReport
    end
    object itemDataSetExportToExcelR: TMenuItem
      Action = DataSetExportToExcel
    end
    object itemDataSetExportToFileR: TMenuItem
      Action = DataSetExportToFileCsv
    end
    object divRepStatR: TMenuItem
      Caption = '-'
    end
    object itemDateSetGroupingR: TMenuItem
      Action = DateSetGrouping
    end
    object itemStatisticR: TMenuItem
      Action = DataSetStatistic
    end
  end
  object RDbEditor: TRDbExportEditor
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    OwnerFieldName = 'id_groups'
    LogEnable = True
    OpenMode = omAuto
    OnAfterProcessRecord = DetailAfterProcessRecord
    OnBeforeShowEditor = DetailBeforeShowEditor
    OnGetNewKey = GetNewKey
    OnFreeNewKey = FreeNewKey
    OnCreateSetDefault = RDbEditorCreateSetDefault
    OnAfterPostLogged = DetailAfterPostLogged
    OnBeforeDelete = DetailBeforeDelete
    OnSaveToLog = SaveToLog
    DbGrid = DbGrid
    OnAfterProcessRecords = DetailAfterProcessRecords
    OnGetExcelCopyright = DetailGetExcelCopyright
    OnGetExcelCaption = DetailGetExcelCaption
    OnGetExcelComment = DetailGetExcelComment
    Left = 284
    Top = 104
  end
  object TitleGridPopupMenu: TPopupMenu
    AutoPopup = False
    Images = BaseData.ImageList
    Left = 180
    Top = 64
    object itemFindColumn: TMenuItem
      Action = FindColumn
    end
    object itemDbLocateT: TMenuItem
      Action = DbLocate
    end
    object divMenuTitle1: TMenuItem
      Caption = '-'
    end
    object itemSetCurrOrderAsc: TMenuItem
      Action = SetCurrOrderAsc
    end
    object itemSetCurrOrderDesc: TMenuItem
      Action = SetCurrOrderDesc
    end
    object itemSortUserT: TMenuItem
      Action = SortUser
    end
    object divMenuTitle2: TMenuItem
      Caption = '-'
    end
    object menuAlgn: TMenuItem
      Caption = #1042#1099#1088#1072#1074#1085#1080#1074#1072#1085#1080#1077
      ImageIndex = 14
      object itemColumnLeft: TMenuItem
        Action = ColumnLeft
      end
      object itemColumnCenter: TMenuItem
        Action = ColumnCenter
      end
      object itemColumnRight: TMenuItem
        Action = ColumnRight
      end
    end
    object itemDbGridSetupT: TMenuItem
      Action = DbGridSetup
    end
    object itemDbGridDefaultT: TMenuItem
      Action = DbGridDefault
    end
    object divMenuTitle3: TMenuItem
      Caption = '-'
    end
    object itemDateSetGroupingT: TMenuItem
      Action = DateSetGrouping
    end
    object itemColumnStatisticT: TMenuItem
      Action = ColumnStatistic
    end
  end
  object RDbLocate: TRDbFind
    AutoActivate = False
    Options = [olUseHistory, olStoreLastFind, olStoreHistory]
    Left = 452
    Top = 104
  end
  object RDbUpdater: TRDbUpdater
    Editor = RDbEditor
    Left = 480
    Top = 104
  end
  object InfoPanelPopupMenu: TPopupMenu
    Images = BaseData.ImageList
    OnPopup = InfoPanelPopupMenuPopup
    Left = 508
    Top = 104
    object itemCopyInfoPanel: TMenuItem
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      Hint = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074' '#1073#1091#1092#1077#1088' '#1086#1073#1084#1077#1085#1072
      ImageIndex = 42
      OnClick = itemCopyInfoPanelClick
    end
  end
  object ViewsPopupMenu: TPopupMenu
    Images = BaseData.ImageList
    Left = 12
    Top = 120
    object itemDbGridSetupV: TMenuItem
      Action = DbGridSetup
    end
  end
end
