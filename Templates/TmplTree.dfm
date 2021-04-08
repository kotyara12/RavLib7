inherited TreeTemplate: TTreeTemplate
  Top = 295
  Width = 764
  ActiveControl = TreeView
  Caption = 'TreeTemplate'
  Icon.Data = {
    0000010001001010000001000800680500001600000028000000100000002000
    0000010008000000000000010000000000000000000000000000000000000000
    00004000000080000000FF000000002000004020000080200000FF2000000040
    00004040000080400000FF400000006000004060000080600000FF6000000080
    00004080000080800000FF80000000A0000040A0000080A00000FFA0000000C0
    000040C0000080C00000FFC0000000FF000040FF000080FF0000FFFF00000000
    20004000200080002000FF002000002020004020200080202000FF2020000040
    20004040200080402000FF402000006020004060200080602000FF6020000080
    20004080200080802000FF80200000A0200040A0200080A02000FFA0200000C0
    200040C0200080C02000FFC0200000FF200040FF200080FF2000FFFF20000000
    40004000400080004000FF004000002040004020400080204000FF2040000040
    40004040400080404000FF404000006040004060400080604000FF6040000080
    40004080400080804000FF80400000A0400040A0400080A04000FFA0400000C0
    400040C0400080C04000FFC0400000FF400040FF400080FF4000FFFF40000000
    60004000600080006000FF006000002060004020600080206000FF2060000040
    60004040600080406000FF406000006060004060600080606000FF6060000080
    60004080600080806000FF80600000A0600040A0600080A06000FFA0600000C0
    600040C0600080C06000FFC0600000FF600040FF600080FF6000FFFF60000000
    80004000800080008000FF008000002080004020800080208000FF2080000040
    80004040800080408000FF408000006080004060800080608000FF6080000080
    80004080800080808000FF80800000A0800040A0800080A08000FFA0800000C0
    800040C0800080C08000FFC0800000FF800040FF800080FF8000FFFF80000000
    A0004000A0008000A000FF00A0000020A0004020A0008020A000FF20A0000040
    A0004040A0008040A000FF40A0000060A0004060A0008060A000FF60A0000080
    A0004080A0008080A000FF80A00000A0A00040A0A00080A0A000FFA0A00000C0
    A00040C0A00080C0A000FFC0A00000FFA00040FFA00080FFA000FFFFA0000000
    C0004000C0008000C000FF00C0000020C0004020C0008020C000FF20C0000040
    C0004040C0008040C000FF40C0000060C0004060C0008060C000FF60C0000080
    C0004080C0008080C000FF80C00000A0C00040A0C00080A0C000FFA0C00000C0
    C00040C0C00080C0C000FFC0C00000FFC00040FFC00080FFC000FFFFC0000000
    FF004000FF008000FF00FF00FF000020FF004020FF008020FF00FF20FF000040
    FF004040FF008040FF00FF40FF000060FF004060FF008060FF00FF60FF000080
    FF004080FF008080FF00FF80FF0000A0FF0040A0FF0080A0FF00FFA0FF0000C0
    FF0040C0FF0080C0FF00FFC0FF0000FFFF0040FFFF0080FFFF00FFFFFF000000
    0000000000000000000000000000000000000000004949494949490000000000
    0000000000FDFDD9B4B44900000000000000000000FEFDD9D8B4490000000000
    0000000000FEFEFEFED94900000000000000000000F8FED80000000000000000
    000000000049494949494900000000000000000000FDFDD9B4B4490000000000
    0000000000FEFDD9D8B44900000000000000000000FEFEFEFED9490000000000
    0000000000F8FED80000000000000049494949494900000000000000000000FD
    FDD9B4B44900000000000000000000FEFDD9D8B44900000000000000000000FE
    FEFEFED94900000000000000000000F8FED8000000000000000000000000FFFF
    7805FE070000FE070000E0070000EE070000EE3F0000EE070000EE070000E007
    0000EE070000EE3F000081FFBFBF81FFBF0081FF00BF81FFBFBF8FFFBF00}
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Width = 748
    Panels = <
      item
        Width = 120
      end
      item
        Width = 1000
      end>
    SimplePanel = False
  end
  inherited CoolBar: TCoolBar
    Width = 748
    Height = 40
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 38
        Width = 748
      end>
    EdgeBorders = [ebTop]
    inherited ToolBar: TToolBar
      Width = 735
      Height = 38
      ButtonHeight = 36
      ButtonWidth = 57
      EdgeBorders = [ebBottom]
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
      object DeleteItemToolButton: TToolButton
        Left = 114
        Top = 0
        Action = DeleteItem
      end
      object SeparatorEdit: TToolButton
        Left = 171
        Top = 0
        Width = 8
        Caption = 'SeparatorEdit'
        ImageIndex = 7
        Style = tbsSeparator
      end
      object FindItemToolButton: TToolButton
        Left = 179
        Top = 0
        Action = Find
      end
      object SeparatorData: TToolButton
        Left = 236
        Top = 0
        Width = 8
        Caption = 'SeparatorData'
        ImageIndex = 2
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
      object OpersToolButton: TToolButton
        Left = 301
        Top = 0
        Hint = #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1076#1072#1085#1085#1099#1093
        Caption = #1054#1087#1077#1088#1072#1094#1080#1080
        DropdownMenu = OperationsPopupMenu
        ImageIndex = 24
        Visible = False
      end
      object ReportsToolButton: TToolButton
        Left = 358
        Top = 0
        Hint = #1055#1077#1095#1072#1090#1100' '#1080' '#1101#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093
        Caption = #1054#1090#1095#1077#1090#1099
        DropdownMenu = ReportsPopupMenu
        ImageIndex = 23
        Visible = False
      end
      object SeparatorRefresh: TToolButton
        Left = 415
        Top = 0
        Width = 8
        Caption = 'SeparatorRefresh'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object RefreshToolButton: TToolButton
        Left = 423
        Top = 0
        Action = Refresh
      end
      object SeparatorEnd: TToolButton
        Left = 480
        Top = 0
        Width = 8
        Caption = 'SeparatorEnd'
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
  object TreeView: TRTreeView [2]
    Left = 0
    Top = 69
    Width = 748
    Height = 268
    Align = alClient
    HideSelection = False
    Indent = 19
    PopupMenu = PopupMenu
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    OnDblClick = TreeViewDblClick
    ListEmptyValue = '-1'
    ListDelimChar = ','
  end
  object FindPanel: TPanel [3]
    Left = 0
    Top = 40
    Width = 748
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 3
    OnResize = FindPanelResize
    object edFastFind: TEdit
      Left = 4
      Top = 4
      Width = 629
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
      Left = 636
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
  inherited ActionList: TActionList
    Left = 20
    Top = 92
    inherited Find: TAction [0]
      OnExecute = FindExecute
      OnUpdate = FindUpdate
    end
    inherited CloseSelect: TAction [1]
    end
    object FindFast: TAction [2]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082
      Hint = #1041#1099#1089#1090#1088#1099#1081' '#1087#1086#1080#1089#1082
      ImageIndex = 13
      OnExecute = FindFastExecute
      OnUpdate = FindFastUpdate
    end
    inherited CloseCancel: TAction [3]
    end
    inherited ShowHelp: TAction [4]
    end
    inherited AboutBox: TAction [5]
    end
    object NewGroup: TAction [6]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1075#1088#1091#1087#1087#1091
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1075#1088#1091#1087#1087#1091
      ImageIndex = 8
      ShortCut = 16462
    end
    object NewSubGroup: TAction [7]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1087#1086#1076#1075#1088#1091#1087#1087#1091
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1087#1086#1076#1075#1088#1091#1087#1087#1091' '#1074' '#1090#1077#1082#1091#1097#1077#1081' '#1075#1088#1091#1087#1087#1077
      ImageIndex = 8
      ShortCut = 24654
    end
    object NewItem: TAction [8]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1079#1072#1087#1080#1089#1100
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 8
      ShortCut = 16429
    end
    object Properties: TAction [9]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1074#1086#1081#1089#1090#1074#1072
      Enabled = False
      Hint = #1057#1074#1086#1081#1089#1090#1074#1072' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1086#1073#1098#1077#1082#1090#1072
      ImageIndex = 9
      ShortCut = 16397
    end
    object MoveItem: TAction [10]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1055#1077#1088#1077#1084#1077#1089#1090#1080#1090#1100
      Enabled = False
      Hint = #1055#1077#1088#1077#1084#1077#1089#1090#1080#1090#1100' '#1086#1073#1098#1077#1082#1090' '#1074' '#1076#1088#1091#1075#1091#1102' '#1075#1088#1091#1087#1087#1091
      ImageIndex = 11
      ShortCut = 16461
      OnExecute = MoveItemExecute
      OnUpdate = MoveItemUpdate
    end
    object DeleteItem: TAction [11]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Enabled = False
      Hint = #1059#1076#1072#1083#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1081' '#1086#1073#1098#1077#1082#1090
      ImageIndex = 10
      ShortCut = 16430
    end
    object ExpandAll: TAction [12]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
      Enabled = False
      Hint = #1055#1086#1082#1072#1079#1072#1090#1100' '#1074#1089#1077' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1101#1083#1077#1084#1077#1085#1090#1099
      ShortCut = 16465
      OnExecute = ExpandAllExecute
      OnUpdate = ExpandAllUpdate
    end
    object CollapseAll: TAction [13]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077
      Enabled = False
      Hint = #1057#1082#1088#1099#1090#1100' '#1074#1089#1077' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1101#1083#1077#1084#1077#1085#1090#1099
      ShortCut = 16471
      OnExecute = CollapseAllExecute
      OnUpdate = CollapseAllUpdate
    end
    object SortNone: TAction [14]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1073#1077#1079' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080
      Enabled = False
      Hint = #1053#1077' '#1091#1087#1086#1088#1103#1076#1086#1095#1080#1074#1072#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1072#1087#1086#1082
      OnExecute = SortNoneExecute
      OnUpdate = SortNoneUpdate
    end
    object SortId: TAction [15]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088#1091
      Enabled = False
      GroupIndex = 10
      Hint = 
        #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1086' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088#1091' '#1079#1072#1087#1080#1089#1080' '#1076#1083#1103' '#1074#1089#1077#1093' '#1101#1083#1077#1084#1077#1085#1090#1086 +
        #1074
      OnExecute = SortIdExecute
      OnUpdate = SortIdUpdate
    end
    object SortTypeId: TAction [16]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1090#1080#1087#1091' '#1080' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088#1091
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1086' '#1090#1080#1087#1091' '#1101#1083#1077#1084#1077#1085#1090#1072' '#1080' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088#1091' '#1079#1072#1087#1080#1089#1080
      ShortCut = 24659
      OnExecute = SortTypeIdExecute
      OnUpdate = SortTypeIdUpdate
    end
    object SortName: TAction [17]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1102
      Enabled = False
      GroupIndex = 10
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1087#1086' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1102' '#1079#1072#1087#1080#1089#1080' '#1076#1083#1103' '#1074#1089#1077#1093' '#1101#1083#1077#1084#1077#1085#1090#1086#1074
      OnExecute = SortNameExecute
      OnUpdate = SortNameUpdate
    end
    object SortTypeName: TAction [18]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1090#1080#1087#1091' '#1080' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1086' '#1090#1080#1087#1091' '#1101#1083#1077#1084#1077#1085#1090#1072' '#1080' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1102' '#1079#1072#1087#1080#1089#1080
      ShortCut = 16467
      OnExecute = SortTypeNameExecute
      OnUpdate = SortTypeNameUpdate
    end
    inherited Refresh: TAction [19]
    end
    object SortIndexSort: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1080#1085#1076#1077#1082#1089#1091
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1087#1086' '#1080#1085#1076#1077#1082#1089#1091' '#1079#1072#1087#1080#1089#1080' '#1076#1083#1103' '#1074#1089#1077#1093' '#1101#1083#1077#1084#1077#1085#1090#1086#1074
      OnExecute = SortIndexSortExecute
      OnUpdate = SortIndexSortUpdate
    end
    object EM_None: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1085#1077' '#1088#1072#1079#1074#1086#1088#1072#1095#1080#1074#1072#1090#1100
      Hint = #1053#1077' '#1088#1072#1079#1074#1086#1088#1072#1095#1080#1074#1072#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1079#1072#1087#1080#1089#1077#1081
      OnExecute = EM_NoneExecute
      OnUpdate = EM_NoneUpdate
    end
    object EM_Root: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1088#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1075#1083#1072#1074#1085#1091#1102' '#1074#1077#1090#1074#1100
      Hint = 
        #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080' '#1090#1086#1083#1100#1082#1086' '#1074' '#1082#1086#1088#1085#1077#1074#1086#1081' '#1087#1072#1087#1082#1077' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1076#1072 +
        #1085#1085#1099#1093
      OnExecute = EM_RootExecute
      OnUpdate = EM_RootUpdate
    end
    object EM_Groups: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1088#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077' '#1074#1077#1090#1074#1080
      Hint = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077' '#1086#1089#1085#1086#1074#1085#1099#1077' '#1087#1072#1087#1082#1080' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1076#1072#1085#1085#1099#1093
      Visible = False
      OnExecute = EM_GroupsExecute
      OnUpdate = EM_GroupsUpdate
    end
    object EM_All: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1088#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1102' '#1089#1090#1088#1091#1082#1090#1091#1088#1091
      Hint = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1087#1072#1087#1082#1080' '#1080' '#1101#1083#1077#1084#1077#1085#1090#1099' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1076#1072#1085#1085#1099#1093
      OnExecute = EM_AllExecute
      OnUpdate = EM_AllUpdate
    end
    object SelectSaveEM: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1047#1072#1087#1086#1084#1080#1085#1072#1090#1100' '#1088#1077#1078#1080#1084' '#1079#1072#1075#1088#1091#1079#1082#1080
      Hint = 
        #1042#1086#1089#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1090#1077#1082#1091#1097#1080#1081' '#1088#1077#1078#1080#1084' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1087#1072#1087#1086#1082' '#1087#1088#1080' '#1089#1083#1077#1076#1091#1102#1097#1077 +
        #1084' '#1074#1099#1079#1086#1074#1077' '#1092#1086#1088#1084#1099
      OnExecute = SelectSaveEMExecute
      OnUpdate = SelectSaveEMUpdate
    end
  end
  inherited PopupMenu: TPopupMenu
    Left = 76
    Top = 92
    object itemNewGroupP: TMenuItem [0]
      Action = NewGroup
    end
    object itemNewSubGroupP: TMenuItem [1]
      Action = NewSubGroup
    end
    object itemNewItemP: TMenuItem [2]
      Action = NewItem
    end
    object divPopupNew: TMenuItem [3]
      Caption = '-'
    end
    object itemPropertiesP: TMenuItem [4]
      Action = Properties
      Default = True
    end
    object itemDeleteItemP: TMenuItem [5]
      Action = DeleteItem
    end
    object divPopupEdit: TMenuItem [6]
      Caption = '-'
    end
    object itemMoveItemP: TMenuItem [7]
      Action = MoveItem
    end
    object divPopupMove: TMenuItem [8]
      Caption = '-'
    end
    inherited divPopupSubmenus: TMenuItem
      Visible = True
    end
    inherited menuDataP: TMenuItem
      Visible = True
      object menuSortTreeP: TMenuItem
        Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072
        Hint = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1076#1072#1085#1085#1099#1093
        ImageIndex = 18
        object itemSortNoneP: TMenuItem
          Action = SortNone
        end
        object itemSortIdP: TMenuItem
          Action = SortId
        end
        object itemSortTypeIdP: TMenuItem
          Action = SortTypeId
        end
        object itemSortNameP: TMenuItem
          Action = SortName
        end
        object itemSotyTypeNameP: TMenuItem
          Action = SortTypeName
        end
        object itemSortIndexSortP: TMenuItem
          Action = SortIndexSort
        end
      end
      object menuExpandDataP: TMenuItem
        Caption = #1056#1077#1078#1080#1084#1099' '#1079#1072#1075#1088#1091#1079#1082#1080
        ImageIndex = 32
        object itemSelectSaveEMP: TMenuItem
          Action = SelectSaveEM
        end
        object divEMP: TMenuItem
          Caption = '-'
        end
        object itemEM_NoneP: TMenuItem
          Action = EM_None
        end
        object itemEM_RootP: TMenuItem
          Action = EM_Root
        end
        object itemEM_GroupsP: TMenuItem
          Action = EM_Groups
        end
        object itemEM_AllP: TMenuItem
          Action = EM_All
        end
      end
      object divPopupExp: TMenuItem
        Caption = '-'
      end
      object itemExpandAllP: TMenuItem
        Action = ExpandAll
      end
      object itemCollapseAllP: TMenuItem
        Action = CollapseAll
      end
    end
  end
  inherited MainMenu: TMainMenu
    Left = 48
    Top = 92
    inherited menuEdit: TMenuItem
      object itemNewGroup: TMenuItem [0]
        Action = NewGroup
      end
      object itemNewSubGroup: TMenuItem [1]
        Action = NewSubGroup
      end
      object itemNewItem: TMenuItem [2]
        Action = NewItem
      end
      object divEditNew: TMenuItem [3]
        Caption = '-'
      end
      object itemProperties: TMenuItem [4]
        Action = Properties
      end
      object itemDeleteItem: TMenuItem [5]
        Action = DeleteItem
      end
      object divEditEdit: TMenuItem [6]
        Caption = '-'
      end
      object itemMoveItem: TMenuItem [7]
        Action = MoveItem
      end
      object divEditRefresh: TMenuItem [8]
        Caption = '-'
      end
    end
    inherited menuData: TMenuItem
      object divDataFind: TMenuItem
        Caption = '-'
      end
      object menuSortTree: TMenuItem
        Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072
        Hint = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1076#1072#1085#1085#1099#1093
        ImageIndex = 18
        object itemSortNone: TMenuItem
          Action = SortNone
        end
        object itemSortId: TMenuItem
          Action = SortId
        end
        object itemSortTypeId: TMenuItem
          Action = SortTypeId
        end
        object itemSortName: TMenuItem
          Action = SortName
        end
        object itemSotyTypeName: TMenuItem
          Action = SortTypeName
        end
        object itemSortIndexSort: TMenuItem
          Action = SortIndexSort
        end
      end
      object menuExpandMode: TMenuItem
        Caption = #1056#1077#1078#1080#1084#1099' '#1079#1072#1075#1088#1091#1079#1082#1080
        ImageIndex = 32
        object itemSelectSaveEM: TMenuItem
          Action = SelectSaveEM
        end
        object divEM: TMenuItem
          Caption = '-'
        end
        object itemEM_None: TMenuItem
          Action = EM_None
        end
        object itemEM_Root: TMenuItem
          Action = EM_Root
        end
        object itemEM_Groups: TMenuItem
          Action = EM_Groups
        end
        object itemEM_All: TMenuItem
          Action = EM_All
        end
      end
      object divDataExp: TMenuItem
        Caption = '-'
      end
      object itemExpandAll: TMenuItem
        Action = ExpandAll
      end
      object itemCollapseAll: TMenuItem
        Action = CollapseAll
      end
    end
    inherited menuReports: TMenuItem
      Visible = False
    end
  end
  object NewPopupMenu: TPopupMenu [7]
    Images = BaseData.ImageList
    Left = 188
    Top = 92
    object itemNewGroupN: TMenuItem
      Action = NewGroup
    end
    object itemNewSubGroupN: TMenuItem
      Action = NewSubGroup
    end
    object itemNewItemN: TMenuItem
      Action = NewItem
    end
  end
  inherited DataPopupMenu: TPopupMenu
    Left = 104
    Top = 92
    object menuSortTreeD: TMenuItem
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072
      Hint = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 18
      object itemSortNoneD: TMenuItem
        Action = SortNone
      end
      object itemSortIdD: TMenuItem
        Action = SortId
      end
      object itemSortTypeIdD: TMenuItem
        Action = SortTypeId
      end
      object itemSortNameD: TMenuItem
        Action = SortName
      end
      object itemSotyTypeNameD: TMenuItem
        Action = SortTypeName
      end
      object itemSortIndexSortD: TMenuItem
        Action = SortIndexSort
      end
    end
    object menuExpandDataD: TMenuItem
      Caption = #1056#1077#1078#1080#1084#1099' '#1079#1072#1075#1088#1091#1079#1082#1080
      ImageIndex = 32
      object itemSelectSaveEMD: TMenuItem
        Action = SelectSaveEM
      end
      object divEMD: TMenuItem
        Caption = '-'
      end
      object itemEM_NoneD: TMenuItem
        Action = EM_None
      end
      object itemEM_RootD: TMenuItem
        Action = EM_Root
      end
      object itemEM_GroupsD: TMenuItem
        Action = EM_Groups
      end
      object itemEM_AllD: TMenuItem
        Action = EM_All
      end
    end
    object divDataDExp: TMenuItem
      Caption = '-'
    end
    object itemExpandAllD: TMenuItem
      Action = ExpandAll
    end
    object itemCollapseAllD: TMenuItem
      Action = CollapseAll
    end
  end
  inherited OperationsPopupMenu: TPopupMenu
    Left = 132
    Top = 92
  end
  inherited ReportsPopupMenu: TPopupMenu
    Left = 160
    Top = 92
  end
end
