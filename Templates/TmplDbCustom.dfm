inherited DbCustomTemplate: TDbCustomTemplate
  Left = 354
  Top = 208
  Width = 802
  ActiveControl = DbGrid
  Caption = 'DbCustomTemplate'
  Icon.Data = {
    0000010001001010000001000800680500001600000028000000100000002000
    0000010008000000000000000000000000000000000000000000000000000000
    0000FFFEFE00BC8F7A00BE937F00C8622F00CD653100E3763E00E2753E00E37A
    4400E37B4500E47F4C00E4804E00E5855400E5875800E68B5D00E68E6200F498
    6900E6916700F59D7000E7956C00F7A27700E7977000E89C7600F8A87F00E89D
    7800FAAF8800E9A17F00E9A28000EAA78600E9A68600FDBA9700EAAA8B00C5A0
    8E00C7A59400CAAA9A00CDAFA000CFB4A600FEFAF800FFFDFC00FBB59000FEBF
    9E00FFC2A200D2B9AB00D6C1B500D8C5BA00D1B7A700F8DBCA00D2BAAB00D3BC
    AE00F9E1D200D6C3B700FAE5D800DAC8BD00FAE7DB00DBCAC000D8C8BE00FBEB
    E100FCEDE400DBCEC600FCF1EA00FDF3ED00FDF5F000C8B2A300CCB6A700D5C2
    B500DACDC400FDF7F30063493500B7A29300C3AE9E00BEA99A00DED6D000DDD4
    CD00DFD9D400E1DCD800FEF9F500FEFCFA00FFFFFF0000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000004443
    4343434343434343434E4E4E4E4E443D4A49483A3740302D434E4E4E4E4E444C
    2A24234443434343434343434343444D264B3C443D4A49483A3740302D43464D
    342C2B444C2A2423330303022F43454D444343444D264B3C3935312E32433E4D
    443D4A464D342C2B3D21212041433F4D444C2A454D4D4D4D4C423B3847431F1D
    444D263E4D3636344D2323224A431F29464D343F4D4D4D4D4D4D01253D431F1C
    454D4D1F1D1A16130F0D0B0906044E4E3E4D361F29281E271917141210054E4E
    3F4D4D1F1C1B1815110E0C0A08074E4E1F1D1A16130F0D0B0906044E4E4E4E4E
    1F29281E271917141210054E4E4E4E4E1F1C1B1815110E0C0A08074E4E4E001F
    0101001F01050000CB4B0000BBD00000CFB50000B3B30000B3B30000B3020000
    0142000041640000426CC0000E9EC0004745C0077574C0074444C007AC02}
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Width = 786
    Panels = <
      item
        Text = #1053#1077#1090' '#1076#1072#1085#1085#1099#1093
        Width = 160
      end
      item
        Text = #1053#1072#1073#1086#1088' '#1076#1072#1085#1085#1099#1093' '#1079#1072#1082#1088#1099#1090
        Width = 160
      end>
    SimplePanel = False
  end
  inherited CoolBar: TCoolBar
    Width = 786
    Height = 27
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 23
        Width = 786
      end>
    inherited ToolBar: TToolBar
      Width = 773
      Height = 23
      ButtonHeight = 36
      ButtonWidth = 51
    end
  end
  object DataPanel: TPanel [2]
    Left = 0
    Top = 56
    Width = 786
    Height = 355
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object DbGrid: TRDbStyledGrid
      Left = 0
      Top = 0
      Width = 786
      Height = 290
      Align = alClient
      DataSource = RDbEditor
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
      PopupMenu = PopupMenu
      TabOrder = 0
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
    object InfoPanel: TRDbInfoPanel
      Left = 0
      Top = 311
      Width = 786
      Height = 44
      Align = alBottom
      BevelOuter = bvNone
      BorderStyle = bsSingle
      PopupMenu = InfoPanelPopupMenu
      TabOrder = 1
    end
    object TabViews: TTabSet
      Left = 0
      Top = 290
      Width = 786
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
  object FindPanel: TPanel [3]
    Left = 0
    Top = 27
    Width = 786
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    OnResize = FindPanelResize
    object edFastFind: TEdit
      Left = 4
      Top = 4
      Width = 665
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
      Left = 672
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
    Left = 32
    Top = 88
    object DataSetFirst: TDataSetFirst [4]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042' '#1085#1072#1095#1072#1083#1086
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1085#1072#1095#1072#1083#1086' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 4
      ShortCut = 36
      DataSource = RDbEditor
    end
    object DataSetPrior: TDataSetPrior [5]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1053#1072#1079#1072#1076
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1085#1072' '#1087#1088#1077#1076#1099#1076#1091#1097#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 5
      DataSource = RDbEditor
    end
    object DataSetNext: TDataSetNext [6]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042#1087#1077#1088#1077#1076
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1085#1072' '#1089#1083#1077#1076#1091#1102#1097#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 6
      DataSource = RDbEditor
    end
    object DataSetLast: TDataSetLast [7]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042' '#1082#1086#1085#1077#1094
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1082#1086#1085#1077#1094' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 7
      ShortCut = 35
      DataSource = RDbEditor
    end
    object DataSetInsert: TAction [8]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 8
      ShortCut = 16429
      OnExecute = DataSetInsertExecute
      OnUpdate = DataSetInsertUpdate
    end
    object DataSetCopy: TAction [9]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1050#1083#1086#1085#1080#1088#1086#1074#1072#1090#1100
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100' '#1085#1072' '#1086#1089#1085#1086#1074#1077' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081
      ImageIndex = 8
      ShortCut = 16459
      OnExecute = DataSetCopyExecute
      OnUpdate = DataSetCopyUpdate
    end
    object DataSetEdit: TAction [10]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1074#1086#1081#1089#1090#1074#1072
      Enabled = False
      Hint = #1057#1074#1086#1081#1089#1090#1074#1072' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 9
      ShortCut = 16397
      OnExecute = DataSetEditExecute
      OnUpdate = DataSetEditUpdate
    end
    object DataSetDelete: TAction [11]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Enabled = False
      Hint = #1059#1076#1072#1083#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 10
      ShortCut = 16430
      OnExecute = DataSetDeleteExecute
      OnUpdate = DataSetDeleteUpdate
    end
    object ImportDS: TAction [12]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072'...'
      Enabled = False
      Hint = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072' '#1076#1072#1085#1085#1099#1093' '#1080#1079' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
      ImageIndex = 31
      OnExecute = ImportDSExecute
      OnUpdate = ImportDSUpdate
    end
    inherited Find: TAction [13]
      ShortCut = 16454
      OnExecute = FindExecute
      OnUpdate = FindUpdate
    end
    object FindColumn: TAction [14]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082'...'
      Enabled = False
      Hint = #1055#1086#1080#1089#1082' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1089#1090#1086#1083#1073#1094#1091
      ImageIndex = 13
      OnExecute = FindColumnExecute
      OnUpdate = FindColumnUpdate
    end
    object FindFast: TAction [15]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082
      Hint = #1041#1099#1089#1090#1088#1099#1081' '#1087#1086#1080#1089#1082
      ImageIndex = 13
      OnExecute = FindFastExecute
      OnUpdate = FindFastUpdate
    end
    object DbLocate: TAction [16]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082'...'
      Enabled = False
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1091#1082#1072#1079#1072#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 13
      ShortCut = 16455
      OnExecute = DbLocateExecute
      OnUpdate = DbLocateUpdate
    end
    object DbGridSetup: TAction [17]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1080#1103'...'
      Enabled = False
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1089#1090#1086#1083#1073#1094#1086#1074' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 19
      OnExecute = DbGridSetupExecute
      OnUpdate = DbGridSetupUpdate
    end
    object DbGridDefault: TAction [18]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1090#1086#1083#1073#1094#1099' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1089#1090#1086#1083#1073#1094#1099' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'" ('#1074#1089#1077' '#1074#1080#1076#1080#1084#1099#1077')'
      ImageIndex = 20
      OnExecute = DbGridDefaultExecute
      OnUpdate = DbGridDefaultUpdate
    end
    object DataSetReportList: TAction [19]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1053#1072#1089#1090#1088#1072#1080#1074#1072#1077#1084#1099#1077' '#1086#1090#1095#1077#1090#1099
      Enabled = False
      Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1089#1087#1080#1089#1082#1086#1084' '#1086#1090#1095#1077#1090#1086#1074
      ImageIndex = 21
      ShortCut = 16464
      OnExecute = DataSetReportListExecute
      OnUpdate = DataSetReportListUpdate
    end
    object ExportToExcel: TAction [20]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' Excel'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' Microsoft Excel'
      ImageIndex = 22
      ShortCut = 16453
      OnExecute = ExportToExcelExecute
      OnUpdate = ExportToExcelUpdate
    end
    inherited Refresh: TAction [21]
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
    object ExportToFileCsv: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' '#1092#1072#1081#1083' CSV'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' '#1092#1072#1081#1083' CSV ('#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083' '#1089' '#1088#1072#1079#1076#1077#1083#1080#1090#1077#1083#1103#1084#1080')'
      ImageIndex = 30
      ShortCut = 16467
      OnExecute = ExportToFileCsvExecute
      OnUpdate = ExportToFileCsvUpdate
    end
    object CreateDynamicReport: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1055#1077#1095#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
      Enabled = False
      Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1090#1077#1082#1089#1090#1086#1074#1086#1075#1086' '#1086#1090#1095#1077#1090#1072' '#1076#1083#1103' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 23
      ShortCut = 16466
      OnExecute = CreateDynamicReportExecute
      OnUpdate = CreateDynamicReportUpdate
    end
    object ColumnLeft: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1083#1077#1074#1086#1084#1091' '#1082#1088#1072#1102
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1083#1077#1074#1086#1084#1091' '#1082#1088#1072#1102
      OnExecute = ColumnLeftExecute
      OnUpdate = ColumnLeftUpdate
    end
    object ColumnCenter: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1094#1077#1085#1090#1088#1091
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1094#1077#1085#1090#1088#1091
      OnExecute = ColumnCenterExecute
      OnUpdate = ColumnCenterUpdate
    end
    object ColumnRight: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1087#1088#1072#1074#1086#1084#1091' '#1082#1088#1072#1102
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1087#1088#1072#1074#1086#1084#1091' '#1082#1088#1072#1102
      OnExecute = ColumnRightExecute
      OnUpdate = ColumnRightUpdate
    end
    object DataSetStatistic: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1079#1072#1087#1080#1089#1077#1081', '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1099#1084' '#1087#1086#1083#1103#1084
      ImageIndex = 45
      ShortCut = 16469
      OnExecute = DataSetStatisticUpdate
      OnUpdate = DataSetStatisticExecute
    end
    object ColumnStatistic: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1079#1072#1087#1080#1089#1077#1081', '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1087#1086#1083#1102
      ImageIndex = 45
      OnExecute = ColumnStatisticUpdate
      OnUpdate = ColumnStatisticExecute
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
  object RDbFind: TRDbSearch [5]
    AutoActivate = False
    Options = [olUseHistory, olStoreLastFind, olStoreHistory]
    Left = 228
    Top = 88
  end
  object RDbGridTuner: TRDbGridTuner [6]
    AutoActivate = False
    DbGrid = DbGrid
    Options = [ogStoreItemsState, ogChangeOptions]
    OnViewChange = RDbGridTunerViewChange
    Left = 256
    Top = 88
  end
  object RDbFilterStatus: TRDbFilterStatus [7]
    DataSource = RDbEditor
    StatusBar = StatusBar
    Options = [soRecordCount, soRecordNum, soShowCaptions]
    Left = 284
    Top = 88
  end
  inherited PopupMenu: TPopupMenu
    AutoPopup = False
    Left = 88
    Top = 88
    object itemDataSetInsertP: TMenuItem [0]
      Action = DataSetInsert
    end
    object itemitemDataSetCopyP: TMenuItem [1]
      Action = DataSetCopy
    end
    object itemDataSetEditP: TMenuItem [2]
      Action = DataSetEdit
      Default = True
    end
    object itemDataSetDeleteP: TMenuItem [3]
      Action = DataSetDelete
    end
    object divPopupEdit: TMenuItem [4]
      Caption = '-'
    end
    object itemDbLocateP: TMenuItem [6]
      Action = DbLocate
    end
    inherited divPopupSubmenus: TMenuItem
      Visible = True
    end
    object menuDbNavigation: TMenuItem [8]
      Caption = #1053#1072#1074#1080#1075#1072#1094#1080#1103' '#1087#1086' '#1090#1072#1073#1083#1080#1094#1077
      Hint = #1053#1072#1074#1080#1075#1072#1094#1080#1103' '#1087#1086' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 14
      object itemDataSetFirstP: TMenuItem
        Action = DataSetFirst
      end
      object itemDataSetPriorP: TMenuItem
        Action = DataSetPrior
      end
      object itemDataSetNextP: TMenuItem
        Action = DataSetNext
      end
      object itemDataSetLastP: TMenuItem
        Action = DataSetLast
      end
    end
    object menuViewsP: TMenuItem [9]
      Caption = #1055#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1080#1103
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1074#1072#1088#1080#1072#1085#1090#1086#1074' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 19
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
      object itemCreateDynamicReportP: TMenuItem
        Action = CreateDynamicReport
      end
      object itemExportToExcelP: TMenuItem
        Action = ExportToExcel
      end
      object itemExportToFileP: TMenuItem
        Action = ExportToFileCsv
      end
    end
    object menuGroupP: TMenuItem [12]
      Caption = #1043#1088#1091#1087#1087#1080#1088#1086#1074#1082#1072
      Hint = #1043#1088#1091#1087#1087#1080#1088#1086#1074#1082#1072' '#1080' '#1089#1090#1072#1090#1080#1089#1090#1080#1082#1072
      ImageIndex = 45
      object itemDateSetGroupingP: TMenuItem
        Action = DateSetGrouping
      end
      object itemDataSetStatisticP: TMenuItem
        Action = DataSetStatistic
      end
    end
    object itemAttachmentsP: TMenuItem [13]
      Action = Attachments
    end
    object divPopupMulti: TMenuItem [14]
      Caption = '-'
    end
    object itemSelectDefaultValuesP: TMenuItem [15]
      Action = SelectDefaultValues
    end
    object itemMultiSelectOnOffP: TMenuItem [16]
      Action = MultiSelectOnOff
    end
    object itemSelectAllP: TMenuItem [17]
      Action = SelectAll
    end
  end
  inherited MainMenu: TMainMenu
    Left = 60
    Top = 88
    inherited menuEdit: TMenuItem
      object itemDataSetInsert: TMenuItem [0]
        Action = DataSetInsert
      end
      object itemDataSetCopy: TMenuItem [1]
        Action = DataSetCopy
      end
      object itemDataSetEdit: TMenuItem [2]
        Action = DataSetEdit
      end
      object itemDataSetDelete: TMenuItem [3]
        Action = DataSetDelete
      end
      object divAttach: TMenuItem [4]
        Caption = '-'
        Visible = False
      end
      object itemAttachments: TMenuItem [5]
        Action = Attachments
      end
      object divEditRefresh: TMenuItem [6]
        Caption = '-'
      end
      object itemImportDS: TMenuItem [7]
        Action = ImportDS
      end
      object divEditMulti: TMenuItem [8]
        Caption = '-'
        Visible = False
      end
      object itemSelectDefaultValues: TMenuItem [9]
        Action = SelectDefaultValues
      end
      object itemMultiSelectOnOff: TMenuItem [10]
        Action = MultiSelectOnOff
      end
      object itemSelectAll: TMenuItem [11]
        Action = SelectAll
      end
      object divEditImport: TMenuItem [12]
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
      object divDataFind: TMenuItem [4]
        Caption = '-'
      end
      object itemDbLocate: TMenuItem
        Action = DbLocate
      end
      object divDataGrid: TMenuItem
        Caption = '-'
      end
      object itemDbGridSetup: TMenuItem
        Action = DbGridSetup
      end
      object itemDbGridDefault: TMenuItem
        Action = DbGridDefault
      end
    end
    inherited menuReports: TMenuItem
      object itemDataSetReportList: TMenuItem
        Action = DataSetReportList
      end
      object divRepExp: TMenuItem
        Caption = '-'
      end
      object itemCreateDynamicReport: TMenuItem
        Action = CreateDynamicReport
      end
      object itemExportToExcel: TMenuItem
        Action = ExportToExcel
      end
      object itemExportToFile: TMenuItem
        Action = ExportToFileCsv
      end
      object divRepStat: TMenuItem
        Caption = '-'
      end
      object itemDateSetGrouping: TMenuItem
        Action = DateSetGrouping
      end
      object itemDataSetStatistic: TMenuItem
        Action = DataSetStatistic
      end
    end
  end
  inherited DataPopupMenu: TPopupMenu
    Left = 116
    Top = 88
    object itemDbGridSetupD: TMenuItem
      Action = DbGridSetup
    end
    object itemDbGridDefaultD: TMenuItem
      Action = DbGridDefault
    end
  end
  inherited OperationsPopupMenu: TPopupMenu
    Left = 144
    Top = 88
  end
  inherited ReportsPopupMenu: TPopupMenu
    Left = 172
    Top = 88
    object itemDataSetReportListR: TMenuItem
      Action = DataSetReportList
    end
    object divRepExpR: TMenuItem
      Caption = '-'
    end
    object itemCreateDynamicReportR: TMenuItem
      Action = CreateDynamicReport
    end
    object itemExportToExcelR: TMenuItem
      Action = ExportToExcel
    end
    object itemExportToFileR: TMenuItem
      Action = ExportToFileCsv
    end
    object divRepStatR: TMenuItem
      Caption = '-'
    end
    object itemDateSetGroupingR: TMenuItem
      Action = DateSetGrouping
    end
    object itemDataSetStatisticR: TMenuItem
      Action = DataSetStatistic
    end
  end
  object RDbEditor: TRDbExportEditor
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    LogEnable = True
    OpenMode = omAuto
    OnBeforeShowEditor = RDbEditorBeforeShowEditor
    OnGetNewKey = RDbEditorGetNewKey
    OnFreeNewKey = RDbEditorFreeNewKey
    OnCreateSetDefault = RDbEditorCreateSetDefault
    OnBeforeDelete = RDbEditorBeforeDelete
    OnSaveToLog = RDbEditorSaveToLog
    DbGrid = DbGrid
    OnGetExcelCopyright = RDbEditorGetExcelCopyright
    OnGetExcelCaption = RDbEditorGetExcelCaption
    Left = 200
    Top = 88
  end
  object TitleGridPopupMenu: TPopupMenu
    AutoPopup = False
    Images = BaseData.ImageList
    Left = 312
    Top = 88
    object itemFindColumn: TMenuItem
      Action = FindColumn
    end
    object itemDbLocateT: TMenuItem
      Action = DbLocate
    end
    object divGridTitleFind: TMenuItem
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
    object divGridTitleStat: TMenuItem
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
    Left = 340
    Top = 88
  end
  object RDbUpdater: TRDbUpdater
    Editor = RDbEditor
    Left = 368
    Top = 88
  end
  object InfoPanelPopupMenu: TPopupMenu
    Images = BaseData.ImageList
    OnPopup = InfoPanelPopupMenuPopup
    Left = 396
    Top = 88
    object itemCopyInfoPanel: TMenuItem
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
      Hint = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074' '#1073#1091#1092#1077#1088' '#1086#1073#1084#1077#1085#1072
      ImageIndex = 42
      OnClick = itemCopyInfoPanelClick
    end
  end
  object ViewsPopupMenu: TPopupMenu
    Images = BaseData.ImageList
    Left = 32
    Top = 116
    object itemDbGridSetupV: TMenuItem
      Action = DbGridSetup
    end
  end
end
