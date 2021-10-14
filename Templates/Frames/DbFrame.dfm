object FrameDb: TFrameDb
  Left = 0
  Top = 0
  Width = 525
  Height = 192
  TabOrder = 0
  object FindPanel: TPanel
    Left = 0
    Top = 0
    Width = 525
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    Visible = False
    OnResize = FindPanelResize
    object edFastFilter: TEdit
      Left = 0
      Top = 4
      Width = 405
      Height = 21
      Hint = 
        #1042#1074#1077#1076#1080#1090#1077' '#1090#1077#1082#1089#1090' '#1076#1083#1103' '#1087#1086#1080#1089#1082#1072' '#1080' '#1085#1072#1078#1084#1080#1090#1077' [ Enter ] ('#1080#1083#1080' '#1082#1085#1086#1087#1082#1091' "'#1055#1086#1080#1089#1082'"' +
        ')'#13#10#1054#1090#1084#1077#1085#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' - '#1082#1083#1072#1074#1080#1096#1072' [ Esc ] '#1080#1083#1080' '#1086#1095#1080#1089#1090#1080#1090#1100' '#1087#1086#1083#1077' '#1080' [ Enter' +
        ' ]'
      TabOrder = 0
      OnEnter = edFastFilterEnter
      OnExit = edFastFilterExit
      OnKeyPress = edFastFilterKeyPress
    end
    object btnFastFilter: TBitBtn
      Left = 412
      Top = 4
      Width = 107
      Height = 21
      Action = actDbFastFilter
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
  object DbGrid: TRDbStyledGrid
    Left = 0
    Top = 29
    Width = 525
    Height = 142
    Align = alClient
    DataSource = RDbEditor
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
    PopupMenu = GridPopupMenu
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
  object DbTabViews: TTabSet
    Left = 0
    Top = 171
    Width = 525
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
    OnChange = DbTabViewsChange
  end
  object DbActions: TActionList
    Images = BaseData.ImageList
    Left = 260
    Top = 80
    object actDbFirst: TDataSetFirst
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042' '#1085#1072#1095#1072#1083#1086
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1085#1072#1095#1072#1083#1086' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 4
      ShortCut = 36
      DataSource = RDbEditor
    end
    object actDbPrior: TDataSetPrior
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1053#1072#1079#1072#1076
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1085#1072' '#1087#1088#1077#1076#1099#1076#1091#1097#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 5
      DataSource = RDbEditor
    end
    object actDbNext: TDataSetNext
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042#1087#1077#1088#1077#1076
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1085#1072' '#1089#1083#1077#1076#1091#1102#1097#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 6
      DataSource = RDbEditor
    end
    object actDbLast: TDataSetLast
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042' '#1082#1086#1085#1077#1094
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1082#1086#1085#1077#1094' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 7
      ShortCut = 35
      DataSource = RDbEditor
    end
    object actDbInsert: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 8
      ShortCut = 16429
      OnExecute = actDbInsertExecute
      OnUpdate = actDbInsertUpdate
    end
    object actDbClone: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1050#1083#1086#1085#1080#1088#1086#1074#1072#1090#1100
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100' '#1085#1072' '#1086#1089#1085#1086#1074#1077' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081
      ImageIndex = 8
      ShortCut = 16459
      OnExecute = actDbCloneExecute
      OnUpdate = actDbCloneUpdate
    end
    object actDbEdit: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1074#1086#1081#1089#1090#1074#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088#1077#1090#1100' '#1080#1083#1080' '#1088#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 9
      ShortCut = 16397
      OnExecute = actDbEditExecute
      OnUpdate = actDbEditUpdate
    end
    object actDbDelete: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Enabled = False
      Hint = #1059#1076#1072#1083#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 10
      ShortCut = 16430
      OnExecute = actDbDeleteExecute
      OnUpdate = actDbDeleteUpdate
    end
    object actDbImport: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072'...'
      Enabled = False
      Hint = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072' '#1076#1072#1085#1085#1099#1093' '#1080#1079' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
      ImageIndex = 31
      OnExecute = actDbImportExecute
      OnUpdate = actDbImportUpdate
    end
    object actDbRefresh: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1054#1073#1085#1086#1074#1080#1090#1100
      Enabled = False
      Hint = #1055#1077#1088#1077#1079#1072#1075#1088#1091#1079#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080#1079' '#1093#1088#1072#1085#1080#1083#1080#1097#1072' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 12
      ShortCut = 116
      OnExecute = actDbRefreshExecute
      OnUpdate = actDbRefreshUpdate
    end
    object actDbAttachments: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1055#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1077' '#1092#1072#1081#1083#1099'...'
      Enabled = False
      Hint = 
        #1055#1088#1086#1089#1084#1086#1090#1088' '#1087#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1093' '#1082' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080' '#1092#1072#1081#1083#1086#1074' '#1080' '#1091#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1080 +
        #1084#1080
      ImageIndex = 38
      ShortCut = 123
      OnExecute = actDbAttachmentsExecute
      OnUpdate = actDbAttachmentsUpdate
    end
    object actDbMultiSelectOnOff: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1052#1091#1083#1100#1090#1080#1086#1073#1088#1072#1073#1086#1090#1082#1072
      Enabled = False
      Hint = 
        #1055#1077#1088#1077#1082#1083#1102#1095#1077#1085#1080#1077' '#1088#1077#1078#1080#1084#1072' '#1074#1099#1076#1077#1083#1077#1085#1080#1103' '#1080' '#1086#1073#1088#1072#1073#1086#1090#1082#1080' '#1085#1077#1089#1082#1086#1083#1100#1082#1080#1093' '#1079#1072#1087#1080#1089#1077#1081' '#1086#1076#1085 +
        #1086#1081' '#1082#1086#1084#1072#1085#1076#1086#1081
      ShortCut = 49229
      OnExecute = actDbMultiSelectOnOffExecute
      OnUpdate = actDbMultiSelectOnOffUpdate
    end
    object actDbSetDefaultValues: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1047#1085#1072#1095#1077#1085#1080#1103' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = 
        #1042#1099#1073#1088#1072#1090#1100' '#1079#1085#1072#1095#1077#1085#1080#1103' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'" '#1076#1083#1103' '#1085#1086#1074#1099#1093' '#1079#1072#1087#1080#1089#1077#1081' '#1074' '#1090#1077#1082#1091#1097#1077#1084' '#1089#1077#1072#1085 +
        #1089#1077' '#1088#1072#1073#1086#1090#1099
      ShortCut = 49220
      OnExecute = actDbSetDefaultValuesExecute
      OnUpdate = actDbSetDefaultValuesUpdate
    end
    object actDbFind: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082'...'
      Enabled = False
      Hint = #1055#1086#1080#1089#1082' '#1086#1076#1085#1086#1081' '#1080#1083#1080' '#1085#1077#1089#1082#1086#1083#1100#1082#1080#1093' '#1079#1072#1087#1080#1089#1077#1081' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 13
      ShortCut = 16454
      OnExecute = actDbFindExecute
      OnUpdate = actDbFindUpdate
    end
    object actDbFindColumn: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082'...'
      Enabled = False
      Hint = #1055#1086#1080#1089#1082' '#1086#1076#1085#1086#1081' '#1080#1083#1080' '#1085#1077#1089#1082#1086#1083#1100#1082#1080#1093' '#1079#1072#1087#1080#1089#1077#1081' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1087#1086#1083#1102
      ImageIndex = 13
      OnExecute = actDbFindColumnExecute
      OnUpdate = actDbFindColumnUpdate
    end
    object actDbLocate: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082'...'
      Enabled = False
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1087#1077#1088#1074#1086#1081' '#1085#1072#1081#1076#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 13
      ShortCut = 16455
      OnExecute = actDbLocateExecute
      OnUpdate = actDbLocateUpdate
    end
    object actDbFastFilter: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082
      Enabled = False
      Hint = #1041#1099#1089#1090#1088#1099#1081' '#1087#1086#1080#1089#1082
      ImageIndex = 13
      OnExecute = actDbFastFilterExecute
      OnUpdate = actDbFastFilterUpdate
    end
    object actDbFilterCustom: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' '#1076#1072#1085#1085#1099#1093'...'
      Enabled = False
      Hint = #1042#1099#1073#1086#1088' '#1087#1088#1086#1080#1079#1074#1086#1083#1100#1085#1086#1075#1086' '#1092#1080#1083#1100#1090#1088#1072' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 15
      ShortCut = 119
      OnExecute = actDbFilterCustomExecute
      OnUpdate = actDbFilterCustomUpdate
    end
    object actDbFilterDefault: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
      ImageIndex = 16
      ShortCut = 16503
      OnExecute = actDbFilterDefaultExecute
      OnUpdate = actDbFilterDefaultUpdate
    end
    object actDbFilterSelected: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' '#1087#1086' '#1074#1099#1076#1077#1083#1077#1085#1080#1102
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' '#1087#1086' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1084' '#1079#1072#1087#1080#1089#1103#1084
      ImageIndex = 16
      ShortCut = 49271
      OnExecute = actDbFilterSelectedExecute
      OnUpdate = actDbFilterSelectedUpdate
    end
    object actDbFilterNone: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1054#1090#1082#1083#1102#1095#1080#1090#1100' '#1074#1089#1077' '#1092#1080#1083#1100#1090#1088#1099
      Enabled = False
      Hint = #1054#1090#1082#1083#1102#1095#1080#1090#1100' '#1074#1089#1077' '#1092#1080#1083#1100#1090#1088#1099' '#1080' '#1087#1086#1082#1072#1079#1072#1090#1100' '#1074#1089#1077' '#1079#1072#1087#1080#1089#1080' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 17
      ShortCut = 32887
      OnExecute = actDbFilterNoneExecute
      OnUpdate = actDbFilterNoneUpdate
    end
    object actDbSortCustom: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072'...'
      Enabled = False
      Hint = #1042#1099#1073#1086#1088' '#1087#1086#1083#1077#1081' '#1080' '#1085#1072#1087#1088#1072#1074#1083#1077#1085#1080#1103' '#1076#1083#1103' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 18
      ShortCut = 120
      OnExecute = actDbSortCustomExecute
      OnUpdate = actDbSortCustomUpdate
    end
    object actDbSortDefault: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1091' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
      ImageIndex = 18
      ShortCut = 16504
      OnExecute = actDbSortDefaultExecute
      OnUpdate = actDbSortDefaultUpdate
    end
    object actDbSortColumnAsc: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1087#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102' '#1074' '#1076#1072#1085#1085#1086#1084' '#1089#1090#1086#1083#1073#1094#1077
      OnExecute = actDbSortColumnAscExecute
      OnUpdate = actDbSortColumnAscUpdate
    end
    object actDbSortColumnDesc: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1091#1073#1099#1074#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1087#1086' '#1091#1073#1099#1074#1072#1085#1080#1102' '#1074' '#1076#1072#1085#1085#1086#1084' '#1089#1090#1086#1083#1073#1094#1077
      OnExecute = actDbSortColumnDescExecute
      OnUpdate = actDbSortColumnDescUpdate
    end
    object actDbGridTune: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1080#1103'...'
      Enabled = False
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1089#1090#1086#1083#1073#1094#1086#1074' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 19
      OnExecute = actDbGridTuneExecute
      OnUpdate = actDbGridTuneUpdate
    end
    object actDbGridDefault: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1090#1086#1083#1073#1094#1099' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1057#1073#1088#1086#1089#1080#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1091' '#1090#1072#1073#1083#1080#1094#1099' '#1082' '#1079#1085#1072#1095#1077#1085#1080#1103#1084' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
      ImageIndex = 20
      OnExecute = actDbGridDefaultExecute
      OnUpdate = actDbGridDefaultUpdate
    end
    object actDbColumnLeft: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1083#1077#1074#1086#1084#1091' '#1082#1088#1072#1102
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1083#1077#1074#1086#1084#1091' '#1082#1088#1072#1102
      OnExecute = actDbColumnLeftExecute
      OnUpdate = actDbColumnLeftUpdate
    end
    object actDbColumnCenter: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1094#1077#1085#1090#1088#1091
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1094#1077#1085#1090#1088#1091
      OnExecute = actDbColumnCenterExecute
      OnUpdate = actDbColumnCenterUpdate
    end
    object actDbColumnRight: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1087#1088#1072#1074#1086#1084#1091' '#1082#1088#1072#1102
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1087#1088#1072#1074#1086#1084#1091' '#1082#1088#1072#1102
      OnExecute = actDbColumnRightExecute
      OnUpdate = actDbColumnRightUpdate
    end
    object actDbSelectAll: TAction
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077
      Enabled = False
      Hint = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077' '#1079#1072#1087#1080#1089#1080' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ShortCut = 16449
      OnExecute = actDbSelectAllExecute
      OnUpdate = actDbSelectAllUpdate
    end
    object actDbExportToExcel: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' Excel'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' Microsoft Excel'
      ImageIndex = 22
      ShortCut = 16453
      OnExecute = actDbExportToExcelExecute
      OnUpdate = actDbExportToExcelUpdate
    end
    object actDbExportToFileCsv: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' '#1092#1072#1081#1083' CSV'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' '#1092#1072#1081#1083' CSV ('#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083' '#1089' '#1088#1072#1079#1076#1077#1083#1080#1090#1077#1083#1103#1084#1080')'
      ImageIndex = 30
      ShortCut = 16467
      OnExecute = actDbExportToFileCsvExecute
      OnUpdate = actDbExportToFileCsvUpdate
    end
    object actDbCreateDynamicReport: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1055#1077#1095#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
      Enabled = False
      Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1090#1077#1082#1089#1090#1086#1074#1086#1075#1086' '#1086#1090#1095#1077#1090#1072' '#1076#1083#1103' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 23
      ShortCut = 16466
      OnExecute = actDbCreateDynamicReportExecute
      OnUpdate = actDbCreateDynamicReportUpdate
    end
    object actDbDataSetStatistic: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1079#1072#1087#1080#1089#1077#1081', '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1099#1084' '#1087#1086#1083#1103#1084
      ImageIndex = 45
      ShortCut = 16469
      OnExecute = actDbDataSetStatisticExecute
      OnUpdate = actDbDataSetStatisticUpdate
    end
    object actDbColumnStatistic: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1079#1072#1087#1080#1089#1077#1081', '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1087#1086#1083#1102
      ImageIndex = 45
      OnExecute = actDbColumnStatisticExecute
      OnUpdate = actDbColumnStatisticUpdate
    end
    object actDbDateSetGrouping: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1043#1088#1091#1087#1087#1080#1088#1086#1074#1082#1072
      Enabled = False
      Hint = 
        #1043#1088#1091#1087#1087#1080#1088#1086#1074#1082#1072' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1099#1084' '#1087#1086#1083#1103#1084' '#1089' '#1074#1086#1079#1084#1086#1078#1085#1086#1089#1090#1100#1102' '#1087#1086#1076#1089#1095#1077#1090#1072' '#1087#1086' '#1076#1088#1091#1075#1080#1084 +
        ' '#1087#1086#1083#1103#1084
      ImageIndex = 45
      ShortCut = 16473
      OnExecute = actDbDateSetGroupingExecute
      OnUpdate = actDbDateSetGroupingUpdate
    end
  end
  object RDbEditor: TRDbExportEditor
    AutoEdit = False
    CheckTags = False
    OpenMode = omAuto
    OnBeforeShowEditor = DbBeforeShowEditor
    OnGetNewKey = DbGetNewKey
    OnFreeNewKey = DbFreeNewKey
    OnCreateSetDefault = DbCreateSetDefault
    OnBeforeDelete = DbBeforeDelete
    OnSaveToLog = DbSaveToLog
    DbGrid = DbGrid
    Left = 36
    Top = 80
  end
  object RDbLocate: TRDbFind
    Options = [olUseHistory, olStoreLastFind, olStoreHistory]
    Left = 120
    Top = 80
  end
  object RDbSearch: TRDbSearch
    Options = [olUseHistory, olStoreLastFind, olStoreHistory]
    Left = 92
    Top = 80
  end
  object RDbGridTuner: TRDbGridTuner
    DbGrid = DbGrid
    Options = [ogStoreItemsState, ogChangeOptions]
    OnViewChange = RDbGridTunerViewChange
    Left = 204
    Top = 80
  end
  object RDbUpdater: TRDbUpdater
    Editor = RDbEditor
    Left = 64
    Top = 80
  end
  object RDbFilterStatus: TRDbFilterStatus
    DataSource = RDbEditor
    Options = [soRecordCount, soShowCaptions]
    RDbFilter = RDbFilter
    Panel_DbFilter = -1
    Left = 232
    Top = 80
  end
  object GridPopupMenu: TPopupMenu
    AutoPopup = False
    Images = BaseData.ImageList
    Left = 288
    Top = 80
    object itemDbInsertP: TMenuItem
      Action = actDbInsert
    end
    object itemDbCloneP: TMenuItem
      Action = actDbClone
    end
    object itemDbEditP: TMenuItem
      Action = actDbEdit
      Default = True
    end
    object itemDbDeleteP: TMenuItem
      Action = actDbDelete
    end
    object divGridEditP: TMenuItem
      Caption = '-'
    end
    object itemDbFindP: TMenuItem
      Action = actDbFind
    end
    object itemDbLocateP: TMenuItem
      Action = actDbLocate
    end
    object divGridNavP: TMenuItem
      Caption = '-'
    end
    object menuDbNavigationP: TMenuItem
      Caption = #1053#1072#1074#1080#1075#1072#1094#1080#1103' '#1087#1086' '#1090#1072#1073#1083#1080#1094#1077
      Hint = #1053#1072#1074#1080#1075#1072#1094#1080#1103' '#1087#1086' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 14
      object itemDbFirstP: TMenuItem
        Action = actDbFirst
      end
      object itemDbPriorP: TMenuItem
        Action = actDbPrior
      end
      object itemDbNextP: TMenuItem
        Action = actDbNext
      end
      object itemDbLastP: TMenuItem
        Action = actDbLast
      end
    end
    object menuDataFilterP: TMenuItem
      Caption = #1060#1080#1083#1100#1090#1088' '#1076#1072#1085#1085#1099#1093
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1086#1090#1073#1086#1088#1072' '#1076#1072#1085#1085#1099#1093' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 15
      Visible = False
      object itemDbFilterCustomP: TMenuItem
        Action = actDbFilterCustom
      end
      object itemDbFilterDefaultP: TMenuItem
        Action = actDbFilterDefault
      end
      object itemDbFilterSelectedP: TMenuItem
        Action = actDbFilterSelected
      end
      object itemDbFilterNoneP: TMenuItem
        Action = actDbFilterNone
      end
    end
    object menuDataSortP: TMenuItem
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072
      Hint = #1042#1099#1073#1086#1088' '#1085#1072#1087#1088#1072#1074#1083#1077#1085#1080#1103' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 18
      Visible = False
      object itemDbSortCustomP: TMenuItem
        Action = actDbSortCustom
      end
      object itemDbSortDefaultP: TMenuItem
        Action = actDbSortDefault
      end
    end
    object menuGridSetupP: TMenuItem
      Caption = #1055#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1080#1103
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 19
      Visible = False
      object itemDbGridTuneP: TMenuItem
        Action = actDbGridTune
      end
      object itemDbGridDefaultP: TMenuItem
        Action = actDbGridDefault
      end
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
      object itemDbExportToExcelP: TMenuItem
        Action = actDbExportToExcel
      end
      object itemactDbExportToFileCsvP: TMenuItem
        Action = actDbExportToFileCsv
      end
      object itemDbCreateDynamicReportP: TMenuItem
        Action = actDbCreateDynamicReport
      end
    end
    object menuGroupP: TMenuItem
      Caption = #1043#1088#1091#1087#1087#1080#1088#1086#1074#1082#1072
      Hint = #1043#1088#1091#1087#1087#1080#1088#1086#1074#1082#1072' '#1080' '#1089#1090#1072#1090#1080#1089#1090#1080#1082#1072
      ImageIndex = 45
      object itemDbDateSetGroupingP: TMenuItem
        Action = actDbDateSetGrouping
      end
      object itemactDbDataSetStatisticP: TMenuItem
        Action = actDbDataSetStatistic
      end
    end
    object divGridDataP: TMenuItem
      Caption = '-'
    end
    object itemDbAttachmentsP: TMenuItem
      Action = actDbAttachments
    end
    object divGridAttachmentsP: TMenuItem
      Caption = '-'
      Visible = False
    end
    object itemDbMultiSelectOnOffP: TMenuItem
      Action = actDbMultiSelectOnOff
    end
    object itemDbSetDefaultValuesP: TMenuItem
      Action = actDbSetDefaultValues
    end
    object itemDbSelectAllP: TMenuItem
      Action = actDbSelectAll
    end
    object divGridMultiP: TMenuItem
      Caption = '-'
    end
    object itemDbRefreshP: TMenuItem
      Action = actDbRefresh
    end
  end
  object TitlePopupMenu: TPopupMenu
    AutoPopup = False
    Images = BaseData.ImageList
    Left = 316
    Top = 80
    object itemDbFindColumnT: TMenuItem
      Action = actDbFindColumn
    end
    object divMenuTitleFind: TMenuItem
      Caption = '-'
    end
    object itemDbFilterCustomT: TMenuItem
      Action = actDbFilterCustom
    end
    object itemDbFilterDefaultT: TMenuItem
      Action = actDbFilterDefault
    end
    object itemDbFilterSelectedT: TMenuItem
      Action = actDbFilterSelected
    end
    object itemDbFilterNoneT: TMenuItem
      Action = actDbFilterNone
    end
    object divMenuTitleFilter: TMenuItem
      Caption = '-'
    end
    object itemDbSortColumnAscT: TMenuItem
      Action = actDbSortColumnAsc
    end
    object itemDbSortColumnDescT: TMenuItem
      Action = actDbSortColumnDesc
    end
    object itemDbSortCustomT: TMenuItem
      Action = actDbSortCustom
    end
    object divMenuTitleSort: TMenuItem
      Caption = '-'
    end
    object menuDbColumnAlignT: TMenuItem
      Caption = #1042#1099#1088#1072#1074#1085#1080#1074#1072#1085#1080#1077
      ImageIndex = 14
      object itemDbColumnLeftT: TMenuItem
        Action = actDbColumnLeft
      end
      object itemDbColumnCenterT: TMenuItem
        Action = actDbColumnCenter
      end
      object itemDbColumnRightT: TMenuItem
        Action = actDbColumnRight
      end
    end
    object itemDbGridTuneT: TMenuItem
      Action = actDbGridTune
    end
    object divMenuTitleView: TMenuItem
      Caption = '-'
    end
    object itemDbDateSetGrouping: TMenuItem
      Action = actDbDateSetGrouping
    end
    object itemDbColumnStatistic: TMenuItem
      Action = actDbColumnStatistic
    end
  end
  object RDbFilter: TRDbFilter
    DateFormatWhere = #39'mm.dd.yyyy'#39
    DateFormatFilter = 'dd.MM.yyyy'
    Options = [foStoreItemsState, foChangeOptions, foItemActivateOnChange]
    Left = 148
    Top = 80
  end
  object RDbOrder: TRDbOrder
    Options = [ooStoreItemsState, ooChangeOptions]
    Left = 176
    Top = 80
  end
  object ViewsPopupMenu: TPopupMenu
    Images = BaseData.ImageList
    Left = 344
    Top = 80
    object itemDbGridTuneV: TMenuItem
      Action = actDbGridTune
    end
  end
end
