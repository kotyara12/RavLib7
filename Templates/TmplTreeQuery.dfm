inherited TreeQueryTemplate: TTreeQueryTemplate
  Left = 398
  Top = 216
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
      ButtonWidth = 58
      object NewToolButton: TToolButton
        Left = 0
        Top = 0
        Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100
        Caption = #1057#1086#1079#1076#1072#1090#1100
        DropdownMenu = NewPopupMenu
        ImageIndex = 8
        PopupMenu = NewPopupMenu
      end
      object PropertiesToolButton: TToolButton
        Left = 58
        Top = 0
        Action = Properties
      end
      object DeleteItemToolButton: TToolButton
        Left = 116
        Top = 0
        Action = Delete
      end
      object SeparatorEdit: TToolButton
        Left = 174
        Top = 0
        Width = 8
        Caption = 'SeparatorEdit'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object FindRecordToolButton: TToolButton
        Left = 182
        Top = 0
        Action = Find
      end
      object SeparatorFind: TToolButton
        Left = 240
        Top = 0
        Width = 8
        Caption = 'SeparatorFind'
        ImageIndex = 13
        Style = tbsSeparator
      end
      object DataToolButton: TToolButton
        Left = 248
        Top = 0
        Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1076#1072#1085#1085#1099#1084#1080
        Caption = #1044#1072#1085#1085#1099#1077
        ImageIndex = 14
        OnClick = DataToolButtonClick
      end
      object OpersToolButton: TToolButton
        Left = 306
        Top = 0
        Hint = #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1076#1072#1085#1085#1099#1093' '#1080' '#1076#1086#1089#1090#1091#1087' '#1082' '#1076#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1086#1081' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1080
        Caption = #1054#1087#1077#1088#1072#1094#1080#1080
        ImageIndex = 24
        Visible = False
        OnClick = OpersToolButtonClick
      end
      object ReportsToolButton: TToolButton
        Left = 364
        Top = 0
        Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1086#1090#1095#1077#1090#1086#1074', '#1087#1077#1095#1072#1090#1100' '#1080' '#1101#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093
        Caption = #1054#1090#1095#1077#1090#1099
        ImageIndex = 23
        OnClick = ReportsToolButtonClick
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
        ImageIndex = 2
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
        Width = 481
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
        Left = 488
        Top = 4
        Width = 107
        Height = 21
        Action = FindFast
        Caption = #1060#1080#1083#1100#1090#1088
        TabOrder = 1
        Glyph.Data = {
          36080000424D3608000000000000360000002800000020000000100000000100
          2000000000000008000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF004A4A4A004A4A4A004A4A4A004A4A
          4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF004A4A4A004A4A4A004A4A4A004A4A
          4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF004A4A4A0084840000848400004A4A
          4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF004A4A4A0058585800585858004A4A
          4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF004A4A4A00CECECE00BDBDBD004A4A
          4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF004A4A4A00CECECE00BDBDBD004A4A
          4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF004A4A4A00CECECE00BDBDBD004A4A
          4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF004A4A4A00CECECE00BDBDBD004A4A
          4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF004A4A4A00C6C6C600D6D6D600BDBDBD009494
          94004A4A4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF004A4A4A00C6C6C600D6D6D600BDBDBD009494
          94004A4A4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF004A4A4A00BDBDBD00CECECE00D6D6D600C6C6C600A5A5
          A500736B73004A4A4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF004A4A4A00BDBDBD00CECECE00D6D6D600C6C6C600A5A5
          A5006F6F6F004A4A4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF005A5A5A00CECECE00DEDEDE00DEDEDE00DEDEDE00C6C6C600B5B5
          B50084848400736B73004A4A4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF005A5A5A00CECECE00DEDEDE00DEDEDE00DEDEDE00C6C6C600B5B5
          B500848484006F6F6F004A4A4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF005A5A5A007B7B7B00FFFFFF00F7F7F700E7E7E700D6D6D600CECECE00B5AD
          B5009C949C007B7B7B00525252004A4A4A00FF00FF00FF00FF00FF00FF00FF00
          FF005A5A5A007B7B7B00FFFFFF00F7F7F700E7E7E700D6D6D600CECECE00B1B1
          B100989898007B7B7B00525252004A4A4A00FF00FF00FF00FF00FF00FF00FF00
          FF0084848400848484007B7B7B007B7B7B006363630063636300636363005A5A
          5A005A5A5A005A5A5A005A5A5A004A4A4A00FF00FF00FF00FF00FF00FF00FF00
          FF0084848400848484007B7B7B007B7B7B006363630063636300636363005A5A
          5A005A5A5A005A5A5A005A5A5A004A4A4A00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        NumGlyphs = 2
      end
      object btnFastFindClear: TBitBtn
        Left = 596
        Top = 4
        Width = 21
        Height = 21
        Action = FindFastClear
        TabOrder = 2
        Glyph.Data = {
          36080000424D3608000000000000360000002800000020000000100000000100
          2000000000000008000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF001E37EB00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF001D26AF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0070707000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0052525200FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF005668F3001030FF001D32FC00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF003C44BC000018C0000E1AAF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00949494007171710072727200FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF006B6B6B004D4D4D004A4A4A00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF002C51FC001030FF000028FF00FF00FF00FF00FF00FF00FF001D2E
          BF000018D0000E22BE00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0085858500717171006B6B6B00FF00FF00FF00FF00FF00FF005C5C
          5C005252520053535300FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF002C51FC001030FF000E2BFB00FF00FF001E2FCD000020
          E0000E22CE00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF0085858500717171006C6C6C00FF00FF00616161005C5C
          5C0059595900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF005564F5001038FF000020F0000E2AED002D3A
          CB00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00929292007575750061616100676767006868
          6800FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF002048FF001030FF000E2AED00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00808080007171710067676700FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF004060FF003050FF002C49FB001038FF000020
          F000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00919191008686860081818100757575006161
          6100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF004060FF004058FF004B6FFB00FF00FF004A67FB002040
          FF000020F000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00919191008C8C8C0099999900FF00FF00959595007C7C
          7C0061616100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF005070FF005078FF006F84F500FF00FF00FF00FF00FF00FF004A66
          FA003048FF000020F000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF009C9C9C00A0A0A000A7A7A700FF00FF00FF00FF00FF00FF009494
          94008282820061616100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF006078FF006078FF00697EFC00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF004A66FA003050FF000028FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00A2A2A200A2A2A200A5A5A500FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF0094949400868686006B6B6B00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF006985FC00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00556CF400FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00A9A9A900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF0096969600FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
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
      Caption = #1060#1080#1083#1100#1090#1088
      Hint = #1041#1099#1089#1090#1088#1099#1081' '#1092#1080#1083#1100#1090#1088' '#1087#1086' '#1074#1089#1077#1084' '#1087#1086#1083#1103#1084
      ImageIndex = 13
      OnExecute = FindFastExecute
      OnUpdate = FindFastUpdate
    end
    object FindFastClear: TAction [8]
      Category = #1044#1072#1085#1085#1099#1077
      Hint = #1054#1095#1080#1089#1090#1080#1090#1100' '#1073#1099#1089#1090#1088#1099#1081' '#1092#1080#1083#1100#1090#1088
      ImageIndex = 17
      OnExecute = FindFastClearExecute
      OnUpdate = FindFastClearUpdate
    end
    object DataSetFirst: TDataSetFirst [9]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042' '#1085#1072#1095#1072#1083#1086
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1085#1072#1095#1072#1083#1086' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 4
      ShortCut = 36
      DataSource = RDbEditor
    end
    object DataSetPrior: TDataSetPrior [10]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1053#1072#1079#1072#1076
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1085#1072' '#1087#1088#1077#1076#1099#1076#1091#1097#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 5
      DataSource = RDbEditor
    end
    object DataSetNext: TDataSetNext [11]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042#1087#1077#1088#1077#1076
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1085#1072' '#1089#1083#1077#1076#1091#1102#1097#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 6
      DataSource = RDbEditor
    end
    object DataSetLast: TDataSetLast [12]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042' '#1082#1086#1085#1077#1094
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1082#1086#1085#1077#1094' '#1090#1072#1073#1083#1080#1094#1099
      ImageIndex = 7
      ShortCut = 35
      DataSource = RDbEditor
    end
    object DbGridSetup: TAction [13]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042#1099#1073#1086#1088' '#1089#1090#1086#1083#1073#1094#1086#1074'...'
      Enabled = False
      Hint = #1042#1099#1073#1086#1088' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1093' '#1089#1090#1086#1083#1073#1094#1086#1074' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 19
      OnExecute = DbGridSetupExecute
      OnUpdate = DbGridSetupUpdate
    end
    object DbGridDefault: TAction [14]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1090#1086#1083#1073#1094#1099' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1089#1090#1086#1083#1073#1094#1099' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'" ('#1074#1089#1077' '#1074#1080#1076#1080#1084#1099#1077')'
      ImageIndex = 20
      OnExecute = DbGridDefaultExecute
      OnUpdate = DbGridDefaultUpdate
    end
    object ColumnLeft: TAction [15]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1083#1077#1074#1086#1084#1091' '#1082#1088#1072#1102
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1083#1077#1074#1086#1084#1091' '#1082#1088#1072#1102
      OnExecute = ColumnLeftExecute
      OnUpdate = ColumnLeftUpdate
    end
    object ColumnCenter: TAction [16]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1094#1077#1085#1090#1088#1091
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1094#1077#1085#1090#1088#1091
      OnExecute = ColumnCenterExecute
      OnUpdate = ColumnCenterUpdate
    end
    object ColumnRight: TAction [17]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1087#1088#1072#1074#1086#1084#1091' '#1082#1088#1072#1102
      Enabled = False
      Hint = #1042#1099#1088#1086#1074#1085#1103#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080' '#1079#1072#1075#1086#1083#1086#1074#1086#1082' '#1089#1090#1086#1083#1073#1094#1072' '#1087#1086' '#1087#1088#1072#1074#1086#1084#1091' '#1082#1088#1072#1102
      OnExecute = ColumnRightExecute
      OnUpdate = ColumnRightUpdate
    end
    inherited NewGroup: TAction [18]
    end
    inherited NewSubGroup: TAction [19]
    end
    inherited NewItem: TAction [20]
    end
    inherited NewRecord: TAction [21]
      OnExecute = NewRecordExecute
      OnUpdate = NewRecordUpdate
    end
    inherited CopyRecord: TAction [22]
    end
    inherited Properties: TAction [23]
    end
    object FilterUser: TAction [24]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103'...'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1087#1088#1086#1080#1079#1074#1086#1083#1100#1085#1086#1075#1086' '#1092#1080#1083#1100#1090#1088#1072' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 15
      ShortCut = 119
      OnExecute = FilterUserExecute
      OnUpdate = FilterUserUpdate
    end
    object FilterDefault: TAction [25]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1092#1080#1083#1100#1090#1088#1072' '#1076#1072#1085#1085#1099#1093' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      ImageIndex = 16
      ShortCut = 16503
      OnExecute = FilterDefaultExecute
      OnUpdate = FilterDefaultUpdate
    end
    object FilterSelected: TAction [26]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1060#1080#1083#1100#1090#1088' "'#1087#1086' '#1074#1099#1076#1077#1083#1077#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' '#1087#1086' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1084' '#1079#1072#1087#1080#1089#1103#1084
      ImageIndex = 16
      ShortCut = 49271
      OnExecute = FilterSelectedExecute
      OnUpdate = FilterSelectedUpdate
    end
    object FilterNone: TAction [27]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1041#1077#1079' '#1092#1080#1083#1100#1090#1088#1072
      Enabled = False
      Hint = #1054#1090#1082#1083#1102#1095#1080#1090#1100' '#1092#1080#1083#1100#1090#1088' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 17
      ShortCut = 32887
      OnExecute = FilterNoneExecute
      OnUpdate = FilterNoneUpdate
    end
    object SortUser: TAction [28]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072'...'
      Enabled = False
      Hint = #1042#1099#1073#1086#1088' '#1089#1090#1086#1083#1073#1094#1086#1074' '#1076#1083#1103' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ImageIndex = 18
      ShortCut = 120
      OnExecute = SortUserExecute
      OnUpdate = SortUserUpdate
    end
    object SortDefault: TAction [29]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1082#1072' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080' '#1076#1072#1085#1085#1099#1093' "'#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102'"'
      ImageIndex = 18
      ShortCut = 16504
      OnExecute = SortDefaultExecute
      OnUpdate = SortDefaultUpdate
    end
    object SetCurrOrderAsc: TAction [30]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1087#1086' '#1074#1086#1079#1088#1072#1089#1090#1072#1085#1080#1102' '#1074' '#1076#1072#1085#1085#1086#1084' '#1089#1090#1086#1083#1073#1094#1077
      OnExecute = SetCurrOrderAscExecute
      OnUpdate = SetCurrOrderAscUpdate
    end
    object SetCurrOrderDesc: TAction [31]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086' '#1091#1073#1099#1074#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1087#1086' '#1091#1073#1099#1074#1072#1085#1080#1102' '#1074' '#1076#1072#1085#1085#1086#1084' '#1089#1090#1086#1083#1073#1094#1077
      OnExecute = SetCurrOrderDescExecute
      OnUpdate = SetCurrOrderDescUpdate
    end
    inherited Move: TAction [32]
    end
    inherited TreeSort_None: TAction [33]
    end
    inherited AboutBox: TAction [34]
    end
    inherited TreeSort_Id: TAction [35]
    end
    inherited TreeSort_TypeId: TAction [36]
    end
    inherited TreeSort_Name: TAction [37]
    end
    inherited TreeSort_TypeName: TAction [38]
    end
    inherited EM_None: TAction [39]
    end
    inherited EM_Root: TAction [40]
    end
    object ImportDS: TAction [41]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072'...'
      Enabled = False
      Hint = #1052#1072#1089#1090#1077#1088' '#1080#1084#1087#1086#1088#1090#1072' '#1076#1072#1085#1085#1099#1093' '#1080#1079' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
      ImageIndex = 31
      OnExecute = ImportDSExecute
      OnUpdate = ImportDSUpdate
    end
    inherited TreeVisible: TAction [42]
    end
    inherited EM_Groups: TAction [43]
    end
    inherited EM_All: TAction [44]
    end
    inherited SelectSaveEM: TAction [45]
    end
    inherited SaveTreePosition: TAction [46]
    end
    inherited ExpandAll: TAction [47]
    end
    inherited CollapseAll: TAction [48]
    end
    inherited ShowHelp: TAction [49]
    end
    inherited TreeSort_IndexSort: TAction [50]
    end
    inherited NewSubitem: TAction [51]
    end
    inherited ExpandNode: TAction [52]
    end
    object DataSetExportToExcel: TAction [56]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' Excel'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' Microsoft Excel'
      ImageIndex = 22
      ShortCut = 16453
      OnExecute = DataSetExportToExcelExecute
      OnUpdate = DataSetExportToExcelUpdate
    end
    object DataSetExportToFileCsv: TAction [57]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074' '#1092#1072#1081#1083' CSV'
      Enabled = False
      Hint = #1069#1082#1089#1087#1086#1088#1090' '#1076#1072#1085#1085#1099#1093' '#1074' '#1092#1072#1081#1083' CSV ('#1090#1077#1082#1089#1090#1086#1074#1099#1081' '#1092#1072#1081#1083' '#1089' '#1088#1072#1079#1076#1077#1083#1080#1090#1077#1083#1103#1084#1080')'
      ImageIndex = 30
      ShortCut = 16467
      OnExecute = DataSetExportToFileCsvExecute
      OnUpdate = DataSetExportToFileCsvUpdate
    end
    object DataSetCreateDynamicReport: TAction [58]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1055#1077#1095#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
      Enabled = False
      Hint = #1043#1077#1085#1077#1088#1072#1094#1080#1103' '#1090#1077#1082#1089#1090#1086#1074#1086#1075#1086' '#1086#1090#1095#1077#1090#1072' '#1076#1083#1103' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 23
      ShortCut = 16466
      OnExecute = DataSetCreateDynamicReportExecute
      OnUpdate = DataSetCreateDynamicReportUpdate
    end
    object DataSetStatistic: TAction [59]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1079#1072#1087#1080#1089#1077#1081', '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1099#1084' '#1087#1086#1083#1103#1084
      ImageIndex = 45
      ShortCut = 16469
      OnExecute = DataSetStatisticExecute
      OnUpdate = DataSetStatisticUpdate
    end
    object Attachments: TAction [60]
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
    object MultiSelectOnOff: TAction [61]
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
    object ColumnStatistic: TAction [62]
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1057#1090#1072#1090#1080#1089#1090#1080#1082#1072
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1079#1072#1087#1080#1089#1077#1081', '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1087#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1087#1086#1083#1102
      ImageIndex = 45
      OnExecute = ColumnStatisticExecute
      OnUpdate = ColumnStatisticUpdate
    end
    object SelectAll: TAction [63]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077
      Enabled = False
      Hint = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077' '#1079#1072#1087#1080#1089#1080' '#1074' '#1090#1072#1073#1083#1080#1094#1077
      ShortCut = 16449
      OnExecute = SelectAllExecute
      OnUpdate = SelectAllUpdate
    end
    object SelectDefaultValues: TAction [64]
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
    object DateSetGrouping: TAction [65]
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
    ParentBiDiMode = False
    OnPopup = PopupMenuPopup
    object itemDbLocateP: TMenuItem [7]
      Action = DbLocate
    end
    inherited divPopupSubmenus: TMenuItem
      Visible = True
    end
    object menuNavigationP: TMenuItem [9]
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
    object menuFilterDataP: TMenuItem [10]
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
    object menuSortDataP: TMenuItem [11]
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
    object menuViewsDataP: TMenuItem [12]
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1090#1072#1073#1083#1080#1094#1099
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
      object itemDataSetCreateDynamicReportP: TMenuItem
        Action = DataSetCreateDynamicReport
      end
      object itemDataSetExportToExcelP: TMenuItem
        Action = DataSetExportToExcel
      end
      object itemDataSetExportToFileP: TMenuItem
        Action = DataSetExportToFileCsv
      end
    end
    object menuGroupsP: TMenuItem [15]
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
    object itemAttachmentsP: TMenuItem [16]
      Action = Attachments
    end
    object divPopupMultiedit: TMenuItem [20]
      Caption = '-'
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
      object divAttach: TMenuItem [11]
        Caption = '-'
        Visible = False
      end
      object itemAttachments: TMenuItem [12]
        Action = Attachments
      end
      object divEditImport: TMenuItem [13]
        Caption = '-'
      end
      object itemImportDS: TMenuItem [14]
        Action = ImportDS
      end
      object divEditMulti: TMenuItem [15]
        Caption = '-'
        Visible = False
      end
      object itemSelectDefaultValues: TMenuItem [16]
        Action = SelectDefaultValues
      end
      object itemMultiSelectOnOff: TMenuItem [17]
        Action = MultiSelectOnOff
      end
      object itemSelectAll: TMenuItem [18]
        Action = SelectAll
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
      object divDataTree: TMenuItem [8]
        Caption = '-'
      end
      object divDataFilter: TMenuItem [13]
        Caption = '-'
      end
      object itemFilterUser: TMenuItem [14]
        Action = FilterUser
      end
      object itemFilterDefault: TMenuItem [15]
        Action = FilterDefault
      end
      object itemFilterSelected: TMenuItem [16]
        Action = FilterSelected
      end
      object itemFilterNone: TMenuItem [17]
        Action = FilterNone
      end
      object divDataSort: TMenuItem [18]
        Caption = '-'
      end
      object itemSortUser: TMenuItem [19]
        Action = SortUser
      end
      object itemSortDefault: TMenuItem [20]
        Action = SortDefault
      end
      object divDataGrid: TMenuItem [21]
        Caption = '-'
      end
      object itemDbGridSetup: TMenuItem [22]
        Action = DbGridSetup
      end
      object itemDbGridDefault: TMenuItem [23]
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
    AutoPopup = False
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
    object divDataDTree: TMenuItem [4]
      Caption = '-'
    end
    object divDataDFilter: TMenuItem [8]
      Caption = '-'
    end
    object itemSortUserD: TMenuItem [9]
      Action = SortUser
    end
    object itemSortDefaultD: TMenuItem [10]
      Action = SortDefault
    end
    object divDataDSort: TMenuItem [11]
      Caption = '-'
    end
    object itemDbGridSetupD: TMenuItem [12]
      Action = DbGridSetup
    end
    object itemDbGridDefaultD: TMenuItem [13]
      Action = DbGridDefault
    end
  end
  inherited OperationsPopupMenu: TPopupMenu
    AutoPopup = False
  end
  inherited ReportsPopupMenu: TPopupMenu
    AutoPopup = False
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
