inherited TreeDetailTemplate: TTreeDetailTemplate
  Left = 378
  Top = 281
  Width = 792
  Height = 531
  ActiveControl = DataPanel
  Caption = 'TreeDetailTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter [0]
    Left = 221
    Top = 29
    Height = 424
  end
  inherited StatusBar: TStatusBar
    Top = 453
    Width = 776
  end
  inherited CoolBar: TCoolBar
    Width = 776
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        Width = 776
      end>
    inherited ToolBar: TToolBar
      Width = 763
    end
  end
  object TreePanel: TPanel [3]
    Left = 0
    Top = 29
    Width = 221
    Height = 424
    Align = alLeft
    BevelOuter = bvNone
    BorderStyle = bsSingle
    TabOrder = 2
    object TreeBevel: TBevel
      Left = 0
      Top = 17
      Width = 217
      Height = 2
      Align = alTop
      Shape = bsTopLine
    end
    object TreeHeaderPanel: TPanel
      Left = 0
      Top = 0
      Width = 217
      Height = 17
      Align = alTop
      Caption = #1055#1072#1087#1082#1080
      TabOrder = 0
      object TreeButtonPanel: TPanel
        Left = 198
        Top = 1
        Width = 18
        Height = 15
        Hint = #1047#1072#1082#1088#1099#1090#1100' '#1087#1072#1085#1077#1083#1100
        Align = alRight
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        object TreeCloseBtn: TSpeedButton
          Left = 0
          Top = 0
          Width = 18
          Height = 17
          Hint = #1047#1072#1082#1088#1099#1090#1100' '#1087#1072#1085#1077#1083#1100
          Flat = True
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888800888880
            0888888880088800888888888800800888888888888000888888888888800088
            8888888888008008888888888008880088888888008888800888888888888888
            8888888888888888888888888888888888888888888888888888}
          Layout = blGlyphBottom
          OnClick = TreePanelHide
        end
      end
    end
    object TreeView: TRTreeView
      Left = 0
      Top = 19
      Width = 217
      Height = 401
      Align = alClient
      BorderStyle = bsNone
      Ctl3D = True
      HideSelection = False
      Indent = 19
      ParentCtl3D = False
      PopupMenu = TreePopupMenu
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      OnChange = TreeViewChange
      OnDblClick = TreeViewDblClick
      OnDeletion = TreeViewDeletion
      ListEmptyValue = '-1'
      ListDelimChar = ','
    end
  end
  object DataPanel: TPanel [4]
    Left = 224
    Top = 29
    Width = 552
    Height = 424
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  inherited ActionList: TActionList
    inherited Find: TAction [0]
      ShortCut = 16454
    end
    object FindTree: TAction [1]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1055#1086#1080#1089#1082' '#1074' '#1087#1072#1087#1082#1072#1093
      Enabled = False
      Hint = #1055#1086#1080#1089#1082' '#1079#1072#1087#1080#1089#1080' '#1074' '#1089#1090#1088#1091#1082#1090#1091#1088#1077' '#1076#1072#1085#1085#1099#1093
      ImageIndex = 13
      ShortCut = 24646
      OnExecute = FindTreeExecute
      OnUpdate = FindTreeUpdate
    end
    inherited CloseSelect: TAction [2]
    end
    inherited CloseCancel: TAction [3]
    end
    object TreeSort_None: TAction [4]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1073#1077#1079' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080
      Enabled = False
      Hint = #1053#1077' '#1091#1087#1086#1088#1103#1076#1086#1095#1080#1074#1072#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1072#1087#1086#1082
      OnExecute = TreeSort_NoneExecute
      OnUpdate = TreeSort_NoneUpdate
    end
    object TreeSort_Id: TAction [5]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088#1091
      Enabled = False
      Hint = 
        #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1086' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088#1091' '#1079#1072#1087#1080#1089#1080' '#1076#1083#1103' '#1074#1089#1077#1093' '#1101#1083#1077#1084#1077#1085#1090#1086 +
        #1074
      ShortCut = 24644
      OnExecute = TreeSort_IdExecute
      OnUpdate = TreeSort_IdUpdate
    end
    object TreeSort_TypeId: TAction [6]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1090#1080#1087#1091' '#1080' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088#1091
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1086' '#1090#1080#1087#1091' '#1101#1083#1077#1084#1077#1085#1090#1072' '#1080' '#1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088#1091' '#1079#1072#1087#1080#1089#1080
      ShortCut = 24659
      OnExecute = TreeSort_TypeIdExecute
      OnUpdate = TreeSort_TypeIdUpdate
    end
    object TreeSort_Name: TAction [7]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1087#1086' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1102' '#1079#1072#1087#1080#1089#1080' '#1076#1083#1103' '#1074#1089#1077#1093' '#1101#1083#1077#1084#1077#1085#1090#1086#1074
      ShortCut = 41028
      OnExecute = TreeSort_NameExecute
      OnUpdate = TreeSort_NameUpdate
    end
    object TreeSort_TypeName: TAction [8]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1090#1080#1087#1091' '#1080' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1102
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1086' '#1090#1080#1087#1091' '#1101#1083#1077#1084#1077#1085#1090#1072' '#1080' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1102' '#1079#1072#1087#1080#1089#1080
      ShortCut = 41043
      OnExecute = TreeSort_TypeNameExecute
      OnUpdate = TreeSort_TypeNameUpdate
    end
    object TreeSort_IndexSort: TAction [9]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1087#1086' '#1080#1085#1076#1077#1082#1089#1091
      Enabled = False
      Hint = #1059#1087#1086#1088#1103#1076#1086#1095#1080#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1087#1086' '#1080#1085#1076#1077#1082#1089#1091' '#1079#1072#1087#1080#1089#1080
      OnExecute = TreeSort_IndexSortExecute
      OnUpdate = TreeSort_IndexSortUpdate
    end
    object EM_None: TAction [10]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1085#1077' '#1088#1072#1079#1074#1086#1088#1072#1095#1080#1074#1072#1090#1100
      Hint = #1053#1077' '#1088#1072#1079#1074#1086#1088#1072#1095#1080#1074#1072#1090#1100' '#1089#1090#1088#1091#1082#1090#1091#1088#1091' '#1079#1072#1087#1080#1089#1077#1081
      ShortCut = 41009
      OnExecute = EM_NoneExecute
      OnUpdate = EM_NoneUpdate
    end
    object EM_Root: TAction [11]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1088#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1075#1083#1072#1074#1085#1091#1102' '#1074#1077#1090#1074#1100
      Hint = 
        #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080' '#1090#1086#1083#1100#1082#1086' '#1074' '#1082#1086#1088#1085#1077#1074#1086#1081' '#1087#1072#1087#1082#1077' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1076#1072 +
        #1085#1085#1099#1093
      ShortCut = 41010
      OnExecute = EM_RootExecute
      OnUpdate = EM_RootUpdate
    end
    object EM_Groups: TAction [12]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1088#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077' '#1074#1077#1090#1074#1080
      Hint = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077' '#1086#1089#1085#1086#1074#1085#1099#1077' '#1087#1072#1087#1082#1080' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1076#1072#1085#1085#1099#1093
      ShortCut = 41011
      OnExecute = EM_GroupsExecute
      OnUpdate = EM_GroupsUpdate
    end
    object EM_All: TAction [13]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1088#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1102' '#1089#1090#1088#1091#1082#1090#1091#1088#1091
      Hint = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1077' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1087#1072#1087#1082#1080' '#1080' '#1101#1083#1077#1084#1077#1085#1090#1099' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1076#1072#1085#1085#1099#1093
      ShortCut = 41012
      OnExecute = EM_AllExecute
      OnUpdate = EM_AllUpdate
    end
    object SelectSaveEM: TAction [14]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1047#1072#1087#1086#1084#1080#1085#1072#1090#1100' '#1088#1077#1078#1080#1084' '#1079#1072#1075#1088#1091#1079#1082#1080
      Hint = 
        #1042#1086#1089#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1090#1077#1082#1091#1097#1080#1081' '#1088#1077#1078#1080#1084' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1087#1072#1087#1086#1082' '#1087#1088#1080' '#1089#1083#1077#1076#1091#1102#1097#1077 +
        #1084' '#1074#1099#1079#1086#1074#1077' '#1092#1086#1088#1084#1099
      ShortCut = 41035
      OnExecute = SelectSaveEMExecute
      OnUpdate = SelectSaveEMUpdate
    end
    inherited ShowHelp: TAction [15]
    end
    inherited AboutBox: TAction [16]
    end
    object TreeVisible: TAction [17]
      Category = #1042#1080#1076
      Caption = #1055#1072#1085#1077#1083#1100' '#1087#1072#1087#1086#1082
      Enabled = False
      Hint = #1057#1082#1088#1099#1090#1100' / '#1087#1086#1082#1072#1079#1072#1090#1100' '#1087#1072#1085#1077#1083#1100' '#1087#1072#1087#1086#1082
      ShortCut = 16468
      OnExecute = TreeVisibleExecute
      OnUpdate = TreeVisibleUpdate
    end
    object SaveTreePosition: TAction [18]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1047#1072#1087#1086#1084#1080#1085#1072#1090#1100' '#1090#1077#1082#1091#1097#1091#1102' '#1087#1086#1079#1080#1094#1080#1102
      Hint = 
        #1042#1086#1089#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1087#1086#1079#1080#1094#1080#1102' '#1074' '#1089#1090#1088#1091#1082#1090#1091#1088#1077' '#1087#1072#1087#1086#1082' '#1087#1088#1080' '#1089#1083#1077#1076#1091#1102#1097#1077#1084' '#1074#1099#1079#1086#1074#1077' '#1092#1086#1088#1084 +
        #1099
      ShortCut = 41036
      OnExecute = SaveTreePositionExecute
      OnUpdate = SaveTreePositionUpdate
    end
    object ExpandAll: TAction [19]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1105
      Enabled = False
      Hint = #1055#1086#1082#1072#1079#1072#1090#1100' '#1074#1089#1077' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1080#1077' '#1101#1083#1077#1084#1077#1085#1090#1099
      OnExecute = ExpandAllExecute
      OnUpdate = ExpandAllUpdate
    end
    object ExpandNode: TAction [20]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1056#1072#1079#1074#1077#1088#1085#1091#1090#1100
      Enabled = False
      Hint = #1055#1086#1082#1072#1079#1072#1090#1100' '#1074#1089#1077' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1101#1083#1077#1084#1077#1085#1090#1099' '#1076#1083#1103' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ShortCut = 24657
      OnExecute = ExpandNodeExecute
      OnUpdate = ExpandNodeUpdate
    end
    object CollapseAll: TAction [21]
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1074#1077#1088#1085#1091#1090#1100' '#1074#1089#1105
      Enabled = False
      Hint = #1057#1082#1088#1099#1090#1100' '#1074#1089#1077' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1101#1083#1077#1084#1077#1085#1090#1099' '#1089#1090#1088#1091#1082#1090#1091#1088#1099
      OnExecute = CollapseAllExecute
      OnUpdate = CollapseAllUpdate
    end
    object NewGroup: TAction [22]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1087#1072#1087#1082#1091
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1087#1072#1087#1082#1091
      ImageIndex = 8
      ShortCut = 16462
      OnExecute = NewGroupExecute
      OnUpdate = NewGroupUpdate
    end
    object NewSubGroup: TAction [23]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1087#1086#1076#1087#1072#1087#1082#1091
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1087#1086#1076#1087#1072#1087#1082#1091' '#1074' '#1090#1077#1082#1091#1097#1077#1081' '#1087#1072#1087#1082#1077
      ImageIndex = 8
      ShortCut = 24654
      OnExecute = NewSubGroupExecute
      OnUpdate = NewSubGroupUpdate
    end
    object NewItem: TAction [24]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1101#1083#1077#1084#1077#1085#1090
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1099#1081' '#1101#1083#1077#1084#1077#1085#1090
      ImageIndex = 8
      ShortCut = 41038
      Visible = False
      OnExecute = NewItemExecute
      OnUpdate = NewItemUpdate
    end
    object NewSubitem: TAction [25]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1087#1086#1076#1101#1083#1077#1084#1077#1085#1090
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1086#1076#1101#1083#1077#1084#1077#1085#1090
      ImageIndex = 8
      Visible = False
      OnExecute = NewSubitemExecute
      OnUpdate = NewSubitemUpdate
    end
    object CopyRecord: TAction [26]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1050#1083#1086#1085#1080#1088#1086#1074#1072#1090#1100
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100' '#1080' '#1089#1082#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080#1079' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 8
      ShortCut = 16459
      OnExecute = CopyRecordExecute
      OnUpdate = CopyRecordUpdate
    end
    object NewRecord: TAction [27]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1079#1072#1087#1080#1089#1100
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100
      ImageIndex = 8
      ShortCut = 16429
    end
    object Properties: TAction [28]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100
      Enabled = False
      Hint = #1048#1079#1084#1077#1085#1080#1090#1100' '#1080#1083#1080' '#1087#1088#1086#1089#1084#1086#1090#1088#1077#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 9
      ShortCut = 16397
      OnExecute = PropertiesExecute
      OnUpdate = PropertiesUpdate
    end
    object Move: TAction [29]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1055#1077#1088#1077#1084#1077#1089#1090#1080#1090#1100
      Enabled = False
      Hint = #1055#1077#1088#1077#1084#1077#1089#1090#1080#1090#1100' '#1079#1072#1087#1080#1089#1100' '#1074' '#1076#1088#1091#1075#1091#1102' '#1075#1088#1091#1087#1087#1091
      ImageIndex = 11
      ShortCut = 16461
      OnExecute = MoveExecute
      OnUpdate = MoveUpdate
    end
    object Delete: TAction [30]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Enabled = False
      Hint = #1059#1076#1072#1083#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 10
      ShortCut = 16430
      OnExecute = DeleteExecute
      OnUpdate = DeleteUpdate
    end
    inherited Refresh: TAction [31]
    end
    object TreeRootOnly: TAction
      Category = #1042#1080#1076
      Caption = #1042#1082#1083#1102#1095#1072#1103' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080
      Enabled = False
      Hint = 
        #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1079#1072#1087#1080#1089#1080' '#1074#1099#1073#1088#1072#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080' '#1076#1077#1088#1077#1074#1072' '#1080' '#1074#1089#1077#1093' '#1077#1077' '#1074#1083#1086#1078#1077#1085#1085#1099#1093' '#1101#1083 +
        #1077#1084#1077#1085#1090#1086#1074
      OnExecute = TreeRootOnlyExecute
      OnUpdate = TreeRootOnlyUpdate
    end
    object CollapseNode: TAction
      Category = #1044#1072#1085#1085#1099#1077
      Caption = #1057#1074#1077#1088#1085#1091#1090#1100
      Enabled = False
      Hint = #1057#1082#1088#1099#1090#1100' '#1074#1083#1086#1078#1077#1085#1085#1099#1077' '#1101#1083#1077#1084#1077#1085#1090#1099' '#1076#1083#1103' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ShortCut = 24663
      OnExecute = CollapseNodeExecute
      OnUpdate = CollapseNodeUpdate
    end
  end
  inherited PopupMenu: TPopupMenu
    object itemNewRecP: TMenuItem [0]
      Action = NewRecord
      Caption = #1057#1086#1079#1076#1072#1090#1100
    end
    object itemCopyRecP: TMenuItem [1]
      Action = CopyRecord
    end
    object itemPropRecP: TMenuItem [2]
      Action = Properties
      Default = True
    end
    object itemMoveItemP: TMenuItem [3]
      Action = Move
    end
    object itemDeleteItemP: TMenuItem [4]
      Action = Delete
    end
    object divPopupEdit: TMenuItem [5]
      Caption = '-'
    end
    object divPopupTree: TMenuItem [10]
      Caption = '-'
    end
    object itemTreeVisibleP: TMenuItem [11]
      Action = TreeVisible
    end
    object itemTreeRootOnlyP: TMenuItem [12]
      Action = TreeRootOnly
    end
  end
  inherited MainMenu: TMainMenu
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
      object itemNewSubitem: TMenuItem [3]
        Action = NewSubitem
      end
      object itemNewRecord: TMenuItem [4]
        Action = NewRecord
      end
      object itemCopyRecord: TMenuItem [5]
        Action = CopyRecord
      end
      object divEditNew: TMenuItem [6]
        Caption = '-'
      end
      object itemProperties: TMenuItem [7]
        Action = Properties
      end
      object itemDelete: TMenuItem [8]
        Action = Delete
      end
      object divEditEdit: TMenuItem [9]
        Caption = '-'
      end
      object itemMove: TMenuItem [10]
        Action = Move
      end
      object divEditRefresh: TMenuItem [11]
        Caption = '-'
      end
    end
    inherited menuData: TMenuItem
      object itemFindTree: TMenuItem
        Action = FindTree
      end
      object divDataFind: TMenuItem
        Caption = '-'
      end
      object itemTreeVisible: TMenuItem
        Action = TreeVisible
      end
      object menuSortTree: TMenuItem
        Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1087#1072#1087#1086#1082
        Hint = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1087#1072#1087#1086#1082' ('#1075#1088#1091#1087#1087' '#1079#1072#1087#1080#1089#1077#1081')'
        ImageIndex = 18
        object itemTreeSort_None: TMenuItem
          Action = TreeSort_None
        end
        object itemTreeSort_Id: TMenuItem
          Action = TreeSort_Id
        end
        object itemTreeSort_TypeId: TMenuItem
          Action = TreeSort_TypeId
        end
        object itemTreeSort_Name: TMenuItem
          Action = TreeSort_Name
        end
        object itemTreeSort_TypeName: TMenuItem
          Action = TreeSort_TypeName
        end
        object itemTreeSort_IndexSort: TMenuItem
          Action = TreeSort_IndexSort
        end
      end
      object menuExpandMode: TMenuItem
        Caption = #1056#1077#1078#1080#1084#1099' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1087#1072#1087#1086#1082
        Hint = #1042#1099#1073#1086#1088' '#1088#1077#1078#1080#1084#1086#1074' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1087#1072#1087#1086#1082' ('#1075#1088#1091#1087#1087' '#1079#1072#1087#1080#1089#1077#1081')'
        ImageIndex = 32
        object itemSaveTreePosition: TMenuItem
          Action = SaveTreePosition
        end
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
      object itemTreeRootOnly: TMenuItem
        Action = TreeRootOnly
      end
      object divDataExp: TMenuItem
        Caption = '-'
      end
      object itemExpandNode: TMenuItem
        Action = ExpandNode
      end
      object itemExpandAll: TMenuItem
        Action = ExpandAll
      end
      object itemCollapseNode: TMenuItem
        Action = CollapseNode
      end
      object itemCollapseAll: TMenuItem
        Action = CollapseAll
      end
    end
  end
  object TreePopupMenu: TPopupMenu [8]
    Images = BaseData.ImageList
    Left = 12
    Top = 92
    object itemNewGroupT: TMenuItem
      Action = NewGroup
    end
    object itemNewSubGroupT: TMenuItem
      Action = NewSubGroup
    end
    object itemNewItemT: TMenuItem
      Action = NewItem
    end
    object itemNewSubitemP: TMenuItem
      Action = NewSubitem
    end
    object itemCopyRecordT: TMenuItem
      Action = CopyRecord
    end
    object itemPropertiesT: TMenuItem
      Action = Properties
      Default = True
    end
    object itemMoveT: TMenuItem
      Action = Move
    end
    object itemDeleteT: TMenuItem
      Action = Delete
    end
    object divTreeEdit: TMenuItem
      Caption = '-'
    end
    object itemFindTreeT: TMenuItem
      Action = FindTree
      Caption = #1055#1086#1080#1089#1082
    end
    object divTreeFind: TMenuItem
      Caption = '-'
    end
    object itemTreeVisibleT: TMenuItem
      Action = TreeVisible
    end
    object menuExpandModeT: TMenuItem
      Caption = #1056#1077#1078#1080#1084#1099' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1087#1072#1087#1086#1082
      Hint = #1042#1099#1073#1086#1088' '#1088#1077#1078#1080#1084#1086#1074' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1087#1072#1087#1086#1082' ('#1075#1088#1091#1087#1087' '#1079#1072#1087#1080#1089#1077#1081')'
      ImageIndex = 32
      object itemEM_NoneT: TMenuItem
        Action = EM_None
      end
      object itemEM_RootT: TMenuItem
        Action = EM_Root
      end
      object itemEM_GroupsT: TMenuItem
        Action = EM_Groups
      end
      object itemEM_AllT: TMenuItem
        Action = EM_All
      end
      object divEMT: TMenuItem
        Caption = '-'
      end
      object itemSelectSaveEMT: TMenuItem
        Action = SelectSaveEM
      end
      object itemSaveTreePositionT: TMenuItem
        Action = SaveTreePosition
      end
    end
    object menuSortTreeT: TMenuItem
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1087#1072#1087#1086#1082
      Hint = #1055#1086#1088#1103#1076#1086#1082' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1079#1072#1087#1080#1089#1077#1081' '#1074' '#1089#1090#1088#1091#1082#1090#1091#1088#1077' '#1087#1072#1087#1086#1082
      ImageIndex = 18
      object itemTreeSort_NoneT: TMenuItem
        Action = TreeSort_None
      end
      object itemTreeSort_IdT: TMenuItem
        Action = TreeSort_Id
      end
      object itemTreeSort_TypeIdT: TMenuItem
        Action = TreeSort_TypeId
      end
      object itemTreeSort_NameT: TMenuItem
        Action = TreeSort_Name
      end
      object itemTreeSort_TypeNameT: TMenuItem
        Action = TreeSort_TypeName
      end
      object itemTreeSort_IndexSortT: TMenuItem
        Action = TreeSort_IndexSort
      end
    end
    object itemTreeRootOnlyT: TMenuItem
      Action = TreeRootOnly
    end
    object divTreeFolders: TMenuItem
      Caption = '-'
    end
    object itemExpandNodeT: TMenuItem
      Action = ExpandNode
    end
    object itemExpandAllT: TMenuItem
      Action = ExpandAll
      Hint = #1055#1086#1082#1072#1079#1072#1090#1100' '#1101#1083#1077#1084#1077#1085#1090#1099' '#1079#1072#1075#1088#1091#1078#1077#1085#1085#1086#1081' '#1089#1090#1088#1091#1082#1090#1091#1088#1099
    end
    object itemCollapseNodeT: TMenuItem
      Action = CollapseNode
    end
    object itemCollapseAllT: TMenuItem
      Action = CollapseAll
    end
    object divTreeData: TMenuItem
      Caption = '-'
    end
    object itemRefreshT: TMenuItem
      Action = Refresh
    end
    object divTreeRefresh: TMenuItem
      Caption = '-'
    end
    object itemCloseCancelT: TMenuItem
      Action = CloseCancel
    end
  end
  object NewPopupMenu: TPopupMenu [9]
    Images = BaseData.ImageList
    Left = 40
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
    object itemNewSubitemN: TMenuItem
      Action = NewSubitem
    end
    object itemNewRecordN: TMenuItem
      Action = NewRecord
    end
  end
  inherited DataPopupMenu: TPopupMenu
    object itemSortTreeD: TMenuItem
      Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1087#1072#1087#1086#1082
      Hint = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1087#1072#1087#1086#1082' ('#1075#1088#1091#1087#1087' '#1079#1072#1087#1080#1089#1077#1081')'
      ImageIndex = 18
      object itemTreeSort_NoneD: TMenuItem
        Action = TreeSort_None
      end
      object itemTreeSort_IdD: TMenuItem
        Action = TreeSort_Id
      end
      object itemTreeSort_TypeIdD: TMenuItem
        Action = TreeSort_TypeId
      end
      object itemTreeSort_NameD: TMenuItem
        Action = TreeSort_Name
      end
      object itemTreeSort_TypeNameD: TMenuItem
        Action = TreeSort_TypeName
      end
      object itemTreeSort_IndexSortD: TMenuItem
        Action = TreeSort_IndexSort
      end
    end
    object menuExpandModeD: TMenuItem
      Caption = #1056#1077#1078#1080#1084#1099' '#1079#1072#1075#1088#1091#1079#1082#1080
      Hint = #1042#1099#1073#1086#1088' '#1088#1077#1078#1080#1084#1086#1074' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1089#1090#1088#1091#1082#1090#1091#1088#1099' '#1087#1072#1087#1086#1082' ('#1075#1088#1091#1087#1087' '#1079#1072#1087#1080#1089#1077#1081')'
      ImageIndex = 32
      object itemSaveTreePositionD: TMenuItem
        Action = SaveTreePosition
      end
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
    object itemTreeRootOnlyD: TMenuItem
      Action = TreeRootOnly
    end
    object divDataDExp: TMenuItem
      Caption = '-'
    end
    object itemExpandNodeD: TMenuItem
      Action = ExpandNode
    end
    object itemExpandAllD: TMenuItem
      Action = ExpandAll
    end
    object itemCollapseNodeD: TMenuItem
      Action = CollapseNode
    end
    object itemCollapseAllD: TMenuItem
      Action = CollapseAll
    end
  end
end
