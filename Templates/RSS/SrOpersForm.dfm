inherited FormSrOpers: TFormSrOpers
  Width = 871
  HelpKeyword = 'IDH_SROPERATIONS'
  Caption = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089#1080#1089#1090#1077#1084#1099
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000B27D
    10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D
    10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D
    10FFFFFFFFFFFFFFFFFFFFFFFFFFF0EFECFF998F7AFF9E9581FFF8F7F6FFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFB27D10FFB27D
    10FFFFFFFFFFFFFFFFFFFFFFFFFFC2BBAFFFA29478FF9D9075FFD1CDC3FFFFFF
    FFFFFFFFFFFFF8F8F6FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFB27D10FFB27D
    10FF948A74FFC2BCAFFFCEC9BFFF978D77FFB2A484FFAC9F80FF9B927DFFCCC7
    BDFFB3AB9BFF91866FFFACA493FFFFFFFFFFFFFFFFFFFFFFFFFFB27D10FFB27D
    10FFB1A281FFA09378FF9D9076FFB1A385FFBEAF8FFFBEAF8FFFB2A487FF9F92
    78FFA99C80FFB7A888FF948A74FFFFFFFFFFFFFFFFFFFFFFFFFFB27D10FFB27D
    10FFA7997CFFBCAD8CFFBEAF8FFFC0B192FFC1B393FFC2B495FFC2B495FFC2B4
    95FFC2B394FFA3977CFFC2BCAFFFFFFFFFFFFFFFFFFFFFFFFFFFB27D10FFB27D
    10FF9E9177FFBFB090FFC0B292FFA79B80FF958972FF958A72FFA89C82FFC5B7
    99FFC5B799FF9F937AFFCFCAC1FFFFFFFFFFFFFFFFFFFFFFFFFFB27D10FFB27D
    10FFB2A486FFC1B393FFA89B80FFB0A898FFF2F1EEFFF2F1EEFFB0A898FFAA9E
    85FFC9BC9FFFBAAC91FF978D77FFC2BCAFFFF1EFECFFFFFFFFFFB27D10FFB27D
    10FFBFB191FFC4B697FF958972FFF3F1EFFFFFFFFFFFFFFFFFFFF4F3F0FF988D
    76FFCDC0A4FFCBBEA2FFBEB296FFAB9F84FF9A907BFFFFFFFFFFB27D10FFB27D
    10FFC2B495FFC6B89AFF958A72FFF3F2EFFFFFFFFFFFFFFFFFFFF4F3F1FF9E93
    7CFFD0C4A9FFCEC1A6FFBCAF95FFAA9E85FFA49A87FFFFFFFFFFB27D10FFB27D
    10FFB7A98CFFC7BA9CFFACA087FFB5AD9DFFF4F3F1FFF5F3F1FFBAB3A3FFB8AD
    95FFD4C8AFFFC5BAA0FFAAA18CFFD7D3C9FFF9F8F7FFFFFFFFFFB27D10FFB27D
    10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D
    10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D
    10FFDFAE48FFDFAE48FFDFAE48FFDFAE48FFDFAE48FFDFAE48FFDFAE48FFDFAE
    48FFDFAE48FFFFFFFFFFDFAE48FFFFFFFFFFDFAE48FF1313FFFFB27D10FFB27D
    10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D
    10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FF0000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000FFFF0000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Width = 855
  end
  inherited CoolBar: TCoolBar
    Width = 855
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 855
      end>
    inherited ToolBar: TToolBar
      Width = 842
    end
  end
  inherited DataPanel: TPanel
    Width = 855
    inherited DbGrid: TRDbStyledGrid
      Width = 855
      Height = 265
      Columns = <
        item
          Alignment = taCenter
          Expanded = False
          FieldName = 'id'
          Title.Alignment = taCenter
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'hidden'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'name'
          Width = 200
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'name_levels'
          Width = 200
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'notes'
          Visible = True
        end>
    end
    inherited InfoPanel: TRDbInfoPanel
      Top = 286
      Width = 855
      Height = 78
      object lblId: TLabel
        Left = 4
        Top = 4
        Width = 97
        Height = 13
        AutoSize = False
        Caption = #1050#1086#1076' (ID):'
        FocusControl = dtId
      end
      object lblName: TLabel
        Left = 184
        Top = 4
        Width = 109
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077':'
        FocusControl = dtName
      end
      object lblNotes: TLabel
        Left = 4
        Top = 22
        Width = 97
        Height = 13
        AutoSize = False
        Caption = #1055#1088#1080#1084#1077#1095#1072#1085#1080#1077':'
        FocusControl = dtNotes
      end
      object lblHidden: TLabel
        Left = 4
        Top = 40
        Width = 97
        Height = 13
        AutoSize = False
        Caption = #1060#1083#1072#1075#1080':'
        FocusControl = dtHidden
      end
      object lblNameLevels: TLabel
        Left = 184
        Top = 40
        Width = 109
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1043#1088#1091#1087#1087#1072':'
        FocusControl = dtNameLevels
      end
      object lblNotesLevels: TLabel
        Left = 4
        Top = 58
        Width = 97
        Height = 13
        AutoSize = False
        Caption = #1055#1088#1080#1084#1077#1095#1072#1085#1080#1077':'
        FocusControl = dtNotesLevels
      end
      object dtId: TRDbText
        Left = 104
        Top = 2
        Width = 77
        Height = 17
        Hint = #1050#1086#1076' (ID)'
        Alignment = taCenter
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'ID'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 0
      end
      object dtName: TRDbText
        Tag = -6
        Left = 296
        Top = 2
        Width = 553
        Height = 17
        Hint = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'NAME'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 1
      end
      object dtNotes: TRDbText
        Tag = -6
        Left = 104
        Top = 20
        Width = 745
        Height = 17
        Hint = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'NOTES'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 2
      end
      object dtHidden: TRDbText
        Left = 104
        Top = 38
        Width = 77
        Height = 17
        Hint = #1060#1083#1072#1075#1080
        Alignment = taCenter
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'HIDDEN'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 3
      end
      object dtNameLevels: TRDbText
        Tag = -6
        Left = 296
        Top = 38
        Width = 553
        Height = 17
        Hint = #1059#1088#1086#1074#1077#1085#1100' '#1073#1077#1079#1086#1087#1072#1089#1085#1086#1089#1090#1080
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'name_levels'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 4
      end
      object dtNotesLevels: TRDbText
        Tag = -6
        Left = 104
        Top = 56
        Width = 745
        Height = 17
        Hint = #1059#1088#1086#1074#1077#1085#1100' '#1073#1077#1079#1086#1087#1072#1089#1085#1086#1089#1090#1080' - '#1076#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'notes_levels'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 5
      end
    end
    inherited TabViews: TTabSet
      Top = 265
      Width = 855
    end
  end
  inherited FindPanel: TPanel
    Width = 855
  end
  inherited ActionList: TActionList
    Left = 40
    Top = 132
    inherited DataSetInsert: TAction
      Visible = False
    end
    inherited DataSetCopy: TAction
      Visible = False
    end
    inherited DataSetDelete: TAction
      Visible = False
    end
  end
  inherited RDbFind: TRDbSearch
    StoreInIniFile = True
    DataSet = AdminData.sr_operations
    DefaultField = 'name'
    ListFields = 'name;notes'
    Left = 236
    Top = 132
  end
  inherited RDbGridTuner: TRDbGridTuner
    StoreInIniFile = True
    Left = 264
    Top = 132
  end
  inherited RDbFilterStatus: TRDbFilterStatus
    Left = 292
    Top = 132
  end
  inherited PopupMenu: TPopupMenu
    Left = 96
    Top = 132
  end
  inherited MainMenu: TMainMenu
    Left = 68
    Top = 132
    inherited menuEdit: TMenuItem
      inherited divEditImport: TMenuItem
        Visible = False
      end
    end
  end
  inherited DataPopupMenu: TPopupMenu
    Left = 124
    Top = 132
  end
  inherited OperationsPopupMenu: TPopupMenu
    Left = 152
    Top = 132
  end
  inherited ReportsPopupMenu: TPopupMenu
    Left = 180
    Top = 132
  end
  inherited RDbEditor: TRDbExportEditor
    DataSet = AdminData.sr_operations
    CopiedFields = 'id_levels;name;notes'
    ObjectDesc = #1054#1087#1077#1088#1072#1094#1080#1080
    StatisticFields = 'name_levels'
    OnGetEditorClass = RDbEditorGetEditorClass
    OnGetEditRights = RDbEditorGetEditRights
    Left = 208
    Top = 132
  end
  inherited TitleGridPopupMenu: TPopupMenu
    Left = 320
    Top = 132
  end
  inherited RDbLocate: TRDbFind
    StoreInIniFile = True
    DataSet = AdminData.sr_operations
    DefaultField = 'name'
    Left = 348
    Top = 132
  end
  inherited RDbUpdater: TRDbUpdater
    Left = 376
    Top = 132
  end
  inherited InfoPanelPopupMenu: TPopupMenu
    Left = 404
    Top = 132
  end
  inherited ViewsPopupMenu: TPopupMenu
    Left = 40
    Top = 160
  end
end
