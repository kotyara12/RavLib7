inherited FormSrLevels: TFormSrLevels
  Left = 253
  Top = 226
  Width = 917
  HelpKeyword = 'IDH_SRLEVELS'
  Caption = #1043#1088#1091#1087#1087#1099' '#1073#1077#1079#1086#1087#1072#1089#1085#1086#1089#1090#1080' '#1086#1087#1077#1088#1072#1094#1080#1081
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000007B6D
    51FF847657FF88795BFF8A7C5EFF8E8062FF918365FF948769FF988A6DFF9B8D
    71FF9E9175FFA29478FFA4977BFFA79A7FFFAB9E83FFADA187FFB5A98EFF7568
    4CFFC6B798FFAF9F7EFFA29270FFA59573FFB6A787FFB9AA8AFFAF9E7CFFB1A1
    7FFFC0B293FFC2B496FFBAA987FFBDAD8BFFC9BC9FFFD3CAB4FFBDB195FF7669
    4EFFAF9F7EFF6D9C52FF26CA42FF50DA7AFF96B187FF76A69DFF27CDF0FF50DC
    F0FF9EB9A3FF8087A5FF2B5AF1FF5570F2FFA89FACFFDDD5C2FFBFB297FF776B
    4FFF9F8E6BFF12C126FF5AF5ABFF65F8B9FF7AEBB3FF13C4EFFF6EDFFFFF78E3
    FFFF7AECEFFF1750F1FF6F7FFFFF7985FFFF7E86F2FFC7B899FFC1B59AFF796C
    51FF9F8E6BFF0FC225FF5BF5ADFFD8FDEEFF7AEBB3FF10C5F1FF6FDFFFFFDDF8
    FFFF7AECEFFF164FF2FF7080FFFFDDE0FFFF7E86F2FFC7B899FFC3B69CFF7A6E
    52FFAF9F7FFF6D9C52FF25CA42FF4FDB7BFF96B187FF76A69DFF26CDF1FF50DD
    F1FF9EB9A3FF8087A5FF2A5AF2FF5470F3FFA89FACFFDDD5C2FFC4B89EFF7C6F
    53FFC8BA9CFFB1A181FFA29370FFA59673FFB8A98AFFBAAC8DFFAF9E7CFFB1A1
    7FFFC1B496FFC4B699FFBAAA88FFBDAD8BFFCBBEA2FFD3CAB4FFC6BAA0FF8C7E
    5EFFA0906EFFA39372FFA69676FFA99A7AFFAB9D7EFFAEA081FFB1A385FFB4A6
    89FFB7A98CFFB9AC90FFBCAF94FFBFB297FFC1B59BFFC4B89FFFC7BBA2FF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000FFFF0000000000000000000000000000000000000000
    0000000000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Width = 901
  end
  inherited CoolBar: TCoolBar
    Width = 901
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 901
      end>
    inherited ToolBar: TToolBar
      Width = 888
    end
  end
  inherited DataPanel: TPanel
    Width = 901
    inherited DbGrid: TRDbStyledGrid
      Width = 901
      Height = 301
      Columns = <
        item
          Expanded = False
          FieldName = 'id'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'name'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'notes'
          Width = 1533
          Visible = True
        end>
    end
    inherited InfoPanel: TRDbInfoPanel
      Top = 322
      Width = 901
      Height = 42
      object lblId: TLabel
        Left = 4
        Top = 4
        Width = 69
        Height = 13
        AutoSize = False
        Caption = #1050#1086#1076' (id):'
        FocusControl = dtId
      end
      object lblName: TLabel
        Left = 184
        Top = 4
        Width = 113
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077':'
        FocusControl = dtName
      end
      object lblNotes: TLabel
        Left = 4
        Top = 22
        Width = 73
        Height = 13
        AutoSize = False
        Caption = #1055#1088#1080#1084#1077#1095#1072#1085#1080#1077':'
        FocusControl = deNotes
      end
      object dtId: TRDbText
        Left = 104
        Top = 2
        Width = 77
        Height = 17
        Alignment = taCenter
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'id'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 0
      end
      object dtName: TRDbText
        Tag = -6
        Left = 300
        Top = 2
        Width = 603
        Height = 17
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'name'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 1
      end
      object deNotes: TRDbText
        Tag = -6
        Left = 104
        Top = 20
        Width = 799
        Height = 17
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'notes'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 2
      end
    end
    inherited TabViews: TTabSet
      Top = 301
      Width = 901
    end
  end
  inherited FindPanel: TPanel
    Width = 901
  end
  inherited ActionList: TActionList
    Left = 40
    Top = 124
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
    DataSet = AdminData.sr_levels
    DefaultField = 'name'
    KeyField = 'id'
    ListFields = 'name;notes'
    Left = 236
    Top = 124
  end
  inherited RDbGridTuner: TRDbGridTuner
    StoreInIniFile = True
    Left = 264
    Top = 124
  end
  inherited RDbFilterStatus: TRDbFilterStatus
    Panel_DbFilter = -1
    Left = 292
    Top = 124
  end
  inherited PopupMenu: TPopupMenu
    Left = 96
    Top = 124
  end
  inherited MainMenu: TMainMenu
    Left = 68
    Top = 124
    inherited menuEdit: TMenuItem
      inherited divEditImport: TMenuItem
        Visible = False
      end
    end
  end
  inherited DataPopupMenu: TPopupMenu
    Left = 124
    Top = 124
  end
  inherited OperationsPopupMenu: TPopupMenu
    Left = 152
    Top = 124
  end
  inherited ReportsPopupMenu: TPopupMenu
    Left = 180
    Top = 124
  end
  inherited RDbEditor: TRDbExportEditor
    DataSet = AdminData.sr_levels
    KeyFieldName = 'id'
    CopiedFields = 'name;notes;hidden;font_style;font_color;cell_color'
    ObjectDesc = #1043#1088#1091#1087#1087#1099' '#1086#1087#1077#1088#1072#1094#1080#1081
    StatisticFields = 'hidden'
    OnGetEditorClass = RDbEditorGetEditorClass
    OnGetEditRights = RDbEditorGetEditRights
    Left = 208
    Top = 124
  end
  inherited TitleGridPopupMenu: TPopupMenu
    Left = 320
    Top = 124
  end
  inherited RDbLocate: TRDbFind
    StoreInIniFile = True
    DataSet = AdminData.sr_levels
    DefaultField = 'name'
    Left = 348
    Top = 124
  end
  inherited RDbUpdater: TRDbUpdater
    Left = 376
    Top = 124
  end
  inherited InfoPanelPopupMenu: TPopupMenu
    Left = 404
    Top = 124
  end
  inherited ViewsPopupMenu: TPopupMenu
    Left = 40
    Top = 152
  end
end
