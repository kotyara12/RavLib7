inherited FormSysLog: TFormSysLog
  Left = 440
  Top = 229
  Height = 500
  HelpKeyword = 'IDH_SYSLOG'
  Caption = #1046#1091#1088#1085#1072#1083' '#1072#1091#1076#1080#1090#1072
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000007F7F
    7F0878787883787878DA767676FD7E7E7EDA808080837F7F7F08000000000000
    00000000000000000000000000000000000000000000000000007F7F7F087878
    78C59E9E9EFFD2D2D2FFE2E2E2FFD3D3D3FFA3A3A3FF868686C57F7F7F081323
    98FF101D7DFF6C6C6CFF6C6C6CFF6C6C6CFF6D6D6DFF6F6F6FFF747474FF9F9F
    9FFFE4E4E4FFE6E6E6FFE8E8E8FFE8E8E8FFE7E7E7FFACACACFF8E8E8E811628
    AFFF273EE6FFD8D8D8FFD8D8D8FFD9D9D9FFD9D9D9FFD9D9D9FF888888FFD3D3
    D3FFE7E7E7FFEBEBEBFFEEEEEEFFEEEEEEFFEDEDEDFFDBDBDBFF919191DB1628
    AFFF273EE6FFDADADAFFDADADAFFDBDBDBFFDBDBDBFFDBDBDBFF7F7F7FFFE4E4
    E4FFEBEBEBFFF0F0F0FF666666FF6D6D6DFFF2F2F2FFEEEEEEFF9A9A9AFF1628
    AFFF273EE6FFDCDCDCFFDCDCDCFFDCDCDCFFDDDDDDFFDDDDDDFF8F8F8FFFD7D7
    D7FFEEEEEEFFF3F3F3FF666666FFFAFAFAFFF6F6F6FFE3E3E3FF9F9F9FDB1628
    AFFF273EE6FFDEDEDEFFDEDEDEFFDEDEDEFFDFDFDFFFDFDFDFFFB2B2B2FFA6A6
    A6FFEFEFEFFFF5F5F5FF6D6D6DFFFDFDFDFFF8F8F8FFBBBBBBFFA4A4A47F1628
    AFFF273EE6FFDFDFDFFFE0E0E0FFE0E0E0FFE1E1E1FFE1E1E1FFDEDEDEFFA1A1
    A1FFAFAFAFFFE3E3E3FFF6F6F6FFE8E8E8FFBEBEBEFFA9A9A9C59F9F9F081628
    AFFF273EE6FFE1E1E1FFE2E2E2FFE2E2E2FFE2E2E2FFE3E3E3FFE3E3E3FFE0E0
    E0FFBDBDBDFFA6A6A6FFA2A2A2FFA6A6A6DAABABAB819F9F9F08000000001628
    AFFF273EE6FFE3E3E3FFE3E3E3FFE4E4E4FF9D9D9DFF9D9D9DFF9E9E9EFFE5E5
    E5FFE5E5E5FFE6E6E6FFC5C5C5FF000000000000000000000000000000001628
    AFFF273EE6FFE5E5E5FFE5E5E5FFE6E6E6FFE6E6E6FFE6E6E6FFE7E7E7FFE7E7
    E7FFE7E7E7FFE8E8E8FFC5C5C5FF000000000000000000000000000000001628
    AFFF273EE6FFE7E7E7FFE7E7E7FFA0A0A0FFA0A0A0FFA0A0A0FFA1A1A1FFA1A1
    A1FFE9E9E9FFEAEAEAFFC5C5C5FF000000000000000000000000000000001628
    AFFF273EE6FFE9E9E9FFE9E9E9FFE9E9E9FFEAEAEAFFEAEAEAFFEAEAEAFFEBEB
    EBFFEBEBEBFFEBEBEBFFC5C5C5FF000000000000000000000000000000001628
    AFFF273EE6FFEBEBEBFFEBEBEBFFEBEBEBFFECECECFFECECECFFECECECFFEDED
    EDFFEDEDEDFFEDEDEDFFC5C5C5FF000000000000000000000000000000001628
    AFFF273EE6FFECECECFFEDEDEDFFEDEDEDFFEDEDEDFFEEEEEEFFEEEEEEFFEFEF
    EFFFEFEFEFFFEFEFEFFFC5C5C5FF000000000000000000000000000000001628
    AFFF1628AFFFC5C5C5FFC5C5C5FFC5C5C5FFC5C5C5FFC5C5C5FFC5C5C5FFC5C5
    C5FFC5C5C5FFC5C5C5FFC5C5C5FF00000000000000000000000000000000FF01
    C5C5FE00C5C50000C5C50000C5C50000C5C50000C5C50000C5C50000C5C50001
    C5C5000FC5C5000FC5C5000FC5C5000FC5C5000FC5C5000FC5C5000FC5C5}
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Top = 423
  end
  inherited CoolBar: TCoolBar
    inherited ToolBar: TToolBar
      inherited OpersToolButton: TToolButton
        Visible = True
      end
    end
  end
  inherited DataPanel: TPanel
    Height = 354
    inherited DbGrid: TRDbStyledGrid
      Height = 255
      Columns = <
        item
          Alignment = taCenter
          Expanded = False
          FieldName = 'datetime'
          Title.Alignment = taCenter
          Visible = True
        end
        item
          Alignment = taCenter
          Expanded = False
          FieldName = 'id_operations'
          Title.Caption = #1054#1087#1077#1088#1072#1094#1080#1103
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'login'
          Title.Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'info255'
          Visible = True
        end>
    end
    inherited InfoPanel: TRDbInfoPanel
      Top = 276
      Height = 78
      object IDRDbTextLabel: TLabel
        Left = 4
        Top = 4
        Width = 93
        Height = 13
        AutoSize = False
        Caption = #1044#1072#1090#1072' '#1080' '#1074#1088#1077#1084#1103':'
        FocusControl = dtDateOper
      end
      object NAMERDbTextLabel: TLabel
        Left = 264
        Top = 4
        Width = 93
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1054#1087#1077#1088#1072#1094#1080#1103':'
        FocusControl = deOperName
      end
      object NOTESRDbTextLabel: TLabel
        Left = 4
        Top = 22
        Width = 97
        Height = 13
        AutoSize = False
        Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1077':'
        FocusControl = dtInfo255
      end
      object ID_USERSRDbTextLabel: TLabel
        Left = 4
        Top = 40
        Width = 93
        Height = 13
        AutoSize = False
        Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100':'
        FocusControl = dtUserId
      end
      object NAME_S_WPRDbTextLabel: TLabel
        Left = 400
        Top = 58
        Width = 93
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1055#1088#1086#1075#1088#1072#1084#1084#1072':'
        FocusControl = dtWpNameS
      end
      object NAME_OPERATIONSRDbTextLabel: TLabel
        Left = 4
        Top = 58
        Width = 93
        Height = 13
        AutoSize = False
        Caption = #1050#1086#1084#1087#1100#1102#1090#1077#1088':'
        FocusControl = deOperName
      end
      object dtDateOper: TRDbText
        Left = 104
        Top = 2
        Width = 157
        Height = 17
        Hint = #1044#1072#1090#1072' '#1080' '#1074#1088#1077#1084#1103
        Alignment = taCenter
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'dateoper'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 0
      end
      object deOperName: TRDbText
        Tag = -6
        Left = 424
        Top = 2
        Width = 426
        Height = 17
        Hint = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1089#1080#1089#1090#1077#1084#1085#1086#1081' '#1086#1087#1077#1088#1072#1094#1080#1080
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'name_operations'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 2
      end
      object dtInfo255: TRDbText
        Tag = -6
        Left = 104
        Top = 20
        Width = 746
        Height = 17
        Hint = #1057#1086#1086#1073#1097#1077#1085#1080#1077' '#1089#1080#1089#1090#1077#1084#1099
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'info255'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 3
      end
      object deOperId: TRDbText
        Left = 362
        Top = 2
        Width = 61
        Height = 17
        Hint = #1050#1086#1076' '#1086#1087#1077#1088#1072#1094#1080#1080
        Alignment = taCenter
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'id_operations'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 1
      end
      object dtUserId: TRDbText
        Left = 104
        Top = 38
        Width = 61
        Height = 17
        Hint = #1050#1086#1076' '#1082#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103' '#1089#1080#1089#1090#1077#1084#1099
        Alignment = taCenter
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'id_users'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 4
      end
      object deUserNotes: TRDbText
        Tag = -6
        Left = 498
        Top = 38
        Width = 352
        Height = 17
        Hint = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1086' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'user_notes'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 5
      end
      object dtUserLogin: TRDbText
        Left = 166
        Top = 38
        Width = 123
        Height = 17
        Hint = #1051#1086#1075#1080#1085' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'login'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 6
      end
      object deUserName: TRDbText
        Left = 290
        Top = 38
        Width = 207
        Height = 17
        Hint = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'fullname'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 7
      end
      object dtWpNameS: TRDbText
        Left = 498
        Top = 56
        Width = 171
        Height = 17
        Hint = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1040#1056#1052
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'name_s_wp'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 10
      end
      object dtHost: TRDbText
        Left = 104
        Top = 56
        Width = 145
        Height = 17
        Hint = #1050#1086#1084#1087#1100#1102#1090#1077#1088
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'host'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 8
      end
      object dtNetuser: TRDbText
        Left = 250
        Top = 56
        Width = 145
        Height = 17
        Hint = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103' '#1074' '#1089#1077#1090#1080
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'netuser'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 9
      end
      object deWpNameF: TRDbText
        Tag = -6
        Left = 670
        Top = 56
        Width = 180
        Height = 17
        Hint = #1055#1086#1083#1085#1086#1077' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1040#1056#1052
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'name_wp'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 11
      end
    end
    inherited TabViews: TTabSet
      Top = 255
    end
  end
  inherited ActionList: TActionList
    Left = 48
    Top = 132
    object SysLogSave: TAction [0]
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1092#1072#1081#1083#1077
      Enabled = False
      Hint = #1042#1099#1075#1088#1091#1079#1080#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1089#1080#1089#1090#1077#1084#1085#1086#1075#1086' '#1078#1091#1088#1085#1072#1083#1072' '#1072#1091#1076#1080#1090#1072' '#1074' '#1092#1072#1081#1083
      ImageIndex = 7
      OnExecute = SysLogSaveExecute
      OnUpdate = SysLogSaveUpdate
    end
    inherited DataSetInsert: TAction
      Visible = False
    end
    inherited DataSetCopy: TAction [11]
      Visible = False
    end
    inherited DataSetEdit: TAction [12]
    end
    inherited DataSetDelete: TAction [13]
      Visible = False
    end
    inherited ImportDS: TAction [14]
    end
    object SysLogClear: TAction
      Caption = #1054#1095#1080#1089#1090#1082#1072' '#1078#1091#1088#1085#1072#1083#1072
      Enabled = False
      Hint = #1059#1076#1072#1083#1077#1085#1080#1077' '#1074#1089#1077#1093' '#1079#1072#1087#1080#1089#1077#1081' '#1080#1079' '#1089#1080#1089#1090#1077#1084#1085#1086#1075#1086' '#1078#1091#1088#1085#1072#1083#1072' '#1072#1091#1076#1080#1090#1072
      ImageIndex = 6
      OnExecute = SysLogClearExecute
      OnUpdate = SysLogClearUpdate
    end
  end
  inherited RDbFilter: TRDbFilter
    StoreInIniFile = True
    DataSet = ss_syslog
    Left = 328
    Top = 132
    object RDbFilter_dateoper: TRDFDateItem
      Active = True
      Operation = loAnd
      FieldName = 'dateoper'
      FieldCaption = #1044#1072#1090#1072' '#1080' '#1074#1088#1077#1084#1103
      CompareMode = emdCurrentDay
    end
    object RDbFilter_id_workplases: TRDFListLinkItem
      Operation = loAnd
      FieldName = 'id_workplases'
      FieldCaption = #1055#1088#1086#1075#1088#1072#1084#1084#1072' ('#1040#1056#1052')'
      LookupDataSet = BaseData.sr_workplases
      KeyField = 'ID'
      LookupFields = 'name_s;name'
      LookupCaptions = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1040#1056#1052';'#1055#1086#1083#1085#1086#1077' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1040#1056#1052
      UseNullValue = False
      DefaultSelection = '*'
      MaxTextValues = 10
      KeysDelimiter = ','
      LookupDelimiter = '; '
    end
    object RDbFilter_id_operations: TRDFListLinkItem
      Operation = loAnd
      FieldName = 'id_operations'
      FieldCaption = #1050#1086#1076' '#1086#1087#1077#1088#1072#1094#1080#1080
      LookupDataSet = AdminData.sr_operations
      KeyField = 'ID'
      LookupFields = 'name;notes'
      LookupCaptions = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077'; '#1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      UseNullValue = False
      DefaultSelection = '*'
      KeysDelimiter = ','
      LookupDelimiter = '; '
    end
    object RDbFilter_id_levels: TRDFListLinkItem
      Operation = loAnd
      FieldName = 'id_levels'
      FieldCaption = #1043#1088#1091#1087#1087#1072' '#1086#1087#1077#1088#1072#1094#1080#1081
      LookupDataSet = AdminData.sr_levels
      KeyField = 'ID'
      LookupFields = 'name;notes'
      LookupCaptions = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077';'#1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      UseNullValue = False
      DefaultSelection = '*'
      KeysDelimiter = ','
      LookupDelimiter = '; '
    end
    object RDbFilter_id_users: TRDFListLinkItem
      Operation = loAnd
      FieldName = 'id_users'
      FieldCaption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100' '#1089#1080#1089#1090#1077#1084#1099
      LookupDataSet = BaseData.su_users
      KeyField = 'ID'
      LookupFields = 'name;fullname'
      LookupCaptions = #1051#1086#1075#1080#1085';'#1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
      UseNullValue = False
      DefaultSelection = '*'
      KeysDelimiter = ','
      LookupDelimiter = '; '
    end
    object RDbFilter_host: TRDFStringItem
      Operation = loAnd
      FieldName = 'host'
      FieldCaption = #1048#1084#1103' '#1082#1086#1084#1087#1100#1102#1090#1077#1088#1072
    end
    object RDbFilter_netuser: TRDFStringItem
      Operation = loAnd
      FieldName = 'netuser'
      FieldCaption = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103' '#1074' '#1089#1077#1090#1080
    end
    object RDbFilter_info: TRDFTextItem
      Operation = loAnd
      FieldName = 'info'
      FieldCaption = #1057#1086#1086#1073#1097#1077#1085#1080#1077' '#1089#1080#1089#1090#1077#1084#1099
    end
  end
  inherited RDbOrder: TRDbOrder
    StoreInIniFile = True
    DataSet = ss_syslog
    OrderString = 'dateoper desc'
    Left = 356
    Top = 132
  end
  inherited RDbFind: TRDbSearch
    StoreInIniFile = True
    DataSet = ss_syslog
    DefaultField = 'info'
    ListFields = 'datetime;info'
    Left = 244
    Top = 132
  end
  inherited RDbGridTuner: TRDbGridTuner
    Left = 272
    Top = 132
  end
  inherited RDbFilterStatus: TRDbFilterStatus
    Left = 300
    Top = 132
  end
  inherited PopupMenu: TPopupMenu
    Left = 104
    Top = 132
    inherited menuOperationsP: TMenuItem
      SubMenuImages = AdminData.AdminImageList
      Visible = True
      object itemSysLogSaveP: TMenuItem
        Action = SysLogSave
      end
      object itemSysLogClearP: TMenuItem
        Action = SysLogClear
      end
    end
  end
  inherited MainMenu: TMainMenu
    Left = 76
    Top = 132
    inherited menuEdit: TMenuItem
      inherited divEditImport: TMenuItem
        Visible = False
      end
    end
    inherited menuOperations: TMenuItem
      SubMenuImages = AdminData.AdminImageList
      Visible = True
      object itemSysLogSave: TMenuItem
        Action = SysLogSave
      end
      object itemSysLogClear: TMenuItem
        Action = SysLogClear
      end
    end
  end
  inherited DataPopupMenu: TPopupMenu
    Left = 132
    Top = 132
  end
  inherited OperationsPopupMenu: TPopupMenu
    Images = AdminData.AdminImageList
    Left = 160
    Top = 132
    object itemSysLogSaveO: TMenuItem
      Action = SysLogSave
    end
    object itemSysLogClearO: TMenuItem
      Action = SysLogClear
    end
  end
  inherited ReportsPopupMenu: TPopupMenu
    Left = 188
    Top = 132
  end
  inherited RDbEditor: TRDbExportEditor
    DataSet = ss_syslog
    StatisticFields = 'fullname'
    OnGetEditorClass = RDbEditorGetEditorClass
    OnGetEditRights = RDbEditorGetEditRights
    Left = 216
    Top = 132
  end
  inherited TitleGridPopupMenu: TPopupMenu
    Left = 440
    Top = 132
  end
  inherited RDbLocate: TRDbFind
    StoreInIniFile = True
    DataSet = ss_syslog
    DefaultField = 'info'
    Left = 384
    Top = 132
  end
  inherited RDbUpdater: TRDbUpdater
    Left = 412
    Top = 132
  end
  inherited InfoPanelPopupMenu: TPopupMenu
    Left = 468
    Top = 132
  end
  inherited ViewsPopupMenu: TPopupMenu
    Left = 48
    Top = 160
  end
  object ss_syslog: TADOQuery
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltReadOnly
    OnCalcFields = ss_syslogCalcFields
    ParamCheck = False
    Parameters = <>
    Prepared = True
    SQL.Strings = (
      'SELECT * FROM vss_syslog')
    Left = 496
    Top = 132
    object ss_syslogdateoper: TDateTimeField
      DisplayLabel = #1044#1072#1090#1072' '#1080' '#1074#1088#1077#1084#1103
      FieldName = 'dateoper'
    end
    object ss_syslogid_operations: TIntegerField
      DisplayLabel = #1050#1086#1076' '#1086#1087#1077#1088#1072#1094#1080#1080
      FieldName = 'id_operations'
      Visible = False
    end
    object ss_syslogname_operations: TStringField
      DisplayLabel = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1089#1080#1089#1090#1077#1084#1085#1086#1081' '#1086#1087#1077#1088#1072#1094#1080#1080
      FieldKind = fkLookup
      FieldName = 'name_operations'
      LookupDataSet = AdminData.sr_operations
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'id_operations'
      Size = 64
      Lookup = True
    end
    object ss_syslognotes_operations: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1086#1073' '#1089#1080#1089#1090#1077#1084#1085#1086#1081' '#1086#1087#1077#1088#1072#1094#1080#1080
      FieldKind = fkLookup
      FieldName = 'notes_operations'
      LookupDataSet = AdminData.sr_operations
      LookupKeyFields = 'id'
      LookupResultField = 'notes'
      KeyFields = 'id_operations'
      Size = 255
      Lookup = True
    end
    object ss_syslogid_levels: TIntegerField
      DisplayLabel = #1050#1086#1076' '#1075#1088#1091#1087#1087#1099' '#1086#1087#1077#1088#1072#1094#1080#1081
      FieldName = 'id_levels'
      Visible = False
    end
    object ss_syslogname_levels: TStringField
      DisplayLabel = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1075#1088#1091#1087#1087#1099' '#1086#1087#1077#1088#1072#1094#1080#1081
      FieldKind = fkLookup
      FieldName = 'name_levels'
      LookupDataSet = AdminData.sr_levels
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'id_levels'
      Size = 64
      Lookup = True
    end
    object ss_syslognotes_levels: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1086' '#1075#1088#1091#1087#1087#1077' '#1086#1087#1077#1088#1072#1094#1080#1081
      FieldKind = fkLookup
      FieldName = 'notes_levels'
      LookupDataSet = AdminData.sr_levels
      LookupKeyFields = 'id'
      LookupResultField = 'notes'
      KeyFields = 'id_levels'
      Size = 255
      Lookup = True
    end
    object ss_syslogid_users: TIntegerField
      DisplayLabel = #1050#1086#1076' '#1082#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103' '#1089#1080#1089#1090#1077#1084#1099
      FieldName = 'id_users'
      Visible = False
    end
    object ss_sysloglogin: TStringField
      DisplayLabel = #1051#1086#1075#1080#1085' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
      FieldKind = fkLookup
      FieldName = 'login'
      LookupDataSet = BaseData.su_users
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'id_users'
      Size = 16
      Lookup = True
    end
    object ss_syslogfullname: TStringField
      DisplayLabel = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
      FieldKind = fkLookup
      FieldName = 'fullname'
      LookupDataSet = BaseData.su_users
      LookupKeyFields = 'id'
      LookupResultField = 'fullname'
      KeyFields = 'id_users'
      Size = 64
      Lookup = True
    end
    object ss_sysloguser_notes: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1086' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077
      FieldKind = fkLookup
      FieldName = 'user_notes'
      LookupDataSet = BaseData.su_users
      LookupKeyFields = 'id'
      LookupResultField = 'notes'
      KeyFields = 'id_users'
      Size = 255
      Lookup = True
    end
    object ss_syslogid_workplases: TIntegerField
      DisplayLabel = #1050#1086#1076' '#1040#1056#1052
      FieldName = 'id_workplases'
      Visible = False
    end
    object ss_syslogname_s_wp: TStringField
      DisplayLabel = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1040#1056#1052
      FieldKind = fkLookup
      FieldName = 'name_s_wp'
      LookupDataSet = BaseData.sr_workplases
      LookupKeyFields = 'id'
      LookupResultField = 'name_s'
      KeyFields = 'id_workplases'
      Size = 16
      Lookup = True
    end
    object ss_syslogname_wp: TStringField
      DisplayLabel = #1055#1086#1083#1085#1086#1077' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1040#1056#1052
      FieldKind = fkLookup
      FieldName = 'name_wp'
      LookupDataSet = BaseData.sr_workplases
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'id_workplases'
      Size = 64
      Lookup = True
    end
    object ss_sysloginfo255: TStringField
      DisplayLabel = #1057#1086#1086#1073#1097#1077#1085#1080#1077' '#1089#1080#1089#1090#1077#1084#1099
      FieldKind = fkCalculated
      FieldName = 'info255'
      Size = 255
      Calculated = True
    end
    object ss_sysloginfo: TMemoField
      DisplayLabel = #1057#1086#1086#1073#1097#1077#1085#1080#1077' '#1089#1080#1089#1090#1077#1084#1099' ('#1087#1086#1083#1085#1086#1089#1090#1100#1102')'
      FieldName = 'info'
      BlobType = ftMemo
    end
    object ss_sysloghost: TStringField
      DisplayLabel = #1050#1086#1084#1087#1100#1102#1090#1077#1088
      FieldName = 'host'
      Size = 32
    end
    object ss_syslognetuser: TStringField
      DisplayLabel = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103' '#1074' '#1089#1077#1090#1080
      FieldName = 'netuser'
      Size = 32
    end
    object ss_syslogfont_style: TIntegerField
      DisplayLabel = #1057#1090#1080#1083#1100' '#1096#1088#1080#1092#1090#1072
      FieldKind = fkLookup
      FieldName = 'font_style'
      LookupDataSet = AdminData.sr_levels
      LookupKeyFields = 'id'
      LookupResultField = 'font_style'
      KeyFields = 'id_levels'
      Visible = False
      Lookup = True
    end
    object ss_syslogfont_color: TIntegerField
      DisplayLabel = #1062#1074#1077#1090' '#1090#1077#1082#1089#1090#1072
      FieldKind = fkLookup
      FieldName = 'font_color'
      LookupDataSet = AdminData.sr_levels
      LookupKeyFields = 'id'
      LookupResultField = 'font_color'
      KeyFields = 'id_levels'
      Visible = False
      Lookup = True
    end
    object ss_syslogcell_color: TIntegerField
      DisplayLabel = #1062#1074#1077#1090' '#1092#1086#1085#1072
      FieldKind = fkLookup
      FieldName = 'cell_color'
      LookupDataSet = AdminData.sr_levels
      LookupKeyFields = 'id'
      LookupResultField = 'cell_color'
      KeyFields = 'id_levels'
      Visible = False
      Lookup = True
    end
  end
end
