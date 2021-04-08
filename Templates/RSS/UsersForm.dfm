inherited FormUsers: TFormUsers
  Left = 269
  Top = 247
  Width = 919
  HelpKeyword = 'IDH_USERS'
  Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1080' '#1089#1080#1089#1090#1077#1084#1099
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000B17C0E77B2780BF3B27D10FFB27D
    10FFB27D10FFB27D10FFB27D10FFB27D10FFB27D10FFB2780BF3B17C0E770000
    000000000000000000000000000000000000AE7911FBE2B756FFE5B853FFE5B8
    53FFE5B853FFE5B853FFE5B853FFE5B853FFE5B853FFE2B756FFAE7911FB0000
    000000000000000000000000000000000000B38013DFE7C57CFFEAC675FFE6BB
    5BFFE5B853FFE5B853FFE5B853FFE7BC5CFFEBC776FFE7C67DFFB38013DF1F70
    44831B6B40F9207045FF207045FF207045FF627830FFC2922EFFDCB76AFFECD0
    92FFEBC673FFE6B954FFEBC674FFEDD294FFDDB96BFFC2922EFFB7831667206B
    40FD3EB475FF25B166FF27B369FF29B56BFF2BB76EFF54A958FFA0912FFFBB87
    1CFFD8AC4EFFE8BB56FFD9AD4EFFBB871CFFB88519C7B9841849000000002070
    42CF70C096FF7DD1A5FF47BF81FF2BB76EFF4BC386FF82D6ACFF78C397FFAC85
    1BF9DAAB45FFEABD58FFDCAD46FFBB831BDFBF7F1F0800000000000000001F6F
    4340206C45E436885CFF55B683FF2DB971FF58B986FF388C60FF677935EFCC9A
    31FFEBBE5AFFECBF5AFFEDC05BFFCC9B32FFBE881E6E00000000000000000000
    0000007F7F02206F46AE268E57FF2FBB75FF27905AFF227848ACBE8A20BEE0B1
    4BFFEDC05CFFEEC15CFFEEC15DFFE2B34DFFBF891EC100000000000000000000
    00002277441E1F744BFB2FB772FF31BD78FF32BA77FF1F7A4EFBBA8C23D8E6B9
    56FFEFC25EFFF0C35EFFF0C35FFFE9BC58FFC18D23D300000000000000000000
    00001F76485829905AFF32BE78FF33BF7BFF35C17DFF2C9760FFB58D28DEEBC3
    6EFFF1C460FFF2C560FFF2C561FFECC671FFC18F24CB00000000000000000000
    0000227A4C683A9F6BFF34C07BFF36C27EFF38C480FF3DA571FFA18D2FC1E8C7
    81FFF5CF7BFFF4C762FFF6CF7AFFE9C983FFC492279400000000000000000000
    0000247F4C46409C6CFF57CC93FF3AC582FF5ACF97FF42A171FF6B8C4166CC9B
    30FBF5DFAFFFFCEECFFFF6E0B1FFCC9B30FBC7952C2E00000000000000000000
    00002A7F5506228451E573C89FFF9BE2C0FF75CBA1FF258B50E72A7F5506C997
    2C51C9962DD6C9962DFFC8952DDCC9962D550000000000000000000000000000
    00000000000024885138228752CF258A54FF268B52CF248D5638000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000F8000000F8000000F8000000000000000001000000030000000300008003
    00008003000080030000800300008003000080070000C1FF0000FFFF0000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar: TStatusBar
    Width = 903
  end
  inherited CoolBar: TCoolBar
    Width = 903
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        MinHeight = 36
        Width = 903
      end>
    inherited ToolBar: TToolBar
      Width = 890
      inherited OpersToolButton: TToolButton
        Visible = True
      end
    end
  end
  inherited TreePanel: TPanel
    inherited TreeHeaderPanel: TPanel
      Caption = #1043#1088#1091#1087#1087#1099' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
    end
    inherited TreeView: TRTreeView
      Images = BaseData.ImageList
    end
  end
  inherited DataPanel: TPanel
    Width = 679
    inherited InfoPanel: TRDbInfoPanel
      Top = 361
      Width = 679
      Height = 60
      object IDRDbTextLabel: TLabel
        Left = 4
        Top = 4
        Width = 97
        Height = 13
        AutoSize = False
        Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100':'
        FocusControl = IDRDbText
      end
      object GroupsLabel: TLabel
        Left = 4
        Top = 22
        Width = 97
        Height = 13
        AutoSize = False
        Caption = #1043#1088#1091#1087#1087#1072':'
        FocusControl = IDRDbText
      end
      object StateLabel: TLabel
        Left = 4
        Top = 40
        Width = 97
        Height = 13
        AutoSize = False
        Caption = #1057#1086#1089#1090#1086#1103#1085#1080#1077':'
        FocusControl = BLOCKEDRDbText
      end
      object NotesLabel: TLabel
        Left = 264
        Top = 40
        Width = 97
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103':'
        FocusControl = BLOCKEDRDbText
      end
      object IDRDbText: TRDbText
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
      object NAMERDbText: TRDbText
        Left = 182
        Top = 2
        Width = 127
        Height = 17
        Hint = #1051#1086#1075#1080#1085
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
      object FULLNAMERDbText: TRDbText
        Tag = -6
        Left = 310
        Top = 2
        Width = 371
        Height = 17
        Hint = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'FULLNAME'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 2
      end
      object NAME_GROUPSRDbText: TRDbText
        Left = 104
        Top = 20
        Width = 205
        Height = 17
        Hint = #1043#1088#1091#1087#1087#1072' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'NAME_GROUPS'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 3
      end
      object NOTES_GROUPSRDbText: TRDbText
        Tag = -6
        Left = 310
        Top = 20
        Width = 371
        Height = 17
        Hint = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1086' '#1075#1088#1091#1087#1087#1077
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'NOTES_GROUPS'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 4
      end
      object BLOCKEDRDbText: TRDbText
        Left = 104
        Top = 38
        Width = 77
        Height = 17
        Hint = #1041#1083#1086#1082#1080#1088#1086#1074#1072#1085
        Alignment = taCenter
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'BLOCKED'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 5
      end
      object DELETEDRDbText: TRDbText
        Left = 182
        Top = 38
        Width = 77
        Height = 17
        Hint = #1059#1076#1072#1083#1077#1085
        Alignment = taCenter
        AutoSize = False
        BevelKind = bkFlat
        Color = clWindow
        DataField = 'DELETED'
        DataSource = RDbEditor
        ParentColor = False
        PopupMenu = InfoPanelPopupMenu
        ShowAccelChar = False
        TabOrder = 6
      end
      object NOTESRDbText: TRDbText
        Tag = -6
        Left = 364
        Top = 38
        Width = 317
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
        TabOrder = 7
      end
    end
    inherited DbGrid: TRDbStyledGrid
      Width = 679
      Height = 311
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
          FieldName = 'fullname'
          Width = 200
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'notes'
          Width = 200
          Visible = True
        end
        item
          Alignment = taCenter
          Expanded = False
          FieldName = 'deleted'
          Title.Alignment = taCenter
          Width = 65
          Visible = True
        end
        item
          Alignment = taCenter
          Expanded = False
          FieldName = 'blocked'
          Title.Alignment = taCenter
          Visible = True
        end>
    end
    inherited FindPanel: TPanel
      Width = 679
    end
    inherited TabViews: TTabSet
      Top = 340
      Width = 679
    end
  end
  inherited ActionList: TActionList
    inherited TreeVisible: TAction
      Caption = #1055#1072#1085#1077#1083#1100' '#1075#1088#1091#1087#1087' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
      Hint = #1057#1082#1088#1099#1090#1100' / '#1087#1086#1082#1072#1079#1072#1090#1100' '#1075#1088#1091#1087#1087#1099' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
    end
  end
  inherited PopupMenu: TPopupMenu
    inherited itemCopyRecordP: TMenuItem
      Visible = False
    end
    inherited menuOperationsP: TMenuItem
      SubMenuImages = AdminData.AdminImageList
      Visible = True
      object itemUnlockUserP: TMenuItem
        Action = UnlockUser
      end
      object itemLockUserP: TMenuItem
        Action = LockUser
      end
      object divOperP1: TMenuItem
        Caption = '-'
      end
      object itemResetPasswordP: TMenuItem
        Action = ResetPassword
      end
    end
  end
  inherited MainMenu: TMainMenu
    inherited menuOperations: TMenuItem
      SubMenuImages = AdminData.AdminImageList
      Visible = True
      object itemUnlockUser: TMenuItem
        Action = UnlockUser
      end
      object itemLockUser: TMenuItem
        Action = LockUser
      end
      object divOper1: TMenuItem
        Caption = '-'
      end
      object itemResetPassword: TMenuItem
        Action = ResetPassword
      end
    end
  end
  object su_users: TADOQuery [9]
    Connection = BaseData.acDb
    LockType = ltPessimistic
    OnCalcFields = CalcFields
    EnableBCD = False
    ParamCheck = False
    Parameters = <>
    Prepared = True
    SQL.Strings = (
      'SELECT * FROM su_users')
    Left = 256
    Top = 104
    object su_usersid: TIntegerField
      Tag = 1
      DisplayLabel = #1050#1086#1076' (id)'
      FieldName = 'id'
      Visible = False
    end
    object su_usersname: TStringField
      Tag = 1
      DisplayLabel = #1051#1086#1075#1080#1085
      FieldName = 'name'
      Required = True
      Size = 16
    end
    object su_usersfullname: TStringField
      DisplayLabel = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
      FieldName = 'fullname'
      Required = True
      Size = 64
    end
    object su_usersnotes: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      FieldName = 'notes'
      Size = 64
    end
    object su_usersid_groups: TIntegerField
      DisplayLabel = #1043#1088#1091#1087#1087#1072' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081' [#]'
      FieldName = 'id_groups'
      Required = True
      Visible = False
    end
    object su_usersname_groups: TStringField
      DisplayLabel = #1043#1088#1091#1087#1087#1072' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
      FieldKind = fkLookup
      FieldName = 'name_groups'
      LookupDataSet = su_groups
      LookupKeyFields = 'id'
      LookupResultField = 'name'
      KeyFields = 'id_groups'
      Size = 64
      Lookup = True
    end
    object su_usersnotes_groups: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1086' '#1075#1088#1091#1087#1087#1077
      FieldKind = fkLookup
      FieldName = 'notes_groups'
      LookupDataSet = su_groups
      LookupKeyFields = 'id'
      LookupResultField = 'notes'
      KeyFields = 'id_groups'
      Visible = False
      Size = 255
      Lookup = True
    end
    object su_userspassword: TStringField
      DisplayLabel = #1055#1072#1088#1086#1083#1100' ('#1079#1072#1096#1080#1092#1088#1086#1074#1072#1085')'
      FieldName = 'password'
      Visible = False
      Size = 255
    end
    object su_usersdeleted: TBooleanField
      DisplayLabel = #1059#1076#1072#1083#1077#1085
      FieldName = 'deleted'
      DisplayValues = 'DELETED;'
    end
    object su_usersblocked: TBooleanField
      DisplayLabel = #1041#1083#1086#1082#1080#1088#1086#1074#1072#1085
      FieldName = 'blocked'
      DisplayValues = 'BLOCKED;'
    end
    object su_userschanged: TDateTimeField
      Alignment = taCenter
      DisplayLabel = #1044#1072#1090#1072' '#1089#1084#1077#1085#1099' '#1087#1072#1088#1086#1083#1103
      FieldName = 'changed'
      Visible = False
    end
    object su_userscount_ep: TIntegerField
      DisplayLabel = #1055#1086#1087#1099#1090#1082#1080' '#1076#1086#1089#1090#1091#1087#1072
      FieldName = 'count_ep'
      Visible = False
    end
    object su_usersfont_style: TIntegerField
      DisplayLabel = #1057#1090#1080#1083#1100' '#1096#1088#1080#1092#1090#1072
      FieldKind = fkCalculated
      FieldName = 'font_style'
      Visible = False
      Calculated = True
    end
    object su_userscell_color: TIntegerField
      DisplayLabel = #1062#1074#1077#1090' '#1092#1086#1085#1072
      FieldKind = fkCalculated
      FieldName = 'cell_color'
      Visible = False
      Calculated = True
    end
  end
  object su_groups: TADOQuery [10]
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    EnableBCD = False
    ParamCheck = False
    Parameters = <>
    Prepared = True
    SQL.Strings = (
      'SELECT id, owner_id, name, notes FROM su_groups '
      'ORDER BY owner_id, id')
    Left = 152
    Top = 92
    object su_groupsid: TIntegerField
      Tag = 1
      DisplayLabel = #1050#1086#1076' (id)'
      FieldName = 'id'
      Visible = False
    end
    object su_groupsname: TStringField
      Tag = 1
      DisplayLabel = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
      FieldName = 'name'
      Size = 64
    end
    object su_groupsowner_id: TIntegerField
      DisplayLabel = #1050#1086#1076' '#1088#1086#1076#1080#1090#1077#1083#1100#1089#1082#1086#1081' '#1075#1088#1091#1087#1087#1099
      FieldName = 'owner_id'
      Visible = False
    end
    object su_groupsnotes: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      FieldName = 'notes'
      Size = 255
    end
  end
  inherited RDbFind: TRDbSearch
    StoreInIniFile = True
    DataSet = su_users
    DefaultField = 'name'
    KeyField = 'id'
    ListFields = 'name;fullname;notes'
  end
  inherited RDbFilter: TRDbFilter
    StoreInIniFile = True
    DataSet = su_users
    AddBrackets = False
    object RDbFilter_ID: TRDFIntegerItem
      Operation = loAnd
      FieldName = 'ID'
      FieldCaption = #1050#1086#1076' (ID)'
    end
    object RDbFilter_ID_GROUPS: TRDFListLinkItem
      Operation = loAnd
      FieldName = 'ID_GROUPS'
      FieldCaption = #1043#1088#1091#1087#1087#1072' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
      LookupDataSet = su_groups
      KeyField = 'ID'
      LookupFields = 'NAME;NOTES'
      LookupCaptions = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077';'#1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      DefaultSelection = '*'
      MaxTextValues = 10
      KeysDelimiter = ','
      LookupDelimiter = '; '
    end
    object RDbFilter_NAME: TRDFStringItem
      Operation = loAnd
      FieldName = 'NAME'
      FieldCaption = #1051#1086#1075#1080#1085
    end
    object RDbFilter_FULLNAME: TRDFStringItem
      Operation = loAnd
      FieldName = 'FULLNAME'
      FieldCaption = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
    end
    object RDbFilter_NOTES: TRDFStringItem
      Operation = loAnd
      FieldName = 'NOTES'
      FieldCaption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
    end
    object RDbFilter_DELETED: TRDFBooleanItem
      Active = True
      Operation = loAnd
      FieldName = 'DELETED'
      FieldCaption = #1057#1090#1072#1090#1091#1089
      CompareMode = embFalse
      ValueWhereTrue = '1'
      ValueWhereFalse = '0'
      ValueFilterTrue = 'TRUE'
      ValueFilterFalse = 'FALSE'
      ValueTextTrue = #1059#1076#1072#1083#1077#1085#1085#1099#1077' '#1091#1095#1077#1090#1085#1099#1077' '#1079#1072#1087#1080#1089#1080
      ValueTextFalse = #1040#1082#1090#1080#1074#1085#1099#1077' '#1091#1095#1077#1090#1085#1099#1077' '#1079#1072#1087#1080#1089#1080
    end
    object RDbFilter_BLOCKED: TRDFBooleanItem
      Operation = loAnd
      FieldName = 'BLOCKED'
      FieldCaption = #1044#1086#1089#1090#1091#1087' '#1074' '#1089#1080#1089#1090#1077#1084#1091
      ValueWhereTrue = '1'
      ValueWhereFalse = '0'
      ValueFilterTrue = 'TRUE'
      ValueFilterFalse = 'FALSE'
      ValueTextTrue = #1044#1086#1089#1090#1091#1087' '#1079#1072#1087#1088#1077#1097#1077#1085
      ValueTextFalse = #1044#1086#1089#1090#1091#1087' '#1088#1072#1079#1088#1077#1096#1077#1085
    end
  end
  inherited RDbOrder: TRDbOrder
    StoreInIniFile = True
    DataSet = su_users
    OrderString = 'name'
  end
  inherited RDbGridTuner: TRDbGridTuner
    StoreInIniFile = True
  end
  inherited OperationsPopupMenu: TPopupMenu
    Images = AdminData.AdminImageList
    object itemUnlockUserO: TMenuItem
      Action = UnlockUser
    end
    object itemLockUserO: TMenuItem
      Action = LockUser
    end
    object divOperO1: TMenuItem
      Caption = '-'
    end
    object itemResetPasswordO: TMenuItem
      Action = ResetPassword
    end
  end
  inherited RDbEditor: TRDbExportEditor
    DataSet = su_users
    BlockFieldName = 'deleted'
    StatisticFields = 'name_groups;deleted;blocked'
    OnGetEditorClass = RDbEditorGetEditorClass
    OnCreateNewRecord = RDbEditorCreateNewRecord
    OnAfterPostLogged = RDbEditorAfterPostLogged
    OnBeforeDelete = RDbEditorBeforeDelete
    OnGetEditRights = RDbEditorGetEditRights
  end
  inherited TreeLoader: TRDbTreeLoader
    OnOpenDataSets = TreeOpenDataSets
    OnGetEditRights = TreeGetEditRights
  end
  inherited GroupsEditor: TRDbTreeEditor
    DataSet = su_groups
    CopiedFields = 'name;notes'
    ObjectDesc = #1043#1088#1091#1087#1087#1099' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1081
    OnGetEditorClass = GroupsGetEditorClass
    NrmImage = 26
    SelImage = 27
  end
  inherited RDbLocate: TRDbFind
    StoreInIniFile = True
    DataSet = su_users
    DefaultField = 'name'
  end
  inherited RDbUpdater: TRDbUpdater
    object RDbUpdater_NAME: TRDUStringItem
      FieldName = 'NAME'
      FieldCaption = #1051#1086#1075#1080#1085
    end
    object RDbUpdater_FULLNAME: TRDUStringItem
      FieldName = 'FULLNAME'
      FieldCaption = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
    end
    object RDbUpdater_NOTES: TRDUStringItem
      FieldName = 'NOTES'
      FieldCaption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
    end
  end
  object OperationsActionList: TActionList
    Images = AdminData.AdminImageList
    Left = 452
    Top = 148
    object UnlockUser: TAction
      Category = #1054#1087#1077#1088#1072#1094#1080#1080
      Caption = #1056#1072#1079#1088#1077#1096#1080#1090#1100' '#1076#1086#1089#1090#1091#1087
      Enabled = False
      Hint = #1056#1072#1079#1088#1077#1096#1080#1090#1100' '#1076#1086#1089#1090#1091#1087' '#1074' '#1089#1080#1089#1090#1077#1084#1091' '#1076#1083#1103' '#1090#1077#1082#1091#1097#1077#1075#1086' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
      ImageIndex = 4
      ShortCut = 16469
      OnExecute = UnlockUserExecute
      OnUpdate = UnlockUserUpdate
    end
    object LockUser: TAction
      Category = #1054#1087#1077#1088#1072#1094#1080#1080
      Caption = #1041#1083#1086#1082#1080#1088#1086#1074#1072#1090#1100' '#1076#1086#1089#1090#1091#1087
      Enabled = False
      Hint = #1047#1072#1087#1088#1077#1090#1080#1090#1100' '#1076#1086#1089#1090#1091#1087' '#1074' '#1089#1080#1089#1090#1077#1084#1091' '#1076#1083#1103' '#1090#1077#1082#1091#1097#1077#1075#1086' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
      ImageIndex = 5
      ShortCut = 16460
      OnExecute = LockUserExecute
      OnUpdate = LockUserUpdate
    end
    object ResetPassword: TAction
      Category = #1054#1087#1077#1088#1072#1094#1080#1080
      Caption = #1057#1073#1088#1086#1089' '#1087#1072#1088#1086#1083#1103
      Enabled = False
      Hint = #1059#1089#1090#1072#1085#1086#1074#1080#1090#1100' '#1087#1072#1088#1086#1083#1100' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103' '#1085#1072' '#1087#1077#1088#1074#1086#1085#1072#1095#1072#1083#1100#1085#1099#1081
      ImageIndex = 3
      ShortCut = 16466
      OnExecute = ResetPasswordExecute
      OnUpdate = ResetPasswordUpdate
    end
  end
  object user_add_operation: TADOStoredProc
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    ProcedureName = 'sp_sys_user_add_operation;1'
    Parameters = <>
    Left = 312
    Top = 148
  end
  object user_del_operation: TADOStoredProc
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    ProcedureName = 'sp_sys_user_del_operation;1'
    Parameters = <>
    Left = 340
    Top = 148
  end
  object user_add_opgroup: TADOStoredProc
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    ProcedureName = 'sp_sys_user_add_opgroup;1'
    Parameters = <>
    Left = 368
    Top = 148
  end
  object user_del_opgroup: TADOStoredProc
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    ProcedureName = 'sp_sys_user_del_opgroup;1'
    Parameters = <>
    Left = 396
    Top = 148
  end
  object user_unlink_all: TADOStoredProc
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    ProcedureName = 'sp_sys_user_unlink_all;1'
    Parameters = <>
    Left = 424
    Top = 148
  end
end
