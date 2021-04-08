inherited BaseDataRssTemplate: TBaseDataRssTemplate
  OldCreateOrder = True
  Left = 752
  Top = 272
  Height = 191
  Width = 354
  inherited DbActionList: TActionList
    object ChangeUserPassword: TAction
      Category = #1060#1072#1081#1083
      Caption = #1057#1084#1077#1085#1072' '#1087#1072#1088#1086#1083#1103
      Enabled = False
      Hint = #1057#1084#1077#1085#1072' '#1087#1072#1088#1086#1083#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1077#1084
      ImageIndex = 8
      OnExecute = ChangeUserPasswordExecute
      OnUpdate = ChangeUserPasswordUpdate
    end
    object ReadMail: TAction
      Category = #1057#1086#1086#1073#1097#1077#1085#1080#1103
      Caption = #1055#1088#1080#1077#1084' '#1089#1086#1086#1073#1097#1077#1085#1080#1081
      Enabled = False
      Hint = #1055#1088#1080#1077#1084' '#1080' '#1095#1090#1077#1085#1080#1077' '#1089#1086#1086#1073#1097#1077#1085#1080#1081
      ImageIndex = 9
      ShortCut = 32890
      OnExecute = ReadMailExecute
      OnUpdate = ReadMailUpdate
    end
    object SendMail: TAction
      Category = #1057#1086#1086#1073#1097#1077#1085#1080#1103
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1089#1086#1086#1073#1097#1077#1085#1080#1077
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1080' '#1086#1090#1087#1088#1072#1074#1080#1090#1100' '#1089#1086#1086#1073#1097#1077#1085#1080#1077
      ImageIndex = 10
      ShortCut = 32891
      OnExecute = SendMailExecute
      OnUpdate = SendMailUpdate
    end
    object ViewMail: TAction
      Category = #1057#1086#1086#1073#1097#1077#1085#1080#1103
      Caption = #1055#1088#1086#1089#1084#1086#1090#1088' '#1089#1086#1086#1073#1097#1077#1085#1080#1081
      Enabled = False
      Hint = #1055#1088#1086#1089#1084#1086#1090#1088' '#1089#1087#1080#1089#1082#1072' '#1087#1088#1080#1085#1103#1090#1099#1093' '#1080' '#1086#1090#1087#1088#1072#1074#1083#1077#1085#1085#1099#1093' '#1089#1086#1086#1073#1097#1077#1085#1080#1081
      ImageIndex = 11
      ShortCut = 49274
      OnExecute = ViewMailExecute
      OnUpdate = ViewMailUpdate
    end
    object AutoStartParams: TAction
      Category = #1060#1072#1081#1083
      Caption = #1055#1077#1088#1072#1084#1077#1090#1088#1099' '#1072#1074#1090#1086#1079#1072#1087#1091#1089#1082#1072
      Enabled = False
      Hint = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1087#1072#1088#1072#1084#1077#1088#1086#1074' '#1072#1074#1090#1086#1079#1072#1087#1091#1089#1082#1072
      OnExecute = AutoStartParamsExecute
      OnUpdate = AutoStartParamsUpdate
    end
  end
  object su_groups: TADOTable
    AutoCalcFields = False
    Connection = acDb
    LockType = ltReadOnly
    IndexName = 'pk_su_groups_id'
    TableName = 'su_groups'
    Left = 24
    Top = 76
    object su_groupsid: TIntegerField
      DisplayLabel = #1050#1086#1076' (id)'
      FieldName = 'id'
      Visible = False
    end
    object su_groupsowner_id: TIntegerField
      DisplayLabel = #1050#1086#1076' '#1074#1074#1077#1088#1093#1085#1077#1075#1086' '#1091#1088#1086#1074#1085#1103
      FieldName = 'owner_id'
      Visible = False
    end
    object su_groupsname: TStringField
      DisplayLabel = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
      FieldName = 'name'
      Size = 64
    end
    object su_groupsnotes: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      FieldName = 'notes'
      Size = 255
    end
  end
  object su_users: TADOTable
    AutoCalcFields = False
    Connection = acDb
    LockType = ltReadOnly
    IndexName = 'pk_su_users_id'
    TableName = 'su_users'
    Left = 108
    Top = 76
    object su_usersid: TIntegerField
      DisplayLabel = #1050#1086#1076' (id)'
      FieldName = 'id'
      Visible = False
    end
    object su_usersid_groups: TIntegerField
      DisplayLabel = #1050#1086#1076' '#1075#1088#1091#1087#1087#1099
      FieldName = 'id_groups'
      Visible = False
    end
    object su_usersname: TStringField
      DisplayLabel = #1051#1086#1075#1080#1085
      FieldName = 'name'
      Size = 16
    end
    object su_usersfullname: TStringField
      DisplayLabel = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103
      FieldName = 'fullname'
      Size = 64
    end
    object su_usersnotes: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      FieldName = 'notes'
      Size = 64
    end
    object su_usersdeleted: TBooleanField
      DisplayLabel = #1059#1076#1072#1083#1077#1085
      FieldName = 'deleted'
      DisplayValues = #1059#1044#1040#1051#1045#1053';'
    end
    object su_usersblocked: TBooleanField
      DisplayLabel = #1041#1083#1086#1082#1080#1088#1086#1074#1072#1085
      FieldName = 'blocked'
      DisplayValues = #1041#1051#1054#1050';'
    end
  end
  object sr_workplases: TADOTable
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    IndexName = 'pk_sr_workplases_id'
    TableName = 'sr_workplases'
    Left = 192
    Top = 76
    object sr_workplasesid: TIntegerField
      DisplayLabel = #1050#1086#1076' (id)'
      FieldName = 'id'
    end
    object sr_workplasesname_s: TStringField
      DisplayLabel = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1040#1056#1052
      FieldName = 'name_s'
      Size = 16
    end
    object sr_workplasesname: TStringField
      DisplayLabel = #1055#1086#1083#1085#1086#1077' '#1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1040#1056#1052
      FieldName = 'name'
      Size = 128
    end
  end
end
