inherited FormTestings: TFormTestings
  Left = 517
  Top = 339
  Caption = #1050#1080#1085#1077#1079#1080#1086#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1077
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000454545FF454545FF454545FF454545FF454545FF454545FF454545FF4545
    45FF454545FF454545FF454545FF454545FF0000000000000000000000000000
    0000525252FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFF707070FF0000000000000000000000000000
    0000535353FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBED7
    C9FF5E9B7AFF328157FF338459FF3C7D5AFF2B84524D00000000000000000000
    0000535353FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8DB8A1FF2C83
    55FF3FA86FFF47BA7DFF47BA7DFF3FAB71FF30905CFF2B8B5887000000000000
    0000545454FFFFFFFFFFFFFFFFFF8B8F90FF8B9091FF6D8A7DFF2C8454FF47B9
    7CFF49BD7FFF49BD7FFF49BD7FFF4BBE80FF4ABC7FFF319660FF2B935A4C0000
    0000575858FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5C9B79FF3FA970FF49BD
    7FFF9EDCBBFFFFFFFFFFA4DEBFFF4FC083FF50C185FF46B379FF2E965CC10000
    00005B5C5CFFFFFFFFFFFFFFFFFF8F9495FF909596FF2D8054FF49BB7EFF9FDD
    BCFFFFFFFFFFFFFFFFFFFFFFFFFF8CD7AEFF54C387FF54C287FF2F9B5FF30000
    00005E6060FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF34895BFF4DBD81FFFFFF
    FFFFAFE3C7FF53C387FFACE2C5FFF2FAF6FF65CA93FF57C58AFF309F5FF30000
    0000626464FFFFFFFFFFFFFFFFFF949A9BFF959B9CFF458D67FF46B177FF54C3
    87FF56C489FF57C58AFF59C68BFFB3E5CAFFC9EDD9FF4FBC81FF2FA264C10000
    0000656768FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0DDCDFF329761FF55C3
    87FF59C78CFF5BC88DFF5CC88EFF5EC98FFF5CC78DFF39AB6DFF32A7684C0000
    0000696D6E91696D6DFF6B6F70FF6C7071FFFFFFFFFF9BA2A3FF619C7FFF359E
    65FF4EBA80FF5DC88EFF5DC88EFF50BE83FF39AC6EFF31A66787000000000000
    0000000000008B8F91898D9293FF8E9495FFFFFFFFFFFFFFFFFFFFFFFFFFC0E1
    CFFF65B78BFF3CA66DFF3CAA6FFF4CA576FF31A5664D00000000000000000000
    000000000000000000008F969689929899FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFF9BA2A4FF0000000000000000000000000000
    0000000000000000000000000000969C9C89989FA0FF99A1A2FF9AA2A3FF9CA3
    A5FF9DA5A6FF9EA6A7FF9EA7A8FF9FA7A8FF0000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000800700008007000080030000800100008000000080000000800000008000
    0000800000008000000080010000C0030000E0070000F0070000FFFF0000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited DataPanel: TPanel
    inherited DbGrid: TRDbStyledGrid
      Columns = <
        item
          Expanded = False
          FieldName = 'fullname'
          Title.Alignment = taCenter
          Width = 200
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'birthday'
          Title.Alignment = taCenter
          Width = 90
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'phone'
          Title.Alignment = taCenter
          Width = 100
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'email'
          Title.Alignment = taCenter
          Width = 150
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'messenger'
          Title.Alignment = taCenter
          Width = 100
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'specialty'
          Title.Alignment = taCenter
          Width = 100
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'test_date_1'
          Title.Alignment = taCenter
          Title.Caption = #1058#1077#1089#1090' 1'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'test_date_2'
          Title.Alignment = taCenter
          Title.Caption = #1058#1077#1089#1090' 2'
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'testing_cost'
          Title.Alignment = taCenter
          Title.Caption = #1057#1090#1086#1080#1084#1086#1089#1090#1100
          Width = 100
          Visible = True
        end
        item
          Expanded = False
          FieldName = 'notes'
          Visible = True
        end>
    end
  end
  inherited ActionList: TActionList
    Top = 108
  end
  inherited RDbFilter: TRDbFilter
    StoreInIniFile = True
    DataSet = testings
    Top = 108
    object RDbFilter_id: TRDFIntegerItem
      Operation = loAnd
      FieldName = 'id'
      FieldCaption = #1050#1086#1076' (id)'
    end
    object RDbFilter_fullname: TRDFStringItem
      Operation = loAnd
      FieldName = 'fullname'
      FieldCaption = #1055#1086#1083#1085#1086#1077' '#1080#1084#1103
    end
    object RDbFilter_birthday: TRDFDateItem
      Operation = loAnd
      FieldName = 'birthday'
      FieldCaption = #1044#1072#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103
      CompareMode = emdPeriod
    end
    object RDbFilter_phone: TRDFStringItem
      Operation = loAnd
      FieldName = 'phone'
      FieldCaption = #1058#1077#1083#1077#1092#1086#1085'('#1099')'
    end
    object RDbFilter_email: TRDFStringItem
      Operation = loAnd
      FieldName = 'email'
      FieldCaption = #1040#1076#1088#1077#1089' '#1101#1083#1077#1082#1090#1088#1086#1085#1085#1086#1081' '#1087#1086#1095#1090#1099
    end
    object RDbFilter_messenger: TRDFStringItem
      Operation = loAnd
      FieldName = 'messenger'
      FieldCaption = #1052#1077#1089#1089#1077#1085#1076#1078#1077#1088
    end
    object RDbFilter_specialty: TRDFStringItem
      Operation = loAnd
      FieldName = 'specialty'
      FieldCaption = #1057#1087#1077#1094#1080#1072#1083#1100#1085#1086#1089#1090#1100
    end
    object RDbFilter_features: TRDFTextItem
      Operation = loAnd
      FieldName = 'features'
      FieldCaption = #1054#1089#1086#1073#1077#1085#1085#1086#1089#1090#1080
    end
    object RDbFilter_test_date_1: TRDFDateItem
      Operation = loAnd
      FieldName = 'test_date_1'
      FieldCaption = #1044#1072#1090#1072' '#1087#1077#1088#1074#1080#1095#1085#1086#1075#1086' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
    end
    object RDbFilter_test_date_2: TRDFDateItem
      Active = True
      Operation = loAnd
      FieldName = 'test_date_2'
      FieldCaption = #1044#1072#1090#1072' '#1087#1086#1074#1090#1086#1088#1085#1086#1075#1086' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
      CompareMode = emdAfterCurrentDay
    end
    object RDbFilter_testing_cost: TRDFFloatItem
      Operation = loAnd
      FieldName = 'testing_cost'
      FieldCaption = #1057#1090#1086#1080#1084#1086#1089#1090#1100' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
    end
    object RDbFilter_notes: TRDFStringItem
      Operation = loAnd
      FieldName = 'notes'
      FieldCaption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
    end
  end
  inherited RDbOrder: TRDbOrder
    StoreInIniFile = True
    DataSet = testings
    OrderString = 'fullname'
    Top = 108
  end
  inherited RDbFind: TRDbSearch
    StoreInIniFile = True
    DataSet = testings
    DefaultField = 'fullname'
    KeyField = 'id'
    ListFields = 'fullname;notes'
    Top = 108
  end
  inherited RDbGridTuner: TRDbGridTuner
    Top = 108
  end
  inherited RDbFilterStatus: TRDbFilterStatus
    Top = 108
  end
  inherited PopupMenu: TPopupMenu
    Top = 108
  end
  inherited MainMenu: TMainMenu
    Top = 108
  end
  inherited DataPopupMenu: TPopupMenu
    Top = 108
  end
  inherited OperationsPopupMenu: TPopupMenu
    Top = 108
  end
  inherited ReportsPopupMenu: TPopupMenu
    Top = 108
  end
  inherited RDbEditor: TRDbExportEditor
    DataSet = testings
    CheckTags = False
    CopiedFields = 
      'fullname;birthday;phone;email;messenger;specialty;features;test_' +
      'date_1;test_date_2;testing_cost;notes'
    LogEnable = False
    StatisticFields = 'messenger;specialty'
    OnGetEditorClass = RDbEditorGetEditorClass
    OnCreateNewRecord = RDbEditorCreateNewRecord
    OnGetEditRights = RDbEditorGetEditRights
    Top = 108
  end
  inherited TitleGridPopupMenu: TPopupMenu
    Top = 108
  end
  inherited RDbLocate: TRDbFind
    StoreInIniFile = True
    DataSet = testings
    DefaultField = 'fullname'
    Top = 108
  end
  inherited RDbUpdater: TRDbUpdater
    Top = 108
    object RDbUpdater_fullname: TRDUStringItem
      FieldName = 'fullname'
      FieldCaption = #1055#1086#1083#1085#1086#1077' '#1080#1084#1103
    end
    object RDbUpdater_birthday: TRDUDateTimeItem
      FieldName = 'birthday'
      FieldCaption = #1044#1072#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103
    end
    object RDbUpdater_phone: TRDUStringItem
      FieldName = 'phone'
      FieldCaption = #1058#1077#1083#1077#1092#1086#1085'('#1099')'
    end
    object RDbUpdater_email: TRDUStringItem
      FieldName = 'email'
      FieldCaption = #1040#1076#1088#1077#1089' '#1101#1083#1077#1082#1090#1088#1086#1085#1085#1086#1081' '#1087#1086#1095#1090#1099
    end
    object RDbUpdater_messenger: TRDUStringItem
      FieldName = 'messenger'
      FieldCaption = #1052#1077#1089#1089#1077#1085#1076#1078#1077#1088
    end
    object RDbUpdater_specialty: TRDUStringItem
      FieldName = 'specialty'
      FieldCaption = #1057#1087#1077#1094#1080#1072#1083#1100#1085#1086#1089#1090#1100
    end
    object RDbUpdater_features: TRDUStringItem
      FieldName = 'features'
      FieldCaption = #1054#1089#1086#1073#1077#1085#1085#1086#1089#1090#1080
    end
    object RDbUpdater_test_date_1: TRDUDateTimeItem
      FieldName = 'test_date_1'
      FieldCaption = #1044#1072#1090#1072' '#1087#1077#1088#1074#1080#1095#1085#1086#1075#1086' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
    end
    object RDbUpdater_test_date_2: TRDUDateTimeItem
      FieldName = 'test_date_2'
      FieldCaption = #1044#1072#1090#1072' '#1087#1086#1074#1090#1086#1088#1085#1086#1075#1086' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
    end
    object RDbUpdater_testing_cost: TRDUFloatItem
      FieldName = 'testing_cost'
      FieldCaption = #1057#1090#1086#1080#1084#1086#1089#1090#1100' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
      DisplayFormat = '%.2n'
    end
    object RDbUpdater_notes: TRDUStringItem
      FieldName = 'notes'
      FieldCaption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
    end
  end
  inherited InfoPanelPopupMenu: TPopupMenu
    Top = 108
  end
  inherited ViewsPopupMenu: TPopupMenu
    Left = 480
    Top = 108
  end
  object testings: TADOQuery
    Connection = BaseData.acDb
    Parameters = <>
    SQL.Strings = (
      'select * from testings')
    Left = 508
    Top = 108
    object testingsid: TIntegerField
      DisplayLabel = #1050#1086#1076' (id)'
      FieldName = 'id'
      Required = True
      Visible = False
    end
    object testingsfullname: TWideStringField
      DisplayLabel = #1055#1086#1083#1085#1086#1077' '#1080#1084#1103
      FieldName = 'fullname'
      Required = True
      Size = 128
    end
    object testingsbirthday: TDateTimeField
      Alignment = taCenter
      DisplayLabel = #1044#1072#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103
      FieldName = 'birthday'
      Required = True
    end
    object testingsphone: TWideStringField
      DisplayLabel = #1058#1077#1083#1077#1092#1086#1085'('#1099')'
      FieldName = 'phone'
      Size = 32
    end
    object testingsemail: TWideStringField
      DisplayLabel = #1040#1076#1088#1077#1089' '#1101#1083#1077#1082#1090#1088#1086#1085#1085#1086#1081' '#1087#1086#1095#1090#1099
      FieldName = 'email'
      Size = 64
    end
    object testingsmessenger: TWideStringField
      DisplayLabel = #1052#1077#1089#1089#1077#1085#1076#1078#1077#1088
      FieldName = 'messenger'
      Size = 32
    end
    object testingsspecialty: TWideStringField
      DisplayLabel = #1057#1087#1077#1094#1080#1072#1083#1100#1085#1086#1089#1090#1100
      FieldName = 'specialty'
      Size = 64
    end
    object testingsfeatures: TMemoField
      DisplayLabel = #1054#1089#1086#1073#1077#1085#1085#1086#1089#1090#1080
      FieldName = 'features'
      BlobType = ftMemo
    end
    object testingstest_date_1: TDateTimeField
      Alignment = taCenter
      DisplayLabel = #1044#1072#1090#1072' '#1087#1077#1088#1074#1080#1095#1085#1086#1075#1086' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
      FieldName = 'test_date_1'
    end
    object testingstest_date_2: TDateTimeField
      Alignment = taCenter
      DisplayLabel = #1044#1072#1090#1072' '#1087#1086#1074#1090#1086#1088#1085#1086#1075#1086' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
      FieldName = 'test_date_2'
    end
    object testingstesting_cost: TBCDField
      DisplayLabel = #1057#1090#1086#1080#1084#1086#1089#1090#1100' '#1090#1077#1089#1090#1080#1088#1086#1074#1072#1085#1080#1103
      FieldName = 'testing_cost'
      DisplayFormat = ',0.00'
      EditFormat = '0.00'
      Precision = 19
    end
    object testingsnotes: TWideStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      FieldName = 'notes'
      Size = 255
    end
  end
end
