inherited DbSelectTemplate: TDbSelectTemplate
  Left = 432
  Top = 285
  Caption = 'DbSelectTemplate'
  ClientHeight = 303
  ClientWidth = 353
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 252
    Width = 353
  end
  object GroupsDBLookupComboBoxLabel: TLabel [1]
    Left = 12
    Top = 12
    Width = 38
    Height = 13
    Caption = #1043#1088#1091#1087#1087#1072':'
    FocusControl = GroupsBox
  end
  object RefButton: TSpeedButton [2]
    Left = 320
    Top = 76
    Width = 21
    Height = 21
    Hint = #1042#1099#1073#1086#1088' '#1079#1085#1072#1095#1077#1085#1080#1103' '#1080#1079' '#1089#1087#1088#1072#1074#1086#1095#1085#1080#1082#1072
  end
  object ItemsDBLookupComboBoxLabel: TLabel [3]
    Left = 12
    Top = 60
    Width = 40
    Height = 13
    Caption = #1047#1072#1087#1080#1089#1100':'
    FocusControl = ItemsBox
  end
  inherited ButtonsPanel: TPanel
    Top = 254
    Width = 353
    TabOrder = 3
    inherited ButtonsMovedPanel: TPanel
      Left = 131
    end
  end
  object InfoGroupBox: TGroupBox
    Left = 12
    Top = 112
    Width = 329
    Height = 121
    Caption = ' '#1057#1074#1077#1076#1077#1085#1080#1103' '#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080' '
    TabOrder = 2
  end
  object GroupsBox: TDBLookupComboBox
    Left = 12
    Top = 28
    Width = 329
    Height = 21
    ListSource = GroupsDataSource
    TabOrder = 0
  end
  object ItemsBox: TDBLookupComboBox
    Left = 12
    Top = 76
    Width = 305
    Height = 21
    ListSource = ItemsDataSource
    TabOrder = 1
  end
  object GroupsDataSource: TDataSource
    AutoEdit = False
    DataSet = GROUPS_DATA
    Left = 268
  end
  object ItemsDataSource: TDataSource
    AutoEdit = False
    DataSet = ITEMS_DATA
    Left = 324
  end
  object ITEMS_DATA: TADOQuery
    AutoCalcFields = False
    Connection = BaseDataTemplate.acDb
    LockType = ltReadOnly
    AfterScroll = ItemsScroll
    DataSource = GroupsDataSource
    Parameters = <>
    Prepared = True
    Left = 296
  end
  object GROUPS_DATA: TADOQuery
    AutoCalcFields = False
    Connection = BaseDataTemplate.acDb
    LockType = ltReadOnly
    AfterScroll = GroupsScroll
    Parameters = <>
    Prepared = True
    Left = 240
  end
end
