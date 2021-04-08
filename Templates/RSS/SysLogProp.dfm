inherited FormSysLogProp: TFormSysLogProp
  Left = 367
  Top = 231
  HelpKeyword = 'IDH_SYSLOG_PROP'
  ClientHeight = 537
  ClientWidth = 588
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 486
    Width = 588
  end
  object lblDatetime: TLabel [1]
    Left = 12
    Top = 12
    Width = 73
    Height = 13
    Caption = #1044#1072#1090#1072' '#1080' '#1074#1088#1077#1084#1103':'
    FocusControl = deDatetime
  end
  object lblInfo: TLabel [2]
    Left = 12
    Top = 56
    Width = 109
    Height = 13
    Caption = #1057#1086#1086#1073#1097#1077#1085#1080#1077' '#1089#1080#1089#1090#1077#1084#1099':'
    FocusControl = deInfo
  end
  object lblHost: TLabel [3]
    Left = 172
    Top = 12
    Width = 91
    Height = 13
    Caption = #1048#1084#1103' '#1082#1086#1084#1087#1100#1102#1090#1077#1088#1072':'
    FocusControl = deHost
  end
  object lblNetUser: TLabel [4]
    Left = 380
    Top = 12
    Width = 142
    Height = 13
    Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100' '#1082#1086#1084#1087#1100#1102#1090#1077#1088#1072':'
    FocusControl = deNetUser
  end
  inherited ButtonsPanel: TPanel
    Top = 488
    Width = 588
    TabOrder = 7
    inherited ButtonsMovedPanel: TPanel
      Left = 366
    end
  end
  object deDatetime: TDBEdit [6]
    Left = 12
    Top = 28
    Width = 149
    Height = 21
    DataField = 'dateoper'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object deInfo: TDBMemo [7]
    Left = 12
    Top = 72
    Width = 565
    Height = 89
    DataField = 'info'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
    WantReturns = False
  end
  object OperationsGroupBox: TGroupBox [8]
    Left = 12
    Top = 168
    Width = 565
    Height = 109
    Caption = ' '#1054#1087#1077#1088#1072#1094#1080#1103' '
    TabOrder = 4
    object lblIdOpertions: TLabel
      Left = 12
      Top = 16
      Width = 73
      Height = 13
      Caption = #1050#1086#1076' '#1086#1087#1077#1088#1072#1094#1080#1080':'
      FocusControl = deIdOpertions
    end
    object lblNameOperations: TLabel
      Left = 136
      Top = 16
      Width = 130
      Height = 13
      Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1086#1087#1077#1088#1072#1094#1080#1080':'
      FocusControl = deNameOperations
    end
    object lblNotesOperations: TLabel
      Left = 12
      Top = 60
      Width = 208
      Height = 13
      Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1086#1073' '#1086#1087#1077#1088#1072#1094#1080#1080':'
      FocusControl = deNotesOperations
    end
    object deIdOpertions: TDBEdit
      Left = 12
      Top = 32
      Width = 113
      Height = 21
      DataField = 'id_operations'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
    object deNameOperations: TDBEdit
      Left = 136
      Top = 32
      Width = 417
      Height = 21
      DataField = 'name_operations'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
    end
    object deNotesOperations: TDBEdit
      Left = 12
      Top = 76
      Width = 541
      Height = 21
      DataField = 'notes_operations'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
    end
  end
  object GroupsGroupBox: TGroupBox [9]
    Left = 12
    Top = 280
    Width = 565
    Height = 113
    Caption = ' '#1043#1088#1091#1087#1087#1072' '#1073#1077#1079#1086#1087#1072#1089#1085#1086#1089#1090#1080' '#1086#1087#1077#1088#1072#1094#1080#1081' '
    TabOrder = 5
    object lblIdLevels: TLabel
      Left = 12
      Top = 20
      Width = 61
      Height = 13
      Caption = #1050#1086#1076' '#1075#1088#1091#1087#1087#1099':'
      FocusControl = deIdLevels
    end
    object lblNameLevels: TLabel
      Left = 136
      Top = 20
      Width = 169
      Height = 13
      Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1075#1088#1091#1087#1087#1099' '#1086#1087#1077#1088#1072#1094#1080#1081':'
      FocusControl = deNameLevels
    end
    object lblNotesLevels: TLabel
      Left = 12
      Top = 64
      Width = 239
      Height = 13
      Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1086' '#1075#1088#1091#1087#1087#1077' '#1086#1087#1077#1088#1072#1094#1080#1081':'
      FocusControl = deNotesLevels
    end
    object deIdLevels: TDBEdit
      Left = 12
      Top = 36
      Width = 113
      Height = 21
      DataField = 'id_levels'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
    object deNameLevels: TDBEdit
      Left = 136
      Top = 36
      Width = 417
      Height = 21
      DataField = 'name_levels'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
    end
    object deNotesLevels: TDBEdit
      Left = 12
      Top = 80
      Width = 541
      Height = 21
      DataField = 'notes_levels'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
    end
  end
  object UserGroupBox: TGroupBox [10]
    Left = 12
    Top = 396
    Width = 565
    Height = 69
    Caption = ' '#1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100' '#1089#1080#1089#1090#1077#1084#1099', '#1074#1099#1087#1086#1083#1085#1080#1074#1096#1080#1081' '#1086#1087#1077#1088#1072#1094#1080#1102' '
    TabOrder = 6
    object lblLogin: TLabel
      Left = 12
      Top = 20
      Width = 34
      Height = 13
      Caption = #1051#1086#1075#1080#1085':'
      FocusControl = deLogin
    end
    object lblFullname: TLabel
      Left = 136
      Top = 20
      Width = 147
      Height = 13
      Caption = #1048#1084#1103' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1103' '#1089#1080#1089#1090#1077#1084#1099':'
      FocusControl = deFullname
    end
    object lblNameWorkplases: TLabel
      Left = 388
      Top = 20
      Width = 62
      Height = 13
      Caption = #1055#1088#1086#1075#1088#1072#1084#1084#1072':'
      FocusControl = deNameWorkplases
    end
    object deLogin: TDBEdit
      Left = 12
      Top = 36
      Width = 113
      Height = 21
      DataField = 'login'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 0
    end
    object deFullname: TDBEdit
      Left = 136
      Top = 36
      Width = 241
      Height = 21
      DataField = 'fullname'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 1
    end
    object deNameWorkplases: TDBEdit
      Left = 388
      Top = 36
      Width = 165
      Height = 21
      DataField = 'name_s_wp'
      DataSource = DataSource
      ParentColor = True
      ReadOnly = True
      TabOrder = 2
    end
  end
  object deHost: TDBEdit [11]
    Left = 172
    Top = 28
    Width = 197
    Height = 21
    DataField = 'host'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object deNetUser: TDBEdit [12]
    Left = 380
    Top = 28
    Width = 197
    Height = 21
    DataField = 'netuser'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  inherited DataSource: TDataSource
    DataSet = FormSysLog.ss_syslog
  end
end
