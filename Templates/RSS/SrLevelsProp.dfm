inherited FormSrLevelsProp: TFormSrLevelsProp
  Left = 305
  Top = 207
  HelpKeyword = 'IDH_SRLEVELSPROP'
  ActiveControl = deName
  ClientHeight = 398
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 347
  end
  object lblName: TLabel [1]
    Left = 88
    Top = 12
    Width = 79
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077':'
    FocusControl = deName
  end
  object lblNotes: TLabel [2]
    Left = 12
    Top = 60
    Width = 142
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103':'
    FocusControl = deNotes
  end
  object lblId: TLabel [3]
    Left = 12
    Top = 12
    Width = 65
    Height = 13
    AutoSize = False
    Caption = #1050#1086#1076' (ID):'
    FocusControl = deId
  end
  inherited ButtonsPanel: TPanel
    Top = 349
    TabOrder = 5
  end
  inherited ColorsGroupBox: TGroupBox
    Top = 168
    TabOrder = 4
  end
  object deName: TDBEdit [6]
    Left = 88
    Top = 28
    Width = 297
    Height = 21
    DataField = 'name'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object deNotes: TDBMemo [7]
    Left = 12
    Top = 76
    Width = 373
    Height = 53
    DataField = 'notes'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
    WantReturns = False
  end
  object deHidden: TDBCheckBox [8]
    Left = 12
    Top = 140
    Width = 373
    Height = 17
    Caption = #1057#1050#1056' - '#1054#1087#1077#1088#1072#1094#1080#1080' '#1075#1088#1091#1087#1087#1099' '#1085#1077' '#1086#1090#1086#1073#1088#1072#1078#1072#1102#1090#1089#1103' '#1074' '#1089#1087#1080#1089#1082#1072#1093
    DataField = 'hidden'
    DataSource = DataSource
    Enabled = False
    ReadOnly = True
    TabOrder = 3
    ValueChecked = 'True'
    ValueUnchecked = 'False'
  end
  object deId: TDBEdit [9]
    Left = 12
    Top = 28
    Width = 65
    Height = 21
    DataField = 'id'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  inherited DataSource: TDataSource
    DataSet = AdminData.sr_levels
    Left = 8
    Top = 8
  end
  inherited ColorDialog: TColorDialog
    Left = 36
    Top = 8
  end
end
