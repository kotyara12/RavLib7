inherited FormSrOpersProp: TFormSrOpersProp
  HelpKeyword = 'IDH_SROPERATIONSPROP'
  ClientHeight = 331
  ClientWidth = 473
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 280
    Width = 473
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
    Top = 108
    Width = 142
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103':'
    FocusControl = deNotes
  end
  object lblId: TLabel [3]
    Left = 12
    Top = 12
    Width = 39
    Height = 13
    Caption = #1050#1086#1076' (id):'
    FocusControl = deId
  end
  object lblNameLevels: TLabel [4]
    Left = 88
    Top = 60
    Width = 169
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1075#1088#1091#1087#1087#1099' '#1086#1087#1077#1088#1072#1094#1080#1081':'
    FocusControl = deNameLevels
  end
  object lblNotesLevels: TLabel [5]
    Left = 12
    Top = 188
    Width = 232
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103' '#1075#1088#1091#1087#1087#1099' '#1086#1087#1077#1088#1072#1094#1080#1081':'
    FocusControl = deNotesLevels
  end
  object lblHidden: TLabel [6]
    Left = 12
    Top = 60
    Width = 37
    Height = 13
    Caption = #1060#1083#1072#1075#1080':'
    FocusControl = deHidden
  end
  inherited ButtonsPanel: TPanel
    Top = 282
    Width = 473
    TabOrder = 6
    inherited ButtonsMovedPanel: TPanel
      Left = 251
    end
  end
  object deName: TDBEdit [8]
    Left = 88
    Top = 28
    Width = 373
    Height = 21
    DataField = 'name'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object deNotes: TDBMemo [9]
    Left = 12
    Top = 124
    Width = 449
    Height = 53
    DataField = 'notes'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 4
    WantReturns = False
  end
  object deId: TDBEdit [10]
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
  object deNameLevels: TDBEdit [11]
    Left = 88
    Top = 76
    Width = 373
    Height = 21
    DataField = 'name_levels'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 3
  end
  object deNotesLevels: TDBMemo [12]
    Left = 12
    Top = 204
    Width = 449
    Height = 53
    DataField = 'notes_levels'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 5
    WantReturns = False
  end
  object deHidden: TDBEdit [13]
    Left = 12
    Top = 76
    Width = 65
    Height = 21
    DataField = 'hidden'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 2
  end
  inherited DataSource: TDataSource
    DataSet = AdminData.sr_operations
  end
end
