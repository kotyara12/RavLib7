inherited FormTreeGroup: TFormTreeGroup
  Left = 570
  Top = 395
  ActiveControl = deNameS
  ClientHeight = 284
  ClientWidth = 425
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 233
    Width = 425
  end
  object lblId: TLabel [1]
    Left = 12
    Top = 12
    Width = 42
    Height = 13
    Caption = #1050#1086#1076' (ID):'
    FocusControl = deId
  end
  object lblOwnerId: TLabel [2]
    Left = 104
    Top = 12
    Width = 52
    Height = 13
    Caption = #1042#1083#1072#1076#1077#1083#1077#1094':'
    FocusControl = deOwnerId
  end
  object lblNameS: TLabel [3]
    Left = 12
    Top = 56
    Width = 123
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1082#1088#1072#1090#1082#1086#1077':'
    FocusControl = deNameS
  end
  object lblNotes: TLabel [4]
    Left = 12
    Top = 144
    Width = 142
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103':'
    FocusControl = deNotes
  end
  object Label1: TLabel [5]
    Left = 12
    Top = 100
    Width = 118
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1087#1086#1083#1085#1086#1077':'
    FocusControl = deNameF
  end
  inherited ButtonsPanel: TPanel
    Top = 235
    Width = 425
    TabOrder = 5
    inherited ButtonsMovedPanel: TPanel
      Left = 203
    end
  end
  object deId: TDBEdit [7]
    Left = 12
    Top = 28
    Width = 81
    Height = 21
    DataField = 'id'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object deOwnerId: TDBEdit [8]
    Left = 104
    Top = 28
    Width = 81
    Height = 21
    DataField = 'owner_id'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object deNameS: TDBEdit [9]
    Left = 12
    Top = 72
    Width = 401
    Height = 21
    DataField = 'name_s'
    DataSource = DataSource
    TabOrder = 2
  end
  object deNotes: TDBMemo [10]
    Left = 12
    Top = 160
    Width = 401
    Height = 53
    DataField = 'notes'
    DataSource = DataSource
    TabOrder = 4
    WantReturns = False
  end
  object deNameF: TDBEdit [11]
    Left = 12
    Top = 116
    Width = 401
    Height = 21
    DataField = 'name_f'
    DataSource = DataSource
    TabOrder = 3
  end
end
