inherited FormTreeGroup: TFormTreeGroup
  Left = 570
  Top = 395
  ActiveControl = deName
  ClientHeight = 255
  ClientWidth = 427
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 204
    Width = 427
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
  object lblName: TLabel [3]
    Left = 12
    Top = 60
    Width = 79
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077':'
    FocusControl = deName
  end
  object lblNotes: TLabel [4]
    Left = 12
    Top = 108
    Width = 142
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103':'
    FocusControl = deNotes
  end
  inherited ButtonsPanel: TPanel
    Top = 206
    Width = 427
    TabOrder = 4
    inherited ButtonsMovedPanel: TPanel
      Left = 205
    end
  end
  object deId: TDBEdit [6]
    Left = 12
    Top = 28
    Width = 81
    Height = 21
    DataField = 'ID'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object deOwnerId: TDBEdit [7]
    Left = 104
    Top = 28
    Width = 81
    Height = 21
    DataField = 'OWNER_ID'
    DataSource = DataSource
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
  object deName: TDBEdit [8]
    Left = 12
    Top = 76
    Width = 401
    Height = 21
    DataField = 'NAME'
    DataSource = DataSource
    TabOrder = 2
  end
  object deNotes: TDBMemo [9]
    Left = 12
    Top = 124
    Width = 401
    Height = 53
    DataField = 'NOTES'
    DataSource = DataSource
    TabOrder = 3
    WantReturns = False
  end
end
