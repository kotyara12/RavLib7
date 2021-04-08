inherited FormDbEditor: TFormDbEditor
  Left = 570
  Top = 395
  ActiveControl = deName
  ClientHeight = 207
  ClientWidth = 518
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 156
    Width = 518
  end
  object lblId: TLabel [1]
    Left = 12
    Top = 12
    Width = 42
    Height = 13
    Caption = #1050#1086#1076' (ID):'
    FocusControl = deId
  end
  object lblName: TLabel [2]
    Left = 104
    Top = 12
    Width = 79
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077':'
    FocusControl = deName
  end
  object lblNotes: TLabel [3]
    Left = 12
    Top = 60
    Width = 142
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103':'
    FocusControl = deNotes
  end
  inherited ButtonsPanel: TPanel
    Top = 158
    Width = 518
    TabOrder = 3
    inherited ButtonsMovedPanel: TPanel
      Left = 296
    end
  end
  object deId: TDBEdit [5]
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
  object deName: TDBEdit [6]
    Left = 104
    Top = 28
    Width = 401
    Height = 21
    DataField = 'NAME'
    DataSource = DataSource
    TabOrder = 1
  end
  object deNotes: TDBMemo [7]
    Left = 12
    Top = 76
    Width = 493
    Height = 53
    DataField = 'NOTES'
    DataSource = DataSource
    TabOrder = 2
    WantReturns = False
  end
end
