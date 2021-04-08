inherited DbSelKeyTemplate: TDbSelKeyTemplate
  Left = 806
  Top = 363
  ActiveControl = ItemsBox
  Caption = 'DbSelKeyTemplate'
  ClientHeight = 258
  ClientWidth = 353
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 207
    Width = 353
  end
  object ItemsDBLookupComboBoxLabel: TLabel [1]
    Left = 12
    Top = 12
    Width = 40
    Height = 13
    Caption = #1047#1072#1087#1080#1089#1100':'
    FocusControl = ItemsBox
  end
  object RefButton: TSpeedButton [2]
    Left = 320
    Top = 28
    Width = 21
    Height = 21
    Hint = #1042#1099#1073#1086#1088' '#1079#1085#1072#1095#1077#1085#1080#1103' '#1080#1079' '#1089#1087#1088#1072#1074#1086#1095#1085#1080#1082#1072
  end
  inherited ButtonsPanel: TPanel
    Top = 209
    Width = 353
    inherited ButtonsMovedPanel: TPanel
      Left = 131
    end
  end
  object ItemsBox: TDBLookupComboBox
    Left = 12
    Top = 28
    Width = 305
    Height = 21
    ListSource = DataSource
    TabOrder = 1
    OnClick = UpdateButtons
  end
  object InfoGroupBox: TGroupBox
    Left = 12
    Top = 64
    Width = 329
    Height = 121
    Caption = ' '#1057#1074#1077#1076#1077#1085#1080#1103' '#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080' '
    TabOrder = 2
  end
  object DataSource: TDataSource
    AutoEdit = False
    Left = 4
    Top = 172
  end
end
