inherited FormGroupGrp: TFormGroupGrp
  Left = 600
  Top = 426
  ActiveControl = cbFields
  Caption = #1042#1099#1073#1086#1088' '#1089#1090#1086#1083#1073#1094#1072
  ClientHeight = 124
  ClientWidth = 392
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 73
    Width = 392
  end
  object lblFields: TLabel [1]
    Left = 12
    Top = 12
    Width = 185
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1089#1090#1086#1083#1073#1077#1094' '#1076#1083#1103' '#1075#1088#1091#1087#1087#1080#1088#1086#1074#1082#1080':'
    FocusControl = cbFields
  end
  inherited ButtonsPanel: TPanel
    Top = 75
    Width = 392
    TabOrder = 1
    inherited ButtonsMovedPanel: TPanel
      Left = 170
    end
  end
  object cbFields: TComboBox
    Left = 12
    Top = 28
    Width = 369
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = CheckButtons
    OnClick = CheckButtons
  end
end
