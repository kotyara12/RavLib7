inherited FormGroupFnc: TFormGroupFnc
  Left = 600
  Top = 426
  ActiveControl = cbFields
  Caption = #1042#1099#1073#1086#1088' '#1089#1090#1086#1083#1073#1094#1072
  ClientHeight = 171
  ClientWidth = 392
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 120
    Width = 392
  end
  object lblFields: TLabel [1]
    Left = 12
    Top = 12
    Width = 248
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1089#1090#1086#1083#1073#1077#1094' '#1076#1083#1103' '#1074#1099#1095#1080#1089#1083#1077#1085#1080#1103' '#1088#1077#1079#1091#1083#1100#1090#1072#1090#1086#1074':'
    FocusControl = cbFields
  end
  object lblFunc: TLabel [2]
    Left = 12
    Top = 60
    Width = 101
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1092#1091#1085#1082#1094#1080#1102':'
    FocusControl = cbFunc
  end
  inherited ButtonsPanel: TPanel
    Top = 122
    Width = 392
    TabOrder = 2
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
  object cbFunc: TComboBox
    Left = 12
    Top = 76
    Width = 369
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = CheckButtons
    OnClick = CheckButtons
  end
end
