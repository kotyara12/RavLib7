inherited FormImportField: TFormImportField
  Left = 558
  Top = 397
  Caption = #1048#1084#1087#1086#1088#1090#1080#1088#1091#1077#1084#1099#1077' '#1076#1072#1085#1085#1099#1077
  ClientHeight = 238
  ClientWidth = 333
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 187
    Width = 333
  end
  object FieldNameTextLabel: TLabel [1]
    Left = 12
    Top = 12
    Width = 98
    Height = 13
    Caption = #1048#1084#1103' '#1087#1086#1083#1103' '#1090#1072#1073#1083#1080#1094#1099':'
    FocusControl = FieldNameText
  end
  inherited ButtonsPanel: TPanel
    Top = 189
    Width = 333
    TabOrder = 5
    inherited HelpBtn: TBitBtn
      TabStop = False
    end
    inherited ButtonsMovedPanel: TPanel
      Left = 111
    end
  end
  object FieldComboBox: TComboBox
    Left = 32
    Top = 84
    Width = 289
    Height = 21
    Hint = #1048#1084#1103' '#1087#1086#1083#1103' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1080#1089#1090#1086#1095#1085#1080#1082#1072' '#1076#1072#1085#1085#1099#1093
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnCloseUp = FieldComboBoxCloseUp
  end
  object FieldRadioButton: TRadioButton
    Left = 12
    Top = 64
    Width = 309
    Height = 17
    Caption = #1048#1084#1087#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080#1079' '#1074#1085#1077#1096#1085#1077#1075#1086' '#1080#1089#1090#1086#1095#1085#1080#1082#1072
    Checked = True
    TabOrder = 1
    TabStop = True
  end
  object ConstRadioButton: TRadioButton
    Left = 12
    Top = 120
    Width = 309
    Height = 17
    Caption = #1047#1072#1087#1086#1083#1085#1080#1090#1100' '#1087#1086#1089#1090#1086#1103#1085#1085#1099#1084#1080' '#1076#1072#1085#1085#1099#1084#1080
    TabOrder = 3
  end
  object FieldNameText: TStaticText
    Left = 12
    Top = 28
    Width = 309
    Height = 21
    AutoSize = False
    BevelKind = bkSoft
    TabOrder = 0
  end
  object ConstComboBox: TComboBox
    Left = 32
    Top = 144
    Width = 289
    Height = 21
    Hint = #1047#1085#1072#1095#1077#1085#1080#1077' '#1082#1086#1085#1089#1090#1072#1085#1090#1099' ('#1074' '#1089#1090#1088#1086#1082#1086#1074#1086#1084' '#1074#1080#1076#1077')'
    ItemHeight = 13
    TabOrder = 4
    OnChange = ConstComboBoxChange
    OnCloseUp = ConstComboBoxChange
  end
end
