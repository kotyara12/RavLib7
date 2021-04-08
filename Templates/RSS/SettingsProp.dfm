inherited FormSettingsProp: TFormSettingsProp
  Left = 477
  Top = 269
  HelpKeyword = 'IDH_SETTINGSPROP'
  Caption = #1055#1072#1088#1072#1084#1077#1090#1088' '#1089#1080#1089#1090#1077#1084#1099
  ClientHeight = 254
  ClientWidth = 529
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 203
    Width = 529
  end
  object lblName: TLabel [1]
    Left = 12
    Top = 12
    Width = 137
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1072':'
    FocusControl = edName
  end
  object lblDefault: TLabel [2]
    Left = 12
    Top = 92
    Width = 125
    Height = 13
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102':'
    FocusControl = edDefault
  end
  object lblValue: TLabel [3]
    Left = 12
    Top = 140
    Width = 51
    Height = 13
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077':'
  end
  object btnOpen: TSpeedButton [4]
    Left = 496
    Top = 156
    Width = 21
    Height = 21
    Hint = #1042#1099#1073#1088#1072#1090#1100' '#1092#1072#1081#1083' '#1080#1083#1080' '#1082#1072#1090#1072#1083#1086#1075
    Caption = '...'
    Visible = False
    OnClick = btnOpenClick
  end
  object edChar: TEdit [5]
    Left = 12
    Top = 156
    Width = 481
    Height = 21
    Hint = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1072
    TabOrder = 2
    Visible = False
  end
  inherited ButtonsPanel: TPanel
    Top = 205
    Width = 529
    TabOrder = 5
    inherited ButtonsMovedPanel: TPanel
      Left = 307
    end
  end
  object edDefault: TEdit
    Left = 12
    Top = 108
    Width = 505
    Height = 21
    Hint = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
    ParentColor = True
    ReadOnly = True
    TabOrder = 7
  end
  object edInt: TSpinEdit
    Left = 12
    Top = 156
    Width = 121
    Height = 22
    Hint = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1072
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
    Visible = False
  end
  object edReal: TRFloatEdit
    Left = 12
    Top = 156
    Width = 121
    Height = 21
    Hint = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1072
    DisplayFormat = ',0.0000'
    TabOrder = 1
    Visible = False
  end
  object edDateTime: TDateTimePicker
    Left = 12
    Top = 156
    Width = 121
    Height = 21
    Hint = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1072
    Date = 39127.923431840280000000
    Time = 39127.923431840280000000
    TabOrder = 3
    Visible = False
  end
  object edBoolean: TComboBox
    Left = 12
    Top = 156
    Width = 177
    Height = 21
    Hint = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1072
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    Visible = False
  end
  object edName: TMemo
    Left = 12
    Top = 28
    Width = 505
    Height = 53
    Hint = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1087#1072#1088#1072#1084#1077#1090#1088#1072
    ParentColor = True
    ReadOnly = True
    TabOrder = 6
  end
end
