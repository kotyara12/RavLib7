inherited FormSelectMode: TFormSelectMode
  Left = 471
  Top = 299
  HelpKeyword = 'IDH_SELECTPROCESSMODE'
  ActiveControl = ModeComboBox
  Caption = #1056#1077#1078#1080#1084' '#1086#1073#1088#1072#1073#1086#1090#1082#1080
  ClientHeight = 153
  ClientWidth = 337
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 102
    Width = 337
  end
  object ModeComboBoxLabel: TLabel [1]
    Left = 12
    Top = 12
    Width = 191
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1088#1077#1078#1080#1084' '#1086#1073#1088#1072#1073#1086#1090#1082#1080' '#1079#1072#1087#1080#1089#1077#1081':'
    FocusControl = ModeComboBox
  end
  object NumTextLabel: TLabel [2]
    Left = 12
    Top = 72
    Width = 170
    Height = 13
    Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086'  '#1074#1099#1073#1088#1072#1085#1085#1099#1093' '#1079#1072#1087#1080#1089#1077#1081':'
    FocusControl = NumText
  end
  inherited ButtonsPanel: TPanel
    Top = 104
    Width = 337
    TabOrder = 2
    inherited ButtonsMovedPanel: TPanel
      Left = 115
    end
  end
  object ModeComboBox: TComboBox
    Left = 12
    Top = 28
    Width = 313
    Height = 21
    Hint = #1056#1077#1078#1080#1084' '#1086#1073#1088#1072#1073#1086#1090#1082#1080' '#1079#1072#1087#1080#1089#1077#1081
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ModeComboBoxChange
    Items.Strings = (
      #1054#1073#1088#1072#1073#1086#1090#1072#1090#1100' '#1074#1089#1077' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1077' '#1079#1072#1087#1080#1089#1080
      #1054#1073#1088#1072#1073#1086#1090#1072#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080)
  end
  object NumText: TStaticText
    Left = 260
    Top = 64
    Width = 65
    Height = 21
    Hint = #1054#1073#1097#1077#1077' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1079#1072#1087#1080#1089#1077#1081', '#1082#1086#1090#1086#1088#1099#1077' '#1073#1091#1076#1091#1090' '#1086#1073#1088#1072#1073#1086#1090#1072#1085#1099
    Alignment = taCenter
    AutoSize = False
    BevelInner = bvLowered
    BevelKind = bkSoft
    BevelOuter = bvSpace
    BorderStyle = sbsSunken
    Caption = '0'
    Color = clWindow
    ParentColor = False
    TabOrder = 1
  end
end
