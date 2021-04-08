inherited FormContact: TFormContact
  Left = 648
  Top = 418
  ActiveControl = edContType
  Caption = #1050#1086#1085#1090#1072#1082#1090
  ClientHeight = 215
  ClientWidth = 378
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 164
    Width = 378
  end
  object lblContType: TLabel [1]
    Left = 12
    Top = 8
    Width = 71
    Height = 13
    Caption = #1058#1080#1087' '#1082#1086#1085#1090#1072#1082#1090#1072':'
    FocusControl = edContType
  end
  object lblContValue: TLabel [2]
    Left = 12
    Top = 56
    Width = 226
    Height = 13
    Caption = #1053#1086#1084#1077#1088' '#1090#1077#1083#1077#1092#1086#1085#1072', '#1072#1076#1088#1077#1089' '#1101#1083#1077#1082#1090#1088#1086#1085#1085#1086#1081' '#1087#1086#1095#1090#1099':'
    FocusControl = edContValue
  end
  object lblContName: TLabel [3]
    Left = 12
    Top = 104
    Width = 144
    Height = 13
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077', '#1082#1086#1085#1090#1072#1082#1090#1085#1086#1077' '#1083#1080#1094#1086':'
    FocusControl = edContName
  end
  inherited ButtonsPanel: TPanel
    Top = 166
    Width = 378
    TabOrder = 3
    inherited ButtonsMovedPanel: TPanel
      Left = 156
    end
  end
  object edContType: TComboBox
    Left = 12
    Top = 24
    Width = 353
    Height = 21
    Hint = #1058#1080#1087' '#1082#1086#1085#1090#1072#1082#1090#1072
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object edContValue: TEdit
    Left = 12
    Top = 72
    Width = 353
    Height = 21
    Hint = #1053#1086#1084#1077#1088' '#1090#1077#1083#1077#1092#1086#1085#1072', '#1072#1076#1088#1077#1089' '#1101#1083#1077#1082#1090#1088#1086#1085#1085#1086#1081' '#1087#1086#1095#1090#1099
    TabOrder = 1
  end
  object edContName: TEdit
    Left = 12
    Top = 120
    Width = 353
    Height = 21
    Hint = #1054#1087#1080#1089#1072#1085#1080#1077', '#1082#1086#1085#1090#1072#1082#1090#1085#1086#1077' '#1083#1080#1094#1086
    TabOrder = 2
  end
end
