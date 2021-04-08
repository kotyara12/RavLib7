inherited FormChPwdRss: TFormChPwdRss
  Left = 497
  Top = 343
  ActiveControl = OldPwdEdit
  Caption = #1057#1084#1077#1085#1072' '#1087#1072#1088#1086#1083#1103
  ClientHeight = 220
  ClientWidth = 347
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 169
    Width = 347
  end
  object OldPwdEditLabel: TLabel [1]
    Left = 12
    Top = 12
    Width = 87
    Height = 13
    Caption = #1058#1077#1082#1091#1097#1080#1081' '#1087#1072#1088#1086#1083#1100':'
    FocusControl = OldPwdEdit
  end
  object NewPwdEditLabel: TLabel [2]
    Left = 12
    Top = 60
    Width = 76
    Height = 13
    Caption = #1053#1086#1074#1099#1081' '#1087#1072#1088#1086#1083#1100':'
    FocusControl = NewPwdEdit
  end
  object CnfPwdEditLabel: TLabel [3]
    Left = 12
    Top = 108
    Width = 123
    Height = 13
    Caption = #1055#1086#1076#1090#1074#1077#1088#1078#1076#1077#1085#1080#1077' '#1087#1072#1088#1086#1083#1103':'
    FocusControl = CnfPwdEdit
  end
  inherited ButtonsPanel: TPanel
    Top = 171
    Width = 347
    TabOrder = 3
    inherited ButtonsMovedPanel: TPanel
      Left = 125
    end
  end
  object OldPwdEdit: TEdit
    Left = 12
    Top = 28
    Width = 321
    Height = 21
    Hint = #1058#1077#1082#1091#1097#1080#1081' '#1087#1072#1088#1086#1083#1100
    PasswordChar = '*'
    TabOrder = 0
  end
  object NewPwdEdit: TEdit
    Left = 12
    Top = 76
    Width = 321
    Height = 21
    Hint = #1053#1086#1074#1099#1081' '#1087#1072#1088#1086#1083#1100
    PasswordChar = '*'
    TabOrder = 1
  end
  object CnfPwdEdit: TEdit
    Left = 12
    Top = 124
    Width = 321
    Height = 21
    Hint = #1055#1086#1076#1090#1074#1077#1088#1078#1076#1077#1085#1080#1077' '#1087#1072#1088#1086#1083#1103
    PasswordChar = '*'
    TabOrder = 2
  end
end
