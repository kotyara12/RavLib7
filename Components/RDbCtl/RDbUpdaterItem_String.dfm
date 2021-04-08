inherited FormDbUpdaterItem_String: TFormDbUpdaterItem_String
  Left = 532
  Top = 251
  ActiveControl = ComboBox
  ClientHeight = 237
  PixelsPerInch = 96
  TextHeight = 13
  inherited UpdaterPanel: TPanel
    Height = 237
    inherited ButtonsBevel: TBevel
      Top = 184
    end
    inherited ButtonsPanel: TPanel
      Top = 186
      TabOrder = 7
    end
    inherited ClearRadioButton: TRadioButton
      OnClick = ChangeState
    end
    inherited SetRadioButton: TRadioButton
      TabOrder = 1
      OnClick = ChangeState
    end
    object ComboBox: TComboBox
      Left = 32
      Top = 96
      Width = 293
      Height = 21
      Hint = #1047#1085#1072#1095#1077#1085#1080#1077
      ItemHeight = 13
      TabOrder = 3
      OnChange = ChangeState
    end
    object ReplaceRadioButton: TRadioButton
      Left = 12
      Top = 124
      Width = 313
      Height = 17
      Caption = #1053#1072#1081#1090#1080' '#1091#1082#1072#1079#1072#1085#1085#1099#1081' '#1090#1077#1082#1089#1090' '#1080' '#1079#1072#1084#1077#1085#1080#1090#1100' '#1077#1075#1086' '#1076#1088#1091#1075#1080#1084':'
      TabOrder = 4
      OnClick = ChangeState
    end
    object FindEdit: TEdit
      Left = 32
      Top = 144
      Width = 137
      Height = 21
      Hint = #1063#1090#1086' '#1080#1089#1082#1072#1090#1100
      TabOrder = 5
      OnChange = ChangeState
    end
    object ReplaceEdit: TEdit
      Left = 184
      Top = 144
      Width = 137
      Height = 21
      Hint = #1063#1077#1084' '#1079#1072#1084#1077#1085#1080#1090#1100
      TabOrder = 6
      OnChange = ChangeState
    end
  end
end
