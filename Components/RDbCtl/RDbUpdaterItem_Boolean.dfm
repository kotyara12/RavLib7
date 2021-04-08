inherited FormDbUpdaterItem_Boolean: TFormDbUpdaterItem_Boolean
  Left = 414
  Top = 368
  ActiveControl = ValueComboBox
  ClientHeight = 191
  PixelsPerInch = 96
  TextHeight = 13
  inherited UpdaterPanel: TPanel
    Height = 191
    inherited ButtonsBevel: TBevel
      Top = 138
    end
    inherited ButtonsPanel: TPanel
      Top = 140
      TabOrder = 4
    end
    inherited ClearRadioButton: TRadioButton
      TabOrder = 1
      OnClick = ChangeState
    end
    inherited SetRadioButton: TRadioButton
      TabOrder = 2
      OnClick = ChangeState
    end
    object ValueComboBox: TComboBox
      Left = 32
      Top = 96
      Width = 293
      Height = 21
      Hint = #1047#1085#1072#1095#1077#1085#1080#1077
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = ChangeState
    end
  end
end
