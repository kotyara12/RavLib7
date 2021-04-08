inherited FormDbUpdaterItem_Int64: TFormDbUpdaterItem_Int64
  Left = 372
  Top = 290
  ActiveControl = SpinEdit
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
    object SpinEdit: TSpinEdit64
      Left = 32
      Top = 96
      Width = 293
      Height = 22
      Hint = #1047#1085#1072#1095#1077#1085#1080#1077
      Increment = 1
      TabOrder = 3
      OnChange = ChangeState
    end
  end
end
