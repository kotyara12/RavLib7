inherited FormDbUpdaterItem_Integer: TFormDbUpdaterItem_Integer
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
    object SpinEdit: TSpinEdit
      Left = 32
      Top = 96
      Width = 293
      Height = 22
      Hint = #1047#1085#1072#1095#1077#1085#1080#1077
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = ChangeState
    end
  end
end
