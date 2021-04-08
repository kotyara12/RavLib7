inherited FormDbUpdaterItem_Float: TFormDbUpdaterItem_Float
  Left = 620
  Top = 466
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
    object FloatEdit: TRxCalcEdit
      Left = 32
      Top = 96
      Width = 293
      Height = 21
      DisplayFormat = ',0.00'
      NumGlyphs = 2
      TabOrder = 3
      OnChange = ChangeState
    end
  end
end
