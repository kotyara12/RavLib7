inherited FormDbUpdaterItem_Color: TFormDbUpdaterItem_Color
  ClientHeight = 192
  PixelsPerInch = 96
  TextHeight = 13
  inherited UpdaterPanel: TPanel
    Height = 192
    inherited ButtonsBevel: TBevel
      Top = 139
    end
    inherited ButtonsPanel: TPanel
      Top = 141
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
    object RColorCombo: TRColorCombo
      Left = 32
      Top = 96
      Width = 293
      Height = 22
      Options = [ccSystemColors, ccUserDefinedColor, ccUserColorDialog]
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 3
      OnChange = ChangeState
    end
  end
end
