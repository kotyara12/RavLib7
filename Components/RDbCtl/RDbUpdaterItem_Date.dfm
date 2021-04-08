inherited FormDbUpdaterItem_Date: TFormDbUpdaterItem_Date
  Left = 537
  Top = 287
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
      TabOrder = 5
    end
    inherited ClearRadioButton: TRadioButton
      TabOrder = 1
      OnClick = ChangeState
    end
    inherited SetRadioButton: TRadioButton
      TabOrder = 2
      OnClick = ChangeState
    end
    object DatePicker: TDateTimePicker
      Left = 32
      Top = 96
      Width = 141
      Height = 21
      Date = 41067.695335902780000000
      Time = 41067.695335902780000000
      ParseInput = True
      TabOrder = 3
      OnChange = ChangeState
    end
    object TimePicker: TDateTimePicker
      Left = 184
      Top = 96
      Width = 141
      Height = 21
      Date = 41067.695513495370000000
      Time = 41067.695513495370000000
      Kind = dtkTime
      ParseInput = True
      TabOrder = 4
      OnChange = ChangeState
    end
  end
end
