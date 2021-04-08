object FormProgress: TFormProgress
  Left = 435
  Top = 180
  BorderStyle = bsNone
  Caption = 'FormProgress'
  ClientHeight = 46
  ClientWidth = 279
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 279
    Height = 46
    Align = alClient
    BevelInner = bvLowered
    TabOrder = 0
    object Text: TLabel
      Left = 6
      Top = 6
      Width = 267
      Height = 13
      AutoSize = False
      Caption = #1055#1086#1076#1086#1078#1076#1080#1090#1077' '#1087#1086#1078#1072#1083#1091#1081#1089#1090#1072'...'
    end
    object Bar: TProgressBar
      Left = 6
      Top = 20
      Width = 267
      Height = 19
      Position = 100
      Step = 1
      TabOrder = 0
    end
  end
end
