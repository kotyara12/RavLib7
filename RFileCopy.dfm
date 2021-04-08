object FormFileCopy: TFormFileCopy
  Left = 475
  Top = 480
  ActiveControl = CancelBtn
  BorderStyle = bsDialog
  Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1085#1080#1077' '#1092#1072#1081#1083#1072'...'
  ClientHeight = 159
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object FileLabel: TLabel
    Left = 12
    Top = 12
    Width = 381
    Height = 17
    AutoSize = False
  end
  object SizeLabel: TLabel
    Left = 12
    Top = 124
    Width = 269
    Height = 25
    AutoSize = False
  end
  object Animate: TAnimate
    Left = 12
    Top = 28
    Width = 272
    Height = 60
    CommonAVI = aviCopyFile
    StopFrame = 20
  end
  object ProgressBar: TProgressBar
    Left = 12
    Top = 96
    Width = 381
    Height = 17
    TabOrder = 1
  end
  object CancelBtn: TBitBtn
    Left = 288
    Top = 124
    Width = 105
    Height = 25
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 2
    OnClick = CancelBtnClick
  end
end
