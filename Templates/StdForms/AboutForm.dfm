inherited FormAbout: TFormAbout
  Left = 426
  Top = 257
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 346
  ClientWidth = 525
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 295
    Width = 525
  end
  object ProgramIcon: TImage [1]
    Left = 24
    Top = 24
    Width = 32
    Height = 32
    Center = True
  end
  object ProgramName: TLabel [2]
    Left = 80
    Top = 24
    Width = 429
    Height = 17
    AutoSize = False
    Caption = 'ProgramName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ProductName: TLabel [3]
    Left = 80
    Top = 46
    Width = 429
    Height = 15
    AutoSize = False
    Caption = 'ProductName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object VersionInfo: TLabel [4]
    Left = 80
    Top = 68
    Width = 53
    Height = 13
    Caption = 'VersionInfo'
  end
  object Bevel: TBevel [5]
    Left = 80
    Top = 104
    Width = 429
    Height = 5
    Shape = bsTopLine
  end
  object AutorName: TLabel [6]
    Left = 80
    Top = 128
    Width = 270
    Height = 13
    Caption = #1040#1074#1090#1086#1088' '#1087#1088#1086#1075#1088#1072#1084#1084#1099': '#1056#1072#1079#1078#1080#1074#1080#1085' '#1040#1083#1077#1082#1089#1072#1085#1076#1088' '#1042#1072#1083#1077#1088#1100#1077#1074#1080#1095
    IsControl = True
  end
  object MailLabel: TLabel [7]
    Left = 80
    Top = 168
    Width = 223
    Height = 13
    Caption = #1042#1086#1087#1088#1086#1089#1099' '#1080' '#1087#1086#1078#1077#1083#1072#1085#1080#1103' '#1085#1072#1087#1088#1072#1074#1083#1103#1090#1100' '#1085#1072' e-mail:'
  end
  object MailToLabel: TLabel [8]
    Left = 404
    Top = 168
    Width = 104
    Height = 13
    Cursor = crHandPoint
    Alignment = taRightJustify
    Caption = 'kotyara12@yandex.ru'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = MailToLabelClick
  end
  object OsLabel: TLabel [9]
    Left = 80
    Top = 256
    Width = 44
    Height = 13
    Caption = 'Windows'
  end
  object PoproshaykaLabel: TLabel [10]
    Left = 80
    Top = 208
    Width = 429
    Height = 37
    AutoSize = False
    Caption = 
      #1053#1086' '#1042#1099' '#1084#1086#1078#1077#1090#1077' '#1086#1082#1072#1079#1072#1090#1100' '#1084#1072#1090#1077#1088#1080#1072#1083#1100#1085#1086#1077' '#1089#1086#1076#1077#1081#1089#1090#1074#1080#1077' '#1077#1077' '#1088#1072#1079#1074#1080#1090#1080#1102' '#1085#1072' '#1101#1083#1077#1082 +
      #1090#1088#1086#1085#1085#1099#1081' '#1082#1086#1096#1077#1083#1077#1082' '#1071#1085#1076#1077#1082#1089'.'#1044#1077#1085#1100#1075#1080' 410011548209950'
    WordWrap = True
  end
  object StateLabel: TLabel [11]
    Left = 80
    Top = 188
    Width = 214
    Height = 13
    Caption = #1044#1072#1085#1085#1072#1103' '#1087#1088#1086#1075#1088#1072#1084#1084#1072' '#1103#1074#1083#1103#1077#1090#1089#1103' '#1073#1077#1089#1087#1083#1072#1090#1085#1086#1081'.'
    IsControl = True
  end
  object SiteLable: TLabel [12]
    Left = 80
    Top = 148
    Width = 135
    Height = 13
    Caption = #1057#1072#1081#1090' '#1072#1074#1090#1086#1088#1072' / '#1087#1088#1086#1075#1088#1072#1084#1084#1099':'
  end
  object HttpLabel: TLabel [13]
    Left = 379
    Top = 148
    Width = 129
    Height = 13
    Cursor = crHandPoint
    Alignment = taRightJustify
    Caption = 'http://ravsoft2004.narod.ru'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = HttpLabelClick
  end
  inherited ButtonsPanel: TPanel
    Top = 297
    Width = 525
    inherited ButtonsMovedPanel: TPanel
      Left = 303
      inherited OkBtn: TBitBtn
        Visible = False
      end
      inherited CancelBtn: TBitBtn
        Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
        Caption = #1047#1072#1082#1088#1099#1090#1100
      end
    end
  end
end
