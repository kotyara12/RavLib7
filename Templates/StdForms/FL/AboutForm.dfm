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
  object InternalName: TLabel [2]
    Left = 80
    Top = 44
    Width = 76
    Height = 13
    Caption = 'InternalName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ProductName: TLabel [3]
    Left = 80
    Top = 22
    Width = 77
    Height = 13
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
    Top = 192
    Width = 429
    Height = 53
    AutoSize = False
    Caption = 
      #1044#1072#1085#1085#1072#1103' '#1087#1088#1086#1075#1088#1072#1084#1084#1072' '#1089#1086#1079#1076#1072#1085#1072' '#1085#1072' '#1079#1072#1082#1072#1079' '#1080' '#1084#1086#1078#1077#1090' '#1073#1099#1090#1100' '#1080#1089#1087#1086#1083#1100#1079#1086#1074#1072#1085#1072' '#1090#1086#1083#1100 +
      #1082#1086' '#1079#1072#1082#1072#1079#1095#1080#1082#1086#1084'. '#1055#1086' '#1074#1089#1077#1084' '#1074#1086#1087#1088#1086#1089#1072#1084' '#1089#1086#1079#1076#1072#1085#1080#1103' '#1080#1083#1080' '#1076#1086#1088#1072#1073#1086#1090#1082#1080' '#1079#1072#1082#1072#1079#1072#1085#1086#1075 +
      #1086' '#1055#1054' '#1086#1073#1088#1072#1097#1072#1090#1100#1089#1103' '#1087#1086' '#1091#1082#1072#1079#1072#1085#1085#1099#1084' '#1074#1099#1096#1077' '#1082#1086#1085#1090#1072#1082#1090#1072#1084'.'
    WordWrap = True
  end
  object SiteLable: TLabel [11]
    Left = 80
    Top = 148
    Width = 135
    Height = 13
    Caption = #1057#1072#1081#1090' '#1072#1074#1090#1086#1088#1072' / '#1087#1088#1086#1075#1088#1072#1084#1084#1099':'
  end
  object HttpLabel: TLabel [12]
    Left = 413
    Top = 148
    Width = 95
    Height = 13
    Cursor = crHandPoint
    Alignment = taRightJustify
    Caption = 'https://kotyara12.ru'
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
