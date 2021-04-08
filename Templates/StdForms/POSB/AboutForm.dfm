inherited FormAbout: TFormAbout
  Left = 426
  Top = 257
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 321
  ClientWidth = 525
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 270
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
    Height = 23
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
    Height = 23
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
    Width = 430
    Height = 20
    AutoSize = False
    Caption = #1040#1074#1090#1086#1088' '#1087#1088#1086#1075#1088#1072#1084#1084#1099': '#1056#1072#1079#1078#1080#1074#1080#1085' '#1040#1083#1077#1082#1089#1072#1085#1076#1088' '#1042#1072#1083#1077#1088#1100#1077#1074#1080#1095
    WordWrap = True
    IsControl = True
  end
  object TrademarksLabel: TLabel [7]
    Left = 80
    Top = 152
    Width = 430
    Height = 20
    AutoSize = False
    Caption = #1055#1072#1074#1083#1086#1074#1089#1082#1086#1077' '#1086#1090#1076#1077#1083#1077#1085#1080#1077' '#1057#1041' '#1056#1060' '#8470' 4378'
  end
  object OsLabel: TLabel [8]
    Left = 80
    Top = 224
    Width = 430
    Height = 20
    AutoSize = False
    Caption = 'Windows'
  end
  object DelphiVersion: TLabel [9]
    Left = 80
    Top = 176
    Width = 430
    Height = 20
    AutoSize = False
    Caption = #1057#1088#1077#1076#1072' '#1088#1072#1079#1088#1072#1073#1086#1090#1082#1080': Borland Delphi 7 + '#1087#1072#1082#1077#1090' Rav Library 7'
  end
  object CommentsLabel: TLabel [10]
    Left = 80
    Top = 200
    Width = 430
    Height = 20
    AutoSize = False
    Caption = 'CommentsLabel'
  end
  inherited ButtonsPanel: TPanel
    Top = 272
    Width = 525
    inherited ButtonsMovedPanel: TPanel
      Left = 303
      inherited OkBtn: TBitBtn
        Visible = False
      end
    end
  end
end
