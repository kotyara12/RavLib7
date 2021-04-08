inherited FormRptName: TFormRptName
  Left = 456
  Top = 430
  ActiveControl = deName
  Caption = #1057#1074#1086#1081#1089#1090#1074#1072' '#1086#1090#1095#1077#1090#1072
  ClientHeight = 225
  ClientWidth = 406
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 174
    Width = 406
  end
  object lblName: TLabel [1]
    Left = 12
    Top = 12
    Width = 115
    Height = 13
    Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077' '#1086#1090#1095#1077#1090#1072':'
    FocusControl = deName
  end
  object lblNotes: TLabel [2]
    Left = 12
    Top = 60
    Width = 142
    Height = 13
    Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103':'
    FocusControl = deNotes
  end
  inherited ButtonsPanel: TPanel
    Top = 176
    Width = 406
    TabOrder = 2
    inherited ButtonsMovedPanel: TPanel
      Left = 184
    end
  end
  object deName: TDBEdit [4]
    Left = 12
    Top = 28
    Width = 381
    Height = 21
    DataField = 'NAME'
    DataSource = DataSource
    TabOrder = 0
  end
  object deNotes: TDBMemo [5]
    Left = 12
    Top = 76
    Width = 381
    Height = 77
    DataField = 'NOTES'
    DataSource = DataSource
    TabOrder = 1
    WantReturns = False
  end
  inherited DataSource: TDataSource
    DataSet = FormReports.SS_REPORTS
  end
end
