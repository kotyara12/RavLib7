inherited FormRptLoad: TFormRptLoad
  ActiveControl = DBGrid
  Caption = #1042#1099#1073#1086#1088' '#1086#1090#1095#1077#1090#1072
  ClientHeight = 339
  ClientWidth = 442
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 288
    Width = 442
  end
  object DBGridLabel: TLabel [1]
    Left = 12
    Top = 12
    Width = 153
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1086#1090#1095#1077#1090' '#1076#1083#1103' '#1079#1072#1075#1088#1091#1079#1082#1080':'
    FocusControl = DBGrid
  end
  inherited ButtonsPanel: TPanel
    Top = 290
    Width = 442
    TabOrder = 1
    inherited ButtonsMovedPanel: TPanel
      Left = 220
    end
  end
  object DBGrid: TDBGrid [3]
    Left = 12
    Top = 28
    Width = 417
    Height = 233
    DataSource = DataSource
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnDblClick = DBGridDblClick
    Columns = <
      item
        Expanded = False
        FieldName = 'NAME'
        Width = 300
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NOTES'
        Visible = True
      end>
  end
  inherited DataSource: TDataSource
    DataSet = FormReports.SS_REPORTS
    OnDataChange = DataSourceDataChange
  end
end
