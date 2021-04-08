inherited FormStatPrm: TFormStatPrm
  ActiveControl = ListView
  Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1089#1090#1072#1090#1080#1089#1090#1080#1082#1080
  ClientHeight = 303
  ClientWidth = 453
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 252
    Width = 453
  end
  object ListViewLabel: TLabel [1]
    Left = 12
    Top = 12
    Width = 327
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1087#1086#1083#1103', '#1087#1086' '#1082#1086#1090#1086#1088#1099#1084' '#1085#1077#1086#1073#1093#1086#1076#1080#1084#1086' '#1089#1075#1088#1091#1087#1087#1080#1088#1086#1074#1072#1090#1100' '#1079#1072#1087#1080#1089#1080':'
    FocusControl = ListView
  end
  inherited ButtonsPanel: TPanel
    Top = 254
    Width = 453
    TabOrder = 1
    inherited ButtonsMovedPanel: TPanel
      Left = 231
      inherited OkBtn: TBitBtn
        Hint = #1055#1088#1086#1076#1086#1083#1078#1080#1090#1100
      end
      inherited CancelBtn: TBitBtn
        Hint = #1054#1090#1084#1077#1085#1080#1090#1100' '#1086#1087#1077#1088#1072#1094#1080#1102
      end
    end
  end
  object ListView: TRSortListView
    Left = 12
    Top = 28
    Width = 429
    Height = 209
    Checkboxes = True
    Columns = <
      item
        Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
        Width = 307
      end
      item
        Caption = #1055#1086#1083#1077
        Width = 100
      end>
    FlatScrollBars = True
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    SortType = stData
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListViewChange
    SortDirection = sdAscending
  end
end
