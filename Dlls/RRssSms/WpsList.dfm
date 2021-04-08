inherited FormWps: TFormWps
  Left = 580
  Top = 349
  ActiveControl = ListView
  Caption = #1057#1087#1080#1089#1086#1082' '#1087#1088#1086#1075#1088#1072#1084#1084
  ClientHeight = 348
  ClientWidth = 429
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 297
    Width = 429
  end
  inherited ButtonsPanel: TPanel
    Top = 299
    Width = 429
    TabOrder = 3
    inherited ButtonsMovedPanel: TPanel
      Left = 207
    end
  end
  object ListView: TRSortListView
    Left = 12
    Top = 12
    Width = 405
    Height = 244
    Checkboxes = True
    Columns = <
      item
        Caption = #1050#1086#1076
        Width = 75
      end
      item
        Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
        Width = 200
      end
      item
        Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
        Width = 800
      end>
    FlatScrollBars = True
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    SortType = stData
    TabOrder = 0
    ViewStyle = vsReport
    SortDirection = sdAscending
  end
  object SelAllButton: TButton
    Left = 156
    Top = 260
    Width = 129
    Height = 25
    Hint = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077' '#1079#1072#1087#1080#1089#1080
    Caption = #1042#1099#1076#1077#1083#1080#1090#1100' '#1074#1089#1077
    TabOrder = 1
    TabStop = False
    OnClick = SelAllButtonClick
  end
  object UnseclectButton: TButton
    Left = 288
    Top = 260
    Width = 129
    Height = 25
    Hint = #1057#1085#1103#1090#1100' '#1074#1089#1077' '#1086#1090#1084#1077#1090#1082#1080
    Caption = #1057#1085#1103#1090#1100' '#1074#1089#1077
    TabOrder = 2
    TabStop = False
    OnClick = UnseclectButtonClick
  end
end
