inherited FormDbSelect: TFormDbSelect
  Left = 602
  Top = 310
  ActiveControl = deSelectId
  Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1079#1072#1087#1080#1089#1100
  ClientHeight = 124
  ClientWidth = 485
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 73
    Width = 485
  end
  object lblSelectId: TLabel [1]
    Left = 12
    Top = 12
    Width = 92
    Height = 13
    Caption = #1042#1099#1073#1077#1088#1080#1090#1077' '#1079#1072#1087#1080#1089#1100':'
    FocusControl = deSelectName
  end
  inherited ButtonsPanel: TPanel
    Top = 75
    Width = 485
    TabOrder = 1
    inherited ButtonsMovedPanel: TPanel
      Left = 263
      inherited OkBtn: TBitBtn
        Hint = #1055#1086#1076#1090#1074#1077#1088#1076#1080#1090#1100' '#1074#1099#1073#1086#1088
        Caption = #1044#1072#1083#1077#1077
      end
      inherited CancelBtn: TBitBtn
        Hint = #1054#1090#1084#1077#1085#1080#1090#1100' '#1086#1087#1077#1088#1072#1094#1080#1102' '#1080' '#1079#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
      end
    end
  end
  object deSelectName: TDBLookupComboBox
    Left = 108
    Top = 28
    Width = 365
    Height = 21
    KeyField = 'ID'
    ListField = 'NAME'
    ListSource = DataSource
    TabOrder = 0
    OnClick = deSelectNameClick
  end
  object deSelectId: TEdit
    Left = 12
    Top = 28
    Width = 85
    Height = 21
    Hint = #1042#1074#1077#1076#1080#1090#1077' '#1085#1086#1084#1077#1088' '#1079#1072#1087#1080#1089#1080
    TabOrder = 2
    OnExit = deSelectIdClick
    OnKeyPress = deSelectIdKeyPress
  end
  object DataSource: TDataSource
    AutoEdit = False
    Left = 356
    Top = 8
  end
end
