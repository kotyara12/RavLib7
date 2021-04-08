inherited FormTextBox: TFormTextBox
  Left = 408
  Top = 386
  ActiveControl = Memo
  Caption = #1042#1074#1086#1076' '#1090#1077#1082#1089#1090#1072
  ClientHeight = 293
  ClientWidth = 411
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 242
    Width = 411
  end
  object MemoLabel: TLabel [1]
    Left = 12
    Top = 8
    Width = 76
    Height = 13
    Caption = #1042#1074#1077#1076#1080#1090#1077' '#1090#1077#1082#1089#1090':'
    FocusControl = Memo
  end
  inherited ButtonsPanel: TPanel
    Top = 244
    Width = 411
    TabOrder = 1
    inherited ButtonsMovedPanel: TPanel
      Left = 189
    end
  end
  object Memo: TMemo
    Left = 12
    Top = 24
    Width = 385
    Height = 201
    ScrollBars = ssBoth
    TabOrder = 0
    WantTabs = True
    WordWrap = False
  end
end
