inherited FormTreeNode: TFormTreeNode
  Left = 465
  Top = 412
  ActiveControl = Memo
  Caption = #1057#1074#1086#1081#1089#1090#1074#1072
  ClientHeight = 141
  ClientWidth = 365
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 90
    Width = 365
  end
  inherited ButtonsPanel: TPanel
    Top = 92
    Width = 365
    inherited ButtonsMovedPanel: TPanel
      Left = 143
      inherited OkBtn: TBitBtn
        Visible = False
      end
      inherited CancelBtn: TBitBtn
        Hint = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086
        Caption = #1047#1072#1082#1088#1099#1090#1100
      end
    end
  end
  object Memo: TMemo
    Left = 12
    Top = 12
    Width = 341
    Height = 65
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 1
  end
end
