inherited FormTreeMove: TFormTreeMove
  Left = 458
  Top = 277
  HelpKeyword = 'IDH_TREEMOVE'
  ActiveControl = TreeView
  Caption = #1055#1077#1088#1077#1084#1077#1089#1090#1080#1090#1100' '#1074'...'
  ClientHeight = 381
  ClientWidth = 387
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 330
    Width = 387
  end
  inherited ButtonsPanel: TPanel
    Top = 332
    Width = 387
    TabOrder = 1
    inherited ButtonsMovedPanel: TPanel
      Left = 165
      inherited OkBtn: TBitBtn
        Hint = #1055#1077#1088#1077#1084#1077#1089#1090#1080#1090#1100' '#1079#1072#1087#1080#1089#1100' '#1074' '#1074#1099#1076#1077#1083#1077#1085#1085#1091#1102' '#1075#1088#1091#1087#1087#1091
      end
    end
  end
  object TreeView: TRTreeView
    Left = 0
    Top = 0
    Width = 387
    Height = 330
    Align = alClient
    BevelKind = bkSoft
    BorderStyle = bsNone
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RowSelect = True
    SortType = stNone
    TabOrder = 0
    OnChange = TreeViewChange
    ListEmptyValue = '-1'
    ListDelimChar = ','
  end
end
