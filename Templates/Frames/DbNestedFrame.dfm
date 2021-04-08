object FrameDbNested: TFrameDbNested
  Left = 0
  Top = 0
  Width = 797
  Height = 294
  TabOrder = 0
  object CoolBar: TCoolBar
    Left = 0
    Top = 0
    Width = 797
    Height = 26
    AutoSize = True
    Bands = <
      item
        Break = False
        Control = ToolBar
        FixedSize = True
        HorizontalOnly = True
        ImageIndex = -1
        MinHeight = 22
        Width = 1164
      end>
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 1164
      Height = 22
      AutoSize = True
      ButtonWidth = 76
      Caption = #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      EdgeBorders = []
      Flat = True
      Images = BaseData.ImageList
      List = True
      ShowCaptions = True
      TabOrder = 0
      Wrapable = False
      object DbFirstToolButton: TToolButton
        Left = 0
        Top = 0
        Action = FrameDb.actDbFirst
      end
      object DbPriorToolButton: TToolButton
        Left = 76
        Top = 0
        Action = FrameDb.actDbPrior
      end
      object DbNextToolButton: TToolButton
        Left = 152
        Top = 0
        Action = FrameDb.actDbNext
      end
      object DataSetLastToolButton: TToolButton
        Left = 228
        Top = 0
        Action = FrameDb.actDbLast
      end
      object SeparatorNav: TToolButton
        Left = 304
        Top = 0
        Width = 8
        Caption = 'SeparatorNav'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object InsertToolButton: TToolButton
        Left = 312
        Top = 0
        Action = FrameDb.actDbInsert
      end
      object EditToolButton: TToolButton
        Left = 388
        Top = 0
        Action = FrameDb.actDbEdit
      end
      object DeleteToolButton: TToolButton
        Left = 464
        Top = 0
        Action = FrameDb.actDbDelete
      end
      object SeparatorEdit: TToolButton
        Left = 540
        Top = 0
        Width = 8
        Caption = 'SeparatorEdit'
        ImageIndex = 2
        Style = tbsSeparator
      end
      object DbFindToolButton: TToolButton
        Left = 548
        Top = 0
        Action = FrameDb.actDbFind
      end
      object SeparatorFind: TToolButton
        Left = 624
        Top = 0
        Width = 8
        Caption = 'SeparatorFind'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object DataToolButton: TToolButton
        Left = 632
        Top = 0
        Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1076#1072#1085#1085#1099#1084#1080
        Caption = #1044#1072#1085#1085#1099#1077
        DropdownMenu = FrameDb.DataPopupMenu
        ImageIndex = 14
      end
      object SeparatorData: TToolButton
        Left = 708
        Top = 0
        Width = 8
        Caption = 'SeparatorData'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object RefreshToolButton: TToolButton
        Left = 716
        Top = 0
        Action = FrameDb.actDbRefresh
      end
    end
  end
  inline FrameDb: TFrameDb
    Left = 0
    Top = 26
    Width = 797
    Height = 268
    Align = alClient
    TabOrder = 1
    inherited FindPanel: TPanel
      Width = 797
    end
    inherited DbGrid: TRDbStyledGrid
      Width = 797
      Height = 218
    end
    inherited DbTabViews: TTabSet
      Top = 247
      Width = 797
    end
    inherited DbActions: TActionList
      Left = 284
      Top = 72
    end
    inherited RDbEditor: TRDbExportEditor
      Left = 60
      Top = 72
    end
    inherited RDbLocate: TRDbFind
      Left = 144
      Top = 72
    end
    inherited RDbSearch: TRDbSearch
      Left = 116
      Top = 72
    end
    inherited RDbGridTuner: TRDbGridTuner
      Left = 228
      Top = 72
    end
    inherited RDbUpdater: TRDbUpdater
      Left = 88
      Top = 72
    end
    inherited RDbFilterStatus: TRDbFilterStatus
      Left = 256
      Top = 72
    end
    inherited GridPopupMenu: TPopupMenu
      Left = 312
      Top = 72
    end
    inherited TitlePopupMenu: TPopupMenu
      Left = 340
      Top = 72
    end
    inherited RDbFilter: TRDbFilter
      Left = 172
      Top = 72
    end
    inherited RDbOrder: TRDbOrder
      Left = 200
      Top = 72
    end
    inherited ViewsPopupMenu: TPopupMenu
      Left = 368
      Top = 72
    end
    inherited DataPopupMenu: TPopupMenu
      Left = 396
      Top = 72
    end
  end
end
