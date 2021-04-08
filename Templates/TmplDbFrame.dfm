inherited DbFrameTemplate: TDbFrameTemplate
  Left = 499
  Top = 340
  Width = 870
  Height = 518
  Caption = 'DbFrameTemplate'
  Menu = MainMenu
  PixelsPerInch = 96
  TextHeight = 13
  object CoolBar: TCoolBar
    Left = 0
    Top = 0
    Width = 854
    Height = 29
    AutoSize = True
    Bands = <
      item
        Control = ToolBar
        ImageIndex = -1
        Width = 854
      end>
    EdgeBorders = [ebTop, ebBottom]
    object ToolBar: TToolBar
      Left = 9
      Top = 0
      Width = 841
      Height = 25
      AutoSize = True
      Caption = #1055#1072#1085#1077#1083#1100' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
      EdgeBorders = []
      Flat = True
      Images = BaseData.ImageList
      ShowCaptions = True
      TabOrder = 0
    end
  end
  inline DbFrame: TFrameDb
    Left = 0
    Top = 29
    Width = 854
    Height = 450
    Align = alClient
    TabOrder = 1
    inherited FindPanel: TPanel
      Width = 854
    end
    inherited DbGrid: TRDbStyledGrid
      Width = 854
      Height = 338
    end
    inherited DbTabViews: TTabSet
      Top = 367
      Width = 854
    end
    inherited DbStatusBar: TStatusBar
      Top = 431
      Width = 854
    end
    inherited InfoPanel: TRDbInfoPanel
      Top = 388
      Width = 854
      Height = 43
    end
  end
  object MainMenu: TMainMenu
    Images = BaseData.ImageList
    Left = 56
    Top = 180
  end
end
