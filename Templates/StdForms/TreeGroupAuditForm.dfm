inherited FormTreeGroupAudit: TFormTreeGroupAudit
  Left = 597
  Top = 296
  ActiveControl = deName
  ClientHeight = 263
  ClientWidth = 475
  PixelsPerInch = 96
  TextHeight = 13
  inherited ButtonsBevel: TBevel
    Top = 212
    Width = 475
  end
  inherited ButtonsPanel: TPanel
    Top = 214
    Width = 475
    inherited ButtonsMovedPanel: TPanel
      Left = 253
    end
  end
  inherited PageControl: TPageControl
    Width = 475
    Height = 212
    Images = BaseData.ImageList
    OnChange = PageControlChange
    inherited tsProperty: TTabSheet
      ImageIndex = 9
      object lblId: TLabel
        Left = 8
        Top = 8
        Width = 42
        Height = 13
        Caption = #1050#1086#1076' (ID):'
        FocusControl = deId
      end
      object lblOwner: TLabel
        Left = 100
        Top = 8
        Width = 52
        Height = 13
        Caption = #1042#1083#1072#1076#1077#1083#1077#1094':'
        FocusControl = deOwner
      end
      object lblName: TLabel
        Left = 8
        Top = 52
        Width = 79
        Height = 13
        Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077':'
        FocusControl = deName
      end
      object lblNotes: TLabel
        Left = 8
        Top = 96
        Width = 142
        Height = 13
        Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103':'
        FocusControl = deNotes
      end
      object deId: TDBEdit
        Left = 8
        Top = 24
        Width = 81
        Height = 21
        TabStop = False
        DataField = 'ID'
        DataSource = DataSource
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      object deOwner: TDBEdit
        Left = 100
        Top = 24
        Width = 81
        Height = 21
        TabStop = False
        DataField = 'OWNER_ID'
        DataSource = DataSource
        ParentColor = True
        ReadOnly = True
        TabOrder = 1
      end
      object deName: TDBEdit
        Left = 8
        Top = 68
        Width = 449
        Height = 21
        DataField = 'NAME'
        DataSource = DataSource
        TabOrder = 2
      end
      object deNotes: TDBMemo
        Left = 8
        Top = 112
        Width = 449
        Height = 53
        DataField = 'NOTES'
        DataSource = DataSource
        TabOrder = 3
        WantReturns = False
      end
    end
    inherited tsAudit: TTabSheet
      ImageIndex = 35
      inherited deCreator: TDBEdit
        Width = 289
      end
      inherited deChanger: TDBEdit
        Width = 289
      end
    end
  end
  inherited DataSource: TDataSource
    Left = 300
    Top = 8
  end
end
