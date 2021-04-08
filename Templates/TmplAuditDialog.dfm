inherited DbAuditDialogTemplate: TDbAuditDialogTemplate
  Left = 600
  Top = 392
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    ActivePage = tsProperty
    object tsProperty: TTabSheet
      Caption = #1057#1074#1086#1081#1089#1090#1074#1072
    end
    object tsAudit: TTabSheet
      Caption = #1040#1091#1076#1080#1090
      ImageIndex = 1
      object lblCreated: TLabel
        Left = 8
        Top = 8
        Width = 85
        Height = 13
        Caption = #1047#1072#1087#1080#1089#1100' '#1089#1086#1079#1076#1072#1085#1072':'
        FocusControl = deCreated
      end
      object lblChanged: TLabel
        Left = 8
        Top = 52
        Width = 118
        Height = 13
        Caption = #1055#1086#1089#1083#1077#1076#1085#1077#1077' '#1080#1079#1084#1077#1085#1077#1085#1080#1077':'
        FocusControl = deCreated
      end
      object deCreated: TDBEdit
        Left = 8
        Top = 24
        Width = 149
        Height = 21
        Ctl3D = True
        DataField = 'created'
        DataSource = DataSource
        Enabled = False
        ParentColor = True
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 0
      end
      object deCreator: TDBEdit
        Left = 168
        Top = 24
        Width = 409
        Height = 21
        Ctl3D = True
        DataField = 'creator'
        DataSource = DataSource
        Enabled = False
        ParentColor = True
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 1
      end
      object deChanged: TDBEdit
        Left = 8
        Top = 68
        Width = 149
        Height = 21
        Ctl3D = True
        DataField = 'changed'
        DataSource = DataSource
        Enabled = False
        ParentColor = True
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 2
      end
      object deChanger: TDBEdit
        Left = 168
        Top = 68
        Width = 409
        Height = 21
        Ctl3D = True
        DataField = 'changer'
        DataSource = DataSource
        Enabled = False
        ParentColor = True
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 3
      end
    end
  end
end
