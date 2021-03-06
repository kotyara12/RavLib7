inherited DbTreeTemplate: TDbTreeTemplate
  Left = 441
  Top = 326
  Caption = 'DbTreeTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  inherited CoolBar: TCoolBar
    inherited ToolBar: TToolBar
      inherited ReportsToolButton: TToolButton
        Visible = True
      end
    end
  end
  inherited ActionList: TActionList
    Left = 28
    object CopyRecord: TAction [7]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1050#1083#1086#1085#1080#1088#1086#1074#1072#1090#1100
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1091#1102' '#1079#1072#1087#1080#1089#1100' '#1080' '#1089#1082#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1076#1072#1085#1085#1099#1077' '#1080#1079' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 8
      ShortCut = 16459
      OnExecute = CopyRecordExecute
      OnUpdate = CopyRecordUpdate
    end
    inherited NewGroup: TAction
      OnExecute = NewGroupExecute
      OnUpdate = NewGroupUpdate
    end
    inherited NewSubGroup: TAction
      OnExecute = NewSubGroupExecute
      OnUpdate = NewSubGroupUpdate
    end
    inherited NewItem: TAction
      OnExecute = NewItemExecute
      OnUpdate = NewItemUpdate
    end
    object NewSubItem: TAction [11]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1057#1086#1079#1076#1072#1090#1100' '#1087#1086#1076#1101#1083#1077#1084#1077#1085#1090
      Enabled = False
      Hint = #1057#1086#1079#1076#1072#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1086#1076#1101#1083#1077#1084#1077#1085#1090' '#1074' '#1090#1077#1082#1091#1097#1077#1081' '#1079#1072#1087#1080#1089#1080
      ImageIndex = 8
      Visible = False
      OnExecute = NewSubItemExecute
      OnUpdate = NewSubItemUpdate
    end
    inherited Properties: TAction
      OnExecute = PropertiesExecute
      OnUpdate = PropertiesUpdate
    end
    inherited DeleteItem: TAction
      OnExecute = DeleteItemExecute
      OnUpdate = DeleteItemUpdate
    end
    object Attachments: TAction [20]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1055#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1077' '#1092#1072#1081#1083#1099
      Enabled = False
      Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1087#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1084#1080' '#1092#1072#1081#1083#1072#1084#1080
      ImageIndex = 38
      ShortCut = 123
      Visible = False
      OnExecute = AttachmentsExecute
      OnUpdate = AttachmentsUpdate
    end
    inherited EM_Groups: TAction
      Visible = True
    end
    object ReportList: TAction
      Category = #1054#1090#1095#1077#1090#1099
      Caption = #1053#1072#1089#1090#1088#1072#1080#1074#1072#1077#1084#1099#1077' '#1086#1090#1095#1077#1090#1099
      Enabled = False
      Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1089#1087#1080#1089#1082#1086#1084' '#1086#1090#1095#1077#1090#1086#1074
      ImageIndex = 21
      ShortCut = 16464
      OnExecute = ReportListExecute
      OnUpdate = ReportListUpdate
    end
  end
  inherited PopupMenu: TPopupMenu
    Left = 84
    object itemNewSubItemP: TMenuItem [3]
      Action = NewSubItem
    end
    object itemCopyRecordP: TMenuItem [4]
      Action = CopyRecord
    end
    inherited meniLoadModeP: TMenuItem
      inherited itemSelectSaveEMP: TMenuItem [0]
      end
      inherited N13: TMenuItem [1]
      end
      inherited itemEM_NoneP: TMenuItem [2]
      end
      inherited itemEM_RootP: TMenuItem [3]
      end
      inherited itemEM_GroupsP: TMenuItem [4]
      end
      inherited itemEM_AllP: TMenuItem [5]
      end
    end
    inherited menuReportsP: TMenuItem
      Visible = True
      object itemReportListP: TMenuItem
        Action = ReportList
      end
    end
    object itemAttachments: TMenuItem [18]
      Action = Attachments
    end
  end
  inherited MainMenu: TMainMenu
    Left = 56
    inherited menuEdit: TMenuItem
      object itemNewSubItem: TMenuItem [3]
        Action = NewSubItem
      end
      object itemCopyRecord: TMenuItem [4]
        Action = CopyRecord
      end
      object itemAttachmentsP: TMenuItem [11]
        Action = Attachments
      end
      object divAttach: TMenuItem [12]
        Caption = '-'
        Visible = False
      end
    end
    inherited menuReports: TMenuItem
      Visible = True
      object itemReportList: TMenuItem
        Action = ReportList
      end
    end
  end
  inherited NewPopupMenu: TPopupMenu
    Left = 196
    object itemNewSubItemN: TMenuItem
      Action = NewSubItem
    end
  end
  inherited DataPopupMenu: TPopupMenu
    Left = 112
  end
  inherited OperationsPopupMenu: TPopupMenu
    Left = 140
  end
  inherited ReportsPopupMenu: TPopupMenu
    Left = 168
    object itemReportListR: TMenuItem
      Action = ReportList
    end
  end
  object TreeLoader: TRDbTreeLoader
    TreeView = TreeView
    RootNrmImage = -1
    RootSelImage = -1
    GroupsEditor = GroupsEditor
    ItemsEditor = ItemsEditor
    SubitemsEditor = SubitemsEditor
    Left = 28
    Top = 120
  end
  object GroupsEditor: TRDbTreeEditor
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    BlockValue = False
    OwnerFieldName = 'owner_id'
    LogEnable = True
    OpenMode = omAuto
    OnAfterProcessRecord = AfterProcessRecord
    OnBeforeShowEditor = GroupsBeforeShowEditor
    OnGetNewKey = GetRecordId
    OnFreeNewKey = FreeRecordId
    OnBeforeDelete = GroupsEditorBeforeDelete
    OnSaveToLog = SaveToLog
    NameFieldName = 'name'
    NotesFieldName = 'notes'
    NrmImage = -1
    SelImage = -1
    Left = 56
    Top = 120
  end
  object ItemsEditor: TRDbTreeEditor
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    BlockValue = False
    OwnerFieldName = 'id_groups'
    LogEnable = True
    OpenMode = omAuto
    OnAfterProcessRecord = AfterProcessRecord
    OnBeforeShowEditor = ItemsBeforeShowEditor
    OnGetNewKey = GetRecordId
    OnFreeNewKey = FreeRecordId
    OnBeforeDelete = ItemsEditorBeforeDelete
    OnSaveToLog = SaveToLog
    NameFieldName = 'name'
    NotesFieldName = 'notes'
    NrmImage = -1
    SelImage = -1
    Left = 84
    Top = 120
  end
  object SubitemsEditor: TRDbTreeEditor
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    BlockValue = False
    OwnerFieldName = 'id_items'
    LogEnable = True
    OpenMode = omAuto
    OnAfterProcessRecord = AfterProcessRecord
    OnBeforeShowEditor = SubitemsEditorBeforeShowEditor
    OnGetNewKey = GetRecordId
    OnFreeNewKey = FreeRecordId
    OnBeforeDelete = SubitemsEditorBeforeDelete
    OnSaveToLog = SaveToLog
    NameFieldName = 'name'
    NotesFieldName = 'notes'
    NrmImage = -1
    SelImage = -1
    Left = 112
    Top = 120
  end
end
