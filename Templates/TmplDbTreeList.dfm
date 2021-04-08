inherited DbTreeListTemplate: TDbTreeListTemplate
  Caption = 'DbTreeListTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  inherited ActionList: TActionList
    object TreeAttachments: TAction [34]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1055#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1077' '#1092#1072#1081#1083#1099
      Enabled = False
      Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1087#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1084#1080' '#1092#1072#1081#1083#1072#1084#1080
      ImageIndex = 38
      ShortCut = 16507
      OnExecute = TreeAttachmentsExecute
      OnUpdate = TreeAttachmentsUpdate
    end
  end
  inherited TreePopupMenu: TPopupMenu
    object itemTreeAttachmentsP: TMenuItem [11]
      Action = TreeAttachments
    end
    object divTreeAttachP: TMenuItem [12]
      Caption = '-'
    end
  end
  object TreeLoader: TRDbTreeLoader
    TreeView = TreeView
    RootNrmImage = -1
    RootSelImage = -1
    GroupsEditor = GroupsEditor
    ItemsEditor = ItemsEditor
    Left = 68
    Top = 92
  end
  object GroupsEditor: TRDbTreeEditor
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    OwnerFieldName = 'owner_id'
    LogEnable = True
    OpenMode = omAuto
    OnAfterProcessRecord = TreeAfterProcess
    OnBeforeShowEditor = GroupsBeforeShowEditor
    OnGetNewKey = GetRecordId
    OnFreeNewKey = FreeRecordId
    OnBeforeDelete = GroupsEditorBeforeDelete
    OnSaveToLog = SaveToLog
    NameFieldName = 'name'
    NotesFieldName = 'notes'
    NrmImage = -1
    SelImage = -1
    Left = 96
    Top = 92
  end
  object ItemsEditor: TRDbTreeEditor
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    ObjectName = 'id_groups'
    OpenMode = omAuto
    OnAfterProcessRecord = TreeAfterProcess
    OnBeforeShowEditor = ItemsBeforeShowEditor
    OnGetNewKey = GetRecordId
    OnFreeNewKey = FreeRecordId
    OnBeforeDelete = ItemsEditorBeforeDelete
    OnSaveToLog = SaveToLog
    NameFieldName = 'name'
    NotesFieldName = 'notes'
    NrmImage = -1
    SelImage = -1
    Left = 124
    Top = 92
  end
end
