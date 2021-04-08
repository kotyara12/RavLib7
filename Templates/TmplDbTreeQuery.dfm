inherited DbTreeQueryTemplate: TDbTreeQueryTemplate
  Left = 351
  Top = 210
  Caption = 'DbTreeQueryTemplate'
  PixelsPerInch = 96
  TextHeight = 13
  inherited ActionList: TActionList
    object TreeAttachments: TAction [50]
      Category = #1055#1088#1072#1074#1082#1072
      Caption = #1055#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1077' '#1092#1072#1081#1083#1099
      Enabled = False
      Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1087#1088#1080#1082#1088#1077#1087#1083#1077#1085#1085#1099#1084#1080' '#1092#1072#1081#1083#1072#1084#1080
      ImageIndex = 38
      ShortCut = 16507
      Visible = False
      OnExecute = TreeAttachmentsExecute
      OnUpdate = TreeAttachmentsUpdate
    end
  end
  inherited TreePopupMenu: TPopupMenu
    object divTreeAttachP: TMenuItem [11]
      Caption = '-'
      Visible = False
    end
    object itemTreeAttachments: TMenuItem [12]
      Action = TreeAttachments
    end
  end
  object TreeLoader: TRDbTreeLoader [19]
    TreeView = TreeView
    RootNrmImage = -1
    RootSelImage = -1
    GroupsEditor = GroupsEditor
    ItemsEditor = ItemsEditor
    Left = 68
    Top = 92
  end
  object GroupsEditor: TRDbTreeEditor [20]
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    BlockValue = False
    OwnerFieldName = 'owner_id'
    LogEnable = True
    OpenMode = omAuto
    OnBeforeShowEditor = GroupsBeforeShowEditor
    OnGetNewKey = GetNewKey
    OnFreeNewKey = FreeNewKey
    OnBeforeDelete = GroupsEditorBeforeDelete
    OnSaveToLog = SaveToLog
    NameFieldName = 'name'
    NotesFieldName = 'notes'
    NrmImage = -1
    SelImage = -1
    Left = 96
    Top = 92
  end
  object ItemsEditor: TRDbTreeEditor [21]
    AutoEdit = False
    CheckTags = True
    KeyFieldName = 'id'
    BlockValue = False
    OwnerFieldName = 'id_groups'
    LogEnable = True
    OpenMode = omAuto
    OnBeforeShowEditor = ItemsBeforeShowEditor
    OnGetNewKey = GetNewKey
    OnFreeNewKey = FreeNewKey
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
