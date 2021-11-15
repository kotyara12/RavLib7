inherited FormOpGroups: TFormOpGroups
  Left = 311
  Top = 235
  Caption = #1043#1088#1091#1087#1087#1099' '#1086#1087#1077#1088#1072#1094#1080#1081
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000040040000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000002F58A0FF2F58A0FF2F58A0FF2F58A0FF2F58A0FF2F58A0FF2F58
    A0FF2F58A0FF2F58A0FF2F58A0FF2F58A0FF2F58A0FF2F58A0FF000000000000
    0000000000002F58A0FF4B79C8FF4B79C8FF4B79C8FF4B79C8FF4B79C8FF4B79
    C8FF4B79C8FF4B79C8FF4B79C8FF4B79C8FF4B79C8FF2F58A0FF000000000000
    0000000000002F58A0FF4B79C8FF4B79C8FF4B79C8FF4B79C8FF4B79C8FF4B79
    C8FF4B79C8FF4B79C8FF4B79C8FF4B79C8FF4B79C8FF2F58A0FF000000000000
    0000000000002F58A0FF4B79C8FF4B79C8FF4B79C8FF4367A4FF39517AFF3951
    7AFF39517AFF41649EFF4B79C8FF4B79C8FF4B79C8FF2F58A0FF000000000000
    0000000000002F58A0FF4B79C8FF4B79C8FF4B79C8FF39517AFF2CA4C1FF2CA4
    C1FF2CA4C1FF39517AFF4B79C8FF4B79C8FF4B79C8FF2F58A0FF000000000000
    00002F4C827B2F4978FF2F4873FF2F4873FF2F4873FF2F4873FF2CA4C1FF37D7
    FFFFFFFFFFFF2F4873FF2F4873FF2F4873FF2F4873FF2F4978FF2F4F89860000
    00002C5FB6EB148BDCFF0E94E4FF0E94E4FF0E94E4FF0E94E4FF0E94E4FF0E94
    E4FF0E94E4FF0E94E4FF0E94E4FF0E94E4FF0E94E4FF1886D7FF3161B6EF0000
    00002F66BAFF139DE8FF139DE8FF139DE8FF139DE8FF139DE8FF139DE8FF139D
    E8FF139DE8FF139DE8FF139DE8FF139DE8FF139DE8FF139DE8FF2F66BAFF0000
    00002C70C1FF19A6EBFF19A6EBFF19A6EBFF19A6EBFF19A6EBFF19A6EBFF19A6
    EBFF19A6EBFF19A6EBFF19A6EBFF19A6EBFF19A6EBFF19A6EBFF2C70C1FF0000
    0000297AC9FF1EAFEFFF1EAFEFFF1EAFEFFF1EAFEFFF1EAFEFFF1EAFEFFF1EAF
    EFFF1EAFEFFF1EAFEFFF1EAFEFFF1EAFEFFF1EAFEFFF1EAFEFFF297AC9FF0000
    00002683D1FF24B8F2FF24B8F2FF24B8F2FF24B8F2FF24B8F2FF24B8F2FF24B8
    F2FF24B8F2FF24B8F2FF24B8F2FF24B8F2FF24B8F2FF24B8F2FF2683D1FF0000
    0000228DD9FF228DD9FF228DD9FF228DD9FF228DD9FF228DD9FF228DD9FF228D
    D9FF228DD9FF228DD9FF228DD9FF228DD9FF228DD9FF228DD9FF228DD9FF0000
    000000000000000000000000000000000000172440C300000000000000000000
    00000000000000000000172440C3000000000000000000000000000000000000
    00000000000000000000000000000000000015263F3C2D3645FF2D3645FF2D36
    45FF2D3645FF2D3645FF15263F3C000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000C0010000C0010000C0010000C0010000C001000080000000800000008000
    000080000000800000008000000080000000FBEF0000F80F0000FFFF0000}
  PixelsPerInch = 96
  TextHeight = 13
  inherited CoolBar: TCoolBar
    inherited ToolBar: TToolBar
      inherited NewToolButton: TToolButton
        Action = NewGroup
        DropdownMenu = nil
      end
    end
  end
  inherited TreeView: TRTreeView
    Images = AdminData.AdminImageList
    AutoExpandMode = emGroups
  end
  inherited ActionList: TActionList
    inherited NewGroup: TAction
      Caption = #1057#1086#1079#1076#1072#1090#1100
    end
    inherited CopyRecord: TAction
      Visible = False
    end
    inherited NewSubGroup: TAction
      Visible = False
    end
    inherited NewItem: TAction
      Visible = False
    end
    inherited MoveItem: TAction
      Visible = False
    end
  end
  inherited MainMenu: TMainMenu
    inherited menuEdit: TMenuItem
      inherited divEditNew: TMenuItem
        Visible = False
      end
      inherited divEditEdit: TMenuItem
        Visible = False
      end
    end
  end
  inherited TreeLoader: TRDbTreeLoader
    OnOpenDataSets = OpenOpGroups
    OnBeforeLoadTree = OpenOperations
    OnAfterLoadTree = CloseOperations
    RootText = #1056#1054#1051#1048' '#1055#1054#1051#1068#1047#1054#1042#1040#1058#1045#1051#1045#1049' ('#1043#1056#1059#1055#1055#1067' '#1054#1055#1045#1056#1040#1062#1048#1049')'
    RootNrmImage = 3
    RootSelImage = 3
    LinkedItemsDS = True
    EnableParentNodeDelete = True
    OnGetEditRights = GetEditRights
  end
  inherited GroupsEditor: TRDbTreeEditor
    DataSet = sr_opgroups
    KeyFieldName = 'id'
    OwnerFieldName = ''
    CopiedFields = 'name;notes'
    ObjectDesc = #1043#1088#1091#1087#1087#1099' '#1086#1087#1077#1088#1072#1094#1080#1081
    OnGetEditorClass = GroupsGetEditorClass
    OnAfterPostLogged = GroupsAfterPostLogged
    OnBeforeDelete = GroupsBeforeDelete
    NameFieldName = 'name'
    NotesFieldName = 'notes'
    NrmImage = 1
    SelImage = 1
    OnAfterUpdateNode = GroupsAfterEditNode
  end
  inherited ItemsEditor: TRDbTreeEditor
    DataSet = sr_opers_link
    KeyFieldName = 'id'
    OwnerFieldName = ''
    ObjectName = 'sr_opglinks'
    ObjectDesc = #1054#1087#1077#1088#1072#1094#1080#1080
    CheckDataSetState = False
    NameFieldName = 'name'
    NotesFieldName = 'notes'
    NrmImage = 2
    SelImage = 2
    OnGetName = ItemsGetName
  end
  object sr_opgroups: TADOTable
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    IndexName = 'pk_sr_opgroups_id'
    TableName = 'sr_opgroups'
    Left = 112
    Top = 120
    object sr_opgroupsid: TIntegerField
      Tag = 1
      DisplayLabel = #1050#1086#1076' (id)'
      FieldName = 'id'
      Visible = False
    end
    object sr_opgroupsname: TStringField
      Tag = 1
      DisplayLabel = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
      FieldName = 'name'
      Required = True
      Size = 64
    end
    object sr_opgroupsnotes: TStringField
      DisplayLabel = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1089#1074#1077#1076#1077#1085#1080#1103
      FieldName = 'notes'
      Size = 255
    end
  end
  object sr_opers_link: TADOQuery
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltReadOnly
    DataSource = GroupsEditor
    Parameters = <
      item
        Name = 'id'
        DataType = ftInteger
        Size = -1
        Value = -1
      end>
    SQL.Strings = (
      'SELECT * FROM vsr_operations_work WHERE id IN'
      '  (SELECT id_operations FROM sr_opglinks WHERE id_opgroups=:id)')
    Left = 140
    Top = 120
    object sr_opers_linkid: TIntegerField
      Tag = 1
      FieldName = 'id'
    end
    object sr_opers_linkid_levels: TIntegerField
      FieldName = 'id_levels'
    end
    object sr_opers_linkname: TStringField
      Tag = 1
      FieldName = 'name'
      Size = 64
    end
    object sr_opers_linknotes: TStringField
      Tag = 1
      FieldName = 'notes'
      Size = 255
    end
    object sr_opers_linklevels_name: TStringField
      FieldName = 'levels_name'
      Size = 64
    end
    object sr_opers_linklevels_notes: TStringField
      FieldName = 'levels_notes'
      Size = 255
    end
    object sr_opers_linkfont_style: TIntegerField
      FieldName = 'font_style'
    end
    object sr_opers_linkfont_color: TIntegerField
      FieldName = 'font_color'
    end
    object sr_opers_linkcell_color: TIntegerField
      FieldName = 'cell_color'
    end
  end
  object sr_opers_free: TADOQuery
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltReadOnly
    DataSource = GroupsEditor
    Parameters = <
      item
        Name = 'id'
        DataType = ftInteger
        Size = -1
        Value = -1
      end>
    SQL.Strings = (
      'SELECT * FROM vsr_operations_work WHERE NOT id IN '
      '  (SELECT id_operations FROM sr_opglinks WHERE id_opgroups=:id)')
    Left = 168
    Top = 120
    object sr_opers_freeid: TIntegerField
      Tag = 1
      FieldName = 'id'
    end
    object sr_opers_freeid_levels: TIntegerField
      FieldName = 'id_levels'
    end
    object sr_opers_freename: TStringField
      Tag = 1
      FieldName = 'name'
      Size = 64
    end
    object sr_opers_freenotes: TStringField
      Tag = 1
      FieldName = 'notes'
      Size = 255
    end
    object sr_opers_freelevels_name: TStringField
      FieldName = 'levels_name'
      Size = 64
    end
    object sr_opers_freelevels_notes: TStringField
      FieldName = 'levels_notes'
      Size = 255
    end
    object sr_opers_freefont_style: TIntegerField
      FieldName = 'font_style'
    end
    object sr_opers_freefont_color: TIntegerField
      FieldName = 'font_color'
    end
    object sr_opers_freecell_color: TIntegerField
      FieldName = 'cell_color'
    end
  end
  object group_add_operation: TADOStoredProc
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    ProcedureName = 'sp_sys_group_add_operation;1'
    Parameters = <>
    Left = 196
    Top = 120
  end
  object group_del_operation: TADOStoredProc
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    ProcedureName = 'sp_sys_group_del_operation;1'
    Parameters = <>
    Left = 224
    Top = 120
  end
  object group_unlink_all: TADOStoredProc
    AutoCalcFields = False
    Connection = BaseData.acDb
    LockType = ltPessimistic
    ProcedureName = 'sp_sys_group_unlink_all;1'
    Parameters = <>
    Left = 252
    Top = 120
  end
end
