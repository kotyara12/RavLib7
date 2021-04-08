unit UsersForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplTreeQuery, TmplDbTreeQuery, DB, Menus, ActnList, ImgList, Grids, DBGrids,
  RDbColorGrid, RDbPanel, ComCtrls, RavTreeView_old, Buttons, ExtCtrls, ToolWin,
  ADODB, RDbStatus, RDbCustom, RDbGridTuner, RDbOrder, RDbFilter, RDbFind,
  DBActns, StdCtrls, RDbText, RDbData, RVclUtils, RDbEditor,
  RDbCustomSearch, RDbSearch, RDbTree, RavTreeView, RDbUpdater, Tabs;

type
  TFormUsers = class(TDbTreeQueryTemplate)
    su_groups: TADOQuery;
    su_groupsid: TIntegerField;
    su_groupsowner_id: TIntegerField;
    su_groupsname: TStringField;
    su_groupsnotes: TStringField;
    su_users: TADOQuery;
    su_usersid: TIntegerField;
    su_usersid_groups: TIntegerField;
    su_usersname: TStringField;
    su_usersfullname: TStringField;
    su_usersnotes: TStringField;
    su_userspassword: TStringField;
    su_usersdeleted: TBooleanField;
    su_usersblocked: TBooleanField;
    su_userschanged: TDateTimeField;
    su_userscount_ep: TIntegerField;
    su_usersname_groups: TStringField;
    su_usersfont_style: TIntegerField;
    su_userscell_color: TIntegerField;
    RDbFilter_id: TRDFIntegerItem;
    RDbFilter_name: TRDFStringItem;
    RDbFilter_fullname: TRDFStringItem;
    RDbFilter_NOTES: TRDFStringItem;
    RDbFilter_DELETED: TRDFBooleanItem;
    RDbFilter_blocked: TRDFBooleanItem;
    RDbFilter_id_GROUPS: TRDFListLinkItem;
    IDRDbTextLabel: TLabel;
    IDRDbText: TRDbText;
    nameRDbText: TRDbText;
    fullnameRDbText: TRDbText;
    GroupsLabel: TLabel;
    name_GROUPSRDbText: TRDbText;
    NOTES_GROUPSRDbText: TRDbText;
    su_usersnotes_groups: TStringField;
    StateLabel: TLabel;
    blockedRDbText: TRDbText;
    DELETEDRDbText: TRDbText;
    NotesLabel: TLabel;
    NOTESRDbText: TRDbText;
    OperationsActionList: TActionList;
    itemLockUser: TMenuItem;
    itemUnlockUser: TMenuItem;
    divOper1: TMenuItem;
    itemResetPassword: TMenuItem;
    itemUnlockUserP: TMenuItem;
    itemLockUserP: TMenuItem;
    divOperP1: TMenuItem;
    itemResetPasswordP: TMenuItem;
    itemUnlockUserO: TMenuItem;
    itemLockUserO: TMenuItem;
    divOperO1: TMenuItem;
    itemResetPasswordO: TMenuItem;
    user_add_operation: TADOStoredProc;
    user_del_operation: TADOStoredProc;
    user_add_opgroup: TADOStoredProc;
    user_del_opgroup: TADOStoredProc;
    user_unlink_all: TADOStoredProc;
    UnlockUser: TAction;
    LockUser: TAction;
    ResetPassword: TAction;
    RDbUpdater_name: TRDUStringItem;
    RDbUpdater_fullname: TRDUStringItem;
    RDbUpdater_NOTES: TRDUStringItem;
    procedure CalcFields(DataSet: TDataSet);
    procedure UnlockUserUpdate(Sender: TObject);
    procedure UnlockUserExecute(Sender: TObject);
    procedure LockUserUpdate(Sender: TObject);
    procedure LockUserExecute(Sender: TObject);
    procedure ResetPasswordUpdate(Sender: TObject);
    procedure ResetPasswordExecute(Sender: TObject);
    procedure RDbEditorGetEditRights(Sender: TObject;
      const Mode: TEditMode; var Enable: Boolean);
    procedure RDbEditorGetEditorClass(Sender: TObject;
      var EditorClass: TFormClass);
    procedure RDbEditorCreateNewRecord(Sender: TObject;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure RDbEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure RDbEditorAfterPostLogged(Sender: TObject; Editor: TForm;
      OldData, NewData: TRecordData; const Mode: TEditMode;
      const EditTag: Integer; var Complete: Boolean);
    procedure TreeOpenDataSets(Sender: TObject; var Completed: Boolean);
    procedure TreeGetEditRights(Sender: TObject;
      const NodeType: TNodeType; const Mode: TEditMode;
      var Enable: Boolean);
    procedure GroupsGetEditorClass(Sender: TObject;
      var EditorClass: TFormClass);
    procedure DetailBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
  private
    procedure LoadLinkOpGroups(LV: TListView; const UserId: Integer);
    procedure LoadFreeOpGroups(LV: TListView; const UserId: Integer);
    procedure LoadLinkOperations(LV: TListView; const UserId: Integer);
    procedure LoadFreeOperations(LV: TListView; const UserId: Integer);
    procedure LoadEnabledOperations(LV: TListView; const UserId: Integer);
    procedure LoadEnabledWps(LV: TListView; const UserId: Integer);
    function  AddOpGroup(const UserId, GroupId: Integer): Boolean;
    function  DelOpGroup(const UserId, GroupId: Integer): Boolean;
    function  AddOperation(const UserId, OperId: Integer): Boolean;
    function  DelOperation(const UserId, OperId: Integer): Boolean;
    function  DeleteAllLinks(const UserId: Integer): Boolean;
    procedure SaveLinkOpGroups(LV: TListView; const UserId: Integer);
    procedure SaveFreeOpGroups(LV: TListView; const UserId: Integer);
    procedure SaveLinkOperations(LV: TListView; const UserId: Integer);
    procedure SaveFreeOperations(LV: TListView; const UserId: Integer);
    // Дополнительные операции
    procedure UnlockUser_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure UnlockUser_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure BlockUser_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure BlockUser_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure ResetPwd_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure ResetPwd_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
  protected
    // Загрузка основной таблицы
    function  LoadDataNode(const TreeActive: Boolean; Node: TTreeNode): Boolean; override;
    procedure ChangeTreeState(const TreeActive: Boolean); override;
  public
  end;

implementation

{$R *.dfm}

uses
  RMsgRu, RDialogs, RAppStyles, RExHandlers, RRssConst, RSysUtils,
  RDbConst, RDbUtils, RDbLog, RListView, RDbListView, BaseDbUnit, OprList,
  AdminUnit, AdminVars, TreeGroupForm, UsersProp;

//resourcestring
//  SCaptionFmt = 'Пользователь системы "%s" (%s)';

{ == Загрузка данных для дерева ================================================ }
procedure TFormUsers.TreeOpenDataSets(Sender: TObject; var Completed: Boolean);
begin
  Completed := BaseData.OpenDataSet(su_groups, False, tagEditUserGroup);
end;

{ == Загрузка данных таблицы =================================================== }
function TFormUsers.LoadDataNode(const TreeActive: Boolean; Node: TTreeNode): Boolean;
begin
  Result := LoadDataNodeDef(TreeActive, Node, tagEditUsers);
end;

{ == Смена статуса панели дерева =============================================== }
procedure TFormUsers.ChangeTreeState(const TreeActive: Boolean);
begin
  RDbFilter_ID_GROUPS.Enabled := not TreeActive;
end;

{ == Обработка вычисляемых полей =============================================== }
procedure TFormUsers.CalcFields(DataSet: TDataSet);
begin
  if su_usersdeleted.AsBoolean then
  begin
    su_usersfont_style.AsInteger := 8;
    su_userscell_color.AsInteger := ApplicationStyle.DataForm.CellNegative;
  end
  else begin
    if su_usersblocked.AsBoolean then
    begin
      su_usersfont_style.AsInteger := -1;
      su_userscell_color.AsInteger := ApplicationStyle.DataForm.CellNegative;
    end
    else begin
      su_usersfont_style.AsInteger := -1;
      su_userscell_color.AsInteger := ApplicationStyle.DataForm.CellPositive;
    end;
  end;
end;

{ == Редактирование дерева ===================================================== }
procedure TFormUsers.TreeGetEditRights(Sender: TObject;
  const NodeType: TNodeType; const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := orEditUserGroup;
end;

procedure TFormUsers.GroupsGetEditorClass(Sender: TObject;
  var EditorClass: TFormClass);
begin
  EditorClass := TFormTreeGroup;
end;

{ == Редактирование данных ===================================================== }
procedure TFormUsers.RDbEditorGetEditRights(Sender: TObject;
  const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := orEditUsers;
end;

procedure TFormUsers.RDbEditorGetEditorClass(Sender: TObject;
  var EditorClass: TFormClass);
begin
  EditorClass := TFormUsersProp;
end;

{ == Создание нового пользователя ============================================== }
procedure TFormUsers.RDbEditorCreateNewRecord(Sender: TObject;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  su_usersblocked.AsBoolean := False;
  su_usersdeleted.AsBoolean := False;
  su_userspassword.Clear;
end;

{ == Удаление пользователя ===================================================== }
procedure TFormUsers.RDbEditorBeforeDelete(Sender: TObject; OldData,
  NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  Complete := DeleteAllLinks(su_usersid.AsInteger);
end;

{ == Загрузка списков групп и операций ========================================= }
procedure TFormUsers.LoadLinkOpGroups(LV: TListView; const UserId: Integer);
const
  sqlLoad = 'SELECT id, name, notes FROM sr_opgroups WHERE id IN ' +
            '(SELECT id_opgroups FROM su_opglinks WHERE id_users=%d)';
var
  Qry: TAdoQuery;
begin
  Qry := nil;
  try
    try
      Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoad, [UserId]));
      if DataSetIsNotEmpty(Qry) then LoadDbListViewID(LV, Qry, fnlNN, imSave);
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoad, [UserId]),
          Format(SErrLoadLinkOpGroups, [UserId]));
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
end;

procedure TFormUsers.LoadFreeOpGroups(LV: TListView; const UserId: Integer);
const
  sqlLoad = 'SELECT id, name, notes FROM sr_opgroups WHERE id NOT IN ' +
            '(SELECT id_opgroups FROM su_opglinks WHERE id_users=%d)';
var
  Qry: TAdoQuery;
begin
  Qry := nil;
  try
    try
      Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoad, [UserId]));
      if DataSetIsNotEmpty(Qry) then LoadDbListViewID(LV, Qry, fnlNN, imSave);
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoad, [UserId]),
          Format(SErrLoadFreeOpGroups, [UserId]));
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
end;

procedure TFormUsers.LoadLinkOperations(LV: TListView; const UserId: Integer);
const
  sqlLoad = 'SELECT id, name, notes FROM vsr_operations_work WHERE id IN ' +
            '(SELECT id_operations FROM su_oprlinks WHERE id_users=%d)';
var
  Qry: TAdoQuery;
begin
  Qry := nil;
  try
    try
      Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoad, [UserId]));
      if DataSetIsNotEmpty(Qry) then LoadDbListViewID(LV, Qry, fnlINN, imSave);
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoad, [UserId]),
          Format(SErrLoadLinkOperations, [UserId]));
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
end;

procedure TFormUsers.LoadFreeOperations(LV: TListView; const UserId: Integer);
const
  sqlLoad = 'SELECT id, name, notes FROM vsr_operations_work WHERE id NOT IN ' +
            '(SELECT id_operations FROM su_oprlinks WHERE id_users=%d)';
var
  Qry: TAdoQuery;
begin
  Qry := nil;
  try
    try
      Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoad, [UserId]));
      if DataSetIsNotEmpty(Qry) then LoadDbListViewID(LV, Qry, fnlINN, imSave);
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoad, [UserId]),
          Format(SErrLoadFreeOperations, [UserId]));
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
end;

procedure TFormUsers.LoadEnabledOperations(LV: TListView; const UserId: Integer);
const
  sqlLoad   = 'SELECT vsr_operations_work.id, vsr_operations_work.name, vsr_operations_work.notes ' +
              'FROM vsr_operations_work WHERE vsr_operations_work.id IN ' +
              '(SELECT su_oprlinks.id_operations FROM su_oprlinks WHERE su_oprlinks.id_users=%d) ' +
              'OR vsr_operations_work.id IN (SELECT sr_opglinks.id_operations FROM sr_opglinks WHERE sr_opglinks.id_opgroups IN ' +
              '(SELECT su_opglinks.id_opgroups FROM su_opglinks WHERE su_opglinks.id_users=%0:d))';
  sqlLoadNo = 'SELECT vsr_operations_work.id, vsr_operations_work.name, vsr_operations_work.notes ' +
              'FROM vsr_operations_work WHERE vsr_operations_work.id NOT IN ' +
              '(SELECT su_oprlinks.id_operations FROM su_oprlinks WHERE su_oprlinks.id_users=%d) ' +
              'AND vsr_operations_work.id NOT IN (SELECT sr_opglinks.id_operations FROM sr_opglinks WHERE sr_opglinks.id_opgroups IN ' +
              '(SELECT su_opglinks.id_opgroups FROM su_opglinks WHERE su_opglinks.id_users=%0:d))';
var
  Qry: TAdoQuery;
begin
  Qry := nil;
  try
    try
      Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoad, [UserId]));
      if DataSetIsNotEmpty(Qry) then LoadDbListViewID(LV, Qry, fnlINN, imOk, True);
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoad, [UserId]),
          Format(SErrLoadEnabledOpers, [UserId]));
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
  try
    try
      Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoadNo, [UserId]));
      if DataSetIsNotEmpty(Qry) then LoadDbListViewID(LV, Qry, fnlINN, imCancel, False);
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoadNo, [UserId]),
          Format(SErrLoadEnabledOpers, [UserId]));
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
end;

procedure TFormUsers.LoadEnabledWps(LV: TListView; const UserId: Integer);
const
  sqlLoad   = 'SELECT sr_workplases.id, sr_workplases.name_s AS name, sr_workplases.name AS notes ' +
              'FROM sr_workplases WHERE sr_workplases.id IN ' +
              '(SELECT sr_opwlinks.id_workplases FROM sr_opwlinks WHERE sr_opwlinks.id_operations IN ' +
              '(SELECT su_oprlinks.id_operations FROM su_oprlinks WHERE su_oprlinks.id_users=%d) ' +
              'OR sr_opwlinks.id_operations IN (SELECT sr_opglinks.id_operations FROM sr_opglinks WHERE sr_opglinks.id_opgroups IN ' +
              '(SELECT su_opglinks.id_opgroups FROM su_opglinks WHERE su_opglinks.id_users=%0:d)))';
  sqlLoadNo = 'SELECT sr_workplases.id, sr_workplases.name_S AS name, sr_workplases.name AS notes ' +
              'FROM sr_workplases WHERE sr_workplases.id NOT IN ' +
              '(SELECT sr_opwlinks.id_workplases FROM sr_opwlinks WHERE sr_opwlinks.id_operations IN ' +
              '(SELECT su_oprlinks.id_operations FROM su_oprlinks WHERE su_oprlinks.id_users=%d) ' +
              'OR sr_opwlinks.id_operations IN (SELECT sr_opglinks.id_operations FROM sr_opglinks WHERE sr_opglinks.id_opgroups IN ' +
              '(SELECT su_opglinks.id_opgroups FROM su_opglinks WHERE su_opglinks.id_users=%0:d)))';
var
  Qry: TAdoQuery;
begin
  Qry := nil;
  try
    try
      Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoad, [UserId]));
      if DataSetIsNotEmpty(Qry) then LoadDbListViewID(LV, Qry, fnlNN, imOk, True);
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoad, [UserId]),
          Format(SErrLoadEnabledWps, [UserId]));
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
  try
    try
      Qry := OpenDynamicQuery(BaseData.acDb, Format(sqlLoadNo, [UserId]));
      if DataSetIsNotEmpty(Qry) then LoadDbListViewID(LV, Qry, fnlNN, imCancel, False);
    except
      on E: Exception do
        HandleSqlExcept(E, Self, Format(sqlLoadNo, [UserId]),
          Format(SErrLoadEnabledWps, [UserId]));
    end;
  finally
    FreeDynamicQuery(Qry);
  end;
end;

procedure TFormUsers.DetailBeforeShowEditor(Sender: TObject; Editor: TForm;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  inherited;

  if Assigned(Editor) then
  begin
    with Editor as TFormUsersProp do
    begin
      // Caption := Format(SCaptionFmt, [su_usersfullname.AsString, su_usersname.AsString]);
      LoadLinkOpGroups(LinkGroupListView, su_usersid.AsInteger);
      LoadFreeOpGroups(FreeGroupListView, su_usersid.AsInteger);
      LoadLinkOperations(LinkOperListView, su_usersid.AsInteger);
      LoadFreeOperations(FreeOperListView, su_usersid.AsInteger);
      LoadEnabledOperations(OpersAllListView, su_usersid.AsInteger);
      LoadEnabledWps(WpListView, su_usersid.AsInteger);
    end;
  end;
end;

{ == Сохранение списков групп и операций ======================================= }
function TFormUsers.AddOpGroup(const UserId, GroupId: Integer): Boolean;
begin
  Result := False;
  try
    BaseData.SPCheckSpName(user_add_opgroup);
    with user_add_opgroup.Parameters do
    begin
      Clear;
      CreateParameter(pnIdSysUser, ftInteger, pdInput, 0, UserId);
      CreateParameter(pnIdOpGroups, ftInteger, pdInput, 0, GroupId);
      CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, BaseData.GetAppTag);
      CreateParameter(pnIdUsers, ftInteger, pdInput,  0, BaseData.User.UserId);
      CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
      CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
    end;
    user_add_opgroup.ExecProc;
    Result := True;
  except
    on E: Exception do
      HandleExcept(E, user_add_opgroup, Format(SErrUserAddOpGroup, [GroupId, UserId]));
  end;
end;

function TFormUsers.DelOpGroup(const UserId, GroupId: Integer): Boolean;
begin
  Result := False;
  try
    BaseData.SPCheckSpName(user_del_opgroup);
    with user_del_opgroup.Parameters do
    begin
      Clear;
      CreateParameter(pnIdSysUser, ftInteger, pdInput, 0, UserId);
      CreateParameter(pnIdOpGroups, ftInteger, pdInput, 0, GroupId);
      CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, BaseData.GetAppTag);
      CreateParameter(pnIdUsers, ftInteger, pdInput,  0, BaseData.User.UserId);
      CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
      CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
    end;
    user_del_opgroup.ExecProc;
    Result := True;
  except
    on E: Exception do
      HandleExcept(E, user_del_opgroup, Format(SErrUserDelOpGroup, [GroupId, UserId]));
  end;
end;

function TFormUsers.AddOperation(const UserId, OperId: Integer): Boolean;
begin
  Result := False;
  try
    BaseData.SPCheckSpName(user_add_operation);
    with user_add_operation.Parameters do
    begin
      Clear;
      CreateParameter(pnIdSysUser, ftInteger, pdInput, 0, UserId);
      CreateParameter(pnIdOperations, ftInteger, pdInput, 0, OperId);
      CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, BaseData.GetAppTag);
      CreateParameter(pnIdUsers, ftInteger, pdInput,  0, BaseData.User.UserId);
      CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
      CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
    end;
    user_add_operation.ExecProc;
    Result := True;
  except
    on E: Exception do
      HandleExcept(E, user_add_operation, Format(SErrUserAddOperation, [OperId, UserId]));
  end;
end;

function TFormUsers.DelOperation(const UserId, OperId: Integer): Boolean;
begin
  Result := False;
  try
    BaseData.SPCheckSpName(user_del_operation);
    with user_del_operation.Parameters do
    begin
      Clear;
      CreateParameter(pnIdSysUser, ftInteger, pdInput, 0, UserId);
      CreateParameter(pnIdOperations, ftInteger, pdInput, 0, OperId);
      CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, BaseData.GetAppTag);
      CreateParameter(pnIdUsers, ftInteger, pdInput,  0, BaseData.User.UserId);
      CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
      CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
    end;
    user_del_operation.ExecProc;
    Result := True;
  except
    on E: Exception do
      HandleExcept(E, user_del_operation, Format(SErrUserDelOperation, [OperId, UserId]));
  end;
end;

function TFormUsers.DeleteAllLinks(const UserId: Integer): Boolean;
begin
  Result := False;
  try
    BaseData.SPCheckSpName(user_unlink_all);
    with user_unlink_all.Parameters do
    begin
      Clear;
      CreateParameter(pnIdSysUser, ftInteger, pdInput, 0, UserId);
      CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, BaseData.GetAppTag);
      CreateParameter(pnIdUsers, ftInteger, pdInput,  0, BaseData.User.UserId);
      CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
      CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
    end;
    user_unlink_all.ExecProc;
    Result := True;
  except
    on E: Exception do
      HandleExcept(E, user_unlink_all, Format(SErrUserDelAllLinks, [UserId]));
  end;
end;

procedure TFormUsers.SaveLinkOpGroups(LV: TListView; const UserId: Integer);
var
  i: Integer;
begin
  LV.Items.BeginUpdate;
  try
    for i := 0 to LV.Items.Count - 1 do
      if LV.Items[i].ImageIndex = imLink then
        AddOpGroup(UserId, GetItemID(LV.Items[i]));
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TFormUsers.SaveFreeOpGroups(LV: TListView; const UserId: Integer);
var
  i: Integer;
begin
  LV.Items.BeginUpdate;
  try
    for i := 0 to LV.Items.Count - 1 do
      if LV.Items[i].ImageIndex = imFree then
        DelOpGroup(UserId, GetItemID(LV.Items[i]));
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TFormUsers.SaveLinkOperations(LV: TListView; const UserId: Integer);
var
  i: Integer;
begin
  LV.Items.BeginUpdate;
  try
    for i := 0 to LV.Items.Count - 1 do
      if LV.Items[i].ImageIndex = imLink then
        AddOperation(UserId, GetItemID(LV.Items[i]));
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TFormUsers.SaveFreeOperations(LV: TListView; const UserId: Integer);
var
  i: Integer;
begin
  LV.Items.BeginUpdate;
  try
    for i := 0 to LV.Items.Count - 1 do
      if LV.Items[i].ImageIndex = imFree then
        DelOperation(UserId, GetItemID(LV.Items[i]));
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TFormUsers.RDbEditorAfterPostLogged(Sender: TObject;
  Editor: TForm; OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  if Assigned(Editor) then
  begin
    with Editor as TFormUsersProp do
    begin
      SaveLinkOpGroups(LinkGroupListView, su_usersid.AsInteger);
      SaveFreeOpGroups(FreeGroupListView, su_usersid.AsInteger);
      SaveLinkOperations(LinkOperListView, su_usersid.AsInteger);
      SaveFreeOperations(FreeOperListView, su_usersid.AsInteger);
    end;
  end;
end;

{ == Разрешить доступ в систему для текущего пользователя ====================== }
procedure TFormUsers.UnlockUserUpdate(Sender: TObject);
begin
  if RDbEditor.SelCount > 1
  then UnlockUser.Enabled := IsNotWait and orBlockUsers and RDbEditor.RecordIsSelected
  else UnlockUser.Enabled := IsNotWait and orBlockUsers and RDbEditor.RecordIsSelected
    and su_usersblocked.AsBoolean and not su_usersdeleted.AsBoolean;
end;

procedure TFormUsers.UnlockUser_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  Complete := su_usersblocked.AsBoolean and not su_usersdeleted.AsBoolean
    and (QueryBoxStdYN(Format(SQueryUnlockUser, [su_usersname.AsString, su_usersfullname.AsString])) = ID_YES);
end;

procedure TFormUsers.UnlockUser_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  StartWait;
  ShowInStatusBar(SMsgUnlockUser);
  try
    su_users.Edit;
    su_usersblocked.AsBoolean := False;
    su_userscount_ep.AsInteger := 0;
    su_users.Post;
    AddToDbLog(tagBlockUsers, Format(SLogUnlockUser, [su_usersname.AsString, su_usersfullname.AsString]));
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
  ResetPwd_Modify(nil, nil, Complete);
end;

procedure TFormUsers.UnlockUserExecute(Sender: TObject);
begin
  RDbEditor.ProcessSelectedRecords(UnlockUser_Check, UnlockUser_Modify, nil, SMsgUnlockUser, True, True, True);
end;

{ == Запретить доступ в систему для текущего пользователя ====================== }
procedure TFormUsers.LockUserUpdate(Sender: TObject);
begin
  if RDbEditor.SelCount > 1
  then LockUser.Enabled := IsNotWait and orBlockUsers and RDbEditor.RecordIsSelected
  else LockUser.Enabled := IsNotWait and orBlockUsers and RDbEditor.RecordIsSelected
    and not su_usersblocked.AsBoolean and not su_usersdeleted.AsBoolean;
end;

procedure TFormUsers.BlockUser_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  Complete := not su_usersblocked.AsBoolean and not su_usersdeleted.AsBoolean
    and (QueryBoxStdYN(Format(SQueryLockUser, [su_usersname.AsString, su_usersfullname.AsString])) = ID_YES);
end;

procedure TFormUsers.BlockUser_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  StartWait;
  ShowInStatusBar(SMsgLockUser);
  try
    su_users.Edit;
    su_usersblocked.AsBoolean := True;
    su_users.Post;
    AddToDbLog(tagBlockUsers, Format(SLogLockUser, [su_usersname.AsString, su_usersfullname.AsString]));
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormUsers.LockUserExecute(Sender: TObject);
begin
  RDbEditor.ProcessSelectedRecords(BlockUser_Check, BlockUser_Modify, nil, SMsgLockUser, True, True, True);
end;

{ == Установить пароль пользователя на первоначальный ========================== }
procedure TFormUsers.ResetPasswordUpdate(Sender: TObject);
begin
  if RDbEditor.SelCount > 1
  then ResetPassword.Enabled := IsNotWait and orBlockUsers and RDbEditor.RecordIsSelected
  else ResetPassword.Enabled := IsNotWait and orBlockUsers and RDbEditor.RecordIsSelected
    and not su_usersdeleted.AsBoolean;
end;

procedure TFormUsers.ResetPwd_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  Complete := not su_usersdeleted.AsBoolean and (QueryBoxStdYN(Format(SQueryResetPwdUser,
    [su_usersname.AsString, su_usersfullname.AsString])) = ID_YES);
end;

procedure TFormUsers.ResetPwd_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  if (Sender <> nil) or (QueryBoxStdYN(Format(SQueryResetPwdUser,
  [su_usersname.AsString, su_usersfullname.AsString])) = ID_YES) then
  begin
    StartWait;
    ShowInStatusBar(SMsgResetPwdUser);
    try
      su_users.Edit;
      su_userspassword.Clear;
      su_userschanged.AsDateTime := Now;
      su_userscount_ep.AsInteger := 0;
      su_users.Post;
      AddToDbLog(tagBlockUsers, Format(SLogResetPwdUser, [su_usersname.AsString, su_usersfullname.AsString]));
    finally
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

procedure TFormUsers.ResetPasswordExecute(Sender: TObject);
begin
  RDbEditor.ProcessSelectedRecords(ResetPwd_Check, ResetPwd_Modify, nil, SMsgResetPwdUser, True, True, True);
end;

end.
