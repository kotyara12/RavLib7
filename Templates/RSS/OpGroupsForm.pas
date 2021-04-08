unit OpGroupsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplTree, Menus, ActnList, ComCtrls, RavTreeView, ToolWin,
  TmplDbTree, DB, RDbEditor, RDbTree, RDbData, ADODB, StdCtrls, Buttons,
  ExtCtrls;

type
  TFormOpGroups = class(TDbTreeTemplate)
    sr_opgroups: TADOTable;
    sr_opgroupsid: TIntegerField;
    sr_opgroupsname: TStringField;
    sr_opgroupsnotes: TStringField;
    sr_opers_link: TADOQuery;
    sr_opers_linkid: TIntegerField;
    sr_opers_linkid_levels: TIntegerField;
    sr_opers_linkname: TStringField;
    sr_opers_linknotes: TStringField;
    sr_opers_linklevels_name: TStringField;
    sr_opers_linklevels_notes: TStringField;
    sr_opers_linkfont_style: TIntegerField;
    sr_opers_linkfont_color: TIntegerField;
    sr_opers_linkcell_color: TIntegerField;
    sr_opers_free: TADOQuery;
    group_add_operation: TADOStoredProc;
    group_del_operation: TADOStoredProc;
    group_unlink_all: TADOStoredProc;
    sr_opers_freeid: TIntegerField;
    sr_opers_freeid_levels: TIntegerField;
    sr_opers_freename: TStringField;
    sr_opers_freenotes: TStringField;
    sr_opers_freelevels_name: TStringField;
    sr_opers_freelevels_notes: TStringField;
    sr_opers_freefont_style: TIntegerField;
    sr_opers_freefont_color: TIntegerField;
    sr_opers_freecell_color: TIntegerField;
    procedure OpenOperations(Sender: TObject;
      var Completed: Boolean);
    procedure OpenOpGroups(Sender: TObject;
      var Completed: Boolean);
    procedure CloseOperations(Sender: TObject;
      var Completed: Boolean);
    procedure GetEditRights(Sender: TObject; const NodeType: TNodeType;
      const Mode: TEditMode; var Enable: Boolean);
    procedure GroupsGetEditorClass(Sender: TObject;
      var EditorClass: TFormClass);
    procedure PropertiesExecute(Sender: TObject);
    procedure GroupsBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure GroupsAfterPostLogged(Sender: TObject;
      Editor: TForm; OldData, NewData: TRecordData; const Mode: TEditMode;
      const EditTag: Integer; var Complete: Boolean);
    procedure GroupsAfterEditNode(Sender: TObject; const Node: TTreeNode;
      const Mode: TEditMode);
    procedure ItemsGetName(Sender: TObject; var Value: String);
    procedure DeleteItemExecute(Sender: TObject);
    procedure GroupsBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
  private
    FStartEditorPage: Integer;
    function  AddOperation(const GroupId, OperId: Integer): Boolean;
    function  DelOperation(const GroupId, OperId: Integer): Boolean;
    function  CheckUserLinks(const GroupId: Integer; const GroupName: string): Boolean;
    function  DeleteAllLinks(const GroupId: Integer): Boolean;
    procedure StoreLinkOperations(const GroupId: Integer; ListView: TListView);
    procedure StoreFreeOperations(const GroupId: Integer; ListView: TListView);
  end;

implementation

{$R *.dfm}

uses
  RVclUtils, RMsgRu, RDialogs, RListView, RDbListView, RDbConst,
  RRssConst, RDbUtils, RExHandlers, RSysUtils,
  BaseDbUnit, AdminUnit, AdminVars, OprList, OpGroupsProp;

{ == Загрузка из базы данных =================================================== }
procedure TFormOpGroups.OpenOpGroups(Sender: TObject; var Completed: Boolean);
begin
  Completed := BaseData.OpenDataSet(sr_opgroups, False, tagEditOpGroups);
end;

procedure TFormOpGroups.OpenOperations(Sender: TObject; var Completed: Boolean);
begin
  Completed := BaseData.OpenDataSet(sr_opers_link, False, 0)
           and BaseData.OpenDataSet(sr_opers_free, False, 0);
end;

procedure TFormOpGroups.CloseOperations(Sender: TObject; var Completed: Boolean);
begin
  Completed := True;
  if sr_opers_link.Active then sr_opers_link.Close;
  if sr_opers_free.Active then sr_opers_free.Close;
end;

procedure TFormOpGroups.ItemsGetName(Sender: TObject; var Value: String);
const
  fmtOperNode = '%d - %s';
begin
  Value := Format(fmtOperNode, [TRDbTreeEditor(Sender).GetKeyValue, Value]);
end;

{ == Сохранение списка операций ================================================ }
function TFormOpGroups.AddOperation(const GroupId, OperId: Integer): Boolean;
begin
  Result := False;
  try
    BaseData.SPCheckSpName(group_add_operation);
    with group_add_operation.Parameters do
    begin
      Clear;
      CreateParameter(pnIdOpGroups, ftInteger, pdInput, 0, GroupId);
      CreateParameter(pnIdOperations, ftInteger, pdInput, 0, OperId);
      CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, BaseData.GetAppTag);
      CreateParameter(pnIdUsers, ftInteger, pdInput,  0, BaseData.User.UserId);
      CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
      CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
    end;
    group_add_operation.ExecProc;
    Result := True;
  except
    on E: Exception do
      HandleExcept(E, group_add_operation, Format(SErrGroupAddOperation, [OperId, GroupId]));
  end;
end;

function TFormOpGroups.DelOperation(const GroupId, OperId: Integer): Boolean;
begin
  Result := False;
  try
    BaseData.SPCheckSpName(group_del_operation);
    with group_del_operation.Parameters do
    begin
      Clear;
      CreateParameter(pnIdOpGroups, ftInteger, pdInput, 0, GroupId);
      CreateParameter(pnIdOperations, ftInteger, pdInput, 0, OperId);
      CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, BaseData.GetAppTag);
      CreateParameter(pnIdUsers, ftInteger, pdInput,  0, BaseData.User.UserId);
      CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
      CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
    end;
    group_del_operation.ExecProc;
    Result := True;
  except
    on E: Exception do
      HandleExcept(E, group_del_operation, Format(SErrGroupDelOperation, [OperId, GroupId]));
  end;
end;

procedure TFormOpGroups.StoreLinkOperations(const GroupId: Integer; ListView: TListView);
var
  i: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for i := 0 to ListView.Items.Count - 1 do
      if ListView.Items[i].ImageIndex = imLink then
        AddOperation(GroupId, GetItemID(ListView.Items[i]));
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure TFormOpGroups.StoreFreeOperations(const GroupId: Integer; ListView: TListView);
var
  i: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for i := 0 to ListView.Items.Count - 1 do
      if ListView.Items[i].ImageIndex = imFree then
        DelOperation(GroupId, GetItemID(ListView.Items[i]));
  finally
    ListView.Items.EndUpdate;
  end;
end;

{ == Проверка на наличие связей группы с пользователями ======================== }
function TFormOpGroups.CheckUserLinks(const GroupId: Integer; const GroupName: string): Boolean;
const
  sqlUserOperGroup = 'SELECT name, fullname FROM su_users WHERE deleted=0 ' +
                     'AND id IN (SELECT id_users FROM su_opglinks WHERE id_opgroups=%d) ' +
                     'ORDER BY fullname';
  fmtUserItem      = ' - %s (%s)';
var
  UserLinks: TAdoQuery;
  UserList: string;
  UserCount: Integer;
begin
  Result := False;
  UserLinks := nil;
  try
    UserCount := 0;
    UserList := EmptyStr;
    try
      UserLinks := OpenDynamicQuery(BaseData.acDb, Format(sqlUserOperGroup, [GroupId]));
      if DataSetIsNotEmpty(UserLinks) then begin
        while not UserLinks.Eof do
        begin
          Inc(UserCount);
          if UserCount < 6
          then UserList := UserList + #13 + Format(fmtUserItem,
                 [UserLinks.FieldByName(fnNAME).AsString,
                  UserLinks.FieldByName(fnFULLNAME).AsString])
          else if UserCount = 6 then UserList := UserList + #13 + SDots;
          UserLinks.Next;
          Application.ProcessMessages;
        end;
        Result := (UserCount = 0) or (WarningBoxNY(Format(SWarningUserOpGroup,
          [GroupName, UserCount, UserList])) = ID_YES);
      end
      else Result := True;
    finally
      FreeDynamicQuery(UserLinks);
    end;
  except
    on E: Exception do
      HandleSqlExcept(E, Self, Format(sqlUserOperGroup, [GroupId]),
        Format(SErrGroupCheckUsrLinks, [GroupId, GroupName]));
  end;
end;

function TFormOpGroups.DeleteAllLinks(const GroupId: Integer): Boolean;
begin
  Result := False;
  try
    BaseData.SPCheckSpName(group_unlink_all);
    with group_unlink_all.Parameters do
    begin
      Clear;
      CreateParameter(pnIdOpGroups, ftInteger, pdInput, 0, GroupId);
      CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, BaseData.GetAppTag);
      CreateParameter(pnIdUsers, ftInteger, pdInput,  0, BaseData.User.UserId);
      CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
      CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
    end;
    group_unlink_all.ExecProc;
    Result := True;
  except
    on E: Exception do
      HandleExcept(E, group_unlink_all, Format(SErrGroupDelAllLinks, [GroupId]));
  end;
end;

{ == Проверка прав на выполнение операций ====================================== }
procedure TFormOpGroups.GetEditRights(Sender: TObject;
  const NodeType: TNodeType; const Mode: TEditMode; var Enable: Boolean);
begin
  Enable := orEditOpGroups;
end;

{ == Вызов редактора =========================================================== }
procedure TFormOpGroups.GroupsGetEditorClass(Sender: TObject;
  var EditorClass: TFormClass);
begin
  EditorClass := TFormOpGroupsProp;
end;

{ == Редактирование ============================================================ }
procedure TFormOpGroups.PropertiesExecute(Sender: TObject);
begin
  FStartEditorPage := 0;
  if TreeView.GetNodeType(TreeView.Selected) = ntItem then
  begin
    TreeView.Selected := TreeView.Selected.Parent;
    FStartEditorPage := 1;
  end;
  if TreeView.Selected <> nil then
    TreeLoader.EditNode(TreeView.Selected, TreeLoader.NodeCanEdited(TreeView.Selected, True));
end;

procedure TFormOpGroups.GroupsBeforeShowEditor(Sender: TObject;
  Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  inherited;
  if Mode = etInsert then
  begin
    sr_opers_link.DataSource := nil;
    sr_opers_free.DataSource := nil;
    sr_opers_link.Parameters.ParamByName(fnID).Value := -1;
    sr_opers_free.Parameters.ParamByName(fnID).Value := -1;
  end;
  try
    OpenOperations(Self, Complete);
    if Complete then
    begin
      try
        LoadDbListViewID(TFormOpGroupsProp(Editor).LinkListView, sr_opers_link, fnlINN, imSave);
        LoadDbListViewID(TFormOpGroupsProp(Editor).FreeListView, sr_opers_free, fnlINN, imSave);
        if Mode in [etView, etEdit]
        then TFormOpGroupsProp(Editor).PageControl.ActivePageIndex := FStartEditorPage
        else TFormOpGroupsProp(Editor).PageControl.ActivePageIndex := 0;
      finally
        CloseOperations(Self, Complete);
      end;
    end;
  finally
    if Mode = etInsert then
    begin
      sr_opers_link.DataSource := GroupsEditor;
      sr_opers_free.DataSource := GroupsEditor;
    end;
  end;
end;

procedure TFormOpGroups.GroupsAfterPostLogged(Sender: TObject;
  Editor: TForm; OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  StoreLinkOperations(sr_opgroupsid.AsInteger, TFormOpGroupsProp(Editor).LinkListView);
  StoreFreeOperations(sr_opgroupsid.AsInteger, TFormOpGroupsProp(Editor).FreeListView);
end;

procedure TFormOpGroups.GroupsAfterEditNode(Sender: TObject;
  const Node: TTreeNode; const Mode: TEditMode);
begin
  try
    if TreeLoader.BeforeLoadTree then
    begin
      try
        if TreeLoader.ReloadNode(Node) then Node.Expand(False);
      finally
        TreeLoader.AfterLoadTree;
      end;
    end;
  except
    on E: Exception do
      HandleExcept(E, TreeLoader, SErrReloadTree);
  end;
end;

{ == Удаление ================================================================== }
procedure TFormOpGroups.DeleteItemExecute(Sender: TObject);
begin
  case TreeView.GetNodeType(TreeView.Selected) of
    ntGroup: TreeLoader.DeleteNode(TreeView.Selected, True);
    ntItem:  if GroupsEditor.LocateKey(TreeView.GetNodeId(TreeView.Selected.Parent)) then
             begin
               if DeleteQueryText(Format(SQryDelOperFromGroup,
                 [TreeView.GetNodeId(TreeView.Selected), GroupsEditor.GetNameValue])) then
               begin
                 if DelOperation(TreeView.GetNodeId(TreeView.Selected.Parent),
                   TreeView.GetNodeId(TreeView.Selected))
                 then TreeView.DeleteSelection;
               end;
             end
             else raise Exception.CreateFmt(SErrIdNotFound,
               [TreeView.GetNodeId(TreeView.Selected.Parent)]);
  end;
end;

procedure TFormOpGroups.GroupsBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  Complete := CheckUserLinks(sr_opgroupsid.AsInteger, sr_opgroupsname.AsString)
    and DeleteAllLinks(sr_opgroupsid.AsInteger);
end;

end.
