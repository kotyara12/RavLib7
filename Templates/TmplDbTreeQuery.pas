unit TmplDbTreeQuery;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplTreeQuery, RDbStatus, RDbCustom, RDbGridTuner, RDbOrder,
  RDbFilter, RDbFind, AdoDb, DB, Menus, DBActns, ActnList, Grids, DBGrids, RDbData,
  RDbColorGrid, RDbPanel, ComCtrls, RavTreeView, Buttons, ExtCtrls, ToolWin,
  RDbEditor, RDbCustomSearch, RDbSearch, RDbTree, RDbUpdater, StdCtrls,
  Tabs;

type
  TDbTreeQueryTemplate = class(TTreeQueryTemplate)
    TreeLoader: TRDbTreeLoader;
    GroupsEditor: TRDbTreeEditor;
    ItemsEditor: TRDbTreeEditor;
    TreeAttachments: TAction;
    itemTreeAttachments: TMenuItem;
    divTreeAttachP: TMenuItem;
    SubitemsEditor: TRDbTreeEditor;
    procedure GroupsBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure ItemsBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure TreeAttachmentsUpdate(Sender: TObject);
    procedure TreeAttachmentsExecute(Sender: TObject);
    procedure GroupsEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure ItemsEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure SubitemsEditorBeforeShowEditor(Sender: TObject;
      Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure SubitemsEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
  protected
    procedure InitDataComponents; override;
    // Загрузка данных
    function  LoadDataTreeBegin: Boolean; override;
    function  LoadDataTree: Boolean; override;
    function  LoadDataTreeEnd: Boolean; override;
    function  LoadDataNodeDef(const TreeActive: Boolean; Node: TTreeNode; const OpenTag: Integer): Boolean;
    procedure CloseDataSets; override;
    // Редактирование дерева
    function  TreeGroupInsertEnabled(Node: TTreeNode): Boolean; override;
    function  TreeSubGroupInsertEnabled(Node: TTreeNode): Boolean; override;
    function  TreeItemInsertEnabled(Node: TTreeNode): Boolean; override;
    function  TreeSubitemInsertEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeCopyEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeOpenEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeMoveEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeEditEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeDeleteEnabled(Node: TTreeNode): Boolean; override;
    procedure TreeNodeInsertGroup(Node: TTreeNode); override;
    procedure TreeNodeInsertSubGroup(Node: TTreeNode); override;
    procedure TreeNodeInsertItem(Node: TTreeNode); override;
    procedure TreeNodeInsertSubitem(Node: TTreeNode); override;
    function  TreeNodeSelectTargetNode(MovedNode: TTreeNode): TTreeNode; override;
    function  TreeNodeMove(MovedNode, TargetNode: TTreeNode): Boolean; override;
    procedure TreeNodeCopy(Node: TTreeNode); override;
    procedure TreeNodeEdit(Node: TTreeNode); override;
    procedure TreeNodeDelete(Node: TTreeNode); override;
    // Редактирование данных
    function  DetailSelectTargetNode(MovedNode: TTreeNode): TTreeNode; override;
    procedure DetailFindOwnerNode; override;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF RSS} RDbLog, {$ENDIF}
  {$IFDEF ATTACH} RDbAttachs, {$ENDIF}
  RVclUtils, RMsgRu, RDialogs, RTreeMove, RDbState, BaseDbUnit, TmplDbDialog;

procedure TDbTreeQueryTemplate.InitDataComponents;
begin
  try
    inherited;
  finally
    FRootOnlyEnabled := (ItemsEditor.DataSet = nil) or not ItemsEditor.Enabled;
    {$IFDEF ATTACH}
    TreeAttachments.Visible := GroupsEditor.KeyFieldIsPresent or ItemsEditor.KeyFieldIsPresent;
    divTreeAttachP.Visible := TreeAttachments.Visible;
    {$ELSE}
    TreeAttachments.Visible := False;
    divTreeAttachP.Visible := False;
    {$ENDIF}
  end;
end;

{ == Загрузка данных =========================================================== }
function TDbTreeQueryTemplate.LoadDataTreeBegin: Boolean;
begin
  Result := TreeLoader.BeforeLoadTree;
end;

function TDbTreeQueryTemplate.LoadDataTree: Boolean;
begin
  Result := TreeLoader.LoadTree;
end;

function TDbTreeQueryTemplate.LoadDataTreeEnd: Boolean;
begin
  Result := TreeLoader.AfterLoadTree;
end;

function TDbTreeQueryTemplate.LoadDataNodeDef(const TreeActive: Boolean; Node: TTreeNode; const OpenTag: Integer): Boolean;
const
  sqlTreeFilter = '(%s IN (%s))';
  sqlBaseSelect = 'SELECT * FROM %s';
var
  NodeFilter: string;
begin
  if TreeActive then
  begin
    if TreeLoader.SubitemsIsLoaded(True)
    then NodeFilter := Format(sqlTreeFilter, [RDbEditor.OwnerFieldName, TreeView.GetIdList(Node, [ntSubitem])])
    else begin
      if TreeLoader.ItemsIsLoaded(True)
      then NodeFilter := Format(sqlTreeFilter, [RDbEditor.OwnerFieldName, TreeView.GetIdList(Node, [ntItem])])
      else NodeFilter := Format(sqlTreeFilter, [RDbEditor.OwnerFieldName, TreeView.GetIdList(Node, [ntGroup])])
    end;
  end
  else NodeFilter := EmptyStr;
  Result := BaseData.OpenVariableQuery(TAdoQuery(RDbEditor.DataSet), RDbFilter, RDbOrder,
    Format(sqlBaseSelect, [RDbEditor.GetObjectName(etView)]), NodeFilter, OpenTag);
end;

procedure TDbTreeQueryTemplate.CloseDataSets;
begin
  inherited;
  TreeLoader.CloseDataSets;
end;

{ == Обработка событий ========================================================= }
procedure TDbTreeQueryTemplate.GroupsBeforeShowEditor(Sender: TObject;
  Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  if Editor is TDbDialogTemplate then
  begin
    TDbDialogTemplate(Editor).Caption := GetEditorCaption(
      TRDbCustomEditor(Sender).GetObjectDesc(etView), TRDbCustomEditor(Sender).DataSet);
    if TDbDialogTemplate(Editor).DataSource.DataSet <> TRDbCustomEditor(Sender).DataSet then
      TDbDialogTemplate(Editor).DataSource.DataSet := TRDbCustomEditor(Sender).DataSet;
  end;
end;

procedure TDbTreeQueryTemplate.ItemsBeforeShowEditor(Sender: TObject;
  Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  if Editor is TDbDialogTemplate then
  begin
    TDbDialogTemplate(Editor).Caption := GetEditorCaption(
      TRDbCustomEditor(Sender).GetObjectDesc(etView), TRDbCustomEditor(Sender).DataSet);
    if TDbDialogTemplate(Editor).DataSource.DataSet <> TRDbCustomEditor(Sender).DataSet then
      TDbDialogTemplate(Editor).DataSource.DataSet := TRDbCustomEditor(Sender).DataSet;
  end;
end;

procedure TDbTreeQueryTemplate.SubitemsEditorBeforeShowEditor(
  Sender: TObject; Editor: TForm; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  if Editor is TDbDialogTemplate then
  begin
    TDbDialogTemplate(Editor).Caption := GetEditorCaption(
      TRDbCustomEditor(Sender).GetObjectDesc(etView), TRDbCustomEditor(Sender).DataSet);
    if TDbDialogTemplate(Editor).DataSource.DataSet <> TRDbCustomEditor(Sender).DataSet then
      TDbDialogTemplate(Editor).DataSource.DataSet := TRDbCustomEditor(Sender).DataSet;
  end;
end;

{ == Контроль доступности команд =============================================== }
function TDbTreeQueryTemplate.TreeGroupInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanInserted(Node, ntGroup, True);
end;

function TDbTreeQueryTemplate.TreeSubGroupInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanInserted(Node, ntGroup, True);
end;

function TDbTreeQueryTemplate.TreeItemInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanInserted(Node, ntItem, True);
end;

function TDbTreeQueryTemplate.TreeSubitemInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanInserted(Node, ntSubitem, True);
end;

function TDbTreeQueryTemplate.TreeNodeCopyEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanCopied(Node, True);
end;

function TDbTreeQueryTemplate.TreeNodeOpenEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanOpened(Node, True);
end;

function TDbTreeQueryTemplate.TreeNodeMoveEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanMoved(Node, True);
end;

function TDbTreeQueryTemplate.TreeNodeEditEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanEdited(Node, True);
end;

function TDbTreeQueryTemplate.TreeNodeDeleteEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanDeleted(Node, True);
end;

{ == Редактирование дерева ===================================================== }
procedure TDbTreeQueryTemplate.TreeNodeInsertGroup(Node: TTreeNode);
var
  ParentNode: TTreeNode;
begin
  case TreeView.GetNodeType(Node) of
    ntRoot:    ParentNode := Node;
    ntGroup:   ParentNode := Node.Parent;
    ntItem:    ParentNode := Node.Parent.Parent;
    ntSubitem: ParentNode := Node.Parent.Parent.Parent;
    else       ParentNode := nil;
  end;
  if ParentNode = nil then ParentNode := TreeView.FindNode([ntRoot], intDisable);
  TreeLoader.InsertNode(ParentNode, ntGroup);
end;

procedure TDbTreeQueryTemplate.TreeNodeInsertSubGroup(Node: TTreeNode);
var
  ParentNode: TTreeNode;
begin
  case TreeView.GetNodeType(Node) of
    ntRoot:    ParentNode := Node;
    ntGroup:   ParentNode := Node;
    ntItem:    ParentNode := Node.Parent;
    ntSubitem: ParentNode := Node.Parent.Parent;
    else       ParentNode := nil;
  end;
  if ParentNode = nil then ParentNode := TreeView.FindNode([ntRoot], intDisable);
  TreeLoader.InsertNode(ParentNode, ntGroup);
end;

procedure TDbTreeQueryTemplate.TreeNodeInsertItem(Node: TTreeNode);
begin
  case TreeView.GetNodeType(Node) of
    ntGroup:   TreeLoader.InsertNode(Node, ntItem);
    ntItem:    TreeLoader.InsertNode(Node.Parent, ntItem);
    ntSubitem: TreeLoader.InsertNode(Node.Parent.Parent, ntItem);
  end;
end;

procedure TDbTreeQueryTemplate.TreeNodeInsertSubitem(Node: TTreeNode);
begin
  case TreeView.GetNodeType(Node) of
    ntItem:    TreeLoader.InsertNode(Node, ntSubitem);
    ntSubitem: TreeLoader.InsertNode(Node.Parent, ntSubitem);
  end;
end;

procedure TDbTreeQueryTemplate.TreeNodeCopy(Node: TTreeNode);
begin
  TreeLoader.CopyNode(Node);
end;

procedure TDbTreeQueryTemplate.TreeNodeEdit(Node: TTreeNode);
begin
  TreeLoader.EditNode(Node, TreeLoader.NodeCanEdited(Node, True));
end;

function TDbTreeQueryTemplate.TreeNodeSelectTargetNode(MovedNode: TTreeNode): TTreeNode;
begin
  Result := MoveToTreeNode(TreeView, False, [ntRoot, ntGroup], [ntRoot, ntGroup]);
end;

function TDbTreeQueryTemplate.TreeNodeMove(MovedNode, TargetNode: TTreeNode): Boolean;
begin
  Result := TreeLoader.MoveNode(MovedNode, TargetNode);
end;

procedure TDbTreeQueryTemplate.GroupsEditorBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  Complete := Complete and
    rAttachs_DeleteAttachments(BaseData.acDb, GroupsEditor);
{$ENDIF}
end;

procedure TDbTreeQueryTemplate.ItemsEditorBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  Complete := Complete and
    rAttachs_DeleteAttachments(BaseData.acDb, ItemsEditor);
{$ENDIF}
end;

procedure TDbTreeQueryTemplate.SubitemsEditorBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  Complete := Complete and
    rAttachs_DeleteAttachments(BaseData.acDb, ItemsEditor);
{$ENDIF}
end;

procedure TDbTreeQueryTemplate.TreeNodeDelete(Node: TTreeNode);
begin
  TreeLoader.DeleteNode(TreeView.Selected, True);
end;

{ == Редактирование данных ===================================================== }
function TDbTreeQueryTemplate.DetailSelectTargetNode(MovedNode: TTreeNode): TTreeNode;
begin
  if TreeLoader.SubitemsIsLoaded
  then Result := MoveToTreeNode(TreeView, True, [ntRoot, ntGroup, ntItem, ntSubitem], [ntItem, ntSubitem])
  else begin
    if TreeLoader.ItemsIsLoaded
    then Result := MoveToTreeNode(TreeView, True, [ntRoot, ntGroup, ntItem], [ntItem])
    else Result := MoveToTreeNode(TreeView, True, [ntRoot, ntGroup], [ntGroup]);
  end;
end;

procedure TDbTreeQueryTemplate.DetailFindOwnerNode;
var
  OwnerId: Integer;
  OwnerNode: TTreeNode;
begin
  OwnerId := RDbEditor.GetOwnerValue;
  if OwnerId > intDisable then
  begin
    if TreeLoader.SubitemsIsLoaded
    then OwnerNode := TreeView.FindNode([ntSubitem], OwnerId)
    else begin
      if TreeLoader.ItemsIsLoaded
      then OwnerNode := TreeView.FindNode([ntItem], OwnerId)
      else OwnerNode := TreeView.FindNode([ntRoot, ntGroup], OwnerId);
    end;
    // 2012-05-25 fixed bug: ошибка поиска элемента дерева, если запись дерева сделана в редакторе
    if not Assigned(OwnerNode) and ReloadTree then
    begin
      if TreeLoader.SubitemsIsLoaded
      then OwnerNode := TreeView.FindNode([ntSubitem], OwnerId)
      else begin
        if TreeLoader.ItemsIsLoaded
        then OwnerNode := TreeView.FindNode([ntItem], OwnerId)
        else OwnerNode := TreeView.FindNode([ntRoot, ntGroup], OwnerId);
      end;
    end;
    // 2012-05-25 fixed bug: конец
    if Assigned(OwnerNode) then
    begin
      if (TreeView.Selected <> nil)
      and not TreeView.CheckNodeParent(OwnerNode, TreeView.Selected)
      then TreeView.Selected := OwnerNode;
    end
    else raise Exception.CreateFmt(SErrIdNotFound, [OwnerId]);
  end;
end;

{ == Прикрепленные файлы ======================================================= }
procedure TDbTreeQueryTemplate.TreeAttachmentsUpdate(Sender: TObject);
begin
  if IsNotWait and TreeAttachments.Visible
  {$IFDEF RSS} and (Tag > 0) {$ENDIF}
  and TreePanel.Visible and Assigned(TreeView.Selected) then
  begin
    case TreeView.GetNodeType(TreeView.Selected) of
      ntGroup:   TreeAttachments.Enabled := GroupsEditor.KeyFieldIsPresent;
      ntItem:    TreeAttachments.Enabled := ItemsEditor.KeyFieldIsPresent;
      ntSubitem: TreeAttachments.Enabled := SubitemsEditor.KeyFieldIsPresent;
    end;
  end
  else TreeAttachments.Enabled := False;
end;

procedure TDbTreeQueryTemplate.TreeAttachmentsExecute(Sender: TObject);
begin
{$IFDEF ATTACH}
  case TreeView.GetNodeType(TreeView.Selected) of
    ntGroup:   rAttachs_EditAttachments(BaseData.acDb, GroupsEditor, Tag,
      TreeLoader.NodeCanOpened(TreeView.Selected, True));
    ntItem:    rAttachs_EditAttachments(BaseData.acDb, ItemsEditor, Tag,
      TreeLoader.NodeCanOpened(TreeView.Selected, True));
    ntSubitem: rAttachs_EditAttachments(BaseData.acDb, SubitemsEditor, Tag,
      TreeLoader.NodeCanOpened(TreeView.Selected, True));
  end;
{$ENDIF}
end;

end.
