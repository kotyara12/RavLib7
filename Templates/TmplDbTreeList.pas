unit TmplDbTreeList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplTreeList, Menus, ActnList, ComCtrls, RavListView,
  RavTreeView, Buttons, ExtCtrls, ToolWin, Db, RDbData, RDbTree, RDbEditor,
  StdCtrls;

type
  TDbTreeListTemplate = class(TTreeListTemplate)
    TreeLoader: TRDbTreeLoader;
    GroupsEditor: TRDbTreeEditor;
    ItemsEditor: TRDbTreeEditor;
    TreeAttachments: TAction;
    itemTreeAttachmentsP: TMenuItem;
    divTreeAttachP: TMenuItem;
    procedure GetRecordId(Sender: TObject; var Value: Integer);
    procedure FreeRecordId(Sender: TObject; var Value: Integer);
    procedure GroupsBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure ItemsBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure SaveToLog(Sender: TObject; const EditTag: Integer;
      const Text: String);
    procedure TreeAttachmentsUpdate(Sender: TObject);
    procedure TreeAttachmentsExecute(Sender: TObject);
    procedure TreeAfterProcess(Sender: TObject; const Mode: TEditMode;
      const EditTag: Integer; var Complete: Boolean);
    procedure GroupsEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure ItemsEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
  protected
    procedure InitDataComponents; override;
    // Загрузка данных
    function  LoadDataTreeBegin: Boolean; override;
    function  LoadDataTree: Boolean; override;
    function  LoadDataTreeEnd: Boolean; override;
    procedure CloseDataSets; override;
    // Редактирование дерева
    function  TreeGroupInsertEnabled(Node: TTreeNode): Boolean; override;
    function  TreeSubGroupInsertEnabled(Node: TTreeNode): Boolean; override;
    function  TreeItemInsertEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeOpenEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeCopyEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeMoveEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeEditEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeDeleteEnabled(Node: TTreeNode): Boolean; override;
    procedure TreeNodeInsertGroup(Node: TTreeNode); override;
    procedure TreeNodeInsertSubGroup(Node: TTreeNode); override;
    procedure TreeNodeInsertItem(Node: TTreeNode); override;
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
  RVclUtils, RMsgRu, RTreeMove, RDbState, BaseDbUnit, TmplDbDialog;

procedure TDbTreeListTemplate.InitDataComponents;
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
function TDbTreeListTemplate.LoadDataTreeBegin: Boolean;
begin
  Result := TreeLoader.BeforeLoadTree;
end;

function TDbTreeListTemplate.LoadDataTree: Boolean;
begin
  Result := TreeLoader.LoadTree;
end;

function TDbTreeListTemplate.LoadDataTreeEnd: Boolean;
begin
  Result := TreeLoader.AfterLoadTree;
end;

procedure TDbTreeListTemplate.CloseDataSets;
begin
  inherited;
  TreeLoader.CloseDataSets;
end;

{ == Обработка событий ========================================================= }
procedure TDbTreeListTemplate.GetRecordId(Sender: TObject; var Value: Integer);
begin
  Value := BaseData.GetNewId(TRDbCustomEditor(Sender).GetObjectName(etView));
end;

procedure TDbTreeListTemplate.FreeRecordId(Sender: TObject; var Value: Integer);
begin
  BaseData.FreeId(TRDbCustomEditor(Sender).GetObjectName(etView), Value);
end;

procedure TDbTreeListTemplate.GroupsBeforeShowEditor(Sender: TObject;
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

procedure TDbTreeListTemplate.ItemsBeforeShowEditor(Sender: TObject;
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

procedure TDbTreeListTemplate.SaveToLog(Sender: TObject;
  const EditTag: Integer; const Text: String);
begin
  {$IFDEF RSS}
  AddToDbLog(EditTag, Text);
  {$ENDIF}
end;

procedure TDbTreeListTemplate.TreeAfterProcess(Sender: TObject;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  ShowItemsCount;
end;

{ == Контроль доступности команд =============================================== }
function TDbTreeListTemplate.TreeGroupInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanInserted(Node, ntGroup, True);
end;

function TDbTreeListTemplate.TreeSubGroupInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanInserted(Node, ntGroup, True);
end;

function TDbTreeListTemplate.TreeItemInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanInserted(Node, ntItem, True);
end;

function TDbTreeListTemplate.TreeNodeOpenEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanOpened(Node, True);
end;

function TDbTreeListTemplate.TreeNodeCopyEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanCopied(Node, True);
end;

function TDbTreeListTemplate.TreeNodeMoveEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanMoved(Node, True);
end;

function TDbTreeListTemplate.TreeNodeEditEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanEdited(Node, True);
end;

function TDbTreeListTemplate.TreeNodeDeleteEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanDeleted(Node, True);
end;

{ == Редактирование дерева ===================================================== }
procedure TDbTreeListTemplate.TreeNodeInsertGroup(Node: TTreeNode);
var
  ParentNode: TTreeNode;
begin
  case TreeView.GetNodeType(Node) of
    ntRoot:  ParentNode := Node;
    ntGroup: ParentNode := Node.Parent;
    ntItem:  ParentNode := Node.Parent.Parent;
    else     ParentNode := nil;
  end;
  if ParentNode = nil then ParentNode := TreeView.FindNode([ntRoot], intDisable);
  TreeLoader.InsertNode(ParentNode, ntGroup);
end;

procedure TDbTreeListTemplate.TreeNodeInsertSubGroup(Node: TTreeNode);
var
  ParentNode: TTreeNode;
begin
  case TreeView.GetNodeType(Node) of
    ntRoot:  ParentNode := Node;
    ntGroup: ParentNode := Node;
    ntItem:  ParentNode := Node.Parent;
    else     ParentNode := nil;
  end;
  if ParentNode = nil then ParentNode := TreeView.FindNode([ntRoot], intDisable);
  TreeLoader.InsertNode(ParentNode, ntGroup);
end;

procedure TDbTreeListTemplate.TreeNodeInsertItem(Node: TTreeNode);
begin
  case TreeView.GetNodeType(Node) of
    ntGroup: TreeLoader.InsertNode(Node, ntItem);
    ntItem:  TreeLoader.InsertNode(Node.Parent, ntItem);
  end;
end;

procedure TDbTreeListTemplate.TreeNodeCopy(Node: TTreeNode);
begin
  TreeLoader.CopyNode(Node);
end;

procedure TDbTreeListTemplate.TreeNodeEdit(Node: TTreeNode);
begin
  TreeLoader.EditNode(Node, TreeLoader.NodeCanEdited(Node, True));
end;

function TDbTreeListTemplate.TreeNodeSelectTargetNode(MovedNode: TTreeNode): TTreeNode;
begin
  Result := MoveToTreeNode(TreeView, False, [ntRoot, ntGroup], [ntRoot, ntGroup]);
end;

function TDbTreeListTemplate.TreeNodeMove(MovedNode, TargetNode: TTreeNode): Boolean;
begin
  Result := TreeLoader.MoveNode(MovedNode, TargetNode);
end;

procedure TDbTreeListTemplate.GroupsEditorBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  Complete := Complete and
    rAttachs_DeleteAttachments(BaseData.acDb, GroupsEditor);
{$ENDIF}
end;

procedure TDbTreeListTemplate.ItemsEditorBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  Complete := Complete and
    rAttachs_DeleteAttachments(BaseData.acDb, ItemsEditor);
{$ENDIF}
end;

procedure TDbTreeListTemplate.TreeNodeDelete(Node: TTreeNode);
begin
  TreeLoader.DeleteNode(TreeView.Selected, True);
end;

{ == Редактирование данных ===================================================== }
function TDbTreeListTemplate.DetailSelectTargetNode(MovedNode: TTreeNode): TTreeNode;
begin
  if TreeLoader.ItemsIsLoaded
  then Result := MoveToTreeNode(TreeView, True, [ntGroup, ntItem], [ntItem])
  else Result := MoveToTreeNode(TreeView, True, [ntGroup], [ntGroup]);
end;

procedure TDbTreeListTemplate.DetailFindOwnerNode;
var
  OwnerId: Integer;
  OwnerNode: TTreeNode;
begin
  OwnerId := GetDetailOwnerId;
  if OwnerId > intDisable then
  begin
    if TreeLoader.ItemsIsLoaded
    then OwnerNode := TreeView.FindNode([ntItem], OwnerId)
    else OwnerNode := TreeView.FindNode([ntRoot, ntGroup], OwnerId);
    if OwnerNode <> nil then
    begin
      if (TreeView.Selected <> nil)
      and not TreeView.CheckNodeParent(OwnerNode, TreeView.Selected)
      then TreeView.Selected := OwnerNode;
    end
    else raise Exception.CreateFmt(SErrIdNotFound, [OwnerId]);
  end;
end;

{ == Прикрепленные файлы ======================================================= }
procedure TDbTreeListTemplate.TreeAttachmentsUpdate(Sender: TObject);
begin
  if IsNotWait and TreeAttachments.Visible and (Tag > 0)
  and TreePanel.Visible and Assigned(TreeView.Selected) then
  begin
    case TreeView.GetNodeType(TreeView.Selected) of
      ntGroup: TreeAttachments.Enabled := GroupsEditor.KeyFieldIsPresent;
      ntItem: TreeAttachments.Enabled := ItemsEditor.KeyFieldIsPresent;
    end;
  end
  else TreeAttachments.Enabled := False;
end;

procedure TDbTreeListTemplate.TreeAttachmentsExecute(Sender: TObject);
begin
{$IFDEF ATTACH}
  case TreeView.GetNodeType(TreeView.Selected) of
    ntGroup: rAttachs_EditAttachments(BaseData.acDb, GroupsEditor, Tag,
      TreeLoader.NodeCanOpened(TreeView.Selected, True));
    ntItem: rAttachs_EditAttachments(BaseData.acDb, ItemsEditor, Tag,
      TreeLoader.NodeCanOpened(TreeView.Selected, True));
  end;
{$ENDIF}
end;

end.
