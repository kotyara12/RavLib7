unit TmplTreeDetail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplData, Menus, ActnList, ImgList, ComCtrls, ToolWin, ExtCtrls,
  RavTreeView, Buttons, RVclUtils;

type
  TTreeDetailTemplate = class(TDataTemplate)
    TreePanel: TPanel;
    Splitter: TSplitter;
    DataPanel: TPanel;
    TreeHeaderPanel: TPanel;
    TreeView: TRTreeView;
    TreePopupMenu: TPopupMenu;
    TreeButtonPanel: TPanel;
    TreeCloseBtn: TSpeedButton;
    TreeBevel: TBevel;
    TreeVisible: TAction;
    itemTreeVisibleP: TMenuItem;
    itemTreeVisibleT: TMenuItem;
    divTreeData: TMenuItem;
    itemCloseCancelT: TMenuItem;
    itemFindTreeT: TMenuItem;
    divTreeFind: TMenuItem;
    TreeSort_Id: TAction;
    TreeSort_Name: TAction;
    menuSortTreeT: TMenuItem;
    divDataFind: TMenuItem;
    menuSortTree: TMenuItem;
    NewPopupMenu: TPopupMenu;
    NewGroup: TAction;
    NewSubGroup: TAction;
    NewItem: TAction;
    NewRecord: TAction;
    Properties: TAction;
    Move: TAction;
    Delete: TAction;
    itemNewGroupN: TMenuItem;
    itemNewSubGroupN: TMenuItem;
    itemNewItemN: TMenuItem;
    itemNewRecordN: TMenuItem;
    itemNewGroupT: TMenuItem;
    itemNewSubGroupT: TMenuItem;
    itemNewItemT: TMenuItem;
    itemPropertiesT: TMenuItem;
    itemDeleteT: TMenuItem;
    itemMoveT: TMenuItem;
    divTreeMove: TMenuItem;
    divTreeNew: TMenuItem;
    divTreeEdit: TMenuItem;
    divTreeRefresh: TMenuItem;
    itemRefreshT: TMenuItem;
    itemNewGroup: TMenuItem;
    itemNewSubGroup: TMenuItem;
    itemNewItem: TMenuItem;
    itemNewRecord: TMenuItem;
    divEditNew: TMenuItem;
    itemProperties: TMenuItem;
    itemDelete: TMenuItem;
    divEditEdit: TMenuItem;
    itemMove: TMenuItem;
    itemNewRecordP: TMenuItem;
    itemPropertiesP: TMenuItem;
    itemDeleteItemP: TMenuItem;
    divPopupNew: TMenuItem;
    itemMoveItemP: TMenuItem;
    divPopupMove: TMenuItem;
    divEditRefresh: TMenuItem;
    itemSortTreeD: TMenuItem;
    ExpandAll: TAction;
    CollapseAll: TAction;
    itemExpandAllT: TMenuItem;
    itemCollapseAllT: TMenuItem;
    menuDataTree: TMenuItem;
    divDataExp: TMenuItem;
    itemExpandAll: TMenuItem;
    itemCollapseAll: TMenuItem;
    divDataDExp: TMenuItem;
    itemExpandAllD: TMenuItem;
    itemCollapseAllD: TMenuItem;
    SelectSaveEM: TAction;
    menuExpandMode: TMenuItem;
    itemSelectSaveEM: TMenuItem;
    divEM: TMenuItem;
    menuExpandModeD: TMenuItem;
    divEMD: TMenuItem;
    itemSelectSaveEMD: TMenuItem;
    menuExpandModeT: TMenuItem;
    divEMT: TMenuItem;
    itemSelectSaveEMT: TMenuItem;
    EM_None: TAction;
    EM_Root: TAction;
    EM_Groups: TAction;
    EM_All: TAction;
    itemEM_None: TMenuItem;
    itemEM_Root: TMenuItem;
    itemEM_Groups: TMenuItem;
    itemEM_All: TMenuItem;
    itemEM_NoneD: TMenuItem;
    itemEM_RootD: TMenuItem;
    itemEM_GroupsD: TMenuItem;
    itemEM_AllD: TMenuItem;
    itemEM_NoneT: TMenuItem;
    itemEM_RootT: TMenuItem;
    itemEM_GroupsT: TMenuItem;
    itemEM_AllT: TMenuItem;
    SaveTreePosition: TAction;
    itemSaveTreePosition: TMenuItem;
    itemSaveTreePositionD: TMenuItem;
    itemSaveTreePositionT: TMenuItem;
    TreeSort_TypeId: TAction;
    TreeSort_TypeName: TAction;
    TreeSort_None: TAction;
    itemTreeSort_None: TMenuItem;
    itemTreeSort_Id: TMenuItem;
    itemTreeSort_TypeId: TMenuItem;
    itemTreeSort_Name: TMenuItem;
    itemTreeSort_TypeName: TMenuItem;
    itemTreeSort_NoneD: TMenuItem;
    itemTreeSort_IdD: TMenuItem;
    itemTreeSort_TypeIdD: TMenuItem;
    itemTreeSort_NameD: TMenuItem;
    itemTreeSort_TypeNameD: TMenuItem;
    itemTreeSort_NoneT: TMenuItem;
    itemTreeSort_IdT: TMenuItem;
    itemTreeSort_TypeIdT: TMenuItem;
    itemTreeSort_NameT: TMenuItem;
    itemTreeSort_TypeNameT: TMenuItem;
    divPopupPanel: TMenuItem;
    FindTree: TAction;
    itemFindTree: TMenuItem;
    TreeRootOnly: TAction;
    itemTreeRootOnlyT: TMenuItem;
    itemTreeRootOnly: TMenuItem;
    itemTreeRootOnlyD: TMenuItem;
    CopyRecord: TAction;
    itemCopyRecord: TMenuItem;
    itemCopyRecordP: TMenuItem;
    itemCopyRecordT: TMenuItem;
    TreeSort_IndexSort: TAction;
    itemTreeSort_IndexSort: TMenuItem;
    itemTreeSort_IndexSortT: TMenuItem;
    itemTreeSort_IndexSortD: TMenuItem;
    ExpandNode: TAction;
    CollapseNode: TAction;
    itemExpandNodeT: TMenuItem;
    itemCollapseNodeT: TMenuItem;
    divTreeExpand: TMenuItem;
    itemCollapseNode: TMenuItem;
    itemExpandNode: TMenuItem;
    itemExpandNodeD: TMenuItem;
    itemCollapseNodeD: TMenuItem;
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreePanelShow(Sender: TObject);
    procedure TreePanelHide(Sender: TObject);
    procedure TreeVisibleUpdate(Sender: TObject);
    procedure TreeVisibleExecute(Sender: TObject);
    procedure TreeSort_IdUpdate(Sender: TObject);
    procedure TreeSort_IdExecute(Sender: TObject);
    procedure TreeSort_NameUpdate(Sender: TObject);
    procedure TreeSort_NameExecute(Sender: TObject);
    procedure ExpandAllUpdate(Sender: TObject);
    procedure ExpandAllExecute(Sender: TObject);
    procedure CollapseAllUpdate(Sender: TObject);
    procedure CollapseAllExecute(Sender: TObject);
    procedure PropertiesUpdate(Sender: TObject);
    procedure PropertiesExecute(Sender: TObject);
    procedure DeleteUpdate(Sender: TObject);
    procedure DeleteExecute(Sender: TObject);
    procedure NewGroupUpdate(Sender: TObject);
    procedure NewGroupExecute(Sender: TObject);
    procedure NewSubGroupUpdate(Sender: TObject);
    procedure NewSubGroupExecute(Sender: TObject);
    procedure NewItemUpdate(Sender: TObject);
    procedure NewItemExecute(Sender: TObject);
    procedure MoveUpdate(Sender: TObject);
    procedure MoveExecute(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
    procedure SelectSaveEMUpdate(Sender: TObject);
    procedure SelectSaveEMExecute(Sender: TObject);
    procedure EM_NoneUpdate(Sender: TObject);
    procedure EM_NoneExecute(Sender: TObject);
    procedure EM_RootUpdate(Sender: TObject);
    procedure EM_RootExecute(Sender: TObject);
    procedure EM_GroupsUpdate(Sender: TObject);
    procedure EM_GroupsExecute(Sender: TObject);
    procedure EM_AllUpdate(Sender: TObject);
    procedure EM_AllExecute(Sender: TObject);
    procedure SaveTreePositionUpdate(Sender: TObject);
    procedure SaveTreePositionExecute(Sender: TObject);
    procedure TreeSort_NoneUpdate(Sender: TObject);
    procedure TreeSort_NoneExecute(Sender: TObject);
    procedure TreeSort_TypeNameExecute(Sender: TObject);
    procedure TreeSort_TypeNameUpdate(Sender: TObject);
    procedure TreeSort_TypeIdUpdate(Sender: TObject);
    procedure TreeSort_TypeIdExecute(Sender: TObject);
    procedure FindTreeUpdate(Sender: TObject);
    procedure FindTreeExecute(Sender: TObject);
    procedure TreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeRootOnlyUpdate(Sender: TObject);
    procedure TreeRootOnlyExecute(Sender: TObject);
    procedure CopyRecordUpdate(Sender: TObject);
    procedure CopyRecordExecute(Sender: TObject);
    procedure TreeSort_IndexSortUpdate(Sender: TObject);
    procedure TreeSort_IndexSortExecute(Sender: TObject);
    procedure ExpandNodeUpdate(Sender: TObject);
    procedure ExpandNodeExecute(Sender: TObject);
    procedure CollapseNodeUpdate(Sender: TObject);
    procedure CollapseNodeExecute(Sender: TObject);
  private
    FTreeLoaded: Boolean;
    FCurrentIndex: Integer;
    FStoredKey: RNodeData;
    function  InternalLoadTree(const ReloadData: Boolean = True): Boolean;
    function  InternalLoadData: Boolean;
  protected
    FRootOnlyEnabled: Boolean;
    procedure InitFormVariables; override;
    procedure StartForm; override;
    function  LoadDataForm: Boolean; override;
    function  LoadDataTreeBegin: Boolean; virtual;
    function  LoadDataTree: Boolean; virtual; abstract;
    function  LoadDataTreeEnd: Boolean; virtual;
    function  LoadDataNodeBegin: Boolean; virtual;
    procedure LoadDataNodeTry; virtual;
    function  LoadDataNode(const TreeActive: Boolean; Node: TTreeNode): Boolean; virtual; abstract;
    procedure LoadDataNodeFinally; virtual;
    function  LoadDataNodeEnd: Boolean; virtual;
    function  ReloadTree(const ReloadData: Boolean = True): Boolean;
    function  ReloadData: Boolean;
    procedure ChangeTreeState(const TreeActive: Boolean); virtual;
    // Редактирование дерева
    function  TreeGroupInsertEnabled(Node: TTreeNode): Boolean; virtual;
    function  TreeSubGroupInsertEnabled(Node: TTreeNode): Boolean; virtual;
    function  TreeItemInsertEnabled(Node: TTreeNode): Boolean; virtual;
    function  TreeNodeCopyEnabled(Node: TTreeNode): Boolean; virtual;
    function  TreeNodeOpenEnabled(Node: TTreeNode): Boolean; virtual;
    function  TreeNodeMoveEnabled(Node: TTreeNode): Boolean; virtual;
    function  TreeNodeEditEnabled(Node: TTreeNode): Boolean; virtual;
    function  TreeNodeDetailIsEmpty(Node: TTreeNode): Boolean; virtual;
    function  TreeNodeDeleteEnabled(Node: TTreeNode): Boolean; virtual;
    procedure TreeNodeInsertGroup(Node: TTreeNode); virtual; abstract;
    procedure TreeNodeInsertSubGroup(Node: TTreeNode); virtual; abstract;
    procedure TreeNodeInsertItem(Node: TTreeNode); virtual; abstract;
    function  TreeNodeSelectTargetNode(MovedNode: TTreeNode): TTreeNode; virtual; abstract;
    function  TreeNodeMove(MovedNode, TargetNode: TTreeNode): Boolean; virtual; abstract;
    procedure TreeNodeCopy(Node: TTreeNode); virtual; abstract;
    procedure TreeNodeEdit(Node: TTreeNode); virtual; abstract;
    procedure TreeNodeDelete(Node: TTreeNode); virtual; abstract;
    // Редактирование данных
    function  DetailInsertEnabled: Boolean; dynamic;
    function  DetailCopyEnabled: Boolean; dynamic;
    function  DetailOpenEnabled: Boolean; dynamic;
    function  DetailMoveEnabled: Boolean; dynamic;
    function  DetailEditEnabled: Boolean; dynamic;
    function  DetailDeleteEnabled: Boolean; dynamic;
    function  DetailSelectTargetNode(MovedNode: TTreeNode): TTreeNode; virtual; abstract;
    function  DetailMove(TargetNode: TTreeNode): Boolean; virtual; abstract;
    procedure DetailFindOwnerNode; virtual; abstract;
    procedure DetailCopy; virtual; abstract;
    procedure DetailEdit; virtual; abstract;
    procedure DetailDelete; virtual; abstract;
  public
    SaveExpandMode: Boolean;
    SavePosition: Boolean;
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure LoadFormControls; override;
    procedure SaveFormControls; override;
    procedure GotoTreeNode(const NodeTypes: TNodeTypes; const NodeId: Integer); virtual;
    procedure PrepareTreeReload;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RExHandlers, RFrmStorage, RMsgRu, RProgress, RFind, RDialogs;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TTreeDetailTemplate.SetStyle;
begin
  inherited;
  TreeView.Color := ApplicationStyle.DataForm.TreeColor;
  FontDataToFont(ApplicationStyle.DataForm.TreeFont, TreeView.Font);
end;
{$ENDIF}

{ == Сохранение и восстановление размеров формы ================================ }
procedure TTreeDetailTemplate.LoadFormControls;
begin
  inherited;
  LoadPanelSize(Self, TreePanel, False, True, True);
  LoadTreeViewParamsExt(Self, TreeView, SaveExpandMode, SavePosition, FStoredKey);
end;

procedure TTreeDetailTemplate.SaveFormControls;
begin
  if TreePanel.Visible then FStoredKey := TreeView.GetNodeKey(TreeView.Selected);
  inherited;
  SavePanelSize(Self, TreePanel);
  SaveTreeViewParamsExt(Self, TreeView, SaveExpandMode, SavePosition, FStoredKey);
end;

{ == Инициализация формы до загрузки данных ==================================== }
procedure TTreeDetailTemplate.InitFormVariables;
begin
  inherited;
  FRootOnlyEnabled := True;
  SaveExpandMode := True;
  SavePosition := True;
  FTreeLoaded := False;
  FCurrentIndex := intDisable;
  FStoredKey.RecordId := intDisable;
  FStoredKey.NodeType := ntRoot;
end;

procedure TTreeDetailTemplate.StartForm;
begin
  inherited;
  // 2013-03-22: fixed bug: change TreeHeaderPanel.Height on change font
  TreeHeaderPanel.Height := Canvas.TextHeight('Wg') + 4;
  TreeButtonPanel.Width := TreeHeaderPanel.Height;
  TreeCloseBtn.Height := TreeHeaderPanel.Height - 2;
  TreeCloseBtn.Width := TreeHeaderPanel.Height - 2;
end;

{ == Загрузка данных =========================================================== }
procedure TTreeDetailTemplate.PrepareTreeReload;
begin
  FTreeLoaded := False;
end;

function TTreeDetailTemplate.LoadDataForm: Boolean;
begin
  PrepareTreeReload;

  Splitter.Visible := TreePanel.Visible;
  ChangeTreeState(TreePanel.Visible);

  if TreePanel.Visible
  then Result := InternalLoadTree
  else Result := InternalLoadData;
end;

{ == Загрузка дерева =========================================================== }
function TTreeDetailTemplate.LoadDataTreeBegin: Boolean;
begin
  Result := True;
end;

function TTreeDetailTemplate.LoadDataTreeEnd: Boolean;
begin
  Result := True;
end;

function TTreeDetailTemplate.InternalLoadTree(const ReloadData: Boolean = True): Boolean;
begin
  Result := False;
  try
    FTreeLoaded := False;
    if LoadDataTreeBegin then
    begin
      try
        TreeView.OnChangeEvent_Block;
        TreeView.Items.BeginUpdate;
        TreeView.OnDeletion := nil;
        try
          TreeView.Items.Clear;
          FTreeLoaded := LoadDataTree;
          TreeView.Sort;
          TreeView.AutoExpand;
          if (FCurrentIndex > intDisable) or not SavePosition
          then TreeView.GotoNode(FCurrentIndex, gtSelectTopNode)
          else TreeView.GotoNode([FStoredKey.NodeType], FStoredKey.RecordId, gtSelectTopNode);
        finally
          TreeView.OnDeletion := TreeViewDeletion;
          TreeView.Items.EndUpdate;
          TreeView.OnChangeEvent_Unblock;
        end;
      finally
        LoadDataTreeEnd;
      end;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, SErrLoadTree);
    end;
  end;
  Application.ProcessMessages;
  if FTreeLoaded and ReloadData then
    Result := InternalLoadData;
end;

{ == Загрузка данных =========================================================== }
function TTreeDetailTemplate.LoadDataNodeBegin: Boolean;
begin
  Result := True;
end;

procedure TTreeDetailTemplate.LoadDataNodeTry;
begin
end;

procedure TTreeDetailTemplate.LoadDataNodeFinally;
begin
end;

function TTreeDetailTemplate.LoadDataNodeEnd: Boolean;
begin
  ShowItemsCount;
  Result := True;
end;

function TTreeDetailTemplate.InternalLoadData: Boolean;

  function IntLoad(const TreeActive: Boolean; Node: TTreeNode): Boolean;
  begin
    try
      Result := LoadDataNodeBegin;
      if Result then
      begin
        LoadDataNodeTry;
        try
          {$IFDEF ATTACH}
          LoadAttachments;
          {$ENDIF}
          Result := LoadDataNode(TreePanel.Visible, TreeView.Selected);
        finally
          LoadDataNodeFinally;
        end;
        if Result then LoadDataNodeEnd;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self, SErrLoadData);
      end;
    end;
  end;

begin
  if TreePanel.Visible then
  begin
    FCurrentIndex := TreeView.GetNodeIndex(TreeView.Selected);
    Result := IntLoad(TreePanel.Visible, TreeView.Selected);
  end
  else Result := IntLoad(TreePanel.Visible, nil);
end;

function TTreeDetailTemplate.ReloadTree(const ReloadData: Boolean = True): Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  Result := False;
  try
    Result := InternalLoadTree(ReloadData);
  finally
    if Result then ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function TTreeDetailTemplate.ReloadData: Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  Result := False;
  try
    Result := InternalLoadData;
  finally
    if Result then ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Перемещение по дереву ===================================================== }
procedure TTreeDetailTemplate.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  LoadRes: Boolean;
begin
  if FCurrentIndex <> TreeView.GetNodeIndex(TreeView.Selected) then
  begin
    StartWait;
    ShowInStatusBar(SMsgLoadDataWait);
    LoadRes := False;
    try
      LoadRes := InternalLoadData;
    finally
      if LoadRes then ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

{ == Переключение видимости панели с папками =================================== }
procedure TTreeDetailTemplate.ChangeTreeState(const TreeActive: Boolean);
begin
end;

procedure TTreeDetailTemplate.TreePanelShow(Sender: TObject);
var
  LoadRes: Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  LoadRes := False;
  try
    Splitter.Visible := True;
    TreePanel.Visible := True;
    ChangeTreeState(TreePanel.Visible);
    if FTreeLoaded
    then LoadRes := InternalLoadData
    else LoadRes := InternalLoadTree(True);
  finally
    if LoadRes then ShowInStatusBar(EmptyStr);
    FindTreeUpdate(Sender);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.TreePanelHide(Sender: TObject);
var
  LoadRes: Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  LoadRes := False;
  try
    TreePanel.Visible := False;
    Splitter.Visible := False;
    ChangeTreeState(TreePanel.Visible);
    LoadRes := InternalLoadData;
  finally
    if LoadRes then ShowInStatusBar(EmptyStr);
    FindTreeUpdate(Sender);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.TreeVisibleUpdate(Sender: TObject);
begin
  TreeVisible.Visible := TreeCloseBtn.Visible;
  TreeVisible.Enabled := IsNotWait and TreeCloseBtn.Enabled;
  TreeVisible.Checked := TreePanel.Visible;
end;

procedure TTreeDetailTemplate.TreeVisibleExecute(Sender: TObject);
begin
  if TreePanel.Visible
  then TreePanelHide(Sender)
  else TreePanelShow(Sender);
end;

procedure TTreeDetailTemplate.GotoTreeNode(const NodeTypes: TNodeTypes; const NodeId: Integer);
begin
  TreeView.GotoNode(NodeTypes, NodeId, gtNoChangeSelection);
end;

{ == Включая вложенные записи ================================================== }
procedure TTreeDetailTemplate.TreeRootOnlyUpdate(Sender: TObject);
begin
  TreeRootOnly.Visible := FRootOnlyEnabled;
  TreeRootOnly.Enabled := IsNotWait and TreePanel.Visible;
  TreeRootOnly.Checked := not TreeView.ListRootOnly;
end;

procedure TTreeDetailTemplate.TreeRootOnlyExecute(Sender: TObject);
begin
  StartWait;
  try
    TreeView.ListRootOnly := not TreeView.ListRootOnly;
    ReloadData;
  finally
    StopWait;
  end;
end;

{ == Поиск по дереву =========================================================== }
procedure TTreeDetailTemplate.FindTreeUpdate(Sender: TObject);
begin
  FindTree.Visible := TreePanel.Visible;
  FindTree.Enabled := IsNotWait and (TreeView.Items.Count > 0);
end;

procedure TTreeDetailTemplate.FindTreeExecute(Sender: TObject);
begin
  FindInTree(TreeView);
end;

{ == Сортировка дерева ========================================================= }
procedure TTreeDetailTemplate.TreeSort_NoneUpdate(Sender: TObject);
begin
  TreeSort_None.Enabled := IsNotWait and TreePanel.Visible;
  TreeSort_None.Checked := TreeView.SortType = stNone;
end;

procedure TTreeDetailTemplate.TreeSort_NoneExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stNone;
    FCurrentIndex := TreeView.GetNodeIndex(TreeView.Selected);
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.TreeSort_IdUpdate(Sender: TObject);
begin
  TreeSort_Id.Enabled := IsNotWait and TreePanel.Visible;
  TreeSort_Id.Checked := TreeView.SortType = stRecordId;
end;

procedure TTreeDetailTemplate.TreeSort_IdExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stRecordId;
    FCurrentIndex := TreeView.GetNodeIndex(TreeView.Selected);
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.TreeSort_TypeIdUpdate(Sender: TObject);
begin
  TreeSort_TypeId.Enabled := IsNotWait and TreePanel.Visible;
  TreeSort_TypeId.Checked := TreeView.SortType = stTypeId;
end;

procedure TTreeDetailTemplate.TreeSort_TypeIdExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stTypeId;
    FCurrentIndex := TreeView.GetNodeIndex(TreeView.Selected);
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.TreeSort_NameUpdate(Sender: TObject);
begin
  TreeSort_Name.Enabled := IsNotWait and TreePanel.Visible;
  TreeSort_Name.Checked := TreeView.SortType = stRecordName;
end;

procedure TTreeDetailTemplate.TreeSort_NameExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stRecordName;
    FCurrentIndex := TreeView.GetNodeIndex(TreeView.Selected);
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.TreeSort_TypeNameUpdate(Sender: TObject);
begin
  TreeSort_TypeName.Enabled := IsNotWait and TreePanel.Visible;
  TreeSort_TypeName.Checked := TreeView.SortType = stTypeName;
end;

procedure TTreeDetailTemplate.TreeSort_TypeNameExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stTypeName;
    FCurrentIndex := TreeView.GetNodeIndex(TreeView.Selected);
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.TreeSort_IndexSortUpdate(Sender: TObject);
begin
  TreeSort_IndexSort.Enabled := IsNotWait and TreePanel.Visible;
  TreeSort_IndexSort.Checked := TreeView.SortType = stRecordSort;
end;

procedure TTreeDetailTemplate.TreeSort_IndexSortExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stRecordSort;
    FCurrentIndex := TreeView.GetNodeIndex(TreeView.Selected);
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Развернуть все вложенные элементы ========================================= }
procedure TTreeDetailTemplate.ExpandAllUpdate(Sender: TObject);
begin
  ExpandAll.Enabled := IsNotWait and TreePanel.Visible and (TreeView.Items.Count > 0);
end;

procedure TTreeDetailTemplate.ExpandAllExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    TreeView.FullExpand;
    TreeViewChange(TreeView, TreeView.Selected);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.ExpandNodeUpdate(Sender: TObject);
begin
  ExpandNode.Enabled := IsNotWait and TreePanel.Visible and Assigned(TreeView.Selected);
end;

procedure TTreeDetailTemplate.ExpandNodeExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    TreeView.Selected.Expand(True);
    TreeViewChange(TreeView, TreeView.Selected);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Свернуть все вложенные элементы =========================================== }
procedure TTreeDetailTemplate.CollapseAllUpdate(Sender: TObject);
begin
  CollapseAll.Enabled := IsNotWait and TreePanel.Visible and (TreeView.Items.Count > 0);
end;

procedure TTreeDetailTemplate.CollapseAllExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    TreeView.FullCollapse;
    TreeViewChange(TreeView, TreeView.Selected);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeDetailTemplate.CollapseNodeUpdate(Sender: TObject);
begin
  CollapseNode.Enabled := IsNotWait and TreePanel.Visible and Assigned(TreeView.Selected);
end;

procedure TTreeDetailTemplate.CollapseNodeExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    TreeView.Selected.Collapse(True);
    TreeViewChange(TreeView, TreeView.Selected);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Редактирование дерева ===================================================== }
function TTreeDetailTemplate.TreeGroupInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.TreeSubGroupInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.TreeItemInsertEnabled(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.TreeNodeCopyEnabled(Node: TTreeNode): Boolean;
begin
  Result := True;
end;

function TTreeDetailTemplate.TreeNodeOpenEnabled(Node: TTreeNode): Boolean;
begin
  Result := True;
end;

function TTreeDetailTemplate.TreeNodeEditEnabled(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.TreeNodeMoveEnabled(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.TreeNodeDetailIsEmpty(Node: TTreeNode): Boolean;
begin
  Result := True;
end;

function TTreeDetailTemplate.TreeNodeDeleteEnabled(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

{ == Редактирование данных ===================================================== }
function TTreeDetailTemplate.DetailInsertEnabled: Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.DetailCopyEnabled: Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.DetailOpenEnabled: Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.DetailEditEnabled: Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.DetailMoveEnabled: Boolean;
begin
  Result := False;
end;

function TTreeDetailTemplate.DetailDeleteEnabled: Boolean;
begin
  Result := False;
end;

{ == Создать новую группу ====================================================== }
procedure TTreeDetailTemplate.NewGroupUpdate(Sender: TObject);
begin
  NewGroup.Enabled := IsNotWait and TreePanel.Visible
    and TreeGroupInsertEnabled(TreeView.Selected);
end;

procedure TTreeDetailTemplate.NewGroupExecute(Sender: TObject);
begin
  TreeView.OnChangeEvent_Block;
  try
    TreeNodeInsertGroup(TreeView.Selected);
  finally
    TreeView.OnChangeEvent_Unblock;
    TreeViewChange(TreeView, TreeView.Selected);
  end;
end;

{ == Создать новую подгруппу в текущей группе ================================== }
procedure TTreeDetailTemplate.NewSubGroupUpdate(Sender: TObject);
begin
  NewSubGroup.Enabled := IsNotWait and TreePanel.Visible
    and Assigned(TreeView.Selected)
    and TreeSubGroupInsertEnabled(TreeView.Selected);
end;

procedure TTreeDetailTemplate.NewSubGroupExecute(Sender: TObject);
begin
  TreeView.OnChangeEvent_Block;
  try
    TreeNodeInsertSubGroup(TreeView.Selected);
  finally
    TreeView.OnChangeEvent_Unblock;
    TreeViewChange(TreeView, TreeView.Selected);
  end;
end;

{ == Создать новый элемент ===================================================== }
procedure TTreeDetailTemplate.NewItemUpdate(Sender: TObject);
begin
  NewItem.Enabled := IsNotWait and TreePanel.Visible
    and Assigned(TreeView.Selected)
    and TreeItemInsertEnabled(TreeView.Selected);
end;

procedure TTreeDetailTemplate.NewItemExecute(Sender: TObject);
begin
  TreeView.OnChangeEvent_Block;
  try
    TreeNodeInsertItem(TreeView.Selected);
  finally
    TreeView.OnChangeEvent_Unblock;
    TreeViewChange(TreeView, TreeView.Selected);
  end;
end;

{ == Копировать элемент ======================================================== }
procedure TTreeDetailTemplate.CopyRecordUpdate(Sender: TObject);
begin
  if TreeView.Focused
  then CopyRecord.Enabled := IsNotWait and Assigned(TreeView.Selected) and TreeNodeCopyEnabled(TreeView.Selected)
  else CopyRecord.Enabled := IsNotWait and DetailCopyEnabled;
end;

procedure TTreeDetailTemplate.CopyRecordExecute(Sender: TObject);
begin
  if TreeView.Focused
  then TreeNodeCopy(TreeView.Selected)
  else DetailCopy;
end;

{ == Свойства выделенного объекта ============================================== }
procedure TTreeDetailTemplate.PropertiesUpdate(Sender: TObject);
begin
  if TreeView.Focused
  then Properties.Enabled := IsNotWait and Assigned(TreeView.Selected) and TreeNodeOpenEnabled(TreeView.Selected)
  else Properties.Enabled := IsNotWait and DetailOpenEnabled;
end;

procedure TTreeDetailTemplate.PropertiesExecute(Sender: TObject);
begin
  if TreeView.Focused
  then TreeNodeEdit(TreeView.Selected)
  else DetailEdit;
end;

procedure TTreeDetailTemplate.TreeViewDblClick(Sender: TObject);
begin
  if Assigned(TreeView.Selected) and not TreeView.Selected.HasChildren
  and Properties.Enabled then Properties.OnExecute(Sender);
end;

{ == Переместить объект в другую группу ======================================== }
procedure TTreeDetailTemplate.MoveUpdate(Sender: TObject);
begin
  if TreeView.Focused
  then Move.Enabled := IsNotWait and Assigned(TreeView.Selected) and TreeNodeMoveEnabled(TreeView.Selected)
  else Move.Enabled := IsNotWait and TreePanel.Visible and DetailMoveEnabled;
end;

procedure TTreeDetailTemplate.MoveExecute(Sender: TObject);
var
  TargetNode: TTreeNode;
begin
  if TreeView.Focused then
  begin
    TargetNode := TreeNodeSelectTargetNode(TreeView.Selected);
    if Assigned(TargetNode) then TreeNodeMove(TreeView.Selected, TargetNode);
  end
  else begin
    TargetNode := DetailSelectTargetNode(TreeView.Selected);
    if Assigned(TargetNode) and DetailMove(TargetNode) then
      TreeView.Selected := TargetNode;
  end;
end;

{ == Удалить выделенный объект ================================================= }
procedure TTreeDetailTemplate.DeleteUpdate(Sender: TObject);
begin
  if TreeView.Focused
  then Delete.Enabled := IsNotWait and Assigned(TreeView.Selected)
    and TreeNodeDetailIsEmpty(TreeView.Selected)
    and TreeNodeDeleteEnabled(TreeView.Selected)
  else Delete.Enabled := IsNotWait and DetailDeleteEnabled;
end;

procedure TTreeDetailTemplate.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
begin
  FCurrentIndex := intDisable;
end;

procedure TTreeDetailTemplate.DeleteExecute(Sender: TObject);
begin
  if TreeView.Focused
  then TreeNodeDelete(TreeView.Selected)
  else DetailDelete;
end;

{ == Сохранять режим загрузки ================================================== }
procedure TTreeDetailTemplate.SelectSaveEMUpdate(Sender: TObject);
begin
  SelectSaveEM.Enabled := IsNotWait;
  SelectSaveEM.Checked := SaveExpandMode;
end;

procedure TTreeDetailTemplate.SelectSaveEMExecute(Sender: TObject);
begin
  SaveExpandMode := not SaveExpandMode;
end;

procedure TTreeDetailTemplate.EM_NoneUpdate(Sender: TObject);
begin
  EM_None.Enabled := IsNotWait;
  EM_None.Checked := TreeView.AutoExpandMode = emNone;
end;

procedure TTreeDetailTemplate.EM_NoneExecute(Sender: TObject);
begin
  TreeView.AutoExpandMode := emNone;
end;

procedure TTreeDetailTemplate.EM_RootUpdate(Sender: TObject);
begin
  EM_Root.Enabled := IsNotWait;
  EM_Root.Checked := TreeView.AutoExpandMode = emRoot;
end;

procedure TTreeDetailTemplate.EM_RootExecute(Sender: TObject);
begin
  TreeView.AutoExpandMode := emRoot;
end;

procedure TTreeDetailTemplate.EM_GroupsUpdate(Sender: TObject);
begin
  EM_Groups.Enabled := IsNotWait;
  EM_Groups.Checked := TreeView.AutoExpandMode = emGroups;
end;

procedure TTreeDetailTemplate.EM_GroupsExecute(Sender: TObject);
begin
  TreeView.AutoExpandMode := emGroups;
end;

procedure TTreeDetailTemplate.EM_AllUpdate(Sender: TObject);
begin
  EM_All.Enabled := IsNotWait;
  EM_All.Checked := TreeView.AutoExpandMode = emAll;
end;

procedure TTreeDetailTemplate.EM_AllExecute(Sender: TObject);
begin
  TreeView.AutoExpandMode := emAll;
end;

{ == Запоминать текущую позицию ================================================ }
procedure TTreeDetailTemplate.SaveTreePositionUpdate(Sender: TObject);
begin
  SaveTreePosition.Enabled := IsNotWait;
  SaveTreePosition.Checked := SavePosition;
end;

procedure TTreeDetailTemplate.SaveTreePositionExecute(Sender: TObject);
begin
  SavePosition := not SavePosition;
end;

end.
