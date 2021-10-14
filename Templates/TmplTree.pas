unit TmplTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplData, Menus, ActnList, ImgList, ComCtrls, ToolWin, RVclUtils,
  RavTreeView, StdCtrls, Buttons, ExtCtrls;

type
  TTreeTemplate = class(TDataTemplate)
    TreeView: TRTreeView;
    NewPopupMenu: TPopupMenu;
    NewToolButton: TToolButton;
    NewGroup: TAction;
    NewSubGroup: TAction;
    NewItem: TAction;
    Properties: TAction;
    DeleteItem: TAction;
    itemNewGroup: TMenuItem;
    itemNewSubGroup: TMenuItem;
    itemNewItem: TMenuItem;
    divEditNew: TMenuItem;
    divEditEdit: TMenuItem;
    itemProperties: TMenuItem;
    itemDeleteItem: TMenuItem;
    itemNewGroupP: TMenuItem;
    itemNewSubGroupP: TMenuItem;
    itemNewItemP: TMenuItem;
    itemPropertiesP: TMenuItem;
    itemDeleteItemP: TMenuItem;
    itemNewGroupN: TMenuItem;
    itemNewSubGroupN: TMenuItem;
    itemNewItemN: TMenuItem;
    PropertiesToolButton: TToolButton;
    DeleteItemToolButton: TToolButton;
    SeparatorEdit: TToolButton;
    RefreshToolButton: TToolButton;
    SeparatorEnd: TToolButton;
    CloseSelectToolButton: TToolButton;
    CloseCancelToolButton: TToolButton;
    FindItemToolButton: TToolButton;
    SeparatorData: TToolButton;
    SortName: TAction;
    SortId: TAction;
    divDataFind: TMenuItem;
    menuSortTree: TMenuItem;
    itemSortId: TMenuItem;
    itemSortName: TMenuItem;
    MoveItem: TAction;
    itemMoveItem: TMenuItem;
    itemMoveItemP: TMenuItem;
    divPopupMove: TMenuItem;
    divEditRefresh: TMenuItem;
    OpersToolButton: TToolButton;
    SeparatorRefresh: TToolButton;
    DataToolButton: TToolButton;
    ReportsToolButton: TToolButton;
    menuSortTreeD: TMenuItem;
    itemSortNameD: TMenuItem;
    itemSortIdD: TMenuItem;
    CollapseAll: TAction;
    ExpandAll: TAction;
    divDataDExp: TMenuItem;
    itemExpandAllD: TMenuItem;
    itemCollapseAllD: TMenuItem;
    itemExpandAll: TMenuItem;
    itemCollapseAll: TMenuItem;
    EM_None: TAction;
    EM_Root: TAction;
    EM_Groups: TAction;
    EM_All: TAction;
    SelectSaveEM: TAction;
    menuExpandMode: TMenuItem;
    itemSelectSaveEM: TMenuItem;
    divEM: TMenuItem;
    itemEM_All: TMenuItem;
    itemEM_None: TMenuItem;
    itemEM_Root: TMenuItem;
    itemEM_Groups: TMenuItem;
    menuExpandDataD: TMenuItem;
    itemEM_AllD: TMenuItem;
    itemEM_GroupsD: TMenuItem;
    itemEM_RootD: TMenuItem;
    itemEM_NoneD: TMenuItem;
    divEMD: TMenuItem;
    itemSelectSaveEMD: TMenuItem;
    SortTypeId: TAction;
    SortTypeName: TAction;
    SortNone: TAction;
    itemSortNone: TMenuItem;
    itemSotyTypeName: TMenuItem;
    itemSortTypeId: TMenuItem;
    itemSotyTypeNameD: TMenuItem;
    itemSortTypeIdD: TMenuItem;
    itemSortNoneD: TMenuItem;
    FindPanel: TPanel;
    edFastFind: TEdit;
    btnFastFind: TBitBtn;
    FindFast: TAction;
    SortIndexSort: TAction;
    itemSortIndexSort: TMenuItem;
    itemSortIndexSortD: TMenuItem;
    menuSortP: TMenuItem;
    itemSortIndexSortP: TMenuItem;
    itemSortTypeNameP: TMenuItem;
    itemSortNameP: TMenuItem;
    itemSortTypeIdP: TMenuItem;
    itemSortIdP: TMenuItem;
    itemSortNoneP: TMenuItem;
    meniLoadModeP: TMenuItem;
    itemEM_AllP: TMenuItem;
    itemEM_GroupsP: TMenuItem;
    itemEM_RootP: TMenuItem;
    itemEM_NoneP: TMenuItem;
    N13: TMenuItem;
    itemSelectSaveEMP: TMenuItem;
    itemExpandAllP: TMenuItem;
    itemCollapseAllP: TMenuItem;
    N15: TMenuItem;
    procedure SortIdUpdate(Sender: TObject);
    procedure SortIdExecute(Sender: TObject);
    procedure SortNameUpdate(Sender: TObject);
    procedure SortNameExecute(Sender: TObject);
    procedure FindUpdate(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure ExpandAllUpdate(Sender: TObject);
    procedure ExpandAllExecute(Sender: TObject);
    procedure CollapseAllUpdate(Sender: TObject);
    procedure CollapseAllExecute(Sender: TObject);
    procedure MoveItemUpdate(Sender: TObject);
    procedure MoveItemExecute(Sender: TObject);
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
    procedure SortNoneUpdate(Sender: TObject);
    procedure SortNoneExecute(Sender: TObject);
    procedure SortTypeNameUpdate(Sender: TObject);
    procedure SortTypeNameExecute(Sender: TObject);
    procedure SortTypeIdUpdate(Sender: TObject);
    procedure SortTypeIdExecute(Sender: TObject);
    procedure FindPanelResize(Sender: TObject);
    procedure FindFastUpdate(Sender: TObject);
    procedure FindFastExecute(Sender: TObject);
    procedure edFastFindKeyPress(Sender: TObject; var Key: Char);
    procedure edFastFindEnter(Sender: TObject);
    procedure edFastFindExit(Sender: TObject);
    procedure SortIndexSortUpdate(Sender: TObject);
    procedure SortIndexSortExecute(Sender: TObject);
  protected
    procedure InitFormVariables; override;
    function  LoadDataForm: Boolean; override;
    function  LoadDataTreeBegin: Boolean; virtual;
    function  LoadDataTree: Boolean; virtual; abstract;
    function  LoadDataTreeEnd: Boolean; virtual;
    function  TreeNodeMoveEnabled(Node: TTreeNode): Boolean; dynamic;
    function  TreeNodeSelectTargetNode(MovedNode: TTreeNode): TTreeNode; virtual; abstract;
    function  TreeNodeMove(MovedNode, TargetNode: TTreeNode): Boolean; virtual; abstract;
    function  SelectEnabled: Boolean; override;
    procedure SetSelection(const SelectedID: Integer); override;
    procedure SetSelections(const GroupID, SelectedID: Integer); override;
    function  GetSelection: Integer; override;
    function  GetSelections(out GroupID, SelectedID: Integer): Boolean; override;
  public
    SaveExpandMode: Boolean;
    SelectedNodes: TNodeTypes;
    {$IFDEF STYLES}
    procedure SetStyle; override;
    {$ENDIF}
    procedure ShowItemsCount; override;
    procedure LoadFormControls; override;
    procedure SaveFormControls; override;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF STYLES} RAppStyles, RFonts, {$ENDIF}
  RExHandlers, RFrmStorage, RMsgRu, RProgress, RFind, RDialogs;

{$IFDEF STYLES}
{ == Установка стиля формы ===================================================== }
procedure TTreeTemplate.SetStyle;
begin
  inherited;
  TreeView.Color := ApplicationStyle.DataForm.TreeColor;
  FontDataToFont(ApplicationStyle.DataForm.TreeFont, TreeView.Font);
  FindPanel.Visible := ApplicationStyle.DataForm.FastFindPanel;
end;
{$ENDIF}

{ == Инициализация формы до загрузки данных ==================================== }
procedure TTreeTemplate.InitFormVariables;
begin
  inherited;

  SaveExpandMode := False;
  SelectedNodes := DefaultSelectedNodes;

  FindPanel.Height := edFastFind.Height + 10;
end;

procedure TTreeTemplate.FindPanelResize(Sender: TObject);
begin
  edFastFind.Width := FindPanel.ClientWidth - btnFastFind.Width - 10;

  btnFastFind.Left := FindPanel.ClientWidth - btnFastFind.Width - 4;
  btnFastFind.Height := edFastFind.Height;
end;

{ == Сохранение и восстановление размеров формы ================================ }
procedure TTreeTemplate.LoadFormControls;
begin
  inherited;
  LoadTreeViewParams(Self, TreeView, SaveExpandMode);
end;

procedure TTreeTemplate.SaveFormControls;
begin
  inherited;
  SaveTreeViewParams(Self, TreeView, SaveExpandMode);
end;

{ == Выбрать запись и закрыть окно ============================================= }
function TTreeTemplate.SelectEnabled: Boolean;
begin
  Result := Assigned(TreeView.Selected)
    and (TreeView.GetNodeType(TreeView.Selected) in SelectedNodes);
end;

{ == Ввод и выборка выбранного значения из формы =============================== }
procedure TTreeTemplate.SetSelection(const SelectedID: Integer);
var
  i: TNodeType;
begin
  for i := High(TNodeType) downto Low(TNodeType) do
  begin
    TreeView.Selected := TreeView.FindNode([i], SelectedID);
    if TreeView.Selected <> nil then Break;
  end;
end;

procedure TTreeTemplate.SetSelections(const GroupID, SelectedID: Integer);
var
  OwnerNode, ChildNode: TTreeNode;
begin
  OwnerNode := TreeView.FindNode([ntGroup], GroupID);
  if Assigned(OwnerNode)
  then ChildNode := TreeView.FindSubNode(OwnerNode, [ntItem], SelectedID)
  else ChildNode := TreeView.FindNode([ntItem], SelectedID);
  if Assigned(ChildNode)
  then TreeView.Selected := ChildNode
  else if Assigned(OwnerNode)
       then TreeView.Selected := OwnerNode
       else TreeView.Selected := TreeView.TopItem;
end;

function TTreeTemplate.GetSelection: Integer;
begin
  Result := TreeView.GetNodeId(TreeView.Selected);
end;

function TTreeTemplate.GetSelections(out GroupID, SelectedID: Integer): Boolean;
begin
  GroupID := intDisable;
  SelectedID := intDisable;
  Result := Assigned(TreeView.Selected);
  case TreeView.GetNodeType(TreeView.Selected) of
    ntGroup:
      GroupID := TreeView.GetNodeId(TreeView.Selected);
    ntItem:
    begin
      GroupID := TreeView.GetNodeId(TreeView.Selected.Parent);
      SelectedID := TreeView.GetNodeId(TreeView.Selected);
    end;
  end;
end;

{ == Загрузка данных =========================================================== }
function TTreeTemplate.LoadDataTreeBegin: Boolean;
begin
  Result := True;
end;

function TTreeTemplate.LoadDataForm: Boolean;
var
  CurrentKey: RNodeData;
begin
  Result := False;
  try
    if LoadDataTreeBegin then
    begin
      try
        TreeView.OnChangeEvent_Block;
        TreeView.Items.BeginUpdate;
        try
          CurrentKey := TreeView.GetNodeKey(TreeView.Selected);
          TreeView.Items.Clear;
          Result := LoadDataTree;
          TreeView.Sort;
          TreeView.AutoExpand;
          TreeView.GotoNode([CurrentKey.NodeType], CurrentKey.RecordId, gtSelectTopNode);
        finally
          TreeView.Items.EndUpdate;
          TreeView.OnChangeEvent_Unblock;
        end;
      finally
        LoadDataTreeEnd;
      end;
      Application.ProcessMessages;
      ShowItemsCount;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, SErrLoadTree);
    end;
  end;
end;

function TTreeTemplate.LoadDataTreeEnd: Boolean;
begin
  Result := True;
end;

{ == Отображение числа элементов строке статуса ================================ }
procedure TTreeTemplate.ShowItemsCount;
begin
  StatusBar.Panels[0].Text := Format(SItemsCount, [TreeView.Items.Count]);
  case TreeView.SortType of
    stNone:       StatusBar.Panels[1].Text := SSortTreeNone;
    stTypeId:     StatusBar.Panels[1].Text := SSortTreeTypeId;
    stTypeName:   StatusBar.Panels[1].Text := SSortTreeTypeName;
    stRecordId:   StatusBar.Panels[1].Text := SSortTreeId;
    stRecordName: StatusBar.Panels[1].Text := SSortTreeName;
  end;
end;

{ == Сортировка ================================================================ }
procedure TTreeTemplate.SortNoneUpdate(Sender: TObject);
begin
  SortNone.Enabled := IsNotWait;
  SortNone.Checked := TreeView.SortType = stNone;
end;

procedure TTreeTemplate.SortNoneExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stNone;
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeTemplate.SortIdUpdate(Sender: TObject);
begin
  SortId.Enabled := IsNotWait;
  SortId.Checked := TreeView.SortType = stRecordId;
end;

procedure TTreeTemplate.SortIdExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stRecordId;
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeTemplate.SortTypeIdUpdate(Sender: TObject);
begin
  SortTypeId.Enabled := IsNotWait;
  SortTypeId.Checked := TreeView.SortType = stTypeId;
end;

procedure TTreeTemplate.SortTypeIdExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stTypeId;
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeTemplate.SortNameUpdate(Sender: TObject);
begin
  SortName.Enabled := IsNotWait;
  SortName.Checked := TreeView.SortType = stRecordName;
end;

procedure TTreeTemplate.SortNameExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stRecordName;
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeTemplate.SortTypeNameUpdate(Sender: TObject);
begin
  SortTypeName.Enabled := IsNotWait;
  SortTypeName.Checked := TreeView.SortType = stTypeName;
end;

procedure TTreeTemplate.SortTypeNameExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stTypeName;
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeTemplate.SortIndexSortUpdate(Sender: TObject);
begin
  SortIndexSort.Enabled := IsNotWait;
  SortIndexSort.Checked := TreeView.SortType = stRecordSort;
end;

procedure TTreeTemplate.SortIndexSortExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSortData);
  try
    TreeView.SortType := stRecordSort;
    ShowItemsCount;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Развернуть все вложенные элементы ========================================= }
procedure TTreeTemplate.ExpandAllUpdate(Sender: TObject);
begin
  ExpandAll.Enabled := IsNotWait and (TreeView.Items.Count > 0);
end;

procedure TTreeTemplate.ExpandAllExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    TreeView.FullExpand;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TTreeTemplate.CollapseAllUpdate(Sender: TObject);
begin
  CollapseAll.Enabled := IsNotWait and (TreeView.Items.Count > 0);
end;

procedure TTreeTemplate.CollapseAllExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgWorkingWait);
  try
    TreeView.FullCollapse;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Поиск строки ============================================================== }
procedure TTreeTemplate.FindUpdate(Sender: TObject);
begin
  Find.Enabled := IsNotWait and (TreeView.Items.Count > 0);
end;

procedure TTreeTemplate.FindExecute(Sender: TObject);
begin
  FindInTree(TreeView);
end;

{ == Быстрый поиск ============================================================= }
procedure TTreeTemplate.FindFastUpdate(Sender: TObject);
begin
  FindFast.Enabled := IsNotWait and (TreeView.Items.Count > 0) and (edFastFind.Text <> '');
end;

procedure TTreeTemplate.FindFastExecute(Sender: TObject);
begin
  TreeView.Selected := TreeView.FindNodePart(edFastFind.Text);

  if Assigned(TreeView.Selected)
  then TreeView.SetFocus
  else ErrorBox(Format(SErrStrNotFound, [edFastFind.Text]));
end;

procedure TTreeTemplate.edFastFindKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    if (TreeView.Items.Count > 0) and (edFastFind.Text <> '') then
      FindFastExecute(Sender);
  end;

  if Key = #27 then
  begin
    Key := #0;
    edFastFind.Clear;
  end;
end;

procedure TTreeTemplate.edFastFindEnter(Sender: TObject);
begin
  CloseSelect.ShortCut := 0;
end;

procedure TTreeTemplate.edFastFindExit(Sender: TObject);
begin
  CloseSelect.ShortCut := 13;
end;

{ == Перемещение выделенного элемента ========================================== }
function TTreeTemplate.TreeNodeMoveEnabled(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

procedure TTreeTemplate.MoveItemUpdate(Sender: TObject);
begin
  MoveItem.Enabled := IsNotWait and Assigned(TreeView.Selected)
    and TreeNodeMoveEnabled(TreeView.Selected);
end;

procedure TTreeTemplate.MoveItemExecute(Sender: TObject);
var
  TargetNode: TTreeNode;
begin
  TargetNode := TreeNodeSelectTargetNode(TreeView.Selected);
  if Assigned(TargetNode) then TreeNodeMove(TreeView.Selected, TargetNode);
end;

procedure TTreeTemplate.TreeViewDblClick(Sender: TObject);
begin
  if Assigned(TreeView.Selected) and not TreeView.Selected.HasChildren
  and Properties.Enabled then Properties.OnExecute(Sender);
end;

{ == Запоминать режим загрузки ================================================= }
procedure TTreeTemplate.SelectSaveEMUpdate(Sender: TObject);
begin
  SelectSaveEM.Enabled := IsNotWait;
  SelectSaveEM.Checked := SaveExpandMode;
end;

procedure TTreeTemplate.SelectSaveEMExecute(Sender: TObject);
begin
  SaveExpandMode := not SaveExpandMode;
end;

procedure TTreeTemplate.EM_NoneUpdate(Sender: TObject);
begin
  EM_None.Enabled := IsNotWait;
  EM_None.Checked := TreeView.AutoExpandMode = emNone;
end;

procedure TTreeTemplate.EM_NoneExecute(Sender: TObject);
begin
  TreeView.AutoExpandMode := emNone;
end;

procedure TTreeTemplate.EM_RootUpdate(Sender: TObject);
begin
  EM_Root.Enabled := IsNotWait;
  EM_Root.Checked := TreeView.AutoExpandMode = emRoot;
end;

procedure TTreeTemplate.EM_RootExecute(Sender: TObject);
begin
  TreeView.AutoExpandMode := emRoot;
end;

procedure TTreeTemplate.EM_GroupsUpdate(Sender: TObject);
begin
  EM_Groups.Enabled := IsNotWait;
  EM_Groups.Checked := TreeView.AutoExpandMode = emGroups;
end;

procedure TTreeTemplate.EM_GroupsExecute(Sender: TObject);
begin
  TreeView.AutoExpandMode := emGroups;
end;

procedure TTreeTemplate.EM_AllUpdate(Sender: TObject);
begin
  EM_All.Enabled := IsNotWait;
  EM_All.Checked := TreeView.AutoExpandMode = emAll;
end;

procedure TTreeTemplate.EM_AllExecute(Sender: TObject);
begin
  TreeView.AutoExpandMode := emAll;
end;

end.
