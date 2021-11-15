unit TmplDbTree;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplTree, Menus, ActnList, ComCtrls, RavTreeView, ToolWin, Db,
  RDbData, RDbTree, RDbEditor, StdCtrls, Buttons, ExtCtrls;

type
  TDbTreeTemplate = class(TTreeTemplate)
    TreeLoader: TRDbTreeLoader;
    GroupsEditor: TRDbTreeEditor;
    ItemsEditor: TRDbTreeEditor;
    ReportList: TAction;
    itemReportList: TMenuItem;
    itemReportListR: TMenuItem;
    itemReportListP: TMenuItem;
    Attachments: TAction;
    itemAttachments: TMenuItem;
    itemAttachmentsP: TMenuItem;
    divAttach: TMenuItem;
    CopyRecord: TAction;
    itemCopyRecord: TMenuItem;
    itemCopyRecordP: TMenuItem;
    SubitemsEditor: TRDbTreeEditor;
    NewSubItem: TAction;
    itemNewSubItem: TMenuItem;
    itemNewSubItemP: TMenuItem;
    itemNewSubItemN: TMenuItem;
    procedure SaveToLog(Sender: TObject;
      const EditTag: Integer; const Text: String);
    procedure GetRecordId(Sender: TObject; var Value: Integer);
    procedure FreeRecordId(Sender: TObject; var Value: Integer);
    procedure ItemsBeforeShowEditor(Sender: TObject; Editor: TForm;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure NewGroupUpdate(Sender: TObject);
    procedure NewGroupExecute(Sender: TObject);
    procedure NewSubGroupUpdate(Sender: TObject);
    procedure NewSubGroupExecute(Sender: TObject);
    procedure NewItemUpdate(Sender: TObject);
    procedure NewItemExecute(Sender: TObject);
    procedure PropertiesUpdate(Sender: TObject);
    procedure PropertiesExecute(Sender: TObject);
    procedure DeleteItemUpdate(Sender: TObject);
    procedure DeleteItemExecute(Sender: TObject);
    procedure GroupsBeforeShowEditor(Sender: TObject;
      Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure ReportListUpdate(Sender: TObject);
    procedure ReportListExecute(Sender: TObject);
    procedure AttachmentsUpdate(Sender: TObject);
    procedure AttachmentsExecute(Sender: TObject);
    procedure AfterProcessRecord(Sender: TObject;
      const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure CopyRecordUpdate(Sender: TObject);
    procedure CopyRecordExecute(Sender: TObject);
    procedure GroupsEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure ItemsEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure SubitemsEditorBeforeDelete(Sender: TObject; OldData,
      NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure SubitemsEditorBeforeShowEditor(Sender: TObject;
      Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
      var Complete: Boolean);
    procedure NewSubItemUpdate(Sender: TObject);
    procedure NewSubItemExecute(Sender: TObject);
  protected
    procedure InitDataComponents; override;
    function  LoadDataTreeBegin: Boolean; override;
    function  LoadDataTree: Boolean; override;
    function  LoadDataTreeEnd: Boolean; override;
    procedure CloseDataSets; override;
    function  TreeNodeMoveEnabled(Node: TTreeNode): Boolean; override;
    function  TreeNodeSelectTargetNode(MovedNode: TTreeNode): TTreeNode; override;
    function  TreeNodeMove(MovedNode, TargetNode: TTreeNode): Boolean; override;
    function  ReportsVar_FilterTree: string; virtual;
    function  ReportsVar_FilterUser: string; virtual;
    function  ReportsVar_Variables: string; virtual;
    function  ReportsVar_DefaultPath: string; virtual;
  end;


implementation

{$R *.dfm}

uses
  {$IFDEF RSS} RDbLog, {$ENDIF}
  {$IFDEF FR4} ReportsForm, {$ENDIF}
  {$IFDEF ATTACH} RDbAttachs, {$ENDIF}
  RVclUtils, RTreeMove, RMsgRu, RDbState, BaseDbUnit, TmplDbDialog;

{ == Инициализация формы ======================================================= }
procedure TDbTreeTemplate.InitDataComponents;
begin
  try
    inherited;
  finally
    {$IFDEF FR4}
    ReportList.Visible := True;
    {$ELSE}
    ReportList.Visible := False;
    {$ENDIF}
    {$IFDEF ATTACH}
    Attachments.Visible := GroupsEditor.KeyFieldIsPresent or ItemsEditor.KeyFieldIsPresent;
    divAttach.Visible := Attachments.Visible;
    {$ELSE}
    Attachments.Visible := False;
    divAttach.Visible := False;
    {$ENDIF}
  end;
end;

{ == Загрузка дерева =========================================================== }
function TDbTreeTemplate.LoadDataTreeBegin: Boolean;
begin
  Result := TreeLoader.BeforeLoadTree;
end;

function TDbTreeTemplate.LoadDataTree: Boolean;
begin
  Result := TreeLoader.LoadTree;
end;

function TDbTreeTemplate.LoadDataTreeEnd: Boolean;
begin
  Result := TreeLoader.AfterLoadTree;
end;

{ == Закрываем открытые наборы данных ========================================== }
procedure TDbTreeTemplate.CloseDataSets;
begin
  TreeLoader.CloseDataSets;
end;

{ == Генерация индекса записи ================================================== }
procedure TDbTreeTemplate.GetRecordId(Sender: TObject; var Value: Integer);
begin
  Value := BaseData.GetNewId(TRDbCustomEditor(Sender).GetObjectName(etView));
end;

procedure TDbTreeTemplate.FreeRecordId(Sender: TObject; var Value: Integer);
begin
  BaseData.FreeId(TRDbCustomEditor(Sender).GetObjectName(etView), Value);
end;

{ == Вызов формы-редактора записи ============================================== }
procedure TDbTreeTemplate.GroupsBeforeShowEditor(Sender: TObject;
  Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  if Editor is TDbDialogTemplate then
  begin
    TDbDialogTemplate(Editor).Caption := GetEditorCaption(
      TRDbCustomEditor(Sender).GetObjectDesc(etView), TRDbCustomEditor(Sender).DataSet);
    TDbDialogTemplate(Editor).DataSource.DataSet := TRDbCustomEditor(Sender).DataSet;
  end;
end;

procedure TDbTreeTemplate.ItemsBeforeShowEditor(Sender: TObject; Editor: TForm;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  if Editor is TDbDialogTemplate then
  begin
    TDbDialogTemplate(Editor).Caption := GetEditorCaption(
      TRDbCustomEditor(Sender).GetObjectDesc(etView), TRDbCustomEditor(Sender).DataSet);
    TDbDialogTemplate(Editor).DataSource.DataSet := TRDbCustomEditor(Sender).DataSet;
  end;
end;

procedure TDbTreeTemplate.SubitemsEditorBeforeShowEditor(Sender: TObject;
  Editor: TForm; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  if Editor is TDbDialogTemplate then
  begin
    TDbDialogTemplate(Editor).Caption := GetEditorCaption(
      TRDbCustomEditor(Sender).GetObjectDesc(etView), TRDbCustomEditor(Sender).DataSet);
    TDbDialogTemplate(Editor).DataSource.DataSet := TRDbCustomEditor(Sender).DataSet;
  end;
end;

{ == Запись в журнал аудита ==================================================== }
procedure TDbTreeTemplate.SaveToLog(Sender: TObject; const EditTag: Integer; const Text: String);
begin
  {$IFDEF RSS}
  AddToDbLog(EditTag, Text);
  {$ENDIF}
end;

{ == Завершение редактирования ================================================= }
procedure TDbTreeTemplate.AfterProcessRecord(Sender: TObject;
  const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean);
begin
  ShowItemsCount;
end;

{ == Создание новой группы ===================================================== }
procedure TDbTreeTemplate.NewGroupUpdate(Sender: TObject);
begin
  NewGroup.Enabled := IsNotWait and TreeLoader.NodeCanInserted(TreeView.Selected, ntGroup, True);
end;

procedure TDbTreeTemplate.NewGroupExecute(Sender: TObject);
var
  ParentNode: TTreeNode;
begin
  case TreeView.GetNodeType(TreeView.Selected) of
    ntRoot:    ParentNode := TreeView.Selected;
    ntGroup:   ParentNode := TreeView.Selected.Parent;
    ntItem:    ParentNode := TreeView.Selected.Parent.Parent;
    ntSubitem: ParentNode := TreeView.Selected.Parent.Parent.Parent;
    else       ParentNode := nil;
  end;
  if ParentNode = nil then ParentNode := TreeView.FindNode([ntRoot], intDisable);
  TreeLoader.InsertNode(ParentNode, ntGroup);
end;

procedure TDbTreeTemplate.NewSubGroupUpdate(Sender: TObject);
begin
  NewSubGroup.Enabled := IsNotWait
    and Assigned(TreeView.Selected)
    and TreeLoader.NodeCanInserted(TreeView.Selected, ntGroup, True);
end;

procedure TDbTreeTemplate.NewSubGroupExecute(Sender: TObject);
var
  ParentNode: TTreeNode;
begin
  case TreeView.GetNodeType(TreeView.Selected) of
    ntRoot:    ParentNode := TreeView.Selected;
    ntGroup:   ParentNode := TreeView.Selected;
    ntItem:    ParentNode := TreeView.Selected.Parent;
    ntSubitem: ParentNode := TreeView.Selected.Parent.Parent;
    else       ParentNode := nil;
  end;
  if ParentNode = nil then ParentNode := TreeView.FindNode([ntRoot], intDisable);
  TreeLoader.InsertNode(ParentNode, ntGroup);
end;

{ == Создание нового элемента ================================================== }
procedure TDbTreeTemplate.NewItemUpdate(Sender: TObject);
begin
  NewItem.Enabled := IsNotWait
    and Assigned(TreeView.Selected)
    and TreeLoader.NodeCanInserted(TreeView.Selected, ntItem, True);
end;

procedure TDbTreeTemplate.NewItemExecute(Sender: TObject);
begin
  case TreeView.GetNodeType(TreeView.Selected) of
    ntGroup:   TreeLoader.InsertNode(TreeView.Selected, ntItem);
    ntItem:    TreeLoader.InsertNode(TreeView.Selected.Parent, ntItem);
    ntSubitem: TreeLoader.InsertNode(TreeView.Selected.Parent.Parent, ntItem);
  end;
end;

{ == Создание нового подэлемента ================================================== }
procedure TDbTreeTemplate.NewSubItemUpdate(Sender: TObject);
begin
  NewSubItem.Enabled := IsNotWait
    and Assigned(TreeView.Selected)
    and TreeLoader.NodeCanInserted(TreeView.Selected, ntSubitem, True);
end;

procedure TDbTreeTemplate.NewSubItemExecute(Sender: TObject);
begin
  case TreeView.GetNodeType(TreeView.Selected) of
    ntGroup:   TreeLoader.InsertNode(TreeView.Selected, ntSubitem);
    ntItem:    TreeLoader.InsertNode(TreeView.Selected, ntSubitem);
    ntSubitem: TreeLoader.InsertNode(TreeView.Selected.Parent, ntSubitem);
  end;
end;

{ == Копирование элемента ====================================================== }
procedure TDbTreeTemplate.CopyRecordUpdate(Sender: TObject);
begin
  CopyRecord.Enabled := IsNotWait and TreeLoader.NodeCanCopied(TreeView.Selected, True)
end;

procedure TDbTreeTemplate.CopyRecordExecute(Sender: TObject);
begin
  TreeLoader.CopyNode(TreeView.Selected);
end;

{ == Редактирование элемента =================================================== }
procedure TDbTreeTemplate.PropertiesUpdate(Sender: TObject);
begin
  Properties.Enabled := IsNotWait and TreeLoader.NodeCanOpened(TreeView.Selected, True);
end;

procedure TDbTreeTemplate.PropertiesExecute(Sender: TObject);
begin
  TreeLoader.EditNode(TreeView.Selected, TreeLoader.NodeCanEdited(TreeView.Selected, True));
end;

{ == Перемещение элемента ====================================================== }
function TDbTreeTemplate.TreeNodeMoveEnabled(Node: TTreeNode): Boolean;
begin
  Result := TreeLoader.NodeCanMoved(Node, True);
end;

function TDbTreeTemplate.TreeNodeSelectTargetNode(MovedNode: TTreeNode): TTreeNode;
begin
  Result := MoveToTreeNode(TreeView, False, [ntRoot, ntGroup], [ntRoot, ntGroup]);
end;

function TDbTreeTemplate.TreeNodeMove(MovedNode, TargetNode: TTreeNode): Boolean;
begin
  Result := TreeLoader.MoveNode(MovedNode, TargetNode);
end;

{ == Удаление элемента ========================================================= }
procedure TDbTreeTemplate.DeleteItemUpdate(Sender: TObject);
begin
  DeleteItem.Enabled := IsNotWait and TreeLoader.NodeCanDeleted(TreeView.Selected, True);
end;

procedure TDbTreeTemplate.GroupsEditorBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  Complete := Complete and
    rAttachs_DeleteAttachments(BaseData.acDb, GroupsEditor);
{$ENDIF}
end;

procedure TDbTreeTemplate.ItemsEditorBeforeDelete(Sender: TObject; OldData,
  NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer;
  var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  Complete := Complete and
    rAttachs_DeleteAttachments(BaseData.acDb, ItemsEditor);
{$ENDIF}
end;

procedure TDbTreeTemplate.SubitemsEditorBeforeDelete(Sender: TObject;
  OldData, NewData: TRecordData; const Mode: TEditMode;
  const EditTag: Integer; var Complete: Boolean);
begin
  inherited;
{$IFDEF ATTACH}
  Complete := Complete and
    rAttachs_DeleteAttachments(BaseData.acDb, SubitemsEditor);
{$ENDIF}
end;

procedure TDbTreeTemplate.DeleteItemExecute(Sender: TObject);
begin
  TreeLoader.DeleteNode(TreeView.Selected, True);
end;

{ == Настраиваемые отчеты ====================================================== }
procedure TDbTreeTemplate.ReportListUpdate(Sender: TObject);
begin
  ReportList.Enabled := IsNotWait and ReportList.Visible
    and GroupsEditor.DataSetIsOpened; // and (Tag > 0);
end;

function TDbTreeTemplate.ReportsVar_FilterTree: string;
begin
  Result := EmptyStr;
end;

function TDbTreeTemplate.ReportsVar_FilterUser: string;
begin
  Result := EmptyStr;
end;

function TDbTreeTemplate.ReportsVar_Variables: string;
begin
  Result := EmptyStr;
end;

function TDbTreeTemplate.ReportsVar_DefaultPath: string;
begin
  Result := EmptyStr;
end;

procedure TDbTreeTemplate.ReportListExecute(Sender: TObject);
{$IFDEF FR4}
var
  DsList: TDataSets;
{$ENDIF}
begin
{$IFDEF FR4}
  SetLength(DsList, 0);
  try
    CreateListDataSet(Self, DsList);
    OpenReportsList(BaseData.acDb, Self, DsList,
      ReportsVar_FilterTree, ReportsVar_FilterUser, ReportsVar_Variables,
      ReportsVar_DefaultPath,
      BaseData.orEditReports);
  finally
    SetLength(DsList, 0);
  end;
{$ENDIF}
end;

{ == Прикрепленные файлы ======================================================= }
procedure TDbTreeTemplate.AttachmentsUpdate(Sender: TObject);
begin
  if IsNotWait and Attachments.Visible
  {$IFDEF RSS} and (Tag > 0) {$ENDIF}
  and Assigned(TreeView.Selected) then
  begin
    case TreeView.GetNodeType(TreeView.Selected) of
      ntGroup: Attachments.Enabled := GroupsEditor.KeyFieldIsPresent;
      ntItem: Attachments.Enabled := ItemsEditor.KeyFieldIsPresent;
    end;
  end
  else Attachments.Enabled := False;
end;

procedure TDbTreeTemplate.AttachmentsExecute(Sender: TObject);
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

