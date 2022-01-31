unit RDbTree;

interface

uses
  Classes, ComCtrls, Db,
  RavTreeView, RVclUtils, RDbData, RDbEditor;

type
  TTreePrgsMode = (pmNone, pmAlways, pmAuto);
  TTreeLoadMode = (lmAll, lmGroups, lmItems, lmAllParam, lmGroupsParam, lmItemsParam, lmSubitemsParam);

  TGetImageNotifyEvent      = procedure (Sender: TObject; const Selected: Boolean; var Value: Integer) of object;
  TCheckNodeLoadNotifyEvent = procedure (Sender: TObject; const LoadMode: TTreeLoadMode;
                                         const NodeType: TNodeType; const RecordId: Integer;
                                         var LoadNode: Boolean) of object;
  TCompletedNotifyEvent     = procedure (Sender: TObject; var Completed: Boolean) of object;
  TNodeNotifyEvent = procedure (Sender: TObject; const Node: TTreeNode; const Mode: TEditMode) of object;
  TResultNodeNotifyEvent    = procedure (Sender: TObject; ParentData: TRecordData; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;

  TRDbTreeLoader = class;

  TRDbTreeEditor = class (TRDbCustomEditor)
  private
    fNameField: string;
    fSortField: string;
    fNoteField: string;
    fBaseFilter: string;
    fNrmImage: Integer;
    fSelImage: Integer;
    fCheckDataSetState: Boolean;
    fOnGetImage: TGetImageNotifyEvent;
    fOnGetName: TVarStringNotifyEvent;
    fOnGetNote: TVarStringNotifyEvent;
    fOnGetText: TVarStringNotifyEvent;
    fOnGetSort: TVarStringNotifyEvent;
    fOnCreateNewNode: TResultNodeNotifyEvent;
    fOnAfterUpdateNode: TNodeNotifyEvent;
  protected
    procedure FindNodeRecord(Node: TTreeNode);
    procedure DoCreateNewNode(const ParentData: TRecordData; var Complete: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // Get node properies
    function GetNodeImage(const Selected: Boolean): Integer;
    function GetNameValueDef: string;
    function GetNameValue: string;
    function GetNoteValueDef: string;
    function GetNoteValue: string;
    function GetSortValueDef: string;
    function GetNodeTextDef: string;
    function GetNodeText: string;
    function GetNodeSort: string;
    // Edit
    function InsertNode(Loader: TRDbTreeLoader; ParentNode: TTreeNode;
      const ParentData: TRecordData; const NodeType: TNodeType): Boolean;
    function ImportNode(Loader: TRDbTreeLoader; ParentNode: TTreeNode;
      const ParentData: TRecordData; const NodeType: TNodeType; Proc: TCustomModifyProc;
      const Data: Pointer; const StatusMsg: string = ''): Boolean;
    function CopyNode(Loader: TRDbTreeLoader; Node: TTreeNode; const ShowEditor: Boolean = True): Boolean;
    function EditNode(Loader: TRDbTreeLoader; Node: TTreeNode; const EnableEdit: Boolean = True): Boolean;
    function MoveNode(MovedNode, ParentNode: TTreeNode): Boolean;
    function DeleteNode(Node: TTreeNode; const ShowDeleteQuery: Boolean = True): Boolean;
  published
    property CheckDataSetState: Boolean read fCheckDataSetState write fCheckDataSetState default True;
    property NameFieldName: string read fNameField write fNameField;
    property NotesFieldName: string read fNoteField write fNoteField;
    property SortFieldName: string read fSortField write fSortField;
    property BaseFilter: string read fBaseFilter write fBaseFilter;
    property NrmImage: Integer read fNrmImage write fNrmImage;
    property SelImage: Integer read fSelImage write fSelImage;
    property OnGetImage: TGetImageNotifyEvent read fOnGetImage write fOnGetImage;
    property OnGetName: TVarStringNotifyEvent read fOnGetName write fOnGetName;
    property OnGetNote: TVarStringNotifyEvent read fOnGetNote write fOnGetNote;
    property OnGetText: TVarStringNotifyEvent read fOnGetText write fOnGetText;
    property OnGetSort: TVarStringNotifyEvent read fOnGetSort write fOnGetSort;
    property OnCreateNewNode: TResultNodeNotifyEvent read fOnCreateNewNode write fOnCreateNewNode;
    property OnAfterUpdateNode: TNodeNotifyEvent read fOnAfterUpdateNode write fOnAfterUpdateNode;
  end;

  TGetNodeRightsNotifyEvent = procedure (Sender: TObject; const NodeType: TNodeType; const Mode: TEditMode; var Enable: Boolean) of object;

  TRDbTreeLoader = class(TComponent)
  private
    fProgress: Boolean;
    // TreeView
    fTreeView: TRTreeView;
    fLoadMode: TTreeLoadMode;
    fPrgsMode: TTreePrgsMode;
    fCheckLoadNode: TCheckNodeLoadNotifyEvent;
    // DataSets
    fOpenDataSets: TCompletedNotifyEvent;
    fBeforeLoadTree: TCompletedNotifyEvent;
    fAfterLoadTree: TCompletedNotifyEvent;
    fCloseDataSets: TNotifyEvent;
    // RootNode
    fRootNodeText: string;
    fRootNodeNrmImage: Integer;
    fRootNodeSelImage: Integer;
    fOnRootGetImage: TGetImageNotifyEvent;
    fOnRootGetText: TVarStringNotifyEvent;
    // Editors
    fGroupsEditor: TRDbTreeEditor;
    fItemsEditor: TRDbTreeEditor;
    fSubitemsEditor: TRDbTreeEditor;
    fItemsIsLinked: Boolean;
    fSubitemsIsLinked: Boolean;
    fOnGetEditRights: TGetNodeRightsNotifyEvent;
    fEnableParentDelete: Boolean;
    // Procedures
    procedure SetGroupsEditor(const Value: TRDbTreeEditor);
    procedure SetItemsEditor(const Value: TRDbTreeEditor);
    procedure SetSubitemsEditor(const Value: TRDbTreeEditor);
    procedure SetTreeView(const Value: TRTreeView);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  CheckNodeLoad(const NodeType: TNodeType; const RecordId: Integer): Boolean;
    function  ItemsIsLinked: Boolean;
    function  SubitemsIsLinked: Boolean;
    function  GetTreeLoadCount: Integer;
    procedure LoadTreeStructure(const BaseNode: TTreeNode);
    procedure FindNodeOwner(Node: TTreeNode);
  public
    // Component
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // TreeView
    function  BeforeLoadTree: Boolean;
    function  LoadTree: Boolean; overload;
    function  LoadTree(const ALoadMode: TTreeLoadMode): Boolean; overload;
    function  ReloadNode(const BaseNode: TTreeNode): Boolean;
    function  AfterLoadTree: Boolean;
    // DataSets
    function  OpenDataSets: Boolean;
    procedure CloseDataSets;
    function  NodeLocate(const Node: TTreeNode): Boolean;
    function  GetNodeRecordData(const Node: TTreeNode): TRecordData;
    // RootNode
    function  RootIsLoaded: Boolean;
    function  GetRootNodeImage(const Selected: Boolean): Integer;
    function  GetRootNodeText: string;
    // Groups
    function  GroupsIsLoaded(const CheckState: Boolean = True): Boolean;
    function  GroupsDataSet: TDataSet;
    function  GroupsDataSetIsOpen(const CheckState: Boolean = True): Boolean;
    function  GroupsDataSetIsNotEmpty: Boolean;
    function  GroupsDataSetIsOwner: Boolean;
    function  GetGroupsKeyField: TField;
    function  GetGroupsKeyValue: Integer;
    function  GetGroupsOwnerField: TField;
    function  GetGroupsOwnerValue: Integer;
    function  GetGroupsNodeImage(const Selected: Boolean): Integer;
    function  GetGroupsNodeText: string;
    function  GetGroupsNodeSort: string;
    function  GroupsDataSetLocate(const RecordId: Integer): Boolean;
    // Items
    function  ItemsIsLoaded(const CheckState: Boolean = True): Boolean;
    function  ItemsDataSet: TDataSet;
    function  ItemsDataSetIsOpen(const CheckState: Boolean = True): Boolean;
    function  ItemsDataSetIsNotEmpty: Boolean;
    function  ItemsDataSetIsOwner: Boolean;
    function  GetItemsKeyField: TField;
    function  GetItemsKeyValue: Integer;
    function  GetItemsOwnerField: TField;
    function  GetItemsOwnerValue: Integer;
    function  GetItemsNodeImage(const Selected: Boolean): Integer;
    function  GetItemsNodeText: string;
    function  GetItemsNodeSort: string;
    function  ItemsDataSetLocate(const RecordId: Integer): Boolean;
    // Subitems
    function  SubitemsIsLoaded(const CheckState: Boolean = True): Boolean;
    function  SubitemsDataSet: TDataSet;
    function  SubitemsDataSetIsOpen(const CheckState: Boolean = True): Boolean;
    function  SubitemsDataSetIsNotEmpty: Boolean;
    function  SubitemsDataSetIsOwner: Boolean;
    function  GetSubitemsKeyField: TField;
    function  GetSubitemsKeyValue: Integer;
    function  GetSubitemsOwnerField: TField;
    function  GetSubitemsOwnerValue: Integer;
    function  GetSubitemsNodeImage(const Selected: Boolean): Integer;
    function  GetSubitemsNodeText: string;
    function  GetSubitemsNodeSort: string;
    function  SubitemsDataSetLocate(const RecordId: Integer): Boolean;
    // Edit Rights
    function  NodeCanInserted(Node: TTreeNode; const NodeType: TNodeType; const CheckRights: Boolean = True): Boolean;
    function  NodeCanCopied(Node: TTreeNode; const CheckRights: Boolean = True): Boolean;
    function  NodeCanOpened(Node: TTreeNode; const CheckRights: Boolean = True): Boolean;
    function  NodeCanEdited(Node: TTreeNode; const CheckRights: Boolean = True): Boolean;
    function  NodeCanMoved(Node: TTreeNode; const CheckRights: Boolean = True): Boolean;
    function  NodeCanDeleted(Node: TTreeNode; const CheckRights: Boolean = True): Boolean;
    // Edit
    function  InsertNode(ParentNode: TTreeNode; const NodeType: TNodeType): Boolean;
    function  CopyNode(Node: TTreeNode): Boolean;
    function  EditNode(Node: TTreeNode; const EnableEdit: Boolean = True): Boolean;
    function  MoveNode(MovedNode, ParentNode: TTreeNode): Boolean;
    function  DeleteNode(Node: TTreeNode; const ShowDeleteQuery: Boolean = True): Boolean;
  published
    property  TreeView: TRTreeView read fTreeView write SetTreeView;
    property  LoadMode: TTreeLoadMode read fLoadMode write fLoadMode default lmAll;
    property  ShowIndicator: TTreePrgsMode read fPrgsMode write fPrgsMode default pmAuto;
    property  OnCheckLoadNode: TCheckNodeLoadNotifyEvent read fCheckLoadNode write fCheckLoadNode;
    // DataSets
    property  OnOpenDataSets: TCompletedNotifyEvent read fOpenDataSets write fOpenDataSets;
    property  OnBeforeLoadTree: TCompletedNotifyEvent read fBeforeLoadTree write fBeforeLoadTree;
    property  OnAfterLoadTree: TCompletedNotifyEvent read fAfterLoadTree write fAfterLoadTree;
    property  OnCloseDataSets: TNotifyEvent read fCloseDataSets write fCloseDataSets;
    // RootNode
    property  RootText: string read fRootNodeText write fRootNodeText;
    property  RootNrmImage: Integer read fRootNodeNrmImage write fRootNodeNrmImage;
    property  RootSelImage: Integer read fRootNodeSelImage write fRootNodeSelImage;
    property  OnRootGetImage: TGetImageNotifyEvent read fOnRootGetImage write fOnRootGetImage;
    property  OnRootGetText: TVarStringNotifyEvent read fOnRootGetText write fOnRootGetText;
    // Editors
    property  GroupsEditor: TRDbTreeEditor read fGroupsEditor write SetGroupsEditor;
    property  ItemsEditor: TRDbTreeEditor read fItemsEditor write SetItemsEditor;
    property  SubitemsEditor: TRDbTreeEditor read fSubitemsEditor write SetSubitemsEditor;
    property  LinkedItemsDS: Boolean read fItemsIsLinked write fItemsIsLinked default False;
    property  LinkedSubitemsDS: Boolean read fSubitemsIsLinked write fSubitemsIsLinked default False;
    property  EnableParentNodeDelete: Boolean read fEnableParentDelete write fEnableParentDelete default False;
    property  OnGetEditRights: TGetNodeRightsNotifyEvent read fOnGetEditRights write fOnGetEditRights;
  end;

implementation

uses
  SysUtils, Forms, Controls,
  RDialogs, RMsgRu, RDbConst, RStrUtils, RProgress, RExHandlers;

resourcestring
  SErrEditNodeIsNull          = '%s: Не указан редактируемый элемент!';
  SErrNodeEditorIsNull        = '%s: Не определен редактор элементов!';

const
  MinTreeLoadCount            = 10;

{ == TRDbTreeEditor ============================================================ }

constructor TRDbTreeEditor.Create(AOwner: TComponent);
begin
  inherited;
  fNameField := EmptyStr;
  fNoteField := EmptyStr;
  fNrmImage := intDisable;
  fSelImage := intDisable;
  fBaseFilter := EmptyStr;
  fCheckDataSetState := True;
  fOnGetImage := nil;
  fOnGetName := nil;
  fOnGetNote := nil;
  fOnGetText := nil;
  fOnCreateNewNode := nil;
  fOnAfterUpdateNode := nil;
end;

// Get node properies ----------------------------------------------------------

function TRDbTreeEditor.GetNodeImage(const Selected: Boolean): Integer;
begin
  if Selected then Result := FSelImage else Result := FNrmImage;
  if Assigned(FOnGetImage) then FOnGetImage(Self, Selected, Result);
end;

function TRDbTreeEditor.GetNameValueDef: string;
var
  DS: TDataSet;
  NameFld: TField;
begin
  Result := EmptyStr;
  DS := DataSet;
  if (DS <> nil) and (FNameField <> EmptyStr) and DS.Active and not DS.IsEmpty then
  begin
    NameFld := DS.FindField(FNameField);
    if NameFld <> nil then Result := NameFld.DisplayText;
  end;
end;

function TRDbTreeEditor.GetNameValue: string;
begin
  Result := GetNameValueDef;
  if Assigned(FOnGetName) then FOnGetName(Self, Result);
end;

function TRDbTreeEditor.GetNoteValueDef: string;
var
  DS: TDataSet;
  NoteFld: TField;
begin
  Result := EmptyStr;
  DS := DataSet;
  if (DS <> nil) and (FNoteField <> EmptyStr) and DS.Active and not DS.IsEmpty then
  begin
    NoteFld := DS.FindField(FNoteField);
    if NoteFld <> nil then Result := NoteFld.DisplayText;
  end;
end;

function TRDbTreeEditor.GetNoteValue: string;
begin
  Result := GetNoteValueDef;
  if Assigned(FOnGetNote) then FOnGetNote(Self, Result);
end;

function TRDbTreeEditor.GetSortValueDef: string;
var
  DS: TDataSet;
  SortFld: TField;
begin
  Result := EmptyStr;
  DS := DataSet;
  if (DS <> nil) and (FSortField <> EmptyStr) and DS.Active and not DS.IsEmpty then
  begin
    SortFld := DS.FindField(FSortField);
    if SortFld <> nil then Result := SortFld.DisplayText;
  end;
end;

function TRDbTreeEditor.GetNodeSort: string;
begin
  Result := GetSortValueDef;
  if Assigned(FOnGetSort) then FOnGetSort(Self, Result);
end;

function TRDbTreeEditor.GetNodeTextDef: string;
begin
  Result := AddNotes(GetNameValue, GetNoteValue);
end;

function TRDbTreeEditor.GetNodeText: string;
begin
  Result := GetNodeTextDef;
  if Assigned(FOnGetText) then FOnGetText(Self, Result);
end;

// Edit ------------------------------------------------------------------------

procedure TRDbTreeEditor.FindNodeRecord(Node: TTreeNode);
begin
  CheckDataSet;
  if (Node = nil) or (Node.Data = nil) then
    raise ERDbEditorError.CreateFmt(SErrEditNodeIsNull, [Self.Name]);
  if not LocateKey(TNodeData(Node.Data)^.RecordId) then
    raise ERDbEditorError.CreateFmt(SErrDSIdNotFound, [TNodeData(Node.Data)^.RecordId, DataSet.Name]);
end;

procedure TRDbTreeEditor.DoCreateNewNode(const ParentData: TRecordData; var Complete: Boolean);
begin
  if Assigned(fOnCreateNewNode) then
    fOnCreateNewNode(Self, ParentData, etInsert, GetEditTag(etInsert), Complete);
end;

function TRDbTreeEditor.InsertNode(Loader: TRDbTreeLoader; ParentNode: TTreeNode;
  const ParentData: TRecordData; const NodeType: TNodeType): Boolean;
var
  NewNode: TTreeNode;
  OwnerId, NewId: Integer;
  Editor: TForm;
begin
  Result := WaitPreviousOperation;
  if Result then
  begin
    CheckDataSet;
    DoPrepareStart;
    try
      try
        Editor := EditorFormCreate;
        try
          if NewRecordAppend then DataSet.Append else DataSet.Insert;
          try
            DoProcessRecordStart(etInsert, Result);
            try
              CreateKeyValue(NewId);
              OwnerId := intDisable;
              if (ParentNode <> nil) and (ParentNode.Data <> nil) then
                OwnerId := TNodeData(ParentNode.Data)^.RecordId;
              SetOwnerValue(OwnerId);
              DoCreateNewRecord(etInsert, Result);
              DoCreateNewNode(ParentData, Result);
              if EditorFormShow(Editor, etInsert, Result) then
              begin
                DoPostData(Editor, nil, etInsert, Result, True);
                if Result then
                begin
                  Loader.TreeView.Items.BeginUpdate;
                  try
                    NewNode := Loader.TreeView.CreateTypeNode(ParentNode, NodeType,
                      GetKeyValue, GetNodeImage(False), GetNodeImage(True), GetNodeText, GetNodeSort);
                    Loader.TreeView.Selected := NewNode;
                    if Assigned(fOnAfterUpdateNode) then
                      fOnAfterUpdateNode(Self, NewNode, etInsert);
                    Loader.TreeView.Sort;
                  finally
                    Loader.TreeView.Items.EndUpdate;
                  end;
                end;
              end
              else DoCancelData(Editor, nil, etInsert, Result, True);
            finally
              FreeKeyValue(NewId);
            end;
          finally
            RollbackModifyState;
          end;
        finally
          EditorFormFree(Editor, etInsert, Result);
        end;
      except
        on E: Exception do
        begin
          Result := False;
          HandleExcept(E, Self, SErrRecordInsert);
        end;
      end;
    finally
      DoProcessRecordEnd(etInsert, Result, True);
    end;
  end;
end;

function TRDbTreeEditor.ImportNode(Loader: TRDbTreeLoader; ParentNode: TTreeNode;
  const ParentData: TRecordData; const NodeType: TNodeType; Proc: TCustomModifyProc;
  const Data: Pointer; const StatusMsg: string = ''): Boolean;
var
  NewNode: TTreeNode;
  OwnerId, NewId: Integer;
begin
  Result := True;
  CheckDataSet;
  DoPrepareStart;
  try
    try
      if NewRecordAppend then DataSet.Append else DataSet.Insert;
      try
        DoProcessRecordStart(etImport, Result);
        try
          CreateKeyValue(NewId);
          OwnerId := intDisable;
          if (ParentNode <> nil) and (ParentNode.Data <> nil) then
            OwnerId := TNodeData(ParentNode.Data)^.RecordId;
          SetOwnerValue(OwnerId);
          DoCreateNewRecord(etImport, Result);
          DoCreateNewNode(ParentData, Result);
          if Assigned(Proc)
          then Proc(Self, Data, Result)
          else Result := False;
          if Result then
          begin
            DoPostData(nil, nil, etImport, Result, True);
            if Result then
            begin
              Loader.TreeView.Items.BeginUpdate;
              try
                NewNode := Loader.TreeView.CreateTypeNode(ParentNode, NodeType,
                  GetKeyValue, GetNodeImage(False), GetNodeImage(True), GetNodeText, GetNodeSort);
                Loader.TreeView.Selected := NewNode;
                if Assigned(fOnAfterUpdateNode) then
                  fOnAfterUpdateNode(Self, NewNode, etImport);
                Loader.TreeView.Sort;
              finally
                Loader.TreeView.Items.EndUpdate;
              end;
            end;
          end
          else DoCancelData(nil, nil, etImport, Result, True);
        finally
          FreeKeyValue(NewId);
        end;
      finally
        RollbackModifyState;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self, SErrRecordImport);
      end;
    end;
  finally
    DoProcessRecordEnd(etImport, Result, True);
  end;
end;

function TRDbTreeEditor.CopyNode(Loader: TRDbTreeLoader; Node: TTreeNode; const ShowEditor: Boolean = True): Boolean;
var
  Buffer: TCopyBuffer;
  ParentNode, NewNode: TTreeNode;
  OwnerId, NewId: Integer;
  Editor: TForm;
begin
  Result := not ShowEditor or WaitPreviousOperation;
  if Result then
  begin
    FindNodeRecord(Node);
    ParentNode := Node.Parent;
    CheckDataSet;
    DoPrepareStart;
    try
      try
        SetLength(Buffer, 0);
        DoCopyBuffer(Buffer);
        try
          Editor := nil;
          if ShowEditor then
            Editor := EditorFormCreate;
          try
            if NewRecordAppend then DataSet.Append else DataSet.Insert;
            try
              DoProcessRecordStart(etInsert, Result);
              try
                CreateKeyValue(NewId);
                OwnerId := intDisable;
                if (ParentNode <> nil) and (ParentNode.Data <> nil) then
                  OwnerId := TNodeData(ParentNode.Data)^.RecordId;
                SetOwnerValue(OwnerId);
                DoCreateNewRecord(etInsert, Result);
                // DoCreateNewNode(ParentData, Result);
                DoPasteBuffer(Buffer);
                if not ShowEditor or EditorFormShow(Editor, etInsert, Result) then
                begin
                  DoPostData(Editor, nil, etInsert, Result, ShowEditor);
                  if Result then
                  begin
                    Loader.TreeView.Items.BeginUpdate;
                    try
                      NewNode := Loader.TreeView.CreateTypeNode(ParentNode,
                        Loader.TreeView.GetNodeType(Node),
                        GetKeyValue, GetNodeImage(False), GetNodeImage(True), GetNodeText, GetNodeSort);
                      Loader.TreeView.Selected := NewNode;
                      if Assigned(fOnAfterUpdateNode) then
                        fOnAfterUpdateNode(Self, NewNode, etInsert);
                      Loader.TreeView.Sort;
                    finally
                      Loader.TreeView.Items.EndUpdate;
                    end;
                  end;
                end
                else DoCancelData(Editor, nil, etInsert, Result, ShowEditor);
              finally
                FreeKeyValue(NewId);
              end;
            finally
              RollbackModifyState;
            end;
          finally
            if Assigned(Editor) then
              EditorFormFree(Editor, etInsert, Result);
          end;
        finally
          FreeCopyBuffer(Buffer);
        end;
      except
        on E: Exception do
        begin
          Result := False;
          HandleExcept(E, Self, SErrRecordInsert);
        end;
      end;
    finally
      DoProcessRecordEnd(etInsert, Result, ShowEditor);
    end;
  end;
end;

function TRDbTreeEditor.EditNode(Loader: TRDbTreeLoader;
  Node: TTreeNode; const EnableEdit: Boolean = True): Boolean;
var
  OldData: TRecordData;
  CurrMode: TEditMode;
  Editor: TForm;
begin
  Result := WaitPreviousOperation;
  if Result then
  begin
    FindNodeRecord(Node);
    CheckDataSet;
    DoPrepareStart;
    if (OpenMode <> omView) and EnableEdit
    then CurrMode := etEdit
    else CurrMode := etView;
    try
      try
        Editor := EditorFormCreate;
        try
          OldData := GetRecordData(DataSet);
          try
            if CurrMode = etEdit then DataSet.Edit;
            DoProcessRecordStart(CurrMode, Result);
            if EditorFormShow(Editor, CurrMode, Result) then
            begin
              DoPostData(Editor, OldData, CurrMode, Result, True);
              if Result then
              begin
                Loader.TreeView.Items.BeginUpdate;
                try
                  Node.ImageIndex := GetNodeImage(False);
                  Node.SelectedIndex := GetNodeImage(True);
                  Node.Text := GetNodeText;
                  Loader.FindNodeOwner(Node);
                  if Assigned(fOnAfterUpdateNode) then
                    fOnAfterUpdateNode(Self, Node, etEdit);
                  Loader.TreeView.Sort;
                finally
                  Loader.TreeView.Items.EndUpdate;
                end;
              end;
            end
            else DoCancelData(Editor, OldData, CurrMode, Result, True);
          finally
            RollbackModifyState;
            FreeRecordData(OldData);
          end;
        finally
          EditorFormFree(Editor, CurrMode, Result);
        end;
      except
        on E: Exception do
        begin
          Result := False;
          HandleExcept(E, Self, SErrRecordEdit);
        end;
      end;
    finally
      DoProcessRecordEnd(CurrMode, Result, True);
    end;
  end;
end;

function TRDbTreeEditor.MoveNode(MovedNode, ParentNode: TTreeNode): Boolean;
var
  OldData: TRecordData;
  OwnerId: Integer;
begin
  Result := WaitPreviousOperation;
  if Result then
  begin
    FindNodeRecord(MovedNode);
    CheckDataSet;
    DoPrepareStart(SMsgSaveDataWait);
    try
      try
        OldData := GetRecordData(DataSet);
        try
          DataSet.Edit;
          DoProcessRecordStart(etMove, Result);
          OwnerId := intDisable;
          if (ParentNode <> nil) and (ParentNode.Data <> nil) then
            OwnerId := TNodeData(ParentNode.Data)^.RecordId;
          SetOwnerValue(OwnerId);
          DoMoveRecord(Result);
          if Result then
          begin
            DoPostData(nil, OldData, etMove, Result, False);
            if Result then
            begin
              TTreeView(MovedNode.TreeView).Items.BeginUpdate;
              try
                if ParentNode = nil
                then MovedNode.MoveTo(nil, naAdd)
                else MovedNode.MoveTo(ParentNode, naAddChild);
                if Assigned(fOnAfterUpdateNode) then
                  fOnAfterUpdateNode(Self, MovedNode, etDelete);
                if MovedNode.TreeView is TRTreeView then
                  TRTreeView(MovedNode.TreeView).Sort;
              finally
                TTreeView(MovedNode.TreeView).Items.EndUpdate;
              end;
            end;
          end
          else DoCancelData(nil, OldData, etMove, Result, False);
        finally
          RollbackModifyState;
          FreeRecordData(OldData);
        end;
      except
        on E: Exception do
        begin
          Result := False;
          HandleExcept(E, Self, SErrRecordEdit);
        end;
      end;
    finally
      DoProcessRecordEnd(etMove, Result, False);
    end;
  end;
end;

function TRDbTreeEditor.DeleteNode(Node: TTreeNode;
  const ShowDeleteQuery: Boolean): Boolean;
begin
  Result := WaitPreviousOperation;
  if Result then
  begin
    FindNodeRecord(Node);
    CheckDataSet;
    DoDeleteQuery(ShowDeleteQuery, Result);
    if Result then
    begin
      DoDeleteData(nil, Result, True);
      if Result then
      begin
        if Node.TreeView is TRTreeView
        then TRTreeView(Node.TreeView).DeleteSelection
        else TTreeView(Node.TreeView).Items.Delete(Node);
      end;
    end;
  end;
end;

{ == TRDbTreeLoader ============================================================ }

constructor TRDbTreeLoader.Create(AOwner: TComponent);
begin
  inherited;
  fProgress := False;
  // TreeView
  fTreeView := nil;
  fLoadMode := lmAll;
  fPrgsMode := pmAuto;
  fCheckLoadNode := nil;
  // Data Sets
  fOpenDataSets := nil;
  fCloseDataSets := nil;
  fBeforeLoadTree := nil;
  fAfterLoadTree := nil;
  // RootNode
  fRootNodeText := EmptyStr;
  fRootNodeNrmImage := intDisable;
  fRootNodeSelImage := intDisable;
  fOnRootGetImage := nil;
  fOnRootGetText := nil;
  // Editors
  fGroupsEditor := nil;
  fItemsEditor := nil;
  fSubitemsEditor := nil;
  fItemsIsLinked := False;
  fSubitemsIsLinked := False;
  fEnableParentDelete := False;
  fOnGetEditRights := nil;
end;

destructor TRDbTreeLoader.Destroy;
begin
  fGroupsEditor := nil;
  fItemsEditor := nil;
  fSubitemsEditor := nil;
  fTreeView := nil;
  inherited;
end;

procedure TRDbTreeLoader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) then
  begin
    if (fGroupsEditor <> nil) and (AComponent = fGroupsEditor) then fGroupsEditor := nil;
    if (fItemsEditor <> nil) and (AComponent = fItemsEditor) then fItemsEditor := nil;
    if (fSubitemsEditor <> nil) and (AComponent = fSubitemsEditor) then fSubitemsEditor := nil;
    if (fTreeView <> nil) and (AComponent = fTreeView) then fTreeView := nil;
  end;
end;

procedure TRDbTreeLoader.SetGroupsEditor(const Value: TRDbTreeEditor);
begin
  if fGroupsEditor <> Value then
  begin
    fGroupsEditor := Value;
    if Assigned(fGroupsEditor) then Value.FreeNotification(Self);
  end;
end;

procedure TRDbTreeLoader.SetItemsEditor(const Value: TRDbTreeEditor);
begin
  if fItemsEditor <> Value then
  begin
    fItemsEditor := Value;
    if Assigned(fItemsEditor) then Value.FreeNotification(Self);
  end;
end;

procedure TRDbTreeLoader.SetSubitemsEditor(const Value: TRDbTreeEditor);
begin
  if fSubitemsEditor <> Value then
  begin
    fSubitemsEditor := Value;
    if Assigned(fSubitemsEditor) then Value.FreeNotification(Self);
  end;
end;

procedure TRDbTreeLoader.SetTreeView(const Value: TRTreeView);
begin
  if fTreeView <> Value then
  begin
    fTreeView := Value;
    if Assigned(fTreeView) then Value.FreeNotification(Self);
  end;
end;

{ == Root Node ================================================================= }

function TRDbTreeLoader.RootIsLoaded: Boolean;
begin
  Result := fRootNodeText <> EmptyStr;
end;

function TRDbTreeLoader.GetRootNodeImage(const Selected: Boolean): Integer;
begin
  if Selected then Result := fRootNodeSelImage else Result := fRootNodeNrmImage;
  if Assigned(fOnRootGetImage) then fOnRootGetImage(Self, Selected, Result);
end;

function TRDbTreeLoader.GetRootNodeText: string;
begin
  Result := fRootNodeText;
  if Assigned(fOnRootGetText) then fOnRootGetText(Self, Result);
end;

{ == Groups ==================================================================== }

function TRDbTreeLoader.GroupsDataSet: TDataSet;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.DataSet
  else Result := nil;
end;

function TRDbTreeLoader.GroupsDataSetIsNotEmpty: Boolean;
begin
  Result := (fGroupsEditor <> nil) and fGroupsEditor.DataSetIsNotEmply;
end;

function TRDbTreeLoader.GroupsDataSetIsOpen(const CheckState: Boolean = True): Boolean;
begin
  if CheckState
  then Result := (fGroupsEditor <> nil) and fGroupsEditor.DataSetIsOpened
  else Result := (fGroupsEditor <> nil)
    and (not fGroupsEditor.CheckDataSetState or fGroupsEditor.DataSetIsOpened);
end;

function TRDbTreeLoader.GroupsDataSetIsOwner: Boolean;
begin
  Result := (fGroupsEditor <> nil) and fGroupsEditor.OwnerFieldIsPresent;
end;

function TRDbTreeLoader.GroupsIsLoaded(const CheckState: Boolean = True): Boolean;
begin
  Result := (fTreeView <> nil) and GroupsDataSetIsOpen(CheckState);
end;

function TRDbTreeLoader.GetGroupsKeyField: TField;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.GetKeyField
  else Result := nil;
end;

function TRDbTreeLoader.GetGroupsKeyValue: Integer;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.GetKeyValue
  else Result := intDisable;
end;

function TRDbTreeLoader.GetGroupsOwnerField: TField;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.GetOwnerField
  else Result := nil;
end;

function TRDbTreeLoader.GetGroupsOwnerValue: Integer;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.GetOwnerValue
  else Result := intDisable;
end;

function TRDbTreeLoader.GetGroupsNodeImage(const Selected: Boolean): Integer;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.GetNodeImage(Selected)
  else Result := intDisable;
end;

function TRDbTreeLoader.GetGroupsNodeText: string;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.GetNodeText
  else Result := EmptyStr;
end;

function TRDbTreeLoader.GetGroupsNodeSort: string;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.GetNodeSort
  else Result := EmptyStr;
end;

function TRDbTreeLoader.GroupsDataSetLocate(const RecordId: Integer): Boolean;
begin
  if (fGroupsEditor <> nil)
  then Result := fGroupsEditor.LocateKey(RecordId)
  else Result := False;
end;

{ == Items ===================================================================== }

function TRDbTreeLoader.ItemsDataSet: TDataSet;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.DataSet
  else Result := nil;
end;

function TRDbTreeLoader.ItemsDataSetIsNotEmpty: Boolean;
begin
  Result := (fItemsEditor <> nil) and fItemsEditor.DataSetIsNotEmply;
end;

function TRDbTreeLoader.ItemsDataSetIsOpen(const CheckState: Boolean = True): Boolean;
begin
  if CheckState
  then Result := (fItemsEditor <> nil) and fItemsEditor.DataSetIsOpened
  else Result := (fItemsEditor <> nil)
    and (not fItemsEditor.CheckDataSetState or fItemsEditor.DataSetIsOpened);
end;

function TRDbTreeLoader.ItemsDataSetIsOwner: Boolean;
begin
  Result := (fItemsEditor <> nil) and fItemsEditor.OwnerFieldIsPresent;
end;

function TRDbTreeLoader.ItemsIsLoaded(const CheckState: Boolean = True): Boolean;
begin
  Result := (fTreeView <> nil)
    and GroupsDataSetIsOpen(CheckState) and ItemsDataSetIsOpen(CheckState)
    and (fLoadMode in [lmAll, lmAllParam, lmItemsParam]);
end;

function TRDbTreeLoader.ItemsIsLinked: Boolean;
begin
  Result := fItemsIsLinked;
end;

function TRDbTreeLoader.GetItemsKeyField: TField;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.GetKeyField
  else Result := nil;
end;

function TRDbTreeLoader.GetItemsKeyValue: Integer;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.GetKeyValue
  else Result := intDisable;
end;

function TRDbTreeLoader.GetItemsOwnerField: TField;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.GetOwnerField
  else Result := nil;
end;

function TRDbTreeLoader.GetItemsOwnerValue: Integer;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.GetOwnerValue
  else Result := intDisable;
end;

function TRDbTreeLoader.GetItemsNodeImage(const Selected: Boolean): Integer;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.GetNodeImage(Selected)
  else Result := intDisable;
end;

function TRDbTreeLoader.GetItemsNodeText: string;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.GetNodeText
  else Result := EmptyStr;
end;

function TRDbTreeLoader.GetItemsNodeSort: string;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.GetNodeSort
  else Result := EmptyStr;
end;

function TRDbTreeLoader.ItemsDataSetLocate(const RecordId: Integer): Boolean;
begin
  if (fItemsEditor <> nil)
  then Result := fItemsEditor.LocateKey(RecordId)
  else Result := False;
end;

{ == Subitems ================================================================== }

function TRDbTreeLoader.SubitemsDataSet: TDataSet;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.DataSet
  else Result := nil;
end;

function TRDbTreeLoader.SubitemsDataSetIsNotEmpty: Boolean;
begin
  Result := (fSubitemsEditor <> nil) and fSubitemsEditor.DataSetIsNotEmply;
end;

function TRDbTreeLoader.SubitemsDataSetIsOpen(const CheckState: Boolean = True): Boolean;
begin
  if CheckState
  then Result := (fSubitemsEditor <> nil) and fSubitemsEditor.DataSetIsOpened
  else Result := (fSubitemsEditor <> nil)
    and (not fSubitemsEditor.CheckDataSetState or fSubitemsEditor.DataSetIsOpened);
end;

function TRDbTreeLoader.SubitemsDataSetIsOwner: Boolean;
begin
  Result := (fSubitemsEditor <> nil) and fSubitemsEditor.OwnerFieldIsPresent;
end;

function TRDbTreeLoader.SubitemsIsLoaded(const CheckState: Boolean = True): Boolean;
begin
  Result := (fTreeView <> nil)
    and GroupsDataSetIsOpen(CheckState) and ItemsDataSetIsOpen(CheckState) and SubitemsDataSetIsOpen(CheckState)
    and (fLoadMode in [lmAll, lmAllParam, lmSubitemsParam]);
end;

function TRDbTreeLoader.SubitemsIsLinked: Boolean;
begin
  Result := fSubitemsIsLinked;
end;

function TRDbTreeLoader.GetSubitemsKeyField: TField;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.GetKeyField
  else Result := nil;
end;

function TRDbTreeLoader.GetSubitemsKeyValue: Integer;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.GetKeyValue
  else Result := intDisable;
end;

function TRDbTreeLoader.GetSubitemsOwnerField: TField;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.GetOwnerField
  else Result := nil;
end;

function TRDbTreeLoader.GetSubitemsOwnerValue: Integer;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.GetOwnerValue
  else Result := intDisable;
end;

function TRDbTreeLoader.GetSubitemsNodeImage(const Selected: Boolean): Integer;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.GetNodeImage(Selected)
  else Result := intDisable;
end;

function TRDbTreeLoader.GetSubitemsNodeText: string;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.GetNodeText
  else Result := EmptyStr;
end;

function TRDbTreeLoader.GetSubitemsNodeSort: string;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.GetNodeSort
  else Result := EmptyStr;
end;

function TRDbTreeLoader.SubitemsDataSetLocate(const RecordId: Integer): Boolean;
begin
  if (fSubitemsEditor <> nil)
  then Result := fSubitemsEditor.LocateKey(RecordId)
  else Result := False;
end;

{ == Data Set ================================================================== }

function TRDbTreeLoader.OpenDataSets: Boolean;
begin
  Result := False;
  if Assigned(fOpenDataSets) then
    fOpenDataSets(Self, Result)
  else begin
    if (GroupsDataSet <> nil) then
    begin
      if GroupsDataSet.Active then GroupsDataSet.Close;
      GroupsDataSet.Open;
      Result := GroupsDataSet.Active;
      if Result and (ItemsDataSet <> nil) then
      begin
        if ItemsDataSet.Active then ItemsDataSet.Close;
        ItemsDataSet.Open;
        Result := ItemsDataSet.Active;
        if Result and (SubitemsDataSet <> nil) then
        begin
          if SubitemsDataSet.Active then SubitemsDataSet.Close;
          SubitemsDataSet.Open;
          Result := SubitemsDataSet.Active;
        end;
      end;
    end;
  end;
end;

procedure TRDbTreeLoader.CloseDataSets;
begin
  if Assigned(fCloseDataSets) then fCloseDataSets(Self)
  else begin
    if SubitemsDataSetIsOpen and (SubitemsDataSet.Owner = Owner) then SubitemsDataSet.Close;
    if ItemsDataSetIsOpen and (ItemsDataSet.Owner = Owner) then ItemsDataSet.Close;
    if GroupsDataSetIsOpen and (GroupsDataSet.Owner = Owner) then GroupsDataSet.Close;
  end;
end;

function TRDbTreeLoader.NodeLocate(const Node: TTreeNode): Boolean;
begin
  Result := False;
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    case TNodeData(Node.Data)^.NodeType of
      ntGroup: Result := GroupsDataSetLocate(TNodeData(Node.Data)^.RecordId);
      ntItem:  Result := ItemsDataSetLocate(TNodeData(Node.Data)^.RecordId);
      ntSubitem:  Result := SubitemsDataSetLocate(TNodeData(Node.Data)^.RecordId);
    end;
  end;
end;

function TRDbTreeLoader.GetNodeRecordData(const Node: TTreeNode): TRecordData;
begin
  Result := nil;
  if (Node <> nil) and (Node.Data <> nil) then
  begin
    case TNodeData(Node.Data)^.NodeType of
      ntGroup:   if GroupsDataSetLocate(TNodeData(Node.Data)^.RecordId)
                 then Result := GetRecordData(GroupsDataSet);
      ntItem:    if ItemsDataSetLocate(TNodeData(Node.Data)^.RecordId)
                 then Result := GetRecordData(ItemsDataSet);
      ntSubitem: if SubitemsDataSetLocate(TNodeData(Node.Data)^.RecordId)
                 then Result := GetRecordData(SubitemsDataSet);
    end;
  end;
end;

{ == Load Tree Structure ======================================================= }

function TRDbTreeLoader.BeforeLoadTree: Boolean;
begin
  Result := True;
  if Assigned(fBeforeLoadTree) then fBeforeLoadTree(Self, Result);
end;

function TRDbTreeLoader.AfterLoadTree: Boolean;
begin
  Result := True;
  if Assigned(fAfterLoadTree) then fAfterLoadTree(Self, Result);
end;

function TRDbTreeLoader.GetTreeLoadCount: Integer;
begin
  Result := 0;
  if RootIsLoaded then Inc(Result);
  if GroupsIsLoaded then Inc(Result, GroupsDataSet.RecordCount);
  if ItemsIsLoaded and not ItemsIsLinked then Inc(Result, ItemsDataSet.RecordCount);
  if SubitemsIsLoaded and not SubitemsIsLinked then Inc(Result, SubitemsDataSet.RecordCount);
end;

function TRDbTreeLoader.CheckNodeLoad(const NodeType: TNodeType; const RecordId: Integer): Boolean;
begin
  case fLoadMode of
    lmAll: Result := True;
    lmGroups: Result := NodeType in [ntRoot, ntGroup];
    lmItems: Result := NodeType in [ntRoot, ntGroup, ntItem];
    lmAllParam:
    begin
      Result := True;
      if Assigned(fCheckLoadNode) then
        fCheckLoadNode(Self, fLoadMode, NodeType, RecordId, Result);
    end;
    lmGroupsParam:
    begin
      Result := NodeType in [ntRoot, ntGroup];
      if Result and Assigned(fCheckLoadNode) then
        fCheckLoadNode(Self, fLoadMode, NodeType, RecordId, Result);
    end;
    lmItemsParam:
    begin
      if NodeType in [ntRoot, ntGroup] then Result := True
      else begin
        Result := NodeType = ntItem;
        if Assigned(fCheckLoadNode) then
          fCheckLoadNode(Self, fLoadMode, NodeType, RecordId, Result);
      end;
    end;
    lmSubitemsParam:
    begin
      if NodeType in [ntRoot, ntGroup, ntItem] then Result := True
      else begin
        Result := NodeType = ntSubitem;
        if Assigned(fCheckLoadNode) then
          fCheckLoadNode(Self, fLoadMode, NodeType, RecordId, Result);
      end;
    end;
    else Result := False;
  end;
end;

function TRDbTreeLoader.LoadTree: Boolean;
var
  GroupsDC, GroupsFS, ItemsDC, ItemsFS, SubitemsFS: Boolean;
  GroupsFL, ItemsFL, SubitemsFL: string;
  ItemsCount: Integer;
begin
  GroupsDC := False; ItemsDC := False;
  GroupsFS := False; ItemsFS := False; SubitemsFS := False;
  GroupsFL := EmptyStr; ItemsFL := EmptyStr; SubitemsFL := EmptyStr;
  fProgress := False;
  if fPrgsMode = pmAlways then
  begin
    ShowProgress(SMsgLoadDataWait, 100);
    fProgress := True;
  end;
  try
    try
      // Отключаем фильтр групп
      if GroupsIsLoaded then
      begin
        if not ItemsIsLoaded or not ItemsIsLinked then
        begin
          GroupsDataSet.DisableControls;
          GroupsDC := True;
        end;
        GroupsFS := GroupsDataSet.Filtered;
        GroupsFL := GroupsDataSet.Filter;
        GroupsDataSet.Filtered := False;
      end;
      try
        // Отключаем фильтр элементов
        if ItemsIsLoaded then
        begin
          if not SubitemsIsLoaded or not SubitemsIsLinked then
          begin
            ItemsDataSet.DisableControls;
            ItemsDC := True;
          end;
          ItemsFS := ItemsDataSet.Filtered;
          ItemsFL := ItemsDataSet.Filter;
          ItemsDataSet.Filtered := False;
        end;
        try
          // Отключаем фильтр подэлементов
          if SubitemsIsLoaded then
          begin
            SubitemsDataSet.DisableControls;
            SubitemsFS := SubitemsDataSet.Filtered;
            SubitemsFL := SubitemsDataSet.Filter;
            SubitemsDataSet.Filtered := False;
          end;
          try
            // Загружаем или обновляем данные
            Result := OpenDataSets;
            if Result then
            begin
              // Считаем количество шагов
              ItemsCount := GetTreeLoadCount;
              if fProgress then UpdateProgressMax(ItemsCount)
              else begin
                if (fPrgsMode = pmAuto) and (ItemsCount >= MinTreeLoadCount) then
                begin
                  ShowProgress(SMsgLoadDataWait, ItemsCount);
                  fProgress := True;
                end;
              end;
              // Загружаем дерево из базы данных
              LoadTreeStructure(nil);
            end;
          finally
            // Восстанавливаем фильтр подэлементов
            if SubitemsIsLoaded then
            begin
              SubitemsDataSet.Filter := SubitemsFL;
              SubitemsDataSet.Filtered := SubitemsFS;
              SubitemsDataSet.EnableControls;
            end;
          end;
        finally
          // Восстанавливаем фильтр элементов
          if ItemsIsLoaded then
          begin
            ItemsDataSet.Filter := ItemsFL;
            ItemsDataSet.Filtered := ItemsFS;
            if ItemsDC then ItemsDataSet.EnableControls;
          end;
        end;
      finally
        // Восстанавливаем фильтр групп
        if GroupsIsLoaded then
        begin
          GroupsDataSet.Filter := GroupsFL;
          GroupsDataSet.Filtered := GroupsFS;
          if GroupsDC then GroupsDataSet.EnableControls;
        end;
      end;
    except
      Result := False;
      raise;
    end;
  finally
    if fProgress then CloseProgress;
  end;
end;

function TRDbTreeLoader.ReloadNode(const BaseNode: TTreeNode): Boolean;
var
  GroupsDC, GroupsFS, ItemsFS, SubitemsFS: Boolean;
  GroupsFL, ItemsFL, SubitemsFL: string;
begin
  Result := True;
  GroupsDC := False;
  GroupsFS := False; ItemsFS := False; SubitemsFS := False;
  GroupsFL := EmptyStr; ItemsFL := EmptyStr; SubitemsFL := EmptyStr;
  fProgress := False;
  // Отключаем фильтр групп
  if GroupsIsLoaded then
  begin
    if not ItemsIsLoaded or not ItemsIsLinked then
    begin
      GroupsDataSet.DisableControls;
      GroupsDC := True;
    end;
    GroupsFS := GroupsDataSet.Filtered;
    GroupsFL := GroupsDataSet.Filter;
    GroupsDataSet.Filtered := False;
  end;
  try
    // Отключаем фильтр элементов
    if ItemsIsLoaded then
    begin
      ItemsDataSet.DisableControls;
      ItemsFS := ItemsDataSet.Filtered;
      ItemsFL := ItemsDataSet.Filter;
      ItemsDataSet.Filtered := False;
    end;
    try
      // Отключаем фильтр подэлементов
      if SubitemsIsLoaded then
      begin
        SubitemsDataSet.DisableControls;
        SubitemsFS := SubitemsDataSet.Filtered;
        SubitemsFL := SubitemsDataSet.Filter;
        SubitemsDataSet.Filtered := False;
      end;
      try
        try
          // Загружаем дерево из базы данных
          LoadTreeStructure(BaseNode);
        except
          Result := False;
          raise;
        end;
      finally
        // Восстанавливаем фильтр подэлементов
        if SubitemsIsLoaded then
        begin
          SubitemsDataSet.Filter := SubitemsFL;
          SubitemsDataSet.Filtered := SubitemsFS;
          SubitemsDataSet.EnableControls;
        end;
      end;
    finally
      // Восстанавливаем фильтр элементов
      if ItemsIsLoaded then
      begin
        ItemsDataSet.Filter := ItemsFL;
        ItemsDataSet.Filtered := ItemsFS;
        ItemsDataSet.EnableControls;
      end;
    end;
  finally
    // Восстанавливаем фильтр групп
    if GroupsIsLoaded then
    begin
      GroupsDataSet.Filter := GroupsFL;
      GroupsDataSet.Filtered := GroupsFS;
      if GroupsDC then GroupsDataSet.EnableControls;
    end;
  end;
end;

function TRDbTreeLoader.LoadTree(const ALoadMode: TTreeLoadMode): Boolean;
begin
  fLoadMode := ALoadMode;
  Result := LoadTree;
end;

procedure TRDbTreeLoader.LoadTreeStructure(const BaseNode: TTreeNode);

  procedure LoadSubitems(ItemNode: TTreeNode);
  var
    SubitemId: Integer;
  begin
    if SubitemsIsLinked then
    begin
      SubitemsDataSet.First;
      while not SubitemsDataSet.Eof do
      begin
        SubitemId := GetSubitemsKeyValue;
        if not fSubitemsEditor.RecordIsBlocked and CheckNodeLoad(ntSubitem, SubitemId) then
          fTreeView.CreateTypeNode(ItemNode, ntSubitem, SubitemId,
            GetSubitemsNodeImage(False), GetSubitemsNodeImage(True), GetSubitemsNodeText, GetSubitemsNodeSort);
        SubitemsDataSet.Next;
      end;
    end
    else begin
      SubitemsDataSet.Filter := SqlConcatBr(Format(fltFieldId, [GetSubitemsOwnerField.FieldName, fTreeView.GetNodeId(ItemNode)]),
        fSubitemsEditor.BaseFilter, sqlAnd);
      SubitemsDataSet.FindFirst;
      while SubitemsDataSet.Found do
      begin
        SubitemId := GetSubitemsKeyValue;
        if not fSubitemsEditor.RecordIsBlocked and CheckNodeLoad(ntSubitem, SubitemId) then
          fTreeView.CreateTypeNode(ItemNode, ntSubitem, SubitemId,
            GetSubitemsNodeImage(False), GetSubitemsNodeImage(True), GetSubitemsNodeText, GetSubitemsNodeSort);
        if fProgress then UpdateProgressStep(1);
        SubitemsDataSet.FindNext;
      end;
    end;
  end;

  procedure LoadItems(GroupNode: TTreeNode);
  var
    ItemId: Integer;
    ItemNode: TTreeNode;
  begin
    if ItemsIsLinked then
    begin
      ItemsDataSet.First;
      while not ItemsDataSet.Eof do
      begin
        ItemId := GetItemsKeyValue;
        if not fItemsEditor.RecordIsBlocked and CheckNodeLoad(ntItem, ItemId) then
        begin
          ItemNode := fTreeView.CreateTypeNode(GroupNode, ntItem, ItemId,
            GetItemsNodeImage(False), GetItemsNodeImage(True), GetItemsNodeText, GetItemsNodeSort);
          if SubitemsIsLoaded then LoadSubitems(ItemNode);
        end;
        ItemsDataSet.Next;
      end;
    end
    else begin
      ItemsDataSet.Filter := SqlConcatBr(Format(fltFieldId, [GetItemsOwnerField.FieldName, fTreeView.GetNodeId(GroupNode)]), fItemsEditor.BaseFilter, sqlAnd);
      ItemsDataSet.FindFirst;
      while ItemsDataSet.Found do
      begin
        ItemId := GetItemsKeyValue;
        if not fItemsEditor.RecordIsBlocked and CheckNodeLoad(ntItem, ItemId) then
        begin
          ItemNode := fTreeView.CreateTypeNode(GroupNode, ntItem, ItemId,
            GetItemsNodeImage(False), GetItemsNodeImage(True), GetItemsNodeText, GetItemsNodeSort);
          if fProgress then UpdateProgressStep(1);
          if SubitemsIsLoaded then LoadSubitems(ItemNode);
        end
        else begin
          if fProgress then UpdateProgressStep(1);
        end;
        ItemsDataSet.FindNext;
      end;
    end;
  end;

  procedure LoadSubGroups(GroupNode: TTreeNode);
  var
    SubId: Integer;
    SubNode: TTreeNode;
    SubBmk: TBookmark;
    SubFilter: string;
  begin
    SubFilter := SqlConcatBr(Format(fltFieldId, [GetGroupsOwnerField.FieldName, fTreeView.GetNodeId(GroupNode)]),
      fGroupsEditor.BaseFilter, sqlAnd);
    GroupsDataSet.Filter := SubFilter;
    GroupsDataSet.FindFirst;
    while GroupsDataSet.Found do
    begin
      SubBmk := GroupsDataSet.GetBookmark;
      try
        SubId := GetGroupsKeyValue;
        if not fGroupsEditor.RecordIsBlocked and CheckNodeLoad(ntGroup, SubId) then
        begin
          SubNode := fTreeView.CreateTypeNode(GroupNode, ntGroup, SubId,
            GetGroupsNodeImage(False), GetGroupsNodeImage(True), GetGroupsNodeText, GetGroupsNodeSort);
          LoadSubGroups(SubNode);
          if ItemsIsLoaded then LoadItems(SubNode);
        end;
      finally
        GroupsDataSet.Filter := SubFilter;
        try
          GroupsDataSet.GotoBookmark(SubBmk);
        finally
          GroupsDataSet.FreeBookmark(SubBmk);
        end;
      end;
      if fProgress then UpdateProgressStep(1);
      GroupsDataSet.FindNext;
    end;
  end;

  procedure LoadRootGroups(RootNode: TTreeNode);
  var
    GroupId: Integer;
    GroupNode: TTreeNode;
    GroupBmk: TBookmark;
    GroupFilter: string;
  begin
    if GroupsDataSetIsOwner then
    begin
      GroupFilter := SqlConcatBr(Format(fltFieldNull, [GetGroupsOwnerField.FieldName]),
        fGroupsEditor.BaseFilter, sqlAnd);
      GroupsDataSet.Filter := GroupFilter;
      GroupsDataSet.FindFirst;
      while GroupsDataSet.Found do
      begin
        GroupBmk := GroupsDataSet.GetBookmark;
        try
          GroupId := GetGroupsKeyValue;
          if not fGroupsEditor.RecordIsBlocked and CheckNodeLoad(ntGroup, GroupId) then
          begin
            GroupNode := fTreeView.CreateTypeNode(RootNode, ntGroup, GroupId,
              GetGroupsNodeImage(False), GetGroupsNodeImage(True), GetGroupsNodeText, GetGroupsNodeSort);
            LoadSubGroups(GroupNode);
            if ItemsIsLoaded then LoadItems(GroupNode);
          end;
        finally
          GroupsDataSet.Filter := GroupFilter;
          try
            GroupsDataSet.GotoBookmark(GroupBmk);
          finally
            GroupsDataSet.FreeBookmark(GroupBmk);
          end;
        end;
        if fProgress then UpdateProgressStep(1);
        GroupsDataSet.FindNext;
      end;
    end
    else begin
      GroupsDataSet.First;
      while not GroupsDataSet.Eof do
      begin
        GroupId := GetGroupsKeyValue;
        if not fGroupsEditor.RecordIsBlocked and CheckNodeLoad(ntGroup, GroupId) then
        begin
          GroupNode := fTreeView.CreateTypeNode(RootNode, ntGroup, GroupId,
            GetGroupsNodeImage(False), GetGroupsNodeImage(True), GetGroupsNodeText, GetGroupsNodeSort);
          if ItemsIsLoaded then LoadItems(GroupNode);
        end;
        if fProgress then UpdateProgressStep(1);
        GroupsDataSet.Next;
      end;
    end;
  end;

var
  RootNode: TTreeNode;
begin
  // Включаем фильтрацию
  if GroupsIsLoaded then GroupsDataSet.Filtered := GroupsDataSetIsOwner;
  if ItemsIsLoaded then ItemsDataSet.Filtered := not ItemsIsLinked;
  if SubitemsIsLoaded then SubitemsDataSet.Filtered := not SubitemsIsLinked;
  // Если не укзана "базовая" нода, грузим все дерево
  if BaseNode = nil then
  begin
    RootNode := nil;
    if RootIsLoaded then
    begin
      RootNode := fTreeView.CreateTypeNode(nil, ntRoot, intDisable,
        GetRootNodeImage(False), GetRootNodeImage(True), GetRootNodeText, EmptyStr);
      if fProgress then UpdateProgressStep(1);
    end;
    if GroupsIsLoaded then LoadRootGroups(RootNode);
  end
  // ...иначе грузим только "детей" укзанной ноды
  else begin
    BaseNode.DeleteChildren;
    case fTreeView.GetNodeType(BaseNode) of
      ntRoot:  if GroupsIsLoaded then LoadRootGroups(BaseNode);
      ntGroup: if GroupsIsLoaded then
               begin
                 if GroupsDataSetIsOwner
                 then LoadSubGroups(BaseNode)
                 else if ItemsIsLoaded then LoadItems(BaseNode);
               end;
    end;
  end;
end;

{ == Check Rights ============================================================== }

function TRDbTreeLoader.NodeCanInserted(Node: TTreeNode; const NodeType: TNodeType; const CheckRights: Boolean): Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if CheckRights and Assigned(OnGetEditRights) then
    OnGetEditRights(Self, NodeType, etInsert, Rights);
  case NodeType of
    ntGroup: Result := Rights and GroupsIsLoaded(False);
    ntItem: Result := Rights and ItemsIsLoaded(False)
      and (fTreeView.GetNodeType(Node) in [ntGroup, ntItem]);
    ntSubitem: Result := Rights and SubitemsIsLoaded(False)
      and (fTreeView.GetNodeType(Node) in [ntGroup, ntItem, ntSubitem]);
    else Result := False;
  end;
end;

function TRDbTreeLoader.NodeCanCopied(Node: TTreeNode; const CheckRights: Boolean = True): Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if CheckRights and Assigned(OnGetEditRights) then
    OnGetEditRights(Self, fTreeView.GetNodeType(Node), etInsert, Rights);
  case fTreeView.GetNodeType(Node) of
    ntGroup: Result := Rights and GroupsIsLoaded(False) and fGroupsEditor.RecordCopyExists;
    ntItem: Result := Rights and ItemsIsLoaded(False) and fItemsEditor.RecordCopyExists;
    ntSubitem: Result := Rights and SubitemsIsLoaded(False) and fSubitemsEditor.RecordCopyExists;
    else Result := False;
  end;
end;

function TRDbTreeLoader.NodeCanOpened(Node: TTreeNode; const CheckRights: Boolean): Boolean;
var
  Rights: Boolean;
begin
  case fTreeView.GetNodeType(Node) of
    ntGroup:
      if GroupsIsLoaded(False) then
      begin
        if fGroupsEditor.OpenMode = omEdit
        then Result := NodeCanEdited(Node, CheckRights)
        else begin
          Rights := True;
          if CheckRights and (fGroupsEditor.OpenMode = omCheck) and Assigned(OnGetEditRights) then
            OnGetEditRights(Self, ntGroup, etView, Rights);
          Result := Rights;
        end;
      end
      else Result := False;
    ntItem:
      if ItemsIsLoaded(False) then
      begin
        if fItemsEditor.OpenMode = omEdit
        then Result := NodeCanEdited(Node, CheckRights)
        else begin
          Rights := True;
          if CheckRights and (fItemsEditor.OpenMode = omCheck) and Assigned(OnGetEditRights) then
            OnGetEditRights(Self, ntItem, etView, Rights);
          Result := Rights;
        end;
      end
      else Result := False;
    ntSubitem:
      if SubitemsIsLoaded(False) then
      begin
        if fSubitemsEditor.OpenMode = omEdit
        then Result := NodeCanEdited(Node, CheckRights)
        else begin
          Rights := True;
          if CheckRights and (fSubitemsEditor.OpenMode = omCheck) and Assigned(OnGetEditRights) then
            OnGetEditRights(Self, ntSubitem, etView, Rights);
          Result := Rights;
        end;
      end
      else Result := False;
    else Result := False;
  end;
end;

function TRDbTreeLoader.NodeCanEdited(Node: TTreeNode; const CheckRights: Boolean): Boolean;
var
  Rights: Boolean;
begin
  case fTreeView.GetNodeType(Node) of
    ntGroup:
      if GroupsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntGroup, etEdit, Rights);
        Result := Rights;
      end
      else Result := False;
    ntItem:
    begin
      if ItemsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntItem, etEdit, Rights);
        Result := Rights;
      end
      else Result := False;
    end;
    ntSubitem:
    begin
      if SubitemsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntSubitem, etEdit, Rights);
        Result := Rights;
      end
      else Result := False;
    end;
    else Result := False;
  end;
end;

function TRDbTreeLoader.NodeCanMoved(Node: TTreeNode; const CheckRights: Boolean): Boolean;
var
  Rights: Boolean;
begin
  case fTreeView.GetNodeType(Node) of
    ntGroup:
      if GroupsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntGroup, etMove, Rights);
        Result := Rights;
      end
      else Result := False;
    ntItem:
    begin
      if ItemsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntItem, etMove, Rights);
        Result := Rights;
      end
      else Result := False;
    end;
    ntSubitem:
    begin
      if SubitemsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntSubitem, etMove, Rights);
        Result := Rights;
      end
      else Result := False;
    end;
    else Result := False;
  end;
end;

function TRDbTreeLoader.NodeCanDeleted(Node: TTreeNode; const CheckRights: Boolean): Boolean;
var
  Rights: Boolean;
begin
  case fTreeView.GetNodeType(Node) of
    ntGroup:
      if GroupsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntGroup, etDelete, Rights);
        Result := Rights and (fEnableParentDelete or not fTreeView.Selected.HasChildren);
      end
      else Result := False;
    ntItem:
    begin
      if ItemsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntItem, etDelete, Rights);
        Result := Rights and (fEnableParentDelete or not fTreeView.Selected.HasChildren);
      end
      else Result := False;
    end;
    ntSubitem:
    begin
      if SubitemsIsLoaded(False) then
      begin
        Rights := True;
        if CheckRights and Assigned(OnGetEditRights) then
          OnGetEditRights(Self, ntSubitem, etDelete, Rights);
        Result := Rights and (fEnableParentDelete or not fTreeView.Selected.HasChildren);
      end
      else Result := False;
    end;
    else Result := False;
  end;
end;

{ == Find Owner and Move node ================================================== }

procedure TRDbTreeLoader.FindNodeOwner(Node: TTreeNode);
var
  OwnerNode: TTreeNode;
  OwnerId: Integer;
begin
  if NodeLocate(Node) then
  begin
    // Поиск владельца
    case TNodeData(Node.Data)^.NodeType of
      ntGroup:
      begin
        OwnerId := GetGroupsOwnerValue;
        OwnerNode := fTreeView.FindNode([ntGroup], OwnerId);
        if OwnerNode = nil then
          OwnerNode := fTreeView.FindNode([ntRoot], intDisable);
      end;
      ntItem:
      begin
        OwnerId := GetItemsOwnerValue;
        OwnerNode := fTreeView.FindNode([ntGroup], OwnerId);
        if OwnerNode = nil then
          raise ERDbEditorError.CreateFmt(SErrIdNotFound, [OwnerId]);
      end;
      ntSubitem:
      begin
        OwnerId := GetSubitemsOwnerValue;
        OwnerNode := fTreeView.FindNode([ntItem], OwnerId);
        if OwnerNode = nil then
          raise ERDbEditorError.CreateFmt(SErrIdNotFound, [OwnerId]);
      end;
      else Exit;
    end;
    // Если текущий владелец и найденный не совпадают - перемещаем и сортируем
    if OwnerNode <> Node.Parent then
    begin
      if OwnerNode = nil
      then Node.MoveTo(nil, naAdd)
      else Node.MoveTo(OwnerNode, naAddChild);
      fTreeView.Sort;
    end;
  end;
end;

{ == Edit ====================================================================== }

function TRDbTreeLoader.InsertNode(ParentNode: TTreeNode; const NodeType: TNodeType): Boolean;
var
  OwnerData: TRecordData;
begin
  Result := False;
  OwnerData := GetNodeRecordData(ParentNode);
  try
    case NodeType of
      ntGroup:   if (fGroupsEditor <> nil)
                 then Result := fGroupsEditor.InsertNode(Self, ParentNode, OwnerData, NodeType)
                 else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
      ntItem:    if (fItemsEditor <> nil)
                 then Result := fItemsEditor.InsertNode(Self, ParentNode, OwnerData, NodeType)
                 else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
      ntSubitem: if (fSubitemsEditor <> nil)
                 then Result := fSubitemsEditor.InsertNode(Self, ParentNode, OwnerData, NodeType)
                 else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    end;
  finally
    FreeRecordData(OwnerData);
  end;
end;

function TRDbTreeLoader.CopyNode(Node: TTreeNode): Boolean;
begin
  Result := False;
  if (Node = nil) or (Node.Data = nil) then
    raise ERDbEditorError.CreateFmt(SErrEditNodeIsNull, [Self.Name]);
  case TNodeData(Node.Data).NodeType of
    ntGroup:   if (fGroupsEditor <> nil)
               then Result := fGroupsEditor.CopyNode(Self, Node)
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    ntItem:    if (fItemsEditor <> nil)
               then Result := fItemsEditor.CopyNode(Self, Node)
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    ntSubitem: if (fSubitemsEditor <> nil)
               then Result := fSubitemsEditor.CopyNode(Self, Node)
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
  end;
end;

function TRDbTreeLoader.EditNode(Node: TTreeNode; const EnableEdit: Boolean): Boolean;
begin
  Result := False;
  if (Node = nil) or (Node.Data = nil) then
    raise ERDbEditorError.CreateFmt(SErrEditNodeIsNull, [Self.Name]);
  case TNodeData(Node.Data).NodeType of
    ntGroup:   if (fGroupsEditor <> nil)
               then Result := fGroupsEditor.EditNode(Self, Node, EnableEdit)
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    ntItem:    if (fItemsEditor <> nil) then
               begin
                 if ItemsIsLinked and (fGroupsEditor <> nil) and (Node.Parent <> nil)
                 then fGroupsEditor.FindNodeRecord(Node.Parent);
                 Result := fItemsEditor.EditNode(Self, Node, EnableEdit);
               end
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    ntSubitem: if (fSubitemsEditor <> nil) then
               begin
                 if SubitemsIsLinked and (fGroupsEditor <> nil) and (Node.Parent <> nil)
                 then fItemsEditor.FindNodeRecord(Node.Parent);
                 Result := fSubitemsEditor.EditNode(Self, Node, EnableEdit);
               end
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
  end;
end;

function TRDbTreeLoader.MoveNode(MovedNode, ParentNode: TTreeNode): Boolean;
begin
  Result := False;
  if (MovedNode = nil) or (MovedNode.Data = nil) then
    raise ERDbEditorError.CreateFmt(SErrEditNodeIsNull, [Self.Name]);
  case TNodeData(MovedNode.Data).NodeType of
    ntGroup:   if (fGroupsEditor <> nil)
               then Result := fGroupsEditor.MoveNode(MovedNode, ParentNode)
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    ntItem:    if (fItemsEditor <> nil) then
               begin
                 if ItemsIsLinked and (fGroupsEditor <> nil) and (MovedNode.Parent <> nil)
                 then fGroupsEditor.FindNodeRecord(MovedNode.Parent);
                 Result := fItemsEditor.MoveNode(MovedNode, ParentNode);
               end
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    ntSubitem: if (fSubitemsEditor <> nil) then
               begin
                 if SubitemsIsLinked and (fGroupsEditor <> nil) and (MovedNode.Parent <> nil)
                 then fItemsEditor.FindNodeRecord(MovedNode.Parent);
                 Result := fSubitemsEditor.MoveNode(MovedNode, ParentNode);
               end
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
  end;
end;

function TRDbTreeLoader.DeleteNode(Node: TTreeNode; const ShowDeleteQuery: Boolean): Boolean;
begin
  Result := False;
  if (Node = nil) or (Node.Data = nil) then
    raise ERDbEditorError.CreateFmt(SErrEditNodeIsNull, [Self.Name]);
  case TNodeData(Node.Data).NodeType of
    ntGroup:   if (fGroupsEditor <> nil)
               then Result := fGroupsEditor.DeleteNode(Node, ShowDeleteQuery)
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    ntItem:    if (fItemsEditor <> nil) then
               begin
                 if ItemsIsLinked and (fGroupsEditor <> nil) and (Node.Parent <> nil)
                 then fGroupsEditor.FindNodeRecord(Node.Parent);
                 Result := fItemsEditor.DeleteNode(Node, ShowDeleteQuery);
               end
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
    ntSubitem: if (fSubitemsEditor <> nil) then
               begin
                 if SubitemsIsLinked and (fGroupsEditor <> nil) and (Node.Parent <> nil)
                 then fItemsEditor.FindNodeRecord(Node.Parent);
                 Result := fSubitemsEditor.DeleteNode(Node, ShowDeleteQuery);
               end
               else raise ERDbEditorError.CreateFmt(SErrNodeEditorIsNull, [Self.Name]);
  end;
end;

end.
