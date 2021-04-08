unit RDbEditor;

interface

uses
  Classes, Db, Forms, SysUtils, ComCtrls,
  RDbData, RavTreeView;

type
  ERDbEditorError = class(Exception);

  TEditMode       = (etView, etInsert, etEdit, etMove, etModify, etDelete);
  TOpenMode       = (omAuto, omEdit, omView, omCheck);
  TExportMode     = (etExcel, etCsvFile);

  TGetEditTagNotifyEvent    = procedure (Sender: TObject; const Mode: TEditMode; var EditTag: Integer) of object;
  TGetEditNameNotifyEvent   = procedure (Sender: TObject; const Mode: TEditMode; var Name: string) of object;
  TGetEditRightsNotifyEvent = procedure (Sender: TObject; const Mode: TEditMode; var Enable: Boolean) of object;
  TSaveToLogNotifyEvent     = procedure (Sender: TObject; const EditTag: Integer; const Text: string) of object;
  TVarBooleanNotifyEvent    = procedure (Sender: TObject; var Value: Boolean) of object;
  TVarIntegerNotifyEvent    = procedure (Sender: TObject; var Value: Integer) of object;
  TVarStringNotifyEvent     = procedure (Sender: TObject; var Value: string) of object;
  TFormClassNotifyEvent     = procedure (Sender: TObject; var EditorClass: TFormClass) of object;
  TResultNotifyEvent        = procedure (Sender: TObject; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;
  TResultDataNotifyEvent    = procedure (Sender: TObject; OldData, NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;
  TEditorNotifyEvent        = procedure (Sender: TObject; Editor: TForm; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;
  TEditorDataNotifyEvent    = procedure (Sender: TObject; Editor: TForm; OldData, NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;
  TLogGetMsgNotifyEvent     = procedure (Sender: TObject; OldData, NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer; var Text: string) of object;
  TLogSaveNotifyEvent       = procedure (Sender: TObject; const EditTag: Integer; const Text: string) of object;

  TCustomModifyProc         = procedure (Sender: TObject; var Complete: Boolean) of object;

  TRDbEditor = class(TDataSource)
  private
    FKeyField: string;
    FOwnerField: string;
    FBlockField: string;
    FObjectName: string;
    FObjectDesc: string;
    FLogEnable: Boolean;
    FNewAppend: Boolean;
    FMoveBlockedRecords: Boolean;
    FOpenMode: TOpenMode;
    FOnGetEditTag: TGetEditTagNotifyEvent;
    FOnGetEditRights: TGetEditRightsNotifyEvent;
    FOnGetObjectName: TGetEditNameNotifyEvent;
    FOnGetObjectDesc: TGetEditNameNotifyEvent;
    FOnFormClass: TFormClassNotifyEvent;
    FOnBeforeShowEditor: TEditorNotifyEvent;
    FOnBeforeFreeEditor: TEditorNotifyEvent;
    FOnGetKey: TVarIntegerNotifyEvent;
    FOnFreeKey: TVarIntegerNotifyEvent;
    FOnCreateNewRecord: TResultNotifyEvent;
    FOnBeforePost: TEditorDataNotifyEvent;
    FOnAfterPost: TEditorDataNotifyEvent;
    FOnAfterPostLogged: TEditorDataNotifyEvent;
    FOnDeleteQuery: TVarStringNotifyEvent;
    FOnBeforeDelete: TResultDataNotifyEvent;
    FOnBlockRecord: TResultNotifyEvent;
    FOnAfterDelete: TResultDataNotifyEvent;
    FOnAfterDeleteLogged: TResultDataNotifyEvent;
    FOnGetInsertLogMsg: TLogGetMsgNotifyEvent;
    FOnGetChangeLogMsg: TLogGetMsgNotifyEvent;
    FOnGetDeleteLogMsg: TLogGetMsgNotifyEvent;
    FOnSaveToLog: TLogSaveNotifyEvent;
    FOnEndInsert: TNotifyEvent;
    FOnEndEdit: TNotifyEvent;
    FOnEndMove: TNotifyEvent;
    FOnEndModify: TNotifyEvent;
    FOnEndDelete: TNotifyEvent;
    FOnEndProcess: TNotifyEvent;
  protected
    // Editor
    function GetEditorClass: TFormClass;
    // Log Messages
    procedure LogOnInsert(const Mode: TEditMode; const EditTag: Integer;
      const NewData: TRecordData); virtual;
    procedure LogOnEdit(const Mode: TEditMode; const EditTag: Integer;
      const OldData, NewData: TRecordData); virtual;
    procedure LogOnDelete(const Mode: TEditMode; const EditTag: Integer;
      const OldData: TRecordData); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    // Operation Tags
    function CheckEditTag(const EditTag: Integer): Integer; virtual;
    function GetEditTag(const Mode: TEditMode): Integer; virtual;
    function GetObjectName(const Mode: TEditMode): string; virtual;
    function GetObjectDesc(const Mode: TEditMode): string; virtual;
    // Key Field
    function KeyFieldIsPresent: Boolean;
    function GetKeyField: TField;
    function GetKeyValue: Integer;
    function LocateKey(const KeyValue: Integer): Boolean;
    // Owner Field
    function OwnerFieldIsPresent: Boolean;
    function GetOwnerField: TField;
    function GetOwnerValue: Integer;
    // Block Field
    function BlockFieldIsPresent: Boolean;
    function GetBlockField: TField;
    function GetBlockValue: Boolean;
    // Record States
    function DataSetIsOpen: Boolean;
    function DataSetIsEmply: Boolean;
    function DataSetIsNotEmply: Boolean;
    function RecordCanModified: Boolean;
    function RecordCanInserted: Boolean;
    function RecordCanOpened: Boolean;
    function RecordCanEdited: Boolean;
    function RecordCanMoved: Boolean;
    function RecordCanDeleted: Boolean;
    // Edit DataSet
    function InsertRecord(const OwnerId: Integer = 0): Boolean; virtual;
    function EditRecord(const EnableEdit: Boolean = True): Boolean; virtual;
    function MoveRecord(const OwnerId: Integer): Boolean; virtual;
    function ModifyRecord(const EditTag: Integer; Proc: TCustomModifyProc): Boolean; virtual;
    function DeleteRecord(const ShowDeleteQuery: Boolean = True): Boolean; virtual;
    function DeleteCurrentRecord: Boolean; virtual;
    // Properties
    property KeyField: TField read GetKeyField;
    property KeyValue: Integer read GetKeyValue;
    property OwnerField: TField read GetOwnerField;
    property OwnerValue: Integer read GetOwnerValue;
  published
    property KeyFieldName: string read FKeyField write FKeyField;
    property OwnerFieldName: string read FOwnerField write FOwnerField;
    property BlockFieldName: string read FBlockField write FBlockField;
    property MoveBlockedRecords: Boolean read FMoveBlockedRecords write FMoveBlockedRecords default True;
    property ObjectName: string read FObjectName write FObjectName;
    property ObjectDesc: string read FObjectDesc write FObjectDesc;
    property LogEnable: Boolean read FLogEnable write FLogEnable default False;
    property NewRecordAppend: Boolean read FNewAppend write FNewAppend default True;
    property OpenMode: TOpenMode read FOpenMode write FOpenMode;
    property OnGetEditTag: TGetEditTagNotifyEvent read FOnGetEditTag write FOnGetEditTag;
    property OnGetEditRights: TGetEditRightsNotifyEvent read FOnGetEditRights write FOnGetEditRights;
    property OnGetObjectName: TGetEditNameNotifyEvent read FOnGetObjectName write FOnGetObjectName;
    property OnGetObjectDesc: TGetEditNameNotifyEvent read FOnGetObjectDesc write FOnGetObjectDesc;
    property OnGetEditorClass: TFormClassNotifyEvent read FOnFormClass write FOnFormClass;
    property OnBeforeShowEditor: TEditorNotifyEvent read FOnBeforeShowEditor write FOnBeforeShowEditor;
    property OnBeforeFreeEditor: TEditorNotifyEvent read FOnBeforeFreeEditor write FOnBeforeFreeEditor;
    property OnGetNewKey: TVarIntegerNotifyEvent read FOnGetKey write FOnGetKey;
    property OnFreeNewKey: TVarIntegerNotifyEvent read FOnFreeKey write FOnFreeKey;
    property OnCreateNewRecord: TResultNotifyEvent read FOnCreateNewRecord write FOnCreateNewRecord;
    property OnBeforePost: TEditorDataNotifyEvent read FOnBeforePost write FOnBeforePost;
    property OnAfterPost: TEditorDataNotifyEvent read FOnAfterPost write FOnAfterPost;
    property OnAfterPostLogged: TEditorDataNotifyEvent read FOnAfterPostLogged write FOnAfterPostLogged;
    property OnDeleteQuery: TVarStringNotifyEvent read FOnDeleteQuery write FOnDeleteQuery;
    property OnBeforeDelete: TResultDataNotifyEvent read FOnBeforeDelete write FOnBeforeDelete;
    property OnBlockRecord: TResultNotifyEvent read FOnBlockRecord write FOnBlockRecord;
    property OnAfterDelete: TResultDataNotifyEvent read FOnAfterDelete write FOnAfterDelete;
    property OnAfterDeleteLogged: TResultDataNotifyEvent read FOnAfterDeleteLogged write FOnAfterDeleteLogged;
    property OnExitInsert: TNotifyEvent read FOnEndInsert write FOnEndInsert;
    property OnExitEdit: TNotifyEvent read FOnEndEdit write FOnEndEdit;
    property OnExitMove: TNotifyEvent read FOnEndMove write FOnEndMove;
    property OnExitModify: TNotifyEvent read FOnEndModify write FOnEndModify;
    property OnExitDelete: TNotifyEvent read FOnEndDelete write FOnEndDelete;
    property OnExitProcess: TNotifyEvent read FOnEndProcess write FOnEndProcess;
    property OnGetInsertLogMsg: TLogGetMsgNotifyEvent read FOnGetInsertLogMsg write FOnGetInsertLogMsg;
    property OnGetChangeLogMsg: TLogGetMsgNotifyEvent read FOnGetChangeLogMsg write FOnGetChangeLogMsg;
    property OnGetDeleteLogMsg: TLogGetMsgNotifyEvent read FOnGetDeleteLogMsg write FOnGetDeleteLogMsg;
    property OnSaveToLog: TLogSaveNotifyEvent read FOnSaveToLog write FOnSaveToLog;
  end;

  TGetExportTagNotifyEvent  = procedure (Sender: TObject; const Mode: TExportMode; var ExportTag: Integer) of object;

  TRDbExportEditor = class(TRDbEditor)
  private
    FExcelCopyright: string;
    FExcelComment: string;
    FOnGetExportTag: TGetExportTagNotifyEvent;
    FOnGetExcelCopyright: TVarStringNotifyEvent;
    FOnGetExcelComment: TVarStringNotifyEvent;
    FOnGetExcelLogMsg: TVarStringNotifyEvent;
    FOnGetCsvFileLogMsg: TVarStringNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    // Operation Tags
    function  GetExportTag(const Mode: TExportMode): Integer; virtual;
    // Export procedures
    procedure ExportToExcel; virtual;
    procedure ExportToCsvFile; virtual;
  published
    property ExcelCopyright: string read FExcelCopyright write FExcelCopyright;
    property ExcelComments: string read FExcelComment write FExcelComment;
    property OnGetExportTag: TGetExportTagNotifyEvent read FOnGetExportTag write FOnGetExportTag;
    property OnGetExcelCopyright: TVarStringNotifyEvent read FOnGetExcelCopyright write FOnGetExcelCopyright;
    property OnGetExcelComment: TVarStringNotifyEvent read FOnGetExcelComment write FOnGetExcelComment;
    property OnGetExcelLogMsg: TVarStringNotifyEvent read FOnGetExcelLogMsg write FOnGetExcelLogMsg;
    property OnGetCsvFileLogMsg: TVarStringNotifyEvent read FOnGetCsvFileLogMsg write FOnGetCsvFileLogMsg;
  end;

  TRDbTreeEditor = class(TComponent)
  private
    // TreeView
    FTreeView: TRTreeView;
    FLoadMode: TTreeNodeMode;
    FSelectMode: TTreeNodeMode;
    FExpandMode: TAutoExpandMode;
    FOnItemLoaded: TVarBooleanNotifyEvent;
    FOnOpenDataSets: TVarBooleanNotifyEvent;
    FOnEndLoad: TNotifyEvent;
    // RootNode
    FRootNodeName: string;
    FRootNodeImage: Integer;
    FRootNodeSelImage: Integer;
    // Groups
    FGroupsDataSet: TDataSet;
    FGroupsKeyField: string;
    FGroupsOwnerField: string;
    FGroupsNameField: string;
    FGroupsNoteField: string;
    FGroupsImage: Integer;
    FGroupsSelImage: Integer;
    FOnGetGroupsImage: TVarIntegerNotifyEvent;
    FOnGetGroupsSelImage: TVarIntegerNotifyEvent;
    FOnGetGroupsNameValue: TVarStringNotifyEvent;
    FOnGetGroupsNoteValue: TVarStringNotifyEvent;
    FOnGetGroupsNodeText: TVarStringNotifyEvent;
    // Items
    FItemsDataSet: TDataSet;
    FMasterDetailItems: Boolean;
    FItemsKeyField: string;
    FItemsOwnerField: string;
    FItemsNameField: string;
    FItemsNoteField: string;
    FItemsImage: Integer;
    FItemsSelImage: Integer;
    FOnGetItemsImage: TVarIntegerNotifyEvent;
    FOnGetItemsSelImage: TVarIntegerNotifyEvent;
    FOnGetItemsNameValue: TVarStringNotifyEvent;
    FOnGetItemsNoteValue: TVarStringNotifyEvent;
    FOnGetItemsNodeText: TVarStringNotifyEvent;
    // Procedures
    procedure SetTreeView(Value: TRTreeView);
    procedure SetGroupsDataSet(Value: TDataSet);
    procedure SetItemsDataSet(Value: TDataSet);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function  GetLoadCount: Integer; virtual;
    function  OpenDataSets: Boolean; virtual;
    procedure LoadTreeStructure; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Groups
    function GroupsDataSetIsOpen: Boolean;
    function GroupsDataSetIsNotEmpty: Boolean;
    function GroupsOwnerFieldIsPresent: Boolean;
    function GetGroupsKeyField: TField;
    function GetGroupsKeyValue: Integer;
    function GetGroupsOwnerField: TField;
    function GetGroupsOwnerValue: Integer;
    function GetGroupsNameField: TField;
    function GetGroupsNameValue: string;
    function GetGroupsNoteField: TField;
    function GetGroupsNoteValue: string;
    function GetGroupsNodeImage: Integer;
    function GetGroupsNodeSelImage: Integer;
    function GetGroupsNodeText: string;
    function LocateGroupsKey(const KeyValue: Integer): Boolean;
    // Items
    function ItemsDataSetIsOpen: Boolean;
    function ItemsDataSetIsNotEmpty: Boolean;
    function ItemsOwnerFieldIsPresent: Boolean;
    function GetItemsKeyField: TField;
    function GetItemsKeyValue: Integer;
    function GetItemsOwnerField: TField;
    function GetItemsOwnerValue: Integer;
    function GetItemsNameField: TField;
    function GetItemsNameValue: string;
    function GetItemsNoteField: TField;
    function GetItemsNoteValue: string;
    function GetItemsNodeImage: Integer;
    function GetItemsNodeSelImage: Integer;
    function GetItemsNodeText: string;
    function LocateItemsKey(const KeyValue: Integer): Boolean;
    // TreeView
    function LoadTree: Boolean; virtual;
    function LocateNode(Node: TTreeNode): Boolean;
  published
    // TreeView
    property TreeView: TRTreeView read FTreeView write SetTreeView;
    property LoadMode: TTreeNodeMode read FLoadMode write FLoadMode;
    property SelectMode: TTreeNodeMode read FSelectMode write FSelectMode;
    property ExpandMode: TAutoExpandMode read FExpandMode write FExpandMode;
    property OnItemLoaded: TVarBooleanNotifyEvent read FOnItemLoaded write FOnItemLoaded;
    property OnPrepareLoad: TVarBooleanNotifyEvent read FOnOpenDataSets write FOnOpenDataSets;
    property OnExitLoad: TNotifyEvent read FOnEndLoad write FOnEndLoad;
    // RootNode
    property RootNodeName: string read FRootNodeName write FRootNodeName;
    property RootNodeImage: Integer read FRootNodeImage write FRootNodeImage;
    property RootNodeSelImage: Integer read FRootNodeSelImage write FRootNodeSelImage;
    // Groups
    property GroupsKeyField: string read FGroupsKeyField write FGroupsKeyField;
    property GroupsOwnerField: string read FGroupsOwnerField write FGroupsOwnerField;
    property GroupsNameField: string read FGroupsNameField write FGroupsNameField;
    property GroupsNoteField: string read FGroupsNoteField write FGroupsNoteField;
    property GroupsImage: Integer read FGroupsImage write FGroupsImage default -1;
    property GroupsSelImage: Integer read FGroupsSelImage write FGroupsSelImage default -1;
    property GroupsDataSet: TDataSet read FGroupsDataSet write SetGroupsDataSet;
    property OnGetGroupsImage: TVarIntegerNotifyEvent read FOnGetGroupsImage write FOnGetGroupsImage;
    property OnGetGroupsSelImage: TVarIntegerNotifyEvent read FOnGetGroupsSelImage write FOnGetGroupsSelImage;
    property OnGetGroupsNameValue: TVarStringNotifyEvent read FOnGetGroupsNameValue write FOnGetGroupsNameValue;
    property OnGetGroupsNoteValue: TVarStringNotifyEvent read FOnGetGroupsNoteValue write FOnGetGroupsNoteValue;
    property OnGetGroupsNodeText: TVarStringNotifyEvent read FOnGetGroupsNodeText write FOnGetGroupsNodeText;
    // Items
    property ItemsKeyField: string read FItemsKeyField write FItemsKeyField;
    property ItemsOwnerField: string read FItemsOwnerField write FItemsOwnerField;
    property ItemsNameField: string read FItemsNameField write FItemsNameField;
    property ItemsNoteField: string read FItemsNoteField write FItemsNoteField;
    property ItemsImage: Integer read FItemsImage write FItemsImage default -1;
    property ItemsSelImage: Integer read FItemsSelImage write FItemsSelImage default -1;
    property ItemsDataSet: TDataSet read FItemsDataSet write SetItemsDataSet;
    property ItemsMasterDetail: Boolean read FMasterDetailItems write FMasterDetailItems default False;
    property OnGetItemsImage: TVarIntegerNotifyEvent read FOnGetItemsImage write FOnGetItemsImage;
    property OnGetItemsSelImage: TVarIntegerNotifyEvent read FOnGetItemsSelImage write FOnGetItemsSelImage;
    property OnGetItemsNameValue: TVarStringNotifyEvent read FOnGetItemsNameValue write FOnGetItemsNameValue;
    property OnGetItemsNoteValue: TVarStringNotifyEvent read FOnGetItemsNoteValue write FOnGetItemsNoteValue;
    property OnGetItemsNodeText: TVarStringNotifyEvent read FOnGetItemsNodeText write FOnGetItemsNodeText;
  end;

implementation

uses
  Controls, RxVerInf, RVclUtils, RMsgRu, RDbConst, RRssConst, RDialogs, RStrUtils,
  RExpExcel, RFileExport, RExHandlers, RProgress;

const
  IntegerDataTypes = [ftSmallint, ftInteger, ftWord, ftLargeint];
  BooleanDataTypes = [ftBoolean];
  EnabledDbStates  = [dsBrowse];

resourcestring
  SErrDataSetNotDefined         = '%s: DataSet not defined!';
  SErrEditorClassNotDefined     = '%s: Record-editor form class not defined!'#13'See OnGetEditorClass event.';

{ == TRDbEditor ================================================================ }

constructor TRDbEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoEdit := False;
  FObjectName := EmptyStr;
  FObjectDesc := EmptyStr;
  FKeyField := EmptyStr;
  FOwnerField := EmptyStr;
  FBlockField := EmptyStr;
  FMoveBlockedRecords := True;
  FLogEnable := False;
  FNewAppend := True;
  FOpenMode := omAuto;
end;

// Operation Tags and Logs -----------------------------------------------------

function TRDbEditor.CheckEditTag(const EditTag: Integer): Integer;
begin
  if EditTag > 0 then Result := EditTag
  else begin
    Result := tagError;
    ErrorBox(Format(EBadOperationTag, [EditTag]));
  end;
end;

function TRDbEditor.GetEditTag(const Mode: TEditMode): Integer;
begin
  if Assigned(DataSet) and (DataSet.Tag > 0)
  then Result := DataSet.Tag
  else Result := Self.Tag;
  if Assigned(FOnGetEditTag) then FOnGetEditTag(Self, Mode, Result);
  Result := CheckEditTag(Result);
end;

function TRDbEditor.GetObjectName(const Mode: TEditMode): string;
begin
  if FObjectName <> EmptyStr
  then Result := FObjectName
  else if Assigned(DataSet)
       then Result := DataSet.Name
       else Result := EmptyStr;
  if Assigned(FOnGetObjectName)
  then FOnGetObjectName(Self, Mode, Result);
end;

function TRDbEditor.GetObjectDesc(const Mode: TEditMode): string;
begin
  if FObjectDesc <> EmptyStr
  then Result := FObjectDesc
  else if Assigned(Owner) and (Owner is TForm)
       then Result := TForm(Owner).Caption
       else Result := EmptyStr;
  if Assigned(FOnGetObjectDesc)
  then FOnGetObjectDesc(Self, Mode, Result);
end;

// Log Messages ----------------------------------------------------------------

procedure TRDbEditor.LogOnInsert(const Mode: TEditMode; const EditTag: Integer;
  const NewData: TRecordData);
var
  LogMsg: string;
begin
  if FLogEnable and Assigned(FOnSaveToLog) then
  begin
    LogMsg := Format(SLogNewRecord,
      [GetObjectDesc(Mode), GetObjectName(Mode),
       GetRecordText(NewData, [rtVisible, rtIndex, rtID, rtAnyTag])]);
    if Assigned(FOnGetInsertLogMsg) then
      FOnGetInsertLogMsg(Self, nil, NewData, Mode, EditTag, LogMsg);
    FOnSaveToLog(Self, EditTag, LogMsg);
  end;
end;

procedure TRDbEditor.LogOnEdit(const Mode: TEditMode; const EditTag: Integer;
  const OldData, NewData: TRecordData);
var
  LogMsg, Changes: string;
begin
  if FLogEnable and Assigned(FOnSaveToLog) then
  begin
    Changes := GetRecordChanges(OldData, NewData, []);
    if Changes <> EmptyStr
    then LogMsg := Format(SLogEditRecord,
      [GetObjectDesc(Mode), GetObjectName(Mode),
       GetRecordChanges(NewData, OldData, [rtIndex, rtID, rtAnyTag]), Changes])
    else LogMsg := Format(SLogEditRecordEmpty,
      [GetObjectDesc(Mode), GetObjectName(Mode),
       GetRecordChanges(NewData, OldData, [rtIndex, rtID, rtAnyTag])]);
    if Assigned(FOnGetChangeLogMsg) then
      FOnGetChangeLogMsg(Self, OldData, NewData, Mode, EditTag, LogMsg);
    FOnSaveToLog(Self, EditTag, LogMsg);
  end;
end;

procedure TRDbEditor.LogOnDelete(const Mode: TEditMode; const EditTag: Integer;
  const OldData: TRecordData);
var
  LogMsg: string;
begin
  if FLogEnable and Assigned(FOnSaveToLog) then
  begin
    LogMsg := Format(SLogDeleteRecord,
      [GetObjectDesc(Mode), GetObjectName(Mode),
      GetRecordText(OldData, [rtVisible, rtIndex, rtID, rtAnyTag])]);
    if Assigned(FOnGetDeleteLogMsg) then
      FOnGetDeleteLogMsg(Self, OldData, nil, Mode, EditTag, LogMsg);
    FOnSaveToLog(Self, EditTag, LogMsg);
  end;
end;

// Key Field -------------------------------------------------------------------

function TRDbEditor.KeyFieldIsPresent: Boolean;
var
  KeyFld: TField;
begin
  KeyFld := GetKeyField;
  Result := (KeyFld <> nil) and (KeyFld.DataType in IntegerDataTypes);
end;

function TRDbEditor.GetKeyField: TField;
begin
  Result := nil;
  if DataSet <> nil then Result := DataSet.FindField(FKeyField);
end;

function TRDbEditor.GetKeyValue: Integer;
var
  KeyFld: TField;
begin
  KeyFld := GetKeyField;
  if (KeyFld <> nil) and (KeyFld.DataType in IntegerDataTypes)
    and DataSet.Active and not DataSet.IsEmpty and not KeyFld.IsNull
  then Result := KeyFld.AsInteger
  else Result := intDisable;
end;

function TRDbEditor.LocateKey(const KeyValue: Integer): Boolean;
begin
  Result := KeyFieldIsPresent and DataSet.Active
    and DataSet.Locate(FKeyField, KeyValue, []);
end;

// Owner Field -----------------------------------------------------------------

function TRDbEditor.OwnerFieldIsPresent: Boolean;
var
  OwnerFld: TField;
begin
  OwnerFld := GetOwnerField;
  Result := (OwnerFld <> nil) and (OwnerFld.DataType in IntegerDataTypes);
end;

function TRDbEditor.GetOwnerField: TField;
begin
  Result := nil;
  if DataSet <> nil then Result := DataSet.FindField(FOwnerField);
end;

function TRDbEditor.GetOwnerValue: Integer;
var
  OwnerFld: TField;
begin
  OwnerFld := GetOwnerField;
  if (OwnerFld <> nil) and (OwnerFld.DataType in IntegerDataTypes)
    and DataSet.Active and not DataSet.IsEmpty and not OwnerFld.IsNull
  then Result := OwnerFld.AsInteger
  else Result := intDisable;
end;

// Block Field -----------------------------------------------------------------

function TRDbEditor.BlockFieldIsPresent: Boolean;
var
  BlockFld: TField;
begin
  BlockFld := GetBlockField;
  Result := (BlockFld <> nil) and (BlockFld.DataType in BooleanDataTypes);
end;

function TRDbEditor.GetBlockField: TField;
begin
  Result := nil;
  if DataSet <> nil then Result := DataSet.FindField(FBlockField);
end;

function TRDbEditor.GetBlockValue: Boolean;
var
  BlockFld: TField;
begin
  BlockFld := GetBlockField;
  if Assigned(BlockFld) and (BlockFld.DataType in BooleanDataTypes)
    and DataSet.Active and not DataSet.IsEmpty
  then Result := BlockFld.AsBoolean
  else Result := False;
end;

// Record States ---------------------------------------------------------------

function TRDbEditor.DataSetIsOpen: Boolean;
begin
  Result := (DataSet <> nil) and DataSet.Active;
end;

function TRDbEditor.DataSetIsEmply: Boolean;
begin
  Result := (DataSet <> nil) and DataSet.Active and DataSet.IsEmpty;
end;

function TRDbEditor.DataSetIsNotEmply: Boolean;
begin
  Result := (DataSet <> nil) and DataSet.Active and not DataSet.IsEmpty;
end;

function TRDbEditor.RecordCanModified: Boolean;
var
  BlockFld: TField;
begin
  Result := (DataSet <> nil) and DataSet.Active
    and not DataSet.IsEmpty and (DataSet.State in EnabledDbStates);
  if Result then
  begin
    BlockFld := GetBlockField;
    Result := not (Assigned(BlockFld) and (BlockFld.DataType in BooleanDataTypes)
      and BlockFld.AsBoolean);
  end;
end;

function TRDbEditor.RecordCanInserted: Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if Assigned(OnGetEditRights) then OnGetEditRights(Self, etInsert, Rights);
  Result := Rights and DataSetIsOpen and (DataSet.State in EnabledDbStates);
end;

function TRDbEditor.RecordCanOpened: Boolean;
var
  Rights: Boolean;
begin
  if FOpenMode = omEdit
  then Result := RecordCanEdited
  else begin
    Rights := True;
    if (FOpenMode = omCheck) and Assigned(OnGetEditRights)
    then OnGetEditRights(Self, etView, Rights);
    Result := Rights and DataSetIsNotEmply and (DataSet.State in EnabledDbStates);
  end;
end;

function TRDbEditor.RecordCanEdited: Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if Assigned(OnGetEditRights) then OnGetEditRights(Self, etEdit, Rights);
  Result := Rights and RecordCanModified;
end;

function TRDbEditor.RecordCanMoved: Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if Assigned(OnGetEditRights) then OnGetEditRights(Self, etMove, Rights);
  if FMoveBlockedRecords
  then Result := Rights and DataSetIsNotEmply and (DataSet.State in EnabledDbStates)
  else Result := Rights and RecordCanModified;
end;

function TRDbEditor.RecordCanDeleted: Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if Assigned(OnGetEditRights) then OnGetEditRights(Self, etDelete, Rights);
  Result := Rights and RecordCanModified;
end;

// Editor ----------------------------------------------------------------------

function TRDbEditor.GetEditorClass: TFormClass;
begin
  Result := nil;
  if Assigned(FOnFormClass) then FOnFormClass(Self, Result)
  else raise ERDbEditorError.CreateFmt(SErrEditorClassNotDefined, [Self.Name]);
end;

// Edit DataSet ----------------------------------------------------------------

function TRDbEditor.InsertRecord(const OwnerId: Integer = 0): Boolean;
var
  NewData: TRecordData;
  NewId: Integer;
  Editor: TForm;
begin
  Result := False;
  // Проверка корректности указателей
  if DataSet = nil then
    raise ERDbEditorError.CreateFmt(SErrDataSetNotDefined, [Self.Name]);
  // Запускаем подготовку операции
  StartWait;
  ShowInStatusBar(SMsgPrepareOperation);
  Result := True;
  try
    try
      NewId := intDisable;
      // Создаем экземпляр редактора записи
      Editor := GetEditorClass.Create(Self.Owner);
      try
        // Вызываем режим редактирования
        if FNewAppend then DataSet.Append else DataSet.Insert;
        // Генерируем новый индекс записи
        if KeyFieldIsPresent and Assigned(FOnGetKey) then
        begin
          FOnGetKey(Self, NewId);
          GetKeyField.AsInteger := NewId;
        end;
        // Если указано поле "владельца" заполняем его
        if OwnerFieldIsPresent then
        begin
          if OwnerId > 0
          then GetOwnerField.AsInteger := OwnerId
          else GetOwnerField.Clear;
        end;
        // Выполняем процедуры при создании новой записи
        if Assigned(FOnCreateNewRecord) then
          FOnCreateNewRecord(Self, etInsert, GetEditTag(etInsert), Result);
        // Выполняем процедуры перед показом редактора
        if Assigned(FOnBeforeShowEditor) then
          FOnBeforeShowEditor(Self, Editor, etInsert, GetEditTag(etInsert), Result);
        // Возвращаем курсор
        ShowInStatusBar(EmptyStr);
        StopWait;
        // Показываем редактор записи
        Result := Result and (Editor.ShowModal = mrOk);
        // Если редактирование завершено, сохраняем данные
        if Result then
        begin
          // Запуск процедуры сохранения изменений
          StartWait;
          ShowInStatusBar(SMsgSaveDataWait);
          // Делаем "снимок" новых данных
          NewData := GetRecordData(DataSet);
          try
            // Выполняем процедуры перед сохранением
            if Assigned(FOnBeforePost) then
              FOnBeforePost(Self, Editor, nil, NewData,
                etInsert, GetEditTag(etInsert), Result);
            // Сохраняем данные
            if Result then
            begin
              DataSet.Post;
              // Выполняем процедуры после сохранения
              if Assigned(FOnAfterPost) then
                FOnAfterPost(Self, Editor, nil, NewData,
                  etInsert, GetEditTag(etInsert), Result);
              // Протоколируем изменения, если необходимо
              if Result then
              begin
                try
                  LogOnInsert(etInsert, GetEditTag(etInsert), NewData);
                finally
                  if Assigned(FOnAfterPostLogged) then
                    FOnAfterPostLogged(Self, Editor, nil, NewData,
                      etInsert, GetEditTag(etInsert), Result);
                  if Result and Assigned(FOnEndInsert) then FOnEndInsert(Self);
                end;
              end;
            end;
          finally
            // Освобождаем "новый" снимок
            FreeRecordData(NewData);
          end;
        end
        else begin
          // Запуск процедуры отката изменений
          StartWait;
          ShowInStatusBar(SMsgCancelChanges);
        end;
      finally
        try
          try
            // Откатываем любые несохраненные изменения
            if DataSet.State in [dsInsert, dsEdit] then DataSet.Cancel;
          finally
            // Удаляем блокировку ключа
            if Assigned(FOnFreeKey) and (NewId > intDisable) then
              FOnFreeKey(Self, NewId);
          end;
        finally
          // Выполняем процедуры перед удалением редактора
          try
            if Assigned(FOnBeforeFreeEditor) then
              FOnBeforeFreeEditor(Self, Editor, etInsert, GetEditTag(etInsert), Result);
          finally
            // Удаляем редактор
            Editor.Free;
          end;
        end;
      end;
    except
      // В случае ошибки выставляем флаг ошибки
      Result := False;
      raise
    end;
  finally
    try
      // Вызываем процедуры после всех действий
      if Assigned(FOnEndProcess) then FOnEndProcess(Self);
    finally
      // Убираем сообщение из системного статус-бара
      if Result then ShowInStatusBar(EmptyStr);
      // Возвращаем курсор
      ExitWait;
    end;
  end;
end;

function TRDbEditor.EditRecord(const EnableEdit: Boolean = True): Boolean;
var
  OldData, NewData: TRecordData;
  Editor: TForm;
begin
  Result := False;
  // Проверка корректности указателей
  if DataSet = nil then
    raise ERDbEditorError.CreateFmt(SErrDataSetNotDefined, [Self.Name]);
  // Запускаем подготовку операции
  StartWait;
  ShowInStatusBar(SMsgPrepareOperation);
  Result := True;
  // Делаем "снимок" записи до изменения
  OldData := GetRecordData(DataSet);
  try
    try
      // Создаем экземпляр редактора записи
      Editor := GetEditorClass.Create(Self.Owner);
      try
        // Если разрешено редатирование, вызываем режим редактирования
        if (FOpenMode <> omView) and RecordCanEdited and EnableEdit
        then DataSet.Edit;
        // Выполняем процедуры перед показом редактора
        if Assigned(FOnBeforeShowEditor) then
          FOnBeforeShowEditor(Self, Editor, etEdit, GetEditTag(etEdit), Result);
        // Возвращаем курсор
        ShowInStatusBar(EmptyStr);
        StopWait;
        // Показываем редактор записи
        Result := Result and (Editor.ShowModal = mrOk);
        // Выполняем процедуры после показа редактора
        // Если редатирование завершено, сохраняем данные
        if Result then
        begin
          // Запуск процедуры сохранения изменений
          StartWait;
          ShowInStatusBar(SMsgSaveDataWait);
          // Делаем "снимок" новых данных
          NewData := GetRecordData(DataSet);
          try
            // Выполняем процедуры перед сохранением
            if Assigned(FOnBeforePost) then
              FOnBeforePost(Self, Editor, OldData, NewData,
                etEdit, GetEditTag(etEdit), Result);
            // Сохраняем данные
            if Result then
            begin
              DataSet.Post;
              // Выполняем процедуры после сохранения
              if Assigned(FOnAfterPost) then
                FOnAfterPost(Self, Editor, OldData, NewData,
                  etEdit, GetEditTag(etEdit), Result);
              // Протоколируем изменения, если необходимо
              if Result then
              begin
                try
                  LogOnEdit(etEdit, GetEditTag(etEdit), OldData, NewData);
                finally
                  if Assigned(FOnAfterPostLogged) then
                    FOnAfterPostLogged(Self, Editor, OldData, NewData,
                      etEdit, GetEditTag(etEdit), Result);
                  if Result and Assigned(FOnEndEdit) then FOnEndEdit(Self);
                end;
              end;
            end;
          finally
            // Освобождаем "новый" снимок
            FreeRecordData(NewData);
          end;
        end
        else begin
          // Запуск процедуры отката изменений
          StartWait;
          ShowInStatusBar(SMsgCancelChanges);
        end;
      finally
        try
          // Откатываем любые несохраненные изменения
          if DataSet.State in [dsInsert, dsEdit] then DataSet.Cancel;
        finally
          // Выполняем процедуры перед удалением редактора
          try
            if Assigned(FOnBeforeFreeEditor) then
              FOnBeforeFreeEditor(Self, Editor, etEdit, GetEditTag(etEdit), Result);
          finally
            // Удаляем редактор
            Editor.Free;
          end;
        end;
      end;
    except
      // В случае ошибки выставляем флаг ошибки
      Result := False;
      raise
    end;
  finally
    try
      // Вызываем процедуры после всех действий
      if Assigned(FOnEndProcess) then FOnEndProcess(Self);
    finally
      // Убираем сообщение из системного статус-бара
      if Result then ShowInStatusBar(EmptyStr);
      // Удаляем "снимок" старых данных
      FreeRecordData(OldData);
      // Возвращаем курсор
      ExitWait;
    end;
  end;
end;

function TRDbEditor.MoveRecord(const OwnerId: Integer): Boolean;
var
  OldData, NewData: TRecordData;
begin
  Result := False;
  // Проверка корректности указателей
  if DataSet = nil then
    raise ERDbEditorError.CreateFmt(SErrDataSetNotDefined, [Self.Name]);
  // Запускаем подготовку операции
  StartWait;
  ShowInStatusBar(SMsgSaveDataWait);
  Result := True;
  // Делаем "снимок" записи до изменения
  OldData := GetRecordData(DataSet);
  try
    try
      DataSet.Edit;
      // Выполняем указаную процедуру
      if OwnerFieldIsPresent then
      begin
        if OwnerId > 0
        then GetOwnerField.AsInteger := OwnerId
        else GetOwnerField.Clear;
      end;
      // Делаем "снимок" новых данных
      NewData := GetRecordData(DataSet);
      try
        // Выполняем процедуры перед сохранением
        if Assigned(FOnBeforePost) then
          FOnBeforePost(Self, nil, OldData, NewData,
            etMove, GetEditTag(etMove), Result);
        // Сохраняем данные
        if Result then
        begin
          DataSet.Post;
          // Выполняем процедуры после сохранения
          if Assigned(FOnAfterPost) then
            FOnAfterPost(Self, nil, OldData, NewData,
              etMove, GetEditTag(etMove), Result);
          // Протоколируем изменения, если необходимо
          if Result then
          begin
            try
              LogOnEdit(etMove, GetEditTag(etMove), OldData, NewData);
            finally
              if Assigned(FOnAfterPostLogged) then
                FOnAfterPostLogged(Self, nil, OldData, NewData,
                  etMove, GetEditTag(etMove), Result);
              if Result and Assigned(FOnEndModify) then FOnEndModify(Self);
            end;
          end;
        end;
      finally
        try
          // Откатываем любые несохраненные изменения
          if DataSet.State in [dsInsert, dsEdit] then DataSet.Cancel;
        finally
          // Освобождаем "новый" снимок
          FreeRecordData(NewData);
        end;
      end;
    except
      // В случае ошибки выставляем флаг ошибки
      Result := False;
      raise
    end;
  finally
    try
      // Вызываем процедуры после всех действий
      if Assigned(FOnEndProcess) then FOnEndProcess(Self);
    finally
      // Убираем сообщение из системного статус-бара
      if Result then ShowInStatusBar(EmptyStr);
      // Удаляем "снимок" старых данных
      FreeRecordData(OldData);
      // Возвращаем курсор
      ExitWait;
    end;
  end;
end;

function TRDbEditor.ModifyRecord(const EditTag: Integer; Proc: TCustomModifyProc): Boolean;
var
  OldData, NewData: TRecordData;
begin
  Result := False;
  // Проверка корректности указателей
  if DataSet = nil then
    raise ERDbEditorError.CreateFmt(SErrDataSetNotDefined, [Self.Name]);
  // Запускаем подготовку операции
  StartWait;
  ShowInStatusBar(SMsgSaveDataWait);
  Result := True;
  // Делаем "снимок" записи до изменения
  OldData := GetRecordData(DataSet);
  try
    try
      DataSet.Edit;
      // Выполняем указаную процедуру
      if Assigned(Proc) then Proc(Self, Result);
      // Делаем "снимок" новых данных
      NewData := GetRecordData(DataSet);
      try
        // Выполняем процедуры перед сохранением
        if Assigned(FOnBeforePost) then
          FOnBeforePost(Self, nil, OldData, NewData, etModify, EditTag, Result);
        // Сохраняем данные
        if Result then
        begin
          DataSet.Post;
          // Выполняем процедуры после сохранения
          if Assigned(FOnAfterPost) then
            FOnAfterPost(Self, nil, OldData, NewData, etModify, EditTag, Result);
          // Протоколируем изменения, если необходимо
          if Result then
          begin
            try
              LogOnEdit(etModify, EditTag, OldData, NewData);
            finally
              if Assigned(FOnAfterPostLogged) then
                FOnAfterPostLogged(Self, nil, OldData, NewData, etModify, EditTag, Result);
              if Result and Assigned(FOnEndModify) then FOnEndModify(Self);
            end;
          end;
        end;
      finally
        try
          // Откатываем любые несохраненные изменения
          if DataSet.State in [dsInsert, dsEdit] then DataSet.Cancel;
        finally
          // Освобождаем "новый" снимок
          FreeRecordData(NewData);
        end;
      end;
    except
      // В случае ошибки выставляем флаг ошибки
      Result := False;
      raise
    end;
  finally
    try
      // Вызываем процедуры после всех действий
      if Assigned(FOnEndProcess) then FOnEndProcess(Self);
    finally
      // Убираем сообщение из системного статус-бара
      if Result then ShowInStatusBar(EmptyStr);
      // Удаляем "снимок" старых данных
      FreeRecordData(OldData);
      // Возвращаем курсор
      ExitWait;
    end;
  end;
end;

function TRDbEditor.DeleteCurrentRecord: Boolean;
var
  OldData: TRecordData;
begin
  Result := False;
  // Проверка корректности указателей
  if DataSet = nil then
    raise ERDbEditorError.CreateFmt(SErrDataSetNotDefined, [Self.Name]);
  StartWait;
  ShowInStatusBar(SMsgDeleteDataWait);
  Result := True;
  try
    try
      // Делаем "снимок" записи
      OldData := GetRecordData(DataSet);
      try
        // Выполниям процедуры до удаления
        if Assigned(FOnBeforeDelete) then
          FOnBeforeDelete(Self, OldData, nil,
            etDelete, GetEditTag(etDelete), Result);
        // Собственно удаление
        if Result then
        begin
          if BlockFieldIsPresent then
          begin
            // Логическое удаление
            DataSet.Edit;
            GetBlockField.AsBoolean := True;
            if Assigned(FOnBlockRecord) then
              FOnBlockRecord(Self, etDelete, GetEditTag(etDelete), Result);
            if Result then DataSet.Post else DataSet.Cancel;
          end
          else begin
            // Физическое удаление
            DataSet.Delete;
          end;
          // Выполняем процедуры после удаления
          if Assigned(FOnAfterDelete) then
            FOnAfterDelete(Self, OldData, nil,
              etDelete, GetEditTag(etDelete), Result);
          // Протоколируем изменения, если необходимо
          if Result then
          begin
            try
              LogOnDelete(etDelete, GetEditTag(etDelete), OldData);
            finally
              if Assigned(FOnAfterDeleteLogged) then
                FOnAfterDeleteLogged(Self, OldData, nil,
                  etDelete, GetEditTag(etDelete), Result);
              if Assigned(FOnEndDelete) then FOnEndDelete(Self);
            end;
          end;
        end;
      finally
        // Удаляем "снимок" данных
        FreeRecordData(OldData);
      end;
    except
      // В случае ошибки выставляем флаг ошибки
      Result := False;
      raise
    end;
  finally
    try
      // Вызываем процедуры после всех действий
      if Assigned(FOnEndProcess) then FOnEndProcess(Self);
    finally
      // Убираем сообщение из системного статус-бара
      if Result then ShowInStatusBar(EmptyStr);
      // Возвращаем курсор
      ExitWait;
    end;
  end;
end;

function TRDbEditor.DeleteRecord(const ShowDeleteQuery: Boolean = True): Boolean;
var
  QueryText: string;
begin
  if ShowDeleteQuery then
  begin
    QueryText := SQueryDeleteSelected;
    if Assigned(FOnDeleteQuery) then FOnDeleteQuery(Self, QueryText);
    Result := DeleteQueryText(QueryText) and DeleteCurrentRecord;
  end
  else Result := DeleteCurrentRecord;
end;

{ == TRDbExportEditor ========================================================== }

constructor TRDbExportEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExcelCopyright := EmptyStr;
  FExcelComment := EmptyStr;
end;

// Operation Tags --------------------------------------------------------------

function TRDbExportEditor.GetExportTag(const Mode: TExportMode): Integer;
begin
  if Assigned(Owner) and (Owner is TForm) and (Owner.Tag > 0)
  then Result := Owner.Tag
  else Result := Self.Tag;
  if Assigned(FOnGetExportTag) then FOnGetExportTag(Self, Mode, Result);
  Result := CheckEditTag(Result);
end;

// Export procedures -----------------------------------------------------------

procedure TRDbExportEditor.ExportToExcel;
var
  LogMsg: string;
  CopyRight, Comment: string;
  VerInfo: TVersionInfo;
begin
  CopyRight := FExcelCopyright;
  if Assigned(FOnGetExcelCopyright) then FOnGetExcelCopyright(Self, CopyRight);
  Comment := FExcelComment;
  if Assigned(FOnGetExcelComment) then FOnGetExcelComment(Self, Comment);
  VerInfo := AppVerInfo;
  try
    if ExportDataSetToExcel(DataSet, GetObjectDesc(etView), GetObjectName(etView),
        Format(CopyRight, [VerInfo.ProductName, VerInfo.InternalName, VerInfo.LegalTrademarks]),
        Comment, 0)
    and FLogEnable and Assigned(FOnSaveToLog) then
    begin
      LogMsg := Format(SLogExportExcel, [GetObjectDesc(etView), GetObjectName(etView)]);
      if Assigned(FOnGetExcelLogMsg) then FOnGetExcelLogMsg(Self, LogMsg);
      FOnSaveToLog(Self, GetExportTag(etExcel), LogMsg);
    end;
  finally
    VerInfo.Free;
  end;
end;

procedure TRDbExportEditor.ExportToCsvFile;
var
  LogMsg: string;
  ExpFileName: string;
  RecordCount: Integer;
begin
  RecordCount := ExportDataSetToCsv(DataSet, ExpFileName);
  if (RecordCount > 0) and Assigned(FOnSaveToLog) then
  begin
    LogMsg := Format(SLogExportFile,
      [GetObjectDesc(etView), GetObjectName(etView), ExpFileName, RecordCount]);
    if Assigned(FOnGetCsvFileLogMsg) then FOnGetCsvFileLogMsg(Self, LogMsg);
    FOnSaveToLog(Self, GetExportTag(etCsvFile), LogMsg);
  end;
end;

{ == TRDbTreeEditor ============================================================ }

constructor TRDbTreeEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTreeView := nil;
  FRootNodeName := EmptyStr;
  FRootNodeImage := -1;
  FRootNodeSelImage := -1;
  FGroupsDataSet := nil;
  FGroupsKeyField := EmptyStr;
  FGroupsOwnerField := EmptyStr;
  FGroupsNameField := EmptyStr;
  FGroupsNoteField := EmptyStr;
  FGroupsImage := -1;
  FGroupsSelImage := -1;
  FItemsDataSet := nil;
  FMasterDetailItems := False;
  FItemsKeyField := EmptyStr;
  FItemsOwnerField := EmptyStr;
  FItemsNameField := EmptyStr;
  FItemsNoteField := EmptyStr;
  FItemsImage := -1;
  FItemsSelImage := -1;
end;

destructor TRDbTreeEditor.Destroy;
begin
  TreeView := nil;
  GroupsDataSet := nil;
  ItemsDataSet := nil;
  inherited Destroy;
end;

procedure TRDbTreeEditor.SetTreeView(Value: TRTreeView);
begin
  if Value <> TreeView then
  begin
    FTreeView := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TRDbTreeEditor.SetGroupsDataSet(Value: TDataSet);
begin
  if Value <> GroupsDataSet then
  begin
    FGroupsDataSet := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TRDbTreeEditor.SetItemsDataSet(Value: TDataSet);
begin
  if Value <> ItemsDataSet then
  begin
    FItemsDataSet := Value;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

procedure TRDbTreeEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = TreeView) then TreeView := nil;
  if (Operation = opRemove) and (AComponent = GroupsDataSet) then GroupsDataSet := nil;
  if (Operation = opRemove) and (AComponent = ItemsDataSet) then ItemsDataSet := nil;
end;

// Groups ----------------------------------------------------------------------

function TRDbTreeEditor.GroupsDataSetIsOpen: Boolean;
begin
  Result := (GroupsDataSet <> nil) and GroupsDataSet.Active;
end;

function TRDbTreeEditor.GroupsDataSetIsNotEmpty: Boolean;
begin
  Result := (GroupsDataSet <> nil) and GroupsDataSet.Active and not GroupsDataSet.IsEmpty;
end;

function TRDbTreeEditor.GetGroupsKeyField: TField;
begin
  Result := nil;
  if GroupsDataSet <> nil then Result := GroupsDataSet.FindField(FGroupsKeyField);
end;

function TRDbTreeEditor.GetGroupsKeyValue: Integer;
var
  KeyFld: TField;
begin
  KeyFld := GetGroupsKeyField;
  if (KeyFld <> nil) and (KeyFld.DataType in IntegerDataTypes)
    and GroupsDataSet.Active and not GroupsDataSet.IsEmpty
    and not KeyFld.IsNull
  then Result := KeyFld.AsInteger
  else Result := intDisable;
end;

function TRDbTreeEditor.LocateGroupsKey(const KeyValue: Integer): Boolean;
begin
  Result := (GetGroupsKeyField <> nil) and GroupsDataSet.Active
    and GroupsDataSet.Locate(FGroupsKeyField, KeyValue, []);
end;

function TRDbTreeEditor.GroupsOwnerFieldIsPresent: Boolean;
begin
  Result := (GetGroupsOwnerField <> nil);
end;

function TRDbTreeEditor.GetGroupsOwnerField: TField;
begin
  Result := nil;
  if GroupsDataSet <> nil then Result := GroupsDataSet.FindField(FGroupsOwnerField);
end;

function TRDbTreeEditor.GetGroupsOwnerValue: Integer;
var
  OwnerFld: TField;
begin
  OwnerFld := GetGroupsOwnerField;
  if (OwnerFld <> nil) and (OwnerFld.DataType in IntegerDataTypes)
    and GroupsDataSet.Active and not GroupsDataSet.IsEmpty
    and not OwnerFld.IsNull
  then Result := OwnerFld.AsInteger
  else Result := intDisable;
end;

function TRDbTreeEditor.GetGroupsNameField: TField;
begin
  Result := nil;
  if GroupsDataSet <> nil then Result := GroupsDataSet.FindField(FGroupsNameField);
end;

function TRDbTreeEditor.GetGroupsNameValue: string;
var
  NameFld: TField;
begin
  Result := EmptyStr;
  NameFld := GetGroupsNameField;
  if (NameFld <> nil) and GroupsDataSet.Active and not GroupsDataSet.IsEmpty
  then Result := NameFld.AsString;
  if Assigned(FOnGetGroupsNameValue) then FOnGetGroupsNameValue(Self, Result);
end;

function TRDbTreeEditor.GetGroupsNoteField: TField;
begin
  Result := nil;
  if GroupsDataSet <> nil then Result := GroupsDataSet.FindField(FGroupsNoteField);
end;

function TRDbTreeEditor.GetGroupsNoteValue: string;
var
  NoteFld: TField;
begin
  Result := EmptyStr;
  NoteFld := GetGroupsNoteField;
  if (NoteFld <> nil) and GroupsDataSet.Active and not GroupsDataSet.IsEmpty
  then Result := NoteFld.AsString;
  if Assigned(FOnGetGroupsNoteValue) then FOnGetGroupsNoteValue(Self, Result);
end;

function TRDbTreeEditor.GetGroupsNodeImage: Integer;
begin
  Result := FGroupsImage;
  if Assigned(FOnGetGroupsImage) then FOnGetGroupsImage(Self, Result);
end;

function TRDbTreeEditor.GetGroupsNodeSelImage: Integer;
begin
  Result := FGroupsSelImage;
  if Assigned(FOnGetGroupsSelImage) then FOnGetGroupsSelImage(Self, Result);
end;

function TRDbTreeEditor.GetGroupsNodeText: string;
begin
  Result := AddNotes(GetGroupsNameValue, GetGroupsNoteValue);
  if Assigned(FOnGetGroupsNodeText) then FOnGetGroupsNodeText(Self, Result);
end;

// Items ----------------------------------------------------------------------

function TRDbTreeEditor.ItemsDataSetIsOpen: Boolean;
begin
  Result := (ItemsDataSet <> nil) and ItemsDataSet.Active;
end;

function TRDbTreeEditor.ItemsDataSetIsNotEmpty: Boolean;
begin
  Result := (ItemsDataSet <> nil) and ItemsDataSet.Active and not ItemsDataSet.IsEmpty;
end;

function TRDbTreeEditor.GetItemsKeyField: TField;
begin
  Result := nil;
  if ItemsDataSet <> nil then Result := ItemsDataSet.FindField(FItemsKeyField);
end;

function TRDbTreeEditor.GetItemsKeyValue: Integer;
var
  KeyFld: TField;
begin
  KeyFld := GetItemsKeyField;
  if (KeyFld <> nil) and (KeyFld.DataType in IntegerDataTypes)
    and ItemsDataSet.Active and not ItemsDataSet.IsEmpty
    and not KeyFld.IsNull
  then Result := KeyFld.AsInteger
  else Result := intDisable;
end;

function TRDbTreeEditor.LocateItemsKey(const KeyValue: Integer): Boolean;
begin
  Result := (GetItemsKeyField <> nil) and ItemsDataSet.Active
    and ItemsDataSet.Locate(FItemsKeyField, KeyValue, []);
end;

function TRDbTreeEditor.ItemsOwnerFieldIsPresent: Boolean;
begin
  Result := (GetItemsOwnerField <> nil);
end;

function TRDbTreeEditor.GetItemsOwnerField: TField;
begin
  Result := nil;
  if ItemsDataSet <> nil then Result := ItemsDataSet.FindField(FItemsOwnerField);
end;

function TRDbTreeEditor.GetItemsOwnerValue: Integer;
var
  OwnerFld: TField;
begin
  OwnerFld := GetItemsOwnerField;
  if (OwnerFld <> nil) and (OwnerFld.DataType in IntegerDataTypes)
    and ItemsDataSet.Active and not ItemsDataSet.IsEmpty
    and not OwnerFld.IsNull
  then Result := OwnerFld.AsInteger
  else Result := intDisable;
end;

function TRDbTreeEditor.GetItemsNameField: TField;
begin
  Result := nil;
  if ItemsDataSet <> nil then Result := ItemsDataSet.FindField(FItemsNameField);
end;

function TRDbTreeEditor.GetItemsNameValue: string;
var
  NameFld: TField;
begin
  Result := EmptyStr;
  NameFld := GetItemsNameField;
  if (NameFld <> nil) and ItemsDataSet.Active and not ItemsDataSet.IsEmpty
  then Result := NameFld.AsString;
  if Assigned(FOnGetItemsNameValue) then FOnGetItemsNameValue(Self, Result);
end;

function TRDbTreeEditor.GetItemsNoteField: TField;
begin
  Result := nil;
  if ItemsDataSet <> nil then Result := ItemsDataSet.FindField(FItemsNoteField);
end;

function TRDbTreeEditor.GetItemsNoteValue: string;
var
  NoteFld: TField;
begin
  Result := EmptyStr;
  NoteFld := GetItemsNoteField;
  if (NoteFld <> nil) and ItemsDataSet.Active and not ItemsDataSet.IsEmpty
  then Result := NoteFld.AsString;
  if Assigned(FOnGetItemsNoteValue) then FOnGetItemsNoteValue(Self, Result);
end;

function TRDbTreeEditor.GetItemsNodeImage: Integer;
begin
  Result := FItemsImage;
  if Assigned(FOnGetItemsImage) then FOnGetItemsImage(Self, Result);
end;

function TRDbTreeEditor.GetItemsNodeSelImage: Integer;
begin
  Result := FItemsSelImage;
  if Assigned(FOnGetItemsSelImage) then FOnGetItemsSelImage(Self, Result);
end;

function TRDbTreeEditor.GetItemsNodeText: string;
begin
  Result := AddNotes(GetItemsNameValue, GetItemsNoteValue);
  if Assigned(FOnGetItemsNodeText) then FOnGetItemsNodeText(Self, Result);
end;

// TreeView --------------------------------------------------------------------

function TRDbTreeEditor.GetLoadCount: Integer;
begin
  Result := 0;
  if FRootNodeName <> EmptyStr then
    Inc(Result);
  if GroupsDataSetIsNotEmpty then
    Inc(Result, GroupsDataSet.RecordCount);
  if (FLoadMode <> tmGroups) and ItemsDataSetIsNotEmpty
  and not FMasterDetailItems and ItemsOwnerFieldIsPresent then
    Inc(Result, ItemsDataSet.RecordCount);
end;

function TRDbTreeEditor.OpenDataSets: Boolean;
begin
  Result := False;
  if Assigned(FOnOpenDataSets) then FOnOpenDataSets(Self, Result)
  else begin
    if GroupsDataSet <> nil then
    begin
      if GroupsDataSet.Active then GroupsDataSet.Close;
      GroupsDataSet.Open;
      Result := GroupsDataSet.Active;
      if Result and (ItemsDataSet <> nil) then
      begin
        if ItemsDataSet.Active then ItemsDataSet.Close;
        ItemsDataSet.Open;
        Result := Result and ItemsDataSet.Active;
      end;
    end;
  end;
end;

procedure TRDbTreeEditor.LoadTreeStructure;
var
  RootNode: TTreeNode;

  procedure LoadChildItems(const Node: TTreeNode);
  var
    LoadItem: Boolean;
  begin
    if not FMasterDetailItems and ItemsOwnerFieldIsPresent then
    begin
      ItemsDataSet.Filter := Format(fltFieldId, [FItemsOwnerField,
        TreeView.GetNodeId(Node)]);
      ItemsDataSet.FindFirst;
      while ItemsDataSet.Found do
      begin
        LoadItem := True;
        if (FLoadMode = tmItemsChecked) and Assigned(FOnItemLoaded) then
          FOnItemLoaded(Self, LoadItem);
        if LoadItem then
          TreeView.AddChildNode(Node, ntItem, GetItemsKeyValue,
            GetItemsNodeImage, GetItemsNodeSelImage, GetItemsNodeText);
        ItemsDataSet.FindNext;
        UpdateProgressStep(1);
      end;
    end
    else begin
      ItemsDataSet.First;
      while not ItemsDataSet.Eof do
      begin
        LoadItem := True;
        if (FLoadMode = tmItemsChecked) and Assigned(FOnItemLoaded) then
          FOnItemLoaded(Self, LoadItem);
        if LoadItem then
          TreeView.AddChildNode(Node, ntItem, GetItemsKeyValue,
            GetItemsNodeImage, GetItemsNodeSelImage, GetItemsNodeText);
        ItemsDataSet.Next;
      end;
    end;
  end;

  procedure LoadChildGroups(const Node: TTreeNode);
  var
    ChildNode: TTreeNode;
    ChildBmk: TBookmark;
  begin
    GroupsDataSet.Filter := Format(fltFieldId, [FGroupsOwnerField,
      TreeView.GetNodeId(Node)]);
    GroupsDataSet.FindFirst;
    while GroupsDataSet.Found do
    begin
      ChildBmk := GroupsDataSet.GetBookmark;
      try
        ChildNode := TreeView.AddChildNode(Node, ntGroup, GetGroupsKeyValue,
            GetGroupsNodeImage, GetGroupsNodeSelImage, GetGroupsNodeText);
        LoadChildGroups(ChildNode);
        if FExpandMode = emGroups then Node.Expand(False);
        if ItemsDataSetIsOpen and (FLoadMode <> tmGroups) then
        begin
          LoadChildItems(ChildNode);
          if FExpandMode = emAll then Node.Expand(False);
        end;
      finally
        GroupsDataSet.Filter := Format(fltFieldId, [FGroupsOwnerField,
          TreeView.GetNodeId(Node)]);
        try
          GroupsDataSet.GotoBookmark(ChildBmk);
        finally
          GroupsDataSet.FreeBookmark(ChildBmk);
        end;
      end;
      GroupsDataSet.FindNext;
      UpdateProgressStep(1);
    end;
  end;

  procedure LoadFirstGroups(const Node: TTreeNode);
  var
    FirstNode: TTreeNode;
    FirstBmk: TBookmark;
  begin
    if GroupsOwnerFieldIsPresent then
    begin
      GroupsDataSet.Filter := Format(fltFieldNull, [FGroupsOwnerField]);
      GroupsDataSet.FindFirst;
      while GroupsDataSet.Found do
      begin
        FirstBmk := GroupsDataSet.GetBookmark;
        try
          if Node = nil
          then FirstNode := TreeView.AddFirstNode(ntGroup, GetGroupsKeyValue,
            GetGroupsNodeImage, GetGroupsNodeSelImage, GetGroupsNodeText)
          else FirstNode := TreeView.AddChildNode(Node, ntGroup, GetGroupsKeyValue,
            GetGroupsNodeImage, GetGroupsNodeSelImage, GetGroupsNodeText);
          LoadChildGroups(FirstNode);
          if ((FExpandMode = emRoot) and (Node = nil)) or (FExpandMode = emGroups)
          then Node.Expand(False);
          if ItemsDataSetIsOpen and (FLoadMode <> tmGroups) then
          begin
            LoadChildItems(FirstNode);
            if FExpandMode = emAll then Node.Expand(False);
          end;
        finally
          GroupsDataSet.Filter := Format(fltFieldNull, [FGroupsOwnerField]);
          try
            GroupsDataSet.GotoBookmark(FirstBmk);
          finally
            GroupsDataSet.FreeBookmark(FirstBmk);
          end;
        end;
        GroupsDataSet.FindNext;
        UpdateProgressStep(1);
      end;
    end
    else begin
      GroupsDataSet.First;
      while not GroupsDataSet.Eof do
      begin
        if Node = nil
        then FirstNode := TreeView.AddFirstNode(ntGroup, GetGroupsKeyValue,
          GetGroupsNodeImage, GetGroupsNodeSelImage, GetGroupsNodeText)
        else FirstNode := TreeView.AddChildNode(Node, ntGroup, GetGroupsKeyValue,
          GetGroupsNodeImage, GetGroupsNodeSelImage, GetGroupsNodeText);
        if ((FExpandMode = emRoot) and (Node = nil)) or (FExpandMode = emGroups)
        then Node.Expand(False);
        if ItemsDataSetIsOpen and (FLoadMode <> tmGroups) then
        begin
          LoadChildItems(FirstNode);
          if FExpandMode = emAll then Node.Expand(False);
        end;
        GroupsDataSet.Next;
        UpdateProgressStep(1);
      end;
    end;
  end;

begin
  if FRootNodeName <> EmptyStr then
  begin
    RootNode := TreeView.AddFirstNode(ntRoot, intDisable,
      FRootNodeImage, FRootNodeSelImage, FRootNodeName);
    UpdateProgressStep(1);
    if FExpandMode = emRoot then RootNode.Expand(False);
  end
  else RootNode := nil;
  if GroupsDataSetIsOpen then
  begin
    GroupsDataSet.Filtered := GroupsOwnerFieldIsPresent;
    if ItemsDataSetIsOpen
    then ItemsDataSet.Filtered := not FMasterDetailItems and ItemsOwnerFieldIsPresent;
    LoadFirstGroups(RootNode);
  end;
end;

function TRDbTreeEditor.LoadTree: Boolean;
var
  LastNode: RNodeData;
  GroupsFilter, ItemsFilter: string;
  GroupsFiltered, ItemsFiltered: Boolean;
begin
  GroupsFilter := EmptyStr;
  GroupsFiltered := False;
  ItemsFilter := EmptyStr;
  ItemsFiltered := False;
  try
    ShowProgress(SMsgLoadDataWait, 100);
    try
      if GroupsDataSet <> nil then
      begin
        if (ItemsDataSet = nil) or not FMasterDetailItems
        then GroupsDataSet.DisableControls;
        GroupsFilter := GroupsDataSet.Filter;
        GroupsFiltered := GroupsDataSet.Filtered;
        GroupsDataSet.Filtered := False;
      end;
      if (ItemsDataSet <> nil) and (FLoadMode <> tmGroups) then
      begin
        ItemsDataSet.DisableControls;
        ItemsFilter := ItemsDataSet.Filter;
        ItemsFiltered := ItemsDataSet.Filtered;
        ItemsDataSet.Filtered := False;
      end;
      try
        Result := OpenDataSets;
        if Result then
        begin
          UpdateProgressMax(GetLoadCount);
          TreeView.Items.BeginUpdate;
          TreeView.DisableOnChange;
          try
            LastNode := TreeView.GetSelectedNodeData;
            TreeView.Items.Clear;
            LoadTreeStructure;
            TreeView.Sort;
            TreeView.GotoNodeOnNodeData(LastNode, gmSelectTopItem);
            if Assigned(FOnEndLoad) then FOnEndLoad(Self);
          finally
            TreeView.RestoreOnChange;
            TreeView.Items.EndUpdate;
          end;
        end;
      finally
        if (ItemsDataSet <> nil) and (FLoadMode <> tmGroups) then
        begin
          ItemsDataSet.Filter := ItemsFilter;
          ItemsDataSet.Filtered := ItemsFiltered;
          ItemsDataSet.EnableControls;
        end;
        if GroupsDataSet <> nil then
        begin
          GroupsDataSet.Filter := GroupsFilter;
          GroupsDataSet.Filtered := GroupsFiltered;
          if (ItemsDataSet = nil) or not FMasterDetailItems
          then GroupsDataSet.EnableControls;
        end;
      end;
    finally
      CloseProgress;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, SErrLoadTree);
    end;
  end;
end;

function TRDbTreeEditor.LocateNode(Node: TTreeNode): Boolean;
var
  NodeData: TNodeData;
begin
  Result := Assigned(Node) and Assigned(Node.Data);
  if Result then
  begin
    NodeData := Node.Data;
    if NodeData.NodeType = ntGroup
    then Result := LocateGroupsKey(NodeData.NodeId)
    else Result := LocateItemsKey(NodeData.NodeId)
  end;
end;

end.
