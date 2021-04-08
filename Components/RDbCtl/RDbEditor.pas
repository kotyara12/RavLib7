{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Rav Soft Database Additional Components         }
{                                                       }
{       Copyright (c) 2006-2012 Razzhivin Alexandr      }
{                                                       }
{*******************************************************}

unit RDbEditor;

interface

uses
  Classes, Db, Forms, SysUtils,
  RDbData, DbGrids;

type
  ERDbEditorError = class(Exception);

  TEditMode       = (etView, etInsert, etImport, etEdit, etMove, etModify, etDelete);
  TOpenMode       = (omAuto, omEdit, omView, omCheck);
  TMultiExcept    = (meQuery, meStop, meContinue);
  TExportMode     = (etExcel, etCsvFile);

  TGetEditTagNotifyEvent    = procedure (Sender: TObject; const Mode: TEditMode; var EditTag: Integer) of object;
  TGetEditNameNotifyEvent   = procedure (Sender: TObject; const Mode: TEditMode; var Name: string) of object;
  TGetEditRightsNotifyEvent = procedure (Sender: TObject; const Mode: TEditMode; var Enable: Boolean) of object;
  TSaveToLogNotifyEvent     = procedure (Sender: TObject; const EditTag: Integer; const Text: string) of object;
  TVarIntegerNotifyEvent    = procedure (Sender: TObject; var Value: Integer) of object;
  TVarStringNotifyEvent     = procedure (Sender: TObject; var Value: string) of object;
  TFormClassNotifyEvent     = procedure (Sender: TObject; var EditorClass: TFormClass) of object;
  TCopyDataNotifyEvent      = procedure (Sender: TObject; var Buffer: TCopyBuffer) of object;
  TResultNotifyEvent        = procedure (Sender: TObject; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;
  TResultDataNotifyEvent    = procedure (Sender: TObject; OldData, NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;
  TEditorNotifyEvent        = procedure (Sender: TObject; Editor: TForm; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;
  TEditorDataNotifyEvent    = procedure (Sender: TObject; Editor: TForm; OldData, NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer; var Complete: Boolean) of object;
  TLogGetMsgNotifyEvent     = procedure (Sender: TObject; OldData, NewData: TRecordData; const Mode: TEditMode; const EditTag: Integer; var Text: string) of object;
  TLogSaveNotifyEvent       = procedure (Sender: TObject; const EditTag: Integer; const Text: string) of object;

  TCustomModifyProc         = procedure (Sender: TObject; const Data: Pointer; var Complete: Boolean) of object;

  TRDbCustomEditor = class(TDataSource)
  private
    fKeyField: string;
    fOwnerField: string;
    fBlockField: string;
    fBlockValue: Boolean;
    fObjectName: string;
    fObjectDesc: string;
    fLogEnable: Boolean;
    fNewAppend: Boolean;
    fOpenMode: TOpenMode;
    fCheckTags: Boolean;
    fCopiedFields: string;
    fGroupsFields: string;
    fEditBlockedRecords: Boolean;
    fMoveBlockedRecords: Boolean;
    fOnGetEditTag: TGetEditTagNotifyEvent;
    fOnGetObjectName: TGetEditNameNotifyEvent;
    fOnGetObjectDesc: TGetEditNameNotifyEvent;
    fOnFormClass: TFormClassNotifyEvent;
    fOnBeforeProcessRecord: TResultNotifyEvent;
    fOnAfterProcessRecord: TResultNotifyEvent;
    fOnBeforeShowEditor: TEditorNotifyEvent;
    fOnBeforeFreeEditor: TEditorNotifyEvent;
    fOnGetKey: TVarIntegerNotifyEvent;
    fOnFreeKey: TVarIntegerNotifyEvent;
    fOnCreateNewRecord: TResultNotifyEvent;
    fOnCreateSetDefault: TResultNotifyEvent;
    fOnCopyRecord: TCopyDataNotifyEvent;
    fOnPasteRecord: TCopyDataNotifyEvent;
    fOnMoveRecord: TResultNotifyEvent;
    fOnBeforePost: TEditorDataNotifyEvent;
    fOnAfterPost: TEditorDataNotifyEvent;
    fOnAfterPostLogged: TEditorDataNotifyEvent;
    fOnBeforeCalcel: TResultNotifyEvent;
    fOnAfterCalcel: TResultNotifyEvent;
    fOnDeleteQuery: TVarStringNotifyEvent;
    fOnBlockRecord: TResultNotifyEvent;
    fOnCustomDelete: TResultNotifyEvent;
    fOnBeforeDelete: TResultDataNotifyEvent;
    fOnAfterDelete: TResultDataNotifyEvent;
    fOnAfterDeleteLogged: TResultDataNotifyEvent;
    fOnGetInsertLogMsg: TLogGetMsgNotifyEvent;
    fOnGetChangeLogMsg: TLogGetMsgNotifyEvent;
    fOnGetDeleteLogMsg: TLogGetMsgNotifyEvent;
    fOnSaveToLog: TLogSaveNotifyEvent;
    // Editor
    function GetEditorClass: TFormClass;
  protected
    // DataSet
    procedure CheckDataSet;
    procedure RollbackModifyState;
    // Editor
    function  EditorFormCreate: TForm;
    function  EditorFormShow(Editor: TForm; const Mode: TEditMode; var Complete: Boolean): Boolean;
    procedure EditorFormFree(Editor: TForm; const Mode: TEditMode; var Complete: Boolean);
    // Routines
    function  WaitPreviousOperation: Boolean;
    procedure DoPrepareStart; overload; virtual;
    procedure DoPrepareStart(const StateMessage: string); overload; virtual;
    procedure DoDeleteStart; virtual;
    procedure DoPrepareEnd; virtual;
    // Processing
    procedure DoProcessRecordStart(const Mode: TEditMode; var Complete: Boolean); virtual;
    procedure DoProcessRecordEnd(const Mode: TEditMode; var Complete: Boolean; const ClearStatusBar: Boolean); virtual;
    procedure DoCreateNewRecord(const Mode: TEditMode; var Complete: Boolean); virtual;
    procedure DoCreateSetDefault(const Mode: TEditMode; var Complete: Boolean); virtual;
    procedure DoCopyBuffer(var Buffer: TCopyBuffer); virtual;
    procedure DoPasteBuffer(var Buffer: TCopyBuffer); virtual;
    procedure DoMoveRecord(var Complete: Boolean); virtual;
    procedure DoPostData(Editor: TForm; const OldData: TRecordData; const Mode: TEditMode;
      var Complete: Boolean; const InitStatusMessage: Boolean); virtual;
    procedure DoCancelData(Editor: TForm; const OldData: TRecordData; const Mode: TEditMode;
      var Complete: Boolean; const InitStatusMessage: Boolean); virtual;
    procedure DoDeleteQuery(const ShowDeleteQuery: Boolean; var Complete: Boolean; const MultiMode: Boolean = True); virtual;
    procedure DoDeleteData(const Data: Pointer; var Complete: Boolean; const ClearStatusBar: Boolean); virtual;
    // Key Value
    procedure CreateKeyValue(var KeyValue: Integer); virtual;
    procedure FreeKeyValue(var KeyValue: Integer); virtual;
    // Owner Value
    procedure SetOwnerValue(const OwnerId: Integer); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetSelCount: Integer; virtual;
    function GetSelIds: string; virtual;
    // Operation Tags
    function CheckEditTag(const EditTag: Integer): Integer; virtual;
    function GetEditTag(const Mode: TEditMode): Integer; virtual;
    function GetObjectName(const Mode: TEditMode): string; virtual;
    function GetObjectDesc(const Mode: TEditMode): string; virtual;
    procedure SaveLogMsg(const EditTag: Integer; const Msg: string); overload; virtual;
    procedure SaveLogMsg(const Mode: TEditMode; const Msg: string); overload; virtual;
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
    function DataSetIsOpened: Boolean;
    function DataSetIsEmply: Boolean;
    function DataSetIsNotEmply: Boolean;
    function DataSetIsEdited: Boolean;
    function RecordCopyExists: Boolean;
    // Properties
    property KeyField: TField read GetKeyField;
    property KeyValue: Integer read GetKeyValue;
    property OwnerField: TField read GetOwnerField;
    property OwnerValue: Integer read GetOwnerValue;
    // Log Messages
    function  IsLogEnabled: Boolean; virtual;
    procedure LogOnInsert(const Mode: TEditMode; const EditTag: Integer;
      const NewData: TRecordData); virtual;
    procedure LogOnEdit(const Mode: TEditMode; const EditTag: Integer; 
      const OldData, NewData: TRecordData); virtual;
    procedure LogOnPost(const Mode: TEditMode; const EditTag: Integer; 
      const OldData, NewData: TRecordData);
    procedure LogOnDelete(const Mode: TEditMode; const EditTag: Integer; 
      const OldData: TRecordData); virtual;
    // Utilites
    procedure ShowStatistic(const DefaultFields: string; const OnlyDefault: Boolean);
    procedure ShowGrouping;
  published
    property CheckTags: Boolean read fCheckTags write fCheckTags;
    property KeyFieldName: string read fKeyField write fKeyField;
    property BlockFieldName: string read FBlockField write FBlockField;
    property BlockValue: Boolean read FBlockValue write FBlockValue default True;
    property OwnerFieldName: string read fOwnerField write fOwnerField;
    property CopiedFields: string read fCopiedFields write fCopiedFields;
    property ObjectName: string read fObjectName write fObjectName;
    property ObjectDesc: string read fObjectDesc write fObjectDesc;
    property LogEnable: Boolean read fLogEnable write fLogEnable default False;
    property NewRecordAppend: Boolean read fNewAppend write fNewAppend default True;
    property OpenMode: TOpenMode read fOpenMode write fOpenMode;
    property EditBlockedRecords: Boolean read fEditBlockedRecords write fEditBlockedRecords default False;
    property MoveBlockedRecords: Boolean read fMoveBlockedRecords write fMoveBlockedRecords default True;
    property StatisticFields: string read fGroupsFields write fGroupsFields;
    property OnGetEditTag: TGetEditTagNotifyEvent read fOnGetEditTag write fOnGetEditTag;
    property OnGetObjectName: TGetEditNameNotifyEvent read fOnGetObjectName write fOnGetObjectName;
    property OnGetObjectDesc: TGetEditNameNotifyEvent read fOnGetObjectDesc write fOnGetObjectDesc;
    property OnGetEditorClass: TFormClassNotifyEvent read fOnFormClass write fOnFormClass;
    property OnBeforeProcessRecord: TResultNotifyEvent read fOnBeforeProcessRecord write fOnBeforeProcessRecord;
    property OnAfterProcessRecord: TResultNotifyEvent read fOnAfterProcessRecord write fOnAfterProcessRecord;
    property OnBeforeCalcel: TResultNotifyEvent read fOnBeforeCalcel write fOnBeforeCalcel;
    property OnAfterCalcel: TResultNotifyEvent read fOnAfterCalcel write fOnAfterCalcel;
    property OnBeforeShowEditor: TEditorNotifyEvent read fOnBeforeShowEditor write fOnBeforeShowEditor;
    property OnBeforeFreeEditor: TEditorNotifyEvent read fOnBeforeFreeEditor write fOnBeforeFreeEditor;
    property OnGetNewKey: TVarIntegerNotifyEvent read fOnGetKey write fOnGetKey;
    property OnFreeNewKey: TVarIntegerNotifyEvent read fOnFreeKey write fOnFreeKey;
    property OnCreateNewRecord: TResultNotifyEvent read fOnCreateNewRecord write fOnCreateNewRecord;
    property OnCreateSetDefault: TResultNotifyEvent read fOnCreateSetDefault write fOnCreateSetDefault;
    property OnCopyRecord: TCopyDataNotifyEvent read fOnCopyRecord write fOnCopyRecord;
    property OnPasteRecord: TCopyDataNotifyEvent read fOnPasteRecord write fOnPasteRecord;
    property OnMoveRecord: TResultNotifyEvent read fOnMoveRecord write fOnMoveRecord;
    property OnBeforePost: TEditorDataNotifyEvent read fOnBeforePost write fOnBeforePost;
    property OnAfterPost: TEditorDataNotifyEvent read fOnAfterPost write fOnAfterPost;
    property OnAfterPostLogged: TEditorDataNotifyEvent read fOnAfterPostLogged write fOnAfterPostLogged;
    property OnDeleteQuery: TVarStringNotifyEvent read fOnDeleteQuery write fOnDeleteQuery;
    property OnBlockRecord: TResultNotifyEvent read fOnBlockRecord write fOnBlockRecord;
    property OnCustomDelete: TResultNotifyEvent read fOnCustomDelete write fOnCustomDelete;
    property OnBeforeDelete: TResultDataNotifyEvent read fOnBeforeDelete write fOnBeforeDelete;
    property OnAfterDelete: TResultDataNotifyEvent read fOnAfterDelete write fOnAfterDelete;
    property OnAfterDeleteLogged: TResultDataNotifyEvent read fOnAfterDeleteLogged write fOnAfterDeleteLogged;
    property OnGetInsertLogMsg: TLogGetMsgNotifyEvent read fOnGetInsertLogMsg write fOnGetInsertLogMsg;
    property OnGetChangeLogMsg: TLogGetMsgNotifyEvent read fOnGetChangeLogMsg write fOnGetChangeLogMsg;
    property OnGetDeleteLogMsg: TLogGetMsgNotifyEvent read fOnGetDeleteLogMsg write fOnGetDeleteLogMsg;
    property OnSaveToLog: TLogSaveNotifyEvent read fOnSaveToLog write fOnSaveToLog;
  end;

  TRDbEditor = class(TRDbCustomEditor)
  private
    fMultiMode: Boolean;
    fShowProgressOnMP: Boolean;
    fShowQueryOnErrorMP: TMultiExcept;
    fShowQueryOnErrorSP: TMultiExcept;
    fClearSelection: Boolean;
    fDbGrid: TDbGrid;
    fOnGetEditRights: TGetEditRightsNotifyEvent;
    fOnBeforeProcessRecords: TNotifyEvent;
    fOnAfterProcessRecords: TNotifyEvent;
    procedure SetDbGrid(Value: TDbGrid);
    function  GetMultiSelect: Boolean;
    procedure SetMultiSelect(const Value: Boolean);
    function ProcessCurrentRecord(CheckProc, ModifyProc: TCustomModifyProc;
      const Data: Pointer; var TotlCnt, ProcCnt, IgnrCnt: Integer;
      const OnException: TMultiExcept): Boolean;
  protected
    procedure DoProcessRecordsStart;
    procedure DoProcessRecordsEnd;
    procedure ChangeOwner_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure ChangeOwner_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure DeleteRecord_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
    procedure DeleteRecord_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // Record States
    function RecordIsSelected: Boolean;
    function RecordIsEnabled(const CheckBlockField: Boolean): Boolean;
    function RecordCanModified(const CheckRights: Boolean = True): Boolean;
    function RecordCanInserted(const CheckRights: Boolean = True): Boolean;
    function RecordCanImported(const CheckRights: Boolean = True): Boolean;
    function RecordCanCopyed(const CheckRights: Boolean = True): Boolean;
    function RecordCanOpened(const CheckRights: Boolean = True): Boolean;
    function RecordCanEdited(const CheckRights: Boolean = True): Boolean;
    function RecordCanMoved(const CheckRights: Boolean = True): Boolean;
    function RecordCanDeleted(const CheckRights: Boolean = True): Boolean;
    // Edit DataSet
    function InsertRecord(const OwnerId: Integer = 0): Boolean;
    function ImportRecord(const OwnerId: Integer; Proc: TCustomModifyProc;
      const Data: Pointer; const StatusMsg: string = ''): Boolean;
    function CopyRecord(const OwnerId: Integer = 0; const ShowEditor: Boolean = True): Boolean;
    function EditRecord(const EnableEdit: Boolean = True): Boolean;
    function MoveRecord(const OwnerId: Integer): Boolean;
    function MoveRecords(const OwnerId: Integer): Boolean;
    function ModifyRecord(Proc: TCustomModifyProc; const Data: Pointer; const StatusMsg: string = ''): Boolean;
    function DeleteCurrentRecord(const ShowDeleteQuery: Boolean = True): Boolean;
    function DeleteRecord(const ShowDeleteQuery: Boolean = True): Boolean;
    // Multiselect
    function  GetSelCount: Integer; override;
    function  GetSelIds: string; override;
    function  GraySelected: Boolean;
    procedure ClearSelection;
    procedure RefreshSelection;
    // MiltiProcess
    function ProcessSelectedRecords(CheckProc, ModifyProc: TCustomModifyProc;
      const Data: Pointer; const StatusMsg: string;
      const ShowResults, ForwardScan, DisableCtrls: Boolean): Boolean;
    function ProcessAllRecords(CheckProc, ModifyProc: TCustomModifyProc;
      const Data: Pointer; const StatusMsg: string;
      const ShowResults, ForwardScan, DisableCtrls: Boolean): Boolean;
    function ProcessAndDeleteRecords(CheckProc, ModifyProc: TCustomModifyProc;
      const Data: Pointer; const StatusMsg: string;
      const ShowResults, DisableCtrls: Boolean): Boolean; // 2016-08-31
    function ProcessAutoRecords(CheckProc, ModifyProc: TCustomModifyProc;
      const Data: Pointer; const StatusMsg: string;
      const ShowResults, ForwardScan, DisableCtrls: Boolean): Boolean;
    function ProcessAutoAllRecords(CheckProc, ModifyProc: TCustomModifyProc;
      const Data: Pointer; const StatusMsg: string;
      const ShowResults, ForwardScan, DisableCtrls: Boolean): Boolean;
    // Properties
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    property MultiMode: Boolean read fMultiMode;
    property SelCount: Integer read GetSelCount;
    property SelIds: string read GetSelIds;
  published
    property ShowProgressOnMP: Boolean read fShowProgressOnMP write fShowProgressOnMP default True;
    property ShowQueryOnErrorMP: TMultiExcept read fShowQueryOnErrorMP write fShowQueryOnErrorMP default meQuery;
    property ShowQueryOnErrorSP: TMultiExcept read fShowQueryOnErrorSP write fShowQueryOnErrorSP default meStop;
    property ClearSelectionAfterProcess: Boolean read fClearSelection write fClearSelection default True;
    property DbGrid: TDbGrid read fDbGrid write SetDbGrid;
    property OnGetEditRights: TGetEditRightsNotifyEvent read fOnGetEditRights write fOnGetEditRights;
    property OnBeforeProcessRecords: TNotifyEvent read fOnBeforeProcessRecords write fOnBeforeProcessRecords; // ver 2.7.5.252 2007-09-09
    property OnAfterProcessRecords: TNotifyEvent read fOnAfterProcessRecords write fOnAfterProcessRecords; // ver 2.7.5.252 2007-09-09
  end;

  TGetExportTagNotifyEvent  = procedure (Sender: TObject; const Mode: TExportMode; var ExportTag: Integer) of object;

  TRDbExportEditor = class(TRDbEditor)
  private
    FExcelCopyright: string;
    FExcelComment: string;
    fOnGetExportTag: TGetExportTagNotifyEvent;
    fOnGetExcelCopyright: TVarStringNotifyEvent;
    fOnGetExcelCaption: TVarStringNotifyEvent;
    fOnGetExcelComment: TVarStringNotifyEvent;
    fOnGetExcelLogMsg: TVarStringNotifyEvent;
    fOnGetCsvFileLogMsg: TVarStringNotifyEvent;
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
    property OnGetExportTag: TGetExportTagNotifyEvent read fOnGetExportTag write fOnGetExportTag;
    property OnGetExcelCopyright: TVarStringNotifyEvent read fOnGetExcelCopyright write fOnGetExcelCopyright;
    property OnGetExcelCaption: TVarStringNotifyEvent read fOnGetExcelCaption write fOnGetExcelCaption;
    property OnGetExcelComment: TVarStringNotifyEvent read fOnGetExcelComment write fOnGetExcelComment;
    property OnGetExcelLogMsg: TVarStringNotifyEvent read fOnGetExcelLogMsg write fOnGetExcelLogMsg;
    property OnGetCsvFileLogMsg: TVarStringNotifyEvent read fOnGetCsvFileLogMsg write fOnGetCsvFileLogMsg;
  end;

resourcestring
  SErrDataSetNotDefined         = '%s: Свойство DataSet не определено!';
  SErrEditorClassNotDefined     = '%s: Не найден класс формы-редактора записи!'#13'Смотри обработчик события OnGetEditorClass.';

implementation

uses
  Controls, RxVerInf, Windows, RDbConst,
  RVclUtils, RMsgRu, RRssConst, RDialogs, RProgress, RExHandlers, RDbUtils,
  RExpExcel, RCsvExport, RSelProcMode,
  RDbStatPrm, RDbStatRes, RDbGroupPrm;

const
  IntegerDataTypes = [ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc];
  BooleanDataTypes = [ftBoolean];
  EnabledDbStates  = [dsBrowse];

{ == TRDbCustomEditor ========================================================== }

constructor TRDbCustomEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AutoEdit := False;
  fObjectName := EmptyStr;
  fObjectDesc := EmptyStr;
  fCopiedFields := EmptyStr;
  fKeyField := EmptyStr;
  fOwnerField := EmptyStr;
  fLogEnable := False;
  fNewAppend := True;
  fOpenMode := omAuto;
  fCheckTags := True;
  fBlockField := EmptyStr;
  fBlockValue := True;
  fEditBlockedRecords := False;
  fMoveBlockedRecords := True;
  fCopiedFields := EmptyStr;
  fGroupsFields := EmptyStr;
end;

function TRDbCustomEditor.GetSelCount: Integer;
begin
  if DataSetIsNotEmply
  then Result := 1
  else Result := 0;
end;

function TRDbCustomEditor.GetSelIds: string;
begin
  if DataSetIsNotEmply
  then Result := IntToStr(GetKeyValue)
  else Result := '';
end;

// Operation Tags and Logs -----------------------------------------------------

function TRDbCustomEditor.CheckEditTag(const EditTag: Integer): Integer;
begin
  if not fCheckTags or (EditTag > 0) then
    Result := EditTag
  else begin
    Result := tagError;
    ErrorBox(Format(EErrBadOperationTag, [EditTag]));
  end;
end;

function TRDbCustomEditor.GetEditTag(const Mode: TEditMode): Integer;
begin
  if Assigned(DataSet) and (DataSet.Tag > 0)
  then Result := DataSet.Tag
  else Result := Self.Tag;
  if Assigned(fOnGetEditTag) then
    fOnGetEditTag(Self, Mode, Result);
  Result := CheckEditTag(Result);
end;

function TRDbCustomEditor.GetObjectName(const Mode: TEditMode): string;
begin
  if fObjectName <> EmptyStr then
    Result := fObjectName
  else if Assigned(DataSet)
       then Result := DataSet.Name
       else Result := EmptyStr;
  if Assigned(fOnGetObjectName) then
    fOnGetObjectName(Self, Mode, Result);
end;

function TRDbCustomEditor.GetObjectDesc(const Mode: TEditMode): string;
begin
  if fObjectDesc <> EmptyStr then
    Result := fObjectDesc
  else if Assigned(Owner) and (Owner is TForm)
       then Result := TForm(Owner).Caption
       else Result := EmptyStr;
  if Assigned(fOnGetObjectDesc) then
    fOnGetObjectDesc(Self, Mode, Result);
end;

// Log Messages ----------------------------------------------------------------

function TRDbCustomEditor.IsLogEnabled: Boolean;
begin
  Result := fLogEnable and Assigned(fOnSaveToLog);
end;

procedure TRDbCustomEditor.LogOnInsert(const Mode: TEditMode; const EditTag: Integer;
  const NewData: TRecordData);
var
  LogMsg: string;
begin
  if IsLogEnabled then
  begin
    (*** removed 2017-03-10 ***
    LogMsg := Format(SLogNewRecord,
      [GetObjectDesc(Mode), GetObjectName(Mode),
       GetRecordText(NewData, [rtRequired, rtVisible, rtIndex, rtID, rtAnyTag])]);
    *** removed 2017-03-10 ***)
    LogMsg := rssSysLog_CreateRecord(GetObjectName(Mode), GetObjectDesc(Mode), NewData);
    if Assigned(fOnGetInsertLogMsg) then
      fOnGetInsertLogMsg(Self, nil, NewData, Mode, EditTag, LogMsg);
    if LogMsg <> EmptyStr then
      fOnSaveToLog(Self, EditTag, LogMsg);
  end;
end;

procedure TRDbCustomEditor.LogOnEdit(const Mode: TEditMode; const EditTag: Integer;
  const OldData, NewData: TRecordData);
var
  LogMsg: string;
begin
  if IsLogEnabled then
  begin
    (*** removed 2017-03-10 ***
    Changes := GetRecordChanges(OldData, NewData, []);
    if Changes <> EmptyStr
    then LogMsg := Format(SLogEditRecord,
      [GetObjectDesc(Mode), GetObjectName(Mode),
       GetRecordChanges(NewData, OldData, [rtRequired, rtIndex, rtID, rtAnyTag]), Changes])
    else LogMsg := Format(SLogEditRecordEmpty,
      [GetObjectDesc(Mode), GetObjectName(Mode),
       GetRecordText(NewData, [rtRequired, rtIndex, rtID, rtAnyTag])]);
    *** removed 2017-03-10 ***)
    LogMsg := rssSysLog_UpdateRecord(GetObjectName(Mode), GetObjectDesc(Mode), OldData, NewData);
    if Assigned(fOnGetChangeLogMsg) then
      fOnGetChangeLogMsg(Self, OldData, NewData, Mode, EditTag, LogMsg);
    if LogMsg <> EmptyStr then
      fOnSaveToLog(Self, EditTag, LogMsg);
  end;
end;

procedure TRDbCustomEditor.LogOnPost(const Mode: TEditMode; const EditTag: Integer;
  const OldData, NewData: TRecordData);
begin
  if Mode in [etInsert, etImport] 
  then LogOnInsert(Mode, EditTag, NewData)
  else LogOnEdit(Mode, EditTag, OldData, NewData);
end;

procedure TRDbCustomEditor.LogOnDelete(const Mode: TEditMode; const EditTag: Integer;
  const OldData: TRecordData);
var
  LogMsg: string;
begin
  if IsLogEnabled then
  begin
    (*** removed 2017-03-10 ***
    LogMsg := Format(SLogDeleteRecord,
      [GetObjectDesc(Mode), GetObjectName(Mode),
      GetRecordText(OldData, [rtRequired, rtVisible, rtIndex, rtID, rtAnyTag])]);
    *** removed 2017-03-10 ***)
    LogMsg := rssSysLog_DeleteRecord(GetObjectName(Mode), GetObjectDesc(Mode), OldData);
    if Assigned(fOnGetDeleteLogMsg) then
      fOnGetDeleteLogMsg(Self, OldData, nil, Mode, EditTag, LogMsg);
    if LogMsg <> EmptyStr then
      fOnSaveToLog(Self, EditTag, LogMsg);
  end;
end;

procedure TRDbCustomEditor.SaveLogMsg(const EditTag: Integer; const Msg: string);
begin
  if IsLogEnabled and (Msg <> EmptyStr) then
    fOnSaveToLog(Self, EditTag, Format(SLogCustom,
      [GetObjectDesc(etView), GetObjectName(etView), Msg]));
end;

procedure TRDbCustomEditor.SaveLogMsg(const Mode: TEditMode; const Msg: string);
begin
  if IsLogEnabled and (Msg <> EmptyStr) then
    fOnSaveToLog(Self, GetEditTag(Mode), Format(SLogCustom,
      [GetObjectDesc(Mode), GetObjectName(Mode), Msg]));
end;

// Routines --------------------------------------------------------------------

function TRDbCustomEditor.WaitPreviousOperation: Boolean;
begin
  Result := IsNotWait;
end;

procedure TRDbCustomEditor.DoPrepareStart;
begin
  StartWait;
  ShowInStatusBar(SMsgPrepareOperation);
end;

procedure TRDbCustomEditor.DoDeleteStart;
begin
  StartWait;
  ShowInStatusBar(SMsgDeleteDataWait);
end;

procedure TRDbCustomEditor.DoPrepareStart(const StateMessage: string);
begin
  StartWait;
  if StateMessage <> EmptyStr
  then ShowInStatusBar(StateMessage)
  else ShowInStatusBar(SMsgSaveDataWait);
end;

procedure TRDbCustomEditor.DoPrepareEnd;
begin
  ShowInStatusBar(EmptyStr);
  StopWait;
end;

// Processing ------------------------------------------------------------------

procedure TRDbCustomEditor.DoProcessRecordStart(const Mode: TEditMode; var Complete: Boolean);
begin
  if Assigned(fOnBeforeProcessRecord) then
    fOnBeforeProcessRecord(Self, Mode, GetEditTag(Mode), Complete);
end;

procedure TRDbCustomEditor.DoProcessRecordEnd(const Mode: TEditMode; var Complete: Boolean; const ClearStatusBar: Boolean);
begin
  try
    if Assigned(fOnAfterProcessRecord) then
      fOnAfterProcessRecord(Self, Mode, GetEditTag(Mode), Complete);
  finally
    if ClearStatusBar then
      ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TRDbCustomEditor.DoCreateNewRecord(const Mode: TEditMode; var Complete: Boolean);
begin
  if Assigned(fOnCreateNewRecord) then
    fOnCreateNewRecord(Self, Mode, GetEditTag(Mode), Complete);
end;

procedure TRDbCustomEditor.DoCreateSetDefault(const Mode: TEditMode; var Complete: Boolean);
begin
  if Assigned(fOnCreateSetDefault) then
    fOnCreateSetDefault(Self, Mode, GetEditTag(Mode), Complete);
end;

procedure TRDbCustomEditor.DoCopyBuffer(var Buffer: TCopyBuffer);
begin
  PutInCopyBuffer(DataSet, Buffer);
  if Assigned(fOnCopyRecord) then
    fOnCopyRecord(Self, Buffer);
end;

procedure TRDbCustomEditor.DoPasteBuffer(var Buffer: TCopyBuffer);
begin
  if fCopiedFields <> EmptyStr then
    PasteCopyBufferC(DataSet, Buffer, fCopiedFields);
  if Assigned(fOnPasteRecord) then
    fOnPasteRecord(Self, Buffer);
end;

procedure TRDbCustomEditor.DoMoveRecord(var Complete: Boolean);
begin
  if Assigned(fOnMoveRecord) then
    fOnMoveRecord(Self, etMove, GetEditTag(etMove), Complete);
end;

procedure TRDbCustomEditor.DoPostData(Editor: TForm; const OldData: TRecordData;
  const Mode: TEditMode; var Complete: Boolean; const InitStatusMessage: Boolean);
var
  NewData: TRecordData;
begin
  if Complete and DataSetIsEdited then
  begin
    if InitStatusMessage then
    begin
      StartWait;
      ShowInStatusBar(SMsgSaveDataWait);
    end;
    NewData := GetRecordData(DataSet);
    try
       if Assigned(fOnBeforePost) then
       begin
         fOnBeforePost(Self, Editor, OldData, NewData, Mode, GetEditTag(Mode), Complete);
         NewData := GetRecordData(DataSet);
       end;
       if Complete then
       begin
         try
           DataSet.Post;
         except
           on E: EAbort do
             Complete := False;
         end;
         if Complete and Assigned(fOnAfterPost) then
           fOnAfterPost(Self, Editor, OldData, NewData, Mode, GetEditTag(Mode), Complete);
         if Complete then
         begin
           try
             LogOnPost(Mode, GetEditTag(Mode), OldData, NewData);
           finally
             if Assigned(fOnAfterPostLogged) then
               fOnAfterPostLogged(Self, Editor, OldData, NewData, Mode, GetEditTag(Mode), Complete);
           end;
         end;
       end;
    finally
      FreeRecordData(NewData);
    end;
  end;
end;

procedure TRDbCustomEditor.DoCancelData(Editor: TForm; const OldData: TRecordData;
  const Mode: TEditMode; var Complete: Boolean; const InitStatusMessage: Boolean);
begin
  if InitStatusMessage then
  begin
    StartWait;
    ShowInStatusBar(SMsgCancelChanges);
  end;
  if Assigned(fOnBeforeCalcel) then
    fOnBeforeCalcel(Self, Mode, GetEditTag(Mode), Complete);
  RollbackModifyState;
  if Complete and Assigned(fOnAfterCalcel) then
    fOnAfterCalcel(Self, Mode, GetEditTag(Mode), Complete);
end;

procedure TRDbCustomEditor.DoDeleteQuery(const ShowDeleteQuery: Boolean; var Complete: Boolean; const MultiMode: Boolean = True);
var
  QueryText: string;
begin
  Complete := GetSelCount > 0;
  if ShowDeleteQuery then
  begin
    if (GetSelCount > 1) and MultiMode
    then QueryText := Format(SQueryDeleteCount, [GetSelCount])
    else QueryText := SQueryDeleteSelected;
    if Assigned(fOnDeleteQuery) then
      fOnDeleteQuery(Self, QueryText);
    Complete := DeleteQueryText(QueryText);
  end;
end;

procedure TRDbCustomEditor.DoDeleteData(const Data: Pointer; var Complete: Boolean; const ClearStatusBar: Boolean);
var
  OldData: TRecordData;
begin
  Complete := True;
  DoDeleteStart;
  try
    try
      OldData := GetRecordData(DataSet);
      try
        if Assigned(fOnBeforeDelete) then
          fOnBeforeDelete(Self, OldData, nil, etDelete, GetEditTag(etDelete), Complete);
        if Complete then
        begin
          if Assigned(fOnCustomDelete) then
          begin
            fOnCustomDelete(Self, etDelete, GetEditTag(etDelete), Complete);
            if Complete then
            begin
              try
                DataSet.Refresh;
              except
              end;
            end;
          end
          else begin
            if BlockFieldIsPresent then
            begin
              DataSet.Edit;
              DoProcessRecordStart(etDelete, Complete);
              if Assigned(fOnBlockRecord)
              then fOnBlockRecord(Self, etDelete, GetEditTag(etDelete), Complete)
              else GetBlockField.AsBoolean := fBlockValue;
              if Complete then
              begin
                try
                  DataSet.Post;
                except
                  on E: EAbort do
                    Complete := False;
                end;
                try
                  DataSet.Refresh;
                except
                end;
              end
              else DataSet.Cancel;
            end
            else begin
              try
                DataSet.Delete;
              except
                on E: EAbort do
                  Complete := False;
              end;
            end;
          end;
          if Complete and Assigned(fOnAfterDelete) then
            fOnAfterDelete(Self, OldData, nil, etDelete, GetEditTag(etDelete), Complete);
          if Complete then
          begin
            try
              LogOnDelete(etDelete, GetEditTag(etDelete), OldData);
            finally
              if Assigned(fOnAfterDeleteLogged) then
                fOnAfterDeleteLogged(Self, OldData, nil, etDelete, GetEditTag(etDelete), Complete);
            end;
          end;
        end;
      finally
        FreeRecordData(OldData);
      end;
    except
      on E: Exception do
      begin
        Complete := False;
        HandleExcept(E, Self, SErrRecordDelete);
      end;
    end;
  finally
    DoProcessRecordEnd(etDelete, Complete, ClearStatusBar);
  end;
end;

// Key Field -------------------------------------------------------------------

function TRDbCustomEditor.KeyFieldIsPresent: Boolean;
var
  KeyFld: TField;
begin
  KeyFld := GetKeyField;
  Result := (KeyFld <> nil) and (KeyFld.DataType in IntegerDataTypes);
end;

function TRDbCustomEditor.GetKeyField: TField;
begin
  Result := nil;
  if DataSet <> nil then
    Result := DataSet.FindField(fKeyField);
end;

function TRDbCustomEditor.GetKeyValue: Integer;
var
  KeyFld: TField;
begin
  KeyFld := GetKeyField;
  if (KeyFld <> nil) and (KeyFld.DataType in IntegerDataTypes)
    and DataSet.Active and not DataSet.IsEmpty
  then Result := KeyFld.AsInteger
  else Result := intDisable;
end;

function TRDbCustomEditor.LocateKey(const KeyValue: Integer): Boolean;
begin
  Result := KeyFieldIsPresent and DataSet.Active
    and DataSet.Locate(fKeyField, KeyValue, []);
end;

procedure TRDbCustomEditor.CreateKeyValue(var KeyValue: Integer);
begin
  KeyValue := intDisable;
  if DataSetIsEdited and KeyFieldIsPresent and Assigned(fOnGetKey) then
  begin
    fOnGetKey(Self, KeyValue);
    GetKeyField.AsInteger := KeyValue;
  end;
end;

procedure TRDbCustomEditor.FreeKeyValue(var KeyValue: Integer);
begin
  if KeyFieldIsPresent and (KeyValue > intDisable) and Assigned(fOnFreeKey) then
    fOnFreeKey(Self, KeyValue);
end;

// Owner Field -----------------------------------------------------------------

function TRDbCustomEditor.OwnerFieldIsPresent: Boolean;
var
  OwnerFld: TField;
begin
  OwnerFld := GetOwnerField;
  Result := (OwnerFld <> nil) and (OwnerFld.DataType in IntegerDataTypes);
end;

function TRDbCustomEditor.GetOwnerField: TField;
begin
  Result := nil;
  if DataSet <> nil then 
    Result := DataSet.FindField(fOwnerField);
end;

function TRDbCustomEditor.GetOwnerValue: Integer;
var
  OwnerFld: TField;
begin
  OwnerFld := GetOwnerField;
  if (OwnerFld <> nil) and (OwnerFld.DataType in IntegerDataTypes)
    and DataSet.Active and not DataSet.IsEmpty
  then Result := OwnerFld.AsInteger
  else Result := intDisable;
end;

procedure TRDbCustomEditor.SetOwnerValue(const OwnerId: Integer);
var
  fOwner: TField;
begin
  if DataSetIsEdited and OwnerFieldIsPresent then
  begin
    fOwner := GetOwnerField;
    if Assigned(fOwner) then
    begin
      if fOwner.ReadOnly then
      begin
        fOwner.ReadOnly := False;
        try
          if OwnerId > 0
          then fOwner.AsInteger := OwnerId
          else fOwner.Clear;
        finally
          fOwner.ReadOnly := True;
        end;
      end
      else begin
        if OwnerId > 0
        then fOwner.AsInteger := OwnerId
        else fOwner.Clear;
      end;
    end;
  end;
end;

// Block Field -----------------------------------------------------------------

function TRDbCustomEditor.BlockFieldIsPresent: Boolean;
var
  BlockFld: TField;
begin
  BlockFld := GetBlockField;
  Result := (BlockFld <> nil) and (BlockFld.DataType in BooleanDataTypes);
end;

function TRDbCustomEditor.GetBlockField: TField;
begin
  Result := nil;
  if DataSet <> nil then Result := DataSet.FindField(fBlockField);
end;

function TRDbCustomEditor.GetBlockValue: Boolean;
var
  BlockFld: TField;
begin
  BlockFld := GetBlockField;
  if Assigned(BlockFld) and (BlockFld.DataType in BooleanDataTypes)
    and DataSet.Active and not DataSet.IsEmpty
  then Result := BlockFld.AsBoolean
  else Result := not fBlockValue;
end;

// Record States ---------------------------------------------------------------

function TRDbCustomEditor.DataSetIsOpened: Boolean;
begin
  Result := Enabled and (DataSet <> nil) and DataSet.Active;
end;

function TRDbCustomEditor.DataSetIsEmply: Boolean;
begin
  Result := Enabled and (DataSet <> nil) and DataSet.Active and DataSet.IsEmpty;
end;

function TRDbCustomEditor.DataSetIsNotEmply: Boolean;
begin
  Result := Enabled and (DataSet <> nil) and DataSet.Active and not DataSet.IsEmpty;
end;

function TRDbCustomEditor.DataSetIsEdited: Boolean;
begin
  Result := DataSetIsOpened and (DataSet.State in [dsInsert, dsEdit]);
end;

function TRDbCustomEditor.RecordCopyExists: Boolean;
begin
  Result := Assigned(fOnPasteRecord) or (fCopiedFields <> EmptyStr);
end;

procedure TRDbCustomEditor.RollbackModifyState;
begin
  if DataSetIsEdited then
    DataSet.Cancel;
end;

procedure TRDbCustomEditor.CheckDataSet;
begin
  if not DataSetIsOpened then
    raise ERDbEditorError.CreateFmt(SErrDataSetNotDefined, [Self.Name]);
end;

// Utilites --------------------------------------------------------------------

procedure TRDbCustomEditor.ShowStatistic(const DefaultFields: string; const OnlyDefault: Boolean);
var
  TempSelFields: string;
begin
  TempSelFields := DefaultFields;
  if TempSelFields = EmptyStr then
    TempSelFields := fGroupsFields;
  if OnlyDefault or SelectStatFields(Self, TempSelFields) then
    ShowDbStatistic(Self, TempSelFields);
end;

procedure TRDbCustomEditor.ShowGrouping;
begin
  if Assigned(DataSet) then
    ShowGroupingData(Self);
end;

// Editor ----------------------------------------------------------------------

function TRDbCustomEditor.GetEditorClass: TFormClass;
begin
  Result := nil;
  if Assigned(fOnFormClass) then
    fOnFormClass(Self, Result);
  if Result = nil then
    raise ERDbEditorError.CreateFmt(SErrEditorClassNotDefined, [Self.Name]);
end;

function TRDbCustomEditor.EditorFormCreate: TForm;
begin
  Result := GetEditorClass.Create(Self.Owner);
end;

function TRDbCustomEditor.EditorFormShow(Editor: TForm; const Mode: TEditMode;
  var Complete: Boolean): Boolean;
begin
  Result := False;
  if Complete and Assigned(Editor) then
  begin
    if Assigned(fOnBeforeShowEditor) then
      fOnBeforeShowEditor(Self, Editor, Mode, GetEditTag(Mode), Complete);
    while not IsNotWait do 
      DoPrepareEnd;
    Result := Complete and (Editor.ShowModal = mrOk);
    Complete := Result;
  end;
end;

procedure TRDbCustomEditor.EditorFormFree(Editor: TForm; const Mode: TEditMode; 
  var Complete: Boolean);
begin
  if Assigned(Editor) then
  begin
    try
      if Assigned(fOnBeforeFreeEditor) then
        fOnBeforeFreeEditor(Self, Editor, Mode, GetEditTag(Mode), Complete);
    finally
      Editor.Free;
    end;
  end;
end;

{ == TRDbEditor ================================================================ }

constructor TRDbEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDbGrid := nil;
  fMultiMode := False;
  fShowProgressOnMP := True;
  fShowQueryOnErrorMP := meQuery;
  fShowQueryOnErrorSP := meStop;
  fClearSelection := True;
end;

destructor TRDbEditor.Destroy;
begin
  DbGrid := nil;
  inherited Destroy;
end;

procedure TRDbEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(fDbGrid) and (AComponent = fDbGrid) then
    DbGrid := nil;
end;

procedure TRDbEditor.SetDbGrid(Value: TDbGrid);
begin
  if fDbGrid <> Value then
  begin
    fDbGrid := Value;
    if Assigned(Value) then
      Value.FreeNotification(Self);
  end;
end;

// MultiSelect -----------------------------------------------------------------

function TRDbEditor.GetMultiSelect: Boolean;
begin
  Result := (fDbGrid <> nil) and (dgMultiSelect in fDbGrid.Options);
end;

procedure TRDbEditor.SetMultiSelect(const Value: Boolean);
begin
  if (fDbGrid <> nil) and ((dgMultiSelect in fDbGrid.Options) <> Value) then
  begin
    if Value
    then fDbGrid.Options := fDbGrid.Options + [dgMultiSelect]
    else fDbGrid.Options := fDbGrid.Options - [dgMultiSelect];
  end;
end;

function TRDbEditor.GetSelCount: Integer;
begin
  if (DataSet <> nil) and DataSet.Active then
  begin
    if DataSet.IsEmpty then Result := 0
    else if (fDbGrid = nil) or not (dgMultiSelect in fDbGrid.Options)
         or (fDbGrid.SelectedRows.Count < 1)
         then Result := 1
         else Result := fDbGrid.SelectedRows.Count;
  end
  else Result := -1;
end;

function TRDbEditor.GetSelIds: string;
var
  fCurrId, i: Integer;
begin
  Result := '';
  if (DataSet <> nil) and DataSet.Active then
  begin
    if not DataSet.IsEmpty then
    begin
      fCurrId := GetKeyValue;
      if (fDbGrid = nil) or not (dgMultiSelect in fDbGrid.Options)
      or (fDbGrid.SelectedRows.Count < 1)
      then Result := IntToStr(fCurrId)
      else begin
        try
          for i := 0 to fDbGrid.SelectedRows.Count - 1 do
          begin
            try
              DataSet.GotoBookmark(Pointer(fDbGrid.SelectedRows.Items[i]));
            except
              Continue;
            end;
            if Result = ''
            then Result := IntToStr(GetKeyValue)
            else Result := Result + sqlListSeparator + IntToStr(GetKeyValue);
          end;
        finally
          LocateKey(fCurrId);
        end;
      end;
    end;
  end;
end;

function TRDbEditor.GraySelected: Boolean;
begin
  Result := (DataSet <> nil) and (fDbGrid <> nil)
    and (dgMultiSelect in fDbGrid.Options)
    and (fDbGrid.SelectedRows.Count > 0);
end;

procedure TRDbEditor.ClearSelection;
begin
  StartWait;
  try
    if MultiSelect then
      fDbGrid.SelectedRows.Clear;
  finally
    StopWait;
  end;
end;

procedure TRDbEditor.RefreshSelection;
begin
  StartWait;
  try
    if MultiSelect then
      fDbGrid.SelectedRows.Refresh;
  finally
    StopWait;
  end;
end;

// MultiProcess ----------------------------------------------------------------

function TRDbEditor.ProcessCurrentRecord(CheckProc, ModifyProc: TCustomModifyProc;
  const Data: Pointer; var TotlCnt, ProcCnt, IgnrCnt: Integer;
  const OnException: TMultiExcept): Boolean;
var
  Enable: Boolean;
begin
  Result := True;
  Inc(TotlCnt);
  // Если указана процедура проверки прав редактирования, проверяем права
  if Assigned(CheckProc)
  then CheckProc(Self, Data, Enable)
  else Enable := True;
  // Выполняем указанную процедуру
  if Enable then
  begin
    // Для "гарантии" обрабатываем ошибки, если в ModifyProc они не были обработаны, дабы не прерывать цикла
    try
      ModifyProc(Self, Data, Result);
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self);
      end;
    end;
    // Если результат выполнения отрицательный, спрашиваем о необходимости прервать операцию
    if Result then Inc(ProcCnt)
    else begin
      case OnException of
        meQuery:
          Result := QueryBoxStdYN(SQueryBreakOperation) <> IDYES;
        meStop:
          Result := False;
        meContinue:
          Result := True;
      end;
    end;
  end
  else Inc(IgnrCnt);
end;

procedure TRDbEditor.DoProcessRecordsStart;
begin
  if Assigned(fOnBeforeProcessRecords) then fOnBeforeProcessRecords(Self);
end;

procedure TRDbEditor.DoProcessRecordsEnd;
begin
  if Assigned(fOnAfterProcessRecords) then fOnAfterProcessRecords(Self);
end;

function TRDbEditor.ProcessSelectedRecords(CheckProc, ModifyProc: TCustomModifyProc;
  const Data: Pointer; const StatusMsg: string;
  const ShowResults, ForwardScan, DisableCtrls: Boolean): Boolean;
var
  i, TotlCnt, ProcCnt, IgnrCnt: Integer;
  bShowProgress: Boolean;
begin
  FMultiMode := False;
  // Проверяем, есть ли выделенные записи. Если нет - сразу выходим
  Result := GetSelCount > 0;
  if not Result then Exit;
  // Выводим сообщение и отключаем контролы
  StartWait;
  ShowInStatusBar(StatusMsg);
  if DisableCtrls then DataSet.DisableControls;
  try
    try
      DoProcessRecordsStart;
      try
        TotlCnt := 0; ProcCnt := 0; IgnrCnt := 0;
        (*
        CurrKey := GetKeyValue;
        try *)
          // Если включен мультиселект, обрабатываем в цикле
          if GetSelCount > 1 then
          begin
            FMultiMode := True;
            bShowProgress := fShowProgressOnMP and not IsShowProgress;
            if bShowProgress then ShowProgress(StatusMsg, fDbGrid.SelectedRows.Count, True);
            try
              // Выбираем направление обработки
              if ForwardScan then
                // Прямая обработка
                for i := 0 to fDbGrid.SelectedRows.Count - 1 do
                begin
                  // Пытаемся перейти на закладку. В случае неудачи - не обрабатываем запись и далее по циклу
                  try
                    DataSet.GotoBookmark(Pointer(fDbGrid.SelectedRows.Items[i]));
                  except
                    Continue;
                  end;
                  // Обрабатываем текущую запись
                  Result := not (bShowProgress and IsStopProgress)
                    and ProcessCurrentRecord(CheckProc, ModifyProc, Data,
                      TotlCnt, ProcCnt, IgnrCnt, fShowQueryOnErrorMP);
                  if not Result then Break;
                  // Индикатор прогресса
                  if bShowProgress then UpdateProgressStep(1);
                end
              else
                // Обратная обработка
                for i := fDbGrid.SelectedRows.Count - 1 downto 0 do
                begin
                  // Пытаемся перейти на закладку. В случае неудачи - не обрабатываем запись и далее по циклу
                  try
                    DataSet.GotoBookmark(Pointer(fDbGrid.SelectedRows.Items[i]));
                  except
                    Continue;
                  end;
                  // Обрабатываем текущую запись
                  Result := not (bShowProgress and IsStopProgress)
                    and ProcessCurrentRecord(CheckProc, ModifyProc, Data,
                      TotlCnt, ProcCnt, IgnrCnt, fShowQueryOnErrorMP);
                  if not Result then Break;
                  // Индикатор прогресса
                  if bShowProgress then UpdateProgressStep(1);
                end;
              // Сбрасываем выделение
              if Result and fClearSelection then
                fDbGrid.SelectedRows.Clear;
            finally
              // Убираем индикатор
              if bShowProgress then CloseProgress;
            end;
          end
          else begin
            // Одиночная обработка
            Result := ProcessCurrentRecord(CheckProc, ModifyProc, Data,
              TotlCnt, ProcCnt, IgnrCnt, fShowQueryOnErrorSP);
          end;
          // Показываем сообщение: 2007-09-05 ver 2.7.2.249
          // Условие сообщения:    2007-09-06 ver 2.7.3.250
          if ShowResults then
          begin
            ShowInStatusBar(Format(SMsgProcessCounts, [TotlCnt, ProcCnt, IgnrCnt]));
            if FMultiMode then
              InfoBox(Format(SMsgProcessCounts, [TotlCnt, ProcCnt, IgnrCnt]));
          end;
        (* finally
          LocateKey(CurrKey);
        end; *)
      finally
        DoProcessRecordsEnd;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self, SErrRecordMultiprocess);
      end;
    end;
  finally
    FMultiMode := False;
    // Включаем контролы и убираем сообщения
    if DisableCtrls then DataSet.EnableControls;
    if Result then ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function TRDbEditor.ProcessAllRecords(CheckProc, ModifyProc: TCustomModifyProc;
  const Data: Pointer; const StatusMsg: string;
  const ShowResults, ForwardScan, DisableCtrls: Boolean): Boolean; // ver 2.7.5.252 2007-09-09
var
  TotlCnt, ProcCnt, IgnrCnt: Integer;
  bShowProgress: Boolean;
begin
  Result := True;
  FMultiMode := True;
  // Выводим сообщение и отключаем контролы
  StartWait;
  ShowInStatusBar(StatusMsg);
  if DisableCtrls then DataSet.DisableControls;
  try
    try
      DoProcessRecordsStart;
      try
        (* CurrKey := GetKeyValue;
        try *)
          TotlCnt := 0; ProcCnt := 0; IgnrCnt := 0;

          bShowProgress := fShowProgressOnMP and not IsShowProgress;
          if bShowProgress then ShowProgress(StatusMsg, DataSet.RecordCount, True);
          try
            // Выбираем направление обработки
            if ForwardScan then
            begin
              // Прямая обработка
              DataSet.First;
              while not DataSet.Eof do
              begin
                // Обрабатываем текущую запись
                Result := not (bShowProgress and IsStopProgress)
                  and ProcessCurrentRecord(CheckProc, ModifyProc, Data,
                    TotlCnt, ProcCnt, IgnrCnt, fShowQueryOnErrorMP);
                if not Result then Break;
                // Индикатор прогресса
                DataSet.Next;
                if bShowProgress then UpdateProgressStep(1);
              end;
            end
            else begin
              // Обратная обработка
              DataSet.Last;
              while not DataSet.Bof do
              begin
                // Обрабатываем текущую запись
                Result := not (bShowProgress and IsStopProgress)
                  and ProcessCurrentRecord(CheckProc, ModifyProc, Data,
                    TotlCnt, ProcCnt, IgnrCnt, fShowQueryOnErrorMP);
                if not Result then Break;
                // Индикатор прогресса
                DataSet.Prior;
                if bShowProgress then UpdateProgressStep(1);
              end;
            end;
          finally
            // Убираем индикатор
            if bShowProgress then CloseProgress;
          end;
          // Показываем сообщение
          if ShowResults then
          begin
            ShowInStatusBar(Format(SMsgProcessCounts, [TotlCnt, ProcCnt, IgnrCnt]));
            InfoBox(Format(SMsgProcessCounts, [TotlCnt, ProcCnt, IgnrCnt]));
          end;
        (* finally
          LocateKey(CurrKey);
        end; *)
      finally
        DoProcessRecordsEnd;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self, SErrRecordMultiprocess);
      end;
    end;
  finally
    FMultiMode := False;
    // Включаем контролы и убираем сообщения
    if DisableCtrls then DataSet.EnableControls;
    if Result then ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function TRDbEditor.ProcessAndDeleteRecords(CheckProc, ModifyProc: TCustomModifyProc;
  const Data: Pointer; const StatusMsg: string;
  const ShowResults, DisableCtrls: Boolean): Boolean; // ver. 10.2.0.382 2016-08-31
var
  TotlCnt, ProcCnt, IgnrCnt: Integer;
  bShowProgress: Boolean;
  bEnable: Boolean;
begin
  Result := True;
  FMultiMode := True;
  // Выводим сообщение и отключаем контролы
  StartWait;
  ShowInStatusBar(StatusMsg);
  if DisableCtrls then DataSet.DisableControls;
  try
    try
      DoProcessRecordsStart;
      try
        TotlCnt := 0; ProcCnt := 0; IgnrCnt := 0;

        bShowProgress := fShowProgressOnMP and not IsShowProgress;
        if bShowProgress then ShowProgress(StatusMsg, DataSet.RecordCount, True);
        try
          DataSet.First;
          while not DataSet.Eof do
          begin
            // Проверяем, нужно ли обрабатывать текущую запись
            if Assigned(CheckProc)
            then CheckProc(Self, Data, bEnable)
            else bEnable := True;

            // Обрабатываем текущую запись
            if bEnable then
            begin
              Result := not (bShowProgress and IsStopProgress)
                and ProcessCurrentRecord(nil, ModifyProc, Data,
                  TotlCnt, ProcCnt, IgnrCnt, fShowQueryOnErrorSP);
              if not Result then Break;
              DeleteRecord(False);
            end
            else DataSet.Next;

            // Индикатор прогресса
            if bShowProgress then UpdateProgressStep(1);
          end;
        finally
          // Убираем индикатор
          if bShowProgress then CloseProgress;
        end;
        // Показываем сообщение
        if ShowResults then
        begin
          ShowInStatusBar(Format(SMsgProcessCounts, [TotlCnt, ProcCnt, IgnrCnt]));
          InfoBox(Format(SMsgProcessCounts, [TotlCnt, ProcCnt, IgnrCnt]));
        end;
      finally
        DoProcessRecordsEnd;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self, SErrRecordMultiprocess);
      end;
    end;
  finally
    FMultiMode := False;
    // Включаем контролы и убираем сообщения
    if DisableCtrls then DataSet.EnableControls;
    if Result then ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function TRDbEditor.ProcessAutoRecords(CheckProc, ModifyProc: TCustomModifyProc;
  const Data: Pointer; const StatusMsg: string;
  const ShowResults, ForwardScan, DisableCtrls: Boolean): Boolean; // ver 2.7.5.252 2007-09-09
var
  bSelectedOnly: Boolean;
begin
  // ver. 2.7.6.255 2007-09-13
  if SelCount > 1 then
  begin
    if SelectProcessMode(DataSet.RecordCount, SelCount, bSelectedOnly) then
    begin
      if bSelectedOnly then
        Result := ProcessSelectedRecords(CheckProc, ModifyProc, Data,
          StatusMsg, ShowResults, ForwardScan, DisableCtrls)
      else
        Result := ProcessAllRecords(CheckProc, ModifyProc, Data,
          StatusMsg, ShowResults, ForwardScan, DisableCtrls);
    end
    else Result := False;
  end
  else Result := ProcessSelectedRecords(CheckProc, ModifyProc, Data,
    StatusMsg, ShowResults, ForwardScan, DisableCtrls);
end;

function TRDbEditor.ProcessAutoAllRecords(CheckProc, ModifyProc: TCustomModifyProc;
  const Data: Pointer; const StatusMsg: string;
  const ShowResults, ForwardScan, DisableCtrls: Boolean): Boolean;
var
  bSelectedOnly: Boolean;
begin
  if SelCount > 1 then
  begin
    if SelectProcessMode(DataSet.RecordCount, SelCount, bSelectedOnly) then
    begin
      if bSelectedOnly then
        Result := ProcessSelectedRecords(CheckProc, ModifyProc, Data,
          StatusMsg, ShowResults, ForwardScan, DisableCtrls)
      else
        Result := ProcessAllRecords(CheckProc, ModifyProc, Data,
          StatusMsg, ShowResults, ForwardScan, DisableCtrls);
    end
    else Result := False;
  end
  else Result := ProcessAllRecords(CheckProc, ModifyProc, Data,
    StatusMsg, ShowResults, ForwardScan, DisableCtrls);
end;

// Record States ---------------------------------------------------------------

function TRDbEditor.RecordIsSelected: Boolean;
begin
  Result := GetSelCount > 0;
end;

function TRDbEditor.RecordIsEnabled(const CheckBlockField: Boolean): Boolean;
var
  BlockFld: TField;
begin
  Result := (GetSelCount > 0) and (DataSet.State in EnabledDbStates);
  if Result and CheckBlockField then
  begin
    BlockFld := GetBlockField;
    Result := not (Assigned(BlockFld) and (BlockFld.DataType in BooleanDataTypes)
      and (BlockFld.AsBoolean = FBlockValue));
  end;
end;

function TRDbEditor.RecordCanInserted(const CheckRights: Boolean = True): Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if CheckRights and Assigned(OnGetEditRights) then
    OnGetEditRights(Self, etInsert, Rights);
  Result := Rights and DataSetIsOpened and (DataSet.State in EnabledDbStates);
end;

function TRDbEditor.RecordCanImported(const CheckRights: Boolean = True): Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if CheckRights and Assigned(OnGetEditRights) then
    OnGetEditRights(Self, etImport, Rights);
  Result := Rights and DataSetIsOpened and (DataSet.State in EnabledDbStates);
end;

function TRDbEditor.RecordCanCopyed(const CheckRights: Boolean = True): Boolean;
begin
  Result := RecordCopyExists and RecordIsEnabled(False) and RecordCanInserted(CheckRights);
end;

function TRDbEditor.RecordCanOpened(const CheckRights: Boolean = True): Boolean;
var
  Rights: Boolean;
begin
  if fOpenMode = omEdit
  then Result := RecordCanEdited(CheckRights)
  else begin
    Rights := True;
    if CheckRights and (fOpenMode = omCheck) and Assigned(OnGetEditRights) then
      OnGetEditRights(Self, etView, Rights);
    Result := Rights and (GetSelCount > 0) and (DataSet.State in EnabledDbStates);
  end;
end;

function TRDbEditor.RecordCanEdited(const CheckRights: Boolean = True): Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if CheckRights and Assigned(OnGetEditRights) then
    OnGetEditRights(Self, etEdit, Rights);
  Result := Rights and RecordIsEnabled(not FEditBlockedRecords);
end;

function TRDbEditor.RecordCanModified(const CheckRights: Boolean = True): Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if CheckRights and Assigned(OnGetEditRights) then
    OnGetEditRights(Self, etModify, Rights);
  Result := Rights and RecordIsEnabled(not FEditBlockedRecords);
end;

function TRDbEditor.RecordCanMoved(const CheckRights: Boolean = True): Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if CheckRights and Assigned(OnGetEditRights) then
    OnGetEditRights(Self, etMove, Rights);
  Result := Rights and RecordIsEnabled(not FMoveBlockedRecords);
end;

function TRDbEditor.RecordCanDeleted(const CheckRights: Boolean = True): Boolean;
var
  Rights: Boolean;
begin
  Rights := True;
  if CheckRights and Assigned(OnGetEditRights) then
    OnGetEditRights(Self, etDelete, Rights);
  Result := Rights and RecordIsEnabled(True);
end;

// Edit DataSet ----------------------------------------------------------------

function TRDbEditor.InsertRecord(const OwnerId: Integer = 0): Boolean;
var
  NewId: Integer;
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
          if fNewAppend then DataSet.Append else DataSet.Insert;
          try
            DoProcessRecordStart(etInsert, Result);
            try
              CreateKeyValue(NewId);
              SetOwnerValue(OwnerId);
              DoCreateNewRecord(etInsert, Result);
              DoCreateSetDefault(etInsert, Result);
              if EditorFormShow(Editor, etInsert, Result)
              then DoPostData(Editor, nil, etInsert, Result, True)
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

function TRDbEditor.ImportRecord(const OwnerId: Integer;
  Proc: TCustomModifyProc; const Data: Pointer; const StatusMsg: string = ''): Boolean;
var
  NewId: Integer;
begin
  Result := True;
  CheckDataSet;
  DoPrepareStart(StatusMsg);
  try
    try
      if fNewAppend then DataSet.Append else DataSet.Insert;
      try
        DoProcessRecordStart(etImport, Result);
        try
          CreateKeyValue(NewId);
          SetOwnerValue(OwnerId);
          DoCreateNewRecord(etImport, Result);
          DoCreateSetDefault(etInsert, Result);
          if Assigned(Proc)
          then Proc(Self, Data, Result)
          else Result := False;
          if Result
          then DoPostData(nil, nil, etImport, Result, False)
          else DoCancelData(nil, nil, etImport, Result, False);
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

function TRDbEditor.CopyRecord(const OwnerId: Integer = 0; const ShowEditor: Boolean = True): Boolean;
var
  Buffer: TCopyBuffer;
  NewId: Integer;
  Editor: TForm;
begin
  Result := not ShowEditor or WaitPreviousOperation;
  if Result then
  begin
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
            if fNewAppend then DataSet.Append else DataSet.Insert;
            try
              DoProcessRecordStart(etInsert, Result);
              try
                CreateKeyValue(NewId);
                SetOwnerValue(OwnerId);
                DoCreateNewRecord(etInsert, Result);
                DoPasteBuffer(Buffer);
                if not ShowEditor or EditorFormShow(Editor, etInsert, Result)
                then DoPostData(Editor, nil, etInsert, Result, ShowEditor)
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

function TRDbEditor.EditRecord(const EnableEdit: Boolean = True): Boolean;
var
  OldData: TRecordData;
  CurrMode: TEditMode;
  Editor: TForm;
begin
  Result := WaitPreviousOperation;
  if Result then
  begin
    CheckDataSet;
    DoPrepareStart;
    if (fOpenMode <> omView) and RecordCanEdited and EnableEdit
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
            if EditorFormShow(Editor, CurrMode, Result)
            then DoPostData(Editor, OldData, CurrMode, Result, True)
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

procedure TRDbEditor.ChangeOwner_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  Complete := RecordCanMoved;
end;

procedure TRDbEditor.ChangeOwner_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
var
  OldData: TRecordData;
begin
  Complete := True;
  CheckDataSet;
  DoPrepareStart(SMsgSaveDataWait);
  try
    try
      OldData := GetRecordData(DataSet);
      try
        DataSet.Edit;
        DoProcessRecordStart(etMove, Complete);
        SetOwnerValue(TId(Data)^);
        DoMoveRecord(Complete);
        if Complete
        then DoPostData(nil, OldData, etMove, Complete, False)
        else DoCancelData(nil, OldData, etMove, Complete, False);
      finally
        RollbackModifyState;
        FreeRecordData(OldData);
      end;
    except
      on E: Exception do
      begin
        Complete := False;
        HandleExcept(E, Self, SErrRecordEdit);
      end;
    end;
  finally
    DoProcessRecordEnd(etMove, Complete, False);
  end;
end;

function TRDbEditor.MoveRecord(const OwnerId: Integer): Boolean;
var
  Id: TId;
begin
  Result := False;
  New(Id);
  try
    Id^ := OwnerId;
    ChangeOwner_Modify(Self, Id, Result);
  finally
    Dispose(Id);
  end;
end;

function TRDbEditor.MoveRecords(const OwnerId: Integer): Boolean;
var
  Id: TId;
begin
  New(Id);
  try
    Id^ := OwnerId;
    Result := ProcessSelectedRecords(ChangeOwner_Check, ChangeOwner_Modify, Id,
      SMsgSaveDataWait, True, True, True);
  finally
    Dispose(Id);
  end;
end;

function TRDbEditor.ModifyRecord(Proc: TCustomModifyProc; const Data: Pointer; const StatusMsg: string = ''): Boolean;
var
  OldData: TRecordData;
begin
  Result := True;
  CheckDataSet;
  DoPrepareStart(StatusMsg);
  try
    try
      OldData := GetRecordData(DataSet);
      try
        DataSet.Edit;
        DoProcessRecordStart(etModify, Result);
        if Assigned(Proc)
        then Proc(Self, Data, Result)
        else Result := False;
        if Result
        then DoPostData(nil, OldData, etModify, Result, False)
        else DoCancelData(nil, OldData, etModify, Result, False);
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
    DoProcessRecordEnd(etModify, Result, False);
  end;
end;

procedure TRDbEditor.DeleteRecord_Check(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  Complete := RecordCanDeleted;
end;

procedure TRDbEditor.DeleteRecord_Modify(Sender: TObject; const Data: Pointer; var Complete: Boolean);
begin
  Complete := True;
  CheckDataSet;
  DoDeleteData(Data, Complete, False);
end;

function TRDbEditor.DeleteCurrentRecord(const ShowDeleteQuery: Boolean = True): Boolean;
var
  TotlCnt, ProcCnt, IgnrCnt: Integer;
begin
  CheckDataSet;
  DoDeleteQuery(ShowDeleteQuery, Result, False);
  if Result then
    Result := ProcessCurrentRecord(DeleteRecord_Check, DeleteRecord_Modify,
      nil, TotlCnt, ProcCnt, IgnrCnt, fShowQueryOnErrorSP);
end;

function TRDbEditor.DeleteRecord(const ShowDeleteQuery: Boolean = True): Boolean;
begin
  CheckDataSet;
  DoDeleteQuery(ShowDeleteQuery, Result);
  if Result then
    ProcessSelectedRecords(DeleteRecord_Check, DeleteRecord_Modify,
      nil, SMsgDeleteDataWait, True, False, True);
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
  if Assigned(fOnGetExportTag) then
    fOnGetExportTag(Self, Mode, Result);
  Result := CheckEditTag(Result);
end;

// Export procedures -----------------------------------------------------------

procedure TRDbExportEditor.ExportToExcel;
var
  LogMsg: string;
  CopyRight, Caption, Comment: string;
  VerInfo: TVersionInfo;
begin
  CopyRight := FExcelCopyright;
  if Assigned(fOnGetExcelCopyright) then fOnGetExcelCopyright(Self, CopyRight);
  Caption := GetObjectDesc(etView);
  if Assigned(fOnGetExcelCaption) then fOnGetExcelCaption(Self, Caption);
  Comment := FExcelComment;
  if Assigned(fOnGetExcelComment) then fOnGetExcelComment(Self, Comment);
  VerInfo := AppVerInfo;
  try
    if ExportDataSetToExcel(DataSet, fDbGrid, Caption, GetObjectName(etView),
        Format(CopyRight, [VerInfo.ProductName, VerInfo.InternalName, VerInfo.LegalTrademarks]),
        Comment, 0)
    and IsLogEnabled then
    begin
      LogMsg := Format(SLogExportExcel, [GetObjectDesc(etView), GetObjectName(etView)]);
      if Assigned(fOnGetExcelLogMsg) then fOnGetExcelLogMsg(Self, LogMsg);
      fOnSaveToLog(Self, GetExportTag(etExcel), LogMsg);
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
  RecordCount := ExportDataSetToCsv(DataSet, fDbGrid, ExpFileName);
  if (RecordCount > 0) and IsLogEnabled then
  begin
    LogMsg := Format(SLogExportFile,
      [GetObjectDesc(etView), GetObjectName(etView), ExpFileName, RecordCount]);
    if Assigned(fOnGetCsvFileLogMsg) then fOnGetCsvFileLogMsg(Self, LogMsg);
    fOnSaveToLog(Self, GetExportTag(etCsvFile), LogMsg);
  end;
end;

end.
