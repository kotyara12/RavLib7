unit RDbAttachs;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplList, Menus, ActnList, StdCtrls, Buttons, ExtCtrls, Db, AdoDb,
  ComCtrls, RavListView, RDbEditor, ToolWin, ImgList;

type
  TFormDbAttachs = class(TListTemplate)
    FileAppend: TAction;
    FileDelete: TAction;
    FileOpen: TAction;
    FileSave: TAction;
    btnFileAppend: TToolButton;
    btnFileDelete: TToolButton;
    ToolButton3: TToolButton;
    btnFileOpen: TToolButton;
    btnFileSave: TToolButton;
    ToolButton6: TToolButton;
    btnRefresh: TToolButton;
    ToolButton8: TToolButton;
    btnCloseCancel: TToolButton;
    itemFileAppend: TMenuItem;
    itemFileDelete: TMenuItem;
    divFile1: TMenuItem;
    itemFileOpen: TMenuItem;
    itemFileSave: TMenuItem;
    divFile2: TMenuItem;
    itemFileAppendP: TMenuItem;
    itemFileDeleteP: TMenuItem;
    divFileP1: TMenuItem;
    itemFileOpenP: TMenuItem;
    itemFileSaveP: TMenuItem;
    divFileP2: TMenuItem;
    ImageList: TImageList;
    procedure FileAppendUpdate(Sender: TObject);
    procedure FileAppendExecute(Sender: TObject);
    procedure FileDeleteUpdate(Sender: TObject);
    procedure FileDeleteExecute(Sender: TObject);
    procedure FileOpenUpdate(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure FileSaveUpdate(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
  private
  protected
    {$IFDEF RSS}
    procedure LogOnOpen; override;
    procedure LogOnClose; override;
    {$ENDIF}
    function  GetDetailName: string; override;
    function  LoadDataForm: Boolean; override;
  public
    fDS: TAdoQuery;
    fEditTag: Integer;
    fEditEnabled: Boolean;
    fAttachsChanged: Boolean;
    fObjectId: Integer;
    fObjectName: string;
    fObjectDescr: string;
    procedure ShowItemsCount; override;
  end;

const
  tnAttachs              = 'ss_attachments';
  tnAttachsExt           = 'ss_attachments_%s';
  tnAttachsCnt           = 'ss_attachs_%s';

  fnAttachsCnt           = 'attachs_cnt';
  fnObjectName           = 'object_name';
  fnObjectId             = 'object_id';
  fnFilename             = 'filename';
  fnFilesize             = 'filesize';
  fnFiletime             = 'filetime';
  fnFiledata             = 'filedata';

  fmtFileSize            = '%8.2f Kb';

  idWrnFileSize          = 1102;
  idMaxFileSize          = 1103;

  defWrnFileSize         = 1024;
  defMaxFileSize         = 10240;

var
  fMaxFileSize: Integer;
  fWrnFileSize: Integer;

resourcestring
  dnAttachs              = 'Прикрепленные файлы';
  sErrOpenAttachsLookup  = 'Ошибка загрузки счетчика прикрепленных файлов!';

{ == Внешние функции =========================================================== }

procedure rAttachs_CreateAttachsFldEx(dsDataSet: TDataSet; const sKeyFieldName: string);
procedure rAttachs_CreateAttachsFld(ceEditor: TRDbCustomEditor);

function  rAttachs_OpenAttachsLookupSql(Db: TAdoConnection; dsDataSet: TDataSet; const sSQL: string): TAdoQuery;
function  rAttachs_OpenAttachsLookupEx(Db: TAdoConnection; dsDataSet: TDataSet; const sObjectName: string): TAdoQuery;
function  rAttachs_OpenAttachsLookup(Db: TAdoConnection; ceEditor: TRDbCustomEditor): TAdoQuery;

function  rAttachs_EditAttachmentsSql(Db: TAdoConnection; dsDataSet: TDataSet;
  const sSQL, sObjectName, sObjectDesc: string; const iObjectId: Integer;
  const iViewTag, iEditTag: Integer; const bEditEnabled: Boolean): Boolean;
function  rAttachs_EditAttachmentsEx(Db: TAdoConnection; dsDataSet: TDataSet;
  const sObjectName, sObjectDesc: string; const iObjectId: Integer;
  const iViewTag, iEditTag: Integer; const bEditEnabled: Boolean): Boolean;
function  rAttachs_EditAttachments(Db: TAdoConnection; ceEditor: TRDbCustomEditor;
  const iViewTag: Integer; const bEditEnabled: Boolean): Boolean;

function  rAttachs_DeleteAttachmentsEx(Db: TAdoConnection; dsDataSet: TDataSet;
  const sObjectName: string; const iObjectId: Integer; const iEditTag: Integer): Boolean;
function  rAttachs_DeleteAttachments(Db: TAdoConnection; ceEditor: TRDbCustomEditor): Boolean;

{ == Системные функции ========================================================= }

procedure rAttachs_ReadSysParameters(Db: TAdoConnection);
function  rAttachs_ItemLocate(LI: TListItem; DS: TAdoQuery): Boolean;
function  rAttachs_ItemCreate(LV: TListView; IM: TImageList; DS: TAdoQuery;
  const DefaultIcon: Integer): TListItem;
function  rAttachs_CreateAttachment(LV: TListView; IM: TImageList; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iEditTag: Integer;
  const sFileName: string): Boolean;
function  rAttachs_DeleteAttachment(LI: TListItem; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iEditTag: Integer): Boolean;
function rAttachs_SaveAttachment(LI: TListItem; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iViewTag, iEditTag: Integer;
  const sFileName: string): Boolean;

{ == Макро-команды для работы со списком вложений ============================== }

function  rAttachs_OpenData(Db: TAdoConnection;
  const sObjectName: string; const iObjectId: Integer): TAdoQuery;
function  rAttachs_LoadData(LV: TListView; IM: TImageList; DS: TAdoQuery;
  const DefaultIcon: Integer): Boolean;
function  rAttachs_FileAppend(LV: TListView; IM: TImageList; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iEditTag: Integer;
  const DefaultIcon: Integer): Boolean;
function  rAttachs_FileDelete(LV: TListView; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iEditTag: Integer): Boolean;
function  rAttachs_FileOpen(LV: TListView; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iViewTag, iEditTag: Integer): Boolean;
function  rAttachs_FileSave(LV: TListView; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iViewTag, iEditTag: Integer): Boolean;
function  rAttachs_CloneData(Db: TAdoConnection; const sObjectNameOld, sObjectNameNew: string;
  const iObjectIdOld, iObjectIdNew: Integer; const iEditTag: Integer): Boolean;

implementation

uses
  RVclUtils, RDbConst, RDbUtils, RDbGetId, RDbSettings, RListView,
  RExHandlers, RFileProcs, RDialogs, RMsgRu, RSysUtils, RWait, FileCtrl, Consts,
  {$IFDEF RSS} RDbLog, RRssConst, {$ENDIF}
  BaseDbUnit;

{$R *.dfm}

const
  sqlOpenAttachments     = 'SELECT * FROM ss_attachments WHERE object_name=''%s'' AND object_id=%d';
  sqlOpenAttachmentsCln  = 'SELECT id, filename, filesize, filetime FROM ss_attachments WHERE object_name=''%s'' AND object_id=%d';
  sqlOpenAttachsCnt      = 'SELECT object_id, Count(id) AS cnt FROM ss_attachments WHERE object_name=''%s'' GROUP BY object_id';

resourcestring
  sErrCreateAttachsFld   = 'Ошибка создания поля "%s"!';
  sErrEditAttachments    = 'Ошибка обработки прикрепленных файлов!';
  sErrDeleteAttachments  = 'Ошибка удаления прикрепленных файлов!';
  sErrCloneAttachments   = 'Ошибка клонирования прикрепленных файлов!';
  sErrFileAppend         = 'Ошибка сохранения вложения для object_name="%s", object_id="%d"!';
  sErrFileClone          = 'Ошибка клонирования вложения для object_name="%s", object_id="%d"!';
  sErrFileSave           = 'Ошибка сохранения вложения для object_name="%s", object_id="%d", id="%d", filename="%s" в файле "%s"!';
  sErrFileDelete         = 'Ошибка удаления вложения для object_name="%s", object_id="%d", id="%d"!';
  sErrAttachments        = 'Ошибка обработки вложений для object_name="%s", object_id="%d"!';

  sErrListLoad           = 'Ошибка загрузки списка вложений!';
  sErrItemCreate         = 'Ошибка добавления файла в список вложений!';
  sErrItemLocate         = 'Ошибка поиска файла в списке вложений!';
  sErrItemOpen           = 'Ошибка открытия файла из списка вложений!';

  sMsgPrepare            = 'Подготовка операции...';
  sMsgLoadFile           = 'Загрузка файла "%s"...';
  sMsgSaveFile           = 'Сохранение файла "%s"...';
  sMsgOpenFile           = 'Открытие файла "%s"...';
  sMsgDeleteFile         = 'Удаление файла "%s"...';
  sMsgCloneData          = 'Копирование списка вложений...';
  sMsgDirSelect          = 'Выберите каталог для сохранения файлов';

  sMsgAttachFileSizeWrn  = 'Файл "%s" слишком большой - %.2n кБ. Вы уверены, что хотите загрузить его в базу данных?';
  sMsgAttachFileSizeMax  = 'Файл "%s" слишком большой - %.2n кБ. Файл не будет загружен в базу данных!';

  SQryDeleteAttachment   = 'Удалить файл "%s" из списка прикрепленных файлов (вложений)?';
  SQryDeleteAttachments  = 'Удалить выделенные файлы ( %d ) из списка прикрепленных файлов (вложений)?';

{$IFDEF RSS}
  sLogOpenAttachments    = '"Вложения" [ss_attachments]: Открыт список вложений для object_name="%s", object_id="%d".';
  sLogCloseAttachments   = '"Вложения" [ss_attachments]: Закрыт список вложений для object_name="%s", object_id="%d".';
  sLogSaveAttachment     = '"Вложения" [ss_attachments]: В список вложений object_name="%s", object_id="%d" добавлен файл id="%d", filename="%s", filesize="%d", filetime="%s".';
  sLogDeleteAttachment   = '"Вложения" [ss_attachments]: Из списка вложений object_name="%s", object_id="%d" удален файл id="%d", filename="%s", filesize="%d", filetime="%s".';
  sLogOpenFileAttach     = '"Вложения" [ss_attachments]: Файл id="%d", object_name="%s", object_id="%d", filename="%s", filesize="%d", filetime="%s" открыт или запущен.';
  sLogSaveFileAttach     = '"Вложения" [ss_attachments]: Файл id="%d", object_name="%s", object_id="%d", filename="%s", filesize="%d", filetime="%s" сохранен на диск как "%s".';
{$ENDIF}

{ == Внутренние функции ======================================================== }

function _AttachmentAppend(DS: TAdoQuery;
  const ObjectName: string; ObjectId: Integer; const EditTag: Integer;
  const FileName: string): Boolean;
var
  NewId: Integer;
  BlobStream: TMemoryStream;
begin
  Result := True;
  try
    DS.Append;
    try
      {$IFDEF RSS}
      NewId := GetBlockedID(DS.Connection, tnAttachs, fnID);
      {$ELSE}
      NewId := GetNextID(DS.Connection, tnAttachs, fnID);
      {$ENDIF}
      try
        DS.FieldByName(fnID).AsInteger := NewId;
        DS.FieldByName(fnObjectName).AsString := ObjectName;
        DS.FieldByName(fnObjectId).AsInteger := ObjectId;
        DS.FieldByName(fnFilename).AsString := ExtractFileName(FileName);
        DS.FieldByName(fnFilesize).AsInteger := wFileGetSize(FileName);
        DS.FieldByName(fnFiletime).AsDateTime := FileTimeToDateTimeLocal(wFileGetLastWriteTime(FileName));
        BlobStream := TMemoryStream.Create;
        try
          BlobStream.LoadFromFile(FileName);
          BlobStream.Position := 0;
          TBlobField(DS.FieldByName(fnFiledata)).LoadFromStream(BlobStream);
        finally
          BlobStream.Free;
        end;
        DS.Post;
        {$IFDEF RSS}
          AddToDbLog(EditTag, Format(sLogSaveAttachment, [ObjectName, ObjectId,
            DS.FieldByName(fnID).AsInteger, DS.FieldByName(fnFilename).AsString,
            DS.FieldByName(fnFilesize).AsInteger, DS.FieldByName(fnFiletime).AsString]));
        {$ENDIF}
      finally
        {$IFDEF RSS}
        FreeBlockedID(DS.Connection, tnAttachs, NewId);
        {$ENDIF}
      end;
    finally
      if DS.State = dsInsert then
        DS.Cancel;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, DS, Format(sErrFileAppend, [ObjectName, ObjectId]));
    end;
  end;
end;

function _AttachmentClone(DS: TAdoQuery; const ObjectName: string;
  ObjectId: Integer; const EditTag: Integer): Boolean;
const
  sqlClone = 'INSERT INTO ss_attachments SELECT %d AS id, ''%s'' AS object_name, %d AS object_id, filename, filedata, filetime, filesize FROM ss_attachments WHERE id=%d';
var
  sSQL: string;
  NewId: Integer;
begin
  try
    NewId := BaseData.GetNewId(tnAttachs, fnID);
    sSQL := Format(sqlClone, [NewId, ObjectName, ObjectId, DS.FieldByName(fnID).AsInteger]);

    Result := ExecDynamicQuery(DS.Connection, sSQL, 30, Format(tnAttachsExt, [LowerCase(CreateUID('_'))]));
    {$IFDEF RSS}
    if Result then
    begin
      AddToDbLog(EditTag, Format(sLogSaveAttachment, [ObjectName, ObjectId,
        NewId, DS.FieldByName(fnFilename).AsString,
        DS.FieldByName(fnFilesize).AsInteger, DS.FieldByName(fnFiletime).AsString]));
    end;
    {$ENDIF}
  except
    on E: Exception do
    begin
      Result := False;
      HandleSqlExcept(E, nil, sSQL, Format(sErrFileClone, [ObjectName, ObjectId]));
    end;
  end;
end;

function _AttachmentDelete(DS: TAdoQuery;
  const ObjectName: string; ObjectId: Integer; const EditTag: Integer): Boolean;
{$IFDEF RSS}
var
  sLogMsg: string;
{$ENDIF}
begin
  Result := True;
  try
    {$IFDEF RSS}
    sLogMsg := Format(sLogDeleteAttachment, [ObjectName, ObjectId,
      DS.FieldByName(fnID).AsInteger, DS.FieldByName(fnFilename).AsString,
      DS.FieldByName(fnFilesize).AsInteger, DS.FieldByName(fnFiletime).AsString]);
    {$ENDIF}
    DS.Delete;
    {$IFDEF RSS}
    AddToDbLog(EditTag, sLogMsg);
    {$ENDIF}
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, DS, Format(sErrFileDelete, [ObjectName, ObjectId, DS.FieldByName(fnID).AsInteger]));
    end;
  end;
end;

function _AttachmentExport(DS: TAdoQuery;
  const ObjectName: string; ObjectId: Integer;
  const FileName: string): Boolean;
var
  BlobStream: TMemoryStream;
begin
  Result := True;
  try
    BlobStream := TMemoryStream.Create;
    try
      TBlobField(DS.FieldByName(fnFiledata)).SaveToStream(BlobStream);
      BlobStream.Position := 0;
      BlobStream.SaveToFile(FileName);
      if DS.FieldByName(fnFilesize).AsFloat > 0 then
        wFileSetLastWriteTime(FileName, LocalDateTimeToFileTime(DS.FieldByName(fnFiletime).AsDateTime));
    finally
      BlobStream.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, DS, Format(sErrFileSave, [ObjectName, ObjectId,
        DS.FieldByName(fnID).AsInteger, DS.FieldByName(fnFilename).AsString, FileName]));
    end;
  end;
end;

{ == Внешние функции =========================================================== }

procedure rAttachs_CreateAttachsFldEx(dsDataSet: TDataSet; const sKeyFieldName: string);
begin
  try
    if Assigned(dsDataSet) then
    begin
      with TIntegerField.Create(dsDataSet) do
      begin
        FieldName := fnAttachsCnt;
        DisplayLabel := dnAttachs;
        Name := dsDataSet.Name + fnAttachsCnt;
        DataSet := dsDataSet;
        FieldKind := fkLookup;
        KeyFields := sKeyFieldName;
        LookupCache := False;
        LookupKeyFields := fnObjectId;
        LookupResultField := fnCOUNT;
      end;
    end;
  except
    on E: Exception do
      HandleExcept(E, dsDataSet, Format(sErrCreateAttachsFld, [dnAttachs]));
  end;
end;

procedure rAttachs_CreateAttachsFld(ceEditor: TRDbCustomEditor);
begin
  rAttachs_CreateAttachsFldEx(ceEditor.DataSet, ceEditor.KeyFieldName);
end;


function rAttachs_OpenAttachsLookupSql(Db: TAdoConnection; dsDataSet: TDataSet; const sSQL: string): TAdoQuery;
begin
  Result := nil;
  try
    if Assigned(dsDataSet) and Assigned(dsDataSet.FindField(fnAttachsCnt)) then
    begin
      Result := OpenDynamicQuery(Db, sSQL, 30, Format(tnAttachsCnt, [LowerCase(CreateUID('_'))]));

      if not Assigned(dsDataSet.FindField(fnAttachsCnt).LookupDataSet) then
        dsDataSet.FindField(fnAttachsCnt).LookupDataSet := Result;
    end;
  except
    on E: Exception do
      HandleExcept(E, dsDataSet, sErrOpenAttachsLookup);
  end;
end;

function rAttachs_OpenAttachsLookupEx(Db: TAdoConnection; dsDataSet: TDataSet; const sObjectName: string): TAdoQuery;
begin
  Result := rAttachs_OpenAttachsLookupSql(Db, dsDataSet, Format(sqlOpenAttachsCnt, [sObjectName]));
end;

function rAttachs_OpenAttachsLookup(Db: TAdoConnection; ceEditor: TRDbCustomEditor): TAdoQuery;
begin
  Result := rAttachs_OpenAttachsLookupEx(Db, ceEditor.DataSet, ceEditor.GetObjectName(etView));
end;

function rAttachs_EditAttachmentsSql(Db: TAdoConnection; dsDataSet: TDataSet;
  const sSQL, sObjectName, sObjectDesc: string; const iObjectId: Integer;
  const iViewTag, iEditTag: Integer; const bEditEnabled: Boolean): Boolean;
var
  qryAttachs: TAdoQuery;
begin
  Result := False;
  try
    if Assigned(dsDataSet) then
    begin
      qryAttachs := OpenDynamicQuery(Db, sSQL, 30, Format(tnAttachsExt, [LowerCase(CreateUID('_'))]));
      try
        if DataSetIsOpen(qryAttachs) then
        begin
          rAttachs_ReadSysParameters(Db);

          with TFormDbAttachs.Create(Application.MainForm) do
          begin
            try
              fDS := qryAttachs;

              Tag := iViewTag;
              fEditTag := iEditTag;
              fEditEnabled := bEditEnabled;
              fAttachsChanged := False;
              fObjectId := iObjectId;
              fObjectName := sObjectName;
              fObjectDescr := sObjectDesc;

              if LoadData then
              begin
                ShowModal;
                Result := fAttachsChanged;
              end;
            finally
              Free;
            end;
          end;
        end;
      finally
        FreeDynamicQuery(qryAttachs);
      end;
    end;
  except
    on E: Exception do
      HandleExcept(E, dsDataSet, sErrEditAttachments);
  end;
end;

function rAttachs_EditAttachmentsEx(Db: TAdoConnection; dsDataSet: TDataSet;
  const sObjectName, sObjectDesc: string; const iObjectId: Integer;
  const iViewTag, iEditTag: Integer; const bEditEnabled: Boolean): Boolean;
begin
  Result := rAttachs_EditAttachmentsSql(Db, dsDataSet,
    Format(sqlOpenAttachments, [sObjectName, iObjectId]),
    sObjectName, sObjectDesc, iObjectId, iViewTag, iEditTag, bEditEnabled);
end;

function rAttachs_EditAttachments(Db: TAdoConnection; ceEditor: TRDbCustomEditor;
  const iViewTag: Integer; const bEditEnabled: Boolean): Boolean;
begin
  Result := rAttachs_EditAttachmentsEx(Db, ceEditor.DataSet,
    ceEditor.GetObjectName(etView), ceEditor.GetObjectDesc(etView), ceEditor.GetKeyValue,
    iViewTag, ceEditor.GetEditTag(etEdit), bEditEnabled);
end;


function rAttachs_DeleteAttachmentsEx(Db: TAdoConnection; dsDataSet: TDataSet;
  const sObjectName: string; const iObjectId: Integer; const iEditTag: Integer): Boolean;
var
  qryAttachs: TAdoQuery;
begin
  Result := True;
  try
    if Assigned(dsDataSet) then
    begin
      qryAttachs := OpenDynamicQuery(Db, Format(sqlOpenAttachments, [sObjectName, iObjectId]), 30, Format(tnAttachsExt, [LowerCase(CreateUID('_'))]));
      try
        if DataSetIsNotEmpty(qryAttachs) then
        begin
          while Result and not qryAttachs.IsEmpty do
            Result := _AttachmentDelete(qryAttachs, sObjectName, iObjectId, iEditTag);
        end;
      finally
        FreeDynamicQuery(qryAttachs);
      end;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, dsDataSet, sErrDeleteAttachments);
    end;
  end;
end;

function rAttachs_DeleteAttachments(Db: TAdoConnection; ceEditor: TRDbCustomEditor): Boolean;
begin
  Result := rAttachs_DeleteAttachmentsEx(Db, ceEditor.DataSet,
    ceEditor.GetObjectName(etView), ceEditor.GetKeyValue, ceEditor.GetEditTag(etEdit));
end;

{ == Системные функции ========================================================= }

procedure rAttachs_ReadSysParameters(Db: TAdoConnection);
begin
  try
    fWrnFileSize := ReadDbSysInteger(Db, idWrnFileSize, defWrnFileSize);
    fMaxFileSize := ReadDbSysInteger(Db, idMaxFileSize, defMaxFileSize);
  except
    on E: Exception do
      HandleExcept(E, nil, SErrLoadSystemParams);
  end;
end;

function rAttachs_ItemLocate(LI: TListItem; DS: TAdoQuery): Boolean;
begin
  try
    Result := DS.Active and not DS.IsEmpty
      and DS.Locate(fnID, GetItemId(LI), []);
    if not Result then
      raise Exception.CreateFmt(SErrDSIdNotFound, [GetItemId(LI), tnAttachs]);
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, LI.ListView, sErrItemLocate);
    end;
  end;
end;

function rAttachs_ItemCreate(LV: TListView; IM: TImageList; DS: TAdoQuery;
  const DefaultIcon: Integer): TListItem;
var
  Id: TId;
  Hndl: HIcon;
  Icon: TIcon;
begin
  Result := LV.Items.Add;
  try
    with Result do
    begin
      New(Id);
      Data := Id;
      ImageIndex := DefaultIcon;
      Id^ := DS.FieldByName(fnID).AsInteger;
      Caption := DS.FieldByName(fnFilename).AsString;
      Subitems.Add(Format(fmtFileSize, [DS.FieldByName(fnFilesize).AsFloat / 1024]));
      Subitems.Add(DS.FieldByName(fnFiletime).DisplayText);

      Icon := TIcon.Create;
      try
        Hndl := GetAssociatedIconHandle(DS.FieldByName(fnFilename).AsString);
        if Hndl > 0 then
        begin
          Icon.Handle := Hndl;
          ImageIndex := IM.AddIcon(Icon);
          DestroyIcon(Hndl);
        end;
      finally
        Icon.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(Result) then
      begin
        Result.Free;
        Result := nil;
      end;
      HandleExcept(E, LV, sErrItemCreate);
    end;
  end;
end;

function rAttachs_CreateAttachment(LV: TListView; IM: TImageList; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iEditTag: Integer;
  const sFileName: string): Boolean;
var
  fFileSizeKb: Double;
begin
  Result := True;
  fFileSizeKb := wFileGetSize(sFileName) / 1024;
  if (fMaxFileSize = 0) or (fFileSizeKb <= fMaxFileSize) then
  begin
    if (fWrnFileSize = 0) or (fFileSizeKb <= fWrnFileSize)
    or (QueryBoxStdNY(Format(sMsgAttachFileSizeWrn,
      [ExtractFileName(sFileName), fFileSizeKb])) = ID_YES) then
    begin
      Result := _AttachmentAppend(DS, sObjectName, iObjectId, iEditTag, sFileName);
      if Result then
        LV.Selected := rAttachs_ItemCreate(LV, IM, DS, 0);
    end;
  end
  else ErrorBox(Format(sMsgAttachFileSizeMax,
    [ExtractFileName(sFileName), fFileSizeKb]));
end;

function rAttachs_DeleteAttachment(LI: TListItem; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iEditTag: Integer): Boolean;
begin
  Result := rAttachs_ItemLocate(LI, DS)
        and _AttachmentDelete(DS, sObjectName, iObjectId, iEditTag);
end;

function rAttachs_OpenAttachment(LI: TListItem; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iViewTag, iEditTag: Integer): Boolean;
var
  sTempFile: string;
begin
  Result := rAttachs_ItemLocate(LI, DS);
  if Result then
  begin
    try
      sTempFile := IncludeTrailingPathDelimiter(GetTempDirEx) + DS.FieldByName(fnFilename).AsString;
      if _AttachmentExport(DS, sObjectName, iObjectId, sTempFile) then
      begin
        try
          {$IFDEF RSS}
          AddToDbLog(iViewTag, Format(sLogOpenFileAttach,
            [DS.FieldByName(fnID).AsInteger, sObjectName, iObjectId,
             DS.FieldByName(fnFilename).AsString, DS.FieldByName(fnFilesize).AsInteger,
             DS.FieldByName(fnFiletime).AsString]));
          {$ENDIF}
          OpenFile32('', sTempFile, '', SW_NORMAL, False, nil, 360);
        finally
          // if FileExists(sTempFile) then
          //  SysUtils.DeleteFile(sTempFile);
        end;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, LI.ListView, sErrItemOpen);
      end;
    end;
  end;
end;

function rAttachs_SaveAttachment(LI: TListItem; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iViewTag, iEditTag: Integer;
  const sFileName: string): Boolean;
begin
  Result := rAttachs_ItemLocate(LI, DS)
        and _AttachmentExport(DS, sObjectName, iObjectId, sFileName);
  {$IFDEF RSS}
  AddToDbLog(iViewTag, Format(SLogSaveFileAttach,
    [DS.FieldByName(fnID).AsInteger, sObjectName, iObjectId,
     DS.FieldByName(fnFilename).AsString, DS.FieldByName(fnFilesize).AsInteger,
     DS.FieldByName(fnFiletime).AsString, sFileName]));
  {$ENDIF}
end;

{ == Внутренние команды для работы со списком вложений ========================= }

function rAttachs_OpenData(Db: TAdoConnection;
  const sObjectName: string; const iObjectId: Integer): TAdoQuery;
begin
  Result := OpenDynamicQuery(Db, Format(sqlOpenAttachments, [sObjectName, iObjectId]), 30, Format(tnAttachsExt, [LowerCase(CreateUID('_'))]));
end;

function rAttachs_LoadData(LV: TListView; IM: TImageList; DS: TAdoQuery;
  const DefaultIcon: Integer): Boolean;
begin
  try
    Result := Assigned(DS) and DS.Active;
    if Result then
    begin
      LV.Items.BeginUpdate;
      try
        LV.Items.Clear;
        DS.First;
        while not DS.Eof do
        begin
          rAttachs_ItemCreate(LV, IM, DS, DefaultIcon);
          DS.Next;
        end;
      finally
        LV.Items.EndUpdate;
      end;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, LV, sErrListLoad);
    end;
  end;
end;

function rAttachs_FileAppend(LV: TListView; IM: TImageList; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iEditTag: Integer;
  const DefaultIcon: Integer): Boolean;
var
  i: Integer;
  OpenDialog: TOpenDialog;
begin
  Result := False;

  OpenDialog := TOpenDialog.Create(Application);
  try
    OpenDialog.Options := [ofAllowMultiSelect,ofPathMustExist,ofFileMustExist,ofEnableSizing];
    OpenDialog.Filter := SDefaultFilter;
    if OpenDialog.Execute then
    begin
      StartWait;
      ShowWaitMsg(sMsgPrepare);
      ShowInStatusBar(sMsgPrepare);
      try
        for i := 0 to OpenDialog.Files.Count - 1 do
        begin
          ShowWaitMsg(Format(sMsgLoadFile, [ExtractFileName(OpenDialog.Files[i])]));
          ShowInStatusBar(Format(sMsgLoadFile, [ExtractFileName(OpenDialog.Files[i])]));
          if rAttachs_CreateAttachment(LV, IM, DS, sObjectName, iObjectId, iEditTag, OpenDialog.Files[i]) then
            Result := True;
        end;
      finally
        CloseWaitMsg;
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

function rAttachs_FileDelete(LV: TListView; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iEditTag: Integer): Boolean;
var
  QryText: string;
  SelItem: TListItem;
begin
  Result := False;

  if LV.MultiSelect
  then QryText := Format(sQryDeleteAttachments, [LV.SelCount])
  else QryText := Format(sQryDeleteAttachment, [LV.Selected.Caption]);
  if DeleteQueryText(QryText) then
  begin
    StartWait;
    ShowWaitMsg(sMsgPrepare);
    ShowInStatusBar(sMsgPrepare);
    try
      try
        SelItem := LV.Selected;
        while SelItem <> nil do
        begin
          ShowWaitMsg(Format(sMsgDeleteFile, [SelItem.Caption]));
          ShowInStatusBar(Format(sMsgDeleteFile, [SelItem.Caption]));
          if rAttachs_DeleteAttachment(SelItem, DS, sObjectName, iObjectId, iEditTag) then
          begin
            Result := True;
            SelItem := LV.GetNextItem(SelItem, sdAll, [isSelected]);
          end
          else begin
            SelItem.Selected := False;
            Break;
          end;
        end;
      finally
        LV.DeleteSelected;
      end;
    finally
      CloseWaitMsg;
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

function rAttachs_FileOpen(LV: TListView; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iViewTag, iEditTag: Integer): Boolean;
var
  SelItem: TListItem;
begin
  Result := True;

  StartWait;
  ShowWaitMsg(sMsgPrepare);
  ShowInStatusBar(sMsgPrepare);
  try
    SelItem := LV.Selected;
    while Result and (SelItem <> nil) do
    begin
      ShowWaitMsg(Format(sMsgOpenFile, [SelItem.Caption]));
      ShowInStatusBar(Format(sMsgOpenFile, [SelItem.Caption]));
      Result := rAttachs_OpenAttachment(SelItem, DS, sObjectName, iObjectId, iViewTag, iEditTag);
      SelItem := LV.GetNextItem(SelItem, sdAll, [isSelected]);
    end;
  finally
    CloseWaitMsg;
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

function rAttachs_FileSave(LV: TListView; DS: TAdoQuery;
  const sObjectName: string; const iObjectId, iViewTag, iEditTag: Integer): Boolean;
var
  SaveDialog: TSaveDialog;
  SelItem: TListItem;
  DirName: string;
begin
  Result := False;

  if LV.SelCount > 1 then
  begin
    if SelectDirectory(sMsgDirSelect, EmptyStr, DirName) then
    begin
      StartWait;
      ShowWaitMsg(sMsgPrepare);
      ShowWaitMsg(sMsgPrepare);
      try
        SelItem := LV.Selected;
        while SelItem <> nil do
        begin
          ShowWaitMsg(Format(sMsgSaveFile, [SelItem.Caption]));
          ShowInStatusBar(Format(sMsgSaveFile, [SelItem.Caption]));
          if not rAttachs_SaveAttachment(SelItem, DS, sObjectName, iObjectId, iViewTag, iEditTag,
              IncludeTrailingPathDelimiter(DirName) + SelItem.Caption) then
          begin
            Result := True;
            SelItem.Selected := False;
          end;
          SelItem := LV.GetNextItem(SelItem, sdAll, [isSelected]);
        end;
      finally
        CloseWaitMsg;
        ShowInStatusBar(EmptyStr);
        StopWait;
      end;
    end;
  end
  else begin
    SelItem := LV.Selected;
    SaveDialog := TSaveDialog.Create(Application);
    try
      SaveDialog.Options := [ofOverwritePrompt,ofExtensionDifferent,ofPathMustExist,ofEnableSizing];
      SaveDialog.FileName := SelItem.Caption;
      if SaveDialog.Execute then
      begin
        StartWait;
        ShowWaitMsg(Format(sMsgSaveFile, [ExtractFileName(SaveDialog.FileName)]));
        ShowInStatusBar(Format(sMsgSaveFile, [ExtractFileName(SaveDialog.FileName)]));
        try
          Result := rAttachs_SaveAttachment(SelItem, DS, sObjectName, iObjectId, iViewTag, iEditTag,
            SaveDialog.FileName);
        finally
          CloseWaitMsg;
          ShowInStatusBar(EmptyStr);
          StopWait;
        end;
      end;
    finally
      SaveDialog.Free;
    end;
  end;
end;

function  rAttachs_CloneData(Db: TAdoConnection; const sObjectNameOld, sObjectNameNew: string;
  const iObjectIdOld, iObjectIdNew: Integer; const iEditTag: Integer): Boolean;
var
  qryOldData: TAdoQuery;
begin
  Result := True;
  try
    StartWait;
    ShowWaitMsg(sMsgCloneData);
    ShowInStatusBar(sMsgCloneData);
    try
      qryOldData := OpenDynamicQuery(Db, Format(sqlOpenAttachmentsCln, [sObjectNameOld, iObjectIdOld]), 30, Format(tnAttachsExt, [LowerCase(CreateUID('_'))]));
      if DataSetIsNotEmpty(qryOldData) then
      begin
        qryOldData.First;
        while Result and not qryOldData.Eof do
        begin
          Result := _AttachmentClone(qryOldData, sObjectNameNew, iObjectIdNew, iEditTag);
          qryOldData.Next;
        end;
      end;
    finally
      CloseWaitMsg;
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, nil, sErrCloneAttachments);
    end;
  end;
end;

{ == TFormDbAttachs ============================================================ }

{$IFDEF RSS}
procedure TFormDbAttachs.LogOnOpen;
begin
  AddToDbLog(Tag, Format(sLogOpenAttachments, [fObjectName, fObjectId]));
end;
{$ENDIF}

{$IFDEF RSS}
procedure TFormDbAttachs.LogOnClose;
begin
  AddToDbLog(Tag, Format(sLogCloseAttachments, [fObjectName, fObjectId]));
end;
{$ENDIF}

function TFormDbAttachs.GetDetailName: string;
begin
  Result := tnAttachs;
end;

function TFormDbAttachs.LoadDataForm: Boolean;
begin
  Result := rAttachs_LoadData(ListView, ImageList, fDS, 0);
end;

procedure TFormDbAttachs.ShowItemsCount;
begin
  StatusBar.Panels[0].Text := fObjectDescr;
  StatusBar.Panels[1].Text := Format('Id: %d', [fObjectId]);
  StatusBar.Panels[2].Text := Format(SItemsCount, [ListView.Items.Count])
end;

procedure TFormDbAttachs.FileAppendUpdate(Sender: TObject);
begin
  FileAppend.Enabled := IsNotWait and fEditEnabled and DataSetIsOpen(fDS);
end;

procedure TFormDbAttachs.FileAppendExecute(Sender: TObject);
begin
  try
    if rAttachs_FileAppend(ListView, ImageList, fDS, fObjectName, fObjectId, fEditTag, 0) then
    begin
      if not fAttachsChanged then
        fAttachsChanged := True;
    end;
  finally
    ShowItemsCount;
  end;
end;

procedure TFormDbAttachs.FileDeleteUpdate(Sender: TObject);
begin
  FileDelete.Enabled := IsNotWait and (ListView.SelCount > 0) and fEditEnabled and DataSetIsOpen(fDS);
end;

procedure TFormDbAttachs.FileDeleteExecute(Sender: TObject);
begin
  try
    if rAttachs_FileDelete(ListView, fDS, fObjectName, fObjectId, fEditTag) then
    begin
      if not fAttachsChanged then
        fAttachsChanged := True;
    end;
  finally
    ShowItemsCount;
  end;
end;

procedure TFormDbAttachs.FileOpenUpdate(Sender: TObject);
begin
  FileOpen.Enabled := IsNotWait and (ListView.SelCount > 0) and DataSetIsOpen(fDS);
end;

procedure TFormDbAttachs.FileOpenExecute(Sender: TObject);
begin
  try
    rAttachs_FileOpen(ListView, fDS, fObjectName, fObjectId, Tag, fEditTag);
  finally
    ShowItemsCount;
  end;
end;

procedure TFormDbAttachs.ListViewDblClick(Sender: TObject);
begin
  if IsNotWait and (ListView.SelCount > 0) and DataSetIsOpen(fDS) then
    FileOpenExecute(Sender);
end;

procedure TFormDbAttachs.FileSaveUpdate(Sender: TObject);
begin
  FileSave.Enabled := IsNotWait and (ListView.SelCount > 0) and DataSetIsOpen(fDS);
end;

procedure TFormDbAttachs.FileSaveExecute(Sender: TObject);
begin
  try
    rAttachs_FileSave(ListView, fDS, fObjectName, fObjectId, Tag, fEditTag);
  finally
    ShowItemsCount;
  end;
end;

end.
