unit RRssAttachs;

interface

uses
  Classes, SysUtils, Controls, ComCtrls, Db, AdoDb;

type
  TRssDbAttachments = class
  private
    fDb: TAdoConnection;
    fDs: TAdoQuery;
    fObjectName: string;
    fRecordName: string;
    fObjectId: Integer;
    fViewTag: Integer;
    fEditTag: Integer;
    fEditEnabled: Boolean;
    fMaxFileSize: Integer;
    fWrnFileSize: Integer;
    function  GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function  GetDetailName: string;
    function  GetRecordCount: Integer;
  protected
    function  _ItemCreate(LV: TListView; const DefaultIcon: Integer): TListItem;
    function  _ItemLocate(LI: TListItem): Boolean;
    function  _FileAppend(const FileName: string): Boolean;
    function  _FileSave(const FileName: string): Boolean;
    function  _FileDelete: Boolean;
  public
    constructor Create(Db: TAdoConnection;
      const ObjectName, RecordName: string;
      const ObjectId, ViewTag, EditTag: Integer;
      const EditEnabled: Boolean);
    destructor Destroy; override;
    function  Open(const Logged: Boolean): Boolean;
    procedure Close(const Logged: Boolean);
    function  IsRssMode: Boolean;
    function  LoadAttachments(LV: TListView; const DefaultIcon: Integer): Boolean;
    function  CreateAttachment(LV: TListView; const FileName: string; const DefaultIcon: Integer): Boolean;
    function  OpenAttachment(LI: TListItem): Boolean;
    function  SaveAttachment(LI: TListItem; const FileName: string): Boolean;
    function  SaveAttachments(const DirName: string): Boolean;
    function  DeleteAttachment(LI: TListItem): Boolean;
    function  FreeAttachments: Boolean;
    function  IsEditEnable: Boolean;
  published
    property Db: TAdoConnection read fDb write fDb;
    property Active: Boolean read GetActive write SetActive;
    property ObjectName: string read fObjectName write fObjectName;
    property RecordName: string read fRecordName write fRecordName;
    property DetailName: string read GetDetailName;
    property RecordCount: Integer read GetRecordCount; 
    property ObjectId: Integer read fObjectId write fObjectId;
    property ViewTag: Integer read fViewTag write fViewTag;
    property EditTag: Integer read fEditTag write fEditTag;
    property EditEnabled: Boolean read fEditEnabled write fEditEnabled;
    property MaxFileSize: Integer read fMaxFileSize write fMaxFileSize;
    property WrnFileSize: Integer read fWrnFileSize write fWrnFileSize;
  end;

implementation

uses
  Windows, Graphics,
  RVclUtils, RDbConst, RDbGetId, RDbLog, RDbOpenDS, RSysUtils, RListView,
  RFileUtils, RExHandlers, RMsgRu, RDialogs, RDbSettings;

const
  sqlOpenAttachments    = 'SELECT * FROM ss_attachments WHERE object_name=''%s'' AND object_id=%d';
  sqlGetAttachmentsCnt  = 'SELECT object_id, Count(id) as CNT FROM ss_attachments WHERE object_name=''%s'' GROUP BY object_id';

  tnTableAttachments    = 'ss_attachments';

  fnOBJECT_NAME         = 'object_name';
  fnOBJECT_ID           = 'object_id';
  fnFILENAME            = 'filename';
  fnFILESIZE            = 'filesize';
  fnFILETIME            = 'filetime';
  fnFILEDATA            = 'filedata';

  SFileSize             = '%8.2f Kb';

  IdWrnFileSize         = 1102;
  IdMaxFileSize         = 1103;

  DefWrnFileSize        = 1024;
  DefMaxFileSize        = 10240;

resourcestring
  SLogOpenAttachments   = '"Вложения" [ss_attachments]: Открыт список вложений для object_name="%s", object_id="%d".';
  SLogCloseAttachments  = '"Вложения" [ss_attachments]: Закрыт список вложений для object_name="%s", object_id="%d".';
  SLogSaveAttachment    = '"Вложения" [ss_attachments]: В список вложений object_name="%s", object_id="%d" добавлен файл id="%d", filename="%s", filesize="%d", filetime="%s".';
  SLogDeleteAttachment  = '"Вложения" [ss_attachments]: Из списка вложений object_name="%s", object_id="%d" удален файл id="%d", filename="%s", filesize="%d", filetime="%s".';
  SLogOpenFileAttach    = '"Вложения" [ss_attachments]: Файл id="%d", object_name="%s", object_id="%d", filename="%s", filesize="%d", filetime="%s" открыт или запущен.';
  SLogSaveFileAttach    = '"Вложения" [ss_attachments]: Файл id="%d", object_name="%s", object_id="%d", filename="%s", filesize="%d", filetime="%s" сохранен на диск как "%s".';

  SErrFileAppend        = 'Ошибка сохранения вложения для object_name="%s", object_id="%d"!';
  SErrFileSave          = 'Ошибка сохранения вложения для object_name="%s", object_id="%d", id="%d", filename="%s" в файле "%s"!';
  SErrFileDelete        = 'Ошибка удаления вложения для object_name="%s", object_id="%d", id="%d"!';
  SErrAttachments       = 'Ошибка обработки вложений для object_name="%s", object_id="%d"!';

  SMsgAttachFileSizeWrn = 'Файл "%s" слишком большой - %.2n кБ. Вы уверены, что хотите загрузить его в базу данных?';
  SMsgAttachFileSizeMax = 'Файл "%s" слишком большой - %.2n кБ. Файл не будет загружен в базу данных!';

{ TDbAttachments }

constructor TRssDbAttachments.Create(Db: TAdoConnection;
  const ObjectName, RecordName: string;
  const ObjectId, ViewTag, EditTag: Integer;
  const EditEnabled: Boolean);
begin
  inherited Create;
  fDb := Db;
  fDs := TAdoQuery.Create(nil);
  fObjectName := ObjectName;
  fRecordName := RecordName;
  fObjectId := ObjectId;
  fViewTag := ViewTag;
  fEditTag := EditTag;
  fEditEnabled := EditEnabled;
  fWrnFileSize := ReadDbSysInteger(Db, IdWrnFileSize, DefWrnFileSize);
  fMaxFileSize := ReadDbSysInteger(Db, IdMaxFileSize, DefMaxFileSize);
end;

destructor TRssDbAttachments.Destroy;
begin
  Close(True);
  fDs.Free;
  fDb := nil;
  inherited Destroy;
end;

function TRssDbAttachments.GetActive: Boolean;
begin
  Result := Assigned(fDb) and fDb.Connected
    and fDs.Active;
end;

procedure TRssDbAttachments.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then
  begin
    if Value
    then Open(True)
    else Close(True);
  end;
end;

function TRssDbAttachments.GetDetailName: string;
begin
  Result := tnTableAttachments;
end;

function TRssDbAttachments.IsRssMode: Boolean;
begin
  Result := fDb.Tag > -1;
end;

function TRssDbAttachments.Open(const Logged: Boolean): Boolean;
begin
  Result := Assigned(fDb) and fDb.Connected and OpenDS_StaticQuery(fDb, fDs,
    Format(sqlOpenAttachments, [fObjectName, fObjectId]), False, fnID, fEditTag);
  if Result and IsRssMode and Logged then
    AddToDbLog(fViewTag, Format(SLogOpenAttachments, [fObjectName, fObjectId]));
end;

procedure TRssDbAttachments.Close(const Logged: Boolean);
begin
  if fDs.Active then
  begin
    fDs.Close;
    if IsRssMode and Logged then
      AddToDbLog(fViewTag, Format(SLogCloseAttachments, [fObjectName, fObjectId]));
  end;
end;

function TRssDbAttachments._ItemCreate(LV: TListView; const DefaultIcon: Integer): TListItem;
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
      Id^ := fDs.FieldByName(fnID).AsInteger;
      Caption := fDs.FieldByName(fnFILENAME).AsString;
      Subitems.Add(Format(SFileSize, [fDs.FieldByName(fnFILESIZE).AsFloat / 1024]));
      Subitems.Add(fDs.FieldByName(fnFILETIME).DisplayText);
      if Assigned(LV.SmallImages) then
      begin
        Icon := TIcon.Create;
        try
          Hndl := GetAssociatedIconHandle(fDs.FieldByName(fnFILENAME).AsString);
          if Hndl > 0 then
          begin
            Icon.Handle := Hndl;
            ImageIndex := LV.SmallImages.AddIcon(Icon);
            DestroyIcon(Hndl);
          end;
        finally
          Icon.Free;
        end;
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
      HandleExcept(E, Self, Format(SErrAttachments, [fObjectName, fObjectId]));
    end;
  end;
end;

function TRssDbAttachments._ItemLocate(LI: TListItem): Boolean;
begin
  try
    Result := fDs.Active and not fDs.IsEmpty
      and fDs.Locate(fnID, GetItemId(LI), []);
    if not Result then
      raise Exception.CreateFmt(SErrDSIdNotFound, [GetItemId(LI), tnTableAttachments]);
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, Format(SErrAttachments, [fObjectName, fObjectId]));
    end;
  end;
end;

function TRssDbAttachments._FileAppend(const FileName: string): Boolean;
var
  NewId: Integer;
  BlobStream: TMemoryStream;
begin
  Result := True;
  try
    fDs.Append;
    try
      if IsRssMode
      then NewId := GetBlockedID(fDb, tnTableAttachments, fnID)
      else NewId := GetNextID(fDb, tnTableAttachments, fnID);
      try
        fDs.FieldByName(fnID).AsInteger := NewId;
        fDs.FieldByName(fnOBJECT_NAME).AsString := fObjectName;
        fDs.FieldByName(fnOBJECT_ID).AsInteger := fObjectId;
        fDs.FieldByName(fnFILENAME).AsString := ExtractFileName(FileName);
        fDs.FieldByName(fnFILESIZE).AsInteger := GetFileSize(FileName);
        fDs.FieldByName(fnFILETIME).AsDateTime := FileDateTime(FileName);
        BlobStream := TMemoryStream.Create;
        try
          BlobStream.LoadFromFile(FileName);
          BlobStream.Position := 0;
          TBlobField(fDs.FieldByName(fnFILEDATA)).LoadFromStream(BlobStream);
        finally
          BlobStream.Free;
        end;
        fDs.Post;
        if IsRssMode then
          AddToDbLog(fEditTag, Format(SLogSaveAttachment, [fObjectName, fObjectId,
            fDs.FieldByName(fnID).AsInteger, fDs.FieldByName(fnFILENAME).AsString,
            fDs.FieldByName(fnFILESIZE).AsInteger, fDs.FieldByName(fnFILETIME).AsString]));
      finally
        if IsRssMode then
          FreeBlockedID(fDb, tnTableAttachments, NewId);
      end;
    finally
      if fDs.State = dsInsert then
        fDs.Cancel;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, Format(SErrFileAppend, [fObjectName, fObjectId]));
    end;
  end;
end;

function TRssDbAttachments._FileSave(const FileName: string): Boolean;
var
  BlobStream: TMemoryStream;
begin
  Result := True;
  try
    BlobStream := TMemoryStream.Create;
    try
      TBlobField(fDs.FieldByName(fnFILEDATA)).SaveToStream(BlobStream);
      BlobStream.Position := 0;
      BlobStream.SaveToFile(FileName);
      if fDs.FieldByName(fnFILESIZE).AsFloat > 0 then
        SetFileDateTime(FileName, fDs.FieldByName(fnFILETIME).AsDateTime);
    finally
      BlobStream.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, Format(SErrFileSave, [fObjectName, fObjectId,
        fDs.FieldByName(fnID).AsInteger, fDs.FieldByName(fnFILENAME).AsString, FileName]));
    end;
  end;
end;

function TRssDbAttachments._FileDelete: Boolean;
var
  LogMsg: string;
begin
  Result := True;
  try
    LogMsg := Format(SLogDeleteAttachment, [fObjectName, fObjectId,
      fDs.FieldByName(fnID).AsInteger, fDs.FieldByName(fnFILENAME).AsString,
      fDs.FieldByName(fnFILESIZE).AsInteger, fDs.FieldByName(fnFILETIME).AsString]);
    fDs.Delete;
    if IsRssMode then
      AddToDbLog(fEditTag, LogMsg);
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, Format(SErrFileDelete, [fObjectName, fObjectId,
        fDs.FieldByName(fnID).AsInteger]));
    end;
  end;
end;

function TRssDbAttachments.LoadAttachments(LV: TListView; const DefaultIcon: Integer): Boolean;
begin
  Result := Open(False);
  if Result then
  begin
    LV.Items.BeginUpdate;
    try
      LV.Items.Clear;
      fDs.First;
      while not fDs.Eof do
      begin
        _ItemCreate(LV, DefaultIcon);
        fDs.Next;
      end;
    finally
      LV.Items.EndUpdate;
    end;
  end;
end;

function TRssDbAttachments.CreateAttachment(LV: TListView; const FileName: string; const DefaultIcon: Integer): Boolean;
var
  fFileSizeKb: Double;
begin
  Result := True;
  fFileSizeKb := GetFileSize(FileName) / 1024;
  if (fMaxFileSize = 0) or (fFileSizeKb <= fMaxFileSize) then
  begin
    if (fWrnFileSize = 0) or (fFileSizeKb <= fWrnFileSize)
    or (QueryBoxStdNY(Format(SMsgAttachFileSizeWrn,
      [ExtractFileName(FileName), fFileSizeKb])) = ID_YES) then
    begin
      Result := _FileAppend(FileName);
      if Result then
        LV.Selected := _ItemCreate(LV, DefaultIcon);
    end;
  end
  else ErrorBox(Format(SMsgAttachFileSizeMax,
    [ExtractFileName(FileName), fFileSizeKb]));
end;

function TRssDbAttachments.OpenAttachment(LI: TListItem): Boolean;
var
  sTempFile: string;
begin
  Result := _ItemLocate(LI);
  if Result then
  begin
    try
      sTempFile := IncludeTrailingPathDelimiter(GetTempDirEx) + fDs.FieldByName(fnFILENAME).AsString;
      if _FileSave(sTempFile) then
      begin
        try
          if IsRssMode then
            AddToDbLog(fViewTag, Format(SLogOpenFileAttach,
              [fDs.FieldByName(fnID).AsInteger, fObjectName, fObjectId,
               fDs.FieldByName(fnFILENAME).AsString, fDs.FieldByName(fnFILESIZE).AsInteger,
               fDs.FieldByName(fnFILETIME).AsString]));
          OpenFile32('', sTempFile, '', SW_NORMAL, True, nil, 360);
        finally
          // if FileExists(sTempFile) then
          //  SysUtils.DeleteFile(sTempFile);
        end;
      end;
    except
      on E: Exception do
      begin
        Result := False;
        HandleExcept(E, Self, Format(SErrAttachments, [fObjectName, fObjectId]));
      end;
    end;
  end;
end;

function TRssDbAttachments.SaveAttachment(LI: TListItem; const FileName: string): Boolean;
begin
  Result := _ItemLocate(LI) and _FileSave(FileName);
  if Result and IsRssMode then
    AddToDbLog(fViewTag, Format(SLogSaveFileAttach,
      [fDs.FieldByName(fnID).AsInteger, fObjectName, fObjectId,
       fDs.FieldByName(fnFILENAME).AsString, fDs.FieldByName(fnFILESIZE).AsInteger,
       fDs.FieldByName(fnFILETIME).AsString, FileName]));
end;

function TRssDbAttachments.SaveAttachments(const DirName: string): Boolean;
var
  bCloseAfterFree: Boolean;
begin
  bCloseAfterFree := not Active;
  try
    Result := Open(False);
    while Result and not fDs.IsEmpty do
      Result := _FileSave(IncludeTrailingPathDelimiter(DirName) + fDs.FieldByName(fnFILENAME).AsString);
  finally
    if bCloseAfterFree then
      Close(False);
  end;
end;

function TRssDbAttachments.DeleteAttachment(LI: TListItem): Boolean;
begin
  Result := _ItemLocate(LI) and _FileDelete;
end;

function TRssDbAttachments.FreeAttachments: Boolean;
var
  bCloseAfterFree: Boolean;
begin
  bCloseAfterFree := not Active;
  try
    Result := Open(False);
    while Result and not fDs.IsEmpty do
      Result := _FileDelete;
  finally
    if bCloseAfterFree then
      Close(False);
  end;
end;

function TRssDbAttachments.IsEditEnable: Boolean;
begin
  Result := EditEnabled and Active and (fDs.State in [dsBrowse]);
end;

function TRssDbAttachments.GetRecordCount: Integer;
begin
  Result := 0;
  if Active then
    Result := fDs.RecordCount;
end;

end.
