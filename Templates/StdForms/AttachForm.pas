unit AttachForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TmplDialog, StdCtrls, Buttons, ExtCtrls, ComCtrls, RavListView,
  ActnList, Menus, Grids, DBGrids, DB, ADODB, TmplList, ToolWin, ImgList;

type
  TFormAttach = class(TListTemplate)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    SS_ATTACHMENTS: TADOQuery;
    SS_ATTACHMENTSID: TIntegerField;
    SS_ATTACHMENTSOBJECT_NAME: TStringField;
    SS_ATTACHMENTSOBJECT_ID: TIntegerField;
    SS_ATTACHMENTSFILENAME: TStringField;
    SS_ATTACHMENTSFILEDATA: TBlobField;
    SS_ATTACHMENTSFILETIME: TDateTimeField;
    SS_ATTACHMENTSFILESIZE: TIntegerField;
    AddFile: TAction;
    DelFile: TAction;
    AddFileToolButton: TToolButton;
    DelFileToolButton: TToolButton;
    itemAddFile: TMenuItem;
    itemDelFile: TMenuItem;
    divEdit: TMenuItem;
    itemAddFileP: TMenuItem;
    itemDelFileP: TMenuItem;
    divPopupEdit: TMenuItem;
    EditSeparator: TToolButton;
    CloseCancelToolButton: TToolButton;
    OpenFile: TAction;
    SaveAsFile: TAction;
    OpenFileToolButton: TToolButton;
    SaveAsFileToolButton: TToolButton;
    FileSeparator: TToolButton;
    itemOpenFile: TMenuItem;
    itemSaveAsFile: TMenuItem;
    divOpen: TMenuItem;
    itemOpenFileP: TMenuItem;
    itemSaveAsFileP: TMenuItem;
    divPopupOpen: TMenuItem;
    RefreshToolButton: TToolButton;
    RefreshSeparator: TToolButton;
    ImageList: TImageList;
    procedure AddFileUpdate(Sender: TObject);
    procedure AddFileExecute(Sender: TObject);
    procedure DelFileUpdate(Sender: TObject);
    procedure DelFileExecute(Sender: TObject);
    procedure OpenFileUpdate(Sender: TObject);
    procedure OpenFileExecute(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure SaveAsFileUpdate(Sender: TObject);
    procedure SaveAsFileExecute(Sender: TObject);
  protected
    function  GetDetailEditTag: Integer; override;
    function  GetDetailName: string; override;
    procedure LogOnOpen; override;
    procedure LogOnClose; override;
    function  CreateAttachmentItem: TListItem;
    function  LocateAttachmentItem(const Item: TListItem): Boolean;
    function  LoadDataForm: Boolean; override;
  public
    ObjectName: string;
    RecordName: string;
    ObjectId: Integer;
    procedure ShowItemsCount; override;
    function OpenAttachments: Boolean;
    function DeleteAttachment: Boolean;
    function FreeAttachments: Boolean;
    function AddAttachments(const FileName: string): Boolean;
    function SaveAttachments(const AsFile: string): Boolean;
  end;

procedure ShowAttachments(const ObjName, RecName: string; const ObjId, ViewTag, EditTag: Integer);
function  DeleteAttachments(const ObjName: string; const ObjId, ViewTag, EditTag: Integer): Boolean;

implementation

{$R *.dfm}

uses
  RVclUtils, RDialogs, RRssConst, RDbLog, RDbConst, RDbUtils, RMsgRu,
  RFileUtils, RListView, RExHandlers, RSysUtils, BaseDbUnit;

resourcestring
  SRecTitleName         = 'Вложения для записи "%s": всего %d файл(ов)';
  SRecTitleCnt          = 'Вложения: всего %d файл(ов)';

  SMsgAttachFileSizeWrn = 'Файл "%s" слишком большой - %.2n кБ. Вы уверены, что хотите загрузить его в базу данных?';
  SMsgAttachFileSizeMax = 'Файл "%s" слишком большой - %.2n кБ. Файл не будет загружен в базу данных!';

  SLogOpenAttachments   = '"Вложения" [SS_ATTACHMENTS]: Открыт список вложений для OBJECT_NAME="%s", OBJECT_ID="%d".';
  SLogCloseAttachments  = '"Вложения" [SS_ATTACHMENTS]: Закрыт список вложений для OBJECT_NAME="%s", OBJECT_ID="%d".';
  SLogSaveAttachment    = '"Вложения" [SS_ATTACHMENTS]: В список вложений OBJECT_NAME="%s", OBJECT_ID="%d" добавлен файл ID="%d", FILENAME="%s", FILESIZE="%d", FILETIME="%s".';
  SLogDeleteAttachment  = '"Вложения" [SS_ATTACHMENTS]: Из списка вложений OBJECT_NAME="%s", OBJECT_ID="%d" удален файл ID="%d", FILENAME="%s", FILESIZE="%d", FILETIME="%s".';
  SLogOpenFileAttach    = '"Вложения" [SS_ATTACHMENTS]: Файл ID="%d", OBJECT_NAME="%s", OBJECT_ID="%d", FILENAME="%s", FILESIZE="%d", FILETIME="%s" открыт или запущен.';
  SLogSaveFileAttach    = '"Вложения" [SS_ATTACHMENTS]: Файл ID="%d", OBJECT_NAME="%s", OBJECT_ID="%d", FILENAME="%s", FILESIZE="%d", FILETIME="%s" сохранен на диск как "%s".';

  SQryDeleteAttachment  = 'Удалить файл "%s" из списка прикрепленных файлов (вложений)?';
  SQryDeleteAttachments = 'Удалить выделенные файлы ( %d ) из списка прикрепленных файлов (вложений)?';

  SErrOperAttachment    = 'Ошибка операции с вложениями для OBJECT_NAME="%s", OBJECT_ID="%d"!';
  SErrSaveAttachment    = 'Ошибка сохранения вложения для OBJECT_NAME="%s", OBJECT_ID="%d"!';
  SErrDeleteAttachment  = 'Ошибка удаления вложения для OBJECT_NAME="%s", OBJECT_ID="%d", ID="%d"!';
  SErrFileAttachment    = 'Ошибка сохранение вложения ID="%d", FILENAME="%s" во внешнем файле "%s"!';

const
  sqlOpenAttachments   = 'select * from SS_ATTACHMENTS where OBJECT_NAME=''%s'' and OBJECT_ID=%d';

procedure ShowAttachments(const ObjName, RecName: string; const ObjId, ViewTag, EditTag: Integer);
begin
  with TFormAttach.Create(Application) do
  begin
    try
      ObjectName := ObjName;
      RecordName := RecName;
      ObjectId := ObjId;
      Tag := ViewTag;
      SS_ATTACHMENTS.Tag := EditTag;
      if LoadData then
      begin
        AddToDbLog(ViewTag, Format(SLogOpenAttachments, [ObjName, ObjId]));
        ShowModal;
        AddToDbLog(ViewTag, Format(SLogCloseAttachments, [ObjName, ObjId]));
      end;
    finally
      Free;
    end;
  end;
end;

function DeleteAttachments(const ObjName: string; const ObjId, ViewTag, EditTag: Integer): Boolean;
begin
  with TFormAttach.Create(Application.MainForm) do
  begin
    try
      ObjectName := ObjName;
      RecordName := EmptyStr;
      ObjectId := ObjId;
      Tag := ViewTag;
      SS_ATTACHMENTS.Tag := EditTag;
      Result := OpenAttachments and FreeAttachments;
    finally
      Free;
    end;
  end;
end;

{ == Свойства набора данных ==================================================== }
function TFormAttach.GetDetailEditTag: Integer;
begin
  Result := SS_ATTACHMENTS.Tag;
end;

function TFormAttach.GetDetailName: string;
begin
  Result := SS_ATTACHMENTS.Name;
end;

procedure TFormAttach.LogOnOpen;
begin
  // Отключаем стандартные процедуры
end;

procedure TFormAttach.LogOnClose;
begin
  // Отключаем стандартные процедуры
end;

{ == Загружаем список ========================================================== }
function TFormAttach.OpenAttachments: Boolean;
begin
  SS_ATTACHMENTS.SQL.Text := Format(sqlOpenAttachments, [ObjectName, ObjectId]);
  Result := BaseData.OpenDataSetMsg(SS_ATTACHMENTS);
end;

function TFormAttach.CreateAttachmentItem: TListItem;
const
  SFileSize  = '%8.2f Kb';
var
  Id: TId;
  Hndl: HIcon;
  Icon: TIcon;
begin
  Result := ListView.Items.Add;
  with Result do
  begin
    New(Id);
    Data := Id;
    ImageIndex := 0;
    Id^ := SS_ATTACHMENTSID.AsInteger;
    Caption := SS_ATTACHMENTSFILENAME.AsString;
    Subitems.Add(Format(SFileSize, [SS_ATTACHMENTSFILESIZE.AsFloat / 1024]));
    Subitems.Add(SS_ATTACHMENTSFILETIME.DisplayText);
    Icon := TIcon.Create;
    try
      Hndl := GetAssociatedIconHandle(SS_ATTACHMENTSFILENAME.AsString);
      if Hndl > 0 then
      begin
        Icon.Handle := Hndl;
        ImageIndex := ImageList.AddIcon(Icon);
        DestroyIcon(Hndl);
      end;
    finally
      Icon.Free;
    end;
  end;
end;

function TFormAttach.LocateAttachmentItem(const Item: TListItem): Boolean;
begin
  SS_ATTACHMENTS.DisableControls;
  try
    Result := SS_ATTACHMENTS.Locate(fnID, GetItemId(Item), []);
    if not Result then
      raise Exception.CreateFmt(SErrDSIdNotFound, [GetItemId(Item), GetDetailName]);
  finally
    SS_ATTACHMENTS.EnableControls;
  end;
end;

function TFormAttach.LoadDataForm: Boolean;
begin
  Result := OpenAttachments;
  if Result then
  begin
    SS_ATTACHMENTS.DisableControls;
    try
      SS_ATTACHMENTS.First;
      ListView.Items.Clear;
      while not SS_ATTACHMENTS.Eof do
      begin
        CreateAttachmentItem;
        SS_ATTACHMENTS.Next;
      end;
      ListView.Sort;
    finally
      SS_ATTACHMENTS.EnableControls;
    end;
  end;
end;

procedure TFormAttach.ShowItemsCount;
begin
  if RecordName <> EmptyStr then
  begin
    if SS_ATTACHMENTS.Active
    then StatusBar.SimpleText := Format(SRecTitleName, [RecordName, SS_ATTACHMENTS.RecordCount])
    else StatusBar.SimpleText := Format(SRecTitleName, [RecordName, 0]);
  end
  else begin
    if SS_ATTACHMENTS.Active
    then StatusBar.SimpleText := Format(SRecTitleCnt, [SS_ATTACHMENTS.RecordCount])
    else StatusBar.SimpleText := Format(SRecTitleCnt, [0]);
  end;
end;

{ == Добавить файл ============================================================= }
function TFormAttach.AddAttachments(const FileName: string): Boolean;
var
  NewId: Integer;
  FileSize: Double;
  BlobStream: TMemoryStream;
begin
  Result := False;
  StartWait;
  ShowInStatusBar(SMsgSaveDataWait);
  try
    FileSize := GetFileSize(FileName) / 1024;
    if FileSize <= BaseData.AttachSizeMax then
    begin
      if (FileSize <= BaseData.AttachSizeWrn)
      or (QueryBoxStdNY(Format(SMsgAttachFileSizeWrn, [FileName, FileSize])) = ID_YES) then
      begin
        try
          NewId := BaseData.GetNewId(SS_ATTACHMENTS.Name);
          try
            Application.ProcessMessages;
            SS_ATTACHMENTS.Append;
            try
              SS_ATTACHMENTSID.AsInteger := NewId;
              SS_ATTACHMENTSOBJECT_NAME.AsString := ObjectName;
              SS_ATTACHMENTSOBJECT_ID.AsInteger := ObjectId;
              SS_ATTACHMENTSFILENAME.AsString := ExtractFileName(FileName);
              SS_ATTACHMENTSFILESIZE.AsInteger := GetFileSize(FileName);
              SS_ATTACHMENTSFILETIME.AsDateTime := FileDateTime(FileName);
              Application.ProcessMessages;
              BlobStream := TMemoryStream.Create;
              try
                ShowInStatusBar(SMsgLoadDataFile);
                BlobStream.LoadFromFile(FileName);
                ShowInStatusBar(SMsgSaveDataServer);
                BlobStream.Position := 0;
                Application.ProcessMessages;
                SS_ATTACHMENTSFILEDATA.LoadFromStream(BlobStream);
              finally
                BlobStream.Free;
              end;
              SS_ATTACHMENTS.Post;
              ListView.Selected := CreateAttachmentItem;
              ListView.Sort;
              Result := True;
              AddToDbLog(SS_ATTACHMENTS.Tag, Format(SLogSaveAttachment, [ObjectName,
                ObjectId, SS_ATTACHMENTSID.AsInteger, SS_ATTACHMENTSFILENAME.AsString,
                SS_ATTACHMENTSFILESIZE.AsInteger, SS_ATTACHMENTSFILETIME.AsString]));
            finally
              if SS_ATTACHMENTS.State in [dsEdit, dsInsert] then
                SS_ATTACHMENTS.Cancel;
            end;
          finally
            BaseData.FreeId(SS_ATTACHMENTS.Name, NewId);
          end;
        except
          on E: Exception do
          begin
            HandleExcept(E, SS_ATTACHMENTS, Format(SErrSaveAttachment, [ObjectName, ObjectId]));
            try
              SS_ATTACHMENTS.Requery([]);
            except
              LoadData;
            end;
          end;
        end;
      end
      else Result := True;
    end
    else begin
      Result := True;
      ErrorBox(Format(SMsgAttachFileSizeMax, [FileName, FileSize]));
    end;
  finally
    ShowItemsCount;
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

procedure TFormAttach.AddFileUpdate(Sender: TObject);
begin
  AddFile.Enabled := IsNotWait and DataSetIsOpen(SS_ATTACHMENTS)
    and (SS_ATTACHMENTS.State = dsBrowse);
end;

procedure TFormAttach.AddFileExecute(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialog.Execute then
  begin
    for i := 0 to OpenDialog.Files.Count - 1 do
      if not AddAttachments(OpenDialog.Files[i])
      then Break;
  end;
end;

{ == Удалить файл ============================================================== }
function TFormAttach.DeleteAttachment: Boolean;
var
  LogMsg: string;
begin
  Result := False;
  try
    LogMsg := Format(SLogDeleteAttachment, [ObjectName, ObjectId,
      SS_ATTACHMENTSID.AsInteger, SS_ATTACHMENTSFILENAME.AsString,
      SS_ATTACHMENTSFILESIZE.AsInteger, SS_ATTACHMENTSFILETIME.AsString]);
    SS_ATTACHMENTS.Delete;
    Result := True;
    AddToDbLog(SS_ATTACHMENTS.Tag, LogMsg);
  except
    on E: Exception do
    begin
      HandleExcept(E, SS_ATTACHMENTS, Format(SErrDeleteAttachment,
        [ObjectName, ObjectId, SS_ATTACHMENTSID.AsInteger]));
      try
        SS_ATTACHMENTS.Requery([]);
      except
        LoadData;
      end;
    end;
  end;
end;

function TFormAttach.FreeAttachments: Boolean;
begin
  Result := SS_ATTACHMENTS.Active;
  while Result and not SS_ATTACHMENTS.IsEmpty do
    Result := DeleteAttachment;
end;

procedure TFormAttach.DelFileUpdate(Sender: TObject);
begin
  DelFile.Enabled := IsNotWait and DataSetIsNotEmpty(SS_ATTACHMENTS)
    and (SS_ATTACHMENTS.State = dsBrowse) and (ListView.Selected <> nil);
end;

procedure TFormAttach.DelFileExecute(Sender: TObject);
var
  QryText: string;
  SelItem: TListItem;
begin
  if ListView.MultiSelect
  then QryText := Format(SQryDeleteAttachments, [ListView.SelCount])
  else QryText := Format(SQryDeleteAttachment, [ListView.Selected.Caption]);
  if DeleteQueryText(QryText) then
  begin
    StartWait;
    ShowInStatusBar(SMsgDeleteDataWait);
    try
      try
        SelItem := ListView.Selected;
        while SelItem <> nil do
        begin
          if not (LocateAttachmentItem(SelItem) and DeleteAttachment)
          then SelItem.Selected := False;
          SelItem := ListView.GetNextItem(SelItem, sdAll, [isSelected]);
        end;
        ListView.DeleteSelected;
      except
        on E: Exception do
          HandleExcept(E, Self, Format(SErrOperAttachment, [ObjectName, ObjectId]));
      end;
    finally
      ShowItemsCount;
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  end;
end;

{ == Сохранение в файл ========================================================= }
function TFormAttach.SaveAttachments(const AsFile: string): Boolean;
var
  BlobStream: TMemoryStream;
begin
  Result := False;
  StartWait;
  ShowInStatusBar(SMsgSaveDataWait);
  try
    try
      Application.ProcessMessages;
      BlobStream := TMemoryStream.Create;
      try
        ShowInStatusBar(SMsgLoadDataServer);
        SS_ATTACHMENTSFILEDATA.SaveToStream(BlobStream);
        ShowInStatusBar(SMsgSaveDataFile);
        BlobStream.Position := 0;
        Application.ProcessMessages;
        BlobStream.SaveToFile(AsFile);
        Application.ProcessMessages;
        if SS_ATTACHMENTSFILETIME.AsFloat > 0
        then Result := SetFileDateTime(AsFile, SS_ATTACHMENTSFILETIME.AsDateTime) = 0
        else Result := True;
        Application.ProcessMessages;
      finally
        BlobStream.Free;
      end;
    except
      on E: Exception do
        HandleExcept(E, SS_ATTACHMENTS, Format(SErrFileAttachment,
          [SS_ATTACHMENTSID.AsInteger, SS_ATTACHMENTSFILENAME.AsString, AsFile]));
    end;
  finally
    if Result then ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Открыть файл ============================================================== }
procedure TFormAttach.OpenFileUpdate(Sender: TObject);
begin
  OpenFile.Enabled := IsNotWait and DataSetIsNotEmpty(SS_ATTACHMENTS)
    and (SS_ATTACHMENTS.State = dsBrowse) and (ListView.Selected <> nil);
end;

procedure TFormAttach.OpenFileExecute(Sender: TObject);
var
  FileName: string;
begin
  try
    if LocateAttachmentItem(ListView.Selected)
    and ForceDirectories(BaseData.TempDir) then
    begin
      FileName := IncludeTrailingBackslash(BaseData.TempDir) +
        Trim(SS_ATTACHMENTSFILENAME.AsString);
      if SaveAttachments(FileName) then
      begin
        StartWait;
        try
          try
            AddToDbLog(Tag, Format(SLogOpenFileAttach,
              [SS_ATTACHMENTSID.AsInteger, ObjectName, ObjectId,
               SS_ATTACHMENTSFILENAME.AsString, SS_ATTACHMENTSFILESIZE.AsInteger,
               SS_ATTACHMENTSFILETIME.AsString]));
          finally
            OpenFile32('', FileName, '', SW_NORMAL, True, nil);
          end;
        finally
          try
            if FileExists(FileName) then DeleteFile(FileName);
          finally
            StopWait;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, Format(SErrOperAttachment, [ObjectName, ObjectId]));
  end;
end;

procedure TFormAttach.ListViewDblClick(Sender: TObject);
begin
  if OpenFile.Enabled then OpenFileExecute(Sender);
end;

{ == Сохранить в файле ========================================================= }
procedure TFormAttach.SaveAsFileUpdate(Sender: TObject);
begin
  SaveAsFile.Enabled := IsNotWait and DataSetIsNotEmpty(SS_ATTACHMENTS)
    and (SS_ATTACHMENTS.State = dsBrowse) and (ListView.Selected <> nil);
end;

procedure TFormAttach.SaveAsFileExecute(Sender: TObject);
begin
  try
    if LocateAttachmentItem(ListView.Selected) then
    begin
      SaveDialog.FileName := SS_ATTACHMENTSFILENAME.AsString;
      if SaveDialog.Execute then
      begin
        if SaveAttachments(SaveDialog.FileName)
        then AddToDbLog(Tag, Format(SLogSaveFileAttach,
          [SS_ATTACHMENTSID.AsInteger, ObjectName, ObjectId,
           SS_ATTACHMENTSFILENAME.AsString, SS_ATTACHMENTSFILESIZE.AsInteger,
           SS_ATTACHMENTSFILETIME.AsString, SaveDialog.FileName]));
      end;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, Format(SErrOperAttachment, [ObjectName, ObjectId]));
  end;
end;

end.
