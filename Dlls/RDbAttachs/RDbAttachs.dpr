library RDbAttachs;

uses
  SysUtils,
  Classes,
  Windows,
  Forms,
  AdoDb,
  RMsgRu in '..\..\RMsgRu.pas',
  RVclUtils in '..\..\RVclUtils.pas',
  RSysUtils in '..\..\RSysUtils.pas',
  RDialogs in '..\..\RDialogs.pas',
  RAppStyles in '..\..\RAppStyles.pas',
  RUserRights in '..\..\RUserRights.pas',
  RExHandlers in '..\..\RExHandlers.pas',
  RDbConst in '..\..\RDbConst.pas',
  RDbUtils in '..\..\RDbUtils.pas',
  RDbGetId in '..\..\RDbGetId.pas',
  RDbSettings in '..\..\RDbSettings.pas',
  RDbLog in '..\..\RDbLog.pas',
  RExHandlersExDlg in '..\..\RExHandlersExDlg.pas' {ExtErrorBox},
  RExHandlersDbLog in '..\..\RExHandlersDbLog.pas',
  RRssConst in '..\..\RRssConst.pas',
  RRssAttachs in '..\..\RRssAttachs.pas',
  TmplBase in '..\..\Templates\TmplBase.pas' {BaseTemplate},
  TmplStorage in '..\..\Templates\TmplStorage.pas' {StorageTemplate},
  TmplListSimple in '..\..\Templates\TmplListSimple.pas' {SimpleListTemplate},
  AttForm in 'AttForm.pas' {FormAttachs};

{$R *.res}

resourcestring
  EDllError        = 'Ошибка в библиотеке RDbAttachs.Dll!'#13#13'%s'#13'Класс: %s';

var
  DllApp: THandle;
  Db: TAdoconnection;

{ == Инициализация библиотеки ================================================== }
function InitDllSmp(AppHandle: THandle; const AppStyle: PApplicationStyle;
  const ConnectionStr: PChar): Boolean;
begin
  Result := False;
  try
    // Подменяем хэндл приложения для правильного управления окнами
    DllApp := Application.Handle;
    Application.Handle := AppHandle;
    // Устанавилваем стили окон
    ApplicationStyle := AppStyle;
    // Создаем обработчик ошибок
    AppExceptionsHandler := TExceptHandler.Create;
    Application.OnException := AppExceptionsHandler.AppExceptHandler;
    AppExceptionsHandler.ChannelCreate(TExDlgExceptChannel, 200, True);
    // Устанавливаем новое соединение с базой данных
    Db := TAdoConnection.Create(Application);
    Db.ConnectionString := string(ConnectionStr);
    Db.LoginPrompt := False;
    Db.KeepConnection := True;
    Db.Tag := -1;
    Db.Open;
    Result := Db.Connected;
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

function InitDllRss(AppHandle: THandle; const AppStyle: PApplicationStyle;
  const ConnectionStr: PChar; const ArmTag: Integer; const User: TUserRights): Boolean;
begin
  Result := False;
  try
    // Подменяем хэндл приложения для правильного управления окнами
    DllApp := Application.Handle;
    Application.Handle := AppHandle;
    // Устанавилваем стили окон
    ApplicationStyle := AppStyle;
    // Создаем обработчик ошибок
    AppExceptionsHandler := TExceptHandler.Create;
    Application.OnException := AppExceptionsHandler.AppExceptHandler;
    AppExceptionsHandler.ChannelCreate(TExDlgExceptChannel, 200, True);
    AppExceptionsHandler.ChannelCreate(TDbLogExceptChannel, 128, True);
    // Устанавливаем новое соединение с базой данных
    Db := TAdoConnection.Create(Application);
    Db.ConnectionString := string(ConnectionStr);
    Db.LoginPrompt := False;
    Db.KeepConnection := True;
    Db.Tag := ArmTag;
    Db.Open;
    Result := Db.Connected;
    // Если соединение установлено...
    if Result then
    begin
      // Инициализируем системный протокол
      try
        InitDbLog(Db, EmptyStr, Db.Tag, User, False);
      except
        on E: Exception do
          HandleExcept(E, nil, SErrInitDbLog);
      end;
    end;
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

{ == Завершение библиотеки ===================================================== }
procedure DoneDllProc(Reason: Integer);
begin
  try
    if Reason = DLL_PROCESS_DETACH then
    begin
      try
        // Если есть соединение с базой данных, закрываем его
        if Assigned(Db) then
        begin
          // Закрываем DbLog
          if Db.Tag > -1 then
            CloseDbLog;
          // Закрываем и уничтожаем соединение
          if Db.Connected then Db.Close;
          Db.Free;
        end;
        Db := nil;
      finally
        // Возвращаем истинный хендл приложения
        Application.OnException := nil;
        Application.Handle := DllApp;
        DllApp := 0;
      end;
    end;
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

procedure ShowAttachments(const ObjectName, RecordName: PChar; const ObjectId, ViewTag, EditTag: Integer; const EditEnabled: Boolean);
var
  Attachs: TRssDbAttachments;
  sObjectName, sRecordName: string;
begin
  try
    sObjectName := string(ObjectName);
    sRecordName := string(RecordName);
    Attachs := TRssDbAttachments.Create(Db, sObjectName, sRecordName,
      ObjectId, ViewTag, EditTag, EditEnabled);
    try
      if Attachs.Open(True) then
      begin
        try
          ShowAttachsList(Attachs);
        finally
          Attachs.Close(True);
        end;
      end;
    finally
      Attachs.Free;
    end;
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

function SaveAttachments(const ObjectName, DirName: PChar; const ObjectId, ViewTag, EditTag: Integer): Boolean;
var
  Attachs: TRssDbAttachments;
  sObjectName, sDirName: string;
begin
  Result := False;
  try
    sObjectName := string(ObjectName);
    sDirName := string(DirName);
    Attachs := TRssDbAttachments.Create(Db, sObjectName, EmptyStr, ObjectId, ViewTag, EditTag, True);
    try
      Result := Attachs.SaveAttachments(sDirName);
    finally
      Attachs.Free;
    end;
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

function DeleteAttachments(const ObjectName: PChar; const ObjectId, ViewTag, EditTag: Integer): Boolean;
var
  Attachs: TRssDbAttachments;
  sObjectName: string;
begin
  Result := False;
  try
    sObjectName := string(ObjectName);
    Attachs := TRssDbAttachments.Create(Db, sObjectName, EmptyStr, ObjectId, ViewTag, EditTag, True);
    try
      Result := Attachs.FreeAttachments;
    finally
      Attachs.Free;
    end;
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

exports
  InitDllSmp name 'InitDllSmp',
  InitDllRss name 'InitDllRss',
  ShowAttachments name 'ShowAttachments',
  SaveAttachments name 'SaveAttachments',
  DeleteAttachments name 'DeleteAttachments';

begin
  DllProc := @DoneDllProc;
end.
