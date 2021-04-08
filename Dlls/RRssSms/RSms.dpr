library RSms;

uses
  SysUtils,
  Classes,
  Controls,
  Windows,
  Forms,
  AdoDb,
  DateUtils,
  RMsgRu in '..\..\RMsgRu.pas',
  RVclUtils in '..\..\RVclUtils.pas',
  RSysUtils in '..\..\RSysUtils.pas',
  RDialogs in '..\..\RDialogs.pas',
  RAppStyles in '..\..\RAppStyles.pas',
  RUserRights in '..\..\RUserRights.pas',
  RAdoUtils in '..\..\RAdoUtils.pas',
  RDbConst in '..\..\RDbConst.pas',
  RDbSettings in '..\..\RDbSettings.pas',
  RDbUtils in '..\..\RDbUtils.pas',
  RDbLog in '..\..\RDbLog.pas',
  RDbGetId in '..\..\RDbGetId.pas',
  RExHandlers in '..\..\RExHandlers.pas',
  RExHandlersExDlg in '..\..\RExHandlersExDlg.pas' {ExtErrorBox},
  RExHandlersDbLog in '..\..\RExHandlersDbLog.pas',
  RxStrUtils in '..\..\RXLib\RxStrUtils.pas',
  RRssConst in '..\..\RRssConst.pas',
  RSmsVars in 'RSmsVars.pas',
  TmplBase in '..\..\Templates\TmplBase.pas' {BaseTemplate},
  TmplDialog in '..\..\Templates\TmplDialog.pas' {DialogTemplate},
  TmplDbDialog in '..\..\Templates\TmplDbDialog.pas' {DbDialogTemplate},
  TmplStorage in '..\..\Templates\TmplStorage.pas' {StorageTemplate},
  TmplDbSimple in '..\..\Templates\TmplDbSimple.pas' {SimpleDbTemplate},
  ReadForm in 'ReadForm.pas' {FormRead},
  SendForm in 'SendForm.pas' {FormSend},
  UsersList in 'UsersList.pas' {FormUsers},
  WpsList in 'WpsList.pas' {FormWps},
  SmsList in 'SmsList.pas' {FormSmsList};

{$R *.res}

resourcestring
  EDllError        = 'Ошибка в библиотеке RSms.Dll!'#13#13'%s'#13'Класс: %s';

var
  DllApp: THandle;
  Db: TAdoConnection;

{ == Инициализация библиотеки ================================================== }
function InitDll(AppHandle: THandle; const AppStyle: PApplicationStyle;
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
    // Сохраняем данные во внутрениих переменных
    UsrDll := User;
    ArmDll := ArmTag;
    // Устанавливаем новое соединение с базой данных
    Dp := ExtractDbParameters(string(ConnectionStr));
    Db := TAdoConnection.Create(Application);
    Db.ConnectionString := ExtractDbConnectStr(string(ConnectionStr));
    Db.LoginPrompt := False;
    Db.KeepConnection := True;
    Db.Tag := ArmTag;
    Db.Open;
    Result := Db.Connected;
    // Если соединение установлено...
    if Result then
    begin
      try
        DbLog_Init(Db, ExtractDbParameters(string(ConnectionStr)).DateFormat, Db.Tag, UsrDll, False);
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

{ == Чтение сообщений ========================================================== }
procedure ReadMessages;
var
  SqlText, Address, AddrPart: string;
  Qry: TAdoQuery;
  i: Integer;
begin
  StartWait;
  try
    // Загрузка списка непрочитанных сообщений
    try
      SqlText := Format(sqlLoadSmsList, [UsrDll.UserId]);
      Qry := nil;
      try
        Qry := OpenDynamicQuery(Db, SqlText);
        if DataSetIsNotEmpty(Qry) then
        begin
          Qry.First;
          while not Qry.Eof do
          begin
            // Проверяем адреса
            Address := Trim(Qry.FieldByName(fnADDRESS).AsString);
            if Address <> EmptyStr then
              for i := 1 to WordCount(Address, chAddrDividers) do
              begin
                AddrPart := AnsiUpperCase(Trim(ExtractWord(i, Address, chAddrDividers)));
                if (AddrPart = AnsiUpperCase(UsrDll.UserName))
                or (AddrPart = Format(fmtWpPrefix, [IntToStr(ArmDll)]))
                or (AddrPart = fmtAllPrefix) then
                begin
                  PauseWait;
                  try
                    ReadSms(Db, Qry.FieldByName(fnID).AsInteger, UsrDll.UserId);
                  finally
                    ContiniueWait;
                  end;
                  Break;
                end;
              end;
            Qry.Next;
          end;
        end;
      finally
        FreeDynamicQuery(Qry);
      end;
    except
      on E: Exception do
        HandleSqlExcept(E, nil, SqlText, EErrLoadMessages);
    end;
  finally
    StopWait;
  end;
end;

{ == Создание сообщения ======================================================== }
procedure SendMessages;
begin
  SendSms(Db, UsrDll.UserId);
end;

{ == Просмотр списка сообщений ================================================= }
procedure ViewMessages;
var
  Data: RSmsListParams;
begin
  Data.ArmId := ArmDll;
  Data.UserId := UsrDll.UserId;
  ShowSimpleDbForm(TFormSmsList, Db, @Data, Dp, EmptyStr, tagReadSms, False);
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
          DbLog_Close;
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

exports
  InitDll name 'InitDll',
  ReadMessages name 'ReadMessages',
  SendMessages name 'SendMessages',
  ViewMessages name 'ViewMessages';

begin
  DllProc := @DoneDllProc;
end.
