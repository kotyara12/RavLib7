unit DM_TmplBaseRss;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, RUserRights, DB, ADODB, RDbConst, ActnList, ComCtrls, DM_TmplBase, ImgList,
  ExtCtrls;

type
  TBaseDataRssTemplate = class(TBaseDataTemplate)
    ChangeUserPassword: TAction;
    su_groups: TADOTable;
    su_users: TADOTable;
    su_usersid: TIntegerField;
    su_usersid_groups: TIntegerField;
    su_usersname: TStringField;
    su_usersfullname: TStringField;
    su_usersnotes: TStringField;
    su_usersdeleted: TBooleanField;
    su_usersblocked: TBooleanField;
    su_groupsid: TIntegerField;
    su_groupsowner_id: TIntegerField;
    su_groupsname: TStringField;
    su_groupsnotes: TStringField;
    sr_workplases: TADOTable;
    sr_workplasesid: TIntegerField;
    sr_workplasesname_s: TStringField;
    sr_workplasesname: TStringField;
    ReadMail: TAction;
    ViewMail: TAction;
    SendMail: TAction;
    AutoStartParams: TAction;
    procedure ChangeUserPasswordUpdate(Sender: TObject);
    procedure ChangeUserPasswordExecute(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure ReadMailUpdate(Sender: TObject);
    procedure ReadMailExecute(Sender: TObject);
    procedure SendMailUpdate(Sender: TObject);
    procedure SendMailExecute(Sender: TObject);
    procedure ViewMailUpdate(Sender: TObject);
    procedure ViewMailExecute(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure AutoStartParamsUpdate(Sender: TObject);
    procedure AutoStartParamsExecute(Sender: TObject);
  private
    function  UserRegistration: Boolean;
    function  InitUserRights: Boolean;
    function  LoadBaseTables: Boolean;
    function  InitMessages: Boolean;
    {$IFDEF AUTOSTART}
    function  AutoRegistration: Boolean;
    {$ENDIF}
  protected
    procedure AfterOpenConnection; override;
    procedure BeforeCloseConnection; override;
    function  InitSysLog: Boolean; override;
    procedure DoneSysLog; override;
    function  ReadDbVersion: Integer; override;
    procedure SaveDbVersion(const iVersion: Integer); override;
    procedure FillUserRights(const OperList: array of Integer); virtual;
  public
    User: TUserRights;
    SmsEnabled: Boolean;
    orReadSms: Boolean;
    orSendSms: Boolean;
    {$IFDEF AUTOSTART}
    fAutoStart_Enabled: Boolean;
    fAutoStart_Login: string;
    fAutoStart_Minimize: Boolean;
    function  LoadUserData(const Login: string): TUserRights;
    {$ENDIF}
    function  GetAppTag: Integer;
    procedure AddToSysLog(const OperTag: Integer; OperMsg: string); override;
    function  InitApplication: Boolean; override;
    function  GetNewId(const DsName: string; const KeyName: string = fnID): Integer; override;
    procedure FreeId(const DsName: string; const Id: Integer); override;
    procedure FixTimeChanges(DataSet: TDataSet;
      const fnTimeCreate: string; const fnUserCreate: string;
      const fnTimeChange: string; const fnUserChange: string);
    procedure FixTimeChangesDef(DataSet: TDataSet);
    procedure SPCheckSpName(SP: TAdoStoredProc);
    procedure SPSetSysParameters(SP: TAdoStoredProc);
    procedure SPAddSysParameters(SP: TAdoStoredProc);
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF AUTOSTART} AutoRunForm, {$ENDIF}
  IniFiles, RVclUtils, RDialogs, RMsgRu, RSysUtils, StrUtils, RxStrUtils,
  RRssConst, RRssBase, RExHandlers, RExHandlersDbLog, RSmsDll,
  RDbUtils, RDbGetId, RDbSettings, RDbLog, PrjVariables;

{ == Инициализация переменных ================================================== }
procedure TBaseDataRssTemplate.DataModuleCreate(Sender: TObject);
begin
  SmsEnabled := False;
  inherited;
end;

procedure TBaseDataRssTemplate.DataModuleDestroy(Sender: TObject);
begin
  inherited;
end;

{ == Соединение с базой данных ================================================= }
procedure TBaseDataRssTemplate.AfterOpenConnection;
begin
  inherited;
end;

{ == Инициализация RSS приложения ============================================== }
function TBaseDataRssTemplate.InitApplication: Boolean;
begin
  // InitSysLog and CheckDbVersion  2014-09-20 - moved CheckDbVersion in function TBaseDataTemplate.ConnectToDatabase...
  Result := ({$IFDEF AUTOSTART} AutoRegistration or {$ENDIF} UserRegistration)
    and InitUserRights
    and LoadBaseTables;
  Application.ProcessMessages;
  if Result then
  begin
    SmsEnabled := InitMessages;
    Application.ProcessMessages;
    if SmsEnabled and OrReadSms then ReadMailExecute(nil);
  end
  else SmsEnabled := False;
end;

{ == Инициализация системного протокола ======================================== }
function TBaseDataRssTemplate.GetAppTag: Integer;
begin
  if Assigned(Application.MainForm)
  then Result := Application.MainForm.Tag
  else Result := intDisable;
end;

function TBaseDataRssTemplate.InitSysLog: Boolean;
begin
  ShowInStatusBar(SMsgInitDbLog);
  Result := False;
  try
    InitUserData(User);
    DbLog_Init(acDb, DbParameters.DateFormat, GetAppTag, User, True);
    AppExceptionsHandler.ChannelCreate(TDbLogExceptChannel, 128, True);
    Result := True;
    Application.ProcessMessages;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrInitDbLog);
  end;
end;

procedure TBaseDataRssTemplate.DoneSysLog;
begin
  DbLog_Close;
end;

procedure TBaseDataRssTemplate.AddToSysLog(const OperTag: Integer; OperMsg: string);
begin
  AddToDbLog(OperTag, OperMsg);
end;

{ == Проверка версии базы данных =============================================== }
function TBaseDataRssTemplate.ReadDbVersion: Integer;
begin
  Result := ReadDbSysInteger(acDb, sidDbVersion, 0);
end;

procedure TBaseDataRssTemplate.SaveDbVersion(const iVersion: Integer);
begin
  SaveDbSysInteger(acDb, sidDbVersion, iVersion);
end;

(* 2014-09-20: moved CheckDbVersion in function TBaseDataTemplate.ConnectToDatabase...
function TBaseDataRssTemplate.CheckDbVersion: Boolean;
var
  BaseDbVersion: Integer;
begin
  ShowInStatusBar(SMsgCheckDbVersion);
  Result := False;
  try
    BaseDbVersion
    Result := (BaseDbVersion = DbVersion);
    if not Result then
    begin
      AddToDbLog(tagError, Format(SLogWarningDbVersion, [Application.Title, BaseDbVersion, DbVersion]));
      Result := WarningBoxNY(Format(SMsgWarningDbVersion, [BaseDbVersion, DbVersion])) = ID_YES;
    end;
    Application.ProcessMessages;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrCheckDbVersion);
  end;
end;
*)

{ == Автологин ================================================================= }
{$IFDEF AUTOSTART}
function TBaseDataRssTemplate.LoadUserData(const Login: string): TUserRights;
var
  qryUser: TAdoQuery;
begin
  Result.UserId := intDisable;
  Result.Registration := False;
  Result.PwdChange := False;
  Result.UserName := EmptyStr;
  Result.FullName := EmptyStr;
  qryUser := nil;
  try
    qryUser := OpenDynamicQuery(acDb, Format(sqlLoadUserData, [Login]));
    try
      if DataSetIsNotEmpty(qryUser) then
        with qryUser do
        begin
          Result.UserId := FieldByName(fnID).AsInteger;
          Result.Registration := not (FieldByName(fnDELETED).AsBoolean
                                   or FieldByName(fnBLOCKED).AsBoolean);
          Result.UserName := FieldByName(fnNAME).AsString;
          Result.FullName := FieldByName(fnFULLNAME).AsString;
        end;
    finally
      FreeDynamicQuery(qryUser);
    end;
  except
    on E: Exception do
      HandleExcept(E, qryUser, SErrUserRegistration);
  end;
end;

function TBaseDataRssTemplate.AutoRegistration: Boolean;
begin
  Result := False;
  ShowInStatusBar(SMsgAutoRegistration);
  try
    (*
    Ini := TIniFile.Create(GetApplicationIniFile);
    try
      fAutoStart_Login := Trim(Ini.ReadString(iniAS_Section, iniAS_Login, EmptyStr));
      fAutoStart_Enabled := (AS_Login <> EmptyStr) and Ini.ReadBool(iniAS_Section, iniAS_Enabled, False);
      fAutoStart_Minimize := Ini.ReadBool(iniAS_Section, iniAS_Minimize, False);
    finally
      Ini.Free;
    end;
    *)

    fAutoStart_Login := ReadDbSysString(acDb, sidAutoStart_Login, EmptyStr);
    fAutoStart_Enabled := ReadDbSysBoolean(acDb, sidAutoStart_Enabled, False);
    fAutoStart_Minimize := ReadDbSysBoolean(acDb, sidAutoStart_Minimize, False);
    Application.ProcessMessages;

    if fAutoStart_Enabled then
    begin
      User := LoadUserData(fAutoStart_Login);
      Result := User.Registration;
      fAutoStart_Enabled := User.Registration;
      Application.ProcessMessages;
      DbLog_UpdateUser(User);
      Application.ProcessMessages;
      if User.Registration then
      begin
        ShowInStatusBarPanel(1, Format(SFmtUserName, [User.UserName, User.FullName]));
        AddToSysLog(tagRegistration, Format(SLogAutoRegistration, [User.UserName, Application.Title]));
      end;
      Application.ProcessMessages;
    end;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrUserRegistration);
  end;
end;
{$ENDIF}

{ == Настройка парамеров автозапуска =========================================== }
procedure TBaseDataRssTemplate.AutoStartParamsUpdate(Sender: TObject);
begin
  AutoStartParams.Enabled := IsNotWait and acDb.Connected and User.Registration;
end;

procedure TBaseDataRssTemplate.AutoStartParamsExecute(Sender: TObject);
{$IFDEF AUTOSTART}
begin
  with TFormAutoRun.Create(Self) do
  begin
    try
      AutoStartCheckBox.Checked := fAutoStart_Enabled;
      MinimizeCheckBox.Checked := fAutoStart_Minimize;
      LoginEdit.Text := fAutoStart_Login;
      if ShowModal = mrOk then
      begin
        fAutoStart_Enabled := AutoStartCheckBox.Checked;
        fAutoStart_Minimize := MinimizeCheckBox.Checked;
        fAutoStart_Login := LoginEdit.Text;

        (*
        Ini := TIniFile.Create(GetApplicationIniFile);
        try
          Ini.WriteString(iniAS_Section, inifAutoStart_Login, fAutoStart_Login);
          Ini.WriteBool(iniAS_Section, inifAutoStart_Enabled, fAutoStart_Enabled);
          Ini.WriteBool(iniAS_Section, inifAutoStart_Minimize, fAutoStart_Minimize);
        finally
          Ini.Free;
        end;
        *)

        SaveDbSysString(acDb, sidAutoStart_Login, fAutoStart_Login);
        SaveDbSysBoolean(acDb, sidAutoStart_Enabled, fAutoStart_Enabled);
        SaveDbSysBoolean(acDb, sidAutoStart_Minimize, fAutoStart_Minimize);
      end;
    finally
      Free;
    end;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

{ == Проверка прав доступа в систему =========================================== }
function TBaseDataRssTemplate.UserRegistration: Boolean;
begin
  Result := False;
  ShowInStatusBar(SMsgUserRegistration);
  try
    PauseWait;
    try
      Rss_LoginUser(acDb, Application.MainForm.Tag, User);
    finally
      ContiniueWait;
    end;
    Result := User.Registration;
    Application.ProcessMessages;
    DbLog_UpdateUser(User);
    Application.ProcessMessages;
    ShowInStatusBarPanel(1, Format(SFmtUserName, [User.UserName, User.FullName]));
    Application.ProcessMessages;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrUserRegistration);
  end;
end;

{ == Инициализация прав пользователя =========================================== }
procedure TBaseDataRssTemplate.FillUserRights(const OperList: array of Integer);
begin
  orReadSms := ChkRights(OperList, tagReadSms);
  orSendSms := ChkRights(OperList, tagSendSms);
  
  orEditReports := ChkRights(OperList, tagEditReports);
end;

function TBaseDataRssTemplate.InitUserRights: Boolean;
const
  sqlUserOperations = 'SELECT id_operations FROM vsr_operations_user WHERE id_users=%d';
var
  QryOperList: TADOQuery;
  i: Integer;
  OperList: array of Integer;
begin
  Result := False;
  ShowInStatusBar(SMsgInitUserRights);
  QryOperList := nil;
  try
    try
      QryOperList := OpenDynamicQuery(acDb, Format(sqlUserOperations, [User.UserId]));
      if DataSetIsOpen(QryOperList) then
      begin
        try
          i := 0;
          QryOperList.First;
          while not QryOperList.Eof do
          begin
            Application.ProcessMessages;
            SetLength(OperList, i + 1);
            OperList[i] := QryOperList.Fields[0].AsInteger;
            Inc(i);
            QryOperList.Next;
          end;
          FillUserRights(OperList);
          Result := True;
        finally
          SetLength(OperList, 0);
        end;
      end;
    Application.ProcessMessages;
    except
      on E: Exception do
        HandleExcept(E, QryOperList, SErrInitUserRights);
    end;
  finally
    FreeDynamicQuery(QryOperList);
  end;
  Application.ProcessMessages;
end;

{ == Смена пароля пользователем ================================================ }
procedure TBaseDataRssTemplate.ChangeUserPasswordUpdate(Sender: TObject);
begin
  ChangeUserPassword.Enabled := IsNotWait and User.Registration;
end;

procedure TBaseDataRssTemplate.ChangeUserPasswordExecute(Sender: TObject);
begin
  ShowInStatusBar(SMsgUserChangePassword);
  try
    try
      PauseWait;
      try
        Rss_ChangePassword(acDb, Application.MainForm.Tag, User);
      finally
        ContiniueWait;
      end;
    except
      on E: Exception do
        HandleExcept(E, Self, SErrUserChangePassword);
    end;
  finally
    ShowInStatusBar(EmptyStr);
  end;
end;

{ == Загрузка таблиц модуля ==================================================== }
function TBaseDataRssTemplate.LoadBaseTables: Boolean;
begin
  Result := False;
  try
    Result := OpenReference(su_groups, SMsgLoadUsers);
    Application.ProcessMessages;
    if Result then Result := OpenReference(su_users, SMsgLoadUsers);
    Application.ProcessMessages;
    if Result then Result := OpenReference(sr_workplases, SMsgLoadUsers);
    Application.ProcessMessages;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrLoadUsers);
  end;
end;

{ == Инициализация подсистемы сообщений ======================================== }
function TBaseDataRssTemplate.InitMessages: Boolean;
begin
  Result := False;
  ShowInStatusBar(SMsgInitMessages);
  try
    Result := CheckLoadSmsDll;
  except
    on E: Exception do
      HandleExcept(E, Self, SErrInitMessages);
  end;
end;

{ == Закрытие соединения с базой данных ======================================== }
procedure TBaseDataRssTemplate.BeforeCloseConnection;
begin
  try
    if User.Registration then AddToSysLog(tagRegistration,
      Format(SLogCloseProgram, [User.UserName, Application.Title]));
  finally
    inherited;
  end;
end;

{ == Генерация индектов таблицы с блокировкой ================================== }
function TBaseDataRssTemplate.GetNewId(const DsName: string; const KeyName: string = fnID): Integer;
begin
  Result := GetBlockedID(acDb, DsName, KeyName);
  {$IFDEF DEMO}
  if Result > 10 then
  begin
    FreeId(DsName, Result);
    raise Exception.Create('Вы используете демонстрационную версию с ограниченной функциональностью!');
  end;
  {$ENDIF}
end;

procedure TBaseDataRssTemplate.FreeId(const DsName: string; const Id: Integer);
begin
  FreeBlockedID(acDb, DsName, Id);
end;

{ == Сохранение времени изменения записи ======================================= }
procedure TBaseDataRssTemplate.FixTimeChanges(DataSet: TDataSet;
  const fnTimeCreate, fnUserCreate, fnTimeChange, fnUserChange: string);
var
  flTimeCreate, flTimeChange, flUserCreate, flUserChange: TField;
begin
  if DataSet.State in [dsInsert, dsEdit] then
  begin
    flTimeCreate := DataSet.FindField(fnTimeCreate);
    flTimeChange := DataSet.FindField(fnTimeChange);
    flUserCreate := DataSet.FindField(fnUserCreate);
    flUserChange := DataSet.FindField(fnUserChange);

    if Assigned(flTimeCreate) and flTimeCreate.IsNull then
      flTimeCreate.AsDateTime := Now;
    if Assigned(flUserCreate) and flUserCreate.IsNull then
      flUserCreate.AsInteger := User.UserId;

    if DataSet.Modified then
    begin
      if Assigned(flTimeChange) then
        flTimeChange.AsDateTime := Now;
      if Assigned(flUserChange) then
        flUserChange.AsInteger := User.UserId;
    end;

    if Assigned(flTimeChange) and flTimeChange.IsNull then
      flTimeChange.AsDateTime := Now;
    if Assigned(flUserChange) and flUserChange.IsNull then
      flUserChange.AsInteger := User.UserId;
  end;
end;

procedure TBaseDataRssTemplate.FixTimeChangesDef(DataSet: TDataSet);
begin
  FixTimeChanges(DataSet, fnCREATED, fnID_CREATOR, fnCHANGED, fnID_CHANGER);
end;

{ == Установка "стандартных" параметров хранимых процедур ====================== }
procedure TBaseDataRssTemplate.SPCheckSpName(SP: TAdoStoredProc);
begin
  {$IFDEF MSSQL}
  if not AnsiEndsText(';1', SP.ProcedureName) then
    SP.ProcedureName := SP.ProcedureName + ';1';
  {$ENDIF}
  {$IFDEF MYSQL}
  if AnsiEndsText(';1', SP.ProcedureName) then
    SP.ProcedureName := Copy(SP.ProcedureName, 1, Length(SP.ProcedureName) - 2);
  {$ENDIF}
end;

procedure TBaseDataRssTemplate.SPSetSysParameters(SP: TAdoStoredProc);
begin
  with SP.Parameters do
  begin
    ParamByName(pnIdWorkplases).Value := GetAppTag;
    ParamByName(pnIdUsers).Value := User.UserId;
    ParamByName(pnHost).Value := GetComputerNetName;
    ParamByName(pnNetUser).Value := GetCurrentUserName;
  end;
end;

procedure TBaseDataRssTemplate.SPAddSysParameters(SP: TAdoStoredProc);
begin
  with SP.Parameters do
  begin
    CreateParameter(pnIdWorkplases, ftInteger, pdInput, 0, GetAppTag);
    CreateParameter(pnIdUsers, ftInteger, pdInput,  0, User.UserId);
    CreateParameter(pnHost, ftString, pdInput, 32, GetComputerNetName);
    CreateParameter(pnNetUser, ftString, pdInput, 32, GetCurrentUserName);
  end;
end;

{ == Прием и чтение сообщений ================================================== }
procedure TBaseDataRssTemplate.ReadMailUpdate(Sender: TObject);
begin
  ReadMail.Enabled := IsNotWait and SmsEnabled and orReadSms;
end;

procedure TBaseDataRssTemplate.ReadMailExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgReadMail);
  try
    try
      DllReadMessages(DbComplexStr, User);
    except
      on E: Exception do
        HandleExcept(E, Self, SErrReadMail);
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Создать и отправить сообщение ============================================= }
procedure TBaseDataRssTemplate.SendMailUpdate(Sender: TObject);
begin
  SendMail.Enabled := IsNotWait and SmsEnabled and OrSendSms;
end;

procedure TBaseDataRssTemplate.SendMailExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgSendMail);
  try
    try
      DllSendMessages(DbComplexStr, User);
    except
      on E: Exception do
        HandleExcept(E, Self, SErrSendMail);
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Просмотр списка принятых и отправленных сообщений ========================= }
procedure TBaseDataRssTemplate.ViewMailUpdate(Sender: TObject);
begin
  ViewMail.Enabled := IsNotWait and SmsEnabled and OrReadSms;
end;

procedure TBaseDataRssTemplate.ViewMailExecute(Sender: TObject);
begin
  StartWait;
  ShowInStatusBar(SMsgViewMail);
  try
    try
      DllViewMessages(DbComplexStr, User);
    except
      on E: Exception do
        HandleExcept(E, Self, SErrViewMail);
    end;
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

end.
