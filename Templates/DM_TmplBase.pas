unit DM_TmplBase;

{$IFDEF MSSQL}
  {$DEFINE SQL}
{$ENDIF}
{$IFDEF MYSQL}
  {$DEFINE SQL}
{$ENDIF}

interface

uses
  SysUtils, Classes, ImgList, Controls, ActnList, Forms, Windows, DB, ADODB,
  RAdoUtils, RDbConst, RDbFilter, RDbOrder, RavTreeView, ExtCtrls;

type
  TBaseDataTemplate = class(TDataModule)
    DbActionList: TActionList;
    acDb: TADOConnection;
    ImageList: TImageList;
    procedure DataModuleCreate(Sender: TObject);
    procedure PostError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    procedure DeleteError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
    procedure acDbAfterConnect(Sender: TObject);
    procedure acDbBeforeDisconnect(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure acDbLogin(Sender: TObject; Username, Password: String);
  private
    procedure SafeLoadSystemParameters;
    function  CheckDbVersion: Boolean;
    function  SafeReadDbVersion: Integer;
    function  SafeUpdateDatabase(const iVersion: Integer): Boolean;
    function  SafeSaveDbVersion(const iVersion: Integer): Boolean;
  protected
    fResqueEnabled: Boolean;
    procedure AfterOpenConnection; dynamic;
    procedure BeforeCloseConnection; dynamic;
    function  InitApplication: Boolean; virtual;
    procedure LoadSystemParameters; virtual;
    function  InitSysLog: Boolean; virtual;
    procedure DoneSysLog; virtual;
    function  ReadDbVersion: Integer; virtual;
    procedure SaveDbVersion(const iVersion: Integer); virtual;
    function  UpdateDatabaseSql(const SqlCmd: string; const LogFileName: string): Boolean; virtual;
    function  UpdateDatabase(const iVersion: Integer; const FileName: string): Boolean; virtual;
  public
    DbComplexStr: string;
    DbParameters: RAdoDbParameters;
    LookupRefreshLock: Boolean;
    AppInitialized: Boolean;
    orEditReports: Boolean;
    {$IFDEF DBWATCHDOG}
    WatchDog: TTimer;
    {$ENDIF}
    function  ConnectToDatabase: Boolean;
    procedure CloseConnection;
    {$IFDEF DBWATCHDOG}
    procedure WatchDogTimer(Sender: TObject);
    procedure ReconnectToDatabase(Connection: TADOConnection; const iTimeLimit: Integer);
    procedure CheckConnection(const iTimeLimit: Integer);
    {$ENDIF}
    function  ChangeConnectProperties: Boolean;
    procedure CreateResqueCopy;
    procedure RestoreResqueCopy;
    procedure AddToSysLog(const OperTag: Integer; OperMsg: string); virtual;
    function  OpenReference(DataSet: TDataSet; const Msg: string;
      const OperId: Integer = 0): Boolean;
    function  OpenDataSet(DataSet: TDataSet; const StorePosition: Boolean = True;
      const OperId: Integer = 0): Boolean;
    function  OpenDataSetWait(DataSet: TDataSet; const StorePosition: Boolean = True;
      const OperId: Integer = 0): Boolean;
    function  OpenDataSetMsg(DataSet: TDataSet; const StorePosition: Boolean = True;
      const OperId: Integer = 0): Boolean;
    function  OpenVariableQueryEx(Query: TAdoQuery; Filter: TRDbFilter; Order: TRDbOrder;
      const SelectSql, BaseWhereSql, GroupBySql: string; const OperId: Integer = 0): Boolean;
    function  OpenVariableQuery(Query: TAdoQuery; Filter: TRDbFilter; Order: TRDbOrder;
      const SelectSql, BaseWhereSql: string; const OperId: Integer = 0): Boolean;
    function OpenStaticQuery(Query: TAdoQuery;
      const SelectSql: string; const OperId: Integer = 0): Boolean;
    function GetNewId(const DsName: string; const KeyName: string = fnID): Integer; virtual;
    procedure FreeId(const DsName: string; const Id: Integer); virtual;
    property ResqueEnabled: Boolean read fResqueEnabled;
  end;

const
  DefSqlDateFormat         = 'dd.MM.yyyy';

  imFolder                 = 26;
  imFolderOpen             = 27;

implementation

uses
  StrUtils, DateUtils,
  {$IFDEF MDI} TmplMdiDb, {$ENDIF}
  {$IFDEF NRM} TmplNrmDb, {$ENDIF}
  {$IFDEF ATTACH} RDbAttachs, {$ENDIF}
  {$IFDEF SQL} RDbSettings, {$ENDIF}
  RVclUtils, RSysUtils, RMsgRu, RAdoCnctDll3, RExHandlers, RDialogs, RMessages, RProgress,
  RDbUtils, RDbOpenDS, RDbGetId, RFileLog, RWait, RxStrUtils, PrjVariables, Dialogs;

{$R *.dfm}

{ == Создание модуля данных ==================================================== }
procedure TBaseDataTemplate.DataModuleCreate(Sender: TObject);
begin
  LookupRefreshLock := False;
  orEditReports := True;
  DbParameters.DateFormat := Format('''%s''', [ShortDateFormat]);
  DbParameters.CaseEnabled := False;
  DbParameters.WatchDogTime := 1000;

  {$IFDEF MDI}
  if Assigned(Application.MainForm) and (Application.MainForm is TMdiMainDbTemplate) then
    TMdiMainDbTemplate(Application.MainForm).DbModule := Self;
  {$ENDIF}
  {$IFDEF NRM}
  if Assigned(Application.MainForm) and (Application.MainForm is TNrmMainDbTemplate) then
    TNrmMainDbTemplate(Application.MainForm).DbModule := Self;
  {$ENDIF}

  {$IFDEF SQL}
  {$ELSE}
  {$ENDIF}
end;

procedure TBaseDataTemplate.DataModuleDestroy(Sender: TObject);
begin
end;

{ == Соединение с базой данных ================================================= }
function TBaseDataTemplate.InitApplication: Boolean;
begin
  Result := True;
end;

procedure TBaseDataTemplate.AfterOpenConnection;
begin
  SafeLoadSystemParameters;
  {$IFDEF DEMO}
  WarningBox('Вы используете демонстрационную версию с ограниченной функциональностью!');
  {$ENDIF}
end;

function TBaseDataTemplate.ConnectToDatabase: Boolean;
var
  i: Integer;
  CfgFile: string;
begin
  Result := False;
  AppInitialized := False;
  try
    CfgFile := EmptyStr;
    for i := 1 to ParamCount do
      if FileExists(ExpandFileName(ParamStr(i))) then
      begin
        CfgFile := ExpandFileName(ParamStr(i));
        Break;
      end;
    if CfgFile = EmptyStr then
      // 2013-05-14: fixed bug: попытка считать cfg из "левого" каталога
      // CfgFile := ExpandFileName(SGlobalCfgFile);
      CfgFile := ExtractFilePath(Application.ExeName) + SGlobalCfgFile;
    // 2013-05-14: fixed bug: выдать ошибку при отсутствии файла конфигурации
    if not FileExists(CfgFile) then
      raise Exception.CreateFmt(SErrBadDbCfgFile, [CfgFile]);
    DbComplexStr := LoadAdoDbParameters(CfgFile,
      MixConnStrAndParams(acDb.ConnectionString, DbParameters), fResqueEnabled);
    if Trim(DbComplexStr) = EmptyStr then
      WarningBox(SMsgConfigureConnect)
    else begin
      acDb.ConnectionString := ExtractDbConnectStr(DbComplexStr);
      DbParameters := ExtractDbParameters(DbComplexStr);

      {$IFDEF DBWATCHDOG}
      WatchDog := TTimer.Create;
      WatchDog.OnTime := WatchDogTimer;
      WatchDog.Interval := DbParameters.WatchDogTime;
      // if IsFileDataSource(DbComplexStr)
      // then WatchDog.Tag := 0
      // else WatchDog.Tag := 1;
      {$ENDIF}

      acDb.LoginPrompt := IsLoginPrompt(DbComplexStr);
      acDb.Open;
      Result := acDb.Connected;
      if Result then
      begin
        LookupRefreshLock := False;
        // 2014-09-20: added CheckDbVersion 
        if InitSysLog and CheckDbVersion then
        begin
          // 2014-10-02: move UserRegistration from TmplMdiRss
          if InitApplication then
          begin
            AfterOpenConnection;
            AppInitialized := True;
          end
          else begin
            CloseConnection;
            Result := False;
            Application.Terminate;
          end;
        end
        else begin
          CloseConnection;
          Result := False;
        end;
      end;
    end;
  except
    on E: Exception do
      HandleExcept(E, acDb, SErrConnDatabase);
  end;
end;

procedure TBaseDataTemplate.acDbLogin(Sender: TObject; Username, Password: String);
begin
  if acDb.LoginPrompt then
  begin
    Username := ExtractParameterValue(acDb.ConnectionString, CI_UserName);
    if Username = EmptyStr then Username := DefUserName;
    Password := ExtractParameterValue(acDb.ConnectionString, CI_Password);
  end;
end;

procedure TBaseDataTemplate.acDbAfterConnect(Sender: TObject);
var
  DbName: string;
begin
  DbName := Trim(acDb.DefaultDatabase);
  if DbName = EmptyStr then
    DbName := ExtractParameterValue(acDb.ConnectionString, CI_InitCatalog);
  if DbName = EmptyStr then
    DbName := ExtractParameterValue(acDb.ConnectionString, CI_DataSource);
  ShowInStatusBarPanel(0, Format(SFmtConnDatabase, [DbName]));

  {$IFDEF DBWATCHDOG}
  WatchDog.Enabled := WatchDog.Interval > 0; //(WatchDog.Tag = 1);
  {$ENDIF}
end;

{ == Проверка версии базы данных =============================================== }
function TBaseDataTemplate.CheckDbVersion: Boolean;
var
  i, iDbVersion: Integer;
begin
  Result := DbVersion = intDisable;
  try
    // Проверяем версию БД, только если DbVersion > -1
    if not Result then
    begin
      ShowInStatusBar(SMsgCheckDbVersion);

      // Считываем версию из базы данных
      iDbVersion := SafeReadDbVersion;
      Result := DbVersion = iDbVersion;

      // Если считанная и заданная версии не совпали...
      if not Result then
      begin
        // Добавляем запись в системный журнал
        AddToSysLog(tagError, Format(SLogDbVersionWarning, [Application.Title, iDbVersion, DbVersion]));

        // Проверяем корректность считанной версии
        if iDbVersion > intDisable then
        begin
          // Проверяем возможность обновления
          if iDbVersion < DbVersion then
          begin
            // Возможность автообновления, заложенная в PrjVariables
            if DbAutoUpdates then
            begin
              // Версия АРМа больше версии БД, можно обновлять
              if iDbVersion = 0 then
              begin
                if QueryBoxStdYN(SDbVersionQryCreate) = ID_YES then
                  Result := True;
              end
              else begin
                if QueryBoxStdYN(Format(SDbVersionQryUpdate, [iDbVersion, DbVersion])) = ID_YES then
                  Result := True;
              end;

              if Result then
              begin
                // Создаем резервную копию БД по запросу
                if iDbVersion > 0 then
                begin
                  if fResqueEnabled then
                  begin
                    if QueryBoxStdYN(SDbVersionQryResque) = ID_YES then
                    begin
                      CloseConnection;
                      CreateResqueCopy;
                      acDb.Open;
                      Result := acDb.Connected;
                      if Result then
                        Result := InitSysLog;
                    end;
                  end
                  else begin
                    Result := QueryBoxStdNY(SDbVersionWrnResque) = ID_YES;
                  end;
                end;

                if Result then
                begin
                  // Создание новой базы данных - только один скрипт
                  if (iDbVersion = 0) and DbAutoCreateDbLatest then
                  begin
                    Result := SafeUpdateDatabase(0);
                  end
                  else begin
                    // Обновляем все версии по очереди
                    if (DbVersion - iDbVersion) > 1 then
                      ShowProgress(SMsgUpdateDatabase, DbVersion - iDbVersion, False);
                    try
                      for i := iDbVersion to DbVersion - 1 do
                      begin
                        Result := SafeUpdateDatabase(i);
                        if not Result then Break;
                      end;
                    finally
                      if IsShowProgress then
                        CloseProgress;
                    end;
                  end;
                end;

                // Сохраняем в базе данных новый номер версии
                if Result then
                  Result := SafeSaveDbVersion(DbVersion);

                // Не удалось завершить обновление БД
                if not Result then
                  Result := WarningBoxNY(SDbVersionWrnCancel) = ID_YES;
              end
              else begin
                // Пользователь отказался от обновления
                Result := WarningBoxNY(SDbVersionWrnCancel) = ID_YES;
              end;
            end
            else begin
              // Автообновление отключено
              Result := WarningBoxNY(Format(SDbVersionWrnOlder, [iDbVersion, DbVersion])) = ID_YES;
              // "Затычка" для компилятора
              for i := 0 to 0 do
              begin
              end;
            end;
          end
          else begin
            // Версия АРМа меньше версии БД
            Result := WarningBoxNY(Format(SDbVersionWrnNewer, [iDbVersion, DbVersion])) = ID_YES;
          end;
        end
        else begin
          // Ошибка чтения версии БД
          Result := True;
          WarningBox(SDbVersionChkError);
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, SErrCheckDbVersion);
    end;
  end;
end;

{$IFDEF SQL}

{ Версия для программ на базе SQL }

function TBaseDataTemplate.ReadDbVersion: Integer;
begin
  Result := ReadDbSysInteger(acDb, sidDbVersion, 0);
end;

procedure TBaseDataTemplate.SaveDbVersion(const iVersion: Integer);
begin
  SaveDbSysInteger(acDb, sidDbVersion, iVersion);
end;

{$ELSE}

{ Версия для программ на базе СУБД Microsoft Access }

function TBaseDataTemplate.ReadDbVersion: Integer;
const
  sqlReadDbVersion = 'SELECT DISTINCT version FROM version';
var
  qryDbVersion: TAdoQuery;
begin
  Result := intDisable;

  qryDbVersion := OpenDynamicQuery(acDb, sqlReadDbVersion);
  try
    if DataSetIsNotEmpty(qryDbVersion) then
      Result := qryDbVersion.Fields[0].AsInteger;
  finally
    FreeDynamicQuery(qryDbVersion);
  end;
end;

procedure TBaseDataTemplate.SaveDbVersion(const iVersion: Integer);
const
  sqlSaveDbVersion = 'UPDATE version SET version=%d';
begin
  ExecDynamicQuery(acDb, Format(sqlSaveDbVersion, [iVersion]));
end;

{$ENDIF}

function TBaseDataTemplate.SafeReadDbVersion: Integer;
const
  sqlCreateDbVersion1 = 'CREATE TABLE version ([version] INT NOT NULL, CONSTRAINT pk_version_key PRIMARY KEY ([version]))';
  sqlCreateDbVersion2 = 'INSERT INTO version VALUES (%d)';
begin
  Result := intDisable;
  try
    Result := ReadDbVersion;
  except
    {$IFDEF SQL}
    { Версия для программ на базе SQL }
    on E: Exception do
      HandleExcept(E, Self, SErrReadDbVersion);
    {$ELSE}
    { Версия для программ на базе СУБД Microsoft Access }
    try
      ExecDynamicQuery(acDb, sqlCreateDbVersion1);
      ExecDynamicQuery(acDb, Format(sqlCreateDbVersion2, [DbVersion]));
      Result := DbVersion;
    except
      on E: Exception do
        HandleExcept(E, Self, SErrReadDbVersion);
    end;
    {$ENDIF}
  end;
end;

function TBaseDataTemplate.SafeSaveDbVersion(const iVersion: Integer): Boolean;
begin
  Result := True;
  ShowInStatusBar(SMsgSaveDbVersion);
  try
    if ReadDbVersion <> iVersion then
    begin
      SaveDbVersion(iVersion);
      AddToSysLog(tagDbUpdate, Format(SLogDbVersionSaveNum, [Application.Title, iVersion]));
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, SErrSaveDbVersion);
    end;
  end;
end;

function TBaseDataTemplate.UpdateDatabaseSql(const SqlCmd: string; const LogFileName: string): Boolean;
begin
  Result := True;
  if SqlCmd <> EmptyStr then
  begin
    try
      (*
      InfoBox(SqlCmd);
      *)
      SaveToLogFile(LogFileName, Format(SDbVersionSqlText, [SqlCmd]));
      if ExecDynamicQuery(acDb, SqlCmd, 300) then
        SaveToLogFile(LogFileName, SDbVersionSqlOk);
    except
      on E: Exception do
      begin
        SaveToLogFile(LogFileName, Format(SDbVersionSqlError, [E.Message]) + #13#10);
        HandleSqlExcept(E, Self, SqlCmd, SErrUpdateDatabase);
        Result := QueryBoxStdNY(SDbVersionQryError) = ID_YES;
      end;
    end;
  end;
end;

function TBaseDataTemplate.UpdateDatabase(const iVersion: Integer; const FileName: string): Boolean;
var
  bProgress: Boolean;
  fSql: Text;
  pComments: Integer;
  sLine, sTrimLine, sCmdDelimiter, sCmdSql, sLogFile: string;
begin
  Result := True;
  try
    sLogFile := ChangeFileExt(FileName, FormatDateTime('_yyyymmdd_hhnnss', Now) + '.log');
    AssignFile(fSql, FileName);
    Reset(fSql);

    bProgress := not IsShowProgress;
    if bProgress then
      ShowProgress(Format(SMsgUpdateDbStep, [iVersion]), FileSize(fSql));
    try
      {$IFDEF MSSQL}
        sCmdDelimiter := sqlGo;
      {$ELSE}
        sCmdDelimiter := sqlAccessEndCmd;
      {$ENDIF}
      sCmdSql := EmptyStr;

      // Читаем файл до конца файла
      while not Eof(fSql) do
      begin
        ReadLn(fSql, sLine);

        if bProgress then
          UpdateProgressPosition(FilePos(fSql));

        // Вырезаем комменты. Комменты могут быть только В ОДНОЙ СТРОКЕ!!!!
        // 2015-12-19: комменты вырезаются только ВНЕ комманд (когда sCmdSql = EmptyStr)
        if sCmdSql = EmptyStr then
        begin
          pComments := Pos(sqlCommentStart1, sLine);
          if pComments > 0 then
            sLine := Copy(sLine, 1, pComments - 1);
          pComments := Pos(sqlCommentStart2, sLine);
          if pComments > 0 then
            sLine := Copy(sLine, 1, pComments - 1);
          pComments := Pos(sqlCommentStart3, sLine);
          if pComments > 0 then
            sLine := Copy(sLine, 1, pComments - 1);
        end;

        sTrimLine := Trim(sLine);
        if (sTrimLine <> EmptyStr) or (sCmdSql <> EmptyStr) then
        begin
          {$IFDEF MSSQL}
          // Режим Ms SQL Server: разделитель команд (GO) в отдельной строке
          if AnsiSameText(sTrimLine, sqlGo) then
          begin
            Result := UpdateDatabaseSql(sCmdSql, sLogFile);
            sCmdSql := EmptyStr;
            if not Result then Exit;
          end
          else begin
            if sCmdSql = EmptyStr
            then sCmdSql := sLine
            else sCmdSql := sCmdSql + #13 + sLine;
          end;
          {$ELSE}
          // Режим MySQL или Access: разделитель команд в конце строки
          if AnsiStartsText(sqlSetDelimiter, sTrimLine) then
          begin
            sCmdDelimiter := Trim(Copy(sTrimLine, Length(sqlSetDelimiter) + 1, Length(sTrimLine)));
          end
          else begin
            if AnsiEndsText(sCmdDelimiter, sTrimLine) then
            begin
              sTrimLine := TrimRight(Copy(sTrimLine, 1, Length(sTrimLine) - Length(sCmdDelimiter)));
              if Trim(sTrimLine) <> EmptyStr then
              begin
                if sCmdSql = EmptyStr
                then sCmdSql := sTrimLine
                else sCmdSql := sCmdSql + #13 + sTrimLine;
              end;

              Result := UpdateDatabaseSql(sCmdSql, sLogFile);
              sCmdSql := EmptyStr;
              if not Result then Exit;
            end
            else begin
              if sCmdSql = EmptyStr
              then sCmdSql := sLine
              else sCmdSql := sCmdSql + #13 + sLine;
            end;
          end;
          {$ENDIF}

          (* старый метод ------------------------------------------------------
          if SameText(sTrimLine, sqlGo) then
          begin
            Result := UpdateDatabaseSql(sCmdSql, sLogFile);
            sCmdSql := EmptyStr;
            if not Result then Exit;
          end
          else begin
            sTrimLine := TrimRight(sLine);
            if SameText(Copy(sTrimLine, Length(sTrimLine), 1), sqlAccessEndCmd) then
            begin
              if sCmdSql = EmptyStr
              then sCmdSql := Copy(sTrimLine, 1, Length(sTrimLine) - 1)
              else sCmdSql := sCmdSql + #13 + Copy(sTrimLine, 1, Length(sTrimLine) - 1);

              Result := UpdateDatabaseSql(sCmdSql, sLogFile);
              sCmdSql := EmptyStr;
              if not Result then Exit;
            end
            else begin
              if sCmdSql = EmptyStr
              then sCmdSql := sLine
              else sCmdSql := sCmdSql + #13 + sLine;
            end;
          end;
          старый метод ------------------------------------------------------ *)

        end;
      end;
      if sCmdSql <> EmptyStr then
        Result := UpdateDatabaseSql(sCmdSql, sLogFile);
    finally
      CloseFile(fSql);
      if bProgress then
        CloseProgress;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, SErrUpdateDatabase);
    end;
  end;
end;

function TBaseDataTemplate.SafeUpdateDatabase(const iVersion: Integer): Boolean;
const
  fnCreates   = 'CreateDb.sql';
  fnUpdates   = 'Update_%d.sql';
  fnSqlFolder = 'SQL';
var
  sUpdateFile: string;
begin
  if iVersion = 0 then
  begin
    ShowInStatusBar(SMsgUpdateCreate);
    if IsShowProgress then UpdateProgressMessage(SMsgUpdateCreate);
  end
  else begin
    ShowInStatusBar(Format(SMsgUpdateDbStep, [iVersion]));
    if IsShowProgress then UpdateProgressMessage(Format(SMsgUpdateDbStep, [iVersion]));
  end;
  try
    if iVersion = 0 then
    begin
      if DirectoryExists(GetExecuteDirectory + fnSqlFolder)
      then sUpdateFile := IncludeTrailingPathDelimiter(GetExecuteDirectory + fnSqlFolder) + fnCreates
      else sUpdateFile := GetExecuteDirectory + fnCreates;
    end
    else begin
      if DirectoryExists(GetExecuteDirectory + fnSqlFolder)
      then sUpdateFile := IncludeTrailingPathDelimiter(GetExecuteDirectory + fnSqlFolder) + Format(fnUpdates, [iVersion + DbAutoUpdatesOffset])
      else sUpdateFile := GetExecuteDirectory + Format(fnUpdates, [iVersion + DbAutoUpdatesOffset]);
    end;

    if FileExists(sUpdateFile) or PromptForFileName(sUpdateFile,
      Format(SDbVersionFilter, [iVersion, ExtractFileName(sUpdateFile)]),
        '', SDbVersionSelFile, ExtractFilePath(sUpdateFile), False) then
    begin
      Result := UpdateDatabase(iVersion, sUpdateFile);
      if Result then
      begin
        if IsShowProgress then UpdateProgressStep(1);
        AddToSysLog(tagDbUpdate, Format(SLogDbVersionUpdate, [Application.Title, iVersion, sUpdateFile]));
      end;
    end
    else raise Exception.CreateFmt(SDbVersionScrNotFound, [ExtractFileName(sUpdateFile)]);
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, SErrUpdateDatabase);
    end;
  end;
end;

{ == Загрузка параметров системы =============================================== }
procedure TBaseDataTemplate.LoadSystemParameters;
begin
  //
end;

procedure TBaseDataTemplate.SafeLoadSystemParameters;
begin
  ShowInStatusBar(SMsgLoadSystemParams);
  try
    LoadSystemParameters;
    Application.ProcessMessages;
    {$IFDEF ATTACH}
    rAttachs_ReadSysParameters(acDb);
    Application.ProcessMessages;
    {$ENDIF}
  except
    on E: Exception do
      HandleExcept(E, Self, SErrLoadSystemParams);
  end;
end;

{ == Журнал аудита системы ===================================================== }
function TBaseDataTemplate.InitSysLog: Boolean;
begin
  Result := True;
end;

procedure TBaseDataTemplate.DoneSysLog;
begin
  //
end;

procedure TBaseDataTemplate.AddToSysLog(const OperTag: Integer; OperMsg: string);
begin
  // InfoBox('Log: ' + OperMsg);
end;

{ == Закрытие соединения с базой данных ======================================== }
procedure TBaseDataTemplate.BeforeCloseConnection;
begin
  DoneSysLog;
end;

procedure TBaseDataTemplate.CloseConnection;
begin
  if acDb.Connected then
  begin
    try
      BeforeCloseConnection;
    finally
      acDb.Close;
    end;
  end;
end;

procedure TBaseDataTemplate.acDbBeforeDisconnect(Sender: TObject);
begin
  {$IFDEF DBWATCHDOG}
  WatchDog.Enabled := False;
  WatchDog.Free;
  {$ENDIF}

  ShowInStatusBarPanel(0, SFmtNotConnected);
end;

{ == Восстановление подключения к базе данных ================================== }
{$IFDEF DBWATCHDOG}
procedure TBaseDataTemplate.ReconnectToDatabase(Connection: TADOConnection; const iTimeLimit: Integer);
var
  i, iCount: integer;
  tStartTime: TDateTime;
  List: TList;
begin
  Connection.AfterConnect := nil;
  Connection.BeforeDisconnect := nil;
  try
    StartWait;
    ShowInStatusBar(SMsgReconnDatabase);
    ShowWaitMsg(SMsgReconnDatabase);
    try
      List := TList.Create;
      try
        tStartTime := Now();

        // Отключаем от соединения все активные наборы данных
        iCount := Connection.DataSetCount - 1;
        for i := 0 to iCount do
          List.Add(Connection.Datasets[i]);

        while Connection.DataSetCount > 0 do
          Connection.Datasets[0].Connection := nil;

        // Закрываем соединение
        Connection.Close;

        // Восстанавливаем соединение
        iCount := 0;
        while not Connection.Connected do
        begin
          try
            Connection.Open;
          except
            if iTimeLimit > 0 then
            begin
              Inc(iCount);
              if iCount > iTimeLimit then
              begin
                ErrorBox(sErrReconnDatabase);
                Application.Terminate;
                Break;
              end;
            end;
            ChangeWaitMsg(SMsgReconnDatabase + #13#13 + TimeToStr(Now() - tStartTime));
            Delay(1000);
          end;
        end;

        // Восстанавливаем наборы данных
        if Connection.Connected then
        begin
          iCount := List.Count - 1;
          for i := 0 to iCount do
            TCustomADODataSet(List[i]).Connection := Connection;
        end;

        List.Clear;
      finally
        List.Free;
      end;
    finally
      CloseWaitMsg;
      ShowInStatusBar(EmptyStr);
      StopWait;
    end;
  finally
    Connection.AfterConnect := acDbAfterConnect;
    Connection.BeforeDisconnect := acDbBeforeDisconnect;
  end;
end;

procedure TBaseDataTemplate.CheckConnection(const iTimeLimit: Integer);
var
  iRes: Integer;
begin
  try
    {$IFDEF SQL}
    acDb.Execute('SELECT 0', iRes);
    {$ELSE}
    acDb.Execute('SELECT * FROM version', iRes);
    {$ENDIF}
  except
    ReconnectToDatabase(acDb, iTimeLimit);
  end;
end;

procedure TBaseDataTemplate.WatchDogTimer(Sender: TObject);
begin
  WatchDog.Enabled := False;
  try
    CheckConnection(300);
  finally
    WatchDog.Enabled := acDb.Connected and (WatchDog.Interval > 0);
  end;
end;
{$ENDIF}

{ == Изменение параметров соединения с базой данных ============================ }
function TBaseDataTemplate.ChangeConnectProperties: Boolean;
begin
  Result := ChangeAdoDbParameters(ExtractFilePath(Application.ExeName) + SGlobalCfgFile,
    MixConnStrAndParams(acDb.ConnectionString, DbParameters));
end;

procedure TBaseDataTemplate.CreateResqueCopy;
begin
  CreateDbResqueCopy(ExtractFilePath(Application.ExeName) + SGlobalCfgFile);
end;

procedure TBaseDataTemplate.RestoreResqueCopy;
begin
  RestoreDbResqueCopy(ExtractFilePath(Application.ExeName) + SGlobalCfgFile);
end;

{ == Обработка ошибок наборов данных ===========================================}
procedure TBaseDataTemplate.PostError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
begin
  HandleExcept(E, DataSet, Format(SErrPostError, [DataSet.Name]));
  Action := daAbort;
end;

procedure TBaseDataTemplate.DeleteError(DataSet: TDataSet; E: EDatabaseError; var Action: TDataAction);
begin
  HandleExcept(E, DataSet, Format(SErrDeleteError, [DataSet.Name]));
  Action := daAbort;
end;

{ == Открыть или обновить набор данных ========================================= }
function TBaseDataTemplate.OpenDataSet(DataSet: TDataSet;
  const StorePosition: Boolean = True; const OperId: Integer = 0): Boolean;
begin
  if Assigned(DataSet) then
  begin
    DataSet.OnPostError := PostError;
    DataSet.OnDeleteError := DeleteError;
  end;
  Result := OpenDS_Static(acDb, DataSet, StorePosition, fnID, OperId);
end;

function TBaseDataTemplate.OpenDataSetWait(DataSet: TDataSet;
  const StorePosition: Boolean = True; const OperId: Integer = 0): Boolean;
begin
  StartWait;
  try
    Result := OpenDataSet(DataSet, StorePosition, OperId);
  finally
    StopWait;
  end;
end;

function TBaseDataTemplate.OpenDataSetMsg(DataSet: TDataSet;
  const StorePosition: Boolean = True; const OperId: Integer = 0): Boolean;
begin
  StartWait;
  ShowInStatusBar(SMsgLoadDataWait);
  try
    Result := OpenDataSet(DataSet, StorePosition, OperId);
  finally
    ShowInStatusBar(EmptyStr);
    StopWait;
  end;
end;

{ == Загрузка справочника ====================================================== }
function TBaseDataTemplate.OpenReference(DataSet: TDataSet; const Msg: string; const OperId: Integer = 0): Boolean;
begin
  ShowInStatusBar(Msg);
  LookupRefreshLock := True;
  try
    Result := OpenDataSet(DataSet, False, OperId);
  finally
    LookupRefreshLock := False;
  end;
end;

{ == Генерация и загрузка "динамического" запроса ============================== }
function TBaseDataTemplate.OpenVariableQueryEx(Query: TAdoQuery; Filter: TRDbFilter; Order: TRDbOrder;
  const SelectSql, BaseWhereSql, GroupBySql: string; const OperId: Integer = 0): Boolean;
begin
  if Assigned(Query) then
  begin
    Query.OnPostError := PostError;
    Query.OnDeleteError := DeleteError;
  end;
  Result := OpenDS_VariableQuery(acDb, Query, Filter, Order,
    SelectSql, BaseWhereSql, GroupBySql, EmptyStr, True, fnID, OperId);
end;

function TBaseDataTemplate.OpenVariableQuery(Query: TAdoQuery; Filter: TRDbFilter; Order: TRDbOrder;
  const SelectSql, BaseWhereSql: string; const OperId: Integer = 0): Boolean;
begin
  Result := OpenVariableQueryEx(Query, Filter, Order,
    SelectSql, BaseWhereSql, EmptyStr, OperId);
end;

function TBaseDataTemplate.OpenStaticQuery(Query: TAdoQuery;
  const SelectSql: string; const OperId: Integer = 0): Boolean;
begin
  Result := OpenVariableQueryEx(Query, nil, nil,
    SelectSql, EmptyStr, EmptyStr, OperId);
end;

{ == Генерация индектов таблицы без блокировок ================================= }
function TBaseDataTemplate.GetNewId(const DsName: string; const KeyName: string = fnID): Integer;
begin
  {$IFDEF SQL}
  { Версия для программ на базе SQL }
  Result := GetBlockedID(acDb, DsName, KeyName);
  {$ELSE}
  { Версия для программ на базе СУБД Microsoft Access }
  Result := GetNextID(acDb, DsName, KeyName);
  {$ENDIF}

  { Контроль демо-версии }
  {$IFDEF DEMO}
  if Result > 10 then
  begin
    FreeId(DsName, Result);
    raise Exception.Create('Вы используете демонстрационную версию с ограниченной функциональностью!');
  end;
  {$ENDIF}
end;

procedure TBaseDataTemplate.FreeId(const DsName: string; const Id: Integer);
begin
  {$IFDEF SQL}
  { Версия для программ на базе SQL }
  FreeBlockedID(acDb, DsName, Id);
  {$ELSE}
  { Версия для программ на базе СУБД Microsoft Access }
  {$ENDIF}
end;

end.

