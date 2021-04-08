unit RDbLogger;

interface

uses
  AdoDb, Db, SysUtils, Classes;

type
  ERDbLoggerError = class(Exception);

  TRDbLogger = class
  private
    fDb: TAdoConnection;
    fInternalDb: Boolean;
    fIntEnabled: Boolean;
    fFirstOpen: Boolean;
    fShowWarning: Boolean;
    fSqlDateFormat: string;
    fMaxMsgSize: Word;
    fHostName: string;
    fUserName: string;
    fAppId: Integer;
    fUserId: Integer;
    fOnOpen: TNotifyEvent;
    fOnClose: TNotifyEvent;
    procedure InternalInitLog;
    procedure FirstInitLog(const MaxLogSize, MaxLogPeriod: Integer);
    function  GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  public
    constructor Create(aDb: TAdoConnection; const aAppId, aUserId: Integer; const aSqlDateFormat: string; const ShowWarning: Boolean); overload;
    constructor Create(aConnectionStr: string; const aAppId, aUserId: Integer; const aSqlDateFormat: string; const ShowWarning: Boolean); overload;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure AddToLog(const AppId, UserId, OperId: Integer; const MsgStr: string); overload;
    procedure AddToLog(const OperId: Integer; const MsgStr: string); overload;
    property Active: Boolean read GetActive write SetActive;
    property AppId: Integer read fAppId write fAppId;
    property UserId: Integer read fUserId write fUserId;
    property MaxMessageLength: Word read fMaxMsgSize write fMaxMsgSize;
    property ShowSizeWarning: Boolean read fShowWarning write fShowWarning;
    property OnOpen: TNotifyEvent read fOnOpen write fOnOpen;
    property OnClose: TNotifyEvent read fOnClose write fOnClose;
  end;

const
  sidEnableSysLog         = 9001;
  sidMaxSysLogRecords     = 9002;
  sidLogPeriod            = 9005;
  
  defMaxMsgSize           = 255;
  defMaxSysLogRecords     = 1000000;
  defLogPeriod            = 0;

implementation

uses
  DateUtils, RDialogs, RSysUtils, RDbUtils, RDbSettings, RExHandlers;
  
resourcestring
  EDbLoggerInit       = 'Ошибка инициализации журнала аудита!';
  EDbLoggerAddMsg     = 'Ошибка записи сообщения в журнал аудита!';
  
  EDbNotCreated       = 'Соединение с базой данных не создано!';
  EDbNotConnected     = 'Соединение с базой данных не установлено!';

  SWrnDbLogTooBig     = 'Журнал аудита системы заполнен!'#13'Для очистки журнала аудита используйте АРМ "Администратор".';

{ TRDbLogger }

constructor TRDbLogger.Create(aDb: TAdoConnection; const aAppId,
  aUserId: Integer; const aSqlDateFormat: string; const ShowWarning: Boolean);
begin
  inherited Create;
  fInternalDb := False;
  fFirstOpen := True;
  fShowWarning := ShowWarning;
  fDb := aDb;
  fSqlDateFormat := aSqlDateFormat;
  fIntEnabled := False;
  fMaxMsgSize := defMaxMsgSize;
  fHostName := GetComputerNetName;
  fUserName := GetCurrentUserName;
  fAppId := aAppId;
  fUserId := aUserId;
  Open;
end;

constructor TRDbLogger.Create(aConnectionStr: string; const aAppId,
  aUserId: Integer; const aSqlDateFormat: string; const ShowWarning: Boolean);
begin
  inherited Create;
  fInternalDb := True;
  fFirstOpen := True;
  fShowWarning := ShowWarning;
  fDb := TAdoConnection.Create(nil);
  fDb.LoginPrompt := False;
  fDb.KeepConnection := True;
  fDb.ConnectionString := aConnectionStr;
  fSqlDateFormat := aSqlDateFormat;
  fIntEnabled := False;
  fMaxMsgSize := defMaxMsgSize;
  fHostName := GetComputerNetName;
  fUserName := GetCurrentUserName;
  fAppId := aAppId;
  fUserId := aUserId;
  Open;
end;

destructor TRDbLogger.Destroy;
begin
  Close;
  if fInternalDb then fDb.Free;
  fDb := nil;
  inherited Destroy;
end;

function TRDbLogger.GetActive: Boolean;
begin
  Result := Assigned(fDb) and fDb.Connected and fIntEnabled;
end;

procedure TRDbLogger.SetActive(const Value: Boolean);
begin
  if Value <> Active then
  begin
    if Value then Open else Close;
  end;
end;

procedure TRDbLogger.Open;
begin
  try
    if fInternalDb then
    begin
      if Assigned(fDb) then
        fDb.Open
      else
        ERDbLoggerError.Create(EDbNotCreated);
    end;
    if fDb.Connected then
      InternalInitLog
    else
      ERDbLoggerError.Create(EDbNotConnected);
    if Active and Assigned(fOnOpen) then
      fOnOpen(Self);
  except
    on E: Exception do
      HandleExcept(E, Self, EDbLoggerInit);
  end;
end;

procedure TRDbLogger.Close;
begin
  fIntEnabled := False;
  if Active and Assigned(fOnClose) then
    fOnClose(Self);
  if fInternalDb and Assigned(fDb) and fDb.Connected then 
    fDb.Close;
end;

procedure TRDbLogger.FirstInitLog(const MaxLogSize, MaxLogPeriod: Integer);

  procedure DeleteOldRecords;
  const
    sqlDelOldRecs = 'DELETE FROM ss_syslog WHERE datetime<%s';
  var
    sqlText: string;
  begin
    try
      sqlText := Format(sqlDelOldRecs, 
        [DateToSqlStr(fSqlDateFormat, IncDay(Now, -MaxLogPeriod))]);
      ExecDynamicQuery(fDb, sqlText, 600);
    except
      on E: Exception do
         HandleSqlExcept(E, Self, sqlText, EDbLoggerInit);
    end;
  end;

  procedure CheckLogSize;
  const
    sqlRecCount = 'SELECT Count(*) FROM ss_syslog';
  var
    sqlDs: TAdoQuery;
  begin
    try
      sqlDs := OpenDynamicQuery(fDb, sqlRecCount, 600);
      try
        if DataSetIsNotEmpty(sqlDs) then
        begin
          if sqlDs.Fields[0].AsInteger > MaxLogSize then
            WarningBox(SWrnDbLogTooBig);
        end;
      finally
        FreeDynamicQuery(sqlDs);
      end;
    except
      on E: Exception do
         HandleSqlExcept(E, Self, sqlRecCount, EDbLoggerInit);
    end;
  end;
  
begin
  fFirstOpen := False;
  if (MaxLogPeriod > 0) and (fSqlDateFormat <> EmptyStr) then
    DeleteOldRecords;
  if fShowWarning then
    CheckLogSize;
end;

procedure TRDbLogger.InternalInitLog;
var
  LogSize, LogPeriod: Integer;
begin
  try
    LogSize := ReadDbSysInteger(FDb, sidMaxSysLogRecords, defMaxSysLogRecords);
    LogPeriod := ReadDbSysInteger(FDb, sidLogPeriod, defLogPeriod);
    fIntEnabled := (LogSize > 0) and ReadDbSysBoolean(FDb, sidEnableSysLog, False);
    if fIntEnabled then
    begin
      if fFirstOpen then
        FirstInitLog(LogSize, LogPeriod);
    end
    else Close;
  except
    on E: Exception do
    begin
      try
        HandleExcept(E, Self, EDbLoggerInit);
      finally
        Close;
      end;
    end;
  end;
end;

procedure TRDbLogger.AddToLog(const AppId, UserId, OperId: Integer; const MsgStr: string);
const
  {$IFDEF MSSQL}
  sqlAddToDbLog = 'INSERT INTO ss_syslog (datetime, id_operations, id_users, id_workplases, info, host, netuser) ' +
                  'VALUES (GetDate(), %d, %d, %d, ''%s'', ''%s'', ''%s'')';
  {$ENDIF}
  {$IFDEF MYSQL}
  sqlAddToDbLog = 'INSERT INTO ss_syslog (datetime, id_operations, id_users, id_workplases, info, host, netuser) ' +
                  'VALUES (Now(), %d, %d, %d, ''%s'', ''%s'', ''%s'')';
  {$ENDIF}
var
  sqlText: string;
begin
  if Active then
  begin
    try
      sqlText := Format(sqlAddToDbLog, [OperID, UserId, AppId, StrToSqlStr(MsgStr, fMaxMsgSize), fHostName, fUserName]);
      ExecDynamicQuery(fDb, sqlText);
    except
      on E: Exception do
      begin
        try
          HandleSqlExcept(E, Self, sqlText, EDbLoggerAddMsg);
        finally
          Close;
        end;
      end;
    end;
  end;
end;

procedure TRDbLogger.AddToLog(const OperId: Integer; const MsgStr: string);
begin
  AddToLog(fAppId, fUserId, OperId, MsgStr);
end;

end.
