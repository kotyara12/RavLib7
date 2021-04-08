unit RDbLogger;

interface

uses
  AdoDb, Db, SysUtils, Classes;

type
  ERDbLoggerError = class(Exception);

  TRDbLogger = class
  private
    fDb: TAdoConnection;
    fActive: Boolean;
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
    function  _DeleteOldRecords(const aLimitDays: Integer): Boolean;
    function  _CheckLogSize(const aLimitRecs: Integer): Boolean;
    function  _InitLog: Boolean;
    function  GetActive: Boolean;
  public
    constructor Create(const aDb: TAdoConnection; const aSqlDateFormat: string;
      const aAppId, aUserId: Integer; const aShowWarning: Boolean); overload;
    destructor Destroy; override;
    function  Open: Boolean;
    procedure Close;
    function  DeleteOldRecords: Boolean;
    function  CheckLogSize: Boolean;
    function  CheckLogState: Boolean;
    function  AddToLog(const AppId, UserId, OperId: Integer; const MsgStr: string): Boolean; overload;
    function  AddToLog(const OperId: Integer; const MsgStr: string): Boolean; overload;
    property Active: Boolean read GetActive;
    property AppId: Integer read fAppId write fAppId;
    property UserId: Integer read fUserId write fUserId;
    property MaxMsgSize: Word read fMaxMsgSize write fMaxMsgSize;
    property ShowSizeWarning: Boolean read fShowWarning write fShowWarning;
    property OnOpen: TNotifyEvent read fOnOpen write fOnOpen;
    property OnClose: TNotifyEvent read fOnClose write fOnClose;
  end;

const
  sidSL_Enabled           = 9001;
  sidSL_LimitRecs         = 9002;
  sidSL_LimitDays         = 9005;

  defSL_MsgSize           = 65535;
  defSL_LimitRecs         = 1000000;
  defSL_LimitDays         = 0;

implementation

uses
  DateUtils, RDialogs, RSysUtils, RDbUtils, RDbSettings, RExHandlers;
  
resourcestring
  EDbLoggerInit       = 'Ошибка инициализации журнала аудита!';
  EDbLoggerAddMsg     = 'Ошибка записи сообщения в журнал аудита!';

  EDbNotConnected     = 'Соединение с базой данных не установлено!';

  SWrnDbLogTooBig     = 'Журнал аудита системы заполнен!'#13'Для очистки журнала аудита используйте АРМ "Администратор".';

const
  tnSysLog            = 'ss_syslog';

{ TRDbLogger }

constructor TRDbLogger.Create(const aDb: TAdoConnection; const aSqlDateFormat: string;
  const aAppId, aUserId: Integer; const aShowWarning: Boolean);
begin
  inherited Create;
  fDb := aDb;
  fFirstOpen := True;
  fSqlDateFormat := aSqlDateFormat;
  fActive := False;
  fShowWarning := aShowWarning;
  fMaxMsgSize := defSL_MsgSize;
  fHostName := GetComputerNetName;
  fUserName := GetCurrentUserName;
  fAppId := aAppId;
  fUserId := aUserId;
  Open;
end;

destructor TRDbLogger.Destroy;
begin
  Close;
  fDb := nil;
  inherited Destroy;
end;

function TRDbLogger.GetActive: Boolean;
begin
  Result := Assigned(fDb) and fDb.Connected and fActive;
end;

function TRDbLogger.Open: Boolean;
begin
  Result := False;
  try
    if fDb.Connected then
      Result := _InitLog
    else
      ERDbLoggerError.Create(EDbNotConnected);

    if Active and Assigned(fOnOpen) then
      fOnOpen(Self);
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, EDbLoggerInit, 0, 0, ecExcludeDbLog);
    end;
  end;
end;

procedure TRDbLogger.Close;
begin
  if fActive then
  begin
    fActive := False;
    if Assigned(fOnClose) then
      fOnClose(Self);
  end;
end;

function TRDbLogger._DeleteOldRecords(const aLimitDays: Integer): Boolean;
const
  sqlDelOldRecs = 'DELETE FROM ss_syslog WHERE dateoper<%s';
var
  sqlText: string;
begin
  Result := True;
  try
    sqlText := Format(sqlDelOldRecs, [DateToSqlStr(fSqlDateFormat, IncDay(Now, -aLimitDays))]);
    ExecDynamicQuery(fDb, sqlText, 600);
  except
    on E: Exception do
    begin
      Result := False;
      HandleSqlExcept(E, Self, sqlText, EDbLoggerInit, 0, 0, ecExcludeDbLog);
    end;
  end;
end;

function TRDbLogger.DeleteOldRecords: Boolean;
var
  fLimitDays: Integer;
begin
  Result := False;
  try
    if CheckLogState then
    begin
      fLimitDays := ReadDbSysInteger(fDb, sidSL_LimitDays, defSL_LimitDays);
      if fLimitDays > 0 then
        Result := _DeleteOldRecords(fLimitDays)
      else
        Result := True;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, EDbLoggerInit, 0, 0, ecExcludeDbLog);
    end;
  end;
end;

function TRDbLogger._CheckLogSize(const aLimitRecs: Integer): Boolean;
const
  sqlRecCount = 'SELECT Count(*) FROM ss_syslog';
var
  qryRecCount: TAdoQuery;
begin
  Result := False;
  try
    qryRecCount := OpenDynamicQuery(fDb, sqlRecCount, 600);
    try
      if DataSetIsNotEmpty(qryRecCount) then
        Result := qryRecCount.Fields[0].AsInteger < aLimitRecs;
    finally
      FreeDynamicQuery(qryRecCount);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleSqlExcept(E, Self, sqlRecCount, EDbLoggerInit, 0, 0, ecExcludeDbLog);
    end;
  end;
end;

function TRDbLogger.CheckLogSize: Boolean;
begin
  Result := False;
  try
    if CheckLogState then
    begin
      Result := _CheckLogSize(ReadDbSysInteger(fDb, sidSL_LimitRecs, defSL_LimitRecs));
      if not Result and fShowWarning then
        WarningBox(SWrnDbLogTooBig);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, EDbLoggerInit, 0, 0, ecExcludeDbLog);
    end;
  end;
end;

function TRDbLogger._InitLog: Boolean;
begin
  Result := False;
  try
    fActive := ReadDbSysBoolean(fDb, sidSL_Enabled, False)
          and (ReadDbSysInteger(fDb, sidSL_LimitRecs, defSL_LimitRecs) > 0);
    if fActive then
      Result := DeleteOldRecords and CheckLogSize;
  except
    on E: Exception do
    begin
      Result := False;
      HandleExcept(E, Self, EDbLoggerInit, 0, 0, ecExcludeDbLog);
    end;
  end;
end;

function TRDbLogger.CheckLogState: Boolean;
const
  sqlCheck = 'SELECT Count(*) FROM ss_syslog WHERE id_operations=0';
var
  qryCheck: TAdoQuery;
begin
  Result := False;
  if Active then
  begin
    try
      qryCheck := OpenDynamicQuery(fDb, sqlCheck, 60);
      try
        Result := DataSetIsOpen(qryCheck);
      finally
        FreeDynamicQuery(qryCheck);
      end;
    except
      on E: Exception do
      begin
        Result := False;
        // Ошибки дополнительно не обрабатываем !!!
      end;
    end;
  end;
end;

function TRDbLogger.AddToLog(const AppId, UserId, OperId: Integer; const MsgStr: string): Boolean;
const
  sqlAddToDbLog = 'INSERT INTO ss_syslog (id_operations, id_users, id_workplases, info, host, netuser) ' +
                  'VALUES (%d, %d, %d, ''%s'', ''%s'', ''%s'')';
var
  sqlText: string;
begin
  Result := True;
  if Active then
  begin
    try
      sqlText := Format(sqlAddToDbLog, [OperId, UserId, AppId, StrToSqlStr(MsgStr, fMaxMsgSize), fHostName, fUserName]);
      ExecDynamicQuery(fDb, sqlText, 60);
    except
      on E: Exception do
      begin
        Result := False;
        HandleSqlExcept(E, Self, sqlText, EDbLoggerAddMsg, 0, 0, ecExcludeDbLog);
      end;
    end;
  end;
end;

function TRDbLogger.AddToLog(const OperId: Integer; const MsgStr: string): Boolean;
begin
  Result := AddToLog(fAppId, fUserId, OperId, MsgStr);
end;

end.
