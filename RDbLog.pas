unit RDbLog;

interface

uses
  SysUtils, AdoDb, RUserRights, RDbLogger;

var
  AppLog: TRDbLogger;

procedure DbLog_Init(const Db: TADOConnection; const sDateFmt: string; const idApp: Integer; const aUser: TUserRights; const bShowWarning: Boolean);
procedure DbLog_UpdateUser(const aUser: TUserRights);
function  DbLog_IsEnabled: Boolean;
function  DbLog_DeleteOldRecords: Boolean;
function  DbLog_CheckLogState: Boolean;
procedure DbLog_Close;

procedure AddToDbLog(const OperId: Integer; const Msg: string);
procedure AddToDbLogUser(const OperId, AppId, UserId: Integer; const Msg: string);

resourcestring
  SErrLogExists    = 'Текущий экземпляр системного протокола уже создан!';
  SErrLogNil       = 'Нет активного экземпляра системного протокола!';

implementation

procedure DbLog_Init(const Db: TADOConnection; const sDateFmt: string; const idApp: Integer; const aUser: TUserRights; const bShowWarning: Boolean);
begin
  DbLog_Close;
  if not Assigned(AppLog) then
    AppLog := TRDbLogger.Create(Db, sDateFmt, idApp, aUser.UserId, bShowWarning);
end;

procedure DbLog_UpdateUser(const aUser: TUserRights);
begin
  if not Assigned(AppLog) then
    raise Exception.Create(SErrLogNil);
  AppLog.UserId := aUser.UserId;
end;

function DbLog_IsEnabled: Boolean;
begin
  Result := Assigned(AppLog) and AppLog.Active;
end;

function DbLog_DeleteOldRecords: Boolean;
begin
  Result := Assigned(AppLog) and AppLog.DeleteOldRecords;
end;

function DbLog_CheckLogState: Boolean;
begin
  Result := Assigned(AppLog) and AppLog.CheckLogState;
end;

procedure DbLog_Close;
begin
  if Assigned(AppLog) then FreeAndNil(AppLog);
end;

procedure AddToDbLog(const OperId: Integer; const Msg: string);
begin
  // if not Assigned(AppLog) then raise Exception.Create(SErrLogNil);
  if Assigned(AppLog) then
    AppLog.AddToLog(OperId, Msg);
end;

procedure AddToDbLogUser(const OperId, AppId, UserId: Integer; const Msg: string);
begin
  // if not Assigned(AppLog) then raise Exception.Create(SErrLogNil);
  if Assigned(AppLog) then
    AppLog.AddToLog(AppId, UserId, OperId, Msg);
end;

initialization
  AppLog := nil;

finalization
  if Assigned(AppLog) then FreeAndNil(AppLog);

end.
