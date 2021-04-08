unit RDbGetId;

interface

uses
  SysUtils, ADODb;

{ Генерация нового ID записи }
function  GetNextID(Db: TADOConnection; const ATabName, AKeyName: string): Integer;
function  GetNextSubID(Db: TADOConnection; const ATabName, AKeyName, ASubWhere: string): Integer;
function  GetBlockedID(Db: TADOConnection; const ATabName, AKeyName: string): Integer;
procedure FreeBlockedID(Db: TADOConnection; const ATabName: string; AID: Integer);

implementation

uses
  RDbConst, RDbUtils, RExHandlers;
  
resourcestring
  SErrNextID       = 'Ошибка генерации идентификатора записи!';

const
  fnMAXID         = 'max_id';

{ == Генерация нового ID записи ================================================ }

function GetNextID(Db: TADOConnection; const ATabName, AKeyName: string): Integer;
const
  sqlGetNextID = 'SELECT Max(%s) AS ' + fnMAXID + ' FROM %s';
var
  qryFreeID: TADOQuery;
begin
  Result := 1;
  qryFreeID := nil;
  try
    try
      qryFreeID := OpenDynamicQuery(Db, Format(sqlGetNextID, [AKeyName, ATabName]));
      if DynamicQueryIsNotEmpty(qryFreeID) then
        Result := qryFreeID.FieldByName(fnMAXID).AsInteger + 1;
    except
      on E: Exception do
        HandleExcept(E, qryFreeID, SErrNextID);
    end;
  finally
    FreeDynamicQuery(qryFreeID);
  end;
end;

function GetNextSubID(Db: TADOConnection; const ATabName, AKeyName, ASubWhere: string): Integer;
const
  sqlGetNextID = 'SELECT Max(%s) AS ' + fnMAXID + ' FROM %s WHERE %s';
var
  qryFreeID: TADOQuery;
begin
  Result := 1;
  qryFreeID := nil;
  try
    try
      qryFreeID := OpenDynamicQuery(Db, Format(sqlGetNextID, [AKeyName, ATabName, ASubWhere]));
      if DynamicQueryIsNotEmpty(qryFreeID) then
        Result := qryFreeID.FieldByName(fnMAXID).AsInteger + 1;
    except
      on E: Exception do
        HandleExcept(E, qryFreeID, SErrNextID);
    end;
  finally
    FreeDynamicQuery(qryFreeID);
  end;
end;

function CheckBlockedID(Db: TADOConnection; const ATabName: string; AID: Integer): Boolean;
const
  sqlBlocked = 'SELECT Count(*) as ' + fnCOUNT + ' FROM ss_blockids WHERE ' + fnNAME + '=''%s'' AND ' + fnID + '=%d';
var
  qryBlocked: TADOQuery;
begin
  Result := False;
  qryBlocked := nil;
  try
    try
      qryBlocked := OpenDynamicQuery(Db, Format(sqlBlocked, [ATabName, AID]));
      if DynamicQueryIsNotEmpty(qryBlocked) then
        Result := qryBlocked.FieldByName(fnCOUNT).AsInteger > 0;
    except
      on E: Exception do
        HandleExcept(E, qryBlocked, SErrNextID);
    end;
  finally
    FreeDynamicQuery(qryBlocked);
  end;
end;

function GetBlockedID(Db: TADOConnection; const ATabName, AKeyName: string): Integer;
const
  sqlSetBlockID = 'INSERT INTO ss_blockids (' + fnNAME + ', ' + fnID + ') VALUES (''%s'', %d)';
begin
  Result := GetNextID(Db, ATabName, AKeyName);
  while CheckBlockedID(Db, ATabName, Result) do
    Inc(Result);
  try
    ExecDynamicQuery(Db, Format(sqlSetBlockID, [ATabName, Result]));
  except
    on E: Exception do
      HandleSqlExcept(E, nil, Format(sqlSetBlockID, [ATabName, Result]), SErrNextID);
  end;
end;

procedure FreeBlockedID(Db: TADOConnection; const ATabName: string; AID: Integer);
const
  sqlFreeBlockID = 'DELETE FROM ss_blockids WHERE ' + fnNAME + '=''%s'' AND ' + fnID + '=%d';
begin
  try
    if CheckBlockedID(Db, ATabName, AID) then
      ExecDynamicQuery(Db, Format(sqlFreeBlockID, [ATabName, AID]));
  except
    on E: Exception do
      HandleSqlExcept(E, nil, Format(sqlFreeBlockID, [ATabName, AID]), SErrNextID);
  end;
end;

end.
