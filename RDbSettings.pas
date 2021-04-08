unit RDbSettings;

interface

uses
  Controls, ADODb;

const
  tnSysSettings  = 'ss_settings';

{ ѕроверка существовани€ записи в таблице системных настроек }
function  CheckDbSys(Db: TADOConnection; const AId: Integer): Boolean;
{ „тение значени€ из таблицы системных настроек }
function  ReadDbSysInteger(Db: TADOConnection; const AId, ADef: Integer): Integer;
function  ReadDbSysFloat(Db: TADOConnection; const AId: Integer; const ADef: Double): Double;
function  ReadDbSysDateTime(Db: TADOConnection; const AId: Integer; const ADef: TDateTime): TDateTime;
function  ReadDbSysDate(Db: TADOConnection; const AId: Integer; const ADef: TDate): TDate;
function  ReadDbSysTime(Db: TADOConnection; const AId: Integer; const ADef: TTime): TTime;
function  ReadDbSysString(Db: TADOConnection; const AId: Integer; const ADef: string): string;
function  ReadDbSysText(Db: TADOConnection; const AId: Integer; const ADef: string): string;
function  ReadDbSysBoolean(Db: TADOConnection; const AId: Integer; const ADef: Boolean): Boolean;
{ —хранение значени€ в таблицев системных настроек }
function  SaveDbSysInteger(Db: TADOConnection; const AId, AValue: Integer): Boolean;
function  SaveDbSysFloat(Db: TADOConnection; const AId: Integer; const AValue: Double): Boolean;
function  SaveDbSysDateTime(Db: TADOConnection; const AId: Integer; const AValue: TDateTime): Boolean;
function  SaveDbSysDate(Db: TADOConnection; const AId: Integer; const AValue: TDate): Boolean;
function  SaveDbSysTime(Db: TADOConnection; const AId: Integer; const AValue: TTime): Boolean;
function  SaveDbSysString(Db: TADOConnection; const AId: Integer; const AValue: string): Boolean;
function  SaveDbSysText(Db: TADOConnection; const AId: Integer; const AValue: string): Boolean;
function  SaveDbSysBoolean(Db: TADOConnection; const AId: Integer; const AValue: Boolean): Boolean;

implementation

uses
  SysUtils, DateUtils, RDbConst, RDbUtils, RExHandlers;

resourcestring
  SErrorReadSettings     = 'ќшибка чтени€ глобального параметра id=%d! ”становлено значение по умолчанию.';
  SErrorSaveSettings     = 'ќшибка сохранени€ глобального параметра id=%d!';

  sqlReadDbSettings      = 'SELECT %s FROM ss_settings WHERE id=%d';

(*$HINTS OFF*)

function ReadDbSysInteger(Db: TADOConnection; const AId, ADef: Integer): Integer;
var
  qrySettings: TADOQuery;
begin
  Result := ADef;
  try
    qrySettings := OpenDynamicQuery(Db, Format(sqlReadDbSettings, [fnVALUE_INT, AId]));
    try
      if DataSetIsNotEmpty(qrySettings)
      then Result := qrySettings.FieldByName(fnVALUE_INT).AsInteger;
    finally
      FreeDynamicQuery(qrySettings);
    end;
  except
    on E: Exception do
      HandleSqlExcept(E, nil, Format(sqlReadDbSettings, [fnVALUE_INT, AId]),
        Format(SErrorReadSettings, [AId]), 0, 0, esReg);
  end;
end;

function ReadDbSysFloat(Db: TADOConnection; const AId: Integer; const ADef: Double): Double;
var
  qrySettings: TADOQuery;
begin
  Result := ADef;
  try
    qrySettings := OpenDynamicQuery(Db, Format(sqlReadDbSettings, [fnVALUE_REAL, AId]));
    try
      if DataSetIsNotEmpty(qrySettings)
      then Result := qrySettings.FieldByName(fnVALUE_REAL).AsFloat;
    finally
      FreeDynamicQuery(qrySettings);
    end;
  except
    on E: Exception do
      HandleSqlExcept(E, nil, Format(sqlReadDbSettings, [fnVALUE_REAL, AId]),
        Format(SErrorReadSettings, [AId]), 0, 0, esReg);
  end;
end;

function ReadDbSysDateTime(Db: TADOConnection; const AId: Integer; const ADef: TDateTime): TDateTime;
var
  qrySettings: TADOQuery;
begin
  Result := TDateTime(ReadDbSysFloat(Db, AId, ADef));
end;

function ReadDbSysDate(Db: TADOConnection; const AId: Integer; const ADef: TDate): TDate;
begin
  Result := DateOf(ReadDbSysDateTime(Db, AId, ADef));
end;

function ReadDbSysTime(Db: TADOConnection; const AId: Integer; const ADef: TTime): TTime;
begin
  Result := TimeOf(ReadDbSysDateTime(Db, AId, ADef));
end;

function ReadDbSysString(Db: TADOConnection; const AId: Integer; const ADef: string): string;
var
  qrySettings: TADOQuery;
begin
  Result := ADef;
  try
    qrySettings := OpenDynamicQuery(Db, Format(sqlReadDbSettings, [fnVALUE_CHAR, AId]));
    try
      if DataSetIsNotEmpty(qrySettings)
      then Result := qrySettings.FieldByName(fnVALUE_CHAR).AsString;
    finally
      FreeDynamicQuery(qrySettings);
    end;
  except
    on E: Exception do
      HandleSqlExcept(E, nil, Format(sqlReadDbSettings, [fnVALUE_CHAR, AId]),
        Format(SErrorReadSettings, [AId]), 0, 0, esReg);
  end;
end;

function ReadDbSysText(Db: TADOConnection; const AId: Integer; const ADef: string): string;
var
  qrySettings: TADOQuery;
begin
  Result := ADef;
  try
    qrySettings := OpenDynamicQuery(Db, Format(sqlReadDbSettings, [fnVALUE_CHAR, AId]));
    try
      if DataSetIsNotEmpty(qrySettings)
      then Result := qrySettings.FieldByName(fnVALUE_CHAR).AsString;
    finally
      FreeDynamicQuery(qrySettings);
    end;
  except
    on E: Exception do
      HandleSqlExcept(E, nil, Format(sqlReadDbSettings, [fnVALUE_CHAR, AId]),
        Format(SErrorReadSettings, [AId]), 0, 0, esReg);
  end;
end;

function ReadDbSysBoolean(Db: TADOConnection; const AId: Integer; const ADef: Boolean): Boolean;
begin
  if ADef
  then Result := ReadDbSysInteger(Db, AId, 1) <> 0
  else Result := ReadDbSysInteger(Db, AId, 0) <> 0;
end;

{ == ѕроверка существовани€ записи в таблице системных нстроек ================= }
function CheckDbSys(Db: TADOConnection; const AId: Integer): Boolean;
const
  sqlValueIsExists     = 'SELECT Count(*) AS cnt FROM ss_settings WHERE id=%d';
var
  qrySettings: TADOQuery;
begin
  Result := False;
  try
    qrySettings := OpenDynamicQuery(Db, Format(sqlValueIsExists, [AId]));
    try
      if DataSetIsNotEmpty(qrySettings)
      then Result := qrySettings.FieldByName(fnCOUNT).AsInteger > 0;
    finally
      FreeDynamicQuery(qrySettings);
    end;
  except
    on E: Exception do
      HandleSqlExcept(E, nil, Format(sqlValueIsExists, [AId]),
        Format(SErrorReadSettings, [AId]));
  end;
end;

{ == —охранение значени€ в таблицев системных настроек ========================= }
function SaveDbSysInteger(Db: TADOConnection; const AId, AValue: Integer): Boolean;
const
  sqlNewIntValue  = 'INSERT INTO ss_settings (id, ' + fnVALUE_INT + ') VALUES (%d, %d)';
  sqlSaveIntValue = 'UPDATE ss_settings SET ' + fnVALUE_INT + '=%d WHERE id=%d';
var
  CurrSqlStr: string;
begin
  Result := False;
  try
    if CheckDbSys(Db, AId)
    then CurrSqlStr := Format(sqlSaveIntValue, [AValue, AId])
    else CurrSqlStr := Format(sqlNewIntValue, [AId, AValue]);
    Result := ExecDynamicQuery(Db, CurrSqlStr);
  except
    on E: Exception do
      HandleSqlExcept(E, nil, CurrSqlStr, Format(SErrorSaveSettings, [AId]));
  end;
end;

function SaveDbSysFloat(Db: TADOConnection; const AId: Integer; const AValue: Double): Boolean;
const
  sqlNewIntValue  = 'INSERT INTO ss_settings (id, ' + fnVALUE_REAL + ') VALUES (%d, %s)';
  sqlSaveIntValue = 'UPDATE ss_settings SET ' + fnVALUE_REAL + '=%s WHERE id=%d';
var
  CurrSqlStr: string;
  SqlValue: string;
begin
  Result := False;
  try
    SqlValue := FloatToSqlStr(AValue);
    if CheckDbSys(Db, AId)
    then CurrSqlStr := Format(sqlSaveIntValue, [SqlValue, AId])
    else CurrSqlStr := Format(sqlNewIntValue, [AId, SqlValue]);
    Result := ExecDynamicQuery(Db, CurrSqlStr);
  except
    on E: Exception do
      HandleSqlExcept(E, nil, CurrSqlStr, Format(SErrorSaveSettings, [AId]));
  end;
end;

function SaveDbSysDateTime(Db: TADOConnection; const AId: Integer; const AValue: TDateTime): Boolean;
begin
  Result := SaveDbSysFloat(Db, AId, AValue);
end;

function SaveDbSysDate(Db: TADOConnection; const AId: Integer; const AValue: TDate): Boolean;
begin
  Result := SaveDbSysDateTime(Db, AId, DateOf(AValue));
end;

function SaveDbSysTime(Db: TADOConnection; const AId: Integer; const AValue: TTime): Boolean;
begin
  Result := SaveDbSysDateTime(Db, AId, TimeOf(AValue));
end;

function SaveDbSysString(Db: TADOConnection; const AId: Integer; const AValue: string): Boolean;
const
  sqlNewIntValue  = 'INSERT INTO ss_settings (id, ' + fnVALUE_CHAR + ') VALUES (%d, ''%s'')';
  sqlSaveIntValue = 'UPDATE ss_settings SET ' + fnVALUE_CHAR + '=''%s'' WHERE id=%d';
var
  CurrSqlStr: string;
begin
  Result := False;
  try
    if CheckDbSys(Db, AId)
    then CurrSqlStr := Format(sqlSaveIntValue, [StrToSqlStr(AValue, 255), AId])
    else CurrSqlStr := Format(sqlNewIntValue, [AId, StrToSqlStr(AValue, 255)]);
    Result := ExecDynamicQuery(Db, CurrSqlStr);
  except
    on E: Exception do
      HandleSqlExcept(E, nil, CurrSqlStr, Format(SErrorSaveSettings, [AId]));
  end;
end;

function SaveDbSysText(Db: TADOConnection; const AId: Integer; const AValue: string): Boolean;
const
  sqlNewIntValue  = 'INSERT INTO ss_settings (id, ' + fnVALUE_CHAR + ') VALUES (%d, ''%s'')';
  sqlSaveIntValue = 'UPDATE ss_settings SET ' + fnVALUE_CHAR + '=''%s'' WHERE id=%d';
var
  CurrSqlStr: string;
begin
  Result := False;
  try
    if CheckDbSys(Db, AId)
    then CurrSqlStr := Format(sqlSaveIntValue, [TextToSqlStr(AValue, 255), AId])
    else CurrSqlStr := Format(sqlNewIntValue, [AId, TextToSqlStr(AValue, 255)]);
    Result := ExecDynamicQuery(Db, CurrSqlStr);
  except
    on E: Exception do
      HandleSqlExcept(E, nil, CurrSqlStr, Format(SErrorSaveSettings, [AId]));
  end;
end;

function SaveDbSysBoolean(Db: TADOConnection; const AId: Integer; const AValue: Boolean): Boolean;
begin
  if AValue
  then Result := SaveDbSysInteger(Db, AId, 1)
  else Result := SaveDbSysInteger(Db, AId, 0);
end;

(*$HINTS ON*)

end.
