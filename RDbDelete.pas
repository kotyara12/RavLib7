unit RDbDelete;

interface

uses
  RDbEditor, Db, AdoDb, Windows;

function GetRecordCount(Db: TADOConnection; const TableName: string;
  sqlWhere: string): Integer;
function DbDeleteLogged(Db: TADOConnection; const TableName, TableDescr: string;
  const sqlWhere: string; const OperTag: Integer; const QueryText: string = ''): Boolean; overload;
function DbDeleteLogged(Editor: TRDbCustomEditor; const OwnerId: Integer; const QueryText: string = ''): Boolean; overload;

implementation

uses
  {$IFDEF RSS} RDbLog, {$ENDIF}
  SysUtils, RExHandlers, RMsgRu, RDbConst, RDbUtils, RDialogs;

resourcestring
  SLogDeleteTemplate = '"%0:s" [%1:s]: Из таблицы "%1:s" удалено %2:d запись(и,ей). {sql: %3:s}';

function GetRecordCount(Db: TADOConnection; const TableName: string; sqlWhere: string): Integer;
const
  sqlGetRecordCount = 'select count(*) from %s where %s';
var
  qryGetRecordCount: TAdoQuery;
begin
  Result := 0;
  try
    qryGetRecordCount := OpenDynamicQuery(Db, Format(sqlGetRecordCount, [TableName, sqlWhere]));
    try
      if DataSetIsNotEmpty(qryGetRecordCount) then
        Result := qryGetRecordCount.Fields[0].AsInteger;
    finally
      FreeDynamicQuery(qryGetRecordCount);
    end;
  except
    on E: Exception do
      HandleExcept(E, nil, Format(SErrGetRecordCount, [TableName]));
  end;
end;

function DbDeleteLogged(Db: TADOConnection; const TableName, TableDescr: string;
  const sqlWhere: string; const OperTag: Integer; const QueryText: string = ''): Boolean;
const
  sqlDeleteData = 'delete from %s where %s';
var
  iRecCount: Integer;
begin
  Result := False;
  iRecCount := GetRecordCount(Db, TableName, sqlWhere);
  if (iRecCount > 0)
  and ((QueryText = '') or (QueryBoxStdNY(QueryText) = ID_YES)) then
  begin
    try
      Result := ExecDynamicQuery(Db, Format(sqlDeleteData, [TableName, sqlWhere]));
      {$IFDEF RSS}
      if Result and (OperTag > 0) then
        AddToDbLog(OperTag, Format(SLogDeleteTemplate, [TableDescr, TableName, iRecCount, Format(sqlDeleteData, [TableName, sqlWhere])]));
      {$ENDIF}
    except
      on E: Exception do
        HandleExcept(E, nil, Format(SErrDeleteError, [TableName]));
    end;
  end
  else Result := True;
end;

function DbDeleteLogged(Editor: TRDbCustomEditor; const OwnerId: Integer; const QueryText: string = ''): Boolean;
begin
  Result := DbDeleteLogged(TAdoDataSet(Editor.DataSet).Connection,
    Editor.GetObjectName(etDelete),
    Editor.GetObjectDesc(etDelete),
    Format(sqlCondNumber, [Editor.OwnerFieldName, OwnerId]),
    Editor.GetEditTag(etDelete),
    QueryText);
end;

end.
