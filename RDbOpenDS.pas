unit RDbOpenDS;

interface

uses
  SysUtils, Db, AdoDb, DbTables, IbDatabase, IbQuery, IbCustomDataSet,
  RDbFilter, RDbOrder, RDbConst;

type
  TOpenDataSetError = class (Exception);

// ��������� ����������� ����� ������ � ��������������� lookup-�����
function OpenDS_Static(Db: TCustomConnection; DataSet: TDataSet;
  const KeepPosition: Boolean = True; const KeyField: string = fnID;
  const Tag: Integer = 0): Boolean;
// ��������� ����������� ����� ������, �� �������� lookup-����
function OpenDS_StaticSimple(Db: TCustomConnection; DataSet: TDataSet;
  const KeepPosition: Boolean = True; const KeyField: string = fnID;
  const Tag: Integer = 0): Boolean;
// ��������� ����������� ������, �� �������� lookup-����
function OpenDS_StaticQuery(Db: TCustomConnection; DataSet: TDataSet;
  const SqlStatement: string; const KeepPosition: Boolean = True;
  const KeyField: string = fnID; const Tag: Integer = 0): Boolean;
// ��������� ���������� ������, �� �������� lookup-����
function OpenDS_VariableQuery(Db: TCustomConnection; DataSet: TDataSet;
  DynamicFilter: TRDbFilter; DynamicSort: TRDbOrder;
  const SelectSql, WhereSql, GroupSql, HavingSql: string;
  const KeepPosition: Boolean = True; const KeyField: string = fnID;
  const Tag: Integer = 0): Boolean;

implementation

uses
  Classes, RDbUtils, RVclUtils, RMsgRu, RExHandlers, RDialogs;

procedure ApplyDatabase(Db: TCustomConnection; DataSet: TDataSet);
begin
  // TCustomAdoDataSet
  if DataSet is TCustomAdoDataSet then
  begin
    if Db is TAdoConnection then
    begin
      TCustomAdoDataSet(DataSet).ConnectionString := EmptyStr;
      TCustomAdoDataSet(DataSet).Connection := TAdoConnection(Db);
    end
    else raise TOpenDataSetError.CreateFmt(SErrBadConnectionType, [Db.ClassName, DataSet.ClassName]);
  end;
  // TCustomIbDataSet
  if DataSet is TIbCustomDataSet then
  begin
    if Db is TIbDatabase then
    begin
      TIbCustomDataSet(DataSet).Database := TIbDatabase(Db);
    end
    else raise TOpenDataSetError.CreateFmt(SErrBadConnectionType, [Db.ClassName, DataSet.ClassName]);
  end;
end;

function OpenDS_StaticSimple(Db: TCustomConnection; DataSet: TDataSet;
  const KeepPosition: Boolean = True; const KeyField: string = fnID;
  const Tag: Integer = 0): Boolean;
var
  LId: Integer;
  Bmk: TBookmark;
begin
  Result := False;
  LId := intDisable;
  Bmk := nil;
  try
    if DataSet = nil then raise TOpenDataSetError.Create(SErrDataSetNull);
    Dataset.DisableControls;
    try
      // ��������� ������� �������
      if KeepPosition then
        PositionDS_Store(DataSet, KeyField, LId, Bmk);
      // ������������� ���������
      if not Assigned(TCustomAdoDataSet(DataSet).Connection) and Assigned(Db) then
        ApplyDatabase(Db, DataSet);
      if Tag > 0 then
        DataSet.Tag := Tag;
      // ���������� ������
      if DataSet.Active then
      begin
        if DataSet is TAdoQuery then
        begin
          try
            TAdoQuery(DataSet).Requery([]);
          except
            DataSet.Close;
            DataSet.Open;
          end;
        end
        else begin
          DataSet.Close;
          DataSet.Open;
        end;
      end
      // ��������� ����� ������
      else begin
        DataSet.Open;
      end;
      // ��������� ���������
      Result := DataSet.Active;
      // ���������� �������
      if DataSet.Active and not DataSet.IsEmpty then
        PositionDS_Restore(DataSet, KeyField, LId, Bmk);
    finally
      Dataset.EnableControls;
    end;
  except
    on E: Exception do
      HandleExcept(E, DataSet, Format(SErrOpenDataSet, [DataSet.Name]));
  end;
end;

function OpenDS_StaticLookup(Db: TCustomConnection; DataSet: TDataSet;
  const KeepPosition: Boolean = True; const KeyField: string = fnID): Boolean;
var
  i: Integer;
  LookupList: TList;
begin
  if Assigned(DataSet) then
  begin
    Result := True;
    LookupList := TList.Create;
    try
      // ������� ������ ��������� ������� ������
      for i := 0 to DataSet.FieldCount - 1 do
        if DataSet.Fields[i].Lookup then
        begin
          if (DataSet.Fields[i].LookupDataSet <> nil)
          and (LookupList.IndexOf(DataSet.Fields[i].LookupDataSet) < 0)
          then LookupList.Add(DataSet.Fields[i].LookupDataSet);
        end;
      // ��������� ��������� ������ ������
      for i := 0 to LookupList.Count - 1 do
      begin
        Result := OpenDS_Static(Db, TDataSet(LookupList.Items[i]), KeepPosition);
        if not Result then Break;
      end;
    finally
      LookupList.Free;
    end;
  end
  else Result := False;
end;

function OpenDS_Static(Db: TCustomConnection; DataSet: TDataSet;
  const KeepPosition: Boolean = True; const KeyField: string = fnID;
  const Tag: Integer = 0): Boolean;
begin
  if DataSet <> nil
  then Result := OpenDS_StaticLookup(Db, DataSet, KeepPosition, KeyField)
             and OpenDS_StaticSimple(Db, DataSet, KeepPosition, KeyField, Tag)
  else raise TOpenDataSetError.Create(SErrDataSetNull);
end;

function OpenDS_StaticQuery(Db: TCustomConnection; DataSet: TDataSet;
  const SqlStatement: string; const KeepPosition: Boolean = True;
  const KeyField: string = fnID; const Tag: Integer = 0): Boolean;
var
  LId: Integer;
  Bmk: TBookmark;
begin
  Result := False;
  LId := intDisable;
  Bmk := nil;
  try
    if DataSet = nil then raise TOpenDataSetError.Create(SErrDataSetNull);
    Dataset.DisableControls;
    try
      // ��������� ������� �������
      if KeepPosition then
        PositionDS_Store(DataSet, KeyField, LId, Bmk);
      // ��������� ����� ������
      if DataSet.Active then DataSet.Close;
      // ������������� ���������
      if not Assigned(TCustomAdoDataSet(DataSet).Connection) and Assigned(Db) then
        ApplyDatabase(Db, DataSet);
      if Tag > 0 then DataSet.Tag := Tag;
      // ������ ����� �������
      if DataSet is TAdoQuery then TAdoQuery(DataSet).SQL.Text := SqlStatement;
      if DataSet is TQuery then TQuery(DataSet).SQL.Text := SqlStatement;
      if DataSet is TIbQuery then TIbQuery(DataSet).SQL.Text := SqlStatement;
      // ��������� lookup-���� � ��������� ����� ������
      if OpenDS_StaticLookup(Db, DataSet, KeepPosition)
      then DataSet.Open;
      // ��������� ���������
      Result := DataSet.Active;
      // ���������� �������
      if DataSet.Active and not DataSet.IsEmpty then
        PositionDS_Restore(DataSet, KeyField, LId, Bmk);
    finally
      Dataset.EnableControls;
    end;
  except
    on E: Exception do
      HandleExcept(E, DataSet, Format(SErrOpenDataSet, [DataSet.Name]));
  end;
end;

function OpenDS_VariableQuery(Db: TCustomConnection; DataSet: TDataSet;
  DynamicFilter: TRDbFilter; DynamicSort: TRDbOrder;
  const SelectSql, WhereSql, GroupSql, HavingSql: string;
  const KeepPosition: Boolean = True; const KeyField: string = fnID;
  const Tag: Integer = 0): Boolean;
var
  SqlStatement, SqlTemp: string;
begin
  Result := False;
  if DataSet <> nil then
  begin
    try
      // ���������� ������� ����������
      if Assigned(DynamicFilter) and not DynamicFilter.Active then
      begin
        PauseWait;
        try
          DynamicFilter.Open;
        finally
          ContiniueWait;
        end;
      end;
      if Assigned(DynamicSort) and not DynamicSort.Active
      then DynamicSort.Open;
      // ���������� ����� �������
      SqlStatement := SelectSql;
      // ..where
      if Assigned(DynamicFilter)
      then SqlTemp := DynamicFilter.GetWhereString
      else SqlTemp := EmptyStr;
      if SqlTemp <> EmptyStr then
      begin
        if WhereSql <> EmptyStr
        then SqlStatement := SqlStatement + #13 +
          Format(sqlWhere, [Format(sqlAndFmt, [WhereSql, SqlTemp])])
        else SqlStatement := SqlStatement + #13 +
          Format(sqlWhere, [SqlTemp]);
      end
      else begin
        if WhereSql <> EmptyStr
        then SqlStatement := SqlStatement + #13 +
          Format(sqlWhere, [WhereSql]);
      end;
      // ..group by
      if GroupSql <> EmptyStr
      then SqlStatement := SqlStatement + #13 +
        Format(sqlGroupBy, [GroupSql]);
      // ..order by
      if Assigned(DynamicSort) then
      begin
        SqlTemp := DynamicSort.GetOrderString;
        if SqlTemp <> EmptyStr
        then SqlStatement := SqlStatement + #13 +
          Format(sqlOrder, [SqlTemp]);
      end;
      // ..having
      if HavingSql <> EmptyStr
      then SqlStatement := SqlStatement + #13 +
        Format(sqlHaving, [HavingSql]);
      // ��������� ������
      // InfoBox(SqlStatement);
      Result := OpenDS_StaticQuery(Db, DataSet, SqlStatement, KeepPosition, KeyField, Tag);
    except
      on E: Exception do
        HandleExcept(E, DataSet, Format(SErrOpenDataSet, [DataSet.Name]));
    end;
  end
  else raise TOpenDataSetError.Create(SErrDataSetNull);
end;

end.
