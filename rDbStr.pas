unit RDbStr;

interface

uses
  Classes, Db, AdoDb, StdCtrls, DbCtrls;

type
  TObjectId = class
  private
    fId: Integer;
  public
    constructor Create(aId: Integer);
    property Id: Integer read fId write fId;
  end;

  TObjectId2 = class
  private
    fId1: Integer;
    fId2: Integer;
  public
    constructor Create(aId1, aId2: Integer);
    property Id1: Integer read fId1 write fId1;
    property Id2: Integer read fId2 write fId2;
  end;

  TObjectId3 = class
  private
    fId1: Integer;
    fId2: Integer;
    fId3: Integer;
  public
    constructor Create(aId1, aId2, aId3: Integer);
    property Id1: Integer read fId1 write fId1;
    property Id2: Integer read fId2 write fId2;
    property Id3: Integer read fId3 write fId3;
  end;

{ == Загрузка списка строк из базы данных с идентификаторами =================== }
{ == SQL: запрос для выборки }
{ == NameFieldIdx: индекс поля с текстом записи в наборе данных }
{ == IdFieldIdx: индекс поля с идентификатором записи в наборе данных (-1 = не используется) }
procedure LoadDbStrings(StrList: TStrings; Db: TAdoConnection; const SQL: string;
  const NameFieldIdx: Integer = 0; const IdFieldIdx: Integer = -1; const MaxStr: Integer = 0);
procedure LoadDbStrings2(StrList: TStrings; Db: TAdoConnection; const SQL: string;
  const NameFieldIdx: Integer; const Id1FieldIdx: Integer; const Id2FieldIdx: Integer);
procedure LoadDbStrings3(StrList: TStrings; Db: TAdoConnection; const SQL: string;
  const NameFieldIdx: Integer; const Id1FieldIdx: Integer; const Id2FieldIdx: Integer; const Id3FieldIdx: Integer);

procedure LoadPopularStrings(ComboBox: TComboBox; Db: TAdoConnection; const DsName, FlName: string); overload;
procedure LoadPopularStrings(ComboBox: TComboBox; Db: TAdoConnection; const DsName, FlName: string; const WhereSql: string); overload;

procedure LoadPopularStrings(DbComboBox: TDBComboBox); overload;
procedure LoadPopularStrings(DbComboBox: TDBComboBox; const DsName, FlName: string); overload;
procedure LoadPopularStrings(DbComboBox: TDBComboBox; const DsName, FlName: string; const WhereSql: string); overload;

implementation

uses
  RDialogs, RDbUtils, SysUtils;

const
  iLimitPopularStrings = 100;

procedure LoadDbStrings(StrList: TStrings; Db: TAdoConnection; const SQL: string;
  const NameFieldIdx: Integer = 0; const IdFieldIdx: Integer = -1; const MaxStr: Integer = 0);
var
  fDS: TAdoQuery;
begin
  StrList.BeginUpdate;
  try
    fDS := OpenDynamicQuery(Db, SQL);
    try
      StrList.Clear;
      if DataSetIsNotEmpty(fDS) then
      begin
        fDS.First;
        while not fDS.Eof do
        begin
          if (MaxStr > 0) and (StrList.Count >= MaxStr) then
            Break;
          if fDS.Fields[NameFieldIdx].DisplayText <> '' then
          begin
            if IdFieldIdx > -1
            then StrList.AddObject(fDS.Fields[NameFieldIdx].DisplayText,
              TObjectId.Create(IntFieldToIdx(fDS.Fields[IdFieldIdx])))
            else StrList.Add(fDS.Fields[NameFieldIdx].DisplayText);
          end;
          fDS.Next;
        end;
      end;
    finally
      FreeDynamicQuery(fDS);
    end;
  finally
    StrList.EndUpdate;
  end;
end;

procedure LoadDbStrings2(StrList: TStrings; Db: TAdoConnection; const SQL: string;
  const NameFieldIdx: Integer; const Id1FieldIdx: Integer; const Id2FieldIdx: Integer);
var
  fDS: TAdoQuery;
begin
  StrList.BeginUpdate;
  try
    fDS := OpenDynamicQuery(Db, SQL);
    try
      StrList.Clear;
      if DataSetIsNotEmpty(fDS) then
      begin
        fDS.First;
        while not fDS.Eof do
        begin
          if fDS.Fields[NameFieldIdx].DisplayText <> '' then
          begin
            StrList.AddObject(fDS.Fields[NameFieldIdx].DisplayText,
              TObjectId2.Create(IntFieldToIdx(fDS.Fields[Id1FieldIdx]),
                                IntFieldToIdx(fDS.Fields[Id2FieldIdx])));
          end;
          fDS.Next;
        end;
      end;
    finally
      FreeDynamicQuery(fDS);
    end;
  finally
    StrList.EndUpdate;
  end;
end;

procedure LoadDbStrings3(StrList: TStrings; Db: TAdoConnection; const SQL: string;
  const NameFieldIdx: Integer; const Id1FieldIdx: Integer; const Id2FieldIdx: Integer; const Id3FieldIdx: Integer);
var
  fDS: TAdoQuery;
begin
  StrList.BeginUpdate;
  try
    fDS := OpenDynamicQuery(Db, SQL);
    try
      StrList.Clear;
      if DataSetIsNotEmpty(fDS) then
      begin
        fDS.First;
        while not fDS.Eof do
        begin
          if fDS.Fields[NameFieldIdx].DisplayText <> '' then
          begin
            StrList.AddObject(fDS.Fields[NameFieldIdx].DisplayText,
              TObjectId3.Create(IntFieldToIdx(fDS.Fields[Id1FieldIdx]),
                                IntFieldToIdx(fDS.Fields[Id2FieldIdx]),
                                IntFieldToIdx(fDS.Fields[Id3FieldIdx])));
          end;
          fDS.Next;
        end;
      end;
    finally
      FreeDynamicQuery(fDS);
    end;
  finally
    StrList.EndUpdate;
  end;
end;

procedure LoadPopularStrings(ComboBox: TComboBox; Db: TAdoConnection; const DsName, FlName: string);
begin
  LoadDbStrings(ComboBox.Items, Db,
    Format('SELECT %0:s, COUNT(*) AS cnt FROM %1:s GROUP BY %0:s ORDER BY 2 desc, 1',
      [FlName, DsName]),
    0, -1, iLimitPopularStrings);
end;

procedure LoadPopularStrings(ComboBox: TComboBox; Db: TAdoConnection; const DsName, FlName: string; const WhereSql: string);
begin
  LoadDbStrings(ComboBox.Items, Db,
    Format('SELECT %0:s, COUNT(*) AS cnt FROM %1:s WHERE %2:s GROUP BY %0:s ORDER BY 2 desc, 1',
      [FlName, DsName, WhereSql]),
    0, -1, iLimitPopularStrings);
end;

procedure LoadPopularStrings(DbComboBox: TDBComboBox);
begin
  LoadDbStrings(DbComboBox.Items, TAdoDataSet(DbComboBox.Field.DataSet).Connection,
    Format('SELECT %0:s, COUNT(*) AS cnt FROM %1:s GROUP BY %0:s ORDER BY 2 desc, 1',
      [DbComboBox.Field.FieldName, DbComboBox.Field.DataSet.Name]),
    0, -1, iLimitPopularStrings);
end;

procedure LoadPopularStrings(DbComboBox: TDBComboBox; const DsName, FlName: string);
begin
  LoadDbStrings(DbComboBox.Items, TAdoDataSet(DbComboBox.Field.DataSet).Connection,
    Format('SELECT %0:s, COUNT(*) AS cnt FROM %1:s GROUP BY %0:s ORDER BY 2 desc, 1',
      [FlName, DsName]),
    0, -1, iLimitPopularStrings);
end;

procedure LoadPopularStrings(DbComboBox: TDBComboBox; const DsName, FlName: string; const WhereSql: string);
begin
  LoadDbStrings(DbComboBox.Items, TAdoDataSet(DbComboBox.Field.DataSet).Connection,
    Format('SELECT %0:s, COUNT(*) AS cnt FROM %1:s WHERE %2:s GROUP BY %0:s ORDER BY 2 desc, 1',
      [FlName, DsName, WhereSql]),
    0, -1, iLimitPopularStrings);
end;

{ TObjectId }

constructor TObjectId.Create(aId: Integer);
begin
  fId := aId;
end;

{ TObjectId2 }

constructor TObjectId2.Create(aId1, aId2: Integer);
begin
  fId1 := aId1;
  fId2 := aId2;
end;

{ TObjectId3 }

constructor TObjectId3.Create(aId1, aId2, aId3: Integer);
begin
  fId1 := aId1;
  fId2 := aId2;
  fId3 := aId3;
end;

end.
