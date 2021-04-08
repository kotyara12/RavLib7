unit RADOQuery;

interface

uses
  SysUtils, Classes, DB, ADODB;

type
  TRefreshMode = (rmRequery, rmCloseOpen);

  TRADOQuery = class(TADOQuery)
  private
    FLookupAR: Boolean;
    FRefreshMode: TRefreshMode;
    FKeyField: TField;
    FLastValue: string;
    FLastPos: TBookmark;
    function  GetKeyField: string;
    procedure SetKeyField(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveLastPosition;
    procedure RestoreLastPosition;
    procedure FreeLastPosition;
    procedure RefreshQuery(const RestorePosition: Boolean = True);
    procedure RefreshRequrse(const RestorePosition: Boolean = True);
  published
    property LookupAutoRefresh: Boolean read FLookupAR write FLookupAR default True;
    property RefreshMode: TRefreshMode read FRefreshMode write FRefreshMode;
    property KeyField: string read GetKeyField write SetKeyField;
  end;

implementation

{ == TRADOQuery ================================================================ }
constructor TRADOQuery.Create(AOwner: TComponent);
begin
  inherited;
  Prepared := True;
  FLookupAR := True;
  FRefreshMode := rmRequery;
  FKeyField := nil;
  FLastValue := EmptyStr;
  FLastPos := nil;
end;

function TRADOQuery.GetKeyField: string;
begin
  if Assigned(FKeyField)
  then Result := FKeyField.FieldName
  else Result := EmptyStr;
end;

procedure TRADOQuery.SetKeyField(const Value: string);
begin
  if (Value <> GetKeyField) then
    FKeyField := FindField(Value);
end;

procedure TRADOQuery.SaveLastPosition;
begin
  FreeLastPosition;
  if Assigned(FKeyField)
  then FLastValue := FKeyField.AsString
  else FLastPos := GetBookmark;
end;

procedure TRADOQuery.RestoreLastPosition;
begin
  if Assigned(FKeyField) and (FLastValue <> EmptyStr) then
    Locate(FKeyField.FieldName, FLastValue, [])
  else if Assigned(FLastPos) then begin
         try
           GotoBookmark(FLastPos);
         except
         end;
       end;
end;

procedure TRADOQuery.FreeLastPosition;
begin
  FLastValue := EmptyStr;
  if Assigned(FLastPos) then FreeAndNil(FLastPos);
end;

procedure TRADOQuery.RefreshQuery(const RestorePosition: Boolean = True);
begin
  if RestorePosition and Active and not IsEmpty then SaveLastPosition;
  try
    if FRefreshMode = rmRequery then
      Requery
    else begin
      Close;
      Open;
    end;
  finally
    try
      RestoreLastPosition;
    finally
      FreeLastPosition;
    end;
  end;
end;

procedure TRADOQuery.RefreshRequrse(const RestorePosition: Boolean = True);
var
  i: Integer;
begin
  if FLookupAR then begin
    for i := 0 to FieldCount - 1 do
      if (Fields[i].FieldKind = fkLookup) and (Fields[i].LookupDataSet is TRADOQuery)
      then TRADOQuery(Fields[i].LookupDataSet).RefreshRequrse(False);
  end;
  RefreshQuery(RestorePosition);
end;

end.
