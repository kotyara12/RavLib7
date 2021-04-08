unit RDbState;

interface

uses
  Db;

function GetDataSetStateText(const DSState: TDataSetState): string; overload;
function GetDataSetStateText(DS: TDataSet): string; overload;
function GetEditorCaption(const DSName: string; const DSState: TDataSetState): string; overload;
function GetEditorCaption(const DSName: string; DS: TDataSet): string; overload;

implementation

uses
  SysUtils, RMsgRu;

const
  SDataSetStates: array [TDataSetState] of string = (SDataSetInactive,
    SDataSetBrowse, SDataSetEdit, SDataSetInsert,
    SDataSetSetKey, SDataSetCalcFields, SDataSetFilter,
    SDataSetNewValue, SDataSetOldValue, SDataSetCurValue,
    SDataSetBlockRead, SDataSetInternalCalc, SDataSetOpening);

function GetDataSetStateText(const DSState: TDataSetState): string;
begin
  Result := SDataSetStates[DSState];
end;

function GetDataSetStateText(DS: TDataSet): string;
begin
  if DS <> nil
  then Result := SDataSetStates[DS.State]
  else Result := SDataSetNil;
end;

function GetEditorCaption(const DSName: string; const DSState: TDataSetState): string;
begin
  if DSName <> EmptyStr
  then Result := Format(SFmtEditorCaption, [DSName, AnsiLowerCase(GetDataSetStateText(DSState))])
  else Result := GetDataSetStateText(DSState);
end;

function GetEditorCaption(const DSName: string; DS: TDataSet): string;
begin
  if DSName <> EmptyStr
  then Result := Format(SFmtEditorCaption, [DSName, AnsiLowerCase(GetDataSetStateText(DS))])
  else Result := GetDataSetStateText(DS);
end;

end.
