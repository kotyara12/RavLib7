unit RDbVars;

interface

uses
  Classes, Db;

procedure AddRecordVariablesEx(Vars: TStringList; Ds: TDataSet;
  const ExcludeFields: string; const NamePrefix: string;
  const bMaskTagChar, bDisplayNames: Boolean; const TagsChar: Char);
procedure AddRecordDescripts(Vars: TStringList; Ds: TDataSet;
  const ExcludeFields: string; const NamePrefix: string = '');
procedure AddRecordVariables(Vars: TStringList; Ds: TDataSet;
  const ExcludeFields: string; const NamePrefix: string = '');
procedure AddFieldVariables(Vars: TStringList; Field: TField);

implementation

uses
  RVarListEx, RxStrUtils, SysUtils;

procedure AddRecordVariablesEx(Vars: TStringList; Ds: TDataSet;
  const ExcludeFields: string; const NamePrefix: string;
  const bMaskTagChar, bDisplayNames: Boolean; const TagsChar: Char);
var
  i, iCount: Integer;
  sValue: string;
begin
  if Assigned(Ds) and (bDisplayNames or (Ds.Active and not Ds.IsEmpty)) then
  begin
    iCount := Ds.FieldCount - 1;
    for i := 0 to iCount do
    begin
      if not (Ds.Fields[i].DataType in [ftBlob, ftGraphic,
        ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftArray, ftReference,
        ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface, ftIDispatch])
      and not IsWordPresent(Ds.Fields[i].FieldName, ExcludeFields, [';']) then
      begin
        sValue := EmptyStr;
        if bDisplayNames then
          sValue := Ds.Fields[i].DisplayName
        else begin
          if Ds.Fields[i].DataType in [ftMemo, ftFmtMemo]
          then sValue := Ds.Fields[i].AsString
          else sValue := Ds.Fields[i].DisplayText;
        end;

        if bMaskTagChar and (Pos(TagsChar, sValue) > 0) then
          sValue := StringReplace(sValue, TagsChar, TagsChar + TagsChar, [rfReplaceAll, rfIgnoreCase]);

        AddVariable(Vars, NamePrefix + Ds.Fields[i].FieldName, sValue);
      end;
    end;
  end;
end;

procedure AddRecordDescripts(Vars: TStringList; Ds: TDataSet; const ExcludeFields: string; const NamePrefix: string = '');
begin
  AddRecordVariablesEx(Vars, Ds, ExcludeFields, NamePrefix, False, True, TagsDefC);
end;

procedure AddRecordVariables(Vars: TStringList; Ds: TDataSet; const ExcludeFields: string; const NamePrefix: string = '');
begin
  AddRecordVariablesEx(Vars, Ds, ExcludeFields, NamePrefix, False, False, TagsDefC);
end;

procedure AddFieldVariables(Vars: TStringList; Field: TField);
begin
  if Assigned(Field) then
    AddVariableList(Vars, Field.AsString);
end;

end.
