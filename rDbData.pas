{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Rav Soft Database Additional Components         }
{                                                       }
{       Copyright (c) 2006-2012 Razzhivin Alexandr      }
{                                                       }
{*******************************************************}

unit RDbData;

interface

uses
  RDbConst, Db;

type
  TFieldData = packed record
    Field: TField;
    FieldName: string;
    IsNull: Boolean;
    Data: Variant;
  end;

  TRecordData = ^RRecordData;
  RRecordData = packed record
    DataSet: TDataSet;
    FieldsData: array of TFieldData;
  end;

  TCopyBuffer     = array of TRecordData;

  TGetRecordTextOption  = (rtAll, rtRequired, rtVisible, rtHidden, rtIndex, rtID, rtZeroTag, rtAnyTag, rtTag255);
  TGetRecordTextOptions = set of TGetRecordTextOption;

const
  sAllFields      = '*';

{ == Генерация "мгновенного снимка" данных ===================================== }
function  GetRecordData(ADataSet: TDataSet): TRecordData;
{ == Добавить произвольные данные ============================================== }
procedure AddSimpleData(var ARecordData: TRecordData; const sName: string; vValue: Variant);
{ == Освобождение памяти ======================================================= }
procedure FreeRecordData(var ARecordData: TRecordData);
{ == Работа с буфером обмена =================================================== }
procedure PutInCopyBuffer(ADataSet: TDataSet; var ABuffer: TCopyBuffer);
procedure PasteCopyBufferC(ADataSet: TDataSet; const ABuffer: TCopyBuffer; const CopiedFields: string); overload;
procedure PasteCopyBufferL(ADataSet: TDataSet; const ABuffer: TCopyBuffer; const LockFields: string); overload;
procedure PasteCopyBufferC(ADataSet: TDataSet; const ABuffer: RRecordData; const CopiedFields: string); overload;
procedure PasteCopyBufferL(ADataSet: TDataSet; const ABuffer: RRecordData; const LockFields: string); overload;
procedure PutDataSetInCopyBuffer(ADataSet: TDataSet; var ABuffer: TCopyBuffer);
procedure FreeCopyBuffer(var ABuffer: TCopyBuffer);
{ == Выборка данных ============================================================ }
function  GetFieldCount(const ARecordData: TRecordData): Integer;
function  GetFieldData(const ARecordData: TRecordData; const Index: Integer): TFieldData;
function  GetFieldValue(const ARecordData: TRecordData; const Index: Integer): Variant;
function  GetFieldValStr(const ARecordData: TRecordData; const Index: Integer): string;
function  GetFieldLogStr(const ARecordData: TRecordData; const Index: Integer): string;
function  GetFieldDataByName(const ARecordData: TRecordData; const Name: string): TFieldData;
function  GetFieldValueByName(const ARecordData: TRecordData; const Name: string): Variant;
function  GetFieldValueByName_Integer(const ARecordData: TRecordData; const Name: string): Integer;
function  GetFieldValueByName_IntNull(const ARecordData: TRecordData; const Name: string): string;
function  GetFieldValueByName_Float(const ARecordData: TRecordData; const Name: string): Extended;
function  GetFieldValueByName_DateTime(const ARecordData: TRecordData; const Name: string): TDateTime;
function  GetFieldValueByName_Boolean(const ARecordData: TRecordData; const Name: string): Boolean;
function  GetFieldValueByName_String(const ARecordData: TRecordData; const Name: string; const NullValue: string = ''): string;
procedure PutFieldValue(Field: TField; const ARecordData: TRecordData);
procedure PutFieldValueCustom(Field: TField; const ARecordData: TRecordData; const ARecordName: string);

{ == Генерация строки с данными на основе "снимка" ============================= }
function  GetRecordString(const ARecordData: TRecordData): string;
function  GetRecordText(const ARecordData: TRecordData; const AOptions: TGetRecordTextOptions): string;
function  GetRecordChanges(const AData1, AData2: TRecordData; const AOptions: TGetRecordTextOptions): string;
function  GetFieldsChanges(const AData1, AData2: TRecordData; const Fields: string): string;
function  HasFieldsChanges(const AData1, AData2: TRecordData; const Fields: string): Boolean;

implementation

uses
  SysUtils, Variants, RStrUtils, RxStrUtils, RVclUtils, RCrc32;

const
  DivChars = [#13,';',','];

{ == Заполнение RRecordData данными ============================================ }
procedure FillRecordData(var ARecordData: RRecordData; ADataSet: TDataSet);
var
  i, iCount: Integer;
begin
  if Assigned(ADataSet) then
  begin
    with ARecordData do
    begin
      DataSet := ADataSet;
      SetLength(FieldsData, ADataSet.FieldCount);
      iCount := ADataSet.FieldCount - 1;
      for i := 0 to iCount do
      begin
        FieldsData[i].Field := ADataSet.Fields[i];
        FieldsData[i].FieldName := ADataSet.Fields[i].FieldName;
        FieldsData[i].IsNull := ADataSet.Fields[i].IsNull;
        FieldsData[i].Data := ADataSet.Fields[i].AsVariant;
      end;
    end;
  end
  else begin
    ARecordData.DataSet := nil;
    SetLength(ARecordData.FieldsData, 0);
  end;
end;

{ == Генерация "мгновенного снимка" данных ===================================== }
function GetRecordData(ADataSet: TDataSet): TRecordData;
begin
  New(Result);
  if Assigned(ADataSet) then
    FillRecordData(Result^, ADataSet);
end;

{ == Добавить произвольные данные ============================================== }
procedure AddSimpleData(var ARecordData: TRecordData; const sName: string; vValue: Variant);
var
  i: Integer;
begin
  if Assigned(ARecordData) then
  begin
    for i := Low(ARecordData^.FieldsData) to High(ARecordData^.FieldsData) do
    begin
      if SameText(sName, ARecordData^.FieldsData[i].FieldName) then
      begin
        ARecordData^.FieldsData[i].Data := vValue;
        ARecordData^.FieldsData[i].IsNull := VarIsNull(vValue) or VarIsEmpty(vValue);
        Exit;
      end;
    end;

    SetLength(ARecordData^.FieldsData, Length(ARecordData^.FieldsData) + 1);
    with ARecordData^.FieldsData[High(ARecordData^.FieldsData)] do
    begin
      Field := nil;
      FieldName := sName;
      Data := vValue;
      IsNull := VarIsNull(vValue) or VarIsEmpty(vValue);
    end;
  end;
end;

{ == Освобождение памяти ======================================================= }
procedure FreeRecordData(var ARecordData: TRecordData);
begin
  if Assigned(ARecordData) then
  begin
    ARecordData^.DataSet := nil;
    SetLength(ARecordData^.FieldsData, 0);
    Dispose(ARecordData);
    ARecordData := nil;
  end;
end;

{ == Работа с буфером обмена =================================================== }
procedure PutInCopyBuffer(ADataSet: TDataSet; var ABuffer: TCopyBuffer);
begin
  SetLength(ABuffer, Length(ABuffer) + 1);
  ABuffer[High(ABuffer)] := GetRecordData(ADataSet);
end;

procedure PasteCopyBufferC(ADataSet: TDataSet; const ABuffer: RRecordData; const CopiedFields: string);

  function IsCopyField(const FieldName: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to WordCount(CopiedFields, DivChars) do
      if SameText(FieldName, Trim(ExtractWord(i, CopiedFields, DivChars))) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  j: Integer;
begin
  for j := Low(ABuffer.FieldsData) to High(ABuffer.FieldsData) do
    with ABuffer.FieldsData[j] do
      if (Field.FieldKind = fkData) and IsCopyField(FieldName) then
      begin
        if IsNull
        then Field.Clear
        else Field.Value := Data;
      end;
end;

procedure PasteCopyBufferC(ADataSet: TDataSet; const ABuffer: TCopyBuffer; const CopiedFields: string);

  function IsCopyField(const FieldName: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 1 to WordCount(CopiedFields, DivChars) do
      if SameText(FieldName, Trim(ExtractWord(i, CopiedFields, DivChars))) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  i, j: Integer;
begin
  for i := Low(ABuffer) to High(ABuffer) do
    if ABuffer[i].DataSet = ADataSet then
      for j := Low(ABuffer[i].FieldsData) to High(ABuffer[i].FieldsData) do
        with ABuffer[i].FieldsData[j] do
          if (Field.FieldKind = fkData) and IsCopyField(FieldName) then
          begin
            if IsNull
            then Field.Clear
            else Field.Value := Data;
          end;
end;

procedure PasteCopyBufferL(ADataSet: TDataSet; const ABuffer: RRecordData; const LockFields: string);

  function IsCopyField(const FieldName: string): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 1 to WordCount(LockFields, DivChars) do
      if SameText(FieldName, Trim(ExtractWord(i, LockFields, DivChars))) then
      begin
        Result := False;
        Break;
      end;
  end;

var
  j: Integer;
begin
  for j := Low(ABuffer.FieldsData) to High(ABuffer.FieldsData) do
    with ABuffer.FieldsData[j] do
      if (Field.FieldKind = fkData) and IsCopyField(FieldName) then
      begin
        if IsNull
        then Field.Clear
        else Field.Value := Data;
      end;
end;

procedure PasteCopyBufferL(ADataSet: TDataSet; const ABuffer: TCopyBuffer; const LockFields: string);

  function IsCopyField(const FieldName: string): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    for i := 1 to WordCount(LockFields, DivChars) do
      if SameText(FieldName, Trim(ExtractWord(i, LockFields, DivChars))) then
      begin
        Result := False;
        Break;
      end;
  end;

var
  i, j: Integer;
begin
  for i := Low(ABuffer) to High(ABuffer) do
    if ABuffer[i].DataSet = ADataSet then
      for j := Low(ABuffer[i].FieldsData) to High(ABuffer[i].FieldsData) do
        with ABuffer[i].FieldsData[j] do
          if (Field.FieldKind = fkData) and IsCopyField(FieldName) then
          begin
            if IsNull
            then Field.Clear
            else Field.Value := Data;
          end;
end;

procedure PutDataSetInCopyBuffer(ADataSet: TDataSet; var ABuffer: TCopyBuffer);
begin
  ADataSet.DisableControls;
  try
    ADataSet.First;
    while not ADataSet.Eof do
    begin
      PutInCopyBuffer(ADataSet, ABuffer);
      ADataSet.Next;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;

procedure FreeCopyBuffer(var ABuffer: TCopyBuffer);
begin
  while Length(ABuffer) > 0 do
  begin
    FreeRecordData(ABuffer[High(ABuffer)]);
    SetLength(ABuffer, Length(ABuffer) - 1);
  end;
end;

{ == Выборка данных ============================================================ }
function GetFieldCount(const ARecordData: TRecordData): Integer;
begin
  if (ARecordData <> nil)
  then Result := Length(ARecordData.FieldsData)
  else Result := 0;
end;

function GetFieldData(const ARecordData: TRecordData; const Index: Integer): TFieldData;
begin
  if (ARecordData <> nil)
  and (Index in [Low(ARecordData.FieldsData)..High(ARecordData.FieldsData)]) then
    Result := ARecordData.FieldsData[Index]
  else begin
    Result.Field := nil;
    Result.FieldName := EmptyStr;
    Result.IsNull := True;
    Result.Data := VarNull;
  end;
end;

function GetFieldValue(const ARecordData: TRecordData; const Index: Integer): Variant;
begin
  Result := GetFieldData(ARecordData, Index).Data;
end;

function GetFieldValStr(const ARecordData: TRecordData; const Index: Integer): string;
var
  sValue: string;
begin
  if (ARecordData <> nil)
  and (Index in [Low(ARecordData.FieldsData)..High(ARecordData.FieldsData)]) then
  begin
    if ARecordData.FieldsData[Index].IsNull
    then Result := SNullText
    else begin
      sValue := string(ARecordData.FieldsData[Index].Data);
      if ARecordData.FieldsData[Index].Field is TBlobField
      then Result := Format(SBlobTextCrc32, [StrCrc32(sValue)])
      else Result := sValue;
    end;
  end
  else Result := EmptyStr;
end;

function GetFieldLogStr(const ARecordData: TRecordData; const Index: Integer): string;
var
  sValue: string;
begin
  if (ARecordData <> nil)
  and (Index in [Low(ARecordData.FieldsData)..High(ARecordData.FieldsData)]) then
  begin
    if ARecordData.FieldsData[Index].IsNull
    then Result := Format(SFldNameText, [ARecordData.FieldsData[Index].FieldName, SNullText])
    else begin
      sValue := string(ARecordData.FieldsData[Index].Data);
      if ARecordData.FieldsData[Index].Field is TBlobField
      then Result := Format(SFldNameText, [ARecordData.FieldsData[Index].FieldName, Format(SBlobTextCrc32, [StrCrc32(sValue)])])
      else Result := Format(SFldNameText, [ARecordData.FieldsData[Index].FieldName, sValue]);
    end;
  end
  else Result := EmptyStr;
end;

function GetFieldDataByName(const ARecordData: TRecordData; const Name: string): TFieldData;
var
  i: Integer;
begin
  Result.Field := nil;
  Result.FieldName := EmptyStr;
  Result.IsNull := True;
  Result.Data := VarNull;
  if (ARecordData <> nil) then
  begin
    for i := Low(ARecordData.FieldsData) to High(ARecordData.FieldsData) do
    begin
      if SameText(ARecordData.FieldsData[i].FieldName, Name) then
      begin
        Result := ARecordData.FieldsData[i];
        Break;
      end;
    end;
  end;
end;

function GetFieldValueByName(const ARecordData: TRecordData; const Name: string): Variant;
var
  FldData: TFieldData;
begin
  FldData := GetFieldDataByName(ARecordData, Name);
  if FldData.IsNull
  then Result := VarNull
  else Result := FldData.Data;
end;

function GetFieldValueByName_Integer(const ARecordData: TRecordData; const Name: string): Integer;
var
  FldData: TFieldData;
begin
  FldData := GetFieldDataByName(ARecordData, Name);
  if FldData.IsNull
  then Result := -1
  else Result := Integer(FldData.Data);
end;

function GetFieldValueByName_IntNull(const ARecordData: TRecordData; const Name: string): string;
var
  FldData: TFieldData;
begin
  FldData := GetFieldDataByName(ARecordData, Name);
  if FldData.IsNull
  then Result := sqlNull
  else Result := IntToStr(Integer(FldData.Data));
end;

function GetFieldValueByName_Float(const ARecordData: TRecordData; const Name: string): Extended;
var
  FldData: TFieldData;
begin
  FldData := GetFieldDataByName(ARecordData, Name);
  if FldData.IsNull
  then Result := 0
  else Result := Extended(FldData.Data);
end;

function GetFieldValueByName_DateTime(const ARecordData: TRecordData; const Name: string): TDateTime;
var
  FldData: TFieldData;
begin
  FldData := GetFieldDataByName(ARecordData, Name);
  if FldData.IsNull
  then Result := -1
  else Result := TDateTime(FldData.Data);
end;

function GetFieldValueByName_Boolean(const ARecordData: TRecordData; const Name: string): Boolean;
var
  FldData: TFieldData;
begin
  FldData := GetFieldDataByName(ARecordData, Name);
  if FldData.IsNull
  then Result := False
  else Result := Boolean(FldData.Data);
end;

function GetFieldValueByName_String(const ARecordData: TRecordData; const Name: string; const NullValue: string = ''): string;
var
  FldData: TFieldData;
begin
  FldData := GetFieldDataByName(ARecordData, Name);
  if FldData.IsNull
  then Result := NullValue
  else Result := string(FldData.Data);
end;

procedure PutFieldValue(Field: TField; const ARecordData: TRecordData);
var
  FldData: TFieldData;
begin
  if Assigned(Field) then
  begin
    FldData := GetFieldDataByName(ARecordData, Field.FieldName);
    if FldData.IsNull
    then Field.Clear
    else Field.Value := FldData.Data;
  end;
end;

procedure PutFieldValueCustom(Field: TField; const ARecordData: TRecordData; const ARecordName: string);
var
  FldData: TFieldData;
begin
  FldData := GetFieldDataByName(ARecordData, ARecordName);
  if FldData.IsNull
  then Field.Clear
  else Field.Value := FldData.Data;
end;

{ == Генерация строки с данными на основе "снимка" ============================= }
function GetRecordString(const ARecordData: TRecordData): string;
var
  i: Integer;
begin
  if (ARecordData <> nil) and (Length(ARecordData.FieldsData) > 0) then
  begin
    Result := EmptyStr;
    for i := Low(ARecordData.FieldsData) to High(ARecordData.FieldsData) do
      Result := AddDelimStr(Result, GetFieldLogStr(ARecordData, i));
  end
  else Result := EmptyStr;
end;

function GetRecordText(const ARecordData: TRecordData; const AOptions: TGetRecordTextOptions): string;
var
  i: Integer;
begin
  if (ARecordData <> nil) and Assigned(ARecordData.DataSet)
  and (Length(ARecordData.FieldsData) > 0) then
  begin
    Result := EmptyStr;
    for i := Low(ARecordData.FieldsData) to High(ARecordData.FieldsData) do
    begin
      if Assigned(ARecordData.FieldsData[i].Field) and not ARecordData.FieldsData[i].Field.Lookup then
      begin
        if (rtAll in AOptions)
        or ((rtRequired in AOptions) and ARecordData.FieldsData[i].Field.Required)
        or ((rtVisible in AOptions) and ARecordData.FieldsData[i].Field.Visible)
        or ((rtHidden in AOptions) and not ARecordData.FieldsData[i].Field.Visible)
        or ((rtIndex in AOptions) and ARecordData.FieldsData[i].Field.IsIndexField)
        or ((rtID in AOptions) and SameText(ARecordData.FieldsData[i].Field.FieldName, fnID))
        or ((rtZeroTag in AOptions) and (ARecordData.FieldsData[i].Field.Tag = 0))
        or ((rtTag255 in AOptions) and (ARecordData.FieldsData[i].Field.Tag = 255))
        or ((rtAnyTag in AOptions) and (ARecordData.FieldsData[i].Field.Tag <> 0)) then
        begin
          Result := AddDelimStr(Result, GetFieldLogStr(ARecordData, i));
        end;
      end;
    end;
  end
  else Result := EmptyStr;
end;

function GetRecordChanges(const AData1, AData2: TRecordData; const AOptions: TGetRecordTextOptions): string;
var
  i: Integer;
begin
  Result := EmptyStr;
  if Assigned(AData1) and Assigned(AData1.DataSet) and (Length(AData1.FieldsData) > 0)
  and Assigned(AData2) and Assigned(AData2.DataSet) and (Length(AData2.FieldsData) > 0)
  and (Length(AData1.FieldsData) = Length(AData2.FieldsData)) then
  begin
    for i := Low(AData1.FieldsData) to High(AData1.FieldsData) do
    begin
      if Assigned(AData1.FieldsData[i].Field)
      and not AData1.FieldsData[i].Field.Lookup
      and not AData1.FieldsData[i].Field.Calculated then
      begin
        if (rtAll in AOptions)
        or ((rtRequired in AOptions) and AData1.FieldsData[i].Field.Required)
        or ((rtVisible in AOptions) and AData1.FieldsData[i].Field.Visible)
        or ((rtHidden in AOptions) and not AData1.FieldsData[i].Field.Visible)
        or ((rtIndex in AOptions) and AData1.FieldsData[i].Field.IsIndexField)
        or ((rtID in AOptions) and SameText(AData1.FieldsData[i].Field.FieldName, fnID))
        or ((rtZeroTag in AOptions) and (AData1.FieldsData[i].Field.Tag = 0))
        or ((rtTag255 in AOptions) and (AData1.FieldsData[i].Field.Tag = 255))
        or ((rtAnyTag in AOptions) and (AData1.FieldsData[i].Field.Tag <> 0))
        // or (AData1.FieldsData[i].Data <> AData2.FieldsData[i].Data)
        or not SameText(GetFieldValStr(AData1, i), GetFieldValStr(AData2, i)) then
        begin
          Result := AddDelimStr(Result, GetFieldLogStr(AData1, i));
        end;
      end;
    end;
  end;
end;

function GetFieldsChanges(const AData1, AData2: TRecordData; const Fields: string): string;
var
  i, j: Integer;
  FldName: string;
begin
  Result := EmptyStr;
  if Assigned(AData1) and Assigned(AData1.DataSet) and (Length(AData1.FieldsData) > 0)
  and Assigned(AData2) and Assigned(AData2.DataSet) and (Length(AData2.FieldsData) > 0)
  and (Length(AData1.FieldsData) = Length(AData2.FieldsData)) then
  begin
    for j := 1 to WordCount(Fields, DivChars) do
    begin
      FldName := ExtractWord(j, Fields, DivChars);
      for i := Low(AData1.FieldsData) to High(AData1.FieldsData) do
      begin
        if (SameText(AData1.FieldsData[i].FieldName, FldName) or SameText(sAllFields, FldName))
        and Assigned(AData1.FieldsData[i].Field)
        and not AData1.FieldsData[i].Field.Lookup
        and not AData1.FieldsData[i].Field.Calculated
        // and (AData1.FieldsData[i].Data <> AData2.FieldsData[i].Data) then
        and not SameText(GetFieldValStr(AData1, i), GetFieldValStr(AData2, i)) then
        begin
          Result := AddDelimStr(Result, GetFieldLogStr(AData1, i));
        end;
      end;
    end;
  end;
end;

function HasFieldsChanges(const AData1, AData2: TRecordData; const Fields: string): Boolean;
var
  i, j: Integer;
  FldName: string;
begin
  Result := False;
  if Assigned(AData1) and Assigned(AData1.DataSet) and (Length(AData1.FieldsData) > 0)
  and Assigned(AData2) and Assigned(AData2.DataSet) and (Length(AData2.FieldsData) > 0)
  and (Length(AData1.FieldsData) = Length(AData2.FieldsData)) then
  begin
    for j := 1 to WordCount(Fields, DivChars) do
    begin
      FldName := ExtractWord(j, Fields, DivChars);
      for i := Low(AData1.FieldsData) to High(AData1.FieldsData) do
      begin
        Result := (SameText(AData1.FieldsData[i].FieldName, FldName) or SameText(sAllFields, FldName))
          and Assigned(AData1.FieldsData[i].Field)
          and not AData1.FieldsData[i].Field.Lookup
          and not AData1.FieldsData[i].Field.Calculated
          // and (AData1.FieldsData[i].Data <> AData2.FieldsData[i].Data);
          and not SameText(GetFieldValStr(AData1, i), GetFieldValStr(AData2, i));
        if Result then Break;
      end;
      if Result then Break;
    end;
  end;
end;

end.
