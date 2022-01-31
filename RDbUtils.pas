unit RDbUtils;

interface

uses
  Classes, Controls, StdCtrls, DbCtrls, AdoDb, Db;

{ == Работа со строками ======================================================== }
function  IsNullField(Field: TField): string;
function  IsEmptyField(Field: TField): string;
function  IsEmptyString(const S: string): string;
function  IsMemoField(Field: TField): Boolean;
function  IsStringField(Field: TField): Boolean;
function  IsNumericField(Field: TField): Boolean;
function  GetFieldString(Field: TField; const chQuote: Char): string;
function  GetFieldText(Field: TField; const chQuote: Char): string;
function  StrToSqlStr(const Str: string; const MaxLen: Integer): string; overload;
function  StrToSqlStr(const Str: string; const chQuote: Char; const bNullIsEmpty: Boolean): string; overload;
function  TextToSqlStr(const Str: string; const MaxLen: Integer): string;
function  StrToLikeStr(const Str: string): string;
function  BoolToSqlStr(const Value: Boolean): string;
function  IntToSqlStr(const Value: Integer): string;
function  FloatToSqlStr(const Value: Double; const ZeroNull: Boolean = False): string;
function  DateToSqlStr(const DateFmt: string; const DateVal: TDateTime): string;
function  DateTimeToSqlStr(const DateFmt: string; const DateVal: TDateTime): string;
function  IntFieldToIdx(const Field: TField): Integer;
procedure SetFieldIntIdx(Field: TField; const aValue: Integer);
procedure SetFieldFloat(Field: TField; const aValue: Extended);
procedure SetFieldDate(Field: TField; const aValue: TDateTime);
procedure SetFieldString(Field: TField; const aValue: string);
{ == Переход на нужную запись ================================================== }
function  DataSetLocateEx(DataSet: TDataSet;
  const KeyField: string; const KeyId: Integer; const RaiseError: Boolean): Boolean;
function  DataSetLocateId(DataSet: TDataSet;
  const KeyId: Integer; const RaiseError: Boolean): Boolean;
{ == Запоминаем и восстанавливаем позицию в наборе данных ====================== }
procedure PositionDS_Store(DataSet: TDataSet; const KeyField: string;
  var KeyId: Integer; var KeyBk: TBookmark);
procedure PositionDS_Restore(DataSet: TDataSet; const KeyField: string;
  const KeyId: Integer; var KeyBk: TBookmark);
{ == Работа с динамически создаваемыми запросами =============================== }
function  OpenDynamicQuery(AConn: TADOConnection; const ASQL: string; const CmdTimeout: Integer = 30; const sName: string = ''): TADOQuery;
function  ExecDynamicQuery(AConn: TADOConnection; const ASQL: string; const CmdTimeout: Integer = 30; const sName: string = ''): Boolean;
procedure FreeDynamicQuery(AQuery: TADOQuery);
{ == Проверка статуса набора данных ============================================ }
function  DataSetIsOpen(ADataSet: TDataSet): Boolean;
function  DataSetIsNotEmpty(ADataSet: TDataSet): Boolean;
function  DynamicQueryIsNotEmpty(AQuery: TAdoQuery): Boolean;
{ == "Безопасное" обновление запроса =========================================== }
procedure SafeRequery(ADataSet: TCustomADODataSet);
{ == Отмена режима редактирования ============================================== }
procedure CancelEditDb(DataSet: TDataSet);
{ == Устанока признака ReadOnly для Db-контролов =============================== }
procedure FillDbReadOnly(WinControl: TWinControl);
{ == Установка Label жирным шрифтом для required полей ========================= }
procedure FillDbRequired(WinControl: TWinControl);
function  CheckDbRequired(WinControl: TWinControl): Boolean;
{ == Установка Hint-ов для контролов, имеющих поле 'DataField' ================= }
procedure FillDbHints(WinControl: TWinControl);
{ == Поверка наличия свойства SQL (TQuery, TAdoQuery...) ======================= }
function  IsSqlDataSet(DataSet: TDataSet; var SqlText: string): Boolean;
{ == Применение фильтра по любым полям ========================================= }
function  GetDbFilterAllFields(DataSet: TDataSet; const FindStr: string; const UseLeadAsterisk, UseLookupTables: Boolean; const FieldsList: string = ''): string;
procedure DbFilterAllFields(DataSet: TDataSet; const FindStr: string; const UseLeadAsterisk, UseLookupTables: Boolean; const FieldsList: string = '');
{ == Создаем список ID по условию ============================================== }
function  GetIdList(DataSet: TDataSet; const KeyField, Filter: string): string;
function  GetFilterOnIdList(const FieldName, IdList: string): string;
{ == Копирование данных из одного поля в другое ================================ }
procedure CopyField(flFrom, flTo: TField);
procedure CopyFieldByName(dsFrom, dsTo: TDataSet; const sFieldName: string);
{ == Установка форматов ======================================================== }
procedure SetFieldFormats(DS: TDataSet; const sFieldName, aDisplayFormat, aEditFormat: string);
procedure SetFieldDisplayFormat(DS: TDataSet; const sFieldName, aDisplayFormat: string);
procedure SetFieldEditFormat(DS: TDataSet; const sFieldName, aEditFormat: string);


implementation

uses
  SysUtils, StrUtils, Variants, ActiveX, TypInfo, Menus, RxStrUtils,
  Graphics, RStrUtils, RDbConst, RDialogs, RMsgRu;

const
  PropField       = 'DataField';
  PropDataSource  = 'DataSource';
  PropListSource  = 'ListSource';
  PropKeyField    = 'KeyField';
  PropListField   = 'ListField';
  PropListIndex   = 'ListFieldIndex';
  PropNullKey     = 'NullValueKey';
  PropReadOnly    = 'ReadOnly';
  PropMaxLength   = 'MaxLength';
  PropParentColor = 'ParentColor';
  PropColor       = 'Color';
  PropSql         = 'SQL';
  SqlTimeFormat   = 'hh:nn:ss';

resourcestring
  SNullValueHint  = '%s (удалить значение: %s)';

{ == Работа со строками ======================================================== }
function IsNullField(Field: TField): string;
begin
  if Assigned(Field) and not Field.IsNull then
    Result := Field.AsString
  else
    Result := SNullText;
end;

function IsEmptyField(Field: TField): string;
begin
  if Assigned(Field) and not Field.IsNull and (Field.AsString <> '') then
    Result := Field.AsString
  else
    Result := SNullText;
end;

function IsEmptyString(const S: string): string;
begin
  if S <> '' then
    Result := S
  else
    Result := SNullText;
end;

function IsMemoField(Field: TField): Boolean;
begin
  Result := Assigned(Field)
       and (Field.DataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftOraBlob, ftOraClob]);
end;

function IsStringField(Field: TField): Boolean;
begin
  Result := Assigned(Field)
       and (Field.DataType in [ftString, ftFixedChar, ftWideString, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftOraBlob, ftOraClob]);
end;

function IsNumericField(Field: TField): Boolean;
begin
  Result := Assigned(Field)
       and (Field.DataType in [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency, ftBCD, ftAutoInc, ftLargeint, ftFMTBcd]);
end;

function GetFieldString(Field: TField; const chQuote: Char): string;
begin
  if Assigned(Field) then
  begin
    if IsStringField(Field) then
    begin
      if chQuote = #0
      then Result := Field.AsString
      else Result := AnsiQuotedStr(Field.AsString, chQuote);
    end
    else Result := Field.AsString;
  end
  else Result := '';
end;

function GetFieldText(Field: TField; const chQuote: Char): string;
begin
  if Assigned(Field) then
  begin
    if IsStringField(Field) then
    begin
      if chQuote = #0
      then Result := Field.AsString
      else Result := AnsiQuotedStr(Field.AsString, chQuote);
    end
    else Result := Field.DisplayText;
  end
  else Result := '';
end;

function _Str2SqlStr(const Str: string): string;
begin
  {$IFDEF MYSQL}
  Result := EncodeEscapeChars(Str);
  {$ELSE}
  Result := AnsiReplaceStr(Str, '''', '''''');
  // Result := AnsiReplaceStr(EncodeChars(Str, [#0..#31], '$'), '''', '''''');
  {$ENDIF}
end;

function StrToSqlStr(const Str: string; const MaxLen: Integer): string;
begin
  Result := _Str2SqlStr(Str);
  if (MaxLen > 0) and (Length(Result) > MaxLen) then
    Result := Copy(Result, 1, MaxLen);
end;

function StrToSqlStr(const Str: string; const chQuote: Char; const bNullIsEmpty: Boolean): string; overload;
begin
  if (Str = EmptyStr) and bNullIsEmpty
  then Result := sqlNull
  else Result := chQuote + _Str2SqlStr(Str) + chQuote;
end;

function TextToSqlStr(const Str: string; const MaxLen: Integer): string;
begin
  Result := _Str2SqlStr(Str);
  if Length(Result) > MaxLen then
    Result := Copy(Result, 1, MaxLen);
end;

function StrToLikeStr(const Str: string): string;
begin
  Result := AnsiReplaceStr(
              AnsiReplaceStr(
                AnsiReplaceStr(Str, '[', '[[]'),
              '_', '[_]'),
            '%', '[%]');
end;

function BoolToSqlStr(const Value: Boolean): string;
begin
  if Value then Result := '1' else Result := '0';
end;

function IntToSqlStr(const Value: Integer): string;
begin
  if Value = -1 then Result := sqlNull else Result := IntToStr(Value);
end;

function FloatToSqlStr(const Value: Double; const ZeroNull: Boolean = False): string;
var
  FormatSettings: TFormatSettings;
begin
  if ZeroNull and (Value = 0) then Result := sqlNull
  else begin
    FormatSettings.ThousandSeparator := #0;
    FormatSettings.DecimalSeparator := '.';
    Result := FloatToStr(Value, FormatSettings);
  end;
end;

function DateToSqlStr(const DateFmt: string; const DateVal: TDateTime): string;
var
  QuoteChar, IntDateFmt: string;
begin
  Result := EmptyStr;
  if DateVal <> 0 then
  begin
    if Trim(DateFmt) <> EmptyStr then
    begin
      QuoteChar := '';
      IntDateFmt := DateFmt;
      if (DateFmt[1] = '''') and (DateFmt[Length(DateFmt)] = '''') then
      begin
        QuoteChar := '''';
        IntDateFmt := Copy(DateFmt, 2, Length(DateFmt) - 2);
      end;
      if (DateFmt[1] = '#') and (DateFmt[Length(DateFmt)] = '#') then
      begin
        QuoteChar := '#';
        IntDateFmt := Copy(DateFmt, 2, Length(DateFmt) - 2);
      end;
    end;
    Result := QuoteChar + FormatDateTime(IntDateFmt, DateVal) + QuoteChar;
  end
  else Result := sqlNull;
end;

function DateTimeToSqlStr(const DateFmt: string; const DateVal: TDateTime): string;
var
  QuoteChar, IntDateFmt: string;
begin
  Result := EmptyStr;
  if Trim(DateFmt) <> EmptyStr then
  begin
    QuoteChar := '';
    IntDateFmt := DateFmt + #32 + SqlTimeFormat;
    if (DateFmt[1] = '''') and (DateFmt[Length(DateFmt)] = '''') then
    begin
      QuoteChar := '''';
      IntDateFmt := Copy(DateFmt, 2, Length(DateFmt) - 2) + #32 + SqlTimeFormat;
    end;
    if (DateFmt[1] = '#') and (DateFmt[Length(DateFmt)] = '#') then
    begin
      QuoteChar := '#';
      IntDateFmt := Copy(DateFmt, 2, Length(DateFmt) - 2) + #32 + SqlTimeFormat;
    end;
  end;
  Result := QuoteChar + FormatDateTime(IntDateFmt, DateVal) + QuoteChar;
end;

function IntFieldToIdx(const Field: TField): Integer;
begin
  if Field.IsNull
  then Result := -1
  else Result := Field.AsInteger;
end;

procedure SetFieldIntIdx(Field: TField; const aValue: Integer);
begin
  if aValue = -1
  then Field.Clear
  else Field.AsInteger := aValue;
end;

procedure SetFieldFloat(Field: TField; const aValue: Extended);
begin
  if aValue = 0
  then Field.Clear
  else Field.AsFloat := aValue;
end;

procedure SetFieldDate(Field: TField; const aValue: TDateTime);
begin
  if aValue = 0
  then Field.Clear
  else Field.AsDateTime := aValue;
end;

procedure SetFieldString(Field: TField; const aValue: string);
begin
  if aValue = EmptyStr
  then Field.Clear
  else Field.AsString := aValue;
end;

{ == Переход на нужную позицию в справочнике =================================== }
function DataSetLocateEx(DataSet: TDataSet;
  const KeyField: string; const KeyId: Integer; const RaiseError: Boolean): Boolean;
begin
  Result := (DataSet.FieldByName(KeyField).AsInteger = KeyId)
          or DataSet.Locate(KeyField, KeyId, []);
  if not Result and RaiseError then
    raise Exception.CreateFmt(SErrDSIdNotFound, [KeyId, DataSet.Name]);
end;

function DataSetLocateId(DataSet: TDataSet;
  const KeyId: Integer; const RaiseError: Boolean): Boolean;
begin
  Result := (DataSet.FieldByName(fnID).AsInteger = KeyId)
          or DataSet.Locate(fnID, KeyId, []);
  if not Result and RaiseError then
    raise Exception.CreateFmt(SErrDSIdNotFound, [KeyId, DataSet.Name]);
end;

{ == Запоминаем и восстанавливаем позицию в наборе данных ====================== }
procedure PositionDS_Store(DataSet: TDataSet; const KeyField: string;
  var KeyId: Integer; var KeyBk: TBookmark);
begin
  KeyId := -1;
  KeyBk := nil;
  if Assigned(DataSet) and DataSet.Active and not DataSet.IsEmpty then
  begin
    if Assigned(DataSet.FindField(KeyField))
    then KeyId := DataSet.FieldByName(KeyField).AsInteger
    else KeyBk := DataSet.GetBookmark;
  end;
end;

procedure PositionDS_Restore(DataSet: TDataSet; const KeyField: string;
  const KeyId: Integer; var KeyBk: TBookmark);
begin
  try
    if Assigned(DataSet) then
    begin
      if (KeyId > -1) and Assigned(DataSet.FindField(KeyField))
      then DataSet.Locate(KeyField, KeyId, [])
      else begin
        if Assigned(KeyBk) then
        begin
          try
            DataSet.GotoBookmark(KeyBk);
          except
          end;
        end;
      end;
    end;
  finally
    if Assigned(DataSet) and Assigned(KeyBk) then
      DataSet.FreeBookmark(KeyBk);
    KeyBk := nil;
  end;
end;

{ == Работа с динамически создаваемыми запросами =============================== }
function CreateQueryName(const sTemplate: string): string;
var
  ID: TGUID;
begin
  Result := EmptyStr;
  if CoCreateGuid(ID) = S_OK then
    Result := Format(sTemplate, [AnsiReplaceStr(Copy(GUIDToString(ID), 2, 36), '-', '_')]);
end;

function OpenDynamicQuery(AConn: TADOConnection; const ASQL: string; const CmdTimeout: Integer = 30; const sName: string = ''): TADOQuery;
begin
  Result := TADOQuery.Create(AConn);
  if sName = ''
  then Result.Name := CreateQueryName('query_%s')
  else Result.Name := sName;
  Result.Connection := AConn;
  // Result.LockType := ltPessimistic;
  Result.CommandTimeout := CmdTimeout;
  Result.ExecuteOptions := [];
  Result.CursorLocation := clUseClient;
  Result.CursorType := ctKeyset;
  Result.ParamCheck := False;
  Result.DataSource := nil;
  // Result.Prepared := True;
  Result.SQL.Clear;
  Result.SQL.Add(ASQL);
  Result.Open;
end;

function ExecDynamicQuery(AConn: TADOConnection; const ASQL: string; const CmdTimeout: Integer = 30; const sName: string = ''): Boolean;
var
  DynQ: TADOQuery;
begin
  DynQ := TADOQuery.Create(AConn);
  try
    if sName = ''
    then DynQ.Name := CreateQueryName('exec_%s')
    else DynQ.Name := sName;
    DynQ.Connection := AConn;
    // DynQ.LockType := ltPessimistic;
    DynQ.CommandTimeout := CmdTimeout;
    DynQ.ExecuteOptions := [];
    DynQ.CursorLocation := clUseClient;
    DynQ.CursorType := ctKeyset;
    DynQ.ParamCheck := False;
    DynQ.DataSource := nil;
    // DynQ.Prepared := True;
    DynQ.SQL.Clear;
    DynQ.SQL.Add(ASQL);
    DynQ.ExecSQL;
    Result := True;
  finally
    if Assigned(DynQ) and DynQ.Active then DynQ.Close;
    FreeAndNil(DynQ);
  end;
end;

procedure FreeDynamicQuery(AQuery: TADOQuery);
begin
  if Assigned(AQuery) then
  begin
    if AQuery.Active then AQuery.Close;
    FreeAndNil(AQuery);
  end;
end;

{ == Проверка статуса набора данных ============================================ }
function DataSetIsOpen(ADataSet: TDataSet): Boolean;
begin
  Result := (ADataSet <> nil) and ADataSet.Active;
end;

function DataSetIsNotEmpty(ADataSet: TDataSet): Boolean;
begin
  Result := DataSetIsOpen(ADataSet) and not ADataSet.IsEmpty;
end;

function DynamicQueryIsNotEmpty(AQuery: TAdoQuery): Boolean;
begin
  Result := DataSetIsNotEmpty(TDataSet(AQuery));
end;

{ == "Безопасное" обновление запроса =========================================== }
procedure SafeRequery(ADataSet: TCustomADODataSet);
begin
  try
    ADataSet.Requery;
  except
    ADataSet.Close;
    ADataSet.Open;
  end;
end;

{ == Отмена режима редактирования ============================================== }
procedure CancelEditDb(DataSet: TDataSet);
begin
  if Assigned(DataSet) and (DataSet.State in [dsInsert, dsEdit]) then
    DataSet.Cancel;
end;

{ == Установка признака ReadOnly для Db-контролов ============================== }

procedure FillDbReadOnly(WinControl: TWinControl);
var
  i, iCount: Integer;
  Control: TControl;
  DataSource: TObject;
  Field: TField;
  ROnly: Boolean;
  FldPropInfo, DSPropInfo, ROPropInfo, PCPropInfo, CLPropInfo, MLPropInfo: PPropInfo;
begin
  if not (csDesigning in WinControl.ComponentState) then
  begin
    iCount := WinControl.ControlCount - 1;
    for i := 0 to iCount do
    begin
      Control := WinControl.Controls[i];
      if Control is TWinControl then
        FillDbReadOnly(TWinControl(Control));
      FldPropInfo := GetPropInfo(Control, PropField);
      DSPropInfo := GetPropInfo(Control, PropDataSource);
      ROPropInfo := GetPropInfo(Control, PropReadOnly);
      PCPropInfo := GetPropInfo(Control, PropParentColor);
      CLPropInfo := GetPropInfo(Control, PropColor);
      MLPropInfo := GetPropInfo(Control, PropMaxLength);
      if Assigned(FldPropInfo) and Assigned(DSPropInfo) and Assigned(ROPropInfo)
      and (DSPropInfo.PropType^.Kind = tkClass) then
      begin
        DataSource := GetObjectProp(Control, DSPropInfo);
        if (DataSource is TDataSource) and Assigned(TDataSource(DataSource))
        and Assigned(TDataSource(DataSource).DataSet) then
        begin
          Field := TDataSource(DataSource).DataSet.FindField(GetStrProp(Control, FldPropInfo));
          ROnly := (GetOrdProp(Control, ROPropInfo) = 1) or not Assigned(Field) or Field.ReadOnly
              or not (TDataSource(DataSource).State in [dsEdit, dsInsert]);
          if ROnly then
          begin
            SetOrdProp(Control, ROPropInfo, 1);
            if Assigned(PCPropInfo) then SetOrdProp(Control, PCPropInfo, 1);
          end
          else begin
            SetOrdProp(Control, ROPropInfo, 0);
            if not (Control is TCustomCheckBox) then begin
              if Assigned(PCPropInfo) then SetOrdProp(Control, PCPropInfo, 0);
              if Assigned(CLPropInfo) then SetOrdProp(Control, CLPropInfo, Integer(clWindow));
            end;
          end;
          if Assigned(MLPropInfo) and (Field.DataType in TStringFields) then
            SetOrdProp(Control, MLPropInfo, Field.Size);
        end;
      end;
    end;
  end;
end;

{ == Установка Label жирным шрифтом для required полей ========================= }
procedure FillDbRequired(WinControl: TWinControl);
var
  i, iCount: Integer;
  Control, LabelControl: TControl;
  Field: TField;
  DataSource: TObject;
  FldPropInfo, DSPropInfo: PPropInfo;
begin
  if not (csDesigning in WinControl.ComponentState) then
  begin
    iCount := WinControl.ControlCount - 1;
    for i := 0 to iCount do
    begin
      Control := WinControl.Controls[i];
      if Control is TWinControl then
        FillDbRequired(TWinControl(Control));
      if (Control is TLabel) and (TLabel(Control).FocusControl <> nil) then
      begin
        if TLabel(Control).Tag < 1 then
        begin
          LabelControl := TLabel(Control).FocusControl;
          FldPropInfo := GetPropInfo(LabelControl, PropField);
          DSPropInfo := GetPropInfo(LabelControl, PropDataSource);
          if Assigned(FldPropInfo) and Assigned(DSPropInfo)
            and (DSPropInfo.PropType^.Kind = tkClass) then
          begin
            DataSource := GetObjectProp(LabelControl, DSPropInfo);
            if (DataSource is TDataSource) and Assigned(TDataSource(DataSource))
            and Assigned(TDataSource(DataSource).DataSet) then
            begin
              Field := TDataSource(DataSource).DataSet.FindField(GetStrProp(LabelControl, FldPropInfo));
              if Field <> nil then
              begin
                if Field.Required
                then TLabel(Control).Font.Style := TLabel(Control).Font.Style + [fsBold]
                else TLabel(Control).Font.Style := TLabel(Control).Font.Style - [fsBold];
              end;
            end;
          end;
        end
        else begin
          TLabel(Control).Font.Style := [];
          if (TLabel(Control).Tag and 1) > 0 then
            TLabel(Control).Font.Style := TLabel(Control).Font.Style + [fsBold];
          if (TLabel(Control).Tag and 2) > 0 then
            TLabel(Control).Font.Style := TLabel(Control).Font.Style + [fsItalic];
          if (TLabel(Control).Tag and 4) > 0 then
            TLabel(Control).Font.Style := TLabel(Control).Font.Style + [fsUnderline];
          if (TLabel(Control).Tag and 8) > 0 then
            TLabel(Control).Font.Style := TLabel(Control).Font.Style + [fsStrikeOut];
        end;
      end;
    end;
  end;
end;

function CheckDbRequired(WinControl: TWinControl): Boolean;
var
  i, iCount: Integer;
  Control: TControl;
  Field: TField;
  DataSource, ListSource: TObject;
  FldPropInfo, DSPropInfo, LSPropInfo, LKPropInfo: PPropInfo;
  KeyField: string;
begin
  Result := True;
  if not (csDesigning in WinControl.ComponentState) then
  begin
    iCount := WinControl.ControlCount - 1;
    for i := 0 to iCount do
    begin
      Control := WinControl.Controls[i];
      if Control is TWinControl then
        Result := CheckDbRequired(TWinControl(Control));
      if Result then
      begin
        FldPropInfo := GetPropInfo(Control, PropField);
        DSPropInfo := GetPropInfo(Control, PropDataSource);
        if Assigned(FldPropInfo) and Assigned(DSPropInfo)
          and (DSPropInfo.PropType^.Kind = tkClass) then
        begin
          DataSource := GetObjectProp(Control, DSPropInfo);
          if (DataSource is TDataSource) and Assigned(TDataSource(DataSource))
          and Assigned(TDataSource(DataSource).DataSet) then
          begin
            Field := TDataSource(DataSource).DataSet.FindField(GetStrProp(Control, FldPropInfo));
            if (Field <> nil) and Field.Required then
            begin
              LSPropInfo := GetPropInfo(Control, PropListSource);
              LKPropInfo := GetPropInfo(Control, PropKeyField);
              if Assigned(LSPropInfo) and (LSPropInfo.PropType^.Kind = tkClass)
              and Assigned(LKPropInfo) and (LKPropInfo.PropType^.Kind in [tkString, tkLString]) then
              begin
                ListSource := GetObjectProp(Control, LSPropInfo);
                KeyField := GetStrProp(Control, LKPropInfo);
                if (ListSource is TDataSource) and Assigned(TDataSource(ListSource))
                and Assigned(TDataSource(ListSource).DataSet)
                and Assigned(TDataSource(ListSource).DataSet.FindField(KeyField)) then
                begin
                  if TDataSource(ListSource).DataSet.IsEmpty then
                  begin
                    Result := False;
                    WarningBox(Format(SFieldRefEmpty, [Field.DisplayLabel]));
                    try
                      TDbEdit(Control).SetFocus;
                    except
                    end;
                    Exit;
                  end
                  else begin
                    if Field.IsNull
                    or not TDataSource(ListSource).DataSet.Locate(KeyField, Field.AsVariant, []) then
                    begin
                      Result := False;
                      WarningBox(Format(SFieldRequired, [Field.DisplayLabel]));
                      try
                        TDbEdit(Control).SetFocus;
                      except
                      end;
                      Exit;
                    end;
                  end;
                end
                else begin
                  Result := False;
                  ErrorBox(Format(SFieldNotListed, [Field.DisplayLabel]));
                  Exit;
                end;
              end
              else begin
                if (Field.IsNull
                or (Field.DisplayText = EmptyStr)) then
                begin
                  Result := False;
                  WarningBox(Format(SFieldRequired, [Field.DisplayLabel]));
                  try
                    TDbEdit(Control).SetFocus;
                  except
                  end;
                  Exit;
                end;
              end;
            end;
          end;
        end;
      end
      else Exit;
    end;
  end;
end;

{ == Установка Hint-ов для контролов, имеющих поле 'DataField' ================= }
procedure FillDbHints(WinControl: TWinControl);
var
  i, iCount: Integer;
  Control: TControl;
  ListName, NullKey: string;
  Field: TField;
  DataSource: TObject;
  // LstDSPropInfo, LstFldPropInfo, LstIdxPropInfo: PPropInfo;
  FldPropInfo, DSPropInfo: PPropInfo;
begin
  if not (csDesigning in WinControl.ComponentState) then
  begin
    iCount := WinControl.ControlCount - 1;
    for i := 0 to iCount do
    begin
      Control := WinControl.Controls[i];
      if Control is TWinControl then
        FillDbHints(TWinControl(Control));
      DSPropInfo := GetPropInfo(Control, PropDataSource);
      FldPropInfo := GetPropInfo(Control, PropField);
      if Assigned(FldPropInfo) and Assigned(DSPropInfo)
      and (DSPropInfo.PropType^.Kind = tkClass) then
      begin
        DataSource := GetObjectProp(Control, DSPropInfo);
        if (DataSource is TDataSource) and Assigned(TDataSource(DataSource))
        and Assigned(TDataSource(DataSource).DataSet) then
        begin
          ListName := GetStrProp(Control, FldPropInfo);
          Control.Hint := TDataSource(DataSource).DataSet.Name + '.' + ListName;
          Field := TDataSource(DataSource).DataSet.FindField(ListName);
          if Assigned(Field) then
          begin
            Control.Hint := AnsiReplaceStr(Field.DisplayName, ' [#]', '');

            if Assigned(GetPropInfo(Control, PropNullKey)) then
            begin
              NullKey := ShortCutToText(GetPropValue(Control, PropNullKey));
              if NullKey <> '' then
                Control.Hint := Format(SNullValueHint, [Control.Hint, NullKey]);
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ == Поверка наличиая свойства SQL (TQuery, TAdoQuery...) ====================== }
function IsSqlDataSet(DataSet: TDataSet; var SqlText: string): Boolean;
var
  SqlPropInfo: PPropInfo;
  Sql: TObject;
begin
  Result := False;
  SqlText := '';
  if Assigned(DataSet) then begin
    SqlPropInfo := GetPropInfo(DataSet, PropSql);
    if Assigned(SqlPropInfo) then begin
      Sql := GetObjectProp(DataSet, SqlPropInfo);
      if Assigned(Sql) and (Sql is TStrings) then
      begin
        SqlText := TStrings(Sql).Text;
        Result := True;
      end;
    end;
  end;
end;

{ == Применение фильтра по любым полям ========================================= }
function GetDbFilterAllFields(DataSet: TDataSet; const FindStr: string; const UseLeadAsterisk, UseLookupTables: Boolean; const FieldsList: string = ''): string;
var
  i, iCount: Integer;
  sSubFilter: string;
  fSrcField: TField;

  (*
  function IsInteger(const sFindStr: string): Boolean;
  begin
    Result := True;
    try
      StrToInt(sFindStr);
    except
      Result := False;
    end;
  end;

  function IsFloat(const sFindStr: string): Boolean;
  begin
    Result := True;
    try
      StrToFloat(sFindStr);
    except
      Result := False;
    end;
  end;
  *)

  function IsInteger(const sFindStr: string): Boolean;
  var
    i, iCount: Integer;
  begin
    iCount := Length(sFindStr);
    if iCount > 0 then
    begin
      Result := True;
      for i := 1 to iCount do
        if not (sFindStr[i] in ['0'..'9']) then
        begin
          Result := False;
          Break;
        end;
    end
    else Result := False;
  end;

  function IsFloat(const sFindStr: string): Boolean;
  var
    i, iCount: Integer;
  begin
    iCount := Length(sFindStr);
    if iCount > 0 then
    begin
      Result := True;
      for i := 1 to iCount do
        if not (sFindStr[i] in ['0'..'9','.',',']) then
        begin
          Result := False;
          Break;
        end;
    end
    else Result := False;
  end;

  function GetLikeString(const sFindStr: string): string;
  begin
    Result := sFindStr;
    if Length(sFindStr) > 0 then
    begin
      if not (sFindStr[Length(sFindStr)] in ['%', '*']) then
        Result := Result + '*';
      if UseLeadAsterisk and not (sFindStr[1] in ['%', '*'])then
        Result := '*' + Result;
    end;
  end;

  procedure AddPartFilter(const sPartFilter: string);
  begin
    if Result = ''
    then Result := Format(sqlBrackets, [sPartFilter])
    else begin
      if AnsiPos(Format(sqlBrackets, [sPartFilter]), Result) = 0 then
        Result := Result + sqlOr + Format(sqlBrackets, [sPartFilter]);
    end;
  end;

  procedure ProcessField(Field: TField);
  begin
    case Field.FieldKind of
      fkData:
      begin
        if Field.Visible then
        begin
          case Field.DataType of
            ftString, ftWideString, ftMemo, ftFmtMemo, ftFixedChar:
            begin
              AddPartFilter(Format(fltFieldLikeStr, [Field.FieldName, GetLikeString(FindStr)]));
            end;
            ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint:
            begin
              if IsInteger(FindStr) then
                AddPartFilter(Format(fltFieldIdStr, [Field.FieldName, FindStr]));
            end;
            ftFloat, ftCurrency:
            begin
              if IsFloat(FindStr) then
                AddPartFilter(Format(fltFieldIdStr, [Field.FieldName, AnsiReplaceStr(FindStr, ',', '.')]));
            end;
          end;
        end;
      end;
      fkLookup:
      begin
        if UseLookupTables
        and Assigned(Field.LookupDataSet)
        and not Assigned(Field.LookupDataSet.DataSource) then
        begin
          sSubFilter := GetDbFilterAllFields(Field.LookupDataSet, FindStr, UseLeadAsterisk, True, Field.LookupResultField);
          if sSubFilter <> '' then
          begin
            with Field.LookupDataSet do
            begin
              DisableControls;
              try
                Filter := sSubFilter;
                Filtered := True;
                First;
                while not Eof do
                begin
                  AddPartFilter(Format(fltFieldIdStr,
                    [Field.KeyFields, FieldByName(Field.LookupKeyFields).AsString]));
                  Next;
                end;
              finally
                Filtered := False;
                Filter := '';
                EnableControls;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  Result := '';

  if FieldsList = '' then
  begin
    iCount := DataSet.Fields.Count - 1;
    for i := 0 to iCount do
      ProcessField(DataSet.Fields[i]);
  end
  else begin
    iCount := WordCount(FieldsList, [';', ',']);
    for i := 1 to iCount do
    begin
      fSrcField := DataSet.FindField(Trim(ExtractWord(i, FieldsList, [';', ','])));
      if Assigned(fSrcField) then
        ProcessField(fSrcField);
    end;
  end;
end;

procedure DbFilterAllFields(DataSet: TDataSet; const FindStr: string; const UseLeadAsterisk, UseLookupTables: Boolean; const FieldsList: string = '');
var
  sFilterStr: string;
begin
  if FindStr = ''
  then sFilterStr := ''
  else sFilterStr := GetDbFilterAllFields(DataSet, FindStr, UseLeadAsterisk, UseLookupTables, FieldsList);
  DataSet.Filter := sFilterStr;
  DataSet.Filtered := sFilterStr <> '';
end;

{ == Создаем список ID по условию ============================================== }
function GetIdList(DataSet: TDataSet; const KeyField, Filter: string): string;
var
  KeyId: Integer;
  KeyBk: Pointer;
begin
  Result := '';

  DataSet.DisableControls;
  PositionDS_Store(DataSet, KeyField, KeyId, KeyBk);
  try
    if Filter <> '' then
    begin
      DataSet.Filter := Filter;
      DataSet.Filtered := True;
    end;
    try
      DataSet.First;
      while not DataSet.Eof do
      begin
        Result := AddDelimStrEx(Result, DataSet.FieldByName(KeyField).AsString, ';');
        DataSet.Next;
      end;
    finally
      if DataSet.Filtered then
      begin
        DataSet.Filter := '';
        DataSet.Filtered := False;
      end;
    end;
  finally
    PositionDS_Restore(DataSet, KeyField, KeyId, KeyBk);
    DataSet.EnableControls;
  end;
end;

function GetFilterOnIdList(const FieldName, IdList: string): string;
var
  i, iCount: Integer;
begin
  Result := '';

  iCount := WordCount(IdList, [';']);
  for i := 1 to iCount do
    Result := AddDelimStrEx(Result,
      Format(fltFieldIdStr, [FieldName, ExtractWord(i, IdList, [';'])]),
      sqlOr);
end;

{ == Копирование данных из одного поля в другое ================================ }
procedure CopyField(flFrom, flTo: TField);
begin
  if Assigned(flFrom) and Assigned(flTo)
    and (flTo.DataSet.State in [dsEdit, dsInsert]) then
  begin
    if flFrom.IsNull then flTo.Clear
    else flTo.Value := flFrom.Value;
  end;
end;

procedure CopyFieldByName(dsFrom, dsTo: TDataSet; const sFieldName: string);
begin
  if Assigned(dsFrom) and Assigned(dsTo) then
    CopyField(dsFrom.FindField(sFieldName), dsTo.FindField(sFieldName));
end;

{ == Установка форматов ======================================================== }
procedure SetFieldFormats(DS: TDataSet; const sFieldName, aDisplayFormat, aEditFormat: string);
var
  fField: TField;
begin
  fField := DS.FindField(sFieldName);
  if Assigned(fField) and (fField is TNumericField) then
  begin
    with TNumericField(fField) do
    begin
      DisplayFormat := aDisplayFormat;
      EditFormat := aEditFormat;
    end;
  end;
end;

procedure SetFieldDisplayFormat(DS: TDataSet; const sFieldName, aDisplayFormat: string);
var
  fField: TField;
begin
  fField := DS.FindField(sFieldName);
  if Assigned(fField) and (fField is TNumericField) then
  begin
    with TNumericField(fField) do
      DisplayFormat := aDisplayFormat;
  end;
end;

procedure SetFieldEditFormat(DS: TDataSet; const sFieldName, aEditFormat: string);
var
  fField: TField;
begin
  fField := DS.FindField(sFieldName);
  if Assigned(fField) and (fField is TNumericField) then
  begin
    with TNumericField(fField) do
      EditFormat := aEditFormat;
  end;
end;

end.

