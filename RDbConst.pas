unit RDbConst;

interface

uses
  Db;

const
  fnID                   = 'id';
  fnID_REF               = 'id_%s';
  fnGUID                 = 'guid';
  fnOWNER_ID             = 'owner_id';
  fnGROUP_ID             = 'id_groups';
  fnFULLNAME             = 'fullname';
  fnPASSWORD             = 'password';
  fnNAME                 = 'name';
  fnNAME_S               = 'name_s';
  fnNOTES                = 'notes';
  fnNUMBER               = 'number';
  fnDATE                 = 'date';
  fnCOUNT                = 'cnt';
  fnCELLCOLOR            = 'cell_color';
  fnFONTCOLOR            = 'font_color';
  fnFONTSTYLE            = 'font_style';
  fnTYPE                 = 'type';
  fnMODE                 = 'mode';
  fnDEF_VALUE            = 'def_value';
  fnVALUE_INT            = 'value_int';
  fnVALUE_CHAR           = 'value_char';
  fnVALUE_REAL           = 'value_real';
  fnVARIABLES            = 'variables';
  fnCREATED              = 'created';
  fnDELETED              = 'deleted';
  fnBLOCKED              = 'blocked';
  fnCHANGED              = 'changed';
  fnDATECREATE           = 'date_create';
  fnDATECHANGE           = 'date_change';
  fnID_USERS             = 'id_users';
  fnID_CREATOR           = 'id_creator';
  fnID_CHANGER           = 'id_changer';
  fnOBJECT_ID            = 'object_id';
  fnATTACHS_CNT          = 'attachs_cnt';

  fnlNN                  = 'name;notes';
  fnlINN                 = 'id;name;notes';
  fnlSNN                 = 'name_s;name;notes';
  fnlNNN                 = 'number;name;notes';

  sqlListSeparator       = ',';
  sqlBrackets            = '(%s)';
  sqlNull                = 'NULL';
  sqlNumber              = '%d';
  sqlString              = '''%s''';
  sqlWhere               = 'WHERE %s';
  sqlGroupBy             = 'GROUP BY %s';
  sqlOrder               = 'ORDER BY %s';
  sqlHaving              = 'HAVING %s';
  sqlAsc                 = ' ASC';
  sqlDesc                = ' DESC';
  sqlNot                 = 'NOT ';
  sqlAnd                 = ' AND ';
  sqlOr                  = ' OR ';
  sqlComma               = ', ';
  sqlNotFmt              = 'NOT %s';
  sqlAndFmt              = '%s AND %s';
  sqlAndBracketsFmt      = '(%s) AND (%s)';
  sqlOrFmt               = '%s OR %s';
  sqlOrBracketsFmt       = '(%s) OR (%s)';
  sqlIsNull              = '(%s IS NULL)';
  sqlNotNull             = 'NOT (%s IS NULL)';
  sqlCondNumber          = '%s=%d';
  sqlCondNumberStr       = '%s=%s';
  sqlCondString          = '%s=''%s''';
  sqlCondStringUpper     = 'Upper(%s)=Upper(''%s'')';
  sqlLikeString          = '%s LIKE ''%s''';
  sqlLikeStringPrc       = '%s LIKE ''%%%s%%''';
  sqlInList              = '%s IN (%s)';
  sqlCondValueV          = '= %s';
  sqlCondStringV         = '= ''%s''';
  sqlLikeStringV         = 'LIKE ''%s''';
  sqlInListV             = 'IN (%s)';
  sqlCondOperator        = '=';
  sqlLikeOperator        = 'LIKE';
  sqlLikeEnd             = '%';
  sqlUpper               = 'Upper';
  sqlUnion               = 'UNION';
  sqlUnionEol            = #13'UNION'#13;
  sqlGo                  = 'GO';
  sqlSetDelimiter        = 'DELIMITER';
  sqlCommentStart1       = '/*';
  sqlCommentStart2       = '--';
  sqlCommentStart3       = '//';
  sqlAccessEndCmd        = ';';

  sqlSelectWhereDef      = 'SELECT * FROM %s WHERE %s';
  sqlSelectWhere         = 'SELECT %s FROM %s WHERE %s';
  sqlDeleteWhere         = 'DELETE FROM %s WHERE %s';

  SNullText              = '<NULL>';
  SDataSetNullName       = '???';
  SFieldNotFound         = '< NOT FOUND >';
  SNullListValue         = '-1';

  SIdNameText            = '%s, id=%s';
  SFldNameText           = '%s="%s"';

  fltFieldNotDeleted     = '[%s]=%d';
  fltFieldNull           = '[%0:s]=NULL OR [%0:s]=0';
  fltFieldVar            = '[%s]=''%s''';
  fltFieldNotVar         = '[%s]<>''%s''';
  fltFieldId             = '[%s]=%d';
  fltFieldIdStr          = '[%s]=%s';
  fltFieldNotId          = '[%s]<>%d';
  fltFieldLikeStr        = '[%s] LIKE ''%s''';
  fltOwnerNull           = '[owner_id]=NULL OR [owner_id]=0';
  fltOwnerId             = '[owner_id]=%d';
  fltVisibleTrue         = '[visible]=1';

  pnPARAM_0              = 'PARAM0';
  pnPARAM_1              = 'PARAM1';

  TStringFields          = [ftString, ftFixedChar, ftWideString];

  sidDbVersion           = 1000;

  tidReadOnly	        	 = 0;
  tidInteger	           = 1;
  tidString              = 2;
  tidReal                = 3;
  tidDate                = 4;
  tidDateTime            = 5;
  tidBoolean             = 6;
  tidFileName            = 7;
  tidFilePath            = 8;

const
  FieldsDivChars         = [';',','];

// procedure rSql_AddPart(var sqlBase: string; const sqlOperator, sqlPart: string);
function SqlConcat(const sqlValue1, sqlValue2, sqlOperator: string): string;
function SqlConcatBr(const sqlValue1, sqlValue2, sqlOperator: string): string;

implementation

uses
  SysUtils;

(* depricated ******************************************************************
procedure rSql_AddPart(var sqlBase: string; const sqlOperator, sqlPart: string);
begin
  if sqlBase = '' then
    sqlBase := sqlPart
  else begin
    if sqlPart <> '' then
      sqlBase := Format(sqlBrackets, [sqlBase]) + sqlOperator + Format(sqlBrackets, [sqlPart]);
  end;
end;
********************************************************************************)

function SqlConcat(const sqlValue1, sqlValue2, sqlOperator: string): string;
begin
  if sqlValue2 = EmptyStr then
    Result := sqlValue1
  else begin
    if sqlValue1 = EmptyStr then
      Result := sqlValue2
    else
      Result := sqlValue1 + sqlOperator + sqlValue2;
  end;
end;

function SqlConcatBr(const sqlValue1, sqlValue2, sqlOperator: string): string;
begin
  if sqlValue2 = EmptyStr then
    Result := sqlValue1
  else begin
    if sqlValue1 = EmptyStr then
      Result := sqlValue2
    else
      Result := Format(sqlBrackets, [sqlValue1]) + sqlOperator + Format(sqlBrackets, [sqlValue2]);
  end;
end;

end.
