unit rHtmlUtils;

interface

uses
  Types, Classes;

type
  THtmlEncodeOption  = (
    heEof,           // преобразовывать переводы строк
    heQuotes,        // преобразовывать двойные кавычки
    heLinks,         // преобразовывать ссылки
    heSysCodes,      // преобразовывать системные символы в html-коды
    heBaseSymbols,   // преобразовывать "базовый" набор символов (&<>)
    heExtSymbols,    // преобразовывать "расширенный" набор символов
    heExtToCode      // преобразовывать "расширенный" набор символов в коды
    );
  THtmlEncodeOptions = set of THtmlEncodeOption;

  THtmlDecodeOption  = (
    hdBlocks,        // замена </p> и </div> на символы CRLF
    hdFormat,        // оставить форматирование текста
    hdLinks,         // оставить ссылки
    hdLinksAttr,     // оставить дополнительные атрибуты в сслыках
    hdEntities,      // преобразовывать html-обозначени€ символов
    hdCharCodes      // преобразовывать html-коды символов
    );
  THtmlDecodeOptions = set of THtmlDecodeOption;

  THtmlRef = record
    sURL: string;
    sText: string;
  end;

  THtmlRefArray = array of THtmlRef;

function html_ReplaceSpaces(const sIsStr: string): string;

function html_CharToCode(const chIn: Char): string;
function html_CodeToChar(const sIn: string): Char;

function html_CharsToEntities(const sIsStr: string; const Options: THtmlEncodeOptions): string;
function html_EntitiesToChars(const sIsStr: string): string;

function html_SysCharsToCodes(const sIsStr: string): string;
function html_CodesToChars(const sIsStr: string): string;

function html_HtmlEncode(const sInStr: string; const Options: THtmlEncodeOptions): string;
function html_HtmlDecode(const sInStr: string; const Options: THtmlDecodeOptions): string;

function html_ExtractRefs(const sInStr: string; const Options: THtmlDecodeOptions): THtmlRefArray;

implementation

uses
  SysUtils, StrUtils, rStrUtils, rDialogs;

function html_ReplaceSpaces(const sIsStr: string): string;
begin
  Result := StringReplace(
            StringReplace(sIsStr, '&nbsp; ', ' ', [rfReplaceAll, rfIgnoreCase]),
                                  '&nbsp;', ' ', [rfReplaceAll, rfIgnoreCase]);
end;

function html_CharToCode(const chIn: Char): string;
begin
  Result := '&#' + IntToStr(Ord(chIn)) + ';';
end;

function html_CodeToChar(const sIn: string): Char;
var
  iLen: Integer;
begin
  Result := '?';

  iLen := Length(sIn);
  if (Pos('&#', sIn) = 1) and (Pos(';', sIn) = iLen) then
    Result := Chr(StrToIntDef(Copy(sIn, 3, iLen - 3), Ord('?')));
end;

function html_CharsToEntities(const sIsStr: string; const Options: THtmlEncodeOptions): string;
begin
  Result := sIsStr;

  if heQuotes in Options then
    Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);

  if heBaseSymbols in Options then
  begin
    Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
    Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  end;

  if heSysCodes in Options then
    Result := html_SysCharsToCodes(Result);

  if heEof in Options then
    Result := StringReplace(
      StringReplace(Result, #13#10, '<br>', [rfReplaceAll]),
      #10, '<br>', [rfReplaceAll]);

  if heExtToCode in Options then
  begin
    Result := StringReplace(Result, 'Ц', html_CharToCode('Ц'), [rfReplaceAll]);
    Result := StringReplace(Result, 'Ч', html_CharToCode('Ч'), [rfReplaceAll]);
    Result := StringReplace(Result, 'С', html_CharToCode('С'), [rfReplaceAll]);
    Result := StringReplace(Result, 'Т', html_CharToCode('Т'), [rfReplaceAll]);
    Result := StringReplace(Result, 'В', html_CharToCode('В'), [rfReplaceAll]);
    Result := StringReplace(Result, 'У', html_CharToCode('У'), [rfReplaceAll]);
    Result := StringReplace(Result, 'Ф', html_CharToCode('Ф'), [rfReplaceAll]);
    Result := StringReplace(Result, 'Д', html_CharToCode('Д'), [rfReplaceAll]);
    Result := StringReplace(Result, 'Ђ', html_CharToCode('Ђ'), [rfReplaceAll]);
    Result := StringReplace(Result, 'ї', html_CharToCode('ї'), [rfReplaceAll]);
    Result := StringReplace(Result, 'ґ', html_CharToCode('ґ'), [rfReplaceAll]);
    Result := StringReplace(Result, 'І', html_CharToCode('І'), [rfReplaceAll]);
    Result := StringReplace(Result, '©', html_CharToCode('©'), [rfReplaceAll]);
    Result := StringReplace(Result, 'Ѓ', html_CharToCode('Ѓ'), [rfReplaceAll]);
    Result := StringReplace(Result, 'Щ', html_CharToCode('Щ'), [rfReplaceAll]);
    Result := StringReplace(Result, '∞', html_CharToCode('∞'), [rfReplaceAll]);
    Result := StringReplace(Result, 'И', html_CharToCode('И'), [rfReplaceAll]);
    Result := StringReplace(Result, '±', html_CharToCode('±'), [rfReplaceAll]);
    // Result := StringReplace(Result, '?', html_CharToCode('?'), [rfReplaceAll]);
    // Result := StringReplace(Result, '?', html_CharToCode('?'), [rfReplaceAll]);
    // Result := StringReplace(Result, '?', html_CharToCode('?'), [rfReplaceAll]);
    // Result := StringReplace(Result, '?', html_CharToCode('?'), [rfReplaceAll]);
    // Result := StringReplace(Result, '?', html_CharToCode('?'), [rfReplaceAll]);
    // Result := StringReplace(Result, '?', html_CharToCode('?'), [rfReplaceAll]);
    // Result := StringReplace(Result, '?', html_CharToCode('?'), [rfReplaceAll]);
  end
  else begin
    if heExtSymbols in Options then
    begin
      Result := StringReplace(Result, 'Ц', '&ndash;', [rfReplaceAll]);
      Result := StringReplace(Result, 'Ч', '&mdash;', [rfReplaceAll]);
      Result := StringReplace(Result, 'С', '&lsquo;', [rfReplaceAll]);
      Result := StringReplace(Result, 'Т', '&rsquo;', [rfReplaceAll]);
      Result := StringReplace(Result, 'В', '&sbquo;', [rfReplaceAll]);
      Result := StringReplace(Result, 'У', '&ldquo;', [rfReplaceAll]);
      Result := StringReplace(Result, 'Ф', '&rdquo;', [rfReplaceAll]);
      Result := StringReplace(Result, 'Д', '&bdquo;', [rfReplaceAll]);
      Result := StringReplace(Result, 'Ђ', '&laquo;', [rfReplaceAll]);
      Result := StringReplace(Result, 'ї', '&raquo;', [rfReplaceAll]);
      Result := StringReplace(Result, 'ґ', '&para;', [rfReplaceAll]);
      Result := StringReplace(Result, 'І', '&sect;', [rfReplaceAll]);
      Result := StringReplace(Result, '©', '&copy;', [rfReplaceAll]);
      Result := StringReplace(Result, 'Ѓ', '&reg;', [rfReplaceAll]);
      Result := StringReplace(Result, 'Щ', '&trade;', [rfReplaceAll]);
      Result := StringReplace(Result, '∞', '&deg;', [rfReplaceAll]);
      Result := StringReplace(Result, 'И', '&euro;', [rfReplaceAll]);
      Result := StringReplace(Result, '±', '&plusmn;', [rfReplaceAll]);
      // Result := StringReplace(Result, '?', '&pound;', [rfReplaceAll]);
      // Result := StringReplace(Result, '?', '&times;', [rfReplaceAll]);
      // Result := StringReplace(Result, '?', '&divide;', [rfReplaceAll]);
      // Result := StringReplace(Result, '?', '&frac14;', [rfReplaceAll]);
      // Result := StringReplace(Result, '?', '&frac12;', [rfReplaceAll]);
      // Result := StringReplace(Result, '?', '&frac34;', [rfReplaceAll]);
      // Result := StringReplace(Result, '?', '&fnof;', [rfReplaceAll]);
    end;
  end;
end;

function html_EntitiesToChars(const sIsStr: string): string;
begin
  Result := sIsStr;

  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&ndash;', 'Ц', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&mdash;', 'Ч', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&lsquo;', 'С', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&rsquo;', 'Т', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&sbquo;', 'В', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&ldquo;', 'У', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&rdquo;', 'Ф', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&bdquo;', 'Д', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&laquo;', 'Ђ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&raquo;', 'ї', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&para;', 'ґ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&sect;', 'І', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&copy;', '©', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&reg;', 'Ѓ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&trade;', 'Щ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&deg;', '∞', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&euro;', 'И', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '&plusmn;', '±', [rfReplaceAll, rfIgnoreCase]);
  // Result := StringReplace(Result, '&pound;', '?', [rfReplaceAll, rfIgnoreCase]);
  // Result := StringReplace(Result, '&times;', '?', [rfReplaceAll, rfIgnoreCase]);
  // Result := StringReplace(Result, '&divide;', '?', [rfReplaceAll, rfIgnoreCase]);
  // Result := StringReplace(Result, '&frac14;', '?', [rfReplaceAll, rfIgnoreCase]);
  // Result := StringReplace(Result, '&frac12;', '?', [rfReplaceAll, rfIgnoreCase]);
  // Result := StringReplace(Result, '&frac34;', '?', [rfReplaceAll, rfIgnoreCase]);
  // Result := StringReplace(Result, '&fnof;', '?', [rfReplaceAll, rfIgnoreCase]);
end;

function html_SysCharsToCodes(const sIsStr: string): string;
var
  i, iCount: Integer;
  iChr: Byte;
begin
  Result := EmptyStr;

  iCount := Length(sIsStr);
  for i := 1 to iCount do
  begin
    iChr := Ord(sIsStr[i]);
    if (iChr < 32) and (iChr <> 10) and (iChr <> 13)
    then Result := Result + html_CharToCode(sIsStr[i])
    else Result := Result + sIsStr[i];
  end;
end;

function html_CodesToChars(const sIsStr: string): string;
var
  iPos, i: Integer;
  sCode: string;
begin
  Result := sIsStr;
  repeat
    iPos := Pos('&#', Result);
    if iPos > 0 then
    begin
      i := iPos + 2;

      sCode := '';
      while (i <= Length(Result)) and (Result[i] <> ';') do
      begin
        sCode := sCode + Result[i];
        Inc(i);
      end;

      Result := StringReplace(Result, '&#' + sCode + ';',
                  Chr(StrToIntDef(sCode, Ord('?'))),
                  [rfReplaceAll, rfIgnoreCase]);
    end;
  until iPos = 0;
end;

function html_HtmlEncode(const sInStr: string; const Options: THtmlEncodeOptions): string;
var
  i, iCount: Integer;
  bTag, bUrl: Boolean;
  sTag, sVal: string;

  function EncodeText(const sValue: string): string;
  begin
    Result := html_CharsToEntities(sValue, Options);
  end;

begin
  if heLinks in Options then
    Result := EncodeText(sInStr)
  else begin
    Result := EmptyStr;

    bTag := False;
    bUrl := False;
    sTag := EmptyStr;
    sVal := EmptyStr;

    iCount := Length(sInStr);
    for i := 1 to iCount do
    begin
      if (sInStr[i] = '<') and not bTag then
      begin
        if sVal <> EmptyStr then
        begin
          Result := Result + EncodeText(sVal);
          sVal := EmptyStr;
        end;
        sTag := EmptyStr;
        bTag := True;

        Continue;
      end;

      if (sInStr[i] = '>') and bTag then
      begin
        bTag := False;
        sTag := Trim(sTag);

        // ссылки - открывающий тег
        if AnsiStartsText('a ', sTag) then
        begin
          bUrl := True;
          Result := Result + '<' + sTag + '>'
        end;

        // закрывающий тег
        if bUrl and AnsiSameText(sTag, '/a') then
        begin
          bUrl := False;
          Result := Result + '<' + sTag + '>';
        end;

        Continue;
      end;

      if bTag then
        sTag := sTag + sInStr[i]
      else
        sVal := sVal + sInStr[i];
    end;

    if sVal <> EmptyStr then
      Result := Result + EncodeText(sVal);
  end;
end;

function html_HtmlDecode(const sInStr: string; const Options: THtmlDecodeOptions): string;
var
  i, iCount: Integer;
  iPos: Integer;
  bTag, bUrl: Boolean;
  sTag, sVal: string;

  function DecodeText(const sValue: string): string;
  begin
    Result := html_ReplaceSpaces(sValue);

    if hdEntities in Options then
      Result := html_EntitiesToChars(Result);

    if hdCharCodes in Options then
      Result := html_CodesToChars(Result);
  end;

begin
  Result := EmptyStr;

  bTag := False;
  bUrl := False;
  sTag := EmptyStr;
  sVal := EmptyStr;
  iCount := Length(sInStr);
  for i := 1 to iCount do
  begin
    if (sInStr[i] = '<') and not bTag then
    begin
      if sVal <> EmptyStr then
      begin
        Result := Result + DecodeText(sVal);
        sVal := EmptyStr;
      end;
      sTag := EmptyStr;
      bTag := True;

      Continue;
    end;

    if (sInStr[i] = '>') and bTag then
    begin
      bTag := False;
      sTag := Trim(sTag);

      // переводы строк
      if AnsiStartsText('br', sTag)
      then Result := Result + #10;

      // горизонтальна€ лини€
      if AnsiStartsText('hr', sTag)
      then Result := Result + '---' + #10;
      // как вариант: Result + #13#10 + '---' + #13#10;

      // блочна€ разметка
      if hdBlocks in Options then
      begin
        if AnsiStartsText('/p', sTag)   // конец абзаца
        or AnsiStartsText('/h', sTag)   // конец заголовка
        or AnsiStartsText('/div', sTag) // конец блока
        then Result := Result + #10;
      end;

      // форматирование текста
      if hdFormat in Options then
      begin
        if AnsiSameText('b', sTag) or AnsiSameText('/b', sTag)
        or AnsiSameText('i', sTag) or AnsiSameText('/i', sTag)
        or AnsiSameText('em', sTag) or AnsiSameText('/em', sTag)
        or AnsiSameText('strong', sTag) or AnsiSameText('/strong', sTag)
        or AnsiSameText('code', sTag) or AnsiSameText('/code', sTag)
        then Result := Result + '<' + sTag + '>';
      end;

      // ссылки
      if hdLinks in Options then
      begin
        // открывающий тег
        if AnsiStartsText('a ', sTag) then
        begin
          bUrl := True;
          // атрибуты ссылок
          if hdLinksAttr in Options then
            Result := Result + '<' + sTag + '>'
          else begin
            iPos := Pos(UpperCase('href='), UpperCase(sTag));
            if iPos > 0 then
            begin
              if sTag[iPos + 5] = '"'
              then sTag := ExtractValueQuoted(Copy(sTag, iPos + 5, Length(sTag) - iPos - 4))
              else sTag := ExtractValueDelimited(Copy(sTag, iPos + 5, Length(sTag) - iPos - 4), [#32]);

              Result := Result + '<a href=' + sTag + '>';
            end;
          end;
        end;

        // закрывающий тег
        if bUrl and AnsiSameText(sTag, '/a') then
        begin
          bUrl := False;
          Result := Result + '<' + sTag + '>';
        end;
      end;

      Continue;
    end;

    if bTag then
      sTag := sTag + sInStr[i]
    else
      sVal := sVal + sInStr[i];
  end;

  if sVal <> EmptyStr then
    Result := Result + DecodeText(sVal);
end;

function html_ExtractRefs(const sInStr: string; const Options: THtmlDecodeOptions): THtmlRefArray;
var
  i, iCount: Integer;
  iPos: Integer;
  bTag, bUrl: Boolean;
  sTag, sUrl, sVal: string;

  function DecodeText(const sValue: string): string;
  begin
    Result := html_ReplaceSpaces(sValue);

    if hdEntities in Options then
      Result := html_EntitiesToChars(Result);

    if hdCharCodes in Options then
      Result := html_CodesToChars(Result);
  end;

begin
  SetLength(Result, 0);

  bTag := False;
  bUrl := False;
  sTag := EmptyStr;
  sUrl := EmptyStr;
  sVal := EmptyStr;

  iCount := Length(sInStr);
  for i := 1 to iCount do
  begin
    if (sInStr[i] = '<') and not bTag then
    begin
      bTag := True;
      sTag := EmptyStr;

      if not bUrl then
      begin
        sUrl := EmptyStr;
        sVal := EmptyStr;
      end;

      Continue;
    end;

    if (sInStr[i] = '>') and bTag then
    begin
      bTag := False;
      sTag := Trim(sTag);

      // открывающий тег
      if AnsiStartsText('a ', sTag) then
      begin
        iPos := Pos(UpperCase('href='), UpperCase(sTag));
        if iPos > 0 then
        begin
          bUrl := True;
          if sTag[iPos + 5] = '"'
          then sUrl := ExtractValueQuoted(Copy(sTag, iPos + 5, Length(sTag) - iPos - 4))
          else sUrl := ExtractValueDelimited(Copy(sTag, iPos + 5, Length(sTag) - iPos - 4), [#32]);
          sVal := EmptyStr;
        end
        else begin
          bUrl := False;
          sUrl := EmptyStr;
          sVal := EmptyStr;
        end;
      end;

      // закрывающий тег
      if bUrl and AnsiSameText(sTag, '/a') then
      begin
        bUrl := False;
        if (sUrl <> EmptyStr) then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)].sURL := sUrl;
          Result[High(Result)].sText := sVal;
        end;
      end;

      Continue;
    end;

    if bTag then
      sTag := sTag + sInStr[i]
    else begin
      if bUrl then
        sVal := sVal + sInStr[i];
    end;
  end;
end;

end.

