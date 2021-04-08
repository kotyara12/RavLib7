unit RCsvUtils;

interface

uses
  Classes;

function rCsv_QuotedStr(const sInStr: string): string;
function rCsv_ExtractQuotedStr(const sInStr: string): string;
function rCsv_CommaText(const slInText: TStringList; const chDiv: Char = ','): string;
function rCsv_ExtractCommaText(const sInStr: string; const chDiv: Char = ','): string;

implementation

uses
  SysUtils;

function rCsv_QuotedStr(const sInStr: string): string;
var
  i: Integer;
begin
  Result := sInStr;
  for i := Length(sInStr) downto 1 do
    if Result[i] = '"' then
      Insert('""', Result, i);
  Result := '"' + Result + '"';
end;

function rCsv_ExtractQuotedStr(const sInStr: string): string;
var
  i: Integer;
begin
  Result := Trim(sInStr);
  if (Length(Result) > 1) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
  begin
    Delete(Result, 1, 1);
    Delete(Result, Length(Result), 1);
    for i := Length(Result) downto 1 do
    begin
      if Result[i] = '"' then
      begin
        if not ((i > 1) and (Result[i - 1] = '"')) then
          Delete(Result, i, 1);
      end;
    end;
  end;
end;

function rCsv_CommaText(const slInText: TStringList; const chDiv: Char = ','): string;
var
  i, iCount: Integer;
begin
  Result := '';
  iCount := slInText.Count - 1;
  for i := 0 to iCount do
  begin
    if Result = ''
    then Result := rCsv_QuotedStr(slInText[i])
    else Result := Result + chDiv + rCsv_QuotedStr(slInText[i]);
  end;
end;

function rCsv_ExtractCommaText(const sInStr: string; const chDiv: Char = ','): string;
var
  slBuf: TStringList;
  sLine: string;
  bQuoted: Boolean;
  i, iCount: Integer;
begin
  Result := EmptyStr;
  if sInStr <> EmptyStr then
  begin
    slBuf := TStringList.Create;
    try
      i := 1;
      iCount := Length(sInStr);
      bQuoted := False;
      sLine := EmptyStr;
      while i <= iCount do
      begin
        if sInStr[i] = '"' then
        begin
          if (i < iCount) and (sInStr[i + 1] = '"') then
          begin
            sLine := sLine + sInStr[i];
            Inc(i);
          end
          else bQuoted := not bQuoted;
        end
        else if sInStr[i] = chDiv then
        begin
          if bQuoted then
            sLine := sLine + sInStr[i]
          else begin
            slBuf.Add(sLine);
            sLine := EmptyStr;
          end;
        end
        else if sInStr[i] = #10 then
        begin
          sLine := sLine + #32;
        end
        else sLine := sLine + sInStr[i];

        Inc(i);
      end;
      if sLine <> EmptyStr then
      begin
        slBuf.Add(sLine);
        sLine := EmptyStr;
      end;

      Result := slBuf.Text;
    finally
      slBuf.Free;
    end;
  end;
end;

end.
