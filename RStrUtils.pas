unit RStrUtils;

interface

type
  TCharSet = set of Char;

function AddNotesEx(const NameStr, NotesStr: string; const CharOpen, CharClose: Char): string;
function AddNotesChs(const NameStr, NotesStr, Chars: string): string;
function AddNotes(const NameStr, NotesStr: string): string;
function ExtractName(const NameAndNotesStr: string): string;
function AddDelimStrEx(const BaseStr, AddStr, DelimStr: string): string;
function AddDelimStr(const BaseStr, AddStr: string): string;

function AnsiCompareFind(const FindStr, TextStr: string; CaseSensitive, PartialFind: Boolean): Boolean;

function ReplaceIncorrectChars(const S: string; const IncorrectChars: TCharSet): string;
function RestoreIncorrectChars(const S: string): string;
function ReplaceSpecialChars(const S: string; const chRepl: Char): string;

function EncodeChars(const inStr: string; const csChars: TCharSet; const chTag: Char): string;
function DecodeChars(const inStr: string; const chTag: Char): string;

function EncodeEscapeChars(const sInStr: string): string;
function DecodeEscapeChars(const sInStr: string): string;

function DelDoubleSpaces(const S: string): string;

function GetShortName(const Name: string; const MinLength: Integer = 1; const UppLength: Integer = 1): string;

function CheckCirylic(const S: string): Boolean;
function CharsCirylicToLatin(const S: string): string;

procedure ParseCommand(const CmdLine: string; var CmdName, CmdParams: string;
  const Delimiter: Char = #32);

function ExtractValueQuoted(const sInStr: string): string;
function ExtractValueDelimited(const sInStr: string; const fDelims: TCharSet): string;

implementation

uses
  SysUtils, StrUtils, RClipBrd;

function AddNotesEx(const NameStr, NotesStr: string; const CharOpen, CharClose: Char): string;
begin
  if Trim(NotesStr) = '' then
    Result := NameStr
  else
    Result := NameStr + ' ' + CharOpen + Trim(NotesStr) + CharClose;
end;


function AddNotesChs(const NameStr, NotesStr, Chars: string): string;
begin
  if Length(Chars) > 1
  then Result := AddNotesEx(NameStr, NotesStr, Chars[1], Chars[2])
  else Result := AddNotesEx(NameStr, NotesStr, '(', ')');
end;

function AddNotes(const NameStr, NotesStr: string): string;
begin
  Result := AddNotesEx(NameStr, NotesStr, '(', ')');
end;

function ExtractName(const NameAndNotesStr: string): string;
begin
  Result := NameAndNotesStr;
  if Pos('(', NameAndNotesStr) > 1 then
    Result := Trim(Copy(NameAndNotesStr, 1, Pos('(', NameAndNotesStr) - 1));
end;

function AddDelimStrEx(const BaseStr, AddStr, DelimStr: string): string;
begin
  if AddStr <> '' then begin
    if BaseStr = '' then
      Result := AddStr
    else
      Result := BaseStr + DelimStr + AddStr;
  end
  else
    Result := BaseStr;
end;

function AddDelimStr(const BaseStr, AddStr: string): string;
begin
  Result := AddDelimStrEx(BaseStr, AddStr, ', ');
end;

function AnsiCompareFind(const FindStr, TextStr: string; CaseSensitive, PartialFind: Boolean): Boolean;
begin
  if PartialFind then
  begin
    if CaseSensitive
    then Result := AnsiContainsStr(TextStr, FindStr)
    else Result := AnsiContainsText(TextStr, FindStr);
  end
  else begin
    if CaseSensitive
    then Result := AnsiSameStr(TextStr, FindStr)
    else Result := AnsiSameText(TextStr, FindStr);
  end;
end;

function ReplaceIncorrectChars(const S: string; const IncorrectChars: TCharSet): string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 1 to Length(S) do
    if S[i] in IncorrectChars
    then Result := Result + '%' + IntToHex(Ord(S[i]), 2)
    else Result := Result + S[i];
end;

function RestoreIncorrectChars(const S: string): string;
var
  i: Integer;
begin
  Result := EmptyStr;
  i := 1;
  while i <= Length(S) do
  begin
    if S[i]='%' then
    begin
      Result := Result + Chr(StrToIntDef('$' + S[i+1] + S[i+2], 32));
      Inc(i, 3);
    end
    else begin
      Result := Result + S[i];
      Inc(i);
    end;
  end;
end;

function ReplaceSpecialChars(const S: string; const chRepl: Char): string;
var
  i, iCount: Integer;
begin
  Result := EmptyStr;

  iCount := Length(S);
  for i := 1 to iCount do
    if S[i] in [#0..#32]
    then Result := Result + chRepl
    else Result := Result + S[i];
end;

function EncodeChars(const inStr: string; const csChars: TCharSet; const chTag: Char): string;
var
  i, iCount: Integer;
begin
  Result := EmptyStr;

  iCount := Length(inStr);
  for i := 1 to iCount do
    if inStr[i] in csChars
    then Result := Concat(Result, chTag, IntToHex(Ord(inStr[i]), 2))
    else Result := Concat(Result, inStr[i]);
end;

function DecodeChars(const inStr: string; const chTag: Char): string;
var
  i, iCount: Integer;
begin
  Result := EmptyStr;

  i := 1;
  iCount := Length(inStr);
  while i <= iCount do
  begin
    if inStr[i] = chTag then
    begin
      Result := Concat(Result, Chr(StrToIntDef(Concat('$', inStr[i+1], inStr[i+2]), 32)));
      Inc(i, 3);
    end
    else begin
      Result := Concat(Result, inStr[i]);
      Inc(i);
    end;
  end;
end;

function EncodeEscapeChars(const sInStr: string): string;
var
  i, iCount: Integer;
begin
  Result := EmptyStr;

  iCount := Length(sInStr);
  for i := 1 to iCount do
  begin
    case sInStr[i] of
      #00: Result := Result + '\0';
      #01: Result := Result + '\1';
      #02: Result := Result + '\2';
      #03: Result := Result + '\3';
      #04: Result := Result + '\4';
      #05: Result := Result + '\5';
      #06: Result := Result + '\6';
      #07: Result := Result + '\a';
      #08: Result := Result + '\b';
      #09: Result := Result + '\t';
      #10: Result := Result + '\n';
      #11: Result := Result + '\v';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      #27: Result := Result + '\e';
      '''': Result := Result + '\''';
      '"': Result := Result + '\"';
      '?': Result := Result + '\?';
      '\': Result := Result + '\\';
      else Result := Result + sInStr[i];
    end;
  end;
end;

function DecodeEscapeChars(const sInStr: string): string;
var
  i: Integer;
begin
  Result := EmptyStr;

  i := 1;
  while i <= Length(sInStr) do
  begin
    if (sInStr[i]='\') and (i < Length(sInStr)) then
    begin
      case sInStr[i + 1] of
        '0': Result := Result + #00;
        '1': Result := Result + #01;
        '2': Result := Result + #02;
        '3': Result := Result + #03;
        '4': Result := Result + #04;
        '5': Result := Result + #05;
        '6': Result := Result + #06;
        'a': Result := Result + #07;
        'b': Result := Result + #08;
        't': Result := Result + #09;
        'n': Result := Result + #10;
        'v': Result := Result + #11;
        'f': Result := Result + #12;
        'r': Result := Result + #13;
        'e': Result := Result + #27;
        else Result := Result + sInStr[i + 1];
      end;
      Inc(i, 2);
    end
    else begin
      Result := Result + sInStr[i];
      Inc(i);
    end;
  end;
end;

function DelDoubleSpaces(const S: string): string;
var
  i: Integer;
begin
  Result := S;
  for i := Length(Result) downto 2 do
  begin
    if (Result[i] = ' ') and (Result[i - 1] = ' ') then
      Delete(Result, i, 1);
  end;
  Result := Trim(Result);
end;

function GetShortName(const Name: string; const MinLength: Integer = 1;
  const UppLength: Integer = 1): string;
const
  Numbers = ['¹', '#'];
  Symbols = ['0'..'9', '/', '¹', '#'];
  Bukovki = ['a'..'z', 'A'..'Z', 'à'..'ÿ', 'À'..'ß'];
  StopsSm = ['(', ')', '[', ']', '{', '}', '.'];
var
  i: Integer;
  SWord: string;

  procedure CalcShortName(const APos: Integer);
  begin
    if Length(SWord) >= MinLength then begin
      if Length(SWord) >= UppLength
      then Result := Result + AnsiUpperCase(Copy(SWord, 1, 1))
      else Result := Result + Copy(SWord, 1, 1);
    end;
    SWord := EmptyStr;
    if Name[APos] in Symbols then begin
      if (APos > 1) and (Name[APos - 1] = #32)
      and not ((APos > 2) and (Name[APos - 2] in Numbers))
      then Result := Result + #32 + Name[APos]
      else Result := Result + Name[APos];
    end;
  end;

begin
  Result := EmptyStr;
  SWord := EmptyStr;
  for i := 1 to Length(Name) do
  begin
    if Name[i] in Bukovki
    then SWord := SWord + Name[i]
    else begin
      CalcShortName(i);
      if Name[i] in StopsSm then Break;
    end;
  end;
  CalcShortName(Length(Name));
end;

function CheckCirylic(const S: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do
    if S[i] in ['À'..'ß', 'à'..'ÿ'] then
    begin
      Result := True;
      Break;
    end;
end;

function CharsCirylicToLatin(const S: string): string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 1 to Length(S) do
  begin
    case S[i] of
      '¨': Result := Result + 'E';
      'É': Result := Result + 'Y';
      'Ö': Result := Result + 'Ts';
      'Ó': Result := Result + 'U';
      'Ê': Result := Result + 'K';
      'Å': Result := Result + 'E';
      'Í': Result := Result + 'N';
      'Ã': Result := Result + 'G';
      'Ø': Result := Result + 'SH';
      'Ù': Result := Result + 'Shch';
      'Ç': Result := Result + 'Z';
      'Õ': Result := Result + 'Kh';
      'Ú': Result := Result + '''';
      'Ô': Result := Result + 'F';
      'Û': Result := Result + 'Y';
      'Â': Result := Result + 'V';
      'À': Result := Result + 'A';
      'Ï': Result := Result + 'P';
      'Ð': Result := Result + 'R';
      'Î': Result := Result + 'O';
      'Ë': Result := Result + 'L';
      'Ä': Result := Result + 'D';
      'Æ': Result := Result + 'Zh';
      'Ý': Result := Result + 'E';
      'ß': Result := Result + 'Ya';
      '×': Result := Result + 'Ch';
      'Ñ': Result := Result + 'S';
      'Ì': Result := Result + 'M';
      'È': Result := Result + 'I';
      'Ò': Result := Result + 'T';
      'Ü': Result := Result + '';
      'Á': Result := Result + 'B';
      'Þ': Result := Result + 'Yu';
      '¸': Result := Result + 'e';
      'é': Result := Result + 'y';
      'ö': Result := Result + 'ts';
      'ó': Result := Result + 'u';
      'ê': Result := Result + 'k';
      'å': Result := Result + 'e';
      'í': Result := Result + 'n';
      'ã': Result := Result + 'g';
      'ø': Result := Result + 'sh';
      'ù': Result := Result + 'shch';
      'ç': Result := Result + 'z';
      'õ': Result := Result + 'kh';
      'ú': Result := Result + '''';
      'ô': Result := Result + 'f';
      'û': Result := Result + 'y';
      'â': Result := Result + 'v';
      'à': Result := Result + 'a';
      'ï': Result := Result + 'p';
      'ð': Result := Result + 'r';
      'î': Result := Result + 'o';
      'ë': Result := Result + 'l';
      'ä': Result := Result + 'd';
      'æ': Result := Result + 'zh';
      'ý': Result := Result + 'e';
      'ÿ': Result := Result + 'ya';
      '÷': Result := Result + 'ch';
      'ñ': Result := Result + 's';
      'ì': Result := Result + 'm';
      'è': Result := Result + 'i';
      'ò': Result := Result + 't';
      'ü': Result := Result + '''';
      'á': Result := Result + 'b';
      'þ': Result := Result + 'yu';
      else Result := Result + S[i];
    end;
  end;
  Result := AnsiReplaceStr(Result, 'YY', 'Y');
  Result := AnsiReplaceStr(Result, 'Yy', 'Y');
  Result := AnsiReplaceStr(Result, 'yy', 'y');
end;

procedure ParseCommand(const CmdLine: string;
  var CmdName, CmdParams: string;
  const Delimiter: Char = #32);
var
  i: Integer;
  CmdFlag: Boolean;
begin
  CmdName := EmptyStr;
  CmdParams := EmptyStr;
  CmdFlag := True;
  for i := 1 to Length(CmdLine) do
    if CmdFlag then
    begin
      if CmdLine[i] = Delimiter
      then CmdFlag := False
      else CmdName := CmdName + CmdLine[i];
    end
    else CmdParams := CmdParams + CmdLine[i];
  CmdName := Trim(CmdName);
  CmdParams := Trim(CmdParams);
end;


function ExtractValueQuoted(const sInStr: string): string;
var
  i, iCount: Integer;
  bValue: Boolean;

  function IsDoubleQuote(const iPos: Integer): Boolean;
  begin
    Result := (iPos < iCount) and (sInStr[iPos] = '"') and (sInStr[iPos + 1] = '"');
  end;

  function IsSingleQuote(const iPos: Integer): Boolean;
  begin
    Result := (iPos <= iCount) and (sInStr[iPos] = '"') and not IsDoubleQuote(iPos);
  end;

begin
  Result := EmptyStr;

  i := 0;
  iCount := Length(sInStr);
  bValue := False;
  while i < iCount do
  begin
    Inc(i);

    if IsSingleQuote(i) then
    begin
      Result := Result + sInStr[i];
      bValue := not bValue;
      if not bValue then
        Break;
    end
    else begin
      if bValue then
      begin
        Result := Result + sInStr[i];
        if IsDoubleQuote(i) then
        begin
          Inc(i);
          Result := Result + sInStr[i];
        end;
      end;
    end;
  end;
end;

function ExtractValueDelimited(const sInStr: string; const fDelims: TCharSet): string;
var
  i, iCount: Integer;
begin
  Result := EmptyStr;

  iCount := Length(sInStr);
  for i := 1 to iCount do
  begin
    if sInStr[i] in fDelims
    then Break
    else Result := Result + sInStr[i];
  end;
end;

end.


