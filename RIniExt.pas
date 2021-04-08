unit RIniExt;

interface

uses
  Classes;

procedure ReadSectionVars(const FileName, Section: string; VarList: TStrings);
procedure ReadSectionStrs(const FileName, Section: string; ScrList: TStrings);
procedure ReadSectionText(const FileName, Section: string; ScrList: TStrings);

implementation

uses
  SysUtils, IniFiles;

procedure ReadSectionVars(const FileName, Section: string; VarList: TStrings);
var
  Ini: TIniFile;
  Sec: TStrings;
  Val: string;
  i: Integer;
begin
  Ini := TIniFile.Create(FileName);
  Sec := TStringList.Create;
  VarList.BeginUpdate;
  try
    // Считываем имена переменных
    Ini.ReadSection(Section, Sec);
    for i := 0 to Sec.Count - 1 do
    begin
      // Считываем значение
      Val := Ini.ReadString(Section, Sec[i], EmptyStr);
      // Проверяем или заменяем значение
      if VarList.IndexOfName(Sec[i]) > -1
      then VarList.Values[Sec[i]] := Val
      else VarList.Add(Sec[i] + '=' + Val);
    end;
  finally
    VarList.EndUpdate;
    Sec.Free;
    Ini.Free;
  end;
end;

procedure ReadSectionStrs(const FileName, Section: string; ScrList: TStrings);
var
  F: TextFile;
  S: string;
  ReadFlag: Boolean;
begin
  ScrList.BeginUpdate;
  try
    ScrList.Clear;
    AssignFile(F, FileName);
    Reset(F);
    try
      ReadFlag := False;
      while not EOF(F) do
      begin
        ReadLn(F, S);
        // Отрезаем комментарии
        S := Trim(S);
        if Pos('//', S) > 0 then S := Trim(Copy(S, 1, Pos('//', S) - 1));
        if (Pos(';', S) = 1) or (Pos('*', S) = 1) then S := EmptyStr;
        // Если не пустая строка, обрабатываем его
        if (S <> EmptyStr) then
        begin
          if ReadFlag then
          begin
            if (S <> EmptyStr) and (S[1] = '[') and (S[Length(S)] = ']')
            then ReadFlag := not ReadFlag
            else ScrList.Add(S);
          end
          else if (AnsiUpperCase(S) = AnsiUpperCase('[' + Section + ']'))
               then ReadFlag := not ReadFlag;
        end;
      end;
    finally
      CloseFile(F);
    end;
  finally
    ScrList.EndUpdate;
  end;
end;

procedure ReadSectionText(const FileName, Section: string; ScrList: TStrings);
var
  F: TextFile;
  S: string;
  ReadFlag: Boolean;
  i: Integer;
begin
  ScrList.BeginUpdate;
  try
    ScrList.Clear;
    AssignFile(F, FileName);
    Reset(F);
    try
      ReadFlag := False;
      while not EOF(F) do
      begin
        ReadLn(F, S);
        if ReadFlag then
        begin
          if AnsiUpperCase(Trim(S)) = '%END' then Break;
          if (S <> EmptyStr) and (S[1] = '[') and (S[Length(S)] = ']')
          then ReadFlag := not ReadFlag
          else ScrList.Add(S);
        end
        else if (AnsiUpperCase(S) = AnsiUpperCase('[' + Section + ']'))
             then ReadFlag := not ReadFlag;
      end;
    finally
      CloseFile(F);
    end;
    for i := ScrList.Count - 1 downto 0 do
      if Trim(ScrList[i]) = EmptyStr then ScrList.Delete(i) else Break;
  finally
    ScrList.EndUpdate;
  end;
end;

end.
