unit RVarList;

interface

uses
  Controls, Classes;

const
  VarComputerName      = 'ComputerName';
  VarUserName          = 'UserName';
  VarSystemRoot        = 'SystemRoot';
  VarAppDataAllUsers   = 'AppData_AllUsers';
  VarAppDataCurrUsers  = 'AppData_CurrUser';
  VarDesktopAllUsers   = 'Desktop_AllUsers';
  VarDesktopCurrUsers  = 'Desktop_CurrUser';
  VarProgramsAllUsers  = 'Programs_AllUsers';
  VarProgramsCurrUsers = 'Programs_CurrUser';
  VarStartupAllUsers   = 'Startup_AllUsers';
  VarStartupCurrUsers  = 'Startup_CurrUser';
  VarWindowsName       = 'WindowsName';
  VarWindowsVersion    = 'WindowsVersion';

  iniVariables         = 'VARIABLES';

var
  VarList: TStrings;

{ == Добавление переменной в список ============================================ }
procedure AddVariable(const VarName, VarValue: string);
procedure AddVariableList(Vars: TStrings; const VarName, VarValue: string);
{ == Получение переменной по имени ============================================= }
function  GetVariable(const VarName: string; const VDate: TDateTime = 0): string;
function  GetVariableList(Vars: TStrings; const VarName: string; const VDate: TDateTime = 0): string;
{ == Добавление стандратных переменных ========================================= }
procedure AddStandardVariables(const AddSystemFolders: Boolean);
procedure AddStandardVariablesList(Vars: TStrings; const AddSystemFolders: Boolean);
{ == Считывание переменных из INI файла ======================================== }
procedure ReadIniVariables(const FileName: string);
procedure ReadIniVariablesList(Vars: TStrings; const FileName: string);
{ == Замена тегов в строке из списка переменных ================================ }
function  ReplaceTags(const Source: string; const VDate: TDateTime = 0): string;
function  ReplaceTagsList(Vars: TStrings; const Source: string; const RaiseError: Boolean;
  const VDate: TDateTime = 0): string;
{ == Замена тегов в списке строк =============================================== }
procedure ReplaceList(List: TStrings; const VDate: TDateTime = 0);
procedure ReplaceListList(Vars: TStrings; List: TStrings; const VDate: TDateTime = 0);

implementation

uses
  SysUtils, RSysUtils, RWinVerEx, RIniExt;

resourcestring
  ETagNotFound = 'Тэг ''%%%s%%'' не найден в списке переменных!';

{ == Добавление переменной в список ============================================ }
procedure AddVariableList(Vars: TStrings; const VarName, VarValue: string);
begin
  if Vars.IndexOfName(VarName) = -1 then
    Vars.Add(VarName + '=' + VarValue)
  else
    Vars.Values[VarName] := VarValue;
end;

procedure AddVariable(const VarName, VarValue: string);
begin
  AddVariableList(VarList, VarName, VarValue);
end;

{ == Получение переменной по имени ============================================= }
function GetVariableList(Vars: TStrings; const VarName: string; const VDate: TDateTime = 0): string;
begin
  if Vars.IndexOfName(VarName) > -1 then
    Result := ReplaceTagsList(Vars, Vars.Values[VarName], True, VDate)
  else
    Result := EmptyStr;
end;

function GetVariable(const VarName: string; const VDate: TDateTime = 0): string;
begin
  Result := GetVariableList(VarList, VarName, VDate);
end;

{ == Добавление стандартных переменных ========================================= }
procedure AddStandardVariablesList(Vars: TStrings; const AddSystemFolders: Boolean);
begin
  AddVariableList(Vars, VarSystemRoot, GetWindowsDir);
  AddVariableList(Vars, VarComputerName, GetComputerNetName);
  AddVariableList(Vars, VarUserName, GetCurrentUserName);
  AddVariableList(Vars, VarWindowsName, GetWindowsVersionAbbr(GetWindowsVersionInfo));
  AddVariableList(Vars, VarWindowsVersion, GetWindowsVersionName(GetWindowsVersionInfo));
  // AddVariableList(Vars, VarWindowsName, GetWindowsName);
  // AddVariableList(Vars, VarWindowsVersion, GetWindowsVersionName);
  if AddSystemFolders then
  begin
    AddVariableList(Vars, VarDesktopAllUsers, GetCommonSystemFolder(KeyCommonDesktop));
    AddVariableList(Vars, VarDesktopCurrUsers, GetUserSystemFolder(KeyUserDesktop));
    AddVariableList(Vars, VarAppDataAllUsers, GetCommonSystemFolder(KeyCommonAppData));
    AddVariableList(Vars, VarAppDataCurrUsers, GetUserSystemFolder(KeyUserAppData));
    AddVariableList(Vars, VarProgramsAllUsers, GetCommonSystemFolder(KeyCommonPrograms));
    AddVariableList(Vars, VarProgramsCurrUsers, GetUserSystemFolder(KeyUserPrograms));
    AddVariableList(Vars, VarStartupAllUsers, GetCommonSystemFolder(KeyCommonStartup));
    AddVariableList(Vars, VarStartupCurrUsers, GetUserSystemFolder(KeyUserStartup));
  end;
end;

procedure AddStandardVariables(const AddSystemFolders: Boolean);
begin
  AddStandardVariablesList(VarList, AddSystemFolders);
end;

{ == Считывание переменных из INI файла ======================================== }
procedure ReadIniVariablesList(Vars: TStrings; const FileName: string);
begin
  ReadSectionVars(FileName, iniVariables, Vars);
end;

procedure ReadIniVariables(const FileName: string);
begin
  ReadIniVariablesList(VarList, FileName);
end;

{ == Замена тегов в строке из произвольного списка ============================= }
function ReplaceTagsList(Vars: TStrings; const Source: string;
  const RaiseError: Boolean; const VDate: TDateTime = 0): string;
const
  TagsChar     = '%';
  DateTag      = 'DATE';
  DateChar     = ':';
var
  i, v, DatePos: Integer;
  Tagged, Repl: Boolean;
  Tag, Rep: string;
begin
  Result := EmptyStr;
  Tag := EmptyStr;
  Tagged := False;
  for i := 1 to Length(Source) do
  begin
    if Source[i] = TagsChar then
    begin
      if Tagged then
      begin
        Tagged := False;
        if Tag = EmptyStr then
        begin
          // Два символа подряд
          Rep := TagsChar;
          Repl := True;
        end
        else begin
          Repl := False;
          Rep := TagsChar + Tag + TagsChar;
          Tag := AnsiUpperCase(Tag);
          // Вставляем дату
          DatePos := Pos(DateTag, Tag);
          if DatePos = 1 then
          begin
            DatePos := DatePos + 4;
            if Tag[DatePos] = DateChar then DatePos := DatePos + 1;
            if VDate = 0
            then Rep := FormatDateTime(Copy(Tag, DatePos, Length(Tag) - DatePos + 1), Now)
            else Rep := FormatDateTime(Copy(Tag, DatePos, Length(Tag) - DatePos + 1), VDate);
            Repl := True;
          end;
         // "Постоянные" теги
          if Assigned(Vars) then
          begin
            for v := 0 to Vars.Count - 1 do
              if Tag = AnsiUpperCase(Vars.Names[v]) then
              begin
                Rep := ReplaceTagsList(Vars, Vars.Values[Vars.Names[v]], RaiseError, VDate);
                Repl := True;
                Break;
              end;
          end;
        end;
        Result := Result + Rep;
        if RaiseError and not Repl then raise Exception.CreateFmt(ETagNotFound, [Tag]);
      end
      else begin
        Tag := EmptyStr;
        Tagged := True;
      end;
    end
    else begin
      if Tagged
      then Tag := Tag + Source[i]
      else Result := Result + Source[i];
    end;
  end;
end;

{ == Замена тегов в строке из списка переменных ================================ }
function ReplaceTags(const Source: string; const VDate: TDateTime = 0): string;
begin
  Result := ReplaceTagsList(VarList, Source, True, VDate);
end;

{ == Замена тегов в списке строк =============================================== }
procedure ReplaceListList(Vars: TStrings; List: TStrings; const VDate: TDateTime = 0);
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do
    List[i] := ReplaceTagsList(Vars, List[i], True, VDate);
end;

procedure ReplaceList(List: TStrings; const VDate: TDateTime = 0);
begin
  ReplaceListList(VarList, List, VDate);
end;

initialization
  VarList := TStringList.Create;

finalization
  VarList.Free;

end.
