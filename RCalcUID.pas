unit RCalcUID;

interface

uses
  Classes;

function GetProcessorID: string;
function GetWindowsID: string;
function GetComputerID: string;
function GetComputerGUID(const FileName: string; const CanCreateGUID: Boolean): string; overload;
function GetComputerGUID(const CanCreateGUID: Boolean): string; overload;

implementation

uses
  SysUtils, StrUtils, Windows, IniFiles,
  RDialogs, RSysUtils, RWinVer, RNetUtils;

const
  fHostGUIDs    = 'HostGUIDs.ids';
  fHostGUIDe    = 'HostGUIDs.err';

resourcestring
  SErrFormatLine        = '%s: %s (%s)';
  SErrCreateUID         = 'Ошибка генерации GUID!';
  SErrGetWindowsVersion = 'Ошибка чтения версии ОС!';

procedure SaveError(E: Exception; const ExtMsg: string);
var
  sFN: string;
  fT: TextFile;
begin
  try
    sFN := ExtractFilePath(ParamStr(0)) + fHostGUIDe;
    AssignFile(fT, sFN);
    if FileExists(sFN) then Append(fT) else Rewrite(fT);
    try
      WriteLn(fT, Format(SErrFormatLine,
        [DateTimeToStr(Now), ExtMsg, E.Message]));
    finally
      CloseFile(fT);
    end;
  except
  end;
end;

function IsGoodId(const Id: string): Boolean;
var
  i: Integer;
begin
  Result := (Trim(Id) <> EmptyStr) and (Trim(Id) <> 'NULL');
  if Result then
    for i := 1 to Length(Id) do
      if not (Id[i] in ['0'..'9','A'..'Z','a'..'z','.','@','#','$','&','*','-',' ']) then
      begin
        Result := False;
        Break;
      end;
end;

procedure AddId(var ResId: string; const AddId: string);
begin
  if IsGoodId(AddId) then
  begin
    if ResId <> EmptyStr then
      ResId := ResId + '-';
    ResId := ResId + AnsiReplaceText(Trim(AddId), ' ', '-');
  end;
end;

function GetProcessorID: string;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := Format('%x%x%x%x%x%x',
    [SysInfo.wProcessorArchitecture,
     SysInfo.dwActiveProcessorMask,
     SysInfo.dwNumberOfProcessors,
     SysInfo.dwProcessorType,
     SysInfo.wProcessorLevel,
     SysInfo.wProcessorRevision]);
end;

function GetWindowsID: string;
begin
  Result := 'DEFAULT';
  try
    with GetWindowsVersionData do
    begin
      Result := Format('%x%x%x%x%x%x%x%x%x-%x',
        [wProductType, dwPlatformId, dwMajorVersion, dwMinorVersion,
         dwBuildNumber, wSuiteMask,
         Integer(tSysMetrics), dwProductInfo, wProcessorArchitecture,
         dwLanguageId]);
    end;
  except
    on E: Exception do
      SaveError(E, SErrGetWindowsVersion);
  end;
end;

function GetComputerID: string;
begin
  Result := GetProcessorID;
  AddId(Result, AnsiUpperCase(GetMacAddresses(0, '-')));
end;

function GetComputerGUID(const FileName: string; const CanCreateGUID: Boolean): string;
var
  Ini: TIniFile;
  SectionName, ItemName: string;
begin
  try
    SectionName := GetWindowsID;
    ItemName := GetComputerID;
    // InfoBox(FileName + #13 + SectionName + #13 + ItemName);
    Ini := TIniFile.Create(FileName);
    try
      Result := Trim(Ini.ReadString(SectionName, ItemName, EmptyStr));
      if (Result = EmptyStr) and CanCreateGUID then
      begin
        Result := CreateGUID;
        Result := Copy(Result, 2, Length(Result) - 2);
        try
          try
            try
              if Ini.SectionExists(SectionName) then
                Ini.EraseSection(SectionName);
            except
              on E: Exception do
                SaveError(E, SErrCreateUID);
            end;
          finally
            Ini.WriteString(SectionName, ItemName, Result);
          end;
        except
          on E: Exception do
          begin
            Result := EmptyStr;
            SaveError(E, SErrCreateUID);
          end;
        end;
      end;
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
    begin
      Result := EmptyStr;
      SaveError(E, SErrCreateUID);
    end;
  end;
end;

function GetComputerGUID(const CanCreateGUID: Boolean): string;
begin
  Result := GetComputerGUID(IncludeTrailingPathDelimiter(
    GetShellFolderLocation(CSIDL_COMMON_APPDATA)) + fHostGUIDs,
    CanCreateGUID);
  if Result = EmptyStr then
    Result := GetComputerGUID(IncludeTrailingPathDelimiter(
      ExtractFilePath(GetApplicationFileName)) + fHostGUIDs,
      CanCreateGUID);
end;

end.
