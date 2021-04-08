unit RAdoUtils;

interface

type
  RAdoDbParameters = packed record
    DateFormat: string[32];
    CaseEnabled: Boolean;
    WatchDogTime: Integer;
  end;

  RAdoDbResqueCopy = packed record
    ResqueEnabled: Boolean;
    ResqueMaxCopies: Byte;
    ResqueInterval: Word;
    UseCurrentDir: Boolean;
    UserResqueDir: string;
  end;

const
  ConnSection    = 'DATABASE_CONNECTION';
  ResqSection    = 'DATABASE_RESQUE_COPIES';
  QuoteChar      = '"';
  DelimChar      = ';';
  DelimPart      = '=';
  DefUserName    = 'Admin';
  PasswordMask   = '********************';
  CI_Password    = 'PASSWORD';
  CI_DataSource  = 'DATA SOURCE';
  CI_InitCatalog = 'INITIAL CATALOG';
  CI_UserName    = 'USER ID';
  CI_PersSecInfo = 'PERSIST SECURITY INFO';
  CI_ValueFalse  = 'FALSE';
  CI_DateFormat  = '*Date_Format';
  CI_CaseEnabled = '*Upper_Enabled';
  CI_WatchDog    = '*WatchDog';

function  DefaultDateFormat: string;
function  DefaultDbParameters: RAdoDbParameters;
procedure ParseInputParameters(const InStr: string; var FileName, Parameters: string);
function  IsFileDataSource(const S: string): Boolean;
function  IsLoginPrompt(const S: string): Boolean;
function  ExtractDbName(const ConnStr: string): string;
function  ExtractDbConnectStr(const ConnStr: string): string;
function  ExtractParameterValue(const ConnStr, ParamName: string): string;
function  ExtractDbParameters(const ConnStr: string): RAdoDbParameters;
function  MixConnStrAndParams(const ConnStr: string; const DbPrm: RAdoDbParameters): string;

function  IsPassword(const S: string): Boolean;
function  LoadConnectionString(const FileName: string; const Default: string): string;
procedure SaveConnectionString(const FileName, ConnStr: string);
function  LoadConnectionParams(const FileName: string; const Default: RAdoDbParameters): RAdoDbParameters;
procedure SaveConnectionParams(const FileName: string; const Params: RAdoDbParameters);
function  LoadConnectionRqCopy(const FileName: string): RAdoDbResqueCopy;
procedure SaveConnectionRqCopy(const FileName: string; const Params: RAdoDbResqueCopy);

function  EnableResqueCopy(const FileName: string): Boolean;
function  CreateResqueCopy(const FileName: string; const ForcedCreate, DeleteOldFiles: Boolean): Boolean;
function  RestoreResqueCopy(const FileName: string): Boolean;

implementation


uses
  Classes, SysUtils, IniFiles, DateUtils, RxStrUtils, Dialogs, Consts,
  RStrUtils, RCryptApiEx, RProgress, RDialogs, RMsgTypes, RSysUtils, RFileProcs;

resourcestring
  SMsgCreateResqueCopy   = 'Создание резервной копии базы данных...';
  SMsgRestoreResqueCopy  = 'Восстановление данных из резервной копии...';
  SMsgSelectResqueCopy   = 'Выберите файл резервной копии базы данных';

  SErrCreateResqueCopy   = 'Ошибка создания резервной копии базы данных!'#13#13'%s';
  SErrRestoreResqueCopy  = 'Ошибка восстановления базы данных из резервной копии!'#13#13'%s';
  SErrDataSourceNotFound = 'Файл (каталог) ''%s'' не найден!';
  SErrDeleteResqueCopy   = 'Ошибка удаления устаревшей копии базы данных ''%s'': ''%s''!';
  SErrFindOldResqueCopy  = 'Ошибка удаления устаревшей копии базы данных: бесконечный цикл!';

type
  TResqCopyData = packed record
    sFileName: string;
    dFileTime: TDateTime;
  end;

  TResqCopyList = array of TResqCopyData;

const
  iniDataSource          = 'Data_Source';
  iniDateFormat          = 'Date_Format';
  iniCaseEnabled         = 'Upper_Enabled';
  iniWatchDog            = 'Watchdog_Time';
  iniResqueEnabled       = 'Create_resque_db_copies';
  iniResqueMaxCopies     = 'Max_resque_db_copies';
  iniResqueUseCurrDir    = 'Use_database_directory';
  iniResqueUserDir       = 'User_resque_directory';
  iniResqueInterval      = 'Resque_hours_interval';

  TimeStamp              = '_YYYYMMDD-HHNNSS';

  PwdKey                 = '9AD37383D47F43C5BB9C12D55916AC1B';

  BadChars = [#0..#31, '''', '"', '=', '#', '@', '`', '~'];

function DefaultDateFormat: string;
begin
  Result := Format('''%s''', [ShortDateFormat]);
end;

function DefaultDbParameters: RAdoDbParameters;
begin
  Result.DateFormat := DefaultDateFormat;
  Result.CaseEnabled := False;
  Result.WatchDogTime := 1000;
end;

procedure ParseInputParameters(const InStr: string; var FileName, Parameters: string);
begin
  FileName := Trim(ExtractWord(1, InStr, [#13]));
  Parameters := Trim(ExtractWord(2, InStr, [#13]));
end;

function IsFileDataSource(const S: string): Boolean;
var
  i: Integer;
  DS: string;
begin
  Result := False;
  for i := 1 to WordCount(S, [DelimChar]) - 1 do
    if Pos(CI_DataSource, AnsiUpperCase(ExtractWord(i, S, [DelimChar]))) = 1 then
    begin
      DS := ExtractWord(2, ExtractWord(i, S, [DelimChar]), [DelimPart]);
      Result := FileExists(ExpandFileName(DS));
    end;
end;

function IsLoginPrompt(const S: string): Boolean;
begin
  Result := (ExtractParameterValue(S, CI_UserName) <> EmptyStr)
    and SameText(ExtractParameterValue(S, CI_PersSecInfo), CI_ValueFalse);
end;

function ExtractDbName(const ConnStr: string): string;
var
  i: Integer;
begin
  Result := EmptyStr;
  if IsFileDataSource(ConnStr) then
  begin
    for i := 1 to WordCount(ConnStr, [DelimChar]) - 1 do
      if Pos(CI_DataSource, AnsiUpperCase(ExtractWord(i, ConnStr, [DelimChar]))) = 1 then
        Result := ExtractWord(2, ExtractWord(i, ConnStr, [DelimChar]), [DelimPart]);
  end
  else begin
    for i := 1 to WordCount(ConnStr, [DelimChar]) - 1 do
      if Pos(CI_InitCatalog, AnsiUpperCase(ExtractWord(i, ConnStr, [DelimChar]))) = 1 then
      begin
        Result := ExtractWord(2, ExtractWord(i, ConnStr, [DelimChar]), [DelimPart]);
        Exit;
      end;
    for i := 1 to WordCount(ConnStr, [DelimChar]) - 1 do
      if Pos(CI_DataSource, AnsiUpperCase(ExtractWord(i, ConnStr, [DelimChar]))) = 1 then
        Result := ExtractWord(2, ExtractWord(i, ConnStr, [DelimChar]), [DelimPart]);
  end;
end;

function ExtractDbConnectStr(const ConnStr: string): string;
var
  i: Integer;
  sPart: string;
begin
  Result := EmptyStr;
  for i := 1 to WordCount(ConnStr, [DelimChar]) do
  begin
    sPart := ExtractWord(i, ConnStr, [DelimChar]);
    if Trim(sPart) <> EmptyStr then
    begin
      if (Pos(AnsiUpperCase(CI_DateFormat), AnsiUpperCase(sPart)) = 0)
      and (Pos(AnsiUpperCase(CI_CaseEnabled), AnsiUpperCase(sPart)) = 0)
      and (Pos(AnsiUpperCase(CI_WatchDog), AnsiUpperCase(sPart)) = 0)
      then Result := Result + DelimChar + sPart;
    end;
  end;
  if Length(Result) > 0 then Delete(Result, 1, 1);
end;

function ExtractParameterValue(const ConnStr, ParamName: string): string;
var
  i: Integer;
  PartStr: string;
begin
  Result := EmptyStr;
  for i := 1 to WordCount(ConnStr, [DelimChar]) do
  begin
    PartStr := Trim(ExtractWord(i, ConnStr, [DelimChar]));
    if Pos(AnsiUpperCase(ParamName), AnsiUpperCase(PartStr)) = 1 then
    begin
      Delete(PartStr, 1, Length(ParamName));
      PartStr := Trim(PartStr);
      while PartStr[1] = DelimPart do
        Delete(PartStr, 1, 1);
      Result := Trim(PartStr);
      Break;
    end;
  end;
end;

function ExtractDbParameters(const ConnStr: string): RAdoDbParameters;
var
  i: Integer;
  sPart: string;
begin
  Result.DateFormat := DefaultDateFormat;
  Result.CaseEnabled := False;
  Result.WatchDogTime := 1000;
  for i := 1 to WordCount(ConnStr, [DelimChar]) do
  begin
    sPart := ExtractWord(i, ConnStr, [DelimChar]);
    if Trim(sPart) <> EmptyStr then
    begin
      if Pos(AnsiUpperCase(CI_DateFormat), AnsiUpperCase(sPart)) = 1
      then Result.DateFormat := ExtractWord(2, sPart, [DelimPart]);
      if Pos(AnsiUpperCase(CI_CaseEnabled), AnsiUpperCase(sPart)) = 1
      then Result.CaseEnabled := StrToIntDef(ExtractWord(2, sPart, [DelimPart]), 0) > 0;
      if Pos(AnsiUpperCase(CI_WatchDog), AnsiUpperCase(sPart)) = 1
      then Result.WatchDogTime := StrToIntDef(ExtractWord(2, sPart, [DelimPart]), 1000);
    end;
  end;
end;

function MixConnStrAndParams(const ConnStr: string; const DbPrm: RAdoDbParameters): string;

  function BoolToStr(const V: Boolean): string;
  begin
    if V then Result := '1' else Result := '0';
  end;

begin
  Result := Trim(ConnStr);
  if (Result <> EmptyStr) and (Result[Length(Result)] <> DelimChar)
  then Result := Result + DelimChar;
  Result := Result + CI_DateFormat + DelimPart + DbPrm.DateFormat + DelimChar +
                     CI_CaseEnabled + DelimPart + BoolToStr(DbPrm.CaseEnabled) + DelimChar +
                     CI_WatchDog + DelimPart + IntToStr(DbPrm.WatchDogTime);
end;

function IsPassword(const S: string): Boolean;
begin
  Result := Pos(CI_Password, AnsiUpperCase(S)) > 0;
end;

function IsExtended(const S: string): Boolean;
begin
  Result := SameText(iniDateFormat, S) or SameText(iniCaseEnabled, S) or SameText(iniWatchDog, S);
end;

function LoadConnectionString(const FileName: string; const Default: string): string;
var
  i: Integer;
  Ini: TMemIniFile;
  Val: TStringList;
begin
  Result := EmptyStr;
  Ini := TMemIniFile.Create(FileName);
  try
    if Ini.SectionExists(ConnSection) then
    begin
      Val := TStringList.Create;
      try
        Ini.ReadSection(ConnSection, Val);
        for i := 0 to Val.Count - 1 do
          if not IsExtended(Val[i]) then
          begin
            if IsPassword(Val[i])
            then Result := Result + DelimChar + ReplaceStr(Val[i], '_', #32) + DelimPart
              + QuotedString(DecryptPwd_Text_RsaShaRc2B64_Ex(Ini.ReadString(ConnSection, Val[i], EmptyStr), PwdKey), QuoteChar)
            else Result := Result + DelimChar + ReplaceStr(Val[i], '_', #32) + DelimPart
              + Ini.ReadString(ConnSection, Val[i], EmptyStr);
          end;
      finally
        Val.Free;
      end;
    end;
    if Result = EmptyStr
    then Result := Default
    else Delete(Result, 1, 1);
  finally
    Ini.Free;
  end;
end;

procedure SaveConnectionString(const FileName, ConnStr: string);
var
  i: Integer;
  Ini: TMemIniFile;
  Val: TStringList;
begin
  Ini := TMemIniFile.Create(FileName);
  try
    if Ini.SectionExists(ConnSection) then
    begin
      Val := TStringList.Create;
      try
        Ini.ReadSection(ConnSection, Val);
        for i := 0 to Val.Count - 1 do
          Ini.DeleteKey(ConnSection, Val[i]);
      finally
        Val.Free;
      end;
      Ini.UpdateFile;
    end;
    for i := 1 to WordCount(ConnStr, [DelimChar]) do
    begin
      if IsPassword(ExtractWord(i, ConnStr, [DelimChar]))
      then Ini.WriteString(ConnSection,
        ReplaceStr(ExtractWord(1, ExtractWord(i, ConnStr, [DelimChar]), [DelimPart]), #32, '_'),
          EncryptPwd_Text_RsaShaRc2B64_Ex(ExtractQuotedString(
            ExtractWord(2, ExtractWord(i, ConnStr, [DelimChar]), [DelimPart]), QuoteChar), PwdKey))
      else Ini.WriteString(ConnSection,
        ReplaceStr(ExtractWord(1, ExtractWord(i, ConnStr, [DelimChar]), [DelimPart]), #32, '_'),
        ExtractWord(2, ExtractWord(i, ConnStr, [DelimChar]), [DelimPart]));
    end;
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

function LoadConnectionParams(const FileName: string; const Default: RAdoDbParameters): RAdoDbParameters;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FileName);
  try
    Result.DateFormat := RestoreIncorrectChars(Ini.ReadString(ConnSection,
      iniDateFormat, ReplaceIncorrectChars(Default.DateFormat, BadChars)));
    if Default.CaseEnabled
    then Result.CaseEnabled := Ini.ReadInteger(ConnSection, iniCaseEnabled, 1) > 0
    else Result.CaseEnabled := Ini.ReadInteger(ConnSection, iniCaseEnabled, 0) > 0;
    Result.WatchDogTime := Ini.ReadInteger(ConnSection, iniWatchDog, 1000);
  finally
    Ini.Free;
  end;
end;

procedure SaveConnectionParams(const FileName: string; const Params: RAdoDbParameters);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FileName);
  try
    if Ini.SectionExists(ConnSection) then
    begin
      Ini.WriteString(ConnSection, iniDateFormat,
        ReplaceIncorrectChars(Params.DateFormat, BadChars));
      if Params.CaseEnabled
      then Ini.WriteInteger(ConnSection, iniCaseEnabled, 1)
      else Ini.WriteInteger(ConnSection, iniCaseEnabled, 0);
      Ini.WriteInteger(ConnSection, iniWatchDog, Params.WatchDogTime);
      Ini.UpdateFile;
    end;
  finally
    Ini.Free;
  end;
end;

function LoadConnectionRqCopy(const FileName: string): RAdoDbResqueCopy;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FileName);
  try
    Result.ResqueEnabled := Ini.ReadInteger(ResqSection, iniResqueEnabled, 0) > 0;
    Result.ResqueMaxCopies := Ini.ReadInteger(ResqSection, iniResqueMaxCopies, 3);
    Result.ResqueInterval := Ini.ReadInteger(ResqSection, iniResqueInterval, 24);
    Result.UseCurrentDir := Ini.ReadInteger(ResqSection, iniResqueUseCurrDir, 1) > 0;
    Result.UserResqueDir := Ini.ReadString(ResqSection, iniResqueUserDir, EmptyStr);
  finally
    Ini.Free;
  end;
end;

procedure SaveConnectionRqCopy(const FileName: string; const Params: RAdoDbResqueCopy);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(FileName);
  try
    if Ini.SectionExists(ConnSection) then
    begin
      if Params.ResqueEnabled
      then Ini.WriteInteger(ResqSection, iniResqueEnabled, 1)
      else Ini.WriteInteger(ResqSection, iniResqueEnabled, 0);
      Ini.WriteInteger(ResqSection, iniResqueMaxCopies, Params.ResqueMaxCopies);
      Ini.WriteInteger(ResqSection, iniResqueInterval, Params.ResqueInterval);
      if Params.UseCurrentDir
      then Ini.WriteInteger(ResqSection, iniResqueUseCurrDir, 1)
      else Ini.WriteInteger(ResqSection, iniResqueUseCurrDir, 0);
      Ini.WriteString(ResqSection, iniResqueUserDir, Params.UserResqueDir);
      Ini.UpdateFile;
    end;
  finally
    Ini.Free;
  end;
end;

function EnableResqueCopy(const FileName: string): Boolean;
begin
  Result := IsFileDataSource(LoadConnectionString(FileName, EmptyStr))
    and LoadConnectionRqCopy(FileName).ResqueEnabled;
end;

function CreateResqueName(const sFile, sNewPart: string): string;
begin
  Result := ChangeFileExt(ChangeFileExt(ExtractFileName(sFile), '') + sNewPart,
    ExtractFileExt(sFile));
end;

function CreateResqueList(const sDir, sFile: string): TResqCopyList;
var
  SR: TSearchRec;
  iFound: Integer;
begin
  SetLength(Result, 0);
  iFound := FindFirst(IncludeTrailingPathDelimiter(sDir) + CreateResqueName(sFile, '*'),
    faAnyFile - faDirectory - faVolumeID, SR);
  try
    while iFound = 0 do
    begin
      if not SameText(sFile, IncludeTrailingPathDelimiter(sDir) + SR.Name) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)].sFileName := IncludeTrailingPathDelimiter(sDir) + SR.Name;
        Result[High(Result)].dFileTime := FileTimeToDateTime(SR.FindData.ftLastWriteTime);
      end;
      iFound := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

function CreateResqueCopy(const FileName: string; const ForcedCreate, DeleteOldFiles: Boolean): Boolean;
var
  Ini: TMemIniFile;
  aResqCopyList: TResqCopyList;
  rResqCopyCopy: TRTransaction;
  i, iIndex, iMaxCopies, iCopyInterval: Integer;
  dFixDateFile: TDateTime;
  bSameDir: Boolean;
  fProgress: TFormProgress;
  sFileDb, sDestDir, sDestFile: string;
begin
  Result := True;
  try
    if IsFileDataSource(LoadConnectionString(FileName, EmptyStr)) then
    begin
      // Открываем INI файл
      Ini := TMemIniFile.Create(FileName);
      try
        if Ini.ReadInteger(ResqSection, iniResqueEnabled, 0) > 0 then
        begin
          // Считываем параметры создания резервной копии
          sFileDb := ExpandFileName(Ini.ReadString(ConnSection, iniDataSource, EmptyStr));
          bSameDir := Ini.ReadBool(ResqSection, iniResqueUseCurrDir, True);
          if bSameDir
          then sDestDir := ExtractFilePath(sFileDb)
          else sDestDir := ExpandFileName(Ini.ReadString(ResqSection, iniResqueUserDir, EmptyStr));
          iMaxCopies := Ini.ReadInteger(ResqSection, iniResqueMaxCopies, 3);
          iCopyInterval := Ini.ReadInteger(ResqSection, iniResqueInterval, 24);
          // Проверяем наличие файла базы данных
          if FileExists(sFileDb) then
          begin
            // Считываем данные о резервных копиях
            aResqCopyList := CreateResqueList(sDestDir, sFileDb);
            try
              // Ищем дату последней копии
              dFixDateFile := 0;
              for i := Low(aResqCopyList) to High(aResqCopyList) do
                if not SameFileName(ExtractFileName(sFileDb), ExtractFileName(aResqCopyList[i].sFileName)) then
                begin
                  if (dFixDateFile = 0) or (aResqCopyList[i].dFileTime > dFixDateFile) then
                    dFixDateFile := aResqCopyList[i].dFileTime;
                end;
              // Проверяем необходимость создания копии
              if ForcedCreate or (IncHour(dFixDateFile, iCopyInterval) <= Now) then
              begin
                // Отображаем индикатор прогресса
                fProgress := TFormProgress.Create(nil);
                fProgress.Text.Caption := SMsgCreateResqueCopy;
                fProgress.StopButton := True;
                fProgress.Show;
                fProgress.Update;
                fProgress.BringToFront;
                try
                  // Удаляем устаревшие копии, если необходимо
                  if DeleteOldFiles and (iMaxCopies > 0) then
                  begin
                    while (Length(aResqCopyList) >= iMaxCopies) do
                    begin
                      // Ищем самый старый файл
                      iIndex := -1; dFixDateFile := 0;
                      for i := Low(aResqCopyList) to High(aResqCopyList) do
                      begin
                        if ((dFixDateFile = 0) or (aResqCopyList[i].dFileTime <= dFixDateFile))
                        and not SameFileName(ExtractFileName(sFileDb), ExtractFileName(aResqCopyList[i].sFileName)) then
                        begin
                          iIndex := i;
                          dFixDateFile := aResqCopyList[i].dFileTime;
                        end;
                      end;
                      // Удаляем найденный файл
                      if iIndex > -1 then
                      begin
                        if SysUtils.DeleteFile(aResqCopyList[iIndex].sFileName) then
                        begin
                          // Удаляем файл из списка
                          for i := iIndex to High(aResqCopyList) - 1 do
                            aResqCopyList[i] := aResqCopyList[i + 1];
                          SetLength(aResqCopyList, Length(aResqCopyList) - 1);
                        end
                        else raise Exception.CreateFmt(SErrDeleteResqueCopy,
                          [aResqCopyList[iIndex].sFileName, GetSystemError(True)]);
                      end
                      else raise Exception.Create(SErrFindOldResqueCopy);
                    end;
                  end;
                  // Создаем новую резервную копию
                  sDestFile := IncludeTrailingPathDelimiter(sDestDir) + CreateResqueName(sFileDb, FormatDateTime('_YYYYMMDD-HHNNSS', Now));
                  rResqCopyCopy := CopyFileEx(sFileDb, sDestFile, False,
                    [rfForceDirs, rfDstFixed, rfCheckCopy, rfCopyFileAttr], nil,
                    64, 100, nil, nil, fProgress.FileProgress, fProgress.CheckBreak);
                  if rResqCopyCopy.State in [msError, msBreak] then
                  begin
                    if FileExists(sDestFile) then
                      DeleteFile(sDestFile);
                    raise Exception.Create(rResqCopyCopy.Title + ':'#13 + rResqCopyCopy.Result);
                  end;
                finally
                  fProgress.Free;
                end;
              end;
            finally
              SetLength(aResqCopyList, 0);
            end;
          end
          else raise Exception.CreateFmt(SErrDataSourceNotFound, [sFileDb]);
        end;
      finally
        Ini.Free;
      end;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ErrorBox(Format(SErrCreateResqueCopy, [E.Message]));
    end;
  end;
end;

function RestoreResqueCopy(const FileName: string): Boolean;
var
  Ini: TMemIniFile;
  sFileDb, sResqDir, sResqName, sTempFile: string;
  rResqCopyCopy: TRTransaction;
  fProgress: TFormProgress;
  sCurrDir: string;
begin
  Result := True;
  try
    if IsFileDataSource(LoadConnectionString(FileName, EmptyStr)) then
    begin
      // Открываем INI файл
      Ini := TMemIniFile.Create(FileName);
      try
        sFileDb := ExpandFileName(Ini.ReadString(ConnSection, iniDataSource, EmptyStr));
        if Ini.ReadBool(ResqSection, iniResqueUseCurrDir, True)
        then sResqDir := ExtractFilePath(sFileDb)
        else sResqDir := ExpandFileName(Ini.ReadString(ResqSection, iniResqueUserDir, EmptyStr));
        // Сохраняем текущий рабочий каталог
        GetDir(0, sCurrDir);
        if PromptForFileName(sResqName,
          CreateResqueName(sFileDb, '*') + '|' + CreateResqueName(sFileDb, '*') + '|' + SDefaultFilter,
          ExtractFileExt(sFileDb), SMsgSelectResqueCopy, sResqDir, False) then
        begin
          // Восстанавливаем текущий рабочий каталог
          ChDir(sCurrDir);
          // Создаем резервную копию текущей базы данных
          if CreateResqueCopy(FileName, True, False) then
          begin
            // Отображаем индикатор прогресса
            fProgress := TFormProgress.Create(nil);
            fProgress.Text.Caption := SMsgRestoreResqueCopy;
            fProgress.StopButton := False;
            fProgress.Show;
            fProgress.Update;
            fProgress.BringToFront;
            try
              sTempFile := ChangeFileExt(sFileDb, '.~tmp~');
              SysUtils.RenameFile(sFileDb, sTempFile);
              try
                rResqCopyCopy := CopyFileEx(sResqName, sFileDb, False,
                  [rfOverWrite, rfDstFixed, rfCopyFileDate, rfCopyFileAttr], nil,
                  64, 100, nil, nil, fProgress.FileProgress, fProgress.CheckBreak);
                if rResqCopyCopy.State in [msError, msBreak] then
                begin
                  if FileExists(sTempFile) and FileExists(sFileDb) then
                    DeleteFile(sFileDb);
                  if FileExists(sTempFile) then
                    SysUtils.RenameFile(sTempFile, sFileDb);
                  raise Exception.Create(rResqCopyCopy.Title + ':'#13 + rResqCopyCopy.Result);
                end;
              finally
                if FileExists(sTempFile) then
                  DeleteFile(sTempFile);
              end;
            finally
              fProgress.Free;
            end;
          end;
        end;
      finally
        Ini.Free;
      end;
    end
    else begin
      // Восстанавливаем текущий рабочий каталог
      ChDir(sCurrDir);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      ErrorBox(Format(SErrRestoreResqueCopy, [E.Message]));
    end;
  end;
end;

end.
