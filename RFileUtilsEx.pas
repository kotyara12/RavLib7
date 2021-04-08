unit RFileUtilsEx;

{=======================================================}
{ File operations (extended)                            }
{ Author: Razzhivin Alexander                           }
{ (с) 2008-2009 Rav Soft                                }
{ http:\\ravsoft2004.narod.ru     icq 272712200         }
{=======================================================}

interface

uses
  Classes, SysUtils;

type
  ERFOError        = class (Exception);

  TRFOState        = (rsFind, rsOk, rsError, rsSkip, rsBreak);
  TRFOOption       = (rfForceDirs, rfSrcSubDirs, rfDstDirFix, rfDstDirAuto,
                      rtMoveFile, rfOverWrite, rfCheckSrcAndDst,
                      rtCopyFileDate, rtCopyFileAttr,
                      rfSkipReadOnly, rfSkipSysFiles, rfSkipHiddenFiles,
                      rfCompareDate, rfCompareSize, rfCompareCrc32);
  TRFOOptions      = set of TRFOOption;

  TRFOResult = packed record
    State: TRFOState;
    TimeBegin: TDateTime;
    TimeEnd: TDateTime;
    OperName: string;
    OperResult: string;
  end;

  TRFOLog = array of TRFOResult;

  TRFGState       = (rgOk, rgWarning, rgError);

  TRFGResult = packed record
    State: TRFGState;
    TimeBegin: TDateTime;
    TimeEnd: TDateTime;
    Found: Cardinal;
    Processed: Cardinal;
    Skipped: Cardinal;
    Errors: Cardinal;
    Log: TRFOLog;
  end;

  TCallbackProc    = procedure (const OperationText, Param_1, Param_2: string) of object;
  TProgressProc    = procedure (const MaxValue, CurrValue: Integer) of object;
  TBreakProc       = procedure (var Break: Boolean) of object;

const
  MaskAll          = '*.*';
  MaskDelims       = [';',','];
  rfCopyDefault    = [rtCopyFileDate, rtCopyFileAttr, rfOverWrite, rfSkipReadOnly, rfSkipSysFiles];
  rfCopyDefaultRO  = rfCopyDefault - [rfSkipReadOnly];
  rfCopyCheckDef   = rfCopyDefault + [rfCompareDate, rfCompareSize];
  rfCopyCheckDefRO = rfCopyDefaultRO + [rfCompareDate, rfCompareSize];

function  GetRFOText(const RFOResult: TRFOResult): string;
procedure WaitBreak(const Sec: Integer; const Callback: TCallbackProc;
  const BreakChk: TBreakProc; var OpResult: TRfoResult);
function  FileAttrInsertDirectory(const FileAttr: Integer): Integer;
function  FileAttrRemoveDirectory(const FileAttr: Integer): Integer;
function  CreateDirectoryList(const Directory: TFileName;
  const FileAttr: Integer; const SubDirs: Boolean; DirList: TStrings;
  const Callback: TCallbackProc; const BreakChk: TBreakProc): TRFOResult;
function  CreateFileList(const Directory, Masks: TFileName;
  const FileAttr: Integer; const SubDirs: Boolean; FileList: TStrings;
  const Callback: TCallbackProc; const BreakChk: TBreakProc): TRFOResult;
function  FileExistsMask(const Directory, Masks: TFileName;
  const FileAttr: Integer; const SubDirs: Boolean;
  const Callback: TCallbackProc; const BreakChk: TBreakProc): Boolean;
function  CompareFiles(const FileName1, FileName2: TFileName;
  const CheckFileDate: Boolean): Boolean;
function  CopyFileBuffered(const SrcFile, DstFile: TFileName;
  const Options: TRFOOptions; const BufSizeKb: Word;
  const Callback: TCallbackProc; const BreakChk: TBreakProc): TRFOResult;
function  CopyFileStream(const SrcFile, DstFile: TFileName;
  const Options: TRFOOptions; const Callback: TCallbackProc): TRFOResult;
function FileDelete(const FileName: TFileName; const Options: TRFOOptions;
  const Callback: TCallbackProc): TRFOResult;
function Execute32(const WorkDir, CommandLine: string;
  const ShowWindow: Integer; const BreakExit: Boolean;
  const Callback: TCallbackProc; const BreakChk: TBreakProc): TRFOResult;

function  CopyFilesBufferedEx(const SrcDir, DstDir, Masks: TFileName;
  const Options: TRFOOptions; const BufSizeKb, TryCount, TryInterval: Word; var OpLog: TRFOLog;
  const Callback: TCallbackProc; const Progress: TProgressProc; const BreakChk: TBreakProc): Boolean;
function CopyFilesBuffered(const SrcDir, DstDir, Masks: TFileName;
  const Options: TRFOOptions; const BufSizeKb: Word; var OpLog: TRFOLog;
  const Callback: TCallbackProc; const Progress: TProgressProc; const BreakChk: TBreakProc): Boolean;

implementation

uses
  Windows, StrUtils, RxStrUtils, RCrc32, RSysUtils, RDialogs;

type
  TFileNameArray       = array of TFileName;

resourcestring
  SMsgFileCopy         = 'Копирование файла "%s" -> "%s"';
  SMsgFileMove         = 'Перемещение файла "%s" -> "%s"';
  SMsgFilesCopy        = 'Копирование файла(ов) "%s\%s" -> "%s"';
  SMsgFileDelete       = 'Удаление файла "%s"';
  SMsgFileExec         = 'Вызов приложения "%s"';
  SMsgFileExecAndBreak = 'Вызов приложения "%s" с ожиданием завершения';
  SMsgFindDirs         = 'Просмотр структуры каталогов "%s"';
  SMsgFindFiles        = 'Поиск файлов "%s" в каталоге "%s"';
  SMsgCreateFileList   = 'Создание списка файлов для каталога "%s", маска "%s"';
  SMsgFileCopySkip     = 'Файл "%s" с заданными атрибутами уже существует';
  SMsgFileAttrRO       = 'Файл "%s" имеет атрибут ReadOnly (Только чтение)';
  SMsgFileAttrSF       = 'Файл "%s" имеет атрибут System (Системный)';
  SMsgFileAttrHF       = 'Файл "%s" имеет атрибут Hidden (Скрытый)';
  SMsgTryNumber        = ' (попытка %d)';
  SMsgOperationOk      = 'OK';
  SMsgReturnCode       = 'Код возврата (завершения) - %d';
  SMsgFindDirsRes      = 'Найдено %d каталог(ов)';
  SMsgFindFilesRes     = 'Найдено %d файл(ов) в %d каталогах';
  SMsgCopyFileRes      = 'Успешно скопировано %d байт';
  SMsgMoveFileRes      = 'Успешно перемещено %d байт';
  SMsgBreak            = 'Операция перервана по запросу ОС или пользователя!';
  SMsgWait             = 'Пауза между попытками %d секунд(ы)...';

  SErrSrcFileIsNull    = 'Не указано имя исходного файла!';
  SErrDstFileIsNull    = 'Не указано имя конечного файла!';
  SErrSameCopy         = 'Нельзя копировать файл сам в себя!';
  SErrDirsNotFound     = 'Ошибка: каталог "%s" не найден(ы)!';
  SErrFilesNotFound    = 'Ошибка: файл(ы) "%s" в каталоге "%s" не найден(ы)!';
  SErrForceDirs        = 'Ошибка создания каталога "%s": %s!';
  SErrOpenFile         = 'Ошибка доступа к файлу "%s": %s!';
  SErrCreateFile       = 'Ошибка создания файла "%s": %s!';
  SErrReadFile         = 'Ошибка чтения файла "%s": %s!';
  SErrWriteFile        = 'Ошибка записи файла "%s": %s!';
  SErrDeleteFile       = 'Ошибка удаления файла "%s": %s!';
  SErrCopyFile         = 'Ошибка копирования файла "%s" -> "%s": %s!';
  SErrCopyCheck        = 'Ошибка сравнения файлов: исходный "%s" и конечный "%s" файлы различаются!';
  SErrSetDateFile      = 'Ошибка изменения даты создания файла "%s": %s!';
  SErrSetAttrFile      = 'Ошибка изменения атрибутов файла "%s": %s!';
  SErrExecError        = 'Ошибка вызова внешней команды: %s!';

  SFullRFOText         = '%s: %s.';

function GetRFOText(const RFOResult: TRFOResult): string;
begin
  Result := Format(SFullRFOText, [RFOResult.OperName, RFOResult.OperResult]);
end;

procedure RfoToLog(const RFOResult: TRFOResult; var RFOLog: TRFOLog);
begin
  SetLength(RFOLog, Length(RFOLog) + 1);
  RFOLog[High(RFOLog)] := RFOResult;
end;

function IsBreak(const BreakChk: TBreakProc; var OpResult: TRfoResult): Boolean;
begin
  Result := OpResult.State = rsBreak;
  if not Result then
  begin
    if Assigned(BreakChk) then BreakChk(Result);
    if Result then
    begin
      OpResult.State := rsBreak;
      OpResult.OperResult := SMsgBreak;
      OpResult.TimeEnd := Now;
    end;
  end;
end;

procedure WaitBreak(const Sec: Integer; const Callback: TCallbackProc;
  const BreakChk: TBreakProc; var OpResult: TRfoResult);
const
  MaxDelay = 100;
var
  TotalMs, SingleMs: Integer;
begin
  TotalMs := Sec * 1000;
  if Assigned(Callback) then
    Callback(Format(SMsgWait, [Sec]), IntToStr(Sec), IntToStr(TotalMs));
  while not IsBreak(BreakChk, OpResult) and (TotalMs > 0) do
  begin
    if TotalMs > MaxDelay then SingleMs := MaxDelay else SingleMs := TotalMs;
    Sleep(SingleMs);
    Dec(TotalMs, SingleMs);
  end;
end;

function FileGetSize(const FileName: string): LongInt;
var
  SR: TSearchRec;
begin
  if SysUtils.FindFirst(FileName, faAnyFile, SR) = 0
  then Result := SR.Size
  else Result := -1;
  SysUtils.FindClose(SR);
end;

function FileAttrInsertDirectory(const FileAttr: Integer): Integer;
begin
  Result := FileAttr;
  if (Result and faVolumeID) = 0 then Result := Result + faVolumeID;
  if (Result and faDirectory) = 0 then Result := Result + faDirectory;
end;

function FileAttrRemoveDirectory(const FileAttr: Integer): Integer;
begin
  Result := FileAttr;
  if (Result and faVolumeID) > 0 then Result := Result - faVolumeID;
  if (Result and faDirectory) > 0 then Result := Result - faDirectory;
end;

function CreateDirectoryList(const Directory: TFileName; const FileAttr: Integer;
  const SubDirs: Boolean; DirList: TStrings;
  const Callback: TCallbackProc; const BreakChk: TBreakProc): TRFOResult;
var
  OwnDirList: TFileNameArray;
  i: Integer;

  function DirectoryFind(const DirName: TFileName; var FindRes: TRFOResult): TFileNameArray;
  var
    IntDirList: TFileNameArray;
    CurrPath, CurrName: string;
    FR, i: Integer;
    SR: TSearchRec;

    function IsNetworkServer(const Path: string): Boolean;
    begin
      Result := (Length(Path) > 0) and (Path[1] = PathDelim)
        and (ExtractFilePath(ExcludeTrailingPathDelimiter(Path)) = PathDelim + PathDelim);
    end;

  begin
    SetLength(Result, 0);
    SetLength(IntDirList, 0);
    CurrPath := ExtractFilePath(ExcludeTrailingPathDelimiter(DirName));
    CurrName := ExtractFileName(ExcludeTrailingPathDelimiter(DirName));
    if not IsNetworkServer(CurrPath) and (CurrName <> EmptyStr) then
    begin
      // Поиск по текущей маске каталога
      IntDirList := DirectoryFind(CurrPath, FindRes);
      if not IsBreak(BreakChk, FindRes) and (Length(IntDirList) > 0) then
      begin
        for i := Low(IntDirList) to High(IntDirList) do
        begin
          FR := SysUtils.FindFirst(IncludeTrailingPathDelimiter(IntDirList[i]) + CurrName,
            FileAttrInsertDirectory(FileAttr), SR);
          try
            while (FR = 0) and not IsBreak(BreakChk, FindRes) do
            begin
              if ((SR.Attr and faVolumeID) = 0) and ((SR.Attr and faDirectory) > 0)
              and (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                SetLength(Result, Length(Result) + 1);
                Result[High(Result)] := IncludeTrailingPathDelimiter(IntDirList[i]) + SR.Name;
              end;
              FR := SysUtils.FindNext(SR);
            end;
          finally
            SysUtils.FindClose(SR);
          end;
        end;
      end;
    end
    else begin
      if not IsBreak(BreakChk, FindRes) then
      begin
        // Поиск по всем локальным дискам
        if Pos('*', DirName) = 1 then
        begin
          CurrName := DirName;
          while Pos('*', CurrName) = 1 do
            CurrName := Copy(DirName, 2, Length(DirName) - 1);
          for i := Ord('A') to Ord('Z') do
          begin
            if IsBreak(BreakChk, FindRes) then Break;
            CurrPath := Chr(i) + DriveDelim;
            if GetDriveType(PAnsiChar(CurrPath)) = DRIVE_FIXED then
            begin
              SetLength(Result, Length(Result) + 1);
              Result[High(Result)] := Chr(i) + CurrName;
            end;
          end;
        end
        // Поиск по текущему диску
        else begin
          SetLength(Result, Length(Result) + 1);
          Result[High(Result)] := DirName;
        end;
      end;
    end;
  end;

  procedure DirectoryScan(const DirName: TFileName; var FindRes: TRFOResult);
  var
    FR: Integer;
    SR: TSearchRec;
  begin
    FR := SysUtils.FindFirst(IncludeTrailingPathDelimiter(DirName) + MaskAll,
      FileAttrInsertDirectory(FileAttr), SR);
    try
      while (FR = 0) and not IsBreak(BreakChk, FindRes) do
      begin
        if ((SR.Attr and faVolumeID) = 0) and ((SR.Attr and faDirectory) > 0)
        and (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          DirList.Add(IncludeTrailingPathDelimiter(DirName) + SR.Name);
          if not IsBreak(BreakChk, FindRes) then
            DirectoryScan(IncludeTrailingPathDelimiter(DirName) + SR.Name, FindRes);
        end;
        FR := SysUtils.FindNext(SR);
      end;
    finally
      SysUtils.FindClose(SR);
    end;
  end;

begin
  Result.State := rsOk;
  Result.OperName := Format(SMsgFindDirs, [Directory]);
  Result.TimeBegin := Now;
  SetLength(OwnDirList, 0);
  DirList.BeginUpdate;
  try
    if Assigned(Callback) then Callback(Result.OperName, Directory, EmptyStr);
    DirList.Clear;
    OwnDirList := DirectoryFind(Directory, Result);
    if not IsBreak(BreakChk, Result) then
    begin
      if Length(OwnDirList) > 0 then
      begin
        for i := Low(OwnDirList) to High(OwnDirList) do
        begin
          DirList.Add(OwnDirList[i]);
          if SubDirs and not IsBreak(BreakChk, Result) then
            DirectoryScan(OwnDirList[i], Result);
          if IsBreak(BreakChk, Result) then Break;
        end;
        if not IsBreak(BreakChk, Result) then
        begin
          Result.State := rsFind;
          Result.OperResult := Format(SMsgFindDirsRes, [DirList.Count]);
        end;
      end
      else begin
        Result.State := rsError;
        Result.OperResult := Format(SErrDirsNotFound, [Directory]);
      end;
    end;
  finally
    SetLength(OwnDirList, 0);
    DirList.EndUpdate;
    Result.TimeEnd := Now;
  end;
end;

function CreateFileList(const Directory, Masks: TFileName; const FileAttr: Integer;
  const SubDirs: Boolean; FileList: TStrings;
  const Callback: TCallbackProc; const BreakChk: TBreakProc): TRFOResult;
var
  i, j, F: Integer;
  DirList: TStringList;
  DirFRes: TRfoResult;
  SR: TSearchRec;
begin
  Result.State := rsOk;
  Result.OperName := Format(SMsgFindFiles, [Masks, Directory]);
  Result.TimeBegin := Now;
  DirList := TStringList.Create;
  try
    DirFRes := CreateDirectoryList(Directory, faAnyFile, SubDirs, DirList, Callback, BreakChk);
    if not IsBreak(BreakChk, DirFRes) and (DirList.Count > 0) then
    begin
      for i := 0 to DirList.Count - 1 do
      begin
        if IsBreak(BreakChk, Result) then Break;
        for j := 1 to WordCount(Masks, MaskDelims) do
        begin
          if IsBreak(BreakChk, Result) then Break;
          if Assigned(Callback) then
            Callback(Format(SMsgFindFiles, [ExtractWord(j, Masks, MaskDelims), DirList[i]]),
              Directory, Masks);
          F := SysUtils.FindFirst(IncludeTrailingPathDelimiter(DirList[i]) +
                 ExtractWord(j, Masks, MaskDelims),
                 FileAttrRemoveDirectory(FileAttr), SR);
          try
            while not IsBreak(BreakChk, Result) and (F = 0) do
            begin
              FileList.Add(IncludeTrailingPathDelimiter(DirList[i]) + SR.Name);
              F := SysUtils.FindNext(SR);
            end;
          finally
            SysUtils.FindClose(SR);
          end;
        end;
      end;
      if not IsBreak(BreakChk, Result) then
      begin
        Result.State := rsFind;
        Result.OperResult := Format(SMsgFindFilesRes, [FileList.Count, DirList.Count]);
      end;
    end
    else begin
      Result.State := DirFRes.State;
      Result.OperResult := DirFRes.OperResult;
    end;
  finally
    DirList.Free;
    Result.TimeEnd := Now;
  end;
end;

function FileExistsMask(const Directory, Masks: TFileName; const FileAttr: Integer;
  const SubDirs: Boolean; const Callback: TCallbackProc; const BreakChk: TBreakProc): Boolean;
var
  i, j: Integer;
  DirList: TStringList;
  SR: TSearchRec;
  FR: TRfoResult;
begin
  Result := False;
  DirList := TStringList.Create;
  try
    FR := CreateDirectoryList(Directory, FileAttr, SubDirs, DirList, Callback, BreakChk);
    if Assigned(Callback) then
      Callback(Format(SMsgFindFiles, [Masks, Directory]), Directory, Masks);
    if not IsBreak(BreakChk, FR) and (DirList.Count > 0) then
    begin
      for i := 1 to WordCount(Masks, MaskDelims) do
      begin
        for j := 0 to DirList.Count - 1 do
        begin
          if IsBreak(BreakChk, FR) then Break;
          Result := SysUtils.FindFirst(IncludeTrailingPathDelimiter(DirList[j]) +
            ExtractWord(i, Masks, MaskDelims),
            FileAttrRemoveDirectory(FileAttr), SR) = 0;
          SysUtils.FindClose(SR);
          if Result then Break;
        end;
        if Result or IsBreak(BreakChk, FR) then Break;
      end;
    end;
  finally
    DirList.Free;
  end;
end;

function CompareFiles(const FileName1, FileName2: TFileName; const CheckFileDate: Boolean): Boolean;
begin
  Result := FileExists(FileName1) and FileExists(FileName2)
        and (FileGetSize(FileName1) = FileGetSize(FileName2))
        and (FileCrc32(FileName1) = FileCrc32(FileName2));
  if CheckFileDate then
    Result := Result and (FileAge(FileName1) = FileAge(FileName2));
end;

function CopyFileBufferedInt(const SrcFile, DstFile: TFileName;
  const BufSizeKb: Word; const BreakChk: TBreakProc): Int64;
var
  Buffer: Pointer;
  SrcHandle, DstHandle: Integer;
  BufferSize, FileSize, ReadSize: Int64;

  function Break: Boolean;
  begin
    Result := False;
    if Assigned(BreakChk) then BreakChk(Result);
    if Result then raise ERFOError.Create(SMsgBreak);
  end;

begin
  Result := 0;
  BufferSize := BufSizeKb * 1024;
  FileSize := FileGetSize(SrcFile);
  if (FileSize > 0) and (FileSize < BufferSize) then BufferSize := FileSize;
  GetMem(Buffer, BufferSize);
  try
    SrcHandle := FileOpen(SrcFile, fmOpenRead or fmShareDenyWrite);
    if SrcHandle > 0 then
    begin
      try
        DstHandle := FileCreate(DstFile, fmCreate or fmShareExclusive);
        if DstHandle > 0 then
        begin
          try
            while not Break and (FileSize > 0) do
            begin
              if FileSize > BufferSize
              then ReadSize := BufferSize
              else ReadSize := FileSize;
              if ReadSize <> FileRead(SrcHandle, Buffer^, ReadSize) then
                raise ERFOError.CreateFmt(SErrReadFile,
                  [SrcFile, SysErrorMessage(GetLastError)]);
              if ReadSize = FileWrite(DstHandle, Buffer^, ReadSize) then
              begin
                Dec(FileSize, ReadSize);
                Inc(Result, ReadSize);
              end
              else raise ERFOError.CreateFmt(SErrWriteFile,
                [DstFile, SysErrorMessage(GetLastError)]);
            end;
          finally
            FileClose(DstHandle);
          end;
        end
        else raise ERFOError.CreateFmt(SErrCreateFile,
                     [DstFile, SysErrorMessage(GetLastError)]);
      finally
        FileClose(SrcHandle);
      end;
    end
    else raise ERFOError.CreateFmt(SErrOpenFile,
                 [SrcFile, SysErrorMessage(GetLastError)]);
  finally
    FreeMem(Buffer, BufferSize);
  end;
end;

function CopyFileStreamInt(const SrcFile, DstFile: TFileName): Int64;
var
  SrcStream, DstStream: TFileStream;
begin
  SrcStream := TFileStream.Create(SrcFile, fmOpenRead or fmShareDenyWrite);
  try
    DstStream := TFileStream.Create(DstFile, fmCreate or fmShareExclusive);
    try
      Result := DstStream.CopyFrom(SrcStream, SrcStream.Size);
    finally
      DstStream.Free;
    end;
  finally
    SrcStream.Free;
  end;
end;

function UpdateDstFileName(const SrcFile, DstFile: TFileName): string;
begin
  Result := DstFile;
  if Trim(DstFile) <> EmptyStr then
  begin
    if IsPathDelimiter(DstFile, Length(DstFile))
    or ((FileGetAttr(DstFile) and faVolumeID) > 0)
    or ((FileGetAttr(DstFile) and faDirectory) > 0)
    or (ExtractFileExt(DstFile) = EmptyStr)
    then Result := IncludeTrailingPathDelimiter(DstFile) + ExtractFileName(SrcFile);
  end;
end;

function OffsetDstFileBase(const FileList: TStringList): string;
var
  i: Integer;

  function ComparePath(const Name, Path: string): string;
  begin
    if Path = EmptyStr
    then Result := ExtractFilePath(Name)
    else begin
      while (Length(Result) > 0)
        and not AnsiStartsText(Result, ExtractFilePath(Name)) do
          Result := ExtractFilePath(Result);
    end;
  end;

begin
  Result := EmptyStr;
  for i := 0 to FileList.Count - 1 do
    Result := ComparePath(FileList[i], Result);
  (* Debug: InfoBox('Dynamic offcet = ' + Result); *)
end;

function OffsetDstFileName(const SrcDir, DstDir, SrcFile: string; const FixDstDir: Boolean): string;
var
  i, N: Integer;
  OffsetDir: string;
begin
  OffsetDir := EmptyStr;
  if not FixDstDir then
  begin
    N := 0;
    OffsetDir := IncludeTrailingPathDelimiter(SrcDir);
    for i := 1 to Length(OffsetDir) do
      if OffsetDir[i] = PathDelim then Inc(N);
    if N > 0 then
    begin
      OffsetDir := ExtractFilePath(SrcFile);
      while (Length(OffsetDir) > 0) and (N > 0) do
      begin
        if OffsetDir[1] = PathDelim then Dec(N);
        Delete(OffsetDir, 1, 1);
      end;
    end
    else OffsetDir := EmptyStr;
  end;
  Result := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(DstDir) + OffsetDir);
  (* Debug:
  InfoBox('SrcDir=' + SrcDir + #13 + 'DstDir=' + DstDir + #13 +
          'OffsetDir=' + OffsetDir + #13 +
          'SrcFile=' + SrcFile + #13 + 'DstFile=' + Result);
  *)
end;

function FileCopyPrepare(const SrcFile: TFileName; var DstFile: TFileName; const Options: TRFOOptions;
  var SrcFileDate, SrcFileAttr: Integer; var OpResult: TRFOResult): Boolean;
var
  DstFileAttr: Integer;
  CopySkip: Boolean;

  function CheckCopyAttr(const FileAttr, CheckedAttr: Integer;
    const SkipOnAttrExists: Boolean; const SkipMessage: string;
    var OpResult: TRFOResult): Boolean;
  var
    ResAttr: Integer;
  begin
    Result := False;
    if ((FileAttr and CheckedAttr) = CheckedAttr) then
    begin
      if SkipOnAttrExists then
      begin
        Result := True;
        OpResult.State := rsError;
        OpResult.OperResult := Format(SkipMessage, [DstFile]);
      end
      else begin
        ResAttr := FileSetAttr(DstFile, FileAttr and not CheckedAttr);
        if ResAttr <> 0 then
        begin
          Result := True;
          OpResult.State := rsError;
          OpResult.OperResult := Format(SErrSetAttrFile, [DstFile, SysErrorMessage(ResAttr)]);
        end;
      end;
    end;
  end;

begin
  Result := False;
  // Считываем атирбуты исходного файла
  SrcFileDate := FileAge(SrcFile);
  SrcFileAttr := FileGetAttr(SrcFile);
  // Сравниваем имена файлов
  if AnsiUpperCase(SrcFile) = AnsiUpperCase(DstFile) then
  begin
    OpResult.State := rsError;
    OpResult.OperResult := SErrSameCopy;
    Exit;
  end;
  // Проверяем наличие конечного файла
  if FileExists(DstFile) then
  begin
    // Проверяем разрешение на перезапись файла
    if rfOverWrite in Options then
    begin
      // Сравниваем исходний и конечный файлы
      CopySkip := (rfCompareDate in Options)
               or (rfCompareSize in Options)
               or (rfCompareCrc32 in Options);
      if CopySkip then
      begin
        if (rfCompareDate in Options) then
          CopySkip := CopySkip and (SrcFileDate = FileAge(DstFile));
        if (rfCompareSize in Options) then
          CopySkip := CopySkip and (FileGetSize(SrcFile) = FileGetSize(DstFile));
        if (rfCompareCrc32 in Options) then
          CopySkip := CopySkip and (FileCrc32(SrcFile) = FileCrc32(DstFile));
      end;
      if CopySkip then
      begin
        OpResult.State := rsSkip;
        OpResult.OperResult := Format(SMsgFileCopySkip, [DstFile]);
        Exit;
      end;
    end
    else begin
      OpResult.State := rsSkip;
      OpResult.OperResult := Format(SMsgFileCopySkip, [DstFile]);
      Exit;
    end;
    // Проверяем атрибуты конечного файла
    DstFileAttr := FileGetAttr(SrcFile);
    if CheckCopyAttr(DstFileAttr, faReadOnly,
      rfSkipReadOnly in Options, SMsgFileAttrRO, OpResult) then Exit;
    if CheckCopyAttr(DstFileAttr, faSysFile,
      rfSkipSysFiles in Options, SMsgFileAttrSF, OpResult) then Exit;
    if CheckCopyAttr(DstFileAttr, faHidden,
      rfSkipHiddenFiles in Options, SMsgFileAttrHF, OpResult) then Exit;
  end;
  // Проверяем и, если надо, создаем путь к конечному файлу
  if (Trim(DstFile) <> EmptyStr) and (rfForceDirs in Options)
  and not DirectoryExists(ExtractFilePath(DstFile))
  and not ForceDirectories(ExtractFilePath(DstFile)) then
  begin
    OpResult.State := rsError;
    OpResult.OperResult := Format(SErrForceDirs, [ExtractFilePath(DstFile), SysErrorMessage(GetLastError)]);
    Exit;
  end;
  // Устанавливаем флаги выполнения
  Result := True;
  OpResult.State := rsOk;
  OpResult.OperResult := SMsgOperationOk;
end;

procedure CopyFileAttr(const DstFile: TFileName; const Options: TRFOOptions;
 const SrcFileDate, SrcFileAttr: Integer);
var
  ResAttr: Integer;
begin
  // Копируем время создания файла
  if rtCopyFileDate in Options then
  begin
    ResAttr := FileSetDate(DstFile, SrcFileDate);
    if ResAttr <> 0 then
      raise ERFOError.CreateFmt(SErrSetDateFile,
        [DstFile, SysErrorMessage(ResAttr)]);
  end;
  // Копируем атрибуты файла
  if rtCopyFileAttr in Options then
  begin
    ResAttr := FileSetAttr(DstFile, SrcFileAttr);
    if ResAttr <> 0 then
      raise ERFOError.CreateFmt(SErrSetAttrFile,
        [DstFile, SysErrorMessage(ResAttr)]);
  end;
end;

procedure DeleteSrcFile(const SrcFile: TFileName; const Options: TRFOOptions; var OpResult: TRFOResult);
var
  FileAttr: Integer;

  function CheckDeleteAttr(const FileAttr, CheckedAttr: Integer;
    const SkipOnAttrExists: Boolean; const SkipMessage: string;
    var OpResult: TRFOResult): Boolean;
  var
    ResAttr: Integer;
  begin
    Result := False;
    if ((FileAttr and CheckedAttr) = CheckedAttr) then
    begin
      if SkipOnAttrExists then
      begin
        Result := True;
        OpResult.State := rsError;
        OpResult.OperResult := Format(SkipMessage, [SrcFile]);
      end
      else begin
        ResAttr := FileSetAttr(SrcFile, FileAttr and not CheckedAttr);
        if ResAttr <> 0 then
        begin
          Result := True;
          OpResult.State := rsError;
          OpResult.OperResult := Format(SErrSetAttrFile, [SrcFile, SysErrorMessage(ResAttr)]);
        end;
      end;
    end;
  end;

begin
  FileAttr := FileGetAttr(SrcFile);
  if CheckDeleteAttr(FileAttr, faReadOnly, rfSkipReadOnly in Options, SMsgFileAttrRO, OpResult) then Exit;
  if CheckDeleteAttr(FileAttr, faSysFile, rfSkipSysFiles in Options, SMsgFileAttrSF, OpResult) then Exit;
  if CheckDeleteAttr(FileAttr, faHidden, rfSkipHiddenFiles in Options, SMsgFileAttrHF, OpResult) then Exit;
  if not SysUtils.DeleteFile(SrcFile) then
  begin
    OpResult.State := rsError;
    OpResult.OperResult := Format(SErrDeleteFile, [SrcFile, SysErrorMessage(GetLastError)]);
  end;
end;

function CopyFileBuffered(const SrcFile, DstFile: TFileName; const Options: TRFOOptions;
  const BufSizeKb: Word; const Callback: TCallbackProc; const BreakChk: TBreakProc): TRFOResult;
var
  DstFileName: TFileName;
  SrcFileDate, SrcFileAttr: Integer;
  BytesCopied: Int64;
begin
  // Инициализируем параметры операции
  Result.State := rsOk;
  Result.OperResult := EmptyStr;
  Result.TimeBegin := Now;
  DstFileName := UpdateDstFileName(ExpandFileName(SrcFile), ExpandFileName(DstFile));
  if rtMoveFile in Options
  then Result.OperName := Format(SMsgFileMove, [SrcFile, DstFileName])
  else Result.OperName := Format(SMsgFileCopy, [SrcFile, DstFileName]);
  try
    if Assigned(Callback) then Callback(Result.OperName, SrcFile, DstFileName);
    // Проверяем исходный и конечный файлы, считываем атрибуты
    if not IsBreak(BreakChk, Result)
    and FileCopyPrepare(SrcFile, DstFileName, Options, SrcFileDate, SrcFileAttr, Result) then
    begin
      try
        // Копируем файл и переносим атрибуты
        BytesCopied := CopyFileBufferedInt(SrcFile, DstFileName, BufSizeKb, BreakChk);
        CopyFileAttr(DstFileName, Options, SrcFileDate, SrcFileAttr);
        if rtMoveFile in Options
        then Result.OperResult := Format(SMsgMoveFileRes, [BytesCopied])
        else Result.OperResult := Format(SMsgCopyFileRes, [BytesCopied]);
      except
        on E: Exception do
        begin
          Result.State := rsError;
          Result.OperResult := E.Message;
        end;
      end;
      // Сравниваем исходный и конечный файлы
      if (Result.State = rsOk) and not IsBreak(BreakChk, Result)
      and (rfCheckSrcAndDst in Options) then
      begin
        if not CompareFiles(SrcFile, DstFileName, rtCopyFileDate in Options) then
        begin
          Result.State := rsError;
          Result.OperResult := Format(SErrCopyCheck, [SrcFile, DstFileName]);
          Exit;
        end;
      end;
      // Удаляем исходный файл
      if (Result.State = rsOk) and (rtMoveFile in Options) then
        DeleteSrcFile(SrcFile, Options, Result);
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function CopyFileStream(const SrcFile, DstFile: TFileName; const Options: TRFOOptions;
  const Callback: TCallbackProc): TRFOResult;
var
  DstFileName: TFileName;
  SrcFileDate, SrcFileAttr: Integer;
  BytesCopied: Int64;
begin
  // Инициализируем параметры операции
  Result.State := rsOk;
  Result.OperResult := EmptyStr;
  Result.TimeBegin := Now;
  DstFileName := UpdateDstFileName(ExpandFileName(SrcFile), ExpandFileName(DstFile));
  if rtMoveFile in Options
  then Result.OperName := Format(SMsgFileMove, [SrcFile, DstFileName])
  else Result.OperName := Format(SMsgFileCopy, [SrcFile, DstFileName]);
  try
    if Assigned(Callback) then Callback(Result.OperName, SrcFile, DstFileName);
    // Проверяем исходный и конечный файлы, считываем атрибуты
    if FileCopyPrepare(SrcFile, DstFileName, Options, SrcFileDate, SrcFileAttr, Result) then
    begin
      // Копируем файл
      try
        BytesCopied := CopyFileStreamInt(SrcFile, DstFileName);
        if rtMoveFile in Options
        then Result.OperResult := Format(SMsgMoveFileRes, [BytesCopied])
        else Result.OperResult := Format(SMsgCopyFileRes, [BytesCopied]);
      except
        on E: Exception do
        begin
          Result.State := rsError;
          Result.OperResult := Format(SErrCopyFile, [SrcFile, DstFileName, E.Message]);
        end;
      end;
      // Переносим атрибуты
      if Result.State = rsOk then
      begin
        try
          CopyFileAttr(DstFileName, Options, SrcFileDate, SrcFileAttr);
        except
          on E: Exception do
          begin
            Result.State := rsError;
            Result.OperResult := E.Message;
          end;
        end;
      end;
      // Сравниваем исходный и конечный файлы
      if (Result.State = rsOk) and (rfCheckSrcAndDst in Options) then
      begin
        if not CompareFiles(SrcFile, DstFileName, rtCopyFileDate in Options) then
        begin
          Result.State := rsError;
          Result.OperResult := Format(SErrCopyCheck, [SrcFile, DstFileName]);
          Exit;
        end;
      end;
      // Удаляем исходный файл
      if (Result.State = rsOk) and (rtMoveFile in Options) then
        DeleteSrcFile(SrcFile, Options, Result);
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function CopyFilesBufferedEx(const SrcDir, DstDir, Masks: TFileName;
  const Options: TRFOOptions; const BufSizeKb, TryCount, TryInterval: Word; var OpLog: TRFOLog;
  const Callback: TCallbackProc; const Progress: TProgressProc; const BreakChk: TBreakProc): Boolean;
var
  TryNum, FileNum: Integer;
  FileList: TStringList;
  OffsetBase: string;
  ResPart: TRFOResult;
begin
  Result := False;
  FileList := TStringList.Create;
  try
    ResPart := CreateFileList(SrcDir, Masks, faAnyFile, rfSrcSubDirs in Options, FileList, Callback, BreakChk);
    RfoToLog(ResPart, OpLog);
    if not IsBreak(BreakChk, ResPart) and (FileList.Count > 0) then
    begin
      if rfDstDirAuto in Options
      then OffsetBase := OffsetDstFileBase(FileList)
      else OffsetBase := SrcDir;
      if IsBreak(BreakChk, ResPart) then Exit;
      for FileNum := 0 to FileList.Count - 1 do
      begin
        if IsBreak(BreakChk, ResPart) then Break;
        if Assigned(Progress) then Progress(FileList.Count, FileNum);
        TryNum := 1;
        repeat
          ResPart := CopyFileBuffered(FileList[FileNum],
            OffsetDstFileName(OffsetBase, DstDir, FileList[FileNum], rfDstDirFix in Options),
            Options, BufSizeKb, Callback, BreakChk);
          if TryNum > 1 then ResPart.OperName := ResPart.OperName + Format(SMsgTryNumber, [TryNum]);
          if (ResPart.State = rsError) and (TryNum < TryCount)
          then WaitBreak(TryInterval, Callback, BreakChk, ResPart);
          RfoToLog(ResPart, OpLog);
          Inc(TryNum);
        until (TryNum > TryCount) or (ResPart.State in [rsOk, rsSkip, rsBreak]);
        if not Result and (ResPart.State in [rsOk, rsSkip]) then Result := True;
      end;
    end;
  finally
    FileList.Free;
  end;
end;

function CopyFilesBuffered(const SrcDir, DstDir, Masks: TFileName;
  const Options: TRFOOptions; const BufSizeKb: Word; var OpLog: TRFOLog;
  const Callback: TCallbackProc; const Progress: TProgressProc; const BreakChk: TBreakProc): Boolean;
begin
  Result := CopyFilesBufferedEx(SrcDir, DstDir, Masks, Options, BufSizeKb, 1, 1,
    OpLog, Callback, Progress, BreakChk);
end;

function FileDelete(const FileName: TFileName; const Options: TRFOOptions;
  const Callback: TCallbackProc): TRFOResult;
var
  FileAttr: Integer;

  function CheckDeleteAttr(const FileAttr, CheckedAttr: Integer;
    const SkipOnAttrExists: Boolean; const SkipMessage: string;
    var OpResult: TRFOResult): Boolean;
  var
    ResAttr: Integer;
  begin
    Result := False;
    if ((FileAttr and CheckedAttr) = CheckedAttr) then
    begin
      if SkipOnAttrExists then
      begin
        Result := True;
        OpResult.State := rsError;
        OpResult.OperResult := Format(SkipMessage, [FileName]);
      end
      else begin
        ResAttr := FileSetAttr(FileName, FileAttr and not CheckedAttr);
        if ResAttr <> 0 then
        begin
          Result := True;
          OpResult.State := rsError;
          OpResult.OperResult := Format(SErrSetAttrFile, [FileName, SysErrorMessage(ResAttr)]);
        end;
      end;
    end;
  end;

begin
  Result.State := rsOk;
  Result.TimeBegin := Now;
  Result.OperName := Format(SMsgFileDelete, [FileName]);
  Result.OperResult := EmptyStr;
  try
    if Assigned(Callback) then Callback(Result.OperName, FileName, EmptyStr);
    FileAttr := FileGetAttr(FileName);
    if CheckDeleteAttr(FileAttr, faReadOnly, rfSkipReadOnly in Options, SMsgFileAttrRO, Result) then Exit;
    if CheckDeleteAttr(FileAttr, faSysFile, rfSkipSysFiles in Options, SMsgFileAttrSF, Result) then Exit;
    if CheckDeleteAttr(FileAttr, faHidden, rfSkipHiddenFiles in Options, SMsgFileAttrHF, Result) then Exit;
    if SysUtils.DeleteFile(FileName) then
    begin
      Result.State := rsOk;
      Result.OperResult := SMsgOperationOk;
    end
    else begin
      Result.State := rsError;
      Result.OperResult := Format(SErrDeleteFile, [FileName, SysErrorMessage(GetLastError)]);
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function Execute32(const WorkDir, CommandLine: string;
  const ShowWindow: Integer; const BreakExit: Boolean;
  const Callback: TCallbackProc; const BreakChk: TBreakProc): TRFOResult;
var
  CloseCode: Integer;
begin
  Result.State := rsOk;
  Result.TimeBegin := Now;
  try
    if BreakExit
    then Result.OperName := Format(SMsgFileExecAndBreak, [CommandLine])
    else Result.OperName := Format(SMsgFileExec, [CommandLine]);
    if Assigned(Callback) then Callback(Result.OperName, WorkDir, CommandLine);
    try
      CloseCode := WinExec32(WorkDir, CommandLine, ShowWindow, BreakExit, nil, 0);
      Result.OperResult := Format(SMsgReturnCode, [CloseCode]);
      if CloseCode < 0 then
      begin
        Result.State := rsError;
        Result.OperResult := Format(SErrExecError, [SysErrorMessage(- CloseCode)]);
      end;
    except
      on E: Exception do
      begin
        Result.State := rsError;
        Result.OperResult := Format(SErrExecError, [E.Message]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

end.
