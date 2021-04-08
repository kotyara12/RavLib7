unit RFileUtils;

interface

uses
  Classes, Controls, ComCtrls, SysUtils;

type
  TScanFlag  = (csDirectory, csSourceNotExists, csDestNotExists, csDestReadOnly,
    csDateNewFile, csDateEquals, csSizeEquals);
  TScanFlags = set of TScanFlag;

  TDateTimeCompareMode = (cmLess, cmLessOrEq, cmMore, cmMoreOrEq);

type
  PFileData = ^RFileData;
  RFileData = record
    FullName: string;
    OffsetName: string;
    Directory: Boolean;
  end;

  PScanData = ^RScanData;
  RScanData = record
    SourceFile: string;
    SourceOffset: string;
    DestinationFile: string;
    Flags: TScanFlags;
  end;

const
  AnyFilesMask = '*.*';
  MaskDelims   = [';'];
  FlagsDelim   = ';';
  SScanFlags   : array [TScanFlag] of string = (
    'DIR', 'NO-SRC', 'NO-DST', 'DST-RO', 'NEW-SRC', 'DATE-EQ', 'SIZE-EQ');

{ == Получение даты и времени создания файла =================================== }
function  FileDateTime(const FileName: string): TDateTime;
function  SetFileDateTime(const FileName: string; const NewDate: TDateTime): Integer;
{ == Получение размера файла =================================================== }
function  GetFileSize(const FileName: string): LongInt;
{ == Сравнение атрибутов файлов ================================================ }
function  CompareTimeFiles(const FileName1, FileName2: string): Integer;
{ == Проверка наличия атрибутов файла ========================================== }
function  HasAttr(const FileName: string; Attr: Integer): Boolean;
{ == Копирование файла ========================================================= }
procedure CopyFile(const FileName, DestName: string; OverwriteReadOnly: Boolean;
  const SourceMode: Cardinal = fmOpenRead or fmShareDenyWrite;
  const TargetMode: Cardinal = fmCreate or fmShareExclusive);
procedure RepeatCopyFile(const FileName, DestName: string; OverwriteReadOnly: Boolean;
  const RepeatOnError: Integer);
{ == Очистка каталога ========================================================== }
function  ClearDir(const Path: string; Delete: Boolean): Boolean;

{ == Поиск файлов в каталоге и подкаталогах ==================================== }
procedure FindFiles(const BaseDir, SourceDir, Mask: string;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
procedure MultiFindFiles(const BaseDir, SourceDir, Masks: string;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);

{ == Поиск файлов по дате ====================================================== }
procedure FindFilesOnTimeCreate(const BaseDir, SourceDir, Mask: string;
  const BaseDate: TDateTime; const CompareMode: TDateTimeCompareMode;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
procedure MultiFindFilesOnTimeCreate(const BaseDir, SourceDir, Masks: string;
  const BaseDate: TDateTime; const CompareMode: TDateTimeCompareMode;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);

{ == Сравнение версий файлов =================================================== }
function GetScanFlags(const SrcFile, TrgFile: string): TScanFlags;
{ == Преобразование набора флагов в строку ===================================== }
function ScanFlagsToStr(const Flags: TScanFlags): string;
{ == Получение флагов копирования файла ======================================== }
function ScanFileFile(const BaseDir, SrcFile, DestFile: string): RScanData;
function ScanFileDir(const BaseDir, SrcFile, DestDir: string): RScanData;
function ScanFileAuto(const BaseDir, SrcFile, Destination: string): RScanData;
function ScanFileOffset(const FullName, OffsetName, DestDir: string): RScanData;
{ == Сканирование каталогов и составление списка файлов ======================== }
procedure ScanDirectories(const BaseDir, SourceDir, DestDir, Mask: string;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
procedure MultiScanDirectories(const BaseDir, SourceDir, DestDir, Masks: string;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
{ == Поиск файлов для замены =================================================== }
procedure ScanUpdateFiles(const BaseDir, FindDir, DestDir, Mask: string;
  const SubFind, SubDest: Boolean; FileList: TStrings);
procedure MultiScanUpdateFiles(const BaseDir, FindDir, DestDir, Masks: string;
  const SubFind, SubDest: Boolean; FileList: TStrings);

{ == Проверка на копирование не сущесвующих или только новых файлов ============ }
function IsNullOrNewFile(const Flags: TScanFlags; const CompareSize: Boolean = False): Boolean;
function IsNullOrChangedFile(const Flags: TScanFlags; const CompareDate: Boolean = True;
  const CompareSize: Boolean = True): Boolean;

implementation

uses
  Types, DateUtils, RVclUtils, RxStrUtils, RDialogs;

resourcestring
  SOpenError     = 'Ошибка открытия файла: %s!';
  SFCreateError  = 'Ошибка создания файла: %s!';
  SReadError     = 'Ошибка чтения файла: %s!';
  SWriteError    = 'Ошибка записи в файл: %s!';

{ == Получение даты и времени создания файла =================================== }
function FileDateTime(const FileName: string): TDateTime;
begin
  if FileExists(FileName)
  then Result := FileDateToDateTime(FileAge(FileName))
  else Result := 0;
end;

function  SetFileDateTime(const FileName: string; const NewDate: TDateTime): Integer;
begin
  Result := FileSetDate(FileName, DateTimeToFileDate(NewDate));
end;

{ == Получение размера файла =================================================== }
function GetFileSize(const FileName: string): LongInt;
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0
  then Result := SearchRec.Size
  else Result := -1;
  FindClose(SearchRec);
end;

{ == Сравнение атрибутов файлов ================================================ }
function CompareTimeFiles(const FileName1, FileName2: string): Integer;
var
  Time1, Time2: TDateTime;
begin
  Time1 := FileDateTime(FileName1);
  Time2 := FileDateTime(FileName2);
  if (GetFileSize(FileName1) = GetFileSize(FileName2)) and (Time1 = Time2)
  then Result := 0
  else if Time1 >= Time2 then Result := 1 else Result := -1;
end;

{ == Проверка наличия атрибутов файла ========================================== }
function HasAttr(const FileName: string; Attr: Integer): Boolean;
var
  FileAttr: Integer;
begin
  FileAttr := FileGetAttr(FileName);
  Result := (FileAttr >= 0) and (FileAttr and Attr = Attr);
end;

{ == Копирование файла ========================================================= }
procedure CopyFile(const FileName, DestName: string; OverwriteReadOnly: Boolean;
  const SourceMode: Cardinal = fmOpenRead or fmShareDenyWrite;
  const TargetMode: Cardinal = fmCreate or fmShareExclusive);
const
  ChunkSize: Longint = 3145728; // 3 Mb
var
  CopyBuffer: Pointer;
  Source, Dest: Integer;
  Destination: TFileName;
  BytesCopied: Longint;
  Attr: Integer;
begin
  Destination := DestName;
  if HasAttr(Destination, faDirectory) then
    Destination := IncludeTrailingPathDelimiter(Destination) + ExtractFileName(FileName);
  GetMem(CopyBuffer, ChunkSize);
  try
    Source := FileOpen(FileName, SourceMode);
    if Source < 0 then
      raise EFOpenError.CreateFmt(SOpenError, [FileName]);
    try
      ForceDirectories(ExtractFilePath(Destination));
      if OverwriteReadOnly then begin
        Attr := FileGetAttr(Destination);
        if (Attr >= 0) and ((Attr and faReadOnly) <> 0) then
          FileSetAttr(Destination, Attr and not faReadOnly);
      end;
      Dest := FileCreate(Destination, TargetMode);
      if Dest < 0 then
        raise EFCreateError.CreateFmt(SFCreateError, [Destination]);
      try
        repeat
          BytesCopied := FileRead(Source, CopyBuffer^, ChunkSize);
          if BytesCopied = -1 then
            raise EReadError.CreateFmt(SReadError, [FileName]);
          if BytesCopied > 0 then begin
            if FileWrite(Dest, CopyBuffer^, BytesCopied) = -1 then
              raise EWriteError.CreateFmt(SWriteError, [Destination]);
          end;
        until BytesCopied < ChunkSize;
        FileSetDate(Dest, FileGetDate(Source));
      finally
        FileClose(Dest);
      end;
    finally
      FileClose(Source);
    end;
  finally
    FreeMem(CopyBuffer, ChunkSize);
  end;
end;

procedure RepeatCopyFile(const FileName, DestName: string; OverwriteReadOnly: Boolean;
  const RepeatOnError: Integer);
var
  Errors: Integer;
  Copyed: Boolean;
begin
  Errors := 0;
  repeat
    try
      CopyFile(FileName, DestName, OverwriteReadOnly);
      Copyed := True;
    except
      Copyed := False;
      if Errors < RepeatOnError then begin
        Inc(Errors);
        Delay(1000);
      end
      else raise;
    end;
  until Copyed;
end;

{ == Очистка каталога ========================================================== }
function ClearDir(const Path: string; Delete: Boolean): Boolean;
const
  FileNotFound = 18;
var
  FileInfo: TSearchRec;
  DosCode: Integer;
begin
  Result := DirectoryExists(Path);
  if not Result then Exit;
  DosCode := FindFirst(IncludeTrailingPathDelimiter(Path) + AnyFilesMask, faAnyFile, FileInfo);
  try
    while DosCode = 0 do begin
      if (FileInfo.Name[1] <> '.') and (FileInfo.Attr <> faVolumeID) then
      begin
        if (FileInfo.Attr and faDirectory = faDirectory) then
          Result := ClearDir(IncludeTrailingPathDelimiter(Path) + FileInfo.Name, Delete) and Result
        else if (FileInfo.Attr and faVolumeID <> faVolumeID) then begin
          if (FileInfo.Attr and faReadOnly = faReadOnly) then
            FileSetAttr(IncludeTrailingPathDelimiter(Path) + FileInfo.Name, faArchive);
          Result := DeleteFile(IncludeTrailingPathDelimiter(Path) + FileInfo.Name) and Result;
        end;
      end;
      DosCode := FindNext(FileInfo);
    end;
  finally
    FindClose(FileInfo);
  end;
  if Delete and Result and (DosCode = FileNotFound) and
    not ((Length(Path) = 2) and (Path[2] = ':')) then
  begin
    Result := RemoveDir(Path);
  end;
end;

{ == Вычисление смещения файла относительно базового каталога ================== }
function GetOffcetName(const FullName, BaseDir: string): string;
begin
  if (BaseDir <> EmptyStr) and (Pos(BaseDir, FullName) = 1)
  then Result := Copy(FullName,
    Length(IncludeTrailingPathDelimiter(BaseDir)) + 1,
    Length(FullName) - Length(IncludeTrailingPathDelimiter(BaseDir)))
  else Result := ExtractFileName(FullName);
end;

{ == Поиск файлов в каталоге и подкаталогах ==================================== }
procedure FindFiles(const BaseDir, SourceDir, Mask: string;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
var
  N: Integer;
  SR: TSearchRec;
  SD: PFileData;
begin
  N := FindFirst(IncludeTrailingPathDelimiter(SourceDir) + AnyFilesMask, faAnyFile, SR);
  try
    while N = 0 do
    begin
      if (SR.Name[1] <> '.') and (SR.Attr <> faVolumeID) then begin
        if (faDirectory and SR.Attr) = faDirectory then begin
          // Каталог
          if not OnlyFiles then
          begin
            New(SD);
            SD^.FullName := IncludeTrailingPathDelimiter(SourceDir) + SR.Name;
            SD^.OffsetName := GetOffcetName(SD^.FullName, BaseDir);
            SD^.Directory := True;
            FileList.AddObject(SR.Name, TObject(SD));
          end;
          // Рекурсивная обработка
          if SubDirs then FindFiles(BaseDir,
            IncludeTrailingPathDelimiter(SourceDir) + SR.Name,
              Mask, SubDirs, OnlyFiles, FileList);
        end
        else begin
          // Файл
          if ((SR.Attr and faVolumeID) <> faVolumeID)
          and IsWild(SR.Name, Mask, True) then begin
            New(SD);
            SD^.FullName := IncludeTrailingPathDelimiter(SourceDir) + SR.Name;
            SD^.OffsetName := GetOffcetName(SD^.FullName, BaseDir);
            SD^.Directory := False;
            FileList.AddObject(SR.Name, TObject(SD));
          end;
        end;
      end;
      N := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

procedure MultiFindFiles(const BaseDir, SourceDir, Masks: string;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
var
  i: Integer;
begin
  for i := 1 to WordCount(Masks, MaskDelims) do
    FindFiles(BaseDir, SourceDir, Trim(ExtractWord(i, Masks, MaskDelims)),
      SubDirs, OnlyFiles, FileList);
end;

{ == Поиск файлов по дате ====================================================== }
procedure FindFilesOnTimeCreate(const BaseDir, SourceDir, Mask: string;
  const BaseDate: TDateTime; const CompareMode: TDateTimeCompareMode;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
var
  N: Integer;
  SR: TSearchRec;
  SD: PFileData;

  function CompareDates(const FileDate: TDateTime): Boolean;
  begin
    Result := False;
    case CompareMode of
      cmLess:
        Result := CompareDateTime(BaseDate, FileDate) = GreaterThanValue;
      cmLessOrEq:
        Result := CompareDateTime(BaseDate, FileDate) <> LessThanValue;
      cmMore:
        Result := CompareDateTime(BaseDate, FileDate) = LessThanValue;
      cmMoreOrEq:
        Result := CompareDateTime(BaseDate, FileDate) <> GreaterThanValue;
    end;
  end;

begin
  N := FindFirst(IncludeTrailingPathDelimiter(SourceDir) + AnyFilesMask, faAnyFile, SR);
  try
    while N = 0 do
    begin
      if (SR.Name[1] <> '.') and (SR.Attr <> faVolumeID) then begin
        if (faDirectory and SR.Attr) = faDirectory then begin
          // Каталог
          if not OnlyFiles
          and CompareDates(FileDateToDateTime(SR.Time)) then
          begin
            New(SD);
            SD^.FullName := IncludeTrailingPathDelimiter(SourceDir) + SR.Name;
            SD^.OffsetName := GetOffcetName(SD^.FullName, BaseDir);
            SD^.Directory := True;
            FileList.AddObject(SR.Name, TObject(SD));
          end;
          // Рекурсивная обработка
          if SubDirs then FindFilesOnTimeCreate(BaseDir,
            IncludeTrailingPathDelimiter(SourceDir) + SR.Name,
            Mask, BaseDate, CompareMode, SubDirs, OnlyFiles, FileList);
        end
        else begin
          // Файл
          if ((SR.Attr and faVolumeID) <> faVolumeID)
          and IsWild(SR.Name, Mask, True)
          and CompareDates(FileDateToDateTime(SR.Time)) then begin
            New(SD);
            SD^.FullName := IncludeTrailingPathDelimiter(SourceDir) + SR.Name;
            SD^.OffsetName := GetOffcetName(SD^.FullName, BaseDir);
            SD^.Directory := False;
            FileList.AddObject(SR.Name, TObject(SD));
          end;
        end;
      end;
      N := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

procedure MultiFindFilesOnTimeCreate(const BaseDir, SourceDir, Masks: string;
  const BaseDate: TDateTime; const CompareMode: TDateTimeCompareMode;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
var
  i: Integer;
begin
  for i := 1 to WordCount(Masks, MaskDelims) do
    FindFilesOnTimeCreate(BaseDir, SourceDir, Trim(ExtractWord(i, Masks, MaskDelims)),
      BaseDate, CompareMode, SubDirs, OnlyFiles, FileList);
end;

{ == Сравнение версий файлов =================================================== }
function GetScanFlags(const SrcFile, TrgFile: string): TScanFlags;
begin
  Result := [];
  // Проверка наличия исходного файла
  if (SrcFile = EmptyStr) or not FileExists(SrcFile)
  then Include(Result, csSourceNotExists);
  // Проверка наличия целевого файла
  if (TrgFile = EmptyStr) or not FileExists(TrgFile)
  then Include(Result, csDestNotExists);
  // Если файлы существуют, устанавливаем другие флаги
  if not ((csSourceNotExists in Result) or (csDestNotExists in Result)) then
  begin
    // Установка флага ReadOnly
    if (FileGetAttr(TrgFile) and faReadOnly) <> 0 then
      Include(Result, csDestReadOnly);
    // Проверка времени создания файла
    if CompareDateTime(FileDateTime(SrcFile), FileDateTime(TrgFile)) = GreaterThanValue then
      Include(Result, csDateNewFile);
    if CompareDateTime(FileDateTime(SrcFile), FileDateTime(TrgFile)) = EqualsValue then
      Include(Result, csDateEquals);
    // Проверка размера файла
    if GetFileSize(SrcFile) = GetFileSize(TrgFile) then
      Include(Result, csSizeEquals);
  end;
end;

{ == Преобразование набора флагов в строку ===================================== }
function ScanFlagsToStr(const Flags: TScanFlags): string;
begin
  Result := EmptyStr;
  if csDirectory in Flags then Result := Result + FlagsDelim + SScanFlags[csDirectory];
  if csSourceNotExists in Flags then Result := Result + FlagsDelim + SScanFlags[csSourceNotExists];
  if csDestNotExists in Flags then Result := Result + FlagsDelim + SScanFlags[csDestNotExists];
  if csDestReadOnly in Flags then Result := Result + FlagsDelim + SScanFlags[csDestReadOnly];
  if csDateNewFile in Flags then Result := Result + FlagsDelim + SScanFlags[csDateNewFile];
  if csDateEquals in Flags then Result := Result + FlagsDelim + SScanFlags[csDateEquals];
  if csSizeEquals in Flags then Result := Result + FlagsDelim + SScanFlags[csSizeEquals];
  if Result <> EmptyStr then Result := Copy(Result, 2, Length(Result) - 1);
end;

{ == Получение флагов копирования файла ======================================== }
function ScanFileFile(const BaseDir, SrcFile, DestFile: string): RScanData;
begin
  Result.SourceFile := SrcFile;
  Result.SourceOffset := GetOffcetName(SrcFile, BaseDir);
  Result.DestinationFile := DestFile;
  Result.Flags := GetScanFlags(Result.SourceFile, Result.DestinationFile);
end;

function ScanFileDir(const BaseDir, SrcFile, DestDir: string): RScanData;
begin
  Result.SourceFile := SrcFile;
  Result.SourceOffset := GetOffcetName(SrcFile, BaseDir);
  Result.DestinationFile := IncludeTrailingPathDelimiter(DestDir) + Result.SourceOffset;
  Result.Flags := GetScanFlags(Result.SourceFile, Result.DestinationFile);
end;

function ScanFileAuto(const BaseDir, SrcFile, Destination: string): RScanData;
begin
  Result.SourceFile := SrcFile;
  Result.SourceOffset := GetOffcetName(SrcFile, BaseDir);
  if ExtractFileExt(Destination) = EmptyStr
  then Result.DestinationFile := IncludeTrailingPathDelimiter(Destination) + Result.SourceOffset
  else Result.DestinationFile := Destination;
  Result.Flags := GetScanFlags(Result.SourceFile, Result.DestinationFile);
end;

function ScanFileOffset(const FullName, OffsetName, DestDir: string): RScanData;
begin
  Result.SourceFile := FullName;
  Result.SourceOffset := OffsetName;
  Result.DestinationFile := IncludeTrailingPathDelimiter(DestDir) + Result.SourceOffset;
  Result.Flags := GetScanFlags(Result.SourceFile, Result.DestinationFile);
end;

{ == Сканирование каталогов и составление списка файлов ======================== }
procedure ScanDirectories(const BaseDir, SourceDir, DestDir, Mask: string;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
var
  N: Integer;
  SR: TSearchRec;
  SD: PScanData;
begin
  N := FindFirst(IncludeTrailingPathDelimiter(SourceDir) + AnyFilesMask, faAnyFile, SR);
  try
    while N = 0 do
    begin
      if (SR.Name[1] <> '.') and (SR.Attr <> faVolumeID) then begin
        if (faDirectory and SR.Attr) = faDirectory then begin
          // Каталог
          if not OnlyFiles then
          begin
            New(SD);
            SD^.SourceFile := IncludeTrailingPathDelimiter(SourceDir) + SR.Name;
            SD^.SourceOffset := GetOffcetName(SD^.SourceFile, BaseDir);
            SD^.DestinationFile := EmptyStr;
            SD^.Flags := [csDirectory];
            FileList.AddObject(SR.Name, TObject(SD));
          end;
          // Рекурсивная обработка
          if SubDirs then ScanDirectories(BaseDir,
            IncludeTrailingPathDelimiter(SourceDir) + SR.Name,
              DestDir, Mask, SubDirs, OnlyFiles, FileList);
        end
        else begin
          // Файл
          if ((SR.Attr and faVolumeID) <> faVolumeID)
          and IsWild(SR.Name, Mask, True) then begin
            New(SD);
            SD^.SourceFile := IncludeTrailingPathDelimiter(SourceDir) + SR.Name;
            SD^.SourceOffset := GetOffcetName(SD^.SourceFile, BaseDir);
            if DestDir <> EmptyStr
            then SD^.DestinationFile := IncludeTrailingPathDelimiter(DestDir) + SD^.SourceOffset
            else SD^.DestinationFile := EmptyStr;
            SD^.Flags := GetScanFlags(SD^.SourceFile, SD^.DestinationFile);
            FileList.AddObject(SR.Name, TObject(SD));
          end;
        end;
      end;
      N := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

procedure MultiScanDirectories(const BaseDir, SourceDir, DestDir, Masks: string;
  const SubDirs, OnlyFiles: Boolean; FileList: TStrings);
var
  i: Integer;
begin
  for i := 1 to WordCount(Masks, MaskDelims) do
    ScanDirectories(BaseDir, SourceDir, DestDir, Trim(ExtractWord(i, Masks, MaskDelims)),
      SubDirs, OnlyFiles, FileList);
end;

{ == Поиск файла в каталоге ==================================================== }
function FileFind(const FindDir, FindFile: string; SubDirs: Boolean): string;
var
  N: Integer;
  SR: TSearchRec;
begin
  Result := EmptyStr;
  // Поиск файла в текущем каталоге
  N := FindFirst(IncludeTrailingPathDelimiter(FindDir) + FindFile, faAnyFile, SR);
  try
    if (N = 0) and ((faDirectory and SR.Attr) = 0) then
      Result := IncludeTrailingPathDelimiter(FindDir) + SR.Name;
  finally
    FindClose(SR);
  end;
  // Если не нашли, пытаемся найти в подкаталогах
  if (Result = EmptyStr) and SubDirs then
  begin
    N := FindFirst(IncludeTrailingPathDelimiter(FindDir) + '*.*', faDirectory, SR);
    try
      while N = 0 do
      begin
        // Рекурсивный вызов для каждого подкаталога
        if ((faDirectory and SR.Attr) > 0)
        and ((SR.Name <> '.') and (SR.Name <> '..')) then
        begin
          Result := FileFind(IncludeTrailingPathDelimiter(FindDir) + SR.Name, FindFile, SubDirs);
          if Result <> EmptyStr then Break;
        end;
        N := FindNext(SR);
      end;
    finally
      FindClose(SR);
    end;
  end;
end;

{ == Поиск файлов для замены =================================================== }
procedure ScanUpdateFiles(const BaseDir, FindDir, DestDir, Mask: string;
  const SubFind, SubDest: Boolean; FileList: TStrings);
var
  N: Integer;
  SR: TSearchRec;
  SD: PScanData;
begin
  N := FindFirst(IncludeTrailingPathDelimiter(DestDir) + AnyFilesMask, faAnyFile, SR);
  try
    while N = 0 do
    begin
      if (faDirectory and SR.Attr) > 0 then
      begin
        // Каталог
        if ((SR.Name <> '.') and (SR.Name <> '..')) and SubDest then
          ScanUpdateFiles(BaseDir, FindDir, IncludeTrailingPathDelimiter(DestDir) + SR.Name,
            Mask, SubFind, SubDest, FileList);
      end
      else if IsWild(SR.Name, Mask, True) then begin
        // Файл
        New(SD);
        // Устанваливаем имя найденного файла
        SD^.DestinationFile := IncludeTrailingPathDelimiter(DestDir) + SR.Name;
        // Вычисляем смещение относительно базового каталога
        if (BaseDir <> EmptyStr) and (Pos(BaseDir, SD^.DestinationFile) = 1)
        then SD^.SourceOffset := Copy(SD^.DestinationFile,
          Length(IncludeTrailingPathDelimiter(BaseDir)) + 1,
          Length(SD^.DestinationFile) - Length(IncludeTrailingPathDelimiter(BaseDir)))
        else SD^.SourceOffset := SR.Name;
        // Ищем файл для замены
        SD^.SourceFile := FileFind(FindDir, SR.Name, SubFind);
        // Установливаем флаги
        SD^.Flags := GetScanFlags(SD^.SourceFile, SD^.DestinationFile);
        // Добавляем строчку в список
        FileList.AddObject(SR.Name, TObject(SD));
      end;
      N := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

procedure MultiScanUpdateFiles(const BaseDir, FindDir, DestDir, Masks: string;
  const SubFind, SubDest: Boolean; FileList: TStrings);
var
  i: Integer;
begin
  for i := 1 to WordCount(Masks, MaskDelims) do
    ScanUpdateFiles(BaseDir, FindDir, DestDir, Trim(ExtractWord(i, Masks, MaskDelims)),
      SubFind, SubDest, FileList);
end;

{ == Проверка на копирование не сущесвующих или только новых файлов ============ }
function IsNullOrNewFile(const Flags: TScanFlags; const CompareSize: Boolean = False): Boolean;
begin
  Result := False;
  // Проверяем наличие исходного файла
  if not ((csSourceNotExists in Flags) or (csDirectory in Flags)) then begin
    // Проверяем, существует ли целевой файл и его время создания
    Result := (csDateNewFile in Flags) or (csDestNotExists in Flags);
    // Если целевой файл существует и время создания совпадает с исходным,
    // проверяем его размер
    if not Result and CompareSize and (csDateEquals in Flags) then
      Result := not (csSizeEquals in Flags);
  end;
end;

function IsNullOrChangedFile(const Flags: TScanFlags;
  const CompareDate: Boolean = True;
  const CompareSize: Boolean = True): Boolean;
begin
  Result := False;
  // Проверяем наличие исходного файла
  if not ((csSourceNotExists in Flags) or (csDirectory in Flags)) then begin
    // Проверяем, существует ли целевой файл
    Result := csDestNotExists in Flags;
    // Проверяем размер файла, если файл существует
    if not Result and CompareSize then Result := not (csSizeEquals in Flags);
    // Проверяем всемя создания файла, если файл существует и размеры одинаковы
    if not Result and CompareDate then Result := not (csDateEquals in Flags);
  end;
end;

end.
