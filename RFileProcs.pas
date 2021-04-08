unit RFileProcs;

{===============================================================================}
{ Набор универсальных процедур и функций для работы с файловой системой (ver.3) }
{ (c) 2008-2015 Разживин Александр Валерьевич                                   }
{ http://www.ravsoft2004.narod.ru                                               }
{===============================================================================}

interface

uses
  SysUtils, Classes, Windows, RMsgTypes;

type
  ERfoError       = class (Exception);

  TwSearchRec = record
    Time: Integer;
    Size: Integer;
    Attr: DWord;
    Name: TFileName;
    ExcludeAttr: DWord;
    FindHandle: THandle;
    FindDataW: TWin32FindDataW;
  end;

  TRfoFileFlag     = (rfForceDirs,        // Создать путь назначения при необходимости
                      rfSrcSubDirs,       // Обработать также вложенные подкаталоги
                      rfDstDirAuto,       // Автоматическое вычисление каталога назначения (при задании каталога в виде шаблона)
                                          // При удалении - удалять только файлы (каталоги не трограть)!
                      rfDstFixed,         // Каталог (или файл!!!) назначения - фиксированный (все файлы из разных подкаталогов будут скопированы в один и тот же)
                                          // При удалении - удалять только не пустые каталоги!
                      rfOverWrite,        // Перезаписать существующие файлы
                      rfRepeatOnError,    // Повторить при ошибке заданное количество раз
                      rfCopyBackup,       // Создать резервную копию существующего файла перед копированием
                      rfCopyLocked,       // Заблокированные файлы компировать с переименованием во временный файл
                      rfCheckCopy,        // Сравнить исходный и скопированный файл (CRC32)
                      rfCopyFileDate,     // Переносить дату изменения файла
                      rfCopyFileAttr,     // Переносить атрибуты файла
                      rfSkipReadOnly,     // Пропустить (не обрабатывать) файлы "Только чтение"
                      rfSkipSysFiles,     // Пропустить (не обрабатывать) системные файлы
                      rfSkipHiddenFiles,  // Пропустить (не обрабатывать) скрытые файлы
                      rfCompareDate,      // Перезаписать файлы, отличные по дате изменения
                      rfCompareSize,      // Перезаписать файлы, отличные по размеру
                      rfCompareCrc32,     // Перезаписать файлы, отличные по содержимому (CRC32)
                      rfCopyList);        // Добавить имена успешно обработанных файлов в список

  TRfoIniFlag      = (riSrcSubDirs,       // Обработать также вложенные подкаталоги
                      riCreateValue,      // Создать значение, если его не было в файле
                      riDeleteValue,      // Удалить значение, если оно есть в файле
                      riDeleteSection,    // Удалить секцию, если она есть в файле
                      riNotExistsOk,      // Если значение не существует - вернуть rsOk (иначе rsSkip)
                      riNotExistsError,   // Если значение не существует - вернуть rsError (иначе rsSkip)
                      riCompareValues,    // Изменить только, если предыдущее значение отличается от нового
                      riSkipReadOnly,     // Пропустить (не обрабатывать) файлы "Только чтение"
                      riSkipSysFiles,     // Пропустить (не обрабатывать) системные файлы
                      riSkipHiddenFiles); // Пропустить (не обрабатывать) скрытые файлы

  TRfoRegFlag      = (rgCreateValue,      // Создать значение, если его не было в реестре
                      rgDeleteValue,      // Удалить значение, если оно есть в реестре
                      rgNotExistsOk,      // Если значение не существует - вернуть gsOk (иначе gsWarning)
                      rgNotExistsError,   // Если значение не существует - вернуть gsError (иначе gsWarning)
                      rgCompareValues);   // Изменить только, если предыдущее значение отличается от нового

  TRfoFileFlags = set of TRfoFileFlag;
  TRfoIniFlags = set of TRfoIniFlag;
  TRfoRegFlags = set of TRfoRegFlag;

  TRFileItem = packed record
    bFile: Boolean;
    sName: string;
  end;

  TRFileList = class
  private
    fFileList: array of TRFileItem;
    function  GetItemsCount: Integer;
    procedure SetItemsCount(const Value: Integer);
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure QuickSort(L, R: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function  Items(const Index: Integer): TRFileItem;
    function  IndexOf(const Name: string): Integer; overload;
    function  ItemFirst: TRFileItem;
    function  ItemLast: TRFileItem;
    procedure Clear;
    procedure Sort;
    procedure Add(const Name: string; const IsFile: Boolean);
    procedure Delete(const Index: Integer);
    property  Count: Integer read GetItemsCount write SetItemsCount;
  end;

  TRCopyItem = packed record
    sSrcName: string;
    sDstName: string;
  end;

  TRCopyList = class
  private
    fCopyList: array of TRCopyItem;
    function  GetItemsCount: Integer;
    procedure SetItemsCount(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function  Items(const Index: Integer): TRCopyItem;
    function  IndexOf(const Name: string): Integer; overload;
    procedure Clear;
    procedure Add(const SrcName, DstName: string);
    procedure Delete(const Index: Integer);
    property Count: Integer read GetItemsCount write SetItemsCount;
  end;

const
  MaskAll          = '*.*';
  CopyListTab      = #9;

  faError          = $FFFFFFFF;

  rfCopyDefault    = [rfCopyFileDate, rfCopyFileAttr, rfOverWrite, rfSkipReadOnly, rfSkipSysFiles];
  rfCopyDefaultRO  = rfCopyDefault - [rfSkipReadOnly];
  rfCopyDefaultAll = rfCopyDefault - [rfSkipReadOnly, rfSkipSysFiles];
  rfCopyCheckDef   = rfCopyDefault + [rfCompareDate, rfCompareSize];
  rfCopyCheckDefRO = rfCopyDefaultRO + [rfCompareDate, rfCompareSize];

function CorrectFileName(const FileName: string; const ChkDelims, ChkParams: Boolean): string;

// Поддержка очень длинных имен файлов -----------------------------------------
procedure wFileNameToWideFileName(const Source: string; Dest: PWideChar; DestSize: Integer);
function wExpandUNCFileName(const FileName: string): string;

function wCreateDir(const Dir: string): Boolean;
function wChangeDir(const Dir: string): Boolean;
function wRemoveDir(const Dir: string): Boolean;

function wFileOpen(const FileName: string; Mode: LongWord): Integer;
function wFileCreate(const FileName: string): Integer;
function wFileDelete(const FileName: string): Boolean;
function wFileRename(const OldName, NewName: string): Boolean;

function wFileGetAttr(const FileName: string): DWord;
function wFileSetAttr(const FileName: string; Attr: DWord): Integer;

function wFindFirst(const Path: string; Attr: DWord; var F: TwSearchRec): Integer;
function wFindNext(var F: TwSearchRec): Integer;
procedure wFindClose(var F: TwSearchRec);

function wFileExists(const FileName: string): Boolean;
function wDirectoryExists(const Directory: string): Boolean;
function wDirectoryIsEmpty(const Directory: string): Boolean;
function wForceDirectories(Dir: string): Boolean;

// Дата файла ------------------------------------------------------------------
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function FileTimeToDateTimeLocal(const FileTime: TFileTime): TDateTime;
function DateTimeToFileTime(const FileTime: TDateTime): TFileTime;
function LocalDateTimeToFileTime(const FileTime: TDateTime): TFileTime;
function wCompareFileTime(const FileTime1, FileTime2: TFileTime): Integer;
function wFileGetCreationTime(const FileName: string): TFileTime; overload;
function wFileGetCreationTime(const Handle: Integer): TFileTime; overload;
function wFileGetLastWriteTime(const FileName: string): TFileTime; overload;
function wFileGetLastWriteTime(const Handle: Integer): TFileTime; overload;
function wFileGetLastAccessTime(const FileName: string): TFileTime; overload;
function wFileGetLastAccessTime(const Handle: Integer): TFileTime; overload;
function wFileSetCreationTime(const FileName: string; const FileTime: TFileTime): Integer; overload;
function wFileSetCreationTime(const Handle: Integer; const FileTime: TFileTime): Integer; overload;
function wFileSetLastWriteTime(const FileName: string; const FileTime: TFileTime): Integer; overload;
function wFileSetLastWriteTime(const Handle: Integer; const FileTime: TFileTime): Integer; overload;
function wFileSetLastAccessTime(const FileName: string; const FileTime: TFileTime): Integer; overload;
function wFileSetLastAccessTime(const Handle: Integer; const FileTime: TFileTime): Integer; overload;

procedure wFileGetWriteTime(const FileName: string; out CreationTime, LastWriteTime: TFileTime); overload;
procedure wFileGetWriteTime(const Handle: Integer; out CreationTime, LastWriteTime: TFileTime); overload;
function wFileSetWriteTime(const FileName: string; const CreationTime, LastWriteTime: TFileTime): Integer; overload;
function wFileSetWriteTime(const Handle: Integer; const CreationTime, LastWriteTime: TFileTime): Integer; overload;

function wDirGetLastCreationTimeLocal(const DirName: string): TDateTime;
function wDirGetLastLastWriteTimeLocal(const DirName, FileMask: string): TDateTime;

// Размер файла ----------------------------------------------------------------
function FileSizeToStr(const iSize: Int64): string;
function FileHandleGetSize(const FileHandle: THandle; const RaiseIsError: Boolean = False): Int64;
function wFileGetSize(const FileName: string; const RaiseIsError: Boolean = False): Int64;
function wFileGetSizeS(const FileName: string): string;

// Сравление и "сжатие" каталогов ----------------------------------------------
procedure CompressFilePaths(const SrcFileIn, DstFileIn: string; out SrcFileOut, DstFileOut: string);

{ == Поиск файлов и каталогов ================================================== }

// Поиск каталогов -------------------------------------------------------------

function CreateDirectoryList(const Directory: string; const FileAttr: DWord;
  DirList: TRFileList; const SubDirs, DirsForward: Boolean; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// Поиск файлов ----------------------------------------------------------------
function CreateFileList(const Directory, FileMasks: string; const FileAttr: DWord;
  FileList: TRFileList; const SubDirs, DirsForward: Boolean; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function CreateFileListInDirList(DirList: TRFileList; const FileMasks: string; const FileAttr: DWord;
  FileList: TRFileList; const EmptyState: TREmptyState; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// Поиск файлов и каталогов ----------------------------------------------------
function FindMasks(const Directory, FileMasks: string; const FileAttr: DWord;
  const SubDirsScan, DirsForward: Boolean; FileList: TRFileList; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// Проверка на наличие файлов в каталоге ---------------------------------------
function DirIsEmpty(const Directory: string): Boolean;
function FilesExists(const Directory, FileMasks: string; const FileAttr: DWord; const SubDirs: Boolean;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): Boolean;

{ == Файловые операции ========================================================= }

// Копирование (перемещение) файла ---------------------------------------------
function CopyFileEx(const SrcFile, DstFile: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; CopyList: TRCopyList; const BufferSizeKb: Word;
  const NetUsage: Byte; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsFile: TRFileProgressNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// Копирование файлов по маске -------------------------------------------------
function CopyFilesMask(const SrcDir, DstDir, Masks: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; CopyList: TRCopyList;
  const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
function CopyFiles(const SrcFiles, DstDir: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; CopyList: TRCopyList;
  const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Копирование файлов по списку ------------------------------------------------
function CopyFilesList(const Title: string; const CopyList: TRCopyList; const Reversed: Boolean;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Обновление файлов по маске --------------------------------------------------
function UpdateFiles(const SrcDir, DstDir, Masks: string;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; CopyList: TRCopyList;
  const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Переменование файла ---------------------------------------------------------
function RenameFileEx(const OldFileName, NewFileName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function RenameFile(const OldFileName, NewFileName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Удаление файла --------------------------------------------------------------
function DeleteFileEx(const FileName: string; Options: TRfoFileFlags;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// Удаление каталога -----------------------------------------------------------
function DeleteFolderEx(const DirName: string; Options: TRfoFileFlags;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// Удаление файлов или каталогов по шаблону ------------------------------------
function DeleteMask(const FileMask: string; const Options: TRfoFileFlags;
  const EmptyState: TREmptyState; const ErrOptions: TRErrorsOptions;
  const TryMax: Byte; const TryDelay: Word; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Очистка каталога ------------------------------------------------------------
function ClearDirectory(const DirName: string; Options: TRfoFileFlags;
  const EmptyState: TREmptyState; const ErrOptions: TRErrorsOptions;
  const TryMax: Byte; const TryDelay: Word; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Создание каталога -----------------------------------------------------------
function ForceDirsEx(const DirName: string; const OnlyLast: Boolean;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function ForceDirs(const DirName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Смена каталога --------------------------------------------------------------
function ChangeDirEx(const DirName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function ChangeDir(const DirName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;

// Запуск файла ----------------------------------------------------------------
function ExecuteFileEx(const CommandLine, WorkDir: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function ExecuteFile(const CommandLine, WorkDir: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TROperation;
function ExecuteFileAsEx(const CommandLine, WorkDir: string;
  const DomainName, UserName, Password: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function ExecuteFileAs(const CommandLine, WorkDir: string;
  const DomainName, UserName, Password: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Открыть файл ----------------------------------------------------------------
function OpenFileEx(const VerbMode, FileName, Parameters: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function OpenFile(const VerbMode, FileName, Parameters: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TROperation;

// Создать ярлык ---------------------------------------------------------------
function CreateShortcutEx(const CmdLine, WorkDir, Arguments, LinkFile: string;
  const ForceDirs: Boolean; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function CreateShortcut(const CmdLine, WorkDir, Arguments, LinkFile: string;
  const ForceDirs: Boolean; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;

{ == Сетевые операции ========================================================== }

// Подключение и отключение сетевых дисков -------------------------------------
function NetworkMapEx(const TypeResource: Cardinal;
  const LocalName, RemoteName, UserName, Password: string;
  const SaveProfile: Boolean; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function NetworkMap(const TypeResource: Cardinal;
  const LocalName, RemoteName, UserName, Password: string;
  const SaveProfile: Boolean; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
function NetworkUnmapEx(const LocalName: string; const ForceMode, SaveProfile: Boolean;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function NetworkUnmap(const LocalName: string; const ForceMode, SaveProfile: Boolean;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent): TROperation;

{ == Операции с INI-файлами и реестром ========================================= }

// Изменение INI-файла ---------------------------------------------------------
function ChangeIniFileEx(const FileName, Section, Name, Value: string;
  const Options: TRfoIniFlags; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// Изменение INI-файлов по маске -----------------------------------------------
function ChangeIniFiles(const FileMask, Section, Name, Value: string;
  const Options: TRfoIniFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// Изменение реестра -----------------------------------------------------------
function DecodeRegistryRootKey(const RootKey: string): Cardinal;
function ChangeRegistryEx(const Root, Path, Name, TypeValue, Value: string;
  const Options: TRfoRegFlags; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function ChangeRegistry(const Root, Path, Name, TypeValue, Value: string;
  const Options: TRfoRegFlags; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;

implementation

uses
  StrUtils, IniFiles, Registry, DateUtils, Math,
  RVclUtils, RxStrUtils, RSysUtils, RCrc32, RDialogs;

resourcestring

  SMsgTryNumber        = ' (попытка %d)';
  SMsgNetUsage         = ' [к.н. %d %%]';

  SFindDirsTitle       = 'Просмотр каталога %s';
  SFindDirsCount       = 'Найдено %d каталог(ов).';
  SFindDirsNotFound    = 'Каталог "%s" не найден!';
  SFindFilesTitle      = 'Поиск файлов "%s" в каталоге "%s"';
  SFindFilesInDirList  = 'Поиск файлов "%s"';
  SFindFilesCount      = 'Найдено %d файл(ов) в %d каталогах.';
  SFindObjCount        = 'Найдено %d файл(ов) и (или) каталог(ов).';

  SFileCopyTitle       = 'Копирование "%s" в "%s"';
  SFileCopyTitleEx     = 'Копирование "%s" из "%s" в "%s"';
  SFileMoveTitle       = 'Перемещение "%s" в "%s"';
  SFileMoveTitleEx     = 'Перемещение "%s" из "%s" в "%s"';
  SFileUpdateTitleEx   = 'Обновление файла "%s" из "%s" в "%s"';
  SFileRenameTitle     = 'Переименование "%s" в "%s"';
  SFileCopyCount       = 'Успешно скопировано %n Kb.';
  SFileMoveCount       = 'Успешно перемещено %n Kb.';
  SFileCopySkip        = 'Файл с заданными свойствами уже существует.';

  SFileLockAttrRO      = 'Файл имеет атрибут ReadOnly (Только чтение).';
  SFileLockAttrSF      = 'Файл имеет атрибут System (Системный).';
  SFileLockAttrHF      = 'Файл имеет атрибут Hidden (Скрытый).';

  SDirLockAttrRO       = 'Каталог имеет атрибут ReadOnly (Только чтение).';
  SDirLockAttrSF       = 'Каталог имеет атрибут System (Системный).';
  SDirLockAttrHF       = 'Каталог имеет атрибут Hidden (Скрытый).';

  SDeleteEmptyDir      = 'Удаление пустых каталогов "%s"';
  SDeleteDir           = 'Удаление каталога "%s"';
  SDeleteFile          = 'Удаление файла "%s"';
  SDeleteObj           = 'Удаление "%s"';

  SDirectoryCheck      = 'Проверка каталога "%s"';
  SDirectoryForce      = 'Создание каталога "%s"';
  SDirectoryChange     = 'Перейти в каталог "%s"';
  SDirectoryExists     = 'Каталог уже существует.';
  SDirectoryDirNotEmpty = 'Каталог не пустой!';
  SDirectoryIgnored    = 'Каталог игнорирован';

  SExecRun             = 'Вызов команды "%s"';
  SExecRunWait         = 'Вызов команды "%s" с ожиданием завершения';
  SExecOpen            = 'Вызов файла "%s" (%s)';
  SExecOpenWait        = 'Вызов файла "%s" (%s) с ожиданием завершения';
  SExecReturnCode      = 'Код завершения: #%d "%s".';

  SLinkCreate          = 'Создание ярлыка "%s" к файлу "%s"';

  SNetworkMap          = 'Подключение сетевого ресурса [%s] = "%s"';
  SNetworkUnMap        = 'Отключение сетевого ресурса [%s]';

  SIniChangeValue      = 'Изменение параметра "%s.%s" в INI-файле "%s"';
  SIniChangeFiles      = 'Изменение параметра "%s.%s" в INI-файлах "%s"';
  SIniDeleteSection    = 'Удаление секции "%s" в INI-файле "%s"';
  SIniDeleteValue      = 'Удаление параметра "%s.%s" в INI-файле "%s"';
  SIniDeleteFiles      = 'Удаление параметра "%s.%s" в INI-файлах "%s"';
  SIniDeleteSections   = 'Удаление секции "%s" в INI-файлах "%s"';

  SIniValueChange      = 'Установлено новое значение "%s". Предыдущее значение: "%s".';
  SIniValueDelete      = 'Удален параметр "%s.%s".';
  SRegValueDelete      = 'Удален параметр "%s\%s".';
  SIniSectionDelete    = 'Удалена секция "%s".';
  SIniValueSame        = 'Значение не изменено: новое и предыдущее значение идентичны.';

  SRegChangeValue      = 'Изменение параметра "%s\%s" в разделе реестра "%s"';
  SRegDeleteValue      = 'Удаление параметра "%s\%s" в разделе реестра "%s"';

  SErrTryWait          = 'Во время выполнения операции произошла ошибка! Повторная попытка ( %d ) будет предпринята через %d секунд...';
  SErrSrcFileIsNull    = 'Не указано имя исходного файла!';
  SErrDstFileIsNull    = 'Не указано имя конечного файла!';
  SErrSelfCopy         = 'Нельзя копировать файл сам в себя!';
  SErrDirsNotFound     = 'Каталог "%s" не найден!';
  SErrFileNotFound     = 'Файл "%s" не найден!';
  SErrFilesNotFound    = 'Файл(ы) "%s" в каталоге "%s" не найден(ы)!';
  SErrForceDirs        = 'Ошибка создания каталога "%s": %s';
  SErrDeleteDir        = 'Ошибка удаления каталога "%s": %s';
  SErrOpenFile         = 'Ошибка чтения файла: %s';
  SErrCreateFile       = 'Ошибка создания файла: %s';
  SErrReadFile         = 'Ошибка чтения файла: %s';
  SErrWriteFile        = 'Ошибка записи файла: %s';
  SErrDeleteFile       = 'Ошибка удаления файла: %s';
  SErrCopyCheck        = 'Исходный и конечный файлы отличаются друг от друга!';
  SErrSetDateFile      = 'Ошибка изменения времени создания файла: %s';
  SErrSetAttrFile      = 'Ошибка изменения атрибутов файла: %s';
  SErrDelphiError      = 'Ошибка: %s!';
  SErrMapError         = 'Ошибка подключения (отключения) диска: %s!';
  SErrIniError         = 'Ошибка изменения INI-файла: %s!';
  SErrRegError         = 'Ошибка изменения реестра: %s!';
  SErrRegTypeNone      = 'Тип значения не указан!';
  SErrValueNotExists   = 'Параметр "%s.%s" не найден!';
  SErrSectionNotExists = 'Секция "%s" не найдена!';
  SErrKeyNotExists     = 'Ключ "%s" не найден!';
  SErrKeyNotOpened     = 'Ошибка открытия ключа: "%s"!';
  SErrKeyNotFound      = 'Параметр "%s\%s" не найден!';

const
  MaskDisk         = '*';
  MaskDelims       = [';'];

  MAX_WIDEPATH     = MAXSHORT;

{ == Операции с файлами (сервисные) ============================================ }

// Замена недопустимых символов
function CorrectFileName(const FileName: string; const ChkDelims, ChkParams: Boolean): string;
var
  sDiskStr, sPathStr: string;
begin
  Result := FileName;
  if Result <> EmptyStr then
  begin
    // Удаляем ":" правее 2-го символа
    if Pos(':\', Result) = 2 then
    begin
      sDiskStr := Copy(Result, 1, 3);
      sPathStr := Copy(Result, 4, Length(Result) - 3);
    end
    else begin
      sDiskStr := EmptyStr;
      sPathStr := Result;
    end;
    Result := sDiskStr + AnsiReplaceText(AnsiReplaceText(sPathStr, ':\', '@'), ':', '');

    // Удаляем все остальные некорректные символы
    Result := AnsiReplaceText(AnsiReplaceText(AnsiReplaceText(
              AnsiReplaceText(AnsiReplaceText(AnsiReplaceText(
              Result, '"', ''''''),
              '<', '{'),
              '>', '}'),
              '*', '#'),
              '?', '_'),
              '  ', ' ');
  end;
  
  // Удаляем разделители
  if ChkDelims then
    Result := AnsiReplaceText(Result, '\\', '#');
  if ChkDelims then
    Result := AnsiReplaceText(Result, '\', '-');
  if ChkParams then
    Result := AnsiReplaceText(Result, '/', '-');
end;

// Поддержка "очень длинных" ( длина > MAX_PATH (260) ) имен файлов ------------
// почти все фунции "переписаны" из модуля SysUtils ----------------------------
// 13.05.2013 ------------------------------------------------------------------
procedure wFileNameToWideFileName(const Source: string; Dest: PWideChar; DestSize: Integer);
begin
  if Copy(Source, 1, 2) = '\\'
  then StringToWideChar('\\?\UNC\' + Copy(Source, 3, Length(Source) - 2), Dest, DestSize)
  else StringToWideChar('\\?\' + Source, Dest, DestSize);
end;

function wCreateDir(const Dir: string): Boolean;
var
  sBuffer: array [0..MAX_PATH - 1] of Char;
  wBuffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(Dir) <= MAX_PATH then
  begin
    StrPCopy(sBuffer, Dir);
    Result := CreateDirectory(@sBuffer, nil);
  end
  else begin
    wFileNameToWideFileName(Dir, wBuffer, SizeOf(wBuffer));
    Result := CreateDirectoryW(@wBuffer, nil);
  end;
end;

function wChangeDir(const Dir: string): Boolean;
var
  sBuffer: array [0..MAX_PATH - 1] of Char;
  wBuffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(Dir) <= MAX_PATH then
  begin
    StrPCopy(sBuffer, Dir);
    Result := SetCurrentDirectory(@sBuffer);
  end
  else begin
    wFileNameToWideFileName(Dir, wBuffer, SizeOf(wBuffer));
    Result := SetCurrentDirectoryW(@wBuffer);
  end;
end;

function wRemoveDir(const Dir: string): Boolean;
var
  sBuffer: array [0..MAX_PATH - 1] of Char;
  wBuffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(Dir) <= MAX_PATH then
  begin
    StrPCopy(sBuffer, Dir);
    Result := RemoveDirectory(@sBuffer);
  end
  else begin
    wFileNameToWideFileName(Dir, wBuffer, SizeOf(wBuffer));
    Result := RemoveDirectoryW(@wBuffer);
  end;
end;

function wFileOpen(const FileName: string; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
var
  sBuffer: array [0..MAX_PATH - 1] of Char;
  wBuffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  Result := -1;
  if Length(FileName) <= MAX_PATH then
  begin
    StrPCopy(sBuffer, FileName);
    if ((Mode and 3) <= fmOpenReadWrite) and
      ((Mode and $F0) <= fmShareDenyNone) then
      Result := Integer(CreateFile(@sBuffer, AccessMode[Mode and 3],
        ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, 0));
  end
  else begin
    if ((Mode and 3) <= fmOpenReadWrite) and
      ((Mode and $F0) <= fmShareDenyNone) then
    begin
      wFileNameToWideFileName(FileName, wBuffer, SizeOf(wBuffer));
      Result := Integer(CreateFileW(@wBuffer, AccessMode[Mode and 3],
        ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, 0));
    end;
  end;
end;

function wFileCreate(const FileName: string): Integer;
var
  sBuffer: array [0..MAX_PATH - 1] of Char;
  wBuffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(FileName) <= MAX_PATH then
  begin
    StrPCopy(sBuffer, FileName);
    Result := Integer(CreateFile(@sBuffer, GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
  end
  else begin
    wFileNameToWideFileName(FileName, wBuffer, SizeOf(wBuffer));
    Result := Integer(CreateFileW(@wBuffer, GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0));
  end;
end;

function wFileDelete(const FileName: string): Boolean;
var
  sBuffer: array [0..MAX_PATH - 1] of Char;
  wBuffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(FileName) <= MAX_PATH then
  begin
    StrPCopy(sBuffer, FileName);
    Result := Windows.DeleteFile(@sBuffer);
  end
  else begin
    wFileNameToWideFileName(FileName, wBuffer, SizeOf(wBuffer));
    Result := Windows.DeleteFileW(@wBuffer);
  end;
end;

function wFileRename(const OldName, NewName: string): Boolean;
var
  sBuffer1: array [0..MAX_PATH - 1] of Char;
  sBuffer2: array [0..MAX_PATH - 1] of Char;
  wBuffer1: array [0..MAX_WIDEPATH - 1] of WideChar;
  wBuffer2: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if (Length(OldName) <= MAX_PATH) and (Length(NewName) <= MAX_PATH) then
  begin
    StrPCopy(sBuffer1, OldName);
    StrPCopy(sBuffer2, NewName);
    Result := MoveFile(@sBuffer1, @sBuffer2);
  end
  else begin
    wFileNameToWideFileName(OldName, wBuffer1, SizeOf(wBuffer1));
    wFileNameToWideFileName(NewName, wBuffer2, SizeOf(wBuffer2));
    Result := MoveFileW(@wBuffer1, @wBuffer2);
  end;
end;


function wFileGetAttr(const FileName: string): DWord;
var
  sBuffer: array [0..MAX_PATH - 1] of AnsiChar;
  wBuffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(FileName) <= MAX_PATH then
  begin
    StrPCopy(sBuffer, FileName);
    Result := GetFileAttributes(@sBuffer);
  end
  else begin
    wFileNameToWideFileName(FileName, wBuffer, SizeOf(wBuffer));
    Result := GetFileAttributesW(@wBuffer);
  end;
end;

function wFileSetAttr(const FileName: string; Attr: DWord): Integer;
var
  sBuffer: array [0..MAX_PATH - 1] of Char;
  wBuffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  Result := 0;
  if Length(FileName) <= MAX_PATH then
  begin
    StrPCopy(sBuffer, FileName);
    if not SetFileAttributes(@sBuffer, Attr) then
      Result := GetLastError;
  end
  else begin
    wFileNameToWideFileName(FileName, wBuffer, SizeOf(wBuffer));
    if not SetFileAttributesW(@wBuffer, Attr) then
      Result := GetLastError;
  end;
end;

function wFileAge(const FileName: string): Integer;
var
  Handle: THandle;
  FindDataA: TWin32FindDataA;
  FindDataW: TWin32FindDataW;
  LocalFileTime: TFileTime;
  BufferA: array [0..MAX_PATH] of AnsiChar;
  BufferW: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(FileName) <= MAX_PATH then
  begin
    StrPCopy(BufferA, FileName);
    Handle := FindFirstFileA(BufferA, FindDataA);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindDataA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        FileTimeToLocalFileTime(FindDataA.ftLastWriteTime, LocalFileTime);
        if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
          LongRec(Result).Lo) then Exit;
      end;
    end;
    Result := -1;
  end
  else begin
    wFileNameToWideFileName(FileName, BufferW, SizeOf(BufferW));
    Handle := FindFirstFileW(BufferW, FindDataW);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindDataW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      begin
        FileTimeToLocalFileTime(FindDataW.ftLastWriteTime, LocalFileTime);
        if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
          LongRec(Result).Lo) then Exit;
      end;
    end;
    Result := -1;
  end;
end;

function wExpandUNCFileName(const FileName: string): string;
begin
  if Length(FileName) <= MAX_PATH then
    Result := ExpandUNCFileName(FileName)
  else begin
    Result := FileName;
    if Copy(Result, 1, 2) = '.\' then
      Result := GetCurrentDir + Copy(Result, 2, Length(Result) - 1);
    if Copy(Result, 1, 3) = '..\' then
      Result := ExtractFilePath(GetCurrentDir) + Copy(Result, 4, Length(Result) - 3);
  end;
end;

function wFindMatchingFile(var F: TwSearchRec): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindDataW.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFileW(FindHandle, FindDataW) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindDataW.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi,
      LongRec(Time).Lo);
    Size := FindDataW.nFileSizeLow;
    Attr := FindDataW.dwFileAttributes;
    Name := WideCharToString(@FindDataW.cFileName);
  end;
  Result := 0;
end;

function wFindFirst(const Path: string; Attr: DWord; var F: TwSearchRec): Integer;
const
  faSpecial = faHidden or faSysFile or faVolumeID or faDirectory;
var
  BufferW: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  FillChar(F, SizeOf(F), #0);
  wFileNameToWideFileName(Path, BufferW, SizeOf(BufferW));
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFileW(BufferW, F.FindDataW);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := wFindMatchingFile(F);
    if Result <> 0 then wFindClose(F);
  end
  else
    Result := GetLastError;
end;

function wFindNext(var F: TwSearchRec): Integer;
begin
  if FindNextFileW(F.FindHandle, F.FindDataW)
  then Result := wFindMatchingFile(F)
  else Result := GetLastError;
end;

procedure wFindClose(var F: TwSearchRec);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

function wFileExists(const FileName: string): Boolean;
begin
  Result := wFileAge(FileName) > -1;
end;

function wDirectoryExists(const Directory: string): Boolean;
var
  Code: DWord;
begin
  Code := wFileGetAttr(Directory);
  Result := (Code <> faError) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function wDirectoryIsEmpty(const Directory: string): Boolean;
begin
  Result := DirIsEmpty(Directory);
end;

function wForceDirectories(Dir: string): Boolean;
var
  E: EInOutError;
begin
  Result := True;
  if Dir = '' then
  begin
    E := EInOutError.CreateFmt(SErrDirsNotFound, ['']);
    E.ErrorCode := 3;
    raise E;
  end;
  Dir := ExcludeTrailingPathDelimiter(Dir);
  if (Length(Dir) < 3) or wDirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit;
  Result := wForceDirectories(ExtractFilePath(Dir))
    and wCreateDir(Dir);
end;

// Проверка на принадлежность к каталогу ---------------------------------------
function IsDirectory(const FileName: string): Boolean;
begin
  Result := wDirectoryExists(ExcludeTrailingPathDelimiter(FileName))
    or IsPathDelimiter(FileName, Length(FileName));
end;

// Дата файла ------------------------------------------------------------------
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  SystemTime: TSystemTime;
begin
  Result := 0;
  if FileTimeToSystemTime(FileTime, SystemTime) then
  begin
    Result := EncodeDateTime(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay,
      SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds);
  end
  else RaiseSystemError;
end;

function DateTimeToFileTime(const FileTime: TDateTime): TFileTime;
var
  SystemTime: TSystemTime;
begin
  DecodeDateTime(FileTime, SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay,
    SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds);

  if not SystemTimeToFileTime(SystemTime, Result) then
    RaiseSystemError;
end;

function FileTimeToDateTimeLocal(const FileTime: TFileTime): TDateTime;
var
  LocalTime: TFileTime;
begin
  Result := 0;
  if FileTimeToLocalFileTime(FileTime, LocalTime) then
    Result := FileTimeToDateTime(LocalTime)
  else RaiseSystemError;
end;

function LocalDateTimeToFileTime(const FileTime: TDateTime): TFileTime;
begin
  if not LocalFileTimeToFileTime(DateTimeToFileTime(FileTime), Result) then
    RaiseSystemError;
end;

function wCompareFileTime(const FileTime1, FileTime2: TFileTime): Integer;
var
  Y1, M1, D1, H1, N1, S1, Z1: Word;
  Y2, M2, D2, H2, N2, S2, Z2: Word;
begin
  DecodeDateTime(FileTimeToDateTime(FileTime1), Y1, M1, D1, H1, N1, S1, Z1);
  DecodeDateTime(FileTimeToDateTime(FileTime2), Y2, M2, D2, H2, N2, S2, Z2);
  Result := CompareDateTime(EncodeDateTime(Y1, M1, D1, H1, N1, S1, 0), EncodeDateTime(Y2, M2, D2, H2, N2, S2, 0));
end;

function wFileGetCreationTime(const FileName: string): TFileTime;
var
  Handle: THandle;
  FindDataA: TWin32FindDataA;
  FindDataW: TWin32FindDataW;
  Buffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(FileName) <= MAX_PATH then
  begin
    Handle := FindFirstFileA(PChar(FileName), FindDataA);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      Result := FindDataA.ftCreationTime;
    end
    else RaiseSystemError;
  end
  else begin
    wFileNameToWideFileName(FileName, Buffer, SizeOf(Buffer));
    Handle := FindFirstFileW(Buffer, FindDataW);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      Result := FindDataW.ftCreationTime;
    end
    else RaiseSystemError;
  end;
end;

function wFileGetCreationTime(const Handle: Integer): TFileTime;
begin
  if not GetFileTime(THandle(Handle), @Result, nil, nil) then
    RaiseSystemError;
end;

function wFileGetLastWriteTime(const FileName: string): TFileTime;
var
  Handle: THandle;
  FindDataA: TWin32FindDataA;
  FindDataW: TWin32FindDataW;
  Buffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(FileName) <= MAX_PATH then
  begin
    Handle := FindFirstFileA(PChar(FileName), FindDataA);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      Result := FindDataA.ftLastWriteTime;
    end
    else RaiseSystemError;
  end
  else begin
    wFileNameToWideFileName(FileName, Buffer, SizeOf(Buffer));
    Handle := FindFirstFileW(Buffer, FindDataW);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      Result := FindDataW.ftLastWriteTime;
    end
    else RaiseSystemError;
  end;
end;

function wFileGetLastWriteTime(const Handle: Integer): TFileTime;
begin
  if not GetFileTime(THandle(Handle), nil, nil, @Result) then
    RaiseSystemError;
end;

function wFileGetLastAccessTime(const FileName: string): TFileTime;
var
  Handle: THandle;
  FindDataA: TWin32FindDataA;
  FindDataW: TWin32FindDataW;
  Buffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(FileName) <= MAX_PATH then
  begin
    Handle := FindFirstFileA(PChar(FileName), FindDataA);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      Result := FindDataA.ftLastAccessTime;
    end
    else RaiseSystemError;
  end
  else begin
    wFileNameToWideFileName(FileName, Buffer, SizeOf(Buffer));
    Handle := FindFirstFileW(Buffer, FindDataW);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      Result := FindDataW.ftLastAccessTime;
    end
    else RaiseSystemError;
  end;
end;

function wFileGetLastAccessTime(const Handle: Integer): TFileTime;
begin
  if not GetFileTime(THandle(Handle), nil, @Result, nil) then
    RaiseSystemError;
end;

function wFileSetCreationTime(const FileName: string; const FileTime: TFileTime): Integer;
var
  Handle: THandle;
begin
  Handle := wFileOpen(FileName, fmOpenWrite);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := wFileSetCreationTime(Handle, FileTime);
    finally
      FileClose(Handle);
    end;
  end
  else Result := GetLastError;
end;

function wFileSetCreationTime(const Handle: Integer; const FileTime: TFileTime): Integer;
begin
  Result := 0;
  if SetFileTime(THandle(Handle), @FileTime, nil, nil) then
    Exit;
  Result := GetLastError;
end;

function wFileSetLastWriteTime(const FileName: string; const FileTime: TFileTime): Integer;
var
  Handle: THandle;
begin
  Handle := wFileOpen(FileName, fmOpenWrite);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := wFileSetLastWriteTime(Handle, FileTime);
    finally
      FileClose(Handle);
    end;
  end
  else Result := GetLastError;
end;

function wFileSetLastWriteTime(const Handle: Integer; const FileTime: TFileTime): Integer;
begin
  Result := 0;
  if SetFileTime(THandle(Handle), nil, nil, @FileTime) then
    Exit;
  Result := GetLastError;
end;

function wFileSetLastAccessTime(const FileName: string; const FileTime: TFileTime): Integer;
var
  Handle: THandle;
begin
  Handle := wFileOpen(FileName, fmOpenWrite);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := wFileSetLastAccessTime(Handle, FileTime);
    finally
      FileClose(Handle);
    end;
  end
  else Result := GetLastError;
end;

function wFileSetLastAccessTime(const Handle: Integer; const FileTime: TFileTime): Integer;
begin
  Result := 0;
  if SetFileTime(THandle(Handle), nil, @FileTime, nil) then
    Exit;
  Result := GetLastError;
end;

procedure wFileGetWriteTime(const FileName: string; out CreationTime, LastWriteTime: TFileTime);
var
  Handle: THandle;
  FindDataA: TWin32FindDataA;
  FindDataW: TWin32FindDataW;
  Buffer: array [0..MAX_WIDEPATH - 1] of WideChar;
begin
  if Length(FileName) <= MAX_PATH then
  begin
    Handle := FindFirstFileA(PChar(FileName), FindDataA);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      CreationTime := FindDataA.ftCreationTime;
      LastWriteTime := FindDataA.ftLastWriteTime;
    end
    else RaiseSystemError;
  end
  else begin
    wFileNameToWideFileName(FileName, Buffer, SizeOf(Buffer));
    Handle := FindFirstFileW(Buffer, FindDataW);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      CreationTime := FindDataW.ftCreationTime;
      LastWriteTime := FindDataW.ftLastWriteTime;
    end
    else RaiseSystemError;
  end;
end;

procedure wFileGetWriteTime(const Handle: Integer; out CreationTime, LastWriteTime: TFileTime);
begin
  if not GetFileTime(THandle(Handle), @CreationTime, nil, @LastWriteTime) then
    RaiseSystemError;
end;

function wFileSetWriteTime(const FileName: string; const CreationTime, LastWriteTime: TFileTime): Integer;
var
  Handle: THandle;
begin
  Handle := wFileOpen(FileName, fmOpenWrite);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := wFileSetWriteTime(Handle, CreationTime, LastWriteTime);
    finally
      FileClose(Handle);
    end;
  end
  else Result := GetLastError;
end;

function wFileSetWriteTime(const Handle: Integer; const CreationTime, LastWriteTime: TFileTime): Integer;
begin
  Result := 0;
  if SetFileTime(THandle(Handle), @CreationTime, nil, @LastWriteTime) then
    Exit;
  Result := GetLastError;
end;

function wDirGetLastCreationTimeLocal(const DirName: string): TDateTime;
var
  FileList: TRFileList;
  rTran: TRTransaction;
  tTime: TDateTime;
  i, iCount: Integer;
begin
  Result := 0;
  FileList := TRFileList.Create;
  try
    rTran := CreateFileList(DirName, MaskAll, faAnyFile, FileList, True, True, ezOk, nil, nil);
    iCount := FileList.Count - 1;
    for i := 0 to iCount do
    begin
      if FileList.Items(i).bFile then
      begin
        tTime := FileTimeToDateTimeLocal(wFileGetCreationTime(FileList.Items(i).sName));
        if tTime > Result then
          Result := tTime;
      end;
    end;
  finally
    FileList.Free;
  end;
end;

function wDirGetLastLastWriteTimeLocal(const DirName, FileMask: string): TDateTime;
var
  FileList: TRFileList;
  rTran: TRTransaction;
  tTime: TDateTime;
  i, iCount: Integer;
begin
  Result := 0;
  FileList := TRFileList.Create;
  try
    rTran := CreateFileList(DirName, FileMask, faAnyFile, FileList, True, True, ezOk, nil, nil);
    iCount := FileList.Count - 1;
    for i := 0 to iCount do
    begin
      if FileList.Items(i).bFile then
      begin
        tTime := FileTimeToDateTimeLocal(wFileGetLastWriteTime(FileList.Items(i).sName));
        if tTime > Result then
          Result := tTime;
      end;
    end;
  finally
    FileList.Free;
  end;
end;

// Размер файла ----------------------------------------------------------------
function FileSizeToStr(const iSize: Int64): string;
const
  iKb = 1024;
  iMb = 1024 * 1024;
  iGb = 1024 * 1024 * 1024;
  fKb = '%.2n Kb';
  fMb = '%.2n Mb';
  fGb = '%.2n Gb';
  fSb = '%d b';
begin
  Result := '';
  if iSize > 0 then
  begin
    if iSize > iGb
    then Result := Format(fGb, [iSize / iGb])
    else begin
      if iSize > iMb
      then Result := Format(fMb, [iSize / iMb])
      else begin
        if iSize > iKb
        then Result := Format(fKb, [iSize / iKb])
        else Result := Format(fSb, [iSize]);
      end;
    end;
  end;
end;

function FileHandleGetSize(const FileHandle: THandle; const RaiseIsError: Boolean = False): Int64;
var
  dwLoSize, dwHiSize: DWord;
  iLastError: Cardinal;
begin
  Result := -1;
  dwLoSize := GetFileSize(FileHandle, @dwHiSize);
  iLastError := GetLastError;
  if (dwLoSize = INVALID_FILE_SIZE) and (iLastError <> NO_ERROR) then
  begin
    if RaiseIsError then
      RaiseSystemError(iLastError);
  end
  else begin
    Int64Rec(Result).Lo := dwLoSize;
    Int64Rec(Result).Hi := dwHiSize;
  end;
end;

function wFileGetSize(const FileName: string; const RaiseIsError: Boolean = False): Int64;
var
  FileHandle: THandle;
begin
  Result := -1;
  FileHandle := wFileOpen(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if FileHandle <> INVALID_HANDLE_VALUE then
      Result := FileHandleGetSize(FileHandle, RaiseIsError)
    else
      if RaiseIsError then
        RaiseSystemError;
  finally
    FileClose(FileHandle);
  end;
end;

function wFileGetSizeS(const FileName: string): string;
begin
  Result := FileSizeToStr(wFileGetSize(FileName, True));
end;

// Сравление и "сжатие" каталогов ----------------------------------------------
procedure CompressFilePaths(const SrcFileIn, DstFileIn: string; out SrcFileOut, DstFileOut: string);
var
  sSrcDir, sDstDir, sSrcSrv, sDstDrv, sFindStr, sReplStr: string;
  i, iCount, iStartPos, iCompareLen, iReplaceLen: Integer;

  function PeakText(const iStartPos: Integer): Integer;
  var
    iPos: Integer;
    sPeakText: string;
  begin
    Result := 0;
    iPos := iStartPos;
    sPeakText := PathDelim + ExtractWord(iPos, sDstDir, [PathDelim]) + PathDelim;
    while (iPos < WordCount(sDstDir, [PathDelim])) and AnsiContainsText(sSrcDir, sPeakText) do
    begin
      Inc(Result);
      Inc(iPos);
      sPeakText := sPeakText + ExtractWord(iPos, sDstDir, [PathDelim]) + PathDelim;
    end;
  end;

begin
  SrcFileOut := SrcFileIn;
  DstFileOut := DstFileIn;
  // Вырезаем диски из пути
  sSrcSrv := ExtractFileDrive(SrcFileIn);
  sDstDrv := ExtractFileDrive(DstFileIn);
  // Вырезаем оставшуюся часть (каталоги в чистом виде)
  sSrcDir := Copy(SrcFileIn, Length(sSrcSrv) + 1, Length(SrcFileIn) - Length(sSrcSrv));
  sDstDir := Copy(DstFileIn, Length(sDstDrv) + 1, Length(DstFileIn) - Length(sDstDrv));
  // Перебираем все субкаталоги на предмет совпадения
  iStartPos := 0; iReplaceLen := 0;
  iCount := WordCount(sDstDir, [PathDelim]);
  for i := 1 to iCount do
  begin
    iCompareLen := PeakText(i);
    if iCompareLen > iReplaceLen then
    begin
      iStartPos := i;
      iReplaceLen := iCompareLen;
    end;
  end;
  // Если совпадения были обнаружены, заменяем их в целевом пути на "..."
  if iReplaceLen > 0 then
  begin
    sFindStr := EmptyStr;
    sReplStr := EmptyStr;
    // Оставим неизменным как минимум 1 каталог слева и 1 справа
    while iReplaceLen > (WordCount(sDstDir, [PathDelim]) - 2) do
    begin
      Inc(iStartPos);
      Dec(iReplaceLen);
    end;
    for i := 1 to iReplaceLen do
    begin
      sFindStr := sFindStr + PathDelim + ExtractWord(iStartPos + i - 1, sDstDir, [PathDelim]);
      sReplStr := sReplStr + PathDelim + '...';
    end;
    // SrcFileOut := AnsiReplaceText(SrcFileOut, sFindStr + PathDelim, sReplStr + PathDelim);
    DstFileOut := AnsiReplaceText(DstFileOut, sFindStr + PathDelim, sReplStr + PathDelim);
  end;
end;

// Добавляем атрибут "каталог" в существующие атрибуты -------------------------
function FileAttrInsertDirectory(const FileAttr: DWord): DWord;
begin
  Result := FileAttr;
  if (Result and faVolumeID) = 0 then Result := Result + faVolumeID;
  if (Result and faDirectory) = 0 then Result := Result + faDirectory;
end;

// Удаляем атрибут "каталог" из существующих атрибутов -------------------------
function FileAttrRemoveDirectory(const FileAttr: DWord): DWord;
begin
  Result := FileAttr;
  if (Result and faVolumeID) > 0 then Result := Result - faVolumeID;
  if (Result and faDirectory) > 0 then Result := Result - faDirectory;
end;

// Сравнение файлов ------------------------------------------------------------
function CompareFiles(const FileName1, FileName2: TFileName; const CheckFileDate: Boolean): Boolean;
(* var
     CreationTime1, LastWriteTime1: TFileTime;
     CreationTime2, LastWriteTime2: TFileTime; *)
begin
  Result := wFileExists(FileName1) and wFileExists(FileName2)
        and (wFileGetSize(FileName1) = wFileGetSize(FileName2))
        and (FileCrc32(FileName1) = FileCrc32(FileName2));
  (* if CheckFileDate then
    Result := Result and (FileAge(FileName1) = FileAge(FileName2)); *)
  if CheckFileDate then
  begin
    (* wFileGetWriteTime(FileName1, CreationTime1, LastWriteTime1);
    wFileGetWriteTime(FileName2, CreationTime2, LastWriteTime2);
    Result := Result and (CompareFileTime(CreationTime1, CreationTime2) = 0)
      and (CompareFileTime(LastWriteTime1, LastWriteTime2) = 0); *)
    Result := Result
      and (wCompareFileTime(wFileGetLastWriteTime(FileName1),
                           wFileGetLastWriteTime(FileName2)) = 0);
  end;
end;

{ == Поиск файлов и каталогов ================================================== }

{ TRFileList }

constructor TRFileList.Create;
begin
  inherited Create;
  Clear;
end;

destructor TRFileList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TRFileList.Clear;
begin
  SetLength(fFileList, 0);
end;

function TRFileList.GetItemsCount: Integer;
begin
  Result := Length(fFileList);
end;

procedure TRFileList.SetItemsCount(const Value: Integer);
begin
  if (Value >= Low(fFileList)) and (Value < High(fFileList)) then
    SetLength(fFileList, Value);
end;

function TRFileList.Items(const Index: Integer): TRFileItem;
begin
  if (Index >= Low(fFileList)) and (Index <= High(fFileList)) then
    Result := fFileList[Index];
end;

function TRFileList.IndexOf(const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(fFileList) to High(fFileList) do
    if SameText(fFileList[i].sName, Name) then
    begin
      Result := i;
      Break;
    end;
end;

procedure TRFileList.Add(const Name: string; const IsFile: Boolean);
begin
  if IndexOf(Name) = -1 then
  begin
    SetLength(fFileList, Length(fFileList) + 1);
    with fFileList[High(fFileList)] do
    begin
      bFile := IsFile;
      sName := Name;
    end;
  end;
end;

procedure TRFileList.Delete(const Index: Integer);
var
  i: Integer;
begin
  if (Index >= Low(fFileList)) and (Index <= High(fFileList)) then
  begin
    for i := Index to High(fFileList) - 1 do
    begin
      fFileList[i].bFile := fFileList[i + 1].bFile;
      fFileList[i].sName := fFileList[i + 1].sName;
    end;
    SetLength(fFileList, Length(fFileList) - 1);
  end;
end;

function TRFileList.ItemFirst: TRFileItem;
begin
  if Length(fFileList) > 0 then
    Result := fFileList[Low(fFileList)]
  else begin
    Result.bFile := False;
    Result.sName := EmptyStr;
  end;
end;

function TRFileList.ItemLast: TRFileItem;
begin
  if Length(fFileList) > 0 then
    Result := fFileList[High(fFileList)]
  else begin
    Result.bFile := False;
    Result.sName := EmptyStr;
  end;
end;

procedure TRFileList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: TRFileItem;
begin
  Temp := fFileList[Index1];
  fFileList[Index1] := fFileList[Index2];
  fFileList[Index2] := Temp;
end;

{ содрано из TStringList }
procedure TRFileList.QuickSort(L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while AnsiCompareStr(fFileList[I].sName, fFileList[P].sName) < 0 do Inc(I);
      while AnsiCompareStr(fFileList[J].sName, fFileList[P].sName) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TRFileList.Sort;
begin
  QuickSort(Low(fFileList), High(fFileList));
end;

{ TRCopyList }

constructor TRCopyList.Create;
begin
  inherited Create;
  Clear;
end;

destructor TRCopyList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TRCopyList.Clear;
begin
  SetLength(fCopyList, 0);
end;

function TRCopyList.GetItemsCount: Integer;
begin
  Result := Length(fCopyList);
end;

procedure TRCopyList.SetItemsCount(const Value: Integer);
begin
  if (Value >= Low(fCopyList)) and (Value < High(fCopyList)) then
    SetLength(fCopyList, Value);
end;

function TRCopyList.IndexOf(const Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(fCopyList) to High(fCopyList) do
    if SameText(fCopyList[i].sSrcName, Name) then
    begin
      Result := i;
      Break;
    end;
end;

function TRCopyList.Items(const Index: Integer): TRCopyItem;
begin
  if (Index >= Low(fCopyList)) and (Index <= High(fCopyList)) then
    Result := fCopyList[Index];
end;

procedure TRCopyList.Add(const SrcName, DstName: string);
begin
  if IndexOf(SrcName) = -1 then
  begin
    SetLength(fCopyList, Length(fCopyList) + 1);
    with fCopyList[High(fCopyList)] do
    begin
      sSrcName := SrcName;
      sDstName := DstName;
    end;
  end;
end;

procedure TRCopyList.Delete(const Index: Integer);
var
  i: Integer;
begin
  if (Index >= Low(fCopyList)) and (Index <= High(fCopyList)) then
  begin
    for i := Index to High(fCopyList) - 1 do
    begin
      fCopyList[i].sSrcName := fCopyList[i + 1].sSrcName;
      fCopyList[i].sDstName := fCopyList[i + 1].sDstName;
    end;
    SetLength(fCopyList, Length(fCopyList) - 1);
  end;
end;

// Поиск каталогов по шаблону --------------------------------------------------
type
  TDirList = array of string;

function FindDirectory(const DirName: string; const FileAttr: DWord;
  var Transaction: TRTransaction; const BreakProc: TRCheckBreakNotifyEvent): TDirList;
var
  IntDirList: TDirList;
  CurrPath, CurrName: string;
  FR, i: Integer;
  SR: TwSearchRec;

  function IsNetworkPath(const Path: string): Boolean;
  begin
    Result := (Length(Path) > 1) and (Path[1] = PathDelim)
      and (ExtractFilePath(ExcludeTrailingPathDelimiter(Path)) = PathDelim + PathDelim);
  end;

begin
  SetLength(Result, 0);
  SetLength(IntDirList, 0);
  if not IsTerminatedTran(BreakProc, Transaction) then
  begin
    // Выделяем последний каталог и путь к нему
    CurrName := ExtractFileName(ExcludeTrailingPathDelimiter(DirName));
    CurrPath := ExtractFilePath(ExcludeTrailingPathDelimiter(DirName));
    if (CurrName = EmptyStr) or IsNetworkPath(CurrPath) then
    begin
      // Поиск по всем локальным дискам
      if Pos(MaskDisk, DirName) = 1 then
      begin
        CurrName := DirName;
        while Pos(MaskDisk, CurrName) = 1 do
          Delete(CurrName, 1, 1);
        for i := Ord('A') to Ord('Z') do
        begin
          if IsTerminatedTran(BreakProc, Transaction) then Break;
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
    end
    else begin
      // Рекурсивный поиск вложенных каталогов
      IntDirList := FindDirectory(CurrPath, FileAttr, Transaction, BreakProc);
      // Поиск по полученному списку вложенных каталогов
      if not IsTerminatedTran(BreakProc, Transaction)
      and (Length(IntDirList) > 0) then
      begin
        for i := Low(IntDirList) to High(IntDirList) do
        begin
          FR := wFindFirst(IncludeTrailingPathDelimiter(IntDirList[i]) + CurrName,
            FileAttrInsertDirectory(FileAttr), SR);
          try
            while (FR = 0) and not IsTerminatedTran(BreakProc, Transaction) do
            begin
              if ((SR.Attr and faVolumeID) = 0) and ((SR.Attr and faDirectory) > 0)
              and (SR.Name <> '.') and (SR.Name <> '..') then
              begin
                SetLength(Result, Length(Result) + 1);
                Result[High(Result)] := IncludeTrailingPathDelimiter(IntDirList[i]) + SR.Name;
              end;
              FR := wFindNext(SR);
            end;
          finally
            wFindClose(SR);
          end;
        end;
      end;
    end;
  end;
end;

// Поиск каталогов -------------------------------------------------------------
function CreateDirectoryList(const Directory: string; const FileAttr: DWord;
  DirList: TRFileList; const SubDirs, DirsForward: Boolean; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  i: Integer;
  DirectoryEx: string;
  RootDirList: TDirList;

  // Поиск вложенных каталогов
  procedure ScanDirectory(const DirName: string; var Transaction: TRTransaction);
  var
    FR: Integer;
    SR: TwSearchRec;
    CD: string;
  begin
    FR := wFindFirst(IncludeTrailingPathDelimiter(DirName) + MaskAll,
      FileAttrInsertDirectory(FileAttr), SR);
    try
      while (FR = 0) and not IsTerminatedTran(BreakProc, Transaction) do
      begin
        if ((SR.Attr and faVolumeID) = 0) and ((SR.Attr and faDirectory) > 0)
        and (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          CD := IncludeTrailingPathDelimiter(DirName) + SR.Name;
          if DirsForward then
          begin
            DirList.Add(CD, False);
            if not IsTerminatedTran(BreakProc, Transaction) then
              ScanDirectory(CD, Transaction);
          end
          else begin
            if not IsTerminatedTran(BreakProc, Transaction) then
              ScanDirectory(CD, Transaction);
            DirList.Add(CD, False);
          end;
        end;
        FR := wFindNext(SR);
      end;
    finally
      wFindClose(SR);
    end;
  end;

begin
  DirectoryEx := wExpandUNCFileName(Directory);
  InitTransaction(Format(SFindDirsTitle, [DirectoryEx]), Result, ShowProc);
  SetLength(RootDirList, 0);
  try
    DirList.Clear;
    // Создаем список каталогов по шаблону
    RootDirList := FindDirectory(DirectoryEx, FileAttr, Result, BreakProc);
    // Обрабатываем полученные результаты
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if Length(RootDirList) > 0 then
      begin
        for i := Low(RootDirList) to High(RootDirList) do
        begin
          if IsTerminatedTran(BreakProc, Result) then Break;
          // Выбираем направление сканирования
          if DirsForward then
          begin
            // Добавляем текущий каталог в список
            DirList.Add(RootDirList[i], False);
            // Поиск вложенных подкаталогов
            if SubDirs and not IsTerminatedTran(BreakProc, Result) then
              ScanDirectory(RootDirList[i], Result);
          end
          else begin
            // Поиск вложенных подкаталогов
            if SubDirs and not IsTerminatedTran(BreakProc, Result) then
              ScanDirectory(RootDirList[i], Result);
            // Добавляем текущий каталог в список
            DirList.Add(RootDirList[i], False);
          end;
        end;
        // Возвращаем результаты работы
        if not IsTerminatedTran(BreakProc, Result) then
          Result.Result := Format(SFindDirsCount, [DirList.Count]);
      end
      else Result.Result := Format(SFindDirsNotFound, [Directory]);
    end;
    // Устанавливаем статус транзакции
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if DirList.Count > 0
      then Result.State := msInfo
      else begin
        case EmptyState of
          ezOk: Result.State := msInfo;
          ezWarning: Result.State := msWarning;
          else Result.State := msError;
        end;
      end;
    end;
  finally
    SetLength(RootDirList, 0);
    Result.TimeEnd := Now;
  end;
end;

// Поиск файлов по шаблону -----------------------------------------------------
function CreateFileList(const Directory, FileMasks: string; const FileAttr: DWord;
  FileList: TRFileList; const SubDirs, DirsForward: Boolean; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  i, iCount, j, jCount, F: Integer;
  DirectoryEx, CurrMask, CurrFile: string;
  DirList: TRFileList;
  SR: TwSearchRec;
begin
  DirList := TRFileList.Create;
  try
    FileList.Clear;
    // Создаем список каталогов
    DirectoryEx := wExpandUNCFileName(Directory);
    Result := CreateDirectoryList(DirectoryEx, FileAttr, DirList, SubDirs, DirsForward, EmptyState, ShowProc, BreakProc);
    if not IsTerminatedTran(BreakProc, Result) and (DirList.Count > 0) then
    begin
      // Поиск файлов в найденных каталогах
      try
        Result.Title := Format(SFindFilesTitle, [FileMasks, DirectoryEx]);
        iCount := DirList.Count - 1;
        for i := 0 to iCount do
        begin
          if IsTerminatedTran(BreakProc, Result) then Break;
          jCount := WordCount(FileMasks, MaskDelims);
          for j := 1 to jCount do
          begin
            if IsTerminatedTran(BreakProc, Result) then Break;
            // Поиск файлов по текущей маске
            CurrMask := Trim(ExtractWord(j, FileMasks, MaskDelims));
            ShowMessageCustom(nil, Now, mcTransaction, msTitle,
              Format(SFindFilesTitle, [CurrMask, DirList.Items(i).sName]), ShowProc);
            F := wFindFirst(IncludeTrailingPathDelimiter(DirList.Items(i).sName) + CurrMask,
              FileAttrRemoveDirectory(FileAttr), SR);
            try
              while (F = 0) and not IsTerminatedTran(BreakProc, Result) do
              begin
                CurrFile := IncludeTrailingPathDelimiter(DirList.Items(i).sName) + SR.Name;
                FileList.Add(CurrFile, True);
                F := wFindNext(SR);
              end;
            finally
              wFindClose(SR);
            end;
          end;
        end;
        // Возвращаем результаты работы
        if not IsTerminatedTran(BreakProc, Result) then
        begin
          Result.Result := Format(SFindFilesCount, [FileList.Count, DirList.Count]);
          if FileList.Count > 0
          then Result.State := msInfo
          else begin
            case EmptyState of
              ezOk: Result.State := msInfo;
              ezWarning: Result.State := msWarning;
              else Result.State := msError;
            end;
          end;
        end;
      finally
        Result.TimeEnd := Now;
      end;
    end;
  finally
    DirList.Free;
  end;
end;

function CreateFileListInDirList(DirList: TRFileList; const FileMasks: string; const FileAttr: DWord;
  FileList: TRFileList; const EmptyState: TREmptyState; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  i, iCount, j, jCount, F: Integer;
  CurrMask, CurrFile: string;
  SR: TwSearchRec;
begin
  InitTransaction(Format(SFindFilesInDirList, [FileMasks]), Result, ShowProc);
  try
    FileList.Clear;
    // Поиск файлов в списке каталогов
    if DirList.Count > 0 then
    begin
      iCount := DirList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedTran(BreakProc, Result) then Break;
        jCount := WordCount(FileMasks, MaskDelims);
        for j := 1 to jCount do
        begin
          if IsTerminatedTran(BreakProc, Result) then Break;
          // Поиск файлов по текущей маске
          CurrMask := Trim(ExtractWord(j, FileMasks, MaskDelims));
          ShowMessageCustom(nil, Now, mcTransaction, msTitle,
            Format(SFindFilesTitle, [CurrMask, DirList.Items(i).sName]), ShowProc);
          F := wFindFirst(IncludeTrailingPathDelimiter(DirList.Items(i).sName) + CurrMask,
            FileAttrRemoveDirectory(FileAttr), SR);
          try
            while (F = 0) and not IsTerminatedTran(BreakProc, Result) do
            begin
              CurrFile := IncludeTrailingPathDelimiter(DirList.Items(i).sName) + SR.Name;
              FileList.Add(CurrFile, True);
              F := wFindNext(SR);
            end;
          finally
            wFindClose(SR);
          end;
        end;
      end;
    end;
    // Возвращаем результаты работы
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      Result.Result := Format(SFindFilesCount, [FileList.Count, DirList.Count]);
      if FileList.Count > 0
      then Result.State := msInfo
      else begin
        case EmptyState of
          ezOk: Result.State := msInfo;
          ezWarning: Result.State := msWarning;
          else Result.State := msError;
        end;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

// Поиск файлов и каталогов по шаблону -----------------------------------------
function FindMasks(const Directory, FileMasks: string; const FileAttr: DWord;
  const SubDirsScan, DirsForward: Boolean; FileList: TRFileList; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  i, j: Integer;
  DirectoryEx: string;
  RootDirList: TDirList;

  procedure ScanFiles(const DirName, Mask: string);
  var
    FR: Integer;
    SR: TwSearchRec;
    FN: string;
  begin
    ShowMessageCustom(nil, Now, mcTransaction, msTitle, Format(SFindFilesTitle, [Mask, DirName]), ShowProc);
    FR := wFindFirst(IncludeTrailingPathDelimiter(DirName) + Mask, FileAttr, SR);
    try
      while (FR = 0) and not IsTerminatedTran(BreakProc, Result) do
      begin
        if IsTerminatedTran(BreakProc, Result) then Break;
        if ((SR.Attr and faVolumeID) = 0) and (SR.Name <> '.') and (SR.Name <> '..') then
        begin
          FN := IncludeTrailingPathDelimiter(DirName) + SR.Name;
          if (SR.Attr and faDirectory) > 0 then
          begin
            if SubDirsScan and not IsTerminatedTran(BreakProc, Result) then
            begin
              if DirsForward then
                FileList.Add(IncludeTrailingPathDelimiter(FN), False);
              ScanFiles(FN, MaskAll);
              if not DirsForward then
                FileList.Add(IncludeTrailingPathDelimiter(FN), False);
            end;
          end
          else begin
            FileList.Add(FN, True);
          end;
        end;
        FR := wFindNext(SR);
      end;
    finally
      wFindClose(SR);
    end;
  end;

begin
  Result.State := msOk;
  DirectoryEx := wExpandUNCFileName(Directory);
  InitTransaction(Format(SFindFilesTitle, [FileMasks, DirectoryEx]), Result, ShowProc);
  SetLength(RootDirList, 0);
  try
    FileList.Clear;
    // Создаем список каталогов по шаблону
    RootDirList := FindDirectory(DirectoryEx, FileAttr, Result, BreakProc);
    // Обрабатываем полученные результаты
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if Length(RootDirList) > 0 then
      begin
        for i := Low(RootDirList) to High(RootDirList) do
        begin
          if IsTerminatedTran(BreakProc, Result) then Break;
          for j := 1 to WordCount(FileMasks, MaskDelims) do
          begin
            if IsTerminatedTran(BreakProc, Result) then Break;
            ScanFiles(RootDirList[i], Trim(ExtractWord(j, FileMasks, MaskDelims)));
          end;
        end;
        // Возвращаем результаты работы
        if not IsTerminatedTran(BreakProc, Result) then
          Result.Result := Format(SFindObjCount, [FileList.Count]);
      end
      else begin
        // Каталоги не найдены
        Result.Result := Format(SFindDirsNotFound, [Directory]);
      end;
    end;
    // Устанавливаем статус транзакции
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if FileList.Count > 0
      then Result.State := msInfo
      else begin
        case EmptyState of
          ezOk: Result.State := msInfo;
          ezWarning: Result.State := msWarning;
          else Result.State := msError;
        end;
      end;
    end;
  finally
    SetLength(RootDirList, 0);
    Result.TimeEnd := Now;
  end;
end;

// Проверка на наличие файлов в каталоге по шаблону ----------------------------
function DirIsEmpty(const Directory: string): Boolean;
var
  SR: TwSearchRec;
  FR: Integer;
begin
  Result := True;
  try
    FR := wFindFirst(IncludeTrailingPathDelimiter(Directory) + MaskAll, faAnyFile, SR);
    while Result and (FR = 0) do
    begin
      if (((SR.Attr and faDirectory) > 0) and ((SR.Name = '.') or (SR.Name = '..')))
      then FR := wFindNext(SR)
      else Result := False;
    end;
  finally
    wFindClose(SR);
  end;
end;

function FilesExists(const Directory, FileMasks: string; const FileAttr: DWord; const SubDirs: Boolean;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): Boolean;
var
  i, iCount, j, jCount: Integer;
  DirectoryEx: string;
  DirList: TRFileList;
  SR: TwSearchRec;
  FR: TRTransaction;
begin
  Result := False;
  DirList := TRFileList.Create;
  try
    // Создаем список каталогов
    DirectoryEx := wExpandUNCFileName(Directory);
    FR := CreateDirectoryList(DirectoryEx, FileAttr, DirList, SubDirs, True, ezError, ShowProc, BreakProc);
    if not IsTerminatedTran(BreakProc, FR) and (DirList.Count > 0) then
    begin
      // Поиск файлов в найденных каталогах
      ShowMessageCustom(nil, Now, mcTransaction, msTitle, Format(SFindFilesTitle, [FileMasks, DirectoryEx]), ShowProc);
      iCount := DirList.Count - 1;
      for i := 0 to iCount do
      begin
        jCount := WordCount(FileMasks, MaskDelims);
        for j := 1 to jCount do
        begin
          if IsTerminatedTran(BreakProc, FR) then Break;
          // Поиск файлов по текущей маске
          Result := wFindFirst(IncludeTrailingPathDelimiter(DirList.Items(i).sName) +
            Trim(ExtractWord(j, FileMasks, MaskDelims)),
            FileAttrRemoveDirectory(FileAttr), SR) = 0;
          wFindClose(SR);
          if Result then Break;
        end;
        if Result or IsTerminatedTran(BreakProc, FR) then Break;
      end;
   end;
  finally
    DirList.Free;
  end;
end;

{ == Копирование (перемещение) файла =========================================== }

// Сохраняем атрибуты файла (int) ----------------------------------------------
(* procedure SaveFileAge(const FileName: string; const NewAge: Integer;
  var Transaction: TRTransaction);
var
  ErrorCode: Integer;
begin
  ErrorCode := wFileSetDate(FileName, NewAge);
  if ErrorCode <> 0 then
  begin
    Transaction.State := msError;
    Transaction.Result := Format(SErrSetDateFile, [GetSystemError(ErrorCode)]);
  end;
end; *)

procedure SaveFileTime(const FileName: string; const CreationTime, LastWriteTime: TFileTime;
  var Transaction: TRTransaction);
var
  ErrorCode: Integer;
begin
  ErrorCode := wFileSetWriteTime(FileName, CreationTime, LastWriteTime);
  if ErrorCode <> 0 then
  begin
    Transaction.State := msError;
    Transaction.Result := Format(SErrSetDateFile, [GetSystemError(ErrorCode)]);
  end;
end;

procedure SaveFileAttributes(const FileName: string; const NewAttr: DWord;
  var Transaction: TRTransaction);
var
  ErrorCode: Integer;
begin
  ErrorCode := wFileSetAttr(FileName, NewAttr);
  if ErrorCode <> 0 then
  begin
    Transaction.State := msError;
    Transaction.Result := Format(SErrSetAttrFile, [GetSystemError(ErrorCode)]);
  end;
end;

// Копирование (перемещение) файла ---------------------------------------------
function CopyFileBuf(const SrcFile, DstFile: string;
  const SafeOverwrite, RenameLocked, TransferTime, TransferAttr: Boolean;
  const SrcCreationTime, SrcLastWriteTime: TFileTime;
  const SrcFileAttr, DstFileAttr: DWord;
  const BufferSizeKb: Word; const NetUsage: Byte;
  const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): Int64;
label
  OpenDstFile;
const
  iMaxPos      = 100 + 1 + 1 + 1; // 100% файл + переименование + перенос даты времени + перенос атрибутов
var
  Buffer: Pointer;
  SleepCopyMode: Boolean;
  StartTick, OperTick: Cardinal;
  SrcHandle, DstHandle: THandle;
  BufferSize, OrgnSize, FileSize, ReadSize, CalcSize: Int64;
  TargetFile: string;
  CurrNetUsage: Byte;
  Percent: Integer;
  ErrorCode: Integer;

  function CopyBreak: Boolean;
  begin
    Result := False;
    if IsTerminatedBool(BreakProc, Result) then
      raise ERfoError.Create(SOprMsgTypeBreak);
  end;

  function CreateTempFileName(const FileName: string): string;
  begin
    Result := FileName;
    repeat
      Result := ChangeFileExt(Result, ReplaceStr(ExtractFileExt(Result), '.', '.~'));
    until not wFileExists(Result);
  end;

  function CreateSafeFileName(const FileName: string): string;
  begin
    Result := Trim(FileName) + '.tmp';
  end;

  procedure DeleteInvalidFile(const FileName: string);
  begin
    if wFileExists(FileName) then
      wFileDelete(FileName);
  end;

  procedure CheckAndRenameLockedFile(const FileName, ErrorText: string);
  var
    ErrorCode: Integer;
    LockedFile: string;
  begin
    ErrorCode := GetLastError;
    if RenameLocked and (ErrorCode in [0, 5, 32]) then
    begin
      LockedFile := CreateTempFileName(FileName);
      if not wFileRename(FileName, LockedFile) then
        raise ERfoError.CreateFmt(ErrorText, [GetSystemError]);
    end
    else raise ERfoError.CreateFmt(ErrorText, [GetSystemError(ErrorCode)]);
  end;

  procedure RenameSafeFile(const SafeFile, DestFile: string);
  begin
    try
      // Удаляем или переименовываем целевой файл
      if not wFileDelete(DestFile) then
        CheckAndRenameLockedFile(DestFile, SErrDeleteFile);
      // Переименовываем скопированный файл в целевой
      if not wFileRename(SafeFile, DestFile) then
        raise ERfoError.CreateFmt(SErrCreateFile, [GetSystemError]);
    finally
      // В случае ошибки переименования...
      // Если скопированный файл по каким-то причинам еще остался - удаляем его
      DeleteInvalidFile(SafeFile);
    end;
  end;

begin
  Result := 0;
  BufferSize := BufferSizeKb * 1024;
  // Открываем исходный файл
  SrcHandle := wFileOpen(SrcFile, fmOpenRead or fmShareDenyWrite); // SrcMode); //
  if SrcHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      // Считываем размер файла
      // Fixed bug: 2012-09-28 - не удавалось считать размер более 2Gb
      OrgnSize := FileHandleGetSize(SrcHandle, True);
      // Если размер файла меньше начального размера буфера - исправляем его
      if (OrgnSize > 0) and (OrgnSize < BufferSize) then
        BufferSize := OrgnSize;
      // Определяем, нужно ли "тормозить" при копировании файла
      CurrNetUsage := CalculateNetUsage(NetUsageProc, NetUsage);
      SleepCopyMode := (CurrNetUsage < 100) and (OrgnSize > BufferSize)
        and (IsNetworkPath(SrcFile) or IsNetworkPath(DstFile));
      // Генерируем имя записываемого файла
      TargetFile := DstFile;
      if SafeOverwrite and wFileExists(DstFile) then
        TargetFile := CreateSafeFileName(DstFile);
      // Открываем файл на запись (не всегда целевой!!!)
OpenDstFile:
      // Открываем файл
      DstHandle := wFileCreate(TargetFile);
      if DstHandle = INVALID_HANDLE_VALUE then
      begin
        // Ошибка: не удалось открыть файл на запись - пробуем переименовать его и повторить попытку
        CheckAndRenameLockedFile(DstFile, SErrCreateFile);
        goto OpenDstFile;
      end
      else begin
        // Инициализируем индикатор прогресса копирования
        ShowProgressFile(nil, 0, iMaxPos, PrgsFile);
        try
          // Перехватываем все ошибки копирования - в случае сбоя нужно удалить созданный "кривой" файл
          try
            // В любом случаем созданный файл нужно закрыть...
            try
              // Выделяем память под буфер копирования
              GetMem(Buffer, BufferSize);
              try
                // Инициализируем счетчик "не скопированных" данных
                FileSize := OrgnSize;
                // Инициализируем счетчик запроса % загрузки сети
                CalcSize := 0;
                // Пока счетчик "не скопированных" данных больше нуля, повторяем в цикле...
                while not CopyBreak and (FileSize > 0) do
                begin
                  // Запоминаем временную метку
                  StartTick := GetTickCount;
                  // Определаем размер считываемых данных
                  if FileSize > BufferSize
                  then ReadSize := BufferSize
                  else ReadSize := FileSize;
                  // Считываем блок данных
                  if ReadSize <> FileRead(SrcHandle, Buffer^, ReadSize) then
                    raise ERfoError.CreateFmt(SErrReadFile, [GetSystemError]);
                  // Записываем считанных блок данных
                  if ReadSize <> FileWrite(DstHandle, Buffer^, ReadSize) then
                    raise ERfoError.CreateFmt(SErrWriteFile, [GetSystemError]);
                  // Уменьшаем счетчик "не скопированных" данных и результат
                  Dec(FileSize, ReadSize); // FileSize := FileSize - ReadSize;
                  Inc(Result, ReadSize); // Result := Result + ReadSize;
                  // Обновляем индикатор прогресса
                  if OrgnSize > 0
                  then Percent := Round(100 * (Result / OrgnSize))
                  else Percent := 0;
                  ShowProgressFile(nil, Percent, iMaxPos, PrgsFile);
                  // Если загрузка канала копирования менее 100%, "подтормаживаем" копирование
                  if SleepCopyMode and (FileSize > 0) then
                  begin
                    // Вычисляем время копирования текущего блока
                    OperTick := GetTickCount - StartTick;
                    // Вычисляем время ожидания и ждем...
                    if CurrNetUsage < 1 then CurrNetUsage := 1;
                    Sleep(Round(((100 * OperTick) / CurrNetUsage) - OperTick));
                    // Увеличиваем счетчик запроса % загрузки сети
                    Inc(CalcSize, 1);
                    // Если скопирован объем в 10 размеров буфера, обновляем % загрузки сети
                    if CalcSize > 9 then
                    begin
                      CalcSize := 0;
                      CurrNetUsage := CalculateNetUsage(NetUsageProc, NetUsage);
                    end;
                  end;
                end;
              finally
                // Удаляем буфер копирования
                FreeMem(Buffer, BufferSize);
              end;
            finally
              // Закрываем скопированный файл
              FileClose(DstHandle);
              // После копирования проведем дополнительную проверку размера исходного и скопированного файла
              if not CopyBreak and ((OrgnSize <> Result) or (OrgnSize <> wFileGetSize(TargetFile, True))) then
                raise ERfoError.Create(SErrCopyCheck);
            end;
          except
            // В случае ошибки копирования удаляем созданный целевой файл
            DeleteInvalidFile(TargetFile);
            raise;
          end;
          // Если копирование прошло успешно...
          if not CopyBreak and wFileExists(TargetFile) then
          begin
            // 1) ... имена целевого и скопированного файла не совпадают
            ShowProgressFile(nil, iMaxPos - 3, iMaxPos, PrgsFile);
            if not SameFileName(DstFile, TargetFile) then
              RenameSafeFile(TargetFile, DstFile);
            // 2) ... переносим дату создания файла
            ShowProgressFile(nil, iMaxPos - 2, iMaxPos, PrgsFile);
            if TransferTime then
            begin
              ErrorCode := wFileSetWriteTime(DstFile, SrcCreationTime, SrcLastWriteTime);
              if ErrorCode <> 0 then
                raise ERfoError.CreateFmt(SErrSetDateFile, [GetSystemError(ErrorCode)]);
            end;
            // 3) ... переносим атрибуты (исходного файла или целевого, если он существовал)
           ShowProgressFile(nil, iMaxPos - 1, iMaxPos, PrgsFile);
           if TransferAttr then
           begin
             ErrorCode := wFileSetAttr(DstFile, SrcFileAttr);
             if ErrorCode <> 0 then
               raise ERfoError.CreateFmt(SErrSetAttrFile, [GetSystemError(ErrorCode)]);
           end
           else begin
             if DstFileAttr <> faError then
             begin
               ErrorCode := wFileSetAttr(DstFile, DstFileAttr);
               if ErrorCode <> 0 then
                 raise ERfoError.CreateFmt(SErrSetAttrFile, [GetSystemError(ErrorCode)]);
             end;
           end;
           ShowProgressFile(nil, iMaxPos, iMaxPos, PrgsFile);
           if Assigned(PrgsFile) then
             Sleep(10);
          end;
        finally
          // Закрываем индикатор прогресса
          ShowProgressFile(nil, 0, -1, PrgsFile);
        end;
      end;
    finally
      // Закрываем исходный файл
      FileClose(SrcHandle);
    end;
  end
  // Ошибка: не удалось открыть файл на чтение
  else raise ERfoError.CreateFmt(SErrOpenFile, [GetSystemError]);
end;

function CopyDirEx(const SrcDir, DstDir: string; const DelSrcDir: Boolean;
  const Options: TRfoFileFlags; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  CurrOpName: string;
  SrcDirEx, DstDirEx: string;
  SrcDirSh, DstDirSh: string;
  SrcDirAttr: DWord;
begin
  Result.State := msOk;
  Result.TimeBegin := Now;
  // Формируем имя каталога назначения
  SrcDirEx := ExcludeTrailingPathDelimiter(wExpandUNCFileName(SrcDir));
  DstDirEx := ExcludeTrailingPathDelimiter(wExpandUNCFileName(DstDir));
  // Формируем наименование операции
  if SameText(ExtractFileName(SrcDirEx), ExtractFileName(DstDirEx)) then
  begin
    CompressFilePaths(ExtractFilePath(SrcDirEx), ExtractFilePath(DstDirEx), SrcDirSh, DstDirSh);
    // Имена исходного и конечного файлов совпадают - сокращенный вариант
    if DelSrcDir
    then Result.Title := Format(SFileMoveTitleEx, [ExtractFileName(SrcDirEx), SrcDirSh, DstDirSh])
    else Result.Title := Format(SFileCopyTitleEx, [ExtractFileName(SrcDirEx), SrcDirSh, DstDirSh]);
  end
  else begin
    CompressFilePaths(ExtractFilePath(SrcDirEx), ExtractFilePath(DstDirEx), SrcDirSh, DstDirSh);
    SrcDirSh := SrcDirSh + ExtractFileName(SrcDirEx);
    DstDirSh := DstDirSh + ExtractFileName(DstDirEx);
    // Имена исходного и конечного файлов не совпадают - полный вариант
    if DelSrcDir
    then Result.Title := Format(SFileMoveTitle, [SrcDirSh, DstDirSh])
    else Result.Title := Format(SFileCopyTitle, [SrcDirSh, DstDirSh]);
  end;
  if not IsTerminatedTran(BreakProc, Result) then
  begin
    Result.Result := EmptyStr;
    try
      ShowTransactionTitle(Result, ShowProc);
      if not IsTerminatedTran(BreakProc, Result) then
      begin
        try
          // Проверка на копирования "в себя"
          if SameText(SrcDirEx, DstDirEx) then
            raise ERfoError.Create(SErrSelfCopy);
          // Проверяем существование конечного каталога
          if wDirectoryExists(DstDirEx) then
          begin
            Result.State := msIgnored;
            Result.Result := SDirectoryExists;
            Exit;
          end;
          // Считываем атрибуты каталога
          SrcDirAttr := wFileGetAttr(SrcDirEx);
          // Проверяем атрибуты каталога
          // ... faReadOnly
          if (SrcDirAttr and faReadOnly) = faReadOnly then
          begin
            if rfSkipReadOnly in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SDirLockAttrRO;
              Exit;
            end;
          end; // ... faReadOnly
          // ... faHidden
          if (SrcDirAttr and faHidden) = faHidden then
          begin
            if rfSkipHiddenFiles in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SDirLockAttrHF;
              Exit;
            end;
          end; // ... faHidden
          // ... faSysFile
          if (SrcDirAttr and faSysFile) = faSysFile then
          begin
            if rfSkipSysFiles in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SDirLockAttrSF;
              Exit;
            end;
          end; // ... faSysFile
          // Создаем каталог
          CurrOpName := Result.Title;
          Result := ForceDirsEx(DstDirEx, not (rfForceDirs in Options), ShowProc, BreakProc);
          Result.Title := CurrOpName;
          // Переносим атрибуты каталога
          if (Result.State <> msError) and (rfCopyFileAttr in Options) then
            SaveFileAttributes(DstDirEx, SrcDirAttr, Result);
        except
          on E: Exception do
          begin
            Result.State := msError;
            Result.Result := E.Message;
          end;
        end;
      end;
    finally
      Result.TimeEnd := Now;
    end;
  end;
end;

function CopyFileEx(const SrcFile, DstFile: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; CopyList: TRCopyList; const BufferSizeKb: Word;
  const NetUsage: Byte; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsFile: TRFileProgressNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  CopySkip: Boolean;
  CopySize: Int64;
  SrcFileEx, DstFileName: string;
  SrcFileSh, DstFileSh: string;
  (* SrcFileDate: Integer; *)
  SrcCreationTime, SrcLastWriteTime: TFileTime;
  TrgCreationTime, TrgLastWriteTime: TFileTime;
  SrcFileAttr, DstFileAttr, TrgFileAttr: DWord;
  DeleteState: TRTransaction;
  IntNetUsage: Byte;

  function UpdateDstFileName(const SrcFile, DstFile: string): string;
  var
    FileAttr: DWord;
  begin
    Result := DstFile;
    if Trim(DstFile) <> EmptyStr then
    begin
      if IsPathDelimiter(DstFile, Length(DstFile)) then
        Result := DstFile + ExtractFileName(SrcFile)
      else begin
        FileAttr := wFileGetAttr(DstFile);
        if ((FileAttr <> faError) and (((FileAttr and faVolumeID) > 0) or ((FileAttr and faDirectory) > 0)))
        or (ExtractFileExt(DstFile) = EmptyStr)
        then Result := IncludeTrailingPathDelimiter(DstFile) + ExtractFileName(SrcFile);
      end;
    end;
  end;

  (* procedure DeleteBadDstFile(const DstFile: string);
  begin
    if wFileExists(DstFile) then
      wFileDelete(DstFile);
  end; *)

  function CheckTransactionResult(const DstFile: string; var TrRes: TRTransaction): Boolean;
  begin
    Result := (TrRes.State = msOk) and not IsTerminatedTran(BreakProc, TrRes);
    (* if not Result then
      DeleteBadDstFile(DstFile); *)
  end;

begin
  Result.State := msOk;
  Result.TimeBegin := Now;
  // Формируем имя файла назначения
  SrcFileEx := wExpandUNCFileName(SrcFile);
  DstFileName := UpdateDstFileName(SrcFileEx, wExpandUNCFileName(DstFile));
  // Формируем наименование операции
  if SameText(ExtractFileName(SrcFileEx), ExtractFileName(DstFileName)) then
  begin
    CompressFilePaths(ExtractFilePath(SrcFileEx), ExtractFilePath(DstFileName), SrcFileSh, DstFileSh);
    // Имена исходного и конечного файлов совпадают - сокращенный вариант
    if DelSrcFile
    then Result.Title := Format(SFileMoveTitleEx, [ExtractFileName(SrcFileEx), SrcFileSh, DstFileSh])
    else Result.Title := Format(SFileCopyTitleEx, [ExtractFileName(SrcFileEx), SrcFileSh, DstFileSh]);
  end
  else begin
    CompressFilePaths(ExtractFilePath(SrcFileEx), ExtractFilePath(DstFileName), SrcFileSh, DstFileSh);
    SrcFileSh := SrcFileSh + ExtractFileName(SrcFileEx);
    DstFileSh := DstFileSh + ExtractFileName(DstFileName);
    // Имена исходного и конечного файлов не совпадают - полный вариант
    if DelSrcFile
    then Result.Title := Format(SFileMoveTitle, [SrcFileSh, DstFileSh])
    else Result.Title := Format(SFileCopyTitle, [SrcFileSh, DstFileSh]);
  end;
  // Добавляем нагрузку на сеть (если менее 100%)
  IntNetUsage := CalculateNetUsage(NetUsageProc, NetUsage);
  if IntNetUsage < 100 then
    Result.Title := Result.Title + Format(SMsgNetUsage, [IntNetUsage]);
  if not IsTerminatedTran(BreakProc, Result) then
  begin
    Result.Result := EmptyStr;
    try
      ShowTransactionTitle(Result, ShowProc);
      if not IsTerminatedTran(BreakProc, Result) then
      begin
        try
          // Проверка на копирования "в себя"
          if SameText(SrcFileEx, DstFileName) then
            raise ERfoError.Create(SErrSelfCopy);

          // Считываем атрибуты исходного файла
          wFileGetWriteTime(SrcFileEx, SrcCreationTime, SrcLastWriteTime);
          SrcFileAttr := wFileGetAttr(SrcFileEx);
          DstFileAttr := 0;

          // Проверяем наличие конечного файла
          if wFileExists(DstFileName) then
          begin
            // Сохраняем атрибуты конечного файла
            if not (rfCopyFileAttr in Options) then
              DstFileAttr := wFileGetAttr(DstFileName);

            // Проверяем, разрешена ли перезапись
            if rfOverWrite in Options then
            begin
              // ... если не установлен ни один из флагов сравнения - перезапись разрешена
              CopySkip := (rfCompareDate in Options)
                       or (rfCompareSize in Options)
                       or (rfCompareCrc32 in Options);
              if CopySkip then
              begin
                if (rfCompareDate in Options) then
                begin
                  wFileGetWriteTime(DstFileName, TrgCreationTime, TrgLastWriteTime);
                  CopySkip := CopySkip
                    and (CompareFileTime(SrcLastWriteTime, TrgLastWriteTime) = 0);
                end;
                if (rfCompareSize in Options) then
                  CopySkip := CopySkip and (wFileGetSize(SrcFileEx) = wFileGetSize(DstFileName));
                if (rfCompareCrc32 in Options) then
                  CopySkip := CopySkip and (FileCrc32(SrcFileEx) = FileCrc32(DstFileName));
              end;
            end
            // ... перезапись запрещена в любом случае
            else CopySkip := True;
            if CopySkip then
            begin
              // Пропускаем файл ...
              Result.State := msIgnored;
              Result.Result := SFileCopySkip;
              Exit;
            end;
          end; // Проверяем наличие конечного файла

          // Проверяем атрибуты исходного файла
          // ... faReadOnly
          if (SrcFileAttr and faReadOnly) = faReadOnly then
          begin
            if rfSkipReadOnly in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SFileLockAttrRO;
              Exit;
            end;
          end; // ... faReadOnly
          // ... faHidden
          if (SrcFileAttr and faHidden) = faHidden then
          begin
            if rfSkipHiddenFiles in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SFileLockAttrHF;
              Exit;
            end;
          end; // ... faHidden
          // ... faSysFile
          if (SrcFileAttr and faSysFile) = faSysFile then
          begin
            if rfSkipSysFiles in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SFileLockAttrSF;
              Exit;
            end;
          end; // ... faSysFile

          // Проверяем атрибуты существующего конечного файла
          if wFileExists(DstFileName) then
          begin
            TrgFileAttr := wFileGetAttr(DstFileName);
            // ... faReadOnly
            if (TrgFileAttr and faReadOnly) = faReadOnly then
            begin
              if rfSkipReadOnly in Options then
              begin
                Result.State := msIgnored;
                Result.Result := SFileLockAttrRO;
                Exit;
              end
              else
                SaveFileAttributes(DstFileName, TrgFileAttr - faReadOnly, Result);
            end; // ... faReadOnly
            // ... faHidden
            if (TrgFileAttr and faHidden) = faHidden then
            begin
              if rfSkipHiddenFiles in Options then
              begin
                Result.State := msIgnored;
                Result.Result := SFileLockAttrHF;
                Exit;
              end
              else
                SaveFileAttributes(DstFileName, TrgFileAttr - faHidden, Result);
            end; // ... faHidden
            // ... faSysFile
            if (TrgFileAttr and faSysFile) = faSysFile then
            begin
              if rfSkipSysFiles in Options then
              begin
                Result.State := msIgnored;
                Result.Result := SFileLockAttrSF;
                Exit;
              end
              else
                SaveFileAttributes(DstFileName, TrgFileAttr - faSysFile, Result);
            end; // ... faSysFile
          end;

          // Создаем каталог назначения при необходимости
          if (Result.State = msOk)
          and not IsTerminatedTran(BreakProc, Result)
          and (rfForceDirs in Options)
          and not wDirectoryExists(ExtractFilePath(DstFileName)) then
          begin
            if wForceDirectories(ExtractFilePath(DstFileName)) then
            begin
              // Переносим атрибуты каталога при необходимости
              if (rfCopyFileAttr in Options) then
              begin
                SaveFileAttributes(ExtractFilePath(DstFileName),
                  wFileGetAttr(ExtractFilePath(SrcFileEx)), Result);
                if Result.State = msError then
                  Exit;
              end;
            end
            else begin
              Result.State := msError;
              Result.Result := Format(SErrForceDirs, [ExtractFilePath(DstFileName), GetSystemError]);
              Exit;
            end;
          end;

          // Собственно копирование файла
          if (Result.State = msOk) and not IsTerminatedTran(BreakProc, Result) then
          begin
            // 2013-09-03: перенос даты и атрибутов файла перемещен непостредственно в CopyFileBuf
            CopySize := CopyFileBuf(SrcFileEx, DstFileName,
              rfCopyBackup in Options, rfCopyLocked in Options,
              rfCopyFileDate in Options, rfCopyFileAttr in Options,
              SrcCreationTime, SrcLastWriteTime, SrcFileAttr, DstFileAttr,
              BufferSizeKb, NetUsage, NetUsageProc, PrgsFile, BreakProc);
            // Сравниваем файлы
            if CheckTransactionResult(DstFileName, Result) then
            begin
              if (rfCheckCopy in Options)
              and not CompareFiles(SrcFileEx, DstFileName, rfCopyFileDate in Options) then
              begin
                Result.State := msError;
                Result.Result := SErrCopyCheck;
                // 2013-11-11: bug fix: не удаляется файл, если ошибка сравнения файлов
                try
                  wFileDelete(DstFileName);
                except
                end;
              end;
              // Добавляем в список скопированных файлов
              if CheckTransactionResult(DstFileName, Result) then
              begin
                if (rfCopyList in Options) and Assigned(CopyList) then
                  CopyList.Add(SrcFileEx, DstFileName);
                // Генерируем результат в текстовом виде
                if CheckTransactionResult(DstFileName, Result) then
                begin
                  Result.Size := CopySize;
                  if DelSrcFile then
                  begin
                    DeleteState := DeleteFileEx(SrcFileEx, Options, nil, BreakProc);
                    case DeleteState.State of
                      msOk: Result.Result := Format(SFileMoveCount, [CopySize / 1024]);
                      msIgnored: Result.Result := Format(SFileCopyCount, [CopySize / 1024]);
                      else begin
                        Result.State := DeleteState.State;
                        Result.Result := DeleteState.Result;
                      end;
                    end;
                  end
                  else Result.Result := Format(SFileCopyCount, [CopySize / 1024]);
                end; // Генерируем результат в текстовом виде
              end; // Добавляем в список скопированных файлов
            end; // Сравниваем файлы
          end; // Собственно копирование файла
        except
          on E: Exception do
          begin
            Result.State := msError;
            Result.Result := E.Message;
            (* DeleteBadDstFile(DstFileName); *)
          end;
        end;
      end;
    finally
      Result.TimeEnd := Now;
    end;
  end;
end;

{ 20130802 - fix bug: необходимо удалять "плохой" файл в случае ошибок при копировании

    // Переносим атрибуты
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if rfCopyFileDate in Options then
        (* SaveFileAge(DstFileName, SrcFileDate, Result); *)
        SaveFileTime(DstFileName, SrcCreationTime, SrcLastWriteTime, Result);
      if rfCopyFileAttr in Options then
        SaveFileAttributes(DstFileName, SrcFileAttr, Result);
    end;
    // Сравниваем файлы
    if (Result.State = msOk) and not IsTerminatedTran(BreakProc, Result)
    and (rfCheckCopy in Options)
    and not CompareFiles(SrcFileEx, DstFileName, rfCopyFileDate in Options) then
    begin
      Result.State := msError;
      Result.Result := SErrCopyCheck;
      Exit;
    end;
    // Добавляем в список
    if (Result.State = msOk) and not IsTerminatedTran(BreakProc, Result)
    and (rfCopyList in Options) and Assigned(CopyList) then
      CopyList.Add(SrcFileEx + CopyListTab + DstFileName);
    // Генерируем результат
    if (Result.State = msOk) and not IsTerminatedTran(BreakProc, Result) then
    begin
      Result.Size := CopySize;
      if DelSrcFile then
      begin
        DeleteState := DeleteFileEx(SrcFileEx, Options, nil, BreakProc);
        case DeleteState.State of
          msOk: Result.Result := Format(SFileMoveCount, [CopySize / 1024]);
          msIgnored: Result.Result := Format(SFileCopyCount, [CopySize / 1024]);
          else begin
            Result.State := DeleteState.State;
            Result.Result := DeleteState.Result;
          end;
        end;
      end
      else Result.Result := Format(SFileCopyCount, [CopySize / 1024]);
    end;
  end;
except
  on E: Exception do
  begin
    Result.State := msError;
    Result.Result := E.Message;
  end;
end;

20130802 - fix bug: необходимо удалять "плохой" файл в случае ошибок при копировании}

{ 20130802 - изменен алгоритм проверки атрибутов (по исходному файлу)

// Проверяем атрибуты конечного файла ...
DstFileAttr := wFileGetAttr(DstFileName);
// ... faReadOnly
if (DstFileAttr and faReadOnly) = faReadOnly then
begin
  if rfSkipReadOnly in Options then
  begin
    Result.State := msIgnored;
    Result.Result := SFileLockAttrRO;
    Exit;
  end
  else DstFileAttr := DstFileAttr - faReadOnly;
end;
// ... faHidden
if (DstFileAttr and faHidden) = faHidden then
begin
  if rfSkipHiddenFiles in Options then
  begin
    Result.State := msIgnored;
    Result.Result := SFileLockAttrHF;
    Exit;
  end
  else DstFileAttr := DstFileAttr - faHidden;
end;
// ... faSysFile
if (DstFileAttr and faSysFile) = faSysFile then
begin
  if rfSkipSysFiles in Options then
  begin
    Result.State := msIgnored;
    Result.Result := SFileLockAttrSF;
    Exit;
  end
  else DstFileAttr := DstFileAttr - faSysFile;
end;
// Записываем измененные атрибуты
if (Result.State = msOk) and (DstFileAttr <> wFileGetAttr(DstFileName)) then
  SaveFileAttributes(DstFileName, DstFileAttr, Result);

20130802 - изменен алгоритм проверки атрибутов (по исходному файлу) }

// Переменование файла ---------------------------------------------------------
function RenameFileEx(const OldFileName, NewFileName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  OldFileNameEx, NewFileNameEx: string;
begin
  OldFileNameEx := wExpandUNCFileName(OldFileName);
  NewFileNameEx := wExpandUNCFileName(NewFileName);
  InitTransaction(Format(SFileRenameTitle, [OldFileNameEx, NewFileNameEx]), Result, ShowProc);
  try
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if wFileRename(OldFileNameEx, NewFileNameEx) then
        Result.Result := GetSystemError(NO_ERROR)
      else begin
        Result.State := msError;
        Result.Result := GetSystemError;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function RenameFile(const OldFileName, NewFileName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  SetTransactionToOperation(RenameFileEx(OldFileName, NewFileName, ShowProc, BreakProc),
    Result, ErrOptions, ShowProc);
end;

// Удаление файла --------------------------------------------------------------
function DeleteFileEx(const FileName: string; Options: TRfoFileFlags;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  FileNameEx: string;
  FileAttr: DWord;
begin
  FileNameEx := wExpandUNCFileName(FileName);
  InitTransaction(Format(SDeleteFile, [FileNameEx]), Result, ShowProc);
  try
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if wFileExists(FileNameEx) then
      begin
        // Проверяем атрибуты
        FileAttr := wFileGetAttr(FileNameEx);
        // .. faReadOnly
        if (FileAttr and faReadOnly) = faReadOnly then
        begin
          if rfSkipReadOnly in Options then
          begin
            Result.State := msIgnored;
            Result.Result := SFileLockAttrRO;
            Exit;
          end
          else FileAttr := FileAttr - faReadOnly;
        end;
        // ... faHidden
        if (FileAttr and faHidden) = faHidden then
        begin
          if rfSkipHiddenFiles in Options then
          begin
            Result.State := msIgnored;
            Result.Result := SFileLockAttrHF;
            Exit;
          end
          else FileAttr := FileAttr - faHidden;
        end;
        // ... faSysFile
        if (FileAttr and faSysFile) = faSysFile then
        begin
          if rfSkipSysFiles in Options then
          begin
            Result.State := msIgnored;
            Result.Result := SFileLockAttrSF;
            Exit;
          end
          else FileAttr := FileAttr - faSysFile;
        end;
        // Записываем измененные атрибуты
        if (Result.State = msOk) and (FileAttr <> wFileGetAttr(FileNameEx)) then
          SaveFileAttributes(FileNameEx, FileAttr, Result);
        // Удаляем файл
        if wFileDelete(FileNameEx) then
          Result.Result := GetSystemError(NO_ERROR)
        else begin
          Result.State := msError;
          Result.Result := GetSystemError; // Format(SErrDeleteFile, [FileNameEx, GetSystemError]);
        end;
      end
      else begin
        Result.State := msError;
        Result.Result := Format(SErrFileNotFound, [FileNameEx]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

// Удаление каталога -----------------------------------------------------------
function DeleteFolderEx(const DirName: string; Options: TRfoFileFlags;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  DirNameEx: string;
  FileAttr: DWord;
begin
  DirNameEx := wExpandUNCFileName(DirName);
  InitTransaction(Format(SDeleteDir, [DirNameEx]), Result, ShowProc);
  Result.TimeBegin := Now;
  try
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if wDirectoryExists(DirNameEx) then
      begin
        // Проверяем атрибуты
        FileAttr := wFileGetAttr(DirNameEx);
        // .. faReadOnly
        if (FileAttr and faReadOnly) = faReadOnly then
        begin
          if rfSkipReadOnly in Options then
          begin
            Result.State := msIgnored;
            Result.Result := SFileLockAttrRO;
            Exit;
          end
          else FileAttr := FileAttr - faReadOnly;
        end;
        // ... faHidden
        if (FileAttr and faHidden) = faHidden then
        begin
          if rfSkipHiddenFiles in Options then
          begin
            Result.State := msIgnored;
            Result.Result := SFileLockAttrHF;
            Exit;
          end
          else FileAttr := FileAttr - faHidden;
        end;
        // ... faSysFile
        if (FileAttr and faSysFile) = faSysFile then
        begin
          if rfSkipSysFiles in Options then
          begin
            Result.State := msIgnored;
            Result.Result := SFileLockAttrSF;
            Exit;
          end
          else FileAttr := FileAttr - faSysFile;
        end;
        // Проверяем на "пустоту"
        if (rfDstFixed in Options) and not DirIsEmpty(DirNameEx) then
        // and FilesExists(DirNameEx, MaskAll, faAnyFile, True, nil, BreakProc) then
        begin
          Result.State := msIgnored;
          Result.Result := SDirectoryDirNotEmpty;
          Exit;
        end;
        // Записываем измененные атрибуты
        if (Result.State = msOk) and (FileAttr <> wFileGetAttr(DirNameEx)) then
          SaveFileAttributes(DirNameEx, FileAttr, Result);
        // Удаляем каталог
        if wRemoveDir(DirNameEx) then
          Result.Result := GetSystemError(S_OK)
        else begin
          Result.State := msError;
          Result.Result := Format(SErrDeleteDir, [DirNameEx,
            GetSystemError]);
        end;
      end
      else begin
        Result.State := msError;
        Result.Result := Format(SErrDirsNotFound, [DirNameEx]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

// Создание каталога -----------------------------------------------------------
function ForceDirsEx(const DirName: string; const OnlyLast: Boolean;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  DirNameEx: string;
begin
  DirNameEx := ExcludeTrailingPathDelimiter(wExpandUNCFileName(DirName));
  InitTransaction(Format(SDirectoryForce, [DirNameEx]), Result, ShowProc);
  try
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if wDirectoryExists(DirNameEx) then
      begin
        Result.State := msIgnored;
        Result.Result := SDirectoryExists;
      end
      else begin
        if OnlyLast and not wDirectoryExists(ExcludeTrailingPathDelimiter(ExtractFilePath(DirNameEx))) then
        begin
          Result.State := msError;
          Result.Result := Format(SErrDirsNotFound, [ExcludeTrailingPathDelimiter(ExtractFilePath(DirNameEx))]);
        end
        else begin
          if wForceDirectories(DirNameEx) then
            Result.Result := GetSystemError(S_OK)
          else begin
            Result.State := msError;
            Result.Result := Format(SErrForceDirs, [DirNameEx, GetSystemError]);
          end;
        end;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function ForceDirs(const DirName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  SetTransactionToOperation(ForceDirsEx(DirName, False, ShowProc, BreakProc), Result, ErrOptions, ShowProc);
end;

// Смена каталога --------------------------------------------------------------
function ChangeDirEx(const DirName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  DirNameEx: string;
begin
  DirNameEx := ExcludeTrailingPathDelimiter(wExpandUNCFileName(DirName));
  InitTransaction(Format(SDirectoryChange, [DirNameEx]), Result, ShowProc);
  try
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if wDirectoryExists(DirNameEx) then
      begin
        try
          wChangeDir(DirNameEx);
          Result.State := msOk;
          Result.Result := GetSystemError(S_OK);
        except
          on E: Exception do
          begin
            Result.State := msError;
            Result.Result := E.Message;
          end;
        end;
      end
      else begin
        Result.State := msError;
        Result.Result := Format(SErrDirsNotFound, [DirNameEx]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function ChangeDir(const DirName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  SetTransactionToOperation(ChangeDirEx(DirName, ShowProc, BreakProc), Result, ErrOptions, ShowProc);
end;

// Запуск файла ----------------------------------------------------------------

function CheckExitCode(const ExitCode: Integer; const ExitCodesList: string): Boolean;
const
  Delims = [#32, ';', ':', ',', '.', '!', '*'];
var
  i, iCount: Integer;
begin
  Result := False;
  iCount := WordCount(ExitCodesList, Delims);
  for i := 1 to iCount do
  begin
    Result := StrToIntDef(Trim(ExtractWord(i, ExitCodesList, Delims)), 0) = ExitCode;
    if Result then Break;
  end;
end;

function ExecuteFileEx(const CommandLine, WorkDir: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
begin
  if WaitExit
  then InitTransaction(Format(SExecRunWait, [CommandLine]), Result, ShowProc)
  else InitTransaction(Format(SExecRun, [CommandLine]), Result, ShowProc);
  ExitCode := 0;
  Result.State := msOk;
  try
    try
      ExitCode := WinExec32(WorkDir, CommandLine, ShowWindow, WaitExit, nil, MaxWaitTime);
      Result.Result := GetSystemError(SExecReturnCode, ExitCode);
      if CheckOk and (ExitCode <> S_OK) then
      begin
        Result.State := msError;
        if CheckExitCode(ExitCode, ExitCodes_Wrn) then
          Result.State := msWarning;
        if CheckExitCode(ExitCode, ExitCodes_Ok) then
          Result.State := msOk;
      end;
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message; // Format(SErrDelphiError, [E.Message]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function ExecuteFileAsEx(const CommandLine, WorkDir: string;
  const DomainName, UserName, Password: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
begin
  if WaitExit
  then InitTransaction(Format(SExecRunWait, [CommandLine]), Result, ShowProc)
  else InitTransaction(Format(SExecRun, [CommandLine]), Result, ShowProc);
  ExitCode := 0;
  Result.State := msOk;
  try
    try
      ExitCode := WinExecWithLogon32(CommandLine, GetWindowsDir,
        DomainName, UserName, Password,
        LOGON_WITH_PROFILE, ShowWindow, WaitExit, nil, MaxWaitTime);
      Result.Result := GetSystemError(SExecReturnCode, ExitCode);
      if CheckOk and (ExitCode <> S_OK) then
      begin
        Result.State := msError;
        if CheckExitCode(ExitCode, ExitCodes_Wrn) then
          Result.State := msWarning;
        if CheckExitCode(ExitCode, ExitCodes_Ok) then
          Result.State := msOk;
      end;
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message; // Format(SErrDelphiError, [E.Message]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function ExecuteFile(const CommandLine, WorkDir: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  SetTransactionToOperation(ExecuteFileEx(CommandLine, WorkDir, ShowWindow,
    CheckOk, WaitExit, MaxWaitTime, ExitCode, ExitCodes_Ok, ExitCodes_Wrn, ShowProc, BreakProc),
    Result, ErrOptions, ShowProc);
end;

function ExecuteFileAs(const CommandLine, WorkDir: string;
  const DomainName, UserName, Password: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TROperation;
var
  bBreak: Boolean;
begin
  SetTransactionToOperation(ExecuteFileAsEx(CommandLine, WorkDir,
      DomainName, UserName, Password, ShowWindow, CheckOk, WaitExit,
        MaxWaitTime, ExitCode, ExitCodes_Ok, ExitCodes_Wrn, ShowProc, BreakProc),
    Result, ErrOptions, ShowProc);
  if (Result.State in [msError]) and not (esErrorStop in ErrOptions) then
  begin
    Result.State := msWarning;
    PutTransactionToOperation(ExecuteFileEx(CommandLine, WorkDir, ShowWindow,
      CheckOk, WaitExit, MaxWaitTime, ExitCode, ExitCodes_Ok, ExitCodes_Wrn, ShowProc, BreakProc),
        Result, ErrOptions, True, bBreak, ShowProc);
  end;
end;

// Открыть файл ----------------------------------------------------------------
function OpenFileEx(const VerbMode, FileName, Parameters: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
begin
  if WaitExit
  then InitTransaction(Format(SExecOpenWait, [FileName, VerbMode]), Result, ShowProc)
  else InitTransaction(Format(SExecOpen, [FileName, VerbMode]), Result, ShowProc);
  ExitCode := 0;
  Result.State := msOk;
  try
    try
      ExitCode := OpenFile32(VerbMode, FileName, Parameters, ShowWindow, WaitExit, nil, MaxWaitTime);
      Result.Result := GetSystemError(SExecReturnCode, ExitCode);
      if CheckOk and (ExitCode <> S_OK) then
      begin
        Result.State := msError;
        if CheckExitCode(ExitCode, ExitCodes_Wrn) then
          Result.State := msWarning;
        if CheckExitCode(ExitCode, ExitCodes_Ok) then
          Result.State := msOk;
      end;
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message; // Format(SErrDelphiError, [E.Message]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function OpenFile(const VerbMode, FileName, Parameters: string;
  const ShowWindow: Integer; const CheckOk, WaitExit: Boolean;
  const MaxWaitTime: Cardinal; var ExitCode: Integer;
  const ExitCodes_Ok: string; const ExitCodes_Wrn: string;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  SetTransactionToOperation(OpenFileEx(VerbMode, FileName, Parameters, ShowWindow, CheckOk,
    WaitExit, MaxWaitTime, ExitCode, ExitCodes_Ok, ExitCodes_Wrn,
    ShowProc, BreakProc), Result, ErrOptions, ShowProc);
end;

// Создать ярлык ---------------------------------------------------------------
function CreateShortcutEx(const CmdLine, WorkDir, Arguments, LinkFile: string;
  const ForceDirs: Boolean; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
var
  DirName: string;
  RetCode: Cardinal;
begin
  InitTransaction(Format(SLinkCreate, [LinkFile, CmdLine]), Result, ShowProc);
  try
    try
      DirName := ExtractFilePath(wExpandUNCFileName(LinkFile));
      if not wDirectoryExists(DirName) and (not ForceDirs
        or not wForceDirectories(DirName)) then
          raise ERfoError.CreateFmt(SErrForceDirs, [DirName, GetSystemError]);
      RetCode := RSysUtils.CreateShortcut(CmdLine, Arguments, WorkDir, LinkFile);
      Result.Result := GetSystemError(RetCode);
      if RetCode <> S_OK then
        Result.State := msError;
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message; // Format(SErrDelphiError, [E.Message]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function CreateShortcut(const CmdLine, WorkDir, Arguments, LinkFile: string;
  const ForceDirs: Boolean; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(CreateShortcutEx(CmdLine, WorkDir, Arguments, LinkFile,
    ForceDirs, ShowProc), Result, ErrOptions, ShowProc);
end;

// Подключение и отключение сетевых дисков -------------------------------------
function NetworkMapEx(const TypeResource: Cardinal;
  const LocalName, RemoteName, UserName, Password: string;
  const SaveProfile: Boolean; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
begin
  InitTransaction(Format(SNetworkMap, [LocalName, RemoteName]), Result, ShowProc);
  try
    try
      PutSystemErrorToTransaction(Result,
        NetConnectionCreate(TypeResource, LocalName, RemoteName, UserName, Password, SaveProfile),
        ERROR_ALREADY_ASSIGNED);
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := Format(SErrMapError, [E.Message]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function NetworkMap(const TypeResource: Cardinal;
  const LocalName, RemoteName, UserName, Password: string;
  const SaveProfile: Boolean; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(NetworkMapEx(TypeResource, LocalName, RemoteName,
      UserName, Password, SaveProfile, ShowProc), Result, ErrOptions, ShowProc);
end;

function NetworkUnmapEx(const LocalName: string; const ForceMode, SaveProfile: Boolean;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
begin
  InitTransaction(Format(SNetworkUnMap, [LocalName]), Result, ShowProc);
  try
    try
      PutSystemErrorToTransaction(Result,
        NetConnectionClose(LocalName, ForceMode, SaveProfile),
        ERROR_NOT_CONNECTED);
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := Format(SErrMapError, [E.Message]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function NetworkUnmap(const LocalName: string; const ForceMode, SaveProfile: Boolean;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(NetworkUnmapEx(LocalName, ForceMode, SaveProfile, ShowProc),
    Result, ErrOptions, ShowProc);
end;


{ == Файловые операции с многими файлами ======================================= }

// Копирование файлов по маске -------------------------------------------------
function CopyFilesMask(const SrcDir, DstDir, Masks: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; CopyList: TRCopyList; const BufferSizeKb: Word;
  const NetUsage: Byte; const TryMax: Byte; const TryDelay: Word;
  const NetUsageProc: TRCalcNetUsageNotifyEvent; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const PrgsFile: TRFileProgressNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TROperation;
var
  bBreak: Boolean;
  Transaction: TRTransaction;
  SrcDirEx, DstDirEx, OffsetBase: string;
  FileList: TRFileList;
  MoveList: TRFileList;
  MoveDelOptions: TRfoFileFlags;
  sCurrFileDir, sNextFileDir: string;
  i, iCount, j, jCount: Integer;
  TryNum: Byte;

  function OffsetDstFileBase(const FileList: TRFileList): string;
  var
    i, iCount: Integer;

    function ComparePath(const Name, Path: string): string;
    begin
      if Path = EmptyStr
      then Result := ExtractFilePath(Name)
      else begin
        while (Length(Result) > 0)
          and not AnsiStartsText(Result, ExtractFilePath(Name)) do
          begin
            Result := ExtractFilePath(ExcludeTrailingPathDelimiter(Result));
          end;
      end;
    end;

  begin
    Result := EmptyStr;
    iCount := FileList.Count - 1;
    for i := 0 to iCount do
      Result := ComparePath(FileList.Items(i).sName, Result);
  end;

  function OffsetDstFileName(const SrcDir, DstDir, SrcFile: string;
    const FixedDst, MultiMode: Boolean): string;
  var
    i, iCount, N: Integer;
    OffsetDir: string;
  begin
    if FixedDst then
    begin
      if MultiMode
      then Result := IncludeTrailingPathDelimiter(DstDir)
      else Result := DstDir;
    end
    else begin
      N := 0;
      OffsetDir := IncludeTrailingPathDelimiter(SrcDir);
      iCount := Length(OffsetDir);
      for i := 1 to iCount do
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
      Result := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(DstDir) + OffsetDir);
    end;
  end;

begin
  SrcDirEx := wExpandUNCFileName(SrcDir);
  DstDirEx := wExpandUNCFileName(DstDir);
  if DelSrcFile
  then InitOperation(Format(SFileMoveTitleEx,
    [Masks, ExcludeTrailingPathDelimiter(SrcDirEx), DstDirEx]),
    Result, ShowProc)
  else InitOperation(Format(SFileCopyTitleEx,
    [Masks, ExcludeTrailingPathDelimiter(SrcDirEx), DstDirEx]),
    Result, ShowProc);
  FileList := TRFileList.Create;
  MoveList := TRFileList.Create;
  try
    bBreak := False;
    // Создаем список файлов
    Transaction := FindMasks(SrcDirEx, Masks, faAnyFile, rfSrcSubDirs in Options,
      True, FileList, EmptyState, ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
    if not bBreak and (FileList.Count > 0) then
    begin
      Inc(Result.Total, FileList.Count);
      // Вычисляем "базовую" часть имени файла наначения
      if rfDstDirAuto in Options
      then OffsetBase := OffsetDstFileBase(FileList)
      else OffsetBase := SrcDirEx;
      // Вычисляем MoveDelOptions для удаления пустых каталогов
      MoveDelOptions := [rfDstFixed];
      if DelSrcFile then
      begin
        if rfSkipReadOnly in Options then
          MoveDelOptions := MoveDelOptions + [rfSkipReadOnly];
        if rfSkipSysFiles in Options then
          MoveDelOptions := MoveDelOptions + [rfSkipSysFiles];
        if rfSkipHiddenFiles in Options then
          MoveDelOptions := MoveDelOptions + [rfSkipHiddenFiles];
      end;
      // Обрабатываем поочередно весь список
      ShowProgressCustom(nil, mcTransaction, 0, FileList.Count, PrgsProc);
      iCount := FileList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedOper(BreakProc, Result, bBreak) then Break;
        if not FileList.Items(i).bFile then // IsDirectory(FileList[i]) then
        begin
          // Создаем каталог (без копирования вложенных файлов!!!)
          Transaction := CopyDirEx(FileList.Items(i).sName,
            OffsetDstFileName(OffsetBase, DstDirEx, FileList.Items(i).sName, (rfDstFixed in Options), True),
            DelSrcFile, Options, ShowProc, BreakProc);
          // Добавляем в общий результат
          PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
          // Добавляем в список на удаление исходного каталога
          if DelSrcFile and not (Transaction.State in [msError, msBreak]) then
            MoveList.Add(ExcludeTrailingPathDelimiter(FileList.Items(i).sName), False);
        end
        else begin
          TryNum := 1;
          repeat
            // Попытка копировать файл
            Transaction := CopyFileEx(FileList.Items(i).sName, OffsetDstFileName(OffsetBase, DstDirEx, FileList.Items(i).sName,
              ((rfDstFixed in Options) or ((ExtractFileExt(DstDir) <> EmptyStr) and (FileList.Count = 1))),
              (FileList.Count > 1)), DelSrcFile, Options, CopyList, BufferSizeKb, NetUsage,
              NetUsageProc, ShowProc, PrgsFile, BreakProc);
            // Если попытка не первая, добавляем номер попыткм
            if TryNum > 1 then
              Transaction.Title := Transaction.Title + Format(SMsgTryNumber, [TryNum]);
            // Добавляем в общий результат
            PutTransactionToOperation(Transaction, Result, ErrOptions,
              (TryNum >= TryMax) or not (rfRepeatOnError in Options),
              bBreak, ShowProc);
            // Если попытки не исчерпаны и ошибка, ждем указанное число секунд
            if (Transaction.State = msError) and (rfRepeatOnError in Options) and (TryNum < TryMax) then
            begin
              WaitTerminated(TryDelay, bBreak, Format(SErrTryWait, [TryNum, TryDelay]), ShowProc, BreakProc);
              if bBreak then
              begin
                Transaction.State := msBreak;
                Transaction.Result := SOprMsgTypeBreak;
                PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
              end;
            end;
            // Увеличиваем счетчик попыток
            Inc(TryNum);
          until IsTerminatedOper(BreakProc, Result, bBreak)
            or not (rfRepeatOnError in Options)
            or (TryNum > TryMax) or (Transaction.State <> msError);
        end;
        // Проверка на ошибку обработки файла
        if (Transaction.State in [msError, msBreak])
          and (esErrorStop in ErrOptions) then
            Break;
        // Попытка удалить добавленные в список удаления каталоги
        if not IsTerminatedOper(BreakProc, Result, bBreak)
        // ... если в списке на удаление что-то есть
        and (MoveList.Count > 0)
        // ... и это не последний файл / каталог в списке
        and (i < FileList.Count - 1) then
        begin
          // Определяем имена каталогов для текущей и следущей записи в списке
          if FileList.Items(i).bFile
          then sCurrFileDir := ExtractFilePath(FileList.Items(i).sName)
          else sCurrFileDir := FileList.Items(i).sName;
          if FileList.Items(i + 1).bFile
          then sNextFileDir := ExtractFilePath(FileList.Items(i + 1).sName)
          else sNextFileDir := FileList.Items(i + 1).sName;
          // Приводим полученные имена к одинаковому виду
          sCurrFileDir := ExcludeTrailingPathDelimiter(sCurrFileDir);
          sNextFileDir := ExcludeTrailingPathDelimiter(sNextFileDir);
          // Только если следущий каталог отличается от последнего добавленного в список
          if (FileList.Items(i).bFile <> FileList.Items(i + 1).bFile)
          or not AnsiSameText(sCurrFileDir, sNextFileDir) then
          begin
            // Пробуем удалить список каталогов "снизу-вверх"
            jCount := MoveList.Count - 1;
            for j := jCount downto 0 do
              // Предварительная проверка на непустые каталоги (для ускорения процесса)
              if DirIsEmpty(MoveList.Items(j).sName) then
              begin
                Transaction := DeleteFolderEx(MoveList.Items(j).sName, MoveDelOptions, ShowProc, BreakProc);
                if Transaction.State in [msOk, msIgnored] then
                begin
                  PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
                  MoveList.Delete(j);
                end;
              end;
          end;
        end;
        // Индикатор прогресса
        ShowProgressCustom(nil, mcTransaction, i + 1, FileList.Count, PrgsProc);
      end;
      // Удаляем оставшиеся каталоги в списке на удаление
      if not IsTerminatedOper(BreakProc, Result, bBreak) then
      begin
        for j := MoveList.Count - 1 downto 0 do
        begin
          Transaction := DeleteFolderEx(MoveList.Items(j).sName, MoveDelOptions, ShowProc, BreakProc);
          // Добавляем в общий результат
          PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
          // Удаляем каталог из списка
          MoveList.Delete(j);
        end;
      end;
    end;
  finally
    MoveList.Free;
    FileList.Free;
    DoneOperation(Result, EmptyState, ShowProc);
  end;
end;

// Копирование файлов по маске -------------------------------------------------
function CopyFiles(const SrcFiles, DstDir: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; CopyList: TRCopyList;
  const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  Result := CopyFilesMask(ExtractFilePath(SrcFiles), DstDir, ExtractFileName(SrcFiles),
    DelSrcFile, Options, EmptyState, ErrOptions, CopyList,
    BufferSizeKb, NetUsage, TryMax, TryDelay, NetUsageProc, ShowProc, PrgsProc, PrgsFile, BreakProc);
end;

// Копирование файлов по списку ------------------------------------------------
function CopyFilesList(const Title: string; const CopyList: TRCopyList; const Reversed: Boolean;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
var
  bBreak: Boolean;
  Transaction: TRTransaction;
  SrcFile, DstFile: string;
  i, iCount: Integer;
  TryNum: Byte;
begin
  InitOperation(Title, Result, ShowProc);
  try
    bBreak := False;
    if not IsTerminatedOper(BreakProc, Result, bBreak)
      and (CopyList.Count > 0) then
    begin
      Inc(Result.Total, CopyList.Count);
      // Обрабатываем поочередно весь список
      ShowProgressCustom(nil, mcTransaction, 0, CopyList.Count, PrgsProc);
      iCount := CopyList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedOper(BreakProc, Result, bBreak) then Break;
        TryNum := 1;
        repeat
          // Попытка копировать файл
          if Reversed then
          begin
            SrcFile := CopyList.Items(i).sDstName;
            DstFile := CopyList.Items(i).sSrcName;
          end
          else begin
            SrcFile := CopyList.Items(i).sSrcName;
            DstFile := CopyList.Items(i).sDstName;
          end;
          Transaction := CopyFileEx(SrcFile, DstFile,
            False, Options, nil, BufferSizeKb, NetUsage, NetUsageProc, ShowProc,
            PrgsFile, BreakProc);
          // Если попытка не первая, добавляем номер попыткм
          if TryNum > 1 then
            Transaction.Title := Transaction.Title + Format(SMsgTryNumber, [TryNum]);
          // Добавляем в общий результат
          PutTransactionToOperation(Transaction, Result, ErrOptions,
            (TryNum >= TryMax) or not (rfRepeatOnError in Options),
            bBreak, ShowProc);
          // Если попытки не исчерпаны и ошибка, ждем указанное число секунд
          if (Transaction.State = msError) and (rfRepeatOnError in Options) and (TryNum < TryMax) then
          begin
            WaitTerminated(TryDelay, bBreak, Format(SErrTryWait, [TryNum, TryDelay]), ShowProc, BreakProc);
            if bBreak then
            begin
              Transaction.State := msBreak;
              Transaction.Result := SOprMsgTypeBreak;
              PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
            end;
          end;
          // Увеличиваем счетчик попыток
          Inc(TryNum);
        until bBreak or IsTerminatedOper(BreakProc, Result, bBreak)
          or (TryNum > TryMax) or (Transaction.State <> msError);
        // Проверка на ошибку обработки файла
        if (Transaction.State in [msError, msBreak])
          and (esErrorStop in ErrOptions) then
            Break;
        // Индикатор прогресса
        ShowProgressCustom(nil, mcTransaction, i + 1, CopyList.Count, PrgsProc);
      end;
    end;
  finally
    DoneOperation(Result, EmptyState, ShowProc);
  end;
end;

// Обновление файлов по маске --------------------------------------------------
function UpdateFiles(const SrcDir, DstDir, Masks: string;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; CopyList: TRCopyList;
  const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
var
  bBreak: Boolean;
  Transaction: TRTransaction;
  SrcDirEx, DstDirEx: string;
  SrcFileList, ScrScanList, DstFileList: TRFileList;
  i, iCount: Integer;
  TryNum: Byte;
begin
  SrcDirEx := wExpandUNCFileName(SrcDir);
  DstDirEx := wExpandUNCFileName(DstDir);
  InitOperation(Format(SFileUpdateTitleEx, [Masks, ExcludeTrailingPathDelimiter(SrcDirEx), DstDirEx]), Result, ShowProc);
  SrcFileList := TRFileList.Create;
  ScrScanList := TRFileList.Create;
  DstFileList := TRFileList.Create;
  try
    bBreak := False;
    // Создаем список файлов назначения
    Transaction := CreateFileList(DstDirEx, Masks, faAnyFile, DstFileList,
      rfSrcSubDirs in Options, True, EmptyState,  ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
    // Создаем список каталогов поиска
    Transaction := CreateDirectoryList(SrcDirEx, faAnyFile, ScrScanList,
      not (rfDstFixed in Options), True, EmptyState, ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
    if not bBreak and (DstFileList.Count > 0) then
    begin
      Inc(Result.Total, DstFileList.Count);
      // Обрабатываем поочередно весь список
      ShowProgressCustom(nil, mcTransaction, 0, DstFileList.Count, PrgsProc);
      iCount := DstFileList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedOper(BreakProc, Result, bBreak) then Break;
        // Ищем файлы для обновления в созданном списке каталогв
        SrcFileList.Clear;
        Transaction := CreateFileListInDirList(ScrScanList, ExtractFileName(DstFileList.Items(i).sName),
          faAnyFile, SrcFileList, ezOk, ShowProc, BreakProc);
        if (SrcFileList.Count > 0) then
        begin
          TryNum := 1;
          repeat
            // Попытка копировать файл
            Transaction := CopyFileEx(SrcFileList.Items(0).sName, DstFileList.Items(i).sName,
              False, Options, CopyList, BufferSizeKb, NetUsage, NetUsageProc, ShowProc, PrgsFile, BreakProc);
            // Переименовываем команду
            Transaction.Title := Format(SFileUpdateTitleEx, [ExtractFileName(DstFileList.Items(i).sName),
              ExtractFilePath(SrcFileList.Items(0).sName), ExtractFilePath(DstFileList.Items(i).sName)]);
            // Если попытка не первая, добавляем номер попытки
            if TryNum > 1 then Transaction.Title := Transaction.Title + Format(SMsgTryNumber, [TryNum]);
            // Добавляем в общий результат
            PutTransactionToOperation(Transaction, Result, ErrOptions,
              (TryNum >= TryMax) or not (rfRepeatOnError in Options),
              bBreak, ShowProc);
            // Если попытки не исчерпаны и ошибка, ждем указанное число секунд
            if (Transaction.State = msError) and (rfRepeatOnError in Options) and (TryNum < TryMax) then
            begin
              WaitTerminated(TryDelay, bBreak, Format(SErrTryWait, [TryNum, TryDelay]), ShowProc, BreakProc);
              if bBreak then
              begin
                Transaction.State := msBreak;
                Transaction.Result := SOprMsgTypeBreak;
                PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
              end;
            end;
            // Увеличиваем счетчик попыток
            Inc(TryNum);
          until IsTerminatedOper(BreakProc, Result, bBreak)
            or not (rfRepeatOnError in Options)
            or (TryNum > TryMax) or (Transaction.State <> msError);
        end
        else begin
          Transaction.Title := Format(SFileUpdateTitleEx, [ExtractFileName(DstFileList.Items(i).sName),
            SrcDirEx, ExtractFilePath(DstFileList.Items(i).sName)]);
          if Transaction.State = msInfo then Transaction.State := msIgnored;
          PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
        end;
        // Проверка на ошибку обработки файла
        if (Transaction.State in [msError, msBreak])
          and (esErrorStop in ErrOptions) then
            Break;
        // Индикатор прогресса
        ShowProgressCustom(nil, mcTransaction, i + 1, DstFileList.Count, PrgsProc);
      end;
    end;
  finally
    SrcFileList.Free;
    ScrScanList.Free;
    DstFileList.Free;
    DoneOperation(Result, EmptyState, ShowProc);
  end;
end;

// Удаление файлов или каталогов по шаблону ------------------------------------
function DeleteMask(const FileMask: string; const Options: TRfoFileFlags;
  const EmptyState: TREmptyState; const ErrOptions: TRErrorsOptions;
  const TryMax: Byte; const TryDelay: Word; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
var
  i, iCount: Integer;
  TryNum: Byte;
  bBreak: Boolean;
  Transaction: TRTransaction;
  FileList: TRFileList;
  FileMaskEx, DirName, MaskName: string;

begin
  FileMaskEx := wExpandUNCFileName(FileMask);
  if rfDstFixed in Options
  then InitOperation(Format(SDeleteEmptyDir, [FileMaskEx]), Result, ShowProc)
  else InitOperation(Format(SDeleteObj, [FileMaskEx]), Result, ShowProc);
  FileList := TRFileList.Create;
  try
    bBreak := False;
    DirName := ExtractFilePath(FileMaskEx);
    MaskName := ExtractFileName(FileMaskEx);
    // Создаем список каталогов (т.к. возможно задание каталогов по маске)
    if rfDstFixed in Options
    then Transaction := CreateDirectoryList(FileMaskEx, faAnyFile, FileList,
      rfSrcSubDirs in Options, False, EmptyState, ShowProc, BreakProc)
    else Transaction := FindMasks(DirName, MaskName, faAnyFile, rfSrcSubDirs in Options,
      False, FileList, EmptyState, ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
    if not bBreak and (FileList.Count > 0) then
    begin
      Inc(Result.Total, FileList.Count);
      // Обрабатываем поочередно весь список
      ShowProgressCustom(nil, mcTransaction, 0, FileList.Count, PrgsProc);
      iCount := FileList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedOper(BreakProc, Result, bBreak) then Break;
        TryNum := 1;
        repeat
          // Попытка удалить файл или каталог
          if IsDirectory(FileList.Items(i).sName) then
          begin
            if rfDstDirAuto in Options then
            begin
              InitTransaction(Format(SDeleteDir, [FileList.Items(i).sName]), Transaction, nil);
              Transaction.State := msIgnored;
              Transaction.Result := SDirectoryIgnored;
              Transaction.TimeEnd := Now;
            end
            else Transaction := DeleteFolderEx(ExcludeTrailingPathDelimiter(FileList.Items(i).sName), Options, ShowProc, BreakProc);
          end
          else Transaction := DeleteFileEx(FileList.Items(i).sName, Options, ShowProc, BreakProc);
          // Если попытка не первая, добавляем номер попыткм
          if TryNum > 1 then
            Transaction.Title := Transaction.Title + Format(SMsgTryNumber, [TryNum]);
          // Добавляем в общий результат
          PutTransactionToOperation(Transaction, Result, ErrOptions,
            (TryNum >= TryMax) or not (rfRepeatOnError in Options),
            bBreak, ShowProc);
          // Если попытки не исчерпаны и ошибка, ждем указанное число секунд
          if (Transaction.State = msError) and (rfRepeatOnError in Options) and (TryNum < TryMax) then
          begin
            WaitTerminated(TryDelay, bBreak, Format(SErrTryWait, [TryNum, TryDelay]), ShowProc, BreakProc);
            if bBreak then
            begin
              Transaction.State := msBreak;
              Transaction.Result := SOprMsgTypeBreak;
              PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
            end;
          end;
          // Увеличиваем счетчик попыток
          Inc(TryNum);
        until IsTerminatedOper(BreakProc, Result, bBreak)
          or not (rfRepeatOnError in Options)
          or (TryNum > TryMax) or (Transaction.State <> msError);
        // Проверка на ошибку обработки файла
        if (Transaction.State in [msError, msBreak])
          and (esErrorStop in ErrOptions) then
            Break;
        // Индикатор прогресса
        ShowProgressCustom(nil, mcTransaction, i + 1, FileList.Count, PrgsProc);
      end;
    end;
  finally
    FileList.Free;
    DoneOperation(Result, EmptyState, ShowProc);
  end;
end;

// Очистка каталога ------------------------------------------------------------
function ClearDirectory(const DirName: string; Options: TRfoFileFlags;
  const EmptyState: TREmptyState; const ErrOptions: TRErrorsOptions;
  const TryMax: Byte; const TryDelay: Word; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  Result := DeleteMask(IncludeTrailingPathDelimiter(DirName) + MaskAll,
    Options, EmptyState, ErrOptions, TryMax, TryDelay, ShowProc, PrgsProc, BreakProc);
end;

{ == Операции с INI-файлами и реестром ========================================= }

// Изменение INI-файла ---------------------------------------------------------
function ChangeIniFileEx(const FileName, Section, Name, Value: string;
  const Options: TRfoIniFlags; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  Ini: TMemIniFile;
  FileNameEx, PrevValue: string;
  FileAttr: DWord;
begin
  FileNameEx := wExpandUNCFileName(FileName);
  if riDeleteSection in Options
  then InitTransaction(Format(SIniDeleteSection, [Section, FileNameEx]), Result, ShowProc)
  else begin
    if riDeleteValue in Options
    then InitTransaction(Format(SIniDeleteValue, [Section, Name, FileNameEx]), Result, ShowProc)
    else InitTransaction(Format(SIniChangeValue, [Section, Name, FileNameEx]), Result, ShowProc);
  end;
  try
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if wFileExists(FileNameEx) or (riCreateValue in Options) then
      begin
        if wFileExists(FileNameEx) then
        begin
          // Проверяем атрибуты
          FileAttr := wFileGetAttr(FileNameEx);
          // .. faReadOnly
          if (FileAttr and faReadOnly) = faReadOnly then
          begin
            if riSkipReadOnly in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SFileLockAttrRO;
              Exit;
            end;
          end;
          // ... faHidden
          if (FileAttr and faHidden) = faHidden then
          begin
            if riSkipHiddenFiles in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SFileLockAttrHF;
              Exit;
            end;
          end;
          // ... faSysFile
          if (FileAttr and faSysFile) = faSysFile then
          begin
            if riSkipSysFiles in Options then
            begin
              Result.State := msIgnored;
              Result.Result := SFileLockAttrSF;
              Exit;
            end;
          end;
        end;
        // Открываем Ini-файл
        if not IsTerminatedTran(BreakProc, Result) then
        begin
          try
            Ini := TMemIniFile.Create(FileNameEx);
            try
              if riDeleteSection in Options then
              begin
                // *** Удаление секции ***
                if Ini.SectionExists(Section) then
                begin
                  Ini.EraseSection(Section);
                  Ini.UpdateFile;
                  Result.State := msOk;
                  Result.Result := Format(SIniSectionDelete, [Name]);
                end
                else begin
                  Result.Result := Format(SErrSectionNotExists, [Section]);
                  if riNotExistsOk in Options then
                    Result.State := msOk
                  else begin
                    if riNotExistsError in Options
                    then Result.State := msError
                    else Result.State := msIgnored;
                  end;
                end;
              end
              else begin
                if riDeleteValue in Options then
                begin
                  // *** Удаление значения ***
                  if Ini.ValueExists(Section, Name) then
                  begin
                    Ini.DeleteKey(Section, Name);
                    Ini.UpdateFile;
                    Result.State := msOk;
                    Result.Result := Format(SIniValueDelete, [Section, Name]);
                  end
                  else begin
                    Result.Result := Format(SErrValueNotExists, [Section, Name]);
                    if riNotExistsOk in Options then
                      Result.State := msOk
                    else begin
                      if riNotExistsError in Options
                      then Result.State := msError
                      else Result.State := msIgnored;
                    end;
                  end;
                end
                else begin
                  // *** Изменение значения ***
                  // Проверяем наличие значения
                  if not Ini.ValueExists(Section, Name) and not (riCreateValue in Options) then
                  begin
                    Result.Result := Format(SErrValueNotExists, [Section, Name]);
                    if riNotExistsOk in Options then
                      Result.State := msOk
                    else begin
                      if riNotExistsError in Options
                      then Result.State := msError
                      else Result.State := msIgnored;
                    end;
                  end
                  else begin
                    // Считываем предыдущее значение
                    PrevValue := Ini.ReadString(Section, Name, EmptyStr);
                    if not (riCompareValues in Options) or not SameText(PrevValue, Value) then
                    begin
                      Ini.WriteString(Section, Name, Value);
                      Ini.UpdateFile;
                      Result.State := msOk;
                      Result.Result := Format(SIniValueChange, [Value, PrevValue]);
                    end
                    else begin
                      Result.State := msIgnored;
                      Result.Result := SIniValueSame;
                    end;
                  end;
                end;
              end;
            finally
              // Ini.UpdateFile;
              Ini.Free;
            end;
          except
            on E: Exception do
            begin
              Result.State := msError;
              Result.Result := Format(SErrIniError, [E.Message]);
            end;
          end;
        end;
      end
      else begin
        Result.State := msError;
        Result.Result := Format(SErrFileNotFound, [FileNameEx]);
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

// Изменение INI-файлов по маске -----------------------------------------------
function ChangeIniFiles(const FileMask, Section, Name, Value: string;
  const Options: TRfoIniFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
var
  i, iCount: Integer;
  bBreak: Boolean;
  Transaction: TRTransaction;
  FileList: TRFileList;
  FileMaskEx, DirName, MaskName: string;
begin
  bBreak := False;
  FileMaskEx := wExpandUNCFileName(FileMask);
  if riDeleteSection in Options
  then InitOperation(Format(SIniDeleteSections, [Section, FileMaskEx]), Result, ShowProc)
  else begin
    if riDeleteValue in Options
    then InitOperation(Format(SIniDeleteFiles, [Section, Name, FileMaskEx]), Result, ShowProc)
    else InitOperation(Format(SIniChangeFiles, [Section, Name, FileMaskEx]), Result, ShowProc);
  end;
  FileList := TRFileList.Create;
  try
    DirName := ExtractFilePath(FileMaskEx);
    MaskName := ExtractFileName(FileMaskEx);
    // Создаем список каталогов (т.к. возможно задание каталогов по маске)
    Transaction := CreateFileList(DirName, MaskName, faAnyFile, FileList, riSrcSubDirs in Options,
      True, EmptyState, ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, not (riCreateValue in Options), bBreak, ShowProc);
    // Принудительно добавляем имя файла в список, если разрешено создание
    if (FileList.Count = 0) and (riCreateValue in Options) then
      FileList.Add(FileMaskEx, True);
    if not bBreak and (FileList.Count > 0) then
    begin
      Inc(Result.Total, FileList.Count);
      // Обрабатываем поочередно весь список
      ShowProgressCustom(nil, mcTransaction, 0, FileList.Count, PrgsProc);
      iCount := FileList.Count - 1;
      for i := 0 to iCount do
      begin
        try
          if IsTerminatedOper(BreakProc, Result, bBreak) then
          begin
            Transaction.State := msBreak;
            Transaction.Result := SOprMsgTypeBreak;
            Break;
          end
          else begin
            Transaction := ChangeIniFileEx(FileList.Items(i).sName, Section, Name, Value, Options, ShowProc, BreakProc);
            ShowProgressCustom(nil, mcTransaction, i + 1, FileList.Count, PrgsProc);
          end
        finally
          PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
        end;
      end;
    end;
  finally
    FileList.Free;
    DoneOperation(Result, EmptyState, ShowProc);
  end;
end;

// Изменение реестра -----------------------------------------------------------
function DecodeRegistryRootKey(const RootKey: string): Cardinal;
const
  RootId : array [HKEY_CLASSES_ROOT..HKEY_DYN_DATA] of array [1..4] of string = (
    ('HKEY_CLASSES_ROOT', 'HK_CLASSES_ROOT', 'HK_CR', 'HKCR'),
    ('HKEY_CURRENT_USER', 'HK_CURRENT_USER', 'HK_CU', 'HKCU'),
    ('HKEY_LOCAL_MACHINE', 'HK_LOCAL_MACHINE', 'HK_LM', 'HKLM'),
    ('HKEY_USERS', 'HK_USERS', 'HK_US', 'HKUS'),
    ('HKEY_PERFORMANCE_DATA', 'HK_PERFORMANCE_DATA', 'HK_PD', 'HKPD'),
    ('HKEY_CURRENT_CONFIG', 'HK_CURRENT_CONFIG', 'HK_СС', 'HKСС'),
    ('HKEY_DYN_DATA', 'HK_DYN_DATA', 'HK_DD', 'HKDD'));
var
  i: Cardinal;
begin
  Result := HKEY_CURRENT_USER;
  for i := HKEY_CLASSES_ROOT to HKEY_DYN_DATA do
    if SameText(RootKey, RootId[i, 1])
    or SameText(RootKey, RootId[i, 2])
    or SameText(RootKey, RootId[i, 3])
    or SameText(RootKey, RootId[i, 4])
    then begin
      Result := i;
      Break;
    end;
end;

function DecodeRegistryTypeValue(const TypeValue: string): TRegDataType;
const
  TypeId: array [TRegDataType] of array [1..4] of string = (
     ('REG_NONE', 'REG_NONE', 'NONE', 'NONE'),
     ('REG_SZ', 'REG_SZ', 'SZ', 'SZ'),
     ('REG_EXPAND_SZ', 'REG_EXPANDSZ', 'EXPAND_SZ', 'EXPANDSZ'),
     ('REG_DWORD', 'REG_INTEGER', 'DWORD', 'INTEGER'),
     ('REG_BINARY', 'REG_FLOAT', 'BINARY', 'FLOAT'));
var
  i: TRegDataType;
begin
  Result := rdUnknown;
  for i := Low(TRegDataType) to High(TRegDataType) do
    if SameText(TypeValue, TypeId[i, 1])
    or SameText(TypeValue, TypeId[i, 2])
    or SameText(TypeValue, TypeId[i, 3])
    or SameText(TypeValue, TypeId[i, 4])
    then begin
      Result := i;
      Break;
    end;
end;

function ChangeRegistryEx(const Root, Path, Name, TypeValue, Value: string;
  const Options: TRfoRegFlags; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  Reg: TRegistry;
  sPrevValue: string;
  iPrevValue, iNewValue: Integer;
  fPrevValue, fNewValue: Double;
  bWriteRes: Boolean;
begin
  if rgDeleteValue in Options
  then InitTransaction(Format(SRegDeleteValue, [Path, Name, Root]), Result, ShowProc)
  else InitTransaction(Format(SRegChangeValue, [Path, Name, Root]), Result, ShowProc);
  try
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      try
        Reg := TRegistry.Create(KEY_ALL_ACCESS);
        try
          // Проверяем корневой раздел
          Reg.RootKey := DecodeRegistryRootKey(Root);
          if rgDeleteValue in Options then
          begin
            if Reg.KeyExists(Path) then
            begin
              // Открываем ключ
              if Reg.OpenKey(Path, rgCreateValue in Options) then
              begin
                try
                  // Проверяем наличие параметра
                  if Reg.ValueExists(Name) then
                  begin
                    if Reg.DeleteValue(Name) then
                    begin
                      Result.State := msOk;
                      Result.Result := Format(SRegValueDelete, [Path, Name]);
                    end
                    else begin
                      Result.State := msError;
                      Result.Result := GetSystemError;
                    end;
                  end
                  else begin
                    Result.Result := Format(SErrValueNotExists, [Path, Name]);
                    if rgNotExistsOk in Options then
                      Result.State := msOk
                    else begin
                      if rgNotExistsError in Options
                      then Result.State := msError
                      else Result.State := msIgnored;
                    end;
                  end;
                finally
                  Reg.CloseKey;
                end;
              end
              else begin
                Result.State := msError;
                Result.Result := Format(SErrKeyNotOpened, [Path]);
              end;
            end
            else begin
              Result.Result := Format(SErrKeyNotExists, [Path]);
              if rgNotExistsOk in Options then
                Result.State := msOk
              else begin
                if rgNotExistsError in Options
                then Result.State := msError
                else Result.State := msIgnored;
              end;
            end;
          end
          else begin
            // Проверяем наличие ключа
            if not Reg.KeyExists(Path) and not (rgCreateValue in Options) then
            begin
              Result.Result := Format(SErrKeyNotExists, [Path]);
              if rgNotExistsOk in Options then
                Result.State := msOk
              else begin
                if rgNotExistsError in Options
                then Result.State := msError
                else Result.State := msIgnored;
              end;
            end
            else begin
              // Открываем ключ
              if Reg.OpenKey(Path, rgCreateValue in Options) then
              begin
                try
                  // Проверяем наличие параметра
                  if not Reg.ValueExists(Name) and not (rgCreateValue in Options) then
                  begin
                    Result.Result := Format(SErrKeyNotExists, [Path, Name]);
                    if rgNotExistsOk in Options then
                      Result.State := msOk
                    else begin
                      if rgNotExistsError in Options
                      then Result.State := msError
                      else Result.State := msIgnored;
                    end;
                  end
                  else begin
                    // Сравниваем предыдущее значение и пишем в реестр
                    sPrevValue := '';
                    case DecodeRegistryTypeValue(TypeValue) of
                      // Строки
                      rdString:
                      begin
                        bWriteRes := True;
                        if Reg.ValueExists(Name) then
                        begin
                          sPrevValue := Reg.ReadString(Name);
                          bWriteRes := not (rgCompareValues in Options)
                                    or not SameText(sPrevValue, Value);
                        end;
                        if bWriteRes then
                          Reg.WriteString(Name, Value);
                      end;
                      // Расширенные строки
                      rdExpandString:
                      begin
                        bWriteRes := True;
                        if Reg.ValueExists(Name) then
                        begin
                          sPrevValue := Reg.ReadString(Name);
                          bWriteRes := not (rgCompareValues in Options)
                                    or not SameText(sPrevValue, Value);
                        end;
                        if bWriteRes then
                          Reg.WriteExpandString(Name, Value);
                      end;
                      // Целое число
                      rdInteger:
                      begin
                        bWriteRes := True;
                        iNewValue := StrToInt(Value);
                        if Reg.ValueExists(Name) then
                        begin
                          iPrevValue := Reg.ReadInteger(Name);
                          sPrevValue := IntToStr(iPrevValue);
                          bWriteRes := not (rgCompareValues in Options)
                                    or not SameValue(iPrevValue, iNewValue);
                        end;
                        if bWriteRes then
                          Reg.WriteInteger(Name, iNewValue);
                      end;
                      // Число с плавающей запятой
                      rdBinary:
                      begin
                        bWriteRes := True;
                        fNewValue := RStrToFloat(Value);
                        if Reg.ValueExists(Name) then
                        begin
                          fPrevValue := Reg.ReadFloat(Name);
                          sPrevValue := FloatToStr(fPrevValue);
                          bWriteRes := not (rgCompareValues in Options)
                                    or not SameValue(fPrevValue, fNewValue);
                        end;
                        if bWriteRes then
                          Reg.WriteFloat(Name, fNewValue);
                      end;
                      
                      else raise Exception.Create(SErrRegTypeNone);
                    end;
                    // Вызвращаем результат
                    if bWriteRes then
                    begin
                      Result.State := msOk;
                      Result.Result := Format(SIniValueChange, [Value, sPrevValue]);
                    end
                    else begin
                      Result.State := msIgnored;
                      Result.Result := SIniValueSame;
                    end;
                  end;
                finally
                  Reg.CloseKey;
                end;
              end
              else begin
                Result.State := msError;
                Result.Result := Format(SErrKeyNotOpened, [Path]);
              end;
            end;
          end;
        finally
          Reg.Free;
        end;
      except
        on E: Exception do
        begin
          Result.State := msError;
          Result.Result := Format(SErrRegError, [E.Message]);
        end;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function ChangeRegistry(const Root, Path, Name, TypeValue, Value: string;
  const Options: TRfoRegFlags; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  SetTransactionToOperation(ChangeRegistryEx(Root, Path, Name, TypeValue, Value, Options, ShowProc, BreakProc),
    Result, ErrOptions, ShowProc);
end;

end.
