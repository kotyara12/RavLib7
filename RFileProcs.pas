unit RFileProcs;

{===============================================================================}
{ ����� ������������� �������� � ������� ��� ������ � �������� �������� (ver.3) }
{ (c) 2008-2015 �������� ��������� ����������                                   }
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

  TRfoFileFlag     = (rfForceDirs,        // ������� ���� ���������� ��� �������������
                      rfSrcSubDirs,       // ���������� ����� ��������� �����������
                      rfDstDirAuto,       // �������������� ���������� �������� ���������� (��� ������� �������� � ���� �������)
                                          // ��� �������� - ������� ������ ����� (�������� �� ��������)!
                      rfDstFixed,         // ������� (��� ����!!!) ���������� - ������������� (��� ����� �� ������ ������������ ����� ����������� � ���� � ��� ��)
                                          // ��� �������� - ������� ������ �� ������ ��������!
                      rfOverWrite,        // ������������ ������������ �����
                      rfRepeatOnError,    // ��������� ��� ������ �������� ���������� ���
                      rfCopyBackup,       // ������� ��������� ����� ������������� ����� ����� ������������
                      rfCopyLocked,       // ��������������� ����� ����������� � ��������������� �� ��������� ����
                      rfCheckCopy,        // �������� �������� � ������������� ���� (CRC32)
                      rfCopyFileDate,     // ���������� ���� ��������� �����
                      rfCopyFileAttr,     // ���������� �������� �����
                      rfSkipReadOnly,     // ���������� (�� ������������) ����� "������ ������"
                      rfSkipSysFiles,     // ���������� (�� ������������) ��������� �����
                      rfSkipHiddenFiles,  // ���������� (�� ������������) ������� �����
                      rfCompareDate,      // ������������ �����, �������� �� ���� ���������
                      rfCompareSize,      // ������������ �����, �������� �� �������
                      rfCompareCrc32,     // ������������ �����, �������� �� ����������� (CRC32)
                      rfCopyList);        // �������� ����� ������� ������������ ������ � ������

  TRfoIniFlag      = (riSrcSubDirs,       // ���������� ����� ��������� �����������
                      riCreateValue,      // ������� ��������, ���� ��� �� ���� � �����
                      riDeleteValue,      // ������� ��������, ���� ��� ���� � �����
                      riDeleteSection,    // ������� ������, ���� ��� ���� � �����
                      riNotExistsOk,      // ���� �������� �� ���������� - ������� rsOk (����� rsSkip)
                      riNotExistsError,   // ���� �������� �� ���������� - ������� rsError (����� rsSkip)
                      riCompareValues,    // �������� ������, ���� ���������� �������� ���������� �� ������
                      riSkipReadOnly,     // ���������� (�� ������������) ����� "������ ������"
                      riSkipSysFiles,     // ���������� (�� ������������) ��������� �����
                      riSkipHiddenFiles); // ���������� (�� ������������) ������� �����

  TRfoRegFlag      = (rgCreateValue,      // ������� ��������, ���� ��� �� ���� � �������
                      rgDeleteValue,      // ������� ��������, ���� ��� ���� � �������
                      rgNotExistsOk,      // ���� �������� �� ���������� - ������� gsOk (����� gsWarning)
                      rgNotExistsError,   // ���� �������� �� ���������� - ������� gsError (����� gsWarning)
                      rgCompareValues);   // �������� ������, ���� ���������� �������� ���������� �� ������

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

// ��������� ����� ������� ���� ������ -----------------------------------------
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

// ���� ����� ------------------------------------------------------------------
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

// ������ ����� ----------------------------------------------------------------
function FileSizeToStr(const iSize: Int64): string;
function FileHandleGetSize(const FileHandle: THandle; const RaiseIsError: Boolean = False): Int64;
function wFileGetSize(const FileName: string; const RaiseIsError: Boolean = False): Int64;
function wFileGetSizeS(const FileName: string): string;

// ��������� � "������" ��������� ----------------------------------------------
procedure CompressFilePaths(const SrcFileIn, DstFileIn: string; out SrcFileOut, DstFileOut: string);

{ == ����� ������ � ��������� ================================================== }

// ����� ��������� -------------------------------------------------------------

function CreateDirectoryList(const Directory: string; const FileAttr: DWord;
  DirList: TRFileList; const SubDirs, DirsForward: Boolean; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// ����� ������ ----------------------------------------------------------------
function CreateFileList(const Directory, FileMasks: string; const FileAttr: DWord;
  FileList: TRFileList; const SubDirs, DirsForward: Boolean; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function CreateFileListInDirList(DirList: TRFileList; const FileMasks: string; const FileAttr: DWord;
  FileList: TRFileList; const EmptyState: TREmptyState; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// ����� ������ � ��������� ----------------------------------------------------
function FindMasks(const Directory, FileMasks: string; const FileAttr: DWord;
  const SubDirsScan, DirsForward: Boolean; FileList: TRFileList; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// �������� �� ������� ������ � �������� ---------------------------------------
function DirIsEmpty(const Directory: string): Boolean;
function FilesExists(const Directory, FileMasks: string; const FileAttr: DWord; const SubDirs: Boolean;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): Boolean;

{ == �������� �������� ========================================================= }

// ����������� (�����������) ����� ---------------------------------------------
function CopyFileEx(const SrcFile, DstFile: string; const DelSrcFile: Boolean;
  const Options: TRfoFileFlags; CopyList: TRCopyList; const BufferSizeKb: Word;
  const NetUsage: Byte; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsFile: TRFileProgressNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// ����������� ������ �� ����� -------------------------------------------------
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
// ����������� ������ �� ������ ------------------------------------------------
function CopyFilesList(const Title: string; const CopyList: TRCopyList; const Reversed: Boolean;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// ���������� ������ �� ����� --------------------------------------------------
function UpdateFiles(const SrcDir, DstDir, Masks: string;
  const Options: TRfoFileFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; CopyList: TRCopyList;
  const BufferSizeKb: Word; const NetUsage: Byte;
  const TryMax: Byte; const TryDelay: Word; const NetUsageProc: TRCalcNetUsageNotifyEvent;
  const ShowProc: TRShowInfoNotifyEvent; const PrgsProc: TRShowProgressNotifyEvent;
  const PrgsFile: TRFileProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// ������������� ����� ---------------------------------------------------------
function RenameFileEx(const OldFileName, NewFileName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function RenameFile(const OldFileName, NewFileName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// �������� ����� --------------------------------------------------------------
function DeleteFileEx(const FileName: string; Options: TRfoFileFlags;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// �������� �������� -----------------------------------------------------------
function DeleteFolderEx(const DirName: string; Options: TRfoFileFlags;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// �������� ������ ��� ��������� �� ������� ------------------------------------
function DeleteMask(const FileMask: string; const Options: TRfoFileFlags;
  const EmptyState: TREmptyState; const ErrOptions: TRErrorsOptions;
  const TryMax: Byte; const TryDelay: Word; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// ������� �������� ------------------------------------------------------------
function ClearDirectory(const DirName: string; Options: TRfoFileFlags;
  const EmptyState: TREmptyState; const ErrOptions: TRErrorsOptions;
  const TryMax: Byte; const TryDelay: Word; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// �������� �������� -----------------------------------------------------------
function ForceDirsEx(const DirName: string; const OnlyLast: Boolean;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function ForceDirs(const DirName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// ����� �������� --------------------------------------------------------------
function ChangeDirEx(const DirName: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
function ChangeDir(const DirName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;

// ������ ����� ----------------------------------------------------------------
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
// ������� ���� ----------------------------------------------------------------
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

// ������� ����� ---------------------------------------------------------------
function CreateShortcutEx(const CmdLine, WorkDir, Arguments, LinkFile: string;
  const ForceDirs: Boolean; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function CreateShortcut(const CmdLine, WorkDir, Arguments, LinkFile: string;
  const ForceDirs: Boolean; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;

{ == ������� �������� ========================================================== }

// ����������� � ���������� ������� ������ -------------------------------------
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

{ == �������� � INI-������� � �������� ========================================= }

// ��������� INI-����� ---------------------------------------------------------
function ChangeIniFileEx(const FileName, Section, Name, Value: string;
  const Options: TRfoIniFlags; const ShowProc: TRShowInfoNotifyEvent;
  const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
// ��������� INI-������ �� ����� -----------------------------------------------
function ChangeIniFiles(const FileMask, Section, Name, Value: string;
  const Options: TRfoIniFlags; const EmptyState: TREmptyState;
  const ErrOptions: TRErrorsOptions; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
// ��������� ������� -----------------------------------------------------------
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

  SMsgTryNumber        = ' (������� %d)';
  SMsgNetUsage         = ' [�.�. %d %%]';

  SFindDirsTitle       = '�������� �������� %s';
  SFindDirsCount       = '������� %d �������(��).';
  SFindDirsNotFound    = '������� "%s" �� ������!';
  SFindFilesTitle      = '����� ������ "%s" � �������� "%s"';
  SFindFilesInDirList  = '����� ������ "%s"';
  SFindFilesCount      = '������� %d ����(��) � %d ���������.';
  SFindObjCount        = '������� %d ����(��) � (���) �������(��).';

  SFileCopyTitle       = '����������� "%s" � "%s"';
  SFileCopyTitleEx     = '����������� "%s" �� "%s" � "%s"';
  SFileMoveTitle       = '����������� "%s" � "%s"';
  SFileMoveTitleEx     = '����������� "%s" �� "%s" � "%s"';
  SFileUpdateTitleEx   = '���������� ����� "%s" �� "%s" � "%s"';
  SFileRenameTitle     = '�������������� "%s" � "%s"';
  SFileCopyCount       = '������� ����������� %n Kb.';
  SFileMoveCount       = '������� ���������� %n Kb.';
  SFileCopySkip        = '���� � ��������� ���������� ��� ����������.';

  SFileLockAttrRO      = '���� ����� ������� ReadOnly (������ ������).';
  SFileLockAttrSF      = '���� ����� ������� System (���������).';
  SFileLockAttrHF      = '���� ����� ������� Hidden (�������).';

  SDirLockAttrRO       = '������� ����� ������� ReadOnly (������ ������).';
  SDirLockAttrSF       = '������� ����� ������� System (���������).';
  SDirLockAttrHF       = '������� ����� ������� Hidden (�������).';

  SDeleteEmptyDir      = '�������� ������ ��������� "%s"';
  SDeleteDir           = '�������� �������� "%s"';
  SDeleteFile          = '�������� ����� "%s"';
  SDeleteObj           = '�������� "%s"';

  SDirectoryCheck      = '�������� �������� "%s"';
  SDirectoryForce      = '�������� �������� "%s"';
  SDirectoryChange     = '������� � ������� "%s"';
  SDirectoryExists     = '������� ��� ����������.';
  SDirectoryDirNotEmpty = '������� �� ������!';
  SDirectoryIgnored    = '������� �����������';

  SExecRun             = '����� ������� "%s"';
  SExecRunWait         = '����� ������� "%s" � ��������� ����������';
  SExecOpen            = '����� ����� "%s" (%s)';
  SExecOpenWait        = '����� ����� "%s" (%s) � ��������� ����������';
  SExecReturnCode      = '��� ����������: #%d "%s".';

  SLinkCreate          = '�������� ������ "%s" � ����� "%s"';

  SNetworkMap          = '����������� �������� ������� [%s] = "%s"';
  SNetworkUnMap        = '���������� �������� ������� [%s]';

  SIniChangeValue      = '��������� ��������� "%s.%s" � INI-����� "%s"';
  SIniChangeFiles      = '��������� ��������� "%s.%s" � INI-������ "%s"';
  SIniDeleteSection    = '�������� ������ "%s" � INI-����� "%s"';
  SIniDeleteValue      = '�������� ��������� "%s.%s" � INI-����� "%s"';
  SIniDeleteFiles      = '�������� ��������� "%s.%s" � INI-������ "%s"';
  SIniDeleteSections   = '�������� ������ "%s" � INI-������ "%s"';

  SIniValueChange      = '����������� ����� �������� "%s". ���������� ��������: "%s".';
  SIniValueDelete      = '������ �������� "%s.%s".';
  SRegValueDelete      = '������ �������� "%s\%s".';
  SIniSectionDelete    = '������� ������ "%s".';
  SIniValueSame        = '�������� �� ��������: ����� � ���������� �������� ���������.';

  SRegChangeValue      = '��������� ��������� "%s\%s" � ������� ������� "%s"';
  SRegDeleteValue      = '�������� ��������� "%s\%s" � ������� ������� "%s"';

  SErrTryWait          = '�� ����� ���������� �������� ��������� ������! ��������� ������� ( %d ) ����� ����������� ����� %d ������...';
  SErrSrcFileIsNull    = '�� ������� ��� ��������� �����!';
  SErrDstFileIsNull    = '�� ������� ��� ��������� �����!';
  SErrSelfCopy         = '������ ���������� ���� ��� � ����!';
  SErrDirsNotFound     = '������� "%s" �� ������!';
  SErrFileNotFound     = '���� "%s" �� ������!';
  SErrFilesNotFound    = '����(�) "%s" � �������� "%s" �� ������(�)!';
  SErrForceDirs        = '������ �������� �������� "%s": %s';
  SErrDeleteDir        = '������ �������� �������� "%s": %s';
  SErrOpenFile         = '������ ������ �����: %s';
  SErrCreateFile       = '������ �������� �����: %s';
  SErrReadFile         = '������ ������ �����: %s';
  SErrWriteFile        = '������ ������ �����: %s';
  SErrDeleteFile       = '������ �������� �����: %s';
  SErrCopyCheck        = '�������� � �������� ����� ���������� ���� �� �����!';
  SErrSetDateFile      = '������ ��������� ������� �������� �����: %s';
  SErrSetAttrFile      = '������ ��������� ��������� �����: %s';
  SErrDelphiError      = '������: %s!';
  SErrMapError         = '������ ����������� (����������) �����: %s!';
  SErrIniError         = '������ ��������� INI-�����: %s!';
  SErrRegError         = '������ ��������� �������: %s!';
  SErrRegTypeNone      = '��� �������� �� ������!';
  SErrValueNotExists   = '�������� "%s.%s" �� ������!';
  SErrSectionNotExists = '������ "%s" �� �������!';
  SErrKeyNotExists     = '���� "%s" �� ������!';
  SErrKeyNotOpened     = '������ �������� �����: "%s"!';
  SErrKeyNotFound      = '�������� "%s\%s" �� ������!';

const
  MaskDisk         = '*';
  MaskDelims       = [';'];

  MAX_WIDEPATH     = MAXSHORT;

{ == �������� � ������� (���������) ============================================ }

// ������ ������������ ��������
function CorrectFileName(const FileName: string; const ChkDelims, ChkParams: Boolean): string;
var
  sDiskStr, sPathStr: string;
begin
  Result := FileName;
  if Result <> EmptyStr then
  begin
    // ������� ":" ������ 2-�� �������
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

    // ������� ��� ��������� ������������ �������
    Result := AnsiReplaceText(AnsiReplaceText(AnsiReplaceText(
              AnsiReplaceText(AnsiReplaceText(AnsiReplaceText(
              Result, '"', ''''''),
              '<', '{'),
              '>', '}'),
              '*', '#'),
              '?', '_'),
              '  ', ' ');
  end;
  
  // ������� �����������
  if ChkDelims then
    Result := AnsiReplaceText(Result, '\\', '#');
  if ChkDelims then
    Result := AnsiReplaceText(Result, '\', '-');
  if ChkParams then
    Result := AnsiReplaceText(Result, '/', '-');
end;

// ��������� "����� �������" ( ����� > MAX_PATH (260) ) ���� ������ ------------
// ����� ��� ������ "����������" �� ������ SysUtils ----------------------------
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

// �������� �� �������������� � �������� ---------------------------------------
function IsDirectory(const FileName: string): Boolean;
begin
  Result := wDirectoryExists(ExcludeTrailingPathDelimiter(FileName))
    or IsPathDelimiter(FileName, Length(FileName));
end;

// ���� ����� ------------------------------------------------------------------
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

// ������ ����� ----------------------------------------------------------------
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

// ��������� � "������" ��������� ----------------------------------------------
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
  // �������� ����� �� ����
  sSrcSrv := ExtractFileDrive(SrcFileIn);
  sDstDrv := ExtractFileDrive(DstFileIn);
  // �������� ���������� ����� (�������� � ������ ����)
  sSrcDir := Copy(SrcFileIn, Length(sSrcSrv) + 1, Length(SrcFileIn) - Length(sSrcSrv));
  sDstDir := Copy(DstFileIn, Length(sDstDrv) + 1, Length(DstFileIn) - Length(sDstDrv));
  // ���������� ��� ����������� �� ������� ����������
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
  // ���� ���������� ���� ����������, �������� �� � ������� ���� �� "..."
  if iReplaceLen > 0 then
  begin
    sFindStr := EmptyStr;
    sReplStr := EmptyStr;
    // ������� ���������� ��� ������� 1 ������� ����� � 1 ������
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

// ��������� ������� "�������" � ������������ �������� -------------------------
function FileAttrInsertDirectory(const FileAttr: DWord): DWord;
begin
  Result := FileAttr;
  if (Result and faVolumeID) = 0 then Result := Result + faVolumeID;
  if (Result and faDirectory) = 0 then Result := Result + faDirectory;
end;

// ������� ������� "�������" �� ������������ ��������� -------------------------
function FileAttrRemoveDirectory(const FileAttr: DWord): DWord;
begin
  Result := FileAttr;
  if (Result and faVolumeID) > 0 then Result := Result - faVolumeID;
  if (Result and faDirectory) > 0 then Result := Result - faDirectory;
end;

// ��������� ������ ------------------------------------------------------------
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

{ == ����� ������ � ��������� ================================================== }

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

{ ������� �� TStringList }
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

// ����� ��������� �� ������� --------------------------------------------------
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
    // �������� ��������� ������� � ���� � ����
    CurrName := ExtractFileName(ExcludeTrailingPathDelimiter(DirName));
    CurrPath := ExtractFilePath(ExcludeTrailingPathDelimiter(DirName));
    if (CurrName = EmptyStr) or IsNetworkPath(CurrPath) then
    begin
      // ����� �� ���� ��������� ������
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
      // ����� �� �������� �����
      else begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := DirName;
      end;
    end
    else begin
      // ����������� ����� ��������� ���������
      IntDirList := FindDirectory(CurrPath, FileAttr, Transaction, BreakProc);
      // ����� �� ����������� ������ ��������� ���������
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

// ����� ��������� -------------------------------------------------------------
function CreateDirectoryList(const Directory: string; const FileAttr: DWord;
  DirList: TRFileList; const SubDirs, DirsForward: Boolean; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TRTransaction;
var
  i: Integer;
  DirectoryEx: string;
  RootDirList: TDirList;

  // ����� ��������� ���������
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
    // ������� ������ ��������� �� �������
    RootDirList := FindDirectory(DirectoryEx, FileAttr, Result, BreakProc);
    // ������������ ���������� ����������
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if Length(RootDirList) > 0 then
      begin
        for i := Low(RootDirList) to High(RootDirList) do
        begin
          if IsTerminatedTran(BreakProc, Result) then Break;
          // �������� ����������� ������������
          if DirsForward then
          begin
            // ��������� ������� ������� � ������
            DirList.Add(RootDirList[i], False);
            // ����� ��������� ������������
            if SubDirs and not IsTerminatedTran(BreakProc, Result) then
              ScanDirectory(RootDirList[i], Result);
          end
          else begin
            // ����� ��������� ������������
            if SubDirs and not IsTerminatedTran(BreakProc, Result) then
              ScanDirectory(RootDirList[i], Result);
            // ��������� ������� ������� � ������
            DirList.Add(RootDirList[i], False);
          end;
        end;
        // ���������� ���������� ������
        if not IsTerminatedTran(BreakProc, Result) then
          Result.Result := Format(SFindDirsCount, [DirList.Count]);
      end
      else Result.Result := Format(SFindDirsNotFound, [Directory]);
    end;
    // ������������� ������ ����������
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

// ����� ������ �� ������� -----------------------------------------------------
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
    // ������� ������ ���������
    DirectoryEx := wExpandUNCFileName(Directory);
    Result := CreateDirectoryList(DirectoryEx, FileAttr, DirList, SubDirs, DirsForward, EmptyState, ShowProc, BreakProc);
    if not IsTerminatedTran(BreakProc, Result) and (DirList.Count > 0) then
    begin
      // ����� ������ � ��������� ���������
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
            // ����� ������ �� ������� �����
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
        // ���������� ���������� ������
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
    // ����� ������ � ������ ���������
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
          // ����� ������ �� ������� �����
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
    // ���������� ���������� ������
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

// ����� ������ � ��������� �� ������� -----------------------------------------
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
    // ������� ������ ��������� �� �������
    RootDirList := FindDirectory(DirectoryEx, FileAttr, Result, BreakProc);
    // ������������ ���������� ����������
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
        // ���������� ���������� ������
        if not IsTerminatedTran(BreakProc, Result) then
          Result.Result := Format(SFindObjCount, [FileList.Count]);
      end
      else begin
        // �������� �� �������
        Result.Result := Format(SFindDirsNotFound, [Directory]);
      end;
    end;
    // ������������� ������ ����������
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

// �������� �� ������� ������ � �������� �� ������� ----------------------------
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
    // ������� ������ ���������
    DirectoryEx := wExpandUNCFileName(Directory);
    FR := CreateDirectoryList(DirectoryEx, FileAttr, DirList, SubDirs, True, ezError, ShowProc, BreakProc);
    if not IsTerminatedTran(BreakProc, FR) and (DirList.Count > 0) then
    begin
      // ����� ������ � ��������� ���������
      ShowMessageCustom(nil, Now, mcTransaction, msTitle, Format(SFindFilesTitle, [FileMasks, DirectoryEx]), ShowProc);
      iCount := DirList.Count - 1;
      for i := 0 to iCount do
      begin
        jCount := WordCount(FileMasks, MaskDelims);
        for j := 1 to jCount do
        begin
          if IsTerminatedTran(BreakProc, FR) then Break;
          // ����� ������ �� ������� �����
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

{ == ����������� (�����������) ����� =========================================== }

// ��������� �������� ����� (int) ----------------------------------------------
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

// ����������� (�����������) ����� ---------------------------------------------
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
  iMaxPos      = 100 + 1 + 1 + 1; // 100% ���� + �������������� + ������� ���� ������� + ������� ���������
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
      // ������� ��� ��������������� ������� ����
      if not wFileDelete(DestFile) then
        CheckAndRenameLockedFile(DestFile, SErrDeleteFile);
      // ��������������� ������������� ���� � �������
      if not wFileRename(SafeFile, DestFile) then
        raise ERfoError.CreateFmt(SErrCreateFile, [GetSystemError]);
    finally
      // � ������ ������ ��������������...
      // ���� ������������� ���� �� �����-�� �������� ��� ������� - ������� ���
      DeleteInvalidFile(SafeFile);
    end;
  end;

begin
  Result := 0;
  BufferSize := BufferSizeKb * 1024;
  // ��������� �������� ����
  SrcHandle := wFileOpen(SrcFile, fmOpenRead or fmShareDenyWrite); // SrcMode); //
  if SrcHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      // ��������� ������ �����
      // Fixed bug: 2012-09-28 - �� ��������� ������� ������ ����� 2Gb
      OrgnSize := FileHandleGetSize(SrcHandle, True);
      // ���� ������ ����� ������ ���������� ������� ������ - ���������� ���
      if (OrgnSize > 0) and (OrgnSize < BufferSize) then
        BufferSize := OrgnSize;
      // ����������, ����� �� "���������" ��� ����������� �����
      CurrNetUsage := CalculateNetUsage(NetUsageProc, NetUsage);
      SleepCopyMode := (CurrNetUsage < 100) and (OrgnSize > BufferSize)
        and (IsNetworkPath(SrcFile) or IsNetworkPath(DstFile));
      // ���������� ��� ������������� �����
      TargetFile := DstFile;
      if SafeOverwrite and wFileExists(DstFile) then
        TargetFile := CreateSafeFileName(DstFile);
      // ��������� ���� �� ������ (�� ������ �������!!!)
OpenDstFile:
      // ��������� ����
      DstHandle := wFileCreate(TargetFile);
      if DstHandle = INVALID_HANDLE_VALUE then
      begin
        // ������: �� ������� ������� ���� �� ������ - ������� ������������� ��� � ��������� �������
        CheckAndRenameLockedFile(DstFile, SErrCreateFile);
        goto OpenDstFile;
      end
      else begin
        // �������������� ��������� ��������� �����������
        ShowProgressFile(nil, 0, iMaxPos, PrgsFile);
        try
          // ������������� ��� ������ ����������� - � ������ ���� ����� ������� ��������� "������" ����
          try
            // � ����� ������� ��������� ���� ����� �������...
            try
              // �������� ������ ��� ����� �����������
              GetMem(Buffer, BufferSize);
              try
                // �������������� ������� "�� �������������" ������
                FileSize := OrgnSize;
                // �������������� ������� ������� % �������� ����
                CalcSize := 0;
                // ���� ������� "�� �������������" ������ ������ ����, ��������� � �����...
                while not CopyBreak and (FileSize > 0) do
                begin
                  // ���������� ��������� �����
                  StartTick := GetTickCount;
                  // ���������� ������ ����������� ������
                  if FileSize > BufferSize
                  then ReadSize := BufferSize
                  else ReadSize := FileSize;
                  // ��������� ���� ������
                  if ReadSize <> FileRead(SrcHandle, Buffer^, ReadSize) then
                    raise ERfoError.CreateFmt(SErrReadFile, [GetSystemError]);
                  // ���������� ��������� ���� ������
                  if ReadSize <> FileWrite(DstHandle, Buffer^, ReadSize) then
                    raise ERfoError.CreateFmt(SErrWriteFile, [GetSystemError]);
                  // ��������� ������� "�� �������������" ������ � ���������
                  Dec(FileSize, ReadSize); // FileSize := FileSize - ReadSize;
                  Inc(Result, ReadSize); // Result := Result + ReadSize;
                  // ��������� ��������� ���������
                  if OrgnSize > 0
                  then Percent := Round(100 * (Result / OrgnSize))
                  else Percent := 0;
                  ShowProgressFile(nil, Percent, iMaxPos, PrgsFile);
                  // ���� �������� ������ ����������� ����� 100%, "��������������" �����������
                  if SleepCopyMode and (FileSize > 0) then
                  begin
                    // ��������� ����� ����������� �������� �����
                    OperTick := GetTickCount - StartTick;
                    // ��������� ����� �������� � ����...
                    if CurrNetUsage < 1 then CurrNetUsage := 1;
                    Sleep(Round(((100 * OperTick) / CurrNetUsage) - OperTick));
                    // ����������� ������� ������� % �������� ����
                    Inc(CalcSize, 1);
                    // ���� ���������� ����� � 10 �������� ������, ��������� % �������� ����
                    if CalcSize > 9 then
                    begin
                      CalcSize := 0;
                      CurrNetUsage := CalculateNetUsage(NetUsageProc, NetUsage);
                    end;
                  end;
                end;
              finally
                // ������� ����� �����������
                FreeMem(Buffer, BufferSize);
              end;
            finally
              // ��������� ������������� ����
              FileClose(DstHandle);
              // ����� ����������� �������� �������������� �������� ������� ��������� � �������������� �����
              if not CopyBreak and ((OrgnSize <> Result) or (OrgnSize <> wFileGetSize(TargetFile, True))) then
                raise ERfoError.Create(SErrCopyCheck);
            end;
          except
            // � ������ ������ ����������� ������� ��������� ������� ����
            DeleteInvalidFile(TargetFile);
            raise;
          end;
          // ���� ����������� ������ �������...
          if not CopyBreak and wFileExists(TargetFile) then
          begin
            // 1) ... ����� �������� � �������������� ����� �� ���������
            ShowProgressFile(nil, iMaxPos - 3, iMaxPos, PrgsFile);
            if not SameFileName(DstFile, TargetFile) then
              RenameSafeFile(TargetFile, DstFile);
            // 2) ... ��������� ���� �������� �����
            ShowProgressFile(nil, iMaxPos - 2, iMaxPos, PrgsFile);
            if TransferTime then
            begin
              ErrorCode := wFileSetWriteTime(DstFile, SrcCreationTime, SrcLastWriteTime);
              if ErrorCode <> 0 then
                raise ERfoError.CreateFmt(SErrSetDateFile, [GetSystemError(ErrorCode)]);
            end;
            // 3) ... ��������� �������� (��������� ����� ��� ��������, ���� �� �����������)
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
          // ��������� ��������� ���������
          ShowProgressFile(nil, 0, -1, PrgsFile);
        end;
      end;
    finally
      // ��������� �������� ����
      FileClose(SrcHandle);
    end;
  end
  // ������: �� ������� ������� ���� �� ������
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
  // ��������� ��� �������� ����������
  SrcDirEx := ExcludeTrailingPathDelimiter(wExpandUNCFileName(SrcDir));
  DstDirEx := ExcludeTrailingPathDelimiter(wExpandUNCFileName(DstDir));
  // ��������� ������������ ��������
  if SameText(ExtractFileName(SrcDirEx), ExtractFileName(DstDirEx)) then
  begin
    CompressFilePaths(ExtractFilePath(SrcDirEx), ExtractFilePath(DstDirEx), SrcDirSh, DstDirSh);
    // ����� ��������� � ��������� ������ ��������� - ����������� �������
    if DelSrcDir
    then Result.Title := Format(SFileMoveTitleEx, [ExtractFileName(SrcDirEx), SrcDirSh, DstDirSh])
    else Result.Title := Format(SFileCopyTitleEx, [ExtractFileName(SrcDirEx), SrcDirSh, DstDirSh]);
  end
  else begin
    CompressFilePaths(ExtractFilePath(SrcDirEx), ExtractFilePath(DstDirEx), SrcDirSh, DstDirSh);
    SrcDirSh := SrcDirSh + ExtractFileName(SrcDirEx);
    DstDirSh := DstDirSh + ExtractFileName(DstDirEx);
    // ����� ��������� � ��������� ������ �� ��������� - ������ �������
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
          // �������� �� ����������� "� ����"
          if SameText(SrcDirEx, DstDirEx) then
            raise ERfoError.Create(SErrSelfCopy);
          // ��������� ������������� ��������� ��������
          if wDirectoryExists(DstDirEx) then
          begin
            Result.State := msIgnored;
            Result.Result := SDirectoryExists;
            Exit;
          end;
          // ��������� �������� ��������
          SrcDirAttr := wFileGetAttr(SrcDirEx);
          // ��������� �������� ��������
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
          // ������� �������
          CurrOpName := Result.Title;
          Result := ForceDirsEx(DstDirEx, not (rfForceDirs in Options), ShowProc, BreakProc);
          Result.Title := CurrOpName;
          // ��������� �������� ��������
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
  // ��������� ��� ����� ����������
  SrcFileEx := wExpandUNCFileName(SrcFile);
  DstFileName := UpdateDstFileName(SrcFileEx, wExpandUNCFileName(DstFile));
  // ��������� ������������ ��������
  if SameText(ExtractFileName(SrcFileEx), ExtractFileName(DstFileName)) then
  begin
    CompressFilePaths(ExtractFilePath(SrcFileEx), ExtractFilePath(DstFileName), SrcFileSh, DstFileSh);
    // ����� ��������� � ��������� ������ ��������� - ����������� �������
    if DelSrcFile
    then Result.Title := Format(SFileMoveTitleEx, [ExtractFileName(SrcFileEx), SrcFileSh, DstFileSh])
    else Result.Title := Format(SFileCopyTitleEx, [ExtractFileName(SrcFileEx), SrcFileSh, DstFileSh]);
  end
  else begin
    CompressFilePaths(ExtractFilePath(SrcFileEx), ExtractFilePath(DstFileName), SrcFileSh, DstFileSh);
    SrcFileSh := SrcFileSh + ExtractFileName(SrcFileEx);
    DstFileSh := DstFileSh + ExtractFileName(DstFileName);
    // ����� ��������� � ��������� ������ �� ��������� - ������ �������
    if DelSrcFile
    then Result.Title := Format(SFileMoveTitle, [SrcFileSh, DstFileSh])
    else Result.Title := Format(SFileCopyTitle, [SrcFileSh, DstFileSh]);
  end;
  // ��������� �������� �� ���� (���� ����� 100%)
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
          // �������� �� ����������� "� ����"
          if SameText(SrcFileEx, DstFileName) then
            raise ERfoError.Create(SErrSelfCopy);

          // ��������� �������� ��������� �����
          wFileGetWriteTime(SrcFileEx, SrcCreationTime, SrcLastWriteTime);
          SrcFileAttr := wFileGetAttr(SrcFileEx);
          DstFileAttr := 0;

          // ��������� ������� ��������� �����
          if wFileExists(DstFileName) then
          begin
            // ��������� �������� ��������� �����
            if not (rfCopyFileAttr in Options) then
              DstFileAttr := wFileGetAttr(DstFileName);

            // ���������, ��������� �� ����������
            if rfOverWrite in Options then
            begin
              // ... ���� �� ���������� �� ���� �� ������ ��������� - ���������� ���������
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
            // ... ���������� ��������� � ����� ������
            else CopySkip := True;
            if CopySkip then
            begin
              // ���������� ���� ...
              Result.State := msIgnored;
              Result.Result := SFileCopySkip;
              Exit;
            end;
          end; // ��������� ������� ��������� �����

          // ��������� �������� ��������� �����
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

          // ��������� �������� ������������� ��������� �����
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

          // ������� ������� ���������� ��� �������������
          if (Result.State = msOk)
          and not IsTerminatedTran(BreakProc, Result)
          and (rfForceDirs in Options)
          and not wDirectoryExists(ExtractFilePath(DstFileName)) then
          begin
            if wForceDirectories(ExtractFilePath(DstFileName)) then
            begin
              // ��������� �������� �������� ��� �������������
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

          // ���������� ����������� �����
          if (Result.State = msOk) and not IsTerminatedTran(BreakProc, Result) then
          begin
            // 2013-09-03: ������� ���� � ��������� ����� ��������� ���������������� � CopyFileBuf
            CopySize := CopyFileBuf(SrcFileEx, DstFileName,
              rfCopyBackup in Options, rfCopyLocked in Options,
              rfCopyFileDate in Options, rfCopyFileAttr in Options,
              SrcCreationTime, SrcLastWriteTime, SrcFileAttr, DstFileAttr,
              BufferSizeKb, NetUsage, NetUsageProc, PrgsFile, BreakProc);
            // ���������� �����
            if CheckTransactionResult(DstFileName, Result) then
            begin
              if (rfCheckCopy in Options)
              and not CompareFiles(SrcFileEx, DstFileName, rfCopyFileDate in Options) then
              begin
                Result.State := msError;
                Result.Result := SErrCopyCheck;
                // 2013-11-11: bug fix: �� ��������� ����, ���� ������ ��������� ������
                try
                  wFileDelete(DstFileName);
                except
                end;
              end;
              // ��������� � ������ ������������� ������
              if CheckTransactionResult(DstFileName, Result) then
              begin
                if (rfCopyList in Options) and Assigned(CopyList) then
                  CopyList.Add(SrcFileEx, DstFileName);
                // ���������� ��������� � ��������� ����
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
                end; // ���������� ��������� � ��������� ����
              end; // ��������� � ������ ������������� ������
            end; // ���������� �����
          end; // ���������� ����������� �����
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

{ 20130802 - fix bug: ���������� ������� "������" ���� � ������ ������ ��� �����������

    // ��������� ��������
    if not IsTerminatedTran(BreakProc, Result) then
    begin
      if rfCopyFileDate in Options then
        (* SaveFileAge(DstFileName, SrcFileDate, Result); *)
        SaveFileTime(DstFileName, SrcCreationTime, SrcLastWriteTime, Result);
      if rfCopyFileAttr in Options then
        SaveFileAttributes(DstFileName, SrcFileAttr, Result);
    end;
    // ���������� �����
    if (Result.State = msOk) and not IsTerminatedTran(BreakProc, Result)
    and (rfCheckCopy in Options)
    and not CompareFiles(SrcFileEx, DstFileName, rfCopyFileDate in Options) then
    begin
      Result.State := msError;
      Result.Result := SErrCopyCheck;
      Exit;
    end;
    // ��������� � ������
    if (Result.State = msOk) and not IsTerminatedTran(BreakProc, Result)
    and (rfCopyList in Options) and Assigned(CopyList) then
      CopyList.Add(SrcFileEx + CopyListTab + DstFileName);
    // ���������� ���������
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

20130802 - fix bug: ���������� ������� "������" ���� � ������ ������ ��� �����������}

{ 20130802 - ������� �������� �������� ��������� (�� ��������� �����)

// ��������� �������� ��������� ����� ...
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
// ���������� ���������� ��������
if (Result.State = msOk) and (DstFileAttr <> wFileGetAttr(DstFileName)) then
  SaveFileAttributes(DstFileName, DstFileAttr, Result);

20130802 - ������� �������� �������� ��������� (�� ��������� �����) }

// ������������� ����� ---------------------------------------------------------
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

// �������� ����� --------------------------------------------------------------
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
        // ��������� ��������
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
        // ���������� ���������� ��������
        if (Result.State = msOk) and (FileAttr <> wFileGetAttr(FileNameEx)) then
          SaveFileAttributes(FileNameEx, FileAttr, Result);
        // ������� ����
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

// �������� �������� -----------------------------------------------------------
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
        // ��������� ��������
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
        // ��������� �� "�������"
        if (rfDstFixed in Options) and not DirIsEmpty(DirNameEx) then
        // and FilesExists(DirNameEx, MaskAll, faAnyFile, True, nil, BreakProc) then
        begin
          Result.State := msIgnored;
          Result.Result := SDirectoryDirNotEmpty;
          Exit;
        end;
        // ���������� ���������� ��������
        if (Result.State = msOk) and (FileAttr <> wFileGetAttr(DirNameEx)) then
          SaveFileAttributes(DirNameEx, FileAttr, Result);
        // ������� �������
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

// �������� �������� -----------------------------------------------------------
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

// ����� �������� --------------------------------------------------------------
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

// ������ ����� ----------------------------------------------------------------

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

// ������� ���� ----------------------------------------------------------------
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

// ������� ����� ---------------------------------------------------------------
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

// ����������� � ���������� ������� ������ -------------------------------------
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


{ == �������� �������� � ������� ������� ======================================= }

// ����������� ������ �� ����� -------------------------------------------------
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
    // ������� ������ ������
    Transaction := FindMasks(SrcDirEx, Masks, faAnyFile, rfSrcSubDirs in Options,
      True, FileList, EmptyState, ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
    if not bBreak and (FileList.Count > 0) then
    begin
      Inc(Result.Total, FileList.Count);
      // ��������� "�������" ����� ����� ����� ���������
      if rfDstDirAuto in Options
      then OffsetBase := OffsetDstFileBase(FileList)
      else OffsetBase := SrcDirEx;
      // ��������� MoveDelOptions ��� �������� ������ ���������
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
      // ������������ ���������� ���� ������
      ShowProgressCustom(nil, mcTransaction, 0, FileList.Count, PrgsProc);
      iCount := FileList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedOper(BreakProc, Result, bBreak) then Break;
        if not FileList.Items(i).bFile then // IsDirectory(FileList[i]) then
        begin
          // ������� ������� (��� ����������� ��������� ������!!!)
          Transaction := CopyDirEx(FileList.Items(i).sName,
            OffsetDstFileName(OffsetBase, DstDirEx, FileList.Items(i).sName, (rfDstFixed in Options), True),
            DelSrcFile, Options, ShowProc, BreakProc);
          // ��������� � ����� ���������
          PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
          // ��������� � ������ �� �������� ��������� ��������
          if DelSrcFile and not (Transaction.State in [msError, msBreak]) then
            MoveList.Add(ExcludeTrailingPathDelimiter(FileList.Items(i).sName), False);
        end
        else begin
          TryNum := 1;
          repeat
            // ������� ���������� ����
            Transaction := CopyFileEx(FileList.Items(i).sName, OffsetDstFileName(OffsetBase, DstDirEx, FileList.Items(i).sName,
              ((rfDstFixed in Options) or ((ExtractFileExt(DstDir) <> EmptyStr) and (FileList.Count = 1))),
              (FileList.Count > 1)), DelSrcFile, Options, CopyList, BufferSizeKb, NetUsage,
              NetUsageProc, ShowProc, PrgsFile, BreakProc);
            // ���� ������� �� ������, ��������� ����� �������
            if TryNum > 1 then
              Transaction.Title := Transaction.Title + Format(SMsgTryNumber, [TryNum]);
            // ��������� � ����� ���������
            PutTransactionToOperation(Transaction, Result, ErrOptions,
              (TryNum >= TryMax) or not (rfRepeatOnError in Options),
              bBreak, ShowProc);
            // ���� ������� �� ��������� � ������, ���� ��������� ����� ������
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
            // ����������� ������� �������
            Inc(TryNum);
          until IsTerminatedOper(BreakProc, Result, bBreak)
            or not (rfRepeatOnError in Options)
            or (TryNum > TryMax) or (Transaction.State <> msError);
        end;
        // �������� �� ������ ��������� �����
        if (Transaction.State in [msError, msBreak])
          and (esErrorStop in ErrOptions) then
            Break;
        // ������� ������� ����������� � ������ �������� ��������
        if not IsTerminatedOper(BreakProc, Result, bBreak)
        // ... ���� � ������ �� �������� ���-�� ����
        and (MoveList.Count > 0)
        // ... � ��� �� ��������� ���� / ������� � ������
        and (i < FileList.Count - 1) then
        begin
          // ���������� ����� ��������� ��� ������� � �������� ������ � ������
          if FileList.Items(i).bFile
          then sCurrFileDir := ExtractFilePath(FileList.Items(i).sName)
          else sCurrFileDir := FileList.Items(i).sName;
          if FileList.Items(i + 1).bFile
          then sNextFileDir := ExtractFilePath(FileList.Items(i + 1).sName)
          else sNextFileDir := FileList.Items(i + 1).sName;
          // �������� ���������� ����� � ����������� ����
          sCurrFileDir := ExcludeTrailingPathDelimiter(sCurrFileDir);
          sNextFileDir := ExcludeTrailingPathDelimiter(sNextFileDir);
          // ������ ���� �������� ������� ���������� �� ���������� ������������ � ������
          if (FileList.Items(i).bFile <> FileList.Items(i + 1).bFile)
          or not AnsiSameText(sCurrFileDir, sNextFileDir) then
          begin
            // ������� ������� ������ ��������� "�����-�����"
            jCount := MoveList.Count - 1;
            for j := jCount downto 0 do
              // ��������������� �������� �� �������� �������� (��� ��������� ��������)
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
        // ��������� ���������
        ShowProgressCustom(nil, mcTransaction, i + 1, FileList.Count, PrgsProc);
      end;
      // ������� ���������� �������� � ������ �� ��������
      if not IsTerminatedOper(BreakProc, Result, bBreak) then
      begin
        for j := MoveList.Count - 1 downto 0 do
        begin
          Transaction := DeleteFolderEx(MoveList.Items(j).sName, MoveDelOptions, ShowProc, BreakProc);
          // ��������� � ����� ���������
          PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
          // ������� ������� �� ������
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

// ����������� ������ �� ����� -------------------------------------------------
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

// ����������� ������ �� ������ ------------------------------------------------
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
      // ������������ ���������� ���� ������
      ShowProgressCustom(nil, mcTransaction, 0, CopyList.Count, PrgsProc);
      iCount := CopyList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedOper(BreakProc, Result, bBreak) then Break;
        TryNum := 1;
        repeat
          // ������� ���������� ����
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
          // ���� ������� �� ������, ��������� ����� �������
          if TryNum > 1 then
            Transaction.Title := Transaction.Title + Format(SMsgTryNumber, [TryNum]);
          // ��������� � ����� ���������
          PutTransactionToOperation(Transaction, Result, ErrOptions,
            (TryNum >= TryMax) or not (rfRepeatOnError in Options),
            bBreak, ShowProc);
          // ���� ������� �� ��������� � ������, ���� ��������� ����� ������
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
          // ����������� ������� �������
          Inc(TryNum);
        until bBreak or IsTerminatedOper(BreakProc, Result, bBreak)
          or (TryNum > TryMax) or (Transaction.State <> msError);
        // �������� �� ������ ��������� �����
        if (Transaction.State in [msError, msBreak])
          and (esErrorStop in ErrOptions) then
            Break;
        // ��������� ���������
        ShowProgressCustom(nil, mcTransaction, i + 1, CopyList.Count, PrgsProc);
      end;
    end;
  finally
    DoneOperation(Result, EmptyState, ShowProc);
  end;
end;

// ���������� ������ �� ����� --------------------------------------------------
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
    // ������� ������ ������ ����������
    Transaction := CreateFileList(DstDirEx, Masks, faAnyFile, DstFileList,
      rfSrcSubDirs in Options, True, EmptyState,  ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
    // ������� ������ ��������� ������
    Transaction := CreateDirectoryList(SrcDirEx, faAnyFile, ScrScanList,
      not (rfDstFixed in Options), True, EmptyState, ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
    if not bBreak and (DstFileList.Count > 0) then
    begin
      Inc(Result.Total, DstFileList.Count);
      // ������������ ���������� ���� ������
      ShowProgressCustom(nil, mcTransaction, 0, DstFileList.Count, PrgsProc);
      iCount := DstFileList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedOper(BreakProc, Result, bBreak) then Break;
        // ���� ����� ��� ���������� � ��������� ������ ��������
        SrcFileList.Clear;
        Transaction := CreateFileListInDirList(ScrScanList, ExtractFileName(DstFileList.Items(i).sName),
          faAnyFile, SrcFileList, ezOk, ShowProc, BreakProc);
        if (SrcFileList.Count > 0) then
        begin
          TryNum := 1;
          repeat
            // ������� ���������� ����
            Transaction := CopyFileEx(SrcFileList.Items(0).sName, DstFileList.Items(i).sName,
              False, Options, CopyList, BufferSizeKb, NetUsage, NetUsageProc, ShowProc, PrgsFile, BreakProc);
            // ��������������� �������
            Transaction.Title := Format(SFileUpdateTitleEx, [ExtractFileName(DstFileList.Items(i).sName),
              ExtractFilePath(SrcFileList.Items(0).sName), ExtractFilePath(DstFileList.Items(i).sName)]);
            // ���� ������� �� ������, ��������� ����� �������
            if TryNum > 1 then Transaction.Title := Transaction.Title + Format(SMsgTryNumber, [TryNum]);
            // ��������� � ����� ���������
            PutTransactionToOperation(Transaction, Result, ErrOptions,
              (TryNum >= TryMax) or not (rfRepeatOnError in Options),
              bBreak, ShowProc);
            // ���� ������� �� ��������� � ������, ���� ��������� ����� ������
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
            // ����������� ������� �������
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
        // �������� �� ������ ��������� �����
        if (Transaction.State in [msError, msBreak])
          and (esErrorStop in ErrOptions) then
            Break;
        // ��������� ���������
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

// �������� ������ ��� ��������� �� ������� ------------------------------------
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
    // ������� ������ ��������� (�.�. �������� ������� ��������� �� �����)
    if rfDstFixed in Options
    then Transaction := CreateDirectoryList(FileMaskEx, faAnyFile, FileList,
      rfSrcSubDirs in Options, False, EmptyState, ShowProc, BreakProc)
    else Transaction := FindMasks(DirName, MaskName, faAnyFile, rfSrcSubDirs in Options,
      False, FileList, EmptyState, ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, True, bBreak, ShowProc);
    if not bBreak and (FileList.Count > 0) then
    begin
      Inc(Result.Total, FileList.Count);
      // ������������ ���������� ���� ������
      ShowProgressCustom(nil, mcTransaction, 0, FileList.Count, PrgsProc);
      iCount := FileList.Count - 1;
      for i := 0 to iCount do
      begin
        if IsTerminatedOper(BreakProc, Result, bBreak) then Break;
        TryNum := 1;
        repeat
          // ������� ������� ���� ��� �������
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
          // ���� ������� �� ������, ��������� ����� �������
          if TryNum > 1 then
            Transaction.Title := Transaction.Title + Format(SMsgTryNumber, [TryNum]);
          // ��������� � ����� ���������
          PutTransactionToOperation(Transaction, Result, ErrOptions,
            (TryNum >= TryMax) or not (rfRepeatOnError in Options),
            bBreak, ShowProc);
          // ���� ������� �� ��������� � ������, ���� ��������� ����� ������
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
          // ����������� ������� �������
          Inc(TryNum);
        until IsTerminatedOper(BreakProc, Result, bBreak)
          or not (rfRepeatOnError in Options)
          or (TryNum > TryMax) or (Transaction.State <> msError);
        // �������� �� ������ ��������� �����
        if (Transaction.State in [msError, msBreak])
          and (esErrorStop in ErrOptions) then
            Break;
        // ��������� ���������
        ShowProgressCustom(nil, mcTransaction, i + 1, FileList.Count, PrgsProc);
      end;
    end;
  finally
    FileList.Free;
    DoneOperation(Result, EmptyState, ShowProc);
  end;
end;

// ������� �������� ------------------------------------------------------------
function ClearDirectory(const DirName: string; Options: TRfoFileFlags;
  const EmptyState: TREmptyState; const ErrOptions: TRErrorsOptions;
  const TryMax: Byte; const TryDelay: Word; const ShowProc: TRShowInfoNotifyEvent;
  const PrgsProc: TRShowProgressNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent): TROperation;
begin
  Result := DeleteMask(IncludeTrailingPathDelimiter(DirName) + MaskAll,
    Options, EmptyState, ErrOptions, TryMax, TryDelay, ShowProc, PrgsProc, BreakProc);
end;

{ == �������� � INI-������� � �������� ========================================= }

// ��������� INI-����� ---------------------------------------------------------
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
          // ��������� ��������
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
        // ��������� Ini-����
        if not IsTerminatedTran(BreakProc, Result) then
        begin
          try
            Ini := TMemIniFile.Create(FileNameEx);
            try
              if riDeleteSection in Options then
              begin
                // *** �������� ������ ***
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
                  // *** �������� �������� ***
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
                  // *** ��������� �������� ***
                  // ��������� ������� ��������
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
                    // ��������� ���������� ��������
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

// ��������� INI-������ �� ����� -----------------------------------------------
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
    // ������� ������ ��������� (�.�. �������� ������� ��������� �� �����)
    Transaction := CreateFileList(DirName, MaskName, faAnyFile, FileList, riSrcSubDirs in Options,
      True, EmptyState, ShowProc, BreakProc);
    PutTransactionToOperation(Transaction, Result, ErrOptions, not (riCreateValue in Options), bBreak, ShowProc);
    // ������������� ��������� ��� ����� � ������, ���� ��������� ��������
    if (FileList.Count = 0) and (riCreateValue in Options) then
      FileList.Add(FileMaskEx, True);
    if not bBreak and (FileList.Count > 0) then
    begin
      Inc(Result.Total, FileList.Count);
      // ������������ ���������� ���� ������
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

// ��������� ������� -----------------------------------------------------------
function DecodeRegistryRootKey(const RootKey: string): Cardinal;
const
  RootId : array [HKEY_CLASSES_ROOT..HKEY_DYN_DATA] of array [1..4] of string = (
    ('HKEY_CLASSES_ROOT', 'HK_CLASSES_ROOT', 'HK_CR', 'HKCR'),
    ('HKEY_CURRENT_USER', 'HK_CURRENT_USER', 'HK_CU', 'HKCU'),
    ('HKEY_LOCAL_MACHINE', 'HK_LOCAL_MACHINE', 'HK_LM', 'HKLM'),
    ('HKEY_USERS', 'HK_USERS', 'HK_US', 'HKUS'),
    ('HKEY_PERFORMANCE_DATA', 'HK_PERFORMANCE_DATA', 'HK_PD', 'HKPD'),
    ('HKEY_CURRENT_CONFIG', 'HK_CURRENT_CONFIG', 'HK_��', 'HK��'),
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
          // ��������� �������� ������
          Reg.RootKey := DecodeRegistryRootKey(Root);
          if rgDeleteValue in Options then
          begin
            if Reg.KeyExists(Path) then
            begin
              // ��������� ����
              if Reg.OpenKey(Path, rgCreateValue in Options) then
              begin
                try
                  // ��������� ������� ���������
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
            // ��������� ������� �����
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
              // ��������� ����
              if Reg.OpenKey(Path, rgCreateValue in Options) then
              begin
                try
                  // ��������� ������� ���������
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
                    // ���������� ���������� �������� � ����� � ������
                    sPrevValue := '';
                    case DecodeRegistryTypeValue(TypeValue) of
                      // ������
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
                      // ����������� ������
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
                      // ����� �����
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
                      // ����� � ��������� �������
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
                    // ���������� ���������
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
