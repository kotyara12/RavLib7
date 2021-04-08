unit RScrCnst;

interface

uses
  Windows, Graphics, Classes, RMsgTypes, RFileProcs;

type
  TRCmdRetFlag   = (rfNoChange, rfOk, rfWarning, rfError);
  TRCmdErrAttr   = (eaContinue, eaRestore, eaRunUndo);
  TRFncOperator  = (coNone, coAnd, coOr);
  TRCmdErrAttrs  = set of TRCmdErrAttr;
  TRScriptState  = (stOk, stWarning, stError, stErrorStop, stErrorRestore, stErrorUndo);
  TRScriptDate   = (dmFullAuto, dmFullFixed, dmTimeAuto);
  (* TRLogMode      = (lmDisabled, lmSingle, lmOperation, lmPacketSize, lmMixed); *)
  TRCmdTokenType = (ttUnknown, ttNotEnabled, ttNotEdited, ttVarList, ttVarsId, ttVarCommand, ttExeCommand, ttCondition, ttCycle);
  TRCmdExtType   = (ctScript, ctCommand);
  TRMsgBoxType   = (mbInfo, mbWarning, mbError);
  TRScrRunState  = (rsOk, rsWarning, rsAny, rsNone);
  TRScrResState  = (rsApply, rsIgnore);
  TRScrObjType   = (otScript, otProc, otAuto);

  TScriptShowMode = (smHideAll, smLogDisable, smLogHidden, smLogShowing, smLogAlways);

  TRCmdType      = (cmSet, cmInfo, cmReturn, cmBreak,
     cmSetState, cmPause, cmWinWait, cmCall, cmChangeDir, cmFDirs,
     cmCopy, cmXCopy, cmMove, cmXMove, cmRename, cmDelete, cmUpdate,
     cmExec, cmExecEx, cmExecAs, cmOpen, cmOpenEx, cmLink, cmLinkEx, cmMap, cmUnmap,
     cmChIni, cmIniReadVariable, cmIniReadSections, cmIniReadKeys,
     cmChReg, cmRegReadVariable, cmRegReadSections, cmRegReadKeys,
     cmBdeAliasCreate, cmBdeAliasDelete,
     cmAddPrinter, cmDelPrinter, cmAddNetPrinter, cmDelNetPrinter,
     cmFreeObject, cmFindDirs, cmFindFiles,
     cmListFirst, cmListPrior, cmListNext, cmListLast, cmFileAppend,
     cmTextCreate, cmTextOpen, cmTextSave, cmTextAddChars, cmTextAddLine, cmTextReplace, cmTextSet,
     cmSubStrings, cmSubString, cmStrReplace, cmExtractFileName, cmExtractFilePath, cmExtractFileExt, cmChangeFileExt, cmCheckFileName,
     cmExitWindows, cmStartService, cmStopService,
     cmCalcCrc32, cmScriptSave, cmChkVar,
     cmFtpOpen, cmFtpCurrDir, cmFtpChDir, cmFtpMkDir, cmFtpFrDir, cmFtpRmDir, cmFtpGet, cmFtpPut, cmFtpDelete, cmFtpClose,
     cmShowVars,
     cmIf, cmWhile, cmRepeat,
     cmForEach, cmForFile, cmForDirs);
  TRFncType     = (ftStateIsOk, ftStateIsGood, ftStateIsWarning, ftStateIsError, 
     ftVarExists, ftVarIs, ftKeyExists, ftWinExists, ftTaskExists, ftQuery,
     ftDiskFree, ftFileExists, ftDirExists, ftMaskExists,
     fnTimeAfter, fnTimeBefore, fnIsAdmin,
     fnBdeEnabled, fnBdeCheckAlias, fnPrinterExists,
     fnIniSecExists, fnIniKeyExists, fnIniValCompare,
     fnRegValueExists, fnRegKeyExists, fnRegKeyCompare,
     fnObjIsExists, fnObjIsList, fnListIsNotEmpty, cmListIsFirst, cmListIsLast, cmTextFind,
     fnSelectFile, fnSelectDir, fnServiceExists, fnServiceStarted, fnServiceStopped);

  TRCmdParams = array of string;

  TRCmdData = packed record
    fParams: TRCmdParams;
    fFlagsDefault: Boolean;
    fFlags: string;
    fNotesDefault: Boolean;
    fNotes: string;
  end;

  (*
  TRCmdMessage = packed record
    ScriptId: Integer;
    TimeStamp: TDateTime;
    MsgClass: TRMsgClass;
    MsgState: TRMsgState;
    MsgText: string;
  end;

  TRCmdBuffer = array of TRCmdMessage;
  *)

  TRListPosition = class
  private
    fListPosition: Integer;
    function GetPosition: Integer;
  public
    procedure SetPosition(const Value: Integer);
    constructor Create;
    function Eof: Boolean;
    function Bof: Boolean;
    function Empty: Boolean;
    function Count: Integer; virtual; abstract;
    function PositionValue: string; virtual; abstract;
    function Value(const Index: Integer): string; virtual; abstract;
    function Text: string; virtual; abstract;
    procedure First;
    procedure Prior;
    procedure Next;
    procedure Last;
    property Position: Integer read GetPosition write SetPosition;
  end;

  CRListPosition = class of TRListPosition;

  TRStringListPos = class (TRListPosition)
  private
    fStrings: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer; override;
    function PositionValue: string; override;
    function Value(const Index: Integer): string; override;
    function Item: string;
    property Items: TStringList read fStrings write fStrings;
    function Text: string; override;
  end;

  TRFileListPos = class (TRListPosition)
  private
    fFileList: TRFileList;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer; override;
    function PositionValue: string; override;
    function Value(const Index: Integer): string; override;
    function Item: TRFileItem;
    property Items: TRFileList read fFileList write fFileList;
    function Text: string; override;
  end;

  TRUpdateControlsNotifyEvent = procedure (Sender: TObject;
    const BreakEnabled, CloseEnabled: Boolean) of object;
  TSelectVariableNotifyEvent = procedure (Sender: TObject;
    var VarName: string; var Selected: Boolean) of object;
  TLoadExtScriptNotifyEvent = procedure (Sender: TObject;
    const Alias: string; var Completed: Boolean) of object;
  TRSaveToLogNotifyEvent = procedure (Sender: TObject; const ScriptId: Integer; const ScriptName: string;
    const TimeStamp: TDateTime; const MsgClass: TRMsgClass; const MsgState: TRMsgState;
    const MsgText: string) of object;
  (* TRStoreMessageNotifyEvent = procedure (Sender: TObject;
    const CmdMessage: TRCmdMessage) of object; *)
  (* TRStoreBufferNotifyEvent = procedure (Sender: TObject;
    const CmdBuffer: TRCmdBuffer) of object; *)
  TRGetScriptName = procedure (Sender: TObject;
    var ScriptName: string) of object;
  TRShowStateNotifyEvent = procedure (Sender: TObject;
    const Global: Boolean; const State: TRScriptState) of object;
  TRExecOnExternalProcessor = procedure (Sender: TObject; const CmdType: TRCmdExtType;
    const CmdText: string; const ScriptName: string; const AliasName: string; const RunProcName: string;
    const ShowInfo: TRShowInfoNotifyEvent; const ShowProgress: TRShowProgressNotifyEvent;
    const FileProgress: TRFileProgressNotifyEvent; var State: TRScriptState) of object;

const
  SRCmdLock         = '#';
  SRCmdExtExec      = '$';
  SRCmdLocalObj     = '@';

  chProcDiv         = '.';
  chLineExt         = '_';
  chQuoteDef        = '"';
  chQuotes          = ['"'];
  chWordDiv         = [#0..#32,'|',','];
  (* chCntrChars       = [#0..#31]; *)
  chAttrChars       = ['A'..'Z','a'..'z', SRCmdLock, SRCmdExtExec, SRCmdLocalObj];
  chEol             = [#13];
  chEcr             = [#10];
  chEolCr           = [#10,#13];
  chAttrBegin       = '[';
  chAttrEnd         = ']';
  chFuncBegin       = '(';
  chFuncEnd         = ')';

  tkComment1        = 'rem';
  tkComment2        = '==';
  tkComment3        = '/*';
  tkComment4        = '//';
  tkComment5        = '--';

  tkNot             = 'not';
  tkAnd             = 'and';
  tkOr              = 'or';

  tkProc1           = 'Proc';
  tkProc2           = 'Procedure';
  tkProc3           = 'Func';
  tkProc4           = 'Function';
  tkProc5           = '!';

  tkInitProc        = 'Init';
  tkDoneProc        = 'Done';
  tkMainProc        = 'Main';
  tkUndoProc        = 'Undo';

  tkBlockBegin1     = 'begin';
  tkBlockBegin2     = '{';

  tkBlockEnd1       = 'end';
  tkBlockEnd2       = '}';

  tkShowVariables   = '_ShowVars';

  tkVar             = 'Var';
  tkVars            = 'Vars';
  tkSet             = 'Set';
  tkReturn          = 'Exit';
  tkPause           = 'Delay';
  tkWinWait         = 'WaitWindow';
  tkCall            = 'Call';
  tkInfo            = 'MsgBox';
  tkQuery           = 'QueryBox';
  tkChkState        = 'CheckState';
  tkChkVar          = 'VarReset';
  tkSetState        = 'SetState';
  tkFileAppend      = 'FileAppend';
  tkBreak           = 'Break';

  tkForceDirs       = 'ForceDirs';
  tkChangeDir       = 'ChDir';
  tkMCopy           = 'MCopy';
  tkFCopy           = 'Copy';
  tkMMove           = 'MMove';
  tkFMove           = 'Move';
  tkUpdate          = 'Update';
  tkRename          = 'Rename';
  tkDelete          = 'Delete';

  tkExec            = 'Run';
  tkExecEx          = 'RunEx';
  tkExecAs          = 'RunAs';
  tkOpen            = 'Open';
  tkOpenEx          = 'OpenEx';
  tkLink            = 'Link';
  tkLinkEx          = 'LinkEx';

  tkNetMap          = 'NetMap';
  tkNetUnmap        = 'NetClose';

  tkChIni           = 'ChIni';
  tkChReg           = 'ChReg';

  tkVarExists       = 'VarExists';
  tkVarIs           = 'VarIs';
  tkKeyExists       = 'KeyExists';
  tkWinExists       = 'WinExists';
  tkTaskExists      = 'TaskExists';
  tkDiskFree        = 'DiskFree';
  tkDirExists       = 'DirExists';
  tkFileExists      = 'FileExists';
  tkMaskExists      = 'MaskExists';
  tkIsAdmin         = 'IsAdmin';

  tkTimeAfter       = 'TimeAfter';
  tkTimeBefore      = 'TimeBefore';

  tkStateIsOk       = 'ScriptStateIsOk';
  tkStateIsGood     = 'ScriptStateIsGood';
  tkStateIsWarning  = 'ScriptStateIsWarning';
  tkStateIsError    = 'ScriptStateIsError';

  tkIniSecExists    = 'IniSectionExists';
  tkIniKeyExists    = 'IniKeyExists';
  tkIniValCompare   = 'IniValueCompare';

  tkRegValueExists  = 'RegValueExists';
  tkRegKeyExists    = 'RegKeyExists';
  tkRegValCompare   = 'RegValueCompare';

  tkIniReadVariable = 'IniReadVariable';
  tkIniReadSections = 'IniReadSections';
  tkIniReadKeys     = 'IniReadKeys';

  tkRegReadVariable = 'RegReadVariable';
  tkRegReadSections = 'RegReadSections';
  tkRegReadKeys     = 'RegReadKeys';

  tkSelectFile      = 'SelectFile';
  tkSelectDir       = 'SelectDir';

  tkCalcCrc32       = 'CalcCrc32';

  tkIf              = 'If';
  tkThen            = 'Then';
  tkElse            = 'Else';
  tkWhile           = 'While';
  tkDo              = 'Do';
  tkRepeat          = 'Repeat';
  tkUntil           = 'Until';

  tkForEach         = 'ForEach';
  tkForFile         = 'ForFiles';
  tkForDirs         = 'ForDirs';

  tkBdeEnabled      = 'BdeEnabled';
  tkBdeCheckAlias   = 'BdeAliasExists';
  tkBdeAliasCreate  = 'BdeAliasCreate';
  tkBdeAliasDelete  = 'BdeAliasDelete';

  tkPrinterExists   = 'PrinterExists';
  tkAddPrinter      = 'PrinterCreate';
  tkDelPrinter      = 'PrinterDelete';
  tkAddNetPrinter   = 'PrinterConnect';
  tkDelNetPrinter   = 'PrinterDisconnect';

  tkFreeObject      = 'ObjectDelete';
  tkFindDirs        = 'FindDirs';
  tkFindFiles       = 'FindFiles';

  tkObjIsExists     = 'ObjectIsExists';
  tkObjIsList       = 'ObjectIsList';

  tkServiceStart    = 'ServiceStart';
  tkServiceStop     = 'ServiceStop';
  tkServiceExists   = 'ServiceExists';
  tkServiceStarted  = 'ServiceStarted';
  tkServiceStopped  = 'ServiceStopped';

  tkListIsNotEmpty  = 'ListNotEmpty';
  tkListIsBof       = 'ListIsFirst';
  tkListIsEof       = 'ListIsLast';
  tkListFirst       = 'ListFirst';
  tkListPrior       = 'ListPrior';
  tkListNext        = 'ListNext';
  tkListLast        = 'ListLast';

  tkTextCreate      = 'TextCreate';
  tkTextOpen        = 'TextOpen';
  tkTextSave        = 'TextSave';
  tkTextAddChars    = 'TextAdd';
  tkTextAddLine     = 'TextAddLine';
  tkTextFind        = 'TextFind';
  tkTextReplace     = 'TextReplace';
  tkTextSet         = 'TextSet';

  tkSubString       = 'SubString';
  tkSubStrings      = 'SubStrings';

  tkStrReplace      = 'StrReplace';

  tkExtractFileName = 'ExtractFileName';
  tkExtractPathName = 'ExtractFilePath';
  tkExtractFileExt  = 'ExtractFileExt';
  tkChangeFileExt   = 'ChangeFileExt';
  tkCheckFileName   = 'CheckFileName';

  tkExitWindows     = 'ExitWindows';

  tkScriptSave      = 'ScriptSaveFile';

  tkFtpOpen         = 'FtpOpen';
  tkFtpCurrDir      = 'FtpGetDir';
  tkFtpChDir        = 'FtpChDir';
  tkFtpMkDir        = 'FtpMkDir';
  tkFtpFrDir        = 'FtpFrDir';
  tkFtpRmDir        = 'FtpRmDir';
  tkFtpGet          = 'FtpGet';
  tkFtpPut          = 'FtpPut';
  tkFtpDelete       = 'FtpDelete';
  tkFtpClose        = 'FtpClose';

  varScriptId       = 'ScriptId';
  varAttributes     = 'ScriptAttr';
  varAlias          = 'ScriptAlias';
  varDescription    = 'ScriptNotes';
  varSourceDir      = 'SourceDir';
  varOnError        = 'OnError';

  varExitCode       = 'ExecReturnCode';
  varForFile        = 'Files';
  varForDirs        = 'Dirs';

  TRErrorsInversed  = [esErrorStop];
  TRFileFlagsInversed = [rfOverWrite, rfRepeatOnError, rfCopyFileDate,
                         rfCopyFileAttr, rfSkipSysFiles];
  TRIniFlagsInversed = [];
  TRRegFlagsInversed = [];

  SRCmdErrAttr  : array [TRCmdErrAttr] of char = (
    'C', // eaContinue,         ���������� ���������� �������
    'R', // eaRestore,          ���������� ��������� � �������� �����
    'U'  // eaRunUndo           ���������� ��������� � ��������� ��������� Undo
    );

  SRCmdRetFlag  : array [TRCmdRetFlag] of Char = (
    'N', // rfNoChange,         �� �������� ������ �������
    'O', // rfOk,               ������� gsOk
    'W', // rfWarning,          ������� gsWarning
    'E'  // rfError             ������� gsError
    );

  SRCmdScriptState : array [TRScriptState] of Char = (
    'O', // stOk                ���������� stOk
    'W', // stWarning           ���������� stWarning
    'E', // stError             ���������� stError
    'S', // stErrorStop         ���������� stErrorStop
    'R', // stErrorRestore      ���������� stErrorRestore
    'U'  // stErrorUndo         ���������� stErrorUndo
    );

  SRCmdErrFlag  : array [TRErrorsOption] of Char = (
    'B', // NOT esErrorStop,    �� !!! �������� ��������� ������ ��� ������ (����� - ��������� ������ � ��������� � ���������� �����)
    'I', // esErrorIgnore,      ��� ������� ��������� ������ ������� gsOk (����� gsError)
    'W', // esErrorWarning,     ��� ������� ��������� ������ ������� gsWarning (����� gsError)
    'E'  // esErrorShow,        �������� ������ ������������
    );

  SRMsgBoxType  : array [TRMsgBoxType] of Char = (
    'I', // mbInfo
    'W', // mbWarning
    'E'  // mbError
    );

  SRCmdEmptyOk      = 'O';
  SRCmdEmptyError   = 'Z';
  SRCmdForceDirs    = 'F';

  SRCmdRfoFlag  : array [TRfoFileFlag] of Char = (
    'F', // rfForceDirs,        ������� ���� ���������� ��� �������������
    'S', // rfSrcSubDirs,       ���������� ����� ��������� �����������
    'A', // rfDstDirAuto,       �������������� ���������� �������� ���������� (��� ������� �������� � ���� �������)
    'X', // rfDstFixed,         ������� (��� ����!!!) ���������� - ������������� (��� ����� �� ������ ������������ ����� ����������� � ���� � ��� ��)
         //                     ��� �������� UPDATE - ����� ������ ������ � ��������� �������� (��� ������������)
    'N', // NOT rfOverWrite,    �� !!! ������������ ������������ �����
    'Q', // NOT rfRepeatOnError �� !!! ��������� ��� ������
    'M', // rfCopyBackup,       ������� ��������� ����� ����������������� �����
    '@', // rfCopyLocked,       ��������������� ����� ����������� � ��������������� �� ��������� ����
    'V', // rfCheckCopy,        �������� �������� � ������������� ���� (CRC32)
    'G', // NOT rfCopyFileDate, �� !!! ���������� ���� ��������� �����
    'J', // NOT rfCopyFileAttr, �� !!! ���������� �������� �����
    'K', // rfSkipReadOnly,     ���������� (�� ������������) ����� "������ ������"
    'Y', // NOT rfSkipSysFiles, �� !!! ���������� (�� ������������) ��������� �����
    'H', // rfSkipHiddenFiles,  ���������� (�� ������������) ������� �����
    'D', // rfCompareDate,      ������������ �����, �������� �� ���� ���������
    'L', // rfCompareSize,      ������������ �����, �������� �� �������
    'T', // rfCompareCrc32,     ������������ �����, �������� �� ����������� (CRC32)
    'P'  // rfCopyList,         �������� ����� ������� ������������ ������ � ������
    );

  SRCmdSwwFlag : array [SW_HIDE..SW_MAX] of Char = (
    'H',  // SW_HIDE = 0;
    'N',  // SW_SHOWNORMAL = 1;
    'M',  // SW_SHOWMINIMIZED = 2;
    'L',  // SW_SHOWMAXIMIZED = 3;
    'A',  // SW_SHOWNOACTIVATE = 4;
    'S',  // SW_SHOW = 5;
    'Z',  // SW_MINIMIZE = 6;
    'X',  // SW_SHOWMINNOACTIVE = 7;
    'Y',  // SW_SHOWNA = 8;
    'V',  // SW_RESTORE = 9;
    'D'   // SW_SHOWDEFAULT = 10;
    );

  chWaitExit  = 'P';
  chCheckOk   = 'O';

  chWaitOpen  = 'O';
  chWaitClose = 'X';

  SRCmdNetMap : array [RESOURCETYPE_ANY..RESOURCETYPE_PRINT] of Char = (
    'A',   // RESOURCETYPE_ANY = 0;
    'D',   // RESOURCETYPE_DISK = 1;
    'P'    // RESOURCETYPE_PRINT = 2;
    );

  chSaveProfile = 'S';
  chForceUnmap  = 'F';

  SRCmdIniFlag  : array [TRfoIniFlag] of Char = (
    'S', // riSrcSubDirs        ���������� ����� ��������� �����������
    'N', // riCreateValue       ������� ��������, ���� ��� �� ���� � �����
    'D', // riDeleteValue       ������� ��������, ���� ��� ���� � �����
    'A', // riDeleteSection     ������� ������, ���� ��� ���� � �����
    'M', // riNotExistsOk       ���� �������� �� ���������� - ������� Ok (����� Skip)
    'X', // riNotExistsError    ���� �������� �� ���������� - ������� Error (����� Skip)
    'V', // riCompareValues     �������� ������, ���� ���������� �������� ���������� �� ������
    'K', // riSkipReadOnly      ���������� (�� ������������) ����� "������ ������"
    'Y', // riSkipSysFiles      ���������� (�� ������������) ��������� �����
    'H'  // riSkipHiddenFiles   ���������� (�� ������������) ������� �����
    );

  SRCmdRegFlag  : array [TRfoRegFlag] of Char = (
    'N', // rgCreateValue       ������� ��������, ���� ��� �� ���� � �����
    'D', // rgDeleteValue       ������� ��������, ���� ��� ���� � �����
    'M', // rgNotExistsOk       ���� �������� �� ���������� - ������� Ok (����� Skip)
    'X', // rgNotExistsError    ���� �������� �� ���������� - ������� Error (����� Skip)
    'V'  // rgCompareValues     �������� ������, ���� ���������� �������� ���������� �� ������
    );

  chSubDirs   = 'S';

  SRCmdExitWinFlag_LOGOFF = 'L';
  SRCmdExitWinFlag_SHUTDOWN = 'S';
  SRCmdExitWinFlag_REBOOT = 'T';
  SRCmdExitWinFlag_FORCE = 'F';
  SRCmdExitWinFlag_POWEROFF = 'O';
  SRCmdExitWinFlag_FORCEIFHUNG = 'H';

  SRCmdTextIgnoreCase = 'I';
  SRCmdTextReplaceAll = 'A';
  SRCmdTextOemCode    = 'O';
  SRCmdTextWriteLn    = 'L';

  SRCmdFtpPassive     = 'P';
  SRCmdFtpBinary      = 'B';
  SRCmdFtpErrIgnore   = 'I';
  SRCmdFtpErrWarning  = 'W';

resourcestring
  EScriptProcAlreadyExists    = '��������� � ������ "%s" ��� ������� � ������ ��������!';
  EScriptProcNotFound         = '��������� � ������ "%s" �� �������!';
  EScriptProcErrorCreate      = '������ �������� ��������� "%s"!';
  EScriptProcErrorExec        = '������ ���������� ��������� "%s": "%s"!';
  EScriptLoadExtScript        = '������ �������� �������� ������� "%s": "%s"!';
  EScriptNotFound             = '������ "%s" �� ������';
  EScriptInvalidObject        = '�������� ��� ������� "%s" ��� ������ �� ����������!';
  EScriptInvalidCurrProc      = '�� ������� ������� ���������!';
  EScriptObjListNotFound      = '�� ������ ������� ������ ��������!';
  EScriptObjectNotFound       = '������ "%s" �� ������!';
  EScriptListEmpty            = '��� �������� ��� ����������!';
  SMsgCannotEdit              = '���������� ����� �� ������������� ����������!';

  EParseUnterminatedString    = '����������� ����� ������ (������ ��� ������������ ������������): "%s"!';
  EParseUnterminatedBlock     = '����������� ����� ����� ������!';
  EParseEmptyToken            = '������ �����!';
  EParseInvalidToken          = '������������ �����: "%s"!';
  EParseInvalidVariable       = '������������ �������� ����������: "%s"!';
  EParseInvalidProcName       = '��� ��������� �� ����������!';
  EParseInvalidAttrChar       = '������������ ������ "%s" � �������� ��������� ��������!';
  EParseInvalidParamsCount    = '������������ ����� ���������� �������: %d. ��� ������ ������� ���������� ������� %d ��������(��)!';
  EParseInvalidParamsNumber   = '������������ ������ ���������: "%d"';
  EParseInvalidParam          = '������������ �������� �������: "%s"!';
  EParseWaitBegin             = '���� ������ ������ ���������� � ������ "begin"!';
  EParseNoActBlock            = '�� ��������� ������� ���� �������!';
  EParseNoEndBlock            = '������ � ����������� ����� �������!';
  EParseEmplyBlock            = '������ ���� �������: "%s"!';

  EExecuteCommand             = '������ ����������: "%s"!';
  EExecuteFunction            = '������ � ������� "%s": %s!';
  EExecuteOpenFile            = '������ ������ �����: "%s"!';
  EExecuteSaveFile            = '������ ������ �����: "%s"!';
  EExecuteRegKeyNotExists     = '���� "%s\%s" �� ������!';

  SExecuteRun                 = '������ ������� "%s"...';
  SExecuteShowMessage         = '���������: "%s"';
  SExecuteStartProcedure      = '������ ��������� "%s"...';
  SExecuteCreateScript        = '���������� ��������� "%s" �������� ���������...';
  SExecuteReturnProcedure     = '��������� "%s" %s';
  SExecuteReturnScript        = '������ "%s" %s';
  SExecuteRestoreFiles        = '����� ���������...';
  SExecuteLoadExtScript       = '�������� �������� ������� "%s"...';
  SExecuteAnalyzeFunctions    = '�������� �������...';
  SExecuteTrue                = '������� ��������� - ���������� ����� <then>...';
  SExecuteElse                = '������� �� ��������� - ���������� ����� <else>...';
  SExecuteSetTitle            = '��������� �������� ����������';
  SExecuteSet                 = '���������� "%s" ��������� �������� "%s"';
  SExecuteCycleCount          = '� ������ "%s" ������� %d �������.';
  SExecuteCycleSet            = '��������� %d: ���������� "%s" ��������� �������� "%s"';
  SExecuteChkVar              = '�������� ������� ���������� "%s"';
  SExecuteVarExists           = '��������� ���������� ��� ���������� � ������ ����������.';
  SExecuteEndIf               = '����� ����� <%s>.';
  SExecuteCycleBegin          = '������ ����� <%s>...';
  SExecuteCycleForBegin       = '���������� ����� ��� ���� ��������� ������ <%s>.';
  SExecuteCycleForFile        = '���������� ����� ��� ���� ������ "%s" � �������� "%s" (��� ����� ������������).';
  SExecuteCycleForFileS       = '���������� ����� ��� ���� ������ "%s" � �������� "%s" (������� ��������� �����������).';
  SExecuteCycleForDirs        = '���������� ����� ��� ���� ��������� "%s" (��� ����� ������������).';
  SExecuteCycleForDirsS       = '���������� ����� ��� ���� ��������� "%s" (������� ��������� �����������).';
  SExecuteCycleBreak          = '���������� ����� �������� ��-�� ���������� ����� ��������� (%d)!';
  SExecuteCycleEnd            = '����� ����� <%s>.';
  SExecuteBreakQry            = '�������� ���������� �������?';
  SExecuteBreakScript         = '���������� ������� ��������!';
  SExecuteBreakBlock          = '���������� ����� ��������.';
  SExecuteBreakError          = '�� ������� ���������� ����� �� �����!';
  SExecutePause               = '����� %d ������(�)...';
  SExecuteWinWaitOpen         = '�������� �������� ���� "%s"';
  SExecuteWinWaitOpenOk       = '���� "%s" �������.';
  SExecuteWinWaitClose        = '�������� �������� ���� "%s"';
  SExecuteWinWaitCloseOk      = '���� "%s" �� ����������.';
  SExecuteSetState            = '��� ������� ��������� ���������� ������: %s';
  SExecuteFreeObject          = '�������� ������� "%s" �� ������';
  SExecuteFindDirs            = '�������� ������ ��������� "%s"';
  SExecuteFindFiles           = '�������� ������ ������ "%s"\"%s"';
  SExecuteInvalidPosition     = '�������� ������� � ������: %d! ������� ���������� ����� � ������: %d.';
  SExecuteInvalidObject       = '�� ������ ������ ��� �������� ��� ������� "%s"!';
  EScriptExistsObject         = '������ � ������ "%s" ��� ���������� � ������ ��������!';
  SExecuteListFirst           = '������� ������ ������ �� ������ "%s"';
  SExecuteListPrior           = '������� ���������� ������ �� ������ "%s"';
  SExecuteListNext            = '������� ��������� ������ �� ������ "%s"';
  SExecuteListLast            = '������� ��������� ������ �� ������ "%s"';
  SExecuteTextCreate          = '�������� ������ � ������ "%s".';
  SExecuteTextLoadTitle       = '�������� ������ "%s" �� ����� "%s"';
  SExecuteTextLoadResult      = '������� %d �����(�) / %d ������(�,��).';
  SExecuteTextSaveTitle       = '������ ������ "%s" � ���� "%s"';
  SExecuteTextSaveResult      = '�������� %d �����(�) / %d ������(�,��).';
  SExecuteTextAddChars        = '���������� ������(�,��) "%s" � ������ "%s"';
  SExecuteTextAddLine         = '���������� �����(�) "%s" � ������ "%s"';
  SExecuteTextAdd             = '��������� %d ������(�,��) (%s).';
  SExecuteTextReplaceTitle    = '������ � ������ "%s" ������ "%s" �� ������ "%s"';
  SExecuteTextReplaceProgress = '�������� ������, ��������� ����������...'#13'����������� ����� - %d.';
  SExecuteTextReplaceResult   = '����������� %d �����.';
  SExecuteTextSet             = '�������� � ���������� "%s" �������� �� ����� ������ "%s"';
  SExecuteStrReplace          = '����� � ������ ��������� "%s" �� "%s" ��� ���������� "%s"';
  SExecuteIniReadVariable     = '������ �������� ���������� "%s" �� INI-����� "%s", ������ "%s", ���� "%s"';
  SExecuteIniSectionsTitle    = '������ ������������ ������ � ������ "%s" �� INI-����� "%s"';
  SExecuteIniKeysTitle        = '������ ������������ ���������� � ������ "%s" �� ������ "%s" INI-����� "%s"';
  SExecuteIniReadResult       = '������� %d ��������(�).';
  SExecuteRegReadVariable     = '������ �������� ���������� "%s" �� �������, ������ "%s", ������ "%s", ���� "%s"';
  SExecuteRegSectionsTitle    = '������ ������������ ������ � ������ "%s" �� ������� ������� "%s\%s"';
  SExecuteRegKeysTitle        = '������ ������������ ���������� � ������ "%s" �� ������� ������� "%s\%s"';
  SExecuteRegReadResult       = '������� %d ��������(�).';
  SExecuteShowVarsTitle       = '����� �������� ������ ���������� (���������� ����������)';
  SExecuteShowVarsResult      = '����� %d ����������.';
  SExecuteScriptFileAppend    = '������ � ���� "%s" ������ "%s"';
  SExecuteCalcCrc32           = '���������� ����������� ����� CRC32 ��� ����� "%s"';
  SExecuteFtpOpen             = '����������� � FTP-������� "%s"';
  SExecuteFtpOpenOk           = '����������� � FTP-������� ���������. ������� �������: "%s"';
  SExecuteFtpOpenError        = '������ ����������� � FTP-������� "%s"!';
  SExecuteFtpNotConnected     = '����������� � FTP-������� �� �����������!';
  SExecuteFtpOk               = '������� ������� ���������.';
  SExecuteFtpOkDir            = '������� ������� ���������. ������� �������: "%s"';
  SExecuteFtpError            = '������ ���������� ������� "%s"!';
  SExecuteFtpClose            = '���������� �� FTP-�������';
  SExecuteFtpClosed           = '����������� � FTP-������� ���������.';
  SExecuteFtpFree             = '������ "%s" ������ �� ������.';
  SExecuteFtpCurrDir          = '��������� �������� �������� �� FTP-�������';
  SExecuteFtpChDir            = '������� � ������� "%s" �� FTP-�������';
  SExecuteFtpMkDir            = '�������� �������� "%s" �� FTP-�������';
  SExecuteFtpFrDir            = '�������� �������� "%s" �� FTP-�������';
  SExecuteFtpRmDir            = '�������� �������� "%s" �� FTP-�������';
  SExecuteFtpGet              = '�������� ����� "%s" c FTP-�������';
  SExecuteFtpPut              = '�������� ����� "%s" �� FTP-������';
  SExecuteFtpDelete           = '�������� ����� "%s" c FTP-�������';

  SExecuteStateOk             = '��������� �������.';
  SExecuteStateWarning        = '��������� � ����������������!';
  SExecuteStateError          = '��������� � ��������!';
  SExecuteStateErrorStop      = '�������� ��-�� ������!';
  SExecuteStateErrorRestore   = '�������� ��-�� ������! ��������� ����� ���������.';
  SExecuteStateErrorUndo      = '�������� ��-�� ������! ��������� ������ ��������� UNDO.';

  SExecuteScriptSaveTitle     = '������ ������� "%s" � ���� "%s"';
  SExecuteScriptSaveError     = '������ "%s" �� ������!';

  SScriptStateOk              = '�������� �������.';
  SScriptStateWarning         = '�������� � ����������������!';
  SScriptStateError           = '�������� � ��������!';
  SScriptStateErrorStop       = '������� ��-�� ������!';
  SScriptStateErrorRestore    = '������� ��-�� ������! �������� ����� ���������.';
  SScriptStateErrorUndo       = '������� ��-�� ������! �������� ������ ��������� UNDO.';

  SCompileStart               = '���������� ������� "%s"...';
  SCompileError               = '������ [������ %d, ������ %d]: %s';
  SCompileEmpty               = '������ ���� - ��� ������ ��� ����������!';
  SCompileResult              = '���������� %d �����(�), ��������� %d ������(�).';

  SViewerWait                 = '��������� ����������...';
  SViewerStatComplete         = '��������� %.1f%% [%d �� %d] �� %s';
  SViewerStatElapsed          = ', �������� %s';
  SViewerEmpty                = '';

  SCmdNoteShowVariables       = ' - (DEBUG) ������� ������� ������ ����������';
  SCmdNoteSet                 = '"name"="value" - ���������� �������� ����������';
  SCmdNoteChkVar              = '"name" "value" - ���� ���������� �� ����������, ������� � ��������� ���������';
  SCmdNoteInfo                = '"������ 1" "������ 2" ... "������ N" - ����� ��������� ������������';
  SCmdNoteFDirs               = '"����" - ������� �������';
  SCmdNoteChangeDir           = '"���" - ������� �������';
  SCmdNoteMCopy               = '"������" "����" "�����" - ���������� ����� ��� �������� �� �����';
  SCmdNoteFCopy               = '"������" "����" - ���������� ����� ��� ��������';
  SCmdNoteMMove               = '"������" "����" "�����" - ����������� ����� ��� �������� �� �����';
  SCmdNoteFMove               = '"������" "����" - ����������� ����� ��� ��������';
  SCmdNoteUpdate              = '"������" "����" "�����" - �������� ������������ �����';
  SCmdNoteRename              = '"�������� ���" "����� ���" - ������������� ���� ��� �������';
  SCmdNoteDelete              = '"����" - ������� ����� ��� ��������';
  SCmdNoteBreak               = ' - �������� ������ �������� ����� (�����)';
  SCmdNoteReturn              = ' - ��������� ������ �������';
  SCmdNoteChkState            = ' - ��������� ���������� ������ �������';
  SCmdNoteSetState            = ' - ���������� ���������� ������ �������';
  SCmdNotePause               = '"����� � ��������" "[���������]" - ������������� ������ �������';
  SCmdNoteWinWait             = '"��� ����" "[���������]" - �������� �������� �(���) �������� ���� � ��������� ������';
  SCmdNoteCall                = '"���", "������ ����������" - ����� ���������';
  SCmdNoteExec                = '"�������" "������� �������" - ��������� ������� ���������';
  SCmdNoteExecEx              = '"�������" "������� �������", "���� [OK]", "���� [WARNING]" - ��������� ������� ��������� � ��������� ����� ��������';
  SCmdNoteExecAs              = '"�������" "�������", "�����", "���", "������" - ��������� ��������� �� ����� ������������';
  SCmdNoteOpen                = '"����" "���������" - ������� ������� ����';
  SCmdNoteOpenEx              = '"����" "���������", "���� [OK]", "���� [WARNING]" - ������� ������� ���� � ��������� ����� ��������';
  SCmdNoteLink                = '"�������" "������� �������" "���� ������" - ������� ����� � �����';
  SCmdNoteLinkEx              = '"�������" "������� �������" "��������" "���� ������" - ������� ����� � ����� � �����������';
  SCmdNoteFileAppend          = '"��� �����", "������" - �������� ������ � �����';
  SCmdNoteChIni               = '"����" "������" "��������" "��������" - �������� �������� ��������� � INI-�����';
  SCmdNoteChReg               = '"������", "������" "��������" "���" "��������" - �������� �������� ��������� � �������';
  SCmdIniReadVariable         = '"����������" "����" "������" "��������" "��������" - ������� �������� �� INI-����� � ����������';
  SCmdIniReadSections         = '"��� �������" "����" - ������� ����� ������ �� INI-����� � ������';
  SCmdIniReadKeys             = '"��� �������" "����" "������" - ������� ����� ���������� �� ������ INI-����� � ������';
  SCmdRegReadVariable         = '"����������" "������" "������" "��������" "��������" - ������� �������� �� ������� � ����������';
  SCmdRegReadSections         = '"��� �������" "������" "������" - ������� ����� ��������� ������ �� ������ ������� � ������';
  SCmdRegReadKeys             = '"��� �������" "������" "������" - ������� ����� ���������� �� ������ ������� � ������';
  SCmdNoteNetMap              = '"����" "������� ����" - ���������� ������� ����';
  SCmdNoteNetUnmap            = '"����" - ��������� ������� ����';
  SCmdNoteBdeAliasCreate      = '"���" "�������" "��������� (���: �������� ; ...)" - ������� BDE alias';
  SCmdNoteBdeAliasDelete      = '"���" - ������� BDE alias';
  SCmdNoteAddPrinter          = '"���", "�������", "����", "winprint", "�����������", "������������", "����" - �������� ��������� �������';
  SCmdNoteDelPrinter          = '"���" - ������� ��������� �������';
  SCmdNoteAddNetPrinter       = '"������� ���" - ���������� ������� ������� ��� �������� ������������';
  SCmdNoteDelNetPrinter       = '"������� ���" - ��������� ������� ������� ��� �������� ������������';
  SCmdNoteFreeObject          = '"��� �������" - ������� ������ �� ������';
  SCmdNoteFindDirs            = '"��� �������", "����" - ������� ������ ���������';
  SCmdNoteFindFiles           = '"��� �������", "����", "�����" - ������� ������ ������ � ��������';
  SCmdNoteListFirst           = '"��� �������" - ������� �� ������ ������ ������ (���������� "��� �������"="������")';
  SCmdNoteListPrior           = '"��� �������" - ������� �� ���������� ������ ������ (���������� "��� �������"="������")';
  SCmdNoteListNext            = '"��� �������" - ������� �� ��������� ������ ������ (���������� "��� �������"="������")';
  SCmdNoteListLast            = '"��� �������" - ������� �� ��������� ������ ������ (���������� "��� �������"="������")';
  SCmdNoteExtractFileName     = '"��� �����", "����������" - �������� ��� ����� � ��������� �������� � ����������';
  SCmdNoteExtractFilePath     = '"��� �����", "����������" - �������� ���� � ����� � ��������� �������� � ����������';
  SCmdNoteExtractFileExt      = '"��� �����", "����������" - �������� ���������� ����� � ��������� �������� � ����������';
  SCmdNoteChangeFileExt       = '"��� �����", "����������", "����������" - �������� ���������� ����� � ��������� �������� � ����������';
  SCmdNoteCheckFileName       = '"��� ����� (��������)", "����������" - ��������� ��� ����� � ������������ � ����������� �����������';
  SCmdNoteExitWindows         = ' - ��������� ����� ������������ ��� ������ Windows';
  SCmdNoteServiceStart        = '"��� �������" - ��������� ������ � ��������� ��������� ������';
  SCmdNoteServiceStop         = '"��� �������" - ���������� ������ � ��������� ��������� ������';
  SCmdNoteTextCreate          = '"��� �������" - ������� ����� (������ �����)';
  SCmdNoteTextOpen            = '"��� �������", "��� �����" - ��������� ����� �� �����';
  SCmdNoteTextSave            = '"��� �������", "��� �����" - ��������� ����� � �����';
  SCmdNoteTextAddChars        = '"��� �������", "�������" - �������� ������� � ������';
  SCmdNoteTextAddLine         = '"��� �������", "������" - �������� ������ � ������';
  SCmdNoteTextReplace         = '"��� �������", "��� ������", "��� ��������" - ����� � ������ ��������� � ������';
  SCmdNoteTextSet             = '"��� �������", "��� ����������" - �������� � ��������� ���������� �����';
  SCmdCalcCrc32               = '"��� ����������", "��� �����" - ��������� ����������� ����� ����� (CRC32) � ��������� ����������';

  SCmdSubString               = '"����������", "������", "������", "�����������" - �������� ��������� �� ������';
  SCmdSubStrings              = '"����������", "������", "�����������" - ���������� ���������� �������� � ������';
  SCmdStrReplace              = '"����������", "��� ������", "��� ��������" - ����� � ������ ��������� � �������� ����������';

  SCmdNoteFtpOpen             = '"��� �������", "�����", "��� ������������", "������" - ����������� � FTP-�������';
  SCmdNoteFtpCurrDir          = '"��� �������", "��� ����������" - �������� ������� ������� �� FTP-�������';
  SCmdNoteFtpChDir            = '"��� �������", "��� ��������" - ������� ������� ������� �� FTP-�������';
  SCmdNoteFtpMkDir            = '"��� �������", "��� ��������" - ������� ������� �� FTP-�������';
  SCmdNoteFtpFrDir            = '"��� �������", "��� ��������" - ��������� � ������� ������� �� FTP-�������';
  SCmdNoteFtpRmDir            = '"��� �������", "��� ��������" - ������� ������� �� FTP-�������';
  SCmdNoteFtpGet              = '"��� �������", "��������� ��� �����", "��������� ��� �����" - ��������� ���� � FTP-�������';
  SCmdNoteFtpPut              = '"��� �������", "��������� ��� �����", "��������� ��� �����" - ��������� ���� �� FTP-������';
  SCmdNoteFtpDelete           = '"��� �������", "��������� ��� �����" - ������� ���� � FTP-�������';
  SCmdNoteFtpClose            = '"��� �������" - ������� ���������� � FTP-��������';

  SCmdNoteScriptSave          = '"�����", "��� �����" - ��������� ������ � ��������� ��� � �����';

  SCmdNoteIf                  = ': ���� "�������" �� {�������} ����� {�������} - (��������� � ����������� �� �������)';
  SCmdNoteWhile               = ': ���� "�������" ��������� {�������} - (���������, ���� ����������� �������)';
  SCmdNoteRepeat              = ': ��������� {�������} ���� �� "�������" ��������� - (���������, ���� �� ����� ��������� �������)';

  SCmdNoteForEach             = ': ��� ������� �������� "������" ��������� {�������}';
  SCmdNoteForFile             = ': ��� ������� ����� (�� "�����") ��������� {�������}';
  SCmdNoteForDirs             = ': ��� ������� �������� (�� "�����") ��������� {�������}';

  SFncNoteStateIsOk           = '() - �������� ������� ������ ������� � "OK"';
  SFncNoteStateIsGood         = '() - �������� ������� ������ ������� � "OK" ��� "WARNING"';
  SFncNoteStateIsWarning      = '() - �������� ������� ������ ������� � "WARNING"';
  SFncNoteStateIsError        = '() - �������� ������� ������ ������� � "ERROR"';
  SFncNoteVarExists           = '("name") - ��������� ������� ����������';
  SFncNoteVarIs               = '("name"="value") - ��������� �������� ����������';
  SFncNoteKeyExists           = '("key") - ��������� ������� ����� � ������ ������';
  SFncNoteWinExists           = '("��������� ����") - ��������� ������� ���� Windows';
  SFncNoteTaskExists          = '("��� �����") - ��������� ������� �������������� �������� Windows';
  SFncNoteQuery               = '("������") - ����� ������� � ������������ (�� / ���)';
  SFncNoteFileExists          = '("��� �����") - ��������� ������� �����';
  SFncNoteDirExists           = '("�������") - ��������� ������� ��������';
  SFncNoteMaskExists          = '("�����") - ��������� ������� ������ �� �����';
  SFncNoteDiskFree            = '("������ � ��") - ��������� ������� ���������� ����� �� ������� ����� � ��';
  SFncNoteTimeAfter           = '("��/��/���� ��:��:��") - ������ TRUE ����� ���������� �������';
  SFncNoteTimeBefore          = '("��/��/���� ��:��:��") - ������ TRUE ����� ���������� �������';
  SFncNoteIsAdmin             = '() - ��������� ������� ����������������� ���� � �������';
  SFncNoteBdeEnabled          = '() - ��������� ����������� BDE � �������';
  SFncNoteBdeCheckAlias       = '("���") - ��������� ������� ���������� BDE';
  SFncNotePrinterExists       = '("���") - ��������� ������� �������� � �������';
  SFncNoteObjIsExists         = '("��� �������") - ��������� ������������� �������';
  SFncNoteObjIsList           = '("��� �������") - ���������, �������� �� ������ �������';
  SFncNoteListIsNotEmpty      = '("��� �������") - ���������, ���� � ������ ������';
  SFncNoteListIsBof           = '("��� �������") - ���������, ���������� �� ��������� � ������ ������';
  SFncNoteListIsEof           = '("��� �������") - ���������, ���������� �� ��������� � ����� ������';
  SFncIniSecExists            = '("��� INI-�����;������") - ���������, ���������� �� ������ � INI-�����';
  SFncIniKeyExists            = '("��� INI-�����;������;����") - ���������, ���������� �� ���� (��������) � INI-�����';
  SFncIniValCompare           = '("��� INI-�����;������;����;��������") - ���������, ������������� �� �������� ����� � INI-����� ����������';
  SFncRegValueExists          = '("������;������") - ���������, ���������� �� �������� � �������';
  SFncRegKeyExists            = '("������;��������") - ���������, ���������� �� ���� � �������';
  SFncRegValCompare           = '("������;������;��������;��������") - ���������, ������������� �� �������� ����� � ������� ����������';
  SFncSelectFile              = '("���_����������<;��������>") - ������� ��� ����� � ��������� �������� � ����������';
  SFncSelectDir               = '("���_����������<;��������>") - ������� ��� �������� � ��������� �������� � ����������';
  SFncServiceExists           = '("���_�������") - ��������� ������� ������� � ��������� ��������� ������';
  SFncServiceStarted          = '("���_�������") - ���������, ������� �� ������ � ��������� ��������� ������';
  SFncServiceStopped          = '("���_�������") - ���������, ���������� �� ������ � ��������� ��������� ������';
  SFncNoteTextFind            = '("��� �������;������") - ���������, ���������� �� ��������� ������ � �����';

  SRCmdErrLockNote            = '��������� ������ �������� �������������';
  SRCmdExtExecNote            = '��������� ������� � ������� ������� ���������';
  SRCmdErrAttrNoteContinue    = '��� ������ ������� ���������� ���������� �������';
  SRCmdErrAttrNoteRestore     = '��� ������ ������� ���������� ������ � �������� ���������';
  SRCmdErrAttrNoteRunUndo     = '��� ������ ������� ���������� ������ � ��������� ��������� Undo';

  SRCmdRfoNoteLocalObj        = '������������ ��������� ������ ��������';

  SRCmdErrFlagNoteErrorStop   = '��� ������� ��������� ������ ���������� ��������� ������ ������';
  SRCmdErrFlagNoteErrorStop1  = '��� ������� ��������� ������� �� ����� �������� ������������';
  SRCmdErrFlagNoteErrorIgnore = '��� ������� ��������� ������ ������� [Ok] (����� [Error])';
  SRCmdErrFlagNoteErrorWarn   = '��� ������� ��������� ������ ������� [Warning] (����� [Error])';
  SRCmdErrFlagNoteErrorShow   = '�������� ������ ������������';

  SRCmdRfoNoteEmptyOk         = '���� ����� �� ������� - ������� [Ok] (����� [Warning])';
  SRCmdRfoNoteEmptyError      = '���� ����� �� ������� - ������� [Error] (����� [Warning])';

  SRMsgBoxTypeNoteInfo        = '��� ���� ���������: ���������� (Info)';
  SRMsgBoxTypeNoteWarning     = '��� ���� ���������: �������������� (Warning)';
  SRMsgBoxTypeNoteError       = '��� ���� ���������: ������ (Error)';

  SRCmdRfoNoteEmpty           = '--- � ���� ������� ������ ���� ������������ ---';
  SRCmdRfoNoteForceDirs       = '������� ���� ���������� ��� �������������';
  SRCmdRfoNoteSrcSubDirs      = '���������� ��������� �����������';
  SRCmdRfoNoteDstDirAuto      = '�������������� ���������� �������� ����������';
  SRCmdRfoNoteDstDirAutoDelete = '������� ������ ����� (���� X ����� �����������)';
  SRCmdRfoNoteDstFixed        = '������������� ������� ��� ���� ���������� (�� ������ ������������ � ����)';
  SRCmdRfoNoteDstFixedDelete  = '������� ������ ������ ��������, �� ���������� ������ � ���������';
  SRCmdRfoNoteDstFixedUpdate  = '����� ������ ������ � ��������� ��������-��������� (��� ������������)';
  SRCmdRfoNoteOverWrite       = '�� �������������� ������������ �����';
  SRCmdRfoNoteRepeatOnError   = '�� ������ ��������� ������� ��� ������� ���������';
  SRCmdRfoNoteCopyBackup      = '������� ��������� ����� ����������������� ����� �� ����� �����������';
  SRCmdRfoNoteCopyLocked      = '���� ���� ��� ���������� � ���������� - ������������� � ���� *.~*';
  SRCmdRfoNoteCheckCopy       = '�������� �������� � ������������� ���� (CRC32)';
  SRCmdRfoNoteCopyFileDate    = '�� ���������� ���� ��������� �����';
  SRCmdRfoNoteCopyFileAttr    = '�� ���������� �������� �����';
  SRCmdRfoNoteSkipReadOnly    = '���������� (�� ������������) ����� "������ ������"';
  SRCmdRfoNoteSkipSysFiles    = '������������ ��������� �����';
  SRCmdRfoNoteSkipSysFiles2   = '���������� (�� ������������) ����� ��������� �����';
  SRCmdRfoNoteSkipHiddenFiles = '���������� (�� ������������) ������� �����';
  SRCmdRfoNoteCompareDate     = '������������ �����, �������� �� ���� ���������';
  SRCmdRfoNoteCompareSize     = '������������ �����, �������� �� �������';
  SRCmdRfoNoteCompareCrc32    = '������������ �����, �������� �� ����������� (CRC32)';
  SRCmdRfoNoteCopyList        = '�������� ����� ������� ������������ ������ � ������ ������';
  SRCmdSwwFlagNote0           = '������ ���� (SW_HIDE)';
  SRCmdSwwFlagNote1           = '���������� ����� (SW_SHOWNORMAL)';
  SRCmdSwwFlagNote2           = '�������������� ���� (SW_SHOWMINIMIZED)';
  SRCmdSwwFlagNote3           = '���������� ���� �� ���� ����� (SW_SHOWMAXIMIZED)';
  SRCmdSwwFlagNote4           = '�������� �� �������� ���� (SW_SHOWNOACTIVATE)';
  SRCmdSwwFlagNote5           = '�������� ���� (SW_SHOW)';
  SRCmdSwwFlagNote6           = '�������� ���� (SW_MINIMIZE)';
  SRCmdSwwFlagNote7           = '�������� ���� � �� ������������ (SW_SHOWMINNOACTIVE)';
  SRCmdSwwFlagNote8           = '�������� ���� � �� ������������ (SW_SHOWNA)';
  SRCmdSwwFlagNote9           = '������������ ���� (SW_RESTORE)';
  SRCmdSwwFlagNote10          = '�������� ���� � ����������� �� ��������� (SW_SHOWDEFAULT)';
  SRCmdSwwFlagWaitExit        = '������� ���������� �������';
  SRCmdSwwFlagWaitOpen        = '������� �������� ����';
  SRCmdSwwFlagWaitClose       = '������� �������� ����';
  SRCmdSwwFlagCheckOk         = '�������������� ��� �������� (0 + ���� [OK], ���� [WARNING])';
  SRCmdNetMapNoteAny          = '��� �� ���������';
  SRCmdNetMapNoteDisk         = '������� ����';
  SRCmdNetMapNotePrint        = '�������';
  SRCmdFlagNoteCreateValue    = '������� ��������, ���� ��� �� ����������';
  SRCmdFlagNoteDeleteValue    = '������� ��������, ���� ��� ����������';
  SRCmdFlagNoteDeleteSection  = '������� ������, ���� ��� ����������';
  SRCmdFlagNoteNotExistsOk    = '���� �������� �� ���������� - ������� [Ok] (����� [Ignored])';
  SRCmdFlagNoteNotExistsError = '���� �������� �� ���������� - ������� [Error] (����� [Ignored])';
  SRCmdFlagNoteCompareValues  = '�������� ������, ���� ���������� �������� ���������� �� ������';
  SRCmdRetFlagNoteNoChange    = '�� �������� ������ �������';
  SRCmdRetFlagNoteOk          = '���������� ������ [Ok]';
  SRCmdRetFlagNoteWarning     = '���������� ������ [Warning]';
  SRCmdRetFlagNoteError       = '���������� ������ [Error]';
  SRCmdRetFlagNoteErrorStop   = '���������� ������ [Error] � ���������� ������';
  SRCmdRetFlagNoteErrorRestore = '���������� ������ [Error] � �������� ���������';
  SRCmdRetFlagNoteErrorUndo   = '���������� ������ [Error] � ��������� ��������� Undo';

  SRCmdNetMapNoteSaveProfile  = '�������� � ������� �������� ������������';
  SRCmdNetMapNoteForceUnmap   = '������������� ������� ��� �������� �����';

  SRNoteExitWinFlag_LOGOFF    = '��������� ����� ������������';
  SRNoteExitWinFlag_SHUTDOWN  = '��������� ������ Windows';
  SRNoteExitWinFlag_REBOOT    = '������������ Windows';
  SRNoteExitWinFlag_POWEROFF  = '��������� ������� ����������';
  SRNoteExitWinFlag_FORCE     = '������������� ������� ���������';
  SRNoteExitWinFlag_FORCEIFHUNG = '������������� ������� "��������" ���������';

  SRNoteTextIgnoreCase        = '�� ��������� ������� ��������';
  SRNoteTextReplaceAll        = '�������� ��� ���������';
  SRNoteTextOemCode           = '����� � DOS (CP-866) ���������';

  SRNoteFtpPassive            = '��������� �����';
  SRNoteFtpBinary             = '�������� (binary) �����';
  SRNoteFtpErrIgnore          = '��� ������� ��������� ������� [Ok] (����� [Error])';
  SRNoteFtpErrWarning         = '��� ������� ��������� ������� [Warning] (����� [Error])';

  SRNoteTextWriteLn           = '�������� ������� ���������� ������ ($0D$0A)';

  SsmHideAll                  = '������� ����������';
  SsmLogDisable               = '������ ���������';
  SsmLogHidden                = '��������� + �������� �������';
  SsmLogShowing               = '��������� + �������� �������';
  SsmLogAlways                = '��������� + �������� ������';

  SrsOk                       = '[OK]';
  SrsWarning                  = '[OK][WRN]';
  SrsAny                      = '[OK][WRN][ERR]';
  SrsNone                     = '[NONE]';

  LrsOk                       = '��� ������ � ��������������';
  LrsWarning                  = '��� ������';
  LrsAny                      = '�� �������������� ������';
  LrsNone                     = '�� ��������� (�����������)';

  SrsApply                    = '���������';
  SrsIgnore                   = '������������';

  LrsApply                    = '��������� ��� ��������';
  LrsIgnore                   = '������������ ��� ��������';

const
  SRProcedureState : array [TRScriptState] of string =
    (SExecuteStateOk, SExecuteStateWarning,
     SExecuteStateError, SExecuteStateErrorStop,
     SExecuteStateErrorRestore, SExecuteStateErrorUndo);

  SRScriptState : array [TRScriptState] of string =
    (SScriptStateOk, SScriptStateWarning,
     SScriptStateError, SScriptStateErrorStop,
     SScriptStateErrorRestore, SScriptStateErrorUndo);

  SRCmdErrAttrNote : array [TRCmdErrAttr] of string =
   (SRCmdErrAttrNoteContinue, SRCmdErrAttrNoteRestore, SRCmdErrAttrNoteRunUndo);

  SRMsgBoxTypeNote: array [TRMsgBoxType] of string =
    (SRMsgBoxTypeNoteInfo, SRMsgBoxTypeNoteWarning, SRMsgBoxTypeNoteError);

  SRCmdRetFlagNote: array [TRCmdRetFlag] of string =
   (SRCmdRetFlagNoteNoChange, SRCmdRetFlagNoteOk,
    SRCmdRetFlagNoteWarning, SRCmdRetFlagNoteError);

  SRCmdScriptStateNote: array [TRScriptState] of string =
   (SRCmdRetFlagNoteOk, SRCmdRetFlagNoteWarning,
    SRCmdRetFlagNoteError, SRCmdRetFlagNoteErrorStop,
    SRCmdRetFlagNoteErrorRestore, SRCmdRetFlagNoteErrorUndo);

  SRCmdErrFlagNote : array [TRErrorsOption] of string =
   (SRCmdErrFlagNoteErrorStop, SRCmdErrFlagNoteErrorIgnore,
    SRCmdErrFlagNoteErrorWarn, SRCmdErrFlagNoteErrorShow);

  SRCmdRfoFlagNote : array [TRfoFileFlag] of string = (
    SRCmdRfoNoteForceDirs, SRCmdRfoNoteSrcSubDirs, SRCmdRfoNoteDstDirAuto,
    SRCmdRfoNoteDstFixed, SRCmdRfoNoteOverWrite, SRCmdRfoNoteRepeatOnError,
    SRCmdRfoNoteCopyBackup, SRCmdRfoNoteCopyLocked, SRCmdRfoNoteCheckCopy, SRCmdRfoNoteCopyFileDate,
    SRCmdRfoNoteCopyFileAttr, SRCmdRfoNoteSkipReadOnly, SRCmdRfoNoteSkipSysFiles,
    SRCmdRfoNoteSkipHiddenFiles, SRCmdRfoNoteCompareDate, SRCmdRfoNoteCompareSize,
    SRCmdRfoNoteCompareCrc32, SRCmdRfoNoteCopyList);

  SRCmdSwwFlagNote : array [SW_HIDE..SW_MAX] of string =
    (SRCmdSwwFlagNote0, SRCmdSwwFlagNote1, SRCmdSwwFlagNote2,
     SRCmdSwwFlagNote3, SRCmdSwwFlagNote4, SRCmdSwwFlagNote5,
     SRCmdSwwFlagNote6, SRCmdSwwFlagNote7, SRCmdSwwFlagNote8,
     SRCmdSwwFlagNote9, SRCmdSwwFlagNote10);

  SRCmdNetMapNote : array [RESOURCETYPE_ANY..RESOURCETYPE_PRINT] of string =
    (SRCmdNetMapNoteAny, SRCmdNetMapNoteDisk, SRCmdNetMapNotePrint);

  SRCmdIniFlagNote  : array [TRfoIniFlag] of string =
    (SRCmdRfoNoteSrcSubDirs, SRCmdFlagNoteCreateValue, SRCmdFlagNoteDeleteValue, SRCmdFlagNoteDeleteSection,
     SRCmdFlagNoteNotExistsOk, SRCmdFlagNoteNotExistsError, SRCmdFlagNoteCompareValues,
     SRCmdRfoNoteSkipReadOnly, SRCmdRfoNoteSkipSysFiles2, SRCmdRfoNoteSkipHiddenFiles);

  SRCmdRegFlagNote  : array [TRfoRegFlag] of string =
    (SRCmdFlagNoteCreateValue, SRCmdFlagNoteDeleteValue, SRCmdFlagNoteNotExistsOk,
     SRCmdFlagNoteNotExistsError, SRCmdFlagNoteCompareValues);

  SScriptShowMode: array [TScriptShowMode] of string =
    (SsmHideAll, SsmLogDisable, SsmLogHidden, SsmLogShowing, SsmLogAlways);

  CScriptShowMode: array [TScriptShowMode] of string =
    ('Hidden', 'Viewer', 'Log Off', 'Log On', 'Log Fixed');

  SRScrRunState: array [TRScrRunState] of string =
    (SrsOk, SrsWarning, SrsAny, SrsNone);

  LRScrRunState: array [TRScrRunState] of string =
    (LrsOk, LrsWarning, LrsAny, LrsNone);

  SRScrResState: array [TRScrResState] of string  =
    (SrsApply, SrsIgnore);

  LRScrResState: array [TRScrResState] of string  =
    (LrsApply, LrsIgnore);

function ScriptQuotedStr(const SrcStr: string): string;

implementation

uses
  SysUtils, RDialogs;

function ScriptQuotedStr(const SrcStr: string): string;
begin
  Result := SrcStr;
  if (Length(SrcStr) = 0) then
    Result := AnsiQuotedStr(SrcStr, chQuoteDef)
  else begin
    if not ((SrcStr[1] = chQuoteDef) and (SrcStr[Length(SrcStr)] = chQuoteDef)) then
    begin
      if (Pos(#32, SrcStr) > 0)
      or (Pos(';', SrcStr) > 0)
      or (Pos(':', SrcStr) > 0)
      or (Pos(',', SrcStr) > 0)
      or (Pos('.', SrcStr) > 0)
      or (Pos('-', SrcStr) > 0)
      or (Pos('+', SrcStr) > 0)
      or (Pos('*', SrcStr) > 0)
      or (Pos('!', SrcStr) > 0)
      or (Pos('?', SrcStr) > 0)
      or (Pos('@', SrcStr) > 0)
      or (Pos('#', SrcStr) > 0)
      or (Pos('$', SrcStr) > 0)
      or (Pos('&', SrcStr) > 0)
      or (Pos('^', SrcStr) > 0)
      or (Pos('~', SrcStr) > 0)
      or (Pos('<', SrcStr) > 0)
      or (Pos('>', SrcStr) > 0)
      or (Pos('''', SrcStr) > 0)
      or (Pos(chQuoteDef, SrcStr) > 0)
      then Result := AnsiQuotedStr(SrcStr, chQuoteDef);
    end;
  end;
end;

{ TRListPosition }

constructor TRListPosition.Create;
begin
  inherited;
  fListPosition := 0;
end;

function TRListPosition.Empty: Boolean;
begin
  Result := (Count = 0);
end;

function TRListPosition.Bof: Boolean;
begin
  Result := (Count = 0) or (fListPosition <= 0);
end;

function TRListPosition.Eof: Boolean;
begin
  Result := (Count = 0) or (fListPosition >= Count);
end;

function TRListPosition.GetPosition: Integer;
begin
  if (fListPosition < 0) and (Count > 0) then
    Result := 0
  else begin
    if fListPosition > Count - 1
    then Result := Count - 1
    else Result := fListPosition;
  end;
   if ((Result < 0) and (Count > 0)) or (Result > Count - 1) then
    raise Exception.CreateFmt(SExecuteInvalidPosition, [Result, Count]);
end;

procedure TRListPosition.SetPosition(const Value: Integer);
begin
  if (Value >= -1) and (Value <= Count) then
    fListPosition := Value
  else raise Exception.CreateFmt(SExecuteInvalidPosition, [Value, Count]);
end;

procedure TRListPosition.First;
begin
  SetPosition(0);
end;

procedure TRListPosition.Last;
begin
  SetPosition(Count - 1);
end;

procedure TRListPosition.Prior;
begin
  SetPosition(fListPosition - 1);
end;

procedure TRListPosition.Next;
begin
  SetPosition(fListPosition + 1);
end;

{ TRStringListPos }

constructor TRStringListPos.Create;
begin
  inherited;
  fStrings := TStringList.Create;
end;

destructor TRStringListPos.Destroy;
begin
  fStrings.Free;
  inherited;
end;

function TRStringListPos.Count: Integer;
begin
  Result := fStrings.Count;
end;

function TRStringListPos.Item: string;
begin
  Result := fStrings[Position];
end;

function TRStringListPos.PositionValue: string;
begin
  Result := Item;
end;

function TRStringListPos.Value(const Index: Integer): string;
begin
  Result := fStrings[Index];
end;

function TRStringListPos.Text: string;
begin
  Result := fStrings.Text;
end;

{ TRFileListPos }

constructor TRFileListPos.Create;
begin
  inherited;
  fFileList := TRFileList.Create;
end;

destructor TRFileListPos.Destroy;
begin
  fFileList.Free;
  inherited;
end;

function TRFileListPos.Count: Integer;
begin
  Result := fFileList.Count;
end;

function TRFileListPos.Item: TRFileItem;
begin
  Result := fFileList.Items(Position);
end;

function TRFileListPos.PositionValue: string;
begin
  Result := Item.sName;
end;

function TRFileListPos.Value(const Index: Integer): string;
begin
  Result := fFileList.Items(Index).sName;
end;

function TRFileListPos.Text: string;
var
  Buf: TStringList;
  i: Integer;
begin
  Buf := TStringList.Create;
  try
    for i := 0 to fFileList.Count - 1 do
      Buf.Add(fFileList.Items(i).sName);
    Result := Buf.Text;
  finally
    Buf.Free;
  end;
end;

end.

