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
    'C', // eaContinue,         Продолжить выполнение скрипта
    'R', // eaRestore,          Остановить обработку и откатить файлы
    'U'  // eaRunUndo           Остановить обработку и запустить процедуру Undo
    );

  SRCmdRetFlag  : array [TRCmdRetFlag] of Char = (
    'N', // rfNoChange,         Не изменять статус скрипта
    'O', // rfOk,               Вернуть gsOk
    'W', // rfWarning,          Вернуть gsWarning
    'E'  // rfError             Вернуть gsError
    );

  SRCmdScriptState : array [TRScriptState] of Char = (
    'O', // stOk                Установить stOk
    'W', // stWarning           Установить stWarning
    'E', // stError             Установить stError
    'S', // stErrorStop         Установить stErrorStop
    'R', // stErrorRestore      Установить stErrorRestore
    'U'  // stErrorUndo         Установить stErrorUndo
    );

  SRCmdErrFlag  : array [TRErrorsOption] of Char = (
    'B', // NOT esErrorStop,    НЕ !!! Прервать обработку файлов при ошибке (иначе - фиксируем статус и переходим к следующему файлу)
    'I', // esErrorIgnore,      При ошибках обработки файлов вернуть gsOk (иначе gsError)
    'W', // esErrorWarning,     При ошибках обработки файлов вернуть gsWarning (иначе gsError)
    'E'  // esErrorShow,        Показать ошибку пользователю
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
    'F', // rfForceDirs,        Создать путь назначения при необходимости
    'S', // rfSrcSubDirs,       Обработать также вложенные подкаталоги
    'A', // rfDstDirAuto,       Автоматическое вычисление каталога назначения (при задании каталога в виде шаблона)
    'X', // rfDstFixed,         Каталог (или файл!!!) назначения - фиксированный (все файлы из разных подкаталогов будут скопированы в один и тот же)
         //                     Для комманды UPDATE - поиск файлов только в указанном каталоге (без субкаталогов)
    'N', // NOT rfOverWrite,    НЕ !!! Перезаписать существующие файлы
    'Q', // NOT rfRepeatOnError НЕ !!! Повторить при ошибке
    'M', // rfCopyBackup,       Создать резервную копию перезаписываемого файла
    '@', // rfCopyLocked,       Заблокированные файлы компировать с переименованием во временный файл
    'V', // rfCheckCopy,        Сравнить исходный и скопированный файл (CRC32)
    'G', // NOT rfCopyFileDate, НЕ !!! Переносить дату изменения файла
    'J', // NOT rfCopyFileAttr, НЕ !!! Переносить атрибуты файла
    'K', // rfSkipReadOnly,     Пропустить (не обрабатывать) файлы "Только чтение"
    'Y', // NOT rfSkipSysFiles, НЕ !!! Пропустить (не обрабатывать) системные файлы
    'H', // rfSkipHiddenFiles,  Пропустить (не обрабатывать) скрытые файлы
    'D', // rfCompareDate,      Перезаписать файлы, отличные по дате изменения
    'L', // rfCompareSize,      Перезаписать файлы, отличные по размеру
    'T', // rfCompareCrc32,     Перезаписать файлы, отличные по содержимому (CRC32)
    'P'  // rfCopyList,         Добавить имена успешно обработанных файлов в список
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
    'S', // riSrcSubDirs        Обработать также вложенные подкаталоги
    'N', // riCreateValue       Создать значение, если его не было в файле
    'D', // riDeleteValue       Удалить значение, если оно есть в файле
    'A', // riDeleteSection     Удалить секцию, если она есть в файле
    'M', // riNotExistsOk       Если значение не существует - вернуть Ok (иначе Skip)
    'X', // riNotExistsError    Если значение не существует - вернуть Error (иначе Skip)
    'V', // riCompareValues     Изменить только, если предыдущее значение отличается от нового
    'K', // riSkipReadOnly      Пропустить (не обрабатывать) файлы "Только чтение"
    'Y', // riSkipSysFiles      Пропустить (не обрабатывать) системные файлы
    'H'  // riSkipHiddenFiles   Пропустить (не обрабатывать) скрытые файлы
    );

  SRCmdRegFlag  : array [TRfoRegFlag] of Char = (
    'N', // rgCreateValue       Создать значение, если его не было в файле
    'D', // rgDeleteValue       Удалить значение, если оно есть в файле
    'M', // rgNotExistsOk       Если значение не существует - вернуть Ok (иначе Skip)
    'X', // rgNotExistsError    Если значение не существует - вернуть Error (иначе Skip)
    'V'  // rgCompareValues     Изменить только, если предыдущее значение отличается от нового
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
  EScriptProcAlreadyExists    = 'Процедура с именем "%s" уже имеется в списке процедур!';
  EScriptProcNotFound         = 'Процедура с именем "%s" не найдена!';
  EScriptProcErrorCreate      = 'Ошибка создания процедуры "%s"!';
  EScriptProcErrorExec        = 'Ошибка выполнения процедуры "%s": "%s"!';
  EScriptLoadExtScript        = 'Ошибка загрузки внешнего скрипта "%s": "%s"!';
  EScriptNotFound             = 'Скрипт "%s" не найден';
  EScriptInvalidObject        = 'Неверное имя объекта "%s" или объект не существует!';
  EScriptInvalidCurrProc      = 'Не найдена текущая процедура!';
  EScriptObjListNotFound      = 'Не найден текущий список объектов!';
  EScriptObjectNotFound       = 'Объект "%s" не найден!';
  EScriptListEmpty            = 'Нет скриптов для выполнения!';
  SMsgCannotEdit              = 'Выделенный токен не поддерживатся редактором!';

  EParseUnterminatedString    = 'Неожиданный конец строки (строка без завершающего ограничителя): "%s"!';
  EParseUnterminatedBlock     = 'Неожиданный конец блока команд!';
  EParseEmptyToken            = 'Пустой токен!';
  EParseInvalidToken          = 'Недопустимый токен: "%s"!';
  EParseInvalidVariable       = 'Некорректное описание переменной: "%s"!';
  EParseInvalidProcName       = 'Имя процедуры не определено!';
  EParseInvalidAttrChar       = 'Некорректный символ "%s" в описании атрибутов операции!';
  EParseInvalidParamsCount    = 'Некорректное число параметров команды: %d. Для данной команды необходимо указать %d параметр(ов)!';
  EParseInvalidParamsNumber   = 'Некорректный индекс параметра: "%d"';
  EParseInvalidParam          = 'Некорректный параметр команды: "%s"!';
  EParseWaitBegin             = 'Блок команд должен начинаться с токена "begin"!';
  EParseNoActBlock            = 'Не определен текущий блок условий!';
  EParseNoEndBlock            = 'Ошибка в определении блока условий!';
  EParseEmplyBlock            = 'Пустой блок условий: "%s"!';

  EExecuteCommand             = 'Ошибка выполнения: "%s"!';
  EExecuteFunction            = 'Ошибка в команде "%s": %s!';
  EExecuteOpenFile            = 'Ошибка чтения файла: "%s"!';
  EExecuteSaveFile            = 'Ошибка записи файла: "%s"!';
  EExecuteRegKeyNotExists     = 'Ключ "%s\%s" не найден!';

  SExecuteRun                 = 'Запуск скрипта "%s"...';
  SExecuteShowMessage         = 'Сообщение: "%s"';
  SExecuteStartProcedure      = 'Запуск процедуры "%s"...';
  SExecuteCreateScript        = 'Выполнение процедуры "%s" сервером сценариев...';
  SExecuteReturnProcedure     = 'Процедура "%s" %s';
  SExecuteReturnScript        = 'Скрипт "%s" %s';
  SExecuteRestoreFiles        = 'Откат изменений...';
  SExecuteLoadExtScript       = 'Загрузка внешнего скрипта "%s"...';
  SExecuteAnalyzeFunctions    = 'Проверка условий...';
  SExecuteTrue                = 'Условие выполнено - выполнение блока <then>...';
  SExecuteElse                = 'Условие не выполнено - выполнение блока <else>...';
  SExecuteSetTitle            = 'Установка значения переменной';
  SExecuteSet                 = 'Переменной "%s" присвоено значение "%s"';
  SExecuteCycleCount          = 'В списке "%s" найдено %d записей.';
  SExecuteCycleSet            = 'Иттерация %d: переменной "%s" присвоено значение "%s"';
  SExecuteChkVar              = 'Проверка наличия переменной "%s"';
  SExecuteVarExists           = 'Указанная переменная уже существует в списке переменных.';
  SExecuteEndIf               = 'Конец блока <%s>.';
  SExecuteCycleBegin          = 'Начало цикла <%s>...';
  SExecuteCycleForBegin       = 'Выполнение цикла для всех элементов списка <%s>.';
  SExecuteCycleForFile        = 'Выполнение цикла для всех файлов "%s" в каталоге "%s" (без учета подкаталогов).';
  SExecuteCycleForFileS       = 'Выполнение цикла для всех файлов "%s" в каталоге "%s" (включая вложенные подкаталоги).';
  SExecuteCycleForDirs        = 'Выполнение цикла для всех каталогов "%s" (без учета подкаталогов).';
  SExecuteCycleForDirsS       = 'Выполнение цикла для всех каталогов "%s" (включая вложенные подкаталоги).';
  SExecuteCycleBreak          = 'Выполнение цикла прервано из-за превышения числа иттераций (%d)!';
  SExecuteCycleEnd            = 'Конец цикла <%s>.';
  SExecuteBreakQry            = 'Прервать выполнение скрипта?';
  SExecuteBreakScript         = 'Выполнение скрипта прервано!';
  SExecuteBreakBlock          = 'Выполнение цикла прервано.';
  SExecuteBreakError          = 'Не удалось произвести выход из цикла!';
  SExecutePause               = 'Пауза %d секунд(ы)...';
  SExecuteWinWaitOpen         = 'Ожидание создания окна "%s"';
  SExecuteWinWaitOpenOk       = 'Окно "%s" найдено.';
  SExecuteWinWaitClose        = 'Ожидание закрытия окна "%s"';
  SExecuteWinWaitCloseOk      = 'Окно "%s" не обнаружено.';
  SExecuteSetState            = 'Для текущей процедуры установлен статус: %s';
  SExecuteFreeObject          = 'Удаление объекта "%s" из памяти';
  SExecuteFindDirs            = 'Создание списка каталогов "%s"';
  SExecuteFindFiles           = 'Создание списка файлов "%s"\"%s"';
  SExecuteInvalidPosition     = 'Неверная позиция в списке: %d! Текущее количество строк в списке: %d.';
  SExecuteInvalidObject       = 'Не найден объект или неверный тип объекта "%s"!';
  EScriptExistsObject         = 'Объект с именем "%s" уже существует в списке объектов!';
  SExecuteListFirst           = 'Выборка первой строки из списка "%s"';
  SExecuteListPrior           = 'Выборка предыдущей строки из списка "%s"';
  SExecuteListNext            = 'Выборка следующей строки из списка "%s"';
  SExecuteListLast            = 'Выборка последней строки из списка "%s"';
  SExecuteTextCreate          = 'Создание текста с именем "%s".';
  SExecuteTextLoadTitle       = 'Загрузка текста "%s" из файла "%s"';
  SExecuteTextLoadResult      = 'Считано %d строк(и) / %d символ(а,ов).';
  SExecuteTextSaveTitle       = 'Запись текста "%s" в файл "%s"';
  SExecuteTextSaveResult      = 'Записано %d строк(и) / %d символ(а,ов).';
  SExecuteTextAddChars        = 'Добавление символ(а,ов) "%s" к тексту "%s"';
  SExecuteTextAddLine         = 'Добавление строк(и) "%s" к тексту "%s"';
  SExecuteTextAdd             = 'Добавлено %d символ(а,ов) (%s).';
  SExecuteTextReplaceTitle    = 'Замена в тексте "%s" строки "%s" на строку "%s"';
  SExecuteTextReplaceProgress = 'Просмотр текста, подождите пожалуйста...'#13'Произведено замен - %d.';
  SExecuteTextReplaceResult   = 'Произведено %d замен.';
  SExecuteTextSet             = 'Загрузка в переменную "%s" значения из блока текста "%s"';
  SExecuteStrReplace          = 'Поиск и замена подстроки "%s" на "%s" для переменной "%s"';
  SExecuteIniReadVariable     = 'Чтение значения переменной "%s" из INI-файла "%s", секция "%s", ключ "%s"';
  SExecuteIniSectionsTitle    = 'Чтение наименований секций в список "%s" из INI-файла "%s"';
  SExecuteIniKeysTitle        = 'Чтение наименований параметров в список "%s" из секции "%s" INI-файла "%s"';
  SExecuteIniReadResult       = 'Считано %d значений(е).';
  SExecuteRegReadVariable     = 'Чтение значения переменной "%s" из реестра, раздел "%s", секция "%s", ключ "%s"';
  SExecuteRegSectionsTitle    = 'Чтение наименований секций в список "%s" из раздела реестра "%s\%s"';
  SExecuteRegKeysTitle        = 'Чтение наименований параметров в список "%s" из раздела реестра "%s\%s"';
  SExecuteRegReadResult       = 'Считано %d значений(е).';
  SExecuteShowVarsTitle       = 'Вывод текущего списка переменных (отладочная информация)';
  SExecuteShowVarsResult      = 'Всего %d переменных.';
  SExecuteScriptFileAppend    = 'Запись в файл "%s" строки "%s"';
  SExecuteCalcCrc32           = 'Вычисление контрольной суммы CRC32 для файла "%s"';
  SExecuteFtpOpen             = 'Подключение к FTP-серверу "%s"';
  SExecuteFtpOpenOk           = 'Подключение к FTP-серверу выполнено. Текущий каталог: "%s"';
  SExecuteFtpOpenError        = 'Ошибка подключения к FTP-серверу "%s"!';
  SExecuteFtpNotConnected     = 'Подключение к FTP-серверу не установлено!';
  SExecuteFtpOk               = 'Команда успешно выполнена.';
  SExecuteFtpOkDir            = 'Команда успешно выполнена. Текущий каталог: "%s"';
  SExecuteFtpError            = 'Ошибка выполнения команды "%s"!';
  SExecuteFtpClose            = 'Отключение от FTP-сервера';
  SExecuteFtpClosed           = 'Подключение к FTP-серверу завершено.';
  SExecuteFtpFree             = 'Объект "%s" удален из памяти.';
  SExecuteFtpCurrDir          = 'Получение текущего каталога на FTP-сервере';
  SExecuteFtpChDir            = 'Переход в каталог "%s" на FTP-сервере';
  SExecuteFtpMkDir            = 'Создание каталога "%s" на FTP-сервере';
  SExecuteFtpFrDir            = 'Проверка каталога "%s" на FTP-сервере';
  SExecuteFtpRmDir            = 'Удаление каталога "%s" на FTP-сервере';
  SExecuteFtpGet              = 'Загрузка файла "%s" c FTP-сервера';
  SExecuteFtpPut              = 'Загрузка файла "%s" на FTP-сервер';
  SExecuteFtpDelete           = 'Удаление файла "%s" c FTP-сервера';

  SExecuteStateOk             = 'завершена успешно.';
  SExecuteStateWarning        = 'завершена с предупреждениями!';
  SExecuteStateError          = 'завершена с ошибками!';
  SExecuteStateErrorStop      = 'прервана из-за ошибки!';
  SExecuteStateErrorRestore   = 'прервана из-за ошибки! Необходим откат изменений.';
  SExecuteStateErrorUndo      = 'прервана из-за ошибки! Необходим запуск процедуры UNDO.';

  SExecuteScriptSaveTitle     = 'Запись скрипта "%s" в файл "%s"';
  SExecuteScriptSaveError     = 'Скрипт "%s" не найден!';

  SScriptStateOk              = 'завершен успешно.';
  SScriptStateWarning         = 'завершен с предупреждениями!';
  SScriptStateError           = 'завершен с ошибками!';
  SScriptStateErrorStop       = 'прерван из-за ошибки!';
  SScriptStateErrorRestore    = 'прерван из-за ошибки! Выполнен откат изменений.';
  SScriptStateErrorUndo       = 'прерван из-за ошибки! Выполнен запуск процедуры UNDO.';

  SCompileStart               = 'Компиляция скрипта "%s"...';
  SCompileError               = 'Ошибка [символ %d, строка %d]: %s';
  SCompileEmpty               = 'Скрипт пуст - нет данных для компиляции!';
  SCompileResult              = 'Обработано %d строк(и), загружено %d команд(ы).';

  SViewerWait                 = 'Подождите пожалуйста...';
  SViewerStatComplete         = 'Завершено %.1f%% [%d из %d] за %s';
  SViewerStatElapsed          = ', осталось %s';
  SViewerEmpty                = '';

  SCmdNoteShowVariables       = ' - (DEBUG) вывести текущий список переменных';
  SCmdNoteSet                 = '"name"="value" - установить значение переменной';
  SCmdNoteChkVar              = '"name" "value" - если переменная не существует, создать с указанным значением';
  SCmdNoteInfo                = '"строка 1" "строка 2" ... "строка N" - вывод сообщения пользователю';
  SCmdNoteFDirs               = '"путь" - создать каталог';
  SCmdNoteChangeDir           = '"имя" - сменить каталог';
  SCmdNoteMCopy               = '"откуда" "куда" "маска" - копировать файлы или каталоги по маске';
  SCmdNoteFCopy               = '"откуда" "куда" - копировать файлы или каталоги';
  SCmdNoteMMove               = '"откуда" "куда" "маска" - переместить файлы или каталоги по маске';
  SCmdNoteFMove               = '"откуда" "куда" - переместить файлы или каталоги';
  SCmdNoteUpdate              = '"откуда" "куда" "маска" - обновить существующие файлы';
  SCmdNoteRename              = '"исходное имя" "новое имя" - переименовать файл или каталог';
  SCmdNoteDelete              = '"путь" - удалить файлы или каталоги';
  SCmdNoteBreak               = ' - прервать работу текущего блока (цикла)';
  SCmdNoteReturn              = ' - завершить работу скрипта';
  SCmdNoteChkState            = ' - проверить глобальный статус скрипта';
  SCmdNoteSetState            = ' - установить глобальный статус скрипта';
  SCmdNotePause               = '"время в секундах" "[сообщение]" - приостановить работу скрипта';
  SCmdNoteWinWait             = '"имя окна" "[сообщение]" - ожидание создания и(или) закрытия окна с указанным именем';
  SCmdNoteCall                = '"имя", "список переменных" - вызов процедуры';
  SCmdNoteExec                = '"команда" "рабочий каталог" - запустить внешнюю программу';
  SCmdNoteExecEx              = '"команда" "рабочий каталог", "коды [OK]", "коды [WARNING]" - запустить внешнюю программу с контролем кодов возврата';
  SCmdNoteExecAs              = '"команда" "каталог", "домен", "имя", "пароль" - запустить программу от имени пользователя';
  SCmdNoteOpen                = '"файл" "параметры" - открыть внешний файл';
  SCmdNoteOpenEx              = '"файл" "параметры", "коды [OK]", "коды [WARNING]" - открыть внешний файл с контролем кодов возврата';
  SCmdNoteLink                = '"команда" "рабочий каталог" "файл ярлыка" - создать ярлык к файлу';
  SCmdNoteLinkEx              = '"команда" "рабочий каталог" "атрибуты" "файл ярлыка" - создать ярлык к файлу с параметрами';
  SCmdNoteFileAppend          = '"имя файла", "строка" - добавить строку к файлу';
  SCmdNoteChIni               = '"файл" "секция" "параметр" "значение" - изменить значение параметра в INI-файле';
  SCmdNoteChReg               = '"раздел", "секция" "параметр" "тип" "значение" - изменить значение параметра в реестре';
  SCmdIniReadVariable         = '"переменная" "файл" "секция" "параметр" "значение" - считать значение из INI-файла в переменную';
  SCmdIniReadSections         = '"имя объекта" "файл" - считать имена секций из INI-файла в список';
  SCmdIniReadKeys             = '"имя объекта" "файл" "секция" - считать имена параметров из секции INI-файла в список';
  SCmdRegReadVariable         = '"переменная" "раздел" "секция" "параметр" "значение" - считать значение из реестра в переменную';
  SCmdRegReadSections         = '"имя объекта" "раздел" "секция" - считать имена вложенных секций из секции реестра в список';
  SCmdRegReadKeys             = '"имя объекта" "раздел" "секция" - считать имена параметров из секции реестра в список';
  SCmdNoteNetMap              = '"диск" "сетевой путь" - подключить сетевой диск';
  SCmdNoteNetUnmap            = '"диск" - отключить сетевой диск';
  SCmdNoteBdeAliasCreate      = '"имя" "драйвер" "параметры (имя: значение ; ...)" - создать BDE alias';
  SCmdNoteBdeAliasDelete      = '"имя" - удалить BDE alias';
  SCmdNoteAddPrinter          = '"имя", "драйвер", "порт", "winprint", "комментарий", "расположение", "шара" - добавить локальный принтер';
  SCmdNoteDelPrinter          = '"имя" - удалить локальный принтер';
  SCmdNoteAddNetPrinter       = '"сетевое имя" - подключить сетевой принтер для текущего пользователя';
  SCmdNoteDelNetPrinter       = '"сетевое имя" - отключить сетевой принтер для текущего пользователя';
  SCmdNoteFreeObject          = '"имя объекта" - удалить объект из памяти';
  SCmdNoteFindDirs            = '"имя объекта", "путь" - создать список каталогов';
  SCmdNoteFindFiles           = '"имя объекта", "путь", "маски" - создать список файлов в каталоге';
  SCmdNoteListFirst           = '"имя объекта" - перейти на первую строку списка (переменная "имя объекта"="строка")';
  SCmdNoteListPrior           = '"имя объекта" - перейти на предыдущую строку списка (переменная "имя объекта"="строка")';
  SCmdNoteListNext            = '"имя объекта" - перейти на следующую строку списка (переменная "имя объекта"="строка")';
  SCmdNoteListLast            = '"имя объекта" - перейти на последнюю строку списка (переменная "имя объекта"="строка")';
  SCmdNoteExtractFileName     = '"имя файла", "переменная" - вырезать имя файла и поместить значение в переменную';
  SCmdNoteExtractFilePath     = '"имя файла", "переменная" - вырезать путь к файлу и поместить значение в переменную';
  SCmdNoteExtractFileExt      = '"имя файла", "переменная" - вырезать расширение файла и поместить значение в переменную';
  SCmdNoteChangeFileExt       = '"имя файла", "расширение", "переменная" - изменить расширение файла и поместить значение в переменную';
  SCmdNoteCheckFileName       = '"имя файла (каталога)", "переменная" - исправить имя файла в соответствии с допустимыми стандартами';
  SCmdNoteExitWindows         = ' - завершить сеанс пользователя или работу Windows';
  SCmdNoteServiceStart        = '"имя сервиса" - запустить сервис с указанным системным именем';
  SCmdNoteServiceStop         = '"имя сервиса" - остановить сервис с указанным системным именем';
  SCmdNoteTextCreate          = '"имя объекта" - создать текст (список строк)';
  SCmdNoteTextOpen            = '"имя объекта", "имя файла" - загрузить текст из файла';
  SCmdNoteTextSave            = '"имя объекта", "имя файла" - сохранить текст в файле';
  SCmdNoteTextAddChars        = '"имя объекта", "символы" - добавить символы к тексту';
  SCmdNoteTextAddLine         = '"имя объекта", "строка" - добавить строку к тексту';
  SCmdNoteTextReplace         = '"имя объекта", "что искать", "чем заменить" - поиск и замена подстроки в тексте';
  SCmdNoteTextSet             = '"имя объекта", "имя переменной" - записать в указанную переменную текст';
  SCmdCalcCrc32               = '"имя переменной", "имя файла" - посчитать контрольную сумму файла (CRC32) в указанную переменную';

  SCmdSubString               = '"переменная", "строка", "индекс", "разделители" - вырезать подстроку из строки';
  SCmdSubStrings              = '"переменная", "строка", "разделители" - определить количество подстрок в строке';
  SCmdStrReplace              = '"переменная", "что искать", "чем заменить" - поиск и замена подстроки в значении переменной';

  SCmdNoteFtpOpen             = '"имя объекта", "адрес", "имя пользователя", "пароль" - подключится к FTP-серверу';
  SCmdNoteFtpCurrDir          = '"имя объекта", "имя переменной" - получить текущий каталог на FTP-сервере';
  SCmdNoteFtpChDir            = '"имя объекта", "имя каталога" - сменить текущий каталог на FTP-сервере';
  SCmdNoteFtpMkDir            = '"имя объекта", "имя каталога" - создать каталог на FTP-сервере';
  SCmdNoteFtpFrDir            = '"имя объекта", "имя каталога" - проверить и создать каталог на FTP-сервере';
  SCmdNoteFtpRmDir            = '"имя объекта", "имя каталога" - удалить каталог на FTP-сервере';
  SCmdNoteFtpGet              = '"имя объекта", "удаленное имя файла", "локальное имя файла" - загрузить файл с FTP-сервера';
  SCmdNoteFtpPut              = '"имя объекта", "локальное имя файла", "удаленное имя файла" - отправить файл на FTP-сервер';
  SCmdNoteFtpDelete           = '"имя объекта", "удаленное имя файла" - удалить файл с FTP-сервера';
  SCmdNoteFtpClose            = '"имя объекта" - закрыть соединение с FTP-сервером';

  SCmdNoteScriptSave          = '"алиас", "имя файла" - загрузить скрипт и сохранить его в файле';

  SCmdNoteIf                  = ': Если "условие" То {команды} Иначе {команды} - (выполнить в зависимости от условия)';
  SCmdNoteWhile               = ': Пока "условие" Повторять {команды} - (повторять, пока выполняется условие)';
  SCmdNoteRepeat              = ': Выполнить {команды} Если не "условие" повторить - (повторять, пока не будет выполнено условие)';

  SCmdNoteForEach             = ': Для каждого элемента "списка" выполнить {команды}';
  SCmdNoteForFile             = ': Для каждого файла (по "маске") выполнить {команды}';
  SCmdNoteForDirs             = ': Для каждого каталога (по "маске") выполнить {команды}';

  SFncNoteStateIsOk           = '() - сравнить текущий статус скрипта с "OK"';
  SFncNoteStateIsGood         = '() - сравнить текущий статус скрипта с "OK" или "WARNING"';
  SFncNoteStateIsWarning      = '() - сравнить текущий статус скрипта с "WARNING"';
  SFncNoteStateIsError        = '() - сравнить текущий статус скрипта с "ERROR"';
  SFncNoteVarExists           = '("name") - проверить наличие переменной';
  SFncNoteVarIs               = '("name"="value") - проверить значение переменной';
  SFncNoteKeyExists           = '("key") - проверить наличие ключа в списке ключей';
  SFncNoteWinExists           = '("заголовок окна") - проверить наличие окна Windows';
  SFncNoteTaskExists          = '("имя файла") - проверить наличие выполняющегося процесса Windows';
  SFncNoteQuery               = '("строка") - вывод запроса к пользователю (да / нет)';
  SFncNoteFileExists          = '("имя файла") - проверить наличие файла';
  SFncNoteDirExists           = '("каталог") - проверить наличие каталога';
  SFncNoteMaskExists          = '("маска") - проверить наличие файлов по маске';
  SFncNoteDiskFree            = '("размер в Мб") - проверить наличие свободного места на текущем диске в Мб';
  SFncNoteTimeAfter           = '("дд/мм/гггг чч:мм:сс") - вернет TRUE после указанного времени';
  SFncNoteTimeBefore          = '("дд/мм/гггг чч:мм:сс") - вернет TRUE ранее указанного времени';
  SFncNoteIsAdmin             = '() - проверить наличие администраторских прав в системе';
  SFncNoteBdeEnabled          = '() - проверить доступность BDE в системе';
  SFncNoteBdeCheckAlias       = '("имя") - проверить наличие псевдонима BDE';
  SFncNotePrinterExists       = '("имя") - проверить наличие принтера в системе';
  SFncNoteObjIsExists         = '("имя объекта") - проверить существование объекта';
  SFncNoteObjIsList           = '("имя объекта") - проверить, является ли объект списком';
  SFncNoteListIsNotEmpty      = '("имя объекта") - проверить, если в списке строки';
  SFncNoteListIsBof           = '("имя объекта") - проверить, установлен ли указатель в начало списка';
  SFncNoteListIsEof           = '("имя объекта") - проверить, установлен ли указатель в конец списка';
  SFncIniSecExists            = '("имя INI-файла;секция") - проверить, существует ли секция в INI-файле';
  SFncIniKeyExists            = '("имя INI-файла;секция;ключ") - проверить, существует ли ключ (параметр) в INI-файле';
  SFncIniValCompare           = '("имя INI-файла;секция;ключ;значение") - проверить, соответствует ли значение ключа в INI-файле указанному';
  SFncRegValueExists          = '("раздел;секция") - проверить, существует ли параметр в реестре';
  SFncRegKeyExists            = '("раздел;параметр") - проверить, существует ли ключ в реестре';
  SFncRegValCompare           = '("раздел;секция;параметр;значение") - проверить, соответствует ли значение ключа в реестре указанному';
  SFncSelectFile              = '("имя_переменной<;описание>") - выбрать имя файла и поместить значение в переменную';
  SFncSelectDir               = '("имя_переменной<;описание>") - выбрать имя каталога и поместить значение в переменную';
  SFncServiceExists           = '("имя_сервиса") - проверить наличие сервиса с указанным системным именем';
  SFncServiceStarted          = '("имя_сервиса") - проверить, запущен ли сервис с указанным системным именем';
  SFncServiceStopped          = '("имя_сервиса") - проверить, остановлен ли сервис с указанным системным именем';
  SFncNoteTextFind            = '("имя объекта;строка") - проверить, существует ли указанная строка в тесте';

  SRCmdErrLockNote            = 'Запретить отмену операции пользователем';
  SRCmdExtExecNote            = 'Выполнить команду с помощью сервера сценариев';
  SRCmdErrAttrNoteContinue    = 'При ошибке команды продолжить выполнение скрипта';
  SRCmdErrAttrNoteRestore     = 'При ошибке команды остановить скрипт и откатить изменения';
  SRCmdErrAttrNoteRunUndo     = 'При ошибке команды остановить скрипт и запустить процедуру Undo';

  SRCmdRfoNoteLocalObj        = 'Использовать локальный список объектов';

  SRCmdErrFlagNoteErrorStop   = 'При ошибках обработки файлов продолжить обработку других файлов';
  SRCmdErrFlagNoteErrorStop1  = 'При ошибках выполнить команду от имени текущего пользователя';
  SRCmdErrFlagNoteErrorIgnore = 'При ошибках обработки файлов вернуть [Ok] (иначе [Error])';
  SRCmdErrFlagNoteErrorWarn   = 'При ошибках обработки файлов вернуть [Warning] (иначе [Error])';
  SRCmdErrFlagNoteErrorShow   = 'Показать ошибку пользователю';

  SRCmdRfoNoteEmptyOk         = 'Если файлы не найдены - вернуть [Ok] (иначе [Warning])';
  SRCmdRfoNoteEmptyError      = 'Если файлы не найдены - вернуть [Error] (иначе [Warning])';

  SRMsgBoxTypeNoteInfo        = 'Тип окна сообщения: информация (Info)';
  SRMsgBoxTypeNoteWarning     = 'Тип окна сообщения: предупреждение (Warning)';
  SRMsgBoxTypeNoteError       = 'Тип окна сообщения: ошибка (Error)';

  SRCmdRfoNoteEmpty           = '--- В этой команде данный ключ игнорируется ---';
  SRCmdRfoNoteForceDirs       = 'Создать путь назначения при необходимости';
  SRCmdRfoNoteSrcSubDirs      = 'Обработать вложенные подкаталоги';
  SRCmdRfoNoteDstDirAuto      = 'Автоматическое вычисление каталога назначения';
  SRCmdRfoNoteDstDirAutoDelete = 'Удалить только файлы (ключ X будет игнорирован)';
  SRCmdRfoNoteDstFixed        = 'Фиксированный каталог или файл назначения (из разных подкаталогов в один)';
  SRCmdRfoNoteDstFixedDelete  = 'Удалить только пустые каталоги, не содержащие файлов и каталогов';
  SRCmdRfoNoteDstFixedUpdate  = 'Поиск файлов только в указанном каталоге-источнике (без подкаталогов)';
  SRCmdRfoNoteOverWrite       = 'Не перезаписывать существующие файлы';
  SRCmdRfoNoteRepeatOnError   = 'Не делать повторных попыток при ошибках обработки';
  SRCmdRfoNoteCopyBackup      = 'Создать резервную копию перезаписываемого файла на время копирования';
  SRCmdRfoNoteCopyLocked      = 'Если файл уже существует и блокирован - переименовать к виду *.~*';
  SRCmdRfoNoteCheckCopy       = 'Сравнить исходный и скопированный файл (CRC32)';
  SRCmdRfoNoteCopyFileDate    = 'Не переносить дату изменения файла';
  SRCmdRfoNoteCopyFileAttr    = 'Не переносить атрибуты файла';
  SRCmdRfoNoteSkipReadOnly    = 'Пропустить (не обрабатывать) файлы "Только чтение"';
  SRCmdRfoNoteSkipSysFiles    = 'Обрабатывать системные файлы';
  SRCmdRfoNoteSkipSysFiles2   = 'Пропустить (не обрабатывать) файлы системные файлы';
  SRCmdRfoNoteSkipHiddenFiles = 'Пропустить (не обрабатывать) скрытые файлы';
  SRCmdRfoNoteCompareDate     = 'Перезаписать файлы, отличные по дате изменения';
  SRCmdRfoNoteCompareSize     = 'Перезаписать файлы, отличные по размеру';
  SRCmdRfoNoteCompareCrc32    = 'Перезаписать файлы, отличные по содержимому (CRC32)';
  SRCmdRfoNoteCopyList        = 'Добавить имена успешно обработанных файлов в список отката';
  SRCmdSwwFlagNote0           = 'Скрыть окно (SW_HIDE)';
  SRCmdSwwFlagNote1           = 'Нормальный режим (SW_SHOWNORMAL)';
  SRCmdSwwFlagNote2           = 'Минимизировать окно (SW_SHOWMINIMIZED)';
  SRCmdSwwFlagNote3           = 'Развернуть окно на весь экран (SW_SHOWMAXIMIZED)';
  SRCmdSwwFlagNote4           = 'Показать не активное окно (SW_SHOWNOACTIVATE)';
  SRCmdSwwFlagNote5           = 'Показать окно (SW_SHOW)';
  SRCmdSwwFlagNote6           = 'Свернуть окно (SW_MINIMIZE)';
  SRCmdSwwFlagNote7           = 'Свернуть окно и не активировать (SW_SHOWMINNOACTIVE)';
  SRCmdSwwFlagNote8           = 'Показать окно и не активировать (SW_SHOWNA)';
  SRCmdSwwFlagNote9           = 'Восстановить окно (SW_RESTORE)';
  SRCmdSwwFlagNote10          = 'Показать окно с параметрами по умолчанию (SW_SHOWDEFAULT)';
  SRCmdSwwFlagWaitExit        = 'Ожидать завершения команды';
  SRCmdSwwFlagWaitOpen        = 'Ожидать создания окна';
  SRCmdSwwFlagWaitClose       = 'Ожидать закрытия окна';
  SRCmdSwwFlagCheckOk         = 'Контролировать код возврата (0 + коды [OK], коды [WARNING])';
  SRCmdNetMapNoteAny          = 'Тип не определен';
  SRCmdNetMapNoteDisk         = 'Сетевой диск';
  SRCmdNetMapNotePrint        = 'Принтер';
  SRCmdFlagNoteCreateValue    = 'Создать значение, если оно не существует';
  SRCmdFlagNoteDeleteValue    = 'Удалить значение, если оно существует';
  SRCmdFlagNoteDeleteSection  = 'Удалить секцию, если она существует';
  SRCmdFlagNoteNotExistsOk    = 'Если значение не существует - вернуть [Ok] (иначе [Ignored])';
  SRCmdFlagNoteNotExistsError = 'Если значение не существует - вернуть [Error] (иначе [Ignored])';
  SRCmdFlagNoteCompareValues  = 'Изменить только, если предыдущее значение отличается от нового';
  SRCmdRetFlagNoteNoChange    = 'Не изменять статус скрипта';
  SRCmdRetFlagNoteOk          = 'Установить статус [Ok]';
  SRCmdRetFlagNoteWarning     = 'Установить статус [Warning]';
  SRCmdRetFlagNoteError       = 'Установить статус [Error]';
  SRCmdRetFlagNoteErrorStop   = 'Установить статус [Error] и остановить скрипт';
  SRCmdRetFlagNoteErrorRestore = 'Установить статус [Error] и откатить изменения';
  SRCmdRetFlagNoteErrorUndo   = 'Установить статус [Error] и запустить процедуру Undo';

  SRCmdNetMapNoteSaveProfile  = 'Записать в профиль текущего пользователя';
  SRCmdNetMapNoteForceUnmap   = 'Принудительно закрыть все открытые файлы';

  SRNoteExitWinFlag_LOGOFF    = 'Завершить сеанс пользователя';
  SRNoteExitWinFlag_SHUTDOWN  = 'Завершить работу Windows';
  SRNoteExitWinFlag_REBOOT    = 'Перезагрузка Windows';
  SRNoteExitWinFlag_POWEROFF  = 'Выключить питание компьютера';
  SRNoteExitWinFlag_FORCE     = 'Принудительно закрыть программы';
  SRNoteExitWinFlag_FORCEIFHUNG = 'Принудительно закрыть "зависшие" программы';

  SRNoteTextIgnoreCase        = 'Не учитывать регистр символов';
  SRNoteTextReplaceAll        = 'Заменить все вхождения';
  SRNoteTextOemCode           = 'Текст в DOS (CP-866) кодировке';

  SRNoteFtpPassive            = 'Пассивный режим';
  SRNoteFtpBinary             = 'Двоичный (binary) режим';
  SRNoteFtpErrIgnore          = 'При ошибках обработки вернуть [Ok] (иначе [Error])';
  SRNoteFtpErrWarning         = 'При ошибках обработки вернуть [Warning] (иначе [Error])';

  SRNoteTextWriteLn           = 'Добавить символы завершения строки ($0D$0A)';

  SsmHideAll                  = 'Скрытое выполнение';
  SsmLogDisable               = 'Только индикатор';
  SsmLogHidden                = 'Индикатор + протокол свернут';
  SsmLogShowing               = 'Индикатор + протокол показан';
  SsmLogAlways                = 'Индикатор + протокол всегда';

  SrsOk                       = '[OK]';
  SrsWarning                  = '[OK][WRN]';
  SrsAny                      = '[OK][WRN][ERR]';
  SrsNone                     = '[NONE]';

  LrsOk                       = 'Нет ошибок и предупреждений';
  LrsWarning                  = 'Нет ошибок';
  LrsAny                      = 'Не контролировать статус';
  LrsNone                     = 'Не запускать (буферизация)';

  SrsApply                    = 'Учитывать';
  SrsIgnore                   = 'Игнорировать';

  LrsApply                    = 'Учитывать код возврата';
  LrsIgnore                   = 'Игнорировать код возврата';

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

