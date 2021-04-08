unit RScrCmds;

interface

uses
  Classes, SysUtils,
  IdBaseComponent, IdComponent, IdGlobal, IdURI, IdFtp, IdFTPCommon,
  RScripts, RMsgTypes, RScrCnst, RFileProcs, RDbiProcs, RPrnProcs, RSysProcs;

type
  TRCmdSetVariable = class (TRCmdExecuted)
  protected
    function  GetCommandParams_Edit: string; override;
    function  GetCommandParams_Exec: string; override;
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  // added 2012-02-22
  TRCmdCheckVariable = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdShowVariables = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdSubString = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdSubStrings= class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdPause = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdWinWait = class (TRCmdExecuted)
  private
    fWaitOpen: Boolean;
    fWaitClose: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdBreak = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdReturn = class (TRCmdExecuted)
  private
    fRetFlag: TRCmdRetFlag;
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdSetState = class (TRCmdExecuted)
  private
    fSetState: TRScriptState;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
    procedure Execute(var State: TRScriptState); override;
  end;

  TRCmdCallProc = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecOnExtProcessor(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    function GetCommandCount: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdShowInfo = class (TRCmdExecuted)
  private
    fBoxType: TRMsgBoxType;
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdRfoCustom = class (TRCmdExecuted)
  private
    fErrOptions: TRErrorsOptions;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    function  ExecuteRfo: TROperation; virtual; abstract;
  public
    constructor Create(Script: TRScriptCustom); override;
    function GetErrorOptions: TRErrorsOptions;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdRfoMulti = class (TRCmdRfoCustom)
  private
    fEmptyFlag: TREmptyState;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdRfoFiles = class (TRCmdRfoMulti)
  private
    fFileFlags: TRfoFileFlags;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdChangeDir = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdForceDirs = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdCopyMask = class (TRCmdRfoFiles)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdCopyFile = class (TRCmdRfoFiles)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdMoveMask = class (TRCmdRfoFiles)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdMoveFile = class (TRCmdRfoFiles)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdUpdate = class (TRCmdRfoFiles)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdRenameFile = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdDelete = class (TRCmdRfoFiles)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
end;

  TRCmdRfoExec = class (TRCmdRfoCustom)
  private
    fShowWindow: Integer;
    fCheckOk: Boolean;
    fWaitExit: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdExec = class (TRCmdRfoExec)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdExecEx = class (TRCmdRfoExec)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdExecAs = class (TRCmdRfoExec)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdOpen = class (TRCmdRfoExec)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdOpenEx = class (TRCmdRfoExec)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdCreateLink = class (TRCmdRfoCustom)
  private
    fForceDirs: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    function  ExecuteRfo: TROperation; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdCreateLinkEx = class (TRCmdRfoCustom)
  private
    fForceDirs: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    function  ExecuteRfo: TROperation; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdNetMap = class (TRCmdRfoCustom)
  private
    fTypeRes: Cardinal;
    fSaveProfile: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    function ExecuteRfo: TROperation; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdNetUnmap = class (TRCmdRfoCustom)
  private
    fForceDisconnect: Boolean;
    fSaveProfile: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    function ExecuteRfo: TROperation; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdChangeIni = class (TRCmdRfoMulti)
  private
    fIniFlags: TRfoIniFlags;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    function ExecuteRfo: TROperation; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdChangeReg = class (TRCmdRfoCustom)
  private
    fRegFlags: TRfoRegFlags;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    function ExecuteRfo: TROperation; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdBdeAliasCreate = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdBdeAliasDelete = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdPrinterAdd = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdPrinterDel = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdNetPrinterAdd = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdNetPrinterDel = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdObjCustom = class (TRCmdExecuted)
  private
    fLocalFlag: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
    property Local: Boolean read fLocalFlag;
  end;

  TRCmdFreeObject = class (TRCmdObjCustom)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdListCustom  = class (TRCmdObjCustom)
  protected
    procedure CheckListType(var List: TRListPosition; const TargetType: CRListPosition);
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); virtual; abstract;
  end;

  TRCmdListFirst = class (TRCmdListCustom)
  protected
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdListPrior = class (TRCmdListCustom)
  protected
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdListNext = class (TRCmdListCustom)
  protected
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdListLast = class (TRCmdListCustom)
  protected
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFindCustom = class (TRCmdObjCustom)
  private
    fSubDirs: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    function  ExecuteFind(var List: TRFileListPos): TRTransaction; virtual; abstract;
  public
    constructor Create(Script: TRScriptCustom); override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdFindDirs = class (TRCmdFindCustom)
  protected
    function ExecuteFind(var List: TRFileListPos): TRTransaction; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFindFiles = class (TRCmdFindCustom)
  protected
    function ExecuteFind(var List: TRFileListPos): TRTransaction; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdExtractFilePath = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdExtractFileName = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdExtractFileExt = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdChangeFileExt = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdCheckFileName = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdExitWindows = class (TRCmdRfoCustom)
  private
    fFlags: Longword;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    function  ExecuteRfo: TROperation; override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdServiceStart = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdServiceStop = class (TRCmdRfoCustom)
  protected
    function ExecuteRfo: TROperation; override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdTextCustom = class (TRCmdObjCustom)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState); virtual; abstract;
  end;

  TRCmdTextCreate = class (TRCmdObjCustom)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdTextOpen = class (TRCmdTextCustom)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdTextSave = class (TRCmdTextCustom)
  protected
    procedure ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdTextAddChars = class (TRCmdTextCustom)
  protected
    procedure ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdTextAddLine = class (TRCmdTextCustom)
  protected
    procedure ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdTextReplace = class (TRCmdTextCustom)
  private
    fOem: Boolean;
    fFlags: TReplaceFlags;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    procedure ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState); override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdTextSet = class (TRCmdTextCustom)
  protected
    procedure ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdStrReplace = class (TRCmdExecuted)
  private
    fFlags: TReplaceFlags;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdFileAppend = class (TRCmdExecuted)
  private
    fOem: Boolean;
    fLine: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdScriptSave = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdIniReadVariable = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdIniReadSections = class (TRCmdListCustom)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdIniReadKeys = class (TRCmdListCustom)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdRegReadVariable = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdRegReadSections = class (TRCmdListCustom)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdRegReadKeys = class (TRCmdListCustom)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecuteList(var List: TRListPosition; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdCalcCrc32 = class (TRCmdExecuted)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpOpen = class (TRCmdObjCustom)
  protected
    fPassive: Boolean;
    fBinary: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    constructor Create(Script: TRScriptCustom); override;
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdFtpCustom  = class (TRCmdObjCustom)
  private
    bIgnoreError: Boolean;
    bWarningError: Boolean;
  protected
    procedure SetAttributes(const Attributes: string); override;
    function  GetAttributes: string; override;
    procedure ExecuteCommand(var OpState: TRScriptState); override;
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); virtual; abstract;
  public
    constructor Create(Script: TRScriptCustom); override;
    function GetCommandAttrList: string; override;
    function GetCommandAttrNote(const Attr: Char): string; override;
  end;

  TRCmdFtpClose = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpCurrDir = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpChDir = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpMkDir = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpFrDir = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpRmDir = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpGet = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpPut = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdFtpDelete = class (TRCmdFtpCustom)
  protected
    procedure ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState); override;
  public
    class function GetParamsLimit: Integer; override;
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

const
  LRCommandsExe : array [cmInfo..cmShowVars] of CRCmdExecuted =
    (TRCmdShowInfo, TRCmdReturn, TRCmdBreak, TRCmdSetState, TRCmdPause, TRCmdWinWait, TRCmdCallProc,
     TRCmdChangeDir, TRCmdForceDirs, TRCmdCopyFile, TRCmdCopyMask, TRCmdMoveFile,
     TRCmdMoveMask, TRCmdRenameFile, TRCmdDelete, TRCmdUpdate,
     TRCmdExec, TRCmdExecEx, TRCmdExecAs, TRCmdOpen, TRCmdOpenEx,
     TRCmdCreateLink, TRCmdCreateLinkEx,
     TRCmdNetMap, TRCmdNetUnmap,
     TRCmdChangeIni, TRCmdIniReadVariable, TRCmdIniReadSections, TRCmdIniReadKeys,
     TRCmdChangeReg, TRCmdRegReadVariable, TRCmdRegReadSections, TRCmdRegReadKeys,
     TRCmdBdeAliasCreate, TRCmdBdeAliasDelete,
     TRCmdPrinterAdd, TRCmdPrinterDel, TRCmdNetPrinterAdd, TRCmdNetPrinterDel,
     TRCmdFreeObject, TRCmdFindDirs, TRCmdFindFiles,
     TRCmdListFirst, TRCmdListPrior, TRCmdListNext, TRCmdListLast, TRCmdFileAppend,
     TRCmdTextCreate, TRCmdTextOpen, TRCmdTextSave,
     TRCmdTextAddChars, TRCmdTextAddLine, TRCmdTextReplace, TRCmdTextSet,
     TRCmdSubStrings, TRCmdSubString, TRCmdStrReplace,
     TRCmdExtractFileName, TRCmdExtractFilePath, TRCmdExtractFileExt, TRCmdChangeFileExt, TRCmdCheckFileName,
     TRCmdExitWindows, TRCmdServiceStart, TRCmdServiceStop,
     TRCmdCalcCrc32, TRCmdScriptSave, TRCmdCheckVariable,
     TRCmdFtpOpen, TRCmdFtpCurrDir, TRCmdFtpChDir, TRCmdFtpMkDir, TRCmdFtpFrDir, TRCmdFtpRmDir, TRCmdFtpGet, TRCmdFtpPut, TRCmdFtpDelete, TRCmdFtpClose,
     TRCmdShowVariables);

implementation

uses
  Windows, IniFiles, Registry, RCrc32,
  RxStrUtils, RVclUtils, RDialogs, RVarListEx, RSysUtils, RTimeDialog;

{ TRCmdSetVariable }

class function TRCmdSetVariable.GetCommandName: string;
begin
  Result := tkSet;
end;

class function TRCmdSetVariable.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdSetVariable.GetCommandParams_Edit: string;
var
  VarName, VarValue: string;
begin
  StrToVariable(Parameters[0], VarName, VarValue);
  Result := ScriptQuotedStr(VarName) + VarsChar + ScriptQuotedStr(VarValue);
end;

function TRCmdSetVariable.GetCommandParams_Exec: string;
var
  VarName, VarValue: string;
begin
  StrToVariable(Parameters[0], VarName, VarValue);
  Result := ScriptQuotedStr(VarName) + VarsChar + ScriptQuotedStr(VarValue);
end;

function TRCmdSetVariable.GetCommandNote: string;
begin
  Result := SCmdNoteSet;
end;

procedure TRCmdSetVariable.ExecuteCommand(var OpState: TRScriptState);
var
  VarName, VarValue: string;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle, SExecuteSetTitle);
  StrToVariable(GetParameter(0), VarName, VarValue);
  Script.PutVariable(True, VarName, VarValue);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [VarName, VarValue]));
end;

{ TRCmdCheckVariable }

class function TRCmdCheckVariable.GetCommandName: string;
begin
  Result := tkChkVar;
end;

class function TRCmdCheckVariable.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdCheckVariable.GetCommandNote: string;
begin
  Result := SCmdNoteChkVar;
end;

procedure TRCmdCheckVariable.ExecuteCommand(var OpState: TRScriptState);
var
  VarName, VarValue: string;
begin
  VarName := GetParameter(0);
  VarValue := GetParameter(1);
  Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteChkVar, [VarName]));
  if Script.VarListExec.IndexOfName(VarName) = -1 then
  begin
    Script.PutVariable(True, VarName, VarValue);
    Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [VarName, VarValue]));
  end
  else Script.DoShowInfo(Script, Now, mcOperation, msOk, SExecuteVarExists);
end;

{ TRCmdShowVariables }

class function TRCmdShowVariables.GetCommandName: string;
begin
  Result := tkShowVariables;
end;

class function TRCmdShowVariables.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRCmdShowVariables.GetCommandNote: string;
begin
  Result := SCmdNoteShowVariables;
end;

procedure TRCmdShowVariables.ExecuteCommand(var OpState: TRScriptState);
var
  i, iCount: Integer;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle, SExecuteShowVarsTitle);
  Script.DoShowProgress(Script, mcTransaction, 0, Script.VarListExec.Count);
  iCount := Script.VarListExec.Count - 1;
  for i := 1 to iCount do
  begin
    Script.DoShowInfo(Script, Now, mcOperation, msOk, Script.VarListExec[i]);
    Script.DoShowProgress(Script, mcTransaction, i + 1, Script.VarListExec.Count);
  end;
  Script.DoShowInfo(Script, Now, mcCommand, msOk,
    Format(SExecuteShowVarsResult, [Script.VarListExec.Count]));
end;

{ TRCmdSubString }

class function TRCmdSubString.GetCommandName: string;
begin
  Result := tkSubString;
end;

function TRCmdSubString.GetCommandNote: string;
begin
  Result := SCmdSubString;
end;

class function TRCmdSubString.GetParamsLimit: Integer;
begin
  Result := 4;
end;

procedure TRCmdSubString.ExecuteCommand(var OpState: TRScriptState);
var
  VarName, VarValue, FullStr, DelimsStr: string;
  Delims: TSysCharSet;
  i, iCount, Index: Integer;
begin
  VarName := GetParameter(0);
  VarValue := EmptyStr;
  FullStr := GetParameter(1);
  DelimsStr := GetParameter(3);
  Delims := [];
  iCount := Length(DelimsStr);
  for i := 1 to iCount do
    Delims := Delims + [DelimsStr[i]];
  Index := StrToIntDef(GetParameter(2), 0);
  if (Index > 0) and (Index <= WordCount(FullStr, Delims)) then
    VarValue := Trim(ExtractWord(Index, FullStr, Delims));
  Script.PutVariable(True, VarName, VarValue);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [VarName, VarValue]));
end;

{ TRCmdSubStrings }

class function TRCmdSubStrings.GetCommandName: string;
begin
  Result := tkSubStrings;
end;

function TRCmdSubStrings.GetCommandNote: string;
begin
  Result := SCmdSubStrings;
end;

class function TRCmdSubStrings.GetParamsLimit: Integer;
begin
  Result := 3;
end;

procedure TRCmdSubStrings.ExecuteCommand(var OpState: TRScriptState);
var
  VarName, FullStr, DelimsStr: string;
  Delims: TSysCharSet;
  i, iCount, WordCnt: Integer;
begin
  VarName := GetParameter(0);
  FullStr := GetParameter(1);
  DelimsStr := GetParameter(2);
  Delims := [];
  iCount := Length(DelimsStr);
  for i := 1 to iCount do
    Delims := Delims + [DelimsStr[i]];
  WordCnt := WordCount(FullStr, Delims);
  Script.PutVariable(True, VarName, IntToStr(WordCnt));
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteSet, [VarName, IntToStr(WordCnt)]));
end;

{ TRCmdPause }

class function TRCmdPause.GetCommandName: string;
begin
  Result := tkPause;
end;

function TRCmdPause.GetCommandNote: string;
begin
  Result := SCmdNotePause;
end;

class function TRCmdPause.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdPause.ExecuteCommand(var OpState: TRScriptState);
var
  IsBreak: Boolean;
  PauseVal: Integer;
  PauseMsg: string;
begin
  IsBreak := False;
  PauseVal := StrToIntDef(GetParameter(0), 0);
  PauseMsg := Trim(GetParameter(1));
  if PauseMsg = EmptyStr then
    PauseMsg := Format(SExecutePause, [PauseVal]);
  Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecutePause, [PauseVal]));
  Script.DoShowInfo(Script, Now, mcTransaction, msTitle, PauseMsg);
  WaitTerminated(PauseVal, IsBreak, EmptyStr, nil, Script.DoCheckBreak);
  if IsBreak then
  begin
    OpState := stError;
    Script.BreakScript;
  end;
end;

{ TRCmdWinWait }

constructor TRCmdWinWait.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fWaitOpen := False;
  fWaitClose := False;
end;

class function TRCmdWinWait.GetCommandName: string;
begin
  Result := tkWinWait;
end;

function TRCmdWinWait.GetCommandNote: string;
begin
  Result := SCmdNoteWinWait;
end;

class function TRCmdWinWait.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdWinWait.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if fWaitOpen then Result := Result + chWaitOpen;
  if fWaitClose then Result := Result + chWaitClose;
end;

procedure TRCmdWinWait.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fWaitOpen := Pos(chWaitOpen, AnsiUpperCase(Attributes)) > 0;
  fWaitClose := Pos(chWaitClose, AnsiUpperCase(Attributes)) > 0;
end;

function TRCmdWinWait.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList + chWaitOpen + chWaitClose;
end;

function TRCmdWinWait.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = chWaitOpen then
      Result := SRCmdSwwFlagWaitOpen;
    if Attr = chWaitClose then
      Result := SRCmdSwwFlagWaitClose;
  end;
end;

procedure TRCmdWinWait.ExecuteCommand(var OpState: TRScriptState);
var
  IsBreak: Boolean;
  WinWaitName, WinWaitTitle: string;
begin
  IsBreak := False;
  Script.DoShowProgress(Script, mcTransaction, 0, 2);
  WinWaitName := Trim(GetParameter(0));
  // Ожидание создания окна
  if fWaitOpen then
  begin
    WinWaitTitle := Format(SExecuteWinWaitOpen, [WinWaitName]);
    Script.DoShowInfo(Script, Now, mcOperation, msTitle, WinWaitTitle);
    if Trim(GetParameter(1)) = EmptyStr
    then Script.DoShowInfo(Script, Now, mcTransaction, msTitle, WinWaitTitle)
    else Script.DoShowInfo(Script, Now, mcTransaction, msTitle, Trim(GetParameter(1)));
    while not IsBreak and (FindWindow(nil, PAnsiChar(WinWaitName)) = 0) do
    begin
      WaitTerminated(1, IsBreak, EmptyStr, nil, Script.DoCheckBreak);
      if IsBreak then
      begin
        OpState := stError;
        Script.BreakScript;
      end;
    end;
    if not IsBreak then
      Script.DoShowInfo(Script, Now, mcOperation, msOk,
        Format(SExecuteWinWaitOpenOk, [WinWaitName]));
  end;
  Script.DoShowProgress(Script, mcTransaction, 1, 2);
  // Ожидание закрытия окна
  if fWaitClose and not IsBreak then
  begin
    WinWaitTitle := Format(SExecuteWinWaitClose, [WinWaitName]);
    Script.DoShowInfo(Script, Now, mcOperation, msTitle, WinWaitTitle);
    if Trim(GetParameter(1)) = EmptyStr
    then Script.DoShowInfo(Script, Now, mcTransaction, msTitle, WinWaitTitle)
    else Script.DoShowInfo(Script, Now, mcTransaction, msTitle, Trim(GetParameter(1)));
    while not IsBreak and (FindWindow(nil, PAnsiChar(WinWaitName)) > 0) do
    begin
      WaitTerminated(1, IsBreak, EmptyStr, nil, Script.DoCheckBreak);
      if IsBreak then
      begin
        OpState := stError;
        Script.BreakScript;
      end;
    end;
    if not IsBreak then
      Script.DoShowInfo(Script, Now, mcOperation, msOk,
        Format(SExecuteWinWaitCloseOk, [WinWaitName]));
  end;
  Script.DoShowProgress(Script, mcTransaction, 2, 2);
end;

{ TRCmdBreak }

class function TRCmdBreak.GetCommandName: string;
begin
  Result := tkBreak;
end;

function TRCmdBreak.GetCommandNote: string;
begin
  Result := SCmdNoteBreak;
end;

class function TRCmdBreak.GetParamsLimit: Integer;
begin
  Result := 0;
end;

procedure TRCmdBreak.ExecuteCommand(var OpState: TRScriptState);
begin
  if Script.BreakBlock
  then Script.DoShowInfo(Script, Now, mcOperation, msOk, SExecuteBreakBlock)
  else begin
    OpState := stError;
    Script.DoShowInfo(Script, Now, mcOperation, msError, SExecuteBreakError);
  end;
end;

{ TRCmdReturn }

constructor TRCmdReturn.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fRetFlag := rfNoChange;
end;

class function TRCmdReturn.GetCommandName: string;
begin
  Result := tkReturn;
end;

class function TRCmdReturn.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRCmdReturn.GetCommandNote: string;
begin
  Result := SCmdNoteReturn;
end;

function TRCmdReturn.GetAttributes: string;
begin
  Result := inherited GetAttributes + SRCmdRetFlag[fRetFlag];
end;

procedure TRCmdReturn.SetAttributes(const Attributes: string);
var
  Attr: TRCmdRetFlag;
begin
  inherited SetAttributes(Attributes);
  for Attr := Low(TRCmdRetFlag) to High(TRCmdRetFlag) do
    if Pos(SRCmdRetFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
    begin
      fRetFlag := Attr;
      Break;
    end;
end;

function TRCmdReturn.GetCommandAttrList: string;
var
  Attr: TRCmdRetFlag;
begin
  Result := inherited GetCommandAttrList;
  for Attr := Low(TRCmdRetFlag) to High(TRCmdRetFlag) do
    Result := Result + SRCmdRetFlag[Attr];
end;

function TRCmdReturn.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: TRCmdRetFlag;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    for Cnt := Low(TRCmdRetFlag) to High(TRCmdRetFlag) do
      if AnsiUpperCase(Attr) = SRCmdRetFlag[Cnt] then
      begin
        Result := SRCmdRetFlagNote[Cnt];
        Break;
      end;
  end;
end;

procedure TRCmdReturn.ExecuteCommand(var OpState: TRScriptState);
begin
  case fRetFlag of
    rfOk: OpState := stOk;
    rfWarning: OpState := stWarning;
    rfError: OpState := stError;
  end;
  Script.BreakScript;
end;

{ TRCmdSetState }

constructor TRCmdSetState.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fSetState := stOk;
end;

class function TRCmdSetState.GetCommandName: string;
begin
  Result := tkSetState;
end;

class function TRCmdSetState.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRCmdSetState.GetCommandNote: string;
begin
  Result := SCmdNoteSetState;
end;

function TRCmdSetState.GetAttributes: string;
begin
  Result := SRCmdScriptState[fSetState];
end;

procedure TRCmdSetState.SetAttributes(const Attributes: string);
var
  Attr: TRScriptState;
begin
  for Attr := Low(TRScriptState) to High(TRScriptState) do
    if Pos(SRCmdScriptState[Attr], AnsiUpperCase(Attributes)) > 0 then
    begin
      fSetState := Attr;
      Break;
    end;
end;

function TRCmdSetState.GetCommandAttrList: string;
var
  Attr: TRScriptState;
begin
  Result := EmptyStr;
  for Attr := Low(TRScriptState) to High(TRScriptState) do
    Result := Result + SRCmdScriptState[Attr];
end;

function TRCmdSetState.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: TRScriptState;
begin
  for Cnt := Low(TRScriptState) to High(TRScriptState) do
    if AnsiUpperCase(Attr) = SRCmdScriptState[Cnt] then
    begin
      Result := SRCmdScriptStateNote[Cnt];
      Break;
    end;
end;

procedure TRCmdSetState.Execute(var State: TRScriptState);
begin
  if Script.Executed then
  begin
    Script.DoShowInfo(Script, Now, mcCommand, msTitle, GetCommandDescription);
    Script.DoShowInfo(Script, Now, mcCommand, msInfo, GetCommandText_Exec);
    Script.DoShowProgress(Script, mcTransaction, 0, 1);
    Script.DoShowInfo(Script, Now, mcTransaction, msTitle,
      Format(SExecuteSetState, [SRProcedureState[fSetState]]));
    State := fSetState;
    Script.DoShowInfo(Script, Now, mcOperation,
      Script.ScriptStateToMsgState(fSetState),
      Format(SExecuteSetState, [SRProcedureState[fSetState]]));
    Script.DoShowProgress(Script, mcTransaction, 1, 1);
    if State in [stError, stErrorStop, stErrorRestore, stErrorUndo] then
      Script.HandleError(State);
  end;
end;

{ TRCmdCallProc }

class function TRCmdCallProc.GetCommandName: string;
begin
  Result := tkCall;
end;

class function TRCmdCallProc.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdCallProc.GetCommandNote: string;
begin
  Result := SCmdNoteCall;
end;

function TRCmdCallProc.GetCommandCount: Integer;
begin
  Result := 0;
end;

procedure TRCmdCallProc.ExecuteCommand(var OpState: TRScriptState);
begin
  OpState := Script.ExecuteProc(GetParameter(0), False, True, GetParameter(1), OpState);
end;

procedure TRCmdCallProc.ExecOnExtProcessor(var OpState: TRScriptState);
var
  FullProcName, ShrtProcName: string;
  ProcVarList: TStringList;
  Proc: TRCmdProc;
begin
  FullProcName := Script.GetFullProcName(Script.Alias, GetParameter(0));
  ShrtProcName := Script.GetShortProcName(GetParameter(0));
  Script.DoShowInfo(Script, Now, mcCommand, msTitle,
    GetCommandDescription);
  Script.DoShowInfo(Script, Now, mcCommand, msInfo,
    GetCommandText_Exec);
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteCreateScript, [GetParameter(0)]));
  Proc := Script.FindProc(FullProcName);
  if not Assigned(Proc) and Script.IsExternalProc(FullProcName) then
    Proc := Script.FindExternalProc(FullProcName);
  if Assigned(Proc) then
  begin
    try
      ProcVarList := TStringList.Create;
      try
        ProcVarList.Assign(Script.VarListExec);
        DelStandartVariables(ProcVarList, True, False);
        AddVariableList(ProcVarList, GetParameter(1));
        Script.DoExecCmdOnEP(Script, ctScript,
          tkVars + #13
            + ProcVarList.Text + #13#13
            + tkProc1 + #32 + ShrtProcName + #13
            + tkBlockBegin1 + #13
            + Proc.GetCommandsCode + #13
            + tkBlockEnd1,
          Script.GetScriptName, Script.Alias, ShrtProcName,
          Script.DoShowInfo, Script.DoShowProgress, Script.DoFileProgress, OpState);
      finally
        ProcVarList.Free;
      end;
    except
      on E: Exception do
      begin
        OpState := stError;
        Script.DoShowInfo(Script, Now, mcOperation, msError,
          Format(EScriptProcErrorExec, [GetParameter(0), E.Message]));
      end;
    end;
  end
  else begin
    OpState := stError;
    Script.DoShowInfo(Script, Now, mcOperation, msError,
      Format(EScriptProcNotFound, [GetParameter(0)]));
  end;
end;

{ TRCmdShowInfo }

constructor TRCmdShowInfo.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fBoxType := mbInfo;
end;

class function TRCmdShowInfo.GetParamsLimit: Integer;
begin
  Result := -1;
end;

class function TRCmdShowInfo.GetCommandName: string;
begin
  Result := tkInfo;
end;

function TRCmdShowInfo.GetCommandNote: string;
begin
  Result := SCmdNoteInfo;
end;

function TRCmdShowInfo.GetAttributes: string;
begin
  Result := inherited GetAttributes +
    SRMsgBoxType[fBoxType];
end;

procedure TRCmdShowInfo.SetAttributes(const Attributes: string);
var
  Attr: TRMsgBoxType;
begin
  inherited SetAttributes(Attributes);
  for Attr := Low(TRMsgBoxType) to High(TRMsgBoxType) do
    if Pos(SRMsgBoxType[Attr], AnsiUpperCase(Attributes)) > 0 then
    begin
      fBoxType := Attr;
      Break;
    end;
end;

function TRCmdShowInfo.GetCommandAttrList: string;
var
  Attr: TRMsgBoxType;
begin
  Result := inherited GetCommandAttrList;
  for Attr := Low(TRMsgBoxType) to High(TRMsgBoxType) do
    Result := Result + SRMsgBoxType[Attr];
end;

function TRCmdShowInfo.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: TRMsgBoxType;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    for Cnt := Low(TRMsgBoxType) to High(TRMsgBoxType) do
      if AnsiUpperCase(Attr) = SRMsgBoxType[Cnt] then
      begin
        Result := SRMsgBoxTypeNote[Cnt];
        Break;
      end;
  end;
end;

procedure TRCmdShowInfo.ExecuteCommand(var OpState: TRScriptState);

  function CreateMessage: string;
  var
    i: Integer;
  begin
    for i := Low(Parameters) to High(Parameters) do
      Result := Result + #13 + GetParameter(i);
    if Result <> EmptyStr then Delete(Result, 1, 1);
  end;

  function CreateLogMessage: string;
  var
    i: Integer;
  begin
    for i := Low(Parameters) to High(Parameters) do
      Result := Result + #32 + GetParameter(i);
    if Result <> EmptyStr then Delete(Result, 1, 1);
    Result := Format(SExecuteShowMessage, [Result]);
  end;

begin
  Script.DoShowInfo(Script, Now, mcTransaction, msTitle, CreateLogMessage);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, CreateLogMessage);
  if Script.GuiEnabled then
  begin
    case fBoxType of
      mbWarning: ShowInfoBox(dmWarning, CreateMessage, 60);
      mbError: ShowInfoBox(dmError, CreateMessage, 60);
      else ShowInfoBox(dmInfo, CreateMessage, 60);
    end;
  end;
end;

{ TRCmdRfoCustom }

constructor TRCmdRfoCustom.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fErrOptions := TRErrorsInversed;
end;

function TRCmdRfoCustom.GetAttributes: string;
var
  Attr: TRErrorsOption;
begin
  Result := inherited GetAttributes;
  for Attr := Low(TRErrorsOption) to High(TRErrorsOption) do
    if Attr in TRErrorsInversed then
    begin
      if not (Attr in fErrOptions) then
        Result := Result + SRCmdErrFlag[Attr];
    end
    else begin
      if Attr in fErrOptions then
        Result := Result + SRCmdErrFlag[Attr];
    end;
end;

procedure TRCmdRfoCustom.SetAttributes(const Attributes: string);
var
  Attr: TRErrorsOption;
begin
  inherited SetAttributes(Attributes);
  for Attr := Low(TRErrorsOption) to High(TRErrorsOption) do
    if Attr in TRErrorsInversed then
    begin
      if Pos(SRCmdErrFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
        fErrOptions := fErrOptions - [Attr];
    end
    else begin
      if Pos(SRCmdErrFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
        fErrOptions := fErrOptions + [Attr];
    end;
end;

function TRCmdRfoCustom.GetCommandAttrList: string;
var
  Attr: TRErrorsOption;
begin
  Result := inherited GetCommandAttrList;
  for Attr := Low(TRErrorsOption) to High(TRErrorsOption) do
    Result := Result + SRCmdErrFlag[Attr];
end;

function TRCmdRfoCustom.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: TRErrorsOption;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    for Cnt := Low(TRErrorsOption) to High(TRErrorsOption) do
      if AnsiUpperCase(Attr) = SRCmdErrFlag[Cnt] then
      begin
        Result := SRCmdErrFlagNote[Cnt];
        Break;
      end;
  end;
end;

procedure TRCmdRfoCustom.ExecuteCommand(var OpState: TRScriptState);
var
  OpResult: TROperation;
begin
  SetLength(OpResult.Details, 0);
  try
    OpResult := ExecuteRfo;
  finally
    case OpResult.State of
      msOk, msIgnored: OpState := stOk;
      msWarning: OpState := stWarning;
      msError: OpState := stError;
      msBreak: OpState := stErrorStop;
    end;
    SetLength(OpResult.Details, 0);
  end;
end;

function TRCmdRfoCustom.GetErrorOptions: TRErrorsOptions;
begin
  if (esErrorShow in fErrOptions) and not Script.GuiEnabled
  then Result := fErrOptions - [esErrorShow]
  else Result := fErrOptions;
end;

{ TRCmdRfoMulti }

constructor TRCmdRfoMulti.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fEmptyFlag := ezWarning;
end;

function TRCmdRfoMulti.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  case fEmptyFlag of
    ezOk: Result := Result + SRCmdEmptyOk;
    ezError: Result := Result + SRCmdEmptyError;
  end;
end;

procedure TRCmdRfoMulti.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  if Pos(SRCmdEmptyOk, AnsiUpperCase(Attributes)) > 0
  then fEmptyFlag := ezOk
  else if Pos(SRCmdEmptyError, AnsiUpperCase(Attributes)) > 0
       then fEmptyFlag := ezError
       else fEmptyFlag := ezWarning;
end;

function TRCmdRfoMulti.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList + SRCmdEmptyOk + SRCmdEmptyError;
end;

function TRCmdRfoMulti.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if AnsiUpperCase(Attr) = SRCmdEmptyOk then
      Result := SRCmdRfoNoteEmptyOk;
    if AnsiUpperCase(Attr) = SRCmdEmptyError then
      Result := SRCmdRfoNoteEmptyError;
  end;
end;

{ TRCmdRfoFiles }

constructor TRCmdRfoFiles.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fFileFlags := TRFileFlagsInversed;
end;

function TRCmdRfoFiles.GetAttributes: string;
var
  Attr: TRfoFileFlag;
begin
  Result := inherited GetAttributes;
  for Attr := Low(TRfoFileFlag) to High(TRfoFileFlag) do
    if Attr in TRFileFlagsInversed then
    begin
      if not (Attr in fFileFlags) then
        Result := Result + SRCmdRfoFlag[Attr];
    end
    else begin
      if Attr in fFileFlags then
        Result := Result + SRCmdRfoFlag[Attr];
    end;
end;

procedure TRCmdRfoFiles.SetAttributes(const Attributes: string);
var
  Attr: TRfoFileFlag;
begin
  inherited SetAttributes(Attributes);
  for Attr := Low(TRfoFileFlag) to High(TRfoFileFlag) do
    if Attr in TRFileFlagsInversed then
    begin
      if Pos(SRCmdRfoFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
        fFileFlags := fFileFlags - [Attr];
    end
    else begin
      if Pos(SRCmdRfoFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
        fFileFlags := fFileFlags + [Attr];
    end;
end;

function TRCmdRfoFiles.GetCommandAttrList: string;
var
  Attr: TRfoFileFlag;
begin
  Result := inherited GetCommandAttrList;
  for Attr := Low(TRfoFileFlag) to High(TRfoFileFlag) do
    Result := Result + SRCmdRfoFlag[Attr];
end;

function TRCmdRfoFiles.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: TRfoFileFlag;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    for Cnt := Low(TRfoFileFlag) to High(TRfoFileFlag) do
      if AnsiUpperCase(Attr) = SRCmdRfoFlag[Cnt] then
      begin
        Result := SRCmdRfoFlagNote[Cnt];
        Break;
      end;
  end;
end;

{ TRCmdForceDirs }

class function TRCmdForceDirs.GetCommandName: string;
begin
  Result := tkForceDirs;
end;

function TRCmdForceDirs.GetCommandNote: string;
begin
  Result := SCmdNoteFDirs;
end;

class function TRCmdForceDirs.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdForceDirs.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.ForceDirs(GetParameter(0), GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRCmdChangeDirs }

class function TRCmdChangeDir.GetCommandName: string;
begin
  Result := tkChangeDir;
end;

function TRCmdChangeDir.GetCommandNote: string;
begin
  Result := SCmdNoteChangeDir;
end;

class function TRCmdChangeDir.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdChangeDir.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.ChangeDir(GetParameter(0), GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRCmdCopyMask }

class function TRCmdCopyMask.GetCommandName: string;
begin
  Result := tkMCopy;
end;

function TRCmdCopyMask.GetCommandNote: string;
begin
  Result := SCmdNoteMCopy;
end;

class function TRCmdCopyMask.GetParamsLimit: Integer;
begin
  Result := 3;
end;

function TRCmdCopyMask.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.CopyFilesMask(GetParameter(0), GetParameter(1), GetParameter(2),
    False, fFileFlags, fEmptyFlag, GetErrorOptions, Script.RestoreList,
    Script.BufferSizeKb, Script.NetUsage, Script.TryMax, Script.TryDelay,
    Script.DoCalcNetUsage, Script.DoShowInfo, Script.DoShowProgress,
    Script.DoFileProgress, Script.DoCheckBreak);
end;

{ TRCmdCopyFile }

class function TRCmdCopyFile.GetCommandName: string;
begin
  Result := tkFCopy;
end;

function TRCmdCopyFile.GetCommandNote: string;
begin
  Result := SCmdNoteFCopy;
end;

class function TRCmdCopyFile.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdCopyFile.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.CopyFiles(GetParameter(0), GetParameter(1), False,
    fFileFlags, fEmptyFlag, GetErrorOptions, Script.RestoreList,
    Script.BufferSizeKb, Script.NetUsage, Script.TryMax, Script.TryDelay,
    Script.DoCalcNetUsage, Script.DoShowInfo, Script.DoShowProgress,
    Script.DoFileProgress, Script.DoCheckBreak);
end;

{ TRCmdMoveMask }

class function TRCmdMoveMask.GetCommandName: string;
begin
  Result := tkMMove;
end;

function TRCmdMoveMask.GetCommandNote: string;
begin
  Result := SCmdNoteMMove;
end;

class function TRCmdMoveMask.GetParamsLimit: Integer;
begin
  Result := 3;
end;

function TRCmdMoveMask.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.CopyFilesMask(GetParameter(0), GetParameter(1), GetParameter(2),
    True, fFileFlags, fEmptyFlag, GetErrorOptions, Script.RestoreList,
    Script.BufferSizeKb, Script.NetUsage, Script.TryMax, Script.TryDelay,
    Script.DoCalcNetUsage, Script.DoShowInfo, Script.DoShowProgress,
    Script.DoFileProgress, Script.DoCheckBreak);
end;

{ TRCmdMoveFile }

class function TRCmdMoveFile.GetCommandName: string;
begin
  Result := tkFMove;
end;

function TRCmdMoveFile.GetCommandNote: string;
begin
  Result := SCmdNoteFMove;
end;

class function TRCmdMoveFile.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdMoveFile.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.CopyFiles(GetParameter(0), GetParameter(1), True,
    fFileFlags, fEmptyFlag, GetErrorOptions, Script.RestoreList,
    Script.BufferSizeKb, Script.NetUsage, Script.TryMax, Script.TryDelay,
    Script.DoCalcNetUsage, Script.DoShowInfo, Script.DoShowProgress,
    Script.DoFileProgress, Script.DoCheckBreak);
end;

{ TRCmdUpdate }

class function TRCmdUpdate.GetCommandName: string;
begin
  Result := tkUpdate;
end;

function TRCmdUpdate.GetCommandNote: string;
begin
  Result := SCmdNoteUpdate;
end;

class function TRCmdUpdate.GetParamsLimit: Integer;
begin
  Result := 3;
end;

function TRCmdUpdate.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.UpdateFiles(GetParameter(0), GetParameter(1), GetParameter(2),
    fFileFlags, fEmptyFlag, GetErrorOptions, Script.RestoreList,
    Script.BufferSizeKb, Script.NetUsage, Script.TryMax, Script.TryDelay,
    Script.DoCalcNetUsage, Script.DoShowInfo, Script.DoShowProgress,
    Script.DoFileProgress, Script.DoCheckBreak);
end;

function TRCmdUpdate.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Attr = SRCmdRfoFlag[rfDstFixed] then
    Result := SRCmdRfoNoteDstFixedUpdate;
  if (Attr = SRCmdRfoFlag[rfForceDirs])
  or (Attr = SRCmdRfoFlag[rfDstDirAuto]) then
    Result := SRCmdRfoNoteEmpty;
end;

{ TRCmdRenameFile }

class function TRCmdRenameFile.GetCommandName: string;
begin
  Result := tkRename;
end;

function TRCmdRenameFile.GetCommandNote: string;
begin
  Result := SCmdNoteRename;
end;

class function TRCmdRenameFile.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdRenameFile.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.RenameFile(GetParameter(0), GetParameter(1), GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRCmdDelete }

class function TRCmdDelete.GetCommandName: string;
begin
  Result := tkDelete;
end;

function TRCmdDelete.GetCommandNote: string;
begin
  Result := SCmdNoteDelete;
end;

class function TRCmdDelete.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdDelete.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.DeleteMask(GetParameter(0), fFileFlags,
    fEmptyFlag, GetErrorOptions, Script.TryMax, Script.TryDelay,
    Script.DoShowInfo, Script.DoShowProgress, Script.DoCheckBreak);
end;

function TRCmdDelete.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if (Attr = SRCmdRfoFlag[rfDstDirAuto]) then
    Result := SRCmdRfoNoteDstDirAutoDelete;
  if Attr = SRCmdRfoFlag[rfDstFixed] then
    Result := SRCmdRfoNoteDstFixedDelete;
  if (Attr = SRCmdRfoFlag[rfForceDirs])
  or (Attr = SRCmdRfoFlag[rfOverWrite])
  or (Attr = SRCmdRfoFlag[rfCopyBackup])
  or (Attr = SRCmdRfoFlag[rfCopyLocked])
  or (Attr = SRCmdRfoFlag[rfCheckCopy])
  or (Attr = SRCmdRfoFlag[rfCopyFileDate])
  or (Attr = SRCmdRfoFlag[rfCopyFileAttr])
  or (Attr = SRCmdRfoFlag[rfCompareDate])
  or (Attr = SRCmdRfoFlag[rfCompareSize])
  or (Attr = SRCmdRfoFlag[rfCompareCrc32])
  or (Attr = SRCmdRfoFlag[rfCopyList]) then
    Result := SRCmdRfoNoteEmpty;
end;

{ TRCmdRfoExec }

constructor TRCmdRfoExec.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fShowWindow := SW_SHOWNORMAL;
  fWaitExit := True;
  fCheckOk := False;
end;

function TRCmdRfoExec.GetAttributes: string;
begin
  Result := inherited GetAttributes + SRCmdSwwFlag[fShowWindow];
  if fWaitExit then Result := Result + chWaitExit;
  if fCheckOk then Result := Result + chCheckOk;
end;

procedure TRCmdRfoExec.SetAttributes(const Attributes: string);
var
  Attr: Integer;
begin
  inherited SetAttributes(Attributes);
  fWaitExit := Pos(chWaitExit, AnsiUpperCase(Attributes)) > 0;
  fCheckOk := Pos(chCheckOk, AnsiUpperCase(Attributes)) > 0;
  for Attr := Low(SRCmdSwwFlag) to High(SRCmdSwwFlag) do
  begin
    if Pos(SRCmdSwwFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
      fShowWindow := Attr;
  end;
end;

function TRCmdRfoExec.GetCommandAttrList: string;
var
  Attr: Integer;
begin
  Result := inherited GetCommandAttrList + chWaitExit + chCheckOk;
  for Attr := Low(SRCmdSwwFlag) to High(SRCmdSwwFlag) do
    Result := Result + SRCmdSwwFlag[Attr];
end;

function TRCmdRfoExec.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: Integer;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = chWaitExit then
      Result := SRCmdSwwFlagWaitExit
    else begin
      if Attr = chCheckOk then
        Result := SRCmdSwwFlagCheckOk
      else begin
        for Cnt := Low(SRCmdSwwFlag) to High(SRCmdSwwFlag) do
          if AnsiUpperCase(Attr) = SRCmdSwwFlag[Cnt] then
          begin
            Result := SRCmdSwwFlagNote[Cnt];
            Break;
          end;
      end;
    end;
  end;
end;

{ TRCmdExec }

class function TRCmdExec.GetCommandName: string;
begin
  Result := tkExec;
end;

function TRCmdExec.GetCommandNote: string;
begin
  Result := SCmdNoteExec;
end;

class function TRCmdExec.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdExec.ExecuteRfo: TROperation;
var
  ExitCode: Integer;
begin
  Result := RFileProcs.ExecuteFile(GetParameter(0), GetParameter(1),
    fShowWindow, fCheckOk, fWaitExit, Script.ExtCmdWaitTime, ExitCode, EmptyStr, EmptyStr, GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
  Script.PutVariable(True, varExitCode, IntToStr(ExitCode));
end;

{ TRCmdExecEx }

class function TRCmdExecEx.GetCommandName: string;
begin
  Result := tkExecEx;
end;

function TRCmdExecEx.GetCommandNote: string;
begin
  Result := SCmdNoteExecEx;
end;

class function TRCmdExecEx.GetParamsLimit: Integer;
begin
  Result := 4;
end;

function TRCmdExecEx.ExecuteRfo: TROperation;
var
  ExitCode: Integer;
begin
  Result := RFileProcs.ExecuteFile(GetParameter(0), GetParameter(1),
    fShowWindow, fCheckOk, fWaitExit, Script.ExtCmdWaitTime, ExitCode,
    GetParameter(2), GetParameter(3), GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
  Script.PutVariable(True, varExitCode, IntToStr(ExitCode));
end;

{ TRCmdExecAs }

class function TRCmdExecAs.GetCommandName: string;
begin
  Result := tkExecAs;
end;

function TRCmdExecAs.GetCommandNote: string;
begin
  Result := SCmdNoteExecAs;
end;

class function TRCmdExecAs.GetParamsLimit: Integer;
begin
  Result := 5;
end;

function TRCmdExecAs.ExecuteRfo: TROperation;
var
  ExitCode: Integer;
begin
  Result := RFileProcs.ExecuteFileAs(GetParameter(0), GetParameter(1),
    GetParameter(2), GetParameter(3), GetParameter(4),
    fShowWindow, fCheckOk, fWaitExit, Script.ExtCmdWaitTime, ExitCode,
    EmptyStr, EmptyStr, GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
  Script.PutVariable(True, varExitCode, IntToStr(ExitCode));
end;

function TRCmdExecAs.GetCommandAttrNote(const Attr: Char): string;
begin
  if AnsiUpperCase(Attr) = SRCmdErrFlag[esErrorStop]
  then Result := SRCmdErrFlagNoteErrorStop1
  else Result := inherited GetCommandAttrNote(Attr);
end;

{ TRCmdOpen }

class function TRCmdOpen.GetCommandName: string;
begin
  Result := tkOpen;
end;

function TRCmdOpen.GetCommandNote: string;
begin
  Result := SCmdNoteOpen;
end;

class function TRCmdOpen.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdOpen.ExecuteRfo: TROperation;
var
  ExitCode: Integer;
begin
  Result := RFileProcs.OpenFile(tkOpen, GetParameter(0), GetParameter(1),
    fShowWindow, fCheckOk, fWaitExit, Script.ExtCmdWaitTime, ExitCode,
    EmptyStr, EmptyStr, GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
  Script.PutVariable(True, varExitCode, IntToStr(ExitCode));
end;

{ TRCmdOpenEx }

class function TRCmdOpenEx.GetCommandName: string;
begin
  Result := tkOpenEx;
end;

function TRCmdOpenEx.GetCommandNote: string;
begin
  Result := SCmdNoteOpenEx;
end;

class function TRCmdOpenEx.GetParamsLimit: Integer;
begin
  Result := 4;
end;

function TRCmdOpenEx.ExecuteRfo: TROperation;
var
  ExitCode: Integer;
begin
  Result := RFileProcs.OpenFile(tkOpenEx, GetParameter(0), GetParameter(1),
    fShowWindow, fCheckOk, fWaitExit, Script.ExtCmdWaitTime, ExitCode,
    GetParameter(2), GetParameter(3), GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
  Script.PutVariable(True, varExitCode, IntToStr(ExitCode));
end;

{ TRCmdCreateLink }

constructor TRCmdCreateLink.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fForceDirs := False;
end;

class function TRCmdCreateLink.GetCommandName: string;
begin
  Result := tkLink;
end;

function TRCmdCreateLink.GetCommandNote: string;
begin
  Result := SCmdNoteLink;
end;

class function TRCmdCreateLink.GetParamsLimit: Integer;
begin
  Result := 3;
end;

function TRCmdCreateLink.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if fForceDirs then
    Result := Result + SRCmdForceDirs;
end;

procedure TRCmdCreateLink.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fForceDirs := Pos(SRCmdForceDirs, AnsiUpperCase(Attributes)) > 0;
end;

function TRCmdCreateLink.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList + SRCmdForceDirs;
end;

function TRCmdCreateLink.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = SRCmdForceDirs then
      Result := SRCmdRfoNoteForceDirs;
  end;
end;

function TRCmdCreateLink.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.CreateShortcut(GetParameter(0), GetParameter(1),
    EmptyStr, GetParameter(2), fForceDirs, GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdCreateLinkEx }

constructor TRCmdCreateLinkEx.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fForceDirs := False;
end;

class function TRCmdCreateLinkEx.GetCommandName: string;
begin
  Result := tkLinkEx;
end;

function TRCmdCreateLinkEx.GetCommandNote: string;
begin
  Result := SCmdNoteLinkEx;
end;

class function TRCmdCreateLinkEx.GetParamsLimit: Integer;
begin
  Result := 4;
end;

function TRCmdCreateLinkEx.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if fForceDirs then
    Result := Result + SRCmdForceDirs;
end;

procedure TRCmdCreateLinkEx.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fForceDirs := Pos(SRCmdForceDirs, AnsiUpperCase(Attributes)) > 0;
end;

function TRCmdCreateLinkEx.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList + SRCmdForceDirs;
end;

function TRCmdCreateLinkEx.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = SRCmdForceDirs then
      Result := SRCmdRfoNoteForceDirs;
  end;
end;

function TRCmdCreateLinkEx.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.CreateShortcut(GetParameter(0), GetParameter(1),
    GetParameter(2), GetParameter(3), fForceDirs, GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdNetMap }

constructor TRCmdNetMap.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fTypeRes := RESOURCETYPE_DISK;
  fSaveProfile := True;
end;

class function TRCmdNetMap.GetCommandName: string;
begin
  Result := tkNetMap;
end;

function TRCmdNetMap.GetCommandNote: string;
begin
  Result := SCmdNoteNetMap;
end;

class function TRCmdNetMap.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdNetMap.GetAttributes: string;
begin
  Result := inherited GetAttributes + SRCmdNetMap[fTypeRes];
  if fSaveProfile then Result := Result + chSaveProfile;
end;

procedure TRCmdNetMap.SetAttributes(const Attributes: string);
var
  Attr: Cardinal;
begin
  inherited SetAttributes(Attributes);
  fSaveProfile := Pos(chSaveProfile, AnsiUpperCase(Attributes)) > 0;
  for Attr := Low(SRCmdNetMap) to High(SRCmdNetMap) do
  begin
    if Pos(SRCmdNetMap[Attr], AnsiUpperCase(Attributes)) > 0 then
      fTypeRes := Attr;
  end;
end;

function TRCmdNetMap.GetCommandAttrList: string;
var
  Attr: Cardinal;
begin
  Result := inherited GetCommandAttrList + chSaveProfile;
  for Attr := Low(SRCmdNetMap) to High(SRCmdNetMap) do
    Result := Result + SRCmdNetMap[Attr];
end;

function TRCmdNetMap.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: Cardinal;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = chSaveProfile then
      Result := SRCmdNetMapNoteSaveProfile
    else begin
      for Cnt := Low(SRCmdNetMap) to High(SRCmdNetMap) do
        if AnsiUpperCase(Attr) = SRCmdNetMap[Cnt] then
        begin
          Result := SRCmdNetMapNote[Cnt];
          Break;
        end;
    end;
  end;
end;

function TRCmdNetMap.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.NetworkMap(fTypeRes, GetParameter(0), GetParameter(1),
    EmptyStr, EmptyStr, fSaveProfile, GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdNetUnmap }

constructor TRCmdNetUnmap.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fForceDisconnect := False;
  fSaveProfile := True;
end;

class function TRCmdNetUnmap.GetCommandName: string;
begin
  Result := tkNetUnmap;
end;

function TRCmdNetUnmap.GetCommandNote: string;
begin
  Result := SCmdNoteNetUnmap;
end;

class function TRCmdNetUnmap.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdNetUnmap.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if fForceDisconnect then Result := Result + chForceUnmap;
  if fSaveProfile then Result := Result + chSaveProfile;
end;

procedure TRCmdNetUnmap.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fSaveProfile := Pos(chSaveProfile, AnsiUpperCase(Attributes)) > 0;
  fForceDisconnect := Pos(chForceUnmap, AnsiUpperCase(Attributes)) > 0;
end;

function TRCmdNetUnmap.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList + chSaveProfile + chForceUnmap;
end;

function TRCmdNetUnmap.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = chSaveProfile then Result := SRCmdNetMapNoteSaveProfile;
    if Attr = chForceUnmap then Result := SRCmdNetMapNoteForceUnmap;
  end;
end;

function TRCmdNetUnmap.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.NetworkUnmap(GetParameter(0), fForceDisconnect,
    fSaveProfile, GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdChangeIni }

constructor TRCmdChangeIni.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fIniFlags := TRIniFlagsInversed;
end;

class function TRCmdChangeIni.GetCommandName: string;
begin
  Result := tkChIni;
end;

function TRCmdChangeIni.GetCommandNote: string;
begin
  Result := SCmdNoteChIni;
end;

class function TRCmdChangeIni.GetParamsLimit: Integer;
begin
  Result := 4;
end;

function TRCmdChangeIni.GetAttributes: string;
var
  Attr: TRfoIniFlag;
begin
  Result := inherited GetAttributes;
  for Attr := Low(TRfoIniFlag) to High(TRfoIniFlag) do
    if Attr in TRIniFlagsInversed then
    begin
      if not (Attr in fIniFlags) then
        Result := Result + SRCmdIniFlag[Attr];
    end
    else begin
      if Attr in fIniFlags then
        Result := Result + SRCmdIniFlag[Attr];
    end;
end;

procedure TRCmdChangeIni.SetAttributes(const Attributes: string);
var
  Attr: TRfoIniFlag;
begin
  inherited SetAttributes(Attributes);
  for Attr := Low(TRfoIniFlag) to High(TRfoIniFlag) do
    if Attr in TRIniFlagsInversed then
    begin
      if Pos(SRCmdIniFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
        fIniFlags := fIniFlags - [Attr];
    end
    else begin
      if Pos(SRCmdIniFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
        fIniFlags := fIniFlags + [Attr];
    end;
end;

function TRCmdChangeIni.GetCommandAttrList: string;
var
  Attr: TRfoIniFlag;
begin
  Result := inherited GetCommandAttrList;
  for Attr := Low(TRfoIniFlag) to High(TRfoIniFlag) do
    Result := Result + SRCmdIniFlag[Attr];
end;

function TRCmdChangeIni.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: TRfoIniFlag;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    for Cnt := Low(TRfoIniFlag) to High(TRfoIniFlag) do
      if AnsiUpperCase(Attr) = SRCmdIniFlag[Cnt] then
      begin
        Result := SRCmdIniFlagNote[Cnt];
        Break;
      end;
  end;
end;

function TRCmdChangeIni.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.ChangeIniFiles(GetParameter(0), GetParameter(1),
    GetParameter(2), GetParameter(3), fIniFlags, fEmptyFlag, GetErrorOptions,
      Script.DoShowInfo, Script.DoShowProgress, Script.DoCheckBreak);
end;

{ TRCmdChangeReg }

constructor TRCmdChangeReg.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fRegFlags := TRRegFlagsInversed;
end;

class function TRCmdChangeReg.GetCommandName: string;
begin
  Result := tkChReg;
end;

function TRCmdChangeReg.GetCommandNote: string;
begin
  Result := SCmdNoteChReg;
end;

class function TRCmdChangeReg.GetParamsLimit: Integer;
begin
  Result := 5;
end;

function TRCmdChangeReg.GetAttributes: string;
var
  Attr: TRfoRegFlag;
begin
  Result := inherited GetAttributes;
  for Attr := Low(TRfoRegFlag) to High(TRfoRegFlag) do
    if Attr in TRRegFlagsInversed then
    begin
      if not (Attr in fRegFlags) then
        Result := Result + SRCmdRegFlag[Attr];
    end
    else begin
      if Attr in fRegFlags then
        Result := Result + SRCmdRegFlag[Attr];
    end;
end;

procedure TRCmdChangeReg.SetAttributes(const Attributes: string);
var
  Attr: TRfoRegFlag;
begin
  inherited SetAttributes(Attributes);
  for Attr := Low(TRfoRegFlag) to High(TRfoRegFlag) do
    if Attr in TRRegFlagsInversed then
    begin
      if Pos(SRCmdRegFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
        fRegFlags := fRegFlags - [Attr];
    end
    else begin
      if Pos(SRCmdRegFlag[Attr], AnsiUpperCase(Attributes)) > 0 then
        fRegFlags := fRegFlags + [Attr];
    end;
end;

function TRCmdChangeReg.GetCommandAttrList: string;
var
  Attr: TRfoRegFlag;
begin
  Result := inherited GetCommandAttrList;
  for Attr := Low(TRfoRegFlag) to High(TRfoRegFlag) do
    Result := Result + SRCmdRegFlag[Attr];
end;

function TRCmdChangeReg.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: TRfoRegFlag;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    for Cnt := Low(TRfoRegFlag) to High(TRfoRegFlag) do
      if AnsiUpperCase(Attr) = SRCmdRegFlag[Cnt] then
      begin
        Result := SRCmdRegFlagNote[Cnt];
        Break;
      end;
  end;
end;

function TRCmdChangeReg.ExecuteRfo: TROperation;
begin
  Result := RFileProcs.ChangeRegistry(GetParameter(0), GetParameter(1),
    GetParameter(2), GetParameter(3), GetParameter(4), fRegFlags, GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRCmdBdeAliasCreate }

class function TRCmdBdeAliasCreate.GetCommandName: string;
begin
  Result := tkBdeAliasCreate;
end;

function TRCmdBdeAliasCreate.GetCommandNote: string;
begin
  Result := SCmdNoteBdeAliasCreate;
end;

class function TRCmdBdeAliasCreate.GetParamsLimit: Integer;
begin
  Result := 3;
end;

function TRCmdBdeAliasCreate.ExecuteRfo: TROperation;
begin
  Result := BdeAliasCreate(GetParameter(0), GetParameter(1), GetParameter(2),
    GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdBdeAliasDelete }

class function TRCmdBdeAliasDelete.GetCommandName: string;
begin
  Result := tkBdeAliasDelete;
end;

function TRCmdBdeAliasDelete.GetCommandNote: string;
begin
  Result := SCmdNoteBdeAliasDelete;
end;

class function TRCmdBdeAliasDelete.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdBdeAliasDelete.ExecuteRfo: TROperation;
begin
  Result := BdeAliasDelete(GetParameter(0), GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdPrinterAdd }

class function TRCmdPrinterAdd.GetCommandName: string;
begin
  Result := tkAddPrinter;
end;

function TRCmdPrinterAdd.GetCommandNote: string;
begin
  Result := SCmdNoteAddPrinter;
end;

class function TRCmdPrinterAdd.GetParamsLimit: Integer;
begin
  Result := 7;
end;

function TRCmdPrinterAdd.ExecuteRfo: TROperation;
begin
  Result := AddPrinter(GetParameter(0), GetParameter(2), GetParameter(1), 
    GetParameter(4), GetParameter(5), GetParameter(6), GetParameter(3),
    GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdPrinterDel }

class function TRCmdPrinterDel.GetCommandName: string;
begin
  Result := tkDelPrinter;
end;

function TRCmdPrinterDel.GetCommandNote: string;
begin
  Result := SCmdNoteDelPrinter;
end;

class function TRCmdPrinterDel.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdPrinterDel.ExecuteRfo: TROperation;
begin
  Result := DelPrinter(GetParameter(0), GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdNetPrinterAdd }

class function TRCmdNetPrinterAdd.GetCommandName: string;
begin
  Result := tkAddNetPrinter;
end;

function TRCmdNetPrinterAdd.GetCommandNote: string;
begin
  Result := SCmdNoteAddNetPrinter;
end;

class function TRCmdNetPrinterAdd.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdNetPrinterAdd.ExecuteRfo: TROperation;
begin
  Result := AddNetworkPrinter(GetParameter(0), GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdNetPrinterDel }

class function TRCmdNetPrinterDel.GetCommandName: string;
begin
  Result := tkDelNetPrinter;
end;

function TRCmdNetPrinterDel.GetCommandNote: string;
begin
  Result := SCmdNoteDelNetPrinter;
end;

class function TRCmdNetPrinterDel.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdNetPrinterDel.ExecuteRfo: TROperation;
begin
  Result := DelNetworkPrinter(GetParameter(0), GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdObjCustom }

constructor TRCmdObjCustom.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fLocalFlag := False;
end;

function TRCmdObjCustom.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if fLocalFlag then Result := Result + SRCmdLocalObj;
end;

procedure TRCmdObjCustom.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fLocalFlag := Pos(SRCmdLocalObj, AnsiUpperCase(Attributes)) > 0;
end;

function TRCmdObjCustom.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList + SRCmdLocalObj;
end;

function TRCmdObjCustom.GetCommandAttrNote(const Attr: Char): string;
begin
  if Attr = SRCmdLocalObj
  then Result := SRCmdRfoNoteLocalObj
  else Result := inherited GetCommandAttrNote(Attr);
end;

{ TRCmdFreeObject }

class function TRCmdFreeObject.GetCommandName: string;
begin
  Result := tkFreeObject;
end;

function TRCmdFreeObject.GetCommandNote: string;
begin
  Result := SCmdNoteFreeObject;
end;

class function TRCmdFreeObject.GetParamsLimit: Integer;
begin
  Result := 1;
end;

procedure TRCmdFreeObject.ExecuteCommand(var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFreeObject, [GetParameter(0)]));
  try
    if fLocalFlag
    then Script.DelObject(otProc, GetParameter(0))
    else Script.DelObject(otAuto, GetParameter(0));
    Script.DoShowInfo(Script, Now, mcOperation, msOk, GetSystemError(0));
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError, E.Message);
    end;
  end;
end;

{ TRCmdFindCustom }

constructor TRCmdFindCustom.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fSubDirs := False;
end;

function TRCmdFindCustom.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if fSubDirs then Result := Result + chSubDirs;
end;

procedure TRCmdFindCustom.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fSubDirs := Pos(chSubDirs, AnsiUpperCase(Attributes)) > 0;
end;

function TRCmdFindCustom.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList + chSubDirs;
end;

function TRCmdFindCustom.GetCommandAttrNote(const Attr: Char): string;
begin
  if Attr = chSubDirs
  then Result := SRCmdRfoNoteSrcSubDirs
  else Result := inherited GetCommandAttrNote(Attr);
end;

procedure TRCmdFindCustom.ExecuteCommand(var OpState: TRScriptState);
var
  TmpObj: TObject;
  FileList: TRFileListPos;
  FindRes: TRTransaction;
begin
  // Пробуем найти объект с заданным именем
  if fLocalFlag
  then TmpObj := Script.FindObject(otProc, GetParameter(0), False)
  else TmpObj := Script.FindObject(otScript, GetParameter(0), False);
  if Assigned(TmpObj) then
  begin
    // Запрашиваемый объект найден - проверяем его тип
    if TmpObj is TRFileListPos then
    begin
      FileList := TRFileListPos(TmpObj);
      FileList.Items.Clear;
    end
    else raise Exception.CreateFmt(SExecuteInvalidObject, [GetParameter(0)]);
  end
  else begin
    // Запрашиваемый объект не найден - создаем новый
    try
      FileList := TRFileListPos.Create;
      if fLocalFlag
      then Script.AddObject(otProc, GetParameter(0), FileList)
      else Script.AddObject(otScript, GetParameter(0), FileList);
    except
      FileList.Free;
      raise;
    end;
  end;
  // Запускаем поиск
  FindRes := ExecuteFind(FileList);
  FileList.First;
  Script.PutVariable(True, GetParameter(0), FileList.PositionValue);
  case FindRes.State of
    msOk, msIgnored, msInfo:
    begin
      OpState := stOk;
      Script.DoShowInfo(Script, Now, mcOperation, msOk, FindRes.Result);
    end;
    msWarning:
    begin
      OpState := stWarning;
      Script.DoShowInfo(Script, Now, mcOperation, msWarning, FindRes.Result);
    end;
    msError:
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError, FindRes.Result);
    end;
    msBreak:
    begin
      OpState := stErrorStop;
      Script.DoShowInfo(Script, Now, mcOperation, msBreak, FindRes.Result);
    end;
  end;
end;

{ TRCmdFindDirs }

class function TRCmdFindDirs.GetCommandName: string;
begin
  Result := tkFindDirs;
end;

function TRCmdFindDirs.GetCommandNote: string;
begin
  Result := SCmdNoteFindDirs;
end;

class function TRCmdFindDirs.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdFindDirs.ExecuteFind(var List: TRFileListPos): TRTransaction;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFindDirs, [GetParameter(1)]));
  Result := CreateDirectoryList(GetParameter(1), faAnyFile, List.Items, fSubDirs, True, ezOk,
    Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRCmdFindFiles }

class function TRCmdFindFiles.GetCommandName: string;
begin
  Result := tkFindFiles;
end;

function TRCmdFindFiles.GetCommandNote: string;
begin
  Result := SCmdNoteFindFiles;
end;

class function TRCmdFindFiles.GetParamsLimit: Integer;
begin
  Result := 3;
end;

function TRCmdFindFiles.ExecuteFind(var List: TRFileListPos): TRTransaction;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFindFiles, [GetParameter(1), GetParameter(2)]));
  Result := CreateFileList(GetParameter(1), GetParameter(2), faAnyFile, List.Items,
    fSubDirs, True, ezOk, Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRCmdListCustom }

procedure TRCmdListCustom.CheckListType(var List: TRListPosition; const TargetType: CRListPosition);
begin
  if not (List is TargetType) then
    raise Exception.CreateFmt(SExecuteInvalidObject, [List.ClassName]);
end;

procedure TRCmdListCustom.ExecuteCommand(var OpState: TRScriptState);
var
  Tmp: TObject;
  sValue: string;
begin
  if fLocalFlag
  then Tmp := Script.FindObject(otProc, GetParameter(0), True)
  else Tmp := Script.FindObject(otAuto, GetParameter(0), True);
  if Assigned(Tmp) and (Tmp is TRListPosition) then
  begin
    ExecuteList(TRListPosition(Tmp), OpState);
    if OpState <> stError then
    begin
      sValue := TRListPosition(Tmp).PositionValue;
      Script.PutVariable(True, GetParameter(0), sValue);
      Script.DoShowInfo(Script, Now, mcOperation, msOk,
        Format(SExecuteSet, [GetParameter(0), sValue]));
    end;
  end
  else begin
    OpState := stError;
    Script.DoShowInfo(Script, Now, mcOperation, msError,
      Format(SExecuteInvalidObject, [GetParameter(0)]));
  end;
end;

{ TRCmdListFirst }

class function TRCmdListFirst.GetCommandName: string;
begin
  Result := tkListFirst;
end;

function TRCmdListFirst.GetCommandNote: string;
begin
  Result := SCmdNoteListFirst;
end;

class function TRCmdListFirst.GetParamsLimit: Integer;
begin
  Result := 1;
end;

procedure TRCmdListFirst.ExecuteList(var List: TRListPosition; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteListFirst, [GetParameter(0)]));
  List.First;
end;

{ TRCmdListPrior }

class function TRCmdListPrior.GetCommandName: string;
begin
  Result := tkListPrior;
end;

function TRCmdListPrior.GetCommandNote: string;
begin
  Result := SCmdNoteListPrior;
end;

class function TRCmdListPrior.GetParamsLimit: Integer;
begin
  Result := 1;
end;

procedure TRCmdListPrior.ExecuteList(var List: TRListPosition; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteListPrior, [GetParameter(0)]));
  List.Prior;
end;

{ TRCmdListFirst }

class function TRCmdListNext.GetCommandName: string;
begin
  Result := tkListNext;
end;

function TRCmdListNext.GetCommandNote: string;
begin
  Result := SCmdNoteListNext;
end;

class function TRCmdListNext.GetParamsLimit: Integer;
begin
  Result := 1;
end;

procedure TRCmdListNext.ExecuteList(var List: TRListPosition; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteListNext, [GetParameter(0)]));
  List.Next;
end;

{ TRCmdListLast }

class function TRCmdListLast.GetCommandName: string;
begin
  Result := tkListLast;
end;

function TRCmdListLast.GetCommandNote: string;
begin
  Result := SCmdNoteListLast;
end;

class function TRCmdListLast.GetParamsLimit: Integer;
begin
  Result := 1;
end;

procedure TRCmdListLast.ExecuteList(var List: TRListPosition; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteListLast, [GetParameter(0)]));
  List.Last;
end;

{ TRCmdExtractFilePath }

class function TRCmdExtractFilePath.GetCommandName: string;
begin
  Result := tkExtractPathName;
end;

function TRCmdExtractFilePath.GetCommandNote: string;
begin
  Result := SCmdNoteExtractFilePath;
end;

class function TRCmdExtractFilePath.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdExtractFilePath.ExecuteCommand(var OpState: TRScriptState);
var
  PathValue: string;
begin
  PathValue := ExcludeTrailingPathDelimiter(ExtractFilePath(GetParameter(0)));
  Script.PutVariable(True, GetParameter(1), PathValue);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [GetParameter(1), PathValue]));
end;

{ TRCmdExtractFileName }

class function TRCmdExtractFileName.GetCommandName: string;
begin
  Result := tkExtractFileName;
end;

function TRCmdExtractFileName.GetCommandNote: string;
begin
  Result := SCmdNoteExtractFileName;
end;

class function TRCmdExtractFileName.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdExtractFileName.ExecuteCommand(var OpState: TRScriptState);
begin
  Script.PutVariable(True, GetParameter(1), ExtractFileName(GetParameter(0)));
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [GetParameter(1), ExtractFileName(GetParameter(0))]));
end;

{ TRCmdExtractFileExt }

class function TRCmdExtractFileExt.GetCommandName: string;
begin
  Result := tkExtractFileExt;
end;

function TRCmdExtractFileExt.GetCommandNote: string;
begin
  Result := SCmdNoteExtractFileExt;
end;

class function TRCmdExtractFileExt.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdExtractFileExt.ExecuteCommand(var OpState: TRScriptState);
begin
  Script.PutVariable(True, GetParameter(1), ExtractFileExt(GetParameter(0)));
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [GetParameter(1), ExtractFileExt(GetParameter(0))]));
end;

{ TRCmdChangeFileExt }

class function TRCmdChangeFileExt.GetCommandName: string;
begin
  Result := tkChangeFileExt;
end;

function TRCmdChangeFileExt.GetCommandNote: string;
begin
  Result := SCmdNoteChangeFileExt;
end;

class function TRCmdChangeFileExt.GetParamsLimit: Integer;
begin
  Result := 3;
end;

procedure TRCmdChangeFileExt.ExecuteCommand(var OpState: TRScriptState);
var
  NewName: string;
begin
  NewName := ChangeFileExt(GetParameter(0), GetParameter(1));
  Script.PutVariable(True, GetParameter(2), NewName);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [GetParameter(2), NewName]));
end;

{ TRCmdCheckFileName }

class function TRCmdCheckFileName.GetCommandName: string;
begin
  Result := tkCheckFileName;
end;

function TRCmdCheckFileName.GetCommandNote: string;
begin
  Result := SCmdNoteCheckFileName;
end;

class function TRCmdCheckFileName.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdCheckFileName.ExecuteCommand(var OpState: TRScriptState);
var
  NewName: string;
begin
  NewName := ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(ReplaceStr(
    ReplaceStr(GetParameter(0), '"', ''''''),
    '>', '@'), '<', '@'), '/', '\'), '№', '#'), '*', '@'), '?', '@');
  Script.PutVariable(True, GetParameter(1), NewName);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [GetParameter(1), NewName]));
end;

{ TRCmdExitWindows }

constructor TRCmdExitWindows.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fFlags := EWX_LOGOFF;
end;

class function TRCmdExitWindows.GetParamsLimit: Integer;
begin
  Result := 0;
end;

class function TRCmdExitWindows.GetCommandName: string;
begin
  Result := tkExitWindows;
end;

function TRCmdExitWindows.GetCommandNote: string;
begin
  Result := SCmdNoteExitWindows;
end;

function TRCmdExitWindows.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if ((EWX_SHUTDOWN and fFlags) = 0) and ((EWX_REBOOT and fFlags) = 0) then
    Result := Result + SRCmdExitWinFlag_LOGOFF;
  if (EWX_SHUTDOWN and fFlags) > 0 then
    Result := Result + SRCmdExitWinFlag_SHUTDOWN;
  if (EWX_REBOOT and fFlags) > 0 then
    Result := Result + SRCmdExitWinFlag_REBOOT;
  if (EWX_FORCE and fFlags) > 0 then
    Result := Result + SRCmdExitWinFlag_FORCE;
  if (EWX_POWEROFF and fFlags) > 0 then
    Result := Result + SRCmdExitWinFlag_POWEROFF;
  if (EWX_FORCEIFHUNG and fFlags) > 0 then
    Result := Result + SRCmdExitWinFlag_FORCEIFHUNG;
end;

procedure TRCmdExitWindows.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fFlags := EWX_LOGOFF;
  if Pos(SRCmdExitWinFlag_REBOOT, AnsiUpperCase(Attributes)) > 0 then
    fFlags := EWX_REBOOT;
  if Pos(SRCmdExitWinFlag_SHUTDOWN, AnsiUpperCase(Attributes)) > 0 then
    fFlags := EWX_SHUTDOWN;
  if Pos(SRCmdExitWinFlag_FORCE, AnsiUpperCase(Attributes)) > 0 then
    fFlags := fFlags or EWX_FORCE;
  if Pos(SRCmdExitWinFlag_POWEROFF, AnsiUpperCase(Attributes)) > 0 then
    fFlags := fFlags or EWX_POWEROFF;
  if Pos(SRCmdExitWinFlag_FORCEIFHUNG, AnsiUpperCase(Attributes)) > 0 then
    fFlags := fFlags or EWX_FORCEIFHUNG;
end;

function TRCmdExitWindows.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList +
    SRCmdExitWinFlag_LOGOFF +
    SRCmdExitWinFlag_REBOOT +
    SRCmdExitWinFlag_SHUTDOWN +
    SRCmdExitWinFlag_POWEROFF +
    SRCmdExitWinFlag_FORCE +
    SRCmdExitWinFlag_FORCEIFHUNG;
end;

function TRCmdExitWindows.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = SRCmdExitWinFlag_LOGOFF then
      Result := SRNoteExitWinFlag_LOGOFF;
    if Attr = SRCmdExitWinFlag_REBOOT then
      Result := SRNoteExitWinFlag_REBOOT;
    if Attr = SRCmdExitWinFlag_SHUTDOWN then
      Result := SRNoteExitWinFlag_SHUTDOWN;
    if Attr = SRCmdExitWinFlag_POWEROFF then
      Result := SRNoteExitWinFlag_POWEROFF;
    if Attr = SRCmdExitWinFlag_FORCE then
      Result := SRNoteExitWinFlag_FORCE;
    if Attr = SRCmdExitWinFlag_FORCEIFHUNG then
      Result := SRNoteExitWinFlag_FORCEIFHUNG;
  end;
end;

function TRCmdExitWindows.ExecuteRfo: TROperation;
begin
  Result := RestartWindows(fFlags, GetErrorOptions, Script.DoShowInfo);
end;

{ TRCmdServiceStart }

class function TRCmdServiceStart.GetCommandName: string;
begin
  Result := tkServiceStart;
end;

function TRCmdServiceStart.GetCommandNote: string;
begin
  Result := SCmdNoteServiceStart;
end;

class function TRCmdServiceStart.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdServiceStart.ExecuteRfo: TROperation;
begin
  Result := StartService(GetParameter(0), GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRCmdServiceStop }

class function TRCmdServiceStop.GetCommandName: string;
begin
  Result := tkServiceStop;
end;

function TRCmdServiceStop.GetCommandNote: string;
begin
  Result := SCmdNoteServiceStop;
end;

class function TRCmdServiceStop.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRCmdServiceStop.ExecuteRfo: TROperation;
begin
  Result := StopService(GetParameter(0), GetErrorOptions,
    Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRCmdTextCustom }

procedure TRCmdTextCustom.ExecuteCommand(var OpState: TRScriptState);
var
  Tmp: TObject;
begin
  if fLocalFlag
  then Tmp := Script.FindObject(otProc, GetParameter(0), True)
  else Tmp := Script.FindObject(otAuto, GetParameter(0), True);
  if Assigned(Tmp) and (Tmp is TRStringListPos)
  then ExecuteText(TRStringListPos(Tmp), OpState)
  else begin
    OpState := stError;
    Script.DoShowInfo(Script, Now, mcOperation, msError,
      Format(SExecuteInvalidObject, [GetParameter(0)]));
  end;
end;

{ TRCmdTextCreate }

class function TRCmdTextCreate.GetCommandName: string;
begin
  Result := tkTextCreate;
end;

function TRCmdTextCreate.GetCommandNote: string;
begin
  Result := SCmdNoteTextCreate;
end;

class function TRCmdTextCreate.GetParamsLimit: Integer;
begin
  Result := 1;
end;

procedure TRCmdTextCreate.ExecuteCommand(var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteTextCreate, [GetParameter(0)]));
  if fLocalFlag
  then Script.AddObject(otProc, GetParameter(0), TRStringListPos.Create)
  else Script.AddObject(otScript, GetParameter(0), TRStringListPos.Create);
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    GetSystemError(S_OK));
end;

{ TRCmdTextOpen }

class function TRCmdTextOpen.GetCommandName: string;
begin
  Result := tkTextOpen;
end;

function TRCmdTextOpen.GetCommandNote: string;
begin
  Result := SCmdNoteTextOpen;
end;

class function TRCmdTextOpen.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdTextOpen.ExecuteCommand(var OpState: TRScriptState);
var
  ObjFind: TObject;
begin
  if fLocalFlag
  then ObjFind := Script.FindObject(otProc, GetParameter(0), False)
  else ObjFind := Script.FindObject(otAuto, GetParameter(0), False);
  if not Assigned(ObjFind) then
  begin
    Script.DoShowInfo(Script, Now, mcOperation, msTitle,
      Format(SExecuteTextCreate, [GetParameter(0)]));
    if fLocalFlag
    then Script.AddObject(otProc, GetParameter(0), TRStringListPos.Create)
    else Script.AddObject(otScript, GetParameter(0), TRStringListPos.Create);
    Script.DoShowInfo(Script, Now, mcOperation, msOk,
      GetSystemError(S_OK));
  end;
  inherited ExecuteCommand(OpState);
end;

procedure TRCmdTextOpen.ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState);
begin
  try
    Script.DoShowInfo(Script, Now, mcOperation, msTitle,
      Format(SExecuteTextLoadTitle, [GetParameter(0), GetParameter(1)]));
    Text.Items.LoadFromFile(GetParameter(1));
    Text.First;
    Script.PutVariable(True, GetParameter(0), Text.PositionValue);
    Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteTextLoadResult, [Text.Count, Length(Text.Text)]));
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(EExecuteOpenFile, [E.Message]));
    end;
  end;
end;

{ TRCmdTextSave }

class function TRCmdTextSave.GetCommandName: string;
begin
  Result := tkTextSave;
end;

function TRCmdTextSave.GetCommandNote: string;
begin
  Result := SCmdNoteTextSave;
end;

class function TRCmdTextSave.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdTextSave.ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState);
begin
  try
    Script.DoShowInfo(Script, Now, mcOperation, msTitle,
      Format(SExecuteTextSaveTitle, [GetParameter(0), GetParameter(1)]));
    Text.Items.SaveToFile(GetParameter(1));
    Script.DoShowInfo(Script, Now, mcOperation, msOk,
      Format(SExecuteTextSaveResult, [Text.Count, Length(Text.Text)]));
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(EExecuteSaveFile, [E.Message]));
    end;
  end;
end;

{ TRCmdTextAddChars }

class function TRCmdTextAddChars.GetCommandName: string;
begin
  Result := tkTextAddChars;
end;

function TRCmdTextAddChars.GetCommandNote: string;
begin
  Result := SCmdNoteTextAddChars;
end;

class function TRCmdTextAddChars.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdTextAddChars.ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteTextAddChars, [GetParameter(1), GetParameter(0)]));
  Text.Items.Text := Text.Items.Text + GetParameter(1);
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteTextAdd, [Length(GetParameter(1)), GetParameter(1)]));
end;

{ TRCmdTextAddLine }

class function TRCmdTextAddLine.GetCommandName: string;
begin
  Result := tkTextAddLine;
end;

function TRCmdTextAddLine.GetCommandNote: string;
begin
  Result := SCmdNoteTextAddLine;
end;

class function TRCmdTextAddLine.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdTextAddLine.ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteTextAddLine, [GetParameter(1), GetParameter(0)]));
  Text.Items.Add(GetParameter(1));
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteTextAdd, [Length(GetParameter(1)), GetParameter(1)]));
end;

{ TRCmdTextReplace }

constructor TRCmdTextReplace.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fOem := False;
  fFlags := [rfReplaceAll, rfIgnoreCase];
end;

class function TRCmdTextReplace.GetCommandName: string;
begin
  Result := tkTextReplace;
end;

function TRCmdTextReplace.GetCommandNote: string;
begin
  Result := SCmdNoteTextReplace;
end;

class function TRCmdTextReplace.GetParamsLimit: Integer;
begin
  Result := 3;
end;

function TRCmdTextReplace.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList +
    SRCmdTextIgnoreCase + SRCmdTextReplaceAll + SRCmdTextOemCode;
end;

function TRCmdTextReplace.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = SRCmdTextIgnoreCase then
      Result := SRNoteTextIgnoreCase;
    if Attr = SRCmdTextReplaceAll then
      Result := SRNoteTextReplaceAll;
    if Attr = SRCmdTextOemCode then
      Result := SRNoteTextOemCode;
  end;
end;

function TRCmdTextReplace.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if rfIgnoreCase in fFlags then
    Result := Result + SRCmdTextIgnoreCase;
  if rfReplaceAll in fFlags then
    Result := Result + SRCmdTextReplaceAll;
  if fOem then
    Result := Result + SRCmdTextOemCode;
end;

procedure TRCmdTextReplace.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fOem := Pos(SRCmdTextOemCode, AnsiUpperCase(Attributes)) > 0;
  fFlags := [];
  if Pos(SRCmdTextIgnoreCase, AnsiUpperCase(Attributes)) > 0 then
    fFlags := fFlags + [rfIgnoreCase];
  if Pos(SRCmdTextReplaceAll, AnsiUpperCase(Attributes)) > 0 then
    fFlags := fFlags + [rfReplaceAll];
end;

procedure TRCmdTextReplace.ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState);
var
  IsBreak: Boolean;
  SourceStr, SearchStr, Patt, NewStr, ResStr, OldPattern, NewPattern: string;
  TextLength, PosOffset, ReplaceCount: Integer;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteTextReplaceTitle, [GetParameter(0), GetParameter(1), GetParameter(2)]));
  ReplaceCount := 0;
  if fOem
  then SourceStr := OemToAnsiStr(Text.Text)
  else SourceStr := Text.Text;
  TextLength := Length(SourceStr);
  OldPattern := GetParameter(1);
  NewPattern := GetParameter(2);
  if rfIgnoreCase in fFlags then
  begin
    SearchStr := AnsiUpperCase(SourceStr);
    Patt := AnsiUpperCase(OldPattern);
  end else
  begin
    SearchStr := SourceStr;
    Patt := OldPattern;
  end;
  IsBreak := False;
  NewStr := SourceStr;
  ResStr := '';
  while SearchStr <> '' do
  begin
    Script.DoCheckBreak(Script, IsBreak);
    if IsBreak then Break;
    Script.DoShowProgress(Script, mcTransaction, Length(ResStr), TextLength);
    Script.DoShowInfo(Script, Now, mcTransaction, msTitle,
      Format(SExecuteTextReplaceProgress, [ReplaceCount]));
    PosOffset := AnsiPos(Patt, SearchStr);
    if PosOffset = 0 then
    begin
      ResStr := ResStr + NewStr;
      Break;
    end;
    Inc(ReplaceCount);
    ResStr := ResStr + Copy(NewStr, 1, PosOffset - 1) + NewPattern;
    NewStr := Copy(NewStr, PosOffset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in fFlags) then
    begin
      ResStr := ResStr + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, PosOffset + Length(Patt), MaxInt);
  end;
  if IsBreak then
  begin
    OpState := stError;
    Script.DoShowInfo(Script, Now, mcOperation, msBreak, SOprMsgTypeBreak);
  end
  else begin
    if fOem
    then Text.Items.Text := StrToOem(ResStr)
    else Text.Items.Text := ResStr;
    Script.DoShowInfo(Script, Now, mcOperation, msOk,
      Format(SExecuteTextReplaceResult, [ReplaceCount]));
  end
end;

{ TRCmdTextSet }

class function TRCmdTextSet.GetCommandName: string;
begin
  Result := tkTextSet;
end;

function TRCmdTextSet.GetCommandNote: string;
begin
  Result := SCmdNoteTextSet;
end;

class function TRCmdTextSet.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdTextSet.ExecuteText(var Text: TRStringListPos; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteTextSet, [GetParameter(1), GetParameter(0)]));
  Script.PutVariable(True, GetParameter(1), Text.Text);
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteSet, [GetParameter(1), Text.Text]));
end;

{ TRCmdFileAppend }

constructor TRCmdFileAppend.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fOem := False;
  fLine := True;
end;

class function TRCmdFileAppend.GetCommandName: string;
begin
  Result := tkFileAppend;
end;

function TRCmdFileAppend.GetCommandNote: string;
begin
  Result := SCmdNoteFileAppend;
end;

class function TRCmdFileAppend.GetParamsLimit: Integer;
begin
  Result := 2;
end;

function TRCmdFileAppend.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList
    + SRCmdTextOemCode + SRCmdTextWriteLn;
end;

function TRCmdFileAppend.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if fOem then
    Result := Result + SRCmdTextOemCode;
  if fLine then
    Result := Result + SRCmdTextWriteLn;
end;

procedure TRCmdFileAppend.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fOem := Pos(SRCmdTextOemCode, AnsiUpperCase(Attributes)) > 0;
  fLine := Pos(SRCmdTextWriteLn, AnsiUpperCase(Attributes)) > 0;
end;

function TRCmdFileAppend.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = SRCmdTextOemCode then
      Result := SRNoteTextOemCode;
    if Attr = SRCmdTextWriteLn then
      Result := SRNoteTextWriteLn;
  end;
end;

procedure TRCmdFileAppend.ExecuteCommand(var OpState: TRScriptState);
var
  TextF: TextFile;
  TextS: string;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteScriptFileAppend, [GetParameter(0), GetParameter(1)]));
  TextS := GetParameter(1);
  if fOem then TextS := StrToOem(TextS);
  if fLine then TextS := TextS + #13#10;
  try
    AssignFile(TextF, GetParameter(0));
    if FileExists(GetParameter(0))
    then Append(TextF)
    else Rewrite(TextF);
    try
      Write(TextF, TextS);
      Script.DoShowInfo(Script, Now, mcOperation, msOk, GetSystemError(S_OK));
    finally
      CloseFile(TextF);
    end;
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(EExecuteSaveFile, [E.Message]));
    end;
  end;
end;

{ TRCmdScriptSave }

class function TRCmdScriptSave.GetCommandName: string;
begin
  Result := tkScriptSave;
end;

function TRCmdScriptSave.GetCommandNote: string;
begin
  Result := SCmdNoteScriptSave;
end;

class function TRCmdScriptSave.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdScriptSave.ExecuteCommand(var OpState: TRScriptState);
var
  ExtScript: TRScriptCustom;
  TextF: TextFile;
  TextS: string;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteScriptSaveTitle, [GetParameter(0), GetParameter(1)]));
  ExtScript := Script.FindExternalScript(GetParameter(0));
  if Assigned(ExtScript) then
  begin
    try
      TextS := ExtScript.CreateScriptText;
      AssignFile(TextF, GetParameter(1));
      Rewrite(TextF);
      try
        Write(TextF, TextS);
        Script.DoShowInfo(Script, Now, mcOperation, msOk, GetSystemError(S_OK));
      finally
        CloseFile(TextF);
      end;
    except
      on E: Exception do
      begin
        OpState := stError;
        Script.DoShowInfo(Script, Now, mcOperation, msError,
          Format(EExecuteSaveFile, [E.Message]));
      end;
    end;
  end
  else begin
    OpState := stError;
    Script.DoShowInfo(Script, Now, mcOperation, msError,
      Format(SExecuteScriptSaveError, [GetParameter(0)]));
  end;
end;

{ TRCmdIniReadVariable }

class function TRCmdIniReadVariable.GetCommandName: string;
begin
  Result := tkIniReadVariable;
end;

class function TRCmdIniReadVariable.GetParamsLimit: Integer;
begin
  Result := 5;
end;

function TRCmdIniReadVariable.GetCommandNote: string;
begin
  Result := SCmdIniReadVariable;
end;

procedure TRCmdIniReadVariable.ExecuteCommand(var OpState: TRScriptState);
var
  Ini: TMemIniFile;
  VarName, VarValue: string;
begin
  VarName := GetParameter(0);
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteIniReadVariable, [VarName, GetParameter(1), GetParameter(2), GetParameter(3)]));
  Ini := TMemIniFile.Create(GetParameter(1));
  try
    VarValue := Ini.ReadString(GetParameter(2), GetParameter(3), GetParameter(4));
  finally
    Ini.Free;
  end;
  Script.PutVariable(True, VarName, VarValue);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [VarName, VarValue]));
end;

{ TRCmdIniReadSections }

class function TRCmdIniReadSections.GetCommandName: string;
begin
  Result := tkIniReadSections;
end;

function TRCmdIniReadSections.GetCommandNote: string;
begin
  Result := SCmdIniReadSections;
end;

class function TRCmdIniReadSections.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdIniReadSections.ExecuteCommand(var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteIniSectionsTitle, [GetParameter(0), GetParameter(1)]));
  if fLocalFlag then
  begin
    if not Assigned(Script.FindObject(otProc, GetParameter(0), False)) then
      Script.AddObject(otProc, GetParameter(0), TRStringListPos.Create);
  end
  else begin
    if not Assigned(Script.FindObject(otAuto, GetParameter(0), False)) then
      Script.AddObject(otScript, GetParameter(0), TRStringListPos.Create);
  end;
  inherited ExecuteCommand(OpState);
end;

procedure TRCmdIniReadSections.ExecuteList(var List: TRListPosition; var OpState: TRScriptState);
var
  Ini: TMemIniFile;
begin
  try
    CheckListType(List, TRStringListPos);
    Ini := TMemIniFile.Create(GetParameter(1));
    try
      Ini.ReadSections(TRStringListPos(List).Items);
      List.First;
      Script.DoShowInfo(Script, Now, mcOperation, msOk,
        Format(SExecuteIniReadResult, [List.Count]));
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(EExecuteOpenFile, [E.Message]));
    end;
  end;
end;

{ TRCmdIniReadKeys }

class function TRCmdIniReadKeys.GetCommandName: string;
begin
  Result := tkIniReadKeys;
end;

function TRCmdIniReadKeys.GetCommandNote: string;
begin
  Result := SCmdIniReadKeys;
end;

class function TRCmdIniReadKeys.GetParamsLimit: Integer;
begin
  Result := 3;
end;

procedure TRCmdIniReadKeys.ExecuteCommand(var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteIniKeysTitle, [GetParameter(0), GetParameter(2), GetParameter(1)]));
  if fLocalFlag then
  begin
    if not Assigned(Script.FindObject(otProc, GetParameter(0), False)) then
      Script.AddObject(otProc, GetParameter(0), TRStringListPos.Create);
  end
  else begin
    if not Assigned(Script.FindObject(otAuto, GetParameter(0), False)) then
      Script.AddObject(otScript, GetParameter(0), TRStringListPos.Create);
  end;
  inherited ExecuteCommand(OpState);
end;

procedure TRCmdIniReadKeys.ExecuteList(var List: TRListPosition; var OpState: TRScriptState);
var
  Ini: TMemIniFile;
begin
  try
    CheckListType(List, TRStringListPos);
    Ini := TMemIniFile.Create(GetParameter(1));
    try
      Ini.ReadSection(GetParameter(2), TRStringListPos(List).Items);
      List.First;
      Script.DoShowInfo(Script, Now, mcOperation, msOk,
        Format(SExecuteIniReadResult, [List.Count]));
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(EExecuteOpenFile, [E.Message]));
    end;
  end;
end;

{ TRCmdRegReadVariable }

class function TRCmdRegReadVariable.GetCommandName: string;
begin
  Result := tkRegReadVariable;
end;

class function TRCmdRegReadVariable.GetParamsLimit: Integer;
begin
  Result := 5;
end;

function TRCmdRegReadVariable.GetCommandNote: string;
begin
  Result := SCmdRegReadVariable;
end;

procedure TRCmdRegReadVariable.ExecuteCommand(var OpState: TRScriptState);
var
  Reg: TRegIniFile;
  VarName, VarValue: string;
begin
  VarName := GetParameter(0);
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteRegReadVariable, [VarName, GetParameter(1), GetParameter(2), GetParameter(3)]));
  Reg := TRegIniFile.Create(KEY_READ);
  try
    Reg.RootKey := DecodeRegistryRootKey(GetParameter(1));
    VarValue := Reg.ReadString(GetParameter(2), GetParameter(3), GetParameter(4));
  finally
    Reg.Free;
  end;
  Script.PutVariable(True, VarName, VarValue);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [VarName, VarValue]));
end;

{ TRCmdRegReadSections }

class function TRCmdRegReadSections.GetCommandName: string;
begin
  Result := tkRegReadSections;
end;

function TRCmdRegReadSections.GetCommandNote: string;
begin
  Result := SCmdRegReadSections;
end;

class function TRCmdRegReadSections.GetParamsLimit: Integer;
begin
  Result := 3;
end;

procedure TRCmdRegReadSections.ExecuteCommand(var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteRegSectionsTitle, [GetParameter(0), GetParameter(1), GetParameter(2)]));
  if fLocalFlag then
  begin
    if not Assigned(Script.FindObject(otProc, GetParameter(0), False)) then
      Script.AddObject(otProc, GetParameter(0), TRStringListPos.Create);
  end
  else begin
    if not Assigned(Script.FindObject(otAuto, GetParameter(0), False)) then
      Script.AddObject(otScript, GetParameter(0), TRStringListPos.Create);
  end;
  inherited ExecuteCommand(OpState);
end;

procedure TRCmdRegReadSections.ExecuteList(var List: TRListPosition; var OpState: TRScriptState);
var
  Reg: TRegIniFile;
begin
  try
    CheckListType(List, TRStringListPos);
    Reg := TRegIniFile.Create(KEY_READ);
    try
      Reg.RootKey := DecodeRegistryRootKey(GetParameter(1));
      if Reg.OpenKey(GetParameter(2), False) then
      begin
        try
          Reg.ReadSections(TRStringListPos(List).Items);
          List.First;
          Script.DoShowInfo(Script, Now, mcOperation, msOk,
            Format(SExecuteRegReadResult, [List.Count]));
        finally
          Reg.CloseKey;
        end;
      end
      else begin
        OpState := stError;
        Script.DoShowInfo(Script, Now, mcOperation, msError,
          Format(EExecuteRegKeyNotExists, [GetParameter(1), GetParameter(2)]));
      end;
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(EExecuteOpenFile, [E.Message]));
    end;
  end;
end;

{ TRCmdRegReadKeys }

class function TRCmdRegReadKeys.GetCommandName: string;
begin
  Result := tkRegReadKeys;
end;

function TRCmdRegReadKeys.GetCommandNote: string;
begin
  Result := SCmdRegReadKeys;
end;

class function TRCmdRegReadKeys.GetParamsLimit: Integer;
begin
  Result := 3;
end;

procedure TRCmdRegReadKeys.ExecuteCommand(var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteRegKeysTitle, [GetParameter(0), GetParameter(1), GetParameter(2)]));
  if fLocalFlag then
  begin
    if not Assigned(Script.FindObject(otProc, GetParameter(0), False)) then
      Script.AddObject(otProc, GetParameter(0), TRStringListPos.Create);
  end
  else begin
    if not Assigned(Script.FindObject(otAuto, GetParameter(0), False)) then
      Script.AddObject(otScript, GetParameter(0), TRStringListPos.Create);
  end;
  inherited ExecuteCommand(OpState);
end;

procedure TRCmdRegReadKeys.ExecuteList(var List: TRListPosition; var OpState: TRScriptState);
var
  Reg: TRegIniFile;
begin
  try
    CheckListType(List, TRStringListPos);
    Reg := TRegIniFile.Create(KEY_READ);
    try
      Reg.RootKey := DecodeRegistryRootKey(GetParameter(1));
      Reg.ReadSection(GetParameter(2), TRStringListPos(List).Items);
      List.First;
      Script.DoShowInfo(Script, Now, mcOperation, msOk,
        Format(SExecuteRegReadResult, [List.Count]));
    finally
      Reg.Free;
    end;
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(EExecuteOpenFile, [E.Message]));
    end;
  end;
end;

{ TRCmdCalcCrc32 }

class function TRCmdCalcCrc32.GetCommandName: string;
begin
  Result := tkCalcCrc32;
end;

function TRCmdCalcCrc32.GetCommandNote: string;
begin
  Result := SCmdCalcCrc32;
end;

class function TRCmdCalcCrc32.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdCalcCrc32.ExecuteCommand(var OpState: TRScriptState);
var
  VarName, VarValue: string;
begin
  VarName := GetParameter(0);
  Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCalcCrc32, [GetParameter(1)]));
  try
    VarValue := AnsiUpperCase(Format('%.8x', [FileCRC32(GetParameter(1))]));
    Script.PutVariable(True, VarName, VarValue);
    Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [VarName, VarValue]));
  except
    on E: Exception do
    begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError, E.Message);
    end;
  end;
end;

{ TRCmdStrReplace }

constructor TRCmdStrReplace.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fFlags := [rfReplaceAll, rfIgnoreCase];
end;

class function TRCmdStrReplace.GetCommandName: string;
begin
  Result := tkStrReplace;
end;

function TRCmdStrReplace.GetCommandNote: string;
begin
  Result := SCmdStrReplace;
end;

class function TRCmdStrReplace.GetParamsLimit: Integer;
begin
  Result := 3;
end;

function TRCmdStrReplace.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList +
    SRCmdTextIgnoreCase + SRCmdTextReplaceAll;
end;

function TRCmdStrReplace.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = SRCmdTextIgnoreCase then
      Result := SRNoteTextIgnoreCase;
    if Attr = SRCmdTextReplaceAll then
      Result := SRNoteTextReplaceAll;
  end;
end;

function TRCmdStrReplace.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if rfIgnoreCase in fFlags then
    Result := Result + SRCmdTextIgnoreCase;
  if rfReplaceAll in fFlags then
    Result := Result + SRCmdTextReplaceAll;
end;

procedure TRCmdStrReplace.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fFlags := [];
  if Pos(SRCmdTextIgnoreCase, AnsiUpperCase(Attributes)) > 0 then
    fFlags := fFlags + [rfIgnoreCase];
  if Pos(SRCmdTextReplaceAll, AnsiUpperCase(Attributes)) > 0 then
    fFlags := fFlags + [rfReplaceAll];
end;

procedure TRCmdStrReplace.ExecuteCommand(var OpState: TRScriptState);
var
  VarName, VarValue: string;
begin
  VarName := GetParameter(0);
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteStrReplace, [GetParameter(1), GetParameter(2), VarName]));
  VarValue := StringReplace(
    GetVariableValue(Script.VarListExec, VarName, True),
    GetParameter(1), GetParameter(2), fFlags);
  Script.PutVariable(True, VarName, VarValue);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteSet, [VarName, VarValue]));
end;

{ TRCmdFtpOpen }

constructor TRCmdFtpOpen.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fPassive := False;
  fBinary := False;
end;

class function TRCmdFtpOpen.GetCommandName: string;
begin
  Result := tkFtpOpen;
end;

function TRCmdFtpOpen.GetCommandNote: string;
begin
  Result := SCmdNoteFtpOpen;
end;

class function TRCmdFtpOpen.GetParamsLimit: Integer;
begin
  Result := 4;
end;

function TRCmdFtpOpen.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList +
    SRCmdFtpPassive + SRCmdFtpBinary;
end;

function TRCmdFtpOpen.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = SRCmdFtpPassive then
      Result := SRNoteFtpPassive;
    if Attr = SRCmdFtpBinary then
      Result := SRNoteFtpBinary;
  end;
end;

function TRCmdFtpOpen.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if fPassive then
    Result := Result + SRCmdFtpPassive;
  if fBinary then
    Result := Result + SRCmdFtpBinary;
end;

procedure TRCmdFtpOpen.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  fPassive := Pos(SRCmdFtpPassive, AnsiUpperCase(Attributes)) > 0;
  fBinary := Pos(SRCmdFtpBinary, AnsiUpperCase(Attributes)) > 0;
end;

procedure TRCmdFtpOpen.ExecuteCommand(var OpState: TRScriptState);
var
  fFtp: TIdFtp;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFtpOpen, [GetParameter(1)]));
  fFtp := TIdFtp.Create;
  try
    fFtp.Passive := fPassive;
    if fBinary
    then fFtp.TransferType := ftBinary
    else fFtp.TransferType := ftASCII;
    fFtp.Host := GetParameter(1);
    fFtp.Username := GetParameter(2);
    fFtp.Password := GetParameter(3);
    fFtp.Connect;
    if fFtp.Connected then
    begin
      if fLocalFlag
      then Script.AddObject(otProc, GetParameter(0), fFtp)
      else Script.AddObject(otScript, GetParameter(0), fFtp);
      Script.DoShowInfo(Script, Now, mcOperation, msOk,
        Format(SExecuteFtpOpenOk, [fFtp.RetrieveCurrentDir]));
    end
    else begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError, SExecuteFtpNotConnected);
    end;
  except
    on E: Exception do
    begin
      fFtp.Free;
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(SExecuteFtpOpenError, [E.Message]));
    end;
  end;
end;

{ TRCmdFtpCustom }

constructor TRCmdFtpCustom.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  bIgnoreError := False;
  bWarningError := False;
end;

procedure TRCmdFtpCustom.ExecuteCommand(var OpState: TRScriptState);
var
  Tmp: TObject;
begin
  if fLocalFlag
  then Tmp := Script.FindObject(otProc, GetParameter(0), True)
  else Tmp := Script.FindObject(otAuto, GetParameter(0), True);
  if Assigned(Tmp) and (Tmp is TIdFtp) then
  begin
    if TIdFtp(Tmp).Connected then
    begin
      try
        ExecuteFtp(TIdFtp(Tmp), OpState);
      except
        on E: Exception do
        begin
          if bIgnoreError then
          begin
            OpState := stOk;
            Script.DoShowInfo(Script, Now, mcOperation, msOk,
              Format(SExecuteFtpError, [E.Message]));
          end
          else begin
            if bWarningError then
            begin
              OpState := stWarning;
              Script.DoShowInfo(Script, Now, mcOperation, msWarning,
                Format(SExecuteFtpError, [E.Message]));
            end
            else begin
              OpState := stError;
              Script.DoShowInfo(Script, Now, mcOperation, msError,
                Format(SExecuteFtpError, [E.Message]));
            end;
          end;
        end;
      end;
    end
    else begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError, SExecuteFtpNotConnected);
      if fLocalFlag
      then Script.DelObject(otProc, GetParameter(0))
      else Script.DelObject(otAuto, GetParameter(0));
      Script.DoShowInfo(Script, Now, mcOperation, msOk,
        Format(SExecuteFtpFree, [GetParameter(0)]));
    end;
  end
  else begin
    OpState := stError;
    Script.DoShowInfo(Script, Now, mcOperation, msError,
      Format(SExecuteInvalidObject, [GetParameter(0)]));
  end;
end;

function TRCmdFtpCustom.GetCommandAttrList: string;
begin
  Result := inherited GetCommandAttrList +
    SRCmdFtpErrIgnore + SRCmdFtpErrWarning;
end;

function TRCmdFtpCustom.GetCommandAttrNote(const Attr: Char): string;
begin
  Result := inherited GetCommandAttrNote(Attr);
  if Result = EmptyStr then
  begin
    if Attr = SRCmdFtpErrIgnore then
      Result := SRNoteFtpErrIgnore;
    if Attr = SRCmdFtpErrWarning then
      Result := SRNoteFtpErrWarning;
  end;
end;

function TRCmdFtpCustom.GetAttributes: string;
begin
  Result := inherited GetAttributes;
  if bIgnoreError then
    Result := Result + SRCmdFtpErrIgnore;
  if bWarningError then
    Result := Result + SRNoteFtpErrWarning;
end;

procedure TRCmdFtpCustom.SetAttributes(const Attributes: string);
begin
  inherited SetAttributes(Attributes);
  bIgnoreError := Pos(SRCmdFtpErrIgnore, AnsiUpperCase(Attributes)) > 0;
  bWarningError := Pos(SRNoteFtpErrWarning, AnsiUpperCase(Attributes)) > 0;
end;

{ TRCmdFtpClose }

class function TRCmdFtpClose.GetCommandName: string;
begin
  Result := tkFtpClose;
end;

function TRCmdFtpClose.GetCommandNote: string;
begin
  Result := SCmdNoteFtpClose;
end;

class function TRCmdFtpClose.GetParamsLimit: Integer;
begin
  Result := 1;
end;

procedure TRCmdFtpClose.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle, SExecuteFtpClose);
  Ftp.Disconnect;
  Script.DoShowInfo(Script, Now, mcOperation, msOk, SExecuteFtpClosed);

  if fLocalFlag
  then Script.DelObject(otProc, GetParameter(0))
  else Script.DelObject(otAuto, GetParameter(0));
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteFtpFree, [GetParameter(0)]));
end;

{ TRCmdFtpCurrDir }

class function TRCmdFtpCurrDir.GetCommandName: string;
begin
  Result := tkFtpCurrDir;
end;

function TRCmdFtpCurrDir.GetCommandNote: string;
begin
  Result := SCmdNoteFtpCurrDir;
end;

class function TRCmdFtpCurrDir.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdFtpCurrDir.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
var
  VarValue: string;
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle, SExecuteFtpCurrDir);
  VarValue := Ftp.RetrieveCurrentDir;
  Script.PutVariable(True, GetParameter(1), VarValue);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteFtpOkDir, [VarValue]));
end;

{ TRCmdFtpChDir }

class function TRCmdFtpChDir.GetCommandName: string;
begin
  Result := tkFtpChDir;
end;

function TRCmdFtpChDir.GetCommandNote: string;
begin
  Result := SCmdNoteFtpChDir;
end;

class function TRCmdFtpChDir.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdFtpChDir.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFtpChDir, [GetParameter(1)]));
  if GetParameter(1) = '.'
  then Ftp.ChangeDirUp
  else Ftp.ChangeDir(GetParameter(1));
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteFtpOkDir, [Ftp.RetrieveCurrentDir]));
end;

{ TRCmdFtpMkDir }

class function TRCmdFtpMkDir.GetCommandName: string;
begin
  Result := tkFtpMkDir;
end;

function TRCmdFtpMkDir.GetCommandNote: string;
begin
  Result := SCmdNoteFtpMkDir;
end;

class function TRCmdFtpMkDir.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdFtpMkDir.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFtpMkDir, [GetParameter(1)]));
  Ftp.MakeDir(GetParameter(1));
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteFtpOkDir, [Ftp.RetrieveCurrentDir]));
end;

{ TRCmdFtpFrDir }

class function TRCmdFtpFrDir.GetCommandName: string;
begin
  Result := tkFtpFrDir;
end;

function TRCmdFtpFrDir.GetCommandNote: string;
begin
  Result := SCmdNoteFtpFrDir;
end;

class function TRCmdFtpFrDir.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdFtpFrDir.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFtpFrDir, [GetParameter(1)]));
  try
    Ftp.MakeDir(GetParameter(1));
  finally
    Ftp.ChangeDir(GetParameter(1));
  end;
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteFtpOkDir, [Ftp.RetrieveCurrentDir]));
end;

{ TRCmdFtpRmDir }

class function TRCmdFtpRmDir.GetCommandName: string;
begin
  Result := tkFtpRmDir;
end;

function TRCmdFtpRmDir.GetCommandNote: string;
begin
  Result := SCmdNoteFtpRmDir;
end;

class function TRCmdFtpRmDir.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdFtpRmDir.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFtpRmDir, [GetParameter(1)]));
  Ftp.RemoveDir(GetParameter(1));
  Script.DoShowInfo(Script, Now, mcOperation, msOk,
    Format(SExecuteFtpOkDir, [Ftp.RetrieveCurrentDir]));
end;

{ TRCmdFtpGet }

class function TRCmdFtpGet.GetCommandName: string;
begin
  Result := tkFtpGet;
end;

function TRCmdFtpGet.GetCommandNote: string;
begin
  Result := SCmdNoteFtpGet;
end;

class function TRCmdFtpGet.GetParamsLimit: Integer;
begin
  Result := 3;
end;

procedure TRCmdFtpGet.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFtpGet, [GetParameter(1)]));
  Ftp.Get(GetParameter(1), GetParameter(2), True, False);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, SExecuteFtpOk);
end;

{ TRCmdFtpPut }

class function TRCmdFtpPut.GetCommandName: string;
begin
  Result := tkFtpPut;
end;

function TRCmdFtpPut.GetCommandNote: string;
begin
  Result := SCmdNoteFtpPut;
end;

class function TRCmdFtpPut.GetParamsLimit: Integer;
begin
  Result := 3;
end;

procedure TRCmdFtpPut.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFtpPut, [GetParameter(1)]));
  Ftp.Put(GetParameter(1), GetParameter(2), False, -1);
  Script.DoShowInfo(Script, Now, mcOperation, msOk, SExecuteFtpOk);
end;

{ TRCmdFtpDelete }

class function TRCmdFtpDelete.GetCommandName: string;
begin
  Result := tkFtpDelete;
end;

function TRCmdFtpDelete.GetCommandNote: string;
begin
  Result := SCmdNoteFtpDelete;
end;

class function TRCmdFtpDelete.GetParamsLimit: Integer;
begin
  Result := 2;
end;

procedure TRCmdFtpDelete.ExecuteFtp(var Ftp: TIdFtp; var OpState: TRScriptState);
begin
  Script.DoShowInfo(Script, Now, mcOperation, msTitle,
    Format(SExecuteFtpDelete, [GetParameter(1)]));
  Ftp.Delete(GetParameter(1));
  Script.DoShowInfo(Script, Now, mcOperation, msOk, SExecuteFtpOk);
end;

end.
