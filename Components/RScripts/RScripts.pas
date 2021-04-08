unit RScripts;

interface

uses
  Classes, SysUtils, Contnrs, Graphics, StdCtrls,
  RMsgTypes, RScrCnst, RSysUtils, RFileProcs;

type
  TRScript       = class;
  TRScripts      = class;
  TRScriptCustom = class;
  ERScriptError  = class (Exception);

  TRCmdCustom = class (TObject)
  private
    fScript: TRScriptCustom;
    fLocked: Boolean;
    fExtExec: Boolean;
    fErrFlags: TRCmdErrAttrs;
    fDescription: string;
  protected
    procedure SetAttributes(const Attributes: string); virtual;
    function  GetAttributes: string; virtual;
    function  GetCommandParams_Edit: string; virtual;
    function  GetCommandParams_Exec: string; virtual;
    procedure ExecuteCommand(var OpState: TRScriptState); virtual; abstract;
    procedure ExecOnExtProcessor(var OpState: TRScriptState); virtual;
  public
    constructor Create(Script: TRScriptCustom); virtual;
    destructor Destroy; override;
    function  GetCommandText_Edit: string; virtual;
    function  GetCommandText_Exec: string; virtual;
    function  GetCommandCode(const OffsetLevel: Integer): string; virtual;
    function  GetCommandCount: Integer; virtual;
    function  UpdateVariables(const ScrStr: string): string; virtual;
    procedure Execute(var State: TRScriptState); virtual;
    class function GetCommandName: string; virtual; abstract;
    function GetCommandDescription: string; virtual;
    function GetCommandNote: string; virtual; abstract;
    function GetCommandAttrList: string; virtual;
    function GetCommandAttrNote(const Attr: Char): string; virtual;
    property Script: TRScriptCustom read fScript;
    property Attributes: string read GetAttributes write SetAttributes;
    property Description: string read fDescription write fDescription;
  end;

  CRCmdExecuted = class of TRCmdExecuted;

  TRCmdExecuted = class (TRCmdCustom)
  private
    fParams: TRCmdParams;
  protected
    function  GetCommandParams_Edit: string; override;
    function  GetCommandParams_Exec: string; override;
    procedure CheckParameter(const Param: string); virtual;
  public
    constructor Create(Script: TRScriptCustom); override;
    destructor Destroy; override;
    function  GetParameter(const Index: Integer): string;
    class function GetParamsLimit: Integer; virtual;
    function  GetParamsCount: Integer;
    function  AddParameter(const Param: string): Integer;
    procedure AddParameters(const AParams: TRCmdParams);
    procedure CheckParameters;
    property  Parameters: TRCmdParams read fParams write fParams;
  end;

 TRCmdBlock = class (TObject)
  private
    fScript: TRScriptCustom;
    fCmdList: TObjectList;
    fIntBreak: Boolean;
    function  GetItem(Index: Integer): TRCmdCustom;
  protected
    fLockOperationProgress: Boolean;
    fLockIncTotalOpCount: Boolean;
  public
    constructor Create(Script: TRScriptCustom);
    destructor Destroy; override;
    function  GetCommandCount: Integer;
    function  Add(Command: TRCmdCustom): Integer;
    procedure Clear;
    procedure Execute(var State: TRScriptState);
    procedure BreakBlock;
    function  IndexOf(Command: TRCmdCustom): Integer;
    function  GetCommandsCode(const OffsetLevel: Integer): string;
    property  Item[Index: Integer]: TRCmdCustom read GetItem; default;
    property  IsBreakBlock: Boolean read fIntBreak;
  end;

  TRCmdProc = class (TObject)
  private
    fScript: TRScriptCustom;
    fName: string;
    fCmdList: TRCmdBlock;
    fObjStack: TList;
    procedure CreateObjList;
    procedure DeleteObjList;
    function  GetCurrList: TStringList;
  public
    constructor Create(Script: TRScriptCustom; const AName: string);
    destructor Destroy; override;
    function  GetCommandCount: Integer;
    function  GetCommandsCode: string;
    procedure Execute(var State: TRScriptState);
    procedure AddObject(const ObjName: string; const ObjPtr: TObject);
    procedure DelObject(const ObjName: string);
    function  FindObject(const ObjName: string): TObject;
    property Name: string read fName write fName;
    property CmdList: TRCmdBlock read fCmdList write fCmdList;
  end;

  TRFncCustom = class (TObject)
  private
    fScript: TRScriptCustom;
    fInversed: Boolean;
    fOperator: TRFncOperator;
  protected
    function GetResult: Boolean; virtual; abstract;
  public
    constructor Create(Script: TRScriptCustom; const Inversed: Boolean; const Operator: TRFncOperator);
    destructor Destroy; override;
    function  GetFunctionParams_Edit: string; virtual;
    function  GetFunctionParams_Exec: string; virtual;
    function  GetFunctionText_Edit: string; virtual;
    function  GetFunctionText_Exec: string; virtual;
    function  GetFunctionItem_Edit: string; virtual;
    function  GetFunctionItem_Exec: string; virtual;
    function  UpdateVariables(const ScrStr: string): string; virtual;
    procedure CalcResult(var Value: Boolean); virtual;
    property Script: TRScriptCustom read fScript;
    property Inversed: Boolean read fInversed write fInversed;
    property Operator: TRFncOperator read fOperator write fOperator;
  end;

  CRFncExecuted = class of TRFncExecuted;

  TRFncExecuted = class (TRFncCustom)
  private
    fParams: TRCmdParams;
  protected
    procedure CheckParameter(const Param: string); virtual;
  public
    constructor Create(Script: TRScriptCustom; const Inversed: Boolean; const Operator: TRFncOperator);
    destructor Destroy; override;
    function  GetParameter(const Index: Integer): string;
    class function GetCommandName: string; virtual; abstract;
    function  GetCommandNote: string; virtual; abstract;
    function  GetFunctionParams_Edit: string; override;
    function  GetFunctionParams_Exec: string; override;
    function  GetFunctionText_Edit: string; override;
    function  GetFunctionText_Exec: string; override;
    class function GetParamsLimit: Integer; virtual;
    function  GetParamsCount: Integer;
    function  AddParameter(const Param: string): Integer;
    procedure AddParameters(const Params: TRCmdParams);
    procedure CheckParameters;
    property  Parameters: TRCmdParams read fParams write fParams;
  end;

  TRFncBlock = class (TRFncCustom)
  private
    fItems: TObjectList;
    fParent: TRFncBlock;
    function GetItem(const Index: Integer): TRFncCustom;
  protected
    function GetResult: Boolean; override;
    property ParentBlock: TRFncBlock read fParent;
  public
    constructor Create(Script: TRScriptCustom; Parent: TRFncBlock;
      const Inversed: Boolean; const Operator: TRFncOperator);
    destructor Destroy; override;
    function  GetFunctionText_Edit: string; override;
    function  GetFunctionText_Exec: string; override;
    procedure AddFunction(CmdFunc: TRFncCustom);
    procedure Clear;
    property  Items: TObjectList read fItems;
    property  Item[const Index: Integer]: TRFncCustom read GetItem;
  end;

  TRFncCondition = class (TObject)
  private
    fScript: TRScriptCustom;
    fTopBlock: TRFncBlock;
    fActBlock: TRFncBlock;
  public
    constructor Create(Script: TRScriptCustom);
    destructor Destroy; override;
    function  GetResult: Boolean; virtual;
    function  GetConditionText_Edit: string;
    function  GetConditionText_Exec: string;
    procedure Clear;
    procedure BlockBegin(const Inversed: Boolean; const Operator: TRFncOperator);
    procedure AddFunction(CmdFunc: TRFncCustom);
    procedure BlockEnd;
    procedure CheckActBlock;
    property  TopBlock: TRFncBlock read fTopBlock;
  end;

  CRCmdCondition = class of TRCmdCondition;

  TRCmdCondition = class (TRCmdCustom)
  private
    fCondition: TRFncCondition;
    fThenList: TRCmdBlock;
    fElseList: TRCmdBlock;
  public
    constructor Create(Script: TRScriptCustom); override;
    destructor Destroy; override;
    class function GetBlockThen: string; virtual;
    class function GetBlockElse: string; virtual;
    function GetCommandCount: Integer; override;
    function GetCommandParams_Edit: string; override;
    function GetCommandParams_Exec: string; override;
    function GetCommandText_Edit: string; override;
    function GetCommandText_Exec: string; override;
    function GetCommandCode(const OffsetLevel: Integer): string; override;
    property Condition: TRFncCondition read fCondition;
    property ThenList: TRCmdBlock read fThenList;
    property ElseList: TRCmdBlock read fElseList;
  end;

  CRCmdCycle = class of TRCmdCycle;

  TRCmdCycle = class (TRCmdCustom)
  private
    fCycleText: string;
    fCmdList: TRCmdBlock;
  public
    constructor Create(Script: TRScriptCustom); override;
    destructor Destroy; override;
    function GetCycleText: string; virtual;
    function GetCycleEnd: string; virtual;
    function GetCommandCount: Integer; override;
    function GetCommandParams_Edit: string; override;
    function GetCommandParams_Exec: string; override;
    function GetCommandText_Edit: string; override;
    function GetCommandText_Exec: string; override;
    function GetCommandCode(const OffsetLevel: Integer): string; override;
    property CmdList: TRCmdBlock read fCmdList;
    property CycleText: string read fCycleText write fCycleText;
  end;

  TRScriptCustom = class (TComponent)
  private
    fVsInit: Boolean;
    fCbCall: Integer;
    fAlias: string;
    fAttrs: string;
    fCaption: string;
    fCanBreak: Boolean;
    fGuiEnabled: Boolean;
    fExtProcessor: Boolean;
    fTagChar: Char;
    fDateMode: TRScriptDate;
    fDateFixed: TDateTime;
    fCycleMax: Integer;
    fBufferSize: Word;
    fNetUsage: Byte;
    fTryMax: Byte;
    fTryDelay: Word;
    fExtCmdWaitTime: Cardinal;
    fObjList: TStrings;
    fKeyList: TStrings;
    fVarList: TStrings;
    fVarWork: TStrings;
    fUndoList: TRCopyList;
    fScript: TStrings;
    fParsePos: Integer;
    fFirstPos: Integer;
    fLastPos: Integer;
    fProcList: TObjectList;
    fCompiled: Boolean;
    fExecuted: Boolean;
    fBlocksStack: TObjectList;
    fProcsStack: TObjectList;
    fCurrState: TRScriptState;
    fShowInfo: TRShowInfoNotifyEvent;
    fShowState: TRShowStateNotifyEvent;
    fShowProgress: TRShowProgressNotifyEvent;
    fFileProgress: TRFileProgressNotifyEvent;
    fCheckBreak: TRCheckBreakNotifyEvent;
    fUpdateControls: TRUpdateControlsNotifyEvent;
    fExecCmdOnEP: TRExecOnExternalProcessor;
    fCompileStart: TNotifyEvent;
    fCompileEnd: TNotifyEvent;
    fExecuteStart: TNotifyEvent;
    fExecuteEnd: TNotifyEvent;
    fGetNetUsage: TRCalcNetUsageNotifyEvent;
    fGetScriptName: TRGetScriptName;
    procedure SetVariables(const Value: TStrings);
    procedure SetKeywords(const Value: TStrings);
    procedure SetAlias(const Value: string);
    procedure SetAttrs(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetScript(const Value: TStrings);
    function  GetScriptText: string;
    function  GetVarListTask: string;
    function  GetProc(Index: Integer): TRCmdProc;
    function  _IsPosValid(const ChPos: Integer): Boolean;
    function  _Eol(const ChPos: Integer): Boolean;
    function  _Eos(const ChPos: Integer): Boolean;
    function  _Eow(const ChPos: Integer): Boolean;
    function  _Test(const Chars: TSysCharSet; const ChPos: Integer): Boolean;
    function  _Quote(const ChPos: Integer): Boolean;
    function  _DblQuote(const ChPos: Integer): Boolean;
    function  _TriQuote(const ChPos: Integer): Boolean;
    function  _Peek(const SubStr: string; const ChPos: Integer): Boolean;
    function  _PeekComments(const SkipLine: Boolean; var ChPos: Integer): Boolean;
    function  _PeekEnd(const ChPos: Integer): Boolean;
    procedure _CheckEol(var ChPos: Integer);
    procedure _SkipLine(var ChPos: Integer);
    procedure _SkipSpaces(const NextLine: Boolean; var ChPos: Integer);
    procedure _SkipComments(var ChPos: Integer);
    procedure _ClearPositions;
    procedure _StorePositions(const ChPos: Integer);
    function  _ReadSimple(var ChPos: Integer; var Value: string; var bQuoted: Boolean): Boolean;
    procedure _ReadQuoted(var ChPos: Integer; var Value: string; var bQuoted: Boolean);
    procedure  DelObjectSctipt(const ObjName: string);
    function   FindObjectScript(const ObjName: string): TObject;
  protected
    fEditMode: Boolean;
    fComplOpCount: Integer;
    fTotalOpCount: Integer;
    function  CompareToken(const Token, Mask: string): Boolean;
    procedure CheckToken(const Token: string); virtual;
    function  ReadToken(const NewLine, NextLine, CheckTkn: Boolean; var ChPos: Integer): string;
    function  ReadFuncToken(var ChPos: Integer): string;
    function  ReadLine(const NewLine: Boolean; var ChPos: Integer): string;
    function  ReadExeParams(const ParamsLimit: Integer;
      const AliasName: string; var ChPos: Integer): TRCmdData;
    function  ReadExeCommand(CmdType: CRCmdExecuted;
      const AliasName: string; var ChPos: Integer): TRCmdExecuted;
    function  ReadFncParams(CmdType: CRFncExecuted; const Inversed: Boolean;
      const Operator: TRFncOperator; var ChPos: Integer;
      const CheckParameters: Boolean): TRFncExecuted;
    procedure ReadFncCondition(const FncCond: TRFncCondition;
      const CheckCndBlock, CheckParameters: Boolean; var ChPos: Integer);
    function  ReadFncCommand(CmdType: CRCmdCondition;
      const AliasName, ThenToken, ElseToken: string; var ChPos: Integer): TRCmdCondition;
    function  ReadCycCommand(CmdType: CRCmdCycle; const AliasName: string;
      var ChPos: Integer): TRCmdCycle;
    procedure ParseCommand(const Token: string; CmdList: TRCmdBlock;
      const AliasName: string; var ChPos: Integer);
    procedure ParseCommands(CmdList: TRCmdBlock;
      const AliasName: string; var ChPos: Integer; const Simple: Boolean);
    procedure Parse(const AliasName: string);
    function  GetDefaultCmdAttributes: string; virtual;
    function  GetDefaultCmdDescription: string; virtual;
    procedure DoInitVisualComponents; virtual;
    procedure DoDoneVisualComponents; virtual;
    procedure DoCompileStart; virtual;
    procedure DoCompileEnd; virtual;
    procedure DoExecuteStart; virtual;
    procedure DoExecuteEnd; virtual;
    procedure CopyVariables;
    procedure FixCurrentState(const NewState: TRScriptState);
  public
    UserData: Pointer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoShowInfo(Sender: TObject; const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
      const MsgState: TRMsgState; const MsgText: string); virtual;
    procedure DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState); virtual;
    procedure DoShowProgress(Sender: TObject; const MsgClass: TRMsgClass;
      const CurrPos, MaxPos: Integer); virtual;
    procedure DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer); virtual;
    procedure DoCheckBreak(Sender: TObject; var IsBreak: Boolean); virtual;
    procedure DoUpdateControls(Sender: TObject; const BreakEnabled, CloseEnabled: Boolean); virtual;
    procedure DoCalcNetUsage(Sender: TObject; var NetUsage: Byte); virtual;
    procedure DoExecCmdOnEP(Sender: TObject; const CmdType: TRCmdExtType;
      const CmdText: string; const ScriptName: string; const AliasName: string; const RunProcName: string;
      const ShowInfo: TRShowInfoNotifyEvent; const ShowProgress: TRShowProgressNotifyEvent;
      const FileProgress: TRFileProgressNotifyEvent; var State: TRScriptState); virtual;
    function  AddProc(const Name: string): Integer;
    function  CreateProc(const Name: string): TRCmdProc;
    function  FindProc(const Name: string): TRCmdProc;
    function  FindExternalProc(const Name: string): TRCmdProc; virtual;
    function  FindExternalScript(const Alias: string):TRScriptCustom; virtual;
    function  IsExternalProc(const Name: string): Boolean;
    function  GetAliasProcName(const Name: string): string;
    function  GetShortProcName(const Name: string): string;
    function  GetFullProcName(const Alias, Name: string): string;
    function  IndexOf(Proc: TRCmdProc): Integer;
    procedure IncTotalOpCount(const Increment: Integer);
    function  GetCommandCount: Integer;
    function  ScriptStateToMsgState(const State: TRScriptState): TRMsgState;
    procedure AddObject(const ObjType: TRScrObjType; const ObjName: string; const ObjPtr: TObject);
    procedure DelObject(const ObjType: TRScrObjType; const ObjName: string);
    function  FindObject(const ObjType: TRScrObjType; const ObjName: string; const RaiseError: Boolean): TObject;
    procedure CmdBlock_Fix(const CmdBlock: TRCmdBlock);
    procedure CmdBlock_Release;
    function  CmdBlock_Last: TRCmdBlock;
    procedure CmdProc_Fix(const CmdProc: TRCmdProc);
    procedure CmdProc_Release;
    function  CmdProc_Last: TRCmdProc;
    procedure InitVariables; virtual;
    procedure PutVariable(const WorkList: Boolean; const Name, Value: string); overload;
    procedure PutVariable(const WorkList: Boolean; const Variable: string); overload;
    procedure PutVariables(const InitList: Boolean; AVarList: TStrings); overload;
    procedure PutVariables(const InitList: Boolean; AVarList: string); overload;
    function  PosToX(const Pos: Integer): Integer;
    function  PosToY(const Pos: Integer): Integer;
    function  GetScriptTitle: string; virtual;
    function  GetScriptName: string; virtual;
    function  GetScriptDate: TDateTime;
    function  GetScriptTemplate: string;
    function  CreateScriptText: string;
    function  CalcNetUsage: Byte;
    function  Compile: Boolean;
    function  ExecuteLine(const CmdLine: string): TRScriptState;
    function  ExecuteProc(const Name: string; const Forced, ExtMode: Boolean;
      const ExtVariable: string = ''; const DefState: TRScriptState = stOk): TRScriptState;
    procedure HandleError(var State: TRScriptState);
    function  Execute(const CallCompile: Boolean; const RunProc: string = tkMainProc): TRScriptState; virtual;
    function  BreakBlock: Boolean;
    procedure BreakScript;
    procedure RestoreFiles;
    property Script: TStrings read fScript write SetScript;
    property ScriptText: string read GetScriptText;
    property VarListExec: TStrings read fVarWork;
    property VarListTask: string read GetVarListTask;
    property RestoreList: TRCopyList read fUndoList;
    property Compiled: Boolean read fCompiled;
    property Executed: Boolean read fExecuted;
    property Item[Index: Integer]: TRCmdProc read GetProc; default;
    property ParsePosition: Integer read fParsePos;
    property FirstSymbol: Integer read fFirstPos;
    property LastSymbol: Integer read fLastPos;
    property CurrentState: TRScriptState read fCurrState;
  published
    property Alias: string read fAlias write SetAlias;
    property Attributes: string read fAttrs write SetAttrs;
    property BufferSizeKb: Word read fBufferSize write fBufferSize default 64;
    property GuiEnabled: Boolean read fGuiEnabled write fGuiEnabled default True;
    property ExternalProcessor: Boolean read fExtProcessor write fExtProcessor default False;
    property CanBreak: Boolean read fCanBreak write fCanBreak default False;
    property Caption: string read fCaption write SetCaption;
    property CycleMax: Integer read fCycleMax write fCycleMax default 10;
    property DateMode: TRScriptDate read fDateMode write fDateMode default dmFullAuto;
    property DateFixed: TDateTime read fDateFixed write fDateFixed;
    property NetUsage: Byte read fNetUsage write fNetUsage default 100;
    property TagsChar: Char read fTagChar write fTagChar default '%';
    property TryMax: Byte read fTryMax write fTryMax default 3;
    property TryDelay: Word read fTryDelay write fTryDelay default 3;
    property ExtCmdWaitTime: Cardinal read fExtCmdWaitTime write fExtCmdWaitTime default DefMaxWaitTime;
    property Keywords: TStrings read fKeyList write SetKeywords;
    property Variables: TStrings read fVarList write SetVariables;
    property OnShowInfo: TRShowInfoNotifyEvent read fShowInfo write fShowInfo;
    property OnShowState: TRShowStateNotifyEvent read fShowState write fShowState;
    property OnShowProgress: TRShowProgressNotifyEvent read fShowProgress write fShowProgress;
    property OnFileProgress: TRFileProgressNotifyEvent read fFileProgress write fFileProgress;
    property OnCheckBreak: TRCheckBreakNotifyEvent read fCheckBreak write fCheckBreak;
    property OnUpdateControls: TRUpdateControlsNotifyEvent read fUpdateControls write fUpdateControls;
    property OnExecCmdOnEP: TRExecOnExternalProcessor read fExecCmdOnEP write fExecCmdOnEP;
    property OnCompileStart: TNotifyEvent read fCompileStart write fCompileStart;
    property OnCompileEnd: TNotifyEvent read fCompileEnd write fCompileEnd;
    property OnExecuteStart: TNotifyEvent read fExecuteStart write fExecuteStart;
    property OnExecuteEnd: TNotifyEvent read fExecuteEnd write fExecuteEnd;
    property OnCalcNetUsage: TRCalcNetUsageNotifyEvent read fGetNetUsage write fGetNetUsage;
    property OnGetScriptName: TRGetScriptName read fGetScriptName write fGetScriptName;
  end;

  TRScriptLogger = class (TComponent)
  private
    (* fLogBuffer: TRCmdBuffer; *)
    (* fLogMode: TRLogMode; *)
    (* fLogSize: Word; *)
    fBreak: Boolean;
    fShowInfo: TRShowInfoNotifyEvent;
    fSaveToLog: TRSaveToLogNotifyEvent;
    fShowState: TRShowStateNotifyEvent;
    (* fStoreMessage: TRStoreMessageNotifyEvent; *)
    (* fStoreBuffer: TRStoreBufferNotifyEvent; *)
    fShowProgress: TRShowProgressNotifyEvent;
    fFileProgress: TRFileProgressNotifyEvent;
    fCheckBreak: TRCheckBreakNotifyEvent;
    fUpdateControls: TRUpdateControlsNotifyEvent;
    fInitVisual: TNotifyEvent;
    fDoneVisual: TNotifyEvent;
    fCompileStart: TNotifyEvent;
    fCompileEnd: TNotifyEvent;
    fExecuteStart: TNotifyEvent;
    fExecuteEnd: TNotifyEvent;
  protected
    procedure DoInitVisualComponents(Sender: TObject); virtual;
    procedure DoDoneVisualComponents(Sender: TObject); virtual;
    procedure DoCompileStart(Sender: TObject); virtual;
    procedure DoCompileEnd(Sender: TObject); virtual;
    procedure DoExecuteStart(Sender: TObject; const BreakEnabled: Boolean); virtual;
    procedure DoExecuteEnd(Sender: TObject); virtual;
  public
    UserData: Pointer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (*
    procedure PutCmdMessage(const ScriptId: Integer;
      const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
      const MsgState: TRMsgState; const MsgText: string);
    procedure FlushLogMessages;
    *)
    procedure DoShowInfo(Sender: TObject; const TimeStamp: TDateTime;
      const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string); virtual;
    procedure DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState); virtual;
    procedure DoShowProgress(Sender: TObject; const MsgClass: TRMsgClass;
      const CurrPos, MaxPos: Integer); virtual;
    procedure DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer); virtual;
    procedure DoCheckBreak(Sender: TObject; var IsBreak: Boolean); virtual;
    procedure DoUpdateControls(Sender: TObject; const BreakEnabled, CloseEnabled: Boolean); virtual;
    procedure BreakScript;
  published
    (* property LogMode: TRLogMode read fLogMode write fLogMode default lmDisabled; *)
    (* property LogSize: Word read fLogSize write fLogSize default 32; *)
    property OnShowInfo: TRShowInfoNotifyEvent read fShowInfo write fShowInfo;
    property OnSaveToLog: TRSaveToLogNotifyEvent read fSaveToLog write fSaveToLog;
    property OnShowState: TRShowStateNotifyEvent read fShowState write fShowState;
    (* property OnStoreMessage: TRStoreMessageNotifyEvent read fStoreMessage write fStoreMessage; *)
    (* property OnStoreBuffer: TRStoreBufferNotifyEvent read fStoreBuffer write fStoreBuffer; *)
    property OnShowProgress: TRShowProgressNotifyEvent read fShowProgress write fShowProgress;
    property OnFileProgress: TRFileProgressNotifyEvent read fFileProgress write fFileProgress;
    property OnCheckBreak: TRCheckBreakNotifyEvent read fCheckBreak write fCheckBreak;
    property OnUpdateControls: TRUpdateControlsNotifyEvent read fUpdateControls write fUpdateControls;
    property OnInitVisual: TNotifyEvent read fInitVisual write fInitVisual;
    property OnDoneVisual: TNotifyEvent read fDoneVisual write fDoneVisual;
    property OnCompileStart: TNotifyEvent read fCompileStart write fCompileStart;
    property OnCompileEnd: TNotifyEvent read fCompileEnd write fCompileEnd;
    property OnExecuteStart: TNotifyEvent read fExecuteStart write fExecuteStart;
    property OnExecuteEnd: TNotifyEvent read fExecuteEnd write fExecuteEnd;
  end;

  TRScript = class (TRScriptCustom)
  private
    fLogger: TRScriptLogger;
    procedure SetLogger(const Value: TRScriptLogger);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckToken(const Token: string); override;
    procedure DoInitVisualComponents; override;
    procedure DoDoneVisualComponents; override;
    procedure DoCompileStart; override;
    procedure DoCompileEnd; override;
    procedure DoExecuteStart; override;
    procedure DoExecuteEnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoShowInfo(Sender: TObject; const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
      const MsgState: TRMsgState; const MsgText: string); override;
    procedure DoShowProgress(Sender: TObject; const MsgClass: TRMsgClass;
      const CurrPos, MaxPos: Integer); override;
    procedure DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer); override;
    procedure DoCheckBreak(Sender: TObject; var IsBreak: Boolean); override;
    procedure DoUpdateControls(Sender: TObject; const BreakEnabled, CloseEnabled: Boolean); override;
  published
    property Logger: TRScriptLogger read fLogger write SetLogger;
    property Script;
  end;

  TRScriptEditor = class (TRScriptCustom)
  private
    fLogger: TRScriptLogger;
    fEditor: TCustomMemo;
    fEnableEdit: Boolean;
    fEnableAddVar: Boolean;
    fEnableAddStr: Boolean;
    fSelectVariable: TSelectVariableNotifyEvent;
    procedure SetLogger(const Value: TRScriptLogger);
    procedure SetEditor(const Value: TCustomMemo);
    (* procedure _DebugShow(const Tag: string; const CurrPos: Integer); *)
    function  _LineStart(const SkipSpaces: Boolean; const CurrPos: Integer): Integer;
    function  _LineEnd(const CurrPos: Integer): Integer;
    function  _LinePrevious(const CurrPos: Integer): Integer;
    function  _PeakToken(var ChPos: Integer): TRCmdTokenType;
    function  TextCommandExe(const CmdClass: TClass; const CmdData: TRCmdData): string;
    function  TextVariable(const VarName, VarValue: string): string;
    function  SelectVariableDefault(var VarName: string): Boolean;
    procedure SelectErrorLine(const ErrPos: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CheckToken(const Token: string); override;
    procedure DoInitVisualComponents; override;
    procedure DoDoneVisualComponents; override;
    procedure DoCompileStart; override;
    procedure DoCompileEnd; override;
    procedure DoExecuteStart; override;
    procedure DoExecuteEnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoShowInfo(Sender: TObject; const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
      const MsgState: TRMsgState; const MsgText: string); override;
    procedure DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState); override;
    procedure DoFileProgress(Sender: TObject;
      const CurrPos, MaxPos: Integer); override;
    procedure DoShowProgress(Sender: TObject; const MsgClass: TRMsgClass;
      const CurrPos, MaxPos: Integer); override;
    procedure DoCheckBreak(Sender: TObject; var IsBreak: Boolean); override;
    procedure DoUpdateControls(Sender: TObject; const BreakEnabled, CloseEnabled: Boolean); override;
    function   SelectVariableName(const Tags: Boolean; var VarName: string): Boolean;
    procedure  UpdateControls;
    function   EditEnabled: Boolean;
    function   VarsEnabled: Boolean;
    function   ProcEnabled: Boolean;
    procedure  EditCommandExe(var ChPos: Integer);
    procedure  EditCommandVar(var ChPos: Integer);
    procedure  EditCommandCyc(var ChPos: Integer);
    procedure  EditCondition(var ChPos: Integer);
    procedure  EditVarList(var ChPos: Integer);
    procedure  Edit;
    procedure  InsertNewLine;
    procedure  AddCommandExe;
    procedure  AddCommandVar;
    procedure  AddCommandCyc;
    procedure  AddCondition;
    procedure  AddVarList;
  published
    property Logger: TRScriptLogger read fLogger write SetLogger;
    property Editor: TCustomMemo read fEditor write SetEditor;
    property OnSelectVariable: TSelectVariableNotifyEvent read fSelectVariable write fSelectVariable;
  end;

  TRScriptItem = class (TRScript)
  private
    fId: Integer;
    fRunState: TRScrRunState;
    fChkState: TRScrResState;
    fScrList: TRScripts;
    procedure SetScrList(const Value: TRScripts);
    procedure SetScriptId(const Value: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoInitVisualComponents; override;
    procedure DoDoneVisualComponents; override;
    procedure DoCompileStart; override;
    procedure DoCompileEnd; override;
    procedure DoExecuteStart; override;
    procedure DoExecuteEnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitVariables; override;
    function  GetScriptTitle: string; override;
    procedure DoShowInfo(Sender: TObject; const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
      const MsgState: TRMsgState; const MsgText: string); override;
    procedure DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState); override;
    procedure DoShowProgress(Sender: TObject; const MsgClass: TRMsgClass;
      const CurrPos, MaxPos: Integer); override;
    procedure DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer); override;
    procedure DoCheckBreak(Sender: TObject; var IsBreak: Boolean); override;
    procedure DoUpdateControls(Sender: TObject; const BreakEnabled, CloseEnabled: Boolean); override;
    procedure DoCalcNetUsage(Sender: TObject; var NetUsage: Byte); override;
    procedure DoExecCmdOnEP(Sender: TObject; const CmdType: TRCmdExtType;
      const CmdText: string; const ScriptName: string; const AliasName: string; const RunProcName: string;
      const ShowInfo: TRShowInfoNotifyEvent; const ShowProgress: TRShowProgressNotifyEvent;
      const FileProgress: TRFileProgressNotifyEvent; var State: TRScriptState); override;
    function FindExternalScript(const Alias: string): TRScriptCustom; override;
    function FindExternalProc(const Name: string): TRCmdProc; override;
    property ScriptId: Integer read fId write SetScriptId default 0;
    property RunState: TRScrRunState read fRunState write fRunState default rsWarning;
    property ChkState: TRScrResState read fChkState write fChkState default rsApply;
    property ScrList: TRScripts read fScrList write SetScrList;
  end;

  TRScripts = class (TComponent)
  private
    fVsInit: Boolean;
    fGuiEnabled: Boolean;
    fExtProcessor: Boolean;
    fScrList: TObjectList;
    fTagChar: Char;
    fDateMode: TRScriptDate;
    fDateFixed: TDateTime;
    fCycleMax: Integer;
    fBufferSize: Word;
    fNetUsage: Byte;
    fTryMax: Byte;
    fTryDelay: Word;
    fExtCmdWaitTime: Cardinal;
    fKeyList: TStrings;
    fVarList: TStrings;
    fBreak: Boolean;
    fTaskName: string;
    fLogger: TRScriptLogger;
    fShowInfo: TRShowInfoNotifyEvent;
    fShowState: TRShowStateNotifyEvent;
    fShowProgress: TRShowProgressNotifyEvent;
    fFileProgress: TRFileProgressNotifyEvent;
    fCheckBreak: TRCheckBreakNotifyEvent;
    fUpdateControls: TRUpdateControlsNotifyEvent;
    fLoadExtScript: TLoadExtScriptNotifyEvent;
    fExecCmdOnEP: TRExecOnExternalProcessor;
    fInitVisual: TNotifyEvent;
    fDoneVisual: TNotifyEvent;
    fCompileStart: TNotifyEvent;
    fCompileEnd: TNotifyEvent;
    fExecuteStart: TNotifyEvent;
    fExecuteEnd: TNotifyEvent;
    fTotalExecuteStart: TNotifyEvent;
    fTotalExecuteEnd: TNotifyEvent;
    fGetNetUsage: TRCalcNetUsageNotifyEvent;
    fGetTaskName: TRGetScriptName;
    procedure SetBufferSize(const Value: Word);
    procedure SetCycleMax(const Value: Integer);
    procedure SetDateFixed(const Value: TDateTime);
    procedure SetDateMode(const Value: TRScriptDate);
    procedure SetNetUsage(const Value: Byte);
    procedure SetTagChar(const Value: Char);
    procedure SetTryDelay(const Value: Word);
    procedure SetTryMax(const Value: Byte);
    procedure SetKeywords(const Value: TStrings);
    procedure SetVariables(const Value: TStrings);
    function  GetScript(Index: Integer): TRScriptItem;
    procedure SetLogger(const Value: TRScriptLogger);
    procedure SetExtCmdWaitTime(const Value: Cardinal);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoInitVisualComponents(Sender: TObject); virtual;
    procedure DoDoneVisualComponents(Sender: TObject); virtual;
    procedure DoCompileStart(Sender: TObject); virtual;
    procedure DoCompileEnd(Sender: TObject); virtual;
    procedure DoExecuteStart(Sender: TObject; const BreakEnabled: Boolean); virtual;
    procedure DoExecuteEnd(Sender: TObject); virtual;
    procedure DoTotalExecuteStart(Sender: TObject); virtual;
    procedure DoTotalExecuteEnd(Sender: TObject); virtual;
  public
    UserData: Pointer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoShowInfo(Sender: TObject; const TimeStamp: TDateTime;
      const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string);
    procedure DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState);
    procedure DoShowProgress(Sender: TObject; const MsgClass: TRMsgClass;
      const CurrPos, MaxPos: Integer);
    procedure DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer);
    procedure DoCheckBreak(Sender: TObject; var IsBreak: Boolean); virtual;
    procedure DoUpdateControls(Sender: TObject;
      const BreakEnabled, CloseEnabled: Boolean); virtual;
    procedure DoCalcNetUsage(Sender: TObject; var NetUsage: Byte); virtual;
    procedure DoExecCmdOnEP(Sender: TObject; const CmdType: TRCmdExtType;
      const CmdText: string; const ScriptName: string; const AliasName: string; const RunProcName: string;
      const ShowInfo: TRShowInfoNotifyEvent; const ShowProgress: TRShowProgressNotifyEvent;
      const FileProgress: TRFileProgressNotifyEvent; var State: TRScriptState); virtual;
    function  Add(Script: TRScriptItem): Integer;
    function  AddScript(const AScript: string; const ACanBreak: Boolean;
      const ARunState: TRScrRunState; const AChkState: TRScrResState;
      const AId: Integer = 0; const ACaption: string = ''; const AAlias: string = '';
      const AAttrs: string = ''; const AVariables: string = ''): TRScriptItem; overload;
    function  AddScript(const AScript: string; const ACanBreak: Boolean;
      const ARunState: TRScrRunState; const AChkState: TRScrResState;
      const AId: Integer = 0; const ACaption: string = ''; const AAlias: string = '';
      const AAttrs: string = ''; const AVariables: TStrings = nil): TRScriptItem; overload;
    procedure Clear;
    procedure Delete(const Index: Integer);
    procedure InitVariables;
    procedure PutVariable(const Name, Value: string); overload;
    procedure PutVariable(const Variable: string); overload;
    procedure PutVariables(VarList: TStrings); overload;
    procedure PutVariables(VarList: string); overload;
    function  LoadExternalScript(Script: TRScriptCustom; const Alias: string): TRScriptItem;
    function  FindScript(const Alias: string): TRScriptItem;
    function  GetTaskName: string; virtual;
    function  CalcNetUsage: Byte;
    function  Compile: Boolean;
    procedure BreakScript;
    function  Execute(const CallCompile: Boolean): TRScriptState;
    property  Script[Index: Integer]: TRScriptItem read GetScript;
  published
    property TaskName: string read fTaskName write fTaskName;
    property GuiEnabled: Boolean read fGuiEnabled write fGuiEnabled default True;
    property ExternalProcessor: Boolean read fExtProcessor write fExtProcessor default False;
    property BufferSizeKb: Word read fBufferSize write SetBufferSize default 64;
    property CycleMax: Integer read fCycleMax write SetCycleMax default 10;
    property DateMode: TRScriptDate read fDateMode write SetDateMode default dmFullAuto;
    property DateFixed: TDateTime read fDateFixed write SetDateFixed;
    property NetUsage: Byte read fNetUsage write SetNetUsage default 100;
    property TagsChar: Char read fTagChar write SetTagChar default '%';
    property TryMax: Byte read fTryMax write SetTryMax default 3;
    property TryDelay: Word read fTryDelay write SetTryDelay default 3;
    property ExtCmdWaitTime: Cardinal read fExtCmdWaitTime write SetExtCmdWaitTime default DefMaxWaitTime;
    property Logger: TRScriptLogger read fLogger write SetLogger;
    property Keywords: TStrings read fKeyList write SetKeywords;
    property Variables: TStrings read fVarList write SetVariables;
    property OnShowInfo: TRShowInfoNotifyEvent read fShowInfo write fShowInfo;
    property OnShowState: TRShowStateNotifyEvent read fShowState write fShowState;
    property OnShowProgress: TRShowProgressNotifyEvent read fShowProgress write fShowProgress;
    property OnFileProgress: TRFileProgressNotifyEvent read fFileProgress write fFileProgress;
    property OnCheckBreak: TRCheckBreakNotifyEvent read fCheckBreak write fCheckBreak;
    property OnUpdateControls: TRUpdateControlsNotifyEvent read fUpdateControls write fUpdateControls;
    property OnLoadExtScript: TLoadExtScriptNotifyEvent read fLoadExtScript write fLoadExtScript;
    property OnExecCmdOnEP: TRExecOnExternalProcessor read fExecCmdOnEP write fExecCmdOnEP;
    property OnInitVisual: TNotifyEvent read fInitVisual write fInitVisual;
    property OnDoneVisual: TNotifyEvent read fDoneVisual write fDoneVisual;
    property OnCompileStart: TNotifyEvent read fCompileStart write fCompileStart;
    property OnCompileEnd: TNotifyEvent read fCompileEnd write fCompileEnd;
    property OnExecuteStart: TNotifyEvent read fExecuteStart write fExecuteStart;
    property OnExecuteEnd: TNotifyEvent read fExecuteEnd write fExecuteEnd;
    property OnTotalExecuteStart: TNotifyEvent read fTotalExecuteStart write fTotalExecuteStart;
    property OnTotalExecuteEnd: TNotifyEvent read fTotalExecuteEnd write fTotalExecuteEnd;
    property OnCalcNetUsage: TRCalcNetUsageNotifyEvent read fGetNetUsage write fGetNetUsage;
    property OnGetTaskName: TRGetScriptName read fGetTaskName write fGetTaskName;
  end;

procedure Register;

implementation

uses
  DateUtils, RScrCmds, RVclUtils, RScrFncs, RVarListEx, RDialogs,
  RScriptsCmd, RScriptsCnd, RScriptsVar, RScriptsCyc, RVarListSelect;

{ Register }

procedure Register;
begin
  RegisterComponents('Rav Soft', [TRScript, TRScripts, TRScriptLogger, TRScriptEditor]);
end;

{ TRCmdCustom }

constructor TRCmdCustom.Create(Script: TRScriptCustom);
begin
  inherited Create;
  fScript := Script;
  fLocked := False;
  fExtExec := False;
  fDescription := EmptyStr;
  fErrFlags := [];
end;

destructor TRCmdCustom.Destroy;
begin
  fScript := nil;
  inherited Destroy;
end;

function TRCmdCustom.GetAttributes: string;
var
  Attr: TRCmdErrAttr;
begin
  if fLocked then
    Result := SRCmdLock
  else
    Result := EmptyStr;
  if fScript.fExtProcessor and fExtExec then
    Result := Result + SRCmdExtExec;
  for Attr := Low(TRCmdErrAttr) to High(TRCmdErrAttr) do
    if Attr in fErrFlags then
      Result := Result + SRCmdErrAttr[Attr];
end;

procedure TRCmdCustom.SetAttributes(const Attributes: string);
var
  Attr: TRCmdErrAttr;
begin
  fErrFlags := [];
  fLocked := Pos(SRCmdLock, Attributes) > 0;
  if fScript.fExtProcessor then
    fExtExec := Pos(SRCmdExtExec, Attributes) > 0
  else
    fExtExec := False;
  for Attr := Low(TRCmdErrAttr) to High(TRCmdErrAttr) do
    if Pos(SRCmdErrAttr[Attr], AnsiUpperCase(Attributes)) > 0 then
      fErrFlags := fErrFlags + [Attr];
end;

function TRCmdCustom.GetCommandAttrList: string;
var
  Attr: TRCmdErrAttr;
begin
  Result := SRCmdLock;
  if fScript.fExtProcessor then
    Result := Result + SRCmdExtExec;
  for Attr := Low(TRCmdErrAttr) to High(TRCmdErrAttr) do
    Result := Result + SRCmdErrAttr[Attr];
end;

function TRCmdCustom.GetCommandAttrNote(const Attr: Char): string;
var
  Cnt: TRCmdErrAttr;
begin
  Result := EmptyStr;
  if AnsiUpperCase(Attr) = SRCmdLock then
    Result := SRCmdErrLockNote
  else begin
    if AnsiUpperCase(Attr) = SRCmdExtExec then
      Result := SRCmdExtExecNote
    else begin
      for Cnt := Low(TRCmdErrAttr) to High(TRCmdErrAttr) do
        if AnsiUpperCase(Attr) = SRCmdErrAttr[Cnt] then
        begin
          Result := SRCmdErrAttrNote[Cnt];
          Break;
        end;
    end;
  end;
end;

function TRCmdCustom.GetCommandCount: Integer;
begin
  Result := 1;
end;

function TRCmdCustom.GetCommandParams_Edit: string;
begin
  Result := EmptyStr;
end;

function TRCmdCustom.GetCommandParams_Exec: string;
begin
  Result := EmptyStr;
end;

function TRCmdCustom.GetCommandDescription: string;
begin
  Result := UpdateVariables(fDescription);
end;

function TRCmdCustom.GetCommandText_Edit: string;
begin
  Result := GetCommandName + #32
    + GetCommandParams_Edit + #32
    + chAttrBegin + GetAttributes + chAttrEnd + #32
    + ScriptQuotedStr(fDescription);
end;

function TRCmdCustom.GetCommandText_Exec: string;
begin
  Result := GetCommandName + #32
    + GetCommandParams_Exec + #32
    + chAttrBegin + GetAttributes + chAttrEnd + #32
    + ScriptQuotedStr(UpdateVariables(fDescription));
end;

function TRCmdCustom.GetCommandCode(const OffsetLevel: Integer): string;
begin
  Result := StringOfChar(#32, OffsetLevel * 2)
    + GetCommandName + #32
    + GetCommandParams_Edit + #32
    + chAttrBegin + GetAttributes + chAttrEnd + #32
    + ScriptQuotedStr(fDescription);
end;

function TRCmdCustom.UpdateVariables(const ScrStr: string): string;
begin
  if Assigned(fScript)
  then Result := ReplaceTags(fScript.fVarWork, ScrStr, True, fScript.GetScriptDate, fScript.TagsChar)
  else Result := ScrStr;
end;

procedure TRCmdCustom.ExecOnExtProcessor(var OpState: TRScriptState);
begin
  fScript.DoExecCmdOnEP(fScript, ctCommand, GetCommandText_Exec,
    fScript.GetScriptName, EmptyStr, EmptyStr,
    fScript.DoShowInfo, fScript.DoShowProgress,
    fScript.DoFileProgress, OpState);
end;

procedure TRCmdCustom.Execute(var State: TRScriptState);
var
  OpState: TRScriptState;
begin
  if fScript.Executed then
  begin
    if not (fExtExec and fScript.fExtProcessor) then
    begin
      fScript.DoShowInfo(fScript, Now, mcCommand, msTitle, GetCommandDescription);
      fScript.DoShowInfo(fScript, Now, mcCommand, msInfo, GetCommandText_Exec);
      fScript.DoUpdateControls(fScript, fScript.fCanBreak and not fLocked, False);
    end
    else fScript.DoUpdateControls(fScript, False, False);
    fScript.DoShowProgress(fScript, mcTransaction, 0, 1);
    OpState := stOk;
    try
      try
        if fExtExec and fScript.fExtProcessor
        then ExecOnExtProcessor(OpState)
        else ExecuteCommand(OpState);
      except
        on E: Exception do
        begin
          OpState := stError;
          fScript.DoShowInfo(fScript, Now, mcCommand, msError, Format(EExecuteCommand, [E.Message]));
        end;
      end;
    finally
      if OpState in [stError, stErrorStop, stErrorRestore, stErrorUndo] then
        Script.HandleError(OpState);
      if OpState in [stError, stErrorStop, stErrorRestore, stErrorUndo] then
      begin
        OpState := stError;
        if not (eaContinue in fErrFlags) then
          OpState := stErrorStop;
        if eaRestore in fErrFlags then
          OpState := stErrorRestore;
        if eaRunUndo in fErrFlags then
          OpState := stErrorUndo;
      end;
      if OpState > State then
        State := OpState;
      fScript.DoShowProgress(fScript, mcTransaction, 1, 1);
    end;
  end;
end;

{ TRCmdExecuted }

constructor TRCmdExecuted.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  SetLength(fParams, 0);
end;

destructor TRCmdExecuted.Destroy;
begin
  SetLength(fParams, 0);
  inherited Destroy;
end;

class function TRCmdExecuted.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRCmdExecuted.GetParamsCount: Integer;
begin
  Result := Length(fParams);
end;

function TRCmdExecuted.AddParameter(const Param: string): Integer;
begin
  SetLength(fParams, Length(fParams) + 1);
  Result := High(fParams);
  fParams[Result] := Param;
end;

procedure TRCmdExecuted.AddParameters(const AParams: TRCmdParams);
var
  i: Integer;
begin
  SetLength(fParams, Length(AParams));
  for i := Low(AParams) to High(AParams) do
    fParams[i] := AParams[i];
  CheckParameters;
end;

procedure TRCmdExecuted.CheckParameter(const Param: string);
begin
  // UpdateVariables(Param);
end;

procedure TRCmdExecuted.CheckParameters;
var
  i: Integer;
begin
  if (GetParamsLimit > -1) and (GetParamsCount <> GetParamsLimit) then
    raise ERScriptError.CreateFmt(EParseInvalidParamsCount,
      [GetParamsCount, GetParamsLimit]);
  for i := Low(fParams) to High(fParams) do
    CheckParameter(fParams[i]);
end;

function TRCmdExecuted.GetParameter(const Index: Integer): string;
begin
  Result := EmptyStr;
  if Index in [Low(fParams)..High(fParams)] then
    Result := UpdateVariables(fParams[Index])
  else raise
    ERScriptError.CreateFmt(EParseInvalidParamsNumber, [Index]);
end;

function TRCmdExecuted.GetCommandParams_Edit: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := Low(fParams) to High(fParams) do
    if Result = EmptyStr
    then Result := ScriptQuotedStr(fParams[i])
    else Result := Result + #32 + ScriptQuotedStr(fParams[i]);
end;

function TRCmdExecuted.GetCommandParams_Exec: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := Low(fParams) to High(fParams) do
    if Result = EmptyStr
    then Result := ScriptQuotedStr(UpdateVariables(fParams[i]))
    else Result := Result + #32 + ScriptQuotedStr(UpdateVariables(fParams[i]));
end;

{ TRCmdBlock }

constructor TRCmdBlock.Create(Script: TRScriptCustom);
begin
  inherited Create;
  fScript := Script;
  fCmdList := TObjectList.Create(True);
  fLockOperationProgress := False;
  fLockIncTotalOpCount := False;
end;

destructor TRCmdBlock.Destroy;
begin
  fCmdList.Clear;
  fCmdList.Free;
  fScript := nil;
  inherited Destroy;
end;

procedure TRCmdBlock.Clear;
begin
  fCmdList.Clear;
end;

function TRCmdBlock.IndexOf(Command: TRCmdCustom): Integer;
begin
  Result := fCmdList.IndexOf(Command);
end;

function TRCmdBlock.GetCommandCount: Integer;
var
  i, iCount: Integer;
begin
  Result := 0;
  iCount := fCmdList.Count - 1;
  for i := 0 to iCount do
    Inc(Result, Item[i].GetCommandCount);
end;

function TRCmdBlock.GetItem(Index: Integer): TRCmdCustom;
begin
  Result := fCmdList[Index] as TRCmdCustom;
end;

function TRCmdBlock.Add(Command: TRCmdCustom): Integer;
begin
  if Assigned(Command)
  then Result := fCmdList.Add(Command)
  else Result := -1;
end;

procedure TRCmdBlock.Execute(var State: TRScriptState);
var
  i, iCount: Integer;
begin
  if fScript.Executed and (State in [stOk, stWarning, stError]) then
  begin
    // fScript.CmdBlock_Fix(Self); 2011-08-04 fix bug - не выходил из цикла по команде Break
    try
      fIntBreak := False;
      if not fLockIncTotalOpCount then
        fScript.IncTotalOpCount(GetCommandCount);
      if not fLockOperationProgress then
        fScript.DoShowProgress(fScript, mcOperation, fScript.fComplOpCount, fScript.fTotalOpCount);
      iCount := fCmdList.Count - 1;
      for i := 0 to iCount do
      begin
        Item[i].Execute(State);
        fScript.FixCurrentState(State);
        Inc(fScript.fComplOpCount, Item[i].GetCommandCount);
        if not fLockOperationProgress then
          fScript.DoShowProgress(fScript, mcOperation, fScript.fComplOpCount, fScript.fTotalOpCount);
        // 2011-11-25 fixed bug id 1882 (После команды Exit скрипт в любом случае "вываливался" в ошибку)
        // if not fScript.CheckExecuted then
        //  State := stErrorStop;
        // 2011-11-25 fixed bug id 1882
        if not fScript.Executed or (fIntBreak or (State in [stErrorStop, stErrorRestore, stErrorUndo])) then
          Break;
      end;
    finally
      // fScript.CmdBlock_Release; 2011-08-04 fix bug - не выходил из цикла по команде Break
    end;
  end;
end;

function TRCmdBlock.GetCommandsCode(const OffsetLevel: Integer): string;
var
  i, iCount: Integer;
begin
  Result := EmptyStr;
  iCount := fCmdList.Count - 1;
  for i := 0 to iCount do
    if Result = EmptyStr
    then Result := Item[i].GetCommandCode(OffsetLevel)
    else Result := Result + #13#10 + Item[i].GetCommandCode(OffsetLevel);
end;

procedure TRCmdBlock.BreakBlock;
begin
  fIntBreak := True;
end;

{ TRCmdProc }

constructor TRCmdProc.Create(Script: TRScriptCustom; const AName: string);
begin
  inherited Create;
  fScript := Script;
  fName := AName;
  fCmdList := TRCmdBlock.Create(Script);
  fObjStack := TObjectList.Create;
end;

destructor TRCmdProc.Destroy;
begin
  fObjStack.Clear;
  fObjStack.Free;
  fCmdList.Clear;
  fCmdList.Free;
  fScript := nil;
  inherited Destroy;
end;

function TRCmdProc.GetCommandCount: Integer;
begin
  Result := fCmdList.GetCommandCount;
end;

function TRCmdProc.GetCommandsCode: string;
begin
  Result := fCmdList.GetCommandsCode(1);
end;

procedure TRCmdProc.Execute(var State: TRScriptState);
begin
  CreateObjList;
  try
    fScript.CmdProc_Fix(Self);
    try
      fCmdList.Execute(State);
    finally
      fScript.CmdProc_Release;
    end;
  finally
    DeleteObjList;
  end;
end;

procedure TRCmdProc.CreateObjList;
begin
  fObjStack.Insert(0, TStringList.Create);
end;

procedure TRCmdProc.DeleteObjList;
begin
  if fObjStack.Count > 0 then
    fObjStack.Delete(0);
end;

function TRCmdProc.GetCurrList: TStringList;
begin
  Result := nil;
  if fObjStack.Count > 0 then
    Result := TStringList(fObjStack.Items[0]);
  if Result = nil then
    raise ERScriptError.Create(EScriptObjListNotFound);
end;

procedure TRCmdProc.AddObject(const ObjName: string; const ObjPtr: TObject);
begin
  GetCurrList.AddObject(ObjName, ObjPtr);
end;

procedure TRCmdProc.DelObject(const ObjName: string);
var
  i, iCount, Found: Integer;
  ObjList: TStringList;
  Tmp: TObject;
begin
  Found := -1;
  ObjList := GetCurrList;
  iCount := ObjList.Count - 1;
  for i := 0 to iCount do
    if SameText(ObjName, ObjList[i]) then
    begin
      Found := i;
      Break;
    end;
  if Found = -1 then
    raise ERScriptError.CreateFmt(EScriptObjectNotFound, [ObjName])
  else begin
    Tmp := ObjList.Objects[Found];
    Tmp.Free;
    ObjList.Objects[Found] := nil;
    ObjList.Delete(Found);
  end;
end;

function TRCmdProc.FindObject(const ObjName: string): TObject;
var
  i, iCount: Integer;
  ObjList: TStringList;
begin
  Result := nil;
  ObjList := GetCurrList;
  iCount := ObjList.Count - 1;
  for i := 0 to iCount do
    if SameText(ObjName, ObjList[i]) then
    begin
      Result := ObjList.Objects[i];
      Break;
    end;
end;

{ TRFncCustom }

constructor TRFncCustom.Create(Script: TRScriptCustom;
  const Inversed: Boolean; const Operator: TRFncOperator);
begin
  inherited Create;
  fScript := Script;
  fInversed := Inversed;
  fOperator := Operator;
end;

destructor TRFncCustom.Destroy;
begin
  fScript := nil;
  inherited Destroy;
end;

function TRFncCustom.GetFunctionParams_Edit: string;
begin
  Result := EmptyStr;
end;

function TRFncCustom.GetFunctionParams_Exec: string;
begin
  Result := EmptyStr;
end;

function TRFncCustom.GetFunctionText_Edit: string;
begin
  Result := EmptyStr;
end;

function TRFncCustom.GetFunctionText_Exec: string;
begin
  Result := EmptyStr;
end;

function TRFncCustom.GetFunctionItem_Edit: string;
begin
  Result := GetFunctionText_Edit;
  if fInversed then Result := tkNot + #32 + Result;
  case fOperator of
    coAnd: Result := tkAnd + #32 + Result;
    coOr: Result := tkOr + #32 + Result;
  end;
end;

function TRFncCustom.GetFunctionItem_Exec: string;
begin
  Result := GetFunctionText_Exec;
  if fInversed then Result := tkNot + #32 + Result;
  case fOperator of
    coAnd: Result := tkAnd + #32 + Result;
    coOr: Result := tkOr + #32 + Result;
  end;
end;

(*
function TRFncCustom.GetFunctionCode: string;
begin
  Result := EmptyStr;
end;

function TRFncCustom.GetCommandCode: string;
begin
  Result := GetFunctionCode;
  if fInversed then Result := tkNot + #32 + Result;
  case fOperator of
    coAnd: Result := tkAnd + #32 + Result;
    coOr: Result := tkOr + #32 + Result;
  end;
end;
*)

function TRFncCustom.UpdateVariables(const ScrStr: string): string;
begin
  if Assigned(fScript)
  then Result := ReplaceTags(fScript.fVarWork, ScrStr, True, fScript.GetScriptDate, fScript.TagsChar)
  else Result := ScrStr;
end;

procedure TRFncCustom.CalcResult(var Value: Boolean);
var
  IntValue: Boolean;
begin
  try
    if fInversed
    then IntValue := not GetResult
    else IntValue := GetResult;
    case fOperator of
      coNone: Value := IntValue;
      coAnd:  Value := Value and IntValue;
      coOr:   Value := Value or IntValue;
    end;
  except
    on E: Exception do
      fScript.DoShowInfo(fScript, Now, mcSystem, msError,
        Format(EExecuteFunction, [GetFunctionText_Exec, E.Message]));
  end;
end;

{ TRFncExecuted }

constructor TRFncExecuted.Create(Script: TRScriptCustom;
  const Inversed: Boolean; const Operator: TRFncOperator);
begin
  inherited Create(Script, Inversed, Operator);
  SetLength(fParams, 0);
end;

destructor TRFncExecuted.Destroy;
begin
  SetLength(fParams, 0);
  inherited Destroy;
end;

class function TRFncExecuted.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRFncExecuted.GetParamsCount: Integer;
begin
  Result := Length(fParams);
end;

procedure TRFncExecuted.CheckParameter(const Param: string);
begin
  if Trim(Param) = EmptyStr then
    raise ERScriptError.CreateFmt(EParseInvalidParam, [Param]);
end;

procedure TRFncExecuted.CheckParameters;
var
  i: Integer;
begin
  if GetParamsCount <> GetParamsLimit then
    raise ERScriptError.CreateFmt(EParseInvalidParamsCount,
      [GetParamsCount, GetParamsLimit]);
  for i := Low(fParams) to High(fParams) do
    CheckParameter(fParams[i]);
end;

function TRFncExecuted.AddParameter(const Param: string): Integer;
begin
  SetLength(fParams, Length(fParams) + 1);
  Result := High(fParams);
  fParams[Result] := Param;
end;

procedure TRFncExecuted.AddParameters(const Params: TRCmdParams);
var
  i: Integer;
begin
  SetLength(fParams, Length(Params));
  for i := Low(fParams) to High(fParams) do
    fParams[i] := Params[i];
  CheckParameters;
end;

function TRFncExecuted.GetParameter(const Index: Integer): string;
begin
  Result := EmptyStr;
  if Index in [Low(fParams)..High(fParams)] then
    Result := UpdateVariables(fParams[Index])
  else raise
    ERScriptError.CreateFmt(EParseInvalidParamsNumber, [Index]);
end;

function TRFncExecuted.GetFunctionParams_Edit: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := Low(fParams) to High(fParams) do
    if Result = EmptyStr
    then Result := ScriptQuotedStr(fParams[i])
    else Result := Result + #32 + ScriptQuotedStr(fParams[i]);
end;

function TRFncExecuted.GetFunctionParams_Exec: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := Low(fParams) to High(fParams) do
    if Result = EmptyStr
    then Result := ScriptQuotedStr(UpdateVariables(fParams[i]))
    else Result := Result + #32 + ScriptQuotedStr(UpdateVariables(fParams[i]));
end;

function TRFncExecuted.GetFunctionText_Edit: string;
begin
  Result := GetCommandName + chFuncBegin + GetFunctionParams_Edit + chFuncEnd;
end;

function TRFncExecuted.GetFunctionText_Exec: string;
begin
  Result := GetCommandName + chFuncBegin + GetFunctionParams_Exec + chFuncEnd;
end;

(*
function TRFncExecuted.GetFunctionCode: string;
begin
  Result := GetCommandName + chFuncBegin + GetCommandParams(False) + chFuncEnd;
end;
*)

{ TRFncBlock }

constructor TRFncBlock.Create(Script: TRScriptCustom; Parent: TRFncBlock;
  const Inversed: Boolean; const Operator: TRFncOperator);
begin
  inherited Create(Script, Inversed, Operator);
  fParent := Parent;
  fItems := TObjectList.Create;
end;

destructor TRFncBlock.Destroy;
begin
  fItems.Clear;
  fItems.Free;
  inherited Destroy;
end;

procedure TRFncBlock.AddFunction(CmdFunc: TRFncCustom);
begin
  if Assigned(CmdFunc) then
    fItems.Add(CmdFunc);
end;

procedure TRFncBlock.Clear;
begin
  fItems.Clear;
end;

function TRFncBlock.GetFunctionText_Edit: string;
var
  i, iCount: Integer;
begin
  Result := EmptyStr;
  iCount := fItems.Count - 1;
  for i := 0 to iCount do
    Result := Result + #32 + TRFncCustom(fItems[i]).GetFunctionItem_Edit;
  if Result <> EmptyStr then Delete(Result, 1, 1);
  Result := chFuncBegin + Result + chFuncEnd;
end;

function TRFncBlock.GetFunctionText_Exec: string;
var
  i, iCount: Integer;
begin
  Result := EmptyStr;
  iCount := fItems.Count - 1;
  for i := 0 to iCount do
    Result := Result + #32 + TRFncCustom(fItems[i]).GetFunctionItem_Exec;
  if Result <> EmptyStr then Delete(Result, 1, 1);
  Result := chFuncBegin + Result + chFuncEnd;
end;

(* function TRFncBlock.GetCommandCode: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to fItems.Count - 1 do
    Result := Result + #32 + TRFncCustom(fItems[i]).GetCommandCode;
  if Result <> EmptyStr then Delete(Result, 1, 1);
  Result := chFuncBegin + Result + chFuncEnd;
end; *)

function TRFncBlock.GetItem(const Index: Integer): TRFncCustom;
begin
  Result := TRFncCustom(fItems[Index]);
end;

function TRFncBlock.GetResult: Boolean;
var
  i, iCount: Integer;
begin
  fScript.DoShowProgress(fScript, mcTransaction, 0, fItems.Count);
  if fScript.Executed and (fItems.Count > 0) then
  begin
    Result := True;
    iCount := fItems.Count - 1;
    for i := 0 to iCount do
    begin
      if not fScript.Executed then
        Break;
      TRFncCustom(fItems[i]).CalcResult(Result);
      fScript.DoShowProgress(fScript, mcTransaction, i + 1, fItems.Count);
    end;
  end
  else raise ERScriptError.CreateFmt(EParseEmplyBlock, [GetFunctionText_Exec]);
end;

{ TRFncCondition }

constructor TRFncCondition.Create(Script: TRScriptCustom);
begin
  inherited Create;
  fScript := Script;
  fTopBlock := TRFncBlock.Create(Script, nil, False, coNone);
  fActBlock := fTopBlock;
end;

destructor TRFncCondition.Destroy;
begin
  fActBlock := nil;
  fTopBlock.Free;
  fScript := nil;
  inherited Destroy;
end;

procedure TRFncCondition.AddFunction(CmdFunc: TRFncCustom);
begin
  if Assigned(fActBlock)
  then fActBlock.AddFunction(CmdFunc)
  else raise ERScriptError.Create(EParseNoActBlock);
end;

procedure TRFncCondition.BlockBegin(const Inversed: Boolean;
  const Operator: TRFncOperator);
var
  fNewBlock: TRFncBlock;
begin
  if Assigned(fActBlock) then
  begin
    fNewBlock := TRFncBlock.Create(fScript, fActBlock, Inversed, Operator);
    fActBlock.AddFunction(fNewBlock);
    fActBlock := fNewBlock;
  end
  else raise ERScriptError.Create(EParseNoActBlock);
end;

procedure TRFncCondition.BlockEnd;
begin
  if Assigned(fActBlock)
  then fActBlock := fActBlock.ParentBlock
  else raise ERScriptError.Create(EParseNoActBlock);
end;

procedure TRFncCondition.CheckActBlock;
begin
  if fActBlock <> fTopBlock then
    raise ERScriptError.Create(EParseNoEndBlock);
end;

procedure TRFncCondition.Clear;
begin
  fTopBlock.Clear;
end;

function TRFncCondition.GetConditionText_Edit: string;
begin
  Result := fTopBlock.GetFunctionText_Edit;
  if (Result <> EmptyStr) and (Result[1] = chFuncBegin) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

function TRFncCondition.GetConditionText_Exec: string;
begin
  Result := fTopBlock.GetFunctionText_Exec;
  if (Result <> EmptyStr) and (Result[1] = chFuncBegin) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

(* function TRFncCondition.GetCommandCode: string;
begin
  Result := fTopBlock.GetCommandCode;
  if (Result <> EmptyStr) and (Result[1] = chFuncBegin) then
    Result := Copy(Result, 2, Length(Result) - 2);
end; *)

function TRFncCondition.GetResult: Boolean;
begin
  Result := True;
  if fScript.Executed then
  begin
    fScript.DoUpdateControls(fScript, fScript.fCanBreak, False);
    fScript.DoShowInfo(fScript, Now, mcTransaction, msTitle, SExecuteAnalyzeFunctions);
    fScript.DoShowProgress(fScript, mcTransaction, 0, 0);
    fTopBlock.CalcResult(Result);
  end;
end;

{ TRCmdCondition }

constructor TRCmdCondition.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fCondition := TRFncCondition.Create(fScript);
  fThenList := TRCmdBlock.Create(fScript);
  fElseList := TRCmdBlock.Create(fScript);
end;

destructor TRCmdCondition.Destroy;
begin
  fElseList.Clear;
  fThenList.Clear;
  fElseList.Free;
  fThenList.Free;
  fCondition.Free;
  inherited Destroy;
end;

class function TRCmdCondition.GetBlockThen: string;
begin
  Result := EmptyStr;
end;

class function TRCmdCondition.GetBlockElse: string;
begin
  Result := EmptyStr;
end;

function TRCmdCondition.GetCommandCount: Integer;
begin
  Result := inherited GetCommandCount + fThenList.GetCommandCount + fElseList.GetCommandCount;
end;

function TRCmdCondition.GetCommandParams_Edit: string;
begin
  Result := fCondition.GetConditionText_Edit;
end;

function TRCmdCondition.GetCommandParams_Exec: string;
begin
  Result := fCondition.GetConditionText_Exec;
end;

function TRCmdCondition.GetCommandText_Edit: string;
begin
  Result := GetCommandName + #32 + GetCommandParams_Edit;
end;

function TRCmdCondition.GetCommandText_Exec: string;
begin
  Result := GetCommandName + #32 + GetCommandParams_Exec;
end;

function TRCmdCondition.GetCommandCode(const OffsetLevel: Integer): string;
begin
  Result := StringOfChar(#32, OffsetLevel * 2) + GetCommandName + #32 + fCondition.GetConditionText_Edit;
  if (GetBlockThen <> EmptyStr) and (fThenList.GetCommandCount > 0) then
  begin
    Result := Result + #32 + GetBlockThen + #13#10
      + StringOfChar(#32, OffsetLevel * 2) + tkBlockBegin1 + #13#10
      + fThenList.GetCommandsCode(OffsetLevel + 1) + #13#10
      + StringOfChar(#32, OffsetLevel * 2) + tkBlockEnd1;
  end;
  if (GetBlockElse <> EmptyStr) and (fElseList.GetCommandCount > 0) then
  begin
    Result := Result + #13#10 + StringOfChar(#32, OffsetLevel * 2) + GetBlockElse + #13#10
      + StringOfChar(#32, OffsetLevel * 2) + tkBlockBegin1 + #13#10
      + fElseList.GetCommandsCode(OffsetLevel + 1) + #13#10
      + StringOfChar(#32, OffsetLevel * 2) + tkBlockEnd1;
  end;
end;

{ TRCmdCycle }

constructor TRCmdCycle.Create(Script: TRScriptCustom);
begin
  inherited Create(Script);
  fCycleText := EmptyStr;
  fCmdList := TRCmdBlock.Create(fScript);
  fCmdList.fLockIncTotalOpCount := True;
end;

destructor TRCmdCycle.Destroy;
begin
  fCmdList.Free;
  inherited Destroy;
end;

function TRCmdCycle.GetCycleText: string;
begin
  Result := fCycleText;
end;

function TRCmdCycle.GetCycleEnd: string;
begin
  Result := tkDo;
end;

function TRCmdCycle.GetCommandCount: Integer;
begin
  Result := inherited GetCommandCount; // + GetList.Count * CmdList.GetCommandCount;
end;

function TRCmdCycle.GetCommandParams_Edit: string;
begin
  Result := ScriptQuotedStr(GetCycleText);
end;

function TRCmdCycle.GetCommandParams_Exec: string;
begin
  Result := UpdateVariables(GetCycleText);
end;

function TRCmdCycle.GetCommandText_Edit: string;
begin
  // !!!! + #32 + GetCycleEnd; - НЕ УБИРАТЬ! Иначе некорректно отрабатывает команда GetCommandCode для "внешнего процесора"!
  Result := GetCommandName + #32 + GetCommandParams_Edit + #32 + GetCycleEnd;
end;

function TRCmdCycle.GetCommandText_Exec: string;
begin
  Result := GetCommandName + #32 + ScriptQuotedStr(GetCommandParams_Exec); // + #32 + GetCycleEnd;
end;

function TRCmdCycle.GetCommandCode(const OffsetLevel: Integer): string;
begin
  Result := StringOfChar(#32, OffsetLevel * 2) + GetCommandText_Edit + #13#10
    + StringOfChar(#32, OffsetLevel * 2) + tkBlockBegin1 + #13#10
    + fCmdList.GetCommandsCode(OffsetLevel + 1) + #13#10
    + StringOfChar(#32, OffsetLevel * 2) + tkBlockEnd1;
end;

{ TRScriptCustom }

constructor TRScriptCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UserData := nil;
  fScript := nil;
  fObjList := TStringList.Create;
  fKeyList := TStringList.Create;
  fVarList := TStringList.Create;
  fVarWork := TStringList.Create;
  fUndoList := TRCopyList.Create;
  fProcList := TObjectList.Create(True);
  fBlocksStack := TObjectList.Create(False);
  fProcsStack := TObjectList.Create(False);
  fAlias := EmptyStr;
  fAttrs := EmptyStr;
  fCanBreak := False;
  fCaption := EmptyStr;
  fBufferSize := 64;
  fCycleMax := 10;
  fDateMode := dmFullAuto;
  fDateFixed := 0;
  fNetUsage := 100;
  fTagChar := '%';
  fTryMax := 3;
  fTryDelay := 3;
  fVsInit := False;
  fCbCall := 0;
  fEditMode := False;
  fCompiled := False;
  fExecuted := False;
  fCurrState := stOk;
  fGuiEnabled := True;
  fExtProcessor := False;
  // v 6.3.0, 10-04-2012
  fExtCmdWaitTime := DefMaxWaitTime;
end;

destructor TRScriptCustom.Destroy;
begin
  if fVsInit then
    DoDoneVisualComponents;
  fProcList.Clear;
  fUndoList.Clear;
  fObjList.Clear;
  fKeyList.Clear;
  fVarWork.Clear;
  fVarList.Clear;
  fProcList.Free;
  fUndoList.Free;
  fObjList.Free;
  fKeyList.Free;
  fVarWork.Free;
  fVarList.Free;
  fBlocksStack.Free;
  fProcsStack.Free;
  fScript := nil;
  UserData := nil;
  inherited Destroy;
end;

procedure TRScriptCustom.InitVariables;
begin
  fVarList.Clear;
  AddStandartVariables(fVarList, True);
  AddVariable(fVarList, varAttributes, fAttrs);
  AddVariable(fVarList, varAlias, fAlias);
  AddVariable(fVarList, varDescription, fCaption);
  AddVariable(fVarList, varOnError, '');
  AddVariable(fVarList, varExitCode, '0');
end;

procedure TRScriptCustom.PutVariable(const WorkList: Boolean; const Name, Value: string);
begin
  if WorkList
  then AddVariable(fVarWork, Name, Value)
  else AddVariable(fVarList, Name, Value);
end;

procedure TRScriptCustom.PutVariable(const WorkList: Boolean; const Variable: string);
begin
  if WorkList
  then AddVariable(fVarWork, Variable)
  else AddVariable(fVarList, Variable);
end;

procedure TRScriptCustom.PutVariables(const InitList: Boolean; AVarList: TStrings);
var
  i, iCount: Integer;
begin
  if InitList then
    InitVariables;
  iCount := AVarList.Count - 1;
  for i := 0 to iCount do
    AddVariable(fVarList, AVarList.Names[i], AVarList.ValueFromIndex[i]);
end;

procedure TRScriptCustom.PutVariables(const InitList: Boolean; AVarList: string);
var
  BufList: TStrings;
begin
  BufList := TStringList.Create;
  try
    BufList.Text := AVarList;
    PutVariables(InitList, BufList);
  finally
    BufList.Free;
  end;
end;

procedure TRScriptCustom.SetAlias(const Value: string);
begin
  if fAlias <> Value then
  begin
    fAlias := Value;
    AddVariable(fVarList, varAlias, fAlias);
  end;
end;

procedure TRScriptCustom.SetAttrs(const Value: string);
begin
  if fAttrs <> Value then
  begin
    fAttrs := Value;
    AddVariable(fVarList, varAttributes, fAttrs);
  end;
end;

procedure TRScriptCustom.SetCaption(const Value: string);
begin
  if fCaption <> Value then
  begin
    fCaption := Value;
    AddVariable(fVarList, varDescription, fCaption);
  end;
end;

procedure TRScriptCustom.SetKeywords(const Value: TStrings);
begin
  fKeyList.Assign(Value);
end;

procedure TRScriptCustom.SetScript(const Value: TStrings);
begin
  fCompiled := False;
  if Assigned(Value) then
  begin
    if Assigned(fScript)
    then fScript.Assign(Value)
    else fScript := Value;
  end
  else fScript := Value;
end;

procedure TRScriptCustom.SetVariables(const Value: TStrings);
begin
  fVarList.Assign(Value);
end;

function TRScriptCustom.AddProc(const Name: string): Integer;
begin
  if Assigned(FindProc(Name)) then
    raise ERScriptError.CreateFmt(EScriptProcAlreadyExists, [Name]);
  Result := fProcList.Add(TRCmdProc.Create(Self, Name));
end;

function TRScriptCustom.CreateProc(const Name: string): TRCmdProc;
begin
  if Assigned(FindProc(Name)) then
    raise ERScriptError.CreateFmt(EScriptProcAlreadyExists, [Name]);
  Result := TRCmdProc.Create(Self, Name);
  if Assigned(Result) then fProcList.Add(Result);
end;

function TRScriptCustom.FindProc(const Name: string): TRCmdProc;
var
  i: Integer;
begin
  Result := nil; i := 0;
  while (i < fProcList.Count) and (Result = nil) do
  begin
    if SameText(Name, Item[i].Name)
    then Result := Item[i]
    else Inc(i);
  end;
end;

function TRScriptCustom.FindExternalProc(const Name: string): TRCmdProc;
begin
  Result := nil;
end;

function TRScriptCustom.FindExternalScript(const Alias: string):TRScriptCustom;
begin
  Result := nil;
  if Trim(Alias) = EmptyStr then
    Result := Self;
end;

function TRScriptCustom.GetProc(Index: Integer): TRCmdProc;
begin
  Result := fProcList[Index] as TRCmdProc;
end;

function TRScriptCustom.IndexOf(Proc: TRCmdProc): Integer;
begin
  Result := fProcList.IndexOf(Proc);
end;

procedure TRScriptCustom.IncTotalOpCount(const Increment: Integer);
begin
  Inc(fTotalOpCount, Increment);
end;

function TRScriptCustom.GetCommandCount: Integer;
var
  i, iCount: Integer;
begin
  Result := 0;
  iCount := fProcList.Count - 1;
  for i := 0 to iCount do
    Inc(Result, TRCmdProc(fProcList[i]).GetCommandCount);
end;

function TRScriptCustom.IsExternalProc(const Name: string): Boolean;
begin
  Result := (Name <> EmptyStr) and (Pos(chProcDiv, Name) > 0)
    and not SameText(Copy(Name, 1, Pos(chProcDiv, Name) - 1), fAlias);
end;

function TRScriptCustom.GetAliasProcName(const Name: string): string;
begin
  if (Name <> EmptyStr) and (Pos(chProcDiv, Name) > 0)
  then Result := Copy(Name, 1, Pos(chProcDiv, Name) - 1)
  else Result := EmptyStr;
end;

function TRScriptCustom.GetShortProcName(const Name: string): string;
begin
  if (Name <> EmptyStr) and (Pos(chProcDiv, Name) > 0)
  then Result := Copy(Name, Pos(chProcDiv, Name) + 1, Length(Name))
  else Result := Name;
end;

function TRScriptCustom.GetFullProcName(const Alias, Name: string): string;
begin
  if (Alias = EmptyStr) or IsExternalProc(Name)
  then Result := Name
  else Result := Alias + chProcDiv + Name;
end;

procedure TRScriptCustom.AddObject(const ObjType: TRScrObjType; const ObjName: string; const ObjPtr: TObject);
var
  CurrProc: TRCmdProc;
begin
  if (ObjName = EmptyStr) or (ObjPtr = nil) then
    raise ERScriptError.CreateFmt(EScriptInvalidObject, [ObjName]);
  if Assigned(FindObject(ObjType, ObjName, False)) then
    raise ERScriptError.CreateFmt(EScriptExistsObject, [ObjName]);
  if ObjType in [otProc, otAuto] then
  begin
    CurrProc := CmdProc_Last;
    if Assigned(CurrProc)
    then CurrProc.AddObject(ObjName, ObjPtr)
    else raise ERScriptError.Create(EScriptInvalidCurrProc);
  end
  else fObjList.AddObject(ObjName, ObjPtr);
end;

procedure TRScriptCustom.DelObjectSctipt(const ObjName: string);
var
  i, iCount, Found: Integer;
  Tmp: TObject;
begin
  Found := -1;
  iCount := fObjList.Count - 1;
  for i := 0 to iCount do
    if SameText(ObjName, fObjList[i]) then
    begin
      Found := i;
      Break;
    end;
  if Found = -1 then
    raise ERScriptError.CreateFmt(EScriptObjectNotFound, [ObjName])
  else begin
    Tmp := fObjList.Objects[Found];
    Tmp.Free;
    fObjList.Objects[Found] := nil;
    fObjList.Delete(Found);
  end;
end;

procedure TRScriptCustom.DelObject(const ObjType: TRScrObjType; const ObjName: string);
var
  CurrProc: TRCmdProc;
begin
  case ObjType of
    otScript: DelObjectSctipt(ObjName);
    otProc:
    begin
      CurrProc := CmdProc_Last;
      if Assigned(CurrProc)
      then CurrProc.DelObject(ObjName)
      else raise ERScriptError.Create(EScriptInvalidCurrProc);
    end;
    otAuto:
    begin
      CurrProc := CmdProc_Last;
      if Assigned(CurrProc) and Assigned(CurrProc.FindObject(ObjName))
      then CurrProc.DelObject(ObjName)
      else DelObjectSctipt(ObjName);
    end;
  end;
end;

function TRScriptCustom.FindObjectScript(const ObjName: string): TObject;
var
  i, iCount: Integer;
begin
  Result := nil;
  iCount := fObjList.Count - 1;
  for i := 0 to iCount do
    if SameText(ObjName, fObjList[i]) then
    begin
      Result := fObjList.Objects[i];
      Break;
    end;
end;

function TRScriptCustom.FindObject(const ObjType: TRScrObjType; const ObjName: string; const RaiseError: Boolean): TObject;
var
  CurrProc: TRCmdProc;
begin
  Result := nil;
  case ObjType of
    otScript: Result := FindObjectScript(ObjName);
    otProc:
    begin
      CurrProc := CmdProc_Last;
      if Assigned(CurrProc)
      then Result := CurrProc.FindObject(ObjName)
      else raise ERScriptError.Create(EScriptInvalidCurrProc);
    end;
    otAuto:
    begin
      CurrProc := CmdProc_Last;
      if Assigned(CurrProc) then
      begin
        Result := CurrProc.FindObject(ObjName);
        if Result = nil then
          Result := FindObjectScript(ObjName);
      end
      else Result := FindObjectScript(ObjName);
    end;
  end;
  if (Result = nil) and RaiseError then
    raise ERScriptError.CreateFmt(EScriptObjectNotFound, [ObjName])
end;

procedure TRScriptCustom.DoShowInfo(Sender: TObject;
  const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
  const MsgState: TRMsgState; const MsgText: string);
begin
  if Assigned(fShowInfo) then
    fShowInfo(Self, TimeStamp, MsgClass, MsgState, MsgText);
end;

procedure TRScriptCustom.DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState);
begin
  if Assigned(fShowState) then
    fShowState(Self, Global, State);
end;

procedure TRScriptCustom.DoShowProgress(Sender: TObject;
  const MsgClass: TRMsgClass; const CurrPos, MaxPos: Integer);
begin
  if Assigned(fShowProgress) then
    fShowProgress(Self, MsgClass, CurrPos, MaxPos);
end;

procedure TRScriptCustom.DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer);
begin
  if Assigned(fFileProgress) then
    fFileProgress(Self, CurrPos, MaxPos);
end;

procedure TRScriptCustom.DoCheckBreak(Sender: TObject; var IsBreak: Boolean);
begin
  IsBreak := IsBreak or not fExecuted;
  if not IsBreak then
  begin
    if Assigned(fCheckBreak) then
      fCheckBreak(Self, IsBreak);
    if IsBreak and fExecuted then
      BreakScript;
  end;
end;

procedure TRScriptCustom.DoUpdateControls(Sender: TObject;
  const BreakEnabled, CloseEnabled: Boolean);
begin
  if Assigned(fUpdateControls) then
    fUpdateControls(Self, BreakEnabled, CloseEnabled);
end;

procedure TRScriptCustom.DoCalcNetUsage(Sender: TObject; var NetUsage: Byte);
begin
  NetUsage := fNetUsage;
  if Assigned(fGetNetUsage) then
    fGetNetUsage(Self, NetUsage);
end;

function TRScriptCustom.CalcNetUsage: Byte;
begin
  Result := fNetUsage;
  if Assigned(fGetNetUsage) then
    fGetNetUsage(Self, Result);
end;

procedure TRScriptCustom.DoExecCmdOnEP(Sender: TObject; const CmdType: TRCmdExtType;
  const CmdText: string; const ScriptName: string; const AliasName: string; const RunProcName: string;
  const ShowInfo: TRShowInfoNotifyEvent; const ShowProgress: TRShowProgressNotifyEvent;
  const FileProgress: TRFileProgressNotifyEvent; var State: TRScriptState);
begin
  if Assigned(fExecCmdOnEP) then
    fExecCmdOnEP(Self, CmdType, CmdText,
      ScriptName, AliasName, RunProcName, ShowInfo, ShowProgress, FileProgress, State);
end;

function TRScriptCustom.GetScriptName: string;
begin
  Result := Trim(fCaption);
  if (Result = EmptyStr) and (fAlias <> EmptyStr) then
    Result := Trim(fAlias);
  if Assigned(fGetScriptName) then
    fGetScriptName(Self, Result);
end;

function TRScriptCustom.GetScriptTitle: string;
begin
  Result := GetScriptName;
end;

function TRScriptCustom.GetScriptDate: TDateTime;
begin
  case fDateMode of
    dmFullFixed: Result := fDateFixed;
    dmTimeAuto: Result := DateOf(fDateFixed) + TimeOf(Now);
    else Result := Now;
  end;
end;

function TRScriptCustom.GetScriptText: string;
begin
  Result := EmptyStr;
  if Assigned(fScript) then
    Result := fScript.Text;
end;

function TRScriptCustom.GetScriptTemplate: string;
begin
  Result := tkVars + #13#10#13#10 + tkProc1 + #32 + tkMainProc
    + #13#10 + tkBlockBegin1 + #13#10 + tkBlockEnd1 + #13#10;
end;

function TRScriptCustom.GetDefaultCmdAttributes: string;
begin
  Result := GetVariableValue(fVarWork, varAttributes);
end;

function TRScriptCustom.GetDefaultCmdDescription: string;
begin
  Result := fTagChar + varDescription + fTagChar;
end;

// Compilator ------------------------------------------------------------------

function TRScriptCustom._IsPosValid(const ChPos: Integer): Boolean;
begin
  Result := (ChPos > 0) and (ChPos <= Length(ScriptText));
end;

function TRScriptCustom._Eol(const ChPos: Integer): Boolean;
begin
  Result := _IsPosValid(ChPos) and (ScriptText[ChPos] in chEol);
end;

function TRScriptCustom._Eow(const ChPos: Integer): Boolean;
begin
  Result := _IsPosValid(ChPos) and (ScriptText[ChPos] in chWordDiv);
end;

function TRScriptCustom._Eos(const ChPos: Integer): Boolean;
begin
  Result := ChPos > Length(ScriptText);
end;

function TRScriptCustom._Test(const Chars: TSysCharSet; const ChPos: Integer): Boolean;
begin
  Result := _IsPosValid(ChPos) and (ScriptText[ChPos] in Chars);
end;

function TRScriptCustom._Quote(const ChPos: Integer): Boolean;
begin
  Result := _Test(chQuotes, ChPos); // and not _Test(chQuotes, ChPos + 1));
end;

function TRScriptCustom._DblQuote(const ChPos: Integer): Boolean;
begin
  Result := _Test(chQuotes, ChPos) and _Test(chQuotes, ChPos + 1);
end;

function TRScriptCustom._TriQuote(const ChPos: Integer): Boolean;
begin
  Result := _Test(chQuotes, ChPos) and _Test(chQuotes, ChPos + 1) and _Test(chQuotes, ChPos + 2);
end;

function TRScriptCustom._Peek(const SubStr: string; const ChPos: Integer): Boolean;
begin
  Result := _IsPosValid(ChPos)
    and SameText(SubStr, Copy(GetScriptText, ChPos, Length(SubStr)));
end;

function TRScriptCustom._PeekComments(const SkipLine: Boolean; var ChPos: Integer): Boolean;
begin
  Result := _Peek(tkComment1 + #32, ChPos) or _Peek(tkComment2, ChPos)
    or _Peek(tkComment3, ChPos) or _Peek(tkComment4, ChPos)
    or _Peek(tkComment5, ChPos);
  if Result and SkipLine then
    _SkipLine(ChPos);
end;

function TRScriptCustom._PeekEnd(const ChPos: Integer): Boolean;
var
  CrPos: Integer;
begin
  CrPos := ChPos;
  _SkipComments(CrPos);
  while (CrPos <= Length(ScriptText))
    and (ScriptText[CrPos] in [#0..#32]) do
      Inc(CrPos);
  Result := CrPos > Length(ScriptText);
end;

procedure TRScriptCustom._CheckEol(var ChPos: Integer);
var
  CrPos: Integer;
begin
  CrPos := ChPos;
  while _IsPosValid(CrPos) and not _Eol(CrPos) and _Eow(CrPos) do
    Inc(CrPos);
  if _IsPosValid(CrPos) and (_Eol(CrPos) or _PeekComments(True, CrPos))
  then ChPos := CrPos;
end;

procedure TRScriptCustom._SkipLine(var ChPos: Integer);
begin
  while _IsPosValid(ChPos) and _IsPosValid(ChPos + 1)
    and not (ScriptText[ChPos] in chEol) do
      Inc(ChPos);
end;

procedure TRScriptCustom._SkipSpaces(const NextLine: Boolean; var ChPos: Integer);
begin
  while _IsPosValid(ChPos) and _IsPosValid(ChPos + 1)
    and (NextLine or not (ScriptText[ChPos] in chEol))
    and (ScriptText[ChPos] in chWordDiv) do
      Inc(ChPos);
end;

procedure TRScriptCustom._SkipComments(var ChPos: Integer);
begin
  _SkipSpaces(False, ChPos);
  while _PeekComments(True, ChPos) do
    _SkipSpaces(True, ChPos);
end;

procedure TRScriptCustom._ClearPositions;
begin
  fFirstPos := -1;
  fLastPos := -1;
end;

procedure TRScriptCustom._StorePositions(const ChPos: Integer);
begin
  if fFirstPos = -1 then fFirstPos := ChPos - 1;
  if ChPos > fLastPos then fLastPos := ChPos;
end;

function TRScriptCustom._ReadSimple(var ChPos: Integer; var Value: string; var bQuoted: Boolean): Boolean;
begin
  Result := False;
  bQuoted := _Quote(ChPos) and (not _DblQuote(ChPos) or _TriQuote(ChPos));
  if bQuoted then
  begin
    _StorePositions(ChPos);
    Inc(ChPos);
  end
  else begin
    if _DblQuote(ChPos) then
    begin
      // !!! В "обычном" режиме двйные кавычки воспринимаются как пустая строка
      // Value := Value + ScriptText[ChPos] + ScriptText[ChPos + 1];
      _StorePositions(ChPos + 1);
      Inc(ChPos, 2);
    end
    else begin
      if _PeekComments(True, ChPos) then
      begin
        if Value = EmptyStr
        then _SkipSpaces(False, ChPos)
        else Result := True;
      end
      else begin
        Value := Value + ScriptText[ChPos];
        _StorePositions(ChPos);
        Inc(ChPos);
      end;
    end;
  end;
end;

procedure TRScriptCustom._ReadQuoted(var ChPos: Integer; var Value: string; var bQuoted: Boolean);
begin
  if _Quote(ChPos) and (not _DblQuote(ChPos) or _TriQuote(ChPos)) then
  begin
    bQuoted := False;
    _StorePositions(ChPos);
    if _TriQuote(ChPos) then
    begin
      Value := Value + ScriptText[ChPos];
      _StorePositions(ChPos + 1);
      Inc(ChPos, 2);
    end;
    Inc(ChPos);
    _CheckEol(ChPos);
  end
  else begin
    if _DblQuote(ChPos) then
    begin
      Value := Value + ScriptText[ChPos];
      _StorePositions(ChPos + 1);
      Inc(ChPos, 2);
    end
    else begin
      Value := Value + ScriptText[ChPos];
      _StorePositions(ChPos);
      Inc(ChPos);
    end;
  end;
  if bQuoted and _Eol(ChPos) then
  begin
    if fEditMode
    then bQuoted := False
    else raise ERScriptError.CreateFmt(EParseUnterminatedString, [Value]);
  end
end;

function TRScriptCustom.CompareToken(const Token, Mask: string): Boolean;
begin
  Result := AnsiSameText(Trim(Token), Mask);
end;

procedure TRScriptCustom.CheckToken(const Token: string);
begin
end;

function TRScriptCustom.ReadToken(const NewLine, NextLine, CheckTkn: Boolean;
  var ChPos: Integer): string;
var
  bExit, bQuoted: Boolean;
begin
  Result := EmptyStr;
  // Пропускаем разделительные символы
  _SkipSpaces(NewLine, ChPos);
  // Считываем текущее слово
  bExit := False; bQuoted := False;
  while not bExit and _IsPosValid(ChPos) and not _Eol(ChPos) do
  begin
    // Считываем символы в кавычках
    if bQuoted
    then _ReadQuoted(ChPos, Result, bQuoted)
    else begin
      // Проверка на конец слова
      if _Eow(ChPos) then
      begin
        Inc(ChPos);
        bExit := True;
      end
      // Считываем символы без кавычек
      else bExit := _ReadSimple(ChPos, Result, bQuoted);
    end;
  end;
  // Пропускаем разделительные символы
  _SkipSpaces(NextLine, ChPos);
  // Проверяем результат
  if CheckTkn then
    CheckToken(Result);
end;

function TRScriptCustom.ReadFuncToken(var ChPos: Integer): string;
var
  bExit, bQuoted: Boolean;
begin
  Result := EmptyStr;
  // Пропускаем разделительные символы
  _SkipSpaces(True, ChPos);
  // Считываем строку
  bExit := False; bQuoted := False;
  while not bExit and _IsPosValid(ChPos) do
  begin
    // Считываем символы в кавычках
    if bQuoted
    then _ReadQuoted(ChPos, Result, bQuoted)
    else begin
      // Проверка на скобки
      if (ScriptText[ChPos] in [chFuncBegin, chAttrBegin, chFuncEnd, chAttrEnd]) then
      begin
        if Result = EmptyStr then
        begin
          _StorePositions(ChPos);
          Result := ScriptText[ChPos];
          Inc(ChPos);
          bExit := True;
        end
        else bExit := True;
      end
      else begin
        // Проверка на конец слова
        if _Eow(ChPos) then
        begin
          Inc(ChPos);
          bExit := True;
        end
        // Считываем символы без кавычек
        else bExit := _ReadSimple(ChPos, Result, bQuoted);
      end;
    end;
  end;
  // Пропускаем разделительные символы
  _SkipSpaces(True, ChPos);
  // Проверяем результат
  CheckToken(Result);
end;

function TRScriptCustom.ReadLine(const NewLine: Boolean; var ChPos: Integer): string;
var
  bExit, bQuoted: Boolean;
begin
  Result := EmptyStr;
  // Пропускаем разделительные символы
  _SkipSpaces(NewLine, ChPos);
  // Считываем строку
  bExit := False; bQuoted := False;
  while not bExit and _IsPosValid(ChPos) do
  begin
    // Считываем символы в кавычках
    if bQuoted
    then _ReadQuoted(ChPos, Result, bQuoted)
    else begin
      // Проверка на конец строки
      if _Eol(ChPos) then
      begin
        Inc(ChPos);
        bExit := True;
      end
      // Считываем символы без кавычек
      else bExit := _ReadSimple(ChPos, Result, bQuoted);
    end;
  end;
  // Пропускаем разделительные символы
  _SkipSpaces(True, ChPos);
  // Проверяем результат
  CheckToken(Result);
end;

function TRScriptCustom.ReadExeParams(const ParamsLimit: Integer;
  const AliasName: string; var ChPos: Integer): TRCmdData;
var
  bExit, bQuoted, bAttr: Boolean;
  fAttrPos: Integer;
  Token: string;
begin
  SetLength(Result.fParams, 0);
  // Пропускаем разделительные символы
  _SkipSpaces(False, ChPos);
  if ParamsLimit >= 0 then
  begin
    // Считываем параметры операции - постоянное количество
    while _IsPosValid(ChPos) and not _Eol(ChPos)
      and (Length(Result.fParams) < ParamsLimit) do
    begin
      // Считываем токен
      Token := ReadToken(False, False, False, ChPos);
      // if Token = #32 then Token := EmptyStr;
      SetLength(Result.fParams, Length(Result.fParams) + 1);
      Result.fParams[High(Result.fParams)] := Token;
    end;
  end
  else begin
    // Считываем параметры операции - переменное количество
    fAttrPos := ChPos;
    while _IsPosValid(ChPos) and not _Eol(ChPos) do
    begin
      // Считываем токен
      Token := ReadToken(False, False, False, ChPos);
      // Проверяем на атрибуты
      if (Copy(Token, 1, 1) = chAttrBegin)
      and (Copy(Token, Length(Token), 1) = chAttrEnd) then
      begin
        ChPos := fAttrPos;
        Break;
      end
      else begin
        fAttrPos := ChPos;
        // if Token = #32 then Token := EmptyStr;
        SetLength(Result.fParams, Length(Result.fParams) + 1);
        Result.fParams[High(Result.fParams)] := Token;
      end;
    end;
  end;
  // Пропускаем разделительные символы
  _SkipSpaces(False, ChPos);
  // Считываем атрибуты и описание
  Token := EmptyStr;
  Result.fFlagsDefault := True; Result.fNotesDefault := True;
  bExit := False; bQuoted := False; bAttr := False;
  while not bExit and _IsPosValid(ChPos) do
  begin
    // Считываем символы в кавычках
    if bQuoted
    then _ReadQuoted(ChPos, Token, bQuoted)
    else begin
      if bAttr then
      begin
        // Режим чтения атрибутов
        if _Test(chAttrChars, ChPos) then
        begin
          Token := Token + ScriptText[ChPos];
          _StorePositions(ChPos);
          Inc(ChPos);
        end
        else begin
          // Проверка на конец строки атрибутов
          if _Test([chAttrEnd], ChPos) then
          begin
            bAttr := False;
            Result.fFlagsDefault := False;
            Result.fFlags := AnsiUpperCase(Trim(Token));
            Token := EmptyStr;
            _StorePositions(ChPos);
            Inc(ChPos);
            _SkipSpaces(False, ChPos);
          end
          else begin
            fParsePos := ChPos;
            raise ERScriptError.CreateFmt(EParseInvalidAttrChar, [ScriptText[fParsePos]]);
          end;
        end;
      end
      else begin
        // Режим чтения комментариев
        if _Eol(ChPos) then
        begin
          bExit := True;
          Inc(ChPos);
        end
        else begin
          // Проверка на начало атрибутов
          if Result.fFlagsDefault and _Test([chAttrBegin], ChPos) then
          begin
            if Token <> EmptyStr then
            begin
              Result.fNotesDefault := False;
              Result.fNotes := Trim(Token);
              Token := EmptyStr;
            end;
            bAttr := True;
            _StorePositions(ChPos);
            Inc(ChPos);
          end
          // Читаем текущий токен
          else bExit := _ReadSimple(ChPos, Token, bQuoted);
        end;
        // Проверка непустого токена при выходе
        if bExit and (Token <> EmptyStr) then
        begin
          Result.fNotesDefault := False;
          Result.fNotes := Token;
        end;
      end;
    end;
  end;
  // Пропускаем разделительные символы
  _SkipSpaces(True, ChPos);
end;

function TRScriptCustom.ReadExeCommand(CmdType: CRCmdExecuted;
  const AliasName: string; var ChPos: Integer): TRCmdExecuted;
var
  CmdData: TRCmdData;
begin
  Result := CmdType.Create(TRScript(Self));
  try
    try
      CmdData := ReadExeParams(CmdType.GetParamsLimit, AliasName, ChPos);
      Result.AddParameters(CmdData.fParams);
      if CmdData.fFlagsDefault
      then Result.Attributes := GetDefaultCmdAttributes
      else Result.Attributes := CmdData.fFlags;
      if CmdData.fNotesDefault
      then Result.Description := GetDefaultCmdDescription
      else Result.Description := CmdData.fNotes;
    finally
      SetLength(CmdData.fParams, 0);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TRScriptCustom.ReadFncParams(CmdType: CRFncExecuted;
  const Inversed: Boolean; const Operator: TRFncOperator;
  var ChPos: Integer; const CheckParameters: Boolean): TRFncExecuted;
var
  Token: string;
  ParamsEnd: Boolean;
begin
  Result := CmdType.Create(TRScript(Self), Inversed, Operator);
  try
    // Пропускаем разделительные символы
    _SkipSpaces(False, ChPos);
    // Считываем первый токен
    Token := ReadFuncToken(ChPos);
    if CompareToken(Token, chFuncBegin) or CompareToken(Token, chAttrBegin) then
    begin
      repeat
        Token := ReadFuncToken(ChPos);
        ParamsEnd := CompareToken(Token, chFuncBegin) or CompareToken(Token, chAttrBegin)
          or CompareToken(Token, chFuncEnd) or CompareToken(Token, chAttrEnd);
        if not ParamsEnd then
          Result.AddParameter(Token);
      until ParamsEnd or _Eos(ChPos);
    end;
    // Проверяем количество считанных параметров
    if CheckParameters then
      Result.CheckParameters;
    // Пропускаем разделительные символы
    _SkipSpaces(True, ChPos);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TRScriptCustom.ReadFncCondition(const FncCond: TRFncCondition;
  const CheckCndBlock, CheckParameters: Boolean; var ChPos: Integer);
var
  bNot, bFncRead: Boolean;
  fType: TRFncType;
  fOpr: TRFncOperator;
  fPrevPos: Integer;
  Token: string;
begin
  if Assigned(FncCond) then
  begin
    // Пропускаем разделительные символы
    _SkipSpaces(True, ChPos);
    // Считываем условия
    Token := EmptyStr; bNot := False; fOpr := coNone;
    while _IsPosValid(ChPos) do
    begin
      fPrevPos := ChPos;
      Token := ReadFuncToken(ChPos);
      // -> not
      if CompareToken(Token, tkNot) then
      begin
        bNot := not bNot;
        Continue;
      end;
      // -> and
      if CompareToken(Token, tkAnd) then
      begin
        fOpr := coAnd;
        Continue;
      end;
      // -> or
      if CompareToken(Token, tkOr) then
      begin
        fOpr := coOr;
        Continue;
      end;
      // -> "(" "["
      if CompareToken(Token, chFuncBegin) or CompareToken(Token, chAttrBegin) then
      begin
        FncCond.BlockBegin(bNot, fOpr);
        bNot := False; fOpr := coNone;
        Continue;
      end;
      // -> ")" "]"
      if CompareToken(Token, chFuncEnd) or CompareToken(Token, chAttrEnd) then
      begin
        FncCond.BlockEnd;
        bNot := False; fOpr := coNone;
        Continue;
      end;
      // -> functions
      for fType := Low(LRFunctions) to High(LRFunctions) do
      begin
        bFncRead := CompareToken(Token, LRFunctions[fType].GetCommandName);
        if bFncRead then
        begin
          FncCond.AddFunction(ReadFncParams(LRFunctions[fType], bNot, fOpr, ChPos, CheckParameters));
          bNot := False; fOpr := coNone;
          Break;
        end;
      end;
      if bFncRead then Continue;
      // -> выход
      if CheckCndBlock then FncCond.CheckActBlock;
      ChPos := fPrevPos;
      Break;
    end;
  end;
end;

function TRScriptCustom.ReadFncCommand(CmdType: CRCmdCondition;
  const AliasName, ThenToken, ElseToken: string;
  var ChPos: Integer): TRCmdCondition;
var
  fPrevPos: Integer;
  Token: string;
begin
  Result := CmdType.Create(TRScript(Self));
  try
    // Считываем условия
    ReadFncCondition(Result.Condition, True, True, ChPos);
    // Считываем блоки then и else
    Token := EmptyStr;
    while _IsPosValid(ChPos) do
    begin
      fPrevPos := ChPos;
      Token := ReadToken(True, True, True, ChPos);
      // -> then
      if (ThenToken <> EmptyStr) and CompareToken(Token, ThenToken) then
      begin
        _SkipComments(ChPos);
        ParseCommands(Result.ThenList, AliasName, ChPos, True);
        _SkipComments(ChPos);
        Continue;
      end;
      // -> else
      if (ElseToken <> EmptyStr) and CompareToken(Token, ElseToken) then
      begin
        _SkipComments(ChPos);
        ParseCommands(Result.ElseList, AliasName, ChPos, True);
        _SkipComments(ChPos);
        Continue;
      end;
      // -> выход
      ChPos := fPrevPos;
      Break;
    end;
    // fixed bug: 2011-10-21, version 5.2.0.86
    // Устанавливаем параметры и описание "по умолчанию"
    Result.Attributes := GetDefaultCmdAttributes;
    Result.Description := GetDefaultCmdDescription;
    // Пропускаем разделительные символы
    _SkipSpaces(True, ChPos);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TRScriptCustom.ReadCycCommand(CmdType: CRCmdCycle;
  const AliasName: string; var ChPos: Integer): TRCmdCycle;
var
  fPrevPos: Integer;
  Token: string;
begin
  Result := CmdType.Create(TRScript(Self));
  try
    // Считываем блоки команд
    Token := EmptyStr;
    while _IsPosValid(ChPos) do
    begin
      fPrevPos := ChPos;
      Token := ReadToken(True, True, True, ChPos);
      if Result.CycleText = EmptyStr then
      begin
        Result.CycleText := Token;
        Continue;
      end;
      if CompareToken(Token, Result.GetCycleEnd) then
      begin
        _SkipComments(ChPos);
        ParseCommands(Result.CmdList, AliasName, ChPos, True);
        _SkipComments(ChPos);
        Continue;
      end;
      // -> выход
      ChPos := fPrevPos;
      Break;
    end;
    // fixed bug: 2011-10-21, version 5.2.0.86
    // Устанавливаем параметры и описание "по умолчанию"
    Result.Attributes := GetDefaultCmdAttributes;
    Result.Description := GetDefaultCmdDescription;
    // Пропускаем разделительные символы
    _SkipSpaces(True, ChPos);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TRScriptCustom.ParseCommand(const Token: string;
  CmdList: TRCmdBlock; const AliasName: string; var ChPos: Integer);
var
  Line: string;
  CmdType: TRCmdType;
  CmdRead: Boolean;
begin
  // -> var ...
  if CompareToken(Token, tkVar) then
  begin
    Line := ReadLine(True, ChPos);
    PutVariable(True, Line);
    Exit;
  end;
  // -> set ...
  if CompareToken(Token, tkSet) then
  begin
    CmdList.Add(ReadExeCommand(TRCmdSetVariable, AliasName, ChPos));
    Exit;
  end;
  // -> exe commands...
  for CmdType := Low(LRCommandsExe) to High(LRCommandsExe) do
  begin
    CmdRead := CompareToken(Token, LRCommandsExe[CmdType].GetCommandName);
    if CmdRead then
    begin
      CmdList.Add(ReadExeCommand(LRCommandsExe[CmdType], AliasName, ChPos));
      Exit;
    end;
  end;
  // -> condition commands...
  for CmdType := Low(LRCommandsCnd) to High(LRCommandsCnd) do
  begin
    CmdRead := CompareToken(Token, LRCommandsCnd[CmdType].GetCommandName);
    if CmdRead then
    begin
      CmdList.Add(ReadFncCommand(LRCommandsCnd[CmdType], AliasName,
        LRCommandsCnd[CmdType].GetBlockThen,
        LRCommandsCnd[CmdType].GetBlockElse, ChPos));
      Exit;
    end;
  end;
  // -> cycles
  for CmdType := Low(LRCommandsCyc) to High((LRCommandsCyc)) do
  begin
    CmdRead := CompareToken(Token, LRCommandsCyc[CmdType].GetCommandName);
    if CmdRead then
    begin
      CmdList.Add(ReadCycCommand(LRCommandsCyc[CmdType], AliasName, ChPos));
      Exit;
    end;
  end;
  // -> процедура
  if FindProc(GetFullProcName(AliasName, Token)) <> nil then
  begin
    ChPos := fParsePos;
    CmdList.Add(ReadExeCommand(TRCmdCallProc, AliasName, ChPos));
  end
  // -> неверный токен
  else raise ERScriptError.CreateFmt(EParseInvalidToken, [Token]);
end;

procedure TRScriptCustom.ParseCommands(CmdList: TRCmdBlock;
  const AliasName: string; var ChPos: Integer; const Simple: Boolean);
type
  TCmdParseStage = (stStart, stWaitCmd, stEnd);
var
  MaxPos: Integer;
  WaitEnd: Boolean;
  State: TCmdParseStage;
  Token: string;
begin
  MaxPos := Length(GetScriptText);
  WaitEnd := False;
  State := stStart;
  while State <> stEnd do
  begin
    // Пропускаем строки с комментариями
    _SkipComments(ChPos);
    fParsePos := ChPos;
    // Проверка на неожиданный конец текста
    if _Eos(ChPos) then
      raise ERScriptError.Create(EParseUnterminatedBlock);
    case State of
      // Начало блока команд -------------------------------------------------
      stStart:
      begin
        Token := ReadToken(True, True, True, ChPos);
        if CompareToken(Token, tkBlockBegin1)
        or CompareToken(Token, tkBlockBegin2) then
        begin
          WaitEnd := True;
          State := stWaitCmd;
          Continue;
        end
        else begin
          if Simple then
          begin
            WaitEnd := False;
            ChPos := fParsePos;
            State := stWaitCmd;
            Continue;
          end
          else raise ERScriptError.Create(EParseWaitBegin);
        end;
      end;
      // Команда -------------------------------------------------------------
      stWaitCmd:
      begin
        Token := ReadToken(True, False, True, ChPos);
        // -> конец блока команд
        if CompareToken(Token, tkBlockEnd1)
        or CompareToken(Token, tkBlockEnd2) then
        begin
          _SkipSpaces(True, ChPos);
          _SkipComments(ChPos);
          State := stEnd;
          Continue;
        end;
        // -> команда
        ParseCommand(Token, CmdList, AliasName, ChPos);
        // -> выход
        if not WaitEnd then State := stEnd;
      end;
    end;
    DoShowProgress(Self, mcTransaction, ChPos, MaxPos);
  end;
end;

procedure TRScriptCustom.Parse(const AliasName: string);
type
  TRootParseStage = (stScriptRoot, stVars, stProc);
var
  MaxPos, CurrPos: Integer;
  State: TRootParseStage;
  Token: string;
begin
  fParsePos := 0;
  MaxPos := Length(GetScriptText);
  CurrPos := 1;
  DoShowProgress(Self, mcTransaction, CurrPos, MaxPos);
  State := stScriptRoot;
  while not _PeekEnd(CurrPos) do
  begin
    // Пропускаем строки с комментариями
    fParsePos := CurrPos;
    _SkipComments(CurrPos);
    if fParsePos <> CurrPos then
    begin
      fParsePos := CurrPos;
      DoShowProgress(Self, mcTransaction, CurrPos, MaxPos);
    end;
    // Запоминаем текущую позицию
    case State of
      // "Корневой" уровень скрипта --------------------------------------------
      stScriptRoot:
      begin
        Token := ReadToken(True, False, False, CurrPos);
        if Token <> EmptyStr then
        begin
          // -> список переменных
          if CompareToken(Token, tkVars) then State := stVars;
          // -> процедура
          if CompareToken(Token, tkProc1) or CompareToken(Token, tkProc2)
          or CompareToken(Token, tkProc3) or CompareToken(Token, tkProc4)
          or CompareToken(Token, tkProc5) then State := stProc;
          // -> другое (статус не изменен)
          if (State = stScriptRoot) and (Trim(Token) <> EmptyStr) then
            raise ERScriptError.CreateFmt(EParseInvalidToken, [Token]);
        end;
      end;
      // Список переменных -----------------------------------------------------
      stVars:
      begin
        Token := ReadToken(True, False, False, CurrPos);
        if Token <> EmptyStr then
        begin
          // -> конец списка переменных
          if CompareToken(Token, tkBlockEnd1) or CompareToken(Token, tkBlockEnd2)
          then State := stScriptRoot;
          // -> процедура (вернемся назад)
          if CompareToken(Token, tkProc1) or CompareToken(Token, tkProc2)
          or CompareToken(Token, tkProc3) or CompareToken(Token, tkProc4)
          or CompareToken(Token, tkProc5) then State := stProc;
          // -> переменная
          if State = stVars then
          begin
            CurrPos := fParsePos;
            Token := ReadLine(True, CurrPos);
            if Token <> EmptyStr then PutVariable(True, Token);
          end;
        end;
      end;
      // Процедура -------------------------------------------------------------
      stProc:
      begin
        Token := Trim(ReadLine(False, CurrPos));
        if Token = EmptyStr then
          raise ERScriptError.Create(EParseInvalidProcName);
        // -> блок команд
        ParseCommands(CreateProc(GetFullProcName(AliasName, Token)).CmdList,
          AliasName, CurrPos, False);
        // -> конец процедуры
        State := stScriptRoot;
      end;
    end;
    DoShowProgress(Self, mcTransaction, CurrPos, MaxPos);
  end;
  DoShowProgress(Self, mcTransaction, MaxPos, MaxPos);
end;

function TRScriptCustom.PosToX(const Pos: Integer): Integer;
var
  ChPos: Integer;
begin
  Result := 0;
  if _IsPosValid(Pos) then
  begin
    Inc(Result);
    for ChPos := 1 to Pos do
      if _Test(chEol, ChPos) then Inc(Result);
  end;
end;

function TRScriptCustom.PosToY(const Pos: Integer): Integer;
var
  ChPos: Integer;
begin
  Result := 0;
  if _IsPosValid(Pos) then
  begin
    Inc(Result);
    for ChPos := 1 to Pos do
      if _Test(chEolCr, ChPos)
      then Result := 1
      else Inc(Result);
  end;
end;

procedure TRScriptCustom.DoInitVisualComponents;
begin
  fVsInit := True;
  fComplOpCount := 0;
  fTotalOpCount := 0;
end;

procedure TRScriptCustom.DoDoneVisualComponents;
begin
  fVsInit := False;
end;

procedure TRScriptCustom.DoCompileStart;
begin
  DoUpdateControls(Self, False, False);
  if Assigned(fCompileStart) then
    fCompileStart(Self);
end;

procedure TRScriptCustom.DoCompileEnd;
begin
  DoUpdateControls(Self, False, True);
  if Assigned(fCompileEnd) then
    fCompileEnd(Self);
end;

procedure TRScriptCustom.CopyVariables;
begin
  fVarWork.Assign(fVarList);
end;

function TRScriptCustom.Compile: Boolean;
var
  fCallVisual: Boolean;
begin
  Result := False;
  fCompiled := False;
  DoShowProgress(fScript, mcOperation, 0, 1);
  if Assigned(fScript) then
  begin
    fCallVisual := not fVsInit;
    if fCallVisual then DoInitVisualComponents;
    DoCompileStart;
    try
      try
        DoShowInfo(Self, Now, mcSystem, msTitle,
          Format(SCompileStart, [GetScriptName]));
        _ClearPositions;
        if fVarList.Count = 0 then
          InitVariables;
        CopyVariables;
        fProcList.Clear;
        Parse(fAlias);
        Result := True;
        fCompiled := True;
        DoShowInfo(Self, Now, mcSystem, msOk, Format(SCompileResult, [Script.Count, GetCommandCount]));
        DoShowProgress(fScript, mcOperation, 1, 1);
      except
        on E: Exception do
        begin
          DoShowInfo(Self, Now, mcSystem, msError, Format(SCompileError,
            [fParsePos, PosToX(fParsePos), E.Message]));
          if Self is TRScriptEditor then
            TRScriptEditor(Self).SelectErrorLine(fParsePos);
        end;
      end;
    finally
      DoCompileEnd;
      if fCallVisual then DoDoneVisualComponents;
    end;
  end
  else DoShowInfo(Self, Now, mcSystem, msError, SCompileEmpty);
end;

function TRScriptCustom.CreateScriptText: string;
var
  i, iCount: Integer;
  Buffer: TStringList;
begin
  Result := EmptyStr;
  if fCompiled or Compile then
  begin
    Buffer := TStringList.Create;
    try
      Buffer.Add(tkVars);
      
      iCount := fVarWork.Count - 1;
      for i := 0 to iCount do
        Buffer.Add(#32#32 + VarListExec[i]);

      iCount := fProcList.Count - 1;
      for i := 0 to iCount do
      begin
        Buffer.Add(EmptyStr);
        Buffer.Add(tkProc1 + #32 + GetShortProcName(TRCmdProc(fProcList[i]).Name));
        Buffer.Add(tkBlockBegin1);
        Buffer.Add(TRCmdProc(fProcList[i]).GetCommandsCode);
        Buffer.Add(tkBlockEnd1);
      end;
      Result := Buffer.Text;
    finally
      Buffer.Free;
    end;
  end;
end;

// Executor --------------------------------------------------------------------

procedure TRScriptCustom.FixCurrentState(const NewState: TRScriptState);
begin
  fCurrState := NewState;
  DoShowState(Self, False, fCurrState);
end;

function TRScriptCustom.ScriptStateToMsgState(const State: TRScriptState): TRMsgState;
begin
  case State of
    stOk: Result := msOk;
    stWarning: Result := msWarning;
    else Result := msError;
  end;
end;

function TRScriptCustom.ExecuteLine(const CmdLine: string): TRScriptState;
var
  ChPos: Integer;
  TempExtProc: Boolean;
  TempScript: string;
  TempCmdBlock: TRCmdBlock;
begin
  if Trim(CmdLine) <> EmptyStr then
  begin
    Result := stOk;
    FixCurrentState(Result);
    TempCmdBlock := TRCmdBlock.Create(Self);
    TempCmdBlock.fLockOperationProgress := True;
    TempExtProc := fExtProcessor;
    TempScript := Script.Text;
    Script.Text := CmdLine;
    fExtProcessor := False;
    try
      try
        ChPos := 1;
        fComplOpCount := 0;
        fTotalOpCount := 0;
        if fVarList.Count = 0 then
          InitVariables;
        CopyVariables;
        ParseCommands(TempCmdBlock, fAlias, ChPos, True);
        fExecuted := True;
        try
          TempCmdBlock.Execute(Result);
        finally
          fExecuted := False;
        end;
      except
        on E: Exception do
        begin
          Result := stError;
          FixCurrentState(Result);
          DoShowInfo(Self, Now, mcSystem, msError, Format(SCompileError,
            [fParsePos, PosToX(fParsePos), E.Message]));
        end;
      end;
    finally
      Script.Text := TempScript;
      fExtProcessor := TempExtProc;
      TempCmdBlock.Free;
    end;
  end
  else begin
    Result := stError;
    FixCurrentState(Result);
    DoShowInfo(Self, Now, mcSystem, msError, Format(EParseInvalidToken, [CmdLine]));
  end;
end;

function TRScriptCustom.ExecuteProc(const Name: string; const Forced, ExtMode: Boolean;
  const ExtVariable: string = ''; const DefState: TRScriptState = stOk): TRScriptState;
var
  BufList: TStrings;
  FullName: string;
  Proc: TRCmdProc;
  i, iCount: Integer;
  ForcedExecute: Boolean;
begin
  // 2011-11-25 fixed bug id 1882 (После команды Exit не выполнялся блок Done)
  ForcedExecute := not fExecuted and Forced;
  if ForcedExecute then fExecuted := True;
  try
    // 2011-11-25 fixed bug id 1882 (После команды Exit не выполнялся блок Done)
    if fExecuted then
    begin
      FullName := GetFullProcName(fAlias, Name);
      Proc := FindProc(FullName);
      if ExtMode or Assigned(Proc) then
        DoShowInfo(Self, Now, mcSystem, msTitle, Format(SExecuteStartProcedure, [FullName]));
      if ExtMode and not Assigned(Proc) and IsExternalProc(FullName) then
      begin
        Proc := FindExternalProc(FullName);
        if Assigned(Proc) then Proc.fScript.fExecuted := True;
      end;
      if Assigned(Proc) then
      begin
        Result := DefState;
        FixCurrentState(Result);
        if Trim(ExtVariable) <> EmptyStr then
        begin
          BufList := TStringList.Create;
          try
            BufList.Text := ExtVariable;
            iCount := BufList.Count - 1;
            for i := 0 to iCount do
              if Trim(BufList[i]) <> EmptyStr then
                Proc.fScript.PutVariable(True, BufList[i]);
          finally
            BufList.Free;
          end;
        end;
        Proc.Execute(Result);
        FixCurrentState(Result);
        DoShowInfo(Self, Now, mcSystem, ScriptStateToMsgState(Result),
          Format(SExecuteReturnProcedure, [FullName, SRProcedureState[Result]]));
      end
      else begin
        if ExtMode then
        begin
          Result := stError;
          FixCurrentState(Result);
          DoShowInfo(Self, Now, mcSystem, msError, Format(EScriptProcNotFound, [FullName]));
        end
        else begin
          Result := DefState;
          FixCurrentState(Result);
        end;
      end;
    end;
  finally
    // 2011-11-25 fixed bug id 1882 (После команды Exit не выполнялся блок Done)
    if ForcedExecute then fExecuted := False;
  end;
end;

procedure TRScriptCustom.HandleError(var State: TRScriptState);
var
  OnErrorName: string;
begin
  OnErrorName := Trim(GetVariableValue(VarListExec, varOnError));
  if OnErrorName <> EmptyStr then
    State := ExecuteProc(OnErrorName, False, False, EmptyStr, State);
  FixCurrentState(State);
end;

procedure TRScriptCustom.RestoreFiles;
const
  CopyFlags = rfCopyDefaultRO;
  ErrFlags  = [];
begin
  DoUpdateControls(Self, False, False);
  DoShowInfo(Self, Now, mcCommand, msTitle, SExecuteRestoreFiles);
  CopyFilesList(SExecuteRestoreFiles, fUndoList, True, 
    CopyFlags, ezWarning, ErrFlags, BufferSizeKb, NetUsage, TryMax, TryDelay,
      DoCalcNetUsage, DoShowInfo, DoShowProgress, DoFileProgress, DoCheckBreak);
end;

function TRScriptCustom.BreakBlock: Boolean;
var
  CurrBlock: TRCmdBlock;
begin
  CurrBlock := CmdBlock_Last;
  if Assigned(CurrBlock) then
  begin
    CurrBlock.BreakBlock;
    Result := True;
  end
  else Result := False;
end;

procedure TRScriptCustom.BreakScript;
begin
  if fExecuted then
  begin
    fExecuted := False;
    DoUpdateControls(Self, False, False);
    DoShowInfo(Script, Now, mcOperation, msBreak, SExecuteBreakScript);
  end;
end;

procedure TRScriptCustom.DoExecuteStart;
begin
  DoUpdateControls(Self, fCanBreak, False);
  if Assigned(fExecuteStart) then
    fExecuteStart(Self);
end;

procedure TRScriptCustom.DoExecuteEnd;
begin
  DoUpdateControls(Self, False, True);
  if Assigned(fExecuteEnd) then
    fExecuteEnd(Self);
end;

function TRScriptCustom.Execute(const CallCompile: Boolean; const RunProc: string = tkMainProc): TRScriptState;
var
  DoneState: TRScriptState;
begin
  DoInitVisualComponents;
  // fPrgsOffset := 0;
  try
    if CallCompile or not Compiled then
    begin
      if not Compile then
      begin
        Result := stErrorStop;
        Exit;
      end;
    end;
    fExecuted := True;
    DoExecuteStart;
    fUndoList.Clear;
    try
      DoShowInfo(Self, Now, mcSystem, msTitle,
        Format(SExecuteRun, [GetScriptName]));
      try
        // Запуск процедуры инициализации
        Result := ExecuteProc(tkInitProc, False, False, EmptyStr, stOk);

        // 2011-11-25 fixed bug id 1882 (После команды Exit в блоке Init не должен выполняться никакой другой блок)
        if fExecuted and (Result in [stOk, stWarning]) then
        begin
          // Запуск основной процедуры
          Result := ExecuteProc(RunProc, False, True, EmptyStr, Result);

          // Запуск процедуры завершения
          DoneState := ExecuteProc(tkDoneProc, True, False, EmptyStr, stOk);
          if DoneState > Result then
          begin
            Result := DoneState;
            FixCurrentState(Result);
          end;
        end;
        // Откат изменений
        case Result of
          stErrorRestore: RestoreFiles;
          stErrorUndo: ExecuteProc(tkUndoProc, True, False, EmptyStr, stOk);
        end;
        DoShowInfo(Self, Now, mcSystem, ScriptStateToMsgState(Result),
          Format(SExecuteReturnScript, [GetScriptName, SRScriptState[Result]]));
      except
        on E: Exception do
        begin
          Result := stErrorStop;
          FixCurrentState(Result);
          DoShowInfo(Self, Now, mcSystem, msError, E.Message);
        end;
      end;
    finally
      fExecuted := False;
      fUndoList.Clear;
      DoExecuteEnd;
    end;
  finally
    DoDoneVisualComponents;
  end;
end;

function TRScriptCustom.GetVarListTask: string;
var
  TmpList: TStringList;
begin
  Result := EmptyStr;
  TmpList := TStringList.Create;
  try
    TmpList.Assign(fVarWork);
    DelStandartVariables(TmpList, True, True);
    Result := TmpList.Text;
  finally
    TmpList.Free;
  end;
end;

procedure TRScriptCustom.CmdBlock_Fix(const CmdBlock: TRCmdBlock);
begin
  if Assigned(CmdBlock) then
    fBlocksStack.Insert(0, CmdBlock);
end;

function TRScriptCustom.CmdBlock_Last: TRCmdBlock;
begin
  Result := nil;
  if fBlocksStack.Count > 0 then
    Result := TRCmdBlock(fBlocksStack.Items[0]);
end;

procedure TRScriptCustom.CmdBlock_Release;
begin
  if fBlocksStack.Count > 0 then
  begin
    fBlocksStack.Items[0] := nil;
    fBlocksStack.Delete(0);
  end;
end;

procedure TRScriptCustom.CmdProc_Fix(const CmdProc: TRCmdProc);
begin
  if Assigned(CmdProc) then
    fProcsStack.Insert(0, CmdProc);
end;

function TRScriptCustom.CmdProc_Last: TRCmdProc;
begin
  Result := nil;
  if fProcsStack.Count > 0 then
    Result := TRCmdProc(fProcsStack.Items[0]);
end;

procedure TRScriptCustom.CmdProc_Release;
begin
  if fProcsStack.Count > 0 then
  begin
    fProcsStack.Items[0] := nil;
    fProcsStack.Delete(0);
  end;
end;

{ TRScriptLogger }

constructor TRScriptLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UserData := nil;
  (*
  SetLength(fLogBuffer, 0);
  fLogMode := lmDisabled;
  fLogSize := 32;
  *)
  fBreak := False;
end;

destructor TRScriptLogger.Destroy;
begin
  (* FlushLogMessages; *)
  UserData := nil;
  inherited Destroy;
end;

(*
procedure TRScriptLogger.PutCmdMessage(const ScriptId: Integer;
  const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
  const MsgState: TRMsgState; const MsgText: string);

  procedure InsertMessage(const ScriptId: Integer;
    const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
    const MsgState: TRMsgState; const MsgText: string);
  begin
    SetLength(fLogBuffer, Length(fLogBuffer) + 1);
    fLogBuffer[High(fLogBuffer)].ScriptId := ScriptId;
    fLogBuffer[High(fLogBuffer)].TimeStamp := TimeStamp;
    fLogBuffer[High(fLogBuffer)].MsgClass := MsgClass;
    fLogBuffer[High(fLogBuffer)].MsgState := MsgState;
    fLogBuffer[High(fLogBuffer)].MsgText := MsgText;
  end;

begin
  if MsgText <> EmptyStr then
  begin
    case fLogMode of
      lmSingle:
      begin
        InsertMessage(ScriptId, TimeStamp, MsgClass, MsgState, MsgText);
        FlushLogMessages;
      end;
      lmOperation:
      begin
        InsertMessage(ScriptId, TimeStamp, MsgClass, MsgState, MsgText);
        if (MsgClass in [mcSystem, mcCommand, mcOperation])
        and (MsgState in [msOk, msIgnored, msWarning, msError, msBreak]) then
          FlushLogMessages;
      end;
      lmPacketSize:
      begin
        InsertMessage(ScriptId, TimeStamp, MsgClass, MsgState, MsgText);
        if ((MsgClass in [mcSystem])
          and (MsgState in [msOk, msIgnored, msWarning, msError, msBreak]))
        or (Length(fLogBuffer) >= fLogSize) then
          FlushLogMessages;
      end;
      lmMixed:
      begin
        InsertMessage(ScriptId, TimeStamp, MsgClass, MsgState, MsgText);
        if ((MsgClass in [mcSystem])
          and (MsgState in [msOk, msIgnored, msWarning, msError, msBreak]))
        or ((MsgClass in [mcCommand, mcOperation, mcTransaction])
          and (MsgState in [msOk, msIgnored, msWarning, msError, msBreak])
          and (Length(fLogBuffer) >= fLogSize)) then
            FlushLogMessages;
      end;
    end;
  end;
end;
*)

(*
procedure TRScriptLogger.FlushLogMessages;
var
  i: Integer;
begin
  if (Length(fLogBuffer) > 0) then
  begin
    if Assigned(fStoreBuffer) then
      fStoreBuffer(Self, fLogBuffer)
    else begin
      if Assigned(fStoreMessage) then
        for i := Low(fLogBuffer) to High(fLogBuffer) do
          fStoreMessage(Self, fLogBuffer[i]);
    end;
    SetLength(fLogBuffer, 0);
  end;
end;
*)

procedure TRScriptLogger.DoShowInfo(Sender: TObject;
  const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
  const MsgState: TRMsgState; const MsgText: string);
var
  ScriptId: Integer;
  ScriptName: string;
begin
  if Assigned(fShowInfo) then
    fShowInfo(Sender, TimeStamp, MsgClass, MsgState, MsgText);
  if Assigned(Sender) and (MsgText <> EmptyStr) and Assigned(fSaveToLog) then
  begin
    if Sender is TRScriptItem then
    begin
      ScriptId := TRScriptItem(Sender).ScriptId;
      ScriptName := TRScriptItem(Sender).Alias;
    end
    else begin
      ScriptId := TComponent(Sender).Tag;
      ScriptName := '';
    end;
    fSaveToLog(Sender, ScriptId, ScriptName, TimeStamp, MsgClass, MsgState, MsgText);
  end;
end;

procedure TRScriptLogger.DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState);
begin
  if Assigned(fShowState) then
    fShowState(Sender, Global, State);
end;

procedure TRScriptLogger.DoShowProgress(Sender: TObject;
  const MsgClass: TRMsgClass; const CurrPos, MaxPos: Integer);
begin
  if Assigned(fShowProgress) then
    fShowProgress(Sender, MsgClass, CurrPos, MaxPos);
end;

procedure TRScriptLogger.DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer);
begin
  if Assigned(fFileProgress) then
    fFileProgress(Sender, CurrPos, MaxPos);
end;

procedure TRScriptLogger.DoCheckBreak(Sender: TObject; var IsBreak: Boolean);
begin
  if not IsBreak and not fBreak then
  begin
    if Assigned(fCheckBreak) then
      fCheckBreak(Sender, fBreak);
  end;
  IsBreak := IsBreak or fBreak;
  if fBreak then fBreak := False;
end;

procedure TRScriptLogger.DoUpdateControls(Sender: TObject;
  const BreakEnabled, CloseEnabled: Boolean);
begin
  if Assigned(fUpdateControls) then
    fUpdateControls(Sender, BreakEnabled, CloseEnabled);
end;

procedure TRScriptLogger.BreakScript;
begin
  fBreak := True;
end;

procedure TRScriptLogger.DoInitVisualComponents(Sender: TObject);
begin
  if Assigned(fInitVisual) then
    fInitVisual(Sender);
end;

procedure TRScriptLogger.DoDoneVisualComponents(Sender: TObject);
begin
  if Assigned(fDoneVisual) then
    fDoneVisual(Sender);
end;

procedure TRScriptLogger.DoCompileStart(Sender: TObject);
begin
  DoUpdateControls(Sender, False, False);
  if Assigned(fCompileStart) then
    fCompileStart(Sender);
end;

procedure TRScriptLogger.DoCompileEnd(Sender: TObject);
begin
  (* FlushLogMessages; *)
  DoUpdateControls(Sender, False, True);
  if Assigned(fCompileEnd) then
    fCompileEnd(Sender);
end;

procedure TRScriptLogger.DoExecuteStart(Sender: TObject; const BreakEnabled: Boolean);
begin
  fBreak := False;
  DoUpdateControls(Sender, BreakEnabled, False);
  if Assigned(fExecuteStart) then
    fExecuteStart(Sender);
end;

procedure TRScriptLogger.DoExecuteEnd(Sender: TObject);
begin
  (* FlushLogMessages; *)
  DoUpdateControls(Sender, False, True);
  if Assigned(fExecuteEnd) then
    fExecuteEnd(Sender);
end;

{ TRScript }

constructor TRScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fScript := TStringList.Create;
  fLogger := nil;
end;

destructor TRScript.Destroy;
begin
  Logger := nil;
  fScript.Clear;
  fScript.Free;
  inherited Destroy;
end;

procedure TRScript.SetLogger(const Value: TRScriptLogger);
begin
  if fLogger <> Value then fLogger := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TRScript.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(fLogger) and (AComponent = fLogger) then
    Logger := nil;
end;

procedure TRScript.CheckToken(const Token: string);
begin
  inherited CheckToken(Token);
  if Token = EmptyStr then
    raise ERScriptError.Create(EParseEmptyToken);
end;

procedure TRScript.DoShowInfo(Sender: TObject;
  const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
  const MsgState: TRMsgState; const MsgText: string);
begin
  inherited DoShowInfo(Self, TimeStamp, MsgClass, MsgState, MsgText);
  if Assigned(fLogger) then
    fLogger.DoShowInfo(Self, TimeStamp, MsgClass, MsgState, MsgText);
end;

procedure TRScript.DoShowProgress(Sender: TObject;
  const MsgClass: TRMsgClass; const CurrPos, MaxPos: Integer);
begin
  inherited DoShowProgress(Self, MsgClass, CurrPos, MaxPos);
  if Assigned(fLogger) then
    fLogger.DoShowProgress(Self, MsgClass, CurrPos, MaxPos);
end;

procedure TRScript.DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer);
begin
  inherited DoFileProgress(Self, CurrPos, MaxPos);
  if Assigned(fLogger) then
    fLogger.DoFileProgress(Self, CurrPos, MaxPos);
end;

procedure TRScript.DoCheckBreak(Sender: TObject; var IsBreak: Boolean);
begin
  inherited DoCheckBreak(Self, IsBreak);
  if not IsBreak then
  begin
    if Assigned(fLogger) then
      fLogger.DoCheckBreak(Self, IsBreak);
    if IsBreak then
      BreakScript;
  end;
end;

procedure TRScript.DoUpdateControls(Sender: TObject;
  const BreakEnabled, CloseEnabled: Boolean);
begin
  inherited DoUpdateControls(Self, BreakEnabled, CloseEnabled);
  if Assigned(fLogger) then
    fLogger.DoUpdateControls(Self, BreakEnabled, CloseEnabled);
end;

procedure TRScript.DoInitVisualComponents;
begin
  inherited DoInitVisualComponents;
  if Assigned(fLogger) then
    fLogger.DoInitVisualComponents(Self);
end;

procedure TRScript.DoDoneVisualComponents;
begin
  inherited DoDoneVisualComponents;
  if Assigned(fLogger) then
    fLogger.DoDoneVisualComponents(Self);
end;

procedure TRScript.DoCompileStart;
begin
  inherited DoCompileStart;
  if Assigned(fLogger) then
    fLogger.DoCompileStart(Self);
end;

procedure TRScript.DoCompileEnd;
begin
  inherited DoCompileEnd;
  if Assigned(fLogger) then
    fLogger.DoCompileEnd(Self);
end;

procedure TRScript.DoExecuteStart;
begin
  inherited DoExecuteStart;
  if Assigned(fLogger) then
    fLogger.DoExecuteStart(Self, fCanBreak);
end;

procedure TRScript.DoExecuteEnd;
begin
  inherited DoExecuteEnd;
  if Assigned(fLogger) then
    fLogger.DoExecuteEnd(Self);
end;

{ TRScriptEditor }

constructor TRScriptEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLogger := nil;
end;

destructor TRScriptEditor.Destroy;
begin
  Logger := nil;
  inherited Destroy;
end;

procedure TRScriptEditor.SetLogger(const Value: TRScriptLogger);
begin
  if fLogger <> Value then fLogger := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TRScriptEditor.SetEditor(const Value: TCustomMemo);
begin
  if fEditor <> Value then
  begin
    fEditor := Value;
    if fEditor = nil
    then fScript := nil
    else fScript := fEditor.Lines;
  end;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TRScriptEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(fLogger) and (AComponent = fLogger) then
    Logger := nil;
  if (Operation = opRemove) and Assigned(fEditor) and (AComponent = fEditor) then
    Editor := nil;
end;

procedure TRScriptEditor.CheckToken(const Token: string);
begin
  inherited CheckToken(Token);
  if Token = EmptyStr then
    raise ERScriptError.Create(EParseEmptyToken);
end;

procedure TRScriptEditor.DoShowInfo(Sender: TObject;
  const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
  const MsgState: TRMsgState; const MsgText: string);
begin
  inherited DoShowInfo(Self, TimeStamp, MsgClass, MsgState, MsgText);
  if Assigned(fLogger) then
    fLogger.DoShowInfo(Self, TimeStamp, MsgClass, MsgState, MsgText);
end;

procedure TRScriptEditor.DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState);
begin
  inherited DoShowState(Self, Global, State);
  if Assigned(fLogger) then
    fLogger.DoShowState(Self, Global, State);
end;

procedure TRScriptEditor.DoFileProgress(Sender: TObject;
  const CurrPos, MaxPos: Integer);
begin
  inherited DoFileProgress(Self, CurrPos, MaxPos);
  if Assigned(fLogger) then
    fLogger.DoFileProgress(Self, CurrPos, MaxPos);
end;

procedure TRScriptEditor.DoShowProgress(Sender: TObject;
  const MsgClass: TRMsgClass; const CurrPos, MaxPos: Integer);
begin
  inherited DoShowProgress(Self, MsgClass, CurrPos, MaxPos);
  if Assigned(fLogger) then
    fLogger.DoShowProgress(Self, MsgClass, CurrPos, MaxPos);
end;

procedure TRScriptEditor.DoCheckBreak(Sender: TObject; var IsBreak: Boolean);
begin
  inherited DoCheckBreak(Self, IsBreak);
  if Executed then
  begin
    if Assigned(fLogger) then
      fLogger.DoCheckBreak(Self, IsBreak);
    if IsBreak and Executed then
      BreakScript;
  end;
end;

procedure TRScriptEditor.DoUpdateControls(Sender: TObject;
  const BreakEnabled, CloseEnabled: Boolean);
begin
  inherited DoUpdateControls(Self, BreakEnabled, CloseEnabled);
  if Assigned(fLogger) then
    fLogger.DoUpdateControls(Self, BreakEnabled, CloseEnabled);
end;

procedure TRScriptEditor.DoInitVisualComponents;
begin
  inherited DoInitVisualComponents;
  if Assigned(fLogger) then
    fLogger.DoInitVisualComponents(Self);
end;

procedure TRScriptEditor.DoDoneVisualComponents;
begin
  inherited DoDoneVisualComponents;
  if Assigned(fLogger) then
    fLogger.DoDoneVisualComponents(Self);
end;

procedure TRScriptEditor.DoCompileStart;
begin
  inherited DoCompileStart;
  if Assigned(fLogger) then
    fLogger.DoCompileStart(Self);
end;

procedure TRScriptEditor.DoCompileEnd;
begin
  inherited DoCompileEnd;
  if Assigned(fLogger) then
    fLogger.DoCompileEnd(Self);
end;

procedure TRScriptEditor.DoExecuteStart;
begin
  inherited DoExecuteStart;
  if Assigned(fLogger) then
    fLogger.DoExecuteStart(Self, fCanBreak);
end;

procedure TRScriptEditor.DoExecuteEnd;
begin
  inherited DoExecuteEnd;
  if Assigned(fLogger) then
    fLogger.DoExecuteEnd(Self);
end;

(* procedure TRScriptEditor._DebugShow(const Tag: string; const CurrPos: Integer);
begin
  WarningBox(Format('%s:'#13'Текущая позиция: %d'#13'%d [%s]'#13'%d [%s]'#13'%d [%s]'#13'%d [%s]'#13'%d [%s]',
    [Tag, CurrPos, Ord(ScriptText[CurrPos]), ScriptText[CurrPos],
     Ord(ScriptText[CurrPos + 1]), ScriptText[CurrPos + 1],
     Ord(ScriptText[CurrPos + 2]), ScriptText[CurrPos + 2],
     Ord(ScriptText[CurrPos + 3]), ScriptText[CurrPos + 3],
     Ord(ScriptText[CurrPos + 4]), ScriptText[CurrPos + 4]]));
end; *)

function TRScriptEditor._LineStart(const SkipSpaces: Boolean;
  const CurrPos: Integer): Integer;
begin
  Result := CurrPos;
  if (Result > 1) and _Eol(Result) then
    Dec(Result);
  while (Result > 1) and not _Eol(Result) do
    Dec(Result);
  { if _IsPosValid(Result) and _Eol(Result) then
    Inc(Result, 2); }
  if _IsPosValid(Result) and _Test(chEol, Result) then
    Inc(Result);
  if _IsPosValid(Result) and _Test(chEcr, Result) then
    Inc(Result);
  if SkipSpaces then
  begin
    _SkipSpaces(True, Result);
    _SkipComments(Result);
  end;
end;

function TRScriptEditor._LineEnd(const CurrPos: Integer): Integer;
begin
  Result := CurrPos;
  while _IsPosValid(Result) and not _Eol(Result) do
    Inc(Result);
end;

function TRScriptEditor._LinePrevious(const CurrPos: Integer): Integer;

  function LineNotEmpty(const CurrPos: Integer): Boolean;
  var
    ChPos: Integer;
  begin
    // По умолчанию строка пустая
    Result := False;
    ChPos := CurrPos;
    // Проходим по всей текущей строке до конца
    while _IsPosValid(ChPos) and not _Eol(ChPos) do
    begin
      // Если код символ меньше пробела - пропускаем
      if _Eow(ChPos) then
        Inc(ChPos)
      else begin
        // Проверяем на комментарии
        Result := not _PeekComments(False, ChPos);
        Break;
      end;
    end;
  end;

begin
  if CurrPos > 0 then
  begin
    Result := CurrPos;
    repeat
      // Переходим в начало текущей строки, если в середине
      while (Result > 1) and not _Test(chEolCr, Result - 1) do
        Dec(Result);
      // Переходим на конец предыдущей строки
      if (Result > 1) and _Test(chEcr, Result - 1) then
        Dec(Result);
      if (Result > 1) and _Test(chEol, Result - 1) then
        Dec(Result);
      // Переходим в начало предыдущей строки, если она не пустая
      while (Result > 1) and not _Test(chEolCr, Result - 1) do
        Dec(Result);
    until (Result <= 1) or LineNotEmpty(Result);
  end
  else Result := 0;
end;

function TRScriptEditor._PeakToken(var ChPos: Integer): TRCmdTokenType;
var
  BeforeToken, LineEmpty, LinePrev: Boolean;
  Token: string;
  TokenPos, IntPos: Integer;
  i: TRCmdType;
begin
  Result := ttUnknown;
  LinePrev := False;
  LineEmpty := False;
  TokenPos := _LineStart(False, ChPos);
  BeforeToken := ChPos < TokenPos;
  while (TokenPos > 0) and (Result in [ttUnknown]) do
  begin
    IntPos := TokenPos;
    Token := ReadToken(False, False, False, IntPos);
    // Проверка на пустой токен
    if not LinePrev and (Trim(Token) = EmptyStr) then
      LineEmpty := True;
    // Проверка на exe-комманды
    for i := Low(LRCommandsExe) to High(LRCommandsExe) do
      if CompareToken(Token, LRCommandsExe[i].GetCommandName) then
      begin
        Result := ttExeCommand;
        Break;
      end;
    // Проверка на условия
    for i := Low(LRCommandsCnd) to High(LRCommandsCnd) do
      if CompareToken(Token, LRCommandsCnd[i].GetCommandName) then
      begin
        Result := ttCondition;
        Break;
      end;
    // Проверка на циклы
    for i := Low(LRCommandsCyc) to High(LRCommandsCyc) do
      if CompareToken(Token, LRCommandsCyc[i].GetCommandName) then
      begin
        Result := ttCycle;
        Break;
      end;
    // Проверка на var-команды
    if CompareToken(Token, tkSet) or CompareToken(Token, tkVar) then
      Result := ttVarCommand;
    // Проверка на список переменных
    if CompareToken(Token, tkVars) then
    begin
      if LineEmpty
      then Result := ttVarsId
      else if LinePrev
           then Result := ttVarList
           else Result := ttVarsId;
    end;
    // Проверка на начало процедуры
    if CompareToken(Token, tkProc1) or CompareToken(Token, tkProc2)
    or CompareToken(Token, tkProc3) or CompareToken(Token, tkProc4)
    or CompareToken(Token, tkProc5) then
    begin
      if BeforeToken
      then Result := ttUnknown
      else Result := ttNotEnabled;
    end;
    // Проверка на неподдерживаемые команды
    if CompareToken(Token, tkBlockBegin1) or CompareToken(Token, tkBlockBegin2)
    or CompareToken(Token, tkBlockEnd1) or CompareToken(Token, tkBlockEnd2) then
      Result := ttNotEdited;
    // Устанавливаем положение укзателя
    case Result of
      ttUnknown:
      begin
        if TokenPos > 1 then
        begin
          TokenPos := _LinePrevious(TokenPos);
          LinePrev := True;
        end
        else Break;
      end;
      ttCondition,
      ttVarsId: ChPos := TokenPos;
      ttVarList: ChPos := _LineStart(False, ChPos);
      else begin
        if LinePrev then Result := ttNotEdited;
        ChPos := TokenPos;
      end;
    end;
  end;
end;

procedure TRScriptEditor.SelectErrorLine(const ErrPos: Integer);
begin
  if Assigned(fEditor) then
  begin
    fEditor.SelStart := _LineStart(False, ErrPos) - 1;
    fEditor.SelLength := _LineEnd(ErrPos) - _LineStart(False, ErrPos);
  end;
end;

function TRScriptEditor.TextCommandExe(const CmdClass: TClass; const CmdData: TRCmdData): string;
var
  i: Integer;
begin
  Result := CRCmdExecuted(CmdClass).GetCommandName;
  for i := Low(CmdData.fParams) to High(CmdData.fParams) do
    Result := Result + #32 + ScriptQuotedStr(CmdData.fParams[i]);
  if not CmdData.fFlagsDefault then
    Result := Result + #32 + chAttrBegin + CmdData.fFlags + chAttrEnd;
  if not CmdData.fNotesDefault then
    Result := Result + #32 + ScriptQuotedStr(CmdData.fNotes);
end;

function TRScriptEditor.TextVariable(const VarName, VarValue: string): string;
begin
  Result := ScriptQuotedStr(VarName) + VarsChar + ScriptQuotedStr(VarValue);
end;

function TRScriptEditor.SelectVariableDefault(var VarName: string): Boolean;
begin
  Result := SelectVarName(fVarList, fTagChar, VarName);
end;

function TRScriptEditor.SelectVariableName(const Tags: Boolean; var VarName: string): Boolean;
var
  FullName: string;
begin
  FullName := UpdateVarName(VarName, fTagChar);
  if Assigned(fSelectVariable)
  then fSelectVariable(Self, FullName, Result)
  else Result := SelectVariableDefault(FullName);
  if Result then
  begin
    if Tags
    then VarName := UpdateVarName(FullName, fTagChar)
    else VarName := ExtractVarName(FullName, fTagChar);
  end;
end;

procedure TRScriptEditor.UpdateControls;
var
  StartPos: Integer;
  TokenType: TRCmdTokenType;
begin
  fEditMode := True;
  try
    StartPos := fEditor.SelStart + 1;
    TokenType := _PeakToken(StartPos);
    fEnableAddVar := TokenType in [ttVarsId, ttVarList];
    fEnableAddStr := TokenType in [ttNotEdited, ttVarCommand, ttExeCommand, ttCondition, ttCycle];
    fEnableEdit := TokenType in [ttVarList, ttVarCommand, ttExeCommand, ttCondition, ttCycle];
  finally
    fEditMode := False;
  end;
end;

function TRScriptEditor.EditEnabled: Boolean;
begin
  Result := Assigned(fEditor) and fEnableEdit;
end;

function TRScriptEditor.ProcEnabled: Boolean;
begin
  Result := Assigned(fEditor) and fEnableAddStr;
end;

function TRScriptEditor.VarsEnabled: Boolean;
begin
  Result := Assigned(fEditor) and fEnableAddVar;
end;

procedure TRScriptEditor.InsertNewLine;
var
  ChPos: Integer;
  Offset: string;

  function IsLineStart(const StartPos: Integer): Boolean;
  begin
    Result := _Eos(StartPos) or _Test(chEolCr, StartPos);
  end;

  function IsEmptyLeft(const StartPos: Integer): Boolean;
  var
    ChPos: Integer;
  begin
    Result := True;
    ChPos := StartPos;
    // Если указатель на конце текущей строки и строка не пустая - сдвигаемся назад
    if (ChPos > 1) and _Eol(ChPos) and not _Test(chEolCr, ChPos - 1) then
      Dec(ChPos);
    // Проверяем символы до следующего конца строки
    while Result and (ChPos > 1) and not _Test(chEolCr, ChPos) do
    begin
      Dec(ChPos);
      Result := _Test([#0..#32], ChPos);
    end;
  end;

  function IsEmptyRight(const StartPos: Integer): Boolean;
  var
    ChPos: Integer;
  begin
    Result := True;
    ChPos := StartPos;
    // Проверяем символы до конца строки
    while Result and _IsPosValid(ChPos) and not _Eol(ChPos) do
    begin
      Result := _Test([#0..#32], ChPos);
      Inc(ChPos);
    end;
  end;

  function CalcOffset(const PrevLine: Boolean; const StartPos: Integer): string;
  var
    ChPos: Integer;
  begin
    Result := EmptyStr;
    if PrevLine
    then ChPos := _LinePrevious(StartPos)
    else ChPos := _LineStart(False, StartPos);
    while (ChPos > 1) and _Test([#32], ChPos) and not _Eol(ChPos) do
    begin
      Result := Result + #32;
      Inc(ChPos);
    end;
  end;

  procedure GotoEndLine(var ChPos: Integer);
  begin
    while _IsPosValid(ChPos) and not _Eol(ChPos) do
      Inc(ChPos);
  end;

begin
  ChPos := fEditor.SelStart + 1;
  if IsEmptyLeft(ChPos) then
  begin
    Offset := CalcOffset(True, ChPos);
    if IsEmptyRight(ChPos) then
    begin
      if IsLineStart(ChPos) then
      begin
        fEditor.SelLength := 0;
        fEditor.SelText := Offset;
      end;
    end
    else begin
      ChPos := _LineStart(False, ChPos);
      if ChPos > 1
      then fEditor.SelStart := ChPos - 1
      else fEditor.SelStart := 0;
      fEditor.SelLength := 0;
      fEditor.SelText := #13#10;
      if ChPos > 1
      then fEditor.SelStart := ChPos - 1
      else fEditor.SelStart := 0;
      fEditor.SelLength := 0;
      fEditor.SelText := Offset;
    end;
  end
  else begin
    Offset := CalcOffset(False, ChPos);
    if IsEmptyRight(ChPos) then
    begin
      fEditor.SelLength := 0;
      fEditor.SelText := #13#10 + Offset;
    end
    else begin
      GotoEndLine(ChPos);
      if ChPos > 1
      then fEditor.SelStart := ChPos - 1
      else fEditor.SelStart := 0;
      fEditor.SelLength := 0;
      fEditor.SelText := #13#10 + Offset;
    end;
  end;
end;

procedure TRScriptEditor.AddCommandExe;
var
  CmdClass: TClass;
  CmdData: TRCmdData;
begin
  // CopyVariables;
  CmdClass := LRCommandsExe[Low(LRCommandsExe)];
  SetLength(CmdData.fParams, 0);
  CmdData.fFlagsDefault := True;
  CmdData.fFlags := EmptyStr;
  CmdData.fNotesDefault := True;
  CmdData.fNotes := EmptyStr;
  if EditExeCommand(Self, CmdClass, CmdData) then
  begin
    InsertNewLine;
    fEditor.SelText := TextCommandExe(CmdClass, CmdData);
  end;
end;

procedure TRScriptEditor.EditCommandExe(var ChPos: Integer);
var
  i: TRCmdType;
  CmdClass: TClass;
  CmdData: TRCmdData;
  Token: string;
begin
  // CopyVariables;
  _ClearPositions;
  CmdClass := nil;
  Token := ReadToken(False, False, False, ChPos);
  for i := Low(LRCommandsExe) to High(LRCommandsExe) do
    if CompareToken(Token, LRCommandsExe[i].GetCommandName) then
    begin
      CmdClass := LRCommandsExe[i];
      Break;
    end;
  if Assigned(CmdClass) then
  begin
    CmdData := ReadExeParams(LRCommandsExe[i].GetParamsLimit, fAlias, ChPos);
    if EditExeCommand(Self, CmdClass, CmdData) then
    begin
      fEditor.SelStart := FirstSymbol;
      fEditor.SelLength := LastSymbol - FirstSymbol + 1;
      fEditor.SelText := TextCommandExe(CmdClass, CmdData);
    end;
  end
  else ErrorBox(SMsgCannotEdit);
end;

procedure TRScriptEditor.AddCondition;
var
  CmdClass: TClass;
  CmdCond: TRFncCondition;
begin
  // CopyVariables;
  CmdCond := TRFncCondition.Create(nil);
  try
    CmdClass := LRCommandsCnd[Low(LRCommandsCnd)];
    if EditFncCondition(Self, CmdClass, CmdCond) then
    begin
      InsertNewLine;
      fEditor.SelText := CRCmdCondition(CmdClass).GetCommandName
        + #32 + CmdCond.GetConditionText_Edit
        + #32 + CRCmdCondition(CmdClass).GetBlockThen;
    end;
  finally
    CmdCond.Free;
  end;
end;

procedure TRScriptEditor.EditCondition(var ChPos: Integer);
var
  i: TRCmdType;
  CmdClass: TClass;
  CmdCond: TRFncCondition;
  Token: string;
begin
  // CopyVariables;
  _ClearPositions;
  CmdClass := nil;
  Token := ReadToken(False, False, False, ChPos);
  for i := Low(LRCommandsCnd) to High(LRCommandsCnd) do
    if CompareToken(Token, LRCommandsCnd[i].GetCommandName) then
    begin
      CmdClass := LRCommandsCnd[i];
      Break;
    end;
  if Assigned(CmdClass) then
  begin
    CmdCond := TRFncCondition.Create(nil);
    try
      ReadFncCondition(CmdCond, False, False, ChPos);
      if EditFncCondition(Self, CmdClass, CmdCond) then
      begin
        fEditor.SelStart := FirstSymbol;
        fEditor.SelLength := LastSymbol - FirstSymbol + 1;
        fEditor.SelText := CRCmdCondition(CmdClass).GetCommandName
          + #32 + CmdCond.GetConditionText_Edit
          + #32 + CRCmdCondition(CmdClass).GetBlockThen;
      end;
    finally
      CmdCond.Free;
    end;
  end
  else ErrorBox(SMsgCannotEdit);
end;

procedure TRScriptEditor.AddCommandVar;
var
  Token, VarName, VarValue: string;
begin
  // CopyVariables;
  Token := tkSet;
  VarName := EmptyStr;
  VarValue := EmptyStr;
  if EditVarCommand(Self, Token, VarName, VarValue) then
  begin
    InsertNewLine;
    fEditor.SelText := Token + #32 + TextVariable(VarName, VarValue);
  end;
end;

procedure TRScriptEditor.EditCommandVar(var ChPos: Integer);
var
  Token, VarFull, VarName, VarValue: string;
begin
  // CopyVariables;
  _ClearPositions;
  Token := ReadToken(False, False, False, ChPos);
  VarFull := ReadLine(False, ChPos);
  if Pos(VarsChar, VarFull) > 0 then
  begin
    StrToVariable(VarFull, VarName, VarValue);
    if EditVarCommand(Self, Token, VarName, VarValue) then
    begin
      fEditor.SelStart := FirstSymbol;
      fEditor.SelLength := LastSymbol - FirstSymbol + 1;
      fEditor.SelText := Token + #32 + TextVariable(VarName, VarValue);
    end;
  end
  else ErrorBox(Format(EParseInvalidVariable, [VarFull]));
end;

procedure TRScriptEditor.AddVarList;
var
  Offset, VarName, VarValue: string;
begin
  // CopyVariables;
  VarName := EmptyStr;
  VarValue := EmptyStr;
  if EditVarCommand(Self, EmptyStr, VarName, VarValue) then
  begin
    Offset := EmptyStr;
    if (Pos(tkVars, fEditor.Text) + Length(tkVars) - 1) >= fEditor.SelStart then
    begin
      fEditor.SelStart := Pos(tkVars, fEditor.Text) + Length(tkVars) - 1;
      Offset := #32#32;
    end;
    InsertNewLine;
    fEditor.SelText := Offset + TextVariable(VarName, VarValue);
  end;
end;

procedure TRScriptEditor.EditVarList(var ChPos: Integer);
var
  Token, VarName, VarValue: string;
begin
  // CopyVariables;
  _ClearPositions;
  Token := ReadLine(False, ChPos);
  if Pos(VarsChar, Token) > 0 then
  begin
    StrToVariable(Token, VarName, VarValue);
    if EditVarCommand(Self, EmptyStr, VarName, VarValue) then
    begin
      fEditor.SelStart := FirstSymbol;
      fEditor.SelLength := LastSymbol - FirstSymbol + 1;
      fEditor.SelText := TextVariable(VarName, VarValue);
    end;
  end
  else ErrorBox(Format(EParseInvalidVariable, [Token]));
end;


procedure TRScriptEditor.AddCommandCyc;
var
  CmdClass: TClass;
  ListText: string;
begin
  // CopyVariables;
  CmdClass := LRCommandsCyc[Low(LRCommandsCyc)];
  ListText := EmptyStr;
  if EditCycCommand(Self, CmdClass, ListText) then
  begin
    InsertNewLine;
    fEditor.SelText := CRCmdCycle(CmdClass).GetCommandName
      + #32 + ScriptQuotedStr(ListText) + #32 + tkDo;
  end;
end;

procedure TRScriptEditor.EditCommandCyc(var ChPos: Integer);
var
  i: TRCmdType;
  CmdClass: TClass;
  Token, ListText: string;
begin
  // CopyVariables;
  _ClearPositions;
  CmdClass := nil;
  Token := ReadToken(False, False, False, ChPos);
  for i := Low(LRCommandsCyc) to High(LRCommandsCyc) do
    if CompareToken(Token, LRCommandsCyc[i].GetCommandName) then
    begin
      CmdClass := LRCommandsCyc[i];
      Break;
    end;
  if Assigned(CmdClass) then
  begin
    ListText := ReadToken(False, False, False, ChPos);
    if EditCycCommand(Self, CmdClass, ListText) then
    begin
      fEditor.SelStart := FirstSymbol;
      fEditor.SelLength := LastSymbol - FirstSymbol + 1;
      fEditor.SelText := CRCmdCycle(CmdClass).GetCommandName
        + #32 + ScriptQuotedStr(ListText) + #32;
    end;
  end
  else ErrorBox(SMsgCannotEdit);
end;

procedure TRScriptEditor.Edit;
var
  StartPos: Integer;
begin
  fEditMode := True;
  try
    StartPos := fEditor.SelStart + 1;
    case _PeakToken(StartPos) of
      ttUnknown, ttVarsId, ttNotEdited: ErrorBox(SMsgCannotEdit);
      ttExeCommand: EditCommandExe(StartPos);
      ttVarCommand: EditCommandVar(StartPos);
      ttCondition: EditCondition(StartPos);
      ttVarList: EditVarList(StartPos);
      ttCycle: EditCommandCyc(StartPos);
    end;
  finally
    fEditMode := False;
  end;
end;

{ TRScriptItem }

constructor TRScriptItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fId := 0;
  fRunState := rsWarning;
  fChkState := rsApply;
  fScrList := nil;
end;

destructor TRScriptItem.Destroy;
begin
  ScrList := nil;
  inherited Destroy;
end;

procedure TRScriptItem.SetScrList(const Value: TRScripts);
begin
  if fScrList <> Value then fScrList := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TRScriptItem.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(fScrList) and (AComponent = fScrList) then
    ScrList := nil;
end;

function TRScriptItem.GetScriptTitle: string;
var
  TaskName: string;
begin
  Result := inherited GetScriptName;
  if Assigned(fScrList) then
  begin
    TaskName := fScrList.GetTaskName;
    if TaskName <> EmptyStr then
      Result := TaskName;
  end;
end;

procedure TRScriptItem.InitVariables;
begin
  inherited InitVariables;
  AddVariable(fVarList, varScriptId, IntToStr(fId));
end;

procedure TRScriptItem.SetScriptId(const Value: Integer);
begin
  if fId <> Value then
  begin
    fId := Value;
    AddVariable(fVarList, varScriptId, IntToStr(fId));
  end;
end;

function TRScriptItem.FindExternalScript(const Alias: string): TRScriptCustom;
begin
  Result := inherited FindExternalScript(Alias);
  if (Result = nil) and Assigned(fScrList) then
    Result := fScrList.LoadExternalScript(Self, Alias);
end;

function TRScriptItem.FindExternalProc(const Name: string): TRCmdProc;
var
  ExtScript: TRScriptCustom;
begin
  Result := inherited FindExternalProc(Name);
  if (Result = nil) and Assigned(fScrList) then
  begin
    ExtScript := FindExternalScript(GetAliasProcName(Name));
    if ExtScript <> nil then
    begin
      Result := ExtScript.FindProc(Name);
      if Result = nil then
        Result := ExtScript.FindProc(GetShortProcName(Name));
    end;
  end;
end;

procedure TRScriptItem.DoInitVisualComponents;
begin
  if not Assigned(fScrList) then
    inherited DoInitVisualComponents;
end;

procedure TRScriptItem.DoDoneVisualComponents;
begin
  if not Assigned(fScrList) then
    inherited DoDoneVisualComponents;
end;

procedure TRScriptItem.DoCompileStart;
begin
  if Assigned(fScrList) then
    fScrList.DoCompileStart(Self)
  else inherited DoCompileStart;
end;

procedure TRScriptItem.DoCompileEnd;
begin
  if Assigned(fScrList) then
    fScrList.DoCompileEnd(Self)
  else inherited DoCompileEnd;
end;

procedure TRScriptItem.DoExecuteStart;
begin
  if Assigned(fScrList) then
    fScrList.DoExecuteStart(Self, CanBreak)
  else inherited DoExecuteStart;
end;

procedure TRScriptItem.DoExecuteEnd;
begin
  if Assigned(fScrList) then
    fScrList.DoExecuteEnd(Self)
  else inherited DoExecuteEnd;
end;

procedure TRScriptItem.DoShowInfo(Sender: TObject;
  const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
  const MsgState: TRMsgState; const MsgText: string);
begin
  if Assigned(fScrList) then
    fScrList.DoShowInfo(Self, TimeStamp, MsgClass, MsgState, MsgText)
  else inherited DoShowInfo(Self, TimeStamp, MsgClass, MsgState, MsgText);
end;

procedure TRScriptItem.DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState);
begin
  if Assigned(fScrList) then
    fScrList.DoShowState(Self, Global, State)
  else inherited DoShowState(Self, Global, State);
end;

procedure TRScriptItem.DoShowProgress(Sender: TObject;
  const MsgClass: TRMsgClass; const CurrPos, MaxPos: Integer);
begin
  if Assigned(fScrList) then
    fScrList.DoShowProgress(Self, MsgClass, CurrPos, MaxPos)
  else inherited DoShowProgress(Self, MsgClass, CurrPos, MaxPos);
end;

procedure TRScriptItem.DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer);
begin
  if Assigned(fScrList) then
    fScrList.DoFileProgress(Self, CurrPos, MaxPos)
  else inherited DoFileProgress(Self, CurrPos, MaxPos);
end;

procedure TRScriptItem.DoUpdateControls(Sender: TObject;
  const BreakEnabled, CloseEnabled: Boolean);
begin
  if Assigned(fScrList) then
    fScrList.DoUpdateControls(Self, BreakEnabled, CloseEnabled)
  else inherited DoUpdateControls(Self, BreakEnabled, CloseEnabled);
end;

procedure TRScriptItem.DoCheckBreak(Sender: TObject; var IsBreak: Boolean);
begin
  if Assigned(fScrList) then
    fScrList.DoCheckBreak(Self, IsBreak);
  inherited DoCheckBreak(Self, IsBreak);
end;

procedure TRScriptItem.DoCalcNetUsage(Sender: TObject; var NetUsage: Byte);
begin
  inherited DoCalcNetUsage(Self, NetUsage);
  if Assigned(fScrList) then
    fScrList.DoCalcNetUsage(Self, NetUsage);
end;

procedure TRScriptItem.DoExecCmdOnEP(Sender: TObject; const CmdType: TRCmdExtType;
  const CmdText: string; const ScriptName: string; const AliasName: string; const RunProcName: string;
  const ShowInfo: TRShowInfoNotifyEvent; const ShowProgress: TRShowProgressNotifyEvent;
  const FileProgress: TRFileProgressNotifyEvent; var State: TRScriptState);
begin
  inherited DoExecCmdOnEP(Self, CmdType, CmdText, ScriptName, AliasName, RunProcName,
    ShowInfo, ShowProgress, FileProgress, State);
  if Assigned(fScrList) then
    fScrList.DoExecCmdOnEP(Self, CmdType, CmdText, ScriptName, AliasName, RunProcName,
      ShowInfo, ShowProgress, FileProgress, State);
end;

{ TRScripts }

constructor TRScripts.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UserData := nil;
  fScrList := TObjectList.Create;
  fKeyList := TStringList.Create;
  fVarList := TStringList.Create;
  fTaskName := EmptyStr;
  fVsInit := False;
  fBufferSize := 64;
  fCycleMax := 10;
  fDateMode := dmFullAuto;
  fDateFixed := 0;
  fNetUsage := 100;
  fTagChar := '%';
  fTryMax := 3;
  fTryDelay := 3;
  fBreak := False;
  fGuiEnabled := True;
  fExtProcessor := False;
  // v 6.3.0, 10-04-2012
  fExtCmdWaitTime := DefMaxWaitTime;
end;

destructor TRScripts.Destroy;
begin
  if fVsInit then
    DoDoneVisualComponents(Self);
  fScrList.Clear;
  fKeyList.Clear;
  fVarList.Clear;
  fScrList.Free;
  fKeyList.Free;
  fVarList.Free;
  UserData := nil;
  inherited Destroy;
end;

procedure TRScripts.SetLogger(const Value: TRScriptLogger);
begin
  if fLogger <> Value then fLogger := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

procedure TRScripts.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(fLogger) and (AComponent = fLogger) then
    Logger := nil;
end;

function TRScripts.GetScript(Index: Integer): TRScriptItem;
begin
  Result := nil;
  if Index in [0..fScrList.Count - 1] then
    Result := TRScriptItem(fScrList.Items[Index]);
end;

function TRScripts.Add(Script: TRScriptItem): Integer;
begin
  Result := fScrList.Add(Script);
  Script.ScrList := Self;
  Script.Logger := fLogger;
  Script.BufferSizeKb := fBufferSize;
  Script.CycleMax := fCycleMax;
  Script.DateMode := fDateMode;
  Script.DateFixed := fDateFixed;
  Script.NetUsage := fNetUsage;
  Script.TryMax := fTryMax;
  Script.TryDelay := fTryDelay;
  Script.TagsChar := fTagChar;
  Script.Keywords := fKeyList;
  Script.GuiEnabled := fGuiEnabled;
  Script.ExternalProcessor := fExtProcessor;
  Script.ExtCmdWaitTime := fExtCmdWaitTime;
  Script.PutVariables(True, fVarList);
end;

function TRScripts.AddScript(const AScript: string; const ACanBreak: Boolean;
  const ARunState: TRScrRunState; const AChkState: TRScrResState;
  const AId: Integer = 0; const ACaption: string = ''; const AAlias: string = '';
  const AAttrs: string = ''; const AVariables: string = ''): TRScriptItem;
begin
  Result := TRScriptItem.Create(Self);
  Result.ScriptId := AId;
  Result.Caption := ACaption;
  Result.Alias := AAlias;
  Result.CanBreak := ACanBreak;
  Result.Attributes := AAttrs;
  Result.RunState := ARunState;
  Result.ChkState := AChkState;
  Result.Script.Text := AScript;
  Add(Result);
  if AVariables <> EmptyStr then
    Result.PutVariables(False, AVariables);
end;

function TRScripts.AddScript(const AScript: string; const ACanBreak: Boolean;
  const ARunState: TRScrRunState; const AChkState: TRScrResState;
  const AId: Integer = 0; const ACaption: string = ''; const AAlias: string = '';
  const AAttrs: string = ''; const AVariables: TStrings = nil): TRScriptItem;
begin
  Result := TRScriptItem.Create(Self);
  Result.ScriptId := AId;
  Result.Caption := ACaption;
  Result.Alias := AAlias;
  Result.CanBreak := ACanBreak;
  Result.Attributes := AAttrs;
  Result.RunState := ARunState;
  Result.ChkState := AChkState;
  Result.Script.Text := AScript;
  Add(Result);
  if AVariables <> nil then
    Result.PutVariables(False, AVariables);
end;

procedure TRScripts.Clear;
begin
  while fScrList.Count > 0 do
    fScrList.Delete(0);
end;

procedure TRScripts.Delete(const Index: Integer);
begin
  if Index in [0..fScrList.Count - 1] then
    fScrList.Delete(Index);
end;

procedure TRScripts.SetBufferSize(const Value: Word);
var
  i, iCount: Integer;
begin
  if Value <> fBufferSize then
  begin
    fBufferSize := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).BufferSizeKb := Value;
  end;
end;

procedure TRScripts.SetCycleMax(const Value: Integer);
var
  i, iCount: Integer;
begin
  if Value <> fCycleMax then
  begin
    fCycleMax := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).CycleMax := Value;
  end;
end;

procedure TRScripts.SetExtCmdWaitTime(const Value: Cardinal);
var
  i, iCount: Integer;
begin
  if Value <> fExtCmdWaitTime then
  begin
    fExtCmdWaitTime := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).ExtCmdWaitTime := Value;
  end;
end;

procedure TRScripts.SetDateMode(const Value: TRScriptDate);
var
  i, iCount: Integer;
begin
  if Value <> fDateMode then
  begin
    fDateMode := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).DateMode := Value;
  end;
end;

procedure TRScripts.SetDateFixed(const Value: TDateTime);
var
  i, iCount: Integer;
begin
  if Value <> fDateFixed then
  begin
    fDateFixed := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).DateFixed := Value;
  end;
end;

procedure TRScripts.SetNetUsage(const Value: Byte);
var
  i, iCount: Integer;
begin
  if Value <> fNetUsage then
  begin
    fNetUsage := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).NetUsage := Value;
  end;
end;

procedure TRScripts.SetTagChar(const Value: Char);
var
  i, iCount: Integer;
begin
  if Value <> fTagChar then
  begin
    fTagChar := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).TagsChar := Value;
  end;
end;

procedure TRScripts.SetTryDelay(const Value: Word);
var
  i, iCount: Integer;
begin
  if Value <> fTryDelay then
  begin
    fTryDelay := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).TryDelay := Value;
  end;
end;

procedure TRScripts.SetTryMax(const Value: Byte);
var
  i, iCount: Integer;
begin
  if Value <> fTryMax then
  begin
    fTryMax := Value;
    iCount := fScrList.Count - 1;
    for i := 0 to iCount do
      TRScriptItem(fScrList.Items[i]).TryMax := Value;
  end;
end;

procedure TRScripts.SetKeywords(const Value: TStrings);
var
  i, iCount: Integer;
begin
  fKeyList.Assign(Value);
  iCount := fScrList.Count - 1;
  for i := 0 to iCount do
    TRScriptItem(fScrList.Items[i]).Keywords := Value;
end;

procedure TRScripts.SetVariables(const Value: TStrings);
var
  i, iCount: Integer;
begin
  fVarList.Assign(Value);
  iCount := fScrList.Count - 1;
  for i := 0 to iCount do
    TRScriptItem(fScrList.Items[i]).PutVariables(True, Value);
end;

procedure TRScripts.InitVariables;
var
  i, iCount: Integer;
begin
  iCount := fScrList.Count - 1;
  for i := 0 to iCount do
    TRScriptItem(fScrList.Items[i]).PutVariables(True, fVarList);
end;

procedure TRScripts.PutVariable(const Name, Value: string);
var
  i, iCount: Integer;
begin
  AddVariable(fVarList, Name, Value);
  iCount := fScrList.Count - 1;
  for i := 0 to iCount do
    TRScriptItem(fScrList.Items[i]).PutVariable(False, Name, Value);
end;

procedure TRScripts.PutVariable(const Variable: string);
var
  i, iCount: Integer;
begin
  AddVariable(fVarList, Variable);
  iCount := fScrList.Count - 1;
  for i := 0 to iCount do
    TRScriptItem(fScrList.Items[i]).PutVariable(False, Variable);
end;

procedure TRScripts.PutVariables(VarList: TStrings);
var
  i, iCount: Integer;
begin
  iCount := VarList.Count - 1;
  for i := 0 to iCount do
    AddVariable(fVarList, VarList.Names[i], VarList.ValueFromIndex[i]);

  iCount := fScrList.Count - 1;
  for i := 0 to iCount do
    TRScriptItem(fScrList.Items[i]).PutVariables(False, VarList);
end;

procedure TRScripts.PutVariables(VarList: string);
var
  BufList: TStrings;
begin
  BufList := TStringList.Create;
  try
    BufList.Text := VarList;
    PutVariables(BufList);
  finally
    BufList.Free;
  end;
end;

procedure TRScripts.DoShowInfo(Sender: TObject;
  const TimeStamp: TDateTime; const MsgClass: TRMsgClass;
  const MsgState: TRMsgState; const MsgText: string);
begin
  if Assigned(fShowInfo) then
    fShowInfo(Sender, TimeStamp, MsgClass, MsgState, MsgText);
  if Assigned(fLogger) then
    fLogger.DoShowInfo(Sender, TimeStamp, MsgClass, MsgState, MsgText);
end;

procedure TRScripts.DoShowState(Sender: TObject; const Global: Boolean; const State: TRScriptState);
begin
  if Assigned(fShowState) then
    fShowState(Sender, Global, State);
  if Assigned(fLogger) then
    fLogger.DoShowState(Sender, Global, State);
end;

procedure TRScripts.DoShowProgress(Sender: TObject;
  const MsgClass: TRMsgClass; const CurrPos, MaxPos: Integer);
begin
  if Assigned(fShowProgress) then
    fShowProgress(Sender, MsgClass, CurrPos, MaxPos);
  if Assigned(fLogger) then
    fLogger.DoShowProgress(Sender, MsgClass, CurrPos, MaxPos);
end;

procedure TRScripts.DoFileProgress(Sender: TObject; const CurrPos, MaxPos: Integer);
begin
  if Assigned(fFileProgress) then
    fFileProgress(Sender, CurrPos, MaxPos);
  if Assigned(fLogger) then
    fLogger.DoFileProgress(Sender, CurrPos, MaxPos);
end;

procedure TRScripts.DoCheckBreak(Sender: TObject; var IsBreak: Boolean);
begin
  if not IsBreak and not fBreak then
  begin
    if Assigned(fCheckBreak) then
      fCheckBreak(Sender, fBreak);
    if not fBreak and Assigned(fLogger) then
      fLogger.DoCheckBreak(Sender, fBreak);
  end;
  IsBreak := IsBreak or fBreak;
end;

procedure TRScripts.DoUpdateControls(Sender: TObject;
  const BreakEnabled, CloseEnabled: Boolean);
begin
  if Assigned(fUpdateControls) then
    fUpdateControls(Sender, BreakEnabled, CloseEnabled);
  if Assigned(fLogger) then
    fLogger.DoUpdateControls(Sender, BreakEnabled, CloseEnabled);
end;

procedure TRScripts.DoCalcNetUsage(Sender: TObject; var NetUsage: Byte);
begin
  NetUsage := fNetUsage;
  if Assigned(fGetNetUsage) then
    fGetNetUsage(Self, NetUsage);
end;

function TRScripts.CalcNetUsage: Byte;
begin
  Result := fNetUsage;
  if Assigned(fGetNetUsage) then
    fGetNetUsage(Self, Result);
end;

procedure TRScripts.DoExecCmdOnEP(Sender: TObject; const CmdType: TRCmdExtType;
  const CmdText: string; const ScriptName: string; const AliasName: string; const RunProcName: string;
  const ShowInfo: TRShowInfoNotifyEvent; const ShowProgress: TRShowProgressNotifyEvent;
  const FileProgress: TRFileProgressNotifyEvent; var State: TRScriptState);
begin
  if Assigned(fExecCmdOnEP) then
    fExecCmdOnEP(Self, CmdType, CmdText, ScriptName, AliasName, RunProcName, ShowInfo, ShowProgress, FileProgress, State);
end;

procedure TRScripts.DoInitVisualComponents(Sender: TObject);
begin
  fVsInit := True;
  if Assigned(fInitVisual) then
    fInitVisual(Self);
  if Assigned(fLogger) then
    fLogger.DoInitVisualComponents(Self);
end;

procedure TRScripts.DoDoneVisualComponents(Sender: TObject);
begin
  if Assigned(fDoneVisual) then
    fDoneVisual(Self);
  if Assigned(fLogger) then
    fLogger.DoDoneVisualComponents(Self);
  fVsInit := False;
end;

procedure TRScripts.DoCompileStart(Sender: TObject);
begin
  if Assigned(fCompileStart) then
    fCompileStart(Sender);
  if Assigned(fLogger) then
    fLogger.DoCompileStart(Sender);
end;

procedure TRScripts.DoCompileEnd(Sender: TObject);
begin
  if Assigned(fCompileEnd) then
    fCompileEnd(Sender);
  if Assigned(fLogger) then
    fLogger.DoCompileEnd(Sender);
end;

procedure TRScripts.DoExecuteStart(Sender: TObject; const BreakEnabled: Boolean);
begin
  fBreak := False;
  if Assigned(fExecuteStart) then
    fExecuteStart(Sender);
  if Assigned(fLogger) then
    fLogger.DoExecuteStart(Sender, BreakEnabled);
end;

procedure TRScripts.DoExecuteEnd(Sender: TObject);
begin
  if Assigned(fExecuteEnd) then
    fExecuteEnd(Sender);
  if Assigned(fLogger) then
    fLogger.DoExecuteEnd(Sender);
end;

procedure TRScripts.DoTotalExecuteStart(Sender: TObject);
begin
  if Assigned(fTotalExecuteStart) then
    fTotalExecuteStart(Sender);
end;

procedure TRScripts.DoTotalExecuteEnd(Sender: TObject);
begin
  if Assigned(fTotalExecuteEnd) then
    fTotalExecuteEnd(Sender);
end;

function TRScripts.GetTaskName: string;
begin
  Result := fTaskName;
  if Assigned(fGetTaskName) then
    fGetTaskName(Self, Result);
end;

function TRScripts.LoadExternalScript(Script: TRScriptCustom; const Alias: string): TRScriptItem;
var
  Loaded: Boolean;
begin
  Result := FindScript(Alias);
  if (Result = nil) and Assigned(fLoadExtScript) then
  begin
    Loaded := False;
    try
      DoShowInfo(Script, Now, mcSystem, msInfo,
        Format(SExecuteLoadExtScript, [Alias]));
      fLoadExtScript(Self, Alias, Loaded);
    except
      on E: Exception do
        DoShowInfo(Script, Now, mcSystem, msError,
          Format(EScriptLoadExtScript, [Alias, E.Message]));
    end;
    if Loaded then Result := FindScript(Alias);
  end;
  if Result = nil
  then DoShowInfo(Script, Now, mcSystem, msError, Format(EScriptNotFound, [Alias]))
  else if not Result.Compiled then Result.Compile;
end;

function TRScripts.FindScript(const Alias: string): TRScriptItem;
var
  i, iCount: Integer;
begin
  Result := nil;
  iCount := fScrList.Count - 1;
  for i := 0 to iCount do
    if SameText(TRScriptItem(fScrList.Items[i]).Alias, Alias) then
    begin
      Result := TRScriptItem(fScrList.Items[i]);
      Break;
    end;
end;

function TRScripts.Compile: Boolean;
var
  i, iCount: Integer;
  fCallVisual: Boolean;
begin
  Result := False;
  // fPrgsOffset := 0;
  fCallVisual := not fVsInit;
  if fCallVisual then
    DoInitVisualComponents(Self);
  try
    if fScrList.Count > 0 then
    begin
      iCount := fScrList.Count - 1;
      for i := 0 to iCount do
      begin
        Result := TRScriptItem(fScrList.Items[i]).Compile;
        if not Result then Break;
      end;
    end
    else DoShowInfo(Self, Now, mcSystem, msError, EScriptListEmpty);
  finally
    if fCallVisual then
      DoDoneVisualComponents(Self);
  end;
end;

procedure TRScripts.BreakScript;
begin
  fBreak := True;
end;

function TRScripts.Execute(const CallCompile: Boolean): TRScriptState;
var
  i, iCount: Integer;
  ActScript: TRScriptItem;
  RunState: TRScrRunState;
  ChkState: TRScrResState;
  ScrResult: TRScriptState;
begin
  Result := stOk;
  // fPrgsOffset := 0;
  DoInitVisualComponents(Self);
  try
    DoTotalExecuteStart(Self);
    try
      fBreak := False;
      if fScrList.Count > 0 then
      begin
        iCount := fScrList.Count - 1;
        for i := 0 to iCount do
        begin
          if fBreak then
          begin
            Result := stErrorStop;
            DoShowState(Self, True, Result);
            Break;
          end;
          RunState := TRScriptItem(fScrList.Items[i]).RunState;
          ChkState := TRScriptItem(fScrList.Items[i]).ChkState;
          if ((RunState = rsOk) and (Result = stOk))
          or ((RunState = rsWarning) and (Result in [stOk, stWarning]))
          or (RunState = rsAny) then
          begin
            ActScript := TRScriptItem(fScrList.Items[i]);
            ScrResult := ActScript.Execute(CallCompile);
            if ScrResult > stError then ScrResult := stError;
            if (ChkState = rsApply) and (Result < ScrResult) then
              Result := ScrResult;
            DoShowState(Self, True, Result);
          end;
        end;
      end
      else begin
        Result := stError;
        DoShowState(Self, True, Result);
        DoShowInfo(Self, Now, mcSystem, msError, EScriptListEmpty);
      end;
    finally
      DoTotalExecuteEnd(Self);
    end;
  finally
    DoDoneVisualComponents(Self);
  end;
end;

end.
