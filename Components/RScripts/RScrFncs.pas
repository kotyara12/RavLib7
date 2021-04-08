unit RScrFncs;

interface

uses
  IniFiles, Registry, RScripts, RMsgTypes, RScrCnst;

type
  TRFncStateIsOk = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncStateIsGood = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncStateIsWarning = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncStateIsError = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncVarExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncVarIs = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncKeyExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncWinExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncTaskExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncQuery = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncDiskFree = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncDirExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncFileExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncMaskExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncIsAdmin = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncBdeEnabled = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncBdeCheckAlias = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncPrinterExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRCmdForEach = class (TRCmdCycle)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdForFile = class (TRCmdCycle)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdForDirs = class (TRCmdCycle)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdIfThenElse = class (TRCmdCondition)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetCommandName: string; override;
    class function GetBlockThen: string; override;
    class function GetBlockElse: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdWhile = class (TRCmdCondition)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetCommandName: string; override;
    class function GetBlockThen: string; override;
    function GetCommandNote: string; override;
  end;

  TRCmdRepeat = class (TRCmdCondition)
  protected
    procedure ExecuteCommand(var OpState: TRScriptState); override;
  public
    class function GetCommandName: string; override;
    class function GetBlockThen: string; override;
    function GetCommandNote: string; override;
  end;

  TRFncObjExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncObjList = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncListIsNotEmpty = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncListIsBof = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncListIsEof = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncIniCustom = class (TRFncExecuted)
  protected
    function GetSubParam(const Index: Integer): string;
    function GetResult: Boolean; override;
    function GetIniResult(Ini: TMemIniFile): Boolean; virtual; abstract;
  public
    class function GetParamsLimit: Integer; override;
  end;

  TRFncIniSecExists = class (TRFncIniCustom)
  protected
    function GetIniResult(Ini: TMemIniFile): Boolean; override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRFncIniKeyExists = class (TRFncIniCustom)
  protected
    function GetIniResult(Ini: TMemIniFile): Boolean; override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRFncIniValCompare = class (TRFncIniCustom)
  protected
    function GetIniResult(Ini: TMemIniFile): Boolean; override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRFncRegCustom = class (TRFncExecuted)
  protected
    function GetSubParam(const Index: Integer): string;
    function GetResult: Boolean; override;
    function GetRegResult(Reg: TRegIniFile): Boolean; virtual; abstract;
  public
    class function GetParamsLimit: Integer; override;
  end;

  TRFncRegValueExists = class (TRFncRegCustom)
  protected
    function GetRegResult(Reg: TRegIniFile): Boolean; override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRFncRegKeyExists = class (TRFncRegCustom)
  protected
    function GetRegResult(Reg: TRegIniFile): Boolean; override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRFncRegValCompare = class (TRFncRegCustom)
  protected
    function GetRegResult(Reg: TRegIniFile): Boolean; override;
  public
    class function GetCommandName: string; override;
    function GetCommandNote: string; override;
  end;

  TRFncSelectFile = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncSelectDir = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncServiceExists = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncServiceStarted = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncServiceStopped = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncTextFind = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncTimeAfter = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

  TRFncTimeBefore = class (TRFncExecuted)
  protected
    function GetResult: Boolean; override;
  public
    class function GetCommandName: string; override;
    class function GetParamsLimit: Integer; override;
    function GetCommandNote: string; override;
  end;

const
  LRFunctions : array [TRFncType] of CRFncExecuted =
    (TRFncStateIsOk, TRFncStateIsGood, TRFncStateIsWarning, TRFncStateIsError,
     TRFncVarExists, TRFncVarIs, TRFncKeyExists, TRFncWinExists, TrFncTaskExists, TRFncQuery,
     TRFncDiskFree, TRFncFileExists, TRFncDirExists, TRFncMaskExists,
     TRFncTimeAfter, TRFncTimeAfter, TRFncIsAdmin,
     TRFncBdeEnabled, TRFncBdeCheckAlias, TRFncPrinterExists,
     TRFncIniSecExists, TRFncIniKeyExists, TRFncIniValCompare,
     TRFncRegValueExists, TRFncRegKeyExists, TRFncRegValCompare,
     TRFncObjExists, TRFncObjList, TRFncListIsNotEmpty, TRFncListIsBof, TRFncListIsEof, TRFncTextFind,
     TRFncSelectFile, TRFncSelectDir, TRFncServiceExists, TRFncServiceStarted, TRFncServiceStopped);

  LRCommandsCnd : array [cmIf..cmRepeat] of CRCmdCondition =
    (TRCmdIfThenElse, TRCmdWhile, TRCmdRepeat);

  LRCommandsCyc: array [cmForEach..cmForDirs] of CRCmdCycle =
    (TRCmdForEach, TRCmdForFile, TRCmdForDirs);

implementation

uses
  Windows, SysUtils, DateUtils, Classes, Dialogs, FileCtrl, RxStrUtils, RTimeDialog,
  RDialogs, RVarListEx, RFileProcs, RDbiUtils, RPrnUtils, RSysUtils, RSysProcs, RKrnlUtils;

const
  ParamsDividers        = [';'];

{ TRFncVarExists }

class function TRFncVarExists.GetCommandName: string;
begin
  Result := tkVarExists;
end;

function TRFncVarExists.GetCommandNote: string;
begin
  Result := SFncNoteVarExists;
end;

class function TRFncVarExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncVarExists.GetResult: Boolean;
begin
  Result := Script.VarListExec.IndexOfName(GetParameter(0)) > -1;
end;

{ TRFncVarIs }

class function TRFncVarIs.GetCommandName: string;
begin
  Result := tkVarIs;
end;

function TRFncVarIs.GetCommandNote: string;
begin
  Result := SFncNoteVarIs;
end;

class function TRFncVarIs.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncVarIs.GetResult: Boolean;
var
  ChkPos: Integer;
  ChkName: string;
  VarValue: string;
  ChkValue: string;
begin
  ChkName := GetParameter(0);
  VarValue := EmptyStr;
  ChkValue := EmptyStr;
  ChkPos := Pos(VarsChar, ChkName);
  if ChkPos > 0 then
  begin
    ChkValue := ChkName;
    ChkName := Trim(Copy(ChkName, 1, ChkPos - 1));
    ChkValue := Copy(ChkValue, ChkPos + 1, Length(ChkValue) - ChkPos);
  end;
  Result := AnsiSameText(GetVariableValue(Script.VarListExec, ChkName), ChkValue);
end;

{ TRFncKeyExists }

class function TRFncKeyExists.GetCommandName: string;
begin
  Result := tkKeyExists;
end;

function TRFncKeyExists.GetCommandNote: string;
begin
  Result := SFncNoteKeyExists;
end;

class function TRFncKeyExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncKeyExists.GetResult: Boolean;
begin
  Result := Script.Keywords.IndexOf(GetParameter(0)) > -1;
end;

{ TRFncWinExists }

class function TRFncWinExists.GetCommandName: string;
begin
  Result := tkWinExists;
end;

function TRFncWinExists.GetCommandNote: string;
begin
  Result := SFncNoteWinExists;
end;

class function TRFncWinExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncWinExists.GetResult: Boolean;
begin
  Result := FindWindow(nil, PAnsiChar(GetParameter(0))) > 0;
end;

{ TRFncTaskExists }

class function TRFncTaskExists.GetCommandName: string;
begin
  Result := tkTaskExists;
end;

function TRFncTaskExists.GetCommandNote: string;
begin
  Result := SFncNoteTaskExists;
end;

class function TRFncTaskExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncTaskExists.GetResult: Boolean;
begin
  Result := ProcessIsRunning(GetParameter(0), True);
end;

{ TRFncQuery }

class function TRFncQuery.GetCommandName: string;
begin
  Result := tkQuery;
end;

function TRFncQuery.GetCommandNote: string;
begin
  Result := SFncNoteQuery;
end;

class function TRFncQuery.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncQuery.GetResult: Boolean;
begin
  Result := ShowQueryBox(True, GetParameter(0), 60);
end;

{ TRFncDiskFree }

class function TRFncDiskFree.GetCommandName: string;
begin
  Result := tkDiskFree;
end;

function TRFncDiskFree.GetCommandNote: string;
begin
  Result := SFncNoteDiskFree;
end;

class function TRFncDiskFree.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncDiskFree.GetResult: Boolean;
begin
  Result := (DiskFree(0) / 1024 / 1024) <= StrToIntDef(GetParameter(0), 0);
end;

{ TRFncDirExists }

class function TRFncDirExists.GetCommandName: string;
begin
  Result := tkDirExists;
end;

function TRFncDirExists.GetCommandNote: string;
begin
  Result := SFncNoteDirExists;
end;

class function TRFncDirExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncDirExists.GetResult: Boolean;
begin
  Result := wDirectoryExists(GetParameter(0));
end;

{ TRFncFileExists }

class function TRFncFileExists.GetCommandName: string;
begin
  Result := tkFileExists;
end;

function TRFncFileExists.GetCommandNote: string;
begin
  Result := SFncNoteFileExists;
end;

class function TRFncFileExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncFileExists.GetResult: Boolean;
begin
  Result := wFileExists(GetParameter(0));
end;

{ TRFncMaskExists }

class function TRFncMaskExists.GetCommandName: string;
begin
  Result := tkMaskExists;
end;

function TRFncMaskExists.GetCommandNote: string;
begin
  Result := SFncNoteMaskExists;
end;

class function TRFncMaskExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncMaskExists.GetResult: Boolean;
begin
  Result := FilesExists(ExtractFilePath(GetParameter(0)), ExtractFileName(GetParameter(0)), faAnyFile, False,
    Script.DoShowInfo, Script.DoCheckBreak);
end;

{ TRFncIsAdmin }

class function TRFncIsAdmin.GetCommandName: string;
begin
  Result := tkIsAdmin;
end;

function TRFncIsAdmin.GetCommandNote: string;
begin
  Result := SFncNoteIsAdmin;
end;

class function TRFncIsAdmin.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRFncIsAdmin.GetResult: Boolean;
begin
  Result := IsAdmin;
end;

{ TRFncBdeEnabled }

class function TRFncBdeEnabled.GetCommandName: string;
begin
  Result := tkBdeEnabled;
end;

function TRFncBdeEnabled.GetCommandNote: string;
begin
  Result := SFncNoteBdeEnabled;
end;

class function TRFncBdeEnabled.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRFncBdeEnabled.GetResult: Boolean;
begin
  Result := RDbiEnabled;
end;

{ TRFncBdeCheckAlias }

class function TRFncBdeCheckAlias.GetCommandName: string;
begin
  Result := tkBdeCheckAlias;
end;

function TRFncBdeCheckAlias.GetCommandNote: string;
begin
  Result := SFncNoteBdeCheckAlias;
end;

class function TRFncBdeCheckAlias.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncBdeCheckAlias.GetResult: Boolean;
begin
  Result := RDbiAliasIsExists(GetParameter(0));
end;

{ TRFncPrinterExists }

class function TRFncPrinterExists.GetCommandName: string;
begin
  Result := tkPrinterExists;
end;

function TRFncPrinterExists.GetCommandNote: string;
begin
  Result := SFncNotePrinterExists;
end;

class function TRFncPrinterExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncPrinterExists.GetResult: Boolean;
begin
  Result := RPrinterExists(GetParameter(0));
end;

{ TRFncObjExists }

class function TRFncObjExists.GetCommandName: string;
begin
  Result := tkObjIsExists;
end;

function TRFncObjExists.GetCommandNote: string;
begin
  Result := SFncNoteObjIsExists;
end;

class function TRFncObjExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncObjExists.GetResult: Boolean;
begin
  Result := Assigned(Script.FindObject(otAuto, GetParameter(0), False));
end;

{ TRFncObjList }

class function TRFncObjList.GetCommandName: string;
begin
  Result := tkObjIsList;
end;

function TRFncObjList.GetCommandNote: string;
begin
  Result := SFncNoteObjIsList;
end;

class function TRFncObjList.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncObjList.GetResult: Boolean;
var
  Tmp: TObject;
begin
  Tmp := Script.FindObject(otAuto, GetParameter(0), False);
  Result := Assigned(Tmp) and (Tmp is TRListPosition);
end;

{ TRFncListIsNotEmpty }

class function TRFncListIsNotEmpty.GetCommandName: string;
begin
  Result := tkListIsNotEmpty;
end;

function TRFncListIsNotEmpty.GetCommandNote: string;
begin
  Result := SFncNoteListIsNotEmpty;
end;

class function TRFncListIsNotEmpty.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncListIsNotEmpty.GetResult: Boolean;
var
  Tmp: TObject;
begin
  Tmp := Script.FindObject(otAuto, GetParameter(0), False);
  Result := Assigned(Tmp) and (Tmp is TRListPosition)
    and not TRListPosition(Tmp).Empty;
end;

{ TRFncListIsBof }

class function TRFncListIsBof.GetCommandName: string;
begin
  Result := tkListIsBof;
end;

function TRFncListIsBof.GetCommandNote: string;
begin
  Result := SFncNoteListIsBof;
end;

class function TRFncListIsBof.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncListIsBof.GetResult: Boolean;
var
  Tmp: TObject;
begin
  Tmp := Script.FindObject(otAuto, GetParameter(0), False);
  Result := Assigned(Tmp) and (Tmp is TRListPosition)
    and TRListPosition(Tmp).Bof;
end;

{ TRFncListIsEof }

class function TRFncListIsEof.GetCommandName: string;
begin
  Result := tkListIsEof;
end;

function TRFncListIsEof.GetCommandNote: string;
begin
  Result := SFncNoteListIsEof;
end;

class function TRFncListIsEof.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncListIsEof.GetResult: Boolean;
var
  Tmp: TObject;
begin
  Tmp := Script.FindObject(otAuto, GetParameter(0), False);
  Result := Assigned(Tmp) and (Tmp is TRListPosition)
    and TRListPosition(Tmp).Eof;
end;

{ TRCmdIfThenElse }

class function TRCmdIfThenElse.GetCommandName: string;
begin
  Result := tkIf;
end;

function TRCmdIfThenElse.GetCommandNote: string;
begin
  Result := SCmdNoteIf;
end;

class function TRCmdIfThenElse.GetBlockThen: string;
begin
  Result := tkThen;
end;

class function TRCmdIfThenElse.GetBlockElse: string;
begin
  Result := tkElse;
end;

procedure TRCmdIfThenElse.ExecuteCommand(var OpState: TRScriptState);
begin
  if Script.Executed then
  begin
    if Condition.GetResult then
    begin
      Script.DoShowInfo(Script, Now, mcOperation, msTitle, SExecuteTrue);
      ThenList.Execute(OpState);
      Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteEndIf, [tkThen]));
    end
    else begin
      Script.DoShowInfo(Script, Now, mcOperation, msTitle, SExecuteElse);
      ElseList.Execute(OpState);
      Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteEndIf, [tkElse]));
    end
  end;
end;

{ TRCmdWhile }

class function TRCmdWhile.GetCommandName: string;
begin
  Result := tkWhile;
end;

class function TRCmdWhile.GetBlockThen: string;
begin
  Result := tkDo;
end;

function TRCmdWhile.GetCommandNote: string;
begin
  Result := SCmdNoteWhile;
end;

procedure TRCmdWhile.ExecuteCommand(var OpState: TRScriptState);
var
  CycleRun, TotalBreak: Boolean;
  CycleCnt: Integer;
begin
  Script.CmdBlock_Fix(ThenList); // 2011-08-04 fix bug - не выходил из цикла по команде Break
  try
    CycleCnt := 1;
    CycleRun := False;
    while Script.Executed and Condition.GetResult and (OpState in [stOk, stWarning, stError]) do
    begin
      // 2012-02-22 fix bug - сообщения о начале цикла писалось в лог, даже если цикл ни разу не выполнялся
      if not CycleRun then
      begin
        CycleRun := True;
        Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleBegin, [GetCommandName]));
      end;
      if Script.CycleMax > 0 then
      begin
        if CycleCnt > Script.CycleMax then
        begin
          Script.DoShowInfo(Script, Now, mcOperation, msBreak, Format(SExecuteCycleBreak, [CycleCnt]));
          Break;
        end;
        Inc(CycleCnt);
      end;
      ThenList.Execute(OpState);
      if ThenList.IsBreakBlock then
        Break;
      Script.DoCheckBreak(Script, TotalBreak);
      if TotalBreak then
        Break;
    end;
    // 2012-02-22 fix bug - сообщения о конце цикла писалось в лог, даже если цикл ни разу не выполнялся
    if CycleRun then
      Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteCycleEnd, [GetCommandName]));
  finally
    Script.CmdBlock_Release; // 2011-08-04 fix bug - не выходил из цикла по команде Break
  end;
end;

{ TRCmdRepeat }

class function TRCmdRepeat.GetCommandName: string;
begin
  Result := tkRepeat;
end;

class function TRCmdRepeat.GetBlockThen: string;
begin
  Result := tkUntil;
end;

function TRCmdRepeat.GetCommandNote: string;
begin
  Result := SCmdNoteRepeat;
end;

procedure TRCmdRepeat.ExecuteCommand(var OpState: TRScriptState);
var
  TotalBreak: Boolean;
  CycleCnt: Integer;
begin
  Script.CmdBlock_Fix(ThenList); // 2011-08-04 fix bug - не выходил из цикла по команде Break
  try
    Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleBegin, [GetCommandName]));
    CycleCnt := 1;
    repeat
      if Script.CycleMax > 0 then
      begin
        if CycleCnt > Script.CycleMax then
        begin
          Script.DoShowInfo(Script, Now, mcOperation, msBreak, Format(SExecuteCycleBreak, [CycleCnt]));
          Break;
        end;
        Inc(CycleCnt);
      end;
      ThenList.Execute(OpState);
      if ThenList.IsBreakBlock then
        Break;
      Script.DoCheckBreak(Script, TotalBreak);
      if TotalBreak then
        Break;
    until not Script.Executed or Condition.GetResult or
      (OpState in [stErrorStop, stErrorRestore, stErrorUndo]);
    Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteCycleEnd, [GetCommandName]));
  finally
    Script.CmdBlock_Release; // 2011-08-04 fix bug - не выходил из цикла по команде Break
  end;
end;

{ TRFncIniCustom }

class function TRFncIniCustom.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncIniCustom.GetSubParam(const Index: Integer): string;
begin
  if (Index > 0) and (Index <= WordCount(GetParameter(0), ParamsDividers))
  then Result := ExtractWord(Index, GetParameter(0), ParamsDividers)
  else Result := EmptyStr;
end;

function TRFncIniCustom.GetResult: Boolean;
var
  Ini: TMemIniFile;
begin
  try
    Ini := TMemIniFile.Create(GetSubParam(1));
    try
      Result := GetIniResult(Ini);
    finally
      Ini.Free;
    end;
  except
    Result := False;
  end;
end;

{ TRFncIniSecExists }

class function TRFncIniSecExists.GetCommandName: string;
begin
  Result := tkIniSecExists;
end;

function TRFncIniSecExists.GetCommandNote: string;
begin
  Result := SFncIniSecExists;
end;

function TRFncIniSecExists.GetIniResult(Ini: TMemIniFile): Boolean;
begin
  Result := Ini.SectionExists(GetSubParam(2));
end;

{ TRFncIniKeyExists }

class function TRFncIniKeyExists.GetCommandName: string;
begin
  Result := tkIniKeyExists;
end;

function TRFncIniKeyExists.GetCommandNote: string;
begin
  Result := SFncIniKeyExists;
end;

function TRFncIniKeyExists.GetIniResult(Ini: TMemIniFile): Boolean;
begin
  Result := Ini.ValueExists(GetSubParam(2), GetSubParam(3));
end;

{ TRFncIniValCompare }

class function TRFncIniValCompare.GetCommandName: string;
begin
  Result := tkIniValCompare;
end;

function TRFncIniValCompare.GetCommandNote: string;
begin
  Result := SFncIniValCompare;
end;

function TRFncIniValCompare.GetIniResult(Ini: TMemIniFile): Boolean;
begin
  Result := SameText(Ini.ReadString(GetSubParam(2), GetSubParam(3), EmptyStr), GetSubParam(4));
end;

{ TRFncRegCustom }

class function TRFncRegCustom.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncRegCustom.GetSubParam(const Index: Integer): string;
begin
  if (Index > 0) and (Index <= WordCount(GetParameter(0), ParamsDividers))
  then Result := ExtractWord(Index, GetParameter(0), ParamsDividers)
  else Result := EmptyStr;
end;

function TRFncRegCustom.GetResult: Boolean;
var
  Reg: TRegIniFile;
begin
  try
    Reg := TRegIniFile.Create(KEY_READ);
    try
      Reg.RootKey := DecodeRegistryRootKey(GetSubParam(1));
      Result := GetRegResult(Reg);
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;

{ TRFncRegValueExists }

class function TRFncRegValueExists.GetCommandName: string;
begin
  Result := tkRegValueExists;
end;

function TRFncRegValueExists.GetCommandNote: string;
begin
  Result := SFncRegValueExists;
end;

function TRFncRegValueExists.GetRegResult(Reg: TRegIniFile): Boolean;
begin
  Result := Reg.ValueExists(GetSubParam(2));
end;

{ TRFncRegKeyExists }

class function TRFncRegKeyExists.GetCommandName: string;
begin
  Result := tkRegKeyExists;
end;

function TRFncRegKeyExists.GetCommandNote: string;
begin
  Result := SFncRegKeyExists;
end;

function TRFncRegKeyExists.GetRegResult(Reg: TRegIniFile): Boolean;
begin
  Result := Reg.KeyExists(GetSubParam(2));
end;

{ TRFncRegValCompare }

class function TRFncRegValCompare.GetCommandName: string;
begin
  Result := tkRegValCompare;
end;

function TRFncRegValCompare.GetCommandNote: string;
begin
  Result := SFncRegValCompare;
end;

function TRFncRegValCompare.GetRegResult(Reg: TRegIniFile): Boolean;
begin
  Result := SameText(Reg.ReadString(GetSubParam(2), GetSubParam(3), EmptyStr), GetSubParam(4));
end;

{ TRFncSelectFile }

class function TRFncSelectFile.GetCommandName: string;
begin
  Result := tkSelectFile;
end;

class function TRFncSelectFile.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncSelectFile.GetCommandNote: string;
begin
  Result := SFncSelectFile;
end;

function TRFncSelectFile.GetResult: Boolean;
var
  VarName, FileName, PromptText: string;
begin
  Result := False;
  if Script.GuiEnabled then
  begin
    VarName := GetParameter(0);
    PromptText := GetVariableValue(Script.VarListExec, varDescription);
    if WordCount(VarName, ParamsDividers) = 2 then
    begin
      PromptText := Trim(ExtractWord(2, VarName, ParamsDividers));
      VarName := Trim(ExtractWord(1, VarName, ParamsDividers));
    end;
    FileName := UpdateVariables(GetVariableValue(Script.VarListExec, VarName));
    PromptText := UpdateVariables(PromptText);
    Result := PromptForFileName(FileName, EmptyStr, EmptyStr, PromptText, EmptyStr, False);
    if Result then Script.PutVariable(True, VarName, FileName);
  end;
end;

{ TRFncSelectDir }

class function TRFncSelectDir.GetCommandName: string;
begin
  Result := tkSelectDir;
end;

class function TRFncSelectDir.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncSelectDir.GetCommandNote: string;
begin
  Result := SFncSelectDir;
end;

function TRFncSelectDir.GetResult: Boolean;
var
  VarName, DirName, PromptText: string;
begin
  Result := False;
  if Script.GuiEnabled then
  begin
    VarName := GetParameter(0);
    PromptText := GetVariableValue(Script.VarListExec, varDescription);
    if WordCount(VarName, ParamsDividers) = 2 then
    begin
      PromptText := ExtractWord(2, VarName, ParamsDividers);
      VarName := ExtractWord(1, VarName, ParamsDividers);
    end;
    DirName := UpdateVariables(GetVariableValue(Script.VarListExec, VarName));
    PromptText := UpdateVariables(PromptText);
    Result := SelectDirectory(PromptText, EmptyStr, DirName);
    if Result then Script.PutVariable(True, VarName, DirName);
  end;
end;

{ TRFncServiceExists }

class function TRFncServiceExists.GetCommandName: string;
begin
  Result := tkServiceExists;
end;

class function TRFncServiceExists.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncServiceExists.GetCommandNote: string;
begin
  Result := SFncServiceExists;
end;

function TRFncServiceExists.GetResult: Boolean;
begin
  Result := ServiceExists(GetParameter(0));
end;

{ TRFncServiceStarted }

class function TRFncServiceStarted.GetCommandName: string;
begin
  Result := tkServiceStarted;
end;

class function TRFncServiceStarted.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncServiceStarted.GetCommandNote: string;
begin
  Result := SFncServiceStarted;
end;

function TRFncServiceStarted.GetResult: Boolean;
begin
  Result := ServiceStarted(GetParameter(0));
end;

{ TRFncServiceStopped }

class function TRFncServiceStopped.GetCommandName: string;
begin
  Result := tkServiceStopped;
end;

class function TRFncServiceStopped.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncServiceStopped.GetCommandNote: string;
begin
  Result := SFncServiceStopped;
end;

function TRFncServiceStopped.GetResult: Boolean;
begin
  Result := ServiceStopped(GetParameter(0));
end;

{ TRFncTextFind }

class function TRFncTextFind.GetCommandName: string;
begin
  Result := tkTextFind;
end;

function TRFncTextFind.GetCommandNote: string;
begin
  Result := SFncNoteTextFind;
end;

class function TRFncTextFind.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncTextFind.GetResult: Boolean;
var
  iPos: Integer;
  ObjName, FindText: string;
  ObjText: TObject;
begin
  Result := False;
  iPos := Pos(';', GetParameter(0));
  if iPos > 0 then
  begin
    ObjName := Copy(GetParameter(0), 1, iPos - 1);
    FindText := Copy(GetParameter(0), iPos + 1, Length(GetParameter(0)) - iPos);
    ObjText := Script.FindObject(otAuto, ObjName, False);
    if Assigned(ObjText) and (ObjText is TRListPosition)
    then Result := (Pos(AnsiUpperCase(FindText),
                        AnsiUpperCase(TRListPosition(ObjText).Text)) > 0)
    else raise Exception.CreateFmt(SExecuteInvalidObject, [ObjName]);
  end;
end;

{ TRFncStateIsOk }

class function TRFncStateIsOk.GetCommandName: string;
begin
  Result := tkStateIsOk;
end;

function TRFncStateIsOk.GetCommandNote: string;
begin
  Result := SFncNoteStateIsOk;
end;

class function TRFncStateIsOk.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRFncStateIsOk.GetResult: Boolean;
begin
  Result := Script.CurrentState = stOk;
end;

{ TRFncStateIsGood }

class function TRFncStateIsGood.GetCommandName: string;
begin
  Result := tkStateIsGood;
end;

function TRFncStateIsGood.GetCommandNote: string;
begin
  Result := SFncNoteStateIsGood;
end;

class function TRFncStateIsGood.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRFncStateIsGood.GetResult: Boolean;
begin
  Result := Script.CurrentState in [stOk, stWarning];
end;

{ TRFncStateIsWarning }

class function TRFncStateIsWarning.GetCommandName: string;
begin
  Result := tkStateIsWarning;
end;

function TRFncStateIsWarning.GetCommandNote: string;
begin
  Result := SFncNoteStateIsWarning;
end;

class function TRFncStateIsWarning.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRFncStateIsWarning.GetResult: Boolean;
begin
  Result := Script.CurrentState = stWarning;
end;

{ TRFncStateIsError }

class function TRFncStateIsError.GetCommandName: string;
begin
  Result := tkStateIsError;
end;

function TRFncStateIsError.GetCommandNote: string;
begin
  Result := SFncNoteStateIsError;
end;

class function TRFncStateIsError.GetParamsLimit: Integer;
begin
  Result := 0;
end;

function TRFncStateIsError.GetResult: Boolean;
begin
  Result := Script.CurrentState in [stError, stErrorStop, stErrorRestore, stErrorUndo];
end;

{ TRFncTimeAfter }

class function TRFncTimeAfter.GetCommandName: string;
begin
  Result := tkTimeAfter;
end;

function TRFncTimeAfter.GetCommandNote: string;
begin
  Result := SFncNoteTimeAfter;
end;

class function TRFncTimeAfter.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncTimeAfter.GetResult: Boolean;
begin
  Result := CompareDateTime(StrToDateTimeDef(GetParameter(0), Now), Now) < 1;
end;

{ TRFncTimeBefore }

class function TRFncTimeBefore.GetCommandName: string;
begin
  Result := tkTimeBefore;
end;

function TRFncTimeBefore.GetCommandNote: string;
begin
  Result := SFncNoteTimeBefore;
end;

class function TRFncTimeBefore.GetParamsLimit: Integer;
begin
  Result := 1;
end;

function TRFncTimeBefore.GetResult: Boolean;
begin
  Result := CompareDateTime(StrToDateTimeDef(GetParameter(0), Now), Now) > -1;
end;

{ TRCmdForEach }

class function TRCmdForEach.GetCommandName: string;
begin
  Result := tkForEach;
end;

function TRCmdForEach.GetCommandNote: string;
begin
  Result := SCmdNoteForEach;
end;

procedure TRCmdForEach.ExecuteCommand(var OpState: TRScriptState);
var
  i, iCount: Integer;
  fObject: TObject;
  fList: TRListPosition;
  sValue: string;
  bTotalBreak: Boolean;
begin
  Script.CmdBlock_Fix(CmdList); // 2011-08-04 fix bug - не выходил из цикла по команде Break
  try
    fObject := Script.FindObject(otAuto, GetCycleText, True);
    if Assigned(fObject) and (fObject is TRListPosition) then
    begin
      fList := TRListPosition(fObject);
      Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleForBegin, [GetCommandParams_Exec]));
      Script.DoShowInfo(Script, Now, mcOperation, msInfo, Format(SExecuteCycleCount, [GetCommandParams_Exec, fList.Count]));
      Script.IncTotalOpCount(CmdList.GetCommandCount * fList.Count);
      iCount := fList.Count - 1;
      for i := 0 to iCount do
      begin
        sValue := fList.Value(i);
        Script.PutVariable(True, GetCommandParams_Exec, sValue);
        Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleSet, [i + 1, GetCommandParams_Exec, sValue]));
        CmdList.Execute(OpState);
        Script.DoCheckBreak(Script, bTotalBreak);
        if bTotalBreak or CmdList.IsBreakBlock or not Script.Executed
        or (OpState in [stErrorStop, stErrorRestore, stErrorUndo]) then
          Break;
      end;
      Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteCycleEnd, [GetCommandText_Exec]));
    end
    else begin
      OpState := stError;
      Script.DoShowInfo(Script, Now, mcOperation, msError,
        Format(SExecuteInvalidObject, [GetCycleText]));
    end;
  finally
    Script.CmdBlock_Release; // 2011-08-04 fix bug - не выходил из цикла по команде Break
  end;
end;


{ TRCmdForFile }

class function TRCmdForFile.GetCommandName: string;
begin
  Result := tkForFile;
end;

function TRCmdForFile.GetCommandNote: string;
begin
  Result := SCmdNoteForFile;
end;

procedure TRCmdForFile.ExecuteCommand(var OpState: TRScriptState);
var
  i, iCount: Integer;
  fFileList: TRFileList;
  sMaskPath, sMaskName: string;
  sFindRes: TRTransaction;
  bSubDirs, bTotalBreak: Boolean;
begin
  Script.CmdBlock_Fix(CmdList); // 2011-08-04 fix bug - не выходил из цикла по команде Break
  try
    fFileList := TRFileList.Create;
    try
      sMaskPath := ExcludeTrailingPathDelimiter(ExtractFilePath(GetCommandParams_Exec));
      sMaskName := ExtractFileName(GetCommandParams_Exec);
      bSubDirs := AnsiPos(SRCmdRfoFlag[rfSrcSubDirs], GetVariableValue(Script.VarListExec, varAttributes)) > 0;
      if bSubDirs
      then Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleForFileS, [sMaskName, sMaskPath]))
      else Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleForFile, [sMaskName, sMaskPath]));
      Script.DoShowInfo(Script, Now, mcOperation, msInfo, Format(SExecuteFindFiles, [sMaskPath, sMaskName]));
      sFindRes := CreateFileList(sMaskPath, sMaskName, faAnyFile, fFileList, bSubDirs, True, ezOk, Script.DoShowInfo, Script.DoCheckBreak);
      if not (sFindRes.State in [msError, msBreak]) then
      begin
        Script.DoShowInfo(Script, Now, mcOperation, msOk, sFindRes.Result);
        Script.IncTotalOpCount(CmdList.GetCommandCount * fFileList.Count);
        iCount := fFileList.Count - 1;
        for i := 0 to iCount do
        begin
          Script.PutVariable(True, varForFile, fFileList.Items(i).sName);
          Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleSet, [i + 1, varForFile, fFileList.Items(i).sName]));
          CmdList.Execute(OpState);
          Script.DoCheckBreak(Script, bTotalBreak);
          if bTotalBreak or CmdList.IsBreakBlock or not Script.Executed
          or (OpState in [stErrorStop, stErrorRestore, stErrorUndo]) then
            Break;
        end;
        Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteCycleEnd, [GetCommandText_Exec]));
      end
      else Script.DoShowInfo(Script, Now, mcOperation, sFindRes.State, sFindRes.Result);
    finally
      fFileList.Free;
    end;
  finally
    Script.CmdBlock_Release; // 2011-08-04 fix bug - не выходил из цикла по команде Break
  end;
end;

{ TRCmdForDirs }

class function TRCmdForDirs.GetCommandName: string;
begin
  Result := tkForDirs;
end;

function TRCmdForDirs.GetCommandNote: string;
begin
  Result := SCmdNoteForDirs;
end;

procedure TRCmdForDirs.ExecuteCommand(var OpState: TRScriptState);
var
  i, iCount: Integer;
  fDirList: TRFileList;
  sFindRes: TRTransaction;
  bSubDirs, bTotalBreak: Boolean;
begin
  Script.CmdBlock_Fix(CmdList); // 2011-08-04 fix bug - не выходил из цикла по команде Break
  try
    bSubDirs := AnsiPos(SRCmdRfoFlag[rfSrcSubDirs], GetVariableValue(Script.VarListExec, varAttributes)) > 0;
    if bSubDirs
    then Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleForDirsS, [GetCommandParams_Exec]))
    else Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleForDirs, [GetCommandParams_Exec]));
    fDirList := TRFileList.Create;
    try
      Script.DoShowInfo(Script, Now, mcOperation, msInfo, Format(SExecuteFindDirs, [GetCommandParams_Exec]));
      sFindRes := CreateDirectoryList(GetCommandParams_Exec, faAnyFile, fDirList, bSubDirs, True, ezOk, Script.DoShowInfo, Script.DoCheckBreak);
      if not (sFindRes.State in [msError, msBreak]) then
      begin
        Script.DoShowInfo(Script, Now, mcOperation, msOk, sFindRes.Result);
        Script.IncTotalOpCount(CmdList.GetCommandCount * fDirList.Count);
        iCount := fDirList.Count - 1;
        for i := 0 to iCount do
        begin
          Script.PutVariable(True, varForDirs, fDirList.Items(i).sName);
          Script.DoShowInfo(Script, Now, mcOperation, msTitle, Format(SExecuteCycleSet, [i + 1, varForDirs, fDirList.Items(i).sName]));
          CmdList.Execute(OpState);
          Script.DoCheckBreak(Script, bTotalBreak);
          if bTotalBreak or CmdList.IsBreakBlock or not Script.Executed
          or (OpState in [stErrorStop, stErrorRestore, stErrorUndo]) then
            Break;
        end;
        Script.DoShowInfo(Script, Now, mcOperation, msOk, Format(SExecuteCycleEnd, [GetCommandText_Exec]));
      end
      else Script.DoShowInfo(Script, Now, mcOperation, sFindRes.State, sFindRes.Result);
    finally
      fDirList.Free;
    end;
  finally
    Script.CmdBlock_Release; // 2011-08-04 fix bug - не выходил из цикла по команде Break
  end;
end;

end.
