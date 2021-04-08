unit RMsgTypes;

interface

uses
  Windows, SysUtils, Graphics;

{ == Log messages ============================================================== }

type
  TRMsgClass       = (mcSystem, mcCommand, mcOperation, mcTransaction);
  TRMsgState       = (msTitle, msInfo, msOk, msIgnored, msWarning, msError, msBreak);

  TREmptyState     = (ezOk, ezWarning, ezError);
  TRErrorsOption   = (esErrorStop, esErrorIgnore, esErrorWarning, esErrorShow);
  TRErrorsOptions  = set of TRErrorsOption;

  TRTransaction = packed record
    Title: string;
    Result: string;
    TimeBegin: TDateTime;
    TimeEnd: TDateTime;
    State: TRMsgState;
    Size: Int64;
  end;

  TRTransactions = array of TRTransaction;

  TROperation = packed record
    Title: string;
    Result: string;
    TimeBegin: TDateTime;
    TimeEnd: TDateTime;
    State: TRMsgState;
    Size: Int64;
    Total: DWord;
    Goods: DWord;
    Ignored: DWord;
    Warnings: DWord;
    Errors: DWord;
    Details: TRTransactions;
  end;

  TRShowInfoNotifyEvent = procedure (Sender: TObject; const TimeStamp: TDateTime;
    const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string) of object;
  TRShowProgressNotifyEvent = procedure (Sender: TObject; const MsgClass: TRMsgClass;
    const CurrPos, MaxPos: Integer) of object;
  TRFileProgressNotifyEvent = procedure (Sender: TObject; const CurrPos, MaxPos: Integer) of object;
  TRCheckBreakNotifyEvent = procedure (Sender: TObject; var IsBreak: Boolean) of object;
  TRCalcNetUsageNotifyEvent = procedure (Sender: TObject; var NetUsage: Byte) of object;

{ == Log messages functions ==================================================== }

function MsgIsLog(const MsgClass: TRMsgClass; const MsgState: TRMsgState): Boolean;
function MsgIsShow(const MsgClass: TRMsgClass; const MsgState: TRMsgState): Boolean;
function MsgFormat(const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string): string;
function MsgFormatEx(const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string): string;

function CopyOperationToTransaction(Operation: TROperation): TRTransaction;
function FormatTransaction(const Transaction: TRTransaction): string;
function FormatOperation(const Operation: TROperation): string;

procedure ShowMessageCustom(Sender: TObject; const TimeStamp: TDateTime;
  const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string;
  const ShowProc: TRShowInfoNotifyEvent);
procedure ShowProgressCustom(Sender: TObject; const MsgClass: TRMsgClass;
  const CurrPos, MaxPos: Integer; const PrgsProc: TRShowProgressNotifyEvent);
procedure ShowProgressFile(Sender: TObject; const CurrPos, MaxPos: Integer;
  const PrgsFile: TRFileProgressNotifyEvent);

procedure ShowTransactionTitle(const Transaction: TRTransaction;
  const ShowProc: TRShowInfoNotifyEvent);
procedure ShowTransactionCustom(const Transaction: TRTransaction; const State: TRMsgState;
  const ShowProc: TRShowInfoNotifyEvent);
procedure ShowTransactionResult(const Transaction: TRTransaction;
  const ShowProc: TRShowInfoNotifyEvent);
procedure ShowTransaction(const Transaction: TRTransaction;
  const ShowProc: TRShowInfoNotifyEvent);
procedure ShowOperationTitle(const Operation: TROperation;
  const ShowProc: TRShowInfoNotifyEvent);
procedure ShowOperationCustom(const Operation: TROperation; const State: TRMsgState;
  const ShowProc: TRShowInfoNotifyEvent);
procedure ShowOperationResult(const Operation: TROperation;
  const ShowProc: TRShowInfoNotifyEvent);
procedure ShowOperation(const Operation: TROperation;
  const ShowProc: TRShowInfoNotifyEvent);

procedure InitTransactionEx(const ATitle: string; var Transaction: TRTransaction);
procedure InitTransaction(const ATitle: string; var Transaction: TRTransaction;
  const ShowProc: TRShowInfoNotifyEvent);
procedure InitOperationEx(const ATitle: string; var Operation: TROperation);
procedure InitOperation(const ATitle: string; var Operation: TROperation;
  const ShowProc: TRShowInfoNotifyEvent);
procedure DoneOperationEx(var Operation: TROperation; const EmptyState: TREmptyState);
procedure DoneOperation(var Operation: TROperation; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent);

procedure PutSystemErrorToTransaction(var Transaction: TRTransaction;
  const RetCode: Integer; const Ignored: Integer = 0);
procedure PutTransactionToOperation(const Transaction: TRTransaction;
  var Operation: TROperation; const ErrorOptions: TRErrorsOptions;
  const CheckErrors: Boolean; var IsBreak: Boolean;
  const ShowProc: TRShowInfoNotifyEvent);
procedure SetTransactionToOperation(const Transaction: TRTransaction;
  var Operation: TROperation; const ErrorOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent);

{ == Check Terminate functions ================================================= }

function IsTerminatedBool(const BreakProc: TRCheckBreakNotifyEvent; var IsBreak: Boolean): Boolean;
function IsTerminatedTran(const BreakProc: TRCheckBreakNotifyEvent; var Transaction: TRTransaction): Boolean;
function IsTerminatedOper(const BreakProc: TRCheckBreakNotifyEvent; var Operation: TROperation; var IsBreak: Boolean): Boolean;

procedure WaitTerminated(const Seconds: Cardinal; var IsBreak: Boolean; const WaitText: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent);

{ == Calculate Net Usage Level (1-100) ========================================= }
function CalculateNetUsage(const NetUsageProc: TRCalcNetUsageNotifyEvent; const DefNetUsage: Byte): Byte;

resourcestring

  SOprResultCounts   = 'Всего - %d, обработано - %d, пропущено - %d, предупреждений - %d, ошибок - %d.';
  SOprResultSize     = 'Всего - %d, обработано - %d ( %n Kb ), пропущено - %d, предупреждений - %d, ошибок - %d.';
  SOprMsgTypeBreak   = 'Операция прервана по команде пользователя или системы!';
  SOprViewWarning    = '[%s] Предупреждение при выполнении операции "%s":'#13#10'%s!';
  SOprViewError      = '[%s] Ошибка при выполнении операции "%s":'#13#10'%s!';
  SOprWaitSeconds    = 'Пауза %d секунд...';

  SSystemTitle       = '# %s';
  SSystemResult      = '# [%s] %s';
  SCommandTitle      = '@ %s';
  SCommandResult     = '@ [%s] %s';
  SOperationTitle    = '%s';
  SOperationResult   = '> [%s] %s';
  STransactionTitle  = '%s';
  STransactionResult = '[%s] %s';

const
  SFmtTransaction    = '%0:s - %1:s';
  SFmtOperation      = '%0:s - %1:s';

  SRMsgState          : array [TRMsgState] of string =
    ('HEADER', 'INFO', 'OK', 'SKIP', 'WARNING', 'ERROR', 'STOP');
  CRMsgState          : array [TRMsgState] of TColor =
    (clBlack, clGreen, clNavy, clDkGray, clMaroon, clMaroon, clRed);

{ ============================================================================== }

implementation

uses
  RSysUtils, RMsgViewer, RDialogs;

{ == Log messages functions ==================================================== }

function MsgIsShow(const MsgClass: TRMsgClass; const MsgState: TRMsgState): Boolean;
begin
  Result := MsgState in [msTitle, msWarning, msError, msBreak];
end;

function MsgIsLog(const MsgClass: TRMsgClass; const MsgState: TRMsgState): Boolean;
begin
  Result := (MsgClass in [mcSystem, mcOperation])
         or (MsgState in [msInfo, msOk, msIgnored, msWarning, msError, msBreak])
         or ((MsgClass = mcCommand) and (MsgState in [msInfo]));
end;

function MsgFormat(const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string): string;
begin
  case MsgClass of
    mcSystem:
      Result := '# ' + MsgText;
    mcCommand:
      Result := '@ ' + MsgText;
    mcOperation:
      case MsgState of
        msTitle: Result := MsgText;
        else Result := '> ' + MsgText;
      end;
    mcTransaction:
      Result := '>> ' + MsgText;
    else Result := MsgText;
  end;
end;

function MsgFormatEx(const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string): string;
begin
  case MsgClass of
    mcSystem:
      if MsgState in [msTitle, msInfo]
      then Result := Format(SSystemTitle, [MsgText])
      else Result := Format(SSystemResult, [SRMsgState[MsgState], MsgText]);
    mcCommand:
      if MsgState in [msTitle, msInfo]
      then Result := Format(SCommandTitle, [MsgText])
      else Result := Format(SCommandResult, [SRMsgState[MsgState], MsgText]);
    mcOperation:
      if MsgState in [msTitle, msInfo]
      then Result := Format(SOperationTitle, [MsgText])
      else Result := Format(SOperationResult, [SRMsgState[MsgState], MsgText]);
    mcTransaction:
      if MsgState in [msTitle, msInfo]
      then Result := Format(STransactionTitle, [MsgText])
      else Result := Format(STransactionResult, [SRMsgState[MsgState], MsgText]);
    else Result := MsgText;
  end;
end;

function CopyOperationToTransaction(Operation: TROperation): TRTransaction;
begin
  Result.Title := Operation.Title;
  Result.Result := Operation.Result;
  Result.State := Operation.State;
  Result.TimeBegin := Operation.TimeBegin;
  Result.TimeEnd := Operation.TimeEnd;
  Result.Size := Operation.Size;
end;

function FormatTransaction(const Transaction: TRTransaction): string;
begin
  Result := Format(SFmtTransaction, [Transaction.Title, Transaction.Result]);
end;

function FormatOperation(const Operation: TROperation): string;
begin
  Result := Format(SFmtOperation, [Operation.Title, Operation.Result]);
end;

procedure ShowMessageCustom(Sender: TObject; const TimeStamp: TDateTime;
  const MsgClass: TRMsgClass; const MsgState: TRMsgState; const MsgText: string;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(Sender, TimeStamp, MsgClass, MsgState, MsgText);
end;

procedure ShowProgressCustom(Sender: TObject; const MsgClass: TRMsgClass;
  const CurrPos, MaxPos: Integer; const PrgsProc: TRShowProgressNotifyEvent);
begin
  if Assigned(PrgsProc) then
    PrgsProc(Sender, MsgClass, CurrPos, MaxPos);
end;

procedure ShowProgressFile(Sender: TObject; const CurrPos, MaxPos: Integer;
  const PrgsFile: TRFileProgressNotifyEvent);
begin
  if Assigned(PrgsFile) then
    PrgsFile(Sender, CurrPos, MaxPos);
end;

procedure ShowTransactionTitle(const Transaction: TRTransaction;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(nil, Transaction.TimeBegin, mcTransaction, msTitle, Transaction.Title);
end;

procedure ShowTransactionCustom(const Transaction: TRTransaction; const State: TRMsgState;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(nil, Transaction.TimeBegin, mcTransaction, State, Transaction.Title);
end;

procedure ShowTransactionResult(const Transaction: TRTransaction;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(nil, Transaction.TimeEnd, mcTransaction, Transaction.State, Transaction.Result);
end;

procedure ShowTransaction(const Transaction: TRTransaction;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(nil, Transaction.TimeEnd, mcTransaction, Transaction.State,
      Format(SFmtTransaction, [Transaction.Title, Transaction.Result]));
end;

procedure ShowOperationTitle(const Operation: TROperation;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(nil, Operation.TimeBegin, mcOperation, msTitle, Operation.Title);
end;

procedure ShowOperationCustom(const Operation: TROperation; const State: TRMsgState;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(nil, Operation.TimeBegin, mcOperation, State, Operation.Title);
end;

procedure ShowOperationResult(const Operation: TROperation;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(nil, Operation.TimeEnd, mcOperation, Operation.State, Operation.Result);
end;

procedure ShowOperation(const Operation: TROperation;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  if Assigned(ShowProc) then
    ShowProc(nil, Operation.TimeEnd, mcOperation, Operation.State,
      Format(SFmtOperation, [Operation.Title, Operation.Result]));
end;

procedure InitTransactionEx(const ATitle: string; var Transaction: TRTransaction);
begin
  Transaction.Title := ATitle;
  Transaction.Result := EmptyStr;
  Transaction.TimeBegin := Now;
  Transaction.TimeEnd := 0.0;
  Transaction.State := msOk;
  Transaction.Size := 0;
end;

procedure InitTransaction(const ATitle: string; var Transaction: TRTransaction;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  InitTransactionEx(ATitle, Transaction);
  ShowTransactionTitle(Transaction, ShowProc);
end;

procedure InitOperationEx(const ATitle: string; var Operation: TROperation);
begin
  SetLength(Operation.Details, 0);
  Operation.Title := ATitle;
  Operation.Result := EmptyStr;
  Operation.TimeBegin := Now;
  Operation.TimeEnd := 0.0;
  Operation.State := msOk;
  Operation.Size := 0;
  Operation.Total := 0;
  Operation.Goods := 0;
  Operation.Ignored := 0;
  Operation.Warnings := 0;
  Operation.Errors := 0;
end;

procedure InitOperation(const ATitle: string; var Operation: TROperation;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  InitOperationEx(ATitle, Operation);
  ShowOperationTitle(Operation, ShowProc);
end;

procedure DoneOperationEx(var Operation: TROperation; const EmptyState: TREmptyState);
var
  TotalSize: Extended;
begin
  if Operation.State = msBreak then
  begin
    if Operation.Result = EmptyStr then
      Operation.Result := SOprMsgTypeBreak;
  end
  else begin
    if Operation.Total = 0 then
    begin
      case EmptyState of
        ezWarning:
          if Operation.State < msWarning then
            Operation.State := msWarning;
        ezError:
          if Operation.State < msError then
            Operation.State := msError;
      end;
    end;
    if Operation.Size = 0
    then Operation.Result := Format(SOprResultCounts, [Operation.Total,
      Operation.Goods, Operation.Ignored, Operation.Warnings, Operation.Errors])
    else begin
      TotalSize := Operation.Size / 1024;
      Operation.Result := Format(SOprResultSize, [Operation.Total,
        Operation.Goods, TotalSize, Operation.Ignored, Operation.Warnings, Operation.Errors]);
    end;
  end;
  Operation.TimeEnd := Now;
end;

procedure DoneOperation(var Operation: TROperation; const EmptyState: TREmptyState;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  DoneOperationEx(Operation, EmptyState);
  ShowOperationResult(Operation, ShowProc);
end;

procedure PutSystemErrorToTransaction(var Transaction: TRTransaction;
  const RetCode: Integer; const Ignored: Integer = 0);
begin
  if RetCode = NO_ERROR
  then Transaction.State := msOk
  else if RetCode = Ignored
       then Transaction.State := msIgnored
       else Transaction.State := msError;
  Transaction.TimeEnd := Now;
  Transaction.Result := GetSystemError(RetCode, True);
end;

procedure PutTransactionToOperation(const Transaction: TRTransaction;
  var Operation: TROperation; const ErrorOptions: TRErrorsOptions;
  const CheckErrors: Boolean; var IsBreak: Boolean;
  const ShowProc: TRShowInfoNotifyEvent);
begin
  SetLength(Operation.Details, Length(Operation.Details) + 1);
  Operation.Details[High(Operation.Details)] := Transaction;
  case Transaction.State of
    msOk:
    begin
      Inc(Operation.Goods);
      Operation.Size := Operation.Size + Transaction.Size;
    end;
    msIgnored: Inc(Operation.Ignored);
    msWarning:
    begin
      Inc(Operation.Warnings);
      if Operation.State < msWarning then
        Operation.State := msWarning;
      if esErrorShow in ErrorOptions then
        ShowMsgViewer(EmptyStr,
          Format(SOprViewWarning, [DateTimeToStr(Transaction.TimeEnd),
            Transaction.Title, Transaction.Result]),
          CRMsgState[Transaction.State], []);
    end;
    msError:
    begin
      if CheckErrors then
      begin
        Inc(Operation.Errors);
        if not (esErrorIgnore in ErrorOptions) then
        begin
          if esErrorWarning in ErrorOptions then
          begin
            if Operation.State < msWarning then
              Operation.State := msWarning;
          end
          else begin
            if Operation.State < msError then
              Operation.State := msError;
          end;
        end;
        IsBreak := esErrorStop in ErrorOptions;
        if esErrorShow in ErrorOptions then
          ShowMsgViewer(EmptyStr,
            Format(SOprViewError, [DateTimeToStr(Transaction.TimeEnd),
              Transaction.Title, Transaction.Result]),
            CRMsgState[Transaction.State], []);
      end;
    end;
    msBreak:
    begin
      IsBreak := True;
      Operation.State := msBreak;
    end;
  end;
  ShowTransaction(Transaction, ShowProc);
end;

procedure SetTransactionToOperation(const Transaction: TRTransaction;
  var Operation: TROperation; const ErrorOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent);
var
  ShowSimple: Boolean;
begin
  Operation.Title := Transaction.Title;
  Operation.Result := Transaction.Result;
  Operation.TimeBegin := Transaction.TimeBegin;
  Operation.TimeEnd := Transaction.TimeEnd;
  SetLength(Operation.Details, Length(Operation.Details) + 1);
  Operation.Details[High(Operation.Details)] := Transaction;
  Inc(Operation.Total);
  ShowSimple := False;
  case Transaction.State of
    msOk:
    begin
      Inc(Operation.Goods);
      Operation.Size := Operation.Size + Transaction.Size;
      Operation.State := msOk;
    end;
    msIgnored:
    begin
      Inc(Operation.Ignored);
      Operation.State := msIgnored;
    end;
    msWarning:
    begin
      Inc(Operation.Warnings);
      Operation.State := msWarning;
      if esErrorShow in ErrorOptions then
        ShowMsgViewer(EmptyStr,
          Format(SOprViewWarning, [DateTimeToStr(Transaction.TimeEnd),
            Transaction.Title, Transaction.Result]),
          CRMsgState[Transaction.State], []);
    end;
    msError:
    begin
      Inc(Operation.Errors);
      if not (esErrorIgnore in ErrorOptions) then
      begin
        ShowSimple := True;
        if esErrorWarning in ErrorOptions then
        begin
          if Operation.State < msWarning then
            Operation.State := msWarning;
        end
        else begin
          if Operation.State < msError then
            Operation.State := msError;
        end;
      end;
      if esErrorShow in ErrorOptions then
        ShowMsgViewer(EmptyStr,
          Format(SOprViewError, [DateTimeToStr(Transaction.TimeEnd),
            Transaction.Title, Transaction.Result]),
          CRMsgState[Transaction.State], []);
    end;
    msBreak:
      Operation.State := msBreak;
  end;
  ShowOperationTitle(Operation, ShowProc);
  if ShowSimple then
    ShowTransaction(Transaction, ShowProc);
  ShowOperationResult(Operation, ShowProc);
end;

{ == Check Terminate functions ================================================= }

function IsTerminatedBool(const BreakProc: TRCheckBreakNotifyEvent; var IsBreak: Boolean): Boolean;
begin
  Result := IsBreak;
  if not Result then
  begin
    if Assigned(BreakProc) then BreakProc(nil, IsBreak);
    Result := IsBreak;
  end;
end;

function IsTerminatedTran(const BreakProc: TRCheckBreakNotifyEvent; var Transaction: TRTransaction): Boolean;
begin
  Result := Transaction.State = msBreak;
  if not Result then
  begin
    if Assigned(BreakProc) then BreakProc(nil, Result);
    if Result then
    begin
      Transaction.State := msBreak;
      Transaction.Result := SOprMsgTypeBreak;
      Transaction.TimeEnd := Now;
    end;
  end;
end;

function IsTerminatedOper(const BreakProc: TRCheckBreakNotifyEvent; var Operation: TROperation; var IsBreak: Boolean): Boolean;
begin
  Result := IsBreak or (Operation.State = msBreak);
  if not Result then
  begin
    if Assigned(BreakProc) then BreakProc(nil, Result);
    if Result then
    begin
      IsBreak := Result;
      Operation.State := msBreak;
      Operation.Result := SOprMsgTypeBreak;
      Operation.TimeEnd := Now;
    end;
  end;
end;

procedure WaitTerminated(const Seconds: Cardinal; var IsBreak: Boolean; const WaitText: string;
  const ShowProc: TRShowInfoNotifyEvent; const BreakProc: TRCheckBreakNotifyEvent);
const
  MaxDelay = 100;
var
  StartTick, TotalValue: Cardinal;
  StepValue: Integer;
begin
  if (Seconds > 0) and not IsTerminatedBool(BreakProc, IsBreak) then
  begin
    if Assigned(ShowProc) then
    begin
      if WaitText = EmptyStr
      then ShowProc(nil, Now, mcTransaction, msTitle, Format(SOprWaitSeconds, [Seconds]))
      else ShowProc(nil, Now, mcTransaction, msTitle, WaitText);
    end;
    StartTick := GetTickCount;
    TotalValue := Seconds * 1000;
    StepValue := TotalValue;
    repeat
      if IsTerminatedBool(BreakProc, IsBreak) then Break;
      StepValue := TotalValue - (GetTickCount - StartTick);
      if StepValue > MaxDelay then StepValue := MaxDelay;
      if StepValue > 0 then Sleep(StepValue);
    until StepValue < 1;
  end;
end;

{ == Calculate Net Usage Level (1-100) ========================================= }
function CalculateNetUsage(const NetUsageProc: TRCalcNetUsageNotifyEvent; const DefNetUsage: Byte): Byte;
begin
  Result := DefNetUsage;
  if Assigned(NetUsageProc) then
    NetUsageProc(nil, Result);
  if Result < 1 then Result := 1;
  if Result > 100 then Result := 100;
end;

end.
