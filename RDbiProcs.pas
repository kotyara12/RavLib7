unit RDbiProcs;

interface

uses
  RMsgTypes;

function BdeAliasCreateEx(const AliasName, Driver, Parameters: string;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function BdeAliasCreate(const AliasName, Driver, Parameters: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
function BdeAliasDeleteEx(const AliasName: string;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function BdeAliasDelete(const AliasName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;

implementation

uses
  SysUtils, RSysUtils, RDbiUtils;

resourcestring
  SBdeAliasCreate     = 'Создание псевдонима BDE "%s" (драйвер "%s")';
  SBdeAliasDelete     = 'Удаление псевдонима BDE "%s"';

function BdeAliasCreateEx(const AliasName, Driver, Parameters: string;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
begin
  InitTransaction(Format(SBdeAliasCreate, [AliasName, Driver]), Result, ShowProc);
  try
    try
      RDbiAliasCreateInit(AliasName, Driver, Parameters);
      Result.Result := GetSystemError(S_OK);
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function BdeAliasCreate(const AliasName, Driver, Parameters: string;
  const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(BdeAliasCreateEx(AliasName, Driver, Parameters, ShowProc),
    Result, ErrOptions, ShowProc);
end;

function BdeAliasDeleteEx(const AliasName: string;
  const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
begin
  InitTransaction(Format(SBdeAliasDelete, [AliasName]), Result, ShowProc);
  try
    try
      RDbiAliasDeleteInit(AliasName);
      Result.Result := GetSystemError(S_OK);
    except
      on E: Exception do
      begin
        Result.State := msError;
        Result.Result := E.Message;
      end;
    end;
  finally
    Result.TimeEnd := Now;
  end;
end;

function BdeAliasDelete(const AliasName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(BdeAliasDeleteEx(AliasName, ShowProc), Result, ErrOptions, ShowProc);
end;

end.
