unit RPrnProcs;

interface

uses
  RMsgTypes;

function AddPrinterEx(const PrinterName, PortName, DriverName, Comment, Location: string;
  const ShareName: string; const PrintProcessor: string; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function AddPrinter(const PrinterName, PortName, DriverName, Comment, Location: string;
  const ShareName: string; const PrintProcessor: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
function DelPrinterEx(const PrinterName: string; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function DelPrinter(const PrinterName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;

function AddNetworkPrinterEx(const PrinterName: string; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function AddNetworkPrinter(const PrinterName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
function DelNetworkPrinterEx(const PrinterName: string; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
function DelNetworkPrinter(const PrinterName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;

implementation

uses
  SysUtils, RSysUtils, RPrnUtils;

resourcestring
  SAddPrinter     = 'Создание принтера "%s" (драйвер "%s", порт "%s")';
  SDelPrinter     = 'Удаление принтера "%s"';
  SAddNetPrinter  = 'Подключение принтера "%s"';
  SDelNetPrinter  = 'Отключение принтера "%s"';

function AddPrinterEx(const PrinterName, PortName, DriverName, Comment, Location: string;
  const ShareName: string; const PrintProcessor: string; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
var
  RetCode: Integer;
begin
  InitTransaction(Format(SAddPrinter, [PrinterName, DriverName, PortName]), Result, ShowProc);
  try
    try
      RetCode := RAddPrinter(PrinterName, PortName, DriverName, 
        Comment, Location, ShareName, PrintProcessor);
      if RetCode = S_OK 
      then Result.State := msOk 
      else Result.State := msError;
      Result.Result := GetSystemError(RetCode);
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

function AddPrinter(const PrinterName, PortName, DriverName, Comment, Location: string;
  const ShareName: string; const PrintProcessor: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(AddPrinterEx(PrinterName, PortName, DriverName, Comment,
    Location, ShareName, PrintProcessor, ShowProc), Result, ErrOptions, ShowProc);
end;

function DelPrinterEx(const PrinterName: string; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
var
  RetCode: Integer;
begin
  InitTransaction(Format(SDelPrinter, [PrinterName]), Result, ShowProc);
  try
    try
      RetCode := RDeletePrinter(PrinterName);
      if RetCode = S_OK 
      then Result.State := msOk 
      else Result.State := msError;
      Result.Result := GetSystemError(RetCode);
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

function DelPrinter(const PrinterName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(DelPrinterEx(PrinterName, ShowProc),
    Result, ErrOptions, ShowProc);
end;

function AddNetworkPrinterEx(const PrinterName: string; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
var
  RetCode: Integer;
begin
  InitTransaction(Format(SAddNetPrinter, [PrinterName]), Result, ShowProc);
  try
    try
      RetCode := RAddPrinterConnection(PrinterName);
      if RetCode = S_OK
      then Result.State := msOk
      else Result.State := msError;
      Result.Result := GetSystemError(RetCode);
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

function AddNetworkPrinter(const PrinterName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(AddNetworkPrinterEx(PrinterName, ShowProc), Result, ErrOptions, ShowProc);
end;

function DelNetworkPrinterEx(const PrinterName: string; const ShowProc: TRShowInfoNotifyEvent): TRTransaction;
var
  RetCode: Integer;
begin
  InitTransaction(Format(SDelNetPrinter, [PrinterName]), Result, ShowProc);
  try
    try
      RetCode := RDeletePrinterConnection(PrinterName);
      if RetCode = S_OK 
      then Result.State := msOk 
      else Result.State := msError;
      Result.Result := GetSystemError(RetCode);
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

function DelNetworkPrinter(const PrinterName: string; const ErrOptions: TRErrorsOptions;
  const ShowProc: TRShowInfoNotifyEvent): TROperation;
begin
  SetTransactionToOperation(DelNetworkPrinterEx(PrinterName, ShowProc), Result, ErrOptions, ShowProc);
end;

end.
