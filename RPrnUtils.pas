unit RPrnUtils;

interface

function RPrinterExists(const PrinterName: string): Boolean;
function RAddPrinter(const PrinterName, PortName, DriverName, Comment, Location: string;
  const ShareName: string = ''; const PrintProcessor: string = 'winprint'): Integer;
function RDeletePrinter(const PrinterName: string): Integer;
function RAddPrinterConnection(const PrinterShareName: string): Integer;
function RDeletePrinterConnection(const PrinterShareName: string): Integer;

implementation

uses
  Windows, WinSpool, SysUtils, Registry, RDialogs;

function RPrinterExists(const PrinterName: string): Boolean;
const
  sPrintersPath = '\System\CurrentControlSet\Control\Print\Printers';
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(STANDARD_RIGHTS_ALL);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(sPrintersPath) then
    begin
      try
        Result := Reg.KeyExists(PrinterName);
      finally
        Reg.CloseKey;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

function RAddPrinter(const PrinterName, PortName, DriverName, Comment, Location: string;
  const ShareName: string = ''; const PrintProcessor: string = 'winprint'): Integer;
var 
  pName: PChar; 
  Level: DWORD; 
  pPrinter: PPrinterInfo2;
begin
  pName := nil; 
  Level := 2; 
  New(pPrinter);
  try
    pPrinter^.pServerName := nil; 
    if PrinterName = EmptyStr 
    then pPrinter^.pPrinterName := nil
    else pPrinter^.pPrinterName := PChar(PrinterName);
    if PortName = EmptyStr 
    then pPrinter^.pPortName := nil
    else pPrinter^.pPortName := PChar(PortName);
    if DriverName = EmptyStr
    then pPrinter^.pDriverName := nil
    else pPrinter^.pDriverName := PChar(DriverName);
    if PrintProcessor = EmptyStr
    then pPrinter^.pPrintProcessor := nil
    else pPrinter^.pPrintProcessor := PChar(PrintProcessor);
    if ShareName = EmptyStr
    then pPrinter^.pShareName := nil
    else pPrinter^.pShareName := PChar(ShareName); 
    if Comment = EmptyStr
    then pPrinter^.pComment := nil
    else pPrinter^.pComment := PChar(Comment); 
    if Location = EmptyStr
    then pPrinter^.pLocation := nil
    else pPrinter^.pLocation := PChar(Location); 
    pPrinter^.pDevMode := nil;
    pPrinter^.pSepFile := nil; 
    pPrinter^.pDatatype := nil; 
    pPrinter^.pParameters := nil; 
    pPrinter^.pSecurityDescriptor := nil; 
    pPrinter^.Attributes := PRINTER_ATTRIBUTE_DEFAULT;
    pPrinter^.Priority := 0;
    pPrinter^.DefaultPriority := 0;
    pPrinter^.StartTime := 0;
    pPrinter^.UntilTime := 0;
    pPrinter^.Status := 0;
    pPrinter^.cJobs := 0;
    pPrinter^.AveragePPM := 0;
    AddPrinter(pName, Level, pPrinter);
    Result := GetLastError;
  finally
    Dispose(pPrinter);
  end; 
end;

function RDeletePrinter(const PrinterName: string): Integer;
var
  hPrinter: THandle;
  pDefault: PPrinterDefaults;
begin
  New(pDefault);
  try
    pDefault^.pDatatype := nil;
    pDefault^.pDevMode := nil;
    pDefault^.DesiredAccess := PRINTER_ALL_ACCESS;
    if OpenPrinter(PAnsiChar(PrinterName), hPrinter, pDefault) then
    begin
      if DeletePrinter(hPrinter) 
      then Result := S_OK 
      else Result := GetLastError;
    end
    else Result := GetLastError;
  finally
    Dispose(pDefault);
  end;
end;

function RAddPrinterConnection(const PrinterShareName: string): Integer;
begin
  if AddPrinterConnection(PChar(PrinterShareName)) 
  then Result := S_OK 
  else Result := GetLastError;
end;

function RDeletePrinterConnection(const PrinterShareName: string): Integer;
begin
  if DeletePrinterConnection(PChar(PrinterShareName)) 
  then Result := S_OK 
  else Result := GetLastError;
end;

end.
