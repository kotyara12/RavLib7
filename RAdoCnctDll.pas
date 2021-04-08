unit RAdoCnctDll;

interface

function GetAdoConnStr(const FileName: string): string;
function ChangeAdoConnStr(const FileName: string): Boolean;

implementation

uses
  SysUtils, Windows, Forms, RVclUtils, RAppStyles, RMsgRu;

type
  TInitDll          = procedure (AppHandle: THandle; const AppStyle: PApplicationStyle);
  TGetAdoConnStr    = function (const FileName: PChar): PChar;
  TChangeAdoConnStr = function (const FileName: PChar): Boolean;

const
  SDllName          = 'RAdoConnStr.dll';
  SInitDll          = 'InitDll';
  SGetAdoConnStr    = 'GetAdoConnStr';
  SChangeAdoConnStr = 'ChangeAdoConnStr';

var
  DllHandle: Cardinal;
  PInitDll: TInitDll;
  PGetAdoConnStr: TGetAdoConnStr;
  PChangeAdoConnStr: TChangeAdoConnStr;

function LoadLibrary_RAdoCnctDll: Boolean;
begin
  DllHandle := LoadLibrary(SDllName);
  if DllHandle = 0 then
    raise EDllException.CreateFmt(SErrLoadLibrary, [SDllName]);
  PInitDll := GetProcAddress(DllHandle, SInitDll);
  if Assigned(PInitDll) then
    PInitDll(Application.Handle, ApplicationStyle)
  else
    raise EDllException.CreateFmt(SErrFindProcedure, [SInitDll, SDllName]);
  PGetAdoConnStr := GetProcAddress(DllHandle, SGetAdoConnStr);
  PChangeAdoConnStr := GetProcAddress(DllHandle, SChangeAdoConnStr);
  Result := True;
end;

procedure FreeLibrary_RAdoCnctDll;
begin
  if DllHandle > 0 then
  begin
    FreeLibrary(DllHandle);
    PInitDll := nil;
    PGetAdoConnStr := nil;
    PChangeAdoConnStr := nil;
  end;
end;

function GetAdoConnStr(const FileName: string): string;
begin
  Result := '';
  try
    if LoadLibrary_RAdoCnctDll then
    begin
      if not Assigned(PGetAdoConnStr) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SGetAdoConnStr, SDllName]);
      Result := string(PGetAdoConnStr(PChar(FileName)));
    end;
  finally
    FreeLibrary_RAdoCnctDll;
  end;
end;

function ChangeAdoConnStr(const FileName: string): Boolean;
begin
  Result := False;
  try
    if LoadLibrary_RAdoCnctDll then
    begin
      if not Assigned(PChangeAdoConnStr) then
        raise EDllException.CreateFmt(SErrFindProcedure, [SChangeAdoConnStr, SDllName]);
      Result := PChangeAdoConnStr(PChar(FileName));
    end;
  finally
    FreeLibrary_RAdoCnctDll;
  end;
end;

end.
