unit RAdoCnctDll3;

interface

uses
  SysUtils, Windows, Forms, RAdoUtils, RVclUtils, RAppStyles, RMsgRu;

function  LoadAdoDbParameters(const FileName, ConnStr: string; var ResqueEnabled: Boolean): string;
function  ChangeAdoDbParameters(const FileName, ConnStr: string): Boolean;
function  EnableDbResqueCopy(const FileName: string): Boolean;
procedure CreateDbResqueCopy(const FileName: string);
function  RestoreDbResqueCopy(const FileName: string): Boolean;

implementation

uses
  RDialogs;

type
  TInitDll               = procedure (AppHandle: THandle; const AppStyle: PApplicationStyle); stdcall;
  TChangeAdoDbParameters = function (ConnStr: PChar): Boolean; stdcall;
  TLoadAdoDbConnection   = procedure (ConnStr: PChar; out FDS: Boolean); stdcall;
  TEnableDbResqueCopy    = function (FileName: PChar): Boolean; stdcall;
  TCreateDbResqueCopy    = procedure (FileName: PChar); stdcall;
  TRestoreDbResqueCopy   = function (FileName: PChar): Boolean; stdcall;

const
  SDllName               = 'RAdoConn.dll';
  SInitDll               = 'InitDll';
  SChangeAdoDbParameters = 'ChangeAdoDbParameters';
  SLoadAdoDbConnection   = 'LoadAdoDbConnection';
  SEnableDbResqueCopy    = 'EnableDbResqueCopy';
  SCreateDbResqueCopy    = 'CreateDbResqueCopy';
  SRestoreDbResqueCopy   = 'RestoreDbResqueCopy';

  PCharBufSize           = 1024;

var
  DllHandle: Cardinal;
  PInitDll: TInitDll;
  PChangeAdoDbParameters: TChangeAdoDbParameters;
  PLoadAdoDbConnection: TLoadAdoDbConnection;
  PEnableDbResqueCopy: TEnableDbResqueCopy;
  PCreateDbResqueCopy: TCreateDbResqueCopy;
  PRestoreDbResqueCopy: TRestoreDbResqueCopy;

procedure LoadLibrary_RAdoCnctDll;
begin
  DllHandle := LoadLibrary(SDllName);
  if DllHandle = 0 then
    raise EDllException.CreateFmt(SErrLoadLibrary, [SDllName]);
  PInitDll := nil;
  PChangeAdoDbParameters := nil;
  PLoadAdoDbConnection := nil;
  PEnableDbResqueCopy := nil;
  PCreateDbResqueCopy := nil;
  PRestoreDbResqueCopy := nil;
end;

procedure InitLibrary_RAdoCnctDll;
begin
  if DllHandle > 0 then
  begin
    PInitDll := GetProcAddress(DllHandle, SInitDll);
    if Assigned(PInitDll)
    then PInitDll(Application.Handle, ApplicationStyle)
    else raise EDllException.CreateFmt(SErrFindProcedure, [SInitDll, SDllName]);
    PChangeAdoDbParameters := GetProcAddress(DllHandle, SChangeAdoDbParameters);
    if not Assigned(PChangeAdoDbParameters) then
      raise EDllException.CreateFmt(SErrFindProcedure, [SChangeAdoDbParameters, SDllName]);
    PLoadAdoDbConnection := GetProcAddress(DllHandle, SLoadAdoDbConnection);
    if not Assigned(PLoadAdoDbConnection) then
      raise EDllException.CreateFmt(SErrFindProcedure, [SLoadAdoDbConnection, SDllName]);
    PEnableDbResqueCopy := GetProcAddress(DllHandle, SEnableDbResqueCopy);
    if not Assigned(PEnableDbResqueCopy) then
      raise EDllException.CreateFmt(SErrFindProcedure, [SEnableDbResqueCopy, SDllName]);
    PCreateDbResqueCopy := GetProcAddress(DllHandle, SCreateDbResqueCopy);
    if not Assigned(PCreateDbResqueCopy) then
      raise EDllException.CreateFmt(SErrFindProcedure, [SCreateDbResqueCopy, SDllName]);
    PRestoreDbResqueCopy := GetProcAddress(DllHandle, SRestoreDbResqueCopy);
    if not Assigned(PRestoreDbResqueCopy) then
      raise EDllException.CreateFmt(SErrFindProcedure, [SRestoreDbResqueCopy, SDllName]);
  end
  else raise EDllException.CreateFmt(SErrLoadLibrary, [SDllName]);
end;

procedure FreeLibrary_RAdoCnctDll;
begin
  if DllHandle > 0 then
  begin
    FreeLibrary(DllHandle);
    PInitDll := nil;
    PChangeAdoDbParameters := nil;
    PLoadAdoDbConnection := nil;
    PEnableDbResqueCopy := nil;
    PCreateDbResqueCopy := nil;
    PRestoreDbResqueCopy := nil;
  end;
end;

function ChangeAdoDbParameters(const FileName, ConnStr: string): Boolean;
var
  Buffer: array [0..PCharBufSize - 1] of Char;
begin
  LoadLibrary_RAdoCnctDll;
  try
    InitLibrary_RAdoCnctDll;
    StrPLCopy(Buffer, FileName + #13 + ConnStr, SizeOf(Buffer));
    Result := PChangeAdoDbParameters(Buffer);
  finally
    FreeLibrary_RAdoCnctDll;
  end;
end;

function LoadAdoDbParameters(const FileName, ConnStr: string; var ResqueEnabled: Boolean): string;
var
  Buffer: array [0..PCharBufSize - 1] of Char;
begin
  LoadLibrary_RAdoCnctDll;
  try
    InitLibrary_RAdoCnctDll;
    StrPLCopy(@Buffer, FileName + #13 + ConnStr, SizeOf(Buffer));
    PLoadAdoDbConnection(Buffer, ResqueEnabled);
    Result := string(Buffer);
  finally
    FreeLibrary_RAdoCnctDll;
  end;
end;

function EnableDbResqueCopy(const FileName: string): Boolean;
var
  Buffer: array [0..PCharBufSize - 1] of Char;
begin
  LoadLibrary_RAdoCnctDll;
  try
    InitLibrary_RAdoCnctDll;
    StrPLCopy(Buffer, FileName, SizeOf(Buffer));
    Result := PEnableDbResqueCopy(Buffer);
  finally
    FreeLibrary_RAdoCnctDll;
  end;
end;

procedure CreateDbResqueCopy(const FileName: string);
var
  Buffer: array [0..PCharBufSize - 1] of Char;
begin
  LoadLibrary_RAdoCnctDll;
  try
    InitLibrary_RAdoCnctDll;
    StrPLCopy(Buffer, FileName, SizeOf(Buffer));
    PCreateDbResqueCopy(Buffer);
  finally
    FreeLibrary_RAdoCnctDll;
  end;
end;

function RestoreDbResqueCopy(const FileName: string): Boolean;
var
  Buffer: array [0..PCharBufSize - 1] of Char;
begin
  LoadLibrary_RAdoCnctDll;
  try
    InitLibrary_RAdoCnctDll;
    StrPLCopy(Buffer, FileName, SizeOf(Buffer));
    Result := PRestoreDbResqueCopy(Buffer);
  finally
    FreeLibrary_RAdoCnctDll;
  end;
end;

end.
