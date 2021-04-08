library RAdoConn;

uses
  SysUtils,
  Classes,
  Forms,
  Windows,
  ActiveX,
  RAdoUtils in '..\..\RAdoUtils.pas',
  RAppStyles in '..\..\RAppStyles.pas',
  RDialogs in '..\..\RDialogs.pas',
  RStrUtils in '..\..\RStrUtils.pas',
  TmplBase in '..\..\Templates\TmplBase.pas' {BaseTemplate},
  TmplDialog in '..\..\Templates\TmplDialog.pas' {DialogTemplate},
  MainDialog in 'MainDialog.pas' {FormParameters};

{$R *.res}

resourcestring
  EDllError   = 'Ошибка в библиотеке RAdoConn.Dll!'#13#13'%s'#13'Класс: %s';

const
  PCharBufSize           = 1024;

var
  DllHandle: THandle;
  CoInit: Boolean;

procedure InitDll(AppHandle: THandle; const AppStyle: PApplicationStyle); stdcall;
begin
  DllHandle := Application.Handle;
  Application.Handle := AppHandle;
  ApplicationStyle := AppStyle;
  CoInit := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED + COINIT_APARTMENTTHREADED));
  if not CoInit then CoInit := Succeeded(CoInitialize(nil));
end;

procedure DoneDllProc(Reason: Integer);
begin
  if Reason = DLL_PROCESS_DETACH then
  begin
    if CoInit then CoUninitialize;
    Application.Handle := DllHandle;
    DllHandle := 0;
  end;
end;

function ChangeAdoDbParameters(ConnStr: PChar): Boolean; stdcall;
var
  sFileName, sConnStr: string;
begin
  Result := False;
  try
    ParseInputParameters(string(ConnStr), sFileName, sConnStr);
    Result := ChangeDbParameters(sFileName,
      ExtractDbConnectStr(sConnStr),
      ExtractDbParameters(sConnStr));
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

procedure LoadAdoDbConnection(ConnStr: PChar; out FDS: Boolean); stdcall;
var
  sFileName, sConnStr: string;
begin
  try
    ParseInputParameters(string(ConnStr), sFileName, sConnStr);
    sConnStr := MixConnStrAndParams(
      LoadConnectionString(sFileName, ExtractDbConnectStr(sConnStr)),
      LoadConnectionParams(sFileName, ExtractDbParameters(sConnStr)));
    ZeroMemory(ConnStr, PCharBufSize);
    StrPLCopy(ConnStr, sConnStr, PCharBufSize - 1);
    FDS := IsFileDataSource(sConnStr)
      and LoadConnectionRqCopy(sFileName).ResqueEnabled;
    if Trim(sConnStr) <> EmptyStr then
      CreateResqueCopy(sFileName, False, True);
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

function EnableDbResqueCopy(FileName: PChar): Boolean; stdcall;
begin
  Result := EnableResqueCopy(string(FileName));
end;

procedure CreateDbResqueCopy(FileName: PChar); stdcall;
begin
  CreateResqueCopy(string(FileName), True, True);
end;

function RestoreDbResqueCopy(FileName: PChar): Boolean; stdcall;
begin
  Result := RestoreResqueCopy(string(FileName));
end;

exports
  InitDll,
  ChangeAdoDbParameters,
  LoadAdoDbConnection,
  EnableDbResqueCopy,
  CreateDbResqueCopy,
  RestoreDbResqueCopy;

begin
  DllProc := @DoneDllProc;
end.
