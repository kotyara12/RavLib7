library RAdoConnStr;

uses
  SysUtils,
  Classes,
  Forms,
  Windows,
  RAdoODBC in '..\..\RAdoODBC.pas',
  RAppStyles in '..\..\RAppStyles.pas',
  TmplBase in '..\..\Templates\TmplBase.pas' {BaseTemplate},
  TmplDialog in '..\..\Templates\TmplDialog.pas' {DialogTemplate},
  RDialogs in '..\..\RDialogs.pas';

{$R *.res}

resourcestring
  EDllError   = 'Ошибка в библиотеке RAdoConnStr.Dll!'#13#13'%s'#13'Класс: %s';

var
  DllApp: THandle;

procedure InitDll(AppHandle: THandle; const AppStyle: PApplicationStyle);
begin
  DllApp := Application.Handle;
  Application.Handle := AppHandle;
  ApplicationStyle := AppStyle;
end;

procedure DoneDllProc(Reason: Integer);
begin
  if Reason = DLL_PROCESS_DETACH then
  begin
    Application.Handle := DllApp;
    DllApp := 0;
  end;
end;

function GetAdoConnStr(const FileName: PChar): PChar;
begin
  Result := '';
  try
    Result := PChar(GetConnectionString(string(FileName)));
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

function ChangeAdoConnStr(const FileName: PChar): Boolean;
begin
  Result := False;
  try
    Result := ChangeConnectionString(string(FileName));
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

exports
  InitDll name 'InitDll',
  GetAdoConnStr name 'GetAdoConnStr',
  ChangeAdoConnStr name 'ChangeAdoConnStr';

begin
  DllProc := @DoneDllProc;
end.
