library RLogin;

uses
  SysUtils,
  Classes,
  Windows,
  Forms,
  AdoDb,
  RMsgRu in '..\..\RMsgRu.pas',
  RVclUtils in '..\..\RVclUtils.pas',
  RSysUtils in '..\..\RSysUtils.pas',
  RDialogs in '..\..\RDialogs.pas',
  RAppStyles in '..\..\RAppStyles.pas',
  RUserRights in '..\..\RUserRights.pas',
  RExHandlers in '..\..\RExHandlers.pas',
  RDbConst in '..\..\RDbConst.pas',
  RDbUtils in '..\..\RDbUtils.pas',
  RDbSettings in '..\..\RDbSettings.pas',
  RDbLog in '..\..\RDbLog.pas',
  RExHandlersExDlg in '..\..\RExHandlersExDlg.pas' {ExtErrorBox},
  RExHandlersDbLog in '..\..\RExHandlersDbLog.pas',
  RCryptApiEx in '..\..\RCryptApiEx.pas',
  RRssConst in '..\..\RRssConst.pas',
  RRssBase in '..\..\RRssBase.pas',
  LoginProcs in 'LoginProcs.pas',
  TmplBase in '..\..\Templates\TmplBase.pas' {BaseTemplate},
  TmplDialog in '..\..\Templates\TmplDialog.pas' {DialogTemplate},
  LoginForm in 'LoginForm.pas' {FormLogin},
  ChPwdForm in 'ChPwdForm.pas' {FormChPwd};

{$R *.res}

resourcestring
  EDllError        = '������ � ���������� RLogin.Dll!'#13#13'%s'#13'�����: %s';

var
  // DllApp: THandle;
  Db: TAdoconnection;

{ == ������������� ���������� ================================================== }
function InitDll(AppHandle: THandle; const AppStyle: PApplicationStyle;
  const ConnectionStr: PChar; const ArmTag: Integer; const User: TUserRights): Boolean;
begin
  Result := False;
  try
    // ��������� ����� ���������� ��� ����������� ���������� ������
    // DllApp := Application.Handle;
    // Application.Handle := AppHandle;
    // ������������� ����� ����
    ApplicationStyle := AppStyle;
    // ������� ���������� ������
    AppExceptionsHandler := TExceptHandler.Create;
    Application.OnException := AppExceptionsHandler.AppExceptHandler;
    AppExceptionsHandler.ChannelCreate(TExDlgExceptChannel, 200, True);
    AppExceptionsHandler.ChannelCreate(TDbLogExceptChannel, 128, True);
    // ������������� ����� ���������� � ����� ������
    Db := TAdoConnection.Create(Application);
    Db.ConnectionString := string(ConnectionStr);
    Db.LoginPrompt := False;
    Db.KeepConnection := True;
    Db.Tag := ArmTag;
    Db.Open;
    Result := Db.Connected;
    // ���� ���������� �����������...
    if Result then
    begin
      // �������������� ��������� ��������
      try
        InitDbLog(Db, EmptyStr, Db.Tag, User, False);
      except
        on E: Exception do
          HandleExcept(E, nil, SErrInitDbLog);
      end;
      // ��������� ������� ��������� ������������ �� ���� ������
      ReadPwdSettings(Db);
    end;
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

{ == ���������� ���������� ===================================================== }
procedure DoneDllProc(Reason: Integer);
begin
  try
    if Reason = DLL_PROCESS_DETACH then
    begin
      try
        // ���� ���� ���������� � ����� ������, ��������� ���
        if Assigned(Db) then
        begin
          // ��������� DbLog
          CloseDbLog;
          // ��������� � ���������� ����������
          if Db.Connected then Db.Close;
          Db.Free;
        end;
        Db := nil;
      finally
        // ���������� �������� ����� ����������
        Application.OnException := nil;
        // Application.Handle := DllApp;
        // DllApp := 0;
      end;
    end;
  except
    on E: Exception do
      ErrorBox(Format(EDllError, [E.Message, E.ClassName]));
  end;
end;

{ == ����������� ������������ � ������� ======================================== }
procedure LoginUser(var User: TUserRights);
begin
  if Assigned(Db) and Db.Connected
  then RssLoginUser(Db, User)
  else ErrorBox(SErrNotDbConnect);
end;

{ == ����� ������ ������������ ================================================= }
procedure ChangePassword(var User: TUserRights);
begin
  if Assigned(Db) and Db.Connected
  then RssChangePassword(Db, User, True)
  else ErrorBox(SErrNotDbConnect);
end;

exports
  InitDll name 'InitDll',
  LoginUser name 'LoginUser',
  ChangePassword name 'ChangePassword';

begin
  DllProc := @DoneDllProc;
end.
