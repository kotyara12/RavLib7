unit RDbLogin;

interface

uses
  SysUtils, Controls, Db, AdoDb;

type
  EChangePwdException = class (Exception);

  PUserData = ^TUserData;
  TUserData = record
    UserId: Integer;
    UserLogin: string;
    UserName: string;
    UserType: Integer;
    IsLogined: Boolean;
    IsDeleted: Boolean;
    NeedPwdChange: Boolean;
  end;

const
  MaxPwdErrors            = 3;
  MinPwdLength            = 4;
  sPwdDefault             = '11111111';

  sqlUserData             = 'SELECT id, login, name, pswd, type, deleted FROM users WHERE login=''%s''';
  sqlReadUserPwd          = 'SELECT pswd FROM users WHERE id=%d';
  sqlSaveUserPwd          = 'UPDATE users SET pswd=''%s'' WHERE id=%d';

resourcestring
  SMsgEnterUsedId         = '������� ����� (��� ������� ������) � ������� Enter...';
  SMsgEnterPassword       = '������� ������ � ������� Enter...';
  SMsgSelectUserPwd       = '�������� ������ ������������...';
  SMsgStoreUserPwd        = '���������� ������ ������������...';
  SMsgFindUser            = '����� ������� ������ ������������ �������...';
  SMsgCheckPwd            = '�������� ������...';
  SMsgChangePassword      = '���������� ������� ������!';
  SMsgPwdChanged          = '������ ������� �������!';

  SQryClearUserPassword   = '���������� ��������� ������ (11111111) ?';

  SErrLoadUserData        = '������ �������� ������ ������������!';
  SErrSaveUserData        = '������ ���������� ������ ������������!';
  SErrFindUser            = '������ �������� ������ ������������!';
  SErrUserNotEnter        = '�� ������� ��� ������������ �������!';
  SErrBadPassword         = '������ ������� �������!';
  SErrBadPasswordBkl      = '������ ������� �������! ������ � ������� ��������.';
  SErrUserNotFound        = '������������ � ������� ������� "%s" � ������� �� ���������������!';
  SErrUserDeleted         = '������������ "%s" ��� ������ ��������������� �������!';

  SBadOldPassword         = '������ �������� ������� ������!';
  SBadCnfPassword         = '������������� ������ �� ��������� � ��������� �������!';
  SBadLenPassword         = '������������ ����� ������!'#13'����������� ����� ������ %d ��������.';
  SBadDefPassword         = '��������� ������ �� ������������� ����������� ������������!';
  SBadNewPassword         = '��������� ������ ��������� � ������� �������!';
  SBadRetryChars          = '� ��������� ������ ���������� ����������� �������!';
  SBadSerialChars         = '� ��������� ������ ���������� ���������������� �������!';

  SFmtUserInfo            = '"%s", �������: %d �� %d';
  SFmtCanLogin            = '"%s": ������ ��������!';

{ == ������������� ������������ ================================================ }
procedure InitUserData(var AUser: TUserData);
{ == �������� ������������ ����� ������� � ������ ������ ======================= }
function  CheckNewPassword(const OriginPwd, CurrentPwd, NewPwd, ConfPwd: string): Boolean;

implementation

uses
  StrUtils, RDialogs;

const
  sStrChars = 'abcdefghijklmnopqrstuvwxyz��������������������������������';
  sQweChars = 'qwertyuiopasdfghjklzxcvbnm���������������������������������';
  sNumChars = '0123456789';
  sSysChars = '`~!@#$%^&*()_|-=\,./<>?:";''';

{ == ������������� ������������ ================================================ }
procedure InitUserData(var AUser: TUserData);
begin
  with AUser do
  begin
    UserId := 0;
    UserLogin := '';
    UserName := '';
    UserType := -1;
    IsLogined := False;
    IsDeleted := False;
    NeedPwdChange := False;
  end;
end;

{ == �������� ������������ ����� ������� � ������ ������ ======================= }
function CheckRetryChars(const Pws: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(Pws) - 2 do
  begin
    Result := (Pws[i] = Pws[i + 1]) and (Pws[i] = Pws[i + 2]);
    if Result then Break;
  end;
end;

function CheckSerialChars(const Pws, Chars: string; const Len: Integer): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(Chars) - Len + 1 do
  begin
    Result := Pos(Copy(Chars, i, Len), Pws) > 0;
    if Result then Break;
  end;
end;

function CheckNewPassword(const OriginPwd, CurrentPwd, NewPwd, ConfPwd: string): Boolean;
begin
  Result := False;
  try
    // ��������� ���������� ������� ������ � ��������
    if not AnsiSameStr(OriginPwd, CurrentPwd) then
      raise EChangePwdException.Create(SBadOldPassword);
    // ��������� ������ ������ � ����������� �������
    if not AnsiSameStr(NewPwd, ConfPwd) then
      raise EChangePwdException.Create(SBadCnfPassword);
    // ��������� ������ ������ �� ������ �������
    if AnsiSameStr(NewPwd, CurrentPwd) then
      raise EChangePwdException.Create(SBadNewPassword);
    // ��������� ������ ������ � ������� �� ���������
    if AnsiSameStr(NewPwd, SPwdDefault) then
      raise EChangePwdException.Create(SBadDefPassword);
    // �������� �� ����������� ����� ������
    if Length(NewPwd) < MinPwdLength then
      raise EChangePwdException.CreateFmt(SBadLenPassword, [MinPwdLength]);
    // �������� �� ������������� �������
    if CheckRetryChars(NewPwd) then
      raise EChangePwdException.Create(SBadRetryChars);
    // �������� �� ���������������� �������
    if CheckSerialChars(NewPwd, sStrChars, 3)
    or CheckSerialChars(NewPwd, sQweChars, 3)
    or CheckSerialChars(NewPwd, AnsiUpperCase(sStrChars), 3)
    or CheckSerialChars(NewPwd, AnsiUpperCase(sQweChars), 3)
    or CheckSerialChars(NewPwd, sNumChars, 3)
    or CheckSerialChars(NewPwd, sSysChars, 3) then
      raise EChangePwdException.Create(SBadSerialChars);
    Result := True;
  except
    on E: Exception do
      ErrorBox(E.Message);
  end;
end;

end.
