unit LoginProcs;

interface

uses
  SysUtils, Controls;

type
  EChangePwdException = class (Exception);

resourcestring
  SErrLoadUserData        = '������ �������� ������ ������������!';
  SErrSaveUserData        = '������ ���������� ������ ������������!';
  SErrRegisterUser        = '������ ������ ��������� �����������!';
  SErrChangeUserPwd       = '������ ����� ������!';
  SErrSelectUserData      = '������ �������� ������ ������������ (ID=%d)!';
  SErrFindUser            = '������ �������� ������ ������������!';
  SErrStoreUserData       = '������ ���������� ������ ������������ (ID=%d)!';
  SErrUserNotEnter        = '�� ������� ��� ������������ �������!';
  SErrBadPassword         = '������ ������� �������!';
  SErrBadPasswordBkl      = '������ ������� �������! ������� ������ �����������. ���������� � �������������� �������.';
  SErrUserDeleted         = '������� ������ "%s" ���� ������� �� �������! ������ ��������!';
  SErrUserBlocked         = '������� ������ "%s" �����������! ���������� � �������������� �������.';
  SErrUserNotFound        = '������������ � ������� ������� "%s" � ������� �� ���������������!';
  SErrUserBadArm          = '������ � [%s] ��� ������� ������� "%s" ��������!';

  SMsgEnterUsedId         = '������� ����� (��� ������� ������) � ������� Enter...';
  SMsgEnterPassword       = '������� ������ � ������� Enter...';
  SMsgSelectUserPwd       = '�������� ������ ������������...';
  SMsgStoreUserPwd        = '���������� ������ ������������...';
  SMsgFindUser            = '����� ������� ������ ������������ �������...';
  SMsgCheckPwd            = '�������� ������';
  SMsgChangePassword      = '���������� ������� ������!';
  SMsgPwdChanged          = '������ ������� �������!';

  SBadOldPassword         = '������ �������� ������� ������!';
  SBadCnfPassword         = '������������� ������ �� ��������� � ��������� �������!';
  SBadLenPassword         = '������������ ����� ������!'#13'����������� ����� ������ %d ��������.';
  SBadDefPassword         = '��������� ������ �� ������������� ����������� ������������!';
  SBadNewPassword         = '��������� ������ ��������� � ������� �������!';
  SBadRetryChars          = '� ��������� ������ ���������� ����������� �������!';
  SBadSerialChars         = '� ��������� ������ ���������� ���������������� �������!';

  SFmtUserInfo            = '"%s", �������: %d �� %d';
  SFmtCanLogin            = '"%s": ������ ��������!';

  SLogSucsRegistration    = '����������� ������������ "%s" � %s ������� ���������.';
  SLogErrUserRegistration = '������� ����������� � %s ��� �������������� ������� ������� "%s".';
  SLogDelUserRegistration = '������� ����������� � %s ��� ��������� ������� ������� "%s".';
  SLogBlkUserRegistration = '������� ����������� � %s ��� ������������� ������� ������� "%s".';
  SLogWpcUserRegistration = '������� �������������������� ����� � %s ��� ������� ������� "%s".';
  SLogBadPassword         = '������� ����������� � %s ��� ������� ������� "%s" - ������ �������� ������.';
  SLogChangePassword      = '����� ������ �������������. ������� ������: "%s".';
  SLogBlockPassword       = '������� ������ "%s" ����������� - �������� ������ ������ ����������� ���������� ����� ���.';

const
  tagChPassword           = 9991;
  tagErrRegistration      = 9999;

  MaxPwdErrors            = 3;

  PwdKey                  = 'AD84F9509C604052A8B955241EEF9E79';

const
  fnPASSWORD              = 'PASSWORD';
  fnERRPWDCOUNT           = 'COUNT_EP';

  sqlGetUserData          = 'select ID, NAME, FULLNAME, PASSWORD, DELETED, BLOCKED, CHANGED, COUNT_EP' +
                            ' from SU_USERS where NAME=''%s''';
  sqlUserWorkplases       = 'select SR_WORKPLASES.ID, SR_WORKPLASES.NAME_S, SR_WORKPLASES.NAME ' +
                            'from SR_WORKPLASES where SR_WORKPLASES.ID in ' +
                            '(select SR_OPWLINKS.ID_WORKPLASES from SR_OPWLINKS ' +
                            'where SR_OPWLINKS.ID_OPERATIONS in (%s) or SR_OPWLINKS.ID_OPERATIONS in (%s)) ' +
                            'order by SR_WORKPLASES.ID';
  sqlFixOpULinks          = 'select SU_OPRLINKS.ID_OPERATIONS from SU_OPRLINKS where SU_OPRLINKS.ID_USERS=%d';
  sqlOpGroupsOpULinks     = 'select SR_OPGLINKS.ID_OPERATIONS from SR_OPGLINKS where SR_OPGLINKS.ID_OPGROUPS in ' +
                            '(select SU_OPGLINKS.ID_OPGROUPS from SU_OPGLINKS where SU_OPGLINKS.ID_USERS=%d)';
  sqlSetErrPwsCount       = 'update SU_USERS set COUNT_EP=%d where ID=%d';
  sqlSetBlocked           = 'update SU_USERS set BLOCKED=1 where ID=%d';
  
  sqlReadUserPwd          = 'select ID, PASSWORD, BLOCKED, COUNT_EP from SU_USERS where ID=%d';
  sqlSaveUserPwd          = 'update SU_USERS set PASSWORD=''%s'', COUNT_EP=0, CHANGED=GETDATE() where ID=%d';

{ == �������� ������������ ����� ������� � ������ ������ ======================= }
function  CheckNewPassword(const OriginPwd, CurrentPwd, NewPwd, ConfPwd: string): Boolean;
{ == �������� ������������� �������������� ����� ������ ======================== }
function  CheckChangePasswordNow(const CurrPwd: string; const LastChange: TDate): Boolean;

implementation

uses
  RRssBase, RDialogs, DateUtils;

const
  sStrChars = 'abcdefghijklmnopqrstuvwxyz��������������������������������';
  sQweChars = 'qwertyuiopasdfghjklzxcvbnm���������������������������������';
  sNumChars = '0123456789';
  sSysChars = '`~!@#$%^&*()_|-=\,./<>?:";''';

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

{ == �������� ������������ ����� ������� � ������ ������ ======================= }
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
    if AnsiSameStr(NewPwd, SDefaultPwd) then
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

{ == �������� ������������� �������������� ����� ������ ======================== }
function CheckChangePasswordNow(const CurrPwd: string; const LastChange: TDate): Boolean;
begin
  Result := (CurrPwd = SDefaultPwd) or (Length(CurrPwd) < MinPwdLength) or
    ((ChangePwdPeriod > 0) and (IncDay(LastChange, ChangePwdPeriod) <= Date));
end;

end.
