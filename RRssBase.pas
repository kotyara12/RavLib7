unit RRssBase;

interface

uses
  Classes, SysUtils, Controls, AdoDb,
  RUserRights;

type
  EChangePwdException = class (Exception);

const
  sidChangePwdPeriod      = 9003;
  sidMinPwdLength         = 9004;

  DefChangePwdPeriod      = 40;
  DefMinPwdLength         = 6;

  SDefaultPwd             = '';

  MaxPwdErrors            = 10;

  PwdKey                  = 'AD84F9509C604052A8B955241EEF9E79';

const
  fnPASSWORD              = 'password';
  fnERRPWDCOUNT           = 'count_ep';

var
  MinPwdLength: Integer;
  ChangePwdPeriod: Integer;

{ == ����������� ������������ � ������� ======================================== }
procedure Rss_LoginUser(Db: TAdoConnection; const AppTag: Integer; var User: TUserRights);
{ == �������� ������������ ����� ������� � ������ ������ ======================= }
function  Rss_CheckNewPassword(const OriginPwd, CurrentPwd, NewPwd, ConfPwd: string): Boolean;
{ == �������� ������������� �������������� ����� ������ ======================== }
function  Rss_CheckChangePasswordNow(const CurrPwd: string; const LastChange: TDate): Boolean;
{ == ����� ������ ������������� ================================================ }
procedure Rss_ChangePassword(Db: TAdoConnection; const AppTag: Integer; var User: TUserRights);

implementation

uses
  DateUtils,
  RDialogs,
  RRssConst,
  RDbSettings,
  LoginRssForm,
  ChPwdRssForm;

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

{ == �������� ���������� ������������ ========================================== }
procedure Rss_ReadPwdSettings(Db: TAdoConnection);
begin
  MinPwdLength := ReadDbSysInteger(Db, sidMinPwdLength, DefMinPwdLength);
  ChangePwdPeriod := ReadDbSysInteger(Db, sidChangePwdPeriod, DefChangePwdPeriod);
end;

{ == ����������� ������������ � ������� ======================================== }
procedure Rss_LoginUser(Db: TAdoConnection; const AppTag: Integer; var User: TUserRights);
begin
  Rss_ReadPwdSettings(Db);
  LoginUserRss(Db, AppTag, User);
end;

{ == �������� ������������ ����� ������� � ������ ������ ======================= }
function Rss_CheckNewPassword(const OriginPwd, CurrentPwd, NewPwd, ConfPwd: string): Boolean;
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
function Rss_CheckChangePasswordNow(const CurrPwd: string; const LastChange: TDate): Boolean;
begin
  Result := (CurrPwd = SDefaultPwd) or (Length(CurrPwd) < MinPwdLength) or
    ((ChangePwdPeriod > 0) and (IncDay(LastChange, ChangePwdPeriod) <= Date));
end;

{ == ����� ������ ������������� ================================================ }
procedure Rss_ChangePassword(Db: TAdoConnection; const AppTag: Integer; var User: TUserRights);
begin
  Rss_ReadPwdSettings(Db);
  ChangePasswordRss(Db, User, True);
end;

initialization
  MinPwdLength := DefMinPwdLength;
  ChangePwdPeriod := DefChangePwdPeriod;

end.
