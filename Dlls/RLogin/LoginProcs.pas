unit LoginProcs;

interface

uses
  SysUtils, Controls;

type
  EChangePwdException = class (Exception);

resourcestring
  SErrLoadUserData        = 'Ошибка загрузки данных пользователя!';
  SErrSaveUserData        = 'Ошибка сохранения данных пользователя!';
  SErrRegisterUser        = 'Ощибка вызова процедуры регистрации!';
  SErrChangeUserPwd       = 'Ощибка смены пароля!';
  SErrSelectUserData      = 'Ошибка загрузки данных пользователя (ID=%d)!';
  SErrFindUser            = 'Ошибка загрузки данных пользователя!';
  SErrStoreUserData       = 'Ошибка сохранения данных пользователя (ID=%d)!';
  SErrUserNotEnter        = 'Не указано имя пользователя системы!';
  SErrBadPassword         = 'Пароль доступа неверен!';
  SErrBadPasswordBkl      = 'Пароль доступа неверен! Учетная запись блокирована. Обратитесь к администратору системы.';
  SErrUserDeleted         = 'Учетная запись "%s" была удалена из системы! Доступ запрещен!';
  SErrUserBlocked         = 'Учетная запись "%s" блокирована! Обратитесь к администратору системы.';
  SErrUserNotFound        = 'Пользователь с учетной записью "%s" в системе не зарегистрирован!';
  SErrUserBadArm          = 'Доступ в [%s] под учетной записью "%s" запрещен!';

  SMsgEnterUsedId         = 'Введите логин (имя учетной записи) и нажмите Enter...';
  SMsgEnterPassword       = 'Введите пароль и нажмите Enter...';
  SMsgSelectUserPwd       = 'Загрузка данных пользователя...';
  SMsgStoreUserPwd        = 'Сохранение данных пользователя...';
  SMsgFindUser            = 'Поиск учетной записи пользователя системы...';
  SMsgCheckPwd            = 'Проверка пароля';
  SMsgChangePassword      = 'Необходимо сменить пароль!';
  SMsgPwdChanged          = 'Пароль успешно изменен!';

  SBadOldPassword         = 'Введен неверный текущий пароль!';
  SBadCnfPassword         = 'Подтверждение пароля не совпадает с введенным паролем!';
  SBadLenPassword         = 'Недопустимая длина пароля!'#13'Минимальная длина пароля %d символов.';
  SBadDefPassword         = 'Введенный пароль не удовлетворяет требованиям безопасности!';
  SBadNewPassword         = 'Введенный пароль совпадает с текущим паролем!';
  SBadRetryChars          = 'В введенном пароле обнаружены повторяющие символы!';
  SBadSerialChars         = 'В введенном пароле обнаружены последовательные символы!';

  SFmtUserInfo            = '"%s", попытка: %d из %d';
  SFmtCanLogin            = '"%s": доступ запрещен!';

  SLogSucsRegistration    = 'Регистрация пользователя "%s" в %s успешно выполнена.';
  SLogErrUserRegistration = 'Попытка регистрации в %s под несуществующей учетной записью "%s".';
  SLogDelUserRegistration = 'Попытка регистрации в %s под удаленной учетной записью "%s".';
  SLogBlkUserRegistration = 'Попытка регистрации в %s под блокированной учетной записью "%s".';
  SLogWpcUserRegistration = 'Попытка несанкционированного входа в %s под учетной записью "%s".';
  SLogBadPassword         = 'Попытка регистрации в %s под учетной записью "%s" - введен неверный пароль.';
  SLogChangePassword      = 'Смена пароля пользователем. Учетная запись: "%s".';
  SLogBlockPassword       = 'Учетная запись "%s" блокирована - неверный пароль введен максимально допустимое число раз.';

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

{ == Проверка правильности ввода старого и нового пароля ======================= }
function  CheckNewPassword(const OriginPwd, CurrentPwd, NewPwd, ConfPwd: string): Boolean;
{ == Проверка необходимости принудительной смены пароля ======================== }
function  CheckChangePasswordNow(const CurrPwd: string; const LastChange: TDate): Boolean;

implementation

uses
  RRssBase, RDialogs, DateUtils;

const
  sStrChars = 'abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя';
  sQweChars = 'qwertyuiopasdfghjklzxcvbnmёйцукенгшщзхъфывапролджэячсмитьбю';
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

{ == Проверка правильности ввода старого и нового пароля ======================= }
function CheckNewPassword(const OriginPwd, CurrentPwd, NewPwd, ConfPwd: string): Boolean;
begin
  Result := False;
  try
    // Сравнение введенного старого пароля с образцом
    if not AnsiSameStr(OriginPwd, CurrentPwd) then
      raise EChangePwdException.Create(SBadOldPassword);
    // Сравнение нового пароля с проверочным паролем
    if not AnsiSameStr(NewPwd, ConfPwd) then
      raise EChangePwdException.Create(SBadCnfPassword);
    // Сравнение нового пароля со старым паролем
    if AnsiSameStr(NewPwd, CurrentPwd) then
      raise EChangePwdException.Create(SBadNewPassword);
    // Сравнение нового пароля с паролем по умолчанию
    if AnsiSameStr(NewPwd, SDefaultPwd) then
      raise EChangePwdException.Create(SBadDefPassword);
    // Проверка на минимальную длину пароля
    if Length(NewPwd) < MinPwdLength then
      raise EChangePwdException.CreateFmt(SBadLenPassword, [MinPwdLength]);
    // Проверка на повторяющиеся символы
    if CheckRetryChars(NewPwd) then
      raise EChangePwdException.Create(SBadRetryChars);
    // Проверка на последовательные символы
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

{ == Проверка необходимости принудительной смены пароля ======================== }
function CheckChangePasswordNow(const CurrPwd: string; const LastChange: TDate): Boolean;
begin
  Result := (CurrPwd = SDefaultPwd) or (Length(CurrPwd) < MinPwdLength) or
    ((ChangePwdPeriod > 0) and (IncDay(LastChange, ChangePwdPeriod) <= Date));
end;

end.
