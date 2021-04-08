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
  SMsgEnterUsedId         = 'Введите логин (имя учетной записи) и нажмите Enter...';
  SMsgEnterPassword       = 'Введите пароль и нажмите Enter...';
  SMsgSelectUserPwd       = 'Загрузка данных пользователя...';
  SMsgStoreUserPwd        = 'Сохранение данных пользователя...';
  SMsgFindUser            = 'Поиск учетной записи пользователя системы...';
  SMsgCheckPwd            = 'Проверка пароля...';
  SMsgChangePassword      = 'Необходимо сменить пароль!';
  SMsgPwdChanged          = 'Пароль успешно изменен!';

  SQryClearUserPassword   = 'Установить временный пароль (11111111) ?';

  SErrLoadUserData        = 'Ошибка загрузки данных пользователя!';
  SErrSaveUserData        = 'Ошибка сохранения данных пользователя!';
  SErrFindUser            = 'Ошибка загрузки данных пользователя!';
  SErrUserNotEnter        = 'Не указано имя пользователя системы!';
  SErrBadPassword         = 'Пароль доступа неверен!';
  SErrBadPasswordBkl      = 'Пароль доступа неверен! Доступ в систему запрещен.';
  SErrUserNotFound        = 'Пользователь с учетной записью "%s" в системе не зарегистрирован!';
  SErrUserDeleted         = 'Пользователь "%s" был удален администратором системы!';

  SBadOldPassword         = 'Введен неверный текущий пароль!';
  SBadCnfPassword         = 'Подтверждение пароля не совпадает с введенным паролем!';
  SBadLenPassword         = 'Недопустимая длина пароля!'#13'Минимальная длина пароля %d символов.';
  SBadDefPassword         = 'Введенный пароль не удовлетворяет требованиям безопасности!';
  SBadNewPassword         = 'Введенный пароль совпадает с текущим паролем!';
  SBadRetryChars          = 'В введенном пароле обнаружены повторяющие символы!';
  SBadSerialChars         = 'В введенном пароле обнаружены последовательные символы!';

  SFmtUserInfo            = '"%s", попытка: %d из %d';
  SFmtCanLogin            = '"%s": доступ запрещен!';

{ == Инициализация пользователя ================================================ }
procedure InitUserData(var AUser: TUserData);
{ == Проверка правильности ввода старого и нового пароля ======================= }
function  CheckNewPassword(const OriginPwd, CurrentPwd, NewPwd, ConfPwd: string): Boolean;

implementation

uses
  StrUtils, RDialogs;

const
  sStrChars = 'abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя';
  sQweChars = 'qwertyuiopasdfghjklzxcvbnmёйцукенгшщзхъфывапролджэячсмитьбю';
  sNumChars = '0123456789';
  sSysChars = '`~!@#$%^&*()_|-=\,./<>?:";''';

{ == Инициализация пользователя ================================================ }
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

{ == Проверка правильности ввода старого и нового пароля ======================= }
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
    if AnsiSameStr(NewPwd, SPwdDefault) then
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

end.
