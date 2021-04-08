unit RRssConst;

interface

uses
  Db, rDbData;

resourcestring
  SMsgInitDbLog           = 'Инициализация системного журнала...';
  SMsgUserRegistration    = 'Регистрация пользователя в системе...';
  SMsgUserChangePassword  = 'Изменение пароля доступа в систему...';
  SMsgLoadUsers           = 'Загрузка справочника пользователей системы...';
  SMsgInitUserRights      = 'Инициализация подсистемы безопасности...';
  SMsgInitMessages        = 'Инициализация подсистемы сообщений...';
  SMsgReadMail            = 'Загрузка и чтение сообщений...';
  SMsgSendMail            = 'Создание сообщения...';
  SMsgViewMail            = 'Просмотр списка сообщений...';

  SErrInitDbLog           = 'Ошибка инициализации системного журнала!';
  SErrCheckDbVersion      = 'Ошибка контроля версии базы данных!';
  SErrAutoRegistration    = 'Ошибка загрузки параметров автоматического запуска!';
  SErrUserRegistration    = 'Ошибка регистрации пользователя в системе!';
  SErrUserChangePassword  = 'Ошибка изменения пароля пользователя!';
  SErrLoadUsers           = 'Ошибка загрузки справочника пользователей системы!';
  SErrInitUserRights      = 'Ошибка инициализации подсистемы безопасности!';
  SErrInitMessages        = 'Ошибка инициализации подсистемы сообщений!';
  EBadOperationTag        = 'Ошибка RSS! Некорректный тэг операции: %d.';
  SErrReadMail            = 'Ошибка загрузки сообщений сообщений!';
  SErrSendMail            = 'Ошибка создания сообщения!';
  SErrViewMail            = 'Ошибка просмотра списка сообщений!';
  SErrUserNotRights       = 'Отсутствуют системные привилегии для выполнения операции.';

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

  SLogCloseProgram        = 'Пользователь "%s" вышел из %s.';
  SLogOpenWindow          = 'Открыто окно "%s".';
  SLogCloseWindow         = 'Закрыто окно "%s".';

  SLogCustom              = '"%s" [%s]: %s.';
  SLogNewRecord           = '"%s" [%s]: Создана новая запись: %s.';
  SLogEditRecord          = '"%s" [%s]: Изменена запись: %s. Прежнее значение: %s.';
  SLogEditRecordEmpty     = '"%s" [%s]: Изменена запись: %s.';
  SLogDeleteRecord        = '"%s" [%s]: Удалена запись: %s.';
  SLogExportExcel         = '"%s" [%s]: Экспорт данных в Microsoft Excel.';
  SLogExportFile          = '"%s" [%s]: Экспорт данных в CSV файл "%s". Всего выгружено %d записей.';
  SLogSetSystemParameter  = '"Параметры системы" [ss_settings]: Изменено значение параметра системы ID="%d". Новое значение: VALUE="%s".';

  SLogReportOpen          = '"Отчеты" [ss_reports]: Открыт список шаблонов отчетов для окна FORM="%s".';
  SLogReportClose         = '"Отчеты" [ss_reports]: Закрыт список шаблонов отчетов для окна FORM="%s".';
  SLogReportInsert        = '"Отчеты" [ss_reports]: Создан новый шаблон отчета для окна FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportEdit          = '"Отчеты" [ss_reports]: Изменены параметры шаблона отчета для окна FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportDesign        = '"Отчеты" [ss_reports]: Шаблон отчета для окна FORM="%s" открыт в дизайнере: ID="%d", NAME="%s", NOTES="%s".';
  SLogReportShow          = '"Отчеты" [ss_reports]: Сформирован отчет для окна FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportSave          = '"Отчеты" [ss_reports]: Сохранен шаблон отчета для окна FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportDelete        = '"Отчеты" [ss_reports]: Удален шаблон отчета для окна FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportImport        = '"Отчеты" [ss_reports]: Импорт шаблона отчета для окна FORM="%s" из файла: ID="%d", NAME="%s", NOTES="%s".';
  SLogReportExport        = '"Отчеты" [ss_reports]: Экспорт шаблона отчета для окна FORM="%s" в файл: ID="%d", NAME="%s", NOTES="%s".';

  SFmtUserName            = '%s: %s';

{ -- Автозапуск ---------------------------------------------------------------- }
  {$IFDEF AUTOSTART}
  SLogAutoRegistration   = 'Автоматическая регистрация пользователя "%s" в "%s".';
  SMsgAutoRegistration   = 'Автоматическая регистрация пользователя в системе...';
  {$ENDIF}

const
{ -- Коды операций ------------------------------------------------------------- }
  tagReadSms              = 9980;
  tagSendSms              = 9981;

  tagRegistration         = 9990;
  tagChPassword           = 9991;
  tagErrRegistration      = 9999;

  tagEditReports          = 8201;


{ -- Параметры системы --------------------------------------------------------- }
  sidSecurityMin          = 9000;
  sidAutoStart_Login      = 9050;
  sidAutoStart_Enabled    = 9051;
  sidAutoStart_Minimize   = 9052;

{ -- Имена параметров SQL процедур --------------------------------------------- }
{$IFDEF MSSQL}
  pnIdWorkplases         = '@id_workplases';
  pnIdUsers              = '@id_users';
  pnHost                 = '@host';
  pnNetUser              = '@netuser';
{$ENDIF}
{$IFDEF MYSQL}
  pnIdWorkplases         = 'id_wps';
  pnIdUsers              = 'id_usr';
  pnHost                 = 'hostname';
  pnNetUser              = 'netuser';
{$ENDIF}

{ -- Запросы для регистрации пользователя -------------------------------------- }
  sqlGetUserData          = 'SELECT id, name, fullname, password, deleted, blocked, changed, count_ep FROM su_users WHERE name=''%s''';
  sqlUserWorkplases       = 'SELECT sr_workplases.id, sr_workplases.name_s, sr_workplases.name FROM sr_workplases WHERE sr_workplases.id IN ' +
                            '(SELECT sr_opwlinks.id_workplases FROM sr_opwlinks ' +
                            'WHERE sr_opwlinks.id_operations IN (%s) OR sr_opwlinks.id_operations IN (%s)) ' +
                            'ORDER BY sr_workplases.id';
  sqlFixOpULinks          = 'SELECT su_oprlinks.id_operations FROM su_oprlinks WHERE su_oprlinks.id_users=%d';
  sqlOpGroupsOpULinks     = 'SELECT sr_opglinks.id_operations FROM sr_opglinks WHERE sr_opglinks.id_opgroups IN ' +
                            '(SELECT su_opglinks.id_opgroups FROM su_opglinks WHERE su_opglinks.id_users=%d)';
  sqlSetErrPwsCount       = 'UPDATE su_users SET count_ep=%d WHERE id=%d';
  sqlSetBlocked           = 'UPDATE su_users SET blocked=1 WHERE id=%d';

  sqlReadUserPwd          = 'SELECT id, password, blocked, count_ep FROM su_users WHERE id=%d';
  {$IFDEF MSSQL}
  sqlSaveUserPwd          = 'UPDATE su_users SET password=''%s'', count_ep=0, changed=GetDate() WHERE id=%d';
  {$ENDIF}
  {$IFDEF MYSQL}
  sqlSaveUserPwd          = 'UPDATE su_users SET password=''%s'', count_ep=0, changed=Now() WHERE id=%d';
  {$ENDIF}

{ -- Автозапуск ---------------------------------------------------------------- }
  {$IFDEF AUTOSTART}
  iniAS_Section          = 'AUTOSTART';
  iniAS_Enabled          = 'Enabled';
  iniAS_Login            = 'Login';
  iniAS_Minimize         = 'Minimize';

  sqlLoadUserData        = 'SELECT id, name, fullname, deleted, blocked FROM su_users WHERE name=''%s''';
  {$ENDIF}

function rssSysLog_CreateRecord(const sTableName, sTableDescr: string; Data: TRecordData): string; overload;
function rssSysLog_UpdateRecord(const sTableName, sTableDescr: string; OldData, NewData: TRecordData): string;
function rssSysLog_DeleteRecord(const sTableName, sTableDescr: string; Data: TRecordData): string; overload;

function rssSysLog_CreateRecord(const sTableName, sTableDescr: string; DS: TDataSet): string; overload;
function rssSysLog_DeleteRecord(const sTableName, sTableDescr: string; DS: TDataSet): string; overload;

implementation

uses
  SysUtils, rDialogs;

function rssSysLog_CreateRecord(const sTableName, sTableDescr: string; Data: TRecordData): string;
begin
  Result := Format(SLogNewRecord, [sTableDescr, sTableName,
    GetRecordText(Data, [rtRequired, rtVisible, rtIndex, rtID, rtAnyTag])]);
end;

function rssSysLog_UpdateRecord(const sTableName, sTableDescr: string; OldData, NewData: TRecordData): string;
var
  sChanges: string;
begin
  sChanges := GetRecordChanges(OldData, NewData, []);
  if sChanges <> EmptyStr
  then Result := Format(SLogEditRecord,
    [sTableDescr, sTableName, GetRecordChanges(NewData, OldData, [rtRequired, rtIndex, rtID, rtAnyTag]), sChanges])
  else Result := Format(SLogEditRecordEmpty,
    [sTableDescr, sTableName, GetRecordText(NewData, [rtRequired, rtIndex, rtID, rtAnyTag])]);
end;

function rssSysLog_DeleteRecord(const sTableName, sTableDescr: string; Data: TRecordData): string;
begin
  Result := Format(SLogDeleteRecord, [sTableDescr, sTableName,
    GetRecordText(Data, [rtRequired, rtVisible, rtIndex, rtID, rtAnyTag])]);
end;

function rssSysLog_CreateRecord(const sTableName, sTableDescr: string; DS: TDataSet): string;
var
  rData: TRecordData;
begin
  rData := GetRecordData(DS);
  try
    Result := rssSysLog_DeleteRecord(sTableName, sTableDescr, rData);
  finally
    FreeRecordData(rData);
  end;
end;

function rssSysLog_DeleteRecord(const sTableName, sTableDescr: string; DS: TDataSet): string;
var
  rData: TRecordData;
begin
  rData := GetRecordData(DS);
  try
    Result := rssSysLog_DeleteRecord(sTableName, sTableDescr, rData);
  finally
    FreeRecordData(rData);
  end;
end;

end.
