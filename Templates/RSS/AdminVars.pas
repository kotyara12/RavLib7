unit AdminVars;

interface

resourcestring
  SQryDelOpGroup         = 'Удалить группу операций "%s"?';
  SQryDelOperFromGroup   = 'Исключить операцию "%d" из группы операций "%s"?';

  SMsgUnlockUser         = 'Разблокировка пользователя...';
  SMsgLockUser           = 'Блокировка пользователя...';
  SMsgResetPwdUser       = 'Установка временного пароля доступа в систему...';
  SMsgClearSysLog        = 'Очистка системного журнала...';

  SQueryUnlockUser       = 'Разрешить пользователю "%s" (%s) доступ в систему?';
  SQueryLockUser         = 'Запретить пользователю "%s" (%s) доступ в систему?';
  SQueryResetPwdUser     = 'Установить временный пароль доступа в систему для пользователя "%s" (%s)?';
  SQuerySaveSysLog       = 'Сохранить системный журнал в файле перед очисткой?';

  SLogSaveSysLog         = 'Журнал системы [ss_syslog]: Из системного журнала работы %d записей выгружено в файл "%s"';
  SLogClearSysLog        = 'Журнал системы [ss_syslog]: Из системного журнала работы удалено %d записей с датой ранее %s г.';
  SLogUnlockUser         = '"Пользователи системы [su_users]: Пользователю "%s" (%s) разрешен доступ в систему.';
  SLogLockUser           = '"Пользователи системы [su_users]: Пользователю "%s" (%s) запрещен доступ в систему.';
  SLogResetPwdUser       = '"Пользователи системы [su_users]: Пароль доступа пользователя "%s" (%s) изменен на временный.';

  SWarningUserOpGroup    = 'Внимание! Группа "%s" назначена %d пользователям(ю).'#13 +
                           'Удаление может привести к блокировке работы следующих пользователей: %s'#13#13 +
                           'Продолжить выполнение операции удаления?';

  SErrSysLogSave         = 'Ошибка выгрузки системного журнала работы в файл';
  SErrClearSysLog        = 'Ошибка очистки системного журнала работы';
  SErrLoadLinkOpGroups   = 'Ошибка загрузки разрешенных групп доступа для пользователя id="%d"';
  SErrLoadFreeOpGroups   = 'Ошибка загрузки запрещенных групп доступа для пользователя id="%d"';
  SErrLoadLinkOperations = 'Ошибка загрузки разрешенных операций для пользователя id="%d"';
  SErrLoadFreeOperations = 'Ошибка загрузки запрещенных операций для пользователя id="%d"';
  SErrLoadEnabledOpers   = 'Ошибка загрузки списка доступных операций для пользователя id="%d"';
  SErrLoadEnabledWps     = 'Ошибка загрузки списка доступных приложений для пользователя id="%d"';
  SErrUserAddOperation   = 'Ошибка добавления операции id="%d" в список пользователя id="%d"';
  SErrUserDelOperation   = 'Ошибка удаления операции id="%d" из списка пользователя id="%d"';
  SErrUserAddOpGroup     = 'Ошибка добавления группы операций id="%d" в список пользователя id="%d"';
  SErrUserDelOpGroup     = 'Ошибка удаления группы операций id="%d" из списка пользователя id="%d"';
  SErrUserDelAllLinks    = 'Ошибка очистки списка разрешений пользователя id="%d"';
  SErrGroupAddOperation  = 'Ошибка добавления операции id="%d" в группу операций id="%d"';
  SErrGroupDelOperation  = 'Ошибка удаления операции id="%d" из группы операций id="%d"';
  SErrGroupCheckUsrLinks = 'Ошибка проверки связей группы операций id="%d", name="%s"';
  SErrGroupDelAllLinks   = 'Ошибка удаления связей группы операций id="%d"';

const
{ -- Индексы изображений ------------------------------------------------------- }
  imOpGroup              = 2;
  imOperation            = 1;
  imWp                   = 3;

{ -- Имена параметров ---------------------------------------------------------- }
{$IFDEF MSSQL}
  pnIdSysUser            = '@id_sysusers';
  pnIdOpGroups           = '@id_opgroups';
  pnIdOperations         = '@id_operations';
{$ENDIF}
{$IFDEF MYSQL}
  pnIdSysUser            = 'id_sus';
  pnIdOpGroups           = 'id_opg';
  pnIdOperations         = 'id_opr';
{$ENDIF}

implementation

end.
