unit RMsgRu;

interface

uses
  Classes;

resourcestring
  SMsgInitApplication    = 'Инициализация приложения...';
  SMsgInitDataForm       = 'Инициализация окна данных...';
  SMsgDoneApplication    = 'Завершение работы приложения...';
  SMsgConnDatabase       = 'Установка соединения с базой данных...';
  SMsgReconnDatabase     = 'Восстановление соединения с базой данных...';
  SMsgCloseDatabase      = 'Завершение соединения с базой данных...';
  SMsgConfigureConnect   = 'Не настроены параметры подключения к базе данных!'#13'Выберите меню ''Файл'' - ''Настройка соединения с БД'' и установите параметры подключения.';
  SMsgDbChangeNextTime   = 'Параметры соединения будут изменены при следующей загрузке программы!';
  SMsgSetStyles          = 'Установка темы оформления...';
  SMsgChangeStyles       = 'Настройка темы оформления...';
  SMsgCheckDbVersion     = 'Проверка версии базы данных...';
  SMsgUpdateCreate       = 'Создание новой базы данных...';
  SMsgUpdateDatabase     = 'Обновление базы данных...';
  SMsgUpdateDbStep       = 'Обновление базы данных #%d...';
  SMsgSaveDbVersion      = 'Обновление версии базы данных...';
  SMsgLoadSystemParams   = 'Загрузка параметров системы...';
  SMsgLoadData           = 'Загрузка данных...';
  SMsgLoadDataWait       = 'Загрузка данных. Подождите пожалуйста...';
  SMsgLoadDataServer     = 'Загрузка данных с сервера...';
  SMsgLoadDataFile       = 'Загрузка данных из файла...';
  SMsgLoadDataFormEx     = 'Загрузка параметров %s...';
  SMsgScanDirectory      = 'Сканирование каталога...';
  SMsgSaveData           = 'Сохранение данных...';
  SMsgSaveDataWait       = 'Сохранение данных. Подождите пожалуйста...';
  SMsgSaveDataServer     = 'Сохранение данных на сервере...';
  SMsgSaveDataFile       = 'Сохранение данных в файле...';
  SMsgSaveDataForm       = 'Сохранение параметров окна...';
  SMsgSaveDataFormEx     = 'Сохранение параметров %s...';
  SMsgPrintingWait       = 'Идет печать...';
  SMsgImportDataWait     = 'Импорт данных. Подождите пожалуйста...';
  SMsgExportDataWait     = 'Экcпорт данных. Подождите пожалуйста...';
  SMsgDeleteDataWait     = 'Удаление данных. Подождите пожалуйста...';
  SMsgExportDataName     = 'Экспорт данных для "%s"...';
  SMsgCancelChanges      = 'Отмена внесенных изменений...';
  SMsgWorkingWait        = 'Подождите пожалуйста...';
  SMsgWait               = 'Подождите пожалуйста...';
  SMsgGetSelection       = 'Позиционирование...';
  SMsgSetSelection       = 'Подождите пожалуйста...';
  SMsgFindData           = 'Поиск данных...';
  SMsgFindFiles          = 'Поиск файлов...';
  SMsgReplaceData        = 'Замена данных...';
  SMsgSortData           = 'Сортировка данных...';
  SMsgCreateNewRecord    = 'Создание новой записи...';
  SMsgGenerateReport     = 'Генерация отчета...';
  SMsgGeneratePage       = 'Генерация страницы %d из %d...';
  SMsgGenerateSheet      = 'Генерация листа "%s"...';
  SMsgPrepareOperation   = 'Подготовка операции...';
  SMsgOperationComplete  = 'Операция выполнена успешно!';
  SMsgRepaceCount        = 'Всего произведено %d замен!';
  SMsgRefreshData        = 'Обновление данных...';
  SMsgCloneSubData       = 'Копирование вложенных данных...';

  SPrmFind               = 'поиска';
  SPrmFilter             = 'фильтра';
  SPrmOrder              = 'сортировки';
  SPrmGridTuner          = 'таблицы';

  STextFilterSelected    = 'По выделению';    

  SWrnNotEnoughRights    = 'Недостаточно прав для выполнения операции!';

  SQueryCloseProgram     = 'Завершить работу с программой?';
  SQueryBreakOperation   = 'Прервать текущую операцию?';
  SQueryResetColumns     = 'Удалить пользовательские настройки столбцов таблицы?';
  SQueryVisibleFields    = 'Вывести содержимое всех стролбцов таблицы, включая невидимые?';
  SQueryCreateNewDoc     = 'Создать новый документ?';
  SQuerySaveChanges      = 'Изменения не сохранены! Сохранить внесенные изменения?';
  SQueryDescardChanges   = 'Все внесенные изменения будут потеряны! Продолжить?';
  SQuerySetDefault       = 'Вы действительно хотите установить значения "по умолчанию"?';
  SQueryCloseConnect     = 'Для выполнения запрошенной операции необходимо закрыть все окна и соединение с базой данных. Продолжить?';
  SQueryReconnect        = 'Выполнить переподключение к базе данных с новыми параметрами?';

  SSelectDirectory       = 'Выберите каталог';
  SSelectFile            = 'Выберите файл';

  SDataSetNil            = 'Набор данных не найден';
  SDataSetInactive	     = 'Набор данных закрыт';    // Dataset is closed, so its data is unavailable.
  SDataSetBrowse	       = 'Просмотр';               // Data can be viewed, but not changed. This is the default state of an open dataset.
  SDataSetEdit	         = 'Редактирование';         // Active record can be modified.
  SDataSetInsert	       = 'Новая запись';           // The active record is a newly inserted buffer that has not been posted. This record can be modified and then either posted or discarded.
  SDataSetSetKey	       = 'Фильтр...';              // TTable and TClientDataSet only. Record searching is enabled, or a SetRange operation is under way. A restricted set of data can be viewed, and no data can be edited or inserted.
  SDataSetCalcFields	   = 'Генерация значений...';  // An OnCalcFields event is in progress. Noncalculated fields cannot be edited, and new records cannot be inserted.
  SDataSetFilter	       = 'Фильтр...';              // An OnFilterRecord event is in progress. A restricted set of data can be viewed. No data can edited or inserted.
  SDataSetNewValue	     = 'Редактирование...';      // Temporary state used internally when a field component’s NewValue property is accessed.
  SDataSetOldValue	     = 'Редактирование...';      // Temporary state used internally when a field component’s OldValue property is accessed.
  SDataSetCurValue	     = 'Редактирование...';      // Temporary state used internally when a field component’s CurValue property is accessed.
  SDataSetBlockRead	     = 'Чтение данных...';       // Data-aware controls are not updated and events are not triggered when moving to the next record.
  SDataSetInternalCalc	 = 'Генерация значений...';  // Temporary state used internally when values need to be calculated for a field that has a FieldKind of fkInternalCalc.
  SDataSetOpening	       = 'Загрузка данных...';     // DataSet is in the process of opening but has not finished. This state occurs when the dataset is opened for asynchronous fetching.

  SFieldRequired         = 'Поле ''%s'' должно быть заполнено!';
  SFieldRefEmpty         = 'Для поля ''%s'' не настроен справочник значений!';
  SFieldNotListed        = 'Поле ''%s'' не подключено к списку значений!';

  SSortTreeNone          = 'Элементы не упорядочены';
  SSortTreeId            = 'Элементы упорядочены по идентификатору записи';
  SSortTreeTypeId        = 'Элементы упорядочены по типу и идентификатору';
  SSortTreeName          = 'Элементы упорядочены по наименованию записи';
  SSortTreeTypeName      = 'Элементы упорядочены по типу и наименованию';

  SLogDbVersionWarning   = '%s: не совпадают версии базы данных (DB ver: %d) и программного обеспечения (DB ver: %d)!';
  SLogDbVersionUpdate    = '%s: выполненено обновление базы данных, предназначенное для DB ver: %d из файла "%s".';
  SLogDbVersionSaveNum   = '%s: изменена версия базы данных (DB ver: %d).';

  SDbVersionFilter       = 'Файл сценария обновления #%0:d|%1:s';
  SDbVersionSelFile      = 'Укажите файл сценария обновления';
  SDbVersionScrNotFound  = 'Не найден файл сценария обновления "%s"!';
  SDbVersionSqlText      = '> %s';
  SDbVersionSqlOk        = 'OK'#13#10;
  SDbVersionSqlError     = '#ERROR#: %s';

  SDbVersionChkError     = 'Не удалось проверить соответствие версии базы данных!';
  SDbVersionQryError     = 'Продолжить выполнение сценария обновления?';

  SDbVersionWrnNewer     = 'Номер версии базы данных (DB ver: %d) выше версии ПО (DB ver: %d)!'#13#13 +
                           'Необходимо выполнить обновление программного обеспечения.'#13#13 +
                           'Вы хотите продолжить работу с данной версией программного обеспечения?';
  SDbVersionWrnOlder     = 'Номер версии базы данных (DB ver: %d) ниже версии ПО (DB ver: %d)!'#13#13 +
                           'Необходимо выполнить обновление базы данных.'#13#13 +
                           'Вы хотите продолжить работу с данной версией программного обеспечения?';
  SDbVersionQryCreate    = 'Версия базы данных не идентифицирована.'#13#13 +
                           'Выполнить сценарий создания новой базы данных?';
  SDbVersionQryUpdate    = 'Номер версии базы данных (DB ver: %d) ниже версии ПО (DB ver: %d)!'#13#13 +
                           'Выполнить обновление базы данных сейчас?';
  SDbVersionWrnCancel    = 'Не удалось выполнить обновление базы данных!'#13#13 +
                           'Вы хотите продолжить работу с данной версией программного обеспечения?';

  SDbVersionQryResque    = 'Создать резервную копию базы данных перед выполнением обновления (рекомендуется)?';
  SDbVersionWrnResque    = 'Перед началом обновления настоятельно рекомендуется создать резервную копии текущей версии базы данных (средствами СУБД)!'#13 +
                           'Выполнить обновление базы данных сейчас?';


  SErrAssertError        = 'Внутренняя ошибка!!! Обратитесь к разработчику!';
  SErrLoadLibrary        = 'Ошибка загрузки динамической библиотеки ''%s''!';
  SErrLoadLibraryEx      = 'Ошибка загрузки динамической библиотеки ''%s'':'#13'%s!';
  SErrFindProcedure      = 'Процедура ''%s'' не найдена в динамической библиотеке ''%s''!';
  SErrWindowsError       = 'Системная ошибка!';
  SErrWindowsCode        = 'Код ошибки: %d!';
  SErrSystemError        = 'Ошибка #%d: "%s"!';
  SErrBadConnectionType  = 'Класс соединения с базой данных ''%s'' не соответствует классу набора данных ''%s''!';
  SErrDataSetNull        = 'Объект данных не указан или не инициализирован (''null'')!';
  SErrInitForm           = 'Ошибка инициализации окна!';
  SErrDoneForm           = 'Ошибка выполнения заключительных операций окна!';
  SErrLoadFormPlacement  = 'Ошибка загрузки и восстановления параметров окна!';
  SErrSaveFormPlacement  = 'Ошибка сохранения параметров окна!';
  SErrChangeStyle        = 'Ошибка изменения стилей приложения!';
  EErrBadOperationTag    = 'Некорректный тэг операции: %d.';
  SErrSetStyles          = 'Ошибка установки темы оформления!';
  EDataModuleNotCreated  = 'Модуль соединения с базой данных не инициализирован!';
  SErrNotDbConnect       = 'Соединение с базой данных не установлено!';
  SErrBadDbCfgFile       = 'Файл конфигурации подключения к базе данных "%s" не найден!'#13'Выполните настройки подключения к базе данных черем меню "Файл" - "Настройка соединения с БД".';
  SErrConnDatabase       = 'Ошибка соединения с базой данных!';
  sErrReconnDatabase     = 'Не удалось восстановить подключение к базе данных за отведенное время!'#13'Программа будет закрыта.';
  SErrConfDatabase       = 'Ошибка изменения конфигурации базы данных!';
  SErrCopyDatabase       = 'Ошибка создания резервной копии базы данных!';
  SErrRestDatabase       = 'Ошибка восстановления базы данных из резервной копии!';
  SErrBackupDisabled     = 'Резервное копирование базы данных не настроено!';
  SErrCheckDbVersion     = 'Ошибка проверки версии базы данных!';
  SErrReadDbVersion      = 'Ошибка чтения версии базы данных!';
  SErrSaveDbVersion      = 'Ошибка сохранения версии базы данных!';
  SErrUpdateDatabase     = 'Ошибка обновления базы данных!';
  SErrLoadSystemParams   = 'Ошибка загрузки параметров системы из базы данных!';
  SErrOpenDataSet        = 'Ошибка загрузки данных из базы данных: объект ''%s''!';
  SErrRefreshDataSet     = 'Ошибка обновления таблицы из базы данных: объект ''%s''!';
  SErrPostError          = 'Ошибка сохранения изменений в базе данных: объект ''%s''!';
  SErrDeleteError        = 'Ошибка удаления записи в базе данных: объект ''%s''!'#13'Возможно эта ошибка возникла из-за того, что в других таблицах существуют записи, ссылающиеся на данную запись.';
  SErrGetRecordCount     = 'Ошибка определения количества записей в таблице ''%s''!';
  SErrLoadData           = 'Ошибка загрузки данных!';
  SErrLoadTree           = 'Ошибка загрузки структуры данных!';
  SErrReloadTree         = 'Ошибка перезагрузки вложенной структуры для выделенного элменета!';
  SErrRecordInsert       = 'Ошибка добавления записи!';
  SErrRecordImport       = 'Ошибка импорта записи!';
  SErrRecordEdit         = 'Ошибка редактирования записи!';
  SErrRecordDelete       = 'Ошибка удаления записи!';
  SErrRecordMultiprocess = 'Ошибка мультиобработки!';
  SErrFindError          = 'Не удалось применить фильтр для условия ''%s''!'#10#10'Попробуйте уточнить условие поиска.';
  SErrTreeDSNil          = 'Не определен набор данных для загрузки иерарихческой структуры!';
  SErrNoSelectedItem     = 'Не выбран объект для выполнения операции!';
  SErrDataSetIsEmpty     = 'Нет данных для выполнения операции!';
  SErrIdNotFound         = 'Объект с id=%d в структуре данных не найден!';
  SErrIdNotFoundS        = 'Объект с id="%s" в структуре данных не найден!';
  SErrDSIdNotFound       = 'Запись с id=%d в наборе данных "%s" не найдена!';
  SErrDSNameNotFound     = 'Запись "%s" в наборе данных "%s" не найдена!';
  SErrDSFieldNotFound    = 'Поле "%s" в наборе данных "%s" не найдено!';
  SErrStrNotFound        = 'Строка "%s" не найдена!';
  SErrFileNotFound       = 'Файл "%s" не найден!';
  SErrDirNotFound        = 'Каталог "%s" не найден!';
  SErrPathNotFound       = 'Путь "%s" не найден!';
  SErrFileDelete         = 'Ошибка удаления файла "%s": %s!';
  EErrCreateRecReport    = 'Ошибка генерация отчета для текущей записи!';
  EErrCreateReport       = 'Ошибка генерация отчета!';
  EErrCreatePage         = 'Ошибка создания страницы "%s"!';
  SErrMoveRecord         = 'Ошибка перемещения записи в выбранную группу / папку!';
  SErrInitDataModule     = 'Ошибка инициализации модуля %s!';
  SErrLoadFile           = 'Ошибка загрузки файла: "%s"!';
  SErrSaveFile           = 'Ошибка записи в файл: "%s"!';
  SErrImportFile         = 'Ошибка загрузки данных из файла: "%s"!';
  SErrExportFile         = 'Ошибка экспорта данных в файл: "%s"!';
  SErrLoadReport         = 'Ошибка загрузки отчета "%s" из базы данных!';
  SErrSaveReport         = 'Ошибка сохранения отчета "%s" в базе данных!';
  SErrGenerateReport     = 'Ошибка создания отчета!';
  SErrNotRightsForOper   = 'Недостаточно прав для выполнения операции "%s"!';
  SErrCancelDisabled     = 'Невозможно закрыть окно с отменой внесенных изменений, так как внесены изменения в подчиненные структуры!';

  SCopyrightsStr         = '%s: %s. Copyright by %s';
  SFmtNotConnected       = 'Not connected';
  SFmtConnDatabase       = 'Db: %s';
  SFmtEditorCaption      = '%s: %s';
  SViewNotEnabled        = '< СКРЫТО >';
  SRecordInactive        = 'Нет данных';
  SRecordNum             = '%d';
  SRecordCount           = '%d';
  SRecordNumCount        = '%d:%d';
  SFileNoName            = 'Без имени %d';
  SCaptionRecordNum      = 'Запись: %d';
  SCaptionRecordCount    = 'Всего записей: %d';
  SCaptionRecordNumCount = 'Запись: %d из %d';
  SCaptionState          = 'Режим: "%s"';
  SFoundFilesCount       = 'Найдено файлов: %d';
  SItemsCount            = 'Объектов: %d';
  SRecordsCount          = 'Записей: %d';
  SSortDisableName       = 'без сортировки';
  SSortDisableHint       = 'Отключить сортировку данных';
  SSortItemName          = 'по столбцу ''%s''';
  SSortItemHint          = 'Упорядочить данные по столбцу ''%s''';
  SBooleanOn             = 'Включено';
  SBooleanOff            = 'Выключено';
  SDots                  = '...';
  SAllFilesFilter        = 'Все файлы (*.*)|*.*';
  SAlignmentLeft         = 'по левому краю';
  SAlignmentRight        = 'по правому краю';
  SAlignmentCenter       = 'по центру';

const
  SAlignments: array [TAlignment] of string =
    (SAlignmentLeft, SAlignmentRight, SAlignmentCenter);

function GetNameAlignments(const iAlignment: TAlignment): string;

implementation

function GetNameAlignments(const iAlignment: TAlignment): string;
begin
  case iAlignment of
    taLeftJustify: Result := SAlignmentLeft;
    taRightJustify: Result := SAlignmentRight;
    taCenter: Result := SAlignmentCenter;
  end;
end;

end.



