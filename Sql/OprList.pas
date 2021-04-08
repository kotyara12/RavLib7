unit OprList;

interface

const
  {$IFDEF ADMIN}
  tagViewSysSettings      = 8000;        // Просмотр параметров системы
  tagEditSysSettings      = 8001;        // Изменение параметров системы
  tagViewOpGroups         = 9000;        // Просмотр списка групп прав
  tagEditOpGroups         = 9001;        // Управление списком групп прав
  tagViewUsers            = 9100;        // Просмотр списка пользователей
  tagEditUsers            = 9101;        // Управление списком пользователей
  tagBlockUsers           = 9102;        // Блокировка и разблокировка пользователей
  tagRepEditUsers         = 9103;        // Редактирование отчета учетных записей
  tagEditUserGroup        = 9104;        // Редактирование групп пользователей
  tagViewSecurity         = 9200;        // Просмотр политики безопасности
  tagEditSecurity         = 9201;        // Редактирование политики безопасности
  tagViewSysLog           = 9210;        // Просмотр протокола системы
  tagClearSysLog          = 9211;        // Очистка протокола системы
  tagRepEditSysLog        = 9212;        // Редактирование отчета протокола системы
  tagViewSrLevels         = 9220;        // Просмотр цветовых схем протокола
  tagEditSrLevels         = 9221;        // Редактирование цветовых схем протокола
  tagViewSrOperations     = 9222;        // Просмотр списка операций
  {$ENDIF}

var
  {$IFDEF ADMIN}
  orViewOperation      	  : Boolean = False; // Доступ к просмотру свойств операций
  orViewOpGroups       	  : Boolean = False; // Доступ к просмотру свойств групп операций
  orEditOpGroups       	  : Boolean = False; // Доступ к изменению свойств групп операций
  orViewUsers          	  : Boolean = False; // Доступ к просмотру свойств пользователей
  orEditUsers          	  : Boolean = False; // Доступ к изменению свойств пользователей
  orBlockUsers         	  : Boolean = False; // Доступ к блокировке и разблокировке пользователй
  orRepEditUsers       	  : Boolean = False; // Доступ к редактированию отчета свойств пользователей
  orEditUserGroup      	  : Boolean = False; // Доступ к редактированию групп пользователей
  orViewSecurity       	  : Boolean = False; // Доступ к просмотру параметров безопасности
  orEditSecurity       	  : Boolean = False; // Доступ к изменению параметров безопасности
  orViewSysLog         	  : Boolean = False; // Доступ к просмотру протокола системы
  orClearSysLog        	  : Boolean = False; // Доступ к очистке протокола системы
  orRepEditSysLog      	  : Boolean = False; // Доступ к редактированию отчета протокола системы
  orViewSysSettings    	  : Boolean = False; // Доступ к просмотру параметров системы
  orEditSysSettings    	  : Boolean = False; // Доступ к редактированию параметров системы
  orViewSrLevels       	  : Boolean = False; // Доступ к просмотру цветовых схем
  orEditSrLevels       	  : Boolean = False; // Доступ к редактированию цветовых схем
  {$ENDIF}

implementation

end.
