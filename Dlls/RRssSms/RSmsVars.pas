unit RSmsVars;

interface

uses
  ComCtrls, RUserRights, RAdoUtils;

type
  TSmsListParams = ^RSmsListParams;
  RSmsListParams = packed record
    ArmId: Integer;
    UserId: Integer;
  end;

const
{ -- Константы ----------------------------------------------------------------- }
  chAddrDivider       = '; ';
  chAddrDividers      = [';',','];
  fmtWpPrefix         = 'WP%s';
  fmtAllPrefix        = '*';
  sidSmsActivePeriod  = 1002;

  defSmsActivePeriod  = 90;
  defSqlDateFormat    = 'mm.dd.yyyy';

{ -- Имена полей --------------------------------------------------------------- }
  fnID_USERS       = 'id_users';
  fnSENDED         = 'sended';
  fnTITLE          = 'title';
  fnADDRESS        = 'address';
  fnMESSAGE        = 'message';
  fnREADED         = 'readed';
  fnCLOSED         = 'closed';

{ -- Запросы ------------------------------------------------------------------- }
  sqlLoadSmsList   = 'SELECT id, sended, address FROM sm_messages ' +
  {$IFDEF MSSQL}
                     'WHERE (DATEADD(DAY, ISNULL((SELECT value_int FROM ss_settings WHERE id=1002), 90), sended) >= GETDATE()) ' +
  {$ENDIF}
  {$IFDEF MYSQL}
                     'WHERE (sended + INTERVAL IFNULL((SELECT value_int FROM ss_settings WHERE id=1002), 90) DAY) >= NOW()) ' +
  {$ENDIF}
                     'AND (id_users<>%0:d) ' +
                     'AND (id NOT IN (SELECT id_messages FROM sm_history WHERE id_users=%0:d AND closed is NOT NULL)) ' +
                     'ORDER BY sended';
  sqlReadSmsId     = 'SELECT id_users, name, fullname, sended, title, address, message ' +
                     'FROM sm_messages left join su_users on sm_messages.id_users=su_users.id ' +
                     'WHERE sm_messages.id=%0:d';
  sqlReadSmsListS  = 'SELECT sm_messages.id, id_users, name, fullname, sended, title, address ' +
                     'FROM sm_messages left join su_users on sm_messages.id_users=su_users.id';
  {$IFDEF MSSQL}
  sqlReadSmsListW  = '((sm_messages.id_users<>%0:d) AND ((address=''*'') OR (address LIKE ''%%WP%1:d%%'') OR (address LIKE (SELECT ''%%'' + name + ''%%'' FROM su_users WHERE id=%0:d))))';
  {$ENDIF}
  {$IFDEF MYSQL}
  sqlReadSmsListW  = '((sm_messages.id_users<>%0:d) AND ((address=''*'') OR (address LIKE ''%%WP%1:d%%'') OR (address LIKE (SELECT Concat(''%%'', name, ''%%'') FROM su_users WHERE id=%0:d))))';
  {$ENDIF}

  sqlReadSmsState  = 'SELECT id_messages, id_users, opened, closed ' +
                     'FROM sm_history WHERE id_messages=%0:d AND id_users=%1:d';
  sqlActiveUsers   = 'SELECT name, fullname, notes, blocked FROM su_users ' +
                     'WHERE id<>%d AND deleted=0 ORDER BY name';
  sqlWpsList       = 'SELECT id, name_s, name FROM sr_workplases';

resourcestring
{ -- Протокол ------------------------------------------------------------------ }
  SLogReadSms      = 'Открыто сообщение title="%s", from="%s; %s", datesend="%s".';
  SLogSendSms      = 'Создано сообщение title="%s", to="%s".';

{ -- Сообщения ----------------------------------------------------------------- }
  EErrLoadMessages = 'Ошибка загрузки списка сообщений!';
  SErrLoadSms      = 'Ошибка загрузки сообщения id="%d"!';
  SErrLoadSmsState = 'Ошибка загрузки статуса сообщения id="%d"!';
  EErrSaveSmsState = 'Ошибка сохранения статуса сообщения id="%d"!';
  SErrCreateSms    = 'Ошибка создания нового сообщения!';
  SErrLoadList     = 'Ошибка загрузки списка объектов!';
  SErrInitDll      = 'Ошибка инициализации RSMS.DLL!';

var
  UsrDll: TUserRights;
  ArmDll: Integer;
  Dp: RAdoDbParameters;
  // SmsActivePeriod: Integer;

function IsAddrPresent(const AddrName, AddrList: string; const AllEnabled: Boolean): Boolean;
function RecreateAddrList(const OldList: string; NewList: TListView): string;

implementation

uses
  SysUtils, RxStrUtils, RDialogs;

function IsAddrPresent(const AddrName, AddrList: string; const AllEnabled: Boolean): Boolean;
var
  AddrWord: string;
  i: Integer;
begin
  Result := False;
  for i := 1 to WordCount(AddrList, chAddrDividers) do
  begin
    AddrWord := Trim(ExtractWord(i, AddrList, chAddrDividers));
    Result := SameText(AddrWord, AddrName) or (AllEnabled and SameText(AddrWord, fmtAllPrefix));
    if Result then Break;
  end;
end;

function RecreateAddrList(const OldList: string; NewList: TListView): string;
var
  OldAddrWord, NewAddrWord: string;
  WordFound: Boolean;
  i, j: Integer;
begin
  Result := EmptyStr;
  for i := 1 to WordCount(OldList, chAddrDividers) do
  begin
    WordFound := False;
    OldAddrWord := Trim(ExtractWord(i, OldList, chAddrDividers));
    if (OldAddrWord <> EmptyStr) and not SameText(OldAddrWord, fmtAllPrefix) then
    begin
      for j := 0 to NewList.Items.Count - 1 do
      begin
        NewAddrWord := Trim(NewList.Items[j].Caption);
        WordFound := SameText(NewAddrWord, OldAddrWord);
        if WordFound then
        begin
          if NewList.Items[j].Checked then Result := Result + chAddrDivider + NewAddrWord;
          Break;
        end;
      end;
      if not WordFound then Result := Result + chAddrDivider + OldAddrWord;
    end;
  end;
  for j := 0 to NewList.Items.Count - 1 do
  begin
    NewAddrWord := Trim(NewList.Items[j].Caption);
    if NewList.Items[j].Checked and not IsAddrPresent(NewAddrWord, Result, False)
    then Result := Result + chAddrDivider + NewAddrWord;
  end;
  if Length(Result) > 0 then
  begin
    Result := Trim(Result);
    Delete(Result, 1, 1);
    Result := Trim(Result);
  end;
end;

end.
