unit RRssConst;

interface

uses
  Db, rDbData;

resourcestring
  SMsgInitDbLog           = '������������� ���������� �������...';
  SMsgUserRegistration    = '����������� ������������ � �������...';
  SMsgUserChangePassword  = '��������� ������ ������� � �������...';
  SMsgLoadUsers           = '�������� ����������� ������������� �������...';
  SMsgInitUserRights      = '������������� ���������� ������������...';
  SMsgInitMessages        = '������������� ���������� ���������...';
  SMsgReadMail            = '�������� � ������ ���������...';
  SMsgSendMail            = '�������� ���������...';
  SMsgViewMail            = '�������� ������ ���������...';

  SErrInitDbLog           = '������ ������������� ���������� �������!';
  SErrCheckDbVersion      = '������ �������� ������ ���� ������!';
  SErrAutoRegistration    = '������ �������� ���������� ��������������� �������!';
  SErrUserRegistration    = '������ ����������� ������������ � �������!';
  SErrUserChangePassword  = '������ ��������� ������ ������������!';
  SErrLoadUsers           = '������ �������� ����������� ������������� �������!';
  SErrInitUserRights      = '������ ������������� ���������� ������������!';
  SErrInitMessages        = '������ ������������� ���������� ���������!';
  EBadOperationTag        = '������ RSS! ������������ ��� ��������: %d.';
  SErrReadMail            = '������ �������� ��������� ���������!';
  SErrSendMail            = '������ �������� ���������!';
  SErrViewMail            = '������ ��������� ������ ���������!';
  SErrUserNotRights       = '����������� ��������� ���������� ��� ���������� ��������.';

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

  SLogCloseProgram        = '������������ "%s" ����� �� %s.';
  SLogOpenWindow          = '������� ���� "%s".';
  SLogCloseWindow         = '������� ���� "%s".';

  SLogCustom              = '"%s" [%s]: %s.';
  SLogNewRecord           = '"%s" [%s]: ������� ����� ������: %s.';
  SLogEditRecord          = '"%s" [%s]: �������� ������: %s. ������� ��������: %s.';
  SLogEditRecordEmpty     = '"%s" [%s]: �������� ������: %s.';
  SLogDeleteRecord        = '"%s" [%s]: ������� ������: %s.';
  SLogExportExcel         = '"%s" [%s]: ������� ������ � Microsoft Excel.';
  SLogExportFile          = '"%s" [%s]: ������� ������ � CSV ���� "%s". ����� ��������� %d �������.';
  SLogSetSystemParameter  = '"��������� �������" [ss_settings]: �������� �������� ��������� ������� ID="%d". ����� ��������: VALUE="%s".';

  SLogReportOpen          = '"������" [ss_reports]: ������ ������ �������� ������� ��� ���� FORM="%s".';
  SLogReportClose         = '"������" [ss_reports]: ������ ������ �������� ������� ��� ���� FORM="%s".';
  SLogReportInsert        = '"������" [ss_reports]: ������ ����� ������ ������ ��� ���� FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportEdit          = '"������" [ss_reports]: �������� ��������� ������� ������ ��� ���� FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportDesign        = '"������" [ss_reports]: ������ ������ ��� ���� FORM="%s" ������ � ���������: ID="%d", NAME="%s", NOTES="%s".';
  SLogReportShow          = '"������" [ss_reports]: ����������� ����� ��� ���� FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportSave          = '"������" [ss_reports]: �������� ������ ������ ��� ���� FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportDelete        = '"������" [ss_reports]: ������ ������ ������ ��� ���� FORM="%s": ID="%d", NAME="%s", NOTES="%s".';
  SLogReportImport        = '"������" [ss_reports]: ������ ������� ������ ��� ���� FORM="%s" �� �����: ID="%d", NAME="%s", NOTES="%s".';
  SLogReportExport        = '"������" [ss_reports]: ������� ������� ������ ��� ���� FORM="%s" � ����: ID="%d", NAME="%s", NOTES="%s".';

  SFmtUserName            = '%s: %s';

{ -- ���������� ---------------------------------------------------------------- }
  {$IFDEF AUTOSTART}
  SLogAutoRegistration   = '�������������� ����������� ������������ "%s" � "%s".';
  SMsgAutoRegistration   = '�������������� ����������� ������������ � �������...';
  {$ENDIF}

const
{ -- ���� �������� ------------------------------------------------------------- }
  tagReadSms              = 9980;
  tagSendSms              = 9981;

  tagRegistration         = 9990;
  tagChPassword           = 9991;
  tagErrRegistration      = 9999;

  tagEditReports          = 8201;


{ -- ��������� ������� --------------------------------------------------------- }
  sidSecurityMin          = 9000;
  sidAutoStart_Login      = 9050;
  sidAutoStart_Enabled    = 9051;
  sidAutoStart_Minimize   = 9052;

{ -- ����� ���������� SQL �������� --------------------------------------------- }
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

{ -- ������� ��� ����������� ������������ -------------------------------------- }
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

{ -- ���������� ---------------------------------------------------------------- }
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
