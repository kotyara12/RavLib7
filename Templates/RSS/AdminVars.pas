unit AdminVars;

interface

resourcestring
  SQryDelOpGroup         = '������� ������ �������� "%s"?';
  SQryDelOperFromGroup   = '��������� �������� "%d" �� ������ �������� "%s"?';

  SMsgUnlockUser         = '������������� ������������...';
  SMsgLockUser           = '���������� ������������...';
  SMsgResetPwdUser       = '��������� ���������� ������ ������� � �������...';
  SMsgClearSysLog        = '������� ���������� �������...';

  SQueryUnlockUser       = '��������� ������������ "%s" (%s) ������ � �������?';
  SQueryLockUser         = '��������� ������������ "%s" (%s) ������ � �������?';
  SQueryResetPwdUser     = '���������� ��������� ������ ������� � ������� ��� ������������ "%s" (%s)?';
  SQuerySaveSysLog       = '��������� ��������� ������ � ����� ����� ��������?';

  SLogSaveSysLog         = '������ ������� [ss_syslog]: �� ���������� ������� ������ %d ������� ��������� � ���� "%s"';
  SLogClearSysLog        = '������ ������� [ss_syslog]: �� ���������� ������� ������ ������� %d ������� � ����� ����� %s �.';
  SLogUnlockUser         = '"������������ ������� [su_users]: ������������ "%s" (%s) �������� ������ � �������.';
  SLogLockUser           = '"������������ ������� [su_users]: ������������ "%s" (%s) �������� ������ � �������.';
  SLogResetPwdUser       = '"������������ ������� [su_users]: ������ ������� ������������ "%s" (%s) ������� �� ���������.';

  SWarningUserOpGroup    = '��������! ������ "%s" ��������� %d �������������(�).'#13 +
                           '�������� ����� �������� � ���������� ������ ��������� �������������: %s'#13#13 +
                           '���������� ���������� �������� ��������?';

  SErrSysLogSave         = '������ �������� ���������� ������� ������ � ����';
  SErrClearSysLog        = '������ ������� ���������� ������� ������';
  SErrLoadLinkOpGroups   = '������ �������� ����������� ����� ������� ��� ������������ id="%d"';
  SErrLoadFreeOpGroups   = '������ �������� ����������� ����� ������� ��� ������������ id="%d"';
  SErrLoadLinkOperations = '������ �������� ����������� �������� ��� ������������ id="%d"';
  SErrLoadFreeOperations = '������ �������� ����������� �������� ��� ������������ id="%d"';
  SErrLoadEnabledOpers   = '������ �������� ������ ��������� �������� ��� ������������ id="%d"';
  SErrLoadEnabledWps     = '������ �������� ������ ��������� ���������� ��� ������������ id="%d"';
  SErrUserAddOperation   = '������ ���������� �������� id="%d" � ������ ������������ id="%d"';
  SErrUserDelOperation   = '������ �������� �������� id="%d" �� ������ ������������ id="%d"';
  SErrUserAddOpGroup     = '������ ���������� ������ �������� id="%d" � ������ ������������ id="%d"';
  SErrUserDelOpGroup     = '������ �������� ������ �������� id="%d" �� ������ ������������ id="%d"';
  SErrUserDelAllLinks    = '������ ������� ������ ���������� ������������ id="%d"';
  SErrGroupAddOperation  = '������ ���������� �������� id="%d" � ������ �������� id="%d"';
  SErrGroupDelOperation  = '������ �������� �������� id="%d" �� ������ �������� id="%d"';
  SErrGroupCheckUsrLinks = '������ �������� ������ ������ �������� id="%d", name="%s"';
  SErrGroupDelAllLinks   = '������ �������� ������ ������ �������� id="%d"';

const
{ -- ������� ����������� ------------------------------------------------------- }
  imOpGroup              = 2;
  imOperation            = 1;
  imWp                   = 3;

{ -- ����� ���������� ---------------------------------------------------------- }
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
