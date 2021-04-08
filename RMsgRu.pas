unit RMsgRu;

interface

uses
  Classes;

resourcestring
  SMsgInitApplication    = '������������� ����������...';
  SMsgInitDataForm       = '������������� ���� ������...';
  SMsgDoneApplication    = '���������� ������ ����������...';
  SMsgConnDatabase       = '��������� ���������� � ����� ������...';
  SMsgReconnDatabase     = '�������������� ���������� � ����� ������...';
  SMsgCloseDatabase      = '���������� ���������� � ����� ������...';
  SMsgConfigureConnect   = '�� ��������� ��������� ����������� � ���� ������!'#13'�������� ���� ''����'' - ''��������� ���������� � ��'' � ���������� ��������� �����������.';
  SMsgDbChangeNextTime   = '��������� ���������� ����� �������� ��� ��������� �������� ���������!';
  SMsgSetStyles          = '��������� ���� ����������...';
  SMsgChangeStyles       = '��������� ���� ����������...';
  SMsgCheckDbVersion     = '�������� ������ ���� ������...';
  SMsgUpdateCreate       = '�������� ����� ���� ������...';
  SMsgUpdateDatabase     = '���������� ���� ������...';
  SMsgUpdateDbStep       = '���������� ���� ������ #%d...';
  SMsgSaveDbVersion      = '���������� ������ ���� ������...';
  SMsgLoadSystemParams   = '�������� ���������� �������...';
  SMsgLoadData           = '�������� ������...';
  SMsgLoadDataWait       = '�������� ������. ��������� ����������...';
  SMsgLoadDataServer     = '�������� ������ � �������...';
  SMsgLoadDataFile       = '�������� ������ �� �����...';
  SMsgLoadDataFormEx     = '�������� ���������� %s...';
  SMsgScanDirectory      = '������������ ��������...';
  SMsgSaveData           = '���������� ������...';
  SMsgSaveDataWait       = '���������� ������. ��������� ����������...';
  SMsgSaveDataServer     = '���������� ������ �� �������...';
  SMsgSaveDataFile       = '���������� ������ � �����...';
  SMsgSaveDataForm       = '���������� ���������� ����...';
  SMsgSaveDataFormEx     = '���������� ���������� %s...';
  SMsgPrintingWait       = '���� ������...';
  SMsgImportDataWait     = '������ ������. ��������� ����������...';
  SMsgExportDataWait     = '��c���� ������. ��������� ����������...';
  SMsgDeleteDataWait     = '�������� ������. ��������� ����������...';
  SMsgExportDataName     = '������� ������ ��� "%s"...';
  SMsgCancelChanges      = '������ ��������� ���������...';
  SMsgWorkingWait        = '��������� ����������...';
  SMsgWait               = '��������� ����������...';
  SMsgGetSelection       = '����������������...';
  SMsgSetSelection       = '��������� ����������...';
  SMsgFindData           = '����� ������...';
  SMsgFindFiles          = '����� ������...';
  SMsgReplaceData        = '������ ������...';
  SMsgSortData           = '���������� ������...';
  SMsgCreateNewRecord    = '�������� ����� ������...';
  SMsgGenerateReport     = '��������� ������...';
  SMsgGeneratePage       = '��������� �������� %d �� %d...';
  SMsgGenerateSheet      = '��������� ����� "%s"...';
  SMsgPrepareOperation   = '���������� ��������...';
  SMsgOperationComplete  = '�������� ��������� �������!';
  SMsgRepaceCount        = '����� ����������� %d �����!';
  SMsgRefreshData        = '���������� ������...';
  SMsgCloneSubData       = '����������� ��������� ������...';

  SPrmFind               = '������';
  SPrmFilter             = '�������';
  SPrmOrder              = '����������';
  SPrmGridTuner          = '�������';

  STextFilterSelected    = '�� ���������';    

  SWrnNotEnoughRights    = '������������ ���� ��� ���������� ��������!';

  SQueryCloseProgram     = '��������� ������ � ����������?';
  SQueryBreakOperation   = '�������� ������� ��������?';
  SQueryResetColumns     = '������� ���������������� ��������� �������� �������?';
  SQueryVisibleFields    = '������� ���������� ���� ��������� �������, ������� ���������?';
  SQueryCreateNewDoc     = '������� ����� ��������?';
  SQuerySaveChanges      = '��������� �� ���������! ��������� ��������� ���������?';
  SQueryDescardChanges   = '��� ��������� ��������� ����� ��������! ����������?';
  SQuerySetDefault       = '�� ������������� ������ ���������� �������� "�� ���������"?';
  SQueryCloseConnect     = '��� ���������� ����������� �������� ���������� ������� ��� ���� � ���������� � ����� ������. ����������?';
  SQueryReconnect        = '��������� ��������������� � ���� ������ � ������ �����������?';

  SSelectDirectory       = '�������� �������';
  SSelectFile            = '�������� ����';

  SDataSetNil            = '����� ������ �� ������';
  SDataSetInactive	     = '����� ������ ������';    // Dataset is closed, so its data is unavailable.
  SDataSetBrowse	       = '��������';               // Data can be viewed, but not changed. This is the default state of an open dataset.
  SDataSetEdit	         = '��������������';         // Active record can be modified.
  SDataSetInsert	       = '����� ������';           // The active record is a newly inserted buffer that has not been posted. This record can be modified and then either posted or discarded.
  SDataSetSetKey	       = '������...';              // TTable and TClientDataSet only. Record searching is enabled, or a SetRange operation is under way. A restricted set of data can be viewed, and no data can be edited or inserted.
  SDataSetCalcFields	   = '��������� ��������...';  // An OnCalcFields event is in progress. Noncalculated fields cannot be edited, and new records cannot be inserted.
  SDataSetFilter	       = '������...';              // An OnFilterRecord event is in progress. A restricted set of data can be viewed. No data can edited or inserted.
  SDataSetNewValue	     = '��������������...';      // Temporary state used internally when a field component�s NewValue property is accessed.
  SDataSetOldValue	     = '��������������...';      // Temporary state used internally when a field component�s OldValue property is accessed.
  SDataSetCurValue	     = '��������������...';      // Temporary state used internally when a field component�s CurValue property is accessed.
  SDataSetBlockRead	     = '������ ������...';       // Data-aware controls are not updated and events are not triggered when moving to the next record.
  SDataSetInternalCalc	 = '��������� ��������...';  // Temporary state used internally when values need to be calculated for a field that has a FieldKind of fkInternalCalc.
  SDataSetOpening	       = '�������� ������...';     // DataSet is in the process of opening but has not finished. This state occurs when the dataset is opened for asynchronous fetching.

  SFieldRequired         = '���� ''%s'' ������ ���� ���������!';
  SFieldRefEmpty         = '��� ���� ''%s'' �� �������� ���������� ��������!';
  SFieldNotListed        = '���� ''%s'' �� ���������� � ������ ��������!';

  SSortTreeNone          = '�������� �� �����������';
  SSortTreeId            = '�������� ����������� �� �������������� ������';
  SSortTreeTypeId        = '�������� ����������� �� ���� � ��������������';
  SSortTreeName          = '�������� ����������� �� ������������ ������';
  SSortTreeTypeName      = '�������� ����������� �� ���� � ������������';

  SLogDbVersionWarning   = '%s: �� ��������� ������ ���� ������ (DB ver: %d) � ������������ ����������� (DB ver: %d)!';
  SLogDbVersionUpdate    = '%s: ����������� ���������� ���� ������, ��������������� ��� DB ver: %d �� ����� "%s".';
  SLogDbVersionSaveNum   = '%s: �������� ������ ���� ������ (DB ver: %d).';

  SDbVersionFilter       = '���� �������� ���������� #%0:d|%1:s';
  SDbVersionSelFile      = '������� ���� �������� ����������';
  SDbVersionScrNotFound  = '�� ������ ���� �������� ���������� "%s"!';
  SDbVersionSqlText      = '> %s';
  SDbVersionSqlOk        = 'OK'#13#10;
  SDbVersionSqlError     = '#ERROR#: %s';

  SDbVersionChkError     = '�� ������� ��������� ������������ ������ ���� ������!';
  SDbVersionQryError     = '���������� ���������� �������� ����������?';

  SDbVersionWrnNewer     = '����� ������ ���� ������ (DB ver: %d) ���� ������ �� (DB ver: %d)!'#13#13 +
                           '���������� ��������� ���������� ������������ �����������.'#13#13 +
                           '�� ������ ���������� ������ � ������ ������� ������������ �����������?';
  SDbVersionWrnOlder     = '����� ������ ���� ������ (DB ver: %d) ���� ������ �� (DB ver: %d)!'#13#13 +
                           '���������� ��������� ���������� ���� ������.'#13#13 +
                           '�� ������ ���������� ������ � ������ ������� ������������ �����������?';
  SDbVersionQryCreate    = '������ ���� ������ �� ����������������.'#13#13 +
                           '��������� �������� �������� ����� ���� ������?';
  SDbVersionQryUpdate    = '����� ������ ���� ������ (DB ver: %d) ���� ������ �� (DB ver: %d)!'#13#13 +
                           '��������� ���������� ���� ������ ������?';
  SDbVersionWrnCancel    = '�� ������� ��������� ���������� ���� ������!'#13#13 +
                           '�� ������ ���������� ������ � ������ ������� ������������ �����������?';

  SDbVersionQryResque    = '������� ��������� ����� ���� ������ ����� ����������� ���������� (�������������)?';
  SDbVersionWrnResque    = '����� ������� ���������� ������������ ������������� ������� ��������� ����� ������� ������ ���� ������ (���������� ����)!'#13 +
                           '��������� ���������� ���� ������ ������?';


  SErrAssertError        = '���������� ������!!! ���������� � ������������!';
  SErrLoadLibrary        = '������ �������� ������������ ���������� ''%s''!';
  SErrLoadLibraryEx      = '������ �������� ������������ ���������� ''%s'':'#13'%s!';
  SErrFindProcedure      = '��������� ''%s'' �� ������� � ������������ ���������� ''%s''!';
  SErrWindowsError       = '��������� ������!';
  SErrWindowsCode        = '��� ������: %d!';
  SErrSystemError        = '������ #%d: "%s"!';
  SErrBadConnectionType  = '����� ���������� � ����� ������ ''%s'' �� ������������� ������ ������ ������ ''%s''!';
  SErrDataSetNull        = '������ ������ �� ������ ��� �� ��������������� (''null'')!';
  SErrInitForm           = '������ ������������� ����!';
  SErrDoneForm           = '������ ���������� �������������� �������� ����!';
  SErrLoadFormPlacement  = '������ �������� � �������������� ���������� ����!';
  SErrSaveFormPlacement  = '������ ���������� ���������� ����!';
  SErrChangeStyle        = '������ ��������� ������ ����������!';
  EErrBadOperationTag    = '������������ ��� ��������: %d.';
  SErrSetStyles          = '������ ��������� ���� ����������!';
  EDataModuleNotCreated  = '������ ���������� � ����� ������ �� ���������������!';
  SErrNotDbConnect       = '���������� � ����� ������ �� �����������!';
  SErrBadDbCfgFile       = '���� ������������ ����������� � ���� ������ "%s" �� ������!'#13'��������� ��������� ����������� � ���� ������ ����� ���� "����" - "��������� ���������� � ��".';
  SErrConnDatabase       = '������ ���������� � ����� ������!';
  sErrReconnDatabase     = '�� ������� ������������ ����������� � ���� ������ �� ���������� �����!'#13'��������� ����� �������.';
  SErrConfDatabase       = '������ ��������� ������������ ���� ������!';
  SErrCopyDatabase       = '������ �������� ��������� ����� ���� ������!';
  SErrRestDatabase       = '������ �������������� ���� ������ �� ��������� �����!';
  SErrBackupDisabled     = '��������� ����������� ���� ������ �� ���������!';
  SErrCheckDbVersion     = '������ �������� ������ ���� ������!';
  SErrReadDbVersion      = '������ ������ ������ ���� ������!';
  SErrSaveDbVersion      = '������ ���������� ������ ���� ������!';
  SErrUpdateDatabase     = '������ ���������� ���� ������!';
  SErrLoadSystemParams   = '������ �������� ���������� ������� �� ���� ������!';
  SErrOpenDataSet        = '������ �������� ������ �� ���� ������: ������ ''%s''!';
  SErrRefreshDataSet     = '������ ���������� ������� �� ���� ������: ������ ''%s''!';
  SErrPostError          = '������ ���������� ��������� � ���� ������: ������ ''%s''!';
  SErrDeleteError        = '������ �������� ������ � ���� ������: ������ ''%s''!'#13'�������� ��� ������ �������� ��-�� ����, ��� � ������ �������� ���������� ������, ����������� �� ������ ������.';
  SErrGetRecordCount     = '������ ����������� ���������� ������� � ������� ''%s''!';
  SErrLoadData           = '������ �������� ������!';
  SErrLoadTree           = '������ �������� ��������� ������!';
  SErrReloadTree         = '������ ������������ ��������� ��������� ��� ����������� ��������!';
  SErrRecordInsert       = '������ ���������� ������!';
  SErrRecordImport       = '������ ������� ������!';
  SErrRecordEdit         = '������ �������������� ������!';
  SErrRecordDelete       = '������ �������� ������!';
  SErrRecordMultiprocess = '������ ���������������!';
  SErrFindError          = '�� ������� ��������� ������ ��� ������� ''%s''!'#10#10'���������� �������� ������� ������.';
  SErrTreeDSNil          = '�� ��������� ����� ������ ��� �������� ������������� ���������!';
  SErrNoSelectedItem     = '�� ������ ������ ��� ���������� ��������!';
  SErrDataSetIsEmpty     = '��� ������ ��� ���������� ��������!';
  SErrIdNotFound         = '������ � id=%d � ��������� ������ �� ������!';
  SErrIdNotFoundS        = '������ � id="%s" � ��������� ������ �� ������!';
  SErrDSIdNotFound       = '������ � id=%d � ������ ������ "%s" �� �������!';
  SErrDSNameNotFound     = '������ "%s" � ������ ������ "%s" �� �������!';
  SErrDSFieldNotFound    = '���� "%s" � ������ ������ "%s" �� �������!';
  SErrStrNotFound        = '������ "%s" �� �������!';
  SErrFileNotFound       = '���� "%s" �� ������!';
  SErrDirNotFound        = '������� "%s" �� ������!';
  SErrPathNotFound       = '���� "%s" �� ������!';
  SErrFileDelete         = '������ �������� ����� "%s": %s!';
  EErrCreateRecReport    = '������ ��������� ������ ��� ������� ������!';
  EErrCreateReport       = '������ ��������� ������!';
  EErrCreatePage         = '������ �������� �������� "%s"!';
  SErrMoveRecord         = '������ ����������� ������ � ��������� ������ / �����!';
  SErrInitDataModule     = '������ ������������� ������ %s!';
  SErrLoadFile           = '������ �������� �����: "%s"!';
  SErrSaveFile           = '������ ������ � ����: "%s"!';
  SErrImportFile         = '������ �������� ������ �� �����: "%s"!';
  SErrExportFile         = '������ �������� ������ � ����: "%s"!';
  SErrLoadReport         = '������ �������� ������ "%s" �� ���� ������!';
  SErrSaveReport         = '������ ���������� ������ "%s" � ���� ������!';
  SErrGenerateReport     = '������ �������� ������!';
  SErrNotRightsForOper   = '������������ ���� ��� ���������� �������� "%s"!';
  SErrCancelDisabled     = '���������� ������� ���� � ������� ��������� ���������, ��� ��� ������� ��������� � ����������� ���������!';

  SCopyrightsStr         = '%s: %s. Copyright by %s';
  SFmtNotConnected       = 'Not connected';
  SFmtConnDatabase       = 'Db: %s';
  SFmtEditorCaption      = '%s: %s';
  SViewNotEnabled        = '< ������ >';
  SRecordInactive        = '��� ������';
  SRecordNum             = '%d';
  SRecordCount           = '%d';
  SRecordNumCount        = '%d:%d';
  SFileNoName            = '��� ����� %d';
  SCaptionRecordNum      = '������: %d';
  SCaptionRecordCount    = '����� �������: %d';
  SCaptionRecordNumCount = '������: %d �� %d';
  SCaptionState          = '�����: "%s"';
  SFoundFilesCount       = '������� ������: %d';
  SItemsCount            = '��������: %d';
  SRecordsCount          = '�������: %d';
  SSortDisableName       = '��� ����������';
  SSortDisableHint       = '��������� ���������� ������';
  SSortItemName          = '�� ������� ''%s''';
  SSortItemHint          = '����������� ������ �� ������� ''%s''';
  SBooleanOn             = '��������';
  SBooleanOff            = '���������';
  SDots                  = '...';
  SAllFilesFilter        = '��� ����� (*.*)|*.*';
  SAlignmentLeft         = '�� ������ ����';
  SAlignmentRight        = '�� ������� ����';
  SAlignmentCenter       = '�� ������';

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



