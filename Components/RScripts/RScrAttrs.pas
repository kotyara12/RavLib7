unit RScrConsts;

interface

const
  SRCmdErrAttr  : array [TRCmdErrAttr] of char = (
    'C', // eaContinue,         ���������� ���������� �������
    'R', // eaRestore,          ���������� ��������� � �������� �����
    'U'  // eaRunUndo           ���������� ��������� � ��������� ��������� Undo
    );

implementation

end.
