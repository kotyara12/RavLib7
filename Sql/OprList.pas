unit OprList;

interface

const
  {$IFDEF ADMIN}
  tagViewSysSettings      = 8000;        // �������� ���������� �������
  tagEditSysSettings      = 8001;        // ��������� ���������� �������
  tagViewOpGroups         = 9000;        // �������� ������ ����� ����
  tagEditOpGroups         = 9001;        // ���������� ������� ����� ����
  tagViewUsers            = 9100;        // �������� ������ �������������
  tagEditUsers            = 9101;        // ���������� ������� �������������
  tagBlockUsers           = 9102;        // ���������� � ������������� �������������
  tagRepEditUsers         = 9103;        // �������������� ������ ������� �������
  tagEditUserGroup        = 9104;        // �������������� ����� �������������
  tagViewSecurity         = 9200;        // �������� �������� ������������
  tagEditSecurity         = 9201;        // �������������� �������� ������������
  tagViewSysLog           = 9210;        // �������� ��������� �������
  tagClearSysLog          = 9211;        // ������� ��������� �������
  tagRepEditSysLog        = 9212;        // �������������� ������ ��������� �������
  tagViewSrLevels         = 9220;        // �������� �������� ���� ���������
  tagEditSrLevels         = 9221;        // �������������� �������� ���� ���������
  tagViewSrOperations     = 9222;        // �������� ������ ��������
  {$ENDIF}

var
  {$IFDEF ADMIN}
  orViewOperation      	  : Boolean = False; // ������ � ��������� ������� ��������
  orViewOpGroups       	  : Boolean = False; // ������ � ��������� ������� ����� ��������
  orEditOpGroups       	  : Boolean = False; // ������ � ��������� ������� ����� ��������
  orViewUsers          	  : Boolean = False; // ������ � ��������� ������� �������������
  orEditUsers          	  : Boolean = False; // ������ � ��������� ������� �������������
  orBlockUsers         	  : Boolean = False; // ������ � ���������� � ������������� ������������
  orRepEditUsers       	  : Boolean = False; // ������ � �������������� ������ ������� �������������
  orEditUserGroup      	  : Boolean = False; // ������ � �������������� ����� �������������
  orViewSecurity       	  : Boolean = False; // ������ � ��������� ���������� ������������
  orEditSecurity       	  : Boolean = False; // ������ � ��������� ���������� ������������
  orViewSysLog         	  : Boolean = False; // ������ � ��������� ��������� �������
  orClearSysLog        	  : Boolean = False; // ������ � ������� ��������� �������
  orRepEditSysLog      	  : Boolean = False; // ������ � �������������� ������ ��������� �������
  orViewSysSettings    	  : Boolean = False; // ������ � ��������� ���������� �������
  orEditSysSettings    	  : Boolean = False; // ������ � �������������� ���������� �������
  orViewSrLevels       	  : Boolean = False; // ������ � ��������� �������� ����
  orEditSrLevels       	  : Boolean = False; // ������ � �������������� �������� ����
  {$ENDIF}

implementation

end.
