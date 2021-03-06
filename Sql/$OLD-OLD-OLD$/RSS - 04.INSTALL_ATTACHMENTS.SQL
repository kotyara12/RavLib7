CREATE TABLE dbo.SS_ATTACHMENTS (
	ID int NOT NULL ,
	OBJECT_NAME str32 NOT NULL ,
	OBJECT_ID int NOT NULL ,
	FILENAME str128 NULL ,
	FILEDATA image NULL ,
	FILETIME datetime NULL ,
	FILESIZE int NULL ,
	CONSTRAINT PK_SS_ATTACHMENTS_ID PRIMARY KEY  CLUSTERED (ID)
)
GO

CREATE INDEX LK_SS_ATTACHMENTS_OBJECT_NAME ON dbo.SS_ATTACHMENTS (OBJECT_NAME)
GO

CREATE INDEX LK_SS_ATTACHMENTS_OBJECT_ID ON dbo.SS_ATTACHMENTS (OBJECT_NAME, OBJECT_ID);
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SS_ATTACHMENTS  TO public
GO

-- INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (1101, 1, 2, '.\Temp', '��������� ������� ��� ��������� ��������')
INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (1102, 1, 1, '1024', '������ �������� � ����������, ����� �������� ������������� �������������')
INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (1103, 1, 1, '10240', '����������� ���������� ������ �������� � ����������')
GO