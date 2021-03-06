/****** �������� �� ��������� ******/

PRINT '�������� �������� �� ���������'
GO

create default bit_false as 0
GO

create default bit_true as 1
GO

create default clBlack as 0
GO

create default clWhite as 16777215
GO

create default int_0 as 0
GO

create default real_0 as 0
GO

create default real_999999999 as 999999999.99
GO

/****** ���� ������ ******/

PRINT '�������� ���������������� ����� ������'
GO

setuser 'dbo'
GO

EXEC sp_addtype 'str255', 'varchar (255)', 'null'
GO

EXEC sp_addtype 'str192', 'varchar (192)', 'null'
GO

EXEC sp_addtype 'str128', 'varchar (128)', 'null'
GO

EXEC sp_addtype 'str64', 'varchar (64)', 'null'
GO

EXEC sp_addtype 'str32', 'varchar (32)', 'null'
GO

EXEC sp_addtype 'str24', 'varchar (24)', 'null'
GO

EXEC sp_addtype 'str16', 'varchar (16)', 'null'
GO

EXEC sp_addtype 'str10', 'varchar (10)', 'null'
GO

EXEC sp_addtype 'str5', 'varchar (5)', 'null'
GO

setuser
GO


/****** ������� ******/

PRINT '�������� ������ ���������� ��������'
GO

CREATE TABLE dbo.SR_LEVELS (
	ID int NOT NULL ,
	NAME str64 NULL ,
	NOTES str255 NULL ,
	HIDDEN bit NOT NULL ,
	FONT_STYLE int NULL ,
	FONT_COLOR int NULL ,
	CELL_COLOR int NULL ,
	CONSTRAINT PK_SR_LEVELS_ID PRIMARY KEY  CLUSTERED (ID) 
)
GO

setuser 'dbo'
GO

EXEC sp_bindefault 'dbo.bit_false', 'SR_LEVELS.HIDDEN'
GO

EXEC sp_bindefault 'dbo.int_0', 'SR_LEVELS.FONT_STYLE'
GO

EXEC sp_bindefault 'dbo.clBlack', 'SR_LEVELS.FONT_COLOR'
GO

EXEC sp_bindefault 'dbo.clWhite', 'SR_LEVELS.CELL_COLOR'
GO

setuser
GO

GRANT  REFERENCES ,  SELECT ,  UPDATE  ON SR_LEVELS  TO public
GO

CREATE TABLE dbo.SR_OPERATIONS (
	ID int NOT NULL ,
	ID_LEVELS int NOT NULL,	
	NAME str64 NULL ,
	NOTES str255 NULL ,
	CONSTRAINT PK_SR_OPERATIONS_ID PRIMARY KEY  CLUSTERED (ID),
	CONSTRAINT FK_SR_OPERATIONS_LEBELS FOREIGN KEY (ID_LEVELS) REFERENCES dbo.SR_LEVELS (ID)
)
GO

GRANT  REFERENCES ,  SELECT  ON SR_OPERATIONS  TO public
GO

CREATE TABLE dbo.SR_OPGROUPS (
	ID int NOT NULL ,
	NAME str64 NULL ,
	NOTES str255 NULL ,
	CONSTRAINT PK_SR_OPGROUPS_ID PRIMARY KEY  CLUSTERED (ID)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SR_OPGROUPS  TO public
GO

CREATE TABLE dbo.SR_WORKPLASES (
	ID int NOT NULL ,
	NAME_S str16 NULL ,
	NAME str128 NULL ,
	CONSTRAINT PK_SR_WORKPLASES_ID PRIMARY KEY  CLUSTERED (ID)
)
GO

GRANT  REFERENCES ,  SELECT  ON SR_WORKPLASES  TO public
GO

CREATE TABLE dbo.SS_BLOCKIDS (
	NAME str32 NOT NULL ,
	ID int NOT NULL ,
	CONSTRAINT PK_SS_BLOCKIDS_ID PRIMARY KEY  CLUSTERED (NAME, ID)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SS_BLOCKIDS  TO public
GO

CREATE TABLE dbo.SS_VARGROUPS (
	ID int NOT NULL ,
	OWNER_ID int NULL ,
	NAME str64 NOT NULL ,
	NOTES str255 NULL ,
	CONSTRAINT PK_SS_VARGROUPS_ID PRIMARY KEY  CLUSTERED (ID)
)
GO

GRANT  REFERENCES ,  SELECT  ON SS_VARGROUPS  TO public
GO

CREATE TABLE dbo.SS_VARNAMES (
	ID int NOT NULL ,
	ID_GROUPS int NOT NULL ,
	TYPE int NOT NULL ,
	DEF_VALUE str255 NULL ,
	NAME str128 NOT NULL ,
	CONSTRAINT PK_SS_VARNAMES_ID PRIMARY KEY  CLUSTERED (ID),
	CONSTRAINT FK_SS_VARNAMES_GROUPS FOREIGN KEY (ID_GROUPS) REFERENCES dbo.SS_VARGROUPS (ID)
)
GO

GRANT  REFERENCES ,  SELECT   ON SS_VARNAMES  TO public
GO

CREATE TABLE dbo.SS_SETTINGS (
	ID int NOT NULL ,
	VALUE_INT int NULL ,
	VALUE_REAL real NULL ,
	VALUE_CHAR str255 NULL ,
	CONSTRAINT PK_SS_SETTINGS_ID PRIMARY KEY  CLUSTERED (ID)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SS_SETTINGS  TO public
GO

CREATE TABLE dbo.SS_SYSLOG (
	DATETIME datetime NOT NULL ,
	ID_OPERATIONS int NOT NULL ,
	ID_USERS int NOT NULL ,
	ID_WORKPLASES int NOT NULL ,
	INFO str255 NOT NULL ,
	HOST str32 NULL ,
	NETUSER str32 NULL ,
	CONSTRAINT PK_SS_SYSLOG_LOG PRIMARY KEY  CLUSTERED (DATETIME, ID_OPERATIONS, ID_USERS, INFO),
	CONSTRAINT FK_SS_SYSLOG_OPER FOREIGN KEY (ID_OPERATIONS) REFERENCES dbo.SR_OPERATIONS (ID),
	CONSTRAINT FK_SS_SYSLOG_WP FOREIGN KEY (ID_WORKPLASES) REFERENCES dbo.SR_WORKPLASES (ID)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE  ON SS_SYSLOG  TO public
GO

CREATE TABLE dbo.SU_GROUPS (
	ID int NOT NULL ,
	OWNER_ID int NULL ,
	NAME str64 NOT NULL ,
	NOTES str255 NULL ,
	CONSTRAINT PK_SU_GROUPS_ID PRIMARY KEY  CLUSTERED (ID)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SU_GROUPS  TO public
GO

CREATE TABLE dbo.SU_USERS (
	ID int NOT NULL ,
	ID_GROUPS int NOT NULL ,
	NAME str16 NOT NULL ,
	FULLNAME str64 NULL ,
	NOTES str64 NULL ,
	PASSWORD str255 NULL ,
	DELETED bit NOT NULL ,
	BLOCKED bit NOT NULL ,
	CHANGED datetime NULL ,
	COUNT_EP int NULL ,
	CONSTRAINT PK_SU_USERS_ID PRIMARY KEY  CLUSTERED (ID),
	CONSTRAINT UQ_SU_USERS_NAME UNIQUE  NONCLUSTERED (NAME),
	CONSTRAINT FK_SU_USERS_GROUPS FOREIGN KEY (ID_GROUPS) REFERENCES dbo.SU_GROUPS (ID)
)
GO

setuser 'dbo'
GO

EXEC sp_bindefault 'dbo.bit_false', 'SU_USERS.BLOCKED'
GO

EXEC sp_bindefault 'dbo.bit_false', 'SU_USERS.DELETED'
GO

EXEC sp_bindefault 'dbo.int_0', 'SU_USERS.COUNT_EP'
GO

setuser
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SU_USERS  TO public
GO

CREATE TABLE dbo.SR_OPGLINKS (
	ID_OPGROUPS int NOT NULL ,
	ID_OPERATIONS int NOT NULL ,
	CONSTRAINT PK_SR_OPGLINKS_ID PRIMARY KEY  CLUSTERED (ID_OPGROUPS, ID_OPERATIONS),
	CONSTRAINT FK_SR_OPGLINKS_OPER FOREIGN KEY (ID_OPERATIONS) REFERENCES dbo.SR_OPERATIONS (ID),
	CONSTRAINT FK_SR_OPGLINKS_GRP FOREIGN KEY (ID_OPGROUPS) REFERENCES dbo.SR_OPGROUPS (ID)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SR_OPGLINKS  TO public
GO

CREATE TABLE dbo.SR_OPWLINKS (
	ID_WORKPLASES int NOT NULL ,
	ID_OPERATIONS int NOT NULL ,
	CONSTRAINT PK_SR_OPWLINKS_ID PRIMARY KEY  CLUSTERED (ID_WORKPLASES, ID_OPERATIONS),
	CONSTRAINT FK_SR_OPWLINKS_WP FOREIGN KEY (ID_WORKPLASES) REFERENCES dbo.SR_WORKPLASES (ID),
	CONSTRAINT FK_SR_OPWLINKS_OPER FOREIGN KEY (ID_OPERATIONS) REFERENCES dbo.SR_OPERATIONS (ID)
)
GO

GRANT  REFERENCES ,  SELECT  ON SR_OPWLINKS  TO public
GO

CREATE TABLE dbo.SU_OPGLINKS (
	ID_USERS int NOT NULL ,
	ID_OPGROUPS int NOT NULL ,
	CONSTRAINT PK_SU_OPGLINKS_ID PRIMARY KEY  CLUSTERED (ID_USERS, ID_OPGROUPS),
	CONSTRAINT FK_SU_OPGLINKS_GRP FOREIGN KEY (ID_OPGROUPS) REFERENCES dbo.SR_OPGROUPS (ID),
	CONSTRAINT FK_SU_OPGLINKS_USER FOREIGN KEY (ID_USERS) REFERENCES dbo.SU_USERS (ID)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SU_OPGLINKS  TO public
GO

CREATE TABLE dbo.SU_OPRLINKS (
	ID_USERS int NOT NULL ,
	ID_OPERATIONS int NOT NULL ,
	CONSTRAINT PK_SU_OPRLINKS_ID PRIMARY KEY  CLUSTERED (ID_USERS, ID_OPERATIONS),
	CONSTRAINT FK_SU_OPRLINKS_OPER FOREIGN KEY (ID_OPERATIONS) REFERENCES dbo.SR_OPERATIONS (ID),
	CONSTRAINT FK_SU_OPRLINKS_USER FOREIGN KEY (ID_USERS) REFERENCES dbo.SU_USERS (ID)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SU_OPRLINKS  TO public
GO


/****** ���� ******/

PRINT '�������� ��������'
GO

CREATE VIEW VSR_OPERATIONS_ALL AS
SELECT
  SR_OPERATIONS.ID, 
  SR_OPERATIONS.ID_LEVELS,
  SR_OPERATIONS.NAME,
  SR_OPERATIONS.NOTES,
  SR_LEVELS.NAME AS LEVEL_NAME,
  SR_LEVELS.NOTES AS LEVEL_NOTES,
  SR_LEVELS.FONT_STYLE,
  SR_LEVELS.FONT_COLOR,
  SR_LEVELS.CELL_COLOR
FROM SR_OPERATIONS, SR_LEVELS
WHERE SR_OPERATIONS.ID_LEVELS = SR_LEVELS.ID
GO

GRANT  SELECT  ON dbo.VSR_OPERATIONS_ALL  TO public
GO

CREATE VIEW VSR_OPERATIONS_WORK AS
SELECT
  SR_OPERATIONS.ID, 
  SR_OPERATIONS.ID_LEVELS,
  SR_OPERATIONS.NAME,
  SR_OPERATIONS.NOTES,
  SR_LEVELS.NAME AS LEVELS_NAME,
  SR_LEVELS.NOTES AS LEVELS_NOTES,
  SR_LEVELS.FONT_STYLE,
  SR_LEVELS.FONT_COLOR,
  SR_LEVELS.CELL_COLOR
FROM SR_OPERATIONS, SR_LEVELS
WHERE (SR_OPERATIONS.ID_LEVELS = SR_LEVELS.ID) 
  AND (SR_OPERATIONS.ID_LEVELS IN (SELECT SR_LEVELS.ID FROM SR_LEVELS WHERE SR_LEVELS.HIDDEN=0))
GO

GRANT  SELECT  ON dbo.VSR_OPERATIONS_WORK  TO public
GO

CREATE VIEW VSR_OPERATIONS_LIST AS
SELECT
  SR_OPERATIONS.ID, 
  SR_OPERATIONS.NAME,
  SR_OPERATIONS.NOTES
FROM SR_OPERATIONS
WHERE SR_OPERATIONS.ID_LEVELS IN (SELECT SR_LEVELS.ID FROM SR_LEVELS WHERE SR_LEVELS.HIDDEN=0)
GO

GRANT  SELECT  ON dbo.VSR_OPERATIONS_LIST  TO public
GO

CREATE VIEW VSR_OPERATIONS_USER AS
SELECT ID_USERS, ID_OPERATIONS
FROM SU_OPGLINKS U left join SR_OPGLINKS G on U.ID_OPGROUPS=G.ID_OPGROUPS
GROUP BY ID_USERS, ID_OPERATIONS
  UNION
SELECT ID_USERS, ID_OPERATIONS
FROM SU_OPRLINKS
GO

GRANT  REFERENCES ,  SELECT ON VSR_OPERATIONS_USER  TO public
GO

CREATE VIEW VSS_SYSLOG AS
SELECT 
  SS_SYSLOG.DATETIME, 
  SS_SYSLOG.ID_OPERATIONS, 
  SR_OPERATIONS.ID_LEVELS, 
  SS_SYSLOG.ID_USERS, 
  SS_SYSLOG.ID_WORKPLASES,
  INFO,  HOST,  NETUSERFROM SS_SYSLOG, SR_OPERATIONS
WHERE SS_SYSLOG.ID_OPERATIONS = SR_OPERATIONS.ID
GO

GRANT  SELECT ,  DELETE  ON dbo.VSS_SYSLOG  TO public
GO

CREATE VIEW VSS_SETTINGS AS
select
  SS_VARNAMES.ID,
  SS_VARNAMES.ID_GROUPS,
  SS_VARNAMES.TYPE,
  SS_VARNAMES.DEF_VALUE,
  SS_SETTINGS.VALUE_INT,
  SS_SETTINGS.VALUE_REAL,
  SS_SETTINGS.VALUE_CHAR,
  SS_VARNAMES.NAME
from SS_VARNAMES left join SS_SETTINGS on SS_VARNAMES.ID = SS_SETTINGS.ID
GO

GRANT  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON dbo.VSS_SETTINGS  TO public
GO

/****** �������� ******/

PRINT '�������� ���������'
GO

CREATE TRIGGER SR_OPERATIONS_DELETE ON dbo.SR_OPERATIONS 
FOR DELETE 
AS
BEGIN
  set NOCOUNT ON
  delete from SR_OPGLINKS where SR_OPGLINKS.ID_OPERATIONS in
   (select ID from DELETED)
  delete from SR_OPWLINKS where SR_OPWLINKS.ID_OPERATIONS in
   (select ID from DELETED)
  delete from SU_OPRLINKS where SU_OPRLINKS.ID_OPERATIONS in
   (select ID from DELETED)
  set NOCOUNT OFF
END
GO

CREATE TRIGGER SR_OPGROUPS_DELETE ON dbo.SR_OPGROUPS 
FOR DELETE 
AS
BEGIN
  set NOCOUNT ON
  delete from SR_OPGLINKS where SR_OPGLINKS.ID_OPGROUPS in
   (select ID from DELETED)
  delete from SU_OPGLINKS where SU_OPGLINKS.ID_OPGROUPS in
   (select ID from DELETED)
  set NOCOUNT OFF
END
GO 

CREATE TRIGGER SR_WORKPLASES_DELETE ON dbo.SR_WORKPLASES 
FOR DELETE 
AS
BEGIN
  set NOCOUNT ON
  delete from SR_OPWLINKS where SR_OPWLINKS.ID_WORKPLASES in
   (select ID from DELETED)
  set NOCOUNT OFF
END
GO

CREATE TRIGGER SU_USERS_DELETE ON dbo.SU_USERS 
FOR DELETE 
AS
BEGIN
  if (select SS_SETTINGS.VALUE_INT from SS_SETTINGS where SS_SETTINGS.ID = 9000) <> 1
  begin
    rollback tran
    raiserror ('�������� ������������� ��������� ��������� ������������ �������!', 16, 10)
  end
  else begin
    set NOCOUNT ON
    delete from SU_OPGLINKS where SU_OPGLINKS.ID_USERS in
     (select DELETED.ID from DELETED)
    delete from SU_OPRLINKS where SU_OPRLINKS.ID_USERS in
     (select DELETED.ID from DELETED)
    set NOCOUNT OFF
  end
END
GO

-- CREATE TRIGGER SS_SYSLOG_INSERT ON dbo.SS_SYSLOG --
-- FOR INSERT --
-- AS --
-- BEGIN --
--   set NOCOUNT ON --
--  declare @PERIOD int --
--   select @PERIOD=VALUE_INT from SS_SETTINGS where ID=9005 --
--   if @PERIOD is NULL select @PERIOD=0 --
--   if @PERIOD > 0 delete from SS_SYSLOG where DATETIME<=DATEADD(day, -@PERIOD, getdate()) --
--   set NOCOUNT OFF --
-- END --
-- GO --

/****** �������� ��������� ******/

PRINT '�������� ��������� ��������'
GO

--- ��������� ������ ���� ������ (��� ������������� � �������� UPDATE) ---
CREATE PROCEDURE SP_SYS_CH_DB_VERSION (@VER int) AS
BEGIN
  update SS_SETTINGS set VALUE_INT=@VER where ID=1000
END
GO

--- ���������� ������ � �������� ������ ������� ---
CREATE PROCEDURE SP_SYS_ADD_LOG(@ID_WORKPLASES int, @ID_USERS int, @ID_OPERATIONS int,
  @HOST str32, @NETUSER str32, @INFO str255) 
AS BEGIN
  if (select VALUE_INT from SS_SETTINGS where ID=9001)=1
    insert into SS_SYSLOG (DATETIME, ID_OPERATIONS, ID_USERS, ID_WORKPLASES, INFO, HOST, NETUSER) 
      values (GETDATE(), @ID_OPERATIONS, @ID_USERS, @ID_WORKPLASES, @INFO, @HOST, @NETUSER)
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_ADD_LOG  TO public
GO

--- ���������� �������� � ������ ������� ----
CREATE PROCEDURE SP_SYS_GROUP_ADD_OPERATION(@ID_OPGROUPS int, @ID_OPERATIONS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  insert into SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) values (@ID_OPGROUPS, @ID_OPERATIONS)
  --- ��������� ������ � �������� ---
  declare @MESSAGE str255
  select @MESSAGE = '"������ �������" [SR_OPGLINKS]: �������� ID="' 
    + convert(varchar(16), @ID_OPERATIONS) 
    + '", NAME="' + (select NAME from SR_OPERATIONS where ID=@ID_OPERATIONS) 
    + '" �������� � ������ ������� ID="' 
    + convert(varchar(16), @ID_OPGROUPS) 
    + '", NAME="' + (select NAME from SR_OPGROUPS where ID=@ID_OPGROUPS) 
    + '". [SP_SYS_GROUP_ADD_OPERATION]'
  exec SP_SYS_ADD_LOG @ID_WORKPLASES, @ID_USERS, 9001, @HOST, @NETUSER, @MESSAGE
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_GROUP_ADD_OPERATION  TO public
GO

--- �������� �������� �� ������ ������� ----
CREATE PROCEDURE SP_SYS_GROUP_DEL_OPERATION(@ID_OPGROUPS int, @ID_OPERATIONS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  if (select count(*) from SR_OPGLINKS where ID_OPGROUPS=@ID_OPGROUPS and ID_OPERATIONS=@ID_OPERATIONS) > 0 
  begin
    delete from SR_OPGLINKS where ID_OPGROUPS=@ID_OPGROUPS and ID_OPERATIONS=@ID_OPERATIONS
    --- ��������� ������ � �������� ---
    declare @MESSAGE str255
    select @MESSAGE = '"������ �������" [SR_OPGLINKS]: �������� ID="' 
      + convert(varchar(16), @ID_OPERATIONS) 
      + '", NAME="' + (select NAME from SR_OPERATIONS where ID=@ID_OPERATIONS) 
      + '" ��������� �� ������ ������� ID="' 
      + convert(varchar(16), @ID_OPGROUPS) 
      + '", NAME="' + (select NAME from SR_OPGROUPS where ID=@ID_OPGROUPS) 
      + '". [SP_SYS_GROUP_DEL_OPERATION]'
    exec SP_SYS_ADD_LOG @ID_WORKPLASES, @ID_USERS, 9001, @HOST, @NETUSER, @MESSAGE
  end
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_GROUP_DEL_OPERATION  TO public
GO

--- ���������� �������� ������������ ----
CREATE PROCEDURE SP_SYS_USER_ADD_OPERATION(@ID_SYSUSERS int, @ID_OPERATIONS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  insert into SU_OPRLINKS (ID_USERS, ID_OPERATIONS) values (@ID_SYSUSERS, @ID_OPERATIONS)
  --- ��������� ������ � �������� ---
  declare @MESSAGE str255
  select @MESSAGE = '"������������ �������" [SU_OPRLINKS]: �������� ID="' 
    + convert(varchar(16), @ID_OPERATIONS) 
    + '", NAME="' + (select NAME from SR_OPERATIONS where ID=@ID_OPERATIONS) 
    + '" �������� � ������ ������������ ID="' 
    + convert(varchar(16), @ID_SYSUSERS) 
    + '", NAME="' + (select NAME from SU_USERS where ID=@ID_SYSUSERS) 
    + '", FULLNAME="' + (select FULLNAME from SU_USERS where ID=@ID_SYSUSERS) 
    + '". [SP_SYS_USER_ADD_OPERATION]'
  exec SP_SYS_ADD_LOG @ID_WORKPLASES, @ID_USERS, 9001, @HOST, @NETUSER, @MESSAGE
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_USER_ADD_OPERATION  TO public
GO

--- �������� �������� � ������������ ----
CREATE PROCEDURE SP_SYS_USER_DEL_OPERATION(@ID_SYSUSERS int, @ID_OPERATIONS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  if (select count(*) from SU_OPRLINKS where ID_USERS=@ID_SYSUSERS and ID_OPERATIONS=@ID_OPERATIONS) > 0 
  begin
    delete from SU_OPRLINKS where ID_USERS=@ID_SYSUSERS and ID_OPERATIONS=@ID_OPERATIONS
    --- ��������� ������ � �������� ---
    declare @MESSAGE str255
    select @MESSAGE = '"������������ �������" [SU_OPRLINKS]: �������� ID="' 
      + convert(varchar(16), @ID_OPERATIONS) 
      + '", NAME="' + (select NAME from SR_OPERATIONS where ID=@ID_OPERATIONS) 
      + '" ��������� �� ������ ������������ ID="' 
      + convert(varchar(16), @ID_SYSUSERS) 
      + '", NAME="' + (select NAME from SU_USERS where ID=@ID_SYSUSERS) 
      + '", FULLNAME="' + (select FULLNAME from SU_USERS where ID=@ID_SYSUSERS) 
      + '". [SP_SYS_USER_DEL_OPERATION]'
    exec SP_SYS_ADD_LOG @ID_WORKPLASES, @ID_USERS, 9001, @HOST, @NETUSER, @MESSAGE
  end
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_USER_DEL_OPERATION  TO public
GO

--- ���������� ������ �������� ������������ ----
CREATE PROCEDURE SP_SYS_USER_ADD_OPGROUP(@ID_SYSUSERS int, @ID_OPGROUPS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  insert into SU_OPGLINKS (ID_USERS, ID_OPGROUPS) values (@ID_SYSUSERS, @ID_OPGROUPS)
  --- ��������� ������ � �������� ---
  declare @MESSAGE str255
  select @MESSAGE = '"������������ �������" [SU_OPGLINKS]: ������ ������� ID="' 
    + convert(varchar(16), @ID_OPGROUPS) 
    + '", NAME="' + (select NAME from SR_OPGROUPS where ID=@ID_OPGROUPS) 
    + '" �������� � ������ ������������ ID="' 
    + convert(varchar(16), @ID_SYSUSERS) 
    + '", NAME="' + (select NAME from SU_USERS where ID=@ID_SYSUSERS) 
    + '", FULLNAME="' + (select FULLNAME from SU_USERS where ID=@ID_SYSUSERS) 
    + '". [SP_SYS_USER_ADD_OPGROUP]'
  exec SP_SYS_ADD_LOG @ID_WORKPLASES, @ID_USERS, 9001, @HOST, @NETUSER, @MESSAGE
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_USER_ADD_OPGROUP  TO public
GO

--- �������� ������ �������� � ������������ ----
CREATE PROCEDURE SP_SYS_USER_DEL_OPGROUP(@ID_SYSUSERS int, @ID_OPGROUPS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  if (select count(*) from SU_OPGLINKS where ID_USERS=@ID_SYSUSERS and ID_OPGROUPS=@ID_OPGROUPS) > 0 
  begin
    delete from SU_OPGLINKS where ID_USERS=@ID_SYSUSERS and ID_OPGROUPS=@ID_OPGROUPS
    --- ��������� ������ � �������� ---
    declare @MESSAGE str255
    select @MESSAGE = '"������������ �������" [SU_OPGLINKS]: ������ ������� ID="' 
      + convert(varchar(16), @ID_OPGROUPS) 
      + '", NAME="' + (select NAME from SR_OPGROUPS where ID=@ID_OPGROUPS) 
      + '" ��������� �� ������ ������������ ID="' 
      + convert(varchar(16), @ID_SYSUSERS) 
      + '", NAME="' + (select NAME from SU_USERS where ID=@ID_SYSUSERS) 
      + '", FULLNAME="' + (select FULLNAME from SU_USERS where ID=@ID_SYSUSERS) 
      + '". [SP_SYS_USER_DEL_OPGROUP]'
    exec SP_SYS_ADD_LOG @ID_WORKPLASES, @ID_USERS, 9001, @HOST, @NETUSER, @MESSAGE
  end
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_USER_DEL_OPGROUP  TO public
GO

--- �������� ������ �������� � ���� ������������� ������� (����� ���������) ---
CREATE PROCEDURE SP_SYS_GROUP_UNLINK_USERS(@ID_OPGROUPS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  declare @ID_SYSUSERS int
  declare CS_OPGLINKS cursor for select ID_USERS from SU_OPGLINKS
  open CS_OPGLINKS
  fetch CS_OPGLINKS into @ID_SYSUSERS
  while @@FETCH_STATUS = 0
  begin
    exec SP_SYS_USER_DEL_OPGROUP @ID_SYSUSERS, @ID_OPGROUPS, @ID_WORKPLASES, @ID_USERS, @HOST, @NETUSER
    fetch next from CS_OPGLINKS into @ID_SYSUSERS
  end
  close CS_OPGLINKS
  deallocate CS_OPGLINKS
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_GROUP_UNLINK_USERS  TO public
GO

--- �������� �� ������ ���� �������� ---
CREATE PROCEDURE SP_SYS_GROUP_UNLINK_OPERS(@ID_OPGROUPS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  declare @ID_OPERATIONS int
  declare CS_OPGLINKS cursor for select ID_OPERATIONS from SR_OPGLINKS
  open CS_OPGLINKS
  fetch CS_OPGLINKS into @ID_OPERATIONS
  while @@FETCH_STATUS = 0
  begin
    exec SP_SYS_GROUP_DEL_OPERATION @ID_OPGROUPS, @ID_OPERATIONS, @ID_WORKPLASES, @ID_USERS, @HOST, @NETUSER
    fetch next from CS_OPGLINKS into @ID_OPERATIONS
  end
  close CS_OPGLINKS
  deallocate CS_OPGLINKS
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_GROUP_UNLINK_OPERS  TO public
GO

--- "�������" ������ ������� ---
CREATE PROCEDURE SP_SYS_GROUP_UNLINK_ALL(@ID_OPGROUPS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  exec SP_SYS_GROUP_UNLINK_USERS @ID_OPGROUPS, @ID_WORKPLASES, @ID_USERS, @HOST, @NETUSER
  exec SP_SYS_GROUP_UNLINK_OPERS @ID_OPGROUPS, @ID_WORKPLASES, @ID_USERS, @HOST, @NETUSER
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_GROUP_UNLINK_ALL  TO public
GO

--- �������� � ������������ ���� �������� ---
CREATE PROCEDURE SP_SYS_USER_UNLINK_OPERS(@ID_SYSUSERS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  declare @ID_OPERATIONS int
  declare CS_OPRLINKS cursor for select ID_OPERATIONS from SU_OPRLINKS
  open CS_OPRLINKS
  fetch CS_OPRLINKS into @ID_OPERATIONS
  while @@FETCH_STATUS = 0
  begin
    exec SP_SYS_USER_DEL_OPERATION @ID_SYSUSERS, @ID_OPERATIONS, @ID_WORKPLASES, @ID_USERS, @HOST, @NETUSER
    fetch next from CS_OPRLINKS into @ID_OPERATIONS
  end
  close CS_OPRLINKS
  deallocate CS_OPRLINKS
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_USER_UNLINK_OPERS  TO public
GO

--- �������� � ������������ ���� ����� �������� ---
CREATE PROCEDURE SP_SYS_USER_UNLINK_GROUPS(@ID_SYSUSERS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  declare @ID_OPGROUPS int
  declare CS_OPGLINKS cursor for select ID_OPGROUPS from SU_OPGLINKS
  open CS_OPGLINKS
  fetch CS_OPGLINKS into @ID_OPGROUPS
  while @@FETCH_STATUS = 0
  begin
    exec SP_SYS_USER_DEL_OPGROUP @ID_SYSUSERS, @ID_OPGROUPS, @ID_WORKPLASES, @ID_USERS, @HOST, @NETUSER
    fetch next from CS_OPGLINKS into @ID_OPGROUPS
  end
  close CS_OPGLINKS
  deallocate CS_OPGLINKS
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_USER_UNLINK_GROUPS  TO public
GO

--- "�������" ������������ ---
CREATE PROCEDURE SP_SYS_USER_UNLINK_ALL(@ID_SYSUSERS int,
  @ID_WORKPLASES int, @ID_USERS int, @HOST str32, @NETUSER str32)
AS BEGIN
  exec SP_SYS_USER_UNLINK_OPERS @ID_SYSUSERS, @ID_WORKPLASES, @ID_USERS, @HOST, @NETUSER
  exec SP_SYS_USER_UNLINK_GROUPS @ID_SYSUSERS, @ID_WORKPLASES, @ID_USERS, @HOST, @NETUSER
END
GO

GRANT  EXECUTE  ON dbo.SP_SYS_USER_UNLINK_ALL  TO public
GO

/****** ���������� ���� ������ ���������� ������� ******/

PRINT '���������� ������ ���������� �������'
GO

/****** ���������� ��������� ���������� ******/

INSERT INTO SS_VARGROUPS (ID, OWNER_ID, NAME, NOTES) VALUES (1, NULL, '��� ���������', NULL)
INSERT INTO SS_VARGROUPS (ID, OWNER_ID, NAME, NOTES) VALUES (2, 1, '���� ������', '��������� ���� ������')
INSERT INTO SS_VARGROUPS (ID, OWNER_ID, NAME, NOTES) VALUES (3, 1, '���������� ������������', '��������� ���������� ������������ RSS')
INSERT INTO SS_VARGROUPS (ID, OWNER_ID, NAME, NOTES) VALUES (4, 3, '�������� �������', '��������� ������� �������������')
INSERT INTO SS_VARGROUPS (ID, OWNER_ID, NAME, NOTES) VALUES (5, 3, '������ ������', '��������� ������� ������ �������')
GO

INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) 
VALUES (1000, 2, 0, NULL, '������� ������ ���� ������')
-- INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) --
-- VALUES (9001, 5, 6, '��������', '��������� ������� ������ �������') --
INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) 
VALUES (9002, 5, 1, '1000000', '����� �������������� � ���������� �������, �������')
INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) 
VALUES (9003, 4, 1, '40', '�������� � ���� ����� ��������� ������ ������')
INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) 
VALUES (9004, 4, 1, '6', '���������� ���������� ����� ������ ������������ � ��������')
INSERT INTO SS_VARNAMES (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) 
VALUES (9005, 5, 1, '365', '������ �������� ������� ������� ������ �������, ����')
GO

INSERT INTO SS_SETTINGS (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (1000, 1, NULL, NULL)              
INSERT INTO SS_SETTINGS (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9000, 0, NULL, NULL)
INSERT INTO SS_SETTINGS (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9001, 1, NULL, NULL)
INSERT INTO SS_SETTINGS (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9002, 1000000, NULL, NULL)
INSERT INTO SS_SETTINGS (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9003, 40, NULL, NULL)
INSERT INTO SS_SETTINGS (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9004, 6, NULL, NULL)
INSERT INTO SS_SETTINGS (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9005, 0, NULL, NULL)
GO

/****** ���������� ������ ������� ���� ******/

INSERT INTO SR_WORKPLASES (ID, NAME_S, NAME) VALUES (1, '�������������', '������������� �������')
GO

/****** ���������� ������ ������� ������������ �������� ******/

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (100, '����������� � �������', '�������� ����� ������������, ������ � ���� ������� � �������.', 1, -1, 8388608, 16777194)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (101, '����� ������', '����� ������ �������������.', 1, -1, 16384, 16777194)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (110, '������ �����������', '������� �������� ��� ������������, �������� ������ ��� ����� � �������.', 1, 0, 213, 16777194)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (200, '��������', '�������� ������ ��� ���������� ��� �������������� ���� �� ��������� ���� ������.', 0, -1, 16384, 15400938)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (300, '��������� ������������', '��������� ������� ������������ ������� - �������� � ��������������.', 0, -1, 16384, 15397375)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (400, '��������� ������ A', '��������� ������ ��������� ������� A.', 0, -1, 213, 15400959)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (410, '��������� ������ B', '��������� ������ ��������� ������� B.', 0, -1, 16512, 15400959)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (420, '��������� ������ C', '��������� ������ ��������� ������� C.', 0, -1, 128, 15400959)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (430, '��������� ������ D', '��������� ������ ��������� ������� D.', 0, -1, 8388608, 15400959)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (440, '��������� ������ E', '��������� ������ ��������� ������� E.', 0, -1, 16384, 15400959)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (480, '������ ��������', '������ �������� ��� ����������������� ��������� ������ � ��.', 0, -1, 16384, 15400938)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (490, '������� ��������', '���������� �������� ��� ��������� ������ � ��.', 0, -1, 16384, 15400938)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (500, '��������� ���������� ������������', '��������� ������ ��� ����������, �������� �� ������� ������������ �������.', 0, -1, 213, 15395583)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (510, '��������� ���������� ����������', '��������� ����������, �������� �� ���������������� ������� � �����.', 0, -1, 64, 15395583)
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (999, '������', '������, �������������� � ��������� ������ �������.', 1, 0, 65535, 8421631)
GO

/****** ���������� ������ �������� ******/

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9990, 100, '����������� � �������', '�������� ����� ������������, ������ � ���� ������� � �������.')
INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9991, 101, '����� ������', '����� ������ �������������.')
INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9995, 510, '���������� ��', '�������������� ���������� ���� ������.')
INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9998, 999, '������', '������ ���������������� ���������.')
INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9999, 110, '������ �����������', '������ ����������� ��� ������� �������������������� ������� � �������.')
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (8000, 200, '�������� ���������� �������', '�������� ���������� �������, �������� �� ��� ���� �������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 8000)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (8001, 510, '��������� ���������� �������', '��������� ���������� �������, �������� �� ��� ���� �������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 8001)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9000, 200, '�������� ����� �������', '�������� ����� ���� �� ���������� �������� � �������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9000)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9001, 500, '���������� �������� �������', '��������, ��������� � �������� ����� ���� �� ���������� �������� � �������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9001)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9100, 200, '�������� ������ �������������', '�������� ������ ������������� � ����������� �� ���� �� ���������� ��������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9100)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9101, 500, '���������� ������� �������������', '��������, ��������� � ���������� ������������� �������, ���������� �� ���� �� ���������� ��������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9101)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9102, 500, '���������� � ������������� �������������', '��������� � ������ ���������� �� ������ ������������� � �������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9102)
GO

-- INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9103, 800, '�������������� ������ ������� �������', '�������������� ����� ������ ������� ������� ������������� �������, ���������� ���������� ������ � ���� ������.') --
-- INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9103) --
-- GO --

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9104, 500, '�������������� ����� �������������', '��������, ��������� � �������� �������� ����� �������������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9104)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9200, 200, '�������� ���������� ������������', '�������� ����������, �������� �� ������� ������������ �������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9200)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9201, 500, '�������������� ���������� ������������', '�������������� ����������, �������� �� ������� ������������ �������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9201)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9210, 200, '�������� ��������� �������', '�������� ������� ���������� ��������� ������, �������� � ���� � ������ ������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9210)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9211, 500, '������� ��������� �������', '�������� ������� �� ���������� ��������� ������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9211)
GO

-- INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9212, 800, '�������������� ������ ��������� �������', '�������������� ����� ������ ��������� ������, ���������� ���������� ������ � ���� ������.') --
-- INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9212) --
-- GO --

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9220, 200, '�������� �������� ���� ���������', '�������� ����������� ������� ������������ ��������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9220)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9221, 300, '�������������� �������� ���� ���������', '��������� ����������� (� ����� �������������� �������� ����) ������� ������������ ��������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9221)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (9222, 200, '�������� ������ �������� �������', '�������� ����������� �������� �������.')
INSERT INTO SR_OPWLINKS (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9222)
GO

/****** �������� ����� �������� ******/

INSERT INTO SR_OPGROUPS (ID, NAME, NOTES) VALUES (1, '����������������� �������', '���������� ������� �� ���������� �������� � �������� ����, ���������� �������������� �������, ��������� ���������� ���������� �������.')
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 8000)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 8001)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9000)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9001)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9200)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9201)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9210)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9211)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9220)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9221)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9222)
GO

INSERT INTO SR_OPGROUPS (ID, NAME, NOTES) VALUES (2, '���������� �������������� �������', '���������� �������������� �������, �������������� ������������� ���� �� ���������� ��������.')
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (2, 9100)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (2, 9101)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (2, 9102)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (2, 9104)
GO

INSERT INTO SR_OPGROUPS (ID, NAME, NOTES) VALUES (3, '�������� ������ �������', '�������� ������ � �������� ���� �������.')
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 8000)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 9000)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 9100)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 9200)
INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 9210)
GO

-- INSERT INTO SR_OPGROUPS (ID, NAME, NOTES) VALUES (4, '�������������� �������', '�������������� ���� ������� Free Reports.')  --
-- INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (4, 9103)							--
-- INSERT INTO SR_OPGLINKS (ID_OPGROUPS, ID_OPERATIONS) VALUES (4, 9212)							--
-- GO																--

/****** ���������� ������ ������������� ******/
INSERT INTO SU_GROUPS VALUES (1, NULL, '��������������', '�������������� �������')
GO

INSERT INTO SU_USERS (ID, ID_GROUPS, NAME, FULLNAME, NOTES, PASSWORD, DELETED, BLOCKED, COUNT_EP) 
VALUES (0, 1, 'system', 'Database account', '', '', 1, 1, 0)
INSERT INTO SU_USERS (ID, ID_GROUPS, NAME, FULLNAME, NOTES, PASSWORD, DELETED, BLOCKED, COUNT_EP) 
VALUES (1, 1, 'admin', '������������� �������', '���������� ������� ������ �������������� �������.', '', 0, 0, 0)
GO

INSERT INTO SU_OPGLINKS (ID_USERS, ID_OPGROUPS) VALUES (1, 1)
INSERT INTO SU_OPGLINKS (ID_USERS, ID_OPGROUPS) VALUES (1, 2)
INSERT INTO SU_OPGLINKS (ID_USERS, ID_OPGROUPS) VALUES (1, 3)
GO

PRINT '�������� ���� ������ ���������� �������� ���������!'
GO

