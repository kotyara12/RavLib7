/**************************************************************************************************/
/*** �������� �� ��������� ************************************************************************/
/*** DEFAULTS *************************************************************************************/
/**************************************************************************************************/

CREATE DEFAULT BIT_FALSE AS 0
GO

CREATE DEFAULT BIT_TRUE AS 1
GO

CREATE DEFAULT clBlack AS 0
GO

CREATE DEFAULT clWhite AS 16777215
GO

CREATE DEFAULT INT_0 AS 0
GO

CREATE DEFAULT REAL_0 AS 0
GO

CREATE DEFAULT REAL_999999999 AS 999999999.99
GO

/**************************************************************************************************/
/*** ������ "�������������" ***********************************************************************/
/*** ADMINISTRATOR ********************************************************************************/
/**************************************************************************************************/

CREATE TABLE dbo.sr_levels (
  id INT NOT NULL ,
  name VARCHAR(64) NULL ,
  notes VARCHAR(255) NULL ,
  hidden BIT NOT NULL ,
  font_style INT NULL ,
  font_color INT NULL ,
  cell_color INT NULL ,
  CONSTRAINT pk_sr_levels_id PRIMARY KEY  CLUSTERED (id) 
)
GO

SETUSER 'dbo'
GO

EXEC sp_bindefault 'dbo.BIT_FALSE', 'sr_levels.hidden'
GO

EXEC sp_bindefault 'dbo.INT_0', 'sr_levels.font_style'
GO

EXEC sp_bindefault 'dbo.clBlack', 'sr_levels.font_color'
GO

EXEC sp_bindefault 'dbo.clWhite', 'sr_levels.cell_color'
GO

SETUSER
GO

GRANT  REFERENCES ,  SELECT ,  UPDATE  ON sr_levels  TO public
GO

CREATE TABLE dbo.sr_operations (
  id INT NOT NULL ,
  id_levels INT NOT NULL,  
  name VARCHAR(64) NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_sr_operations_id PRIMARY KEY  CLUSTERED (id),
  CONSTRAINT fk_sr_operations_levels FOREIGN KEY (id_levels) REFERENCES dbo.sr_levels (id)
)
GO

GRANT  REFERENCES ,  SELECT  ON sr_operations  TO public
GO

CREATE TABLE dbo.sr_opgroups (
  id INT NOT NULL ,
  name VARCHAR(64) NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_sr_opgroups_id PRIMARY KEY  CLUSTERED (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON sr_opgroups  TO public
GO

CREATE TABLE dbo.sr_workplases (
  id INT NOT NULL ,
  name_s VARCHAR(16) NULL ,
  name VARCHAR(128) NULL ,
  CONSTRAINT pk_sr_workplases_id PRIMARY KEY  CLUSTERED (id)
)
GO

GRANT  REFERENCES ,  SELECT  ON sr_workplases  TO public
GO

CREATE TABLE dbo.ss_blockids (
  name VARCHAR(32) NOT NULL ,
  id INT NOT NULL ,
  CONSTRAINT pk_ss_blockids_id PRIMARY KEY  CLUSTERED (name, id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON ss_blockids  TO public
GO

CREATE TABLE dbo.ss_vargroups (
  id INT NOT NULL ,
  owner_id INT NULL ,
  name VARCHAR(64) NOT NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_vargroups_id PRIMARY KEY  CLUSTERED (id)
)
GO

GRANT  REFERENCES ,  SELECT  ON ss_vargroups  TO public
GO

CREATE TABLE dbo.ss_varnames (
  id INT NOT NULL ,
  id_groups INT NOT NULL ,
  type INT NOT NULL ,
  def_value VARCHAR(255) NULL ,
  name VARCHAR(128) NOT NULL ,
  CONSTRAINT pk_ss_varnames_id PRIMARY KEY  CLUSTERED (id),
  CONSTRAINT fk_ss_varnames_groups FOREIGN KEY (id_groups) REFERENCES dbo.ss_vargroups (id)
)
GO

GRANT  REFERENCES ,  SELECT   ON ss_varnames  TO public
GO

CREATE TABLE dbo.ss_settings (
  id INT NOT NULL ,
  value_int INT NULL ,
  value_real REAL NULL ,
  value_char VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_settings_id PRIMARY KEY  CLUSTERED (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON ss_settings  TO public
GO

CREATE TABLE dbo.ss_syslog (
  [datetime] DATETIME NOT NULL ,
  id_operations INT NOT NULL ,
  id_users INT NOT NULL ,
  id_workplases INT NOT NULL ,
  info VARCHAR(255) NOT NULL ,
  host VARCHAR(32) NULL ,
  netuser VARCHAR(32) NULL ,
  CONSTRAINT pk_ss_syslog_log PRIMARY KEY  CLUSTERED (DATETIME, id_operations, id_users, info),
  CONSTRAINT fk_ss_syslog_oper FOREIGN KEY (id_operations) REFERENCES dbo.sr_operations (id),
  CONSTRAINT fk_ss_syslog_wp FOREIGN KEY (id_workplases) REFERENCES dbo.sr_workplases (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE  ON ss_syslog  TO public
GO

CREATE TABLE dbo.su_groups (
  id INT NOT NULL ,
  owner_id INT NULL ,
  name VARCHAR(64) NOT NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_su_groups_id PRIMARY KEY  CLUSTERED (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON su_groups  TO public
GO

CREATE TABLE dbo.su_users (
  id INT NOT NULL ,
  id_groups INT NOT NULL ,
  name VARCHAR(16) NOT NULL ,
  fullname VARCHAR(64) NULL ,
  notes VARCHAR(64) NULL ,
  password VARCHAR(255) NULL ,
  deleted BIT NOT NULL ,
  blocked BIT NOT NULL ,
  changed DATETIME NULL ,
  count_ep INT NULL ,
  CONSTRAINT pk_su_users_id PRIMARY KEY  CLUSTERED (id),
  CONSTRAINT uq_su_users_name UNIQUE  NONCLUSTERED (name),
  CONSTRAINT fk_su_users_groups FOREIGN KEY (id_groups) REFERENCES dbo.su_groups (id)
)
GO

SETUSER 'dbo'
GO

EXEC sp_bindefault 'dbo.BIT_FALSE', 'su_users.blocked'
GO

EXEC sp_bindefault 'dbo.BIT_FALSE', 'su_users.deleted'
GO

EXEC sp_bindefault 'dbo.INT_0', 'su_users.count_ep'
GO

SETUSER
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON su_users  TO public
GO

CREATE TABLE dbo.sr_opglinks (
  id_opgroups INT NOT NULL ,
  id_operations INT NOT NULL ,
  CONSTRAINT pk_sr_opglinks_id PRIMARY KEY  CLUSTERED (id_opgroups, id_operations),
  CONSTRAINT fk_sr_opglinks_oper FOREIGN KEY (id_operations) REFERENCES dbo.sr_operations (id),
  CONSTRAINT fk_sr_opglinks_grp FOREIGN KEY (id_opgroups) REFERENCES dbo.sr_opgroups (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON sr_opglinks  TO public
GO

CREATE TABLE dbo.sr_opwlinks (
  id_workplases INT NOT NULL ,
  id_operations INT NOT NULL ,
  CONSTRAINT pk_sr_opwlinks_id PRIMARY KEY  CLUSTERED (id_workplases, id_operations),
  CONSTRAINT fk_sr_opwlinks_wp FOREIGN KEY (id_workplases) REFERENCES dbo.sr_workplases (id),
  CONSTRAINT fk_sr_opwlinks_oper FOREIGN KEY (id_operations) REFERENCES dbo.sr_operations (id)
)
GO

GRANT  REFERENCES ,  SELECT  ON sr_opwlinks  TO public
GO

CREATE TABLE dbo.su_opglinks (
  id_users INT NOT NULL ,
  id_opgroups INT NOT NULL ,
  CONSTRAINT pk_su_opglinks_id PRIMARY KEY  CLUSTERED (id_users, id_opgroups),
  CONSTRAINT fk_su_opglinks_grp FOREIGN KEY (id_opgroups) REFERENCES dbo.sr_opgroups (id),
  CONSTRAINT fk_su_opglinks_user FOREIGN KEY (id_users) REFERENCES dbo.su_users (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON su_opglinks  TO public
GO

CREATE TABLE dbo.su_oprlinks (
  id_users INT NOT NULL ,
  id_operations INT NOT NULL ,
  CONSTRAINT pk_su_oprlinks_id PRIMARY KEY  CLUSTERED (id_users, id_operations),
  CONSTRAINT fk_su_oprlinks_oper FOREIGN KEY (id_operations) REFERENCES dbo.sr_operations (id),
  CONSTRAINT fk_su_oprlinks_user FOREIGN KEY (id_users) REFERENCES dbo.su_users (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON su_oprlinks  TO public
GO

/****** ���������� "���������" ********************************************************************/

CREATE TABLE dbo.sm_messages (
  id INT NOT NULL ,
  id_users INT NOT NULL ,
  sended DATETIME NOT NULL ,
  title VARCHAR(255) NULL ,
  address VARCHAR(255) NULL ,
  message TEXT NULL ,
  CONSTRAINT pk_sm_messages_id PRIMARY KEY  CLUSTERED (id),
  CONSTRAINT fk_sm_messages_users FOREIGN KEY (id_users) REFERENCES dbo.su_users (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON sm_messages  TO public
GO

CREATE TABLE dbo.sm_history (
  id_messages INT NOT NULL ,
  id_users INT NOT NULL ,
  opened DATETIME NULL ,
  closed DATETIME NULL ,
  CONSTRAINT pk_sm_history_id PRIMARY KEY  CLUSTERED (id_messages, id_users),
  CONSTRAINT fk_sm_history_msg FOREIGN KEY (id_messages) REFERENCES dbo.sm_messages (id),
  CONSTRAINT fk_sm_history_users FOREIGN KEY (id_users) REFERENCES dbo.su_users (id)
)
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON sm_history  TO public
GO

/****** ���������� "������" ***********************************************************************/

CREATE TABLE dbo.ss_reports (
  id INT NOT NULL ,
  form VARCHAR(32) NOT NULL ,
  report IMAGE NULL ,
  name VARCHAR(64) NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_reports_id PRIMARY KEY  CLUSTERED (id)
) 
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON ss_reports  TO public
GO

/****** ���������� "������������� �����" **********************************************************/

CREATE TABLE dbo.ss_attachments (
  id INT NOT NULL ,
  object_name VARCHAR(32) NOT NULL ,
  object_id INT NOT NULL ,
  filename VARCHAR(128) NULL ,
  filedata IMAGE NULL ,
  filetime DATETIME NULL ,
  filesize INT NULL ,
  CONSTRAINT pk_ss_attachments_id PRIMARY KEY  CLUSTERED (id)
)
GO

CREATE INDEX lk_ss_attachments_object_name ON dbo.ss_attachments (object_name)
GO

CREATE INDEX lk_ss_attachments_object_id ON dbo.ss_attachments (object_name, object_id);
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON ss_attachments  TO public
GO

/****** �������� �������� *************************************************************************/

CREATE VIEW vsr_operations_all AS
SELECT
  sr_operations.id, 
  sr_operations.id_levels,
  sr_operations.name,
  sr_operations.notes,
  sr_levels.name AS level_name,
  sr_levels.notes AS level_notes,
  sr_levels.font_style,
  sr_levels.font_color,
  sr_levels.cell_color
FROM sr_operations, sr_levels
WHERE sr_operations.id_levels = sr_levels.id
GO

GRANT  SELECT  ON dbo.vsr_operations_all  TO public
GO

CREATE VIEW vsr_operations_work AS
SELECT
  sr_operations.id, 
  sr_operations.id_levels,
  sr_operations.name,
  sr_operations.notes,
  sr_levels.name AS levels_name,
  sr_levels.notes AS levels_notes,
  sr_levels.font_style,
  sr_levels.font_color,
  sr_levels.cell_color
FROM sr_operations, sr_levels
WHERE (sr_operations.id_levels = sr_levels.id) 
  AND (sr_operations.id_levels IN (SELECT sr_levels.id FROM sr_levels WHERE sr_levels.hidden=0))
GO

GRANT  SELECT  ON dbo.vsr_operations_work  TO public
GO

CREATE VIEW vsr_operations_list AS
SELECT
  sr_operations.id, 
  sr_operations.name,
  sr_operations.notes
FROM sr_operations
WHERE sr_operations.id_levels IN (SELECT sr_levels.id FROM sr_levels WHERE sr_levels.hidden=0)
GO

GRANT  SELECT  ON dbo.vsr_operations_list  TO public
GO

CREATE VIEW vsr_operations_user AS
SELECT id_users, id_operations
FROM su_opglinks U LEFT JOIN sr_opglinks G ON U.id_opgroups=G.id_opgroups
GROUP BY id_users, id_operations
  UNION
SELECT id_users, id_operations
FROM su_oprlinks
GO

GRANT  REFERENCES ,  SELECT ON vsr_operations_user  TO public
GO

CREATE VIEW vss_syslog AS
SELECT 
  ss_syslog.[datetime], 
  ss_syslog.id_operations, 
  sr_operations.id_levels, 
  ss_syslog.id_users, 
  ss_syslog.id_workplases,
  info,
  host,
  netuser
FROM ss_syslog, sr_operations
WHERE ss_syslog.id_operations = sr_operations.id
GO

GRANT  SELECT ,  DELETE  ON dbo.vss_syslog  TO public
GO

CREATE VIEW vss_settings AS
SELECT
  ss_varnames.id,
  ss_varnames.id_groups,
  ss_varnames.type,
  ss_varnames.def_value,
  ss_settings.value_int,
  ss_settings.value_real,
  ss_settings.value_char,
  ss_varnames.name
FROM ss_varnames LEFT JOIN ss_settings ON ss_varnames.id = ss_settings.id
GO

GRANT  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON dbo.vss_settings  TO public
GO

/****** �������� ��������� ************************************************************************/

CREATE TRIGGER sr_operations_delete ON dbo.sr_operations 
FOR DELETE 
AS
BEGIN
  SET NOCOUNT ON
  DELETE FROM sr_opglinks WHERE sr_opglinks.id_operations IN
   (SELECT id FROM deleted)
  DELETE FROM sr_opwlinks WHERE sr_opwlinks.id_operations IN
   (SELECT id FROM deleted)
  DELETE FROM su_oprlinks WHERE su_oprlinks.id_operations IN
   (SELECT id FROM deleted)
  SET NOCOUNT OFF
END
GO

CREATE TRIGGER sr_opgroups_delete ON dbo.sr_opgroups 
FOR DELETE 
AS
BEGIN
  SET NOCOUNT ON
  DELETE FROM sr_opglinks WHERE sr_opglinks.id_opgroups IN
   (SELECT id FROM deleted)
  DELETE FROM su_opglinks WHERE su_opglinks.id_opgroups IN
   (SELECT id FROM deleted)
  SET NOCOUNT OFF
END
GO 

CREATE TRIGGER sr_workplases_delete ON dbo.sr_workplases 
FOR DELETE 
AS
BEGIN
  SET NOCOUNT ON
  DELETE FROM sr_opwlinks WHERE sr_opwlinks.id_workplases IN
   (SELECT id FROM deleted)
  SET NOCOUNT OFF
END
GO

CREATE TRIGGER su_users_delete ON dbo.su_users 
FOR DELETE 
AS
BEGIN
  IF (SELECT ss_settings.value_int FROM ss_settings WHERE ss_settings.id = 9000) <> 1
  BEGIN
    ROLLBACK TRAN
    RAISERROR ('�������� ������������� ��������� ��������� ������������ �������!', 16, 10)
  END
  else BEGIN
    SET NOCOUNT ON
    DELETE FROM su_opglinks WHERE su_opglinks.id_users IN
     (SELECT deleted.id FROM deleted)
    DELETE FROM su_oprlinks WHERE su_oprlinks.id_users IN
     (SELECT deleted.id FROM deleted)
    SET NOCOUNT OFF
  END
END
GO

/****** �������� ��������� �������� ***************************************************************/

--- ��������� ������ ���� ������ (��� ������������� � �������� UPDATE) ---
CREATE PROCEDURE sp_sys_ch_db_version (@ver INT) AS
BEGIN
  UPDATE ss_settings SET value_int=@ver WHERE id=1000
END
GO

--- ���������� ������ � �������� ������ ������� ---
CREATE PROCEDURE sp_sys_add_log(@id_workplases INT, @id_users INT, @id_operations INT,
  @host VARCHAR(32), @netuser VARCHAR(32), @info VARCHAR(255)) 
AS BEGIN
  IF (SELECT value_int FROM ss_settings WHERE id=9001)=1
    INsert INTO ss_syslog (DATETIME, id_operations, id_users, id_workplases, info, host, netuser) 
      values (GetDate(), @id_operations, @id_users, @id_workplases, @info, @host, @netuser)
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_add_log  TO public
GO

--- ���������� �������� � ������ ������� ----
CREATE PROCEDURE sp_sys_group_add_operation (@id_opgroups INT, @id_operations INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  INsert INTO sr_opglinks (id_opgroups, id_operations) values (@id_opgroups, @id_operations)
  --- ��������� ������ � �������� ---
  DECLARE @message VARCHAR(255)
  SELECT @message = '"������ �������" [sr_opglinks]: �������� id="' 
    + Convert(VARCHAR(16), @id_operations) 
    + '", name="' + (SELECT name FROM sr_operations WHERE id=@id_operations) 
    + '" �������� � ������ ������� id="' 
    + Convert(VARCHAR(16), @id_opgroups) 
    + '", name="' + (SELECT name FROM sr_opgroups WHERE id=@id_opgroups) 
    + '". [sp_sys_group_add_operation]'
  EXEC sp_sys_add_log @id_workplases, @id_users, 9001, @host, @netuser, @message
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_group_add_operation  TO public
GO

--- �������� �������� �� ������ ������� ----
CREATE PROCEDURE sp_sys_group_del_operation (@id_opgroups INT, @id_operations INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  IF (SELECT count(*) FROM sr_opglinks WHERE id_opgroups=@id_opgroups AND id_operations=@id_operations) > 0 
  BEGIN
    DELETE FROM sr_opglinks WHERE id_opgroups=@id_opgroups AND id_operations=@id_operations
    --- ��������� ������ � �������� ---
    DECLARE @message VARCHAR(255)
    SELECT @message = '"������ �������" [sr_opglinks]: �������� id="' 
      + Convert(VARCHAR(16), @id_operations) 
      + '", name="' + (SELECT name FROM sr_operations WHERE id=@id_operations) 
      + '" ��������� �� ������ ������� id="' 
      + Convert(VARCHAR(16), @id_opgroups) 
      + '", name="' + (SELECT name FROM sr_opgroups WHERE id=@id_opgroups) 
      + '". [sp_sys_group_del_operation]'
    EXEC sp_sys_add_log @id_workplases, @id_users, 9001, @host, @netuser, @message
  END
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_group_del_operation  TO public
GO

--- ���������� �������� ������������ ----
CREATE PROCEDURE sp_sys_user_add_operation (@id_sysusers INT, @id_operations INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  INsert INTO su_oprlinks (id_users, id_operations) values (@id_sysusers, @id_operations)
  --- ��������� ������ � �������� ---
  DECLARE @message VARCHAR(255)
  SELECT @message = '"������������ �������" [su_oprlinks]: �������� id="' 
    + Convert(VARCHAR(16), @id_operations) 
    + '", name="' + (SELECT name FROM sr_operations WHERE id=@id_operations) 
    + '" �������� � ������ ������������ id="' 
    + Convert(VARCHAR(16), @id_sysusers) 
    + '", name="' + (SELECT name FROM su_users WHERE id=@id_sysusers) 
    + '", fullname="' + (SELECT fullname FROM su_users WHERE id=@id_sysusers) 
    + '". [sp_sys_user_add_operation]'
  EXEC sp_sys_add_log @id_workplases, @id_users, 9001, @host, @netuser, @message
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_user_add_operation  TO public
GO

--- �������� �������� � ������������ ----
CREATE PROCEDURE sp_sys_user_del_operation (@id_sysusers INT, @id_operations INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  IF (SELECT count(*) FROM su_oprlinks WHERE id_users=@id_sysusers AND id_operations=@id_operations) > 0 
  BEGIN
    DELETE FROM su_oprlinks WHERE id_users=@id_sysusers AND id_operations=@id_operations
    --- ��������� ������ � �������� ---
    DECLARE @message VARCHAR(255)
    SELECT @message = '"������������ �������" [su_oprlinks]: �������� id="' 
      + Convert(VARCHAR(16), @id_operations) 
      + '", name="' + (SELECT name FROM sr_operations WHERE id=@id_operations) 
      + '" ��������� �� ������ ������������ id="' 
      + Convert(VARCHAR(16), @id_sysusers) 
      + '", name="' + (SELECT name FROM su_users WHERE id=@id_sysusers) 
      + '", fullname="' + (SELECT fullname FROM su_users WHERE id=@id_sysusers) 
      + '". [sp_sys_user_del_operation]'
    EXEC sp_sys_add_log @id_workplases, @id_users, 9001, @host, @netuser, @message
  END
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_user_del_operation  TO public
GO

--- ���������� ������ �������� ������������ ----
CREATE PROCEDURE sp_sys_user_add_opgroup(@id_sysusers INT, @id_opgroups INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  INsert INTO su_opglinks (id_users, id_opgroups) values (@id_sysusers, @id_opgroups)
  --- ��������� ������ � �������� ---
  DECLARE @message VARCHAR(255)
  SELECT @message = '"������������ �������" [su_opglinks]: ������ ������� id="' 
    + Convert(VARCHAR(16), @id_opgroups) 
    + '", name="' + (SELECT name FROM sr_opgroups WHERE id=@id_opgroups) 
    + '" �������� � ������ ������������ id="' 
    + Convert(VARCHAR(16), @id_sysusers) 
    + '", name="' + (SELECT name FROM su_users WHERE id=@id_sysusers) 
    + '", fullname="' + (SELECT fullname FROM su_users WHERE id=@id_sysusers) 
    + '". [sp_sys_user_add_opgroup]'
  EXEC sp_sys_add_log @id_workplases, @id_users, 9001, @host, @netuser, @message
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_user_add_opgroup  TO public
GO

--- �������� ������ �������� � ������������ ----
CREATE PROCEDURE sp_sys_user_del_opgroup(@id_sysusers INT, @id_opgroups INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  IF (SELECT count(*) FROM su_opglinks WHERE id_users=@id_sysusers AND id_opgroups=@id_opgroups) > 0 
  BEGIN
    DELETE FROM su_opglinks WHERE id_users=@id_sysusers AND id_opgroups=@id_opgroups
    --- ��������� ������ � �������� ---
    DECLARE @message VARCHAR(255)
    SELECT @message = '"������������ �������" [su_opglinks]: ������ ������� id="' 
      + Convert(VARCHAR(16), @id_opgroups) 
      + '", name="' + (SELECT name FROM sr_opgroups WHERE id=@id_opgroups) 
      + '" ��������� �� ������ ������������ id="' 
      + Convert(VARCHAR(16), @id_sysusers) 
      + '", name="' + (SELECT name FROM su_users WHERE id=@id_sysusers) 
      + '", fullname="' + (SELECT fullname FROM su_users WHERE id=@id_sysusers) 
      + '". [sp_sys_user_del_opgroup]'
    EXEC sp_sys_add_log @id_workplases, @id_users, 9001, @host, @netuser, @message
  END
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_user_del_opgroup  TO public
GO

--- �������� ������ �������� � ���� ������������� ������� (����� ���������) ---
CREATE PROCEDURE sp_sys_group_unlink_users (@id_opgroups INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  DECLARE @id_sysusers INT
  DECLARE cs_opglinks CURSOR FOR SELECT id_users FROM su_opglinks
  OPEN cs_opglinks
  FETCH cs_opglinks INTO @id_sysusers
  WHILE @@FETCH_STATUS = 0
  BEGIN
    EXEC sp_sys_user_del_opgroup @id_sysusers, @id_opgroups, @id_workplases, @id_users, @host, @netuser
    FETCH NEXT FROM cs_opglinks INTO @id_sysusers
  END
  CLOSE cs_opglinks
  DEALLOCATE cs_opglinks
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_group_unlink_users  TO public
GO

--- �������� �� ������ ���� �������� ---
CREATE PROCEDURE sp_sys_group_unlink_opers (@id_opgroups INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  DECLARE @id_operations INT
  DECLARE cs_opglinks CURSOR FOR SELECT id_operations FROM sr_opglinks
  OPEN cs_opglinks
  FETCH cs_opglinks INTO @id_operations
  WHILE @@FETCH_STATUS = 0
  BEGIN
    EXEC sp_sys_group_del_operation @id_opgroups, @id_operations, @id_workplases, @id_users, @host, @netuser
    FETCH NEXT FROM cs_opglinks INTO @id_operations
  END
  CLOSE cs_opglinks
  DEALLOCATE cs_opglinks
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_group_unlink_opers  TO public
GO

--- "�������" ������ ������� ---
CREATE PROCEDURE sp_sys_group_unlink_all (@id_opgroups INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  EXEC sp_sys_group_unlink_users @id_opgroups, @id_workplases, @id_users, @host, @netuser
  EXEC sp_sys_group_unlink_opers @id_opgroups, @id_workplases, @id_users, @host, @netuser
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_group_unlink_all  TO public
GO

--- �������� � ������������ ���� �������� ---
CREATE PROCEDURE sp_sys_user_unlink_opers (@id_sysusers INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  DECLARE @id_operations INT
  DECLARE cs_oprlinks CURSOR FOR SELECT id_operations FROM su_oprlinks
  OPEN cs_oprlinks
  FETCH cs_oprlinks INTO @id_operations
  WHILE @@FETCH_STATUS = 0
  BEGIN
    EXEC sp_sys_user_del_operation @id_sysusers, @id_operations, @id_workplases, @id_users, @host, @netuser
    FETCH NEXT FROM cs_oprlinks INTO @id_operations
  END
  CLOSE cs_oprlinks
  DEALLOCATE cs_oprlinks
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_user_unlink_opers  TO public
GO

--- �������� � ������������ ���� ����� �������� ---
CREATE PROCEDURE sp_sys_user_unlink_groups(@id_sysusers INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  DECLARE @id_opgroups INT
  DECLARE cs_opglinks CURSOR FOR SELECT id_opgroups FROM su_opglinks
  OPEN cs_opglinks
  FETCH cs_opglinks INTO @id_opgroups
  WHILE @@FETCH_STATUS = 0
  BEGIN
    EXEC sp_sys_user_del_opgroup @id_sysusers, @id_opgroups, @id_workplases, @id_users, @host, @netuser
    FETCH NEXT FROM cs_opglinks INTO @id_opgroups
  END
  CLOSE cs_opglinks
  DEALLOCATE cs_opglinks
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_user_unlink_groups  TO public
GO

--- "�������" ������������ ---
CREATE PROCEDURE sp_sys_user_unlink_all (@id_sysusers INT,
  @id_workplases INT, @id_users INT, @host VARCHAR(32), @netuser VARCHAR(32))
AS BEGIN
  EXEC sp_sys_user_unlink_opers @id_sysusers, @id_workplases, @id_users, @host, @netuser
  EXEC sp_sys_user_unlink_groups @id_sysusers, @id_workplases, @id_users, @host, @netuser
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_user_unlink_all  TO public
GO

/**************************************************************************************************/
/*** ���������� ������ ****************************************************************************/
/*** APPLICATION **********************************************************************************/
/**************************************************************************************************/


/**************************************************************************************************/
/*** ��������� ������� ****************************************************************************/
/*** PARAMS ***************************************************************************************/
/**************************************************************************************************/

/*** ������ ���������� ****************************************************************************/

INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (1, NULL, '��� ���������', NULL)
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (2, 1, '���� ������', '��������� ���� ������')
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (3, 1, '���������� ������������', '��������� ���������� ������������')
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (4, 3, '�������� �������', '��������� ������� �������������')
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (5, 3, '������ ������', '��������� ������� ������ �������')
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (6, 1, '������������� �����', '��������� ���������� �������������� ������')
GO

/*** ��������� ************************************************************************************/

INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (1000, 2, 0, NULL, '������� ������ ���� ������')
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (1002, 2, 1, '90', '���� �������� �������� ���������, ����')
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (1102, 6, 1, '1024', '������ �������� � ����������, ����� �������� ������������� �������������')
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (1103, 6, 1, '10240', '����������� ���������� ������ �������� � ����������')
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9001, 5, 6, '��������', '��������� ������� ������ �������')
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9002, 5, 1, '1000000', '����� �������������� � ���������� �������, �������')
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9003, 4, 1, '40', '�������� � ���� ����� ��������� ������ ������')
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9004, 4, 1, '6', '���������� ���������� ����� ������ ������������ � ��������')
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9005, 5, 1, '365', '������ �������� ������� ������� ������ �������, ����')
GO

-- Note: �������� 1000 ����������� �� �����, � � ����� �������� --
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (1002, 90, NULL, NULL)        
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (1102, 1024, NULL, NULL)        
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (1103, 10240, NULL, NULL)        
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9000, 0, NULL, NULL)
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9001, 1, NULL, NULL)
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9002, 1000000, NULL, NULL)
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9003, 40, NULL, NULL)
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9004, 6, NULL, NULL)
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9005, 0, NULL, NULL)
GO

/**************************************************************************************************/
/*** �������� ������� ���� ������� ****************************************************************/
/*** WORKPLASES ***********************************************************************************/
/**************************************************************************************************/

INSERT INTO sr_workplases (id, name_s, name) VALUES (1, '�������������', '������������� �������')
GO

/**************************************************************************************************/
/*** ������� ������������ �������� ****************************************************************/
/*** sr_levels ************************************************************************************/
/**************************************************************************************************/

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (100, '����������� � �������', '�������� ����� ������������, ������ � ���� ������� � �������.', 1, -1, 8388608, 16777194)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (101, '����� ������', '����� ������ �������������.', 1, -1, 16384, 16777194)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (110, '������ �����������', '������� �������� ��� ������������, �������� ������ ��� ����� � �������.', 1, 0, 213, 16777194)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (150, '������ ���������', '������ �������� ���������, ������������ ������� ��������������.', 0, -1, 8388608, 16777194)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (151, '�������� ���������', '�������� �������� ��������� ��� ������ ������������� �������.', 0, -1, 8388608, 16777194)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (200, '��������', '�������� ������ ��� ���������� ��� �������������� ���� �� ��������� ���� ������.', 0, -1, 16384, 15400938)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (300, '��������� ������������', '��������� ������� ������������ ������� - �������� � ��������������.', 0, -1, 16384, 15397375)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (400, '��������� ������ A', '��������� ������ ��������� ������� A.', 0, -1, 213, 15400959)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (410, '��������� ������ B', '��������� ������ ��������� ������� B.', 0, -1, 16512, 15400959)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (420, '��������� ������ C', '��������� ������ ��������� ������� C.', 0, -1, 128, 15400959)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (430, '��������� ������ D', '��������� ������ ��������� ������� D.', 0, -1, 8388608, 15400959)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (440, '��������� ������ E', '��������� ������ ��������� ������� E.', 0, -1, 16384, 15400959)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (480, '������ ��������', '������ �������� ��� ����������������� ��������� ������ � ��.', 0, -1, 16384, 15400938)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (490, '������� ��������', '���������� �������� ��� ��������� ������ � ��.', 0, -1, 16384, 15400938)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (500, '��������� ���������� ������������', '��������� ������ ��� ����������, �������� �� ������� ������������ �������.', 0, -1, 213, 15395583)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (510, '��������� ���������� ����������', '��������� ����������, �������� �� ���������������� ������� � �����.', 0, -1, 64, 15395583)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (800, '�������������� �������', '�������� ��� ��������� �������� ����, ������������ ��� ���������� �������.', 0, -1, 8388608, 15400938)
GO

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) 
VALUES (999, '������', '������, �������������� � ��������� ������ �������.', 1, 0, 65535, 8421631)
GO

/**************************************************************************************************/
/*** �������� *************************************************************************************/
/*** OPERATIONS ***********************************************************************************/
/**************************************************************************************************/

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9990, 100, '����������� � �������', '�������� ����� ������������, ������ � ���� ������� � �������.')
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9991, 101, '����� ������', '����� ������ �������������.')
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9995, 510, '���������� ��', '�������������� ���������� ���� ������.')
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9998, 999, '������', '������ ���������������� ���������.')
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9999, 110, '������ �����������', '������ ����������� ��� ������� �������������������� ������� � �������.')
GO

/*** ������ "�������������" ***********************************************************************/

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (8000, 200, '�������� ���������� �������', '�������� ���������� �������, �������� �� ��� ���� �������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 8000)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (8001, 510, '��������� ���������� �������', '��������� ���������� �������, �������� �� ��� ���� �������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 8001)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (8201, 800, '�������������� �������', '�������� ��� ��������� �������� ����, ������������ ��� ���������� �������.')
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9000, 200, '�������� ����� �������', '�������� ����� ���� �� ���������� �������� � �������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9000)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9001, 500, '���������� �������� �������', '��������, ��������� � �������� ����� ���� �� ���������� �������� � �������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9001)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9100, 200, '�������� ������ �������������', '�������� ������ ������������� � ����������� �� ���� �� ���������� ��������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9100)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9101, 500, '���������� ������� �������������', '��������, ��������� � ���������� ������������� �������, ���������� �� ���� �� ���������� ��������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9101)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9102, 500, '���������� � ������������� �������������', '��������� � ������ ���������� �� ������ ������������� � �������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9102)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9104, 500, '�������������� ����� �������������', '��������, ��������� � �������� �������� ����� �������������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9104)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9200, 200, '�������� ���������� ������������', '�������� ����������, �������� �� ������� ������������ �������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9200)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9201, 500, '�������������� ���������� ������������', '�������������� ����������, �������� �� ������� ������������ �������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9201)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9210, 200, '�������� ��������� �������', '�������� ������� ���������� ��������� ������, �������� � ���� � ������ ������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9210)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9211, 500, '������� ��������� �������', '�������� ������� �� ���������� ��������� ������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9211)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9220, 200, '�������� �������� ���� ���������', '�������� ����������� ������� ������������ ��������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9220)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9221, 300, '�������������� �������� ���� ���������', '��������� ����������� (� ����� �������������� �������� ����) ������� ������������ ��������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9221)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9222, 200, '�������� ������ �������� �������', '�������� ����������� �������� �������.')
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9222)
GO

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9980, 150, '������ ���������', '������ �������� ���������, ������������ ������� ��������������.')
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9981, 151, '�������� ���������', '�������� �������� ��������� ��� ������ ������������� �������.')
GO

/**************************************************************************************************/
/*** ���� *****************************************************************************************/
/*** ROLES ****************************************************************************************/
/**************************************************************************************************/

/*** ������ "�������������" ***********************************************************************/

INSERT INTO sr_opgroups (id, name, notes) VALUES (1, '����������������� �������', '���������� ������� �� ���������� �������� � �������� ����, ���������� �������������� �������, ��������� ���������� ���������� �������.')
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 8000)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 8001)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9000)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9001)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9200)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9201)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9210)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9211)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9220)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9221)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9222)
GO

INSERT INTO sr_opgroups (id, name, notes) VALUES (2, '���������� �������������� �������', '���������� �������������� �������, �������������� ������������� ���� �� ���������� ��������.')
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (2, 9100)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (2, 9101)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (2, 9102)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (2, 9104)
GO

INSERT INTO sr_opgroups (id, name, notes) VALUES (3, '�������� ������ �������', '�������� ������ � �������� ���� �������.')
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 8000)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 9000)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 9100)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 9200)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 9210)
GO

INSERT INTO sr_opgroups (id, name, notes) VALUES (4, '���������', '����� � �������� ��������� ������ �������.')
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (4, 9980)
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (4, 9981)
GO

INSERT INTO sr_opgroups (id, name, notes) VALUES (5, '�������������� �������', '�������������� �������� ������� FASt Reports.')
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (5, 8201)
GO

/**************************************************************************************************/
/*** ������������**********************************************************************************/
/*** USERS ****************************************************************************************/
/**************************************************************************************************/

INSERT INTO su_groups VALUES (1, NULL, '��� ������������', NULL)
INSERT INTO su_groups VALUES (2, 1, '��������������', '�������������� �������')
INSERT INTO su_groups VALUES (3, 1, '������������', '������������ �������')
GO

INSERT INTO su_users (id, id_groups, name, fullname, notes, password, deleted, blocked, count_ep) 
VALUES (0, 1, 'system', 'DatabASe account', '', '', 1, 1, 0)
INSERT INTO su_users (id, id_groups, name, fullname, notes, password, deleted, blocked, count_ep) 
VALUES (1, 2, 'admin', '������������� �������', '���������� ������� ������ �������������� �������.', '', 0, 0, 0)
GO

INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 1)
INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 2)
INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 3)
INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 4)
INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 5)
GO


/**************************************************************************************************/
/*** ���������� ������ ****************************************************************************/
/*** DbVersion ************************************************************************************/
/**************************************************************************************************/

INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (1000, 1, NULL, NULL)
GO

PRINT '�������� ���� ������ ���������!'
GO

