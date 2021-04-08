DROP VIEW vss_syslog
DROP TABLE dbo.ss_syslog
DROP PROCEDURE sp_sys_add_log
GO

CREATE TABLE dbo.ss_syslog (
  uid UNIQUEIDENTIFIER NOT NULL,
  dateoper DATETIME NOT NULL ,
  id_operations INT NOT NULL ,
  id_users INT NOT NULL ,
  id_workplases INT NOT NULL ,
  info TEXT NULL ,
  host VARCHAR(32) NULL ,
  netuser VARCHAR(32) NULL ,
  CONSTRAINT pk_ss_syslog_key PRIMARY KEY  CLUSTERED (uid),
  CONSTRAINT fk_ss_syslog_oper FOREIGN KEY (id_operations) REFERENCES dbo.sr_operations (id),
  CONSTRAINT fk_ss_syslog_wp FOREIGN KEY (id_workplases) REFERENCES dbo.sr_workplases (id)
)
GO

ALTER TABLE dbo.ss_syslog ADD CONSTRAINT df_ss_syslog_id DEFAULT (NewId()) FOR uid
GO

ALTER TABLE dbo.ss_syslog ADD CONSTRAINT df_ss_syslog_time DEFAULT (GetDate()) FOR dateoper
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE  ON ss_syslog  TO public
GO

CREATE VIEW vss_syslog AS
SELECT 
  ss_syslog.dateoper, 
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

CREATE PROCEDURE sp_sys_add_log (@id_workplases INT, @id_users INT, @id_operations INT,
  @host VARCHAR(32), @netuser VARCHAR(32), @info TEXT) 
AS BEGIN
  IF (SELECT IsNull(value_int, 0) FROM ss_settings WHERE id=9001)=1
    INSERT INTO ss_syslog (id_operations, id_users, id_workplases, info, host, netuser) 
      VALUES (@id_operations, @id_users, @id_workplases, @info, @host, @netuser)
END
GO

GRANT  EXECUTE  ON dbo.sp_sys_add_log  TO public
GO

IF NOT EXISTS(SELECT * FROM ss_varnames WHERE id=9001) INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9001, 5, 6, 'Включено', 'Состояние журнала аудита системы')
GO