DROP VIEW vss_syslog;
DROP TABLE ss_syslog;
DROP PROCEDURE sp_sys_add_log;
DROP PROCEDURE sp_sys_user_add_opgroup;
DROP PROCEDURE sp_sys_user_del_opgroup;


CREATE TABLE ss_syslog (
  uid INT NOT NULL AUTO_INCREMENT,
  dateoper TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  id_operations INT NOT NULL ,
  id_users INT NOT NULL ,
  id_workplases INT NOT NULL ,
  info TEXT NULL ,
  host VARCHAR(32) NULL ,
  netuser VARCHAR(32) NULL ,
  CONSTRAINT pk_ss_syslog_key PRIMARY KEY (uid),
  CONSTRAINT fk_ss_syslog_oper FOREIGN KEY (id_operations) REFERENCES sr_operations (id),
  CONSTRAINT fk_ss_syslog_wp FOREIGN KEY (id_workplases) REFERENCES sr_workplases (id)
);

CREATE VIEW vss_syslog AS
SELECT 
  ss_syslog.dateoper, 
  ss_syslog.id_operations, 
  sr_operations.id_levels, 
  ss_syslog.id_users, 
  ss_syslog.id_workplases,
  ss_syslog.info,
  ss_syslog.host,
  ss_syslog.netuser
FROM ss_syslog LEFT JOIN sr_operations ON ss_syslog.id_operations = sr_operations.id;

DELIMITER \\

CREATE PROCEDURE sp_sys_add_log (id_wps INT, id_usr INT, id_opr INT, hostname VARCHAR(32), netuser VARCHAR(32), info TEXT) 
BEGIN
  IF (SELECT IfNull(value_int, 0) FROM ss_settings WHERE id=9001) = 1 THEN
    INSERT INTO ss_syslog (id_operations, id_users, id_workplases, info, host, netuser) 
      VALUES (id_opr, id_usr, id_wps, info, hostname, netuser);
  END IF;
END
\\

CREATE PROCEDURE sp_sys_user_add_opgroup (id_sus INT, id_opg INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (id_sus, id_opg);
  
  /** Добавляем запись в протокол  **/
  SET @message = Concat('"Пользователи системы" [su_oprlinks]: Группа доступа id="', id_opg,
    '", name="', (SELECT IfNull(name, '') FROM sr_opgroups WHERE id=id_opg),
    '" включена в список пользователя id="', id_sus,
    '", name="', (SELECT IfNull(name, '') FROM su_users WHERE id=id_sus),
    '", fullname="', (SELECT IfNull(fullname, '') FROM su_users WHERE id=id_sus),
    '". [sp_sys_user_add_opgroup]');
  CALL sp_sys_add_log (id_wps, id_usr, 9001, hostname, netuser, @message);
END
\\

CREATE PROCEDURE sp_sys_user_del_opgroup (id_sus INT, id_opg INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  IF exists (SELECT * FROM su_opglinks WHERE id_users=id_sus AND id_opgroups=id_opg) THEN
  BEGIN
    DELETE FROM su_opglinks WHERE id_users=id_sus AND id_opgroups=id_opg;
    
    /** Добавляем запись в протокол  **/
    SET @message = Concat('"Пользователи системы" [su_oprlinks]: Группа доступа id="', id_opg,
      '", name="', (SELECT IfNull(name, '') FROM sr_opgroups WHERE id=id_opg),
      '" исключена в список пользователя id="', id_sus,
      '", name="', (SELECT IfNull(name, '') FROM su_users WHERE id=id_sus),
      '", fullname="', (SELECT IfNull(fullname, '') FROM su_users WHERE id=id_sus),
      '". [sp_sys_user_del_opgroup]');
    CALL sp_sys_add_log (id_wps, id_usr, 9001, hostname, netuser, @message);
  END; 
  END IF;
END
\\

DELIMITER ;

INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9001, 5, 6, 'Включено', 'Состояние журнала аудита системы');