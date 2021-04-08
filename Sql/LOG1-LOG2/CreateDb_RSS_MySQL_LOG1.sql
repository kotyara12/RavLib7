/**** SYSTEM TABLES ****/

CREATE TABLE ss_blockids (
  name VARCHAR(32) NOT NULL ,
  id INT NOT NULL ,
  CONSTRAINT pk_ss_blockids_id PRIMARY KEY (name, id)
);

CREATE TABLE sr_levels (
  id INT NOT NULL ,
  name VARCHAR(64) NULL ,
  notes VARCHAR(255) NULL ,
  hidden BIT NOT NULL ,
  font_style INT NULL ,
  font_color INT NULL ,
  cell_color INT NULL ,
  CONSTRAINT pk_sr_levels_id PRIMARY KEY (id) 
);

CREATE TABLE sr_operations (
  id INT NOT NULL ,
  id_levels INT NOT NULL,  
  name VARCHAR(64) NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_sr_operations_id PRIMARY KEY (id),
  CONSTRAINT fk_sr_operations_levels FOREIGN KEY (id_levels) REFERENCES sr_levels (id)
);

CREATE TABLE sr_opgroups (
  id INT NOT NULL ,
  name VARCHAR(64) NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_sr_opgroups_id PRIMARY KEY (id)
);

CREATE TABLE sr_workplases (
  id INT NOT NULL ,
  name_s VARCHAR(16) NULL ,
  name VARCHAR(128) NULL ,
  CONSTRAINT pk_sr_workplases_id PRIMARY KEY (id)
);

CREATE TABLE ss_vargroups (
  id INT NOT NULL ,
  owner_id INT NULL ,
  name VARCHAR(64) NOT NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_vargroups_id PRIMARY KEY (id)
);

CREATE TABLE ss_varnames (
  id INT NOT NULL ,
  id_groups INT NOT NULL ,
  type INT NOT NULL ,
  def_value VARCHAR(255) NULL ,
  name VARCHAR(128) NOT NULL ,
  CONSTRAINT pk_ss_varnames_id PRIMARY KEY (id),
  CONSTRAINT fk_ss_varnames_groups FOREIGN KEY (id_groups) REFERENCES ss_vargroups (id)
);

CREATE TABLE ss_settings (
  id INT NOT NULL ,
  value_int INT NULL ,
  value_real REAL NULL ,
  value_char VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_settings_id PRIMARY KEY (id)
);

CREATE TABLE ss_syslog (
  datetime DATETIME NOT NULL ,
  id_operations INT NOT NULL ,
  id_users INT NOT NULL ,
  id_workplases INT NOT NULL ,
  info VARCHAR(255) NOT NULL ,
  host VARCHAR(32) NULL ,
  netuser VARCHAR(32) NULL ,
  CONSTRAINT pk_ss_syslog_log PRIMARY KEY (datetime, id_operations, id_users, info),
  CONSTRAINT fk_ss_syslog_oper FOREIGN KEY (id_operations) REFERENCES sr_operations (id),
  CONSTRAINT fk_ss_syslog_wp FOREIGN KEY (id_workplases) REFERENCES sr_workplases (id)
);

CREATE TABLE su_groups (
  id INT NOT NULL ,
  owner_id INT NULL ,
  name VARCHAR(64) NOT NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_su_groups_id PRIMARY KEY (id)
);

CREATE TABLE su_users (
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
  CONSTRAINT pk_su_users_id PRIMARY KEY (id),
  CONSTRAINT uq_su_users_name UNIQUE (name),
  CONSTRAINT fk_su_users_groups FOREIGN KEY (id_groups) REFERENCES su_groups (id)
);

CREATE TABLE sr_opglinks (
  id_opgroups INT NOT NULL ,
  id_operations INT NOT NULL ,
  CONSTRAINT pk_sr_opglinks_id PRIMARY KEY (id_opgroups, id_operations),
  CONSTRAINT fk_sr_opglinks_oper FOREIGN KEY (id_operations) REFERENCES sr_operations (id),
  CONSTRAINT fk_sr_opglinks_grp FOREIGN KEY (id_opgroups) REFERENCES sr_opgroups (id)
);

CREATE TABLE sr_opwlinks (
  id_workplases INT NOT NULL ,
  id_operations INT NOT NULL ,
  CONSTRAINT pk_sr_opwlinks_id PRIMARY KEY (id_workplases, id_operations),
  CONSTRAINT fk_sr_opwlinks_wp FOREIGN KEY (id_workplases) REFERENCES sr_workplases (id),
  CONSTRAINT fk_sr_opwlinks_oper FOREIGN KEY (id_operations) REFERENCES sr_operations (id)
);

CREATE TABLE su_opglinks (
  id_users INT NOT NULL ,
  id_opgroups INT NOT NULL ,
  CONSTRAINT pk_su_opglinks_id PRIMARY KEY (id_users, id_opgroups),
  CONSTRAINT fk_su_opglinks_grp FOREIGN KEY (id_opgroups) REFERENCES sr_opgroups (id),
  CONSTRAINT fk_su_opglinks_user FOREIGN KEY (id_users) REFERENCES su_users (id)
);

CREATE TABLE su_oprlinks (
  id_users INT NOT NULL ,
  id_operations INT NOT NULL ,
  CONSTRAINT pk_su_oprlinks_id PRIMARY KEY (id_users, id_operations),
  CONSTRAINT fk_su_oprlinks_oper FOREIGN KEY (id_operations) REFERENCES sr_operations (id),
  CONSTRAINT fk_su_oprlinks_user FOREIGN KEY (id_users) REFERENCES su_users (id)
);

CREATE TABLE ss_reports (
  id INT NOT NULL ,
  form VARCHAR(32) NOT NULL ,
  report MEDIUMBLOB NULL ,
  name VARCHAR(64) NULL ,
  notes VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_reports_id PRIMARY KEY (id)
);

CREATE TABLE ss_attachments (
  id INT NOT NULL ,
  object_name VARCHAR(32) NOT NULL ,
  object_id INT NOT NULL ,
  filename VARCHAR(128) NULL ,
  filedata MEDIUMBLOB NULL ,
  filetime DATETIME NULL ,
  filesize INT NULL ,
  CONSTRAINT pk_ss_attachments_id PRIMARY KEY (id)
);

CREATE INDEX lk_ss_attachments_object_name ON ss_attachments (object_name);
CREATE INDEX lk_ss_attachments_object_id ON ss_attachments (object_name, object_id);

CREATE TABLE sm_messages (
  id INT NOT NULL ,
  id_users INT NOT NULL ,
  sended DATETIME NOT NULL ,
  title VARCHAR(255) NULL ,
  address VARCHAR(255) NULL ,
  message MEDIUMBLOB NULL ,
  CONSTRAINT pk_sm_messages_id PRIMARY KEY (id),
  CONSTRAINT fk_sm_messages_users FOREIGN KEY (id_users) REFERENCES su_users (id)
);

CREATE TABLE sm_history (
  id_messages INT NOT NULL ,
  id_users INT NOT NULL ,
  opened DATETIME NULL ,
  closed DATETIME NULL ,
  CONSTRAINT pk_sm_history_id PRIMARY KEY (id_messages, id_users),
  CONSTRAINT fk_sm_history_msg FOREIGN KEY (id_messages) REFERENCES sm_messages (id),
  CONSTRAINT fk_sm_history_users FOREIGN KEY (id_users) REFERENCES su_users (id)
);

/**** APP TABLES ****/

/**** SYSTEM VIEWS ****/

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
WHERE sr_operations.id_levels = sr_levels.id;

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
  AND (sr_operations.id_levels IN (SELECT sr_levels.id FROM sr_levels WHERE sr_levels.hidden=0));

CREATE VIEW vsr_operations_list AS
SELECT
  sr_operations.id, 
  sr_operations.name,
  sr_operations.notes
FROM sr_operations
WHERE sr_operations.id_levels IN (SELECT sr_levels.id FROM sr_levels WHERE sr_levels.hidden=0);

CREATE VIEW vsr_operations_user AS
SELECT id_users, id_operations
FROM su_opglinks U LEFT JOIN sr_opglinks G ON U.id_opgroups=G.id_opgroups
GROUP BY id_users, id_operations
  UNION
SELECT id_users, id_operations
FROM su_oprlinks;

CREATE VIEW vss_syslog AS
SELECT 
  ss_syslog.datetime, 
  ss_syslog.id_operations, 
  sr_operations.id_levels, 
  ss_syslog.id_users, 
  ss_syslog.id_workplases,
  ss_syslog.info,  
  ss_syslog.host,  
  ss_syslog.netuser 
FROM ss_syslog LEFT JOIN sr_operations ON ss_syslog.id_operations = sr_operations.id;

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
FROM ss_varnames LEFT JOIN ss_settings ON ss_varnames.id = ss_settings.id;

/**** SYSTEM TRIGGERS ****/

DELIMITER \\

CREATE TRIGGER sr_operations_delete BEFORE DELETE ON sr_operations
FOR EACH ROW BEGIN
  DELETE FROM sr_opglinks WHERE id_operations = old.id;
  DELETE FROM sr_opwlinks WHERE id_operations = old.id;
  DELETE FROM su_oprlinks WHERE id_operations = old.id;
END
\\

CREATE TRIGGER sr_opgroups_delete BEFORE DELETE ON sr_opgroups 
FOR EACH ROW BEGIN
  DELETE FROM sr_opglinks WHERE id_opgroups = old.id;
  DELETE FROM su_opglinks WHERE id_opgroups = old.id;
END
\\

CREATE TRIGGER sr_workplases_delete BEFORE DELETE ON sr_workplases 
FOR EACH ROW BEGIN
  DELETE FROM sr_opwlinks WHERE id_workplases = old.id;
END
\\

CREATE TRIGGER su_users_delete BEFORE DELETE ON su_users 
FOR EACH ROW BEGIN
  IF (SELECT IfNull(value_int, 0) FROM ss_settings WHERE id = 9000) <> 1 THEN
    SIGNAL SQLSTATE '90000' SET message_text = 'Удаление пользователей запрещено политикой безопасности системы!';
  ELSE 
    BEGIN
      DELETE FROM su_opglinks WHERE id_users = old.id;
      DELETE FROM su_oprlinks WHERE id_users = old.id;
    END; 
  END IF;
END
\\

DELIMITER ;

/**** SYSTEM PROCEDURES ****/

DELIMITER \\

CREATE PROCEDURE sp_sys_ch_db_version (VER INT)
BEGIN
  UPDATE ss_settings SET value_int=VER WHERE id=1000;
END
\\

CREATE PROCEDURE sp_sys_add_log (id_wps INT, id_usr INT, id_opr INT, hostname VARCHAR(32), netuser VARCHAR(32), info VARCHAR(255)) 
BEGIN
  IF (SELECT IfNull(value_int, 0) FROM ss_settings WHERE id=9001) = 1 THEN
    INSERT INTO ss_syslog (DATETIME, id_operations, id_users, id_workplases, info, host, netuser) 
      VALUES (NOW(), id_opr, id_usr, id_wps, info, hostname, netuser);
  END IF;
END
\\

CREATE PROCEDURE sp_sys_group_add_operation (id_opg INT, id_opr INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (id_opg, id_opr);

  /** Добавляем запись в протокол  **/
  SET @message = Concat('"Группы доступа" [sr_opglinks]: Операция id="', id_opr, 
    '", name="', (SELECT IfNull(name, '') FROM sr_operations WHERE id=id_opr),  
    '" включена в группу доступа id="', id_opg, 
    '", name="', (SELECT IfNull(name, '') FROM sr_opgroups WHERE id=id_opg),
    '". [sp_sys_group_add_operation]');
  CALL sp_sys_add_log (id_wps, id_usr, 9001, hostname, netuser, @message);
END
\\

CREATE PROCEDURE sp_sys_group_del_operation (id_opg INT, id_opr INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  IF EXISTS (SELECT * FROM sr_opglinks WHERE id_opgroups=id_opg AND id_operations=id_opr) THEN
  BEGIN
    DELETE FROM sr_opglinks WHERE id_opgroups=id_opg AND id_operations=id_opr;

    /** Добавляем запись в протокол  **/
    SET @message = Concat('"Группы доступа" [sr_opglinks]: Операция id="', id_opr, 
      '", name="', (SELECT IfNull(name, '') FROM sr_operations WHERE id=id_opr),  
      '" исключена из группы доступа id="', id_opg, 
      '", name="', (SELECT IfNull(name, '') FROM sr_opgroups WHERE id=id_opg),
      '". [sp_sys_group_del_operation]');
    CALL sp_sys_add_log (id_wps, id_usr, 9001, hostname, netuser, @message);
  END; 
  END IF;
END
\\

CREATE PROCEDURE sp_sys_user_add_operation (id_sus INT, id_opr INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  INSERT INTO su_oprlinks (id_users, id_operations) VALUES (id_sus, id_opr);
  
  /** Добавляем запись в протокол  **/
  SET @message = Concat('"Пользователи системы" [su_oprlinks]: Операция id="', id_opr,
    '", name="', (SELECT IfNull(name, '') FROM sr_operations WHERE id=id_opr),
    '" включена в список пользователя id="', id_sus,
    '", name="', (SELECT IfNull(name, '') FROM su_users WHERE id=id_sus),
    '", fullname="', (SELECT IfNull(fullname, '') FROM su_users WHERE id=id_sus),
    '". [sp_sys_user_add_operation]');
  CALL sp_sys_add_log (id_wps, id_usr, 9001, hostname, netuser, @message);
END
\\

CREATE PROCEDURE sp_sys_user_del_operation (id_sus INT, id_opr INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  IF EXISTS (SELECT * FROM su_oprlinks WHERE id_users=id_sus AND id_operations=id_opr) THEN
  BEGIN
    DELETE FROM su_oprlinks WHERE id_users=id_sus AND id_operations=id_opr;
    
    /** Добавляем запись в протокол  **/
    SET @message = Concat('"Пользователи системы" [su_oprlinks]: Операция id="', id_opr,
      '", name="', (SELECT IfNull(name, '') FROM sr_operations WHERE id=id_opr),
      '" исключена в список пользователя id="', id_sus,
      '", name="', (SELECT IfNull(name, '') FROM su_users WHERE id=id_sus),
      '", fullname="', (SELECT IfNull(fullname, '') FROM su_users WHERE id=id_sus),
      '". [sp_sys_user_del_operation]');
    CALL sp_sys_add_log (id_wps, id_usr, 9001, hostname, netuser, @message);
  END; 
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
  IF EXISTS (SELECT * FROM su_opglinks WHERE id_users=id_sus AND id_opgroups=id_opg) THEN
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

CREATE PROCEDURE sp_sys_group_unlink_users (id_opg INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  DECLARE DONE INT DEFAULT FALSE;
  DECLARE id_sus INT;
  DECLARE cs_opglinks CURSOR FOR SELECT id_users FROM su_opglinks;
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE = TRUE;
  
  OPEN cs_opglinks;
  WHILE NOT DONE DO
    FETCH cs_opglinks INTO id_sus;
    
    IF NOT DONE THEN
      CALL sp_sys_user_del_opgroup (id_sus, id_opg, id_wps, id_usr, hostname, netuser);
    END IF;
  END WHILE;
  CLOSE cs_opglinks;
END
\\

CREATE PROCEDURE sp_sys_group_unlink_opers (id_opg INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  DECLARE DONE INT DEFAULT FALSE;
  DECLARE id_opr INT;
  DECLARE cs_opglinks CURSOR FOR SELECT id_operations FROM sr_opglinks;
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE = TRUE;

  OPEN cs_opglinks;
  WHILE NOT DONE DO
    FETCH cs_opglinks INTO id_opr;
    
    IF NOT DONE THEN
      CALL sp_sys_group_del_operation (id_opg, id_opr, id_wps, id_usr, hostname, netuser);
    END IF;
  END WHILE;
  CLOSE cs_opglinks;
END
\\

CREATE PROCEDURE sp_sys_group_unlink_all (id_opg INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  CALL sp_sys_group_unlink_users (id_opg, id_wps, id_usr, hostname, netuser);
  CALL sp_sys_group_unlink_opers (id_opg, id_wps, id_usr, hostname, netuser);
END
\\

CREATE PROCEDURE sp_sys_user_unlink_opers (id_sus INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  DECLARE DONE INT DEFAULT FALSE;
  DECLARE id_opr INT;
  DECLARE cs_oprlinks CURSOR FOR SELECT id_opr FROM su_oprlinks;

  OPEN cs_oprlinks;
  WHILE NOT DONE DO
    FETCH cs_oprlinks INTO id_opr;

    IF NOT DONE THEN
      CALL sp_sys_user_del_operation (id_sus, id_opr, id_wps, id_usr, hostname, netuser);
    END IF;
  END WHILE;
  CLOSE cs_oprlinks;
END
\\

CREATE PROCEDURE sp_sys_user_unlink_groups (id_sus INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  DECLARE DONE INT DEFAULT FALSE;
  DECLARE id_opg INT;
  DECLARE cs_opglinks CURSOR FOR SELECT id_opgroups FROM su_opglinks;

  OPEN cs_opglinks;
  WHILE NOT DONE DO
    FETCH cs_opglinks INTO id_opg;

    IF NOT DONE THEN
      CALL sp_sys_user_del_opgroup (id_sus, id_opg, id_wps, id_usr, hostname, netuser);
    END IF;
  END WHILE;
  CLOSE cs_opglinks;
END
\\

CREATE PROCEDURE sp_sys_user_unlink_all (id_sus INT, id_wps INT, id_usr INT, hostname VARCHAR(32), netuser VARCHAR(32))
BEGIN
  CALL sp_sys_user_unlink_opers (id_sus, id_wps, id_usr, hostname, netuser);
  CALL sp_sys_user_unlink_groups (id_sus, id_wps, id_usr, hostname, netuser);
END
\\

DELIMITER ;

/**** INITIAL DATA ****/

/**** SYSTEM SETTINGS ****/

INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (1, NULL, 'Все параметры', NULL);
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (2, 1, 'База данных', 'Параметры базы данных');
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (3, 1, 'Подсистема безопасности', 'Параметры подсистемы безопасности');
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (4, 3, 'Политика паролей', 'Параметры паролей пользователей');
INSERT INTO ss_vargroups (id, owner_id, name, notes) VALUES (5, 3, 'Журнал аудита', 'Параметры журнала аудита системы');

INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (1000, 2, 0, NULL, 'Текущая версия базы данных');
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (1002, 2, 1, '90', 'Срок действия коротких сообщений, дней');
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (1102, 1, 1, '1024', 'Размер вложений в килобайтах, свыше которого запрашивается подтверждение');
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (1103, 1, 1, '10240', 'Максимально допустимый размер вложений в килобайтах');
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9001, 5, 6, 'Включено', 'Состояние журнала аудита системы');
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9002, 5, 1, '1000000', 'Порог предупреждения о заполнении журнала, записей');
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9003, 4, 1, '40', 'Интервал в днях между очередной сменой пароля');
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9004, 4, 1, '6', 'Минимально допустимая длина пароля пользователя в символах');
INSERT INTO ss_varnames (id, id_groups, type, def_value, name) VALUES (9005, 5, 1, '365', 'Период хранения записей журнала аудита системы, дней');

/**** DATABASE VERSION ****/

INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (1000, 1, NULL, NULL); 

/**** SYSTEM SETTINGS VALUES ****/

INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (1002, 90, NULL, NULL); 
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (1102, 1024, NULL, NULL); 
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (1103, 10240, NULL, NULL); 
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9000, 0, NULL, NULL);
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9001, 1, NULL, NULL);
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9002, 1000000, NULL, NULL);
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9003, 40, NULL, NULL);
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9004, 6, NULL, NULL);
INSERT INTO ss_settings (id, value_int, value_real, value_char) VALUES (9005, 0, NULL, NULL);

/**** WORKPLASES ****/

INSERT INTO sr_workplases (id, name_s, name) VALUES (1, 'АДМИНИСТРАТОР', 'Администратор системы');

/**** SECURITY LEVELS ****/

INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (100, 'Регистрация в системе', 'Проверка имени пользователя, пароля и прав доступа в систему.', 1, -1, 8388608, 16777194);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (101, 'Смена пароля', 'Смена пароля пользователем.', 1, -1, 16384, 16777194);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (110, 'Ошибки регистрации', 'Введено неверное имя пользователя, неверный пароль или отказ в доступе.', 1, 0, 213, 16777194);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (150, 'Чтение сообщений', 'Чтение коротких сообщений, отправляемых другими пользователями.', 0, -1, 8388608, 16777194);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (151, 'Создание сообщений', 'Создание коротких сообщений для других пользователей системы.', 0, -1, 8388608, 16777194);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (200, 'Просмотр', 'Просмотр данных или параметров без предоставления прав на изменение этих данных.', 0, -1, 16384, 15400938);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (300, 'Настройка справочников', 'Настройка рабочих справочников системы - просмотр и редактирование.', 0, -1, 16384, 15397375);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (400, 'Изменение данных A', 'Изменение данных категории доступа A.', 0, -1, 213, 15400959);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (410, 'Изменение данных B', 'Изменение данных категории доступа B.', 0, -1, 16512, 15400959);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (420, 'Изменение данных C', 'Изменение данных категории доступа C.', 0, -1, 128, 15400959);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (430, 'Изменение данных D', 'Изменение данных категории доступа D.', 0, -1, 8388608, 15400959);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (440, 'Изменение данных E', 'Изменение данных категории доступа E.', 0, -1, 16384, 15400959);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (480, 'Прочие операции', 'Прочие операции без непосредственного изменения данных в БД.', 0, -1, 16384, 15400938);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (490, 'Внешние операции', 'Выполнение операций без изменения данных в БД.', 0, -1, 16384, 15400938);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (500, 'Изменение параметров безопасности', 'Изменение данных или параметров, влияющих на уровень безопасности системы.', 0, -1, 213, 15395583);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (510, 'Изменение глобальных параметров', 'Изменение параметров, влияющих на функционирование системы в целом.', 0, -1, 64, 15395583);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (800, 'Редактирование отчетов', 'Создание или изменение отчетных форм, используемых при построении отчетов.', 0, -1, 8388608, 15400938);
INSERT INTO sr_levels (id, name, notes, hidden, font_style, font_color, cell_color) VALUES (999, 'Ошибка', 'Ошибки, регистрируемые в протоколе работы системы.', 1, 0, 65535, 8421631);

/**** SYSTEM OPERATIONS ****/

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9990, 100, 'Регистрация в системе', 'Проверка имени пользователя, пароля и прав доступа в систему.');
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9991, 101, 'Смена пароля', 'Смена пароля пользователем.');
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9995, 510, 'Обновление БД', 'Автоматическое обновление базы данных.');
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9998, 999, 'Ошибка', 'Ошибка функционирования программы.');
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9999, 110, 'Ошибка регистрации', 'Ошибка регистрации или попытка несанкционированного доступа в систему.');

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9980, 150, 'Чтение сообщений', 'Чтение коротких сообщений, отправляемых другими пользователями.');
INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9981, 151, 'Создание сообщений', 'Создание коротких сообщений для других пользователей системы.');

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (8000, 200, 'Просмотр параметров системы', 'Просмотр параметров системы, влияющих на все АРМы системы.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 8000);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (8001, 510, 'Изменение параметров системы', 'Изменение параметров системы, влияющих на все АРМы системы.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 8001);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (8201, 800, 'Редактирование отчетов', 'Создание или изменение отчетных форм, используемых при построении отчетов.');

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9000, 200, 'Просмотр групп доступа', 'Просмотр групп прав на выполнение операций в системе.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9000);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9001, 500, 'Управление группами доступа', 'Создание, изменение и удаление групп прав на выполнение операций в системе.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9001);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9100, 200, 'Просмотр списка пользователей', 'Просмотр списка пользователей и назначенных им прав на выполнение операций.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9100);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9101, 500, 'Управление списком пользователей', 'Создание, изменение и блокировка пользователей системы, назначение им прав на выполнение операций.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9101);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9102, 500, 'Блокировка и разблокировка пользователей', 'Установки и снятие блокировки на доступ пользователей в систему.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9102);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9104, 500, 'Редактирование групп пользователей', 'Создание, изменение и удаление условных групп пользователей.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9104);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9200, 200, 'Просмотр параметров безопасности', 'Просмотр параметров, влияющих на уровень безопасности системы.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9200);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9201, 500, 'Редактирование параметров безопасности', 'Редактирование параметров, влияющих на уровень безопасности системы.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9201);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9210, 200, 'Просмотр протокола системы', 'Просмотр записей системного протокола работы, выгрузка в файл и печать отчета.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9210);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9211, 500, 'Очистка протокола системы', 'Удаление записей из системного протокола работы.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9211);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9220, 200, 'Просмотр цветовых схем протокола', 'Просмотр справочника уровней безопасности операций.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9220);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9221, 300, 'Редактирование цветовых схем протокола', 'Настройка справочника (в части редактирования цветовых схем) уровней безопасности операций.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9221);

INSERT INTO sr_operations (id, id_levels, name, notes) VALUES (9222, 200, 'Просмотр списка операций системы', 'Просмотр справочника операций системы.');
INSERT INTO sr_opwlinks (id_workplases, id_operations) VALUES (1, 9222);

/**** APP OPERATIONS ****/

/**** ADMIN ROLES ****/

INSERT INTO sr_opgroups (id, name, notes) VALUES (1, 'АДМИНИСТРИРОВАНИЕ СИСТЕМЫ', 'Управление правами на выполнение операций и группами прав, управление пользователями системы, настройка глобальных параметров системы.');
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 8000);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 8001);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9000);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9001);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9200);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9201);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9210);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9211);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9220);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9221);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (1, 9222);

INSERT INTO sr_opgroups (id, name, notes) VALUES (2, 'УПРАВЛЕНИЕ ПОЛЬЗОВАТЕЛЯМИ СИСТЕМЫ', 'Управление пользователями системы, предоставление пользователям прав на выполнение операций.');
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (2, 9100);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (2, 9101);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (2, 9102);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (2, 9104);

INSERT INTO sr_opgroups (id, name, notes) VALUES (3, 'КОНТРОЛЬ РАБОТЫ СИСТЕМЫ', 'Контроль работы и настроек всей системы.');
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 8000);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 9000);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 9100);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 9200);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (3, 9210);

INSERT INTO sr_opgroups (id, name, notes) VALUES (4, 'СООБЩЕНИЯ', 'Прием и отправка сообщений внутри системы.');
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (4, 9980);
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (4, 9981);

INSERT INTO sr_opgroups (id, name, notes) VALUES (5, 'РЕДАКТИРОВАНИЕ ОТЧЕТОВ', 'Редактирование шаблонов отчетов FASt Reports.');
INSERT INTO sr_opglinks (id_opgroups, id_operations) VALUES (5, 8201);

/**** APP ROLES ****/

/**** DEFAULT USERS ****/

INSERT INTO su_groups VALUES (1, NULL, 'Все пользователи', NULL);
INSERT INTO su_groups VALUES (2, 1, 'Администраторы', 'Администраторы системы');
INSERT INTO su_groups VALUES (3, 1, 'Пользователи', 'Пользователи системы');

INSERT INTO su_users (id, id_groups, name, fullname, notes, password, deleted, blocked, count_ep) VALUES (0, 1, 'system', 'DatabASe account', '', '', 1, 1, 0);
INSERT INTO su_users (id, id_groups, name, fullname, notes, password, deleted, blocked, count_ep) VALUES (1, 2, 'admin', 'Администратор системы', 'Встроенная учетная запись администратора системы.', '', 0, 0, 0);

INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 1);
INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 2);
INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 3);
INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 4);
INSERT INTO su_opglinks (id_users, id_opgroups) VALUES (1, 5);

/**** DONE ****/