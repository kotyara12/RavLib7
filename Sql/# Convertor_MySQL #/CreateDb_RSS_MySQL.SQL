/**** SYSTEM TABLES ****/

CREATE TABLE ss_blockids (
  NAME VARCHAR(32) NOT NULL ,
  ID INT NOT NULL ,
  CONSTRAINT pk_ss_blockids_id PRIMARY KEY (NAME, ID)
);

CREATE TABLE sr_levels (
  ID INT NOT NULL ,
  NAME VARCHAR(64) NULL ,
  NOTES VARCHAR(255) NULL ,
  HIDDEN BIT NOT NULL ,
  FONT_STYLE INT NULL ,
  FONT_COLOR INT NULL ,
  CELL_COLOR INT NULL ,
  CONSTRAINT pk_sr_levels_id PRIMARY KEY (ID) 
);

CREATE TABLE sr_operations (
  ID INT NOT NULL ,
  ID_LEVELS INT NOT NULL,  
  NAME VARCHAR(64) NULL ,
  NOTES VARCHAR(255) NULL ,
  CONSTRAINT pk_sr_operations_id PRIMARY KEY (ID),
  CONSTRAINT fk_sr_operations_levels FOREIGN KEY (ID_LEVELS) REFERENCES sr_levels (ID)
);

CREATE TABLE sr_opgroups (
  ID INT NOT NULL ,
  NAME VARCHAR(64) NULL ,
  NOTES VARCHAR(255) NULL ,
  CONSTRAINT pk_sr_opgroups_id PRIMARY KEY (ID)
);

CREATE TABLE sr_workplases (
  ID INT NOT NULL ,
  NAME_S VARCHAR(16) NULL ,
  NAME VARCHAR(128) NULL ,
  CONSTRAINT pk_sr_workplases_id PRIMARY KEY (ID)
);

CREATE TABLE ss_vargroups (
  ID INT NOT NULL ,
  OWNER_ID INT NULL ,
  NAME VARCHAR(64) NOT NULL ,
  NOTES VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_vargroups_id PRIMARY KEY (ID)
);

CREATE TABLE ss_varnames (
  ID INT NOT NULL ,
  ID_GROUPS INT NOT NULL ,
  TYPE INT NOT NULL ,
  DEF_VALUE VARCHAR(255) NULL ,
  NAME VARCHAR(128) NOT NULL ,
  CONSTRAINT pk_ss_varnames_id PRIMARY KEY (ID),
  CONSTRAINT fk_ss_varnames_groups FOREIGN KEY (ID_GROUPS) REFERENCES ss_vargroups (ID)
);

CREATE TABLE ss_settings (
  ID INT NOT NULL ,
  VALUE_INT INT NULL ,
  VALUE_REAL REAL NULL ,
  VALUE_CHAR VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_settings_id PRIMARY KEY (ID)
);

CREATE TABLE ss_syslog (
  DATETIME DATETIME NOT NULL ,
  ID_OPERATIONS INT NOT NULL ,
  ID_USERS INT NOT NULL ,
  ID_WORKPLASES INT NOT NULL ,
  INFO VARCHAR(255) NOT NULL ,
  HOST VARCHAR(32) NULL ,
  NETUSER VARCHAR(32) NULL ,
  CONSTRAINT pk_ss_syslog_log PRIMARY KEY (DATETIME, ID_OPERATIONS, ID_USERS, INFO),
  CONSTRAINT fk_ss_syslog_oper FOREIGN KEY (ID_OPERATIONS) REFERENCES sr_operations (ID),
  CONSTRAINT fk_ss_syslog_wp FOREIGN KEY (ID_WORKPLASES) REFERENCES sr_workplases (ID)
);

CREATE TABLE su_groups (
  ID INT NOT NULL ,
  OWNER_ID INT NULL ,
  NAME VARCHAR(64) NOT NULL ,
  NOTES VARCHAR(255) NULL ,
  CONSTRAINT pk_su_groups_id PRIMARY KEY (ID)
);

CREATE TABLE su_users (
  ID INT NOT NULL ,
  ID_GROUPS INT NOT NULL ,
  NAME VARCHAR(16) NOT NULL ,
  FULLNAME VARCHAR(64) NULL ,
  NOTES VARCHAR(64) NULL ,
  PASSWORD VARCHAR(255) NULL ,
  DELETED BIT NOT NULL ,
  BLOCKED BIT NOT NULL ,
  CHANGED DATETIME NULL ,
  COUNT_EP INT NULL ,
  CONSTRAINT pk_su_users_id PRIMARY KEY (ID),
  CONSTRAINT UQ_su_users_name UNIQUE (NAME),
  CONSTRAINT fk_su_users_groups FOREIGN KEY (ID_GROUPS) REFERENCES su_groups (ID)
);

CREATE TABLE sr_opglinks (
  ID_OPGROUPS INT NOT NULL ,
  ID_OPERATIONS INT NOT NULL ,
  CONSTRAINT pk_sr_opglinks_id PRIMARY KEY (ID_OPGROUPS, ID_OPERATIONS),
  CONSTRAINT fk_sr_opglinks_OPER FOREIGN KEY (ID_OPERATIONS) REFERENCES sr_operations (ID),
  CONSTRAINT fk_sr_opglinks_GRP FOREIGN KEY (ID_OPGROUPS) REFERENCES sr_opgroups (ID)
);

CREATE TABLE sr_opwlinks (
  ID_WORKPLASES INT NOT NULL ,
  ID_OPERATIONS INT NOT NULL ,
  CONSTRAINT pk_sr_opwlinks_id PRIMARY KEY (ID_WORKPLASES, ID_OPERATIONS),
  CONSTRAINT fk_sr_opwlinks_wp FOREIGN KEY (ID_WORKPLASES) REFERENCES sr_workplases (ID),
  CONSTRAINT fk_sr_opwlinks_oper FOREIGN KEY (ID_OPERATIONS) REFERENCES sr_operations (ID)
);

CREATE TABLE su_opglinks (
  ID_USERS INT NOT NULL ,
  ID_OPGROUPS INT NOT NULL ,
  CONSTRAINT pk_su_opglinks_id PRIMARY KEY (ID_USERS, ID_OPGROUPS),
  CONSTRAINT fk_su_opglinks_grp FOREIGN KEY (ID_OPGROUPS) REFERENCES sr_opgroups (ID),
  CONSTRAINT fk_su_opglinks_user FOREIGN KEY (ID_USERS) REFERENCES su_users (ID)
);

CREATE TABLE su_oprlinks (
  ID_USERS INT NOT NULL ,
  ID_OPERATIONS INT NOT NULL ,
  CONSTRAINT pk_su_oprlinks_id PRIMARY KEY (ID_USERS, ID_OPERATIONS),
  CONSTRAINT fk_su_oprlinks_oper FOREIGN KEY (ID_OPERATIONS) REFERENCES sr_operations (ID),
  CONSTRAINT fk_su_oprlinks_user FOREIGN KEY (ID_USERS) REFERENCES su_users (ID)
);

CREATE TABLE ss_reports (
  ID INT NOT NULL ,
  FORM VARCHAR(32) NOT NULL ,
  REPORT MEDIUMBLOB NULL ,
  NAME VARCHAR(64) NULL ,
  NOTES VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_reports_id PRIMARY KEY (ID)
);

CREATE TABLE ss_attachments (
  ID INT NOT NULL ,
  OBJECT_NAME VARCHAR(32) NOT NULL ,
  OBJECT_ID INT NOT NULL ,
  FILENAME VARCHAR(128) NULL ,
  FILEDATA MEDIUMBLOB NULL ,
  FILETIME DATETIME NULL ,
  FILESIZE INT NULL ,
  CONSTRAINT pk_ss_attachments_id PRIMARY KEY (ID)
);

CREATE INDEX lk_ss_attachments_object_name ON ss_attachments (OBJECT_NAME);
CREATE INDEX lk_ss_attachments_object_id ON ss_attachments (OBJECT_NAME, OBJECT_ID);

CREATE TABLE sm_messages (
  ID INT NOT NULL ,
  ID_USERS INT NOT NULL ,
  SENDED DATETIME NOT NULL ,
  TITLE VARCHAR(255) NULL ,
  ADDRESS VARCHAR(255) NULL ,
  MESSAGE MEDIUMBLOB NULL ,
  CONSTRAINT pk_sm_messages_id PRIMARY KEY (ID),
  CONSTRAINT fk_sm_messages_users FOREIGN KEY (ID_USERS) REFERENCES su_users (ID)
);

CREATE TABLE sm_history (
  ID_MESSAGES INT NOT NULL ,
  ID_USERS INT NOT NULL ,
  OPENED DATETIME NULL ,
  CLOSED DATETIME NULL ,
  CONSTRAINT pk_sm_history_id PRIMARY KEY (ID_MESSAGES, ID_USERS),
  CONSTRAINT fk_sm_history_msg FOREIGN KEY (ID_MESSAGES) REFERENCES sm_messages (ID),
  CONSTRAINT fk_sm_history_users FOREIGN KEY (ID_USERS) REFERENCES su_users (ID)
);

/**** APP TABLES ****/

/**** SYSTEM VIEWS ****/

CREATE VIEW vsr_operations_all AS
SELECT
  sr_operations.ID, 
  sr_operations.ID_LEVELS,
  sr_operations.NAME,
  sr_operations.NOTES,
  sr_levels.NAME AS LEVEL_NAME,
  sr_levels.NOTES AS LEVEL_NOTES,
  sr_levels.FONT_STYLE,
  sr_levels.FONT_COLOR,
  sr_levels.CELL_COLOR
FROM sr_operations, sr_levels
WHERE sr_operations.ID_LEVELS = sr_levels.ID;

CREATE VIEW vsr_operations_work AS
SELECT
  sr_operations.ID, 
  sr_operations.ID_LEVELS,
  sr_operations.NAME,
  sr_operations.NOTES,
  sr_levels.NAME AS LEVELS_NAME,
  sr_levels.NOTES AS LEVELS_NOTES,
  sr_levels.FONT_STYLE,
  sr_levels.FONT_COLOR,
  sr_levels.CELL_COLOR
FROM sr_operations, sr_levels
WHERE (sr_operations.ID_LEVELS = sr_levels.ID) 
  AND (sr_operations.ID_LEVELS IN (SELECT sr_levels.ID FROM sr_levels WHERE sr_levels.HIDDEN=0));

CREATE VIEW vsr_operations_list AS
SELECT
  sr_operations.ID, 
  sr_operations.NAME,
  sr_operations.NOTES
FROM sr_operations
WHERE sr_operations.ID_LEVELS IN (SELECT sr_levels.ID FROM sr_levels WHERE sr_levels.HIDDEN=0);

CREATE VIEW vsr_operations_user AS
SELECT ID_USERS, ID_OPERATIONS
FROM su_opglinks U LEFT JOIN sr_opglinks G ON U.ID_OPGROUPS=G.ID_OPGROUPS
GROUP BY ID_USERS, ID_OPERATIONS
  UNION
SELECT ID_USERS, ID_OPERATIONS
FROM su_oprlinks;

CREATE VIEW vss_syslog AS
SELECT 
  ss_syslog.DATETIME, 
  ss_syslog.ID_OPERATIONS, 
  sr_operations.ID_LEVELS, 
  ss_syslog.ID_USERS, 
  ss_syslog.ID_WORKPLASES,
  ss_syslog.INFO,  
  ss_syslog.HOST,  
  ss_syslog.NETUSER 
FROM ss_syslog LEFT JOIN sr_operations ON ss_syslog.ID_OPERATIONS = sr_operations.ID;

CREATE VIEW vss_settings AS
SELECT
  ss_varnames.ID,
  ss_varnames.ID_GROUPS,
  ss_varnames.TYPE,
  ss_varnames.DEF_VALUE,
  ss_settings.VALUE_INT,
  ss_settings.VALUE_REAL,
  ss_settings.VALUE_CHAR,
  ss_varnames.NAME
FROM ss_varnames LEFT JOIN ss_settings ON ss_varnames.ID = ss_settings.ID;

/**** SYSTEM TRIGGERS ****/

DELIMITER \\

CREATE TRIGGER sr_operations_delete BEFORE DELETE ON sr_operations
FOR EACH ROW BEGIN
  DELETE FROM sr_opglinks WHERE ID_OPERATIONS = old.ID;
  DELETE FROM sr_opwlinks WHERE ID_OPERATIONS = old.ID;
  DELETE FROM su_oprlinks WHERE ID_OPERATIONS = old.ID;
END
\\

CREATE TRIGGER sr_opgroups_delete BEFORE DELETE ON sr_opgroups 
FOR EACH ROW BEGIN
  DELETE FROM sr_opglinks WHERE ID_OPGROUPS = old.ID;
  DELETE FROM su_opglinks WHERE ID_OPGROUPS = old.ID;
END
\\

CREATE TRIGGER sr_workplases_delete BEFORE DELETE ON sr_workplases 
FOR EACH ROW BEGIN
  DELETE FROM sr_opwlinks WHERE ID_WORKPLASES = old.ID;
END
\\

CREATE TRIGGER su_users_delete BEFORE DELETE ON su_users 
FOR EACH ROW BEGIN
  IF (SELECT IfNull(VALUE_INT, 0) FROM ss_settings WHERE ID = 9000) <> 1 THEN
    SIGNAL SQLSTATE '90000' SET message_text = 'Удаление пользователей запрещено политикой безопасности системы!';
  ELSE 
    BEGIN
      DELETE FROM su_opglinks WHERE ID_USERS = old.ID;
      DELETE FROM su_oprlinks WHERE ID_USERS = old.ID;
    END; 
  END IF;
END
\\

DELIMITER ;

/**** SYSTEM PROCEDURES ****/

DELIMITER \\

CREATE PROCEDURE sp_sys_ch_db_version (VER INT)
BEGIN
  update ss_settings SET VALUE_INT=VER WHERE ID=1000;
END
\\

CREATE PROCEDURE sp_sys_add_log (ID_WPS INT, ID_USR INT, ID_OPR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32), INFO VARCHAR(255)) 
BEGIN
  IF (SELECT IfNull(VALUE_INT, 0) FROM ss_settings WHERE ID=9001) = 1 THEN
    INSERT INTO ss_syslog (DATETIME, ID_OPERATIONS, ID_USERS, ID_WORKPLASES, INFO, HOST, NETUSER) 
      VALUES (NOW(), ID_OPR, ID_USR, ID_WPS, INFO, HOSTNAME, NETUSER);
  END IF;
END
\\

CREATE PROCEDURE sp_sys_group_add_operation (ID_OPG INT, ID_OPR INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (ID_OPG, ID_OPR);

  /** Добавляем запись в протокол  **/
  SET @MESSAGE = Concat('"Группы доступа" [sr_opglinks]: Операция ID="', ID_OPR, 
    '", NAME="', (SELECT IfNull(NAME, '') FROM sr_operations WHERE ID=ID_OPR),  
    '" включена в группу доступа ID="', ID_OPG, 
    '", NAME="', (SELECT IfNull(NAME, '') FROM sr_opgroups WHERE ID=ID_OPG),
    '". [sp_sys_group_add_operation]');
  CALL sp_sys_add_log (ID_WPS, ID_USR, 9001, HOSTNAME, NETUSER, @MESSAGE);
END
\\

CREATE PROCEDURE sp_sys_group_del_operation (ID_OPG INT, ID_OPR INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  IF EXISTS (SELECT * FROM sr_opglinks WHERE ID_OPGROUPS=ID_OPG and ID_OPERATIONS=ID_OPR) THEN
  BEGIN
    DELETE FROM sr_opglinks WHERE ID_OPGROUPS=ID_OPG and ID_OPERATIONS=ID_OPR;

    /** Добавляем запись в протокол  **/
    SET @MESSAGE = Concat('"Группы доступа" [sr_opglinks]: Операция ID="', ID_OPR, 
      '", NAME="', (SELECT IfNull(NAME, '') FROM sr_operations WHERE ID=ID_OPR),  
      '" исключена из группы доступа ID="', ID_OPG, 
      '", NAME="', (SELECT IfNull(NAME, '') FROM sr_opgroups WHERE ID=ID_OPG),
      '". [sp_sys_group_del_operation]');
    CALL sp_sys_add_log (ID_WPS, ID_USR, 9001, HOSTNAME, NETUSER, @MESSAGE);
  END; 
  END IF;
END
\\

CREATE PROCEDURE sp_sys_user_add_operation (ID_SUS INT, ID_OPR INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  INSERT INTO su_oprlinks (ID_USERS, ID_OPERATIONS) VALUES (ID_SUS, ID_OPR);
  
  /** Добавляем запись в протокол  **/
  SET @MESSAGE = Concat('"Пользователи системы" [su_oprlinks]: Операция ID="', ID_OPR,
    '", NAME="', (SELECT IfNull(NAME, '') FROM sr_operations WHERE ID=ID_OPR),
    '" включена в список пользователя ID="', ID_SUS,
    '", NAME="', (SELECT IfNull(NAME, '') FROM su_users WHERE ID=ID_SUS),
    '", FULLNAME="', (SELECT IfNull(FULLNAME, '') FROM su_users WHERE ID=ID_SUS),
    '". [sp_sys_user_add_operation]');
  CALL sp_sys_add_log (ID_WPS, ID_USR, 9001, HOSTNAME, NETUSER, @MESSAGE);
END
\\

CREATE PROCEDURE sp_sys_user_del_operation (ID_SUS INT, ID_OPR INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  IF EXISTS (SELECT * FROM su_oprlinks WHERE ID_USERS=ID_SUS and ID_OPERATIONS=ID_OPR) THEN
  BEGIN
    DELETE FROM su_oprlinks WHERE ID_USERS=ID_SUS and ID_OPERATIONS=ID_OPR;
    
    /** Добавляем запись в протокол  **/
    SET @MESSAGE = Concat('"Пользователи системы" [su_oprlinks]: Операция ID="', ID_OPR,
      '", NAME="', (SELECT IfNull(NAME, '') FROM sr_operations WHERE ID=ID_OPR),
      '" исключена в список пользователя ID="', ID_SUS,
      '", NAME="', (SELECT IfNull(NAME, '') FROM su_users WHERE ID=ID_SUS),
      '", FULLNAME="', (SELECT IfNull(FULLNAME, '') FROM su_users WHERE ID=ID_SUS),
      '". [sp_sys_user_del_operation]');
    CALL sp_sys_add_log (ID_WPS, ID_USR, 9001, HOSTNAME, NETUSER, @MESSAGE);
  END; 
  END IF;
END
\\

CREATE PROCEDURE sp_sys_user_add_opgroup (ID_SUS INT, ID_OPG INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  INSERT INTO su_opglinks (ID_USERS, ID_OPGROUPS) VALUES (ID_SUS, ID_OPG);
  
  /** Добавляем запись в протокол  **/
  SET @MESSAGE = Concat('"Пользователи системы" [su_oprlinks]: Группа доступа ID="', ID_OPG,
    '", NAME="', (SELECT IfNull(NAME, '') FROM sr_opgroups WHERE ID=ID_OPG),
    '" включена в список пользователя ID="', ID_SUS,
    '", NAME="', (SELECT IfNull(NAME, '') FROM su_users WHERE ID=ID_SUS),
    '", FULLNAME="', (SELECT IfNull(FULLNAME, '') FROM su_users WHERE ID=ID_SUS),
    '". [sp_sys_user_add_opgroup]');
  CALL sp_sys_add_log (ID_WPS, ID_USR, 9001, HOSTNAME, NETUSER, @MESSAGE);
END
\\

CREATE PROCEDURE sp_sys_user_del_opgroup (ID_SUS INT, ID_OPG INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  IF EXISTS (SELECT * FROM su_opglinks WHERE ID_USERS=ID_SUS and ID_OPGROUPS=ID_OPG) THEN
  BEGIN
    DELETE FROM su_opglinks WHERE ID_USERS=ID_SUS and ID_OPGROUPS=ID_OPG;
    
    /** Добавляем запись в протокол  **/
    SET @MESSAGE = Concat('"Пользователи системы" [su_oprlinks]: Группа доступа ID="', ID_OPG,
      '", NAME="', (SELECT IfNull(NAME, '') FROM sr_opgroups WHERE ID=ID_OPG),
      '" исключена в список пользователя ID="', ID_SUS,
      '", NAME="', (SELECT IfNull(NAME, '') FROM su_users WHERE ID=ID_SUS),
      '", FULLNAME="', (SELECT IfNull(FULLNAME, '') FROM su_users WHERE ID=ID_SUS),
      '". [sp_sys_user_del_opgroup]');
    CALL sp_sys_add_log (ID_WPS, ID_USR, 9001, HOSTNAME, NETUSER, @MESSAGE);
  END; 
  END IF;
END
\\

CREATE PROCEDURE sp_sys_group_unlink_users (ID_OPG INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  DECLARE DONE INT DEFAULT FALSE;
  DECLARE ID_SUS INT;
  DECLARE CS_OPGLINKS CURSOR FOR SELECT ID_USERS FROM su_opglinks;
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE = TRUE;
  
  OPEN CS_OPGLINKS;
  WHILE NOT DONE DO
    FETCH CS_OPGLINKS INTO ID_SUS;
    
    IF NOT DONE THEN
      CALL sp_sys_user_del_opgroup (ID_SUS, ID_OPG, ID_WPS, ID_USR, HOSTNAME, NETUSER);
    END IF;
  END WHILE;
  CLOSE CS_OPGLINKS;
END
\\

CREATE PROCEDURE sp_sys_group_unlink_opers (ID_OPG INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  DECLARE DONE INT DEFAULT FALSE;
  DECLARE ID_OPR INT;
  DECLARE CS_OPGLINKS CURSOR FOR SELECT ID_OPERATIONS FROM sr_opglinks;
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE = TRUE;

  OPEN CS_OPGLINKS;
  WHILE NOT DONE DO
    FETCH CS_OPGLINKS INTO ID_OPR;
    
    IF NOT DONE THEN
      CALL sp_sys_group_del_operation (ID_OPG, ID_OPR, ID_WPS, ID_USR, HOSTNAME, NETUSER);
    END IF;
  END WHILE;
  CLOSE CS_OPGLINKS;
END
\\

CREATE PROCEDURE sp_sys_group_unlink_all (ID_OPG INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  CALL sp_sys_group_unlink_users (ID_OPG, ID_WPS, ID_USR, HOSTNAME, NETUSER);
  CALL sp_sys_group_unlink_opers (ID_OPG, ID_WPS, ID_USR, HOSTNAME, NETUSER);
END
\\

CREATE PROCEDURE sp_sys_user_unlink_opers (ID_SUS INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  DECLARE DONE INT DEFAULT FALSE;
  DECLARE ID_OPR INT;
  DECLARE CS_OPRLINKS CURSOR FOR SELECT ID_OPR FROM su_oprlinks;

  OPEN CS_OPRLINKS;
  WHILE NOT DONE DO
    FETCH CS_OPRLINKS INTO ID_OPR;

    IF NOT DONE THEN
      CALL sp_sys_user_del_operation (ID_SUS, ID_OPR, ID_WPS, ID_USR, HOSTNAME, NETUSER);
    END IF;
  END WHILE;
  CLOSE CS_OPRLINKS;
END
\\

CREATE PROCEDURE sp_sys_user_unlink_groups (ID_SUS INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  DECLARE DONE INT DEFAULT FALSE;
  DECLARE ID_OPG INT;
  DECLARE CS_OPGLINKS CURSOR FOR SELECT ID_OPGROUPS FROM su_opglinks;

  OPEN CS_OPGLINKS;
  WHILE NOT DONE DO
    FETCH CS_OPGLINKS INTO ID_OPG;

    IF NOT DONE THEN
      CALL sp_sys_user_del_opgroup (ID_SUS, ID_OPG, ID_WPS, ID_USR, HOSTNAME, NETUSER);
    END IF;
  END WHILE;
  CLOSE CS_OPGLINKS;
END
\\

CREATE PROCEDURE sp_sys_user_unlink_all (ID_SUS INT, ID_WPS INT, ID_USR INT, HOSTNAME VARCHAR(32), NETUSER VARCHAR(32))
BEGIN
  CALL sp_sys_user_unlink_opers (ID_SUS, ID_WPS, ID_USR, HOSTNAME, NETUSER);
  CALL sp_sys_user_unlink_groups (ID_SUS, ID_WPS, ID_USR, HOSTNAME, NETUSER);
END
\\

DELIMITER ;

/**** INITIAL DATA ****/

/**** SYSTEM SETTINGS ****/

INSERT INTO ss_vargroups (ID, OWNER_ID, NAME, NOTES) VALUES (1, NULL, 'Все параметры', NULL);
INSERT INTO ss_vargroups (ID, OWNER_ID, NAME, NOTES) VALUES (2, 1, 'База данных', 'Параметры базы данных');
INSERT INTO ss_vargroups (ID, OWNER_ID, NAME, NOTES) VALUES (3, 1, 'Подсистема безопасности', 'Параметры подсистемы безопасности');
INSERT INTO ss_vargroups (ID, OWNER_ID, NAME, NOTES) VALUES (4, 3, 'Политика паролей', 'Параметры паролей пользователей');
INSERT INTO ss_vargroups (ID, OWNER_ID, NAME, NOTES) VALUES (5, 3, 'Журнал аудита', 'Параметры журнала аудита системы');

INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (1000, 2, 0, NULL, 'Текущая версия базы данных');
INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (1002, 2, 1, '90', 'Срок действия коротких сообщений, дней');
INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (1102, 1, 1, '1024', 'Размер вложений в килобайтах, свыше которого запрашивается подтверждение');
INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (1103, 1, 1, '10240', 'Максимально допустимый размер вложений в килобайтах');
INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (9001, 5, 6, 'Включено', 'Состояние журнала аудита системы');
INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (9002, 5, 1, '1000000', 'Порог предупреждения о заполнении журнала, записей');
INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (9003, 4, 1, '40', 'Интервал в днях между очередной сменой пароля');
INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (9004, 4, 1, '6', 'Минимально допустимая длина пароля пользователя в символах');
INSERT INTO ss_varnames (ID, ID_GROUPS, TYPE, DEF_VALUE, NAME) VALUES (9005, 5, 1, '365', 'Период хранения записей журнала аудита системы, дней');

/**** DATABASE VERSION ****/

INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (1000, 1, NULL, NULL); 

/**** SYSTEM SETTINGS VALUES ****/

INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (1002, 90, NULL, NULL); 
INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (1102, 1024, NULL, NULL); 
INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (1103, 10240, NULL, NULL); 
INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9000, 0, NULL, NULL);
INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9001, 1, NULL, NULL);
INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9002, 1000000, NULL, NULL);
INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9003, 40, NULL, NULL);
INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9004, 6, NULL, NULL);
INSERT INTO ss_settings (ID, VALUE_INT, VALUE_REAL, VALUE_CHAR) VALUES (9005, 0, NULL, NULL);

/**** WORKPLASES ****/

INSERT INTO sr_workplases (ID, NAME_S, NAME) VALUES (1, 'АДМИНИСТРАТОР', 'Администратор системы');

/**** SECURITY LEVELS ****/

INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (100, 'Регистрация в системе', 'Проверка имени пользователя, пароля и прав доступа в систему.', 1, -1, 8388608, 16777194);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (101, 'Смена пароля', 'Смена пароля пользователем.', 1, -1, 16384, 16777194);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (110, 'Ошибки регистрации', 'Введено неверное имя пользователя, неверный пароль или отказ в доступе.', 1, 0, 213, 16777194);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (150, 'Чтение сообщений', 'Чтение коротких сообщений, отправляемых другими пользователями.', 0, -1, 8388608, 16777194);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (151, 'Создание сообщений', 'Создание коротких сообщений для других пользователей системы.', 0, -1, 8388608, 16777194);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (200, 'Просмотр', 'Просмотр данных или параметров без предоставления прав на изменение этих данных.', 0, -1, 16384, 15400938);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (300, 'Настройка справочников', 'Настройка рабочих справочников системы - просмотр и редактирование.', 0, -1, 16384, 15397375);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (400, 'Изменение данных A', 'Изменение данных категории доступа A.', 0, -1, 213, 15400959);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (410, 'Изменение данных B', 'Изменение данных категории доступа B.', 0, -1, 16512, 15400959);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (420, 'Изменение данных C', 'Изменение данных категории доступа C.', 0, -1, 128, 15400959);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (430, 'Изменение данных D', 'Изменение данных категории доступа D.', 0, -1, 8388608, 15400959);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (440, 'Изменение данных E', 'Изменение данных категории доступа E.', 0, -1, 16384, 15400959);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (480, 'Прочие операции', 'Прочие операции без непосредственного изменения данных в БД.', 0, -1, 16384, 15400938);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (490, 'Внешние операции', 'Выполнение операций без изменения данных в БД.', 0, -1, 16384, 15400938);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (500, 'Изменение параметров безопасности', 'Изменение данных или параметров, влияющих на уровень безопасности системы.', 0, -1, 213, 15395583);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (510, 'Изменение глобальных параметров', 'Изменение параметров, влияющих на функционирование системы в целом.', 0, -1, 64, 15395583);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (800, 'Редактирование отчетов', 'Создание или изменение отчетных форм, используемых при построении отчетов.', 0, -1, 8388608, 15400938);
INSERT INTO sr_levels (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) VALUES (999, 'Ошибка', 'Ошибки, регистрируемые в протоколе работы системы.', 1, 0, 65535, 8421631);

/**** SYSTEM OPERATIONS ****/

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9990, 100, 'Регистрация в системе', 'Проверка имени пользователя, пароля и прав доступа в систему.');
INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9991, 101, 'Смена пароля', 'Смена пароля пользователем.');
INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9995, 510, 'Обновление БД', 'Автоматическое обновление базы данных.');
INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9998, 999, 'Ошибка', 'Ошибка функционирования программы.');
INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9999, 110, 'Ошибка регистрации', 'Ошибка регистрации или попытка несанкционированного доступа в систему.');

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9980, 150, 'Чтение сообщений', 'Чтение коротких сообщений, отправляемых другими пользователями.');
INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9981, 151, 'Создание сообщений', 'Создание коротких сообщений для других пользователей системы.');

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (8000, 200, 'Просмотр параметров системы', 'Просмотр параметров системы, влияющих на все АРМы системы.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 8000);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (8001, 510, 'Изменение параметров системы', 'Изменение параметров системы, влияющих на все АРМы системы.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 8001);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (8201, 800, 'Редактирование отчетов', 'Создание или изменение отчетных форм, используемых при построении отчетов.');

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9000, 200, 'Просмотр групп доступа', 'Просмотр групп прав на выполнение операций в системе.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9000);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9001, 500, 'Управление группами доступа', 'Создание, изменение и удаление групп прав на выполнение операций в системе.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9001);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9100, 200, 'Просмотр списка пользователей', 'Просмотр списка пользователей и назначенных им прав на выполнение операций.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9100);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9101, 500, 'Управление списком пользователей', 'Создание, изменение и блокировка пользователей системы, назначение им прав на выполнение операций.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9101);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9102, 500, 'Блокировка и разблокировка пользователей', 'Установки и снятие блокировки на доступ пользователей в систему.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9102);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9104, 500, 'Редактирование групп пользователей', 'Создание, изменение и удаление условных групп пользователей.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9104);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9200, 200, 'Просмотр параметров безопасности', 'Просмотр параметров, влияющих на уровень безопасности системы.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9200);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9201, 500, 'Редактирование параметров безопасности', 'Редактирование параметров, влияющих на уровень безопасности системы.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9201);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9210, 200, 'Просмотр протокола системы', 'Просмотр записей системного протокола работы, выгрузка в файл и печать отчета.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9210);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9211, 500, 'Очистка протокола системы', 'Удаление записей из системного протокола работы.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9211);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9220, 200, 'Просмотр цветовых схем протокола', 'Просмотр справочника уровней безопасности операций.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9220);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9221, 300, 'Редактирование цветовых схем протокола', 'Настройка справочника (в части редактирования цветовых схем) уровней безопасности операций.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9221);

INSERT INTO sr_operations (ID, ID_LEVELS, NAME, NOTES) VALUES (9222, 200, 'Просмотр списка операций системы', 'Просмотр справочника операций системы.');
INSERT INTO sr_opwlinks (ID_WORKPLASES, ID_OPERATIONS) VALUES (1, 9222);

/**** APP OPERATIONS ****/

/**** ADMIN ROLES ****/

INSERT INTO sr_opgroups (ID, NAME, NOTES) VALUES (1, 'АДМИНИСТРИРОВАНИЕ СИСТЕМЫ', 'Управление правами на выполнение операций и группами прав, управление пользователями системы, настройка глобальных параметров системы.');
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 8000);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 8001);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9000);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9001);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9200);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9201);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9210);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9211);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9220);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9221);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (1, 9222);

INSERT INTO sr_opgroups (ID, NAME, NOTES) VALUES (2, 'УПРАВЛЕНИЕ ПОЛЬЗОВАТЕЛЯМИ СИСТЕМЫ', 'Управление пользователями системы, предоставление пользователям прав на выполнение операций.');
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (2, 9100);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (2, 9101);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (2, 9102);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (2, 9104);

INSERT INTO sr_opgroups (ID, NAME, NOTES) VALUES (3, 'КОНТРОЛЬ РАБОТЫ СИСТЕМЫ', 'Контроль работы и настроек всей системы.');
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 8000);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 9000);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 9100);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 9200);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (3, 9210);

INSERT INTO sr_opgroups (ID, NAME, NOTES) VALUES (4, 'СООБЩЕНИЯ', 'Прием и отправка сообщений внутри системы.');
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (4, 9980);
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (4, 9981);

INSERT INTO sr_opgroups (ID, NAME, NOTES) VALUES (5, 'РЕДАКТИРОВАНИЕ ОТЧЕТОВ', 'Редактирование шаблонов отчетов Fast Reports.');
INSERT INTO sr_opglinks (ID_OPGROUPS, ID_OPERATIONS) VALUES (5, 8201);

/**** APP ROLES ****/

/**** DEFAULT USERS ****/

INSERT INTO su_groups VALUES (1, NULL, 'Все пользователи', NULL);
INSERT INTO su_groups VALUES (2, 1, 'Администраторы', 'Администраторы системы');
INSERT INTO su_groups VALUES (3, 1, 'Пользователи', 'Пользователи системы');

INSERT INTO su_users (ID, ID_GROUPS, NAME, FULLNAME, NOTES, PASSWORD, DELETED, BLOCKED, COUNT_EP) VALUES (0, 1, 'system', 'Database account', '', '', 1, 1, 0);
INSERT INTO su_users (ID, ID_GROUPS, NAME, FULLNAME, NOTES, PASSWORD, DELETED, BLOCKED, COUNT_EP) VALUES (1, 2, 'admin', 'Администратор системы', 'Встроенная учетная запись администратора системы.', '', 0, 0, 0);

INSERT INTO su_opglinks (ID_USERS, ID_OPGROUPS) VALUES (1, 1);
INSERT INTO su_opglinks (ID_USERS, ID_OPGROUPS) VALUES (1, 2);
INSERT INTO su_opglinks (ID_USERS, ID_OPGROUPS) VALUES (1, 3);
INSERT INTO su_opglinks (ID_USERS, ID_OPGROUPS) VALUES (1, 4);
INSERT INTO su_opglinks (ID_USERS, ID_OPGROUPS) VALUES (1, 5);

/**** DONE ****/