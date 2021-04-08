DROP TABLE sm_history;
DROP TABLE sm_messages;

CREATE TABLE sm_messages (
  id INT NOT NULL ,
  id_users INT NOT NULL ,
  sended DATETIME NOT NULL ,
  title VARCHAR(255) NULL ,
  address VARCHAR(255) NULL ,
  message MEDIUMTEXT NULL ,
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
