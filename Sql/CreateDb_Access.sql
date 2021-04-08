CREATE TABLE version (
  [version] INT NOT NULL ,
  constraint pk_version_id PRIMARY KEY ([version])
);

CREATE TABLE ss_settings (
  [id] INT NOT NULL ,
  [value_int] INT NULL ,
  [value_real] REAL NULL ,
  [value_char] VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_settings_id PRIMARY KEY ([id])
);

CREATE TABLE ss_reports (
  [id] INT NOT NULL ,
  [form] VARCHAR(32) NOT NULL ,
  [report] IMAGE NULL ,
  [name] VARCHAR(64) NULL ,
  [notes] VARCHAR(255) NULL ,
  CONSTRAINT pk_ss_reports_id PRIMARY KEY ([id])
);

CREATE TABLE ss_attachments (
  [id] INT NOT NULL ,
  [object_name] VARCHAR(32) NOT NULL,
  [object_id] INT NOT NULL,
  [filename] VARCHAR(128) NULL ,
  [filedata] IMAGE NULL ,
  [filetime] DATETIME NULL ,
  [filesize] INT NULL ,
  constraint pk_ss_attachments_id PRIMARY KEY ([id])
);

CREATE INDEX lk_ss_attachments_object_name ON ss_attachments ([object_name]);
CREATE INDEX lk_ss_attachments_object_id ON ss_attachments ([object_name], [object_id]);

