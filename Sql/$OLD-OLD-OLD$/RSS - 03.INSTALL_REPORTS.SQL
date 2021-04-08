CREATE TABLE dbo.SS_REPORTS (
	ID int NOT NULL ,
	FORM str32 NOT NULL ,
	REPORT image NULL ,
	NAME str64 NULL ,
	NOTES str255 NULL ,
	CONSTRAINT PK_SS_REPORTS_ID PRIMARY KEY  CLUSTERED (ID)
) 
GO

GRANT  REFERENCES ,  SELECT ,  INSERT ,  DELETE ,  UPDATE  ON SS_REPORTS  TO public
GO

INSERT INTO SR_LEVELS (ID, NAME, NOTES, HIDDEN, FONT_STYLE, FONT_COLOR, CELL_COLOR) 
VALUES (800, 'Редактирование отчетов', 'Создание или изменение отчетных форм, используемых при построении отчетов.', 0, -1, 8388608, 15400938)
GO

INSERT INTO SR_OPERATIONS (ID, ID_LEVELS, NAME, NOTES) VALUES (8201, 800, 'Редактирование отчетов', 'Создание или изменение отчетных форм, используемых при построении отчетов.')
GO
