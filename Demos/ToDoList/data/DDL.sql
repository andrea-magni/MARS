/********************* ROLES **********************/

CREATE ROLE RDB$ADMIN;
/********************* UDFS ***********************/

/****************** GENERATORS ********************/

CREATE GENERATOR GEN_ACCOUNT_ID;
CREATE GENERATOR GEN_ITEMS_ID;
/******************** DOMAINS *********************/

/******************* PROCEDURES ******************/

/******************** TABLES **********************/

CREATE TABLE ACCOUNT
(
  ID Integer NOT NULL,
  USERNAME Varchar(100) NOT NULL,
  PWD_HASH Varchar(256) NOT NULL,
  FIRST_NAME Varchar(100),
  LAST_NAME Varchar(100),
  ROLES Varchar(100),
  LAST_LOGIN Timestamp,
  "ACTIVE" Smallint,
  CONSTRAINT PK_ACCOUNT PRIMARY KEY (ID)
);
CREATE TABLE ITEMS
(
  ID Integer NOT NULL,
  OWNER_ID Integer,
  TEXT Varchar(1024),
  DONE Smallint,
  CREATION_DATE Timestamp,
  DONE_DATE Timestamp,
  CONSTRAINT PK_ITEMS_ID PRIMARY KEY (ID)
);
/********************* VIEWS **********************/

/******************* EXCEPTIONS *******************/

/******************** TRIGGERS ********************/

SET TERM ^ ;
CREATE TRIGGER ACCOUNT_BI FOR ACCOUNT ACTIVE
BEFORE INSERT POSITION 0
AS
DECLARE VARIABLE tmp DECIMAL(18,0);
BEGIN
  IF (NEW.ID IS NULL) THEN
    NEW.ID = GEN_ID(GEN_ACCOUNT_ID, 1);
  ELSE
  BEGIN
    tmp = GEN_ID(GEN_ACCOUNT_ID, 0);
    if (tmp < new.ID) then
      tmp = GEN_ID(GEN_ACCOUNT_ID, new.ID-tmp);
  END
END^
SET TERM ; ^
SET TERM ^ ;
CREATE TRIGGER ITEMS_BI FOR ITEMS ACTIVE
BEFORE INSERT POSITION 0
AS
DECLARE VARIABLE tmp DECIMAL(18,0);
BEGIN
  IF (NEW.ID IS NULL) THEN
    NEW.ID = GEN_ID(GEN_ITEMS_ID, 1);
  ELSE
  BEGIN
    tmp = GEN_ID(GEN_ITEMS_ID, 0);
    if (tmp < new.ID) then
      tmp = GEN_ID(GEN_ITEMS_ID, new.ID-tmp);
  END
END^
SET TERM ; ^

ALTER TABLE ITEMS ADD CONSTRAINT FK_ITEMS_OWNER
  FOREIGN KEY (OWNER_ID) REFERENCES ACCOUNT (ID) ON UPDATE CASCADE ON DELETE CASCADE;
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON ACCOUNT TO  SYSDBA WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON ITEMS TO  SYSDBA WITH GRANT OPTION;

