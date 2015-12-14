
/****************** GENERATORS ********************/

CREATE GENERATOR GEN_ITEMS;
/******************** DOMAINS *********************/

/******************* PROCEDURES ******************/

/******************** TABLES **********************/

CREATE TABLE ACCOUNT
(
  USERNAME Varchar(100) NOT NULL,
  PWD Varchar(256) NOT NULL,
  LAST_LOGIN Timestamp,
  IS_ADMIN Smallint,
  CONSTRAINT PK_ACCOUNT_USERNAME PRIMARY KEY (USERNAME)
);
CREATE TABLE ITEMS
(
  ID Integer NOT NULL,
  OWNER Varchar(100),
  TEXT Varchar(1024),
  DONE Smallint,
  CREATION_DATE Timestamp,
  DONE_DATE Timestamp,
  CONSTRAINT PK_ITEMS_ID PRIMARY KEY (ID)
);
/********************* VIEWS **********************/

/******************* EXCEPTIONS *******************/

/******************** TRIGGERS ********************/


ALTER TABLE ITEMS ADD CONSTRAINT FK_ITEMS_USERNAME
  FOREIGN KEY (OWNER) REFERENCES ACCOUNT (USERNAME) ON UPDATE CASCADE ON DELETE CASCADE;
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON ACCOUNT TO  SYSDBA WITH GRANT OPTION;

GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON ITEMS TO  SYSDBA WITH GRANT OPTION;

