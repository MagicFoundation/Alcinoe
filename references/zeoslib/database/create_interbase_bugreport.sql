/*==============================================================*/
/* Tables for Bug#789879                                        */
/*==============================================================*/

CREATE TABLE TABLE789879 (
  FLD NUMERIC(10,2)
);

/*==============================================================*/
/* Tables for Bug#841559                                        */
/*==============================================================*/
CREATE TABLE TABLE841559 (
  FLD1 INTEGER NOT NULL,
  FLD2 VARCHAR(50)
);
ALTER TABLE TABLE841559 ADD CONSTRAINT
PK_PARTICOLARI PRIMARY KEY (FLD1);

CREATE EXCEPTION EXCEPTION841559 'Just exception EXCEPTION841559';

SET TERM ^ ;
CREATE TRIGGER TRIGGER841559 FOR TABLE841559
ACTIVE BEFORE INSERT POSITION 0
AS
begin
  IF ((new.FLD2 IS NULL) OR (new.FLD2 =''))
  THEN EXCEPTION EXCEPTION841559;
end
^
SET TERM ; ^

/*==============================================================*/
/* Tables for Bug#864622                                        */
/*==============================================================*/
CREATE TABLE TABLE864622 (
  FLD1 INTEGER NOT NULL,
  FLD2 NUMERIC(3,1)
);

INSERT INTO TABLE864622 (FLD1, FLD2) VALUES(1, 1.2);


/*==============================================================*/
/* Tables for Bug#865441                                        */
/*==============================================================*/
CREATE TABLE TABLE865441 (
  ID INTEGER NOT NULL,
  "PASSWORD" VARCHAR(50)
);

/*==============================================================*/
/* Tables for Bug#886914                                        */
/*==============================================================*/
CREATE TABLE TABLE886914 (
  ID INTEGER NOT NULL,
  DESCRIPTION VARCHAR(30) CHARACTER SET NONE,
  FLAG CHAR(2) CHARACTER SET NONE
);

ALTER TABLE TABLE886914 ADD CONSTRAINT PK_TABLE886914 PRIMARY KEY (ID);

/*==============================================================*/
/* Tables for Bug#886194                                        */
/*==============================================================*/
CREATE TABLE TABLE886194(
  FLD1 varchar(50) CHARACTER SET WIN1252 COLLATE PXW_INTL850,
  FLD2 varchar(90) CHARACTER SET WIN1252 COLLATE PXW_INTL850
);

/*==============================================================*/
/* Tables for Bug#897631                                        */
/*==============================================================*/

CREATE TABLE TABLE897631 (
  FLD NUMERIC(15,2)
);

/*==============================================================*/
/* Tables for Bug#909181                                        */
/*==============================================================*/

CREATE TABLE TABLE909181 (
    FLD1     CHAR(2) NOT NULL,
    FLD2     INTEGER NOT NULL,
    FLD3     DATE NOT NULL
);

/*==============================================================*/
/* Tables for Bug#920450                                        */
/*==============================================================*/

CREATE TABLE TABLE920450 (
    FLD1     CHAR(1)
);

/*==============================================================*/
/* Tables for Bug#1021705                                       */
/*==============================================================*/

CREATE TABLE TABLE1021705 (
   ID INTEGER NOT NULL,
   FLD1 NUMERIC (10, 6),
   FLD2 NUMERIC (9, 6),
   PRIMARY KEY(ID)
);
