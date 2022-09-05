/*==============================================================*/
/* Database name:  MySql                                        */
/* DBMS name:      MySQL 3.23                                   */
/* Created on:     04.02.2003 19:48:39                          */
/*==============================================================*/

use zeoslib;

/*==============================================================*/
/* Tables for Bug#735226                                        */
/*==============================================================*/

CREATE TABLE table735226a
(
  id INT NOT NULL,
  referenceid INT NOT NULL
);

CREATE TABLE table735226b
(
  id INT NOT NULL
);

/*==============================================================*/
/* Tables for Bug#726788                                        */
/*==============================================================*/

CREATE TABLE Table726788
(
  FieldName INT NOT NULL
);

/*==============================================================*/
/* Tables for Bug#735299                                        */
/*==============================================================*/

CREATE TABLE table735299
(
  id INT NOT NULL,
  fld1 ENUM('Y','N'),
  fld2 enum('n','y'),
  PRIMARY KEY (id)
);

/*==============================================================*/
/* Tables for Bug#740899                                        */
/*==============================================================*/

CREATE TABLE table740899
(
  id INT NOT NULL,
  fld VARCHAR(35)
);

/*==============================================================*/
/* Tables for Bug #724542                                       */
/*==============================================================*/

CREATE TABLE table724542
(
  fld1 int(11),
  fld2 char(30),
  fld3 char(10)
);

/*==============================================================*/
/* Tables for Bug #739448                                       */
/*==============================================================*/

CREATE TABLE table739448a
(
  fld1 int(11),
  fld2 char(30),
  fld3 char(10)
);

insert into table739448a values (1, 'abcdefghi', '123456789');

CREATE TABLE table739448b
(
  fld1 int(11),
  fld2 char(30),
  fld3 char(10)
);

insert into table739448b values(1, 'abcdefghi', '123456789');

/*==============================================================*/
/* Tables for Bug #733236                                       */
/*==============================================================*/

CREATE TABLE table733236
(
  date DATE,
  time TIME,
  datetime DATETIME,
  timestamp TIMESTAMP
);

INSERT INTO table733236 VALUES("1998-12-31", "23:59:59", "1998-12-31 23:59:59",19981231235959);
INSERT INTO table733236 VALUES("1999-01-01", "00:00:00", "1999-01-01 00:00:00",19990101000000);
INSERT INTO table733236 VALUES("2000-01-01", "00:00:00", "2000-01-01 00:00:00",20000101000000);
INSERT INTO table733236 VALUES("2001-01-01", "00:00:00", "2001-01-01 00:00:00",20010101000000);
INSERT INTO table733236 VALUES("2004-12-31", "23:59:59", "2004-12-31 23:59:59",20041231235959);

/*==============================================================*/
/* Tables for Bug #768163                                       */
/*==============================================================*/

create table table768163
(
  fld1 int unsigned
);

/*==============================================================*/
/* Tables for Bug #799863                                       */
/*==============================================================*/

create table table799863
(
  fld1 year
);

insert into table799863 values (1024);
insert into table799863 values (1940);
insert into table799863 values (2003);

/*==============================================================*/
/* Tables for Bug #000001                                       */
/*==============================================================*/

create table table000001
(
  fld1 longtext,
  fld2 longblob
);

/*==============================================================*/
/* Tables for Bug #817607                                       */
/*==============================================================*/

create table table817607
(
  id int,
  `fruit name` varchar(10)
);

insert into table817607 values (1, 'apple');
insert into table817607 values (2, 'cherry');
insert into table817607 values (3, 'mango');

/*==============================================================*/
/* Tables for Bug #816925                                       */
/*==============================================================*/

create table table816925
(
  fld1 decimal(5,0),
  fld2 decimal(9,2),
  fld3 numeric(11,0),
  fld4 numeric(8,3)
);

/*==============================================================*/
/* Tables for Bug #828147                                       */
/*==============================================================*/

create table table828147
(
  id int4,
  txt text
);

insert into table828147 values (1, 'abc');

/*==============================================================*/
/* Tables for Bug #840608                                       */
/*==============================================================*/

CREATE TABLE table840608 (
`icode` varchar(7) default NULL,
`name` varchar(100) default NULL,
`strength` varchar(15) default NULL,
`units` varchar(50) default NULL,
`unitprice` double(15,3) default NULL,
`dosageform` varchar(100) default NULL,
`criticalpriority` int(11) default NULL,
`drugaccount` char(1) default NULL,
`drugcategory` varchar(150) default NULL,
`drugnote` varchar(150) default NULL,
`hintcode` char(2) default NULL,
`istatus` char(1) default NULL,
`lastupdatestdprice` datetime default NULL,
`lockprice` char(1) default NULL,
`lockprint` char(1) default NULL,
`maxlevel` int(11) default NULL,
`minlevel` int(11) default NULL,
`maxunitperdose` int(11) default NULL,
`packqty` int(11) default NULL,
`reorderqty` int(11) default NULL,
`stdprice` double(15,3) default NULL,
`stdtaken` varchar(30) default NULL,
`therapeutic` varchar(150) default NULL,
`therapeuticgroup` varchar(150) default NULL,
`default_qty` int(11) default NULL,
UNIQUE KEY `icode_unique` (`icode`),
UNIQUE KEY `units_icode_name_unique` 
(`units`,`icode`,`name`,`packqty`,`strength`),
KEY `dosageform` (`dosageform`),
KEY `drugaccount` (`drugaccount`),
KEY `drugcategory` (`drugcategory`),
KEY `drugnote` (`drugnote`),
KEY `hintcode` (`hintcode`),
KEY `icode` (`icode`),
KEY `istatus` (`istatus`),
KEY `name` (`name`),
KEY `packqty` (`packqty`),
KEY `strength` (`strength`),
KEY `therapeutic` (`therapeutic`),
KEY `therapeuticgroup` (`therapeuticgroup`),
KEY `units` (`units`)
);

INSERT INTO table840608
(icode,name,strength,units,unitprice,dosageform,criticalpriority,
drugaccount,drugcategory,drugnote,hintcode,istatus,
lastupdatestdprice,lockprice,lockprint,maxlevel,minlevel,
maxunitperdose,packqty,reorderqty,stdprice,stdtaken,therapeutic,
therapeuticgroup,default_qty) VALUES 
('1450012','PARACETAMOL (GPO)','500 mg.','TAB',0.5,'Tablets',0,'',
NULL,'p5','58','N',NULL,'N','N',1,1,0,1000,1,NULL,'0153',
'ANTI-PYRETIC','',NULL);


/*==============================================================*/
/* Tables for Bug #849723                                       */
/*==============================================================*/

CREATE TABLE table849723 (
  fld1 time not null,
  fld2 varchar(10),
  primary key (fld1)
);

INSERT INTO table849723 values ('00:00:00', 'abc');

/*==============================================================*/
/* Tables for Bug #869609                                       */
/*==============================================================*/

CREATE TABLE table869609 (
  id int8 not null auto_increment,
  primary key (id)
);

/*==============================================================*/
/* Tables for Bug #865564                                       */
/*==============================================================*/

CREATE TABLE table865564 (
  fld1 float(30,2),
  fld2 decimal(10,4)
);

INSERT INTO table865564 VALUES (123.45, 123.4567);

/*==============================================================*/
/* Tables for Bug #881634                                       */
/*==============================================================*/

CREATE TABLE table881634a (
  idt1 int4 not null,
  ft1 varchar(20),
  PRIMARY KEY (idt1)
);

CREATE TABLE table881634b (
  idt2 int4 not null,
  ft2 varchar(20),
  ft1 integer,
  PRIMARY KEY (idt2)
);

/*==============================================================*/
/* Tables for Bug #884135                                       */
/*==============================================================*/

CREATE TABLE table884135a (
  id int4 unsigned auto_increment not null,
  fld varchar(20),
  PRIMARY KEY (id)
);

INSERT INTO table884135a VALUES (1, 'aaa');
INSERT INTO table884135a VALUES (2, 'bbb');
INSERT INTO table884135a VALUES (3, 'ccc');

CREATE TABLE table884135b (
  id int4 unsigned auto_increment not null,
  mid int4 unsigned,
  fld varchar(20),
  PRIMARY KEY (id)
);

INSERT INTO table884135b VALUES (1, 1, 'aaaa');
INSERT INTO table884135b VALUES (2, 1, 'aabb');
INSERT INTO table884135b VALUES (3, 1, 'aacc');
INSERT INTO table884135b VALUES (4, 2, 'bbaa');
INSERT INTO table884135b VALUES (5, 2, 'bbbb');
INSERT INTO table884135b VALUES (6, 2, 'bbcc');
INSERT INTO table884135b VALUES (7, 3, 'ccaa');
INSERT INTO table884135b VALUES (8, 3, 'ccbb');
INSERT INTO table884135b VALUES (9, 3, 'cccc');

/*==============================================================*/
/* Tables for Bug #886841                                       */
/*==============================================================*/

CREATE TABLE table886841 (
  fld enum('y','n') default 'y'  
);

/*==============================================================*/
/* Tables for Bug #894367                                       */
/*==============================================================*/

CREATE TABLE table894367a (
  fld1 varchar(30),
  fld2 enum('y','n')
);

CREATE TABLE table894367b (
  fld1 int4,
  fld2 float
);

CREATE TABLE table894367c (
  fld1 tinyblob,
  fld2 int8
); 

/*==============================================================*/
/* Tables for Bug #886841                                       */
/*==============================================================*/
/*
CREATE TABLE table914436 (
  fld1 CHAR(15) CHARACTER SET LATIN1,
  fld2 VARCHAR(15) CHARACTER SET LATIN1,
  fld3 TEXT(15) CHARACTER SET LATIN1
);
/**/

/*==============================================================*/
/* Tables for Bug #938705                                       */
/*==============================================================*/

CREATE TABLE `Table 938705` (
  `Field First` int4 not null auto_increment,
  `Field Second` varchar(10),
  primary key (`Field First`)
);

/*==============================================================*/
/* Tables for Bug #957126                                       */
/*==============================================================*/
CREATE TABLE table957126 (
  fld1 VARCHAR(15) DEFAULT '',
  fld2 VARCHAR(15)
);

/*==============================================================*/
/* Tables for Bug #987022                                       */
/*==============================================================*/
CREATE TABLE table987022 (
  fld1 BIGINT,
  fld2 VARCHAR(15)
);

/*==============================================================*/
/* Tables for Bug #989474                                       */
/*==============================================================*/
CREATE TABLE table989474 (
  CustID INT4,
  CreateDate DATE,
  PRIMARY KEY (CustID)
);

/*==============================================================*/
/* Tables for Bug #1045286                                      */
/*==============================================================*/
CREATE TABLE table1045286 (
  fld text not null
);

INSERT INTO table1045286 VALUES ('');
