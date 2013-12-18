/*==============================================================*/
/* Database name:  zeoslib                                      */
/* Created on:     28.12.2002 20:27:07                          */
/*==============================================================*/

alter session set NLS_DATE_FORMAT = 'YYYY-MM-DD';
alter session set NLS_TIMESTAMP_FORMAT = 'YYYY-MM-DD HH24:MI:SS';

INSERT INTO department VALUES (3,'Delivery agency','UKR','Donetsk Artema st. 113');
INSERT INTO department VALUES (2,'Container agency','USA','Krasnodar Komsomolskaya st. 17');
INSERT INTO department VALUES (1,'Line agency','RUS','Novorossiysk Lenina st. 2');

INSERT INTO equipment VALUES (1,'Volvo',1,15000.0000,'1998-03-04',NULL);
INSERT INTO equipment VALUES (2,'Laboratoy',10,40000.0000,'2001-10-07',NULL);
INSERT INTO equipment VALUES (3,'Computer',7,900.0000,'1999-09-03',NULL);
INSERT INTO equipment VALUES (4,'Radiostation',19,400.0000,'2000-07-08',NULL);

INSERT INTO equipment2 VALUES (1,1);
INSERT INTO equipment2 VALUES (1,2);
INSERT INTO equipment2 VALUES (1,4);
INSERT INTO equipment2 VALUES (2,1);
INSERT INTO equipment2 VALUES (2,3);

INSERT INTO people VALUES (1,1,'Vasia Pupkin','0001-01-01 09:00:00','0001-01-01 18:00:00',NULL,NULL,0);
INSERT INTO people VALUES (2,2,'Andy Karto','0001-01-01 08:30:00','0001-01-01 17:30:00',NULL,NULL,0);
INSERT INTO people VALUES (3,1,'Kristen Sato','0001-01-01 09:00:00','0001-01-01 18:00:00',NULL,NULL,0);
INSERT INTO people VALUES (4,2,'Aleksey Petrov','0001-01-01 08:30:00','0001-01-01 17:30:00',NULL,NULL,1);
INSERT INTO people VALUES (5,3,'Yan Pater','0001-01-01 08:00:00','0001-01-01 17:00:00',NULL,NULL,1);

INSERT INTO cargo VALUES (1,2,'Grain',1,'2002-12-20 02:00:00','2002-12-20 02:00:00',5000,NULL,NULL,1769.4300,NULL);
INSERT INTO cargo VALUES (2,1,'Paper',2,'2002-12-19 14:00:00','2002-12-23 00:00:00',1000,10,10,986.4700,'#14#СЃав2');
INSERT INTO cargo VALUES (3,1,'Wool',0,'2002-12-20 18:00:00',NULL,400,7,4,643.1100,NULL);
INSERT INTO cargo VALUES (4,2,'Suagr',1,'2002-12-21 10:20:00','2002-12-26 00:00:00',2034,NULL,NULL,1964.8700,NULL);
