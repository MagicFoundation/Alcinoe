alter session set NLS_DATE_FORMAT = 'YYYY-MM-DD';
alter session set NLS_TIMESTAMP_FORMAT = 'YYYY-MM-DD HH24:MI:SS';

/*==============================================================*/
/* Table for check Numbers wo Precission/Scale                  */
/*==============================================================*/
create table Table_Num1 (
id                 NUMBER(9, 0) not null,
Num		   NUMBER
);

INSERT INTO Table_Num1 VALUES(1, 54321.0123456789);


