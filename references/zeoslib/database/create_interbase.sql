/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values (
b_id                 INTEGER                        not null,
b_text               BLOB SUB_TYPE TEXT,
b_image              BLOB,
primary key (b_id)
);

/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 INTEGER                        not null,
c_dep_id             SMALLINT,
c_name               CHAR(10),
c_seal               SMALLINT,
c_date_came          TIMESTAMP,
c_date_out           TIMESTAMP,
c_weight             FLOAT,
c_width              INTEGER,
c_height             INTEGER,
c_cost               NUMERIC(12,4),
c_attributes         BLOB SUB_TYPE TEXT,
primary key (c_id)
);

/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values (
d_id                 INTEGER                        not null,
d_date               DATE,
d_time               TIME,
d_datetime           TIMESTAMP,
d_timestamp          TIMESTAMP,
primary key (d_id)
);

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               SMALLINT                       not null,
dep_name             VARCHAR(20),
dep_shname           CHAR(5),
dep_address          VARCHAR(255),
primary key (dep_id)
);

/*==============================================================*/
/* View : department                                            */
/*==============================================================*/
create view dep_view as select * from department;

/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                INTEGER                        not null,
eq_name              VARCHAR(30),
eq_type              SMALLINT,
eq_cost              NUMERIC(9,4),
eq_date              DATE,
woff_date            DATE,
primary key (eq_id)
);

/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               SMALLINT                       not null,
eq_id                INTEGER                        not null,
primary key (dep_id, eq_id)
);

/*==============================================================*/
/* Table : number_values                                        */
/*==============================================================*/
create table number_values (
n_id                 INTEGER                        not null,
n_tint               SMALLINT,
n_sint               SMALLINT,
n_int                INTEGER,
n_bdecimal           DECIMAL(18),
n_numeric            NUMERIC(9,4),
n_float              FLOAT,
n_real               FLOAT,
n_dprecission        DOUBLE PRECISION,
n_money              NUMERIC(8,2),
primary key (n_id)
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 SMALLINT                       not null,
p_dep_id             SMALLINT,
p_name               VARCHAR(40),
p_begin_work         TIME,
p_end_work           TIME,
p_picture            BLOB,
p_resume             BLOB SUB_TYPE TEXT,
p_redundant          SMALLINT,
primary key (p_id)
);

/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values
(
   s_id                           int              not null,
   s_char                         CHAR(255),
   s_varchar                      VARCHAR(255),
   s_nchar                        CHAR(255),
   s_nvarchar                     VARCHAR(255),
   s_bit                          BLOB SUB_TYPE TEXT,
   s_varbit                       BLOB,
   primary key (s_id)
);

/*==============================================================*/
/* Table : Case_Sensitive                                       */
/*==============================================================*/
create table "Case_Sensitive" (
cs_id                 INTEGER                        not null,
"Cs_Data1"            INTEGER,
"cs_data1"            INTEGER,
"cs data1"            INTEGER,
primary key (cs_id)
);

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
create table case_sensitive (
cs_id                 INTEGER                        not null,
"CS_DATA1"            INTEGER,
"CS_Data2"            INTEGER,
"Cs_Data3"            INTEGER,
primary key (cs_id)
);

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INTEGER NOT NULL,
data1		      FLOAT,
data2		      CHAR(10),
primary key (hl_id)
);

/*==============================================================*/
/* Table : default_values                                       */
/*==============================================================*/
create table default_values
(
   d_id                           INTEGER NOT NULL,
   d_fld1                         INTEGER DEFAULT 123456,
   d_fld2                         FLOAT DEFAULT 123.456,
   d_fld3                     	  VARCHAR(10) DEFAULT 'xyz',
   d_fld4                     	  DATE default '2003-12-11',
   d_fld5                     	  TIME default '23:12:11',
   d_fld6                     	  TIMESTAMP default '2003-12-11 23:12:11',
   primary key (d_id)
);


/*==============================================================*/
/* Table : domain_values                                        */
/*==============================================================*/

create domain tinteger INT default 123456;
create domain tfloat FLOAT default 123.456;
create domain tstring VARCHAR(10) default 'xyz';

create table domain_values
(
   d_id                       INTEGER not null,
   d_fld1                     tinteger,
   d_fld2                     tfloat,
   d_fld3                     tstring,
   primary key (d_id)
);


alter table cargo
   add foreign key (c_dep_id) references department (dep_id);

alter table equipment2
   add foreign key (dep_id) references department (dep_id);

alter table equipment2
   add foreign key (eq_id) references equipment (eq_id);

alter table people
   add foreign key (p_dep_id) references department (dep_id);

SET TERM ^ ;

/*==============================================================*/
/* Stored procedure: procedure1                                 */
/*==============================================================*/

CREATE PROCEDURE PROCEDURE1(P1 INTEGER)
   RETURNS(R1 INTEGER)
AS
BEGIN
  R1 = P1 + 1;
SUSPEND;
END
^


/*==============================================================*/
/* Stored procedure: procedure2                                 */
/*==============================================================*/

CREATE PROCEDURE PROCEDURE2
   RETURNS(R1 VARCHAR(30))
AS
BEGIN
  FOR SELECT eq_name FROM equipment ORDER BY eq_name INTO :R1
  DO
  SUSPEND;
END
^
SET TERM ; ^

/*==============================================================*/
/* Grant privileges to columns                                  */
/*==============================================================*/
grant update(p_resume, p_redundant) on people to SYSDBA;

/*==============================================================*/
/* Grant privileges to table                                    */
/*==============================================================*/
grant select on people to SYSDBA;
