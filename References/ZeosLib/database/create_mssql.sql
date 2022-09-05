/*==============================================================*/
/* Database name:  MS SQL                                       */
/* DBMS name:      Microsoft SQL Server 7.x                     */
/* Created on:     04.02.2003 20:01:43                          */
/*==============================================================*/


/*==============================================================*/
/* Script to create databse                                     */
/*==============================================================*/
/*
create database zeoslib 
ON PRIMARY
( NAME = zeoslib_Data,
   FILENAME = 'C:\Program Files\Microsoft SQL Server\MSSQL\Data\zeoslib_Data.MDF',
   SIZE = 10,
   FILEGROWTH = 15%
)
LOG ON
( NAME = 'zeoslib_Log',
   FILENAME = 'C:\Program Files\Microsoft SQL Server\MSSQL\Data\zeoslib_Log.LDF',
   SIZE = 5,
   FILEGROWTH = 5MB 
)
COLLATE SQL_Latin1_General_Cp1251_CS_AS
*/

set quoted_identifier on
go

/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values (
b_id                 int                  not null,
b_text               text                 null,
b_image              image                null,
primary key  (b_id)
)
go


/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               smallint             not null,
dep_name             varchar(20)          null,
dep_shname           char(5)              null,
dep_address          varchar(255)         null,
primary key  (dep_id)
)
go


/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 int                  not null,
c_dep_id             smallint             null,
c_name               char(10)             null,
c_seal               tinyint              null,
c_date_came          datetime             null,
c_date_out           datetime             null,
c_weight             float                null,
c_width              int                  null,
c_height             int                  null,
c_cost               money                null,
c_attributes         binary(10)           null,
primary key  (c_id)
)
go


create   index cargo_FK on cargo (c_dep_id)
go

alter table cargo
   add foreign key (c_dep_id) references department (dep_id)
go


/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values (
d_id                 int                  not null,
d_date               datetime             null,
d_time               datetime             null,
d_datetime           datetime             null,
d_timestamp          datetime             null,
constraint PK_DATE_VALUES primary key  (d_id)
)
go

/*==============================================================*/
/* Table : default_values                                       */
/*==============================================================*/
create table default_values(
   d_id                           int 	      not null,
   d_fld1                         int 	      default 123456,
   d_fld2                         float       default 123.456,
   d_fld3                     	  varchar(10) default 'xyz',
   d_fld4                     	  datetime    default '2003-12-11',
   d_fld5                     	  datetime    default '23:12:11',
   d_fld6                     	  datetime    default '2003-12-11 23:12:11',
   primary key (d_id)
)
go


/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                int                  not null,
eq_name              varchar(30)          null,
eq_type              smallint             null,
eq_cost              numeric(9,4)         null,
eq_date              datetime             null,
woff_date            datetime             null,
primary key  (eq_id)
)
go


/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               smallint             not null,
eq_id                int                  not null,
primary key  (dep_id, eq_id)
)
go


create   index equipment_FK on equipment2 (dep_id)
go


create index equipment2_FK on equipment2 (eq_id)
go


alter table equipment2
   add foreign key (dep_id) references department (dep_id)
go


alter table equipment2
   add foreign key (eq_id) references equipment (eq_id)
go




/*==============================================================*/
/* Table : number_values                                        */
/*==============================================================*/
create table number_values (
n_id                 int                  not null,
n_tint               tinyint              null,
n_sint               smallint             null,
n_int                int                  null,
n_bdecimal           int                  null,
n_numeric            numeric(9,4)         null,
n_float              float                null,
n_real               real                 null,
n_dprecission        double precision     null,
n_money              money                null,
primary key  (n_id)
)
go


/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 smallint             not null,
p_dep_id             smallint             null,
p_name               varchar(40)          null,
p_begin_work         datetime             null,
p_end_work           datetime             null,
p_picture            image                null,
p_resume             text                 null,
p_redundant          tinyint              null,
primary key  (p_id)
)
go

create   index people_FK on people (p_dep_id)
go

alter table people
   add foreign key (p_dep_id) references department (dep_id)
go



/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values (
s_id                 int                  not null,
s_char               char(255)            null,
s_varchar            varchar(255)         null,
s_nchar              char(255)            null,
s_nvarchar           varchar(255)         null,
s_bit                binary(255)          null,
s_varbit             varbinary(1024)      null,
primary key  (s_id)
)
go

/*==============================================================*/
/* Table : Case_Sensitive                                       */
/*==============================================================*/
create table [Case_Sensitive] (
cs_id                 INTEGER           not null,
[Cs_Data1]            INTEGER		null,
[cs_data1]            INTEGER		null,
[cs data1]            INTEGER		null,
primary key (cs_id)
)
go

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
create table case_sensitive (
cs_id                 INTEGER                        not null,
"CS_DATA1"            INTEGER		null,
"CS_Data2"            INTEGER		null,
"Cs_Data3"            INTEGER		null,
primary key (cs_id)
)
go

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load (
hl_id		      INTEGER NOT NULL,
data1		      FLOAT,
data2		      CHAR(10),
primary key (hl_id)
)
go

/*==============================================================*/
/* Stored procedure: procedure1                                 */
/*==============================================================*/
create procedure procedure1
	@p1 integer,
	@r1 integer output
as
  set @r1 = @p1 + 1
  select @r1
go

/*==============================================================*/
/* Stored procedure: procedure2                                 */
/*==============================================================*/
create procedure procedure2
as
  select eq_name FROM equipment ORDER BY eq_name
go

