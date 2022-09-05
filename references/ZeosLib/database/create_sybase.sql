/*==============================================================*/
/* Database name:  Sybase                                       */
/* DBMS name:      Sybase Adaptive Server Enterprise 12.5       */
/* Created on:     17.02.2003 20:22:43                          */
/*==============================================================*/

set quoted_identifier on
go

if exists (select 1
            from  sysindexes
           where  id    = object_id('cargo')
            and   name  = 'cargo_FK'
            and   indid > 0
            and   indid < 255)
   drop index cargo.cargo_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('equipment2')
            and   name  = 'equipment2_FK'
            and   indid > 0
            and   indid < 255)
   drop index equipment2.equipment2_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('equipment2')
            and   name  = 'equipment_FK'
            and   indid > 0
            and   indid < 255)
   drop index equipment2.equipment_FK
go


if exists (select 1
            from  sysindexes
           where  id    = object_id('people')
            and   name  = 'people_FK'
            and   indid > 0
            and   indid < 255)
   drop index people.people_FK
go


if exists (select 1
            from  sysobjects
           where  id = object_id('blob_values')
            and   type = 'U')
   drop table blob_values
go


if exists (select 1
            from  sysobjects
           where  id = object_id('cargo')
            and   type = 'U')
   drop table cargo


if exists (select 1
            from  sysobjects
           where  id = object_id('date_values')
            and   type = 'U')
   drop table date_values
go


if exists (select 1
            from  sysobjects
           where  id = object_id('department')
            and   type = 'U')
   drop table department
go


if exists (select 1
            from  sysobjects
           where  id = object_id('equipment')
            and   type = 'U')
   drop table equipment
go


if exists (select 1
            from  sysobjects
           where  id = object_id('equipment2')
            and   type = 'U')
   drop table equipment2
go



if exists (select 1
            from  sysobjects
           where  id = object_id('number_values')
            and   type = 'U')
   drop table number_values
go


if exists (select 1
            from  sysobjects
           where  id = object_id('people')
            and   type = 'U')
   drop table people
go


if exists (select 1
            from  sysobjects
           where  id = object_id('string_values')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('"Case_Sensitive"')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('case_sensitive')
            and   type = 'U')
   drop table string_values
go

if exists (select 1
            from  sysobjects
           where  id = object_id('high_load')
            and   type = 'U')
   drop table string_values
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


/*==============================================================*/
/* Index: cargo_FK                                              */
/*==============================================================*/
create   index cargo_FK on cargo (
c_dep_id
)
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


/*==============================================================*/
/* Index: equipment_FK                                          */
/*==============================================================*/
create   index equipment_FK on equipment2 (
dep_id
)
go


/*==============================================================*/
/* Index: equipment2_FK                                         */
/*==============================================================*/
create   index equipment2_FK on equipment2 (
eq_id
)
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
p_redundant          tinyint           	  null,
primary key  (p_id)
)
go


/*==============================================================*/
/* Index: people_FK                                             */
/*==============================================================*/
create   index people_FK on people (
p_dep_id
)
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
create table "Case_Sensitive" (
cs_id                 INTEGER                        not null,
"Cs_Data1"            INTEGER		null,
"cs_data1"            INTEGER		null,
"cs data1"            INTEGER		null,
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

alter table cargo
   add foreign key (c_dep_id) references department (dep_id)
go


alter table equipment2
   add foreign key (dep_id) references department (dep_id)
go


alter table equipment2
   add foreign key (eq_id) references equipment (eq_id)
go


alter table people
   add foreign key (p_dep_id) references department (dep_id)
go


