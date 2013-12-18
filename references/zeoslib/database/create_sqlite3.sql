/*==============================================================*/
/* Database name:  SQLite                                       */
/* DBMS name:      SQLite 3                                     */
/* Created on:     22.11.2005                                   */
/*==============================================================*/


/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values (
b_id                 int                 not null,
b_text               text                null,
b_blob               blob                null,
primary key  (b_id)
);


/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department (
dep_id               smallint             not null,
dep_name             varchar(20)          null,
dep_shname           char(5)              null,
dep_address          varchar(255)         null,
primary key  (dep_id)
);

/*==============================================================*/
/* View : department                                            */
/*==============================================================*/
create view dep_view as select * from department;


/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo (
c_id                 integer              not null,
c_dep_id             smallint             null,
c_name               char(10)             null,
c_seal               tinyint              null,
c_date_came          datetime             null,
c_date_out           datetime             null,
c_weight             float                null,
c_width              int                  null,
c_height             int                  null,
c_cost               money                null,
c_attributes         blob                 null,
primary key  (c_id),
foreign key (c_dep_id) references department (dep_id)
);


create   index cargo_FK on cargo (c_dep_id);


/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values (
d_id                 int                  not null,
d_date               date                 null,
d_time               time                 null,
d_datetime           datetime             null,
d_timestamp          timestamp            null,
primary key  (d_id)
);

/*==============================================================*/
/* Table : default_values                                       */
/*==============================================================*/
create table default_values(
   d_id                           integer     not null,
   d_fld1                         int         default 123456,
   d_fld2                         float       default 123.456,
   d_fld3                     	  varchar(10) default 'xyz',
   d_fld4                     	  date        default '2003-12-11',
   d_fld5                     	  time        default '23:12:11',
   d_fld6                     	  datetime    default '2003-12-11 23:12:11',
   primary key (d_id)
);


/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment (
eq_id                integer              not null,
eq_name              varchar(30)          null,
eq_type              smallint             null,
eq_cost              numeric(9,4)         null,
eq_date              datetime             null,
woff_date            datetime             null,
primary key  (eq_id)
);


/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2 (
dep_id               smallint             not null,
eq_id                int                  not null,
primary key  (dep_id, eq_id),
foreign key (dep_id) references department (dep_id),
foreign key (eq_id) references equipment (eq_id)
);


create   index equipment_FK on equipment2 (dep_id);

create index equipment2_FK on equipment2 (eq_id);



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
);


/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people (
p_id                 integer              not null,
p_dep_id             smallint             null,
p_name               varchar(40)          null,
p_begin_work         time                 null,
p_end_work           time                 null,
p_picture            blob                 null,
p_resume             text                 null,
p_redundant          tinyint              null,
primary key  (p_id),
foreign key (p_dep_id) references department (dep_id)
);

create   index people_FK on people (p_dep_id);



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
);

/*==============================================================*/
/* Table : case_sensitive                                       */
/*==============================================================*/
create table [case_sensitive] (
cs_id                 INTEGER                        not null,
"CS_DATA1"            INTEGER		null,
"CS_Data2"            INTEGER		null,
"Cs_Data3"            INTEGER		null,
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


