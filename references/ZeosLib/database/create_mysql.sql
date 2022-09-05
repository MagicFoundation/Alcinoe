/*==============================================================*/
/* Database name:  MySql                                        */
/* DBMS name:      MySQL 3.23                                   */
/* Created on:     04.02.2003 19:48:39                          */
/*==============================================================*/

/*==============================================================*/
/* Table : blob_values                                          */
/*==============================================================*/
create table blob_values
(
   b_id                           int			 not null,
   b_text                         text,
   b_image                        longblob   
);

/*INSERT INTO blob_values VALUES (1, LOAD_FILE('text/lgpl.txt'), LOAD_FILE('images/dogs.jpg'));*/

/*==============================================================*/
/* Table : date_values                                          */
/*==============================================================*/
create table date_values
(
   d_id                           int                    not null,
   d_date                         date,
   d_time                         time,
   d_datetime                     datetime,
   d_timestamp                    timestamp,
   primary key (d_id)
);

/*==============================================================*/
/* Table : default_values                                          */
/*==============================================================*/
create table default_values
(
   d_id                           int not null auto_increment,
   d_fld1                         int default 123456,
   d_fld2                         float default 123.456,
   d_fld3                     	  varchar(10) default 'xyz',
   d_fld4                     	  date default '2003-12-11',
   d_fld5                     	  time default '23:12:11',
   d_fld6                     	  datetime default '2003-12-11 23:12:11',
   primary key (d_id)
);

/*==============================================================*/
/* Table : department                                           */
/*==============================================================*/
create table department
(
   dep_id                         smallint not null auto_increment,
   dep_name                       varchar(20),
   dep_shname                     char(5),
   dep_address                    varchar(255),
   primary key (dep_id)
);

/*==============================================================*/
/* Table : equipment                                            */
/*==============================================================*/
create table equipment
(
   eq_id                          int not null auto_increment,
   eq_name                        varchar(30),
   eq_type                        smallint,
   eq_cost                        numeric(9,4),
   eq_date                        date,
   woff_date                      date,
   primary key (eq_id)
);

/*==============================================================*/
/* Table : equipment2                                           */
/*==============================================================*/
create table equipment2
(
   dep_id                         smallint not null,
   eq_id                          int not null,
   primary key (dep_id, eq_id)   
);

/*==============================================================*/
/* Index: equipment_FK                                          */
/*==============================================================*/
create index equipment_FK on equipment2 (dep_id);

/*==============================================================*/
/* Index: equipment2_FK                                         */
/*==============================================================*/
create index equipment2_FK on equipment2 (eq_id);

/*==============================================================*/
/* Table : extension                                            */
/*==============================================================*/
create table extension
(
   ext_id                         CHAR(10),
   ext_set1                       SET('Y', 'N'),
   ext_set2                       SET('White', 'Black', 'Yellow'),
   ext_enum                       ENUM('Car', 'House', 'Work', 'Dog', 'Wife', 'Child')
);

/*==============================================================*/
/* Table : number_values                                        */
/*==============================================================*/
create table number_values
(
   n_id                           int   		 not null,
   n_tint                         tinyint,
   n_sint                         smallint,
   n_int                          int,
   n_bdecimal                     bigint,
   n_numeric                      numeric(9,4),
   n_float                        float,
   n_real                         real,
   n_dprecission                  double,
   n_money                        float(8,2),
   primary key (n_id)
);

/*==============================================================*/
/* Table : people                                               */
/*==============================================================*/
create table people
(
   p_id                           smallint  not null auto_increment,
   p_dep_id                       smallint,
   p_name                         varchar(40),
   p_begin_work                   time,
   p_end_work                     time,
   p_picture                      longblob,
   p_resume                       text,
   p_redundant                    tinyint(1),
   primary key (p_id)   
);

/*==============================================================*/
/* Index: people_FK                                             */
/*==============================================================*/
create index people_FK on people (p_dep_id);

/*==============================================================*/
/* Table : string_values                                        */
/*==============================================================*/
create table string_values
(
   s_id                           int                            not null,
   s_char                         char(255),
   s_varchar                      varchar(255),
   s_nchar                        char(255),
   s_nvarchar                     varchar(255),
   s_bit                          blob,
   s_varbit                       longblob,
   primary key (s_id)
);

/*==============================================================*/
/* Table : cargo                                                */
/*==============================================================*/
create table cargo
(
   c_id                           bigint not null auto_increment,
   c_dep_id                       smallint,
   c_name                         CHAR(10),
   c_seal                         tinyint(1),
   c_date_came                    datetime,
   c_date_out                     datetime,
   c_weight                       float,
   c_width                        int,
   c_height                       int,
   c_cost                         float(12,4),
   c_attributes                   blob,
   primary key (c_id)   
);

/*==============================================================*/
/* Table : high_load                                            */
/*==============================================================*/
create table high_load
(
  hl_id		      INTEGER NOT NULL,
  data1		      FLOAT,
  data2		      CHAR(10),
  primary key (hl_id)
);

/*==============================================================*/
/* Index: cargo_FK                                              */
/*==============================================================*/
create index cargo_FK on cargo (c_dep_id);

/*==============================================================*/
/* Grant privileges to columns                                  */
/*==============================================================*/
/*grant update(p_resume, p_redundant) on zeoslib.people to root@localhost;*/

/*==============================================================*/
/* Grant privileges to table                                    */
/*==============================================================*/
/*grant select on zeoslib.people to root@localhost;*/
