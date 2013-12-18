/*==============================================================*/
/* Database name:  zeoslib                                      */
/* DBMS name:      PostgreSQL                                   */
/* Created on:     28.12.2002 20:27:07                          */
/*==============================================================*/

/*==============================================================*/
/* Table : extension                                            */
/*==============================================================*/

CREATE TYPE TEnumTest AS ENUM ('Car', 'House', 'Work', 'Dog', 'Wife', 'Child'); 

create table extension
(
   ext_id                         int,
   ext_enum                    TEnumTest
);

INSERT into extension values (1,'Car'), (2,'House'), (3,'Work'), (4,'Dog'), (5,'Wife'), (6,'Child');