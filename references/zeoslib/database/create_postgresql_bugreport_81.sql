/*==============================================================*/
/* Tables for Bug#815852                                       */
/*==============================================================*/

CREATE DOMAIN tinteger int4 not null;
CREATE DOMAIN tstring varchar(35);

CREATE TABLE test815852 (
  fld1 tinteger,
  fld2 tstring
);

/*==============================================================*/
/* Tables for Bug#824780                                       */
/*==============================================================*/

CREATE SCHEMA xyz;

CREATE TABLE xyz.test824780 (
  fld1 int4 not null,
  fld2 varchar(35)
);

CREATE TABLE test824780 (
  fld1 int4 not null,
  fld2 varchar(35)
);

/*==============================================================*/
/* Tables for Bug#1014416                                       */
/*==============================================================*/

CREATE TABLE test1014416 (
  fld1 cidr,
  fld2 inet,
  fld3 macaddr
);

INSERT INTO test1014416 VALUES ('192.168.100.128/25',
  '192.168.100.128/25', '08:00:2b:01:02:03');
INSERT INTO test1014416 VALUES ('2001:4f8:3:ba:2e0:81ff:fe22:d1f1/128',
  '2001:4f8:3:ba:2e0:81ff:fe22:d1f1/128', '08-00-2b-01-02-03');