/*==============================================================*/
/* Procedure for Bug#907497                                     */
/*==============================================================*/
CREATE PROCEDURE proc907497
	@zzz varchar(60) out
AS
        set @zzz='7890'
	if @zzz='12345'
	begin
	 set @zzz='99999'
	end
GO

/*==============================================================*/
/* Tables and procedures for Bug#959307                         */
/*==============================================================*/

CREATE TABLE table959307 (
	id int identity not null,
	fld1 Varchar(10)
)
go


CREATE PROCEDURE proc959307 (@p varchar(10)) as
	delete from table959307
	insert into table959307 (fld1) values (@p)
GO


