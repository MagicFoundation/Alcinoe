set Host=localhost
set TempFile=temp.sql
set Database=zeoslib.fdb
set User=SYSDBA
set Password=masterkey
set fb_home=d:\sqlserverfarm\firebird\15\bin
del %Database% 

echo CREATE DATABASE "%Database%" USER "%User%" PASSWORD "%Password%" PAGE_SIZE=4096; exit; > %TempFile%
%fb_home%\isql -u %User% -p %Password% -s 3 < %TempFile%
del %TempFile%

%fb_home%\isql %Database% -u %User% -p %Password% -s 3 < ..\create_interbase.sql
%fb_home%\isql %Database% -u %User% -p %Password% -s 3 < ..\create_interbase_bugreport.sql
%fb_home%\isql %Database% -u %User% -p %Password% -s 3 < ..\populate_any.sql
%fb_home%\isql %Database% -u %User% -p %Password% -s 3 < ..\populate_interbase.sql


