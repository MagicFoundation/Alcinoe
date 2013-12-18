set Host=localhost
set Database=zeoslib
set User=zeos
set Password=zeos
set MySQLBin="C:\Programme\MySQL\MySQL Server 5.0\bin"

echo drop database %Database% | %MySQLBin%\mysql.exe -h %Host% -u %User% --password=%Password%
echo create database %Database% | %MySQLBin%\mysql.exe -h %Host% -u %User% --password=%Password%

%MySQLBin%\mysql.exe -h %Host% -u %User% --password=%Password% %Database% < ..\create_mysql.sql
%MySQLBin%\mysql.exe -h %Host% -u %User% --password=%Password% %Database% < ..\populate_any.sql
%MySQLBin%\mysql.exe -h %Host% -u %User% --password=%Password% %Database% < ..\populate_mysql.sql
%MySQLBin%\mysql.exe -h %Host% -u %User% --password=%Password% %Database% < ..\create_mysql_bugreport.sql
