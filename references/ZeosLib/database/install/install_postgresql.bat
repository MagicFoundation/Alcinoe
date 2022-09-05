set Host=127.0.0.1
set Database=zeoslib
set user=zeos
set Password=zeos
set pg_home=C:\Programme\PostgreSQL\8.1\bin

%pg_home%\dropdb -h %Host% -U %User% %Database%
%pg_home%\createdb -E LATIN1 -h %Host% -U %User% %Database%

%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\create_postgresql.sql
%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\populate_any.sql
%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\populate_postgresql_81.sql
%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\create_postgresql_bugreport.sql
%pg_home%\psql -h %Host% -U %User% -d %Database% -f ..\create_postgresql_bugreport_81.sql
