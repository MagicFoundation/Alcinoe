set Database=zeoslib

del /P /F %Database%.db 
del /P /F %Database%.log

dbinit -p 4096 "%Database%" 
dbspawn dbeng8 "%Database%" 

dbisql -c "uid=dba;pwd=sql;dbf=%Database%" -nogui -d1 ..\create_asa.sql
dbisql -c "uid=dba;pwd=sql;dbf=%Database%" -nogui -d1 ..\create_asa_bugreport.sql
dbisql -c "uid=dba;pwd=sql;dbf=%Database%" -nogui -d1 ..\populate_any.sql
dbisql -c "uid=dba;pwd=sql;dbf=%Database%" -nogui -d1 ..\populate_asa.sql

dbstop -c "uid=dba;pwd=sql;dbf=%Database%" -x
