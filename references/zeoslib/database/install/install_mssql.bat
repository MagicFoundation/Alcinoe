set Host=localhost
set Database=zeoslib
set User=sa
set Password=

osql -D%Database% -U%User% -P%Password% -i ..\create_mssql.sql
osql -D%Database% -U%User% -P%Password% -i ..\populate_any_mssybase.sql
osql -D%Database% -U%User% -P%Password% -i ..\populate_mssql.sql
