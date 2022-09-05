set Host=localhost
set Database=zeoslib
set User=sa
set Password=

isql -D%Database% -U%User% -P%Password% -i ..\create_sybase.sql
isql -D%Database% -U%User% -P%Password% -i ..\populate_any_mssybase.sql
isql -D%Database% -U%User% -P%Password% -i ..\populate_sybase.sql
