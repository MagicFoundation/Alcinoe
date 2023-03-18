UnitNormalizer
==============

A source code formatters to clean up the layout of existing 
code and promote Alcinoe standards. Despite the fact that 
there are some Delphi source code formatting guidelines, 
it is very difficult to find perfect agreement among different 
Delphi programmers on how exactly the code should be written. 
Formatting is generally subject of intense debate. The Alcinoe 
standard requires function declarations to be written 
like this:

```
{*****************}
procedure xxxxxxxx(
            param1: boolean;
            param2: boolean;
            param3: boolean);

  {~~~~~~~~~~~~~~~}
  function yyyyyyy(
            param1: boolean;
            param2: boolean;
            param3: boolean): boolean;
  begin
    ...
  end;

begin
end;
```
        
Usage
-----

```
  UnitNormalizer.exe
    -Dir=The directory where all source files are located.
    -FilesToIgnore=The list of filenames to ignore. Separate filename with '';''.
    -CreateBackup=true or false.
    -NoInteraction=Non-interactive mode.
```

Example
-------

```
  UnitNormalizer.exe^');
    -Dir="c:\MyProject\"^');
    -CreateBackup=false');
```
 