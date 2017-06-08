@echo off

rmdir .\res /s /q
mkdir .\res 2> nul

"..\..\..\..\tools\xmlmerge\XmlMerge.exe"^
 .\res^
 .\libraries\com.embarcadero.alfmxcontrols\res^
 .\libraries\com.alcinoe\res

@echo Finished

pause