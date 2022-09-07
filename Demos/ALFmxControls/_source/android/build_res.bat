@echo off

rmdir .\res /s /q
mkdir .\res 2> nul

"..\..\..\..\Tools\XmlMerge\XmlMerge.exe"^
 .\res^
 .\libraries\com.embarcadero.alfmxcontrols\res

@echo Finished

pause