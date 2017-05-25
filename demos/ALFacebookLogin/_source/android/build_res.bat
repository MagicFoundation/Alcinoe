@echo off

rmdir .\res /s /q
mkdir .\res 2> nul

"C:\Dev\Alcinoe\tools\xmlmerge\XmlMerge.exe"^
 .\res^
 .\libraries\com.ALFacebookLogin.app\res^
 .\libraries\com.facebook\res^
 .\libraries\android.support.v7.cardview\res^
 .\libraries\android.support.v7.appcompat\res
 
 
@echo Finished

pause