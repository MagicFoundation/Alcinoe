@echo off

rmdir .\res /s /q
mkdir .\res 2> nul

"C:\Dev\Alcinoe\tools\xmlmerge\XmlMerge.exe"^
 .\res^
 .\libraries\com.ALFirebaseMessagingDemo.app\res^
 .\libraries\com.google.android.gms\res^
 .\libraries\com.google.android.gms\com.google.firebase.crash

@echo Finished

pause