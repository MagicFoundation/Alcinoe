@echo off

rmdir .\res /s /q
mkdir .\res 2> nul

"..\..\..\..\tools\xmlmerge\XmlMerge.exe"^
 .\res^
 .\libraries\com.ALFirebaseMessagingDemo.app\res^
 .\libraries\com.google.firebase.messaging\res^
 .\libraries\com.google.android.gms.base\res^
 .\libraries\com.google.android.gms\res^
 .\libraries\android.support.customtabs\res^
 .\libraries\android.support.mediacompat\res^
 .\libraries\android.support.coreui\res^
 .\libraries\android.support.compat\res^
 .\libraries\android.support.v7.cardview\res^
 .\libraries\android.support.v7.appcompat\res

@echo Finished

pause