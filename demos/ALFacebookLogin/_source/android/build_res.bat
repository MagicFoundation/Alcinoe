@echo off

rmdir .\res /s /q
mkdir .\res 2> nul

"..\..\..\..\tools\xmlmerge\XmlMerge.exe"^
 .\res^
 .\libraries\com.ALFacebookLogin.app\res^
 .\libraries\com.facebook.common\res^
 .\libraries\com.facebook.share\res^
 .\libraries\com.facebook.login\res^
 .\libraries\android.support.customtabs\res^
 .\libraries\android.support.mediacompat\res^
 .\libraries\android.support.coreui\res^
 .\libraries\android.support.compat\res^
 .\libraries\android.support.v7.cardview\res^
 .\libraries\android.support.v7.appcompat\res
 
 
@echo Finished

pause