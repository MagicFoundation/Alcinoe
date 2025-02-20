@echo off
SETLOCAL

set AAB_FILENAME=%~dp0\..\..\Android64\Release\ALFmxControls\bin\ALFmxControls.aab
set KEYSTORE_FILENAME=%~dp0\Keystore\release.keystore
set KEYSTORE_ALIAS=alfmxcontrolsdemo
set KEYSTORE_PWD=123456

call %~dp0\..\..\..\..\Tools\AndroidDebugBridge\InstallAABToConnectedDevice.bat