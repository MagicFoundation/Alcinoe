@echo off
SETLOCAL

set AAB_FILENAME=%~dp0\..\..\Android64\Release\ALFmxDynamicListBoxDemo\bin\ALFmxDynamicListBoxDemo.aab
set KEYSTORE_FILENAME=%~dp0\Keystore\release.keystore
set KEYSTORE_ALIAS=alfmxdynamiclistbox
set KEYSTORE_PWD=123456

call %~dp0\..\..\..\..\Tools\AndroidDebugBridge\InstallAABToConnectedDevice.bat