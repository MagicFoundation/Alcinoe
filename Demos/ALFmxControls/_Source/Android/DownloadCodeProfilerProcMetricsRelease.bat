@echo off
SETLOCAL

set androidFile=/storage/emulated/0/Android/data/io.magicfoundation.alcinoe.alfmxcontrolsdemo/files/Download/ALCodeProfilerProcMetrics.dat
set destFolder=..\..\..\..\Tools\CodeProfiler\Data\ALCodeProfilerProcMetrics.dat
set deleteFile=Y

call %~dp0\..\..\..\..\Tools\AndroidDebugBridge\Download.bat