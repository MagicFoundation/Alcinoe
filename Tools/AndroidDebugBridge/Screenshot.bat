@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

if "%ALBaseDir%"=="" (
  
  Set InitEnvironmentQuietMode=1
  call "%~dp0\..\..\InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR
  
)

REM ------------
REM Init ADBPath
REM ------------

set ADBPath=
for /f "tokens=2*" %%A in ('reg query "HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\%ALDelphiVersion%\Environment Variables" /v "BDSPLATFORMSDKSDIR"') do set ADBPath=%%B
set ADBPath=%ADBPath%\android\platform-tools\adb.exe
IF NOT EXIST "%ADBPath%" (
  echo Error: Unable to locate adb.exe using the path specified in HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\%ALDelphiVersion%\Environment Variables. Please ensure that the ADB path is correctly set in the registry and that adb.exe is available in the specified location.
  goto ERROR
)


REM === ADB Screenshot Script ===
REM Takes a screenshot from the connected Android device via ADB

REM Get timestamp for unique filename
for /f "tokens=2 delims==" %%I in ('wmic os get localdatetime /value') do set dt=%%I
set TIMESTAMP=%dt:~0,4%%dt:~4,2%%dt:~6,2%_%dt:~8,2%%dt:~10,2%%dt:~12,2%

set REMOTE_PATH=/sdcard/screenshot_%TIMESTAMP%.png
set LOCAL_PATH=%USERPROFILE%\Desktop\screenshot_%TIMESTAMP%.png

echo [*] Checking ADB connection...
%ADBPath% devices | findstr /v "List" | findstr "device" >nul
if errorlevel 1 (
    echo [!] No Android device found. Make sure:
    echo     - USB Debugging is enabled on your device
    echo     - The device is connected via USB
    echo     - ADB drivers are installed
    pause
    exit /b 1
)

echo [*] Taking screenshot...
"%ADBPath%" shell screencap -p "%REMOTE_PATH%"
if errorlevel 1 (
    echo [!] Failed to take screenshot.
    pause
    exit /b 1
)

echo [*] Pulling screenshot to Desktop...
"%ADBPath%" pull "%REMOTE_PATH%" "%LOCAL_PATH%"
if errorlevel 1 (
    echo [!] Failed to pull screenshot from device.
    pause
    exit /b 1
)

echo [*] Cleaning up remote file...
"%ADBPath%" shell rm "%REMOTE_PATH%"

echo [+] Done! Screenshot saved to:
echo     %LOCAL_PATH%

REM Open the screenshot automatically
start "" "%LOCAL_PATH%"

pause
endlocal