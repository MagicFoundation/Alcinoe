@echo off
SETLOCAL

REM ----------------
REM Init Environment
REM ----------------

if "%ALBaseDir%"=="" (
  set InitEnvironmentQuietMode=1
  call "%~dp0\..\..\InitEnvironment.bat"
  IF ERRORLEVEL 1 goto ERROR
)

REM ------------
REM Init ADBPath
REM ------------

set "ADBPath="
for /f "tokens=2*" %%A in ('reg query "HKEY_CURRENT_USER\SOFTWARE\Embarcadero\BDS\%ALDelphiVersion%\Environment Variables" /v "BDSPLATFORMSDKSDIR"') do set "ADBPath=%%B"
set "ADBPath=%ADBPath%\android\platform-tools\adb.exe"
IF NOT EXIST "%ADBPath%" (
  echo Error: Unable to locate adb.exe. Check the Delphi BDSPLATFORMSDKSDIR registry setting.
  goto ERROR
)

REM === ADB Screen Recording Script ===
REM Press any key to stop recording and save the MP4 to the Desktop.
REM Android screenrecord normally stops automatically after 180 seconds.

for /f %%I in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMdd_HHmmss"') do set "TIMESTAMP=%%I"
set "REMOTE_PATH=/sdcard/recording_%TIMESTAMP%.mp4"
set "LOCAL_PATH=%USERPROFILE%\Desktop\recording_%TIMESTAMP%.mp4"
set "FRAMES_DIR=%USERPROFILE%\Desktop\recording_%TIMESTAMP%"
set "FFMPEG_PATH=C:\Program Files\ffmpeg\bin\ffmpeg.exe"

echo [*] Checking ADB connection...
"%ADBPath%" devices | findstr /R /C:"device$" >nul
if errorlevel 1 (
  echo [!] No Android device found. Check USB debugging and connection.
  goto ERROR
)

echo [*] Starting screen recording...
start "Android Screen Recording" /B "%ADBPath%" shell screenrecord --bit-rate 8000000 "%REMOTE_PATH%"
if errorlevel 1 goto ERROR

echo [*] Recording... Press any key to stop and save.
pause >nul

echo [*] Stopping recording...
"%ADBPath%" shell pkill -2 screenrecord
timeout /t 2 /nobreak >nul

echo [*] Pulling video to Desktop...
"%ADBPath%" pull "%REMOTE_PATH%" "%LOCAL_PATH%"
if errorlevel 1 goto ERROR

echo [*] Cleaning up remote file...
"%ADBPath%" shell rm "%REMOTE_PATH%"

REM ------------------------------------
REM Extract all frames if FFmpeg exists
REM ------------------------------------

if exist "%FFMPEG_PATH%" (
  echo [*] Extracting PNG frames...
  if not exist "%FRAMES_DIR%" mkdir "%FRAMES_DIR%"
  "%FFMPEG_PATH%" -i "%LOCAL_PATH%" -fps_mode passthrough "%FRAMES_DIR%\frame_%%06d.png"
  if errorlevel 1 (
    echo [!] FFmpeg frame extraction failed.
  ) else (
    echo [+] Frames saved to: "%FRAMES_DIR%"
  )
) else (
  echo [*] FFmpeg not found. Skipping frame extraction.
)

echo [+] Done! Recording saved to:
echo     %LOCAL_PATH%
start "" "%LOCAL_PATH%"
pause
endlocal
exit /b 0

:ERROR
echo [!] Screen recording failed.
pause
endlocal
exit /b 1
