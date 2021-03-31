@if "%DEBUG%" == "" @echo off
@rem ##########################################################################
@rem
@rem  Android sqlite buildscript for Windows
@rem  Builds the mORMot sqlite static libraries with the Android NDK
@rem  Please set the path towards the NDK
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal


@rem set ndk=C:\Users\Public\Documents\Embarcadero\Studio\17.0\PlatformSDKs\android-ndk-r10e
@rem set gccverextension=

@rem set ndk=C:\Users\Alfred\AppData\Local\Android\android-ndk-r19c
@rem set ndk=C:\Users\Alfred\AppData\Local\Android\android-ndk-r20b
set ndk=C:\Users\Alfred\AppData\Local\Android\Sdk\ndk\21.1.6352462
set gccverextension=.x

set path=%ndk%;%path%

@rem echo path

call %ndk%\ndk-build.cmd clean
call %ndk%\ndk-build.cmd all

set bindir=.\obj\local

set targetdir=.\..\..\..\static\i386-android
copy %bindir%\x86\libsqlite3.a %targetdir%\libsqlite3.a
copy %ndk%\toolchains\x86-4.9\prebuilt\windows-x86_64\lib\gcc\i686-linux-android\4.9.x\libgcc.a %targetdir%\libgcc.a

set targetdir=.\..\..\..\static\x86_64-android
copy %bindir%\x86_64\libsqlite3.a %targetdir%\libsqlite3.a
copy %ndk%\toolchains\x86_64-4.9\prebuilt\windows-x86_64\lib\gcc\x86_64-linux-android\4.9.x\libgcc.a %targetdir%\libgcc.a

set targetdir=.\..\..\..\static\arm-android
copy %bindir%\armeabi-v7a\libsqlite3.a %targetdir%\libsqlite3.a
copy %ndk%\toolchains\arm-linux-androideabi-4.9\prebuilt\windows-x86_64\lib\gcc\arm-linux-androideabi\4.9.x\libgcc.a %targetdir%\libgcc.a
@rem copy %ndk%\toolchains\arm-linux-androideabi-4.9\prebuilt\windows-x86_64\lib\gcc\arm-linux-androideabi\4.9.x\armv7-a\libgcc.a %targetdir%\libgcc.a

set targetdir=.\..\..\..\static\aarch64-android
copy %bindir%\arm64-v8a\libsqlite3.a %targetdir%\libsqlite3.a
copy %ndk%\toolchains\aarch64-linux-android-4.9\prebuilt\windows-x86_64\lib\gcc\aarch64-linux-android\4.9.x\libgcc.a %targetdir%\libgcc.a

if "%OS%"=="Windows_NT" endlocal

pause