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
set ndk=C:\Users\Alfred\AppData\Local\Android\android-ndk-r20b
set gccverextension=.x

set path=%ndk%;%path%

@rem echo path

call %ndk%\ndk-build.cmd clean all

set bindir=.\obj\local

set gccver=4.9

set target=x86
set cpu=x86
set targetname=i686
set targetextension=

set targetdir=%target%-%gccver%
set bintargetdir=%bindir%\%cpu%
@rem mkdir %bintargetdir%
copy %ndk%\toolchains\%targetdir%\prebuilt\windows\lib\gcc\%targetname%-linux-android%targetextension%\%gccver%%gccverextension%\libgcc.a %bintargetdir%\libgcc.a

set target=x86_64
set cpu=x86_64
set targetname=x86_64
set targetextension=

set targetdir=%target%-%gccver%
set bintargetdir=%bindir%\%cpu%
@rem mkdir %bintargetdir%
copy %ndk%\toolchains\%targetdir%\prebuilt\windows\lib\gcc\%targetname%-linux-android%targetextension%\%gccver%%gccverextension%\libgcc.a %bintargetdir%\libgcc.a


set target=arm
set cpu=armeabi-v7a
set targetname=arm
set targetextension=eabi

set targetdir=%target%-linux-android%targetextension%-%gccver%
set bintargetdir=%bindir%\%cpu%
@rem mkdir %bintargetdir%
copy %ndk%\toolchains\%targetdir%\prebuilt\windows\lib\gcc\%targetname%-linux-android%targetextension%\%gccver%%gccverextension%\libgcc.a %bintargetdir%\libgcc.a


set target=aarch64
set cpu=arm64-v8a
set targetname=aarch64
set targetextension=

set targetdir=%target%-linux-android%targetextension%-%gccver%
set bintargetdir=%bindir%\%cpu%
@rem mkdir %bintargetdir%
copy %ndk%\toolchains\%targetdir%\prebuilt\windows\lib\gcc\%targetname%-linux-android%targetextension%\%gccver%%gccverextension%\libgcc.a %bintargetdir%\libgcc.a

if "%OS%"=="Windows_NT" endlocal

pause