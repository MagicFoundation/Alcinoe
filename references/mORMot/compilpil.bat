@echo off

rem Caller may have defined the following variables:
rem   set DCC=...\bin\dcc32.exe
rem   set DelphiVersion=Delphi ##
rem   set mORMot=... root framework  source folder
rem   set bin=... compilation output folder
rem   call compilpil.bat


if "%mORMot%"=="" set mORMot=\dev\lib
if "%bin%"==""    set bin=%mORMot%\tempbuild

set defaultFolders=%mORMot%;%mORMot%\sqlite3;%mORMot%\syndbdataset;%mORMot%\crossplatform;%mORMot%\sqlite3\DDD\dom;%mORMot%\sqlite3\DDD\infra -I%mORMot%;%mORMot%\crossplatform
if "%DelphiVersion%"=="" (
	rem ** Default compiler is Delphi 7
	set DCC=c:\progs\delphi7\bin\dcc32.exe
	set DelphiVersion=Delphi 7 %LVCL%
	if exist %mORMot%\RTL7\Classes.pas (
		set Switches=-DENHANCEDRTL;INCLUDE_FTS3;USEZEOS -U%mORMot%\RTL7;\dev\zeos\src\core;\dev\zeos\src\dbc;\dev\zeos\src\parsesql;\dev\zeos\src\plain;%defaultFolders%;\dev\zeos\src
	) else (
		if "%LVCL%"=="" (
			set Switches=-DINCLUDE_FTS3 -U%mORMot%\RTL7;%defaultFolders%
		) else (
			set Switches=-DLVCL;INCLUDE_FTS3 -U%mORMot%\LVCL;%mORMot%\RTL7;%defaultFolders%
		)
	)
) else (
	if "%DelphiVersion%"=="Delphi 5" (
		set Switches=-DINCLUDE_FTS3 -GD -Uc:\progs\delphi5\lib;%defaultFolders%
	) else (
		if "%DelphiVersion%"=="Delphi 6" (
			set Switches=-B -Q -DINCLUDE_FTS3 -GD -Uc:\progs\delphi6\lib;%defaultFolders%
		) else (
			rem e.g. Delphi 2007, Delphi XE8
			set Switches=-B -Q -DINCLUDE_FTS3 -GD -U%defaultFolders% -NSSystem;Xml;Data;Datasnap;Web;Soap;Winapi;Vcl;System.Win
		)
	)
)
set Switches=-B -Q -GD -O%mORMot%\SQLite3 -R%mORMot% -E%bin%\exe -N%bin%\dcu %Switches%
if not exist %DCC% goto NoDCCCompiler

echo.
echo ********** mORMot integration using %DelphiVersion% *********
echo.

echo Switches=%Switches%
echo.

if not exist %bin% (
	mkdir %bin%\exe
	mkdir %bin%\dcu
	cd %mORMot%\sqlite3\exe
	copy sqlite3-64.dll %bin%\exe >nul
	copy comments.json %bin%\exe >nul
	copy discogs.json %bin%\exe >nul
	copy interpolation.json %bin%\exe >nul
	copy inverted.json %bin%\exe >nul
	copy partials.json %bin%\exe >nul
	copy sections.json %bin%\exe >nul
	copy transactions.json %bin%\exe >nul
	copy zendframework.json %bin%\exe >nul
	mkdir %bin%\dcu
) else (
	del /q %bin%\exe\*.exe %bin%\exe\*.drc %bin%\exe\*.map %bin%\exe\*.db3 %bin%\exe\*.ini %bin%\exe\*.data %bin%\exe\*.mdb %bin%\exe\TestSQL3.* %bin%\dcu\*.dcu
)

cd %mORMot%\sqlite3
copy TestSQL3.cfg TestSQL3.cfg.bak /Y> nul
del TestSQL3.cfg

echo %CD%
%DCC% TestSQL3.dpr %Switches%
@if errorlevel 1 pause

copy TestSQL3.cfg.bak TestSQL3.cfg /Y> nul

if "%DelphiVersion%"=="Delphi 5" goto SKIPSYNPROJECT

%DCC% TestSQL3Register.dpr %Switches%
@if errorlevel 1 pause
%DCC% ServiceTestSQL3.dpr %Switches%
@if errorlevel 1 pause

if "%LVCL%"=="LVCL" goto NoDCCCompiler

%DCC% TestOleDB.dpr %Switches%
@if errorlevel 1 pause

set samples=%mORMot%\sqlite3\Samples\

cd "%samples%01 - In Memory ORM"
echo.
echo %CD%
%DCC% Project01.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%02 - Embedded SQLite3 ORM"
echo.
echo %CD%
%DCC% Project02.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%03 - NamedPipe Client-Server"
echo.
echo %CD%
%DCC% Project03Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project03Server.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%04 - HTTP Client-Server"
echo.
echo %CD%
%DCC% Project04Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project04Server.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project04ServerRegister.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project04ServerStatic.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%05 - Report created from code"
echo.
echo %CD%
%DCC% TestSQLite3Pages.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%06 - Remote JSON REST Service"
echo.
echo %CD%
%DCC% Project06Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project06Server.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%07 - SynTest"
echo.
echo %CD%
%DCC% SynTest.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%08 - TaskDialog"
echo.
echo %CD%
%DCC% TaskDialogTest.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%09 - HttpApi web server"
echo.
echo %CD%
%DCC% HttpApiServer.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%10 - Background Http service"
echo.
echo %CD%
%DCC% httpservice.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%11 - Exception logging"
echo.
echo %CD%
%DCC% LibraryTest.dpr %Switches%
@if errorlevel 1 pause
%DCC% LoggingTest.dpr %Switches%
@if errorlevel 1 pause
%DCC% LogView.dpr %Switches%
@if errorlevel 1 pause
%DCC% Map2Mab.dpr %Switches%
@if errorlevel 1 pause
%DCC% MyLibrary.dpr %Switches%
@if errorlevel 1 pause
%DCC% UnSynLz.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%12 - SynDB Explorer"
echo.
echo %CD%
%DCC% SynDBExplorer.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%13 - StandAlone JSON SQL server"
echo.
echo %CD%
%DCC% JSONSQLClient.dpr %Switches%
@if errorlevel 1 pause
%DCC% JSONSQLServer.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%14 - Interface based services"
echo.
echo %CD%
%DCC% Project14Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14Server.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerExternal.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerHttp.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerHttpWeak.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerInMemory.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%15 - External DB performance"
echo.
echo %CD%
%DCC% PerfTest.dpr %Switches% -DNONET;NOSTATIC -I\dev\UniDac\Source;\dev\zeos\src -U\dev\UniDac\Source;D:\Dev\UniDAC\Source\UniProviders\Oracle;D:\Dev\UniDAC\Source\UniProviders\InterBase;D:\Dev\UniDAC\Source\UniProviders\SQLite;D:\Dev\UniDAC\Source\UniProviders\SQLServer;\Dev\Zeos\packages\delphi7\build;\Dev\Zeos\src;\Dev\Zeos\src\core;\Dev\Zeos\src\dbc;\Dev\Zeos\src\parsesql;\Dev\Zeos\src\plain
@if errorlevel 1 pause

cd "%samples%16 - Execute SQL via services"
echo.
echo %CD%
%DCC% Project16Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project16ServerHttp.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%17 - TClientDataset use"
echo.
echo %CD%
%DCC% mORMotVCLTest.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%18 - AJAX ExtJS Grid"
echo.
echo %CD%
%DCC% Project18Server.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%19 - AJAX ExtJS FishFacts"
echo.
echo %CD%
%DCC% Project19Server.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%20 - DTO interface based service"
echo.
echo %CD%
%DCC% Project20Client.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project20ServerInMemory.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%21 - HTTP Client-Server performance"
echo.
echo %CD%
%DCC% Project21HttpClient.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project21HttpServer.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%22 - JavaScript HTTPApi web server"
echo.
echo %CD%
%DCC% JSHttpApiServer.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%23 - JavaScript Tests"
echo.
echo %CD%
%DCC% TestSynSM.dpr %Switches%
@if errorlevel 1 pause
%DCC% TestMustache.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%24 - MongoDB"
echo.
echo %CD%
%DCC% MongoDBTests.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%25 - JSON performance"
echo.
echo %CD%
%DCC% JSONPerfTests.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%26 - RESTful ORM"
echo.
echo %CD%
%DCC% RESTserver.dpr %Switches%
%DCC% RESTClient.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%27 - CrossPlatform Clients"
echo.
echo %CD%
%DCC% RegressionTests.dpr %Switches%
@if errorlevel 1 pause
%DCC% RegressionTestsServer.dpr %Switches%
@if errorlevel 1 pause
%DCC% VCLClient.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project14ServerHttpWrapper.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%28 - Simple RESTful ORM Server"
echo.
echo %CD%
%DCC% RESTserver.dpr %Switches%
@if errorlevel 1 pause
%DCC% RESTclient.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%30 - MVC Server"
echo.
echo %CD%
%DCC% MVCServer.dpr %Switches%
rem %DCC% MVCServerMongoDB.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%31 - WebSockets"
echo.
echo %CD%
%DCC% Project31SimpleEchoServer.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project31LongWorkClient.dpr %Switches%
@if errorlevel 1 pause
%DCC% Project31LongWorkServer.dpr %Switches%
@if errorlevel 1 pause

cd "%samples%33 - ECC"
echo.
echo %CD%
%DCC% ECC.dpr %Switches%
@if errorlevel 1 pause


cd "%samples%MainDemo"
echo.
echo %CD%
call FileMainRes.bat
%DCC% SynFile.dpr %Switches%
@if errorlevel 1 pause

goto SKIPSYNPROJECT

cd "%mORMot%\SynProject"
brcc32 FileMain.rc
%DCC% SynProject.dpr %Switches%
@if errorlevel 1 pause

:SKIPSYNPROJECT

echo.
echo Appending .map information into all generated .exe
if exist %bin%\exe\Map2Map.exe %bin%\exe\Map2Mab.exe %bin%\exe\*.exe
del /q %bin%\exe\*.map %bin%\exe\*.drc

echo.
echo Running automated tests for mORMot
%bin%\exe\TestSQL3 "%DelphiVersion% "
@if errorlevel 1 echo "Some test fails. See "%bin%\exe\%DelphiVersion% Synopse mORMot Framework Automated tests.txt" for details"

:NoDCCCompiler
cd %mORMot%
set DCC=
set DelphiVersion=
set mORMot=
set bin=
rem pause
