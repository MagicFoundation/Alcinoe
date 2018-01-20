@echo off
@echo FossilRepository=%1
@echo GitRepository=%2
@echo GitExe=%3
@echo DescFile=%4

@echo.
@echo.
@echo SynPDF repository
@echo -----------------

@cd ..\SynPDF

%3 pull

@xcopy %1\SynCommons.pas . /Y >nul
@xcopy %1\SynLZ.pas . /Y >nul
@xcopy %1\Synopse.inc . /Y >nul
@xcopy %1\SynopseCommit.inc . /Y >nul
@xcopy %1\SynPdf.pas . /Y >nul
@xcopy %1\SynCrypto.pas . /Y >nul
@xcopy %1\SynFPCTypInfo.pas . /Y >nul
@xcopy %1\SynGdiPlus.pas . /Y >nul
@xcopy %1\SynZip.pas . /Y >nul
@xcopy %1\deflate.obj . /Y >nul
@xcopy %1\trees.obj . /Y >nul
@xcopy %1\SQLite3\mORMotReport.pas . /Y >nul

%3 add .
%3 commit -a --file=%4
%3 push

