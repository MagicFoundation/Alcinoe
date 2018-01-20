@echo off
@echo FossilRepository=%1
@echo GitRepository=%2
@echo GitExe=%3
@echo DescFile=%4

@echo.
@echo.
@echo dmustache repository
@echo --------------------

@cd ..\dmustache

%3 pull

@xcopy %1\SynCommons.pas . /Y >nul
@xcopy %1\SynLZ.pas . /Y >nul
@xcopy %1\SynMustache.pas . /Y >nul
@xcopy %1\Synopse.inc . /Y >nul
@xcopy %1\SynopseCommit.inc . /Y >nul
@xcopy %1\SynFPCLinux.pas . /Y >nul
@xcopy %1\SynFPCTypInfo.pas . /Y >nul

%3 add .
%3 commit -a --file=%4
%3 push

@echo.
@pause
