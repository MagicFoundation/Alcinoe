@echo off
@echo FossilRepository=%1
@echo GitRepository=%2
@echo GitExe=%3
@echo DescFile=%4

@echo.
@echo.
@echo SynProject repository
@echo ---------------------

@cd ..\SynProject

%3 pull

ROBOCOPY %1\SynProject . /s >nul

%3 add .
%3 commit -a --file=%4
%3 push

@echo.
@pause
