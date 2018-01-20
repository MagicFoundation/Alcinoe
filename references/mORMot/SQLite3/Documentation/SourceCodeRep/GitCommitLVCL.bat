@echo off
@echo FossilRepository=%1
@echo GitRepository=%2
@echo GitExe=%3
@echo DescFile=%4

@echo.
@echo.
@echo LVCL repository
@echo ---------------

@cd ..\LVCL

%3 pull

ROBOCOPY %1\LVCL . /s >nul

%3 add .
%3 commit -a --file=%4
%3 push

@echo.
@pause
