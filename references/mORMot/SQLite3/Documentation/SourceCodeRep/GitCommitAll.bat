@echo off
@echo FossilRepository=%1
@echo GitRepository=%2
@echo GitExe=%3
@echo DescFile=%4
@echo DevPath=%5

@echo.
@echo.
@echo mORMot repository
@echo -----------------

del /q /s %2\*.bak %2\*.bk2 > nul 2> nul
%3 add .
%3 commit -a --file=%4
%3 push


@echo.
@echo.
@echo SynPDF repository
@echo -----------------

@cd ..\SynPDF
%3 add .
%3 commit -a --file=%4
%3 push

@echo.
@echo.
@echo dmustache repository
@echo --------------------

@cd ..\dmustache
%3 add .
%3 commit -a --file=%4
%3 push

@echo.
@echo.
@echo LVCL repository
@echo ---------------

@cd ..\LVCL
%3 add .
%3 commit -a --file=%4
%3 push

@echo.
@echo.
@echo SynProject repository
@echo ---------------------

@cd ..\SynProject
%3 add .
%3 commit -a --file=%4
%3 push

@echo.
@pause
