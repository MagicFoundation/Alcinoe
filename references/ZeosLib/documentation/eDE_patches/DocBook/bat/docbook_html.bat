@echo off
echo\

rem ----------------------------------------------------------------------------
rem >e-novative> DocBook Environment (eDE)
rem (c) 2002-2003 e-novative GmbH, Munich, Germany
rem http://www.e-novative.de
rem
rem Single HTML File Generation
rem
rem This file is part of eDE
rem
rem eDE is free software; you can redistribute it and/or modify
rem it under the terms of the GNU General Public License as published by
rem the Free Software Foundation; either version 2 of the License, or
rem (at your option) any later version.
rem
rem eDE is distributed in the hope that it will be useful,
rem but WITHOUT ANY WARRANTY; without even the implied warranty of
rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem GNU General Public License for more details.
rem
rem You should have received a copy of the GNU General Public License
rem along with eDe; if not, write to the Free Software
rem Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
rem ----------------------------------------------------------------------------

rem %1: document name
rem %2: output directory (optional)

rem load configuration
call docbook_configuration.bat

rem set output directory
if %2()==() (
  if not exist %docbook_document_output%\%1 md %docbook_document_output%\%1
  set docbook_outputdir=%docbook_document_output%\%1\html\
) else (
  set docbook_outputdir=%2
)

rem create document subdirectory if not present
if not exist %docbook_outputdir% md %docbook_outputdir%

rem change to document directory
call docbook_process %1
if not %errorlevel%==0 goto error

echo eDE: creating %1.html in %docbook_outputdir%:

call docbook_validate %1
if not %errorlevel%==0 goto error

rem copy appropriate css stylesheet
call docbook_select_css %1 %docbook_outputdir%

rem copy the original docbook images and user figures to output dir
xcopy %docbook_path%\xsl\images\*.png %docbook_outputdir%\images\ /s /e /v /d
xcopy .\figure\*.*   %docbook_outputdir%\figure\ /s /e /v /d

rem copy repository images to output dir
xcopy %docbook_document_repository%\%1\images\*.* %docbook_outputdir%\images\ /s /e /v /d

rem determine stylesheet to use
if not exist %docbook_path%\stylesheet\document_%1_html.xsl goto is_default

echo eDE: using document XSL stylesheet
set docbook_use_stylesheet=%docbook_path%\stylesheet\document_%1_html.xsl

goto translate

:is_default

rem detect document type
findstr /c:"<!DOCTYPE article" %1.xml >nul
if %errorlevel%==0 goto is_article

rem detect document type
findstr /c:"<!DOCTYPE book" %1.xml >nul
if %errorlevel%==0 goto is_book

goto error_doctype

:is_book

echo eDE: using default book XSL stylesheet
set docbook_use_stylesheet=%docbook_path%\stylesheet\e-novative_book_html.xsl
goto translate

:is_article

echo eDE: using default article XSL stylesheet
set docbook_use_stylesheet=%docbook_path%\stylesheet\e-novative_article_html.xsl

:translate

java.exe %docbook_java_parameters% -cp %docbook_path%\saxon\saxon.jar;%docbook_path%\xsl\extensions\saxon651.jar com.icl.saxon.StyleSheet -o %docbook_outputdir%\%1.html %1.xml %docbook_use_stylesheet%
if not %ERRORLEVEL%==0 goto error_saxon

goto ok

:error_saxon
echo eDE: XSL transformation failed
goto error

:error_doctype
echo eDE: cannot detect document type of %1.xml
goto error

:error
exit /B -1

:ok

echo e-novative DocBook: %1.html created

:end
