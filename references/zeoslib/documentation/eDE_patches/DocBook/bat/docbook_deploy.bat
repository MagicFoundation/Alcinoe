@echo off
echo\

rem ----------------------------------------------------------------------------
rem >e-novative> DocBook Environment (eDE)
rem (c) 2002-2003 e-novative GmbH, Munich, Germany
rem http://www.e-novative.de
rem
rem eDE Document Deployment
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
rem %2: deploy directory (optional)

rem load configugration
call docbook_configuration.bat

rem set deploy directory
if %2()==() (
  set docbook_deploydir=%docbook_document_deploy%\%1
) else (
  set docbook_deploydir=%2
)

rem create deploy subdirectory if not present
if not exist %docbook_deploydir% md %docbook_deploydir%

rem change to document directory
call docbook_process %1
if not %errorlevel%==0 goto error

echo eDE: deploying %1 to %docbook_deploydir%:

call docbook_html %1
cd /d %docbook_outputdir%
7za.exe a -tzip -mx %docbook_deploydir%\%1_html.zip %1.html ede.css "figure\*" "images\*" "images\callouts\*"
if not %errorlevel%==0 goto error

call docbook_chunk %1
cd /d %docbook_outputdir%
7za.exe a -tzip -mx %docbook_deploydir%\%1_chunk.zip *.html ede.css "figure\*" "images\*" "images\callouts\*"
if not %errorlevel%==0 goto error

call docbook_pdf %1
cd /d %docbook_outputdir%
7za.exe a -tzip -mx %docbook_deploydir%\%1_pdf.zip %1.pdf
if not %errorlevel%==0 goto error

call docbook_htmlhelp %1
cd /d %docbook_outputdir%
if not %errorlevel%==0 goto error
7za.exe a -tzip -mx %docbook_deploydir%\%1_htmlhelp.zip %1.chm
if not %errorlevel%==0 goto error

call docbook_text %1
cd /d %docbook_outputdir%
if not %errorlevel%==0 goto error
7za.exe a -tzip -mx %docbook_deploydir%\%1_text.zip %1.txt
if not %errorlevel%==0 goto error


goto ok

:error
exit /B -1

:ok

echo eDE: document %1 deployed

:end
