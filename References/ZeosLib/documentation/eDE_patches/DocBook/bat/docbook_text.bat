@echo off
echo\

rem ----------------------------------------------------------------------------
rem Single Text File Generation
rem ----------------------------------------------------------------------------

rem %1: document name

rem load configuration
call docbook_configuration.bat

rem generate html if no html file is present
if not exist %docbook_document_output%\%1\html\%1.html call docbook_html.bat %1

rem set output directory
if %2()==() (
  if not exist %docbook_document_output%\%1 md %docbook_document_output%\%1
  set docbook_outputdir=%docbook_document_output%\%1\text\
) else (
  set docbook_outputdir=%2
)

rem create document subdirectory if not present
if not exist %docbook_outputdir% md %docbook_outputdir%

rem create configuration file for text conversion
copy %docbook_path%\bin\textconvert_template.cfg %docbook_path%\bin\textconvert.cfg
echo Source=%docbook_document_output%\%1\html\%1.html >> %docbook_path%\bin\textconvert.cfg
echo Dest=%docbook_document_output%\%1\text\%1.txt >> %docbook_path%\bin\textconvert.cfg

rem start conversion
del %docbook_outputdir%\*.txt
HTMLAsText /run "%docbook_path%\bin\textconvert.cfg"
del "%docbook_path%\bin\textconvert.cfg"

echo Textfile %1.txt created

:end
