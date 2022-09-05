-------------------------------------UIB SQL MONITOR-----------------------------------

This tool show in real time queries used by an application connected to Interbase or Firebird.
It is also possible to view and sort query's performances. 
This SQL monitor usually don't need you modify your application if it use an Interbase or Firebird 
library (gds32.dll, fbclient.dll or fbembed.dll).
It also works with dot.net applications.

-- Knows issues --

Sometimes nothing appear in the scroll box

   1. Have you selected the righ library ?.
   2. Hooked program can't find "madCHook.dll", in this case copy this DLL in system32 directory.
   3. You need administrator rights.

-- "Firebird .Net Data Provider" compatibility --

You have to change connection string like this:

FbConnectionStringBuilder *cs = new FbConnectionStringBuilder();
cs->DataSource = "localhost";
cs->Database = "C:\\Program Files\\Firebird\\Firebird_2_0\\examples\\empbuild\\EMPLOYEE.FDB";
cs->UserID = "SYSDBA";
cs->Password = "masterkey";
cs->Dialect = 3;
cs->ServerType = 1; // <<<<< here

In this case your application will use "fbembed.dll", so just copy "fbclient.dll" 
in the hooked application directory and rename it.

-- Used tools --

MadCodeHook: http://www.madshi.net
VirtualTreeView: http://www.delphi-gems.com/VirtualTreeview
SynEdit: http://synedit.sourceforge.net
UIB: http://www.progdigy.com


Henri Gourvest
http://www.progdigy.com