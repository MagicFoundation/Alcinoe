SQLite for Delphi and FreePascal/Lazarus
========================================

SQLite is a software library that implements a self-contained, serverless, zero-configuration,
transactional SQL database engine. The source code for SQLite is in the public domain and is thus
free for use for any purpose, commercial or private. SQLite is the most widely deployed SQL database
engine in the world.

This package contains complete SQLite3 API translation for Delphi and FreePascal/Lazarus, as well as
a simple Unicode-enabled object wrapper to simplify the use of this database engine.


Compatibility
-------------

Compatible with Delphi/C++Builder 6–XE5 and FreePascal/Lazarus.


Installation and usage notes
----------------------------

Unzip the package somewhere on your hard drive. Now you need to add the full path of the Source
folder to your IDE options.

#### For Delphi 6–7 or C++Builder 6
Select *Tools > Environment Options* from the menu, then go to the *Library* tab and add the full
path of this package's Source directory to the *Library Path*.

#### For Delphi 2005 and up, or C++Builder 2006 and up
Select *Tools > Options* from the menu, then select *Environment Options > Delphi Options >
Library - Win32*, and add the full path of this package's Source directory to the *Library Path*.

#### For Lazarus
Select *Environment > Options* from the menu, then select *CodeTools*, and add the full path of this
package's Source directory to the *Search Path* ("Additional source search path for all projects").

When using this translation in your projects, add the SQLite3 unit to the uses clause of your source
file if you want to use SQLite3 API, and the SQLite3Wrap unit if you want to use the accompanying
wrapper.

Do not forget to distribute the shared library (sqlite3.dll for Windows, sqlite3.so for Linux) with
your program.


Credits
-------

Yury Plashenkov (http://www.plashenkov.com), developer.  
Marek Mauder (http://galfar.vevb.net), contributor.
