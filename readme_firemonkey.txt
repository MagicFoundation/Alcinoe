You can download the latest version available at
http://sourceforge.net/projects/alcinoe/

you can access the last svn version at:
svn checkout svn://svn.code.sf.net/p/alcinoe/code/ alcinoe-code 
or with a web browser at:
http://alcinoe.svn.sourceforge.net/viewvc/alcinoe/


INSTALL:
--------
 
If you plan to use native control on android (like TALEdit) then you
will need to create a new classes.dex with contain the alcinoe java source 
file. The alcinoe java source file need to be compiled to .class files, which 
are then archived into a .jar file, converted from Java byte code to DEX 
(Dalvik Executable) format and merged into the normally used (by Delphi's 
Android deployment process) classes.dex.

Open java/build_xx.bat file in editor and set %ANDROID% and %EMBO_DEX% variables
or use the already compiled java\dex\xx\classes.dex 
  
Delphi default classes.dex file must be unchecked in Deployment window 
and new java\dex\xx\classes.dex with classes\ remote path name, must be added 
in the Deployment window. (see classes_dex.png image)

(Thanks to "Brian Long" and "Babak Yaghoobi" for merging dex files)