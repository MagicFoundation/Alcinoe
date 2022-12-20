Here we will be put the original delphi source code and we will
patch it. For that simply execute the /delphiName/delphiVersion/update.bat
This batch will copy here the source code from your delphi directory 
(assuming you install delphi in c:\Program Files (x86)\Embarcadero\Studio)
and will then apply the patch to it (using git apply command) 

--------

To create the patch file:
* create somewhere a local svn repository:
    * With tortoiseSVN Right-click a folder (ie: c:\temp\LocalRepo for exemple), 
      and in the TortoiseSVN menu, select Create repository here
    * Then With tortoiseSVN Right-click a different folder (ie: c:\temp\LocalCheckOut 
      for exemple), and select SVN CheckOUT, url of the repository: file:///c:\temp\LocalRepo
      Checkout directory: c:\temp\LocalCheckOut
* Commit all the original delphi source code somwhere in an SVN directory
* Update now all the original delphi source code
* Select the directory that contain the modified files
* Right click on it and select "TortoiseSVN > Create patch..."
* Click OK
* Select "Save in Git format" and save the file somewhere 
* Update now the patch file by updating in it all paths to reflect the Alcinoe SVN: 
  "Embarcadero/<delphiName ex:sydney>/<delphiVersion ex:10_4_2>" 
   