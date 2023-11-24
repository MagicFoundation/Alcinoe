Instructions for Patching Delphi Source Code
--------------------------------------------

To patch the original Delphi source code, please follow these steps:

1. Run the batch file located at `/delphiName/update.bat`. 
   This file is designed to automate the patching process.

2. The batch file will first copy locally the source code 
   from your Delphi installation directory. 

3. After copying the source code, the batch file will apply 
   the necessary patch using the `git apply` command.