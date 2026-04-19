# Windows Manager Credential Extractor Script

This PowerShell script retrieves a stored Windows generic credential by 
its target name and returns either the username or the password based 
on the specified `-Field` parameter; 

it accepts two mandatory parameters: `-TargetName` (the name of the credential) 
and `-Field` (which must be either "UserName" or "Password"), utilizes 
embedded C# code to call Windows Credential API functions such 
as `CredRead` and `CredFree` to read the credential data, converts the 
password from a UTF-16 encoded blob if present, and outputs the requested 
information, all while requiring a Windows system with PowerShell 5.1 or 
later and proper permissions.

To Store a Username/Password Pair in Windows Credential Manager:
  - Open Credential Manager from the Control Panel.
  - Select the Windows Credentials tab.
  - Click "Add a generic credential."
  - Internet or network address: {TargetName}
  - User Name:{UserName} 
  - Password:{Password} 
    
Exemple:

```
SET PASSWORD=
for /f "usebackq delims=" %%i in (
  `powershell -NoProfile -ExecutionPolicy Bypass -File "GetWindowsCredential.ps1" -TargetName "MyTargetName" -Field Password`
) do set PASSWORD=%%i
```