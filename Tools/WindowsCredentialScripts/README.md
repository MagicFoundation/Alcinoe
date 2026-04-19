## GetWindowsCredential.ps1

This PowerShell script retrieves a stored Windows generic credential 
by its target name and returns either the username or password via 
the `-Field` parameter.

It requires:
- `-TargetName`: credential name
- `-Field`: `Username` or `Password`

It uses embedded C# (`CredRead`, `CredFree`) to access the Windows 
Credential API and interprets both username and password as UTF-8 
encoded values before returning the result.

Usage:

```powershell
.\GetWindowsCredential.ps1 -TargetName "MyTargetName" -Field Username
.\GetWindowsCredential.ps1 -TargetName "MyTargetName" -Field Password
```

Example:

```cmd
SET PASSWORD=
for /f "usebackq delims=" %%i in (
  `powershell -NoProfile -ExecutionPolicy Bypass -File "GetWindowsCredential.ps1" -TargetName "MyTargetName" -Field Password`
) do set PASSWORD=%%i
```


## SetWindowsCredential.ps1

This PowerShell script stores a Windows generic credential in Credential 
Manager. It prompts for target name, username, and password, then uses 
embedded C# (`CredWrite`) to store the credential. Both username and 
password are stored using UTF-8 encoding.

Usage:

```powershell
.\SetWindowsCredential.ps1 -TargetName "MyTargetName" -Username "MyUsername" -Password "MyPassword"
```