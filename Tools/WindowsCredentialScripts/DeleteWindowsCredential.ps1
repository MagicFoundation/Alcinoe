param (
    [string]$TargetName = ""
)

# Capture whether the parameter was missing BEFORE prompting, so we know to pause at the end
$MissingParams = (-not $TargetName)

function Exit-Script {
    param([int]$Code = 0)
    if ($MissingParams) {
        Write-Host "`nPress any key to close..." -ForegroundColor Yellow
        $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    }
    exit $Code
}

# If the parameter is missing, prompt the user interactively
if (-not $TargetName) {
    $TargetName = Read-Host "Enter TargetName"
    if (-not $TargetName) {
        Write-Host "Cancelled (empty TargetName)." -ForegroundColor Yellow
        Exit-Script -Code 1
    }
}

# Guard against re-loading the type in the same PowerShell session
if (-not ([System.Management.Automation.PSTypeName]'CredManDelete').Type) {
    Add-Type @"
using System;
using System.Runtime.InteropServices;

public class CredManDelete {
    [DllImport("advapi32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
    public static extern bool CredDelete(string target, int type, int flags);

    public static bool Delete(string target) {
        // Type 1 = CRED_TYPE_GENERIC
        return CredDelete(target, 1, 0);
    }
}
"@
}

$ok = [CredManDelete]::Delete($TargetName)

if ($ok) {
    Write-Host "[OK] Credential deleted successfully." -ForegroundColor Green
} else {
    $err = [System.Runtime.InteropServices.Marshal]::GetLastWin32Error()
    if ($err -eq 1168) {
        Write-Host "[FAIL] Credential not found: $TargetName" -ForegroundColor Red
    } else {
        Write-Host "[FAIL] Failed to delete credential. Win32 error: $err" -ForegroundColor Red
    }
    Exit-Script -Code 1
}

Exit-Script -Code 0
