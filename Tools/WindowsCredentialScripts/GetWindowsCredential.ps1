param (
    [string]$TargetName = "",
    [string]$Field = ""
)

# Capture whether params were missing BEFORE prompting, so we know to pause at the end
$MissingParams = (-not $TargetName -or -not $Field)

function Exit-Script {
    param([int]$Code = 0)
    if ($MissingParams) {
        Write-Host "`nPress any key to close..." -ForegroundColor Yellow
        $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    }
    exit $Code
}

# If TargetName is missing, prompt for it
if (-not $TargetName) {
    $TargetName = Read-Host "Enter TargetName"
}

# If Field is missing, we will output both Username and Password (no prompt)
$OutputBoth = (-not $Field)

# Guard against re-loading types in the same PowerShell session
if (-not ([System.Management.Automation.PSTypeName]'CredentialUtil').Type) {
    Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;
using System.Text;

[StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
public struct CREDENTIAL {
    public int Flags;
    public int Type;
    public IntPtr TargetName;
    public IntPtr Comment;
    public System.Runtime.InteropServices.ComTypes.FILETIME LastWritten;
    public int CredentialBlobSize;
    public IntPtr CredentialBlob;
    public int Persist;
    public int AttributeCount;
    public IntPtr Attributes;
    public IntPtr TargetAlias;
    public IntPtr Username;
}

public static class CredentialUtil {
    [DllImport("advapi32", SetLastError = true, CharSet = CharSet.Unicode)]
    public static extern bool CredRead(string target, int type, int reservedFlag, out IntPtr credentialPtr);

    [DllImport("advapi32", SetLastError = true)]
    public static extern void CredFree(IntPtr buffer);

    public static string BlobToUtf8String(IntPtr blobPtr, int blobSize) {
        if (blobPtr == IntPtr.Zero || blobSize <= 0) return "";
        byte[] bytes = new byte[blobSize];
        Marshal.Copy(blobPtr, bytes, 0, blobSize);
        // Strip trailing null bytes
        int length = blobSize;
        while (length > 0 && bytes[length - 1] == 0) length--;
        if (length == 0) return "";
        return Encoding.UTF8.GetString(bytes, 0, length);
    }
}
"@
}

function Get-StoredCredential {
    param([string]$TargetName)

    $credPtr = [IntPtr]::Zero
    $CRED_TYPE_GENERIC = 1

    if ([CredentialUtil]::CredRead($TargetName, $CRED_TYPE_GENERIC, 0, [ref]$credPtr)) {
        $credStruct = [Runtime.InteropServices.Marshal]::PtrToStructure($credPtr, [Type][CREDENTIAL])
        $username = [Runtime.InteropServices.Marshal]::PtrToStringUni($credStruct.Username)
        $password = ""
        if ($credStruct.CredentialBlobSize -gt 0) {
            $password = [CredentialUtil]::BlobToUtf8String($credStruct.CredentialBlob, $credStruct.CredentialBlobSize)
        }
        [CredentialUtil]::CredFree($credPtr)
        return @{ Username = $username; Password = $password }
    }
    else {
        $errorCode = [Runtime.InteropServices.Marshal]::GetLastWin32Error()
        # Error code 1168 = ERROR_NOT_FOUND, return empty values silently
        if ($errorCode -eq 1168) {
            return @{ Username = ""; Password = "" }
        }
        Write-Error "CredRead failed with error code: $errorCode"
        Exit-Script -Code 1
    }
}

$cred = Get-StoredCredential -TargetName $TargetName

if ($OutputBoth) {
    Write-Host "Username: $($cred.Username)"
    Write-Host "Password: $($cred.Password)"
} elseif ($Field -eq "Username") {
    Write-Output $cred.Username
} elseif ($Field -eq "Password") {
    Write-Output $cred.Password
}

Exit-Script -Code 0
