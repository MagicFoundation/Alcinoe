param (
    [Parameter(Mandatory=$true)]
    [string]$TargetName,
    
    [Parameter(Mandatory=$true)]
    [ValidateSet("UserName","Password")]
    [string]$Field
)

Add-Type -TypeDefinition @"
using System;
using System.Runtime.InteropServices;

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
    public IntPtr UserName;
}

public static class CredentialUtil {
    [DllImport("advapi32", SetLastError = true, CharSet = CharSet.Unicode)]
    public static extern bool CredRead(string target, int type, int reservedFlag, out IntPtr credentialPtr);

    [DllImport("advapi32", SetLastError = true)]
    public static extern void CredFree(IntPtr buffer);
}
"@

function Get-StoredCredential {
    param([string]$TargetName)
    
    $credPtr = [IntPtr]::Zero
    $CRED_TYPE_GENERIC = 1

    if ([CredentialUtil]::CredRead($TargetName, $CRED_TYPE_GENERIC, 0, [ref]$credPtr)) {
        $credStruct = [Runtime.InteropServices.Marshal]::PtrToStructure($credPtr, [Type][CREDENTIAL])
        $username = [Runtime.InteropServices.Marshal]::PtrToStringUni($credStruct.UserName)
        $password = ""
        if ($credStruct.CredentialBlobSize -gt 0) {
            # Divide size by 2 for UTF-16 characters
            $password = [Runtime.InteropServices.Marshal]::PtrToStringUni($credStruct.CredentialBlob, $credStruct.CredentialBlobSize / 2)
        }
        [CredentialUtil]::CredFree($credPtr)
        return @{ UserName = $username; Password = $password.TrimEnd([char]0) }
    }
    else {
        $errorCode = [Runtime.InteropServices.Marshal]::GetLastWin32Error()
        Write-Error "CredRead failed with error code: $errorCode"
        exit 1
    }
}

$cred = Get-StoredCredential -TargetName $TargetName

if ($Field -eq "UserName") {
    Write-Output $cred.UserName
} elseif ($Field -eq "Password") {
    Write-Output $cred.Password
}