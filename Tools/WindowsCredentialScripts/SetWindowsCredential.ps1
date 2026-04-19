param (
    [string]$TargetName = "",
    [string]$Username = "",
    [string]$Password = ""
)

# Capture whether params were missing BEFORE prompting, so we know to pause at the end
$MissingParams = (-not $TargetName -or -not $Username -or -not $Password)

function Exit-Script {
    param([int]$Code = 0)
    if ($MissingParams) {
        Write-Host "`nPress any key to close..." -ForegroundColor Yellow
        $null = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
    }
    exit $Code
}

function Read-MultiLinePassword {
    # Read key by key so we capture everything including pasted CRLF.
    # Press F6 to finish.
    Write-Host "Enter Password (press F6 to finish):"
    $sb = New-Object System.Text.StringBuilder
    while ($true) {
        $key = $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown")
        # F6 = virtual key code 117 — used as terminator
        if ($key.VirtualKeyCode -eq 117) {
            Write-Host ""
            break
        }
        # Backspace
        elseif ($key.VirtualKeyCode -eq 8) {
            if ($sb.Length -gt 0) {
                $sb.Remove($sb.Length - 1, 1) | Out-Null
                Write-Host -NoNewline "`b `b"
            }
        }
        # Enter key — append CRLF and move to next line on screen
        elseif ($key.VirtualKeyCode -eq 13) {
            $sb.Append("`r`n") | Out-Null
            Write-Host ""
        }
        # Any other printable character — append and echo the real character
        elseif ($key.Character -ne [char]0) {
            $sb.Append($key.Character) | Out-Null
            Write-Host -NoNewline $key.Character
        }
    }
    return $sb.ToString()
}

# If parameters are missing, prompt the user interactively (clean prompts, no PowerShell noise)
if (-not $TargetName) {
    $TargetName = Read-Host "Enter TargetName"
}

if (-not $Username) {
    $Username = Read-Host "Enter Username"
}

if (-not $Password) {
    $Password = Read-MultiLinePassword
    if (-not $Password) {
        Write-Host "Cancelled (empty password)." -ForegroundColor Yellow
        Exit-Script -Code 1
    }
}

# Guard against re-loading types in the same PowerShell session
if (-not ([System.Management.Automation.PSTypeName]'CredMan').Type) {
    Add-Type @"
using System;
using System.Runtime.InteropServices;
using System.Text;

public class CredMan {
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    public struct CREDENTIAL {
        public int Flags;
        public int Type;
        public string TargetName;
        public string Comment;
        public long LastWritten;
        public int CredentialBlobSize;
        public IntPtr CredentialBlob;
        public int Persist;
        public int AttributeCount;
        public IntPtr Attributes;
        public string TargetAlias;
        public string Username;
    }

    [DllImport("advapi32.dll", CharSet=CharSet.Unicode, SetLastError=true)]
    public static extern bool CredWrite(ref CREDENTIAL cred, int flags);

    public static bool Store(string target, string username, string password) {
        byte[] blob = Encoding.UTF8.GetBytes(password);
        IntPtr ptr = Marshal.AllocHGlobal(blob.Length);
        Marshal.Copy(blob, 0, ptr, blob.Length);
        CREDENTIAL cred = new CREDENTIAL {
            Type               = 1,
            TargetName         = target,
            Username           = username,
            CredentialBlob     = ptr,
            CredentialBlobSize = blob.Length,
            Persist            = 2
        };
        bool result = CredWrite(ref cred, 0);
        Marshal.FreeHGlobal(ptr);
        return result;
    }
}
"@
}

$ok = [CredMan]::Store($TargetName, $Username, $Password)

if ($ok) {
    Write-Host "[OK] Credential stored successfully." -ForegroundColor Green
} else {
    $err = [System.Runtime.InteropServices.Marshal]::GetLastWin32Error()
    Write-Host "[FAIL] Failed to store credential. Win32 error: $err" -ForegroundColor Red
    Exit-Script -Code 1
}

Exit-Script -Code 0
