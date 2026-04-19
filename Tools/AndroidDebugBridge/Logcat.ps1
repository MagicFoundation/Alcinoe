# =====================================================
# ADB Logcat - App Package + Filter + Level
# =====================================================

# --------------------- Set exact console colors + font ---------------------
$colorTableCode = @'
using System;
using System.Runtime.InteropServices;

public struct COORD { public short X; public short Y; }
public struct SMALL_RECT { public short Left; public short Top; public short Right; public short Bottom; }

[StructLayout(LayoutKind.Sequential)]
public struct CONSOLE_SCREEN_BUFFER_INFO_EX {
    public uint  cbSize;
    public COORD dwSize;
    public COORD dwCursorPosition;
    public ushort wAttributes;
    public SMALL_RECT srWindow;
    public COORD dwMaximumWindowSize;
    public ushort wPopupAttributes;
    public bool  bFullscreenSupported;
    [MarshalAs(UnmanagedType.ByValArray, SizeConst = 16)]
    public uint[] ColorTable;
}

public class ConsoleHelper {
    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern IntPtr GetStdHandle(int nStdHandle);

    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern bool GetConsoleScreenBufferInfoEx(IntPtr hConsoleOutput, ref CONSOLE_SCREEN_BUFFER_INFO_EX info);

    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern bool SetConsoleScreenBufferInfoEx(IntPtr hConsoleOutput, ref CONSOLE_SCREEN_BUFFER_INFO_EX info);

    // Convert #RRGGBB to the COLORREF format Windows uses (0x00BBGGRR)
    public static uint ToColorRef(byte r, byte g, byte b) {
        return (uint)((b << 16) | (g << 8) | r);
    }

    public static void SetColorTableEntry(int index, byte r, byte g, byte b) {
        IntPtr handle = GetStdHandle(-11); // STD_OUTPUT_HANDLE
        var info = new CONSOLE_SCREEN_BUFFER_INFO_EX();
        info.cbSize = (uint)Marshal.SizeOf(info);
        GetConsoleScreenBufferInfoEx(handle, ref info);
        info.ColorTable[index] = ToColorRef(r, g, b);
        // Windows shrinks the window by 1 row on SetConsoleScreenBufferInfoEx — compensate
        info.srWindow.Bottom++;
        SetConsoleScreenBufferInfoEx(handle, ref info);
    }
}

public class ConsoleFont {
    [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
    public struct CONSOLE_FONT_INFOEX {
        public uint cbSize;
        public uint nFont;
        public COORD dwFontSize;
        public int FontFamily;
        public int FontWeight;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 32)]
        public string FaceName;
    }

    [DllImport("kernel32.dll", SetLastError = true)]
    public static extern IntPtr GetStdHandle(int nStdHandle);

    [DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
    public static extern bool SetCurrentConsoleFontEx(IntPtr hOut, bool bMaximumWindow, ref CONSOLE_FONT_INFOEX cfi);

    public static void SetFont(string faceName, short size) {
        IntPtr handle = GetStdHandle(-11);
        var cfi = new CONSOLE_FONT_INFOEX();
        cfi.cbSize     = (uint)Marshal.SizeOf(cfi);
        cfi.FaceName   = faceName;
        cfi.dwFontSize = new COORD { X = 0, Y = size };
        cfi.FontWeight = 400;
        SetCurrentConsoleFontEx(handle, false, ref cfi);
    }
}
'@

if (-not ([System.Management.Automation.PSTypeName]'ConsoleHelper').Type) {
    Add-Type -TypeDefinition $colorTableCode
}

function Set-ConsolePalette {
    # Black      -> #303030
    [ConsoleHelper]::SetColorTableEntry(0,  0x30, 0x30, 0x30)
    # Blue       -> #5c97c7
    [ConsoleHelper]::SetColorTableEntry(9,  0x5c, 0x97, 0xc7)
    # Green      -> #9bc736
    [ConsoleHelper]::SetColorTableEntry(10, 0x9b, 0xc7, 0x36)
    # Cyan       -> #5391c4
    [ConsoleHelper]::SetColorTableEntry(11, 0x53, 0x91, 0xc4)
    # Red        -> #e73925
    [ConsoleHelper]::SetColorTableEntry(12, 0xe7, 0x39, 0x25)
    # Magenta    -> #af86fa
    [ConsoleHelper]::SetColorTableEntry(13, 0xaf, 0x86, 0xfa)
    # Yellow     -> #fbd655
    [ConsoleHelper]::SetColorTableEntry(14, 0xfb, 0xd6, 0x55)
    # Gray       -> #7d867c
    [ConsoleHelper]::SetColorTableEntry(7,  0x7d, 0x86, 0x7c)
    # White      -> #dcdcd4
    [ConsoleHelper]::SetColorTableEntry(15, 0xdc, 0xdc, 0xd4)
}

function Restore-ConsolePalette {
    # Restore standard Windows console defaults
    [ConsoleHelper]::SetColorTableEntry(0,  0x00, 0x00, 0x00) # Black
    [ConsoleHelper]::SetColorTableEntry(7,  0xc0, 0xc0, 0xc0) # Gray
    [ConsoleHelper]::SetColorTableEntry(9,  0x00, 0x00, 0xff) # Blue
    [ConsoleHelper]::SetColorTableEntry(10, 0x00, 0xff, 0x00) # Green
    [ConsoleHelper]::SetColorTableEntry(11, 0x00, 0xff, 0xff) # Cyan
    [ConsoleHelper]::SetColorTableEntry(12, 0xff, 0x00, 0x00) # Red
    [ConsoleHelper]::SetColorTableEntry(13, 0xff, 0x00, 0xff) # Magenta
    [ConsoleHelper]::SetColorTableEntry(14, 0xff, 0xff, 0x00) # Yellow
    [ConsoleHelper]::SetColorTableEntry(15, 0xff, 0xff, 0xff) # White
}

[ConsoleFont]::SetFont("Consolas", 21)
Set-ConsolePalette

$Host.UI.RawUI.BackgroundColor = 'Black'   # renders as #303030
$Host.UI.RawUI.ForegroundColor = 'White'   # renders as #dcdcd4
Clear-Host

# Helper: Read-Host with prompt rendered in #dcdcd4
function Prompt-Read {
    param([string]$Prompt)
    $prev = $Host.UI.RawUI.ForegroundColor
    $Host.UI.RawUI.ForegroundColor = 'White'
    $result = Read-Host $Prompt
    $Host.UI.RawUI.ForegroundColor = $prev
    return $result
}

# --------------------- Resolve ADB Path ---------------------
if (-not $env:ALBaseDir) {
    $env:InitEnvironmentQuietMode = "1"
    $initBat = Join-Path $PSScriptRoot "..\..\InitEnvironment.bat"
    if (-not (Test-Path $initBat)) {
        Write-Host "Error: InitEnvironment.bat not found at: $initBat" -ForegroundColor Red
        pause; exit 1
    }
    $envDump = cmd /c "call `"$initBat`" && set"
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Error: InitEnvironment.bat failed." -ForegroundColor Red
        pause; exit 1
    }
    foreach ($line in $envDump) {
        if ($line -match '^([^=]+)=(.*)$') {
            [System.Environment]::SetEnvironmentVariable($Matches[1], $Matches[2], 'Process')
        }
    }
}

$delphiVersion = $env:ALDelphiVersion
if ([string]::IsNullOrWhiteSpace($delphiVersion)) {
    Write-Host "Error: ALDelphiVersion environment variable is not set." -ForegroundColor Red
    pause; exit 1
}

$regKey = "HKCU:\SOFTWARE\Embarcadero\BDS\$delphiVersion\Environment Variables"
try {
    $bdsPlat = (Get-ItemProperty -Path $regKey -Name "BDSPLATFORMSDKSDIR" -ErrorAction Stop).BDSPLATFORMSDKSDIR
} catch {
    Write-Host "Error: Unable to read BDSPLATFORMSDKSDIR from registry key:" -ForegroundColor Red
    Write-Host "  $regKey" -ForegroundColor Red
    pause; exit 1
}

$adbPath = Join-Path $bdsPlat "android\platform-tools\adb.exe"
if (-not (Test-Path $adbPath)) {
    Write-Host "Error: Unable to locate adb.exe at:" -ForegroundColor Red
    Write-Host "  $adbPath" -ForegroundColor Red
    Write-Host "Please ensure the ADB path is correctly set in the registry and adb.exe is available." -ForegroundColor Red
    pause; exit 1
}

# --------------------- Package History ---------------------
$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$iniFile = Join-Path $scriptDir "logcat.ini"

function Get-SavedPackages {
    $packages = @()
    if (Test-Path $iniFile) {
        $content = Get-Content $iniFile -Raw -Encoding UTF8
        if ($content -match '(?s)\[Packages\](.*)') {
            $section = $Matches[1].Trim()
            $lines = $section -split "`r?`n"
            foreach ($line in $lines) {
                $t = $line.Trim()
                if ($t -and -not $t.StartsWith('[')) { $packages += $t }
            }
        }
    }
    return @($packages | Sort-Object -Unique)
}

function Save-Package {
    param([string]$NewPackage)
    if ([string]::IsNullOrWhiteSpace($NewPackage)) { return }
    $pkgs = @(Get-SavedPackages)
    if ($pkgs -notcontains $NewPackage) { $pkgs += $NewPackage }
    "[Packages]`r`n" + ($pkgs -join "`r`n") | Set-Content $iniFile -Encoding UTF8 -Force
    Write-Host "Saved: $NewPackage" -ForegroundColor Gray
}

$savedPackages = @(Get-SavedPackages)

# --------------------- Select Package ---------------------
if ($savedPackages.Count -gt 0) {
    Write-Host "Previously used packages:"
    for ($i = 0; $i -lt $savedPackages.Count; $i++) {
        Write-Host "  $($i+1). $($savedPackages[$i])"
    }
    Write-Host ""
    $choice = Prompt-Read "Select App Package, N for new, or blank for all"
    if ($choice -match '^\d+$' -and [int]$choice -ge 1 -and [int]$choice -le $savedPackages.Count) {
        $packageName = $savedPackages[[int]$choice - 1]
        Write-Host "Selected: $packageName" -ForegroundColor Gray
    } elseif ($choice -eq '') {
        $packageName = ''
        Write-Host "No package filter - logging all packages" -ForegroundColor Gray
    } else {
        $packageName = Prompt-Read "Enter new app package name"
        if (-not [string]::IsNullOrWhiteSpace($packageName)) { Save-Package $packageName }
    }
} else {
    $packageName = Prompt-Read "Enter app package name (or leave blank to log all packages)"
    if (-not [string]::IsNullOrWhiteSpace($packageName)) { Save-Package $packageName }
}

Write-Host ""

# --------------------- Log Level & Filter ---------------------
$logLevel = Prompt-Read "Minimum log level [V/D/I/W/E/F/S] (default V)"
if ([string]::IsNullOrWhiteSpace($logLevel)) { $logLevel = "V" }
$logLevel = $logLevel.Trim().ToUpper()

$levelNames = @{ 'V'='Verbose'; 'D'='Debug'; 'I'='Info'; 'W'='Warning'; 'E'='Error'; 'F'='Fatal'; 'S'='Silent' }
$levelLabel = if ($levelNames.ContainsKey($logLevel)) { $levelNames[$logLevel] } else { $logLevel }
Write-Host "Selected $levelLabel log level" -ForegroundColor Gray

Write-Host ""

$filterText = Prompt-Read "Filter text / RegEx (optional)"
$filterRegex = $filterText
if (-not [string]::IsNullOrWhiteSpace($filterText)) {
    try {
        [void][regex]::new($filterText)
    } catch {
        Write-Host "Invalid RegEx pattern - treating as literal text." -ForegroundColor Yellow
        $filterRegex = [regex]::Escape($filterText)
    }
}

# --------------------- Setup Console ---------------------
$oldBg = $Host.UI.RawUI.BackgroundColor
$oldFg = $Host.UI.RawUI.ForegroundColor
Clear-Host

function Show-Header {
    param([string]$AppPackage, [string]$CurrentFilter, [string]$CurrentLevel)

    Write-Host "====================================================" -ForegroundColor Magenta
    if ($AppPackage) {
        Write-Host " App: $AppPackage" -ForegroundColor Magenta
    } else {
        Write-Host " All packages" -ForegroundColor Magenta
    }
    Write-Host " Minimum level: $CurrentLevel" -ForegroundColor Magenta
    if ($CurrentFilter) {
        Write-Host " Filter: $CurrentFilter" -ForegroundColor Magenta
    }
    Write-Host " C = Clear" -ForegroundColor White
    Write-Host "====================================================" -ForegroundColor Magenta
    Write-Host ""
}

function Refresh-AppPids {
    param([string]$Package)
    $script:_freshPids = [System.Collections.Generic.HashSet[string]]::new()
    if ([string]::IsNullOrWhiteSpace($Package)) { return }
    $raw = "$( & $adbPath shell pidof $Package 2>$null )".Trim()
    if ($raw) {
        foreach ($p in ($raw -split '\s+')) {
            if ($p -match '^\d+$') { [void]$script:_freshPids.Add($p) }
        }
    }
}

Show-Header $packageName $filterText $logLevel
& $adbPath logcat -c 2>$null

$knownPids = [System.Collections.Generic.HashSet[string]]::new()
Refresh-AppPids $packageName
if ($script:_freshPids.Count -gt 0) {
    $knownPids = [System.Collections.Generic.HashSet[string]]::new($script:_freshPids)
}

$job = Start-Job -ScriptBlock { param($a,$l) & $a logcat -v threadtime "*:$l" 2>$null } -ArgumentList $adbPath, $logLevel

# --------------------- Background Key Reader (Runspace) ---------------------
$keyQueue = [System.Collections.Concurrent.ConcurrentQueue[string]]::new()

$rsPool = [runspacefactory]::CreateRunspacePool(1, 1)
$rsPool.Open()

$ps = [powershell]::Create()
$ps.RunspacePool = $rsPool
[void]$ps.AddScript({
    param($queue, $hostRef)
    $flags = [System.Management.Automation.Host.ReadKeyOptions]::NoEcho -bor
             [System.Management.Automation.Host.ReadKeyOptions]::IncludeKeyDown
    while ($true) {
        try {
            $k = $hostRef.UI.RawUI.ReadKey($flags)
            $queue.Enqueue($k.Character.ToString().ToUpper())
        } catch {
            break
        }
    }
}).AddArgument($keyQueue).AddArgument($Host)

$psHandle = $ps.BeginInvoke()

# --------------------- Main Loop ---------------------
$running = $true
try {
    while ($running) {

        $keyStr = $null
        while ($keyQueue.TryDequeue([ref]$keyStr)) {
            switch ($keyStr) {
                'C' {
                    & $adbPath logcat -c 2>$null
                    Clear-Host
                    Show-Header $packageName $filterText $logLevel
                    Receive-Job $job | Out-Null
                }
            }
        }

        if (-not $running) { break }

        if (-not [string]::IsNullOrWhiteSpace($packageName)) {
            Refresh-AppPids $packageName
            if ($script:_freshPids.Count -gt 0) {
                if (-not $knownPids.SetEquals($script:_freshPids)) {
                    $knownPids = [System.Collections.Generic.HashSet[string]]::new($script:_freshPids)
                    Write-Host "[PID] Tracking $($knownPids.Count) PIDs" -ForegroundColor Gray
                }
            } elseif ($knownPids.Count -gt 0) {
                $knownPids.Clear()
                Write-Host "[PID] App not running - waiting..." -ForegroundColor Yellow
            }
        }

        $output = Receive-Job $job
        if ($output) {
            foreach ($line in $output) {

                # Skip any line that does not match the expected logcat format
                if ($line -notmatch '^(?<date>\d\d-\d\d)\s+(?<time>[\d:.]+)\s+(?<pid>\d+)\s+(?<tid>\d+)\s+(?<prio>[VDIWEFS])\s+(?<tag>.*?):\s?(?<msg>.*)$') { continue }

                # Save captures now — the filter -notmatch below will overwrite $Matches
                $m = $Matches

                if (-not [string]::IsNullOrWhiteSpace($packageName) -and ($knownPids.Count -eq 0 -or -not $knownPids.Contains($m.pid))) { continue }
                if ($filterText -and $line -notmatch "(?i)$filterRegex") { continue }

                $color = switch ($m.prio) {
                    'V' { 'Gray'   }
                    'D' { 'Blue'   }
                    'I' { 'White'  }
                    'W' { 'Yellow' }
                    'E' { 'Red'    }
                    'F' { 'Red'    }
                    default { 'White' }
                }

                Write-Host "$($m.date) $($m.time) $($m.pid) $($m.prio) $($m.tag): $($m.msg)" -ForegroundColor $color
            }
        }

        Start-Sleep -Milliseconds 10
    }
} finally {
    $ps.Stop()
    $ps.Dispose()
    $rsPool.Close()
    $rsPool.Dispose()

    Stop-Job $job -ErrorAction SilentlyContinue
    Remove-Job $job -ErrorAction SilentlyContinue
    Write-Host "Logcat stopped." -ForegroundColor Gray

    Restore-ConsolePalette
    $Host.UI.RawUI.BackgroundColor = $oldBg
    $Host.UI.RawUI.ForegroundColor = $oldFg
    Clear-Host
}

pause