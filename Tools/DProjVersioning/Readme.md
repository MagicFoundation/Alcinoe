DProjVersioning
===============

**DProjVersioning** is a command-line tool to read and update application versioning
directly inside a Delphi `.dproj` file. It keeps **all platforms in sync**:

- Windows: `VerInfo_*`, `FileVersion`, `ProductVersion`
- Android: `versionName`, `versionCode`
- iOS / macOS: `CFBundleShortVersionString`, `CFBundleVersion`


## Usage

```
DProjVersioning.exe
  -DProj="<file.dproj>"
  -GetVersionName | -GetVersionInfo | -IncVersion
  -SetVersionInfo="<VersionName>+<BuildNumber>"
  [-CreateBackup=false]
```


## Commands

### `-GetVersionName`

Prints the semantic version name.

```
2.0.5
```


### `-GetVersionInfo`

Prints combined version info.

```
2.0.5+190
```


### `-SetVersionInfo`

Sets version name **and** build number across all targets.

```
DProjVersioning.exe -DProj="MyApp.dproj" -SetVersionInfo="2.0.5+190"
```

Updates:
- Windows version resources
- Android `versionName` / `versionCode`
- iOS / macOS bundle versions


### `-IncVersion`

Increments patch version and build number:

```
x.y.z   -> x.y.(z+1)
build   -> build+1
```

Example:

```
2.0.5+190 -> 2.0.6+191
```

Optional overrides:

- `-MajorNumber=<x>`
- `-MinorNumber=<y>`

When major or minor is overridden, the patch number is reset.


## Backup behavior

By default a backup is created:

```
<Project>.dproj.bak
```

Disable with:

```
-CreateBackup=false
```


## Requirements

- `.dproj` must contain version information
- Build number must be enabled in Project Options


## CI example

```
DProjVersioning.exe -DProj="MyApp.dproj" -GetVersionInfo
DProjVersioning.exe -DProj="MyApp.dproj" -IncVersion -CreateBackup=false
```
