DProjVersioning
===============

This tool allows you to get or update the app version directly within 
a .dproj file. It ensures synchronization of the app version across 
all targets (ios32, ios64, android, android64, win32, win64, macOS, 
macOS64) so that all platforms use the same version.

Usage: 

```
  DProjVersioning.exe 
    -DProj="<DprojFilename>" 
    -Action=<getVersionName/incMajorMinorPatchVersion/decMajorMinorPatchVersion> 
    -MajorNumber=<x> 
    -MinorNumber=<y> 
    -PatchBase=<z> 
    -CreateBackup=<true/false>
```

Note: In the project options, you must include version information and enable 
auto-increment for the build number (this can be done only for release builds).

Given a version number MAJOR.MINOR.PATCH, increment the:
  1. MAJOR version when you make incompatible API changes,
  2. MINOR version when you add functionality in a backwards compatible manner, and
  3. PATCH version when you make backwards compatible bug fixes.

Example: If the current VerInfo_Build is 215 and the version name is 2.0.5:

```
DProjVersioning.exe -DProj="{DprojFilename}" -Action=incMajorMinorPatchVersion -MajorNumber=2 -MinorNumber=0 -PatchBase=210
```

This will increment the VerInfo_Build by 1 (from 215 to 216) and update the 
version name to 2.0.6, where the patch version 6 is calculated as 
216 (VerInfo_Build) - 210 (PatchBase).