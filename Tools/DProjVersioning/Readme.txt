DProjVersioning
===============

This tool is to get/update the app version from inside a Dproj file. 
this permit to synch all target (ios32, ios64, android, android64, 
win32, win64, macos, macos64) with the same app version

Usage: 
DProjVersioning.exe 
  -DProj="<DprojFilename>" 
  -Action=<getVersionName/incMajorMinorPatchVersion/decMajorMinorPatchVersion> 
  -MajorNumber=<x> 
  -MinorNumber=<y> 
  -PatchOffset=<z> 
  -CreateBackup=<true/false>

Note: In project options you must include version information and 
choose auto increment build number (you can do it only for release)

Given a version number MAJOR.MINOR.PATCH, increment the:
  1. MAJOR version when you make incompatible API changes,
  2. MINOR version when you add functionality in a backwards compatible manner, and
  3. PATCH version when you make backwards compatible bug fixes.

For exemple: 
* if current VerInfo_Build is 215
* DProjVersioning.exe -DProj="<DprojFilename>" -Action=incMajorMinorPatchVersion -MajorNumber=2 -MinorNumber=0 -PatchOffset=210
  will increase by 1 the VerInfo_Build (so increase 215 to 216) and make the version name equal to
  2.0.6 with the 6 = 216(VerInfo_Build) - 210(PatchOffset)
  
  
  
