This tool is to get/update the app version from inside a Dproj file. 
this permit to synch all target (ios32, ios64, android, android64, 
win32, win64, macos, macos64) with the same app version

Usage: 
DprojVersioning.exe "<DprojFilename>" getVersionName
DprojVersioning.exe "<DprojFilename>" incMajorMinorPatchVersion <major>(ie: 2) <minor>(ie: 5) <PatchOffset>(ie: 210) <createBackup>(ie: true/false)

Note: in project options you must include version information and 
choose auto increment build number (you can do it only for release)

Given a version number MAJOR.MINOR.PATCH, increment the:
  1. MAJOR version when you make incompatible API changes,
  2. MINOR version when you add functionality in a backwards compatible manner, and
  3. PATCH version when you make backwards compatible bug fixes.

For exemple: 
* if current VerInfo_Build is 215
* DprojVersioning.exe "<DprojFilename>" incMajorMinorPatchVersion 2 0 210 false
  will increase by 1 the VerInfo_Build (so increase 215 to 216) and make the version name equal to
  2.0.6 with the 6 = 216(VerInfo_Build) - 210(PatchOffset)
  
  
  
