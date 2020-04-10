This tool is to answer this problem: 
https://quality.embarcadero.com/browse/RSP-28003

Usage: 
DprojNormalizer.exe "<DprojFilename>" <createBackup>(ie: true/false)

Note: this tools is not 100% efficient, for exemple it's consider that
you can only enable/disable some default deployements but it's not
consider that you could also update the deployment paths, etc. in short
this tool delete every default items in deployment section of the 
Dproj except thoses where enabled is overriden to false. 
!! Use with caution and always make a backup of your Dproj !!