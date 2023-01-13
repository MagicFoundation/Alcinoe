This tool is to answer this problem: 
https://quality.embarcadero.com/browse/RSP-28003

Usage: 
DProjNormalizer.exe "<DprojFilename>" <createBackup>(ie: true/false)

DProjNormalizer will order all nodes so that the Dproj stay consistent
between each commit for easy diff compare. It's will also remove
from deployement all unnecessary items like all items that 
are automatiquelly added by the IDE every time you open the DPROJ (icons, 
launchscreen, binary, etc.)
