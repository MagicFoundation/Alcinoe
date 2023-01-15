DProjNormalizer
===============

Order all nodes in a DProj so that the Dproj stay consistent 
between each commit for easy diff compare. It's will also 
remove from deployement all unnecessary items like all items 
that are automatiquelly added by the IDE every time you open 
the DPROJ (icons, launchscreen, binary, etc.)

It's was made to answer this problem: 
[https://quality.embarcadero.com/browse/RSP-28003](https://quality.embarcadero.com/browse/RSP-28003)

Usage: 
DProjNormalizer.exe -DProj="{DprojFilename}" -CreateBackup={true/false}
