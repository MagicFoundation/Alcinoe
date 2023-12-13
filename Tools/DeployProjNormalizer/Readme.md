DeployProjNormalizer
====================

Create from the dproj a new deployproj file from scratch 
and normalize it (ie: order the node so that you can 
compare different revision with diff compare tools).

It's was made to answer this problem: 
[https://quality.embarcadero.com/browse/RSP-27612](https://quality.embarcadero.com/browse/RSP-27612)

Usage: 
DeployProjNormalizer.exe -DProj="{DprojFilename}" -CreateBackup={true/false}

When you choose to create a backup the tool will normalize 
the existing deployproj and rename it to deployproj.bak so that 
you can make a diff compare against the new generated dployproj