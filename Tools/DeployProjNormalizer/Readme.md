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

To verify that this tool still work with a new delphi compiler: 
1/ Create a new multi plateform blank project. You can create it
   in {Alcinoe}\References\BlankApplication\
2/ Deploy each plateform in release and debug and each time run 
   this tool against the dproj with createBackup:true and make 
   a diff compare with the new generated deployproj vs deployproj.bak
3/ Run DProjNormalizer on the Dproj and do again the previous step  
4/ if you do not detect any difference then it's ok else you need
   to update the source code