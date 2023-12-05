@echo off

del "%~dp0\Project1.deployproj.bak" /s

call "%~dp0\..\..\Tools\DeployProjNormalizer\DeployProjNormalizer.exe" -DProj="%~dp0\Project1.dproj" -CreateBackup="true"