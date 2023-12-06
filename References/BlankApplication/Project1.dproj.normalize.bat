@echo off

del "%~dp0\Project1.dproj.bak" /s

call "%~dp0\..\..\Tools\DProjNormalizer\DProjNormalizer.exe" -DProj="%~dp0\Project1.dproj" -CreateBackup="true"