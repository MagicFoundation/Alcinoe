@echo off

set ALBaseDir=%~dp0

"%ALBaseDir%\..\..\..\..\Tools\AndroidMerger\AndroidMerger.exe"^
 -LocalMavenRepositoryDir="%ALBaseDir%\..\..\..\..\Libraries\jar\"^
 -Libraries="%ALBaseDir%\ALConfettiDemoApp\"^
 -OutputDir="%ALBaseDir%\Merged"^
 -DProj="%ALBaseDir%\..\ALConfettiDemo.dproj"^
 -AndroidManifest="%ALBaseDir%\..\AndroidManifest.template.xml"^
 -DProjNormalizer="%ALBaseDir%\..\..\..\..\Tools\DProjNormalizer\DProjNormalizer.exe"^
 -RJarSwapper="%ALBaseDir%\..\..\..\..\Tools\RJarSwapper\RJarSwapper.bat"^
 -UseGradle=true