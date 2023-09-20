@echo off

set ALBaseDir=%~dp0

set Libraries=%ALBaseDir%\App\
set Libraries=%Libraries%;com.alcinoe:alcinoe-edittext:1.0.0
set Libraries=%Libraries%;com.alcinoe:alcinoe-datepicker:1.0.0
set Libraries=%Libraries%;com.alcinoe:alcinoe-common:1.0.1
set Libraries=%Libraries%;com.google.android.exoplayer:exoplayer-core:2.18.2
set Libraries=%Libraries%;com.google.android.exoplayer:exoplayer-hls:2.18.2

"%ALBaseDir%\..\..\..\..\Tools\AndroidMerger\AndroidMerger.exe"^
 -LocalMavenRepositoryDir="%ALBaseDir%\..\..\..\..\Libraries\jar\"^
 -Libraries="%Libraries%"^
 -OutputDir="%ALBaseDir%\Merged"^
 -DProj="%ALBaseDir%\..\ALFmxControls.dproj"^
 -AndroidManifest="%ALBaseDir%\..\AndroidManifest.template.xml"^
 -DProjNormalizer="%ALBaseDir%\..\..\..\..\Tools\DProjNormalizer\DProjNormalizer.exe"^
 -RJarSwapper="%ALBaseDir%\..\..\..\..\Tools\RJarSwapper\RJarSwapper.bat"^
 -UseGradle=true