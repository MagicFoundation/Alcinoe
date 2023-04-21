@echo off

set ALBaseDir=%~dp0

"%ALBaseDir%\..\..\..\..\Tools\AndroidMerger\AndroidMerger.exe"^
 -LocalMavenRepositoryDir="%ALBaseDir%\..\..\..\..\Libraries\jar\"^
 -Libraries="com.google.android.gms:play-services-base:18.2.0;com.google.android.gms:play-services-location:21.0.1;%ALBaseDir%\App\"^
 -OutputDir="%ALBaseDir%\Merged"^
 -DProj="%ALBaseDir%\..\ALGeoPositionSensorDemo.dproj"^
 -AndroidManifest="%ALBaseDir%\..\AndroidManifest.template.xml"^
 -DProjNormalizer="%ALBaseDir%\..\..\..\..\Tools\DProjNormalizer\DProjNormalizer.exe"^
 -RJarSwapper="%ALBaseDir%\..\..\..\..\Tools\RJarSwapper\RJarSwapper.bat"^
 -UseGradle=true