@echo off

set ALBaseDir=%~dp0

"%ALBaseDir%\..\..\..\..\Tools\AndroidMerger\AndroidMerger.exe"^
 -LocalMavenRepositoryDir="%ALBaseDir%\..\..\..\..\Libraries\jar\"^
 -Libraries="com.alcinoe:alcinoe-firebase-messaging:1.0.0;%ALBaseDir%\App\"^
 -OutputDir="%ALBaseDir%\Merged"^
 -DProj="%ALBaseDir%\..\ALFirebaseMessaging.dproj"^
 -AndroidManifest="%ALBaseDir%\..\AndroidManifest.template.xml"^
 -GoogleServicesJson="%ALBaseDir%\GoogleServices\google-services.json"^
 -DProjNormalizer="%ALBaseDir%\..\..\..\..\Tools\DProjNormalizer\DProjNormalizer.exe"^
 -RJarSwapper="%ALBaseDir%\..\..\..\..\Tools\RJarSwapper\RJarSwapper.bat"^
 -UseGradle=true