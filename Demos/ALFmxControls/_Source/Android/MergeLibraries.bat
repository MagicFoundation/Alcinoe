@echo off

set ALBaseDir=%~dp0

set Libraries=%ALBaseDir%\App\
set Libraries=%Libraries%;com.alcinoe:alcinoe-edittext:1.0.0
set Libraries=%Libraries%;com.alcinoe:alcinoe-datepicker:1.0.0
set Libraries=%Libraries%;com.alcinoe:alcinoe-common:1.0.1
set Libraries=%Libraries%;com.google.android.exoplayer:exoplayer-core:2.19.1
set Libraries=%Libraries%;com.google.android.exoplayer:exoplayer-hls:2.19.1

REM Required by fmx.jar, else the app crash at startup with 
REM java.lang.NoClassDefFoundError: Failed resolution of: Landroidx/activity/result/contract/ActivityResultContracts$OpenDocument;
set Libraries=%Libraries%;androidx.activity:activity:1.8.1
REM Issue with androidx.activity:activity:1.8.1 transitive dependencies:
REM This library indirectly requires org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.6.21 due to one of its transitive dependencies.
REM Additionally, there is another transitive dependency that necessitates org.jetbrains.kotlin:kotlin-stdlib:1.8.22.
REM To resolve this, we are enforcing the use of org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.8.22.
set Libraries=%Libraries%;org.jetbrains.kotlin:kotlin-stdlib-jdk8:1.8.22
REM Required else the app crash few seconds after startup with 
REM java.lang.NoClassDefFoundError: Failed resolution of: Lcom/google/common/util/concurrent/ListenableFuture;
set Libraries=%Libraries%;com.google.guava:guava:32.1.3-android

"%ALBaseDir%\..\..\..\..\Tools\AndroidMerger\AndroidMerger.exe"^
 -LocalMavenRepositoryDir="%ALBaseDir%\..\..\..\..\Libraries\jar\"^
 -Libraries="%Libraries%"^
 -OutputDir="%ALBaseDir%\Merged"^
 -DProj="%ALBaseDir%\..\ALFmxControls.dproj"^
 -AndroidManifest="%ALBaseDir%\..\AndroidManifest.template.xml"^
 -DProjNormalizer="%ALBaseDir%\..\..\..\..\Tools\DProjNormalizer\DProjNormalizer.exe"^
 -UseGradle=true