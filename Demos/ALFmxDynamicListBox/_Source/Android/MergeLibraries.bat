@echo off
SETLOCAL

Set NoInteraction=true
if "%ALBaseDir%"=="" (
  Set NoInteraction=false
  call "%~dp0\..\..\..\..\InitEnvironment.bat" >nul
  IF ERRORLEVEL 1 goto ERROR  
)

set Libraries=%ALBaseDir%\Demos\ALFmxDynamicListBox\_Source\Android\App\
set Libraries=%Libraries%;androidx.media3:media3-exoplayer:1.5.1
set Libraries=%Libraries%;androidx.media3:media3-exoplayer-hls:1.5.1

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
set Libraries=%Libraries%;androidx.concurrent:concurrent-futures:1.2.0

call "%ALBaseDir%\Tools\AndroidMerger\AndroidMerger.exe"^
 -LocalMavenRepositoryDir="%ALBaseDir%\Libraries\jar\"^
 -Libraries="%Libraries%"^
 -OutputDir="%ALBaseDir%\Demos\ALFmxDynamicListBox\_Source\Android\Merged"^
 -DProj="%ALBaseDir%\Demos\ALFmxDynamicListBox\_Source\ALFmxDynamicListBoxDemo.dproj"^
 -AndroidManifest="%ALBaseDir%\Demos\ALFmxDynamicListBox\_Source\AndroidManifest.template.xml"^
 -DProjNormalizer="%ALBaseDir%\Tools\DProjNormalizer\DProjNormalizer.exe"^
 -Configurations="Debug;Debug_Skia;Debug_Skia_Vulkan;Debug_Skia_Metal;Debug_ALSkiaEngine;Debug_ALSkiaEngine_Vulkan;Debug_ALSkiaEngine_Metal;Debug_Metal;Debug_OpenGL;Release"^
 -NoInteraction=%NoInteraction%
IF ERRORLEVEL 1 goto ERROR 

goto FINISHED 


:ERROR

pause
EXIT /B 1

:FINISHED