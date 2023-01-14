AndroidMerger: Integrate AAR SDK in FMX Android app
===================================================

<img src="https://github.com/MagicFoundation/Alcinoe/blob/master/References/DocImages/img-26.jpg?raw=true" alt="video player for FireMonkey" width="540" style="width:540px;"/>
                                 
An Android library, also called as Android Archive, includes 
everything you need to build an app like source files, 
resource files, manifest etc. This is the reason why AARs are 
different from JARs. AARs can contain resource files as well 
other than compiled byte code. 

Adding such library to Delphi project is long and convoluted 
process consisting of extracting resources from library, 
manually adding them to Delphi deployment files, compiling 
R.Java class, checking dependancies, etc.

With AndroidMerger all of the above can now be done 
automatically in a single command line. In brief 
AndroidMerger will:
 
* Use graddle or internal implementation to list all dependencies.
* Download libraries and dependancies from local or central maven repository.
* Merge the resources of all AARs inside a single directory.
* Merge the AndroidManifest files of all AARs inside AndroidManifest.template.xml.
* Merge google-services.json in the resources of the project.
* Create the R.jar with all resource IDs using aapt or aapt2.
* Update the project file (.dproj) to include all resources.
* Generate the Delphi native bridge file from the Java libraries.
   
Merge the AndroidManifest file of all AARs
------------------------------------------

All AndroidManifest files will be merged inside AndroidManifest.template.xml.
Auto-generated sections like below will be added to the 
AndroidManifest.template.xml:

```
  <!-- ============================================ -->
  <!-- AndroidMerger auto-generated section (BEGIN) -->
  <!-- ============================================ -->
  
  ...
  
  <!-- ========================================== -->
  <!-- AndroidMerger auto-generated section (END) -->
  <!-- ========================================== --> 
```

Everytime you will launch AndroidMerger, those sections will be recreated.
If you want to disable an element inside those auto-generated sections
and make this change persistent to AndroidMerger like for example:

```
  <provider android:name="com.facebook.internal.FacebookInitProvider" />
```

then just comment the element by adding "!!" at the begin and the end 
of the comment node: 

```
  <!-- !! <provider android:name="com.facebook.internal.FacebookInitProvider" /> !! -->
```
   
Usage
-----

```
  AndroidMerger.exe
    -Libraries: Paths to libraries (aar, jar, directory, maven name). Separate paths with ';'.
    -DownloadDependencies: Download dependencies from local or central maven repository. Default true
    -LocalMavenRepositoryDir: Directory on the local machine where to stores all artifacts.
    -OutputDir: Path where all libraries will be merged.
    -DProj: Path to the project file (*.dproj).
    -AndroidManifest: Path to the AndroidManifest.template.xml of the project.
    -Configurations: Default Debug;Release. Separate Configurations with ';'.
    -Platforms: Default Android;Android64. Separate Platforms with ';'.
    -GoogleServicesJson: Path to the google-services.json
    -DProjNormalizer: Path to the Alcinoe DProjNormalizer tool.
    -RJarSwapper: Path to the Alcinoe RJarSwapper tool.
    -UseGradle: Use Gradle build tool to retrieve the dependencies. default false
    -GenerateNativeBridgeFile: Generate in OutputDir the Delphi native bridge file from the Java libraries.
    -NoInteraction: Non-interactive mode.
```

Example
-------

```
  AndroidMerger.exe^
    -Libraries=com.facebook.android:facebook-android-sdk:15.1.0;c:\MyLibs\af-android.aar;c:\MyProject\Android\ToMerge^
    -LocalMavenRepositoryDir=c:\LocalMavenRepository\^
    -OutputDir=c:\MyProject\Android\Merged^
    -DProj=c:\MyProject\MyProject.dproj^
    -AndroidManifest=c:\MyProject\AndroidManifest.template.xml^
    -DProjNormalizer=c:\Alcinoe\Tools\DeployProjNormalizer\DeployProjNormalizer.exe^
    -RJarSwapper=c:\Alcinoe\Tools\RJarSwapper\RJarSwapper.bat
    -UseGradle=true
```
 
You can also check the ALFacebookLogin Demo and in particular the script:
[{Alcinoe}\Demos\ALFacebookLogin\_source\android\MergeLibraries.bat](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFacebookLogin/_source/android/MergeLibraries.bat)

Note
----

You cannot add the resources below via the project option. 

  * Android_Strings
  * Android_Colors
  * AndroidSplashImageDef
  * AndroidSplashStylesV21
  * AndroidSplashStyles
  * Android_LauncherIcon36
  * Android_LauncherIcon48
  * Android_LauncherIcon72
  * Android_LauncherIcon96
  * Android_LauncherIcon144
  * Android_LauncherIcon192
  * Android_SplashImage426
  * Android_SplashImage470
  * Android_SplashImage640
  * Android_SplashImage960
  * Android_NotificationIcon24
  * Android_NotificationIcon36
  * Android_NotificationIcon48
  * Android_NotificationIcon72
  * Android_NotificationIcon96
  
Instead you must create a directory with this structure:

```
  <mydir>
    res
      drawable-hdpi
      drawable-mdpi
      drawable-xhdpi
      drawable-xxhdpi
      drawable-xxxhdpi
      ...
```

and put in it all the needed resources. for exemple in drawable-hdpi you 
can add the file $(BDS)\bin\Artwork\Android\FM_LauncherIcon_36x36.png that 
you renamed in ic_launcher.png, in drawable-hdpi the file 
$(BDS)\bin\Artwork\Android\FM_LauncherIcon_48x48.png that you renamed in 
ic_launcher.png, etc. Then include this dir in the -libraries path like :

```
AndroidMerger.exe -Libraries="androidx.appcompat:appcompat:1.5.1;<mydir>" ...
```

You can look the [{Alcinoe}\Demos\ALFirebaseMessaging](https://github.com/MagicFoundation/Alcinoe/tree/master/Demos/ALFirebaseMessaging) as example