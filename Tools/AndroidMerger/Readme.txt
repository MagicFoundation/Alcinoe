AndroidMerger: Integrate AAR SDK in FMX Android app
===================================================
                                 
An Android library, also called as Android Archive, includes 
everything you need to build an app like source files, 
resource files, manifest etc. This is the reason why AARs are 
different from JARs. AARs can contain resource files as well 
other than compiled byte code. 

Adding such library to Delphi project is long and convoluted 
process consisting of extracting resources from library, 
manually adding them to Delphi deployment files, compiling 
R.Java class, creating library jar files...

With AndroidMerger all of the above can now be done 
automatically. With AndroidMerger you will:
 
  * Merge the resources of all AARs inside a single 
    directory.
  * Merge the AndroidManifest file of all AARs inside 
    AndroidManifest.template.xml.
  * Create the R.jar with all resource IDs using aapt 
    or aapt2.
  * Update the project file (*.dproj).   
   
Usage
-----

AndroidMerger.exe -Libraries=<arg> -ResOutputDir=<arg> -RJarOutputDir=<arg> -AndroidManifest=<arg>

  -Libraries: Paths to aar libraries or directories or dproj. Separate paths with ';'.
              If you specify a dproj then aar files must be in the same directory of the jars.
  -ResOutputDir: Path where all resources will be merged. !Warning! The directory will be cleared.
  -RJarOutputDir: Path where to place the r.jar, r-aab.jar and r-apk.jar.
  -AndroidManifest: Path to the AndroidManifest.template.xml of the project.
  -NoInteraction: Non-interactive mode.

Example
--------

AndroidMerger.exe^
 -Libraries=c:\mylibs\facebook-share-5.15.1.aar;c:\mylibs\facebook-login-5.15.1.aar^
 -ResOutputDir=c:\myproject\res\^
 -RJarOutputDir=c:\myproject\jars\^
 -AndroidManifest=c:\myproject\AndroidManifest.template.xml
 
You can find a working example of how to use AndroidMerger
in <Alcinoe>\Demos\ALFirebaseMessaging