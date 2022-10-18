RJarSwapper
===========

When we build an aab package we use Aapt and when we build an 
apk package we use Aapt2. Aapt and Aapt2 make different 
resource IDs which are incompatible. so in AndroidResourcesMerger.exe 
we make 3 jars: r.jar, r-aab.jar (with Aapt2) and r-apk.jar (With Aapt) 
and we include in the project only r.jar but in the Pre-Build 
event option of the project with RJarSwapper.bat command line 
we swap r-aab.jar/r-apk.jar to r.jar dependly if we are building 
an aab package or an apk package.
                      
Add this Pre-Build event to the project option (android platform) :
<Alcinoe>\Tools\RJarSwapper\RJarSwapper.bat 
  -RJarDir="<directory where r-apk.jar and r-aab.jar are located>" 
  -IsAabPackage="true if you build an .aab package, false if you build an .apk"

exemple:
  <Alcinoe>\Tools\RJarSwapper\RJarSwapper.bat -RJarDir="c:\myproject\android\librairies\r.java\" -IsAabPackage="true"

You can find a working example of how to use RJarSwapper
in <Alcinoe>\Demos\ALFirebaseMessaging