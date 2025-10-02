download the framework from 
https://cocoapods.org/pods/GoogleWebRTC

Current version
GoogleWebRTC-1.1.25331
https://dl.google.com/dl/cpdc/0d72a13625d8f3e5/GoogleWebRTC-1.1.25331.tar.gz

*************************

The framework is a dynamyc library so you need to deploy yourself 
the WebRTC.framework in the .\Frameworks\WebRTC.framework
file to deploy: 
  Info.plist
  WebRTC

You must also set this in your Project Options :
Building > Delphi Compiler > Linking > Options passed to the LD linker : -rpath @executable_path/Frameworks

and as in ALIosWebRTCApi we declare stuff like
function RTCCleanupSSL: Boolean; cdecl; external 'WebRTC' name _PU + 'RTCCleanupSSL';
you must also add in the search path the path where is located on you local machine the dynamyc library

*************************

Stripping Unwanted Architectures From WebRTC
http://ikennd.ac/blog/2015/02/stripping-unwanted-architectures-from-dynamic-libraries-in-xcode/

lipo -info /Users/zeus/Documents/WebRTC.framework/WebRTC
lipo -remove i386 /Users/zeus/Documents/WebRTC.framework/WebRTC -o /Users/zeus/Documents/WebRTC.framework/WebRTC
lipo -remove x86_64 /Users/zeus/Documents/WebRTC.framework/WebRTC -o /Users/zeus/Documents/WebRTC.framework/WebRTC
lipo -remove armv7 /Users/zeus/Documents/WebRTC.framework/WebRTC -o /Users/zeus/Documents/WebRTC.framework/WebRTC
lipo -info /Users/zeus/Documents/WebRTC.framework/WebRTC