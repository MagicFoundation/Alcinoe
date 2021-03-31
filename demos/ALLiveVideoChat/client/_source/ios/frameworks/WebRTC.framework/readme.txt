download the framework from 
https://cocoapods.org/pods/GoogleWebRTC (ex: https://dl.google.com/dl/cpdc/0d72a13625d8f3e5/GoogleWebRTC-1.1.25331.tar.gz)

*************************

The framework must be signed with the same team identifier in the main executable code signature.
The team identifier is the 10-character alphanumeric string, such as YH9SZ5LKR4, associated with your developer account, 
and recorded in your Apple-issued signing certificate.

You must do this manually still embarcadero implement it
https://quality.embarcadero.com/browse/RSP-22899

codesign -s <Identity> /YourPathto/WebRTC.framework/WebRTC
  
To find the identity, on the mac launch the PAServer, activate the option V (verbose) and
then deploy the app from delphi. In the PAServer console you will see the identity after the 
-s option, like for example:  

  "/usr/bin/codesign" --deep -s "<Identity>" -f ....

*************************

Stripping Unwanted Architectures From WebRTC
http://ikennd.ac/blog/2015/02/stripping-unwanted-architectures-from-dynamic-libraries-in-xcode/

lipo -info /YourPathto/WebRTC.framework/WebRTC
lipo -remove i386 /YourPathto/WebRTC.framework/WebRTC -o /YourPathto/WebRTC.framework/WebRTC 
lipo -remove x86_64 /YourPathto/WebRTC.framework/WebRTC -o /YourPathto/WebRTC.framework/WebRTC
lipo -info /YourPathto/WebRTC.framework/WebRTC