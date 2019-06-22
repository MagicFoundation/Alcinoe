download the framework from 
https://cocoapods.org/pods/GoogleWebRTC (ex: https://dl.google.com/dl/cpdc/0d72a13625d8f3e5/GoogleWebRTC-1.1.25331.tar.gz)

*************************

The framework must be signed with the same team identifier in the main executable code signature.
The team identifier is the 10-character alphanumeric string, such as YH9SZ5LKR4, associated with your developer account, 
and recorded in your Apple-issued signing certificate.

You must do this manually still embarcadero implement it
https://quality.embarcadero.com/browse/RSP-22899

codesign -s <Identity> /YourPathto/WebRTC.framework/WebRTC

After you need to deploy yourself the WebRTC.framework in the .\Frameworks\WebRTC.framework
file to deploy: 
  Info.plist
  LICENSE.md
  WebRTC
  _CodeSignature\CodeResources
  
  
Stripping Unwanted Architectures From WebRTC
http://ikennd.ac/blog/2015/02/stripping-unwanted-architectures-from-dynamic-libraries-in-xcode/

lipo -info /YourPathto/WebRTC.framework/WebRTC
lipo -remove i386 /YourPathto/WebRTC.framework/WebRTC -o /YourPathto/WebRTC.framework/WebRTC 
lipo -remove x86_64 /YourPathto/WebRTC.framework/WebRTC -o /YourPathto/WebRTC.framework/WebRTC
lipo -info /YourPathto/WebRTC.framework/WebRTC