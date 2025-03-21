//
// Made from Facebook SDK version 15.1.0
//
unit Alcinoe.iOSApi.FacebookCoreKit;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported123}
  //Pleast update <Alcinoe>\Libraries\ios\facebook\ to the last one and then run
  //<Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorIOS.bat
  //and gave the path to <Alcinoe>\Source\Alcinoe.iOSApi.FacebookCoreKit.pas to build
  //the compare source file. Then make a diff compare between the new generated
  //Alcinoe.iOSApi.FacebookCoreKit.pas and this one to see if the api signature is
  //still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of facebook CoreKit (ios) is still the same'}
{$ENDIF}

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.UIKit;

{$M+}

type

  {***************************}
  FBSDKAccessToken = interface;

  {**********************************************************************************}
  //https://developers.facebook.com/docs/reference/ios/current/class/FBSDKAccessToken/
  //@interface FBSDKAccessToken : NSObject <NSCopying, NSObject, NSSecureCoding, FBSDKAccessTokenProviding, FBSDKTokenStringProviding>
  FBSDKAccessTokenClass = interface(NSObjectClass)
    ['{4D123544-3287-472A-95B2-AF69E30C2738}']
    {class} procedure setCurrentAccessToken(currentAccessToken: FBSDKAccessToken); cdecl;
    {class} function currentAccessToken: FBSDKAccessToken; cdecl;
  end;
  FBSDKAccessToken = interface(NSObject)
    ['{BFA7F5C8-3718-45D0-8397-045512EC8730}']
    function declinedPermissions : NSSet; cdecl;
    function permissions : NSSet; cdecl;
    function tokenString : NSString; cdecl;
    function userID : NSString; cdecl;
  end;
  TFBSDKAccessToken = class(TOCGenericImport<FBSDKAccessTokenClass, FBSDKAccessToken>) end;

  {***********************************}
  FBSDKApplicationDelegate = interface;

  {******************************************************************************************}
  //https://developers.facebook.com/docs/reference/ios/current/class/FBSDKApplicationDelegate/
  //@interface FBSDKApplicationDelegate : NSObject
  FBSDKApplicationDelegateClass = interface(NSObjectClass)
    ['{B4050326-F748-4FC6-9018-734AEADEE9ED}']
    {class} function sharedInstance: FBSDKApplicationDelegate; cdecl;
  end;
  FBSDKApplicationDelegate = interface(NSObject)
    ['{FA8D976C-3BC9-4152-A053-CD08C919210D}']
    procedure initializeSDK; cdecl;
    [MethodName('application:openURL:options:')]
    function applicationOpenURLOptions(application: UIApplication; openURL: NSURL; options: NSDictionary) : Boolean; cdecl;
    [MethodName('application:didFinishLaunchingWithOptions:')]
    function applicationDidFinishLaunchingWithOptions(application: UIApplication; didFinishLaunchingWithOptions: NSDictionary) : Boolean; cdecl;
  end;
  TFBSDKApplicationDelegate = class(TOCGenericImport<FBSDKApplicationDelegateClass, FBSDKApplicationDelegate>) end;

  {*************************************}
  //@protocol FBSDKGraphRequestConnecting
  FBSDKGraphRequestConnecting = interface(IObjectiveC)
  ['{21964E12-BDE3-4910-8DC9-31E41B987519}']
  end;

  {**************************************************************************************************************************}
  FBSDKGraphRequestCompletion = procedure(connection: FBSDKGraphRequestConnecting; result: pointer; error: NSError) of object;

  {*************************}
  FBSDKHTTPMethod = NSString;

  {***********************************************************************************}
  //https://developers.facebook.com/docs/reference/ios/current/class/FBSDKGraphRequest/
  //@interface FBSDKGraphRequest : NSObject <FBSDKGraphRequest>
  FBSDKGraphRequestClass = interface(NSObjectClass)
    ['{1233C916-F3DA-45F0-8F05-F702A42C2BBE}']
  end;
  FBSDKGraphRequest = interface(NSObject)
    ['{C964E2C1-1500-4A35-B61D-02F46AF53B3C}']
    [MethodName('initWithGraphPath:parameters:HTTPMethod:')]
    function initWithGraphPathParametersHTTPMethod(graphPath: NSString; parameters: NSDictionary; HTTPMethod: FBSDKHTTPMethod) : FBSDKGraphRequest; cdecl;
    function startWithCompletion(completion: FBSDKGraphRequestCompletion) : FBSDKGraphRequestConnecting; cdecl;
  end;
  TFBSDKGraphRequest = class(TOCGenericImport<FBSDKGraphRequestClass, FBSDKGraphRequest>) end;

implementation

{*************************}
{$IF Defined(IOSSIMULATOR)}
procedure libclangrtiosLoader; cdecl; external '/usr/lib/clang/lib/darwin/libclang_rt.iossim.a';
{$ELSE}
procedure libclangrtiosLoader; cdecl; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
{$ENDIF}
procedure libcLoader; cdecl; external '/usr/lib/libc++.dylib';
procedure libvDSPLoader; cdecl; external '/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/libvDSP.dylib';
procedure libvMiscLoader; cdecl; external '/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/libvMisc.dylib';

{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-38700 is corrected and if yes check if all declarations below are still mandatories.'}
{$ENDIF}
{$IF Defined(IOSSIMULATOR)}
procedure libswiftCompatibility50Loader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibility50.a';
procedure libswiftCompatibility51Loader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibility51.a';
procedure libswiftCompatibilityConcurrencyLoader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibilityConcurrency.a';
procedure libswiftCompatibilityDynamicReplacementsLoader; cdecl; external '/usr/lib/swift/iphonesimulator/libswiftCompatibilityDynamicReplacements.a';
{$ELSE}
procedure libswiftCompatibility50Loader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibility50.a';
procedure libswiftCompatibility51Loader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibility51.a';
procedure libswiftCompatibilityConcurrencyLoader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibilityConcurrency.a';
procedure libswiftCompatibilityDynamicReplacementsLoader; cdecl; external '/usr/lib/swift/iphoneos/libswiftCompatibilityDynamicReplacements.a';
{$ENDIF}
procedure libswiftCoreFoundationLoader; cdecl; external '/usr/lib/swift/libswiftCoreFoundation.dylib';
procedure libswiftCoreImageLoader; cdecl; external '/usr/lib/swift/libswiftCoreImage.dylib';
procedure libswiftCoreLoader; cdecl; external '/usr/lib/swift/libswiftCore.dylib';
procedure libswiftDarwinLoader; cdecl; external '/usr/lib/swift/libswiftDarwin.dylib';
procedure libswiftDataDetectionLoader; cdecl; external '/usr/lib/swift/libswiftDataDetection.dylib';
procedure libswiftDispatchLoader; cdecl; external '/usr/lib/swift/libswiftDispatch.dylib';
procedure libswiftFileProviderLoader; cdecl; external '/usr/lib/swift/libswiftFileProvider.dylib';
procedure libswiftMetalLoader; cdecl; external '/usr/lib/swift/libswiftMetal.dylib';
procedure libswiftObjectiveCLoader; cdecl; external '/usr/lib/swift/libswiftObjectiveC.dylib';
procedure libswiftQuartzCoreLoader; cdecl; external '/usr/lib/swift/libswiftQuartzCore.dylib';
procedure libswiftUIKitLoader; cdecl; external '/usr/lib/swift/libswiftUIKit.dylib';
procedure libswiftCoreAudioLoader; cdecl; external '/usr/lib/swift/libswiftCoreAudio.dylib';
procedure libswiftosLoader; cdecl; external '/usr/lib/swift/libswiftos.dylib';
procedure libswiftWebKitLoader; cdecl; external '/usr/lib/swift/libswiftWebKit.dylib';
procedure libswiftCoreMIDILoader; cdecl; external '/usr/lib/swift/libswiftCoreMIDI.dylib';
procedure libswiftPhotosLoader; cdecl; external '/usr/lib/swift/libswiftPhotos.dylib';
procedure libswiftAVFoundationLoader; cdecl; external '/usr/lib/swift/libswiftAVFoundation.dylib';
procedure libswiftCoreMediaLoader; cdecl; external '/usr/lib/swift/libswiftCoreMedia.dylib';
procedure libswiftsimdLoader; cdecl; external '/usr/lib/swift/libswiftsimd.dylib';
procedure libswiftUniformTypeIdentifiersLoader; cdecl; external '/usr/lib/swift/libswiftUniformTypeIdentifiers.dylib';
procedure libswiftCoreLocationLoader; cdecl; external '/usr/lib/swift/libswiftCoreLocation.dylib';
procedure libswiftCoreGraphicsLoader; cdecl; external '/usr/lib/swift/libswiftCoreGraphics.dylib';

end.
