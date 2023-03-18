//
// Made from Facebook SDK version 15.1.0
//
unit Alcinoe.iOSApi.FacebookLoginKit;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported}
  //Pleast update <Alcinoe>\Libraries\ios\facebook\ to the last one and then run
  //<Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorIOS.bat
  //and gave the path to <Alcinoe>\Source\Alcinoe.iOSApi.FacebookLoginKit.pas to build
  //the compare source file. Then make a diff compare between the new generated
  //Alcinoe.iOSApi.FacebookLoginKit.pas and this one to see if the api signature is
  //still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of facebook LoginKit (ios) is still the same'}
{$ENDIF}

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.UIKit,
  Alcinoe.iOSApi.FacebookCoreKit; // [MANDATORY] Because we need it's initialization/finalization section

{$M+}

type

  {**********************************************************************************************}
  //https://developers.facebook.com/docs/reference/ios/current/class/FBSDKLoginManagerLoginResult/
  //@interface FBSDKLoginManagerLoginResult : NSObject
  FBSDKLoginManagerLoginResultClass = interface(NSObjectClass)
    ['{EFF1C859-BDE8-4C2F-8975-6048A9BC6688}']
  end;
  FBSDKLoginManagerLoginResult = interface(NSObject)
    ['{8566BC06-D2A5-4554-8F97-E225B19B61C3}']
    function token : FBSDKAccessToken; cdecl;
    function isCancelled : Boolean; cdecl;
  end;
  TFBSDKLoginManagerLoginResult = class(TOCGenericImport<FBSDKLoginManagerLoginResultClass, FBSDKLoginManagerLoginResult>) end;

  {************************************************************************************************************}
  FBSDKLoginManagerLoginResultBlock = procedure(result: FBSDKLoginManagerLoginResult; error: NSError) of object;

  {***********************************************************************************}
  //https://developers.facebook.com/docs/reference/ios/current/class/FBSDKLoginManager/
  //@interface FBSDKLoginManager : NSObject
  FBSDKLoginManagerClass = interface(NSObjectClass)
    ['{EE432533-246C-4246-AF3D-B1E19D69E0A1}']
  end;
  FBSDKLoginManager = interface(NSObject)
    ['{0FE53A8C-6B7D-498A-AA0B-0A78AC2FA15B}']
    procedure logInWithPermissions(permissions: NSArray; fromViewController: UIViewController; handler: FBSDKLoginManagerLoginResultBlock); cdecl;
    procedure logOut; cdecl;
  end;
  TFBSDKLoginManager = class(TOCGenericImport<FBSDKLoginManagerClass, FBSDKLoginManager>) end;

implementation

{***********************************************************************}
procedure FBSDKLoginKitLoader; cdecl; external framework 'FBSDKLoginKit';

end.
