unit ALIosFacebookApi;

interface

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  iOSapi.UIKit;

{$M+}

type

  {****************************************************}
  FBSDKSharingValidationClass = interface(NSObjectClass)
    ['{DF413176-F10E-47EC-AA97-780E17134C31}']
  end;
  FBSDKSharingValidation = interface(NSObject)
    ['{84615337-929D-4C31-A1FB-3690D3AA6ADC}']
  end;

  {***************************************************************}
  FBSDKSharingContentClass = interface(FBSDKSharingValidationClass)
    ['{9008EF53-C3B4-46C9-AA04-B82C846F779C}']
  end;
  FBSDKSharingContent = interface(FBSDKSharingValidation)
    ['{CF844104-87D4-440C-9E4B-D6AF0837AD3F}']
    function contentURL: NSURL; cdecl;
    procedure setContentURL(contentURL: NSURL); cdecl;
  end;

  {**************************************************************}
  FBSDKShareLinkContentClass = interface(FBSDKSharingContentClass)
    ['{D9BFD669-37D3-412A-9E7C-678F6F33CE93}']
  end;
  FBSDKShareLinkContent = interface(FBSDKSharingContent)
    ['{F7560939-39E5-435B-A4A8-D6521C8E646B}']
    function quote : NSString; cdecl;
    procedure setQuote(quote: NSString); cdecl;
    function isEqualToShareLinkContent(content: FBSDKShareLinkContent) : Boolean; cdecl;
  end;
  TFBSDKShareLinkContent = class(TOCGenericImport<FBSDKShareLinkContentClass, FBSDKShareLinkContent>) end;

  {******************************************}
  FBSDKSharingClass = interface(NSObjectClass)
    ['{3835CD8F-7649-4704-A9F3-C83F9BE02209}']
  end;
  FBSDKSharing = interface(NSObject)
    ['{B19665D8-B0CA-4E71-8D7D-F78436226F72}']
    function shareContent: FBSDKSharingContent; cdecl;
    procedure setShareContent(shareContent: FBSDKSharingContent); cdecl;
    function shouldFailOnDataError: boolean; cdecl;
    procedure setShouldFailOnDataError(shouldFailOnDataError: boolean); cdecl;
  end;

  {****************************************************}
  FBSDKSharingDialogClass = interface(FBSDKSharingClass)
    ['{CF17E43D-D280-468C-9732-54E39CD703D1}']
  end;
  FBSDKSharingDialog = interface(FBSDKSharing)
    ['{000A5A87-5204-4B32-BF78-3CF912582947}']
    function canShow: Boolean; cdecl;
    function show: Boolean; cdecl;
  end;

  {********************************************************}
  FBSDKShareDialogClass = interface(FBSDKSharingDialogClass)
    ['{68A676C8-8775-441F-8663-F211E464C766}']
  end;
  FBSDKShareDialog = interface(FBSDKSharingDialog)
    ['{162B3B9F-903A-4312-9B79-940E80492913}']
    procedure setFromViewController(fromViewController: UIViewController); cdecl;
    function fromViewController : UIViewController; cdecl;
  end;
  TFBSDKShareDialog = class(TOCGenericImport<FBSDKShareDialogClass, FBSDKShareDialog>) end;

  {***************************}
  FBSDKAccessToken = interface;
  FBSDKAccessTokenClass = interface(NSObjectClass)
    ['{4D123544-3287-472A-95B2-AF69E30C2738}']
    {class} procedure setCurrentAccessToken(currentAccessToken: FBSDKAccessToken); cdecl;
    {class} function currentAccessToken: FBSDKAccessToken; cdecl;
  end;
  FBSDKAccessToken = interface(NSObject)
    ['{BFA7F5C8-3718-45D0-8397-045512EC8730}']
    function appID : NSString; cdecl;
    function declinedPermissions : NSSet; cdecl;
    function expirationDate : NSDate; cdecl;
    function permissions : NSSet; cdecl;
    function refreshDate : NSDate; cdecl;
    function tokenString : NSString; cdecl;
    function userID : NSString; cdecl;
    function hasGranted(permission: NSString) : Boolean; cdecl;
    function isEqualToAccessToken(token: FBSDKAccessToken) : Boolean; cdecl;
  end;
  TFBSDKAccessToken = class(TOCGenericImport<FBSDKAccessTokenClass, FBSDKAccessToken>) end;

  {**********************************************************}
  FBSDKLoginManagerLoginResultClass = interface(NSObjectClass)
    ['{EFF1C859-BDE8-4C2F-8975-6048A9BC6688}']
  end;
  FBSDKLoginManagerLoginResult = interface(NSObject)
    ['{8566BC06-D2A5-4554-8F97-E225B19B61C3}']
    procedure setToken(token: FBSDKAccessToken); cdecl;
    function token : FBSDKAccessToken; cdecl;
    function isCancelled : Boolean; cdecl;
    procedure setGrantedPermissions(grantedPermissions: NSSet); cdecl;
    function grantedPermissions : NSSet; cdecl;
    procedure setDeclinedPermissions(declinedPermissions: NSSet); cdecl;
    function declinedPermissions : NSSet; cdecl;
    function initWithToken(token: FBSDKAccessToken; isCancelled: Boolean; grantedPermissions: NSSet; declinedPermissions: NSSet) : Pointer {instancetype}; cdecl;
  end;
  TFBSDKLoginManagerLoginResult = class(TOCGenericImport<FBSDKLoginManagerLoginResultClass, FBSDKLoginManagerLoginResult>) end;

type

  {*******************************************************************************************************}
  LoginManagerLoginResultBlock = procedure(result: FBSDKLoginManagerLoginResult; error: NSError) of object;

Type

  {********************************}
  FBSDKDefaultAudience = NSUInteger;

const

  {******************************}
  FBSDKDefaultAudienceFriends = 0;
  FBSDKDefaultAudienceOnlyMe = 1;
  FBSDKDefaultAudienceEveryone = 2;

type

  {******************************}
  FBSDKLoginBehavior = NSUInteger;

const

  {***************************}
  FBSDKLoginBehaviorNative = 0;
  FBSDKLoginBehaviorBrowser = 1;
  FBSDKLoginBehaviorSystemAccount = 2;
  FBSDKLoginBehaviorWeb = 3;

type

  {***********************************************}
  FBSDKLoginManagerClass = interface(NSObjectClass)
    ['{EE432533-246C-4246-AF3D-B1E19D69E0A1}']
  end;
  FBSDKLoginManager = interface(NSObject)
    ['{0FE53A8C-6B7D-498A-AA0B-0A78AC2FA15B}']
    procedure setDefaultAudience(defaultAudience: FBSDKDefaultAudience); cdecl;
    function defaultAudience : FBSDKDefaultAudience; cdecl;
    procedure setLoginBehavior(loginBehavior: FBSDKLoginBehavior); cdecl;
    function loginBehavior : FBSDKLoginBehavior; cdecl;
    procedure logInWithPermissions(permissions: NSArray;
                                   fromViewController: UIViewController;
                                   handler: LoginManagerLoginResultBlock); cdecl;
    procedure logOut; cdecl;
  end;
  TFBSDKLoginManager = class(TOCGenericImport<FBSDKLoginManagerClass, FBSDKLoginManager>) end;

  {******************************************************}
  FBSDKApplicationDelegateClass = interface(NSObjectClass)
    ['{B4050326-F748-4FC6-9018-734AEADEE9ED}']
    {class} function sharedInstance: Pointer{instancetype}; cdecl;
    {class} procedure initializeSDK(launchOptions: NSDictionary); cdecl;
  end;
  FBSDKApplicationDelegate = interface(NSObject)
    ['{FA8D976C-3BC9-4152-A053-CD08C919210D}']
    [MethodName('application:openURL:sourceApplication:annotation:')]
    function applicationOpenURLSourceApplicationAnnotation(application: UIApplication; openURL: NSURL; sourceApplication: NSString; annotation: Pointer) : Boolean; cdecl;
    [MethodName('application:openURL:options:')]
    function applicationOpenURLOptions(application: UIApplication; openURL: NSURL; options: NSDictionary) : Boolean; cdecl;
    [MethodName('application:didFinishLaunchingWithOptions:')]
    function applicationDidFinishLaunchingWithOptions(application: UIApplication; didFinishLaunchingWithOptions: NSDictionary) : Boolean; cdecl;
  end;
  TFBSDKApplicationDelegate = class(TOCGenericImport<FBSDKApplicationDelegateClass, FBSDKApplicationDelegate>) end;

  {**************************************}
  FBSDKGraphRequestConnection = interface;
  FBSDKGraphRequest = interface;

  {*********************************************************************************************************************}
  FBSDKGraphRequestBlock = procedure(connection: FBSDKGraphRequestConnection; result: Pointer; error: NSError) of object;

  {*********************************************************}
  FBSDKGraphRequestConnectionClass = interface(NSObjectClass)
    ['{9E03FA98-40DB-48B8-BE1F-B2554F3C6C0E}']
  end;
  FBSDKGraphRequestConnection = interface(NSObject)
    ['{C59C5865-7AC4-47C4-8D66-28BCF1D81FEB}']
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate : Pointer; cdecl;
    procedure setTimeout(timeout: NSTimeInterval); cdecl;
    function timeout : NSTimeInterval; cdecl;
    function urlResponse : NSHTTPURLResponse; cdecl;
    procedure cancel; cdecl;
    procedure start; cdecl;
  end;
  TFBSDKGraphRequestConnection = class(TOCGenericImport<FBSDKGraphRequestConnectionClass, FBSDKGraphRequestConnection>) end;

  {*************************}
  FBSDKHTTPMethod = NSString;

  {***********************************************}
  FBSDKGraphRequestClass = interface(NSObjectClass)
    ['{1233C916-F3DA-45F0-8F05-F702A42C2BBE}']
  end;
  FBSDKGraphRequest = interface(NSObject)
    ['{C964E2C1-1500-4A35-B61D-02F46AF53B3C}']
    [MethodName('initWithGraphPath:')]
    function initWithGraphPath(graphPath: NSString) : Pointer {instancetype}; cdecl;
    [MethodName('initWithGraphPath:HTTPMethod:')]
    function initWithGraphPathHTTPMethod(graphPath: NSString; HTTPMethod: FBSDKHTTPMethod) : Pointer {instancetype}; cdecl;
    [MethodName('initWithGraphPath:parameters:')]
    function initWithGraphPathParameters(graphPath: NSString; parameters: NSDictionary) : Pointer {instancetype}; cdecl;
    [MethodName('initWithGraphPath:parameters:HTTPMethod:')]
    function initWithGraphPathParametersHTTPMethod(graphPath: NSString; parameters: NSDictionary; HTTPMethod: FBSDKHTTPMethod) : Pointer {instancetype}; cdecl;
    [MethodName('initWithGraphPath:parameters:tokenString:version:HTTPMethod:')]
    function initWithGraphPathParametersTokenStringVersionHTTPMethod(graphPath: NSString; parameters: NSDictionary; tokenString: NSString; version: NSString; HTTPMethod: FBSDKHTTPMethod) : Pointer {instancetype}; cdecl;
    procedure setParameters(parameters: NSDictionary); cdecl;
    function parameters: NSDictionary; cdecl;
    function tokenString: NSString; cdecl;
    function graphPath: NSString; cdecl;
    function HTTPMethod: FBSDKHTTPMethod; cdecl;
    function version : NSString; cdecl;
    procedure setGraphErrorRecoveryDisabled(disable: Boolean); cdecl;
    function startWithCompletionHandler(handler: FBSDKGraphRequestBlock) : FBSDKGraphRequestConnection; cdecl;
  end;
  TFBSDKGraphRequest = class(TOCGenericImport<FBSDKGraphRequestClass, FBSDKGraphRequest>) end;


implementation

{$IF defined(CPUARM)}

procedure StubProc1;  cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKSharingContent';
procedure StubProc2;  cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKShareLinkContent';
procedure StubProc3;  cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKSharing';
procedure StubProc4;  cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKSharingDialog';
procedure StubProc5;  cdecl; external 'FBSDKShareKit' name 'OBJC_CLASS_$_FBSDKShareDialog';
procedure StubProc6;  cdecl; external 'FBSDKCoreKit'  name 'OBJC_CLASS_$_FBSDKAccessToken';
procedure StubProc7;  cdecl; external 'FBSDKLoginKit' name 'OBJC_CLASS_$_FBSDKLoginManagerLoginResult';
procedure StubProc8;  cdecl; external 'FBSDKLoginKit' name 'OBJC_CLASS_$_FBSDKLoginManager';
procedure StubProc9;  cdecl; external 'FBSDKCoreKit'  name 'OBJC_CLASS_$_FBSDKApplicationDelegate';
procedure StubProc10; cdecl; external 'FBSDKCoreKit'  name 'OBJC_CLASS_$_FBSDKGraphRequestConnection';
procedure StubProc11; cdecl; external 'FBSDKCoreKit'  name 'OBJC_CLASS_$_FBSDKGraphRequest';
procedure StubProc12; cdecl; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a' name '__isOSVersionAtLeast'; // << else I have Error: "___isOSVersionAtLeast", referenced from: ...
procedure StubProc13; cdecl; external '/usr/lib/libc++.dylib' name '__ZNSt3__119__shared_weak_count14__release_weakEv'; // << else I have Error: "__ZNSt3__119__shared_weak_count14__release_weakEv", referenced from: ...
procedure StubProc14; cdecl; external '/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/libvDSP.dylib' name '_vDSP_vclip'; // << else I have Error: "_vDSP_vclip", referenced from: ...

{$ELSE}

// i don't know how to do under ios simulator :(

{$ENDIF}

end.
