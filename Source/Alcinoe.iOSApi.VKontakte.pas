//
// Made from VKontakte SDK version 1.6.3
//
unit Alcinoe.iOSApi.VKontakte;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported122}
  //Pleast update <Alcinoe>\Libraries\ios\vkontakte\ to the last one and then run
  //<Alcinoe>\Tools\NativeBridgeFileGenerator\NativeBridgeFileGeneratorIOS.bat
  //and gave the path to <Alcinoe>\Source\Alcinoe.iOSApi.VKontakte.pas to build
  //the compare source file. Then make a diff compare between the new generated
  //Alcinoe.iOSApi.VKontakte.pas and this one to see if the api signature is
  //still the same
  {$MESSAGE WARN 'Check if the api signature of the last version of VKontakte sdk (ios) is still the same'}
{$ENDIF}

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  iOSapi.UIKit;

{$M+}

type

  {************************}
  VKObjectClass = interface;
  VKError = interface;
  VKAuthorizationResult = interface;
  VKAccessToken = interface;
  VKSdkDelegate = interface;
  VKSdkUIDelegate = interface;
  VKSdk = interface;

  {******************************}
  //@interface VKObject : NSObject
  VKObjectClass = interface(NSObjectClass)
    ['{73A33031-F242-4DEA-ACDA-235D6F5D119A}']
  end;
  VKObject = interface(NSObject)
    ['{2B83B160-E4D9-49CD-B568-3E899154D7F8}']
  end;
  TVKObject = class(TOCGenericImport<VKObjectClass, VKObject>) end;

  {*****************************}
  //@interface VKError : VKObject
  VKErrorClass = interface(VKObjectClass)
  ['{B33FC424-445C-4F10-AFEA-23A82935E562}']
  end;
  VKError = interface(VKObject)
  ['{37FECAB7-19A6-4366-9046-69F04E676BCE}']
  end;
  TVKError = class(TOCGenericImport<VKErrorClass, VKError>)  end;

  {*******************************************}
  //@interface VKAuthorizationResult : VKObject
  VKAuthorizationResultClass = interface(VKObjectClass)
    ['{6ED5DDCA-39EF-43E2-AA51-B626DF082867}']
  end;
  VKAuthorizationResult = interface(VKObject)
    ['{9120AD24-BCA3-4651-A756-275A5D2E55AE}']
    function token : VKAccessToken; cdecl;
    function error : NSError; cdecl;
  end;
  TVKAuthorizationResult = class(TOCGenericImport<VKAuthorizationResultClass, VKAuthorizationResult>)  end;

  {**********************************************}
  //@interface VKAccessToken : VKObject <NSCoding>
  VKAccessTokenClass = interface(VKObjectClass)
  ['{363241C5-57C5-4ACE-8B6D-2AC3385504A2}']
  end;
  VKAccessToken = interface(VKObject)
  ['{CF2B08FD-A5BC-4CA2-8FD3-D34D7D7EACC1}']
    function accessToken : NSString; cdecl;
    function userId : NSString; cdecl;
    function email : NSString; cdecl;
  end;
  TVKAccessToken = class(TOCGenericImport<VKAccessTokenClass, VKAccessToken>)  end;

  {**********************************}
  //@protocol VKSdkDelegate <NSObject>
  VKSdkDelegate = interface(IObjectiveC)
  ['{B86838F7-433A-49BA-8D4D-E3E6698B3F3B}']
    procedure vkSdkAccessAuthorizationFinishedWithResult(result: VKAuthorizationResult); cdecl;
    procedure vkSdkUserAuthorizationFailed; cdecl;
    procedure vkSdkAuthorizationStateUpdatedWithResult(result: VKAuthorizationResult); cdecl;
    procedure vkSdkAccessTokenUpdated(newToken: VKAccessToken; oldToken: VKAccessToken); cdecl;
    procedure vkSdkTokenHasExpired(expiredToken: VKAccessToken); cdecl;
  end;

  {************************************}
  //@protocol VKSdkUIDelegate <NSObject>
  VKSdkUIDelegate = interface(IObjectiveC)
  ['{D2F9A031-4CFF-4EFF-8639-E4C1F4B49CF4}']
    procedure vkSdkShouldPresentViewController(controller: UIViewController); cdecl;
    procedure vkSdkNeedCaptchaEnter(captchaError: VKError); cdecl;
    procedure vkSdkWillDismissViewController(controller: UIViewController); cdecl;
    procedure vkSdkDidDismissViewController(controller: UIViewController); cdecl;
  end;

  {***************************}
  //@interface VKSdk : NSObject
  VKSdkClass = interface(NSObjectClass)
  ['{7412935A-53D4-403A-8C90-31A76A964B22}']
    {class} function instance : VKSdk; cdecl;
    [MethodName('initializeWithAppId:')]
    {class} function initializeWithAppId(appId: NSString) : VKSdk; cdecl;
    [MethodName('authorize:')]
    {class} procedure authorize(permissions: NSArray); cdecl;
    {class} function accessToken : VKAccessToken; cdecl;
    {class} function processOpenURL(passedUrl: NSURL; fromApplication: NSString) : Boolean; cdecl;
    {class} procedure forceLogout; cdecl;
  end;
  VKSdk = interface(NSObject)
  ['{E81B4F55-E1D1-4822-B61F-0D9A299F0899}']
    procedure setUiDelegate(uiDelegate: VKSdkUIDelegate); cdecl;
    function uiDelegate : VKSdkUIDelegate; cdecl;
    procedure registerDelegate(delegate: VKSdkDelegate); cdecl;
    procedure unregisterDelegate(delegate: VKSdkDelegate); cdecl;
  end;
  TVKSdk = class(TOCGenericImport<VKSdkClass, VKSdk>) end;

implementation

{*************************************************************************}
procedure VKSdkFrameworkLoader; cdecl; external framework 'VKSdkFramework';

end.
