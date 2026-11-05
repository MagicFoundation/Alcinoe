unit Alcinoe.iOSApi.AuthenticationServices;

interface

{$I Alcinoe.inc}

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation;

{$M+}

type

  {********************************}
  ASUserDetectionStatus = NSInteger;

const

  {***********************************}
  ASUserDetectionStatusUnsupported = 0;
  ASUserDetectionStatusUnknown = 1;
  ASUserDetectionStatusLikelyReal = 2;

type

  {********************************************}
  ASAuthorizationControllerDelegate = interface;
  ASAuthorizationControllerPresentationContextProviding = interface;

  {*********************************************}
  ASAuthorizationClass = interface(NSObjectClass)
  ['{B3EE72FE-E71F-47B7-AF7B-C0D403FC3469}']
  end;
  ASAuthorization = interface(NSObject)
  ['{3E3F8886-0CA3-4C89-BFF6-CC33F355BBD4}']
    //function provider: ASAuthorizationProvider; cdecl;
    function credential: pointer; cdecl;
  end;
  TASAuthorization = class(TOCGenericImport<ASAuthorizationClass, ASAuthorization>) end;
  PASAuthorization = Pointer;

  {**********************************************************************************}
  //@interface ASAuthorizationAppleIDCredential : NSObject <ASAuthorizationCredential>
  ASAuthorizationAppleIDCredentialClass = interface(NSObjectClass)
    ['{76C8B027-86AB-49DB-B924-7129BF9E9375}']
  end;
  ASAuthorizationAppleIDCredential = interface(NSObject)
    ['{2255DB92-789C-430B-A4DE-46B309CA2812}']
    function user : NSString; cdecl;
    function state : NSString; cdecl;
    function authorizedScopes : NSArray; cdecl;
    function authorizationCode : NSData; cdecl;
    function identityToken : NSData; cdecl;
    function email : NSString; cdecl;
    function fullName : NSPersonNameComponents; cdecl;
    function realUserStatus : ASUserDetectionStatus; cdecl;
  end;
  TASAuthorizationAppleIDCredential = class(TOCGenericImport<ASAuthorizationAppleIDCredentialClass, ASAuthorizationAppleIDCredential>)  end;
  PASAuthorizationAppleIDCredential = Pointer;

  {****************************************************}
  ASAuthorizationRequestClass = interface(NSObjectClass)
    ['{5AD0FB7E-FF24-4865-B718-524C0CC7DDEF}']
  end;
  ASAuthorizationRequest = interface(NSObject)
    ['{A8F407B9-DC6F-45BA-82D7-CBF8D58A556F}']
    //function provider : Pointer; cdecl;
  end;
  TASAuthorizationRequest = class(TOCGenericImport<ASAuthorizationRequestClass, ASAuthorizationRequest>)  end;
  PASAuthorizationRequest = Pointer;

  {************************************************************************}
  ASAuthorizationOpenIDRequestClass = interface(ASAuthorizationRequestClass)
    ['{855A26FA-A5F7-410D-992E-716540F37697}']
  end;
  ASAuthorizationOpenIDRequest = interface(ASAuthorizationRequest)
    ['{81FDDC8E-C937-40E2-A776-0522190D5F90}']
    procedure setRequestedScopes(requestedScopes: NSArray); cdecl;
    function requestedScopes : NSArray; cdecl;
    //procedure setState(state: NSString); cdecl;
    //function state : NSString; cdecl;
    //procedure setNonce(nonce: NSString); cdecl;
    //function nonce : NSString; cdecl;
    //procedure setRequestedOperation(requestedOperation: ASAuthorizationOpenIDOperation); cdecl;
    //function requestedOperation : ASAuthorizationOpenIDOperation; cdecl;
  end;
  TASAuthorizationOpenIDRequest = class(TOCGenericImport<ASAuthorizationOpenIDRequestClass, ASAuthorizationOpenIDRequest>)  end;
  PASAuthorizationOpenIDRequest = Pointer;

  {*******************************************************************************}
  ASAuthorizationAppleIDRequestClass = interface(ASAuthorizationOpenIDRequestClass)
    ['{DD1BE600-3F35-45DB-B0ED-594C7FEC98C3}']
  end;
  ASAuthorizationAppleIDRequest = interface(ASAuthorizationOpenIDRequest)
    ['{06C37879-9A4E-4C4C-88A3-DCCB0CD69EA7}']
    //procedure setUser(user: NSString); cdecl;
    //function user : NSString; cdecl;
  end;
  TASAuthorizationAppleIDRequest = class(TOCGenericImport<ASAuthorizationAppleIDRequestClass, ASAuthorizationAppleIDRequest>)  end;
  PASAuthorizationAppleIDRequest = Pointer;

  {************************************************************}
  ASAuthorizationAppleIDProviderClass = interface(NSObjectClass)
    ['{037C5CCA-93C0-47BF-A664-3BBE288AD211}']
  end;
  ASAuthorizationAppleIDProvider = interface(NSObject)
    ['{CBEA2AEE-19AC-4C71-8904-618B6DD4F47F}']
    function createRequest: ASAuthorizationAppleIDRequest; cdecl;
    //procedure getCredentialStateForUserID(userID: NSString; completion: TAuthenticationServicesCompletion); cdecl;
  end;
  TASAuthorizationAppleIDProvider = class(TOCGenericImport<ASAuthorizationAppleIDProviderClass, ASAuthorizationAppleIDProvider>) end;
  PASAuthorizationAppleIDProvider = Pointer;

  {*******************************************************}
  ASAuthorizationControllerClass = interface(NSObjectClass)
    ['{1FFA7BB5-DAA7-4FD5-A5B7-FE466A83A23F}']
  end;
  ASAuthorizationController = interface(NSObject)
    ['{214873D8-CCF0-42B8-A381-04BBCFB75269}']
    //function authorizationRequests : NSArray; cdecl;
    procedure setDelegate(delegate: ASAuthorizationControllerDelegate); cdecl;
    function delegate: ASAuthorizationControllerDelegate; cdecl;
    procedure setPresentationContextProvider(presentationContextProvider: ASAuthorizationControllerPresentationContextProviding); cdecl;
    function presentationContextProvider: ASAuthorizationControllerPresentationContextProviding; cdecl;
    function initWithAuthorizationRequests(authorizationRequests: NSArray): ASAuthorizationController; cdecl;
    procedure performRequests; cdecl;
  end;
  TASAuthorizationController = class(TOCGenericImport<ASAuthorizationControllerClass, ASAuthorizationController>)  end;
  PASAuthorizationController = Pointer;

  {************************************************}
  ASAuthorizationCredential = interface(IObjectiveC)
    ['{0D923E10-CD5D-4EFB-AE75-E69DBB3066E5}']
  end;

  {********************************************************}
  ASAuthorizationControllerDelegate = interface(IObjectiveC)
    ['{1DF3A867-FA9E-4A29-9D91-6BF18C335DF7}']
    [MethodName('authorizationController:didCompleteWithAuthorization:')]
    procedure authorizationControllerDidCompleteWithAuthorization(controller: ASAuthorizationController; didCompleteWithAuthorization: ASAuthorization); cdecl;
    [MethodName('authorizationController:didCompleteWithError:')]
    procedure authorizationControllerDidCompleteWithError(controller: ASAuthorizationController; didCompleteWithError: NSError); cdecl;
  end;

  {****************************************************************************}
  ASAuthorizationControllerPresentationContextProviding = interface(IObjectiveC)
    ['{13445BA6-0290-4745-9E26-BB99ADBDE7BD}']
    //function presentationAnchorForAuthorizationController(controller: ASAuthorizationController) : ASPresentationAnchor; cdecl;
  end;

function ASAuthorizationScopeFullName: NSString;
function ASAuthorizationScopeEmail: NSString;

const
  libAuthenticationServices = '/System/Library/Frameworks/AuthenticationServices.framework/AuthenticationServices';

implementation

{**********************************************}
function ASAuthorizationScopeFullName: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationScopeFullName');
end;

{*******************************************}
function ASAuthorizationScopeEmail: NSString;
begin
  Result := CocoaNSStringConst(libAuthenticationServices, 'ASAuthorizationScopeEmail');
end;

end.