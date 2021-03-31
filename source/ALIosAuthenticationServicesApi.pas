unit ALIosAuthenticationServicesApi;

interface

uses
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation;

{$M+}

//type

  //ASPresentationAnchor = UIWindow; PASPresentationAnchor = ^ASPresentationAnchor;
  //ASAuthorizationScope = NSString; PASAuthorizationScope = ^ASAuthorizationScope;
  //ASAuthorizationAppleIDButtonType = NSInteger;
  //ASAuthorizationAppleIDButtonStyle = NSInteger;
  //ASAuthorizationOpenIDOperation = NSString; PASAuthorizationOpenIDOperation = ^ASAuthorizationOpenIDOperation;
  //ASAuthorizationAppleIDProviderCredentialState = NSInteger;
  //TAuthenticationServicesCompletion = procedure(param1: ASAuthorizationAppleIDProviderCredentialState; param2: NSError) of object;
  //ASAuthorizationError = NSInteger;
  //ASAuthorizationProviderAuthorizationOperation = NSString; PASAuthorizationProviderAuthorizationOperation = ^ASAuthorizationProviderAuthorizationOperation;
  //TAuthenticationServicesCompletion1 = procedure(param1: Boolean; param2: NSError) of object;
  //ASCredentialIdentityStoreErrorCode = NSInteger;
  //TAuthenticationServicesCompletion2 = procedure(param1: ASCredentialIdentityStoreState) of object;
  //ASExtensionErrorCode = NSInteger;
  //TAuthenticationServicesCompletionHandler = procedure(param1: Boolean) of object;
  //ASCredentialServiceIdentifierType = NSInteger;
  //ASWebAuthenticationSessionErrorCode = NSInteger;
  //ASWebAuthenticationSessionCompletionHandler = procedure(param1: NSURL; param2: NSError) of object;

//const

  //ASAuthorizationAppleIDButtonTypeSignIn = 0;
  //ASAuthorizationAppleIDButtonTypeContinue = 1;
  //ASAuthorizationAppleIDButtonTypeSignUp = 2;
  //ASAuthorizationAppleIDButtonTypeDefault = ASAuthorizationAppleIDButtonTypeSignIn;
  //ASAuthorizationAppleIDButtonStyleWhite = 0;
  //ASAuthorizationAppleIDButtonStyleWhiteOutline = 1;
  //ASAuthorizationAppleIDButtonStyleBlack = 2;
  //ASAuthorizationAppleIDProviderCredentialRevoked = 0;
  //ASAuthorizationAppleIDProviderCredentialAuthorized = 1;
  //ASAuthorizationAppleIDProviderCredentialNotFound = 2;
  //ASAuthorizationAppleIDProviderCredentialTransferred = 3;
  //ASAuthorizationErrorUnknown = 1000;
  //ASAuthorizationErrorCanceled = 1001;
  //ASAuthorizationErrorInvalidResponse = 1002;
  //ASAuthorizationErrorNotHandled = 1003;
  //ASAuthorizationErrorFailed = 1004;
  //ASCredentialIdentityStoreErrorCodeInternalError = 0;
  //ASCredentialIdentityStoreErrorCodeStoreDisabled = 1;
  //ASCredentialIdentityStoreErrorCodeStoreBusy = 2;
  //ASExtensionErrorCodeFailed = 0;
  //ASExtensionErrorCodeUserCanceled = 1;
  //ASExtensionErrorCodeUserInteractionRequired = 100;
  //ASExtensionErrorCodeCredentialIdentityNotFound = 101;
  //ASCredentialServiceIdentifierTypeDomain = 0;
  //ASCredentialServiceIdentifierTypeURL = 1;
  //ASWebAuthenticationSessionErrorCodeCanceledLogin = 1;
  //ASWebAuthenticationSessionErrorCodePresentationContextNotProvided = 2;
  //ASWebAuthenticationSessionErrorCodePresentationContextInvalid = 3;

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

  {*************************************}
  //@interface ASAuthorization : NSObject
  ASAuthorizationClass = interface(NSObjectClass)
  ['{B3EE72FE-E71F-47B7-AF7B-C0D403FC3469}']
  end;
  ASAuthorization = interface(NSObject)
  ['{3E3F8886-0CA3-4C89-BFF6-CC33F355BBD4}']

    //@property (nonatomic, readonly, strong) id <ASAuthorizationProvider> provider;
    //function provider: ASAuthorizationProvider; cdecl;

    //@property (nonatomic, readonly, strong) id <ASAuthorizationCredential> credential;
    function credential: pointer; cdecl;

  end;
  TASAuthorization = class(TOCGenericImport<ASAuthorizationClass, ASAuthorization>) end;
  PASAuthorization = Pointer;

  {*************************************************************}
  //ASAuthorizationAppleIDButtonClass = interface(UIControlClass)
    //['{39896F41-A860-4F1D-83CA-22B074680EDB}']
    //{class} function buttonWithType(&type: ASAuthorizationAppleIDButtonType; style: ASAuthorizationAppleIDButtonStyle) : Pointer {instancetype}; cdecl;
  //end;
  //ASAuthorizationAppleIDButton = interface(UIControl)
    //['{ADAE4A1C-AA81-4C2F-A182-84D7EB605B3C}']
    //function initWithAuthorizationButtonType(&type: ASAuthorizationAppleIDButtonType; authorizationButtonStyle: ASAuthorizationAppleIDButtonStyle) : Pointer {instancetype}; cdecl;
    //procedure setCornerRadius(cornerRadius: CGFloat); cdecl;
    //function cornerRadius : CGFloat; cdecl;
  //end;
  //TASAuthorizationAppleIDButton = class(TOCGenericImport<ASAuthorizationAppleIDButtonClass, ASAuthorizationAppleIDButton>)  end;
  //PASAuthorizationAppleIDButton = Pointer;

  {**********************************************************************************}
  //@interface ASAuthorizationAppleIDCredential : NSObject <ASAuthorizationCredential>
  ASAuthorizationAppleIDCredentialClass = interface(NSObjectClass)
    ['{76C8B027-86AB-49DB-B924-7129BF9E9375}']
  end;
  ASAuthorizationAppleIDCredential = interface(NSObject)
    ['{2255DB92-789C-430B-A4DE-46B309CA2812}']

    //@property (nonatomic, readonly, copy) NSString *user;
    function user : NSString; cdecl;

    //@property (nonatomic, readonly, copy, nullable) NSString *state;
    function state : NSString; cdecl;

    //@property (nonatomic, readonly, copy) NSArray<ASAuthorizationScope> *authorizedScopes;
    function authorizedScopes : NSArray; cdecl;

    //@property (nonatomic, readonly, copy, nullable) NSData *authorizationCode;
    function authorizationCode : NSData; cdecl;

    //@property (nonatomic, readonly, copy, nullable) NSData *identityToken;
    function identityToken : NSData; cdecl;

    //@property (nonatomic, readonly, copy, nullable) NSString *email;
    function email : NSString; cdecl;

    //@property (nonatomic, readonly, copy, nullable) NSPersonNameComponents *fullName;
    function fullName : NSPersonNameComponents; cdecl;

    //@property (nonatomic, readonly) ASUserDetectionStatus realUserStatus;
    function realUserStatus : ASUserDetectionStatus; cdecl;

  end;
  TASAuthorizationAppleIDCredential = class(TOCGenericImport<ASAuthorizationAppleIDCredentialClass, ASAuthorizationAppleIDCredential>)  end;
  PASAuthorizationAppleIDCredential = Pointer;

  {*************************************************************************}
  //@interface ASAuthorizationRequest : NSObject <NSCopying, NSSecureCoding>
  ASAuthorizationRequestClass = interface(NSObjectClass)
    ['{5AD0FB7E-FF24-4865-B718-524C0CC7DDEF}']
  end;
  ASAuthorizationRequest = interface(NSObject)
    ['{A8F407B9-DC6F-45BA-82D7-CBF8D58A556F}']

    //@property (nonatomic, readonly, strong) id <ASAuthorizationProvider> provider;
    //function provider : Pointer; cdecl;

  end;
  TASAuthorizationRequest = class(TOCGenericImport<ASAuthorizationRequestClass, ASAuthorizationRequest>)  end;
  PASAuthorizationRequest = Pointer;

  {****************************************************************}
  //@interface ASAuthorizationOpenIDRequest : ASAuthorizationRequest
  ASAuthorizationOpenIDRequestClass = interface(ASAuthorizationRequestClass)
    ['{855A26FA-A5F7-410D-992E-716540F37697}']
  end;
  ASAuthorizationOpenIDRequest = interface(ASAuthorizationRequest)
    ['{81FDDC8E-C937-40E2-A776-0522190D5F90}']

    //@property (nonatomic, copy, nullable) NSArray<ASAuthorizationScope> *requestedScopes;
    procedure setRequestedScopes(requestedScopes: NSArray); cdecl;
    function requestedScopes : NSArray; cdecl;

    //@property (nonatomic, copy, nullable) NSString *state;
    //procedure setState(state: NSString); cdecl;
    //function state : NSString; cdecl;

    //@property (nonatomic, copy, nullable) NSString *nonce;
    //procedure setNonce(nonce: NSString); cdecl;
    //function nonce : NSString; cdecl;

    //@property (nonatomic, copy) ASAuthorizationOpenIDOperation requestedOperation;
    //procedure setRequestedOperation(requestedOperation: ASAuthorizationOpenIDOperation); cdecl;
    //function requestedOperation : ASAuthorizationOpenIDOperation; cdecl;

  end;
  TASAuthorizationOpenIDRequest = class(TOCGenericImport<ASAuthorizationOpenIDRequestClass, ASAuthorizationOpenIDRequest>)  end;
  PASAuthorizationOpenIDRequest = Pointer;

  {***********************************************************************}
  //@interface ASAuthorizationAppleIDRequest : ASAuthorizationOpenIDRequest
  ASAuthorizationAppleIDRequestClass = interface(ASAuthorizationOpenIDRequestClass)
    ['{DD1BE600-3F35-45DB-B0ED-594C7FEC98C3}']
  end;
  ASAuthorizationAppleIDRequest = interface(ASAuthorizationOpenIDRequest)
    ['{06C37879-9A4E-4C4C-88A3-DCCB0CD69EA7}']

    //@property (nonatomic, copy, nullable) NSString *user;
    //procedure setUser(user: NSString); cdecl;
    //function user : NSString; cdecl;

  end;
  TASAuthorizationAppleIDRequest = class(TOCGenericImport<ASAuthorizationAppleIDRequestClass, ASAuthorizationAppleIDRequest>)  end;
  PASAuthorizationAppleIDRequest = Pointer;

  {******************************************************************************}
  //@interface ASAuthorizationAppleIDProvider : NSObject <ASAuthorizationProvider>
  ASAuthorizationAppleIDProviderClass = interface(NSObjectClass)
    ['{037C5CCA-93C0-47BF-A664-3BBE288AD211}']
  end;
  ASAuthorizationAppleIDProvider = interface(NSObject)
    ['{CBEA2AEE-19AC-4C71-8904-618B6DD4F47F}']

    //- (ASAuthorizationAppleIDRequest *)createRequest;
    function createRequest: ASAuthorizationAppleIDRequest; cdecl;

    //- (void)getCredentialStateForUserID:(NSString *)userID completion:(void (^)(ASAuthorizationAppleIDProviderCredentialState credentialState, NSError * _Nullable error))completion;
    //procedure getCredentialStateForUserID(userID: NSString; completion: TAuthenticationServicesCompletion); cdecl;

  end;
  TASAuthorizationAppleIDProvider = class(TOCGenericImport<ASAuthorizationAppleIDProviderClass, ASAuthorizationAppleIDProvider>) end;
  PASAuthorizationAppleIDProvider = Pointer;

  {***********************************************}
  //@interface ASAuthorizationController : NSObject
  ASAuthorizationControllerClass = interface(NSObjectClass)
    ['{1FFA7BB5-DAA7-4FD5-A5B7-FE466A83A23F}']
  end;
  ASAuthorizationController = interface(NSObject)
    ['{214873D8-CCF0-42B8-A381-04BBCFB75269}']

    //@property (nonatomic, readonly, strong) NSArray<ASAuthorizationRequest *> *authorizationRequests;
    //function authorizationRequests : NSArray; cdecl;

    //@property (nonatomic, weak, nullable) id <ASAuthorizationControllerDelegate> delegate;
    procedure setDelegate(delegate: ASAuthorizationControllerDelegate); cdecl;
    function delegate: ASAuthorizationControllerDelegate; cdecl;

    //@property (nonatomic, weak, nullable) id <ASAuthorizationControllerPresentationContextProviding> presentationContextProvider API_UNAVAILABLE(watchos);
    procedure setPresentationContextProvider(presentationContextProvider: ASAuthorizationControllerPresentationContextProviding); cdecl;
    function presentationContextProvider: ASAuthorizationControllerPresentationContextProviding; cdecl;

    //- (instancetype)initWithAuthorizationRequests:(NSArray<ASAuthorizationRequest *> *)authorizationRequests NS_DESIGNATED_INITIALIZER;
    function initWithAuthorizationRequests(authorizationRequests: NSArray): ASAuthorizationController; cdecl;

    //- (void)performRequests;
    procedure performRequests; cdecl;

  end;
  TASAuthorizationController = class(TOCGenericImport<ASAuthorizationControllerClass, ASAuthorizationController>)  end;
  PASAuthorizationController = Pointer;

  {****************************************************************************}
  //ASAuthorizationPasswordRequestClass = interface(ASAuthorizationRequestClass)
    //['{B3EE04D2-EB2E-4F6B-A2E8-58E336426645}']
  //end;
  //ASAuthorizationPasswordRequest = interface(ASAuthorizationRequest)
    //['{65031079-B50D-4055-9069-02CC180DD067}']
  //end;
  //TASAuthorizationPasswordRequest = class(TOCGenericImport<ASAuthorizationPasswordRequestClass, ASAuthorizationPasswordRequest>)  end;
  //PASAuthorizationPasswordRequest = Pointer;

  {***************************************************************}
  //ASAuthorizationPasswordProviderClass = interface(NSObjectClass)
    //['{29C7816A-F9DC-4EC5-AADC-CBF4CBB34FC7}']
  //end;
  //ASAuthorizationPasswordProvider = interface(NSObject)
    //['{180221E2-685A-445D-91C8-4B1D1F069A48}']
    //function createRequest : ASAuthorizationPasswordRequest; cdecl;
  //end;
  //TASAuthorizationPasswordProvider = class(TOCGenericImport<ASAuthorizationPasswordProviderClass, ASAuthorizationPasswordProvider>)  end;
  //PASAuthorizationPasswordProvider = Pointer;

  {************************************************************************************}
  //ASAuthorizationProviderExtensionAuthorizationRequestClass = interface(NSObjectClass)
    //['{BC418477-4C1C-4EF9-8868-E19799CFB960}']
  //end;
  //ASAuthorizationProviderExtensionAuthorizationRequest = interface(NSObject)
    //['{1BD6C8DB-4CF8-4271-B07B-F40C58FAF8FC}']
    //procedure doNotHandle; cdecl;
    //procedure cancel; cdecl;
    //procedure complete; cdecl;
    //procedure completeWithHTTPAuthorizationHeaders(httpAuthorizationHeaders: NSDictionary); cdecl;
    //procedure completeWithHTTPResponse(httpResponse: NSHTTPURLResponse; httpBody: NSData); cdecl;
    //procedure completeWithError(error: NSError); cdecl;
    //procedure presentAuthorizationViewControllerWithCompletion(completion: TAuthenticationServicesCompletion1); cdecl;
    //function url : NSURL; cdecl;
    //function requestedOperation : ASAuthorizationProviderAuthorizationOperation; cdecl;
    //function httpHeaders : NSDictionary; cdecl;
    //function httpBody : NSData; cdecl;
    //function realm : NSString; cdecl;
    //function extensionData : NSDictionary; cdecl;
    //function callerBundleIdentifier : NSString; cdecl;
    //function authorizationOptions : NSDictionary; cdecl;
  //end;
  //TASAuthorizationProviderExtensionAuthorizationRequest = class(TOCGenericImport<ASAuthorizationProviderExtensionAuthorizationRequestClass, ASAuthorizationProviderExtensionAuthorizationRequest>)  end;
  //PASAuthorizationProviderExtensionAuthorizationRequest = Pointer;

  {*********************************************************************}
  //ASAuthorizationSingleSignOnCredentialClass = interface(NSObjectClass)
    //['{ECE8DF9E-013D-4D78-97CB-96A52AA09E7B}']
  //end;
  //ASAuthorizationSingleSignOnCredential = interface(NSObject)
    //['{6C2C9687-817A-49F9-9827-28429A868DEC}']
    //function state : NSString; cdecl;
    //function accessToken : NSData; cdecl;
    //function identityToken : NSData; cdecl;
    //function authorizedScopes : NSArray; cdecl;
    //function authenticatedResponse : NSHTTPURLResponse; cdecl;
  //end;
  //TASAuthorizationSingleSignOnCredential = class(TOCGenericImport<ASAuthorizationSingleSignOnCredentialClass, ASAuthorizationSingleSignOnCredential>)  end;
  //PASAuthorizationSingleSignOnCredential = Pointer;

  {**************************************************************************************}
  //ASAuthorizationSingleSignOnRequestClass = interface(ASAuthorizationOpenIDRequestClass)
    //['{7CA25F1D-5CD5-4A89-9981-3D58C9313FFB}']
  //end;
  //ASAuthorizationSingleSignOnRequest = interface(ASAuthorizationOpenIDRequest)
    //['{6B818705-9F7D-408E-8A89-52AE7E730B49}']
    //procedure setAuthorizationOptions(authorizationOptions: NSArray); cdecl;
    //function authorizationOptions : NSArray; cdecl;
  //end;
  //TASAuthorizationSingleSignOnRequest = class(TOCGenericImport<ASAuthorizationSingleSignOnRequestClass, ASAuthorizationSingleSignOnRequest>)  end;
  //PASAuthorizationSingleSignOnRequest = Pointer;

  {*******************************************************************}
  //ASAuthorizationSingleSignOnProviderClass = interface(NSObjectClass)
    //['{A4085F67-B9A7-41DF-8397-86EB4D0EE17A}']
    //{class} function authorizationProviderWithIdentityProviderURL(url: NSURL) : Pointer {instancetype}; cdecl;
  //end;
  //ASAuthorizationSingleSignOnProvider = interface(NSObject)
    //['{94353D21-59DB-4AB4-A7E4-24E1632FFDFB}']
    //function createRequest : ASAuthorizationSingleSignOnRequest; cdecl;
    //function url : NSURL; cdecl;
    //function canPerformAuthorization : Boolean; cdecl;
  //end;
  //TASAuthorizationSingleSignOnProvider = class(TOCGenericImport<ASAuthorizationSingleSignOnProviderClass, ASAuthorizationSingleSignOnProvider>)  end;
  //PASAuthorizationSingleSignOnProvider = Pointer;

  {**************************************************************}
  //ASCredentialIdentityStoreStateClass = interface(NSObjectClass)
    //['{B04A7AB9-13F6-4E2B-88F1-6D2A863A5AE0}']
  //end;
  //ASCredentialIdentityStoreState = interface(NSObject)
    //['{CB1A10BE-319F-4805-9BA1-2454AB3FA096}']
    //function isEnabled : Boolean; cdecl;
    //function supportsIncrementalUpdates : Boolean; cdecl;
  //end;
  //TASCredentialIdentityStoreState = class(TOCGenericImport<ASCredentialIdentityStoreStateClass, ASCredentialIdentityStoreState>)  end;
  //PASCredentialIdentityStoreState = Pointer;

  {************************************************************}
  //ASPasswordCredentialIdentityClass = interface(NSObjectClass)
    //['{FCD97F4F-E3CA-400F-AFE4-9EEC081983DA}']
    //{class} function identityWithServiceIdentifier(serviceIdentifier: ASCredentialServiceIdentifier; user: NSString; recordIdentifier: NSString) : Pointer {instancetype}; cdecl;
  //end;
  //ASPasswordCredentialIdentity = interface(NSObject)
    //['{450CDB56-C06C-4263-9D99-AD9F0220FB36}']
    //function initWithServiceIdentifier(serviceIdentifier: ASCredentialServiceIdentifier; user: NSString; recordIdentifier: NSString) : Pointer {instancetype}; cdecl;
    //function serviceIdentifier : ASCredentialServiceIdentifier; cdecl;
    //function user : NSString; cdecl;
    //function recordIdentifier : NSString; cdecl;
    //procedure setRank(rank: NSInteger); cdecl;
    //function rank : NSInteger; cdecl;
  //end;
  //TASPasswordCredentialIdentity = class(TOCGenericImport<ASPasswordCredentialIdentityClass, ASPasswordCredentialIdentity>)  end;
  //PASPasswordCredentialIdentity = Pointer;

  {*********************************************************}
  //ASCredentialIdentityStoreClass = interface(NSObjectClass)
    //['{FC3883A8-2B93-45D0-8E84-0FA38795124C}']
  //end;
  //ASCredentialIdentityStore = interface(NSObject)
    //['{35AE3526-5747-4257-A7F2-575962C1BCDC}']
    //procedure setSharedStore(sharedStore: ASCredentialIdentityStore); cdecl;
    //function sharedStore : ASCredentialIdentityStore; cdecl;
    //procedure getCredentialIdentityStoreStateWithCompletion(completion: TAuthenticationServicesCompletion2); cdecl;
    //procedure saveCredentialIdentities(credentialIdentities: NSArray; completion: TAuthenticationServicesCompletion1); cdecl;
    //procedure removeCredentialIdentities(credentialIdentities: NSArray; completion: TAuthenticationServicesCompletion1); cdecl;
    //procedure removeAllCredentialIdentitiesWithCompletion(completion: TAuthenticationServicesCompletion1); cdecl;
    //procedure replaceCredentialIdentitiesWithIdentities(newCredentialIdentities: NSArray; completion: TAuthenticationServicesCompletion1); cdecl;
  //end;
  //TASCredentialIdentityStore = class(TOCGenericImport<ASCredentialIdentityStoreClass, ASCredentialIdentityStore>)  end;
  //PASCredentialIdentityStore = Pointer;

  {****************************************************}
  //ASPasswordCredentialClass = interface(NSObjectClass)
    //['{4433ED6C-A682-4D4C-8D2E-1576CCC5B141}']
    //{class} function credentialWithUser(user: NSString; password: NSString) : Pointer {instancetype}; cdecl;
  //end;
  //ASPasswordCredential = interface(NSObject)
    //['{0EDD4137-DCB2-41BA-A458-4E09866F3D7E}']
    //function initWithUser(user: NSString; password: NSString) : Pointer {instancetype}; cdecl;
    //function user : NSString; cdecl;
    //function password : NSString; cdecl;
  //end;
  //TASPasswordCredential = class(TOCGenericImport<ASPasswordCredentialClass, ASPasswordCredential>)  end;
  //PASPasswordCredential = Pointer;

  {******************************************************************************}
  //ASCredentialProviderExtensionContextClass = interface(NSExtensionContextClass)
    //['{EB581D97-60E3-49C1-9063-19681C3AB384}']
  //end;
  //ASCredentialProviderExtensionContext = interface(NSExtensionContext)
    //['{FB1B3523-145E-47EB-95B2-B752CCA1CA9F}']
    //procedure completeRequestWithSelectedCredential(credential: ASPasswordCredential; completionHandler: TAuthenticationServicesCompletionHandler); cdecl;
    //procedure completeExtensionConfigurationRequest; cdecl;
    //procedure cancelRequestWithError(error: NSError); cdecl;
  //end;
  //TASCredentialProviderExtensionContext = class(TOCGenericImport<ASCredentialProviderExtensionContextClass, ASCredentialProviderExtensionContext>)  end;
  //PASCredentialProviderExtensionContext = Pointer;

  {*************************************************************}
  //ASCredentialServiceIdentifierClass = interface(NSObjectClass)
    //['{2CCFA9BD-8ED4-4BD8-8538-C198574A8379}']
  //end;
  //ASCredentialServiceIdentifier = interface(NSObject)
    //['{4136B746-2FE1-4AC3-A35C-B33301A2BA04}']
    //function initWithIdentifier(identifier: NSString; &type: ASCredentialServiceIdentifierType) : Pointer {instancetype}; cdecl;
    //function identifier : NSString; cdecl;
    //function &type : ASCredentialServiceIdentifierType; cdecl;
  //end;
  //TASCredentialServiceIdentifier = class(TOCGenericImport<ASCredentialServiceIdentifierClass, ASCredentialServiceIdentifier>)  end;
  //PASCredentialServiceIdentifier = Pointer;

  {**************************************************************************}
  //ASCredentialProviderViewControllerClass = interface(UIViewControllerClass)
    //['{774F23B0-6E78-4B2B-AE29-8557A41BAD00}']
  //end;
  //ASCredentialProviderViewController = interface(UIViewController)
    //['{4C702389-10A4-44FC-AD46-3E3755589E42}']
    //function extensionContext : ASCredentialProviderExtensionContext; cdecl;
    //procedure prepareCredentialListForServiceIdentifiers(serviceIdentifiers: NSArray); cdecl;
    //procedure provideCredentialWithoutUserInteractionForIdentity(credentialIdentity: ASPasswordCredentialIdentity); cdecl;
    //procedure prepareInterfaceToProvideCredentialForIdentity(credentialIdentity: ASPasswordCredentialIdentity); cdecl;
    //procedure prepareInterfaceForExtensionConfiguration; cdecl;
  //end;
  //TASCredentialProviderViewController = class(TOCGenericImport<ASCredentialProviderViewControllerClass, ASCredentialProviderViewController>)  end;
  //PASCredentialProviderViewController = Pointer;

  {**********************************************************}
  //ASWebAuthenticationSessionClass = interface(NSObjectClass)
    //['{7D37E2D9-2CEA-4B3D-8B15-6145203C780F}']
  //end;
  //ASWebAuthenticationSession = interface(NSObject)
    //['{85B1076A-7A07-402F-900D-98117E8DE7D4}']
    //function initWithURL(URL: NSURL; callbackURLScheme: NSString; completionHandler: ASWebAuthenticationSessionCompletionHandler) : Pointer {instancetype}; cdecl;
    //procedure setPresentationContextProvider(presentationContextProvider: Pointer); cdecl;
    //function presentationContextProvider : Pointer; cdecl;
    //procedure setPrefersEphemeralWebBrowserSession(prefersEphemeralWebBrowserSession: Boolean); cdecl;
    //function prefersEphemeralWebBrowserSession : Boolean; cdecl;
    //function canStart : Boolean; cdecl;
    //function start : Boolean; cdecl;
    //procedure cancel; cdecl;
  //end;
  //TASWebAuthenticationSession = class(TOCGenericImport<ASWebAuthenticationSessionClass, ASWebAuthenticationSession>)  end;
  //PASWebAuthenticationSession = Pointer;

  {*****************************************************************}
  //ASWebAuthenticationSessionRequestClass = interface(NSObjectClass)
    //['{B2DE84AF-21DD-4F8F-A3AE-C1D5F6EB85FF}']
  //end;
  //ASWebAuthenticationSessionRequest = interface(NSObject)
    //['{36794901-5A07-44DE-8F42-EC8A95BE6F0F}']
    //function UUID : NSUUID; cdecl;
    //function URL : NSURL; cdecl;
    //function callbackURLScheme : NSString; cdecl;
    //function shouldUseEphemeralSession : Boolean; cdecl;
    //procedure setDelegate(delegate: Pointer); cdecl;
    //function delegate : Pointer; cdecl;
    //procedure cancelWithError(error: NSError); cdecl;
    //procedure completeWithCallbackURL(url: NSURL); cdecl;
  //end;
  //TASWebAuthenticationSessionRequest = class(TOCGenericImport<ASWebAuthenticationSessionRequestClass, ASWebAuthenticationSessionRequest>)  end;
  //PASWebAuthenticationSessionRequest = Pointer;

  {**********************************************************************************}
  //ASWebAuthenticationSessionWebBrowserSessionManagerClass = interface(NSObjectClass)
    //['{3C8D0CDD-467F-41FF-B43A-C13D7B38024D}']
  //end;
  //ASWebAuthenticationSessionWebBrowserSessionManager = interface(NSObject)
    //['{2E49517F-93EF-4E4C-A1AB-E3F57609E7CD}']
    //procedure setSharedManager(sharedManager: ASWebAuthenticationSessionWebBrowserSessionManager); cdecl;
    //function sharedManager : ASWebAuthenticationSessionWebBrowserSessionManager; cdecl;
    //procedure setSessionHandler(sessionHandler: Pointer); cdecl;
    //function sessionHandler : Pointer; cdecl;
    //function wasLaunchedByAuthenticationServices : Boolean; cdecl;
  //end;
  //TASWebAuthenticationSessionWebBrowserSessionManager = class(TOCGenericImport<ASWebAuthenticationSessionWebBrowserSessionManagerClass, ASWebAuthenticationSessionWebBrowserSessionManager>)  end;
  //PASWebAuthenticationSessionWebBrowserSessionManager = Pointer;

  {************************************************}
  //ASAuthorizationProvider = interface(IObjectiveC)
    //['{BD365B9A-72C6-4563-87F3-475CFA7C601C}']
  //end;

  {*************************************************************************}
  //@protocol ASAuthorizationCredential <NSObject, NSCopying, NSSecureCoding>
  ASAuthorizationCredential = interface(IObjectiveC)
    ['{0D923E10-CD5D-4EFB-AE75-E69DBB3066E5}']
  end;

  {******************************************************}
  //@protocol ASAuthorizationControllerDelegate <NSObject>
  ASAuthorizationControllerDelegate = interface(IObjectiveC)
    ['{1DF3A867-FA9E-4A29-9D91-6BF18C335DF7}']

    //- (void)authorizationController:(ASAuthorizationController *)controller didCompleteWithAuthorization:(ASAuthorization *)authorization NS_SWIFT_NAME(authorizationController(controller:didCompleteWithAuthorization:));
    [MethodName('authorizationController:didCompleteWithAuthorization:')]
    procedure authorizationControllerDidCompleteWithAuthorization(controller: ASAuthorizationController; didCompleteWithAuthorization: ASAuthorization); cdecl;

    //- (void)authorizationController:(ASAuthorizationController *)controller didCompleteWithError:(NSError *)error  NS_SWIFT_NAME(authorizationController(controller:didCompleteWithError:));
    [MethodName('authorizationController:didCompleteWithError:')]
    procedure authorizationControllerDidCompleteWithError(controller: ASAuthorizationController; didCompleteWithError: NSError); cdecl;

  end;

  {**************************************************************************}
  //@protocol ASAuthorizationControllerPresentationContextProviding <NSObject>
  ASAuthorizationControllerPresentationContextProviding = interface(IObjectiveC)
    ['{13445BA6-0290-4745-9E26-BB99ADBDE7BD}']

    //- (ASPresentationAnchor)presentationAnchorForAuthorizationController:(ASAuthorizationController *)controller;
    //function presentationAnchorForAuthorizationController(controller: ASAuthorizationController) : ASPresentationAnchor; cdecl;

  end;

  {************************************************************************************}
  //ASAuthorizationProviderExtensionAuthorizationRequestHandler = interface(IObjectiveC)
    //['{03DF20D9-64AD-4101-82C0-DAEF95098030}']
    //procedure beginAuthorizationWithRequest(request: ASAuthorizationProviderExtensionAuthorizationRequest); cdecl;
    //procedure cancelAuthorizationWithRequest(request: ASAuthorizationProviderExtensionAuthorizationRequest); cdecl;
  //end;

  {************************************************************************}
  //ASWebAuthenticationPresentationContextProviding = interface(IObjectiveC)
    //['{93CF90B1-25F5-4A73-A7B9-BBC0464E771C}']
    //function presentationAnchorForWebAuthenticationSession(session: ASWebAuthenticationSession) : ASPresentationAnchor; cdecl;
  //end;

  {******************************************************************}
  //ASWebAuthenticationSessionRequestDelegate = interface(IObjectiveC)
    //['{036E064C-6E49-45F4-B8BB-3CE3CA99B0E5}']
    //[MethodName('authenticationSessionRequest:didCompleteWithCallbackURL:')]
    //procedure authenticationSessionRequestDidCompleteWithCallbackURL(authenticationSessionRequest: ASWebAuthenticationSessionRequest; didCompleteWithCallbackURL: NSURL); cdecl;
    //[MethodName('authenticationSessionRequest:didCancelWithError:')]
    //procedure authenticationSessionRequestDidCancelWithError(authenticationSessionRequest: ASWebAuthenticationSessionRequest; didCancelWithError: NSError); cdecl;
  //end;

  {****************************************************************************}
  //ASWebAuthenticationSessionWebBrowserSessionHandling = interface(IObjectiveC)
    //['{9D44EB22-1E36-450A-A4D0-78E237C4DB1A}']
    //procedure beginHandlingWebAuthenticationSessionRequest(request: ASWebAuthenticationSessionRequest); cdecl;
    //procedure cancelWebAuthenticationSessionRequest(request: ASWebAuthenticationSessionRequest); cdecl;
  //end;

function ASAuthorizationScopeFullName: NSString;
function ASAuthorizationScopeEmail: NSString;
//function ASAuthorizationOperationImplicit: Pointer;
//function ASAuthorizationOperationLogin: Pointer;
//function ASAuthorizationOperationRefresh: Pointer;
//function ASAuthorizationOperationLogout: Pointer;
//function ASAuthorizationAppleIDProviderCredentialRevokedNotification: Pointer;
//function ASAuthorizationErrorDomain: Pointer;
//function ASCredentialIdentityStoreErrorDomain: Pointer;
//function ASExtensionErrorDomain: Pointer;
//function ASWebAuthenticationSessionErrorDomain: Pointer;

const
  libAuthenticationServices = '/System/Library/Frameworks/AuthenticationServices.framework/AuthenticationServices';

implementation

// {$IF defined(IOS) and NOT defined(CPUARM)} => https://stackoverflow.com/questions/52475704/how-to-link-correctly-apple-ios-library

uses
  Posix.Dlfcn;

var
  AuthenticationServicesModule: THandle;

// {$ENDIF IOS} => https://stackoverflow.com/questions/52475704/how-to-link-correctly-apple-ios-library

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

{***************************************************}
//function ASAuthorizationOperationImplicit: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASAuthorizationOperationImplicit');
//end;

{************************************************}
//function ASAuthorizationOperationLogin: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASAuthorizationOperationLogin');
//end;

{**************************************************}
//function ASAuthorizationOperationRefresh: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASAuthorizationOperationRefresh');
//end;

{*************************************************}
//function ASAuthorizationOperationLogout: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASAuthorizationOperationLogout');
//end;

{******************************************************************************}
//function ASAuthorizationAppleIDProviderCredentialRevokedNotification: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASAuthorizationAppleIDProviderCredentialRevokedNotification');
//end;

{*********************************************}
//function ASAuthorizationErrorDomain: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASAuthorizationErrorDomain');
//end;

{*******************************************************}
//function ASCredentialIdentityStoreErrorDomain: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASCredentialIdentityStoreErrorDomain');
//end;

{*****************************************}
//function ASExtensionErrorDomain: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASExtensionErrorDomain');
//end;

{********************************************************}
//function ASWebAuthenticationSessionErrorDomain: Pointer;
//begin
//  Result := CocoaPointerConst(libAuthenticationServices, 'ASWebAuthenticationSessionErrorDomain');
//end;

// {$IF defined(IOS) and NOT defined(CPUARM)} => https://stackoverflow.com/questions/52475704/how-to-link-correctly-apple-ios-library

initialization
  AuthenticationServicesModule := dlopen(MarshaledAString(libAuthenticationServices), RTLD_LAZY);

finalization
  dlclose(AuthenticationServicesModule);

// {$ENDIF IOS} => https://stackoverflow.com/questions/52475704/how-to-link-correctly-apple-ios-library

end.
