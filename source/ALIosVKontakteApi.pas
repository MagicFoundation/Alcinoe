unit ALIosVKontakteApi;

interface

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  iOSapi.UIKit;

{$M+}

type

  {*****************************************}
  //NS_ENUM(NSUInteger, VKAuthorizationState)
  VKAuthorizationState = NSUInteger;

const

  {*************************}
  VKAuthorizationUnknown = 0;
  VKAuthorizationInitialized = 1;
  VKAuthorizationPending = 2;
  VKAuthorizationExternal = 3;
  VKAuthorizationSafariInApp = 4;
  VKAuthorizationWebview = 5;
  VKAuthorizationAuthorized = 6;
  VKAuthorizationError = 7;

type

  {*************************************************}
  //NS_ENUM(NSInteger, VKShareDialogControllerResult)
  VKShareDialogControllerResult = NSUInteger;

const

  {*****************************************}
  VKShareDialogControllerResultCancelled = 0;
  VKShareDialogControllerResultDone = 1;

type

  {****************}
  //enum VKImageType
  VKImageType = Cardinal;

const

  {*****************}
  VKImageTypeJpg = 0;
  VKImageTypePng = 1;

type

  {************************}
  VKObjectClass = interface;
  VKError = interface;
  VKAuthorizationResult = interface;
  VKAccessToken = interface;
  VKSdkDelegate = interface;
  VKSdkUIDelegate = interface;
  VKSdk = interface;
  VKImageParameters = interface;
  VKUploadImage = interface;
  VKShareLink = interface;
  VKShareDialogController = interface;

  {**************************************************************************************************************************************}
  TVKShareDialogControllerCompletionHandler = procedure(dialog: VKShareDialogController; result: VKShareDialogControllerResult) of object;

  {******************************}
  //@interface VKObject : NSObject
  VKObjectClass = interface(NSObjectClass)
    ['{73A33031-F242-4DEA-ACDA-235D6F5D119A}']
  end;
  VKObject = interface(NSObject)
    ['{2B83B160-E4D9-49CD-B568-3E899154D7F8}']
  end;
  TVKObject = class(TOCGenericImport<VKObjectClass, VKObject>) end;

  {******************************}
  //@interface VKError : VKObject
  VKErrorClass = interface(VKObjectClass)
  ['{B33FC424-445C-4F10-AFEA-23A82935E562}']

    //+ (instancetype)errorWithCode:(NSInteger)errorCode;
    {class} function errorWithCode(errorCode: NSInteger) : VKError {instancetype}; cdecl;

    //+ (instancetype)errorWithJson:(id)JSON;
    {class} function errorWithJson(JSON: Pointer) : VKError {instancetype}; cdecl;

    //+ (instancetype)errorWithQuery:(NSDictionary *)queryParams;
    {class} function errorWithQuery(queryParams: NSDictionary) : VKError {instancetype}; cdecl;

  end;
  VKError = interface(VKObject)
  ['{37FECAB7-19A6-4366-9046-69F04E676BCE}']

    //@property(nonatomic, strong) NSError *httpError;
    procedure setHttpError(httpError: NSError); cdecl;
    function httpError : NSError; cdecl;

    //@property(nonatomic, strong) VKError *apiError;
    procedure setApiError(apiError: VKError); cdecl;
    function apiError : VKError; cdecl;

    //@property(nonatomic, strong) VKRequest *request;
    //procedure setRequest(request: VKRequest); cdecl;
    //function request : VKRequest; cdecl;

    //@property(nonatomic, assign) NSInteger errorCode;
    procedure setErrorCode(errorCode: NSInteger); cdecl;
    function errorCode : NSInteger; cdecl;

    //@property(nonatomic, strong) NSString *errorMessage;
    procedure setErrorMessage(errorMessage: NSString); cdecl;
    function errorMessage : NSString; cdecl;

    //@property(nonatomic, strong) NSString *errorReason;
    procedure setErrorReason(errorReason: NSString); cdecl;
    function errorReason : NSString; cdecl;

    //@property(nonatomic, strong) NSString *errorText;
    procedure setErrorText(errorText: NSString); cdecl;
    function errorText : NSString; cdecl;

    //@property(nonatomic, strong) NSDictionary *requestParams;
    procedure setRequestParams(requestParams: NSDictionary); cdecl;
    function requestParams : NSDictionary; cdecl;

    //@property(nonatomic, strong) NSString *captchaSid;
    procedure setCaptchaSid(captchaSid: NSString); cdecl;
    function captchaSid : NSString; cdecl;

    //@property(nonatomic, strong) NSString *captchaImg;
    procedure setCaptchaImg(captchaImg: NSString); cdecl;
    function captchaImg : NSString; cdecl;

    //@property(nonatomic, strong) NSString *redirectUri;
    procedure setRedirectUri(redirectUri: NSString); cdecl;
    function redirectUri : NSString; cdecl;

    //@property(nonatomic, strong) id json;
    procedure setJson(json: Pointer); cdecl;
    function json : Pointer; cdecl;

    //- (void)answerCaptcha:(NSString *)userEnteredCode;
    procedure answerCaptcha(userEnteredCode: NSString); cdecl;

  end;
  TVKError = class(TOCGenericImport<VKErrorClass, VKError>)  end;

  {*******************************************}
  //@interface VKAuthorizationResult : VKObject
  VKAuthorizationResultClass = interface(VKObjectClass)
    ['{6ED5DDCA-39EF-43E2-AA51-B626DF082867}']
  end;
  VKAuthorizationResult = interface(VKObject)
    ['{9120AD24-BCA3-4651-A756-275A5D2E55AE}']

    //@property(nonatomic, readonly, strong) VKAccessToken *token;
    function token : VKAccessToken; cdecl;

    //@property(nonatomic, readonly, strong) VKUser *user;
    //function user : VKUser; cdecl;

    //@property(nonatomic, readonly, strong) NSError *error;
    function error : NSError; cdecl;

    //@property(nonatomic, readonly, assign) VKAuthorizationState state;
    function state : VKAuthorizationState; cdecl;

  end;
  TVKAuthorizationResult = class(TOCGenericImport<VKAuthorizationResultClass, VKAuthorizationResult>)  end;

  {**********************************************}
  //@interface VKAccessToken : VKObject <NSCoding>
  VKAccessTokenClass = interface(VKObjectClass)
  ['{363241C5-57C5-4ACE-8B6D-2AC3385504A2}']

    //+ (instancetype)tokenFromUrlString:(NSString *)urlString;
    {class} function tokenFromUrlString(urlString: NSString) : VKAccessToken {instancetype}; cdecl;

    //+ (instancetype)tokenWithToken:(NSString *)accessToken secret:(NSString *)secret userId:(NSString *)userId;
    {class} function tokenWithToken(accessToken: NSString; secret: NSString; userId: NSString) : VKAccessToken {instancetype}; cdecl;

    //+ (instancetype)savedToken:(NSString *)defaultsKey;
    {class} function savedToken(defaultsKey: NSString) : VKAccessToken {instancetype}; cdecl;

    //+ (void)delete:(NSString *)service;
    {class} procedure delete(service: NSString); cdecl;

  end;
  VKAccessToken = interface(VKObject)
  ['{CF2B08FD-A5BC-4CA2-8FD3-D34D7D7EACC1}']

    //@property(nonatomic, readonly, copy) NSString *accessToken;
    function accessToken : NSString; cdecl;

    //@property(nonatomic, readonly, copy) NSString *userId;
    function userId : NSString; cdecl;

    //@property(nonatomic, readonly, copy) NSString *secret;
    function secret : NSString; cdecl;

    //@property(nonatomic, readonly, copy) NSArray *permissions;
    function permissions : NSArray; cdecl;

    //@property(nonatomic, readonly, copy) NSString *email;
    function email : NSString; cdecl;

    //@property(nonatomic, readonly, assign) NSInteger expiresIn;
    function expiresIn : NSInteger; cdecl;

    //@property(nonatomic, readonly, assign) BOOL httpsRequired;
    function httpsRequired : Boolean; cdecl;

    //@property(nonatomic, readonly, assign) NSTimeInterval created;
    function created : NSTimeInterval; cdecl;

    //@property(nonatomic, readonly, strong) VKUser *localUser;
    //function localUser : VKUser; cdecl;

    //- (void)saveTokenToDefaults:(NSString *)defaultsKey;
    procedure saveTokenToDefaults(defaultsKey: NSString); cdecl;

    //- (BOOL)isExpired;
    function isExpired : Boolean; cdecl;

  end;
  TVKAccessToken = class(TOCGenericImport<VKAccessTokenClass, VKAccessToken>)  end;

  {**********************************}
  //@protocol VKSdkDelegate <NSObject>
  VKSdkDelegate = interface(IObjectiveC)
  ['{B86838F7-433A-49BA-8D4D-E3E6698B3F3B}']

    //- (void)vkSdkAccessAuthorizationFinishedWithResult:(VKAuthorizationResult *)result;
    procedure vkSdkAccessAuthorizationFinishedWithResult(result: VKAuthorizationResult); cdecl;

    //- (void)vkSdkUserAuthorizationFailed;
    procedure vkSdkUserAuthorizationFailed; cdecl;

    //- (void)vkSdkAuthorizationStateUpdatedWithResult:(VKAuthorizationResult *)result;
    procedure vkSdkAuthorizationStateUpdatedWithResult(result: VKAuthorizationResult); cdecl;

    //- (void)vkSdkAccessTokenUpdated:(VKAccessToken *)newToken oldToken:(VKAccessToken *)oldToken;
    procedure vkSdkAccessTokenUpdated(newToken: VKAccessToken; oldToken: VKAccessToken); cdecl;

    //- (void)vkSdkTokenHasExpired:(VKAccessToken *)expiredToken;
    procedure vkSdkTokenHasExpired(expiredToken: VKAccessToken); cdecl;

  end;

  {************************************}
  //@protocol VKSdkUIDelegate <NSObject>
  VKSdkUIDelegate = interface(IObjectiveC)
  ['{D2F9A031-4CFF-4EFF-8639-E4C1F4B49CF4}']

    //- (void)vkSdkShouldPresentViewController:(UIViewController *)controller;
    procedure vkSdkShouldPresentViewController(controller: UIViewController); cdecl;

    //- (void)vkSdkNeedCaptchaEnter:(VKError *)captchaError;
    procedure vkSdkNeedCaptchaEnter(captchaError: VKError); cdecl;

    //- (void)vkSdkWillDismissViewController:(UIViewController *)controller;
    procedure vkSdkWillDismissViewController(controller: UIViewController); cdecl;

    //- (void)vkSdkDidDismissViewController:(UIViewController *)controller;
    procedure vkSdkDidDismissViewController(controller: UIViewController); cdecl;

  end;

  {***************************}
  //@interface VKSdk : NSObject
  VKSdkClass = interface(NSObjectClass)
  ['{7412935A-53D4-403A-8C90-31A76A964B22}']

    //+ (instancetype)instance;
    {class} function instance : VKSdk {instancetype}; cdecl;

    //+ (BOOL)initialized;
    {class} function initialized : Boolean; cdecl;

    //+ (instancetype)initializeWithAppId:(NSString *)appId;
    [MethodName('initializeWithAppId:')]
    {class} function initializeWithAppId(appId: NSString) : VKSdk {instancetype}; cdecl;

    //+ (instancetype)initializeWithAppId:(NSString *)appId
    //                         apiVersion:(NSString *)version;
    [MethodName('initializeWithAppId:apiVersion:')]
    {class} function initializeWithAppIdApiVersion(appId: NSString; apiVersion: NSString) : VKSdk {instancetype}; cdecl;

    //+ (void)authorize:(NSArray *)permissions;
    [MethodName('authorize:')]
    {class} procedure authorize(permissions: NSArray); cdecl;

    //+ (void)authorize:(NSArray *)permissions withOptions:(VKAuthorizationOptions)options;
    //[MethodName('authorize:withOptions:')]
    //{class} procedure authorizeWithOptions(permissions: NSArray; withOptions: VKAuthorizationOptions); cdecl;

    //+ (VKAccessToken *)accessToken;
    {class} function accessToken : VKAccessToken; cdecl;

    //+ (BOOL)processOpenURL:(NSURL *)passedUrl fromApplication:(NSString *)sourceApplication;
    {class} function processOpenURL(passedUrl: NSURL; fromApplication: NSString) : Boolean; cdecl;

    //+ (BOOL)isLoggedIn;
    {class} function isLoggedIn : Boolean; cdecl;

    //+ (void)wakeUpSession:(NSArray *)permissions completeBlock:(void (^)(VKAuthorizationState state, NSError *error))wakeUpBlock;
    //{class} procedure wakeUpSession(permissions: NSArray; completeBlock: TVKSdkFrameworkCompleteBlock2); cdecl;

    //+ (void)forceLogout;
    {class} procedure forceLogout; cdecl;

    //+ (BOOL)vkAppMayExists;
    {class} function vkAppMayExists : Boolean; cdecl;

    //+ (void)setSchedulerEnabled:(BOOL)enabled;
    {class} procedure setSchedulerEnabled(enabled: Boolean); cdecl;

  end;
  VKSdk = interface(NSObject)
  ['{E81B4F55-E1D1-4822-B61F-0D9A299F0899}']

    //@property(nonatomic, readwrite, weak) id <VKSdkUIDelegate> uiDelegate;
    procedure setUiDelegate(uiDelegate: VKSdkUIDelegate); cdecl;
    function uiDelegate : VKSdkUIDelegate; cdecl;

    //@property(nonatomic, readonly, copy) NSString *currentAppId;
    function currentAppId : NSString; cdecl;

    //@property(nonatomic, readonly, copy) NSString *apiVersion;
    function apiVersion : NSString; cdecl;

    //- (void)registerDelegate:(id <VKSdkDelegate>)delegate;
    procedure registerDelegate(delegate: VKSdkDelegate); cdecl;

    //- (void)unregisterDelegate:(id <VKSdkDelegate>)delegate;
    procedure unregisterDelegate(delegate: VKSdkDelegate); cdecl;

    //- (BOOL)hasPermissions:(NSArray *)permissions;
    function hasPermissions(permissions: NSArray) : Boolean; cdecl;

  end;
  TVKSdk = class(TOCGenericImport<VKSdkClass, VKSdk>) end;

  {***************************************}
  //@interface VKImageParameters : VKObject
  VKImageParametersClass = interface(VKObjectClass)
  ['{6D7D4165-6DCB-424C-B0B0-197AE55B9495}']

    //+ (instancetype)pngImage;
    {class} function pngImage : VKImageParameters {instancetype}; cdecl;

    //+ (instancetype)jpegImageWithQuality:(float)quality;
    {class} function jpegImageWithQuality(quality: Single) : VKImageParameters {instancetype}; cdecl;

  end;
  VKImageParameters = interface(VKObject)
  ['{E787BC8E-21F9-4C35-859F-F263A0CF15B5}']

    //@property(nonatomic, assign) VKImageType imageType;
    procedure setImageType(imageType: VKImageType); cdecl;
    function imageType : VKImageType; cdecl;

    //@property(nonatomic, assign) CGFloat jpegQuality;
    procedure setJpegQuality(jpegQuality: CGFloat); cdecl;
    function jpegQuality : CGFloat; cdecl;

    //- (NSString *)fileExtension;
    function fileExtension : NSString; cdecl;

    //- (NSString *)mimeType;
    function mimeType : NSString; cdecl;

  end;
  TVKImageParameters = class(TOCGenericImport<VKImageParametersClass, VKImageParameters>)  end;

  {***********************************}
  //@interface VKUploadImage : VKObject
  VKUploadImageClass = interface(VKObjectClass)
    ['{FC55F5A9-235B-46E9-A99F-87DEFFDBC87F}']

    //+ (instancetype)uploadImageWithData:(NSData *)data andParams:(VKImageParameters *)params;
    {class} function uploadImageWithData(data: NSData; andParams: VKImageParameters) : VKUploadImage {instancetype}; cdecl;

    //+ (instancetype)uploadImageWithImage:(UIImage *)image andParams:(VKImageParameters *)params;
    {class} function uploadImageWithImage(image: UIImage; andParams: VKImageParameters) : VKUploadImage {instancetype}; cdecl;

  end;
  VKUploadImage = interface(VKObject)
    ['{FBD59C1C-D865-4BD1-967D-8A57A0DD3859}']

    //@property(nonatomic, strong) NSData *imageData;
    procedure setImageData(imageData: NSData); cdecl;
    function imageData : NSData; cdecl;

    //@property(nonatomic, strong) UIImage *sourceImage;
    procedure setSourceImage(sourceImage: UIImage); cdecl;
    function sourceImage : UIImage; cdecl;

    //@property(nonatomic, strong) VKImageParameters *parameters;
    procedure setParameters(parameters: VKImageParameters); cdecl;
    function parameters : VKImageParameters; cdecl;

  end;
  TVKUploadImage = class(TOCGenericImport<VKUploadImageClass, VKUploadImage>)  end;

  {*********************************}
  //@interface VKShareLink : VKObject
  VKShareLinkClass = interface(VKObjectClass)
    ['{3FB9D911-11F8-4DA0-95A4-6C827573B8E2}']
  end;
  VKShareLink = interface(VKObject)
    ['{4E949F57-5713-4F8F-99F7-E02A9BD74B50}']

    //@property(nonatomic, copy) NSString *title;
    procedure setTitle(title: NSString); cdecl;
    function title : NSString; cdecl;

    //@property(nonatomic, copy) NSURL *link;
    procedure setLink(link: NSURL); cdecl;
    function link : NSURL; cdecl;

    //- (instancetype)initWithTitle:(NSString *)title link:(NSURL *)link;
    function initWithTitle(title: NSString; link: NSURL) : VKShareLink {instancetype}; cdecl;

  end;
  TVKShareLink = class(TOCGenericImport<VKShareLinkClass, VKShareLink>)  end;

  {*****************************************************}
  //@interface VKShareDialogController : UIViewController
  VKShareDialogControllerClass = interface(UIViewControllerClass)
  ['{D12E9EBA-FA76-4240-863C-3F2F9F11D056}']
  end;
  VKShareDialogController = interface(UIViewController)
  ['{A2BADE99-4607-419E-B62A-BBC28B89E6A3}']

    //@property(nonatomic, strong) NSArray *uploadImages;
    procedure setUploadImages(uploadImages: NSArray); cdecl;
    function uploadImages : NSArray; cdecl;

    //@property(nonatomic, strong) NSArray *vkImages;
    procedure setVkImages(vkImages: NSArray); cdecl;
    function vkImages : NSArray; cdecl;

    //@property(nonatomic, strong) VKShareLink *shareLink;
    procedure setShareLink(shareLink: VKShareLink); cdecl;
    function shareLink : VKShareLink; cdecl;

    //@property(nonatomic, copy) NSString *text;
    procedure setText(text: NSString); cdecl;
    function text : NSString; cdecl;

    //@property(nonatomic, strong) NSArray *requestedScope;
    procedure setRequestedScope(requestedScope: NSArray); cdecl;
    function requestedScope : NSArray; cdecl;

    //@property(nonatomic, copy) void (^completionHandler)(VKShareDialogController *dialog, VKShareDialogControllerResult result);
    procedure setCompletionHandler(completionHandler: TVKShareDialogControllerCompletionHandler); cdecl;
    function completionHandler : TVKShareDialogControllerCompletionHandler; cdecl;

    //@property(nonatomic, assign) BOOL dismissAutomatically;
    procedure setDismissAutomatically(dismissAutomatically: Boolean); cdecl;
    function dismissAutomatically : Boolean; cdecl;

    //@property(nonatomic, readonly, copy) NSString *postId;
    function postId : NSString; cdecl;

  end;
  TVKShareDialogController = class(TOCGenericImport<VKShareDialogControllerClass, VKShareDialogController>)  end;

implementation

{$IF defined(CPUARM)}

procedure StubProc1;  cdecl; external 'VKSdkFramework' name 'OBJC_CLASS_$_VKSdk';

{$ELSE}

// i don't know how to do under ios simulator :(

{$ENDIF}

end.
