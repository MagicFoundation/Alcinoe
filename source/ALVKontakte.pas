unit ALVKontakte;

interface

uses
  system.Classes,
  {$IF defined(android)}
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  ALAndroidVKontakteApi,
  {$ELSEIF defined(IOS)}
  Macapi.ObjectiveC,
  iOSapi.UIKit,
  ALIosVKontakteApi,
  {$ENDIF}
  system.Messaging;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALVKontakteLoginOnCancelEvent = procedure of object;
  TALVKontakteLoginOnErrorEvent = procedure(const aMsg: String) of object;
  TALVKontakteLoginOnSuccessEvent = procedure(const aUserID: String; const aEmail: String; const aToken: String) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALVKontakteLogin = class(TObject)
  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TAuthCallback = class(TJavaLocal, JVKAuthCallback)
      private
        [Weak] fVKontakteLogin: TALVKontakteLogin;
      public
        constructor Create(const aVKontakteLogin: TALVKontakteLogin);
        procedure onLogin(token: JVKAccessToken); cdecl;
        procedure onLoginFailed(errorCode: Integer); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(ios)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TSdkDelegate = class(TOCLocal, VKSdkDelegate)
      private
        [Weak] fVKontakteLogin: TALVKontakteLogin;
      public
        constructor Create(const aVKontakteLogin: TALVKontakteLogin);
        procedure vkSdkAccessAuthorizationFinishedWithResult(result: VKAuthorizationResult); cdecl;
        procedure vkSdkUserAuthorizationFailed; cdecl;
        procedure vkSdkAuthorizationStateUpdatedWithResult(result: VKAuthorizationResult); cdecl;
        procedure vkSdkAccessTokenUpdated(newToken: VKAccessToken; oldToken: VKAccessToken); cdecl;
        procedure vkSdkTokenHasExpired(expiredToken: VKAccessToken); cdecl;
      end;

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TSdkUIDelegate = class(TOCLocal, VKSdkUIDelegate)
      private
        [Weak] fVKontakteLogin: TALVKontakteLogin;
      public
        constructor Create(const aVKontakteLogin: TALVKontakteLogin);
        procedure vkSdkShouldPresentViewController(controller: UIViewController); cdecl;
        procedure vkSdkNeedCaptchaEnter(captchaError: VKError); cdecl;
        procedure vkSdkWillDismissViewController(controller: UIViewController); cdecl;
        procedure vkSdkDidDismissViewController(controller: UIViewController); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    FAuthCallback: TAuthCallback;
    procedure ActivityResultHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    fSdkDelegate: TSdkDelegate;
    fSdkUIDelegate: TSdkUIDelegate;
    {$ENDIF}
    {$ENDREGION}

  private

    fOnCancel: TALVKontakteLoginOnCancelEvent;
    fOnError: TALVKontakteLoginOnErrorEvent;
    fOnSuccess: TALVKontakteLoginOnSuccessEvent;

  public
    constructor Create(const aAppId: String); virtual;
    destructor Destroy; override;
    procedure logIn(const aScopes: TArray<String>);
    procedure logout;
    function CurrentUserId: String;
    property onCancel: TALVKontakteLoginOnCancelEvent read fOnCancel write fOnCancel;
    property onError: TALVKontakteLoginOnErrorEvent read fOnError write fOnError;
    property onSuccess: TALVKontakteLoginOnSuccessEvent read fOnSuccess write fOnSuccess;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALVKontakteShareDialog = class(TObject)
  private
  public
    class function ShowShareLinkDialog(const aLinkUrl: String;
                                       const aLinkText: String;
                                       const aLinkImageUrl: String): boolean;
  end;

{**********************************************}
procedure ALInitVKontakte(const aAppId: String);

implementation

uses
  system.SysUtils,
  {$IF defined(android)}
  Androidapi.Helpers,
  Androidapi.JNI.Net,
  Androidapi.JNI.app,
  Androidapi.JNI.GraphicsContentViewText,
  {$ELSEIF defined(IOS)}
  iOSapi.Foundation,
  Macapi.Helpers,
  FMX.Platform,
  FMX.Helpers.iOS,
  FMX.Platform.iOS,
  {$ENDIF}
  ALFmxCommon,
  AlString,
  alcommon;

{*}
var
  _ALVKontakteInitialised: Boolean;

{**********************************************}
procedure ALInitVKontakte(const aAppId: String);
begin
  if not _ALVKontakteInitialised then begin

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    TJVK.JavaClass.initialize(TAndroidHelper.Context);
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(ios)}
    TVKSdk.OCClass.initializeWithAppId(StrToNSStr(aAppId));
    {$ENDIF}
    {$ENDREGION}

    _ALVKontakteInitialised := True;

  end;
end;

{*********************************************************}
constructor TALVKontakteLogin.Create(const aAppId: String);
begin

  inherited Create;

  fOnCancel := nil;
  fOnError := nil;
  fOnSuccess := nil;
  ALInitVKontakte(aAppId);

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, ActivityResultHandler);
    FAuthCallback := TAuthCallback.Create(Self);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    fSdkDelegate := TSdkDelegate.Create(self);
    TVKSdk.OCClass.instance.registerDelegate(fSdkDelegate);
    //----
    fSdkUIDelegate := TSdkUIDelegate.Create(self);
    TVKSdk.OCClass.instance.setUiDelegate(fSdkUIDelegate);

  {$ENDIF}
  {$ENDREGION}

end;

{***********************************}
destructor TALVKontakteLogin.Destroy;
begin

  fOnCancel := nil;
  fOnError := nil;
  fOnSuccess := nil;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, ActivityResultHandler);
    AlFreeAndNil(FAuthCallback);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    TVKSdk.OCClass.instance.unregisterDelegate(fSdkDelegate);
    alFreeAndNil(fSdkDelegate);
    TVKSdk.OCClass.instance.setUiDelegate(nil);
    alFreeAndNil(fSdkUIDelegate);

  {$ENDIF}
  {$ENDREGION}

  inherited Destroy;

end;

{***************************************************************}
procedure TALVKontakteLogin.logIn(const aScopes: TArray<String>);

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  var LArrayList: JArrayList;
      LCollection: JCollection;
      LString: String;
      LScope: JVKScope;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  var LNSPermissions: NSArray;
  {$ENDIF}
  {$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  LArrayList := TJArrayList.JavaClass.init(Length(aScopes));
  for LString in aScopes do begin
    LScope := TJVKScope.JavaClass.valueOf(StringToJString(ALUppercaseU(LString)));
    LArrayList.add(LScope);
  end;
  LCollection := TJCollection.Wrap((LArrayList as ILocalObject).GetObjectID);
  TJVK.JavaClass.login(TAndroidHelper.Activity, LCollection);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

  LNSPermissions := ALStringsToNSArray(aScopes);
  try
    TVKSdk.OCClass.authorize(LNSPermissions);
  finally
    LNSPermissions.release;
  end;

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************}
procedure TALVKontakteLogin.logout;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    TJVK.JavaClass.logout;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

  TVKSdk.OCClass.forceLogout;

  {$ENDIF}
  {$ENDREGION}

end;

{***********************************************}
function TALVKontakteLogin.CurrentUserId: String;

  {$REGION ' IOS'}
  {$IF defined(ios)}
  var LToken: VKAccessToken;
  {$ENDIF}
  {$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  Result := ALIntToStrU(TJVK.JavaClass.getUserId);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

  LToken := TVKSdk.OCClass.accessToken;
  if LToken = nil then result := ''
  else result := NSStrToStr(LToken.userID)

  {$ENDIF}
  {$ENDREGION}

end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{******************************************************************************************}
procedure TALVKontakteLogin.ActivityResultHandler(const Sender: TObject; const M: TMessage);
begin

  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.ActivityResultHandler', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if M is TMessageResultNotification then begin
    TJVK.javaclass.onActivityResult(
      TMessageResultNotification(M).RequestCode,
      TMessageResultNotification(M).ResultCode,
      TMessageResultNotification(M).Value,
      FAuthCallback);
  end;

end;

{*******************************************************************************************}
constructor TALVKontakteLogin.TAuthCallback.Create(const aVKontakteLogin: TALVKontakteLogin);
begin
  inherited Create;
  fVKontakteLogin := aVKontakteLogin;
end;

{***********************************************************************}
procedure TALVKontakteLogin.TAuthCallback.onLogin(token: JVKAccessToken);
var LTokenStr: String;
    LUserID: Integer;
    LEmail: String;
begin

  if (token <> nil) and (token.isValid) then begin
    LUserID := Token.getUserId;
    LEmail := JStringToString(Token.getEmail);
    LTokenStr := JStringToString(Token.getAccessToken);
  end
  else begin
    LUserID := 0;
    LEmail := '';
    LTokenStr := '';
  end;
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TAuthCallback.onSuccess', 'UserID: ' + ALintToStrU(LUserID) +
                                                     ' - Email: ' + LEmail +
                                                     ' - Token: ' + LTokenStr +
                                                     ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.info);
  {$ENDIF}

  if assigned(fVKontakteLogin.fOnsuccess) then
    fVKontakteLogin.fOnsuccess(ALinttoStrU(LUserID), LEmail, LTokenStr);

end;

{**************************************************************************}
procedure TALVKontakteLogin.TAuthCallback.onLoginFailed(errorCode: Integer);
begin

  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TAuthCallback.onError', 'ErrorCode: ' + ALIntToStrU(errorCode) +
                                                   ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}

  if errorCode = TJVKAuthCallback.javaclass.AUTH_CANCELED then begin
    if assigned(fVKontakteLogin.fOnCancel) then
      fVKontakteLogin.fOnCancel;
  end
  else begin
    if assigned(fVKontakteLogin.fOnError) then
      fVKontakteLogin.fOnError('');
  end;

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{******************************************************************************************}
constructor TALVKontakteLogin.TSdkDelegate.Create(const aVKontakteLogin: TALVKontakteLogin);
begin
  inherited Create;
  fVKontakteLogin := aVKontakteLogin;
end;

{*****************************************************************************************************}
//Notifies about authorization was completed, and returns authorization result with new token or error.
//@param result contains new token or error, retrieved after VK authorization.
procedure TALVKontakteLogin.TSdkDelegate.vkSdkAccessAuthorizationFinishedWithResult(result: VKAuthorizationResult);
begin

  {$IFDEF DEBUG}
  if (result <> nil) and (result.token <> nil) then
    allog('TALVKontakteLogin.TSdkDelegate.vkSdkAccessAuthorizationFinishedWithResult.onSuccess', 'UserID: ' + NSStrToStr(result.token.userId) +
                                                                                                 ' - Email: ' + NSStrToStr(result.token.Email) +
                                                                                                 ' - Token: ' + NSStrToStr(result.token.accessToken) +
                                                                                                 ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose)
  else if (result <> nil) and (result.error <> nil) then
    allog('TALVKontakteLogin.TSdkDelegate.vkSdkAccessAuthorizationFinishedWithResult.onError', 'ErrorCode: ' + ALIntToStrU(result.error.code) +
                                                                                               ' - ErrorDescription: ' + NSStrToStr(result.error.localizedDescription) +
                                                                                               ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error)
  else
    allog('TALVKontakteLogin.TSdkDelegate.vkSdkAccessAuthorizationFinishedWithResult.onError', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}

  if (result <> nil) and (result.token <> nil) then begin
    if assigned(fVKontakteLogin.fOnsuccess) then
      fVKontakteLogin.fOnsuccess(NSStrToStr(result.token.userId),
                                 NSStrToStr(result.token.Email),
                                 NSStrToStr(result.token.accessToken));
  end
  else if (result <> nil) and (result.error <> nil) then begin
    if assigned(fVKontakteLogin.fOnError) then
      fVKontakteLogin.fOnError(NSStrToStr(result.error.localizedDescription){aMsg})
  end
  else begin
    if assigned(fVKontakteLogin.fOnError) then
      fVKontakteLogin.fOnError('');
  end;

end;

{***********************************************************************************************************}
//Notifies about access error. For example, this may occurs when user rejected app permissions through VK.com
procedure TALVKontakteLogin.TSdkDelegate.vkSdkUserAuthorizationFailed;
begin
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TSdkDelegate.vkSdkUserAuthorizationFailed', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{*********************************************************************************************************}
//Notifies about authorization state was changed, and returns authorization result with new token or error.
//If authorization was successfull, also contains user info.
//@param result contains new token or error, retrieved after VK authorization
procedure TALVKontakteLogin.TSdkDelegate.vkSdkAuthorizationStateUpdatedWithResult(result: VKAuthorizationResult);
begin
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TSdkDelegate.vkSdkAuthorizationStateUpdatedWithResult', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{********************************************}
//Notifies about access token has been changed
//@param newToken new token for API requests
//@param oldToken previous used token
procedure TALVKontakteLogin.TSdkDelegate.vkSdkAccessTokenUpdated(newToken: VKAccessToken; oldToken: VKAccessToken);
begin
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TSdkDelegate.vkSdkAccessTokenUpdated', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{**********************************************************************************************************************}
//Notifies about existing token has expired (by timeout). This may occurs if you requested token without no_https scope.
//@param expiredToken old token that has expired.
procedure TALVKontakteLogin.TSdkDelegate.vkSdkTokenHasExpired(expiredToken: VKAccessToken);
begin
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TSdkDelegate.vkSdkTokenHasExpired', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{********************************************************************************************}
constructor TALVKontakteLogin.TSdkUIDelegate.Create(const aVKontakteLogin: TALVKontakteLogin);
begin
  inherited Create;
  fVKontakteLogin := aVKontakteLogin;
end;

{*********************************************************************************************}
//Pass view controller that should be presented to user. Usually, it's an authorization window.
//@param controller view controller that must be shown to user
procedure TALVKontakteLogin.TSdkUIDelegate.vkSdkShouldPresentViewController(controller: UIViewController);
var LWindow: UIWindow;
begin
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TSdkUIDelegate.vkSdkShouldPresentViewController', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
  LWindow := SharedApplication.keyWindow;
  if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
    LWindow.rootViewController.presentViewController(controller, //viewControllerToPresent: UIViewController;
                                                     true, // animated: Boolean;
                                                     nil); // completion: TOnUIViewControllerCompletion)
end;

{*******************************************}
//Calls when user must perform captcha-check.
//If you implementing this method by yourself, call -[VKError answerCaptcha:] method for captchaError with user entered answer.
//@param captchaError error returned from API. You can load captcha image from <b>captchaImg</b> property.
procedure TALVKontakteLogin.TSdkUIDelegate.vkSdkNeedCaptchaEnter(captchaError: VKError);
begin
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TSdkUIDelegate.vkSdkNeedCaptchaEnter', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{************************************************************}
//Called when a controller presented by SDK will be dismissed.
procedure TALVKontakteLogin.TSdkUIDelegate.vkSdkWillDismissViewController(controller: UIViewController);
begin
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TSdkUIDelegate.vkSdkWillDismissViewController', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{******************************************************}
//Called when a controller presented by SDK did dismiss.
procedure TALVKontakteLogin.TSdkUIDelegate.vkSdkDidDismissViewController(controller: UIViewController);
begin
  {$IFDEF DEBUG}
  allog('TALVKontakteLogin.TSdkUIDelegate.vkSdkDidDismissViewController', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}
end;

{$ENDIF}
{$ENDREGION}

{********************************************************************************}
class function TALVKontakteShareDialog.ShowShareLinkDialog(const aLinkUrl: String;
                                                           const aLinkText: String;
                                                           const aLinkImageUrl: String): boolean;
begin

  // the VK sdk api is a TRUE PIECE OF SHEET regarding
  // the share of a link! first it is complicate like hell
  // to implemente (and I think even not possible in android),
  // but at the end it's simply an ugly interface! if we want
  // to share a link via the sdk api, then it's will just
  // show a dialog with a link: toto.com NOTHING ELSE! no picture
  // taken from the og: tag in the destination url. You can
  // attach an image but in this way totally stupid because
  // in the wall of the user you will see the post with the
  // image you just attached, but also with the image coming
  // from the OG: tag of the destination url !! STUPID !!
  // So i use instead the http://vk.com/share.php to
  // share a link, on ios seam to always use the browser and
  // on android it's ask you to use the browser OR the vk app
  // if the app was installed. At least you don't have to
  // request wall access for the user

  //-----
  {$IF (not defined(android)) and (not defined(ios))}
  result := false;
  {$ENDIF}

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    // https://vk.com/dev/widget_share
    // https://vk.com/dev/pages.clearCache

    result := True;
    var LIntent := TJIntent.Create;
    LIntent.setAction(TJIntent.JavaClass.ACTION_VIEW);
    LIntent.setData(
      StrToJURI(
        'http://vk.com/share.php?'+
          'url=' + ALHttpEncodeU(aLinkUrl)+'&'+    // (string) - URL of the page, the link to which should be published
          'title=' + ALHttpEncodeU(aLinkText)+'&'+ // (string) - the title of the publication. If the parameter is not specified, the title
                                                   //            will be taken from the publication page automatically
          'image='+ALHttpEncodeU(aLinkImageUrl))); // (string) - URL of the image to publish. If the parameter is not specified, the image
                                                   //            will be taken from the publication page automatically
          //'&noparse='                            // (boolean) - if true is specified in this parameter , the VKontakte server will not make
                                                   //             an additional request to download the missing information from the published page. If
                                                   //             false , the request will always be sent.
    TAndroidHelper.Activity.startActivity(LIntent);
    LIntent := nil;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    // https://vk.com/dev/widget_share
    // https://vk.com/dev/pages.clearCache

    result := True;
    var LURL := StrToNSUrl(
                  'http://vk.com/share.php?'+
                    'url=' + ALHttpEncodeU(aLinkUrl)+'&'+    // (string) - URL of the page, the link to which should be published
                    'title=' + ALHttpEncodeU(aLinkText)+'&'+ // (string) - the title of the publication. If the parameter is not specified, the title
                                                             //            will be taken from the publication page automatically
                    'image='+ALHttpEncodeU(aLinkImageUrl));  // (string) - URL of the image to publish. If the parameter is not specified, the image
                                                             //            will be taken from the publication page automatically
                    //'&noparse='                            // (boolean) - if true is specified in this parameter , the VKontakte server will not make
                                                             //             an additional request to download the missing information from the published page. If
                                                             //             false , the request will always be sent.
    if SharedApplication.canOpenURL(LURL) then SharedApplication.openUrl(LURL)
    else result := false;

    //var LShareDialogController := TVKShareDialogController.Wrap(TVKShareDialogController.OCClass.alloc);
    //try
    //  LShareDialogController := TVKShareDialogController.Wrap(LShareDialogController.init);
    //  var LURL := TNSUrl.Wrap(TNSUrl.OCClass.URLWithString(StrToNSStr(aLinkUrl)));
    //  try
    //    var LShareLink := TVKShareLink.Wrap(TVKShareLink.OCClass.alloc);
    //    try
    //      LShareLink := LShareLink.initWithTitle(StrToNSStr(aLinkText), LURL);
    //      LShareDialogController.setshareLink(LShareLink);
    //      LShareDialogController.setDismissAutomatically(true);
    //      var LWindow := SharedApplication.keyWindow;
    //      if (LWindow <> nil) and (LWindow.rootViewController <> nil) then
    //        LWindow.rootViewController.presentViewController(LShareDialogController, //viewControllerToPresent: UIViewController;
    //                                                         true, // animated: Boolean;
    //                                                         nil); // completion: TOnUIViewControllerCompletion)
    //    finally
    //      LShareLink.release;
    //    end;
    //  finally
    //    //LURL.release; access violation if i do so
    //  end;
    //finally
    //  LShareDialogController.release;
    //end;

  {$ENDIF}
  {$ENDREGION}

end;


{$REGION ' IOS'}
{$IF defined(IOS)}

Type

  {*******************************************}
  //if i don't don't this i have internal error
  _TVKontakteProcOfObjectWrapper = class(Tobject)
  public
    class procedure ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
  end;

{**********************************************************************************************************************}
class procedure _TVKontakteProcOfObjectWrapper.ApplicationEventMessageHandler(const Sender: TObject; const M: TMessage);
begin
  if not _ALVKontakteInitialised then exit;
  if M is TApplicationEventMessage then begin
    var LValue := (M as TApplicationEventMessage).value;
    if LValue.Event = TApplicationEvent.OpenURL then begin

      var Lcontext := TiOSOpenApplicationContext(LValue.Context);
      {$IFDEF DEBUG}
      ALLog('_TVKontakteProcOfObjectWrapper.ApplicationEventMessageHandler', 'Url: ' + Lcontext.URL);
      {$ENDIF}

      //application:openURL:options: (iOS 9 and up)
      If TOSVersion.Check(9) then begin
        if (Lcontext.Context <> nil) then begin
          var LPointer := TNSDictionary.Wrap(Lcontext.Context).valueForKey(UIApplicationOpenURLOptionsSourceApplicationKey);
          if LPointer <> nil then TVKSdk.OCClass.processOpenURL(StrToNSUrl(Lcontext.Url), TNSString.Wrap(LPointer))
          else TVKSdk.OCClass.processOpenURL(StrToNSUrl(Lcontext.Url), nil);
        end;
      end

      //application:openURL:sourceApplication:annotation: (iOS 8 and older)
      else begin
        TVKSdk.OCClass.processOpenURL(StrToNSUrl(Lcontext.Url), StrToNSStr(Lcontext.SourceApp));
      end;

    end;
  end;
end;

{$ENDIF}
{$ENDREGION}

initialization

  _ALVKontakteInitialised := False;

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, _TVKontakteProcOfObjectWrapper.ApplicationEventMessageHandler);
  {$ENDIF}
  {$ENDREGION}


finalization

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, _TVKontakteProcOfObjectWrapper.ApplicationEventMessageHandler);
  {$ENDIF}
  {$ENDREGION}

end.
