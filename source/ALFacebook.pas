{*****************************************************
This unit was (originaly) inspired by
https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
https://blog.grijjy.com/2017/01/30/embed-facebook-sdk-for-android-in-your-delphi-mobile-app-part-2/
*****************************************************}

unit ALFacebook;

interface

uses system.Classes,
     {$IF defined(android)}
     Androidapi.JNI.JavaTypes,
     Androidapi.JNIBridge,
     ALAndroidFacebookApi,
     {$ELSEIF defined(IOS)}
     iOSapi.Foundation,
     ALIosFacebookApi,
     {$ENDIF}
     system.Messaging;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookLoginOnCancelEvent = procedure of object;
  TALFacebookLoginOnErrorEvent = procedure(const aMsg: String) of object;
  TALFacebookLoginOnSuccessEvent = procedure(const aUserID: String; const aToken: String; const AGrantedPermissions: TArray<String>; const ADeniedPermissions: TArray<String>) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookLogin = class(TObject)
  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TLoginCallback = class(TJavaLocal, JFacebookCallback)
      private
        [Weak] fFacebookLogin: TALFacebookLogin;
      public
        constructor Create(const aFacebookLogin: TALFacebookLogin);
        procedure onCancel; cdecl;
        procedure onError(error: JFacebookException); cdecl;
        procedure onSuccess(result: JObject); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    FCallback: TLoginCallback;
    FCallbackManager: JCallbackManager;
    procedure ActivityResultHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    FLoginManager: FBSDKLoginManager;
    procedure logInWithReadPermissionsHandler(result: FBSDKLoginManagerLoginResult; error: NSError);
    {$ENDIF}
    {$ENDREGION}

  private

    fIsRunning: boolean;
    fOnCancel: TALFacebookLoginOnCancelEvent;
    fOnError: TALFacebookLoginOnErrorEvent;
    fOnSuccess: TALFacebookLoginOnSuccessEvent;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure logInWithReadPermissions(const APermissions: TArray<String>);
    procedure logout;
    function CurrentToken: String;
    function CurrentUserId: String;
    function CurrentGrantedPermissions: TArray<String>;
    function CurrentDeniedPermissions: TArray<String>;
    property onCancel: TALFacebookLoginOnCancelEvent read fOnCancel write fOnCancel;
    property onError: TALFacebookLoginOnErrorEvent read fOnError write fOnError;
    property onSuccess: TALFacebookLoginOnSuccessEvent read fOnSuccess write fOnSuccess;
    property isRunning: boolean read fIsRunning;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookGraphRequestOnCompletedEvent = procedure(const aResponse: string; Const aErrorCode: integer; Const aErrorMsg: String) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookGraphRequest = class(TObject)
  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    type

      {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
      TGraphRequestCallback = class(TJavaLocal, JGraphRequest_Callback)
      private
        [Weak] fFacebookGraphRequest: TALFacebookGraphRequest;
      public
        constructor Create(const aFacebookGraphRequest: TALFacebookGraphRequest);
        procedure onCompleted(response: JGraphResponse); cdecl;
      end;

    {$ENDIF}
    {$ENDREGION}

  private

    {$REGION ' ANDROID'}
    {$IF defined(android)}
    FCallback: TGraphRequestCallback;
    {$ENDIF}
    {$ENDREGION}

    {$REGION ' IOS'}
    {$IF defined(IOS)}
    procedure GraphRequestCompletionHandler(connection: FBSDKGraphRequestConnection; result: Pointer; error: NSError);
    {$ENDIF}
    {$ENDREGION}

  private

    fIsRunning: Boolean;
    fOnCompleted: TALFacebookGraphRequestOnCompletedEvent;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Request(const aGraphPath: String; aParameters: Tarray<String>; const aHttpMethod: string = 'GET');
    property onCompleted: TALFacebookGraphRequestOnCompletedEvent read fOnCompleted write fOnCompleted;
    property isRunning: boolean read fIsRunning;
  end;


implementation

uses system.SysUtils,
     {$IF defined(android)}
     Androidapi.Helpers,
     Androidapi.JNI.app,
     Androidapi.JNI.Os,
     FMX.Helpers.Android,
     {$ELSEIF defined(IOS)}
     Macapi.Helpers,
     FMX.Platform.iOS,
     {$ENDIF}
     ALFmxCommon,
     AlString,
     alcommon;

{**********************************}
constructor TALFacebookLogin.Create;
begin

  inherited Create;

  fOnCancel := nil;
  fOnError := nil;
  fOnSuccess := nil;
  fIsRunning := False;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, ActivityResultHandler);
    FCallback := TLoginCallback.Create(Self);

    FCallbackManager := TJCallbackManager_Factory.JavaClass.create;
    TJloginManager.JavaClass.getInstance.registerCallback(FCallbackManager, FCallback);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    FLoginManager := TFBSDKLoginManager.Create; // https://stackoverflow.com/questions/42222508/why-we-need-to-do-retain-for-objective-c-object-field
                                                // In general, if you create such a class with Create, alloc, copy, mutableCopy, new... then retain
                                                // is called for you. Then you need to call release or autorelease.

  {$ENDIF}
  {$ENDREGION}

end;

{****************************************************************}
//if we launch logInWithReadPermissions we must wait it's finished
//before to destroy the facebooklogin
destructor TALFacebookLogin.Destroy;
begin

  fOnCancel := nil;
  fOnError := nil;
  fOnSuccess := nil;
  while fIsRunning do sleep(100);

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, ActivityResultHandler);
    AlFreeAndNil(FCallback);
    FCallbackManager := nil;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    FLoginManager.release;

  {$ENDIF}
  {$ENDREGION}

  inherited Destroy;

end;

{********************************************************}
//Logs the user in with the requested publish permissions.
//Permissions Reference: https://developers.facebook.com/docs/facebook-login/permissions#reference-public_profile
procedure TALFacebookLogin.logInWithReadPermissions(const APermissions: TArray<String>);

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  var aArrayList: JArrayList;
      aCollection: JCollection;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  var ANSPermissions: NSArray;
  {$ENDIF}
  {$ENDREGION}

begin

  if fIsRunning then raise Exception.Create('Already Running');
  fIsRunning := true;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  aArrayList := ALStringsToJArrayList(APermissions);
  aCollection := TJCollection.Wrap((aArrayList as ILocalObject).GetObjectID);
  TJloginManager.JavaClass.getInstance.logInWithReadPermissions(TAndroidHelper.Activity, aCollection);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

  ANSPermissions := ALStringsToNSArray(APermissions);
  try
    FLoginManager.logInWithReadPermissions(ANSPermissions, nil, logInWithReadPermissionsHandler);
  finally
    ANSPermissions.release;
  end;

  {$ENDIF}
  {$ENDREGION}

end;

{********************************}
procedure TALFacebookLogin.logout;
begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    TJloginManager.JavaClass.getInstance.logout;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    FloginManager.logout;

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************}
function TALFacebookLogin.CurrentToken: String;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  var aToken: JAccessToken;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}
  var aToken: FBSDKAccessToken;
  {$ENDIF}
  {$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    aToken := TJAccessToken.JavaClass.getCurrentAccessToken;
    if aToken = nil then Result := ''
    else Result := JStringToString(aToken.GetToken);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    aToken := TFBSDKAccessToken.OCClass.currentAccessToken;
    if aToken = nil then result := ''
    else result := NSStrToStr(aToken.tokenString);

  {$ENDIF}
  {$ENDREGION}

end;

{**********************************************}
function TALFacebookLogin.CurrentUserId: String;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  var aToken: JAccessToken;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}
  var aToken: FBSDKAccessToken;
  {$ENDIF}
  {$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    aToken := TJAccessToken.JavaClass.getCurrentAccessToken;
    if aToken = nil then Result := ''
    else Result := JStringToString(aToken.getUserId);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    aToken := TFBSDKAccessToken.OCClass.currentAccessToken;
    if aToken = nil then result := ''
    else result := NSStrToStr(aToken.userID)

  {$ENDIF}
  {$ENDREGION}

end;

{******************************************************************}
function TALFacebookLogin.CurrentGrantedPermissions: TArray<String>;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  var aToken: JAccessToken;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}
  var aToken: FBSDKAccessToken;
  {$ENDIF}
  {$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    aToken := TJAccessToken.JavaClass.getCurrentAccessToken;
    if aToken = nil then setlength(Result, 0)
    else Result := ALJsetToStrings(aToken.getPermissions);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    aToken := TFBSDKAccessToken.OCClass.currentAccessToken;
    if aToken = nil then setlength(result, 0)
    else result := ALNSSetToStrings(aToken.permissions);

  {$ENDIF}
  {$ENDREGION}

end;

{*****************************************************************}
function TALFacebookLogin.CurrentDeniedPermissions: TArray<String>;

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  var aToken: JAccessToken;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(ios)}
  var aToken: FBSDKAccessToken;
  {$ENDIF}
  {$ENDREGION}

begin

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    aToken := TJAccessToken.JavaClass.getCurrentAccessToken;
    if aToken = nil then setlength(Result, 0)
    else Result := ALJsetToStrings(aToken.getDeclinedPermissions);

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

    aToken := TFBSDKAccessToken.OCClass.currentAccessToken;
    if aToken = nil then setlength(result, 0)
    else result := ALNSSetToStrings(aToken.declinedPermissions)

  {$ENDIF}
  {$ENDREGION}

end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{*********************************************************************************************}
//in onActivityResult() forward the login results to the callbackManager created in onCreate():
//https://developers.facebook.com/docs/facebook-login/android
procedure TALFacebookLogin.ActivityResultHandler(const Sender: TObject; const M: TMessage);
begin

  if not fIsRunning then exit;

  {$IFDEF DEBUG}
  allog('TALFacebookLogin.ActivityResultHandler', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.VERBOSE);
  {$ENDIF}

  if M is TMessageResultNotification then begin
    FCallbackManager.onActivityResult(TMessageResultNotification(M).RequestCode, TMessageResultNotification(M).ResultCode, TMessageResultNotification(M).Value);
  end;

end;

{*****************************************************************************************}
constructor TALFacebookLogin.TLoginCallback.Create(const aFacebookLogin: TALFacebookLogin);
begin
  inherited Create;
  fFacebookLogin := aFacebookLogin;
end;

{**********}
//onCancel()
//Called when the dialog is canceled.
//Note: FacebookCallback.onSuccess(Object) will be called instead if any of the following conditions are true.
//MessageDialog is used.
//The logged in Facebook user has not authorized the app that has initiated the dialog.
procedure TALFacebookLogin.TLoginCallback.onCancel;
begin

  {$IFDEF DEBUG}
  allog('TALFacebookLogin.TLoginCallback.onCancel', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.warn);
  {$ENDIF}

  fFacebookLogin.fIsRunning := False;
  if assigned(fFacebookLogin.fOnCancel) then
    fFacebookLogin.fOnCancel;

end;

{**************************}
//onError(FacebookException)
//Called when the dialog finishes with an error.
procedure TALFacebookLogin.TLoginCallback.onError(error: JFacebookException);
var aErrorMsg: String;
begin

  if error <> nil then aErrorMsg := JStringToString(error.toString)
  else aErrorMsg := '';
  {$IFDEF DEBUG}
  allog('TALFacebookLogin.TLoginCallback.onError', 'Error: ' + aErrorMsg +
                                                   ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
  {$ENDIF}

  fFacebookLogin.fIsRunning := False;
  if assigned(fFacebookLogin.fOnError) then
    fFacebookLogin.fOnError(aErrorMsg{aMsg});

end;

{*****************}
//onSuccess(Object)
//Called when the dialog completes without error.
//Note: This will be called instead of FacebookCallback.onCancel() if any of the following conditions are true.
// * MessageDialog is used.
// * The logged in Facebook user has not authorized the app that has initiated the dialog.
procedure TALFacebookLogin.TLoginCallback.onSuccess(result: JObject);
var aLoginResult: JLoginResult;
    aToken: JAccessToken;
    aTokenStr: String;
    aUserIDStr: String;
    aGrantedPermissions: TArray<String>;
    aDeniedPermissions: TArray<String>;
begin

  aLoginResult := TJLoginResult.Wrap((result as ILocalObject).GetObjectID);
  aToken := aLoginResult.getAccessToken;
  if aToken <> nil then begin
    aUserIDStr := JStringToString(aToken.getUserId);
    aTokenStr := JStringToString(aToken.getToken);
    aGrantedPermissions := ALJSetToStrings(aToken.getPermissions);
    aDeniedPermissions := ALJSetToStrings(aToken.getDeclinedPermissions);
  end
  else begin
    aUserIDStr := '';
    aTokenStr := '';
    setlength(aGrantedPermissions, 0);
    setlength(aDeniedPermissions, 0);
  end;
  {$IFDEF DEBUG}
  allog('TALFacebookLogin.TLoginCallback.onSuccess', 'UserID: ' + aUserIDStr +
                                                     ' - Token: ' + aTokenStr +
                                                     ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.info);
  {$ENDIF}

  fFacebookLogin.fIsRunning := False;
  if assigned(fFacebookLogin.fOnsuccess) then
    fFacebookLogin.fOnsuccess(aUserIDStr, aTokenStr, aGrantedPermissions, aDeniedPermissions);

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{***************************************************************************************************************}
procedure TALFacebookLogin.logInWithReadPermissionsHandler(result: FBSDKLoginManagerLoginResult; error: NSError);
var aToken: FBSDKAccessToken;
    aTokenStr: String;
    aUserIDStr: String;
    aGrantedPermissions: TArray<String>;
    aDeniedPermissions: TArray<String>;
begin

  fIsRunning := False;

  //ERROR
  if (error <> nil) or (result = nil) then begin

    {$IFDEF DEBUG}
    if error <> nil then allog('TALFacebookLogin.logInWithReadPermissionsHandler.onError', NSStrToStr(error.localizedDescription) +
                                                                                           ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error)
    else allog('TALFacebookLogin.logInWithReadPermissionsHandler.onError', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.error);
    {$ENDIF}

    if assigned(fOnError) then begin
      if error <> nil then fOnError(NSStrToStr(error.localizedDescription){aMsg})
      else fOnError(''{aMsg});
    end;

  end

  //CANCELED
  else if result.isCancelled then begin

    {$IFDEF DEBUG}
    allog('TALFacebookLogin.logInWithReadPermissionsHandler.onCancel', 'ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.warn);
    {$ENDIF}

    if assigned(fOnCancel) then
      fOnCancel;

  end

  //SUCCESS
  else begin

    aToken := result.token;
    if aToken <> nil then begin
      aUserIDStr := NSStrToStr(aToken.UserId);
      aTokenStr := NSStrToStr(aToken.TokenString);
      aGrantedPermissions := ALNSSetToStrings(aToken.Permissions);
      aDeniedPermissions := ALNSSetToStrings(aToken.DeclinedPermissions);
    end
    else begin
      aUserIDStr := '';
      aTokenStr := '';
      setlength(aGrantedPermissions, 0);
      setlength(aDeniedPermissions, 0);
    end;

    {$IFDEF DEBUG}
    allog('TALFacebookLogin.logInWithReadPermissionsHandler.onSuccess', 'UserID: ' + aUserIDStr +
                                                                        ' - Token: ' + aTokenStr +
                                                                        ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.info);
    {$ENDIF}

    if assigned(fOnsuccess) then
      fOnsuccess(aUserIDStr, aTokenStr, aGrantedPermissions, aDeniedPermissions);

  end;

end;

{$ENDIF}
{$ENDREGION}

{*****************************************}
constructor TALFacebookGraphRequest.Create;
begin

  inherited Create;

  fIsRunning := False;
  fonCompleted := nil;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    FCallback := TGraphRequestCallback.Create(Self);

  {$ENDIF}
  {$ENDREGION}

end;

{*************************************************}
//if we launch a request we must wait it's finished
//before to destroy the TALFacebookGraphRequest
destructor TALFacebookGraphRequest.Destroy;
begin

  fOnCompleted := nil;
  while fIsRunning do sleep(100);

  {$REGION ' ANDROID'}
  {$IF defined(android)}

    AlFreeAndNil(FCallback);

  {$ENDIF}
  {$ENDREGION}

  inherited Destroy;

end;

{******************************************************************************}
Function _ExtractNameValue(const S: String; var aName, aValue: String): Boolean;
Var P1: Integer;
begin
  P1 := AlPosU('=',S);
  if P1 > 0 then begin
    result := True;
    aName := AlCopyStrU(S,1,P1-1);
    aValue := AlCopyStrU(S, P1+1, maxint);
  end
  else begin
    Result := False;
    aName := S;
    aValue := '';
  end;
end;

{********************************************************}
//https://developers.facebook.com/docs/graph-api/reference
//Note: The /me node is a special endpoint that translates to the user_id of the person (or the page_id of the Facebook Page)
//      whose access token is currently being used to make the API calls
procedure TALFacebookGraphRequest.Request(const aGraphPath: String; aParameters: Tarray<String>; const aHttpMethod: string = 'GET');

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  var aGraphRequest: FBSDKGraphRequest;
      aNSDictParameters: NSMutableDictionary;
      aName, aValue: String;
      i : integer;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' ANDROID'}
  {$IF defined(android)}
  var aBundle: JBundle;
      aName, aValue: String;
      aJName, aJValue: JString;
      aGraphRequest: JGraphRequest;
      aJHttpMethod: JHttpMethod;
      i : integer;
  {$ENDIF}
  {$ENDREGION}

begin

  if fIsRunning then raise Exception.Create('Already Running');
  fIsRunning := True;

  {$REGION ' ANDROID'}
  {$IF defined(android)}

  aBundle := TJBundle.JavaClass.init;
  for i := Low(aparameters) to High(aparameters) do begin
    _ExtractNameValue(aparameters[i], aName, aValue);
    aJName := StringToJstring(aName);
    aJValue := StringToJstring(aValue);
    aBundle.putString(aJName, aJValue);
    aJName := nil;  // << because of
    aJValue := nil; // << https://quality.embarcadero.com/browse/RSP-14187
  end;
  if AlSameTextU(aHttpMethod, 'POST') then aJHttpMethod := TJHttpMethod.JavaClass.POST
  else aJHttpMethod := TJHttpMethod.JavaClass.GET;
  aGraphRequest := TJGraphRequest.JavaClass.init(TJAccessToken.JavaClass.getCurrentAccessToken, StringToJstring(aGraphPath), aBundle, aJHttpMethod, FCallback);
  aGraphRequest.executeAsync;

  {$ENDIF}
  {$ENDREGION}

  {$REGION ' IOS'}
  {$IF defined(IOS)}

  aNSDictParameters := TNSMutableDictionary.Create;
  try
    for i := Low(aparameters) to High(aparameters) do begin
      _ExtractNameValue(aparameters[i], aName, aValue);
      aNSDictParameters.setValue(StringToID(aName), StrToNsStr(aValue));
    end;
    aGraphRequest := TFBSDKGraphRequest.Wrap(TFBSDKGraphRequest.Alloc.initWithGraphPath(StrToNSStr(aGraphPath), // graphPath: NSString;
                                                                                        aNSDictParameters, // parameters: NSDictionary;
                                                                                        StrToNSStr(aHTTPMethod)));//HTTPMethod: NSString
  finally
    aNSDictParameters.release;
  end;
  aGraphRequest.startWithCompletionHandler(GraphRequestCompletionHandler);
  aGraphRequest.release; // https://stackoverflow.com/questions/44176681/delphi-ios-release-retain-and-reference-counting-with-objective-c-object

  {$ENDIF}
  {$ENDREGION}


end;

{$REGION ' ANDROID'}
{$IF defined(android)}

{*********************************************************************************************************************}
constructor TALFacebookGraphRequest.TGraphRequestCallback.Create(const aFacebookGraphRequest: TALFacebookGraphRequest);
begin
  inherited Create;
  fFacebookGraphRequest := aFacebookGraphRequest;
end;

{*****************************************************}
//when their is an error, it's return an empty response
procedure TALFacebookGraphRequest.TGraphRequestCallback.onCompleted(response: JGraphResponse);
var aRawResponse: String;
    aErrorCode: Integer;
    aErrorMsg: String;
begin

  aRawResponse := '';
  aErrorCode := 0;
  aErrorMsg := '';
  if response <> nil then begin
    aRawResponse := JstringToString(response.getRawResponse);
    if (response.getError <> nil) then begin
      aErrorCode := response.getError.getErrorCode;
      aErrorMsg := JStringToString(response.getError.getErrorMessage);
    end;
  end;
  {$IFDEF DEBUG}
  allog('TALFacebookGraphRequest.TGraphRequestCallback.onCompleted', 'response: ' + aRawResponse +
                                                                     ' - ErrorCode: ' + alinttoStrU(aErrorCode) +
                                                                     ' - ErrorMsg: ' + aErrorMsg +
                                                                     ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  fFacebookGraphRequest.fIsRunning := False;
  if assigned(fFacebookGraphRequest.fOnCompleted) then
    fFacebookGraphRequest.fOnCompleted(aRawResponse, aErrorCode, aErrorMsg);

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{****************************************************************************************************************************************}
procedure TALFacebookGraphRequest.GraphRequestCompletionHandler(connection: FBSDKGraphRequestConnection; result: Pointer; error: NSError);
var aRawResponse: String;
    aErrorCode: integer;
    aErrorMsg: String;
    aJsonErr: NSError;
    aJsonData: NSData;
    aNSString: NSString;
begin

  aRawResponse := '';
  aErrorCode := 0;
  aErrorMsg := '';
  if error <> nil then begin
    aErrorCode := error.code;
    aErrorMsg := NSStrToStr(error.localizedDescription);
  end
  else if result <> nil then begin
    aJsonData := TNSJSONSerialization.OCClass.dataWithJSONObject(result, 0, Addr(aJsonErr));
    if (aJsonData <> nil) and (aJsonErr = nil) then begin
      aNSString := TNSString.Wrap(TNSString.Alloc.initWithData(aJsonData, NSUTF8StringEncoding));
      try
        aRawResponse := NSStrToStr(aNSString);
      finally
        aNSString.release;
      end;
    end;
  end;

  {$IFDEF DEBUG}
  allog('TALFacebookGraphRequest.TGraphRequestCallback.onCompleted', 'response: ' + aRawResponse +
                                                                     ' - ErrorCode: ' + alinttoStrU(aErrorCode) +
                                                                     ' - ErrorMsg: ' + aErrorMsg +
                                                                     ' - ThreadID: ' + alIntToStrU(TThread.Current.ThreadID) + '/' + alIntToStrU(MainThreadID), TalLogType.verbose);
  {$ENDIF}

  fIsRunning := False;
  if assigned(fOnCompleted) then
    fOnCompleted(aRawResponse, aErrorCode, aErrorMsg);

end;

{$ENDIF}
{$ENDREGION}


{$REGION ' IOS'}
{$IF defined(IOS)}

Type

  {*******************************************}
  //if i don't don't this i have internal error
  _TProcOfObjectWrapper = class(Tobject)
  public
    class procedure applicationDidFinishLaunchingWithOptionsHandler(const Sender: TObject; const M: TMessage);
    class procedure applicationOpenURLWithSourceAnnotationHandler(const Sender: TObject; const M: TMessage);
    class procedure applicationOpenURLWithOptionsHandler(const Sender: TObject; const M: TMessage);
  end;

{****************************************************************************}
// To bypass compile errors you must add a custom modified FMX.Platform.iOS to
// your project, see the instructions at https://github.com/grijjy/DelphiSocialFrameworks for more details
class procedure _TProcOfObjectWrapper.applicationDidFinishLaunchingWithOptionsHandler(const Sender: TObject; const M: TMessage);
var aFBSDKApplicationDelegate: FBSDKApplicationDelegate;
    aValue: TAppDelegate_applicationDidFinishLaunchingWithOptions;
begin
  if M is TAppDelegateMessage_applicationDidFinishLaunchingWithOptions then begin
    aValue := (M as TAppDelegateMessage_applicationDidFinishLaunchingWithOptions).Value;
    aFBSDKApplicationDelegate := TFBSDKApplicationDelegate.Wrap(TFBSDKApplicationDelegate.OCClass.sharedInstance);
    aFBSDKApplicationDelegate.applicationDidFinishLaunchingWithOptions(aValue.Application, aValue.Options);
  end;
end;

{****************************************************************************}
// To bypass compile errors you must add a custom modified FMX.Platform.iOS to
// your project, see the instructions at https://github.com/grijjy/DelphiSocialFrameworks for more details
class procedure _TProcOfObjectWrapper.applicationOpenURLWithSourceAnnotationHandler(const Sender: TObject; const M: TMessage);
var aFBSDKApplicationDelegate: FBSDKApplicationDelegate;
    aValue: TAppDelegate_applicationOpenURLWithSourceAnnotation;
begin
  if M is TAppDelegateMessage_applicationOpenURLWithSourceAnnotation then begin
    aValue := (M as TAppDelegateMessage_applicationOpenURLWithSourceAnnotation).value;
    aFBSDKApplicationDelegate := TFBSDKApplicationDelegate.Wrap(TFBSDKApplicationDelegate.OCClass.sharedInstance);
    aFBSDKApplicationDelegate.applicationOpenURLSourceApplicationAnnotation(aValue.Application, // application: UIApplication;
                                                                            aValue.Url,  // openURL: NSURL;
                                                                            aValue.SourceApplication,  // sourceApplication: NSString;
                                                                            aValue.Annotation); // annotation: Pointer
  end;
end;

{*******************************************************************************************************************}
class procedure _TProcOfObjectWrapper.applicationOpenURLWithOptionsHandler(const Sender: TObject; const M: TMessage);
var aFBSDKApplicationDelegate: FBSDKApplicationDelegate;
    aValue: TAppDelegate_applicationOpenURLWithOptions;
begin
  if M is TAppDelegateMessage_applicationOpenURLWithOptions then begin
    aValue := (M as TAppDelegateMessage_applicationOpenURLWithOptions).Value;
    aFBSDKApplicationDelegate := TFBSDKApplicationDelegate.Wrap(TFBSDKApplicationDelegate.OCClass.sharedInstance);
    aFBSDKApplicationDelegate.applicationOpenURLOptions(aValue.Application, // application: UIApplication
                                                        aValue.Url,  // openURL: NSURL;
                                                        aValue.Options); // options: NSDictionary
  end;
end;

{$ENDIF}
{$ENDREGION}

initialization

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.SubscribeToMessage(TAppDelegateMessage_applicationDidFinishLaunchingWithOptions, _TProcOfObjectWrapper.applicationDidFinishLaunchingWithOptionsHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TAppDelegateMessage_applicationOpenURLWithSourceAnnotation, _TProcOfObjectWrapper.applicationOpenURLWithSourceAnnotationHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TAppDelegateMessage_applicationOpenURLWithOptions, _TProcOfObjectWrapper.applicationOpenURLWithOptionsHandler);
  {$ENDIF}
  {$ENDREGION}

finalization

  {$REGION ' IOS'}
  {$IF defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TAppDelegateMessage_applicationDidFinishLaunchingWithOptions, _TProcOfObjectWrapper.applicationDidFinishLaunchingWithOptionsHandler);
  TMessageManager.DefaultManager.Unsubscribe(TAppDelegateMessage_applicationOpenURLWithSourceAnnotation, _TProcOfObjectWrapper.applicationOpenURLWithSourceAnnotationHandler);
  TMessageManager.DefaultManager.Unsubscribe(TAppDelegateMessage_applicationOpenURLWithOptions, _TProcOfObjectWrapper.applicationOpenURLWithOptionsHandler);
  {$ENDIF}
  {$ENDREGION}

end.
