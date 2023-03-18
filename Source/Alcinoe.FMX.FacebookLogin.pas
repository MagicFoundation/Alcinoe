(*******************************************************************************
When people log into your app with Facebook, they can grant permissions to your
app so you can retrieve information or perform actions on Facebook on their
behalf.

Setup (ANDROID)
---------------

https://developers.facebook.com/docs/facebook-login/android/

1) follow the step described in Alcinoe.FMX.FacebookCore

2) On android you just need to include the library
     * com.facebook.android:facebook-login:15.2.0
   in the project. You can do this with the help of AndroidMerger. You can see
   an exemple in <Alcinoe>\Demos\ALFacebookLogin\_source\android\MergeLibraries.bat

3) https://developers.facebook.com/docs/facebook-login/android/
   Add an activity for Facebook, and an activity and intent filter for Chrome
   Custom Tabs inside your application element:

    <activity android:name="com.facebook.FacebookActivity"
              android:configChanges="keyboard|keyboardHidden|screenLayout|screenSize|orientation"
              android:theme="@style/com_facebook_activity_theme"
              android:label="%label%"/>
    <activity android:name="com.facebook.CustomTabActivity"
              android:exported="true">
      <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />
        <data android:scheme="@string/fb_login_protocol_scheme" />
      </intent-filter>
    </activity>


Setup (IOS)
-----------

https://developers.facebook.com/docs/facebook-login/ios/

1) follow the step described in Alcinoe.FMX.FacebookCore
*******************************************************************************)
unit Alcinoe.FMX.FacebookLogin;

interface

{$I Alcinoe.inc}

uses
  {$IF defined(android)}
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Alcinoe.AndroidApi.Facebook,
  {$ELSEIF defined(IOS)}
  iOSapi.Foundation,
  Alcinoe.iOSApi.FacebookCoreKit,
  Alcinoe.iOSApi.FacebookLoginKit,
  {$ENDIF}
  system.Messaging;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookLoginOnCancelEvent = procedure of object;
  TALFacebookLoginOnErrorEvent = procedure(const aMsg: String) of object;
  TALFacebookLoginOnSuccessEvent = procedure(const aUserID: String; const aToken: String; const AGrantedPermissions: TArray<String>; const ADeniedPermissions: TArray<String>) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookLogin = class(TObject)

  {$REGION 'ANDROID'}
  {$IF defined(android)}
  private
    type
      TLoginCallback = class(TJavaLocal, JFacebookCallback)
      private
        [Weak] fFacebookLogin: TALFacebookLogin;
      public
        constructor Create(const aFacebookLogin: TALFacebookLogin);
        procedure onCancel; cdecl;
        procedure onError(error: JFacebookException); cdecl;
        procedure onSuccess(result: JObject); cdecl;
      end;
  private
    FLoginCallback: TLoginCallback;
    FCallbackManager: JCallbackManager;
    procedure ActivityResultHandler(const Sender: TObject; const M: TMessage);
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  private
    FLoginManager: FBSDKLoginManager;
    procedure logInWithPermissionsHandler(result: FBSDKLoginManagerLoginResult; error: NSError);
  {$ENDIF}
  {$ENDREGION}

  private
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
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookGraphRequestOnCompletedEvent = procedure(const aResponse: string; Const aErrorCode: integer; Const aErrorMsg: String) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFacebookGraphRequest = class(TObject)

  {$REGION 'ANDROID'}
  {$IF defined(android)}
  private
    type
      TGraphRequestCallback = class(TJavaLocal, JGraphRequest_Callback)
      private
        [Weak] fFacebookGraphRequest: TALFacebookGraphRequest;
      public
        constructor Create(const aFacebookGraphRequest: TALFacebookGraphRequest);
        procedure onCompleted(response: JGraphResponse); cdecl;
      end;
  private
    FGraphRequestCallback: TGraphRequestCallback;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  procedure GraphRequestCompletionHandler(connection: FBSDKGraphRequestConnecting; result: pointer; error: NSError);
  {$ENDIF}
  {$ENDREGION}

  private
    fOnCompleted: TALFacebookGraphRequestOnCompletedEvent;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Request(const aGraphPath: String; aParameters: Tarray<String>; const aHttpMethod: string = 'GET');
    property onCompleted: TALFacebookGraphRequestOnCompletedEvent read fOnCompleted write fOnCompleted;
  end;

implementation

uses
  system.SysUtils,
  {$IF defined(android)}
  Androidapi.Helpers,
  Androidapi.JNI.app,
  Androidapi.JNI.Os,
  {$ELSEIF defined(IOS)}
  Macapi.Helpers,
  FMX.Platform,
  {$ENDIF}
  Alcinoe.FMX.FacebookCore, // [MANDATORY] Because we need it's initialization/finalization section
  Alcinoe.FMX.Common,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**********************************}
constructor TALFacebookLogin.Create;
begin

  inherited Create;

  fOnCancel := nil;
  fOnError := nil;
  fOnSuccess := nil;
  ALInitFacebook;

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, ActivityResultHandler);
  FLoginCallback := TLoginCallback.Create(Self);
  //-----
  FCallbackManager := TJCallbackManager_Factory.JavaClass.create;
  TJloginManager.JavaClass.getInstance.registerCallback(FCallbackManager, FLoginCallback);

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  // https://stackoverflow.com/questions/42222508/why-we-need-to-do-retain-for-objective-c-object-field
  // In general, if you create such a class with Create, alloc, copy, mutableCopy, new... then retain
  // is called for you. Then you need to call release or autorelease.
  FLoginManager := TFBSDKLoginManager.Create;

  {$ENDIF}
  {$ENDREGION}

end;

{**********************************}
destructor TALFacebookLogin.Destroy;
begin

  fOnCancel := nil;
  fOnError := nil;
  fOnSuccess := nil;

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, ActivityResultHandler);
  AlFreeAndNil(FLoginCallback);
  FCallbackManager := nil;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
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
begin

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  var LArrayList := ALStringsToJArrayList(APermissions);
  var LCollection := TJCollection.Wrap((LArrayList as ILocalObject).GetObjectID);
  TJloginManager.JavaClass.getInstance.logInWithReadPermissions(TAndroidHelper.Activity, LCollection);

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  var LNSPermissions := ALStringsToNSArray(APermissions);
  try
    FLoginManager.logInWithPermissions(LNSPermissions, nil, logInWithPermissionsHandler);
  finally
    LNSPermissions.release;
  end;

  {$ENDIF}
  {$ENDREGION}

end;

{********************************}
procedure TALFacebookLogin.logout;
begin

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  TJloginManager.JavaClass.getInstance.logout;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  FloginManager.logout;

  {$ENDIF}
  {$ENDREGION}

end;

{*********************************************}
function TALFacebookLogin.CurrentToken: String;
begin

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  var LToken := TJAccessToken.JavaClass.getCurrentAccessToken;
  if LToken = nil then Result := ''
  else Result := JStringToString(LToken.GetToken);

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  var LToken := TFBSDKAccessToken.OCClass.currentAccessToken;
  if LToken = nil then result := ''
  else result := NSStrToStr(LToken.tokenString);

  {$ENDIF}
  {$ENDREGION}

end;

{**********************************************}
function TALFacebookLogin.CurrentUserId: String;
begin

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  var LToken := TJAccessToken.JavaClass.getCurrentAccessToken;
  if LToken = nil then Result := ''
  else Result := JStringToString(LToken.getUserId);

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  var LToken := TFBSDKAccessToken.OCClass.currentAccessToken;
  if LToken = nil then result := ''
  else result := NSStrToStr(LToken.userID)

  {$ENDIF}
  {$ENDREGION}

end;

{******************************************************************}
function TALFacebookLogin.CurrentGrantedPermissions: TArray<String>;
begin

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  var LToken := TJAccessToken.JavaClass.getCurrentAccessToken;
  if LToken = nil then setlength(Result, 0)
  else Result := ALJsetToStrings(LToken.getPermissions);

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  var LToken := TFBSDKAccessToken.OCClass.currentAccessToken;
  if LToken = nil then setlength(result, 0)
  else result := ALNSSetToStrings(LToken.permissions);

  {$ENDIF}
  {$ENDREGION}

end;

{*****************************************************************}
function TALFacebookLogin.CurrentDeniedPermissions: TArray<String>;
begin

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  var LToken := TJAccessToken.JavaClass.getCurrentAccessToken;
  if LToken = nil then setlength(Result, 0)
  else Result := ALJsetToStrings(LToken.getDeclinedPermissions);

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}

  var LToken := TFBSDKAccessToken.OCClass.currentAccessToken;
  if LToken = nil then setlength(result, 0)
  else result := ALNSSetToStrings(LToken.declinedPermissions)

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

  {$IFDEF DEBUG}
  allog('TALFacebookLogin.ActivityResultHandler', TalLogType.VERBOSE);
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
  allog('TALFacebookLogin.TLoginCallback.onCancel', TalLogType.warn);
  {$ENDIF}

  if assigned(fFacebookLogin.fOnCancel) then
    fFacebookLogin.fOnCancel;

end;

{**************************}
//onError(FacebookException)
//Called when the dialog finishes with an error.
procedure TALFacebookLogin.TLoginCallback.onError(error: JFacebookException);
var LErrorMsg: String;
begin

  if error <> nil then LErrorMsg := JStringToString(error.toString)
  else LErrorMsg := '';
  {$IFDEF DEBUG}
  allog('TALFacebookLogin.TLoginCallback.onError', 'Error: ' + LErrorMsg, TalLogType.error);
  {$ENDIF}

  if assigned(fFacebookLogin.fOnError) then
    fFacebookLogin.fOnError(LErrorMsg{aMsg});

end;

{*****************}
//onSuccess(Object)
//Called when the dialog completes without error.
//Note: This will be called instead of FacebookCallback.onCancel() if any of the following conditions are true.
// * MessageDialog is used.
// * The logged in Facebook user has not authorized the app that has initiated the dialog.
procedure TALFacebookLogin.TLoginCallback.onSuccess(result: JObject);
var LLoginResult: JLoginResult;
    LToken: JAccessToken;
    LTokenStr: String;
    LUserIDStr: String;
    LGrantedPermissions: TArray<String>;
    LDeniedPermissions: TArray<String>;
begin

  LLoginResult := TJLoginResult.Wrap((result as ILocalObject).GetObjectID);
  LToken := LLoginResult.getAccessToken;
  if LToken <> nil then begin
    LUserIDStr := JStringToString(LToken.getUserId);
    LTokenStr := JStringToString(LToken.getToken);
    LGrantedPermissions := ALJSetToStrings(LToken.getPermissions);
    LDeniedPermissions := ALJSetToStrings(LToken.getDeclinedPermissions);
  end
  else begin
    LUserIDStr := '';
    LTokenStr := '';
    setlength(LGrantedPermissions, 0);
    setlength(LDeniedPermissions, 0);
  end;
  {$IFDEF DEBUG}
  allog(
    'TALFacebookLogin.TLoginCallback.onSuccess',
    'UserID: ' + LUserIDStr + ' | ' +
    'Token: ' + LTokenStr,
    TalLogType.info);
  {$ENDIF}

  if assigned(fFacebookLogin.fOnsuccess) then
    fFacebookLogin.fOnsuccess(LUserIDStr, LTokenStr, LGrantedPermissions, LDeniedPermissions);

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{***********************************************************************************************************}
procedure TALFacebookLogin.logInWithPermissionsHandler(result: FBSDKLoginManagerLoginResult; error: NSError);
var LToken: FBSDKAccessToken;
    LTokenStr: String;
    LUserIDStr: String;
    LGrantedPermissions: TArray<String>;
    LDeniedPermissions: TArray<String>;
begin

  //ERROR
  if (error <> nil) or (result = nil) then begin

    {$IFDEF DEBUG}
    if error <> nil then allog('TALFacebookLogin.logInWithReadPermissionsHandler.onError', NSStrToStr(error.localizedDescription), TalLogType.error)
    else allog('TALFacebookLogin.logInWithReadPermissionsHandler.onError', TalLogType.error);
    {$ENDIF}

    if assigned(fOnError) then begin
      if error <> nil then fOnError(NSStrToStr(error.localizedDescription){aMsg})
      else fOnError(''{aMsg});
    end;

  end

  //CANCELED
  else if result.isCancelled then begin

    {$IFDEF DEBUG}
    allog('TALFacebookLogin.logInWithReadPermissionsHandler.onCancel', TalLogType.warn);
    {$ENDIF}

    if assigned(fOnCancel) then
      fOnCancel;

  end

  //SUCCESS
  else begin

    LToken := result.token;
    if LToken <> nil then begin
      LUserIDStr := NSStrToStr(LToken.UserId);
      LTokenStr := NSStrToStr(LToken.TokenString);
      LGrantedPermissions := ALNSSetToStrings(LToken.Permissions);
      LDeniedPermissions := ALNSSetToStrings(LToken.DeclinedPermissions);
    end
    else begin
      LUserIDStr := '';
      LTokenStr := '';
      setlength(LGrantedPermissions, 0);
      setlength(LDeniedPermissions, 0);
    end;

    {$IFDEF DEBUG}
    allog('TALFacebookLogin.logInWithReadPermissionsHandler.onSuccess', 'UserID: ' + LUserIDStr + ' | Token: ' + LTokenStr, TalLogType.info);
    {$ENDIF}

    if assigned(fOnsuccess) then
      fOnsuccess(LUserIDStr, LTokenStr, LGrantedPermissions, LDeniedPermissions);

  end;

end;

{$ENDIF}
{$ENDREGION}

{*****************************************}
constructor TALFacebookGraphRequest.Create;
begin

  inherited Create;

  fonCompleted := nil;
  ALInitFacebook;

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  FGraphRequestCallback := TGraphRequestCallback.Create(Self);

  {$ENDIF}
  {$ENDREGION}

end;

{*****************************************}
destructor TALFacebookGraphRequest.Destroy;
begin

  fOnCompleted := nil;

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  AlFreeAndNil(FGraphRequestCallback);

  {$ENDIF}
  {$ENDREGION}

  inherited Destroy;

end;

{******************************************************************************}
Function _ExtractNameValue(const S: String; var aName, aValue: String): Boolean;
Var P1: Integer;
begin
  P1 := ALPosW('=',S);
  if P1 > 0 then begin
    result := True;
    aName := ALCopyStr(S,1,P1-1);
    aValue := ALCopyStr(S, P1+1, maxint);
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
begin

  {$REGION 'ANDROID'}
  {$IF defined(android)}

  var LBundle := TJBundle.JavaClass.init;
  for var i := Low(aparameters) to High(aparameters) do begin
    var LName, LValue: String;
    _ExtractNameValue(aparameters[i], LName, LValue);
    var LJName := StringToJstring(LName);
    var LJValue := StringToJstring(LValue);
    LBundle.putString(LJName, LJValue);
  end;
  var LJHttpMethod: JHttpMethod;
  if ALSameTextW(aHttpMethod, 'POST') then LJHttpMethod := TJHttpMethod.JavaClass.POST
  else LJHttpMethod := TJHttpMethod.JavaClass.GET;
  var LGraphRequest := TJGraphRequest.JavaClass.init(TJAccessToken.JavaClass.getCurrentAccessToken, StringToJstring(aGraphPath), LBundle, LJHttpMethod, FGraphRequestCallback);
  LGraphRequest.executeAsync;

  {$ENDIF}
  {$ENDREGION}

  {$REGION 'IOS'}
  {$IF defined(IOS)}
  var LNSDictParameters := TNSMutableDictionary.Create;
  try
    for var i := Low(aparameters) to High(aparameters) do begin
      var LName, LValue: String;
      _ExtractNameValue(aparameters[i], LName, LValue);
      LNSDictParameters.setValue(StringToID(LName), StrToNsStr(LValue));
    end;
    var LGraphRequest := TFBSDKGraphRequest.Alloc.initWithGraphPathParametersHTTPMethod(
                           StrToNSStr(aGraphPath), // graphPath: NSString;
                           LNSDictParameters, // parameters: NSDictionary;
                           StrToNSStr(aHTTPMethod));//HTTPMethod: NSString
    try
      LGraphRequest.startWithCompletion(GraphRequestCompletionHandler);
    finally
      LGraphRequest.release;
    end;
  finally
    LNSDictParameters.release;
  end;
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
var LRawResponse: String;
    LErrorCode: Integer;
    LErrorMsg: String;
begin

  LRawResponse := '';
  LErrorCode := 0;
  LErrorMsg := '';
  if response <> nil then begin
    LRawResponse := JstringToString(response.getRawResponse);
    if (response.getError <> nil) then begin
      LErrorCode := response.getError.getErrorCode;
      LErrorMsg := JStringToString(response.getError.getErrorMessage);
    end;
  end;
  {$IFDEF DEBUG}
  allog(
    'TALFacebookGraphRequest.TGraphRequestCallback.onCompleted',
    'response: ' + LRawResponse + ' | ' +
    'ErrorCode: ' + ALIntToStrW(LErrorCode) + ' | ' +
    'ErrorMsg: ' + LErrorMsg,
    TalLogType.verbose);
  {$ENDIF}

  if assigned(fFacebookGraphRequest.fOnCompleted) then
    fFacebookGraphRequest.fOnCompleted(LRawResponse, LErrorCode, LErrorMsg);

end;

{$ENDIF}
{$ENDREGION}

{$REGION ' IOS'}
{$IF defined(IOS)}

{****************************************************************************************************************************************}
procedure TALFacebookGraphRequest.GraphRequestCompletionHandler(connection: FBSDKGraphRequestConnecting; result: pointer; error: NSError);
var LRawResponse: String;
    LErrorCode: integer;
    LErrorMsg: String;
    LJsonErr: NSError;
    LJsonData: NSData;
    LNSString: NSString;
begin

  LRawResponse := '';
  LErrorCode := 0;
  LErrorMsg := '';
  if error <> nil then begin
    LErrorCode := error.code;
    LErrorMsg := NSStrToStr(error.localizedDescription);
  end
  else if result <> nil then begin
    LJsonData := TNSJSONSerialization.OCClass.dataWithJSONObject(result, 0, Addr(LJsonErr));
    if (LJsonData <> nil) and (LJsonErr = nil) then begin
      LNSString := TNSString.Wrap(TNSString.Alloc.initWithData(LJsonData, NSUTF8StringEncoding));
      try
        LRawResponse := NSStrToStr(LNSString);
      finally
        LNSString.release;
      end;
    end;
  end;

  {$IFDEF DEBUG}
  allog(
    'TALFacebookGraphRequest.GraphRequestCompletionHandler',
    'response: ' + LRawResponse + ' | ' +
    'ErrorCode: ' + ALIntToStrW(LErrorCode) + ' | ' +
    'ErrorMsg: ' + LErrorMsg,
    TalLogType.verbose);
  {$ENDIF}

  if assigned(fOnCompleted) then
    fOnCompleted(LRawResponse, LErrorCode, LErrorMsg);

end;

{$ENDIF}
{$ENDREGION}

end.
