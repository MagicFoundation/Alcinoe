unit ALIosFacebookApi;

interface

uses Macapi.ObjectiveC,
     iOSapi.Foundation,
     iOSapi.CocoaTypes,
     iOSapi.UIKit;

{$M+}

type

  {*************************************************************}
  // @protocol FBSDKSharingContent <FBSDKCopying, NSSecureCoding>
  // A base interface for content to be shared.
  FBSDKSharingContentClass = interface(NSObjectClass)
  ['{F888BB6E-227D-4790-8533-EEBFB1BB750A}']
  end;
  FBSDKSharingContent = interface(NSObject)
  ['{FF54B9AE-76F4-47E8-A0EE-4803D47E4D36}']

    // URL for the content being shared.
    // This URL will be checked for all link meta tags for linking in platform specific ways.  See documentation
    // for App Links (https:// developers.facebook.com/docs/applinks/)
    //  - Returns: URL representation of the content link
    // @property (nonatomic, copy) NSURL *contentURL;
    function contentURL: NSURL; cdecl;
    procedure setContentURL(contentURL: NSURL); cdecl;

    // Hashtag for the content being shared.
    //  - Returns: The hashtag for the content being shared.
    // @property (nonatomic, copy) FBSDKHashtag *hashtag;

    // List of IDs for taggable people to tag with this content.
    // See documentation for Taggable Friends
    // (https:// developers.facebook.com/docs/graph-api/reference/user/taggable_friends)
    //  - Returns: Array of IDs for people to tag (NSString)
    // @property (nonatomic, copy) NSArray *peopleIDs;

    // The ID for a place to tag with this content.
    //  - Returns: The ID for the place to tag
    // @property (nonatomic, copy) NSString *placeID;

    // A value to be added to the referrer URL when a person follows a link from this shared content on feed.
    // - Returns: The ref for the content.
    // @property (nonatomic, copy) NSString *ref;

    // For shares into Messenger, this pageID will be used to map the app to page and attach attribution to the share.
    // - Returns: The ID of the Facebok page this share is associated with.
    // @property (nonatomic, copy) NSString *pageID;

    // A unique identifier for a share involving this content, useful for tracking purposes.
    // - Returns: A unique string identifying this share data.
    // @property (nonatomic, copy, readonly) NSString *shareUUID;

  end;
  TFBSDKSharingContent = class(TOCGenericImport<FBSDKSharingContentClass, FBSDKSharingContent>) end;

  {*******************************************************************}
  // @interface FBSDKShareLinkContent : NSObject <FBSDKSharingContent>
  // A model for status and link content to be shared.
  FBSDKShareLinkContentClass = interface(FBSDKSharingContentClass)
  ['{CF0DB5C8-54C6-4200-A99D-E835DC0745B6}']
  end;
  FBSDKShareLinkContent = interface(FBSDKSharingContent)
  ['{E93992B4-0243-47AD-93C0-58B0A651E08F}']

    // The description of the link.
    // If not specified, this field is automatically populated by information scraped from the contentURL,
    // typically the title of the page.  This value may be discarded for specially handled links (ex: iTunes URLs).
    //  - Returns: The description of the link
    // @deprecated `contentDescription` is deprecated from Graph API 2.9.
    // For more information, see https://developers.facebook.com/docs/apps/changelog#v2_9_deprecations.
    // @property (nonatomic, readonly) NSString *contentDescription DEPRECATED_MSG_ATTRIBUTE("`contentDescription` is deprecated from Graph API 2.9");
    function contentDescription: NSString; cdecl; deprecated 'contentDescription is deprecated from Graph API 2.9';

    // The title to display for this link.
    // This value may be discarded for specially handled links (ex: iTunes URLs).
    // - Returns: The link title
    // @deprecated `contentTitle` is deprecated from Graph API 2.9.
    // For more information, see https://developers.facebook.com/docs/apps/changelog#v2_9_deprecations
    // @property (nonatomic, readonly) NSString *contentTitle DEPRECATED_MSG_ATTRIBUTE("`contentTitle` is deprecated from Graph API 2.9");
    function contentTitle: NSString; cdecl; deprecated 'contentTitle is deprecated from Graph API 2.9';

    // The URL of a picture to attach to this content.
    //  - Returns: The network URL of an image
    // @deprecated `imageURL` is deprecated from Graph API 2.9.
    // For more information, see https://developers.facebook.com/docs/apps/changelog#v2_9_deprecations
    // @property (nonatomic, readonly) NSURL *imageURL DEPRECATED_MSG_ATTRIBUTE("`imageURL` is deprecated from Graph API 2.9");
    function imageURL: NSURL; cdecl; deprecated 'imageURL is deprecated from Graph API 2.9';

    // Some quote text of the link.
    // If specified, the quote text will render with custom styling on top of the link.
    //  - Returns: The quote text of a link
    // @property (nonatomic, copy) NSString *quote;
    function quote: NSString; cdecl;
    procedure setQuote(quote: NSString); cdecl;

    // Compares the receiver to another link content.
    //  - Parameter content: The other content
    //  - Returns: YES if the receiver's values are equal to the other content's values; otherwise NO
    {// - (BOOL)isEqualToShareLinkContent:(FBSDKShareLinkContent *)content;}
    function isEqualToShareLinkContent(content: FBSDKShareLinkContent): Boolean; cdecl;

  end;
  TFBSDKShareLinkContent = class(TOCGenericImport<FBSDKShareLinkContentClass, FBSDKShareLinkContent>) end;

  {***********************************************************}
  // The common interface for components that initiate sharing.
  // - See:FBSDKShareDialog
  // - See:FBSDKMessageDialog
  // - See:FBSDKShareAPI
  // @protocol FBSDKSharing <NSObject>
  FBSDKSharingClass = interface(NSObjectClass)
  ['{3835CD8F-7649-4704-A9F3-C83F9BE02209}']
  end;
  FBSDKSharing = interface(NSObject)
  ['{B19665D8-B0CA-4E71-8D7D-F78436226F72}']

    // The receiver's delegate or nil if it doesn't have a delegate.
    // @property (nonatomic, weak) id<FBSDKSharingDelegate> delegate;

    // @property (nonatomic, copy) id<FBSDKSharingContent> shareContent;
    // The content to be shared.
    function shareContent: FBSDKSharingContent; cdecl;
    procedure setShareContent(shareContent: FBSDKSharingContent); cdecl;

    // A Boolean value that indicates whether the receiver should fail if it finds an error with the share content.
    // If NO, the sharer will still be displayed without the data that was mis-configured.  For example, an
    // invalid placeID specified on the shareContent would produce a data error.
    // @property (nonatomic, assign) BOOL shouldFailOnDataError;
    function shouldFailOnDataError: boolean; cdecl;
    procedure setShouldFailOnDataError(shouldFailOnDataError: boolean); cdecl;

    // Validates the content on the receiver.
    //  - Parameter errorRef: If an error occurs, upon return contains an NSError object that describes the problem.
    //  - Returns: YES if the content is valid, otherwise NO.
    // - (BOOL)validateWithError:(NSError **)errorRef;

  end;
  TFBSDKSharing = class(TOCGenericImport<FBSDKSharingClass, FBSDKSharing>) end;

  {********************************************************************}
  // @interface FBSDKSharingDialog : NSObject <FBSDKSharingDialogDialog>
  // A dialog for sharing content on Facebook.
  FBSDKSharingDialogClass = interface(FBSDKSharingClass)
  ['{CF17E43D-D280-468C-9732-54E39CD703D1}']
  end;
  FBSDKSharingDialog = interface(FBSDKSharing)
  ['{000A5A87-5204-4B32-BF78-3CF912582947}']

    // A Boolean value that indicates whether the receiver can initiate a share.
    // May return NO if the appropriate Facebook app is not installed and is required or an access token is
    // required but not available.  This method does not validate the content on the receiver, so this can be checked before
    // building up the content.
    //  - See:[FBSDKSharing validateWithError:]
    //  - Returns: YES if the receiver can share, otherwise NO.
    // - (BOOL)canShow;
    function canShow: Boolean; cdecl;

    // Shows the dialog.
    // - Returns: YES if the receiver was able to begin sharing, otherwise NO.
    // - (BOOL)show;
    function show: Boolean; cdecl;

  end;
  TFBSDKSharingDialog = class(TOCGenericImport<FBSDKSharingDialogClass, FBSDKSharingDialog>) end;

  {************************************************************}
  // @interface FBSDKShareDialog : NSObject <FBSDKSharingDialog>
  // A dialog for sharing content on Facebook.
  FBSDKShareDialogClass = interface(FBSDKSharingDialogClass)
  ['{9E1FD20C-C942-4DF1-8645-D705CCE3196C}']

    // Convenience method to show an FBSDKShareDialog with a fromViewController, content and a delegate.
    //  - Parameter viewController: A UIViewController to present the dialog from, if appropriate.
    //  - Parameter content: The content to be shared.
    //  - Parameter delegate: The receiver's delegate.
    // + (instancetype)showFromViewController:(UIViewController *)viewController
    //                            withContent:(id<FBSDKSharingContent>)content
    //                               delegate:(id<FBSDKSharingDelegate>)delegate;

  end;
  FBSDKShareDialog = interface(FBSDKSharingDialog)
  ['{8E175438-3671-43BE-A9EE-17F6F90AD10F}']

    // A UIViewController to present the dialog from.
    // If not specified, the top most view controller will be automatically determined as best as possible.
    // @property (nonatomic, weak) UIViewController *fromViewController;
    function fromViewController: UIViewController; cdecl;
    procedure setFromViewController(fromViewController: UIViewController); cdecl;

    // The mode with which to display the dialog.
    // @property (nonatomic, assign) FBSDKShareDialogMode mode;
    // Defaults to FBSDKShareDialogModeAutomatic, which will automatically choose the best available mode.

  end;
  TFBSDKShareDialog = class(TOCGenericImport<FBSDKShareDialogClass, FBSDKShareDialog>) end;

  {******************************************************************}
  // Represents an immutable access token for using Facebook services.
  // @interface FBSDKAccessToken : NSObject<FBSDKCopying, NSSecureCoding>
  FBSDKAccessToken = interface;
  FBSDKAccessTokenClass = interface(NSObjectClass)
  ['{767E50A0-B5E0-48FF-A74D-6052CE88393C}']

    // Returns the "global" access token that represents the currently logged in user.
    // The `currentAccessToken` is a convenient representation of the token of the
    // current user and is used by other SDK components (like `FBSDKLoginManager`).
    // + (FBSDKAccessToken *)currentAccessToken;
    {class} function currentAccessToken: FBSDKAccessToken; cdecl;

    // Sets the "global" access token that represents the currently logged in user.
    // - Parameter token: The access token to set.
    // This will broadcast a notification and save the token to the app keychain.
    // + (void)setCurrentAccessToken:(FBSDKAccessToken *)token;
    {class} procedure setCurrentAccessToken(token: FBSDKAccessToken); cdecl;

    // Refresh the current access token's permission state and extend the token's expiration date,
    // if possible.
    // - Parameter completionHandler: an optional callback handler that can surface any errors related to permission refreshing.
    // On a successful refresh, the currentAccessToken will be updated so you typically only need to
    // observe the `FBSDKAccessTokenDidChangeNotification` notification.
    // If a token is already expired, it cannot be refreshed.
    //+ (void)refreshCurrentAccessToken:(FBSDKGraphRequestHandler)completionHandler;
    {class} //procedure refreshCurrentAccessToken(completionHandler: FBSDKGraphRequestHandler); cdecl;

    //+ (instancetype)new NS_UNAVAILABLE;

  end;
  FBSDKAccessToken = interface(NSObject)
  ['{006A1DB9-1E0D-4101-8F6C-CD69B9D3D4C8}']

    // Returns the app ID.
    // @property (readonly, copy, nonatomic) NSString *appID;
    function appID: NSString; cdecl;

    // Returns the known declined permissions.
    // @property (readonly, copy, nonatomic) NSSet *declinedPermissions;
    function declinedPermissions: NSSet; cdecl;

    // Returns the expiration date.
    // @property (readonly, copy, nonatomic) NSDate *expirationDate;
    function expirationDate: NSDate; cdecl;

    // Returns the known granted permissions.
    // @property (readonly, copy, nonatomic) NSSet *permissions;
    function permissions: NSSet; cdecl;

    // Returns the date the token was last refreshed.
    // @property (readonly, copy, nonatomic) NSDate *refreshDate;
    function refreshDate: NSDate; cdecl;

    // Returns the opaque token string.
    // @property (readonly, copy, nonatomic) NSString *tokenString;
    function tokenString: NSString; cdecl;

    // Returns the user ID.
    // @property (readonly, copy, nonatomic) NSString *userID;
    function userID: NSString; cdecl;

    // Initializes a new instance.
    // - Parameter tokenString: the opaque token string.
    // - Parameter permissions: the granted permissions. Note this is converted to NSSet and is only
    // an NSArray for the convenience of literal syntax.
    // - Parameter declinedPermissions: the declined permissions. Note this is converted to NSSet and is only
    // an NSArray for the convenience of literal syntax.
    // - Parameter appID: the app ID.
    // - Parameter userID: the user ID.
    // - Parameter expirationDate: the optional expiration date (defaults to distantFuture).
    // - Parameter refreshDate: the optional date the token was last refreshed (defaults to today).
    // This initializer should only be used for advanced apps that
    // manage tokens explicitly. Typical login flows only need to use `FBSDKLoginManager`
    // along with `+currentAccessToken`.
    // - (instancetype)initWithTokenString:(NSString *)tokenString
    //                         permissions:(NSArray *)permissions
    //                 declinedPermissions:(NSArray *)declinedPermissions
    //                               appID:(NSString *)appID
    //                              userID:(NSString *)userID
    //                      expirationDate:(NSDate *)expirationDate
    //                         refreshDate:(NSDate *)refreshDate
    // NS_DESIGNATED_INITIALIZER;
    function initWithTokenString(tokenString: NSString; permissions: NSArray; declinedPermissions: NSArray; appID: NSString; userID: NSString; expirationDate: NSDate; refreshDate: NSDate): Pointer { instancetype }; cdecl;

    // Convenience getter to determine if a permission has been granted
    // - Parameter permission:  The permission to check.
    // - (BOOL)hasGranted:(NSString *)permission;
    function hasGranted(permission: NSString): Boolean; cdecl;

    // Compares the receiver to another FBSDKAccessToken
    // - Parameter token: The other token
    // - Returns: YES if the receiver's values are equal to the other token's values; otherwise NO
    // - (BOOL)isEqualToAccessToken:(FBSDKAccessToken *)token;
    function isEqualToAccessToken(token: FBSDKAccessToken): Boolean; cdecl;

    //- (instancetype)init NS_UNAVAILABLE;

  end;
  TFBSDKAccessToken = class(TOCGenericImport<FBSDKAccessTokenClass, FBSDKAccessToken>) end;

  {*****************************************}
  // Describes the result of a login attempt.
  // @interface FBSDKLoginManagerLoginResult : NSObject
  FBSDKLoginManagerLoginResultClass = interface(NSObjectClass)
  ['{8406268C-C01F-467C-8A23-58CCE8EA9964}']
  end;
  FBSDKLoginManagerLoginResult = interface(NSObject)
  ['{D8223440-048B-4E8E-A2AC-CD96C012DEC0}']

    // the access token.
    // @property (copy, nonatomic) FBSDKAccessToken *token;
    procedure setToken(token: FBSDKAccessToken); cdecl;
    function token: FBSDKAccessToken; cdecl;

    // whether the login was cancelled by the user.
    // @property (readonly, nonatomic) BOOL isCancelled;
    function isCancelled: Boolean; cdecl;

    // the set of permissions granted by the user in the associated request.
    // inspect the token's permissions set for a complete list.
    // @property (copy, nonatomic) NSSet *grantedPermissions;
    procedure setGrantedPermissions(grantedPermissions: NSSet); cdecl;
    function grantedPermissions: NSSet; cdecl;

    // the set of permissions declined by the user in the associated request.
    // inspect the token's permissions set for a complete list.
    // @property (copy, nonatomic) NSSet *declinedPermissions;
    procedure setDeclinedPermissions(declinedPermissions: NSSet); cdecl;
    function declinedPermissions: NSSet; cdecl;

    // Initializes a new instance.
    // - Parameter token: the access token
    // - Parameter isCancelled: whether the login was cancelled by the user
    // - Parameter grantedPermissions: the set of granted permissions
    // - Parameter declinedPermissions: the set of declined permissions
    // - (instancetype)initWithToken:(FBSDKAccessToken *)token
    //                   isCancelled:(BOOL)isCancelled
    //            grantedPermissions:(NSSet *)grantedPermissions
    //           declinedPermissions:(NSSet *)declinedPermissions
    // NS_DESIGNATED_INITIALIZER;
    function initWithToken(token: FBSDKAccessToken; isCancelled: Boolean; grantedPermissions: NSSet; declinedPermissions: NSSet): Pointer {instancetype}; cdecl;

  end;
  TFBSDKLoginManagerLoginResult = class(TOCGenericImport<FBSDKLoginManagerLoginResultClass, FBSDKLoginManagerLoginResult>) end;

type

  {*************************************************}
  // Describes the call back to the FBSDKLoginManager
  // - Parameter result: the result of the authorization
  // - Parameter error: the authorization error, if any.
  // typedef void (^FBSDKLoginManagerRequestTokenHandler)(FBSDKLoginManagerLoginResult *result, NSError *error);
  FBSDKLoginManagerRequestTokenHandler = procedure(result: FBSDKLoginManagerLoginResult; error: NSError) of object;

Type

  {**************************}
  // FBSDKDefaultAudience enum
  // Passed to open to indicate which default audience to use for sessions that post data to Facebook.
  // Certain operations such as publishing a status or publishing a photo require an audience. When the user
  // grants an application permission to perform a publish operation, a default audience is selected as the
  // publication ceiling for the application. This enumerated value allows the application to select which
  // audience to ask the user to grant publish permission for.
  //typedef NS_ENUM(NSUInteger, FBSDKDefaultAudience)
  FBSDKDefaultAudience = NSUInteger;

const

  // Indicates that the user's friends are able to see posts made by the application
  FBSDKDefaultAudienceFriends = 0;
  // Indicates that only the user is able to see posts made by the application
  FBSDKDefaultAudienceOnlyMe = 1;
  //Indicates that all Facebook users are able to see posts made by the application
  FBSDKDefaultAudienceEveryone = 2;

type

  {************************}
  // FBSDKLoginBehavior enum
  // Passed to the \c FBSDKLoginManager to indicate how Facebook Login should be attempted.
  // Facebook Login authorizes the application to act on behalf of the user, using the user's
  // Facebook account. Usually a Facebook Login will rely on an account maintained outside of
  // the application, by the native Facebook application, the browser, or perhaps the device
  // itself. This avoids the need for a user to enter their username and password directly, and
  // provides the most secure and lowest friction way for a user to authorize the application to
  // interact with Facebook.
  // The \c FBSDKLoginBehavior enum specifies which log-in methods may be used. The SDK
  // will determine the best behavior based on the current device (such as iOS version).
  // typedef NS_ENUM(NSUInteger, FBSDKLoginBehavior)
  FBSDKLoginBehavior = NSUInteger;

const

  // This is the default behavior, and indicates logging in through the native
  // Facebook app may be used. The SDK may still use Safari instead.
  FBSDKLoginBehaviorNative = 0;
  // Attempts log in through the Safari or SFSafariViewController, if available.
  FBSDKLoginBehaviorBrowser = 1;
  // Attempts log in through the Facebook account currently signed in through
  // the device Settings.
  // @note If the account is not available to the app (either not configured by user or
  // as determined by the SDK) this behavior falls back to \c FBSDKLoginBehaviorNative.
  FBSDKLoginBehaviorSystemAccount = 2;
  // Attempts log in through a modal \c UIWebView pop up
  // @note This behavior is only available to certain types of apps. Please check the Facebook
  // Platform Policy to verify your app meets the restrictions.
  FBSDKLoginBehaviorWeb = 3;

type

  {*********************************************************************}
  // FBSDKLoginManager provides methods for logging the user in and out.
  // FBSDKLoginManager works directly with `[FBSDKAccessToken currentAccessToken]` and
  // sets the "currentAccessToken" upon successful authorizations (or sets `nil` in case of `logOut`).
  // You should check `[FBSDKAccessToken currentAccessToken]` before calling logIn* to see if there is
  // a cached token available (typically in your viewDidLoad).
  // If you are managing your own token instances outside of "currentAccessToken", you will need to set
  // "currentAccessToken" before calling logIn* to authorize further permissions on your tokens.
  // @interface FBSDKLoginManager : NSObject
  FBSDKLoginManagerClass = interface(NSObjectClass)
    ['{0449E118-BDAC-44FD-A9ED-67947BF73D7C}']

    // @method
    // Issues an asynchronous renewCredentialsForAccount call to the device's Facebook account store.
    // - Parameter handler: The completion handler to call when the renewal is completed. This can be invoked on an arbitrary thread.
    // This can be used to explicitly renew account credentials and is provided as a convenience wrapper around
    // `[ACAccountStore renewCredentialsForAccount:completion]`. Note the method will not issue the renewal call if the the
    // Facebook account has not been set on the device, or if access had not been granted to the account (though the handler
    // wil receive an error).
    // If the `[FBSDKAccessToken currentAccessToken]` was from the account store, a succesful renewal will also set
    // a new "currentAccessToken".
    // + (void)renewSystemCredentials:(void (^)(ACAccountCredentialRenewResult result, NSError *error))handler;
    {class} //procedure renewSystemCredentials(handler: TFBSDKLoginKitHandler); cdecl;

  end;
  FBSDKLoginManager = interface(NSObject)
  ['{A8828F5C-FC65-40A1-B8B3-EEA420EDA3A9}']

    // the default audience.
    // you should set this if you intend to ask for publish permissions.
    // @property (assign, nonatomic) FBSDKDefaultAudience defaultAudience;
    procedure setDefaultAudience(defaultAudience: FBSDKDefaultAudience); cdecl;
    function defaultAudience: FBSDKDefaultAudience; cdecl;

    // the login behavior
    // @property (assign, nonatomic) FBSDKLoginBehavior loginBehavior;
    procedure setLoginBehavior(loginBehavior: FBSDKLoginBehavior); cdecl;
    function loginBehavior: FBSDKLoginBehavior; cdecl;

    // - Warning:use logInWithReadPermissions:fromViewController:handler: instead
    // - (void)logInWithReadPermissions:(NSArray *)permissions handler:(FBSDKLoginManagerRequestTokenHandler)handler
    // __attribute__ ((deprecated("use logInWithReadPermissions:fromViewController:handler: instead")));
    // procedure logInWithReadPermissions(permissions: NSArray; handler: FBSDKLoginManagerRequestTokenHandler); cdecl;

    // - Warning:use logInWithPublishPermissions:fromViewController:handler: instead
    // - (void)logInWithPublishPermissions:(NSArray *)permissions handler:(FBSDKLoginManagerRequestTokenHandler)handler
    // __attribute__ ((deprecated("use logInWithPublishPermissions:fromViewController:handler: instead")));
    // procedure logInWithPublishPermissions(permissions: NSArray; handler: FBSDKLoginManagerRequestTokenHandler); cdecl;

    // Logs the user in or authorizes additional permissions.
    // - Parameter permissions: the optional array of permissions. Note this is converted to NSSet and is only
    // an NSArray for the convenience of literal syntax.
    // - Parameter fromViewController: the view controller to present from. If nil, the topmost view controller will be
    // automatically determined as best as possible.
    // - Parameter handler: the callback.
    // Use this method when asking for read permissions. You should only ask for permissions when they
    // are needed and explain the value to the user. You can inspect the result.declinedPermissions to also
    // provide more information to the user if they decline permissions.
    // This method will present UI the user. You typically should check if `[FBSDKAccessToken currentAccessToken]`
    // already contains the permissions you need before asking to reduce unnecessary app switching. For example,
    // you could make that check at viewDidLoad.
    // You can only do one login call at a time. Calling a login method before the completion handler is called
    // on a previous login will return an error.
    // - (void)logInWithReadPermissions:(NSArray *)permissions
    //               fromViewController:(UIViewController *)fromViewController
    //                          handler:(FBSDKLoginManagerRequestTokenHandler)handler;
    procedure logInWithReadPermissions(permissions: NSArray;
                                       fromViewController: UIViewController;
                                       handler: FBSDKLoginManagerRequestTokenHandler); cdecl;

    // Logs the user in or authorizes additional permissions.
    // - Parameter permissions: the optional array of permissions. Note this is converted to NSSet and is only
    // an NSArray for the convenience of literal syntax.
    // - Parameter fromViewController: the view controller to present from. If nil, the topmost view controller will be
    // automatically determined as best as possible.
    // - Parameter handler: the callback.
    // Use this method when asking for publish permissions. You should only ask for permissions when they
    // are needed and explain the value to the user. You can inspect the result.declinedPermissions to also
    // provide more information to the user if they decline permissions.
    // This method will present UI the user. You typically should check if `[FBSDKAccessToken currentAccessToken]`
    // already contains the permissions you need before asking to reduce unnecessary app switching. For example,
    // you could make that check at viewDidLoad.
    // You can only do one login call at a time. Calling a login method before the completion handler is called
    // on a previous login will return an error.
    // - (void)logInWithPublishPermissions:(NSArray *)permissions
    //                  fromViewController:(UIViewController *)fromViewController
    //                             handler:(FBSDKLoginManagerRequestTokenHandler)handler;
    procedure logInWithPublishPermissions(permissions: NSArray;
                                          fromViewController: UIViewController;
                                          handler: FBSDKLoginManagerRequestTokenHandler); cdecl;

    // Logs the user out
    // This calls [FBSDKAccessToken setCurrentAccessToken:nil] and [FBSDKProfile setCurrentProfile:nil].
    // - (void)logOut;
    procedure logOut; cdecl;

  end;
  TFBSDKLoginManager = class(TOCGenericImport<FBSDKLoginManagerClass, FBSDKLoginManager>) end;

  {*****************************************************************************************}
  // The FBSDKApplicationDelegate is designed to post process the results from Facebook Login
  // or Facebook Dialogs (or any action that requires switching over to the native Facebook
  // app or Safari).
  // The methods in this class are designed to mirror those in UIApplicationDelegate, and you
  // should call them in the respective methods in your AppDelegate implementation.
  // @interface FBSDKApplicationDelegate : NSObject
  FBSDKApplicationDelegateClass = interface(NSObjectClass)
  ['{AF5EB2E0-8250-4E6A-B244-6CD47E945874}']

    // Gets the singleton instance.
    // + (instancetype)sharedInstance;
    {class}function sharedInstance: Pointer{instancetype}; cdecl;

  end;
  FBSDKApplicationDelegate = interface(NSObject)
  ['{C1D0CAA3-3677-4DB9-9B45-E75436E9016F}']

    // Call this method from the [UIApplicationDelegate application:openURL:sourceApplication:annotation:] method
    // of the AppDelegate for your app. It should be invoked for the proper processing of responses during interaction
    // with the native Facebook app or Safari as part of SSO authorization flow or Facebook dialogs.
    // - Parameter application: The application as passed to [UIApplicationDelegate application:openURL:sourceApplication:annotation:].
    // - Parameter url: The URL as passed to [UIApplicationDelegate application:openURL:sourceApplication:annotation:].
    // - Parameter sourceApplication: The sourceApplication as passed to [UIApplicationDelegate application:openURL:sourceApplication:annotation:].
    // - Parameter annotation: The annotation as passed to [UIApplicationDelegate application:openURL:sourceApplication:annotation:].
    // - Returns: YES if the url was intended for the Facebook SDK, NO if not.
    // - (BOOL)application:(UIApplication *)application
    //             openURL:(NSURL *)url
    //   sourceApplication:(NSString *)sourceApplication
    //          annotation:(id)annotation;
    [MethodName('application:openURL:sourceApplication:annotation:')]
    function applicationOpenURLSourceApplicationAnnotation(application: UIApplication; openURL: NSURL; sourceApplication: NSString; annotation: Pointer): Boolean; cdecl;

    // #if __IPHONE_OS_VERSION_MAX_ALLOWED > __IPHONE_9_0
    // Call this method from the [UIApplicationDelegate application:openURL:options:] method
    // of the AppDelegate for your app. It should be invoked for the proper processing of responses during interaction
    // with the native Facebook app or Safari as part of SSO authorization flow or Facebook dialogs.
    // - Parameter application: The application as passed to [UIApplicationDelegate application:openURL:options:].
    // - Parameter url: The URL as passed to [UIApplicationDelegate application:openURL:options:].
    // - Parameter options: The options dictionary as passed to [UIApplicationDelegate application:openURL:options:].
    // - Returns: YES if the url was intended for the Facebook SDK, NO if not.
    // - (BOOL)application:(UIApplication *)application
    //             openURL:(NSURL *)url
    //             options:(NSDictionary<UIApplicationOpenURLOptionsKey,id> *)options;
    // #endif
    [MethodName('application:openURL:options:')]
    function applicationOpenURLOptions(application: UIApplication; openURL: NSURL; options: NSDictionary): Boolean; cdecl;

    // Call this method from the [UIApplicationDelegate application:didFinishLaunchingWithOptions:] method
    // of the AppDelegate for your app. It should be invoked for the proper use of the Facebook SDK.
    // As part of SDK initialization basic auto logging of app events will occur, this can be
    // controlled via 'FacebookAutoLogAppEventsEnabled' key in the project info plist file.
    // - Parameter application: The application as passed to [UIApplicationDelegate application:didFinishLaunchingWithOptions:].
    // - Parameter launchOptions: The launchOptions as passed to [UIApplicationDelegate application:didFinishLaunchingWithOptions:].
    // - Returns: YES if the url was intended for the Facebook SDK, NO if not.
    // - (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions;
    [MethodName('application:didFinishLaunchingWithOptions:')]
    function applicationDidFinishLaunchingWithOptions(application: UIApplication; didFinishLaunchingWithOptions: NSDictionary): Boolean; cdecl;

  end;
  TFBSDKApplicationDelegate = class(TOCGenericImport<FBSDKApplicationDelegateClass, FBSDKApplicationDelegate>) end;

  {**************************************}
  FBSDKGraphRequestConnection = interface;
  FBSDKGraphRequest = interface;

  {*************************}
  // FBSDKGraphRequestHandler
  // A block that is passed to addRequest to register for a callback with the results of that
  // request once the connection completes.
  // Pass a block of this type when calling addRequest.  This will be called once
  // the request completes.  The call occurs on the UI thread.
  // - Parameter connection:      The `FBSDKGraphRequestConnection` that sent the request.
  // - Parameter result:          The result of the request.  This is a translation of
  // JSON data to `NSDictionary` and `NSArray` objects.  This
  // is nil if there was an error.
  // - Parameter error:           The `NSError` representing any error that occurred.
  // typedef void (^FBSDKGraphRequestHandler)(FBSDKGraphRequestConnection *connection,
  //                                          id result,
  //                                          NSError *error);
  FBSDKGraphRequestHandler = procedure(connection: FBSDKGraphRequestConnection; result: Pointer; error: NSError) of object;

  {***************************************************************************************************}
  // The `FBSDKGraphRequestConnection` represents a single connection to Facebook to service a request.
  // The request settings are encapsulated in a reusable <FBSDKGraphRequest> object. The
  // `FBSDKGraphRequestConnection` object encapsulates the concerns of a single communication
  // e.g. starting a connection, canceling a connection, or batching requests.
  // @interface FBSDKGraphRequestConnection : NSObject
  FBSDKGraphRequestConnectionClass = interface(NSObjectClass)
  ['{08E93F22-7942-422F-B23D-0B1BD9F9CAA4}']

    // @method
    //  This method sets the default timeout on all FBSDKGraphRequestConnection instances. Defaults to 60 seconds.
    // - Parameter defaultConnectionTimeout:     The timeout interval.
    // + (void)setDefaultConnectionTimeout:(NSTimeInterval)defaultConnectionTimeout;
    {class} procedure setDefaultConnectionTimeout(defaultConnectionTimeout: NSTimeInterval); cdecl;

  end;
  FBSDKGraphRequestConnection = interface(NSObject)
  ['{CEEFE128-6EBD-40EB-B02F-B362E6C8C701}']

    // The delegate object that receives updates.
    // @property (nonatomic, weak) id<FBSDKGraphRequestConnectionDelegate> delegate;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;

    // Gets or sets the timeout interval to wait for a response before giving up.
    // @property (nonatomic) NSTimeInterval timeout;
    procedure setTimeout(timeout: NSTimeInterval); cdecl;
    function timeout: NSTimeInterval; cdecl;

    // The raw response that was returned from the server.  (readonly)
    // This property can be used to inspect HTTP headers that were returned from
    // the server.
    // The property is nil until the request completes.  If there was a response
    // then this property will be non-nil during the FBSDKGraphRequestHandler callback.
    // @property (nonatomic, retain, readonly) NSHTTPURLResponse *URLResponse;
    function URLResponse: NSHTTPURLResponse; cdecl;

    // @method
    // This method adds an <FBSDKGraphRequest> object to this connection.
    // - Parameter request:       A request to be included in the round-trip when start is called.
    // - Parameter handler:       A handler to call back when the round-trip completes or times out.
    // The completion handler is retained until the block is called upon the
    // completion or cancellation of the connection.
    // - (void)addRequest:(FBSDKGraphRequest *)request
    //  completionHandler:(FBSDKGraphRequestHandler)handler;
    procedure addRequest(request: FBSDKGraphRequest; completionHandler: FBSDKGraphRequestHandler); overload; cdecl;

    // @method
    // This method adds an <FBSDKGraphRequest> object to this connection.
    // - Parameter request:         A request to be included in the round-trip when start is called.
    // - Parameter handler:         A handler to call back when the round-trip completes or times out.
    // The handler will be invoked on the main thread.
    // - Parameter name:            An optional name for this request.  This can be used to feed
    // the results of one request to the input of another <FBSDKGraphRequest> in the same
    // `FBSDKGraphRequestConnection` as described in
    // [Graph API Batch Requests]( https://developers.facebook.com/docs/reference/api/batch/ ).
    // The completion handler is retained until the block is called upon the
    // completion or cancellation of the connection. This request can be named
    // to allow for using the request's response in a subsequent request.
    // - (void)addRequest:(FBSDKGraphRequest *)request
    //  completionHandler:(FBSDKGraphRequestHandler)handler
    //     batchEntryName:(NSString *)name;
    procedure addRequest(request: FBSDKGraphRequest; completionHandler: FBSDKGraphRequestHandler; batchEntryName: NSString); overload; cdecl;

    // @method
    // This method adds an <FBSDKGraphRequest> object to this connection.
    // - Parameter request:         A request to be included in the round-trip when start is called.
    // - Parameter handler:         A handler to call back when the round-trip completes or times out.
    // - Parameter batchParameters: The optional dictionary of parameters to include for this request
    // as described in [Graph API Batch Requests]( https://developers.facebook.com/docs/reference/api/batch/ ).
    // Examples include "depends_on", "name", or "omit_response_on_success".
    // The completion handler is retained until the block is called upon the
    // completion or cancellation of the connection. This request can be named
    // to allow for using the request's response in a subsequent request.
    // - (void)addRequest:(FBSDKGraphRequest *)request
    //  completionHandler:(FBSDKGraphRequestHandler)handler
    //    batchParameters:(NSDictionary *)batchParameters;
    procedure addRequest(request: FBSDKGraphRequest; completionHandler: FBSDKGraphRequestHandler; batchParameters: NSDictionary); overload; cdecl;

    // @method
    // Signals that a connection should be logically terminated as the
    // application is no longer interested in a response.
    // Synchronously calls any handlers indicating the request was cancelled. Cancel
    // does not guarantee that the request-related processing will cease. It
    // does promise that  all handlers will complete before the cancel returns. A call to
    // cancel prior to a start implies a cancellation of all requests associated
    // with the connection.
    // - (void)cancel;
    procedure cancel; cdecl;

    // This method starts a connection with the server and is capable of handling all of the
    // requests that were added to the connection.
    // By default, a connection is scheduled on the current thread in the default mode when it is created.
    // See `setDelegateQueue:` for other options.
    // This method cannot be called twice for an `FBSDKGraphRequestConnection` instance.
    // - (void)start;
    procedure start; cdecl;

    // Determines the operation queue that is used to call methods on the connection's delegate.
    // - Parameter queue: The operation queue to use when calling delegate methods.
    // By default, a connection is scheduled on the current thread in the default mode when it is created.
    // You cannot reschedule a connection after it has started.
    // This is very similar to `[NSURLConnection setDelegateQueue:]`.
    // - (void)setDelegateQueue:(NSOperationQueue *)queue;
    procedure setDelegateQueue(queue: NSOperationQueue); cdecl;

    // @method
    //  Overrides the default version for a batch request
    // The SDK automatically prepends a version part, such as "v2.0" to API paths in order to simplify API versioning
    // for applications. If you want to override the version part while using batch requests on the connection, call
    // this method to set the version for the batch request.
    // - Parameter version:   This is a string in the form @"v2.0" which will be used for the version part of an API path
    //- (void)overrideVersionPartWith:(NSString *)version;
    procedure overrideVersionPartWith(version: NSString); cdecl;

  end;
  TFBSDKGraphRequestConnection = class(TOCGenericImport<FBSDKGraphRequestConnectionClass, FBSDKGraphRequestConnection>) end;

  {************************************************}
  // Represents a request to the Facebook Graph API.
  // `FBSDKGraphRequest` encapsulates the components of a request (the
  // Graph API path, the parameters, error recovery behavior) and should be
  // used in conjunction with `FBSDKGraphRequestConnection` to issue the request.
  // Nearly all Graph APIs require an access token. Unless specified, the
  // `[FBSDKAccessToken currentAccessToken]` is used. Therefore, most requests
  // will require login first (see `FBSDKLoginManager` in FBSDKLoginKit.framework).
  // A `- start` method is provided for convenience for single requests.
  // By default, FBSDKGraphRequest will attempt to recover any errors returned from
  // Facebook. You can disable this via `disableErrorRecovery:`.
  // - See:FBSDKGraphErrorRecoveryProcessor
  // @interface FBSDKGraphRequest : NSObject
  FBSDKGraphRequestClass = interface(NSObjectClass)
  ['{8A5C1638-E469-4319-BF1A-CDA8FD2A27EB}']
  end;
  FBSDKGraphRequest = interface(NSObject)
  ['{EF201D97-24C8-4E50-AA6D-FCD4E4716131}']

    // Initializes a new instance that use use `[FBSDKAccessToken currentAccessToken]`.
    // - Parameter graphPath: the graph path (e.g., @"me").
    // - Parameter parameters: the optional parameters dictionary.
    // - (instancetype)initWithGraphPath:(NSString *)graphPath
    //                        parameters:(NSDictionary *)parameters;
    function initWithGraphPath(graphPath: NSString; parameters: NSDictionary): Pointer {instancetype}; overload; cdecl;

    // Initializes a new instance that use use `[FBSDKAccessToken currentAccessToken]`.
    // - Parameter graphPath: the graph path (e.g., @"me").
    // - Parameter parameters: the optional parameters dictionary.
    // - Parameter HTTPMethod: the optional HTTP method. nil defaults to @"GET".
    // - (instancetype)initWithGraphPath:(NSString *)graphPath
    //                        parameters:(NSDictionary *)parameters
    //                        HTTPMethod:(NSString *)HTTPMethod;
    function initWithGraphPath(graphPath: NSString; parameters: NSDictionary; HTTPMethod: NSString): Pointer {instancetype}; overload; cdecl;

    // Initializes a new instance.
    // - Parameter graphPath: the graph path (e.g., @"me").
    // - Parameter parameters: the optional parameters dictionary.
    // - Parameter tokenString: the token string to use. Specifying nil will cause no token to be used.
    // - Parameter version: the optional Graph API version (e.g., @"v2.0"). nil defaults to `[FBSDKSettings graphAPIVersion]`.
    // - Parameter HTTPMethod: the optional HTTP method (e.g., @"POST"). nil defaults to @"GET".
    // - (instancetype)initWithGraphPath:(NSString *)graphPath
    //                        parameters:(NSDictionary *)parameters
    //                       tokenString:(NSString *)tokenString
    //                           version:(NSString *)version
    //                        HTTPMethod:(NSString *)HTTPMethod
    // NS_DESIGNATED_INITIALIZER;
    function initWithGraphPath(graphPath: NSString; parameters: NSDictionary; tokenString: NSString; version: NSString; HTTPMethod: NSString): Pointer {instancetype}; overload; cdecl;

    // The request parameters.
    // @property (nonatomic, strong, readonly) NSMutableDictionary *parameters;
    function parameters: NSMutableDictionary; cdecl;

    // The access token string used by the request.
    // @property (nonatomic, copy, readonly) NSString *tokenString;
    function tokenString: NSString; cdecl;

    // The Graph API endpoint to use for the request, for example "me".
    // @property (nonatomic, copy, readonly) NSString *graphPath;
    function graphPath: NSString; cdecl;

    // The HTTPMethod to use for the request, for example "GET" or "POST".
    // @property (nonatomic, copy, readonly) NSString *HTTPMethod;
    function HTTPMethod: NSString; cdecl;

    // The Graph API version to use (e.g., "v2.0")
    // @property (nonatomic, copy, readonly) NSString *version;
    function version: NSString; cdecl;

    // If set, disables the automatic error recovery mechanism.
    // - Parameter disable: whether to disable the automatic error recovery mechanism
    // By default, non-batched FBSDKGraphRequest instances will automatically try to recover
    // from errors by constructing a `FBSDKGraphErrorRecoveryProcessor` instance that
    // re-issues the request on successful recoveries. The re-issued request will call the same
    // handler as the receiver but may occur with a different `FBSDKGraphRequestConnection` instance.
    // This will override [FBSDKSettings setGraphErrorRecoveryDisabled:].
    // - (void)setGraphErrorRecoveryDisabled:(BOOL)disable;
    procedure setGraphErrorRecoveryDisabled(disable: Boolean); cdecl;

    // Starts a connection to the Graph API.
    // - Parameter handler: The handler block to call when the request completes.
    // - (FBSDKGraphRequestConnection *)startWithCompletionHandler:(FBSDKGraphRequestHandler)handler;
    function startWithCompletionHandler(handler: FBSDKGraphRequestHandler): FBSDKGraphRequestConnection; cdecl;

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
procedure StubProc12; cdecl; external 'Bolts'         name 'OBJC_CLASS_$_BFAppLink'; // else i have ld: b/bl/blx thumb2 branch out of range (16777216 max is +/-16MB): from -[FIRAAlarm runAfterDelay:withBlock:] (0x00032AA0) to _objc_msgSend.island (0x00E09DE4) for architecture armv7
procedure StubProc13; cdecl; external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a' name '__isOSVersionAtLeast'; // << else I have Error: "___isOSVersionAtLeast", referenced from: ...


{$ELSE}

// i don't know how to do under ios simulator :(

{$ENDIF}

end.
