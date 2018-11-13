unit ALIosFirebaseApi;

interface

uses Macapi.ObjectiveC,
     iOSapi.CocoaTypes,
     iOSapi.Foundation;

{$M+}

type

  {**********************************}
  // The entry point of Firebase SDKs.
  // Initialize and configure FIRApp using +[FIRApp configure]
  // or other customized ways as shown below.
  // The logging system has two modes: default mode and debug mode. In default mode, only logs with
  // log level Notice, Warning and Error will be sent to device. In debug mode, all logs will be sent
  // to device. The log levels that Firebase uses are consistent with the ASL log levels.
  // Enable debug mode by passing the -FIRDebugEnabled argument to the application. You can add this
  // argument in the application's Xcode scheme. When debug mode is enabled via -FIRDebugEnabled,
  // further executions of the application will also be in debug mode. In order to return to default
  // mode, you must explicitly disable the debug mode with the application argument -FIRDebugDisabled.
  // It is also possible to change the default logging level in code by calling setLoggerLevel: on
  // the FIRConfiguration interface.
  // NS_SWIFT_NAME(FirebaseApp)
  // @interface FIRApp : NSObject
  FIRAppClass = interface(NSObjectClass)
  ['{37BEFC11-8AE6-4312-971D-53BF9D8DB22A}']

    // Configures a default Firebase app. Raises an exception if any configuration step fails. The
    // default app is named "__FIRAPP_DEFAULT". This method should be called after the app is launched
    // and before using Firebase services. This method is thread safe.
    // + (void)configure;
    procedure configure; cdecl;

    // Configures the default Firebase app with the provided options. The default app is named
    // "__FIRAPP_DEFAULT". Raises an exception if any configuration step fails. This method is thread
    // safe.
    // @param options The Firebase application options used to configure the service.
    // + (void)configureWithOptions:(FIROptions *)options NS_SWIFT_NAME(configure(options:));
    //procedure configureWithOptions(options: FIROptions);

    // Configures a Firebase app with the given name and options. Raises an exception if any
    // configuration step fails. This method is thread safe.
    // @param name The application's name given by the developer. The name should should only contain
    //             Letters, Numbers and Underscore.
    // @param options The Firebase application options used to configure the services.
    // + (void)configureWithName:(NSString *)name options:(FIROptions *)options NS_SWIFT_NAME(configure(name:options:));
    //procedure configureWithName(name: NSString; options: FIROptions);

    // Returns the default app, or nil if the default app does not exist.
    // + (nullable FIRApp *)defaultApp NS_SWIFT_NAME(app());

    // Returns a previously created FIRApp instance with the given name, or nil if no such app exists.
    // This method is thread safe.
    // + (nullable FIRApp *)appNamed:(NSString *)name NS_SWIFT_NAME(app(name:));

    // #if defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0
    //   Returns the set of all extant FIRApp instances, or nil if there are no FIRApp instances. This
    //   method is thread safe.
    //   @property(class, readonly, nullable) NSDictionary<NSString *, FIRApp *> *allApps;
    // #else
    //   Returns the set of all extant FIRApp instances, or nil if there are no FIRApp instances. This
    //   method is thread safe.
    //   + (nullable NSDictionary<NSString *, FIRApp *> *)allApps NS_SWIFT_NAME(allApps());
    // #endif  // defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0

  end;
  FIRApp = interface(NSObject)
  ['{69F89279-48F0-4276-B337-37FE79821507}']

    // Cleans up the current FIRApp, freeing associated data and returning its name to the pool for
    // future use. This method is thread safe.
    // - (void)deleteApp:(FIRAppVoidBoolCallback)completion;

    // FIRApp instances should not be initialized directly. Call +[FIRApp configure],
    // +[FIRApp configureWithOptions:], or +[FIRApp configureWithNames:options:] directly.
    // - (instancetype)init NS_UNAVAILABLE;

    // Gets the name of this app.
    // @property(nonatomic, copy, readonly) NSString *name;

    // Gets a copy of the options for this app. These are non-modifiable.
    // @property(nonatomic, copy, readonly) FIROptions *options;

  end;
  TFIRApp = class(TOCGenericImport<FIRAppClass, FIRApp>) end;

  {************************}
  // @memberof FIRInstanceID
  // The scope to be used when fetching/deleting a token for Firebase Messaging.
  // FOUNDATION_EXPORT NSString *__nonnull const kFIRInstanceIDScopeFirebaseMessaging NS_SWIFT_NAME(InstanceIDScopeFirebaseMessaging);

  {******************************************************************************}
  //#if defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0
  //  Called when the system determines that tokens need to be refreshed.
  //  This method is also called if Instance ID has been reset in which
  //  case, tokens and FCM topic subscriptions also need to be refreshed.
  //  Instance ID service will throttle the refresh event across all devices
  //  to control the rate of token updates on application servers.
  //  FOUNDATION_EXPORT const NSNotificationName __nonnull kFIRInstanceIDTokenRefreshNotification NS_SWIFT_NAME(InstanceIDTokenRefresh);
  //#else
  //  Called when the system determines that tokens need to be refreshed.
  //  This method is also called if Instance ID has been reset in which
  //  case, tokens and FCM topic subscriptions also need to be refreshed.
  //  Instance ID service will throttle the refresh event across all devices
  //  to control the rate of token updates on application servers.
  //  FOUNDATION_EXPORT NSString *__nonnull const kFIRInstanceIDTokenRefreshNotification NS_SWIFT_NAME(InstanceIDTokenRefreshNotification);
  //#endif  // defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0
  function kFIRInstanceIDTokenRefreshNotification: NSString; cdecl;

type

  {*********************************************************************}
  // The completion handler invoked when the InstanceID token returns. If
  // the call fails we return the appropriate `error code` as described below.
  // @param token The valid token as returned by InstanceID backend.
  // @param error The error describing why generating a new token
  //              failed. See the error codes below for a more detailed
  //              description.
  // typedef void (^FIRInstanceIDTokenHandler)(NSString *__nullable token, NSError *__nullable error) NS_SWIFT_NAME(InstanceIDTokenHandler);

  {***********************}
  // @related FIRInstanceID
  // The completion handler invoked when the InstanceID `deleteToken` returns. If
  // the call fails we return the appropriate `error code` as described below
  // @param error The error describing why deleting the token failed.
  //              See the error codes below for a more detailed description.
  // typedef void (^FIRInstanceIDDeleteTokenHandler)(NSError *__nullable error) NS_SWIFT_NAME(InstanceIDDeleteTokenHandler);

  {***********************}
  // @related FIRInstanceID
  // The completion handler invoked when the app identity is created. If the
  // identity wasn't created for some reason we return the appropriate error code.
  // @param identity A valid identity for the app instance, nil if there was an error
  //                 while creating an identity.
  // @param error    The error if fetching the identity fails else nil.
  // typedef void (^FIRInstanceIDHandler)(NSString *__nullable identity, NSError *__nullable error) NS_SWIFT_NAME(InstanceIDHandler);

  {***********************}
  // @related FIRInstanceID
  // The completion handler invoked when the app identity and all the tokens associated
  // with it are deleted. Returns a valid error object in case of failure else nil.
  // @param error The error if deleting the identity and all the tokens associated with
  //              it fails else nil.
  // typedef void (^FIRInstanceIDDeleteHandler)(NSError *__nullable error) NS_SWIFT_NAME(InstanceIDDeleteHandler);
  FIRInstanceIDDeleteHandler = procedure(error: NSError) of object;

Type

  // Public errors produced by InstanceID.
  FIRInstanceIDError = NSUInteger;

const

  // Unknown error.
  FIRInstanceIDErrorUnknown = 0;

  // Auth Error -- GCM couldn't validate request from this client.
  FIRInstanceIDErrorAuthentication = 1;

  // NoAccess -- InstanceID service cannot be accessed.
  FIRInstanceIDErrorNoAccess = 2;

  // Timeout -- Request to InstanceID backend timed out.
  FIRInstanceIDErrorTimeout = 3;

  // Network -- No network available to reach the servers.
  FIRInstanceIDErrorNetwork = 4;

  // OperationInProgress -- Another similar operation in progress,
  // bailing this one.
  FIRInstanceIDErrorOperationInProgress = 5;

  // InvalidRequest -- Some parameters of the request were invalid.
  FIRInstanceIDErrorInvalidRequest = 7;

Type

  // The APNS token type for the app. If the token type is set to `UNKNOWN`
  // InstanceID will implicitly try to figure out what the actual token type
  // is from the provisioning profile.
  FIRInstanceIDAPNSTokenType = NSInteger;

const

  // Unknown token type.
  FIRInstanceIDAPNSTokenTypeUnknown = 0 deprecated 'Use FIRMessaging''s APNSToken property instead.';

  // Sandbox token type.
  FIRInstanceIDAPNSTokenTypeSandbox = 1 deprecated 'Use FIRMessaging''s APNSToken property instead.';

  // Production token type.
  FIRInstanceIDAPNSTokenTypeProd = 2 deprecated 'Use FIRMessaging''s APNSToken property instead.';

type

  {*******************************************************************************}
  // Instance ID provides a unique identifier for each app instance and a mechanism
  // to authenticate and authorize actions (for example, sending an FCM message).
  // Once an InstanceID is generated, the library periodically sends information about the
  // application and the device where it's running to the Firebase backend. To stop this. see
  // `[FIRInstanceID deleteIDWithHandler:]`.
  // Instance ID is long lived but, may be reset if the device is not used for
  // a long time or the Instance ID service detects a problem.
  // If Instance ID is reset, the app will be notified via
  // `kFIRInstanceIDTokenRefreshNotification`.
  // If the Instance ID has become invalid, the app can request a new one and
  // send it to the app server.
  // To prove ownership of Instance ID and to allow servers to access data or
  // services associated with the app, call
  // `[FIRInstanceID tokenWithAuthorizedEntity:scope:options:handler]`.
  // NS_SWIFT_NAME(InstanceID)
  // @interface FIRInstanceID : NSObject
  FIRInstanceIDClass = interface(NSObjectClass)
  ['{A39E9F77-78C9-4756-BCDE-6E1C08593250}']

    // FIRInstanceID.
    // @return A shared instance of FIRInstanceID.
    // + (nonnull instancetype)instanceID NS_SWIFT_NAME(instanceID());
    function instanceID: pointer; cdecl;

  end;
  FIRInstanceID = interface(NSObject)
  ['{E63F2AA7-4A90-4EEB-A3F9-5057015E0DB8}']

    // Unavailable. Use +instanceID instead.
    // - (nonnull instancetype)init __attribute__((unavailable("Use +instanceID instead.")));

    // Set APNS token for the application. This APNS token will be used to register
    // with Firebase Messaging using `token` or
    // `tokenWithAuthorizedEntity:scope:options:handler`. If the token type is set to
    // `FIRInstanceIDAPNSTokenTypeUnknown` InstanceID will read the provisioning profile
    // to find out the token type.
    // @param token The APNS token for the application.
    // @param type  The APNS token type for the above token.
    // - (void)setAPNSToken:(nonnull NSData *)token type:(FIRInstanceIDAPNSTokenType)type __deprecated_msg("Use FIRMessaging's APNSToken property instead.");

    // #pragma mark - Tokens
    // Returns a Firebase Messaging scoped token for the firebase app.
    // @return Returns the stored token if the device has registered with Firebase Messaging, otherwise
    //         returns nil.
    // - (nullable NSString *)token;
    function token: NSString; cdecl;

    // Returns a token that authorizes an Entity (example: cloud service) to perform
    // an action on behalf of the application identified by Instance ID.
    // This is similar to an OAuth2 token except, it applies to the
    // application instance instead of a user.
    // This is an asynchronous call. If the token fetching fails for some reason
    // we invoke the completion callback with nil `token` and the appropriate
    // error.
    // This generates an Instance ID if it does not exist yet, which starts periodically sending
    // information to the Firebase backend (see `[FIRInstanceID getIDWithHandler:]`).
    // Note, you can only have one `token` or `deleteToken` call for a given
    // authorizedEntity and scope at any point of time. Making another such call with the
    // same authorizedEntity and scope before the last one finishes will result in an
    // error with code `OperationInProgress`.
    // @see FIRInstanceID deleteTokenWithAuthorizedEntity:scope:handler:
    // @param authorizedEntity Entity authorized by the token.
    // @param scope            Action authorized for authorizedEntity.
    // @param options          The extra options to be sent with your token request. The
    //                         value for the `apns_token` should be the NSData object
    //                         passed to the UIApplicationDelegate's
    //                         `didRegisterForRemoteNotificationsWithDeviceToken` method.
    //                         The value for `apns_sandbox` should be a boolean (or an
    //                         NSNumber representing a BOOL in Objective C) set to true if
    //                         your app is a debug build, which means that the APNs
    //                         device token is for the sandbox environment. It should be
    //                         set to false otherwise. If the `apns_sandbox` key is not
    //                         provided, an automatically-detected value shall be used.
    // @param handler          The callback handler which is invoked when the token is
    //                         successfully fetched. In case of success a valid `token` and
    //                         `nil` error are returned. In case of any error the `token`
    //                         is nil and a valid `error` is returned. The valid error
    //                         codes have been documented above.
    //- (void)tokenWithAuthorizedEntity:(nonnull NSString *)authorizedEntity
    //                            scope:(nonnull NSString *)scope
    //                          options:(nullable NSDictionary *)options
    //                          handler:(nonnull FIRInstanceIDTokenHandler)handler;

    // Revokes access to a scope (action) for an entity previously
    // authorized by `[FIRInstanceID tokenWithAuthorizedEntity:scope:options:handler]`.
    // This is an asynchronous call. Call this on the main thread since InstanceID lib
    // is not thread safe. In case token deletion fails for some reason we invoke the
    // `handler` callback passed in with the appropriate error code.
    // Note, you can only have one `token` or `deleteToken` call for a given
    // authorizedEntity and scope at a point of time. Making another such call with the
    // same authorizedEntity and scope before the last one finishes will result in an error
    // with code `OperationInProgress`.
    // @param authorizedEntity Entity that must no longer have access.
    // @param scope            Action that entity is no longer authorized to perform.
    // @param handler          The handler that is invoked once the unsubscribe call ends.
    //                         In case of error an appropriate error object is returned
    //                         else error is nil.
    // - (void)deleteTokenWithAuthorizedEntity:(nonnull NSString *)authorizedEntity
    //                                   scope:(nonnull NSString *)scope
    //                                 handler:(nonnull FIRInstanceIDDeleteTokenHandler)handler;

    // #pragma mark - Identity
    // Asynchronously fetch a stable identifier that uniquely identifies the app
    // instance. If the identifier has been revoked or has expired, this method will
    // return a new identifier.
    // Once an InstanceID is generated, the library periodically sends information about the
    // application and the device where it's running to the Firebase backend. To stop this. see
    // `[FIRInstanceID deleteIDWithHandler:]`.
    // @param handler The handler to invoke once the identifier has been fetched.
    //                In case of error an appropriate error object is returned else
    //                a valid identifier is returned and a valid identifier for the
    //                application instance.
    // - (void)getIDWithHandler:(nonnull FIRInstanceIDHandler)handler NS_SWIFT_NAME(getID(handler:));

    // Resets Instance ID and revokes all tokens.
    // This method also triggers a request to fetch a new Instance ID and Firebase Messaging scope
    // token. Please listen to kFIRInstanceIDTokenRefreshNotification when the new ID and token are
    // ready.
    // This stops the periodic sending of data to the Firebase backend that began when the Instance ID
    // was generated. No more data is sent until another library calls Instance ID internally again
    // (like FCM, RemoteConfig or Analytics) or user explicitly calls Instance ID APIs to get an
    // Instance ID and token again.
    // - (void)deleteIDWithHandler:(nonnull FIRInstanceIDDeleteHandler)handler NS_SWIFT_NAME(deleteID(handler:));
    procedure deleteIDWithHandler(handler: FIRInstanceIDDeleteHandler); cdecl;

  end;
  TFIRInstanceID = class(TOCGenericImport<FIRInstanceIDClass, FIRInstanceID>) end;

  // The completion handler invoked when the registration token returns.
  // If the call fails we return the appropriate `error code`, described by
  // `FIRMessagingError`.
  // @param FCMToken The valid registration token returned by FCM.
  // @param error The error describing why a token request failed. The error code
  //              will match a value from the FIRMessagingError enumeration.
  // typedef void(^FIRMessagingFCMTokenFetchCompletion)(NSString * _Nullable FCMToken, NSError * _Nullable error) NS_SWIFT_NAME(MessagingFCMTokenFetchCompletion);

  // The completion handler invoked when the registration token deletion request is
  // completed. If the call fails we return the appropriate `error code`, described
  // by `FIRMessagingError`.
  // @param error The error describing why a token deletion failed. The error code
  //              will match a value from the FIRMessagingError enumeration.
  // typedef void(^FIRMessagingDeleteFCMTokenCompletion)(NSError * _Nullable error) NS_SWIFT_NAME(MessagingDeleteFCMTokenCompletion);

  // The completion handler invoked once the data connection with FIRMessaging is
  // established.  The data connection is used to send a continous stream of
  // data and all the FIRMessaging data notifications arrive through this connection.
  // Once the connection is established we invoke the callback with `nil` error.
  // Correspondingly if we get an error while trying to establish a connection
  // we invoke the handler with an appropriate error object and do an
  // exponential backoff to try and connect again unless successful.
  // @param error The error object if any describing why the data connection
  //              to FIRMessaging failed.
  // typedef void(^FIRMessagingConnectCompletion)(NSError * __nullable error) NS_SWIFT_NAME(MessagingConnectCompletion) __deprecated_msg("Please listen for the FIRMessagingConnectionStateChangedNotification " "NSNotification instead.");
  FIRMessagingConnectCompletion = procedure(error: NSError) of object; // deprecated 'Please listen for the FIRMessagingConnectionStateChangedNotification NSNotification instead.';

  //#if defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0
  //  Notification sent when the upstream message has been delivered
  //  successfully to the server. The notification object will be the messageID
  //  of the successfully delivered message.
  //  FOUNDATION_EXPORT const NSNotificationName __nonnull FIRMessagingSendSuccessNotification NS_SWIFT_NAME(MessagingSendSuccess);
  //#else
  //  Notification sent when the upstream message has been delivered
  //  successfully to the server. The notification object will be the messageID
  //  of the successfully delivered message.
  //  FOUNDATION_EXPORT NSString * __nonnull const FIRMessagingSendSuccessNotification NS_SWIFT_NAME(MessagingSendSuccessNotification);
  //#endif  // defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0

  //#if defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0
  //  Notification sent when the upstream message was failed to be sent to the
  //  server.  The notification object will be the messageID of the failed
  //  message. The userInfo dictionary will contain the relevant error
  //  information for the failure.
  //  FOUNDATION_EXPORT const NSNotificationName __nonnull FIRMessagingSendErrorNotification NS_SWIFT_NAME(MessagingSendError);
  //#else
  //  Notification sent when the upstream message was failed to be sent to the
  //  server.  The notification object will be the messageID of the failed
  //  message. The userInfo dictionary will contain the relevant error
  //  information for the failure.
  //  FOUNDATION_EXPORT NSString * __nonnull const FIRMessagingSendErrorNotification NS_SWIFT_NAME(MessagingSendErrorNotification);
  //#endif  // defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0

  //#if defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0
  //  Notification sent when the Firebase messaging server deletes pending
  //  messages due to exceeded storage limits. This may occur, for example, when
  //  the device cannot be reached for an extended period of time.
  //  It is recommended to retrieve any missing messages directly from the
  //  server.
  //  FOUNDATION_EXPORT const NSNotificationName __nonnull FIRMessagingMessagesDeletedNotification NS_SWIFT_NAME(MessagingMessagesDeleted);
  //#else
  //  Notification sent when the Firebase messaging server deletes pending
  //  messages due to exceeded storage limits. This may occur, for example, when
  //  the device cannot be reached for an extended period of time.
  //  It is recommended to retrieve any missing messages directly from the
  //  server.
  //  FOUNDATION_EXPORT NSString * __nonnull const FIRMessagingMessagesDeletedNotification NS_SWIFT_NAME(MessagingMessagesDeletedNotification);
  //#endif  // defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0

  //#if defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0
  //  Notification sent when Firebase Messaging establishes or disconnects from
  //  an FCM socket connection. You can query the connection state in this
  //  notification by checking the `isDirectChannelEstablished` property of FIRMessaging.
  //  FOUNDATION_EXPORT const NSNotificationName __nonnull FIRMessagingConnectionStateChangedNotification NS_SWIFT_NAME(MessagingConnectionStateChanged);
  //#else
  //  Notification sent when Firebase Messaging establishes or disconnects from
  //  an FCM socket connection. You can query the connection state in this
  //  notification by checking the `isDirectChannelEstablished` property of FIRMessaging.
  //  FOUNDATION_EXPORT NSString * __nonnull const FIRMessagingConnectionStateChangedNotification NS_SWIFT_NAME(MessagingConnectionStateChangedNotification);
  //#endif  // defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0

  //#if defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0
  //  Notification sent when the FCM registration token has been refreshed. Please use the
  //  FIRMessaging delegate method `messaging:didReceiveRegistrationToken:` to receive current and
  //  updated tokens.
  //  FOUNDATION_EXPORT const NSNotificationName __nonnull FIRMessagingRegistrationTokenRefreshedNotification NS_SWIFT_NAME(MessagingRegistrationTokenRefreshed);
  //#else
  //  Notification sent when the FCM registration token has been refreshed. Please use the
  //  FIRMessaging delegate method `messaging:didReceiveRegistrationToken:` to receive current and
  //  updated tokens.
  //  FOUNDATION_EXPORT NSString * __nonnull const FIRMessagingRegistrationTokenRefreshedNotification NS_SWIFT_NAME(MessagingRegistrationTokenRefreshedNotification);
  //#endif  // defined(__IPHONE_10_0) && __IPHONE_OS_VERSION_MAX_ALLOWED >= __IPHONE_10_0


Type

  // @enum FIRMessagingError
  FIRMessagingError = NSUInteger;

const

   // Unknown error.
   FIRMessagingErrorUnknown = 0;

   // FIRMessaging couldn't validate request from this client.
   FIRMessagingErrorAuthentication = 1;

   // InstanceID service cannot be accessed.
   FIRMessagingErrorNoAccess = 2;

   // Request to InstanceID backend timed out.
   FIRMessagingErrorTimeout = 3;

   // No network available to reach the servers.
   FIRMessagingErrorNetwork = 4;

   // Another similar operation in progress, bailing this one.
   FIRMessagingErrorOperationInProgress = 5;

   // Some parameters of the request were invalid.
   FIRMessagingErrorInvalidRequest = 7;

type

  // Status for the downstream message received by the app.
  FIRMessagingMessageStatus = NSInteger;

const

   // Unknown status.
   FIRMessagingMessageStatusUnknown = 0;

   // New downstream message received by the app.
   FIRMessagingMessageStatusNew = 1;

type

 // The APNS token type for the app. If the token type is set to `UNKNOWN`
 // Firebase Messaging will implicitly try to figure out what the actual token type
 // is from the provisioning profile.
 // Unless you really need to specify the type, you should use the `APNSToken`
 // property instead.
 FIRMessagingAPNSTokenType = NSInteger;

const

  // Unknown token type.
  FIRMessagingAPNSTokenTypeUnknown = 0;

  // Sandbox token type.
  FIRMessagingAPNSTokenTypeSandbox = 1;

  // Production token type.
  FIRMessagingAPNSTokenTypeProd= 2;


type

  {******************************************************************}
  // Firebase Messaging lets you reliably deliver messages at no cost.
  // To send or receive messages, the app must get a
  // registration token from FIRInstanceID. This token authorizes an
  // app server to send messages to an app instance.
  // In order to receive FIRMessaging messages, declare `application:didReceiveRemoteNotification:`.
  // NS_SWIFT_NAME(Messaging)
  // @interface FIRMessaging : NSObject
  FIRMessagingClass = interface(NSObjectClass)
  ['{FC9DDBCE-4C91-4DE4-B2FC-80289562D9F5}']

    // FIRMessaging
    // @return An instance of FIRMessaging.
    // + (nonnull instancetype)messaging NS_SWIFT_NAME(messaging());
    function messaging: pointer; cdecl;

  end;
  FIRMessaging = interface(NSObject)
  ['{FCF96F2C-513B-409C-87D7-3FFE504EA79D}']

    // Delegate to handle FCM token refreshes, and remote data messages received via FCM for devices
    // running iOS 10 or above.
    // @property(nonatomic, weak, nullable) id<FIRMessagingDelegate> delegate;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;

    // Delegate to handle remote data messages received via FCM for devices running iOS 10 or above.
    // @property(nonatomic, weak, nullable) id<FIRMessagingDelegate> remoteMessageDelegate __deprecated_msg("Use 'delegate' property");
    procedure setRemoteMessageDelegate(delegate: Pointer); cdecl; deprecated 'Use ''delegate'' property';
    function remoteMessageDelegate: Pointer; cdecl; deprecated 'Use ''delegate'' property';

    // When set to `YES`, Firebase Messaging will automatically establish a socket-based, direct
    // channel to the FCM server. Enable this only if you are sending upstream messages or
    // receiving non-APNS, data-only messages in foregrounded apps.
    // Default is `NO`.
    // @property(nonatomic) BOOL shouldEstablishDirectChannel;
    procedure setShouldEstablishDirectChannel(shouldEstablishDirectChannel: Boolean); cdecl;
    function shouldEstablishDirectChannel : Boolean; cdecl;

    // Returns `YES` if the direct channel to the FCM server is active, and `NO` otherwise.
    // @property(nonatomic, readonly) BOOL isDirectChannelEstablished;

    // Unavailable. Use +messaging instead.
    // - (nonnull instancetype)init __attribute__((unavailable("Use +messaging instead.")));

    // #pragma mark - APNS
    // This property is used to set the APNS Token received by the application delegate.
    // FIRMessaging uses method swizzling to ensure that the APNS token is set
    // automatically. However, if you have disabled swizzling by setting
    // `FirebaseAppDelegateProxyEnabled` to `NO` in your app's
    // Info.plist, you should manually set the APNS token in your application
    // delegate's `-application:didRegisterForRemoteNotificationsWithDeviceToken:`
    // method.
    // If you would like to set the type of the APNS token, rather than relying on
    // automatic detection, see: `-setAPNSToken:type:`.
    //@property(nonatomic, copy, nullable) NSData *APNSToken NS_SWIFT_NAME(apnsToken);
    [MethodName('setAPNSToken:')]
    procedure setAPNSToken(APNSToken: NSData); cdecl;
    function APNSToken: NSData; cdecl;

    // Set APNS token for the application. This APNS token will be used to register
    // with Firebase Messaging using `FCMToken` or
    // `tokenWithAuthorizedEntity:scope:options:handler`.
    // @param apnsToken The APNS token for the application.
    // @param type  The type of APNS token. Debug builds should use
    // FIRMessagingAPNSTokenTypeSandbox. Alternatively, you can supply
    // FIRMessagingAPNSTokenTypeUnknown to have the type automatically
    // detected based on your provisioning profile.
    //- (void)setAPNSToken:(nonnull NSData *)apnsToken type:(FIRMessagingAPNSTokenType)type;
    [MethodName('setAPNSToken:type:')]
    procedure setAPNSTokenType(APNSToken: NSData; &type: FIRMessagingAPNSTokenType); cdecl;

    // #pragma mark - FCM Tokens
    // Is Firebase Messaging token auto generation enabled?  If this flag is disabled,
    // Firebase Messaging will not generate token automatically for message delivery.
    // If this flag is disabled, Firebase Messaging does not generate new tokens automatically for
    // message delivery. If this flag is enabled, FCM generates a registration token on application
    // start when there is no existing valid token. FCM also generates a new token when an existing
    // token is deleted.
    // This setting is persisted, and is applied on future
    // invocations of your application.  Once explicitly set, it overrides any
    // settings in your Info.plist.
    // By default, FCM automatic initialization is enabled.  If you need to change the
    // default (for example, because you want to prompt the user before getting token)
    // set FirebaseMessagingAutoInitEnabled to false in your application's Info.plist.
    // @property(nonatomic, assign, getter=isAutoInitEnabled) BOOL autoInitEnabled;

    // The FCM token is used to identify this device so that FCM can send notifications to it.
    // It is associated with your APNS token when the APNS token is supplied, so that sending
    // messages to the FCM token will be delivered over APNS.
    // The FCM token is sometimes refreshed automatically. In your FIRMessaging delegate, the
    // delegate method `messaging:didReceiveRegistrationToken:` will be called once a token is
    // available, or has been refreshed. Typically it should be called once per app start, but
    // may be called more often, if token is invalidated or updated.
    // Once you have an FCM token, you should send it to your application server, so it can use
    // the FCM token to send notifications to your device.
    // @property(nonatomic, readonly, nullable) NSString *FCMToken NS_SWIFT_NAME(fcmToken);

    // Retrieves an FCM registration token for a particular Sender ID. This can be used to allow
    // multiple senders to send notifications to the same device. By providing a different Sender
    // ID than your default when fetching a token, you can create a new FCM token which you can
    // give to a different sender. Both tokens will deliver notifications to your device, and you
    // can revoke a token when you need to.
    // This registration token is not cached by FIRMessaging. FIRMessaging should have an APNS
    // token set before calling this to ensure that notifications can be delivered via APNS using
    // this FCM token. You may re-retrieve the FCM token once you have the APNS token set, to
    // associate it with the FCM token. The default FCM token is automatically associated with
    // the APNS token, if the APNS token data is available.
    // @param senderID The Sender ID for a particular Firebase project.
    // @param completion The completion handler to handle the token request.
    // - (void)retrieveFCMTokenForSenderID:(nonnull NSString *)senderID
    //                          completion:(nonnull FIRMessagingFCMTokenFetchCompletion)completion NS_SWIFT_NAME(retrieveFCMToken(forSenderID:completion:));

    // Invalidates an FCM token for a particular Sender ID. That Sender ID cannot no longer send
    // notifications to that FCM token.
    // @param senderID The senderID for a particular Firebase project.
    // @param completion The completion handler to handle the token deletion.
    // - (void)deleteFCMTokenForSenderID:(nonnull NSString *)senderID
    //                        completion:(nonnull FIRMessagingDeleteFCMTokenCompletion)completion NS_SWIFT_NAME(deleteFCMToken(forSenderID:completion:));

    // #pragma mark - Connect
    // Create a FIRMessaging data connection which will be used to send the data notifications
    // sent by your server. It will also be used to send ACKS and other messages based
    // on the FIRMessaging ACKS and other messages based  on the FIRMessaging protocol.
    // @param handler  The handler to be invoked once the connection is established.
    //                 If the connection fails we invoke the handler with an
    //                 appropriate error code letting you know why it failed. At
    //                 the same time, FIRMessaging performs exponential backoff to retry
    //                 establishing a connection and invoke the handler when successful.
    // - (void)connectWithCompletion:(nonnull FIRMessagingConnectCompletion)handler NS_SWIFT_NAME(connect(handler:)) __deprecated_msg("Please use the shouldEstablishDirectChannel property instead.");
    procedure connectWithCompletion(handler: FIRMessagingConnectCompletion); cdecl; deprecated 'Please use the shouldEstablishDirectChannel property instead.';

    // Disconnect the current FIRMessaging data connection. This stops any attempts to
    // connect to FIRMessaging. Calling this on an already disconnected client is a no-op.
    // Call this before `teardown` when your app is going to the background.
    // Since the FIRMessaging connection won't be allowed to live when in the background, it is
    // prudent to close the connection.
    // - (void)disconnect __deprecated_msg("Please use the shouldEstablishDirectChannel property instead.");
    procedure disconnect; cdecl; deprecated 'Please use the shouldEstablishDirectChannel property instead.';

    // #pragma mark - Topics
    // Asynchronously subscribes to a topic.
    // @param topic The name of the topic, for example, @"sports".
    // - (void)subscribeToTopic:(nonnull NSString *)topic NS_SWIFT_NAME(subscribe(toTopic:));

    // Asynchronously unsubscribe from a topic.
    // @param topic The name of the topic, for example @"sports".
    // - (void)unsubscribeFromTopic:(nonnull NSString *)topic NS_SWIFT_NAME(unsubscribe(fromTopic:));

    // #pragma mark - Upstream
    // Sends an upstream ("device to cloud") message.
    // The message is queued if we don't have an active connection.
    // You can only use the upstream feature if your FCM implementation
    // uses the XMPP server protocol.
    // @param message      Key/Value pairs to be sent. Values must be String, any
    //                     other type will be ignored.
    // @param receiver     A string identifying the receiver of the message. For FCM
    //                     project IDs the value is `SENDER_ID@gcm.googleapis.com`.
    // @param messageID    The ID of the message. This is generated by the application. It
    //                     must be unique for each message generated by this application.
    //                     It allows error callbacks and debugging, to uniquely identify
    //                     each message.
    // @param ttl          The time to live for the message. In case we aren't able to
    //                     send the message before the TTL expires we will send you a
    //                     callback. If 0, we'll attempt to send immediately and return
    //                     an error if we're not connected.  Otherwise, the message will
    //                     be queued.  As for server-side messages, we don't return an error
    //                     if the message has been dropped because of TTL; this can happen
    //                     on the server side, and it would require extra communication.
    // - (void)sendMessage:(nonnull NSDictionary *)message
    //                  to:(nonnull NSString *)receiver
    //       withMessageID:(nonnull NSString *)messageID
    //          timeToLive:(int64_t)ttl;

    // #pragma mark - Analytics
    // Use this to track message delivery and analytics for messages, typically
    // when you receive a notification in `application:didReceiveRemoteNotification:`.
    // However, you only need to call this if you set the `FirebaseAppDelegateProxyEnabled`
    // flag to `NO` in your Info.plist. If `FirebaseAppDelegateProxyEnabled` is either missing
    // or set to `YES` in your Info.plist, the library will call this automatically.
    // @param message The downstream message received by the application.
    // @return Information about the downstream message.
    // - (nonnull FIRMessagingMessageInfo *)appDidReceiveMessage:(nonnull NSDictionary *)message;

  end;
  TFIRMessaging = class(TOCGenericImport<FIRMessagingClass, FIRMessaging>) end;
  PFIRMessaging = Pointer;

  {************************************************************}
  // Information about a downstream message received by the app.
  // NS_SWIFT_NAME(MessagingMessageInfo)
  // @interface FIRMessagingMessageInfo : NSObject
  FIRMessagingMessageInfoClass = interface(NSObjectClass)
  ['{03659448-3D94-4135-9F4B-4A5EFA0F725A}']

  end;
  FIRMessagingMessageInfo = interface(NSObject)
  ['{4D70F5C5-3635-405F-895C-F41C8D1FD76B}']

    // The status of the downstream message
    // @property(nonatomic, readonly, assign) FIRMessagingMessageStatus status;
    function status: FIRMessagingMessageStatus; cdecl;

  end;
  TFIRMessagingMessageInfo = class(TOCGenericImport<FIRMessagingMessageInfoClass, FIRMessagingMessageInfo>) end;

  {*********************************************************************************}
  // A remote data message received by the app via FCM (not just the APNs interface).
  // This is only for devices running iOS 10 or above. To support devices running iOS 9 or below, use
  // the local and remote notifications handlers defined in UIApplicationDelegate protocol.
  // NS_SWIFT_NAME(MessagingRemoteMessage)
  // @interface FIRMessagingRemoteMessage : NSObject
  FIRMessagingRemoteMessageClass = interface(NSObjectClass)
  ['{3A36492A-2889-4BE3-B702-60C4489728DC}']

  end;
  FIRMessagingRemoteMessage = interface(NSObject)
  ['{39594348-10DD-488F-ADFA-D7266F5284E6}']

    // The downstream message received by the application.
    // @property(nonatomic, readonly, strong, nonnull) NSDictionary *appData;
    function appData: NSDictionary; cdecl;

  end;
  TFIRMessagingRemoteMessage = class(TOCGenericImport<FIRMessagingRemoteMessageClass, FIRMessagingRemoteMessage>) end;

  {**************************************************************************}
  // A protocol to handle events from FCM for devices running iOS 10 or above.
  // To support devices running iOS 9 or below, use the local and remote notifications handlers
  // defined in UIApplicationDelegate protocol.
  // NS_SWIFT_NAME(MessagingDelegate)
  // @protocol FIRMessagingDelegate <NSObject>
  FIRMessagingDelegate = interface(IObjectiveC)
  ['{9784786A-515F-41F0-84C3-8F298623275E}']

    // @optional
    // This method will be called once a token is available, or has been refreshed. Typically it
    // will be called once per app start, but may be called more often, if token is invalidated or
    // updated. In this method, you should perform operations such as:
    // * Uploading the FCM token to your application server, so targeted notifications can be sent.
    // * Subscribing to any topics.
    // - (void)messaging:(nonnull FIRMessaging *)messaging didReceiveRegistrationToken:(nonnull NSString *)fcmToken NS_SWIFT_NAME(messaging(_:didReceiveRegistrationToken:));
    [MethodName('messaging:didReceiveRegistrationToken:')]
    procedure messagingDidReceiveRegistrationToken(messaging: FIRMessaging; didReceiveRegistrationToken: NSString); cdecl;

    // This method will be called whenever FCM receives a new, default FCM token for your
    // Firebase project's Sender ID. This method is deprecated. Please use
    // `messaging:didReceiveRegistrationToken:`.
    // - (void)messaging:(nonnull FIRMessaging *)messaging didRefreshRegistrationToken:(nonnull NSString *)fcmToken NS_SWIFT_NAME(messaging(_:didRefreshRegistrationToken:)) __deprecated_msg("Please use messaging:didReceiveRegistrationToken:, which is called for both current and refreshed tokens.");
    //[MethodName('messaging:didRefreshRegistrationToken:')]
    //procedure messagingDidRefreshRegistrationToken(messaging: FIRMessaging; didRefreshRegistrationToken: NSString); cdecl; deprecated 'Please use messaging:didReceiveRegistrationToken:, which is called for both current and refreshed tokens.';

    // This method is called on iOS 10 devices to handle data messages received via FCM through its
    // direct channel (not via APNS). For iOS 9 and below, the FCM data message is delivered via the
    // UIApplicationDelegate's -application:didReceiveRemoteNotification: method.
    // - (void)messaging:(nonnull FIRMessaging *)messaging didReceiveMessage:(nonnull FIRMessagingRemoteMessage *)remoteMessage NS_SWIFT_NAME(messaging(_:didReceive:)) __IOS_AVAILABLE(10.0);
    [MethodName('messaging:didReceiveMessage:')]
    procedure messagingDidReceiveMessage(messaging: FIRMessaging; didReceiveMessage: FIRMessagingRemoteMessage); cdecl;

    // The callback to handle data message received via FCM for devices running iOS 10 or above.
    // - (void)applicationReceivedRemoteMessage:(nonnull FIRMessagingRemoteMessage *)remoteMessage NS_SWIFT_NAME(application(received:)) __deprecated_msg("Use FIRMessagingDelegate’s -messaging:didReceiveMessage:");
    // procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl; deprecated 'Use FIRMessagingDelegate’s -messaging:didReceiveMessage:';

  end;

  //The top level Firebase Analytics singleton that provides methods for logging events and setting
  //user properties. See <a href="http://goo.gl/gz8SLz">the developer guides</a> for general
  //information on using Firebase Analytics in your apps.
  //NS_SWIFT_NAME(Analytics)
  //@interface FIRAnalytics : NSObject
  FIRAnalyticsClass = interface(NSObjectClass)
    ['{0E46F730-2EC0-462F-BAF0-E56DA5E43CC7}']

    //Logs an app event. The event can have up to 25 parameters. Events with the same name must have
    //the same parameters. Up to 500 event names are supported. Using predefined events and/or
    //parameters is recommended for optimal reporting.
    //
    //The following event names are reserved and cannot be used:
    //<ul>
    //    <li>ad_activeview</li>
    //    <li>ad_click</li>
    //    <li>ad_exposure</li>
    //    <li>ad_impression</li>
    //    <li>ad_query</li>
    //    <li>adunit_exposure</li>
    //    <li>app_clear_data</li>
    //    <li>app_remove</li>
    //    <li>app_update</li>
    //    <li>error</li>
    //    <li>first_open</li>
    //    <li>in_app_purchase</li>
    //    <li>notification_dismiss</li>
    //    <li>notification_foreground</li>
    //    <li>notification_open</li>
    //    <li>notification_receive</li>
    //    <li>os_update</li>
    //    <li>screen_view</li>
    //    <li>session_start</li>
    //    <li>user_engagement</li>
    //</ul>
    //
    //@param name The name of the event. Should contain 1 to 40 alphanumeric characters or
    //    underscores. The name must start with an alphabetic character. Some event names are
    //    reserved. See FIREventNames.h for the list of reserved event names. The "firebase_",
    //    "google_", and "ga_" prefixes are reserved and should not be used. Note that event names are
    //    case-sensitive and that logging two events whose names differ only in case will result in
    //    two distinct events.
    //@param parameters The dictionary of event parameters. Passing nil indicates that the event has
    //    no parameters. Parameter names can be up to 40 characters long and must start with an
    //    alphabetic character and contain only alphanumeric characters and underscores. Only NSString
    //    and NSNumber (signed 64-bit integer and 64-bit floating-point number) parameter types are
    //    supported. NSString parameter values can be up to 100 characters long. The "firebase_",
    //    "google_", and "ga_" prefixes are reserved and should not be used for parameter names.
    //+ (void)logEventWithName:(NSString *)name
    //              parameters:(nullable NSDictionary<NSString *, id> *)parameters NS_SWIFT_NAME(logEvent(_:parameters:));
    { class } procedure logEventWithName(name: NSString; parameters: NSDictionary); cdecl;

    //Sets a user property to a given value. Up to 25 user property names are supported. Once set,
    //user property values persist throughout the app lifecycle and across sessions.
    //
    //The following user property names are reserved and cannot be used:
    //<ul>
    //    <li>first_open_time</li>
    //    <li>last_deep_link_referrer</li>
    //    <li>user_id</li>
    //</ul>
    //
    //@param value The value of the user property. Values can be up to 36 characters long. Setting the
    //    value to nil removes the user property.
    //@param name The name of the user property to set. Should contain 1 to 24 alphanumeric characters
    //    or underscores and must start with an alphabetic character. The "firebase_", "google_", and
    //    "ga_" prefixes are reserved and should not be used for user property names.
    //+ (void)setUserPropertyString:(nullable NSString *)value forName:(NSString *)name NS_SWIFT_NAME(setUserProperty(_:forName:));
    { class } procedure setUserPropertyString(value: NSString; forName: NSString); cdecl;


    //Sets the user ID property. This feature must be used in accordance with
    //<a href="https://www.google.com/policies/privacy">Google's Privacy Policy</a>
    //
    //@param userID The user ID to ascribe to the user of this app on this device, which must be
    //    non-empty and no more than 256 characters long. Setting userID to nil removes the user ID.
    //+ (void)setUserID:(nullable NSString *)userID;
    { class } procedure setUserID(userID: NSString); cdecl;

    //Sets the current screen name, which specifies the current visual context in your app. This helps
    //identify the areas in your app where users spend their time and how they interact with your app.
    //Must be called on the main thread.
    //
    //Note that screen reporting is enabled automatically and records the class name of the current
    //UIViewController for you without requiring you to call this method. If you implement
    //viewDidAppear in your UIViewController but do not call [super viewDidAppear:], that screen class
    //will not be automatically tracked. The class name can optionally be overridden by calling this
    //method in the viewDidAppear callback of your UIViewController and specifying the
    //screenClassOverride parameter. setScreenName:screenClass: must be called after
    //[super viewDidAppear:].
    //
    //If your app does not use a distinct UIViewController for each screen, you should call this
    //method and specify a distinct screenName each time a new screen is presented to the user.
    //
    //The screen name and screen class remain in effect until the current UIViewController changes or
    //a new call to setScreenName:screenClass: is made.
    //
    //@param screenName The name of the current screen. Should contain 1 to 100 characters. Set to nil
    //    to clear the current screen name.
    //@param screenClassOverride The name of the screen class. Should contain 1 to 100 characters. By
    //    default this is the class name of the current UIViewController. Set to nil to revert to the
    //    default class name.
    //+ (void)setScreenName:(nullable NSString *)screenName
    //          screenClass:(nullable NSString *)screenClassOverride;
    { class } procedure setScreenName(screenName: NSString; screenClass: NSString); cdecl;

    //The unique ID for this instance of the application.
    //+ (NSString *)appInstanceID;
    { class } function appInstanceID: NSString; cdecl;

  end;
  FIRAnalytics = interface(NSObject)
    ['{3548840A-4932-4CB2-92CD-B9CA1AAFA966}']
  end;
  TFIRAnalytics = class(TOCGenericImport<FIRAnalyticsClass, FIRAnalytics>) end;

implementation

uses Macapi.Helpers,
     System.Sqlite; // << else I have Error: "_sqlite3_errstr", referenced from: ....

{********************************************************}
function kFIRInstanceIDTokenRefreshNotification: NSString;
begin
  // http://stackoverflow.com/questions/43592047/delphi-ios-how-to-import-const-defined-in-3rd-party-library
  // i don't yet find a way to import a Const defined in 3rd party library
  // so i write here the static value of this const
  result := StrToNsStr('com.firebase.iid.notif.refresh-token');
end;

{$IF defined(CPUARM)}

// you must also add in the linker flags -Objc else you have the error
// Terminating app due to uncaught exception 'NSInvalidArgumentException', reason: '-[__NSCFString fira_UTF32Length]: unrecognized selector sent to instance 0x1758c350
// this is also written in the firebase doc: https://firebase.google.com/docs/ios/setup (Add the ObjC linker flag in your Other Linker Settings in your target's build settings.)
// but when i add -ObjC i have this error : https://stackoverflow.com/questions/49722316/why-under-ios-when-i-add-objc-i-have-error-ld-framework-not-found-fbsdkcorekit
// so i Add instead -force_load:
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\FirebaseCore.framework\FirebaseCore
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\FirebaseCoreDiagnostics.framework\FirebaseCoreDiagnostics
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\FirebaseAnalytics.framework\FirebaseAnalytics
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\FirebaseInstanceID.framework\FirebaseInstanceID
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\FirebaseMessaging.framework\FirebaseMessaging
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\FirebaseNanoPB.framework\FirebaseNanoPB
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\nanopb.framework\nanopb
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\Protobuf.framework\Protobuf
// -force_load C:\Dev\Alcinoe\lib\ios\firebase\GoogleToolboxForMac.framework\GoogleToolboxForMac

procedure StubProc1;  cdecl; external 'FirebaseCore'         name 'OBJC_CLASS_$_FIRApp';
procedure StubProc2;  cdecl; external 'FirebaseInstanceID'   name 'OBJC_CLASS_$_FIRInstanceID';
procedure StubProc3;  cdecl; external 'FirebaseMessaging'    name 'OBJC_CLASS_$_FIRMessagingMessageInfo';
procedure StubProc4;  cdecl; external 'FirebaseMessaging'    name 'OBJC_CLASS_$_FIRMessagingRemoteMessage';
procedure StubProc5;  cdecl; external 'FirebaseMessaging'    name 'OBJC_CLASS_$_FIRMessagingDelegate';
procedure StubProc6;  cdecl; external 'FirebaseMessaging'    name 'OBJC_CLASS_$_FIRMessaging';
procedure StubProc7;  cdecl; external 'FirebaseAnalytics'    name 'OBJC_CLASS_$_FIRAnalytics'; // << else the firebase analytics will be not included
procedure StubProc8;  cdecl; external 'FirebaseNanoPB'       name 'nano_decode_repeated_string'; // << else (without -force_load: FirebaseNanoPB) I have Error: "_nano_decode_repeated_string", referenced from: ....
procedure StubProc9;  cdecl; external 'nanopb'               name 'pb_encode_varint'; // << else (without -force_load: nanopb) I have Error: "_pb_encode_varint", referenced from: ....
procedure StubProc10; cdecl; external 'Protobuf'             name 'OBJC_CLASS_$_GPBRootObject'; // else (without -force_load: Protobuf) I have Error: "_OBJC_CLASS_$_GPBRootObject", referenced from: ...
procedure StubProc11; cdecl; external '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration' name 'SCNetworkReachabilityUnscheduleFromRunLoop'; // else I have Error: "_SCNetworkReachabilityUnscheduleFromRunLoop", referenced from: ....
procedure StubProc12; cdecl; external '/System/Library/Frameworks/StoreKit.framework/StoreKit' name 'OBJC_CLASS_$_SKPaymentQueue'; // else I have Error: "_OBJC_CLASS_$_SKPaymentQueue", referenced from: ...

{$ELSE}

// i don't know how to do under ios simulator :(

{$ENDIF}

end.
