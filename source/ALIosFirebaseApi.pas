unit ALIosFirebaseApi;

interface

uses Macapi.ObjectiveC,
     iOSapi.CocoaTypes,
     iOSapi.Foundation;

{$M+}

type

  {**********************************}
  // The entry point of Firebase SDKs.
  //
  // Initialize and configure FIRApp using +[FIRApp configure]
  // or other customized ways as shown below.
  //
  // The logging system has two modes: default mode and debug mode. In default mode, only logs with
  // log level Notice, Warning and Error will be sent to device. In debug mode, all logs will be sent
  // to device. The log levels that Firebase uses are consistent with the ASL log levels.
  //
  // Enable debug mode by passing the -FIRDebugEnabled argument to the application. You can add this
  // argument in the application's Xcode scheme. When debug mode is enabled via -FIRDebugEnabled,
  // further executions of the application will also be in debug mode. In order to return to default
  // mode, you must explicitly disable the debug mode with the application argument -FIRDebugDisabled.
  //
  // It is also possible to change the default logging level in code by calling setLoggerLevel: on
  // the FIRConfiguration interface.
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
    // + (void)configureWithOptions:(FIROptions *)options;
    //procedure configureWithOptions(options: FIROptions);

    // Configures a Firebase app with the given name and options. Raises an exception if any
    // configuration step fails. This method is thread safe.
    // @param name The application's name given by the developer. The name should should only contain
    //             Letters, Numbers and Underscore.
    // @param options The Firebase application options used to configure the services.
    // + (void)configureWithName:(NSString *)name options:(FIROptions *)options;
    //procedure configureWithName(name: NSString; options: FIROptions);

    // Returns the default app, or nil if the default app does not exist.
    // + (nullable FIRApp *)defaultApp NS_SWIFT_NAME(defaultApp());

    // Returns a previously created FIRApp instance with the given name, or nil if no such app exists.
    // This method is thread safe.
    // + (nullable FIRApp *)appNamed:(NSString *)name;

    // Returns the set of all extant FIRApp instances, or nil if there are no FIRApp instances. This
    // method is thread safe.
    // + (nullable NSDictionary *)allApps;

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

    // Gets the options for this app.
    // @property(nonatomic, readonly) FIROptions *options;

  end;
  TFIRApp = class(TOCGenericImport<FIRAppClass, FIRApp>) end;

  {************************}
  // @memberof FIRInstanceID
  // The scope to be used when fetching/deleting a token for Firebase Messaging.
  // FOUNDATION_EXPORT NSString * __nonnull const kFIRInstanceIDScopeFirebaseMessaging;

  {********************************************************************}
  // Called when the system determines that tokens need to be refreshed.
  // This method is also called if Instance ID has been reset in which
  // case, tokens and FCM topic subscriptions also need to be refreshed.
  // Instance ID service will throttle the refresh event across all devices
  // to control the rate of token updates on application servers.
  // FOUNDATION_EXPORT NSString * __nonnull const kFIRInstanceIDTokenRefreshNotification;
  function kFIRInstanceIDTokenRefreshNotification: NSString; cdecl;

type

  {*********************************************************************}
  // The completion handler invoked when the InstanceID token returns. If
  // the call fails we return the appropriate `error code` as described below.
  // @param token The valid token as returned by InstanceID backend.
  // @param error The error describing why generating a new token
  //              failed. See the error codes below for a more detailed
  //              description.
  // typedef void(^FIRInstanceIDTokenHandler)( NSString * __nullable token, NSError * __nullable error);

  {***********************}
  // @related FIRInstanceID
  // The completion handler invoked when the InstanceID `deleteToken` returns. If
  // the call fails we return the appropriate `error code` as described below
  // @param error The error describing why deleting the token failed.
  //              See the error codes below for a more detailed description.
  // typedef void(^FIRInstanceIDDeleteTokenHandler)(NSError * __nullable error);

  {***********************}
  // @related FIRInstanceID
  // The completion handler invoked when the app identity is created. If the
  // identity wasn't created for some reason we return the appropriate error code.
  // @param identity A valid identity for the app instance, nil if there was an error
  //                 while creating an identity.
  // @param error    The error if fetching the identity fails else nil.
  // typedef void(^FIRInstanceIDHandler)(NSString * __nullable identity, NSError * __nullable error);

  {***********************}
  // @related FIRInstanceID
  // The completion handler invoked when the app identity and all the tokens associated
  // with it are deleted. Returns a valid error object in case of failure else nil.
  // @param error The error if deleting the identity and all the tokens associated with
  //              it fails else nil.
  // typedef void(^FIRInstanceIDDeleteHandler)(NSError * __nullable error);
  FIRInstanceIDDeleteHandler = procedure(error: NSError) of object;

Type

  // Http related errors.
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
  FIRInstanceIDAPNSTokenTypeUnknown = 0;

  // Sandbox token type.
  FIRInstanceIDAPNSTokenTypeSandbox = 1;

  // Production token type.
  FIRInstanceIDAPNSTokenTypeProd = 2;

type

  {*******************************************************************************}
  // Instance ID provides a unique identifier for each app instance and a mechanism
  // to authenticate and authorize actions (for example, sending an FCM message).
  // Instance ID is long lived but, may be reset if the device is not used for
  // a long time or the Instance ID service detects a problem.
  // If Instance ID is reset, the app will be notified via
  // `kFIRInstanceIDTokenRefreshNotification`.
  // If the Instance ID has become invalid, the app can request a new one and
  // send it to the app server.
  // To prove ownership of Instance ID and to allow servers to access data or
  // services associated with the app, call
  // `[FIRInstanceID tokenWithAuthorizedEntity:scope:options:handler]`.
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
    // - (void)setAPNSToken:(nonnull NSData *)token type:(FIRInstanceIDAPNSTokenType)type;

    // #pragma mark - Tokens
    // Returns a Firebase Messaging scoped token for the firebase app.
    // @return Null Returns null if the device has not yet been registerd with
    //         Firebase Message else returns a valid token.
    // - (nullable NSString *)token;
    function token: NSString; cdecl;

    // Returns a token that authorizes an Entity (example: cloud service) to perform
    // an action on behalf of the application identified by Instance ID.
    // This is similar to an OAuth2 token except, it applies to the
    // application instance instead of a user.
    // This is an asynchronous call. If the token fetching fails for some reason
    // we invoke the completion callback with nil `token` and the appropriate
    // error.
    // Note, you can only have one `token` or `deleteToken` call for a given
    // authorizedEntity and scope at any point of time. Making another such call with the
    // same authorizedEntity and scope before the last one finishes will result in an
    // error with code `OperationInProgress`.
    // @see FIRInstanceID deleteTokenWithAuthorizedEntity:scope:handler:
    // @param authorizedEntity Entity authorized by the token.
    // @param scope            Action authorized for authorizedEntity.
    // @param options          The extra options to be sent with your token request. The
    //                         value for the `apns_token` should be the NSData object
    //                         passed to UIApplication's
    //                         `didRegisterForRemoteNotificationsWithDeviceToken` method.
    //                         All other keys and values in the options dict need to be
    //                         instances of NSString or else they will be discarded. Bundle
    //                         keys starting with 'GCM.' and 'GOOGLE.' are reserved.
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
    // @param handler The handler to invoke once the identifier has been fetched.
    //                In case of error an appropriate error object is returned else
    //                a valid identifier is returned and a valid identifier for the
    //                application instance.
    // - (void)getIDWithHandler:(nonnull FIRInstanceIDHandler)handler;

    // Resets Instance ID and revokes all tokens.
    // - (void)deleteIDWithHandler:(nonnull FIRInstanceIDDeleteHandler)handler;
    procedure deleteIDWithHandler(handler: FIRInstanceIDDeleteHandler); cdecl;

  end;
  TFIRInstanceID = class(TOCGenericImport<FIRInstanceIDClass, FIRInstanceID>) end;

  // The completion handler invoked once the data connection with FIRMessaging is
  // established.  The data connection is used to send a continous stream of
  // data and all the FIRMessaging data notifications arrive through this connection.
  // Once the connection is established we invoke the callback with `nil` error.
  // Correspondingly if we get an error while trying to establish a connection
  // we invoke the handler with an appropriate error object and do an
  // exponential backoff to try and connect again unless successful.
  // @param error The error object if any describing why the data connection
  //              to FIRMessaging failed.
  // typedef void(^FIRMessagingConnectCompletion)(NSError * __nullable error);
  FIRMessagingConnectCompletion = procedure(error: NSError) of object;

  // Notification sent when the upstream message has been delivered
  // successfully to the server. The notification object will be the messageID
  // of the successfully delivered message.
  // FOUNDATION_EXPORT NSString * __nonnull const FIRMessagingSendSuccessNotification;

  // Notification sent when the upstream message was failed to be sent to the
  // server.  The notification object will be the messageID of the failed
  // message. The userInfo dictionary will contain the relevant error
  // information for the failure.
  // FOUNDATION_EXPORT NSString * __nonnull const FIRMessagingSendErrorNotification;

  // Notification sent when the Firebase messaging server deletes pending
  // messages due to exceeded storage limits. This may occur, for example, when
  // the device cannot be reached for an extended period of time.
  // It is recommended to retrieve any missing messages directly from the
  // server.
  // FOUNDATION_EXPORT NSString * __nonnull const FIRMessagingMessagesDeletedNotification;


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

  {************************************************************}
  // Information about a downstream message received by the app.
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

  {********************************************************************************}
  // A protocol to receive data message via FCM for devices running iOS 10 or above.
  // To support devices running iOS 9 or below, use the local and remote notifications handlers
  // defined in UIApplicationDelegate protocol.
  // @protocol FIRMessagingDelegate <NSObject>
  FIRMessagingDelegate = interface(IObjectiveC)
  ['{9784786A-515F-41F0-84C3-8F298623275E}']

    // The callback to handle data message received via FCM for devices running iOS 10 or above.
    // - (void)applicationReceivedRemoteMessage:(nonnull FIRMessagingRemoteMessage *)remoteMessage;
    procedure applicationReceivedRemoteMessage(remoteMessage: FIRMessagingRemoteMessage); cdecl;

  end;


  {******************************************************************}
  // Firebase Messaging lets you reliably deliver messages at no cost.
  // To send or receive messages, the app must get a
  // registration token from FIRInstanceID. This token authorizes an
  // app server to send messages to an app instance.
  // In order to receive FIRMessaging messages, declare `application:didReceiveRemoteNotification:`.
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

    // Delegate to handle remote data messages received via FCM for devices running iOS 10 or above.
    // @property(nonatomic, weak, nullable) id<FIRMessagingDelegate> remoteMessageDelegate;
    procedure setRemoteMessageDelegate(delegate: Pointer); cdecl;
    function remoteMessageDelegate: Pointer; cdecl;

    // Unavailable. Use +messaging instead.
    // - (nonnull instancetype)init __attribute__((unavailable("Use +messaging instead.")));

    // #pragma mark - Connect
    // Create a FIRMessaging data connection which will be used to send the data notifications
    // sent by your server. It will also be used to send ACKS and other messages based
    // on the FIRMessaging ACKS and other messages based  on the FIRMessaging protocol.
    // @param handler  The handler to be invoked once the connection is established.
    //                 If the connection fails we invoke the handler with an
    //                 appropriate error code letting you know why it failed. At
    //                 the same time, FIRMessaging performs exponential backoff to retry
    //                 establishing a connection and invoke the handler when successful.
    // - (void)connectWithCompletion:(nonnull FIRMessagingConnectCompletion)handler;
    procedure connectWithCompletion(handler: FIRMessagingConnectCompletion); cdecl;

    // Disconnect the current FIRMessaging data connection. This stops any attempts to
    // connect to FIRMessaging. Calling this on an already disconnected client is a no-op.
    // Call this before `teardown` when your app is going to the background.
    // Since the FIRMessaging connection won't be allowed to live when in background it is
    // prudent to close the connection.
    // - (void)disconnect;
    procedure disconnect; cdecl;

    // #pragma mark - Topics
    // Asynchronously subscribes to a topic.
    // @param topic The name of the topic, for example, @"sports".
    // - (void)subscribeToTopic:(nonnull NSString *)topic;

    // Asynchronously unsubscribe from a topic.
    // @param topic The name of the topic, for example @"sports".
    // - (void)unsubscribeFromTopic:(nonnull NSString *)topic;

    // #pragma mark - Upstream
    // Sends an upstream ("device to cloud") message.
    // The message is queued if we don't have an active connection.
    // You can only use the upstream feature if your FCM implementation
    // uses the XMPP server protocol.
    // @param message      Key/Value pairs to be sent. Values must be String, any
    //                     other type will be ignored.
    // @param to           A string identifying the receiver of the message. For FCM
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
    // flag to NO in your Info.plist. If `FirebaseAppDelegateProxyEnabled` is either missing
    // or set to YES in your Info.plist, the library will call this automatically.
    // @param message The downstream message received by the application.
    // @return Information about the downstream message.
    // - (nonnull FIRMessagingMessageInfo *)appDidReceiveMessage:(nonnull NSDictionary *)message;

  end;
  TFIRMessaging = class(TOCGenericImport<FIRMessagingClass, FIRMessaging>) end;
  PFIRMessaging = Pointer;

{*******************************************************************************************}
// http://stackoverflow.com/questions/43843006/how-to-call-in-delphi-an-ios-variadic-function
// @abstract Logs a message to the Firebase Crash Reporter system.
// @discussion This method adds a message to the crash reporter
//   logging system.  The recent logs will be sent with the crash
//   report when the application exits abnormally.  Note that the
//   timestamp of this message and the timestamp of the console
//   message may differ by a few milliseconds.
// Messages should be brief as the total size of the message payloads
//   is limited by the uploader and may change between releases of the
//   crash reporter.  Excessively long messages will be truncated
//   safely but that will introduce a delay in submitting the message.
// @warning Raises an NSInvalidArgumentException if @p format is nil.
// @param format A format string.
// @param ap A variable argument list.
// FOUNDATION_EXTERN NS_FORMAT_FUNCTION(1, 0)
// void FIRCrashLogv(NSString *format, va_list ap);
procedure FIRCrashLogv(format: PNSString); cdecl; varargs; external 'FirebaseCrash' name 'FIRCrashLogv';

implementation

uses Macapi.Helpers,
     System.Sqlite, // << because we need to link the lib
     System.ZLib; // << because we need to link the lib

{********************************************************}
function kFIRInstanceIDTokenRefreshNotification: NSString;
begin
  // http://stackoverflow.com/questions/43592047/delphi-ios-how-to-import-const-defined-in-3rd-party-library
  // i don't yet find a way to import a Const defined in 3rd party library
  // so i write here the static value of this const
  result := StrToNsStr('com.firebase.iid.notif.refresh-token');
end;

{$IF defined(CPUARM)}

procedure StubProc1; cdecl; external 'FirebaseCore' name 'OBJC_CLASS_$_FIRApp';
procedure StubProc2; cdecl; external 'FirebaseAnalytics' name 'OBJC_CLASS_$_FIRAnalytics';
procedure StubProc3; cdecl; external 'FirebaseInstanceID' name 'OBJC_CLASS_$_FIRInstanceID';
procedure StubProc4; cdecl; external 'GoogleToolboxForMac' name 'OBJC_CLASS_$_GTMABAddressBook';
procedure StubProc5; cdecl; external 'FirebaseMessaging' name 'OBJC_CLASS_$_FIRMessaging';
procedure StubProc6; cdecl; external 'Protobuf' name 'OBJC_CLASS_$_GPBRootObject';
procedure StubProc7; cdecl; external '/usr/lib/libc++.dylib';

{$ELSE}

// i don't know how to do under ios simulator :(

{$ENDIF}

end.
