unit ALIosAppsFlyerApi;

interface

uses Macapi.ObjectiveC,
     iOSapi.Foundation;

{$M+}

//const

  //EmailCryptTypeNone = 0;
  //EmailCryptTypeSHA1 = 1;
  //EmailCryptTypeMD5 = 2;
  //EmailCryptTypeSHA256 = 3;

type

  //AppsFlyerCrossPromotionHelper = interface;
  //AppsFlyerLinkGenerator = interface;
  //AppsFlyerShareInviteHelper = interface;
  //AppsFlyerTrackerDelegate = interface;
  AppsFlyerTracker = interface;
  //TAppsFlyerLibOpenStore = procedure(param1: NSURLSession; param2: NSURL) of object;
  //TAppsFlyerLibGeneratorCreator = function(param1: AppsFlyerLinkGenerator): AppsFlyerLinkGenerator; cdecl;
  //TAppsFlyerLibCompletionHandler = procedure(param1: NSURL) of object;
  //EmailCryptType = Cardinal;
  //TAppsFlyerLibSuccess = procedure(param1: NSDictionary) of object;
  //TAppsFlyerLibFailure = procedure(param1: NSError; param2: Pointer) of object;
  //TAppsFlyerLibRestorationHandler = procedure(param1: NSArray) of object;
  //NSUInteger = Cardinal;
  //PNSUInteger = ^NSUInteger;

  {***********************************************************}
  //@interface AppsFlyerCrossPromotionHelper : NSObject
  AppsFlyerCrossPromotionHelperClass = interface(NSObjectClass)
    ['{8EB76D9A-7F56-4876-AA30-C9E59CB6BD80}']

    //+ (void) trackCrossPromoteImpression:(nonnull NSString*) appID
    //                            campaign:(nullable NSString*) campaign;
    //{ class } procedure trackCrossPromoteImpression(appID: NSString; campaign: NSString); cdecl;

    //+ (void) trackAndOpenStore:(nonnull NSString*) appID
    //                  campaign:(nullable NSString *) campaign
    //                 paramters:(nullable NSDictionary*) parameters
    //                 openStore:(void (^)(NSURLSession *urlSession,NSURL *clickURL))openStoreBlock;
    //{ class } procedure trackAndOpenStore(appID: NSString; campaign: NSString; paramters: NSDictionary; openStore: TAppsFlyerLibOpenStore); cdecl;

  end;
  AppsFlyerCrossPromotionHelper = interface(NSObject)
    ['{7D86BE3A-C13E-4CCE-BD5A-8902EE445F24}']
  end;
  TAppsFlyerCrossPromotionHelper = class(TOCGenericImport<AppsFlyerCrossPromotionHelperClass, AppsFlyerCrossPromotionHelper>) end;
  PAppsFlyerCrossPromotionHelper = Pointer;

  {*******************************************************************************************************************}
  //Payload container for the `generateInviteUrlWithLinkGenerator:completionHandler:` from `AppsFlyerShareInviteHelper`
  //@interface AppsFlyerLinkGenerator: NSObject
  AppsFlyerLinkGeneratorClass = interface(NSObjectClass)
    ['{70AEA3EE-5AB3-4280-8DE7-4FF9B095A4B9}']
  end;
  AppsFlyerLinkGenerator = interface(NSObject)
    ['{218E4C68-6891-44CF-95D1-3609EFB6CA54}']

    //The channel through which the invite was sent (e.g. Facebook/Gmail/etc.). Usage: Recommended
    //- (void)setChannel           :(nonnull NSString *)channel;
    //procedure setChannel(channel: NSString); cdecl;

    //- (void)setReferrerCustomerId:(nonnull NSString *)referrerCustomerId;
    //procedure setReferrerCustomerId(referrerCustomerId: NSString); cdecl;

    //A campaign name. Usage: Optional
    //- (void)setCampaign          :(nonnull NSString *)campaign;
    //procedure setCampaign(campaign: NSString); cdecl;

    //- (void)setReferrerUID       :(nonnull NSString *)referrerUID;
    //procedure setReferrerUID(referrerUID: NSString); cdecl;

    //- (void)setReferrerName      :(nonnull NSString *)referrerName;
    //procedure setReferrerName(referrerName: NSString); cdecl;

    // The URL to referrer user avatar. Usage: Optional
    //- (void)setReferrerImageURL  :(nonnull NSString *)referrerImageURL;
    //procedure setReferrerImageURL(referrerImageURL: NSString); cdecl;

    //- (void)setAppleAppID        :(nonnull NSString *)appleAppID;
    //procedure setAppleAppID(appleAppID: NSString); cdecl;

    //- (void)setDeeplinkPath      :(nonnull NSString *)deeplinkPath;
    //procedure setDeeplinkPath(deeplinkPath: NSString); cdecl;

    //- (void)setBaseDeeplink      :(nonnull NSString *)baseDeeplink;
    //procedure setBaseDeeplink(baseDeeplink: NSString); cdecl;

    //A single key value custom parameter. Usage: Optional
    //- (void)addParameterValue    :(nonnull NSString *)value forKey:(NSString*)key;
    //procedure addParameterValue(value: NSString; forKey: NSString); cdecl;

    //Multiple key value custom parameters. Usage: Optional
    //- (void)addParameters        :(nonnull NSDictionary *)parameters;
    //procedure addParameters(parameters: NSDictionary); cdecl;

  end;
  TAppsFlyerLinkGenerator = class(TOCGenericImport<AppsFlyerLinkGeneratorClass, AppsFlyerLinkGenerator>) end;
  PAppsFlyerLinkGenerator = Pointer;

  {************************************************}
  //@interface AppsFlyerShareInviteHelper : NSObject
  AppsFlyerShareInviteHelperClass = interface(NSObjectClass)
    ['{9451C346-0EFD-4983-B923-612633F005CB}']

    //The AppsFlyerShareInviteHelper class builds the invite URL according to various setter methods
    //which allow passing on additional information on the click.
    //This information is available through `onConversionDataReceived:` when the user accepts the invite and installs the app.
    //In addition, campaign and channel parameters are visible within the AppsFlyer Dashboard.
    //+ (void) generateInviteUrlWithLinkGenerator:(AppsFlyerLinkGenerator * (^)(AppsFlyerLinkGenerator *generator))generatorCreator
    //                          completionHandler:(void (^)(NSURL * _Nullable url))completionHandler;
    //{ class } procedure generateInviteUrlWithLinkGenerator(generatorCreator: TAppsFlyerLibGeneratorCreator; completionHandler: TAppsFlyerLibCompletionHandler); cdecl;

    //It is recommended to generate an in-app event after the invite is sent to track the invites from the senders' perspective.
    //This enables you to find the users that tend most to invite friends, and the media sources that get you these users.
    //+ (void) trackInvite:(nullable NSString *)channel parameters:(nullable NSDictionary *)parameters;
    //{ class } procedure trackInvite(channel: NSString; parameters: NSDictionary); cdecl;

  end;
  AppsFlyerShareInviteHelper = interface(NSObject)
    ['{A1A7F259-7045-4A34-97AB-5824DEB757A5}']
  end;
  TAppsFlyerShareInviteHelper = class(TOCGenericImport<AppsFlyerShareInviteHelperClass, AppsFlyerShareInviteHelper>) end;
  PAppsFlyerShareInviteHelper = Pointer;

  {**************************************}
  //@interface AppsFlyerTracker : NSObject
  AppsFlyerTrackerClass = interface(NSObjectClass)
    ['{E5868A6F-CEED-423D-81AE-52A9301186CA}']

    //+(AppsFlyerTracker*) sharedTracker;
    { class } function sharedTracker: AppsFlyerTracker; cdecl;

  end;
  AppsFlyerTracker = interface(NSObject)
    ['{8041C4ED-3F00-4F30-8BE2-601956831FC6}']

    //In case you use your own user ID in your app, you can set this property to that ID.
    //@property (nonatomic, strong, setter=setCustomerUserID:) NSString *customerUserID;
    procedure setCustomerUserID(customerUserID: NSString); cdecl;
    function customerUserID: NSString; cdecl;

    //In case you use Custom data and you want to receive it in the raw reports.
    //@property (nonatomic, strong, setter=setAdditionalData:) NSDictionary *customData;
    //procedure setAdditionalData(customData: NSDictionary); cdecl;
    //function customData: NSDictionary; cdecl;

    //Use this property to set your AppsFlyer's dev key.
    //@property (nonatomic, strong, setter=setAppsFlyerDevKey:) NSString *appsFlyerDevKey;
    procedure setAppsFlyerDevKey(appsFlyerDevKey: NSString); cdecl;
    function appsFlyerDevKey: NSString; cdecl;

    //Use this property to set your app's Apple ID (taken from the app's page on iTunes Connect)
    //@property (nonatomic, strong, setter=setAppleAppID:) NSString *appleAppID;
    procedure setAppleAppID(appleAppID: NSString); cdecl;
    function appleAppID: NSString; cdecl;

    //In case of in app purchase events, you can set the currency code your user has purchased with.
    //The currency code is a 3 letter code according to ISO standards. Example: "USD"
    //@property (nonatomic, strong) NSString *currencyCode;
    //procedure setCurrencyCode(currencyCode: NSString); cdecl;
    //function currencyCode: NSString; cdecl;

    //AppsFLyer SDK collect Apple's advertisingIdentifier if the AdSupport framework included in the SDK.
    //You can disable this behavior by setting the following property to YES.
    //@property BOOL disableAppleAdSupportTracking;
    //procedure setDisableAppleAdSupportTracking(disableAppleAdSupportTracking: Boolean); cdecl;
    //function disableAppleAdSupportTracking: Boolean; cdecl;

    //Prints our messages to the log. This property should only be used in DEBUG mode. The default value
    //is NO.
    //@property (nonatomic, setter = setIsDebug:) BOOL isDebug;
    procedure setIsDebug(isDebug: Boolean); cdecl;
    function isDebug: Boolean; cdecl;

    //Set this flag to `YES`, to collect the current device name. Default value is `NO`
    //@property (nonatomic, setter = setShouldCollectDeviceName:) BOOL shouldCollectDeviceName;
    //procedure setShouldCollectDeviceName(shouldCollectDeviceName: Boolean); cdecl;
    //function shouldCollectDeviceName: Boolean; cdecl;

    //@property (nonatomic, setter = setAppInviteOneLink:) NSString* appInviteOneLinkID;
    //procedure setAppInviteOneLink(appInviteOneLinkID: NSString); cdecl;
    //function appInviteOneLinkID: NSString; cdecl;

    //Opt-out tracking for specific user
    //@property BOOL deviceTrackingDisabled;
    //procedure setDeviceTrackingDisabled(deviceTrackingDisabled: Boolean); cdecl;
    //function deviceTrackingDisabled: Boolean; cdecl;

    //procedure setDisableIAdTracking(disableIAdTracking: Boolean); cdecl;
    //function disableIAdTracking: Boolean; cdecl;

    //AppsFlyer delegate. See AppsFlyerTrackerDelegate abvoe
    //@property (weak, nonatomic) id<AppsFlyerTrackerDelegate> delegate;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;

    //In app purchase receipt validation Apple environment (production or sandbox). The default value
    //is NO.
    //@property (nonatomic, setter = setUseReceiptValidationSandbox:) BOOL useReceiptValidationSandbox;
    //procedure setUseReceiptValidationSandbox(useReceiptValidationSandbox: Boolean); cdecl;
    //function useReceiptValidationSandbox: Boolean; cdecl;

    //Set this flag to test uninstall on Apple environment (production or sandbox). The default value
    //is NO.
    //@property (nonatomic, setter = setUseUninstallSandbox:) BOOL useUninstallSandbox;
    //procedure setUseUninstallSandbox(useUninstallSandbox: Boolean); cdecl;
    //function useUninstallSandbox: Boolean; cdecl;

    //Advertising Id (exposed for RemoteDebug)
    //@property (nonatomic, strong) NSString *advertiserId;
    //procedure setAdvertiserId(advertiserId: NSString); cdecl;
    //function advertiserId: NSString; cdecl;

    //Use this to send the User's emails
    //-(void) setUserEmails:(NSArray *) userEmails withCryptType:(EmailCryptType) type;
    //procedure setUserEmails(userEmails: NSArray; withCryptType: EmailCryptType); cdecl;

    //Track application launch
    //- (void) trackAppLaunch;
    procedure trackAppLaunch; cdecl;

    //Use this method to track events in your app like purchases or user actions.
    //Example :
    //  [[AppsFlyer sharedTracker] trackEvent:@"hotel-booked" withValue:"200"];
    //- (void) trackEvent:(NSString*)eventName withValue:(NSString*)value __attribute__((deprecated));
    //[MethodName('trackEvent:withValue:')]
    //procedure trackEventWithValue(eventName: NSString; withValue: NSString); cdecl;

    //Use this method to track an events with mulitple values. See AppsFlyer's documentation for details.
    //- (void) trackEvent:(NSString *)eventName withValues:(NSDictionary*)values;
    [MethodName('trackEvent:withValues:')]
    procedure trackEventWithValues(eventName: NSString; withValues: NSDictionary); cdecl;

    //To track in app purchases you can call this method from the completeTransaction: method on
    //your SKPaymentTransactionObserver.
    //- (void) validateAndTrackInAppPurchase:(NSString *)productIdentifier
    //                                 price:(NSString *)price
    //                              currency:(NSString *)currency
    //                         transactionId:(NSString *) tranactionId
    //                  additionalParameters:(NSDictionary *)params
    //                               success:(void (^)(NSDictionary *response))successBlock
    //                               failure:(void (^)(NSError *error, id reponse)) failedBlock NS_AVAILABLE(10_7, 7_0);
    //procedure validateAndTrackInAppPurchase(productIdentifier: NSString;
    //                                        price: NSString;
    //                                        currency: NSString;
    //                                        transactionId: NSString;
    //                                        additionalParameters: NSDictionary;
    //                                        success: TAppsFlyerLibSuccess;
    //                                        failure: TAppsFlyerLibFailure); cdecl;

    //To Track location for geo-fencing.
    //- (void) trackLocation:(double) longitude latitude:(double) latitude;
    //procedure trackLocation(longitude: Double; latitude: Double); cdecl;

    //This method returns AppsFLyer's internal user ID (unique for your app)
    //- (NSString *) getAppsFlyerUID;
    //function getAppsFlyerUID: NSString; cdecl;

    //In case you want to use AppsFlyer tracking data in your app you can use the following method set a
    //delegate with callback buttons for the tracking data. See AppsFlyerTrackerDelegate above.
    //- (void) loadConversionDataWithDelegate:(id<AppsFlyerTrackerDelegate>) delegate __attribute__((deprecated));
    //procedure loadConversionDataWithDelegate(delegate: Pointer); cdecl;

    //In case you want to track deep linking, call this method from your delegate's openURL method.
    //- (void) handleOpenURL:(NSURL *)url sourceApplication:(NSString *)sourceApplication;
    [MethodName('handleOpenURL:sourceApplication:')]
    procedure handleOpenURLSourceApplication(url: NSURL; sourceApplication: NSString); cdecl;

    //In case you want to track deep linking, call this method from your delegate's openURL method with refferer.
    //- (void) handleOpenURL:(NSURL *)url sourceApplication:(NSString *)sourceApplication withAnnotation:(id) annotation;
    [MethodName('handleOpenURL:sourceApplication:withAnnotation:')]
    procedure handleOpenURLSourceApplicationWithAnnotation(url: NSURL; sourceApplication: NSString; withAnnotation: Pointer); cdecl;

    //For Universal links iOS 9
    //- (void) handleOpenUrl:(NSURL *) url options:(NSDictionary *)options;
    procedure handleOpenUrl(url: NSURL; options: NSDictionary); cdecl;

    //- (BOOL) continueUserActivity:(NSUserActivity *) userActivity restorationHandler:(void (^)(NSArray *))restorationHandler NS_AVAILABLE_IOS(9_0);
    //function continueUserActivity(userActivity: NSUserActivity; restorationHandler: TAppsFlyerLibRestorationHandler): Boolean; cdecl;

    //- (void) didUpdateUserActivity:(NSUserActivity *)userActivity NS_AVAILABLE_IOS(9_0);
    //procedure didUpdateUserActivity(userActivity: NSUserActivity); cdecl;

    //- (void) handlePushNotification:(NSDictionary *) pushPayload;
    //procedure handlePushNotification(pushPayload: NSDictionary); cdecl;

    //Register uninstall - you should register for remote notification and provide Appsflyer the push device token.
    //- (void) registerUninstall:(NSData *) deviceToken;
    procedure registerUninstall(deviceToken: NSData); cdecl;

    //Get SDK version.
    //- (NSString *) getSDKVersion;
    //function getSDKVersion: NSString; cdecl;

    //- (void) remoteDebuggingCallWithData:(NSString *) data;
    //procedure remoteDebuggingCallWithData(data: NSString); cdecl;

    //@brief This property accepts a string value representing the host name for all enpoints.
    //@warning To use `default` SDK endpoint – set value to `nil`.
    //@code
    //Objective-C:
    //[[AppsFlyerTracker sharedTracker] setHost:@"example.com"];
    //Swift:
    //AppsFlyerTracker.shared().host = "example.com"
    //@endcode
    //@property (nonatomic, strong) NSString *host;
    //procedure setHost(host: NSString); cdecl;
    //function host: NSString; cdecl;

    //This property is responsible for timeout between sessions in seconds.
    //Default value is 5 seconds.
    //@property (atomic) NSUInteger minTimeBetweenSessions;
    //procedure setMinTimeBetweenSessions(minTimeBetweenSessions: NSUInteger); cdecl;
    //function minTimeBetweenSessions: NSUInteger; cdecl;

    //WARNING! This will disable all requests from AppsFlyer SDK
    //@property (atomic) BOOL isStopTracking;
    //procedure setIsStopTracking(isStopTracking: Boolean); cdecl;
    //function isStopTracking: Boolean; cdecl;

  end;
  TAppsFlyerTracker = class(TOCGenericImport<AppsFlyerTrackerClass, AppsFlyerTracker>) end;
  PAppsFlyerTracker = Pointer;

  {*******************************************************************************************}
  //This delegate should be use if you want to use AppsFlyer conversion data. See AppsFlyer iOS
  //@protocol AppsFlyerTrackerDelegate <NSObject>
  AppsFlyerTrackerDelegate = interface(IObjectiveC)
    ['{A3B2AC81-26C4-49A2-88C6-95D168461F1A}']

    //- (void) onConversionDataReceived:(NSDictionary*) installData;
    procedure onConversionDataReceived(installData: NSDictionary); cdecl;

    //- (void) onConversionDataRequestFailure:(NSError *)error;
    procedure onConversionDataRequestFailure(error: NSError); cdecl;

    //- (void) onAppOpenAttribution:(NSDictionary*) attributionData;
    procedure onAppOpenAttribution(attributionData: NSDictionary); cdecl;

    //- (void) onAppOpenAttributionFailure:(NSError *)error;
    procedure onAppOpenAttributionFailure(error: NSError); cdecl;

  end;

implementation

{$IF defined(CPUARM)}

procedure StubProc1;  cdecl; external 'AppsFlyerLib' name 'OBJC_CLASS_$_AppsFlyerCrossPromotionHelper';
procedure StubProc2;  cdecl; external 'AppsFlyerLib' name 'OBJC_CLASS_$_AppsFlyerLinkGenerator';
procedure StubProc3;  cdecl; external 'AppsFlyerLib' name 'OBJC_CLASS_$_AppsFlyerShareInviteHelper';
procedure StubProc4;  cdecl; external 'AppsFlyerLib' name 'OBJC_CLASS_$_AppsFlyerTracker';
procedure StubProc5;  cdecl; external 'AppsFlyerLib' name 'OBJC_CLASS_$_AppsFlyerTrackerDelegate';
procedure StubProc7;  cdecl; external '/System/Library/Frameworks/iAd.framework/iAd' name 'OBJC_CLASS_$_ADClient'; // << else error [AFSDK.iAD]: AdSupport.framework is missing

{$ELSE}

// i don't know how to do under ios simulator :(

{$ENDIF}

end.

