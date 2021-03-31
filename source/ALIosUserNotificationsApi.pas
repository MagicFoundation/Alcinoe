unit ALIosUserNotificationsApi;

interface

uses
  iOSapi.CoreLocation,
  iOSapi.CocoaTypes,
  Macapi.ObjectiveC,
  iOSapi.Foundation;

{$M+}

type

  {**********************************}
  UNAuthorizationOptions = NSUInteger;
  UNNotificationPresentationOptions = NSUInteger;

const

  {*************************************}
  UNAuthorizationOptionBadge = (1 shl 0);
  UNAuthorizationOptionSound = (1 shl 1);
  UNAuthorizationOptionAlert = (1 shl 2);
  UNAuthorizationOptionCarPlay = (1 shl 3);
  UNNotificationPresentationOptionBadge = (1 shl 0);
  UNNotificationPresentationOptionSound = (1 shl 1);
  UNNotificationPresentationOptionAlert = (1 shl 2);
  UNNotificationPresentationOptionNone = 0;

//const

  {***************************************}
  //UNErrorCodeNotificationsNotAllowed = 1;
  //UNErrorCodeAttachmentInvalidURL = 100;
  //UNErrorCodeAttachmentUnrecognizedType = 101;
  //UNErrorCodeAttachmentInvalidFileSize = 102;
  //UNErrorCodeAttachmentNotInDataStore = 103;
  //UNErrorCodeAttachmentMoveIntoDataStoreFailed = 104;
  //UNErrorCodeAttachmentCorrupt = 105;
  //UNErrorCodeNotificationInvalidNoDate = 1400;
  //UNErrorCodeNotificationInvalidNoContent = 1401;
  //UNNotificationActionOptionAuthenticationRequired = (1 shl 0);
  //UNNotificationActionOptionDestructive = (1 shl 1);
  //UNNotificationActionOptionForeground = (1 shl 2);
  //UNNotificationCategoryOptionNone = (0);
  //UNNotificationCategoryOptionCustomDismissAction = (1 shl 0);
  //UNNotificationCategoryOptionAllowInCarPlay = (2 shl 0);
  //UNAuthorizationStatusNotDetermined = 0;
  //UNAuthorizationStatusDenied = 1;
  //UNAuthorizationStatusAuthorized = 2;
  //UNNotificationSettingNotSupported = 0;
  //UNNotificationSettingDisabled = 1;
  //UNNotificationSettingEnabled = 2;
  //UNAlertStyleNone = 0;
  //UNAlertStyleBanner = 1;
  //UNAlertStyleAlert = 2;

type

  {********************************}
  UNNotificationRequest = interface;
  UNNotification = interface;
  UNNotificationSound = interface;
  UNNotificationContent = interface;
  UNNotificationTrigger = interface;
  UNNotificationResponse = interface;
  UNUserNotificationCenter = interface;
  TUserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler = procedure(granted: Boolean; error: NSError) of object;
  TUserNotificationCenterWillPresentNotificationCompletionHandler = pointer; // and not procedure (options: UNNotificationPresentationOptions); cdecl; because of https://quality.embarcadero.com/browse/RSP-17998?filter=-2
  TUserNotificationCenterDidReceiveNotificationResponseCompletionHandler = pointer; // and not procedure(); cdecl; because of https://quality.embarcadero.com/browse/RSP-17998?filter=-2
  //UNUserNotificationCenterSupport = interface;
  //UNNotificationAction = interface;
  //UNTextInputNotificationAction = interface;
  //UNNotificationAttachment = interface;
  //UNNotificationCategory = interface;
  //UNMutableNotificationContent = interface;
  //UNTextInputNotificationResponse = interface;
  //UNNotificationServiceExtension = interface;
  //UNNotificationSettings = interface;
  //UNPushNotificationTrigger = interface;
  //UNTimeIntervalNotificationTrigger = interface;
  //UNCalendarNotificationTrigger = interface;
  //UNLocationNotificationTrigger = interface;
  //UNUserNotificationCenterDelegate = interface;
  //NSInteger = Integer;
  //UNErrorCode = NSInteger;
  //NSUInteger = Cardinal;
  //UNNotificationActionOptions = NSUInteger;
  //UNNotificationCategoryOptions = NSUInteger;
  //TUserNotificationsWithContentHandler = procedure(param1: UNNotificationContent) of object;
  //UNAuthorizationStatus = NSInteger;
  //UNNotificationSetting = NSInteger;
  //UNAlertStyle = NSInteger;
  //NSTimeInterval = Double;
  //TUserNotificationsCompletionHandler1 = procedure(param1: NSSet) of object;
  //TUserNotificationsCompletionHandler2 = procedure(param1: UNNotificationSettings) of object;
  TUserNotificationsWithCompletionHandler = procedure(error: NSError) of object;
  //TUserNotificationsCompletionHandler3 = procedure(param1: NSArray) of object;

  //UNUserNotificationCenterSupport = interface(IObjectiveC)
  //  ['{9B6F403B-1BAC-4A74-AFD7-1821FBD4D777}']
  //  function localizedUserNotificationStringForKey(key: NSString; arguments: NSArray): NSString; cdecl;
  //end;

  {************************************************************************}
  // @interface UNNotificationRequest : NSObject <NSCopying, NSSecureCoding>
  UNNotificationRequestClass = interface(NSObjectClass)
  ['{1F781FE9-F421-4043-849E-78672F007E84}']

    // + (instancetype)requestWithIdentifier:(NSString *)identifier content:(UNNotificationContent *)content trigger:(nullable UNNotificationTrigger *)trigger;
    { class } function requestWithIdentifier(identifier: NSString; content: UNNotificationContent; trigger: UNNotificationTrigger): Pointer { instancetype }; cdecl;

  end;
  UNNotificationRequest = interface(NSObject)
  ['{42D5FD21-C2D9-413F-BA83-235AA168DE31}']

    // The unique identifier for this notification request. It can be used to replace or remove a pending notification request or a delivered notification.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSString *identifier;
    function identifier: NSString; cdecl;

    // The content that will be shown on the notification.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) UNNotificationContent *content;
    function content: UNNotificationContent; cdecl;

    // The trigger that will or did cause the notification to be delivered. No trigger means deliver now.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy, nullable) UNNotificationTrigger *trigger;
    function trigger: UNNotificationTrigger; cdecl;

    // - (instancetype)init NS_UNAVAILABLE;

  end;
  TUNNotificationRequest = class(TOCGenericImport<UNNotificationRequestClass, UNNotificationRequest>) end;
  PUNNotificationRequest = Pointer;

  {****************************************************************}
  //@interface UNNotification : NSObject <NSCopying, NSSecureCoding>
  UNNotificationClass = interface(NSObjectClass)
  ['{35E6E230-F6F3-4C3E-BBBB-DC07ABBA9B70}']
  end;
  UNNotification = interface(NSObject)
  ['{965F182E-59A9-44F1-909B-947B9CACC072}']

    // The date displayed on the notification.
    // @property (nonatomic, readonly, copy) NSDate *date;
    function date: NSDate; cdecl;

    // The notification request that caused the notification to be delivered.
    // @property (nonatomic, readonly, copy) UNNotificationRequest *request;
    function request: UNNotificationRequest; cdecl;

    // - (instancetype)init NS_UNAVAILABLE;

  end;
  TUNNotification = class(TOCGenericImport<UNNotificationClass, UNNotification>) end;
  PUNNotification = Pointer;

  //UNNotificationActionClass = interface(NSObjectClass)
  //  ['{7088A880-9536-426B-B7C2-1978D48C961A}']
  //  { class } function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions): Pointer { instancetype }; cdecl;
  //end;
  //UNNotificationAction = interface(NSObject)
  //  ['{BC3786AF-4AE7-45F1-A048-89975D9B41B6}']
  //  function identifier: NSString; cdecl;
  //  function title: NSString; cdecl;
  //  function options: UNNotificationActionOptions; cdecl;
  //end;
  //TUNNotificationAction = class(TOCGenericImport<UNNotificationActionClass, UNNotificationAction>) end;
  //PUNNotificationAction = Pointer;

  //UNTextInputNotificationActionClass = interface(UNNotificationActionClass)
  //  ['{0AEE0CF8-2053-45D7-9C02-4BE5AB88D9D9}']
  //  { class } function actionWithIdentifier(identifier: NSString; title: NSString; options: UNNotificationActionOptions; textInputButtonTitle: NSString; textInputPlaceholder: NSString): Pointer { instancetype }; cdecl;
  //end;
  //UNTextInputNotificationAction = interface(UNNotificationAction)
  //  ['{0E869D8F-0E09-4190-897B-90F66EC6F132}']
  //  function textInputButtonTitle: NSString; cdecl;
  //  function textInputPlaceholder: NSString; cdecl;
  //end;
  //TUNTextInputNotificationAction = class (TOCGenericImport<UNTextInputNotificationActionClass, UNTextInputNotificationAction>) end;
  //PUNTextInputNotificationAction = Pointer;

  //UNNotificationAttachmentClass = interface(NSObjectClass)
  //  ['{89A3043D-4FE4-4D35-89DF-8BA95FAB096B}']
  //  { class } function attachmentWithIdentifier(identifier: NSString; URL: NSURL; options: NSDictionary; error: NSError) : Pointer { instancetype }; cdecl;
  //end;
  //UNNotificationAttachment = interface(NSObject)
  //  ['{A0C037C8-58EE-4AE0-9057-D3DEA3223B2D}']
  //  function identifier: NSString; cdecl;
  //  function URL: NSURL; cdecl;
  //  function &type: NSString; cdecl;
  //end;
  //TUNNotificationAttachment = class (TOCGenericImport<UNNotificationAttachmentClass, UNNotificationAttachment>) end;
  //PUNNotificationAttachment = Pointer;

  //UNNotificationCategoryClass = interface(NSObjectClass)
  //  ['{D9DC4AB7-DFB6-4CF9-B3BF-7091E9C657EA}']
  //  { class } function categoryWithIdentifier(identifier: NSString; actions: NSArray; intentIdentifiers: NSArray; options: UNNotificationCategoryOptions): Pointer { instancetype }; cdecl;
  //end;
  //UNNotificationCategory = interface(NSObject)
  //  ['{9524593F-DE7C-4A60-96E4-1F9DD3474029}']
  //  function identifier: NSString; cdecl;
  //  function actions: NSArray; cdecl;
  //  function intentIdentifiers: NSArray; cdecl;
  //  function options: UNNotificationCategoryOptions; cdecl;
  //end;
  //TUNNotificationCategory = class(TOCGenericImport<UNNotificationCategoryClass, UNNotificationCategory>) end;
  //PUNNotificationCategory = Pointer;

  {*********************************************************************}
  //@interface UNNotificationSound : NSObject <NSCopying, NSSecureCoding>
  UNNotificationSoundClass = interface(NSObjectClass)
  ['{B5E7E5D7-6DD6-4F70-9F9A-43F37064F8F5}']

    // The default sound used for notifications.
    // + (instancetype)defaultSound;
    { class } function defaultSound: Pointer { instancetype }; cdecl;

    // The name of a sound file to be played for the notification. The sound file must be contained in the app’s bundle or in the Library/Sounds folder of the app's data container. If files exist in both locations then the file in ~/Library/Sounds will be preferred.
    // + (instancetype)soundNamed:(NSString *)name __WATCHOS_PROHIBITED;
    { class } function soundNamed(name: NSString): Pointer { instancetype }; cdecl;

  end;
  UNNotificationSound = interface(NSObject)
  ['{8D612513-35D2-4996-ACF9-BA2F3D5FC382}']

    // - (instancetype)init NS_UNAVAILABLE;

  end;
  TUNNotificationSound = class(TOCGenericImport<UNNotificationSoundClass, UNNotificationSound>) end;
  PUNNotificationSound = Pointer;

  {*****************************************************************************************}
  //@interface UNNotificationContent : NSObject <NSCopying, NSMutableCopying, NSSecureCoding>
  UNNotificationContentClass = interface(NSObjectClass)
  ['{B3B1DEE1-4F43-4625-9383-5F650CA1B1A8}']
  end;
  UNNotificationContent = interface(NSObject)
  ['{805AFB7F-3710-43C2-A4C5-E1116D9ECB52}']

    // Optional array of attachments.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSArray <UNNotificationAttachment *> *attachments __TVOS_PROHIBITED;
    function attachments: NSArray; cdecl;

    // The application badge number.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy, nullable) NSNumber *badge;
    function badge: NSNumber; cdecl;

    // The body of the notification.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSString *body __TVOS_PROHIBITED;
    function body: NSString; cdecl;

    // The identifier for a registered UNNotificationCategory that will be used to determine the appropriate actions to display for the notification.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSString *categoryIdentifier __TVOS_PROHIBITED;
    function categoryIdentifier: NSString; cdecl;

    // The launch image that will be used when the app is opened from the notification.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSString *launchImageName __TVOS_PROHIBITED;
    function launchImageName: NSString; cdecl;

    // The sound that will be played for the notification.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy, nullable) UNNotificationSound *sound __TVOS_PROHIBITED;
    function sound: UNNotificationSound; cdecl;

    // The subtitle of the notification.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSString *subtitle __TVOS_PROHIBITED;
    function subtitle: NSString; cdecl;

    // The unique identifier for the thread or conversation related to this notification request. It will be used to visually group notifications together.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSString *threadIdentifier __TVOS_PROHIBITED;
    function threadIdentifier: NSString; cdecl;

    // The title of the notification.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSString *title __TVOS_PROHIBITED;
    function title: NSString; cdecl;

    // Apps can set the userInfo for locally scheduled notification requests. The contents of the push payload will be set as the userInfo for remote notifications.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSDictionary *userInfo __TVOS_PROHIBITED;
    function userInfo: NSDictionary; cdecl;

  end;
  TUNNotificationContent = class(TOCGenericImport<UNNotificationContentClass, UNNotificationContent>) end;
  PUNNotificationContent = Pointer;

  {***************************************************************}
  //@interface UNMutableNotificationContent : UNNotificationContent
  UNMutableNotificationContentClass = interface(UNNotificationContentClass)
  ['{4BA5670A-A0F7-42D3-9B55-BB1036B4FF94}']
  end;
  UNMutableNotificationContent = interface(UNNotificationContent)
  ['{AF4CDFC1-F933-4892-B307-80DFCFF609D0}']

    // Optional array of attachments.
    // @property (NS_NONATOMIC_IOSONLY, copy) NSArray <UNNotificationAttachment *> *attachments __TVOS_PROHIBITED;
    procedure setAttachments(attachments: NSArray); cdecl;
    function attachments: NSArray; cdecl;

    // The application badge number. nil means no change. 0 to hide.
    // @property (NS_NONATOMIC_IOSONLY, copy, nullable) NSNumber *badge;
    procedure setBadge(badge: NSNumber); cdecl;
    function badge: NSNumber; cdecl;

    // The body of the notification. Use -[NSString localizedUserNotificationStringForKey:arguments:] to provide a string that will be localized at the time that the notification is presented.
    // @property (NS_NONATOMIC_IOSONLY, copy) NSString *body __TVOS_PROHIBITED;
    procedure setBody(body: NSString); cdecl;
    function body: NSString; cdecl;

    // The identifier for a registered UNNotificationCategory that will be used to determine the appropriate actions to display for the notification.
    // @property (NS_NONATOMIC_IOSONLY, copy) NSString *categoryIdentifier __TVOS_PROHIBITED;
    procedure setCategoryIdentifier(categoryIdentifier: NSString); cdecl;
    function categoryIdentifier: NSString; cdecl;

    // The launch image that will be used when the app is opened from the notification.
    // @property (NS_NONATOMIC_IOSONLY, copy) NSString *launchImageName __TVOS_PROHIBITED;
    procedure setLaunchImageName(launchImageName: NSString); cdecl;
    function launchImageName: NSString; cdecl;

    // The sound that will be played for the notification.
    // @property (NS_NONATOMIC_IOSONLY, copy, nullable) UNNotificationSound *sound __TVOS_PROHIBITED;
    procedure setSound(sound: UNNotificationSound); cdecl;
    function sound: UNNotificationSound; cdecl;

    // The subtitle of the notification. Use -[NSString localizedUserNotificationStringForKey:arguments:] to provide a string that will be localized at the time that the notification is presented.
    // @property (NS_NONATOMIC_IOSONLY, copy) NSString *subtitle __TVOS_PROHIBITED;
    procedure setSubtitle(subtitle: NSString); cdecl;
    function subtitle: NSString; cdecl;

    // The unique identifier for the thread or conversation related to this notification request. It will be used to visually group notifications together.
    // @property (NS_NONATOMIC_IOSONLY, copy) NSString *threadIdentifier __TVOS_PROHIBITED;
    procedure setThreadIdentifier(threadIdentifier: NSString); cdecl;
    function threadIdentifier: NSString; cdecl;

    // The title of the notification. Use -[NSString localizedUserNotificationStringForKey:arguments:] to provide a string that will be localized at the time that the notification is presented.
    // @property (NS_NONATOMIC_IOSONLY, copy) NSString *title __TVOS_PROHIBITED;
    procedure setTitle(title: NSString); cdecl;
    function title: NSString; cdecl;

    // Apps can set the userInfo for locally scheduled notification requests. The contents of the push payload will be set as the userInfo for remote notifications.
    // @property (NS_NONATOMIC_IOSONLY, copy) NSDictionary *userInfo;
    procedure setUserInfo(userInfo: NSDictionary); cdecl;
    function userInfo: NSDictionary; cdecl;

  end;
  TUNMutableNotificationContent = class (TOCGenericImport<UNMutableNotificationContentClass, UNMutableNotificationContent>) end;
  PUNMutableNotificationContent = Pointer;

  {************************************************************************}
  // @interface UNNotificationTrigger : NSObject <NSCopying, NSSecureCoding>
  UNNotificationTriggerClass = interface(NSObjectClass)
  ['{CE177F1D-976C-48DA-BB8D-872B2D6C2D6E}']
  end;
  UNNotificationTrigger = interface(NSObject)
  ['{663EB428-D720-487E-8511-CB54BFCA137F}']

    // @property (NS_NONATOMIC_IOSONLY, readonly) BOOL repeats;
    function repeats: Boolean; cdecl;

    // - (instancetype)init NS_UNAVAILABLE;

  end;
  TUNNotificationTrigger = class(TOCGenericImport<UNNotificationTriggerClass, UNNotificationTrigger>) end;
  PUNNotificationTrigger = Pointer;

  {*************************************************************************}
  // @interface UNNotificationResponse : NSObject <NSCopying, NSSecureCoding>
  UNNotificationResponseClass = interface(NSObjectClass)
  ['{059FD5C5-8350-4AD8-AFDA-A0E40EDE8CB3}']
  end;
  UNNotificationResponse = interface(NSObject)
  ['{C1B5B312-71D8-4EC6-BE3E-FD2EF7483FD9}']

    // The notification to which the user responded.
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) UNNotification *notification;
    function notification: UNNotification; cdecl;

    // The action identifier that the user chose:
    // * UNNotificationDismissActionIdentifier if the user dismissed the notification
    // * UNNotificationDefaultActionIdentifier if the user opened the application from the notification
    // * the identifier for a registered UNNotificationAction for other actions
    // @property (NS_NONATOMIC_IOSONLY, readonly, copy) NSString *actionIdentifier;
    function actionIdentifier: NSString; cdecl;

    // - (instancetype)init NS_UNAVAILABLE;

  end;
  TUNNotificationResponse = class(TOCGenericImport<UNNotificationResponseClass, UNNotificationResponse>) end;
  PUNNotificationResponse = Pointer;

  //UNTextInputNotificationResponseClass = interface(UNNotificationResponseClass)
  //  ['{C31D5BBF-F597-4A62-B26B-1C2631F712FE}']
  //end;
  //UNTextInputNotificationResponse = interface(UNNotificationResponse)
  //  ['{A196D3E6-91E0-4737-9002-B8A55021B289}']
  //  function userText: NSString; cdecl;
  //end;
  //TUNTextInputNotificationResponse = class (TOCGenericImport<UNTextInputNotificationResponseClass, UNTextInputNotificationResponse>) end;
  //PUNTextInputNotificationResponse = Pointer;

  //UNNotificationServiceExtensionClass = interface(NSObjectClass)
  //  ['{417C013C-4D8B-486D-AA73-249B519E71EF}']
  //end;
  //UNNotificationServiceExtension = interface(NSObject)
  //  ['{048994F3-75FB-417C-9C61-001722024244}']
  //  procedure didReceiveNotificationRequest(request: UNNotificationRequest; withContentHandler: TUserNotificationsWithContentHandler); cdecl;
  //  procedure serviceExtensionTimeWillExpire; cdecl;
  //end;
  //TUNNotificationServiceExtension = class (TOCGenericImport<UNNotificationServiceExtensionClass, UNNotificationServiceExtension>) end;
  //PUNNotificationServiceExtension = Pointer;

  //UNNotificationSettingsClass = interface(NSObjectClass)
  //  ['{892AFFA6-3C8A-4323-B065-A3097032624D}']
  //end;
  //UNNotificationSettings = interface(NSObject)
  //  ['{272FE531-6A41-404F-9A5B-61219ECD44C8}']
  //  function authorizationStatus: UNAuthorizationStatus; cdecl;
  //  function soundSetting: UNNotificationSetting; cdecl;
  //  function badgeSetting: UNNotificationSetting; cdecl;
  //  function alertSetting: UNNotificationSetting; cdecl;
  //  function notificationCenterSetting: UNNotificationSetting; cdecl;
  //  function lockScreenSetting: UNNotificationSetting; cdecl;
  //  function carPlaySetting: UNNotificationSetting; cdecl;
  //  function alertStyle: UNAlertStyle; cdecl;
  //end;
  //TUNNotificationSettings = class(TOCGenericImport<UNNotificationSettingsClass, UNNotificationSettings>) end;
  //PUNNotificationSettings = Pointer;

  //UNPushNotificationTriggerClass = interface(UNNotificationTriggerClass)
  //  ['{DDDF112C-33C9-4CB4-9C85-9A30C3C265BC}']
  //end;
  //UNPushNotificationTrigger = interface(UNNotificationTrigger)
  //  ['{51CF43F2-D21B-4F2A-BD8C-9790B1800E39}']
  //end;
  //TUNPushNotificationTrigger = class (TOCGenericImport<UNPushNotificationTriggerClass, UNPushNotificationTrigger>) end;
  //PUNPushNotificationTrigger = Pointer;

  //UNTimeIntervalNotificationTriggerClass = interface(UNNotificationTriggerClass)
  //  ['{6E8689E8-C839-423E-9C28-EC9A18F4D2ED}']
  //  { class } function triggerWithTimeInterval(timeInterval: NSTimeInterval; repeats: Boolean): Pointer { instancetype }; cdecl;
  //end;
  //UNTimeIntervalNotificationTrigger = interface(UNNotificationTrigger)
  //  ['{3DDD298B-4C9A-4071-9A46-545B1C869735}']
  //  function timeInterval: NSTimeInterval; cdecl;
  //  function nextTriggerDate: NSDate; cdecl;
  //end;
  //TUNTimeIntervalNotificationTrigger = class (TOCGenericImport<UNTimeIntervalNotificationTriggerClass, UNTimeIntervalNotificationTrigger>) end;
  //PUNTimeIntervalNotificationTrigger = Pointer;

  //UNCalendarNotificationTriggerClass = interface(UNNotificationTriggerClass)
  //  ['{1E2B9148-C90B-499C-9EAD-0EA9A1B903A0}']
  //  { class } function triggerWithDateMatchingComponents(dateComponents: NSDateComponents; repeats: Boolean): Pointer { instancetype }; cdecl;
  //end;
  //UNCalendarNotificationTrigger = interface(UNNotificationTrigger)
  //  ['{4A07A531-9163-48E1-9023-132495C46F9B}']
  //  function dateComponents: NSDateComponents; cdecl;
  //  function nextTriggerDate: NSDate; cdecl;
  //end;
  //TUNCalendarNotificationTrigger = class (TOCGenericImport<UNCalendarNotificationTriggerClass, UNCalendarNotificationTrigger>) end;
  //PUNCalendarNotificationTrigger = Pointer;

  //UNLocationNotificationTriggerClass = interface(UNNotificationTriggerClass)
  //  ['{127A406E-13BB-4083-9A04-2F73A18C63DC}']
  //  { class } function triggerWithRegion(region: CLRegion; repeats: Boolean): Pointer { instancetype }; cdecl;
  //end;
  //UNLocationNotificationTrigger = interface(UNNotificationTrigger)
  //  ['{025C4F2B-2C28-414F-A490-C65B04345052}']
  //  function region: CLRegion; cdecl;
  //end;
  //TUNLocationNotificationTrigger = class (TOCGenericImport<UNLocationNotificationTriggerClass, UNLocationNotificationTrigger>) end;
  //PUNLocationNotificationTrigger = Pointer;

  {**********************************************}
  //@interface UNUserNotificationCenter : NSObject
  UNUserNotificationCenterClass = interface(NSObjectClass)
  ['{2F37EFB2-BEEE-460E-B217-5A0B438C24BC}']

    // The UNUserNotificationCenter for the current application
    // + (UNUserNotificationCenter *)currentNotificationCenter;
    { class } function currentNotificationCenter: UNUserNotificationCenter; cdecl;

  end;
  UNUserNotificationCenter = interface(NSObject)
  ['{78E8F4A0-00E4-46CB-9A88-E88B26FDF1F5}']

    // The delegate can only be set from an application
    // @property (NS_NONATOMIC_IOSONLY, nullable, weak) id <UNUserNotificationCenterDelegate> delegate;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;

    // Returns YES if the current device supports content extensions
    // @property (NS_NONATOMIC_IOSONLY, readonly) BOOL supportsContentExtensions;
    function supportsContentExtensions: Boolean; cdecl;

    // - (instancetype)init NS_UNAVAILABLE;

    // User authorization is required for applications to notify the user using UNUserNotificationCenter via both local and remote notifications.
    // - (void)requestAuthorizationWithOptions:(UNAuthorizationOptions)options completionHandler:(void (^)(BOOL granted, NSError *__nullable error))completionHandler;
    procedure requestAuthorizationWithOptions(options: UNAuthorizationOptions; completionHandler: TUserNotificationCenterRequestAuthorizationWithOptionsCompletionHandler); cdecl;

    // Notification categories can be used to choose which actions will be displayed on which notifications.
    // - (void)setNotificationCategories:(NSSet<UNNotificationCategory *> *)categories __TVOS_PROHIBITED;
    // - (void)getNotificationCategoriesWithCompletionHandler:(void(^)(NSSet<UNNotificationCategory *> *categories))completionHandler __TVOS_PROHIBITED;
    // procedure setNotificationCategories(categories: NSSet); cdecl;
    // procedure getNotificationCategoriesWithCompletionHandler(completionHandler: TUserNotificationsCompletionHandler1); cdecl;

    // The application's user notification settings
    // - (void)getNotificationSettingsWithCompletionHandler:(void(^)(UNNotificationSettings *settings))completionHandler;
    // procedure getNotificationSettingsWithCompletionHandler(completionHandler: TUserNotificationsCompletionHandler2); cdecl;

    // Notification requests can be scheduled to notify the user via time and location. See UNNotificationTrigger for more information. Calling -addNotificationRequest: will replace an existing notification request with the same identifier. A notification request with the identifier as an existing delivered notifications will alert for the new notification request and replace the existing delivered notification when it is triggered. The number of pending notification requests that may be scheduled by an application at any one time is limited by the system.
    // - (void)addNotificationRequest:(UNNotificationRequest *)request withCompletionHandler:(nullable void(^)(NSError *__nullable error))completionHandler;
    procedure addNotificationRequest(request: UNNotificationRequest; withCompletionHandler: TUserNotificationsWithCompletionHandler); cdecl;

    // Notification requests that are waiting for their trigger to fire
    // - (void)getPendingNotificationRequestsWithCompletionHandler:(void(^)(NSArray<UNNotificationRequest *> *requests))completionHandler;
    // - (void)removePendingNotificationRequestsWithIdentifiers:(NSArray<NSString *> *)identifiers;
    // - (void)removeAllPendingNotificationRequests;
    // procedure getPendingNotificationRequestsWithCompletionHandler(completionHandler: TUserNotificationsCompletionHandler3); cdecl;
    procedure removePendingNotificationRequestsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure removeAllPendingNotificationRequests; cdecl;

    // Notifications that have been delivered and remain in Notification Center. Notifiations triggered by location cannot be retrieved, but can be removed.
    // - (void)getDeliveredNotificationsWithCompletionHandler:(void(^)(NSArray<UNNotification *> *notifications))completionHandler __TVOS_PROHIBITED;
    // - (void)removeDeliveredNotificationsWithIdentifiers:(NSArray<NSString *> *)identifiers __TVOS_PROHIBITED;
    // - (void)removeAllDeliveredNotifications __TVOS_PROHIBITED;
    // procedure getDeliveredNotificationsWithCompletionHandler(completionHandler: TUserNotificationsCompletionHandler3); cdecl;
    procedure removeDeliveredNotificationsWithIdentifiers(identifiers: NSArray); cdecl;
    procedure removeAllDeliveredNotifications; cdecl;

  end;
  TUNUserNotificationCenter = class (TOCGenericImport<UNUserNotificationCenterClass, UNUserNotificationCenter>) end;

  //PUNUserNotificationCenter = Pointer;

  {*****************************************************}
  //@protocol UNUserNotificationCenterDelegate <NSObject>
  UNUserNotificationCenterDelegate = interface(IObjectiveC)
  ['{9784786A-515F-41F0-84C3-8F298623275E}']

    // The method will be called on the delegate only if the application is in the foreground. If the method is not implemented or the handler is not called in a timely manner then the notification will not be presented. The application can choose to have the notification presented as a sound, badge, alert and/or in the notification list. This decision should be based on whether the information in the notification is otherwise visible to the user.
    // - (void)userNotificationCenter:(UNUserNotificationCenter *)center willPresentNotification:(UNNotification *)notification withCompletionHandler:(void (^)(UNNotificationPresentationOptions options))completionHandler __IOS_AVAILABLE(10.0) __TVOS_AVAILABLE(10.0) __WATCHOS_AVAILABLE(3.0);
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenterWillPresentNotificationWithCompletionHandler(center: UNUserNotificationCenter;
                                                                                 willPresentNotification: UNNotification;
                                                                                 withCompletionHandler: TUserNotificationCenterWillPresentNotificationCompletionHandler); cdecl;

    // The method will be called on the delegate when the user responded to the notification by opening the application, dismissing the notification or choosing a UNNotificationAction. The delegate must be set before the application returns from applicationDidFinishLaunching:.
    // - (void)userNotificationCenter:(UNUserNotificationCenter *)center didReceiveNotificationResponse:(UNNotificationResponse *)response withCompletionHandler:(void(^)())completionHandler __IOS_AVAILABLE(10.0) __WATCHOS_AVAILABLE(3.0) __TVOS_PROHIBITED;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenterDidReceiveNotificationResponseWithCompletionHandler(center: UNUserNotificationCenter;
                                                                                        didReceiveNotificationResponse: UNNotificationResponse;
                                                                                        withCompletionHandler: TUserNotificationCenterDidReceiveNotificationResponseCompletionHandler); cdecl;
  end;

//function UNErrorDomain: NSString;
//function UNNotificationAttachmentOptionsTypeHintKey: NSString;
//function UNNotificationAttachmentOptionsThumbnailHiddenKey: NSString;
//function UNNotificationAttachmentOptionsThumbnailClippingRectKey: NSString;
//function UNNotificationAttachmentOptionsThumbnailTimeKey: NSString;
//function UNNotificationDefaultActionIdentifier: NSString;
//function UNNotificationDismissActionIdentifier: NSString;

const
  libUserNotifications = '/System/Library/Frameworks/UserNotifications.framework/UserNotifications';

implementation

{$IF defined(IOS) and NOT defined(CPUARM)}

uses
  Posix.Dlfcn;

var
  UserNotificationsModule: THandle;

{$ENDIF IOS}

//{*******************************}
//function UNErrorDomain: NSString;
//begin
//  Result := CocoaNSStringConst(libUserNotifications, 'UNErrorDomain');
//end;

//{************************************************************}
//function UNNotificationAttachmentOptionsTypeHintKey: NSString;
//begin
//  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsTypeHintKey');
//end;

//{*******************************************************************}
//function UNNotificationAttachmentOptionsThumbnailHiddenKey: NSString;
//begin
//  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailHiddenKey');
//end;

//{*************************************************************************}
//function UNNotificationAttachmentOptionsThumbnailClippingRectKey: NSString;
//begin
//  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailClippingRectKey');
//end;

//{*****************************************************************}
//function UNNotificationAttachmentOptionsThumbnailTimeKey: NSString;
//begin
//  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationAttachmentOptionsThumbnailTimeKey');
//end;

//{*******************************************************}
//function UNNotificationDefaultActionIdentifier: NSString;
//begin
//  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationDefaultActionIdentifier');
//end;

//{*******************************************************}
//function UNNotificationDismissActionIdentifier: NSString;
//begin
//  Result := CocoaNSStringConst(libUserNotifications, 'UNNotificationDismissActionIdentifier');
//end;

{$IF defined(IOS) and NOT defined(CPUARM)}

initialization
  UserNotificationsModule := dlopen(MarshaledAString(libUserNotifications), RTLD_LAZY);

finalization
  dlclose(UserNotificationsModule);

{$ENDIF IOS}

end.
