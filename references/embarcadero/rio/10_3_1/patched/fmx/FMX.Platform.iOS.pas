{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2019 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.iOS;

interface

{$WARNINGS OFF}
{$SCOPEDENUMS ON}

uses
  Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.GLKit, FMX.Types, FMX.Forms, FMX.ZOrder.iOS,
  System.Messaging, //https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
  iOSapi.Foundation; //https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/

const
  /// <summary>Notification. Posted when native UIViewcontroller changed frame of root view.</summary>
  FMXViewControllerFrameChanged = 'FMXViewControllerFrameChanged';
  libobjc = '/usr/lib/libobjc.dylib';
  
//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
type
  // Moved from implementation
  id = Pointer;
  SEL = Pointer;

  // New types
  TAppDelegate_applicationDidFinishLaunchingWithOptions = record
  public
    Application: UIApplication;
    Options: NSDictionary;
  end;

  TAppDelegateMessage_applicationDidFinishLaunchingWithOptions = class(TMessage<TAppDelegate_applicationDidFinishLaunchingWithOptions>)
  public
    constructor Create(const AValue: TAppDelegate_applicationDidFinishLaunchingWithOptions);
  end;

  TAppDelegate_applicationOpenURLWithSourceAnnotation = record
  public
    Application: UIApplication;
    Url: NSUrl;
    SourceApplication: NSString;
    Annotation: id;
  end;

  TAppDelegateMessage_applicationOpenURLWithSourceAnnotation = class(TMessage<TAppDelegate_applicationOpenURLWithSourceAnnotation>)
  public
    constructor Create(const AValue: TAppDelegate_applicationOpenURLWithSourceAnnotation);
  end;

  TAppDelegate_applicationOpenURLWithOptions = record
  public
    Application: UIApplication;
    Url: NSUrl;
    Options: NSDictionary;
  end;

  TAppDelegateMessage_applicationOpenURLWithOptions = class(TMessage<TAppDelegate_applicationOpenURLWithOptions>)
  public
    constructor Create(const AValue: TAppDelegate_applicationOpenURLWithOptions);
  end;

  TAppDelegate_applicationDidBecomeActive = record
  public
    Application: UIApplication;
  end;

  TAppDelegateMessage_applicationDidBecomeActive = class(TMessage<TAppDelegate_applicationDidBecomeActive>)
  public
    constructor Create(const AValue: TAppDelegate_applicationDidBecomeActive);
  end;
//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
  
type

  TiOSWindowHandle = class(TWindowHandle)
  private
    FHandle: TOCLocal;
    FZOrderManager: TiOSZOrderManager;
    function GetZOrderManager: TiOSZOrderManager;
  protected
    function GetView: UIView; virtual;
    function GetGLView: GLKView; virtual;
    function GetForm: TCommonCustomForm; virtual;
    function GetWnd: UIWindow; virtual;
    function GetScale: Single; override;
  public
    constructor Create(const AHandle: TOCLocal);
    destructor Destroy; override;
    property View: UIView read GetView;
    property Wnd: UIWindow read GetWnd;
    property GLView: GLKView read GetGLView;
    property Form: TCommonCustomForm read GetForm;
    property Handle: TOCLocal read FHandle;
    /// <summary>Link to Z-Order manager that used as shared manager for platform controls
    property ZOrderManager: TiOSZOrderManager read GetZOrderManager;
  end;

  TiOSOpenApplicationContext = class
  private
    FSourceApp: string;
    FURL: string;
    FContext: Pointer;
  public
    constructor Create(ASourceApp: string; AURL: string; AContext: Pointer);
    property SourceApp: string read FSourceApp;
    property URL: string read FURL;
    property Context: Pointer read FContext;
  end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TiOSWindowHandle;

procedure RegisterCorePlatformServices;

implementation

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.TypInfo, System.RTLConsts,
  System.Math, Macapi.ObjCRuntime, Macapi.CoreFoundation, Macapi.Helpers, iOSapi.CocoaTypes,
  iOSapi.CoreGraphics, iOSapi.Helpers, FMX.Graphics, FMX.Consts, FMX.Controls, FMX.Canvas.GPU, FMX.TextLayout,
  FMX.Text, FMX.Styles, FMX.Gestures, FMX.Context.GLES, FMX.Forms3D, FMX.Utils, FMX.Graphics.iOS, FMX.Context.GLES.iOS,
  FMX.Controls.iOS, FMX.Gestures.iOS, FMX.Helpers.iOS, FMX.Dialogs.iOS, FMX.Platform, FMX.Platform.Timer.iOS,
  FMX.Platform.SaveState.iOS, FMX.MultiTouch.iOS, FMX.Platform.Metrics.iOS, FMX.Platform.Device.iOS,
  FMX.Platform.Screen.iOS, FMX.Platform.Logger.iOS, //https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
  System.Generics.Collections, ALFmxInertialMovement;

const
  UNNotificationPresentationOptionBadge = 1;
  UNNotificationPresentationOptionSound = 2;
  UNNotificationPresentationOptionAlert = 4;

type

{$M+}

  TCocoaTouchWindowManager = class;
  TFMXWakeHandler = class;
  TNotificationCenterDelegate = class;

  { TPlatformCocoaTouch }

  TPlatformCocoaTouch = class(TInterfacedObject, IFMXApplicationService, IFMXApplicationEventService, IFMXTextService,
    IFMXGestureRecognizersService, IFMXMouseService)
  private
    FTimerService: TCocoaTouchTimerService;
    FMetricsServices: TCocoaTouchMetricsServices;
    FGraphicServices: TCocoaTouchGraphicServices;
    FDeviceServices: TCocoaTouchDeviceServices;
    FSaveStateService: TCocoaTouchSaveStateService;
    FScreenServices: TCocoaTouchScreenServices;
    FLoggerService: TCocoaTouchLoggerService;
    FWindowManagerService: TCocoaTouchWindowManager;
    FWakeHandler: TFMXWakeHandler;
    FTerminating: Boolean;
    FRunning: Boolean;
    FOnApplicationEvent: TApplicationEventHandler;
    FMouseCoord: TPointF;
    FTitle: string;
    FRunLoopObserver: CFRunLoopObserverRef;
    FNotificationCenterDelegate: TNotificationCenterDelegate;
    procedure InitializeFormFactor(AFormFactor: TFormFactor);
    procedure WakeMainThread(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXApplicationService }
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetVersionString: string;
    function Terminating: Boolean;
    function Running: Boolean;
    procedure Terminate;
    { IFMXMouseService }
    function GetMousePos: TPointF;
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXApplicationEventService }
    procedure SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
    function HandleApplicationEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
    { IFMXGestureRecognizersService }
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
  public
    property DeviceManager: TCocoaTouchDeviceServices read FDeviceServices;
    property Logger: TCocoaTouchLoggerService read FLoggerService;
    property ScreenManager: TCocoaTouchScreenServices read FScreenServices;
    property TimerManager: TCocoaTouchTimerService read FTimerService;
    property WindowManager: TCocoaTouchWindowManager read FWindowManagerService;
  end;

  // id = Pointer;  //https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
  // SEL = Pointer;  //https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/

  IFMXWakeHandler = interface(NSObject)
  ['{ECEC50FA-6A4A-4DAE-9B23-A59A7C2CACC1}']
    procedure DoCheckSynchronize; cdecl;
  end;

  TFMXWakeHandler = class(TOCLocal)
  private
    function GetNativeObject: NSObject;
  protected
    { TOCLocal }
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure DoCheckSynchronize; cdecl;
    property NativeObject: NSObject read GetNativeObject;
  end;

  FMXViewController = interface(UIViewController)
    ['{FB1283E6-B1AB-419F-B331-160096B10C62}']
    { Managing the View }
    procedure loadView; cdecl;
    { Configuring the View Rotation Settings }
    function supportedInterfaceOrientations: NSUInteger; cdecl;
    function shouldAutorotate: Boolean; cdecl;
    { Configuring the View's Layout Behavior }
    procedure viewWillLayoutSubviews; cdecl;
    procedure viewDidLayoutSubviews; cdecl;
    { Managing the Status Bar }
    function prefersStatusBarHidden: Boolean; cdecl;
    function preferredStatusBarStyle: UIStatusBarStyle; cdecl;
    { Handling View Rotations }
    procedure viewWillTransitionToSize(size: CGSize; withTransitionCoordinator: UIViewControllerTransitionCoordinator); cdecl;
 end;

  TFMXViewController = class(TOCLocal)
  public const
    DefaultStatusBarBackgroundColor = TAlphaColorRec.White;
  private
    FRootViewSavedFrame: NSRect;
    FStatusBar: UIView;
    FStatusBarVisible: Boolean;
    FStatusBarLuminance: Single;
    FRotationView: UIImageView;
    FStatusBarHeightConstraint: NSLayoutConstraint;
    procedure SetStatusBarVisible(const AValue: Boolean);
    procedure SetStatusBarBackgroundColor(const ABackgroundColor: TAlphaColor);
    function GetViewController: UIViewController;
    function GetView: UIView;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
    procedure BeforeOrientationChange;
    procedure StartRotation(param1: Pointer);
    procedure AfterOrientationChange(param1: Pointer);
    function CaptureScreenSnapshot: UIImageView;
  public
    { UIViewController }
    procedure loadView; cdecl;
    { Configuring the View Rotation Settings }
    function supportedInterfaceOrientations: NSUInteger; cdecl;
    function shouldAutorotate: Boolean; cdecl;
    { Configuring the View's Layout Behavior }
    procedure viewWillLayoutSubviews; cdecl;
    procedure viewDidLayoutSubviews; cdecl;
    { Managing the Status Bar }
    function prefersStatusBarHidden: Boolean; cdecl;
    function preferredStatusBarStyle: UIStatusBarStyle; cdecl;
    { Handling View Rotations }
    procedure viewWillTransitionToSize(size: CGSize; withTransitionCoordinator: UIViewControllerTransitionCoordinator); cdecl;
  public
    property View: UIView read GetView;
    property ViewController: UIViewController read GetViewController;
    property StatusBarView: UIView read FStatusBar;
    property StatusBarColor: TAlphaColor write SetStatusBarBackgroundColor;
    property StatusBarVisible: Boolean read FStatusBarVisible write SetStatusBarVisible;
  end;

  FMXWindow = interface(UIWindow)
    ['{B0EB8A41-2F1D-43DF-9207-25E3ACE7E08A}']
  end;

  TFMXWindow = class(TOCLocal)
  private
    FRootViewController: TFMXViewController;
    procedure SetRootViewController(const AValue: TFMXViewController);
    function GetNativeWindow: UIWindow;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const ABounds: NSRect); overload;
    property RootViewController: TFMXViewController read FRootViewController write SetRootViewController;
    property NativeWindow: UIWindow read GetNativeWindow;
  end;

{ iOSApi.UserNotification }

  UNNotificationPresentationOptions = NSInteger;

  UNNotification = interface;
  UNNotificationContent = interface;
  UNNotificationSound = interface;
  UNNotificationRequest = interface;
  UNUserNotificationCenter = interface;
  UNUserNotificationCenterDelegate = interface;

  UNNotification = interface(NSObject)
    ['{AFEE6151-57D2-4811-B5E8-3CDF1DDFBE57}']
    function request: UNNotificationRequest; cdecl;
  end;

  UNNotificationContent = interface(NSObject)
    ['{BDF46729-F7A4-4780-8699-424E5A342426}']
    function badge: NSNumber; cdecl;
    function sound: UNNotificationSound; cdecl;
  end;

  UNNotificationSound = interface(NSObject)
    ['{8C29C92A-EE23-423F-8F97-38735D4E2544}']
  end;

  UNNotificationRequest = interface(NSObject)
    ['{77D79B65-2B67-4255-B40E-7E98F2C91D8C}']
    function content: UNNotificationContent; cdecl;
  end;

  UNNotificationResponse = interface(NSObject)
    ['{12B346C3-E55B-4A93-81D3-01699EBAEA43}']
    function actionIdentifier: NSString; cdecl;
    function notification: UNNotification; cdecl;
  end;

  UNUserNotificationCenterClass = interface(NSObjectClass)
    ['{499732CA-F14F-4A91-82F9-903E1B0C125C}']
    {class} function currentNotificationCenter: UNUserNotificationCenter; cdecl;
  end;
  UNUserNotificationCenter = interface(NSObject)
    ['{23C1DA35-7D1A-483F-ACE5-872CA4068EFF}']
    procedure setDelegate(delegate: UNUserNotificationCenterDelegate); cdecl;
  end;
  TUNUserNotificationCenter = class(TOCGenericImport<UNUserNotificationCenterClass, UNUserNotificationCenter>) end;

  UNUserNotificationCenterDelegate = interface(IObjectiveC)
    ['{07BBABF7-BB08-44A7-A654-37D3EEE16DA9}']
    [MethodName('userNotificationCenter:openSettingsForNotification:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification); overload; cdecl;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse; completionHandler: Pointer); overload; cdecl;
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification; completionHandler: Pointer); overload; cdecl;
  end;

  TNotificationCenterDelegate = class(TOCLocal, UNUserNotificationCenterDelegate)
  public
    { UNUserNotificationCenterDelegate }
    [MethodName('userNotificationCenter:openSettingsForNotification:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification); overload; cdecl;
    [MethodName('userNotificationCenter:didReceiveNotificationResponse:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse; completionHandler: Pointer); overload; cdecl;
    [MethodName('userNotificationCenter:willPresentNotification:withCompletionHandler:')]
    procedure userNotificationCenter(center: UNUserNotificationCenter; notification: UNNotification; completionHandler: Pointer); overload; cdecl;
  end;

  TApplicationDelegate = class sealed
  public type
    TApplicationTransitionState = (Launching, Rotating, IncomingCall);
    TApplicationTransitionStates = set of TApplicationTransitionState;
  public const
    DelegateName = 'DelphiAppDelegate';
  private
    class var FState: TApplicationTransitionStates;
    class var FRotatingDuration: NSTimeInterval;
    class function CheckLocalNotificationPermission: Boolean;
    { Startup }
    class function applicationDidFinishLaunchingWithOptions(self: id; _cmd: SEL; application: PUIApplication;
      options: PNSDictionary): Boolean; cdecl; static;
    { Application state }
    class procedure applicationDidEnterBackground(self: id; _cmd: SEL; application: PUIApplication); cdecl; static;
    class procedure applicationDidBecomeActive(self: id; _cmd: SEL; application: PUIApplication); cdecl; static;
    class procedure applicationWillEnterForeground(self: id; _cmd: SEL; application: PUIApplication); cdecl; static;
    class procedure applicationWillTerminate(self: id; _cmd: SEL; application: PUIApplication); cdecl; static;
    class procedure applicationSignificantTimeChange(self: id; _cmd: SEL; application: PUIApplication); cdecl; static;
    class procedure applicationWillResignActive(self: id; _cmd: SEL; application: PUIApplication); cdecl; static;
    class procedure applicationDidReceiveMemoryWarning(self: id; _cmd: SEL; application: PUIApplication); cdecl; static;
    { Managing Interface Geometry }
    class procedure applicationWillChangeStatusBarOrientationDuration(self: id; _cmd: SEL; application: PUIApplication;
      newStatusBarOrientation: UIInterfaceOrientation; duration : NSTimeInterval); cdecl; static;
    class procedure applicationDidChangeStatusBarOrientation(self: id; _cmd: SEL; application: PUIApplication;
      oldStatusBarOrientation: UIInterfaceOrientation); cdecl; static;
    class procedure applicationWillChangeStatusBarFrame(self: id; _cmd: SEL; application: PUIApplication;
      newStatusBarFrame: CGRect); cdecl; static;
    class procedure applicationDidChangeStatusBarFrame(self: id; _cmd: SEL; application: PUIApplication;
      oldStatusBarFrame: CGRect); cdecl; static;
    { Local notifications }
    class procedure applicationDidReceiveLocalNotification(self: id; _cmd: SEL; application: PUIApplication;
      notification: Pointer); cdecl; static;
    { Remote notifications }
    class procedure applicationDidReceiveRemoteNotification(self: id; _cmd: SEL; application: PUIApplication;
      notification: PNSDictionary); cdecl; static;
    class procedure applicationDidFailToRegisterForRemoteNotificationsWithError(self: id; _cmd: SEL;
      application: PUIApplication; error: PNSError); cdecl; static;
    class procedure applicationDidRegisterForRemoteNotificationsWithDeviceToken(self: id; _cmd: SEL;
      application: PUIApplication; deviceToken: PNSData); cdecl; static;
    { Opening a URL-Specified Resource }
    class function applicationOpenURLWithSourceAnnotation(self: id; _cmd: SEL; application: PUIApplication;
      url: Pointer; sourceApplication: PNSString; annotation: id): Boolean; cdecl; static;
    class function applicationOpenURLWithOptions(self: id; _cmd: SEL; application: PUIApplication; url: Pointer;
      options: PNSDictionary): Boolean; cdecl; static;
  public
    class procedure CreateDelegateMetaClass;
    class property TransitionState: TApplicationTransitionStates read FState;
    class property RotatingDuration: NSTimeInterval read FRotatingDuration;
  end;

  TCocoaTouchWindowManager = class(TInterfacedObject, IFMXWindowService, IFMXWindowSystemStatusBarService)
  public const
    DefaultStatusBarOffset = 20;
  private
    FWindow: TFMXWindow;
    FRootViewController: TFMXViewController;
    FCanSetState: Boolean;
    FStatusBarOffset: Single;
    procedure RegisterService;
    procedure UnregisterService;
    function GetNativeWindow: UIWindow;
    procedure SetRootViewController(const Value: TFMXViewController);
  protected
    procedure UpdateStatusBar(const AForm: TCommonCustomForm);
    procedure UpdateFormState(const AForm: TCommonCustomForm; const ANewState: TWindowState);
    function CalculateFormViewFrame(const AForm: TCommonCustomForm): NSRect;
  public
    constructor Create;
    destructor Destroy; override;
    { IFMXWindowService }
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
    procedure DestroyWindow(const AForm: TCommonCustomForm);
    procedure ReleaseWindow(const AForm: TCommonCustomForm);
    procedure ShowWindow(const AForm: TCommonCustomForm);
    procedure HideWindow(const AForm: TCommonCustomForm);
    procedure BringToFront(const AForm: TCommonCustomForm);
    procedure SendToBack(const AForm: TCommonCustomForm);
    procedure Activate(const AForm: TCommonCustomForm);
    function ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
    function CanShowModal: Boolean;
    procedure InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
    procedure InvalidateImmediately(const AForm: TCommonCustomForm);
    procedure SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
    function GetWindowRect(const AForm: TCommonCustomForm): TRectF;
    function GetClientSize(const AForm: TCommonCustomForm): TPointF;
    procedure SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
    procedure SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
    procedure SetCapture(const AForm: TCommonCustomForm);
    procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
    procedure ReleaseCapture(const AForm: TCommonCustomForm);
    function ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function GetWindowScale(const AForm: TCommonCustomForm): Single;
    { IFMXFullScreenWindowService }
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
    { IFMXWindowStatusBarService }
    procedure SetBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
    procedure SetVisibility(const AForm: TCommonCustomForm; const AMode: TFormSystemStatusBar.TVisibilityMode);
    { Status Bar }
    function HasFormStatusBar(const AForm: TCommonCustomForm): Boolean;
    function IsPopupForm(const AForm: TCommonCustomForm): Boolean;
  public
    property RootViewController: TFMXViewController read FRootViewController write SetRootViewController;
    property Window: TFMXWindow read FWindow write FWindow;
    property NativeWindow: UIWindow read GetNativeWindow;
    property StatusBarOffset: Single read FStatusBarOffset write FStatusBarOffset;
  end;

{ TFMXEditActionsMenu }

  TStandardActionType = (Unknown, Cut, Copy, Paste, Select, SelectAll, PromptForReplace, Replace, Spell1, Spell2,
    Spell3);

  { Context menu with standart edit actions: Cut, Copy, Past, Select, SelectAll }
  TFMXEditActionsMenu = class abstract (TInterfacedObject, IFreeNotification)
  strict private
    [Weak] FParentView: UIView;
    procedure SetControl(const AControl: TControl);
  private
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
  protected
    FMenuController: UIMenuController;
    [Weak] FControl: TControl;
    FReplaceMenu: Boolean;
    procedure DoControlChanged; virtual;
    procedure DoDefineSelectionFrame(var Frame: CGRect); virtual;
  public
    constructor Create(const AParentView: UIView);
    destructor Destroy; override;
    // Do we need to show action in the context menu in the current conditions?
    function CanPerformAction(const AAction: SEL): Boolean; virtual; abstract;
    function DefineActionType(const AAction: SEL): TStandardActionType;
    procedure Show;
    procedure Hide;
    function IsVisible: Boolean;
    function HasControl: Boolean;
    { Standart Actions }
    procedure Cut; virtual; abstract;
    procedure Copy; virtual; abstract;
    procedure Paste; virtual; abstract;
    procedure Select; virtual; abstract;
    procedure SelectAll; virtual; abstract;
    procedure PromptForReplace; virtual; abstract;
    procedure Spell1; virtual; abstract;
    procedure Spell2; virtual; abstract;
    procedure Spell3; virtual; abstract;
    property Control: TControl read FControl write SetControl;
  end;

{ TFMXTextEditActionsMenu }

  { Implemented context menu with standart edit actions for text controls }

  TFMXTextEditActionsMenu = class (TFMXEditActionsMenu)
  private
    FTextInput: ITextInput;
    FTextActions: ITextActions;
    FSpellCheck: ITextSpellCheck;
    FSpellActions: ITextSpellCheckActions;
    FSpells: TArray<string>;
    FVirtualKeyboard: IVirtualKeyboardControl;
    FShowSpellItems: Boolean;
    FSpellItem1: UIMenuItem;
    FSpellItem2: UIMenuItem;
    FSpellItem3: UIMenuItem;
  protected
    procedure DoControlChanged; override;
    procedure DoDefineSelectionFrame(var Frame: CGRect); override;
    function GetClipboardService: IFMXClipboardService;
  public
    function CanPerformAction(const AAction: SEL): Boolean; override;
    procedure Cut; override;
    procedure Copy; override;
    procedure Paste; override;
    procedure Select; override;
    procedure SelectAll; override;
    procedure PromptForReplace; override;
    procedure Spell1; override;
    procedure Spell2; override;
    procedure Spell3; override;
    //
    procedure SetSpellItems(items: TArray<string>);
    procedure HighlightSpell;
    procedure HideHighlightSpell;
    //
    property ShowSpellItems: Boolean read FShowSpellItems write FShowSpellItems;
  end;

  TTextServiceCocoa = class;

  TFMXViewBase = class(TOCLocal, UIKeyInput, UITextInput, UITextInputTraits, UIGestureRecognizerDelegate)
  public const
    DblTapDelay = 0.3; // Sec, Duration between first and second tap (Apple recommend use this value)
  private
    FGestureControl: TComponent;
    FMultiTouchManager: TMultiTouchManagerIOS;
    FNoOfTouches: NativeUInt;
    [Weak] FTextService: TTextServiceCocoa;
    function GetView: UIView;
    function GetTouchCoord(const touches: NSSet; const Window: UIView; var x, y: single): Boolean;
    procedure SendTouches(const ATouches: NSSet; Action: TTouchAction; const Control: IControl);
    procedure AddRecognizer(const Gesture: TInteractiveGesture);
    procedure DoLMouseUp(const X, Y: Single; DoClick: Boolean = True);
    procedure DoLMouseDown(const X, Y: Single);
    procedure DoLMouseMove(const X, Y: Single);
    procedure DefineFocusControl;
    procedure FormKeyPress(Ch: Char; Key: Word; Shift: TShiftState);
    procedure PrepareClosePopups(const SaveForm: TCommonCustomForm);
    procedure ClosePopups;
  protected
    FContextMenu: TFMXTextEditActionsMenu;
    FIgnorePosition: Boolean;
    FCarretPositionChanged: Boolean;
    FLastCaretPosition: TPoint;
    FLastContextMenuVisibility: Boolean;
    FClickedAnotherControl: Boolean;
    FChangedFocusedControl: Boolean;
    [Weak]FForm: TCommonCustomForm;
    FKeyboardType: TVirtualKeyboardType;
    FReturnKeyType: TReturnKeyType;
    FPassword: Boolean;
    FDown: Boolean;
    FTap: Boolean;
    FResigned: Boolean;
    FInputDelegate: UITextInputDelegate;
    destructor Destroy; override;
    function GetMultiTouchManager: TMultiTouchManagerIOS;
    property MultiTouchManager: TMultiTouchManagerIOS read GetMultiTouchManager;
  public
    constructor Create(const AForm: TCommonCustomForm);
    { UIView overrides }
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canResignFirstResponder: Boolean; cdecl;
    function isFirstResponder: Boolean; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
    function isAccessibilityElement: Boolean; cdecl;
    { Touch Actions }
    function isMultipleTouchEnabled: Boolean; cdecl;
    procedure SingleTap(Sender: id); cdecl;
    procedure LongTap(gestureRecognizer: UILongPressGestureRecognizer); cdecl;
    procedure DblTap(X, Y: Single);
    { UIGestureRecognizerDelegate }
    function gestureRecognizer(gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch): Boolean; overload; cdecl;
    function gestureRecognizer(gestureRecognizer: UIGestureRecognizer; shouldRecognizeSimultaneouslyWithGestureRecognizer: UIGestureRecognizer): Boolean; overload; cdecl;
    function gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean; cdecl;
    { GestureRecognizer }
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    procedure HandleRotate(gestureRecognizer: UIRotationGestureRecognizer); cdecl;
    procedure HandleTwoFingerTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
    procedure HandleZoom(gestureRecognizer: UIPinchGestureRecognizer); cdecl;
    procedure HandleDoubleTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
    { Cut, Copy, Paste, Replace, SpellChecker Actions }
    procedure cut(Sender: id); cdecl;
    procedure copy(Sender: id); cdecl;
    procedure paste(Sender: id); cdecl;
    procedure select(Sender: id); cdecl;
    procedure selectAll(Sender: id); cdecl;
    procedure spell1(Sender: id); cdecl;
    procedure spell2(Sender: id); cdecl;
    procedure spell3(Sender: id); cdecl;
    { Context Menu Showing }
    procedure ShowContextMenu;
    procedure HideContextMenu;
    { UIKeyInput }
    procedure deleteBackward; cdecl;
    function hasText: Boolean; cdecl;
    procedure insertText(text: NSString); cdecl;
    { UITextInput }
    function baseWritingDirectionForPosition(position: UITextPosition; inDirection: UITextStorageDirection): UITextWritingDirection; cdecl;
    function beginningOfDocument: UITextPosition; cdecl;
    function endOfDocument: UITextPosition; cdecl;
    function inputDelegate: Pointer; cdecl;
    function markedTextRange: UITextRange; cdecl;
    function markedTextStyle: NSDictionary; cdecl;
    function selectedTextRange: UITextRange; cdecl;
    procedure setBaseWritingDirection(writingDirection: UITextWritingDirection; forRange: UITextRange); cdecl;
    procedure setInputDelegate(inputDelegate: UITextInputDelegate); cdecl;
    procedure setMarkedText(markedText: NSString; selectedRange: NSRange); cdecl;
    procedure setMarkedTextStyle(markedTextStyle: NSDictionary); cdecl;
    procedure setSelectedTextRange(selectedTextRange: UITextRange); cdecl;
    { Dictation }
    procedure insertDictationResult(dictationResult: NSArray); cdecl;
    function insertDictationResultPlaceholder: Pointer {id}; cdecl;
    function frameForDictationResultPlaceholder(placeholder: Pointer {id}): CGRect; cdecl;
    procedure removeDictationResultPlaceholder(placeholder: Pointer {id}; willInsertResult: Boolean); cdecl;
    { Returning and replacing text by text range }
    function textInRange(range: UITextRange): NSString; cdecl;
    procedure replaceRange(range: UITextRange; withText: NSString); cdecl;
    { Computing text ranges and text positions }
    function positionFromPosition(position: UITextPosition; offset: NSInteger): UITextPosition; overload; cdecl;
    function positionFromPosition(position: UITextPosition; inDirection: UITextLayoutDirection; offset: NSInteger): UITextPosition; overload; cdecl;
    function textRangeFromPosition(fromPosition: UITextPosition; toPosition: UITextPosition): UITextRange; cdecl;
    { Evaluating text positions }
    function comparePosition(position: UITextPosition; toPosition: UITextPosition): NSComparisonResult; cdecl;
    function offsetFromPosition(from: UITextPosition; toPosition: UITextPosition): NSInteger; cdecl;
    { Answering layout questions }
    function positionWithinRange(range: UITextRange; farthestInDirection: UITextLayoutDirection): UITextPosition; cdecl; //overload;
    function characterRangeByExtendingPosition(position: UITextPosition; inDirection: UITextLayoutDirection): UITextRange; cdecl;
    { Hit-testing }
    function closestPositionToPoint(point: CGPoint): UITextPosition; overload; cdecl;
    function closestPositionToPoint(point: CGPoint; withinRange: UITextRange): UITextPosition; overload; cdecl;
    function characterRangeAtPoint(point: CGPoint): UITextRange; cdecl;
    { Returning rectangles for text ranges and text positions }
    function firstRectForRange(range: UITextRange): CGRect; cdecl;
    function caretRectForPosition(position: UITextPosition): CGRect; cdecl;

    function tokenizer: Pointer; cdecl;
    procedure unmarkText; cdecl;
    { UITextInputTraits }
    function autocapitalizationType: UITextAutocapitalizationType; cdecl;
    function autocorrectionType: UITextAutocorrectionType; cdecl;
    function enablesReturnKeyAutomatically: Boolean; cdecl;
    function isSecureTextEntry: Boolean; cdecl;
    function keyboardAppearance: UIKeyboardAppearance; cdecl;
    function keyboardType: UIKeyboardType; cdecl;
    function returnKeyType: UIReturnKeyType; cdecl;
    procedure setAutocapitalizationType(autocapitalizationType: UITextAutocapitalizationType); cdecl;
    procedure setAutocorrectionType(autocorrectionType: UITextAutocorrectionType); cdecl;
    procedure setEnablesReturnKeyAutomatically(enablesReturnKeyAutomatically: Boolean); cdecl;
    procedure setKeyboardAppearance(keyboardAppearance: UIKeyboardAppearance); cdecl;
    procedure setKeyboardType(keyboardType: UIKeyboardType); cdecl;
    procedure setReturnKeyType(returnKeyType: UIReturnKeyType); cdecl;
    procedure setSecureTextEntry(secureTextEntry: Boolean); cdecl;
    procedure setSpellCheckingType(spellCheckingType: NSInteger); cdecl;
    function spellCheckingType: NSInteger; cdecl;
  public
    property Form: TCommonCustomForm read FForm;
    property View: UIView read GetView;
  end;

  FMXView3D = interface(GLKView)
    ['{CC0FB04D-56B0-446D-9464-F18D1B4AFE22}']
    procedure touchesBegan(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesCancelled(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesEnded(touches: NSSet; withEvent: UIEvent); cdecl;
    procedure touchesMoved(touches: NSSet; withEvent: UIEvent); cdecl;
    function canBecomeFirstResponder: Boolean; cdecl;
    function canResignFirstResponder: Boolean; cdecl;
    function canPerformAction(action: SEL; withSender: Pointer): Boolean; cdecl;
    function isFirstResponder: Boolean; cdecl;
    procedure drawRect(R: CGRect); cdecl;
    { Cut, Copy, Paste }
    procedure cut(Sender: id); cdecl;
    procedure copy(Sender: id); cdecl;
    procedure paste(Sender: id); cdecl;
    procedure select(Sender: id); cdecl;
    procedure selectAll(Sender: id); cdecl;
    procedure spell1(Sender: id); cdecl;
    procedure spell2(Sender: id); cdecl;
    procedure spell3(Sender: id); cdecl;
    { Touches }
    function isMultipleTouchEnabled: Boolean; cdecl;
    procedure SingleTap(Sender: id); cdecl;
    procedure LongTap(gestureRecognizer: UILongPressGestureRecognizer); cdecl;
    procedure HandlePan(gestureRecognizer: UIPanGestureRecognizer); cdecl;
    procedure HandleRotate(gestureRecognizer: UIRotationGestureRecognizer); cdecl;
    procedure HandleTwoFingerTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
    procedure HandleZoom(gestureRecognizer: UIPinchGestureRecognizer); cdecl;
    procedure HandleDoubleTap(gestureRecognizer: UITapGestureRecognizer); cdecl;
    { Accessibility }
    function isAccessibilityElement: Boolean; cdecl;
  end;

  TFMXView3D = class(TFMXViewBase, UIKeyInput, UITextInput, UITextInputTraits, UIGestureRecognizerDelegate)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AOwner: TCommonCustomForm); overload;
    constructor Create(const AOwner: TCommonCustomForm; AFrameRect: NSRect); overload;
    procedure drawRect(R: CGRect); cdecl;
  end;

  TFMXTextPosition = class;
  TFMXTextRange = class;

  { TTextServiceCocoa }
  
  TTextServiceCocoa = class (TTextService)
  private
    [Weak] FView: TFMXViewBase;
    FCaretPostion: TPoint;
    FText : string;
    FMarkedText : string;
    FImeMode: TImeMode;
    FCursorShift: Integer;
    FDocumentBegin: TFMXTextPosition;
    FDocumentEnd: TFMXTextPosition;
    FSelectedTextRange: TFMXTextRange;
    FMarkedTextRange: TFMXTextRange;
    procedure InsertText(const AText: string);
    function DeleteBackward: Boolean;
    procedure SetMarkedText(const AText: string);
    procedure UpdateMarkedTextRange;
    procedure SelectionUpdated;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function GetCaretPosition: TPoint; override;
    procedure SetCaretPosition(const Value: TPoint); override;
    procedure SetCursorShift(const Value: Integer);
  public
    procedure InternalSetMarkedText( const AMarkedText: string ); override;
    function InternalGetMarkedText: string; override;
    procedure InternalStartIMEInput;
    procedure InternalBreakIMEInput;
    procedure InternalEndIMEInput;

    function CombinedText: string; override;
    function TargetClausePosition: TPoint; override;

    procedure EnterControl(const FormHandle: TWindowHandle); override;
    procedure ExitControl(const FormHandle: TWindowHandle); override;

    procedure DrawSingleLine(const  Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload;  override;

    procedure DrawSingleLine(const Canvas: TCanvas;
      const S: string;
      const ARect: TRectF;
      const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload; override;

    function HasMarkedText: Boolean; override;

    function GetImeMode: TImeMode; override;
    procedure SetImeMode(const Value: TImeMode); override;

    { Selection }
    procedure BeginSelection; override;
    procedure EndSelection; override;
  public
    constructor Create(const Owner: IControl; SupportMultiLine: Boolean); override;
    destructor Destroy; override;
  end;

  IFMXTextPosition = interface(UITextPosition)
    ['{9AC99E26-3750-4CA5-9CF3-60F1F3708570}']
  end;

  TFMXTextPosition = class(TOCLocal)
  private
    FPosition: Integer;
    FLine: Integer;
    FHardLink: Boolean;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const APosition, ALine: Integer; const AHardLink: Boolean = False);
    destructor Destroy; override;

    function ToUITextPosition: UITextPosition;

    property Position: Integer read FPosition write FPosition;
    property Line: Integer read FLine write FLine;

    class function FromUITextPosition(const APosition: UITextPosition): TFMXTextPosition; static;
    class operator Equal(const A, B: TFMXTextPosition): Boolean;
    class operator NotEqual(const A, B: TFMXTextPosition): Boolean;
    class operator GreaterThan(const A, B: TFMXTextPosition): Boolean;
    class operator GreaterThanOrEqual(const A, B: TFMXTextPosition): Boolean;
    class operator LessThan(const A, B: TFMXTextPosition): Boolean;
    class operator LessThanOrEqual(const A, B: TFMXTextPosition): Boolean;
  end;

  IFMXTextRange = interface(UITextRange)
    ['{F1D7A08B-1CC8-4C2A-959B-18D163BB39E0}']
    function isEmpty: Boolean; cdecl;
    function start: UITextPosition; cdecl;
    function &end: UITextPosition; cdecl;
  end;

  TFMXTextRange = class(TOCLocal)
  private
    FStart: TFMXTextPosition;
    FEnd: TFMXTextPosition;
    FHardLink: Boolean;
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor Create(const AStartPosition, AStartLine, AEndPosition, AEndLine: Integer;
      const AHardLink: Boolean = False);
    destructor Destroy; override;
    { IFMXTextRange }
    function isEmpty: Boolean; cdecl;
    function start: UITextPosition; cdecl;
    function &end: UITextPosition; cdecl;

    function ToUITextRange: UITextRange;

    property RangeStart: TFMXTextPosition read FStart write FStart;
    property RangeEnd: TFMXTextPosition read FEnd write FEnd;

    class function FromUITextRange(const ARange: UITextRange): TFMXTextRange; static;
  end;
    
function imp_implementationWithBlock(block: id): Pointer; cdecl; external libobjc name _PU + 'imp_implementationWithBlock';
function imp_removeBlock(anImp: Pointer): Integer; cdecl; external libobjc name _PU + 'imp_removeBlock';

var
  PlatformCocoaTouch: TPlatformCocoaTouch;

procedure RegisterCorePlatformServices;
begin
  PlatformCocoaTouch := TPlatformCocoaTouch.Create;
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformCocoaTouch);
  TPlatformServices.Current.AddPlatformService(IFMXApplicationEventService, PlatformCocoaTouch);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformCocoaTouch);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformCocoaTouch);
  TPlatformServices.Current.AddPlatformService(IFMXGestureRecognizersService, PlatformCocoaTouch);
end;

function InternalWaitMessage(const AInterval: Single = 0.1): Boolean;
var
  TimeoutDate: NSDate;
begin
  TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(AInterval));
  Result := TNSRunLoop.Wrap(TNSRunLoop.OCClass.currentRunLoop).runMode(NSDefaultRunLoopMode, TimeoutDate);
end;

function PNSDictionaryToJSON(const ADictionary: PNSDictionary): string;
var
  LData: NSData;
  LString: NSString;
  LError: NSError;
begin
  if ADictionary = nil then
    raise EArgumentException.Create(sArgumentInvalid);

  LData := TNSJSONSerialization.OCClass.dataWithJSONObject(ADictionary, 0, Addr(LError));
  if (LData <> nil) and (LError = nil) then
  begin
    LString := TNSString.Wrap(TNSString.Alloc.initWithData(LData, NSUTF8StringEncoding));
    Result :=  NSStrToStr(LString);
    LString.release;
  end
  else
    Result := string.Empty;
end;

{ TApplicationDelegate }

class procedure TApplicationDelegate.applicationDidBecomeActive(self: id; _cmd: SEL; application: PUIApplication);

//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
var
  AppDelegate_applicationDidBecomeActive: TAppDelegate_applicationDidBecomeActive;
  AppDelegate_applicationDidBecomeActiveMessage: TAppDelegateMessage_applicationDidBecomeActive;
begin
  AppDelegate_applicationDidBecomeActive.Application := TUIApplication.Wrap(application);
  AppDelegate_applicationDidBecomeActiveMessage := TAppDelegateMessage_applicationDidBecomeActive.Create(AppDelegate_applicationDidBecomeActive);
  TMessageManager.DefaultManager.SendMessage(Self, AppDelegate_applicationDidBecomeActiveMessage);
//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/

  PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.BecameActive, nil);
end;

class procedure TApplicationDelegate.applicationDidChangeStatusBarFrame(self: id; _cmd: SEL;
  application: PUIApplication; oldStatusBarFrame: CGRect);
begin
  Exclude(FState, TApplicationDelegate.TApplicationTransitionState.IncomingCall);
end;

class procedure TApplicationDelegate.applicationDidChangeStatusBarOrientation(self: id; _cmd: SEL;
  application: PUIApplication; oldStatusBarOrientation: UIInterfaceOrientation);
begin
  Exclude(FState, TApplicationDelegate.TApplicationTransitionState.Rotating);
end;

class procedure TApplicationDelegate.applicationDidEnterBackground(self: id; _cmd: SEL; application: PUIApplication);
begin
  TMessageManager.DefaultManager.SendMessage(Self, TSaveStateMessage.Create);
  PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.EnteredBackground, nil);
end;

class procedure TApplicationDelegate.applicationDidFailToRegisterForRemoteNotificationsWithError(self: id; _cmd: SEL;
  application: PUIApplication; error: PNSError);
var
  Message:  TPushFailToRegisterMessage;
  ErrorDescription: string;
begin
  ErrorDescription := NSStrToStr(TNSError.Wrap(error).localizedDescription);
  Message := TPushFailToRegisterMessage.Create(TPushFailToRegisterData.Create(ErrorDescription));
  TMessageManager.DefaultManager.SendMessage(nil, Message);
end;

class function TApplicationDelegate.applicationDidFinishLaunchingWithOptions(self: id; _cmd: SEL; application: PUIApplication;
  options: PNSDictionary): Boolean;

  procedure ReceivedStartupNotification(const ANotification: PNSDictionary);
  var
    LMessage: TPushStartupNotificationMessage;
  begin
    LMessage := TPushStartupNotificationMessage.Create(TPushNotificationData.Create(PNSDictionaryToJSON(ANotification)));
    TMessageManager.DefaultManager.SendMessage(nil, LMessage);
  end;

var
  StartupOptions: NSDictionary;
  NotificationSettings: UIUserNotificationSettings;
  LocalNotification: UILocalNotification;
  RemoteNotification: Pointer;
  WindowManager: TCocoaTouchWindowManager;

//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
  AppDelegate_applicationDidFinishLaunchingWithOptions: TAppDelegate_applicationDidFinishLaunchingWithOptions;
  AppDelegate_applicationDidFinishLaunchingWithOptionsMessage: TAppDelegateMessage_applicationDidFinishLaunchingWithOptions;
begin
  AppDelegate_applicationDidFinishLaunchingWithOptions.Application := TUIApplication.Wrap(application);
  AppDelegate_applicationDidFinishLaunchingWithOptions.Options := TNSDictionary.Wrap(options);
  AppDelegate_applicationDidFinishLaunchingWithOptionsMessage := TAppDelegateMessage_applicationDidFinishLaunchingWithOptions.Create(AppDelegate_applicationDidFinishLaunchingWithOptions);
  TMessageManager.DefaultManager.SendMessage(Self, AppDelegate_applicationDidFinishLaunchingWithOptionsMessage);
//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/

  Include(FState, TApplicationDelegate.TApplicationTransitionState.Launching);
  try
    StartupOptions := TNSDictionary.Wrap(options);

    // Notifications permissions
    if TOSVersion.Check(8) and CheckLocalNotificationPermission then
    begin
      NotificationSettings := TUIUserNotificationSettings.Wrap(TUIUserNotificationSettings.OCClass.settingsForTypes(
        UIUserNotificationTypeBadge or UIUserNotificationTypeSound or UIUserNotificationTypeAlert, nil));
      TiOSHelper.SharedApplication.registerUserNotificationSettings(NotificationSettings);
    end;

    // Local notification
    if TOSVersion.Check(10) then
    begin
      PlatformCocoaTouch.FNotificationCenterDelegate := TNotificationCenterDelegate.Create;
      TUNUserNotificationCenter.OCClass.currentNotificationCenter.setDelegate(PlatformCocoaTouch.FNotificationCenterDelegate);
    end
    else if StartupOptions.valueForKey(UIApplicationLaunchOptionsLocalNotificationKey) <> nil then
    begin
      LocalNotification := TUILocalNotification.Wrap(StartupOptions.valueForKey(UIApplicationLaunchOptionsLocalNotificationKey));
      TMessageManager.DefaultManager.SendMessage(nil, TMessageReceivedNotification.Create(LocalNotification));
    end;

    // Remote notification
    RemoteNotification := StartupOptions.valueForKey(UIApplicationLaunchOptionsRemoteNotificationKey);
    if RemoteNotification <> nil then
      ReceivedStartupNotification(RemoteNotification);

    // Creating window
    WindowManager := PlatformCocoaTouch.WindowManager;
    WindowManager.Window := TFMXWindow.Create(MainScreen.bounds);
    WindowManager.RootViewController := TFMXViewController.Create;
    WindowManager.NativeWindow.makeKeyAndVisible;

    // Post initialization
    PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.FinishedLaunching, nil);
    FMX.Forms.Application.RealCreateForms;
    TiOSHelper.CurrentDevice.beginGeneratingDeviceOrientationNotifications;
  finally
    Exclude(FState, TApplicationDelegate.TApplicationTransitionState.Launching);
  end;
  Result := True;
end;

class procedure TApplicationDelegate.applicationDidReceiveLocalNotification(self: id; _cmd: SEL;
  application: PUIApplication; notification: Pointer);
var
  LocalNotification: UILocalNotification;
begin
  if UIApplicationStateInactive = TUIApplication.Wrap(application).applicationState then
  begin
    LocalNotification := TUILocalNotification.Wrap(notification);
    TMessageManager.DefaultManager.SendMessage(nil, TMessageReceivedNotification.Create(LocalNotification));
  end;
end;

class procedure TApplicationDelegate.applicationDidReceiveMemoryWarning(self: id; _cmd: SEL;
  application: PUIApplication);
begin
  PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.LowMemory, nil);
end;

class procedure TApplicationDelegate.applicationDidReceiveRemoteNotification(self: id; _cmd: SEL;
  application: PUIApplication; notification: PNSDictionary);
var
  LMessage: TPushRemoteNotificationMessage;
begin
  LMessage := TPushRemoteNotificationMessage.Create(TPushNotificationData.Create(PNSDictionaryToJSON(notification)));
  TMessageManager.DefaultManager.SendMessage(nil, LMessage);
end;

class procedure TApplicationDelegate.applicationDidRegisterForRemoteNotificationsWithDeviceToken(self: id; _cmd: SEL;
  application: PUIApplication; deviceToken: PNSData);
var
  Message:  TPushDeviceTokenMessage;
  Token: string;
begin
  if deviceToken <> nil then
  begin
    Token := NSStrToStr(TNSData.Wrap(deviceToken).description);
    //The token comes in like "< abcdef ghij klmno >" - we want it plain: "abcdefghijklmno"
    Token := Token.Replace('<', '', [rfReplaceAll]).Replace('>', '', [rfReplaceAll]).Replace(' ', '', [rfReplaceAll]);
    Message := TPushDeviceTokenMessage.Create(TPushDeviceTokenData.Create(Token, deviceToken));  // https://quality.embarcadero.com/browse/RSP-21539
    TMessageManager.DefaultManager.SendMessage(nil, Message);
  end;
end;

class function TApplicationDelegate.applicationOpenURLWithOptions(self: id; _cmd: SEL; application: PUIApplication;
  url: Pointer; options: PNSDictionary): Boolean;
var
  URLString: string;

//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
  AppDelegate_applicationOpenURLWithOptions: TAppDelegate_applicationOpenURLWithOptions;
  AppDelegate_applicationOpenURLWithOptionsMessage: TAppDelegateMessage_applicationOpenURLWithOptions;
begin
  AppDelegate_applicationOpenURLWithOptions.Application := TUIApplication.Wrap(application);
  AppDelegate_applicationOpenURLWithOptions.Url := TNSUrl.Wrap(url);
  AppDelegate_applicationOpenURLWithOptions.Options := TNSDictionary.Wrap(options);
  AppDelegate_applicationOpenURLWithOptionsMessage := TAppDelegateMessage_applicationOpenURLWithOptions.Create(AppDelegate_applicationOpenURLWithOptions);
  TMessageManager.DefaultManager.SendMessage(Self, AppDelegate_applicationOpenURLWithOptionsMessage);
//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/

  if url <> nil then
    URLString := NSStrToStr(TNSURL.Wrap(url).absoluteString)
  else
    URLString := string.Empty;

  Result := PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.OpenURL,
    TiOSOpenApplicationContext.Create(string.Empty, URLString, options));
end;

class function TApplicationDelegate.applicationOpenURLWithSourceAnnotation(self: id; _cmd: SEL;
  application: PUIApplication; url: Pointer; sourceApplication: PNSString; annotation: id): Boolean;
var
  URLString: string;
  SourceAppString: string;

//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
  AppDelegate_applicationOpenURLWithSourceAnnotation: TAppDelegate_applicationOpenURLWithSourceAnnotation;
  AppDelegate_applicationOpenURLWithSourceAnnotationMessage: TAppDelegateMessage_applicationOpenURLWithSourceAnnotation;
begin
  AppDelegate_applicationOpenURLWithSourceAnnotation.Application := TUIApplication.Wrap(application);
  AppDelegate_applicationOpenURLWithSourceAnnotation.Url := TNSUrl.Wrap(url);
  AppDelegate_applicationOpenURLWithSourceAnnotation.SourceApplication := TNSString.Wrap(sourceApplication);
  AppDelegate_applicationOpenURLWithSourceAnnotation.Annotation := annotation;
  AppDelegate_applicationOpenURLWithSourceAnnotationMessage := TAppDelegateMessage_applicationOpenURLWithSourceAnnotation.Create(AppDelegate_applicationOpenURLWithSourceAnnotation);
  TMessageManager.DefaultManager.SendMessage(Self, AppDelegate_applicationOpenURLWithSourceAnnotationMessage);
//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/

  if url <> nil then
    URLString := NSStrToStr(TNSURL.Wrap(url).absoluteString)
  else
    URLString := string.Empty;

  if sourceApplication <> nil then
    SourceAppString := NSStrToStr(TNSString.Wrap(sourceApplication))
  else
    SourceAppString := string.Empty;

  Result := PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.OpenURL,
    TiOSOpenApplicationContext.Create(SourceAppString, URLString, annotation));
end;

class procedure TApplicationDelegate.applicationSignificantTimeChange(self: id; _cmd: SEL; application: PUIApplication);
begin
  PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.TimeChange, nil);
end;

class procedure TApplicationDelegate.applicationWillChangeStatusBarFrame(self: id; _cmd: SEL;
  application: PUIApplication; newStatusBarFrame: CGRect);
begin
  Include(FState, TApplicationDelegate.TApplicationTransitionState.IncomingCall);
end;

class procedure TApplicationDelegate.applicationWillChangeStatusBarOrientationDuration(self: id; _cmd: SEL;
  application: PUIApplication; newStatusBarOrientation: UIInterfaceOrientation; duration : NSTimeInterval);
begin
  FRotatingDuration := duration;
  Include(FState, TApplicationDelegate.TApplicationTransitionState.Rotating);
end;

class procedure TApplicationDelegate.applicationWillEnterForeground(self: id; _cmd: SEL; application: PUIApplication);
begin
  PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.WillBecomeForeground, nil);
end;

class procedure TApplicationDelegate.applicationWillResignActive(self: id; _cmd: SEL; application: PUIApplication);
begin
  PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.WillBecomeInactive, nil);
end;

class procedure TApplicationDelegate.applicationWillTerminate(self: id; _cmd: SEL; application: PUIApplication);
begin
  PlatformCocoaTouch.HandleApplicationEvent(TApplicationEvent.WillTerminate, nil);
end;

class function TApplicationDelegate.CheckLocalNotificationPermission: Boolean;
const
  FMLocalNotificationPermission = 'FMLocalNotificationPermission'; //do not localize
var
  Value: Pointer;
  ValueStr: string;
begin
  Value := TiOSHelper.MainBundle.objectForInfoDictionaryKey(StrToNSStr(FMLocalNotificationPermission));
  Result := Value <> nil;
  if Result then
  begin
    ValueStr := NSStrToStr(TNSString.Wrap(Value));
    Result := SameText(ValueStr.ToLower, 'true'); //do not localize
  end;
end;

class procedure TApplicationDelegate.CreateDelegateMetaClass;
var
  DelegateClass: Pointer;
begin
  // Set up application delegate manually for now
  // Create a class to serve as our application delegate
  DelegateClass := objc_allocateClassPair(objc_getClass('NSObject'), DelegateName, 0);

  // Add the UIApplciationDelegate protocol
  class_addProtocol(DelegateClass, objc_getProtocol('UIApplicationDelegate'));

  // Application startup
  class_addMethod(DelegateClass, sel_getUid('application:didFinishLaunchingWithOptions:'),
    @applicationDidFinishLaunchingWithOptions, 'B@:@@');

  // Application state
  class_addMethod(DelegateClass, sel_getUid('applicationDidEnterBackground:'),
    @applicationDidEnterBackground, 'v@:@');
  class_addMethod(DelegateClass, sel_getUid('applicationDidBecomeActive:'),
    @applicationDidBecomeActive, 'v@:@');
  class_addMethod(DelegateClass, sel_getUid('applicationWillEnterForeground:'),
    @applicationWillEnterForeground, 'v@:@');
  class_addMethod(DelegateClass, sel_getUid('applicationWillTerminate:'),
    @applicationWillTerminate, 'v@:@');
  class_addMethod(DelegateClass, sel_getUid('applicationDidReceiveMemoryWarning:'),
    @applicationDidReceiveMemoryWarning, 'v@:@');
  class_addMethod(DelegateClass, sel_getUid('applicationWillResignActive:'),
    @applicationWillResignActive, 'v@:@');
  class_addMethod(DelegateClass, sel_getUid('applicationSignificantTimeChange:'),
    @applicationSignificantTimeChange, 'v@:@');

  // Managing Interface Geometry
  class_addMethod(DelegateClass, sel_getUid('application:willChangeStatusBarOrientation:duration:'),
    @applicationWillChangeStatusBarOrientationDuration, 'v@:@id');
  class_addMethod(DelegateClass, sel_getUid('application:didChangeStatusBarOrientation:'),
    @applicationDidChangeStatusBarOrientation, 'v@:@i');
  class_addMethod(DelegateClass, sel_getUid('application:willChangeStatusBarFrame:'),
    @applicationWillChangeStatusBarFrame, 'v@:@{CGRect=ffff}');
  class_addMethod(DelegateClass, sel_getUid('application:didChangeStatusBarFrame:'),
    @applicationDidChangeStatusBarFrame, 'v@:@{CGRect=ffff}');

  // Local and Remote Notifications
  class_addMethod(DelegateClass, sel_getUid('application:didReceiveLocalNotification:'),
    @applicationDidReceiveLocalNotification, 'v@:@@');
  class_addMethod(DelegateClass, sel_getUid('application:didReceiveRemoteNotification:'),
    @applicationDidReceiveRemoteNotification, 'v@:@@');
  class_addMethod(DelegateClass, sel_getUid('application:didRegisterForRemoteNotificationsWithDeviceToken:'),
    @applicationDidRegisterForRemoteNotificationsWithDeviceToken, 'v@:@@');
  class_addMethod(DelegateClass, sel_getUid('application:didFailToRegisterForRemoteNotificationsWithError:'),
    @applicationDidFailToRegisterForRemoteNotificationsWithError, 'v@:@@');

  // Opening a URL-Specified Resource
  if TOSVersion.Major >= 9 then
    class_addMethod(DelegateClass, sel_getUid('application:openURL:options:'),
      @applicationOpenURLWithOptions, 'B@:@@@@')
  else
    class_addMethod(DelegateClass, sel_getUid('application:openURL:sourceApplication:annotation:'),
      @applicationOpenURLWithSourceAnnotation, 'B@:@@@@');

  // Register the delegate class
  objc_registerClassPair(DelegateClass);
end;

{ TPlatformCocoaTouch }

procedure RunLoopObserverCallback(observer: CFRunLoopObserverRef; activity: CFRunLoopActivity; info: Pointer); cdecl;
var
  Done: Boolean;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    CheckSynchronize;
  Application.DoIdle(Done);
end;

constructor TPlatformCocoaTouch.Create;
begin
  inherited;
  ALAniCalcTimerProcs := Tlist<TALAniCalculations>.create; // added to support ALFmxInertialMovement
  FTimerService := TCocoaTouchTimerService.Create;
  FMetricsServices := TCocoaTouchMetricsServices.Create;
  FGraphicServices := TCocoaTouchGraphicServices.Create;
  FDeviceServices := TCocoaTouchDeviceServices.Create;
  FSaveStateService := TCocoaTouchSaveStateService.Create;
  FScreenServices := TCocoaTouchScreenServices.Create;
  FLoggerService := TCocoaTouchLoggerService.Create;
  FWindowManagerService := TCocoaTouchWindowManager.Create;
  TApplicationDelegate.CreateDelegateMetaClass;

  Application := TApplication.Create(nil);
  InitializeFormFactor(Application.FormFactor);
  FRunning := False;

  FWakeHandler := TFMXWakeHandler.Create;
  System.Classes.WakeMainThread := WakeMainThread;

  FRunLoopObserver := CFRunLoopObserverCreate(kCFAllocatorDefault, kCFRunLoopBeforeWaiting, True, 0,
    RunLoopObserverCallback, nil);
  CFRunLoopAddObserver(CFRunLoopGetCurrent, FRunLoopObserver, kCFRunLoopCommonModes);
end;

destructor TPlatformCocoaTouch.Destroy;
begin
  FreeAndNil(FNotificationCenterDelegate);
  FreeAndNil(FScreenServices);
  FreeAndNil(FSaveStateService);
  FreeAndNil(FTimerService);
  FreeAndNil(FDeviceServices);
  FreeAndNil(FGraphicServices);
  FreeAndNil(FMetricsServices);
  FreeAndNil(FLoggerService);
  CFRunLoopRemoveObserver(CFRunLoopGetCurrent, FRunLoopObserver, kCFRunLoopCommonModes);
  CFRelease(FRunLoopObserver);
  System.Classes.WakeMainThread := nil;
  FreeAndNil(FWakeHandler);
  ALAniCalcTimerProcs.Free; // added to support ALFmxInertialMovement
  ALAniCalcTimerProcs := nil;
  inherited;
end;

{ App =========================================================================}

procedure TPlatformCocoaTouch.Run;
begin
  FRunning := True;
{$WARN SYMBOL_PLATFORM OFF}
  ExitCode := UIApplicationMain(System.ArgCount, System.ArgValues, nil, StringToID(TApplicationDelegate.DelegateName));
{$WARN SYMBOL_PLATFORM DEFAULT}
end;

function TPlatformCocoaTouch.Terminating: Boolean;
begin
  Result := FTerminating;
end;

function TPlatformCocoaTouch.Running: Boolean;
begin
  Result := FRunning;
end;

procedure TPlatformCocoaTouch.Terminate;
begin
  FTerminating := True;
  FRunning := False;
  TMessageManager.DefaultManager.SendMessage(nil, TApplicationTerminatingMessage.Create);
  raise EUnsupportedPlatformService.CreateFMT(SUnsupportedPlatformService, ['Terminate']);
end;

function TPlatformCocoaTouch.HandleApplicationEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
var
  ApplicationEventMessage: TApplicationEventMessage;
begin
  Result := False;

  { Send broadcast message }
  ApplicationEventMessage := TApplicationEventMessage.Create(TApplicationEventData.Create(AEvent, AContext));
  TMessageManager.DefaultManager.SendMessage(nil, ApplicationEventMessage);

  { Invoke application event}
  if Assigned(FOnApplicationEvent) then
    try
      Result := FOnApplicationEvent(AEvent, AContext);
    except
      Application.HandleException(Self);
    end;
end;

function TPlatformCocoaTouch.HandleMessage: Boolean;
begin
  WaitMessage;
  Result := False;
end;

procedure TPlatformCocoaTouch.WaitMessage;
begin
  InternalWaitMessage;
end;

procedure TPlatformCocoaTouch.WakeMainThread(Sender: TObject);
begin
  FWakeHandler.NativeObject.performSelectorOnMainThread(sel_getUid('DoCheckSynchronize'), nil, False,
    TNSArray.Wrap(TNSArray.OCClass.arrayWithObject(NSObjectToID(NSDefaultRunLoopMode))));
end;

function TPlatformCocoaTouch.GetDefaultTitle: string;
var
  AppNameKey: Pointer;
  NSAppName: NSString;
begin
  AppNameKey := StringToID('CFBundleName'); // do not localize
  NSAppName := TNSString.Wrap(TiOSHelper.MainBundle.infoDictionary.objectForKey(AppNameKey));
  Result := NSStrToStr(NSAppName);
end;

function TPlatformCocoaTouch.GetTitle: string;
begin
  Result := FTitle;
end;

function TPlatformCocoaTouch.GetVersionString: string;
var
  VersionObject: Pointer;
begin
  VersionObject := TiOSHelper.MainBundle.infoDictionary.objectForKey(StringToID('CFBundleVersion'));  // do not localize
  if VersionObject <> nil then
    Result := NSStrToStr(TNSString.Wrap(VersionObject))
  else
    Result := string.Empty;
end;

procedure TPlatformCocoaTouch.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

{ Text Service }

procedure TTextServiceCocoa.InsertText(const AText: string);
begin
  if HasMarkedText then
  begin
    InternalSetMarkedText(string.Empty);
    InternalEndIMEInput;
  end;
end;

function TTextServiceCocoa.DeleteBackward: Boolean;
var
  LMarkedText: string;
begin
  if HasMarkedText then
  begin
    LMarkedText := FMarkedText.Remove(FMarkedText.Length - 1);
    SetCursorShift(LMarkedText.Length);
    InternalSetMarkedText(LMarkedText);
    Result := True;
  end
  else
    Result := False;
end;

procedure TTextServiceCocoa.SetMarkedText(const AText: string);
var
  TextInput: ITextInput;
begin
  if not HasMarkedText and Supports(Owner, ITextInput, TextInput) and (TextInput.GetSelectionBounds.Width = 0) then
    InternalStartIMEInput;

  SetCursorShift(AText.Length);
  InternalSetMarkedText(AText);
end;

procedure TTextServiceCocoa.UpdateMarkedTextRange;
begin
  FMarkedTextRange.RangeStart.Position := FCaretPostion.X;
  FMarkedTextRange.RangeStart.Line := FCaretPostion.Y;
  FMarkedTextRange.RangeEnd.Position := FCaretPostion.X + FMarkedText.Length;
  FMarkedTextRange.RangeEnd.Line := FCaretPostion.Y;
end;

procedure TTextServiceCocoa.SelectionUpdated;
var
  TextInput: ITextInput;
  SelRect: TRect;
begin
  if Supports(Owner, ITextInput, TextInput) then
  begin
    SelRect := TextInput.GetSelectionBounds;
    if (SelRect.Top > SelRect.Bottom) or ((SelRect.Height = 0) and (SelRect.Left > SelRect.Right)) then
    begin
      FSelectedTextRange.RangeStart.Position := SelRect.Right;
      FSelectedTextRange.RangeStart.Line := SelRect.Bottom;
      FSelectedTextRange.RangeEnd.Position := SelRect.Left;
      FSelectedTextRange.RangeEnd.Line := SelRect.Top;
    end
    else
    begin
      FSelectedTextRange.RangeStart.Position := SelRect.Left;
      FSelectedTextRange.RangeStart.Line := SelRect.Top;
      FSelectedTextRange.RangeEnd.Position := SelRect.Right;
      FSelectedTextRange.RangeEnd.Line := SelRect.Bottom;
    end;
  end
  else
  begin
    FSelectedTextRange.RangeStart.Position := CaretPosition.X;
    FSelectedTextRange.RangeStart.Line := CaretPosition.Y;
    FSelectedTextRange.RangeEnd.Position := CaretPosition.X;
    FSelectedTextRange.RangeEnd.Line := CaretPosition.Y;
  end;
  FDocumentEnd.Line := FSelectedTextRange.RangeEnd.Line;
end;

function TTextServiceCocoa.GetText: string;
begin
  Result := FText;
end;

procedure TTextServiceCocoa.SetText(const Value: string);
begin
  FText := Value;
  FDocumentEnd.Position := Value.Length;
  SelectionUpdated;
end;

function TTextServiceCocoa.GetCaretPosition: TPoint;
begin
  Result := FCaretPostion;
end;

procedure TTextServiceCocoa.SetCaretPosition(const Value: TPoint);
begin
  FCaretPostion := Value;
  if not HasMarkedText then
    SelectionUpdated;
end;

procedure TTextServiceCocoa.SetCursorShift(const Value: Integer);
begin
  FCursorShift := Value;
end;

procedure TTextServiceCocoa.InternalSetMarkedText(const AMarkedText: string);
begin
  FMarkedText := AMarkedText;
  UpdateMarkedTextRange;
  (Owner as ITextInput).IMEStateUpdated;
end;

function TTextServiceCocoa.InternalGetMarkedText: string;
begin
  Result := FMarkedText;
end;

procedure TTextServiceCocoa.InternalStartIMEInput;
begin
  UpdateMarkedTextRange;
  (Owner as ITextInput).StartIMEInput;
end;

procedure TTextServiceCocoa.InternalBreakIMEInput;
begin
  FMarkedText := string.Empty;
  FCursorShift := 0;
  (Owner as ITextInput).IMEStateUpdated;
end;

procedure TTextServiceCocoa.InternalEndIMEInput;
begin
  (Owner as ITextInput).EndIMEInput;
  FMarkedText := string.Empty;
  FCursorShift := 0;
  UpdateMarkedTextRange;
end;

function TTextServiceCocoa.CombinedText: string;
begin
  if not FMarkedText.IsEmpty then
    Result := System.Copy(FText, 1, FCaretPostion.X) + FMarkedText + System.Copy(FText, FCaretPostion.X + 1, MaxInt)
  else
    Result := FText;
end;

function TTextServiceCocoa.TargetClausePosition: TPoint;
begin
  Result := CaretPosition;
  Result.X := Result.X + FCursorShift;
end;

procedure TTextServiceCocoa.EnterControl(const FormHandle: TWindowHandle);
var
  View: TFMXViewBase;
  VirtKBControl: IVirtualKeyboardControl;
begin
  View := TFMXViewBase(WindowHandleToPlatform(FormHandle).Handle);
  FView := View;
  View.FTextService := Self;
  if Supports(View.Form.Focused, IVirtualKeyboardControl, VirtKBControl) then
  begin
    View.FKeyboardType := VirtKBControl.KeyboardType;
    View.FReturnKeyType := VirtKBControl.ReturnKeyType;
    View.FPassword := VirtKBControl.IsPassword;
  end
  else
  begin
    View.FKeyboardType := TVirtualKeyboardType.Default;
    View.FReturnKeyType := TReturnKeyType.Default;
    View.FPassword := False;
  end;
  View.unmarkText;
  SelectionUpdated;
end;

procedure TTextServiceCocoa.ExitControl(const FormHandle: TWindowHandle);
begin
  FView.FTextService := nil;
  FView.unmarkText;
  FView := nil;
end;

procedure TTextServiceCocoa.DrawSingleLine(const Canvas: TCanvas; const ARect: TRectF; const FirstVisibleChar: Integer;
  const Font: TFont; const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  I: Integer;
  S: string;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := TPointF.Create(ARect.Width, ARect.Height);
    Layout.WordWrap := AWordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Font;
    Layout.Color := Canvas.Fill.Color;
    Layout.Opacity := AOpacity;
    Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
    S := CombinedText;
    Layout.Text := S.Substring(FirstVisibleChar - 1, S.Length - FirstVisibleChar + 1);
    Layout.EndUpdate;
    Layout.RenderLayout(Canvas);

    if not FMarkedText.IsEmpty then
    try
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.Stroke.Thickness := 1;
      Canvas.Stroke.Dash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X, FMarkedText.Length));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          TPointF.Create(Region[I].Left, Region[I].Bottom),
          TPointF.Create(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);

      if FSelectedTextRange.RangeStart <> FSelectedTextRange.RangeEnd then
      begin
        Canvas.Stroke.Thickness := 3;
        Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X + 1 + FSelectedTextRange.RangeStart.Position,
		  FSelectedTextRange.RangeEnd.Position - FSelectedTextRange.RangeStart.Position));
        for I := Low(Region) to High(Region) do
          Canvas.DrawLine(
            TPointF.Create(Region[I].Left, Region[I].Bottom),
            TPointF.Create(Region[I].Right, Region[I].Bottom),
            AOpacity, Canvas.Stroke);
      end;
    finally
      Canvas.Stroke.Thickness := 1;
      Canvas.Stroke.Dash := TStrokeDash.Solid;
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

procedure TTextServiceCocoa.DrawSingleLine(const Canvas: TCanvas; const S: string; const ARect: TRectF;
  const Font: TFont; const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  I: Integer;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := TPointF.Create(ARect.Width, ARect.Height);
    Layout.WordWrap := AWordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Font;
    Layout.Color := Canvas.Fill.Color;
    Layout.Opacity := AOpacity;
    Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
    Layout.Text := S;
    Layout.EndUpdate;
    Layout.RenderLayout(Canvas);

    if not FMarkedText.IsEmpty then
    try
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.Stroke.Thickness := 1;
      Canvas.Stroke.Dash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X, FMarkedText.Length));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          TPointF.Create(Region[I].Left, Region[I].Bottom),
          TPointF.Create(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);

      if FSelectedTextRange.RangeStart <> FSelectedTextRange.RangeEnd then
      begin
        Canvas.Stroke.Thickness := 3;
        Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X + FSelectedTextRange.RangeStart.Position,
		  FSelectedTextRange.RangeEnd.Position - FSelectedTextRange.RangeStart.Position));
        for I := Low(Region) to High(Region) do
          Canvas.DrawLine(
            TPointF.Create(Region[I].Left, Region[I].Bottom),
            TPointF.Create(Region[I].Right, Region[I].Bottom),
            AOpacity, Canvas.Stroke);
      end;
    finally
      Canvas.Stroke.Thickness := 1;
      Canvas.Stroke.Dash := TStrokeDash.Solid;
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

function TTextServiceCocoa.HasMarkedText: boolean;
begin
  Result := not FMarkedText.IsEmpty;
end;

function TTextServiceCocoa.GetImeMode: TImeMode;
begin
  Result := FImeMode;
end;

procedure TTextServiceCocoa.SetImeMode(const Value: TImeMode);
begin
  FImeMode := Value;
end;

procedure TTextServiceCocoa.BeginSelection;
begin
  if (FView <> nil) and (FView.FInputDelegate <> nil) then
  begin
    FView.FInputDelegate.selectionWillChange(FView.GetObjectID);
    FView.HideContextMenu;
  end;
end;

procedure TTextServiceCocoa.EndSelection;
begin
  if (FView <> nil) and (FView.FInputDelegate <> nil) then
  begin
    FView.FInputDelegate.selectionDidChange(FView.GetObjectID);
    FView.FContextMenu.Show;
  end;
end;

constructor TTextServiceCocoa.Create(const Owner: IControl; SupportMultiLine: Boolean);
begin
  FDocumentBegin := TFMXTextPosition.Create(0, 0, True);
  FDocumentEnd := TFMXTextPosition.Create(0, 0, True);
  FSelectedTextRange := TFMXTextRange.Create(0, 0, 0, 0, True);
  FMarkedTextRange := TFMXTextRange.Create(0, 0, 0, 0, True);
  inherited;
end;

destructor TTextServiceCocoa.Destroy;
begin
  FDocumentBegin.Free;
  FDocumentEnd.Free;
  FSelectedTextRange.Free;
  FMarkedTextRange.Free;
  inherited;
end;

{ TFMXTextPosition }

function TFMXTextPosition.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXTextPosition);
end;

constructor TFMXTextPosition.Create(const APosition, ALine: Integer; const AHardLink: Boolean = False);
begin
  inherited Create;
  FPosition := APosition;
  FLine := ALine;
  FHardLink := AHardLink;
end;

destructor TFMXTextPosition.Destroy;
begin
  inherited;
end;

function TFMXTextPosition.ToUITextPosition: UITextPosition;
begin
  if RefCount > 0 then
  begin
    if RefCount <= 1 then
      if not FHardLink then
        _AddRef;
    Result := TUITextPosition.Wrap(GetObjectID);
  end
  else
    Result := nil;
end;

class function TFMXTextPosition.FromUITextPosition(const APosition: UITextPosition): TFMXTextPosition;
var
  ObjectPointer: Pointer;
begin
  if APosition.retainCount > 0 then
  begin
    object_getInstanceVariable(ILocalObject(APosition).GetObjectID, MarshaledAString('FMXImplObject'), ObjectPointer);
    Result := TFMXTextPosition(ObjectPointer);
  end
  else
    Result := nil;
end;

class operator TFMXTextPosition.Equal(const A, B: TFMXTextPosition): Boolean;
begin
  Result := (A.Line = B.Line) and (A.Position = B.Position);
end;

class operator TFMXTextPosition.NotEqual(const A, B: TFMXTextPosition): Boolean;
begin
  Result := (A.Line <> B.Line) or (A.Position <> B.Position);
end;

class operator TFMXTextPosition.GreaterThan(const A, B: TFMXTextPosition): Boolean;
begin
  Result := (A.Line > B.Line) or ((A.Line = B.Line) and (A.Position > B.Position));
end;

class operator TFMXTextPosition.GreaterThanOrEqual(const A, B: TFMXTextPosition): Boolean;
begin
  Result := (A.Line > B.Line) or ((A.Line = B.Line) and (A.Position >= B.Position));
end;

class operator TFMXTextPosition.LessThan(const A, B: TFMXTextPosition): Boolean;
begin
  Result := (A.Line < B.Line) or ((A.Line = B.Line) and (A.Position < B.Position));
end;

class operator TFMXTextPosition.LessThanOrEqual(const A, B: TFMXTextPosition): Boolean;
begin
  Result := (A.Line < B.Line) or ((A.Line = B.Line) and (A.Position <= B.Position));
end;

{ TFMXTextRange }

function TFMXTextRange.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXTextRange);
end;

constructor TFMXTextRange.Create(const AStartPosition, AStartLine, AEndPosition, AEndLine: Integer;
  const AHardLink: Boolean);
begin
  inherited Create;
  FHardLink := AHardLink;
  FStart := TFMXTextPosition.Create(AStartPosition, AStartLine, True);
  FEnd := TFMXTextPosition.Create(AEndPosition, AEndLine, True);
end;

destructor TFMXTextRange.Destroy;
begin
  FStart := nil;
  FEnd := nil;
  inherited;
end;

function TFMXTextRange.isEmpty: Boolean;
begin
  Result := (Self = nil) or (FStart = nil) or (FEnd = nil) or (FStart = FEnd);
end;

function TFMXTextRange.start: UITextPosition;
begin
  if (Self <> nil) and (FStart <> nil) then
    Result := FStart.ToUITextPosition;
end;

function TFMXTextRange.&end: UITextPosition;
begin
  if (Self <> nil) and (FEnd <> nil) then
    Result := FEnd.ToUITextPosition;
end;

function TFMXTextRange.ToUITextRange: UITextRange;
begin
  if RefCount <= 1 then
    if not FHardLink then
      _AddRef;
  Result := TUITextRange.Wrap(GetObjectID);
end;

class function TFMXTextRange.FromUITextRange(const ARange: UITextRange): TFMXTextRange;
var
  ObjectPointer: Pointer;
begin
  if ARange.retainCount > 0 then
  begin
    object_getInstanceVariable(ILocalObject(ARange).GetObjectID, MarshaledAString('FMXImplObject'), ObjectPointer);
    Result := TFMXTextRange(ObjectPointer);
  end
  else
    Result := nil;
end;

function TPlatformCocoaTouch.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceCocoa;
end;

procedure TPlatformCocoaTouch.RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
end;

procedure TPlatformCocoaTouch.AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  if (AForm <> nil) and (AForm.Handle <> nil) then
    TFMXViewBase(WindowHandleToPlatform(AForm.Handle).Handle).AddRecognizer(ARec);
end;

procedure TPlatformCocoaTouch.SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
begin
  FOnApplicationEvent := AEventHandler;
end;

function TPlatformCocoaTouch.GetMousePos: TPointF;
begin
  Result := FMouseCoord;
end;

procedure TPlatformCocoaTouch.InitializeFormFactor(AFormFactor: TFormFactor);
begin
  AFormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  if IsPad then
    AFormFactor.Orientations := AFormFactor.Orientations + [TFormOrientation.InvertedPortrait];
end;

{ TFMXViewController }

function TFMXViewController.CaptureScreenSnapshot: UIImageView;
var
  Image: UIImage;
begin
  Image := UIViewToUIImage(View);
  Result := TUIImageView.Wrap(TUIImageView.Alloc.initWithImage(Image));
  Result.setFrame(View.Frame);
end;

procedure TFMXViewController.BeforeOrientationChange;
var
  Form: TCommonCustomForm;
  FormView: UIView;
begin
  if Screen.ActiveForm <> nil then
    Form := Screen.ActiveForm
  else if Application.MainForm <> nil then
    Form := Application.MainForm
  else
    Form := nil;

  if Form <> nil then
  begin
    FormView := WindowHandleToPlatform(Form.Handle).View;
    FRotationView := CaptureScreenSnapshot;
    if FRotationView <> nil then
    begin
      View.addSubview(FRotationView);
      FRotationView.setAlpha(1);
      FormView.setAlpha(0);
      // Cross fade
      TUIView.OCClass.beginAnimations(nil, nil);
      try
        FRotationView.setAlpha(0);
        FormView.setAlpha(1);
      finally
        TUIView.OCClass.commitAnimations;
      end;
    end;
  end;
end;

procedure TFMXViewController.AfterOrientationChange;
begin
  if FRotationView <> nil then
  begin
    FRotationView.removeFromSuperview;
    FRotationView.release;
    FRotationView := nil;
  end;
  TMessageManager.DefaultManager.SendMessage(Self, TOrientationChangedMessage.Create, True);
end;

function TFMXViewController.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXViewController);
end;

function TFMXViewController.GetView: UIVIew;
begin
  if ViewController.isViewLoaded then
    Result := ViewController.view
  else
    Result := nil;
end;

function TFMXViewController.GetViewController: UIViewController;
begin
  Result := UIViewController(Super);
end;

procedure TFMXViewController.loadView;
var
  RootView: UIView;
  Constraint: NSLayoutConstraint;
begin
  RootView := TUIView.Create;
  RootView.setOpaque(False);

  FStatusBar := TUIView.Create;
  FStatusBar.setTranslatesAutoresizingMaskIntoConstraints(False);
  RootView.addSubview(FStatusBar);
  ViewController.setView(RootView);

  Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FStatusBar), NSLayoutAttributeLeft,
    NSLayoutRelationEqual, NSObjectToID(RootView), NSLayoutAttributeLeft, 1, 0));
  Constraint.setActive(True);

  Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FStatusBar), NSLayoutAttributeRight,
    NSLayoutRelationEqual, NSObjectToID(RootView), NSLayoutAttributeRight, 1, 0));
  Constraint.setActive(True);

                                       
//  Constraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FStatusBar), NSLayoutAttributeTop,
//    NSLayoutRelationEqual, NSObjectToID(RootView), NSLayoutAttributeTopMargin, 1, 0));
//  Constraint.setActive(True);

  FStatusBarHeightConstraint := TNSLayoutConstraint.Wrap(TNSLayoutConstraint.OCClass.constraintWithItem(NSObjectToID(FStatusBar), NSLayoutAttributeHeight,
    NSLayoutRelationEqual, nil, NSLayoutAttributeNotAnAttribute, 1, 20));
  FStatusBarHeightConstraint.setActive(True);

  FStatusBar.updateConstraintsIfNeeded;
end;

function TFMXViewController.preferredStatusBarStyle: UIStatusBarStyle;
const
  MaxLightLuminance = 0.5;
begin
  if FStatusBarLuminance < MaxLightLuminance then
    Result := UIStatusBarStyleLightContent
  else
    Result := UIStatusBarStyleDefault;
end;

function TFMXViewController.prefersStatusBarHidden: Boolean;
begin
  Result := not FStatusBarVisible;
end;

procedure TFMXViewController.SetStatusBarBackgroundColor(const ABackgroundColor: TAlphaColor);
var
  Color: UIColor;
  BackgroundColor: TAlphaColor;
begin
  if ABackgroundColor = TAlphaColorRec.Null then
    BackgroundColor := DefaultStatusBarBackgroundColor
  else
    BackgroundColor := ABackgroundColor;
  Color := AlphaColorToUIColor(BackgroundColor);
  FStatusBar.setBackgroundColor(Color);
  FStatusBarLuminance := Luminance(BackgroundColor);
  ViewController.setNeedsStatusBarAppearanceUpdate;
end;

procedure TFMXViewController.SetStatusBarVisible(const AValue: Boolean);
begin
  FStatusBarVisible := AValue;
  FStatusBar.setHidden(not AValue);
  ViewController.setNeedsStatusBarAppearanceUpdate;
  if AValue then
    View.bringSubviewToFront(FStatusBar)
  else
    View.sendSubviewToBack(FStatusBar);
end;

function TFMXViewController.shouldAutorotate: Boolean;
begin
  Result := True;
end;

procedure TFMXViewController.StartRotation(param1: Pointer);
begin
end;

function TFMXViewController.supportedInterfaceOrientations: NSUInteger;
begin
  Result := ScreenOrientationToUIInterfaceOrientation(Application.FormFactor.Orientations);
end;

procedure TFMXViewController.viewDidLayoutSubviews;
begin
  // Send notification to our controls, that UIViewController changed frame
  if FRootViewSavedFrame <> View.Frame then
  begin
    TiOSHelper.DefaultNotificationCenter.postNotificationName(StringToID(FMXViewControllerFrameChanged), nil);
    FRootViewSavedFrame := View.frame;
  end;
end;

procedure TFMXViewController.viewWillLayoutSubviews;

  function NeedAnimate: Boolean;
  begin
    Result := (TApplicationDelegate.TApplicationTransitionState.Rotating in TApplicationDelegate.TransitionState) or
      (TApplicationDelegate.TApplicationTransitionState.IncomingCall in TApplicationDelegate.TransitionState);
  end;

  function DefineDuration: NSTimeInterval;
  const
    StatusBarChangingDuration = 0.35;
  begin
    if TApplicationDelegate.TApplicationTransitionState.Rotating in TApplicationDelegate.TransitionState then
      Result := TApplicationDelegate.RotatingDuration
    else
      Result := StatusBarChangingDuration;
  end;

var
  NewFrame: NSRect;
  Form: TCommonCustomForm;
  I: Integer;
  FormView: UIView;
  FormBounds: TRect;
begin
  if TOSVersion.Check(11) then
  begin
    // It returns system status bar height
    PlatformCocoaTouch.WindowManager.StatusBarOffset := View.safeAreaLayoutGuide.layoutFrame.origin.y;
    // Refresh height of status bar view spacer TFMXViewController.StatusBarView
    FStatusBarHeightConstraint.setConstant(PlatformCocoaTouch.WindowManager.StatusBarOffset);
    FStatusBar.updateConstraints;
    View.updateConstraints;
  end;

  for I := 0 to Screen.FormCount - 1 do
  begin
    Form := Screen.Forms[I];
    if Form.Visible then
    begin
      NewFrame := PlatformCocoaTouch.WindowManager.CalculateFormViewFrame(Form);
      FormView := WindowHandleToPlatform(Form.Handle).View;
      if NeedAnimate then
      begin
        TUIView.OCClass.beginAnimations(nil, nil);
        try
          TUIView.OCClass.setAnimationDuration(DefineDuration);
          FormView.setFrame(NewFrame);
        finally
          TUIView.OCClass.commitAnimations;
        end;
      end
      else
        FormView.setFrame(NewFrame);

      FormBounds := Form.Bounds;
      Form.SetBounds(FormBounds.Left, FormBounds.Top, FormBounds.Width, FormBounds.Height);
      // When we change frame of form's view, view doesn't repaint itself
      Form.Invalidate;
    end;
  end;
  UIViewController(Super).viewWillLayoutSubviews;
end;

procedure TFMXViewController.viewWillTransitionToSize(size: CGSize; withTransitionCoordinator: UIViewControllerTransitionCoordinator);
const
  ArmArchitecture = [TOSVersion.TArchitecture.arARM32, TOSVersion.TArchitecture.arARM64];
begin
  ViewController.viewWillTransitionToSize(size, withTransitionCoordinator);
  withTransitionCoordinator.animateAlongsideTransition(StartRotation, AfterOrientationChange);
  // Delphi on iOS simulator doesn't support Objective-C blocks, So we don't use cross-fading effect for rotation.
  // Also pay attention, that we do snapshot of screen in BeforeOrientationChange instead of doing it in StartRotation,
  // because in StartRotation all view already has changed frames.
  if TOSVersion.Architecture in ArmArchitecture then
    BeforeOrientationChange;
end;

{ TFMXWindow }

constructor TFMXWindow.Create(const ABounds: NSRect);
var
  V: Pointer;
begin
  inherited Create;
  V := UIWindow(Super).initWithFrame(ABounds);
  if GetObjectID <> V then
    UpdateObjectID(V);
end;

function TFMXWindow.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXWindow);
end;

function TFMXWindow.GetNativeWindow: UIWindow;
begin
  Result := UIWindow(Super);
end;

procedure TFMXWindow.SetRootViewController(const AValue: TFMXViewController);
begin
  FRootViewController := AValue;
  NativeWindow.setRootViewController(FRootViewController.ViewController);
end;

{ TFMXViewBase }

function CorrectLocationInView(const Touch: UITouch; const View: UIView; const Form: TCommonCustomForm): CGPoint; overload;
begin
  Result := Touch.locationInView(View);
end;

function CorrectLocationInView(const GestureRecognizer: UIGestureRecognizer; const View: UIView; const Form: TCommonCustomForm): CGPoint; overload;
begin
  Result := GestureRecognizer.locationInView(View);
end;

procedure TFMXViewBase.AddRecognizer(const Gesture: TInteractiveGesture);
var
  TwoFingerTapRecognizer: UITapGestureRecognizer;
  RotateRecognizer: UIRotationGestureRecognizer;
  ZoomRecognizer: UIPinchGestureRecognizer;
  PanRecognizer: UIPanGestureRecognizer;
  LongTapRecognizer: UILongPressGestureRecognizer;
  DoubleTapRecognizer: UITapGestureRecognizer;
begin
  case Gesture of
    TInteractiveGesture.Zoom:
      begin
        ZoomRecognizer := TUIPinchGestureRecognizer.Alloc;
        ZoomRecognizer := TUIPinchGestureRecognizer.Wrap(ZoomRecognizer.initWithTarget(GetObjectID, sel_getUid('HandleZoom:')));
        ZoomRecognizer.setDelaysTouchesBegan(False);
        ZoomRecognizer.setCancelsTouchesInView(True);
        ZoomRecognizer.setDelegate(GetObjectID);
        View.addGestureRecognizer(ZoomRecognizer);
        ZoomRecognizer.release;
      end;
    TInteractiveGesture.Rotate:
      begin
        RotateRecognizer := TUIRotationGestureRecognizer.Alloc;
        RotateRecognizer := TUIRotationGestureRecognizer.Wrap(RotateRecognizer.initWithTarget(GetObjectID, sel_getUid('HandleRotate:')));
        RotateRecognizer.setDelaysTouchesBegan(False);
        RotateRecognizer.setCancelsTouchesInView(True);
        RotateRecognizer.setDelegate(GetObjectID);
        View.addGestureRecognizer(RotateRecognizer);
        RotateRecognizer.release;
      end;
    TInteractiveGesture.Pan:
      begin
        PanRecognizer := TUIPanGestureRecognizer.Alloc;
        PanRecognizer := TUIPanGestureRecognizer.Wrap(PanRecognizer.initWithTarget(GetObjectID, sel_getUid('HandlePan:')));
        PanRecognizer.setMinimumNumberOfTouches(1);
        PanRecognizer.setMaximumNumberOfTouches(2);
        PanRecognizer.setDelaysTouchesBegan(False);
        PanRecognizer.setCancelsTouchesInView(False);
        PanRecognizer.setDelegate(GetObjectID);
        View.addGestureRecognizer(PanRecognizer);
        PanRecognizer.release;
      end;
    TInteractiveGesture.TwoFingerTap:
      begin
        TwoFingerTapRecognizer := TUITapGestureRecognizer.Alloc;
        TwoFingerTapRecognizer := TUITapGestureRecognizer.Wrap(TwoFingerTapRecognizer.initWithTarget(GetObjectID, sel_getUid('HandleTwoFingerTap:')));
        TwoFingerTapRecognizer.setNumberOfTapsRequired(1);
        TwoFingerTapRecognizer.setNumberOfTouchesRequired(2);
        TwoFingerTapRecognizer.setDelaysTouchesBegan(False);
        TwoFingerTapRecognizer.setCancelsTouchesInView(True);
        TwoFingerTapRecognizer.setDelegate(GetObjectID);
        View.addGestureRecognizer(TwoFingerTapRecognizer);
        TwoFingerTapRecognizer.release;
      end;
    TInteractiveGesture.LongTap:
      begin
        LongTapRecognizer := TUILongPressGestureRecognizer.Alloc;
        LongTapRecognizer := TUILongPressGestureRecognizer.Wrap(LongTapRecognizer.initWithTarget(GetObjectID, sel_getUid('LongTap:')));
        LongTapRecognizer.setDelaysTouchesBegan(False);
        LongTapRecognizer.setCancelsTouchesInView(True);
        LongTapRecognizer.setDelegate(GetObjectID);
        View.addGestureRecognizer(LongTapRecognizer);
        LongTapRecognizer.release;
      end;
    TInteractiveGesture.DoubleTap:
      begin
        DoubleTapRecognizer := TUITapGestureRecognizer.Alloc;
        DoubleTapRecognizer := TUITapGestureRecognizer.Wrap(DoubleTapRecognizer.initWithTarget(GetObjectID, sel_getUid('HandleDoubleTap:')));
        DoubleTapRecognizer.setNumberOfTapsRequired(2);
        DoubleTapRecognizer.setDelegate(GetObjectID);
        View.addGestureRecognizer(DoubleTapRecognizer);
        DoubleTapRecognizer.release;
      end;
  end;
end;

function TFMXViewBase.autocapitalizationType: UITextAutocapitalizationType;
begin
  if FPassword or not (FKeyboardType in [TVirtualKeyboardType.Default, TVirtualKeyboardType.Alphabet, TVirtualKeyboardType.NamePhonePad]) then
    Result := UITextAutocapitalizationTypeNone
  else
    Result := UITextAutocapitalizationTypeSentences;
end;

function TFMXViewBase.autocorrectionType: UITextAutocorrectionType;
begin
  Result := UITextAutocorrectionTypeNo
end;

function TFMXViewBase.baseWritingDirectionForPosition(position: UITextPosition; inDirection: UITextStorageDirection): UITextWritingDirection;
begin
  Result := UITextWritingDirectionLeftToRight;
end;

function TFMXViewBase.beginningOfDocument: UITextPosition;
begin
  if FTextService <> nil then
    Result := FTextService.FDocumentBegin.ToUITextPosition
  else
    Result := nil;
end;

function TFMXViewBase.canBecomeFirstResponder: Boolean;
begin
  FResigned := False;
  Result := True;
end;

function TFMXViewBase.canPerformAction(action: SEL; withSender: Pointer): Boolean;
begin
  Result := FContextMenu.CanPerformAction(action);
end;

function TFMXViewBase.canResignFirstResponder: Boolean;
begin
  FResigned := True;
  Result := True;
end;

function TFMXViewBase.caretRectForPosition(position: UITextPosition): CGRect;
begin
  Result := CGRectMake(0, 0, 0, 0);
end;

function TFMXViewBase.characterRangeAtPoint(point: CGPoint): UITextRange;
begin
  Result := nil;
end;

function TFMXViewBase.closestPositionToPoint(point: CGPoint; withinRange: UITextRange): UITextPosition;
begin
  Result := nil;
end;

function TFMXViewBase.closestPositionToPoint(point: CGPoint): UITextPosition;
begin
  Result := nil;
end;

procedure TFMXViewBase.copy(Sender: id);
begin
  FContextMenu.Copy;
end;

constructor TFMXViewBase.Create(const AForm: TCommonCustomForm);
begin
  inherited Create;
  FForm := AForm;
  FContextMenu := TFMXTextEditActionsMenu.Create(View);
  FMultiTouchManager := nil;
end;

procedure TFMXViewBase.cut(Sender: id);
begin
  FContextMenu.Cut;
end;

procedure TFMXViewBase.DblTap(X, Y: Single);

  function GetTappedControl: TFmxObject;
  var
    Obj: IControl;
  begin
    Obj := FForm.ObjectAtPoint(FForm.ClientToScreen(TPointF.Create(X, Y)));
    if Obj <> nil then
      Result := Obj.GetObject
    else
      Result := FForm;
  end;

var
  EventInfo: TGestureEventInfo;
  TextInput: ITextInput;
  LGObj: IGestureControl;
begin
  FGestureControl := GetTappedControl;
  if Supports(FGestureControl, IGestureControl, LGObj) then
    FGestureControl := LGObj.GetFirstControlWithGesture(TInteractiveGesture.DoubleTap)
  else
    FGestureControl := nil;

  if FGestureControl <> nil then
  begin
    FillChar(EventInfo, Sizeof(EventInfo), 0);
    EventInfo.Location := TPointF.Create(X, Y);
    EventInfo.GestureID := igiDoubleTap;

    // send message to the control
    if Supports(FGestureControl, IGestureControl, LGObj) then
      LGObj.CMGesture(EventInfo);

    if Supports(FGestureControl, ITextInput, TextInput) then
    begin
      TextInput := nil;
      FIgnorePosition := True;
      FLastContextMenuVisibility := False;
      FClickedAnotherControl := False;
      FChangedFocusedControl := False;
      ShowContextMenu;
    end;
  end;
end;

procedure TFMXViewBase.DefineFocusControl;
var
  FocusedControl: TControl;
begin
  if (Form.Focused <> nil) and (Form.Focused.GetObject is TControl) then
    FocusedControl := Form.Focused.GetObject as TControl
  else
    FocusedControl := nil;

  if (Form.Focused <> nil) and (FContextMenu.Control <> FocusedControl) then
    FCarretPositionChanged := True;
  FChangedFocusedControl := FContextMenu.Control <> FocusedControl;
  FContextMenu.Control := FocusedControl;
end;

procedure TFMXViewBase.deleteBackward;
begin
  if (FTextService = nil) or not FTextService.DeleteBackward then
    FormKeyPress(#0, vkBack, []);
end;

destructor TFMXViewBase.Destroy;
begin
  iOSapi.Foundation.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget(Self.GetObjectID);
  FreeAndNil(FContextMenu);
  FreeAndNil(FMultiTouchManager);
  NSObject(Super).release;
  inherited Destroy;
end;

function TFMXViewBase.enablesReturnKeyAutomatically: Boolean;
begin
  Result := False;
end;

function TFMXViewBase.endOfDocument: UITextPosition;
begin
  if FTextService <> nil then
    Result := FTextService.FDocumentEnd.ToUITextPosition
  else
    Result := nil;
end;

function TFMXViewBase.firstRectForRange(range: UITextRange): CGRect;
var
  R: TRectF;
  TSObj: ITextInput;
begin
  if Form.Focused <> nil then
    if Supports(Form.Focused, ITextInput, TSObj) then
      R := TRectF.Create(TSObj.GetTargetClausePointF)
    else
      R := TControl(Form.Focused.GetObject).AbsoluteRect
  else
    R := TRectF.Empty;

  Result := CGRect.Create(R);
end;

function TFMXViewBase.gestureRecognizer(gestureRecognizer: UIGestureRecognizer; shouldReceiveTouch: UITouch): Boolean;
var
  Obj: IControl;
  GestureObj: IGestureControl;
  LControl: TComponent;
  LPoint: NSPoint;
  GestureBeingRecognized: TInteractiveGesture;
begin
  Result := False;
  LControl := nil;

  if gestureRecognizer.isKindOfClass(objc_getClass('UIPanGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.Pan
  else if gestureRecognizer.isKindOfClass(objc_getClass('UIRotationGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.Rotate
  else if gestureRecognizer.isKindOfClass(objc_getClass('UIPinchGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.Zoom
  else if gestureRecognizer.isKindOfClass(objc_getClass('UITapGestureRecognizer')) and
    (TUITapGestureRecognizer.Wrap(NSObjectToID(gestureRecognizer)).numberOfTapsRequired = 2) then
    GestureBeingRecognized := TInteractiveGesture.DoubleTap
  else if gestureRecognizer.isKindOfClass(objc_getClass('UITapGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.TwoFingerTap
  else if gestureRecognizer.isKindOfClass(objc_getClass('UILongPressGestureRecognizer')) then
    GestureBeingRecognized := TInteractiveGesture.LongTap
  else
    Exit;

  LPoint := CorrectLocationInView(shouldReceiveTouch, View, Form);

  Obj := FForm.ObjectAtPoint(FForm.ClientToScreen(LPoint.ToPointF));
  if Obj <> nil then
    LControl := Obj.GetObject
  else
    LControl := FForm;

  if Supports(LControl, IGestureControl, GestureObj) then
    LControl := GestureObj.GetFirstControlWithGesture(GestureBeingRecognized)
  else
    LControl := nil;

  if LControl <> nil then
    Result := True;
end;

function TFMXViewBase.gestureRecognizer(gestureRecognizer, shouldRecognizeSimultaneouslyWithGestureRecognizer: UIGestureRecognizer): Boolean;
begin
  // We need to avoid executing gestures from form and native control
  Result := NSObjectToID(shouldRecognizeSimultaneouslyWithGestureRecognizer.view) = NSObjectToID(gestureRecognizer.view);
end;

function TFMXViewBase.gestureRecognizerShouldBegin(gestureRecognizer: UIGestureRecognizer): Boolean;
begin
  Result := True;
end;

procedure TFMXViewBase.HandleDoubleTap(gestureRecognizer: UITapGestureRecognizer);
var
  TouchPoint: NSPoint;
begin
  if gestureRecognizer.state = UIGestureRecognizerStateEnded then
  begin
    TouchPoint := CorrectLocationInView(gestureRecognizer, View, Form);
    DblTap(TouchPoint.Y, TouchPoint.Y);
  end;
end;

procedure TFMXViewBase.HandlePan(gestureRecognizer: UIPanGestureRecognizer);
var
  LPoint, LPoint2: NSPoint;
  Distance: Integer;
  State: TInteractiveGestureFlags;
begin
  State := [];
  Distance := 0;
  case gestureRecognizer.state of
    UIGestureRecognizerStateBegan:
      begin
        State := [TInteractiveGestureFlag.gfBegin];
        LPoint := CorrectLocationInView(gestureRecognizer, View, Form);
        gestureRecognizer.setTranslation(LPoint, View);
      end;
    UIGestureRecognizerStateEnded:
      State := [TInteractiveGestureFlag.gfEnd];
    UIGestureRecognizerStateCancelled:
      State := [TInteractiveGestureFlag.gfEnd];
  end;

  if gestureRecognizer.numberOfTouches = 2 then
  begin
    LPoint := gestureRecognizer.locationOfTouch(0, View);
    LPoint2 := gestureRecognizer.locationOfTouch(1, View);
    Distance := Round(Sqrt(Sqr(LPoint.X - LPoint2.X) + Sqr(LPoint.Y - LPoint2.Y)));
  end;
  LPoint := gestureRecognizer.translationInView(View);

  MultiTouchManager.HandlePan(LPoint.ToPointF, Distance, State, gestureRecognizer.numberOfTouches);
end;

procedure TFMXViewBase.HandleRotate(gestureRecognizer: UIRotationGestureRecognizer);
var
  State: TInteractiveGestureFlags;
  LPoint: NSPoint;
begin
  LPoint := CorrectLocationInView(gestureRecognizer, View, Form);

  State := [];
  case gestureRecognizer.state of
    UIGestureRecognizerStateBegan:
      State := [TInteractiveGestureFlag.gfBegin];
    UIGestureRecognizerStateEnded:
      State := [TInteractiveGestureFlag.gfEnd];
    UIGestureRecognizerStateCancelled:
      State := [TInteractiveGestureFlag.gfEnd];
  end;
  
  MultiTouchManager.HandleRotate(LPoint.ToPointF, -gestureRecognizer.Rotation, State, gestureRecognizer.numberOfTouches);
end;

procedure TFMXViewBase.HandleTwoFingerTap(gestureRecognizer: UITapGestureRecognizer);
var
  LPoint: NSPoint;
  State: TInteractiveGestureFlags;
begin
  LPoint := CorrectLocationInView(gestureRecognizer, View, Form);

  State := [];
  case gestureRecognizer.state of
    UIGestureRecognizerStateBegan:
      State := [TInteractiveGestureFlag.gfBegin];
    UIGestureRecognizerStateEnded:
      State := [TInteractiveGestureFlag.gfEnd];
    UIGestureRecognizerStateCancelled:
      State := [TInteractiveGestureFlag.gfEnd];
  end;

  MultiTouchManager.HandleTwoFingerTap(LPoint.ToPointF, State, gestureRecognizer.numberOfTouches);
end;

procedure TFMXViewBase.HandleZoom(gestureRecognizer: UIPinchGestureRecognizer);
var
  LPoint, LPoint2: NSPoint;
  State: TInteractivegestureFlags;
  Distance: Integer;
begin
  State := [];
  Distance := 0;
  case gestureRecognizer.state of
    UIGestureRecognizerStateBegan:
      State := [TInteractiveGestureFlag.gfBegin];
    UIGestureRecognizerStateEnded:
      State := [TInteractiveGestureFlag.gfEnd];
    UIGestureRecognizerStateCancelled:
      State := [TInteractiveGestureFlag.gfEnd];
  end;

  if gestureRecognizer.numberOfTouches = 2 then
  begin
    LPoint := gestureRecognizer.locationOfTouch(0, View);
    LPoint2 := gestureRecognizer.locationOfTouch(1, View);
    Distance := Round(Sqrt(Sqr(LPoint.X - LPoint2.X) + Sqr(LPoint.Y - LPoint2.Y)));
  end;
  LPoint := CorrectLocationInView(gestureRecognizer, View, Form);

  MultiTouchManager.HandleZoom(LPoint.ToPointF, Distance, State, gestureRecognizer.numberOfTouches);
end;

function TFMXViewBase.hasText: Boolean;
begin
  Result := (FTextService <> nil) and not FTextService.Text.IsEmpty;
end;

procedure TFMXViewBase.HideContextMenu;
begin
  if FContextMenu.ShowSpellItems then
    FContextMenu.HideHighlightSpell;
  FContextMenu.Hide;
end;

function TFMXViewBase.inputDelegate: Pointer;
begin
  Result := nil;
end;

procedure TFMXViewBase.insertDictationResult(dictationResult: NSArray);
var
  I: Integer;
begin
  for I := 0 to dictationResult.count - 1 do
    insertText(TUIDictationPhrase.Wrap(dictationResult.objectAtIndex(I)).text);
end;

function TFMXViewBase.insertDictationResultPlaceholder: Pointer;
begin
  Result := nil;
end;

function TFMXViewBase.frameForDictationResultPlaceholder(placeholder: Pointer): CGRect;
begin
end;

procedure TFMXViewBase.removeDictationResultPlaceholder(placeholder: Pointer; willInsertResult: Boolean);
begin
end;

procedure TFMXViewBase.insertText(text: NSString);
var
  I: Integer;
  Ch: Char;
  Str: string;
  Key: Word;
begin
  if FTextService <> nil then
    FTextService.InsertText(NSStrToStr(text));

  if text.length > 0 then
  begin
    Str := NSStrToStr(text);
    for I := 0 to Str.Length - 1 do
    begin
      Ch := Str.Chars[I];
      if Ch = #10 then
      begin
        if not FTextService.Multiline and (FReturnKeyType = TReturnKeyType.Next) then
          Key := vkTab
        else
          Key := vkReturn;
        FormKeyPress(#0, Key, [])
      end
      else
        FormKeyPress(Ch, 0, []);
    end;
  end;
end;

function TFMXViewBase.isAccessibilityElement: Boolean;
begin
  Result := False;
end;

function TFMXViewBase.isFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXViewBase.isMultipleTouchEnabled: Boolean;
begin
  Result := True;
end;

function TFMXViewBase.isSecureTextEntry: Boolean;
begin
  Result := FPassword;
end;

function TFMXViewBase.keyboardAppearance: UIKeyboardAppearance;
begin
  Result := UIKeyboardAppearanceDefault;
end;

function TFMXViewBase.keyboardType: UIKeyboardType;
begin
  case FKeyboardType of
    TVirtualKeyboardType.Default:               Result := UIKeyboardTypeDefault;
    TVirtualKeyboardType.NumbersAndPunctuation: Result := UIKeyboardTypeNumbersAndPunctuation;
    TVirtualKeyboardType.NumberPad:             Result := UIKeyboardTypeNumberPad;
    TVirtualKeyboardType.PhonePad:              Result := UIKeyboardTypePhonePad;
    TVirtualKeyboardType.Alphabet:              Result := UIKeyboardTypeDefault;
    TVirtualKeyboardType.URL:                   Result := UIKeyboardTypeURL;
    TVirtualKeyboardType.NamePhonePad:          Result := UIKeyboardTypeNamePhonePad;
    TVirtualKeyboardType.EmailAddress:          Result := UIKeyboardTypeEmailAddress;
    TVirtualKeyboardType.DecimalNumberPad:      Result := UIKeyboardTypeDecimalPad;
  else
    Result := UIKeyboardTypeDefault;
  end;
end;

procedure TFMXViewBase.FormKeyPress(Ch: Char; Key: Word; Shift: TShiftState);
var
  LCh: Char;
  LKey: Word;
begin
  LCh := Ch;
  LKey := Key;
  try
    Form.KeyDown(LKey, LCh, Shift);
  except
    Application.HandleException(Form);
  end;

  LCh := Ch;
  LKey := Key;
  try
    Form.KeyUp(LKey, LCh, Shift);
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.LongTap(gestureRecognizer: UILongPressGestureRecognizer);
var
  TouchPoint: NSPoint;
  EventInfo: TGestureEventInfo;
  Handled: Boolean;
  Obj: IControl;
  GestureObj: IGestureControl;
begin
  TouchPoint := CorrectLocationInView(gestureRecognizer, View, Form);

  if gestureRecognizer.state = UIGestureRecognizerStateBegan then
  begin
    // Get the control from "under" the gesture.
    Obj := Form.ObjectAtPoint(FForm.ClientToScreen(TouchPoint.ToPointF));
    if Obj <> nil then
      FGestureControl := Obj.GetObject
    else
      FGestureControl := Form;

    if Supports(FGestureControl, IGestureControl, GestureObj) then
      FGestureControl := GestureObj.GetFirstControlWithGesture(TInteractiveGesture.LongTap);
  end;

  if FGestureControl <> nil then
  begin
    Handled := True;
    FillChar(EventInfo, Sizeof(EventInfo), 0);
    EventInfo.Location := TouchPoint.ToPointF;
    EventInfo.GestureID := igiLongTap;
    // set flags
    if gestureRecognizer.state = UIGestureRecognizerStateBegan then
      EventInfo.Flags := [TInteractiveGestureFlag.gfBegin]
    else if ((gestureRecognizer.state = UIGestureRecognizerStateEnded) or (gestureRecognizer.state = UIGestureRecognizerStateCancelled)) then
      EventInfo.Flags := [TInteractiveGestureFlag.gfEnd];

    HideContextMenu;
    DoLMouseDown(TouchPoint.x, TouchPoint.y);
    DefineFocusControl;
    // send message to the control
    if Supports(FGestureControl, IGestureControl, GestureObj) then
        GestureObj.CMGesture(EventInfo);

    if gestureRecognizer.state = UIGestureRecognizerStateChanged then
      Form.MouseMove([ssTouch], TouchPoint.X, TouchPoint.Y);

    if gestureRecognizer.state = UIGestureRecognizerStateEnded then
    begin
      FContextMenu.ShowSpellItems := False;
      FContextMenu.Show;
    end;

    if ((gestureRecognizer.state = UIGestureRecognizerStateEnded) or (gestureRecognizer.state = UIGestureRecognizerStateCancelled)) then
    begin
      FGestureControl := nil;
      if Handled then
        DoLMouseUp(TouchPoint.X, TouchPoint.Y, False)
      else
        DoLMouseUp(TouchPoint.X, TouchPoint.Y);
    end;
  end;
end;

function TFMXViewBase.markedTextRange: UITextRange;
begin
  if (FTextService <> nil) and FTextService.HasMarkedText then
    Result := FTextService.FMarkedTextRange.ToUITextRange
  else
    Result := nil;
end;

function TFMXViewBase.markedTextStyle: NSDictionary;
begin
  Result := nil;
end;

procedure TFMXViewBase.paste(Sender: id);
begin
  try
    FContextMenu.Paste;
  except
    Application.HandleException(Self);
  end;
end;

function TFMXViewBase.textInRange(range: UITextRange): NSString;
var
  FMXRange: TFMXTextRange;
  SPos, EPos: Integer;
begin
  FMXRange := TFMXTextRange.FromUITextRange(range);
  if (FMXRange <> nil) and (FTextService <> nil) then
  begin
    SPos := Min(FMXRange.RangeStart.Position, FMXRange.RangeEnd.Position);
    EPos := Max(FMXRange.RangeStart.Position, FMXRange.RangeEnd.Position);
    Result := StrToNSStr(FTextService.Text.Substring(SPos, EPos - SPos + 1));
  end
  else
    Result := nil;
end;

procedure TFMXViewBase.replaceRange(range: UITextRange; withText: NSString);
begin			 
  unmarkText;
end;

function TFMXViewBase.positionFromPosition(position: UITextPosition;
  offset: NSInteger): UITextPosition;
var
  FMXPosition: TFMXTextPosition;
  EndPosition: Integer;
begin
  FMXPosition := TFMXTextPosition.FromUITextPosition(position);

  if FMXPosition <> nil then
  begin
    EndPosition := FMXPosition.Position + offset;

    if (FTextService = nil) or (EndPosition > FTextService.Text.Length) or (EndPosition < 0) then
      Result := nil
    else
      Result := TFMXTextPosition.Create(EndPosition, FMXPosition.Line).ToUITextPosition;
  end
  else
    Result := position;
end;

function TFMXViewBase.positionFromPosition(position: UITextPosition;
  inDirection: UITextLayoutDirection; offset: NSInteger): UITextPosition;
var
  FMXPosition: TFMXTextPosition;
begin
  FMXPosition := TFMXTextPosition.FromUITextPosition(position);

  if FMXPosition <> nil then
  begin
    case inDirection of
      UITextLayoutDirectionRight:
        Result := TFMXTextPosition.Create(FMXPosition.Position + offset, FMXPosition.Line).ToUITextPosition;
      UITextLayoutDirectionLeft:
        Result := TFMXTextPosition.Create(FMXPosition.Position - offset, FMXPosition.Line).ToUITextPosition;
      UITextLayoutDirectionUp:
        Result := TFMXTextPosition.Create(FMXPosition.Position, FMXPosition.Line - offset).ToUITextPosition;
      UITextLayoutDirectionDown:
        Result := TFMXTextPosition.Create(FMXPosition.Position, FMXPosition.Line + offset).ToUITextPosition;
    end;
  end
  else
    Result := position;
end;

function TFMXViewBase.textRangeFromPosition(fromPosition, toPosition: UITextPosition): UITextRange;
var
  FMXPosition1, FMXPosition2: TFMXTextPosition;
begin
  FMXPosition1 := TFMXTextPosition.FromUITextPosition(fromPosition);
  FMXPosition2 := TFMXTextPosition.FromUITextPosition(toPosition);

  if (FMXPosition1 <> nil) and (FMXPosition2 <> nil) then
  begin
    Result := TFMXTextRange.Create(FMXPosition1.Position, FMXPosition1.Line,
      FMXPosition2.Position, FMXPosition2.Line).ToUITextRange;
    if not FMXPosition1.FHardLink then
    begin
      FMXPosition1._Release;
      fromPosition.release;
    end;
    if not FMXPosition2.FHardLink then
    begin
      FMXPosition2._Release;
      toPosition.release;
    end;
  end
  else
    Result := nil;
end;

function TFMXViewBase.comparePosition(position, toPosition: UITextPosition): NSComparisonResult;
var
  FMXPosition1, FMXPosition2: TFMXTextPosition;
begin
  FMXPosition1 := TFMXTextPosition.FromUITextPosition(position);
  FMXPosition2 := TFMXTextPosition.FromUITextPosition(toPosition);
  if (FMXPosition1 <> nil) and (FMXPosition2 <> nil) then
  begin
    if FMXPosition1 = FMXPosition2 then
      Result := NSOrderedSame
    else if FMXPosition1 > FMXPosition2 then
      Result := NSOrderedDescending
    else
      Result := NSOrderedAscending
  end
  else
    Result := NSOrderedSame;
end;

function TFMXViewBase.offsetFromPosition(from, toPosition: UITextPosition): NSInteger;
var
  FMXPosition1, FMXPosition2: TFMXTextPosition;
begin
  FMXPosition1 := TFMXTextPosition.FromUITextPosition(from);
  FMXPosition2 := TFMXTextPosition.FromUITextPosition(toPosition);

  if (FMXPosition1 <> nil) and (FMXPosition2 <> nil) then
    Result := FMXPosition2.Position - FMXPosition1.Position
  else
    Result := 0;
end;

function TFMXViewBase.positionWithinRange(range: UITextRange; farthestInDirection: UITextLayoutDirection): UITextPosition;
var
  FMXRange: TFMXTextRange;
begin
  FMXRange := TFMXTextRange.FromUITextRange(range);

  if FMXRange <> nil then
  begin
    case farthestInDirection of
      UITextLayoutDirectionLeft, UITextLayoutDirectionUp:
        Result := TFMXTextPosition.Create(FMXRange.RangeStart.Position, FMXRange.RangeStart.Line).ToUITextPosition;
      UITextLayoutDirectionDown, UITextLayoutDirectionRight:
        Result := TFMXTextPosition.Create(FMXRange.RangeEnd.Position, FMXRange.RangeEnd.Line).ToUITextPosition;
    end;
  end
  else
    Result := nil;
end;

function TFMXViewBase.characterRangeByExtendingPosition(position: UITextPosition; inDirection: UITextLayoutDirection): UITextRange;
var
  FMXPosition: TFMXTextPosition;
begin
  FMXPosition := TFMXTextPosition.FromUITextPosition(position);

  if FMXPosition <> nil then
  begin
    case inDirection of
      UITextLayoutDirectionLeft, UITextLayoutDirectionUp:
        Result := TFMXTextRange.Create(FMXPosition.Position - 1, FMXPosition.Line, FMXPosition.Position,
          FMXPosition.Line).ToUITextRange;
      UITextLayoutDirectionDown, UITextLayoutDirectionRight:
        Result := TFMXTextRange.Create(FMXPosition.Position, FMXPosition.Line, FMXPosition.Position + 1,
          FMXPosition.Line).ToUITextRange;
    end;
  end
  else
    Result := nil;
end;

function TFMXViewBase.returnKeyType: UIReturnKeyType;
begin
  case FReturnKeyType of
    TReturnKeyType.Default:
      Result := UIReturnKeyDefault;
    TReturnKeyType.Done:
      Result := UIReturnKeyDone;
    TReturnKeyType.Go:
      Result := UIReturnKeyGo;
    TReturnKeyType.Next:
      Result := UIReturnKeyNext;
    TReturnKeyType.Search:
      Result := UIReturnKeySearch;
    TReturnKeyType.Send:
      Result := UIReturnKeySend;
  else
    Result := UIReturnKeyDefault;
  end;
end;

procedure TFMXViewBase.select(Sender: id);
begin
  FContextMenu.Select;
end;

procedure TFMXViewBase.selectAll(Sender: id);
begin
  FContextMenu.SelectAll;
end;

function TFMXViewBase.selectedTextRange: UITextRange;
begin
  if FTextService <> nil then
    Result := FTextService.FSelectedTextRange.ToUITextRange
  else
    Result := nil;
end;

procedure TFMXViewBase.SendTouches(const ATouches: NSSet; Action: TTouchAction; const Control: IControl);
var
  Touch: UITouch;
  TouchesArray: NSArray;
  FMXTouches: TTouches;
  FMXTouch: TTouch;
  Point: CGPoint;
  I: Integer;
begin
  if (ATouches <> nil) and (ATouches.Count > 0) then
  begin
    TouchesArray := ATouches.allObjects;
    SetLength(FMXTouches, TouchesArray.Count);
    for I := 0 to TouchesArray.Count - 1 do
    begin
      Touch := TUITouch.Wrap(TouchesArray.objectAtIndex(I));
      Point := CorrectLocationInView(Touch, View, Form);
      FMXTouch.Location.X := Point.x;
      FMXTouch.Location.Y := Point.y;
      FMXTouches[I] := FMXTouch;

      if ((Action = TTouchAction.Move) and ((Touch.phase = UITouchPhaseBegan) or
        (TouchesArray.Count > FNoOfTouches))) then
        Action := TTouchAction.Down;
    end;
    FNoOfTouches := TouchesArray.Count;
  end;

  MultiTouchManager.HandleTouches(FMXTouches, Action, Control);
end;

procedure TFMXViewBase.setAutocapitalizationType(autocapitalizationType: UITextAutocapitalizationType);
begin
end;

procedure TFMXViewBase.setAutocorrectionType(autocorrectionType: UITextAutocorrectionType);
begin
end;

procedure TFMXViewBase.setBaseWritingDirection(writingDirection: UITextWritingDirection; forRange: UITextRange);
begin
end;

procedure TFMXViewBase.setEnablesReturnKeyAutomatically(enablesReturnKeyAutomatically: Boolean);
begin
end;

procedure TFMXViewBase.setInputDelegate(inputDelegate: UITextInputDelegate);
begin
  FInputDelegate := UITextInputDelegate(inputDelegate);
end;

procedure TFMXViewBase.setKeyboardAppearance(keyboardAppearance: UIKeyboardAppearance);
begin
end;

procedure TFMXViewBase.setKeyboardType(keyboardType: UIKeyboardType);
begin
end;

procedure TFMXViewBase.setMarkedText(markedText: NSString;  selectedRange: NSRange);
begin
  if (FTextService <> nil) and not FResigned then
    FTextService.SetMarkedText(NSStrToStr(markedText));
end;

procedure TFMXViewBase.setMarkedTextStyle(markedTextStyle: NSDictionary);
begin
end;

procedure TFMXViewBase.setReturnKeyType(returnKeyType: UIReturnKeyType);
begin
end;

procedure TFMXViewBase.setSecureTextEntry(secureTextEntry: Boolean);
begin
end;

procedure TFMXViewBase.setSelectedTextRange(selectedTextRange: UITextRange);
var
  FMXRange: TFMXTextRange;
  Key: Word;
  Shift: TShiftState;
begin
  FMXRange := TFMXTextRange.FromUITextRange(selectedTextRange);

  if (FMXRange <> nil) and (FTextService <> nil) then
  begin
    Key := 0;
    Shift := [];
    if FMXRange.RangeStart <> FMXRange.RangeEnd then
      Include(Shift, ssShift);
    if FMXRange.RangeStart.Line = FTextService.CaretPosition.Y then
    begin
      if FTextService.FSelectedTextRange.RangeEnd.Position > FMXRange.RangeEnd.Position then
      begin
        Key := vkLeft;
        if (FMXRange.RangeEnd.Position < (FTextService.FSelectedTextRange.RangeEnd.Position - 1)) then
          Include(Shift, ssCommand);
      end
      else if FTextService.FSelectedTextRange.RangeEnd.Position < FMXRange.RangeEnd.Position then
      begin
        Key := vkRight;
        if (FMXRange.RangeEnd.Position > (FTextService.FSelectedTextRange.RangeEnd.Position + 1)) then
          Include(Shift, ssCommand);
      end;
    end
    else if FMXRange.RangeEnd.Line < FTextService.FSelectedTextRange.RangeEnd.Line then
      Key := vkUp
    else if FMXRange.RangeEnd.Line > FTextService.FSelectedTextRange.RangeEnd.Line then
      Key := vkDown;
    if Key > 0 then
      FormKeyPress(#0, Key, Shift);

    if not FMXRange.FHardLink then
    begin
      FMXRange._Release;
      selectedTextRange.release;
    end;
  end;
end;

procedure TFMXViewBase.setSpellCheckingType(spellCheckingType: NSInteger);
begin
end;

procedure TFMXViewBase.ShowContextMenu;

  procedure DefineSelectionStates;
  var
    TextInput: ITextInput;
  begin
    if not FIgnorePosition and FContextMenu.HasControl and (Form <> nil) and (Form.Focused <> nil) and 
      Form.Focused.GetObject.GetInterface(ITextInput, TextInput) then
      FCarretPositionChanged := TextInput.GetTextService.CaretPosition <> FLastCaretPosition
    else
      FCarretPositionChanged := False;
    FIgnorePosition := False;
  end;

begin
  DefineSelectionStates;
  if not FCarretPositionChanged and not FLastContextMenuVisibility and
     not FClickedAnotherControl and not FChangedFocusedControl then
  begin
    if FContextMenu.ShowSpellItems then
      FContextMenu.HighlightSpell
    else
      FContextMenu.HideHighlightSpell;
    FContextMenu.Show;
  end
  else
    FContextMenu.HideHighlightSpell;
end;

procedure TFMXViewBase.SingleTap(Sender: id);
var
  SpellControl: ITextSpellCheck;
begin
  if Form <> nil then
  begin
    FContextMenu.ShowSpellItems := (Form.Focused <> nil) and
      Supports(Form.Focused, ITextSpellCheck, SpellControl) and
      SpellControl.IsSpellCheckEnabled and SpellControl.IsCurrentWordWrong;
    if FContextMenu.ShowSpellItems then
      FContextMenu.SetSpellItems(SpellControl.GetListOfPrepositions)
    else
      FContextMenu.SetSpellItems(nil);
    ShowContextMenu;
  end;
end;

procedure TFMXViewBase.spell1(Sender: id);
begin
  FContextMenu.Spell1;
end;

procedure TFMXViewBase.spell2(Sender: id);
begin
  FContextMenu.Spell2;
end;

procedure TFMXViewBase.spell3(Sender: id);
begin
  FContextMenu.Spell3;
end;

function TFMXViewBase.spellCheckingType: NSInteger;
begin
  Result := 0;
end;

function TFMXViewBase.tokenizer: Pointer;
begin
  Result := TUITextInputStringTokenizer.Alloc.initWithTextInput(View);
  objc_msgSend(Result, sel_registerName('autorelease'));
end;

function TFMXViewBase.GetMultiTouchManager: TMultiTouchManagerIOS;
begin
  if FMultiTouchManager = nil then
    FMultiTouchManager := TMultiTouchManagerIOS.Create(Form);
  Result := FMultiTouchManager;
end;

function TFMXViewBase.GetTouchCoord(const touches: NSSet; const Window: UIView; var x, y: single): Boolean;
var
  touch : UITouch;
  p     : CGPoint;
begin
  Result := False;
  if (touches <> nil) and (touches.count >= 1) then
  begin
    touch := TUITouch.Wrap(touches.anyObject);
    p := CorrectLocationInView(touch, Window, Form);
    x := p.x;
    y := p.y;
    PlatformCocoaTouch.FMouseCoord.X := X;
    PlatformCocoaTouch.FMouseCoord.Y := Y;
    PlatformCocoaTouch.FMouseCoord := Form.ClientToScreen(PlatformCocoaTouch.FMouseCoord);
    Result := True;
  end;
end;

function TFMXViewBase.GetView: UIView;
begin
  Result := UIView(Super);
end;

procedure TFMXViewBase.PrepareClosePopups(const SaveForm: TCommonCustomForm);
begin
  if Screen <> nil then
    if SaveForm <> nil then
      Screen.PrepareClosePopups(SaveForm)
    else
      Screen.PrepareClosePopups(nil);
end;

procedure TFMXViewBase.ClosePopups;
begin
  if Screen <> nil then
    Screen.ClosePopupForms;
end;

procedure TFMXViewBase.DoLMouseDown(const X, Y: Single);
begin
  if not FDown then
  try
    FDown := True;
    FTap := True;
    PrepareClosePopups(Form);
    if Form <> nil then
    begin
      Form.MouseMove([ssTouch], X, Y);
      Form.MouseMove([], X, Y); // Require for correct IsMouseOver handle
      Form.MouseDown(TMouseButton.mbLeft, [ssLeft, ssTouch], X, Y);
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.DoLMouseMove(const X, Y: Single);
begin
  if FDown then
  try
    if Form <> nil then
      Form.MouseMove([ssLeft, ssTouch], X, Y);
  except
    Application.HandleException(Form);
  end;
  FTap := False;
end;

procedure TFMXViewBase.DoLMouseUp(const X, Y: Single; DoClick: Boolean = True);
begin
  if FDown then
  try
    FDown := False;
    if Form <> nil then
      Form.MouseUp(TMouseButton.mbLeft, [ssLeft, ssTouch], X, Y, DoClick);
    if Form <> nil then
      Form.MouseLeave;
    ClosePopups;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.touchesBegan(touches: NSSet; withEvent: UIEvent);

  procedure ResetSelectionStates;
  begin
    FLastContextMenuVisibility := FContextMenu.IsVisible;
    FCarretPositionChanged := False;
  end;

var
  X, Y : single;
  TextInput: ITextInput;
  Obj: IControl;
  Touch: UITouch;
begin
  try
    if not GetTouchCoord(touches, View, X, Y) then
      Exit;
    // find the control from under the gesture
    Obj := Form.ObjectAtPoint(Form.ClientToScreen(TPointF.Create(X, Y)));
    SendTouches(withEvent.allTouches, TTouchAction.Down, Obj);

    ResetSelectionStates;

    // Hide old context menu and set new focused control
    if FContextMenu.IsVisible then
      FContextMenu.Hide;

    // Save caret position for define selection change before mouse down
    if FContextMenu.HasControl and FContextMenu.Control.GetInterface(ITextInput, TextInput) then
      FLastCaretPosition := TextInput.GetTextService.CaretPosition;
    try
      DoLMouseDown(X, Y);
      Touch := TUITouch.Wrap(touches.anyObject);
      if Touch.tapCount = 2 then
        iOSapi.Foundation.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget(Self.GetObjectID);
      DefineFocusControl;
    finally
      FClickedAnotherControl := (Obj <> nil) and (FContextMenu.Control <> Obj.GetObject);
      View.touchesBegan(touches, withEvent);
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.touchesCancelled(touches: NSSet; withEvent: UIEvent);

  procedure DefineSelectionStates;
  var
    TextInput: ITextInput;
  begin
    if FContextMenu.HasControl and (Form.Focused <> nil) and (Form.Focused.GetObject.GetInterface(ITextInput, TextInput)) then
      FCarretPositionChanged := TextInput.GetTextService.CaretPosition <> FLastCaretPosition
    else
      FCarretPositionChanged := False;
  end;

var
  X, Y : single;
  Obj: IControl;
  LPoint: TPointF;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
begin
  try
    if not GetTouchCoord(touches, View, X, Y) then
      Exit;
    LPoint := TPointF.Create(X, Y);

    Obj := Form.ObjectAtPoint(Form.ClientToScreen(LPoint));
    SendTouches(withEvent.allTouches, TTouchAction.Up, Obj);

    try
      DoLMouseUp(X, Y, False);
      DefineSelectionStates;
    finally
      View.touchesCancelled(touches, withEvent);
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.touchesEnded(touches: NSSet; withEvent: UIEvent);

  procedure DefineSelectionStates;
  var
    TextInput: ITextInput;
  begin
     if (FContextMenu <> nil) and FContextMenu.HasControl and (Form <> nil) and
      (Form.Focused <> nil) and (Form.Focused.GetObject.GetInterface(ITextInput, TextInput)) then
      FCarretPositionChanged := TextInput.GetTextService.CaretPosition <> FLastCaretPosition
    else
      FCarretPositionChanged := False;
  end;

var
  X, Y : single;
  Touch: UITouch;
  Obj: IControl;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
begin
  try
    if not GetTouchCoord(touches, View, X, Y) then
      Exit;

    // find the control from under the gesture
    Obj := Form.ObjectAtPoint(Form.ClientToScreen(TPointF.Create(X, Y)));
    SendTouches(withEvent.allTouches, TTouchAction.Up, Obj);

    try
      DoLMouseUp(X, Y);
      Touch := TUITouch.Wrap(touches.anyObject);
      if FTap and (Touch.tapCount = 1) and (Form <> nil) then
        NSObject(Self.Super).performSelector(sel_getUid('SingleTap:'), nil, DblTapDelay);
      DefineSelectionStates;
    finally
      if Form <> nil then
        View.touchesEnded(touches, withEvent);
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.touchesMoved(touches: NSSet; withEvent: UIEvent);
var
  X, Y : single;
  Obj: IControl;
  LPoint: TPointF;
begin
  try
    if not GetTouchCoord(touches, View, X, Y) then
      Exit;

    LPoint := TPointF.Create(X, Y);
    Obj := Form.ObjectAtPoint(Form.ClientToScreen(LPoint));
    SendTouches(withEvent.allTouches, TTouchAction.Move, Obj);

    try
      DoLMouseMove(X, Y);
    finally
      View.touchesMoved(touches, withEvent);
    end;
  except
    Application.HandleException(Form);
  end;
end;

procedure TFMXViewBase.unmarkText;
var
  I: Integer;
  Ch: Char;
  LMarkedText: string;
  Key: Word;
begin
  if (FTextService <> nil) and FTextService.HasMarkedText then
  begin
    LMarkedText := FTextService.InternalGetMarkedText;
    FTextService.InternalBreakIMEInput;
    for I := 0 to LMarkedText.Length - 1 do
    begin
      Ch := LMarkedText.Chars[I];
      if Ch = #10 then
      begin
        if not FTextService.Multiline and (FReturnKeyType = TReturnKeyType.Next) then
          Key := vkTab
        else
          Key := vkReturn;
        FormKeyPress(#0, Key, [])
      end 
      else
        FormKeyPress(Ch, 0, []);
    end;
    FTextService.InternalEndIMEInput;
  end;
end;

{ TFMXView3D }

var
  GLKitMod: HMODULE;

constructor TFMXView3D.Create(const AOwner: TCommonCustomForm; AFrameRect: NSRect);

  function GetDefaultMultisamples: Integer;
  const
    HighQualitySamples = 4;
  begin
    if (AOwner is TCustomForm) and (TCustomForm(AOwner).Quality = TCanvasQuality.HighQuality) then
      Result := HighQualitySamples
    else if AOwner is TCustomForm3D then
      Result := MultisampleTypeToNumber(TCustomForm3D(AOwner).Multisample)
    else if (Application.MainForm is TCustomForm) and
      (TCustomForm(Application.MainForm).Quality = TCanvasQuality.HighQuality) then
      Result := HighQualitySamples
    else if Application.MainForm is TCustomForm3D then
      Result := MultisampleTypeToNumber(TCustomForm3D(Application.MainForm).Multisample)
    else
      Result := 0;
  end;

var
  V: Pointer;
  RenderingSetupService: IFMXRenderingSetupService;
  ColorBits, DepthBits, Multisamples: Integer;
  Stencil: Boolean;
begin
  GLKitMod := LoadLibrary(PWideChar(libGLKit));
  inherited Create(AOwner);
  V := GLKView(Super).initWithFrame(AFrameRect, TCustomContextIOS.SharedContext);
  GLKView(Super).setContentScaleFactor(MainScreen.scale);
  GLKView(Super).setOpaque(not FForm.Transparency);

  // Default rendering configuration.
  ColorBits := 24;
  DepthBits := 24;
  Stencil := True;
  Multisamples := GetDefaultMultisamples;

  // Request adjustment of rendering configuration.
  if TPlatformServices.Current.SupportsPlatformService(IFMXRenderingSetupService, RenderingSetupService) then
    RenderingSetupService.Invoke(ColorBits, DepthBits, Stencil, Multisamples);

  // Color Bitdepth.
  if ColorBits <= 16 then
    GLKView(Super).setDrawableColorFormat(GLKViewDrawableColorFormatRGB565);

  // Depth Buffer.
  if DepthBits > 0 then
  begin
    if DepthBits > 16 then
      GLKView(Super).setDrawableDepthFormat(GLKViewDrawableDepthFormat24)
    else
      GLKView(Super).setDrawableDepthFormat(GLKViewDrawableDepthFormat16);
  end
  else
    GLKView(Super).setDrawableDepthFormat(GLKViewDrawableDepthFormatNone);

  // Stencil Buffer.
  if Stencil then
    GLKView(Super).setDrawableStencilFormat(GLKViewDrawableStencilFormat8)
  else
    GLKView(Super).setDrawableStencilFormat(GLKViewDrawableStencilFormatNone);

  // Multisamples.
  if Multisamples > 0 then
    GLKView(Super).setDrawableMultisample(GLKViewDrawableMultisample4X)
  else
    GLKView(Super).setDrawableMultisample(GLKViewDrawableMultisampleNone);

  if V <> GetObjectID then
    UpdateObjectID(V);
end;

function TFMXView3D.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXView3D);
end;

constructor TFMXView3D.Create(const AOwner: TCommonCustomForm);
begin
  Create(AOwner, CGRectMake(0, 0, 0, 0));
end;

procedure TFMXView3D.drawRect(R: CGRect);
var
  PaintControl: IPaintControl;
  i: integer;
begin

  //process all the pending input event
  //NOTE: when the mouse is down, MouseMove is continuously firing
  for i := ALAniCalcTimerProcs.Count - 1 downto 0 do
    if (ALAniCalcTimerProcs[i].Down) and
       (not ALAniCalcTimerProcs[i].mouseEventReceived) then exit;

  if Supports(Form, IPaintControl, PaintControl) then
    try
      PaintControl.PaintRects([Form.ClientRect]);
    except
      Application.HandleException(Form);
    end;
end;

{ TFMXEditActionsMenu }

constructor TFMXEditActionsMenu.Create(const AParentView: UIView);
begin
  Assert(AParentView <> nil, 'Context menu must have parent UIView');
  FParentView := AParentView;
  FMenuController := TUIMenuController.Wrap(TUIMenuController.OCClass.sharedMenuController);
end;

function TFMXEditActionsMenu.DefineActionType(const AAction: SEL): TStandardActionType;
begin
  if AAction = sel_getUid('select:') then
    Result := TStandardActionType.Select
  else if AAction = sel_getUid('selectAll:') then
    Result := TStandardActionType.SelectAll
  else if AAction = sel_getUid('copy:') then
    Result := TStandardActionType.Copy
  else if AAction = sel_getUid('paste:') then
    Result := TStandardActionType.Paste
  else if AAction = sel_getUid('cut:') then
    Result := TStandardActionType.Cut
  else if AAction = sel_getUid('PromptForReplace:') then
    Result := TStandardActionType.PromptForReplace
  else if AAction = sel_getUid('replace:') then
    Result := TStandardActionType.Replace
  else if AAction = sel_getUid('spell1:') then
    Result := TStandardActionType.Spell1
  else if AAction = sel_getUid('spell2:') then
    Result := TStandardActionType.Spell2
  else if AAction = sel_getUid('spell3:') then
    Result := TStandardActionType.Spell3
  else
    Result := TStandardActionType.Unknown;
end;

destructor TFMXEditActionsMenu.Destroy;
begin
  if FControl <> nil then
    FControl.RemoveFreeNotify(Self);
  FParentView := nil;
  FMenuController := nil;
  inherited Destroy;
end;

procedure TFMXEditActionsMenu.DoControlChanged;
begin
  // Nothing
end;

procedure TFMXEditActionsMenu.DoDefineSelectionFrame(var Frame: CGRect);
begin

end;

function TFMXEditActionsMenu.HasControl: Boolean;
begin
  Result := Control <> nil;
end;

procedure TFMXEditActionsMenu.Hide;
begin
  if IsVisible then
    FMenuController.setMenuVisible(False{, True});
end;

function TFMXEditActionsMenu.IsVisible: Boolean;
begin
  Result := FMenuController.isMenuVisible;
end;

procedure TFMXEditActionsMenu.FreeNotification(AObject: TObject);
begin
  if AObject = Control then
    Control := nil;
end;

procedure TFMXEditActionsMenu.SetControl(const AControl: TControl);
begin
  if FControl <> AControl then
  begin
    if FControl <> nil then
      FControl.RemoveFreeNotify(Self);
    FControl := AControl;
    if FControl <> nil then
      FControl.AddFreeNotify(Self);
    DoControlChanged;
    Hide;
  end;
end;

procedure TFMXEditActionsMenu.Show;
var
  AbsolutePos: TPointF;
  ControlFrame: CGRect;
begin
  FReplaceMenu := False;
  if not HasControl then
    Exit;
  // Define default control frame
  AbsolutePos := FControl.LocalToAbsolute(TPointF.Zero);
  ControlFrame := CGRectMake(AbsolutePos.X, AbsolutePos.Y, FControl.Width, FControl.Height);
  // Define user control frame
  DoDefineSelectionFrame(ControlFrame);
  // Show menu
  FMenuController.setTargetRect(ControlFrame, FParentView);
  FMenuController.update;
  FMenuController.setMenuVisible(True, True);
end;

{ TFMXTextEditActionsMenu }

function TFMXTextEditActionsMenu.CanPerformAction(const AAction: SEL): Boolean;
var
  ClipboardService: IFMXClipboardService;
begin
  ClipboardService := GetClipboardService;
  try
    Result := (FTextInput <> nil) and (ClipboardService <> nil);
    if Result then
      case DefineActionType(AAction) of
        TStandardActionType.Cut:
          Result := not FShowSpellItems and not FTextInput.GetSelection.IsEmpty and not FReplaceMenu and
            ((FVirtualKeyboard = nil) or not FVirtualKeyboard.IsPassword);
        TStandardActionType.Copy:
          Result := not FShowSpellItems and not FTextInput.GetSelection.IsEmpty and not FReplaceMenu and
            ((FVirtualKeyboard = nil) or not FVirtualKeyboard.IsPassword);
        TStandardActionType.Paste:
          Result := not FShowSpellItems and not ClipboardService.GetClipboard.IsEmpty and not FReplaceMenu;
        TStandardActionType.Select:
          Result := not FShowSpellItems and FTextInput.GetSelection.IsEmpty and FTextInput.HasText and not FReplaceMenu;
        TStandardActionType.SelectAll:
          Result := not FShowSpellItems and FTextInput.GetSelection.IsEmpty and FTextInput.HasText and not FReplaceMenu;
        TStandardActionType.Unknown:
          Result := False;
        TStandardActionType.PromptForReplace:
          Result := not FShowSpellItems and not FTextInput.GetSelection.IsEmpty and not FReplaceMenu;
        TStandardActionType.Replace:
          Result := not FShowSpellItems and not FTextInput.GetSelection.IsEmpty and FReplaceMenu;
        TStandardActionType.Spell1:
          Result := FShowSpellItems and (Length(FSpells) > 0);
        TStandardActionType.Spell2:
          Result := FShowSpellItems and (Length(FSpells) > 1);
        TStandardActionType.Spell3:
          Result := FShowSpellItems and (Length(FSpells) > 2);
      else
        Result := False;
      end;
  finally
    ClipboardService := nil;
  end;
end;

procedure TFMXTextEditActionsMenu.Copy;
begin
  if FTextActions <> nil then
    FTextActions.CopyToClipboard;
end;

procedure TFMXTextEditActionsMenu.Cut;
begin
  if FTextActions <> nil then
    FTextActions.CutToClipboard;
end;

procedure TFMXTextEditActionsMenu.DoControlChanged;
var
  MenuItems: NSMutableArray;
  SpellTitle: NSString;
begin
  if Control <> nil then
  begin
    Supports(Control, ITextInput, FTextInput);
    Supports(Control, ITextActions, FTextActions);
    Supports(Control, ITextSpellCheck, FSpellCheck);
    Supports(Control, ITextSpellCheckActions, FSpellActions);
    Supports(Control, IVirtualKeyboardControl, FVirtualKeyboard);
  end
  else
  begin
    FTextInput := nil;
    FTextActions := nil;
    FSpellCheck := nil;
    FSpellActions := nil;
    FVirtualKeyboard := nil;
  end;
  if FSpellActions <> nil then
  begin
    if FSpellItem1 = nil then
    begin
      MenuItems := TNSMutableArray.Create;
      FSpellItem1 := TUIMenuItem.Alloc;
      if Length(FSpells) > 0 then
        SpellTitle := StrToNSStr(FSpells[0])
      else
        SpellTitle := StrToNSStr('Spell1');
      FSpellItem1 := TUIMenuItem.Wrap(FSpellItem1.initWithTitle(SpellTitle, sel_getUid('spell1:')));
      MenuItems.addObject(NSObjectToID(FSpellItem1));
      FSpellItem2 := TUIMenuItem.Alloc;
      if Length(FSpells) > 1 then
        SpellTitle := StrToNSStr(FSpells[1])
      else
        SpellTitle := StrToNSStr('Spell2');
      FSpellItem2 := TUIMenuItem.Wrap(FSpellItem2.initWithTitle(SpellTitle, sel_getUid('spell2:')));
      MenuItems.addObject(NSObjectToID(FSpellItem2));
      FSpellItem3 := TUIMenuItem.Alloc;
      if Length(FSpells) > 2 then
        SpellTitle := StrToNSStr(FSpells[2])
      else
        SpellTitle := StrToNSStr('Spell3');
      FSpellItem3 := TUIMenuItem.Wrap(FSpellItem3.initWithTitle(SpellTitle, sel_getUid('spell3:')));
      MenuItems.addObject(NSObjectToID(FSpellItem3));
      //
      FMenuController.setMenuItems(MenuItems);
      MenuItems.release;
    end;
  end;
end;

procedure TFMXTextEditActionsMenu.DoDefineSelectionFrame(var Frame: CGRect);
var
  SelectionRect: TRectF;
  AbsolutePos: TPointF;
begin
  if FTextInput <> nil then
  begin
    SelectionRect := FTextInput.GetSelectionRect;
    AbsolutePos := FControl.LocalToAbsolute(SelectionRect.TopLeft);
    Frame := CGRectMake(AbsolutePos.X, AbsolutePos.Y, SelectionRect.Width, SelectionRect.Height);
  end;
end;

function TFMXTextEditActionsMenu.GetClipboardService: IFMXClipboardService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, Result);
end;

procedure TFMXTextEditActionsMenu.HideHighlightSpell;
begin
  if FSpellCheck <> nil then
    FSpellCheck.HideHighlightSpell;
end;

procedure TFMXTextEditActionsMenu.HighlightSpell;
begin
  if FSpellCheck <> nil then
    FSpellCheck.HighlightSpell;
end;

procedure TFMXTextEditActionsMenu.Paste;
begin
  if FTextActions <> nil then
    FTextActions.PasteFromClipboard;
end;

procedure TFMXTextEditActionsMenu.PromptForReplace;

  function GetLanguage: NSString;
  begin
    Result := TNSString.Wrap(TUITextChecker.OCClass.availableLanguages.objectAtIndex(0));
    if Result = nil then
      Result := StrToNSStr('en_US');
  end;

var
  TheLanguage: NSString;
  StringRange: NSRange;
  TextChecker: UITextChecker;
  CurrentOffset: NSInteger;
  CurrentRange: NSRange;
  Guesses: NSArray;

  TheText: NSString;
  Word: NSString;
  MenuTmp: UIMenuItem;
  I: Integer;

  GuessesMenuItems: NSMutableArray;
begin
  if (FTextInput = nil) or (FTextActions = nil) then
    Exit;

  TextChecker := TUITextChecker.Create;
  CurrentOffset := 0;
  StringRange.location := 0;
  TheText := StrToNSStr(FTextInput.GetSelection);
  StringRange.length := TheText.length - 1;

  TheLanguage := GetLanguage;

  currentRange := TextChecker.rangeOfMisspelledWordInString(TheText,
    StringRange, CurrentOffset, False, TheLanguage);

  if CurrentRange.location = NSNotFound then
    Exit;

  Guesses := TextChecker.guessesForWordRange(currentRange, theText, theLanguage);
  GuessesMenuItems := TNSMutableArray.Create;
  for I := 0 to Guesses.count - 1 do
  begin
    MenuTmp := TUIMenuItem.Alloc;
    Word := TNSString.Wrap(Guesses.objectAtIndex(I));
    MenuTmp.initWithTitle(Word, sel_getUid('replace:'));
    GuessesMenuItems.addObject(NSObjectToID(MenuTmp));
  end;

  Hide;
  FReplaceMenu := True;
  FMenuController.setMenuItems( GuessesMenuItems);
  FMenuController.setMenuVisible(True);
end;

procedure TFMXTextEditActionsMenu.Select;
var
  View: TFMXViewBase;
begin
  if FTextActions <> nil then
  begin
    View := TFMXViewBase(WindowHandleToPlatform(TCommonCustomForm(Control.Root.GetObject).Handle).Handle);
    View.FInputDelegate.selectionWillChange(View.GetObjectID);
    FTextActions.SelectWord;
    View.FInputDelegate.selectionDidChange(View.GetObjectID);
    Show;
  end;
end;

procedure TFMXTextEditActionsMenu.SelectAll;
var
  View: TFMXViewBase;
begin
  if FTextActions <> nil then
  begin
    View := TFMXViewBase(WindowHandleToPlatform(TCommonCustomForm(Control.Root.GetObject).Handle).Handle);
    View.FInputDelegate.selectionWillChange(View.GetObjectID);
    FTextActions.SelectAll;
    View.FInputDelegate.selectionDidChange(View.GetObjectID);
    Show;
  end;
end;

procedure TFMXTextEditActionsMenu.SetSpellItems(items: TArray<string>);
begin
  FSpells := items;
  if (FSpellItem1 <> nil) and (Length(FSpells) > 0) then
  begin
    FSpellItem1.setTitle(StrToNSStr(FSpells[0]));
    if (Length(FSpells) > 1) and (FSpellItem2 <> nil) then
    begin
      FSpellItem2.setTitle(StrToNSStr(FSpells[1]));
      if (Length(FSpells) > 2) and (FSpellItem3 <> nil) then
        FSpellItem3.setTitle(StrToNSStr(FSpells[2]));
    end;
  end;
end;

procedure TFMXTextEditActionsMenu.Spell1;
begin
  if FSpellActions <> nil then
    FSpellActions.Spell(FSpells[0]);
end;

procedure TFMXTextEditActionsMenu.Spell2;
begin
  if FSpellActions <> nil then
    FSpellActions.Spell(FSpells[1]);
end;

procedure TFMXTextEditActionsMenu.Spell3;
begin
  if FSpellActions <> nil then
    FSpellActions.Spell(FSpells[2]);
end;

{ TiOSWindowHandle }

function WindowHandleToPlatform(const AHandle: TWindowHandle): TiOSWindowHandle;
begin
  if AHandle is TiOSWindowHandle then
    Result := TiOSWindowHandle(AHandle)
  else
    raise EInvalidFmxHandle.CreateFmt(SInvalidFmxHandleClass, ['AHandle', 'TiOSWindowHandle']);
end;

constructor TiOSWindowHandle.Create(const AHandle: TOCLocal);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TiOSWindowHandle.Destroy;
begin
  FZOrderManager.Free;
  FHandle.Free;
  inherited;
end;

function TiOSWindowHandle.GetForm: TCommonCustomForm;
begin
  Result := TFMXViewBase(FHandle).Form;
end;

function TiOSWindowHandle.GetGLView: GLKView;
begin
  if TWindowStyle.GPUSurface in Form.WindowStyle then
    Result := GLKView(TFMXViewBase(FHandle).Super)
  else
    Result := nil;
end;

function TiOSWindowHandle.GetScale: Single;
begin
  Result := MainScreen.scale;
end;

function TiOSWindowHandle.GetView: UIView;
begin
  Result := TFMXViewBase(FHandle).View;
end;

function TiOSWindowHandle.GetWnd: UIWindow;
begin
  if View <> nil then
    Result := View.window
  else
    Result := nil;
end;

function TiOSWindowHandle.GetZOrderManager: TiOSZOrderManager;
begin
  if FZOrderManager = nil then
    FZOrderManager := TiOSZOrderManager.Create(Self);
  Result := FZOrderManager;
end;

{ TiOSOpenApplicationContext }

constructor TiOSOpenApplicationContext.Create(ASourceApp: string; AURL: string; AContext: Pointer);
begin
  inherited Create;
  FSourceApp := ASourceApp;
  FURL := AURL;
  FContext := AContext;
end;

{ TFMXWakeHandler }

procedure TFMXWakeHandler.DoCheckSynchronize;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    CheckSynchronize;
end;

function TFMXWakeHandler.GetNativeObject: NSObject;
begin
  Result := NSObject(Super);
end;

function TFMXWakeHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IFMXWakeHandler);
end;

{ TWindowManager }

procedure TCocoaTouchWindowManager.Activate(const AForm: TCommonCustomForm);
begin
  UpdateStatusBar(AForm);
end;

procedure TCocoaTouchWindowManager.BringToFront(const AForm: TCommonCustomForm);
var
  View: UIView;
begin
  if AForm.Visible then
  begin
    View := WindowHandleToPlatform(AForm.Handle).View;
    RootViewController.View.bringSubviewToFront(View);
  end;
end;

function TCocoaTouchWindowManager.CalculateFormViewFrame(const AForm: TCommonCustomForm): NSRect;
begin
  if IsPopupForm(AForm) then
    Result := CGRectMake(AForm.Left, AForm.Top, AForm.Width, AForm.Height)
  else
  begin
    Result := Window.NativeWindow.rootViewController.view.bounds;
    if (AForm.SystemStatusBar.Visibility = TFormSystemStatusBar.TVisibilityMode.Visible) and (AForm.BorderStyle <> TFmxFormBorderStyle.None) then
    begin
      Result.origin.y := Result.origin.y + StatusBarOffset;
      Result.size.height := Result.size.height - StatusBarOffset;
    end;
  end;
end;

function TCocoaTouchWindowManager.ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
begin
  Result := WindowHandleToPlatform(AForm.Handle).View.convertPointToView(CGPoint.Create(Point), NativeWindow).ToPointF;
end;

constructor TCocoaTouchWindowManager.Create;
begin
  inherited;
  FCanSetState := True;
  FStatusBarOffset := DefaultStatusBarOffset;
  RegisterService;
end;

function TCocoaTouchWindowManager.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
var
  FormView: TFMXViewBase;
begin
  Result := nil;

  FormView := TFMXView3D.Create(AForm);
  FormView.View.setHidden(True);

  Result := TiOSWindowHandle.Create(FormView);
end;

destructor TCocoaTouchWindowManager.Destroy;
begin
  UnregisterService;
  inherited;
end;

procedure TCocoaTouchWindowManager.DestroyWindow(const AForm: TCommonCustomForm);
begin
  if (AForm <> nil) and (AForm.Handle <> nil) then
    WindowHandleToPlatform(AForm.Handle).View.removeFromSuperview;
end;

function TCocoaTouchWindowManager.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := WindowHandleToPlatform(AHandle).Form;
end;

function TCocoaTouchWindowManager.GetClientSize(const AForm: TCommonCustomForm): TPointF;
begin
  Result := GetWindowRect(AForm).Size;
end;

function TCocoaTouchWindowManager.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
begin
  Result := not IsPopupForm(AForm);
end;

function TCocoaTouchWindowManager.GetNativeWindow: UIWindow;
begin
  Result := Window.NativeWindow;
end;

function TCocoaTouchWindowManager.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
begin
  if IsPopupForm(AForm) then
    Result := WindowHandleToPlatform(AForm.Handle).View.bounds.ToRectF
  else
  begin
    Result := Window.RootViewController.View.frame.ToRectF;
    if (AForm.SystemStatusBar.Visibility = TFormSystemStatusBar.TVisibilityMode.Visible) and (AForm.BorderStyle <> TFmxFormBorderStyle.None) then
      Result.Top := Result.Top + StatusBarOffset;
  end;
end;

function TCocoaTouchWindowManager.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  Result := AForm.Handle.Scale;
end;

function TCocoaTouchWindowManager.HasFormStatusBar(const AForm: TCommonCustomForm): Boolean;
begin
  Result := (AForm <> nil) and (AForm.BorderStyle <> TFmxFormBorderStyle.None) and (AForm.SystemStatusBar.Visibility <> TFormSystemStatusBar.TVisibilityMode.Invisible);
end;

procedure TCocoaTouchWindowManager.HideWindow(const AForm: TCommonCustomForm);
begin
  if (AForm <> nil) and (AForm.Handle <> nil) then
    WindowHandleToPlatform(AForm.Handle).View.setHidden(True);
  UpdateFormState(AForm, TWindowState.wsMinimized);
end;

procedure TCocoaTouchWindowManager.InvalidateImmediately(const AForm: TCommonCustomForm);
var
  WindowHandle: TiOSWindowHandle;
begin
  WindowHandle := WindowHandleToPlatform(AForm.Handle);
  if WindowHandle.GLView <> nil then
    WindowHandle.GLView.display
  else
    WindowHandle.View.setNeedsDisplay;
end;

procedure TCocoaTouchWindowManager.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
begin
  WindowHandleToPlatform(AForm.Handle).View.setNeedsDisplayInRect(NSRect.Create(R));
end;

function TCocoaTouchWindowManager.IsPopupForm(const AForm: TCommonCustomForm): Boolean;
begin
  Result := (AForm <> nil) and ((AForm.FormStyle = TFormStyle.Popup) or (AForm.Owner is TPopup) or (AForm is TCustomPopupForm));
end;

procedure TCocoaTouchWindowManager.RegisterService;
begin
  if not TPlatformServices.Current.SupportsPlatformService(IFMXWindowService) then
    TPlatformServices.Current.AddPlatformService(IFMXWindowService, Self);
  if not TPlatformServices.Current.SupportsPlatformService(IFMXWindowSystemStatusBarService) then
    TPlatformServices.Current.AddPlatformService(IFMXWindowSystemStatusBarService, Self);
end;

procedure TCocoaTouchWindowManager.ReleaseCapture(const AForm: TCommonCustomForm);
begin
  // NOP on iOS
end;

procedure TCocoaTouchWindowManager.ReleaseWindow(const AForm: TCommonCustomForm);
begin
  if (AForm <> nil) and (AForm.Handle <> nil) then
    WindowHandleToPlatform(AForm.Handle).View.removeFromSuperview;
end;

function TCocoaTouchWindowManager.ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
begin
  Result := NativeWindow.convertPointToView(CGPoint.Create(Point), WindowHandleToPlatform(AForm.Handle).View).ToPointF;
end;

procedure TCocoaTouchWindowManager.SendToBack(const AForm: TCommonCustomForm);
var
  View: UIView;
begin
  if AForm.Visible then
  begin
    View := WindowHandleToPlatform(AForm.Handle).View;
    RootViewController.View.sendSubviewToBack(View);
  end;
end;

procedure TCocoaTouchWindowManager.SetBackgroundColor(const AForm: TCommonCustomForm; const AColor: TAlphaColor);
begin
  if (AColor = TAlphaColorRec.Null) and (AForm is TCustomForm) then
    RootViewController.SetStatusBarBackgroundColor(TCustomForm(AForm).Fill.Color)
  else
    RootViewController.SetStatusBarBackgroundColor(AColor);
end;

procedure TCocoaTouchWindowManager.SetCapture(const AForm: TCommonCustomForm);
begin
  // NOP on iOS
end;

procedure TCocoaTouchWindowManager.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
begin
  // Only popup forms can be placed in any position on the screen and can have any size
  if IsPopupForm(AForm) then
    AForm.SetBounds(AForm.Left, AForm.Top, Round(ASize.X), Round(ASize.Y));
end;

procedure TCocoaTouchWindowManager.SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
begin
  // NOP on iOS
end;

procedure TCocoaTouchWindowManager.SetRootViewController(const Value: TFMXViewController);
begin
  FRootViewController := Value;
  Window.RootViewController := Value;
end;

procedure TCocoaTouchWindowManager.SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
begin
  // NOP on iOS
end;

procedure TCocoaTouchWindowManager.SetVisibility(const AForm: TCommonCustomForm; const AMode: TFormSystemStatusBar.TVisibilityMode);
begin
  UpdateStatusBar(AForm);
end;

procedure TCocoaTouchWindowManager.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
begin
  // NOP on iOS
end;

procedure TCocoaTouchWindowManager.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  if (AForm.Handle <> nil) and IsPopupForm(AForm) then
    WindowHandleToPlatform(AForm.Handle).View.setFrame(NSRect.Create(ARect));
end;

procedure TCocoaTouchWindowManager.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
begin
  if FCanSetState then
    try
      FCanSetState := False;
      if AForm.Visible and (AState = TWindowState.wsMinimized) then
        AForm.Visible := False;
      if AForm.Visible then
        if IsPopupForm(AForm) then
          AForm.WindowState := TWindowState.wsNormal
        else
          AForm.WindowState := TWindowState.wsMaximized
      else
        AForm.WindowState := TWindowState.wsMinimized;
    finally
      FCanSetState := True;
    end;
end;

procedure TCocoaTouchWindowManager.ShowWindow(const AForm: TCommonCustomForm);
var
  FormView: UIView;
begin
  // Added form view to application Root view
  FormView := WindowHandleToPlatform(AForm.Handle).View;
  RootViewController.View.addSubview(FormView);
  UpdateStatusBar(AForm);
  FormView.setHidden(False);

  if IsPopupForm(AForm) then
    UpdateFormState(AForm, TWindowState.wsNormal)
  else
    UpdateFormState(AForm, TWindowState.wsMaximized);

  AForm.Activate;
end;

function TCocoaTouchWindowManager.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
begin
  AForm.Show;
  try
    AForm.ModalResult := mrNone;
    repeat
      if not Application.HandleMessage then
        InternalWaitMessage;
      if Application.Terminated then
        AForm.ModalResult := mrCancel
      else if AForm.ModalResult <> mrNone then
        AForm.CloseModal;
    until AForm.ModalResult <> mrNone;
  finally
    AForm.Hide;
  end;
  Result := AForm.ModalResult;
end;

function TCocoaTouchWindowManager.CanShowModal: Boolean;
begin
  Result := True;
end;

procedure TCocoaTouchWindowManager.UnregisterService;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXWindowService);
end;

procedure TCocoaTouchWindowManager.UpdateFormState(const AForm: TCommonCustomForm; const ANewState: TWindowState);
begin
  if FCanSetState then
  begin
    FCanSetState := False;
    try
      AForm.WindowState := ANewState;
    finally
      FCanSetState := True;
    end;
  end;
end;

procedure TCocoaTouchWindowManager.UpdateStatusBar(const AForm: TCommonCustomForm);
begin
  if not IsPopupForm(AForm) then
  begin
    RootViewController.StatusBarVisible := HasFormStatusBar(AForm);
    if AForm is TCustomForm then
      RootViewController.StatusBarColor := TCustomForm(AForm).SystemStatusBar.BackgroundColor;
  end;
end;

//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/
constructor TAppDelegateMessage_applicationDidFinishLaunchingWithOptions.Create(
  const AValue: TAppDelegate_applicationDidFinishLaunchingWithOptions);
begin
  inherited Create(AValue);
end;

constructor TAppDelegateMessage_applicationOpenURLWithSourceAnnotation.Create(
  const AValue: TAppDelegate_applicationOpenURLWithSourceAnnotation);
begin
  inherited Create(AValue);
end;

constructor TAppDelegateMessage_applicationOpenURLWithOptions.Create(
  const AValue: TAppDelegate_applicationOpenURLWithOptions);
begin
  inherited Create(AValue);
end;

constructor TAppDelegateMessage_applicationDidBecomeActive.Create(
  const AValue: TAppDelegate_applicationDidBecomeActive);
begin
  inherited Create(AValue);
end;
//https://blog.grijjy.com/2017/01/23/using-facebook-sdk-native-framework-for-ios-and-android-for-social-login-and-more-part-1/

{ TNotificationCenterDelegate }

procedure TNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter;
  notification: UNNotification);
begin
end;

procedure TNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter; response: UNNotificationResponse; completionHandler: Pointer); cdecl;
var
  CompletionHandlerImpl: procedure; cdecl;
begin
  // We don't use TMessage<UNNotificationResponse>, because we declare 2 types of UNNotificationResponse>
  // in the different units with the same guid for avoiding interface breaking changes.
  // So TMessage<UNNotificationResponse> are not equaled types. So we use Pointer instead.
  // It's only for Update 1!
  TMessageManager.DefaultManager.SendMessage(Self, TMessage<Pointer>.Create(NSObjectToID(response)));
  @CompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  CompletionHandlerImpl;
  imp_removeBlock(@CompletionHandlerImpl);
end;

procedure TNotificationCenterDelegate.userNotificationCenter(center: UNUserNotificationCenter;
  notification: UNNotification; completionHandler: Pointer);
var
  CompletionHandlerImpl: procedure(options: UNNotificationPresentationOptions); cdecl;
  Content: UNNotificationContent;
  Options: UNNotificationPresentationOptions;
begin
  Content := notification.request.content;
  Options := UNNotificationPresentationOptionAlert;
  if Content.sound <> nil then
    Options := Options or UNNotificationPresentationOptionSound;
  if Content.badge <> nil then
    Options := Options or UNNotificationPresentationOptionBadge;

  @CompletionHandlerImpl := imp_implementationWithBlock(completionHandler);
  CompletionHandlerImpl(Options);
  imp_removeBlock(@CompletionHandlerImpl);
end;

initialization
finalization
  FreeLibrary(GLKitMod);
end.
