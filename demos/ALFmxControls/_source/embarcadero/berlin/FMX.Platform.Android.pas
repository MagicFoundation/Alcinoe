{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Android;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.UITypes, System.Rtti, System.SyncObjs, System.Messaging, FMX.Types, FMX.Platform,
  FMX.Forms, FMX.Types3D, Androidapi.Helpers, Androidapi.NativeWindow, Androidapi.NativeActivity, Androidapi.AppGlue,
  Androidapi.Timer, Androidapi.JNI.Hardware, Androidapi.JNI.App, Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Embarcadero, Posix.SysTypes, FMX.MultiTouch.Android;

type
  TAndroidWindowHandle = class(TWindowHandle)
  strict private
    FTexture: TTexture;
    FBounds: TRectF;
    [Weak] FForm: TCommonCustomForm;
    FNeedsUpdate: Boolean;
  private
    procedure SetBounds(const Value: TRectF);
    procedure SetNeedsUpdate(const Value: Boolean);
    function GetIsPopup: Boolean;
  protected
    function GetScale: Single; override;
  public
    constructor Create(const AForm: TCommonCustomForm);
    procedure CreateTexture;
    procedure DestroyTexture;
    function RequiresComposition: Boolean;
    property Bounds: TRectF read FBounds write SetBounds;
    property Form: TCommonCustomForm read FForm;
    property Texture: TTexture read FTexture;
    property NeedsUpdate: Boolean read FNeedsUpdate write SetNeedsUpdate;
    property IsPopup: Boolean read GetIsPopup;
  end;

  { Broadcast messages: Taking images }

  TMessageCancelReceivingImage = class(TMessage<Integer>);
  TMessageReceivedImagePath = class (TMessage<string>)
  public
    RequestCode: Integer;
  end;

  TScreenScaleOverrideHook = procedure(const UserContext: Pointer; const DensityScale, DensityDPI: Single;
    var ScreenScale: Single);

function WindowHandleToPlatform(const AHandle: TWindowHandle): TAndroidWindowHandle;
function GetAndroidApp: PAndroid_app;
function MainActivity: JFMXNativeActivity;

function ConvertPixelToPoint(const P: TPointF): TPointF;
function ConvertPointToPixel(const P: TPointF): TPointF;

procedure SetScreenScaleOverrideHook(const UserContext: Pointer; const Hook: TScreenScaleOverrideHook);
procedure UnsetScreenScaleOverrideHook;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

implementation

uses
  Posix.StdLib, Posix.Unistd, Posix.Time, Androidapi.Input, Androidapi.Looper, Androidapi.Jni, Androidapi.Jni.JavaTypes,
  Androidapi.Egl, Androidapi.Gles2, Androidapi.Gles2ext, Androidapi.Jni.Util, Androidapi.Log, Androidapi.JNI.Widget,
  Androidapi.JNI.Os, Androidapi.Keycodes, Androidapi.Rect, FMX.WebBrowser, Androidapi.NativeWindowJni, Androidapi.Gles,
  Androidapi.JNI.Telephony, System.SysUtils, System.Math, System.Math.Vectors, System.Generics.Collections,
  System.Character, System.RTLConsts, System.IOUtils, FMX.KeyMapping, FMX.Graphics.Android, FMX.Helpers.Android,
  FMX.Canvas.GPU, FMX.Context.GLES.Android, FMX.Controls, FMX.Controls.Android, FMX.Materials.Canvas, FMX.Pickers,
  FMX.Gestures, FMX.Gestures.Android, FMX.Dialogs, FMX.Dialogs.Android, FMX.VirtualKeyboard, FMX.VirtualKeyboard.Android, FMX.Consts,
  FMX.Text, FMX.Graphics, FMX.TextLayout, FMX.Maps, System.Devices, FMX.Presentation.Style, ALFmxInertialMovement;

const
  DBL_TAP_DELAY = 300; //delay between the 2 taps
  SINGLE_TAP_DELAY = 150;
  LONG_TAP_DURATION = 500;
  LONG_TAP_MOVEMENT = 10; //10 pixels - use scale to transform to points to use on each device
  HIDE_PASTE_MENU_DELAY = 2500;

type
  TContextMenuItem = (Copy, Cut, Paste);
  TContextMenuItems = set of TContextMenuItem;

  TAndroidAppCmdState = (InitWindow, LostFocus, ConfigChanged);
  TAndroidAppCmdStates = set of TAndroidAppCmdState;

const
  AllContextMenuItems = [TContextMenuItem.Copy, TContextMenuItem.Cut, TContextMenuItem.Paste];

type
  TWindowManager = class;

  TFMXNativeActivityListener = class (TJavaLocal, JOnActivityListener)
  public
    procedure onCancelReceiveImage(ARequestCode: Integer); cdecl;
    procedure onReceiveImagePath(ARequestCode: Integer; AFileName: JString); cdecl;
    procedure onReceiveNotification(P1: JIntent); cdecl;
    procedure onReceiveResult(ARequestCode, AResultCode: Integer; AResultObject: JIntent); cdecl;
  end;

  TCopyButtonClickListener = class(TJavaLocal, JView_OnClickListener)
  public
    procedure onClick(P1: JView); cdecl;
  end;

  TCutButtonClickListener = class(TJavaLocal, JView_OnClickListener)
  public
    procedure onClick(P1: JView); cdecl;
  end;

  TPasteButtonClickListener = class(TJavaLocal, JView_OnClickListener)
  public
    procedure onClick(P1: JView); cdecl;
  end;

  TTextServiceAndroid = class;
  TTextUpdate = record
    X: Integer;
    Text: string;
    [Weak] Service: TTextServiceAndroid;
  end;

  TWindowManager = class(TInterfacedObject, IFreeNotification)
  private
    FWindows: TList<TAndroidWindowHandle>;
    FVisibleStack: TStack<TAndroidWindowHandle>;
    [Weak] FGestureControl: TComponent;
    [Weak] FMouseDownControl: TControl;
    FNeedsRender: Boolean;
    FNeedsRender2: Boolean; // https://quality.embarcadero.com/browse/RSP-19238
    FPause: Boolean;
    FScale: Single;
    FContentRect: TRect;
    FNewContentRect: TRect;
    FStatusBarHeight: Integer;
    //Text editing
    FFocusedControl: IControl;
    FContextMenuPopup: JPopupWindow;
    FContextMenuPopupSize: TSize;
    FContextMenuLayout: JLinearLayout;
    FContextButtonsLayout: JLinearLayout;
    FCopyButton: JButton;
    FCopyClickListener: TCopyButtonClickListener;
    FCutButton: JButton;
    FCutClickListener: TCutButtonClickListener;
    FPasteButton: JButton;
    FPasteClickListener: TPasteButtonClickListener;
    FContextMenuVisible: Boolean;
    [Weak] FCapturedWindow: TAndroidWindowHandle;
    FSelectionInProgress: Boolean;
    FPasteMenuTimer: TFmxHandle;
    FVKStateMessageID: Integer;
    FMultiTouchManager: TMultiTouchManagerAndroid;
    procedure SetPause(const Value: Boolean);
    function RetrieveContentRect: TRect;
    procedure UpdateContentRect;
    procedure UpdateFormSizes;
    procedure ShowContextMenu(const ItemsToShow: TContextMenuItems = AllContextMenuItems);
    procedure DoShowContextMenu;
    procedure VKStateHandler(const Sender: TObject; const M: TMessage);
    procedure HideContextMenu;
    class var FInstance: TWindowManager;
    class function GetInstance: TWindowManager; static;
    procedure CreatePasteMenuTimer;
    procedure DestroyPasteMenuTimer;
    procedure PasteMenuTimerCall;
    procedure PrepareClosePopups(const SaveForm: TAndroidWindowHandle);
    procedure ClosePopups;
    function GetMultiTouchManager: TMultiTouchManagerAndroid;
    property MultiTouchManager: TMultiTouchManagerAndroid read GetMultiTouchManager;
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginSelection;
    procedure EndSelection;
    procedure SetNeedsRender;
    function RenderIfNeeds: Boolean;
    procedure RenderImmediately;
    procedure Render;
    procedure BringToFront(const AHandle: TAndroidWindowHandle);
    procedure SendToBack(const AHandle: TAndroidWindowHandle);
    procedure AddWindow(const AHandle: TAndroidWindowHandle);
    procedure RemoveWindow(const AHandle: TAndroidWindowHandle);
    function AlignToPixel(const Value: Single): Single; inline;
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function FindWindowByPoint(X, Y: Single): TAndroidWindowHandle;
    function FindTopWindow: TAndroidWindowHandle;
    function FindTopWindowForTextInput: TAndroidWindowHandle;
    function SendCMGestureMessage(AEventInfo: TGestureEventInfo): Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoCLick: Boolean = True);
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState; KeyDownHandled: Boolean);
    procedure MultiTouch(const Touches: TTouches; const Action: TTouchAction; const AEnabledGestures: TInteractiveGestures = []);
    procedure SetCapture(const AForm: TAndroidWindowHandle);
    procedure ReleaseCapture;
    function PixelToPoint(const P: TPointF): TPointF;
    function PointToPixel(const P: TPointF): TPointF;
    function ClientToScreen(const Handle: TAndroidWindowHandle; const Point: TPointF): TPointF;
    function ScreenToClient(const Handle: TAndroidWindowHandle; const Point: TPointF): TPointF;
    procedure ContentRectChanged(const NewRect: TRect);
    function CheckContentRectUpdate: Boolean;
    procedure SingleTap;
    function TextGetService: TTextServiceAndroid;
    function TextGetActions: ITextActions;
    procedure TextResetSelection;
    function TextReadOnly: Boolean;
    procedure SetFocusedControl(const Control: IControl);
    property ContentRect: TRect read FContentRect;
    property Windows: TList<TAndroidWindowHandle> read FWindows;
    property Pause: Boolean read FPause write SetPause;
    property Scale: Single read FScale;
    property StatusBarHeight: Integer read FStatusBarHeight;
    class property Current: TWindowManager read GetInstance;
  end;

  { TPlatformAndroid }

  TTextServiceUpdater = class;

  TPlatformAndroid = class(TInterfacedObject, IFMXApplicationEventService, IFMXApplicationService, IFMXWindowService,
    IFMXCanvasService, IFMXContextService, IFMXTimerService, IFMXLoggingService,
    IFMXSystemInformationService, IFMXDialogService, IFMXGestureRecognizersService, IFMXScreenService,
    IFMXSystemFontService, IFMXMouseService, IFMXTextService, IFMXLocaleService,
    IFMXDefaultMetricsService, IFMXListingService, IFMXDefaultPropertyValueService, IFMXFullScreenWindowService,
    IFMXSaveStateService, IFMXDeviceMetricsService, IFMXKeyMappingService)
  private const
    // Number of seconds to wait before switching to low priority message processing mode (for power saving).
    PriorityProcessingTime = 1;
    // Timeout in milliseconds for different processing priorities when waiting for messages.
    PollTimeoutHighPriority = 1;
    PollTimeoutLowPriority = 10;
    PollTimeoutPausedState = 250;
    PollOrientationChange = 100;
    DefaultAndroidFontSize = 14;
    DefaultAndroidFontName = 'Roboto';
    ContentRectChangeRefreshCount = 3;
    UndefinedOrientation = TScreenOrientation(-1);
  private type
    TMotionEvent = record
      Position: TPointF;
      EventAction: Int32;
      Shift: TShiftState;
    end;
    TMotionEvents = TList<TMotionEvent>;
  private
    FAlertListeners: TFMXDialogListenerParentList;
    FAndroidApp: PAndroid_app;
    FOnApplicationEvent: TApplicationEventHandler;
    FActivityListener: TFMXNativeActivityListener;
    FFirstRun: Boolean;
    FLogPrefix: string;
    FActiveInteractiveGestures: TInteractiveGestures;
    FEnabledInteractiveGestures: TInteractiveGestures;
    FOldPoint1, FOldPoint2: TPointF;
    FDoubleTapTimer: TFmxHandle;
    FLongTapTimer: TFmxHandle;
    FSingleTapTimer: TFmxHandle;
    FDblClickFirstMouseUp: Boolean;
    FGestureEnded: Boolean;
    FSingleTap: Boolean;
    FScreenScale: Single;
    FScreenSize: TPointF;
    FMouseCoord: TPointF;
    FMouseDownCoordinates: TPointF;
    FKeyCharacterMap: JKeyCharacterMap;
    FTextEditorProxy: JFMXTextEditorProxy;
    FRotationAngle: Single;
    FInternalEventQueue: TQueue<TThreadProcedure>;
    FEventQueueEmptyEvent: TEvent;
    FMotionEvents: TMotionEvents;
    FDownKey: Word;
    FDownKeyChar: System.WideChar;
    FKeyDownHandled: Boolean;
    FPriorityProcessing: Boolean;
    FPriorityProcessingWait: Boolean;
    FPriorityProcessingStartTime: Double;
    FContentRectMightHaveChanged: Integer;
    FOrientationMightHaveChanged: Boolean;
    FLastOrientation: TScreenOrientation;
    FLastOrientationTime: Double;
    FSkipEventQueue: TQueue<JKeyEvent>;
    FCanSetState: Boolean;
    FTerminating: Boolean;
    FAppCmdStates: TAndroidAppCmdStates;
    FTitle: string;
    FSaveStateStoragePath: string;
    //I do not need it - more clear code
    (*
    FTextServiceUpdater: TTextServiceUpdater;
    *)
    FKeyMapping: TKeyMapping;
    procedure CreateDoubleTapTimer;
    procedure DestroyDoubleTapTimer;
    procedure DoubleTapTimerCall;
    procedure CreateLongTapTimer;
    procedure DestroyLongTapTimer;
    procedure LongTapTimerCall;
    procedure SingleTap;
    procedure CreateSingleTapTimer;
    procedure DestroySingleTapTimer;
    procedure SingleTapTimerCall;
    function GetLongTapAllowedMovement: Single;
    function IsPopupForm(const AForm: TCommonCustomForm): Boolean;
    function ShiftStateFromMetaState(const AMetaState: Integer): TShiftState;
    procedure InternalProcessMessages;
    function CreateGestureEventInfo(ASecondPointer: TPointF; const AGesture: TInteractiveGesture; const AGestureEnded: Boolean = False): TGestureEventInfo;
    function HandleAndroidKeyEvent(AEvent: PAInputEvent): Int32;
    function ObtainKeyCharacterMap(DeviceId: Integer): JKeyCharacterMap;
    procedure ProcessAndroidGestureEvents;
    procedure ProcessAndroidMouseEvents;
    function HandleAndroidMotionEvent(AEvent: PAInputEvent): Int32;
    //I do not need it - more clear code
    (*
    procedure CheckOrientationChange;
    *)
    procedure HandleMultiTouch;
    procedure SetKeyboardEventToSkip(event: JKeyEvent);
    // IFMXListingService
    function GetListingHeaderBehaviors: TListingHeaderBehaviors;
    function GetListingSearchFeatures: TListingSearchFeatures;
    function GetListingTransitionFeatures: TListingTransitionFeatures;
    function GetListingEditModeFeatures: TListingEditModeFeatures;
    function IFMXListingService.GetHeaderBehaviors = GetListingHeaderBehaviors;
    function IFMXListingService.GetSearchFeatures = GetListingSearchFeatures;
    function IFMXListingService.GetTransitionFeatures = GetListingTransitionFeatures;
    function IFMXListingService.GetEditModeFeatures = GetListingEditModeFeatures;
    // IFMXSaveStateService
    function GetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;
    function SetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;
    function GetSaveStateStoragePath: string;
    procedure SetSaveStateStoragePath(const ANewPath: string);
    function GetSaveStateNotifications: Boolean;
    function IFMXSaveStateService.GetBlock = GetSaveStateBlock;
    function IFMXSaveStateService.SetBlock = SetSaveStateBlock;
    function IFMXSaveStateService.GetStoragePath = GetSaveStateStoragePath;
    procedure IFMXSaveStateService.SetStoragePath = SetSaveStateStoragePath;
    function IFMXSaveStateService.GetNotifications = GetSaveStateNotifications;
    // IFMXDeviceMetricsService
    function GetDisplayMetrics: TDeviceDisplayMetrics;
    procedure WakeMainThread(Sender: TObject);
    procedure RegisterWakeMainThread;
    procedure UnregisterWakeMainThread;
  public
    constructor Create;
    destructor Destroy; override;
    procedure HandleAndroidCmd(ACmd: Int32); inline;
    function HandleAndroidInputEvent(AEvent: PAInputEvent): Int32; inline;
    procedure RunOnUIThread(Proc: TThreadProcedure);
    procedure SynchronizeOnUIThread(Proc: TThreadProcedure);

    { IFMXMouseService }
    function GetMousePos: TPointF;
    { IFMXTimerService }
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    function GetTick: Double;
    { IFMXApplicationService }
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetVersionString: string;
    function Terminating: Boolean;
    procedure Terminate;
    { IFMXApplicationEventService }
    procedure SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
    function HandleApplicationEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
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
    class function ScaleByMetrics(const Metrics: JDisplayMetrics): Single; static;
    class function OrientationByDisplay(const Display: JDisplay): TScreenOrientation; static;
    { IFMXScreenService }
    function GetScreenSize: TPointF;
    function GetScreenScale: Single;
    function GetScreenOrientation: TScreenOrientation;
    procedure SetScreenOrientation(AOrientations: TScreenOrientations);
    { IFMXCanvasService }
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    { IFMXContextService }
    procedure RegisterContextClasses;
    procedure UnregisterContextClasses;
    { IFMXLoggingService }
    procedure Log(const Fmt: string; const Params: array of const);
    { IFMXSystemInformationService }
    function GetScrollingBehaviour: TScrollingBehaviours;
    function GetMinScrollThumbSize: Single;
    function GetCaretWidth: Integer;
    function GetMenuShowDelay: Integer;
    { IFMXDialogService }
    function DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
    function DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
    function PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
      AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
      var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
    function DialogPrinterSetup: Boolean;
    function MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt;
      const AHelpFileName: string): Integer; overload;
    procedure MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt; const AHelpFileName: string;
      const ACloseDialogProc: TInputCloseDialogProc); overload;
    function InputQuery(const ACaption: string; const APrompts: array of string;
      var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc = nil): Boolean; overload;
    procedure InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
      const ACloseQueryProc: TInputCloseQueryProc); overload;
    { IFMXGestureRecognizersService }
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    { IFMXSystemFontService }
    function GetDefaultFontFamilyName: string;
    function GetDefaultFontSize: Single;
    { IFMXDefaultPropertyValueService }
    function GetDefaultPropertyValue(const AClassName, PropertyName: string): TValue;
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXLocaleService }
    function GetCurrentLangID: string;
    function GetLocaleFirstDayOfWeek: string;
    function GetFirstWeekday: Byte;
    { IFMXDefaultMetricsService }
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;
    { Android view for IME }
    function GetTextEditorProxy: JFmxTextEditorProxy;
    { IFMXFullScreenWindowService }
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
    //I do not need it - more clear code
    (*
    property TextServiceUpdater: TTextServiceUpdater read FTextServiceUpdater;
    *)

    //IFMXKeyMappingService
    /// <summary>Registers a platform key as the given virtual key.</summary>
    function RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
    /// <summary>Unegisters a platform key as the given virtual key.</summary>
    function UnregisterKeyMapping(const PlatformKey: Word): Boolean;
    /// <summary>Obtains the virtual key from a given platform key.</summary>
    function PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
    /// <summary>Obtains the platform key from a given virtual key.</summary>
    function VirtualKeyToPlatformKey(const VirtualKey: Word): Word;    

  end;

  TDeviceServiceAndroid = class(TInterfacedObject, IFMXDeviceService)
  private
    FDeviceClassCached: Boolean;
    FDeviceClass: TDeviceInfo.TDeviceClass;
  public
    function GetModel: string;
    function GetFeatures: TDeviceFeatures;
    function GetDeviceClass: TDeviceInfo.TDeviceClass;
    function SuppportsTelephony: Boolean;
  end;

  TMultiDisplayAndroid = class(TInterfacedObject, IFMXMultiDisplayService)
  strict private
    FDisplayManager: JDisplayManager;
    FWindowManager: JWindowManager;
  private type
    TDisplayAndroid = record
      Display: TDisplay;
      Id: Integer;
      constructor Create(const AIndex: Integer; const APrimary: Boolean; const ABounds, AWorkArea: TRect;
        const AId: Integer);
    end;
  private
    FDisplayCount: Integer;
    FSystemDisplayCount: Integer;
    FWorkAreaRect: TRect;
    FDesktopRect: TRect;
    FDisplayList: TList<TDisplayAndroid>;
    function GetDisplayInfo(const Display: JDisplay; var BoundsRect, WorkareaRect: TRect): Boolean;
    function FindDisplay(const Activity: JActivity): TDisplay;
  protected
    procedure UpdateDisplays;
    property DisplayManager: JDisplayManager read FDisplayManager;
    property WindowManager: JWindowManager read FWindowManager;
  public
    constructor Create;
    procedure UpdateDisplayInformation;
    function GetDisplayCount: Integer;
    function GetDesktopCenterRect(const Size: TSize): TRect;
    function GetWorkAreaRect: TRect;
    function GetDesktopRect: TRect;
    function GetDisplay(const Index: Integer): TDisplay;
    function DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
    function DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
  end;

  TTimerManager = class
  strict private
    FHandlers: TDictionary<TFmxHandle, TTimerProc>;
    FQueueDictionary: TObjectDictionary<TFMXHandle, TThreadedQueue<TFmxHandle>>;
    class var FInstance: TTimerManager;
    class function GetInstance: TTimerManager; static;
    procedure DoOnTimer(TimerHandle: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    procedure DestroyTimer(TimerHandle: TFmxHandle);
    procedure CheckSynchronize;
    procedure KillAllTimers;
    class property Current: TTimerManager read GetInstance;
  end;

  TWaitableValueBase = class(TJavaLocal)
  public
    Done: TEvent;
    Value: TValue;
    constructor Create;
  end;

  TFMXTextListener = class(TJavaLocal, JFMXTextListener)
  strict private
    [Weak] FTextService: TTextServiceAndroid;
  public
    constructor Create(const TextService: TTextServiceAndroid); overload;
    procedure onTextUpdated(text: JCharSequence; position: Integer); cdecl;
    procedure onComposingText(beginPosition: Integer; endPosition: Integer); cdecl;
    procedure onSkipKeyEvent(event: JKeyEvent); cdecl;
  end;

  TTextServiceUpdater = class
  private
    FTextUpdates: TThreadedQueue<TTextUpdate>;
    FTextPostUpdate: TList<TTextServiceAndroid>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure QueueTextUpdate(const Update: TTextUpdate);
    procedure ProcessTextUpdates;
  end;

  TTextServiceAndroid = class(TTextService)
  type
    TRange = record
      location: Integer;
      length: Integer;
    end;
  private
    FCaretPosition: TPoint;
    FText : string;
    FImeMode: TImeMode;
    FTextView: JFmxTextEditorProxy;
    FTextListener: TFMXTextListener;
    FComposingBegin: Integer;
    FComposingEnd: Integer;
    FLines: TStrings;
    FMessageID: Integer;
    FInternalUpdate: Boolean;
    procedure UnpackText;
    procedure CalculateSelectionBounds(out SelectionStart, SelectionEnd: Integer);
    procedure HandleVK(const Sender: TObject; const M: TMessage);
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function GetCaretPostion: TPoint; override;
    procedure SetCaretPosition(const Value: TPoint); override;
  public
    procedure InternalUpdate;
    procedure InternalUpdateSelection;

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

    procedure ProcessUpdate(const Update: TTextUpdate);
    procedure PostProcessUpdate;
  public
    constructor Create(const Owner: IControl; SupportMultiLine: Boolean); override;
    destructor Destroy; override;
    procedure CutSelectedText;
    procedure CopySelectedText;
    procedure PasteText;
  end;

var
  PlatformAndroid: TPlatformAndroid;
  VirtualKeyboardAndroid: TVirtualKeyboardAndroid;
  MultiDisplayAndroid: TMultiDisplayAndroid;
  ScreenScaleOverrideHook: TScreenScaleOverrideHook;
  ScreenScaleOverrideHookContext: Pointer;
  DeviceServiceAndroid: TDeviceServiceAndroid;

procedure SetScreenScaleOverrideHook(const UserContext: Pointer; const Hook: TScreenScaleOverrideHook);
begin
  ScreenScaleOverrideHook := Hook;
  ScreenScaleOverrideHookContext := UserContext;
end;

procedure UnsetScreenScaleOverrideHook;
begin
  ScreenScaleOverrideHook := nil;
  ScreenScaleOverrideHookContext := nil;
end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TAndroidWindowHandle;
begin
  Result := TAndroidWindowHandle(AHandle);
end;

function GetAndroidApp: PAndroid_app;
begin
  Result := PlatformAndroid.FAndroidApp;
end;

function MainActivity: JFMXNativeActivity;
begin
  if (PlatformAndroid.FAndroidApp <> nil) and (PlatformAndroid.FAndroidApp^.activity <> nil) then
    Result := TJFMXNativeActivity.Wrap(PlatformAndroid.FAndroidApp^.activity.clazz)
  else
    Result := nil;
end;

function HandleAndroidInputEvent(var App: TAndroid_app; Event: PAInputEvent): Int32;
begin
  Result := PlatformAndroid.HandleAndroidInputEvent(Event);
end;

procedure HandleAndroidCmd(var App: TAndroid_app; Cmd: Int32);
begin
  PlatformAndroid.HandleAndroidCmd(Cmd);
end;

procedure RegisterCorePlatformServices;
begin
  DeviceServiceAndroid := TDeviceServiceAndroid.Create;
  PlatformAndroid := TPlatformAndroid.Create;
  PlatformAndroid.FAndroidApp := PANativeActivity(System.DelphiActivity)^.instance;
  if Assigned(PlatformAndroid.FAndroidApp) then
  begin
    PlatformAndroid.FAndroidApp^.onAppCmd := @HandleAndroidCmd;
    PlatformAndroid.FAndroidApp^.onInputEvent := @HandleAndroidInputEvent;
  end;

  TPlatformServices.Current.AddPlatformService(IFMXLoggingService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXApplicationEventService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXWindowService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXContextService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXTimerService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXDialogService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXGestureRecognizersService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXScreenService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXLocaleService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultPropertyValueService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXListingService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXSaveStateService, PlatformAndroid);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, PlatformAndroid);


  TPlatformServices.Current.AddPlatformService(IFMXDeviceService, DeviceServiceAndroid);

  if DeviceServiceAndroid.GetDeviceClass <> TDeviceInfo.TDeviceClass.Watch then
  begin
    VirtualKeyboardAndroid := TVirtualKeyboardAndroid.Create;
    TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardService, VirtualKeyboardAndroid);
    TPlatformServices.Current.AddPlatformService(IFMXKeyMappingService, PlatformAndroid);
  end;

  if TOSVersion.Check(4, 2) then
  begin
    MultiDisplayAndroid := TMultiDisplayAndroid.Create;
    TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, MultiDisplayAndroid);
  end;
end;

procedure UnregisterCorePlatformServices;
begin
end;

{ TCopyButtonClickListener }

procedure TCopyButtonClickListener.onClick(P1: JView);
var
  TextService: TTextServiceAndroid;
begin
  TThread.Queue(nil,
    procedure
    begin
      TWindowManager.Current.TextResetSelection;
    end);
  TextService := TWindowManager.Current.TextGetService;
  if TextService <> nil then
    TextService.CopySelectedText;
  TWindowManager.Current.HideContextMenu;
end;

{ TCutButtonClickListener }

procedure TCutButtonClickListener.onClick(P1: JView);
var
  TextService: TTextServiceAndroid;
begin
  TThread.Queue(nil,
    procedure
    begin
      TWindowManager.Current.TextResetSelection;
    end);
  TextService := TWindowManager.Current.TextGetService;
  if (TextService <> nil) and not TWindowManager.Current.TextReadOnly then
    TextService.CutSelectedText;
  TWindowManager.Current.HideContextMenu;
end;

{ TPasteButtonClickListener }

procedure TPasteButtonClickListener.onClick(P1: JView);
var
  TextService: TTextServiceAndroid;
begin
  TThread.Queue(nil,
    procedure
    begin
      TWindowManager.Current.TextResetSelection;
    end);

  TextService := TWindowManager.Current.TextGetService;
  if (TextService <> nil) and not TWindowManager.Current.TextReadOnly then
    TextService.PasteText;
  TWindowManager.Current.HideContextMenu;
end;

{ TWindowManager }

constructor TWindowManager.Create;
begin
  inherited Create;
  FWindows := TList<TAndroidWindowHandle>.Create;
  FVisibleStack := TStack<TAndroidWindowHandle>.Create;
  FScale := PlatformAndroid.GetScreenScale;
  UpdateContentRect;
  FMouseDownControl := nil;
  FMultiTouchManager := nil;
  FVKStateMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, VKStateHandler);
end;

procedure TWindowManager.CreatePasteMenuTimer;
begin
  if FPasteMenuTimer = 0 then
    FPasteMenuTimer := TTimerManager.Current.CreateTimer(HIDE_PASTE_MENU_DELAY, PasteMenuTimerCall);
end;

destructor TWindowManager.Destroy;
begin
  FVisibleStack.DisposeOf;
  FWindows.DisposeOf;
  SetFocusedControl(nil);
  FContextMenuPopup := nil;
  FContextMenuLayout := nil;
  FContextButtonsLayout := nil;
  FCopyButton := nil;
  FCopyClickListener := nil;
  FCutButton := nil;
  FCutClickListener := nil;
  FPasteButton := nil;
  FPasteClickListener := nil;
  FMouseDownControl := nil;
  FreeAndNil(FMultiTouchManager);
  inherited;
end;

procedure TWindowManager.DestroyPasteMenuTimer;
begin
  if FPasteMenuTimer <> 0 then
    TTimerManager.Current.DestroyTimer(FPasteMenuTimer);
  FPasteMenuTimer := 0;
end;

procedure TWindowManager.EndSelection;
begin
  FSelectionInProgress := False;
end;

procedure TWindowManager.BeginSelection;
begin
  FSelectionInProgress := True;
end;

procedure TWindowManager.AddWindow(const AHandle: TAndroidWindowHandle);
begin
  FWindows.Add(AHandle);
  UpdateContentRect;
end;

procedure TWindowManager.BringToFront(const AHandle: TAndroidWindowHandle);
begin
  if FWindows.Contains(AHandle) then
  begin
    FWindows.Remove(AHandle);
    FWindows.Add(AHandle);
    if (FVisibleStack.Count = 0) or (FVisibleStack.Peek <> AHandle) then
      FVisibleStack.Push(AHandle);
    SetNeedsRender;
  end;
end;

function TWindowManager.ClientToScreen(const Handle: TAndroidWindowHandle; const Point: TPointF): TPointF;
begin
  if Handle <> nil then
    Result := Point + Handle.Bounds.TopLeft
  else
    Result := Point;
end;

function TWindowManager.ScreenToClient(const Handle: TAndroidWindowHandle; const Point: TPointF): TPointF;
begin
  if Handle <> nil then
    Result := Point - Handle.Bounds.TopLeft
  else
    Result := Point;
end;

function TWindowManager.SendCMGestureMessage(AEventInfo: TGestureEventInfo): Boolean;
var
  Window: TAndroidWindowHandle;
  Obj, LFocusedControl: IControl;
  OldGestureControl: TComponent;
  TmpControl: TFmxObject;
  GObj: IGestureControl;
  ClientEventInfo: TGestureEventInfo;
  TextInput: ITextInput;
  TextActions: ITextActions;
const
  LGestureMap: array [igiZoom .. igiDoubleTap] of TInteractiveGesture =
    (TInteractiveGesture.Zoom, TInteractiveGesture.Pan,
    TInteractiveGesture.Rotate, TInteractiveGesture.TwoFingerTap,
    TInteractiveGesture.PressAndtap, TInteractiveGesture.LongTap,
    TInteractiveGesture.DoubleTap);
begin
  Result := False;
  OldGestureControl := nil;
  Window := FindWindowByPoint(AEventInfo.Location.X, AEventInfo.Location.Y);
  if Window <> nil then
  begin
    if TInteractiveGestureFlag.gfBegin in AEventInfo.Flags then
    begin
      // find the control from under the gesture
      Obj := Window.Form.ObjectAtPoint(AEventInfo.Location);
      if FGestureControl <> nil then
        OldGestureControl := FGestureControl;
      if Obj <> nil then
        FGestureControl := Obj.GetObject
      else
        FGestureControl := Window.Form;

      if Supports(FGestureControl, IGestureControl, GObj) then
        FGestureControl := GObj.GetFirstControlWithGesture(LGestureMap[AEventInfo.GestureID])
      else
        FGestureControl := nil;
    end;

    if not FSelectionInProgress then
      if Window.Form.Focused <> nil then
      begin
        LFocusedControl := Window.Form.Focused;
        if LFocusedControl is TStyledPresentation then
          LFocusedControl := TStyledPresentation(LFocusedControl).PresentedControl;
        if Window.Form.Focused <> LFocusedControl then
          SetFocusedControl(LFocusedControl);
      end
      else if FFocusedControl <> nil then
        SetFocusedControl(nil);

    if FGestureControl <> nil then
    begin
      if Supports(FGestureControl, IGestureControl, GObj) then
        try
          ClientEventInfo := AEventInfo;
          ClientEventInfo.Location := Window.Form.ScreenToClient(ClientEventInfo.Location);
          GObj.CMGesture(ClientEventInfo);
        except
          Application.HandleException(FGestureControl);
        end;

      if not FSelectionInProgress then
      begin
        Obj := Window.Form.ObjectAtPoint(AEventInfo.Location);
        if Obj <> nil then
          TmpControl := Obj.GetObject
        else
          TmpControl := Window.Form;

        if TmpControl is TStyledPresentation then
          TmpControl := TStyledPresentation(TmpControl).PresentedControl;

        if (AEventInfo.GestureID = igiLongTap) and Supports(TmpControl, ITextInput, TextInput) and
          Supports(TmpControl, ITextActions, TextActions) then
        begin
          TextActions.SelectWord;
          TTextServiceAndroid(TextInput.GetTextService).InternalUpdateSelection;
          ShowContextMenu;
        end;

        if AEventInfo.GestureID = igiDoubleTap then
        begin
          while (TmpControl <> nil) and
            not (Supports(TmpControl, ITextInput, TextInput) and Supports(TmpControl, ITextActions, TextActions)) do
            TmpControl := TmpControl.Parent;
          if (TextInput <> nil) and (TextActions <> nil) then
          begin
            TTextServiceAndroid(TextInput.GetTextService).InternalUpdateSelection;
            if not TextInput.GetSelection.IsEmpty then
              ShowContextMenu;
          end;
        end;
      end;
      Result := True;
    end
    else
      FGestureControl := OldGestureControl;
  end;
  if TInteractiveGestureFlag.gfEnd in AEventInfo.Flags then
    FGestureControl := nil;
end;

procedure TWindowManager.SendToBack(const AHandle: TAndroidWindowHandle);
begin
  if FWindows.Contains(AHandle) then
  begin
    FWindows.Remove(AHandle);
    FWindows.Insert(0, AHandle);
    SetNeedsRender;
  end;
end;

function TWindowManager.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := nil;
  if FWindows.Contains(TAndroidWindowHandle(AHandle)) then
    Result := FWindows[FWindows.IndexOf(TAndroidWindowHandle(AHandle))].Form;
end;

function TWindowManager.FindTopWindow: TAndroidWindowHandle;
var
  I: Integer;
begin
  for I := FWindows.Count - 1 downto 0 do
    if FWindows[I].Form.Visible then
      Exit(FWindows[I]);
  Result := nil;
end;

function TWindowManager.FindTopWindowForTextInput: TAndroidWindowHandle;
var
  I: Integer;
begin
  for I := FWindows.Count - 1 downto 0 do
    if FWindows[I].Form.Visible and not (FWindows[I].Form is TCustomPopupForm) then
      Exit(FWindows[I]);
  Result := nil;
end;

function TWindowManager.FindWindowByPoint(X, Y: Single): TAndroidWindowHandle;
var
  I: Integer;
begin
  for I := FWindows.Count - 1 downto 0 do
    if FWindows[I].Form.Visible and FWindows[I].Bounds.Contains(PointF(X, Y)) then
      Exit(FWindows[I]);
  Result := nil;
end;

procedure TWindowManager.FreeNotification(AObject: TObject);
begin
  FFocusedControl := nil;
end;

class function TWindowManager.GetInstance: TWindowManager;
begin
  if FInstance = nil then
    FInstance := TWindowManager.Create;
  Result := FInstance;
end;

function TWindowManager.GetMultiTouchManager: TMultiTouchManagerAndroid;
var
  Window: TAndroidWindowHandle;
begin
  Window := FindTopWindow;
  if Window <> nil then
    if FMultiTouchManager = nil then
      FMultiTouchManager := TMultiTouchManagerAndroid.Create(Window.Form)
    else
      if FMultiTouchManager.Parent <> Window.Form then
        FMultiTouchManager.Parent := Window.Form;

  Result := FMultiTouchManager;
end;

function TWindowManager.TextGetService: TTextServiceAndroid;
var
  TextInput: ITextInput;
begin
  Result := nil;
  if Supports(FFocusedControl, ITextInput, TextInput) then
    Result := TTextServiceAndroid(TextInput.GetTextService);
end;

function TWindowManager.TextGetActions: ITextActions;
begin
  Supports(FFocusedControl, ITextActions, Result);
end;

function TWindowManager.TextReadOnly: Boolean;
var
  ReadOnly: IReadOnly;
begin
  Result := True;
  if Supports(FFocusedControl, IReadOnly, ReadOnly) then
    Result := ReadOnly.ReadOnly;
end;

procedure TWindowManager.TextResetSelection;
var
  TextActions: ITextActions;
begin
  if Supports(FFocusedControl, ITextActions, TextActions) then
    TextActions.ResetSelection;
end;

procedure TWindowManager.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  Window: TAndroidWindowHandle;
begin
  HideContextMenu;
  Window := FindTopWindowForTextInput;
  if Window <> nil then
    try
      Window.Form.KeyDown(Key, KeyChar, Shift);
    except
      Application.HandleException(Window.Form);
    end;
end;

procedure TWindowManager.KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState; KeyDownHandled: Boolean);
var
  Window: TAndroidWindowHandle;
  function HideVKB: Boolean;
  begin
    Result := VirtualKeyboardAndroid.GetVirtualKeyboardState * [TVirtualKeyboardState.Visible] <> [];
    if Result then
    begin
      Key := 0;
      KeyChar := #0;
      Screen.ActiveForm.Focused := nil;
      VirtualKeyboardAndroid.HideVirtualKeyboard
    end
  end;
begin
  Window := FindTopWindowForTextInput;
  if Window <> nil then
    try
      Window.Form.KeyUp(Key, KeyChar, Shift);
    except
      Application.HandleException(Window.Form);
    end;
  // some actions by default
  if KeyDownHandled then // If you press of key was processed
    case Key of
      vkHardwareBack: HideVKB;
    end
  else // If you press of key wasn't processed
    case Key of
      vkHardwareBack:
        begin
          if (not HideVKB) and (Window <> nil) then
          begin
            try
              Key := 0;
              KeyChar := #0;
              Window.Form.Close;
            except
              Application.HandleException(Window.Form);
            end;

            if Application.MainForm <> Window.Form then
            begin
              if (FVisibleStack.Count > 0) and (FVisibleStack.Peek = Window) then
                  FVisibleStack.Pop;
              if (FVisibleStack.Count > 0) and (FVisibleStack.Peek <> nil) then
                BringToFront(FVisibleStack.Peek);
            end;
          end;
        end
    end
end;

procedure TWindowManager.PrepareClosePopups(const SaveForm: TAndroidWindowHandle);
begin
  if Screen <> nil then
  begin
    if SaveForm <> nil then
      Screen.PrepareClosePopups(SaveForm.Form)
    else
      Screen.PrepareClosePopups(nil);
  end;
end;

procedure TWindowManager.ClosePopups;
begin
  if Screen <> nil then
    Screen.ClosePopupForms;
end;

procedure TWindowManager.ContentRectChanged(const NewRect: TRect);
begin
  FNewContentRect := NewRect;
end;

procedure TWindowManager.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Window: TAndroidWindowHandle;
  ClientPoint: TPointF;
  Obj: IControl;
  GObj: IGestureControl;
begin
  Window := FindWindowByPoint(X, Y);
  if Window <> nil then
  begin
    PrepareClosePopups(Window);
    ClientPoint := ScreenToClient(Window, TPointF.Create(X, Y));
    try
      Window.Form.MouseMove([ssTouch], ClientPoint.X, ClientPoint.Y);
      Window.Form.MouseMove([], ClientPoint.X, ClientPoint.Y); // Required for correct IsMouseOver handling
      Window.Form.MouseDown(TMouseButton.mbLeft, Shift, ClientPoint.X, ClientPoint.Y);
    except
      Application.HandleException(Window.Form);
    end;
    // find the control from under the gesture
    Obj := Window.Form.ObjectAtPoint(Window.Form.ClientToScreen(ClientPoint));
    if Obj <> nil then
      FGestureControl := Obj.GetObject
    else
      FGestureControl := Window.Form;

    if FGestureControl is TControl then
      FMouseDownControl := TControl(FGestureControl);
    if FMouseDownControl is TStyledPresentation then
      FMouseDownControl := TStyledPresentation(FMouseDownControl).PresentedControl;

    HideContextMenu;

    if (FGestureControl <> nil) and Supports(FGestureControl, IGestureControl, GObj) then
      FGestureControl := GObj.GetFirstControlWithGestureEngine;

    if (FGestureControl <> nil) and Supports(FGestureControl, IGestureControl, GObj) then
      begin
        TPlatformGestureEngine(GObj.TouchManager.GestureEngine).InitialPoint := ClientPoint;

        // Retain the points/touches.
        TPlatformGestureEngine(GObj.TouchManager.GestureEngine).ClearPoints;
        TPlatformGestureEngine(GObj.TouchManager.GestureEngine).AddPoint(ClientPoint.X, ClientPoint.Y);
      end;
  end;
end;

procedure TWindowManager.MouseMove(Shift: TShiftState; X, Y: Single);
var
  Window: TAndroidWindowHandle;
  ClientPoint: TPointF;
  GObj: IGestureControl;
begin
  if FCapturedWindow <> nil then
    Window := FCapturedWindow
  else
    Window := FindWindowByPoint(X, Y);
  if Window <> nil then
  begin
    ClientPoint := ScreenToClient(Window, TPointF.Create(X, Y));
    try
      Window.Form.MouseMove(Shift, ClientPoint.X, ClientPoint.Y);
    except
      Application.HandleException(Window.Form);
    end;
    if (FGestureControl <> nil) and Supports(FGestureControl, IGestureControl, GObj) then
        if GObj.TouchManager.GestureEngine <> nil then
        begin
          TPlatformGestureEngine(GObj.TouchManager.GestureEngine).AddPoint(ClientPoint.X, ClientPoint.Y);
        end;
  end;
end;

procedure TWindowManager.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoCLick: Boolean);
var
  Window: TAndroidWindowHandle;
  ClientPoint: TPointF;
  EventInfo: TGestureEventInfo;
  GObj: IGestureControl;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
begin

  //when we swipe fastly the finger from one side to another side to finally get out of the screen
  //then the coordinnate are sometime "out of the screen" (ex: x: -5)
  X := min(ContentRect.right - 1, max(ContentRect.Left, X));
  y := min(ContentRect.bottom - 1, max(ContentRect.top, y));

  if FCapturedWindow <> nil then
    Window := FCapturedWindow
  else
    Window := FindWindowByPoint(X, Y);
  if Window <> nil then
  begin
    ClientPoint := ScreenToClient(Window, TPointF.Create(X, Y));
    try
      Window.Form.MouseUp(TMouseButton.mbLeft, Shift, ClientPoint.X, ClientPoint.Y, DoClick);
      if Window.Form <> nil then
        Window.Form.MouseLeave; // Require for correct IsMouseOver handle
      ClosePopups;
    except
      Application.HandleException(Window.Form);
    end;
    if (FGestureControl <> nil) and Supports(FGestureControl, IGestureControl, GObj) then
      if GObj.TouchManager.GestureEngine <> nil then
      begin
        if TPlatformGestureEngine(GObj.TouchManager.GestureEngine).PointCount > 1 then
        begin
          FillChar(EventInfo, Sizeof(EventInfo), 0);
          if TPlatformGestureEngine.IsGesture
            (TPlatformGestureEngine(GObj.TouchManager.GestureEngine).Points,
            TPlatformGestureEngine(GObj.TouchManager.GestureEngine).GestureList, LGestureTypes, EventInfo) then
            TPlatformGestureEngine(GObj.TouchManager.GestureEngine).BroadcastGesture(FGestureControl, EventInfo);
        end;
      end;
  end;
end;

procedure TWindowManager.MultiTouch(const Touches: TTouches; const Action: TTouchAction; const AEnabledGestures: TInteractiveGestures);
var
  Window: TAndroidWindowHandle;
  Control: IControl;
  I: Integer;
begin
  if FCapturedWindow <> nil then
    Window := FCapturedWindow
  else
    Window := FindWindowByPoint(Touches[0].Location.X, Touches[0].Location.Y);
  if Window <> nil then
  begin
    if Length(Touches) = 1 then
      Control := Window.Form.ObjectAtPoint(Touches[0].Location)
    else if Length(Touches) = 2 then
      Control := Window.Form.ObjectAtPoint(Touches[0].Location.MidPoint(Touches[1].Location))
    else
      Control := nil;

    for I := 0 to Length(Touches) - 1 do
      Touches[I].Location := Window.Form.ScreenToClient(Touches[I].Location);

    MultiTouchManager.SetEnabledGestures(AEnabledGestures);
    MultiTouchManager.HandleTouches(Touches, Action, Control);
  end;
end;

procedure TWindowManager.PasteMenuTimerCall;
begin
  DestroyPasteMenuTimer;
  //
  HideContextMenu;
end;

function TWindowManager.PixelToPoint(const P: TPointF): TPointF;
begin
  Result := TPointF.Create(P.X / FScale, P.Y / FScale);
end;

function TWindowManager.PointToPixel(const P: TPointF): TPointF;
begin
  Result := TPointF.Create(P.X * FScale, P.Y * FScale);
end;

procedure TWindowManager.ReleaseCapture;
begin
  FCapturedWindow := nil;
end;

procedure TWindowManager.RemoveWindow(const AHandle: TAndroidWindowHandle);
begin
  if FVisibleStack.Peek = AHandle then
    FVisibleStack.Pop;
  FWindows.Remove(AHandle);
  FMouseDownControl := nil;
  SetFocusedControl(nil);
  FGestureControl := nil;
  SetNeedsRender;
end;

function TWindowManager.AlignToPixel(const Value: Single): Single;
begin
  Result := Round(Value * Scale) / Scale;
end;

procedure TWindowManager.Render;

  procedure ContextInit;
  begin
    eglMakeCurrent(eglGetCurrentDisplay, TCustomAndroidContext.SharedSurface, TCustomAndroidContext.SharedSurface,
      TCustomAndroidContext.SharedContext);

    glViewport(Round(FContentRect.Left * FScale), Round(FContentRect.Top * FScale), Round(FContentRect.Width * FScale),
     Round(FContentRect.Height * FScale));

    glDepthMask(1);
    glClearDepthf(1);
    glClearStencil(0);
    glClearColor(0, 0, 0, 0);
    glClear(GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  end;

  procedure ContextFlip;
  begin
    if eglSwapBuffers(TCustomAndroidContext.SharedDisplay, TCustomAndroidContext.SharedSurface) = 0 then
      glGetError;
  end;

  //I do not need it - more clear code
  (*
  function HaveAnyCompositionedWindows: Boolean;
  var
    I: Integer;
  begin
    for I := 0 to FWindows.Count - 1 do
      if FWindows[I].Form.Visible and FWindows[I].RequiresComposition then
        Exit(True);

    Result := False;
  end;
  *)

  procedure RenderNormalWindows;
  var
    I: Integer;
    PaintControl: IPaintControl;
  begin
    for I := FWindows.Count - 1 downto 0 do
      if FWindows[I].Form.Visible and (not FWindows[I].RequiresComposition) and Supports(FWindows[I].Form,
        IPaintControl, PaintControl) then
      begin
        PaintControl.PaintRects([TRectF.Create(0, 0, FContentRect.Width, FContentRect.Height)]);
        Break;
      end;
  end;

  //I do not need it - more clear code
  (*
  procedure RenderCompositionedWindows;
  var
    I: Integer;
    Mat: TCanvasTextureMaterial;
    Ver: TVertexBuffer;
    Ind: TIndexBuffer;
    CurrentForm: TAndroidWindowHandle;
    FormBounds: TRectF;
    Context: TContext3D;
  begin
    for I := FWindows.Count - 1 downto 0 do
    begin
      CurrentForm := FWindows[I];

      if CurrentForm.RequiresComposition and (CurrentForm.Texture <> nil) and CurrentForm.Form.Visible then
      begin
        FormBounds := CurrentForm.Bounds;
        if CurrentForm.NeedsUpdate then
        begin
          IPaintControl(CurrentForm.Form).PaintRects([RectF(0, 0, FormBounds.Width, FormBounds.Height)]);
          CurrentForm.NeedsUpdate := False;
        end;
      end;
    end;

    Context := TCustomAndroidContext.CreateContextFromActivity(Round(FContentRect.Width * FScale),
      Round(FContentRect.Height * FScale), TMultisample.None, False);
    if Context.BeginScene then
    try
      Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0, TVertexFormat.Color0], 4);
      Ver.Color0[0] := $FFFFFFFF;
      Ver.Color0[1] := $FFFFFFFF;
      Ver.Color0[2] := $FFFFFFFF;
      Ver.Color0[3] := $FFFFFFFF;
      Ver.TexCoord0[0] := PointF(0.0, 1.0);
      Ver.TexCoord0[1] := PointF(1.0, 1.0);
      Ver.TexCoord0[2] := PointF(1.0, 0.0);
      Ver.TexCoord0[3] := PointF(0.0, 0.0);

      Ind := TIndexBuffer.Create(6);
      Ind[0] := 0;
      Ind[1] := 1;
      Ind[2] := 3;
      Ind[3] := 3;
      Ind[4] := 1;
      Ind[5] := 2;

      Mat := TCanvasTextureMaterial.Create;

      for I := FWindows.Count - 1 downto 0 do
      begin
        CurrentForm := FWindows[I];

        if CurrentForm.RequiresComposition and (CurrentForm.Texture <> nil) and CurrentForm.Form.Visible then
        begin
          FormBounds := CurrentForm.Bounds;
          FormBounds.Offset(-FContentRect.Left, -FContentRect.Top);

          FormBounds.Left := Round(FormBounds.Left * Scale);
          FormBounds.Top := Round(FormBounds.Top * Scale);
          FormBounds.Right := FormBounds.Left + CurrentForm.Texture.Width;
          FormBounds.Bottom := FormBounds.Top + CurrentForm.Texture.Height;

          Ver.Vertices[0] := TPoint3D.Create(FormBounds.Left, FormBounds.Top, 0);
          Ver.Vertices[1] := TPoint3D.Create(FormBounds.Right, FormBounds.Top, 0);
          Ver.Vertices[2] := TPoint3D.Create(FormBounds.Right, FormBounds.Bottom, 0);
          Ver.Vertices[3] := TPoint3D.Create(FormBounds.Left, FormBounds.Bottom, 0);

          Mat.Texture := CurrentForm.Texture;

          Context.SetMatrix(TMatrix3D.Identity);
          Context.SetContextState(TContextState.cs2DScene);
          Context.SetContextState(TContextState.csZWriteOff);
          Context.SetContextState(TContextState.csZTestOff);
          Context.SetContextState(TContextState.csAllFace);
          Context.SetContextState(TContextState.csScissorOff);
          if CurrentForm.Form.Transparency then
            Context.SetContextState(TContextState.csAlphaBlendOn)
          else
            Context.SetContextState(TContextState.csAlphaBlendOff);

          Context.DrawTriangles(Ver, Ind, Mat, 1);
        end;
      end;

      Mat.Free;
      Ind.Free;
      Ver.Free;
    finally
      Context.EndScene;
    end;
    Context.Free;
  end;
  *)

begin
  if TCustomAndroidContext.IsContextAvailable and not FPause then
  begin
    ContextInit;
    try
      // Render normal opaque windows that are occupying entire client space.
      RenderNormalWindows;

      // If there are any visible popups or transparent windows, render them using buffered texture.
      //I do not need it - more clear code
      (*
      if HaveAnyCompositionedWindows then
        RenderCompositionedWindows;
      *)
    finally
      ContextFlip;
    end;
  end;
  FNeedsRender := False;
end;

// https://quality.embarcadero.com/browse/RSP-19238
function TWindowManager.RenderIfNeeds: Boolean;
begin
  Result := FNeedsRender;
  if FNeedsRender then begin
    Render;
    FNeedsRender2 := True;
  end
  else if FNeedsRender2 then begin
    Render;
    FNeedsRender2 := False;
  end;
end;

procedure TWindowManager.RenderImmediately;
begin
  SetNeedsRender;
  RenderIfNeeds;
end;

procedure TWindowManager.SetCapture(const AForm: TAndroidWindowHandle);
begin
  FCapturedWindow := AForm;
end;

procedure TWindowManager.SetFocusedControl(const Control: IControl);
begin
  if FFocusedControl <> nil then
    FFocusedControl.RemoveFreeNotify(Self);
  FFocusedControl := Control;
  if FFocusedControl <> nil then
    FFocusedControl.AddFreeNotify(Self);
end;

procedure TWindowManager.SetNeedsRender;
begin
  FNeedsRender := True;
end;

procedure TWindowManager.SetPause(const Value: Boolean);
begin
  FPause := Value;
end;

procedure TWindowManager.ShowContextMenu(const ItemsToShow: TContextMenuItems);
var
  LA: TTextLayout;
  P: TPoint;
  HasSelection, HasClipboard: Boolean;
  ApproxWidth: Integer;
  ApproxHeight: Integer;
  ClipboardValue: TValue;
  ResID: Integer;
  TextInput: ITextInput;
  VirtualKeyboard: IVirtualKeyboardControl;
  ClipboardSvc: IFMXClipboardService;
begin
  DestroyPasteMenuTimer;
  ApproxWidth := FContextMenuPopupSize.cx;
  ApproxHeight := FContextMenuPopupSize.cy;
  if not FContextMenuVisible and Supports(FFocusedControl, ITextInput, TextInput) and not FSelectionInProgress then
  begin
    FContextMenuVisible := True;
    HasSelection := not TextInput.GetSelection.IsEmpty;
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipboardSvc);
    ClipboardValue := ClipboardSvc.GetClipboard;
    HasClipboard := not ClipboardValue.IsEmpty and not ClipboardValue.ToString.IsEmpty;
    if FContextMenuPopup = nil then
    begin
      FContextMenuLayout := TJLinearLayout.JavaClass.init(TAndroidHelper.Activity);
      FContextButtonsLayout := TJLinearLayout.JavaClass.init(TAndroidHelper.Activity);

      LA := TTextLayoutManager.DefaultTextLayout.Create;
      LA.Font.Style := LA.Font.Style + [TFontStyle.fsBold];

      P := Point(0, 0);
      Supports(FFocusedControl, IVirtualKeyboardControl, VirtualKeyboard);

      if HasSelection then
      begin
        //Copy button
        if (TContextMenuItem.Copy in ItemsToShow) and ((VirtualKeyboard = nil) or not VirtualKeyboard.IsPassword) then
        begin
          ResID := TAndroidHelper.GetResourceID('android:string/copy');
          if ResID <> 0 then
            LA.Text := TAndroidHelper.GetResourceString(ResID)
          else
            LA.Text := SEditCopy.ToUpper;
          FCopyButton := TJButton.JavaClass.init(TAndroidHelper.Activity);
          if ResID <> 0 then
            FCopyButton.setText(ResID)
          else
            FCopyButton.setText(StrToJCharSequence(LA.Text));
          FCopyButton.setTypeface(TJTypeface.JavaClass.DEFAULT_BOLD);
          FCopyClickListener := TCopyButtonClickListener.Create;
          FCopyButton.setOnClickListener(FCopyClickListener);
          LA.Font.Size := FCopyButton.getTextSize;
          P.X := P.X + Ceil((LA.TextWidth + 2) * FScale);
          P.Y := Max(P.Y, Ceil((LA.TextHeight + 2) * FScale));
          ApproxHeight := P.Y + FCopyButton.getPaddingTop + FCopyButton.getPaddingBottom;
        end;
        //Cut button
        if (TContextMenuItem.Cut in ItemsToShow) and not TextReadOnly and ((VirtualKeyboard = nil) or not VirtualKeyboard.IsPassword) then
        begin
          ResID := TAndroidHelper.GetResourceID('android:string/cut');
          if ResID <> 0 then
            LA.Text := TAndroidHelper.GetResourceString(ResID)
          else
            LA.Text := SEditCut.ToUpper;
          FCutButton := TJButton.JavaClass.init(TAndroidHelper.Activity);
          if ResID <> 0 then
            FCutButton.setText(ResID)
          else
            FCutButton.setText(StrToJCharSequence(LA.Text));
          FCutButton.setTypeface(TJTypeface.JavaClass.DEFAULT_BOLD);
          FCutClickListener := TCutButtonClickListener.Create;
          FCutButton.setOnClickListener(FCutClickListener);
          LA.Font.Size := FCopyButton.getTextSize;
          P.X := P.X + Ceil((LA.TextWidth + 2) * FScale);
          P.Y := Max(P.Y, Ceil((LA.TextHeight + 2) * FScale));
        end;
      end;

      if HasClipboard and (TContextMenuItem.Paste in ItemsToShow) and not TextReadOnly then
      begin
        //Paste button
        ResID := TAndroidHelper.GetResourceID('android:string/paste');
        if ResID <> 0 then
          LA.Text := TAndroidHelper.GetResourceString(ResID)
        else
          LA.Text := SEditPaste.ToUpper;
        FPasteButton := TJButton.JavaClass.init(TAndroidHelper.Activity);
        if ResID <> 0 then
          FPasteButton.setText(ResID)
        else
          FPasteButton.setText(StrToJCharSequence(LA.Text));
        FPasteButton.setTypeface(TJTypeface.JavaClass.DEFAULT_BOLD);
        FPasteClickListener := TPasteButtonClickListener.Create;
        FPasteButton.setOnClickListener(FPasteClickListener);
        LA.Font.Size := FPasteButton.getTextSize;
        P.X := P.X + Ceil((LA.TextWidth + 2) * FScale);
        P.Y := Max(P.Y, Ceil((LA.TextHeight + 2) * FScale));
        if ApproxHeight = 0 then
          ApproxHeight := P.Y + FPasteButton.getPaddingTop + FPasteButton.getPaddingBottom;
      end;

      ApproxWidth := P.X;

      FContextMenuPopup := TJPopupWindow.JavaClass.init(TAndroidHelper.Activity);
      FContextMenuPopup.setBackgroundDrawable(TJColorDrawable.JavaClass.init(0));

      FContextMenuPopup.setContentView(FContextButtonsLayout);
      FContextMenuPopup.setWidth(TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT);
      FContextMenuPopup.setHeight(TJViewGroup_LayoutParams.JavaClass.WRAP_CONTENT);
    end;

    FContextMenuPopupSize.cx := ApproxWidth;
    if FContextMenuPopupSize.cy <= 0 then
    begin
      FContextMenuPopupSize.cy := ApproxHeight;
    end;

    PlatformAndroid.SynchronizeOnUIThread(
      procedure
      begin
        if FCopyButton <> nil then
          FContextButtonsLayout.addView(FCopyButton);
        if FCutButton <> nil then
          FContextButtonsLayout.addView(FCutButton);
        if FPasteButton <> nil then
          FContextButtonsLayout.addView(FPasteButton);
        if (VirtualKeyboardAndroid <> nil) and (TVirtualKeyboardState.Visible in
          VirtualKeyboardAndroid.VirtualKeyboardState) then
          DoShowContextMenu;
      end);
  end;
end;

procedure TWindowManager.DoShowContextMenu;
var
  SelRect: TRectF;
  PopupLoc: TPoint;
  TempPointF: TPointF;
  TextInput: ITextInput;
begin
  if Supports(FFocusedControl, ITextInput, TextInput) and (FContextMenuLayout <> nil) and (FContextMenuPopup <> nil) and
    (TWindowManager.Current.FContextButtonsLayout.getChildCount > 0) then
  begin
    SelRect := TextInput.GetSelectionRect;
    TempPointF := FFocusedControl.LocalToScreen(SelRect.TopLeft) * FScale;

    TempPointF.Offset(SelRect.Width * FScale / 2 - FContextMenuPopupSize.cx / 2, -FContextMenuPopupSize.cy);
    PopupLoc := TempPointF.Round;
    if PopupLoc.Y < StatusBarHeight then
    begin
      TempPointF := FFocusedControl.LocalToScreen(TPointF.Create(SelRect.Left, SelRect.Bottom)) * FScale;
      TempPointF.Offset(SelRect.Width * FScale / 2 - FContextMenuPopupSize.cx / 2, FContextMenuPopupSize.cy);
      PopupLoc := TempPointF.Round;
    end;
    FContextMenuPopup.showAtLocation(FContextMenuLayout, 0, PopupLoc.X, PopupLoc.Y);
  end
  else
    HideContextMenu;
end;

procedure TWindowManager.VKStateHandler(const Sender: TObject; const M: TMessage);
begin
  if TVKStateChangeMessage(M).KeyboardVisible and Supports(FFocusedControl, ITextInput) and (FContextMenuLayout <> nil) and
    (FContextMenuPopup <> nil) and (TWindowManager.Current.FContextButtonsLayout.getChildCount > 0) then
    PlatformAndroid.SynchronizeOnUIThread(
      procedure
      begin
        DoShowContextMenu;
      end);
end;

procedure TWindowManager.SingleTap;
begin
  if FMouseDownControl <> nil then
    try
      if not FSelectionInProgress then
      begin
        if (FMouseDownControl <> nil) and Supports(FMouseDownControl, ITextInput) and
          Supports(FMouseDownControl, ITextActions) then
        begin
          SetFocusedControl(FMouseDownControl);
          ShowContextMenu([TContextMenuItem.Paste]);
          CreatePasteMenuTimer;
        end
        else
          HideContextMenu;
      end;
    except
      Application.HandleException(Self);
      FMouseDownControl := nil;
    end;
end;

procedure TWindowManager.HideContextMenu;
begin
  DestroyPasteMenuTimer;
  if FContextMenuVisible and (FContextMenuPopup <> nil) and (FContextButtonsLayout <> nil) then
  begin
    PlatformAndroid.SynchronizeOnUIThread(
      procedure begin
        if FContextMenuVisible and (FContextButtonsLayout <> nil) and (FContextMenuPopup <> nil) then
        begin
          FContextMenuPopupSize.cx := FContextButtonsLayout.getWidth;
          FContextMenuPopupSize.cy := FContextButtonsLayout.getHeight;
          FContextMenuPopup.dismiss;
        end;
      end);
    FContextMenuPopup := nil;
    FCopyButton := nil;
    FCopyClickListener := nil;
    FPasteButton := nil;
    FPasteClickListener := nil;
    FCutButton := nil;
    FCutClickListener := nil;
    FContextMenuLayout := nil;
    FContextButtonsLayout := nil;
  end;
  FContextMenuVisible := False;
end;

//https://quality.embarcadero.com/browse/RSP-12782
function TWindowManager.RetrieveContentRect: TRect;
var
  Activity: JActivity;
  NativeWin: JWindow;
  DecorView: JView;
  ContentRect: JRect;
  ContentRectVisible: JRect;
begin
  Activity := TAndroidHelper.Activity;
  if Activity <> nil then
  begin
    NativeWin := Activity.getWindow;
    if NativeWin <> nil then
    begin
      
      ContentRect := TJRect.Create;
      DecorView := NativeWin.getDecorView;
      DecorView.getDrawingRect(ContentRect);

      ContentRectVisible := TJRect.Create;
      DecorView.getWindowVisibleDisplayFrame(ContentRectVisible);
      FNewContentRect.Top := ContentRectVisible.Top;
      FStatusBarHeight := FNewContentRect.top;

      Result := TRect.Create(Round(FNewContentRect.left / FScale), Round(FNewContentRect.top / FScale),
        Round(ContentRect.right / FScale), Round(ContentRect.bottom / FScale));
    end;
  end;
end;

procedure TWindowManager.UpdateContentRect;
begin
  FContentRect := RetrieveContentRect;
end;

procedure TWindowManager.UpdateFormSizes;
var
  I: Integer;
  StaticWindowList: TList<TAndroidWindowHandle>;
begin
  StaticWindowList := TList<TAndroidWindowHandle>.Create;
  try
    for I := FWindows.Count - 1 downto 0 do
      if not FWindows[I].IsPopup then
        StaticWindowList.Add(FWindows[I]);

    for I := StaticWindowList.Count - 1 downto 0 do
    begin
      StaticWindowList[I].SetBounds(TRectF.Create(FContentRect));
      try
        StaticWindowList[I].Form.SetBounds(FContentRect.Left, FContentRect.Top, FContentRect.Width, FContentRect.Height);
      except
        Application.HandleException(StaticWindowList[I].Form);
      end;
    end;
    if MultiDisplayAndroid <> nil then
      MultiDisplayAndroid.UpdateDisplayInformation;
  finally
    StaticWindowList.Free;
  end;
end;

function TWindowManager.CheckContentRectUpdate: Boolean;
var
  NewRect: TRect;
begin
  NewRect := RetrieveContentRect;
  if NewRect <> FContentRect then
  begin
    FContentRect := NewRect;

    UpdateFormSizes;
    SetNeedsRender;
    Result := True;
  end
  else
    Result := False;
end;

procedure OnContentRectChanged(Activity: PANativeActivity; Rect: PARect); cdecl;
begin
  TWindowManager.Current.ContentRectChanged(TRect.Create(Rect.left, Rect.top, Rect.right, Rect.bottom));
  TThread.Queue(nil, procedure begin
    PlatformAndroid.FContentRectMightHaveChanged := TPlatformAndroid.ContentRectChangeRefreshCount;
  end);
end;

{ TPlatformAndroid }

constructor TPlatformAndroid.Create;
begin
  inherited;
  ALAniCalcTimerProcs := Tlist<TALAniCalculations>.create; // added to support ALFmxInertialMovement
  FMotionEvents := TMotionEvents.Create;
  Application := TApplication.Create(nil);
  FCanSetState := True;
  PlatformAndroid := Self;
  TWindowManager.Current;
  TTimerManager.Current;
  FFirstRun := True;
  FLogPrefix := 'FMX: ' + TAndroidHelper.ApplicationTitle + ': ';
  FActivityListener := TFMXNativeActivityListener.Create;
  TJFMXNativeActivity.Wrap(PANativeActivity(System.DelphiActivity)^.clazz).setListener(FActivityListener);
  PANativeActivity(System.DelphiActivity)^.callbacks^.onContentRectChanged := OnContentRectChanged;
  if TOSVersion.Check(3, 0) then
    FKeyCharacterMap := TJKeyCharacterMap.JavaClass.load(TJKeyCharacterMap.JavaClass.VIRTUAL_KEYBOARD)
  else
    FKeyCharacterMap := TJKeyCharacterMap.JavaClass.load(TJKeyCharacterMap.JavaClass.BUILT_IN_KEYBOARD);
  FInternalEventQueue := TQueue<TThreadProcedure>.Create;
  FEventQueueEmptyEvent := TEvent.Create;
  FSkipEventQueue := TQueue<JKeyEvent>.Create;
  FLastOrientation := UndefinedOrientation;
  FLastOrientationTime := GetTick;
  if DeviceServiceAndroid.GetModel = 'Glass 1' then
    TPlatformServices.Current.GlobalFlags.Add(EnableGlassFPSWorkaround, True);
  FAlertListeners := TFMXDialogListenerParentList.Create;
  RegisterWakeMainThread;
  //I do not need it - more clear code
  (*
  FTextServiceUpdater := TTextServiceUpdater.Create;
  *)
  FKeyMapping := TKeyMapping.Create;
end;

destructor TPlatformAndroid.Destroy;
begin
  if FAndroidApp <> nil then
  begin
    FAndroidApp^.onAppCmd := nil;
    FAndroidApp^.onInputEvent := nil;
  end;
  FActivityListener.DisposeOf;
  DestroyDoubleTapTimer;
  DestroyLongTapTimer;
  DestroySingleTapTimer;
  FInternalEventQueue := nil;
  FMotionEvents.Free;
  FSkipEventQueue.Free;
  FAlertListeners.Free;
  UnregisterWakeMainThread;
  //I do not need it - more clear code
  (*
  FTextServiceUpdater.Free;
  *)
  FKeyMapping.Free;
  ALAniCalcTimerProcs.Free; // added to support ALFmxInertialMovement
  inherited;
end;

procedure TPlatformAndroid.CreateDoubleTapTimer;
begin
  if FDoubleTapTimer = 0 then
    FDoubleTapTimer := TTimerManager.Current.CreateTimer(DBL_TAP_DELAY, DoubleTapTimerCall);
end;

function TPlatformAndroid.CreateGestureEventInfo(ASecondPointer: TPointF; const AGesture: TInteractiveGesture;
  const AGestureEnded: Boolean = False): TGestureEventInfo;
begin
  FillChar(Result, Sizeof(Result), 0);
  Result.Location := FMouseCoord;
  Result.GestureID := igiZoom + Ord(AGesture);

  if not(AGesture in FActiveInteractiveGestures) then
    Result.Flags := [TInteractiveGestureFlag.gfBegin];
  if AGestureEnded then
    Result.Flags := [TInteractiveGestureFlag.gfEnd];

  if AGesture = TInteractiveGesture.LongTap then
    Result.Location := FMouseDownCoordinates;
end;

function TPlatformAndroid.GetMousePos: TPointF;
begin
  Result := FMouseCoord;
end;

procedure TPlatformAndroid.CreateLongTapTimer;
begin
  if FLongTapTimer = 0 then
    FLongTapTimer := TTimerManager.Current.CreateTimer(LONG_TAP_DURATION, LongTapTimerCall);
end;

procedure TPlatformAndroid.CreateSingleTapTimer;
begin
  if FSingleTapTimer = 0 then
    FSingleTapTimer := TTimerManager.Current.CreateTimer(SINGLE_TAP_DELAY, SingleTapTimerCall);
end;

procedure TPlatformAndroid.DestroyDoubleTapTimer;
begin
  if FDoubleTapTimer <> 0 then
    TTimerManager.Current.DestroyTimer(FDoubleTapTimer);
  FDoubleTapTimer := 0;
  FDblClickFirstMouseUp := False;
end;

procedure TPlatformAndroid.DestroyLongTapTimer;
begin
  if FLongTapTimer <> 0 then
    TTimerManager.Current.DestroyTimer(FLongTapTimer);
  FLongTapTimer := 0;
end;

procedure TPlatformAndroid.DestroySingleTapTimer;
begin
  if FSingleTapTimer <> 0 then
    TTimerManager.Current.DestroyTimer(FSingleTapTimer);
  FSingleTapTimer := 0;
  FSingletap := False;
end;

procedure TPlatformAndroid.Run;
begin
  { Although calling this routine is not really necessary, but it is a way to ensure that "Androidapi.AppGlue.pas" is
    kept in uses list, in order to export ANativeActivity_onCreate callback. }
  app_dummy;

  repeat
    InternalProcessMessages;
  until FAndroidApp^.destroyRequested <> 0;
end;

function TPlatformAndroid.Terminating: Boolean;
begin
  Result := FTerminating;
end;

procedure TPlatformAndroid.Terminate;
begin
  TThread.CreateAnonymousThread(procedure
    begin
      TThread.Queue(nil, procedure
        begin
          TPurgatoryManager.DisablePurgatory;
          FTerminating := True;
          TTimerManager.Current.KillAllTimers;
          // When we manually finish our activity, android will not generate OnSaveInstanceState event, because it is generated
          // only in case, when system should kill our activity for reclaim resources. In current case we initiate correct
          // closing our application, so we have to invoke OnSaveInstanceState manually. Because in FireMonket we always
          // invoke TForm.OnSaveState every time, when application is closed.
          HandleAndroidCmd(APP_CMD_SAVE_STATE);
          ANativeActivity_finish(System.DelphiActivity);
        end);
    end).Start;
end;

procedure TPlatformAndroid.HandleAndroidCmd(ACmd: Int32);
begin
  case ACmd of
    APP_CMD_INPUT_CHANGED: ;

    APP_CMD_INIT_WINDOW:
      begin
        if FFirstRun then
        begin
          Application.RealCreateForms;
          FFirstRun := False;
          HandleApplicationEvent(TApplicationEvent.FinishedLaunching, nil);
        end
        else
        begin
          TCustomAndroidContext.UnfreezeSharedContext;
          Include(FAppCmdStates, TAndroidAppCmdState.InitWindow);
        end;
        MainActivity.applicationActivated;
        FContentRectMightHaveChanged := ContentRectChangeRefreshCount;
        FOrientationMightHaveChanged := True;
      end;

    APP_CMD_TERM_WINDOW:
      begin
        MainActivity.applicationDeactivated;
        TCustomAndroidContext.FreezeSharedContext;
      end;

    APP_CMD_WINDOW_RESIZED: ;

    APP_CMD_WINDOW_REDRAW_NEEDED: ;

    APP_CMD_CONTENT_RECT_CHANGED: ;

    APP_CMD_GAINED_FOCUS:
      begin
        if ([TAndroidAppCmdState.LostFocus, TAndroidAppCmdState.ConfigChanged] <= FAppCmdStates) and
          not (TAndroidAppCmdState.InitWindow in FAppCmdStates) then
          TCustomAndroidContext.RecreateSharedContext;
        Exclude(FAppCmdStates, TAndroidAppCmdState.LostFocus);
        TWindowManager.Current.RenderImmediately;
        HandleApplicationEvent(TApplicationEvent.BecameActive, nil);
      end;

    APP_CMD_LOST_FOCUS:
      begin
        if not TWindowManager.Current.Pause then HandleApplicationEvent(TApplicationEvent.WillBecomeInactive, nil); // << https://quality.embarcadero.com/browse/RSP-18686
        FAppCmdStates := [TAndroidAppCmdState.LostFocus];
      end;

    APP_CMD_CONFIG_CHANGED:
      begin
        FContentRectMightHaveChanged := ContentRectChangeRefreshCount;
        FOrientationMightHaveChanged := True;
        Include(FAppCmdStates, TAndroidAppCmdState.ConfigChanged);
      end;

    APP_CMD_LOW_MEMORY:
      HandleApplicationEvent(TApplicationEvent.LowMemory, nil);

    APP_CMD_START: ;

    APP_CMD_RESUME:
      begin
        TWindowManager.Current.Pause := False;
        HandleApplicationEvent(TApplicationEvent.WillBecomeForeground, nil);
        if not (TAndroidAppCmdState.LostFocus in FAppCmdStates) then HandleApplicationEvent(TApplicationEvent.BecameActive, nil); // << https://quality.embarcadero.com/browse/RSP-18686
      end;

    APP_CMD_SAVE_STATE:
      TMessageManager.DefaultManager.SendMessage(Self, TSaveStateMessage.Create);

    APP_CMD_PAUSE:
      begin
        TWindowManager.Current.Pause := True;
        if not (TAndroidAppCmdState.LostFocus in FAppCmdStates) then HandleApplicationEvent(TApplicationEvent.WillBecomeInactive, nil); // << https://quality.embarcadero.com/browse/RSP-18686
        HandleApplicationEvent(TApplicationEvent.EnteredBackground, nil);
      end;

    APP_CMD_STOP: ;

    APP_CMD_DESTROY:
      HandleApplicationEvent(TApplicationEvent.WillTerminate, nil);
  end;
end;

function TPlatformAndroid.ShiftStateFromMetaState(const AMetaState: Integer): TShiftState;
begin
  Result := [];
  if (AMetaState and AMETA_SHIFT_ON) > 0 then
    Result := Result + [ssShift];
  if (AMetaState and AMETA_ALT_ON) > 0 then
    Result := Result + [ssAlt];
end;

function TPlatformAndroid.HandleAndroidKeyEvent(AEvent: PAInputEvent): Int32;
var
  KeyCode, vkKeyCode: Word;
  Action, MetaState: Integer;
  KeyChar: Char;
  KeyEvent: JKeyEvent;
  KeyEventChars: string;
  C: WideChar;
  SkipEvent: JKeyEvent;
  LKeyDownHandled: Boolean;
  KeyKind: TKeyKind;
  EventTime, DownTime: Int64;
  DeviceId: Integer;
begin
  Result := 0;

  Action := AKeyEvent_getAction(AEvent);
  KeyCode := AKeyEvent_getKeyCode(AEvent);
  MetaState := AKeyEvent_getMetaState(AEvent);
  EventTime := AKeyEvent_getEventTime(AEvent) div 1000000;
  DownTime := AKeyEvent_getDownTime(AEvent) div 1000000;
  DeviceId := AInputEvent_getDeviceId(AEvent);

  if FSkipEventQueue.Count > 0 then
    SkipEvent := FSkipEventQueue.Peek
  else
    SkipEvent := nil;

  if (SkipEvent <> nil) and ((SkipEvent.getEventTime < EventTime) or (SkipEvent.getDownTime < DownTime)) then
  begin
    SkipEvent := nil;
    FSkipEventQueue.Dequeue;
  end;

  if (SkipEvent = nil) or (SkipEvent.getAction <> Action) or (SkipEvent.getFlags <> AKeyEvent_getFlags(AEvent)) or
    (SkipEvent.getKeyCode <> KeyCode) or (SkipEvent.getMetaState <> MetaState) or (SkipEvent.getEventTime <> EventTime) or
    (SkipEvent.getDownTime <> DownTime) then
  begin
    KeyChar := #0;

    vkKeyCode := PlatformKeyToVirtualKey(KeyCode, KeyKind);
    if (vkKeyCode <> 0) and (KeyKind <> TKeyKind.Usual) then
    begin
      KeyCode := vkKeyCode;
      if KeyCode in [vkEscape] then
        KeyChar := Char(KeyCode);
    end
    else
    begin
      if FKeyCharacterMap <> nil then
      begin
        KeyChar := Char(ObtainKeyCharacterMap(DeviceId).get(KeyCode, MetaState));
        if KeyChar <> #0 then
          KeyCode := 0
        else
          KeyCode := vkKeyCode;
      end;
    end;

    case AKeyEvent_getAction(AEvent) of
      AKEY_EVENT_ACTION_DOWN:
        begin
          if (KeyCode = vkHardwareBack) and (KeyChar = #0) and (VirtualKeyboardAndroid <> nil) and
            (VirtualKeyboardAndroid.VirtualKeyboardState * [TVirtualKeyboardState.Visible] <> []) then
          begin
            FDownKey := 0;
            FDownKeyChar := #0;
          end
          else
          begin
            FDownKey := KeyCode;
            FDownKeyChar := KeyChar;
            TWindowManager.Current.KeyDown(KeyCode, KeyChar, ShiftStateFromMetaState(MetaState));
          end;
          FKeyDownHandled := (KeyCode = 0) and (KeyChar = #0);
          if FKeyDownHandled then
            Result := 1;
        end;
      AKEY_EVENT_ACTION_UP:
        begin
          LKeyDownHandled := (FDownKey = KeyCode) and (FDownKeyChar = KeyChar) and FKeyDownHandled;
          TWindowManager.Current.KeyUp(KeyCode, KeyChar, ShiftStateFromMetaState(MetaState), LKeyDownHandled);
          if (KeyCode = 0) and (KeyChar = #0) then
            Result := 1; // indicate that we have handled the event
        end;
      AKEY_EVENT_ACTION_MULTIPLE:
        begin
          KeyEvent := JFMXNativeActivity(MainActivity).getLastEvent;
          if KeyEvent <> nil then
          begin
            KeyEventChars := JStringToString(KeyEvent.getCharacters);
            KeyCode := 0;
            for C in KeyEventChars do
            begin
              FDownKey := KeyCode;
              FDownKeyChar := C;
              TWindowManager.Current.KeyDown(KeyCode, FDownKeyChar, ShiftStateFromMetaState(MetaState));
              FKeyDownHandled := (KeyCode = 0) and (FDownKeyChar = #0);
            end;
            Result := 1;
          end;
        end;
    end;
  end
  else
    FSkipEventQueue.Dequeue;
end;

function TPlatformAndroid.ObtainKeyCharacterMap(DeviceId: Integer): JKeyCharacterMap;
begin
  Result := TJKeyCharacterMap.JavaClass.load(DeviceId)
end;

procedure TPlatformAndroid.ProcessAndroidGestureEvents;
var
  SecondPointer: TPointF;
begin
  if FMotionEvents.Count < 1 then
    Exit;

  FMouseCoord := FMotionEvents[0].Position;

  if FMotionEvents.Count > 1 then
    SecondPointer := FMotionEvents[1].Position
  else
    SecondPointer := TPointF.Zero;

  case (FMotionEvents[0].EventAction and AMOTION_EVENT_ACTION_MASK) of
    AMOTION_EVENT_ACTION_DOWN:
      begin
        if FSingleTapTimer <> 0 then
          DestroySingleTapTimer
        else
          FSingleTap := True;

        if FDoubleTapTimer = 0 then
          if (TInteractiveGesture.DoubleTap in FEnabledInteractiveGestures) and (FMotionEvents.Count = 1) then
            CreateDoubleTapTimer;

        if (TInteractiveGesture.LongTap in FEnabledInteractiveGestures)  and (FMotionEvents.Count = 1) then
          CreateLongTapTimer;
      end;
    AMOTION_EVENT_ACTION_UP:
      begin
        if FSingleTap then
          CreateSingleTapTimer;

        if FDoubleTapTimer <> 0 then
          if not FDblClickFirstMouseUp then
            FDblClickFirstMouseUp := True
          else
          begin
            DestroyDoubleTapTimer;
            FDblClickFirstMouseUp := False;
            TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(TPointF.Zero,
              TInteractiveGesture.DoubleTap));
          end;

        //stop longtap timer
        DestroyLongTapTimer;
      end;
    AMOTION_EVENT_ACTION_MOVE:
      begin
        // Stop longtap and double tap timers only if the coordinates did not change (much) since the last event.
        // Allow for some slight finger movement.
        if FMotionEvents.Count = 1 then
        begin
          if FMouseCoord.Distance(FMouseDownCoordinates) > GetLongTapAllowedMovement then
          begin
            DestroySingleTapTimer;
          end;

          if FMouseCoord.Distance(FOldPoint1) > GetLongTapAllowedMovement then
          begin
            DestroyLongTapTimer;
            DestroyDoubleTapTimer;
          end;

          FOldPoint2 := TPointF.Zero;
        end;
      end;
    AMOTION_EVENT_ACTION_CANCEL:
      begin
        DestroyLongTapTimer;
        DestroyDoubleTapTimer;
        DestroySingleTapTimer;
        FActiveInteractiveGestures := [];
        FRotationAngle := 0;
        FOldPoint1 := TPointF.Zero;
        FOldPoint2 := TPointF.Zero;
        FMouseDownCoordinates := TPointF.Zero;
      end;
    AMOTION_EVENT_ACTION_POINTER_DOWN:
      begin
        //stop timers
        DestroyLongTapTimer;
        DestroyDoubleTapTimer;
        DestroySingleTapTimer;
        if FMotionEvents.Count = 2 then
          FOldPoint2 := SecondPointer;
      end;
    AMOTION_EVENT_ACTION_POINTER_UP:
      begin
        //from 2 pointers now, there will be only 1 pointer
        if FMotionEvents.Count = 2 then
          FOldPoint2 := TPointF.Zero;
      end;
  end;

  FOldPoint1 := FMotionEvents[0].Position;
  FOldPoint2 := SecondPointer;
end;

procedure TPlatformAndroid.ProcessAndroidMouseEvents;
var
  MotionEvent: TMotionEvent;
begin
  if FMotionEvents.Count > 0 then
  begin
    MotionEvent := FMotionEvents[0];

    case MotionEvent.EventAction of
      AMOTION_EVENT_ACTION_DOWN:
      begin
        TWindowManager.Current.MouseDown(TMouseButton.mbLeft, MotionEvent.Shift, MotionEvent.Position.X, MotionEvent.Position.Y);
        FMouseDownCoordinates := MotionEvent.Position;
      end;

      AMOTION_EVENT_ACTION_UP:
      begin
        TWindowManager.Current.MouseUp(TMouseButton.mbLeft, MotionEvent.Shift, MotionEvent.Position.X, MotionEvent.Position.Y, not FGestureEnded);
        FGestureEnded := False;
      end;

      AMOTION_EVENT_ACTION_MOVE:
        TWindowManager.Current.MouseMove(MotionEvent.Shift, MotionEvent.Position.X, MotionEvent.Position.Y);
    end;

    FMouseCoord := MotionEvent.Position;
  end;
end;

function TPlatformAndroid.HandleAndroidMotionEvent(AEvent: PAInputEvent): Int32;
var
  I: Integer;
  MotionEvent: TMotionEvent;
begin
  Result := 0;

  FMotionEvents.Clear;
  for I := 0 to AMotionEvent_getPointerCount(AEvent) - 1 do
  begin
    MotionEvent.EventAction := AKeyEvent_getAction(AEvent) and AMOTION_EVENT_ACTION_MASK;
    MotionEvent.Position := TWindowManager.Current.PixelToPoint(TPointF.Create(AMotionEvent_getX(AEvent, I),
      AMotionEvent_getY(AEvent, I)));
    MotionEvent.Shift := [ssLeft];
    if AInputEvent_getType(AEvent) <> AINPUT_SOURCE_MOUSE then
      Include(MotionEvent.Shift, ssTouch);
    FMotionEvents.Add(MotionEvent);
  end;

  HandleMultiTouch;

  if (FActiveInteractiveGestures = []) or (FActiveInteractiveGestures = [TInteractiveGesture.Pan]) then
    ProcessAndroidMouseEvents;
  ProcessAndroidGestureEvents;
end;

function TPlatformAndroid.HandleAndroidInputEvent(AEvent: PAInputEvent): Int32;
var
  EventType: Int64;
begin
  EventType := AInputEvent_getType(AEvent);

  if EventType = AINPUT_EVENT_TYPE_KEY then
    // Keyboard input
    Result := HandleAndroidKeyEvent(AEvent)
  else if EventType = AINPUT_EVENT_TYPE_MOTION then
    // Motion Event
    Result := HandleAndroidMotionEvent(AEvent)
  else
    Result := 0;
end;

function TPlatformAndroid.HandleMessage: Boolean;
begin
  InternalProcessMessages;
  Result := False;
end;

procedure TPlatformAndroid.HandleMultiTouch;
var
  Touches: TTouches;
  TouchAction: TTouchAction;
  I: Integer;
begin
  if FMotionEvents.Count > 0 then
  begin
    SetLength(Touches, FMotionEvents.Count);
    for I := 0 to FMotionEvents.Count - 1 do
      Touches[I].Location := FMotionEvents[I].Position;

    case FMotionEvents[0].EventAction of
      AMOTION_EVENT_ACTION_DOWN:
        TouchAction := TTouchAction.Down;
      AMOTION_EVENT_ACTION_UP:
        TouchAction := TTouchAction.Up;
      AMOTION_EVENT_ACTION_MOVE:
        TouchAction := TTouchAction.Move;
      AMOTION_EVENT_ACTION_CANCEL:
        TouchAction := TTouchAction.Cancel;
      AMOTION_EVENT_ACTION_POINTER_DOWN:
        TouchAction := TTouchAction.Down;
      AMOTION_EVENT_ACTION_POINTER_UP:
        TouchAction := TTouchAction.Up;
      else
      begin
        TouchAction := TTouchAction.None;
      end;
    end;
    TWindowManager.Current.MultiTouch(Touches, TouchAction, FEnabledInteractiveGestures);
  end;
end;

//I do not need it - more clear code
(*
procedure TPlatformAndroid.CheckOrientationChange;
var
  LOrientation: TScreenOrientation;
begin
  LOrientation := GetScreenOrientation;
  if FLastOrientation <> LOrientation then
  begin
    FLastOrientation := LOrientation;
    TMessageManager.DefaultManager.SendMessage(Self, TOrientationChangedMessage.Create, True);
  end;
  FLastOrientationTime := GetTick;
end;
*)

procedure TPlatformAndroid.WaitMessage;
begin
  InternalProcessMessages;
end;

procedure TPlatformAndroid.RegisterWakeMainThread;
begin
  System.Classes.WakeMainThread := WakeMainThread;
end;

procedure TPlatformAndroid.UnregisterWakeMainThread;
begin
  System.Classes.WakeMainThread := nil;
end;

procedure TPlatformAndroid.WakeMainThread(Sender: TObject);
var
  WakeCmd: ShortInt;
begin
  WakeCmd := APP_CMD_CONTENT_RECT_CHANGED;
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    __write(FAndroidApp.msgwrite, @WakeCmd, SizeOf(WakeCmd));
end;

procedure TPlatformAndroid.InternalProcessMessages;

  function GetAndUpdateTimeout: Integer;
  var
    PriorityProcessingEndTime: Double;
  begin
    Result := PollTimeoutLowPriority;

    if TWindowManager.Current.Pause then
      Result := PollTimeoutPausedState
    else if FPriorityProcessing then
      Result := 0
    else if FPriorityProcessingWait then
    begin
      PriorityProcessingEndTime := GetTick;

      if Abs(PriorityProcessingEndTime - FPriorityProcessingStartTime) > PriorityProcessingTime then
      begin
        FPriorityProcessingStartTime := PriorityProcessingEndTime;
        FPriorityProcessingWait := False;
      end
      else
        Result := PollTimeoutHighPriority;
    end;
  end;

  procedure StartPriorityProcessing;
  begin
    FPriorityProcessingStartTime := GetTick;
    FPriorityProcessingWait := True;
    FPriorityProcessing := True;
  end;

var
  HasEvents, LDone: Boolean;
  PEventPollSource: Pandroid_poll_source;
  EventPollValue: Integer;
  i: integer; // added to support ALFmxInertialMovement
begin
  HasEvents := False;

  while True do
  begin
    EventPollValue := ALooper_pollAll(GetAndUpdateTimeout, nil, nil, PPointer(@PEventPollSource));
    if (EventPollValue = ALOOPER_POLL_ERROR) or (EventPollValue = ALOOPER_POLL_TIMEOUT) then
      Break;

    if FAndroidApp.destroyRequested <> 0 then
      Break;

    if (PEventPollSource <> nil) and Assigned(PEventPollSource^.process) then
    begin
      PEventPollSource^.process(FAndroidApp, PEventPollSource);
      if EventPollValue = LOOPER_ID_MAIN then
        HasEvents := True;
    end
    else
      Break;
  end;

  FPriorityProcessing := False;

  // added to support ALFmxInertialMovement
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    for i := ALAniCalcTimerProcs.Count - 1 downto 0 do
      ALAniCalcTimerProcs[i].timerProc;
  end;

  if TWindowManager.Current.RenderIfNeeds then
    HasEvents := True;

  //I do not need it - more clear code
  (*
  FTextServiceUpdater.ProcessTextUpdates;
  *)

  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    TTimerManager.Current.CheckSynchronize;
    CheckSynchronize;
  end;

  if HasEvents then
    StartPriorityProcessing;

  //I do not need it - more clear code
  (*
  if FContentRectMightHaveChanged > 0 then
  begin
    if TCustomAndroidContext.IsContextAvailable then begin // << https://quality.embarcadero.com/browse/RSP-16142
      Dec(FContentRectMightHaveChanged);
      TWindowManager.Current.CheckContentRectUpdate;
      TWindowManager.Current.SetNeedsRender;
      if (FContentRectMightHaveChanged <= 0) and FOrientationMightHaveChanged then
      begin
        CheckOrientationChange;
        FOrientationMightHaveChanged := False;
      end;
    end;
  end
  else if (Abs(FLastOrientationTime - GetTick) * 1000 > PollOrientationChange) and not TWindowManager.Current.Pause and TCustomAndroidContext.IsContextAvailable then // << https://quality.embarcadero.com/browse/RSP-16142
    CheckOrientationChange;
  *)

  LDone := False;
  if not HasEvents and not Terminating then
  try
    Application.DoIdle(LDone);
  except
    Application.HandleException(Application);
  end;
end;

{ IFMXTimerService }

function TPlatformAndroid.CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
begin
  Result := TTimerManager.Current.CreateTimer(Interval, TimerFunc);
end;

function TPlatformAndroid.DestroyTimer(Timer: TFmxHandle): Boolean;
begin
  TTimerManager.Current.DestroyTimer(Timer);
  Result := True;
end;

function TPlatformAndroid.GetTick: Double;
var
  Res: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @Res);
  Result := (Int64(1000000000) * res.tv_sec + res.tv_nsec) / 1000000000;
end;

procedure TPlatformAndroid.Activate(const AForm: TCommonCustomForm);
var
  BrowserManager : IFMXWBService;
  MapManager: IFMXMapService;
begin  
  if TPlatformServices.Current.SupportsPlatformService(IFMXWBService, BrowserManager) then
    BrowserManager.RealignBrowsers;
  if TPlatformServices.Current.SupportsPlatformService(IFMXMapService, MapManager) then
    MapManager.RealignMapViews;
end;

function TPlatformAndroid.ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
begin
  Result := TWindowManager.Current.ClientToScreen(TAndroidWindowHandle(AForm.Handle), Point);
end;

function TPlatformAndroid.ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
begin
  Result := TWindowManager.Current.ScreenToClient(TAndroidWindowHandle(AForm.Handle), Point);
end;

function TPlatformAndroid.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
begin
  Result := nil;
  if (AForm.Handle <> nil) and TWindowManager.Current.Windows.Contains(TAndroidWindowHandle(AForm.Handle)) then
    raise Exception.Create('Window already exists.');

  Result := TAndroidWindowHandle.Create(AForm);
  TWindowManager.Current.AddWindow(TAndroidWindowHandle(Result));
  if not IsPopupForm(AForm) then
    TAndroidWindowHandle(Result).Bounds := TRectF.Create(TWindowManager.Current.ContentRect);
end;

procedure TPlatformAndroid.DestroyWindow(const AForm: TCommonCustomForm);
begin
  if (AForm.Handle <> nil) and TWindowManager.Current.Windows.Contains(TAndroidWindowHandle(AForm.Handle)) then
    TWindowManager.Current.Windows.Remove(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.DoubleTapTimerCall;
begin
  //no double tap was made
  DestroyDoubleTapTimer;
end;

procedure TPlatformAndroid.HideWindow(const AForm: TCommonCustomForm);
begin
  if TWindowManager.Current.FVisibleStack.Peek = AForm.Handle then
    TWindowManager.Current.FVisibleStack.Pop;
  if AForm.Handle <> nil then
    TWindowManager.Current.SetNeedsRender;
  if FCanSetState then
  try
    FCanSetState := False;
    AForm.WindowState := TWindowState.wsMinimized;
  finally
    FCanSetState := True;
  end;
end;

procedure TPlatformAndroid.ReleaseWindow(const AForm: TCommonCustomForm);
begin
  if (AForm.Handle <> nil) and TWindowManager.Current.Windows.Contains(TAndroidWindowHandle(AForm.Handle)) then
    TWindowManager.Current.RemoveWindow(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Exclude(FEnabledInteractiveGestures, ARec);
end;

procedure TPlatformAndroid.ShowWindow(const AForm: TCommonCustomForm);
var
  NativeWin: JWindow;
begin
  if AForm.Handle <> nil then
  begin
    if not IsPopupForm(AForm) then
    begin
      NativeWin := TAndroidHelper.Activity.getWindow;
      if AForm.BorderStyle = TFmxFormBorderStyle.None then
        MainActivity.setStatusBarVisibility(False)
      else
        MainActivity.setStatusBarVisibility(True);
      try
        AForm.SetBounds(TWindowManager.Current.ContentRect.Left, TWindowManager.Current.ContentRect.Top,
          TWindowManager.Current.ContentRect.Width, TWindowManager.Current.ContentRect.Height);
      except
        Application.HandleException(AForm);
      end;
    end
    else
      TWindowManager.Current.SetNeedsRender;
    TWindowManager.Current.BringToFront(TAndroidWindowHandle(AForm.Handle));
    if FCanSetState then
    try
      FCanSetState := False;
      if IsPopupForm(AForm) then
        AForm.WindowState := TWindowState.wsNormal
      else
        AForm.WindowState := TWindowState.wsMaximized;
    finally
      FCanSetState := True;
    end;
  end;
end;

function TPlatformAndroid.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
begin
  raise ENotImplemented.CreateFmt(SNotImplementedOnPlatform, ['ShowModal']);
  Result := mrCancel;
end;

procedure TPlatformAndroid.SingleTap;
begin
  TWindowManager.Current.SingleTap;
end;

procedure TPlatformAndroid.SingleTapTimerCall;
begin
  DestroySingleTapTimer;
  SingleTap;
end;

function TPlatformAndroid.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := TWindowManager.Current.FindForm(AHandle);
end;

function TPlatformAndroid.GetClientSize(const AForm: TCommonCustomForm): TPointF;
begin
  if not IsPopupForm(AForm) then
    Result := TPointF.Create(TWindowManager.Current.ContentRect.Width, TWindowManager.Current.ContentRect.Height)
  else
    Result := TAndroidWindowHandle(AForm.Handle).Bounds.Size;
end;

function TPlatformAndroid.GetDefaultTitle: string;
begin
  Result := TAndroidHelper.ApplicationTitle;
end;

function TPlatformAndroid.GetTitle: string;
begin
  Result := FTitle;
end;

function TPlatformAndroid.GetVersionString: string;
var
  PackageInfo: JPackageInfo;
  PackageManager: JPackageManager;
  AppContext: JContext;
begin
  AppContext := TAndroidHelper.Context;
  if AppContext <> nil then
  begin
    PackageManager := AppContext.getPackageManager;
    if PackageManager <> nil then
    begin
      PackageInfo := AppContext.getPackageManager.getPackageInfo(AppContext.getPackageName, 0);
      if PackageInfo <> nil then
        Exit(JStringToString(PackageInfo.versionName));
    end;
  end;
  Result := string.Empty;
end;

procedure TPlatformAndroid.SetTitle(const Value: string);
begin
  if FTitle <> Value then
    FTitle := Value;
end;

function TPlatformAndroid.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
begin
  if IsPopupForm(AForm) then
    Result := TAndroidWindowHandle(AForm.Handle).Bounds
  else
    Result := TRectF.Create(TWindowManager.Current.ContentRect);
end;

procedure TPlatformAndroid.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  if IsPopupForm(AForm) then
    TAndroidWindowHandle(AForm.Handle).Bounds := ARect;
end;

function TPlatformAndroid.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  Result := TWindowManager.Current.Scale;
end;

procedure TPlatformAndroid.InvalidateImmediately(const AForm: TCommonCustomForm);
begin
  TAndroidWindowHandle(AForm.Handle).NeedsUpdate := True;
end;

procedure TPlatformAndroid.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
begin
  TAndroidWindowHandle(AForm.Handle).NeedsUpdate := True;
end;

function TPlatformAndroid.IsPopupForm(const AForm: TCommonCustomForm): Boolean;
begin
  Result := (AForm <> nil) and ((AForm.FormStyle = TFormStyle.Popup) or (AForm.Owner is TPopup));
end;

procedure TPlatformAndroid.Log(const Fmt: string; const Params: array of const);
var
  Msg: string;
  M: TMarshaller;
begin
  Msg := Format(FLogPrefix + Fmt, Params);
  LOGI(M.AsUtf8(Msg).ToPointer);
end;

{ IFMXScreenService }

function TPlatformAndroid.GetScreenSize: TPointF;
var
  Metrics: JDisplayMetrics;
begin
  Metrics := TAndroidHelper.DisplayMetrics;
  if Metrics <> nil then
    FScreenSize := TPointF.Create(Trunc(Metrics.widthPixels / GetScreenScale), Trunc(Metrics.heightPixels / GetScreenScale))
  else
    FScreenSize := TPointF.Zero;
  Result := FScreenSize;
end;

class function TPlatformAndroid.ScaleByMetrics(const Metrics: JDisplayMetrics): Single;
const
// Default values taken from Android SDK reference:
//   http://developer.android.com/reference/android/util/DisplayMetrics.html#density
  DefaultDensityScale = 1;
  DefaultDensityDPI = 160;
var
  DensityScale, DensityDPI: Single;
begin
  if Metrics <> nil then
  begin
    DensityScale := Metrics.density; // API level 1
    DensityDPI := Metrics.densityDpi; // API level 4
  end
  else
  begin
    DensityScale := DefaultDensityScale;
    DensityDPI := DefaultDensityDPI;
  end;

  Result := DensityScale;

  if Assigned(ScreenScaleOverrideHook) then
  begin
    ScreenScaleOverrideHook(ScreenScaleOverrideHookContext, DensityScale, DensityDPI, Result);
    if Result < 1 then
      Result := 1
    else if Result > 3 then
      Result := 3;
  end;
end;

function TPlatformAndroid.GetScreenScale: Single;
var
  Metrics: JDisplayMetrics;
begin
  if SameValue(FScreenScale, 0, TEpsilon.Scale) then
  begin
    Metrics := TAndroidHelper.DisplayMetrics;
    FScreenScale := ScaleByMetrics(Metrics);
  end;
  Result := FScreenScale;
end;

class function TPlatformAndroid.OrientationByDisplay(const Display: JDisplay): TScreenOrientation;

  function IsLandscapeDevice(const Rotation: Integer): Boolean;
  var
    Resources: JResources;
    Configuration: JConfiguration;
    Straight: Boolean;
  begin
    Straight := (Rotation = TJSurface.JavaClass.ROTATION_0) or (Rotation = TJSurface.JavaClass.ROTATION_180);
    if TAndroidHelper.Context <> nil then
    begin
      Resources := TAndroidHelper.Context.getResources;
      if Resources <> nil then
      begin
        Configuration := Resources.getConfiguration;
        if Configuration <> nil then
          Exit(((Configuration.orientation = TJConfiguration.JavaClass.ORIENTATION_LANDSCAPE) and Straight) or
            ((Configuration.orientation = TJConfiguration.JavaClass.ORIENTATION_PORTRAIT) and not Straight));
      end;
    end;
    Result := ((Screen.Width > Screen.Height) and Straight) or ((Screen.Width < Screen.Height) and not Straight);
  end;

var
  Rotation: Integer;
begin
  if Display <> nil then
  begin
    Rotation := Display.getRotation;
    if IsLandscapeDevice(Rotation) then
    begin // landscape device
      if Rotation = TJSurface.JavaClass.ROTATION_180 then
        Result := TScreenOrientation.InvertedLandscape
      else if Rotation = TJSurface.JavaClass.ROTATION_90 then
        Result := TScreenOrientation.InvertedPortrait
      else if Rotation = TJSurface.JavaClass.ROTATION_270 then
        Result := TScreenOrientation.Portrait
      else
        Result := TScreenOrientation.Landscape;
    end
    else
    begin // portrait device
      if Rotation = TJSurface.JavaClass.ROTATION_180 then
        Result := TScreenOrientation.InvertedPortrait
      else if Rotation = TJSurface.JavaClass.ROTATION_90 then
        Result := TScreenOrientation.Landscape
      else if Rotation = TJSurface.JavaClass.ROTATION_270 then
        Result := TScreenOrientation.InvertedLandscape
      else
        Result := TScreenOrientation.Portrait;
    end;
  end
  else
    Result := TScreenOrientation.Portrait;
end;

function TPlatformAndroid.GetScreenOrientation: TScreenOrientation;
begin
  Result := OrientationByDisplay(TAndroidHelper.Display);
end;

procedure TPlatformAndroid.SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
begin
  SynchronizeOnUIThread(procedure begin
      if AValue then
        MainActivity.setSystemUIVisibility(False)
      else
        MainActivity.setSystemUIVisibility(True);
    end);
end;

function TPlatformAndroid.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  Calendar: JCalendar;
begin
  Calendar := TJCalendar.JavaClass.getInstance;
  // On the Android Zero index ñorresponds Sunday, so we need to add offset. Because in RTL DayMonday = 1
  Result := Calendar.getFirstDayOfWeek - MondayOffset;
end;

function TPlatformAndroid.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
begin
  //https://quality.embarcadero.com/browse/RSP-12782
  Result := False;
  if MainActivity <> nil then
    Result := not MainActivity.getSystemUIVisibility;
end;

procedure TPlatformAndroid.SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
begin
end;

var
  MsgTitles: array[TMsgDlgType] of string = (SMsgDlgWarning, SMsgDlgError, SMsgDlgInformation, SMsgDlgConfirm, '');
  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0, mrClose);
  ButtonCaptions: array[TMsgDlgBtn] of string = (
    SMsgDlgYes, SMsgDlgNo, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort,
    SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll,
    SMsgDlgHelp, SMsgDlgClose);

function TPlatformAndroid.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt;
  const AHelpFileName: string): Integer;
begin
  raise ENotImplemented.CreateFmt(SNotImplementedOnPlatform, [SBlockingDialogs]);
  Result := -1;
end;

procedure TPlatformAndroid.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt;
  const AHelpFileName: string; const ACloseDialogProc: TInputCloseDialogProc);
var
  DialogFactory: JFMXDialogFactory;
  DialogListener: TFMXDialogListener;
  AlertDialog: JFMXStandardDialog;
  PosButton, NegButton, NeutralButton: Integer;
  B: TMsgDlgBtn;
  ButtonIndex: Integer;
  ButtonsCount: Integer;
  LCaptions: TJavaObjectArray<JString>;
  I: TMsgDlgBtn;
begin
  ButtonsCount := 0;
  ButtonIndex := 0;
  PosButton := -1;
  NegButton := -1;
  NeutralButton := -1;

  for I := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if I in AButtons then
      Inc(ButtonsCount);
  ButtonsCount := Min(ButtonsCount, 3);

  LCaptions := TJavaObjectArray<JString>.Create(ButtonsCount);

  for B in AButtons do
  begin
    if ButtonIndex < ButtonsCount then
    begin
      LCaptions.Items[ButtonIndex] := StringToJString(ButtonCaptions[B]);
      case ButtonIndex of
        0: PosButton := ModalResults[B];
        1: NegButton := ModalResults[B];
        2: NeutralButton := ModalResults[B];
      end;
    end;
    Inc(ButtonIndex);
  end;

  DialogFactory := TJFMXDialogFactory.JavaClass.getFactory;
  if DialogFactory <> nil then
  begin
    CallInUIThreadAndWaitFinishing(
      procedure
      begin
        AlertDialog := DialogFactory.createMessageDialog(MainActivity, GetNativeTheme, StringToJString(AMessage),
          Ord(ADialogType), LCaptions, PosButton, NegButton, NeutralButton);

        if AlertDialog <> nil then
        begin
          if Assigned(ACloseDialogProc) then
          begin
            DialogListener := TFMXDialogListener.Create(
              procedure (const AResult: TModalResult; const AValues: array of string)
              begin
                ACloseDialogProc(AResult);
              end);
            DialogListener.ParentList := FAlertListeners;
            AlertDialog.setListener(DialogListener);
          end;
          AlertDialog.show;
        end;
      end);
  end;
end;

function TPlatformAndroid.GetTextEditorProxy: JFmxTextEditorProxy;
begin
  if FTextEditorProxy = nil then
    FTextEditorProxy := MainActivity.getTextEditorProxy;
  Result := FTextEditorProxy;
end;

function TPlatformAndroid.InputQuery(const ACaption: string; const APrompts: array of string;
  var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc): Boolean;
begin
  raise ENotImplemented.CreateFmt(SNotImplementedOnPlatform + sLineBreak + SInputQueryAndroidOverloads,
    [SBlockingDialogs]);
  Result := False;
end;

procedure TPlatformAndroid.InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
  const ACloseQueryProc: TInputCloseQueryProc);
var
  DialogFactory: JFMXDialogFactory;
  QueryDialog: JFMXStandardDialog;
  JavaPrompts: TJavaObjectArray<JString>;
  JavaDefaultValues: TJavaObjectArray<JString>;
  DialogListener: TFMXDialogListener;
  LCaptions: TJavaObjectArray<JString>;
  I: Integer;
begin
  if Length(ADefaultValues) < Length(APrompts) then
    raise EInvalidOperation.Create(SPromptArrayTooShort);
  if Length(APrompts) = 0 then
    raise EInvalidOperation.Create(SPromptArrayEmpty);

  JavaPrompts := TJavaObjectArray<JString>.Create(Length(APrompts));
  JavaDefaultValues := TJavaObjectArray<JString>.Create(Length(ADefaultValues));
  for I := 0 to Length(APrompts) - 1 do
    JavaPrompts[I] := StringToJString(APrompts[I]);
  for I := 0 to Length(ADefaultValues) - 1 do
    JavaDefaultValues[I] := StringToJString(ADefaultValues[I]);
  LCaptions := TJavaObjectArray<JString>.Create(2);
  LCaptions.Items[0] := StringToJString(ButtonCaptions[TMsgDlgBtn.mbOK]);
  LCaptions.Items[1] := StringToJString(ButtonCaptions[TMsgDlgBtn.mbCancel]);
  DialogFactory := TJFMXDialogFactory.JavaClass.getFactory;
  if DialogFactory <> nil then
  begin
    CallInUIThreadAndWaitFinishing(
      procedure
        begin
          QueryDialog := DialogFactory.createInputQueryDialog(MainActivity, GetNativeTheme, StringToJString(ACaption),
            JavaPrompts, JavaDefaultValues, LCaptions);
          if QueryDialog <> nil then
          begin
            if Assigned(ACloseQueryProc) then
            begin
              DialogListener := TFMXDialogListener.Create(ACloseQueryProc);
              DialogListener.ParentList := FAlertListeners;
              QueryDialog.setListener(DialogListener);
            end;
            QueryDialog.show;
          end;
        end);
  end;
end;

function TPlatformAndroid.DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.DialogPageSetup(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
  var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.DialogPrint(var ACollate, APrintToFile: Boolean; var AFromPage, AToPage, ACopies: Integer;
  AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange; AOptions: TPrintDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.DialogPrinterSetup: Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
begin
  Result := False;
end;

function TPlatformAndroid.PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
  AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

type

  TSimpleProcedureRunner = class(TJavaLocal, JRunnable)
  strict private
    FSelfLock: TSimpleProcedureRunner;
    FProc: TThreadProcedure;
    FEvent: TEvent;
  public
    constructor Create(Proc: TThreadProcedure); overload;
    procedure run; cdecl;
    property Event: TEvent read FEvent;
  end;

constructor TSimpleProcedureRunner.Create(Proc: TThreadProcedure);
begin
  inherited Create;
  FProc := Proc;
  FSelfLock := Self;
  FEvent := TEvent.Create;
end;

procedure TSimpleProcedureRunner.run;
begin
  FProc;
  FSelfLock := nil;
  FEvent.SetEvent;
end;

procedure TPlatformAndroid.RunOnUIThread(Proc: TThreadProcedure);
begin
  MainActivity.runOnUiThread(TSimpleProcedureRunner.Create(Proc));
end;

procedure TPlatformAndroid.SynchronizeOnUIThread(Proc: TThreadProcedure);
var
  Runner: TSimpleProcedureRunner;
begin
  Runner := TSimpleProcedureRunner.Create(Proc);
  MainActivity.runOnUiThread(Runner);
  Runner.Event.WaitFor;
end;

procedure TPlatformAndroid.LongTapTimerCall;
begin
  //a long press was recognized
  DestroyLongTapTimer;
  DestroySingleTapTimer;
  TWindowManager.Current.SendCMGestureMessage(CreateGestureEventInfo(PointF(0, 0), TInteractiveGesture.LongTap));
end;

procedure TPlatformAndroid.AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Include(FEnabledInteractiveGestures, ARec);
end;

procedure TPlatformAndroid.BringToFront(const AForm: TCommonCustomForm);
begin
  if AForm.Handle <> nil then
    TWindowManager.Current.BringToFront(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.SendToBack(const AForm: TCommonCustomForm);
begin
  if AForm.Handle <> nil then
    TWindowManager.Current.SendToBack(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.SetApplicationEventHandler(AEventHandler: TApplicationEventHandler);
begin
  FOnApplicationEvent := AEventHandler;
end;

function TPlatformAndroid.HandleApplicationEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
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

procedure TPlatformAndroid.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
var
  Bounds: TRectF;
begin
  if IsPopupForm(AForm) then
  begin
    Bounds := TAndroidWindowHandle(AForm.Handle).Bounds;
    TAndroidWindowHandle(AForm.Handle).Bounds := TRectF.Create(Bounds.TopLeft, ASize.X, ASize.Y);
  end;
end;

procedure TPlatformAndroid.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
begin
  // NOP on Android
end;

procedure TPlatformAndroid.SetCapture(const AForm: TCommonCustomForm);
begin
  TWindowManager.Current.SetCapture(TAndroidWindowHandle(AForm.Handle));
end;

procedure TPlatformAndroid.ReleaseCapture(const AForm: TCommonCustomForm);
begin
  TWindowManager.Current.ReleaseCapture;
end;

procedure TPlatformAndroid.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
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

procedure TPlatformAndroid.RegisterCanvasClasses;
begin
  FMX.Canvas.GPU.RegisterCanvasClasses;
end;

procedure TPlatformAndroid.RegisterContextClasses;
begin
  FMX.Context.GLES.Android.RegisterContextClasses;
end;

procedure TPlatformAndroid.UnregisterCanvasClasses;
begin
  FMX.Canvas.GPU.UnregisterCanvasClasses;
end;

procedure TPlatformAndroid.UnregisterContextClasses;
begin
  FMX.Context.GLES.Android.UnregisterContextClasses;
end;

{ IFMXSystemInformationService }

function TPlatformAndroid.GetScrollingBehaviour: TScrollingBehaviours;
begin
  Result := [TScrollingBehaviour.Animation, TScrollingBehaviour.AutoShowing, TScrollingBehaviour.TouchTracking];
end;

function TPlatformAndroid.GetMinScrollThumbSize: Single;
begin
  Result := 30;
end;

function TPlatformAndroid.GetCaretWidth: Integer;
begin
  Result := 2;
end;

function TPlatformAndroid.GetMenuShowDelay: Integer;
begin
  Result := 0;
end;

{ IFMXSystemFontService }

function TPlatformAndroid.GetDefaultFontFamilyName: string;
begin
  Result := DefaultAndroidFontName;
end;

function TPlatformAndroid.GetDefaultFontSize: Single;
begin
  Result := DefaultAndroidFontSize;
end;

{ IFMXDefaultPropertyValueService }

function TPlatformAndroid.GetDefaultPropertyValue(const AClassName, PropertyName: string): TValue;

  function GetSpinBoxPropertyDefaultValue: TValue;
  const
    SpinBoxDefaultPropName = 'CanFocusOnPlusMinus'; //Do not localize
  begin
    Result := TValue.Empty;
    if string.Compare(PropertyName, SpinBoxDefaultPropName, True) = 0 then
      Result := False;
  end;

  function GetComboEditPropertyDefaultValue: TValue;
  const
    ComboEditDefaultPropName = 'NeedSetFocusAfterButtonClick'; //Do not localize
  begin
    Result := TValue.Empty;
    if string.Compare(PropertyName, ComboEditDefaultPropName, True) = 0 then
      Result := False;
  end;

const
  ColorComboBoxClassName = 'tcolorcombobox'; //Do not localize
  SpibBoxClassName = 'tspinbox'; //Do not localize
  ComboExitClassName = 'tcomboeditbox'; //Do not localize
begin
  Result := TValue.Empty;

  if string.Compare(AClassName, ColorComboBoxClassName, True) = 0 then
    Result := TValue.From<TDropDownKind>(TDropDownKind.Native)
  else if string.Compare(AClassName, SpibBoxClassName, True) = 0 then
    Result := GetSpinBoxPropertyDefaultValue
  else if string.Compare(AClassName, ComboExitClassName, True) = 0 then
    Result := GetComboEditPropertyDefaultValue
  else
    Result := False;
end;

{ IFMXDefaultMetricsService }

function TPlatformAndroid.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  case AComponent of
    TComponentKind.Button: Result := TSize.Create(73, 44);
    TComponentKind.Label: Result := TSize.Create(82, 23);
    TComponentKind.Edit: Result := TSize.Create(97, 32);
    TComponentKind.ScrollBar: Result := TSize.Create(7, 7);
    TComponentKind.ListBoxItem: Result := TSize.Create(44, 44);
    TComponentKind.Calendar: Result := TSize.Create(346, 300);
  else
    Result := TSize.Create(80, 22);
  end;
end;

function TPlatformAndroid.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  case AComponent of
    TComponentKind.Button: Result := True;
    TComponentKind.Label: Result := True;
    TComponentKind.Edit: Result := True;
    TComponentKind.ScrollBar: Result := True;
    TComponentKind.ListBoxItem: Result := True;
    TComponentKind.Calendar: Result := True;
  else
    Result := False;
  end;
end;

{ IFMXLocaleService }

function TPlatformAndroid.GetCurrentLangID: string;
var
  Locale: JLocale;
begin
  Locale := TJLocale.JavaClass.getDefault;
  Result := JStringToString(Locale.getISO3Language);
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;

function TPlatformAndroid.GetLocaleFirstDayOfWeek: string;
var
  Calendar: JCalendar;
begin
  Calendar := TJCalendar.JavaClass.getInstance;
  Result := IntToStr(Calendar.getFirstDayOfWeek);
end;

function TPlatformAndroid.GetLongTapAllowedMovement: Single;
begin
  Result := LONG_TAP_MOVEMENT/TWindowManager.Current.Scale;
end;

{ IFMXListingService }

function TPlatformAndroid.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [];
end;

function TPlatformAndroid.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop];
end;

function TPlatformAndroid.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [TListingTransitionFeature.ScrollGlow];
end;

function TPlatformAndroid.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [];
end;

function TPlatformAndroid.GetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

  procedure SeekAndReadBlock(const AStream: TStream);
  var
    R: TBinaryReader;
    LBlockSize: Integer;
    LBlockName: string;
  begin
    R := TBinaryReader.Create(AStream);
    try
      AStream.Seek(0, TSeekOrigin.soBeginning);
      while AStream.Position < AStream.Size do
      begin
        LBlockSize := R.ReadInteger;
        LBlockName := R.ReadString;
        if SameText(LBlockName, ABlockName) then
        begin
          ABlockData.CopyFrom(AStream, LBlockSize);
          Break;
        end
        else
          AStream.Seek(LBlockSize, TSeekOrigin.soCurrent);
      end;
    finally
      R.Free;
    end;
  end;

  procedure ReadPersistent(const AFileName: string);
  var
    S: TFileStream;
  begin
    S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      ABlockData.CopyFrom(S, S.Size);
    finally
      S.Free;
    end;
  end;

var
  LStream: TMemoryStream;
  LFileName: string;
begin
  if ABlockName.IsEmpty or (ABlockData = nil) then
    Exit(False);
  if FSaveStateStoragePath.Length > 0 then
  begin
    // Persistent state is read from fixed storage.
    LFileName := FSaveStateStoragePath + ABlockName;
    if not TFile.Exists(LFileName) then
      Exit(False);
    try
      ReadPersistent(LFileName);
    except
      Exit(False);
    end;
  end
  else
  begin
    // Transient state is read from native activity.
    if (FAndroidApp^.savedState = nil) or (FAndroidApp^.savedStateSize < 1) then
      Exit(False);
    LStream := TMemoryStream.Create;
    try
      try
        LStream.Size := FAndroidApp^.savedStateSize;
        LStream.WriteBuffer(FAndroidApp^.savedState^, FAndroidApp^.savedStateSize);
        SeekAndReadBlock(LStream);
      except
        Exit(False);
      end;
    finally
      LStream.Free;
    end;
  end;
  Result := True;
end;

function TPlatformAndroid.SetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

  procedure WriteBlockToEnd(const AStream: TStream);
  var
    W: TBinaryWriter;
  begin
    W := TBinaryWriter.Create(AStream);
    try
      W.Write(Integer(ABlockData.Size));
      W.Write(ABlockName);
    finally
      W.Free;
    end;
    ABlockData.Seek(0, TSeekOrigin.soBeginning);
    AStream.CopyFrom(ABlockData, ABlockData.Size);
  end;

  procedure WritePersistent(const AFileName: string);
  var
    S: TFileStream;
  begin
    S := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
    try
      ABlockData.Seek(0, TSeekOrigin.soBeginning);
      S.CopyFrom(ABlockData, ABlockData.Size);
    finally
      S.Free;
    end;
  end;

var
  LStream: TMemoryStream;
  LFileName: string;
begin
  if ABlockName.IsEmpty then
    Exit(False);
  if not FSaveStateStoragePath.IsEmpty then
  begin
    // Persistent state is written to fixed storage.
    LFileName := FSaveStateStoragePath + ABlockName;
    if (ABlockData = nil) or (ABlockData.Size < 1) then
    begin
      if TFile.Exists(LFileName) then
        TFile.Delete(LFileName);
    end
    else
      try
        WritePersistent(LFileName);
      except
        Exit(False);
      end;
  end
  else
  begin
    // Transient state is saved to native activity.
    if (ABlockData = nil) or (ABlockData.Size < 1) then
      Exit(True);
    LStream := TMemoryStream.Create;
    try
      try
        if (FAndroidApp^.savedState <> nil) and (FAndroidApp^.savedStateSize > 0) then
          LStream.WriteBuffer(FAndroidApp^.savedState^, FAndroidApp^.savedStateSize);
        WriteBlockToEnd(LStream);
      except
        Exit(False);
      end;
      if FAndroidApp^.savedState <> nil then
        Posix.StdLib.free(FAndroidApp^.savedState);
      FAndroidApp^.savedStateSize := LStream.Size;
      FAndroidApp^.savedState := Posix.StdLib.__malloc(FAndroidApp^.savedStateSize);
      try
        LStream.Seek(0, TSeekOrigin.soBeginning);
        LStream.ReadBuffer(FAndroidApp^.savedState^, FAndroidApp^.savedStateSize);
      except
        Exit(False);
      end;
    finally
      LStream.Free;
    end;
  end;
  Result := True;
end;

function TPlatformAndroid.GetSaveStateStoragePath: string;
begin
  Result := FSaveStateStoragePath;
end;

procedure TPlatformAndroid.SetSaveStateStoragePath(const ANewPath: string);
begin
  if not ANewPath.IsEmpty then
    FSaveStateStoragePath := IncludeTrailingPathDelimiter(ANewPath)
  else
    FSaveStateStoragePath := '';
end;

function TPlatformAndroid.GetSaveStateNotifications: Boolean;
begin
  Result := True;
end;

function TPlatformAndroid.GetDisplayMetrics: TDeviceDisplayMetrics;
var
  Metrics: JDisplayMetrics;
  RawScreenSize: JPoint;
  DensityDPI: Single;
begin
  Metrics := TAndroidHelper.DisplayMetrics;
  if Metrics <> nil then
  begin
    Result.PhysicalScreenSize := TSize.Create(Metrics.widthPixels, Metrics.heightPixels);
    DensityDPI := Round((Metrics.xdpi + Metrics.ydpi) / 2);
    if DensityDPI <> 0 then
    begin
      Result.LogicalScreenSize.cx := Trunc(Metrics.widthPixels / DensityDPI);
      Result.LogicalScreenSize.cy := Trunc(Metrics.heightPixels / DensityDPI);
    end
    else
      Result.LogicalScreenSize := Result.PhysicalScreenSize;
    if Metrics.widthPixels <> 0 then
      Result.AspectRatio := Metrics.heightPixels / Metrics.widthPixels
    else
      Result.AspectRatio := 1;
    Result.PixelsPerInch := Round(DensityDPI);
    Result.ScreenScale := Metrics.density;
    Result.FontScale := Metrics.scaledDensity;
  end
  else
    Result := TDeviceDisplayMetrics.Default;
  RawScreenSize := MainActivity.getRawDisplaySize;
  if RawScreenSize <> nil then
    if (Result.PhysicalScreenSize.cx > Result.PhysicalScreenSize.cy) and (RawScreenSize.x > RawScreenSize.y) then
      Result.RawScreenSize := TSize.Create(RawScreenSize.x, RawScreenSize.y)
    else
      Result.RawScreenSize := TSize.Create(RawScreenSize.y, RawScreenSize.x)
  else
    Result.RawScreenSize := Result.PhysicalScreenSize;
end;

{ IFMXDeviceService }

function TDeviceServiceAndroid.GetModel: string;
begin
  Result := JStringToString(TJBuild.JavaClass.MODEL);
end;

function TDeviceServiceAndroid.SuppportsTelephony: Boolean;
var
  TelephonyServiceNative: JObject;
  TelephoneManager: JTelephonyManager;
begin
  TelephonyServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.TELEPHONY_SERVICE);
  if TelephonyServiceNative <> nil then
  begin
    TelephoneManager := TJTelephonyManager.Wrap((TelephonyServiceNative as ILocalObject).GetObjectID);
    Result := TelephoneManager.getPhoneType <> TJTelephonyManager.JavaClass.PHONE_TYPE_NONE;
  end
  else
    Result := False;
end;

function TDeviceServiceAndroid.GetFeatures: TDeviceFeatures;
begin
  Result := [TDeviceFeature.HasTouchScreen];
end;

function TDeviceServiceAndroid.GetDeviceClass: TDeviceInfo.TDeviceClass;
const
  MaxPhoneDiagonalSize = 6;
  MinWatchAPILevel = 20;
var
  DisplayMetrics: JDisplayMetrics;
  Display: JDisplay;
  ScreenWidth: Integer;
  ScreenHeight: Integer;
  ScreenDiagonal: Double;
begin
  if FDeviceClassCached then
    Exit(FDeviceClass);

  Display := MainActivity.getWindowManager.getDefaultDisplay;
  DisplayMetrics := TJDisplayMetrics.Create;
  Display.getMetrics(DisplayMetrics);
  ScreenWidth := Round(DisplayMetrics.widthPixels / DisplayMetrics.densityDpi);
  ScreenHeight := Round(DisplayMetrics.heightPixels / DisplayMetrics.densityDpi);
  ScreenDiagonal := Sqrt(ScreenWidth * ScreenWidth + ScreenHeight * ScreenHeight);

  if (TOSVersion.Check(5) or (TJBuild_VERSION.JavaClass.SDK_INT >= MinWatchAPILevel)) and
    TAndroidHelper.HasSystemService(TJPackageManager.JavaClass.FEATURE_WATCH) then
    FDeviceClass := TDeviceInfo.TDeviceClass.Watch
  else if GetModel.StartsWith('Glass') then
    FDeviceClass := TDeviceInfo.TDeviceClass.Glasses
  else if (ScreenDiagonal <= MaxPhoneDiagonalSize) and SuppportsTelephony then
    FDeviceClass := TDeviceInfo.TDeviceClass.Phone
  else
    FDeviceClass := TDeviceInfo.TDeviceClass.Tablet;

  FDeviceClassCached := True;
  Result := FDeviceClass;
end;

{ TMultiDisplayAndroid.TDisplayAndroid }

constructor TMultiDisplayAndroid.TDisplayAndroid.Create(const AIndex: Integer; const APrimary: Boolean; const ABounds,
  AWorkArea: TRect; const AId: Integer);
begin
  Display := TDisplay.Create(AIndex, APrimary, ABounds, AWorkArea);
  Id := AId;
end;

{ TMultiDisplayAndroid }

constructor TMultiDisplayAndroid.Create;
var
  ServiceClassName: string;
  WindowService, DisplayService: JObject;
begin
  inherited Create;
  DisplayService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.DISPLAY_SERVICE);
  if DisplayService <> nil then
  begin
    ServiceClassName := JStringToString(DisplayService.getClass.getName);
    if string.Compare(ServiceClassName, 'android.hardware.display.DisplayManager', True) = 0 then
    begin
      FDisplayManager := TJDisplayManager.Wrap((DisplayService as ILocalObject).GetObjectID);
      WindowService := TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.WINDOW_SERVICE);
      if WindowService <> nil then
        FWindowManager := TJWindowManager.Wrap((WindowService as ILocalObject).GetObjectID);
      if FWindowManager = nil then
        raise EUnsupportedPlatformService.CreateFMT(SUnsupportedPlatformService, ['IFMXMultiDisplayService']);
    end;
  end;
end;

procedure TMultiDisplayAndroid.UpdateDisplayInformation;
begin
  FDisplayCount := 0;
  FSystemDisplayCount := 0;
  FDesktopRect := TRect.Empty;
  FWorkAreaRect := TRect.Empty;
  FreeAndNil(FDisplayList);
end;

function TMultiDisplayAndroid.GetDisplayInfo(const Display: JDisplay; var BoundsRect, WorkareaRect: TRect): Boolean;
var
  LMetrics: JDisplayMetrics;
  Scale: Single;
  R: JRect;
  PW, PH: JPoint;
  W, H: Single;
begin
  if Display = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Result := False;
  R := TJRect.Create;
  PW := TJPoint.Create;
  PH := TJPoint.Create;
  LMetrics := TJDisplayMetrics.Create;
  Display.getMetrics(LMetrics);
  Scale := TPlatformAndroid.ScaleByMetrics(LMetrics);
  if Scale > 0 then
  begin
    Display.getRectSize(R);
    if (R.width > 0) and (R.height > 0) then
    begin
      BoundsRect := TRect.Create(Trunc(R.left / Scale), Trunc(R.top / Scale), Trunc(R.right / Scale),
        Trunc(R.bottom / Scale));
      Result := True;
      Display.getCurrentSizeRange(PW, PH);
      if TPlatformAndroid.OrientationByDisplay(Display) in [TScreenOrientation.Portrait,
        TScreenOrientation.InvertedPortrait] then
      begin
        W := PW.x;
        H := PH.y;
      end
      else
      begin
        W := PH.x;
        H := PW.y;
      end;
      WorkareaRect := TRect.Create(TPoint.Create(Round((R.right - W) / Scale),
        Round((R.bottom - H) / Scale)), Trunc(W / Scale), Trunc(H / Scale));
    end;
  end;
end;

procedure TMultiDisplayAndroid.UpdateDisplays;
type
  TTmpDisplay = record
    Default: Boolean;
    BoundsRect, WorkareaRect: TRect;
    Id: Integer;
  end;
var
  I, DefaultId: Integer;
  Display: JDisplay;
  DefaultExists: Boolean;
  TmpDisplay: TTmpDisplay;
  TmpDisplays: TList<TTmpDisplay>;
begin
  UpdateDisplayInformation;
  if DisplayManager <> nil then
  begin
    FSystemDisplayCount := DisplayManager.getDisplays.Length;
    DefaultId := FWindowManager.getDefaultDisplay.getDisplayId;
    if FDisplayList = nil then
      FDisplayList := TList<TDisplayAndroid>.Create
    else
      FDisplayList.Clear;

    TmpDisplays := TList<TTmpDisplay>.Create;
    try
      DefaultExists := False;
      for I := 0 to FSystemDisplayCount - 1 do
      begin
        Display := DisplayManager.getDisplays[I];
        // We use only displays with the correct information
        if GetDisplayInfo(Display, TmpDisplay.BoundsRect, TmpDisplay.WorkareaRect) then
        begin
          if Display.getDisplayId = DefaultId then
            DefaultExists := True;
          TmpDisplay.Default := Display.getDisplayId = DefaultId;
          TmpDisplay.Id := Display.getDisplayId;
          TmpDisplays.Add(TmpDisplay);
        end;
      end;
      FDisplayCount := TmpDisplays.Count;
      // If there is no primary display use the first on the list
      if (FDisplayCount > 0) and not DefaultExists then
      begin
        TmpDisplay := TmpDisplays[0];
        TmpDisplay.Default := True;
        TmpDisplays[0] := TmpDisplay;
      end;
      for I := 0 to FDisplayCount - 1 do
        FDisplayList.Add(TDisplayAndroid.Create(I, TmpDisplays[I].Default, TmpDisplays[I].BoundsRect,
          TmpDisplays[I].WorkareaRect, TmpDisplays[I].Id));
    finally
      TmpDisplays.Free;
    end;
  end;
end;

function TMultiDisplayAndroid.GetDisplayCount: Integer;
begin
  if FDisplayCount = 0 then
    UpdateDisplays;
  Result := FDisplayCount;
end;

function TMultiDisplayAndroid.GetDesktopCenterRect(const Size: TSize): TRect;
var
  DesktopCenter: TPoint;
begin
  DesktopCenter := GetWorkAreaRect.CenterPoint;
  Result := TRect.Create(TPoint.Create(DesktopCenter.X - Size.cx div 2, DesktopCenter.Y - Size.cy div 2), Size.cx,
    Size.cy);
end;

function TMultiDisplayAndroid.GetWorkAreaRect: TRect;
var
  TmpRect: TRect;
begin
  if (FWorkAreaRect.Width <= 0) or (FWorkAreaRect.Height <= 0) then
  begin
    self.GetDisplayInfo(WindowManager.getDefaultDisplay, TmpRect, FWorkAreaRect);
  end;
  Result := FWorkAreaRect;
end;

function TMultiDisplayAndroid.GetDesktopRect: TRect;
var
  I: Integer;
begin
  if (FDesktopRect.Width <= 0) or (FDesktopRect.Height <= 0) then
  begin
    FDesktopRect := TRect.Empty;
    for I := 0 to GetDisplayCount - 1 do
      FDesktopRect.Union(GetDisplay(I).BoundsRect);
  end;
  Result := FDesktopRect;
end;

function TMultiDisplayAndroid.GetDisplay(const Index: Integer): TDisplay;
begin
  if Index < 0 then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  if (FDisplayList = nil) or (FSystemDisplayCount <> DisplayManager.getDisplays.Length) then
    UpdateDisplays;
  if Index >= GetDisplayCount then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FDisplayList[Index].Display;
end;

function TMultiDisplayAndroid.FindDisplay(const Activity: JActivity): TDisplay;
  function DoFind(const Id: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    if FDisplayList <> nil then
      for I := 0 to FDisplayList.Count - 1 do
        if Id = FDisplayList[I].Id then
          Exit(I);
  end;

var
  Index, DisplayId: Integer;
begin
  if (Activity = nil) or (Activity.getWindowManager = nil) or (Activity.getWindowManager.getDefaultDisplay = nil) then
    raise EInvalidFmxHandle.Create(sArgumentInvalid);
  DisplayId := Activity.getWindowManager.getDefaultDisplay.getDisplayId;
  Index := DoFind(DisplayId);
  if Index = -1 then
  begin
    UpdateDisplays;
    Index := DoFind(DisplayId);
  end;
  if Index = -1 then
    raise EInvalidArgument.Create(sArgumentInvalid)
  else
    Result := FDisplayList[Index].Display;
end;

function TMultiDisplayAndroid.DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
begin
  Result := FindDisplay(TAndroidHelper.Activity)
end;

function TMultiDisplayAndroid.DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
begin
  Result := DisplayFromWindow(Handle);
end;

constructor TWaitableValueBase.Create;
begin
  inherited Create;

  Done := TEvent.Create;
  Value := TValue.Empty;
end;

procedure TPlatformAndroid.SetKeyboardEventToSkip(event: JKeyEvent);
begin
  FSkipEventQueue.Enqueue(event);
end;

procedure TPlatformAndroid.SetScreenOrientation(AOrientations: TScreenOrientations);
begin
  if (TScreenOrientation.Portrait in AOrientations) and
     (TScreenOrientation.Landscape in AOrientations) and
     (TScreenOrientation.InvertedPortrait in AOrientations) and
     (TScreenOrientation.InvertedLandscape in AOrientations) then
    MainActivity.embSetOrientation(-1)
  else if TScreenOrientation.Portrait in AOrientations then
    MainActivity.embSetOrientation(Integer(TScreenOrientation.Portrait))
  else if TScreenOrientation.Landscape in AOrientations then
    MainActivity.embSetOrientation(Integer(TScreenOrientation.Landscape))
  else if TScreenOrientation.InvertedPortrait in AOrientations then
    MainActivity.embSetOrientation(Integer(TScreenOrientation.InvertedPortrait))
  else if TScreenOrientation.InvertedLandscape in AOrientations then
    MainActivity.embSetOrientation(Integer(TScreenOrientation.InvertedLandscape));
end;

function TPlatformAndroid.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceAndroid;
end;

function TPlatformAndroid.RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
begin
    Result := FKeyMapping.RegisterKeyMapping(PlatformKey, VirtualKey, KeyKind);
end;

function TPlatformAndroid.UnregisterKeyMapping(const PlatformKey: Word): Boolean;
begin
    Result := FKeyMapping.UnregisterKeyMapping(PlatformKey);
end;

function TPlatformAndroid.PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
begin
    Result := FKeyMapping.PlatformKeyToVirtualKey(PlatformKey, KeyKind);
end;

function TPlatformAndroid.VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
begin
    Result := FKeyMapping.VirtualKeyToPlatformKey(VirtualKey);
end;

{ TAndroidWindowHandle }

constructor TAndroidWindowHandle.Create(const AForm: TCommonCustomForm);
begin
  inherited Create;
  FNeedsUpdate := True;
  FForm := AForm;
  FBounds := TRectF.Create(AForm.Left, AForm.Top, AForm.Left + AForm.Width, AForm.Top + AForm.Height);
end;

procedure TAndroidWindowHandle.CreateTexture;
var
  ScaledSize: TSize;
begin
  if FTexture = nil then
  begin
    ScaledSize := TSize.Create(Round(Bounds.Width * TWindowManager.Current.Scale),
      Round(Bounds.Height * TWindowManager.Current.Scale));

    FTexture := TTexture.Create;
    FTexture.Style := [TTextureStyle.RenderTarget];
    FTexture.SetSize(ScaledSize.Width, ScaledSize.Height);
    FTexture.Initialize;
  end;
end;

procedure TAndroidWindowHandle.DestroyTexture;
begin
  if FTexture <> nil then
  begin
    FTexture.DisposeOf;
    FTexture := nil;
  end;
end;

procedure TAndroidWindowHandle.SetBounds(const Value: TRectF);
begin
  if FBounds <> Value then
  begin
    FBounds := Value;
    if FForm <> nil then
      FForm.SetBounds(Trunc(Value.Left), Trunc(Value.Top), Trunc(Value.Width), Trunc(Value.Height));
    TWindowManager.Current.SetNeedsRender;
  end;
end;

function TAndroidWindowHandle.GetIsPopup: Boolean;
begin
  Result := PlatformAndroid.IsPopupForm(FForm);
end;

function TAndroidWindowHandle.GetScale: Single;
begin
  Result := TWindowManager.Current.Scale;
end;

function TAndroidWindowHandle.RequiresComposition: Boolean;
begin
  Result := PlatformAndroid.IsPopupForm(FForm) or (FForm.Transparency and (Application.MainForm <> FForm));
end;

procedure TAndroidWindowHandle.SetNeedsUpdate(const Value: Boolean);
begin
  FNeedsUpdate := Value;
  if FNeedsUpdate then
    TWindowManager.Current.SetNeedsRender;
end;

{ TTimerManager }

constructor TTimerManager.Create;
begin
  FHandlers := TDictionary<TFmxHandle, TTimerProc>.Create;
  FQueueDictionary := TObjectDictionary<TFMXHandle, TThreadedQueue<TFmxHandle>>.Create;
  AndroidTimerSetHandler(DoOnTimer);
end;

destructor TTimerManager.Destroy;
begin
  AndroidTimerSetHandler(nil);
  FQueueDictionary.Free;
  FHandlers.Free;
  inherited;
end;

function TTimerManager.CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
const
  QueueDepth = 1;
  QueueTimeout = 0;
var
  TimerHandle: Integer;
begin
  TMonitor.Enter(Self);
  try
    TimerHandle := AndroidTimerCreate;
    FHandlers.Add(TimerHandle, TimerFunc);
    FQueueDictionary.Add(TimerHandle, TThreadedQueue<TFmxHandle>.Create(QueueDepth, QueueTimeout, QueueTimeout));
    AndroidTimerSetInterval(TimerHandle, Interval);
  finally
    TMonitor.Exit(Self);
  end;

  Result := TimerHandle;
end;

procedure TTimerManager.DestroyTimer(TimerHandle: TFmxHandle);
begin
  TMonitor.Enter(Self);
  try
    AndroidTimerDestroy(TimerHandle);
    FQueueDictionary.Remove(TimerHandle);
    FHandlers.Remove(TimerHandle);
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TTimerManager.DoOnTimer(TimerHandle: Integer);
var
  LQueue: TThreadedQueue<TFmxHandle>;
begin
  TMonitor.Enter(Self);
  try
    FQueueDictionary.TryGetValue(TFmxHandle(TimerHandle), LQueue);
  finally
    TMonitor.Exit(Self);
  end;

  if LQueue <> nil then
    LQueue.PushItem(TimerHandle);
end;

procedure TTimerManager.CheckSynchronize;
var
  QueuedTimerHandle: TFmxHandle;
  Proc: TTimerProc;
  HasProc: Boolean;
  LQueueKeys: TArray<TFmxHandle>;
  LQueue: TThreadedQueue<TFmxHandle>;
  I: Integer;
begin

  TMonitor.Enter(Self);
  try
    LQueueKeys := FQueueDictionary.Keys.ToArray;
  finally
    TMonitor.Exit(Self);
  end;

  for I := 0 to Length(LQueueKeys) - 1 do
  begin
    HasProc := False;
    TMonitor.Enter(Self);
    try
      FQueueDictionary.TryGetValue(LQueueKeys[I], LQueue);
    finally
      TMonitor.Exit(Self);
    end;
    if LQueue <> nil then
    begin
      QueuedTimerHandle := LQueue.PopItem;
      TMonitor.Enter(Self);
      try
        HasProc := FHandlers.TryGetValue(QueuedTimerHandle, Proc);
      finally
        TMonitor.Exit(Self);
      end;
    end;
    if HasProc and Assigned(Proc) then
      Proc;
  end;
end;

class function TTimerManager.GetInstance: TTimerManager;
begin
  if FInstance = nil then
    FInstance := TTimerManager.Create;
  Result := FInstance;
end;

procedure TTimerManager.KillAllTimers;
var
  Keys: TArray<TFmxHandle>;
  I: Integer;
begin
  TMonitor.Enter(Self);
  try
    FQueueDictionary.Clear;
  finally
    TMonitor.Exit(Self);
  end;
  Keys := FHandlers.Keys.ToArray;
  for I := Length(Keys) - 1 downto 0 do
  try
    DestroyTimer(Keys[I]);
  except
    Continue;
  end;
  FInstance := nil;
end;

{ TTextServiceAndroid }

constructor TTextServiceAndroid.Create(const Owner: IControl;
  SupportMultiLine: Boolean);
begin
  inherited Create(Owner, SupportMultiLine);
  FLines := TStringList.Create;
  FComposingBegin := -1;
  FComposingEnd := -1;
end;

destructor TTextServiceAndroid.Destroy;
begin
  inherited;
end;

procedure TTextServiceAndroid.BeginSelection;
begin
  TWindowManager.Current.BeginSelection;
  TWindowManager.Current.HideContextMenu;
end;

procedure TTextServiceAndroid.EndSelection;
begin
  TWindowManager.Current.EndSelection;
  InternalUpdateSelection;
  TWindowManager.Current.ShowContextMenu;
end;

function TTextServiceAndroid.CombinedText: string;
var
  I, TextLength: Integer;
  Builder: TStringBuilder;
begin
  TextLength := 0;
  for I := 0 to FLines.Count - 1 do
  begin
    if I > 0 then
      Inc(TextLength);
    Inc(TextLength, FLines[I].Length);
  end;
  Builder := TStringBuilder.Create(TextLength);
  for I := 0 to FLines.Count - 1 do
  begin
    if I > 0 then
      Builder.Append(FLines.LineBreak);
    Builder.Append(FLines[I]);
  end;
  Result := Builder.ToString;
end;

procedure TTextServiceAndroid.CopySelectedText;
begin
  if FTextView <> nil then
    FTextView.copySelectedText;
end;

procedure TTextServiceAndroid.CutSelectedText;
begin
  if FTextView <> nil then
    FTextView.cutSelectedText;
end;

procedure TTextServiceAndroid.PasteText;
begin
  if FTextView <> nil then
    FTextView.pasteText;
end;

procedure TTextServiceAndroid.DrawSingleLine(const Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False);
var
  I, Shift: Integer;
  S: string;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.WordWrap := AWordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Font;
    Layout.Color := Canvas.Fill.Color;
    Layout.Opacity := AOpacity;
    Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
    if FLines.Count > 0 then
      S := FLines[FCaretPosition.Y]
    else
      S := '';
    Layout.Text := S.Substring(FirstVisibleChar - 1, S.Length - FirstVisibleChar + 1);
    Layout.EndUpdate;
    Layout.RenderLayout(Canvas);

    if (FComposingBegin >= 0) and (FComposingEnd >= 0) and (FComposingBegin < FComposingEnd) then
    try
      Shift := 0;
      if FLines.Count > 0 then
        for I := 0 to FCaretPosition.Y - 1 do
          Inc(Shift, FLines[I].Length + FLines.LineBreak.Length);

      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(TTextRange.Create(FComposingBegin - Shift - (FirstVisibleChar - 1), FComposingEnd - FComposingBegin));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);
    finally
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

procedure TTextServiceAndroid.DrawSingleLine(const Canvas: TCanvas;
      const S: string;
      const ARect: TRectF;
      const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False);
var
  I: Integer;
  Layout: TTextLayout;
  Region: TRegion;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Canvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
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

    if (FComposingBegin >= 0) and (FComposingEnd >= 0) and (FComposingBegin < FComposingEnd) then
    try
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(TTextRange.Create(FComposingBegin, FComposingEnd - FComposingBegin));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);
    finally
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

{ TFMXTextListener }

constructor TFMXTextListener.Create(const TextService: TTextServiceAndroid);
begin
  inherited Create;
  FTextService := TextService;
end;

procedure TFMXTextListener.onTextUpdated(text: JCharSequence; position: Integer);
//I do not need it - more clear code
(*
var
  Update: TTextUpdate;
*)
begin
  //I do not need it - more clear code
  (*
  Update.X := position;
  Update.Text := JCharSequenceToStr(text);
  Update.Service := FTextService;
  PlatformAndroid.TextServiceUpdater.QueueTextUpdate(Update);
  *)
end;

procedure TFMXTextListener.onComposingText(beginPosition: Integer; endPosition: Integer);
begin
  TWindowManager.Current.HideContextMenu;
  FTextService.FComposingBegin := beginPosition;
  FTextService.FComposingEnd := endPosition;
end;

procedure TFMXTextListener.onSkipKeyEvent(event: JKeyEvent);
begin
  PlatformAndroid.SetKeyboardEventToSkip(event);
end;

procedure TTextServiceAndroid.EnterControl(const FormHandle: TWindowHandle);
var
  VirtKBControl: IVirtualKeyboardControl;
  KbType: Integer;
  RKType: Integer;
  SelStart, SelEnd: Integer;
  ReadOnly, Password: Boolean;
  LReadOnly: IReadOnly;
begin
  if (FormHandle is TAndroidWindowHandle) and (TAndroidWindowHandle(FormHandle).Form.Focused <> nil) then
  begin
    if Supports(TAndroidWindowHandle(FormHandle).Form.Focused, IVirtualKeyboardControl, VirtKBControl) then
    begin
      case VirtKBControl.ReturnKeyType of
        TReturnKeyType.Default:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_ENTER;
        TReturnKeyType.Done:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_DONE;
        TReturnKeyType.Go:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_GO;
        TReturnKeyType.Next:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_NEXT;
        TReturnKeyType.Search:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_SEARCH;
        TReturnKeyType.Send:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_SEND;
      end;

      case VirtKBControl.KeyboardType of
        TVirtualKeyboardType.Default:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_TEXT;
        TVirtualKeyboardType.NumbersAndPunctuation:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NUMBER_AND_PUNCTUATION;
        TVirtualKeyboardType.NumberPad:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NUMBER;
        TVirtualKeyboardType.PhonePad:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_PHONE;
        TVirtualKeyboardType.Alphabet:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_ALPHABET;
        TVirtualKeyboardType.URL:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_URL;
        TVirtualKeyboardType.NamePhonePad:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NAME_PHONE_PAD;
        TVirtualKeyboardType.EmailAddress:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_EMAIL_ADDRESS;
      end;
      Password := VirtKBControl.IsPassword;
    end;

    if Supports(TAndroidWindowHandle(FormHandle).Form.Focused, IReadOnly, LReadOnly) then
    try
      ReadOnly := LReadOnly.ReadOnly;
    finally
      LReadOnly := nil;
    end
    else
      ReadOnly := True;

    if FTextView = nil then
      FTextView := PlatformAndroid.GetTextEditorProxy;

    if FTextView <> nil then
    begin
      if FTextListener = nil then
        FTextListener := TFMXTextListener.Create(Self);
      CalculateSelectionBounds(SelStart, SelEnd);
      PlatformAndroid.SynchronizeOnUIThread(
        procedure
        begin
          if FTextView <> nil then
          begin
            FTextView.setMaxLength(MaxLength);
            FTextView.setMultiline(MultiLine);
            FTextView.setReadOnly(ReadOnly);
            FTextView.setInputType(KbType);
            FTextView.setIsPassword(Password);
            FTextView.setEnterAction(RKType);
            FTextView.setText(StrToJCharSequence(FText));
            if (SelEnd - SelStart) > 0 then
              FTextView.setSelection(SelStart, SelEnd)
            else
              FTextView.setCursorPosition(CaretPosition.X);
            FTextView.addTextListener(FTextListener);
            MainActivity.getViewStack.addView(nil);
            FTextView.requestFocus;
          end;
        end);
    end;

    FMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TVKStateChangeMessage, HandleVK);
  end;
end;

procedure TTextServiceAndroid.ExitControl(const FormHandle: TWindowHandle);
begin
  if FMessageID > 0 then
  begin
    TMessageManager.DefaultManager.Unsubscribe(TVKStateChangeMessage, FMessageID);
    FMessageID := 0;
  end;
  if (FTextView <> nil) and (FTextListener <> nil) then
  begin
    FComposingBegin := -1;
    FComposingEnd := -1;
    PlatformAndroid.SynchronizeOnUIThread(
      procedure
      begin
        if FTextView <> nil then
        begin
          FTextView.setCursorPosition(FCaretPosition.X);
          FTextView.removeTextListener(FTextListener);
          FTextView.clearFocus;
          FTextView := nil;
        end;
        TWindowManager.Current.HideContextMenu;
      end);
    FTextListener := nil;
  end;
end;

function TTextServiceAndroid.GetCaretPostion: TPoint;
begin
  Result := FCaretPosition;
end;

procedure TTextServiceAndroid.SetCaretPosition(const Value: TPoint);
var
  SelStart, SelEnd: Integer;
begin
  if FCaretPosition <> Value then
  begin
    FCaretPosition := Value;
    CalculateSelectionBounds(SelStart, SelEnd);
    if (FTextView <> nil) and not FInternalUpdate then
      PlatformAndroid.SynchronizeOnUIThread(
        procedure
        begin
          if (SelEnd - SelStart) > 0 then
            FTextView.setSelection(SelStart, SelEnd)
          else
            FTextView.setCursorPosition(CaretPosition.X);
        end);
  end;
end;

function TTextServiceAndroid.GetText: string;
begin
  Result := FText;
end;

procedure TTextServiceAndroid.SetText(const Value: string);
begin
  if not SameStr(FText, Value) then
  begin
    FText := Value;
    UnpackText;
    if FTextView <> nil then
      PlatformAndroid.SynchronizeOnUIThread(
        procedure
        begin
          FTextView.setText(StrToJCharSequence(Value));
        end);
  end;
end;

procedure TTextServiceAndroid.HandleVK(const Sender: TObject; const M: TMessage);
var
  VirtKBControl: IVirtualKeyboardControl;
  KbType: Integer;
  RKType: Integer;
  LReadOnly: IReadOnly;
  ReadOnly, Password: Boolean;
begin
  if (FTextView <> nil) and (Screen.ActiveForm <> nil) and (Screen.ActiveForm.Focused <> nil) and
    TVKStateChangeMessage(M).KeyboardVisible then
  begin
    if Supports(Screen.ActiveForm.Focused, IVirtualKeyboardControl, VirtKBControl) then
    begin
      case VirtKBControl.ReturnKeyType of
        TReturnKeyType.Default:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_ENTER;
        TReturnKeyType.Done:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_DONE;
        TReturnKeyType.Go:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_GO;
        TReturnKeyType.Next:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_NEXT;
        TReturnKeyType.Search:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_SEARCH;
        TReturnKeyType.Send:
          RKType := TJFMXTextEditorProxy.JavaClass.ACTION_SEND;
      end;

      case VirtKBControl.KeyboardType of
        TVirtualKeyboardType.Default:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_TEXT;
        TVirtualKeyboardType.NumbersAndPunctuation:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NUMBER_AND_PUNCTUATION;
        TVirtualKeyboardType.NumberPad:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NUMBER;
        TVirtualKeyboardType.PhonePad:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_PHONE;
        TVirtualKeyboardType.Alphabet:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_ALPHABET;
        TVirtualKeyboardType.URL:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_URL;
        TVirtualKeyboardType.NamePhonePad:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_NAME_PHONE_PAD;
        TVirtualKeyboardType.EmailAddress:
          KbType := TJFMXTextEditorProxy.JavaClass.INPUT_EMAIL_ADDRESS;
      end;
      Password := VirtKBControl.IsPassword;
    end;

    if Supports(Screen.ActiveForm.Focused, IReadOnly, LReadOnly) then
    try
      ReadOnly := LReadOnly.ReadOnly;
    finally
      LReadOnly := nil;
    end
    else
      ReadOnly := False;

    PlatformAndroid.SynchronizeOnUIThread(
      procedure
      begin
        if FTextView <> nil then
        begin
          FTextView.setReadOnly(ReadOnly);
          FTextView.setInputType(KbType);
          FTextView.setIsPassword(Password);
          FTextView.setEnterAction(RKType);
        end;
      end);
  end;
end;

function TTextServiceAndroid.HasMarkedText: Boolean;
begin
  Result := (FComposingBegin >= 0) and (FComposingEnd >= 0) and (FComposingBegin < FComposingEnd);
end;

function TTextServiceAndroid.GetImeMode: TImeMode;
begin
  Result := FImeMode;
end;

procedure TTextServiceAndroid.CalculateSelectionBounds(out SelectionStart, SelectionEnd: Integer);
var
  TextInput: ITextInput;
  I: Integer;
  SelBounds: TRect;
  TopLeft, BottomRight: TPoint;
begin
  if Supports(Owner, ITextInput, TextInput) then
  begin
    if FLines.Count > 0 then
    begin
      SelBounds := TextInput.GetSelectionBounds;
      if (SelBounds.Top > SelBounds.Bottom) or ((SelBounds.Height = 0) and (SelBounds.Left > SelBounds.Right)) then
      begin
        TopLeft := SelBounds.BottomRight;
        BottomRight := SelBounds.TopLeft;
      end
      else
      begin
        TopLeft := SelBounds.TopLeft;
        BottomRight := SelBounds.BottomRight;
      end;

      SelectionStart := TopLeft.X;
      for I := 0 to Min(TopLeft.Y - 1, FLines.Count - 1) do
        Inc(SelectionStart, FLines[I].Length + FLines.LineBreak.Length);
      SelectionEnd := SelBounds.Right + (SelectionStart - SelBounds.Left);
      for I := Min(TopLeft.Y, FLines.Count - 1) to Min(BottomRight.Y - 1, FLines.Count - 1) do
        Inc(SelectionEnd, FLines[I].Length + FLines.LineBreak.Length);
    end
    else
    begin
      SelectionStart := Min(SelBounds.Left, SelBounds.Right);
      SelectionEnd := Max(SelBounds.Left, SelBounds.Right);
    end
  end
  else
  begin
    SelectionStart := FCaretPosition.X;
    SelectionEnd := FCaretPosition.X;
  end;
end;

procedure TTextServiceAndroid.SetImeMode(const Value: TImeMode);
begin
  FImeMode := Value;
end;

procedure TTextServiceAndroid.InternalUpdate;
begin
  FInternalUpdate := True;
  try
    (Owner as ITextInput).IMEStateUpdated;
  finally
    FInternalUpdate := False;
  end;
end;

procedure TTextServiceAndroid.InternalUpdateSelection;
var
  SelStart, SelEnd: Integer;
begin
  CalculateSelectionBounds(SelStart, SelEnd);
  PlatformAndroid.SynchronizeOnUIThread(
    procedure
    begin
      if FTextView <> nil then
        FTextView.setSelection(SelStart, SelEnd);
    end);
end;

procedure TTextServiceAndroid.ProcessUpdate(const Update: TTextUpdate);
begin
  FText := Update.Text;
  UnpackText;
  FCaretPosition.X := Update.X;
end;

procedure TTextServiceAndroid.PostProcessUpdate;
begin
  InternalUpdate;
end;

function TTextServiceAndroid.TargetClausePosition: TPoint;
begin
  Result := CaretPosition;
end;

procedure TTextServiceAndroid.UnpackText;
var
  HeaderBegin, HeaderEnd: Integer;
  LinesLength: TArray<string>;
  I, LineLength, LengthBefore: Integer;
begin
  FLines.Clear;
  HeaderBegin := FText.IndexOf('[');
  HeaderEnd := FText.IndexOf(']');
  if not FText.IsEmpty and (HeaderBegin >= 0) and (HeaderEnd > 0) then
  begin
    LinesLength := FText.Substring(HeaderBegin + 1, HeaderEnd - HeaderBegin - 1).Split([',']);
    LengthBefore := 0;
    for I := 0 to Length(LinesLength) - 1 do
    begin
      LineLength := StrToInt(LinesLength[I]);
      if LineLength > 0 then
        FLines.Add(FText.Substring(HeaderEnd + 1 + LengthBefore, LineLength))
      else
        FLines.Add(string.Empty);
      Inc(LengthBefore, LineLength);
    end;
  end;
end;

{ TFMXNativeActivityListener }

procedure TFMXNativeActivityListener.onCancelReceiveImage(ARequestCode: Integer);
begin
  TThread.Queue(nil, procedure
  begin
    TMessageManager.DefaultManager.SendMessage(nil, TMessageCancelReceivingImage.Create(ARequestCode));
  end);
end;

procedure TFMXNativeActivityListener.onReceiveImagePath(ARequestCode: Integer; AFileName: JString);
var
  Message: TMessageReceivedImagePath;
begin
  TThread.Queue(nil, procedure
  var
    ImageFileName: string;
  begin
    ImageFileName := JStringToString(AFileName);
    Message := TMessageReceivedImagePath.Create(ImageFileName);
    Message.RequestCode := ARequestCode;
    TMessageManager.DefaultManager.SendMessage(nil, Message);
  end);
end;

procedure TFMXNativeActivityListener.onReceiveNotification(P1: JIntent);
begin
  TThread.queue(nil,  // https://quality.embarcadero.com/browse/RSP-18092 - maybe not need anymore in tokyo
    procedure
    begin
      TMessageManager.DefaultManager.SendMessage(nil, TMessageReceivedNotification.Create(P1));
    end);
end;

procedure TFMXNativeActivityListener.onReceiveResult(ARequestCode, AResultCode: Integer; AResultObject: JIntent);
begin
  TThread.queue(nil,  // https://quality.embarcadero.com/browse/RSP-18092 - maybe not need anymore in tokyo
    procedure
    var
      Msg: TMessageResultNotification;
    begin
      Msg := TMessageResultNotification.Create(AResultObject);
      Msg.RequestCode := ARequestCode;
      Msg.ResultCode := AResultCode;
      TMessageManager.DefaultManager.SendMessage(nil, Msg);
    end);
end;

function ConvertPixelToPoint(const P: TPointF): TPointF;
begin
  Result := TWindowManager.Current.PixelToPoint(P);
end;

function ConvertPointToPixel(const P: TPointF): TPointF;
begin
  Result := TWindowManager.Current.PointToPixel(P);
end;

{ TTextServiceUpdater }

constructor TTextServiceUpdater.Create;
begin
  FTextUpdates := TThreadedQueue<TTextUpdate>.Create;
  FTextPostUpdate := TList<TTextServiceAndroid>.Create;
end;

destructor TTextServiceUpdater.Destroy;
begin
  FTextUpdates.Free;
  FTextPostUpdate.Free;
  inherited;
end;

procedure TTextServiceUpdater.QueueTextUpdate(const Update: TTextUpdate);
begin
  FTextUpdates.PushItem(Update);
end;

procedure TTextServiceUpdater.ProcessTextUpdates;
const
  TextEventsLimit = 9;
var
  Update: TTextUpdate;
  Count: Integer;
begin
  Count := 0;
  FTextPostUpdate.Clear;
  while (FTextUpdates.QueueSize > 0) and (Count < TextEventsLimit) do
  begin
    TWindowManager.Current.HideContextMenu;
    Update := FTextUpdates.PopItem;
    if Update.Service <> nil then
    begin
      Update.Service.ProcessUpdate(Update);
      if not FTextPostUpdate.Contains(Update.Service) then
        FTextPostUpdate.Add(Update.Service);
    end;
    Inc(Count);
  end;
  if FTextPostUpdate <> nil then
  begin
    for Count := 0 to FTextPostUpdate.Count - 1 do
      FTextPostUpdate[Count].PostProcessUpdate;
  end;
end;

end.
