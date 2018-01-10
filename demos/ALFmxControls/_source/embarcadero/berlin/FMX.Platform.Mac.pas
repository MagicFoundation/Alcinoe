{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Mac;

interface

{$SCOPEDENUMS ON}

uses
  System.TypInfo,
  Macapi.ObjectiveC, Macapi.CocoaTypes, System.Types, System.UITypes, System.Classes, System.Generics.Collections,
  Macapi.Foundation, Macapi.AppKit, FMX.Types, FMX.Platform, FMX.Text, FMX.Forms, FMX.Controls, FMX.Graphics;

type
  TMacWindowHandle = class(TWindowHandle)
  private class var
    FWindowHandles: TList<TMacWindowHandle>;
  private
    FHandle: TOCLocal;
    FBufferSize: TSize;
    FBuffer: CGContextRef;
    FBits: Pointer;
    FTrackingArea: NSTrackingArea;
    function GetWindow: NSWindow;
    function GetView: NSView;
    function GetGLView: NSOpenGLView;
    function GetForm: TCommonCustomForm;
    procedure UpdateLayer(const Ctx: CGContextRef);
    procedure CreateBuffer;
    procedure FreeBuffer;
  protected
    function GetScale: Single; override;
  public
    constructor Create(const AHandle: TOCLocal);
    destructor Destroy; override;
    class function FindForm(const window: NSWindow): TCommonCustomForm;
    property Wnd: NSWindow read GetWindow;
    property View: NSView read GetView;
    property TrackingArea: NSTrackingArea read FTrackingArea;
    property GLView: NSOpenGLView read GetGLView;
    property Form: TCommonCustomForm read GetForm;
    property Handle: TOCLocal read FHandle;
  end;

{ TFMXAlertDelegate }

  AlertDelegate = interface(NSObject)
    ['{F7AE5530-0A75-4303-8F62-7ABCEB6EF8FE}']
    procedure alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer); cdecl;
  end;

  TFMXAlertDelegate = class(TOCLocal, NSAlertDelegate)
  public
    Modal: Boolean;
    Results: array of Integer;
    Result: Integer;
    constructor Create; //Needed to build this object correctly.
    function GetObjectID: Pointer;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer); cdecl;
  end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TMacWindowHandle;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

function PlatformHookObserverCallback(CancelIdle: Boolean; Mask: NSUInteger = NSAnyEventMask): Boolean;

implementation

uses
  System.RTLConsts, System.SysUtils, System.SyncObjs, System.Rtti, System.Math, System.Messaging,
  System.RegularExpressions, System.StrUtils, System.Variants, System.IOUtils, Macapi.QuartzCore,
  Macapi.KeyCodes, Macapi.CoreGraphics, Macapi.ObjCRuntime, Macapi.CoreFoundation, Macapi.Helpers, Macapi.Mach,
  FMX.Consts, FMX.Dialogs, FMX.Dialogs.Mac, FMX.Menus, FMX.Canvas.Mac, FMX.Canvas.GPU, FMX.Context.Mac, FMX.KeyMapping, FMX.ExtCtrls,
  FMX.Gestures, FMX.Gestures.Mac, FMX.Styles, FMX.TextLayout, FMX.Types3D, FMX.Controls.Mac, FMX.Forms.Border.Mac,
  FMX.Helpers.Mac, FMX.Surfaces;

const
  IntervalPress = 1/SecsPerDay/10;
  WaitMessageTimeout = 0.1;
  SSpotlightFeature = 'Help';

type

{$M+}

  TEventKind = (Other, Key, MouseOther, MouseDown, MouseUp, MouseMove);

  TEventRec = record
    Event: NSEvent;
    EventKind: TEventKind;
    Window: NSWindow;
    View: NSView;
    Form: TCommonCustomForm;
    FormMousePos: TPointF;
    ScreenMousePos: TPointF;
    Shift: TShiftState;
    Button: TMouseButton;
    MouseInContent: Boolean;
    HandledInApp: Boolean;
    constructor Create(const AEvent: NSEvent);
  end;

function IsPopupForm(const Form: TCommonCustomForm): Boolean;
begin
  Result := (Form <> nil) and ((Form.FormStyle = TFormStyle.Popup) or (Form is TCustomPopupForm));
end;

  { TEventRec }

constructor TEventRec.Create(const AEvent: NSEvent);
var
  AutoReleasePool: NSAutoreleasePool;
  LMousePoint, LScreenPos: NSPoint;
  R: TRectF;
begin
  // In this record contains the existing information about event.
  FillChar(Self, SizeOf(Self), 0);
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Self.Event := AEvent;
  {$REGION 'event kind'}
    case AEvent.&type of
      NSKeyDown, NSKeyUp:
        Self.EventKind := TEventKind.Key;
      NSLeftMouseDown, NSRightMouseDown, NSOtherMouseDown:
        Self.EventKind := TEventKind.MouseDown;
      NSLeftMouseUp, NSRightMouseUp, NSOtherMouseUp:
        Self.EventKind := TEventKind.MouseUp;
      NSMouseMoved, NSLeftMouseDragged, NSRightMouseDragged, NSOtherMouseDragged:
        Self.EventKind := TEventKind.MouseMove;
      NSMouseEntered, NSMouseExited, NSScrollWheel:
        Self.EventKind := TEventKind.MouseOther;
    end;
  {$ENDREGION}
  {$REGION 'shift state'}
    Self.Shift := ShiftStateFromModFlags(AEvent.modifierFlags);
    case AEvent.&type of
      NSLeftMouseDown, NSLeftMouseUp, NSLeftMouseDragged:
        Self.Shift := Self.Shift + [ssLeft];
      NSRightMouseDown, NSRightMouseUp, NSRightMouseDragged:
        Self.Shift := Self.Shift + [ssRight];
      NSOtherMouseDown, NSOtherMouseUp, NSOtherMouseDragged:
        Self.Shift := Self.Shift + [ssMiddle];
      NSScrollWheel:
        if Abs(AEvent.deltaX) > Abs(AEvent.deltaY) then
          Self.Shift := Self.Shift + [ssHorizontal];
    end;
    if (Self.EventKind = TEventKind.MouseDown) and (AEvent.clickCount > 1) then
      Self.Shift := Self.Shift + [ssDouble];
  {$ENDREGION}
  {$REGION 'mouse button'}
    case AEvent.&type of
      NSLeftMouseDown, NSLeftMouseUp:
        Self.Button := TMouseButton.mbLeft;
      NSRightMouseDown, NSRightMouseUp:
        Self.Button := TMouseButton.mbRight;
      NSOtherMouseDown, NSOtherMouseUp:
        Self.Button := TMouseButton.mbMiddle;
    end;
  {$ENDREGION}
  {$REGION 'system objects, Form, mouse coodinates'}
    LMousePoint := AEvent.locationInWindow;
    LScreenPos := LMousePoint;
    if AEvent.window <> nil then
    begin
      Self.View := TNSView.Wrap(AEvent.window.contentView);
      Self.window := AEvent.window;
      Self.Form := TMacWindowHandle.FindForm(AEvent.window);
      Self.FormMousePos := TPointF.Create(LMousePoint.X, Self.View.bounds.size.height - LMousePoint.y);
      LScreenPos := AEvent.window.convertBaseToScreen(LMousePoint);
    end;
    Self.ScreenMousePos := TPointF.Create(LScreenPos.X,
      TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - LScreenPos.y);
  {$ENDREGION}
  {$REGION 'test cursor pos in form'}
    if Self.Form <> nil then
    begin
      if Self.Form is TCustomPopupForm then
        Self.MouseInContent := TCustomPopupForm(Self.Form).ScreenContentRect.Contains(Self.ScreenMousePos)
      else
      begin
        R := TRectF.Create(0, 0, Self.View.bounds.size.width, Self.View.bounds.size.height);
        Self.MouseInContent := R.Contains(Self.FormMousePos);
      end;
    end;
  {$ENDREGION}
  finally
    AutoReleasePool.release;
  end;
end;

type

  TOpenMenuItem = class(TMenuItem);
  TOpenCustomForm = class(TCommonCustomForm);

  THookEvent = procedure (const EventRec: TEventRec; var CancelIdle, CancelDefaultAction: Boolean) of object;

  IFMXApplicationDelegate = interface(NSApplicationDelegate)
   ['{A54E08CA-77CC-4F22-B6D9-833DD6AB696D}']
    procedure onMenuClicked(sender: NSMenuItem); cdecl;
  end;

  TFMXApplicationDelegate = class(TOCLocal, IFMXApplicationDelegate)
  public
    function applicationShouldTerminate(Notification: NSNotification): NSInteger; cdecl;
    procedure applicationWillTerminate(Notification: NSNotification); cdecl;
    procedure applicationDidFinishLaunching(Notification: NSNotification); cdecl;
    function applicationDockMenu(sender: NSApplication): NSMenu; cdecl;
    procedure onMenuClicked(sender: NSMenuItem); cdecl;
  end;

  TFMXMenuDelegate = class(TOCLocal, NSMenuDelegate)
  public class var
    FFMXMenuDictionary: TDictionary<Pointer, TMenuItem>;

  public
    class procedure RegisterMenu(const ALocalId: Pointer; const AMenuItem: TMenuItem);
    class procedure UnregisterMenu(const ALocalId: Pointer);
    class function GetMenuItem(const ALocalId: Pointer): TMenuItem;

    procedure menuNeedsUpdate(menu: NSMenu); cdecl;
    function numberOfItemsInMenu(menu: NSMenu): NSInteger; cdecl;
    function menu(menu: NSMenu; updateItem: NSMenuItem; atIndex: NSInteger; shouldCancel: Boolean): Boolean; overload; cdecl;
    procedure menu(menu: NSMenu; willHighlightItem: NSMenuItem); overload; cdecl;
    function menuHasKeyEquivalent(menu: NSMenu; forEvent: NSEvent; target: Pointer; action: SEL): Boolean; cdecl;
    procedure menuWillOpen(menu: NSMenu); cdecl;
    procedure menuDidClose(menu: NSMenu); cdecl;
    function confinementRectForMenu(menu: NSMenu; onScreen: NSScreen): NSRect; cdecl;
  end;

  { TPlatformCocoa }

  TCustomCursor = class
  private
    FData: NSData;
    FImage: NSImage;
    FCursor: NSCursor;
  public
    constructor Create(const ABytes: Pointer; const ALength: NSUInteger);
    destructor Destroy; override;
    property Cursor: NSCursor read FCursor;
  end;

  TPlatformCocoa = class(TInterfacedObject, IFMXApplicationService, IFMXSystemFontService, IFMXTimerService,
    IFMXWindowService, IFMXMenuService, IFMXDragDropService, IFMXCursorService, IFMXMouseService,
    IFMXScreenService, IFMXLocaleService, IFMXDialogService, IFMXTextService, IFMXContextService, IFMXCanvasService,
    IFMXWindowBorderService, IFMXHideAppService, IFMXSystemInformationService, IFMXLoggingService,
    IFMXFullScreenWindowService, IFMXListingService, IFMXSaveStateService, IFMXDeviceMetricsService,
    IFMXDefaultMetricsService, IFMXKeyMappingService)
  private const
    DefaultMacFontSize = 13;
  private type
    TCurrentMenu = (System, Default, Main);
    TMainMenuState = (Empty, Created, Recreated, Recreating);
  private
    NSApp: NSApplication;
    FFMXApplicationDelegate: TFMXApplicationDelegate;
    FFMXMenuDelegate: NSMenuDelegate;
    FRunLoopObserver: CFRunLoopObserverRef;
    FAppKitMod: HMODULE;
    FHandleCounter: TFmxHandle;
    FObjectiveCMap: TDictionary<TFmxHandle, IObjectiveC>;
    FNSHandles: TList<TFmxHandle>;
    FTimers: TList<TFmxHandle>;
    FModalStack: TStack<TCommonCustomForm>;
    FAlertCount: Integer;
    FRestartModal: Boolean;
    FMenuBar: NSMenu;
    FMainMenu: NSMenu;
    FInDestroyMenuItem: Boolean;
    FDefaultMenu: TCurrentMenu;
    FClassHookCount: Integer;
    FTerminating: Boolean;
    FCursor: TCursor;
    FCustomCursor: TCustomCursor;
    FDisableClosePopups: Boolean;
    FCanSetState: Boolean;
    FPerformKeyExecuted: Boolean;
    FHookEvent: THookEvent;
    FMenuStack: TStack<IMenuView>;
    FTitle: string;
    FSaveStateStoragePath: string;
    FMainMenuState: TMainMenuState;
    FStoredButtons: NSUInteger;
    FDefaultFontName: string;
    FKeyMapping: TKeyMapping;
    function NewFmxHandle: TFmxHandle;
    procedure ValidateHandle(FmxHandle: TFmxHandle);
    function HandleToObjC(FmxHandle: TFmxHandle): IObjectiveC; overload;
    function HandleToObjC(FmxHandle: TFmxHandle; const IID: TGUID; out Intf): Boolean; overload;
    function AllocHandle(const Objc: IObjectiveC): TFmxHandle;
    procedure DeleteHandle(FmxHandle: TFmxHandle);
    procedure CreateChildMenuItems(AChildMenu: IItemsContainer; AParentMenu: NSMenu);
    procedure DoReleaseWindow(AForm: TCommonCustomForm);
    procedure RemoveChildHandles(const AMenu: IItemsContainer);
    procedure HookFrame(const NSWin: NSWindow);
    procedure UnHookFrame(const NSWin: NSWindow);
    procedure ClearNSHandles;
    function NewNSMenu(const Text: string): NSMenu;
    function KeyProc(const Sender: TObject; const Form: TCommonCustomForm; const event: NSEvent; const IsInputContext,
      IsDown: Boolean): Boolean;
    procedure MouseEvent(const EventRec: TEventRec); overload;
    procedure MouseEvent(const Event: NSevent); overload;
    procedure CreateDefaultMenuItem;

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
    function GetSaveStateFileName(const ABlockName: string): string;
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

    procedure CreateFirstVisibleItem(var ANewMenu: NSMenu; var ANSMenuItem: NSMenuItem);
    procedure WakeMainThread(Sender: TObject);
    function HookObserverCallback(CancelIdle: Boolean; Mask: NSUInteger = NSAnyEventMask): Boolean;
    procedure MenuLoopEvent(const EventRec: TEventRec; var CancelIdle, CancelDefaultAction: Boolean);
    property HookEvent: THookEvent read FHookEvent write FHookEvent;
    function FindFormAtScreenPos(var AForm: TCommonCustomForm; const ScreenMousePos: TPointF): Boolean;
    function ShowContextMenu(const AForm: TCommonCustomForm; const AScreenMousePos: TPointF): Boolean;

    //IFMXKeyMappingService
    /// <summary>Registers a platform key as the given virtual key.</summary>
    function RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
    /// <summary>Unegisters a platform key as the given virtual key.</summary>
    function UnregisterKeyMapping(const PlatformKey: Word): Boolean;
    /// <summary>Obtains the virtual key from a given platform key.</summary>
    function PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
    /// <summary>Obtains the platform key from a given virtual key.</summary>
    function VirtualKeyToPlatformKey(const VirtualKey: Word): Word;

  public
    constructor Create;
    destructor Destroy; override;
    function DefaultAction(Key: Char; Shift: TShiftState): boolean;

    class function ClosePopupForms: Boolean;
    class function PrepareClosePopups(const SaveForm: TCommonCustomForm): Boolean;

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
    { IFMXHideAppService }
    function GetHidden: boolean;
    procedure SetHidden(const Value: boolean);
    procedure HideOthers;
    { IFMXSystemFontService }
    function GetDefaultFontFamilyName: string;
    function GetDefaultFontSize: Single;
    { IFMXTimerService }
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    procedure DestroyTimers;
    function GetTick: Double;
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
    { IFMXWindowBorderService }
    function CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
    { IFMXMenuService }
    procedure StartMenuLoop(const AView: IMenuView);
    function ShortCutToText(ShortCut: TShortCut): string;
    procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
    function TextToShortCut(Text: string): Integer;
    procedure CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);
    procedure UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);
    procedure DestroyMenuItem(const AItem: IItemsContainer);
    function IsMenuBarOnWindowBorder: boolean;
    procedure UpdateMenuBar;
    { IFMXDragDropService }
    procedure BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
    { IFMXCursorService }
    procedure SetCursor(const ACursor: TCursor);
    function GetCursor: TCursor;
    { IFMXMouseService }
    function GetMousePos: TPointF;
    { IFMXScreenService }
    function GetScreenSize: TPointF;
    function GetScreenScale: Single;
    function GetScreenOrientation: TScreenOrientation;
    procedure SetScreenOrientation(AOrientations: TScreenOrientations);
    { IFMXLocaleService }
    function GetCurrentLangID: string;
    function GetLocaleFirstDayOfWeek: string;
    function GetFirstWeekday: Byte;
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
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXContextService }
    procedure RegisterContextClasses;
    procedure UnregisterContextClasses;
    { IFMXCanvasService }
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    { IFMXSystemInformationService }
    function GetScrollingBehaviour: TScrollingBehaviours;
    function GetMinScrollThumbSize: Single;
    function GetCaretWidth: Integer;
    function GetMenuShowDelay: Integer;
    procedure Log(const Fmt: string; const Params: array of const);
    { IFMXWindowService }
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
    { IFMXDefaultMetricsService }
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;
  end;

  TMultiDisplayMac = class (TInterfacedObject, IFMXMultiDisplayService)
  private
    FDisplayCount: Integer;
    FWorkAreaRect: TRect;
    FDesktopRect: TRect;
    FDisplayList: TList<TDisplay>;
    function NSRectToRect(const ANSRect: NSRect): TRect;
    procedure UpdateDisplays;
    function FindDisplay(const screen: NSScreen): TDisplay;
  public
    destructor Destroy; override;
    procedure UpdateDisplayInformation;
    function GetDisplayCount: Integer;
    function GetDesktopCenterRect(const Size: TSize): TRect;
    function GetWorkAreaRect: TRect;
    function GetDesktopRect: TRect;
    function GetDisplay(const Index: Integer): TDisplay;
    function DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
    function DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
  end;

{$M+}
  TFMXWindow = class;
  TTextServiceCocoa = class;

  TFMXViewBase = class(TOCLocal, NSTextInputClient)
  private
    FOwner: TFMXWindow;
    FShift: TShiftState;
    FMarkedRange: NSRange;
    FBackingStore: NSMutableAttributedString;//NSTextStorage;
    FSelectedRange: NSRange;
    FGestureControl: TComponent;
    FCurrentGestureEngine: TPlatformGestureEngine;
    FSwipePoint: TPointF;
    FEventInfo: TGestureEventInfo;
  private
    procedure SetFirstGesturePoint(const AnEvent: NSEvent);
    procedure SetTrackGesturePoint(const AnEvent: NSEvent);
    function GetGestureControlUnderMouse(const APoint: TPointF): IGestureControl;
    function GetGestureEngineUnderMouse(const APoint: TPointF): TPlatformGestureEngine;
    procedure ReleaseGestureEngine;
    procedure DiscardGestureEngine;
  protected
    function GetNativeView: NSView;
  public
    constructor Create(const AOwner: TFMXWindow);
    destructor Destroy; override;
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    procedure drawRect(dirtyRect: NSRect); cdecl;
    procedure keyDown(event: NSEvent); cdecl;
    procedure keyUp(event: NSEvent); cdecl;
    procedure scrollWheel(event: NSEvent); cdecl;
    { mouse }
    procedure mouseMoved(theEvent: NSEvent); cdecl;
    procedure mouseDown(theEvent: NSEvent); cdecl;
    procedure mouseUp(theEvent: NSEvent); cdecl;
    procedure mouseDragged(theEvent: NSEvent); cdecl;
    procedure rightMouseDown(theEvent: NSEvent); cdecl;
    procedure rightMouseUp(theEvent: NSEvent); cdecl;
    procedure rightMouseDragged(theEvent: NSEvent); cdecl;
    procedure otherMouseDown(theEvent: NSEvent); cdecl;
    procedure otherMouseUp(theEvent: NSEvent); cdecl;
    procedure otherMouseDragged(theEvent: NSEvent); cdecl;
    {Touch and Gestures}
    procedure magnifyWithEvent(event: NSEvent); cdecl;
    procedure rotateWithEvent(event: NSEvent); cdecl;
    procedure swipeWithEvent(event: NSEvent); cdecl;
    procedure touchesBeganWithEvent(event: NSEvent); cdecl;
    procedure touchesCancelledWithEvent(event: NSEvent); cdecl;
    procedure touchesEndedWithEvent(event: NSEvent); cdecl;
    procedure touchesMovedWithEvent(event: NSEvent); cdecl;
    { NSTextInputClient }
    procedure insertText(text: Pointer {NSString}; replacementRange: NSRange); cdecl;
    procedure doCommandBySelector(selector: SEL); cdecl;
    procedure setMarkedText(text: Pointer {NSString}; selectedRange: NSRange; replacementRange: NSRange); cdecl;
    procedure unMarkText; cdecl;
    function selectedRange: NSRange; cdecl;
    function markedRange: NSRange; cdecl;
    function hasMarkedText: Boolean; cdecl;
    function attributedSubstringForProposedRange(aRange: NSRange; actualRange: PNSRange): NSAttributedString; cdecl;
    function validAttributesForMarkedText: Pointer {NSArray}; cdecl;
    function firstRectForCharacterRange(aRange: NSRange; actualRange: PNSRange): NSRect; cdecl;
    function characterIndexForPoint(aPoint: NSPoint): NSUInteger; cdecl;
    function attributedString: NSAttributedString; cdecl;
    function fractionOfDistanceThroughGlyphForPoint(aPoint: NSPoint): CGFloat; cdecl;
    function baselineDeltaForCharacterAtIndex(anIndex: NSUInteger): CGFloat; cdecl;
    function windowLevel: NSInteger; cdecl;
    function drawsVerticallyForCharacterAtIndex(charIndex: NSUInteger): Boolean; cdecl;
    { Text Service }
    function FocusedTextService: TTextServiceCocoa;
    procedure UpdateTextServiceControl;
    { }
    property NativeView: NSView read GetNativeView;
    property Owner: TFMXWindow read FOwner;
  end;

  FMXView = interface(NSView)
    ['{56304E8C-08A2-4386-B116-D4E364FDC2AD}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    procedure drawRect(dirtyRect: NSRect); cdecl;
    procedure keyDown(event: NSEvent); cdecl;
    procedure keyUp(event: NSEvent); cdecl;
    procedure scrollWheel(event: NSEvent); cdecl;
    { mouse }
    procedure mouseMoved(theEvent: NSEvent); cdecl;
    procedure mouseDown(theEvent: NSEvent); cdecl;
    procedure mouseUp(theEvent: NSEvent); cdecl;
    procedure mouseDragged(theEvent: NSEvent); cdecl;
    procedure rightMouseDown(theEvent: NSEvent); cdecl;
    procedure rightMouseUp(theEvent: NSEvent); cdecl;
    procedure rightMouseDragged(theEvent: NSEvent); cdecl;
    procedure otherMouseDown(theEvent: NSEvent); cdecl;
    procedure otherMouseUp(theEvent: NSEvent); cdecl;
    procedure otherMouseDragged(theEvent: NSEvent); cdecl;
    {Touch and Gestures}
    procedure magnifyWithEvent(event: NSEvent); cdecl;
    procedure rotateWithEvent(event: NSEvent); cdecl;
    procedure swipeWithEvent(event: NSEvent); cdecl;
    procedure touchesBeganWithEvent(event: NSEvent); cdecl;
    procedure touchesCancelledWithEvent(event: NSEvent); cdecl;
    procedure touchesEndedWithEvent(event: NSEvent); cdecl;
    procedure touchesMovedWithEvent(event: NSEvent); cdecl;
  end;

  TFMXView = class(TFMXViewBase, NSTextInputClient)
  public
    constructor Create(const AOwner: TFMXWindow; AFRameRect: NSRect);
    function GetObjectiveCClass: PTypeInfo; override;
    {Touch and Gestures}
  end;

  FMXView3D = interface(NSOpenGLView)
    ['{FC9E6699-53C6-4117-BAF0-A7BD455BAF75}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    procedure drawRect(dirtyRect: NSRect); cdecl;
    procedure keyDown(event: NSEvent); cdecl;
    procedure keyUp(event: NSEvent); cdecl;
    procedure scrollWheel(event: NSEvent); cdecl;
    { mouse }
    procedure mouseMoved(theEvent: NSEvent); cdecl;
    procedure mouseDown(theEvent: NSEvent); cdecl;
    procedure mouseUp(theEvent: NSEvent); cdecl;
    procedure mouseDragged(theEvent: NSEvent); cdecl;
    procedure rightMouseDown(theEvent: NSEvent); cdecl;
    procedure rightMouseUp(theEvent: NSEvent); cdecl;
    procedure rightMouseDragged(theEvent: NSEvent); cdecl;
    procedure otherMouseDown(theEvent: NSEvent); cdecl;
    procedure otherMouseUp(theEvent: NSEvent); cdecl;
    procedure otherMouseDragged(theEvent: NSEvent); cdecl;
    {Touch and Gestures}
    procedure magnifyWithEvent(event: NSEvent); cdecl;
    procedure rotateWithEvent(event: NSEvent); cdecl;
    procedure swipeWithEvent(event: NSEvent); cdecl;
    procedure touchesBeganWithEvent(event: NSEvent); cdecl;
    procedure touchesCancelledWithEvent(event: NSEvent); cdecl;
    procedure touchesEndedWithEvent(event: NSEvent); cdecl;
    procedure touchesMovedWithEvent(event: NSEvent); cdecl;
  end;

  TFMXView3D = class(TFMXViewBase, NSTextInputClient)
  public
    constructor Create(const AOwner: TFMXWindow; AFrameRect: NSRect);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
  end;

  FMXWindow = interface(NSWindow)
    ['{A4C4B329-38C4-401F-8937-1C380801B1C8}']
    function draggingEntered(Sender: Pointer): NSDragOperation; cdecl;
    procedure draggingExited(Sender: Pointer {id}); cdecl;
    function draggingUpdated(Sender: Pointer): NSDragOperation; cdecl;
    function performDragOperation(Sender: Pointer): Boolean; cdecl;
    function canBecomeKeyWindow: Boolean; cdecl;
    function canBecomeMainWindow: Boolean; cdecl;
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    function performKeyEquivalent(event: NSEvent): Boolean; cdecl;
  end;

  TFMXWindow = class(TOCLocal) //(NSWindow)
  private
    FViewObj: TFMXViewBase;
    FDelegate: NSWindowDelegate;
    FDelayRelease: Boolean;
    function CanActivate: Boolean; inline;
  protected
    function GetView: NSView;
    procedure UpdateWindowState;
  public
    NeedUpdateShadow: Boolean;
    Wnd: TCommonCustomForm;
    LastEvent: NSEvent; // for DragNDrop
    DragOperation: Integer;
    function GetObjectiveCClass: PTypeInfo; override;
    destructor Destroy; override;
    function windowShouldClose(Sender: Pointer {id}): Boolean; cdecl;
    procedure windowWillClose(notification: NSNotification); cdecl;
    procedure windowDidBecomeKey(notification: NSNotification); cdecl;
    procedure windowDidResignKey(notification: NSNotification); cdecl;
    procedure windowDidResize(notification: NSNotification); cdecl;
    procedure windowDidMiniaturize(notification: NSNotification); cdecl;
    procedure windowDidDeminiaturize(notification: NSNotification); cdecl;
    procedure windowDidEnterFullScreen(notification: NSNotification); cdecl;
    procedure windowDidExitFullScreen(notification: NSNotification); cdecl;
    procedure windowDidMove(notification: NSNotification); cdecl;
    procedure windowDidChangeBackingProperties(notification: NSNotification); cdecl;
    function draggingEntered(Sender: Pointer {NSDraggingInfo}): NSDragOperation; cdecl;
    procedure draggingExited(Sender: Pointer {NSDraggingInfo} {id}); cdecl;
    function draggingUpdated(Sender: Pointer {NSDraggingInfo}): NSDragOperation; cdecl;
    function performDragOperation(Sender: Pointer {NSDraggingInfo}): Boolean; cdecl;
    function acceptsFirstResponder: Boolean; cdecl;
    function canBecomeKeyWindow: Boolean; cdecl;
    function canBecomeMainWindow: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    function performKeyEquivalent(event: NSEvent): Boolean; cdecl;
    property View: NSView read GetView;
  end;
  PFMXWindow = ^TFMXWindow;

  { TTextServiceCocoa }
  TTextServiceCocoa = class(TTextService)
  private
    FCaretPosition: TPoint;
    FText : string;
    FMarkedText : string;
    FImeMode: TImeMode;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function GetCaretPostion: TPoint; override;
    procedure SetCaretPosition(const Value: TPoint); override;

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


    procedure DrawSingleLine(const Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: Integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload; override;

    procedure DrawSingleLine(const Canvas: TCanvas;
      const S: string;
      const ARect: TRectF;
      const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False); overload; override;

    function HasMarkedText: boolean; override;

    function GetImeMode: TImeMode; override;
    procedure SetImeMode(const Value: TImeMode); override;

    { Cocoa }
  private
    FMarkedRange: NSRange;
    FSelectedRange: NSRange;
  public
    constructor Create(const Owner: IControl; SupportMultiLine: Boolean); override;
    destructor Destroy; override;
    procedure SetMarkedRange(const Value: NSRange);
    procedure SetSelectedRange(const Value: NSRange);
  end;

procedure SetMenuBitmap(const ANSMenuItem: NSMenuItem; const Item: TMenuItem);
var
  Img: NSImage;
  LBitmap: TBitmap;
begin
  LBitmap := nil;
  if (ANSMenuItem <> nil) and (Item <> nil) then
  begin
    if Item.Images <> nil then
      LBitmap := Item.Images.Bitmap(TSizeF.Create(16, 16), Item.ImageIndex);
    if LBitmap = nil then
      LBitmap := Item.Bitmap;
    if (LBitmap <> nil) and not LBitmap.IsEmpty then
      Img := BitmapToMenuBitmap(LBitmap)
    else
      Img := nil;
    try
      ANSMenuItem.setImage(Img);
    finally
      if Img <> nil then
        Img.release;
    end;
  end;
end;

var
  PlatformCocoa: TPlatformCocoa;
  MultiDisplayMac: TMultiDisplayMac;

procedure RegisterCorePlatformServices;
begin
  PlatformCocoa := TPlatformCocoa.Create;
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXHideAppService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXTimerService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXWindowService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXMenuService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDragDropService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXCursorService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXScreenService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXLocaleService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDialogService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXContextService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXWindowBorderService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXLoggingService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXListingService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXSaveStateService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXKeyMappingService, PlatformCocoa);  
  MultiDisplayMac := TMultiDisplayMac.Create;
  TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, MultiDisplayMac);  
end;

procedure UnregisterCorePlatformServices;
begin
end;

type

  TMouseDownTimer = class (TTimer)
  private
    FEventRec: TEventRec;
    procedure TimerProc(Sender: TObject);
  protected
    procedure DoOnTimer; override;
  public
    constructor Create(const EventRec: TEventRec); reintroduce;
  end;

{ TMouseDownTimer }

constructor TMouseDownTimer.Create(const EventRec: TEventRec);
begin
  inherited Create(EventRec.Form);
  FEventRec := EventRec;
  OnTimer := TimerProc;
  Interval := 1;
  Enabled := True;
end;

procedure TMouseDownTimer.DoOnTimer;
begin
  Enabled := False;
  if not Released then
  begin
    Release;
    if (FEventRec.Form <> nil) and not FEventRec.Form.Released then
      inherited;
  end;
end;

procedure TMouseDownTimer.TimerProc(Sender: TObject);
var
  MenuDisplayed: Boolean;
begin
  TPlatformCocoa.PrepareClosePopups(nil);
  TPlatformCocoa.ClosePopupForms;

  if (ssRight in FEventRec.Shift) or ((ssLeft in FEventRec.Shift) and (ssCtrl in FEventRec.Shift)) then
    MenuDisplayed := PlatformCocoa.ShowContextMenu(FEventRec.Form, FEventRec.ScreenMousePos)
  else
    MenuDisplayed := False;

  if not MenuDisplayed then
    try
      FEventRec.Form.MouseDown(FEventRec.Button, FEventRec.Shift, FEventRec.FormMousePos.X, FEventRec.FormMousePos.Y);
      FEventRec.Form.MouseUp(FEventRec.Button, FEventRec.Shift, FEventRec.FormMousePos.X, FEventRec.FormMousePos.Y);
    except
      HandleException(FEventRec.Form);
    end;
end;

type
  TKeyDownInfo = record
    VKKeyCode: Integer;
    Key: Word;
    KeyChar: Char;
    Initialized: Boolean;
  end;

  TDownKeyList = class(TList<TKeyDownInfo>)
  strict private
    class var FCurrent: TDownKeyList;
    class function GetCurrent: TDownKeyList; static;
  private
    procedure AddKeyDown(const VKKeyCode: Integer);
    procedure InitializeKeyDown(const Key: Word; const KeyChar: Char);
    function RemoveKeyDown(const VKKeyCode: Integer; var Key: Word; var KeyChar: Char): Boolean;
    class destructor UnInitialize;
    class property Current: TDownKeyList read GetCurrent;
  end;

  { TDownKeyList }

class function TDownKeyList.GetCurrent: TDownKeyList;
begin
  if FCurrent = nil then
    FCurrent := TDownKeyList.Create;
  Result := FCurrent;
end;

class destructor TDownKeyList.UnInitialize;
begin
  FCurrent.DisposeOf;
  FCurrent := nil;
end;

procedure TDownKeyList.AddKeyDown(const VKKeyCode: Integer);
var
  Info: TKeyDownInfo;
begin
  if (VKKeyCode > 0) and ((Count = 0) or (Items[Count - 1].VKKeyCode <> VKKeyCode)) then
  begin
    FillChar(Info, SizeOf(Info), 0);
    Info.VKKeyCode := VKKeyCode;
    Add(Info);
  end;
end;

procedure TDownKeyList.InitializeKeyDown(const Key: Word; const KeyChar: Char);
var
  Info: TKeyDownInfo;
  I, LastIndex: Integer;
begin
  LastIndex := -1;
  for I := Count - 1 downto 0 do
    if not Items[I].Initialized then
      LastIndex := I
    else
      Break;

  if LastIndex >= 0 then
  begin
    Info := Items[LastIndex];
    Info.Key := Key;
    Info.KeyChar := KeyChar;
    Info.Initialized := True;
    Items[LastIndex] := Info;
    for I := Count - 1 downto LastIndex + 1 do
      Delete(I);
  end;
end;

function TDownKeyList.RemoveKeyDown(const VKKeyCode: Integer; var Key: Word; var KeyChar: Char): Boolean;
var
  Info: TKeyDownInfo;
  I: Integer;
begin
  Result := False;
  if VKKeyCode > 0 then
    for I := Count - 1 downto 0 do
    begin
      Info := Items[I];
      if Info.VKKeyCode = VKKeyCode then
      begin
        Delete(I);
        if not Result and Info.Initialized then
        begin
          Key := Info.Key;
          KeyChar := Info.KeyChar;
          Result := True;
        end;
      end;
    end;
end;

procedure DoUpdateKey(var Key: Word;
                      var Ch: WideChar;
                      var Shift: TShiftState);
begin
  //Zero out Key, if you pressed the usual character
  if (Ch >= ' ') and
     ((Shift * [ssCtrl, ssCommand]) = []) then
  begin
    Shift := Shift - [ssAlt];
    Key := 0;
  end;
  //Zero out Ch, if you pressed a keypad of the Alt, Ctrl, or Cmd
  if (([ssAlt, ssCtrl, ssCommand] * Shift) <> []) and (Key > 0) then
  begin
    Ch := #0;
  end;
end;

procedure DoKeyUp(const Sender: TObject;
                  Form: TCommonCustomForm;
                  Key: Word;
                  Ch: WideChar;
                  Shift: TShiftState);
begin
  if Form <> nil then
  try
    if not TDownKeyList.Current.RemoveKeyDown(Key, Key, Ch) then
      DoUpdateKey(Key, Ch, Shift);
    Form.KeyUp(Key, Ch, Shift);
  except
    HandleException(Sender);
  end;
end;

function DoKeyDown(const Sender: TObject;
                    const Form: TCommonCustomForm;
                    var Key: Word;
                    var Ch: WideChar;
                    Shift: TShiftState): boolean;
begin
  TDownKeyList.Current.AddKeyDown(Key);
  Result := False;
  if Form <> nil then
  try
    DoUpdateKey(Key, Ch, Shift);
    TDownKeyList.Current.InitializeKeyDown(Key, Ch);
    Form.KeyDown(Key, Ch, Shift);
    Result := Key = 0;
  except
    HandleException(Sender);
  end;
end;

var
  NSFMXPBoardtype: NSString;

function SendOSXMessage(const Sender: TObject; const OSXMessageClass: TOSXMessageClass;
  const NSSender: NSObject): NSObject;
var
  MessageObject: TOSXMessageObject;
begin
  if OSXMessageClass = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  MessageObject := TOSXMessageObject.Create(NSSender);
  try
    TMessageManager.DefaultManager.SendMessage(Sender, OSXMessageClass.Create(MessageObject, False), True);
    Result := MessageObject.ReturnValue;
  finally
    MessageObject.Free;
  end;
end;

{ TApplicationDelegate }

function TFMXApplicationDelegate.applicationDockMenu(sender: NSApplication): NSMenu;
var
  ReturnValue: NSObject;
begin
  ReturnValue := SendOSXMessage(Self, TApplicationDockMenuMessage, sender);
  if ReturnValue <> nil then
    Result := ReturnValue as NSMenu
  else
    Result := nil;
end;

function TFMXApplicationDelegate.applicationShouldTerminate(Notification: NSNotification): NSInteger;
begin
  if (Application = nil) or (PlatformCocoa = nil) or PlatformCocoa.Terminating or
    PlatformCocoa.DefaultAction('Q', [ssCommand]) then
    Result := NSTerminateNow
  else
    Result := NSTerminateCancel;
end;

procedure TFMXApplicationDelegate.applicationWillTerminate(Notification: NSNotification);
begin
  SendOSXMessage(Self, TApplicationWillTerminateMessage, Notification);
  Halt;
end;

procedure TFMXApplicationDelegate.onMenuClicked(sender: NSMenuItem);
begin
  SendOSXMessage(Self, TApplicationMenuClickedMessage, sender);
end;

procedure TFMXApplicationDelegate.applicationDidFinishLaunching(Notification: NSNotification);
begin
  SendOSXMessage(Self, TApplicationDidFinishLaunchingMessage, Notification);
end;

{ TPlatformCocoa }

constructor TPlatformCocoa.Create;
var
  AutoReleasePool: NSAutoreleasePool;
begin
  inherited;
  FDefaultMenu := TCurrentMenu.System;
  FAppKitMod := LoadLibrary('/System/Library/Frameworks/AppKit.framework/AppKit');
  AutoReleasePool := TNSAutoreleasePool.Alloc;
  try
    AutoReleasePool.init;
    NSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
    FFMXApplicationDelegate := TFMXApplicationDelegate.Create;
    FFMXMenuDelegate := TFMXMenuDelegate.Create;
    NSApp.setDelegate(IFMXApplicationDelegate(FFMXApplicationDelegate));
    Application := TApplication.Create(nil);
    FObjectiveCMap := TDictionary<TFmxHandle, IObjectiveC>.Create;
  finally
    AutoReleasePool.release;
  end;
  FNSHandles := TList<TFmxHandle>.Create;
  FTimers := TList<TFmxHandle>.Create;
  NSFMXPBoardtype := StrToNSStr('NSFMXPBoardtype' + IntToStr(Integer(Pointer(Application))));
  FCanSetState := True;
  FKeyMapping := TKeyMapping.Create;
  System.Classes.WakeMainThread := WakeMainThread;
end;

destructor TPlatformCocoa.Destroy;
begin
  DestroyTimers;
  FTimers.Free;
  ClearNSHandles;
  FNSHandles.Free;
  FreeAndNil(FMenuStack);
  FreeAndNil(FModalStack);
  FreeLibrary(FAppKitMod);
  FreeAndNil(Application);
  FObjectiveCMap.Free;
  TFMXMenuDelegate.FFMXMenuDictionary.Free;
  FKeyMapping.Free;
  inherited;
end;

function TPlatformCocoa.DefaultAction(Key: Char; Shift: TShiftState): boolean;
var
  Form: TCommonCustomForm;
begin
  Result := False;
  if (Shift = [ssCommand]) then
  begin
    if Key = 'Q' then
    begin
      try
        if Application.MainForm <> nil then
        begin
          Application.MainForm.Close;
          if not Terminating then
            Exit;
        end
        else
        begin
          if Screen <> nil then
            Screen.ActiveForm := nil;
          Application.Terminate;
        end;
      except
        HandleException(Application);
      end;
      Result := True;
    end
    else if Key = 'H' then
    begin
      SetHidden(not GetHidden);
      Result := True;
    end
    else if (Key = 'W') and (Screen <> nil) then
    begin
      Form := Screen.ActiveForm;
      if (Form <> nil) and (not (TFmxFormState.Modal in Form.FormState)) then
      begin
        Form.Close;
        Result := True;
      end;
    end;
  end;
  if (Shift = [ssAlt, ssCommand]) and (Key = 'H') then
  begin
    HideOthers;
    Result := True;
  end;
end;

function VKeyFromKeyCode(AKeyCode: Word; Shift: TShiftState; var ISFNkey: boolean): Integer;
var
  Kind: TKeyKind;
begin
  Result:= PlatformCocoa.PlatformKeyToVirtualKey(AKeyCode, Kind);
  ISFNkey := False;
  ISFNkey := Kind = TKeyKind.Functional;
end;

function TPlatformCocoa.KeyProc(const Sender: TObject; const Form: TCommonCustomForm; const Event: NSEvent;
  const IsInputContext, IsDown: Boolean): Boolean;
var
  Shift: TShiftState;
  Key: Word;
  Ch: WideChar;
  ShortcutKey: string;
  I: Integer;
  NSChars: NSString;
  ISFNkey, IsModified: Boolean;
  LHandle: TMacWindowHandle;
begin
  Result := False;
  Shift := ShiftStateFromModFlags(Event.modifierFlags);
  IsModified := [ssAlt, ssCtrl, ssCommand] * Shift <> [];
  Key := VKeyFromKeyCode(Event.keyCode, Shift, ISFNkey);
  Ch := #0;
  // Do not handle the modified letters in the KeyDown event
  if not IsInputContext and not ISFNkey and IsDown then
  begin
    TDownKeyList.Current.AddKeyDown(Key);
    Exit;
  end;
  if (Key <> 0) and (IsInputContext or ISFNkey) then
  begin
    if not IsInputContext then
      Result := Key < vkSpace
    else
      Result := ISFNkey;
  end;
  if Result or ISFNkey or IsModified then
  begin
    Result := False;
    if Form <> nil then
    begin
      LHandle := WindowHandleToPlatform(Form.Handle);
      if (LHandle.Handle is TFMXWindow) and (not IsInputContext or (Key <> vkEscape)) then
      begin
        if IsModified and not ISFNkey then
        begin
          NSChars := Event.charactersIgnoringModifiers;
          if NSChars <> nil then
            ShortcutKey := UTF8ToString(NSChars.UTF8String);
          for I := 0 to ShortcutKey.Length - 1 do
          begin
            if (UpCase(ShortcutKey.Chars[I]) >= 'A') and (UpCase(ShortcutKey.Chars[I]) <= 'Z') then
              Key := Word(UpCase(ShortcutKey.Chars[I]));
          end;
        end;
        if IsDown then
          Result := DoKeyDown(Sender, Form, Key, Ch, Shift)
        else
          DoKeyUp(Sender, Form, Key, Ch, Shift);
      end;
    end;
    if not Result and IsDown then
      Result := DefaultAction(Char(Key), Shift);
  end
  else
  begin
    // handle is not modified by the letter
    NSChars := Event.charactersIgnoringModifiers;
    if NSChars <> nil then
      ShortcutKey := UTF8ToString(NSChars.UTF8String);
    for I := 0 to ShortcutKey.Length - 1 do
    begin
      Ch := ShortcutKey.Chars[I];
      Key := Word(Ch);
      if IsDown then
        Result := DoKeyDown(Sender, Form, Key, Ch, Shift)
      else
        DoKeyUp(Sender, Form, Key, Ch, Shift);
    end;
    if not IsInputContext then
      Result := True;
  end;
end;

procedure TPlatformCocoa.MouseEvent(const EventRec: TEventRec);
var
  LHandle: TMacWindowHandle;
  MenuDisplayed: Boolean;
  OldDisableClosePopups: Boolean;
  Obj: IControl;

  procedure InitLastEvent;
  begin
    if (EventRec.Form <> nil) and (EventRec.Form.Handle <> nil) and
      (EventRec.EventKind in [TEventKind.MouseDown, TEventKind.MouseUp, TEventKind.MouseMove]) then
    begin
      LHandle := WindowHandleToPlatform(EventRec.Form.Handle);
      if LHandle.Handle is TFMXWindow then
      begin
        if EventRec.EventKind = TEventKind.MouseUp then
          TFMXWindow(LHandle.Handle).LastEvent := nil
        else
          TFMXWindow(LHandle.Handle).LastEvent := EventRec.Event;
      end;
    end;
  end;

  procedure CleanLastEvent;
  begin
    if (EventRec.Form <> nil) and not EventRec.Form.Released and (EventRec.Form.Handle <> nil) and
      (EventRec.EventKind = TEventKind.MouseMove) and (LHandle.Handle is TFMXWindow) then
    begin
      LHandle := WindowHandleToPlatform(EventRec.Form.Handle);
      if LHandle.Handle is TFMXWindow then
        TFMXWindow(LHandle.Handle).LastEvent := nil;
    end;
  end;

begin
  if EventRec.Form = nil then
    Exit;
  InitLastEvent;
  try
    case EventRec.EventKind of
      TEventKind.MouseDown:
        begin
          // Popups
          PrepareClosePopups(EventRec.Form);
          // Activate
          if not EventRec.Form.Active and (EventRec.Form.FormStyle <> TFormStyle.Popup) then
          begin
            OldDisableClosePopups := FDisableClosePopups;
            try
              FDisableClosePopups := True;
              Activate(EventRec.Form);
            finally
              FDisableClosePopups := OldDisableClosePopups;
            end;
            Obj := EventRec.Form.ObjectAtPoint(EventRec.ScreenMousePos);
            if (Obj <> nil) and (Obj.GetObject is TControl) and (EventRec.Button = TMouseButton.mbLeft) and
              (TControl(Obj.GetObject).DragMode = TDragMode.dmAutomatic) then
              EventRec.Form.MouseDown(EventRec.Button, EventRec.Shift, EventRec.FormMousePos.X,
                EventRec.FormMousePos.Y)
            else
            begin
              LHandle := WindowHandleToPlatform(EventRec.Form.Handle);
              if LHandle.Handle is TFMXWindow then
                TFMXWindow(LHandle.Handle).LastEvent := nil;

              TMouseDownTimer.Create(EventRec);
            end;
          end
          else
          begin
            // Context menu and mouse down
            if (EventRec.Button = TMouseButton.mbRight) or ((EventRec.Button = TMouseButton.mbLeft) and
              (ssCtrl in EventRec.Shift)) then
              MenuDisplayed := ShowContextMenu(EventRec.Form, EventRec.ScreenMousePos)
            else
              MenuDisplayed := False;
            if not MenuDisplayed then
            try
              EventRec.Form.MouseDown(EventRec.Button, EventRec.Shift, EventRec.FormMousePos.X,
                EventRec.FormMousePos.Y);
            except
              HandleException(EventRec.Form);
            end;
          end;
        end;
      TEventKind.MouseUp:
        begin
          // mouse up
          try
            EventRec.Form.MouseUp(EventRec.Button, EventRec.Shift, EventRec.FormMousePos.X, EventRec.FormMousePos.Y);
          except
            HandleException(EventRec.Form);
          end;
          // popups
          ClosePopupForms;
        end;
      TEventKind.MouseMove:
        try
          EventRec.Form.MouseMove(EventRec.Shift, EventRec.FormMousePos.X, EventRec.FormMousePos.Y);
        except
          HandleException(EventRec.Form);
        end;
    end;
  finally
    CleanLastEvent;
  end;
end;

procedure TPlatformCocoa.MouseEvent(const Event: NSEvent);
var
  LEventRec: TEventRec;
begin
  LEventRec := TEventRec.Create(Event);
  MouseEvent(LEventRec);
end;

{ App =========================================================================}

procedure RunLoopObserverCallback(observer: CFRunLoopObserverRef; activity: CFRunLoopActivity; info: Pointer); cdecl;
const
  DefaultMask = NSSystemDefined or NSAppKitDefinedMask or NSApplicationDefinedMask or NSLeftMouseDownMask;
  WithoutEventHandling = 0;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    CheckSynchronize;

  if (PlatformCocoa <> nil) and (PlatformCocoa.NSApp <> nil) and (PlatformCocoa.FAlertCount = 0) then
  begin
    if (PlatformCocoa.FMainMenuState = TPlatformCocoa.TMainMenuState.Created) and (Application <> nil) and
      (Application.MainForm <> nil) then
    begin
      PlatformCocoa.FMainMenuState := TPlatformCocoa.TMainMenuState.Recreated;
      if TOpenCustomForm(Application.MainForm).MainMenu <> nil then
        TMainMenu(TOpenCustomForm(Application.MainForm).MainMenu).RecreateOSMenu
      else
        Application.MainForm.RecreateOsMenu;
    end;
    if PlatformCocoa.NSApp.isActive then
      PlatformCocoa.HookObserverCallback(False, WithoutEventHandling)
    else
      PlatformCocoa.HookObserverCallback(False, DefaultMask);
  end;
end;

class function TPlatformCocoa.ClosePopupForms: Boolean;
begin
  Result := False;
  if Screen <> nil then
    try
      Result := Screen.ClosePopupForms;
    except
      HandleException(Screen);
    end;
end;

class function TPlatformCocoa.PrepareClosePopups(const SaveForm: TCommonCustomForm): Boolean;
begin
  Result := False;
  if Screen <> nil then
    try
      Result := Screen.PrepareClosePopups(SaveForm);
    except
      HandleException(Screen);
    end;
end;

function TPlatformCocoa.HookObserverCallback(CancelIdle: Boolean; Mask: NSUInteger = NSAnyEventMask): Boolean;

  procedure Idle;
  var
    Done: Boolean;
  begin
    Done := False;
    if Application <> nil then
      try
        FPerformKeyExecuted := False;
        Application.DoIdle(Done);
      except
        HandleException(Application);
      end;
  end;

  procedure DefaultMouseAction(const AEventRec: TEventRec);
  var
    TopModal: Boolean;
  begin
    if AEventRec.Form <> nil then
    begin
      if (AEventRec.Form.FormStyle <> TFormStyle.Popup) and (AEventRec.Shift * [ssLeft, ssRight, ssMiddle] <> []) and
        (AEventRec.EventKind in [TEventKind.MouseMove, TEventKind.MouseDown]) and not AEventRec.MouseInContent then
      begin
        PrepareClosePopups(AEventRec.Form);
        ClosePopupForms;
      end;
      if AEventRec.EventKind = TEventKind.MouseDown then
      begin
        TopModal := (FModalStack = nil) or (FModalStack.Count = 0) or (FModalStack.Peek = AEventRec.Form);
        // if LeftClick on inactive form then perform event of mouse
        if (FAlertCount = 0) and TopModal and (AEventRec.Button = TMouseButton.mbLeft) and not AEventRec.Form.Active and
          (AEventRec.Form.FormStyle <> TFormStyle.Popup) then
          MouseEvent(AEventRec);
      end;
    end;
  end;

  procedure EmulateCaptionClick;
  var
    Buttons: NSUInteger;
    Form: TCommonCustomForm;
    Pos: TPointF;
  begin
    Buttons := TNSEvent.OCClass.pressedMouseButtons;
    try
      if (Buttons <> 0) and (Buttons <> FStoredButtons) and (Screen <> nil) and (Screen.PopupFormCount > 0) then
      try
        Pos := GetMousePos;
        if FindFormAtScreenPos(Form, Pos) then
        begin
          if Form.FormStyle <> TFormStyle.Popup then
          begin
            Pos := Form.ScreenToClient(Pos);
             if Pos.Y < 0 then
            begin
              PrepareClosePopups(nil);
              ClosePopupForms;
            end;
          end; // else do nothing
        end
        else
        begin
          PrepareClosePopups(nil);
          ClosePopupForms;
        end;
      except
        HandleException(Application);
      end;
    finally
      FStoredButtons := Buttons;
    end;
  end;

var
  AutoReleasePool: NSAutoreleasePool;
  CancelDefaultAction: Boolean;
  LEvent: NSEvent;
  LEventRec: TEventRec;
  OldDisableClosePopups: Boolean;
  TimeoutDate: NSDate;
const
  WaitTimeout = 0.001;
begin
  Result := False;
  CancelDefaultAction := False;
  LEvent := nil;
  if not CancelIdle then
    Idle;

  if Mask <> 0 then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(WaitTimeout));
      LEvent := NSApp.nextEventMatchingMask(Mask, TimeoutDate, NSDefaultRunLoopMode, True);
      try
        if LEvent <> nil then
        begin
          Result := True;
          LEventRec := TEventRec.Create(LEvent);
          LEventRec.HandledInApp := True;
          if Assigned(HookEvent) then
            try
              HookEvent(LEventRec, CancelIdle, CancelDefaultAction);
            except
              HandleException(LEventRec.Form);
            end;
          if not CancelDefaultAction then
            DefaultMouseAction(LEventRec);
        end;
      finally
        if not CancelDefaultAction and (LEvent <> nil) then
        begin
          OldDisableClosePopups := FDisableClosePopups;
          try
            FDisableClosePopups := True;
            NSApp.sendEvent(LEvent);
          finally
            FDisableClosePopups := OldDisableClosePopups;
          end;
        end;
        FPerformKeyExecuted := False;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
  if (Screen <> nil) and (Screen.PopupFormCount > 0) then
    EmulateCaptionClick;
end;

procedure TPlatformCocoa.Run;
begin
  Application.RealCreateForms;
  FRunLoopObserver := CFRunLoopObserverCreate(kCFAllocatorDefault, kCFRunLoopBeforeWaiting, True, 0,
    RunLoopObserverCallback, nil);
  CFRunLoopAddObserver(CFRunLoopGetCurrent, FRunLoopObserver, kCFRunLoopCommonModes);
  NSApp.Run;
end;

function TPlatformCocoa.Terminating: boolean;
begin
  Result := FTerminating;
end;

procedure TPlatformCocoa.Terminate;
begin
  FTerminating := True;
  NSApp.terminate(nil);
end;

function TPlatformCocoa.HandleToObjC(FmxHandle: TFmxHandle): IObjectiveC;
begin
  TMonitor.Enter(FObjectiveCMap);
  try
    ValidateHandle(FmxHandle);
    if FObjectiveCMap.ContainsKey(FmxHandle) then
      Result := FObjectiveCMap[FmxHandle]
    else
      Result := nil;
  finally
    TMonitor.Exit(FObjectiveCMap);
  end;
end;

function TPlatformCocoa.HandleMessage: Boolean;
begin
  HookObserverCallback(True);
  Result := False;
end;

procedure TPlatformCocoa.WaitMessage;
var
  TimeoutDate: NSDate;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(WaitMessageTimeout));
    NSApp.nextEventMatchingMask(NSAnyEventMask, TimeoutDate, NSDefaultRunLoopMode, False);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.WakeMainThread(Sender: TObject);
var
  NullEvent: NSEvent;
  Origin: NSPoint;
  AutoReleasePool: NSAutoReleasePool;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      NullEvent := TNSEvent.Wrap(TNSEvent.OCClass.otherEventWithType(NSApplicationDefined, Origin, 0, Now, 0, nil, 0, 0, 0));
      TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).postEvent(NullEvent, True);
    finally
      AutoReleasePool.release;
    end;
  end;
end;

function TPlatformCocoa.GetDefaultTitle: string;
var AppNameKey: Pointer;
    AppBundle: NSBundle;
    NSAppName: NSString;
    AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    AppNameKey := (StrToNSStr('CFBundleName') as ILocalObject).GetObjectID;
    AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
    NSAppName := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppNameKey));
    Result := UTF8ToString(NSAppName.UTF8String);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetTitle: string;
begin
  Result := FTitle;
end;

function TPlatformCocoa.GetVersionString: string;
var
  VersionObject: Pointer;
begin
  VersionObject := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).infoDictionary.objectForKey((
    StrToNSStr('CFBundleVersion') as ILocalObject).GetObjectID); // do not localize
  if VersionObject <> nil then
    Result := NSStrToStr(TNSString.Wrap(VersionObject))
  else
    Result := string.Empty;
end;

procedure TPlatformCocoa.SetTitle(const Value: string);
begin
  if FTitle <> Value then
    FTitle := Value;
end;

function TPlatformCocoa.GetHidden: boolean;
begin
  Result := NSApp.isHidden;
end;

procedure TPlatformCocoa.SetHidden(const Value: boolean);
begin
  if Value <> GetHidden then
  begin
    if Value then
      NSApp.Hide(Self)
    else
      NSApp.UnHide(Self);
  end;
end;

procedure TPlatformCocoa.SetScreenOrientation(
  AOrientations: TScreenOrientations);
begin
  // Not needed for MAC
end;

procedure TPlatformCocoa.SetShowFullScreenIcon(const AForm: TCommonCustomForm;
  const AValue: Boolean);
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if AValue then
      NSWin.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary)
    else
      NSWin.setCollectionBehavior(NSWindowCollectionBehaviorDefault);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.HideOthers;
begin
  NSApp.hideOtherApplications(Self);
end;

function TPlatformCocoa.GetDisplayMetrics: TDeviceDisplayMetrics;
const
  MacBasePPI = 110;
var
  Screen: NSScreen;
  ScreenSize: TPointF;
  ScreenScale: Single;
begin
  Screen := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen);
  ScreenSize := TPointF(Screen.frame.size);
  ScreenScale := Screen.backingScaleFactor;

  Result.PhysicalScreenSize := TSize.Create(Trunc(ScreenSize.X * ScreenScale), Trunc(ScreenSize.Y * ScreenScale));
  Result.RawScreenSize := Result.PhysicalScreenSize;
  Result.LogicalScreenSize := TSize.Create(Trunc(ScreenSize.X), Trunc(ScreenSize.Y));
  if Abs(ScreenSize.X) > 0 then
    Result.AspectRatio := ScreenSize.Y / ScreenSize.X
  else
    Result.AspectRatio := 1;
  Result.PixelsPerInch := Trunc(MacBasePPI * GetScreenScale);
  Result.ScreenScale := ScreenScale;
  Result.FontScale := ScreenScale;
end;

function TPlatformCocoa.RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
begin
    Result := FKeyMapping.RegisterKeyMapping(PlatformKey, VirtualKey, KeyKind);
end;

function TPlatformCocoa.UnregisterKeyMapping(const PlatformKey: Word): Boolean;
begin
    Result := FKeyMapping.UnregisterKeyMapping(PlatformKey);
end;

function TPlatformCocoa.PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
begin
    Result := FKeyMapping.PlatformKeyToVirtualKey(PlatformKey, KeyKind);
end;

function TPlatformCocoa.VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
begin
    Result := FKeyMapping.VirtualKeyToPlatformKey(VirtualKey);
end;

{ Timer =======================================================================}

type
  CocoaTimer = interface(NSObject)
    ['{337887FF-BA77-4703-BE0E-34DC1CB26276}']
    procedure timerEvent; cdecl;
                                                                                  
    procedure release; cdecl;
  end;

  TCocoaTimer = class(TOCLocal)
  private
    FFunc : TTimerProc;
  public
    function GetObjectiveCClass: PTypeInfo; override;
    procedure timerEvent; cdecl;
    procedure SetTimerFunc(AFunc: TTimerProc);
    procedure release; cdecl;
  end;

function TCocoaTimer.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(CocoaTimer);
end;

procedure TCocoaTimer.timerEvent;
begin
  if Assigned(@FFunc) then
  try
    FFunc;
  except
    HandleException(nil);
  end;
end;

procedure TCocoaTimer.release;
var
  RC: Integer;
begin
  RC := NSObject(Super).retainCount;
  NSObject(Super).release;
  if RC = 1 then
    Destroy;
end;

procedure TCocoaTimer.SetTimerFunc(AFunc: TTimerProc);
begin
  FFunc := AFunc;
end;

function TPlatformCocoa.CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
var
  Timer: NSTimer;
  User: TCocoaTimer;
  LInterval: NSTimeInterval;
begin
  Result := 0;
  if not FTerminating and (Interval > 0) and Assigned(TimerFunc) then
  begin
    User := TCocoaTimer.Create;
    try
      User.SetTimerFunc(TimerFunc);
      LInterval := Interval / 1000;

      Timer := TNSTimer.Wrap(TNSTimer.OCClass.scheduledTimerWithTimeInterval(LInterval, User.GetObjectID,
        sel_getUid('timerEvent'), User.GetObjectID, True));

      Result := AllocHandle(Timer);
      FTimers.Add(Result);
    finally
      // User is retained twice (because it's target) by the timer and released twice on timer invalidation.
      NSObject(User.Super).release;
    end;
  end;
end;

function TPlatformCocoa.DestroyTimer(Timer: TFmxHandle): Boolean;
var
  CocoaTimer: NSTimer;
  I: Integer;
begin
  Result := False;
  if HandleToObjC(Timer, NSTimer, CocoaTimer) then
  begin
    Result := True;
    CocoaTimer.invalidate;
    DeleteHandle(Timer);
    for I := FTimers.Count - 1 downto 0 do
      if FTimers[I] = Timer then
      begin
        FTimers.Delete(I);
        Break;
      end;
  end;
end;

procedure TPlatformCocoa.DestroyTimers;
var
  I: Integer;
begin
  for I := FTimers.Count - 1 downto 0 do
  try
    DestroyTimer(FTimers[I]);
  except
    Continue;
  end;
end;

function TPlatformCocoa.GetTick: Double;
const
  NanoToSeconds = 1E-9;
begin
  Result := AbsoluteToNanoseconds(mach_absolute_time) * NanoToSeconds;
end;

{ Window ======================================================================}

const
  kCGBaseWindowLevelKey = 0;
  kCGMinimumWindowLevelKey = 1;
  kCGDesktopWindowLevelKey = 2;
  kCGBackstopMenuLevelKey = 3;
  kCGNormalWindowLevelKey = 4;
  kCGFloatingWindowLevelKey = 5;
  kCGTornOffMenuWindowLevelKey = 6;
  kCGDockWindowLevelKey = 7;
  kCGMainMenuWindowLevelKey = 8;
  kCGStatusWindowLevelKey = 9;
  kCGModalPanelWindowLevelKey = 10;
  kCGPopUpMenuWindowLevelKey = 11;
  kCGDraggingWindowLevelKey = 12;
  kCGScreenSaverWindowLevelKey = 13;
  kCGMaximumWindowLevelKey = 14;
  kCGOverlayWindowLevelKey = 15;
  kCGHelpWindowLevelKey = 16;
  kCGUtilityWindowLevelKey = 17;
  kCGDesktopIconWindowLevelKey = 18;
  kCGCursorWindowLevelKey = 19;
  kCGAssistiveTechHighWindowLevelKey = 20;
  kCGNumberOfWindowLevelKeys = 21; { Must be last. }

{ TFMXView }

function TFMXView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXView);
end;

constructor TFMXView.Create(const AOwner: TFMXWindow; AFrameRect: NSRect);
var
  V: Pointer;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    inherited Create(AOwner);
    V := NSView(Super).initWithFrame(AFrameRect);
    if GetObjectID <> V then
      UpdateObjectID(V);
    NSView(Super).setAcceptsTouchEvents(True);
  finally
    AutoReleasePool.release;
  end;
end;

{ Text Service }

constructor TTextServiceCocoa.Create(const Owner: IControl; SupportMultiLine: Boolean);
begin
  inherited;
end;

destructor TTextServiceCocoa.Destroy;
begin
  inherited;
end;

function TTextServiceCocoa.GetText: string;
begin
  Result := FText;
end;

procedure TTextServiceCocoa.SetText(const Value: string);
begin
  FText := Value;
end;

function TTextServiceCocoa.GetCaretPostion: TPoint;
begin
  Result := FCaretPosition;
end;

procedure TTextServiceCocoa.SetCaretPosition(const Value: TPoint);
begin
  FCaretPosition := Value;
end;

procedure TTextServiceCocoa.InternalSetMarkedText( const AMarkedText: string );
begin
  FMarkedText := AMarkedText;
  (Owner as ITextInput).IMEStateUpdated;
end;

procedure TTextServiceCocoa.InternalBreakIMEInput;
begin
  FMarkedText := string.Empty;
  (Owner as ITextInput).IMEStateUpdated;
end;

procedure TTextServiceCocoa.InternalEndIMEInput;
begin
  (Owner as ITextInput).EndIMEInput;
  FMarkedText := string.Empty;
end;

procedure TTextServiceCocoa.InternalStartIMEInput;
begin
  (Owner as ITextInput).StartIMEInput;
end;

function TTextServiceCocoa.InternalGetMarkedText: string;
begin
  Result := FMarkedText;
end;

function TTextServiceCocoa.CombinedText: string;
begin
  if FMarkedText <> '' then
    Result := Copy(FText, 1, FCaretPosition.X) + FMarkedText + Copy(FText, FCaretPosition.X + 1, MaxInt)
  else
    Result := FText;
end;

function TTextServiceCocoa.TargetClausePosition: TPoint;
begin
  Result := FCaretPosition;
  Result.X := Result.X + FMarkedText.Length;
end;

procedure TTextServiceCocoa.EnterControl(const FormHandle: TWindowHandle);
begin
end;

procedure TTextServiceCocoa.ExitControl(const FormHandle: TWindowHandle);
var
  Handle: TMacWindowHandle;
  Range: NSRange;
begin
  if not FMarkedText.IsEmpty then
  begin
    Handle := WindowHandleToPlatform(FormHandle);
    Range.location := NSNotFound;
    Range.length := 0;
    TFMXWindow(Handle.Handle).FViewObj.insertText(TNSString.OCClass.stringWithString(StrToNSStr(FMarkedText)), Range);
  end;
end;

procedure TTextServiceCocoa.DrawSingleLine(const Canvas: TCanvas;
      const ARect: TRectF; const FirstVisibleChar: Integer; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center;
      const AWordWrap: Boolean = False);
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
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
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
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X, FMarkedText.Length));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);

      if FSelectedRange.length > 0 then
      begin
        Canvas.StrokeThickness := 3;
        Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X + Integer(FSelectedRange.location), FSelectedRange.length));
        for I := Low(Region) to High(Region) do
          Canvas.DrawLine(
            PointF(Region[I].Left, Region[I].Bottom),
            PointF(Region[I].Right, Region[I].Bottom),
            AOpacity, Canvas.Stroke);
      end;
    finally
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

procedure TTextServiceCocoa.DrawSingleLine(const Canvas: TCanvas;
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

    if not FMarkedText.IsEmpty then
    try
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;

      Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X, FMarkedText.Length));
      for I := Low(Region) to High(Region) do
        Canvas.DrawLine(
          PointF(Region[I].Left, Region[I].Bottom),
          PointF(Region[I].Right, Region[I].Bottom),
          AOpacity, Canvas.Stroke);

      if FSelectedRange.length > 0 then
      begin
        Canvas.StrokeThickness := 3;
        Region := Layout.RegionForRange(TTextRange.Create(CaretPosition.X + Integer(FSelectedRange.location), FSelectedRange.length));
        for I := Low(Region) to High(Region) do
          Canvas.DrawLine(
            PointF(Region[I].Left, Region[I].Bottom),
            PointF(Region[I].Right, Region[I].Bottom),
            AOpacity, Canvas.Stroke);
      end;
    finally
      Canvas.StrokeThickness := 1;
      Canvas.StrokeDash := TStrokeDash.Solid;
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

function TTextServiceCocoa.HasMarkedText: boolean;
begin
  Result := FMarkedText <> '';
end;

function TTextServiceCocoa.GetImeMode: TImeMode;
begin
  Result := FImeMode;
end;

procedure TTextServiceCocoa.SetImeMode(const Value: TImeMode);
begin
  FImeMode := Value;
end;

procedure TTextServiceCocoa.SetMarkedRange(const Value: NSRange);
begin
  FMarkedRange := Value;
end;

procedure TTextServiceCocoa.SetSelectedRange(const Value: NSRange);
begin
  FSelectedRange := Value;
end;

function TPlatformCocoa.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceCocoa;
end;

{ TFMXViewBase }

constructor TFMXViewBase.Create(const AOwner: TFMXWindow);
begin
  inherited Create;
  FOwner := AOwner;
  FBackingStore := TNSMutableAttributedString.Alloc;
  FBackingStore := TNSMutableAttributedString.Wrap(FBackingStore.initWithString(StrToNSStr('')));
  FMarkedRange.location := NSNotFound;
  FMarkedRange.length := 0;
  FSelectedRange.location := 0;
  FSelectedRange.length := 0;
  UpdateTextServiceControl;
end;

destructor TFMXViewBase.Destroy;
var
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FBackingStore.release;
    FOwner := nil;
    NSView(Super).release;
  finally
    AutoreleasePool.release;
  end;
  inherited;
end;

type
  TNSRectArray = array [0..$FFFF] of NSRect;
  PNSRectArray = ^TNSRectArray;

procedure TFMXViewBase.drawRect(dirtyRect: NSRect);
var
  nctx: NSGraphicsContext;
  boundRect: NSRect;
  PaintControl: IPaintControl;
  UpdateRects: array of TRectF;
  R: PNSRectArray;
  I, RCount: NSInteger;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    boundRect := NSView(Super).bounds;

    NativeView.getRectsBeingDrawn(@R, @RCount);
    SetLength(UpdateRects, RCount);
    for I := 0 to RCount - 1 do
    begin
      UpdateRects[I] := RectF(R[I].origin.x, boundRect.size.height - R[I].origin.y - R[I].size.height,
        R[I].origin.x + R[I].size.width, boundRect.size.height - R[I].origin.y);
    end;

    if FOwner <> nil then
    begin
      nctx := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext);

      if Supports(FOwner.Wnd, IPaintControl, PaintControl) then
      begin
        if not (TWindowStyle.GPUSurface in FOwner.Wnd.WindowStyle) then
          PaintControl.ContextHandle := THandle(nctx.graphicsPort);
        PaintControl.PaintRects(UpdateRects);
        if not (TWindowStyle.GPUSurface in FOwner.Wnd.WindowStyle) then
          PaintControl.ContextHandle := 0;
      end;

      if (FOwner.Wnd.Transparency) then
        WindowHandleToPlatform(FOwner.Wnd.Handle).UpdateLayer(nctx.graphicsPort);

      if FOwner.NeedUpdateShadow and NSWindow(FOwner.Super).isVisible then
      begin
        NSWindow(FOwner.Super).invalidateShadow;
        FOwner.NeedUpdateShadow := False;
      end;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXViewBase.GetGestureControlUnderMouse(const APoint: TPointF): IGestureControl;
var
  LObject: IControl;
  LGestureControl: TComponent;
begin
  LObject := FOwner.Wnd.ObjectAtPoint(FOwner.Wnd.ClientToScreen(APoint));
  if LObject <> nil then
    LGestureControl := LObject.GetObject
  else
    LGestureControl := FOwner.Wnd;
  Supports(LGestureControl, IGestureControl, Result);
end;

function TFMXViewBase.GetGestureEngineUnderMouse(const APoint: TPointF): TPlatformGestureEngine;
var
  LControl: IGestureControl;
begin
  Result := nil;
  LControl := GetGestureControlUnderMouse(APoint);
  if LControl <> nil then
    Result := TPlatformGestureEngine(LControl.TouchManager.GestureEngine);
end;

function TFMXViewBase.GetNativeView: NSView;
begin
  Result := NSView(Super);
end;

procedure TFMXViewBase.scrollWheel(event: NSEvent);
var
  H: Boolean;
  SS: TShiftState;
  D: Single;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    try
      SS := ShiftStateFromModFlags(event.modifierFlags);
      H := False;
      D := 0;
      if event.deltaY <> 0 then
        D := event.deltaY
      else if event.deltaX <> 0 then
      begin
        D := event.deltaX;
        SS := SS + [ssHorizontal];
      end;
      if D <> 0 then
        FOwner.Wnd.MouseWheel(SS, Round(D * 30), H)
    except
      HandleException(Self);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TFMXViewBase.keyDown(event: NSEvent);
var
  Performed: Boolean;
begin
  FShift := [];
  Performed := False;
                                                          
  if not hasMarkedText then // IME's conversion window.is active.
  begin
    FOwner.FDelayRelease := True;
    try
      if not PlatformCocoa.FPerformKeyExecuted then
        Performed := PlatformCocoa.KeyProc(Self, FOwner.Wnd, event, False, True);
    finally
      FOwner.FDelayRelease := False;
      if csDestroying in FOwner.Wnd.ComponentState then
        FOwner.Wnd.Release;
    end;
  end;
  if (not Performed) and (not (csDestroying in FOwner.Wnd.ComponentState)) then
    NativeView.inputContext.handleEvent(event);
end;

procedure TFMXViewBase.keyUp(event: NSEvent);
var
  K: word;
  Ch: WideChar;
  Shift: TShiftState;
  VKKeyCode: Integer;
  IsFNKey: boolean;
begin
  if hasMarkedText then
  begin
    // IME's conversion window.is active.
    VKKeyCode := VKeyFromKeyCode(event.keyCode, Shift, IsFNKey);
    TDownKeyList.Current.RemoveKeyDown(VKKeyCode, K, Ch);
  end
  else
    PlatformCocoa.KeyProc(Self, FOwner.Wnd, event, False, False);
end;

procedure TFMXViewBase.mouseMoved(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.mouseDown(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
  SetFirstGesturePoint(theEvent);
end;

procedure TFMXViewBase.mouseUp(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
  ReleaseGestureEngine;
end;

procedure TFMXViewBase.mouseDragged(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
  SetTrackGesturePoint(theEvent);
end;

procedure TFMXViewBase.rightMouseDown(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.rightMouseUp(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.rightMouseDragged(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.otherMouseDown(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.otherMouseUp(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.ReleaseGestureEngine;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
var
  LEventInfo: TGestureEventInfo;
begin
  if FCurrentGestureEngine <> nil then
  begin
    if FCurrentGestureEngine.PointCount > 1 then
    begin
      FillChar(LEventInfo, Sizeof(FEventInfo), 0);
      if TPlatformGestureEngine.IsGesture(FCurrentGestureEngine.Points, FCurrentGestureEngine.GestureList,
        LGestureTypes, LEventInfo) then
        FCurrentGestureEngine.BroadcastGesture(FCurrentGestureEngine.Control, LEventInfo);
    end;
    // reset the points/touches
    FCurrentGestureEngine.ClearPoints;
    FCurrentGestureEngine := nil;
  end;
end;

procedure TFMXViewBase.DiscardGestureEngine;
begin
  if FCurrentGestureEngine <> nil then
  begin
    FCurrentGestureEngine.ClearPoints;
    FCurrentGestureEngine := nil;
  end;
end;

procedure TFMXViewBase.otherMouseDragged(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

function TFMXViewBase.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXViewBase.becomeFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXViewBase.resignFirstResponder: Boolean;
begin
  Result := True;
end;

procedure TFMXViewBase.magnifyWithEvent(event: NSEvent);
var
  Obj: IControl;
  LTouches: NSSet;
  LTouchesArray: NSArray;
  LPoint, LPoint2: NSPoint;
  LTouch: NSTouch;
  LDeviceSize: NSSize;
  I, J, Distance: Integer;
  GestureObj: IGestureControl;
begin
  { Use mouseLocation instead of locationInWindow because gesture events do not have locationInWindow set
   (gestures don't move the mouse cursor). We will consider that the gestures are for the control that is
   under the mouse cursor (on OSX the gestures are for the view under the mouse cursor).}
  LPoint := TNSEvent.OCClass.mouseLocation;
  LPoint.y := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - LPoint.y;
  FEventInfo.Location := PointF(LPoint.x, LPoint.y);

  // Get the control from "under" the gesture.
  Obj := FOwner.Wnd.ObjectAtPoint(FOwner.Wnd.ClientToScreen(FEventInfo.Location));
  if Obj <> nil then
    FGestureControl := Obj.GetObject
  else
    FGestureControl := FOwner.Wnd;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
    FGestureControl := GestureObj.GetFirstControlWithGesture(TInteractiveGesture.Zoom);

  if FGestureControl <> nil then
  begin
    LTouches := event.touchesMatchingPhase(NSTouchPhaseTouching, NSView(Super));
    if LTouches.count >= 2 then
    begin
      LTouchesArray := LTouches.allObjects;
      LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(0));
      LDeviceSize := LTouch.deviceSize;
      FEventInfo.Distance := 0; //reset the distance
      // Find the greatest distance between the touches.
      for I := 0 to LTouches.count - 2 do
      begin
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(I));
        LPoint := LTouch.normalizedPosition;
        for J := 1 to LTouches.count - 1 do
        begin
          LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(J));
          LPoint2 := LTouch.normalizedPosition;

          Distance := Round(Sqrt(Sqr(LPoint.x * LDeviceSize.width - LPoint2.x * LDeviceSize.width) +
            Sqr(LPoint.y * LDeviceSize.height - LPoint2.y * LDeviceSize.height)));
          if Distance > FEventInfo.Distance then
            FEventInfo.Distance := Distance;
        end;

        FEventInfo.GestureID := igiZoom;
        if Supports(FGestureControl, IGestureControl, GestureObj) then
          GestureObj.CMGesture(FEventInfo);
        FEventInfo.Flags := [];
      end
    end
  end
  else
    //send the message up the responder chain
    NSView(Super).magnifyWithEvent(event);
end;

procedure TFMXViewBase.rotateWithEvent(event: NSEvent);
var
  Obj: IControl;
  LPoint: NSPoint;
  GestureObj: IGestureControl;
begin
  // Use mouseLocation instead of locationInWindow because gesture events do not have locationInWindow set
  // (gestures don't move the mouse cursor). We will consider that the gestures are for the control that is
  // under the mouse cursor (on OSX the gestures are for the view under the mouse cursor).
  LPoint := TNSEvent.OCClass.mouseLocation;
  LPoint.y := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - LPoint.y;
  FEventInfo.Location := TPointF.Create(LPoint.x, LPoint.y);

  // Get the control from "under" the gesture.
  Obj := FOwner.Wnd.ObjectAtPoint(FOwner.Wnd.ClientToScreen(FEventInfo.location));
  if Obj <> nil then
    FGestureControl := Obj.GetObject
  else
    FGestureControl := FOwner.Wnd;
  if Supports(FGestureControl, IGestureControl, GestureObj) then
    FGestureControl := GestureObj.GetFirstControlWithGesture(TInteractiveGesture.Rotate);

  if FGestureControl <> nil then
  begin
    //Transform degrees in radians and add them to the existing angle of rotation.
    FEventInfo.Angle := FEventInfo.Angle + event.rotation* Pi / 180;
    FEventInfo.GestureID := igiRotate;
    if Supports(FGestureControl, IGestureControl, GestureObj) then
      GestureObj.CMGesture(FEventInfo);
    FEventInfo.Flags := [];
  end
  else
    //send the message up the responder chain
    NSView(Super).rotateWithEvent(event);
end;

procedure TFMXViewBase.swipeWithEvent(event: NSEvent);
var
  Obj: IControl;
  LTouches: NSSet;
  LTouchesArray: NSArray;
  LPoint, LPoint2: NSPoint;
  LTouch: NSTouch;
  Width, Height: Single;
  GestureObj: IGestureControl;
begin
  // Use mouseLocation instead of locationInWindow because gesture events do not have locationInWindow set
  // (gestures don't move the mouse cursor). We will consider that the gestures are for the control that is
  // under the mouse cursor (on OSX the gestures are for the view under the mouse cursor).
  LPoint := TNSEvent.OCClass.mouseLocation;
  LPoint.y := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - LPoint.y;

  FSwipePoint := TPointF.Create(LPoint.x, LPoint.y);

  // Get the control from "under" the gesture.
  Obj := FOwner.Wnd.ObjectAtPoint(FOwner.Wnd.ClientToScreen(FSwipePoint));
  if Obj <> nil then
    FGestureControl := Obj.GetObject
  else
    FGestureControl := FOwner.Wnd;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
    FGestureControl := GestureObj.GetFirstControlWithGesture(TInteractiveGesture.Pan);

  if FGestureControl <> nil then
  begin
    FEventInfo.Location := FSwipePoint;
    LTouches := event.touchesMatchingPhase(NSTouchPhaseTouching, NSView(Super));
    if LTouches.count = 2 then
    begin
      LTouchesArray := LTouches.allObjects;
      LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(0));
      LPoint := LTouch.normalizedPosition;
      LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(1));
      LPoint2 := LTouch.normalizedPosition;

      //the distance between the 2 fingers
      FEventInfo.Distance := Round(Sqrt(Sqr(LPoint.x - LPoint2.x) + Sqr(LPoint.y - LPoint2.y)));

      LPoint.X := Min(LPoint.x, LPoint2.x) + Abs(LPoint.x - LPoint2.x);
      LPoint.Y := 1.0 - Min(LPoint.y, LPoint2.y) + Abs(LPoint.y - LPoint2.y);

      Width := 0;
      Height := 0;
      if FGestureControl is TCommonCustomForm then
      begin
        Width := TCommonCustomForm(FGestureControl).ClientWidth;
        Height := TCommonCustomForm(FGestureControl).ClientHeight;
      end
      else if FGestureControl is TControl then
      begin
        Width := TControl(FGestureControl).Canvas.Width;
        Height := TControl(FGestureControl).Canvas.Height;
      end;

      LPoint.x := LPoint.x * Width;
      LPoint.y := LPoint.y * Height;

      if event.deltaX <> 0 then
      begin
        //horizontal swipe
        if event.deltaX > 0 then
        begin
          //swipe-left
          LPoint.x := LPoint.x + FEventInfo.Location.X;
          FEventInfo.Location := TPointF.Create(LPoint.x, LPoint.y);
        end
        else
        begin
          // swipe-right
          LPoint.X := FEventInfo.Location.x - LPoint.x;
          FEventInfo.Location := TPointF.Create(LPoint.x, LPoint.y);
        end;
      end
      else if event.deltaY <> 0 then
      begin
        //vertical swipe
        if event.deltaY > 0 then
        begin
          //swipe-up
          LPoint.y := LPoint.y + FEventInfo.Location.Y;
          FEventInfo.Location := PointF(LPoint.x, LPoint.y);
        end
        else
        begin
          //swipe-down
          LPoint.y := FEventInfo.Location.Y - LPoint.Y;
          FEventInfo.Location := TPointF.Create(LPoint.x, LPoint.y);
        end;
      end;

      FSwipePoint := FEventInfo.Location;
    end
    else
    begin
      // send the message up the responder chain
      NSView(Super).swipeWithEvent(event);
      Exit;
    end;

    // EventInfo.Distance := Sqrt(Sqr(Point.X - Source.X) + Sqr(Point.Y - Source.Y));;
    // EventInfo.InertiaVector := TPointF(SmallPointToPoint(InertiaVectorFromArgument(LGestureInfo.ullArguments)));
    FEventInfo.GestureID := igiPan;

    // send message to the control
    if Supports(FGestureControl, IGestureControl, GestureObj) then
      GestureObj.CMGesture(FEventInfo);
    FEventInfo.Flags := [];
  end
  else
    // send the message up the responder chain
    NSView(Super).swipeWithEvent(event);
end;

procedure TFMXViewBase.touchesBeganWithEvent(event: NSEvent);
var
  Obj: IControl;
  LTouches: NSSet;
  LTouchesArray: NSArray;
  LPoint, LPoint2: NSPoint;
  LLocation: TPointF;
  LTouch: NSTouch;
  Handled: Boolean;
  GestureObj: IGestureControl;
begin
  Handled := False;
  FillChar(FEventInfo, Sizeof(FEventInfo), 0);
  // Get the location of the gesture.
  // Use mouseLocation instead of locationInWindow because gesture events do not have locationInWindow set
  // (gestures don't move the mouse cursor). We will consider that the gestures are for the control that is
  // under the mouse cursor (on OSX the gestures are for the view under the mouse cursor).
  LPoint := TNSEvent.OCClass.mouseLocation;
  LPoint.y := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - LPoint.y;
  LLocation := TPointF.Create(LPoint.x, LPoint.y);

  DiscardGestureEngine;

  // Find the control from "under" the gesture.
  Obj := FOwner.Wnd.ObjectAtPoint(LLocation);
  if Obj <> nil then
    FGestureControl := Obj.GetObject
  else
    FGestureControl := FOwner.Wnd;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
    FGestureControl := GestureObj.GetFirstControlWithGestureEngine;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
  begin
    TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).InitialPoint := LLocation;

    // Use NSTouch.normalizedPosition to get the movement of the fingers on the trackpad.
    LTouches := event.touchesMatchingPhase(NSTouchPhaseTouching, NSView(Super));
    if LTouches.count = 2 then
    begin
      LTouchesArray := LTouches.allObjects;
      if FGestureControl <> nil then
      begin
        Handled := True;
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(0));
        LPoint := LTouch.normalizedPosition;
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(1));
        LPoint2 := LTouch.normalizedPosition;

        LPoint.x := Min(LPoint.x, LPoint2.x) + Abs(LPoint.x - LPoint2.x);
        LPoint.y := 1.0 - Min(LPoint.y, LPoint2.y) - Abs(LPoint.y - LPoint2.y);

        // Retain the points/touches.
        TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).ClearPoints;
        TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).AddPoint(LPoint.x * 100, LPoint.y * 100);
      end;
    end;
  end;

  //set the gfBegin flag for interactive gestures.
  FEventInfo.Flags := [TInteractiveGestureFlag.gfBegin];

  if not Handled then
    //send the message up the responder chain
    NSView(Super).touchesBeganWithEvent(event);
end;

procedure TFMXViewBase.touchesCancelledWithEvent(event: NSEvent);
var
  GestureObj: IGestureControl;
begin
  if Supports(FGestureControl, IGestureControl, GestureObj) then
  begin
    //Handle "end" flag for interactive gestures.
    if FEventInfo.GestureID > igiFirst then
    begin
      FEventInfo.Flags := [TInteractiveGestureFlag.gfEnd];
      // send message to the control
      GestureObj.CMGesture(FEventInfo);
      FillChar(FEventInfo, Sizeof(FEventInfo), 0);
    end;

    //reset the points/touches
    if GestureObj.TouchManager.GestureEngine <> nil then
      TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).ClearPoints;
    FGestureControl := nil;
  end
  else
    //send the message up the responder chain
    NSView(Super).touchesCancelledWithEvent(event);
end;

const
  MIN_NO_GESTURE_POINTS = 10;

procedure TFMXViewBase.touchesEndedWithEvent(event: NSEvent);
var
  LEngine: TPlatformGestureEngine;
  Handled: Boolean;
  GestureObj: IGestureControl;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
begin
  Handled := False;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
  begin
    // Handle "end" flag for interactive gestures.
    if FEventInfo.GestureID > igiFirst then
    begin
      FEventInfo.Flags := [TInteractiveGestureFlag.gfEnd];
      // send message to the control
      GestureObj.CMGesture(FEventInfo);
    end;

    if GestureObj.TouchManager.GestureEngine <> nil then
    begin
      // Handle standard gestures.
      LEngine := TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine);
      if LEngine.PointCount > 1 then
      begin
        // Make sure there are at least MIN_NO_GESTURE_POINTS points.
        if LEngine.PointCount < MIN_NO_GESTURE_POINTS then
          LEngine.AddPoints(MIN_NO_GESTURE_POINTS - LEngine.PointCount);
        FillChar(FEventInfo, Sizeof(FEventInfo), 0);
        if TPlatformGestureEngine.IsGesture(LEngine.Points, LEngine.GestureList, LGestureTypes, FEventInfo) then
        begin
          LEngine.BroadcastGesture(FGestureControl, FEventInfo);
          Handled := True;
        end;
      end;
      // reset the points/touches
      TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).ClearPoints;
    end;
    FGestureControl := nil;
    FillChar(FEventInfo, Sizeof(FEventInfo), 0);
  end;

  if not Handled then
    NSView(Super).touchesEndedWithEvent(event);
end;

procedure TFMXViewBase.touchesMovedWithEvent(event: NSEvent);
var
  LTouches: NSSet;
  LTouchesArray: NSArray;
  LPoint, LPoint2: NSPoint;
  LTouch: NSTouch;
  Handled: Boolean;
  GestureObj: IGestureControl;
begin
  Handled := False;

  if Supports(FGestureControl, IGestureControl, GestureObj) and (GestureObj.TouchManager.GestureEngine <> nil) then
  begin
    // retain the points/touches
    LTouches := event.touchesMatchingPhase(NSTouchPhaseTouching, NSView(Super));
    if LTouches.count = 2 then
    begin
      LTouchesArray := LTouches.allObjects;
      // Retain the points/touches.
      if FGestureControl <> nil then
      begin
        Handled := True;
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(0));
        LPoint := LTouch.normalizedPosition;
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(1));
        LPoint2 := LTouch.normalizedPosition;

        LPoint.x := Min(LPoint.x, LPoint2.x) + Abs(LPoint.x - LPoint2.x);
        LPoint.y := 1.0 - Min(LPoint.y, LPoint2.y) - Abs(LPoint.y - LPoint2.y);
        // Retain the points/touches.
        TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).AddPoint(LPoint.x * 100, LPoint.y * 100);
      end;
    end;
  end;

  if not Handled then
    NSView(Super).touchesMovedWithEvent(event);
end;


{ NSTextInputClient }

function TFMXViewBase.firstRectForCharacterRange(aRange: NSRange;
  actualRange: PNSRange): NSRect;
var
  glyphRect: NSRect;
  R: TRectF;
  TSObj: ITextInput;
begin
  if (FOwner.Wnd.Focused <> nil) and Supports(FOwner.Wnd.Focused, ITextInput, TSObj) then
    R := TRectF.Create(TSObj.GetTargetClausePointF)
  else if FOwner.Wnd.Focused <> nil then
    R := TControl(FOwner.Wnd.Focused.GetObject).AbsoluteRect
  else
    R := TRectF.Create(0, 0, 0, 0);

  glyphRect := MakeNSRect(R.Left, NativeView.bounds.size.height-R.Bottom, R.Right - R.Left, R.Bottom - R.Top);
//   Convert the rect to screen coordinates
  glyphRect := NativeView.convertRectToBase(glyphRect);
  glyphRect.origin := NativeView.window.convertBaseToScreen(glyphRect.origin);
  Result := glyphRect;
end;

function TFMXViewBase.hasMarkedText: Boolean;
begin
  Result := FMarkedRange.location <> NSNotFound;
end;

function ToNSString(const text : Pointer; var NStr: NSString): Boolean;
begin
  if TNSObject.Wrap(text).isKindOfClass(objc_getClass(MarshaledAString('NSAttributedString'))) then
  begin
    NStr := TNSString.Wrap(objc_msgSend(text, sel_getUid(MarshaledAString('string'))));
    Result := True;
  end
  else
  begin
    NStr := TNSString.Wrap(text);
    Result := False;
  end;
end;

procedure TFMXViewBase.insertText(text: Pointer{NSString}; replacementRange: NSRange);
var
  I: Integer;
  K: Word;
  R : NSRange;
  Ch: WideChar;
  Str: string;
  NStr: NSString;
  AutoReleasePool: NSAutoreleasePool;
  TSC: ITextInput;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if (FShift - [ssAlt, ssShift]) <> [] then
      Exit;
    if hasMarkedText then
    begin
      NativeView.inputContext.discardMarkedText;
      try
        if (FOwner.Wnd.Focused <> nil) and Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
        begin
          TTextServiceCocoa(TSC.GetTextService).InternalSetMarkedText('');
          TTextServiceCocoa(TSC.GetTextService).InternalEndIMEInput;
        end;
      except
        HandleException(Self);
      end;
    end;
    NativeView.inputContext.invalidateCharacterCoordinates;
    ToNSString(text, NStr);
    if NStr.length > 0 then
    begin
      Str := UTF8ToString(NStr.UTF8String);
      for I := 0 to Str.Length - 1 do
      begin
        Ch := Str.Chars[I];
        K := Ord(Ch);
        DoKeyDown(Self, FOwner.Wnd, K, Ch, FShift);
      end;

      // Get a valid range
      if replacementRange.location = NSNotFound then
        if FMarkedRange.location <> NSNotFound then
          replacementRange := FMarkedRange
        else
          replacementRange := FSelectedRange
      else
      begin
        replacementRange.location := 0;
        replacementRange.length := 0;
      end;

      NativeView.inputContext.invalidateCharacterCoordinates;
      try
        if (FOwner.Wnd.Focused <> nil) then
          FOwner.Wnd.Focused.Repaint;
      except
        HandleException(Self);
      end;
    end;
    FBackingStore.beginEditing;
    R.location := 0;
    R.length := FBackingStore.mutableString.length;
    FBackingStore.deleteCharactersInRange(R);
    FBackingStore.endEditing;

    FMarkedRange.location := NSNotFound;
    FMarkedRange.length := 0;
    FSelectedRange.location := 0;
    FSelectedRange.length := 0;
    UpdateTextServiceControl;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXViewBase.selectedRange: NSRange;
begin
  Result := FSelectedRange;
end;

procedure TFMXViewBase.SetFirstGesturePoint(const AnEvent: NSEvent);
var
  LPoint: NSPoint;
  LLocation: TPointF;
  LWindow: NSWindow;
begin
  LWindow := AnEvent.window;
  if LWindow <> nil then
  begin
    LPoint := AnEvent.locationInWindow;
    LLocation := TPointF.Create(LPoint.X, NSView(Super).bounds.size.height - LPoint.y);
    FCurrentGestureEngine := GetGestureEngineUnderMouse(LLocation);
    if FCurrentGestureEngine <> nil then
    begin
      FCurrentGestureEngine.ClearPoints;
      FCurrentGestureEngine.InitialPoint := LLocation;
    end;
  end;
end;

procedure TFMXViewBase.setMarkedText(text: Pointer {NSString}; selectedRange,
  replacementRange: NSRange);
var
  NStr: NSString;
  IsAttrString: Boolean;
  TSC: ITextInput;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if not hasMarkedText then
    try
      if (FOwner.Wnd.Focused <> Nil) and Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
        TTextServiceCocoa(TSC.GetTextService).InternalStartIMEInput;
      TDownKeyList.Current.Clear;
    except
      HandleException(Self);
    end;

    IsAttrString := ToNSString(text, NStr);

    NativeView.inputContext.invalidateCharacterCoordinates;
    // Get a valid range
    if replacementRange.location = NSNotFound then
      if FMarkedRange.location <> NSNotFound then
        replacementRange := FMarkedRange
      else
        replacementRange := FSelectedRange
    else
    begin
      replacementRange.location := 0;
      replacementRange.length := 0;
    end;

    // Add the text
    FBackingStore.beginEditing;
    try
      if NStr.length = 0 then
      begin
        FBackingStore.deleteCharactersInRange(replacementRange);
        NativeView.inputContext.discardMarkedText;
        FMarkedRange.location := NSNotFound;
        FMarkedRange.length := 0;
        FSelectedRange.location := 0;
        FSelectedRange.length := 0;
        UpdateTextServiceControl;
        try
          if (FOwner.Wnd.Focused <> nil) and Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
          begin
            TTextServiceCocoa(TSC.GetTextService).InternalBreakIMEInput;
            TTextServiceCocoa(TSC.GetTextService).InternalEndIMEInput;
          end;
        except
          HandleException(Self);
        end;
      end
      else
      begin
        FSelectedRange.location := replacementRange.location + selectedRange.location;
        FSelectedRange.length := selectedRange.length;
        FMarkedRange.location := replacementRange.location;
        FMarkedRange.length := NStr.length;
        UpdateTextServiceControl;
        if IsAttrString then
          FBackingStore.replaceCharactersInRange(replacementRange, TNSAttributedString.Wrap(text))
        else
          FBackingStore.replaceCharactersInRange(replacementRange, TNSString.Wrap(text));
        try
          if (FOwner.Wnd.Focused <> nil) and Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
            TSC.GetTextService.InternalSetMarkedText(UTF8ToString(NStr.UTF8String));
        except
          HandleException(Self);
        end;
      end;
    finally
      FBackingStore.endEditing;
    end;

    NativeView.inputContext.invalidateCharacterCoordinates;
    try
      if (FOwner.Wnd.Focused <> nil) then
        FOwner.Wnd.Focused.Repaint;
    except
      HandleException(Self);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TFMXViewBase.SetTrackGesturePoint(const AnEvent: NSEvent);
var
  LPoint: NSPoint;
  LLocation: TPointF;
  LWindow: NSWindow;
  LEngine: TPlatformGestureEngine;
begin
  if FCurrentGestureEngine <> nil then
  begin
    LWindow := AnEvent.window;
    if LWindow <> nil then
    begin
      LPoint := AnEvent.locationInWindow;
      LLocation := TPointF.Create(LPoint.X, NSView(Super).bounds.size.height - LPoint.y);
      LEngine := GetGestureEngineUnderMouse(LLocation);
      if FCurrentGestureEngine <> LEngine then
        ReleaseGestureEngine
      else
        FCurrentGestureEngine.AddPoint(LLocation.X, LLocation.Y);
    end;
  end;
end;

procedure TFMXViewBase.unMarkText;
var
  TSC: ITextInput;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
  //  NativeView.inputContext.invalidateCharacterCoordinates;
    FMarkedRange.location := NSNotFound;
    FMarkedRange.length := 0;
    UpdateTextServiceControl;

    try
      if (FOwner.Wnd.Focused <> Nil) and Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
      begin
        TTextServiceCocoa(TSC.GetTextService).InternalSetMarkedText('');
      end;
    except
      HandleException(Self);
    end;

    NativeView.inputContext.discardMarkedText;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXViewBase.validAttributesForMarkedText: Pointer {NSArray};
var
  Attribs: array[0..1] of Pointer;
  Attrib: NSString;
  AttrArray: NSArray;
begin
  Attrib := NSMarkedClauseSegmentAttributeName;
  Attribs[0] := (Attrib as ILocalObject).GetObjectID;
  Attrib := NSGlyphInfoAttributeName;
  Attribs[1] := (Attrib as ILocalObject).GetObjectID;
  AttrArray := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@Attribs[0], 2));
  Result := (AttrArray as ILocalObject).GetObjectID;

//  Attrib := NSMarkedClauseSegmentAttributeName;
//  Attribs[0] := (Attrib as ILocalObject).GetObjectID;
//  AttrArray := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@Attribs[0], 1));
//  Result := (AttrArray as ILocalObject).GetObjectID;
end;

procedure TFMXViewBase.doCommandBySelector(selector: SEL);
begin
  NativeView.doCommandBySelector(selector);
end;

function TFMXViewBase.drawsVerticallyForCharacterAtIndex(
  charIndex: NSUInteger): Boolean;
begin
  Result := False;
end;

function TFMXViewBase.fractionOfDistanceThroughGlyphForPoint(
  aPoint: NSPoint): CGFloat;
begin
  Result := 0;
end;

function TFMXViewBase.windowLevel: NSInteger;
begin
  Result := NativeView.window.level;
end;

function TFMXViewBase.FocusedTextService: TTextServiceCocoa;
var
  TSC : ITextInput;
begin
  Result := nil;
  if Owner <> nil then
    if Owner.Wnd <> nil then
      if Owner.Wnd.Focused <> nil then
        if Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
          Result := TTextServiceCocoa(TSC.GetTextService);
end;

procedure TFMXViewBase.UpdateTextServiceControl;
var
  TSC: ITextInput;
begin
  if (FOwner.Wnd.Focused <> Nil) and Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
  begin
    TTextServiceCocoa( TSC.GetTextService ).SetMarkedRange(FMarkedRange);
    TTextServiceCocoa( TSC.GetTextService ).SetSelectedRange(FSelectedRange);
  end;
end;

function TFMXViewBase.attributedString: NSAttributedString;
begin
  Result := FBackingStore;
end;

function TFMXViewBase.attributedSubstringForProposedRange(aRange: NSRange;
  actualRange: PNSRange): NSAttributedString;
begin
  // Get a valid range
  if actualRange <> nil then
  begin
    if (aRange.location <> NSNotFound) and (aRange.location < (FBackingStore.length - 1)) then
      actualRange^.location := aRange.location
    else
      actualRange^.location := 0;
    if (aRange.length) <= (FBackingStore.length - actualRange^.location) then
      actualRange^.length := aRange.length
    else
      actualRange^.length := FBackingStore.length - actualRange^.location - 1;

    // Get the backing store matching the range
    if (actualRange^.location = 0) and (actualRange^.length = FBackingStore.length) then
    begin
      Result := FBackingStore;
    end
    else
    begin
      Result := TNSAttributedString.Wrap(FBackingStore.attributedSubstringFromRange(actualRange^));
    end;
  end
  else
    Result := nil;
end;

function TFMXViewBase.baselineDeltaForCharacterAtIndex(
  anIndex: NSUInteger): CGFloat;
begin
  Result := 0;
end;

function TFMXViewBase.characterIndexForPoint(aPoint: NSPoint): NSUInteger;
begin
  Result := 0;
end;

function TFMXViewBase.markedRange: NSRange;
begin
  Result := FMarkedRange;
end;

{ TFMXView3D }

constructor TFMXView3D.Create(const AOwner: TFMXWindow; AFrameRect: NSRect);
var
  V: Pointer;
begin
  inherited Create(AOwner);
  V := NSOpenGLView(Super).initWithFrame(AFrameRect, TNSOpenGLView.OCClass.defaultPixelFormat);
  if NSAppKitVersionNumber >= NSAppKitVersionNumber10_7 then
    NSOpenGLView(Super).setWantsBestResolutionOpenGLSurface(True);
  if GetObjectID <> V then
    UpdateObjectID(V);
  NSView(Super).setAcceptsTouchEvents(True);
  FillChar(FEventInfo, Sizeof(FEventInfo), 0);
end;

destructor TFMXView3D.Destroy;
begin
  NSOpenGLView(Super).clearGLContext;
  inherited;
end;

function TFMXView3D.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXView3D);
end;

{ TFMXWindow}

function TFMXWindow.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXWindow);
end;

function TFMXWindow.GetView: NSView;
begin
  Result := FViewObj.NativeView;
end;

function TFMXWindow.windowShouldClose(Sender: Pointer): Boolean;
var
  Action: TCloseAction;
begin
  Result := False;
  if Application = nil then
    Exit;
  if Application.Terminated then
    Exit;
  try
    Result := Wnd.CloseQuery;
    if Result and Assigned(Wnd.OnClose) then
    begin
      Action := TCloseAction.caHide;
      Wnd.OnClose(Wnd, Action);
      if Action = TCloseAction.caMinimize then
      begin
        Result := False;
        Wnd.WindowState := TWindowState.wsMinimized;
      end
      else
        if (Application <> nil) and (Wnd = Application.MainForm) then
          Result := (Action in [TCloseAction.caHide, TCloseAction.caFree])
        else
        begin
          Result := (Action = TCloseAction.caHide);
          if Action = TCloseAction.caFree then
            Wnd.Release;
        end;
    end;
  except
    HandleException(Self);
  end;
end;

procedure TFMXWindow.windowWillClose(notification: NSNotification);
var
  LParent: NSWindow;
begin
  if (Application <> nil) and (Application.MainForm <> nil) then
  begin
    LParent := WindowHandleToPlatform(Wnd.Handle).Wnd;
    while (LParent <> nil) and (LParent <> WindowHandleToPlatform(Application.MainForm.Handle).Wnd) do
      LParent := LParent.ParentWindow;
    if LParent <> nil then
      Application.Terminate;
  end;
end;

procedure TFMXWindow.windowDidBecomeKey(notification: NSNotification);
begin
  if Wnd.FormStyle <> TFormStyle.Popup then
  begin
    if (PlatformCocoa <> nil) and (not PlatformCocoa.FDisableClosePopups) then
    begin
      TPlatformCocoa.PrepareClosePopups(Wnd);
      TPlatformCocoa.ClosePopupForms;
    end;
    try
      if PlatformCocoa.FAlertCount = 0 then
        Wnd.Activate;
    except
      HandleException(Self);
    end;
  end;
end;

procedure TFMXWindow.windowDidChangeBackingProperties(notification: NSNotification);
begin
  if (Application = nil) or (Application.Terminated) then
    Exit;
  try
    TMessageManager.DefaultManager.SendMessage(nil, TScaleChangedMessage.Create(Wnd), True);
  except
    HandleException(Self);
  end;
end;

procedure TFMXWindow.windowDidResignKey(notification: NSNotification);
begin
  if (Wnd = nil) or (Application = nil) or (Application.Terminated) then
    Exit;
  try
    Wnd.Deactivate;
  except
    HandleException(Self);
  end;
end;

procedure TFMXWindow.UpdateWindowState;
var
  NSWin: NSWindow;
begin
  if (Wnd <> nil) and PlatformCocoa.FCanSetState then
  begin
    PlatformCocoa.FCanSetState := False;
    try
      NSWin := WindowHandleToPlatform(Wnd.Handle).Wnd;
      if NSWin.isMiniaturized then
        Wnd.WindowState := TWindowState.wsMinimized
      else if IsZoomed(NSWin) then
        Wnd.WindowState := TWindowState.wsMaximized
      else
        Wnd.WindowState := TWindowState.wsNormal;
    finally
      PlatformCocoa.FCanSetState := True;
    end;
  end;
end;

procedure TFMXWindow.windowDidResize(notification: NSNotification);
var
  LFrame: NSRect;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    UpdateWindowState;
    LFrame := NSWindow(Super).frame;
    try
      Wnd.SetBounds(round(LFrame.origin.x),
        round(TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - LFrame.origin.y - LFrame.size.height),
        round(LFrame.size.width), round(LFrame.size.height));
    except
      HandleException(Self);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TFMXWindow.windowDidMiniaturize(notification: NSNotification);
begin
  UpdateWindowState;
end;

procedure TFMXWindow.windowDidDeminiaturize(notification: NSNotification);
begin
  UpdateWindowState;
end;

procedure TFMXWindow.windowDidEnterFullScreen(notification: NSNotification);
begin
  UpdateWindowState;
end;

procedure TFMXWindow.windowDidExitFullScreen(notification: NSNotification);
var
  NSWin: NSWindow;
begin
  if (Wnd <> nil) and PlatformCocoa.FCanSetState and (not Wnd.Visible) then
  begin
    PlatformCocoa.FCanSetState := False;
    try
      NSWin := WindowHandleToPlatform(Wnd.Handle).Wnd;
      PlatformCocoa.SetShowFullScreenIcon(Wnd, Wnd.ShowFullScreenIcon);
      NSWin.orderOut(nil);
    finally
      PlatformCocoa.FCanSetState := True;
    end;
  end;
  UpdateWindowState;
end;

procedure TFMXWindow.windowDidMove(notification: NSNotification);
var
  LFrame: NSRect;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    LFrame := NSWindow(Super).frame;
    try
      Wnd.SetBounds(round(LFrame.origin.x),
        round(TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - LFrame.origin.y - LFrame.size.height),
        round(LFrame.size.width), round(LFrame.size.height));
    except
      HandleException(Self);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

var
  GlobalData: TDragObject;

function GetDataObject(sender: NSDraggingInfo): TDragObject;
var
  PBoard: NSPasteboard;
  Str: NSString;
  Arr: NSArray;
  W: string;
  I: Integer;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FillChar(Result, SizeOf(Result), 0);

    PBoard := sender.draggingPasteboard;
    if PBoard.types.containsObject((NSFMXPboardType as ILocalObject).GetObjectID) then
    begin
      Result := GlobalData;
      Exit;
    end;

    if PBoard.types.containsObject((NSPasteboardTypeString as ILocalObject).GetObjectID) then
    begin
      Str := PBoard.stringForType(NSPasteboardTypeString);
      W := UTF8ToString(str.UTF8String);
      Result.Data := W;
    end;

    if PBoard.types.containsObject((NSFilenamesPboardType as ILocalObject).GetObjectID) then
    begin
      Arr := TNSArray.Wrap(PBoard.propertyListForType(NSFilenamesPboardType));
      SetLength(Result.Files, Arr.count);
      for I := 0 to Arr.count - 1 do
      begin
        Str := TNSString.Wrap(Arr.objectAtIndex(I));
        W := UTF8ToString(Str.UTF8String);
        Result.Files[I] := W;
      end;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXWindow.draggingEntered(Sender: Pointer): NSDragOperation;
var
  mp: NSPoint;
  P: TPointF;
  DragInfo: NSDraggingInfo;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    DragInfo := TNSDraggingInfo.Wrap(Sender);
    mp := DragInfo.draggingLocation;
    mp.y := View.bounds.size.height - mp.y;
    P := PointF(mp.x, mp.y);
    try
      Wnd.DragEnter(GetDataObject(DragInfo), Wnd.ClientToScreen(P));
    except
      HandleException(Self);
    end;
    Result := NSDragOperationEvery;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TFMXWindow.draggingExited(Sender: Pointer {id});
begin
  try
    Wnd.DragLeave;
  except
    HandleException(Self);
  end;
end;

function TFMXWindow.draggingUpdated(Sender: Pointer): NSDragOperation;
var
  mp: NSPoint;
  P: TPointF;
  Operation: TDragOperation;
  DragInfo: NSDraggingInfo;
  AutoReleasePool: NSAutoreleasePool;
begin
  Result := NSDragOperationNone;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    DragInfo := TNSDraggingInfo.Wrap(Sender);
    mp := DragInfo.draggingLocation;
    mp.y := View.bounds.size.height - mp.y;
    P := PointF(mp.x, mp.y);
    Operation := TDragOperation.None;
    try
      Wnd.DragOver(GetDataObject(DragInfo), Wnd.ClientToScreen(P), Operation);
    except
      HandleException(Self);
    end;
    case Operation of
      TDragOperation.None:
        Result := NSDragOperationNone;
      TDragOperation.Move:
        Result := NSDragOperationMove;
      TDragOperation.Copy:
        Result := NSDragOperationCopy;
      TDragOperation.Link:
        Result := NSDragOperationLink;
    end;
    DragOperation := Result;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXWindow.performDragOperation(Sender: Pointer): Boolean;
var
  mp: NSPoint;
  P: TPointF;
  DragInfo: NSDraggingInfo;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    DragInfo := TNSDraggingInfo.Wrap(Sender);
    mp := DragInfo.draggingLocation;
    mp.y := View.bounds.size.height - mp.y;
    P := PointF(mp.x, mp.y);
    try
      Wnd.DragDrop(GetDataObject(DragInfo), Wnd.ClientToScreen(P));
    except
      HandleException(Self);
    end;
    Result := True;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXWindow.performKeyEquivalent(event: NSEvent): Boolean;
var
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Result := False;
    if (NSWindow(Super).firstResponder as ILocalObject).GetObjectID = (GetView as ILocalObject).GetObjectID then
    begin
      //IME's conversion window.is active.
      if (FViewObj <> nil) and FViewObj.hasMarkedText then
        exit;
      if not PlatformCocoa.FPerformKeyExecuted then
        Result := PlatformCocoa.KeyProc(Self, Wnd, event, True, True);
      PlatformCocoa.FPerformKeyExecuted := True;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXWindow.CanActivate: Boolean;
begin
  Result := (Wnd <> nil) and (Wnd.Handle <> nil) and (Wnd.FormStyle <> TFormStyle.Popup) and
    ((PlatformCocoa.FModalStack = nil) or (PlatformCocoa.FModalStack.Count = 0) or
     (PlatformCocoa.FModalStack.Peek = Wnd));
end;

function TFMXWindow.acceptsFirstResponder: Boolean;
begin
  Result := CanActivate;
end;

function TFMXWindow.becomeFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXWindow.resignFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXWindow.canBecomeKeyWindow: Boolean;
begin
  Result := CanActivate;
end;

function TFMXWindow.canBecomeMainWindow: Boolean;
begin
  Result := CanActivate;
end;

destructor TFMXWindow.Destroy;
begin
  if FViewObj <> nil then
  begin
    FViewObj.NativeView.setHidden(True);
    NSWindow(Super).setContentView(nil);
    // A reference for the paint context was manually added when the window was
    // created. Clear the reference here to avoid leaking view objects.
    FViewObj._Release;
    FViewObj := nil;
  end;
  if FDelegate <> nil then
  begin
    FDelegate := nil;
    NSWindow(Super).setDelegate(nil);
  end;
  if Assigned(Wnd.Handle) then
    NSWindow(Super).close;
  Wnd := nil;
  NSWindow(Super).release;
  inherited;
end;

{ TFMXWindowDelegate }

type
  TFMXWindowDelegate = class(TOCLocal, NSWindowDelegate)
  private
    FWindow: TFMXWindow;
  public
    constructor Create(AOwner: TFMXWindow);
    destructor Destroy; override;
    function windowShouldClose(Sender: Pointer {id}): Boolean; cdecl;
    procedure windowWillClose(notification: NSNotification); cdecl;
    procedure windowDidBecomeKey(notification: NSNotification); cdecl;
    procedure windowDidResignKey(notification: NSNotification); cdecl;
    procedure windowDidResize(notification: NSNotification); cdecl;
    procedure windowDidMiniaturize(notification: NSNotification); cdecl;
    procedure windowDidDeminiaturize(notification: NSNotification); cdecl;
    procedure windowDidEnterFullScreen(notification: NSNotification); cdecl;
    procedure windowDidExitFullScreen(notification: NSNotification); cdecl;
    procedure windowDidMove(notification: NSNotification); cdecl;
    procedure windowDidChangeBackingProperties(notification: NSNotification); cdecl;
  end;

constructor TFMXWindowDelegate.Create(AOwner: TFMXWindow);
begin
  inherited Create;
  FWindow := AOwner;
end;

destructor TFMXWindowDelegate.Destroy;
begin
  FWindow := nil;
  objc_msgSend(GetObjectID, sel_getUid('release'));
  inherited;
end;

procedure TFMXWindowDelegate.windowDidBecomeKey(notification: NSNotification);
begin
  FWindow.windowDidBecomeKey(notification);
end;

procedure TFMXWindowDelegate.windowDidChangeBackingProperties(notification: NSNotification);
begin
  FWindow.windowDidChangeBackingProperties(notification);
end;

procedure TFMXWindowDelegate.windowDidMove(notification: NSNotification);
begin
  FWindow.windowDidMove(notification);
end;

procedure TFMXWindowDelegate.windowDidResignKey(notification: NSNotification);
begin
  FWindow.windowDidResignKey(notification);
end;

procedure TFMXWindowDelegate.windowDidResize(notification: NSNotification);
begin
  FWindow.windowDidResize(notification);
end;

procedure TFMXWindowDelegate.windowDidDeminiaturize(notification: NSNotification);
begin
  FWindow.windowDidDeminiaturize(notification);
end;

procedure TFMXWindowDelegate.windowDidEnterFullScreen(notification: NSNotification);
begin
  FWindow.windowDidEnterFullScreen(notification);
end;

procedure TFMXWindowDelegate.windowDidExitFullScreen(notification: NSNotification);
begin
  FWindow.windowDidExitFullScreen(notification);
end;

procedure TFMXWindowDelegate.windowDidMiniaturize(notification: NSNotification);
begin
  FWindow.windowDidMiniaturize(notification);
end;

function TFMXWindowDelegate.windowShouldClose(Sender: Pointer): Boolean;
begin
  Result := FWindow.windowShouldClose(Sender);
end;

procedure TFMXWindowDelegate.windowWillClose(notification: NSNotification);
var
  NSApp: NSApplication;
  ModWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
    ModWin := NSApp.modalWindow;
    if (ModWin <> nil) and (FWindow <> nil) and
      ((ModWin as ILocalObject).GetObjectID = (FWindow.Super as ILocalObject).GetObjectID) then
    begin
      NSApp.abortModal;
    end;
    FWindow.windowWillClose(notification);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := WindowHandleToPlatform(AHandle).Form;
end;

procedure objc_msgSendNSRect(theReceiver: Pointer; theSelector: Pointer; dirtyRect: NSRect); cdecl;
  external libobjc name _PU + 'objc_msgSend';

procedure frameDrawRect(Self: Pointer; _cmd: Pointer; dirtyRect: NSRect); cdecl;
var
  nctx: NSGraphicsContext;
  windowRect: NSRect;
  cornerRadius: Single;
  Path: NSBezierPath;
  Wnd: NSWindow;
  WindowBorder: TWindowBorderCocoa;
  Form: TCommonCustomForm;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Wnd := TNSView.Wrap(Self).window;
    Form := TMacWindowHandle.FindForm(Wnd);
    if (Form <> nil) and Form.Border.IsSupported then
    begin
      WindowBorder := TWindowBorderCocoa(Form.Border.WindowBorder);

      windowRect := Wnd.frame;
      windowRect.origin.x := 0;
      windowRect.origin.y := 0;
      cornerRadius := 4;

      nctx := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext);
      CGContextClearRect(nctx.graphicsPort, windowRect);

      Path := TNSBezierPath.Wrap(TNSBezierPath.OCClass.bezierPathWithRoundedRect(windowRect, cornerRadius, cornerRadius));
      Path.addClip;

      CGContextTranslateCTM(nctx.graphicsPort, 0, Wnd.frame.size.height);
//      CGContextScaleCTM(nctx.graphicsPort, 1, -1);
      WindowBorder.Paint(nctx.graphicsPort);
    end
    else
    begin
      objc_msgSendNSRect(Self, sel_getUid('drawRectOriginal:'), dirtyRect);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.HookFrame(const NSWin: NSWindow);
var
  FrameClass: Pointer;
  M1, M2: Pointer;
  AutoReleasePool: NSAutoreleasePool;
begin
  Inc(FClassHookCount);
  if FClassHookCount > 1 then Exit;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    // Replace drawRect method on window frame
    FrameClass := object_getClass(TNSView.Wrap(NSWin.contentView).superview);
    class_addMethod(FrameClass, sel_getUid('drawRectOriginal:'), @frameDrawRect, 'v@:@{NSRect={NSPoint=ff}{NSSize=ff}}');
    M1 := class_getInstanceMethod(FrameClass, sel_getUid('drawRect:'));
    M2 := class_getInstanceMethod(FrameClass, sel_getUid('drawRectOriginal:'));
    method_exchangeImplementations(M1, M2);
    //
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.UnHookFrame(const NSWin: NSWindow);
var
  FrameClass: Pointer;
  M1, M2: Pointer;
  AutoReleasePool: NSAutoreleasePool;
begin
  Dec(FClassHookCount);
  if FClassHookCount > 0 then Exit;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    // restore old
    FrameClass := object_getClass(TNSView.Wrap(NSWin.contentView).superview);
    M1 := class_getInstanceMethod(FrameClass, sel_getUid('drawRect:'));
    M2 := class_getInstanceMethod(FrameClass, sel_getUid('drawRectOriginal:'));
    method_exchangeImplementations(M1, M2);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
var
  Style: NSUInteger;
  FMXWin: TFMXWindow;
  NSWin: NSWindow;
  NSTitle: NSString;
  R: NSRect;
  LocalObj: ILocalObject;
  DraggedTypes: array[0..3] of Pointer;
  RegTypes: NSArray;
  PaintControl: IPaintControl;
  AutoReleasePool: NSAutoReleasePool;
  LParentWindow: NSWindow;
  ParentLevel, NewLevel: NSInteger;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FMXWin := TFMXWindow.Create;
    NSWin := NSWindow(FMXWin.Super);
    if AForm.Transparency or (AForm.BorderStyle = TFmxFormBorderStyle.None) then
      Style := NSBorderlessWindowMask
    else
    begin
      Style := NSTitledWindowMask or NSUnifiedTitleAndToolbarWindowMask;
      if AForm.BorderStyle <> TFmxFormBorderStyle.None then
      begin
        if TBorderIcon.biMinimize in AForm.BorderIcons then
          Style := Style or NSMiniaturizableWindowMask;
        if TBorderIcon.biMaximize in AForm.BorderIcons then
          Style := Style or NSWindowZoomButton;
        if TBorderIcon.biSystemMenu in AForm.BorderIcons then
          Style := Style or NSClosableWindowMask;
      end;
      if AForm.BorderStyle in [TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin] then
        Style := Style or NSResizableWindowMask;
    end;
    R := TNSWindow.OCClass.contentRectForFrameRect(MakeNSRect(AForm.Left,
      TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - AForm.Top - AForm.height,
      AForm.width, AForm.height), Style);
    NSWin.initWithContentRect(R, Style, NSBackingStoreBuffered, True);
    NSWin.setAcceptsMouseMovedEvents(True);
    NSWin.setReleasedWhenClosed(False);
    NSWin.setShowsToolbarButton(True);
    if Supports(NSWin, NSPanel) then
    begin
      (NSWin as NSPanel).setBecomesKeyOnlyIfNeeded(True);
      (NSWin as NSPanel).setWorksWhenModal(True);
      (NSWin as NSPanel).setFloatingPanel(True);
    end;
    NSWin.useOptimizedDrawing(True);
    NSTitle := StrToNSStr(AForm.Caption);
    NSWin.setTitle(NSTitle);

    // set level of window
    case TOpenCustomForm(AForm).FormStyle of
      TFormStyle.Popup: NewLevel := kCGMinimumWindowLevelKey;
      TFormStyle.StayOnTop: NewLevel := kCGFloatingWindowLevelKey;
    else
      NewLevel := kCGBaseWindowLevelKey;
    end;
    if (AForm.ParentForm <> nil) and (AForm.ParentForm.Handle <> nil) then
    begin
      LParentWindow := WindowHandleToPlatform(AForm.ParentForm.Handle).Wnd;
      if LParentWindow <> nil then
      begin
        ParentLevel := LParentWindow.level;
        if ParentLevel < kCGScreenSaverWindowLevelKey then
          Inc(ParentLevel);
        NewLevel := Max(NewLevel, ParentLevel);
      end;
    end;
    NSWin.setLevel(NewLevel);
    FMXWin.Wnd := AForm;

    HookFrame(NSWin);

    R := MakeNSRect(0, 0, R.size.width, R.size.height);

    if TWindowStyle.GPUSurface in AForm.WindowStyle then
      FMXWin.FViewObj := TFMXView3D.Create(FMXWin, R)
    else
      FMXWin.FViewObj := TFMXView.Create(FMXWin, R);
    if Supports(FMXWin.FViewObj, ILocalObject, LocalObj) then
    begin
      if Supports(AForm, IPaintControl, PaintControl) then
        PaintControl.ContextHandle := THandle(LocalObj.GetObjectID);
      FMXWin.FViewObj._AddRef;
    end;
    FMXWin.View.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
      TNSView.Wrap(NSWin.contentView).setAutoresizesSubviews(True);
      TNSView.Wrap(NSWin.contentView).addSubview(FMXWin.View);

    if AForm.Transparency then
    begin
      NSWin.setOpaque(False);
      NSWin.setHasShadow(False);
    end
    else
    begin
      NSWin.setOpaque(True);
    end;

    DraggedTypes[0] := (NSPasteboardTypeString as ILocalObject).GetObjectID;
    DraggedTypes[1] := (NSFMXPBoardtype as ILocalObject).GetObjectID;
    DraggedTypes[2] := (NSFilenamesPboardType as ILocalObject).GetObjectID;
    DraggedTypes[3] := nil;
    RegTypes := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@DraggedTypes[0], 3));
    NSWin.registerForDraggedTypes(RegTypes);

    FMXWin.FDelegate := TFMXWindowDelegate.Create(FMXWin);
    NSWin.setDelegate(FMXWin.FDelegate);

    Result := TMacWindowHandle.Create(FMXWin);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
begin
  Result := FMX.Forms.Border.Mac.CreateWindowBorder(AForm);
end;

procedure TPlatformCocoa.DestroyWindow(const AForm: TCommonCustomForm);
var
  NSWin: NSWindow;
  LFMXWindow: TFMXWindow;
  PaintControl: IPaintControl;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if AForm.Handle <> nil then
    begin
      LFMXWindow := TFMXWindow(WindowHandleToPlatform(AForm.Handle).Handle);
      LFMXWindow.FViewObj.FOwner := nil;
      NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
      NSWin.setParentWindow(nil);
      UnHookFrame(NSWin);
      if NSWin.isVisible then
        NSWin.orderOut(nil);
      if Supports(AForm, IPaintControl, PaintControl) then
        PaintControl.ContextHandle := 0;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.ReleaseWindow(const AForm: TCommonCustomForm);
begin
  if AForm <> nil then
  begin
    DoReleaseWindow(AForm);
  end;
end;

procedure TPlatformCocoa.DoReleaseWindow(AForm: TCommonCustomForm);
var
  AutoReleasePool: NSAutoreleasePool;
  LDisableClosePopups: Boolean;
  Wnd: NSWindow;
begin
  if AForm <> nil then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      if AForm.Handle <> nil then
      begin
        LDisableClosePopups := FDisableClosePopups;
        try
          if AForm.FormStyle = TFormStyle.Popup then
            FDisableClosePopups := True;
          if (AForm.FormStyle = TFormStyle.Popup) and
              (AForm.ParentForm <> nil) and
             (AForm.ParentForm.FormStyle = TFormStyle.Popup) then
          begin
            Wnd := WindowHandleToPlatform(AForm.ParentForm.Handle).Wnd;
            if (Wnd <> nil) and (NSApp <> nil) then
              Wnd.makeKeyAndOrderFront((NSApp as ILocalObject).GetObjectID);
          end;
          Wnd := WindowHandleToPlatform(AForm.Handle).Wnd;
          if Wnd <> nil then
          begin
            Wnd.setOneShot(True);
            Wnd.orderOut(nil);
          end;
        finally
          FDisableClosePopups := LDisableClosePopups;
        end;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

procedure TPlatformCocoa.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
var
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    WindowHandleToPlatform(AForm.Handle).Wnd.setFrame(MakeNSRect(ARect.Left, TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - ARect.Bottom,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top), True);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    Result := RectF(NSWin.frame.origin.x, TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - NSWin.frame.origin.y - NSWin.frame.size.height,
      NSWin.frame.origin.x + NSWin.frame.size.width, TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - NSWin.frame.origin.y);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  Result := WindowHandleToPlatform(AForm.Handle).Wnd.backingScaleFactor;
end;

procedure TPlatformCocoa.InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
	const ACloseQueryProc: TInputCloseQueryProc);
var
  LResult: TModalResult;
  LValues: array of string;
  I: Integer;
begin
  SetLength(LValues, Length(ADefaultValues));
  for I := Low(ADefaultValues) to High(ADefaultValues) do
    LValues[I] := ADefaultValues[I];
  if InputQuery(ACaption, APrompts, LValues) then
    LResult := mrOk
  else
    LResult := mrCancel;
  if Assigned(ACloseQueryProc) then
    ACloseQueryProc(LResult, LValues);
end;

procedure TPlatformCocoa.InvalidateImmediately(const AForm: TCommonCustomForm);
begin
  InvalidateWindowRect(AForm, AForm.ClientRect);
end;

procedure TPlatformCocoa.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
var
  View: NSView;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if IntersectRect(R, RectF(0, 0, AForm.width, AForm.height)) then
    begin
      View := WindowHandleToPlatform(AForm.Handle).View;
      View.setNeedsDisplayInRect(MakeNSRect(R.Left, View.bounds.size.height - R.Bottom, R.Right - R.Left, R.Bottom - R.Top));
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.IsMenuBarOnWindowBorder: boolean;
begin
  Result := False;
end;

constructor TFMXAlertDelegate.Create;
begin
  inherited;
end;

function TFMXAlertDelegate.GetObjectID: Pointer;
begin
  Result := inherited;
end;

function TFMXAlertDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(AlertDelegate);
end;

procedure TFMXAlertDelegate.alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer);
var
  R: Integer;
begin
  R := returnCode - NSAlertFirstButtonReturn;
  if (R >= 0) and (R < Length(Results)) then
    Result := Results[R]
  else
    Result := mrCancel;
  Modal := False;
end;

var
  MsgTitles: array[TMsgDlgType] of string = (SMsgDlgWarning, SMsgDlgError, SMsgDlgInformation, SMsgDlgConfirm, '');

  ModalResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore,
    mrAll, mrNoToAll, mrYesToAll, 0, mrClose);
  ButtonCaptions: array[TMsgDlgBtn] of string = (
    SMsgDlgYes, SMsgDlgNo, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort,
    SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll,
    SMsgDlgHelp, SMsgDlgClose);

function TPlatformCocoa.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons;	const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt;
  const AHelpFileName: string): Integer;
var
  Alert: NSAlert;
  Delegate: TFMXAlertDelegate;
  Session: NSModalSession;
  NSWin: NSWindow;
  S: SEL;
  ActiveForm: TCommonCustomForm;
  R: Integer;
  AutoReleasePool: NSAutoreleasePool;

  procedure AddButtons(IsDefault: Boolean);
  var
    B: TMsgDlgBtn;
    procedure AddBtn(B: TMsgDlgBtn);
    begin
      SetLength(Delegate.Results, Length(Delegate.Results) + 1);
      Delegate.Results[High(Delegate.Results)] := ModalResults[B];
      Alert.addButtonWithTitle(StrToNSStr(ButtonCaptions[B]));
    end;
  begin
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if (B in AButtons) and (IsDefault xor (B <> ADefaultButton)) then
        AddBtn(B);
    if (not IsDefault) and (Length(Delegate.Results) = 0) then
      AddBtn(ADefaultButton);
  end;
begin
  ActiveForm := nil;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Delegate := TFMXAlertDelegate.Create;
    try
      Delegate.Modal := True;

      if Screen <> nil then
      begin
        PrepareClosePopups(nil);
        ClosePopupForms;
        ActiveForm := Screen.ActiveForm;
        if (ActiveForm <> nil) and (ActiveForm.Visible) and (ActiveForm.Handle <> nil) and
          not (ActiveForm.Owner is TPopup) then
        begin
          NSWin := WindowHandleToPlatform(ActiveForm.Handle).Wnd;
          if NSWin <> nil then
            NSWin.retain;
        end;
      end;

      S := sel_getUid('alertDidEndSelector:returnCode:contextInfo:');
      Alert := TNSAlert.Create;
      Alert.setInformativeText(StrToNSStr(AMessage));
      Alert.setMessageText(StrToNSStr(MsgTitles[ADialogType]));
      if ADialogType = TMsgDlgType.mtWarning then
        Alert.setAlertStyle(NSWarningAlertStyle)
      else if ADialogType = TMsgDlgType.mtError then
        Alert.setAlertStyle(NSCriticalAlertStyle)
      else
        Alert.setAlertStyle(NSInformationalAlertStyle);

      AddButtons(True);
      AddButtons(False);

      if NSWin <> nil then
      begin
        Alert.beginSheetModalForWindow(NSWin, Delegate.GetObjectID, S, nil);
        if TFmxFormState.Modal in ActiveForm.FormState then
          Session := nil
        else
          Session := NSApp.beginModalSessionForWindow(NSWin);
        Inc(FAlertCount);
        try
          while Delegate.Modal do
          begin
            if Session <> nil then
              NSApp.runModalSession(Session);
            HookObserverCallback(False);
          end;
        finally
          Dec(FAlertCount);
          if Session <> nil then
            NSApp.endModalSession(Session);
        end;
        Result := Delegate.Result;
      end
      else
      begin
        R := Alert.runModal - NSAlertFirstButtonReturn;
        if (R >= 0) and (R < Length(Delegate.Results)) then
          Result := Delegate.Results[R]
        else
          Result := mrCancel;
      end;
    finally
      if NSWin <> nil then
        NSWin.release;
      if (ActiveForm <> nil) and (FAlertCount = 0) then
        ActiveForm.Activate;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: LongInt;
  const AHelpFileName: string; const ACloseDialogProc: TInputCloseDialogProc);
var
  LResult: TModalResult;
begin
  LResult := MessageDialog(AMessage, ADialogType, AButtons, ADefaultButton, AX, AY, AHelpCtx, AHelpFileName);
  if Assigned(ACloseDialogProc) then
    ACloseDialogProc(LResult);
end;

function TPlatformCocoa.InputQuery(const ACaption: string; const APrompts: array of string;
  var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc): Boolean;
const
  InputWidth = 400;
  InputHeight = 24;
  VerticalSpacing = 60;
  PromptRelativePosition = 36;

  function CreatePrompt(const YPos: Single; const Prompt: string; out Password: Boolean): NSTextField;
  begin
    Result := TNSTextField.Wrap(TNSTextField.Alloc.initWithFrame(MakeNSRect(0, YPos, InputWidth, InputHeight)));
    Result.setDrawsBackground(False);
    Result.setEditable(False);
    Result.setSelectable(False);
    Result.setBordered(False);
    Result.setBackgroundColor(TNSColor.Wrap(TNSColor.OCClass.clearColor));
    Password := (Prompt.Length > 0) and (Prompt.Chars[0] < #32);
    Result.setStringValue(StrToNSStr(Prompt.Substring(IfThen(Password, 1, 0))));
  end;

  function CreateInput(const YPos: Single; const InitialValue: string; const Password: Boolean): NSTextField;
  begin
    if Password then
      Result := TNSSecureTextField.Wrap(TNSSecureTextField.Alloc.initWithFrame(MakeNSRect(0, YPos, InputWidth,
        InputHeight)))
    else
      Result := TNSTextField.Wrap(TNSTextField.Alloc.initWithFrame(MakeNSRect(0, YPos, InputWidth, InputHeight)));
    Result.setStringValue(StrToNSStr(InitialValue));
  end;

var
  Alert: NSAlert;
  Delegate: TFMXAlertDelegate;
  Session: NSModalSession;
  NSWin: NSWindow;
  S: SEL;
  View: NSView;
  Inputs: array of NSTextField;
  I: Integer;
  ActiveForm: TCommonCustomForm;
  AutoReleasePool: NSAutoreleasePool;
  Password: Boolean;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Result := False;
    if (Length(AValues) > 0) and (Length(APrompts) > 0) then
    begin
      ActiveForm := nil;
      Delegate := TFMXAlertDelegate.Create;
      try
        Delegate.Modal := True;
        SetLength(Delegate.Results, 2);
        Delegate.Results[0] := ModalResults[TMsgDlgBtn.mbOK];
        Delegate.Results[1] := ModalResults[TMsgDlgBtn.mbCancel];

        if Screen <> nil then
        begin
          ActiveForm := Screen.ActiveForm;
          if (ActiveForm <> nil) and (ActiveForm.Visible) and (ActiveForm.Handle <> nil) and
            not (ActiveForm.Owner is TPopup) then
          begin
            NSWin := WindowHandleToPlatform(ActiveForm.Handle).Wnd;
            if NSWin <> nil then
              NSWin.retain;
          end;
        end;

        S := sel_getUid('alertDidEndSelector:returnCode:contextInfo:');
        Alert := TNSAlert.Wrap(TNSAlert.Alloc.init);
        Alert.setMessageText(StrToNSStr(ACaption));

        Alert.addButtonWithTitle(StrToNSStr(ButtonCaptions[TMsgDlgBtn.mbOK]));
        Alert.addButtonWithTitle(StrToNSStr(ButtonCaptions[TMsgDlgBtn.mbCancel]));

        View := TNSView.Wrap(TNSView.Alloc.initWithFrame(MakeNSRect(0, 0, InputWidth, Length(AValues) * VerticalSpacing)));

        SetLength(Inputs, Length(AValues));
        for I := 0 to High(Inputs) do
        begin
          View.addSubview(CreatePrompt(View.frame.size.height - I * VerticalSpacing - PromptRelativePosition,
            APrompts[I], Password));
          Inputs[I] := CreateInput(View.frame.size.height - I * VerticalSpacing - VerticalSpacing, AValues[I], Password);
          View.addSubview(Inputs[I]);
        end;

        Alert.setAccessoryView(View);

        if NSWin <> nil then
        begin
          Alert.beginSheetModalForWindow(NSWin, Delegate.GetObjectID, S, nil);
          if TFmxFormState.Modal in ActiveForm.FormState then
            Session := nil
          else
            Session := NSApp.beginModalSessionForWindow(NSWin);
          Inc(FAlertCount);
          try
            while Delegate.Modal do
            begin
              if Session <> nil then
                NSApp.runModalSession(Session);
              HookObserverCallback(False);
            end;
          finally
            Dec(FAlertCount);
            if Session <> nil then
              NSApp.endModalSession(Session);
          end;
          Result := Delegate.Result = mrOk;
        end
        else
          Result := Alert.runModal = NSAlertFirstButtonReturn;

        for I := 0 to High(Inputs) do
          AValues[I] := NSStrToStr(Inputs[I].stringValue);
      finally
        if NSWin <> nil then
          NSWin.release;
        if (ActiveForm <> nil) and (FAlertCount = 0) then
          ActiveForm.Activate;
      end;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.AllocHandle(const Objc: IObjectiveC): TFmxHandle;
begin
  Result := NewFmxHandle;
  TMonitor.Enter(FObjectiveCMap);
  try
    FObjectiveCMap.Add(Result, Objc);
  finally
    TMonitor.Exit(FObjectiveCMap);
  end;
end;

function TPlatformCocoa.NewFmxHandle: TFmxHandle;
begin
{$IFDEF CPUX64}
  Result := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ENDIF}
{$IFDEF CPUX86}
  Result := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
end;

procedure TPlatformCocoa.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    NSWin.setTitle(StrToNSStr(ACaption));
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  if (AForm.Visible or (TFmxFormState.Showing in AForm.FormState)) and FCanSetState then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
      if NSWin <> nil then
      begin
        if (NSWin.styleMask and NSFullScreenWindowMask) <> 0 then
          AForm.WindowState := TWindowState.wsMaximized
        else
          case AState of
            TWindowState.wsMinimized:
              if not NSWin.isMiniaturized then
                NSWin.miniaturize(nil);
            TWindowState.wsNormal:
              begin
                if NSWin.isMiniaturized then
                  NSWin.deminiaturize(nil);
                if IsZoomed(NSWin) then
                  NSWin.zoom(nil);
              end;
            TWindowState.wsMaximized:
              begin
                if NSWin.isMiniaturized then
                  NSWin.deminiaturize(nil);
                if not IsZoomed(NSWin) then
                  NSWin.zoom(nil);
              end;
          end;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

procedure TPlatformCocoa.RegisterCanvasClasses;
begin
  if GlobalUseGPUCanvas then
    FMX.Canvas.GPU.RegisterCanvasClasses;
  FMX.Canvas.Mac.RegisterCanvasClasses;
end;

procedure TPlatformCocoa.UnregisterCanvasClasses;
begin
  if GlobalUseGPUCanvas then
    FMX.Canvas.GPU.UnregisterCanvasClasses;
  FMX.Canvas.Mac.UnregisterCanvasClasses;
end;

procedure TPlatformCocoa.RegisterContextClasses;
begin
  FMX.Context.Mac.RegisterContextClasses;
end;

procedure TPlatformCocoa.UnregisterContextClasses;
begin
  FMX.Context.Mac.UnregisterContextClasses;
end;

procedure TPlatformCocoa.ReleaseCapture(const AForm: TCommonCustomForm);
begin
//  Windows.ReleaseCapture;
end;

procedure TPlatformCocoa.SetCapture(const AForm: TCommonCustomForm);
begin
//  Windows.SetCapture(AHandle);
end;

function TPlatformCocoa.GetCaretWidth: Integer;
begin
  Result := 1;
end;

function TPlatformCocoa.GetMenuShowDelay: Integer;
begin
  Result := 150;
end;

function TPlatformCocoa.GetClientSize(const AForm: TCommonCustomForm): TPointF;
var
  LView: NSView;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    LView := WindowHandleToPlatform(AForm.Handle).View;
    Result := PointF(LView.frame.size.width, LView.frame.size.height);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.HideWindow(const AForm: TCommonCustomForm);
var
  NSWin: NSWindow;
begin
  if (AForm <> nil) and (AForm.Handle <> nil) then
  begin
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if NSWin <> nil then
    begin
      if (NSWin.styleMask and NSFullScreenWindowMask) <> 0 then
      begin
        NSWin.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary);
        NSWin.toggleFullScreen(nil);
      end
      else
        NSWin.orderOut(nil);
      NSWin.setParentWindow(nil);
    end;
  end;
end;

procedure TPlatformCocoa.ShowWindow(const AForm: TCommonCustomForm);
var
  NSWin: NSWindow;
  frame: NSRect;
  AutoReleasePool: NSAutoReleasePool;
  LWindowState: TWindowState;
  ParentForm: TCommonCustomForm;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    LWindowState := AForm.WindowState;
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if LWindowState <> TWindowState.wsMinimized then
    begin
      SetWindowState(AForm, LWindowState);
      NSWin.makeKeyAndOrderFront((NSApp as ILocalObject).GetObjectID);
    end;
    if AForm = Application.MainForm then
    begin
      NSWin.makeMainWindow;
      NSApp.activateIgnoringOtherApps(True);
    end
    else if IsPopupForm(AForm) then
    begin
      ParentForm := AForm.ParentForm;
      while (ParentForm <> nil) and not (TFmxFormState.Modal in ParentForm.FormState) do
        ParentForm := ParentForm.ParentForm;
      if ParentForm <> nil then
        NSWin.setParentWindow(WindowHandleToPlatform(ParentForm.Handle).Wnd);
    end;
    SetWindowState(AForm, LWindowState);
    frame := NSWin.frame;
      AForm.SetBounds(round(frame.origin.x),
        round(TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - frame.origin.y - frame.size.height),
        round(frame.size.width), round(frame.size.height));
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.BringToFront(const AForm: TCommonCustomForm);
var
  AutoReleasePool: NSAutoreleasePool;
  NSWin: NSWindow;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if (NSWin <> nil) and NSWin.isVisible then
      NSWin.orderFront(nil);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.SendToBack(const AForm: TCommonCustomForm);
var
  AutoReleasePool: NSAutoreleasePool;
  NSWin: NSWindow;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if (NSWin <> nil) and NSWin.isVisible then
      NSWin.orderBack(nil);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.Activate(const AForm: TCommonCustomForm);
var
  AutoReleasePool: NSAutoreleasePool;
  NSWin: NSWindow;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    NSApp.activateIgnoringOtherApps(True);
    NSWin.makeKeyAndOrderFront((NSApp as ILocalObject).GetObjectID);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
var
  Session: NSModalSession;
  MR: Integer;
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    AForm.HandleNeeded;
    if FModalStack = nil then
      FModalStack := TStack<TCommonCustomForm>.Create;
    FRestartModal := False;
    FModalStack.Push(AForm);
    AForm.Show;
    try
      Result := mrNone;
      NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
      if NSWin <> nil then
        NSWin.retain;
      AForm.ModalResult := mrNone;
      Session := NSApp.beginModalSessionForWindow(NSWin);
      AForm.BringToFront;
      while True do
      begin
        MR := NSApp.runModalSession(Session);
        if MR <> NSRunContinuesResponse then
        begin
          if FRestartModal then
          begin
            FRestartModal := False;
            NSApp.endModalSession(Session);
            Session := NSApp.beginModalSessionForWindow(NSWin);
            Continue;
          end;
          if AForm.Visible then
            AForm.Hide;
          Result := AForm.ModalResult;
          if csDestroying in AForm.ComponentState then
            DoReleaseWindow(AForm);
          FModalStack.Pop;
          if FModalStack.Count > 0 then
            FRestartModal := True;
          Break;
        end;
        if AForm.ModalResult <> 0 then
        begin
          AForm.CloseModal;
          if AForm.ModalResult <> 0 then
          begin
            NSApp.stopModal;
            Continue;
          end;
        end;
        HookObserverCallback(False);
      end;
      NSApp.endModalSession(Session);
    finally
      if NSWin <> nil then
        NSWin.release;
      if (FModalStack.Count > 0) and (FModalStack.Peek = AForm) then
        FModalStack.Pop;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  np: NSPoint;
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    np := NSPoint(Point);
    np.y := TNSView.Wrap(NSWin.contentView).bounds.size.height - np.y;
    Result := TPointF(NSWin.convertBaseToScreen(np));
    Result.y := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - Result.y;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  np: NSPoint;
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    np := NSPoint(Point);
    np.y := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - np.y;
    Result := TPointF(NSWin.convertScreenToBase(np));
    Result.y := TNSView.Wrap(NSWin.contentView).bounds.size.height - Result.y;
  finally
    AutoReleasePool.release;
  end;
end;

{ Menus }

function TPlatformCocoa.FindFormAtScreenPos(var AForm: TCommonCustomForm; const ScreenMousePos: TPointF): Boolean;
var
  I: Integer;
  ScreenPos: TPoint;

  function Contains(const Form: TCommonCustomForm): Boolean;
  var
    R: TRect;
  begin
    Result := False;
    if (Form <> nil) and Form.Visible and not Form.Released then
    begin
      if Form is TCustomPopupForm then
        R := TRect.Create(TCustomPopupForm(Form).ScreenContentRect.Round)
      else
        R := TRect.Create(TPoint.Create(Form.Left, Form.Top), Form.Width, Form.Height);
      Result := R.Contains(ScreenPos);
      if Result then
        AForm := Form;
    end;
  end;

begin
  Result := False;
  if Screen <> nil then
  begin
    ScreenPos := TPoint.Create(Round(RoundTo(ScreenMousePos.X, 0)), Round(RoundTo(ScreenMousePos.y, 0)));
    // Popups
    for I := Screen.PopupFormCount - 1 downto 0 do
      if Contains(Screen.PopupForms[I]) then
        Exit(True);
    // top forms
    for I := Screen.FormCount - 1 downto 0 do
      if (Screen.Forms[I].FormStyle = TFormStyle.StayOnTop) and Contains(Screen.Forms[I]) then
        Exit(True);
    // active form
    if Contains(Screen.ActiveForm) then
      Exit(True);
    // other forms
    for I := Screen.FormCount - 1 downto 0 do
      if (Screen.Forms[I].FormStyle <> TFormStyle.StayOnTop) and Contains(Screen.Forms[I]) then
        Exit(True);
  end;
end;

function TPlatformCocoa.ShowContextMenu(const AForm: TCommonCustomForm; const AScreenMousePos: TPointF): Boolean;
var
  Obj: IControl;
begin
  Result := False;
  if AForm <> nil then
  begin
    Obj := IControl(AForm.ObjectAtPoint(AScreenMousePos));
    if (Obj <> nil) and (Obj.GetObject <> nil) then
      try
        Obj.SetFocus;
        Result := Obj.ShowContextMenu(AScreenMousePos);
      except
        HandleException(AForm);
      end;
  end;
end;

procedure TPlatformCocoa.MenuLoopEvent(const EventRec: TEventRec; var CancelIdle, CancelDefaultAction: Boolean);
  procedure EndLoop;
  var
    LView: IMenuView;
  begin
    LView := FMenuStack.Peek;
    while LView <> nil do
    begin
      LView.Loop := False;
      LView.Selected := nil;
      LView := LView.ParentView;
    end;
  end;

  procedure EndLoopAndClose;
  var
    LView: IMenuView;
  begin
    EndLoop;
    LView := FMenuStack.Peek;
    while (LView <> nil) do
    begin
      if LView.Parent is TPopup then
        TPopup(LView.Parent).IsOpen := False;
      LView := LView.ParentView;
    end;
  end;

  function IsItemSelectable(const Item: TFmxObject): Boolean;
  begin
    Result := (Item is TMenuItem) and TMenuItem(Item).Visible and (TMenuItem(Item).Text <> SMenuSeparator);
  end;

  function ForwardSelectNextMenuItem(AView: IMenuView; AStartInd, AEndInd: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if AView <> nil then
      for I := AStartInd to AEndInd do
        if IsItemSelectable(AView.GetItem(I)) then
        begin
          AView.Selected := TMenuItem(AView.GetItem(I));
          Result := True;
          Break;
        end;
  end;

  function BackwardSelectNextMenuItem(AView: IMenuView; AStartInd, AEndInd: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if AView <> nil then
      for I := AStartInd downto AEndInd do
        if IsItemSelectable(AView.GetItem(I)) then
        begin
          AView.Selected := TMenuItem(AView.GetItem(I));
          Result := True;
          Break;
        end;
  end;

  procedure SelectFirstMenuItem(AView: IMenuView);
  var
    I: Integer;
  begin
    if AView <> nil then
    begin
      I := 0;
      while (I < AView.GetItemsCount) and not IsItemSelectable(AView.GetItem(I)) do
        Inc(I);
      if I < AView.GetItemsCount then
        AView.Selected := TMenuItem(AView.GetItem(I));
    end;
  end;

  procedure SelectLastMenuItem(AView: IMenuView);
  var
    I: Integer;
  begin
    if AView <> nil then
    begin
      I := AView.GetItemsCount - 1;
      while (I >= 0) and not IsItemSelectable(AView.GetItem(I)) do
        Dec(I);
      if I >= 0 then
        AView.Selected := TMenuItem(AView.GetItem(I));
    end;
  end;

  procedure SelectPrevMenuItem(AView: IMenuView);
  begin
    if AView <> nil then
      if (AView.Selected = nil) or
        not BackwardSelectNextMenuItem(AView, AView.Selected.Index - 1, 0) then
        SelectLastMenuItem(AView);
  end;

  procedure SelectNextMenuItem(AView: IMenuView);
  begin
    if AView <> nil then
      if (AView.Selected = nil) or
        not ForwardSelectNextMenuItem(AView, AView.Selected.Index + 1, AView.GetItemsCount - 1) then
        SelectFirstMenuItem(AView);
  end;

  function RedirectMouseEventToParentForm(AView: IMenuView): Boolean;
  var
    Root: IRoot;
    Form: TCommonCustomForm;
    FormMousePos: TPointF;
    LEventRec: TEventRec;
  begin
    Result := False;
    if (AView <> nil) and (AView.Parent <> nil) and (AView.GetObject <> nil) then
    begin
      Root := AView.GetObject.Root;
      if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
      begin
        Form := TCommonCustomForm(Root.GetObject);
        while (Form <> nil) and (IsPopup(Form)) do
          Form := Form.ParentForm;
        if (Form <> nil) and (Form.Handle <> nil) and (EventRec.EventKind = TEventKind.MouseDown) then
        begin
          FormMousePos := Form.ScreenToClient(EventRec.ScreenMousePos);
          LEventRec := EventRec;
          LEventRec.Form := Form;
          LEventRec.FormMousePos := FormMousePos;
          LEventRec.Event := nil;
          TMouseDownTimer.Create(LEventRec);
          Result := True;
        end;
      end;
    end;
  end;

  procedure FindViewByEvent(var AView: IMenuView; var Item: TMenuItem);
  var
    Obj: IControl;
  begin
    AView := nil;
    Obj := nil;
    Item := nil;
    repeat
      if AView = nil then
        AView := FMenuStack.Peek
      else
        AView := AView.ParentView;
      if AView <> nil then
      begin
        Obj := AView.ObjectAtPoint(EventRec.ScreenMousePos);
        if (Obj <> nil) and (Obj.GetObject is TMenuItem) then
          Item := TMenuItem(Obj.GetObject);
      end;
    until (AView = nil) or (Item <> nil);
  end;

var
  LForm: TCommonCustomForm;
  LFormMousePos: TPointF;
  Key: Word;
  LView, LCurrentView: IMenuView;
  Item: TMenuItem;
begin
  if (FMenuStack <> nil) and (FMenuStack.Count > 0) then
  begin
    LView := FMenuStack.Peek;
    case EventRec.EventKind of
      TEventKind.Other:
        begin
          case EventRec.Event.&type of
            NSAppKitDefined:
              EndLoopAndClose;
            NSSystemDefined:
              if not FindFormAtScreenPos(LForm, EventRec.ScreenMousePos) then
                EndLoopAndClose;
          end;
        end;
      TEventKind.Key:
        begin
          Key := EventRec.Event.keyCode;
          if EventRec.Event.&type = NSKeyDown then
            case Key of
              KEY_ENTER, KEY_NUMPADENTER:
                if (LView.Selected <> nil) then
                begin
                  if LView.Selected.HavePopup then
                    LView.Selected.NeedPopup
                  else
                  begin
                    TOpenMenuItem(LView.Selected).Click;
                    EndLoopAndClose;
                  end;
                end
                else
                  EndLoopAndClose;
              KEY_ESC:
                begin
                  LView.Selected := nil;
                  EndLoopAndClose;
                end;
              KEY_LEFT:
                if (LView.ParentView <> nil) and LView.ParentView.IsMenuBar then
                begin
                  LView.Loop := False;
                  SelectPrevMenuItem(LView.ParentView);
                  if LView.ParentView.Selected <> nil then
                    LView.ParentView.Selected.NeedPopup;
                end;
              KEY_RIGHT:
                if (LView.ParentView <> nil) and LView.ParentView.IsMenuBar then
                begin
                  LView.Loop := False;
                  SelectNextMenuItem(LView.ParentView);
                  if LView.ParentView.Selected <> nil then
                    LView.ParentView.Selected.NeedPopup;
                end;
              KEY_UP:
                SelectPrevMenuItem(LView);
              KEY_DOWN:
                SelectNextMenuItem(LView);
              KEY_HOME, KEY_PAGUP:
                SelectFirstMenuItem(LView);
              KEY_END, KEY_PAGDN:
                SelectLastMenuItem(LView);
            end;
          CancelDefaultAction := True;
        end;
      TEventKind.MouseDown:
        begin
          FindViewByEvent(LCurrentView, Item);
          if (Item <> nil) and (LCurrentView = FMenuStack.Peek) then
          begin
            if not Item.IsSelected and Item.HavePopup then
              Item.NeedPopup
            else
            begin
              TOpenMenuItem(Item).Click;
              EndLoopAndClose;
            end;
          end
          else if LCurrentView = nil then
          begin
            RedirectMouseEventToParentForm(LView);
            EndLoopAndClose;
            CancelDefaultAction := True;
          end
          else
            CancelDefaultAction := True;
        end;
      TEventKind.MouseMove:
        if (LView.GetObject <> nil) and (LView.GetObject.Root is TCommonCustomForm) then
        begin
          LForm := TCommonCustomForm(LView.GetObject.Root);
          LFormMousePos := LForm.ScreenToClient(EventRec.ScreenMousePos);
          try
            LForm.MouseMove(EventRec.Shift, LFormMousePos.X, LFormMousePos.y);
          except
            HandleException(LForm);
          end;
          CancelDefaultAction := True;
          CancelIdle := True;
        end;
    end;
  end;
end;

procedure TPlatformCocoa.StartMenuLoop(const AView: IMenuView);
var
  AutoReleasePool: NSAutoreleasePool;
begin
  if (AView <> nil) and (AView.GetObject <> nil) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      if FMenuStack = nil then
        FMenuStack := TStack<IMenuView>.Create;
      FMenuStack.Push(AView);
      try
        HookEvent := MenuLoopEvent;
        AView.Loop := True;
        try
          while AView.Loop do
            HookObserverCallback(False);
        finally
          AView.Loop := False;
        end;
      finally
        FMenuStack.Pop;
        if FMenuStack.Count = 0 then
        begin
          HookEvent := nil;
          FreeAndNil(FMenuStack);
        end;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

function TPlatformCocoa.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  case AComponent of
    TComponentKind.Calendar:
      Result := True;
  else
    Result := False;
  end;
end;

function TPlatformCocoa.HandleToObjC(FmxHandle: TFmxHandle; const IID: TGUID; out Intf): Boolean;
begin
  Result := Supports(HandleToObjC(FmxHandle), IID, Intf);
end;

procedure TPlatformCocoa.ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
begin
  FMX.Helpers.Mac.ShortCutToKey(ShortCut, Key, Shift);
end;

function TPlatformCocoa.ShortCutToText(ShortCut: TShortCut): string;
begin
  Result := FMX.Helpers.Mac.ShortCutToText(ShortCut);
end;

function TPlatformCocoa.TextToShortCut(Text: string): Integer;
begin
  Result := FMX.Helpers.Mac.TextToShortCut(Text);
end;

  { TFMXOSMenuItem }

type
  FMXOSMenuItem = interface(NSMenuItem)
    ['{A922028A-C1EE-41AF-8345-26671E6879AD}']
    procedure DispatchMenuClick(Sender: Pointer); cdecl;
  end;

  TFMXOSMenuItem = class(TOCLocal)
  private
    FMXMenuItem: TMenuItem;
  public
    constructor Create(const AFMXMenuItem: TMenuItem);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure DispatchMenuClick(Sender: Pointer); cdecl;
  end;

constructor TFMXOSMenuItem.Create(const AFMXMenuItem: TMenuItem);
var
  Key: Char;
  ModMask: NSUInteger;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    inherited Create;
    FMXMenuItem := AFMXMenuItem;
    ShortCutToMACKey(FMXMenuItem.ShortCut, Key, ModMask);
    UpdateObjectID(NSMenuItem(Super).initWithTitle(StrToNSStr(DelAmp(FMXMenuItem.Text)),
      sel_getUid(MarshaledAString('DispatchMenuClick:')), StrToNSStr(Key)));

    if AFMXMenuItem.IsChecked then
      NSMenuItem(Super).setState(NSOnState);

    if AFMXMenuItem.Enabled then
      NSMenuItem(Super).setEnabled(True)
    else
      NSMenuItem(Super).setEnabled(False);

    SetMenuBitmap(NSMenuItem(Super), AFMXMenuItem);

    NSMenuItem(Super).setKeyEquivalentModifierMask(ModMask);
    NSMenuItem(Super).setTarget(GetObjectID);
    TFMXMenuDelegate.RegisterMenu((Self as ILocalObject).GetObjectID, AFMXMenuItem);

  finally
    AutoReleasePool.Release;
  end;
end;

destructor TFMXOSMenuItem.Destroy;
begin
  TFMXMenuDelegate.UnregisterMenu((Self as ILocalObject).GetObjectID);
  NSMenuItem(Super).Release;
  inherited;
end;

procedure TFMXOSMenuItem.DispatchMenuClick(Sender: Pointer);
begin
  try
    if ((Now - Application.LastKeyPress) > IntervalPress) and
       (FMXMenuItem is TMenuItem) then
      TOpenMenuItem(FMXMenuItem).Click;
  except
    HandleException(Self);
  end;
end;

function TFMXOSMenuItem.GetObjectiveCClass: PTypeInfo;
begin
    Result := TypeInfo(FMXOSMenuItem);
end;

procedure TPlatformCocoa.CreateChildMenuItems(AChildMenu: IItemsContainer; AParentMenu: NSMenu);
var
  I, J: Integer;
  LChildMenuItem: TOpenMenuItem;
  LNSSubMenuItem: TFMXOSMenuItem;
  LNewSubMenu: NSMenu;
  Items: IItemsContainer;
  VisibleItemCount: Integer;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    AParentMenu.setAutoenablesItems(False);
    for J := 0 to AChildMenu.GetItemsCount - 1 do
    if AChildMenu.GetItem(J) is TMenuItem then
    begin
      LChildMenuItem := TOpenMenuItem(AChildMenu.GetItem(J));
      if LChildMenuItem.Visible then
      begin
        if LChildMenuItem.Text = SMenuSeparator then
          AParentMenu.addItem(TNSMenuItem.Wrap(TNSMenuItem.OCClass.separatorItem))
        else
        begin
          LNSSubMenuItem := TFMXOSMenuItem.Create(LChildMenuItem);
          LChildMenuItem.Handle := AllocHandle(LNSSubMenuItem);
          if Supports(AChildMenu.GetItem(J), IItemsContainer, Items) then
          begin
            VisibleItemCount := 0;
            for I := 0 to Items.GetItemsCount - 1 do
              if (Items.GetItem(I) is TMenuItem) and
                 (TMenuItem(Items.GetItem(I)).Visible) then
                inc(VisibleItemCount);
            if VisibleItemCount > 0 then
            begin
              LNewSubMenu := NewNsMenu(TMenuItem(AChildMenu.GetItem(J)).Text);
              CreateChildMenuItems(Items, LNewSubMenu);
              NSMenuItem(LNSSubMenuItem.Super).setSubmenu(LNewSubMenu);
            end;
          end;
          AParentMenu.addItem(NSMenuItem(LNSSubMenuItem.Super));
        end;
      end;
    end;
  finally
    AutoReleasePool.Release;
  end;
end;

procedure TPlatformCocoa.RemoveChildHandles(const AMenu: IItemsContainer);
var
  I: Integer;
  MenuItem: TMenuItem;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    for I := 0 to AMenu.GetItemsCount - 1 do
      if (AMenu.GetItem(I) is TMenuItem) then
      begin
        MenuItem := TMenuItem(AMenu.GetItem(I));
        RemoveChildHandles(MenuItem);
        DestroyMenuItem(MenuItem);
      end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.ClearNSHandles;
var I: Integer;
begin
  for I := FNSHandles.Count - 1 downto 0 do
    DeleteHandle(FNSHandles[I]);
  FNSHandles.Clear;
end;

function TPlatformCocoa.NewNSMenu(const Text: string): NSMenu;
var
  LNSMenu: NSMenu;
  S: NSString;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    inherited;
    LNSMenu := TNSMenu.Alloc;
    S := StrToNSStr(DelAmp(Text));
    Result := TNSMenu.Wrap(LNSMenu.initWithTitle(S));
    FNSHandles.Add(AllocHandle(Result));
    Result.setDelegate(FFMXMenuDelegate);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.CreateFirstVisibleItem(var ANewMenu: NSMenu; var ANSMenuItem: NSMenuItem);
var
  AutoReleasePool: NSAutoreleasePool;
begin
  ANewMenu := nil;
  ANSMenuItem := nil;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FDefaultMenu := TCurrentMenu.Default;
    ANewMenu := NewNsMenu(Application.Title);
    ANSMenuItem := TNSMenuItem.Alloc;
    ANSMenuItem := TNSMenuItem.Wrap(ANSMenuItem.initWithTitle(StrToNSStr(Application.Title), nil, StrToNSStr('')));
    ANSMenuItem.setSubmenu(ANewMenu);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.CreateDefaultMenuItem;
var
  LNSMenuItem, LQuitItem: NSMenuItem;
  LNewMenu: NSMenu;
  Key: Char;
  ModifierMask: NSUInteger;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    CreateFirstVisibleItem(LNewMenu, LNSMenuItem);
    LQuitItem := TNSMenuItem.Alloc;
    ShortCutToMACKey(4177, Key, ModifierMask);
    LQuitItem := TNSMenuItem.Wrap(LQuitItem.initWithTitle(StrToNSStr(Format(SMenuAppQuit, [Application.Title])),
      sel_getUid(MarshaledAString('terminate:')), StrToNSStr(Key)));
    LQuitItem.setKeyEquivalentModifierMask(ModifierMask);

    LNewMenu.addItem(LQuitItem);
    if FMenuBar = nil then
      CreateOSMenu(nil, nil);
    FMenuBar.insertItem(LNSMenuItem, 0);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);
var
  LNSMenuItem: NSMenuItem;
  LNewMenu: NSMenu;
  MenuItem: TOpenMenuItem;
  I, J, VisibleItemCount: Integer;
  AutoReleasePool: NSAutoreleasePool;
  Native: INativeControl;
  First: Boolean;
  OldState: TMainMenuState;
begin
  if FMainMenuState in [TMainMenuState.Empty, TMainMenuState.Recreated] then
  begin
    OldState := FMainMenuState;
    try
      FMainMenuState := TMainMenuState.Recreating;
      AutoReleasePool := TNSAutoreleasePool.Create;
      try
        FMainMenu := NSApp.mainMenu;
        if FMainMenu = nil then
        begin
          FMainMenu := TNSMenu.Create;
          FMenuBar := TNSMenu.Wrap(FMainMenu.initWithTitle(StrToNSStr('')));
          NSApp.setMainMenu(FMainMenu);
        end;
        FMenuBar.removeAllItems;
        FMenuBar.setAutoenablesItems(False);
        ClearNSHandles;
        VisibleItemCount := 0;
        if AMenu <> nil then
        begin
          RemoveChildHandles(AMenu);
          if Supports(AMenu, INativeControl, Native) and Native.HandleSupported then
            for I := 0 to AMenu.GetItemsCount - 1 do
            if (AMenu.GetItem(I) is TMenuItem) and
               (TMenuItem(AMenu.GetItem(I)).Visible) then
              inc(VisibleItemCount);
        end;
        // TMainMenu items
        if VisibleItemCount > 0 then
        begin
          J := 0;
          First := True;
          for I := 0 to AMenu.GetItemsCount - 1 do
          if AMenu.GetItem(I) is TMenuItem then
          begin
            MenuItem := TOpenMenuItem(AMenu.GetItem(I));
            if MenuItem.Visible then
            begin
              if First then
              begin
                CreateFirstVisibleItem(LNewMenu, LNSMenuItem);
                First := False;
              end
              else
              begin
                FDefaultMenu := TCurrentMenu.Main;
                if J = VisibleItemCount - 1 then
                  LNewMenu := NewNSMenu(SSpotlightFeature)
                else
                  LNewMenu := NewNSMenu('');
                LNSMenuItem := TNSMenuItem.Alloc;
                LNSMenuItem := TNSMenuItem.Wrap(LNSMenuItem.initWithTitle(StrToNSStr(''), nil, StrToNSStr('')));
                LNSMenuItem.setSubmenu(LNewMenu);
                SetMenuBitmap(LNSMenuItem, MenuItem);
              end;

              if MenuItem.Enabled then
                LNSMenuItem.setEnabled(True)
              else
                LNSMenuItem.setEnabled(False);

              if MenuItem.IsChecked then
                LNSMenuItem.setState(NSOnState);

              if OldState > TMainMenuState.Empty then
                CreateChildMenuItems((MenuItem as IItemsContainer), LNewMenu);
              MenuItem.Handle := AllocHandle(LNSMenuItem);
              FMenuBar.addItem(LNSMenuItem);
              Inc(J);
            end;
          end;
          for I := 0 to AMenu.GetItemsCount - 1 do
            UpdateMenuItem(AMenu.GetItem(I) as IItemsContainer, [TMenuItemChange.Text]);
        end
        else
          CreateDefaultMenuItem;
      finally
        AutoReleasePool.release;
      end;
    finally
      if OldState < TMainMenuState.Recreated then
        FMainMenuState := Succ(OldState)
      else
        FMainMenuState := OldState;
    end;
  end;
end;

procedure TPlatformCocoa.UpdateMenuBar;
begin
  if FMenuBar <> nil then
    FMenuBar.removeAllItems;
  CreateDefaultMenuItem;
end;

procedure TPlatformCocoa.UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);
var
  I: Integer;
  LMenuItem: TMenuItem;
  LHandle: TFmxHandle;
  LNativeMenuItem: TFMXOSMenuItem;
  LNSMenuItem: NSMenuItem;
  IsTopLevelItem, IsFirstVisibleItem: Boolean;
  Key: Char;
  ModMask: NSUInteger;
  AutoReleasePool: NSAutoreleasePool;
  IObj: IObjectiveC;
  Parent: TFmxObject;
  MainMenu: TMainMenu;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if AItem.GetObject is TMenuItem then
      LMenuItem := TMenuItem(AItem.GetObject)
    else
      Exit;

    LHandle := (LMenuItem as INativeControl).Handle;
    // Invisible item does not have a handle
    if (LHandle = 0) then
    begin
      // If you changed the visibility, then re-creates all the menus
      if (TMenuItemChange.Visible in AChange) and LMenuItem.Visible then
      begin
        Parent := LMenuItem;
        while (Parent is TFmxObject) and not (Parent is TMainMenu) do
          Parent := Parent.Parent;
        if Parent is TMainMenu then
          TMainMenu(Parent).RecreateOSMenu;
      end;
      Exit;
    end;
    // If you changed the visibility of top level menu, then re-creates all menu
    IsFirstVisibleItem := False;
    IsTopLevelItem := LMenuItem.Parent is TMainMenu;
    if IsTopLevelItem then
      MainMenu := TMainMenu(LMenuItem.Parent)
    else
      MainMenu := nil;
    if IsTopLevelItem and (TMenuItemChange.Visible in AChange) then
    begin
      MainMenu.RecreateOSMenu;
      Exit;
    end;
    // else update this menu item
    IObj := HandleToObjC(LHandle);
    if IObj <> nil then
    begin
      if IObj.QueryInterface(NSMenuItem, LNSMenuItem) <> S_OK then
      begin
        LNativeMenuItem := IObj as TFMXOSMenuItem;
        LNSMenuItem := LNativeMenuItem.Super as NSMenuItem;
      end;
      // Determine whether the menu item is the first visible and change text
      if IsTopLevelItem then
        for I := 0 to MainMenu.ItemsCount - 1 do
          if (MainMenu.Items[I] is TMenuItem) and TMenuItem(MainMenu.Items[I]).Visible then
          begin
            IsFirstVisibleItem := MainMenu.Items[I] = LMenuItem;
            Break;
          end;
      if (not IsFirstVisibleItem) and ([TMenuItemChange.Text, TMenuItemChange.Bitmap] * AChange <> []) then
      begin
        if LNSMenuItem.submenu <> nil then
          LNSMenuItem.submenu.setTitle(StrToNSStr(DelAmp(LMenuItem.Text)));
        LNSMenuItem.setTitle(StrToNSStr(DelAmp(LMenuItem.Text)));
        SetMenuBitmap(LNSMenuItem, LMenuItem);
      end;
      // Change other params
      if (TMenuItemChange.ShortCut in AChange) then
      begin
        ShortCutToMACKey(LMenuItem.ShortCut, Key, ModMask);
        LNSMenuItem.setKeyEquivalent(StrToNSStr(Key));
        LNSMenuItem.setKeyEquivalentModifierMask(ModMask);
      end;
      if TMenuItemChange.Enabled in AChange then
        LNSMenuItem.setEnabled(LMenuItem.Enabled);
      if TMenuItemChange.Visible in AChange then
        LNSMenuItem.setHidden(not LMenuItem.Visible);
      if TMenuItemChange.Checked in AChange then
      begin
        if LMenuItem.IsChecked then
          LNSMenuItem.setState(NSOnState)
        else
          LNSMenuItem.setState(NSOffState);
      end;
    end
    else
      raise EInvalidFmxHandle.CreateFMT(SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(LHandle) * 2, LHandle]);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.DestroyMenuItem(const AItem: IItemsContainer);
var
  P: TFmxObject;
  Native: INativeControl;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if (not FInDestroyMenuItem) and
        (AItem <> nil) and
       (AItem.GetObject is TFmxObject) then
      P := AItem.GetObject
    else
      Exit;
    FInDestroyMenuItem := true;
    try
      if (P <> nil) and
        (Supports(P, INativeControl, Native)) and
        (Native.Handle <> 0) then
      begin
        if (P.Root <> nil) and (P.Root.GetObject is TCommonCustomForm) then
          CreateOSMenu(TCommonCustomForm(P.Root.GetObject), nil);
        DeleteHandle(Native.Handle);
        Native.Handle := 0;
      end;
    finally
      FInDestroyMenuItem := False;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.ValidateHandle(FmxHandle: TFmxHandle);
begin
  if (FmxHandle and $F <> 0) then
    raise EInvalidFmxHandle.CreateResFmt(@SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(TFmxHandle) * 2, FmxHandle]);
end;

{ Drag and Drop ===============================================================}

procedure TPlatformCocoa.BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
function AdjustOffset(const MousePos: NSPoint; const Width, Height: Single): NSPoint;
  var
    Coord: TPointF;
  begin
    if Data.Source is TControl then
    begin
      // bottom left point of control at form
      Coord := TControl(Data.Source).LocalToAbsolute(TPointF.Create(0, TControl(Data.Source).Height));
      // distance between bottom bound of control and bottom bound of form
      Coord.Y := TNSView.Wrap(WindowHandleToPlatform(AForm.Handle).Wnd.contentView).bounds.size.height - Coord.y;
    end
    else
      Coord := TPointF.Create(MousePos.x, MousePos.y) - TPointF.Create(Width / 2, Height / 2);
    Result := CGPointMake(Coord.X, Coord.Y);
  end;

var
  Img: NSImage;
  Scale: Single;
  Pasteboard: NSPasteboard;
  PboardTypes: NSArray;
  FMXPBoardTypePtrs: array[0..1] of Pointer;
  FMXPBoardTypePtrCount: Integer;
  FMXWin: TFMXWindow;
  AutoReleasePool: NSAutoreleasePool;
  Control: IControl;
  PNGStream: TMemoryStream;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FMXWin := TFMXWindow(WindowHandleToPlatform(AForm.Handle).Handle);
    if FMXWin.LastEvent <> nil then
    begin
      Img := BitmapToMacBitmap(ABitmap);

      Scale := GetWindowScale(AForm);
      if Scale > 1 then
        Img.setSize(CGSizeMake(ABitmap.Width / Scale, ABitmap.Height / Scale));

      Pasteboard := TNSPasteboard.Wrap(TNSPasteboard.OCClass.pasteboardWithName(NSDragPboard));
      FMXPBoardTypePtrs[0] := (NSFMXPBoardType as ILocalObject).GetObjectID;
      FMXPBoardTypePtrCount := 1;
      if Data.Data.IsType<string> then
      begin
        FMXPBoardTypePtrs[FMXPBoardTypePtrCount] := (NSPasteboardTypeString as ILocalObject).GetObjectID;
        Inc(FMXPBoardTypePtrCount);
      end
      else if Data.Data.IsType<TBitmap> or Data.Data.IsType<TBitmapSurface> then
      begin
        FMXPBoardTypePtrs[FMXPBoardTypePtrCount] := (NSPasteboardTypePNG as ILocalObject).GetObjectID;
        Inc(FMXPBoardTypePtrCount);
      end;
      PboardTypes := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@FMXPBoardTypePtrs, FMXPBoardTypePtrCount));

      Pasteboard.declareTypes(PBoardTypes, (Pasteboard as ILocalObject).GetObjectID);
      if FMXPBoardTypePtrCount = 2 then
        if Data.Data.IsType<string> then
          Pasteboard.setString(StrToNSStr(Data.Data.ToString), NSPasteboardTypeString)
        else if Data.Data.IsType<TBitmap> or Data.Data.IsType<TBitmapSurface> then
        begin
          PNGStream := TMemoryStream.Create;
          try
            if Data.Data.IsType<TBitmap> then
              //Saving to stream without converting it to PNG SaveToStream doing it
              Data.Data.AsType<TBitmap>.SaveToStream(PNGStream)
            else
              TBitmapCodecManager.SaveToStream(PNGStream, Data.Data.AsType<TBitmapSurface>, SPNGImageExtension);
            Pasteboard.setData(TNSData.Wrap(TNSData.OCClass.dataWithBytes(PNGStream.Memory, pngStream.size)), NSPasteboardTypePNG);
          finally
            PNGStream.Free;
          end;
        end;
      GlobalData := Data;

      FMXWin.DragOperation := NSDragOperationNone;
      NSWindow(FMXWin.Super).dragImage(Img,
        AdjustOffset(FMXWin.LastEvent.locationInWindow, Img.size.width, Img.size.height),
        CGSizeMake(0, 0), FMXWin.LastEvent, Pasteboard, (FMXWin.View as ILocalObject).GetObjectID, True);
      if (FMXWin.DragOperation = NSDragOperationNone) and Supports(Data.Source, IControl, Control) then
        Control.DragEnd;

      FMXWin.LastEvent := nil;
    end;
  finally
    if Img <> nil then
      Img.release;
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
var
  NSWin: NSWindow;
  OldFrame, Frame, R: NSRect;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if NSWin.isVisible then
    begin
      OldFrame := NSWin.frame;
      R.origin := OldFrame.origin;
      R.size := NSSize(ASize);
      Frame := NSWin.frameRectForContentRect(R);
      Frame.origin.x := OldFrame.origin.x;
      Frame.origin.y := OldFrame.origin.y + OldFrame.size.height - Frame.size.height;
      NSWin.setFrame(Frame, True);
    end
    else
      NSWin.setContentSize(NSSize(ASize));
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.SetCursor(const ACursor: TCursor);
const
  SizeNWSECursor: array [0..192] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $10, $00, $00, $00, $10, $08, $06, $00, $00, $00, $1F, $F3, $FF, $61, $00, $00, $00, $88, $49, $44, $41,
    $54, $78, $9C, $AC, $93, $4B, $0A, $C0, $20, $0C, $44, $45, $8A, $69, $D7, $5D, $7B, $00, $0F, $98, $EB, $6B, $15, $8C, $44, $F1, $1B, $3A, $20, $BA, $D0, $E7, $4C, $A2, $4A, $FD, $A1, $30, $D1, $36,
    $20, $4D, $69, $00, $40, $59, $8B, $00, $FC, $B0, $08, $60, $8C, $A9, $6E, $BF, $A2, $44, $0E, $08, $82, $88, $EA, $8D, $DA, $02, $78, $EF, $43, $0B, $63, $31, $EE, $29, $80, $67, $26, $88, $D6, $BA,
    $82, $58, $6B, $97, $69, $CA, $A6, $91, $93, $AD, $16, $3F, $51, $23, $48, $8A, $D9, $44, $EB, $8B, $AA, $3F, $2B, $F0, $3A, $4F, $16, $41, $A8, $C5, $47, $00, $96, $F7, $DC, $81, $73, $AE, $FB, $C8,
    $44, $0E, $C4, $1F, $6D, $A5, $0F, $00, $00, $FF, $FF, $03, $00, $FD, $DF, $FC, $72, $CD, $04, $2F, $27, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );
  SizeNESWCursor: array [0..211] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $10, $00, $00, $00, $10, $08, $06, $00, $00, $00, $1F, $F3, $FF, $61, $00, $00, $00, $9B, $49, $44, $41,
    $54, $78, $9C, $9C, $93, $51, $0E, $C0, $10, $0C, $86, $3D, $88, $CC, $F3, $0E, $E3, $2A, $2E, $E2, $04, $6E, $E0, $C5, $5D, $DC, $4D, $4C, $93, $CD, $1A, $46, $AD, $7F, $D2, $14, $49, $3F, $D5, $96,
    $10, $0B, $95, $52, $48, $23, $55, $D6, $DA, $03, $80, $EB, $ED, $17, $20, $E7, $CC, $06, $1C, $29, $A5, $96, $85, $52, $AA, $79, $12, $A0, $AB, $62, $8C, $BC, $27, $9C, $55, $21, $84, $21, $18, $45,
    $CD, $01, $52, $4A, $E1, $9C, $FB, $0C, $F6, $DE, $F7, $5D, $79, $0B, $85, $4F, $26, $37, $C3, $42, $0E, $33, $70, $6F, $86, $14, $B7, $AB, $8D, $01, $5F, $85, $32, $C6, $C0, $42, $93, $00, $DC, $A2,
    $27, $D8, $5A, $0B, $DD, $58, $8F, $EC, $2C, $03, $18, $1E, $54, $13, $FE, $13, $B6, $01, $33, $ED, $02, $78, $5F, $B5, $EA, $02, $00, $00, $FF, $FF, $03, $00, $27, $CE, $7B, $C4, $F5, $A4, $B6, $D6,
    $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );
  SizeAllCursor: array [0..174] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $10, $00, $00, $00, $10, $08, $06, $00, $00, $00, $1F, $F3, $FF, $61, $00, $00, $00, $09, $70, $48, $59,
    $73, $00, $00, $0B, $13, $00, $00, $0B, $13, $01, $00, $9A, $9C, $18, $00, $00, $00, $61, $49, $44, $41, $54, $78, $9C, $AC, $53, $CB, $0A, $00, $20, $0C, $1A, $F4, $FF, $DF, $6C, $10, $74, $68, $0F,
    $17, $65, $E0, $A9, $74, $BA, $36, $03, $60, $04, $FB, $94, $6F, $28, $D9, $6C, $2C, $30, $91, $96, $DC, $89, $5C, $91, $99, $48, $95, $19, $49, $84, $E3, $2A, $13, $F0, $55, $B2, $CA, $C1, $49, $D5,
    $B0, $D2, $81, $17, $A5, $99, $3B, $04, $AB, $AF, $02, $DF, $11, $24, $4D, $94, $7C, $A3, $64, $90, $24, $A3, $2C, $59, $A6, $EB, $75, $9E, $00, $00, $00, $FF, $FF, $03, $00, $3A, $00, $A6, $5B, $CC,
    $0B, $A4, $58, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );
  WaitCursor: array [0..124] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $10, $00, $00, $00, $10, $08, $06, $00, $00, $00, $1F, $F3, $FF, $61, $00, $00, $00, $44, $49, $44, $41,
    $54, $78, $9C, $62, $60, $C0, $0E, $FE, $E3, $C0, $44, $83, $21, $6E, $C0, $7F, $5C, $80, $18, $43, $70, $6A, $26, $D6, $10, $BA, $19, $80, $D3, $10, $6C, $0A, $C9, $33, $00, $59, $03, $45, $5E, $C0,
    $65, $00, $94, $4D, $5A, $38, $10, $B2, $1D, $C5, $10, $1C, $98, $68, $30, $84, $0C, $00, $00, $00, $00, $FF, $FF, $03, $00, $A9, $31, $25, $E9, $C0, $2C, $FB, $9B, $00, $00, $00, $00, $49, $45, $4E,
    $44, $AE, $42, $60, $82
  );
  HelpCursor: array [0..238] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $12, $00, $00, $00, $12, $08, $06, $00, $00, $00, $56, $CE, $8E, $57, $00, $00, $00, $B6, $49, $44, $41,
    $54, $78, $9C, $A4, $94, $3B, $12, $80, $20, $0C, $44, $69, $6C, $6D, $6C, $BC, $83, $8D, $B5, $F7, $E0, $FE, $37, $01, $89, $93, $8C, $61, $F9, $18, $21, $33, $19, $15, $C9, $73, $B3, $46, $9D, $83,
    $88, $31, $52, $36, $03, $F7, $17, $C5, $1A, $E2, $BD, $0F, $74, $89, $49, $EB, $9F, $30, $06, $05, $81, $70, $51, $D0, $6B, $66, $18, $15, $49, $01, $9F, $9F, $29, $77, $BD, $CE, $F7, $E8, $B8, $98,
    $40, $1A, $D6, $00, $ED, $05, $4C, $79, $94, $B5, $C1, $80, $0B, $40, $D2, $1A, $A9, $5D, $BB, $AA, $30, $1B, $1E, $5D, $29, $B7, $AE, $57, $FC, $A4, $23, $ED, $CF, $D4, $00, $A4, $AF, $08, $D5, $C1,
    $5B, $FC, $0F, $11, $D0, $34, $44, $83, $A6, $20, $4E, $08, $EF, $A7, $61, $32, $B7, $0A, $A9, $F8, $53, $CE, $8E, $05, $E4, $CA, $21, $1C, $F2, $A7, $A6, $68, $BC, $3D, $F0, $28, $53, $64, $F9, $11,
    $48, $3C, $83, $59, $83, $FC, $8D, $85, $8B, $B7, $2F, $C8, $0D, $00, $00, $FF, $FF, $03, $00, $A5, $D1, $28, $C9, $B0, $25, $E3, $01, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );
var
  C: NSCursor;
  AutoReleasePool: NSAutoreleasePool;
  NewCustomCursor: TCustomCursor;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NewCustomCursor := nil;
    case ACursor of
      crCross: C := TNSCursor.Wrap(TNSCursor.OCClass.crosshairCursor);
      crArrow, crDefault: C := TNSCursor.Wrap(TNSCursor.OCClass.arrowCursor);
      crIBeam: C := TNSCursor.Wrap(TNSCursor.OCClass.IBeamCursor);
      crSizeNS: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeUpDownCursor);
      crSizeWE: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeLeftRightCursor);
      crUpArrow: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeUpCursor);
      crDrag, crMultiDrag:  C := TNSCursor.Wrap(TNSCursor.OCClass.dragCopyCursor);
      crHSplit: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeLeftRightCursor);
      crVSplit: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeUpDownCursor);
      crNoDrop, crNo: C := TNSCursor.Wrap(TNSCursor.OCClass.operationNotAllowedCursor);
      crHandPoint: C := TNSCursor.Wrap(TNSCursor.OCClass.pointingHandCursor);
      crAppStart, crSQLWait, crHourGlass: NewCustomCursor := TCustomCursor.Create(@WaitCursor[0], Length(WaitCursor));
      crHelp:  NewCustomCursor := TCustomCursor.Create(@HelpCursor[0], Length(HelpCursor));
      crSizeNWSE: NewCustomCursor := TCustomCursor.Create(@SizeNWSECursor[0], Length(SizeNWSECursor));
      crSizeNESW: NewCustomCursor := TCustomCursor.Create(@SizeNESWCursor[0], Length(SizeNESWCursor));
      crSizeAll: NewCustomCursor := TCustomCursor.Create(@SizeAllCursor[0], Length(SizeAllCursor));
    else
      C := TNSCursor.Wrap(TNSCursor.OCClass.arrowCursor);
    end;
    if ACursor = crNone then
      TNSCursor.OCClass.setHiddenUntilMouseMoves(True)
    else
    begin
      TNSCursor.OCClass.setHiddenUntilMouseMoves(False);
      // Remove old custom cursor
      if FCustomCursor <> nil then
        FreeAndNil(FCustomCursor);
      // Set new custom cursor
      if NewCustomCursor <> nil then
      begin
        FCustomCursor := NewCustomCursor;
        C := FCustomCursor.Cursor;
      end;
      C.&set;
    end;
    FCursor := ACursor;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetCursor: TCursor;
begin
  Result := FCursor;
end;

procedure TPlatformCocoa.SetFullScreen(const AForm: TCommonCustomForm;
  const AValue: Boolean);
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  if AValue and not (TFmxFormState.Showing in AForm.FormState) then
    AForm.Visible := True;
  if AForm.Visible or (TFmxFormState.Showing in AForm.FormState) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
      NSWin.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary);
      try
        if not (AValue = ((NSWin.styleMask and NSFullScreenWindowMask) > 0)) then
          NSWin.toggleFullScreen(nil);
      finally
        SetShowFullScreenIcon(AForm, AForm.ShowFullScreenIcon);
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

{ Mouse  ===============================================================}

function TPlatformCocoa.GetMousePos: TPointF;
var
  P: NSPoint;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    P := TNSEvent.OCClass.mouseLocation;
    Result := TPointF.Create(P.x, P.y);
    Result.y := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size.height - Result.y;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetScreenSize: TPointF;
begin
  Result := TPointF(TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size)
end;

function TPlatformCocoa.GetScreenScale: Single;
begin
  Result := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).backingScaleFactor;
end;

function TPlatformCocoa.GetScreenOrientation: TScreenOrientation;
begin
  Result := TScreenOrientation.Landscape;
end;

{ IFMXSystemInformationService }

function TPlatformCocoa.GetScrollingBehaviour: TScrollingBehaviours;
begin
  Result := [TScrollingBehaviour.BoundsAnimation, TScrollingBehaviour.AutoShowing];
end;

function TPlatformCocoa.GetMinScrollThumbSize: Single;
begin
  Result := 25;
end;

{ IFMXScreenService }

{ International ===============================================================}

function TPlatformCocoa.GetCurrentLangID: string;
begin
  Result := UTF8ToString(TNSLocale.Wrap(TNSLocale.OCClass.currentLocale).localeIdentifier.UTF8String);
  if Length(Result) > 2 then
    Delete(Result, 3, MaxInt);
end;

function TPlatformCocoa.GetDefaultFontFamilyName: String;
begin
  if FDefaultFontName.IsEmpty then
    FDefaultFontName := NSStrToStr(TNSFont.Wrap(TNSFont.OCClass.systemFontOfSize(DefaultMacFontSize)).fontName);
  Result := FDefaultFontName;
end;

function TPlatformCocoa.GetDefaultFontSize: Single;
begin
  Result := DefaultMacFontSize;
end;

function TPlatformCocoa.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  case AComponent of
    TComponentKind.Calendar: Result := TSize.Create(230, 204);
  else
    Result := TSize.Create(80, 22);
  end;
end;

function TPlatformCocoa.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  Calendar: NSCalendar;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Calendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
    // On the OSX Zero index ñorresponds Sunday, so we need to add offset. Because in RTL DayMonday = 1
    Result := Calendar.firstWeekday - MondayOffset;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    Result := (NSWin.styleMask and NSFullScreenWindowMask) > 0;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetLocaleFirstDayOfWeek: string;
var
  cal: NSCalendar;
  firstDay: NSUInteger;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    cal:= TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
    firstDay:= Cal.firstWeekday;
    Result:= IntToStr(firstDay);
  finally
    AutoReleasePool.release;
  end;
end;

{ Dialogs ===============================================================}

function TPlatformCocoa.DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
var
  OpenFile: NSOpenPanel;
  DefaultExt: string;
  Filter: NSArray;
  outcome: NSInteger;
  I: Integer;
  AutoReleasePool: NSAutoreleasePool;

  function AllocFilterStr(const S: string; var Filter: NSArray): Boolean;
  var
    input, pattern: string;
    FileTypes: array of string;
    outcome, aux: TArray<string>;
    i, j: Integer;
    FileTypesNS: array of Pointer;
    NStr: NSString;
    LocObj: ILocalObject;
  begin
    // First, split the string by using '|' as a separator
    Result := False;
    input := S;
    pattern := '\|';

    outcome := TRegEx.Split(input, pattern);

    pattern := '\*\.';
    SetLength(FileTypes, 0);

    for i := 0 to length(outcome) - 1 do
    begin
      if Odd(i) then
        if outcome[i] <> '*.*' then
          if AnsiLeftStr(outcome[i], 2) = '*.' then
          begin
            aux := TRegEx.Split(outcome[i], pattern);
            for j := 0 to length(aux) - 1 do
            begin
              aux[j] := Trim(aux[j]);
              if aux[j] <> '' then
              begin
                if AnsiEndsStr(';', aux[j]) then
                  aux[j] := AnsiLeftStr(aux[j], length(aux[j]) - 1);
                SetLength(FileTypes, length(FileTypes) + 1);
                FileTypes[length(FileTypes) - 1] := aux[j];
              end;
            end;
          end;
    end;

    // create the NSArray from the FileTypes array
    SetLength(FileTypesNS, length(FileTypes));
    for i := 0 to length(FileTypes) - 1 do
    begin
      NStr := StrToNSStr(FileTypes[i]);
      if Supports(NStr, ILocalObject, LocObj) then
        FileTypesNS[i] := LocObj.GetObjectID;
    end;
    if length(FileTypes) > 0 then begin
      Filter := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@FileTypesNS[0], length(FileTypes)));
      Result := True;
    end;
  end;

begin
  Result := False;
  DefaultExt := ADialog.DefaultExt;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    OpenFile := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);

    OpenFile.setAllowsMultipleSelection(TOpenOption.ofAllowMultiSelect in ADialog.Options);
    OpenFile.setCanChooseFiles(AType <> TDialogType.Directory);
    OpenFile.setCanChooseDirectories(AType = TDialogType.Directory);

    AFiles.Clear;

    if ADialog.InitialDir <> '' then
      OpenFile.setDirectoryURL(TNSUrl.Wrap(TNSUrl.OCCLass.fileURLWithPath(StrToNSStr(ADialog.InitialDir))));

    if ADialog.FileName <> '' then
      OpenFile.setNameFieldStringValue(StrToNSStr(ADialog.FileName));

    if ADialog.Filter <> '' then
    begin
      if AllocFilterStr(ADialog.Filter, Filter) then
      begin
        OpenFile.setAllowedFileTypes(Filter);
        OpenFile.setAllowsOtherFileTypes(False);
      end;
    end;

    if ADialog.Title <> '' then
      OpenFile.setTitle(StrToNSStr(ADialog.Title));

    OpenFile.retain;
    try
      outcome := OpenFile.runModal;
      if (FModalStack <> nil) and (FModalStack.Count > 0) then
        FRestartModal := True;
      if outcome = NSOKButton then
      begin
        for I := 0 to OpenFile.URLs.count - 1 do
          AFiles.Add(UTF8ToString(TNSUrl.Wrap(OpenFile.URLs.objectAtIndex(I)).relativePath.UTF8String));

        if AFiles.Count > 0 then
          ADialog.FileName := AFiles[0];

        if DefaultExt <> '' then
          if ExtractFileExt(ADialog.FileName) = '' then
            ChangeFileExt(ADialog.FileName, DefaultExt);
        Result := True;
      end;
    finally
      OpenFile.release;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
var
  printPanel: NSPrintPanel;
  printInfo: NSPrintInfo;
  outcome : NSInteger;
  dict: NSMutableDictionary;
  AutoReleasePool: NSAutoreleasePool;
begin
  Result := False;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    printInfo := TNSPrintInfo.Wrap(TNSPrintInfo.OCClass.sharedPrintInfo);
    printPanel := TNSPrintPanel.Wrap(TNSPrintPanel.OCClass.printPanel);
    dict := printInfo.dictionary;

    dict.setValue(TNSNumber.OCClass.numberWithBool(ACollate), NSPrintMustCollate);
    dict.setValue(TNSNumber.OCClass.numberWithInt(AFromPage), NSPrintFirstpage);
    dict.setValue(TNSNumber.OCClass.numberWithInt(AToPage), NSPrintLastPage);
    dict.setValue(TNSNumber.OCClass.numberWithInt(ACopies), NSPrintCopies);
    if APrintrange = TPrintRange.prAllPages then
      dict.setValue(TNSNumber.OCClass.numberWithBool(True), NSPrintAllPages);
    if TPrintDialogOption.poPrintToFile in AOptions then
      printInfo.setJobDisposition(NSPrintSaveJob);

    printPanel.retain;
    try
      outcome := printPanel.runModalWithPrintInfo(printInfo);
      if outcome = NSOKButton then
      begin
        ACollate := TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintMustCollate)).boolValue();
        ACopies := TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintCopies)).integerValue();
        if printInfo.jobDisposition = NSPrintSaveJob then
          APrintToFile := True;
        if TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintAllPages)).boolValue() = True then
          APrintRange := TPrintRange.prAllPages
        else
        begin
          APrintRange := TPrintRange.prPageNums;
          AFromPage := TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintFirstpage)).integerValue();
          AToPage := TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintLastPage)).integerValue();
        end;
        Result := True;
      end;
    finally
      printPanel.release;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF; AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformCocoa.DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
  var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
const
  POINTS_PER_INCHES = 72;
  MM_PER_INCH = 25.4;
var
  pageSetup: NSPageLayout;
  printInfo: NSPrintInfo;
  outcome: Integer;
  newSize: TPointF;
  AutoReleasePool: NSAutoreleasePool;

  function ToPoints(Value: Single): Single;
  begin
    Result := Value /1000;
    Result := Result * POINTS_PER_INCHES;
    if AUnits = TPageMeasureUnits.pmMillimeters then
    begin
      Result := Result / MM_PER_INCH;
    end;
  end;

  function FromPoints(Value: Single): Single;
  begin
    Result := Value * 1000;
    Result := Result / POINTS_PER_INCHES;
    if AUnits = TPageMeasureUnits.pmMillimeters then
    begin
      Result := Result * MM_PER_INCH;
    end;
  end;

begin
  Result := False;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    printInfo := TNSPrintInfo.Wrap(TNSPrintInfo.OCClass.sharedPrintInfo);
    pageSetup := TNSPageLayout.Wrap(TNSPageLayout.OCClass.pageLayout);

    //Calculate paper size for MAC side
    newSize.X := ToPoints(APaperSize.X);
    newSize.y := ToPoints(APaperSize.y);
    printInfo.setPaperSize(NSSize(newSize));

    //If psoMargins is set, use the margins specified by the user,
    //else, let the panel use the defaults.
    if TPageSetupDialogOption.psoMargins in AOptions then
    begin
      printInfo.setLeftMargin(ToPoints(AMargin.Left));
      printInfo.setTopMargin(ToPoints(AMargin.Top));
      printInfo.setRightMargin(ToPoints(AMargin.Right));
      printInfo.setBottomMargin(ToPoints(AMargin.Bottom));
    end;

    printInfo.setHorizontallyCentered(False);
    printInfo.setVerticallyCentered(False);

    pageSetup.retain;
    try
      outcome := pageSetup.runModalWithPrintInfo(printInfo);
      if outcome = NSOKButton then
      begin
        APaperSize := TPointF(printInfo.paperSize);
        //transfrom from points into inches
        APaperSize.X := FromPoints(APaperSize.X);
        APaperSize.y := FromPoints(APaperSize.Y);

        // Set the margins to the values from the dialog.
        AMargin.Left := round(FromPoints(printInfo.LeftMargin));
        AMargin.Top := round(FromPoints(printInfo.TopMargin));
        AMargin.Right := round(FromPoints(printInfo.RightMargin));
        AMargin.Bottom := round(FromPoints(printInfo.BottomMargin));

        //if psoMinMargins is set in options, then adjust the margins to fit
        if TPageSetupDialogOption.psoMinMargins in AOptions then
        begin
          if AMargin.Left < AMinMargin.Left then
            AMargin.Left := AMinMargin.Left;
          if AMargin.Top < AMinMargin.Top then
            AMargin.Top := AMinMargin.Top;
          if AMargin.Right < AMinMargin.Right then
            AMargin.Right := AMinMargin.Right;
          if AMargin.Bottom < AMinMargin.Bottom then
            AMargin.Bottom := AMinMargin.Bottom;
        end;
        Result := True;
      end;
    finally
      pageSetup.release;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.DialogPrinterSetup: Boolean;
begin
  Result := False;
end;

function TPlatformCocoa.DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
var
  SaveFile: NSSavePanel;
  DefaultExt: string;
  Filter: NSArray;
  outcome : NSInteger;
  FileName: TFileName;
  AutoReleasePool: NSAutoreleasePool;

  function AllocFilterStr(const S: string; var Filter: NSArray): Boolean;
  var
    input, pattern: string;
    FileTypes: array of string;
    outcome, aux: TArray<string>;
    i, j: Integer;
    FileTypesNS: array of Pointer;
    NStr: NSString;
    LocObj: ILocalObject;
  begin
    Result := False;
    // First, split the string by using '|' as a separator
    input := S;
    pattern := '\|';

    outcome := TRegEx.Split(input, pattern);

    pattern := '\*\.';
    SetLength(FileTypes, 0);

    for i := 0 to length(outcome) - 1 do
    begin
      if Odd(i) then
        if outcome[i] <> '*.*' then
          if AnsiLeftStr(outcome[i], 2) = '*.' then
          begin
            // Split the string by using '*.' as a separator
            aux := TRegEx.Split(outcome[i], pattern);
            for j := 0 to length(aux) - 1 do
            begin
              aux[j] := Trim(aux[j]);
              if aux[j] <> '' then
              begin
                //Remove the ';' if necessary
                if AnsiEndsStr(';', aux[j]) then
                  aux[j] := AnsiLeftStr(aux[j], length(aux[j]) - 1);
                SetLength(FileTypes, length(FileTypes) + 1);
                FileTypes[length(FileTypes) - 1] := aux[j];
              end;
            end;
          end;
    end;

    // create the NSArray from the FileTypes array
    SetLength(FileTypesNS, Length(FileTypes));
    for i := 0 to Length(FileTypes) - 1 do
    begin
      NStr := StrToNSStr(FileTypes[i]);
      if Supports(NStr, ILocalObject, LocObj) then
        FileTypesNS[i] := LocObj.GetObjectID;
    end;
    if length(FileTypes) > 0 then begin
      Filter := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@FileTypesNS[0], Length(FileTypes)));
      Result := True;
    end;
  end;

begin
  Result := False;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    SaveFile := TNSSavePanel.Wrap(TNSSavePanel.OCClass.savePanel);

    if ADialog.InitialDir <> '' then
      SaveFile.setDirectoryURL(TNSUrl.Wrap(TNSUrl.OCCLass.fileURLWithPath(StrToNSStr(ADialog.InitialDir))));

    if ADialog.FileName <> '' then
      SaveFile.setNameFieldStringValue(StrToNSStr(ADialog.FileName));

    if ADialog.Filter <> '' then
    begin
      if AllocFilterStr(ADialog.Filter, Filter) then
        SaveFile.setAllowedFileTypes(Filter);
    end;

    if ADialog.Title <> '' then
      SaveFile.setTitle(StrToNSStr(ADialog.Title));

    SaveFile.retain;
    try
      outcome := SaveFile.runModal;
      if (FModalStack <> nil) and (FModalStack.Count > 0) then
        FRestartModal := True;
      if outcome = NSOKButton then
      begin
        FileName := UTF8ToString(SaveFile.URL.relativePath.UTF8String);
        if DefaultExt <> '' then
          if ExtractFileExt(FileName) = '' then
            ChangeFileExt(FileName, DefaultExt);
        ADialog.FileName := Filename;
        Result := True;
      end;
    finally
      SaveFile.release;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.DeleteHandle(FmxHandle: TFmxHandle);
var IObj: IObjectiveC;
    Item: NSMenuItem;
    Menu: NSMenu;
    AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    ValidateHandle(FmxHandle);
    TMonitor.Enter(FObjectiveCMap);
    try
      if FObjectiveCMap.TryGetValue(FmxHandle, IObj) then
      begin
        if IObj.QueryInterface(NSMenuItem, Item) = 0 then
        begin
          Item.release;
          Item := nil;
        end;
        if IObj.QueryInterface(NSMenu, Menu) = 0 then
        begin
          Menu.release;
          Menu := nil;
        end;
        FObjectiveCMap.Remove(FmxHandle);
        IObj := nil;
      end;
    finally
      TMonitor.Exit(FObjectiveCMap);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.Log(const Fmt: string; const Params: array of const);
begin
  WriteLn(Format(Fmt, Params));
end;

function TPlatformCocoa.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [];
end;

function TPlatformCocoa.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop];
end;

function TPlatformCocoa.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [TListingTransitionFeature.EditMode, TListingTransitionFeature.DeleteButtonSlide,
    TListingTransitionFeature.PullToRefresh];
end;

function TPlatformCocoa.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [];
end;

function TPlatformCocoa.GetSaveStateFileName(const ABlockName: string): string;
const
  Separator = '_';
var
  S: TStringBuilder;
  FilePath: string;
begin
  if FSaveStateStoragePath.IsEmpty then
    FilePath := TPath.GetTempPath
  else
    FilePath := FSaveStateStoragePath;
  S := TStringBuilder.Create(FilePath.Length + Length(Separator) + ABlockName.Length);
  try
    S.Append(FilePath);
    S.Append(ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
    S.Append(Separator);
    S.Append(ABlockName);
    Result := S.ToString;
  finally
    S.Free;
  end;
end;

function TPlatformCocoa.GetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

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
  LFileName: string;
begin
  if ABlockName.IsEmpty or (ABlockData = nil) then
    Exit(False);
  LFileName := GetSaveStateFileName(ABlockName);
  if not TFile.Exists(LFileName) then
    Exit(False);
  try
    ReadPersistent(LFileName);
  except
    Exit(False);
  end;
  Result := True;
end;

function TPlatformCocoa.SetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

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
  LFileName: string;
begin
  if ABlockName.IsEmpty then
    Exit(False);
  LFileName := GetSaveStateFileName(ABlockName);
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
  Result := True;
end;

function TPlatformCocoa.GetSaveStateStoragePath: string;
begin
  Result := FSaveStateStoragePath;
end;

procedure TPlatformCocoa.SetSaveStateStoragePath(const ANewPath: string);
begin
  if not ANewPath.IsEmpty then
    FSaveStateStoragePath := IncludeTrailingPathDelimiter(ANewPath)
  else
    FSaveStateStoragePath := '';
end;

function TPlatformCocoa.GetSaveStateNotifications: Boolean;
begin
  Result := False;
end;

{ TMacWindowHandle }

function WindowHandleToPlatform(const AHandle: TWindowHandle): TMacWindowHandle;
begin
  Result := TMacWindowHandle(AHandle);
end;

constructor TMacWindowHandle.Create(const AHandle: TOCLocal);
var
  AutoReleasePool: NSAutoreleasePool;
  boundRect: NSRect;
  Options: NSTrackingAreaOptions;
begin
  inherited Create;
  FHandle := AHandle;
  if IsPopup(Form) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      boundRect := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame;
      Options := NSTrackingMouseMoved or NSTrackingActiveAlways or NSTrackingAssumeInside;
      FTrackingArea := TNSTrackingArea.Wrap(TNSTrackingArea.Alloc.initWithRect(boundRect, Options,
        View.superview, nil));
      View.addTrackingArea(FTrackingArea);
    finally
      AutoReleasePool.release;
    end;
  end;
  if FWindowHandles = nil then
    FWindowHandles := TList<TMacWindowHandle>.Create;
  FWindowHandles.Add(Self);
end;

destructor TMacWindowHandle.Destroy;
begin
  if FTrackingArea <> nil then
  begin
    View.removeTrackingArea(FTrackingArea);
    FTrackingArea.release;
    FTrackingArea := nil;
  end;
  FWindowHandles.Remove(Self);
  if FWindowHandles.Count = 0 then
    FreeAndNil(FWindowHandles);
  FreeBuffer;
  TFMXWindow(FHandle).DisposeOf;
  inherited;
end;

function TMacWindowHandle.GetForm: TCommonCustomForm;
begin
  Result := TFMXWindow(FHandle).Wnd;
end;

function TMacWindowHandle.GetGLView: NSOpenGLView;
begin
  Result := NSOpenGLView(TFMXWindow(FHandle).View);
end;

function TMacWindowHandle.GetView: NSView;
begin
  Result := TFMXWindow(FHandle).View;
end;

function TMacWindowHandle.GetWindow: NSWindow;
var
  LSuper: IObjectiveCInstance;
  LNSWindow: NSWindow;
begin
  LSuper := TFMXWindow(FHandle).Super;
  if LSuper.QueryInterface(NSWindow, LNSWindow) = S_OK then
    Result := LNSWindow
  else
    Result := nil;
end;

function TMacWindowHandle.GetScale: Single;
begin
  Result := Wnd.backingScaleFactor
end;

procedure TMacWindowHandle.CreateBuffer;
var
  ColorSpace: CGColorSpaceRef;
begin
  FBufferSize := TSize.Create(Form.Width, Form.Height);
  GetMem(FBits, FBufferSize.Width * FBufferSize.Height * 4);
  ColorSpace := CGColorSpaceCreateDeviceRGB;
  FBuffer := CGBitmapContextCreate(FBits, FBufferSize.Width, FBufferSize.Height, 8, FBufferSize.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
  CGColorSpaceRelease(ColorSpace);
end;

procedure TMacWindowHandle.FreeBuffer;
begin
  if FBits <> nil then
  begin
    CGContextRelease(FBuffer);
    FreeMem(FBits);
    FBits := nil;
    FBufferSize := TSize.Create(0, 0);
  end;
end;

procedure TMacWindowHandle.UpdateLayer(const Ctx: CGContextRef);
var
  Img: CGImageRef;
  R: CGRect;
  ContextObject: IContextObject;
begin
  if Supports(Form, IContextObject, ContextObject) and (ContextObject.Context <> nil) then
  begin
    if FBits = nil then
      CreateBuffer
    else if (FBufferSize.Width <> Form.Width) or (FBufferSize.Height <> Form.Height) then
    begin
      FreeBuffer;
      CreateBuffer;
    end;
    { Copy from Context }
    ContextObject.Context.CopyToBits(FBits, FBufferSize.Width * 4, Rect(0, 0, FBufferSize.Width, FBufferSize.Height));
    { Draw }
    R.origin.x := 0;
    R.origin.y := 0;
    R.size.width := FBufferSize.Width;
    R.size.height := FBufferSize.Height;
    CGContextClearRect(Ctx, R);
    Img := CGBitmapContextCreateImage(FBuffer);
    CGContextDrawImage(Ctx, R, Img);
    CGImageRelease(Img);
  end;
end;

class function TMacWindowHandle.FindForm(const window: NSWindow): TCommonCustomForm;
var
  I: Integer;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Result := nil;
    if FWindowHandles <> nil then
      for I := 0 to FWindowHandles.Count - 1 do
      begin
        if FWindowHandles[I].Wnd.windowRef = window.windowRef then
        begin
          Result := FWindowHandles[I].Form;
          Break;
        end;
      end;
  finally
    AutoReleasePool.release;
  end;
end;


{ TCursorInfo }

constructor TCustomCursor.Create(const ABytes: Pointer; const ALength: NSUInteger);
begin
  FData := TNSData.Wrap(TNSData.Alloc.initWithBytes(ABytes, ALength));
  FImage := TNSImage.Wrap(TNSImage.Alloc.initWithData(FData));
  FCursor := TNSCursor.Wrap(TNSCursor.Alloc.initWithImage(FImage, NSPoint(PointF(10, 10))));
end;

destructor TCustomCursor.Destroy;
begin
  FCursor.release;
  FImage.release;
  FData.release;
end;

{ TMultiDisplayMac }

destructor TMultiDisplayMac.Destroy;
begin
  FDisplayList.Free;
  inherited;
end;

procedure TMultiDisplayMac.UpdateDisplayInformation;
begin
  FDisplayCount := 0;
  FDesktopRect := TRect.Empty;
  FWorkAreaRect := TRect.Empty;
  FreeAndNil(FDisplayList);
end;

function TMultiDisplayMac.GetDisplayCount: Integer;
begin
  if FDisplayCount = 0 then
    FDisplayCount := TNSScreen.OCClass.screens.count;
  Result := FDisplayCount;
end;

function TMultiDisplayMac.GetDesktopCenterRect(const Size: TSize): TRect;
var
  I, MinI: Integer;
  DesktopCenter: TPoint;
  Dist, MinDist: Double;
  WorkArea: TRect;
begin
  DesktopCenter := GetDesktopRect.CenterPoint;
  Result := TRect.Create(TPoint.Create(DesktopCenter.X - Size.cx div 2, DesktopCenter.Y - Size.cy div 2), Size.cx,
    Size.cy);
  MinDist := MaxInt;
  MinI := -1;

  for I := 0 to GetDisplayCount - 1 do
  begin
    Dist := GetDisplay(I).WorkArea.CenterPoint.Distance(DesktopCenter);
    if Dist < MinDist then
    begin
      MinDist := Dist;
      MinI := I;
    end;
  end;
  WorkArea := GetDisplay(MinI).WorkArea;
  if Result.Top < WorkArea.Top then
    Result.SetLocation(Result.Left, WorkArea.Top);
  if Result.Bottom > WorkArea.Bottom then
    Result.SetLocation(Result.Left, WorkArea.Bottom - Result.Height);
  if Result.Left < WorkArea.Left then
    Result.SetLocation(WorkArea.Left, Result.Top);
  if Result.Right > WorkArea.Right then
    Result.SetLocation(WorkArea.Right - Result.Width, Result.Top);
end;

function TMultiDisplayMac.GetDesktopRect: TRect;
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

function TMultiDisplayMac.NSRectToRect(const ANSRect: NSRect): TRect;
var
  LNSSize: NSSize;
begin
  LNSSize := TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).frame.size;
  Result := TRect.Create(TPoint.Create(Round(ANSRect.origin.x),
    Round(LNSSize.height - ANSRect.origin.y - ANSRect.size.height)), Round(ANSRect.size.width),
    Round(ANSRect.size.height));
end;

procedure TMultiDisplayMac.UpdateDisplays;
var
  I: Integer;
  AutoReleasePool: NSAutoreleasePool;
  LNSScreen: NSScreen;
begin
  UpdateDisplayInformation;
  FDisplayCount := TNSScreen.OCClass.screens.count;
  if FDisplayList = nil then
    FDisplayList := TList<TDisplay>.Create
  else
    FDisplayList.Clear;
  for I := 0 to FDisplayCount - 1 do
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      LNSScreen := TNSScreen.Wrap(TNSScreen.OCClass.screens.objectAtIndex(I));
      FDisplayList.Add(TDisplay.Create(I, I = 0, NSRectToRect(LNSScreen.frame), NSRectToRect(LNSScreen.visibleFrame)));
    finally
      AutoReleasePool.release;
    end;
  end;
end;

function TMultiDisplayMac.FindDisplay(const screen: NSScreen): TDisplay;
  function DoFind(const R: TRect): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    if FDisplayList <> nil then
      for I := 0 to FDisplayList.Count - 1 do
        if R = FDisplayList[I].BoundsRect then
        begin
          Result := I;
          Exit;
        end;
  end;
var
  Index: Integer;
  R: TRect;
begin
  if screen = nil then
    raise EInvalidFmxHandle.Create(sArgumentInvalid);
  R := NSRectToRect(screen.Frame);
  Index := DoFind(R);
  if Index = -1 then
  begin
    UpdateDisplays;
    Index := DoFind(R);
  end;
  if Index = -1 then
    raise EInvalidArgument.Create(sArgumentInvalid)
  else
    Result := FDisplayList[Index];
end;

function TMultiDisplayMac.DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
var
  Wnd: TMacWindowHandle;
  Form, ParentForm: TCommonCustomForm;
begin
  if Handle = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Wnd := WindowHandleToPlatform(Handle);
  if Wnd.Wnd <> nil then
    Form := Wnd.FindForm(Wnd.Wnd)
  else
    Form := nil;
  if IsPopupForm(Form) then
  begin
    ParentForm := Form.ParentForm;
    while IsPopupForm(ParentForm) do
      ParentForm := ParentForm.ParentForm;
    if ParentForm <> nil then
      Wnd := WindowHandleToPlatform(ParentForm.Handle);
  end;
  if (Wnd = nil) or (Wnd.Wnd = nil) then
    raise EArgumentException.Create(sArgumentInvalid);
  Result := FindDisplay(Wnd.Wnd.screen);
end;

function TMultiDisplayMac.DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
begin
  Result := DisplayFromWindow(Handle);
end;

function TMultiDisplayMac.GetDisplay(const Index: Integer): TDisplay;
begin
  if Index < 0 then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  if (FDisplayList = nil) or (FDisplayList.Count <> GetDisplayCount) then
    UpdateDisplays;
  if Index >= GetDisplayCount then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FDisplayList[Index];
end;

function TMultiDisplayMac.GetWorkAreaRect: TRect;
begin
  if (FWorkAreaRect.Width <= 0) or (FWorkAreaRect.Height <= 0) then
    FWorkAreaRect := NSRectToRect(TNSScreen.Wrap(TNSScreen.OCClass.mainScreen).visibleFrame);
  Result := FWorkAreaRect;
end;

{ TFMXMenuDelegate }

function TFMXMenuDelegate.confinementRectForMenu(menu: NSMenu; onScreen: NSScreen): NSRect;
begin
  Result.origin.x := 0;
  Result.origin.y := 0;
  Result.size.width := 0;
  Result.size.height := 0;
end;

function TFMXMenuDelegate.menu(menu: NSMenu; updateItem: NSMenuItem; atIndex: NSInteger;
  shouldCancel: Boolean): Boolean;
begin
  Result := True;
end;

class function TFMXMenuDelegate.GetMenuItem(const ALocalId: Pointer): TMenuItem;
begin
  if FFMXMenuDictionary <> nil then
    FFMXMenuDictionary.TryGetValue(ALocalId, Result)
  else
    Result := nil;
end;

procedure TFMXMenuDelegate.menu(menu: NSMenu; willHighlightItem: NSMenuItem);
var
  LMenu: TMenuItem;
begin
  if (FFMXMenuDictionary <> nil) and FFMXMenuDictionary.TryGetValue((willHighlightItem as ILocalObject).GetObjectID, LMenu) then
    Application.Hint := LMenu.Hint
  else
    Application.Hint := string.Empty;
end;

procedure TFMXMenuDelegate.menuDidClose(menu: NSMenu);
begin
  Application.Hint := string.Empty;
end;

function TFMXMenuDelegate.menuHasKeyEquivalent(menu: NSMenu; forEvent: NSEvent; target: Pointer; action: SEL): Boolean;
begin
  Result := False;
end;

procedure TFMXMenuDelegate.menuNeedsUpdate(menu: NSMenu);
begin

end;

procedure TFMXMenuDelegate.menuWillOpen(menu: NSMenu);
var
  LMenu: TMenuItem;
begin
  if (FFMXMenuDictionary <> nil) and FFMXMenuDictionary.TryGetValue((menu as ILocalObject).GetObjectID, LMenu) then
    Application.Hint := LMenu.Hint
  else
    Application.Hint := string.Empty;
end;

function TFMXMenuDelegate.numberOfItemsInMenu(menu: NSMenu): NSInteger;
begin
  Result := -1;
end;

class procedure TFMXMenuDelegate.RegisterMenu(const ALocalId: Pointer; const AMenuItem: TMenuItem);
begin
  if FFMXMenuDictionary = nil then
    FFMXMenuDictionary := TDictionary<Pointer, TMenuItem>.Create;

  FFMXMenuDictionary.Add(ALocalId, AMenuItem);
end;

class procedure TFMXMenuDelegate.UnregisterMenu(const ALocalId: Pointer);
begin
  if FFMXMenuDictionary <> nil then
  begin
    FFMXMenuDictionary.Remove(ALocalId);
    if FFMXMenuDictionary.Count = 0 then
      FreeAndNil(FFMXMenuDictionary);
  end;
end;

function PlatformHookObserverCallback(CancelIdle: Boolean; Mask: NSUInteger = NSAnyEventMask): Boolean;
begin
  Result := PlatformCocoa.HookObserverCallback(CancelIdle, Mask);
end;

end.
