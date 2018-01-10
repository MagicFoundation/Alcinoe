{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2016 Embarcadero Technologies, Inc.      }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Platform.Win;

(*$HPPEMIT '#if defined(WIN32) && defined(CreateWindow)'*)
(*$HPPEMIT '  #define __SAVE_CREATEWINDOW CreateWindow'*)
(*$HPPEMIT '  #undef  CreateWindow'*)
(*$HPPEMIT '#endif'*)

(*$HPPEMIT END '#if defined(__SAVE_CREATEWINDOW)'*)
(*$HPPEMIT END '  #define CreateWindow __SAVE_CREATEWINDOW'*)
(*$HPPEMIT END '  #undef  __SAVE_CREATEWINDOW'*)
(*$HPPEMIT END '#endif'*)

{$HPPEMIT NOUSINGNAMESPACE}
{$R-}
interface

{$SCOPEDENUMS ON}

uses
  Winapi.CommCtrl, Winapi.Windows, Winapi.ActiveX, System.Types, System.Classes, System.UITypes, System.UIConsts,
  System.Generics.Collections, FMX.Forms, FMX.Platform, FMX.Types, FMX.Graphics, FMX.ZOrder.Win;

type
  TWinDropTarget = class;

  PRgnRects = ^TRgnRects;
  TRgnRects = array [0..MaxInt div SizeOf(TRect) - 1] of TRect;

  TUpdateRects = array of TRectF;

  TWinWindowHandle = class(TWindowHandle)
  private class var
    FForcedScale: Single;
  private
    FWnd: HWND;
    FZOrderManager: TWinZOrderManager;
    FBufferBitmap: THandle;
    FBitmapInfo: TBitmapInfo;
    FBufferBits: Pointer;
    FBufferHandle: THandle;
    FBufferSize: TSize;
    FForm: TCommonCustomForm;
    FDisableDeactivate: Boolean;
    FWinDropTarget: TWinDropTarget;
    FCurrentScale: Single;
    FClientSize: TSizeF;
    FWndClientSize: TSize;
    FBounds: TRectF;
    FWndBounds: TRect;
    FNearestIntegerMultiple: Integer;
    procedure UpdateLayer;
    function GetZOrderManager: TWinZOrderManager;
    procedure SetBounds(const Value: TRect);
    procedure SetWndBounds(const Value: TRect);
    procedure SetClientSize(const Value: TSize);
    procedure UpdateClientSize;
    procedure SetWindowSizeByClientSize;
    procedure CalcNearestIntegerMultiple;
    function GetNearestIntegerMultiple: Integer;
  protected
    function GetBounds: TRect; virtual;
    function GetClientSize: TSize; virtual;
    function GetWndBounds: TRect; virtual;
    function GetWndClientSize: TSize; virtual;
    function GetTransparency: Boolean; virtual;
    function GetScale: Single; override;
  public
    constructor Create(const AForm: TCommonCustomForm; const AWnd: HWND);
    destructor Destroy; override;
    class procedure SetForcedScale(NewScale: Single);
    procedure CreateBuffer(const Width, Height: Integer);
    procedure ResizeBuffer(const Width, Height: Integer);
    procedure FreeBuffer;
    procedure CorrectWindowSize(const WindowPos: PWindowPos);
    procedure ScaleChanged;
    procedure DpiChanged(const NewDpi: Integer);
    function WndToForm(const Rect: TRect): TRectF; overload;
    function WndToForm(const Rect: TRectF): TRectF; overload;
    function FormToWnd(const Rect: TRectF): TRectF;
    property Wnd: HWND read FWnd;
    property Form: TCommonCustomForm read FForm;
    property BufferBits: Pointer read FBufferBits;
    property BufferHandle: THandle read FBufferHandle;
    property ZOrderManager: TWinZOrderManager read GetZOrderManager;
    property Transparency: Boolean read GetTransparency;
    property ClientSize: TSize read GetClientSize write SetClientSize;
    property NearestIntegerMultiple: Integer read GetNearestIntegerMultiple;
    property Bounds: TRect read GetBounds write SetBounds;
    property BoundsF: TRectF read FBounds;
    property WndClientSize: TSize read GetWndClientSize;
    property WndBounds: TRect read GetWndBounds write SetWndBounds;
  end;

  TWinDropTarget = class(TComponent, IDropTarget)
  private
    Form: TCommonCustomForm;
    function GetDataObject: TDragObject;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
  end;

function FindWindow(Handle: HWND): TCommonCustomForm;
function WindowHandleToPlatform(const AHandle: TWindowHandle): TWinWindowHandle;

function FmxHandleToHWND(const FmxHandle: TWindowHandle): HWND;
function FormToHWND(Form: TCommonCustomForm): HWND;
function ApplicationHWND: HWND;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

const
  IDC_NODROP =    PChar(32767);
  IDC_DRAG   =    PChar(32766);
  IDC_MULTIDRAG = PChar(32763);
  IDC_SQLWAIT =   PChar(32762);

type
  TApplicationHWNDProc = function: HWND;

procedure RegisterApplicationHWNDProc(const Proc: TApplicationHWNDProc);

implementation

{$SCOPEDENUMS OFF}


uses
  System.Messaging, System.IOUtils, Winapi.CommDlg, Winapi.Messages, Winapi.ShlObj, Winapi.MMSystem, Winapi.ShellAPI,
  Winapi.MultiMon, Winapi.Imm, Winapi.UxTheme, System.Variants, System.SysUtils, System.Math,
  System.Math.Vectors, System.StrUtils, System.DateUtils, System.RTLConsts, System.SyncObjs, System.Rtti, System.Devices,
  FMX.Consts, FMX.Menus, FMX.Helpers.Win, FMX.Printer, FMX.Printer.Win, FMX.Dialogs.Win, FMX.Canvas.GDIP, FMX.Canvas.D2D,
  FMX.Context.DX9, FMX.Context.DX11, FMX.Canvas.GPU, FMX.Forms.Border.Win, FMX.Controls.Win, FMX.Gestures.Win,
  FMX.TextLayout, FMX.Text, FMX.Types3D, FMX.VirtualKeyboard, FMX.Controls, FMX.BehaviorManager, FMX.Styles,
  FMX.MultiTouch.Win, FMX.ImgList, FMX.WebBrowser, FMX.Surfaces, FMX.Utils, FMX.KeyMapping, FMX.AcceleratorKey,
  FMX.AcceleratorKey.Win;

type

  EUnavailableMenuId = class(Exception);

  TOpenMenuItem = class(TMenuItem);

  MySTGMEDIUM = record // for compatibility
    Tymed: DWORD;
    Case Integer Of
      0:
        (HBITMAP: HBITMAP; UnkForRelease: Pointer { IUnknown } );
      1:
        (HMETAFILEPICT: THandle);
      2:
        (HENHMETAFILE: THandle);
      3:
        (HGLOBAL: HGLOBAL);
      4:
        (lpszFileName: POleStr);
      5:
        (stm: Pointer { IStream } );
      6:
        (stg: Pointer { IStorage } );
  end;

  { TDropSource }

  TDataObjectInfo = record
    FormatEtc: TFormatEtc;
    StgMedium: TStgMedium;
    OwnedByDataObject: Boolean;
  end;

  TDataObjectInfoArray = array of TDataObjectInfo;

  TDropSource = class(TComponent, IDataObject, IDropSource)
  private
    Data: TDragObject;
    Formats: TDataObjectInfoArray;
    { IDropSource }
    function QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Longint): HRESULT; stdcall;
    function GiveFeedback(dwEffect: Longint): HRESULT; stdcall;
    { IDataObject }
    function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HRESULT; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HRESULT; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcout: TFormatEtc): HRESULT; stdcall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: BOOL): HRESULT; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out EnumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
    function dAdvise(const FormatEtc: TFormatEtc; advf: Longint; const advsink: IAdviseSink;
      out dwConnection: Longint): HRESULT; stdcall;
    function dUnadvise(dwConnection: Longint): HRESULT; stdcall;
    function EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT; stdcall;
    { For IDropSourceHelper }
    function FindFormatEtc(TestFormatEtc: TFormatEtc): Integer;
    function EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
    function HGlobalClone(HGLOBAL: THandle): THandle;
    function RetrieveOwnedStgMedium(Format: TFormatEtc; var StgMedium: TStgMedium): HRESULT;
    function StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
      CopyInMedium: Boolean): HRESULT;
    function CanonicalIUnknown(const TestUnknown: IUnknown): IUnknown;
  end;

type
  TWin32TimerInfo = record
    TimerID: UIntPtr; // the Windows timer ID for this timer
    TimerHandle: TFmxHandle; // the unique FMX Handle for this timer
    TimerFunc: TTimerProc; // owner function to handle timer
  end;

  TWin32MenuInfo = record
    MenuID: Integer;
    FMXMenuItem: TMenuItem;

    constructor Create(const AMenuId: Integer; const AnItem: TMenuItem);
  end;

  { TPlatformWin }

  TFullScreenParams = record
    BorderStyle: TFmxFormBorderStyle;
    WindowState: TWindowState;
    Position: TPoint;
    Size: TPoint;
  public
    function IsFullScreen: Boolean;
    procedure Clean;
  end;

  TPlatformWin = class(TInterfacedObject, IFMXApplicationService, IFMXSystemFontService, IFMXTimerService,
    IFMXWindowService, IFMXMenuService, IFMXDragDropService, IFMXCursorService, IFMXMouseService,
    IFMXScreenService, IFMXLocaleService, IFMXTextService, IFMXContextService, IFMXCanvasService, IFMXDeviceService,
    IFMXWindowBorderService, IFMXSystemInformationService, IFMXLoggingService, IFMXFullScreenWindowService,
    IFMXListingService, IFMXSaveStateService, IFMXDeviceMetricsService, IFMXGestureRecognizersService,
    IFMXWindowsTouchService, IFMXDefaultMetricsService, IFMXKeyMappingService)
  private const
    DefaultWindowsFontSize = 12;
  private
    FTitle: string;
    FDefaultTitle: string;
    FApplicationHWNDProc: TApplicationHWNDProc;
    FApplicationHWND: HWND;
    FFullScreenSupport: TDictionary<TCommonCustomForm, TFullScreenParams>;
    FHandleCounter: TFmxHandle;
    FHMenuMap: TDictionary<TFmxHandle, TWin32MenuInfo>;
    FHMenuIdMap: TDictionary<Integer, TFmxHandle>;
    FTimerData: TList<TWin32TimerInfo>;
    FDiableUpdateState: Boolean;
    FThreadSyncHandle: HWND;
    FCreateOSMenu: Boolean;
    FInDestroyMenuItem: Boolean;
    FInPaintUpdateRects: TDictionary<TWindowHandle, TUpdateRects>;
    FTerminating: Boolean;
    FCursor: TCursor;
    FCaptionChangedId: Integer;
    FMultiTouchManager: TMultiTouchManagerWin;
    FEnabledInteractiveGestures: TInteractiveGestures;
    FSaveStateStoragePath: string;
    FPerformanceFrequency: Int64;
    FDragAndDropActive: Boolean;
    FKeyMapping: TKeyMapping;
    FAcceleratorKeyRegistry: IFMXAcceleratorKeyRegistryService;
    FIsPostQuitMessage: Boolean;
    procedure ThreadSync(var Msg: TMessage);
    procedure WakeMainThread(Sender: TObject);
    procedure RemoveChildHandles(const AMenu: IItemsContainer);
    procedure CreateAppHandle;
    procedure MinimizeApp;
    procedure RestoreApp;
    // Menu functions
    procedure RemoveMenuFromMaps(MenuHandle: TFmxHandle);
    function GetAvailableIdForMenu: Integer;
    procedure RegisterMenuWithId(const AnId: Integer; const AMenu: HMENU);
    function AssignIdToMenu(ParentMenu, Menu: HMENU): Integer;
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
    procedure UpdateAppTitle;
    procedure CaptionChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    function GetApplicationHWND: HWND;

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
    { IFMXApplicationService }
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function Terminating: Boolean;
    procedure Terminate;
    function GetVersionString: string;
    property ApplicationHWND: HWND read GetApplicationHWND;
    { IFMXSystemFontService }
    function GetDefaultFontFamilyName: string;
    function GetDefaultFontSize: Single;
    { IFMXTimerService }
    function CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
    function DestroyTimer(Timer: TFmxHandle): Boolean;
    function GetTick: Double;
    { IFMXWindowService }
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
    procedure DestroyWindow(const AForm: TCommonCustomForm);
    procedure ReleaseWindow(const AForm: TCommonCustomForm);
    procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
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
    procedure ReleaseCapture(const AForm: TCommonCustomForm);
    function ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function GetWindowScale(const AForm: TCommonCustomForm): Single;
    // for desingtime and testing only
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
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
    function IsMenuBarOnWindowBorder: Boolean;
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
    { IFMXGestureRecognizersService }
    procedure AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    procedure RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
    { IFMXWindowsTouchService }
    procedure HookTouchHandler(const AForm: TCommonCustomForm);
    procedure UnhookTouchHandler(const AForm: TCommonCustomForm);
    { IFMXDeviceService }
    function GetModel: string;
    function GetFeatures: TDeviceFeatures;
    function GetDeviceClass: TDeviceInfo.TDeviceClass;
    { IFMXDefaultMetricsService }
    function SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
    function GetDefaultSize(const AComponent: TComponentKind): TSize;
  end;

  TVirtualKeyboardWin = class(TInterfacedObject, IFMXVirtualKeyboardService)
  private type
    TvkbState = (None, Hidden, Shown);
  private
    FPath: string;
    FExeName: string;
    FWndClassName: string;
    FHTmerLang: TFmxHandle;
    FHTmerVisible: TFmxHandle;
    FKBPresent: Boolean;
    FFormHandle: HWND;
    FInst: HINST;
    FError: Boolean;
    FLastvkbState: TvkbState;
    FLastHandle: HWND;
    FLastTime: TDateTime;
    FNewvkbState: TvkbState;
    FWait: Boolean;
    FStepActivate: Integer;
    FCodeKeyboard: HKL;
    FTimerService: IFMXTimerService;

    procedure KillTimerLang;
    procedure TimerLangProc;
    procedure StartTimerLang;

    procedure KillTimerVisible;
    procedure TimerVisibleProc;
    procedure StartTimerVisible;
    function FindKeyValue(const Key: HKey; const Name, Value, SubKeyName, SubValueName: string): string;
    function GetVirtualKeyboardState: TVirtualKeyboardStates;
    procedure vkbExecute(FormHandle: HWND);
    function vkbHandle: HWND;
    function vkbState: TvkbState;
    function GetVKBounds: TRect;
  protected
    procedure Clear;
    function IsAutoShow: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
    function HideVirtualKeyboard: Boolean;
    procedure SetTransientState(Value: Boolean);
    property VirtualKeyboardState: TVirtualKeyboardStates read GetVirtualKeyboardState;
    property ExeName: string read FExeName write FExeName;
    property Path: string read FPath write FPath;
    property WndClassName: string read FWndClassName write FWndClassName;
  end;

  TMultiDisplayWin = class(TInterfacedObject, IFMXMultiDisplayService)
  private type
    TDisplayWin = record
      Display: TDisplay;
      Handle: HMONITOR;
      constructor Create(const AIndex: Integer; const APrimary: Boolean; const ABounds, AWorkArea: TRect;
        const AHandle: HMONITOR);
    end;
  private
    FDisplayCount: Integer;
    FWorkAreaRect: TRect;
    FDesktopRect: TRect;
    FDisplayList: TList<TDisplayWin>;
    function FindDisplay(const hm: HMONITOR): TDisplay;
    procedure UpdateDisplays;
  public
    destructor Destroy; override;
    procedure UpdateDisplayInformation;
    function GetDisplayCount: Integer;
    function GetWorkAreaRect: TRect;
    function GetDesktopRect: TRect;
    function GetDisplay(const Index: Integer): TDisplay;
    function GetDesktopCenterRect(const Size: TSize): TRect;
    function DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
    function DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
  end;

var
  VirtualKeyboardWin: TVirtualKeyboardWin;
  MultiDisplayWin: TMultiDisplayWin;

const
  CF_FMOBJECT = CF_PRIVATEFIRST + 1;

var
  WindowAtom: TAtom;
  WindowAtomString: string;
  DropAtom: TAtom;
  DropAtomString: string;
  PlatformWin: TPlatformWin;
  CapturedGestureControl: TComponent;

procedure RegisterCorePlatformServices;
begin
  PlatformWin := TPlatformWin.Create;
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXTimerService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXWindowService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXMenuService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDragDropService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXCursorService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXScreenService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXLocaleService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXContextService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXWindowBorderService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXLoggingService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXListingService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXSaveStateService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXGestureRecognizersService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXWindowsTouchService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, PlatformWin);
  TPlatformServices.Current.AddPlatformService(IFMXKeyMappingService, PlatformWin);

  VirtualKeyboardWin := TVirtualKeyboardWin.Create;
  TPlatformServices.Current.AddPlatformService(IFMXVirtualKeyboardService, VirtualKeyboardWin);

  MultiDisplayWin := TMultiDisplayWin.Create;
  TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, MultiDisplayWin);
end;

procedure UnregisterCorePlatformServices;
begin
  TPlatformServices.Current.RemovePlatformService(IFMXDeviceService);
  TPlatformServices.Current.RemovePlatformService(IFMXDeviceMetricsService);
  TPlatformServices.Current.RemovePlatformService(IFMXApplicationService);
  TPlatformServices.Current.RemovePlatformService(IFMXSystemFontService);
  TPlatformServices.Current.RemovePlatformService(IFMXTimerService);
  TPlatformServices.Current.RemovePlatformService(IFMXWindowService);
  TPlatformServices.Current.RemovePlatformService(IFMXMenuService);
  TPlatformServices.Current.RemovePlatformService(IFMXDragDropService);
  TPlatformServices.Current.RemovePlatformService(IFMXCursorService);
  TPlatformServices.Current.RemovePlatformService(IFMXMouseService);
  TPlatformServices.Current.RemovePlatformService(IFMXScreenService);
  TPlatformServices.Current.RemovePlatformService(IFMXLocaleService);
  TPlatformServices.Current.RemovePlatformService(IFMXTextService);
  TPlatformServices.Current.RemovePlatformService(IFMXContextService);
  TPlatformServices.Current.RemovePlatformService(IFMXCanvasService);
  TPlatformServices.Current.RemovePlatformService(IFMXWindowBorderService);
  TPlatformServices.Current.RemovePlatformService(IFMXSystemInformationService);
  TPlatformServices.Current.RemovePlatformService(IFMXFullScreenWindowService);
  TPlatformServices.Current.RemovePlatformService(IFMXVirtualKeyboardService);
  TPlatformServices.Current.RemovePlatformService(IFMXDefaultMetricsService);
end;

{ TPlatformWin }

constructor TPlatformWin.Create;
begin
  inherited;
  WindowAtomString := Format('FIREMONKEY%.8X', [GetCurrentProcessID]);
  WindowAtom := GlobalAddAtomW(PChar(WindowAtomString));
  DropAtomString := Format('FIREMONKEYDROP%.8X', [GetCurrentProcessID]);
  DropAtom := GlobalAddAtomW(PChar(DropAtomString));
  FTimerData := TList<TWin32TimerInfo>.Create;
  FHMenuMap := TDictionary<TFmxHandle, TWin32MenuInfo>.Create;
  FHMenuIdMap := TDictionary<Integer, TFmxHandle>.Create;
  FFullScreenSupport := TDictionary<TCommonCustomForm, TFullScreenParams>.Create;
  FInPaintUpdateRects := TDictionary<TWindowHandle, TUpdateRects>.Create;
  FHandleCounter := 128; // Start counting handles at 128. All valid handles have lower nibble = 0;
  FThreadSyncHandle := AllocateHWnd(ThreadSync);
  FKeyMapping := TKeyMapping.Create;
  FAcceleratorKeyRegistry := TWinAcceleratorKeyRegistry.Create;
  System.Classes.WakeMainThread := WakeMainThread;
  Application := TApplication.Create(nil);
  FCaptionChangedId := TMessageManager.DefaultManager.SubscribeToMessage(TMainCaptionChangedMessage,
    CaptionChangedHandler);
  FMultiTouchManager := nil;
  if not QueryPerformanceFrequency(FPerformanceFrequency) then
    FPerformanceFrequency := 0;
end;

destructor TPlatformWin.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMainCaptionChangedMessage, FCaptionChangedId);
  FreeAndNil(Application);
  FInPaintUpdateRects.Free;
  FHMenuMap.Free;
  FHMenuIdMap.Free;
  FFullScreenSupport.Free;
  FTimerData.Free;
  FAcceleratorKeyRegistry := nil;
  FKeyMapping.Free;
  if FThreadSyncHandle <> 0 then
    DeallocateHWnd(FThreadSyncHandle);
  if PlatformWin = Self then
    PlatformWin := nil;
  inherited;
end;

{ App }

procedure TPlatformWin.Run;
begin
  Application.RealCreateForms;
  UpdateAppTitle;
  repeat
    try
      Application.HandleMessage;
    except
      Application.HandleException(Self);
    end;
  until Application.Terminated;
end;

function TPlatformWin.Terminating: Boolean;
begin
  Result := FTerminating;
end;

procedure TPlatformWin.Terminate;
var
  D: TWin32TimerInfo;
  L: TList<TFmxHandle>;
  I: Integer;
begin
  FTerminating := True;
  L := TList<TFmxHandle>.Create;
  try
    for D in FTimerData do
      if D.TimerHandle <> 0 then
        L.Add(D.TimerHandle);
    for I := L.Count - 1 downto 0 do
      try
        DestroyTimer(L[I]);
      except
        Continue;
      end;
  finally
    FreeAndNil(L);
  end;
  FIsPostQuitMessage := True;
end;

function TPlatformWin.HandleMessage: Boolean;
var
  Msg: TMsg;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    Result := True;
    if Msg.Message <> WM_QUIT then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
      if FIsPostQuitMessage then
        PostQuitMessage(0);
    end
    else
      Application.Terminated := True;
  end;
end;

procedure TPlatformWin.WaitMessage;
begin
  Winapi.Windows.WaitMessage;
end;

function TPlatformWin.GetDefaultTitle: string;
begin
  Result := FDefaultTitle;
end;

function TPlatformWin.GetTitle: string;
begin
  Result := FTitle;
end;

procedure TPlatformWin.SetTitle(const Value: string);
begin
  if FTitle <> Value then
  begin
    if (Value = SAppDefault) and (FDefaultTitle <> '') then
      FTitle := FDefaultTitle
    else
      FTitle := Value;
    UpdateAppTitle;
  end;
end;

function TPlatformWin.GetVersionString: string;
const
  UndefinedVersionInfo = Cardinal(-1);
var
  VersionInfo: Cardinal;
begin
  VersionInfo := GetFileVersion(ParamStr(0));
  if VersionInfo <> UndefinedVersionInfo then
    Result := Format('%d.%d', [HiWord(VersionInfo), LoWord(VersionInfo)])
  else
    Result := string.Empty;
end;

procedure TPlatformWin.UpdateAppTitle;
begin
  if FApplicationHWND <> 0 then
  begin
    { Don't update the title when working in the IDE }
    if (Application <> nil) and (Application.MainForm <> nil) and ((FTitle = SAppDefault) or
      (FTitle = GetDefaultTitle)) then
      SetWindowText(ApplicationHWND, PChar(Application.MainForm.Caption))
    else if FTitle.IsEmpty then
      SetWindowText(ApplicationHWND, PChar(GetDefaultTitle))
    else
      SetWindowText(ApplicationHWND, PChar(FTitle));
  end;
end;

procedure TPlatformWin.CaptionChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
begin
  UpdateAppTitle;
end;

procedure TPlatformWin.WakeMainThread(Sender: TObject);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    PostMessage(FThreadSyncHandle, WM_NULL, 0, 0);
end;

{ Timer }

procedure TimerCallBackProc(window_hwnd: HWND; Msg: Longint; idEvent: UINT; dwTime: Longint); stdcall;
var
  Index: Integer;
begin
  try
    Index := PlatformWin.FTimerData.Count;
    while (Index > 0) do
    begin
      Dec(Index);
      if PlatformWin.FTimerData[Index].TimerID = idEvent then
      begin
        PlatformWin.FTimerData[Index].TimerFunc;
        Break;
      end;
    end;
  except
    on E: Exception do
    begin
      if Application <> nil then
        Application.HandleException(nil)
      else
        Raise;
    end;
  end;
end;

function TPlatformWin.CreateTimer(Interval: Integer; TimerFunc: TTimerProc): TFmxHandle;
var
  TimerInfo: TWin32TimerInfo;
begin
  Result := 0;
  if not FTerminating and (Interval > 0) and Assigned(TimerFunc) then
  begin
    TimerInfo.TimerFunc := TimerFunc;
    TimerInfo.TimerID := Winapi.Windows.SetTimer(0, 0, Interval, @TimerCallBackProc);
    if TimerInfo.TimerID <> 0 then
    begin
{$IFDEF CPUX64}
      TimerInfo.TimerHandle := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ENDIF}
{$IFDEF CPUX86}
      TimerInfo.TimerHandle := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
      FTimerData.Add(TimerInfo);
      Result := TimerInfo.TimerHandle;
    end;
  end;
end;

function TPlatformWin.DestroyTimer(Timer: TFmxHandle): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := FTimerData.Count;
  while (Index > 0) do
  begin
    Dec(Index);
    if FTimerData[Index].TimerHandle = Timer then
    begin
      Result := Winapi.Windows.KillTimer(0, FTimerData[Index].TimerID);
      FTimerData.Delete(Index);
    end;
  end;
end;

function TPlatformWin.GetTick: Double;
var
  PerformanceCounter: Int64;
begin
  if FPerformanceFrequency <> 0 then
  begin
    QueryPerformanceCounter(PerformanceCounter);
    Result := PerformanceCounter / FPerformanceFrequency;
  end
  else
    Result := timeGetTime / 1000;
end;

{ Text Service }

type
  TTextServiceWin = class(TTextService)
  private const
    LCID_Korean_Default = (SUBLANG_KOREAN shl 10) + LANG_KOREAN;
  private
    FCaretPosition: TPoint;
    FText: string;
    FMarkedText: string;
    FImeMode: TImeMode;
    FWorking: Boolean;
  protected
    function GetText: string; override;
    procedure SetText(const Value: string); override;
    function GetCaretPostion: TPoint; override;
    procedure SetCaretPosition(const Value: TPoint); override;
  public
    CompAttrBuf: Array of Byte;
    ImmCompCursorPos: Integer;
    CompClauseBuf: Array of DWORD;

    procedure InternalSetMarkedText(const AMarkedText: string); override;
    function InternalGetMarkedText: string; override;
    procedure InternalStartIMEInput;
    procedure InternalBreakIMEInput;
    procedure InternalEndIMEInput;

    function CombinedText: string; override;
    function TargetClausePosition: TPoint; override;

    procedure EnterControl(const FormHandle: TWindowHandle); override;
    procedure ExitControl(const FormHandle: TWindowHandle); override;

    procedure DrawSingleLine(const Canvas: TCanvas; const ARect: TRectF; const FirstVisibleChar: Integer;
      const Font: TFont; const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False); overload; override;

    procedure DrawSingleLine(const Canvas: TCanvas; const S: string; const ARect: TRectF; const Font: TFont;
      const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False); overload; override;

    function HasMarkedText: Boolean; override;

    function GetImeMode: TImeMode; override;
    procedure SetImeMode(const Value: TImeMode); override;

    { Windows }
    constructor Create(const Owner: IControl; SupportMultiLine: Boolean); override;
    destructor Destroy; override;
  end;

  { Text Service }

constructor TTextServiceWin.Create(const Owner: IControl; SupportMultiLine: Boolean);
begin
  inherited;
end;

destructor TTextServiceWin.Destroy;
begin
  inherited;
end;

function TTextServiceWin.GetText: string;
begin
  Result := FText;
end;

procedure TTextServiceWin.SetText(const Value: string);
begin
  FText := Value;
end;

function TTextServiceWin.GetCaretPostion: TPoint;
begin
  Result := FCaretPosition;
end;

procedure TTextServiceWin.SetCaretPosition(const Value: TPoint);
begin
  FCaretPosition := Value;
end;

procedure TTextServiceWin.InternalSetMarkedText(const AMarkedText: string);
var
  TextInput: ITextInput;
begin
  if FWorking and Supports(Owner, ITextInput, TextInput) then
  begin
    FMarkedText := AMarkedText;
    // Need update.
    TextInput.IMEStateUpdated;
  end;
end;

procedure TTextServiceWin.InternalStartIMEInput;
var
  TextInput: ITextInput;
begin
  if not FWorking and Supports(Owner, ITextInput, TextInput) then
  begin
    FWorking := True;
    TextInput.StartIMEInput;
  end;
end;

function TTextServiceWin.InternalGetMarkedText: string;
begin
  Result := FMarkedText;
end;

procedure TTextServiceWin.InternalBreakIMEInput;
var
  TextInput: ITextInput;
begin
  if FWorking and Supports(Owner, ITextInput, TextInput) then
  begin
    FMarkedText := string.Empty;
    ImmCompCursorPos := 0;
    TextInput.IMEStateUpdated;
    FWorking := False;
  end;
end;

function TTextServiceWin.CombinedText: string;
begin
  if not FMarkedText.IsEmpty then
    Result := FText.Substring(0, FCaretPosition.X) + FMarkedText + FText.Substring(FCaretPosition.X, MaxInt)
  else
    Result := FText;
end;

function TTextServiceWin.TargetClausePosition: TPoint;
begin
  Result := FCaretPosition;
  Result.X := Result.X + ImmCompCursorPos;
end;

procedure TTextServiceWin.InternalEndIMEInput;
var
  TextInput: ITextInput;
begin
  if FWorking and Supports(Owner, ITextInput, TextInput) then
  begin
    TextInput.EndIMEInput;
    FMarkedText := string.Empty;
    ImmCompCursorPos := 0;
    FWorking := False;
  end;
end;

procedure ProcessImeParameters(const Context: HIMC; const Parameters: LPARAM; const TextService: TTextServiceWin);
var
  Result: Integer;
  CompositionString: string;
begin
  if (Parameters and GCS_CURSORPOS) <> 0 then
  begin
    Result := ImmGetCompositionString(Context, GCS_CURSORPOS, nil, 0);
    if (GetLastError = 0) then
      TextService.ImmCompCursorPos := Result;
  end;

  Result := ImmGetCompositionString(Context, GCS_COMPSTR, nil, 0);
  SetLength(CompositionString, Result div SizeOf(Char));
  ImmGetCompositionString(Context, GCS_COMPSTR, PChar(CompositionString), Result);

  if not CompositionString.IsEmpty or TextService.HasMarkedText then
  begin
    if ((Parameters and GCS_CURSORPOS) = 0) and ((GetKeyboardLayout(0) and $FFF) = TTextServiceWin.LCID_Korean_Default)
    then // True for Special support for Korean IME
      TextService.ImmCompCursorPos := Max(1, TextService.FMarkedText.Length);

    TextService.InternalStartIMEInput;
    TextService.InternalSetMarkedText(CompositionString);

    if (Parameters and GCS_COMPATTR) <> 0 then
    begin
      Result := ImmGetCompositionString(Context, GCS_COMPATTR, nil, 0);
      if GetLastError = 0 then
      begin
        SetLength(TextService.CompAttrBuf, Result);
        ImmGetCompositionString(Context, GCS_COMPATTR, @(TextService.CompAttrBuf[0]), Result);
      end;
    end;

    if (Parameters and GCS_COMPCLAUSE) <> 0 then
    begin
      Result := ImmGetCompositionString(Context, GCS_COMPCLAUSE, nil, 0);
      if GetLastError = 0 then
      begin
        SetLength(TextService.CompClauseBuf, Result div SizeOf(DWORD));
        ImmGetCompositionString(Context, GCS_COMPCLAUSE, @(TextService.CompClauseBuf[0]), Result);
      end;
    end;
  end;
end;

procedure TTextServiceWin.EnterControl(const FormHandle: TWindowHandle);
var
  Form: TCommonCustomForm;
  IMC: HIMC;
  WindowHandle: HWND;
  TextInput: ITextInput;
begin
  WindowHandle := WindowHandleToPlatform(FormHandle).Wnd;
  if TPlatformServices.Current.SupportsPlatformService(IFMXWBService) then
    SetFocus(WindowHandle);
  IMC := 0;
  if FImeMode <> TImeMode.imDisable then
  begin
    IMC := ImmGetContext(WindowHandle);
    if IMC = 0 then
    begin
      ImmAssociateContextEx(WindowHandle, 0, IACE_DEFAULT);
      ImmReleaseContext(WindowHandle, IMC);
    end;
  end;
  TImeModeHelper.SetIme(WindowHandle, FImeMode);

  Form := WindowHandleToPlatform(FormHandle).Form;
  if (IMC <> 0) and (Form <> nil) and (Form.Focused <> nil) and Supports(Form.Focused, ITextInput, TextInput) then
  begin
    if ImmGetCompositionString(IMC, GCS_COMPSTR, nil, 0) > 0 then
      ProcessImeParameters(IMC, $FFFF, TTextServiceWin(TextInput.GetTextService));
  end;
end;

procedure TTextServiceWin.ExitControl(const FormHandle: TWindowHandle);
begin
  InternalBreakIMEInput;
  TImeModeHelper.ResetIme(WindowHandleToPlatform(FormHandle).Wnd, FImeMode);
end;

procedure TTextServiceWin.DrawSingleLine(const Canvas: TCanvas; const ARect: TRectF; const FirstVisibleChar: Integer;
  const Font: TFont; const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  I, J: Integer;
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

    if (FMarkedText.Length > 0) and (Length(CompAttrBuf) > 0) then
    begin
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.Stroke.Thickness := 2;
      Canvas.Stroke.Dash := TStrokeDash.Solid;
      try
        for I := 1 to FMarkedText.Length do
        begin
          case CompAttrBuf[I - 1] of
            ATTR_INPUT:
              begin
                Canvas.Stroke.Thickness := 1;
                Canvas.Stroke.Dash := TStrokeDash.Dash
              end;
            ATTR_TARGET_CONVERTED:
              begin
                Canvas.Stroke.Thickness := 2;
                Canvas.Stroke.Dash := TStrokeDash.Solid;
              end;
            ATTR_CONVERTED:
              begin
                Canvas.Stroke.Thickness := 1;
                Canvas.Stroke.Dash := TStrokeDash.Solid;
              end;
            ATTR_TARGET_NOTCONVERTED:
              begin
                Canvas.Stroke.Thickness := 4;
                Canvas.Stroke.Dash := TStrokeDash.Solid;
              end;
            ATTR_INPUT_ERROR:
              begin
                Canvas.Stroke.Thickness := 1;
                Canvas.Stroke.Dash := TStrokeDash.Dot
              end;
          end;

          Region := Layout.RegionForRange(TTextRange.Create(Max(0, CaretPosition.X + I - FirstVisibleChar), 1));
          for J := Low(Region) to High(Region) do
            Canvas.DrawLine(
              PointF(Region[J].Left, Region[J].Bottom),
              PointF(Region[J].Right, Region[J].Bottom),
              AOpacity, Canvas.Stroke);
        end;
      finally
        Canvas.Stroke.Thickness := 1;
        Canvas.Stroke.Dash := TStrokeDash.Solid;
      end;
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

procedure TTextServiceWin.DrawSingleLine(const Canvas: TCanvas; const S: string; const ARect: TRectF; const Font: TFont;
  const AOpacity: Single; const Flags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  I, J: Integer;
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

    if (FMarkedText.Length > 0) and (Length(CompAttrBuf) > 0) then
    begin
      Canvas.Stroke.Assign(Canvas.Fill);
      Canvas.Stroke.Thickness := 2;
      Canvas.Stroke.Dash := TStrokeDash.Solid;
      try
        for I := 1 to FMarkedText.Length do
        begin
          case CompAttrBuf[I - 1] of
            ATTR_INPUT:
              begin
                Canvas.Stroke.Thickness := 1;
                Canvas.Stroke.Dash := TStrokeDash.Dash
              end;
            ATTR_TARGET_CONVERTED:
              begin
                Canvas.Stroke.Thickness := 2;
                Canvas.Stroke.Dash := TStrokeDash.Solid;
              end;
            ATTR_CONVERTED:
              begin
                Canvas.Stroke.Thickness := 1;
                Canvas.Stroke.Dash := TStrokeDash.Solid;
              end;
            ATTR_TARGET_NOTCONVERTED:
              begin
                Canvas.Stroke.Thickness := 4;
                Canvas.Stroke.Dash := TStrokeDash.Solid;
              end;
            ATTR_INPUT_ERROR:
              begin
                Canvas.Stroke.Thickness := 1;
                Canvas.Stroke.Dash := TStrokeDash.Dot
              end;
          end;

          Region := Layout.RegionForRange(TTextRange.Create(Max(0, CaretPosition.X + I), 1));
          for J := Low(Region) to High(Region) do
            Canvas.DrawLine(
              PointF(Region[J].Left, Region[J].Bottom),
              PointF(Region[J].Right, Region[J].Bottom),
              AOpacity, Canvas.Stroke);
        end;
      finally
        Canvas.Stroke.Thickness := 1;
        Canvas.Stroke.Dash := TStrokeDash.Solid;
      end;
    end;
  finally
    FreeAndNil(Layout);
  end;
end;

function TTextServiceWin.HasMarkedText: Boolean;
begin
  Result := not FMarkedText.IsEmpty;
end;

function TTextServiceWin.GetImeMode: TImeMode;
begin
  Result := FImeMode;
end;

procedure TTextServiceWin.SetImeMode(const Value: TImeMode);
begin
  FImeMode := Value;
end;

function TPlatformWin.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceWin;
end;

{ Window }

procedure TWinWindowHandle.CalcNearestIntegerMultiple;
const
  MaxMul = 9;
var
  I: Integer;
begin
  if not SameValue(Frac(Scale), 0, TEpsilon.Scale) then
  begin
    FNearestIntegerMultiple := Round(Scale);
    for I := 2 to MaxMul do
      if SameValue(Frac(Scale * I), 0, TEpsilon.Scale) then
      begin
        FNearestIntegerMultiple := Trunc(Scale * I);
        Break;
      end;
  end
  else
    FNearestIntegerMultiple := Round(Scale);
end;

procedure TWinWindowHandle.CorrectWindowSize(const WindowPos: PWindowPos);
var
  LClientSize: TSizeF;
  WndClientSize: TSize;
  WndClientOffset: TPoint;
  LClientRect, LWindowRect: TRect;
  FinalScale: TPointF;
begin
  GetClientRect(Wnd, LClientRect);
  ClientToScreen(Wnd, LClientRect.TopLeft);
  ClientToScreen(Wnd, LClientRect.BottomRight);
  GetWindowRect(Wnd, LWindowRect);

  WndClientOffset := (LClientRect.TopLeft - LWindowRect.TopLeft) + (LWindowRect.BottomRight - LClientRect.BottomRight);
  WndClientSize := TSize.Create(WindowPos^.cx - WndClientOffset.X, WindowPos^.cy - WndClientOffset.Y);
  if SameValue(Frac(WndClientSize.Width / Scale), 0, TEpsilon.Scale) then
    FinalScale.X := Scale
  else
    FinalScale.X := NearestIntegerMultiple;
  if SameValue(Frac(WndClientSize.Height / Scale), 0, TEpsilon.Scale) then
    FinalScale.Y := Scale
  else
    FinalScale.Y := NearestIntegerMultiple;

  LClientSize := TSizeF.Create(Round(WndClientSize.Width / FinalScale.X), Round(WndClientSize.Height / FinalScale.Y));

  WindowPos^.cx := Round(LClientSize.Width * FinalScale.X) + WndClientOffset.X;
  WindowPos^.cy := Round(LClientSize.Height * FinalScale.Y) + WndClientOffset.Y;
end;

class procedure TWinWindowHandle.SetForcedScale(NewScale: Single);
begin
  if NewScale < 1 then
    NewScale := 1;
  if not SameValue(FForcedScale, NewScale, TEpsilon.Scale) then
    FForcedScale := NewScale;
end;

constructor TWinWindowHandle.Create(const AForm: TCommonCustomForm; const AWnd: HWND);
begin
  inherited Create;
  if not (TCanvasManager.DefaultCanvas.InheritsFrom(TCustomCanvasD2D)) or GlobalUseGPUCanvas then
    FForcedScale := 1;
  FForm := AForm;
  FWnd := AWnd;
  GetWindowRect(Wnd, FWndBounds);
  FBounds := TRectF.Create(FWndBounds.Left, FWndBounds.Top, FWndBounds.Width / Scale, FWndBounds.Height / Scale);
  UpdateClientSize;
end;

function FindWindow(Handle: HWND): TCommonCustomForm;
begin
  Result := nil;
  if (Handle <> 0) then
  begin
    if GlobalFindAtomW(PChar(WindowAtomString)) = WindowAtom then
      Result := Pointer(GetProp(Handle, MakeIntAtom(WindowAtom)))
    else
      Result := nil;
  end;
end;

function FmxHandleToHWND(const FmxHandle: TWindowHandle): HWND;
begin
  if FmxHandle <> nil then
    Result := WindowHandleToPlatform(FmxHandle).Wnd
  else
    Result := 0;
end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TWinWindowHandle;
begin
  Result := TWinWindowHandle(AHandle);
end;

function FormToHWND(Form: TCommonCustomForm): HWND;
begin
  if (Form <> nil) and (Form.Handle is TWinWindowHandle) then
    Result := TWinWindowHandle(Form.Handle).Wnd
  else
    Result := 0;
end;

function ApplicationHWND: HWND;
begin
  if PlatformWin <> nil then
    Result := PlatformWin.GetApplicationHWND
  else
    Result := 0;
end;

destructor TWinWindowHandle.Destroy;
begin
  FreeBuffer;
  FZOrderManager.Free;
  inherited;
end;

procedure TWinWindowHandle.ScaleChanged;
begin
  CalcNearestIntegerMultiple;

  FWndClientSize := TSize.Create(Ceil(FClientSize.Width * Scale), Ceil(FClientSize.Height * Scale));
  FClientSize := TSizeF.Create(FWndClientSize.Width / Scale, FWndClientSize.Height / Scale);
  SetWindowSizeByClientSize;

  Form.RecreateResources;
end;

procedure TWinWindowHandle.DpiChanged;
var
  NewScale: Single;
begin
  NewScale := NewDpi / StandardDpi;
  if not SameValue(FCurrentScale, NewScale, TEpsilon.Scale) then
  begin
    FCurrentScale := NewScale;
    ScaleChanged;
  end;
end;

procedure TWinWindowHandle.CreateBuffer;
begin
  FBufferSize := TSize.Create(Width, Height);
  FBitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
  FBitmapInfo.bmiHeader.biPlanes := 1;
  FBitmapInfo.bmiHeader.biBitCount := 32;
  FBitmapInfo.bmiHeader.biCompression := BI_RGB;
  FBitmapInfo.bmiHeader.biWidth := FBufferSize.Width;
  if FBitmapInfo.bmiHeader.biWidth <= 0 then
    FBitmapInfo.bmiHeader.biWidth := 1;
  FBitmapInfo.bmiHeader.biHeight := -FBufferSize.Height;
  if FBitmapInfo.bmiHeader.biHeight >= 0 then
    FBitmapInfo.bmiHeader.biHeight := -1;
  try
    FBufferBitmap := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBufferBits), 0, 0);
    if FBufferBits = nil then
      RaiseLastOSError;
    try
      FBufferHandle := CreateCompatibleDC(0);
      if FBufferHandle = 0 then
        RaiseLastOSError;
      try
        if SelectObject(FBufferHandle, FBufferBitmap) = 0 then
          RaiseLastOSError;
      except
        DeleteDC(FBufferHandle);
        FBufferHandle := 0;
        raise;
      end;
    except
      DeleteObject(FBufferBitmap);
      FBufferBits := nil;
      raise;
    end;
  except
    FBufferBitmap := 0;
    raise;
  end;
end;

procedure TWinWindowHandle.ResizeBuffer(const Width, Height: Integer);
begin
  if FBufferHandle = 0 then
    CreateBuffer(Width, Height)
  else if (FBitmapInfo.bmiHeader.biWidth <> Width) or (Abs(FBitmapInfo.bmiHeader.biHeight) <> Height) then
  begin
    FreeBuffer;
    CreateBuffer(Width, Height)
  end;
end;

function TWinWindowHandle.GetClientSize: TSize;
begin
  Result := FClientSize.Round;
end;

function TWinWindowHandle.GetNearestIntegerMultiple: Integer;
begin
  if FNearestIntegerMultiple = 0 then
    CalcNearestIntegerMultiple;
  Result := FNearestIntegerMultiple;
end;

procedure TWinWindowHandle.SetWindowSizeByClientSize;
var
  LClientRect, LWindowRect: TRect;
  NewWindowSize: TSize;
begin
  GetClientRect(Wnd, LClientRect);
  ClientToScreen(Wnd, LClientRect.TopLeft);
  ClientToScreen(Wnd, LClientRect.BottomRight);
  GetWindowRect(Wnd, LWindowRect);

  NewWindowSize := TSize.Create(Ceil(FWndClientSize.Width) + (LClientRect.Left - LWindowRect.Left) +
    (LWindowRect.Right - LClientRect.Right), Ceil(FWndClientSize.Height) + (LClientRect.Top - LWindowRect.Top) +
    (LWindowRect.Bottom - LClientRect.Bottom));

  if (LWindowRect.Width <> NewWindowSize.Width) or (LWindowRect.Height <> NewWindowSize.Height) then
  begin
    FWndBounds.Width := NewWindowSize.Width;
    FWndBounds.Height := NewWindowSize.Height;
    FBounds.Width := NewWindowSize.Width / Scale;
    FBounds.Height := NewWindowSize.Height / Scale;
    SetWindowPos(Wnd, 0, LWindowRect.Left, LWindowRect.Top, NewWindowSize.Width, NewWindowSize.Height,
      SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE);
  end;
end;

procedure TWinWindowHandle.SetClientSize(const Value: TSize);
begin
  if FClientSize.Round <> Value then
  begin
    FWndClientSize := TSize.Create(Ceil(Value.Width * Scale), Ceil(Value.Height * Scale));
    FClientSize := TSizeF.Create(FWndClientSize.Width / Scale, FWndClientSize.Height / Scale);
    SetWindowSizeByClientSize;
  end;
end;

function TWinWindowHandle.GetBounds: TRect;
begin
  Result := FBounds.Ceiling;
end;

procedure TWinWindowHandle.SetBounds(const Value: TRect);
var
  CurrentWndRect: TRect;
begin
  if FBounds.Round <> Value then
  begin
    FWndBounds := TRect.Create(Value.Left, Value.Top, Value.Left + Ceil(Value.Width * Scale),
      Value.Top + Ceil(Value.Height * Scale));
    FBounds := TRectF.Create(FWndBounds.Left, FWndBounds.Top, FWndBounds.Left + FWndBounds.Width / Scale,
      FWndBounds.Top + FWndBounds.Height / Scale);
    GetWindowRect(Wnd, CurrentWndRect);
    if CurrentWndRect <> FWndBounds then
      SetWindowPos(Wnd, 0, FWndBounds.Left, FWndBounds.Top, FWndBounds.Width, FWndBounds.Height,
        SWP_NOACTIVATE or SWP_NOZORDER);
    UpdateClientSize;
  end;
end;

function TWinWindowHandle.GetWndBounds: TRect;
begin
  Result := FWndBounds;
end;

procedure TWinWindowHandle.SetWndBounds(const Value: TRect);
var
  CurrentWndRect: TRect;
begin
  if FWndBounds <> Value then
  begin
    FWndBounds := Value;
    FBounds := TRectF.Create(Value.Left, Value.Top, Value.Left + Value.Width / Scale, Value.Top + Value.Height / Scale);
    GetWindowRect(Wnd, CurrentWndRect);
    if CurrentWndRect <> Value then
      SetWindowPos(Wnd, 0, Value.Left, Value.Top, Value.Width, Value.Height,
        SWP_NOACTIVATE or SWP_NOZORDER);
    UpdateClientSize;
  end;
end;

function TWinWindowHandle.GetWndClientSize: TSize;
begin
  Result := FWndClientSize;
end;

procedure TWinWindowHandle.FreeBuffer;
begin
  if FBufferHandle <> 0 then
    DeleteDC(FBufferHandle);
  if FBufferBitmap <> 0 then
    DeleteObject(FBufferBitmap);
end;

function TWinWindowHandle.GetScale: Single;
begin
  if not SameValue(FForcedScale, 0, TEpsilon.Scale) then
    Result := FForcedScale
  else if not SameValue(FCurrentScale, 0, TEpsilon.Scale) then
    Result := FCurrentScale
  else
    Result := GetWndScale(FWnd);

  FCurrentScale := Result;
end;

function TWinWindowHandle.GetTransparency: Boolean;
begin
  Result := (Form <> nil) and Form.Transparency;
end;

function TWinWindowHandle.GetZOrderManager: TWinZOrderManager;
begin
  if FZOrderManager = nil then
    FZOrderManager := TWinZOrderManager.Create;
  Result := FZOrderManager;
end;

procedure TWinWindowHandle.UpdateClientSize;
var
  R: TRect;
begin
  GetClientRect(Wnd, R);
  FWndClientSize := TSize.Create(R.Width, R.Height);
  FClientSize := TSizeF.Create(FWndClientSize.Width / Scale, FWndClientSize.Height / Scale);
end;

procedure TWinWindowHandle.UpdateLayer;
var
  Blend: TBlendFunction;
  Origin, Size, BitmapOrigin: TPoint;
  ContextObject: IContextObject;
begin
  if FBufferHandle <> 0 then
  begin
    { Copy from Context }
    if Supports(Form, IContextObject, ContextObject) and (ContextObject.Context <> nil) then
      ContextObject.Context.CopyToBits(FBufferBits, Form.Width * 4, Rect(0, 0, Form.Width, Form.Height));
    { Update }
    Origin := WindowHandleToPlatform(Form.Handle).WndBounds.TopLeft;
    Size := TPoint(WindowHandleToPlatform(Form.Handle).WndBounds.Size);
    Blend.BlendOp := AC_SRC_OVER;
    Blend.AlphaFormat := $01; // AC_SRC_ALPHA;
    Blend.BlendFlags := 0;
    Blend.SourceConstantAlpha := $FF;
    BitmapOrigin := Point(0, 0);
    UpdateLayeredWindow(Wnd, 0, @Origin, @Size, FBufferHandle, @BitmapOrigin, $00000000, @Blend, ULW_ALPHA);
  end;
end;

function TWinWindowHandle.FormToWnd(const Rect: TRectF): TRectF;
begin
  Result := TRectF.Create(Rect.Left * Scale, Rect.Top * Scale, Rect.Right * Scale, Rect.Bottom * Scale);
end;

function TWinWindowHandle.WndToForm(const Rect: TRect): TRectF;
begin
  Result := WndToForm(TRectF.Create(Rect));
end;

function TWinWindowHandle.WndToForm(const Rect: TRectF): TRectF;
begin
  Result := TRectF.Create(Rect.Left / Scale, Rect.Top / Scale, Rect.Right / Scale, Rect.Bottom / Scale);
end;

function WMPaint(HWND: HWND; uMsg: UINT; wParam: wParam; LPARAM: LPARAM): LRESULT; stdcall;
var
  I, rgnStatus: Integer;
  Region: HRgn;
  RegionSize: DWORD;
  RegionData: PRgnData;
  R: TRect;
  LForm: TCommonCustomForm;
  UpdateRects, InPaintUpdateRects: TUpdateRects;
  PS: TPaintStruct;
  Wnd: Winapi.Windows.HWND;
  PaintControl: IPaintControl;
begin
  LForm := FindWindow(HWND);
  if LForm <> nil then
  begin
    Wnd := FormToHWND(LForm);
    GetUpdateRect(Wnd, R, False);
    Region := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    if Region <> 0 then
      try
        rgnStatus := GetUpdateRgn(Wnd, Region, False);
        if (rgnStatus = 2) or (rgnStatus = 3) then
        begin
          RegionSize := GetRegionData(Region, $FFFF, nil);
          if RegionSize > 0 then
          begin
            GetMem(RegionData, RegionSize);
            try
              if RegionSize = GetRegionData(Region, RegionSize, RegionData) then
              begin
                SetLength(UpdateRects, RegionData.rdh.nCount);
                for I := 0 to RegionData.rdh.nCount - 1 do
                  UpdateRects[I] := WindowHandleToPlatform(LForm.Handle).WndToForm(PRgnRects(@RegionData.buffer[0])[I]);
              end;
            finally
              FreeMem(RegionData, RegionSize);
            end;
            if Supports(LForm, IPaintControl, PaintControl) then
            begin
              PaintControl.ContextHandle := BeginPaint(Wnd, PS);
              try
                if PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, InPaintUpdateRects) and
                  (Length(InPaintUpdateRects) > 0) then
                begin
                  // add update rects from FInPaintUpdateRects
                  for I := 0 to High(InPaintUpdateRects) do
                  begin
                    SetLength(UpdateRects, Length(UpdateRects) + 1);
                    UpdateRects[High(UpdateRects)] := WindowHandleToPlatform(LForm.Handle).WndToForm(InPaintUpdateRects[I]);
                  end;
                end;
                PaintControl.PaintRects(UpdateRects);
                if PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, InPaintUpdateRects) and
                  (Length(InPaintUpdateRects) > 0) then
                begin
                  // paint second time - when Repaint called in painting
                  PlatformWin.FInPaintUpdateRects.TryGetValue(LForm.Handle, UpdateRects);
                  SetLength(InPaintUpdateRects, 0);
                  PlatformWin.FInPaintUpdateRects.AddOrSetValue(LForm.Handle, InPaintUpdateRects);
                  PaintControl.PaintRects(UpdateRects);
                end;
                PaintControl.ContextHandle := 0;
              finally
                EndPaint(Wnd, PS);
              end;
            end;
          end;
        end;
      finally
        DeleteObject(Region);
      end;
    Result := DefWindowProc(HWND, uMsg, wParam, LPARAM);
  end
  else
    Result := DefWindowProc(HWND, uMsg, wParam, LPARAM);
end;

function WMImeStartComposition(const AForm: TCommonCustomForm; HWND: HWND; uMsg: UINT; wParam: wParam;
  LPARAM: LPARAM): LRESULT;
var
  TSObj: ITextInput;
begin
  if (AForm.Focused <> nil) and Supports(AForm.Focused, ITextInput, TSObj) then
    TTextServiceWin(TSObj.GetTextService).InternalStartIMEInput;
  if System.SysUtils.Win32MajorVersion >= 6 then
    Result := DefWindowProc(HWND, uMsg, wParam, LPARAM)
  else
    Result := 0;
end;

function WMImeComposition(const AForm: TCommonCustomForm; uMsg: UINT; wParam: wParam; LPARAM: LPARAM): LRESULT;
var
  IMC: HIMC;
  S: string;
  Size: Integer;
  Wnd: HWND;
  TextInput: ITextInput;
  TextService: TTextServiceWin;
  I: Integer;
  Key: Word;
  Ch: Char;
  Processed: Boolean;
begin
  Processed := False;
  Result := 0;

  Wnd := FormToHWND(AForm);
  if (LPARAM and GCS_RESULTSTR) <> 0 then
  begin
    Processed := True;
    IMC := ImmGetContext(Wnd);
    if IMC <> 0 then
    begin
      try
        if (AForm.Focused <> nil) and Supports(AForm.Focused, ITextInput, TextInput) then
        begin
          if not TTextServiceWin(TextInput.GetTextService).FWorking then
            Exit;
          TTextServiceWin(TextInput.GetTextService).InternalBreakIMEInput;
        end;
        Size := ImmGetCompositionString(IMC, GCS_RESULTSTR, nil, 0);
        SetLength(S, Size div SizeOf(Char));
        ImmGetCompositionString(IMC, GCS_RESULTSTR, PChar(S), Size);
      finally
        ImmReleaseContext(Wnd, IMC);
      end;
      for I := 0 to S.Length - 1 do
      begin
        Key := 0;
        Ch := S.Chars[I];
        AForm.KeyDown(Key, Ch, []);
        AForm.KeyUp(Key, Ch, []);
      end;
    end;
    if (GetKeyboardLayout(0) and $FFF) = TTextServiceWin.LCID_Korean_Default then // Special support for Korean IME
      SendMessage(Wnd, WM_IME_STARTCOMPOSITION, 0, 0);
  end;

  if (LPARAM and GCS_COMPSTR) <> 0 then
  begin
    Processed := True;
    IMC := ImmGetContext(Wnd);
    if IMC <> 0 then
    begin
      try
        if (AForm.Focused <> nil) and Supports(AForm.Focused, ITextInput, TextInput) then
          ProcessImeParameters(IMC, LPARAM, TTextServiceWin(TextInput.GetTextService));
      finally
        ImmReleaseContext(Wnd, IMC);
      end;
    end;
  end;

  if not Processed then
  begin
    // Pressed ESC
    if (AForm.Focused <> nil) and Supports(AForm.Focused, ITextInput, TextInput) then
    begin
      TextService := TTextServiceWin(TextInput.GetTextService);
      // For Chinese Microsoft engkoo - do not call break
      if (GetKeyboardLayout(0) and $FFFF) <> $0804 then
        if TextService.FWorking then
          TextService.InternalBreakIMEInput;
    end;
    Result := DefWindowProc(Wnd, uMsg, wParam, LPARAM);
  end;
end;

// When the WM_GESTURENOTIFY message is received, use SetGestureConfig to specify the gestures to receive.
// This message should always be bubbled up using the DefWindowProc function.
procedure WMGestureNotify(const AForm: TCommonCustomForm; uMsg: UINT; AGestureNotify: LPARAM);
const
  // Gestures
  CPan: array [Boolean] of Cardinal = (0, GC_PAN);
  CZoom: array [Boolean] of Cardinal = (0, GC_ZOOM);
  CRotate: array [Boolean] of Cardinal = (0, GC_ROTATE);
  CPressAndTap: array [Boolean] of Cardinal = (0, GC_PRESSANDTAP);
  CTwoFingerTap: array [Boolean] of Cardinal = (0, GC_TWOFINGERTAP);
var
  LPoint: TPoint;
  LPointF: TPointF;
  LControl: TComponent;
  LConfigs: array of TGestureConfig;
  LGestures: TInteractiveGestures;
  LObj: IControl;
  LGObj: IGestureControl;
  LGestureNotify: ^GESTURENOTIFYSTRUCT;
begin
  LGestureNotify := Pointer(AGestureNotify);

  // Get the location of the gesture.
  LPoint := SmallPointToPoint(LGestureNotify.ptsLocation);
  LPointF := TPointF.Create(LPoint.X, LPoint.Y);

  // Find the object that the gesture belongs to.
  LObj := AForm.ObjectAtPoint(LPointF);
  if LObj <> nil then
    LControl := LObj.GetObject
  else
    LControl := AForm;

  if Supports(LControl, IGestureControl, LGObj) then
    LGestures := LGObj.GetListOfInteractiveGestures;

  SetLength(LConfigs, 5);
  ZeroMemory(@LConfigs[0], SizeOf(GestureConfig) * 5);

  // Pan gesture & options
  LConfigs[0].dwID := GID_PAN;
  LConfigs[0].dwWant := CPan[TInteractiveGesture.Pan in LGestures] or GC_PAN_WITH_SINGLE_FINGER_VERTICALLY or
    GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY or GC_PAN_WITH_INERTIA;
  LConfigs[0].dwBlock := CPan[not (TInteractiveGesture.Pan in LGestures)] or GC_PAN_WITH_GUTTER;

  // Zoom gesture
  LConfigs[1].dwID := GID_ZOOM;
  LConfigs[1].dwWant := CZoom[TInteractiveGesture.Zoom in LGestures];
  LConfigs[1].dwBlock := CZoom[not (TInteractiveGesture.Zoom in LGestures)];

  // Rotate gesture
  LConfigs[2].dwID := GID_ROTATE;
  LConfigs[2].dwWant := CRotate[TInteractiveGesture.Rotate in LGestures];
  LConfigs[2].dwBlock := CRotate[not (TInteractiveGesture.Rotate in LGestures)];

  // TwoFingerTap gesture
  LConfigs[3].dwID := GID_TWOFINGERTAP;
  LConfigs[3].dwWant := CTwoFingerTap[TInteractiveGesture.TwoFingerTap in LGestures];
  LConfigs[3].dwBlock := CTwoFingerTap[not (TInteractiveGesture.TwoFingerTap in LGestures)];

  // PressAnTap gesture
  LConfigs[4].dwID := GID_PRESSANDTAP;
  LConfigs[4].dwWant := CPressAndTap[TInteractiveGesture.PressAndTap in LGestures];
  LConfigs[4].dwBlock := CPressAndTap[not (TInteractiveGesture.PressAndTap in LGestures)];

  SetGestureConfig(FormToHWND(AForm), 0, Length(LConfigs), @LConfigs[0], SizeOf(TGestureConfig));
end;

function WMGesture(const AForm: TCommonCustomForm; uMsg: UINT; AParam: wParam; AGestureInfo: LPARAM): LRESULT;
var
  LPoint: TPointF;
  LControl: TComponent;
  LGestureInfo: GestureInfo;
  EventInfo: TGestureEventInfo;
  Obj: IControl;
  LGObj: IGestureControl;
begin
  Result := 0;
  ZeroMemory(@LGestureInfo, SizeOf(LGestureInfo));
  LGestureInfo.cbSize := SizeOf(LGestureInfo);
  if GetGestureInfo(AGestureInfo, LGestureInfo) then
    try
      ZeroMemory(@EventInfo, SizeOf(EventInfo));
      EventInfo.GestureID := LGestureInfo.dwID + igiFirst;

      // Get the control
      LPoint := TPointF.Create(LGestureInfo.ptsLocation.X, LGestureInfo.ptsLocation.Y);
      Obj := AForm.ObjectAtPoint(LPoint);
      if Obj <> nil then
        LControl := Obj.GetObject
      else
        LControl := AForm;

      if EventInfo.GestureID = igiBegin then
        CapturedGestureControl := LControl;

      // Don't pass GID_BEGIN and GID_END to the control
      if (EventInfo.GestureID <> igiBegin) and
        (EventInfo.GestureID <> igiEnd) then
      begin
        // Set EventInfo fields from LGestureInfo
        EventInfo.Location := AForm.ScreenToClient(LPoint);
        EventInfo.Flags := [];
        if LGestureInfo.dwFlags and GF_BEGIN = GF_BEGIN then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfBegin);
        if LGestureInfo.dwFlags and GF_INERTIA = GF_INERTIA then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfInertia);
        if LGestureInfo.dwFlags and GF_END = GF_END then
          Include(EventInfo.Flags, TInteractiveGestureFlag.gfEnd);
        case EventInfo.GestureID of
          igiPan:
            begin
              EventInfo.Distance := Cardinal(LGestureInfo.ullArguments);
              EventInfo.InertiaVector :=
                TPointF(SmallPointToPoint(InertiaVectorFromArgument(LGestureInfo.ullArguments)));
            end;
          igiZoom, igiTwoFingerTap:
            EventInfo.Distance := Cardinal(LGestureInfo.ullArguments);
          igiPressAndTap:
            begin
              // ullArguments is distance/offset. Add to Location to make TapLocation
              LPoint := TPointF(SmallPointToPoint(TSmallPoint(Cardinal(LGestureInfo.ullArguments))));
              // EventInfo.TapLocation := AForm.ScreenToClient(TPointF(LPoint.X + LGestureInfo.ptsLocation.X, LPoint.Y + LGestureInfo.ptsLocation.Y));
            end;
          igiRotate:
            EventInfo.Angle := RotateAngleFromArgument(LGestureInfo.ullArguments);
        end;
        // send message to the control
        if Supports(CapturedGestureControl, IGestureControl, LGObj) then
          LGObj.CMGesture(EventInfo);
      end
      else
        Result := DefWindowProc(FormToHWND(AForm), uMsg, AParam, AGestureInfo);

      if EventInfo.GestureID = igiEnd then
        CapturedGestureControl := nil;

    finally
      CloseGestureInfoHandle(AGestureInfo);
    end;
end;

procedure InitializeMultiTouch(const AForm: TCommonCustomForm);
begin
  if PlatformWin.FMultiTouchManager = nil then
    PlatformWin.FMultiTouchManager := TMultiTouchManagerWin.Create(AForm)
  else if PlatformWin.FMultiTouchManager.Parent <> AForm then
    PlatformWin.FMultiTouchManager.Parent := AForm;
end;

function WMTouch(const AForm: TCommonCustomForm; uMsg: UINT; TouchInputCount: wParam; TouchInputInfo: LPARAM): LRESULT;
var
  TouchCount: Integer;
  Inputs: array of TTouchInput;
  Input: TTouchInput;
  I: Integer;
  Touches: TTouches;
  Action: TTouchAction;
  Point: TPointF;
  Control: IControl;
begin
  Result := 0;
  TouchCount := LoWord(Cardinal(TouchInputCount));
  SetLength(Inputs, TouchCount);
  Action := TTouchAction.None;

  if GetTouchInputInfo(TouchInputInfo, TouchCount, @Inputs[0], SizeOf(TTouchInput)) then
    try
      SetLength(Touches, TouchCount);
      for I := 0 to TouchCount - 1 do
      begin
        Input := Inputs[I];

        if (Input.dwFlags and TOUCHEVENTF_DOWN) <> 0 then
          Action := TTouchAction.Down
        else if ((Input.dwFlags and TOUCHEVENTF_UP) <> 0) then
          Action := TTouchAction.Up
        else if ((Input.dwFlags and TOUCHEVENTF_MOVE) <> 0) then
          Action := TTouchAction.Move;

        // TOUCHINFO point coordinates is in 1/100 of a pixel
        Point := TPointF.Create(Input.X / 100, Input.Y / 100);
        Touches[I].Location := AForm.ScreenToClient(Point);
      end;

      if Length(Touches) = 1 then
        Control := AForm.ObjectAtPoint(Point)
      else if Length(Touches) = 2 then
        Control := AForm.ObjectAtPoint(AForm.ClientToScreen(Touches[0].Location.MidPoint(Touches[1].Location)))
      else
        Control := nil;

      InitializeMultiTouch(AForm);
      PlatformWin.FMultiTouchManager.SetEnabledGestures(PlatformWin.FEnabledInteractiveGestures);
      PlatformWin.FMultiTouchManager.HandleTouches(Touches, Action, Control);
      Result := DefWindowProc(FormToHWND(AForm), uMsg, TouchInputCount, TouchInputInfo);
    finally
      CloseTouchInputHandle(TouchInputInfo);
    end;
end;

procedure HandleMouseGestures(const AForm: TCommonCustomForm; uMsg: UINT; const X, Y: Single);
var
  GestureObj: IGestureControl;
  Control: TComponent;
  Obj: IControl;
  Action: TTouchAction;
  Point: TPointF;
begin
  if TWinTouchGestureEngine.Supported(AForm) then
    if not ((uMsg <> WM_LBUTTONDOWN) and (PlatformWin.FMultiTouchManager = nil)) then
    begin
      Point := TPointF.Create(X, Y);
      Obj := AForm.ObjectAtPoint(Point);
      if Obj <> nil then
        Control := Obj.GetObject
      else
        Control := AForm;

      if Supports(Control, IGestureControl, GestureObj) then
      begin
        Control := GestureObj.GetFirstControlWithGestureEngine;
        if (Control <> nil) then
        begin
          case uMsg of
            WM_MOUSEMOVE:
              Action := TTouchAction.Move;
            WM_LBUTTONDOWN:
              Action := TTouchAction.Down;
            WM_LBUTTONUP:
              Action := TTouchAction.Up;
          else
            Action := TTouchAction.None;
          end;

          InitializeMultiTouch(AForm);
          PlatformWin.FMultiTouchManager.HandleMouseGestures(Point, Action, Obj);
        end;
      end;
    end;
end;

function SetIMEWndPosition(const AForm: TCommonCustomForm; uMsg: UINT; wParam: wParam; LPARAM: LPARAM): LRESULT;
var
  IMC: HIMC;
  Wnd: HWND;
  Candidate: TCandidateForm;
  TSObj: ITextInput;
begin
  Wnd := FormToHWND(AForm);
  Result := 0;
  IMC := ImmGetContext(Wnd);
  if IMC <> 0 then
  begin
    try
      if (AForm.Focused <> nil) and Supports(AForm.Focused, ITextInput, TSObj) then
      begin
        Candidate.dwIndex := 0;
        Candidate.dwStyle := CFS_POINT;
        Candidate.ptCurrentPos := TSObj.GetTargetClausePointF.Round;
        Result := LRESULT(ImmSetCandidateWindow(IMC, @Candidate));

        Candidate.dwStyle := CFS_EXCLUDE;
        Candidate.rcArea := TRect.Create(Candidate.ptCurrentPos.X, Candidate.ptCurrentPos.Y,
          Candidate.ptCurrentPos.X + 1, Candidate.ptCurrentPos.Y + 1);
        ImmSetCandidateWindow(IMC, @Candidate);
      end;
    finally
      ImmReleaseContext(Wnd, IMC);
    end;
  end;
end;

function WMImeNotify(const AForm: TCommonCustomForm; HWND: HWND; uMsg: UINT; wParam: wParam; LPARAM: LPARAM): LRESULT;
begin
  if wParam = IMN_OPENCANDIDATE then
    Result := SetIMEWndPosition(AForm, uMsg, wParam, LPARAM)
  else
    Result := DefWindowProc(HWND, uMsg, wParam, LPARAM);
end;

//

type
  PDestroyChildData = ^TDestroyChildData;

  TDestroyChildData = record
    Parent: HWND;
    Recreating: Boolean;
  end;

var
  LastKeyIsDeadKey: Boolean = False;
  LastMousePos: TPoint;

const
  ImpossibleMousePosition: TPoint = (X: Low(FixedInt); Y: Low(FixedInt));

type
  TOpenForm = class(TCommonCustomForm)

  end;

function WndProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  UpdateRects: array of TRectF;
  LForm: TCommonCustomForm;
  Wnd: Winapi.Windows.HWND;
  WindowBorder: TWindowBorderWin;

  procedure ProcessUpdateMessages;
  var
    Msg: TMsg;
  begin
    SetLength(UpdateRects, 1);
    UpdateRects[0] := TRectF.Create(TSmallPoint(Cardinal(wParam)).X, TSmallPoint(Cardinal(wParam)).Y,
      TSmallPoint(Cardinal(lParam)).X, TSmallPoint(Cardinal(lParam)).Y);
    while PeekMessage(Msg, hwnd, WM_ADDUPDATERECT, WM_ADDUPDATERECT, PM_REMOVE) do
    begin
      if Msg.Message = WM_QUIT then
      begin
        { Repost WM_QUIT messages }
        PostQuitMessage(Msg.wParam);
        Break;
      end;
      SetLength(UpdateRects, Length(UpdateRects) + 1);
      UpdateRects[High(UpdateRects)] := RectF(TSmallPoint(Cardinal(Msg.wParam)).X, TSmallPoint(Cardinal(Msg.wParam)).Y,
        TSmallPoint(Cardinal(Msg.lParam)).X, TSmallPoint(Cardinal(Msg.lParam)).Y);
    end;
  end;

  procedure PrepareClosePopups;
  begin
    if (Screen <> nil) and (LForm <> nil) and (not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate) then
    begin
      if LForm.FormStyle = TFormStyle.Popup then
        Screen.PrepareClosePopups(LForm)
      else
        Screen.PrepareClosePopups(nil);
    end;
  end;

  procedure ClosePopupList;
  begin
    if (Screen <> nil) and (LForm <> nil) and (not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate) then
      Screen.ClosePopupForms;
  end;

  procedure InitialActionsOfPopups;
  begin
    case uMsg of
      WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCMBUTTONDOWN, WM_NCLBUTTONDBLCLK, WM_NCRBUTTONDBLCLK, WM_NCMBUTTONDBLCLK:
        begin
          PrepareClosePopups;
          ClosePopupList;
        end;
      WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK:
        PrepareClosePopups;
    end;
  end;

  procedure FinalActionsOfPopups;
  begin
    case uMsg of
      WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP, WM_DESTROY:
        begin
          LForm := FindWindow(hwnd);
          ClosePopupList;
        end;
    end;
  end;

  function DispatchMouseWheelToPopups: Boolean;
  var
    I: Integer;
    Handled: Boolean;
  begin
    Handled := False;
    if Screen <> nil then
      for I := Screen.PopupFormCount - 1 downto 0 do
        if Screen.PopupForms[I].Visible then
        begin
          Screen.PopupForms[I].MouseWheel(KeysToShiftState(wParam), TSmallPoint(Cardinal(wParam)).Y, Handled);
          if Handled then
            Break;
        end;
    Result := Handled;
  end;

  procedure CurrentChar(Msg: tagMsg; var Key: Word; var Ch: WideChar; var Shift: TShiftState);
  begin
    Key := wParam;
    Ch := WideChar(Msg.wParam);
    Shift := KeyDataToShiftState(lParam);
    if (Ch >= ' ') then
    begin
      if ((Shift * [ssAlt, ssCtrl]) = [ssAlt, ssCtrl]) then
      begin
        // AltGr + Char (in German keyboard)
        Shift := Shift - [ssAlt, ssCtrl];
      end;
      if (([ssAlt, ssCtrl, ssCommand] * Shift) = []) then
        Key := 0;
    end;
    if ((([ssAlt, ssCtrl, ssCommand] * Shift) <> []) or (Ch < ' ')) and (Key > 0) then
      Ch := #0;
  end;

  procedure ScaleMousePos(const Handle: TWindowHandle; var P: TPoint);
  begin
    ScreenToClient(WindowHandleToPlatform(Handle).Wnd, P);
    P := TPoint.Create(Round(P.X / Handle.Scale), Round(P.Y / Handle.Scale));
  end;

var
  R: TRect;
  P: TPoint;
  H: Boolean;
  Key: Word;
  Ch: WideChar;
  tme: TTRACKMOUSEEVENT;
  TMsg: TMessage;
  Shift: TShiftState;
  Placement: TWindowPlacement;
  TSObj: ITextInput;
  Msg: tagMsg;
  PaintControl: IPaintControl;
  LMenuItem: TWin32MenuInfo;
  Obj: IControl;
  MenuDisplayed: Boolean;
  MenuIdentifier: Integer;
  MenuHandle: HMENU;
  FMXMenuHandle: TFmxHandle;
  OldWindowState: TWindowState;
  CharMsg, DeadCharMsg: UInt32;
const
  FlagZOrder: UINT = SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE;

begin
                                                                       
  Result := 0;
  LForm := FindWindow(hwnd);

  // Check to see if this is a design message
  if (LForm <> nil) and (LForm.Designer <> nil) then
  begin
    TMsg.Msg := uMsg;
    TMsg.WParam := wParam;
    TMsg.LParam := lParam;
    TMsg.Result := 0;
    if LForm.Designer.IsDesignMsg(LForm, TMsg) then
      Exit;
  end;

  if (LForm <> nil) and (not LForm.Released) then
  begin
    Wnd := FormToHWND(LForm);
    try
      InitialActionsOfPopups;
      try
        case uMsg of
          WM_NCHITTEST,
            WM_NCACTIVATE,
            WM_NCADDUPDATERECT,
            WM_NCMOUSEMOVE, WM_NCLBUTTONDOWN, WM_NCLBUTTONUP,
            WM_NCCALCSIZE, WM_NCPAINT:
            Result := WMNCMessages(LForm, uMsg, wParam, lParam);
          $B000 + 74: // CM_DESTROYHANDLE
            begin
              if (LForm.ClassName = 'TFormContainerForm') and (wParam = 1) then
              begin
                // IDE parent recreate
                SetParent(Wnd, GetDesktopWindow);
                SetWindowPos(Wnd, 0, $A000, $A000, 10, 10, SWP_NOSIZE or SWP_NOZORDER);
              end;
            end;
          WM_DESTROY:
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          WM_ACTIVATE:
            begin
              if not ((TFmxFormState.Recreating in LForm.FormState) or (LForm.FormStyle = TFormStyle.Popup) or
                (WindowHandleToPlatform(LForm.Handle).FDisableDeactivate)) then
              begin
                if LoWord(wParam) <> 0 then
                begin
                  if HiWord(wParam) = 0 then
                    LForm.Activate;
                  // If the window is minimized, then do nothing.
                end
                else
                begin
                  PrepareClosePopups;
                  LForm.Deactivate;
                  ClosePopupList;
                end;
              end;
              Result := 0;
            end;
          WM_MOUSEACTIVATE:
            begin
              if not (TFmxFormState.Recreating in LForm.FormState) then
              begin
                if (LForm.FormStyle = TFormStyle.Popup) then
                  Result := MA_NOACTIVATE
                else
                  Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
                // Default result if nothing happens
              end;
            end;
          WM_ERASEBKGND:
            begin
              Result := 1;
            end;
          WM_PAINT:
            begin
              Result := WMPaint(hwnd, uMsg, wParam, lParam);
            end;
          WM_DPICHANGED:
            begin
              WindowHandleToPlatform(LForm.Handle).DpiChanged(HiWord(wParam));
              Result := 0;
            end;
          WM_ADDUPDATERECT:
            begin
              ProcessUpdateMessages;
              if Supports(LForm, IPaintControl, PaintControl) then
                PaintControl.PaintRects(UpdateRects);
              WindowHandleToPlatform(LForm.Handle).UpdateLayer;
            end;
          WM_WINDOWPOSCHANGING:
            begin
              if ([csLoading, csDesigning] * LForm.ComponentState = [csLoading]) then
              begin
                if (LForm.Position in [TFormPosition.Default, TFormPosition.DefaultPosOnly]) and
                  (LForm.WindowState <> TWindowState.wsMaximized) then
                begin
                  PWindowPos(lParam)^.Flags := PWindowPos(lParam)^.Flags or SWP_NOMOVE;
                end;
                if (LForm.Position in [TFormPosition.Default, TFormPosition.DefaultSizeOnly]) and
                  (LForm.BorderStyle in [TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin]) then
                begin
                  PWindowPos(lParam)^.Flags := PWindowPos(lParam)^.Flags or SWP_NOSIZE;
                end;
              end;
              if (not ((PWindowPos(lParam)^.Flags and FlagZOrder) = FlagZOrder)) then
              begin
                if (Screen <> nil) and (LForm <> nil) and (not WindowHandleToPlatform(LForm.Handle).FDisableDeactivate)
                then
                begin
                  if (LForm.FormStyle = TFormStyle.Popup) then
                    ClosePopupList;
                end;
              end;
              if (not ((PWindowPos(lParam)^.Flags and SWP_NOSIZE) = SWP_NOSIZE)) then
                WindowHandleToPlatform(LForm.Handle).CorrectWindowSize(PWindowPos(lParam));
              Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
              if (TFmxFormState.InDesigner in LForm.FormState) and (TFmxFormState.WasNotShown in LForm.FormState) then
              begin
                TOpenForm(LForm).ShowInDesigner;
              end;
            end;
          WM_WINDOWPOSCHANGED:
            begin
              Placement.Length := SizeOf(TWindowPlacement);
              GetWindowPlacement(hwnd, Placement);
              if (Application.MainForm <> nil) and (LForm = Application.MainForm)
                and (Placement.showCmd = SW_SHOWMINIMIZED) then
              begin
                PlatformWin.MinimizeApp;
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
              end
              else
              begin
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
                if (PWindowPos(lParam)^.Flags and SWP_NOSIZE = 0) or
                  (PWindowPos(lParam)^.Flags and SWP_NOMOVE = 0) then
                begin
                  GetWindowRect(hwnd, R);
                  WindowHandleToPlatform(LForm.Handle).WndBounds := R;
                  R := WindowHandleToPlatform(LForm.Handle).Bounds;
                  LForm.SetBounds(R.Left, R.Top, R.Width, R.Height);
                end;
                if PWindowPos(lParam)^.Flags and SWP_FRAMECHANGED = SWP_FRAMECHANGED then
                  WindowHandleToPlatform(LForm.Handle).UpdateClientSize;
                { update state }
                PlatformWin.FDiableUpdateState := True;
                try
                  OldWindowState := LForm.WindowState;
                  Placement.Length := SizeOf(TWindowPlacement);
                  GetWindowPlacement(hwnd, Placement);
                  case Placement.showCmd of
                    SW_SHOWMINIMIZED:
                      LForm.WindowState := TWindowState.wsMinimized;
                    SW_SHOWMAXIMIZED:
                      LForm.WindowState := TWindowState.wsMaximized;
                  else
                    if csDesigning in LForm.ComponentState then
                    begin
                      { for using Metro-style interface in designer we set Maximized but we can change window size }
                      if LForm.WindowState <> TWindowState.wsMaximized then
                        LForm.WindowState := TWindowState.wsNormal;
                    end
                    else
                    begin
                      if not (TFmxFormState.WasNotShown in LForm.FormState) then
                        LForm.WindowState := TWindowState.wsNormal;
                    end;
                  end;
                  if OldWindowState <> LForm.WindowState then
                  begin
                    PostMessage(hwnd, WM_CLOSEMENU, 0, 0);
                    PrepareClosePopups;
                    ClosePopupList;
                  end;
                finally
                  PlatformWin.FDiableUpdateState := False;
                end;
                WMNCMessages(LForm, uMsg, wParam, lParam);
              end;
            end;
          WM_CLOSE:
            LForm.Close;
          WM_LBUTTONDOWN:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseDown(TMouseButton.mbLeft, MouseToShiftState(wParam), P.X, P.Y);
              HandleMouseGestures(LForm, uMsg, P.X, P.Y);
            end;
          WM_LBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseDown(TMouseButton.mbLeft, MouseToShiftState(wParam) + [ssDouble], P.X, P.Y);
            end;
          WM_LBUTTONUP:
            begin
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);
              if LForm.Border.IsSupported and WindowBorder.NCClick then
                Result := WMNCMessages(LForm, uMsg, wParam, lParam)
              else
              begin
                GetCursorPos(P);
                LastMousePos := ImpossibleMousePosition;
                ScaleMousePos(LForm.Handle, P);
                LForm.MouseUp(TMouseButton.mbLeft, MouseToShiftState(wParam), P.X, P.Y);
              end;
              HandleMouseGestures(LForm, uMsg, P.X, P.Y);
            end;
          WM_RBUTTONDOWN, WM_RBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              Obj := IControl(LForm.ObjectAtPoint(P));
              if (Obj <> nil) and (Obj.GetObject <> nil) and not (csDesigning in Obj.GetObject.ComponentState) then
              begin
                Obj.SetFocus;
                MenuDisplayed := Obj.ShowContextMenu(P);
              end
              else
                MenuDisplayed := False;
              if not MenuDisplayed then
              begin
                ScaleMousePos(LForm.Handle, P);
                if uMsg = WM_RBUTTONDBLCLK then
                  LForm.MouseDown(TMouseButton.mbRight, MouseToShiftState(wParam) + [ssDouble], P.X, P.Y)
                else
                  LForm.MouseDown(TMouseButton.mbRight, MouseToShiftState(wParam), P.X, P.Y);
              end;
            end;
          WM_RBUTTONUP:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseUp(TMouseButton.mbRight, MouseToShiftState(wParam), P.X, P.Y);
            end;
          WM_MBUTTONDOWN:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseDown(TMouseButton.mbMiddle, MouseToShiftState(wParam), P.X, P.Y);
            end;
          WM_MBUTTONUP:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseUp(TMouseButton.mbMiddle, MouseToShiftState(wParam), P.X, P.Y);
            end;
          WM_MBUTTONDBLCLK:
            begin
              GetCursorPos(P);
              LastMousePos := ImpossibleMousePosition;
              ScaleMousePos(LForm.Handle, P);
              LForm.MouseDown(TMouseButton.mbMiddle, MouseToShiftState(wParam) + [ssDouble], P.X, P.Y);
            end;
          WM_MENUSELECT:
            begin
              MenuIdentifier := wParam and $FFFF;
              MenuHandle := GetSubMenu(lParam, MenuIdentifier);
              if MenuHandle = 0 then
              begin
                if (lParam <> 0) and PlatformWin.FHMenuIdMap.TryGetValue(MenuIdentifier, FMXMenuHandle) then
                begin
                  if PlatformWin.FHMenuMap.TryGetValue(TFmxHandle(FMXMenuHandle), LMenuItem) and
                    (LMenuItem.FMXMenuItem <> nil) then
                    Application.Hint := LMenuItem.FMXMenuItem.Hint
                  else
                    Application.Hint := string.Empty;
                end
                else
                  Application.Hint := string.Empty;
              end;
            end;
          WM_MOUSEMOVE:
            begin
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);
              if LForm.Border.IsSupported then
              begin
                if WindowBorder.NCClick then
                  Result := WMNCMessages(LForm, uMsg, wParam, lParam)
                else
                begin
                  WindowBorder.MouseLeave;
                  GetCursorPos(P);
                  if P <> LastMousePos then
                  begin
                    LastMousePos := P;
                    ScaleMousePos(LForm.Handle, P);
                    LForm.MouseMove(MouseToShiftState(wParam), P.X, P.Y);
                  end;
                end;
              end
              else
              begin
                GetCursorPos(P);
                if P <> LastMousePos then
                begin
                  LastMousePos := P;
                  ScaleMousePos(LForm.Handle, P);
                  LForm.MouseMove(MouseToShiftState(wParam), P.X, P.Y);
                end;
              end;
              tme.cbSize := SizeOf(tme);
              tme.hwndTrack := hwnd;
              tme.dwFlags := TME_LEAVE;
              tme.dwHoverTime := HOVER_DEFAULT;
              TrackMouseEvent(tme);
              HandleMouseGestures(LForm, uMsg, P.X, P.Y);
            end;
          WM_MOUSELEAVE:
            begin
              WindowBorder := TWindowBorderWin(LForm.Border.WindowBorder);
              if LForm.Border.IsSupported and WindowBorder.NCClick then
                Result := WMNCMessages(LForm, uMsg, wParam, lParam)
              else
                LForm.MouseLeave;
            end;
          WM_MOUSEWHEEL:
            begin
              H := DispatchMouseWheelToPopups;

              if not H then
                LForm.MouseWheel(KeysToShiftState(wParam),
                  TSmallPoint(Cardinal(wParam)).Y, H);

              Result := Integer(H = True);
            end;
          WM_GETDLGCODE:
            begin
              Result := DLGC_WANTTAB or dlgc_WantArrows or DLGC_WANTCHARS;
            end;
          WM_CHAR:
            begin
              Ch := WideChar(wParam);
              Key := 0;
              LForm.KeyDown(Key, Ch, KeyDataToShiftState(lParam));
              LForm.KeyUp(Key, Ch, KeyDataToShiftState(lParam));
              Result := 0;
            end;
          WM_KEYDOWN,
          WM_SYSKEYDOWN:
            begin
              // Check if this key translates to a WM_CHAR/WM_SYSCHAR message
              // and if it does, pass KeyDown with character code
              // and clear the original WM_CHAR from the queue
              Msg.hwnd := hwnd;
              Msg.Message := uMsg;
              Msg.wParam := wParam;
              Msg.lParam := lParam;

              Result := 0;

              if uMsg = WM_SYSKEYDOWN then
              begin
                CharMsg := WM_SYSCHAR;
                DeadCharMsg := WM_SYSDEADCHAR;
              end
              else
              begin
                CharMsg := WM_CHAR;
                DeadCharMsg := WM_DEADCHAR;
              end;

              LastKeyIsDeadKey := False;
              if PeekMessage(Msg, hwnd, DeadCharMsg, DeadCharMsg, PM_NOREMOVE + PM_NOYIELD) then
                LastKeyIsDeadKey := True
              else if TranslateMessage(Msg) then
              begin
                if PeekMessage(Msg, hwnd, CharMsg, CharMsg, PM_REMOVE) then
                begin
                  CurrentChar(Msg, Key, Ch, Shift);
                   // clear duplicate WM_CHAR
                  PeekMessage(Msg, hwnd, CharMsg, CharMsg, PM_REMOVE);
                  LForm.KeyDown(Key, Ch, Shift);
                end
                else
                begin
                  Key := wParam;
                  Ch := #0;
                  LForm.KeyDown(Key, Ch, KeyDataToShiftState(lParam));
                end;
              end;

              // always let the system handle system shortcuts
              if uMsg = WM_SYSKEYDOWN then
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_DEADCHAR:
            begin
              PeekMessage(Msg, hwnd, WM_DEADCHAR, WM_DEADCHAR, PM_REMOVE);
              PeekMessage(Msg, hwnd, WM_CHAR, WM_CHAR, PM_REMOVE);
            end;
          WM_KEYUP:
            begin
              Ch := #0;
              Key := wParam;
              Shift := KeyDataToShiftState(lParam);
              if LastKeyIsDeadKey then
              begin
                Result := 0;
              end
              else
              begin
                Msg.hwnd := hwnd;
                Msg.Message := WM_KEYDOWN;
                Msg.wParam := wParam;
                Msg.lParam := Msg.lParam and $7FFFFFFF;
                if TranslateMessage(Msg) then
                  if PeekMessage(Msg, hwnd, WM_CHAR, WM_CHAR, PM_REMOVE) then
                  begin
                    CurrentChar(Msg, Key, Ch, Shift);
                  end;
                LForm.KeyUp(Key, Ch, Shift);
                Result := 0;
              end
            end;
          WM_SYSKEYUP:
            begin
              if (wParam = VK_MENU) or (wParam = VK_F10) then
              begin
                LForm.EnterMenuLoop;
                Result := 0;
              end
              else
                Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_RELEASEFORM:
            begin
              LForm.Free;
            end;
          { IME }
          WM_INPUTLANGCHANGE:
            begin
              // Result := SetIMECompositionWndPosition(LForm, uMsg, wParam, lParam);
              // OnInputLangChange();
            end;
          WM_IME_SETCONTEXT:
            begin
              // Request the candidate windows only
              Result := DefWindowProc(hwnd, uMsg, wParam, lParam and ISC_SHOWUIALLCANDIDATEWINDOW);
            end;
          WM_IME_STARTCOMPOSITION:
            begin
              // InitCompStringData();
              // *trapped = true;
              SetIMEWndPosition(LForm, uMsg, wParam, lParam);
              Result := WMImeStartComposition(LForm, hwnd, uMsg, wParam, lParam);
            end;
          WM_IME_COMPOSITION:
            begin
              SetIMEWndPosition(LForm, uMsg, wParam, lParam);
              Result := WMImeComposition(LForm, uMsg, wParam, lParam);
            end;
          WM_IME_ENDCOMPOSITION:
            begin
              SetIMEWndPosition(LForm, uMsg, wParam, lParam);
              if (LForm.Focused <> nil) and Supports(LForm.Focused, ITextInput, TSObj) then
              begin
                if ((GetKeyboardLayout(0) and $FFFF) = $0412) and not TTextServiceWin(TSObj.GetTextService).FWorking
                then // Special support for Korean IME
                begin
                  if not TTextServiceWin(TSObj.GetTextService).FWorking then
                    SendMessage(Wnd, WM_IME_STARTCOMPOSITION, 0, 0);
                end
                else
                begin
                  // For Chinese Microsoft engkoo - pressed ESC, need to interrupt input
                  if ((GetKeyboardLayout(0) and $FFFF) = $0804) and TTextServiceWin(TSObj.GetTextService).FWorking then
                    TTextServiceWin(TSObj.GetTextService).InternalBreakIMEInput;
                  TTextServiceWin(TSObj.GetTextService).InternalEndIMEInput;
                end;
              end;
            end;
          WM_IME_NOTIFY:
            begin
              Result := WMImeNotify(LForm, hwnd, uMsg, wParam, lParam);
            end;
          // WM_IME_SETCONTEXT:
          // begin
                                              
          // end;
          { }
          WM_COMMAND:
            begin
              if wParam <> 0 then
              begin
                if PlatformWin.FHMenuIdMap.TryGetValue(wParam, FMXMenuHandle) then
                  if PlatformWin.FHMenuMap.TryGetValue(FMXMenuHandle, LMenuItem) and (LMenuItem.FMXMenuItem <> nil) then
                    TOpenMenuItem(LMenuItem.FMXMenuItem).Click;
              end;
              Result := 0;
            end;
          WM_GESTURENOTIFY:
            begin
              WMGestureNotify(LForm, uMsg, lParam);
              Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            end;
          WM_GESTURE:
            begin
              Result := WMGesture(LForm, uMsg, wParam, lParam);
            end;
          WM_TOUCH:
            begin
              GetCursorPos(P);
              Result := WMTouch(LForm, uMsg, wParam, lParam);
              ScreenToClient(Wnd, P);
            end;
          WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
            Result := SendMessage(lParam, CN_BASE + uMsg, wParam, lParam);
        else
          Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          // Default result if nothing happens
        end;
      finally
        FinalActionsOfPopups;
      end;
    except
      on E: Exception do
        Application.HandleException(E);
    end;
  end
  else { if LForm = nil }
  begin
    if (PlatformWin <> nil) and (hwnd = PlatformWin.FApplicationHWND) then
    begin
      case uMsg of
        WM_CLOSE:
          begin
            if (Application.MainForm <> nil) and not Application.MainForm.Released then
            begin
              try
                if Screen <> nil then
                  Screen.ActiveForm := nil;
                Application.MainForm.Close;
              except
                on E: Exception do
                  Application.HandleException(E);
              end;
              if not Application.MainForm.Released then
                Exit;
            end;
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
          end;
        WM_DESTROY:
          Application.Terminate;
        WM_ACTIVATEAPP:
          begin
            Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
            if BOOL(wParam) then
              PlatformWin.RestoreApp;
          end;
      else
        Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
      end;
    end
    else
      Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
  end;
  // Default result if nothing happens
end;

var
  FMAppClass: TWndClass = (
    style: 0;
    lpfnWndProc: @WndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TFMAppClass');

procedure TPlatformWin.CreateAppHandle;
var
  TempClass: TWndClass;
  P: PChar;
  ClassRegistered: Boolean;
  ModuleName: array [0 .. 255] of Char;
  AtomName: string;
  AppAtom: Word;
  LApplicationTitle: string;
begin
  GetModuleFileName(MainInstance, ModuleName, Length(ModuleName));
  P := AnsiStrRScan(ModuleName, '\');
  if P <> nil then
    StrCopy(ModuleName, P + 1);
  P := AnsiStrScan(ModuleName, '.');
  if P <> nil then
    P^ := #0;
  AtomName := ModuleName;
  AppAtom := GlobalFindAtom(PChar(AtomName));
  if AppAtom > 0 then
    FApplicationHWND := 0
  else
  begin
    CharLower(CharNext(ModuleName));
    FDefaultTitle := ModuleName;
    if Application <> nil then
      LApplicationTitle := Application.Title
    else
      LApplicationTitle := FDefaultTitle;
    FTitle := LApplicationTitle;
    FMAppClass.hInstance := hInstance;
    ClassRegistered := GetClassInfo(hInstance, FMAppClass.lpszClassName,
      TempClass);
    FMAppClass.hIcon := LoadIconW(MainInstance, PChar('MAINICON'));
    if not ClassRegistered or (TempClass.lpfnWndProc <> @WndProc) then
    begin
      if ClassRegistered then
        Winapi.Windows.UnregisterClass(FMAppClass.lpszClassName, hInstance);
      Winapi.Windows.RegisterClass(FMAppClass);
    end;
    FApplicationHWND := CreateWindowEx(WS_EX_WINDOWEDGE or WS_EX_APPWINDOW, FMAppClass.lpszClassName,
      PChar(LApplicationTitle), WS_POPUP or WS_GROUP, 0, 0, 0, 0, GetDesktopWindow, 0, hInstance, nil);
    Winapi.Windows.ShowWindow(FApplicationHWND, SW_SHOWNORMAL);
  end;
end;

function TPlatformWin.GetApplicationHWND: HWND;
begin
  if Assigned(FApplicationHWNDProc) then
    Result := FApplicationHWNDProc
  else
  begin
    if FApplicationHWND = 0 then
      CreateAppHandle;
    Result := FApplicationHWND;
  end;
end;

function TPlatformWin.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
var
  DesignerForm: IDesignerForm;
  IsDesignerForm: Boolean;
  WindowClass: TWndClass;
  LDropTarget: TWinDropTarget;
  Style, ExStyle: DWORD;
  Wnd: HWND;
  ParentWnd: HWND;
  WndClassName: string;
  LForm, LParentForm: TCommonCustomForm;

  function GetParentFormHandle: HWND;
  begin
    Result := 0;
    if LForm.ParentForm <> nil then
    begin
      if LForm.ParentForm.Handle = nil then
        raise EArgumentException.CreateFMT(SNotInstance, ['ParentForm.Handle'])at ReturnAddress;
      Result := FormToHWND(LForm.ParentForm);
    end;
  end;

begin
  if AForm = nil then
    raise EArgumentException.Create(SArgumentNil);
  LDropTarget := nil;
  LForm := AForm;
  Style := WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
  ExStyle := 0;
  WndClassName := 'FM' + LForm.ClassName;
  IsDesignerForm := TFmxFormState.InDesigner in LForm.FormState;
  if not GetClassInfo(hInstance, PChar(WndClassName), WindowClass) then
  begin
    FillChar(WindowClass, SizeOf(WindowClass), 0);
    WindowClass.style := CS_DBLCLKS or CS_HREDRAW or CS_VREDRAW;
    WindowClass.lpfnWndProc := @WndProc;
    WindowClass.cbClsExtra := 0;
    WindowClass.cbWndExtra := 0;
    WindowClass.hInstance := hInstance;
    WindowClass.hIcon := LoadIconW(MainInstance, PChar('MAINICON'));
    if csDesigning in LForm.ComponentState then
      WindowClass.hCursor := LoadCursorW(0, PChar(IDC_ARROW))
    else
      WindowClass.hCursor := 0;
    WindowClass.hbrBackground := GetStockObject(NULL_BRUSH);
    WindowClass.lpszMenuName := nil;
    WindowClass.lpszClassName := PChar(WndClassName);
    if Winapi.Windows.RegisterClass(WindowClass) = 0 then
      RaiseLastOSError;
  end;
  if (csDesigning in LForm.ComponentState) or IsDesignerForm then
  begin
    Style := Style or WS_CHILD;
    // Parent handle going to set in IDE.
    // Now set temporary value
    ParentWnd := GetDesktopWindow;
  end
  else
  begin
    case LForm.FormStyle of
      TFormStyle.Popup:
        begin
          Style := style or WS_POPUP;
          ExStyle := ExStyle or WS_EX_NOACTIVATE;
        end;
      TFormStyle.StayOnTop:
        begin
          ExStyle := ExStyle or WS_EX_TOPMOST;
        end;
    end;
    if LForm.Transparency then
    begin
      Style := style or WS_POPUP;
      ExStyle := ExStyle or WS_EX_LAYERED;
    end
    else
    begin
      case LForm.BorderStyle of
        TFmxFormBorderStyle.None:
          Style := Style or WS_POPUP or WS_SYSMENU;
        TFmxFormBorderStyle.Single, TFmxFormBorderStyle.ToolWindow:
          Style := Style or (WS_CAPTION or WS_BORDER);
        TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin:
          Style := Style or (WS_CAPTION or WS_THICKFRAME);
      end;
      if LForm.BorderStyle in [TFmxFormBorderStyle.ToolWindow, TFmxFormBorderStyle.SizeToolWin] then
        ExStyle := ExStyle or WS_EX_TOOLWINDOW;
      if LForm.BorderStyle <> TFmxFormBorderStyle.None then
      begin
        if TBorderIcon.biMinimize in LForm.BorderIcons then
          Style := Style or WS_MINIMIZEBOX;
        if TBorderIcon.biMaximize in LForm.BorderIcons then
          Style := Style or WS_MAXIMIZEBOX;
        if TBorderIcon.biSystemMenu in LForm.BorderIcons then
          Style := Style or WS_SYSMENU;
      end;
    end;
    // Use handle of parent form
    ParentWnd := GetParentFormHandle;
    // For Dialogs and Popups we used handle of active form
    if (ParentWnd = 0) and ((TFmxFormState.Modal in LForm.FormState) or (LForm.FormStyle = TFormStyle.Popup)) then
    begin
      if (Screen <> nil) and (Screen.ActiveForm <> nil) and not Screen.ActiveForm.Released then
        ParentWnd := FormToHWND(Screen.ActiveForm);
      if (ParentWnd = 0) then
      begin
        ParentWnd := GetActiveWindow;
        LParentForm := FindWindow(ParentWnd);
        if (LParentForm = nil) or LParentForm.Released then
          ParentWnd := 0;
      end;
    end;
    // If none parent then we use handle of Application
    if ParentWnd = 0 then
      ParentWnd := ApplicationHWND;
    // If none parent then we use handle of first form
    if (ParentWnd = 0) and (Screen <> nil) and (Screen.FormCount > 0) and (LForm <> Screen.Forms[0]) and
      not Screen.Forms[0].Released then
      ParentWnd := FormToHWND(Screen.Forms[0]);
  end;
  Wnd := CheckWinapiHandle(CreateWindowEx(ExStyle, WindowClass.lpszClassName, PChar(LForm.Caption), style,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), ParentWnd, 0,
    hInstance, nil));
  try
    SetProp(Wnd, MakeIntAtom(WindowAtom), THandle(LForm));
    try
      if not ((csDesigning in AForm.ComponentState) or Supports(AForm, IDesignerForm, DesignerForm)) then
      begin
        LDropTarget := TWinDropTarget.Create(nil);
        LDropTarget.Form := LForm;
      end;
      try
        if LDropTarget <> nil then
        begin
          SetProp(Wnd, MakeIntAtom(DropAtom), THandle(LDropTarget));
          RegisterDragDrop(Wnd, LDropTarget);
        end;
        Result := TWinWindowHandle.Create(LForm, Wnd);
        TWinWindowHandle(Result).FWinDropTarget := LDropTarget;
      except
        if LDropTarget <> nil then
          RevokeDragDrop(Wnd);
        raise;
      end;
    except
      FreeAndNil(LDropTarget);
      raise;
    end;
  except
    Winapi.Windows.DestroyWindow(Wnd);
    raise;
  end;
  TWinWindowHandle(Result).FWinDropTarget := LDropTarget;
end;

function TPlatformWin.CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
begin
  Result := FMX.Forms.Border.Win.CreateWindowBorder(AForm);
end;

procedure TPlatformWin.DestroyWindow(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
  DesignerForm: IDesignerForm;
begin
  HideWindow(AForm);
  Wnd := FormToHWND(AForm);
  if not ((csDesigning in AForm.ComponentState) or Supports(AForm, IDesignerForm, DesignerForm)) then
    RevokeDragDrop(Wnd);
  WindowHandleToPlatform(AForm.Handle).FWinDropTarget.Free;
  Winapi.Windows.DestroyWindow(Wnd);
end;

procedure TPlatformWin.ReleaseWindow(const AForm: TCommonCustomForm);
begin
end;

procedure TPlatformWin.InvalidateImmediately(const AForm: TCommonCustomForm);
begin
  InvalidateWindowRect(AForm, AForm.ClientRect);
end;

procedure TPlatformWin.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
var
  WR: TRect;
  Wnd: HWND;
  PaintControl: IPaintControl;
  UpdateRects: TUpdateRects;
  I: Integer;
begin
  if IntersectRect(R, TRectF.Create(0, 0, AForm.ClientWidth, AForm.ClientHeight)) then
  begin
    Wnd := FormToHWND(AForm);
    if AForm.Transparency and not (csDesigning in AForm.ComponentState) then
    begin
      PostMessage(Wnd, WM_ADDUPDATERECT, Integer(SmallPoint(Round(R.Left), Round(R.Top))),
        Integer(SmallPoint(Round(R.Right), Round(R.Bottom))));
    end
    else
    begin
      R := WindowHandleToPlatform(AForm.Handle).FormToWnd(R);
      if Supports(AForm, IPaintControl, PaintControl) and (PaintControl.ContextHandle <> 0) then
      begin
        // In Paint
        if not FInPaintUpdateRects.TryGetValue(AForm.Handle, UpdateRects) then
        begin
          SetLength(UpdateRects, 1);
          UpdateRects[0] := R;
          FInPaintUpdateRects.Add(AForm.Handle, UpdateRects);
          Exit;
        end
        else
          for I := 0 to High(UpdateRects) do
            if (UpdateRects[I] = R) or (UpdateRects[I].Contains(R.TopLeft) and UpdateRects[I].Contains(R.BottomRight))
            then
              Exit;
        SetLength(UpdateRects, Length(UpdateRects) + 1);
        UpdateRects[High(UpdateRects)] := R;
        FInPaintUpdateRects.AddOrSetValue(AForm.Handle, UpdateRects);
      end
      else
      begin
        WR := TRect.Create(Trunc(R.Left), Trunc(R.Top), Ceil(R.Right), Ceil(R.Bottom));
        Winapi.Windows.InvalidateRect(Wnd, @WR, False);
      end;
    end;
  end;
end;

function TPlatformWin.IsMenuBarOnWindowBorder: Boolean;
begin
  Result := True;
end;

procedure TPlatformWin.MinimizeApp;
var
  AnimationEnable: Boolean;

  function GetAnimation: Boolean;
  var
    Info: TAnimationInfo;
  begin
    Info.cbSize := SizeOf(TAnimationInfo);
    if SystemParametersInfo(SPI_GETANIMATION, Info.cbSize, @Info, 0) then
      Result := Info.iMinAnimate <> 0
    else
      Result := False;
  end;

  procedure SetAnimation(Value: Boolean);
  var
    Info: TAnimationInfo;
  begin
    Info.cbSize := SizeOf(TAnimationInfo);
    Info.iMinAnimate := Integer(BOOL(Value));
    SystemParametersInfo(SPI_SETANIMATION, Info.cbSize, @Info, 0);
  end;

begin
  AnimationEnable := GetAnimation;
  try
    SetAnimation(False);
    if Application.MainForm <> nil then
      SetWindowPos(ApplicationHWND, FormToHWND(Application.MainForm), Application.MainForm.Left,
        Application.MainForm.Top, Application.MainForm.Width, 0, SWP_SHOWWINDOW);
    DefWindowProc(ApplicationHWND, WM_SYSCOMMAND, SC_MINIMIZE, 0);
  finally
    SetAnimation(AnimationEnable);
  end;
end;

function TPlatformWin.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
begin
  Result := TRectF.Create(WindowHandleToPlatform(AForm.Handle).Bounds);
end;

function TPlatformWin.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  Result := AForm.Handle.Scale;
end;

procedure TPlatformWin.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
begin
  { for using Metro-style interface in designer we set Maximized but we can change window size }
  if AForm.WindowState in [TWindowState.wsNormal, TWindowState.wsMaximized] then
    WindowHandleToPlatform(AForm.Handle).Bounds := ARect.Round;
end;

procedure TPlatformWin.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
begin
  SetWindowText(FormToHWND(AForm), ACaption);
end;

procedure TPlatformWin.RegisterCanvasClasses;
begin
  if GlobalUseGPUCanvas then
    FMX.Canvas.GPU.RegisterCanvasClasses;
  FMX.Canvas.D2D.RegisterCanvasClasses;
  FMX.Canvas.GDIP.RegisterCanvasClasses;
end;

procedure TPlatformWin.UnhookTouchHandler(const AForm: TCommonCustomForm);
begin
  if TOSVersion.Check(6, 1) then
    UnregisterTouchWindow(FormToHWND(AForm));
end;

procedure TPlatformWin.UnregisterCanvasClasses;
begin
  if GlobalUseGPUCanvas then
    FMX.Canvas.GPU.UnregisterCanvasClasses;
  FMX.Canvas.D2D.UnregisterCanvasClasses;
  FMX.Canvas.GDIP.UnregisterCanvasClasses;
end;

procedure TPlatformWin.RegisterContextClasses;
begin
  FMX.Context.DX9.RegisterContextClasses;
  FMX.Context.DX11.RegisterContextClasses;
end;

procedure TPlatformWin.UnregisterContextClasses;
begin
  FMX.Context.DX9.UnregisterContextClasses;
  FMX.Context.DX11.UnregisterContextClasses;
end;

procedure TPlatformWin.ReleaseCapture(const AForm: TCommonCustomForm);
begin
  Winapi.Windows.ReleaseCapture;
end;

procedure TPlatformWin.SetCapture(const AForm: TCommonCustomForm);
begin
  Winapi.Windows.SetCapture(FormToHWND(AForm));
end;

function TPlatformWin.GetCaretWidth: Integer;
begin
  Result := 1;
end;

function TPlatformWin.GetClientSize(const AForm: TCommonCustomForm): TPointF;
begin
  Result := TSizeF.Create(WindowHandleToPlatform(AForm.Handle).ClientSize);
end;

procedure TPlatformWin.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
begin
  WindowHandleToPlatform(AForm.Handle).ClientSize := TSizeF.Create(ASize).Round;
end;

procedure TPlatformWin.HideWindow(const AForm: TCommonCustomForm);
begin
  SetWindowPos(FormToHWND(AForm), 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or
    SWP_NOACTIVATE);
end;

const
  ShowCommands: array [TWindowState] of Integer = (SW_SHOWNORMAL, SW_MINIMIZE, SW_SHOWMAXIMIZED);

procedure TPlatformWin.ShowWindow(const AForm: TCommonCustomForm);
const
  uFlags = SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE;
var
  Wnd, WndParent: HWND;
  nCmdShow: Integer;
  OldActiveForm: TCommonCustomForm;
  OldDisableDeactivate: Boolean;
  OldActiveHandle: TWinWindowHandle;
begin
  Wnd := FormToHWND(AForm);
  nCmdShow := ShowCommands[AForm.WindowState];
  if (AForm.FormStyle = TFormStyle.Popup) then
  begin
    nCmdShow := nCmdShow or SW_SHOWNOACTIVATE;
    OldDisableDeactivate := False;
    OldActiveHandle := nil;
    if Screen <> nil then
    begin
      OldActiveForm := Screen.ActiveForm;
      if OldActiveForm <> nil then
        OldActiveHandle := WindowHandleToPlatform(OldActiveForm.Handle);
    end;
    try
      if OldActiveHandle <> nil then
      begin
        OldDisableDeactivate := OldActiveHandle.FDisableDeactivate;
        OldActiveHandle.FDisableDeactivate := True;
      end;
      Winapi.Windows.ShowWindow(Wnd, nCmdShow);
      if OldActiveHandle <> nil then
        SetWindowPos(OldActiveHandle.Wnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
    finally
      if OldActiveHandle <> nil then
        OldActiveHandle.FDisableDeactivate := OldDisableDeactivate;
    end;
  end
  else
    Winapi.Windows.ShowWindow(Wnd, nCmdShow);

  if AForm.Transparency and not (csDesigning in AForm.ComponentState) then
    PostMessage(FormToHWND(AForm), WM_ADDUPDATERECT, Integer(SmallPoint(0, 0)),
      Integer(SmallPoint(AForm.Width, AForm.Height)));

  if AForm.FormStyle in [TFormStyle.StayOnTop, TFormStyle.Popup] then
  begin
    WndParent := GetParent(Wnd);
    if (WndParent = GetDesktopWindow) or (WndParent = 0) then
      SetWindowPos(Wnd, HWND_TOPMOST, 0, 0, 0, 0, uFlags)
    else
      SetWindowPos(Wnd, HWND_TOP, 0, 0, 0, 0, uFlags);
  end;
end;

procedure TPlatformWin.BringToFront(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  Wnd := FormToHWND(AForm);
  SetWindowPos(Wnd, HWND_TOP, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TPlatformWin.SendToBack(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  Wnd := FormToHWND(AForm);
  SetWindowPos(Wnd, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
end;

procedure TPlatformWin.Activate(const AForm: TCommonCustomForm);
var
  Wnd: HWND;
begin
  Wnd := FormToHWND(AForm);
  if (not IsWindowVisible(Wnd)) or (IsIconic(Wnd)) then
  begin
    if AForm.FormStyle = TFormStyle.Popup then
      Winapi.Windows.ShowWindow(Wnd, SW_RESTORE or SW_SHOWNOACTIVATE)
    else
      Winapi.Windows.ShowWindow(Wnd, SW_RESTORE);
  end;
  if AForm.FormStyle <> TFormStyle.Popup then
    Winapi.Windows.SetActiveWindow(Wnd);
end;

procedure TPlatformWin.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
var
  Wnd: HWND;

  procedure DoSetState(const AState: TWindowState);
  begin
    if AForm.FormStyle = TFormStyle.Popup then
      Winapi.Windows.ShowWindow(Wnd, ShowCommands[AState] or SW_SHOWNOACTIVATE)
    else
    begin
      if (Application.MainForm = AForm) and (AState = TWindowState.wsMinimized) then
        Winapi.Windows.ShowWindow(ApplicationHWND, ShowCommands[AState])
      else
        Winapi.Windows.ShowWindow(Wnd, ShowCommands[AState])
    end;
  end;

begin
  if AForm.Visible and not FDiableUpdateState then
  begin
    Wnd := FormToHWND(AForm);
    if AForm.FullScreen then
      try
        FDiableUpdateState := True;
        AForm.WindowState := TWindowState.wsMaximized;
        if not Winapi.Windows.IsZoomed(Wnd) then
          DoSetState(TWindowState.wsMaximized);
      finally
        FDiableUpdateState := False;
      end
    else
      DoSetState(AState);
  end;
end;

function TPlatformWin.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
var
  WindowList: Pointer;
  AppService: IFMXApplicationService;
begin
  Result := mrNone;
  if GetCapture <> 0 then
    SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  Winapi.Windows.ReleaseCapture;
  AForm.HandleNeeded;
  WindowList := DisableTaskWindows(FormToHWND(AForm));
  try
    AForm.Show;
    AForm.ModalResult := mrNone;    
    SetActiveWindow(FormToHWND(AForm));
    SetFocus(FormToHWND(AForm));
    Screen.ActiveForm := AForm;
    AppService := IFMXApplicationService(TPlatformServices.Current.GetPlatformService(IFMXApplicationService));
    repeat
      if not Application.HandleMessage then
        AppService.WaitMessage;
      if Application.Terminated then
        AForm.ModalResult := mrCancel;
      if AForm.ModalResult <> mrNone then
      begin
        Result := AForm.ModalResult;
        AForm.Hide;
        AForm.CloseModal;
      end;
    until Result <> mrNone;

  finally
    EnableTaskWindows(WindowList);
  end;

  Result := AForm.ModalResult;
end;

function TPlatformWin.ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  P: TPoint;
begin
  P := (Point * WindowHandleToPlatform(AForm.Handle).Scale).Round;
  Winapi.Windows.ClientToScreen(FormToHWND(AForm), P);
  Result := TPointF.Create(P);
end;

function TPlatformWin.ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  P: TPoint;
begin
  P := Point.Round;
  Winapi.Windows.ScreenToClient(FormToHWND(AForm), P);
  Result := TPointF.Create(P) / WindowHandleToPlatform(AForm.Handle).Scale;
end;

{ Menus }

procedure TPlatformWin.StartMenuLoop(const AView: IMenuView);
var
  FirstLoop: Boolean;

  procedure EndLoop;
  var
    View: IMenuView;
  begin
    View := AView;
    while View <> nil do
    begin
      View.Loop := False;
      View.Selected := nil;
      View := View.ParentView;
    end;
  end;

  function ContinueLoop: Boolean;
  begin
    Result := AView.Loop;
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

  procedure SelectNextMenuItem(const AView: IMenuView; const Backward: Boolean);
  begin
    if AView <> nil then
      if Backward then
      begin
        if (AView.Selected = nil) or not BackwardSelectNextMenuItem(AView, AView.Selected.Index - 1, 0) then
          SelectLastMenuItem(AView);
        // otherwise nothing
      end
      else
      begin
        if (AView.Selected = nil) or not ForwardSelectNextMenuItem(AView, AView.Selected.Index + 1,
          AView.GetItemsCount - 1) then
          SelectFirstMenuItem(AView);
        // otherwise nothing
      end;
    // otherwise nothing
  end;

  function ParentWindow(AView: IMenuView): HWND;
  var
    Obj: TFmxObject;
    Form: TCommonCustomForm;
  begin
    Result := 0;
    if (AView <> nil) and (AView.Parent <> nil) then
    begin
      Obj := AView.Parent;
      while (Obj <> nil) and (not (Obj is TCommonCustomForm)) do
        Obj := Obj.Parent;
      if Obj is TCommonCustomForm then
      begin
        Form := TCommonCustomForm(Obj);
        while (Form <> nil) and (IsPopup(Form)) do
          Form := Form.ParentForm;
        if (Form <> nil) and (Form.Handle <> nil) then
          Result := WindowHandleToPlatform(Form.Handle).Wnd;
      end;
    end;
  end;

var
  Msg: TMsg;
  WP: TPoint;
  ParentWnd: HWND;
  P: TPointF;
  InMenus: Boolean;
  CurrentView, NewView: IMenuView;
  Obj: IControl;
  TimerID: THandle;
begin
  AView.Loop := True;
  TimerID := SetTimer(0, 0, 50, nil);
  try
    FirstLoop := True;
    while ContinueLoop do
    begin
                                                         
      if FirstLoop then
        FirstLoop := False
      else
        WaitMessage;

      while ContinueLoop and PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) do
      begin
        case Msg.Message of
          WM_WINDOWPOSCHANGING:
            begin
              EndLoop;
              Exit;
            end;
          WM_QUIT { , WM_NCLBUTTONDOWN..WM_NCMBUTTONDBLCLK } :
            begin
              EndLoop;
              Continue;
            end;
          WM_TIMER:
            begin
              TranslateMessage(Msg);
            end;
        end;
        if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
        begin
          case Msg.Message of
            WM_CLOSEMENU:
              EndLoop;
            WM_NCMOUSEMOVE, WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN, WM_NCLBUTTONUP:
              begin
                case Msg.Message of
                  WM_NCMOUSEMOVE:
                    begin
                      { Handle MouseOver }
{$IFDEF CPUX64}
                      WP := SmallPointToPoint(TSmallPoint(Cardinal(Msg.LPARAM)));
{$ELSE}
                      WP := SmallPointToPoint(TSmallPoint(Msg.LPARAM));
{$ENDIF}
                      P := PointF(WP.X, WP.Y);
                      Obj := AView.ObjectAtPoint(P);
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                      { Find top level menu }
                      CurrentView := AView;
                      while CurrentView.ParentView <> nil do
                        CurrentView := CurrentView.ParentView;
                      { Check all items }
                      while CurrentView <> nil do
                      begin
                        Obj := CurrentView.ObjectAtPoint(P);
                        if (Obj <> nil) and (Obj.GetObject is TMenuItem) and not (TMenuItem(Obj.GetObject).IsSelected)
                        then
                        begin
                          if (CurrentView <> AView) then
                          begin
                            NewView := AView;
                            while NewView <> CurrentView do
                            begin
                              NewView.Loop := False;
                              NewView := NewView.ParentView;
                            end;
                            TOpenMenuItem(Obj.GetObject).NeedPopup;
                            Exit;
                          end;
                        end;
                        CurrentView := CurrentView.ChildView;
                      end;
                      Continue;
                    end;
                  WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN:
                    begin
                      { Handle MouseOver if mouse over not menuitem }
{$IFDEF CPUX64}
                      WP := SmallPointToPoint(TSmallPoint(Cardinal(Msg.LPARAM)));
{$ELSE}
                      WP := SmallPointToPoint(TSmallPoint(Msg.LPARAM));
{$ENDIF}
                      P := PointF(WP.X, WP.Y);
                      Obj := AView.ObjectAtPoint(P);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                      { Menus }
                      if (Obj <> nil) and (Obj.GetObject is TMenuItem) then
                      begin
                        if not (TMenuItem(Obj.GetObject).IsSelected) and TMenuItem(Obj.GetObject).HavePopup then
                          TOpenMenuItem(Obj.GetObject).NeedPopup
                        else
                        begin
                          EndLoop;
                          TOpenMenuItem(Obj.GetObject).Click;
                        end;
                      end
                      else
                      begin
                        CurrentView := AView;
                        InMenus := False;
                        while (CurrentView <> nil) and not InMenus do
                        begin
                          if not (CurrentView.IsMenuBar) and (CurrentView.ObjectAtPoint(P) <> nil) then
                            InMenus := True;
                          CurrentView := CurrentView.ParentView;
                        end;
                        if not InMenus then
                          EndLoop;
                      end;
                    end;
                  WM_NCLBUTTONUP:
                    begin
                      { Handle MouseOver if mouse over not menuitem }
{$IFDEF CPUX64}
                      WP := SmallPointToPoint(TSmallPoint(Cardinal(Msg.LPARAM)));
{$ELSE}
                      WP := SmallPointToPoint(TSmallPoint(Msg.LPARAM));
{$ENDIF}
                      P := PointF(WP.X, WP.Y);
                      Obj := AView.ObjectAtPoint(P);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                    end;
                end;
              end;
            WM_MOUSEFIRST .. WM_MOUSELAST:
              begin
                { Handle MouseOver if mouse over not menuitem }
{$IFDEF CPUX64}
                WP := SmallPointToPoint(TSmallPoint(Cardinal(Msg.LPARAM)));
{$ELSE}
                WP := SmallPointToPoint(TSmallPoint(Msg.LPARAM));
{$ENDIF}
                Winapi.Windows.ClientToScreen(Msg.HWND, WP);
                case Msg.Message of
                  WM_MOUSEMOVE:
                    begin
                      TranslateMessage(Msg);
                      DispatchMessage(Msg);
                      Continue;
                    end;
                  WM_LBUTTONDOWN, WM_RBUTTONDOWN:
                    begin
                      P := PointF(WP.X, WP.Y);
                      Obj := AView.ObjectAtPoint(P);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                      { Menus }
                      if (Obj <> nil) and (Obj.GetObject is TMenuItem) then
                      begin
                        if not (TMenuItem(Obj.GetObject).IsSelected) and TMenuItem(Obj.GetObject).HavePopup then
                          TOpenMenuItem(Obj.GetObject).NeedPopup
                        else
                        begin
                          EndLoop;
                          TOpenMenuItem(Obj.GetObject).Click;
                        end;
                      end
                      else
                      begin
                        CurrentView := AView;
                        InMenus := False;
                        while (CurrentView <> nil) and not InMenus do
                        begin
                          if not (CurrentView.IsMenuBar) and (CurrentView.ObjectAtPoint(P) <> nil) then
                            InMenus := True;
                          CurrentView := CurrentView.ParentView;
                        end;
                        if not InMenus then
                        begin
                          EndLoop;
                          ParentWnd := ParentWindow(AView);
                          if ParentWnd <> 0 then
                          begin
                            // Redirect messages to the parent form
                            Winapi.Windows.ScreenToClient(ParentWnd, WP);
                            Msg.LPARAM := Cardinal(PointToSmallPoint(WP));
                            PostMessage(ParentWnd, Msg.Message, Msg.wParam, Msg.LPARAM);
                          end;
                        end;
                      end;
                    end;
                  WM_LBUTTONUP, WM_RBUTTONUP:
                    begin
                      P := PointF(WP.X, WP.Y);
                      Obj := AView.ObjectAtPoint(P);
                      if (Obj <> nil) and not (Obj is TMenuItem) then
                      begin
                        TranslateMessage(Msg);
                        DispatchMessage(Msg);
                        Continue;
                      end;
                    end;
                end;
              end;
            WM_KEYFIRST .. WM_KEYLAST:
              if (GetKeyState(VK_LBUTTON) >= 0) then
                case Msg.Message of
                  WM_KEYDOWN, WM_SYSKEYDOWN:
                    case Msg.wParam of
                      VK_RETURN:
                        begin
                          if AView.Selected <> nil then
                          begin
                            if AView.Selected.HavePopup then
                              AView.Selected.NeedPopup
                            else
                            begin
                              TOpenMenuItem(AView.Selected).Click;
                              EndLoop;
                            end;
                          end
                          else
                            EndLoop;
                        end;
                      VK_ESCAPE:
                        begin
                          AView.Selected := nil;
                          Exit;
                        end;
                      VK_MENU, VK_F10:
                        EndLoop;
                      VK_LEFT, VK_RIGHT:
                          if AView.IsMenuBar then
                            SelectNextMenuItem(AView, Msg.wParam = VK_LEFT)
                          else if AView.ParentView <> nil then
                            if (AView.Selected <> nil) and AView.Selected.HavePopup and (Msg.wParam = VK_RIGHT) then
                              AView.Selected.NeedPopup
                            else if AView.ParentView.IsMenuBar then
                            begin
                              AView.Loop := False;
                              SelectNextMenuItem(AView.ParentView, Msg.wParam = VK_LEFT);
                              if AView.ParentView.Selected <> nil then
                                AView.ParentView.Selected.NeedPopup;
                              Exit;
                            end
                            else
                              AView.Loop := False;
                      VK_UP, VK_DOWN:
                          if AView.IsMenuBar then
                          begin
                            if (AView.Selected <> nil) and (Msg.wParam = VK_DOWN) then
                              AView.Selected.NeedPopup;
                          end
                          else
                            SelectNextMenuItem(AView, Msg.wParam = VK_UP);
                    end;
                end;
          else
            TranslateMessage(Msg);
            DispatchMessage(Msg);
          end;
        end;
      end;
    end;
  finally
    KillTimer(0, TimerID);
    AView.Loop := False;
    Winapi.Windows.ReleaseCapture;
  end;
end;

function TPlatformWin.SupportsDefaultSize(const AComponent: TComponentKind): Boolean;
begin
  Result := AComponent = TComponentKind.Calendar;
end;

procedure TPlatformWin.ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
begin
  FMX.Helpers.Win.ShortCutToKey(ShortCut, Key, Shift);
end;

function TPlatformWin.ShortCutToText(ShortCut: TShortCut): string;
begin
  Result := FMX.Helpers.Win.ShortCutToText(ShortCut);
end;

function TPlatformWin.TextToShortCut(Text: string): Integer;
begin
  Result := FMX.Helpers.Win.TextToShortCut(Text);
end;

procedure TPlatformWin.ThreadSync(var Msg: TMessage);
begin
  if Msg.Msg = WM_NULL then
  begin
    CheckSynchronize;
    Msg.Result := 0;
  end
  else
    Msg.Result := DefWindowProc(FThreadSyncHandle, Msg.Msg, Msg.wParam, Msg.LPARAM);
end;

{ OS Menu }

procedure TPlatformWin.RemoveChildHandles(const AMenu: IItemsContainer);
var
  I: Integer;
  ItemsContainer: IItemsContainer;
begin
  for I := 0 to AMenu.GetItemsCount - 1 do
    if Supports(AMenu.GetItem(I), IItemsContainer, ItemsContainer) then
      RemoveChildHandles(ItemsContainer);
  DestroyMenuItem(AMenu);
end;

procedure TPlatformWin.RemoveRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Exclude(FEnabledInteractiveGestures, ARec);
end;

procedure TPlatformWin.RestoreApp;
var
  I: Integer;
  LWND: HWND;
begin
  if Screen.ActiveForm <> nil then
    Screen.ActiveForm.Activate
  else
  begin
    LWND := GetActiveWindow;
    if LWND <> 0 then
      for I := 0 to Screen.FormCount - 1 do
        if FormToHWND(Screen.Forms[I]) = LWND then
        begin
          Screen.Forms[I].Activate;
          Exit;
        end;
    if Application.MainForm <> nil then
      Application.MainForm.Activate;
  end;
end;

procedure TPlatformWin.RemoveMenuFromMaps(MenuHandle: TFmxHandle);
var
  LPair: TPair<Integer, TFmxHandle>;
begin
  FHMenuMap.Remove(MenuHandle);
  for LPair in FHMenuIdMap do
    if LPair.Value = MenuHandle then
    begin
      FreeUniqueMenuCommand(LPair.Key);
      FHMenuIdMap.Remove(LPair.Key);
      Break;
    end;
end;

procedure TPlatformWin.RegisterMenuWithId(const AnId: Integer; const AMenu: HMENU);
begin
  FHMenuIdMap.Add(AnId, AMenu);
end;

function TPlatformWin.GetAvailableIdForMenu: Integer;
begin
  Result := NewUniqueMenuCommand;
  if Result > 65535 then
  begin
    FreeUniqueMenuCommand(Result);
    raise EUnavailableMenuId.Create(SUnavailableMenuId);
  end;
end;

function TPlatformWin.AssignIdToMenu(ParentMenu, Menu: HMENU): Integer;
var
  LMenuItemInfo: tagMENUITEMINFOW;
begin
  Result := GetAvailableIdForMenu;

  FillChar(LMenuItemInfo, SizeOf(LMenuItemInfo), 0);
  LMenuItemInfo.cbSize := SizeOf(LMenuItemInfo);
  LMenuItemInfo.fMask := MIIM_ID;
  if not GetMenuItemInfo(ParentMenu, Menu, False, LMenuItemInfo) then
    RaiseLastOSError;
  LMenuItemInfo.wID := Result;
  try
    if not SetMenuItemInfo(ParentMenu, Menu, False, LMenuItemInfo) then
      RaiseLastOSError;
    RegisterMenuWithId(Result, Menu);
  except
    LMenuItemInfo.wID := 0;
    SetMenuItemInfo(ParentMenu, Menu, False, LMenuItemInfo);
    raise;
  end;
end;

procedure AddBitmapToMenu(ParentMenu, Menu: HMENU; Bitmap: HBITMAP);
var
  LMenuItemInfo: tagMENUITEMINFOW;
begin
  if Bitmap <> 0 then
  begin
    FillChar(LMenuItemInfo, SizeOf(LMenuItemInfo), 0);
    LMenuItemInfo.cbSize := SizeOf(LMenuItemInfo);
    LMenuItemInfo.fMask := MIIM_BITMAP;
    if not GetMenuItemInfo(ParentMenu, Menu, False, LMenuItemInfo) then
      RaiseLastOSError;
    LMenuItemInfo.hbmpItem := Bitmap;
    try
      if not SetMenuItemInfo(ParentMenu, Menu, False, LMenuItemInfo) then
        RaiseLastOSError;
    except
      LMenuItemInfo.hbmpItem := 0;
      SetMenuItemInfo(ParentMenu, Menu, False, LMenuItemInfo);
      raise;
    end;
  end;
end;

procedure RemoveBitmapFromMenu(ParentMenu, Menu: HMENU; const ADictionary: TDictionary<TFmxHandle, TWin32MenuInfo>);
var
  LMenuItemInfo: tagMENUITEMINFOW;
  LMenuItem: TWin32MenuInfo;
begin
  if (ADictionary <> nil) and ADictionary.TryGetValue(Menu, LMenuItem) then
    Menu := LMenuItem.MenuID;
  FillChar(LMenuItemInfo, SizeOf(LMenuItemInfo), 0);
  LMenuItemInfo.cbSize := SizeOf(LMenuItemInfo);
  LMenuItemInfo.fMask := MIIM_BITMAP;
  if not GetMenuItemInfo(ParentMenu, Menu, False, LMenuItemInfo) then
    RaiseLastOSError;
  if (LMenuItemInfo.hbmpItem <> HBITMAP(-1)) and (LMenuItemInfo.hbmpItem > HBMMENU_POPUP_MINIMIZE) then
  begin
    if not DeleteObject(LMenuItemInfo.hbmpItem) then
      RaiseLastOSError;
  end;
end;

procedure TPlatformWin.CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);

  function SetItemFlags(const AMenuItem: TMenuItem): Integer;
  begin
    Result := 0;
    if AMenuItem.Text = SMenuSeparator then
      Result := Result or MF_SEPARATOR
    else
    begin
      if AMenuItem.IsChecked then
        Result := Result or MF_CHECKED;

      if not AMenuItem.Enabled then
        Result := Result or MF_DISABLED;

      Result := Result or MF_STRING;
    end;
  end;

  procedure InsertItems(Parent: HMENU; Child: IItemsContainer; L: Integer);
  var
    I, Flags, VisibleCount: Integer;
    PopupMenu: HMENU;
    Native: INativeControl;
    Item: TMenuItem;
    SubChild: IItemsContainer;
    S: string;
    Bitmap: HBITMAP;
  begin
    if (Child <> nil) and (Child.GetObject is TMenuItem) then
    begin
      Bitmap := 0;
      Item := TMenuItem(Child.GetObject);
      if Item.Visible and
        Supports(Item, INativeControl, Native) then
      begin
        Flags := SetItemFlags(Item);
        VisibleCount := 0;
        for I := 0 to Child.GetItemsCount - 1 do
          if (Child.GetItem(I) is TMenuItem) and (TMenuItem(Child.GetItem(I)).Visible) then
            Inc(VisibleCount);
        if VisibleCount > 0 then
          Flags := Flags or MF_POPUP;

        Native.Handle := 0;
        PopupMenu := CreateMenu;
        if PopupMenu = 0 then
          RaiseLastOSError;
        try
          if L > 0 then
            S := ShortCutToText(Item.ShortCut)
          else
            S := '';
          if S <> '' then
            S := #9 + S;
          S := Item.Text + S;
          if AppendMenu(Parent, Flags, PopupMenu, PChar(S)) then
          begin
            Bitmap := ImageListToMenuBitmap(L = 0, Item.Images, Item.ImageIndex);
            if Bitmap = 0 then
              Bitmap := BitmapToMenuBitmap(L = 0, Item.Bitmap);
            if Bitmap <> 0 then
              try
                AddBitmapToMenu(Parent, PopupMenu, Bitmap);
              except
                DeleteObject(Bitmap);
                Bitmap := 0;
                raise;
              end;
            if VisibleCount > 0 then
              for I := 0 to Child.GetItemsCount - 1 do
                if Supports(Child.GetItem(I), IItemsContainer, SubChild) then
                  InsertItems(PopupMenu, SubChild, L + 1);
            Native.Handle := PopupMenu;
          end
          else
            RaiseLastOSError;
        except
          if Bitmap <> 0 then
            RemoveBitmapFromMenu(Parent, PopupMenu, FHMenuMap);
          DestroyMenu(PopupMenu);
          raise;
        end;
        FHMenuMap.Add(PopupMenu, TWin32MenuInfo.Create(AssignIdToMenu(Parent, PopupMenu), Item));
      end;
    end;
  end;

var
  Handle: HMENU;
  Wnd: HWND;
  I, VisibleCount: Integer;
  WindowBorder: TWindowBorderWin;
  Native: INativeControl;
  LWindowState: TWindowState;
begin
  if FCreateOSMenu then
    Exit;
  LWindowState := AForm.WindowState;
  try
    FCreateOSMenu := True;
    Wnd := FormToHWND(AForm);
    if Wnd <> 0 then
    begin
      VisibleCount := 0;
      Native := nil;
      if AMenu <> nil then
      begin
        RemoveChildHandles(AMenu);
        Supports(AMenu.GetObject, INativeControl, Native);
        if (not (csDesigning in AForm.ComponentState)) and AForm.Border.IsSupported then
        begin
          WindowBorder := TWindowBorderWin(AForm.Border.WindowBorder);
          WindowBorder.CreateOSMenu(AMenu);
        end
        else
        begin
          if (Native <> nil) and Native.HandleSupported then
          begin
            for I := 0 to AMenu.GetItemsCount - 1 do
              if AMenu.GetItem(I) is TMenuItem and TMenuItem(AMenu.GetItem(I)).Visible then
                Inc(VisibleCount);
          end;
        end;
      end;
      Handle := GetMenu(Wnd);
      if VisibleCount > 0 then
      begin
        if Handle = 0 then
        begin
          Native.Handle := 0;
          Handle := CreateMenu;
          if Handle = 0 then
            RaiseLastOSError;
        end;
        try
          for I := 0 to AMenu.GetItemsCount - 1 do
            if AMenu.GetItem(I) is TMenuItem and TMenuItem(AMenu.GetItem(I)).Visible then
              InsertItems(Handle, TMenuItem(AMenu.GetItem(I)), 0);
        except
          DestroyMenu(Handle);
          raise;
        end;
        SetMenu(Wnd, Handle);
      end
      else
      begin
        if Handle <> 0 then
        begin
          if not DestroyMenu(Handle) then
            RaiseLastOSError;
          SetMenu(Wnd, 0);
          Handle := 0;
        end;
      end;
      if Native <> nil then
        Native.Handle := Handle;
    end;
  finally
    if LWindowState <> AForm.WindowState then
      AForm.WindowState := LWindowState;
    FCreateOSMenu := False;
  end;
end;

procedure TPlatformWin.UpdateMenuBar;
begin

end;

procedure TPlatformWin.UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);
var
  P: TFmxObject;
  RootMenu: TFmxObject;
  NativeControl: INativeControl;
  Root: IItemsContainer;
begin
  if (AItem <> nil) and (AItem.GetObject is TFmxObject) then
    P := AItem.GetObject
  else
    Exit;
  RootMenu := nil;
  while P.Parent <> nil do
  begin
    if Supports(P.Parent, IContent) then
      P := P.Parent;
    if (P is TMenuBar) or (P is TMainMenu) then
      RootMenu := P;
    P := P.Parent;
  end;
  if (RootMenu <> nil) and
    (RootMenu.Root <> nil) and
    (RootMenu.Root.GetObject is TCommonCustomForm) and
    Supports(RootMenu, INativeControl, NativeControl) and
    NativeControl.HandleSupported and
    Supports(RootMenu, IItemsContainer, Root) then
    CreateOSMenu(TCommonCustomForm(RootMenu.Root.GetObject), Root);
end;

procedure TPlatformWin.HookTouchHandler(const AForm: TCommonCustomForm);
begin
  if TOSVersion.Check(6, 1) and TWinTouchGestureEngine.Supported(AForm) then
    RegisterTouchWindow(FormToHWND(AForm), TWF_WANTPALM);
end;

procedure TPlatformWin.DestroyMenuItem(const AItem: IItemsContainer);
var
  LForm: TCommonCustomForm;
  P: TFmxObject;
  Wnd: HWND;
  MenuHandle, Handle: TFmxHandle;
  Native: INativeControl;
  Root: IRoot;
begin
  if (not FInDestroyMenuItem) and
    (AItem <> nil) and
    (AItem.GetObject is TFmxObject) then
    P := AItem.GetObject
  else
    Exit;
  FInDestroyMenuItem := True;
  try
    Root := P.Root;
    if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
    begin
      LForm := TCommonCustomForm(Root.GetObject);
      if (P <> nil) and (Supports(P, INativeControl, Native)) and (Native.Handle <> 0) then
      begin
        Handle := Native.Handle;
        if not (csDesigning in LForm.ComponentState) and (LForm.Border.WindowBorder is TWindowBorderWin) and
          TWindowBorderWin(LForm.Border.WindowBorder).HandleExists(Handle) then
        begin
          TWindowBorderWin(LForm.Border.WindowBorder).RemoveHandle(Handle);
        end
        else
        begin
          if (Handle <> INVALID_HANDLE_VALUE) then
          begin
            Wnd := FormToHWND(LForm);
            MenuHandle := GetMenu(Wnd);
            if MenuHandle = Handle then
            begin
              CreateOSMenu(LForm, nil);
              SetMenu(Wnd, 0)
            end
            else
            begin
              if MenuHandle <> 0 then
                RemoveBitmapFromMenu(MenuHandle, Handle, FHMenuMap);
            end;
            if IsMenu(Handle) then
              CheckWinapiResult(DestroyMenu(Handle));
          end;
          RemoveMenuFromMaps(Handle);
        end;
        Native.Handle := 0
      end;
    end;
  finally
    FInDestroyMenuItem := False;
  end;
end;

{ Drag and Drop }

const
  SID_IDropTargetHelper = '{4657278B-411B-11d2-839A-00C04FD918D0}';
  CLSID_DragDropHelper: TGUID = (D1: $4657278A; D2: $411B; D3: $11D2; D4: ($83, $9A, $00, $C0, $4F, $D9, $18, $D0));

type
  IDropTargetHelper = interface(IUnknown)
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget: HWND; const dataObj: IDataObject; var pt: TPoint;
      dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function DragOver(var pt: TPoint; dwEffect: Longint): HRESULT; stdcall;
    function Drop(const dataObj: IDataObject; var pt: TPoint; dwEffect: Longint): HRESULT; stdcall;
    function Show(Show: BOOL): HRESULT; stdcall;
  end;

var
  FDropTargetHelper: IDropTargetHelper;
  FDataObj: IDataObject;

function TWinDropTarget.GetDataObject: TDragObject;
var
  FormatEtc: TFormatEtc;
  StgMedium: TStgMedium;
  str: string;
  Drop: HDrop;
  I, numFiles, buflen: Integer;
  buffer: MarshaledString;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FDataObj = nil then
    Exit;
  // get file name first
  FormatEtc.cfFormat := CF_HDROP;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.Tymed := TYMED_HGLOBAL;
  // get FireMonkey
  FormatEtc.cfFormat := CF_FMOBJECT;
  if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    Result := TDropSource(StgMedium.HGLOBAL).Data;
    Exit;
  end;
  // files
  str := '';
  FormatEtc.cfFormat := CF_HDROP;
  if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    try
      Drop := HDrop(GlobalLock(StgMedium.HGLOBAL));
      { Replace Text }
      numFiles := DragQueryFile(Drop, $FFFFFFFF, nil, 0);
      SetLength(Result.Files, numFiles);
      for I := 0 to numFiles - 1 do
      begin
        // Setting length of buffer plus 1 for string end char (#0)
        buflen := DragQueryFile(Drop, I, nil, 0) + 1;
        buffer := StrAlloc(buflen);
        DragQueryFile(Drop, I, buffer, buflen);
        Result.Files[I] := buffer;
        StrDispose(buffer);
        if I = 0 then
          Result.Data := Result.Files[0];
      end;
    finally
      GlobalUnlock(StgMedium.HGLOBAL);
      { Free the memory }
      ReleaseStgMedium(StgMedium);
    end;
    Exit;
  end;
  // get text
  FormatEtc.cfFormat := CF_UNICODETEXT;
  if FDataObj.GetData(FormatEtc, StgMedium) = S_OK then
  begin
    try
      { Lock the global memory handle to get a pointer to the data }
      str := PChar(GlobalLock(StgMedium.HGLOBAL));
      Result.Data := str;
    finally
      { Finished with the pointer }
      GlobalUnlock(StgMedium.HGLOBAL);
      { Free the memory }
      ReleaseStgMedium(StgMedium);
    end;
    Exit;
  end;
end;

function TWinDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HRESULT;
var
  Res: HRESULT;
begin
  try
    FDataObj := dataObj;
    Result := S_OK;
    dwEffect := DROPEFFECT_NONE;
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDropTargetHelper,
      FDropTargetHelper))) and (FDropTargetHelper <> nil) then
    begin
      Res := FDropTargetHelper.DragEnter(FormToHWND(Form), dataObj, pt, dwEffect);
      if (Failed(Res)) then
        FDropTargetHelper := nil;
    end;
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TWinDropTarget.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
var
  P: TPointF;
  Operation: TDragOperation;
begin
  Result := E_UNEXPECTED;
  try
    dwEffect := DROPEFFECT_NONE;
    P := PointF(pt.X, pt.Y);
    Operation := TDragOperation.None;
    Form.DragOver(GetDataObject, P, Operation);
    case Operation of
      TDragOperation.None:
        dwEffect := DROPEFFECT_NONE;
      TDragOperation.Move:
        dwEffect := DROPEFFECT_MOVE;
      TDragOperation.Copy:
        dwEffect := DROPEFFECT_COPY;
      TDragOperation.Link:
        dwEffect := DROPEFFECT_LINK;
    end;

    // do NOT translate the screen coordinates to form coordinates because
    // it seems that the drop target helper needs screen coordinates
    if FDropTargetHelper <> nil then
      FDropTargetHelper.DragOver(pt, dwEffect);
    Result := S_OK;
  except
    dwEffect := DROPEFFECT_NONE;
  end;
end;

function TWinDropTarget.DragLeave: HRESULT;
begin
  Form.DragLeave;
  if (FDropTargetHelper <> nil) then
    FDropTargetHelper.DragLeave;
  FDropTargetHelper := nil;
  FDataObj := nil;
  Result := S_OK;
end;

function TWinDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
  var dwEffect: Longint): HRESULT;
var
  P: TPointF;
begin
  Result := S_OK;
  try
    if (dataObj = nil) then
      Exit;
    P := PointF(pt.X, pt.Y);
    Form.DragDrop(GetDataObject, P);
    // do NOT translate the screen coordinates to form coordinates because
    // it seems that the drop target helper needs screen coordinates
    if (FDropTargetHelper <> nil) then
      FDropTargetHelper.Drop(dataObj, pt, dwEffect)
  finally
    FDataObj := nil;
    FDropTargetHelper := nil;
  end;
end;

{ TDropSource }

{ IDropSource }

function TDropSource.QueryContinueDrag(fEscapePressed: BOOL; grfKeyState: Integer): HRESULT;
var
  ContinueDrop: Boolean;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
  begin
    ContinueDrop := True;
    if ContinueDrop then
      Result := DRAGDROP_S_DROP
    else
      Result := DRAGDROP_S_CANCEL;
  end
  else
    Result := S_OK;
end;

function TDropSource.GiveFeedback(dwEffect: Integer): HRESULT;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

{ IDataObject }

function TDropSource.dAdvise(const FormatEtc: TFormatEtc; advf: Longint; const advsink: IAdviseSink;
  out dwConnection: Longint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.dUnadvise(dwConnection: Longint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TDropSource.EnumFormatEtc(dwDirection: Longint; out EnumFormatEtc: IEnumFormatEtc): HRESULT;
begin
  if (dwDirection = DATADIR_GET) then
    Result := OleRegEnumFormatEtc(IEnumFormatEtc, dwDirection, EnumFormatEtc)
  else
    Result := E_NOTIMPL;
end;

function TDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcout: TFormatEtc): HRESULT;
begin
  Result := DATA_S_SAMEFORMATETC;
end;

function TDropSource.EqualFormatEtc(FormatEtc1, FormatEtc2: TFormatEtc): Boolean;
begin
  Result := (FormatEtc1.cfFormat = FormatEtc2.cfFormat) and (FormatEtc1.ptd = FormatEtc2.ptd) and
    (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and (FormatEtc1.lindex = FormatEtc2.lindex) and
    (FormatEtc1.Tymed = FormatEtc2.Tymed)
end;

function TDropSource.FindFormatEtc(TestFormatEtc: TFormatEtc): Integer;
var
  I: Integer;
  Found: Boolean;
begin
  I := 0;
  Found := False;
  Result := -1;
  while (I < Length(Formats)) and not Found do
  begin
    Found := EqualFormatEtc(Formats[I].FormatEtc, TestFormatEtc);
    if Found then
      Result := I;
    Inc(I);
  end
end;

function TDropSource.HGlobalClone(HGLOBAL: THandle): THandle;
// Returns a global memory block that is a copy of the passed memory block.
var
  Size: LongWord;
  Data, NewData: PByte;
begin
  Size := GlobalSize(HGLOBAL);
  Result := GlobalAlloc(GPTR, Size);
  Data := GlobalLock(HGLOBAL);
  try
    NewData := GlobalLock(Result);
    try
      Move(Data, NewData, Size);
    finally
      GlobalUnlock(Result);
    end
  finally
    GlobalUnlock(HGLOBAL)
  end
end;

function TDropSource.RetrieveOwnedStgMedium(Format: TFormatEtc; var StgMedium: TStgMedium): HRESULT;
var
  I: Integer;
begin
  Result := E_INVALIDARG;
  I := FindFormatEtc(Format);
  if (I > -1) and Formats[I].OwnedByDataObject then
    Result := StgMediumIncRef(Formats[I].StgMedium, StgMedium, False)
end;

function TDropSource.StgMediumIncRef(const InStgMedium: TStgMedium; var OutStgMedium: TStgMedium;
  CopyInMedium: Boolean): HRESULT;
begin
  Result := S_OK;
  // Simply copy all fields to start with.
  OutStgMedium := InStgMedium;
  case InStgMedium.Tymed of
    TYMED_HGLOBAL:
      begin
        if CopyInMedium then
        begin
          // Generate a unique copy of the data passed
          OutStgMedium.HGLOBAL := HGlobalClone(InStgMedium.HGLOBAL);
          if OutStgMedium.HGLOBAL = 0 then
            Result := E_OUTOFMEMORY
        end
        else
          // Don't generate a copy just use ourselves and the copy previoiusly saved
          MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject) // Does increase RefCount
      end;
    TYMED_FILE:
      begin
        if CopyInMedium then
        begin
          OutStgMedium.lpszFileName := CoTaskMemAlloc(lstrLenW(InStgMedium.lpszFileName));
          // !!          StrCopyW(PChar(OutStgMedium.lpszFileName), PChar(InStgMedium.lpszFileName))
        end
        else
          MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject) // Does increase RefCount
      end;
    TYMED_ISTREAM:
                                              
      IUnknown(MySTGMEDIUM(OutStgMedium).stm)._AddRef;
    TYMED_ISTORAGE:
      IUnknown(MySTGMEDIUM(OutStgMedium).stg)._AddRef;
    TYMED_GDI:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED; // Don't know how to copy GDI objects right now
    TYMED_MFPICT:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED;
    // Don't know how to copy MetaFile objects right now
    TYMED_ENHMF:
      if not CopyInMedium then
        MySTGMEDIUM(OutStgMedium).UnkForRelease := Pointer(Self as IDataObject)
        // Does not increase RefCount
      else
        Result := DV_E_TYMED;
    // Don't know how to copy enhanced metafiles objects right now
  else
    Result := DV_E_TYMED
  end;

  // I still have to do this. The Compiler will call _Release on the above Self as IDataObject
  // casts which is not what is necessary.  The DataObject is released correctly.
  if (MySTGMEDIUM(OutStgMedium).UnkForRelease <> nil) and (Result = S_OK) then
    IUnknown(MySTGMEDIUM(OutStgMedium).UnkForRelease)._AddRef
end;

function TDropSource.GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium): HRESULT;
var
  Global: Cardinal;
  P: Pointer;
  TextData: string;
  B: TBitmap;
  BitmapHandle: HBITMAP;
begin
  FillChar(Medium, SizeOf(Medium), 0);
  Result := DV_E_FORMATETC;
  if QueryGetData(FormatEtcIn) <> S_OK then
    Exit;

  case FormatEtcIn.cfFormat of
    CF_UNICODETEXT:
      begin
        TextData := Data.Data.ToString;
        Global := GlobalAlloc(0, (Length(TextData) + 1) * 2);
        P := GlobalLock(Global);
        try
          Move(PChar(TextData)^, P^, GlobalSize(Global));
        finally
          GlobalUnlock(Global);
        end;
        Medium.Tymed := TYMED_HGLOBAL;
        Medium.HGLOBAL := Global;
        Result := S_OK;
      end;
    CF_BITMAP:
      begin
        BitmapHandle := 0;
        if Data.Data.IsType<TBitmap> then
          BitmapHandle := BitmapToWinBitmap(TBitmap(Data.Data.AsObject), TAlphaColorRec.White)
        else if Data.Data.IsType<TBitmapSurface> then
        begin
          B := TBitmap.Create;
          try
            B.Assign(TBitmapSurface(Data.Data.AsObject));
            BitmapHandle := BitmapToWinBitmap(B, TAlphaColorRec.White);
          finally
            B.Free;
          end;
        end;
        if BitmapHandle <> 0 then
        begin
          Medium.Tymed := TYMED_GDI;
          Medium.HBITMAP := BitmapHandle;
          Result := S_OK;
        end;
      end;
    CF_FMOBJECT:
      begin
        Medium.Tymed := TYMED_HGLOBAL;
        Medium.HGLOBAL := THandle(Self);
        Result := S_OK;
      end;
  end;
  if (Result <> S_OK) and (Formats <> nil) then
  begin
    Result := QueryGetData(FormatEtcIn);
    if (Result = S_OK) and (RetrieveOwnedStgMedium(FormatEtcIn, Medium) = E_INVALIDARG) then
      Result := E_UNEXPECTED
  end
end;

function TDropSource.GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TDropSource.QueryGetData(const FormatEtc: TFormatEtc): HRESULT;
var
  I: Integer;
begin
  Result := DV_E_FORMATETC;
  case FormatEtc.cfFormat of
    CF_UNICODETEXT:
      if not Data.Data.IsObject then
        Result := S_OK;
    CF_BITMAP:
      if Data.Data.IsObject and (Data.Data.IsType<TBitmap> or Data.Data.IsType<TBitmapSurface>) then
        Result := S_OK;
    CF_FMOBJECT:
      Result := S_OK;
  end;
  if Result <> S_OK then
  begin
    if Formats <> nil then
    begin
      I := 0;
      Result := DV_E_FORMATETC;
      while (I < Length(Formats)) and (Result = DV_E_FORMATETC) do
      begin
        if Formats[I].FormatEtc.cfFormat = FormatEtc.cfFormat then
        begin
          if (Formats[I].FormatEtc.dwAspect = FormatEtc.dwAspect) then
          begin
            if (Formats[I].FormatEtc.Tymed and FormatEtc.Tymed <> 0) then
              Result := S_OK
            else
              Result := DV_E_TYMED;
          end
          else
            Result := DV_E_DVASPECT;
        end
        else
          Result := DV_E_FORMATETC;
        Inc(I)
      end
    end
    else
      Result := E_UNEXPECTED;
  end;
end;

function TDropSource.CanonicalIUnknown(const TestUnknown: IUnknown): IUnknown;
// Uses COM object identity: An explicit call to the IUnknown::QueryInterface
// method, requesting the IUnknown interface, will always return the same
// pointer.
begin
  if TestUnknown <> nil then
  begin
    if Supports(TestUnknown, IUnknown, Result) then
      IUnknown(Result)._Release
      // Don't actually need it just need the pointer value
    else
      Result := TestUnknown
  end
  else
    Result := TestUnknown
end;

function TDropSource.SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: BOOL): HRESULT;
var
  Index: Integer;
begin
  // See if we already have a format of that type available.
  Index := FindFormatEtc(FormatEtc);
  if Index > -1 then
  begin
    // Yes we already have that format type stored.  Just use the TClipboardFormat
    // in the List after releasing the data
    ReleaseStgMedium(Formats[Index].StgMedium);
    FillChar(Formats[Index].StgMedium, SizeOf(Formats[Index].StgMedium), #0);
  end
  else
  begin
    // It is a new format so create a new TDataObjectInfo record and store it in
    // the Format array
    SetLength(Formats, Length(Formats) + 1);
    Formats[Length(Formats) - 1].FormatEtc := FormatEtc;
    Index := Length(Formats) - 1;
  end;
  // The data is owned by the TClipboardFormat object
  Formats[Index].OwnedByDataObject := True;

  if fRelease then
  begin
    // We are simply being given the data and we take control of it.
    Formats[Index].StgMedium := Medium;
    Result := S_OK
  end
  else
    // We need to reference count or copy the data and keep our own references
    // to it.
    Result := StgMediumIncRef(Medium, Formats[Index].StgMedium, True);

  // Can get a circular reference if the client calls GetData then calls
  // SetData with the same StgMedium.  Because the unkForRelease and for
  // the IDataObject can be marshalled it is necessary to get pointers that
  // can be correctly compared.
  // See the IDragSourceHelper article by Raymond Chen at MSDN.
  if MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease <> nil then
  begin
    if CanonicalIUnknown(Self) = CanonicalIUnknown(IUnknown(MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease)) then
    begin
      IUnknown(MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease)._Release;
      MySTGMEDIUM(Formats[Index].StgMedium).UnkForRelease := nil
    end;
  end;
end;

{ Platform DragDrop }

procedure TPlatformWin.AddRecognizer(const ARec: TInteractiveGesture; const AForm: TCommonCustomForm);
begin
  Include(FEnabledInteractiveGestures, ARec);
end;

procedure TPlatformWin.BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
var
  DropSource: TDropSource;
  DropEffect: Longint;
  DragSourceHelper: IDragSourceHelper;
  SHDRAGIMAGE: TSHDRAGIMAGE;
  DragMousePos, Offset: TPointF;
  Control: IControl;
begin
  DropSource := TDropSource.Create(nil);
  try
    DropSource.Data := Data;

    DragMousePos := GetMousePos;
    // CoCreateInstance takes too long to execute so mouse position becomes different when called later
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDragSourceHelper, DragSourceHelper)
      ))
      and (DragSourceHelper <> nil) then
    begin
      if Data.Source is TControl then
        Offset := TControl(Data.Source).AbsoluteToLocal(AForm.ScreenToClient(DragMousePos))
      else
        Offset := PointF((ABitmap.Width div 2), ABitmap.Height div 2);

      FillChar(SHDRAGIMAGE, SizeOf(SHDRAGIMAGE), 0);
      SHDRAGIMAGE.sizeDragImage.cx := ABitmap.Width;
      SHDRAGIMAGE.sizeDragImage.cy := ABitmap.Height;
      SHDRAGIMAGE.ptOffset.X := Round(Offset.X);
      SHDRAGIMAGE.ptOffset.Y := Round(Offset.Y);
      SHDRAGIMAGE.hbmpDragImage := BitmapToWinBitmap(ABitmap, True);
      if not Succeeded(DragSourceHelper.InitializeFromBitmap(@SHDRAGIMAGE, DropSource)) then
        DeleteObject(SHDRAGIMAGE.hbmpDragImage);
    end;

    if not IsThemeActive then
    begin
      SetCursor(crDrag);
      FDragAndDropActive := True;
      try
        DoDragDrop(DropSource, DropSource, DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE, DropEffect);
      finally
        FDragAndDropActive := False;
        SetCursor(crDefault);
      end;
    end
    else
      DoDragDrop(DropSource, DropSource, DROPEFFECT_LINK or DROPEFFECT_COPY or DROPEFFECT_MOVE, DropEffect);

    if (DropEffect = 0) and Supports(Data.Source, IControl, Control) then
      Control.DragEnd;

    if DragSourceHelper <> nil then
      DragSourceHelper := nil;
  finally
    DropSource.Free;
  end;
end;

{ Mouse }

procedure TPlatformWin.SetCursor(const ACursor: TCursor);
const
  CustomCursorMap: array [crSizeAll .. crNone] of PChar = (
    nil, nil, nil, nil, nil, IDC_SQLWAIT, IDC_MULTIDRAG, nil, nil, IDC_NODROP, IDC_DRAG, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, nil, nil);
const
  CursorMap: array [crSizeAll .. crNone] of PChar = (
    IDC_SIZEALL, IDC_HAND, IDC_HELP, IDC_APPSTARTING, IDC_NO, nil, nil, IDC_SIZENS, IDC_SIZEWE, nil, nil, IDC_WAIT,
    IDC_UPARROW, IDC_SIZEWE, IDC_SIZENWSE, IDC_SIZENS, IDC_SIZENESW, IDC_SIZEALL, IDC_IBEAM, IDC_CROSS, IDC_ARROW, nil);
var
  NewCursor: HCURSOR;
begin
  if not FDragAndDropActive then
  begin
    // We don't set cursor by default, when we create window. So we should use crArrow cursor by default.
    if (ACursor = crDefault) and not (csDesigning in Application.ComponentState) then
      FCursor := crArrow
    else
      FCursor := ACursor;

    if FCursor < 0 then
    begin
      if CustomCursorMap[FCursor] <> nil then
        NewCursor := LoadCursorW(HInstance, CustomCursorMap[FCursor])
      else
        NewCursor := LoadCursorW(0, CursorMap[FCursor]);
      Winapi.Windows.SetCursor(NewCursor);
    end;
  end;
end;

function TPlatformWin.GetCursor: TCursor;
begin
  Result := FCursor;
end;

function TPlatformWin.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
var
  LFSParam: TFullScreenParams;
begin
  Result := False;
  if FFullScreenSupport.TryGetValue(AForm, LFSParam) then
    Result := LFSParam.IsFullScreen;
end;

procedure TPlatformWin.SetFullScreen(const AForm: TCommonCustomForm;
  const AValue: Boolean);
var
  LFSParam: TFullScreenParams;
  LClean: TFullScreenParams;
begin
  if AValue and not (TFmxFormState.Showing in AForm.FormState) then
    AForm.Visible := True;
  FillChar(LFSParam, SizeOf(LFSParam), 0);
  if not FFullScreenSupport.TryGetValue(AForm, LFSParam) then
    FFullScreenSupport.Add(AForm, LFSParam);
  if AValue and (AForm.Visible or (TFmxFormState.Showing in AForm.FormState)) then
  begin
    LFSParam.WindowState := AForm.WindowState;
    LFSParam.BorderStyle := AForm.BorderStyle;
    if AForm.WindowState = TWindowState.wsNormal then
    begin
      LFSParam.Size := Point(AForm.Width, AForm.Height);
      LFSParam.Position := Point(AForm.Left, AForm.Top);
    end;
    FFullScreenSupport.Items[AForm] := LFSParam;
    if AForm.WindowState = TWindowState.wsMinimized then
      AForm.WindowState := TWindowState.wsMaximized;
    AForm.BorderStyle := TFmxFormBorderStyle.None;
    AForm.WindowState := TWindowState.wsMaximized;
  end
  else
  begin
    LClean := LFSParam;
    LClean.Clean;
    FFullScreenSupport.Items[AForm] := LClean;
    if (LFSParam.Size.X > 0) and (LFSParam.Size.Y > 0) then
    begin
      AForm.BorderStyle := LFSParam.BorderStyle;
      AForm.SetBounds(LFSParam.Position.X, LFSParam.Position.Y, LFSParam.Size.X, LFSParam.Size.Y);
      AForm.WindowState := LFSParam.WindowState;
    end;
  end;
end;

procedure TPlatformWin.SetShowFullScreenIcon(const AForm: TCommonCustomForm;
  const AValue: Boolean);
begin
end;

function TPlatformWin.GetMousePos: TPointF;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result := PointF(P.X, P.Y);
end;

{ Screen }

function TPlatformWin.GetScreenSize: TPointF;
var
  WR: TRect;
begin
  Winapi.Windows.GetWindowRect(GetDesktopWindow, WR);
  Result := PointF(WR.Right, WR.Bottom);
end;

function TPlatformWin.GetScreenScale: Single;
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    Result := GetDCScale(DC);
  finally
    ReleaseDC(0, DC);
  end;
end;

function TPlatformWin.GetScreenOrientation: TScreenOrientation;
begin
  Result := TScreenOrientation.Landscape;
end;

procedure TPlatformWin.SetScreenOrientation(AOrientations: TScreenOrientations);
begin
  // Not needed for Windows
end;

{ IFMXDeviceService }

function TPlatformWin.GetFeatures: TDeviceFeatures;
var
  Value: Integer;
begin
  Value := GetSystemMetrics(SM_DIGITIZER);
  if ((Value and NID_READY) = NID_READY) and (((Value and NID_MULTI_INPUT) = NID_MULTI_INPUT)) then
    Result := [TDeviceFeature.HasTouchScreen]
  else
    Result := [];
end;

function TPlatformWin.GetFirstWeekday: Byte;
const
  MondayOffset = 1;
var
  buffer: DWORD;
begin
  // On Windows zero index corresponds to Monday, so we need to add offset to match DayMonday = 1 in RTL
  if GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, @buffer, SizeOf(buffer) div SizeOf(Char)) > 0 then
    Result := buffer - Ord('0') + MondayOffset
  else
    Result := DayMonday;
end;

function TPlatformWin.GetModel: string;
begin
  Result := '';
end;

function TPlatformWin.GetDeviceClass: TDeviceInfo.TDeviceClass;
begin
  if (GetSystemMetrics(SM_TABLETPC) <> 0) and (GetSystemMetrics(SM_DIGITIZER) and NID_MULTI_INPUT = NID_MULTI_INPUT)
  then
    Result := TDeviceInfo.TDeviceClass.Tablet
  else
    Result := TDeviceInfo.TDeviceClass.Desktop;
end;

{ IFMXSystemInformationService }

function TPlatformWin.GetScrollingBehaviour: TScrollingBehaviours;
var
  Value: Integer;
begin
  Value := GetSystemMetrics(SM_DIGITIZER);
  if ((Value and NID_READY) = NID_READY) and (((Value and NID_MULTI_INPUT) = NID_MULTI_INPUT)) then
    Result := [TScrollingBehaviour.Animation, TScrollingBehaviour.TouchTracking]
  else
    Result := [];
end;

function TPlatformWin.GetMinScrollThumbSize: Single;
begin
  Result := 15;
end;

function TPlatformWin.GetMenuShowDelay: Integer;
begin
  if SystemParametersInfo(SPI_GETMENUSHOWDELAY, 0, @Result, 0) then
    Result := Result div 2
  else
    Result := 0;
end;

function TPlatformWin.GetCurrentLangID: string;
var
  buffer: MarshaledString;
  UserLCID: LCID;
  buflen: Integer;
begin
  // defaults
  UserLCID := GetUserDefaultLCID;
  buflen := GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, nil, 0);
  buffer := StrAlloc(buflen);
  if GetLocaleInfo(UserLCID, LOCALE_SISO639LANGNAME, buffer, buflen) <> 0 then
    Result := buffer
  else
    Result := 'en';
  StrDispose(buffer);
end;

function TPlatformWin.GetDefaultFontFamilyName: string;
begin
  if TOSVersion.Check(6) then
    Result := 'Segoe UI'
  else
    Result := 'Tahoma';
end;

function TPlatformWin.GetDefaultFontSize: Single;
begin
  Result := DefaultWindowsFontSize;
end;

function TPlatformWin.GetDefaultSize(const AComponent: TComponentKind): TSize;
begin
  if AComponent = TComponentKind.Calendar then
    Result := TSize.Create(202, 180)
  else
    Result := TSize.Create(80, 22);
end;

function TPlatformWin.GetLocaleFirstDayOfWeek: string;
var
  buffer: DWORD;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, @buffer, SizeOf(buffer) div SizeOf(Char));
  Result := Chr(buffer);
end;

function TPlatformWin.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := nil;
end;

procedure TPlatformWin.Log(const Fmt: string; const Params: array of const);
begin
  OutputDebugString(PChar(Format(Fmt, Params)));
end;

function TPlatformWin.GetListingHeaderBehaviors: TListingHeaderBehaviors;
begin
  Result := [];
end;

function TPlatformWin.GetListingSearchFeatures: TListingSearchFeatures;
begin
  Result := [TListingSearchFeature.StayOnTop];
end;

function TPlatformWin.GetListingTransitionFeatures: TListingTransitionFeatures;
begin
  Result := [];
end;

function TPlatformWin.GetListingEditModeFeatures: TListingEditModeFeatures;
begin
  Result := [];
end;

function TPlatformWin.GetSaveStateFileName(const ABlockName: string): string;
const
  Prefix = '~';
  Separator = '_';
var
  S: TStringBuilder;
  FilePath: string;
begin
  if FSaveStateStoragePath.IsEmpty then
    FilePath := TPath.GetTempPath
  else
    FilePath := FSaveStateStoragePath;
  S := TStringBuilder.Create(FilePath.Length + Length(Prefix) + Length(Separator) + ABlockName.Length);
  try
    S.Append(FilePath);
    S.Append(Prefix);
    S.Append(ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
    S.Append(Separator);
    S.Append(ABlockName);
    Result := S.ToString;
  finally
    S.Free;
  end;
end;

function TPlatformWin.GetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

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

function TPlatformWin.SetSaveStateBlock(const ABlockName: string; const ABlockData: TStream): Boolean;

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

function TPlatformWin.GetSaveStateStoragePath: string;
begin
  Result := FSaveStateStoragePath;
end;

procedure TPlatformWin.SetSaveStateStoragePath(const ANewPath: string);
begin
  if not ANewPath.IsEmpty then
    FSaveStateStoragePath := IncludeTrailingPathDelimiter(ANewPath)
  else
    FSaveStateStoragePath := '';
end;

function TPlatformWin.GetSaveStateNotifications: Boolean;
begin
  Result := False;
end;

function TPlatformWin.GetDisplayMetrics: TDeviceDisplayMetrics;
var
  R: TRect;
begin
  Winapi.Windows.GetWindowRect(GetDesktopWindow, R);
  Result.PhysicalScreenSize := TSize.Create(R.Width, R.Height);
  Result.RawScreenSize := Result.PhysicalScreenSize;
  Result.LogicalScreenSize := Result.PhysicalScreenSize;
  if Result.PhysicalScreenSize.cx > 0 then
    Result.AspectRatio := Result.PhysicalScreenSize.cy / Result.PhysicalScreenSize.cx
  else
    Result.AspectRatio := 1;
  Result.PixelsPerInch := 96; // Windows Default
  Result.ScreenScale := 1;
  Result.FontScale := 1;
end;

function TPlatformWin.RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
begin
    Result := FKeyMapping.RegisterKeyMapping(PlatformKey, VirtualKey, KeyKind);
end;

function TPlatformWin.UnregisterKeyMapping(const PlatformKey: Word): Boolean;
begin
    Result := FKeyMapping.UnregisterKeyMapping(PlatformKey);
end;

function TPlatformWin.PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
begin
    Result := FKeyMapping.PlatformKeyToVirtualKey(PlatformKey, KeyKind);
end;

function TPlatformWin.VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
begin
    Result := FKeyMapping.VirtualKeyToPlatformKey(VirtualKey);
end;

procedure RegisterApplicationHWNDProc(const Proc: TApplicationHWNDProc);
begin
  if PlatformWin <> nil then
    PlatformWin.FApplicationHWNDProc := Proc
  else
    raise EArgumentNilException.Create(SArgumentNil);
end;

{ TVirtualKeyboardWin }

constructor TVirtualKeyboardWin.Create;
var
  L: Integer;
  S: string;
  HID: HKey;
begin
  S := '';
  inherited Create;
  SetLength(S, MAX_PATH);
  L := GetSystemDirectory(PChar(S), MAX_PATH);
  SetLength(S, L);
  FPath := S;
  FExeName := 'osk.exe';
  FWndClassName := 'OSKMainClass';
  FKBPresent := True;
  if not TOSVersion.Check(6, 2) then
  begin
    if Winapi.Windows.RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Enum', 0, KEY_READ,
      HID) = ERROR_SUCCESS then
      try
        S := FindKeyValue(HID, 'ClassGUID', '{4D36E96B-E325-11CE-BFC1-08002BE10318}', 'Control',
          'ActiveService');
        FKBPresent := S <> '';
      finally
        RegCloseKey(HID);
      end;
  end;
  FNewvkbState := vkbState;
  StartTimerLang;
end;

procedure TVirtualKeyboardWin.Clear;
var
  H: HWND;
begin
  H := vkbHandle;
  if (H <> 0) and (FInst > 32) then
  begin
    PostMessage(H, WM_SYSCOMMAND, SC_CLOSE, 0);
  end;
  KillTimerVisible;
  KillTimerLang;
  FInst := 0;
  FError := False;
  FLastTime := 0;
  FLastHandle := 0;
end;

destructor TVirtualKeyboardWin.Destroy;
begin
  Clear;
  inherited;
end;

function TVirtualKeyboardWin.FindKeyValue(const Key: HKey; const Name, Value, SubKeyName, SubValueName: string): string;
var
  Buf, Val: string;
  R, I, J: Integer;
  SubKey: HKey;
  BufSize, T, ValSize: Cardinal;
begin
  Result := '';
  I := 0;
  Buf := '';
  Val := '';
  BufSize := 2048;
  SetLength(Buf, BufSize);
  ValSize := BufSize;
  SetLength(Val, ValSize);
  repeat
    BufSize := Length(Buf);
    ValSize := Length(Val);
    R := Winapi.Windows.RegEnumValue(Key, I, @Buf[1], BufSize, nil, @T, @Val[1], @ValSize);
    if (R = ERROR_SUCCESS) then
    begin
      if (string(PChar(Buf)) = Name) and (T = REG_SZ) and (SameText(string(PChar(Val)), Value)) then
      begin
        if Winapi.Windows.RegOpenKeyEx(Key, PChar(SubKeyName), 0, KEY_READ, SubKey) = ERROR_SUCCESS
        then
          try
            J := 0;
            repeat
              BufSize := Length(Buf);
              ValSize := Length(Val);
              R := Winapi.Windows.RegEnumValue(SubKey, J, @Buf[1], BufSize, nil, @T, @Val[1],
                @ValSize);
              if (R = ERROR_SUCCESS) and (string(PChar(Buf)) = SubValueName) and (T = REG_SZ) and
                (string(PChar(Val)) <> '') then
              begin
                Result := string(PChar(Val));
              end;
              Inc(J);
            until (Result <> '') or (R <> ERROR_SUCCESS);
          finally
            RegCloseKey(SubKey);
          end;
      end;
      Inc(I);
    end;
  until (Result <> '') or (R <> ERROR_SUCCESS);
  if Result = '' then
  begin
    I := 0;
    repeat
      R := Winapi.Windows.RegEnumKey(Key, I, PChar(Buf), BufSize);
      if R = ERROR_SUCCESS then
      begin
        if Winapi.Windows.RegOpenKeyEx(Key, PChar(Buf), 0, KEY_READ, SubKey) = ERROR_SUCCESS then
          try
            Result := FindKeyValue(SubKey, Name, Value, SubKeyName, SubValueName);
          finally
            RegCloseKey(SubKey);
          end;
        Inc(I);
      end;
    until (Result <> '') or (R <> ERROR_SUCCESS);
  end;
end;

function TVirtualKeyboardWin.GetVirtualKeyboardState: TVirtualKeyboardStates;
var
  LState: TvkbState;
begin
  if FError then
    Result := [TVirtualKeyboardState.Error]
  else
    Result := [];
  if IsAutoShow then
    Result := Result + [TVirtualKeyboardState.AutoShow];
  if not FError then
  begin
    if Abs(Now - FLastTime) > 1 / SecsPerDay then
      LState := vkbState
    else
      LState := FLastvkbState;
    if LState = TvkbState.Shown then
      Result := Result + [TVirtualKeyboardState.Visible];
  end;
end;

function TVirtualKeyboardWin.GetVKBounds: TRect;
begin
  if FLastHandle <> 0 then
    GetWindowRect(FLastHandle, Result);
end;

function TVirtualKeyboardWin.HideVirtualKeyboard: Boolean;
begin
  Result := not FError;
  if (not FError) then
  begin
    if IsAutoShow then
      FNewvkbState := TvkbState.Hidden
    else
      FNewvkbState := TvkbState.None;
    if FNewvkbState <> vkbState then
    begin
      StartTimerVisible;
    end;
  end;
end;

procedure TVirtualKeyboardWin.SetTransientState(Value: Boolean);
begin
end;

function TVirtualKeyboardWin.ShowVirtualKeyboard(const AControl: TFmxObject): Boolean;
var
  Root: IRoot;
begin
  Result := not FError;
  if (not FError) then
  begin
    FNewvkbState := TvkbState.Shown;
    if FNewvkbState <> vkbState then
      StartTimerVisible;
    FWait := True;
    FFormHandle := 0;
    if (AControl <> nil) then
    begin
      Root := AControl.Root;
      if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
        FFormHandle := FormToHWND(TCommonCustomForm(Root.GetObject));
    end;
  end;
end;

function TVirtualKeyboardWin.IsAutoShow: Boolean;
begin
  Result := (VKAutoShowMode = TVKAutoShowMode.Always) or ((VKAutoShowMode = TVKAutoShowMode.DefinedBySystem) and
    (not FKBPresent));
end;

procedure TVirtualKeyboardWin.KillTimerLang;
begin
  if FHTmerLang <> 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FTimerService.DestroyTimer(FHTmerLang);
      FHTmerLang := 0;
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TVirtualKeyboardWin.StartTimerLang;
begin
  if FHTmerLang = 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FHTmerLang := FTimerService.CreateTimer(250, TimerLangProc);
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TVirtualKeyboardWin.TimerLangProc;
var
  LCodeKeyboard: HKL;
begin
  if FStepActivate > 0 then
  begin
    FLastHandle := vkbHandle;
    case FStepActivate of
      1:
        begin
          SetActiveWindow(FLastHandle);
          SetFocus(FLastHandle);
        end;
      4:
        begin
          SetActiveWindow(FFormHandle);
        end;
      5:
        begin
          SetFocus(FFormHandle);
          FCodeKeyboard := GetKeyboardLayout(0);
        end;
    end;
    if FStepActivate = 5 then
      FStepActivate := 0
    else
    begin
      Inc(FStepActivate);
      Exit;
    end;
  end
  else
  begin
    if vkbState = TvkbState.Shown then
    begin
      LCodeKeyboard := GetKeyboardLayout(0);
      if FCodeKeyboard <> LCodeKeyboard then
      begin
        SetActiveWindow(0);
        SetActiveWindow(FFormHandle);
        SetFocus(FFormHandle);
        FCodeKeyboard := LCodeKeyboard;
      end;
    end;
  end;
end;

procedure TVirtualKeyboardWin.KillTimerVisible;
begin
  if FHTmerVisible <> 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FTimerService.DestroyTimer(FHTmerVisible);
      FHTmerVisible := 0;
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TVirtualKeyboardWin.StartTimerVisible;
begin
  if FHTmerVisible = 0 then
  begin
    if (FTimerService <> nil) or
      TPlatformServices.Current.SupportsPlatformService(IFMXTimerService, FTimerService) then
    begin
      FHTmerVisible := FTimerService.CreateTimer(100, TimerVisibleProc);
    end
    else
      raise EUnsupportedPlatformService.Create('IFMXTimerService');
  end;
end;

procedure TVirtualKeyboardWin.TimerVisibleProc;
var
  LState: TvkbState;
  procedure Quit;
  begin
    if FLastHandle <> 0 then
      PostMessage(FLastHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
    Sleep(40);
    FLastHandle := 0;
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(False, TRect.Empty), True);
  end;
  procedure Restore;
  begin
    if FLastHandle <> 0 then
    begin
      if Winapi.Windows.GetActiveWindow <> FLastHandle then
      begin
        SendMessage(FLastHandle, WM_SYSCOMMAND, SC_RESTORE, 0);
        TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(True, GetVKBounds), True);
      end;
    end;
  end;
  procedure Hide;
  begin
    if FLastHandle <> 0 then
      PostMessage(FLastHandle, WM_SYSCOMMAND, SC_CLOSE, 0);
    FWait := True;
    FLastHandle := 0;
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(False, TRect.Empty), True);
  end;

begin
  if FWait then
  begin
    FLastHandle := vkbHandle;
    FWait := False;
    Exit;
  end;
  FWait := True;
  LState := vkbState;
  if LState <> FNewvkbState then
  begin
    case LState of
      TvkbState.None:
        case FNewvkbState of
          TvkbState.Hidden: { none }
            ;
          TvkbState.Shown:
            begin
              vkbExecute(FFormHandle);
              FWait := False;
              FStepActivate := 1;
              Exit;
            end;
        end;
      TvkbState.Hidden:
        case FNewvkbState of
          TvkbState.None:
            Quit;
          TvkbState.Shown:
            Restore;
        end;
      TvkbState.Shown:
        case FNewvkbState of
          TvkbState.None:
            Quit;
          TvkbState.Hidden:
            Hide;
        end;
    end;
    FNewvkbState := vkbState;
  end
  else if (FNewvkbState = TvkbState.Shown) and (FStepActivate = 1) then
    // Here we are sending a deferred message, otherwise there will be incorrect coordinates
    TMessageManager.DefaultManager.SendMessage(Self, TVKStateChangeMessage.Create(True, GetVKBounds), True);
  KillTimerVisible;
end;

procedure TVirtualKeyboardWin.vkbExecute(FormHandle: HWND);
var
  S: string;
  N: Integer;
  H: HWND;
  CMD: Integer;
begin
  if FError then
    Exit;
  H := vkbHandle;
  if H = 0 then
  begin
    S := IncludeTrailingPathDelimiter(Path);
    S := S + ExeName;
    if FileExists(S) then
    begin
      TWow64Redirection.Current.Disable;
      try
        CMD := SW_SHOWNOACTIVATE;
        FInst := ShellExecute(FormHandle, 'open', PChar(S), nil, PChar(ExtractFileDir(S)), CMD);
      finally
        TWow64Redirection.Current.Restore;
      end;
      if FInst <= 32 then
        FError := True
      else
      begin
        N := 0;
        while (N < 100) and (vkbState = TvkbState.None) do
        begin
          Inc(N);
          Sleep(40);
        end;
        if N >= 100 then
        begin
          FInst := 0;
          FError := True;
        end;
      end;
    end
    else
    begin
      FError := True;
      FInst := 0;
    end;
  end;
end;

function TVirtualKeyboardWin.vkbHandle: HWND;
begin
  Result := Winapi.Windows.FindWindow(PChar(FWndClassName), nil);
end;

function TVirtualKeyboardWin.vkbState: TvkbState;
var
  H: HWND;
begin
  H := vkbHandle;
  if (H <> INVALID_HANDLE_VALUE) and (H <> 0) then
  begin
    if (not IsWindowVisible(H)) or (IsIconic(H)) then
      Result := TvkbState.Hidden
    else
      Result := TvkbState.Shown;
    FLastHandle := H;
  end
  else
  begin
    Result := TvkbState.None;
    FLastHandle := 0;
  end;
  FLastvkbState := Result;
  FLastTime := Now;
end;

{ TMultiDisplayWin.TDisplayWin }

constructor TMultiDisplayWin.TDisplayWin.Create(const AIndex: Integer; const APrimary: Boolean; const ABounds,
  AWorkArea: TRect; const AHandle: HMONITOR);
begin
  Display := TDisplay.Create(AIndex, APrimary, ABounds, AWorkArea);
  Handle := AHandle;
end;

{ TMultiDisplayWin }

destructor TMultiDisplayWin.Destroy;
begin
  FDisplayList.Free;
  inherited;
end;

function TMultiDisplayWin.GetDisplayCount: Integer;
begin
  if FDisplayCount <= 0 then
    FDisplayCount := GetSystemMetrics(SM_CMONITORS);
  Result := FDisplayCount;
end;

function TMultiDisplayWin.GetDesktopCenterRect(const Size: TSize): TRect;
  function PointOutOfDisplay(const P: TPoint): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to GetDisplayCount - 1 do
      if PtInRect(GetDisplay(I).Bounds, P) then
        Exit(False);
    Result := True;
  end;
  function CornerOutOfDisplay(const R: TRect): Boolean;
  begin
    Result := PointOutOfDisplay(R.TopLeft) or PointOutOfDisplay(TPoint.Create(R.Right, R.Top)) or
      PointOutOfDisplay(R.BottomRight) or PointOutOfDisplay(TPoint.Create(R.Left, R.Bottom));
  end;

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
  if CornerOutOfDisplay(Result) then
  begin
    if Result.Top < WorkArea.Top then
      Result.SetLocation(Result.Left, WorkArea.Top);
    if Result.Bottom > WorkArea.Bottom then
      Result.SetLocation(Result.Left, WorkArea.Bottom - Result.Height);
    if CornerOutOfDisplay(Result) then
    begin
      if Result.Left < WorkArea.Left then
        Result.SetLocation(WorkArea.Left, Result.Top);
      if Result.Right > WorkArea.Right then
        Result.SetLocation(WorkArea.Right - Result.Width, Result.Top);
    end;
  end;
end;

function TMultiDisplayWin.GetDesktopRect: TRect;
begin
  if (FDesktopRect.Width <= 0) or (FDesktopRect.Height <= 0) then
    FDesktopRect := TRect.Create(TPoint.Create(GetSystemMetrics(SM_XVIRTUALSCREEN),
      GetSystemMetrics(SM_YVIRTUALSCREEN)), GetSystemMetrics(SM_CXVIRTUALSCREEN), GetSystemMetrics(SM_CYVIRTUALSCREEN));
  Result := FDesktopRect;
end;

function TMultiDisplayWin.GetWorkAreaRect: TRect;
begin
  if (FWorkAreaRect.Width <= 0) or (FWorkAreaRect.Height <= 0) then
    if not SystemParametersInfo(SPI_GETWORKAREA, 0, FWorkAreaRect, 0) then
      FWorkAreaRect := TRect.Empty;
  Result := FWorkAreaRect;
end;

procedure TMultiDisplayWin.UpdateDisplayInformation;
begin
  FDisplayCount := 0;
  FDesktopRect := TRect.Empty;
  FWorkAreaRect := TRect.Empty;
  FreeAndNil(FDisplayList);
end;

function TMultiDisplayWin.FindDisplay(const hm: HMONITOR): TDisplay;
  function DoFind(const Handle: HMONITOR): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    if FDisplayList <> nil then
      for I := 0 to FDisplayList.Count - 1 do
        if Handle = FDisplayList[I].Handle then
          Exit(I);
  end;

var
  Index: Integer;
begin
  if Screen = nil then
    raise EInvalidFmxHandle.Create(sArgumentInvalid);
  Index := DoFind(hm);
  if Index = -1 then
  begin
    UpdateDisplays;
    Index := DoFind(hm);
  end;
  if Index = -1 then
    raise EInvalidArgument.Create(sArgumentInvalid)
  else
    Result := FDisplayList[Index].Display;
end;

function IsPopupForm(const Form: TCommonCustomForm): Boolean;
begin
  Result := (Form <> nil) and ((Form.FormStyle = TFormStyle.Popup) or (Form is TCustomPopupForm));
end;

function TMultiDisplayWin.DisplayFromWindow(const Handle: TWindowHandle): TDisplay;
var
  Wnd: TWinWindowHandle;
  Form, ParentForm: TCommonCustomForm;
  ParentControl: TFmxObject;
  Control: TControl;
  P: TPointF;
  ScreenPosition: TPoint;
begin
  if Handle = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Wnd := WindowHandleToPlatform(Handle);
  Form := FindWindow(Wnd.Wnd);
  ParentControl := nil;
  if IsPopupForm(Form) then
  begin
    if Form is TCustomPopupForm then
    begin
      if GetKeyState(VK_RBUTTON) <> 0 then
      begin
        GetCursorPos(ScreenPosition);
        Result := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
        Exit;
      end;
      ParentControl := TCustomPopupForm(Form).PlacementTarget;
    end;
    ParentForm := Form.ParentForm;
    if (ParentControl = nil) and (ParentForm <> nil) then
    begin
      while IsPopupForm(ParentForm) and (ParentForm.ParentForm <> nil) do
        ParentForm := ParentForm.ParentForm;
      if ParentControl = nil then
      begin
        ParentControl := Form.Parent;
        while (ParentControl <> nil) and (ParentControl.Root <> nil) and (ParentControl.Root.GetObject <> ParentForm) do
          ParentControl := ParentControl.Parent;
        while (ParentControl <> nil) and not (ParentControl is TControl) do
          ParentControl := ParentControl.Parent;
      end;
      if (ParentControl = nil) and (ParentForm <> nil) and (ParentForm.Focused <> nil) then
        ParentControl := ParentForm.Focused.GetObject;
    end;
    if ParentControl is TControl then
    begin
      Control := TControl(ParentControl);
      P := Control.Padding.Rect.TopLeft;
      P := Control.LocalToAbsolute(P);
      if Control.Root.GetObject is TCommonCustomForm then
      begin
        ParentForm := TCommonCustomForm(Control.Root.GetObject);
        P := ParentForm.ClientToScreen(P);
      end;
      P.Offset((Control.Width - Control.Padding.Right) / 2, (Control.Height - Control.Padding.Bottom) / 2);
      ScreenPosition := TPoint.Create(Round(P.X), Round(P.Y));
      Result := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
      Exit;
    end;
    if ParentForm <> nil then
      Wnd := WindowHandleToPlatform(ParentForm.Handle);
  end;
  if (Wnd = nil) or (Wnd.Wnd = 0) then
    raise EArgumentException.Create(sArgumentInvalid);
  Result := FindDisplay(Winapi.MultiMon.MonitorFromWindow(Wnd.Wnd, MONITOR_DEFAULTTONEAREST));
end;

function TMultiDisplayWin.DisplayFromPoint(const Handle: TWindowHandle; const Point: TPoint): TDisplay;
var
  Wnd: TWinWindowHandle;
  Form: TCommonCustomForm;
  DispByPoint, DispByMouse: TDisplay;
  PointF: TPointF;
  ScreenPosition: TPoint;
begin
  if Handle = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  Wnd := WindowHandleToPlatform(Handle);
  if (Wnd = nil) or (Wnd.Wnd = 0) then
    raise EArgumentException.Create(sArgumentInvalid);
  Form := FindWindow(Wnd.Wnd);
  if not IsPopupForm(Form) then
  begin
    PointF := TPointF.Create(Point);
    ScreenPosition := Form.ClientToScreen(PointF).Round;
    DispByPoint := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
    if (GetKeyState(VK_RBUTTON) <> 0) or (GetKeyState(VK_LBUTTON) <> 0) or (GetKeyState(VK_MBUTTON) <> 0) then
    begin
      Result := DisplayFromWindow(Handle);
      GetCursorPos(ScreenPosition);
      DispByMouse := FindDisplay(Winapi.MultiMon.MonitorFromPoint(ScreenPosition, MONITOR_DEFAULTTONEAREST));
      if DispByMouse.Index <> Result.Index then
        Result := DispByPoint;
    end
    else
      Result := DispByPoint;
  end
  else if Form.Visible and (Form is TCustomPopupForm) then
    Result := FindDisplay(Winapi.MultiMon.MonitorFromWindow(Wnd.Wnd, MONITOR_DEFAULTTONEAREST))
  else
    Result := DisplayFromWindow(Handle);
end;

function EnumMonitorsProc(hm: HMONITOR; dc: HDC; R: PRect; Data: Pointer): Boolean; stdcall;
var
  Sender: TMultiDisplayWin;
  MonInfo: TMonitorInfo;
begin
  Sender := TMultiDisplayWin(Data);
  MonInfo.cbSize := SizeOf(MonInfo);
  if GetMonitorInfo(hm, @MonInfo) then
  begin
    if Sender.FDisplayList = nil then
      Sender.FDisplayList := TList<TMultiDisplayWin.TDisplayWin>.Create;
    Sender.FDisplayList.Add(TMultiDisplayWin.TDisplayWin.Create(Sender.FDisplayList.Count,
      (MonInfo.dwFlags and MONITORINFOF_PRIMARY) <> 0, MonInfo.rcMonitor, MonInfo.rcWork, hm));
  end;
  Result := True;
end;

procedure TMultiDisplayWin.UpdateDisplays;
begin
  UpdateDisplayInformation;
  EnumDisplayMonitors(0, nil, @EnumMonitorsProc, Winapi.Windows.LPARAM(Self));
  if FDisplayList <> nil then
    FDisplayCount := FDisplayList.Count
  else
    FDisplayCount := 0;
end;

function TMultiDisplayWin.GetDisplay(const Index: Integer): TDisplay;
begin
  if Index < 0 then
    raise EListError.CreateFMT(SListIndexError, [Index]);
  if (FDisplayList = nil) or (FDisplayList.Count <> GetDisplayCount) then
    UpdateDisplays;
  if Index >= GetDisplayCount then
    raise EListError.CreateFMT(SListIndexError, [Index]);
  Result := FDisplayList[Index].Display;
end;

{ TFullScreenParams }

procedure TFullScreenParams.Clean;
begin
  Self.BorderStyle := TFmxFormBorderStyle.None;
  Self.WindowState := TWindowState.wsMaximized;
end;

function TFullScreenParams.IsFullScreen: Boolean;
begin
  Result := not ((Self.BorderStyle = TFmxFormBorderStyle.None) and (Self.WindowState = TWindowState.wsMaximized));
end;

{ TWin32MenuInfo }

constructor TWin32MenuInfo.Create(const AMenuId: Integer; const AnItem: TMenuItem);
begin
  MenuID := AMenuId;
  FMXMenuItem := AnItem;
end;

initialization

OleInitialize(nil);
CapturedGestureControl := nil;
LastMousePos := ImpossibleMousePosition;

finalization

// UnregisterCorePlatformServices;
end.
