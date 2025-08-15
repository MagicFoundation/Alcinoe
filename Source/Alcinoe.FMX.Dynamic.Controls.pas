unit Alcinoe.FMX.Dynamic.Controls;

interface

{$I Alcinoe.inc}

uses
  System.UITypes,
  system.Types,
  System.Classes,
  System.Generics.Collections,
  FMX.Forms,
  FMX.Controls,
  FMX.Types,
  FMX.Graphics,
  Alcinoe.Common,
  Alcinoe.fmx.Common,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.FMX.CacheEngines,
  Alcinoe.fmx.Controls;

Type

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicControl = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicControlHost = class(TALControl)
  private
    FHovered: TALDynamicControl;
    FCaptured: TALDynamicControl;
    FCacheEngine: TALBufDrawableCacheEngine;
  protected
    procedure SetCaptured(const Value: TALDynamicControl); virtual;
    procedure SetHovered(const Value: TALDynamicControl); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               out AControlPos: TALPointD; // AControlPos is local to the founded control
               const ACheckHitTest: Boolean = true): TALDynamicControl; overload; virtual; abstract;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               const ACheckHitTest: Boolean = true): TALDynamicControl; overload; virtual;
    property Captured: TALDynamicControl read FCaptured;
    property Hovered: TALDynamicControl read FHovered;
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALDynamicControl is similar to TControl
  // but optimized for minimal overhead.
  TALDynamicControl = class(TObject)
  public
    type
      TOnPaintEvent = procedure(Sender: TALDynamicControl; Canvas: TCanvas) of object;
  private
    FNotifyList: TList<TALDynamicControl>; // 8 bytes | [TFmxObject] FNotifyList: TList<Pointer>;
    FName: string; // 8 bytes | [TComponent] FName: TComponentName;
    FTag: Int64; // 8 bytes | [TComponent] FTag: NativeInt;
    FTagString: string; // 8 bytes | [TFmxObject] FTagString: string;
    FTagObject: TObject; // 8 bytes | [TFmxObject] [Weak] FTagObject: TObject;
    FLeft: Double; // 8 bytes | [TControl] FPosition: TPosition;
    FTop: Double; // 8 bytes | [TControl] FPosition: TPosition;
    FWidth: Double; // 8 bytes | [TControl] FSize: TControlSize;
    FHeight: Double; // 8 bytes | [TControl] FSize: TControlSize;
    FTouchTargetExpansion: TRectF; // 16 bytes | [TControl] FTouchTargetExpansion: TBounds;
    FPivot: TPointF; // 8 Bytes | [TControl] FRotationCenter: TPosition;
    FScale: TPointF; // 8 bytes | [TControl] FScale: TPosition;
    FPadding: TALBounds; // 8 bytes | [TControl] FPadding: TBounds;
    FMargins: TALBounds; // 8 bytes | [TControl] FMargins: TBounds;
    FPressedPosition: TPointF; // 8 bytes | [TControl] FPressedPosition: TPointF;
    FCanvas: TCanvas; // 8 bytes;
    FIsDestroying: boolean; // 1 byte | [TControl] FComponentState: TComponentState; & csDestroying
    FIsEphemeral: boolean; // 1 byte
    FEnabled: Boolean; // 1 byte | [TControl] FEnabled: Boolean;
    FAbsoluteEnabled: Boolean; // 1 byte | [TControl] FAbsoluteEnabled: Boolean;
    FAlign: TALAlignLayout; // 1 byte | [TControl] FAlign: TAlignLayout;
    FVisible: Boolean; // 1 byte | [TControl] FVisible: Boolean;
    FAbsoluteVisible: Boolean; // 1 byte
    FHitTest: Boolean; // 1 byte | [TControl] FHitTest: Boolean;
    FPressed: Boolean; // 1 byte | [TControl] FPressed: Boolean;
    FAutoCapture: Boolean; // 1 byte | [TControl] FAutoCapture: Boolean;
    FRotationAngle: Single; // 4 bytes | [TControl] FRotationAngle: Single;
    FDisabledOpacity: Single; // 4 bytes | [TControl] FDisabledOpacity: Single;
    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    FCursor: TCursor; // 2 bytes | [TControl] FCursor: TCursor;
    FAbsoluteCursor: TCursor; // 2 bytes | [TControl] FInheritedCursor: TCursor;
    {$ENDIF}
    FOnMouseEnter: TNotifyEvent; // 16 bytes | [TControl] FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent; // 16 bytes | [TControl] FOnMouseLeave: TNotifyEvent;
    FOnMouseDown: TMouseEvent; // 16 bytes | [TControl] FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent; // 16 bytes | [TControl] FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent; // 16 bytes | [TControl] FOnMouseUp: TMouseEvent;
    FOnClick: TNotifyEvent; // 16 bytes | [TControl] FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent; // 16 bytes | [TControl] FOnDblClick: TNotifyEvent;
    FOnPaint: TOnPaintEvent; // 16 bytes | [TControl] FOnPaint: TOnPaintEvent;
    FOnPainting: TOnPaintEvent; // 16 bytes | [TControl] FOnPainting: TOnPaintEvent;
    FOnResized: TNotifyEvent; // 16 bytes | [TControl] FOnResized: TNotifyEvent;
    function GetTagFloat: Double;
    procedure SetTagFloat(const AValue: Double);
    procedure SetIndex(const AValue: Integer); // [TFmxObject] procedure SetIndex(NewIndex: Integer);
    function GetRight: Double;
    function GetBottom: Double;
    procedure SetOpacity(const Value: Single); // [TControl] procedure SetOpacity(const Value: Single);
    procedure SetDisabledOpacity(const Value: Single); // [TControl] procedure SetDisabledOpacity(const Value: Single);
    procedure RefreshAbsoluteOpacity;
    function GetCursor: TCursor; // [TControl] function GetCursor: TCursor;
    procedure SetCursor(const Value: TCursor); // [TControl] procedure SetCursor(const Value: TCursor);
    function GetAbsoluteCursor: TCursor; // [TControl] function GetInheritedCursor: TCursor;
    procedure RefreshAbsoluteCursor; // [TControl] procedure RefreshInheritedCursor;
    procedure SetPadding(const AValue: TALBounds); // [TControl] procedure SetPadding(const Value: TBounds);
    procedure SetMargins(const AValue: TALBounds); // [TControl] procedure SetMargins(const Value: TBounds);
    procedure PaddingChangedHandler(Sender: TObject); // [TControl] procedure PaddingChangedHandler(Sender: TObject); overload;
    procedure MarginsChangedHandler(Sender: TObject); // [TControl] procedure MarginsChanged(Sender: TObject);
    function GetControlByIndex(Const AIndex: Integer): TALDynamicControl;
    function GetControlByName(Const AName: String): TALDynamicControl;
    function GetForm: TCommonCustomForm;
  protected
    FOwner: TALDynamicControl; // 8 bytes | [TControl] FOwner: TComponent; FParentControl: TControl;
    FHost: TALDynamicControlHost; // 8 bytes
    // The order of the list determines the alignment priority and z-order: the
    // first control in the list aligns and paints before the others. first
    // control in the list catches touch events after others
    FControls: TArray<TALDynamicControl>; // 8 bytes | [TControl] FControls: TControlList;
    FControlsCount: Integer; // 4 bytes
    FUpdating: Integer; // 4 bytes | [TControl] FUpdating: Integer;
    FIndex: Integer; // 4 bytes
    FOpacity: Single; // 4 bytes | [TControl] FOpacity: Single;
    FAbsoluteOpacity: Single; // 4 bytes | [TControl] FAbsoluteOpacity: Single;
    FDisableAlign: Boolean; // 1 byte | [TControl] FDisableAlign: Boolean;
    FIsMouseOver: Boolean; // 1 byte | [TControl] FIsMouseOver: Boolean;
    function CreateMargins: TALBounds; virtual;
    function CreatePadding: TALBounds; virtual;
    procedure SetOwner(const Value: TALDynamicControl); virtual;
    procedure SetHost(Const Value: TALDynamicControlHost); virtual;
    procedure SetPivot(const AValue: TPointF);
    procedure SetRotationAngle(const AValue: Single);
    procedure SetScale(const AValue: TPointF);
    function GetBoundsRect: TALRectD; // [TControl] function GetBoundsRect: TRectF; virtual;
    procedure SetBoundsRect(const Value: TALRectD); // [TControl] procedure SetBoundsRect(const Value: TRectF); virtual;
    function GetLocalRect: TALRectD; virtual; // [TControl] function GetLocalRect: TRectF; virtual;
    function GetExpandedLocalRect: TALRectD;
    function GetAbsoluteRect: TALRectD; virtual; // [TControl] function GetAbsoluteRect: TRectF; virtual;
    function GetAbsoluteDisplayedRect: TRectF; virtual;
    function GetExpandedBoundsRect: TALRectD;
    function GetAbsoluteVisible: Boolean; // [TControl] function GetParentedVisible: Boolean; virtual;
    function GetPosition: TALPointD;
    procedure SetPosition(const AValue: TALPointD); overload;
    procedure SetPosition(const AValue: TPointf); overload;
    procedure SetPosition(const ALeft, ATop: Double); overload;
    procedure SetLeft(const Value: Double);
    procedure SetTop(const Value: Double);
    procedure SetWidth(const Value: Double); // [TControl] procedure SetWidth(const Value: Single); virtual;
    procedure SetHeight(const Value: Double); // [TControl] procedure SetHeight(const Value: Single); virtual;
    function GetDefaultSize: TSizeF; virtual; // [TControl] function GetDefaultSize: TSizeF; virtual;
    procedure SetTouchTargetExpansion(const AValue: TRectF); virtual;
    procedure SetEnabled(const Value: Boolean); virtual; // [TControl] procedure SetEnabled(const Value: Boolean); virtual;
    procedure refreshAbsoluteEnabled; virtual;
    procedure SetVisible(const Value: Boolean); virtual; // [TControl] procedure SetVisible(const Value: Boolean); virtual;
    function GetFirstVisibleObjectIndex: Integer; virtual; // [TControl] function GetFirstVisibleObjectIndex: Integer; virtual;
    function GetLastVisibleObjectIndex: Integer; virtual; // [TControl] function GetLastVisibleObjectIndex: Integer; virtual;
    function IsVisibleObject(const AObject: TALDynamicControl): Boolean; virtual;
    procedure RefreshAbsoluteVisible;
    procedure SetAlign(const Value: TALAlignLayout); virtual; // [TControl] procedure SetAlign(const Value: TAlignLayout); virtual;
    function DoGetDownloadPriority: Int64; virtual;
    class function GetDownloadPriority(const AContext: Tobject): Int64; virtual;
    function GetDoubleBuffered: boolean; virtual; abstract;
    procedure SetDoubleBuffered(const AValue: Boolean); virtual; abstract;
    function GetScrollEngine: TALScrollEngine; Virtual;
    function FillTextFlags: TFillTextFlags; virtual; // [TControl] function FillTextFlags: TFillTextFlags; virtual;
    procedure PaddingChanged; virtual; // [TControl] procedure PaddingChanged; overload; virtual;
    procedure MarginsChanged; virtual;
    procedure EnabledChanged; virtual; // [TControl] procedure EnabledChanged; virtual;
    procedure VisibleChanged; virtual; // [TControl] procedure VisibleChanged; virtual;
    procedure AncestorVisibleChanged(const AVisible: Boolean); virtual; // [TControl] procedure AncestorVisibleChanged(const Visible: Boolean); virtual;
    procedure ParentChanged; virtual; // [TControl] procedure ParentChanged; virtual;
    procedure AncestorParentChanged; virtual; // [TControl] procedure AncestorParentChanged; virtual;
    procedure PositionChanged; virtual;
    procedure SizeChanged; virtual; // [TControl] procedure SizeChanged(Sender: TObject);
    procedure DoResized; virtual; // [TControl] procedure DoResized; virtual;
    procedure Realign; // [TControl] procedure Realign;
    procedure DoRealign; virtual; // [TControl] procedure DoRealign; virtual;
    procedure AdjustSize; virtual; abstract;
    procedure DoBeginUpdate; virtual; // [TControl] procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual; // [TControl] procedure DoEndUpdate; virtual;
    procedure MouseEnter; virtual; // [TControl] procedure DoMouseEnter; virtual;
    procedure MouseLeave; virtual; // [TControl] procedure DoMouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // [TControl] procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual; // [TControl] procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // [TControl] procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // [TControl] procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); virtual; // [TControl] procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); virtual;
    procedure ChildrenMouseDown(const AObject: TALDynamicControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure ChildrenMouseMove(const AObject: TALDynamicControl; Shift: TShiftState; X, Y: Single); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure ChildrenMouseUp(const AObject: TALDynamicControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure ChildrenMouseEnter(const AObject: TALDynamicControl); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure ChildrenMouseLeave(const AObject: TALDynamicControl); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure Click; virtual; // [TControl] procedure Click; virtual;
    procedure DblClick; virtual; // [TControl] procedure DblClick; virtual;
    function IsInMotion: Boolean; virtual; abstract;
    procedure Capture; // [TControl] procedure Capture;
    procedure ReleaseCapture; // [TControl] procedure ReleaseCapture;
    procedure PaintInternal(const ACanvas: TCanvas); virtual; // [TControl] procedure PaintInternal;
    procedure Painting; virtual; // [TControl] procedure Painting; virtual;
    procedure Paint; virtual; // [TControl] procedure Paint; virtual;
    function PaintChildrenOnly: Boolean; virtual;
    procedure PaintChildren; virtual;  // [TControl] procedure PaintChildren; virtual;
    procedure AfterPaint; virtual; // [TControl] procedure AfterPaint; virtual;
    procedure DoInsertControl(const AControl: TALDynamicControl; const AIndex: Integer); virtual; // [TFmxObject] procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); virtual;
    procedure DoRemoveControl(const AControl: TALDynamicControl); overload; virtual; // [TFmxObject] procedure DoRemoveObject(const AObject: TFmxObject); virtual;
    procedure DoMoveControl(const AControl: TALDynamicControl; const ANewIndex: Integer); virtual; // [TFmxObject] procedure Exchange(const AObject1, AObject2: TFmxObject); virtual;
    procedure FreeNotification(const AObject: TALDynamicControl); virtual; // [TFmxObject] procedure FreeNotification(AObject: TObject); virtual;
  public
    constructor Create(const AOwner: TObject); virtual; // [TControl] constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; // [TControl] destructor Destroy; override;
    procedure BeforeDestruction; override;
    Property IsDestroying: boolean read FIsDestroying; // [MultiThread]
    Property IsEphemeral: boolean read FIsEphemeral write FIsEphemeral; // [MultiThread]
    procedure Assign(Source: TALDynamicControl); virtual; abstract;
    procedure BeginUpdate; virtual; // [TControl] procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual; abstract; // [TControl] procedure EndUpdate; virtual;
    function IsUpdating: Boolean; virtual; // [TControl] function IsUpdating: Boolean; virtual;
    function IsReadyToDisplay(const AStrict: Boolean = False): Boolean; virtual; abstract;
    procedure ApplyColorScheme; virtual; abstract;
    procedure AddControl(const AControl: TALDynamicControl); inline; // [TFmxObject] procedure AddObject(const AObject: TFmxObject);
    procedure InsertControl(const AControl: TALDynamicControl; const AIndex: Integer); // [TFmxObject] procedure InsertObject(Index: Integer; const AObject: TFmxObject);
    procedure RemoveControl(const AControl: TALDynamicControl); overload; // [TFmxObject] procedure RemoveObject(const AObject: TFmxObject); overload;
    procedure RemoveControl(const AIndex: Integer); overload; // [TFmxObject] procedure RemoveObject(Index: Integer); overload;
    procedure MoveControl(const AControl: TALDynamicControl; const ANewIndex: Integer); // [TFmxObject] procedure Exchange(const AObject1, AObject2: TFmxObject); virtual;
    procedure AddFreeNotify(const AControl: TALDynamicControl); // [TFmxObject] procedure AddFreeNotify(const AObject: IFreeNotification);
    procedure RemoveFreeNotify(const AControl: TALDynamicControl); // [TFmxObject] procedure RemoveFreeNotify(const AObject: IFreeNotification);
    procedure MakeBufDrawable; virtual; abstract;
    procedure ClearBufDrawable; virtual; abstract;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default False;
    procedure Repaint; // [TControl] procedure Repaint;
    procedure BringToFront; virtual; // [TFmxObject] procedure BringToFront;
    procedure SendToBack; virtual; // [TFmxObject] procedure SendToBack;
    function HasUnconstrainedAutosizeWidth: Boolean; virtual; abstract;
    function HasUnconstrainedAutosizeHeight: Boolean; virtual; abstract;
    function AbsoluteToLocal(const APoint: TALPointD): TALPointD; overload; virtual; // [TControl] function AbsoluteToLocal(const APoint: TPointF): TPointF; overload; virtual;
    function AbsoluteToLocal(const ARect: TALRectD): TALRectD; overload; virtual; // [TControl] function AbsoluteToLocal(const ARect: TRectF): TRectF; overload;
    function AbsoluteToLocal(const APoint: TPointF): TALPointD; overload; // [TControl] function AbsoluteToLocal(const APoint: TPointF): TPointF; overload; virtual;
    function AbsoluteToLocal(const ARect: TRectF): TALRectD; overload; // [TControl] function AbsoluteToLocal(const ARect: TRectF): TRectF; overload;
    function LocalToAbsolute(const APoint: TALPointD): TALPointD; overload; virtual; // [TControl] function LocalToAbsolute(const APoint: TPointF): TPointF; overload; virtual;
    function LocalToAbsolute(const ARect: TALRectD): TALRectD; overload; virtual; // [TControl] function LocalToAbsolute(const ARect: TRectF): TRectF; overload;
    function LocalToAbsolute(const APoint: TPointF): TALPointD; overload; // [TControl] function LocalToAbsolute(const APoint: TPointF): TPointF; overload; virtual;
    function LocalToAbsolute(const ARect: TRectF): TALRectD; overload; // [TControl] function LocalToAbsolute(const ARect: TRectF): TRectF; overload;
    function PointInObjectLocal(X, Y: Double): Boolean; virtual; // [TControl] function PointInObject(X, Y: Single): Boolean; virtual;
    function IsDisplayed: Boolean; virtual; abstract;
    property Name: String read FName write FName; // [TComponent:published] property Name: TComponentName read FName write SetName stored False;
    property Tag: Int64 read FTag write FTag; // [TComponent:published] property Tag: NativeInt read FTag write FTag default 0;
    property TagFloat: Double read GetTagFloat write SetTagFloat; // [TComponent:published] property TagFloat: Single read FTagFloat write FTagFloat;
    property TagString: string read FTagString write FTagString; // [TFmxObject] property TagString: string read FTagString write FTagString;
    property TagObject: TObject read FTagObject write FTagObject; // [TFmxObject] property TagObject: TObject read FTagObject write FTagObject;
    property Enabled: Boolean read FEnabled write SetEnabled; // [TControl] property Enabled: Boolean read FEnabled write SetEnabled stored EnabledStored default True;
    property AbsoluteEnabled: Boolean read FAbsoluteEnabled; // [TControl] property AbsoluteEnabled: Boolean read GetAbsoluteEnabled;
    property Opacity: Single read FOpacity write SetOpacity; // [TControl] property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
    property DisabledOpacity: Single read FDisabledOpacity write SetDisabledOpacity; // [TControl] property DisabledOpacity: Single read FDisabledOpacity write SetDisabledOpacity stored DisabledOpacityStored nodefault;
    property AbsoluteOpacity: Single read FAbsoluteOpacity; // [TControl] property AbsoluteOpacity: Single read GetAbsoluteOpacity;
    property Visible: Boolean read FVisible write SetVisible; // [TControl] property Visible: Boolean read FVisible write SetVisible stored VisibleStored default True;
    property AbsoluteVisible: Boolean read GetAbsoluteVisible; // [TControl] property ParentedVisible: Boolean read GetParentedVisible;
    property Owner: TALDynamicControl read FOwner write SetOwner; // [TControl] property Owner: TComponent read FOwner;
    property Host: TALDynamicControlHost read FHost;
    property Form: TCommonCustomForm read GetForm;
    property Canvas: TCanvas read FCanvas; // [TControl] property Canvas: TCanvas read GetCanvas;
    property BoundsRect: TALRectD read GetBoundsRect write SetBoundsRect; // [TControl] property BoundsRect: TRectF read GetBoundsRect write SetBoundsRect;
    property LocalRect: TALRectD read GetLocalRect; // [TControl] property LocalRect: TRectF read GetLocalRect;
    property AbsoluteRect: TALRectD read GetAbsoluteRect; // [TControl] property AbsoluteRect: TRectF read GetAbsoluteRect;
    property ExpandedBoundsRect: TALRectD read GetExpandedBoundsRect;
    property ExpandedLocalRect: TALRectD read GetExpandedLocalRect;
    property TouchTargetExpansion: TRectF read FTouchTargetExpansion write SetTouchTargetExpansion; // [TControl] property TouchTargetExpansion: TBounds read FTouchTargetExpansion write SetTouchTargetExpansion;
    property Pivot: TPointF read FPivot write SetPivot; // [TControl] property RotationCenter: TPosition read GetRotationCenter write SetRotationCenter;
    property RotationAngle: Single read FRotationAngle write SetRotationAngle; // [TControl] property RotationAngle: Single read GetRotationAngle write SetRotationAngle;
    property Scale: TPointF read FScale write SetScale; // [TControl] property Scale: TPosition read GetScale write SetScale;
    property Position: TALPointD read GetPosition write SetPosition; // [TControl] property Position: TPosition read FPosition write SetPosition stored IsPositionStored;
    property Left: Double read FLeft write SetLeft; // [TControl] property Position: TPosition read FPosition write SetPosition stored IsPositionStored;
    property Top: Double read FTop write SetTop; // [TControl] property Position: TPosition read FPosition write SetPosition stored IsPositionStored;
    property Right: Double read GetRight;
    property Bottom: Double read GetBottom;
    property Width: Double read FWidth write SetWidth; // [TControl] property Width: Single read GetWidth write SetWidth stored False nodefault;
    property Height: Double read FHeight write SetHeight; // [TControl] property Height: Single read GetHeight write SetHeight stored False nodefault;
    property Padding: TALBounds read FPadding write SetPadding; // [TControl] property Padding: TBounds read GetPadding write SetPadding;
    property Margins: TALBounds read FMargins write SetMargins; // [TControl] property Margins: TBounds read GetMargins write SetMargins;
    property Align: TALAlignLayout read FAlign write SetAlign; // [TControl] property Align: TAlignLayout read FAlign write SetAlign default TAlignLayout.None;
    procedure SetBounds(X, Y, AWidth, AHeight: Double); virtual; // [TControl] procedure SetBounds(X, Y, AWidth, AHeight: Single); virtual;
    procedure SetSize(const ASize: TALSizeD); overload;
    procedure SetSize(const ASize: TSizeF); overload;
    procedure SetSize(const AWidth, AHeight: Double); overload;
    property DefaultSize: TSizeF read GetDefaultSize; // [TControl] property DefaultSize: TSizeF read GetDefaultSize;
    property ScrollEngine: TALScrollEngine read GetScrollEngine;
    property HitTest: Boolean read FHitTest write FHitTest; // [TControl] property HitTest: Boolean read FHitTest write SetHitTest default True;
    property Cursor: TCursor read GetCursor write SetCursor; // [TControl] property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property AbsoluteCursor: TCursor read GetAbsoluteCursor; // [TControl] property InheritedCursor: TCursor read GetInheritedCursor default crDefault;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               out AControlPos: TALPointD; // AControlPos is local to the founded control
               const ACheckHitTest: Boolean = true): TALDynamicControl; overload; virtual; // [TControl] function ObjectAtPoint(AScreenPoint: TPointF): IControl; virtual;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               const ACheckHitTest: Boolean = true): TALDynamicControl; overload; virtual; // [TControl] function ObjectAtPoint(AScreenPoint: TPointF): IControl; virtual;
    property Controls[const Index: Integer]: TALDynamicControl read GetControlByIndex; default; // [TControl] property Controls: TControlList read GetControls;
    property Controls[const Name: String]: TALDynamicControl read GetControlByName; default; // [TControl] property Controls: TControlList read GetControls;
    property ControlsCount: Integer read FControlsCount; // [TControl] property ControlsCount: Integer read GetControlsCount;
    property Index: Integer read FIndex write SetIndex; // [TFmxObject] property Index: Integer read GetIndex write SetIndex;
    property IsMouseOver: Boolean read FIsMouseOver; // [TControl] property IsMouseOver: Boolean read FIsMouseOver;
    property Pressed: Boolean read FPressed write FPressed; // [TControl] property Pressed: Boolean read FPressed write FPressed;
    property PressedPosition: TPointF read FPressedPosition write FPressedPosition; // [TControl] property PressedPosition: TPointF read FPressedPosition write FPressedPosition;
    property AutoCapture: Boolean read FAutoCapture write FAutoCapture; // [TControl] property AutoCapture: Boolean read FAutoCapture write FAutoCapture default False;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;// [TControl] property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;// [TControl] property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;// [TControl] property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnClick: TNotifyEvent read FOnClick write FOnClick; // [TControl] property OnClick: TNotifyEvent read FOnClick write SetOnClick stored OnClickStored;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick; // [TControl] property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;// [TControl] property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;// [TControl] property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPaint: TOnPaintEvent read FOnPaint write FOnPaint; // [TControl] property OnPaint: TOnPaintEvent read FOnPaint write FOnPaint;
    property OnPainting: TOnPaintEvent read FOnPainting write FOnPainting; // [TControl] property OnPainting: TOnPaintEvent read FOnPainting write FOnPainting;
    property OnResized: TNotifyEvent read FOnResized write FOnResized; // [TControl] property OnResized: TNotifyEvent read FOnResized write FOnResized;
  end;

{$REGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (1)'}

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicExtendedControl = class(TALDynamicControl)
  private
    //**FForm: TCommonCustomForm; // 8 bytes
    //**FOwner: TALDynamicControl; // 8 bytes
    //**FFormerMarginsChangedHandler: TNotifyEvent; // 16 bytes
    FControlAbsolutePosAtMouseDown: TALPointD; // 8 bytes
    //**FScale: TPosition; // 8 bytes | TPosition instead of TALPosition to avoid circular reference
    //**FFocusOnMouseDown: Boolean; // 1 byte
    //**FFocusOnMouseUp: Boolean; // 1 byte
    FMouseDownAtRest: Boolean; // 1 byte
    FDoubleClick: Boolean; // 1 byte
    FAutoAlignToPixel: Boolean; // 1 byte
    //**FAlign: TALAlignLayout; // 1 byte
    FIsSetBoundsLocked: Boolean; // 1 byte
    FBeforeDestructionExecuted: Boolean; // 1 byte
    //**procedure SetScale(const AValue: TPosition);
    //**function GetPivot: TPosition;
    //**procedure SetPivot(const Value: TPosition);
    function GetPressed: Boolean;
    procedure SetPressed(const AValue: Boolean);
    //**procedure DelayOnResize(Sender: TObject);
    //**procedure DelayOnResized(Sender: TObject);
    //**procedure MarginsChangedHandler(Sender: TObject);
    //**procedure ScaleChangedHandler(Sender: TObject);
  protected
    FClickSound: TALClickSoundMode; // 1 byte
    FAutoSize: TALAutoSizeMode; // 1 byte
    FIsAdjustingSize: Boolean; // 1 byte
    FAdjustSizeOnEndUpdate: Boolean; // 1 byte
    property BeforeDestructionExecuted: Boolean read FBeforeDestructionExecuted;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    //**property Scale: TPosition read FScale write SetScale;
    //**property Pivot: TPosition read GetPivot write SetPivot;
    function GetAutoSize: TALAutoSizeMode; virtual;
    procedure SetAutoSize(const Value: TALAutoSizeMode); virtual;
    // Dynamically adjusts the dimensions to accommodate child controls,
    // considering their sizes, positions, margins, and alignments.
    property AutoSize: TALAutoSizeMode read GetAutoSize write SetAutoSize default TALAutoSizeMode.None;
    function GetAutoAlignToPixel: Boolean; virtual;
    procedure SetAutoAlignToPixel(const AValue: Boolean); Virtual;
    //**property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;
    //**property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;
    //**procedure DoEnter; override;
    //**procedure DoExit; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoClickSound; virtual;
    procedure Click; override;
    function IsInMotion: Boolean; override;
    //**function GetParentedVisible: Boolean; override;
    //**procedure DoMatrixChanged(Sender: TObject); override;
    //**procedure DoRootChanged; override;
    procedure IsMouseOverChanged; virtual;
    //**procedure IsFocusedChanged; virtual;
    procedure PressedChanged; virtual;
    //**Procedure MarginsChanged; virtual;
    procedure PaddingChanged; override;
    procedure ParentChanged; override;
    //**procedure Loaded; override;
    //**function IsOwnerLoading: Boolean;
    //**function IsSizeStored: Boolean; override;
    //**function GetAlign: TALAlignLayout; Reintroduce;
    //**procedure SetAlign(const Value: TALAlignLayout); Reintroduce; virtual;
    procedure DoEndUpdate; override;
    procedure DoResized; override;
    procedure DoRealign; override;
    procedure ParentRealigning; virtual;
    /// <summary>
    ///   Return the largest size this control can have while still
    ///   being fully contained within its container (parent).
    /// </summary>
    function GetMaxContainedSize: TSizeF; Virtual;
    procedure AdjustSize; override;
    procedure SetFixedSizeBounds(X, Y, AWidth, AHeight: Single); Virtual;
    //**function GetAbsoluteDisplayedRect: TRectF; virtual;
    //**function FillTextFlags: TFillTextFlags; override;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure Assign(Source: TALDynamicControl); override;
    procedure EndUpdate; override;
    //**procedure SetNewScene(AScene: IScene); override;
    function IsReadyToDisplay(const AStrict: Boolean = False): Boolean; override;
    function IsDisplayed: Boolean; override;
    property DisplayedRect: TRectF read GetAbsoluteDisplayedRect;
    //**property Form: TCommonCustomForm read FForm;
    {$IFNDEF ALCompilerVersionSupported123}
      {$MESSAGE WARN 'Check if property FMX.Controls.TControl.Pressed still not fire a PressChanged event when it gets updated, and adjust the IFDEF'}
    {$ENDIF}
    property Pressed: Boolean read GetPressed write SetPressed;
    procedure AlignToPixel; virtual;
    procedure ApplyColorScheme; override;
    procedure SetBounds(X, Y, AWidth, AHeight: Double); override;
    function HasUnconstrainedAutosizeWidth: Boolean; override;
    function HasUnconstrainedAutosizeHeight: Boolean; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    //**property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default False;
    /// <summary>
    ///   When AutoAlignToPixel is true, all dimensions used to build the buffered drawable
    ///   are aligned to the pixel grid. Additionally, after the object is loaded, all properties
    ///   related to the pixel grid (e.g., margins) are automatically aligned. Note that setting these
    ///   properties at runtime does not change their alignment; alignment is applied only via the
    ///   loading process.
    /// </summary>
    property AutoAlignToPixel: Boolean read GetAutoAlignToPixel write SetAutoAlignToPixel;
    //**property Align: TALAlignLayout read FAlign write SetAlign default TALAlignLayout.None;
    //**property Owner: TALDynamicControl read FOwner;
    property ClickSound: TALClickSoundMode read FClickSound write FClickSound default TALClickSoundMode.Default;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicContent = class(TALDynamicExtendedControl)
  protected
    //**function GetTabStopController: ITabStopController; override;
    procedure DoInsertControl(const AControl: TALDynamicControl; const AIndex: Integer); override;
    procedure DoRemoveControl(const AControl: TALDynamicControl); override;
    //**procedure DoDeleteChildren; override;
    procedure DoRealign; override;
    //**procedure IContent.Changed = ContentChanged;
    procedure DoContentChanged; Virtual;
    procedure ContentChanged;
  public
    constructor Create(const AOwner: TObject); override;
    //**function GetTabListClass: TTabListClass; override;
  public
    //property Action;
    property Align;
    //property Anchors;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClickSound;
    //**property ClipChildren default False;
    //property ClipParent;
    property Cursor;
    //**property DragMode;
    //**property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    //**property Locked;
    property Margins;
    property Opacity;
    property Padding;
    //**property PopupMenu;
    //**property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    //**property Size;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    //**property OnCanFocus;
    //**property OnDragEnter;
    //**property OnDragLeave;
    //**property OnDragOver;
    //**property OnDragDrop;
    //**property OnDragEnd;
    //**property OnEnter;
    //**property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //**property OnMouseWheel;
    property OnClick;
    property OnDblClick;
    //**property OnKeyDown;
    //**property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //**property OnResized;
  end;

{$ENDREGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (1)'}

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  Fmx.utils,
  Alcinoe.HTTP.Client.Net.Pool,
  Alcinoe.Localization,
  Alcinoe.StringUtils;


type
  _TALWorkerContextProtectedAccess = class(TALWorkerContext);

{***********************************************************}
constructor TALDynamicControlHost.Create(aOwner: TComponent);
begin
  inherited create(AOwner);
  ClipChildren := True;
  FHovered := nil;
  FCaptured := nil;
  FCacheEngine := TALBufDrawableCacheEngine.Create;
end;

{***************************************}
destructor TALDynamicControlHost.Destroy;
begin
  FCacheEngine.DecreaseRefCount;
  inherited Destroy;
end;

{*********************************************}
function TALDynamicControlHost.GetControlAtPos(
           const APos: TALPointD; // APos is local to the control
           const ACheckHitTest: Boolean = true): TALDynamicControl;
begin
  Var LControlPos: TALPointD;
  result := GetControlAtPos(APos, LControlPos, ACheckHitTest);
end;

{**************************************************************************}
procedure TALDynamicControlHost.SetCaptured(const Value: TALDynamicControl);
begin
  FCaptured := Value;
end;

{*************************************************************************}
procedure TALDynamicControlHost.SetHovered(const Value: TALDynamicControl);
begin
  if (Value <> FHovered) then begin
    if FHovered <> nil then FHovered.MouseLeave;
    FHovered := Value;
    if FHovered <> nil then FHovered.MouseEnter;
  end;
end;

{************************************************************************************************}
procedure TALDynamicControlHost.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LControlMousePos: TALPointD;
  var LControl := GetControlAtPos(
                    TALPointD.create(X, Y), // const APos: TPointF; // APos is local to the control
                    LControlMousePos); // out AControlPos: TPointF; // AControlPos is local to the founded control
  If LControl <> nil then
    LControl.MouseDown(Button, Shift, LControlMousePos.X, LControlMousePos.Y);
  inherited;
end;

{**************************************************************************}
procedure TALDynamicControlHost.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if FCaptured <> nil then begin
    Var LCapturedMousePos := FCaptured.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(X, Y)));
    Cursor := FCaptured.Cursor;
    FCaptured.MouseMove(Shift, LCapturedMousePos.X, LCapturedMousePos.Y);
  end
  else if not ALGetHasTouchScreen then begin
    var LControlMousePos: TALPointD;
    var LControl := GetControlAtPos(
                      TALPointD.create(X, Y), // const APos: TPointF; // APos is local to the control
                      LControlMousePos); // out AControlPos: TPointF; // AControlPos is local to the founded control
    If LControl <> nil then begin
      SetHovered(LControl);
      Cursor := LControl.Cursor;
      LControl.MouseMove(Shift, LControlMousePos.X, LControlMousePos.Y);
    end
    else begin
      Cursor := CrDefault;
      SetHovered(nil);
    end;
  end;
  inherited;
end;

{**********************************************************************************************}
procedure TALDynamicControlHost.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FCaptured <> nil then begin
    Var LCapturedMousePos := FCaptured.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(X, Y)));
    FCaptured.MouseUp(Button, Shift, LCapturedMousePos.X, LCapturedMousePos.Y);
    SetCaptured(nil);
  end
  else begin
    var LControlMousePos: TALPointD;
    var LControl := GetControlAtPos(
                      TALPointD.create(X, Y), // const APos: TPointF; // APos is local to the control
                      LControlMousePos); // out AControlPos: TPointF; // AControlPos is local to the founded control
    If LControl <> nil then
      LControl.MouseUp(Button, Shift, LControlMousePos.X, LControlMousePos.Y);
  end;
  inherited;
end;

{*************************************************************************************************}
procedure TALDynamicControlHost.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FCaptured <> nil then begin
    Var LCapturedMousePos := FCaptured.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(X, Y)));
    FCaptured.MouseClick(Button, Shift, LCapturedMousePos.X, LCapturedMousePos.Y);
  end
  else begin
    var LControlMousePos: TALPointD;
    var LControl := GetControlAtPos(
                      TALPointD.create(X, Y), // const APos: TPointF; // APos is local to the control
                      LControlMousePos); // out AControlPos: TPointF; // AControlPos is local to the founded control
    If LControl <> nil then
      LControl.MouseClick(Button, Shift, LControlMousePos.X, LControlMousePos.Y);
  end;
  inherited;
end;

{*******************************************}
procedure TALDynamicControlHost.DoMouseLeave;
begin
  SetCaptured(Nil);
  SetHovered(nil);
  Cursor := CrDefault;
  inherited;
end;

{**************}
// [MultiThread]
constructor TALDynamicControl.Create(const AOwner: TObject);
begin
  inherited create;
  FNotifyList := nil;
  FName := '';
  FTag := 0;
  FTagString := '';
  FTagObject := nil;
  FLeft := 0;
  FTop := 0;
  var LDefaultSize := GetDefaultSize;
  FWidth := LDefaultSize.Width;
  FHeight := LDefaultSize.Height;
  FTouchTargetExpansion := TRectF.Empty;
  FPivot := TPointF.Zero;
  FScale := TPointF.Create(1,1);
  FPadding := CreatePadding;
  FPadding.OnChanged := PaddingChangedHandler;
  FMargins := CreateMargins;
  FMargins.OnChanged := MarginsChangedHandler;
  FPressedPosition := TPointF.Zero;
  FCanvas := nil;
  FIsDestroying := False;
  FIsEphemeral := False;
  FEnabled := True;
  FAbsoluteEnabled := True;
  FAlign := TALAlignLayout.None;
  FVisible := True;
  FAbsoluteVisible := True;
  FHitTest := True;
  FPressed := False;
  FAutoCapture := False;
  FRotationAngle := 0;
  FDisabledOpacity := Tcontrol.DefaultDisabledOpacity;
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  FCursor := crDefault;
  FAbsoluteCursor := crDefault;
  {$ENDIF}
  FOnMouseEnter := nil;
  FOnMouseLeave := nil;
  FOnMouseDown := nil;
  FOnMouseMove := nil;
  FOnMouseUp := nil;
  FOnClick := nil;
  FOnDblClick := nil;
  FOnPaint := nil;
  FOnPainting := nil;
  FOnResized := nil;
  FOwner := nil;
  FHost := nil;
  SetLength(FControls, 0);
  FControlsCount := 0;
  FUpdating := 0;
  FIndex := -1;
  FOpacity := 1;
  FAbsoluteOpacity := 1;
  FDisableAlign := False;
  FIsMouseOver := False;
  if AOwner <> nil then begin
    if AOwner is TALDynamicControlHost then
      SetHost(TALDynamicControlHost(AOwner))
    else begin
      {$IF defined(DEBUG)}
      if not (AOwner is TALDynamicControl) then
        Raise Exception.Create('Invalid AOwner type: expected TALDynamicListBox or TALDynamicControl');
      {$ENDIF}
      SetOwner(TALDynamicControl(AOwner));
    end;
  end;
end;

{***********************************}
destructor TALDynamicControl.Destroy;
begin
  SetOwner(nil);
  ALFreeAndNil(FPadding);
  ALFreeAndNil(FMargins);
  for Var I := 0 to FControlsCount - 1 do
    ALFreeAndNil(FControls[i]);
  inherited;
end;

{********************************************}
procedure TALDynamicControl.BeforeDestruction;
begin
  fIsDestroying := True;
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Types.TFmxObject.BeforeDestruction was not updated and adjust the IFDEF'}
  {$ENDIF}
  if FNotifyList <> nil then begin
    var L2 := TList<TALDynamicControl>.Create;
    try
      L2.AddRange(FNotifyList);
      for var I := L2.Count - 1 downto 0 do
        if FNotifyList.Contains(L2[I]) then
          L2[I].FreeNotification(Self);
    finally
      ALFreeAndNil(L2);
      ALFreeAndNil(FNotifyList);
    end;
  end;
  inherited;
end;

{**************************************************}
function TALDynamicControl.CreateMargins: TALBounds;
begin
  result := TALBounds.Create;
end;

{**************************************************}
function TALDynamicControl.CreatePadding: TALBounds;
begin
  result := TALBounds.Create;
end;

{****************************************}
procedure TALDynamicControl.DoBeginUpdate;
begin
end;

{**************************************}
procedure TALDynamicControl.DoEndUpdate;
begin
  Realign;
  // We should repaint control, because control could receive new children in BeginUpdate - EndUpdate phase.
  Repaint;
end;

{**************************************}
procedure TALDynamicControl.BeginUpdate;
begin
  if FUpdating = 0 then
    DoBeginUpdate;
  Inc(FUpdating);
  for var I := 0 to FControlsCount - 1 do
    FControls[I].BeginUpdate;
end;

{*********************************************}
function TALDynamicControl.IsUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

{*******************************************************************}
procedure TALDynamicControl.SetOwner(const Value: TALDynamicControl);
begin
  if Value <> fOwner then begin
    // this will set FOwner to nil
    if FOwner <> nil then
      FOwner.RemoveControl(self);
    // this will set FOwner = Value
    if Value <> nil then
      value.AddControl(Self);
  end;
end;

{**********************************************************************}
procedure TALDynamicControl.SetHost(Const Value: TALDynamicControlHost);
begin
  {$IF defined(DEBUG)}
  if (Owner <> nil) and (Value <> Owner.Host) then
    Raise exception.Create('Error 8FFB3C46-BFC2-4474-AAA0-001AF039D87E');
  {$ENDIF}

  if FHost <> value then begin

    If FHost <> nil then begin
      var LTmpControl := FHost.Captured;
      while (LTmpControl <> nil) do begin
        if LTmpControl = Self then begin
          Host.SetCaptured(nil);
          break;
        end;
        LTmpControl := LTmpControl.Owner;
      end;
      //--
      LTmpControl := Host.Hovered;
      while (LTmpControl <> nil) do begin
        if LTmpControl = Self then begin
          Host.SetHovered(nil);
          break;
        end;
        LTmpControl := LTmpControl.Owner;
      end;
    end;

    FHost := Value;

  end;
end;

{************************************************************************}
procedure TALDynamicControl.AddControl(const AControl: TALDynamicControl);
begin
  InsertControl(AControl, Maxint);
end;

{**************************************************************************************************}
procedure TALDynamicControl.InsertControl(const AControl: TALDynamicControl; const AIndex: Integer);
begin
  if (AControl <> nil) and (AControl.Owner <> Self) then begin
    AControl.Owner := nil;
    DoInsertControl(AControl, AIndex);
  end;
end;

{****************************************************************************************************}
procedure TALDynamicControl.DoInsertControl(const AControl: TALDynamicControl; const AIndex: Integer);
begin
  for var I := AControl.FUpdating downto FUpdating + 1 do AControl.EndUpdate;
  for var I := AControl.FUpdating to FUpdating - 1 do AControl.BeginUpdate;
  //--
  var LIndex := Max(0, Min(AIndex, FControlsCount));
  If length(FControls) <= FControlsCount then
    Setlength(FControls, FControlsCount + 1);
  if LIndex <= FControlsCount - 1 then begin
    ALMove(FControls[LIndex], FControls[LIndex+1], (FControlsCount - 1 - LIndex) * SizeOf(Pointer));
    For var I := LIndex + 1 to FControlsCount - 1 do
      FControls[I].FIndex := I;
  end;
  Inc(FControlsCount);
  FControls[LIndex] := AControl;
  AControl.FIndex := LIndex;
  AControl.FOwner := self;
  AControl.ParentChanged;
  if AControl.Align <> TALAlignLayout.None then
    Realign;
  Repaint;
end;

{***************************************************************************}
procedure TALDynamicControl.RemoveControl(const AControl: TALDynamicControl);
begin
  if (not FIsDestroying) and (AControl <> nil) and (AControl.Owner = Self) then
    DoRemoveControl(AControl);
end;

{***************************************************************}
procedure TALDynamicControl.RemoveControl(const AIndex: Integer);
begin
  if (not FIsDestroying) and (InRange(AIndex, 0, FControlsCount - 1)) then
    DoRemoveControl(FControls[AIndex]);
end;

{*****************************************************************************}
procedure TALDynamicControl.DoRemoveControl(const AControl: TALDynamicControl);
begin
  var LIndex := AControl.Index;
  if LIndex < FControlsCount - 1 then begin
    ALMove(FControls[LIndex + 1], FControls[LIndex], (FControlsCount - 1 - LIndex) * SizeOf(Pointer));
    For var I := LIndex to FControlsCount - 2 do
      FControls[I].FIndex := I;
  end;
  Dec(FControlsCount);
  AControl.FIndex := -1;
  AControl.FOwner := nil;
  AControl.SetHost(nil);
  If not AControl.FIsDestroying then begin
    AControl.ParentChanged;
    for var I := 1 to FUpdating do AControl.EndUpdate;
  end;
  if AControl.Align <> TALAlignLayout.None then
    Realign;
  Repaint;
end;

{***************************************************************************************************}
procedure TALDynamicControl.MoveControl(const AControl: TALDynamicControl; const ANewIndex: Integer);
begin
  IF AControl.Owner <> self then
    raise Exception.Create('AControl is not a child of this control');
  var LNewIndex := Max(0, Min(ANewIndex, FControlsCount - 1));
  if AControl.Index = LNewIndex then exit;
  DoMoveControl(AControl, LNewIndex);
end;

{*****************************************************************************************************}
procedure TALDynamicControl.DoMoveControl(const AControl: TALDynamicControl; const ANewIndex: Integer);
begin
  var LOldIndex := AControl.Index;
  if LOldIndex < ANewIndex then begin
    Move(FControls[LOldIndex + 1], FControls[LOldIndex], (ANewIndex - LOldIndex) * SizeOf(Pointer));
    FControls[ANewIndex] := AControl;
    For var I := LOldIndex to ANewIndex do
      FControls[I].FIndex := I;
  end
  else begin
    Move(FControls[ANewIndex], FControls[ANewIndex + 1], (LOldIndex - ANewIndex) * SizeOf(Pointer));
    FControls[ANewIndex] := AControl;
    For var I := ANewIndex to LOldIndex do
      FControls[I].FIndex := I;
  end;
  if AControl.Align <> TALAlignLayout.None then
    Realign;
  Repaint;
end;

{*************************************************************************************}
function TALDynamicControl.GetControlByIndex(Const AIndex: Integer): TALDynamicControl;
begin
  {$IF defined(debug)}
  if (AIndex < 0) or (AIndex > FControlsCount - 1) then raise Exception.Create('Index is out of bounds');
  {$ENDIF}
  result := FControls[AIndex];
end;

{**********************************************************************************}
function TALDynamicControl.GetControlByName(Const AName: String): TALDynamicControl;
begin
  Result := nil;
  For var i := 0 to FControlsCount - 1 do
    if FControls[I].Name = AName then
      exit(FControls[I]);
end;

{*****************************************}
function TALDynamicControl.GetControlAtPos(
           const APos: TALPointD; // APos is local to the control
           out AControlPos: TALPointD; // AControlPos is local to the founded control
           const ACheckHitTest: Boolean = true): TALDynamicControl;
begin
  if (not Visible) or (not AbsoluteEnabled) then begin
    AControlPos := TALPointD.Zero;
    exit(nil);
  end;
  //if (ClipChildren) and (not ExpandedLocalRect.Contains(aPos)) then begin
  //  AControlPos := TALPointD.Zero;
  //  exit(nil);
  //end;
  if ControlsCount > 0 then
    for var I := GetLastVisibleObjectIndex downto GetFirstVisibleObjectIndex do
    begin
      var LControl := Controls[I];
      if not IsVisibleObject(LControl) then
        Continue;
      result := LControl.GetControlAtPos(
                  APos - LControl.BoundsRect.TopLeft,
                  AControlPos,
                  ACheckHitTest);
      if result <> nil then
        exit;
    end;

  if ExpandedLocalRect.Contains(aPos) and ((not ACheckHitTest) or (HitTest)) then begin
    result := Self;
    AControlPos := aPos;
  end
  else begin
    AControlPos := TALPointD.Zero;
    exit(nil);
  end;
end;

{*****************************************}
function TALDynamicControl.GetControlAtPos(
           const APos: TALPointD;
           const ACheckHitTest: Boolean = true): TALDynamicControl;
begin
  Var LControlPos: TALPointD;
  result := GetControlAtPos(aPos, LControlPos, ACheckHitTest);
end;

{***************************************************************************}
procedure TALDynamicControl.AddFreeNotify(const AControl: TALDynamicControl);
begin
  if AControl <> nil then begin
    if FNotifyList = nil then FNotifyList := TList<TALDynamicControl>.Create;
    FNotifyList.Add(AControl);
  end;
end;

{******************************************************************************}
procedure TALDynamicControl.RemoveFreeNotify(const AControl: TALDynamicControl);
begin
  if FNotifyList <> nil then
    FNotifyList.Remove(AControl);
end;

{*****************************************************************************}
procedure TALDynamicControl.FreeNotification(const AObject: TALDynamicControl);
begin
  // Virtual;
end;

{**********************************}
procedure TALDynamicControl.Repaint;
begin
  If Host <> nil then
    Host.Repaint;
end;

{*********************************************}
function TALDynamicControl.GetTagFloat: Double;
begin
  ALMove(FTag, Result, SizeOf(Double));
end;

{************************************************************}
procedure TALDynamicControl.SetTagFloat(const AValue: Double);
begin
  ALMove(AValue, FTag, SizeOf(Double));
end;

{**********************************************************}
procedure TALDynamicControl.SetIndex(const AValue: Integer);
begin
  if (Owner <> nil) then
    Owner.MoveControl(Self,AValue);
end;

{***************************************}
procedure TALDynamicControl.BringToFront;
begin
  If Owner <> nil then
    Owner.MoveControl(Self,MaxInt);
end;

{*************************************}
procedure TALDynamicControl.SendToBack;
begin
  If Owner <> nil then
    Owner.MoveControl(Self,0);
end;

{**********************************************************}
procedure TALDynamicControl.SetPivot(const AValue: TPointF);
begin
  if not FPivot.EqualsTo(AValue, TEpsilon.Scale) then begin
    FPivot := AValue;
    Repaint;
  end;
end;

{*****************************************************************}
procedure TALDynamicControl.SetRotationAngle(const AValue: Single);
begin
  if not SameValue(FRotationAngle, AValue, TEpsilon.Scale) then begin
    FRotationAngle := AValue;
    Repaint;
  end;
end;

{**********************************************************}
procedure TALDynamicControl.SetScale(const AValue: TPointF);
begin
  if not FScale.EqualsTo(AValue, TEpsilon.Scale) then begin
    FScale := AValue;
    Repaint;
  end;
end;

{*******************************************************************}
procedure TALDynamicControl.SetBounds(X, Y, AWidth, AHeight: Double);
begin
  AWidth := System.Math.Max(0, AWidth);
  AHeight := System.Math.Max(0, AHeight);
  var LMoved := not (SameValue(X, FLeft, TEpsilon.Position) and SameValue(Y, FTop, TEpsilon.Position));
  var LSizeChanged := not (SameValue(AWidth, FWidth, TEpsilon.Position) and SameValue(AHeight, FHeight, TEpsilon.Position));
  if LMoved or LSizeChanged then begin
    FLeft := X;
    FTop := Y;
    FWidth := AWidth;
    FHeight := AHeight;
    if LMoved then PositionChanged;
    if LSizeChanged then SizeChanged;
  end;
end;

{*************************************************}
function TALDynamicControl.GetBoundsRect: TALRectD;
begin
  Result := TALRectD.Create(Left, Top, Left + Width, Top + Height);
end;

{***************************************************************}
procedure TALDynamicControl.SetBoundsRect(const Value: TALRectD);
begin
  SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);
end;

{************************************************}
function TALDynamicControl.GetLocalRect: TALRectD;
begin
  Result := TALRectD.Create(0, 0, Width, Height);
end;

{********************************************************}
function TALDynamicControl.GetExpandedLocalRect: TALRectD;
begin
  result := TALRectD.Create(
              0-TouchTargetExpansion.Left,
              0-TouchTargetExpansion.top,
              Width+TouchTargetExpansion.right,
              Height+TouchTargetExpansion.bottom);
end;

{***************************************************}
function TALDynamicControl.GetAbsoluteRect: TALRectD;
begin
  Result := TALRectD.Create(LocalToAbsolute(LocalRect));
end;

{**********************************************************}
function TALDynamicControl.GetAbsoluteDisplayedRect: TRectF;
begin
  if (FIsDestroying) or (not FAbsoluteVisible) or (Host = nil) then Exit(TRectF.Empty);
  // This function assumes that ClipChildren is not implemented.
  // If this changes, use the implementation found in TALControl.GetAbsoluteDisplayedRect.
  Result := TRectF.Intersect(Host.LocalToAbsolute(Host.DisplayedRect), AbsoluteRect.ReducePrecision);
end;

{*********************************************************}
function TALDynamicControl.GetExpandedBoundsRect: TALRectD;
begin
  result := TALRectD.Create(
              Left-TouchTargetExpansion.Left,
              top-TouchTargetExpansion.top,
              Right+TouchTargetExpansion.right,
              Bottom+TouchTargetExpansion.bottom);
end;

{*****************************************************}
function TALDynamicControl.GetAbsoluteVisible: Boolean;
begin
  Result := (FAbsoluteVisible) and (Host <> nil) and (Host.GetParentedVisible);
end;

{************************************************}
function TALDynamicControl.GetPosition: TALPointD;
begin
  Result := TALPointD.Create(FLeft, FTop);
end;

{***************************************************************}
procedure TALDynamicControl.SetPosition(const AValue: TALPointD);
begin
  SetBounds(AValue.X, AValue.Y, FWidth, FHeight);
end;

{*************************************************************}
procedure TALDynamicControl.SetPosition(const AValue: TPointf);
begin
  SetBounds(AValue.X, AValue.Y, FWidth, FHeight);
end;

{*****************************************************************}
procedure TALDynamicControl.SetPosition(const ALeft, ATop: Double);
begin
  SetBounds(ALeft, ATop, FWidth, FHeight);
end;

{*********************************************************}
procedure TALDynamicControl.SetSize(const ASize: TALSizeD);
begin
  SetBounds(Fleft, FTop, ASize.Width, ASize.Height);
end;

{*******************************************************}
procedure TALDynamicControl.SetSize(const ASize: TSizeF);
begin
  SetBounds(Fleft, FTop, ASize.Width, ASize.Height);
end;

{*****************************************************************}
procedure TALDynamicControl.SetSize(const AWidth, AHeight: Double);
begin
  SetBounds(Fleft, FTop, AWidth, AHeight);
end;

{*******************************************************}
procedure TALDynamicControl.SetLeft(const Value: Double);
begin
  SetBounds(Value, FTop, FWidth, FHeight);
end;

{******************************************************}
procedure TALDynamicControl.SetTop(const Value: Double);
begin
  SetBounds(FLeft, Value, FWidth, FHeight);
end;

{********************************************************}
procedure TALDynamicControl.SetWidth(const Value: Double);
begin
  SetBounds(FLeft, FTop, Value, FHeight);
end;

{*********************************************************}
procedure TALDynamicControl.SetHeight(const Value: Double);
begin
  SetBounds(FLeft, FTop, FWidth, Value);
end;

{******************************************}
function TALDynamicControl.GetRight: Double;
begin
  Result := Left + Width;
end;

{*******************************************}
function TALDynamicControl.GetBottom: Double;
begin
  Result := Top + Height;
end;

{*****************************************************************************}
function TALDynamicControl.AbsoluteToLocal(const APoint: TALPointD): TALPointD;
begin
  if Owner <> nil then Result := Owner.AbsoluteToLocal(APoint)
  else if Host <> nil then begin
    Result := APoint;
    Result.Offset(-Host.AbsoluteToLocal(TPointF.Zero));
  end
  else Result := APoint;
  Result.Offset(-Left, -Top);
end;

{**************************************************************************}
function TALDynamicControl.AbsoluteToLocal(const ARect: TALRectD): TALRectD;
begin
  if Owner <> nil then Result := Owner.AbsoluteToLocal(ARect)
  else if Host <> nil then begin
    Result := ARect;
    Result.Offset(-Host.AbsoluteToLocal(TPointF.Zero));
  end
  else Result := ARect;
  Result.Offset(-Left, -Top);
end;

{***************************************************************************}
function TALDynamicControl.AbsoluteToLocal(const APoint: TPointF): TALPointD;
begin
  Result := AbsoluteToLocal(TALPointD.Create(APoint));
end;

{************************************************************************}
function TALDynamicControl.AbsoluteToLocal(const ARect: TRectF): TALRectD;
begin
  Result := AbsoluteToLocal(TALRectD.Create(ARect));
end;

{*****************************************************************************}
function TALDynamicControl.LocalToAbsolute(const APoint: TALPointD): TALPointD;
begin
  Result := APoint;
  Result.Offset(left, top);
  if Owner <> nil then result := Owner.LocalToAbsolute(Result)
  else if Host <> nil then Result.Offset(Host.LocalToAbsolute(TPointF.Zero));
end;

{**************************************************************************}
function TALDynamicControl.LocalToAbsolute(const ARect: TALRectD): TALRectD;
begin
  Result := ARect;
  Result.Offset(left, top);
  if Owner <> nil then result := Owner.LocalToAbsolute(Result)
  else if Host <> nil then Result.Offset(Host.LocalToAbsolute(TPointF.Zero));
end;

{***************************************************************************}
function TALDynamicControl.LocalToAbsolute(const APoint: TPointF): TALPointD;
begin
  result := LocalToAbsolute(TALPointD.Create(APoint));
end;

{************************************************************************}
function TALDynamicControl.LocalToAbsolute(const ARect: TRectF): TALRectD;
begin
  result := LocalToAbsolute(TALRectD.Create(ARect));
end;

{*******************************************************************}
function TALDynamicControl.PointInObjectLocal(X, Y: Double): Boolean;
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.PointInObjectLocal was not updated and adjust the IFDEF'}
  {$ENDIF}
  Result := (X >= (0 - TouchTargetExpansion.Left)) and
            (X <= (Width + TouchTargetExpansion.Right)) and
            (Y >= (0 - TouchTargetExpansion.Top)) and
            (Y <= (Height + TouchTargetExpansion.Bottom));
end;

{************************************************}
function TALDynamicControl.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(50, 50);
end;

{************************************************************************}
procedure TALDynamicControl.SetTouchTargetExpansion(const AValue: TRectF);
begin
  FTouchTargetExpansion := AValue;
end;

{***********************************************************}
procedure TALDynamicControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    EnabledChanged;
  end;
end;

{*************************************************}
procedure TALDynamicControl.refreshAbsoluteEnabled;
begin
  var LNewAbsoluteEnabled: Boolean;
  if (Owner <> nil) and (not FOwner.AbsoluteEnabled) then LNewAbsoluteEnabled := False
  else LNewAbsoluteEnabled := Enabled;
  if LNewAbsoluteEnabled <> FAbsoluteEnabled then begin
    FAbsoluteEnabled := LNewAbsoluteEnabled;
    For var I := 0 to FControlsCount - 1 do
      FControls[i].RefreshAbsoluteEnabled;
    Repaint;
  end;
end;

{***********************************************************}
procedure TALDynamicControl.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then begin
    FVisible := Value;
    VisibleChanged;
  end;
end;

{*************************************************************}
function TALDynamicControl.GetFirstVisibleObjectIndex: Integer;
begin
  Result := 0;
end;

{************************************************************}
function TALDynamicControl.GetLastVisibleObjectIndex: Integer;
begin
  // Note: TControl.GetLastVisibleObjectIndex returns FControls.Count,
  // which refers to an index beyond the last valid one.
  // We return the correct last index here.
  Result := FControlsCount - 1;
end;

{************************************************************************************}
function TALDynamicControl.IsVisibleObject(const AObject: TALDynamicControl): Boolean;
begin
  result := AObject.Visible;
end;

{*************************************************}
procedure TALDynamicControl.RefreshAbsoluteVisible;
begin
  var LNewAbsoluteVisible: Boolean;
  if (Owner <> nil) and (not FOwner.FAbsoluteVisible) then LNewAbsoluteVisible := False
  else LNewAbsoluteVisible := Visible;
  if LNewAbsoluteVisible <> FAbsoluteVisible then begin
    FAbsoluteVisible := LNewAbsoluteVisible;
    For var I := 0 to FControlsCount - 1 do
      FControls[i].RefreshAbsoluteVisible;
    Repaint;
  end;
end;

{**********************************************************}
procedure TALDynamicControl.SetOpacity(const Value: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.SetOpacity was not updated and adjust the IFDEF'}
  {$ENDIF}
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := EnsureRange(Value, 0, 1);
    RefreshAbsoluteOpacity;
  end;
end;

{******************************************************************}
procedure TALDynamicControl.SetDisabledOpacity(const Value: Single);
begin
  if not SameValue(FDisabledOpacity, Value, TEpsilon.Scale) then begin
    FDisabledOpacity := Value;
    if FDisabledOpacity < 0 then FDisabledOpacity := 0;
    if FDisabledOpacity > 1 then FDisabledOpacity := 1;
    if not Enabled then
      RefreshAbsoluteOpacity;
  end;
end;

{*************************************************}
procedure TALDynamicControl.RefreshAbsoluteOpacity;
begin
  var LNewAbsoluteOpacity: Single;
  if Owner <> nil then LNewAbsoluteOpacity := Opacity * Owner.AbsoluteOpacity
  else LNewAbsoluteOpacity := Opacity;
  if not Enabled then
    LNewAbsoluteOpacity := LNewAbsoluteOpacity * DisabledOpacity;
  if not SameValue(LNewAbsoluteOpacity, FAbsoluteOpacity, TEpsilon.Scale) then begin
    FAbsoluteOpacity := LNewAbsoluteOpacity;
    For var I := 0 to FControlsCount - 1 do
      FControls[i].RefreshAbsoluteOpacity;
    Repaint;
  end;
end;

{********************************************}
function TALDynamicControl.GetCursor: TCursor;
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  Result := FCursor;
  {$ELSE}
  Result := crDefault;
  {$ENDIF}
end;

{**********************************************************}
procedure TALDynamicControl.SetCursor(const Value: TCursor);
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  if FCursor <> Value then begin
    FCursor := Value;
    RefreshAbsoluteCursor;
  end;
  {$ENDIF}
end;

{****************************************************}
function TALDynamicControl.GetAbsoluteCursor: TCursor;
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  Result := FAbsoluteCursor;
  {$ELSE}
  Result := crDefault;
  {$ENDIF}
end;

{************************************************}
procedure TALDynamicControl.RefreshAbsoluteCursor;
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  var LNewAbsoluteCursor: TCursor;
  if (Cursor = crDefault) and (Owner <> nil) then LNewAbsoluteCursor := Owner.AbsoluteCursor
  else LNewAbsoluteCursor := Cursor;
  if LNewAbsoluteCursor <> FAbsoluteCursor then begin
    FAbsoluteCursor := LNewAbsoluteCursor;
    if IsMouseOver and (Host <> nil) then
      Host.Cursor := FAbsoluteCursor;
    For var I := 0 to FControlsCount - 1 do
      FControls[i].RefreshAbsoluteCursor;
  end;
  {$ENDIF}
end;

{****************************************************************}
procedure TALDynamicControl.SetAlign(const Value: TALAlignLayout);
begin
  {$IF defined(DEBUG)}
  //if Value in [TALAlignLayout.Contents,
  //             TALAlignLayout.Scale,
  //             TALAlignLayout.Fit,
  //             TALAlignLayout.FitLeft,
  //             TALAlignLayout.FitRight] then
  //  Raise Exception.Create('Unsupported Align value');
  {$ENDIF}
  if FAlign <> Value then begin
    FAlign := Value;
    If Owner <> nil then
      Owner.Realign;
  end;
end;

{****************************************************}
function TALDynamicControl.GetForm: TCommonCustomForm;
begin
  if Host <> nil then
    Result := Host.Form
  else
    result := nil;
end;

{*************}
//[MultiThread]
function TALDynamicControl.DoGetDownloadPriority: Int64;
begin
  // This method is called from a background thread, so Owner
  // might be modified concurrently. To ensure thread safety,
  // we assign it to a local variable before using it.
  var LOwner := Owner;
  if LOwner <> nil then Result := {Result=0 +} LOwner.DoGetDownloadPriority
  else Result := 0;
end;

{*************}
//[MultiThread]
class function TALDynamicControl.GetDownloadPriority(const AContext: Tobject): Int64;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GetlowestPriority: Int64;
  begin
    if TALNetHttpClientPool.Instance.PriorityDirection = TALNetHttpClientPool.TPriorityDirection.lessThan then
      Result := ALMaxInt
    else
      Result := -ALMaxInt;
  end;

begin

  // The code below is not intended to be accessed from a background thread.
  // However, in our case, we’re only reading values without modifying anything,
  // so occasional inaccuracies are acceptable. Normally, we shouldn't encounter
  // an EAccessViolation (since TALDynamicControl instances are released in delayed mode),
  // but we still catch any exceptions just in case.

  Try
    Var LOwner := TALDynamicControl(_TALWorkerContextProtectedAccess(AContext).FOwner);
    if LOwner = nil then exit(_GetlowestPriority);
    if LOwner.Host = nil then begin
      if TALNetHttpClientPool.Instance.PriorityDirection = TALNetHttpClientPool.TPriorityDirection.lessThan then
        Result := -ALMaxInt
      else
        Result := ALMaxInt;
    end
    else begin
      result := LOwner.DoGetDownloadPriority;
      var LNetHttpClientPool := TALNetHttpClientPool.Instance;
      if LNetHttpClientPool.PriorityDirection = TALNetHttpClientPool.TPriorityDirection.lessThan then
        Result := LNetHttpClientPool.PriorityStartingPoint - Result
      else
        Result := LNetHttpClientPool.PriorityStartingPoint + Result;
    end;
  Except
    On E: Exception do begin
      ALLog('TALDynamicControl.GetDownloadPriority', E);
      Result := _GetlowestPriority;
    end;
  End;

end;

{**********************************************************}
function TALDynamicControl.GetScrollEngine: TALScrollEngine;
begin
  Result := Nil;
end;

{*******************************************************}
function TALDynamicControl.FillTextFlags: TFillTextFlags;
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.FillTextFlags was not updated and adjust the IFDEF'}
  {$ENDIF}
  if (Form = nil) then result := ALGetFillTextFlags
  else if (Form.BiDiMode = bdRightToLeft) then
    Result := [TFillTextFlag.RightToLeft]
  else
    Result := [];
end;

{**************************************************************}
procedure TALDynamicControl.SetPadding(const AValue: TALBounds);
begin
  FPadding.Assign(AValue);
end;

{**************************************************************}
procedure TALDynamicControl.SetMargins(const AValue: TALBounds);
begin
  FMargins.Assign(AValue);
end;

{*****************************************************************}
procedure TALDynamicControl.PaddingChangedHandler(Sender: TObject);
begin
  PaddingChanged;
end;

{*****************************************************************}
procedure TALDynamicControl.MarginsChangedHandler(Sender: TObject);
begin
  MarginsChanged;
end;

{*****************************************}
procedure TALDynamicControl.PaddingChanged;
begin
  Realign;
end;

{*****************************************}
procedure TALDynamicControl.MarginsChanged;
begin
  if Owner <> nil then
    Owner.Realign;
end;

{*****************************************}
procedure TALDynamicControl.EnabledChanged;
begin
  RefreshAbsoluteEnabled;
  RefreshAbsoluteOpacity;
end;

{*****************************************}
procedure TALDynamicControl.VisibleChanged;
begin
  RefreshAbsoluteVisible;
  AncestorVisibleChanged(FVisible);
  if (Align <> TALAlignLayout.None) and (Owner <> nil) then
    Owner.Realign;
end;

{**************************************************************************}
procedure TALDynamicControl.AncestorVisibleChanged(const AVisible: Boolean);
begin
  for var I := 0 to FControlsCount - 1 do
    FControls[I].AncestorVisibleChanged(AVisible);
end;

{****************************************}
procedure TALDynamicControl.ParentChanged;
begin
  RefreshAbsoluteCursor;
  RefreshAbsoluteOpacity;
  RefreshAbsoluteEnabled;
  RefreshAbsoluteVisible;
  AncestorParentChanged;
end;

{************************************************}
procedure TALDynamicControl.AncestorParentChanged;
begin
  If FOwner <> nil then SetHost(FOwner.FHost);
  for var I := 0 to FControlsCount - 1 do
    FControls[I].AncestorParentChanged;
end;

{******************************************}
procedure TALDynamicControl.PositionChanged;
begin
  if (Align <> TALAlignLayout.None) and (Owner <> nil) then
    Owner.Realign;
  Repaint;
end;

{**************************************}
procedure TALDynamicControl.SizeChanged;
begin
  if (Align <> TALAlignLayout.None) and (Owner <> nil) then
    Owner.Realign;
  Realign;
  Repaint;
  DoResized
end;

{************************************}
procedure TALDynamicControl.DoResized;
begin
  if Assigned(FOnResized) then
    FOnResized(Self);
end;

{**********************************}
procedure TALDynamicControl.Realign;
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.Realign was not updated and adjust the IFDEF'}
  {$ENDIF}
  if IsDestroying then
    Exit;
  if FDisableAlign then
    Exit;
  if IsUpdating then
    Exit;
  DoRealign;
end;

{************************************}
procedure TALDynamicControl.DoRealign;
begin

  {$REGION 'Exit if no alignment is needed'}
  if (FIsDestroying) or
     (FDisableAlign) or
     (controlsCount = 0) then exit;
  {$ENDREGION}

  FDisableAlign := True;
  try

    {$REGION 'Initialize LClientRect based on bounds and padding'}
    var LClientRect: TALRectD;
    LClientRect := TALRectD.Create(
                     Padding.Left, {Left}
                     Padding.Top, {Top}
                     Width - Padding.Right, {Right}
                     Height - Padding.Bottom); {Bottom}
    {$ENDREGION}

    {$REGION 'First Pass: MostTop, MostTopCenter, ...'}
    for var I := 0 to FControlsCount - 1 do begin
      var LControl := FControls[i];
      if not LControl.Visible then Continue;
      case LControl.Align of

        {$REGION 'MostTop'}
        TALAlignLayout.MostTop:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Top := LControl.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'MostTopCenter'}
        TALAlignLayout.MostTopCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.Width) / 2, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.Width) / 2, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Top := LControl.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'MostTopLeft'}
        TALAlignLayout.MostTopLeft:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Top := LControl.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'MostTopRight'}
        TALAlignLayout.MostTopRight:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Top := LControl.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'MostLeft'}
        TALAlignLayout.MostLeft:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Left := LControl.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'MostLeftCenter'}
        TALAlignLayout.MostLeftCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.Height) / 2, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.Height) / 2)); {Bottom}
          LClientRect.Left := LControl.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'MostLeftTop'}
        TALAlignLayout.MostLeftTop:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Left := LControl.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'MostLeftBottom'}
        TALAlignLayout.MostLeftBottom:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Left := LControl.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'MostRight'}
        TALAlignLayout.MostRight:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Right := LControl.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'MostRightCenter'}
        TALAlignLayout.MostRightCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.Height) / 2, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.Height) / 2)); {Bottom}
          LClientRect.Right := LControl.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'MostRightTop'}
        TALAlignLayout.MostRightTop:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Right := LControl.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'MostRightBottom'}
        TALAlignLayout.MostRightBottom:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Right := LControl.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'MostBottom'}
        TALAlignLayout.MostBottom:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'MostBottomCenter'}
        TALAlignLayout.MostBottomCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.Width) / 2, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.Width) / 2, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'MostBottomLeft'}
        TALAlignLayout.MostBottomLeft:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'MostBottomRight'}
        TALAlignLayout.MostBottomRight:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'None,Center,VertCenter,HorzCenter,Horizontal,Vertical,Client'}
        TALAlignLayout.None,
        TALAlignLayout.Top,
        TALAlignLayout.Bottom,
        TALAlignLayout.Left,
        TALAlignLayout.Right,
        TALAlignLayout.TopCenter,
        TALAlignLayout.TopLeft,
        TALAlignLayout.TopRight,
        TALAlignLayout.LeftCenter,
        TALAlignLayout.LeftTop,
        TALAlignLayout.LeftBottom,
        TALAlignLayout.RightCenter,
        TALAlignLayout.RightTop,
        TALAlignLayout.RightBottom,
        TALAlignLayout.BottomCenter,
        TALAlignLayout.BottomLeft,
        TALAlignLayout.BottomRight,
        TALAlignLayout.Center,
        TALAlignLayout.VertCenter,
        TALAlignLayout.HorzCenter,
        TALAlignLayout.Horizontal,
        TALAlignLayout.Vertical,
        TALAlignLayout.Client:;
        {$ENDREGION}

        {$REGION 'Unknown'}
        else
          raise Exception.Create('Error 5B11A29B-7DAF-4C08-BD96-72F49D47D198');
        {$ENDREGION}

      end;
    end;
    {$ENDREGION}

    {$REGION 'Second Pass: Top, TopCenter, ...'}
    for var I := 0 to FControlsCount - 1 do begin
      var LControl := FControls[i];
      if not LControl.Visible then Continue;
      case LControl.Align of

        {$REGION 'Top'}
        TALAlignLayout.Top:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Top := LControl.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'TopCenter'}
        TALAlignLayout.TopCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.Width) / 2, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.Width) / 2, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Top := LControl.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'TopLeft'}
        TALAlignLayout.TopLeft:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Top := LControl.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'TopRight'}
        TALAlignLayout.TopRight:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Top := LControl.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'Left'}
        TALAlignLayout.Left:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Left := LControl.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'LeftCenter'}
        TALAlignLayout.LeftCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.Height) / 2, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.Height) / 2)); {Bottom}
          LClientRect.Left := LControl.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'LeftTop'}
        TALAlignLayout.LeftTop:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Left := LControl.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'LeftBottom'}
        TALAlignLayout.LeftBottom:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Left := LControl.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'Right'}
        TALAlignLayout.Right:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Right := LControl.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'RightCenter'}
        TALAlignLayout.RightCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.Height) / 2, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.Height) / 2)); {Bottom}
          LClientRect.Right := LControl.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'RightTop'}
        TALAlignLayout.RightTop:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.Height)); {Bottom}
          LClientRect.Right := LControl.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'RightBottom'}
        TALAlignLayout.RightBottom:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Right := LControl.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'Bottom'}
        TALAlignLayout.Bottom:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'BottomCenter'}
        TALAlignLayout.BottomCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.Width) / 2, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.Width) / 2, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'BottomLeft'}
        TALAlignLayout.BottomLeft:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.Width, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'BottomRight'}
        TALAlignLayout.BottomRight:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.Width, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.Height, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'None,Center,VertCenter,HorzCenter,Horizontal,Vertical,Client'}
        TALAlignLayout.None,
        TALAlignLayout.MostTop,
        TALAlignLayout.MostBottom,
        TALAlignLayout.MostLeft,
        TALAlignLayout.MostRight,
        TALAlignLayout.MostTopCenter,
        TALAlignLayout.MostTopLeft,
        TALAlignLayout.MostTopRight,
        TALAlignLayout.MostLeftCenter,
        TALAlignLayout.MostLeftTop,
        TALAlignLayout.MostLeftBottom,
        TALAlignLayout.MostRightCenter,
        TALAlignLayout.MostRightTop,
        TALAlignLayout.MostRightBottom,
        TALAlignLayout.MostBottomCenter,
        TALAlignLayout.MostBottomLeft,
        TALAlignLayout.MostBottomRight,
        TALAlignLayout.Center,
        TALAlignLayout.VertCenter,
        TALAlignLayout.HorzCenter,
        TALAlignLayout.Horizontal,
        TALAlignLayout.Vertical,
        TALAlignLayout.Client:;
        {$ENDREGION}

        {$REGION 'Unknown'}
        else
          raise Exception.Create('Error 8A5743C0-D90D-4845-A19C-1F9E792AD0A4');
        {$ENDREGION}

      end;
    end;
    {$ENDREGION}

    {$REGION 'Third pass: Client, Center, ...'}
    for var I := 0 to FControlsCount - 1 do
    begin
      var LControl := FControls[i];
      if not LControl.Visible then Continue;
      case LControl.Align of

        {$REGION 'Client'}
        TALAlignLayout.Client:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'Center'}
        TALAlignLayout.Center:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.Width) / 2, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.Height) / 2, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.Width) / 2, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.Height) / 2)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'Horizontal'}
        TALAlignLayout.Horizontal:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LControl.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LControl.Bottom)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'Vertical'}
        TALAlignLayout.Vertical:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LControl.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LControl.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'VertCenter'}
        TALAlignLayout.VertCenter: begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.Height) / 2, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.Height) / 2)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'HorzCenter'}
        TALAlignLayout.HorzCenter: begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.Width) / 2, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.Width) / 2, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'None,Top,Left,Right,Bottom,TopCenter,LeftCenter,RightCenter,BottomCenter'}
        TALAlignLayout.None,
        TALAlignLayout.Top,
        TALAlignLayout.Left,
        TALAlignLayout.Right,
        TALAlignLayout.Bottom,
        TALAlignLayout.MostTop,
        TALAlignLayout.MostBottom,
        TALAlignLayout.MostLeft,
        TALAlignLayout.MostRight,
        TALAlignLayout.TopCenter,
        TALAlignLayout.TopLeft,
        TALAlignLayout.TopRight,
        TALAlignLayout.LeftCenter,
        TALAlignLayout.LeftTop,
        TALAlignLayout.LeftBottom,
        TALAlignLayout.RightCenter,
        TALAlignLayout.RightTop,
        TALAlignLayout.RightBottom,
        TALAlignLayout.BottomCenter,
        TALAlignLayout.BottomLeft,
        TALAlignLayout.BottomRight,
        TALAlignLayout.MostTopCenter,
        TALAlignLayout.MostTopLeft,
        TALAlignLayout.MostTopRight,
        TALAlignLayout.MostLeftCenter,
        TALAlignLayout.MostLeftTop,
        TALAlignLayout.MostLeftBottom,
        TALAlignLayout.MostRightCenter,
        TALAlignLayout.MostRightTop,
        TALAlignLayout.MostRightBottom,
        TALAlignLayout.MostBottomCenter,
        TALAlignLayout.MostBottomLeft,
        TALAlignLayout.MostBottomRight:;
        {$ENDREGION}

        {$REGION 'Unknown'}
        else
          raise Exception.Create('Error 6C6651D9-1DEC-49B4-9741-9BDFB28093A8');
        {$ENDREGION}

      end;
    end;
    {$ENDREGION}

  finally
    FDisableAlign := False;
  end;

end;

{*************************************}
procedure TALDynamicControl.MouseEnter;
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.DoMouseEnter was not updated and adjust the IFDEF'}
  {$ENDIF}
  FIsMouseOver := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  if fOwner <> nil then fOwner.ChildrenMouseEnter(Self); // https://quality.embarcadero.com/browse/RSP-24397
end;

{*************************************}
procedure TALDynamicControl.MouseLeave;
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.DoMouseLeave was not updated and adjust the IFDEF'}
  {$ENDIF}
  FIsMouseOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  if fOwner <> nil then fOwner.ChildrenMouseLeave(Self); // https://quality.embarcadero.com/browse/RSP-24397
end;

{********************************************************************************************}
procedure TALDynamicControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseDown was not updated and adjust the IFDEF'}
  {$ENDIF}
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  if fOwner <> nil then fOwner.ChildrenMouseDown(Self, Button, Shift, X, Y); // https://quality.embarcadero.com/browse/RSP-24397
  if FAutoCapture then
    Capture;
  if Button = TMouseButton.mbLeft then begin
    FPressed := True;
    FPressedPosition := TPointF.Create(X, Y);
  end;
end;

{**********************************************************************}
procedure TALDynamicControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseMove was not updated and adjust the IFDEF'}
  {$ENDIF}
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
  if fOwner <> nil then fOwner.ChildrenMouseMove(Self, Shift, X, Y); // https://quality.embarcadero.com/browse/RSP-24397
end;

{******************************************************************************************}
procedure TALDynamicControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseUp was not updated and adjust the IFDEF'}
  {$ENDIF}
  ReleaseCapture;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  if fOwner <> nil then fOwner.ChildrenMouseUp(Self, Button, Shift, X, Y); // https://quality.embarcadero.com/browse/RSP-24397
  FPressed := False;
end;

{*********************************************************************************************}
procedure TALDynamicControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseClick was not updated and adjust the IFDEF'}
  {$ENDIF}
  if AbsoluteEnabled and FPressed and PointInObjectLocal(X, Y) then begin
    Click;
    FPressed := False;
  end;
end;

{****************************************************************************************************}
procedure TALDynamicControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseWheel was not updated and adjust the IFDEF'}
  {$ENDIF}
  //if Assigned(FOnMouseWheel) then
  //  FOnMouseWheel(Self, Shift, WheelDelta, Handled)
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicControl.ChildrenMouseDown(const AObject: TALDynamicControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if fOwner <> nil then
    fOwner.ChildrenMouseDown(AObject, Button, Shift, X, Y);
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicControl.ChildrenMouseMove(const AObject: TALDynamicControl; Shift: TShiftState; X, Y: Single);
begin
  if fOwner <> nil then
    fOwner.ChildrenMouseMove(AObject, Shift, X, Y);
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicControl.ChildrenMouseUp(const AObject: TALDynamicControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if fOwner <> nil then
    fOwner.ChildrenMouseUp(AObject, Button, Shift, X, Y);
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicControl.ChildrenMouseEnter(const AObject: TALDynamicControl);
begin
  if fOwner <> nil then
    fOwner.ChildrenMouseEnter(AObject);
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicControl.ChildrenMouseLeave(const AObject: TALDynamicControl);
begin
  if fOwner <> nil then
    fOwner.ChildrenMouseLeave(AObject);
end;

{********************************}
procedure TALDynamicControl.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self)
end;

{***********************************}
procedure TALDynamicControl.DblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self)
end;

{**********************************}
procedure TALDynamicControl.Capture;
begin
  if Host <> nil then
    Host.SetCaptured(Self);
end;

{*****************************************}
procedure TALDynamicControl.ReleaseCapture;
begin
  if (Host <> nil) and (Host.Captured = Self) then
    Host.SetCaptured(nil);
end;

{****************************************************************}
procedure TALDynamicControl.PaintInternal(const ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  if not PaintChildrenOnly then begin
    Painting;
    Paint;
    PaintChildren;
    AfterPaint;
  end
  else
    PaintChildren;
  FCanvas := Nil;
end;

{***********************************}
procedure TALDynamicControl.Painting;
begin
  if Assigned(FOnPainting) then
    FOnPainting(Self, Canvas);
end;

{********************************}
procedure TALDynamicControl.Paint;
begin
end;

{****************************************************}
function TALDynamicControl.PaintChildrenOnly: Boolean;
begin
  result := False;
end;

{****************************************}
procedure TALDynamicControl.PaintChildren;
begin
  if FControlsCount > 0 then begin
    var LSavedMatrix := Canvas.Matrix;
    try
      for var i := GetFirstVisibleObjectIndex to GetLastVisibleObjectIndex do begin
        Var LControl := FControls[i];
        if not LControl.Visible then continue;
        // Unfortunately, TMatrix uses Single precision. However, when there are many
        // items inside a view, the ViewMainContent's left or top value can exceed
        // the precision limits of Single. To avoid this, we skip painting the ViewMainContent
        // itself and only paint its visible children.
        If not LControl.PaintChildrenOnly then begin

          var LPosX: Single;
          var LPosY: Single;
          if PaintChildrenOnly then begin
            LPosX := Left + LControl.left;
            LPosY := Top + LControl.Top;
          end
          else begin
            LPosX := LControl.left;
            LPosY := LControl.Top;
          end;

          if SameValue(LControl.Scale.X, 1.0, TEpsilon.Scale) and SameValue(LControl.Scale.Y, 1.0, TEpsilon.Scale) and SameValue(LControl.RotationAngle, 0.0, TEpsilon.Scale) then begin
            var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(LPosX, LPosY);
            Canvas.SetMatrix(LMatrix);
          end
          else begin
            var LPivotOffsetX := LControl.Pivot.X * LControl.Width;
            var LPivotOffsetY := LControl.Pivot.Y * LControl.Height;
            var LMatrix := TMatrix.CreateTranslation(-LPivotOffsetX, -LPivotOffsetY) *
                           TMatrix.CreateScaling(LControl.Scale.X, LControl.Scale.Y);
            if not SameValue(LControl.RotationAngle, 0.0, TEpsilon.Scale) then
              LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(LControl.RotationAngle));
            LMatrix := LMatrix * TMatrix.CreateTranslation(LPosX + LPivotOffsetX, LPosY + LPivotOffsetY);
            Canvas.SetMatrix(LMatrix * LSavedMatrix);
          end;

        end;
        LControl.PaintInternal(Canvas);
      end;
    finally
      Canvas.SetMatrix(LSavedMatrix);
    end;
  end;
end;

{*************************************}
procedure TALDynamicControl.AfterPaint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
end;

{$REGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (2)'}

{******************************************************************}
constructor TALDynamicExtendedControl.Create(const AOwner: TObject);
begin
  inherited;
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if MarginsChanged is not implemented in FMX.Controls.TControl and adjust the IFDEF'}
  {$ENDIF}
  //**FFormerMarginsChangedHandler := Margins.OnChange;
  //**Margins.OnChange := MarginsChangedHandler;
  //**Size.SetPlatformDefaultWithoutNotification(False);
  //**FForm := nil;
  //**FOwner := nil;
  FControlAbsolutePosAtMouseDown := TALPointD.zero;
  //**FScale := TPosition.Create(TPointF.Create(1, 1));
  //**FScale.OnChange := ScaleChangedHandler;
  //**FFocusOnMouseDown := False;
  //**FFocusOnMouseUp := False;
  FMouseDownAtRest := True;
  FDoubleClick := False;
  FAutoAlignToPixel := True;
  //**FAlign := TALAlignLayout.None;
  FIsSetBoundsLocked := False;
  FBeforeDestructionExecuted := False;
  FClickSound := TALClickSoundMode.Default;
  FAutoSize := TALAutoSizeMode.None;
  FIsAdjustingSize := False;
  FAdjustSizeOnEndUpdate := False;
end;

{*******************************************}
destructor TALDynamicExtendedControl.Destroy;
begin
  ClearBufDrawable;
  //**ALFreeAndNil(FScale);
  inherited;
end;

{****************************************************}
procedure TALDynamicExtendedControl.BeforeDestruction;
begin
  if FBeforeDestructionExecuted then exit;
  FBeforeDestructionExecuted := True;
  for var I := 0 to ControlsCount - 1 do
    Controls[I].BeforeDestruction;
  inherited;
end;

{********************************************************************}
procedure TALDynamicExtendedControl.Assign(Source: TALDynamicControl);
begin
  BeginUpdate;
  Try
    if Source is TALDynamicControl then begin
      // --TALDynamicControl
      Align := TALDynamicControl(Source).Align;
      //**AutoAlignToPixel := TALDynamicControl(Source).AutoAlignToPixel;
      //**AutoSize := TALDynamicControl(Source).AutoSize;
      //**DoubleBuffered := TALDynamicControl(Source).DoubleBuffered;
      //**Pivot.Assign(TALDynamicControl(Source).Pivot);
      //**Scale.assign(TALDynamicControl(Source).Scale);
      // --TControl
      //**Anchors := TALDynamicControl(Source).Anchors;
      //**CanFocus := TALDynamicControl(Source).CanFocus;
      //**CanParentFocus := TALDynamicControl(Source).CanParentFocus;
      //**ClipChildren := TALDynamicControl(Source).ClipChildren;
      //**ClipParent := TALDynamicControl(Source).ClipParent;
      Cursor := TALDynamicControl(Source).Cursor;
      DisabledOpacity := TALDynamicControl(Source).DisabledOpacity;
      //**DragMode := TALDynamicControl(Source).DragMode;
      //**EnableDragHighlight := TALDynamicControl(Source).EnableDragHighlight;
      Enabled := TALDynamicControl(Source).Enabled;
      //**Hint := TALDynamicControl(Source).Hint;
      HitTest := TALDynamicControl(Source).HitTest;
      //**Locked := TALDynamicControl(Source).Locked;
      Margins.Assign(TALDynamicControl(Source).Margins);
      Opacity := TALDynamicControl(Source).Opacity;
      Padding.Assign(TALDynamicControl(Source).Padding);
      //**ParentShowHint := TALDynamicControl(Source).ParentShowHint;
      //**Position.Assign(TALDynamicControl(Source).Position);
      RotationAngle := TALDynamicControl(Source).RotationAngle;
      //**ShowHint := TALDynamicControl(Source).ShowHint;
      //**Size.Assign(TALDynamicControl(Source).Size);
      //**StyleName := TALDynamicControl(Source).StyleName;
      //**TabOrder := TALDynamicControl(Source).TabOrder;
      //**TabStop := TALDynamicControl(Source).TabStop;
      Tag := TALDynamicControl(Source).Tag;
      //**TagFloat := TALDynamicControl(Source).TagFloat;
      TagObject := TALDynamicControl(Source).TagObject;
      TagString := TALDynamicControl(Source).TagString;
      //**TouchTargetExpansion.Assign(TALDynamicControl(Source).TouchTargetExpansion);
      Visible := TALDynamicControl(Source).Visible;
      //**OnDragEnter := TALDynamicControl(Source).OnDragEnter;
      //**OnDragLeave := TALDynamicControl(Source).OnDragLeave;
      //**OnDragOver := TALDynamicControl(Source).OnDragOver;
      //**OnDragDrop := TALDynamicControl(Source).OnDragDrop;
      //**OnDragEnd := TALDynamicControl(Source).OnDragEnd;
      //**OnKeyDown := TALDynamicControl(Source).OnKeyDown;
      //**OnKeyUp := TALDynamicControl(Source).OnKeyUp;
      OnClick := TALDynamicControl(Source).OnClick;
      //**OnDblClick := TALDynamicControl(Source).OnDblClick;
      //**OnCanFocus := TALDynamicControl(Source).OnCanFocus;
      //**OnEnter := TALDynamicControl(Source).OnEnter;
      //**OnExit := TALDynamicControl(Source).OnExit;
      OnMouseDown := TALDynamicControl(Source).OnMouseDown;
      OnMouseMove := TALDynamicControl(Source).OnMouseMove;
      OnMouseUp := TALDynamicControl(Source).OnMouseUp;
      //**OnMouseWheel := TALDynamicControl(Source).OnMouseWheel;
      OnMouseEnter := TALDynamicControl(Source).OnMouseEnter;
      OnMouseLeave := TALDynamicControl(Source).OnMouseLeave;
      OnPainting := TALDynamicControl(Source).OnPainting;
      OnPaint := TALDynamicControl(Source).OnPaint;
      //**OnResize := TALDynamicControl(Source).OnResize;
      OnResized := TALDynamicControl(Source).OnResized;
      //**OnActivate := TALDynamicControl(Source).OnActivate;
      //**OnDeactivate := TALDynamicControl(Source).OnDeactivate;
    end
    else
      ALAssignError(Source{ASource}, Self{ADest});
  Finally
    EndUpdate;
  End;
end;

{***************************************************************}
// The current implementation of TControl's BeginUpdate/EndUpdate
// and Realign methods is inefficient—particularly for TALDynamicText,
// which must rebuild its internal buffer every time its size
// changes. When EndUpdate is called, the update propagates through
// the deepest nested children first. For example, consider the
// following hierarchy:
//
// Control1
//   └─ Control2
//         └─ AlText1
//
// When Control1.EndUpdate is called, the sequence of events is as
// follows:
//
//   * AlText1.EndUpdate is called first, triggering an AdjustSize
//     and a Realign.
//   * Next, Control2.EndUpdate executes, which calls Realign and
//     may trigger AlText1.AdjustSize again.
//   * Finally, Control1.EndUpdate runs, calling Realign and possibly
//     causing yet another AlText1.AdjustSize.
//
// This series of operations results in the BufDrawable being
// recalculated multiple times, leading to significant performance
// overhead.
{$IFNDEF ALCompilerVersionSupported123}
  {$MESSAGE WARN 'Check if FMX.Controls.TControl.EndUpdate was not updated and adjust the IFDEF'}
{$ENDIF}
procedure TALDynamicExtendedControl.EndUpdate;
begin
  if IsUpdating then
  begin
    Dec(FUpdating);
    if not IsUpdating then
    begin
      DoEndUpdate;
      //RefreshInheritedCursorForChildren;
    end;
    for var I := 0 to ControlsCount - 1 do
      Controls[I].EndUpdate;
  end;
end;

{*********************************************}
//**procedure TALDynamicExtendedControl.Loaded;
//**begin
//**  {$IF not defined(ALDPK)}
//**  if AutoAlignToPixel then
//**    AlignToPixel;
//**  {$ENDIF}
//**  Inherited;
//**  AdjustSize;
//**end;

{*************************************************************}
//**function TALDynamicExtendedControl.IsOwnerLoading: Boolean;
//**begin
//**  result := (Owner <> nil) and
//**            (csloading in Owner.ComponentState);
//**end;

{***********************************************************}
//**function TALDynamicExtendedControl.IsSizeStored: Boolean;
//**begin
//**  var LDefaultSize := GetDefaultSize;
//**  result := (not SameValue(LDefaultSize.cx, Size.Size.cx, TEpsilon.Position)) or
//**            (not SameValue(LDefaultSize.cy, Size.Size.cy, TEpsilon.Position));
//**end;

{**************************************************************}
//**function TALDynamicExtendedControl.GetAlign: TALAlignLayout;
//**begin
//**  Result := FAlign;
//**end;

{****************************************************************************}
//**procedure TALDynamicExtendedControl.SetAlign(const Value: TALAlignLayout);
//**begin
//**  If FAlign <> Value then begin
//**    FAlign := Value;
//**    var LLegacyAlign: TAlignLayout;
//**    Case Value of
//**      TALAlignLayout.None: LLegacyAlign := TAlignLayout.None;
//**      TALAlignLayout.Top: LLegacyAlign := TAlignLayout.Top;
//**      TALAlignLayout.Left: LLegacyAlign := TAlignLayout.Left;
//**      TALAlignLayout.Right: LLegacyAlign := TAlignLayout.Right;
//**      TALAlignLayout.Bottom: LLegacyAlign := TAlignLayout.Bottom;
//**      TALAlignLayout.MostTop: LLegacyAlign := TAlignLayout.MostTop;
//**      TALAlignLayout.MostBottom: LLegacyAlign := TAlignLayout.MostBottom;
//**      TALAlignLayout.MostLeft: LLegacyAlign := TAlignLayout.MostLeft;
//**      TALAlignLayout.MostRight: LLegacyAlign := TAlignLayout.MostRight;
//**      TALAlignLayout.Client: LLegacyAlign := TAlignLayout.Client;
//**      TALAlignLayout.Contents: LLegacyAlign := TAlignLayout.Contents;
//**      TALAlignLayout.Center: LLegacyAlign := TAlignLayout.Center;
//**      TALAlignLayout.VertCenter: LLegacyAlign := TAlignLayout.VertCenter;
//**      TALAlignLayout.HorzCenter: LLegacyAlign := TAlignLayout.HorzCenter;
//**      TALAlignLayout.Horizontal: LLegacyAlign := TAlignLayout.Horizontal;
//**      TALAlignLayout.Vertical: LLegacyAlign := TAlignLayout.Vertical;
//**      //TALAlignLayout.Scale: LLegacyAlign := TAlignLayout.Scale;
//**      //TALAlignLayout.Fit: LLegacyAlign := TAlignLayout.Fit;
//**      //TALAlignLayout.FitLeft: LLegacyAlign := TAlignLayout.FitLeft;
//**      //TALAlignLayout.FitRight: LLegacyAlign := TAlignLayout.FitRight;
//**      TALAlignLayout.TopCenter: LLegacyAlign := TAlignLayout.Top;
//**      TALAlignLayout.TopLeft: LLegacyAlign := TAlignLayout.Top;
//**      TALAlignLayout.TopRight: LLegacyAlign := TAlignLayout.Top;
//**      TALAlignLayout.LeftCenter: LLegacyAlign := TAlignLayout.Left;
//**      TALAlignLayout.LeftTop: LLegacyAlign := TAlignLayout.Left;
//**      TALAlignLayout.LeftBottom: LLegacyAlign := TAlignLayout.Left;
//**      TALAlignLayout.RightCenter: LLegacyAlign := TAlignLayout.Right;
//**      TALAlignLayout.RightTop: LLegacyAlign := TAlignLayout.Right;
//**      TALAlignLayout.RightBottom: LLegacyAlign := TAlignLayout.Right;
//**      TALAlignLayout.BottomCenter: LLegacyAlign := TAlignLayout.Bottom;
//**      TALAlignLayout.BottomLeft: LLegacyAlign := TAlignLayout.Bottom;
//**      TALAlignLayout.BottomRight: LLegacyAlign := TAlignLayout.Bottom;
//**      TALAlignLayout.MostTopCenter: LLegacyAlign := TAlignLayout.MostTop;
//**      TALAlignLayout.MostTopLeft: LLegacyAlign := TAlignLayout.MostTop;
//**      TALAlignLayout.MostTopRight: LLegacyAlign := TAlignLayout.MostTop;
//**      TALAlignLayout.MostLeftCenter: LLegacyAlign := TAlignLayout.MostLeft;
//**      TALAlignLayout.MostLeftTop: LLegacyAlign := TAlignLayout.MostLeft;
//**      TALAlignLayout.MostLeftBottom: LLegacyAlign := TAlignLayout.MostLeft;
//**      TALAlignLayout.MostRightCenter: LLegacyAlign := TAlignLayout.MostRight;
//**      TALAlignLayout.MostRightTop: LLegacyAlign := TAlignLayout.MostRight;
//**      TALAlignLayout.MostRightBottom: LLegacyAlign := TAlignLayout.MostRight;
//**      TALAlignLayout.MostBottomCenter: LLegacyAlign := TAlignLayout.MostBottom;
//**      TALAlignLayout.MostBottomLeft: LLegacyAlign := TAlignLayout.MostBottom;
//**      TALAlignLayout.MostBottomRight: LLegacyAlign := TAlignLayout.MostBottom;
//**      else Raise Exception.Create('Error D527A470-23AC-4E3C-BCC5-4C2DB578A691');
//**    end;
//**    Inherited SetAlign(LLegacyAlign);
//**  end;
//**end;

{**********************************************}
procedure TALDynamicExtendedControl.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if FAdjustSizeOnEndUpdate then
    AdjustSize;
end;

{********************************************}
procedure TALDynamicExtendedControl.DoResized;
begin
  {$IF defined(debug)}
  //ALLog(ClassName+'.DoResized', 'Name: ' + Name);
  {$ENDIF}
  inherited;
  AdjustSize;
end;

{********************************************}
procedure TALDynamicExtendedControl.DoRealign;
begin
  {$IF defined(debug)}
  //ALLog(ClassName+'.DoRealign', 'Name: ' + Name);
  {$ENDIF}

  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://quality.embarcadero.com/browse/RSP-15768 was implemented and adjust the IFDEF'}
  {$ENDIF}
  // I decided to remove this workaround because it doesn't
  // work well with TContent (e.g., ScrollBox).
  // The TContent size is always updated during realignment,
  // causing the process to be applied twice.
  //https://quality.embarcadero.com/browse/RSP-15768
  //var LOriginalSize: TPointF := Size.Size;
  inherited;
  //https://quality.embarcadero.com/browse/RSP-15768
  //if not LOriginalSize.EqualsTo(Size.Size) then DoRealign;

  // Unfortunately, FNeedAlign is set to true when any child control with
  // Align <> None is added. However, it does not take AutoSize into
  // consideration
  AdjustSize;
end;

{***************************************************}
procedure TALDynamicExtendedControl.ParentRealigning;
begin
  // Virtual
end;

{************************************************************************************}
procedure TALDynamicExtendedControl.SetFixedSizeBounds(X, Y, AWidth, AHeight: Single);
begin
  if TNonReentrantHelper.EnterSection(FIsSetBoundsLocked) then begin
    try

      {$IF defined(debug)}
      //ALLog(ClassName+'.SetFixedSizeBounds', 'Name: ' + Name + ' | X : '+ALFloatToStrW(X)+'('+ALFloatToStrW(Left)+') | Y : '+ALFloatToStrW(Y)+'('+ALFloatToStrW(Top)+') | AWidth : '+ALFloatToStrW(AWidth)+'('+ALFloatToStrW(Width)+') | AHeight : '+ALFloatToStrW(AHeight)+'('+ALFloatToStrW(Height)+')');
      {$ENDIF}

      inherited SetBounds(X, Y, AWidth, AHeight);

    finally
      TNonReentrantHelper.LeaveSection(FIsSetBoundsLocked)
    end;
  end
  else
    SetBounds(X, Y, AWidth, AHeight);
end;

{***************************************************************************}
procedure TALDynamicExtendedControl.SetBounds(X, Y, AWidth, AHeight: Double);
begin
  // Owner.FDisableAlign = True means SetBounds was
  // called by the AlignObjects procedure inside Owner.DoRealign.
  // Fortunately, AlignObjects systematically calls SetBounds on all
  // its children, even when there’s nothing to update.
  var LParentRealigning := (((Owner <> nil) and (Owner.FDisableAlign)) or
                            ((Owner = nil) and (Host <> nil) and (Host.FDisableAlign)));
  If LParentRealigning then
    ParentRealigning;

  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2342 was implemented and adjust the IFDEF'}
  {$ENDIF}
  //TALAlignLayout.TopCenter
  //TALAlignLayout.TopLeft
  //TALAlignLayout.TopRight
  //TALAlignLayout.LeftCenter
  //TALAlignLayout.LeftTop
  //TALAlignLayout.LeftBottom
  //TALAlignLayout.RightCenter
  //TALAlignLayout.RightTop
  //TALAlignLayout.RightBottom
  //TALAlignLayout.BottomCenter
  //TALAlignLayout.BottomLeft
  //TALAlignLayout.BottomRight
  //TALAlignLayout.MostTopCenter
  //TALAlignLayout.MostTopLeft
  //TALAlignLayout.MostTopRight
  //TALAlignLayout.MostLeftCenter
  //TALAlignLayout.MostLeftTop
  //TALAlignLayout.MostLeftBottom
  //TALAlignLayout.MostRightCenter
  //TALAlignLayout.MostRightTop
  //TALAlignLayout.MostRightBottom
  //TALAlignLayout.MostBottomCenter
  //TALAlignLayout.MostBottomLeft
  //TALAlignLayout.MostBottomRight
  //**If (LParentRealigning) and
  //**   (integer(FAlign) >= integer(TALAlignLayout.TopCenter)) and
  //**   (integer(FAlign) <= integer(TALAlignLayout.MostBottomRight)) then begin
  //**  case FAlign of
  //**    TALAlignLayout.TopCenter,
  //**    TALAlignLayout.BottomCenter,
  //**    TALAlignLayout.MostTopCenter,
  //**    TALAlignLayout.MostBottomCenter: begin
  //**      X := X + ((AWidth - Width) / 2);
  //**      AWidth := Width;
  //**    end;
  //**    TALAlignLayout.TopLeft,
  //**    TALAlignLayout.BottomLeft,
  //**    TALAlignLayout.MostTopLeft,
  //**    TALAlignLayout.MostBottomLeft: begin
  //**      AWidth := Width;
  //**    end;
  //**    TALAlignLayout.TopRight,
  //**    TALAlignLayout.BottomRight,
  //**    TALAlignLayout.MostTopRight,
  //**    TALAlignLayout.MostBottomRight: begin
  //**      X := X + (AWidth - Width);
  //**      AWidth := Width;
  //**    end;
  //**    TALAlignLayout.LeftCenter,
  //**    TALAlignLayout.RightCenter,
  //**    TALAlignLayout.MostLeftCenter,
  //**    TALAlignLayout.MostRightCenter: begin
  //**      Y := Y + ((AHeight - Height) / 2);
  //**      AHeight := Height;
  //**    end;
  //**    TALAlignLayout.LeftTop,
  //**    TALAlignLayout.RightTop,
  //**    TALAlignLayout.MostLeftTop,
  //**    TALAlignLayout.MostRightTop: begin
  //**      AHeight := Height;
  //**    end;
  //**    TALAlignLayout.LeftBottom,
  //**    TALAlignLayout.RightBottom,
  //**    TALAlignLayout.MostLeftBottom,
  //**    TALAlignLayout.MostRightBottom: begin
  //**      Y := Y + (AHeight - Height);
  //**      AHeight := Height;
  //**    end;
  //**    else
  //**      raise Exception.Create('Error 9431A388-3F2F-4F06-8296-210708F60C66');
  //**  end;
  //**end;

  if FIsSetBoundsLocked then begin
    AWidth := Width;
    AHeight := Height;
  end;

  {$IF defined(debug)}
  //var LMoved := not (SameValue(X, Left, TEpsilon.Position) and SameValue(Y, Top, TEpsilon.Position));
  //var LSizeChanged := not (SameValue(AWidth, Width, TEpsilon.Position) and SameValue(AHeight, Height, TEpsilon.Position));
  //if LMoved or LSizeChanged then
  //  ALLog(ClassName+'.SetBounds', 'Name: ' + Name + ' | X : '+ALFloatToStrW(X)+'('+ALFloatToStrW(Left)+') | Y : '+ALFloatToStrW(Y)+'('+ALFloatToStrW(Top)+') | AWidth : '+ALFloatToStrW(AWidth)+'('+ALFloatToStrW(Width)+') | AHeight : '+ALFloatToStrW(AHeight)+'('+ALFloatToStrW(Height)+')');
  {$ENDIF}

  inherited;
end;

{*************************************************************}
function TALDynamicExtendedControl.GetMaxContainedSize: TSizeF;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function CalculateAvailableClientSize(
             const AParentWidth: Single;
             const AParentHeight: Single;
             const AParentPadding: TRectF;
             const AChildControls: TArray<TALDynamicControl>;
             const AChildControlsCount: Integer): TSizeF;
  begin

    if Align in [TALAlignLayout.None, TALAlignLayout.Center] then begin
      Result := TSizeF.Create(65535, 65535);
      Exit;
    end
    else if Align = TALAlignLayout.Contents then begin
      Result := TSizeF.Create(
                  AParentWidth - Margins.Left - Margins.Right,
                  AParentHeight - Margins.top - Margins.Bottom);
      Exit;
    end;

    Result := TSizeF.Create(
                AParentWidth - AParentPadding.Left - AParentPadding.Right,
                AParentHeight - AParentPadding.Top - AParentPadding.Bottom);

    var IsHorzAligned := Align in [TALAlignLayout.Top,
                                   TALAlignLayout.TopCenter,
                                   TALAlignLayout.TopLeft,
                                   TALAlignLayout.TopRight,
                                   TALAlignLayout.Bottom,
                                   TALAlignLayout.BottomCenter,
                                   TALAlignLayout.BottomLeft,
                                   TALAlignLayout.BottomRight,
                                   TALAlignLayout.Client,
                                   TALAlignLayout.VertCenter,
                                   TALAlignLayout.Horizontal];
    //var IsMostHorzAligned := Align in [TALAlignLayout.MostTop,
    //                                   TALAlignLayout.MostTopCenter,
    //                                   TALAlignLayout.MostTopLeft,
    //                                   TALAlignLayout.MostTopRight,
    //                                   TALAlignLayout.MostBottom,
    //                                   TALAlignLayout.MostBottomCenter,
    //                                   TALAlignLayout.MostBottomLeft,
    //                                   TALAlignLayout.MostBottomRight];
    var IsVertAligned := Align in [TALAlignLayout.Left,
                                   TALAlignLayout.LeftCenter,
                                   TALAlignLayout.LeftTop,
                                   TALAlignLayout.LeftBottom,
                                   TALAlignLayout.Right,
                                   TALAlignLayout.RightCenter,
                                   TALAlignLayout.RightTop,
                                   TALAlignLayout.RightBottom,
                                   TALAlignLayout.Client,
                                   TALAlignLayout.HorzCenter,
                                   TALAlignLayout.Vertical];
    var IsMostVertAligned := Align in [TALAlignLayout.MostLeft,
                                       TALAlignLayout.MostLeftCenter,
                                       TALAlignLayout.MostLeftTop,
                                       TALAlignLayout.MostLeftBottom,
                                       TALAlignLayout.MostRight,
                                       TALAlignLayout.MostRightCenter,
                                       TALAlignLayout.MostRightTop,
                                       TALAlignLayout.MostRightBottom];
    // These align values are not classified as horizontal
    // or vertical alignment:
    //   TALAlignLayout.None
    //   TALAlignLayout.Contents
    //   TALAlignLayout.Center

    for var I := 0 to AChildControlsCount - 1 do begin
      var LChildControl := AChildControls[I];
      if not LChildControl.Visible then Continue;
      if LChildControl = Self then Continue;
      {$IF defined(ALDPK)}
      // At design time, the Delphi IDE may add children such as
      // TGrabHandle.TGrabHandleRectangle
      if Supports(LChildControl, IDesignerControl) then
        Continue;
      {$ENDIF}

      //**var LChildControlAlign: TALAlignLayout;
      //**If (LChildControl is TALDynamicControl) then LChildControlAlign := TALDynamicControl(LChildControl).Align
      //**else begin
      //**  {$If defined(debug)}
      //**  if LChildControl.Align in [TAlignLayout.Scale,
      //**                             TAlignLayout.Fit,
      //**                             TAlignLayout.FitLeft,
      //**                             TAlignLayout.FitRight] then
      //**    Raise Exception.Create('Align values Scale, Fit, FitLeft, and FitRight are not supported');
      //**  {$ENDIF}
      //**  LChildControlAlign := TALAlignLayout(LChildControl.Align);
      //**end;
      var LChildControlAlign := LChildControl.Align;

      //
      //  #################            ######## #################            ##################            ##################
      //  #      TOP      #            #      # #      TOP      #            #    MOST TOP    #            #    MOST TOP    #
      //  #################            #      # #################            ##################            ##################
      //                               #      #
      //  ########                     # MOST #                              ########                      ########
      //  #      #                     # LEFT #                              #      #                      #      #
      //  # LEFT #                     #      #                              # LEFT #                      # MOST #
      //  #      #                     #      #                              #      #                      # LEFT #
      //  #      #                     #      #                              #      #                      #      #
      //  ########                     ########                              ########                      ########
      //

      case LChildControlAlign of

        //--
        TALAlignLayout.None,
        TALAlignLayout.Center,
        TALAlignLayout.Contents,
        TALAlignLayout.Client,
        TALAlignLayout.Horizontal,
        TALAlignLayout.VertCenter,
        TALAlignLayout.Vertical,
        TALAlignLayout.HorzCenter:;

        //--
        TALAlignLayout.Top,
        TALAlignLayout.TopCenter,
        TALAlignLayout.TopLeft,
        TALAlignLayout.TopRight,
        TALAlignLayout.MostTop,
        TALAlignLayout.MostTopCenter,
        TALAlignLayout.MostTopLeft,
        TALAlignLayout.MostTopRight,
        TALAlignLayout.Bottom,
        TALAlignLayout.BottomCenter,
        TALAlignLayout.BottomLeft,
        TALAlignLayout.BottomRight,
        TALAlignLayout.MostBottom,
        TALAlignLayout.MostBottomCenter,
        TALAlignLayout.MostBottomLeft,
        TALAlignLayout.MostBottomRight:
          Result.Height := Result.Height - LChildControl.Height - LChildControl.Margins.Top - LChildControl.Margins.Bottom;

        //--
        TALAlignLayout.Left,
        TALAlignLayout.LeftCenter,
        TALAlignLayout.LeftTop,
        TALAlignLayout.LeftBottom,
        TALAlignLayout.Right,
        TALAlignLayout.RightCenter,
        TALAlignLayout.RightTop,
        TALAlignLayout.RightBottom: begin
          if IsVertAligned or
             IsMostVertAligned then
            Result.Width := Result.Width - LChildControl.Width - LChildControl.Margins.Left - LChildControl.Margins.Right;
        end;

        //--
        TALAlignLayout.MostLeft,
        TALAlignLayout.MostLeftCenter,
        TALAlignLayout.MostLeftTop,
        TALAlignLayout.MostLeftBottom,
        TALAlignLayout.MostRight,
        TALAlignLayout.MostRightCenter,
        TALAlignLayout.MostRightTop,
        TALAlignLayout.MostRightBottom: begin
          if IsVertAligned or
             IsMostVertAligned or
             IsHorzAligned then
            Result.Width := Result.Width - LChildControl.Width - LChildControl.Margins.Left - LChildControl.Margins.Right;
        end;

        //--
        else
          raise Exception.Create('Error 6DF0FA18-83E4-4B1C-806C-D04A6ED29DB0');

      end;
    end;

    if not (Align in [TALAlignLayout.None,
                      TALAlignLayout.Center]) then begin
      Result.Width := Result.Width - Margins.Left - Margins.Right;
      Result.Height := Result.Height - Margins.Top - Margins.Bottom;
    end;
  end;

begin
  if Owner <> nil then begin
    var LHasUnconstrainedAutosizeWidth := Owner.HasUnconstrainedAutosizeWidth;
    var LHasUnconstrainedAutosizeHeight := Owner.HasUnconstrainedAutosizeHeight;
    if (LHasUnconstrainedAutosizeWidth) and (LHasUnconstrainedAutosizeHeight) then
      Exit(TSizeF.Create(65535, 65535));

    Result := CalculateAvailableClientSize(
                Owner.Width, // const AWidth: Single;
                Owner.Height, // const AHeight: Single;
                Owner.Padding.Rect, // const APadding: TRectF;
                Owner.FControls, // const AChildControls: TArray<TALDynamicControl>
                Owner.ControlsCount); // const AChildControlsCount: Integer

    if (LHasUnconstrainedAutosizeWidth) then Result.Width := 65535;
    if (LHasUnconstrainedAutosizeHeight) then Result.Height := 65535;

    var LOwner := Owner;
    if LOwner is TALDynamicContent then LOwner := LOwner.Owner;
    if LOwner <> nil then begin
      //**var LScrollableControl: IALScrollableControl;
      //**if (Supports(LOwner, IALScrollableControl, LScrollableControl)) then begin
      //**  if ttVertical in LScrollableControl.GetScrollEngine.TouchTracking then Result.Height := 65535;
      //**  if tthorizontal in LScrollableControl.GetScrollEngine.TouchTracking then Result.Width := 65535;
      //**end;
      if LOwner.ScrollEngine <> nil then begin
        if ttVertical in LOwner.ScrollEngine.TouchTracking then Result.Height := 65535;
        if tthorizontal in LOwner.ScrollEngine.TouchTracking then Result.Width := 65535;
      end;
    end;
  end
  //**else if Owner <> nil then begin
  //**  Result := CalculateAvailableClientSize(
  //**              Owner.Width, // const AWidth: Single;
  //**              Owner.Height, // const AHeight: Single;
  //**              Owner.Padding.Rect, // const APadding: TRectF;
  //**              Owner.FControls, // const AChildControls: TArray<TALDynamicControl>
  //**              Owner.ControlsCount); // const AChildControlsCount: Integer
  //**
  //**  var LOwner := Owner;
  //**  if LOwner is TALDynamicContent then LOwner := LOwner.Owner;
  //**  if LOwner <> nil then begin
  //**    var LScrollableControl: IALScrollableControl;
  //**    if (Supports(LOwner, IALScrollableControl, LScrollableControl)) then begin
  //**      if ttVertical in LScrollableControl.GetScrollEngine.TouchTracking then Result.Height := 65535;
  //**      if tthorizontal in LScrollableControl.GetScrollEngine.TouchTracking then Result.Width := 65535;
  //**    end;
  //**  end;
  //**end
  //**else if FForm <> nil then begin
  //**  var LChildControls: TArray<TControl>;
  //**  Setlength(LChildControls, FForm.ChildrenCount);
  //**  var LChildControlsCount := 0;
  //**  For var I := 0 to FForm.ChildrenCount - 1 do begin
  //**    var LObject := FForm.Children[i];
  //**    if LObject is TControl then begin
  //**      LChildControls[LChildControlsCount] := TControl(LObject);
  //**      inc(LChildControlsCount);
  //**    end;
  //**  end;
  //**  var LClientSize := _TCustomFormProtectedAccess(FForm).FWinService.GetClientSize(FForm);
  //**  Result := CalculateAvailableClientSize(
  //**              LClientSize.X , // const AWidth: Single;
  //**              LClientSize.Y, // const AHeight: Single;
  //**              FForm.Padding.Rect, // const APadding: TRectF;
  //**              LChildControls, // const AChildControls: TArray<TALDynamicControl>
  //**              LChildControlsCount); // const AChildControlsCount: Integer
  //**end
  else
    Result := TSizeF.Create(Width, Height);
end;

{*********************************************}
procedure TALDynamicExtendedControl.AdjustSize;
begin
  var LHasUnconstrainedAutosizeWidth := HasUnconstrainedAutosizeWidth;
  var LHasUnconstrainedAutosizeHeight := HasUnconstrainedAutosizeHeight;
  if //**(not (csLoading in ComponentState)) and // Loaded will call again AdjustSize
     (not IsDestroying) and // If csDestroying do not do autosize
     (ControlsCount > 0) and // If there are no controls, do not perform autosizing
     (LHasUnconstrainedAutosizeWidth or LHasUnconstrainedAutosizeHeight) and // If AutoSize is false nothing to adjust
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // Non-reantrant
    try

      if IsUpdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      {$IF defined(debug)}
      //ALLog(ClassName+'.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(LHasUnconstrainedAutosizeWidth)+'/'+ALBoolToStrW(LHasUnconstrainedAutosizeHeight));
      {$ENDIF}

      var LSize := TSizeF.Create(0,0);
      for var I := 0 to ControlsCount - 1 do begin
        var LChildControl := Controls[I];
        if not LChildControl.Visible then Continue;
        {$IF defined(ALDPK)}
        // At design time, the Delphi IDE may add children such as
        // TGrabHandle.TGrabHandleRectangle
        //**if (csDesigning in ComponentState) and Supports(LChildControl, IDesignerControl) then
        //**  Continue;
        {$ENDIF}

        //**var LALChildControl: TALDynamicControl;
        //**var LChildControlAlign: TALAlignLayout;
        //**If (LChildControl is TALDynamicControl) then begin
        //**  LALChildControl := TALDynamicControl(LChildControl);
        //**  LChildControlAlign := LALChildControl.Align
        //**end
        //**else begin
        //**  LALChildControl := nil;
        //**  LChildControlAlign := TALAlignLayout(LChildControl.Align);
        //**end;
        var LALChildControl := LChildControl;
        var LChildControlAlign := LChildControl.Align;

        case LChildControlAlign of

          //--
          TALAlignLayout.None: begin
            // Adjusts AControl size to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right);
            LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Center: begin
            // Adjusts AControl size to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is more than
            //    one child control. !!
            LSize.Width := Max(LSize.Width, LChildControl.Margins.left + padding.left + LChildControl.width + LChildControl.Margins.right + padding.right);
            LSize.height := Max(LSize.height, LChildControl.Margins.top + padding.top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Top,
          TALAlignLayout.MostTop: begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeWidth then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl width to ensure it contains the child control at its
              // current position. For example, TALDynamicText will never have
              // HasUnconstrainedAutosizeWidth set to true with TALAlignLayout.Top,
              // but TALDynamicLayout/TRectangle will have it set to true if their
              // autosize property is enabled.
              LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right)
            else
              // Otherwise, do not adjust AControl width.
              LSize.Width := Max(LSize.Width, Width);
            // Adjusts AControl height to ensure it contains
            // the child control at its current position.
            LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Bottom,
          TALAlignLayout.MostBottom: begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeWidth then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl width to ensure it contains the child control at its
              // current position. For example, TALDynamicText will never have
              // HasUnconstrainedAutosizeWidth set to true with TALAlignLayout.Top,
              // but TALDynamicLayout/TRectangle will have it set to true if their
              // autosize property is enabled.
              LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right)
            else
              // Otherwise, do not adjust AControl width.
              LSize.Width := Max(LSize.Width, Width);
            // Adjusts AControl height to ensure it contains
            // the child control at its current position.
            LSize.height := Max(LSize.height, Height - LChildControl.Top + LChildControl.Margins.Top + padding.Top);
          end;

          //--
          TALAlignLayout.TopCenter,
          TALAlignLayout.TopLeft,
          TALAlignLayout.TopRight,
          TALAlignLayout.MostTopCenter,
          TALAlignLayout.MostTopLeft,
          TALAlignLayout.MostTopRight: begin
            // Adjusts AControl width to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is another child control
            //    that is not aligned to the top or bottom. !!
            LSize.Width := Max(LSize.Width, LChildControl.Margins.left + padding.left + LChildControl.width + LChildControl.Margins.right + padding.right);
            // Adjusts AControl height to ensure it contains
            // the child control at its current position.
            LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.BottomCenter,
          TALAlignLayout.BottomLeft,
          TALAlignLayout.BottomRight,
          TALAlignLayout.MostBottomCenter,
          TALAlignLayout.MostBottomLeft,
          TALAlignLayout.MostBottomRight: begin
            // Adjusts AControl width to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is another child control
            //    that is not aligned to the top or bottom. !!
            LSize.Width := Max(LSize.Width, LChildControl.Margins.left + padding.left + LChildControl.width + LChildControl.Margins.right + padding.right);
            // Adjusts AControl height to ensure it contains
            // the child control at its current position.
            LSize.height := Max(LSize.height, Height - LChildControl.Top + LChildControl.Margins.Top + padding.Top);
          end;

          //--
          TALAlignLayout.Left,
          TALAlignLayout.MostLeft: Begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right);
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeHeight then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl height to ensure it contains the child control at its
              // current position. For example, TALDynamicText will never have
              // HasUnconstrainedAutosizeWidth set to true with TALAlignLayout.Left,
              // but TALDynamicLayout/TRectangle will have it set to true if their
              // autosize property is enabled.
              LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
            else
              // Otherwise, do not adjust AControl height.
              LSize.height := Max(LSize.Height, Height);
          End;

          //--
          TALAlignLayout.Right,
          TALAlignLayout.MostRight: Begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, Width - LChildControl.Left + LChildControl.Margins.left + padding.left);
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeHeight then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl height to ensure it contains the child control at its
              // current position. For example, TALDynamicText will never have
              // HasUnconstrainedAutosizeWidth set to true with TALAlignLayout.Left,
              // but TALDynamicLayout/TRectangle will have it set to true if their
              // autosize property is enabled.
              LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
            else
              // Otherwise, do not adjust AControl height.
              LSize.height := Max(LSize.Height, Height);
          End;

          //--
          TALAlignLayout.LeftCenter,
          TALAlignLayout.LeftTop,
          TALAlignLayout.LeftBottom,
          TALAlignLayout.MostLeftCenter,
          TALAlignLayout.MostLeftTop,
          TALAlignLayout.MostLeftBottom: begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right);
            // Adjusts AControl height to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is another child control
            //    that is not aligned to the left or right. !!
            LSize.height := Max(LSize.height, LChildControl.Margins.top + padding.top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.RightCenter,
          TALAlignLayout.RightTop,
          TALAlignLayout.RightBottom,
          TALAlignLayout.MostRightCenter,
          TALAlignLayout.MostRightTop,
          TALAlignLayout.MostRightBottom: begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, Width - LChildControl.Left + LChildControl.Margins.left + padding.left);
            // Adjusts AControl height to ensure it contains the
            // child control without considering its current position.
            // !! Note: This may not work well if there is another child control
            //    that is not aligned to the left or right. !!
            LSize.height := Max(LSize.height, LChildControl.Margins.top + padding.top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Contents,
          //TALAlignLayout.Scale,
          //TALAlignLayout.Fit,
          //TALAlignLayout.FitLeft,
          //TALAlignLayout.FitRight,
          TALAlignLayout.Client: Begin
            if LALChildControl <> nil then begin
              if LALChildControl.HasUnconstrainedAutosizeWidth then LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right)
              else LSize.Width := Max(LSize.Width, Width);
              if LALChildControl.HasUnconstrainedAutosizeHeight then LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
              else LSize.height := Max(LSize.Height, Height);
            end
            else begin
              LSize.Width := Max(LSize.Width, Width);
              LSize.height := Max(LSize.Height, Height);
            end;
          End;

          //--
          TALAlignLayout.Horizontal,
          TALAlignLayout.VertCenter: Begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeWidth then
              LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right)
            else
              LSize.Width := Max(LSize.Width, Width);
          End;

          //--
          TALAlignLayout.Vertical,
          TALAlignLayout.HorzCenter: Begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeHeight then
              LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
            else
              LSize.height := Max(LSize.Height, Height);
          End;

          //--
          else
            raise Exception.Create('Error 431814A4-5A5F-462E-9491-88F1874210DC');

        end;
      end;

      if (not LHasUnconstrainedAutosizeWidth) or (SameValue(LSize.Width, 0, Tepsilon.Position)) then
        LSize.Width := Width;
      if (not LHasUnconstrainedAutosizeHeight) or (SameValue(LSize.Height, 0, Tepsilon.Position)) then
        LSize.Height := Height;

      SetFixedSizeBounds(Left, Top, LSize.Width, LSize.Height);

    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{***********************************************}
procedure TALDynamicExtendedControl.AlignToPixel;
begin
  // Note: We do not align the position here. The position is aligned during
  // the painting process (e.g., via ALDrawDrawable).
  BeginUpdate;
  Try
    // OnResize and OnResized will be called in loaded
    //**var LOldOnResize := OnResize;
    //**var LOldOnResized := OnResized;
    //**if CSLoading in componentState then begin
    //**  OnResize := DelayOnResize;
    //**  OnResized := DelayOnResized;
    //**end;
    //**try
      Margins.Rect := ALAlignEdgesToPixelRound(Margins.Rect, ALGetScreenScale, TEpsilon.Position);
      Padding.Rect := ALAlignEdgesToPixelRound(Padding.Rect, ALGetScreenScale, TEpsilon.Position);
      case Align of
        TALAlignLayout.None,
        TALAlignLayout.Center,
        TALAlignLayout.TopCenter,
        TALAlignLayout.TopLeft,
        TALAlignLayout.TopRight,
        TALAlignLayout.LeftCenter,
        TALAlignLayout.LeftTop,
        TALAlignLayout.LeftBottom,
        TALAlignLayout.RightCenter,
        TALAlignLayout.RightTop,
        TALAlignLayout.RightBottom,
        TALAlignLayout.BottomCenter,
        TALAlignLayout.BottomLeft,
        TALAlignLayout.BottomRight,
        TALAlignLayout.MostTopCenter,
        TALAlignLayout.MostTopLeft,
        TALAlignLayout.MostTopRight,
        TALAlignLayout.MostLeftCenter,
        TALAlignLayout.MostLeftTop,
        TALAlignLayout.MostLeftBottom,
        TALAlignLayout.MostRightCenter,
        TALAlignLayout.MostRightTop,
        TALAlignLayout.MostRightBottom,
        TALAlignLayout.MostBottomCenter,
        TALAlignLayout.MostBottomLeft,
        TALAlignLayout.MostBottomRight:
          SetSize(ALAlignDimensionToPixelRound(TSizeF.Create(Width, Height), ALGetScreenScale, TEpsilon.Position));
        //--
        TALAlignLayout.Top,
        TALAlignLayout.MostTop,
        TALAlignLayout.Bottom,
        TALAlignLayout.MostBottom,
        TALAlignLayout.Horizontal,
        TALAlignLayout.VertCenter:
          Height := ALAlignDimensionToPixelRound(Height, ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Left,
        TALAlignLayout.MostLeft,
        TALAlignLayout.Right,
        TALAlignLayout.MostRight,
        TALAlignLayout.Vertical,
        TALAlignLayout.HorzCenter:
          Width := ALAlignDimensionToPixelRound(Width, ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Contents,
        //TALAlignLayout.Scale,
        //TALAlignLayout.Fit,
        //TALAlignLayout.FitLeft,
        //TALAlignLayout.FitRight
        TALAlignLayout.Client:;
        //--
        else
          Raise Exception.Create('Error AC54DF90-F880-4BD5-8474-E62BD8D099FB')
      end;
    //**finally
    //**  OnResize := LOldOnResize;
    //**  OnResized := LOldOnResized;
    //**end;
  finally
    EndUpdate;
  end;
end;

{***************************************************}
procedure TALDynamicExtendedControl.ApplyColorScheme;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  Procedure ApplyColorSchemeRecursive(const AControl: TALDynamicControl);
  begin
    for var I := 0 to AControl.ControlsCount - 1 do
      if AControl.Controls[i] is TALDynamicControl then TALDynamicControl(AControl.Controls[i]).ApplyColorScheme
      else ApplyColorSchemeRecursive(AControl.Controls[i]);
  end;

begin
  // ClearBufDrawable because when switching between dark and light mode,
  // the resource name remains the same, but the loaded resource changes
  // (e.g., xxx_dark instead of xxx in dark mode).
  ClearBufDrawable;
  ApplyColorSchemeRecursive(self);
end;

{*********************************************************************}
//**procedure TALDynamicExtendedControl.DelayOnResize(Sender: TObject);
//**begin
//**  Include(TALDynamicControlAccessPrivate(Self).FDelayedEvents, TALDynamicControlAccessPrivate.TDelayedEvent.Resize);
//**end;

{**********************************************************************}
//**procedure TALDynamicExtendedControl.DelayOnResized(Sender: TObject);
//**begin
//**  Include(TALDynamicControlAccessPrivate(Self).FDelayedEvents, TALDynamicControlAccessPrivate.TDelayedEvent.Resized);
//**end;

{************************************************************}
function TALDynamicExtendedControl.GetDoubleBuffered: boolean;
begin
  result := False;
end;

{***************************************************************************}
procedure TALDynamicExtendedControl.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not supported
end;

{************************************************************************}
//**procedure TALDynamicExtendedControl.SetScale(const AValue: TPosition);
//**begin
//**  FScale.Assign(AValue);
//**end;

{**************************************************************}
function TALDynamicExtendedControl.GetAutoSize: TALAutoSizeMode;
begin
  result := FAutoSize;
end;

{****************************************************************************}
procedure TALDynamicExtendedControl.SetAutoSize(const Value: TALAutoSizeMode);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    {$IFNDEF ALCompilerVersionSupported123}
      {$MESSAGE WARN 'Delete this temp hack'}
    {$ENDIF}
    //**{$IF defined(ALBackwardCompatible)}
    //**if FAutoSize = TALAutoSizeMode.False then
    //**  FAutoSize := TALAutoSizeMode.None
    //**else if FAutoSize = TALAutoSizeMode.True then
    //**  FAutoSize := TALAutoSizeMode.Both;
    //**{$ENDIF}
    AdjustSize;
  end;
end;

{************************************************************************}
function TALDynamicExtendedControl.HasUnconstrainedAutosizeWidth: Boolean;
begin
  Result := GetAutoSize in [TALAutoSizeMode.Both, TALAutoSizeMode.Width];
  if Result then begin
    result := not (Align in [TALAlignLayout.Client,
                             TALAlignLayout.Contents,
                             TALAlignLayout.Top,
                             TALAlignLayout.Bottom,
                             TALAlignLayout.MostTop,
                             TALAlignLayout.MostBottom,
                             TALAlignLayout.Horizontal,
                             TALAlignLayout.VertCenter]);
    if (not result) and (Owner <> nil) then
      Result := Owner.HasUnconstrainedAutosizeWidth;
  end;
end;

{*************************************************************************}
function TALDynamicExtendedControl.HasUnconstrainedAutosizeHeight: Boolean;
begin
  Result := GetAutoSize in [TALAutoSizeMode.Both, TALAutoSizeMode.Height];
  if Result then begin
    result := not (Align in [TALAlignLayout.Client,
                             TALAlignLayout.Contents,
                             TALAlignLayout.Left,
                             TALAlignLayout.Right,
                             TALAlignLayout.MostLeft,
                             TALAlignLayout.MostRight,
                             TALAlignLayout.Vertical,
                             TALAlignLayout.HorzCenter]);
    if (not result) and (Owner <> nil) then
      Result := Owner.HasUnconstrainedAutosizeHeight;
  end;
end;

{**************************************************}
procedure TALDynamicExtendedControl.MakeBufDrawable;
begin
 // Virtual;
end;

{***************************************************}
procedure TALDynamicExtendedControl.ClearBufDrawable;
begin
 // Virtual;
end;

{**************************************************************}
function TALDynamicExtendedControl.GetAutoAlignToPixel: Boolean;
begin
  Result := FAutoAlignToPixel;
end;

{*****************************************************************************}
procedure TALDynamicExtendedControl.SetAutoAlignToPixel(const AValue: Boolean);
begin
  FAutoAlignToPixel := AValue;
end;

{*******************************************************************************************}
function TALDynamicExtendedControl.IsReadyToDisplay(const AStrict: Boolean = False): Boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function CheckAllChildrenAreReadyToDisplay(const AControl: TALDynamicControl): boolean;
  begin
    Result := True;
    for var I := 0 to AControl.ControlsCount - 1 do begin
      //**if AControl.Controls[i] is TALDynamicControl then Result := TALDynamicControl(AControl.Controls[i]).IsReadyToDisplay(AStrict)
      //**else Result := CheckAllChildrenAreReadyToDisplay(AControl.Controls[i]);
      Result := AControl.Controls[i].IsReadyToDisplay(AStrict);
      if not Result then exit;
    end;
  end;

begin
  MakeBufDrawable;
  Result := CheckAllChildrenAreReadyToDisplay(Self);
end;

{******************************************************}
function TALDynamicExtendedControl.IsDisplayed: Boolean;
begin
  Result := not GetAbsoluteDisplayedRect.IsEmpty;
end;

{**********************************************************************}
//**function TALDynamicExtendedControl.GetAbsoluteDisplayedRect: TRectF;
//**begin
//**  if (not Visible) or (form = nil) then Exit(TRectF.Empty);
//**  var LAbsoluteIntersectionRect := AbsoluteRect;
//**  var LControlTmp := Owner;
//**  while LControlTmp <> nil do begin
//**    if not LControlTmp.Visible then Exit(TRectF.Empty);
//**    if LControlTmp.ClipChildren then begin
//**      var LAbsoluteClipRect := LControlTmp.LocalToAbsolute(LControlTmp.ClipRect);
//**      LAbsoluteIntersectionRect.Intersect(LAbsoluteClipRect);
//**      if LAbsoluteIntersectionRect.IsEmpty then
//**        Exit(TRectF.Empty);
//**    end;
//**    LControlTmp := LControlTmp.Owner;
//**  end;
//**  Result := TRectF.Intersect(Form.ClientRect, LAbsoluteIntersectionRect)
//**end;

{*******************************************************************}
//**function TALDynamicExtendedControl.FillTextFlags: TFillTextFlags;
//**begin
//**  if (Root = nil) then result := ALGetFillTextFlags
//**  else result := inherited;
//**end;

{******************************************************************}
//**procedure TALDynamicExtendedControl.SetNewScene(AScene: IScene);
//**begin
//**  {$IFNDEF ALCompilerVersionSupported123}
//**    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
//**    {$MESSAGE WARN 'Check if FMX.Controls.TControl.Pressed is still changed only in SetNewScene/MouseLeave/MouseDown/MouseUp/MouseClick'}
//**    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsFocused is still changed only in SetNewScene/DoEnter/DoExit'}
//**    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsMouseOver is still changed only in SetNewScene/MouseEnter/MouseLeave'}
//**  {$ENDIF}
//**  var LPrevPressed := Pressed;
//**  var LPrevIsFocused := IsFocused;
//**  var LPrevIsMouseOver := IsMouseOver;
//**  inherited;
//**  {$IF defined(ANDROID) or defined(IOS)}
//**  FIsMouseOver := False;
//**  {$ENDIF}
//**  if LPrevPressed <> Pressed then PressedChanged;
//**  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
//**  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
//**end;

{*********************************************************}
//**function TALDynamicExtendedControl.GetPivot: TPosition;
//**begin
//**  Result := Inherited RotationCenter;
//**end;

{***********************************************************************}
//**procedure TALDynamicExtendedControl.SetPivot(const Value: TPosition);
//**begin
//**  Inherited RotationCenter := Value;
//**end;

{*****************************************************}
function TALDynamicExtendedControl.GetPressed: Boolean;
begin
  result := inherited Pressed;
end;

{********************************************************************}
procedure TALDynamicExtendedControl.SetPressed(const AValue: Boolean);
begin
  if AValue <> GetPressed then begin
    inherited Pressed := AValue;
    pressedChanged;
  end;
end;

{**********************************************}
//**procedure TALDynamicExtendedControl.DoEnter;
//**begin
//**  var LPrevIsFocused := IsFocused;
//**  inherited;
//**  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
//**end;

{*********************************************}
//**procedure TALDynamicExtendedControl.DoExit;
//**begin
//**  var LPrevIsFocused := IsFocused;
//**  inherited;
//**  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
//**end;

{*********************************************}
procedure TALDynamicExtendedControl.MouseEnter;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{*********************************************}
procedure TALDynamicExtendedControl.MouseLeave;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if not AutoCapture then
    Pressed := False;
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{****************************************************************************************************}
procedure TALDynamicExtendedControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  //--
  FControlAbsolutePosAtMouseDown := LocalToAbsolute(TPointF.Zero);
  FMouseDownAtRest := not IsInMotion;
  //--
  FDoubleClick := ssDouble in Shift;
  if FDoubleClick then begin
    {$IF defined(IOS)}
    //**if FForm <> nil then
    //**  TALFMXViewBaseAccessPrivate(WindowHandleToPlatform(FForm.Handle).Handle).FShouldIgnoreNextClick := False;
    {$ENDIF}
    Shift := Shift - [ssDouble];
  end;
  //--
  //**if (not FFocusOnMouseDown) or (FFocusOnMouseUp) or (not FMouseDownAtRest) then begin
  //**  Var LOldIsfocused := FIsfocused;
  //**  FIsfocused := True;
  //**  Try
  //**    inherited;
  //**  finally
  //**    FIsfocused := LOldIsfocused;
  //**  End;
  //**end
  //**else
    inherited;
  //--
  if LPrevPressed <> Pressed then PressedChanged;
end;

{**************************************************************************************************}
procedure TALDynamicExtendedControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
  FDoubleClick := False;
  //**var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  //**if (FFocusOnMouseUp) and
  //**   (FMouseDownAtRest) and
  //**   (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) <= TALScrollEngine.DefaultTouchSlop) and
  //**   (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) <= TALScrollEngine.DefaultTouchSlop) and
  //**   (not (csDesigning in ComponentState)) and
  //**   (not FIsFocused) then
  //**  SetFocus;
end;

{*****************************************************************************************************}
procedure TALDynamicExtendedControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (not FMouseDownAtRest) or
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) or
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then begin
    {$IF defined(debug)}
    if (not FMouseDownAtRest) then
      ALLog(Classname+'.MouseClick', 'Skipped | Mouse Down was not made at Low Velocity')
    else if (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) then
      ALLog(Classname+'.MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x)) + ' horizontally')
    else if (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then
      ALLog(Classname+'.MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y)) + ' vertically')
    else
      raise Exception.Create('Error 79BF6F83-8725-476D-A283-507BE9CC671C');
    {$ENDIF}
    exit;
  end;
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
end;

{***********************************************}
procedure TALDynamicExtendedControl.DoClickSound;
begin
  if (ClickSound=TALClickSoundMode.Always) or
     ((assigned(OnClick)) and
      (ClickSound=TALClickSoundMode.Default) and
      (ALGlobalClickSoundEnabled)) then
    ALPlayClickSound;
end;

{****************************************}
procedure TALDynamicExtendedControl.Click;
begin
  DoClickSound;
  inherited;
  if FDoubleClick then begin
    DblClick;
    FDoubleClick := False;
  end;
end;

{*****************************************************}
function TALDynamicExtendedControl.IsInMotion: Boolean;
begin
  If (Owner <> nil) then begin
    //**var LScrollableControl: IALScrollableControl;
    //**if (Supports(Owner, IALScrollableControl, LScrollableControl)) and
    //**   (not LScrollableControl.GetScrollEngine.IsVelocityLow) then result := True
    if (Owner.ScrollEngine <> nil) and (not Owner.ScrollEngine.IsVelocityLow) then result := True
    else result := Owner.IsInMotion;
  end
  else result := False;
end;

{*************************************}
// Optimized GetParentedVisible to work
// exclusively with Owner.
//**function TALDynamicExtendedControl.GetParentedVisible: Boolean;
//**begin
//**  {$IF defined(ALDPK)}
//**  result := Inherited GetParentedVisible;
//**  {$ELSE}
//**  var P: TALDynamicControl := Self;
//**  Result := False;
//**  while P <> nil do begin
//**    if not p.Visible then Exit;
//**    P := P.Owner;
//**  end;
//**  // We do not care about FForm.visible like in
//**  // Inherited GetParentedVisible
//**  // Result := (FForm = nil) or (FForm.visible);
//**  Result := True;
//**  {$ENDIF}
//**end;

{***********************************************************************}
//**procedure TALDynamicExtendedControl.DoMatrixChanged(Sender: TObject);
//**begin
//**  {$IFNDEF ALCompilerVersionSupported123}
//**    {$MESSAGE WARN 'Check if FMX.Controls.TControl.DoMatrixChanged was not updated and adjust the IFDEF'}
//**    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2823 was not corrected and adjust the IFDEF'}
//**  {$ENDIF}
//**  if not FInPaintTo and not IsUpdating then
//**    Repaint;
//**  if SameValue(Scale.X, 1.0, TEpsilon.Scale) and SameValue(Scale.Y, 1.0, TEpsilon.Scale) and SameValue(RotationAngle, 0.0, TEpsilon.Scale) then
//**  begin
//**    if (Owner <> nil) and not TALDynamicControlAccessPrivate(Owner).FSimpleTransform then
//**      TALDynamicControlAccessPrivate(Self).FSimpleTransform := False
//**    else
//**      TALDynamicControlAccessPrivate(Self).FSimpleTransform := True;
//**  end
//**  else
//**    TALDynamicControlAccessPrivate(Self).FSimpleTransform := False;
//**
//**  if not TALDynamicControlAccessPrivate(Self).FSimpleTransform then
//**  begin
//**    if not SameValue(RotationAngle, 0.0, TEpsilon.Scale) then
//**    begin
//**      FLocalMatrix :=
//**        TMatrix.CreateTranslation(-Pivot.X * FSize.Width, -Pivot.Y * FSize.Height) *
//**        TMatrix.CreateScaling(Scale.X, Scale.Y) *
//**        TMatrix.CreateRotation(DegToRad(RotationAngle)) *
//**        TMatrix.CreateTranslation(Pivot.X * FSize.Width + Left, Pivot.Y * FSize.Height + Top);
//**    end
//**    else
//**    begin
//**      FLocalMatrix := TMatrix.Identity;
//**      FLocalMatrix.m31 := Left + ((1 - Scale.X) * Pivot.X * FSize.Width);
//**      FLocalMatrix.m32 := Top + ((1 - Scale.Y) * Pivot.Y * FSize.Height);
//**      FLocalMatrix.m11 := Scale.X;
//**      FLocalMatrix.m22 := Scale.Y;
//**    end;
//**  end
//**  else
//**  begin
//**    FLocalMatrix := TMatrix.Identity;
//**    FLocalMatrix.m31 := Left;
//**    FLocalMatrix.m32 := Top;
//**  end;
//**
//**  RecalcAbsolute;
//**  RecalcUpdateRect;
//**  if HasDisablePaintEffect then
//**    UpdateEffects;
//**  if Visible then
//**    ParentContentChanged;
//**
//**  if not GetAnchorMove then
//**  begin
//**    UpdateExplicitBounds;
//**    UpdateAnchorRules(True);
//**  end;
//**  if not FInPaintTo and not IsUpdating then
//**    Repaint;
//**end;

{****************************************************}
//**procedure TALDynamicExtendedControl.DoRootChanged;
//**begin
//**  inherited;
//**  if Root is TCommonCustomForm then FForm := TCommonCustomForm(Root)
//**  else begin
//**    FForm := nil;
//**    {$IF defined(ALDPK)}
//**    // At design time, the root is not a TCommonCustomForm,
//**    // but an opaque TFmxDesignSurface instance.
//**    // I happen to know (don’t ask how) that TFmxDesignSurface has
//**    // a private field named:
//**    //   FForm: FMX.Forms.TCommonCustomForm;
//**    if Root is TObject then begin
//**      var LObj := TObject(Root);
//**      var LContext: TRttiContext;
//**      var LType: TRttiType := LContext.GetType(LObj.ClassType);
//**      var LField: TRttiField := LType.GetField('FForm');
//**      if Assigned(LField) then
//**        FForm := TCommonCustomForm(LField.GetValue(LObj).AsObject);
//**    end;
//**    {$ENDIF}
//**  end;
//**end;

{*****************************************************}
procedure TALDynamicExtendedControl.IsMouseOverChanged;
begin
  // virtual
end;

{*******************************************************}
//**procedure TALDynamicExtendedControl.IsFocusedChanged;
//**begin
//**  // virtual
//**end;

{*************************************************}
procedure TALDynamicExtendedControl.PressedChanged;
begin
  // virtual
end;

{*****************************************************}
//**Procedure TALDynamicExtendedControl.MarginsChanged;
//**begin
//**  // virtual
//**end;

{*************************************************}
procedure TALDynamicExtendedControl.PaddingChanged;
begin
  Inherited;
  AdjustSize;
end;

{************************************************}
procedure TALDynamicExtendedControl.ParentChanged;
begin
  inherited;
  // Note: The procedure TControl.PaintTo(const ACanvas: TCanvas; const ARect: TRectF; const AParent: TFmxObject = nil)
  // temporarily updates the Owner. Unfortunately, Owner is not a virtual property,
  // and TControl.UpdateParentProperties is strictly private. As a result, within TControl.PaintTo,
  // the value of FOwner will be incorrect.
  //**if (Owner <> nil) and (Owner is TALDynamicControl) then
  //**  FOwner := TALDynamicControl(Owner)
  //**else
  //**  FOwner := nil;
end;

{*****************************************************************************}
//**procedure TALDynamicExtendedControl.MarginsChangedHandler(Sender: TObject);
//**begin
//**  if Assigned(FFormerMarginsChangedHandler) then
//**    FFormerMarginsChangedHandler(Sender);
//**  MarginsChanged;
//**end;

{***************************************************************************}
//**procedure TALDynamicExtendedControl.ScaleChangedHandler(Sender: TObject);
//**begin
//**  Repaint; //** DoMatrixChanged(Sender);
//**end;

{**********************************************************}
constructor TALDynamicContent.Create(const AOwner: TObject);
begin
  inherited;
  //**SetAcceptsControls(False);
end;

{****************************************************************************************************}
procedure TALDynamicContent.DoInsertControl(const AControl: TALDynamicControl; const AIndex: Integer);
begin
  inherited;
  ContentChanged;
end;

{*****************************************************************************}
procedure TALDynamicContent.DoRemoveControl(const AControl: TALDynamicControl);
begin
  inherited;
  ContentChanged;
end;

{***********************************************}
//**procedure TALDynamicContent.DoDeleteChildren;
//**begin
//**  inherited;
//**  ContentChanged;
//**end;

{************************************}
procedure TALDynamicContent.DoRealign;
//var
//  AlignRoot: IAlignRoot;
begin
//  if (Parent <> nil) and not(csLoading in Parent.ComponentState) then
//    inherited;
//  if (Parent <> nil) and not FParentAligning and not(csLoading in ComponentState) then
//  begin
//    FParentAligning := True;
//    if Owner <> nil then
//      _TALDynamicControlProtectedAccess(Owner).Realign
//    else
//      if not(csLoading in ComponentState) and Supports(Parent, IAlignRoot, AlignRoot) then
//        AlignRoot.Realign;
//    FParentAligning := False;
//  end;

  // There is nothing wrong with the previous implementation.
  // This code is taken from TContent. We must call ContentChanged every time we realign,
  // because if one of its child controls changes its position or size during realignment,
  // ContentChanged would normally be triggered. However, since ContentChanged is deactivated
  // (i.e., FDisableAlign is checked), DoContentChanged will not be called,
  // and thus the parent of the content will not be notified.

  inherited;
  ContentChanged;

end;

{*******************************************}
procedure TALDynamicContent.DoContentChanged;
begin
  // Virtual
end;

{*****************************************}
procedure TALDynamicContent.ContentChanged;
begin

  // ContentChanged is called by the TControl.ParentContentChanged function,
  // which in turn is invoked by:
  //
  //  * TControl.DoMatrixChanged (i.e., when the position of a child control changes)
  //  * TControl.SetAlign (i.e., when the alignment property of a child control changes)
  //  * TControl.SetVisible (i.e., when the visibility of a child control changes)
  //  * TControl.InternalSizeChanged (i.e., when the size of a child control changes)
  //
  // Monitoring this event is important so that we can update the size of TALDynamicContent
  // to best fit its content—this is especially crucial for components such as TALScrollBox.

  {$IF defined(debug)}
  //ALLog(ClassName+'.ContentChanged', 'Name: ' + Name);
  {$ENDIF}

  // * We do not call DoContentChanged if IsUpdating because calling EndUpdate triggers a
  //   realignment (DoEndUpdate > Realign > DoRealign) that invokes ContentChanged again.
  // * Likewise, we ignore FDisableAlign since ContentChanged will be called again
  //   (in DoRealign) once the alignment process is complete.
  if (not IsUpdating) and (not FDisableAlign) and (not IsDestroying) then
    DoContentChanged;

end;

{************************************************************}
//**function TALDynamicContent.GetTabListClass: TTabListClass;
//**begin
//**  Result := TALDynamicContentTabList;
//**end;

{**********************************************************************}
//**function TALDynamicContent.GetTabStopController: ITabStopController;
//**var
//**  Control: IControl;
//**begin
//**  if Supports(Parent, IControl, Control) then
//**    Result := Control.GetTabStopController
//**  else
//**    Result := nil;
//**end;

{$ENDREGION 'Auto-generated by <ALCINOE>\Tools\CodeBuilder (2)'}

end.
