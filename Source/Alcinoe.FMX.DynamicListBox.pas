unit Alcinoe.FMX.DynamicListBox;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.SyncObjs,
  System.Messaging,
  System.Generics.Collections,
  System.Diagnostics,
  System.Net.HttpClient,
  {$IF defined(ANDROID)}
  FMX.types3D,
  {$ENDIF}
  {$IF defined(IOS)}
  FMX.types3D,
  {$ENDIF}
  FMX.graphics,
  FMX.Controls,
  FMX.types,
  FMX.StdActns,
  Fmx.Forms,
  Alcinoe.JSONDoc,
  Alcinoe.FMX.Ani,
  Alcinoe.Common,
  Alcinoe.FMX.CacheEngines,
  Alcinoe.StringList,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.BreakText,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.fmx.Common;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBox = class;
  TALDynamicListBoxItem = class;
  TALDynamicListBoxView = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALDynamicListBoxControl is similar to TControl
  // but optimized for minimal overhead.
  TALDynamicListBoxControl = class(TObject)
  public
    type
      TOnPaintEvent = procedure(Sender: TALDynamicListBoxControl; Canvas: TCanvas) of object;
  private
    FOwnerControl: TALDynamicListBoxControl; // 8 bytes | [TControl] FOwner: TComponent; FParentControl: TControl;
    FOwnerItem: TALDynamicListBoxItem; // 8 bytes
    FOwnerView: TALDynamicListBoxView; // 8 bytes
    FOwnerListBox: TALDynamicListBox; // 8 bytes
    // The order of the list determines the alignment priority and z-order: the
    // first control in the list aligns and paints before the others. first
    // control in the list catches touch events after others
    FControls: TArray<TALDynamicListBoxControl>; // 8 bytes | [TControl] FControls: TControlList;
    FName: string; // 8 bytes | [TControl] FName: TComponentName;
    FLeft: Double; // 8 bytes | [TControl] FPosition: TPosition;
    FTop: Double; // 8 bytes | [TControl] FPosition: TPosition;
    FWidth: Double; // 8 bytes | [TControl] FSize: TControlSize;
    FHeight: Double; // 8 bytes | [TControl] FSize: TControlSize;
    FTouchTargetExpansion: TRectF; // 16 bytes | [TControl] FTouchTargetExpansion: TBounds;
    FPivot: TPointF; // 8 Bytes | [TControl] FRotationCenter: TPosition;
    FPadding: TALBounds; // 8 bytes | [TControl] FPadding: TBounds;
    FMargins: TALBounds; // 8 bytes | [TControl] FMargins: TBounds;
    FPressedPosition: TPointF; // 8 bytes | [TControl] FPressedPosition: TPointF;
    FCanvas: TCanvas; // 8 bytes;
    FIsDestroying: boolean; // 1 byte | [TControl] FComponentState: TComponentState; & csDestroying
    FIsEphemeral: boolean; // 1 byte
    FEnabled: Boolean; // 1 byte | [TControl] FEnabled: Boolean;
    FAbsoluteEnabled: Boolean; // 1 byte | [TControl] FAbsoluteEnabled: Boolean;
    FDisableAlign: Boolean; // 1 byte | [TControl] FDisableAlign: Boolean;
    FAlign: TALAlignLayout; // 1 byte | [TControl] FAlign: TAlignLayout;
    FVisible: Boolean; // 1 byte | [TControl] FVisible: Boolean;
    FAbsoluteVisible: Boolean; // 1 byte
    FHitTest: Boolean; // 1 byte | [TControl] FHitTest: Boolean;
    FIsMouseOver: Boolean; // 1 byte | [TControl] FIsMouseOver: Boolean;
    FPressed: Boolean; // 1 byte | [TControl] FPressed: Boolean;
    FAutoCapture: Boolean; // 1 byte | [TControl] FAutoCapture: Boolean;
    FOpacity: Single; // 4 bytes | [TControl] FOpacity: Single;
    FDisabledOpacity: Single; // 4 bytes | [TControl] FDisabledOpacity: Single;
    FAbsoluteOpacity: Single; // 4 bytes | [TControl] FAbsoluteOpacity: Single;
    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    FCursor: TCursor; // 2 bytes | [TControl] FCursor: TCursor;
    FAbsoluteCursor: TCursor; // 2 bytes | [TControl] FInheritedCursor: TCursor;
    {$ENDIF}
    FUpdating: Integer; // 4 bytes | [TControl] FUpdating: Integer;
    FIndex: Integer; // 4 bytes
    FOnMouseEnter: TNotifyEvent; // 16 bytes | [TControl] FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent; // 16 bytes | [TControl] FOnMouseLeave: TNotifyEvent;
    FOnMouseDown: TMouseEvent; // 16 bytes | [TControl] FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent; // 16 bytes | [TControl] FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent; // 16 bytes | [TControl] FOnMouseUp: TMouseEvent;
    FOnClick: TNotifyEvent; // 16 bytes | [TControl] FOnClick: TNotifyEvent;
    FOnPaint: TOnPaintEvent; // 16 bytes | [TControl] FOnPaint: TOnPaintEvent;
    FOnPainting: TOnPaintEvent; // 16 bytes | [TControl] FOnPainting: TOnPaintEvent;
    FOnResized: TNotifyEvent; // 16 bytes | [TControl] FOnResized: TNotifyEvent;
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
    procedure SetOwnerControl(const Value: TALDynamicListBoxControl);
    function GetControl(Index: Integer): TALDynamicListBoxControl;
    function GetControlsCount: Integer; // [TControl] function GetControlsCount: Integer;
    function GetForm: TCommonCustomForm;
  protected
    function CreateMargins: TALBounds; virtual;
    function CreatePadding: TALBounds; virtual;
    procedure SetBounds(X, Y, AWidth, AHeight: Double); virtual; // [TControl] procedure SetBounds(X, Y, AWidth, AHeight: Single); virtual;
    function GetBoundsRect: TALRectD; // [TControl] function GetBoundsRect: TRectF; virtual;
    procedure SetBoundsRect(const Value: TALRectD); // [TControl] procedure SetBoundsRect(const Value: TRectF); virtual;
    function GetLocalRect: TALRectD; virtual; // [TControl] function GetLocalRect: TRectF; virtual;
    function GetExpandedLocalRect: TALRectD;
    function GetAbsoluteRect: TALRectD; virtual; // [TControl] function GetAbsoluteRect: TRectF; virtual;
    function GetExpandedBoundsRect: TALRectD;
    procedure SetPosition(const AValue: TALPointD); overload;
    procedure SetPosition(const AValue: TPointf); overload;
    procedure SetSize(const ASize: TALSizeD); overload;
    procedure SetSize(const ASize: TSizeF); overload;
    procedure SetSize(const AWidth, AHeight: Double); overload;
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
    function IsVisibleWithinListboxBounds: Boolean; virtual;
    procedure RefreshAbsoluteVisible;
    procedure SetAlign(const Value: TALAlignLayout); virtual; // [TControl] procedure SetAlign(const Value: TAlignLayout); virtual;
    function GetPriority(const AContext: Tobject): Int64; virtual;
    function GetDoubleBuffered: boolean; virtual; abstract;
    procedure SetDoubleBuffered(const AValue: Boolean); virtual; abstract;
    function FillTextFlags: TFillTextFlags; virtual; // [TControl] function FillTextFlags: TFillTextFlags; virtual;
    procedure PaddingChanged; virtual; // [TControl] procedure PaddingChanged; overload; virtual;
    procedure MarginsChanged; virtual;
    procedure EnabledChanged; virtual; // [TControl] procedure EnabledChanged; virtual;
    procedure VisibleChanged; virtual; // [TControl] procedure VisibleChanged; virtual;
    procedure ParentChanged; virtual; // [TControl] procedure ParentChanged; virtual;
    procedure AncestorParentChanged; virtual; // [TControl] procedure AncestorParentChanged; virtual;
    procedure PositionChanged; virtual;
    procedure SizeChanged; virtual; // [TControl] procedure SizeChanged(Sender: TObject);
    procedure DoResized; virtual; // [TControl] procedure DoResized; virtual;
    procedure Realign; // [TControl] procedure Realign;
    procedure DoRealign; virtual; // [TControl] procedure DoRealign; virtual;
    function GetUnalignedSpace: TALRectD;
    procedure DoBeginUpdate; virtual; // [TControl] procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual; // [TControl] procedure DoEndUpdate; virtual;
    procedure BeginTextUpdate; virtual; abstract;
    procedure EndTextUpdate; virtual; abstract;
    procedure MouseEnter; virtual; // [TControl] procedure DoMouseEnter; virtual;
    procedure MouseLeave; virtual; // [TControl] procedure DoMouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // [TControl] procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual; // [TControl] procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // [TControl] procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // [TControl] procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure ChildrenMouseDown(const AObject: TALDynamicListBoxControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure ChildrenMouseMove(const AObject: TALDynamicListBoxControl; Shift: TShiftState; X, Y: Single); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure ChildrenMouseUp(const AObject: TALDynamicListBoxControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure ChildrenMouseEnter(const AObject: TALDynamicListBoxControl); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure ChildrenMouseLeave(const AObject: TALDynamicListBoxControl); virtual; // https://quality.embarcadero.com/browse/RSP-24397
    procedure Click; virtual; // [TControl] procedure Click; virtual;
    procedure Capture; // [TControl] procedure Capture;
    procedure ReleaseCapture; // [TControl] procedure ReleaseCapture;
    procedure PaintInternal(const ACanvas: TCanvas); virtual; // [TControl] procedure PaintInternal;
    procedure Painting; virtual; // [TControl] procedure Painting; virtual;
    procedure Paint; virtual; // [TControl] procedure Paint; virtual;
    function PaintChildrenOnly: Boolean; virtual;
    procedure PaintChildren; virtual;  // [TControl] procedure PaintChildren; virtual;
    procedure AfterPaint; virtual; // [TControl] procedure AfterPaint; virtual;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               out AControlPos: TALPointD; // AControlPos is local to the founded control
               const ACheckHitTest: Boolean = true): TALDynamicListBoxControl; overload; virtual; // [TControl] function ObjectAtPoint(AScreenPoint: TPointF): IControl; virtual;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               const ACheckHitTest: Boolean = true): TALDynamicListBoxControl; overload; virtual; // [TControl] function ObjectAtPoint(AScreenPoint: TPointF): IControl; virtual;
  public
    constructor Create(const AOwner: TObject); virtual; // [TControl] constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; // [TControl] destructor Destroy; override;
    procedure BeforeDestruction; override;
    Property IsDestroying: boolean read FIsDestroying; // [MultiThread]
    Property IsEphemeral: boolean read FIsEphemeral write FIsEphemeral; // [MultiThread]
    procedure BeginUpdate; virtual; // [TControl] procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual; // [TControl] procedure EndUpdate; virtual;
    function IsUpdating: Boolean; virtual; // [TControl] function IsUpdating: Boolean; virtual;
    procedure AddControl(const AControl: TALDynamicListBoxControl); inline; // [TControl] procedure AddObject(const AObject: TFmxObject);
    procedure InsertControl(const AControl: TALDynamicListBoxControl; const AIndex: Integer); virtual; // [TControl] procedure InsertObject(Index: Integer; const AObject: TFmxObject);
    procedure RemoveControl(const AControl: TALDynamicListBoxControl); virtual; // [TControl] procedure RemoveObject(const AObject: TFmxObject); overload;
    procedure MoveControl(const AControl: TALDynamicListBoxControl; const ANewIndex: Integer); virtual; // [TControl] procedure Exchange(const AObject1, AObject2: TFmxObject); virtual;
    procedure MakeBufDrawable; virtual; abstract;
    procedure ClearBufDrawable; virtual; abstract;
    procedure Repaint; // [TControl] procedure Repaint;
    procedure BringToFront; virtual; // [TFmxObject] procedure BringToFront;
    procedure SendToBack; virtual; // [TFmxObject] procedure SendToBack;
    function HasUnconstrainedAutosizeX: Boolean; virtual; abstract;
    function HasUnconstrainedAutosizeY: Boolean; virtual; abstract;
    function AbsoluteToLocal(const APoint: TALPointD): TALPointD; overload; virtual; // [TControl] function AbsoluteToLocal(const APoint: TPointF): TPointF; overload; virtual;
    function AbsoluteToLocal(const ARect: TALRectD): TALRectD; overload; virtual; // [TControl] function AbsoluteToLocal(const ARect: TRectF): TRectF; overload;
    function AbsoluteToLocal(const APoint: TPointF): TALPointD; overload; // [TControl] function AbsoluteToLocal(const APoint: TPointF): TPointF; overload; virtual;
    function AbsoluteToLocal(const ARect: TRectF): TALRectD; overload; // [TControl] function AbsoluteToLocal(const ARect: TRectF): TRectF; overload;
    function LocalToAbsolute(const APoint: TALPointD): TALPointD; overload; virtual; // [TControl] function LocalToAbsolute(const APoint: TPointF): TPointF; overload; virtual;
    function LocalToAbsolute(const ARect: TALRectD): TALRectD; overload; virtual; // [TControl] function LocalToAbsolute(const ARect: TRectF): TRectF; overload;
    function LocalToAbsolute(const APoint: TPointF): TALPointD; overload; // [TControl] function LocalToAbsolute(const APoint: TPointF): TPointF; overload; virtual;
    function LocalToAbsolute(const ARect: TRectF): TALRectD; overload; // [TControl] function LocalToAbsolute(const ARect: TRectF): TRectF; overload;
    function PointInObjectLocal(X, Y: Double): Boolean; virtual; // [TControl] function PointInObject(X, Y: Single): Boolean; virtual;
    property Name: String read FName write FName; // [TControl:published] property Name: TComponentName read FName write SetName stored False;
    property Enabled: Boolean read FEnabled write SetEnabled; // [TControl] property Enabled: Boolean read FEnabled write SetEnabled stored EnabledStored default True;
    property AbsoluteEnabled: Boolean read FAbsoluteEnabled; // [TControl] property AbsoluteEnabled: Boolean read GetAbsoluteEnabled;
    property Opacity: Single read FOpacity write SetOpacity; // [TControl] property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
    property DisabledOpacity: Single read FDisabledOpacity write SetDisabledOpacity; // [TControl] property DisabledOpacity: Single read FDisabledOpacity write SetDisabledOpacity stored DisabledOpacityStored nodefault;
    property AbsoluteOpacity: Single read FAbsoluteOpacity; // [TControl] property AbsoluteOpacity: Single read GetAbsoluteOpacity;
    property Visible: Boolean read FVisible write SetVisible; // [TControl] property Visible: Boolean read FVisible write SetVisible stored VisibleStored default True;
    property AbsoluteVisible: Boolean read FAbsoluteVisible;
    property OwnerControl: TALDynamicListBoxControl read FOwnerControl write SetOwnerControl; // [TControl] property Owner: TComponent read FOwner;
    Property OwnerItem: TALDynamicListBoxItem read FOwnerItem;
    Property OwnerView: TALDynamicListBoxView read FOwnerView;
    property OwnerListBox: TALDynamicListBox read FOwnerListBox;
    property Form: TCommonCustomForm read GetForm;
    property Canvas: TCanvas read FCanvas; // [TControl] property Canvas: TCanvas read GetCanvas;
    property BoundsRect: TALRectD read GetBoundsRect write SetBoundsRect; // [TControl] property BoundsRect: TRectF read GetBoundsRect write SetBoundsRect;
    property LocalRect: TALRectD read GetLocalRect; // [TControl] property LocalRect: TRectF read GetLocalRect;
    property AbsoluteRect: TALRectD read GetAbsoluteRect; // [TControl] property AbsoluteRect: TRectF read GetAbsoluteRect;
    property ExpandedBoundsRect: TALRectD read GetExpandedBoundsRect;
    property ExpandedLocalRect: TALRectD read GetExpandedLocalRect;
    property TouchTargetExpansion: TRectF read FTouchTargetExpansion write SetTouchTargetExpansion; // [TControl] property TouchTargetExpansion: TBounds read FTouchTargetExpansion write SetTouchTargetExpansion;
    property Pivot: TPointF read FPivot write FPivot; // [TControl] property RotationCenter: TPosition read GetRotationCenter write SetRotationCenter;
    property Left: Double read FLeft write SetLeft; // [TControl] property Position: TPosition read FPosition write SetPosition stored IsPositionStored;
    property Top: Double read FTop write SetTop; // [TControl] property Position: TPosition read FPosition write SetPosition stored IsPositionStored;
    property Right: Double read GetRight;
    property Bottom: Double read GetBottom;
    property Width: Double read FWidth write SetWidth; // [TControl] property Width: Single read GetWidth write SetWidth stored False nodefault;
    property Height: Double read FHeight write SetHeight; // [TControl] property Height: Single read GetHeight write SetHeight stored False nodefault;
    property Padding: TALBounds read FPadding write SetPadding; // [TControl] property Padding: TBounds read GetPadding write SetPadding;
    property Margins: TALBounds read FMargins write SetMargins; // [TControl] property Margins: TBounds read GetMargins write SetMargins;
    property Align: TALAlignLayout read FAlign write SetAlign; // [TControl] property Align: TAlignLayout read FAlign write SetAlign default TAlignLayout.None;
    property HitTest: Boolean read FHitTest write FHitTest; // [TControl] property HitTest: Boolean read FHitTest write SetHitTest default True;
    property Cursor: TCursor read GetCursor write SetCursor; // [TControl] property Cursor: TCursor read GetCursor write SetCursor default crDefault;
    property AbsoluteCursor: TCursor read GetAbsoluteCursor; // [TControl] property InheritedCursor: TCursor read GetInheritedCursor default crDefault;
    property Controls[Index: Integer]: TALDynamicListBoxControl read GetControl; // [TControl] property Controls: TControlList read GetControls;
    property ControlsCount: Integer read GetControlsCount; // [TControl] property ControlsCount: Integer read GetControlsCount;
    property Index: Integer read FIndex; // [TControl] property Index: Integer read GetIndex write SetIndex;
    property IsMouseOver: Boolean read FIsMouseOver; // [TControl] property IsMouseOver: Boolean read FIsMouseOver;
    property Pressed: Boolean read FPressed write FPressed; // [TControl] property Pressed: Boolean read FPressed write FPressed;
    property PressedPosition: TPointF read FPressedPosition write FPressedPosition; // [TControl] property PressedPosition: TPointF read FPressedPosition write FPressedPosition;
    property AutoCapture: Boolean read FAutoCapture write FAutoCapture; // [TControl] property AutoCapture: Boolean read FAutoCapture write FAutoCapture default False;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;// [TControl] property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;// [TControl] property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;// [TControl] property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnClick: TNotifyEvent read FOnClick write FOnClick; // [TControl] property OnClick: TNotifyEvent read FOnClick write SetOnClick stored OnClickStored;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;// [TControl] property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;// [TControl] property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPaint: TOnPaintEvent read FOnPaint write FOnPaint; // [TControl] property OnPaint: TOnPaintEvent read FOnPaint write FOnPaint;
    property OnPainting: TOnPaintEvent read FOnPainting write FOnPainting; // [TControl] property OnPainting: TOnPaintEvent read FOnPainting write FOnPainting;
    property OnResized: TNotifyEvent read FOnResized write FOnResized; // [TControl] property OnResized: TNotifyEvent read FOnResized write FOnResized;
  end;

//////////////////////////////////////////////
/// THE CODE BELOW WAS AUTO-GENERATED FROM ///
/// <ALCINOE>\Tools\CodeBuilder.           ///
//////////////////////////////////////////////

{$REGION 'AUTO-GENERATED'}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxExtendedControl = class(TALDynamicListBoxControl)
  private
    //FForm: TCommonCustomForm; // 8 bytes
    //FOwnerControl: TALDynamicListBoxControl; // 8 bytes
    //FFormerMarginsChangedHandler: TNotifyEvent; // 16 bytes
    FControlAbsolutePosAtMouseDown: TALPointD; // 8 bytes
    FScale: Single; // 4 bytes
    //FFocusOnMouseDown: Boolean; // 1 byte
    //FFocusOnMouseUp: Boolean; // 1 byte
    FMouseDownAtLowVelocity: Boolean; // 1 byte
    //FDisableDoubleClickHandling: Boolean; // 1 byte
    FIsPixelAlignmentEnabled: Boolean; // 1 byte
    //FAlign: TALAlignLayout; // 1 byte
    FIsSetBoundsLocked: Boolean; // 1 byte
    //function GetPivot: TPosition;
    //procedure SetPivot(const Value: TPosition);
    function GetPressed: Boolean;
    procedure SetPressed(const AValue: Boolean);
    //procedure DelayOnResize(Sender: TObject);
    //procedure DelayOnResized(Sender: TObject);
    //procedure MarginsChangedHandler(Sender: TObject);
    function IsScaledStored: Boolean;
  protected
    FTextUpdating: Boolean; // 1 byte
    FAutoSize: Boolean; // 1 byte
    FIsAdjustingSize: Boolean; // 1 byte
    FAdjustSizeOnEndUpdate: Boolean; // 1 byte
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure SetScale(const AValue: Single); virtual;
    property Scale: Single read FScale write SetScale stored IsScaledStored nodefault;
    //property Pivot: TPosition read GetPivot write SetPivot;
    function GetAutoSize: Boolean; virtual;
    procedure SetAutoSize(const Value: Boolean); virtual;
    // Dynamically adjusts the dimensions to accommodate child controls,
    // considering their sizes, positions, margins, and alignments.
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    function GetIsPixelAlignmentEnabled: Boolean; virtual;
    procedure SetIsPixelAlignmentEnabled(const AValue: Boolean); Virtual;
    //property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;
    //property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;
    //procedure DoEnter; override;
    //procedure DoExit; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    //function GetParentedVisible: Boolean; override;
    //procedure DoMatrixChanged(Sender: TObject); override;
    //procedure DoRootChanged; override;
    procedure IsMouseOverChanged; virtual;
    //procedure IsFocusedChanged; virtual;
    procedure PressedChanged; virtual;
    //Procedure MarginsChanged; virtual;
    procedure PaddingChanged; override;
    //procedure ParentChanged; override;
    //procedure Loaded; override;
    //function IsOwnerLoading: Boolean;
    //function IsSizeStored: Boolean; override;
    //function GetAlign: TALAlignLayout; Reintroduce;
    //procedure SetAlign(const Value: TALAlignLayout); Reintroduce; virtual;
    procedure DoEndUpdate; override;
    procedure DoResized; override;
    procedure DoRealign; override;
    procedure AdjustSize; virtual;
    procedure BeginTextUpdate; override;
    procedure EndTextUpdate; override;
    procedure SetFixedSizeBounds(X, Y, AWidth, AHeight: Single); Virtual;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure EndUpdate; override;
    //procedure SetNewScene(AScene: IScene); override;
    function IsVisibleWithinFormBounds: Boolean;
    //property Form: TCommonCustomForm read FForm;
    //property DisableDoubleClickHandling: Boolean read FDisableDoubleClickHandling write FDisableDoubleClickHandling;
    {$IFNDEF ALCompilerVersionSupported122}
      {$MESSAGE WARN 'Check if property FMX.Controls.TControl.Pressed still not fire a PressChanged event when it gets updated, and adjust the IFDEF'}
    {$ENDIF}
    property Pressed: Boolean read GetPressed write SetPressed;
    procedure AlignToPixel; virtual;
    procedure SetBounds(X, Y, AWidth, AHeight: Double); override;
    function HasUnconstrainedAutosizeX: Boolean; override;
    function HasUnconstrainedAutosizeY: Boolean; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default False;
    property IsPixelAlignmentEnabled: Boolean read GetIsPixelAlignmentEnabled write SetIsPixelAlignmentEnabled;
    //property Align: TALAlignLayout read FAlign write SetAlign default TALAlignLayout.None;
    //property OwnerControl: TALDynamicListBoxControl read FOwnerControl;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxShape = class(TALDynamicListBoxExtendedControl)
  strict private
    FFill: TALBrush;
    FStroke: TALStrokeBrush;
    fShadow: TALShadow;
    function GetFill: TALBrush;
    procedure SetFill(const Value: TALBrush);
    function GetStroke: TALStrokeBrush;
    procedure SetStroke(const Value: TALStrokeBrush);
    function GetShadow: TALShadow;
    procedure SetShadow(const Value: TALShadow);
  protected
    function CreateFill: TALBrush; virtual;
    function CreateStroke: TALStrokeBrush; virtual;
    function CreateShadow: TALShadow; virtual;
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure ShadowChanged(Sender: TObject); virtual;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    property Fill: TALBrush read GetFill write SetFill;
    property Stroke: TALStrokeBrush read GetStroke write SetStroke;
    property Shadow: TALShadow read GetShadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxImage = class(TALDynamicListBoxExtendedControl)
  public
    type
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      TCropCenter = class(TALPosition)
      protected
        function GetDefaultValue: TPointF; override;
      end;
  protected
    type
      TResourceDownloadContext = Class(TObject)
      private
        Lock: TObject;
        FreeByThread: Boolean;
      public
        Owner: TALDynamicListBoxImage;
        Rect: TRectF;
        Scale: Single;
        AlignToPixel: Boolean;
        Color: TAlphaColor;
        ResourceName: String;
        ResourceStream: TStream;
        MaskResourceName: String;
        MaskBitmap: TALBitmap;
        WrapMode: TALImageWrapMode;
        CropCenter: TpointF;
        RotateAccordingToExifOrientation: Boolean;
        StrokeColor: TAlphaColor;
        StrokeThickness: Single;
        ShadowBlur: Single;
        ShadowOffsetX: Single;
        ShadowOffsetY: Single;
        ShadowColor: TAlphaColor;
        Corners: TCorners;
        Sides: TSides;
        XRadius: Single;
        YRadius: Single;
        BlurRadius: single;
        constructor Create(const AOwner: TALDynamicListBoxImage); virtual;
        destructor Destroy; override;
      End;
  private
    FBackgroundColor: TAlphaColor; // 4 bytes
    FLoadingColor: TAlphaColor; // 4 bytes
    fResourceName: String; // 8 bytes
    FMaskResourceName: String; // 8 bytes
    FMaskBitmap: TALBitmap; // 8 bytes
    FWrapMode: TALImageWrapMode; // 1 bytes
    fExifOrientationInfo: TalExifOrientationInfo; // 1 bytes
    fRotateAccordingToExifOrientation: Boolean; // 1 bytes
    FCorners: TCorners; // 1 bytes
    FSides: TSides; // 1 bytes
    FXRadius: Single; // 4 bytes
    FYRadius: Single; // 4 bytes
    FBlurRadius: single; // 4 bytes
    FCacheIndex: Integer; // 4 bytes
    FLoadingCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    FCropCenter: TALPosition; // 8 bytes
    FStroke: TALStrokeBrush; // 8 bytes
    fShadow: TALShadow; // 8 bytes
    FResourceDownloadContext: TResourceDownloadContext; // [MultiThread] | 8 bytes
    function GetCropCenter: TALPosition;
    procedure SetCropCenter(const Value: TALPosition);
    function GetStroke: TALStrokeBrush;
    procedure SetStroke(const Value: TALStrokeBrush);
    function GetShadow: TALShadow;
    procedure SetShadow(const Value: TALShadow);
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure SetRotateAccordingToExifOrientation(const Value: Boolean);
    procedure setResourceName(const Value: String);
    procedure setMaskResourceName(const Value: String);
    procedure setMaskBitmap(const Value: TALBitmap);
    procedure setBackgroundColor(const Value: TAlphaColor);
    procedure setLoadingColor(const Value: TAlphaColor);
    function IsBackgroundColorStored: Boolean;
    function IsLoadingColorStored: Boolean;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
    function IsBlurRadiusStored: Boolean;
  protected
    fBufDrawable: TALDrawable; // 8 bytes
    fBufDrawableRect: TRectF; // 16 bytes
    function CreateCropCenter: TALPosition; virtual;
    function CreateStroke: TALStrokeBrush; virtual;
    function CreateShadow: TALShadow; virtual;
    function GetCacheSubIndex: Integer; virtual;
    function GetLoadingCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    function GetDefaultBackgroundColor: TalphaColor; virtual;
    function GetDefaultLoadingColor: TalphaColor; virtual;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
    function GetDefaultBlurRadius: Single; virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetBlurRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure CropCenterChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure ShadowChanged(Sender: TObject); virtual;
    procedure CancelResourceDownload;
    class function CanStartResourceDownload(var AContext: Tobject): boolean; virtual; // [MultiThread]
    class procedure HandleResourceDownloadSuccess(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject); virtual; // [MultiThread]
    class procedure HandleResourceDownloadError(const AErrMessage: string; var AContext: Tobject); virtual; // [MultiThread]
    class function GetResourceDownloadPriority(const AContext: Tobject): Int64; virtual; // [MultiThread]
    class Procedure CreateBufDrawable(var AContext: TObject); overload; virtual; // [MultiThread]
    class Procedure CreateBufDrawable(
                      var ABufDrawable: TALDrawable;
                      out ABufDrawableRect: TRectF;
                      out AExifOrientationInfo: TalExifOrientationInfo;
                      const ARect: TRectF;
                      const AScale: Single;
                      const AAlignToPixel: Boolean;
                      const AColor: TAlphaColor;
                      const AResourceName: String;
                      const AResourceStream: TStream;
                      const AMaskResourceName: String;
                      const AMaskBitmap: TALBitmap;
                      const AWrapMode: TALImageWrapMode;
                      const ACropCenter: TpointF;
                      const ARotateAccordingToExifOrientation: Boolean;
                      const AStrokeColor: TAlphaColor;
                      const AStrokeThickness: Single;
                      const AShadowBlur: Single;
                      const AShadowOffsetX: Single;
                      const AShadowOffsetY: Single;
                      const AShadowColor: TAlphaColor;
                      const ACorners: TCorners;
                      const ASides: TSides;
                      const AXRadius: Single;
                      const AYRadius: Single;
                      const ABlurRadius: Single); overload; virtual; // [MultiThread]
    procedure Paint; override;
    procedure DoResized; override;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DefaultBackgroundColor: TAlphaColor read GetDefaultBackgroundColor;
    property DefaultLoadingColor: TAlphaColor read GetDefaultLoadingColor;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
    property DefaultBlurRadius: Single read GetDefaultBlurRadius;
    // MaskBitmap will not be owned and will not be freed with the TALDynamicListBoxImage
    property MaskBitmap: TALBitmap read fMaskBitmap write setMaskBitmap;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    property LoadingCacheIndex: Integer read FLoadingCacheIndex write FLoadingCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    //property Action;
    property Align;
    //property Anchors;
    //property AutoSize;
    property BackgroundColor: TAlphaColor read fBackgroundColor write setBackgroundColor Stored IsBackgroundColorStored;
    property LoadingColor: TAlphaColor read FLoadingColor write setLoadingColor Stored IsLoadingColorStored;
    property BlurRadius: Single read FBlurRadius write SetBlurRadius stored IsBlurRadiusStored nodefault;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    // CropCenter is use when wrapmode = FitIntoAndCrop. It's define the center of
    // crop in the source image (ex: center the result on a face instead
    // of the middle of the bounds). If CropCenter contain negative value then it's
    // indicate percentage
    property CropCenter: TALPosition read GetCropCenter write SetCropCenter;
    property Cursor;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    //property Locked;
    property Margins;
    property MaskResourceName: String read fMaskResourceName write setMaskResourceName;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    // If a file extension (e.g., .png) is detected in ResourceName, the image is loaded from the
    // specified file (With the full path of the file obtained using ALGetResourceFilename).
    // If ResourceName is a URL, the image is downloaded in the background from the internet.
    // In debug mode, the image is loaded from a file located in the /Resources/ sub-folder of the
    // project directory (with the extensions .png or .jpg).
    property ResourceName: String read fResourceName write setResourceName;
    property RotateAccordingToExifOrientation: Boolean read fRotateAccordingToExifOrientation write SetRotateAccordingToExifOrientation default false;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow: TALShadow read GetShadow write SetShadow;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    //property Size;
    property Stroke: TALStrokeBrush read GetStroke write SetStroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode default TALImageWrapMode.Fit;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxBaseRectangle = class(TALDynamicListBoxShape)
  private
    fDoubleBuffered: boolean;
    FXRadius: Single;
    FYRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function HasCustomDraw: Boolean; virtual;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    function IsSimpleRenderPossible: Boolean;
    procedure Paint; override;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                const AScale: Single;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStateLayerContentColor: TAlphaColor;
                const ADrawStateLayerOnTop: Boolean;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow); virtual;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    procedure DoResized; override;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DoubleBuffered default true;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxRectangle = class(TALDynamicListBoxBaseRectangle)
  public
    property CacheEngine;
    property CacheIndex;
  public
    //property Action;
    property Align;
    //property Anchors;
    property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor;
    property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    //property Locked;
    property Margins;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    property Sides;
    //property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxCircle = class(TALDynamicListBoxShape)
  private
    fDoubleBuffered: boolean;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure Paint; override;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                const AScale: Single;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStateLayerContentColor: TAlphaColor;
                const ADrawStateLayerOnTop: Boolean;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow); virtual;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    procedure DoResized; override;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    function PointInObjectLocal(X, Y: Double): Boolean; override;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    //property Action;
    property Align;
    //property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered default true;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    //property Locked;
    property Margins;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    //property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxLine = class(TALDynamicListBoxShape)
  private
    fDoubleBuffered: boolean;
    FLineType: TALLineType;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    procedure SetLineType(const Value: TALLineType);
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure DoResized; override;
  public
    constructor Create(const AOwner: TObject); override;
    procedure Paint; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    //property Action;
    property Align;
    //property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered default true;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property LineType: TALLineType read FLineType write SetLineType;
    //property Locked;
    property Margins;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    //property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxBaseText = class(TALDynamicListBoxShape)
  public
    type
      TFill = class(TALBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
  public
    type
      TElementMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single; const Element: TALTextElement) of object;
      TElementMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Single; const Element: TALTextElement) of object;
      TElementNotifyEvent = procedure(Sender: TObject; const Element: TALTextElement) of object;
  private
    fDoubleBuffered: boolean;
    FMultiLineTextOptions: TALMultiLineTextOptions;
    FOnElementClick: TElementNotifyEvent;
    FOnElementMouseDown: TElementMouseEvent;
    FOnElementMouseMove: TElementMouseMoveEvent;
    FOnElementMouseUp: TElementMouseEvent;
    FOnElementMouseEnter: TElementNotifyEvent;
    FOnElementMouseLeave: TElementNotifyEvent;
    FHoveredElement: TALTextElement;
    FPressedElement: TALTextElement;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
    fTextBroken: Boolean;
    fAllTextDrawn: Boolean;
    fElements: TALTextElements;
    FAutoTranslate: Boolean;
    FText: String;
    FTextSettings: TALBaseTextSettings;
    FMaxWidth: Single;
    FMaxHeight: Single;
    FYRadius: Single;
    FXRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    procedure SetText(const Value: string);
    procedure SetMaxWidth(const Value: Single);
    procedure SetMaxHeight(const Value: Single);
    function IsMaxWidthStored: Boolean;
    function IsMaxHeightStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure SetAlign(const Value: TALAlignLayout); override;
    procedure SetAutoSize(const Value: Boolean); override;
    function GetElementAtPos(const APos: TPointF): TALTextElement;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Click; override;
    procedure PaddingChanged; override;
    procedure TextSettingsChanged(Sender: TObject); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure SetTextSettings(const Value: TALBaseTextSettings); virtual;
    function CreateTextSettings: TALBaseTextSettings; virtual; abstract;
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    procedure Paint; override;
    //procedure Loaded; override;
    procedure DoResized; override;
    procedure AdjustSize; override;
    procedure BeginTextUpdate; override;
    procedure EndTextUpdate; override;
    function GetMultiLineTextOptions(
               const AScale: Single;
               const AOpacity: Single;
               const AFont: TALFont;
               const ADecoration: TALTextDecoration;
               const AEllipsisFont: TALFont;
               const AEllipsisDecoration: TALTextDecoration;
               const AFill: TALBrush;
               const AStateLayer: TALStateLayer;
               const AStroke: TALStrokeBrush;
               const AShadow: TALShadow): TALMultiLineTextOptions;
    Procedure DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF); virtual;
    Procedure DrawMultilineTextBeforeDrawBackground(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF); virtual;
    Procedure DrawMultilineTextBeforeDrawParagraph(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF); virtual;
    Procedure DrawMultilineText(
                const ACanvas: TALCanvas;
                var ARect: TRectF;
                out ATextBroken: Boolean;
                out AAllTextDrawn: Boolean;
                out AElements: TALTextElements;
                const AScale: Single;
                const AOpacity: Single;
                const AText: String;
                const AFont: TALFont;
                const ADecoration: TALTextDecoration;
                const AEllipsisFont: TALFont;
                const AEllipsisDecoration: TALTextDecoration;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow);
    Procedure MeasureMultilineText(
                out ARect: TRectF;
                out ATextBroken: Boolean;
                out AAllTextDrawn: Boolean;
                out AElements: TALTextElements;
                const AScale: Single;
                const AText: String;
                const AFont: TALFont;
                const ADecoration: TALTextDecoration;
                const AEllipsisFont: TALFont;
                const AEllipsisDecoration: TALTextDecoration;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow);
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                out ATextBroken: Boolean;
                out AAllTextDrawn: Boolean;
                out AElements: TALTextElements;
                const AScale: Single;
                const AText: String;
                const AFont: TALFont;
                const ADecoration: TALTextDecoration;
                const AEllipsisFont: TALFont;
                const AEllipsisDecoration: TALTextDecoration;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStroke: TALStrokeBrush;
                const AShadow: TALShadow);
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    property Elements: TALTextElements read fElements;
    property OnElementClick: TElementNotifyEvent read FOnElementClick write FOnElementClick;
    property OnElementMouseDown: TElementMouseEvent read FOnElementMouseDown write FOnElementMouseDown;
    property OnElementMouseMove: TElementMouseMoveEvent read FOnElementMouseMove write FOnElementMouseMove;
    property OnElementMouseUp: TElementMouseEvent read FOnElementMouseUp write FOnElementMouseUp;
    property OnElementMouseEnter: TElementNotifyEvent read FOnElementMouseEnter write FOnElementMouseEnter;
    property OnElementMouseLeave: TElementNotifyEvent read FOnElementMouseLeave write FOnElementMouseLeave;
    property TextSettings: TALBaseTextSettings read fTextSettings write SetTextSettings;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    function TextBroken: Boolean;
    property AutoSize;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate default true;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property HitTest default False;
    property MaxWidth: Single read fMaxWidth write SetMaxWidth stored IsMaxWidthStored nodefault;
    property MaxHeight: Single read fMaxHeight write SetMaxHeight stored IsMaxHeightStored nodefault;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property Text: string read FText write SetText;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property DoubleBuffered default true;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxText = class(TALDynamicListBoxBaseText)
  private
    function GetTextSettings: TALTextSettings;
  protected
    procedure SetTextSettings(const Value: TALTextSettings); reintroduce;
    function CreateTextSettings: TALBaseTextSettings; override;
  //{$IF defined(ALBackwardCompatible)}
  //private
  //  procedure ReadTextIsHtml(Reader: TReader);
  //  procedure ReadLineSpacing(Reader: TReader);
  //protected
  //  procedure DefineProperties(Filer: TFiler); override;
  //{$ENDIF}
  public
    property CacheEngine;
    property CacheIndex;
    property Elements;
  public
    //property Action;
    property Align;
    //property Anchors;
    property AutoSize;
    property AutoTranslate;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor;
    property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    //property Locked;
    property Margins;
    property MaxWidth;
    property MaxHeight;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    property Sides;
    //property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property Text;
    property TextSettings: TALTextSettings read GetTextSettings write SetTextSettings;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    property OnElementClick;
    property OnElementMouseDown;
    property OnElementMouseMove;
    property OnElementMouseUp;
    property OnElementMouseEnter;
    property OnElementMouseLeave;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxBaseStateStyle = class(TALPersistentObserver)
  private
    FParent: Tobject; // 8 bytes
    FStateStyleParent: TALDynamicListBoxBaseStateStyle; // 8 bytes
    FControlParent: TALDynamicListBoxControl; // 8 bytes
    FFill: TALInheritBrush; // 8 bytes
    FStateLayer: TALStateLayer; // 8 bytes
    FStroke: TALInheritStrokeBrush; // 8 bytes
    FShadow: TALInheritShadow; // 8 bytes
    FScale: Single; // 4 bytes
    fSuperseded: Boolean; // 1 byte
    procedure SetFill(const AValue: TALInheritBrush);
    procedure SetStateLayer(const AValue: TALStateLayer);
    procedure SetStroke(const AValue: TALInheritStrokeBrush);
    procedure SetShadow(const AValue: TALInheritShadow);
    procedure SetScale(const Value: Single);
    procedure FillChanged(ASender: TObject);
    procedure StateLayerChanged(ASender: TObject);
    procedure StrokeChanged(ASender: TObject);
    procedure ShadowChanged(ASender: TObject);
    function IsScaleStored: Boolean;
  protected
    FBufDrawable: TALDrawable; // 8 bytes
    FBufDrawableRect: TRectF; // 16 bytes
    function CreateSavedState: TALPersistentObserver; override;
    function CreateFill(const AParent: TALBrush): TALInheritBrush; virtual;
    function CreateStateLayer: TALStateLayer; virtual;
    function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; virtual;
    function CreateShadow(const AParent: TALShadow): TALInheritShadow; virtual;
    function GetDefaultScale: Single; virtual;
    function GetInherit: Boolean; virtual;
    function GetCacheSubIndex: Integer; Virtual;
    procedure DoSupersede; virtual;
    property Fill: TALInheritBrush read FFill write SetFill;
    property Shadow: TALInheritShadow read FShadow write SetShadow;
    property StateLayer: TALStateLayer read FStateLayer write SetStateLayer;
    property Stroke: TALInheritStrokeBrush read FStroke write SetStroke;
    property Scale: Single read FScale write SetScale stored IsScaleStored nodefault;
  public
    constructor Create(const AParent: TObject); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure ClearBufDrawable; virtual;
    Property Inherit: Boolean read GetInherit;
    procedure Interpolate(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single);
    procedure Supersede(Const ASaveState: Boolean = False); virtual;
    procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
    property Superseded: Boolean read FSuperseded;
    property Parent: TObject read FParent;
    property StateStyleParent: TALDynamicListBoxBaseStateStyle read FStateStyleParent;
    property ControlParent: TALDynamicListBoxControl read FControlParent;
    property DefaultScale: Single read GetDefaultScale;
    property CacheSubIndex: integer read GetCacheSubIndex;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxBaseStateStyles = class(TALPersistentObserver)
  private
    FParent: TALDynamicListBoxControl; // 8 bytes
    FTransition: TALStateTransition; // 8 bytes
    FTransitionAnimation: TALfloatAnimation; // 8 bytes
    FTransitionFrom: TALDynamicListBoxBaseStateStyle; // 8 bytes
    FTransitionTo: TALDynamicListBoxBaseStateStyle; // 8 bytes
    FTransitionClickDelayed: Boolean; // 1 byte
    FLastPaintedRawStyle: TALDynamicListBoxBaseStateStyle; // 8 bytes
    FCurrentAdjustedStyle: TALDynamicListBoxBaseStateStyle; // 8 bytes
    procedure SetTransition(const Value: TALStateTransition);
    procedure TransitionChanged(ASender: TObject);
  protected
    function CreateSavedState: TALPersistentObserver; override;
    function CreateTransition: TALStateTransition; virtual;
    procedure StartTransition; virtual;
    procedure TransitionAnimationProcess(Sender: TObject); virtual;
    procedure TransitionAnimationFinish(Sender: TObject); virtual;
    property Transition: TALStateTransition read FTransition write SetTransition;
    property TransitionClickDelayed: Boolean read FTransitionClickDelayed write FTransitionClickDelayed;
  public
    constructor Create(const AParent: TALDynamicListBoxControl); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure ClearBufDrawable; virtual;
    /// <summary>
    ///   Determines and returns the current raw state style of the control
    ///   based on its current state, such as Disabled, Pressed, Focused, or
    ///   Hovered. This function does not apply any adjustments, animations,
    ///   or transitions.
    /// </summary>
    function GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle; virtual;
    /// <summary>
    ///   Determines and returns the current state style of the control,
    ///   applying any necessary adjustments and handling state transition
    ///   animations. This function provides the fully adjusted style that
    ///   reflects the control's current visual appearance, including
    ///   interpolations during transitions.
    /// </summary>
    function GetCurrentAdjustedStyle: TALDynamicListBoxBaseStateStyle; virtual;
    function IsTransitionAnimationRunning: Boolean; virtual;
    property TransitionFrom: TALDynamicListBoxBaseStateStyle read FTransitionFrom;
    property TransitionTo: TALDynamicListBoxBaseStateStyle read FTransitionTo;
    procedure UpdateLastPaintedRawStyle; virtual;
    Property Parent: TALDynamicListBoxControl read FParent;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxBaseCheckBox = class(TALDynamicListBoxShape)
  public
    type
      // ---------------
      // TCheckMarkBrush
      TCheckMarkBrush = class(TALPersistentObserver)
      public
        Type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      private
        FColor: TAlphaColor;
        FResourceName: String;
        FWrapMode: TALImageWrapMode;
        FThickness: Single;
        FMargins: TALBounds;
        procedure SetColor(const Value: TAlphaColor);
        procedure SetResourceName(const Value: String);
        procedure SetWrapMode(const Value: TALImageWrapMode);
        procedure SetThickness(const Value: Single);
        procedure SetMargins(const Value: TALBounds);
        procedure MarginsChanged(Sender: TObject); virtual;
        function IsColorStored: Boolean;
        function IsResourceNameStored: Boolean;
        function IsWrapModeStored: Boolean;
        function IsThicknessStored: Boolean;
      protected
        function CreateMargins: TALBounds; virtual;
        function GetDefaultColor: TAlphaColor; virtual;
        function GetDefaultResourceName: String; virtual;
        function GetDefaultWrapMode: TALImageWrapMode; virtual;
        function GetDefaultThickness: Single; virtual;
      public
        constructor Create; override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; virtual;
        procedure Interpolate(const ATo: TCheckMarkBrush; const ANormalizedTime: Single); virtual;
        procedure InterpolateNoChanges(const ATo: TCheckMarkBrush; const ANormalizedTime: Single);
        function HasCheckMark: boolean;
        property DefaultColor: TAlphaColor read GetDefaultColor;
        property DefaultResourceName: String read GetDefaultResourceName;
        property DefaultWrapMode: TALImageWrapMode read GetDefaultWrapMode;
        property DefaultThickness: Single read GetDefaultThickness;
      public
        property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
        property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored nodefault;
        property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
        property Thickness: Single read FThickness write SetThickness stored IsThicknessStored nodefault;
        property Margins: TALBounds read FMargins write SetMargins;
      end;
      // ----------------------
      // TInheritCheckMarkBrush
      TInheritCheckMarkBrush = class(TCheckMarkBrush)
      private
        FParent: TCheckMarkBrush;
        FInherit: Boolean;
        fSuperseded: Boolean;
        procedure SetInherit(const AValue: Boolean);
      protected
        function CreateSavedState: TALPersistentObserver; override;
        procedure DoSupersede; virtual;
      public
        constructor Create(const AParent: TCheckMarkBrush); reintroduce; virtual;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure Supersede(Const ASaveState: Boolean = False); virtual;
        procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
        property Superseded: Boolean read FSuperseded;
        property Parent: TCheckMarkBrush read FParent;
      public
        property Inherit: Boolean read FInherit write SetInherit Default True;
      end;
      // ---------------
      // TBaseStateStyle
      TBaseStateStyle = class(TALDynamicListBoxBaseStateStyle)
      public
        type
          TStateLayer = class(TALStateLayer)
          public
            Type
              TMargins = class(TALBounds)
              protected
                function GetDefaultValue: TRectF; override;
              end;
          protected
            function CreateMargins: TALBounds; override;
            function GetDefaultXRadius: Single; override;
            function GetDefaultYRadius: Single; override;
          end;
      private
        FCheckMark: TInheritCheckMarkBrush;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALDynamicListBoxBaseCheckBox;
        procedure SetCheckMark(const AValue: TInheritCheckMarkBrush);
        procedure CheckMarkChanged(ASender: TObject);
      protected
        function CreateStateLayer: TALStateLayer; override;
        function CreateCheckMark(const AParent: TCheckMarkBrush): TInheritCheckMarkBrush; virtual;
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure Interpolate(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALDynamicListBoxBaseCheckBox read GetControlParent;
      public
        property CheckMark: TInheritCheckMarkBrush read FCheckMark write SetCheckMark;
        property Fill;
        property Scale;
        property Shadow;
        property Stroke;
      end;
      // ------------------
      // TDefaultStateStyle
      TDefaultStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TBaseStateStyle)
      private
        FOpacity: Single;
        procedure SetOpacity(const Value: Single);
        function IsOpacityStored: Boolean;
      protected
        function GetInherit: Boolean; override;
        function GetCacheSubIndex: Integer; override;
      public
        constructor Create(const AParent: TObject); override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
      public
        property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      public
        property StateLayer;
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      public
        property StateLayer;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      public
        property StateLayer;
      end;
      // -----------------
      // TCheckStateStyles
      TCheckStateStyles = class(TALPersistentObserver)
      private
        FDefault: TDefaultStateStyle;
        FDisabled: TDisabledStateStyle;
        FHovered: THoveredStateStyle;
        FPressed: TPressedStateStyle;
        FFocused: TFocusedStateStyle;
        procedure SetDefault(const AValue: TDefaultStateStyle);
        procedure SetDisabled(const AValue: TDisabledStateStyle);
        procedure SetHovered(const AValue: THoveredStateStyle);
        procedure SetPressed(const AValue: TPressedStateStyle);
        procedure SetFocused(const AValue: TFocusedStateStyle);
        procedure DefaultChanged(ASender: TObject);
        procedure DisabledChanged(ASender: TObject);
        procedure HoveredChanged(ASender: TObject);
        procedure PressedChanged(ASender: TObject);
        procedure FocusedChanged(ASender: TObject);
      protected
        function CreateSavedState: TALPersistentObserver; override;
        function CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle; virtual;
        function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
        function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
        function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
        function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
      public
        constructor Create(const AParent: TALDynamicListBoxControl); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; virtual;
        procedure ClearBufDrawable; virtual;
      public
        property &Default: TDefaultStateStyle read FDefault write SetDefault;
        property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
        property Hovered: THoveredStateStyle read FHovered write SetHovered;
        property Pressed: TPressedStateStyle read FPressed write SetPressed;
        property Focused: TFocusedStateStyle read FFocused write SetFocused;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALDynamicListBoxBaseStateStyles)
      private
        FChecked: TCheckStateStyles;
        FUnchecked: TCheckStateStyles;
        function GetParent: TALDynamicListBoxBaseCheckBox;
        procedure SetChecked(const AValue: TCheckStateStyles);
        procedure SetUnchecked(const AValue: TCheckStateStyles);
        procedure CheckedChanged(ASender: TObject);
        procedure UncheckedChanged(ASender: TObject);
      protected
        function CreateCheckedStateStyles(const AParent: TALDynamicListBoxControl): TCheckStateStyles; virtual;
        function CreateUncheckedStateStyles(const AParent: TALDynamicListBoxControl): TCheckStateStyles; virtual;
      public
        constructor Create(const AParent: TALDynamicListBoxControl); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ClearBufDrawable; override;
        function GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle; override;
        Property Parent: TALDynamicListBoxBaseCheckBox read GetParent;
      public
        property Checked: TCheckStateStyles read FChecked write SetChecked;
        property Unchecked: TCheckStateStyles read FUnchecked write SetUnchecked;
      end;
  private
    FStateStyles: TStateStyles;
    FCheckMark: TCheckMarkBrush;
    FChecked: Boolean;
    FDoubleBuffered: boolean;
    FXRadius: Single;
    FYRadius: Single;
    FCacheIndex: Integer; // 4 bytes
    FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
    {$IF NOT DEFINED(ALSkiaCanvas)}
    FRenderTargetSurface: TALSurface; // 8 bytes
    FRenderTargetCanvas: TALCanvas; // 8 bytes
    fRenderTargetDrawable: TALDrawable; // 8 bytes
    {$ENDIF}
    FOnChange: TNotifyEvent;
    procedure SetCheckMark(const Value: TCheckMarkBrush);
    procedure SetStateStyles(const AValue: TStateStyles);
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    function CreateCheckMark: TCheckMarkBrush; virtual;
    function CreateStateStyles: TStateStyles; virtual;
    function GetCacheSubIndex: Integer; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure CheckMarkChanged(Sender: TObject); virtual;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure IsMouseOverChanged; override;
    //procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    function GetDefaultSize: TSizeF; override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    //procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure Click; override;
    procedure DoChanged; virtual;
    procedure DoResized; override;
    procedure DrawCheckMark(
                const ACanvas: TALCanvas;
                const AScale: Single;
                const ADstRect: TrectF;
                const AChecked: Boolean;
                const ACheckMark: TCheckMarkBrush); virtual;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                out ABufDrawableRect: TRectF;
                const AScale: Single;
                const AFill: TALBrush;
                const AStateLayer: TALStateLayer;
                const AStroke: TALStrokeBrush;
                const ACheckMark: TCheckMarkBrush;
                const AShadow: TALShadow); virtual;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
    procedure InitRenderTargets(var ARect: TrectF); virtual;
    procedure ClearRenderTargets; virtual;
    Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
    Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
    Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
    {$ENDIF}
    procedure Paint; override;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read FCacheIndex write FCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    //property CanFocus default True;
    property Cursor default crHandPoint;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property CheckMark: TCheckMarkBrush read FCheckMark write SetCheckMark;
    property DoubleBuffered default true;
    property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxCheckBox = class(TALDynamicListBoxBaseCheckBox)
  public
    type
      // ------------
      // TStateStyles
      TStateStyles = class(TALDynamicListBoxBaseCheckBox.TStateStyles)
      public
        property Transition;
      end;
  private
    function GetStateStyles: TStateStyles;
    procedure SetStateStyles(const AValue: TStateStyles);
  protected
    function CreateStateStyles: TALDynamicListBoxBaseCheckBox.TStateStyles; override;
  public
    property CacheEngine;
    property CacheIndex;
  public
    //property Action;
    property Align;
    //property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property CheckMark;
    property Checked;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    //property Locked;
    property Margins;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    //property Size;
    property StateStyles: TStateStyles read GetStateStyles write SetStateStyles;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    property OnChange;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxRadioButton = class(TALDynamicListBoxCheckBox)
  public
    type
      // ---------------
      // TCheckMarkBrush
      TCheckMarkBrush = class(TALDynamicListBoxCheckBox.TCheckMarkBrush)
      public
        Type
          TMargins = class(TALDynamicListBoxCheckBox.TCheckMarkBrush.TMargins)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      protected
        function CreateMargins: TALBounds; override;
      end;
      // ----------------------
      // TInheritCheckMarkBrush
      TInheritCheckMarkBrush = class(TALDynamicListBoxCheckBox.TInheritCheckMarkBrush)
      public
        Type
          TMargins = class(TALDynamicListBoxCheckBox.TInheritCheckMarkBrush.TMargins)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      protected
        function CreateMargins: TALBounds; override;
      end;
      // ------------------
      // TDefaultStateStyle
      TDefaultStateStyle = class(TALDynamicListBoxCheckBox.TDefaultStateStyle)
      protected
        function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TALDynamicListBoxCheckBox.TDisabledStateStyle)
      protected
        function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TALDynamicListBoxCheckBox.THoveredStateStyle)
      protected
        function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TALDynamicListBoxCheckBox.TPressedStateStyle)
      protected
        function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TALDynamicListBoxCheckBox.TFocusedStateStyle)
      protected
        function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
      end;
      // -----------------
      // TCheckStateStyles
      TCheckStateStyles = class(TALDynamicListBoxCheckBox.TCheckStateStyles)
      protected
        function CreateDefaultStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TDefaultStateStyle; override;
        function CreateDisabledStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TDisabledStateStyle; override;
        function CreateHoveredStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.THoveredStateStyle; override;
        function CreatePressedStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TPressedStateStyle; override;
        function CreateFocusedStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TFocusedStateStyle; override;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALDynamicListBoxCheckBox.TStateStyles)
      protected
        function CreateCheckedStateStyles(const AParent: TALDynamicListBoxControl): TALDynamicListBoxBaseCheckBox.TCheckStateStyles; override;
        function CreateUncheckedStateStyles(const AParent: TALDynamicListBoxControl): TALDynamicListBoxBaseCheckBox.TCheckStateStyles; override;
      end;
  private
    FGroupName: string;
    fMandatory: boolean;
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GroupNameStored: Boolean;
    procedure GroupMessageCall(const Sender : TObject; const M : TMessage);
  protected
    function CreateCheckMark: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush; override;
    function CreateStateStyles: TALDynamicListBoxBaseCheckBox.TStateStyles; override;
    procedure SetChecked(const Value: Boolean); override;
    function GetDefaultXRadius: Single; override;
    function GetDefaultYRadius: Single; override;
    function GetDefaultSize: TSizeF; override;
    procedure DrawCheckMark(
                const ACanvas: TALCanvas;
                const AScale: Single;
                const ADstRect: TrectF;
                const AChecked: Boolean;
                const ACheckMark: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush); override;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
  public
    property GroupName: string read GetGroupName write SetGroupName stored GroupNameStored nodefault;
    property Mandatory: Boolean read fMandatory write fMandatory default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxSwitch = class(TALDynamicListBoxExtendedControl)
  public
    type
      // ------
      // TTrack
      TTrack = class(TALDynamicListBoxShape)
      public
        type
          // -----
          // TFill
          TFill = class(TALBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------
          // TStroke
          TStroke = class(TALStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // ---------------
          // TBaseStateStyle
          TBaseStateStyle = class(TALDynamicListBoxBaseStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          public
            property Fill;
            // When the track is scaled, the thumb is no longer aligned with the track.
            // Therefore, currently, scaling of the track is disabled.
            //property Scale;
            property Shadow;
            property Stroke;
          end;
          // ------------------
          // TDefaultStateStyle
          TDefaultStateStyle = class(TBaseStateStyle)
          protected
            function GetCacheSubIndex: Integer; override;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TBaseStateStyle)
          private
            FOpacity: Single;
            procedure SetOpacity(const Value: Single);
            function IsOpacityStored: Boolean;
          protected
            function GetInherit: Boolean; override;
            function GetCacheSubIndex: Integer; override;
          public
            constructor Create(const AParent: TObject); override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
          public
            property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
          end;
          // ------------------
          // THoveredStateStyle
          THoveredStateStyle = class(TBaseStateStyle)
          protected
            function GetCacheSubIndex: Integer; override;
          public
            property StateLayer;
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TBaseStateStyle)
          protected
            function GetCacheSubIndex: Integer; override;
          public
            property StateLayer;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TBaseStateStyle)
          protected
            function GetCacheSubIndex: Integer; override;
          public
            property StateLayer;
          end;
          // -----------------
          // TCheckStateStyles
          TCheckStateStyles = class(TALPersistentObserver)
          private
            FDefault: TDefaultStateStyle;
            FDisabled: TDisabledStateStyle;
            FHovered: THoveredStateStyle;
            FPressed: TPressedStateStyle;
            FFocused: TFocusedStateStyle;
            procedure SetDefault(const AValue: TDefaultStateStyle);
            procedure SetDisabled(const AValue: TDisabledStateStyle);
            procedure SetHovered(const AValue: THoveredStateStyle);
            procedure SetPressed(const AValue: TPressedStateStyle);
            procedure SetFocused(const AValue: TFocusedStateStyle);
            procedure DefaultChanged(ASender: TObject);
            procedure DisabledChanged(ASender: TObject);
            procedure HoveredChanged(ASender: TObject);
            procedure PressedChanged(ASender: TObject);
            procedure FocusedChanged(ASender: TObject);
          protected
            function CreateSavedState: TALPersistentObserver; override;
            function CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle; virtual;
            function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
            function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
            function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
            function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
          public
            constructor Create(const AParent: TALDynamicListBoxControl); reintroduce; virtual;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; virtual;
            procedure ClearBufDrawable; virtual;
          public
            property &Default: TDefaultStateStyle read FDefault write SetDefault;
            property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
            property Hovered: THoveredStateStyle read FHovered write SetHovered;
            property Pressed: TPressedStateStyle read FPressed write SetPressed;
            property Focused: TFocusedStateStyle read FFocused write SetFocused;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALDynamicListBoxBaseStateStyles)
          private
            FChecked: TCheckStateStyles;
            FUnchecked: TCheckStateStyles;
            function GetParent: TTrack;
            procedure SetChecked(const AValue: TCheckStateStyles);
            procedure SetUnchecked(const AValue: TCheckStateStyles);
            procedure CheckedChanged(ASender: TObject);
            procedure UncheckedChanged(ASender: TObject);
          protected
            function CreateCheckedStateStyles(const AParent: TALDynamicListBoxControl): TCheckStateStyles; virtual;
            function CreateUncheckedStateStyles(const AParent: TALDynamicListBoxControl): TCheckStateStyles; virtual;
          public
            constructor Create(const AParent: TALDynamicListBoxControl); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure ClearBufDrawable; override;
            function GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle; override;
            Property Parent: TTrack read GetParent;
          public
            property Checked: TCheckStateStyles read FChecked write SetChecked;
            property Unchecked: TCheckStateStyles read FUnchecked write SetUnchecked;
          end;
      private
        FStateStyles: TStateStyles;
        FChecked: Boolean;
        FDoubleBuffered: boolean;
        FXRadius: Single;
        FYRadius: Single;
        FCacheIndex: Integer; // 4 bytes
        FCacheEngine: TALBufDrawableCacheEngine; // 8 bytes
        {$IF NOT DEFINED(ALSkiaCanvas)}
        FRenderTargetSurface: TALSurface; // 8 bytes
        FRenderTargetCanvas: TALCanvas; // 8 bytes
        fRenderTargetDrawable: TALDrawable; // 8 bytes
        {$ENDIF}
        procedure SetStateStyles(const AValue: TStateStyles);
        function IsXRadiusStored: Boolean;
        function IsYRadiusStored: Boolean;
      protected
        function CreateFill: TALBrush; override;
        function CreateStroke: TALStrokeBrush; override;
        function CreateStateStyles: TStateStyles; virtual;
        function GetDefaultSize: TSizeF; override;
        function GetCacheSubIndex: Integer; virtual;
        function GetDoubleBuffered: boolean; override;
        procedure SetDoubleBuffered(const AValue: Boolean); override;
        function GetDefaultXRadius: Single; virtual;
        function GetDefaultYRadius: Single; virtual;
        procedure SetXRadius(const Value: Single); virtual;
        procedure SetYRadius(const Value: Single); virtual;
        procedure StateStylesChanged(Sender: TObject); virtual;
        procedure FillChanged(Sender: TObject); override;
        procedure StrokeChanged(Sender: TObject); override;
        procedure ShadowChanged(Sender: TObject); override;
        procedure IsMouseOverChanged; override;
        //procedure IsFocusedChanged; override;
        procedure PressedChanged; override;
        function GetChecked: Boolean; virtual;
        procedure SetChecked(const Value: Boolean); virtual;
        procedure DoChanged; virtual;
        procedure DoResized; override;
        Procedure CreateBufDrawable(
                    var ABufDrawable: TALDrawable;
                    out ABufDrawableRect: TRectF;
                    const AScale: Single;
                    const AFill: TALBrush;
                    const AStateLayer: TALStateLayer;
                    const AStroke: TALStrokeBrush;
                    const AShadow: TALShadow); virtual;
        {$IF NOT DEFINED(ALSkiaCanvas)}
        function GetRenderTargetRect(const ARect: TrectF): TRectF; virtual;
        procedure InitRenderTargets(var ARect: TrectF); virtual;
        procedure ClearRenderTargets; virtual;
        Property RenderTargetSurface: TALSurface read FRenderTargetSurface;
        Property RenderTargetCanvas: TALCanvas read FRenderTargetCanvas;
        Property RenderTargetDrawable: TALDrawable read fRenderTargetDrawable;
        {$ENDIF}
        procedure Paint; override;
        property Checked: Boolean read GetChecked write SetChecked default False;
        property CacheIndex: Integer read FCacheIndex write FCacheIndex;
        property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine write FCacheEngine;
      public
        constructor Create(const AOwner: TObject); override;
        destructor Destroy; override;
        procedure AlignToPixel; override;
        procedure MakeBufDrawable; override;
        procedure ClearBufDrawable; override;
        property DefaultXRadius: Single read GetDefaultXRadius;
        property DefaultYRadius: Single read GetDefaultYRadius;
        property DoubleBuffered default true;
        //property Position stored false;
      public
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        //property CanFocus default False;
        //property CanParentFocus;
        //property DisableFocusEffect;
        //property ClipChildren;
        //property ClipParent;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        //property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest default False;
        //property Locked default True;
        property Margins;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Shadow;
        //property Size;
        property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        //property TouchTargetExpansion;
        //property Visible;
        //property Width;
        property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
        property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
        //property OnCanFocus;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
      // ------
      // TThumb
      TThumb = class(TALDynamicListBoxBaseCheckBox)
      public
        Type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      protected
        function CreateMargins: TALBounds; override;
      public
        type
          // -------
          // TStroke
          TStroke = class(TALStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // --------------------
          // TCheckMarkBrush
          TCheckMarkBrush = class(TALDynamicListBoxBaseCheckBox.TCheckMarkBrush)
          public
            Type
              TMargins = class(TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.TMargins)
              protected
                function GetDefaultValue: TRectF; override;
              end;
          protected
            function CreateMargins: TALBounds; override;
          end;
          // ----------------------
          // TInheritCheckMarkBrush
          TInheritCheckMarkBrush = class(TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush)
          public
            Type
              TMargins = class(TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.TMargins)
              protected
                function GetDefaultValue: TRectF; override;
              end;
          protected
            function CreateMargins: TALBounds; override;
          end;
          // ------------------
          // TDefaultStateStyle
          TDefaultStateStyle = class(TALDynamicListBoxBaseCheckBox.TDefaultStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TALDynamicListBoxBaseCheckBox.TDisabledStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // ------------------
          // THoveredStateStyle
          THoveredStateStyle = class(TALDynamicListBoxBaseCheckBox.THoveredStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TALDynamicListBoxBaseCheckBox.TPressedStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TALDynamicListBoxBaseCheckBox.TFocusedStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush; override;
          end;
          // -----------------
          // TCheckStateStyles
          TCheckStateStyles = class(TALDynamicListBoxBaseCheckBox.TCheckStateStyles)
          protected
            function CreateDefaultStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TDefaultStateStyle; override;
            function CreateDisabledStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TDisabledStateStyle; override;
            function CreateHoveredStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.THoveredStateStyle; override;
            function CreatePressedStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TPressedStateStyle; override;
            function CreateFocusedStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TFocusedStateStyle; override;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALDynamicListBoxBaseCheckBox.TStateStyles)
          private
            FStartPositionX: Single;
          protected
            function CreateCheckedStateStyles(const AParent: TALDynamicListBoxControl): TALDynamicListBoxBaseCheckBox.TCheckStateStyles; override;
            function CreateUncheckedStateStyles(const AParent: TALDynamicListBoxControl): TALDynamicListBoxBaseCheckBox.TCheckStateStyles; override;
            procedure StartTransition; override;
            procedure TransitionAnimationProcess(Sender: TObject); override;
            procedure TransitionAnimationFinish(Sender: TObject); override;
          end;
      protected
        function CreateStroke: TALStrokeBrush; override;
        function CreateCheckMark: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush; override;
        function CreateStateStyles: TALDynamicListBoxBaseCheckBox.TStateStyles; override;
        function GetDefaultXRadius: Single; override;
        function GetDefaultYRadius: Single; override;
        function GetDefaultSize: TSizeF; override;
        procedure Click; override;
      public
        constructor Create(const AOwner: TObject); override;
        //property Position stored false;
      public
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        //property CanFocus default False;
        //property CanParentFocus;
        //property DisableFocusEffect;
        property CheckMark;
        //property Checked;
        //property ClipChildren;
        //property ClipParent;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        //property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest default False;
        //property Locked default True;
        property Margins;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Shadow;
        //property Size;
        property StateStyles;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        //property TouchTargetExpansion;
        //property Visible;
        property Width;
        property XRadius;
        property YRadius;
        //property OnCanFocus;
        //property OnChange;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
  private
    FThumb: TThumb;
    FTrack: TTrack;
    FTransition: TALStateTransition;
    FPressedThumbPos: TPointF;
    FOnChange: TNotifyEvent;
    fScrollCapturedByMe: boolean;
    procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
    procedure SetTransition(const Value: TALStateTransition);
    procedure TransitionChanged(ASender: TObject);
    function GetCacheIndex: integer;
    procedure SetCacheIndex(const AValue: Integer);
    function GetCacheEngine: TALBufDrawableCacheEngine;
    procedure SetCacheEngine(const AValue: TALBufDrawableCacheEngine);
    function GetMinThumbPos: Single;
    function GetMaxThumbPos: Single;
    procedure AlignThumb;
  protected
    function CreateTrack: TTrack; virtual;
    function CreateThumb: TThumb; virtual;
    function GetDefaultSize: TSizeF; override;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure StartTransition; virtual;
    procedure IsMouseOverChanged; override;
    //procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    procedure EnabledChanged; override;
    procedure DoChange;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseLeave; override;
    procedure Click; override;
    function GetChecked: boolean; virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    //procedure Loaded; override;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure AlignToPixel; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    // CacheIndex and CacheEngine are primarily used in TALDynamicListBox to
    // prevent duplicate drawables across multiple identical controls.
    // CacheIndex specifies the slot in the cache engine where an existing
    // drawable can be retrieved.
    property CacheIndex: Integer read GetCacheIndex write SetCacheIndex;
    // CacheEngine is not owned by the current control.
    property CacheEngine: TALBufDrawableCacheEngine read GetCacheEngine write SetCacheEngine;
  public
    //property Action;
    property Align;
    //property Anchors;
    //property AutoSize;
    //property CanFocus default true;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered default true;
    property Checked: Boolean read GetChecked write SetChecked default false;
    //property ClipChildren;
    //property ClipParent;
    property Cursor default crHandPoint;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    //property Locked;
    property Margins;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    //property Size;
    //property TabOrder;
    //property TabStop;
    property Thumb: TThumb read FThumb;
    property TouchTargetExpansion;
    property Track: TTrack read FTrack;
    property Transition: TALStateTransition read FTransition write SetTransition;
    property Visible;
    property Width;
    //property OnCanFocus;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxButton = class(TALDynamicListBoxBaseText)
  public
    Type
      TPadding = class(TALBounds)
      protected
        function GetDefaultValue: TRectF; override;
      end;
  protected
    function CreatePadding: TALBounds; override;
  public
    type
      // -----
      // TFill
      TFill = class(TALDynamicListBoxBaseText.TFill)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------
      // TStroke
      TStroke = class(TALDynamicListBoxBaseText.TStroke)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // -------------
      // TTextSettings
      TTextSettings = class(TALBaseTextSettings)
      public
        Type
          TFont = Class(TALFont)
          protected
            function GetDefaultWeight: TFontWeight; override;
          End;
      protected
        function CreateFont: TALFont; override;
        function GetDefaultHorzAlign: TALTextHorzAlign; override;
      public
        property Font;
        property Decoration;
        property Ellipsis;
        property MaxLines;
        property IsHtml;
        property Trimming;
        property HorzAlign;
        property VertAlign;
        property LineHeightMultiplier;
        property LetterSpacing;
      end;
      // ---------------
      // TBaseStateStyle
      TBaseStateStyle = class(TALDynamicListBoxBaseStateStyle)
      public
        type
          TFill = class(TALInheritBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          TStroke = class(TALInheritStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          TTextSettings = class(TALInheritBaseTextSettings)
          public
            Type
              TFont = Class(TALFont)
              protected
                function GetDefaultWeight: TFontWeight; override;
              End;
          protected
            function CreateFont: TALFont; override;
          public
            property Font;
            property Decoration;
          end;
      private
        FText: String;
        FTextSettings: TBaseStateStyle.TTextSettings;
        FPriorSupersedeText: String;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALDynamicListBoxButton;
        procedure SetText(const Value: string);
        procedure SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
        procedure TextSettingsChanged(ASender: TObject);
        function IsTextStored: Boolean;
      protected
        function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
        function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
        function CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings; virtual;
        function GetDefaultText: String; virtual;
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure Interpolate(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single); override;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALDynamicListBoxButton read GetControlParent;
        property DefaultText: String read GetDefaultText;
      public
        property Fill;
        property Scale;
        property Shadow;
        property Stroke;
        property Text: string read FText write SetText stored IsTextStored nodefault;
        property TextSettings: TBaseStateStyle.TTextSettings read fTextSettings write SetTextSettings;
      end;
      // -------------------
      // TDisabledStateStyle
      TDisabledStateStyle = class(TBaseStateStyle)
      private
        FOpacity: Single;
        procedure SetOpacity(const Value: Single);
        function IsOpacityStored: Boolean;
      protected
        function GetInherit: Boolean; override;
        function GetCacheSubIndex: Integer; override;
      public
        constructor Create(const AParent: TObject); override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
      public
        property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      public
        property StateLayer;
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      public
        property StateLayer;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      protected
        function GetCacheSubIndex: Integer; override;
      public
        property StateLayer;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALDynamicListBoxBaseStateStyles)
      private
        FDisabled: TDisabledStateStyle;
        FHovered: THoveredStateStyle;
        FPressed: TPressedStateStyle;
        FFocused: TFocusedStateStyle;
        function GetParent: TALDynamicListBoxButton;
        procedure SetDisabled(const AValue: TDisabledStateStyle);
        procedure SetHovered(const AValue: THoveredStateStyle);
        procedure SetPressed(const AValue: TPressedStateStyle);
        procedure SetFocused(const AValue: TFocusedStateStyle);
        procedure DisabledChanged(ASender: TObject);
        procedure HoveredChanged(ASender: TObject);
        procedure PressedChanged(ASender: TObject);
        procedure FocusedChanged(ASender: TObject);
      protected
        function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
        function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
        function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
        function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
      public
        constructor Create(const AParent: TALDynamicListBoxControl); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; override;
        procedure ClearBufDrawable; override;
        function GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle; override;
        Property Parent: TALDynamicListBoxButton read GetParent;
      public
        property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
        property Hovered: THoveredStateStyle read FHovered write SetHovered;
        property Pressed: TPressedStateStyle read FPressed write SetPressed;
        property Focused: TFocusedStateStyle read FFocused write SetFocused;
        property Transition;
      end;
  private
    {$IF defined(ALDPK)}
    FPrevStateStyles: TStateStyles;
    {$ENDIF}
    FStateStyles: TStateStyles;
    function GetTextSettings: TTextSettings;
    procedure SetStateStyles(const AValue: TStateStyles);
  protected
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    function CreateTextSettings: TALBaseTextSettings; override;
    function CreateStateStyles: TStateStyles; virtual;
    procedure SetTextSettings(const Value: TTextSettings); reintroduce;
    //procedure SetName(const Value: TComponentName); override;
    procedure TextSettingsChanged(Sender: TObject); override;
    procedure SetXRadius(const Value: Single); override;
    procedure SetYRadius(const Value: Single); override;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure IsMouseOverChanged; override;
    //procedure IsFocusedChanged; override;
    procedure PressedChanged; override;
    procedure Click; override;
    Procedure DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF); override;
    {$IF NOT DEFINED(ALSkiaCanvas)}
    function GetRenderTargetRect(const ARect: TrectF): TRectF; override;
    {$ENDIF}
    procedure Paint; override;
    //procedure Loaded; override;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property CacheEngine;
    property CacheIndex;
  public
    //property Action;
    property Align;
    //property Anchors;
    property AutoSize default True;
    property AutoTranslate;
    //property CanFocus default true;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor default crHandPoint;
    property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest default True;
    //property Locked;
    property Margins;
    property MaxWidth;
    property MaxHeight;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    property Sides;
    //property Size;
    property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property Text;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxCustomTrack = class(TALDynamicListBoxExtendedControl)
  public
    type
      // ------
      // TTrack
      TTrack = class(TALDynamicListBoxBaseRectangle)
      public
        Type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      protected
        function CreateMargins: TALBounds; override;
      public
        type
          // -----
          // TFill
          TFill = class(TALBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------
          // TStroke
          TStroke = class(TALStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------------------
          // TStopIndicatorBrush
          TStopIndicatorBrush = class(TALPersistentObserver)
          private
            FColor: TAlphaColor;
            FResourceName: String;
            FWrapMode: TALImageWrapMode;
            FSize: Single;
            procedure SetColor(const Value: TAlphaColor);
            procedure SetResourceName(const Value: String);
            procedure SetWrapMode(const Value: TALImageWrapMode);
            procedure SetSize(const Value: Single);
            function IsColorStored: Boolean;
            function IsResourceNameStored: Boolean;
            function IsWrapModeStored: Boolean;
            function IsSizeStored: Boolean;
          protected
            function GetDefaultColor: TAlphaColor; virtual;
            function GetDefaultResourceName: String; virtual;
            function GetDefaultWrapMode: TALImageWrapMode; virtual;
            function GetDefaultSize: Single; virtual;
          public
            constructor Create; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; virtual;
            procedure Interpolate(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single); virtual;
            procedure InterpolateNoChanges(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single);
            function hasStopIndicator: Boolean;
            property DefaultColor: TAlphaColor read GetDefaultColor;
            property DefaultResourceName: String read GetDefaultResourceName;
            property DefaultWrapMode: TALImageWrapMode read GetDefaultWrapMode;
            property DefaultSize: Single read GetDefaultSize;
          public
            property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
            property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored nodefault;
            property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
            property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
          end;
          // --------------------------
          // TInheritStopIndicatorBrush
          TInheritStopIndicatorBrush = class(TStopIndicatorBrush)
          private
            FParent: TStopIndicatorBrush;
            FInherit: Boolean;
            fSuperseded: Boolean;
            procedure SetInherit(const AValue: Boolean);
          protected
            function CreateSavedState: TALPersistentObserver; override;
            procedure DoSupersede; virtual;
          public
            constructor Create(const AParent: TStopIndicatorBrush); reintroduce; virtual;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure Supersede(Const ASaveState: Boolean = False); virtual;
            procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
            property Superseded: Boolean read FSuperseded;
            property Parent: TStopIndicatorBrush read FParent;
          public
            property Inherit: Boolean read FInherit write SetInherit Default True;
          end;
          // ---------------
          // TBaseStateStyle
          TBaseStateStyle = class(TALDynamicListBoxBaseStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          private
            FStopIndicator: TInheritStopIndicatorBrush;
            function GetControlParent: TTrack;
            procedure SetStopIndicator(const AValue: TInheritStopIndicatorBrush);
            procedure StopIndicatorChanged(ASender: TObject);
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
            function CreateStopIndicator(const AParent: TStopIndicatorBrush): TInheritStopIndicatorBrush; virtual;
            procedure DoSupersede; override;
            function GetInherit: Boolean; override;
          public
            constructor Create(const AParent: TObject); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure Interpolate(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single); override;
            property ControlParent: TTrack read GetControlParent;
          public
            property Fill;
            property StopIndicator: TInheritStopIndicatorBrush read FStopIndicator write SetStopIndicator;
            property Stroke;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TBaseStateStyle)
          private
            FOpacity: Single;
            procedure SetOpacity(const Value: Single);
            function IsOpacityStored: Boolean;
          protected
            function GetInherit: Boolean; override;
          public
            constructor Create(const AParent: TObject); override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
          public
            property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALDynamicListBoxBaseStateStyles)
          private
            FDisabled: TDisabledStateStyle;
            function GetParent: TTrack;
            procedure SetDisabled(const AValue: TDisabledStateStyle);
            procedure DisabledChanged(ASender: TObject);
          protected
            function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
          public
            constructor Create(const AParent: TALDynamicListBoxControl); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure ClearBufDrawable; override;
            function GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle; override;
            Property Parent: TTrack read GetParent;
          public
            property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
          end;
      private
        {$IF defined(ALDPK)}
        FPrevStateStyles: TStateStyles;
        {$ENDIF}
        FStateStyles: TStateStyles;
        FCustomTrack: TALDynamicListBoxCustomTrack;
        FStopIndicator: TStopIndicatorBrush;
        procedure SetStateStyles(const AValue: TStateStyles);
        procedure SetStopIndicator(const Value: TStopIndicatorBrush);
      protected
        function CreateFill: TALBrush; override;
        function CreateStroke: TALStrokeBrush; override;
        function CreateStopIndicator: TStopIndicatorBrush; virtual;
        function CreateStateStyles: TStateStyles; virtual;
        procedure SetXRadius(const Value: Single); override;
        procedure SetYRadius(const Value: Single); override;
        procedure StateStylesChanged(Sender: TObject); virtual;
        procedure StopIndicatorChanged(Sender: TObject); virtual;
        procedure PaddingChanged; override;
        function HasCustomDraw: Boolean; override;
        Procedure CreateBufDrawable(
                    var ABufDrawable: TALDrawable;
                    out ABufDrawableRect: TRectF;
                    const AScale: Single;
                    const AFill: TALBrush;
                    const AStateLayer: TALStateLayer;
                    const AStateLayerContentColor: TAlphaColor;
                    const ADrawStateLayerOnTop: Boolean;
                    const AStroke: TALStrokeBrush;
                    const AShadow: TALShadow); overload; override;
        Procedure CreateBufDrawable(
                    var ABufDrawable: TALDrawable;
                    out ABufDrawableRect: TRectF;
                    const AScale: Single;
                    const AFill: TALBrush;
                    const AStateLayer: TALStateLayer;
                    const AStateLayerContentColor: TAlphaColor;
                    const ADrawStateLayerOnTop: Boolean;
                    const AStroke: TALStrokeBrush;
                    const AShadow: TALShadow;
                    const AStopIndicator: TStopIndicatorBrush); reintroduce; overload; virtual;
        function GetBufDrawableSrcRect: TRectF; virtual; abstract;
        procedure Paint; override;
      public
        constructor Create(const ACustomTrack: TALDynamicListBoxCustomTrack); reintroduce; virtual;
        destructor Destroy; override;
        procedure AlignToPixel; override;
        procedure MakeBufDrawable; override;
        procedure ClearBufDrawable; override;
        property HitTest default false;
        //property Locked default True;
        //property Position stored false;
      public
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        //property CanFocus;
        //property CanParentFocus;
        //property DisableFocusEffect;
        //property ClipChildren;
        //property ClipParent;
        property Corners;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        //property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest;
        //property Locked;
        property Margins;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        //property Shadow;
        //property Sides;
        //property Size;
        property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
        property StopIndicator: TStopIndicatorBrush read FStopIndicator write SetStopIndicator;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        //property TouchTargetExpansion;
        //property Visible;
        //property Width;
        property XRadius;
        property YRadius;
        //property OnCanFocus;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
      // --------------
      // TInactiveTrack
      TInactiveTrack = class(TTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // ------------
      // TActiveTrack
      TActiveTrack = class(TTrack)
      public
        type
          // -----
          // TFill
          TFill = class(TTrack.TFill)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TTrack.TDisabledStateStyle)
          public
            type
              TFill = class(TTrack.TDisabledStateStyle.TFill)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TTrack.TStateStyles)
          protected
            function CreateDisabledStateStyle(const AParent: TObject): TTrack.TDisabledStateStyle; override;
          end;
      protected
        function CreateFill: TALBrush; override;
        function CreateStateStyles: TTrack.TStateStyles; override;
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // ------
      // TThumb
      TThumb = class(TALDynamicListBoxBaseRectangle)
      public
        type
          // -------
          // TStroke
          TStroke = class(TALStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // ---------------
          // TBaseStateStyle
          TBaseStateStyle = class(TALDynamicListBoxBaseStateStyle)
          public
            type
              TStroke = class(TALInheritStrokeBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          public
            property Fill;
            property Scale;
            property Shadow;
            property Stroke;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TBaseStateStyle)
          private
            FOpacity: Single;
            procedure SetOpacity(const Value: Single);
            function IsOpacityStored: Boolean;
          protected
            function GetInherit: Boolean; override;
          public
            constructor Create(const AParent: TObject); override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
          public
            property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
          end;
          // ------------------
          // THoveredStateStyle
          THoveredStateStyle = class(TBaseStateStyle)
          public
            property StateLayer;
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TBaseStateStyle)
          public
            property StateLayer;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TBaseStateStyle)
          public
            property StateLayer;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALDynamicListBoxBaseStateStyles)
          private
            FDisabled: TDisabledStateStyle;
            FHovered: THoveredStateStyle;
            FPressed: TPressedStateStyle;
            FFocused: TFocusedStateStyle;
            function GetParent: TThumb;
            procedure SetDisabled(const AValue: TDisabledStateStyle);
            procedure SetHovered(const AValue: THoveredStateStyle);
            procedure SetPressed(const AValue: TPressedStateStyle);
            procedure SetFocused(const AValue: TFocusedStateStyle);
            procedure DisabledChanged(ASender: TObject);
            procedure HoveredChanged(ASender: TObject);
            procedure PressedChanged(ASender: TObject);
            procedure FocusedChanged(ASender: TObject);
          protected
            function CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle; virtual;
            function CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle; virtual;
            function CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle; virtual;
            function CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle; virtual;
          public
            constructor Create(const AParent: TALDynamicListBoxControl); override;
            destructor Destroy; override;
            procedure Assign(Source: TPersistent); override;
            procedure Reset; override;
            procedure AlignToPixel; override;
            procedure ClearBufDrawable; override;
            function GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle; override;
            Property Parent: TThumb read GetParent;
          public
            property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
            property Hovered: THoveredStateStyle read FHovered write SetHovered;
            property Pressed: TPressedStateStyle read FPressed write SetPressed;
            property Focused: TFocusedStateStyle read FFocused write SetFocused;
            property Transition;
          end;
      private
        {$IF defined(ALDPK)}
        FPrevStateStyles: TStateStyles;
        {$ENDIF}
        FStateStyles: TStateStyles;
        fValueRange: TValueRange;
        FCustomTrack: TALDynamicListBoxCustomTrack;
        fCustomTrackMouseDownPos: TPointF;
        fScrollCapturedByMe: boolean;
        procedure SetStateStyles(const AValue: TStateStyles);
        procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
      protected
        function CreateStroke: TALStrokeBrush; override;
        function CreateStateStyles: TStateStyles; virtual;
        procedure DoBeginUpdate; override;
        procedure DoEndUpdate; override;
        function GetDefaultXRadius: Single; override;
        function GetDefaultYRadius: Single; override;
        procedure SetXRadius(const Value: Single); override;
        procedure SetYRadius(const Value: Single); override;
        procedure StateStylesChanged(Sender: TObject); virtual;
        procedure IsMouseOverChanged; override;
        //procedure IsFocusedChanged; override;
        procedure PressedChanged; override;
        procedure ValueRangeChanged(Sender: TObject); Virtual;
        //procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
        {$IF NOT DEFINED(ALSkiaCanvas)}
        function GetRenderTargetRect(const ARect: TrectF): TRectF; override;
        {$ENDIF}
        procedure Paint; override;
      public
        constructor Create(const ACustomTrack: TALDynamicListBoxCustomTrack); reintroduce; virtual;
        destructor Destroy; override;
        procedure AlignToPixel; override;
        function GetValue: Double;
        procedure MakeBufDrawable; override;
        procedure ClearBufDrawable; override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
        procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
        procedure MouseLeave; override;
        //property Locked default True;
        //property Position stored false;
        //property CanFocus default true;
      public
        //property Action;
        //property Align;
        //property Anchors;
        //property AutoSize;
        //property CanFocus default true;
        //property CanParentFocus;
        //property DisableFocusEffect;
        //property ClipChildren;
        //property ClipParent;
        property Corners;
        property Cursor default crHandPoint;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest;
        //property Locked;
        property Margins;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Shadow;
        //property Sides;
        //property Size;
        property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        property TouchTargetExpansion;
        //property Visible;
        property Width;
        property XRadius;
        property YRadius;
        //property OnCanFocus;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
      // ---------------
      // TValueIndicator
      TValueIndicator = class(TALDynamicListBoxBaseText)
      public
        Type
          TPadding = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      protected
        function CreatePadding: TALBounds; override;
      public
        Type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
      protected
        function CreateMargins: TALBounds; override;
      public
        class var Format0: String;
      public
        type
          // -----
          // TFill
          TFill = class(TALDynamicListBoxBaseText.TFill)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------------
          // TTextSettings
          TTextSettings = Class(TALTextSettings)
          public
            Type
              TFont = Class(TALFont)
              protected
                function GetDefaultWeight: TFontWeight; override;
                function GetDefaultColor: TAlphaColor; override;
              End;
          protected
            function CreateFont: TALFont; override;
            function GetDefaultHorzAlign: TALTextHorzAlign; override;
          End;
          // ----------
          // TAnimation
          TAnimation = (None, ScaleInOut, Opacity);
          // ------------------
          // TCustomFormatEvent
          TCustomFormatEvent = procedure(ASender: TObject; const AValue: Double; Var AText: String) of object;
      private
        FCustomTrack: TALDynamicListBoxCustomTrack;
        FFormat: String;
        FOnCustomFormat: TCustomFormatEvent;
        FFloatAnimation: TALFloatAnimation;
        FAnimation: TAnimation;
        FShowOnInteraction: Boolean;
        function GetTextSettings: TALTextSettings;
        procedure SetFormat(const Value: string);
        function IsFormatStored: Boolean;
        procedure AnimationProcess(Sender: TObject);
        procedure AnimationFinish(Sender: TObject);
        procedure AdjustPosition(const AThumb: TThumb);
      protected
        function CreateFill: TALBrush; override;
        function CreateTextSettings: TALBaseTextSettings; override;
        procedure SetTextSettings(const Value: TALTextSettings); reintroduce;
        function GetDefaultXRadius: Single; override;
        function GetDefaultYRadius: Single; override;
        function GetDefaultFormat: String; virtual;
      public
        constructor Create(const ACustomTrack: TALDynamicListBoxCustomTrack); reintroduce; virtual;
        destructor Destroy; override;
        procedure Refresh(const AThumb: TThumb);
        property DefaultFormat: String read GetDefaultFormat;
        property Visible default false;
      public
        //property Action;
        //property Align;
        //property Anchors;
        property Animation: TAnimation read FAnimation write FAnimation default TAnimation.ScaleInOut;
        property AutoSize default true;
        //property AutoTranslate;
        //property CanFocus;
        //property CanParentFocus;
        //property DisableFocusEffect;
        //property ClipChildren;
        //property ClipParent;
        property Corners;
        //property Cursor;
        //property DoubleBuffered;
        //property DragMode;
        //property EnableDragHighlight;
        //property Enabled;
        property Fill;
        property Format: string read FFormat write SetFormat stored IsFormatStored;
        property Height;
        //property Hint;
        //property ParentShowHint;
        //property ShowHint;
        //property HitTest;
        //property Locked;
        property Margins;
        //property MaxWidth;
        //property MaxHeight;
        property Opacity;
        property Padding;
        //property PopupMenu;
        //property Position;
        //property RotationAngle;
        //property RotationCenter;
        //property Pivot;
        //property Scale;
        property Shadow;
        property ShowOnInteraction: Boolean read FShowOnInteraction write FShowOnInteraction default false;
        property Sides;
        //property Size;
        property Stroke;
        //property TabOrder;
        //property TabStop;
        //property Text;
        property TextSettings: TALTextSettings read GetTextSettings write SetTextSettings;
        //property TouchTargetExpansion;
        //property Visible default false;
        property Width;
        property XRadius;
        property YRadius;
        //property OnCanFocus;
        property OnCustomFormat: TCustomFormatEvent read FOnCustomFormat write FOnCustomFormat;
        //property OnDragEnter;
        //property OnDragLeave;
        //property OnDragOver;
        //property OnDragDrop;
        //property OnDragEnd;
        //property OnElementClick;
        //property OnElementDblClick;
        //property OnElementMouseDown;
        //property OnElementMouseMove;
        //property OnElementMouseUp;
        //property OnElementMouseEnter;
        //property OnElementMouseLeave;
        //property OnEnter;
        //property OnExit;
        //property OnMouseEnter;
        //property OnMouseLeave;
        //property OnMouseDown;
        //property OnMouseUp;
        //property OnMouseMove;
        //property OnMouseWheel;
        //property OnClick;
        //property OnDblClick;
        //property OnKeyDown;
        //property OnKeyUp;
        property OnPainting;
        property OnPaint;
        //property OnResize;
        //property OnResized;
      end;
  private
    FTabStop: Boolean;
    FIsAligningTracks: Boolean;
    function FrequencyStored: Boolean;
    function MaxStored: Boolean;
    function MinStored: Boolean;
    function ViewportSizeStored: Boolean;
  protected
    type
      TInactiveTrackClass = Class of TInactiveTrack;
      TActiveTrackClass = Class of TActiveTrack;
      TThumbClass = Class of TThumb;
      TValueIndicatorClass = Class of TValueIndicator;
  protected
    FInactiveTrack: TInactiveTrack;
    FActiveTrack: TActiveTrack;
    FThumb: TThumb;
    FValueIndicator: TValueIndicator;
    FOrientation: TOrientation;
    FOnChange: TNotifyEvent;
    function GetLeadingTrack: TTrack; virtual;
    function GetTrailingTrack: TTrack; virtual;
    function GetLeadingTrackStartPadding: Single;
    function GetTrailingTrackEndPadding: Single;
    /// <summary>
    ///   Return the precise size of the track where the center
    ///   of the thumb can be positioned
    /// </summary>
    function GetTrackSize(Const AIncludeTrackPadding: Boolean = False): Single; virtual;
    function GetDoubleBuffered: boolean; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    //function _GetCanFocus: Boolean; virtual; virtual;
    //procedure _SetCanFocus(const Value: Boolean); virtual; virtual;
    //procedure _SetTabStop(const Value: Boolean); virtual; virtual;
    procedure SetViewportSize(const Value: Double); virtual;
    function GetViewportSize: Double; virtual;
    function GetFrequency: Double; virtual;
    procedure SetFrequency(const Value: Double); virtual;
    function GetMax: Double; virtual;
    procedure SetMax(const Value: Double); virtual;
    function GetMin: Double; virtual;
    procedure SetMin(const Value: Double); virtual;
    function GetValue: Double; virtual;
    procedure SetValue(Value: Double); virtual;
    function ValueStored: Boolean; virtual;
    procedure SetOrientation(const Value: TOrientation); virtual;
    /// <summary>
    ///   Return the center position of the thumb corresponding to AValue
    /// </summary>
    function GetThumbPos(const AValue: single): Single; virtual;
    procedure DoResized; override;
    procedure DoRealign; override;
    //procedure Loaded; override;
    procedure DoChanged; virtual;
    procedure EnabledChanged; override;
    property Value: Double read GetValue write SetValue stored ValueStored nodefault;
    property InactiveTrack: TInactiveTrack read FInactiveTrack;
    property ActiveTrack: TActiveTrack read FActiveTrack;
    property Thumb: TThumb read FThumb;
    property ValueIndicator: TValueIndicator read FValueIndicator;
    function CreateInactiveTrack(const AInactiveTrackClass: TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TInactiveTrack; virtual;
    function CreateActiveTrack(const AActiveTrackClass: TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TActiveTrack; virtual;
    function CreateThumb(const AThumbClass: TThumbClass = nil; Const AName: String = 'Thumb'): TThumb; virtual;
    function CreateValueIndicator(const AValueIndicatorClass: TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TValueIndicator; virtual;
    procedure AlignThumb; virtual;
    procedure AlignTracks; virtual;
  public
    constructor Create(const AOwner: TObject); override;
    procedure AfterConstruction; override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
    property DoubleBuffered default true;
    //property CanFocus: Boolean read _GetCanFocus write _SetCanFocus default True;
    //property TabStop: Boolean read FTabStop write _SetTabStop default True;
    property Min: Double read GetMin write SetMin stored MinStored nodefault;
    property Max: Double read GetMax write SetMax stored MaxStored nodefault;
    property Frequency: Double read GetFrequency write SetFrequency stored FrequencyStored nodefault;
    property ViewportSize: Double read GetViewportSize write SetViewportSize stored ViewportSizeStored nodefault;
    property Orientation: TOrientation read FOrientation write SetOrientation default TOrientation.Horizontal;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxTrackBar = class(TALDynamicListBoxCustomTrack)
  protected
    function GetDefaultSize: TSizeF; override;
  public
    //property Action;
    property ActiveTrack;
    property Align;
    //property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Frequency;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property InactiveTrack;
    //property Locked;
    property Margins;
    property Min;
    property Max;
    property Opacity;
    property Orientation;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    //property Size;
    //property TabOrder;
    //property TabStop;
    property Thumb;
    property TouchTargetExpansion;
    property Value;
    property ValueIndicator;
    property Visible;
    property Width;
    //property OnCanFocus;
    property OnChange;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxRangeTrackBar = class(TALDynamicListBoxCustomTrack)
  public
    Type
      // -----------------
      // TMinInactiveTrack
      TMinInactiveTrack = class(TALDynamicListBoxCustomTrack.TInactiveTrack)
      private
        function _GetOpacity: Single;
        procedure _SetOpacity(const AValue: Single);
        function _IsOpacityStored: boolean;
      protected
        procedure SetXRadius(const Value: Single); override;
        procedure SetYRadius(const Value: Single); override;
        procedure SetCorners(const Value: TCorners); override;
        procedure MarginsChanged; override;
        procedure PaddingChanged; override;
        procedure StopIndicatorChanged(Sender: TObject); override;
        procedure FillChanged(Sender: TObject); override;
        procedure StrokeChanged(Sender: TObject); override;
        function GetBufDrawableSrcRect: TRectF; override;
      public
        property Opacity: Single read _GetOpacity write _SetOpacity stored _IsOpacityStored nodefault;
      end;
      // -----------------
      // TMaxInactiveTrack
      TMaxInactiveTrack = class(TALDynamicListBoxCustomTrack.TInactiveTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // ------------
      // TActiveTrack
      TActiveTrack = class(TALDynamicListBoxCustomTrack.TActiveTrack)
      protected
        function GetBufDrawableSrcRect: TRectF; override;
      end;
      // ---------
      // TMinThumb
      TMinThumb = class(TALDynamicListBoxCustomTrack.TThumb)
      private
        //FFormerTouchTargetExpansionChangedHandler: TNotifyEvent;
        function _GetOpacity: Single;
        procedure _SetOpacity(const AValue: Single);
        function _GetCursor: TCursor;
        procedure _SetCursor(const AValue: TCursor);
        function _IsOpacityStored: boolean;
      protected
        procedure SetXRadius(const Value: Single); override;
        procedure SetYRadius(const Value: Single); override;
        procedure SetCorners(const Value: TCorners); override;
        procedure MarginsChanged; override;
        procedure PaddingChanged; override;
        procedure FillChanged(Sender: TObject); override;
        procedure StrokeChanged(Sender: TObject); override;
        procedure ShadowChanged(Sender: TObject); override;
        procedure StateStylesChanged(Sender: TObject); override;
        procedure SetTouchTargetExpansion(const AValue: TRectF); override;
        procedure DoResized; override;
     public
       constructor Create(const ACustomTrack: TALDynamicListBoxCustomTrack); override;
     public
        property Opacity: Single read _GetOpacity write _SetOpacity stored _IsOpacityStored nodefault;
        property Cursor: TCursor read _GetCursor write _SetCursor default crHandPoint;
      end;
      // ---------
      // TMaxThumb
      TMaxThumb = class(TALDynamicListBoxCustomTrack.TThumb)
      public
        constructor Create(const ACustomTrack: TALDynamicListBoxCustomTrack); override;
      end;
  protected
    FMaxInactiveTrack: TALDynamicListBoxCustomTrack.TInactiveTrack;
    FMaxThumb: TALDynamicListBoxCustomTrack.TThumb;
    function GetLeadingTrack: TALDynamicListBoxCustomTrack.TTrack; override;
    function GetTrailingTrack: TALDynamicListBoxCustomTrack.TTrack; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    //procedure _SetCanFocus(const Value: Boolean); virtual; override;
    //procedure _SetTabStop(const Value: Boolean); virtual; override;
    procedure SetViewportSize(const Value: Double); override;
    procedure SetFrequency(const Value: Double); override;
    procedure SetMax(const Value: Double); override;
    procedure SetMin(const Value: Double); override;
    function MaxValueStored: Boolean; virtual;
    function GetDefaultSize: TSizeF; override;
    procedure SetValue(Value: Double); override;
    function GetMaxValue: Double; virtual;
    procedure SetMaxValue(Value: Double); virtual;
    procedure SetOrientation(const Value: TOrientation); override;
    procedure EnabledChanged; override;
    //procedure Loaded; override;
    function CreateInactiveTrack(const AInactiveTrackClass: TALDynamicListBoxCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALDynamicListBoxCustomTrack.TInactiveTrack; override;
    function CreateActiveTrack(const AActiveTrackClass: TALDynamicListBoxCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALDynamicListBoxCustomTrack.TActiveTrack; override;
    function CreateThumb(const AThumbClass: TALDynamicListBoxCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALDynamicListBoxCustomTrack.TThumb; override;
    procedure DoResized; override;
    procedure DoRealign; override;
    procedure AlignThumb; override;
    procedure AlignTracks; override;
  public
    constructor Create(const AOwner: TObject); override;
    procedure MakeBufDrawable; override;
    procedure ClearBufDrawable; override;
  public
    //property Action;
    property ActiveTrack;
    property Align;
    //property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Frequency;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property InactiveTrack;
    //property Locked;
    property Margins;
    property Min;
    property Max;
    property MinValue: Double read GetValue write SetValue stored ValueStored nodefault;
    property MaxValue: Double read GetMaxValue write SetMaxValue stored MaxValueStored nodefault;
    property Thumb;
    property Opacity;
    property Orientation;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    //property Size;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Value;
    property ValueIndicator;
    property Visible;
    property Width;
    //property OnCanFocus;
    property OnChange;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxCustomScrollBar = class(TALDynamicListBoxCustomTrack)
  public
    type
      // ------
      // TThumb
      TThumb = class(TALDynamicListBoxCustomTrack.TThumb)
      public
        type
          // -----
          // TFill
          TFill = class(TALBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------
          // TStroke
          TStroke = class(TALDynamicListBoxCustomTrack.TThumb.TStroke)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          // -------------------
          // TDisabledStateStyle
          TDisabledStateStyle = class(TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle.TStroke)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle.TStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          end;
          // ------------------
          // THoveredStateStyle
          THoveredStateStyle = class(TALDynamicListBoxCustomTrack.TThumb.THoveredStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALDynamicListBoxCustomTrack.TThumb.THoveredStateStyle.TStroke)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALDynamicListBoxCustomTrack.TThumb.THoveredStateStyle.TStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          end;
          // ------------------
          // TPressedStateStyle
          TPressedStateStyle = class(TALDynamicListBoxCustomTrack.TThumb.TPressedStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALDynamicListBoxCustomTrack.TThumb.TPressedStateStyle.TStroke)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALDynamicListBoxCustomTrack.TThumb.TPressedStateStyle.TStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          end;
          // ------------------
          // TFocusedStateStyle
          TFocusedStateStyle = class(TALDynamicListBoxCustomTrack.TThumb.TFocusedStateStyle)
          public
            type
              TFill = class(TALInheritBrush)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStroke = class(TALDynamicListBoxCustomTrack.TThumb.TFocusedStateStyle.TStroke)
              protected
                function GetDefaultColor: TAlphaColor; override;
              end;
              TStateLayer = class(TALDynamicListBoxCustomTrack.TThumb.TFocusedStateStyle.TStateLayer)
              protected
                function GetDefaultXRadius: Single; override;
                function GetDefaultYRadius: Single; override;
              end;
          protected
            function CreateFill(const AParent: TALBrush): TALInheritBrush; override;
            function CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush; override;
            function CreateStateLayer: TALStateLayer; override;
          end;
          // ------------
          // TStateStyles
          TStateStyles = class(TALDynamicListBoxCustomTrack.TThumb.TStateStyles)
          protected
            function CreateDisabledStateStyle(const AParent: TObject): TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle; override;
            function CreateHoveredStateStyle(const AParent: TObject): TALDynamicListBoxCustomTrack.TThumb.THoveredStateStyle; override;
            function CreatePressedStateStyle(const AParent: TObject): TALDynamicListBoxCustomTrack.TThumb.TPressedStateStyle; override;
            function CreateFocusedStateStyle(const AParent: TObject): TALDynamicListBoxCustomTrack.TThumb.TFocusedStateStyle; override;
          end;
      protected
        function CreateFill: TALBrush; override;
        function CreateStroke: TALStrokeBrush; override;
        function CreateStateStyles: TALDynamicListBoxCustomTrack.TThumb.TStateStyles; override;
        function GetDefaultXRadius: Single; override;
        function GetDefaultYRadius: Single; override;
      end;
  protected
    function GetDefaultSize: TSizeF; override;
    procedure AlignThumb; override;
    function CreateInactiveTrack(const AInactiveTrackClass: TALDynamicListBoxCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALDynamicListBoxCustomTrack.TInactiveTrack; override;
    function CreateActiveTrack(const AActiveTrackClass: TALDynamicListBoxCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALDynamicListBoxCustomTrack.TActiveTrack; override;
    function CreateValueIndicator(const AValueIndicatorClass: TALDynamicListBoxCustomTrack.TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TALDynamicListBoxCustomTrack.TValueIndicator; override;
    function CreateThumb(const AThumbClass: TALDynamicListBoxCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALDynamicListBoxCustomTrack.TThumb; override;
  public
    constructor Create(const AOwner: TObject); override;
    //property CanFocus default False;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxScrollBar = class(TALDynamicListBoxCustomScrollBar)
  public
    //property Action;
    property Align;
    //property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property DoubleBuffered;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    //property Locked;
    property Margins;
    property Min;
    property Max;
    property Opacity;
    property Orientation;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    //property Size;
    //property TabOrder;
    //property TabStop;
    property Thumb;
    property TouchTargetExpansion;
    property Value;
    property ViewportSize;
    property Visible;
    property Width;
    //property OnCanFocus;
    property OnChange;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxLayout = class(TALDynamicListBoxExtendedControl)
  protected
    procedure Paint; override;
  public
    constructor Create(const AOwner: TObject); override;
  public
    //property Action;
    property Align;
    //property Anchors;
    property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest default False;
    //property Locked;
    property Margins;
    property Opacity;
    property Padding;
    //property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    //property Size;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    //property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    //property OnResized;
  end;

{$ENDREGION}

//////////////////////////////////////////////
/// THE CODE ABOVE WAS AUTO-GENERATED FROM ///
/// <ALCINOE>\Tools\CodeBuilder.           ///
//////////////////////////////////////////////

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxItemContent = class(TALDynamicListBoxExtendedControl)
  public
    constructor Create(const AOwner: TALDynamicListBoxItem); reintroduce; virtual;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxItemMainContent = class(TALDynamicListBoxItemContent)
  public
    constructor Create(const AOwner: TALDynamicListBoxItem); override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxItemLoadingContent = class(TALDynamicListBoxItemContent)
  public
    type
      TSkeletonAnimationKind = (None, Pulse, Wave);
      TSkeletonAnimation = class(Tobject)
      private
        FOwner: TALDynamicListBoxItemLoadingContent;
        FKind: TSkeletonAnimationKind;
        FAnimation: TALFloatAnimation;
        FWaveColor: TAlphaColor;
        FWaveAngle: Single;
      protected
        procedure SetKind(const AValue: TSkeletonAnimationKind); virtual;
        procedure AnimationProcess(Sender: TObject); virtual;
      public
        constructor Create(const AOwner: TALDynamicListBoxItemLoadingContent); Virtual;
        destructor Destroy; override;
        property Kind: TSkeletonAnimationKind read FKind write SetKind;
        property Animation: TALFloatAnimation read FAnimation;
        property WaveColor: TAlphaColor read FWaveColor write FWaveColor;
        property WaveAngle: Single read FWaveAngle write FWaveAngle;
      end;
  private
    FSkeletonAnimation: TSkeletonAnimation;
  protected
    procedure Paint; override;
  public
    constructor Create(const AOwner: TALDynamicListBoxItem); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    property SkeletonAnimation: TSkeletonAnimation read FSkeletonAnimation;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxItemErrorContent = class(TALDynamicListBoxItemContent)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxItem = class(TALDynamicListBoxExtendedControl)
  public
    type
      TDownloadDataContext = class;
      TDownloadDataEvent = procedure(const AContext: TDownloadDataContext; out AData: TALJSONNodeW; var AErrorCode: Integer) of object;
      TDownloadDataContext = Class(TObject)
      private
        // Before accessing the Owner property,
        // you must first lock the context.
        Lock: TObject;
        FreeByThread: boolean;
        OnDownloadData: TDownloadDataEvent;
      public
        Owner: TALDynamicListBoxItem;
        CustomParams: TALStringListW;
        constructor Create(const AOwner: TALDynamicListBoxItem); virtual;
        destructor Destroy; override;
      end;
  public
    type
      TCreateContentContext = class;
      TCreateMainContentEvent = function(const AContext: TCreateContentContext): TALDynamicListBoxItemMainContent of object;
      TCreateLoadingContentEvent = function(const AContext: TCreateContentContext): TALDynamicListBoxItemLoadingContent of object;
      TCreateErrorContentEvent = function(const AContext: TCreateContentContext): TALDynamicListBoxItemErrorContent of object;
      TCreateContentContext = Class(TObject)
      private
        // Before accessing the Owner property,
        // you must first lock the context.
        Lock: TObject;
        FreeByThread: boolean;
        OnCreateMainContent: TCreateMainContentEvent;
        OnCreateErrorContent: TCreateErrorContentEvent;
      public
        Owner: TALDynamicListBoxItem;
        CustomParams: TALStringListW;
        CacheEngine: TALBufDrawableCacheEngine;
        TargetRect: TALRectD;
        constructor Create(const AOwner: TALDynamicListBoxItem); virtual;
        destructor Destroy; override;
      end;
  private
    FData: TALJSONNodeW; // 8 bytes
    FDownloadDataContext: TDownloadDataContext; // 8 bytes
    FDownloadDataErrorCode: Integer; // 4 byte
    FOnDownloadData: TDownloadDataEvent; // 16 bytes
    //--
    FCreateContentContext: TCreateContentContext; // 8 bytes
    FBackgroundContent: TALDynamicListBoxItemContent; // 8 Bytes
    FMainContent: TALDynamicListBoxItemContent; // 8 bytes
    FLoadingContent: TALDynamicListBoxItemContent; // 8 bytes
    FErrorContent: TALDynamicListBoxItemContent; // 8 bytes
    FForegroundContent: TALDynamicListBoxItemContent; // 8 Bytes
    FOnCreateMainContent: TCreateMainContentEvent; // 16 bytes
    FOnCreateLoadingContent: TCreateLoadingContentEvent; // 16 bytes
    FOnCreateErrorContent: TCreateErrorContentEvent; // 16 bytes
  protected
    function DownloadData(const AForceReload: Boolean = False): boolean; virtual;
    function CreateDownloadDataContext: TDownloadDataContext; virtual;
    class procedure DownloadDataBackgroundProc(var AContext: Tobject); virtual; // [MultiThread]
    class procedure DownloadDataBackgroundProcFetchData(const AContext: TDownloadDataContext; out AData: TALJSONNodeW; var AErrorCode: Integer); virtual; // [MultiThread]
    class procedure DownloadDataBackgroundProcInitData(const AContext: TDownloadDataContext; const AErrorCode: Integer; const AData: TALJSONNodeW); virtual; // [MultiThread]
    class function DownloadDataBackgroundProcCanProcessData(const AContext: TDownloadDataContext): boolean; virtual; // [MultiThread]
    procedure DownloadDataProcessData(const AContext: TDownloadDataContext; const AErrorCode: Integer; var AData: TALJSONNodeW); virtual;
    procedure DownloadDataFinished; virtual;
    function CanDownloadData: Boolean; virtual;
    function IsDownloadDataRunning: Boolean;
    Function HasDataBeenDownloaded: Boolean;
    procedure CancelDownloadData;
    //--
    function CreateContentCreationContext : TCreateContentContext; virtual;
    procedure CancelPreloadContent; virtual;
    function IsContentPreloading: Boolean; virtual;
    procedure ActivateCoreContent(const AContent: TALDynamicListBoxItemContent); virtual;
    //--
    function CanCreateMainContent: Boolean; virtual;
    procedure PreloadMainContent(var AContext: Tobject); virtual;
    function CreateMainContent(const AContext: TCreateContentContext): TALDynamicListBoxItemMainContent; virtual;
    procedure ShiftMainContent(const AContent: TALDynamicListBoxItemMainContent); virtual;
    //--
    function CanCreateLoadingContent: Boolean; virtual;
    function CreateLoadingContent: TALDynamicListBoxItemLoadingContent; virtual;
    procedure ShiftLoadingContent(const AContent: TALDynamicListBoxItemLoadingContent); virtual;
    //--
    function CanCreateErrorContent: Boolean; virtual;
    procedure PreloadErrorContent(var AContext: Tobject); virtual;
    function CreateErrorContent(const AContext: TCreateContentContext): TALDynamicListBoxItemErrorContent; virtual;
    procedure ShiftErrorContent(const AContent: TALDynamicListBoxItemErrorContent); virtual;
    //--
    function IsVisibleWithinListboxBounds: Boolean; override;
    procedure PaintInternal(const ACanvas: TCanvas); override;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure RemoveControl(const AControl: TALDynamicListBoxControl); override;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function FetchContent(const APreload: boolean): TALDynamicListBoxItemContent; overload; virtual;
    function FetchContent: TALDynamicListBoxItemContent; overload; virtual;
    property Data: TALJSONNodeW read FData;
    property OnDownloadData: TDownloadDataEvent read FOnDownloadData write FOnDownloadData; // [MultiThread]
    property OnCreateMainContent: TCreateMainContentEvent read FOnCreateMainContent write FOnCreateMainContent;
    property OnCreateLoadingContent: TCreateLoadingContentEvent read FOnCreateLoadingContent write FOnCreateLoadingContent;
    property OnCreateErrorContent: TCreateErrorContentEvent read FOnCreateErrorContent write FOnCreateErrorContent;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxViewMainContent = class(TALDynamicListBoxItemMainContent)
  private
    Type
      TRealignEvent = procedure(const AContent: TALDynamicListBoxViewMainContent; const AStartIndex: integer) of object;
  private
    FOnRealign: TRealignEvent; // 8 bytes
  protected
    procedure DoRealign(const AStartIndex: integer); reintroduce; overload; virtual;
    procedure DoRealign; overload; override;
    procedure Realign(const AStartIndex: integer); overload;
    procedure AdjustSize; override;
    procedure ParentChanged; override;
    function GetFirstVisibleObjectIndex: Integer; override;
    function GetLastVisibleObjectIndex: Integer; override;
    function PaintChildrenOnly: Boolean; override;
  public
    constructor Create(const AOwner: TALDynamicListBoxItem); override;
    procedure InsertItems(const AItems: TArray<TALDynamicListBoxItem>; const AIndex: Integer); virtual;
    property OnRealign: TRealignEvent read FOnRealign write FOnRealign;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxViewNoItemsContent = class(TALDynamicListBoxItemContent)
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicListBoxView = class(TALDynamicListBoxItem)
  public
    type
      TItemIdType = (Unknown, Int64, Text);
  public
    type
      TDownloadItemsContext = class;
      TCreateItemEvent = function(const AContext: TDownloadItemsContext; out AData: TALJSONNodeW): TALDynamicListBoxItem of object;
      TDownloadItemsEvent = procedure(const AContext: TDownloadItemsContext; out AData: TALJSONNodeW; var APaginationToken: String; var AErrorCode: Integer) of object;
      TDownloadItemsContext = Class(TObject)
      private
        // Before accessing the Owner property,
        // you must first lock the context.
        Lock: TObject;
        FreeByThread: boolean;
        OnDownloadItems: TDownloadItemsEvent;
        OnCreateItem: TCreateItemEvent;
        OnCreateItemMainContent: TCreateMainContentEvent;
        OnCreateItemLoadingContent: TCreateLoadingContentEvent;
        OnCreateItemErrorContent: TCreateErrorContentEvent;
        OnDownloadItemData: TDownloadDataEvent;
      public
        Owner: TALDynamicListBoxView;
        ItemIdType: TItemIdType;
        ItemIdNodeName: String;
        MaxItems: integer;
        PaginationToken: String;
        CustomParams: TALStringListW;
        constructor Create(const AOwner: TALDynamicListBoxView); virtual;
        destructor Destroy; override;
      end;
  public
    type
      TCreateMainContentEvent = function(const AContext: TALDynamicListBoxItem.TCreateContentContext): TALDynamicListBoxViewMainContent of object;
      TCreateNoItemsContentEvent = function(const AContext: TALDynamicListBoxItem.TCreateContentContext): TALDynamicListBoxViewNoItemsContent of object;
      TCreateContentContext = Class(TALDynamicListBoxItem.TCreateContentContext)
      private
        OnCreateNoItemsContent: TCreateNoItemsContentEvent;
      public
        constructor Create(const AOwner: TALDynamicListBoxView); reintroduce; virtual;
      end;
  {$IFDEF DEBUG}
  private
    class var DisplayDefaultRefreshRate: single;
  private
    fDebugFpsStarted: integer;
    fDebugFpsCount: integer;
    fDebugFpsStopWatch: TstopWatch;
    fDebugFpsRenderTimeStopWatch: TstopWatch;
    fDebugAverageFpsCount: integer;
    fDebugAverageFps: double;
  protected
    procedure LogFPS;
  {$ENDIF}
  private
    FOrientation: TOrientation; // 1 byte
    FItemIdType: TItemIdType; // 1 byte
    fScrollCapturedByMe: boolean; // 1 byte
    FHandleMouseEvents: Boolean; // 1 byte
    FMouseDownPos: TpointF; // 8 bytes
    FNoItemsContent: TALDynamicListBoxItemContent; // 8 bytes
    FTopBarContent: TALDynamicListBoxItemContent; // 8 bytes
    FBottomBarContent: TALDynamicListBoxItemContent; // 8 bytes
    FItems: ^TArray<TALDynamicListBoxItem>; // 8 bytes
    FItemIdNodeName: String; // 8 bytes
    FUniqueInt64ItemIds: TDictionary<Int64, boolean>; // 8 bytes | Used to deduplicate items that contain "Int64" IDs. Must be locked using LockItemIds/UnLockItemIds before used
    FUniqueTextItemIds: TDictionary<String, boolean>; // 8 bytes | Used to deduplicate items that contain "String" IDs. Must be locked using LockItemIds/UnLockItemIds before used
    FMaxItems: integer; // 4 bytes
    FPreloadItemCount: integer; // 4 bytes
    FScrollEngine: TALScrollEngine; // 8 bytes
    FScrollBar: TALDynamicListBoxScrollBar; // 8 bytes
    FFirstVisibleItemIndex: integer; // 4 bytes
    FLastVisibleItemIndex: integer; // 4 bytes
    FFirstPreloadedItemIndex: integer; // 4 bytes
    FLastPreloadedItemIndex: integer; // 4 bytes
    FPaginationToken: String; // 8 bytes
    FTriggerDownloadItemsAtIndex: integer; // 4 byte
    FDownloadItemsErrorCode: Integer; // 4 byte
    FDownloadItemsContext: TDownloadItemsContext; // 8 bytes
    FOnDownloadItems: TDownloadItemsEvent; // 16 bytes
    FOnCreateItem: TCreateItemEvent; // 16 bytes
    FOnCreateItemMainContent: TALDynamicListBoxItem.TCreateMainContentEvent; // 16 bytes
    FOnCreateItemLoadingContent: TALDynamicListBoxItem.TCreateLoadingContentEvent; // 16 bytes
    FOnCreateItemErrorContent: TALDynamicListBoxItem.TCreateErrorContentEvent; // 16 bytes
    FOnDownloadItemData: TALDynamicListBoxItem.TDownloadDataEvent; // 16 bytes
    FOnCreateNoItemsContent: TCreateNoItemsContentEvent; // 16 bytes
    function GetOnCreateMainContent: TCreateMainContentEvent;
    procedure SetOnCreateMainContent(const AValue: TCreateMainContentEvent);
    procedure SetOrientation(const AValue: TOrientation);
    procedure InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Single);
    procedure InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure InternalMouseLeave;
  protected
    procedure LockItemIds; virtual;
    procedure UnLockItemIds; virtual;
    function DownloadItems(const AForceReload: Boolean = False): boolean; virtual;
    function CreateDownloadItemsContext: TDownloadItemsContext; virtual;
    class procedure DownloadItemsBackgroundProc(var AContext: Tobject); virtual; // [MultiThread]
    class procedure DownloadItemsBackgroundProcFetchData(const AContext: TDownloadItemsContext; out AData: TALJSONNodeW; var AErrorCode: Integer); virtual; // [MultiThread]
    class procedure DownloadItemsBackgroundProcCreateItems(const AContext: TDownloadItemsContext; const AErrorCode: Integer; const AData: TALJSONNodeW; out AItems: TArray<TALDynamicListBoxItem>); virtual; // [MultiThread]
    class function DownloadItemsBackgroundProcCanProcessItems(const AContext: TDownloadItemsContext): boolean; virtual; // [MultiThread]
    procedure DownloadItemsProcessItems(const AContext: TDownloadItemsContext; const AErrorCode: Integer; var AItems: TArray<TALDynamicListBoxItem>); virtual;
    procedure DownloadItemsFinished; virtual;
    function CanDownloadItems: Boolean; virtual;
    function IsDownloadItemsRunning: Boolean;
    procedure CancelDownloadItems;
    procedure ActivateCoreContent(const AContent: TALDynamicListBoxItemContent); override;
    function CanCreateMainContent: Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseLeave; override;
    procedure ChildrenMouseDown(const AObject: TALDynamicListBoxControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseMove(const AObject: TALDynamicListBoxControl; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseUp(const AObject: TALDynamicListBoxControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseLeave(const AObject: TALDynamicListBoxControl); override;
    function GetViewportPosition: TALPointD; virtual;
    procedure SetViewportPosition(const AValue: TALPointD); virtual;
    procedure ScrollEngineChanged(Sender: TObject); virtual;
    procedure ScrollEngineStart(Sender: TObject); virtual;
    procedure ScrollEngineStop(Sender: TObject); virtual;
    procedure UpdateScrollEngineLimits; virtual;
    function FindFirstVisibleItemIndex: integer; virtual;
    function FindLastVisibleItemIndex: integer; virtual;
    function FindLastActiveItem: TALDynamicListBoxItem; virtual;
    function IsVisibleWithinListboxBounds: Boolean; override;
    property FirstVisibleItemIndex: integer read FFirstVisibleItemIndex;
    property LastVisibleItemIndex: integer read FLastVisibleItemIndex;
    property FirstPreloadedItemIndex: integer read FFirstPreloadedItemIndex;
    property LastPreloadedItemIndex: integer read FLastPreloadedItemIndex;
    procedure PaintInternal(const ACanvas: TCanvas); override;
  public
    constructor Create(const AOwner: TObject); override;
    destructor Destroy; override;
    procedure RemoveControl(const AControl: TALDynamicListBoxControl); override;
    procedure Prepare; override;
    procedure Unprepare; override;
    function FetchContent(const APreload: boolean): TALDynamicListBoxItemContent; overload; override;
    property ScrollEngine: TALScrollEngine read fScrollEngine;
    property ViewportPosition: TALPointD read GetViewportPosition;
    property ItemIdNodeName: String read FItemIdNodeName write FItemIdNodeName;
    property MaxItems: integer read FMaxItems write FMaxItems;
    property PreloadItemCount: Integer read FPreloadItemCount write FPreloadItemCount;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property OnDownloadItems: TDownloadItemsEvent read FOnDownloadItems write FOnDownloadItems; // [MultiThread]
    property OnCreateItem: TCreateItemEvent read FOnCreateItem write FOnCreateItem; // [MultiThread]
    property OnCreateItemMainContent: TALDynamicListBoxItem.TCreateMainContentEvent read FOnCreateItemMainContent write FOnCreateItemMainContent; // [MultiThread]
    property OnCreateItemLoadingContent: TALDynamicListBoxItem.TCreateLoadingContentEvent read FOnCreateItemLoadingContent write FOnCreateItemLoadingContent; // [MultiThread]
    property OnCreateItemErrorContent: TALDynamicListBoxItem.TCreateErrorContentEvent read FOnCreateItemErrorContent write FOnCreateItemErrorContent; // [MultiThread]
    property OnDownloadItemData: TALDynamicListBoxItem.TDownloadDataEvent read FOnDownloadItemData write FOnDownloadItemData; // [MultiThread]
    property OnCreateMainContent: TCreateMainContentEvent read GetOnCreateMainContent write SetOnCreateMainContent; // [MultiThread]
    property OnCreateNoItemsContent: TCreateNoItemsContentEvent read FOnCreateNoItemsContent write FOnCreateNoItemsContent; // [MultiThread]
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALDynamicListBox = class(TALControl)
  public
    type
      TCreateMainViewEvent = function(Const AListBox: TALDynamicListBox): TALDynamicListBoxView of object;
  private
    FMainView: TALDynamicListBoxView;
    FHovered: TALDynamicListBoxControl;
    FCaptured: TALDynamicListBoxControl;
    FCacheEngine: TALBufDrawableCacheEngine;
    FHasBeenPrepared: Boolean;
    FActiveScrollEnginesCount: Integer;
    FOnDownloadItems: TALDynamicListBoxView.TDownloadItemsEvent; // [MultiThread]
    FOnDownloadItemData: TALDynamicListBoxItem.TDownloadDataEvent; // [MultiThread]
    FOnCreateMainView: TCreateMainViewEvent;
    FOnCreateMainViewMainContent: TALDynamicListBoxView.TCreateMainContentEvent;
    FOnCreateMainViewLoadingContent: TALDynamicListBoxItem.TCreateLoadingContentEvent;
    FOnCreateMainViewErrorContent: TALDynamicListBoxItem.TCreateErrorContentEvent; // [MultiThread]
    FOnCreateMainViewNoItemsContent: TALDynamicListBoxView.TCreateNoItemsContentEvent;
    FOnCreateItem: TALDynamicListBoxView.TCreateItemEvent; // [MultiThread]
    FOnCreateItemMainContent: TALDynamicListBoxItem.TCreateMainContentEvent; // [MultiThread]
    FOnCreateItemLoadingContent: TALDynamicListBoxItem.TCreateLoadingContentEvent;
    FOnCreateItemErrorContent: TALDynamicListBoxItem.TCreateErrorContentEvent; // [MultiThread]
    FOnRealignItems: TALDynamicListBoxViewMainContent.TRealignEvent;
    function GetHasActiveScrollEngines: Boolean;
  protected
    function CreateMainView: TALDynamicListBoxView; virtual;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               out AControlPos: TALPointD; // AControlPos is local to the founded control
               const ACheckHitTest: Boolean = true): TALDynamicListBoxControl; overload; virtual;
    function GetControlAtPos(
               const APos: TALPointD; // APos is local to the control
               const ACheckHitTest: Boolean = true): TALDynamicListBoxControl; overload; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure SetCaptured(const Value: TALDynamicListBoxControl);
    procedure SetHovered(const Value: TALDynamicListBoxControl);
    function GetPriority(const AContext: Tobject): Int64; virtual;
    procedure DoResized; override;
    procedure Paint; override;
    property HasActiveScrollEngines: Boolean read GetHasActiveScrollEngines;
    procedure ShowErrorMessageBanner(const AErrorCode: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Prepare; virtual;
    property MainView: TALDynamicListBoxView read FMainView;
    property Captured: TALDynamicListBoxControl read FCaptured;
    property Hovered: TALDynamicListBoxControl read FHovered;
    property CacheEngine: TALBufDrawableCacheEngine read FCacheEngine;
  published
    //property Action;
    property Align;
    property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren default true;
    //property ClipParent;
    // CropCenter is use when wrapmode = FitIntoAndCrop. It's define the center of
    // crop in the source image (ex: center the result on a face instead
    // of the middle of the bounds). If CropCenter contain negative value then it's
    // indicate percentage
    //property Cursor;
    //property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    //property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    property Visible;
    property Width;
    property OnCreateMainView: TCreateMainViewEvent read FOnCreateMainView write FOnCreateMainView;
    property OnCreateMainViewMainContent: TALDynamicListBoxView.TCreateMainContentEvent read FOnCreateMainViewMainContent write FOnCreateMainViewMainContent;
    property OnCreateMainViewLoadingContent: TALDynamicListBoxItem.TCreateLoadingContentEvent read FOnCreateMainViewLoadingContent write FOnCreateMainViewLoadingContent;
    property OnCreateMainViewErrorContent: TALDynamicListBoxItem.TCreateErrorContentEvent read FOnCreateMainViewErrorContent write FOnCreateMainViewErrorContent; // [MultiThread]
    property OnCreateMainViewNoItemsContent: TALDynamicListBoxview.TCreateNoItemsContentEvent read FOnCreateMainViewNoItemsContent write FOnCreateMainViewNoItemsContent;
    property OnCreateItem: TALDynamicListBoxView.TCreateItemEvent read FOnCreateItem write FOnCreateItem; // [MultiThread]
    property OnCreateItemMainContent: TALDynamicListBoxItem.TCreateMainContentEvent read FOnCreateItemMainContent write FOnCreateItemMainContent; // [MultiThread]
    property OnCreateItemLoadingContent: TALDynamicListBoxItem.TCreateLoadingContentEvent read FOnCreateItemLoadingContent write FOnCreateItemLoadingContent;
    property OnCreateItemErrorContent: TALDynamicListBoxItem.TCreateErrorContentEvent read FOnCreateItemErrorContent write FOnCreateItemErrorContent; // [MultiThread]
    property OnDownloadItems: TALDynamicListBoxView.TDownloadItemsEvent read FOnDownloadItems write FOnDownloadItems; // [MultiThread]
    property OnDownloadItemData: TALDynamicListBoxItem.TDownloadDataEvent read FOnDownloadItemData write FOnDownloadItemData; // [MultiThread]
    property OnRealignItems: TALDynamicListBoxViewMainContent.TRealignEvent read FOnRealignItems write FOnRealignItems;
    //property OnCanFocus;
    //property OnDragEnter;
    //property OnDragLeave;
    //property OnDragOver;
    //property OnDragDrop;
    //property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    //property OnMouseEnter;
    //property OnMouseLeave;
    //property OnMouseDown;
    //property OnMouseUp;
    //property OnMouseMove;
    //property OnMouseWheel;
    //property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    //property OnPainting;
    //property OnPaint;
    //property OnResize;
    property OnResized;
  end;

Procedure ALDynamicListBoxMakeBufDrawables(const AControl: TALDynamicListBoxControl; const AEnsureDoubleBuffered: Boolean = True);

procedure Register;

implementation

uses
  System.UIConsts,
  System.Math,
  System.SysUtils,
  System.DateUtils,
  System.Math.Vectors,
  {$IFDEF DEBUG}
  System.Rtti,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Os,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  {$ENDIF}
  {$IF defined(IOS)}
  iOSapi.CoreGraphics,
  Alcinoe.iOSApi.AudioToolbox,
  {$ENDIF}
  {$IF DEFINED(ALMacOS)}
  Macapi.CoreGraphics,
  {$ENDIF}
  {$IF defined(ALSkiaEngine)}
  System.Skia.API,
  FMX.Skia.Canvas,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  FMX.Utils,
  FMX.Platform,
  FMX.StdCtrls,
  FMX.Surfaces,
  Alcinoe.GuardianThread,
  Alcinoe.HTTP.Client,
  Alcinoe.StringUtils,
  Alcinoe.Cipher,
  Alcinoe.FMX.Layouts,
  Alcinoe.HTTP.Client.Net.Pool;

{*}
var
  _ALDummyComponent: TComponent;

{**}
Type
  _TALDynamicListBoxBaseStateStyleAccessProtected = class(TALDynamicListBoxBaseStateStyle);

{********************************************************************************************************************************}
Procedure ALDynamicListBoxMakeBufDrawables(const AControl: TALDynamicListBoxControl; const AEnsureDoubleBuffered: Boolean = True);
begin

  if AControl is TALDynamicListBoxControl then begin
    var LControl := TALDynamicListBoxControl(AControl);
    if AEnsureDoubleBuffered then
      LControl.SetDoubleBuffered(true);
    LControl.MakeBufDrawable;
  end;

  for var I := 0 to AControl.ControlsCount - 1 do
    ALDynamicListBoxMakeBufDrawables(AControl.Controls[I], AEnsureDoubleBuffered);
end;

{**************}
// [MultiThread]
constructor TALDynamicListBoxControl.Create(const AOwner: TObject);
begin
  inherited create;
  FOwnerControl := nil;
  FOwnerItem := nil;
  FOwnerView := nil;
  FOwnerListBox := nil;
  SetLength(FControls, 0);
  FName := '';
  FLeft := 0;
  FTop := 0;
  var LDefaultSize := GetDefaultSize;
  FWidth := LDefaultSize.Width;
  FHeight := LDefaultSize.Height;
  FTouchTargetExpansion := TRectF.Empty;
  FPadding := CreatePadding;
  FPadding.OnChange := PaddingChangedHandler;
  FMargins := CreateMargins;
  FMargins.OnChange := MarginsChangedHandler;
  FPressedPosition := TPointF.Zero;
  FCanvas := nil;
  FIsDestroying := False;
  FIsEphemeral := False;
  FEnabled := True;
  FAbsoluteEnabled := True;
  FDisableAlign := False;
  FAlign := TALAlignLayout.None;
  FVisible := True;
  FAbsoluteVisible := True;
  FHitTest := True;
  FIsMouseOver := False;
  FPressed := False;
  FAutoCapture := False;
  FOpacity := 1;
  FDisabledOpacity := Tcontrol.DefaultDisabledOpacity;
  FAbsoluteOpacity := 1;
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  FCursor := crDefault;
  FAbsoluteCursor := crDefault;
  {$ENDIF}
  FUpdating := 0;
  FIndex := -1;
  FOnMouseEnter := nil;
  FOnMouseLeave := nil;
  FOnMouseDown := nil;
  FOnMouseMove := nil;
  FOnMouseUp := nil;
  FOnClick := nil;
  FOnPaint := nil;
  FOnPainting := nil;
  FOnResized := nil;
  if AOwner <> nil then begin
    if AOwner is TALDynamicListBox then
      FOwnerListBox := TALDynamicListBox(AOwner)
    else begin
      {$IF defined(DEBUG)}
      if not (AOwner is TALDynamicListBoxControl) then
        Raise Exception.Create('Invalid AOwner type: expected TALDynamicListBox or TALDynamicListBoxControl');
      {$ENDIF}
      SetOwnerControl(TALDynamicListBoxControl(AOwner));
    end;
  end;
end;

{******************************************}
destructor TALDynamicListBoxControl.Destroy;
begin
  SetOwnerControl(nil);
  ALFreeAndNil(FPadding);
  ALFreeAndNil(FMargins);
  for Var I := low(FControls) to High(FControls) do
    ALFreeAndNil(FControls[i]);
  inherited;
end;

{***************************************************}
procedure TALDynamicListBoxControl.BeforeDestruction;
begin
  if fisDestroying then exit;
  fisDestroying := True;
  for Var I := low(FControls) to High(FControls) do
    FControls[i].BeforeDestruction;
  inherited;
end;

{*********************************************************}
function TALDynamicListBoxControl.CreateMargins: TALBounds;
begin
  result := TALBounds.Create;
end;

{*********************************************************}
function TALDynamicListBoxControl.CreatePadding: TALBounds;
begin
  result := TALBounds.Create;
end;

{***********************************************}
procedure TALDynamicListBoxControl.DoBeginUpdate;
begin
end;

{*********************************************}
procedure TALDynamicListBoxControl.DoEndUpdate;
begin
  Realign;
  // We should repaint control, because control could receive new children in BeginUpdate - EndUpdate phase.
  Repaint;
end;

{*********************************************}
procedure TALDynamicListBoxControl.BeginUpdate;
begin
  if FUpdating = 0 then
    DoBeginUpdate;
  Inc(FUpdating);
  for var I := low(FControls) to high(FControls) do
    FControls[I].BeginUpdate;
end;

{*******************************************}
procedure TALDynamicListBoxControl.EndUpdate;
begin
  if IsUpdating then begin
    for var I := low(FControls) to high(FControls) do
      FControls[I].EndUpdate;
    Dec(FUpdating);
    if not IsUpdating then
      DoEndUpdate;
  end;
end;

{****************************************************}
function TALDynamicListBoxControl.IsUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

{****************************************************************************************}
procedure TALDynamicListBoxControl.SetOwnerControl(const Value: TALDynamicListBoxControl);
begin
  if Value <> fOwnerControl then begin
    // this will set FOwnerControl to nil
    if FOwnerControl <> nil then
      FOwnerControl.RemoveControl(self);
    // this will set FOwnerControl = Value
    if Value <> nil then
      value.AddControl(Self);
  end;
end;

{**************************************************************************************}
procedure TALDynamicListBoxControl.AddControl(const AControl: TALDynamicListBoxControl);
begin
  InsertControl(AControl, Maxint);
end;

{****************************************************************************************************************}
procedure TALDynamicListBoxControl.InsertControl(const AControl: TALDynamicListBoxControl; const AIndex: Integer);
begin
  if AControl = nil then
    Raise Exception.Create('AControl cannot be nil');
  //--
  if AControl.OwnerControl <> nil then begin
    if AControl.OwnerControl = OwnerControl then
      Raise Exception.Create('AControl is already a child of this control');
    AControl.OwnerControl.RemoveControl(AControl);
  end;
  //--
  for var I := AControl.FUpdating downto FUpdating + 1 do AControl.EndUpdate;
  for var I := AControl.FUpdating to FUpdating - 1 do AControl.BeginUpdate;
  //--
  var LIndex := Max(0, Min(AIndex, High(FControls) + 1));
  if LIndex <= High(FControls) then begin
    Setlength(FControls, length(FControls) + 1);
    ALMove(FControls[LIndex], FControls[LIndex+1], (High(FControls) - LIndex) * SizeOf(Pointer));
    For var I := LIndex + 1 to High(FControls) do
      FControls[I].FIndex := I;
  end
  else
    Setlength(FControls, length(FControls) + 1);
  FControls[LIndex] := AControl;
  AControl.FIndex := LIndex;
  AControl.FOwnerControl := self;
  AControl.ParentChanged;
  if AControl.Align <> TALAlignLayout.None then
    Realign;
end;

{*****************************************************************************************}
procedure TALDynamicListBoxControl.RemoveControl(const AControl: TALDynamicListBoxControl);
begin
  if AControl.OwnerControl <> Self then exit;
  if FIsDestroying then exit;
  if OwnerListBox <> nil then begin
    var LTmpControl := OwnerListBox.Captured;
    while (LTmpControl <> nil) do begin
      if LTmpControl = AControl then begin
        OwnerListBox.SetCaptured(nil);
        break;
      end;
      LTmpControl := LTmpControl.OwnerControl;
    end;
    //--
    LTmpControl := OwnerListBox.Hovered;
    while (LTmpControl <> nil) do begin
      if LTmpControl = AControl then begin
        OwnerListBox.SetHovered(nil);
        break;
      end;
      LTmpControl := LTmpControl.OwnerControl;
    end;
  end;
  var LIndex := AControl.Index;
  if LIndex < high(FControls) then begin
    ALMove(FControls[LIndex + 1], FControls[LIndex], (high(FControls) - LIndex) * SizeOf(Pointer));
    For var I := LIndex to High(FControls) - 1 do
      FControls[I].FIndex := I;
  end;
  Setlength(FControls, length(FControls) - 1);
  AControl.FIndex := -1;
  AControl.FOwnerControl := nil;
  If not AControl.FIsDestroying then begin
    AControl.ParentChanged;
    for var I := 1 to FUpdating do AControl.EndUpdate;
  end;
  if AControl.Align <> TALAlignLayout.None then
    Realign;
end;

{*****************************************************************************************************************}
procedure TALDynamicListBoxControl.MoveControl(const AControl: TALDynamicListBoxControl; const ANewIndex: Integer);
begin
  IF AControl.OwnerControl <> self then
    raise Exception.Create('AControl is not a child of this control');
  var LNewIndex := Max(0, Min(ANewIndex, High(FControls)));
  var LOldIndex := AControl.Index;
  if LOldIndex = LNewIndex then exit;
  if LOldIndex < LNewIndex then begin
    Move(FControls[LOldIndex + 1], FControls[LOldIndex], (LNewIndex - LOldIndex) * SizeOf(Pointer));
    FControls[LNewIndex] := AControl;
    For var I := LOldIndex to LNewIndex do
      FControls[I].FIndex := I;
  end
  else begin
    Move(FControls[LNewIndex], FControls[LNewIndex + 1], (LOldIndex - LNewIndex) * SizeOf(Pointer));
    FControls[LNewIndex] := AControl;
    For var I := LNewIndex to LOldIndex do
      FControls[I].FIndex := I;
  end;
  if AControl.Align <> TALAlignLayout.None then
    Realign;
end;

{*************************************************************************************}
function TALDynamicListBoxControl.GetControl(Index: Integer): TALDynamicListBoxControl;
begin
  {$IF defined(debug)}
  if (Index < 0) or (Index > High(FControls)) then raise Exception.Create('Index is out of bounds');
  {$ENDIF}
  result := FControls[Index];
end;

{**********************************************************}
function TALDynamicListBoxControl.GetControlsCount: Integer;
begin
  Result := Length(FControls);
end;

{************************************************}
function TALDynamicListBoxControl.GetControlAtPos(
           const APos: TALPointD; // APos is local to the control
           out AControlPos: TALPointD; // AControlPos is local to the founded control
           const ACheckHitTest: Boolean = true): TALDynamicListBoxControl;
begin
  if not ExpandedLocalRect.Contains(aPos) then begin
    AControlPos := TALPointD.Zero;
    exit(nil);
  end;
  //--
  for var i := High(FControls) downto low(FControls) do begin
    var LControl := FControls[i];
    if not LControl.Visible then continue;
    if LControl.ExpandedBoundsRect.Contains(aPos) then begin
      result := LControl.GetControlAtPos(
                  APos - LControl.BoundsRect.TopLeft,
                  AControlPos,
                  ACheckHitTest);
      if Result = nil then begin
        IF ACheckHitTest and (not LControl.HitTest) then AControlPos := TALPointD.Zero
        else begin
          result := LControl;
          AControlPos := APos - LControl.BoundsRect.TopLeft;
        end;
      end;
      exit;
    end;
  end;
  //--
  IF ACheckHitTest and (not HitTest) then begin
    Result := Nil;
    AControlPos := TALPointD.Zero
  end
  else begin
    result := Self;
    AControlPos := aPos;
  end;
end;

{************************************************}
function TALDynamicListBoxControl.GetControlAtPos(
           const APos: TALPointD;
           const ACheckHitTest: Boolean = true): TALDynamicListBoxControl;
begin
  Var LControlPos: TALPointD;
  result := GetControlAtPos(aPos, LControlPos, ACheckHitTest);
end;

{*****************************************}
procedure TALDynamicListBoxControl.Repaint;
begin
  If OwnerListBox <> nil then
    OwnerListBox.Repaint;
end;

{**********************************************}
procedure TALDynamicListBoxControl.BringToFront;
begin
  If ownerControl <> nil then
    ownerControl.MoveControl(Self,MaxInt);
end;

{********************************************}
procedure TALDynamicListBoxControl.SendToBack;
begin
  If ownerControl <> nil then
    ownerControl.MoveControl(Self,0);
end;

{**************************************************************************}
procedure TALDynamicListBoxControl.SetBounds(X, Y, AWidth, AHeight: Double);
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

{********************************************************}
function TALDynamicListBoxControl.GetBoundsRect: TALRectD;
begin
  Result := TALRectD.Create(Left, Top, Left + Width, Top + Height);
end;

{**********************************************************************}
procedure TALDynamicListBoxControl.SetBoundsRect(const Value: TALRectD);
begin
  SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);
end;

{*******************************************************}
function TALDynamicListBoxControl.GetLocalRect: TALRectD;
begin
  Result := TALRectD.Create(0, 0, Width, Height);
end;

{***************************************************************}
function TALDynamicListBoxControl.GetExpandedLocalRect: TALRectD;
begin
  result := TALRectD.Create(
              0-TouchTargetExpansion.Left,
              0-TouchTargetExpansion.top,
              Width+TouchTargetExpansion.right,
              Height+TouchTargetExpansion.bottom);
end;

{**********************************************************}
function TALDynamicListBoxControl.GetAbsoluteRect: TALRectD;
begin
  Result := TALRectD.Create(LocalToAbsolute(LocalRect));
end;

{****************************************************************}
function TALDynamicListBoxControl.GetExpandedBoundsRect: TALRectD;
begin
  result := TALRectD.Create(
              Left-TouchTargetExpansion.Left,
              top-TouchTargetExpansion.top,
              Right+TouchTargetExpansion.right,
              Bottom+TouchTargetExpansion.bottom);
end;

{**********************************************************************}
procedure TALDynamicListBoxControl.SetPosition(const AValue: TALPointD);
begin
  SetBounds(AValue.X, AValue.Y, FWidth, FHeight);
end;

{********************************************************************}
procedure TALDynamicListBoxControl.SetPosition(const AValue: TPointf);
begin
  SetBounds(AValue.X, AValue.Y, FWidth, FHeight);
end;

{****************************************************************}
procedure TALDynamicListBoxControl.SetSize(const ASize: TALSizeD);
begin
  SetBounds(Fleft, FTop, ASize.Width, ASize.Height);
end;

{**************************************************************}
procedure TALDynamicListBoxControl.SetSize(const ASize: TSizeF);
begin
  SetBounds(Fleft, FTop, ASize.Width, ASize.Height);
end;

{************************************************************************}
procedure TALDynamicListBoxControl.SetSize(const AWidth, AHeight: Double);
begin
  SetBounds(Fleft, FTop, AWidth, AHeight);
end;

{**************************************************************}
procedure TALDynamicListBoxControl.SetLeft(const Value: Double);
begin
  SetBounds(Value, FTop, FWidth, FHeight);
end;

{*************************************************************}
procedure TALDynamicListBoxControl.SetTop(const Value: Double);
begin
  SetBounds(FLeft, Value, FWidth, FHeight);
end;

{***************************************************************}
procedure TALDynamicListBoxControl.SetWidth(const Value: Double);
begin
  SetBounds(FLeft, FTop, Value, FHeight);
end;

{****************************************************************}
procedure TALDynamicListBoxControl.SetHeight(const Value: Double);
begin
  SetBounds(FLeft, FTop, FWidth, Value);
end;

{*************************************************}
function TALDynamicListBoxControl.GetRight: Double;
begin
  Result := Left + Width;
end;

{**************************************************}
function TALDynamicListBoxControl.GetBottom: Double;
begin
  Result := Top + Height;
end;

{************************************************************************************}
function TALDynamicListBoxControl.AbsoluteToLocal(const APoint: TALPointD): TALPointD;
begin
  if OwnerControl <> nil then Result := OwnerControl.AbsoluteToLocal(APoint)
  else if OwnerListbox <> nil then begin
    Result := APoint;
    Result.Offset(-OwnerListbox.AbsoluteToLocal(TPointF.Zero));
  end
  else Result := APoint;
  Result.Offset(-Left, -Top);
end;

{*********************************************************************************}
function TALDynamicListBoxControl.AbsoluteToLocal(const ARect: TALRectD): TALRectD;
begin
  if OwnerControl <> nil then Result := OwnerControl.AbsoluteToLocal(ARect)
  else if OwnerListbox <> nil then begin
    Result := ARect;
    Result.Offset(-OwnerListbox.AbsoluteToLocal(TPointF.Zero));
  end
  else Result := ARect;
  Result.Offset(-Left, -Top);
end;

{**********************************************************************************}
function TALDynamicListBoxControl.AbsoluteToLocal(const APoint: TPointF): TALPointD;
begin
  Result := AbsoluteToLocal(TALPointD.Create(APoint));
end;

{*******************************************************************************}
function TALDynamicListBoxControl.AbsoluteToLocal(const ARect: TRectF): TALRectD;
begin
  Result := AbsoluteToLocal(TALRectD.Create(ARect));
end;

{************************************************************************************}
function TALDynamicListBoxControl.LocalToAbsolute(const APoint: TALPointD): TALPointD;
begin
  Result := APoint;
  Result.Offset(left, top);
  if OwnerControl <> nil then result := OwnerControl.LocalToAbsolute(Result)
  else if OwnerListBox <> nil then Result.Offset(OwnerListBox.LocalToAbsolute(TPointF.Zero));
end;

{*********************************************************************************}
function TALDynamicListBoxControl.LocalToAbsolute(const ARect: TALRectD): TALRectD;
begin
  Result := ARect;
  Result.Offset(left, top);
  if OwnerControl <> nil then result := OwnerControl.LocalToAbsolute(Result)
  else if OwnerListBox <> nil then Result.Offset(OwnerListBox.LocalToAbsolute(TPointF.Zero));
end;

{**********************************************************************************}
function TALDynamicListBoxControl.LocalToAbsolute(const APoint: TPointF): TALPointD;
begin
  result := LocalToAbsolute(TALPointD.Create(APoint));
end;

{*******************************************************************************}
function TALDynamicListBoxControl.LocalToAbsolute(const ARect: TRectF): TALRectD;
begin
  result := LocalToAbsolute(TALRectD.Create(ARect));
end;

{**************************************************************************}
function TALDynamicListBoxControl.PointInObjectLocal(X, Y: Double): Boolean;
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.PointInObjectLocal was not updated and adjust the IFDEF'}
  {$ENDIF}
  Result := (X >= (0 - TouchTargetExpansion.Left)) and
            (X <= (Width + TouchTargetExpansion.Right)) and
            (Y >= (0 - TouchTargetExpansion.Top)) and
            (Y <= (Height + TouchTargetExpansion.Bottom));
end;

{*******************************************************}
function TALDynamicListBoxControl.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(50, 50);
end;

{*******************************************************************************}
procedure TALDynamicListBoxControl.SetTouchTargetExpansion(const AValue: TRectF);
begin
  FTouchTargetExpansion := AValue;
end;

{******************************************************************}
procedure TALDynamicListBoxControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    EnabledChanged;
  end;
end;

{********************************************************}
procedure TALDynamicListBoxControl.refreshAbsoluteEnabled;
begin
  var LNewAbsoluteEnabled: Boolean;
  if (OwnerControl <> nil) and (not FOwnerControl.AbsoluteEnabled) then LNewAbsoluteEnabled := False
  else LNewAbsoluteEnabled := Enabled;
  if LNewAbsoluteEnabled <> FAbsoluteEnabled then begin
    FAbsoluteEnabled := LNewAbsoluteEnabled;
    For var I := Low(FControls) to high(FControls) do
      FControls[i].RefreshAbsoluteEnabled;
    Repaint;
  end;
end;

{******************************************************************}
procedure TALDynamicListBoxControl.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then begin
    FVisible := Value;
    VisibleChanged;
  end;
end;

{********************************************************************}
function TALDynamicListBoxControl.GetFirstVisibleObjectIndex: Integer;
begin
  Result := low(Fcontrols);
end;

{*******************************************************************}
function TALDynamicListBoxControl.GetLastVisibleObjectIndex: Integer;
begin
  Result := high(Fcontrols);
end;

{**********************************************************************}
function TALDynamicListBoxControl.IsVisibleWithinListboxBounds: Boolean;
begin
  Result := AbsoluteVisible;
  if not result then exit;
  if OwnerListbox <> nil then
    Result := OwnerListbox.AbsoluteRect.IntersectsWith(LocalToAbsolute(LocalRect).ReducePrecision);
end;

{********************************************************}
procedure TALDynamicListBoxControl.RefreshAbsoluteVisible;
begin
  var LNewAbsoluteVisible: Boolean;
  if (OwnerControl <> nil) and (not FOwnerControl.AbsoluteVisible) then LNewAbsoluteVisible := False
  else LNewAbsoluteVisible := Visible;
  if LNewAbsoluteVisible <> FAbsoluteVisible then begin
    FAbsoluteVisible := LNewAbsoluteVisible;
    For var I := Low(FControls) to high(FControls) do
      FControls[i].RefreshAbsoluteVisible;
    Repaint;
  end;
end;

{*****************************************************************}
procedure TALDynamicListBoxControl.SetOpacity(const Value: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.SetOpacity was not updated and adjust the IFDEF'}
  {$ENDIF}
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    if FOpacity < 0 then FOpacity := 0;
    if FOpacity > 1 then FOpacity := 1;
    RefreshAbsoluteOpacity;
  end;
end;

{*************************************************************************}
procedure TALDynamicListBoxControl.SetDisabledOpacity(const Value: Single);
begin
  if not SameValue(FDisabledOpacity, Value, TEpsilon.Scale) then begin
    FDisabledOpacity := Value;
    if FDisabledOpacity < 0 then FDisabledOpacity := 0;
    if FDisabledOpacity > 1 then FDisabledOpacity := 1;
    if not Enabled then
      RefreshAbsoluteOpacity;
  end;
end;

{********************************************************}
procedure TALDynamicListBoxControl.RefreshAbsoluteOpacity;
begin
  var LNewAbsoluteOpacity: Single;
  if OwnerControl <> nil then LNewAbsoluteOpacity := Opacity * OwnerControl.AbsoluteOpacity
  else LNewAbsoluteOpacity := Opacity;
  if not Enabled then
    LNewAbsoluteOpacity := LNewAbsoluteOpacity * DisabledOpacity;
  if not SameValue(LNewAbsoluteOpacity, FAbsoluteOpacity, TEpsilon.Scale) then begin
    FAbsoluteOpacity := LNewAbsoluteOpacity;
    For var I := Low(FControls) to high(FControls) do
      FControls[i].RefreshAbsoluteOpacity;
    Repaint;
  end;
end;

{***************************************************}
function TALDynamicListBoxControl.GetCursor: TCursor;
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  Result := FCursor;
  {$ELSE}
  Result := crDefault;
  {$ENDIF}
end;

{*****************************************************************}
procedure TALDynamicListBoxControl.SetCursor(const Value: TCursor);
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  if FCursor <> Value then begin
    FCursor := Value;
    RefreshAbsoluteCursor;
  end;
  {$ENDIF}
end;

{***********************************************************}
function TALDynamicListBoxControl.GetAbsoluteCursor: TCursor;
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  Result := FAbsoluteCursor;
  {$ELSE}
  Result := crDefault;
  {$ENDIF}
end;

{*******************************************************}
procedure TALDynamicListBoxControl.RefreshAbsoluteCursor;
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  var LNewAbsoluteCursor: TCursor;
  if (Cursor = crDefault) and (OwnerControl <> nil) then LNewAbsoluteCursor := OwnerControl.AbsoluteCursor
  else LNewAbsoluteCursor := Cursor;
  if LNewAbsoluteCursor <> FAbsoluteCursor then begin
    FAbsoluteCursor := LNewAbsoluteCursor;
    if IsMouseOver and (OwnerListBox <> nil) then
      OwnerListBox.Cursor := FAbsoluteCursor;
    For var I := Low(FControls) to high(FControls) do
      FControls[i].RefreshAbsoluteCursor;
  end;
  {$ENDIF}
end;

{***********************************************************************}
procedure TALDynamicListBoxControl.SetAlign(const Value: TALAlignLayout);
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
    If OwnerControl <> nil then
      OwnerControl.Realign;
  end;
end;

{***********************************************************}
function TALDynamicListBoxControl.GetForm: TCommonCustomForm;
begin
  if OwnerListBox <> nil then
    Result := OwnerListBox.Form
  else
    result := nil;
end;

{****************************************************************************}
function TALDynamicListBoxControl.GetPriority(const AContext: Tobject): Int64;
begin
  if OwnerItem <> nil then result := OwnerItem.GetPriority(AContext)
  else if OwnerView <> nil then Result := OwnerView.GetPriority(AContext)
  else if OwnerListBox <> nil then result := OwnerListBox.GetPriority(AContext)
  else result := TALNetHttpClientPool.Instance.PriorityStartingPoint;
end;

{**************************************************************}
function TALDynamicListBoxControl.FillTextFlags: TFillTextFlags;
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.FillTextFlags was not updated and adjust the IFDEF'}
  {$ENDIF}
  if (Form <> nil) and (Form.BiDiMode = bdRightToLeft) then
    Result := [TFillTextFlag.RightToLeft]
  else
    Result := [];
end;

{*********************************************************************}
procedure TALDynamicListBoxControl.SetPadding(const AValue: TALBounds);
begin
  FPadding.Assign(AValue);
end;

{*********************************************************************}
procedure TALDynamicListBoxControl.SetMargins(const AValue: TALBounds);
begin
  FMargins.Assign(AValue);
end;

{************************************************************************}
procedure TALDynamicListBoxControl.PaddingChangedHandler(Sender: TObject);
begin
  PaddingChanged;
end;

{************************************************************************}
procedure TALDynamicListBoxControl.MarginsChangedHandler(Sender: TObject);
begin
  MarginsChanged;
end;

{************************************************}
procedure TALDynamicListBoxControl.PaddingChanged;
begin
  Realign;
end;

{************************************************}
procedure TALDynamicListBoxControl.MarginsChanged;
begin
  if OwnerControl <> nil then
    OwnerControl.Realign;
end;

{************************************************}
procedure TALDynamicListBoxControl.EnabledChanged;
begin
  RefreshAbsoluteEnabled;
  RefreshAbsoluteOpacity;
end;

{************************************************}
procedure TALDynamicListBoxControl.VisibleChanged;
begin
  RefreshAbsoluteVisible;
  if (Align <> TALAlignLayout.None) and (OwnerControl <> nil) then
    OwnerControl.Realign;
end;

{***********************************************}
procedure TALDynamicListBoxControl.ParentChanged;
begin
  RefreshAbsoluteCursor;
  RefreshAbsoluteOpacity;
  RefreshAbsoluteEnabled;
  RefreshAbsoluteVisible;
  AncestorParentChanged;
end;

{*******************************************************}
procedure TALDynamicListBoxControl.AncestorParentChanged;
begin
  If FOwnerControl = nil then begin
    FOwnerItem := nil;
    FOwnerView := nil;
    FOwnerListBox := nil;
  end
  else begin
    if FOwnerControl is TALDynamicListBoxView then FOwnerView := TALDynamicListBoxView(FOwnerControl)
    else FOwnerView := FOwnerControl.FOwnerView;
    //--
    if FOwnerControl is TALDynamicListBoxItem then FOwnerItem := TALDynamicListBoxItem(FOwnerControl)
    else FOwnerItem := FOwnerControl.FOwnerItem;
    //--
    FOwnerListbox := FOwnerControl.FOwnerListbox;
  end;
  //--
  for var I := low(FControls) to high(FControls) do
    FControls[I].AncestorParentChanged;
end;

{*************************************************}
procedure TALDynamicListBoxControl.PositionChanged;
begin
  if (Align <> TALAlignLayout.None) and (OwnerControl <> nil) then
    OwnerControl.Realign;
  Repaint;
end;

{*********************************************}
procedure TALDynamicListBoxControl.SizeChanged;
begin
  if (Align <> TALAlignLayout.None) and (OwnerControl <> nil) then
    OwnerControl.Realign;
  Realign;
  Repaint;
  DoResized
end;

{*******************************************}
procedure TALDynamicListBoxControl.DoResized;
begin
  if Assigned(FOnResized) then
    FOnResized(Self);
end;

{*****************************************}
procedure TALDynamicListBoxControl.Realign;
begin
  {$IFNDEF ALCompilerVersionSupported122}
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

{*******************************************}
procedure TALDynamicListBoxControl.DoRealign;
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
    for var I := low(FControls) to High(FControls) do begin
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
    for var I := low(FControls) to High(FControls) do begin
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
    for var I := low(FControls) to High(FControls) do
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

{************************************************************}
function TALDynamicListBoxControl.GetUnalignedSpace: TALRectD;
begin

  //Initialize LClientRect based on bounds and padding
  Result := TALRectD.Create(
              Padding.Left, {Left}
              Padding.Top, {Top}
              Width - Padding.Right, {Right}
              Height - Padding.Bottom); {Bottom}

  for var I := low(FControls) to High(FControls) do begin
    var LControl := FControls[i];
    if not LControl.Visible then Continue;
    case LControl.Align of

      {$REGION 'Top'}
      TALAlignLayout.Top,
      TALAlignLayout.TopCenter,
      TALAlignLayout.TopLeft,
      TALAlignLayout.TopRight,
      TALAlignLayout.MostTop,
      TALAlignLayout.MostTopCenter,
      TALAlignLayout.MostTopLeft,
      TALAlignLayout.MostTopRight:
        Result.Top := Result.Top + LControl.Margins.Top + LControl.Height + LControl.Margins.Bottom;
      {$ENDREGION}

      {$REGION 'Left'}
      TALAlignLayout.Left,
      TALAlignLayout.LeftCenter,
      TALAlignLayout.LeftTop,
      TALAlignLayout.LeftBottom,
      TALAlignLayout.MostLeft,
      TALAlignLayout.MostLeftCenter,
      TALAlignLayout.MostLeftTop,
      TALAlignLayout.MostLeftBottom:
        Result.Left := Result.Left + LControl.Margins.Left + LControl.Width + LControl.Margins.Right;
      {$ENDREGION}

      {$REGION 'Right'}
      TALAlignLayout.Right,
      TALAlignLayout.RightCenter,
      TALAlignLayout.RightTop,
      TALAlignLayout.RightBottom,
      TALAlignLayout.MostRight,
      TALAlignLayout.MostRightCenter,
      TALAlignLayout.MostRightTop,
      TALAlignLayout.MostRightBottom:
        Result.Right := Result.Right - LControl.Margins.Right - LControl.Width - LControl.Margins.Left;
      {$ENDREGION}

      {$REGION 'Bottom'}
      TALAlignLayout.Bottom,
      TALAlignLayout.BottomCenter,
      TALAlignLayout.BottomLeft,
      TALAlignLayout.BottomRight,
      TALAlignLayout.MostBottom,
      TALAlignLayout.MostBottomCenter,
      TALAlignLayout.MostBottomLeft,
      TALAlignLayout.MostBottomRight:
        Result.Bottom := Result.Bottom - LControl.Margins.Bottom - LControl.Height - LControl.Margins.Top;
      {$ENDREGION}

      {$REGION 'Client'}
      TALAlignLayout.Client:
        Exit(TalRectD.Empty);
      {$ENDREGION}

      {$REGION 'None,Center,VertCenter,HorzCenter,Horizontal,Vertical'}
      TALAlignLayout.None,
      TALAlignLayout.Center,
      TALAlignLayout.VertCenter,
      TALAlignLayout.HorzCenter,
      TALAlignLayout.Horizontal,
      TALAlignLayout.Vertical:;
      {$ENDREGION}

      {$REGION 'Unknown'}
      else
        raise Exception.Create('Error D2EFEFBE-8076-4F8A-BDB5-C733157D4289');
      {$ENDREGION}

    end;
  end;

end;

{********************************************}
procedure TALDynamicListBoxControl.MouseEnter;
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseEnter was not updated and adjust the IFDEF'}
  {$ENDIF}
  FIsMouseOver := True;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
  if fOwnerControl <> nil then fOwnerControl.ChildrenMouseEnter(Self); // https://quality.embarcadero.com/browse/RSP-24397
end;

{********************************************}
procedure TALDynamicListBoxControl.MouseLeave;
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseLeave was not updated and adjust the IFDEF'}
  {$ENDIF}
  FIsMouseOver := False;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
  if fOwnerControl <> nil then fOwnerControl.ChildrenMouseLeave(Self); // https://quality.embarcadero.com/browse/RSP-24397
end;

{***************************************************************************************************}
procedure TALDynamicListBoxControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseDown was not updated and adjust the IFDEF'}
  {$ENDIF}
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  if fOwnerControl <> nil then fOwnerControl.ChildrenMouseDown(Self, Button, Shift, X, Y); // https://quality.embarcadero.com/browse/RSP-24397
  if FAutoCapture then
    Capture;
  if Button = TMouseButton.mbLeft then begin
    FPressed := True;
    FPressedPosition := TPointF.Create(X, Y);
  end;
end;

{*****************************************************************************}
procedure TALDynamicListBoxControl.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseMove was not updated and adjust the IFDEF'}
  {$ENDIF}
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
  if fOwnerControl <> nil then fOwnerControl.ChildrenMouseMove(Self, Shift, X, Y); // https://quality.embarcadero.com/browse/RSP-24397
end;

{*************************************************************************************************}
procedure TALDynamicListBoxControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseUp was not updated and adjust the IFDEF'}
  {$ENDIF}
  ReleaseCapture;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  if fOwnerControl <> nil then fOwnerControl.ChildrenMouseUp(Self, Button, Shift, X, Y); // https://quality.embarcadero.com/browse/RSP-24397
  FPressed := False;
end;

{****************************************************************************************************}
procedure TALDynamicListBoxControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl.MouseClick was not updated and adjust the IFDEF'}
  {$ENDIF}
  if AbsoluteEnabled and FPressed and PointInObjectLocal(X, Y) then begin
    Click;
    FPressed := False;
  end;
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicListBoxControl.ChildrenMouseDown(const AObject: TALDynamicListBoxControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if fOwnerControl <> nil then
    fOwnerControl.ChildrenMouseDown(AObject, Button, Shift, X, Y);
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicListBoxControl.ChildrenMouseMove(const AObject: TALDynamicListBoxControl; Shift: TShiftState; X, Y: Single);
begin
  if fOwnerControl <> nil then
    fOwnerControl.ChildrenMouseMove(AObject, Shift, X, Y);
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicListBoxControl.ChildrenMouseUp(const AObject: TALDynamicListBoxControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if fOwnerControl <> nil then
    fOwnerControl.ChildrenMouseUp(AObject, Button, Shift, X, Y);
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicListBoxControl.ChildrenMouseEnter(const AObject: TALDynamicListBoxControl);
begin
  if fOwnerControl <> nil then
    fOwnerControl.ChildrenMouseEnter(AObject);
end;

{*************************************************}
// https://quality.embarcadero.com/browse/RSP-24397
procedure TALDynamicListBoxControl.ChildrenMouseLeave(const AObject: TALDynamicListBoxControl);
begin
  if fOwnerControl <> nil then
    fOwnerControl.ChildrenMouseLeave(AObject);
end;

{***************************************}
procedure TALDynamicListBoxControl.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self)
end;

{*****************************************}
procedure TALDynamicListBoxControl.Capture;
begin
  if OwnerListBox <> nil then
    OwnerListBox.SetCaptured(Self);
end;

{************************************************}
procedure TALDynamicListBoxControl.ReleaseCapture;
begin
  if (OwnerListBox <> nil) and (OwnerListBox.Captured = Self) then
    OwnerListBox.SetCaptured(nil);
end;

{***********************************************************************}
procedure TALDynamicListBoxControl.PaintInternal(const ACanvas: TCanvas);
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

{******************************************}
procedure TALDynamicListBoxControl.Painting;
begin
  if Assigned(FOnPainting) then
    FOnPainting(Self, Canvas);
end;

{***************************************}
procedure TALDynamicListBoxControl.Paint;
begin
end;

{***********************************************************}
function TALDynamicListBoxControl.PaintChildrenOnly: Boolean;
begin
  result := False;
end;

{***********************************************}
procedure TALDynamicListBoxControl.PaintChildren;
begin
  if length(FControls) > 0 then begin
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
          if PaintChildrenOnly then begin
            var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(Left + LControl.left, Top + LControl.Top);
            Canvas.SetMatrix(LMatrix);
          end
          else begin
            var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(LControl.left, LControl.Top);
            Canvas.SetMatrix(LMatrix);
          end;
        end;
        LControl.PaintInternal(Canvas);
      end;
    finally
      Canvas.SetMatrix(LSavedMatrix);
    end;
  end;
end;

{********************************************}
procedure TALDynamicListBoxControl.AfterPaint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
end;

//////////////////////////////////////////////
/// THE CODE BELOW WAS AUTO-GENERATED FROM ///
/// <ALCINOE>\Tools\CodeBuilder.           ///
//////////////////////////////////////////////

{$REGION 'AUTO-GENERATED'}

{*************************************************************************}
constructor TALDynamicListBoxExtendedControl.Create(const AOwner: TObject);
begin
  inherited;
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if MarginsChanged is not implemented in FMX.Controls.TControl and adjust the IFDEF'}
  {$ENDIF}
  //FFormerMarginsChangedHandler := Margins.OnChange;
  //Margins.OnChange := MarginsChangedHandler;
  //Size.SetPlatformDefaultWithoutNotification(False);
  //FForm := nil;
  //FOwnerControl := nil;
  FControlAbsolutePosAtMouseDown := TALPointD.zero;
  FScale := 1;
  //FFocusOnMouseDown := False;
  //FFocusOnMouseUp := False;
  FMouseDownAtLowVelocity := True;
  // Double-clicks, or double-taps, are rarely used in mobile design due to
  // touch screen challenges and user experience considerations. Mobile devices
  // favor simpler, more intuitive gestures like swiping and pinching, which are
  // better suited to smaller screens and prevent confusion with similar
  // actions. Consequently, functionalities often tied to double-clicks on
  // desktops are handled by different gestures or interface elements in
  // mobile apps, leading to a more user-friendly experience.
  //FDisableDoubleClickHandling := True;
  FIsPixelAlignmentEnabled := True;
  //FAlign := TALAlignLayout.None;
  FIsSetBoundsLocked := False;
  FTextUpdating := False;
  FAutoSize := False;
  FIsAdjustingSize := False;
  FAdjustSizeOnEndUpdate := False;
end;

{**************************************************}
destructor TALDynamicListBoxExtendedControl.Destroy;
begin
  ClearBufDrawable;
  inherited;
end;

{***************************************************}
procedure TALDynamicListBoxExtendedControl.EndUpdate;
begin
  if IsUpdating then
  begin
    if not FTextUpdating then begin
      BeginTextUpdate;
      Inherited;
      EndTextUpdate;
    end
    else
      Inherited;
  end;
end;

{*************************************************}
// Unfortunately, the way BeginUpdate/EndUpdate and
// Realign are implemented is not very efficient for TALDynamicListBoxText.
// When calling EndUpdate, it first propagates to the most
// deeply nested children in this hierarchy:
//   Control1
//     Control2
//       AlText1
// This means when Control1.EndUpdate is called,
// it executes in the following order:
//       AlText1.EndUpdate => AdjustSize and Realign
//     Control2.EndUpdate => Realign and potentially calls AlText1.AdjustSize again
//   Control1.EndUpdate => Realign and possibly triggers AlText1.AdjustSize once more
// This poses a problem since the BufDrawable will be
// recalculated multiple times.
// To mitigate this, we can use:
//   BeginTextUpdate;
//   EndUpdate;
//   EndTextUpdate;
procedure TALDynamicListBoxExtendedControl.BeginTextUpdate;
begin
  FTextUpdating := True;
  for var I := 0 to ControlsCount - 1 do
    if Controls[i] is TALDynamicListBoxControl then
      TALDynamicListBoxControl(Controls[i]).BeginTextUpdate;
end;

{*******************************************************}
procedure TALDynamicListBoxExtendedControl.EndTextUpdate;
begin
  FTextUpdating := False;
  for var I := 0 to ControlsCount - 1 do
    if Controls[i] is TALDynamicListBoxControl then
      TALDynamicListBoxControl(Controls[i]).EndTextUpdate;
end;

{**************************************************}
//procedure TALDynamicListBoxExtendedControl.Loaded;
//begin
//  {$IF not DEFINED(ALDPK)}
//  if IsPixelAlignmentEnabled then
//    AlignToPixel;
//  {$ENDIF}
//  Inherited;
//  AdjustSize;
//end;

{******************************************************************}
//function TALDynamicListBoxExtendedControl.IsOwnerLoading: Boolean;
//begin
//  result := (Owner <> nil) and
//            (csloading in Owner.ComponentState);
//end;

{****************************************************************}
//function TALDynamicListBoxExtendedControl.IsSizeStored: Boolean;
//begin
//  var LDefaultSize := GetDefaultSize;
//  result := (not SameValue(LDefaultSize.cx, Size.Size.cx, TEpsilon.Position)) or
//            (not SameValue(LDefaultSize.cy, Size.Size.cy, TEpsilon.Position));
//end;

{*******************************************************************}
//function TALDynamicListBoxExtendedControl.GetAlign: TALAlignLayout;
//begin
//  Result := FAlign;
//end;

{*********************************************************************************}
//procedure TALDynamicListBoxExtendedControl.SetAlign(const Value: TALAlignLayout);
//begin
//  If FAlign <> Value then begin
//    FAlign := Value;
//    var LLegacyAlign: TAlignLayout;
//    Case Value of
//      TALAlignLayout.None: LLegacyAlign := TAlignLayout.None;
//      TALAlignLayout.Top: LLegacyAlign := TAlignLayout.Top;
//      TALAlignLayout.Left: LLegacyAlign := TAlignLayout.Left;
//      TALAlignLayout.Right: LLegacyAlign := TAlignLayout.Right;
//      TALAlignLayout.Bottom: LLegacyAlign := TAlignLayout.Bottom;
//      TALAlignLayout.MostTop: LLegacyAlign := TAlignLayout.MostTop;
//      TALAlignLayout.MostBottom: LLegacyAlign := TAlignLayout.MostBottom;
//      TALAlignLayout.MostLeft: LLegacyAlign := TAlignLayout.MostLeft;
//      TALAlignLayout.MostRight: LLegacyAlign := TAlignLayout.MostRight;
//      TALAlignLayout.Client: LLegacyAlign := TAlignLayout.Client;
//      //TALAlignLayout.Contents: LLegacyAlign := TAlignLayout.Contents;
//      TALAlignLayout.Center: LLegacyAlign := TAlignLayout.Center;
//      TALAlignLayout.VertCenter: LLegacyAlign := TAlignLayout.VertCenter;
//      TALAlignLayout.HorzCenter: LLegacyAlign := TAlignLayout.HorzCenter;
//      TALAlignLayout.Horizontal: LLegacyAlign := TAlignLayout.Horizontal;
//      TALAlignLayout.Vertical: LLegacyAlign := TAlignLayout.Vertical;
//      //TALAlignLayout.Scale: LLegacyAlign := TAlignLayout.Scale;
//      //TALAlignLayout.Fit: LLegacyAlign := TAlignLayout.Fit;
//      //TALAlignLayout.FitLeft: LLegacyAlign := TAlignLayout.FitLeft;
//      //TALAlignLayout.FitRight: LLegacyAlign := TAlignLayout.FitRight;
//      TALAlignLayout.TopCenter: LLegacyAlign := TAlignLayout.Top;
//      TALAlignLayout.TopLeft: LLegacyAlign := TAlignLayout.Top;
//      TALAlignLayout.TopRight: LLegacyAlign := TAlignLayout.Top;
//      TALAlignLayout.LeftCenter: LLegacyAlign := TAlignLayout.Left;
//      TALAlignLayout.LeftTop: LLegacyAlign := TAlignLayout.Left;
//      TALAlignLayout.LeftBottom: LLegacyAlign := TAlignLayout.Left;
//      TALAlignLayout.RightCenter: LLegacyAlign := TAlignLayout.Right;
//      TALAlignLayout.RightTop: LLegacyAlign := TAlignLayout.Right;
//      TALAlignLayout.RightBottom: LLegacyAlign := TAlignLayout.Right;
//      TALAlignLayout.BottomCenter: LLegacyAlign := TAlignLayout.Bottom;
//      TALAlignLayout.BottomLeft: LLegacyAlign := TAlignLayout.Bottom;
//      TALAlignLayout.BottomRight: LLegacyAlign := TAlignLayout.Bottom;
//      TALAlignLayout.MostTopCenter: LLegacyAlign := TAlignLayout.MostTop;
//      TALAlignLayout.MostTopLeft: LLegacyAlign := TAlignLayout.MostTop;
//      TALAlignLayout.MostTopRight: LLegacyAlign := TAlignLayout.MostTop;
//      TALAlignLayout.MostLeftCenter: LLegacyAlign := TAlignLayout.MostLeft;
//      TALAlignLayout.MostLeftTop: LLegacyAlign := TAlignLayout.MostLeft;
//      TALAlignLayout.MostLeftBottom: LLegacyAlign := TAlignLayout.MostLeft;
//      TALAlignLayout.MostRightCenter: LLegacyAlign := TAlignLayout.MostRight;
//      TALAlignLayout.MostRightTop: LLegacyAlign := TAlignLayout.MostRight;
//      TALAlignLayout.MostRightBottom: LLegacyAlign := TAlignLayout.MostRight;
//      TALAlignLayout.MostBottomCenter: LLegacyAlign := TAlignLayout.MostBottom;
//      TALAlignLayout.MostBottomLeft: LLegacyAlign := TAlignLayout.MostBottom;
//      TALAlignLayout.MostBottomRight: LLegacyAlign := TAlignLayout.MostBottom;
//      else Raise Exception.Create('Error D527A470-23AC-4E3C-BCC5-4C2DB578A691');
//    end;
//    Inherited SetAlign(LLegacyAlign);
//  end;
//end;

{*****************************************************}
procedure TALDynamicListBoxExtendedControl.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if FAdjustSizeOnEndUpdate then
    AdjustSize;
end;

{***************************************************}
procedure TALDynamicListBoxExtendedControl.DoResized;
begin
  {$IF defined(debug)}
  //ALLog(ClassName+'.DoResized', 'Name: ' + Name);
  {$ENDIF}
  inherited;
  AdjustSize;
end;

{***************************************************}
procedure TALDynamicListBoxExtendedControl.DoRealign;
begin
  {$IF defined(debug)}
  //ALLog(ClassName+'.DoRealign', 'Name: ' + Name);
  {$ENDIF}

  {$IFNDEF ALCompilerVersionSupported122}
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

{*******************************************************************************************}
procedure TALDynamicListBoxExtendedControl.SetFixedSizeBounds(X, Y, AWidth, AHeight: Single);
begin
  if TNonReentrantHelper.EnterSection(FIsSetBoundsLocked) then begin
    try

      {$IFNDEF ALCompilerVersionSupported122}
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
      //If (integer(FAlign) >= integer(TALAlignLayout.TopCenter)) and
      //   (integer(FAlign) <= integer(TALAlignLayout.MostBottomRight)) and
      //   (OwnerControl <> nil) and                              // FDisableAlign = true mean that SetBounds was called by
      //   (OwnerControl.FDisableAlign) // AlignObjects procedure inside inherited DoRealign
      //then begin
      //  case FAlign of
      //    TALAlignLayout.TopCenter,
      //    TALAlignLayout.BottomCenter,
      //    TALAlignLayout.MostTopCenter,
      //    TALAlignLayout.MostBottomCenter: begin
      //      X := X + ((AWidth - Width) / 2);
      //      AWidth := Width;
      //    end;
      //    TALAlignLayout.TopLeft,
      //    TALAlignLayout.BottomLeft,
      //    TALAlignLayout.MostTopLeft,
      //    TALAlignLayout.MostBottomLeft: begin
      //      AWidth := Width;
      //    end;
      //    TALAlignLayout.TopRight,
      //    TALAlignLayout.BottomRight,
      //    TALAlignLayout.MostTopRight,
      //    TALAlignLayout.MostBottomRight: begin
      //      X := X + (AWidth - Width);
      //      AWidth := Width;
      //    end;
      //    TALAlignLayout.LeftCenter,
      //    TALAlignLayout.RightCenter,
      //    TALAlignLayout.MostLeftCenter,
      //    TALAlignLayout.MostRightCenter: begin
      //      Y := Y + ((AHeight - Height) / 2);
      //      AHeight := Height;
      //    end;
      //    TALAlignLayout.LeftTop,
      //    TALAlignLayout.RightTop,
      //    TALAlignLayout.MostLeftTop,
      //    TALAlignLayout.MostRightTop: begin
      //      AHeight := Height;
      //    end;
      //    TALAlignLayout.LeftBottom,
      //    TALAlignLayout.RightBottom,
      //    TALAlignLayout.MostLeftBottom,
      //    TALAlignLayout.MostRightBottom: begin
      //      Y := Y + (AHeight - Height);
      //      AHeight := Height;
      //    end;
      //    else
      //      raise Exception.Create('Error 9431A388-3F2F-4F06-8296-210708F60C66');
      //  end;
      //end;

      {$IF defined(debug)}
      //ALLog(ClassName+'.SetFixedSizeBounds', 'Name: ' + Name + ' | X : '+ALFloatToStrW(X, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Left, ALDefaultFormatSettingsW)+') | Y : '+ALFloatToStrW(Y, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Top, ALDefaultFormatSettingsW)+') | AWidth : '+ALFloatToStrW(AWidth, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Width, ALDefaultFormatSettingsW)+') | AHeight : '+ALFloatToStrW(AHeight, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Height, ALDefaultFormatSettingsW)+')');
      {$ENDIF}

      inherited SetBounds(X, Y, AWidth, AHeight);

    finally
      TNonReentrantHelper.LeaveSection(FIsSetBoundsLocked)
    end;
  end
  else
    SetBounds(X, Y, AWidth, AHeight);
end;

{**********************************************************************************}
procedure TALDynamicListBoxExtendedControl.SetBounds(X, Y, AWidth, AHeight: Double);
begin
  if FIsSetBoundsLocked then begin
    AWidth := Width;
    AHeight := Height;
  end;

  {$IFNDEF ALCompilerVersionSupported122}
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
  //If (integer(FAlign) >= integer(TALAlignLayout.TopCenter)) and
  //   (integer(FAlign) <= integer(TALAlignLayout.MostBottomRight)) and
  //   (OwnerControl <> nil) and                              // FDisableAlign = true mean that SetBounds was called by
  //   (OwnerControl.FDisableAlign) // AlignObjects procedure inside inherited DoRealign
  //then begin
  //  case FAlign of
  //    TALAlignLayout.TopCenter,
  //    TALAlignLayout.BottomCenter,
  //    TALAlignLayout.MostTopCenter,
  //    TALAlignLayout.MostBottomCenter: begin
  //      X := X + ((AWidth - Width) / 2);
  //      AWidth := Width;
  //    end;
  //    TALAlignLayout.TopLeft,
  //    TALAlignLayout.BottomLeft,
  //    TALAlignLayout.MostTopLeft,
  //    TALAlignLayout.MostBottomLeft: begin
  //      AWidth := Width;
  //    end;
  //    TALAlignLayout.TopRight,
  //    TALAlignLayout.BottomRight,
  //    TALAlignLayout.MostTopRight,
  //    TALAlignLayout.MostBottomRight: begin
  //      X := X + (AWidth - Width);
  //      AWidth := Width;
  //    end;
  //    TALAlignLayout.LeftCenter,
  //    TALAlignLayout.RightCenter,
  //    TALAlignLayout.MostLeftCenter,
  //    TALAlignLayout.MostRightCenter: begin
  //      Y := Y + ((AHeight - Height) / 2);
  //      AHeight := Height;
  //    end;
  //    TALAlignLayout.LeftTop,
  //    TALAlignLayout.RightTop,
  //    TALAlignLayout.MostLeftTop,
  //    TALAlignLayout.MostRightTop: begin
  //      AHeight := Height;
  //    end;
  //    TALAlignLayout.LeftBottom,
  //    TALAlignLayout.RightBottom,
  //    TALAlignLayout.MostLeftBottom,
  //    TALAlignLayout.MostRightBottom: begin
  //      Y := Y + (AHeight - Height);
  //      AHeight := Height;
  //    end;
  //    else
  //      raise Exception.Create('Error 9431A388-3F2F-4F06-8296-210708F60C66');
  //  end;
  //end;

  {$IF defined(debug)}
  //var LMoved := not (SameValue(X, Left, TEpsilon.Position) and SameValue(Y, Top, TEpsilon.Position));
  //var LSizeChanged := not (SameValue(AWidth, Width, TEpsilon.Position) and SameValue(AHeight, Height, TEpsilon.Position));
  //if LMoved or LSizeChanged then
  //  ALLog(ClassName+'.SetBounds', 'Name: ' + Name + ' | X : '+ALFloatToStrW(X, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Left, ALDefaultFormatSettingsW)+') | Y : '+ALFloatToStrW(Y, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Top, ALDefaultFormatSettingsW)+') | AWidth : '+ALFloatToStrW(AWidth, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Width, ALDefaultFormatSettingsW)+') | AHeight : '+ALFloatToStrW(AHeight, ALDefaultFormatSettingsW)+'('+ALFloatToStrW(Height, ALDefaultFormatSettingsW)+')');
  {$ENDIF}

  inherited;
end;

{****************************************************}
procedure TALDynamicListBoxExtendedControl.AdjustSize;
begin
  if //(not (csLoading in ComponentState)) and // Loaded will call again AdjustSize
     (not IsDestroying) and // If csDestroying do not do autosize
     (ControlsCount > 0) and // If there are no controls, do not perform autosizing
     (HasUnconstrainedAutosizeX or HasUnconstrainedAutosizeY) and // If AutoSize is false nothing to adjust
     //(scene <> nil) and // SetNewScene will call again AdjustSize
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // Non-reantrant
    try

      if IsUpdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      {$IF defined(debug)}
      //ALLog(ClassName+'.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(HasUnconstrainedAutosizeX)+'/'+ALBoolToStrW(HasUnconstrainedAutosizeY));
      {$ENDIF}

      var LSize := TSizeF.Create(0,0);
      for var I := Low(FControls) to High(FControls) do begin
        var LChildControl := Controls[I];
        //if (csDesigning in ComponentState) and (LChildControl.ClassName = 'TGrabHandle.TGrabHandleRectangle') then
        //  continue;

        var LALChildControl: TALDynamicListBoxControl;
        var LALChildControlAlign: TALAlignLayout;
        If (LChildControl is TALDynamicListBoxControl) then begin
          LALChildControl := TALDynamicListBoxControl(LChildControl);
          LALChildControlAlign := LALChildControl.Align
        end
        else begin
          LALChildControl := nil;
          LALChildControlAlign := TALAlignLayout(LChildControl.Align);
        end;

        case LALChildControlAlign of

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
          TALAlignLayout.MostTop,
          TALAlignLayout.Bottom,
          TALAlignLayout.MostBottom: begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeX then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl width to ensure it contains the child control at its
              // current position. For example, TALDynamicListBoxText will never have
              // HasUnconstrainedAutosizeX set to true with TALAlignLayout.Top,
              // but TALDynamicListBoxLayout/TRectangle will have it set to true if their
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
          TALAlignLayout.TopCenter,
          TALAlignLayout.TopLeft,
          TALAlignLayout.TopRight,
          TALAlignLayout.BottomCenter,
          TALAlignLayout.BottomLeft,
          TALAlignLayout.BottomRight,
          TALAlignLayout.MostTopCenter,
          TALAlignLayout.MostTopLeft,
          TALAlignLayout.MostTopRight,
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
            LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom);
          end;

          //--
          TALAlignLayout.Left,
          TALAlignLayout.MostLeft,
          TALAlignLayout.Right,
          TALAlignLayout.MostRight: Begin
            // Adjusts AControl width to ensure it contains
            // the child control at its current position.
            LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right);
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeY then
              // If the child control has autosize enabled on the X-axis, adjusts
              // AControl height to ensure it contains the child control at its
              // current position. For example, TALDynamicListBoxText will never have
              // HasUnconstrainedAutosizeX set to true with TALAlignLayout.Left,
              // but TALDynamicListBoxLayout/TRectangle will have it set to true if their
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
          TALAlignLayout.RightCenter,
          TALAlignLayout.RightTop,
          TALAlignLayout.RightBottom,
          TALAlignLayout.MostLeftCenter,
          TALAlignLayout.MostLeftTop,
          TALAlignLayout.MostLeftBottom,
          TALAlignLayout.MostRightCenter,
          TALAlignLayout.MostRightTop,
          TALAlignLayout.MostRightBottom: begin
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
          //TALAlignLayout.Contents,
          //TALAlignLayout.Scale,
          //TALAlignLayout.Fit,
          //TALAlignLayout.FitLeft,
          //TALAlignLayout.FitRight,
          TALAlignLayout.Client: Begin
            if LALChildControl <> nil then begin
              if LALChildControl.HasUnconstrainedAutosizeX then LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right)
              else LSize.Width := Max(LSize.Width, Width);
              if LALChildControl.HasUnconstrainedAutosizeY then LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
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
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeX then
              LSize.Width := Max(LSize.Width, LChildControl.Left + LChildControl.width + LChildControl.Margins.right + padding.right)
            else
              LSize.Width := Max(LSize.Width, Width);
          End;

          //--
          TALAlignLayout.Vertical,
          TALAlignLayout.HorzCenter: Begin
            if (LALChildControl <> nil) and LALChildControl.HasUnconstrainedAutosizeY then
              LSize.height := Max(LSize.height, LChildControl.Top + LChildControl.Height + LChildControl.Margins.bottom + padding.bottom)
            else
              LSize.height := Max(LSize.Height, Height);
          End;

          //--
          else
            raise Exception.Create('Error 431814A4-5A5F-462E-9491-88F1874210DC');

        end;
      end;

      if (not HasUnconstrainedAutosizeX) or (SameValue(LSize.Width, 0, Tepsilon.Position)) then
        LSize.Width := Width;
      if (not HasUnconstrainedAutosizeY) or (SameValue(LSize.Height, 0, Tepsilon.Position)) then
        LSize.Height := Height;
      SetFixedSizeBounds(Left, Top, LSize.Width, LSize.Height);
    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{******************************************************}
procedure TALDynamicListBoxExtendedControl.AlignToPixel;
begin
  // Note: We do not align the position here. The position is aligned during
  // the painting process (e.g., via ALDrawDrawable).
  BeginUpdate;
  Try
    // OnResize and OnResized will be called in loaded
    //var LOldOnResize := OnResize;
    //var LOldOnResized := OnResized;
    //if CSLoading in componentState then begin
    //  OnResize := DelayOnResize;
    //  OnResized := DelayOnResized;
    //end;
    //try
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
        //TALAlignLayout.Contents,
        //TALAlignLayout.Scale,
        //TALAlignLayout.Fit,
        //TALAlignLayout.FitLeft,
        //TALAlignLayout.FitRight
        TALAlignLayout.Client:;
        //--
        else
          Raise Exception.Create('Error AC54DF90-F880-4BD5-8474-E62BD8D099FB')
      end;
    //finally
    //  OnResize := LOldOnResize;
    //  OnResized := LOldOnResized;
    //end;
  finally
    EndUpdate;
  end;
end;

{**************************************************************************}
//procedure TALDynamicListBoxExtendedControl.DelayOnResize(Sender: TObject);
//begin
//  Include(TALDynamicListBoxControlAccessPrivate(Self).FDelayedEvents, TALDynamicListBoxControlAccessPrivate.TDelayedEvent.Resize);
//end;

{***************************************************************************}
//procedure TALDynamicListBoxExtendedControl.DelayOnResized(Sender: TObject);
//begin
//  Include(TALDynamicListBoxControlAccessPrivate(Self).FDelayedEvents, TALDynamicListBoxControlAccessPrivate.TDelayedEvent.Resized);
//end;

{*******************************************************************}
function TALDynamicListBoxExtendedControl.GetDoubleBuffered: boolean;
begin
  result := False;
end;

{**********************************************************************************}
procedure TALDynamicListBoxExtendedControl.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not supported
end;

{************************************************************************}
procedure TALDynamicListBoxExtendedControl.SetScale(const AValue: Single);
begin
  if not SameValue(FScale, AValue, TEpsilon.Scale) then begin
    ClearBufDrawable;
    FScale := AValue;
    //DoMatrixChanged(nil);
    Repaint;
  end;
end;

{*************************************************************}
function TALDynamicListBoxExtendedControl.GetAutoSize: Boolean;
begin
  result := FAutoSize;
end;

{***************************************************************************}
procedure TALDynamicListBoxExtendedControl.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
  end;
end;

{***************************************************************************}
function TALDynamicListBoxExtendedControl.HasUnconstrainedAutosizeX: Boolean;
begin
  Result := GetAutoSize;
  if Result then begin
    result := not (Align in [TALAlignLayout.Client,
                             //TALAlignLayout.Contents,
                             TALAlignLayout.Top,
                             TALAlignLayout.Bottom,
                             TALAlignLayout.MostTop,
                             TALAlignLayout.MostBottom,
                             TALAlignLayout.Horizontal,
                             TALAlignLayout.VertCenter]);
    if (not result) and (OwnerControl <> nil) then
      Result := OwnerControl.HasUnconstrainedAutosizeX;
  end;
end;

{***************************************************************************}
function TALDynamicListBoxExtendedControl.HasUnconstrainedAutosizeY: Boolean;
begin
  Result := GetAutoSize;
  if Result then begin
    result := not (Align in [TALAlignLayout.Client,
                             //TALAlignLayout.Contents,
                             TALAlignLayout.Left,
                             TALAlignLayout.Right,
                             TALAlignLayout.MostLeft,
                             TALAlignLayout.MostRight,
                             TALAlignLayout.Vertical,
                             TALAlignLayout.HorzCenter]);
    if (not result) and (OwnerControl <> nil) then
      Result := OwnerControl.HasUnconstrainedAutosizeY;
  end;
end;

{*********************************************************}
procedure TALDynamicListBoxExtendedControl.MakeBufDrawable;
begin
 // Virtual;
end;

{**********************************************************}
procedure TALDynamicListBoxExtendedControl.ClearBufDrawable;
begin
 // Virtual;
end;

{****************************************************************************}
function TALDynamicListBoxExtendedControl.GetIsPixelAlignmentEnabled: Boolean;
begin
  Result := FIsPixelAlignmentEnabled;
end;

{*******************************************************************************************}
procedure TALDynamicListBoxExtendedControl.SetIsPixelAlignmentEnabled(const AValue: Boolean);
begin
  FIsPixelAlignmentEnabled := AValue;
end;

{***************************************************************************}
function TALDynamicListBoxExtendedControl.IsVisibleWithinFormBounds: Boolean;
begin
  Result := IsVisibleWithinListBoxBounds and (OwnerListBox <> nil) and (OwnerListBox.IsVisibleWithinFormBounds);
end;

{***********************************************************************}
//procedure TALDynamicListBoxExtendedControl.SetNewScene(AScene: IScene);
//begin
//  {$IFNDEF ALCompilerVersionSupported122}
//    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
//    {$MESSAGE WARN 'Check if FMX.Controls.TControl.Pressed is still changed only in SetNewScene/MouseLeave/MouseDown/MouseUp/MouseClick'}
//    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsFocused is still changed only in SetNewScene/DoEnter/DoExit'}
//    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsMouseOver is still changed only in SetNewScene/MouseEnter/MouseLeave'}
//  {$ENDIF}
//  var LPrevPressed := Pressed;
//  var LPrevIsFocused := IsFocused;
//  var LPrevIsMouseOver := IsMouseOver;
//  inherited;
//  // At design time, when a new TEdit/TMemo is added to the form,
//  // or a new TALDynamicListBoxBaseText control with AutoSize=true is added to the form,
//  // the size will not adjust and will remain at its default (200x50).
//  // Calling AdjustSize here will correct this.
//  AdjustSize;
//  {$IF defined(ANDROID) or defined(IOS)}
//  FIsMouseOver := False;
//  {$ENDIF}
//  if LPrevPressed <> Pressed then PressedChanged;
//  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
//  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
//end;

{**************************************************************}
//function TALDynamicListBoxExtendedControl.GetPivot: TPosition;
//begin
//  Result := Inherited RotationCenter;
//end;

{****************************************************************************}
//procedure TALDynamicListBoxExtendedControl.SetPivot(const Value: TPosition);
//begin
//  Inherited RotationCenter := Value;
//end;

{************************************************************}
function TALDynamicListBoxExtendedControl.GetPressed: Boolean;
begin
  result := inherited Pressed;
end;

{***************************************************************************}
procedure TALDynamicListBoxExtendedControl.SetPressed(const AValue: Boolean);
begin
  if AValue <> GetPressed then begin
    inherited Pressed := AValue;
    pressedChanged;
  end;
end;

{***************************************************}
//procedure TALDynamicListBoxExtendedControl.DoEnter;
//begin
//  var LPrevIsFocused := IsFocused;
//  inherited;
//  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
//end;

{**************************************************}
//procedure TALDynamicListBoxExtendedControl.DoExit;
//begin
//  var LPrevIsFocused := IsFocused;
//  inherited;
//  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
//end;

{****************************************************}
procedure TALDynamicListBoxExtendedControl.MouseEnter;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{****************************************************}
procedure TALDynamicListBoxExtendedControl.MouseLeave;
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

{***********************************************************************************************************}
procedure TALDynamicListBoxExtendedControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  //--
  FControlAbsolutePosAtMouseDown := LocalToAbsolute(TPointF.Zero);
  FMouseDownAtLowVelocity := True;
  //--
  //if FDisableDoubleClickHandling then Shift := Shift - [ssDouble];
  //--
  var LScrollableControl: IALScrollableControl;
  var LOwnerControl := OwnerControl;
  while LOwnerControl <> nil do begin
    if Supports(LOwnerControl, IALScrollableControl, LScrollableControl) then begin
      if not LScrollableControl.GetScrollEngine.IsVelocityLow then begin
        FMouseDownAtLowVelocity := False;
        Break;
      end
      else LOwnerControl := LOwnerControl.OwnerControl;
    end
    else LOwnerControl := LOwnerControl.OwnerControl;
  end;
  //--
  //if (not FFocusOnMouseDown) or (FFocusOnMouseUp) or (not FMouseDownAtLowVelocity) then begin
  //  Var LOldIsfocused := FIsfocused;
  //  FIsfocused := True;
  //  Try
  //    inherited;
  //  finally
  //    FIsfocused := LOldIsfocused;
  //  End;
  //end
  //else
    inherited;
  //--
  if LPrevPressed <> Pressed then PressedChanged;
end;

{*********************************************************************************************************}
procedure TALDynamicListBoxExtendedControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
  //var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  //if (FFocusOnMouseUp) and
  //   (FMouseDownAtLowVelocity) and
  //   (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) <= TALScrollEngine.DefaultTouchSlop) and
  //   (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) <= TALScrollEngine.DefaultTouchSlop) and
  //   (not (csDesigning in ComponentState)) and
  //   (not FIsFocused) then
  //  SetFocus;
end;

{************************************************************************************************************}
procedure TALDynamicListBoxExtendedControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (not FMouseDownAtLowVelocity) or
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) or
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then begin
    {$IF defined(debug)}
    if (not FMouseDownAtLowVelocity) then
      ALLog(Classname+'.MouseClick', 'Skipped | Mouse Down was not made at Low Velocity')
    else if (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) then
      ALLog(Classname+'.MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x), ALDefaultFormatSettingsW) + ' horizontally')
    else if (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then
      ALLog(Classname+'.MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y), ALDefaultFormatSettingsW) + ' vertically')
    else
      raise Exception.Create('Error 79BF6F83-8725-476D-A283-507BE9CC671C');
    {$ENDIF}
    exit;
  end;
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
end;

{*************************************}
// Optimized GetParentedVisible to work
// exclusively with OwnerControl.
//function TALDynamicListBoxExtendedControl.GetParentedVisible: Boolean;
//begin
//  {$IF defined(ALDPK)}
//  result := Inherited GetParentedVisible;
//  {$ELSE}
//  var P: TALDynamicListBoxControl := Self;
//  Result := False;
//  while P <> nil do begin
//    if not p.Visible then Exit;
//    P := P.OwnerControl;
//  end;
//  // We do not care about FForm.visible like in
//  // Inherited GetParentedVisible
//  // Result := (FForm = nil) or (FForm.visible);
//  Result := True;
//  {$ENDIF}
//end;

{****************************************************************************}
//procedure TALDynamicListBoxExtendedControl.DoMatrixChanged(Sender: TObject);
//begin
//  {$IFNDEF ALCompilerVersionSupported122}
//    {$MESSAGE WARN 'Check if FMX.Controls.TControl.DoMatrixChanged was not updated and adjust the IFDEF'}
//    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2823 was not corrected and adjust the IFDEF'}
//  {$ENDIF}
//  if not FInPaintTo and not IsUpdating then
//    Repaint;
//  if SameValue(Scale, 1.0, TEpsilon.Scale) and SameValue(RotationAngle, 0.0, TEpsilon.Scale) then
//  begin
//    if (OwnerControl <> nil) and not TALDynamicListBoxControlAccessPrivate(OwnerControl).FSimpleTransform then
//      TALDynamicListBoxControlAccessPrivate(Self).FSimpleTransform := False
//    else
//      TALDynamicListBoxControlAccessPrivate(Self).FSimpleTransform := True;
//  end
//  else
//    TALDynamicListBoxControlAccessPrivate(Self).FSimpleTransform := False;
//
//  if not TALDynamicListBoxControlAccessPrivate(Self).FSimpleTransform then
//  begin
//    if not SameValue(RotationAngle, 0.0, TEpsilon.Scale) then
//    begin
//      FLocalMatrix :=
//        TMatrix.CreateTranslation(-Pivot.X * FSize.Width, -Pivot.Y * FSize.Height) *
//        TMatrix.CreateScaling(Scale, Scale) *
//        TMatrix.CreateRotation(DegToRad(RotationAngle)) *
//        TMatrix.CreateTranslation(Pivot.X * FSize.Width + Left, Pivot.Y * FSize.Height + Top);
//    end
//    else
//    begin
//      FLocalMatrix := TMatrix.Identity;
//      FLocalMatrix.m31 := Left + ((1 - Scale) * Pivot.X * FSize.Width);
//      FLocalMatrix.m32 := Top + ((1 - Scale) * Pivot.Y * FSize.Height);
//      FLocalMatrix.m11 := Scale;
//      FLocalMatrix.m22 := Scale;
//    end;
//  end
//  else
//  begin
//    FLocalMatrix := TMatrix.Identity;
//    FLocalMatrix.m31 := Left;
//    FLocalMatrix.m32 := Top;
//  end;
//
//  RecalcAbsolute;
//  RecalcUpdateRect;
//  if HasDisablePaintEffect then
//    UpdateEffects;
//  if Visible then
//    ParentContentChanged;
//
//  if not GetAnchorMove then
//  begin
//    UpdateExplicitBounds;
//    UpdateAnchorRules(True);
//  end;
//  if not FInPaintTo and not IsUpdating then
//    Repaint;
//end;

{*********************************************************}
//procedure TALDynamicListBoxExtendedControl.DoRootChanged;
//begin
//  inherited;
//  if Root is TCommonCustomForm then FForm := TCommonCustomForm(Root)
//  else FForm := nil;
//end;

{************************************************************}
procedure TALDynamicListBoxExtendedControl.IsMouseOverChanged;
begin
  // virtual
end;

{************************************************************}
//procedure TALDynamicListBoxExtendedControl.IsFocusedChanged;
//begin
//  // virtual
//end;

{********************************************************}
procedure TALDynamicListBoxExtendedControl.PressedChanged;
begin
  // virtual
end;

{**********************************************************}
//Procedure TALDynamicListBoxExtendedControl.MarginsChanged;
//begin
//  // virtual
//end;

{********************************************************}
procedure TALDynamicListBoxExtendedControl.PaddingChanged;
begin
  Inherited;
  AdjustSize;
end;

{*********************************************************}
//procedure TALDynamicListBoxExtendedControl.ParentChanged;
//begin
//  inherited;
//  // Note: The procedure TControl.PaintTo(const ACanvas: TCanvas; const ARect: TRectF; const AParent: TFmxObject = nil)
//  // temporarily updates the OwnerControl. Unfortunately, OwnerControl is not a virtual property,
//  // and TControl.UpdateParentProperties is strictly private. As a result, within TControl.PaintTo,
//  // the value of FOwnerControl will be incorrect.
//  if (OwnerControl <> nil) and (OwnerControl is TALDynamicListBoxControl) then
//    FOwnerControl := TALDynamicListBoxControl(OwnerControl)
//  else
//    //FOwnerControl := nil;
//end;

{**********************************************************************************}
//procedure TALDynamicListBoxExtendedControl.MarginsChangedHandler(Sender: TObject);
//begin
//  if Assigned(FFormerMarginsChangedHandler) then
//    FFormerMarginsChangedHandler(Sender);
//  MarginsChanged;
//end;

{****************************************************************}
function TALDynamicListBoxExtendedControl.IsScaledStored: Boolean;
begin
  Result := not SameValue(FScale, 1, TEpsilon.Scale);
end;

{***************************************************************}
constructor TALDynamicListBoxShape.Create(const AOwner: TObject);
begin
  inherited;
  FFill := CreateFill;
  FFill.OnChanged := FillChanged;
  FStroke := CreateStroke;
  FStroke.OnChanged := StrokeChanged;
  fShadow := CreateShadow;
  fShadow.OnChanged := ShadowChanged;
end;

{****************************************}
destructor TALDynamicListBoxShape.Destroy;
begin
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(fShadow);
  inherited;
end;

{***************************************************}
function TALDynamicListBoxShape.CreateFill: TALBrush;
begin
  Result := TALBrush.Create;
end;

{***********************************************************}
function TALDynamicListBoxShape.CreateStroke: TALStrokeBrush;
begin
  Result := TALStrokeBrush.Create;
end;

{******************************************************}
function TALDynamicListBoxShape.CreateShadow: TALShadow;
begin
  Result := TALShadow.Create;
end;

{********************************************}
procedure TALDynamicListBoxShape.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    Fill.AlignToPixel;
    Stroke.AlignToPixel;
    Shadow.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
procedure TALDynamicListBoxShape.FillChanged(Sender: TObject);
begin
  Repaint;
end;

{**************************************************************}
procedure TALDynamicListBoxShape.StrokeChanged(Sender: TObject);
begin
  Repaint;
end;

{**************************************************************}
procedure TALDynamicListBoxShape.ShadowChanged(Sender: TObject);
begin
  Repaint;
end;

{************************************************}
function TALDynamicListBoxShape.GetFill: TALBrush;
begin
  Result := FFill;
end;

{**************************************************************}
procedure TALDynamicListBoxShape.SetFill(const Value: TALBrush);
begin
  FFill.Assign(Value);
end;

{********************************************************}
function TALDynamicListBoxShape.GetStroke: TALStrokeBrush;
begin
  Result := FStroke;
end;

{**********************************************************************}
procedure TALDynamicListBoxShape.SetStroke(const Value: TALStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{***************************************************}
function TALDynamicListBoxShape.GetShadow: TALShadow;
begin
  Result := FShadow;
end;

{*****************************************************************}
procedure TALDynamicListBoxShape.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
end;

{*******************************************************************}
function TALDynamicListBoxImage.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*******************************************************************}
function TALDynamicListBoxImage.TCropCenter.GetDefaultValue: TPointF;
begin
  Result := TpointF.Create(-50,-50);
end;

{*******************************************************************************************************}
constructor TALDynamicListBoxImage.TResourceDownloadContext.Create(const AOwner: TALDynamicListBoxImage);
begin
  inherited Create;
  Lock := TObject.Create;
  FreeByThread := True;
  Owner := AOwner;
  Rect := Owner.LocalRect.ReducePrecision;
  Scale := ALGetScreenScale;
  AlignToPixel := Owner.IsPixelAlignmentEnabled;
  Color := Owner.BackgroundColor;
  ResourceName := Owner.ResourceName;
  ResourceStream := nil;
  MaskResourceName := Owner.MaskResourceName;
  MaskBitmap := Owner.MaskBitmap;
  WrapMode := Owner.WrapMode;
  CropCenter := Owner.CropCenter.Point;
  RotateAccordingToExifOrientation := Owner.RotateAccordingToExifOrientation;
  StrokeColor := Owner.Stroke.Color;
  StrokeThickness := Owner.Stroke.Thickness;
  ShadowBlur := Owner.Shadow.Blur;
  ShadowOffsetX := Owner.Shadow.OffsetX;
  ShadowOffsetY := Owner.Shadow.OffsetY;
  ShadowColor := Owner.Shadow.Color;
  Corners := Owner.Corners;
  Sides := Owner.Sides;
  XRadius := Owner.XRadius;
  YRadius := Owner.YRadius;
  BlurRadius := Owner.BlurRadius;
end;

{*****************************************************************}
destructor TALDynamicListBoxImage.TResourceDownloadContext.Destroy;
begin
  ALFreeAndNil(Lock);
  ALFreeAndNil(ResourceStream);
  inherited
end;

{***************************************************************}
constructor TALDynamicListBoxImage.Create(const AOwner: TObject);
begin
  inherited Create(AOwner);
  FBackgroundColor := DefaultBackgroundColor;
  FLoadingColor := DefaultLoadingColor;
  fResourceName := '';
  FMaskResourceName := '';
  FMaskBitmap := ALNullBitmap;
  FWrapMode := TALImageWrapMode.Fit;
  fExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;
  fRotateAccordingToExifOrientation := False;
  FCorners := AllCorners;
  FSides := AllSides;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FBlurRadius := DefaultBlurRadius;
  FCacheIndex := 0;
  FLoadingCacheIndex := 0;
  FCacheEngine := nil;
  FCropCenter := CreateCropCenter;
  FCropCenter.OnChange := CropCenterChanged;
  FStroke := CreateStroke;
  FStroke.OnChanged := StrokeChanged;
  fShadow := CreateShadow;
  fShadow.OnChanged := ShadowChanged;
  FResourceDownloadContext := nil;
  fBufDrawable := ALNullDrawable;
end;

{****************************************}
destructor TALDynamicListBoxImage.Destroy;
begin
  ALFreeAndNil(fCropCenter);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(fShadow);
  inherited; // Will call CancelResourceDownload via ClearBufDrawable
end;

{************************************************************}
function TALDynamicListBoxImage.CreateCropCenter: TALPosition;
begin
  Result := TCropCenter.create;
end;

{***********************************************************}
function TALDynamicListBoxImage.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{******************************************************}
function TALDynamicListBoxImage.CreateShadow: TALShadow;
begin
  Result := TALShadow.Create;
end;

{********************************************}
procedure TALDynamicListBoxImage.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    Stroke.AlignToPixel;
    Shadow.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{********************************************************}
function TALDynamicListBoxImage.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{***************************************************************}
function TALDynamicListBoxImage.GetLoadingCacheSubIndex: Integer;
begin
  Result := 0;
end;

{*********************************************************}
function TALDynamicListBoxImage.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{*********************************************************************}
function TALDynamicListBoxImage.GetDefaultBackgroundColor: TalphaColor;
begin
  Result := TalphaColors.Null;
end;

{******************************************************************}
function TALDynamicListBoxImage.GetDefaultLoadingColor: TalphaColor;
begin
  Result := $FFe0e4e9;
end;

{********************************************************}
function TALDynamicListBoxImage.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{********************************************************}
function TALDynamicListBoxImage.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{***********************************************************}
function TALDynamicListBoxImage.GetDefaultBlurRadius: Single;
begin
  Result := 0;
end;

{*********************************************************}
function TALDynamicListBoxImage.GetCropCenter: TALPosition;
begin
  Result := FCropCenter;
end;

{***********************************************************************}
procedure TALDynamicListBoxImage.SetCropCenter(const Value: TALPosition);
begin
  FCropCenter.Assign(Value);
end;

{********************************************************}
function TALDynamicListBoxImage.GetStroke: TALStrokeBrush;
begin
  Result := FStroke;
end;

{**********************************************************************}
procedure TALDynamicListBoxImage.SetStroke(const Value: TALStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{***************************************************}
function TALDynamicListBoxImage.GetShadow: TALShadow;
begin
  Result := FShadow;
end;

{*****************************************************************}
procedure TALDynamicListBoxImage.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
end;

{**************************************************************************}
procedure TALDynamicListBoxImage.SetWrapMode(const Value: TALImageWrapMode);
begin
  if FWrapMode <> Value then begin
    ClearBufDrawable;
    FWrapMode := Value;
    Repaint;
  end;
end;

{*****************************************************************************************}
procedure TALDynamicListBoxImage.SetRotateAccordingToExifOrientation(const Value: Boolean);
begin
  if FRotateAccordingToExifOrientation <> Value then begin
    ClearBufDrawable;
    FRotateAccordingToExifOrientation := Value;
    Repaint;
  end;
end;

{********************************************************************}
procedure TALDynamicListBoxImage.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    ClearBufDrawable;
    FResourceName := Value;
    Repaint;
  end;
end;

{************************************************************************}
procedure TALDynamicListBoxImage.setMaskResourceName(const Value: String);
begin
  if FMaskResourceName <> Value then begin
    ClearBufDrawable;
    FMaskResourceName := Value;
    Repaint;
  end;
end;

{*********************************************************************}
procedure TALDynamicListBoxImage.setMaskBitmap(const Value: TALBitmap);
begin
  if FMaskBitmap <> Value then begin
    ClearBufDrawable;
    FMaskBitmap := Value;
    Repaint;
  end;
end;

{****************************************************************************}
procedure TALDynamicListBoxImage.setBackgroundColor(const Value: TAlphaColor);
begin
  if FBackgroundColor <> Value then begin
    ClearBufDrawable;
    FBackgroundColor := Value;
    Repaint;
  end;
end;

{*************************************************************************}
procedure TALDynamicListBoxImage.setLoadingColor(const Value: TAlphaColor);
begin
  if FLoadingColor <> Value then begin
    ClearBufDrawable;
    FLoadingColor := Value;
    Repaint;
  end;
end;

{***************************************************************}
procedure TALDynamicListBoxImage.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{***************************************************************}
procedure TALDynamicListBoxImage.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{******************************************************************}
procedure TALDynamicListBoxImage.SetBlurRadius(const Value: Single);
begin
  if not SameValue(FBlurRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FBlurRadius := Value;
    Repaint;
  end;
end;

{*****************************************************************}
procedure TALDynamicListBoxImage.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    ClearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{*************************************************************}
procedure TALDynamicListBoxImage.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    ClearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{***************************************************************}
function TALDynamicListBoxImage.IsBackgroundColorStored: Boolean;
begin
  Result := FBackgroundColor <> DefaultBackgroundColor;
end;

{************************************************************}
function TALDynamicListBoxImage.IsLoadingColorStored: Boolean;
begin
  Result := FLoadingColor <> DefaultLoadingColor;
end;

{*******************************************************}
function TALDynamicListBoxImage.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{*****************************************************}
function TALDynamicListBoxImage.IsSidesStored: Boolean;
begin
  Result := FSides <> AllSides
end;

{*******************************************************}
function TALDynamicListBoxImage.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{*******************************************************}
function TALDynamicListBoxImage.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{**********************************************************}
function TALDynamicListBoxImage.IsBlurRadiusStored: Boolean;
begin
  Result := not SameValue(FBlurRadius, DefaultBlurRadius, TEpsilon.Vector);
end;

{******************************************************************}
procedure TALDynamicListBoxImage.CropCenterChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{**************************************************************}
procedure TALDynamicListBoxImage.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{**************************************************************}
procedure TALDynamicListBoxImage.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{*****************************************}
procedure TALDynamicListBoxImage.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{************************************************}
procedure TALDynamicListBoxImage.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not IsDestroying) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  CancelResourceDownload;
  ALFreeAndNilDrawable(fBufDrawable);
end;

{******************************************************}
procedure TALDynamicListBoxImage.CancelResourceDownload;
begin
  // The FResourceDownloadContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FResourceDownloadContext <> nil then begin
    var LContextToFree: TResourceDownloadContext;
    var LLock := FResourceDownloadContext.lock;
    TMonitor.Enter(LLock);
    try
      if not FResourceDownloadContext.FreeByThread then LContextToFree := FResourceDownloadContext
      else LContextToFree := nil;
      FResourceDownloadContext.Owner := nil;
      FResourceDownloadContext := nil;
    Finally
      TMonitor.Exit(LLock);
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{*************}
//[MultiThread]
class function TALDynamicListBoxImage.CanStartResourceDownload(var AContext: Tobject): boolean;
begin
  result := TResourceDownloadContext(AContext).owner <> nil;
end;

{*************}
//[MultiThread]
class procedure TALDynamicListBoxImage.HandleResourceDownloadSuccess(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AContext: TObject);
begin
  var LContext := TResourceDownloadContext(AContext);
  if LContext.owner = nil then exit;
  LContext.ResourceStream := AContentStream;
  TALGraphicThreadPool.Instance.ExecuteProc(
    CreateBufDrawable, // const AProc: TALWorkerThreadProc;
    LContext, // const AContext: Tobject; TALGraphicThreadPool.Instance will own and release the Context object
    GetResourceDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  AContentStream := nil; // AContentStream Will be free by AContext
  AContext := nil; // AContext will be free by TALGraphicThreadPool.Instance
end;

{*************}
//[MultiThread]
class procedure TALDynamicListBoxImage.HandleResourceDownloadError(const AErrMessage: string; var AContext: Tobject);
begin
  var LContext := TResourceDownloadContext(AContext);
  if LContext.owner = nil then exit;
  {$IFDEF ALDPK}
  TMonitor.Enter(LContext.Lock);
  try
    if LContext.Owner <> nil then begin
      LContext.FreeByThread := False;
      AContext := nil; // AContext will be free by CancelResourceDownload
    end;
  finally
    TMonitor.Exit(LContext.Lock);
  end;
  exit;
  {$ENDIF}
  if LContext.ResourceName = ALBrokenImageResourceName then begin
    ALLog(
      'TALDynamicListBoxImage.HandleResourceDownloadError',
      'BrokenImage resource is missing or incorrect | ' +
      AErrMessage,
      TalLogType.error);
    TMonitor.Enter(LContext.Lock);
    try
      if LContext.Owner <> nil then begin
        LContext.FreeByThread := False;
        AContext := nil; // AContext will be free by CancelResourceDownload
      end;
    finally
      TMonitor.Exit(LContext.Lock);
    end;
    exit;
  end;
  ALLog(
    'TALDynamicListBoxImage.HandleResourceDownloadError',
    'Url: ' + LContext.ResourceName + ' | ' +
    AErrMessage,
    TalLogType.warn);
  LContext.Rect := TRectF.Create(
                     LContext.Rect.TopLeft,
                     ALBrokenImageWidth,
                     ALBrokenImageHeight);
  //LContext.Scale: Single;
  //LContext.AlignToPixel: Boolean;
  LContext.Color := TalphaColors.Null;
  LContext.ResourceName := ALBrokenImageResourceName;
  ALFreeAndNil(LContext.ResourceStream);
  LContext.MaskResourceName := '';
  LContext.MaskBitmap := ALNullBitmap;
  LContext.WrapMode := TALImageWrapMode.Fit;
  //LContext.CropCenter: TpointF;
  //LContext.RotateAccordingToExifOrientation: Boolean;
  LContext.StrokeColor := TalphaColors.Null;
  //LContext.StrokeThickness: Single;
  //LContext.ShadowBlur: Single;
  //LContext.ShadowOffsetX: Single;
  //LContext.ShadowOffsetY: Single;
  LContext.ShadowColor := TAlphaColors.Null;
  LContext.Corners := AllCorners;
  LContext.Sides := AllSides;
  LContext.XRadius := 0;
  LContext.YRadius := 0;
  LContext.BlurRadius := 0;
  TALGraphicThreadPool.Instance.ExecuteProc(
    CreateBufDrawable, // const AProc: TALWorkerThreadProc;
    LContext, // const AContext: Tobject; TALGraphicThreadPool.Instance will own and release the Context object
    GetResourceDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  AContext := nil; // AContext will be free by TALGraphicThreadPool.Instance
end;

{*************}
//[MultiThread]
class function TALDynamicListBoxImage.GetResourceDownloadPriority(const AContext: Tobject): Int64;
begin
  result := TALNetHttpClientPool.Instance.PriorityStartingPoint;
end;

{*************}
//[MultiThread]
class Procedure TALDynamicListBoxImage.CreateBufDrawable(var AContext: TObject);
begin
  var LContext := TResourceDownloadContext(AContext);
  if LContext.owner = nil then exit;
  var LBufDrawable: TALDrawable := ALNullDrawable;
  var LBufDrawableRect: TRectF;
  var LExifOrientationInfo: TalExifOrientationInfo;
  Try
    CreateBufDrawable(
      LBufDrawable, // var ABufDrawable: TALDrawable;
      LBufDrawableRect, // out ABufDrawableRect: TRectF;
      LExifOrientationInfo, // out AExifOrientationInfo: TalExifOrientationInfo;
      LContext.Rect, // const ARect: TRectF;
      LContext.Scale, // const AScale: Single;
      LContext.AlignToPixel, // const AAlignToPixel: Boolean;
      LContext.Color, // const AColor: TAlphaColor;
      LContext.ResourceName, // const AResourceName: String;
      LContext.ResourceStream, // const AResourceStream: TStream;
      LContext.MaskResourceName, // const AMaskResourceName: String;
      LContext.MaskBitmap, // const AMaskBitmap: TALBitmap;
      LContext.WrapMode, // const AWrapMode: TALImageWrapMode;
      LContext.CropCenter, // const ACropCenter: TpointF;
      LContext.RotateAccordingToExifOrientation, // const ARotateAccordingToExifOrientation: Boolean;
      LContext.StrokeColor, // const AStrokeColor: TAlphaColor;
      LContext.StrokeThickness, // const AStrokeThickness: Single;
      LContext.ShadowBlur, // const AShadowBlur: Single;
      LContext.ShadowOffsetX, // const AShadowOffsetX: Single;
      LContext.ShadowOffsetY, // const AShadowOffsetY: Single;
      LContext.ShadowColor, // const AShadowColor: TAlphaColor;
      LContext.Corners, // const ACorners: TCorners;
      LContext.Sides, // const ASides: TSides;
      LContext.XRadius, // const AXRadius: Single;
      LContext.YRadius, // const AYRadius: Single)
      LContext.BlurRadius); // const ABlurRadius: Single)
  except
    On E: Exception do begin
      HandleResourceDownloadError(E.Message, AContext);
      exit;
    end;
  End;
  TThread.queue(nil,
    procedure
    begin
      if LContext.Owner <> nil then begin
        ALFreeAndNilDrawable(LContext.Owner.fBufDrawable);
        LContext.Owner.fBufDrawable := LBufDrawable;
        LContext.Owner.FBufDrawableRect := LBufDrawableRect;
        LContext.Owner.FExifOrientationInfo := LExifOrientationInfo;
        LContext.Owner.FResourceDownloadContext := nil;
        LContext.Owner.Repaint;
      end;
      ALFreeAndNil(LContext);
    end);
  AContext := nil; // AContext will be free by TThread.queue
end;

{*************}
//[MultiThread]
class Procedure TALDynamicListBoxImage.CreateBufDrawable(
                  var ABufDrawable: TALDrawable;
                  out ABufDrawableRect: TRectF;
                  out AExifOrientationInfo: TalExifOrientationInfo;
                  const ARect: TRectF;
                  const AScale: Single;
                  const AAlignToPixel: Boolean;
                  const AColor: TAlphaColor;
                  const AResourceName: String;
                  const AResourceStream: TStream;
                  const AMaskResourceName: String;
                  const AMaskBitmap: TALBitmap;
                  const AWrapMode: TALImageWrapMode;
                  const ACropCenter: TpointF;
                  const ARotateAccordingToExifOrientation: Boolean;
                  const AStrokeColor: TAlphaColor;
                  const AStrokeThickness: Single;
                  const AShadowBlur: Single;
                  const AShadowOffsetX: Single;
                  const AShadowOffsetY: Single;
                  const AShadowColor: TAlphaColor;
                  const ACorners: TCorners;
                  const ASides: TSides;
                  const AXRadius: Single;
                  const AYRadius: Single;
                  const ABlurRadius: Single);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var lResourceName: String;
  if ALIsHttpOrHttpsUrl(AResourceName) then lResourceName := ''
  else lResourceName := AResourceName;
  var LFileName := ALGetResourceFilename(lResourceName);

  ABufDrawableRect := ARect;
  if (ARotateAccordingToExifOrientation) then begin
    {$IF defined(ALSkiaEngine)}
    // SkImage automatically loads the image with the correct orientation based on EXIF data.
    // Therefore, if we apply ExifOrientationInfo to rotate the image again, the result will be incorrect.
    AExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;
    {$ELSE}
    if LFileName <> '' then AExifOrientationInfo := AlGetExifOrientationInfo(LFilename)
    else if AResourceStream <> nil then AExifOrientationInfo := AlGetExifOrientationInfo(AResourceStream)
    else AExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;
    if AExifOrientationInfo in [TalExifOrientationInfo.TRANSPOSE,
                                TalExifOrientationInfo.ROTATE_90,
                                TalExifOrientationInfo.TRANSVERSE,
                                TalExifOrientationInfo.ROTATE_270] then begin
      ABufDrawableRect.Width := ARect.Height;
      ABufDrawableRect.Height := ARect.Width;
    end;
    {$ENDIF}
  end
  else begin
    {$IF defined(ALSkiaEngine)}
    // SkImage automatically loads the image with the correct orientation based on EXIF data.
    // If we want to disable this behavior, we need to manually rotate the image to its original orientation.
    if LFileName <> '' then AExifOrientationInfo := AlGetExifOrientationInfo(LFilename)
    else if AResourceStream <> nil then AExifOrientationInfo := AlGetExifOrientationInfo(AResourceStream)
    else AExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;
    if AExifOrientationInfo in [TalExifOrientationInfo.TRANSPOSE,
                                TalExifOrientationInfo.ROTATE_90,
                                TalExifOrientationInfo.TRANSVERSE,
                                TalExifOrientationInfo.ROTATE_270] then begin
      ABufDrawableRect.Width := ARect.Height;
      ABufDrawableRect.Height := ARect.Width;
    end;
    {$ELSE}
    AExifOrientationInfo := TalExifOrientationInfo.UNDEFINED;
    {$ENDIF}
  end;

  {$REGION 'Use ALCreateDrawableFromResource'}
  if ((AResourceStream <> nil) or (LFileName <> '') or (LResourceName <> '')) and
     (ACorners = AllCorners) and
     (Acolor = TalphaColors.Null) and
     ((AShadowColor = TalphaColors.Null) or (CompareValue(AShadowBlur, 0, TEpsilon.position) <= 0)) and
     ((AStrokeColor = TalphaColors.Null) or (CompareValue(AStrokeThickness, 0, TEpsilon.position) <= 0)) then begin

    {$IFDEF ALDPK}
    try
    {$ENDIF}

      ABufDrawable := ALCreateDrawableFromResource(
                        AResourceName, // const AResourceName: String;
                        AResourceStream, // const AResourceStream: TStream;
                        AMaskResourceName, // const AMaskResourceName: String;
                        AMaskBitmap, // const AMaskBitmap: TALBitmap;
                        AScale, // const AScale: Single;
                        ARect.Width, ARect.Height, // const W, H: single;
                        AWrapMode, // const AWrapMode: TALImageWrapMode;
                        ACropCenter, // const ACropCenter: TpointF;
                        ABlurRadius, // const ABlurRadius: single;
                        AXRadius, // const AXRadius: Single;
                        AYRadius); // const AYRadius: Single);

    {$IFDEF ALDPK}
    except
      ABufDrawable := ALCreateEmptyDrawable1x1;
      Exit;
    end;
    {$ENDIF}

  end;
  {$ENDREGION}

  if not ALIsDrawableNull(ABufDrawable) then begin
    ABufDrawableRect := TrectF.Create(0,0, ALGetDrawableWidth(ABufDrawable)/AScale, ALGetDrawableHeight(ABufDrawable)/AScale).CenterAt(ARect);
    Exit;
  end;

  {$REGION 'Use TALDrawRectangleHelper'}
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AColor, // const AFillColor: TAlphaColor;
                        [], // const AFillGradientColors: TArray<TAlphaColor>;
                        LResourceName, // const AFillResourceName: String;
                        AResourceStream, // const AFillResourceStream: TStream;
                        TRectF.Empty, // Const AFillBackgroundMarginsRect: TRectF;
                        TRectF.Empty, // Const AFillImageMarginsRect: TRectF;
                        0, // const AStateLayerOpacity: Single;
                        TAlphaColors.Null, // const AStateLayerColor: TAlphaColor;
                        False, // const AStateLayerUseContentColor: Boolean;
                        TRectF.Empty, // Const AStateLayerMarginsRect: TRectF;
                        AShadowColor, // const AShadowColor: TAlphaColor;
                        AShadowBlur, // const AShadowBlur: Single;
                        AShadowOffsetX, // const AShadowOffsetX: Single;
                        AShadowOffsetY); // const AShadowOffsetY: Single): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(AAlignToPixel)
        .SetDstRect(ABufDrawableRect)
        .SetFillColor(AColor)
        .SetFillResourceName(LResourceName)
        .SetFillResourceStream(AResourceStream)
        .SetFillMaskResourceName(AMaskResourceName)
        .SetFillMaskBitmap(AMaskBitmap)
        .SetFillWrapMode(AWrapMode)
        .SetFillCropCenter(ACropCenter)
        .SetFillBlurRadius(ABlurRadius)
        .SetStrokeColor(AStrokecolor)
        .SetStrokeThickness(AStrokeThickness)
        .SetShadowColor(AShadowColor)
        .SetShadowBlur(AShadowBlur)
        .SetShadowOffsetX(AShadowOffsetX)
        .SetShadowOffsetY(AShadowOffsetY)
        .SetSides(ASides)
        .SetCorners(ACorners)
        .SetXRadius(AXRadius)
        .SetYRadius(AYRadius)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;
  {$ENDREGION}

end;

{***********************************************}
procedure TALDynamicListBoxImage.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if the size is 0
     (BoundsRect.IsEmpty) or
     //--- Do not create BufDrawable if fResourceName is empty
     (fResourceName = '')
  then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) or
     (FResourceDownloadContext <> nil) then exit;

  If FResourceDownloadContext <> nil then begin
    if (LoadingCacheIndex > 0) and
       (CacheEngine <> nil) and
       (CacheEngine.HasEntry(LoadingCacheIndex{AIndex}, GetLoadingCacheSubIndex{ASubIndex})) then Exit;
  end
  else begin
    if (CacheIndex > 0) and
       (CacheEngine <> nil) and
       (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;
  end;

  if (FResourceDownloadContext = nil) and
     (ALIsHttpOrHttpsUrl(ResourceName)) then begin

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Starting download | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
    {$endif}

    FResourceDownloadContext := TResourceDownloadContext.Create(Self);
    Try
      TALNetHttpClientPool.Instance.Get(
        ResourceName, // const AUrl: String;
        CanStartResourceDownload, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
        HandleResourceDownloadSuccess, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
        HandleResourceDownloadError, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
        FResourceDownloadContext, // const AContext: Tobject; // Context will be free by the worker thread
        true, // const AUseCache: Boolean = True;
        GetResourceDownloadPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
    except
      ALFreeAndNil(FResourceDownloadContext);
      Raise;
    End;

    if (LoadingColor <> TAlphaColors.Null) and
       ((MaskResourceName <> '') or
        (not ALIsBitmapNull(MaskBitmap)) or
        (not SameValue(xRadius, 0, TEpsilon.Vector)) or
        (not SameValue(yRadius, 0, TEpsilon.Vector))) then begin

      if (LoadingCacheIndex > 0) and
         (CacheEngine <> nil) and
         (CacheEngine.HasEntry(LoadingCacheIndex{AIndex}, GetLoadingCacheSubIndex{ASubIndex})) then Exit;

      {$IFDEF debug}
      ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Creating loading content | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
      {$endif}

      CreateBufDrawable(
        FBufDrawable, // var ABufDrawable: TALDrawable;
        FBufDrawableRect, // out ABufDrawableRect: TRectF;
        FExifOrientationInfo, // out AExifOrientationInfo: TalExifOrientationInfo;
        LocalRect.ReducePrecision, // const ARect: TRectF;
        ALGetScreenScale, // const AScale: Single;
        IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
        LoadingColor, // const AColor: TAlphaColor;
        '', // const AResourceName: String;
        nil, // const AResourceStream: TStream;
        MaskResourceName, // const AMaskResourceName: String;
        MaskBitmap, // const AMaskBitmap: TALBitmap;
        TALImageWrapMode.Fit, // const AWrapMode: TALImageWrapMode;
        TpointF.Zero, // const ACropCenter: TpointF;
        false, // const ARotateAccordingToExifOrientation: Boolean;
        TAlphaColors.Null, // const AStrokeColor: TAlphaColor;
        0, // const AStrokeThickness: Single;
        0, // const AShadowBlur: Single;
        0, // const AShadowOffsetX: Single;
        0, // const AShadowOffsetY: Single;
        TAlphaColors.Null, // const AShadowColor: TAlphaColor;
        Corners, // const ACorners: TCorners;
        AllSides, // const ASides: TSides;
        XRadius, // const AXRadius: Single;
        YRadius, // const AYRadius: Single;
        0); // const ABlurRadius: Single);
    end;

    exit;

  end;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // out ABufDrawableRect: TRectF;
    FExifOrientationInfo, // out AExifOrientationInfo: TalExifOrientationInfo;
    LocalRect.ReducePrecision, // const ARect: TRectF;
    ALGetScreenScale, // const AScale: Single;
    IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
    BackGroundColor, // const AColor: TAlphaColor;
    ResourceName, // const AResourceName: String;
    nil, // const AResourceStream: TStream;
    MaskResourceName, // const AMaskResourceName: String;
    MaskBitmap, // const AMaskBitmap: TALBitmap;
    WrapMode, // const AWrapMode: TALImageWrapMode;
    CropCenter.Point, // const ACropCenter: TpointF;
    RotateAccordingToExifOrientation, // const ARotateAccordingToExifOrientation: Boolean;
    Stroke.Color, // const AStrokeColor: TAlphaColor;
    Stroke.Thickness, // const AStrokeThickness: Single;
    Shadow.Blur, // const AShadowBlur: Single;
    Shadow.OffsetX, // const AShadowOffsetX: Single;
    Shadow.OffsetY, // const AShadowOffsetY: Single;
    Shadow.Color, // const AShadowColor: TAlphaColor;
    Corners, // const ACorners: TCorners;
    Sides, // const ASides: TSides;
    XRadius, // const AXRadius: Single;
    YRadius, // const AYRadius: Single)
    BlurRadius); // const ABlurRadius: Single);

end;

{*************************************}
procedure TALDynamicListBoxImage.Paint;
begin

  //if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  //begin
  //  var R := LocalRect.ReducePrecision;
  //  InflateRect(R, -0.5, -0.5);
  //  Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  //end;

  var LCacheIndex: integer;
  var LCacheSubIndex: Integer;
  if FResourceDownloadContext <> nil then begin
    LCacheIndex := LoadingCacheIndex;
    LCacheSubIndex := GetLoadingCacheSubIndex;
  end
  else begin
    LCacheIndex := CacheIndex;
    LCacheSubIndex := GetCacheSubIndex;
  end;
  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if (LCacheIndex <= 0) or
     (CacheEngine = nil) or
     (not CacheEngine.TryGetEntry(LCacheIndex{AIndex}, LCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
    MakeBufDrawable;
    if FResourceDownloadContext <> nil then LCacheIndex := LoadingCacheIndex
    else LCacheIndex := CacheIndex;
    if (LCacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
      if not CacheEngine.TrySetEntry(LCacheIndex{AIndex}, LCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
      else fBufDrawable := ALNullDrawable;
      if not CacheEngine.TryGetEntry(LCacheIndex{AIndex}, LCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    if LoadingColor <> TAlphaColors.Null then begin
      {$IF DEFINED(ALSkiaCanvas)}
      TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LocalRect.ReducePrecision)
        .SetOpacity(AbsoluteOpacity)
        .SetFillColor(FloadingColor)
        .SetCorners(FCorners)
        .SetXRadius(FXRadius)
        .SetYRadius(FYRadius)
        .Draw;
      {$ELSE}
      Canvas.Fill.kind := TBrushKind.solid;
      Canvas.Fill.color := FloadingColor;
      Canvas.FillRect(ALAlignToPixelRound(LocalRect.ReducePrecision, Canvas.Matrix, Canvas.Scale, TEpsilon.position), XRadius, YRadius, FCorners, AbsoluteOpacity, TCornerType.Round);
      {$ENDIF}
    end;
    exit;
  end;

  case fExifOrientationInfo of
    TalExifOrientationInfo.FLIP_HORIZONTAL: begin
      var LMatrixRotationCenter: TpointF;
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      var LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.FLIP_VERTICAL: begin
      var LMatrixRotationCenter: TpointF;
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      var LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateScaling(1, -1); // matrix.setRotate(180); matrix.setScale(-1, 1);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.NORMAL:;
    TalExifOrientationInfo.ROTATE_180: begin
      var LMatrixRotationCenter: TpointF;
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      var LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(180)); // matrix.setRotate(180);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.ROTATE_270: begin
      var LMatrixRotationCenter: TpointF;
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      var LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      {$IF defined(ALSkiaEngine)}
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(90)); // matrix.setRotate(90);
      {$ELSE}
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(-90)); // matrix.setRotate(-90);
      {$ENDIF}
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.ROTATE_90: begin
      var LMatrixRotationCenter: TpointF;
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      var LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      {$IF defined(ALSkiaEngine)}
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(-90)); // matrix.setRotate(-90);
      {$ELSE}
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(90)); // matrix.setRotate(90);
      {$ENDIF}
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.TRANSPOSE: begin
      var LMatrixRotationCenter: TpointF;
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      var LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(90)); // matrix.setRotate(90);
      LMatrix := LMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.TRANSVERSE: begin
      var LMatrixRotationCenter: TpointF;
      LMatrixRotationCenter.X := (width / 2) + Canvas.Matrix.m31;
      LMatrixRotationCenter.Y := (height / 2) + Canvas.Matrix.m32;
      var LMatrix := Canvas.Matrix * TMatrix.CreateTranslation(-LMatrixRotationCenter.X,-LMatrixRotationCenter.Y);
      LMatrix := LMatrix * TMatrix.CreateRotation(DegToRad(-90)); // matrix.setRotate(-90);
      LMatrix := LMatrix * TMatrix.CreateScaling(-1, 1); // matrix.setScale(-1, 1);
      LMatrix := LMatrix * TMatrix.CreateTranslation(LMatrixRotationCenter.X,LMatrixRotationCenter.Y);
      Canvas.SetMatrix(LMatrix);
    end;
    TalExifOrientationInfo.UNDEFINED:;
    else
      Raise Exception.Create('Error 015D39FD-8A61-4F7F-A8AA-639A91FCBC37');
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***********************************************************************}
constructor TALDynamicListBoxBaseRectangle.Create(const AOwner: TObject);
begin
  inherited;
  fDoubleBuffered := true;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FCorners := AllCorners;
  FSides := AllSides;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  fBufDrawable := ALNullDrawable;
end;

{************************************************}
destructor TALDynamicListBoxBaseRectangle.Destroy;
begin
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{*************************************************}
procedure TALDynamicListBoxBaseRectangle.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALDynamicListBoxBaseRectangle.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not IsDestroying) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{***************************************************************}
function TALDynamicListBoxBaseRectangle.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{*************************************************************}
function TALDynamicListBoxBaseRectangle.IsSidesStored: Boolean;
begin
  Result := FSides <> AllSides
end;

{***************************************************************}
function TALDynamicListBoxBaseRectangle.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{***************************************************************}
function TALDynamicListBoxBaseRectangle.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{*************************************************************}
function TALDynamicListBoxBaseRectangle.HasCustomDraw: Boolean;
begin
  Result := False;
end;

{****************************************************************}
function TALDynamicListBoxBaseRectangle.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{*****************************************************************}
function TALDynamicListBoxBaseRectangle.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{********************************************************************************}
procedure TALDynamicListBoxBaseRectangle.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{****************************************************************}
function TALDynamicListBoxBaseRectangle.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{****************************************************************}
function TALDynamicListBoxBaseRectangle.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{***********************************************************************}
procedure TALDynamicListBoxBaseRectangle.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{***********************************************************************}
procedure TALDynamicListBoxBaseRectangle.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{*************************************************************************}
procedure TALDynamicListBoxBaseRectangle.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    ClearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{*********************************************************************}
procedure TALDynamicListBoxBaseRectangle.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    ClearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{********************************************************************}
procedure TALDynamicListBoxBaseRectangle.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************************************************}
procedure TALDynamicListBoxBaseRectangle.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************************************************}
procedure TALDynamicListBoxBaseRectangle.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************************************************}
function TALDynamicListBoxBaseRectangle.IsSimpleRenderPossible: Boolean;
begin
  Result := (not HasCustomDraw)
            and
            ((not Stroke.HasStroke) or
             (sides = []))
            and
            ((SameValue(xRadius, 0, TEpsilon.Vector)) or
             (SameValue(yRadius, 0, TEpsilon.Vector)) or
             (corners=[]))
            and
            (not Shadow.HasShadow)
            and
            ((not Fill.HasFill) or
             (Fill.Styles = [TALBrushStyle.solid]));
end;

{*********************************************************}
Procedure TALDynamicListBoxBaseRectangle.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect.ReducePrecision;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AFill, // const AFill: TALBrush;
                        nil, // const AFillResourceStream: TStream;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, AStateLayerContentColor)
        .SetDrawStateLayerOnTop(ADrawStateLayerOnTop)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetSides(Sides)
        .SetCorners(Corners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{*******************************************************}
procedure TALDynamicListBoxBaseRectangle.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     {$IF not DEFINED(ALDPK)}(not DoubleBuffered) or{$ENDIF}
     //--- Do not create BufDrawable if the size is 0
     (BoundsRect.IsEmpty) or
     //--- Do not create BufDrawable if only fill with solid color
     (IsSimpleRenderPossible) then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    ALGetScreenScale, // const AScale: Single;
    Fill, // const AFill: TALBrush;
    nil, // const AStateLayer: TALStateLayer;
    TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
    True, // const ADrawStateLayerOnTop: Boolean;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALDynamicListBoxBaseRectangle.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  Result := ALGetShapeSurfaceRect(
              ARect, // const ARect: TRectF;
              Fill, // const AFill: TALBrush;
              nil, // const AFillResourceStream: TStream;
              nil, // const AStateLayer: TALStateLayer;
              Shadow); // const AShadow: TALShadow): TRectF;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxBaseRectangle.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxBaseRectangle.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{*********************************************}
procedure TALDynamicListBoxBaseRectangle.Paint;
begin

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if (CacheIndex <= 0) or
     (CacheEngine = nil) or
     (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
    MakeBufDrawable;
    if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
      if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
      else fBufDrawable := ALNullDrawable;
      if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
      .SetAlignToPixel(IsPixelAlignmentEnabled)
      .SetDstRect(LocalRect.ReducePrecision)
      .SetOpacity(AbsoluteOpacity)
      .SetFill(Fill)
      .SetStroke(Stroke)
      .SetShadow(Shadow)
      .SetSides(Sides)
      .SetCorners(Corners)
      .SetXRadius(XRadius)
      .SetYRadius(YRadius)
      .Draw;
    {$ELSE}
    if IsSimpleRenderPossible then begin
      If Fill.Styles = [TALBrushStyle.Solid] then begin
        Canvas.Fill.kind := TBrushKind.solid;
        Canvas.Fill.color := Fill.color;
        Canvas.FillRect(ALAlignToPixelRound(LocalRect.ReducePrecision, Canvas.Matrix, Canvas.Scale, TEpsilon.position), XRadius, YRadius, FCorners, AbsoluteOpacity, TCornerType.Round);
      end;
    end
    else begin
      var LRect := LocalRect.ReducePrecision;
      InitRenderTargets(LRect);
      if ALCanvasBeginScene(FRenderTargetCanvas) then
      try
        ALClearCanvas(FRenderTargetCanvas, TAlphaColors.Null);
        TALDrawRectangleHelper.Create(FRenderTargetCanvas)
          .SetScale(ALGetScreenScale)
          .SetAlignToPixel(IsPixelAlignmentEnabled)
          .SetDstRect(LRect)
          .SetFill(Fill)
          .SetStroke(Stroke)
          .SetShadow(Shadow)
          .SetSides(Sides)
          .SetCorners(Corners)
          .SetXRadius(XRadius)
          .SetYRadius(YRadius)
          .Draw;
      finally
        ALCanvasEndScene(FRenderTargetCanvas)
      end;
      ALUpdateDrawableFromSurface(FRenderTargetSurface, FRenderTargetDrawable);
      // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
      // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
      LRect.Offset(-2*LRect.Left, -2*LRect.Top);
      ALDrawDrawable(
        Canvas, // const ACanvas: Tcanvas;
        FRenderTargetDrawable, // const ADrawable: TALDrawable;
        LRect.TopLeft, // const ADstTopLeft: TpointF;
        AbsoluteOpacity); // const AOpacity: Single)
    end;
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{****************************************************************}
constructor TALDynamicListBoxCircle.Create(const AOwner: TObject);
begin
  inherited;
  fDoubleBuffered := true;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  fBufDrawable := ALNullDrawable;
end;

{*****************************************}
destructor TALDynamicListBoxCircle.Destroy;
begin
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{*************************************************}
procedure TALDynamicListBoxCircle.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not IsDestroying) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*********************************************************}
function TALDynamicListBoxCircle.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{**********************************************************}
function TALDynamicListBoxCircle.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{*************************************************************************}
procedure TALDynamicListBoxCircle.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{*************************************************************}
procedure TALDynamicListBoxCircle.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{***************************************************************}
procedure TALDynamicListBoxCircle.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{***************************************************************}
procedure TALDynamicListBoxCircle.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**************************************************}
Procedure TALDynamicListBoxCircle.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect.ReducePrecision;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AFill, // const AFill: TALBrush;
                        nil, // const AFillResourceStream: TStream;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.Height); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(ABufDrawableRect))
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, AStateLayerContentColor)
        .SetDrawStateLayerOnTop(ADrawStateLayerOnTop)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetXRadius(-50)
        .SetYRadius(-50)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{************************************************}
procedure TALDynamicListBoxCircle.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     {$IF not DEFINED(ALDPK)}(not DoubleBuffered) or{$ENDIF}
     //--- Do not create BufDrawable if the size is 0
     (BoundsRect.IsEmpty) then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // var ABufDrawableRect: TRectF;
    ALGetScreenScale, // const AScale: Single;
    Fill, // const AFill: TALBrush;
    nil, // const AStateLayer: TALStateLayer;
    TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
    True, // const ADrawStateLayerOnTop: Boolean;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALDynamicListBoxCircle.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  Result := ALGetShapeSurfaceRect(
              ARect, // const ARect: TRectF;
              Fill, // const AFill: TALBrush;
              nil, // const AFillResourceStream: TStream;
              nil, // const AStateLayer: TALStateLayer;
              Shadow); // const AShadow: TALShadow): TRectF;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxCircle.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxCircle.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{**************************************}
procedure TALDynamicListBoxCircle.Paint;
begin

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if (CacheIndex <= 0) or
     (CacheEngine = nil) or
     (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
    MakeBufDrawable;
    if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
      if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
      else fBufDrawable := ALNullDrawable;
      if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
      .SetAlignToPixel(IsPixelAlignmentEnabled)
      .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(LocalRect.ReducePrecision))
      .SetOpacity(AbsoluteOpacity)
      .SetFill(Fill)
      .SetStroke(Stroke)
      .SetShadow(Shadow)
      .SetXRadius(-50)
      .SetYRadius(-50)
      .Draw;
    {$ELSE}
    var LRect := LocalRect.ReducePrecision;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(FRenderTargetCanvas) then
    try
      ALClearCanvas(FRenderTargetCanvas, TAlphaColors.Null);
      TALDrawRectangleHelper.Create(FRenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(LRect))
        .SetFill(Fill)
        .SetStroke(Stroke)
        .SetShadow(Shadow)
        .SetXRadius(-50)
        .SetYRadius(-50)
        .Draw;
    finally
      ALCanvasEndScene(FRenderTargetCanvas)
    end;
    ALUpdateDrawableFromSurface(FRenderTargetSurface, FRenderTargetDrawable);
    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FRenderTargetDrawable, // const ADrawable: TALDrawable;
      LRect.TopLeft, // const ADstTopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single)
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*************************************************************************}
function TALDynamicListBoxCircle.PointInObjectLocal(X, Y: Double): Boolean;
begin
  Result := False;
  if Width * Height = 0 then Exit;
  var LShapeRect := TRectF.Create(0, 0, 1, 1).FitInto(LocalRect.ReducePrecision);
  var LRadius := TSizeF.Create(LShapeRect.Width / 2, LShapeRect.Height / 2);
  var LCenter := LShapeRect.CenterPoint;
  Result := Sqr((X - LCenter.X) / LRadius.Width) + Sqr((Y - LCenter.Y) / LRadius.Height) <= 1;
end;

{******************************************}
procedure TALDynamicListBoxCircle.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{**************************************************************}
constructor TALDynamicListBoxLine.Create(const AOwner: TObject);
begin
  inherited;
  fDoubleBuffered := true;
  FLineType := TALLineType.TopLeftToBottomRight;
  FCacheIndex := 0;
  FCacheEngine := nil;
  fBufDrawable := ALNullDrawable;
end;

{***********************************************}
procedure TALDynamicListBoxLine.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not IsDestroying) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*******************************************************}
function TALDynamicListBoxLine.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{********************************************************}
function TALDynamicListBoxLine.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{***********************************************************************}
procedure TALDynamicListBoxLine.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable;
  end;
end;

{***********************************************************}
procedure TALDynamicListBoxLine.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*************************************************************}
procedure TALDynamicListBoxLine.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************************}
procedure TALDynamicListBoxLine.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     {$IF not DEFINED(ALDPK)}(not DoubleBuffered) or{$ENDIF}
     //--- Do not create BufDrawable if the size is 0
     (BoundsRect.IsEmpty) or
     //--- Do not create BufDrawable if linetype <> TALLineType.Diagonal
     (not (lineType in [TALLineType.TopLeftToBottomRight, TALLineType.BottomLeftToTopRight])) or
     //--- // Do not create BufDrawable if no stroke
     (Not Stroke.HasStroke) then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
  {$endif}

  //init fBufDrawableRect / LRect
  case lineType of
    TALLineType.TopLeftToBottomRight,
    TALLineType.BottomLeftToTopRight: fBufDrawableRect := ALAlignDimensionToPixelRound(LocalRect.ReducePrecision, ALGetScreenScale); // to have the pixel aligned width and height
    TALLineType.Top: fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Width, Stroke.Thickness), ALGetScreenScale); // to have the pixel aligned width and height
    TALLineType.Left: fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, 0, Stroke.Thickness, height), ALGetScreenScale); // to have the pixel aligned width and height
    TALLineType.Bottom: fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(0, height - Stroke.Thickness, Width, height), ALGetScreenScale); // to have the pixel aligned width and height
    TALLineType.Right: fBufDrawableRect := ALAlignDimensionToPixelRound(TrectF.Create(width - Stroke.Thickness, 0, width, height), ALGetScreenScale); // to have the pixel aligned width and height
    else
      raise Exception.Create('Error C251AE86-B150-43E8-80DE-A6D96F085AF2');
  end;
  var LRect := TrectF.Create(0, 0, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.height * ALGetScreenScale);

  {$IF DEFINED(ALSkiaEngine)}

  var LSurface: sk_surface_t;
  var LCanvas: sk_canvas_t;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALCeil(LRect.Width, TEpsilon.Position), // const w: integer;
    ALCeil(LRect.Height, TEpsilon.Position)); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      var LPaint := ALSkCheckHandle(sk4d_paint_create);
      try
        sk4d_paint_set_antialias(LPaint, true);
        sk4d_paint_set_dither(LPaint, true);

        //stroke the line
        if Stroke.HasStroke then begin

          //init LPaint
          sk4d_paint_set_stroke_width(LPaint, Stroke.Thickness * ALGetScreenScale);

          //stroke with solid color
          sk4d_paint_set_color(LPaint, Stroke.Color);
          case lineType of
            TALLineType.TopLeftToBottomRight: begin
              var Lpoint1 := TPointF.Create(LRect.left, LRect.top);
              var Lpoint2 := TPointF.Create(LRect.right, LRect.Bottom);
              sk4d_canvas_draw_line(
                LCanvas,
                @Lpoint1,
                @Lpoint2,
                LPaint);
            end;
            //--
            TALLineType.BottomLeftToTopRight: begin
              var Lpoint1 := TPointF.Create(LRect.left, LRect.Bottom);
              var Lpoint2 := TPointF.Create(LRect.right, LRect.top);
              sk4d_canvas_draw_line(
                LCanvas,
                @Lpoint1,
                @Lpoint2,
                LPaint);
            end;
            //--
            TALLineType.Top,
            TALLineType.Bottom: begin
              var Lpoint1 := TPointF.Create(LRect.left, (LRect.bottom - LRect.top) / 2);
              var Lpoint2 := TPointF.Create(LRect.right, (LRect.bottom - LRect.top) / 2);
              sk4d_canvas_draw_line(
                LCanvas,
                @Lpoint1,
                @Lpoint2,
                LPaint);
            end;
            //--
            TALLineType.Left,
            TALLineType.Right: begin
              var Lpoint1 := TPointF.Create((LRect.right - LRect.left) / 2, LRect.top);
              var Lpoint2 := TPointF.Create((LRect.right - LRect.left) / 2, LRect.bottom);
              sk4d_canvas_draw_line(
                LCanvas,
                @Lpoint1,
                @Lpoint2,
                LPaint);
            end;
          end;

        end;

      finally
        sk4d_paint_destroy(LPaint);
      end;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    fBufDrawable := ALCreateDrawableFromSurface(LSurface);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

  {$ELSEIF DEFINED(ANDROID)}

  var LSurface: Jbitmap;
  var LCanvas: Jcanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALCeil(LRect.Width, TEpsilon.Position), // const w: integer;
    ALCeil(LRect.Height, TEpsilon.Position)); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      var LPaint := TJPaint.JavaClass.init;
      LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
      LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
      LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

      //stroke the line
      if Stroke.HasStroke then begin

        //init LPaint
        LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
        LPaint.setStrokeWidth(Stroke.Thickness * ALGetScreenScale);

        //stroke with solid color
        LPaint.setColor(integer(Stroke.Color));
        case lineType of
          TALLineType.TopLeftToBottomRight: LCanvas.drawLine(
                                              LRect.left {startX},
                                              LRect.top {startY},
                                              LRect.right {stopX},
                                              LRect.Bottom {stopY},
                                              LPaint);
          TALLineType.BottomLeftToTopRight: LCanvas.drawLine(
                                              LRect.left {startX},
                                              LRect.Bottom {startY},
                                              LRect.right {stopX},
                                              LRect.Top {stopY},
                                              LPaint);
          TALLineType.Top,
          TALLineType.Bottom: LCanvas.drawLine(
                                LRect.left {startX},
                                (LRect.bottom - LRect.top) / 2 {startY},
                                LRect.right {stopX},
                                (LRect.bottom - LRect.top) / 2 {stopY},
                                LPaint);
          TALLineType.Left,
          TALLineType.Right: LCanvas.drawLine(
                               (LRect.right - LRect.left) / 2 {startX},
                               LRect.top {startY},
                               (LRect.right - LRect.left) / 2 {stopX},
                               LRect.bottom {stopY},
                               LPaint);
        end;

      end;

      //free the paint
      LPaint := nil;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    fBufDrawable := ALCreateDrawableFromSurface(LSurface);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

  {$ELSEIF DEFINED(IOS)}

  var LGridHeight := ALCeil(LRect.Height, TEpsilon.Position);
  var LSurface: CGContextRef;
  var LCanvas: CGContextRef;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALCeil(LRect.Width, TEpsilon.Position), // const w: integer;
    LGridHeight); // const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      //stroke the line
      if Stroke.HasStroke then begin

        //stroke with solid color
        var LAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
        CGContextSetRGBStrokeColor(LCanvas, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
        CGContextSetLineWidth(LCanvas, Stroke.Thickness * ALGetScreenScale);
        case lineType of
          TALLineType.TopLeftToBottomRight: begin
                                              CGContextBeginPath(LCanvas);
                                              CGContextMoveToPoint(LCanvas, LRect.left, LGridHeight - LRect.top);
                                              CGContextAddLineToPoint(LCanvas, LRect.right, LGridHeight - LRect.Bottom);
                                            end;
          TALLineType.BottomLeftToTopRight: begin
                                              CGContextBeginPath(LCanvas);
                                              CGContextMoveToPoint(LCanvas, LRect.left, LGridHeight - LRect.Bottom);
                                              CGContextAddLineToPoint(LCanvas, LRect.right, LGridHeight - LRect.top);
                                            end;
          TALLineType.Top,
          TALLineType.Bottom: begin
                              CGContextBeginPath(LCanvas);
                              CGContextMoveToPoint(LCanvas, LRect.left, LGridHeight - ((LRect.bottom - LRect.top) / 2));
                              CGContextAddLineToPoint(LCanvas, LRect.right, LGridHeight - ((LRect.bottom - LRect.top) / 2));
                            end;
          TALLineType.Left,
          TALLineType.Right: begin
                             CGContextBeginPath(LCanvas);
                             CGContextMoveToPoint(LCanvas, (LRect.right - LRect.left) / 2, LGridHeight - LRect.top);
                             CGContextAddLineToPoint(LCanvas, (LRect.right - LRect.left) / 2, LGridHeight - LRect.Bottom);
                           end;
        end;
        CGContextStrokePath(LCanvas);

      end;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    fBufDrawable := ALCreateDrawableFromSurface(LCanvas);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas); // Var aContext: CGContextRef;
  end;

  {$ENDIF}

end;

{************************************}
procedure TALDynamicListBoxLine.Paint;
begin

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if (CacheIndex <= 0) or
     (CacheEngine = nil) or
     (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
    MakeBufDrawable;
    if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
      if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
      else fBufDrawable := ALNullDrawable;
      if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) and Stroke.HasStroke then begin
    var LPt1, LPt2: TPointF;
    case lineType of
      TALLineType.TopLeftToBottomRight: begin
                                          LPt1 := TpointF.Create(0,     0);
                                          LPt2 := TpointF.Create(Width, Height);
                                        end;
      TALLineType.BottomLeftToTopRight: begin
                                          LPt1 := TpointF.Create(0,     Height);
                                          LPt2 := TpointF.Create(Width, 0);
                                        end;
      TALLineType.Top: begin
                       LPt1 := TpointF.Create((Stroke.Thickness / 2),         Stroke.Thickness / 2);
                       LPt2 := TpointF.Create(Width - (Stroke.Thickness / 2), Stroke.Thickness / 2);
                     end;
      TALLineType.Left: begin
                        LPt1 := TpointF.Create(Stroke.Thickness / 2, Stroke.Thickness / 2);
                        LPt2 := TpointF.Create(Stroke.Thickness / 2, height-(Stroke.Thickness / 2));
                      end;
      TALLineType.Bottom: begin
                          LPt1 := TpointF.Create((Stroke.Thickness / 2),         height-(Stroke.Thickness / 2));
                          LPt2 := TpointF.Create(Width - (Stroke.Thickness / 2), height-(Stroke.Thickness / 2));
                        end;
      TALLineType.Right: begin
                         LPt1 := TpointF.Create(Width-(Stroke.Thickness / 2), Stroke.Thickness / 2);
                         LPt2 := TpointF.Create(Width-(Stroke.Thickness / 2), height-(Stroke.Thickness / 2));
                       end;
      else
        raise Exception.Create('Error E353EE34-4D44-4487-9C1C-21BC44E36B40');
    end;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := Stroke.Color;
    Canvas.Stroke.Thickness := Stroke.Thickness;
    Canvas.DrawLine(
      ALAlignToPixelRound(LPt1, Canvas.Matrix, Canvas.Scale, TEpsilon.position),
      ALAlignToPixelRound(LPt2, Canvas.Matrix, Canvas.Scale, TEpsilon.position),
      AbsoluteOpacity);
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{****************************************}
procedure TALDynamicListBoxLine.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{********************************************************************}
procedure TALDynamicListBoxLine.SetLineType(const Value: TALLineType);
begin
  if FLineType <> Value then
  begin
    ClearBufDrawable;
    FLineType := Value;
    Repaint;
  end;
end;

{********************************************************************}
function TALDynamicListBoxBaseText.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{**********************************************************************}
function TALDynamicListBoxBaseText.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{******************************************************************}
constructor TALDynamicListBoxBaseText.Create(const AOwner: TObject);
begin
  inherited Create(AOwner);
  //-----
  fDoubleBuffered := true;
  FMultiLineTextOptions := TALMultiLineTextOptions.Create;
  //-----
  FOnElementClick := nil;
  FOnElementMouseDown := nil;
  FOnElementMouseMove := nil;
  FOnElementMouseUp := nil;
  FOnElementMouseEnter := nil;
  FOnElementMouseLeave := nil;
  // No need to do this, as it was already initialized via InitInstance.
  // It's a slightly expensive operation.
  //FHoveredElement := TALTextElement.Empty;
  //FPressedElement := TALTextElement.Empty;
  //-----
  FCacheIndex := 0;
  FCacheEngine := nil;
  //-----
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  fBufDrawable := ALNullDrawable;
  //-----
  fTextBroken := False;
  fAllTextDrawn := False;
  SetLength(fElements, 0);
  //-----
  FCorners := AllCorners;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FSides := AllSides;
  //-----
  HitTest := False;
  //-----
  FAutoTranslate := true;
  FMaxWidth := 65535;
  FMaxHeight := 65535;
  FText := '';
  //-----
  FTextSettings := CreateTextSettings;
  FTextSettings.OnChanged := TextSettingsChanged;
end;

{*******************************************}
destructor TALDynamicListBoxBaseText.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FMultiLineTextOptions);
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{******************************************************}
function TALDynamicListBoxBaseText.CreateFill: TALBrush;
begin
  result := TFill.Create;
end;

{**************************************************************}
function TALDynamicListBoxBaseText.CreateStroke: TALStrokeBrush;
begin
  result := TStroke.Create;
end;

{*******************************************}
//procedure TALDynamicListBoxBaseText.Loaded;
//begin
//  {$IF defined(ALBackwardCompatible)}
//  if ALSametextW(TextSettings.Font.Family, 'sans-serif-thin') then begin
//    TextSettings.Font.Family := 'sans-serif';
//    if TextSettings.Font.Weight = TFontWeight.Bold then TextSettings.Font.Weight := TFontWeight.regular
//    else TextSettings.Font.Weight := TFontWeight.thin;
//  end
//  else if ALSametextW(TextSettings.Font.Family, 'sans-serif-light') then begin
//    TextSettings.Font.Family := 'sans-serif';
//    if TextSettings.Font.Weight = TFontWeight.Bold then TextSettings.Font.Weight := TFontWeight.Semibold
//    else TextSettings.Font.Weight := TFontWeight.light;
//  end
//  else if ALSametextW(TextSettings.Font.Family, 'sans-serif-medium') then begin
//    TextSettings.Font.Family := 'sans-serif';
//    if TextSettings.Font.Weight = TFontWeight.Bold then TextSettings.Font.Weight := TFontWeight.UltraBold
//    else TextSettings.Font.Weight := TFontWeight.medium;
//  end
//  else if ALSametextW(TextSettings.Font.Family, 'sans-serif-black') then begin
//    TextSettings.Font.Family := 'sans-serif';
//    if TextSettings.Font.Weight = TFontWeight.Bold then TextSettings.Font.Weight := TFontWeight.UltraBlack
//    else TextSettings.Font.Weight := TFontWeight.black;
//  end;
//  {$ENDIF}
//
//  if (AutoTranslate) and
//     (Text <> '') and
//     (not (csDesigning in ComponentState)) then
//    Text := ALTranslate(Text);
//
//  if (TextSettings.Font.Family <> '') and
//     (not (csDesigning in ComponentState)) then
//    TextSettings.Font.Family := ALConvertFontFamily(TextSettings.Font.Family);
//
//  if (TextSettings.EllipsisSettings.Font.Family <> '') and
//     (not (csDesigning in ComponentState)) then
//    TextSettings.EllipsisSettings.Font.Family := ALConvertFontFamily(TextSettings.EllipsisSettings.Font.Family);
//
//  inherited Loaded;
//end;

{***********************************************}
procedure TALDynamicListBoxBaseText.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    MaxWidth := ALAlignDimensionToPixelRound(MaxWidth, ALGetScreenScale, Tepsilon.position);
    MaxHeight := ALAlignDimensionToPixelRound(MaxHeight, ALGetScreenScale, Tepsilon.position);
    TextSettings.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{********************************************}
procedure TALDynamicListBoxBaseText.DoResized;
begin
  if not FIsAdjustingSize then begin
    ClearBufDrawable;
    inherited;
  end
  else
    inherited;
end;

{*********************************************}
procedure TALDynamicListBoxBaseText.AdjustSize;
begin
  if //(not (csLoading in ComponentState)) and // loaded will call again AdjustSize
     (not IsDestroying) and // if csDestroying do not do autosize
     (HasUnconstrainedAutosizeX or HasUnconstrainedAutosizeY) and // if AutoSize is false nothing to adjust
     (Text <> '') and // if Text is empty do not do autosize
     //(scene <> nil) and // SetNewScene will call again AdjustSize
     (TNonReentrantHelper.EnterSection(FIsAdjustingSize)) then begin // non-reantrant
    try

      if isupdating then begin
        FAdjustSizeOnEndUpdate := True;
        Exit;
      end
      else
        FAdjustSizeOnEndUpdate := False;

      {$IF defined(debug)}
      //ALLog(ClassName + '.AdjustSize', 'Name: ' + Name + ' | HasUnconstrainedAutosize(X/Y) : '+ALBoolToStrW(HasUnconstrainedAutosizeX)+'/'+ALBoolToStrW(HasUnconstrainedAutosizeY));
      {$ENDIF}

      var R: TrectF;
      If {$IF not DEFINED(ALDPK)}DoubleBuffered{$ELSE}True{$ENDIF} then begin
        if (CacheIndex > 0) and (CacheEngine <> nil) then begin
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, R{ARect}) then begin
            MakeBufDrawable;
            R := FBufDrawableRect;
          end;
        end
        else begin
          MakeBufDrawable;
          R := FBufDrawableRect;
        end;
      end
      else begin
        MeasureMultilineText(
          R, // out ARect: TRectF;
          FTextBroken, // out ATextBroken: Boolean;
          FAllTextDrawn, // out AAllTextDrawn: Boolean;
          FElements, // out AElements: TALTextElements;
          1, // const AScale: Single;
          Text, // const AText: String;
          TextSettings.Font, // const AFont: TALFont;
          TextSettings.Decoration, // const ADecoration: TALTextDecoration;
          TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
          TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
          Fill, // const AFill: TALBrush;
          nil, // const AStateLayer: TALStateLayer;
          Stroke, // const AStroke: TALStrokeBrush;
          Shadow); // const AShadow: TALShadow);
      end;

      if not HasUnconstrainedAutosizeX then begin
        r.Left := 0;
        r.Width := Width;
      end;
      if not HasUnconstrainedAutosizeY then begin
        r.Top := 0;
        r.height := height;
      end;

      SetFixedSizeBounds(Left, Top, R.Width, R.Height);

    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{**************************************************}
procedure TALDynamicListBoxBaseText.BeginTextUpdate;
begin
  BeginUpdate;
  Inherited;
end;

{************************************************}
procedure TALDynamicListBoxBaseText.EndTextUpdate;
begin
  EndUpdate;
  Inherited;
end;

{**************************************************************************************}
function TALDynamicListBoxBaseText.GetElementAtPos(const APos: TPointF): TALTextElement;
begin
  for var I := Low(fElements) to High(fElements) do
    if fElements[i].Rect.Contains(APos) then begin
      Result := fElements[i];
      Exit;
    end;
  Result.Id := '';
  Result.Rect := TrectF.Empty;
end;

{****************************************************************************************************}
procedure TALDynamicListBoxBaseText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(FOnElementMouseDown) or
     assigned(FOnElementMouseUp) or
     assigned(FOnElementClick) then begin

    FPressedElement := GetElementAtPos(TPointF.Create(X, Y));
    if assigned(FOnElementMouseDown) and (FPressedElement.ID <> '') then
      FOnElementMouseDown(Self, Button, Shift, X, Y, FPressedElement);

  end;

  inherited MouseDown(Button, Shift, X, Y);
end;

{******************************************************************************}
procedure TALDynamicListBoxBaseText.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited MouseMove(Shift, X, Y);

  if (assigned(FOnElementMouseMove) or
      assigned(FOnElementMouseEnter) or
      assigned(FOnElementMouseLeave)) then begin

    var LElement := GetElementAtPos(TPointF.Create(X, Y));
    if (FHoveredElement.ID <> LElement.ID) or (FHoveredElement.Rect <> LElement.Rect) then begin
      if assigned(FOnElementMouseLeave) and (FHoveredElement.ID <> '') then
        FOnElementMouseLeave(self, FHoveredElement);
      FHoveredElement := LElement;
      if assigned(FOnElementMouseEnter) and (FHoveredElement.ID <> '') then
        FOnElementMouseEnter(self, FHoveredElement);
    end;
    if assigned(FOnElementMouseMove) and (FHoveredElement.ID <> '') then
      FOnElementMouseMove(self, Shift, X, Y, LElement);

  end;
end;

{**************************************************************************************************}
procedure TALDynamicListBoxBaseText.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if assigned(FOnElementMouseUp) and (FPressedElement.ID <> '') then
    FOnElementMouseUp(Self, Button, Shift, X, Y, FPressedElement);
  FPressedElement := TALTextElement.Empty;
end;

{*********************************************}
procedure TALDynamicListBoxBaseText.MouseEnter;
begin
  FHoveredElement := TALTextElement.Empty;
  inherited MouseEnter;
end;

{*********************************************}
procedure TALDynamicListBoxBaseText.MouseLeave;
begin
  inherited MouseLeave;
  if assigned(FOnElementMouseLeave) and (FHoveredElement.ID <> '') then
    FOnElementMouseLeave(self, FHoveredElement);
  FHoveredElement := TALTextElement.Empty;
end;

{****************************************}
procedure TALDynamicListBoxBaseText.Click;
begin
  inherited Click;
  if assigned(FOnElementClick) and (FPressedElement.ID <> '') then
    FOnElementClick(Self, FPressedElement);
end;

{*************************************************}
procedure TALDynamicListBoxBaseText.PaddingChanged;
begin
  ClearBufDrawable;
  inherited;
  Repaint;
end;

{***************************************************************}
procedure TALDynamicListBoxBaseText.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*****************************************************************}
procedure TALDynamicListBoxBaseText.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*****************************************************************}
procedure TALDynamicListBoxBaseText.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************************************}
function TALDynamicListBoxBaseText.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{********************************************************}
function TALDynamicListBoxBaseText.IsSidesStored: Boolean;
begin
  Result := FSides <> AllSides
end;

{********************************************************************}
procedure TALDynamicListBoxBaseText.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then begin
    ClearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{***********************************************************}
function TALDynamicListBoxBaseText.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{***********************************************************}
function TALDynamicListBoxBaseText.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{******************************************************************}
procedure TALDynamicListBoxBaseText.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{******************************************************************}
procedure TALDynamicListBoxBaseText.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{****************************************************************}
procedure TALDynamicListBoxBaseText.SetSides(const Value: TSides);
begin
  if FSides <> Value then begin
    ClearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{***********************************************************************}
procedure TALDynamicListBoxBaseText.TextSettingsChanged(Sender: TObject);
begin
  ClearBufDrawable;
  AdjustSize;
  Repaint;
end;

{************************************************************************************}
procedure TALDynamicListBoxBaseText.SetTextSettings(const Value: TALBaseTextSettings);
begin
  FTextSettings.Assign(Value);
end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALDynamicListBoxBaseText.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  Result := ALGetShapeSurfaceRect(
              ARect, // const ARect: TRectF;
              Fill, // const AFill: TALBrush;
              nil, // const AFillResourceStream: TStream;
              nil, // const AStateLayer: TALStateLayer;
              Shadow); // const AShadow: TALShadow): TRectF;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxBaseText.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxBaseText.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{****************************************}
procedure TALDynamicListBoxBaseText.Paint;
begin

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if (CacheIndex <= 0) or
     (CacheEngine = nil) or
     (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
    MakeBufDrawable;
    if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
      if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
      else fBufDrawable := ALNullDrawable;
      if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
        raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    var LRect := LocalRect.ReducePrecision;
    DrawMultilineText(
      TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
      LRect, // out ARect: TRectF;
      FTextBroken, // out ATextBroken: Boolean;
      FAllTextDrawn, // out AAllTextDrawn: Boolean;
      FElements, // out AElements: TALTextElements;
      1, // const AScale: Single;
      AbsoluteOpacity, // const AOpacity: Single;
      Text, // const AText: String;
      TextSettings.Font, // const AFont: TALFont;
      TextSettings.Decoration, // const ADecoration: TALTextDecoration;
      TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
      TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
      Fill, // const AFill: TALBrush;
      nil, // const AStateLayer: TALStateLayer;
      Stroke, // const AStroke: TALStrokeBrush;
      Shadow); // const AShadow: TALShadow);
    {$ELSE}
    var LRect := LocalRect.ReducePrecision;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(FRenderTargetCanvas) then
    try
      ALClearCanvas(FRenderTargetCanvas, TAlphaColors.Null);
      DrawMultilineText(
        FRenderTargetCanvas, // const ACanvas: TALCanvas;
        LRect, // out ARect: TRectF;
        FTextBroken, // out ATextBroken: Boolean;
        FAllTextDrawn, // out AAllTextDrawn: Boolean;
        FElements, // out AElements: TALTextElements;
        ALGetScreenScale, // const AScale: Single;
        1, // const AOpacity: Single;
        Text, // const AText: String;
        TextSettings.Font, // const AFont: TALFont;
        TextSettings.Decoration, // const ADecoration: TALTextDecoration;
        TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
        TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
        Fill, // const AFill: TALBrush;
        nil, // const AStateLayer: TALStateLayer;
        Stroke, // const AStroke: TALStrokeBrush;
        Shadow); // const AShadow: TALShadow);
    finally
      ALCanvasEndScene(FRenderTargetCanvas)
    end;
    ALUpdateDrawableFromSurface(FRenderTargetSurface, FRenderTargetDrawable);
    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      FRenderTargetDrawable, // const ADrawable: TALDrawable;
      LRect.TopLeft, // const ADstTopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single)
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

  //if (csDesigning in ComponentState) and not Locked then
  //  DrawDesignBorder;

end;

{***********************************************************}
function TALDynamicListBoxBaseText.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{************************************************************}
function TALDynamicListBoxBaseText.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{***************************************************************************}
procedure TALDynamicListBoxBaseText.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{************************************************************************}
procedure TALDynamicListBoxBaseText.SetAlign(const Value: TALAlignLayout);
begin
  if Align <> Value then ClearBufDrawable;
  inherited SetAlign(Value);
end;

{********************************************************************}
procedure TALDynamicListBoxBaseText.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    ClearBufDrawable;
    Repaint;
    inherited;
  end;
end;

{***************************************************************}
procedure TALDynamicListBoxBaseText.SetText(const Value: string);
begin
  if FText <> Value then begin
    ClearBufDrawable;
    FText := Value;
    AdjustSize;
    Repaint;
  end;
end;

{***************************************************}
procedure TALDynamicListBoxBaseText.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (not IsDestroying) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*********************************************************}
function TALDynamicListBoxBaseText.GetMultiLineTextOptions(
           const AScale: Single;
           const AOpacity: Single;
           const AFont: TALFont;
           const ADecoration: TALTextDecoration;
           const AEllipsisFont: TALFont;
           const AEllipsisDecoration: TALTextDecoration;
           const AFill: TALBrush;
           const AStateLayer: TALStateLayer;
           const AStroke: TALStrokeBrush;
           const AShadow: TALShadow): TALMultiLineTextOptions;
begin
  Result := FMultiLineTextOptions;
  Result.Scale := AScale;
  Result.AlignToPixel := IsPixelAlignmentEnabled;
  Result.Opacity := AOpacity;
  //--
  Result.FontFamily := Afont.Family;
  Result.FontSize := Afont.Size;
  Result.FontWeight := Afont.Weight;
  Result.FontSlant := Afont.Slant;
  Result.FontStretch := Afont.Stretch;
  Result.FontColor := AFont.Color;
  //--
  Result.DecorationKinds := ADecoration.Kinds;
  Result.DecorationStyle := ADecoration.Style;
  Result.DecorationThicknessMultiplier := ADecoration.ThicknessMultiplier;
  Result.DecorationColor := ADecoration.Color;
  //--
  Result.EllipsisText := TextSettings.Ellipsis;
  Result.EllipsisInheritSettings := TextSettings.EllipsisSettings.inherit;
  //--
  Result.EllipsisFontFamily := AEllipsisfont.Family;
  Result.EllipsisFontSize := AEllipsisfont.Size;
  Result.EllipsisFontWeight := AEllipsisfont.Weight;
  Result.EllipsisFontSlant := AEllipsisfont.Slant;
  Result.EllipsisFontStretch := AEllipsisfont.Stretch;
  Result.EllipsisFontColor := AEllipsisFont.Color;
  //--
  Result.EllipsisDecorationKinds := AEllipsisDecoration.Kinds;
  Result.EllipsisDecorationStyle := AEllipsisDecoration.Style;
  Result.EllipsisDecorationThicknessMultiplier := AEllipsisDecoration.ThicknessMultiplier;
  Result.EllipsisDecorationColor := AEllipsisDecoration.Color;
  //--
  Result.AutoSize := False;
  Result.AutoSizeX := False;
  Result.AutoSizeY := False;
  if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then Result.AutoSize := True
  else if HasUnconstrainedAutosizeX then Result.AutoSizeX := True
  else if HasUnconstrainedAutosizeY then Result.AutoSizeY := True;
  //--
  Result.MaxLines := TextSettings.MaxLines;
  Result.LineHeightMultiplier := TextSettings.LineHeightMultiplier;
  Result.LetterSpacing := TextSettings.LetterSpacing;
  Result.Trimming := TextSettings.Trimming;
  Result.FailIfTextBroken := false;
  //--
  if TFillTextFlag.RightToLeft in FillTextFlags then Result.Direction := TALTextDirection.RightToLeft
  else Result.Direction := TALTextDirection.LeftToRight;
  Result.HTextAlign := TextSettings.HorzAlign;
  Result.VTextAlign := TextSettings.VertAlign;
  //--
  Result.FillColor := AFill.Color;
  Result.FillGradientStyle := AFill.Gradient.Style;
  Result.FillGradientAngle := AFill.Gradient.Angle;
  Result.FillGradientColors := AFill.Gradient.Colors;
  Result.FillGradientOffsets := AFill.Gradient.Offsets;
  Result.FillResourceName := AFill.ResourceName;
  Result.FillMaskResourceName := '';
  Result.FillMaskBitmap := ALNullBitmap;
  Result.FillBackgroundMargins := AFill.BackgroundMargins.Rect;
  Result.FillImageMargins := AFill.ImageMargins.Rect;
  Result.FillImageNoRadius := AFill.ImageNoRadius;
  Result.FillWrapMode := AFill.WrapMode;
  Result.FillCropCenter := TPointF.create(-50,-50);
  Result.FillBlurRadius := 0;
  //--
  if AStateLayer <> nil then begin
    Result.StateLayerOpacity := AStateLayer.Opacity;
    if AStateLayer.UseContentColor then Result.StateLayerColor := AFont.Color
    else Result.StateLayerColor := AStateLayer.Color;
    Result.StateLayerMargins := AStateLayer.Margins.Rect;
    Result.StateLayerXRadius := AStateLayer.XRadius;
    Result.StateLayerYRadius := AStateLayer.YRadius;
  end
  else begin
    Result.StateLayerOpacity := 0;
    Result.StateLayerColor := TalphaColors.Null;
    Result.StateLayerMargins := TRectF.Empty;
    Result.StateLayerXRadius := 0;
    Result.StateLayerYRadius := 0;
  end;
  //--
  Result.StrokeColor := AStroke.Color;
  Result.StrokeThickness := AStroke.Thickness;
  //--
  Result.ShadowColor := AShadow.Color;
  Result.ShadowBlur := AShadow.Blur;
  Result.ShadowOffsetX := AShadow.OffsetX;
  Result.ShadowOffsetY := AShadow.OffsetY;
  //--
  Result.Sides := Sides;
  Result.XRadius := XRadius;
  Result.YRadius := YRadius;
  Result.Corners := Corners;
  Result.Padding := padding.Rect;
  //--
  Result.TextIsHtml := TextSettings.IsHtml;
  //--
  Result.OnAdjustRect := DrawMultilineTextAdjustRect;
  Result.OnBeforeDrawBackground := DrawMultilineTextBeforeDrawBackground;
  Result.OnBeforeDrawParagraph := DrawMultilineTextBeforeDrawParagraph;
end;

{******************************************************************************************************************************************************************************}
Procedure TALDynamicListBoxBaseText.DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF);
begin
  // Virtual
end;

{****************************************************************************************************************************************************************}
Procedure TALDynamicListBoxBaseText.DrawMultilineTextBeforeDrawBackground(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF);
begin
  // Virtual
end;

{***************************************************************************************************************************************************************}
Procedure TALDynamicListBoxBaseText.DrawMultilineTextBeforeDrawParagraph(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF);
begin
  // Virtual
end;

{****************************************************}
Procedure TALDynamicListBoxBaseText.DrawMultilineText(
            const ACanvas: TALCanvas;
            var ARect: TRectF;
            out ATextBroken: Boolean;
            out AAllTextDrawn: Boolean;
            out AElements: TALTextElements;
            const AScale: Single;
            const AOpacity: Single;
            const AText: String;
            const AFont: TALFont;
            const ADecoration: TALTextDecoration;
            const AEllipsisFont: TALFont;
            const AEllipsisDecoration: TALTextDecoration;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if ALIsCanvasNull(ACanvas) then
    Raise Exception.Create('Canvas cannot be null');

  var LMultiLineTextOptions := GetMultiLineTextOptions(
                                 AScale,
                                 AOpacity,
                                 AFont,
                                 ADecoration,
                                 AEllipsisFont,
                                 AEllipsisDecoration,
                                 AFill,
                                 AStateLayer,
                                 AStroke,
                                 AShadow);

  if (AText <> '') then begin

    var LMaxSize: TSizeF;
    if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(maxWidth, maxHeight)
    else if HasUnconstrainedAutosizeX then LMaxSize := TSizeF.Create(maxWidth, Height)
    else if HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(Width, maxHeight)
    else LMaxSize := TSizeF.Create(width, height);

    ARect.Width := LMaxSize.cX;
    ARect.Height := LMaxSize.cY;

    var LSurface: TALSurface := ALNullSurface;
    var LCanvas: TALCanvas := ACanvas;
    ALDrawMultiLineText(
      LSurface,
      LCanvas,
      AText,
      ARect,
      ATextBroken,
      AAllTextDrawn,
      AElements,
      LMultiLineTextOptions);

  end
  else begin

    ARect.Width := Min(MaxWidth, ARect.Width);
    ARect.Height := Min(MaxHeight, ARect.Height);

    Var LSurfaceSize := ARect.Size;
    DrawMultilineTextAdjustRect(
      ACanvas, // const ACanvas: TALCanvas;
      LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
      ARect, // var ARect: TrectF;
      LSurfaceSize); // var ASurfaceSize: TSizeF

    DrawMultilineTextBeforeDrawBackground(
      ACanvas, //const ACanvas: TALCanvas;
      LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
      ARect); // Const ARect: TrectF

    TALDrawRectangleHelper.Create(ACanvas)
      .SetScale(AScale)
      .SetAlignToPixel(IsPixelAlignmentEnabled)
      .SetDstRect(ARect)
      .SetOpacity(AOpacity)
      .SetFill(AFill)
      .SetStateLayer(AStateLayer, AFont.color)
      .SetDrawStateLayerOnTop(AText <> '')
      .SetStroke(AStroke)
      .SetShadow(AShadow)
      .SetSides(Sides)
      .SetCorners(Corners)
      .SetXRadius(XRadius)
      .SetYRadius(YRadius)
      .Draw;

    DrawMultilineTextBeforeDrawParagraph(
      ACanvas, //const ACanvas: TALCanvas;
      LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
      ARect); // Const ARect: TrectF

  end;

end;

{*******************************************************}
Procedure TALDynamicListBoxBaseText.MeasureMultilineText(
            out ARect: TRectF;
            out ATextBroken: Boolean;
            out AAllTextDrawn: Boolean;
            out AElements: TALTextElements;
            const AScale: Single;
            const AText: String;
            const AFont: TALFont;
            const ADecoration: TALTextDecoration;
            const AEllipsisFont: TALFont;
            const AEllipsisDecoration: TALTextDecoration;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin
  var LMaxSize: TSizeF;
  if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(maxWidth, maxHeight)
  else if HasUnconstrainedAutosizeX then LMaxSize := TSizeF.Create(maxWidth, Height)
  else if HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(Width, maxHeight)
  else LMaxSize := TSizeF.Create(width, height);

  ARect := TRectF.Create(0, 0, LMaxSize.cX, LMaxSize.cY);

  ALMeasureMultiLineText(
    AText,
    ARect,
    ATextBroken,
    AAllTextDrawn,
    AElements,
    GetMultiLineTextOptions(
      AScale,
      AbsoluteOpacity,
      AFont,
      ADecoration,
      AEllipsisFont,
      AEllipsisDecoration,
      AFill,
      AStateLayer,
      AStroke,
      AShadow));
end;

{****************************************************}
Procedure TALDynamicListBoxBaseText.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            out ATextBroken: Boolean;
            out AAllTextDrawn: Boolean;
            out AElements: TALTextElements;
            const AScale: Single;
            const AText: String;
            const AFont: TALFont;
            const ADecoration: TALTextDecoration;
            const AEllipsisFont: TALFont;
            const AEllipsisDecoration: TALTextDecoration;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  var LMultiLineTextOptions := GetMultiLineTextOptions(
                                 AScale,
                                 1{AOpacity},
                                 AFont,
                                 ADecoration,
                                 AEllipsisFont,
                                 AEllipsisDecoration,
                                 AFill,
                                 AStateLayer,
                                 AStroke,
                                 AShadow);

  if (AText <> '') then begin

    var LMaxSize: TSizeF;
    if HasUnconstrainedAutosizeX and HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(maxWidth, maxHeight)
    else if HasUnconstrainedAutosizeX then LMaxSize := TSizeF.Create(maxWidth, Height)
    else if HasUnconstrainedAutosizeY then LMaxSize := TSizeF.Create(Width, maxHeight)
    else LMaxSize := TSizeF.Create(width, height);

    ABufDrawableRect := TRectF.Create(0, 0, LMaxSize.cX, LMaxSize.cY);
    ABufDrawable := ALCreateMultiLineTextDrawable(
                      AText,
                      ABufDrawableRect,
                      ATextBroken,
                      AAllTextDrawn,
                      AElements,
                      LMultiLineTextOptions);

    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  end;

  if (ALIsDrawableNull(ABufDrawable)) then begin

    if (AText <> '') or (LocalRect.IsEmpty) then begin
      ABufDrawable := ALCreateEmptyDrawable1x1;
      ABufDrawableRect := TRectF.Create(0,0,1/AScale,1/AScale);
      exit;
    end;

    ABufDrawableRect := LocalRect.ReducePrecision;
    var LSurfaceRect := ALGetShapeSurfaceRect(
                          ABufDrawableRect, // const ARect: TRectF;
                          AFill, // const AFill: TALBrush;
                          nil, // const AFillResourceStream: TStream;
                          AStateLayer, // const AStateLayer: TALStateLayer;
                          AShadow); // const AShadow: TALShadow): TRectF;
    LSurfaceRect.Width := Min(MaxWidth, LSurfaceRect.Width);
    LSurfaceRect.Height := Min(MaxHeight, LSurfaceRect.Height);
    ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

    Var LSurfaceSize := LSurfaceRect.Size;
    DrawMultilineTextAdjustRect(
      ALNullCanvas, // const ACanvas: TALCanvas;
      LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
      ABufDrawableRect, // var ARect: TrectF;
      LSurfaceSize); // var ASurfaceSize: TSizeF
    LSurfaceRect.Width := LSurfaceSize.Width;
    LSurfaceRect.Height := LSurfaceSize.Height;

    var LSurface: TALSurface;
    var LCanvas: TALCanvas;
    ALCreateSurface(
      LSurface, // out ASurface: TALSurface;
      LCanvas, // out ACanvas: TALCanvas;
      AScale, // const AScale: Single;
      LSurfaceRect.Width, // const w: integer;
      LSurfaceRect.height);// const h: integer)
    try

      if ALCanvasBeginScene(LCanvas) then
      try

        DrawMultilineTextBeforeDrawBackground(
          LCanvas, //const ACanvas: TALCanvas;
          LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
          ABufDrawableRect); // Const ARect: TrectF

          TALDrawRectangleHelper.Create(LCanvas)
            .SetScale(AScale)
            .SetAlignToPixel(IsPixelAlignmentEnabled)
            .SetDstRect(ABufDrawableRect)
            .SetFill(AFill)
            .SetStateLayer(AStateLayer, AFont.color)
            .SetDrawStateLayerOnTop(Atext <> '')
            .SetStroke(AStroke)
            .SetShadow(AShadow)
            .SetSides(Sides)
            .SetCorners(Corners)
            .SetXRadius(XRadius)
            .SetYRadius(YRadius)
            .Draw;

        DrawMultilineTextBeforeDrawParagraph(
          LCanvas, //const ACanvas: TALCanvas;
          LMultiLineTextOptions, // const AOptions: TALMultiLineTextOptions;
          ABufDrawableRect); // Const ARect: TrectF

      finally
        ALCanvasEndScene(LCanvas)
      end;

      ABufDrawable := ALCreateDrawableFromSurface(LSurface);
      // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
      // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
      ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

    finally
      ALFreeAndNilSurface(LSurface, LCanvas);
    end;

  end;

end;

{**************************************************}
procedure TALDynamicListBoxBaseText.MakeBufDrawable;
begin

  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex})) then Exit;

  {$IFDEF debug}
  ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
  {$endif}

  CreateBufDrawable(
    FBufDrawable, // var ABufDrawable: TALDrawable;
    FBufDrawableRect, // out ABufDrawableRect: TRectF;
    FTextBroken, // out ATextBroken: Boolean;
    FAllTextDrawn, // out AAllTextDrawn: Boolean;
    FElements, // out AElements: TALTextElements;
    ALGetScreenScale, // const AScale: Single;
    Text, // const AText: String;
    TextSettings.Font, // const AFont: TALFont;
    TextSettings.Decoration, // const ADecoration: TALTextDecoration;
    TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
    TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
    Fill, // const AFill: TALBrush;
    nil, // const AStateLayer: TALStateLayer;
    Stroke, // const AStroke: TALStrokeBrush;
    Shadow); // const AShadow: TALShadow);

end;

{*****************************************************}
function TALDynamicListBoxBaseText.TextBroken: Boolean;
begin
  result := fTextBroken;
end;

{*******************************************************************}
procedure TALDynamicListBoxBaseText.SetMaxWidth(const Value: Single);
begin
  if compareValue(fMaxWidth, Value, Tepsilon.position) <> 0 then begin
    ClearBufDrawable;
    fMaxWidth := Value;
    AdjustSize;
  end;
end;

{********************************************************************}
procedure TALDynamicListBoxBaseText.SetMaxHeight(const Value: Single);
begin
  if compareValue(fMaxHeight, Value, Tepsilon.position) <> 0 then begin
    ClearBufDrawable;
    fMaxHeight := Value;
    AdjustSize;
  end;
end;

{***********************************************************}
function TALDynamicListBoxBaseText.IsMaxWidthStored: Boolean;
begin
  result := compareValue(fMaxWidth, 65535, Tepsilon.position) <> 0;
end;

{************************************************************}
function TALDynamicListBoxBaseText.IsMaxHeightStored: Boolean;
begin
  result := compareValue(fMaxHeight, 65535, Tepsilon.position) <> 0;
end;

{**********************************************************}
function TALDynamicListBoxBaseText.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{**********************************************************}
function TALDynamicListBoxBaseText.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{***********************************}
//{$IF defined(ALBackwardCompatible)}
//procedure TALDynamicListBoxText.DefineProperties(Filer: TFiler);
//begin
//  inherited;
//  Filer.DefineProperty('TextIsHtml', ReadTextIsHtml{ReadData}, nil{WriteData}, false{hasdata});
//  Filer.DefineProperty('LineSpacing', ReadLineSpacing{ReadData}, nil{WriteData}, false{hasdata});
//end;
//{$ENDIF}

{***********************************}
//{$IF defined(ALBackwardCompatible)}
//procedure TALDynamicListBoxText.ReadTextIsHtml(Reader: TReader);
//begin
//  TextSettings.IsHtml := Reader.ReadBoolean;
//end;
//{$ENDIF}

{***********************************}
//{$IF defined(ALBackwardCompatible)}
//procedure TALDynamicListBoxText.ReadLineSpacing(Reader: TReader);
//begin
//  var LLineSpacing: Extended := Reader.ReadFloat;
//  // Luckily, TextSettings was defined before LineSpacing,
//  // so its properties are already read.
//  var LFontFamily := TextSettings.Font.Family;
//  if LFontFamily = '' then LFontFamily := 'sans-serif';
//  var LFontWeight := TextSettings.Font.Weight;
//  // There is a slight discrepancy in line height between platforms: Windows (Segoe UI),
//  // iOS (Helvetica Neue), and Android (Roboto). However, the line spacing remains consistent
//  // across all operating systems. Given the need to choose a font, we prefer to use the
//  // Roboto (Android) font for consistency.
//  if ALSametextW(LFontFamily, 'sans-serif') then LFontFamily := 'Roboto'
//  else if ALSametextW(LFontFamily, 'sans-serif-thin') then begin
//    LFontFamily := 'Roboto';
//    if LFontWeight = TFontWeight.Bold then LFontWeight := TFontWeight.regular
//    else LFontWeight := TFontWeight.thin;
//  end
//  else if ALSametextW(LFontFamily, 'sans-serif-light') then begin
//    LFontFamily := 'Roboto';
//    if LFontWeight = TFontWeight.Bold then LFontWeight := TFontWeight.Semibold
//    else LFontWeight := TFontWeight.light;
//  end
//  else if ALSametextW(LFontFamily, 'sans-serif-medium') then begin
//    LFontFamily := 'Roboto';
//    if LFontWeight = TFontWeight.Bold then LFontWeight := TFontWeight.UltraBold
//    else LFontWeight := TFontWeight.medium;
//  end
//  else if ALSametextW(LFontFamily, 'sans-serif-black') then begin
//    LFontFamily := 'Roboto';
//    if LFontWeight = TFontWeight.Bold then LFontWeight := TFontWeight.UltraBlack
//    else LFontWeight := TFontWeight.black;
//  end
//  else LFontFamily := ALConvertFontFamily(LFontFamily);
//  var LFontMetrics := ALGetFontMetrics(
//                        LFontFamily, // TextSettings const AFontFamily: String;
//                        TextSettings.Font.Size, // const AFontSize: single;
//                        LFontWeight, // const AFontWeight: TFontWeight;
//                        TextSettings.Font.Slant); // const AFontSlant: TFontSlant;
//  var LLineHeight := -LFontMetrics.Ascent + LFontMetrics.Descent;
//  // The LineHeightMultiplier property allows manual adjustment
//  // of the height of the line as a multiple of fontSize.
//  TextSettings.LineHeightMultiplier := RoundTo((LLineSpacing + LLineHeight) / TextSettings.Font.Size, -1);
//end;
//{$ENDIF}

{*********************************************************************}
function TALDynamicListBoxText.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TALTextSettings.Create;
end;

{**************************************************************}
function TALDynamicListBoxText.GetTextSettings: TALTextSettings;
begin
  Result := TALTextSettings(Inherited TextSettings);
end;

{****************************************************************************}
procedure TALDynamicListBoxText.SetTextSettings(const Value: TALTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{*************************************************************************}
constructor TALDynamicListBoxBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create;
  //--
  FParent := AParent;
  if (AParent is TALDynamicListBoxShape) then begin
    var LShapeControl := TALDynamicListBoxShape(AParent);
    FStateStyleParent := nil;
    FControlParent := TALDynamicListBoxControl(AParent);
    FFill := CreateFill(LShapeControl.fill);
    FStateLayer := CreateStateLayer;
    FStroke := CreateStroke(LShapeControl.Stroke);
    FShadow := CreateShadow(LShapeControl.Shadow);
  end
  else if (AParent is TALDynamicListBoxBaseStateStyle) then begin
    FStateStyleParent := TALDynamicListBoxBaseStateStyle(AParent);
    FControlParent := nil;
    FFill := CreateFill(FStateStyleParent.fill);
    FStateLayer := CreateStateLayer;
    FStroke := CreateStroke(FStateStyleParent.Stroke);
    FShadow := CreateShadow(FStateStyleParent.Shadow);
  end
  else begin
    {$IF defined(debug)}
    if (AParent <> nil) then
      raise Exception.Create('Parent object type is invalid');
    {$ENDIF}
    FStateStyleParent := nil;
    FControlParent := nil;
    FFill := CreateFill(nil);
    FStateLayer := CreateStateLayer;
    FStroke := CreateStroke(nil);
    FShadow := CreateShadow(nil);
  end;
  FFill.OnChanged := FillChanged;
  FStateLayer.OnChanged := StateLayerChanged;
  FStroke.OnChanged := StrokeChanged;
  FShadow.OnChanged := ShadowChanged;
  //--
  FScale := DefaultScale;
  //--
  fSuperseded := False;
  //--
  FBufDrawable := ALNullDrawable;
  //BufDisabledDrawableRect
end;

{*************************************************}
destructor TALDynamicListBoxBaseStateStyle.Destroy;
begin
  ClearBufDrawable;
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStateLayer);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FShadow);
  inherited Destroy;
end;

{*******************************************************************************}
function TALDynamicListBoxBaseStateStyle.CreateSavedState: TALPersistentObserver;
type
  TALDynamicListBoxBaseStateStyleClass = class of TALDynamicListBoxBaseStateStyle;
begin
  result := TALDynamicListBoxBaseStateStyleClass(classtype).Create(nil{AParent});
end;

{********************************************************************************************}
function TALDynamicListBoxBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TALInheritBrush.Create(AParent)
end;

{***********************************************************************}
function TALDynamicListBoxBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TALStateLayer.Create;
end;

{**********************************************************************************************************}
function TALDynamicListBoxBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TALInheritStrokeBrush.Create(AParent);
end;

{************************************************************************************************}
function TALDynamicListBoxBaseStateStyle.CreateShadow(const AParent: TALShadow): TALInheritShadow;
begin
  Result := TALInheritShadow.Create(AParent);
end;

{********************************************************************}
procedure TALDynamicListBoxBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TALDynamicListBoxBaseStateStyle then begin
    BeginUpdate;
    Try
      Fill.Assign(TALDynamicListBoxBaseStateStyle(Source).Fill);
      StateLayer.Assign(TALDynamicListBoxBaseStateStyle(Source).StateLayer);
      Stroke.Assign(TALDynamicListBoxBaseStateStyle(Source).Stroke);
      Shadow.Assign(TALDynamicListBoxBaseStateStyle(Source).Shadow);
      Scale := TALDynamicListBoxBaseStateStyle(Source).Scale;
      fSuperseded := TALDynamicListBoxBaseStateStyle(Source).fSuperseded;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************************}
procedure TALDynamicListBoxBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Fill.Reset;
    StateLayer.Reset;
    Stroke.Reset;
    Shadow.Reset;
    Scale := DefaultScale;
    fSuperseded := False;
  finally
    EndUpdate;
  end;
end;

{*****************************************************}
procedure TALDynamicListBoxBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  try
    Fill.AlignToPixel;
    StateLayer.AlignToPixel;
    Stroke.AlignToPixel;
    Shadow.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*********************************************************}
procedure TALDynamicListBoxBaseStateStyle.ClearBufDrawable;
begin
  ALFreeAndNilDrawable(FBufDrawable);
end;

{*******************************************************************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.Interpolate(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    var LPrevStateLayerHasfill := StateLayer.HasFill;
    var LPrevStateLayerUseContentColor := StateLayer.UseContentColor;
    var LPrevStateLayerXRadius := StateLayer.XRadius;
    var LPrevStateLayerYRadius := StateLayer.YRadius;

    if ATo <> nil then begin
      Fill.Interpolate(ATo.Fill, ANormalizedTime);
      StateLayer.Interpolate(ATo.StateLayer, ANormalizedTime);
      Stroke.Interpolate(ATo.Stroke, ANormalizedTime);
      Shadow.Interpolate(ATo.Shadow, ANormalizedTime);
      Scale := InterpolateSingle(Scale{Start}, ATo.Scale{Stop}, ANormalizedTime);
      //Transition
    end
    else if FStateStyleParent <> nil then begin
      FStateStyleParent.SupersedeNoChanges(true{ASaveState});
      try
        Fill.Interpolate(FStateStyleParent.Fill, ANormalizedTime);
        StateLayer.Interpolate(FStateStyleParent.StateLayer, ANormalizedTime);
        Stroke.Interpolate(FStateStyleParent.Stroke, ANormalizedTime);
        Shadow.Interpolate(FStateStyleParent.Shadow, ANormalizedTime);
        Scale := InterpolateSingle(Scale{Start}, FStateStyleParent.Scale{Stop}, ANormalizedTime);
        //Transition
      finally
        FStateStyleParent.RestoreStateNoChanges;
      end;
    end
    else if (FControlParent is TALDynamicListBoxShape) then begin
      var LShapeControl := TALDynamicListBoxShape(FControlParent);
      Fill.Interpolate(LShapeControl.Fill, ANormalizedTime);
      StateLayer.Interpolate(nil, ANormalizedTime);
      Stroke.Interpolate(LShapeControl.Stroke, ANormalizedTime);
      Shadow.Interpolate(LShapeControl.Shadow, ANormalizedTime);
      Scale := InterpolateSingle(Scale{Start}, DefaultScale{Stop}, ANormalizedTime);
      //Transition
    end
    else begin
      Fill.Interpolate(nil, ANormalizedTime);
      StateLayer.Interpolate(nil, ANormalizedTime);
      Stroke.Interpolate(nil, ANormalizedTime);
      Shadow.Interpolate(nil, ANormalizedTime);
      Scale := InterpolateSingle(Scale{Start}, DefaultScale{Stop}, ANormalizedTime);
      //Transition
    end;

    // If StateLayer or ATo.StateLayer is empty, then the interpolation
    // should only be applied to its opacity. Do not modify UseContentColor,
    // XRadius, or YRadius.
    if (ATo = nil) or (not ATo.StateLayer.HasFill) then begin
      StateLayer.UseContentColor := LPrevStateLayerUseContentColor;
      StateLayer.XRadius := LPrevStateLayerXRadius;
      StateLayer.YRadius := LPrevStateLayerYRadius;
    end
    else if (not LPrevStateLayerHasfill) and (ATo <> nil) then begin
      StateLayer.UseContentColor := ATo.StateLayer.UseContentColor;
      StateLayer.XRadius := ATo.StateLayer.XRadius;
      StateLayer.YRadius := ATo.StateLayer.YRadius;
    end;
  Finally
    EndUpdate;
  End;
end;

{****************************************************************************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.InterpolateNoChanges(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{****************************************************}
procedure TALDynamicListBoxBaseStateStyle.DoSupersede;
begin
  Fill.Supersede;
  Stroke.Supersede;
  Shadow.Supersede;
  // Do not supersede the scale
end;

{*************************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) then exit;
  BeginUpdate;
  try
    DoSupersede;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{**********************************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{**************************************************************}
function TALDynamicListBoxBaseStateStyle.IsScaleStored: Boolean;
begin
  result := not SameValue(fScale, DefaultScale, Tepsilon.Scale);
end;

{***************************************************************}
function TALDynamicListBoxBaseStateStyle.GetDefaultScale: Single;
begin
  Result := 1;
end;

{*******************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.SetFill(const AValue: TALInheritBrush);
begin
  FFill.Assign(AValue);
end;

{***********************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.SetStateLayer(const AValue: TALStateLayer);
begin
  FStateLayer.Assign(AValue);
end;

{***************************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.SetStroke(const AValue: TALInheritStrokeBrush);
begin
  FStroke.Assign(AValue);
end;

{**********************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.SetShadow(const AValue: TALInheritShadow);
begin
  FShadow.Assign(AValue);
end;

{**********************************************************************}
procedure TALDynamicListBoxBaseStateStyle.SetScale(const Value: Single);
begin
  if not SameValue(FScale, Value, TEpsilon.Scale) then begin
    FScale := Value;
    Change;
  end;
end;

{***********************************************************}
function TALDynamicListBoxBaseStateStyle.GetInherit: Boolean;
begin
  Result := Fill.Inherit and
            (not StateLayer.HasFill) and
            Stroke.Inherit and
            Shadow.Inherit and
            Samevalue(Scale, DefaultScale, TEpsilon.Scale);
end;

{*****************************************************************}
function TALDynamicListBoxBaseStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{**********************************************************************}
procedure TALDynamicListBoxBaseStateStyle.FillChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.StateLayerChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.StrokeChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************************}
procedure TALDynamicListBoxBaseStateStyle.ShadowChanged(ASender: TObject);
begin
  Change;
end;

{*******************************************************************************************}
constructor TALDynamicListBoxBaseStateStyles.Create(const AParent: TALDynamicListBoxControl);
begin
  inherited Create;
  //--
  FParent := AParent;
  //--
  FTransition := CreateTransition;
  FTransition.OnChanged := TransitionChanged;
  //--
  FTransitionAnimation := TALFloatAnimation.Create;
  FTransitionAnimation.OnProcess := TransitionAnimationProcess;
  FTransitionAnimation.OnFinish := TransitionAnimationFinish;
  //--
  FTransitionFrom := nil;
  FTransitionTo := nil;
  FTransitionClickDelayed := False;
  //--
  FLastPaintedRawStyle := nil;
  FCurrentAdjustedStyle := nil;
end;

{**************************************************}
destructor TALDynamicListBoxBaseStateStyles.Destroy;
begin
  ALFreeAndNil(FTransitionAnimation);
  ALfreeandNil(FTransitionFrom);
  ALfreeandNil(FTransitionTo);
  //FLastPaintedRawStyle
  ALfreeandNil(FCurrentAdjustedStyle);
  ALFreeAndNil(FTransition);
  inherited Destroy;
end;

{********************************************************************************}
function TALDynamicListBoxBaseStateStyles.CreateSavedState: TALPersistentObserver;
type
  TALDynamicListBoxBaseStateStylesClass = class of TALDynamicListBoxBaseStateStyles;
begin
  result := TALDynamicListBoxBaseStateStylesClass(classtype).Create(nil{AParent});
end;

{*****************************************************************************}
function TALDynamicListBoxBaseStateStyles.CreateTransition: TALStateTransition;
begin
  result := TALStateTransition.Create;
end;

{*********************************************************}
procedure TALDynamicListBoxBaseStateStyles.StartTransition;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _IsSameStateStyleClass(const AStateStyleA, AStateStyleB: TALDynamicListBoxBaseStateStyle): boolean;
  begin
    result := AStateStyleA = AStateStyleB;
    if (not result) and
       (AStateStyleA <> nil) and
       (AStateStyleB <> nil) then begin
      result := AStateStyleA.ClassType = AStateStyleB.ClassType;
    end;
  end;

type
  TALDynamicListBoxBaseStateStyleClass = class of TALDynamicListBoxBaseStateStyle;

begin

  If parent.IsDestroying then Exit;
  var LPrevTransitionClickDelayed := FTransitionClickDelayed;
  FTransitionClickDelayed := False;
  try

    if SameValue(FTransition.Duration,0.0,TEpsilon.Scale) then Exit;
    //--
    var LCurrentRawStyle := GetCurrentRawStyle;
    //--
    var LIsInReverseAnimation := False;
    if (FTransitionAnimation.Enabled) then begin
      if _IsSameStateStyleClass(FTransitionFrom, LCurrentRawStyle) then
        LIsInReverseAnimation := True;
      ALFreeAndNil(FTransitionFrom);
      {$IF defined(debug)}
      if FCurrentAdjustedStyle = nil then
        Raise Exception.Create('Error D92ACB4F-F9FA-4245-B347-225978347708');
      {$ENDIF}
      FTransitionFrom := TALDynamicListBoxBaseStateStyleClass(FCurrentAdjustedStyle.classtype).Create(FCurrentAdjustedStyle.Parent{AParent});
      FTransitionFrom.Assign(FCurrentAdjustedStyle);
    end
    else begin
      ALFreeAndNil(FTransitionFrom);
      if FLastPaintedRawStyle = nil then FTransitionFrom := nil
      else begin
        FTransitionFrom := TALDynamicListBoxBaseStateStyleClass(FLastPaintedRawStyle.classtype).Create(FLastPaintedRawStyle.Parent{AParent});
        FTransitionFrom.Assign(FLastPaintedRawStyle);
      end;
    end;
    //--
    ALFreeAndNil(FTransitionTo);
    if LCurrentRawStyle = nil then FTransitionTo := nil
    else begin
      FTransitionTo := TALDynamicListBoxBaseStateStyleClass(LCurrentRawStyle.classtype).Create(LCurrentRawStyle.parent{AParent});
      FTransitionTo.Assign(LCurrentRawStyle);
    end;
    //--
    if (FTransitionFrom = nil) and (FTransitionto = nil) then begin
      FTransitionAnimation.Enabled := False;
      FParent.Repaint;
      exit;
    end;
    //--
    if FTransitionFrom <> nil then FTransitionFrom.SupersedeNoChanges(false{ASaveState});
    if FTransitionTo <> nil then FTransitionTo.SupersedeNoChanges(false{ASaveState});
    //--
    FTransitionAnimation.Enabled := False;
    if LIsInReverseAnimation then FTransitionAnimation.Duration := FTransition.Duration * FTransitionAnimation.CurrentValue
    else FTransitionAnimation.Duration := FTransition.Duration;
    FTransitionAnimation.StartValue := 0;
    FTransitionAnimation.StopValue := 1;
    FTransitionAnimation.AnimationType := FTransition.animationType;
    FTransitionAnimation.Interpolation := FTransition.Interpolation;
    FTransitionAnimation.Start;
    //--
    // This is necessary in case StartTransition is called again immediately after
    // (multiple simultaneous events).
    GetCurrentAdjustedStyle;
    //--
    {$IF defined(debug)}
    //var LTransitionFromClassName: String;
    //if FTransitionFrom <> nil then LTransitionFromClassName := FTransitionFrom.ClassName
    //else LTransitionFromClassName := 'nil';
    //var LTransitionToClassName: String;
    //if FTransitionTo <> nil then LTransitionToClassName := FTransitionTo.ClassName
    //else LTransitionToClassName := 'nil';
    //ALLog(
    //  'TALDynamicListBoxBaseStateStyles.StartTransition',
    //  'From: '+LTransitionFromClassName + ' | ' +
    //  'To: '+LTransitionToClassName);
    {$ENDIF}

  finally
    if FTransitionAnimation.Running then
      FTransitionClickDelayed := LPrevTransitionClickDelayed
    else if LPrevTransitionClickDelayed then
      FParent.click;
  end;
end;

{*************************************************************************************}
procedure TALDynamicListBoxBaseStateStyles.TransitionAnimationProcess(Sender: TObject);
begin
  {$IF defined(debug)}
  //ALLog('TALDynamicListBoxBaseStateStyles.TransitionAnimationProcess');
  {$ENDIF}
  FParent.Repaint;
end;

{************************************************************************************}
procedure TALDynamicListBoxBaseStateStyles.TransitionAnimationFinish(Sender: TObject);
begin
  {$IF defined(debug)}
  //ALLog('TALDynamicListBoxBaseStateStyles.TransitionAnimationFinish');
  {$ENDIF}
  FTransitionAnimation.Enabled := False;
  if FTransitionClickDelayed then begin
    FTransitionClickDelayed := False;
    FParent.click;
  end;
  FParent.Repaint;
end;

{*********************************************************************}
procedure TALDynamicListBoxBaseStateStyles.Assign(Source: TPersistent);
begin
  if Source is TALDynamicListBoxBaseStateStyles then begin
    BeginUpdate;
    Try
      Transition.Assign(TALDynamicListBoxBaseStateStyles(Source).Transition);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{***********************************************}
procedure TALDynamicListBoxBaseStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Transition.Reset;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALDynamicListBoxBaseStateStyles.AlignToPixel;
begin
  // Virtual
end;

{**********************************************************}
procedure TALDynamicListBoxBaseStateStyles.ClearBufDrawable;
begin
  // Virtual
end;

{********************************************************************************************}
function TALDynamicListBoxBaseStateStyles.GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle;
begin
  Raise Exception.Create('Not implemented')
end;

{*************************************************************************************************}
function TALDynamicListBoxBaseStateStyles.GetCurrentAdjustedStyle: TALDynamicListBoxBaseStateStyle;
type
  TALDynamicListBoxBaseStateStyleClass = class of TALDynamicListBoxBaseStateStyle;
begin
  if FTransitionAnimation.Enabled then begin
    var LStateStyle := FTransitionTo;
    if LStateStyle = nil then LStateStyle := FTransitionFrom;
    {$IF defined(debug)}
    if LStateStyle = nil then
      raise Exception.Create('Error 45CB6D22-AB78-4857-B03F-1636E5184C12');
    {$ENDIF}
    if (FCurrentAdjustedStyle = nil) or
       (FCurrentAdjustedStyle.ClassType <> LStateStyle.ClassType) then begin
      ALFreeAndNil(FCurrentAdjustedStyle);
      FCurrentAdjustedStyle := TALDynamicListBoxBaseStateStyleClass(LStateStyle.classtype).Create(LStateStyle.parent{AParent});
    end;
    FCurrentAdjustedStyle.Assign(LStateStyle);
    FCurrentAdjustedStyle.SupersedeNoChanges(false{ASaveState});
    //--
    if FTransitionTo = nil then FCurrentAdjustedStyle{AFromStateStyle}.InterpolateNoChanges(nil{AToStateStyle}, FTransitionAnimation.CurrentValue)
    else if FTransitionFrom = nil then FCurrentAdjustedStyle{AToStateStyle}.InterpolateNoChanges(nil{AFromStateStyle}, 1-FTransitionAnimation.CurrentValue)
    else begin
      {$IF defined(debug)}
      if not FTransitionFrom.Superseded then
        raise Exception.Create('Error 3A71A6B7-40C3-40A6-B678-D1FC6A0DD152');
      {$ENDIF}
      FCurrentAdjustedStyle{AToStateStyle}.InterpolateNoChanges(FTransitionFrom{AFromStateStyle}, 1-FTransitionAnimation.CurrentValue);
    end;
  end
  else begin
    var LStateStyle := GetCurrentRawStyle;
    if LStateStyle = nil then ALFreeAndNil(FCurrentAdjustedStyle)
    else begin
      if (FCurrentAdjustedStyle = nil) or
         (FCurrentAdjustedStyle.ClassType <> LStateStyle.ClassType) then begin
        ALFreeAndNil(FCurrentAdjustedStyle);
        FCurrentAdjustedStyle := TALDynamicListBoxBaseStateStyleClass(LStateStyle.classtype).Create(LStateStyle.parent{AParent});
      end;
      FCurrentAdjustedStyle.Assign(LStateStyle);
      FCurrentAdjustedStyle.SupersedeNoChanges(false{ASaveState});
    end;
  end;
  Result := FCurrentAdjustedStyle;
end;

{******************************************************************************}
function TALDynamicListBoxBaseStateStyles.IsTransitionAnimationRunning: Boolean;
begin
  Result := FTransitionAnimation.Enabled and
            FTransitionAnimation.Running;
end;

{*******************************************************************}
procedure TALDynamicListBoxBaseStateStyles.UpdateLastPaintedRawStyle;
begin
  FLastPaintedRawStyle := GetCurrentRawStyle;
end;

{****************************************************************************************}
procedure TALDynamicListBoxBaseStateStyles.SetTransition(const Value: TALStateTransition);
begin
  FTransition.Assign(Value);
end;

{*****************************************************************************}
procedure TALDynamicListBoxBaseStateStyles.TransitionChanged(ASender: TObject);
begin
  Change;
end;

{**************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(3,3,3,3);
end;

{***************************************************************}
constructor TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.Create;
begin
  inherited Create;
  //--
  FColor := DefaultColor;
  FResourceName := DefaultResourceName;
  FWrapMode := DefaultWrapMode;
  FThickness := DefaultThickness;
  //--
  FMargins := CreateMargins;
  FMargins.OnChange := MarginsChanged;
end;

{***************************************************************}
destructor TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.Destroy;
begin
  ALFreeAndNil(FMargins);
  inherited;
end;

{******************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{**********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Black;
end;

{************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.GetDefaultResourceName: String;
begin
  Result := '';
end;

{******************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.GetDefaultWrapMode: TALImageWrapMode;
begin
  Result := TALImageWrapMode.Fit;
end;

{*********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.GetDefaultThickness: Single;
begin
  Result := 2;
end;

{**********************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.Assign(Source: TPersistent);
begin
  if Source is TCheckMarkBrush then begin
    BeginUpdate;
    Try
      Color := TCheckMarkBrush(Source).Color;
      ResourceName := TCheckMarkBrush(Source).ResourceName;
      WrapMode := TCheckMarkBrush(Source).WrapMode;
      Thickness := TCheckMarkBrush(Source).Thickness;
      Margins.Assign(TCheckMarkBrush(Source).Margins);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Color := DefaultColor;
    ResourceName := DefaultResourceName;
    WrapMode := DefaultWrapMode;
    Thickness := DefaultThickness;
    Margins.Rect := Margins.DefaultValue;
  finally
    EndUpdate;
  end;
end;

{*******************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.AlignToPixel;
begin
  BeginUpdate;
  Try
    Thickness := ALAlignDimensionToPixelRound(Thickness, ALGetScreenScale, Tepsilon.vector);
    Margins.Rect := ALAlignEdgesToPixelRound(Margins.Rect, ALGetScreenScale, TEpsilon.Position);
  finally
    EndUpdate;
  end;
end;

{*****************************************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.Interpolate(const ATo: TCheckMarkBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      ResourceName := ATo.ResourceName;
      WrapMode := ATo.WrapMode;
      Thickness := InterpolateSingle(Thickness{Start}, ATo.Thickness{Stop}, ANormalizedTime);
      Margins.Left := InterpolateSingle(Margins.Left{Start}, ATo.Margins.Left{Stop}, ANormalizedTime);
      Margins.Right := InterpolateSingle(Margins.Right{Start}, ATo.Margins.Right{Stop}, ANormalizedTime);
      Margins.Top := InterpolateSingle(Margins.Top{Start}, ATo.Margins.Top{Stop}, ANormalizedTime);
      Margins.Bottom := InterpolateSingle(Margins.Bottom{Start}, ATo.Margins.Bottom{Stop}, ANormalizedTime);
    end
    else begin
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      ResourceName := DefaultResourceName;
      WrapMode := DefaultWrapMode;
      Thickness := InterpolateSingle(Thickness{Start}, DefaultThickness{Stop}, ANormalizedTime);
      Margins.Left := InterpolateSingle(Margins.Left{Start}, Margins.DefaultValue.Left{Stop}, ANormalizedTime);
      Margins.Right := InterpolateSingle(Margins.Right{Start}, Margins.DefaultValue.Right{Stop}, ANormalizedTime);
      Margins.Top := InterpolateSingle(Margins.Top{Start}, Margins.DefaultValue.Top{Stop}, ANormalizedTime);
      Margins.Bottom := InterpolateSingle(Margins.Bottom{Start}, Margins.DefaultValue.Bottom{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{**************************************************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.InterpolateNoChanges(const ATo: TCheckMarkBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{***************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.HasCheckMark: boolean;
begin
  result := ((Color <> TalphaColors.Null) and
             (CompareValue(FThickness, 0, TEpsilon.Vector) > 0)) or
            (ResourceName <> '');
end;

{****************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{***********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> DefaultResourceName;
end;

{*******************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> DefaultWrapMode;
end;

{********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.IsThicknessStored: Boolean;
begin
  result := not SameValue(FThickness, DefaultThickness, TEpsilon.Vector);
end;

{*****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
  end;
end;

{*******************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.SetResourceName(const Value: String);
begin
  if fResourceName <> Value then begin
    fResourceName := Value;
    Change;
  end;
end;

{*************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.SetWrapMode(const Value: TALImageWrapMode);
begin
  if fWrapMode <> Value then begin
    fWrapMode := Value;
    Change;
  end;
end;

{****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.SetThickness(const Value: Single);
begin
  if not SameValue(Value, FThickness, TEpsilon.Vector) then begin
    fThickness := Value;
    Change;
  end;
end;

{*****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.SetMargins(const Value: TALBounds);
begin
  FMargins.Assign(Value);
end;

{**************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckMarkBrush.MarginsChanged(Sender: TObject);
begin
  change;
end;

{******************************************************************************************************}
constructor TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.Create(const AParent: TCheckMarkBrush);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{****************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.CreateSavedState: TALPersistentObserver;
type
  TInheritCheckMarkBrushClass = class of TInheritCheckMarkBrush;
begin
  result := TInheritCheckMarkBrushClass(classtype).Create(nil{AParent});
end;

{***********************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{*****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TInheritCheckMarkBrush then begin
      Inherit := TInheritCheckMarkBrush(Source).Inherit;
      fSuperseded := TInheritCheckMarkBrush(Source).fSuperseded;
    end
    else begin
      Inherit := False;
      fSuperseded := False;
    end;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*******************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Inherit := True;
    fSuperseded := False;
  finally
    EndUpdate;
  end;
end;

{*************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.DoSupersede;
begin
  Assign(FParent);
end;

{**********************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  BeginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TInheritCheckMarkBrush then begin
      TInheritCheckMarkBrush(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TInheritCheckMarkBrush(FParent).restoreStateNoChanges;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{*******************************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{**************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.TStateLayer.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(-12,-12,-12,-12);
end;

{******************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.TStateLayer.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{*******************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{*******************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{***************************************************************************************}
constructor TALDynamicListBoxBaseCheckBox.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  if StateStyleParent <> nil then FCheckMark := CreateCheckMark(StateStyleParent.CheckMark)
  else if ControlParent <> nil then FCheckMark := CreateCheckMark(ControlParent.CheckMark)
  else FCheckMark := CreateCheckMark(nil);
  FCheckMark.OnChanged := CheckMarkChanged;
end;

{***************************************************************}
destructor TALDynamicListBoxBaseCheckBox.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FCheckMark);
  inherited Destroy;
end;

{*************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{*****************************************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.CreateCheckMark(const AParent: TCheckMarkBrush): TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{**********************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      CheckMark.Assign(TBaseStateStyle(Source).CheckMark);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    CheckMark.Reset;
  finally
    EndUpdate;
  end;
end;

{*******************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    CheckMark.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TBaseStateStyle.Interpolate(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error F3C72244-894F-4B67-AD86-F24DF5039927');
  {$ENDIF}
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then CheckMark.Interpolate(TBaseStateStyle(ATo).CheckMark, ANormalizedTime)
    else if StateStyleParent <> nil then begin
      StateStyleParent.SupersedeNoChanges(true{ASaveState});
      try
        CheckMark.Interpolate(StateStyleParent.CheckMark, ANormalizedTime)
      finally
        StateStyleParent.RestoreStateNoChanges;
      end;
    end
    else if ControlParent <> nil then CheckMark.Interpolate(ControlParent.CheckMark, ANormalizedTime)
    else CheckMark.Interpolate(nil, ANormalizedTime);
  Finally
    EndUpdate;
  End;
end;

{******************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TBaseStateStyle.DoSupersede;
begin
  inherited;
  CheckMark.Supersede;
end;

{******************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.GetStateStyleParent: TBaseStateStyle;
begin
  {$IF defined(debug)}
  if (inherited StateStyleParent <> nil) and
     (not (inherited StateStyleParent is TBaseStateStyle)) then
    raise Exception.Create('StateStyleParent must be of type TBaseStateStyle');
  {$ENDIF}
  result := TBaseStateStyle(inherited StateStyleParent);
end;

{*****************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.GetControlParent: TALDynamicListBoxBaseCheckBox;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TALDynamicListBoxBaseCheckBox)) then
    raise Exception.Create('ControlParent must be of type TALDynamicListBoxBaseCheckBox');
  {$ENDIF}
  result := TALDynamicListBoxBaseCheckBox(inherited ControlParent);
end;

{*********************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TBaseStateStyle.SetCheckMark(const AValue: TInheritCheckMarkBrush);
begin
  FCheckMark.Assign(AValue);
end;

{*************************************************************************}
function TALDynamicListBoxBaseCheckBox.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            CheckMark.Inherit;
end;

{*****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TBaseStateStyle.CheckMarkChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TDefaultStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 1;
end;

{**********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{******************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*******************************************************************************************}
constructor TALDynamicListBoxBaseCheckBox.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{**************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{****************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{*****************************************************************************}
function TALDynamicListBoxBaseCheckBox.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{***********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TDisabledStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 2;
end;

{**********************************************************************************}
function TALDynamicListBoxBaseCheckBox.THoveredStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 3;
end;

{**********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TPressedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 4;
end;

{**********************************************************************************}
function TALDynamicListBoxBaseCheckBox.TFocusedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 5;
end;

{**********************************************************************************************************}
constructor TALDynamicListBoxBaseCheckBox.TCheckStateStyles.Create(const AParent: TALDynamicListBoxControl);
begin
  inherited Create;
  //--
  FDefault := CreateDefaultStateStyle(AParent);
  FDefault.OnChanged := DefaultChanged;
  //--
  FDisabled := CreateDisabledStateStyle(FDefault);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(FDefault);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(FDefault);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(FDefault);
  FFocused.OnChanged := FocusedChanged;
end;

{*****************************************************************}
destructor TALDynamicListBoxBaseCheckBox.TCheckStateStyles.Destroy;
begin
  ALFreeAndNil(FDefault);
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{***********************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckStateStyles.CreateSavedState: TALPersistentObserver;
type
  TCheckStateStylesClass = class of TCheckStateStyles;
begin
  result := TCheckStateStylesClass(classtype).Create(nil{AParent});
end;

{***************************************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{*****************************************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{***************************************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{***************************************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{***************************************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.Assign(Source: TPersistent);
begin
  if Source is TCheckStateStyles then begin
    BeginUpdate;
    Try
      Default.Assign(TCheckStateStyles(Source).Default);
      Disabled.Assign(TCheckStateStyles(Source).Disabled);
      Hovered.Assign(TCheckStateStyles(Source).Hovered);
      Pressed.Assign(TCheckStateStyles(Source).Pressed);
      Focused.Assign(TCheckStateStyles(Source).Focused);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Default.Reset;
    Disabled.Reset;
    Hovered.Reset;
    Pressed.Reset;
    Focused.Reset;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    Default.AlignToPixel;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.ClearBufDrawable;
begin
  Default.ClearBufDrawable;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{*****************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.SetDefault(const AValue: TDefaultStateStyle);
begin
  FDefault.Assign(AValue);
end;

{*******************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{*****************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{*****************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{*****************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{*****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.DefaultChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{*****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{*****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{*****************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TCheckStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{*****************************************************************************************************}
constructor TALDynamicListBoxBaseCheckBox.TStateStyles.Create(const AParent: TALDynamicListBoxControl);
begin
  inherited Create(AParent);
  //--
  FChecked := CreateCheckedStateStyles(AParent);
  FChecked.OnChanged := CheckedChanged;
  //--
  FUnchecked := CreateUnCheckedStateStyles(AParent);
  FUnchecked.OnChanged := UncheckedChanged;
end;

{************************************************************}
destructor TALDynamicListBoxBaseCheckBox.TStateStyles.Destroy;
begin
  ALFreeAndNil(FChecked);
  ALFreeAndNil(FUnchecked);
  inherited Destroy;
end;

{***************************************************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TStateStyles.CreateCheckedStateStyles(const AParent: TALDynamicListBoxControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{*****************************************************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TStateStyles.CreateUncheckedStateStyles(const AParent: TALDynamicListBoxControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{*******************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Checked.Assign(TStateStyles(Source).Checked);
      Unchecked.Assign(TStateStyles(Source).Unchecked);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*********************************************************}
procedure TALDynamicListBoxBaseCheckBox.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.reset;
    Unchecked.reset;
  finally
    EndUpdate;
  end;
end;

{****************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.AlignToPixel;
    Unchecked.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{********************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Checked.ClearBufDrawable;
  Unchecked.ClearBufDrawable;
end;

{******************************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TStateStyles.GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle;
begin
  if Parent.Checked then begin
    if Not Parent.Enabled then Result := Checked.Disabled
    else if Parent.Pressed then Result := Checked.Pressed
    //else if Parent.IsFocused then Result := Checked.Focused
    else if Parent.IsMouseOver then Result := Checked.Hovered
    else result := Checked.Default;
  end
  else begin
    if Not Parent.Enabled then Result := UnChecked.Disabled
    else if Parent.Pressed then Result := UnChecked.Pressed
    //else if Parent.IsFocused then Result := UnChecked.Focused
    else if Parent.IsMouseOver then Result := UnChecked.Hovered
    else result := UnChecked.Default;
  end;
end;

{*******************************************************************************************}
function TALDynamicListBoxBaseCheckBox.TStateStyles.GetParent: TALDynamicListBoxBaseCheckBox;
begin
  Result := TALDynamicListBoxBaseCheckBox(inherited Parent);
end;

{***********************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TStateStyles.SetChecked(const AValue: TCheckStateStyles);
begin
  FChecked.Assign(AValue);
end;

{*************************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TStateStyles.SetUnchecked(const AValue: TCheckStateStyles);
begin
  FUnchecked.Assign(AValue);
end;

{************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TStateStyles.CheckedChanged(ASender: TObject);
begin
  Change;
end;

{**************************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.TStateStyles.UncheckedChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************************}
constructor TALDynamicListBoxBaseCheckBox.Create(const AOwner: TObject);
begin
  inherited;
  //--
  //SetAcceptsControls(False);
  //CanFocus := True;
  Cursor := crHandPoint;
  //--
  FCheckMark := CreateCheckMark;
  FCheckMark.OnChanged := CheckMarkChanged;
  //--
  FChecked := False;
  FDoubleBuffered := true;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  FOnChange := nil;
  //--
  // Must be created at the end because it requires FCheckMark to
  // be already created.
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{***********************************************}
destructor TALDynamicListBoxBaseCheckBox.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  ALFreeAndNil(FCheckMark);
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{**********************************************************************}
function TALDynamicListBoxBaseCheckBox.CreateCheckMark: TCheckMarkBrush;
begin
  Result := TCheckMarkBrush.Create;
end;

{*********************************************************************}
function TALDynamicListBoxBaseCheckBox.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{***************************************************}
procedure TALDynamicListBoxBaseCheckBox.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
    CheckMark.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***************************************************************}
function TALDynamicListBoxBaseCheckBox.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{****************************************************************}
function TALDynamicListBoxBaseCheckBox.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{*******************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{*********************************************************}
function TALDynamicListBoxBaseCheckBox.GetChecked: Boolean;
begin
  Result := FChecked;
end;

{***********************************************************************}
procedure TALDynamicListBoxBaseCheckBox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then begin
    FChecked := Value;
    if FChecked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
    else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
    DoChanged;
  end;
end;

{*********************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.SetCheckMark(const Value: TCheckMarkBrush);
begin
  FCheckMark.Assign(Value);
end;

{*********************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{**************************************************************}
function TALDynamicListBoxBaseCheckBox.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{**************************************************************}
function TALDynamicListBoxBaseCheckBox.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{***************************************************************}
function TALDynamicListBoxBaseCheckBox.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{***************************************************************}
function TALDynamicListBoxBaseCheckBox.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{**********************************************************************}
procedure TALDynamicListBoxBaseCheckBox.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{**********************************************************************}
procedure TALDynamicListBoxBaseCheckBox.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.CheckMarkChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{*******************************************************************}
procedure TALDynamicListBoxBaseCheckBox.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*********************************************************************}
procedure TALDynamicListBoxBaseCheckBox.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{*********************************************************************}
procedure TALDynamicListBoxBaseCheckBox.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**************************************************************************}
procedure TALDynamicListBoxBaseCheckBox.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  if Checked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
  else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
  Repaint;
end;

{*********************************************************}
procedure TALDynamicListBoxBaseCheckBox.IsMouseOverChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{*********************************************************}
//procedure TALDynamicListBoxBaseCheckBox.IsFocusedChanged;
//begin
//  inherited;
//  StateStyles.startTransition;
//  repaint;
//end;

{*****************************************************}
procedure TALDynamicListBoxBaseCheckBox.PressedChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{*****************************************************************************************************************}
//procedure TALDynamicListBoxBaseCheckBox.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
//begin
//  inherited;
//  if (KeyChar = ' ') then begin
//    Click; // Emulate mouse click to perform Action.OnExecute
//    KeyChar := #0;
//  end;
//end;

{********************************************}
procedure TALDynamicListBoxBaseCheckBox.Click;
begin
  Checked := not Checked;
  inherited;
end;

{************************************************************}
function TALDynamicListBoxBaseCheckBox.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(18, 18);
end;

{************************************************}
procedure TALDynamicListBoxBaseCheckBox.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;
end;

{************************************************}
procedure TALDynamicListBoxBaseCheckBox.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{*******************************************************}
procedure TALDynamicListBoxBaseCheckBox.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not IsDestroying) and
     ((not ALIsDrawableNull(FStateStyles.Checked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Focused.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
end;

{******************************************************}
procedure TALDynamicListBoxBaseCheckBox.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _DoMakeBufDrawable(const AStateStyle: TBaseStateStyle): boolean;
  begin
    if (not ALIsDrawableNull(AStateStyle.FBufDrawable)) then exit(False);
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try

      {$IFDEF debug}
      ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + AStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
      {$endif}

      CreateBufDrawable(
        AStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
        AStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
        ALGetScreenScale * AStateStyle.Scale, // const AScale: Single;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.CheckMark, // const ACheckMark: TCheckMarkBrush;
        AStateStyle.Shadow); // const AShadow: TALShadow);

      // LStateStyle.FBufDrawableRect must include the LScale
      AStateStyle.FBufDrawableRect.Top := AStateStyle.FBufDrawableRect.Top * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.right := AStateStyle.FBufDrawableRect.right * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.left := AStateStyle.FBufDrawableRect.left * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.bottom := AStateStyle.FBufDrawableRect.bottom * AStateStyle.Scale;

      // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect.ReducePrecision)
      // (due to scale), we must center LStateStyle.FBufDrawableRect within the main BufDrawableRect (LocalRect.ReducePrecision)
      // to ensure that all changes are visually centered.
      var LCenteredRect := AStateStyle.FBufDrawableRect.CenterAt(LocalRect.ReducePrecision);
      AStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    finally
      AStateStyle.RestorestateNoChanges;
    end;
    Result := True;
  end;

begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  var LSubIndexOffset: Integer;
  var LDefaultStateStyle: TBaseStateStyle;
  if Checked then begin
    LSubIndexOffset := GetCacheSubIndex{+0};
    LDefaultStateStyle := StateStyles.Checked.Default;
  end
  else begin
    LSubIndexOffset := GetCacheSubIndex+5;
    LDefaultStateStyle := StateStyles.UnChecked.Default;
  end;
  //--
  if (CacheIndex = 0) or
     (CacheEngine = nil) or
     (not CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex})) then
    _DoMakeBufDrawable(LDefaultStateStyle);
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex})) then Exit;
  _DoMakeBufDrawable(LStateStyle);
  // No need to center LStateStyle.FBufDrawableRect on the main BufDrawableRect
  // because BufDrawableRect always has the width and height of the localRect.
end;

{****************************************************}
procedure TALDynamicListBoxBaseCheckBox.DrawCheckMark(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const ADstRect: TrectF;
            const AChecked: Boolean;
            const ACheckMark: TCheckMarkBrush);
begin

  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if IsPixelAlignmentEnabled then ALExtractMatrixFromCanvas(Acanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;
  var LRect := ADstRect;
  LRect.Top := LRect.Top * AScale;
  LRect.right := LRect.right * AScale;
  LRect.left := LRect.left * AScale;
  LRect.bottom := LRect.bottom * AScale;
  if IsPixelAlignmentEnabled then
    LRect := ALAlignToPixelRound(LRect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);
  var LScaledMarginsRect := ACheckMark.Margins.Rect;
  LScaledMarginsRect.Left := LScaledMarginsRect.Left * AScale;
  LScaledMarginsRect.right := LScaledMarginsRect.right * AScale;
  LScaledMarginsRect.top := LScaledMarginsRect.top * AScale;
  LScaledMarginsRect.bottom := LScaledMarginsRect.bottom * AScale;
  if IsPixelAlignmentEnabled then
    LScaledMarginsRect := ALAlignEdgesToPixelRound(LScaledMarginsRect, LCanvasScale, TEpsilon.Position);
  LRect.Top := LRect.Top + LScaledMarginsRect.top;
  LRect.right := LRect.right - LScaledMarginsRect.right;
  LRect.left := LRect.left + LScaledMarginsRect.left;
  LRect.bottom := LRect.bottom - LScaledMarginsRect.bottom;
  if LRect.IsEmpty then exit;

  // Without ResourceName
  if ACheckMark.ResourceName = '' then begin

    // Exit if no color or no stroke
    var LScaledCheckMarkThickness := ACheckMark.Thickness * AScale;
    if (ACheckMark.Color = TalphaColors.Null) or (CompareValue(LScaledCheckMarkThickness, 0, TEpsilon.position) <= 0) then
      exit;
    if IsPixelAlignmentEnabled then
      LScaledCheckMarkThickness := ALAlignDimensionToPixelRound(LScaledCheckMarkThickness, LCanvasScale, TEpsilon.Position);

    // exit if not checked
    if not Achecked then
      exit;

    // Try to find LPoint2.x so that LPoint1, LPoint2 and LPoint3 form
    // a right triangle
    Var LHalfScaledCheckMarkThickness := ((LScaledCheckMarkThickness / Sqrt(2)) / 2);
    Var LCheckMarkRect := TRectF.Create(0,0,342,270).FitInto(Lrect);
    var LPoint1 := TPointF.Create(LCheckMarkRect.left + LHalfScaledCheckMarkThickness, LCheckMarkRect.top+(LCheckMarkRect.height * 0.5));
    var LPoint2 := TPointF.Create(0, LCheckMarkRect.Bottom - (2*LHalfScaledCheckMarkThickness));
    var LPoint3 := TPointF.Create(LCheckMarkRect.right-LHalfScaledCheckMarkThickness, LCheckMarkRect.top+LHalfScaledCheckMarkThickness);
    // Coefficients for the quadratic equation ax^2 + bx + c = 0
    var a: Single := 1;
    var b: Single := -(LPoint1.X + LPoint3.X);
    var c: Single := LPoint1.X * LPoint3.X + LPoint1.Y * LPoint3.Y - LPoint1.Y * LPoint2.Y - LPoint2.Y * LPoint3.Y + Sqr(LPoint2.Y);
    // Calculate the discriminant
    var LDiscriminant: Single := b * b - 4 * a * c;
    // Check if there are real solutions
    if LDiscriminant < 0 then begin
      // No real solution so use place LPoint2.x
      // at 33% from the left border
      LPoint2.x := LCheckMarkRect.Left + (LCheckMarkRect.Width * 0.33);
    end
    else begin
      // 2 solutions:
      //     (-b - Sqrt(LDiscriminant)) / (2 * a);
      //     (-b + Sqrt(LDiscriminant)) / (2 * a);
      // Use only the first one
      LPoint2.x := (-b - Sqrt(LDiscriminant)) / (2 * a);
    end;

    {$REGION 'SKIA'}
    {$IF defined(ALSkiaEngine)}

    // Create LPaint
    var LPaint := ALSkCheckHandle(sk4d_paint_create);
    try

      // Requests, but does not require, that edge pixels draw opaque or with partial transparency.
      sk4d_paint_set_antialias(LPaint, true);
      // Sets whether the geometry is filled, stroked, or filled and stroked.
      sk4d_paint_set_dither(LPaint, true);

      // Stroke with solid color
      sk4d_paint_set_style(LPaint, sk_paintstyle_t.STROKE_SK_PAINTSTYLE);
      sk4d_paint_set_stroke_width(LPaint, LScaledCheckMarkThickness);
      sk4d_paint_set_color(LPaint, ACheckMark.Color);
      var LPathBuilder := ALSkCheckHandle(sk4d_pathbuilder_create);
      try
        sk4d_pathbuilder_move_to(LPathBuilder, @LPoint1);
        sk4d_pathbuilder_line_to(LPathBuilder, @LPoint2);
        sk4d_pathbuilder_line_to(LPathBuilder, @LPoint3);
        var LPath := sk4d_pathbuilder_detach(LPathBuilder);
        try
          sk4d_canvas_draw_Path(ACanvas, LPath, LPaint);
        finally
          sk4d_path_destroy(LPath);
        end;
      finally
        sk4d_pathbuilder_destroy(LPathBuilder);
      end;

    finally
      sk4d_paint_destroy(LPaint);
    end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'ANDROID'}
    {$IF (defined(ANDROID)) and (not defined(ALSkiaEngine))}

    // Create LPaint
    var LPaint := TJPaint.JavaClass.init;
    LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
    LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
    LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

    // Stroke with solid color
    LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    LPaint.setStrokeWidth(LScaledCheckMarkThickness);
    LPaint.setColor(integer(ACheckMark.Color));
    var LPath := TJPath.Create;
    LPath.moveTo(LPoint1.x, LPoint1.y);
    LPath.LineTo(LPoint2.x, LPoint2.y);
    LPath.LineTo(LPoint3.x, LPoint3.y);
    aCanvas.drawPath(LPath,LPaint);
    LPath := nil;

    //free the paint
    LPaint := nil;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'APPLEOS'}
    {$IF (defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

    var LAlphaColor := TAlphaColorCGFloat.Create(ACheckMark.Color);
    CGContextSetRGBStrokeColor(ACanvas, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
    CGContextSetLineWidth(ACanvas, LScaledCheckMarkThickness);

    var LGridHeight := CGBitmapContextGetHeight(ACanvas);

    CGContextBeginPath(ACanvas);
    CGContextMoveToPoint(ACanvas, LPoint1.x, LGridHeight - LPoint1.y);
    CGContextAddLineToPoint(ACanvas, LPoint2.x, LGridHeight - LPoint2.y);
    CGContextAddLineToPoint(ACanvas, LPoint3.x, LGridHeight - LPoint3.y);
    CGContextStrokePath(ACanvas);


    {$ENDIF}
    {$ENDREGION}

    {$REGION 'MSWINDOWS'}
    {$IF (not defined(ANDROID)) and (not defined(ALAppleOS)) and (not defined(ALSkiaEngine))}

    var LSaveState := ACanvas.SaveState;
    try
      ACanvas.Stroke.Color := ACheckMark.Color;
      ACanvas.Stroke.Thickness := LScaledCheckMarkThickness;
      ACanvas.DrawLine(LPoint1, LPoint2, 1{AOpacity});
      ACanvas.DrawLine(LPoint2, LPoint3, 1{AOpacity});
    finally
      ACanvas.RestoreState(LSaveState)
    end;

    {$ENDIF}
    {$ENDREGION}

  end

  // With ResourceName
  else begin

    TALDrawRectangleHelper.Create(ACanvas)
      .SetAlignToPixel(IsPixelAlignmentEnabled)
      .SetDstRect(LRect)
      .SetFillColor(ACheckMark.Color)
      .SetFillResourceName(ACheckMark.ResourceName)
      .SetFillWrapMode(ACheckMark.WrapMode)
      .Draw;

  end;

end;

{********************************************************}
Procedure TALDynamicListBoxBaseCheckBox.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const ACheckMark: TCheckMarkBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect.ReducePrecision;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AFill, // const AFill: TALBrush;
                        nil, // const AFillResourceStream: TStream;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  if ACheckMark.HasCheckMark then begin
    var LCheckMarkRect := ABufDrawableRect;
    LCheckMarkRect.Inflate(-ACheckMark.margins.Left, -ACheckMark.margins.top, -ACheckMark.margins.right, -ACheckMark.margins.Bottom);
    LSurfaceRect := TRectF.Union(LCheckMarkRect, LSurfaceRect);
  end;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, ACheckMark.Color)
        .SetDrawStateLayerOnTop(False)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetSides(AllSides)
        .SetCorners(AllCorners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

      DrawCheckMark(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        ABufDrawableRect, // const ADstRect: TrectF;
        Checked, // const AChecked: Boolean
        ACheckMark); // const ACheckMark: TCheckMarkBrush;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALDynamicListBoxBaseCheckBox.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.IsTransitionAnimationRunning then begin
    Result := ARect;
    if StateStyles.TransitionFrom <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.TransitionTo <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxBaseCheckBox.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxBaseCheckBox.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{********************************************}
procedure TALDynamicListBoxBaseCheckBox.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;

  var LDrawable: TALDrawable := ALNullDrawable;
  var LDrawableRect: TRectF := TRectF.Empty;
  if not StateStyles.IsTransitionAnimationRunning then begin
    //--
    var LSubIndexOffset: Integer;
    var LDefaultStateStyle: TBaseStateStyle;
    if Checked then begin
      LSubIndexOffset := GetCacheSubIndex{+0};
      LDefaultStateStyle := StateStyles.Checked.Default;
    end
    else begin
      LSubIndexOffset := GetCacheSubIndex+5;
      LDefaultStateStyle := StateStyles.UnChecked.Default;
    end;
    //--
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LStateStyle.FBufDrawable{ADrawable}, LStateStyle.FBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LStateStyle.FBufDrawable)
          else LStateStyle.FBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LStateStyle.FBufDrawable;
          LDrawableRect := LStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
    If ALIsDrawableNull(LDrawable) then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        if LStateStyle = nil then MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LDefaultStateStyle.fBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDefaultStateStyle.fBufDrawable{ADrawable}, LDefaultStateStyle.fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LDefaultStateStyle.fBufDrawable)
          else LDefaultStateStyle.fBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LDefaultStateStyle.FBufDrawable;
          LDrawableRect := LDefaultStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect.ReducePrecision, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect.ReducePrecision;

      if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then begin
        var LLayerRect := ALGetShapeSurfaceRect(
                            LRect, // const ARect: TrectF;
                            LCurrentAdjustedStateStyle.Fill.Color, // const AFillColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Fill.Gradient.Colors, // const AFillGradientColors: TArray<TAlphaColor>;
                            LCurrentAdjustedStateStyle.Fill.ResourceName, // const AFillResourceName: String;
                            nil, // const AFillResourceStream: TStream;
                            LCurrentAdjustedStateStyle.Fill.BackgroundMargins.Rect, // Const AFillBackgroundMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.Fill.ImageMargins.Rect, // Const AFillImageMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.StateLayer.Opacity, // const AStateLayerOpacity: Single;
                            LCurrentAdjustedStateStyle.StateLayer.Color, // const AStateLayerColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.StateLayer.UseContentColor, // const AStateLayerUseContentColor: Boolean;
                            LCurrentAdjustedStateStyle.StateLayer.Margins.Rect, // Const AStateLayerMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.Shadow.Color, // const AShadowColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Shadow.Blur, // const AShadowBlur: Single;
                            LCurrentAdjustedStateStyle.Shadow.OffsetX, // const AShadowOffsetX: Single;
                            LCurrentAdjustedStateStyle.Shadow.OffsetY); // const AShadowOffsetY: Single);
        ALBeginTransparencyLayer(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const aCanvas: TALCanvas;
          LLayerRect, // const ARect: TRectF;
          AbsoluteOpacity); // const AOpacity: Single);
      end;
      try

        TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
          .SetAlignToPixel(IsPixelAlignmentEnabled)
          .SetDstRect(LRect)
          .SetFill(LCurrentAdjustedStateStyle.Fill)
          .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, LCurrentAdjustedStateStyle.CheckMark.Color)
          .SetDrawStateLayerOnTop(False)
          .SetStroke(LCurrentAdjustedStateStyle.Stroke)
          .SetShadow(LCurrentAdjustedStateStyle.Shadow)
          .SetSides(AllSides)
          .SetCorners(AllCorners)
          .SetXRadius(XRadius)
          .SetYRadius(YRadius)
          .Draw;

        DrawCheckMark(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
          1, // const AScale: Single;
          LRect, // const ADstRect: TrectF;
          // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
          // Without this, the checkMark disappears immediately.
          Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
          LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

      finally
        if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then
          ALEndTransparencyLayer(TSkCanvasCustom(Canvas).Canvas.Handle);
      end;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect.ReducePrecision;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      TALDrawRectangleHelper.Create(RenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LRect)
        .SetFill(LCurrentAdjustedStateStyle.Fill)
        .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, LCurrentAdjustedStateStyle.CheckMark.Color)
        .SetDrawStateLayerOnTop(False)
        .SetStroke(LCurrentAdjustedStateStyle.Stroke)
        .SetShadow(LCurrentAdjustedStateStyle.Shadow)
        .SetSides(AllSides)
        .SetCorners(AllCorners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

      DrawCheckMark(
        RenderTargetCanvas, // const ACanvas: TALCanvas;
        ALGetScreenScale, // const AScale: Single;
        LRect, // const ADstRect: TrectF;
        // Check CheckMark.Color = TalphaColors.Null to enable an interpolation fade-out effect on the checkMark.
        // Without this, the checkMark disappears immediately.
        Checked or (TBaseStateStyle(StateStyles.GetCurrentRawStyle).CheckMark.Color = TalphaColors.Null), // const AChecked: Boolean
        LCurrentAdjustedStateStyle.CheckMark); // const ACheckMark: TCheckMarkBrush;

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect.ReducePrecision)
    // (due to scale), we must center LStateStyle.FBufDrawableRect within the main BufDrawableRect (LocalRect.ReducePrecision)
    // to ensure that all changes are visually centered.
    var LCenteredRect := LRect.CenterAt(LocalRect.ReducePrecision);
    LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{**************************************************************}
function TALDynamicListBoxCheckBox.GetStateStyles: TStateStyles;
begin
  Result := TStateStyles(inherited StateStyles);
end;

{*****************************************************************************}
procedure TALDynamicListBoxCheckBox.SetStateStyles(const AValue: TStateStyles);
begin
  inherited StateStyles := AValue;
end;

{***********************************************************************************************}
function TALDynamicListBoxCheckBox.CreateStateStyles: TALDynamicListBoxBaseCheckBox.TStateStyles;
begin
  Result := TStateStyles.Create(Self);
end;

{*************************************************************************************}
function TALDynamicListBoxRadioButton.TCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(5,5,5,5);
end;

{*****************************************************************************}
function TALDynamicListBoxRadioButton.TCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{********************************************************************************************}
function TALDynamicListBoxRadioButton.TInheritCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(5,5,5,5);
end;

{************************************************************************************}
function TALDynamicListBoxRadioButton.TInheritCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{*******************************************************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TDefaultStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{********************************************************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TDisabledStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{*******************************************************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.THoveredStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{*******************************************************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TPressedStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{*******************************************************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TFocusedStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{********************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{**********************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{********************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{********************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{********************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{********************************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TStateStyles.CreateCheckedStateStyles(const AParent: TALDynamicListBoxControl): TALDynamicListBoxBaseCheckBox.TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{**********************************************************************************************************************************************************************}
function TALDynamicListBoxRadioButton.TStateStyles.CreateUncheckedStateStyles(const AParent: TALDynamicListBoxControl): TALDynamicListBoxBaseCheckBox.TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{*********************************************************************}
constructor TALDynamicListBoxRadioButton.Create(const AOwner: TObject);
begin
  inherited;
  FGroupName := '';
  fMandatory := false;
  TMessageManager.DefaultManager.SubscribeToMessage(TRadioButtonGroupMessage, GroupMessageCall);
end;

{**********************************************}
destructor TALDynamicListBoxRadioButton.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TRadioButtonGroupMessage, GroupMessageCall);
  inherited;
end;

{***************************************************************************************************}
function TALDynamicListBoxRadioButton.CreateCheckMark: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush;
begin
  Result := TCheckMarkBrush.Create;
end;

{**************************************************************************************************}
function TALDynamicListBoxRadioButton.CreateStateStyles: TALDynamicListBoxBaseCheckBox.TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{**********************************************************************}
procedure TALDynamicListBoxRadioButton.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then begin
    //if (csDesigning in ComponentState) and FChecked then inherited SetChecked(Value) // allows check/uncheck in design-mode
    //else begin
      if (not value) and fMandatory then exit;
      inherited SetChecked(Value);
      if Value then begin
        var M := TRadioButtonGroupMessage.Create(GroupName);
        TMessageManager.DefaultManager.SendMessage(Self, M, True);
      end;
    //end;
  end;
end;

{**************************************************************}
function TALDynamicListBoxRadioButton.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{**************************************************************}
function TALDynamicListBoxRadioButton.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{***********************************************************}
function TALDynamicListBoxRadioButton.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(20, 20);
end;

{*********************************************************}
function TALDynamicListBoxRadioButton.GetGroupName: string;
begin
  Result := FGroupName;
end;

{************************************************************************************************}
procedure TALDynamicListBoxRadioButton.GroupMessageCall(const Sender: TObject; const M: TMessage);
begin
  if SameText(TRadioButtonGroupMessage(M).GroupName, GroupName) and (Sender <> Self) and (OwnerListBox <> nil) and
     (not (Sender is TALDynamicListBoxControl) or ((Sender as TALDynamicListBoxControl).OwnerListBox = OwnerListBox)) then begin
    var LOldMandatory := fMandatory;
    fMandatory := False;
    try
      Checked := False;
    finally
      fMandatory := LOldMandatory;
    end;
  end;
end;

{*************************************************************}
function TALDynamicListBoxRadioButton.GroupNameStored: Boolean;
begin
  Result := FGroupName <> '';
end;

{***********************************************************************}
procedure TALDynamicListBoxRadioButton.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
    FGroupName := Value;
end;

{***************************************************}
procedure TALDynamicListBoxRadioButton.DrawCheckMark(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const ADstRect: TrectF;
            const AChecked: Boolean;
            const ACheckMark: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush);
begin

  var LRect := ADstRect;
  LRect.Top := LRect.Top * AScale;
  LRect.right := LRect.right * AScale;
  LRect.left := LRect.left * AScale;
  LRect.bottom := LRect.bottom * AScale;
  var LScaledMarginsRect := ACheckMark.Margins.Rect;
  LScaledMarginsRect.Left := LScaledMarginsRect.Left * AScale;
  LScaledMarginsRect.right := LScaledMarginsRect.right * AScale;
  LScaledMarginsRect.top := LScaledMarginsRect.top * AScale;
  LScaledMarginsRect.bottom := LScaledMarginsRect.bottom * AScale;
  LRect.Top := LRect.Top + LScaledMarginsRect.top;
  LRect.right := LRect.right - LScaledMarginsRect.right;
  LRect.left := LRect.left + LScaledMarginsRect.left;
  LRect.bottom := LRect.bottom - LScaledMarginsRect.bottom;
  if LRect.IsEmpty then exit;

  // Without ResourceName
  if ACheckMark.ResourceName = '' then begin

    // exit if not checked
    if not AChecked then
      exit;

    TALDrawRectangleHelper.Create(ACanvas)
      .SetAlignToPixel(IsPixelAlignmentEnabled)
      .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(LRect))
      .SetFillColor(ACheckMark.Color)
      .SetFillResourceName(ACheckMark.ResourceName)
      .SetFillWrapMode(ACheckMark.WrapMode)
      .SetXRadius(-50)
      .SetYRadius(-50)
      .Draw;

  end

  // With ResourceName
  else begin

    TALDrawRectangleHelper.Create(ACanvas)
      .SetAlignToPixel(IsPixelAlignmentEnabled)
      .SetDstRect(LRect)
      .SetFillColor(ACheckMark.Color)
      .SetFillResourceName(ACheckMark.ResourceName)
      .SetFillWrapMode(ACheckMark.WrapMode)
      .Draw;

  end;

end;

{*************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffc5c5c5;
end;

{***************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{*****************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TBaseStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffc5c5c5;
end;

{*******************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{********************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TBaseStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{********************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TBaseStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{***********************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{*************************************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{**************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{***********************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{*******************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{***********************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TDefaultStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 1;
end;

{********************************************************************************************}
constructor TALDynamicListBoxSwitch.TTrack.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{***************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*****************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{******************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TDisabledStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 2;
end;

{***********************************************************************************}
function TALDynamicListBoxSwitch.TTrack.THoveredStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 3;
end;

{***********************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TPressedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 4;
end;

{***********************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TFocusedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 5;
end;

{***********************************************************************************************************}
constructor TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.Create(const AParent: TALDynamicListBoxControl);
begin
  inherited Create;
  //--
  FDefault := CreateDefaultStateStyle(AParent);
  FDefault.OnChanged := DefaultChanged;
  //--
  FDisabled := CreateDisabledStateStyle(FDefault);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(FDefault);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(FDefault);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(FDefault);
  FFocused.OnChanged := FocusedChanged;
end;

{******************************************************************}
destructor TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.Destroy;
begin
  ALFreeAndNil(FDefault);
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.CreateSavedState: TALPersistentObserver;
type
  TCheckStateStylesClass = class of TCheckStateStyles;
begin
  result := TCheckStateStylesClass(classtype).Create(nil{AParent});
end;

{****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{******************************************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{*************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.Assign(Source: TPersistent);
begin
  if Source is TCheckStateStyles then begin
    BeginUpdate;
    Try
      Default.Assign(TCheckStateStyles(Source).Default);
      Disabled.Assign(TCheckStateStyles(Source).Disabled);
      Hovered.Assign(TCheckStateStyles(Source).Hovered);
      Pressed.Assign(TCheckStateStyles(Source).Pressed);
      Focused.Assign(TCheckStateStyles(Source).Focused);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{***************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Default.Reset;
    Disabled.Reset;
    Hovered.Reset;
    Pressed.Reset;
    Focused.Reset;
  finally
    EndUpdate;
  end;
end;

{**********************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    Default.AlignToPixel;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{**************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.ClearBufDrawable;
begin
  Default.ClearBufDrawable;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{******************************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.SetDefault(const AValue: TDefaultStateStyle);
begin
  FDefault.Assign(AValue);
end;

{********************************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{******************************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{******************************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{******************************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{******************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.DefaultChanged(ASender: TObject);
begin
  Change;
end;

{*******************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TCheckStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************************}
constructor TALDynamicListBoxSwitch.TTrack.TStateStyles.Create(const AParent: TALDynamicListBoxControl);
begin
  inherited Create(AParent);
  //--
  FChecked := CreateCheckedStateStyles(AParent);
  FChecked.OnChanged := CheckedChanged;
  //--
  FUnchecked := CreateUnCheckedStateStyles(AParent);
  FUnchecked.OnChanged := UncheckedChanged;
end;

{*************************************************************}
destructor TALDynamicListBoxSwitch.TTrack.TStateStyles.Destroy;
begin
  ALFreeAndNil(FChecked);
  ALFreeAndNil(FUnchecked);
  inherited Destroy;
end;

{****************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TStateStyles.CreateCheckedStateStyles(const AParent: TALDynamicListBoxControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{******************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TStateStyles.CreateUncheckedStateStyles(const AParent: TALDynamicListBoxControl): TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{********************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Checked.Assign(TStateStyles(Source).Checked);
      Unchecked.Assign(TStateStyles(Source).Unchecked);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.reset;
    Unchecked.reset;
  finally
    EndUpdate;
  end;
end;

{*****************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Checked.AlignToPixel;
    Unchecked.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Checked.ClearBufDrawable;
  Unchecked.ClearBufDrawable;
end;

{*******************************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TStateStyles.GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle;
begin
  if Parent.Checked then begin
    if Not Parent.Enabled then Result := Checked.Disabled
    else if Parent.Pressed then Result := Checked.Pressed
    //else if Parent.IsFocused then Result := Checked.Focused
    else if Parent.IsMouseOver then Result := Checked.Hovered
    else result := Checked.Default;
  end
  else begin
    if Not Parent.Enabled then Result := UnChecked.Disabled
    else if Parent.Pressed then Result := UnChecked.Pressed
    //else if Parent.IsFocused then Result := UnChecked.Focused
    else if Parent.IsMouseOver then Result := UnChecked.Hovered
    else result := UnChecked.Default;
  end;
end;

{*********************************************************************************************}
function TALDynamicListBoxSwitch.TTrack.TStateStyles.GetParent: TALDynamicListBoxSwitch.TTrack;
begin
  Result := TALDynamicListBoxSwitch.TTrack(inherited Parent);
end;

{************************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TStateStyles.SetChecked(const AValue: TCheckStateStyles);
begin
  FChecked.Assign(AValue);
end;

{**************************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TStateStyles.SetUnchecked(const AValue: TCheckStateStyles);
begin
  FUnchecked.Assign(AValue);
end;

{*************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TStateStyles.CheckedChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.TStateStyles.UncheckedChanged(ASender: TObject);
begin
  Change;
end;

{***********************************************************************}
constructor TALDynamicListBoxSwitch.TTrack.Create(const AOwner: TObject);
begin
  inherited;
  //--
  //SetAcceptsControls(False);
  //CanFocus := False;
  //Locked := True;
  HitTest := False;
  //--
  FChecked := False;
  FDoubleBuffered := true;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FCacheIndex := 0;
  FCacheEngine := nil;
  {$IF NOT DEFINED(ALSkiaCanvas)}
  FRenderTargetSurface := ALNullSurface;
  FRenderTargetCanvas := ALNullCanvas;
  FRenderTargetDrawable := ALNullDrawable;
  {$ENDIF}
  //--
  // Must be created at the end because it requires FCheckMark to
  // be already created.
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{************************************************}
destructor TALDynamicListBoxSwitch.TTrack.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  {$IF NOT DEFINED(ALSkiaCanvas)}
  ClearRenderTargets;
  {$ENDIF}
  inherited;
end;

{***********************************************************}
function TALDynamicListBoxSwitch.TTrack.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{*******************************************************************}
function TALDynamicListBoxSwitch.TTrack.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{**********************************************************************}
function TALDynamicListBoxSwitch.TTrack.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{****************************************************}
procedure TALDynamicListBoxSwitch.TTrack.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*************************************************************}
function TALDynamicListBoxSwitch.TTrack.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(52, 32);
end;

{****************************************************************}
function TALDynamicListBoxSwitch.TTrack.GetCacheSubIndex: Integer;
begin
  // The Thumb uses 11 slots:
  // 0     - Unused
  // 1..5  - Checked state drawables
  // 6..10 - Unchecked state drawables
  Result := 11;
end;

{*****************************************************************}
function TALDynamicListBoxSwitch.TTrack.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{********************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then ClearBufDrawable
    {$IF NOT DEFINED(ALSkiaCanvas)}
    else ClearRenderTargets;
    {$ENDIF}
  end;
end;

{**********************************************************}
function TALDynamicListBoxSwitch.TTrack.GetChecked: Boolean;
begin
  Result := FChecked;
end;

{************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then begin
    FChecked := Value;
    if FChecked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
    else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
    DoChanged;
  end;
end;

{**********************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{***************************************************************}
function TALDynamicListBoxSwitch.TTrack.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{***************************************************************}
function TALDynamicListBoxSwitch.TTrack.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{****************************************************************}
function TALDynamicListBoxSwitch.TTrack.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{****************************************************************}
function TALDynamicListBoxSwitch.TTrack.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{***********************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FXRadius := Value;
    Repaint;
  end;
end;

{***********************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    ClearBufDrawable;
    FYRadius := Value;
    Repaint;
  end;
end;

{********************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.FillChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.StrokeChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.ShadowChanged(Sender: TObject);
begin
  ClearBufDrawable;
  inherited;
end;

{***************************************************************************}
procedure TALDynamicListBoxSwitch.TTrack.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  if Checked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
  else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
  Repaint;
end;

{**********************************************************}
procedure TALDynamicListBoxSwitch.TTrack.IsMouseOverChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{**********************************************************}
//procedure TALDynamicListBoxSwitch.TTrack.IsFocusedChanged;
//begin
//  inherited;
//  StateStyles.startTransition;
//  repaint;
//end;

{******************************************************}
procedure TALDynamicListBoxSwitch.TTrack.PressedChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{*************************************************}
procedure TALDynamicListBoxSwitch.TTrack.DoChanged;
begin
  Repaint;
end;

{*************************************************}
procedure TALDynamicListBoxSwitch.TTrack.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{********************************************************}
procedure TALDynamicListBoxSwitch.TTrack.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not IsDestroying) and
     ((not ALIsDrawableNull(FStateStyles.Checked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Checked.Focused.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Default.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.UnChecked.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
end;

{*******************************************************}
procedure TALDynamicListBoxSwitch.TTrack.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _DoMakeBufDrawable(const AStateStyle: TBaseStateStyle): boolean;
  begin
    if (not ALIsDrawableNull(AStateStyle.FBufDrawable)) then exit(False);
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try

      {$IFDEF debug}
      ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + AStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
      {$endif}

      CreateBufDrawable(
        AStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
        AStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
        ALGetScreenScale * AStateStyle.Scale, // const AScale: Single;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.Shadow); // const AShadow: TALShadow);

      // LStateStyle.FBufDrawableRect must include the LScale
      AStateStyle.FBufDrawableRect.Top := AStateStyle.FBufDrawableRect.Top * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.right := AStateStyle.FBufDrawableRect.right * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.left := AStateStyle.FBufDrawableRect.left * AStateStyle.Scale;
      AStateStyle.FBufDrawableRect.bottom := AStateStyle.FBufDrawableRect.bottom * AStateStyle.Scale;

      // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect.ReducePrecision)
      // (due to scale), we must center LStateStyle.FBufDrawableRect within the main BufDrawableRect (LocalRect.ReducePrecision)
      // to ensure that all changes are visually centered.
      var LCenteredRect := AStateStyle.FBufDrawableRect.CenterAt(LocalRect.ReducePrecision);
      AStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    finally
      AStateStyle.RestorestateNoChanges;
    end;
    Result := True;
  end;

begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  var LSubIndexOffset: Integer;
  var LDefaultStateStyle: TBaseStateStyle;
  if Checked then begin
    LSubIndexOffset := GetCacheSubIndex{+0};
    LDefaultStateStyle := StateStyles.Checked.Default;
  end
  else begin
    LSubIndexOffset := GetCacheSubIndex+5;
    LDefaultStateStyle := StateStyles.UnChecked.Default;
  end;
  //--
  if (CacheIndex = 0) or
     (CacheEngine = nil) or
     (not CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex})) then
    _DoMakeBufDrawable(LDefaultStateStyle);
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex})) then Exit;
  _DoMakeBufDrawable(LStateStyle);
  // No need to center LStateStyle.FBufDrawableRect on the main BufDrawableRect
  // because BufDrawableRect always has the width and height of the localRect.
end;

{*********************************************************}
Procedure TALDynamicListBoxSwitch.TTrack.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect.ReducePrecision;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AFill, // const AFill: TALBrush;
                        nil, // const AFillResourceStream: TStream;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, TAlphaColors.Null)
        .SetDrawStateLayerOnTop(False)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetSides(AllSides)
        .SetCorners(AllCorners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALDynamicListBoxSwitch.TTrack.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.IsTransitionAnimationRunning then begin
    Result := ARect;
    if StateStyles.TransitionFrom <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.TransitionTo <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxSwitch.TTrack.InitRenderTargets(var ARect: TrectF);
begin
  var LSurfaceRect := GetRenderTargetRect(ARect);
  ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);
  ALInitControlRenderTargets(
    LSurfaceRect, // Const ARect: TrectF;
    FRenderTargetSurface, // var ARenderTargetSurface: TALSurface;
    FRenderTargetCanvas, // var ARenderTargetCanvas: TALCanvas;
    FRenderTargetDrawable); // var ARenderTargetDrawable: TALDrawable):
end;
{$ENDIF}

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
procedure TALDynamicListBoxSwitch.TTrack.ClearRenderTargets;
begin
  ALFreeAndNilDrawable(FRenderTargetDrawable);
  ALFreeAndNilSurface(FRenderTargetSurface, FRenderTargetCanvas);
end;
{$ENDIF}

{*********************************************}
procedure TALDynamicListBoxSwitch.TTrack.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;

  var LDrawable: TALDrawable := ALNullDrawable;
  var LDrawableRect: TRectF := TRectF.Empty;
  if not StateStyles.IsTransitionAnimationRunning then begin
    //--
    var LSubIndexOffset: Integer;
    var LDefaultStateStyle: TBaseStateStyle;
    if Checked then begin
      LSubIndexOffset := GetCacheSubIndex{+0};
      LDefaultStateStyle := StateStyles.Checked.Default;
    end
    else begin
      LSubIndexOffset := GetCacheSubIndex+5;
      LDefaultStateStyle := StateStyles.UnChecked.Default;
    end;
    //--
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LStateStyle.FBufDrawable{ADrawable}, LStateStyle.FBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LStateStyle.FBufDrawable)
          else LStateStyle.FBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LStateStyle.FBufDrawable;
          LDrawableRect := LStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
    If ALIsDrawableNull(LDrawable) then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        if LStateStyle = nil then MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LDefaultStateStyle.fBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDefaultStateStyle.fBufDrawable{ADrawable}, LDefaultStateStyle.fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LDefaultStateStyle.fBufDrawable)
          else LDefaultStateStyle.fBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, LSubIndexOffset+LDefaultStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LDefaultStateStyle.FBufDrawable;
          LDrawableRect := LDefaultStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect.ReducePrecision, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect.ReducePrecision;

      if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then begin
        var LLayerRect := ALGetShapeSurfaceRect(
                            LRect, // const ARect: TrectF;
                            LCurrentAdjustedStateStyle.Fill.Color, // const AFillColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Fill.Gradient.Colors, // const AFillGradientColors: TArray<TAlphaColor>;
                            LCurrentAdjustedStateStyle.Fill.ResourceName, // const AFillResourceName: String;
                            nil, // const AFillResourceStream: TStream;
                            LCurrentAdjustedStateStyle.Fill.BackgroundMargins.Rect, // Const AFillBackgroundMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.Fill.ImageMargins.Rect, // Const AFillImageMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.StateLayer.Opacity, // const AStateLayerOpacity: Single;
                            LCurrentAdjustedStateStyle.StateLayer.Color, // const AStateLayerColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.StateLayer.UseContentColor, // const AStateLayerUseContentColor: Boolean;
                            LCurrentAdjustedStateStyle.StateLayer.Margins.Rect, // Const AStateLayerMarginsRect: TRectF;
                            LCurrentAdjustedStateStyle.Shadow.Color, // const AShadowColor: TAlphaColor;
                            LCurrentAdjustedStateStyle.Shadow.Blur, // const AShadowBlur: Single;
                            LCurrentAdjustedStateStyle.Shadow.OffsetX, // const AShadowOffsetX: Single;
                            LCurrentAdjustedStateStyle.Shadow.OffsetY); // const AShadowOffsetY: Single);
        ALBeginTransparencyLayer(
          TSkCanvasCustom(Canvas).Canvas.Handle, // const aCanvas: TALCanvas;
          LLayerRect, // const ARect: TRectF;
          AbsoluteOpacity); // const AOpacity: Single);
      end;
      try

        TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
          .SetAlignToPixel(IsPixelAlignmentEnabled)
          .SetDstRect(LRect)
          .SetFill(LCurrentAdjustedStateStyle.Fill)
          .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TAlphaColors.Null)
          .SetDrawStateLayerOnTop(False)
          .SetStroke(LCurrentAdjustedStateStyle.Stroke)
          .SetShadow(LCurrentAdjustedStateStyle.Shadow)
          .SetSides(AllSides)
          .SetCorners(AllCorners)
          .SetXRadius(XRadius)
          .SetYRadius(YRadius)
          .Draw;

      finally
        if compareValue(AbsoluteOpacity, 1, Tepsilon.Scale) < 0 then
          ALEndTransparencyLayer(TSkCanvasCustom(Canvas).Canvas.Handle);
      end;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect.ReducePrecision;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      TALDrawRectangleHelper.Create(RenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LRect)
        .SetFill(LCurrentAdjustedStateStyle.Fill)
        .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TAlphaColors.Null)
        .SetDrawStateLayerOnTop(False)
        .SetStroke(LCurrentAdjustedStateStyle.Stroke)
        .SetShadow(LCurrentAdjustedStateStyle.Shadow)
        .SetSides(AllSides)
        .SetCorners(AllCorners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect (LocalRect.ReducePrecision)
    // (due to scale), we must center LStateStyle.FBufDrawableRect within the main BufDrawableRect (LocalRect.ReducePrecision)
    // to ensure that all changes are visually centered.
    var LCenteredRect := LRect.CenterAt(LocalRect.ReducePrecision);
    LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{***************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(6,6,6,6);
end;

{*******************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{**********************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TInheritCheckMarkBrush.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(6,6,6,6);
end;

{**************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TInheritCheckMarkBrush.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{**********************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TDefaultStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TDefaultStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*********************************************************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TDefaultStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{***********************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TDisabledStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{*****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TDisabledStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{**********************************************************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TDisabledStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{**********************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.THoveredStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.THoveredStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*********************************************************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.THoveredStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{**********************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TPressedStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TPressedStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*********************************************************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TPressedStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{**********************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TFocusedStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null;
end;

{****************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TFocusedStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*********************************************************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TFocusedStateStyle.CreateCheckMark(const AParent: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush): TALDynamicListBoxBaseCheckBox.TInheritCheckMarkBrush;
begin
  Result := TInheritCheckMarkBrush.Create(AParent);
end;

{**********************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TCheckStateStyles.CreateDefaultStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TDefaultStateStyle;
begin
  Result := TDefaultStateStyle.Create(AParent);
end;

{************************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TCheckStateStyles.CreateDisabledStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{**********************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TCheckStateStyles.CreateHoveredStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{**********************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TCheckStateStyles.CreatePressedStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{**********************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TCheckStateStyles.CreateFocusedStateStyle(const AParent: TObject): TALDynamicListBoxBaseCheckBox.TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{**********************************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TStateStyles.CreateCheckedStateStyles(const AParent: TALDynamicListBoxControl): TALDynamicListBoxBaseCheckBox.TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{************************************************************************************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.TStateStyles.CreateUncheckedStateStyles(const AParent: TALDynamicListBoxControl): TALDynamicListBoxBaseCheckBox.TCheckStateStyles;
begin
  Result := TCheckStateStyles.Create(AParent);
end;

{********************************************************************}
procedure TALDynamicListBoxSwitch.TThumb.TStateStyles.StartTransition;
begin
  FStartPositionX := Parent{Thumb}.Left;
  inherited;
  if not IsTransitionAnimationRunning then
    TALDynamicListBoxSwitch(Parent{Thumb}.OwnerControl{Track}.OwnerControl{Switch}).AlignThumb;
end;

{************************************************************************************************}
procedure TALDynamicListBoxSwitch.TThumb.TStateStyles.TransitionAnimationProcess(Sender: TObject);
begin
  var LThumb := Parent;
  var LSwitch := TALDynamicListBoxSwitch(LThumb.OwnerControl{Track}.OwnerControl{Switch});
  if (not LSwitch.Pressed) and (Lthumb.Align = TALAlignLayout.None) then begin
    var LFloatAnimation := TALFloatAnimation(Sender);
    var LStopPositionX: Single;
    If LSwitch.Checked then LStopPositionX := LSwitch.GetMaxThumbPos
    else LStopPositionX := LSwitch.GetMinThumbPos;
    LThumb.Left := FStartPositionX + (LStopPositionX - FStartPositionX) * LFloatAnimation.CurrentValue;
  end;
  inherited;
end;

{***********************************************************************************************}
procedure TALDynamicListBoxSwitch.TThumb.TStateStyles.TransitionAnimationFinish(Sender: TObject);
begin
  TALDynamicListBoxSwitch(Parent{Thumb}.OwnerControl{Track}.OwnerControl{Switch}).AlignThumb;
  inherited;
end;

{***********************************************************************}
function TALDynamicListBoxSwitch.TThumb.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(4,4,4,4);
end;

{***********************************************************************}
constructor TALDynamicListBoxSwitch.TThumb.Create(const AOwner: TObject);
begin
  inherited;
  //--
  //SetAcceptsControls(False);
  //CanFocus := False;
  //Locked := True;
  HitTest := False;
  //--
  //Margins.DefaultValue := TRectF.Create(4,4,4,4);
  //Margins.Rect := Margins.DefaultValue;
end;

{***************************************************************}
function TALDynamicListBoxSwitch.TThumb.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{*******************************************************************}
function TALDynamicListBoxSwitch.TThumb.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{*****************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.CreateCheckMark: TALDynamicListBoxBaseCheckBox.TCheckMarkBrush;
begin
  Result := TCheckMarkBrush.Create;
end;

{****************************************************************************************************}
function TALDynamicListBoxSwitch.TThumb.CreateStateStyles: TALDynamicListBoxBaseCheckBox.TStateStyles;
begin
  result := TStateStyles.Create(Self);
end;

{****************************************************************}
function TALDynamicListBoxSwitch.TThumb.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{****************************************************************}
function TALDynamicListBoxSwitch.TThumb.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{*************************************************************}
function TALDynamicListBoxSwitch.TThumb.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(24, 24);
end;

{*********************************************}
procedure TALDynamicListBoxSwitch.TThumb.Click;
begin
  // Since TALDynamicListBoxSwitch.TThumb has HitTest set to false, this event
  // is triggered only at the end of the transition animation when
  // DelayClick is set to true.
  TALDynamicListBoxSwitch(OwnerControl{Track}.OwnerControl{Switch}).click;
end;

{****************************************************************}
constructor TALDynamicListBoxSwitch.Create(const AOwner: TObject);
begin
  inherited;
  //CanFocus := True;
  //SetAcceptsControls(False);
  AutoCapture := True;
  Cursor := crHandPoint;
  DisabledOpacity := 1;
  //--
  FOnChange := nil;
  FPressedThumbPos := TpointF.create(0,0);
  //--
  fScrollCapturedByMe := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  //--
  FTransition := TALStateTransition.Create;
  FTransition.OnChanged := TransitionChanged;
  //--
  FTrack := CreateTrack;
  //FTrack.Parent := self;
  //FTrack.Stored := False;
  //FTrack.SetSubComponent(True);
  //FTrack.Name := 'Track'; // Useful at design time in the IDE
  FTrack.Align := TALAlignLayout.Client;
  //--
  // Use 'self' instead of 'FTrack' to ensure that
  // 'Fthumb.loaded' is called.
  FThumb := CreateThumb;
  //FThumb.Parent := FTrack;
  //FThumb.Stored := False;
  //FThumb.SetSubComponent(True);
  //FThumb.Name := 'Thumb'; // Useful at design time in the IDE
end;

{*****************************************}
destructor TALDynamicListBoxSwitch.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  ALFreeAndNil(FTransition);
  inherited;
end;

{**************************************************}
procedure TALDynamicListBoxSwitch.AfterConstruction;
begin
  inherited;
  //if not IsOwnerLoading then
    AlignThumb;
end;

{***************************************************}
function TALDynamicListBoxSwitch.CreateTrack: TTrack;
begin
  Result := TTrack.Create(self);
end;

{***************************************************}
function TALDynamicListBoxSwitch.CreateThumb: TThumb;
begin
  Result := TThumb.Create(self);
end;

{*****************************************}
//procedure TALDynamicListBoxSwitch.Loaded;
//begin
//  inherited;
//  Thumb.StateStyles.Transition.Assign(Transition);
//  Track.StateStyles.Transition.Assign(Transition);
//  AlignThumb;
//end;

{*********************************************}
procedure TALDynamicListBoxSwitch.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    Thumb.AlignToPixel;
    Track.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************************************}
procedure TALDynamicListBoxSwitch.MakeBufDrawable;
begin
  Track.MakeBufDrawable;
  Thumb.MakeBufDrawable;
end;

{*************************************************}
procedure TALDynamicListBoxSwitch.ClearBufDrawable;
begin
  Track.ClearBufDrawable;
  Thumb.ClearBufDrawable;
end;

{******************************************************}
function TALDynamicListBoxSwitch.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(52, 32);
end;

{**********************************************************}
function TALDynamicListBoxSwitch.GetDoubleBuffered: boolean;
begin
  result := Track.DoubleBuffered and Thumb.DoubleBuffered;
end;

{*************************************************************************}
procedure TALDynamicListBoxSwitch.SetDoubleBuffered(const AValue: Boolean);
begin
  Track.DoubleBuffered := AValue;
  Thumb.DoubleBuffered := AValue;
end;

{***************************************************}
procedure TALDynamicListBoxSwitch.IsMouseOverChanged;
begin
  inherited;
  Track.FIsMouseOver := IsMouseOver;
  Thumb.FIsMouseOver := IsMouseOver;
  Track.IsMouseOverChanged;
  Thumb.IsMouseOverChanged;
end;

{***************************************************}
//procedure TALDynamicListBoxSwitch.IsFocusedChanged;
//begin
//  inherited;
//  Track.FIsFocused := IsFocused;
//  Thumb.FIsFocused := IsFocused;
//  Track.IsFocusedChanged;
//  Thumb.IsFocusedChanged;
//end;

{***********************************************}
procedure TALDynamicListBoxSwitch.PressedChanged;
begin
  inherited;
  Track.Pressed := Pressed;
  Thumb.Pressed := Pressed;
end;

{***********************************************}
procedure TALDynamicListBoxSwitch.EnabledChanged;
begin
  inherited;
  Track.enabled := enabled;
  Thumb.enabled := enabled;
end;

{*****************************************}
procedure TALDynamicListBoxSwitch.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{*******************************************************************************}
procedure TALDynamicListBoxSwitch.SetTransition(const Value: TALStateTransition);
begin
  FTransition.Assign(Value);
end;

{********************************************************************}
procedure TALDynamicListBoxSwitch.TransitionChanged(ASender: TObject);
begin
  //if csLoading in ComponentState then exit;
  Thumb.StateStyles.Transition.Assign(Transition);
  Track.StateStyles.Transition.Assign(Transition);
end;

{************************************************}
procedure TALDynamicListBoxSwitch.StartTransition;
begin
  Thumb.StateStyles.StartTransition;
  Track.StateStyles.StartTransition;
end;

{**************************************************************************************************}
procedure TALDynamicListBoxSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Pressed then begin
    fThumb.Align := TALALignLayout.None;
    FPressedThumbPos := TPointF.Create(Single(FThumb.left), Single(FThumb.top));
  end;
end;

{****************************************************************************}
procedure TALDynamicListBoxSwitch.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALDynamicListBoxSwitch.MouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if Pressed then begin

    if (not fScrollCapturedByMe) then begin
      If (abs(X - PressedPosition.X) > abs(Y - PressedPosition.Y)) and
         (abs(X - PressedPosition.X) > TALScrollEngine.DefaultTouchSlop) then begin
        {$IFDEF DEBUG}
        //ALLog(
        //  'TALDynamicListBoxSwitch.MouseMove',
        //  'ScrollCapturedByMe');
        {$ENDIF}
        PressedPosition := TpointF.Create(X,Y);
        fScrollCapturedByMe := true;
        TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(true), True);
      end;
    end;

    if fScrollCapturedByMe then begin
      var LNewThumbPosX := FPressedThumbPos.x + (X - PressedPosition.X);
      LNewThumbPosX := min(LNewThumbPosX, GetMaxThumbPos);
      LNewThumbPosX := max(LNewThumbPosX, GetMinThumbPos);
      FThumb.Left :=LNewThumbPosX;
    end;

  end;
  inherited;
end;

{************************************************************************************************}
procedure TALDynamicListBoxSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if fScrollCapturedByMe then begin
    FScrollCapturedByMe := False;
    var LChecked: Boolean;
    if fThumb.Left + (fThumb.Width / 2) < Track.Width / 2 then LChecked := False
    else LChecked := True;
    if LChecked <> Checked then begin
      if (transition.DelayClick) and
         (compareValue(FTransition.Duration,0.0,TEpsilon.Scale) > 0) then
        Thumb.StateStyles.TransitionClickDelayed := True;
      FTrack.Checked := LChecked;
      FThumb.Checked := LChecked;
      if not Thumb.StateStyles.TransitionClickDelayed then
        DoChange;
    end;
    fThumb.Align := TALALignLayout.None;
    StartTransition;
  end;
end;

{*******************************************}
procedure TALDynamicListBoxSwitch.MouseLeave;
begin
  inherited;
  if fScrollCapturedByMe then begin
    FScrollCapturedByMe := False;
    var LChecked: Boolean;
    if fThumb.Left + (fThumb.Width / 2) < Track.Width / 2 then LChecked := False
    else LChecked := True;
    if LChecked <> Checked then begin
      if (transition.DelayClick) and
         (compareValue(FTransition.Duration,0.0,TEpsilon.Scale) > 0) then
        Thumb.StateStyles.TransitionClickDelayed := True;
      FTrack.Checked := LChecked;
      FThumb.Checked := LChecked;
      if not Thumb.StateStyles.TransitionClickDelayed then
        DoChange;
    end;
    fThumb.Align := TALALignLayout.None;
    StartTransition;
  end;
end;

{**************************************}
procedure TALDynamicListBoxSwitch.Click;
begin
  // If fScrollCapturedByMe is true, the MouseUp event will handle the task.
  if fScrollCapturedByMe then Exit
  // If Pressed is true, it means this event is triggered by MouseDown/MouseUp.
  // In this case, if a delay is requested for the click, apply the delay.
  else if (Pressed) and
          (Transition.DelayClick) and
          (compareValue(FTransition.Duration,0.0,TEpsilon.Scale) > 0) then begin
    Thumb.StateStyles.TransitionClickDelayed := True;
    var LChecked := not Checked;
    FTrack.Checked := LChecked;
    FThumb.Checked := LChecked;
    fThumb.Align := TALALignLayout.None;
    StartTransition;
    exit;
  end
  // If Pressed is true, it means this event is triggered by MouseDown/MouseUp.
  else if Pressed then begin
    var LChecked := not Checked;
    FTrack.Checked := LChecked;
    FThumb.Checked := LChecked;
    fThumb.Align := TALALignLayout.None;
    DoChange;
    inherited;
    StartTransition;
  end
  // if not Pressed, it means this event is triggered by event like TransitionAnimationFinish
  else begin
    DoChange;
    inherited;
    AlignThumb;
  end;
end;

{*******************************************************************************************************}
procedure TALDynamicListBoxSwitch.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALDynamicListBoxSwitch.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'Pressed: ' + ALBoolToStrW(Pressed));
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    {$IFDEF DEBUG}
    if fScrollCapturedByMe then
      raise Exception.Create('Error 6C41BEC8-3AE9-4EC0-9D80-117ED5697397');
    {$ENDIF}
    Pressed := False;
  end;
end;

{******************************************************}
function TALDynamicListBoxSwitch.GetCacheIndex: integer;
begin
  Result := FThumb.CacheIndex;
end;

{*********************************************************************}
procedure TALDynamicListBoxSwitch.SetCacheIndex(const AValue: Integer);
begin
  FThumb.CacheIndex := AValue;
  FTrack.CacheIndex := AValue;
end;

{*************************************************************************}
function TALDynamicListBoxSwitch.GetCacheEngine: TALBufDrawableCacheEngine;
begin
  Result := FThumb.CacheEngine;
end;

{****************************************************************************************}
procedure TALDynamicListBoxSwitch.SetCacheEngine(const AValue: TALBufDrawableCacheEngine);
begin
  FThumb.CacheEngine := AValue;
  FTrack.CacheEngine := AValue;
end;

{******************************************************}
function TALDynamicListBoxSwitch.GetMinThumbPos: Single;
begin
  result := Track.Padding.left + fThumb.Margins.left;
end;

{******************************************************}
function TALDynamicListBoxSwitch.GetMaxThumbPos: Single;
begin
  result := Track.Width - fThumb.Width - Track.Padding.Right - fThumb.Margins.Right;
end;

{*******************************************}
procedure TALDynamicListBoxSwitch.AlignThumb;
begin
  //if csLoading in ComponentState then exit;
  if pressed or fScrollCapturedByMe then exit;
  If Checked then FThumb.Align := TALAlignLayout.right
  else FThumb.Align := TALAlignLayout.left;
end;

{***************************************************}
function TALDynamicListBoxSwitch.GetChecked: boolean;
begin
  Result := FTrack.Checked and FThumb.Checked;
end;

{*****************************************************************}
procedure TALDynamicListBoxSwitch.SetChecked(const Value: Boolean);
begin
  if GetChecked <> Value then begin
    FTrack.Checked := Value;
    FThumb.Checked := Value;
    AlignThumb;
    DoChange;
  end;
end;

{******************************************************************}
function TALDynamicListBoxButton.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffe1e1e1;
end;

{********************************************************************}
function TALDynamicListBoxButton.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $ffadadad;
end;

{*********************************************************************************}
function TALDynamicListBoxButton.TTextSettings.TFont.GetDefaultWeight: TFontWeight;
begin
  Result := TFontWeight.medium;
end;

{*****************************************************************}
function TALDynamicListBoxButton.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.Create;
end;

{***********************************************************************************}
function TALDynamicListBoxButton.TTextSettings.GetDefaultHorzAlign: TALTextHorzAlign;
begin
  Result := TALTextHorzAlign.center;
end;

{**********************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $FFE1E1E1;
end;

{************************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $FFADADAD;
end;

{*************************************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.TTextSettings.TFont.GetDefaultWeight: TFontWeight;
begin
  Result := TFontWeight.medium;
end;

{*********************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.Create;
end;

{*********************************************************************************}
constructor TALDynamicListBoxButton.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FText := DefaultText;
  //--
  if StateStyleParent <> nil then FTextSettings := CreateTextSettings(StateStyleParent.TextSettings)
  else if ControlParent <> nil then FTextSettings := CreateTextSettings(ControlParent.TextSettings)
  else FTextSettings := CreateTextSettings(nil);
  FTextSettings.OnChanged := TextSettingsChanged;
  //--
  //FPriorSupersedeText
end;

{*********************************************************}
destructor TALDynamicListBoxButton.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{****************************************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{******************************************************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*************************************************************************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.CreateTextSettings(const AParent: TALBaseTextSettings): TBaseStateStyle.TTextSettings;
begin
  Result := TTextSettings.Create(AParent);
end;

{****************************************************************************}
procedure TALDynamicListBoxButton.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      Text := TBaseStateStyle(Source).text;
      TextSettings.Assign(TBaseStateStyle(Source).TextSettings);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{******************************************************}
procedure TALDynamicListBoxButton.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Text := DefaultText;
    TextSettings.reset;
  finally
    EndUpdate;
  end;
end;

{*************************************************************}
procedure TALDynamicListBoxButton.TBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    TextSettings.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***************************************************************************************************************************************}
procedure TALDynamicListBoxButton.TBaseStateStyle.Interpolate(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error F3C72244-894F-4B67-AD86-F24DF5039927');
  {$ENDIF}
  BeginUpdate;
  try
    Inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then begin
      Text := TBaseStateStyle(ATo).Text;
      TextSettings.Interpolate(TBaseStateStyle(ATo).TextSettings, ANormalizedTime);
    end
    else if StateStyleParent <> nil then begin
      StateStyleParent.SupersedeNoChanges(true{ASaveState});
      try
        Text := StateStyleParent.Text;
        TextSettings.Interpolate(StateStyleParent.TextSettings, ANormalizedTime);
      finally
        StateStyleParent.RestoreStateNoChanges;
      end;
    end
    else if ControlParent <> nil then begin
      Text := ControlParent.Text;
      TextSettings.Interpolate(ControlParent.TextSettings, ANormalizedTime);
    end
    else begin
      Text := DefaultText;
      TextSettings.Interpolate(nil, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
procedure TALDynamicListBoxButton.TBaseStateStyle.DoSupersede;
begin
  Inherited;
  //--
  FPriorSupersedeText := Text;
  //--
  if Text = '' then begin
    if StateStyleParent <> nil then Text := StateStyleParent.Text
    else Text := ControlParent.Text;
  end;
  TextSettings.SuperSede;
end;

{************************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.GetStateStyleParent: TBaseStateStyle;
begin
  {$IF defined(debug)}
  if (inherited StateStyleParent <> nil) and
     (not (inherited StateStyleParent is TBaseStateStyle)) then
    raise Exception.Create('StateStyleParent must be of type TBaseStateStyle');
  {$ENDIF}
  Result := TBaseStateStyle(inherited StateStyleParent);
end;

{*****************************************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.GetControlParent: TALDynamicListBoxButton;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TALDynamicListBoxButton)) then
    raise Exception.Create('ControlParent must be of type TALDynamicListBoxButton');
  {$ENDIF}
  Result := TALDynamicListBoxButton(inherited ControlParent);
end;

{*****************************************************************************}
procedure TALDynamicListBoxButton.TBaseStateStyle.SetText(const Value: string);
begin
  if FText <> Value then begin
    FText := Value;
    Change;
  end;
end;

{*************************************************************************************************************}
procedure TALDynamicListBoxButton.TBaseStateStyle.SetTextSettings(const AValue: TBaseStateStyle.TTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

{**********************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.GetDefaultText: String;
begin
  Result := '';
end;

{*******************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            Text.IsEmpty and
            TextSettings.Inherit;
end;

{**************************************************************************************}
procedure TALDynamicListBoxButton.TBaseStateStyle.TextSettingsChanged(ASender: TObject);
begin
  Change;
end;

{*********************************************************************}
function TALDynamicListBoxButton.TBaseStateStyle.IsTextStored: Boolean;
begin
  Result := FText <> DefaultText;
end;

{****************************************************************************}
function TALDynamicListBoxButton.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{************************************************************************************}
procedure TALDynamicListBoxButton.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*************************************************************************************}
constructor TALDynamicListBoxButton.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{********************************************************************************}
procedure TALDynamicListBoxButton.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**********************************************************}
procedure TALDynamicListBoxButton.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{***********************************************************************}
function TALDynamicListBoxButton.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{*****************************************************************************}
function TALDynamicListBoxButton.TDisabledStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 1;
end;

{****************************************************************************}
function TALDynamicListBoxButton.THoveredStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 2;
end;

{****************************************************************************}
function TALDynamicListBoxButton.TPressedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 3;
end;

{****************************************************************************}
function TALDynamicListBoxButton.TFocusedStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 4;
end;

{***********************************************************************************************}
constructor TALDynamicListBoxButton.TStateStyles.Create(const AParent: TALDynamicListBoxControl);
begin
  inherited Create(AParent);
  //--
  FDisabled := CreateDisabledStateStyle(AParent);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(AParent);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(AParent);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(AParent);
  FFocused.OnChanged := FocusedChanged;
end;

{******************************************************}
destructor TALDynamicListBoxButton.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{******************************************************************************************************************}
function TALDynamicListBoxButton.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{****************************************************************************************************************}
function TALDynamicListBoxButton.TStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{****************************************************************************************************************}
function TALDynamicListBoxButton.TStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{****************************************************************************************************************}
function TALDynamicListBoxButton.TStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{*************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TStateStyles(Source).Disabled);
      Hovered.Assign(TStateStyles(Source).Hovered);
      Pressed.Assign(TStateStyles(Source).Pressed);
      Focused.Assign(TStateStyles(Source).Focused);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{***************************************************}
procedure TALDynamicListBoxButton.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.reset;
    Hovered.reset;
    Pressed.reset;
    Focused.reset;
  finally
    EndUpdate;
  end;
end;

{**********************************************************}
procedure TALDynamicListBoxButton.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{**************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{************************************************************************************************}
function TALDynamicListBoxButton.TStateStyles.GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle;
begin
  if Not Parent.Enabled then Result := Disabled
  else if Parent.Pressed then Result := Pressed
  //else if Parent.IsFocused then Result := Focused
  else if Parent.IsMouseOver then Result := Hovered
  else result := nil;
end;

{*******************************************************************************}
function TALDynamicListBoxButton.TStateStyles.GetParent: TALDynamicListBoxButton;
begin
  Result := TALDynamicListBoxButton(inherited Parent);
end;

{********************************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{******************************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{******************************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{******************************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{*******************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************}
procedure TALDynamicListBoxButton.TStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************}
function TALDynamicListBoxButton.TPadding.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(12{Left}, 6{Top}, 12{Right}, 6{Bottom});
end;

{****************************************************************}
constructor TALDynamicListBoxButton.Create(const AOwner: TObject);
begin
  {$IF defined(ALDPK)}
  FPrevStateStyles := nil;
  {$ENDIF}
  FStateStyles := nil;
  //--
  inherited Create(AOwner);
  //--
  //CanFocus := True;
  HitTest := True;
  AutoSize := True;
  Cursor := crHandPoint;
  //--
  //var LPaddingChange: TNotifyEvent := Padding.OnChange;
  //Padding.OnChange := nil;
  //Padding.DefaultValue := TRectF.create(12{Left}, 6{Top}, 12{Right}, 6{Bottom});
  //Padding.Rect := Padding.DefaultValue;
  //padding.OnChange := LPaddingChange;
  //--
  {$IF defined(ALDPK)}
  FPrevStateStyles := TStateStyles.Create(nil);
  {$ENDIF}
  //--
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{********************************************************}
function TALDynamicListBoxButton.CreatePadding: TALBounds;
begin
  Result := TPadding.Create;
end;

{*****************************************}
destructor TALDynamicListBoxButton.Destroy;
begin
  {$IF defined(ALDPK)}
  ALFreeAndNil(FPrevStateStyles);
  {$ENDIF}
  ALFreeAndNil(FStateStyles);
  inherited Destroy;
end;

{*****************************************}
//procedure TALDynamicListBoxButton.Loaded;
//
//  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
//  procedure _ConvertFontFamily(const AStateStyle: TBaseStateStyle);
//  begin
//    if (AStateStyle.TextSettings.Font.Family <> '') and
//       (not (csDesigning in ComponentState)) then
//      AStateStyle.TextSettings.Font.Family := ALConvertFontFamily(AStateStyle.TextSettings.Font.Family);
//  end;
//
//begin
//  _ConvertFontFamily(StateStyles.Disabled);
//  _ConvertFontFamily(StateStyles.Hovered);
//  _ConvertFontFamily(StateStyles.Pressed);
//  _ConvertFontFamily(StateStyles.Focused);
//  inherited Loaded;
//end;

{*********************************************}
procedure TALDynamicListBoxButton.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{****************************************************}
function TALDynamicListBoxButton.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{************************************************************}
function TALDynamicListBoxButton.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{***********************************************************************}
function TALDynamicListBoxButton.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TTextSettings.Create;
end;

{***************************************************************}
function TALDynamicListBoxButton.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{**************************************************************}
function TALDynamicListBoxButton.GetTextSettings: TTextSettings;
begin
  Result := TTextSettings(Inherited TextSettings);
end;

{****************************************************************************}
procedure TALDynamicListBoxButton.SetTextSettings(const Value: TTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{***********************************************************************}
//procedure TALDynamicListBoxButton.SetName(const Value: TComponentName);
//begin
//  var LChangeText := not (csLoading in ComponentState) and (Name = Text) and
//    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
//  inherited SetName(Value);
//  if LChangeText then
//    Text := Value;
//end;

{***************************************************************************}
procedure TALDynamicListBoxButton.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{*********************************************************************}
procedure TALDynamicListBoxButton.TextSettingsChanged(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin

    if //(not (csLoading in ComponentState)) and
       (not AToStateStyle.TextSettings.inherit) then begin

      if APrevStateStyle.TextSettings.font.Family = AToStateStyle.TextSettings.font.Family then AToStateStyle.TextSettings.font.Family := TextSettings.font.Family;
      if SameValue(APrevStateStyle.TextSettings.font.Size, AToStateStyle.TextSettings.font.Size, TEpsilon.fontSize) then AToStateStyle.TextSettings.font.Size := TextSettings.font.Size;
      if APrevStateStyle.TextSettings.font.Weight = AToStateStyle.TextSettings.font.Weight then AToStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
      if APrevStateStyle.TextSettings.font.Slant = AToStateStyle.TextSettings.font.Slant then AToStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
      if APrevStateStyle.TextSettings.font.Stretch = AToStateStyle.TextSettings.font.Stretch then AToStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
      if APrevStateStyle.TextSettings.font.Color = AToStateStyle.TextSettings.font.Color then AToStateStyle.TextSettings.font.Color := TextSettings.font.Color;

      if APrevStateStyle.TextSettings.Decoration.Kinds = AToStateStyle.TextSettings.Decoration.Kinds then AToStateStyle.TextSettings.Decoration.Kinds := TextSettings.Decoration.Kinds;
      if APrevStateStyle.TextSettings.Decoration.Style = AToStateStyle.TextSettings.Decoration.Style then AToStateStyle.TextSettings.Decoration.Style := TextSettings.Decoration.Style;
      if SameValue(APrevStateStyle.TextSettings.Decoration.ThicknessMultiplier, AToStateStyle.TextSettings.Decoration.ThicknessMultiplier, TEpsilon.Scale) then AToStateStyle.TextSettings.Decoration.ThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
      if APrevStateStyle.TextSettings.Decoration.Color = AToStateStyle.TextSettings.Decoration.Color then AToStateStyle.TextSettings.Decoration.Color := TextSettings.Decoration.Color;

    end;

    APrevStateStyle.TextSettings.font.Family := TextSettings.font.Family;
    APrevStateStyle.TextSettings.font.Size := TextSettings.font.Size;
    APrevStateStyle.TextSettings.font.Weight := TextSettings.font.Weight;
    APrevStateStyle.TextSettings.font.Slant := TextSettings.font.Slant;
    APrevStateStyle.TextSettings.font.Stretch := TextSettings.font.Stretch;
    APrevStateStyle.TextSettings.font.Color := TextSettings.font.Color;

    APrevStateStyle.TextSettings.Decoration.Kinds := TextSettings.Decoration.Kinds;
    APrevStateStyle.TextSettings.Decoration.Style := TextSettings.Decoration.Style;
    APrevStateStyle.TextSettings.Decoration.ThicknessMultiplier := TextSettings.Decoration.ThicknessMultiplier;
    APrevStateStyle.TextSettings.Decoration.Color := TextSettings.Decoration.Color;

  end;
  {$ENDIF}

begin
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
  inherited;
end;

{****************************************************************}
procedure TALDynamicListBoxButton.SetXRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if //(not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.XRadius, AToStateStyle.StateLayer.XRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.XRadius := XRadius;
    end;
    APrevStateStyle.StateLayer.XRadius := XRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{****************************************************************}
procedure TALDynamicListBoxButton.SetYRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if //(not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.YRadius, AToStateStyle.StateLayer.YRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.YRadius := YRadius;
    end;
    APrevStateStyle.StateLayer.YRadius := YRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{********************************************************************}
procedure TALDynamicListBoxButton.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{***************************************************}
procedure TALDynamicListBoxButton.IsMouseOverChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{***************************************************}
//procedure TALDynamicListBoxButton.IsFocusedChanged;
//begin
//  inherited;
//  StateStyles.startTransition;
//  repaint;
//end;

{***********************************************}
procedure TALDynamicListBoxButton.PressedChanged;
begin
  inherited;
  StateStyles.startTransition;
  repaint;
end;

{**************************************}
procedure TALDynamicListBoxButton.Click;
begin
  if StateStyles.IsTransitionAnimationRunning and StateStyles.Transition.DelayClick then
    StateStyles.TransitionClickDelayed := True
  else
    inherited click;
end;

{*************************************************}
procedure TALDynamicListBoxButton.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not IsDestroying) and
     (ALIsDrawableNull(FBufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(FStateStyles.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited ClearBufDrawable;
end;

{************************************************}
procedure TALDynamicListBoxButton.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then exit;
  if (CacheIndex > 0) and
     (CacheEngine <> nil) and
     (CacheEngine.HasEntry(CacheIndex{AIndex}, GetCacheSubIndex+LStateStyle.CacheSubIndex{ASubIndex})) then Exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + LStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
    {$endif}

    // Create the BufDrawable
    var LTextBroken: Boolean;
    var LAllTextDrawn: Boolean;
    var LElements: TALTextElements;
    CreateBufDrawable(
      LStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
      LTextBroken, // var ABufTextBroken: Boolean;
      LAllTextDrawn, // var ABufAllTextDrawn: Boolean;
      LElements, // var ABufElements: TALTextElements;
      ALGetScreenScale * LStateStyle.Scale, // const AScale: Single;
      LStateStyle.Text, // const AText: String;
      LStateStyle.TextSettings.Font, // const AFont: TALFont;
      LStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
      LStateStyle.TextSettings.Font, // const AEllipsisFont: TALFont;
      LStateStyle.TextSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
      LStateStyle.Fill, // const AFill: TALBrush;
      LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
      LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
      LStateStyle.Shadow); // const AShadow: TALShadow);

    // LStateStyle.FBufDrawableRect must include the LScale
    LStateStyle.FBufDrawableRect.Top := LStateStyle.FBufDrawableRect.Top * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.right := LStateStyle.FBufDrawableRect.right * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.left := LStateStyle.FBufDrawableRect.left * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.bottom := LStateStyle.FBufDrawableRect.bottom * LStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect: TRectF;
    if (CacheIndex <= 0) or
       (CacheEngine = nil) or
       (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LMainDrawableRect{ARect})) then begin
      If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect :=LocalRect.ReducePrecision
      else LMainDrawableRect := FBufDrawableRect;
    end;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LStateStyle.FBufDrawableRect.CenterAt(LMainDrawableRect);
    LStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{****************************************************************************************************************************************************************************}
Procedure TALDynamicListBoxButton.DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF);
begin

  // If we are drawing directly on the form, center ARect in LocalRect. This is necessary if, for example,
  // the 'to' font size is smaller than the 'from' font size.
  {$IF defined(ALSkiaCanvas)}
  If (Canvas <> nil) and (TSkCanvasCustom(Canvas).Canvas <> nil) and (TSkCanvasCustom(Canvas).Canvas.Handle = ACanvas) then
    // ALAlignToPixelRound is used because when we call ALDrawDrawable,
    // we do LDstRect := AALAlignToPixelRound(LDstRect).
    // Therefore, when drawing directly on the canvas,
    // we must draw at the exact same position as when we call ALDrawDrawable.
    ARect := ALAlignToPixelRound(ARect.CenterAt(LocalRect.ReducePrecision), Canvas.Matrix, Canvas.Scale, TEpsilon.position)
  else
  {$ENDIF}

end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALDynamicListBoxButton.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.IsTransitionAnimationRunning then begin
    Result := ARect;
    if StateStyles.TransitionFrom <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.TransitionTo <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{**************************************}
procedure TALDynamicListBoxButton.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;

  var LDrawable: TALDrawable := ALNullDrawable;
  var LDrawableRect: TRectF := TRectF.Empty;
  if not StateStyles.IsTransitionAnimationRunning then begin
    //--
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex+LStateStyle.CacheSubIndex{ASubIndex}, LStateStyle.FBufDrawable{ADrawable}, LStateStyle.FBufDrawableRect{ARect}) then ALFreeAndNilDrawable(LStateStyle.FBufDrawable)
          else LStateStyle.FBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex+LStateStyle.CacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := LStateStyle.FBufDrawable;
          LDrawableRect := LStateStyle.FBufDrawableRect;
        end;
      end;
    end;
    //--
    If ALIsDrawableNull(LDrawable) then begin
      if (CacheIndex <= 0) or
         (CacheEngine = nil) or
         (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect})) then begin
        if LStateStyle = nil then MakeBufDrawable;
        if (CacheIndex > 0) and (CacheEngine <> nil) and (not ALIsDrawableNull(fBufDrawable)) then begin
          if not CacheEngine.TrySetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, fBufDrawable{ADrawable}, fBufDrawableRect{ARect}) then ALFreeAndNilDrawable(fBufDrawable)
          else fBufDrawable := ALNullDrawable;
          if not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LDrawable{ADrawable}, LDrawableRect{ARect}) then
            raise Exception.Create('Error BB5ACD27-7CF2-44D3-AEB1-22C8BB492762');
        end
        else begin
          LDrawable := FBufDrawable;
          LDrawableRect := FBufDrawableRect;
        end;
      end;
    end;
    //--
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    // Using a matrix on the canvas results in smoother animations compared to using
    // Ascale with DrawMultilineText. This is because changes in scale affect the font size,
    // leading to rounding issues (I spent many hours looking for a way to avoid this).
    // If there is an animation, it appears jerky because the text position
    // shifts up or down with scale changes due to pixel alignment.
    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect.ReducePrecision, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      var LRect := LocalRect.ReducePrecision;
      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;
      DrawMultilineText(
        TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
        LRect, // var ARect: TRectF;
        LTextBroken, // out ATextBroken: Boolean;
        LAllTextDrawn, // out AAllTextDrawn: Boolean;
        LElements, // out AElements: TALTextElements;
        1{Ascale},
        AbsoluteOpacity, // const AOpacity: Single;
        LCurrentAdjustedStateStyle.Text, // const AText: String;
        LCurrentAdjustedStateStyle.TextSettings.Font, // const AFont: TALFont;
        LCurrentAdjustedStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
        LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
        LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
        LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
        LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        LCurrentAdjustedStateStyle.Shadow); // const AShadow: TALShadow);

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect.ReducePrecision;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;
      DrawMultilineText(
        RenderTargetCanvas, // const ACanvas: TALCanvas;
        LRect, // out ARect: TRectF;
        LTextBroken, // out ATextBroken: Boolean;
        LAllTextDrawn, // out AAllTextDrawn: Boolean;
        LElements, // out AElements: TALTextElements;
        ALGetScreenScale{Ascale},
        1, // const AOpacity: Single;
        LCurrentAdjustedStateStyle.Text, // const AText: String;
        LCurrentAdjustedStateStyle.TextSettings.Font, // const AFont: TALFont;
        LCurrentAdjustedStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
        LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
        LCurrentAdjustedStateStyle.TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
        LCurrentAdjustedStateStyle.Fill, // const AFill: TALBrush;
        LCurrentAdjustedStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
        LCurrentAdjustedStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        LCurrentAdjustedStateStyle.Shadow); // const AShadow: TALShadow);

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect: TRectF;
    if (CacheIndex <= 0) or
       (CacheEngine = nil) or
       (not CacheEngine.TryGetEntry(CacheIndex{AIndex}, GetCacheSubIndex{ASubIndex}, LMainDrawableRect{ARect})) then begin
      If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect :=LocalRect.ReducePrecision
      else LMainDrawableRect := FBufDrawableRect;
    end;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
    LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{********************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $ffd5d5d5;
end;

{************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := $ffd5d5d5;
end;

{*************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TBaseStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{*************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TBaseStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{******************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*******************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{****************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*************************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{********************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**********************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{***********************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{***********************************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TThumb.TStateStyles.Create(const AParent: TALDynamicListBoxControl);
begin
  inherited Create(AParent);
  //--
  FDisabled := CreateDisabledStateStyle(AParent);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := CreateHoveredStateStyle(AParent);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := CreatePressedStateStyle(AParent);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := CreateFocusedStateStyle(AParent);
  FFocused.OnChanged := FocusedChanged;
end;

{******************************************************************}
destructor TALDynamicListBoxCustomTrack.TThumb.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{******************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TStateStyles.CreateHoveredStateStyle(const AParent: TObject): THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TStateStyles.CreatePressedStateStyle(const AParent: TObject): TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{****************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TStateStyles.CreateFocusedStateStyle(const AParent: TObject): TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{*************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TStateStyles(Source).Disabled);
      Hovered.Assign(TStateStyles(Source).Hovered);
      Pressed.Assign(TStateStyles(Source).Pressed);
      Focused.Assign(TStateStyles(Source).Focused);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{***************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.reset;
    Hovered.reset;
    Pressed.reset;
    Focused.reset;
  finally
    EndUpdate;
  end;
end;

{**********************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.AlignToPixel;
    Hovered.AlignToPixel;
    Pressed.AlignToPixel;
    Focused.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{**************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Disabled.ClearBufDrawable;
  Hovered.ClearBufDrawable;
  Pressed.ClearBufDrawable;
  Focused.ClearBufDrawable;
end;

{************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TStateStyles.GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle;
begin
  if Not Parent.Enabled then Result := Disabled
  else if Parent.Pressed then Result := Pressed
  //else if Parent.IsFocused then Result := Focused
  else if Parent.IsMouseOver then Result := Hovered
  else result := nil;
end;

{*******************************************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.TStateStyles.GetParent: TALDynamicListBoxCustomTrack.TThumb;
begin
  Result := TALDynamicListBoxCustomTrack.TThumb(inherited Parent);
end;

{********************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{******************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{******************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{******************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.TStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{*******************************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TThumb.Create(const ACustomTrack: TALDynamicListBoxCustomTrack);
begin
  {$IF defined(ALDPK)}
  FPrevStateStyles := nil;
  {$ENDIF}
  FStateStyles := nil;
  //--
  inherited create(ACustomTrack);
  FCustomTrack := ACustomTrack;
  //--
  //CanFocus := ACustomTrack.CanFocus;
  //TabStop := ACustomTrack.TabStop;
  cursor := crHandPoint;
  AutoCapture := True;
  //Locked := True;
  //--
  FValueRange := TValueRange.create(_ALDummyComponent);
  {$IFDEF debug}
  if (FValueRange.Min <> 0) or
     (FValueRange.Max <> FMX.StdActns.DefaultMaxValue) or
     (FValueRange.Value <> 0) or
     (FValueRange.ViewportSize <> 0) or
     (FValueRange.Frequency <> 0) then
    Raise Exception.Create('Error 577E0A1F-9305-475A-AD94-AE60E257C8D2');
  {$ENDIF}
  FValueRange.onchanged := ValueRangeChanged;
  //--
  fCustomTrackMouseDownPos := TpointF.Zero;
  fScrollCapturedByMe := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  //--
  {$IF defined(ALDPK)}
  FPrevStateStyles := TStateStyles.Create(nil);
  {$ENDIF}
  //--
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{*****************************************************}
destructor TALDynamicListBoxCustomTrack.TThumb.Destroy;
begin
  {$IF defined(ALDPK)}
  ALFreeAndNil(FPrevStateStyles);
  {$ENDIF}
  ALFreeAndNil(FStateStyles);
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  ALFreeAndNil(FValueRange);
  inherited;
end;

{************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{***************************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{**********************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.DoBeginUpdate;
begin
  fValueRange.BeginUpdate;
  inherited;
end;

{********************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.DoEndUpdate;
begin
  fValueRange.EndUpdate;
  inherited;
end;

{*********************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.AlignToPixel;
begin
  BeginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.GetValue: Double;
begin
  Result := FValueRange.Value;
end;

{***************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{*********************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.GetDefaultXRadius: Single;
begin
  result := -50;
end;

{*********************************************************************}
function TALDynamicListBoxCustomTrack.TThumb.GetDefaultYRadius: Single;
begin
  result := -50;
end;

{****************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.SetXRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if //(not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.XRadius, AToStateStyle.StateLayer.XRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.XRadius := XRadius;
    end;
    APrevStateStyle.StateLayer.XRadius := XRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{****************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.SetYRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if //(not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.YRadius, AToStateStyle.StateLayer.YRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.YRadius := YRadius;
    end;
    APrevStateStyle.StateLayer.YRadius := YRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
    _PropagateChanges(FPrevStateStyles.Hovered, StateStyles.Hovered);
    _PropagateChanges(FPrevStateStyles.Pressed, StateStyles.Pressed);
    _PropagateChanges(FPrevStateStyles.Focused, StateStyles.Focused);
  end;
  {$ENDIF}
end;

{********************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{***************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.IsMouseOverChanged;
begin
  inherited;
  StateStyles.startTransition;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  repaint;
end;

{***************************************************************}
//procedure TALDynamicListBoxCustomTrack.TThumb.IsFocusedChanged;
//begin
//  inherited;
//  StateStyles.startTransition;
//  if FcustomTrack.FValueIndicator <> nil then
//    FcustomTrack.FValueIndicator.Refresh(Self);
//  repaint;
//end;

{***********************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.PressedChanged;
begin
  inherited;
  StateStyles.startTransition;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  repaint;
end;

{*******************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.ValueRangeChanged(Sender: TObject);
begin
  FcustomTrack.Realign;
  if FcustomTrack.FValueIndicator <> nil then
    FcustomTrack.FValueIndicator.Refresh(Self);
  FcustomTrack.DoChanged;
end;

{***********************************************************************************************************************}
//procedure TALDynamicListBoxCustomTrack.TThumb.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
//begin
//  var LInc: Double := fValueRange.Frequency;
//  if LInc = 0 then LInc := 1;
//  inherited;
//  var LValue: Double;
//  case Key of
//    vkHome: LValue := fValueRange.Min;
//    vkEnd: LValue := fValueRange.Max;
//    vkUp: LValue := fValueRange.Value - LInc;
//    vkDown: LValue := fValueRange.Value + LInc;
//    vkLeft: LValue := fValueRange.Value - LInc;
//    vkRight: LValue := fValueRange.Value + LInc;
//    else Exit;
//  end;
//  Key := 0;
//  fValueRange.Value := LValue;
//end;

{*******************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALDynamicListBoxCustomTrack.TThumb.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'Pressed: ' + ALBoolToStrW(Pressed));
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    {$IFDEF DEBUG}
    if fScrollCapturedByMe then
      raise Exception.Create('Error 40ED19CA-9F47-4A56-AC46-FA5D8D5429C0');
    {$ENDIF}
    Pressed := False;
  end;
end;

{**************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Pressed then begin
    BringToFront;
    fCustomTrackMouseDownPos := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(PressedPosition)).ReducePrecision;
  end;
end;

{****************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.MouseMove(Shift: TShiftState; X, Y: Single);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _PosToValue(MinValue, MaxValue, ViewportSize: Double; TrackSize, Pos: Single): Double;
  begin
    Result := MinValue;
    if ViewportSize < 0 then ViewportSize := 0;
    var LValRel: Double := TrackSize;
    if LValRel > 0 then begin
      LValRel := Pos / LValRel;
      if LValRel < 0 then LValRel := 0;
      if LValRel > 1 then LValRel := 1;
      Result := MinValue + LValRel * (MaxValue - MinValue - ViewportSize);
    end;
  end;

begin
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALDynamicListBoxCustomTrack.MouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if Pressed then begin

    if (not fScrollCapturedByMe) then begin
      var LCustomTrackMousePos := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(TpointF.Create(X,Y))).ReducePrecision;
      If ((FCustomtrack.Orientation = TOrientation.Horizontal) and
          (abs(FCustomTrackMouseDownPos.x - LCustomTrackMousePos.x) > abs(FCustomTrackMouseDownPos.y - LCustomTrackMousePos.y)) and
          (abs(FCustomTrackMouseDownPos.x - LCustomTrackMousePos.x) > TALScrollEngine.DefaultTouchSlop))
         or
         ((FCustomtrack.Orientation = TOrientation.Vertical) and
          (abs(FCustomTrackMouseDownPos.y - LCustomTrackMousePos.y) > abs(FCustomTrackMouseDownPos.x - LCustomTrackMousePos.x)) and
          (abs(FCustomTrackMouseDownPos.y - LCustomTrackMousePos.y) > TALScrollEngine.DefaultTouchSlop)) then begin
        {$IFDEF DEBUG}
        //ALLog(
        //  'TALDynamicListBoxCustomTrack.MouseMove',
        //  'ScrollCapturedByMe');
        {$ENDIF}
        PressedPosition := PointF(X, Y);
        fCustomTrackMouseDownPos := LCustomTrackMousePos;
        fScrollCapturedByMe := true;
        TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(true), True);
      end;
    end;

    if fScrollCapturedByMe then begin
      if FCustomTrack.Orientation = TOrientation.Horizontal then begin
        var P := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(X - PressedPosition.X, 0)));
        FValueRange.Value := _PosToValue(
                               FCustomTrack.Min, // MinValue
                               FCustomTrack.Max, // MaxValue
                               FCustomTrack.ViewportSize, // ViewportSize
                               FCustomTrack.GetTrackSize, // TrackSize
                               P.X - FCustomTrack.padding.Left - FCustomTrack.GetLeadingTrackStartPadding); // Pos
      end
      else begin
        var P := FCustomTrack.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(0, Y - PressedPosition.Y)));
        FValueRange.Value := _PosToValue(
                               FCustomTrack.Min, // MinValue
                               FCustomTrack.Max, // MaxValue
                               FCustomTrack.ViewportSize, // ViewportSize
                               FCustomTrack.GetTrackSize, // TrackSize
                               P.Y - FCustomTrack.padding.Top - FCustomTrack.GetLeadingTrackStartPadding); // Pos
      end;
    end;

  end;
  inherited;
end;

{************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FScrollCapturedByMe then
    FScrollCapturedByMe := False;
end;

{*******************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.MouseLeave;
begin
  inherited;
  if FScrollCapturedByMe then
    FScrollCapturedByMe := False;
end;

{*************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.ClearBufDrawable;
begin
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not IsDestroying) and
     (ALIsDrawableNull(FBufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(FStateStyles.Disabled.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Hovered.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Pressed.FBufDrawable)) or
      (not ALIsDrawableNull(FStateStyles.Focused.FBufDrawable))) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited ClearBufDrawable;
end;

{************************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + LStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
    {$endif}

    // Create the BufDrawable
    CreateBufDrawable(
      LStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
      ALGetScreenScale * LStateStyle.Scale, // const AScale: Single;
      LStateStyle.Fill, // const AFill: TALBrush;
      LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
      TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
      False, // const ADrawStateLayerOnTop: Boolean;
      LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
      LStateStyle.Shadow); // const AShadow: TALShadow);

    // LStateStyle.FBufDrawableRect must include the LStateStyle.Scale
    LStateStyle.FBufDrawableRect.Top := LStateStyle.FBufDrawableRect.Top * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.right := LStateStyle.FBufDrawableRect.right * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.left := LStateStyle.FBufDrawableRect.left * LStateStyle.Scale;
    LStateStyle.FBufDrawableRect.bottom := LStateStyle.FBufDrawableRect.bottom * LStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to scale), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect: TRectF;
    If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect :=LocalRect.ReducePrecision
    else LMainDrawableRect := FBufDrawableRect;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LStateStyle.FBufDrawableRect.CenterAt(LMainDrawableRect);
    LStateStyle.FBufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);

  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{*****************************}
{$IF NOT DEFINED(ALSkiaCanvas)}
function TALDynamicListBoxCustomTrack.TThumb.GetRenderTargetRect(const ARect: TrectF): TRectF;
begin
  if StateStyles.IsTransitionAnimationRunning then begin
    Result := ARect;
    if StateStyles.TransitionFrom <> nil then begin
      var LFromSurfaceRect := ALGetShapeSurfaceRect(
                                ARect, // const ARect: TRectF;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Fill, // const AFill: TALBrush;
                                nil, // const AFillResourceStream: TStream;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).StateLayer, // const AStateLayer: TALStateLayer;
                                _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionFrom).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LFromSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
    if StateStyles.TransitionTo <> nil then begin
      var LToSurfaceRect := ALGetShapeSurfaceRect(
                              ARect, // const ARect: TRectF;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).Fill, // const AFill: TALBrush;
                              nil, // const AFillResourceStream: TStream;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).StateLayer, // const AStateLayer: TALStateLayer;
                              _TALDynamicListBoxBaseStateStyleAccessProtected(StateStyles.TransitionTo).Shadow); // const AShadow: TALShadow): TRectF;
      Result := TRectF.Union(Result, LToSurfaceRect); // add the extra space needed to draw the shadow/statelayer
    end;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  LStateStyle.Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
                  LStateStyle.Shadow); // const AShadow: TALShadow): TRectF;
    end
    else begin
      Result := ALGetShapeSurfaceRect(
                  ARect, // const ARect: TRectF;
                  Fill, // const AFill: TALBrush;
                  nil, // const AFillResourceStream: TStream;
                  nil, // const AStateLayer: TALStateLayer;
                  Shadow); // const AShadow: TALShadow): TRectF;
    end;
  end;
end;
{$ENDIF}

{**************************************************}
procedure TALDynamicListBoxCustomTrack.TThumb.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  if StateStyles.IsTransitionAnimationRunning then begin
    LDrawable := ALNullDrawable;
    LDrawableRect := TRectF.Empty;
  end
  else begin
    var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
    if LStateStyle <> nil then begin
      LDrawable := LStateStyle.FBufDrawable;
      LDrawableRect := LStateStyle.FBufDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := FBufDrawable;
        LDrawableRect := FBufDrawableRect;
      end;
    end
    else begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    // Using a matrix on the canvas results in smoother animations compared to using
    // Ascale with DrawMultilineText. This is because changes in scale affect the font size,
    // leading to rounding issues (I spent many hours looking for a way to avoid this).
    // If there is an animation, it appears jerky because the text position
    // shifts up or down with scale changes due to pixel alignment.
    var LCanvasSaveState: TCanvasSaveState := ALScaleAndCenterCanvas(
                                                Canvas, // Const ACanvas: TCanvas;
                                                AbsoluteRect.ReducePrecision, // Const AAbsoluteRect: TRectF;
                                                LCurrentAdjustedStateStyle.Scale, // Const AScale: Single;
                                                true); // Const ASaveState: Boolean);
    try

      TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LocalRect.ReducePrecision)
        .SetOpacity(AbsoluteOpacity)
        .SetFill(LCurrentAdjustedStateStyle.Fill)
        .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TalphaColors.Null)
        .SetStroke(LCurrentAdjustedStateStyle.Stroke)
        .SetShadow(LCurrentAdjustedStateStyle.Shadow)
        .SetSides(Sides)
        .SetCorners(Corners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      if LCanvasSaveState <> nil then
        Canvas.RestoreState(LCanvasSaveState);
    end;

    {$ELSE}

    var LRect := LocalRect.ReducePrecision;
    InitRenderTargets(LRect);
    if ALCanvasBeginScene(RenderTargetCanvas) then
    try

      ALClearCanvas(RenderTargetCanvas, TAlphaColors.Null);

      TALDrawRectangleHelper.Create(RenderTargetCanvas)
        .SetScale(ALGetScreenScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(LRect)
        .SetOpacity(AbsoluteOpacity)
        .SetFill(LCurrentAdjustedStateStyle.Fill)
        .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TalphaColors.Null)
        .SetStroke(LCurrentAdjustedStateStyle.Stroke)
        .SetShadow(LCurrentAdjustedStateStyle.Shadow)
        .SetSides(Sides)
        .SetCorners(Corners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

    finally
      ALCanvasEndScene(RenderTargetCanvas)
    end;

    ALUpdateDrawableFromSurface(RenderTargetSurface, RenderTargetDrawable);

    // The Shadow or Statelayer are not included in the dimensions of the LRect rectangle.
    // However, the LRect rectangle is offset by the dimensions of the shadow/Statelayer.
    LRect.Offset(-2*LRect.Left, -2*LRect.Top);

    // LRect must include the LScale
    LRect.Top := LRect.Top * LCurrentAdjustedStateStyle.Scale;
    LRect.right := LRect.right * LCurrentAdjustedStateStyle.Scale;
    LRect.left := LRect.left * LCurrentAdjustedStateStyle.Scale;
    LRect.bottom := LRect.bottom * LCurrentAdjustedStateStyle.Scale;

    // Since LStateStyle.FBufDrawableRect can have different dimensions than the main BufDrawableRect
    // (due to autosizing with different font sizes), we must center LStateStyle.FBufDrawableRect
    // within the main BufDrawableRect to ensure that all changes are visually centered.
    var LMainDrawableRect: TRectF;
    If AlIsDrawableNull(FBufDrawable) then LMainDrawableRect :=LocalRect.ReducePrecision
    else LMainDrawableRect := FBufDrawableRect;
    LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
    var LCenteredRect := LRect.CenterAt(LMainDrawableRect);
    LRect.Offset(LCenteredRect.Left, LCenteredRect.top);

    // We cannot use the matrix because, if we do, ALAlignToPixelRound in ALDrawDrawable
    // will be ineffective since the matrix will no longer be a simple translation matrix.
    // In such a case, TCustomCanvasGpu(ACanvas).DrawTexture may produce border artifacts
    // if the texture is not perfectly pixel-aligned.
    var LDstRect := TRectF.Create(0, 0, ALGetDrawableWidth(RenderTargetDrawable), ALGetDrawableHeight(RenderTargetDrawable));
    LDstRect.Width := (LDstRect.Width / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.height := (LDstRect.height / ALGetScreenScale) * LCurrentAdjustedStateStyle.Scale;
    LDstRect.SetLocation(
      LRect.Left,
      LRect.Top);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      RenderTargetDrawable, // const ADrawable: TALDrawable;
      LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single)

    {$ENDIF}

    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphacolors.Black;
end;

{******************************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.TTextSettings.TFont.GetDefaultWeight: TFontWeight;
begin
  Result := TFontWeight.medium;
end;

{*****************************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.TTextSettings.TFont.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.White;
end;

{**************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.TTextSettings.CreateFont: TALFont;
begin
  Result := TFont.Create;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.TTextSettings.GetDefaultHorzAlign: TALTextHorzAlign;
begin
  Result := TALTextHorzAlign.center;
end;

{*************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(6{Left}, 4{Top}, 6{Right}, 4{Bottom});
end;

{*************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.TPadding.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
end;

{****************************************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TValueIndicator.Create(const ACustomTrack: TALDynamicListBoxCustomTrack);
begin
  inherited create(ACustomTrack);
  AutoSize := True;
  Visible := False;
  Pivot := TPointF.Create(0.5,1);
  FCustomTrack := ACustomTrack;
  FFormat := DefaultFormat;
  FOnCustomFormat := nil;
  FAnimation := TAnimation.ScaleInOut;
  FShowOnInteraction := False;
  //--
  FFloatAnimation := TALFloatAnimation.Create;
  FFloatAnimation.StartValue := 0;
  FFloatAnimation.StopValue := 1;
  FFloatAnimation.Duration := 0.2;
  FFloatAnimation.AnimationType := TanimationType.out;
  FFloatAnimation.Interpolation := TALInterpolationType.cubic;
  FFloatAnimation.OnProcess := AnimationProcess;
  FFloatAnimation.OnFinish := AnimationFinish;
  //--
  //var LMarginsChange: TNotifyEvent := Margins.OnChange;
  //Margins.OnChange := nil;
  //Margins.DefaultValue := TRectF.create(6{Left}, 4{Top}, 6{Right}, 4{Bottom});
  //Margins.Rect := Margins.DefaultValue;
  //Margins.OnChange := LMarginsChange;
  //--
  //var LPaddingChange: TNotifyEvent := Padding.OnChange;
  //Padding.OnChange := nil;
  //Padding.DefaultValue := TRectF.create(16{Left}, 12{Top}, 16{Right}, 12{Bottom});
  //Padding.Rect := Padding.DefaultValue;
  //padding.OnChange := LPaddingChange;
end;

{*****************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.CreatePadding: TALBounds;
begin
  Result := TPadding.Create;
end;

{*****************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{**************************************************************}
destructor TALDynamicListBoxCustomTrack.TValueIndicator.Destroy;
begin
  ALFreeAndNil(FFloatAnimation);
  inherited;
end;

{******************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TValueIndicator.AdjustPosition(const AThumb: TThumb);
begin
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    SetPosition(
      TpointF.Create(
        AThumb.Left - ((Width - AThumb.Width) / 2),
        AThumb.Top - Height - Margins.Bottom));
  end
  else begin
    SetPosition(
      TpointF.Create(
        AThumb.Left + AThumb.Width + Margins.Left,
        AThumb.Top - ((Height - AThumb.Height) / 2)));
  end;
end;

{***********************************************************************************}
procedure TALDynamicListBoxCustomTrack.TValueIndicator.Refresh(const AThumb: TThumb);
begin
  if not FShowOnInteraction then exit;
  If {AThumb.IsFocused or} Athumb.IsMouseOver or AThumb.Pressed then begin
    if assigned(OnCustomFormat) then begin
      var LText: String;
      OnCustomFormat(Self, AThumb.GetValue, LText);
      Text := LText;
    end
    else
      Text := ALFormatFloatW(Format, AThumb.GetValue);
    if FFloatAnimation.TagObject <> AThumb then begin
      FFloatAnimation.Enabled := False;
      visible := False;
    end;
    FFloatAnimation.TagObject := AThumb;
    if not visible then begin
      case FAnimation of
        TAnimation.None: begin
          AdjustPosition(AThumb);
          Visible := True;
          exit;
        end;
        TAnimation.ScaleInOut: begin
          AdjustPosition(AThumb);
          Opacity := 1;
          Scale := 0;
        end;
        TAnimation.Opacity: begin
          AdjustPosition(AThumb);
          Opacity := 0;
          Scale := 1;
        end
        else
          Raise Exception.Create('Error A2F4F658-97FC-4F92-AFA4-3BB8192003A8')
      end;
      Visible := True;
      FFloatAnimation.StopAtCurrent;
      FFloatAnimation.Inverse := False;
      FFloatAnimation.Start;
    end
    else if (FFloatAnimation.Running) and
            (FFloatAnimation.Inverse) then begin
      FFloatAnimation.Inverse := False;
    end
    else begin
      AdjustPosition(AThumb);
    end;
  end
  else begin
    if FFloatAnimation.TagObject <> AThumb then begin
      FFloatAnimation.Enabled := False;
      visible := False;
    end;
    FFloatAnimation.TagObject := AThumb;
    If Visible then begin
      if (not FFloatAnimation.Running) then begin
        FFloatAnimation.Inverse := true;
        FFloatAnimation.Start;
      end
      else if (not FFloatAnimation.Inverse) then begin
        FFloatAnimation.Inverse := True;
      end;
    end;
  end;
end;

{***************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TValueIndicator.AnimationProcess(Sender: TObject);
begin
  case FAnimation of
    TAnimation.ScaleInOut: Scale := FFloatAnimation.CurrentValue;
    TAnimation.Opacity: Opacity := FFloatAnimation.CurrentValue
    else Raise Exception.Create('Error D6F17D76-E47E-4144-8FBA-5CAD3EBF84F3')
  end;
end;

{**************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TValueIndicator.AnimationFinish(Sender: TObject);
begin
  FFloatAnimation.Enabled := False;
  case FAnimation of
    TAnimation.ScaleInOut: begin
      if SameValue(Scale, 0, TEpsilon.Scale) then
        visible := False;
    end;
    TAnimation.Opacity: begin
      if SameValue(Opacity, 0, TEpsilon.Vector) then
        visible := False;
    end
    else
      Raise Exception.Create('Error D6F17D76-E47E-4144-8FBA-5CAD3EBF84F3')
  end;
end;

{*************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{********************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TTextSettings.Create;
end;

{*************************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.GetTextSettings: TALTextSettings;
begin
  Result := TALTextSettings(Inherited TextSettings);
end;

{***************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TValueIndicator.SetTextSettings(const Value: TALTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{******************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{******************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{*****************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.GetDefaultFormat: String;
begin
  Result := Format0;
end;

{************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TValueIndicator.SetFormat(const Value: string);
begin
  if FFormat <> Value then
  begin
    ClearBufDrawable;
    FFormat := Value;
  end;
end;

{****************************************************************************}
function TALDynamicListBoxCustomTrack.TValueIndicator.IsFormatStored: Boolean;
begin
  Result := FFormat <> DefaultFormat;
end;

{******************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffc5c5c5;
end;

{********************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*************************************************************************}
constructor TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.Create;
begin
  inherited Create;
  FColor := DefaultColor;
  FResourceName := DefaultResourceName;
  FWrapMode := DefaultWrapMode;
  FSize := DefaultSize;
end;

{********************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{**********************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultResourceName: String;
begin
  Result := '';
end;

{****************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultWrapMode: TALImageWrapMode;
begin
  Result := TALImageWrapMode.Fit;
end;

{**************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.GetDefaultSize: Single;
begin
  Result := 4;
end;

{********************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.Assign(Source: TPersistent);
begin
  if Source is TStopIndicatorBrush then begin
    BeginUpdate;
    Try
      Color := TStopIndicatorBrush(Source).Color;
      ResourceName := TStopIndicatorBrush(Source).ResourceName;
      WrapMode := TStopIndicatorBrush(Source).WrapMode;
      Size := TStopIndicatorBrush(Source).Size;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Color := DefaultColor;
    ResourceName := DefaultResourceName;
    WrapMode := DefaultWrapMode;
    Size := DefaultSize;
  finally
    EndUpdate;
  end;
end;

{*****************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.AlignToPixel;
begin
  BeginUpdate;
  try
    Size := ALAlignDimensionToPixelRound(Size, ALGetScreenScale, Tepsilon.Position);
  finally
    EndUpdate;
  end;
end;

{*******************************************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.Interpolate(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      ResourceName := ATo.ResourceName;
      WrapMode := ATo.WrapMode;
      Size := InterpolateSingle(Size{Start}, ATo.Size{Stop}, ANormalizedTime);
    end
    else begin
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      ResourceName := DefaultResourceName;
      WrapMode := DefaultWrapMode;
      Size := InterpolateSingle(Size{Start}, DefaultSize{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{****************************************************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.InterpolateNoChanges(const ATo: TStopIndicatorBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{*****************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.hasStopIndicator: Boolean;
begin
  Result := (CompareValue(FSize, 0, TEpsilon.Position) > 0) and
            ((Color <> TalphaColors.Null) or
             (ResourceName <> ''));
end;

{**************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{*********************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> DefaultResourceName;
end;

{*****************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> DefaultWrapMode;
end;

{*************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.IsSizeStored: Boolean;
begin
  result := not SameValue(fSize, DefaultSize, Tepsilon.Position);
end;

{***************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
  end;
end;

{*****************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.SetResourceName(const Value: String);
begin
  if fResourceName <> Value then begin
    fResourceName := Value;
    Change;
  end;
end;

{***********************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.SetWrapMode(const Value: TALImageWrapMode);
begin
  if fWrapMode <> Value then begin
    fWrapMode := Value;
    Change;
  end;
end;

{*********************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStopIndicatorBrush.SetSize(const Value: Single);
begin
  if not SameValue(FSize, Value, TEpsilon.Position) then begin
    FSize := Value;
    Change;
  end;
end;

{********************************************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TTrack.TInheritStopIndicatorBrush.Create(const AParent: TStopIndicatorBrush);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{**************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TInheritStopIndicatorBrush.CreateSavedState: TALPersistentObserver;
type
  TInheritStopIndicatorBrushClass = class of TInheritStopIndicatorBrush;
begin
  result := TInheritStopIndicatorBrushClass(classtype).Create(nil{AParent});
end;

{*********************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TInheritStopIndicatorBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{***************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TInheritStopIndicatorBrush.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TInheritStopIndicatorBrush then begin
      Inherit := TInheritStopIndicatorBrush(Source).Inherit;
      fSuperseded := TInheritStopIndicatorBrush(Source).fSuperseded;
    end
    else begin
      Inherit := False;
      fSuperseded := False;
    end;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*****************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TInheritStopIndicatorBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Inherit := True;
    fSuperseded := False;
  finally
    EndUpdate;
  end;
end;

{***********************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TInheritStopIndicatorBrush.DoSupersede;
begin
  Assign(FParent);
end;

{********************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TInheritStopIndicatorBrush.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  BeginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TInheritStopIndicatorBrush then begin
      TInheritStopIndicatorBrush(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TInheritStopIndicatorBrush(FParent).restoreStateNoChanges;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{*****************************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TInheritStopIndicatorBrush.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{**********************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ffc5c5c5;
end;

{************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TalphaColors.Null;
end;

{*************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{*************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{*********************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  if StateStyleParent <> nil then Raise Exception.create('Error 19ACDBF0-E33C-49D2-B199-5C232A0A71DB')
  else if ControlParent <> nil then FStopIndicator := CreateStopIndicator(ControlParent.StopIndicator)
  else FStopIndicator := CreateStopIndicator(nil);
  FStopIndicator.OnChanged := StopIndicatorChanged;
end;

{*********************************************************************}
destructor TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FStopIndicator);
  inherited Destroy;
end;

{****************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{******************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{*******************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{***********************************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.CreateStopIndicator(const AParent: TStopIndicatorBrush): TInheritStopIndicatorBrush;
begin
  Result := TInheritStopIndicatorBrush.Create(AParent);
end;

{****************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TBaseStateStyle then begin
    BeginUpdate;
    Try
      StopIndicator.Assign(TBaseStateStyle(Source).StopIndicator);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{******************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    StopIndicator.Reset;
  finally
    EndUpdate;
  end;
end;

{*************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.AlignToPixel;
begin
  BeginUpdate;
  try
    Inherited;
    StopIndicator.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{***************************************************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.Interpolate(const ATo: TALDynamicListBoxBaseStateStyle; const ANormalizedTime: Single);
begin
  {$IF defined(debug)}
  if (ATo <> nil) and (not (ATo is TBaseStateStyle)) then
    Raise Exception.Create('Error 70FA71DE-6270-441D-AB33-6F987A011C09');
  {$ENDIF}
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then StopIndicator.Interpolate(TBaseStateStyle(ATo).StopIndicator, ANormalizedTime)
    {$IF defined(debug)}
    else if StateStyleParent <> nil then Raise Exception.Create('Error 9B674B61-66C2-4BB1-8A94-D6A58AEAF404')
    {$ENDIF}
    else if ControlParent <> nil then StopIndicator.Interpolate(ControlParent.StopIndicator, ANormalizedTime)
    else StopIndicator.Interpolate(nil, ANormalizedTime);
  Finally
    EndUpdate;
  End;
end;

{************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.GetControlParent: TTrack;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TTrack)) then
    raise Exception.Create('ControlParent must be of type TTrack');
  {$ENDIF}
  result := TTrack(inherited ControlParent);
end;

{************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.DoSupersede;
begin
  inherited;
  StopIndicator.Supersede;
end;

{***********************************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.SetStopIndicator(const AValue: TInheritStopIndicatorBrush);
begin
  FStopIndicator.Assign(AValue);
end;

{*******************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            StopIndicator.Inherit;
end;

{***************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TBaseStateStyle.StopIndicatorChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*************************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TTrack.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{********************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TDisabledStateStyle then
      Opacity := TDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{**********************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{***********************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{***********************************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TTrack.TStateStyles.Create(const AParent: TALDynamicListBoxControl);
begin
  inherited Create(AParent);
  FDisabled := CreateDisabledStateStyle(AParent);
  FDisabled.OnChanged := DisabledChanged;
end;

{******************************************************************}
destructor TALDynamicListBoxCustomTrack.TTrack.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  inherited Destroy;
end;

{******************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{*************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TStateStyles(Source).Disabled);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{***************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.reset;
  finally
    EndUpdate;
  end;
end;

{**********************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStateStyles.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    Disabled.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{**************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStateStyles.ClearBufDrawable;
begin
  inherited;
  Disabled.ClearBufDrawable;
end;

{************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStateStyles.GetCurrentRawStyle: TALDynamicListBoxBaseStateStyle;
begin
  if Not Parent.Enabled then Result := Disabled
  else result := nil;
end;

{*******************************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TStateStyles.GetParent: TALDynamicListBoxCustomTrack.TTrack;
begin
  Result := TALDynamicListBoxCustomTrack.TTrack(inherited Parent);
end;

{********************************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.TStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(0{Left}, 15{Top}, 0{Right}, 15{Bottom});
end;

{*******************************************************************************************************}
constructor TALDynamicListBoxCustomTrack.TTrack.Create(const ACustomTrack: TALDynamicListBoxCustomTrack);
begin
  {$IF defined(ALDPK)}
  FPrevStateStyles := nil;
  {$ENDIF}
  FStateStyles := nil;
  //--
  inherited Create(ACustomTrack);
  FCustomTrack := ACustomTrack;
  FStopIndicator := CreateStopIndicator;
  FStopIndicator.OnChanged := StopIndicatorChanged;
  //Locked := True;
  HitTest := False;
  //--
  {$IF defined(ALDPK)}
  FPrevStateStyles := TStateStyles.Create(nil);
  {$ENDIF}
  //--
  FStateStyles := CreateStateStyles;
  FStateStyles.OnChanged := StateStylesChanged;
end;

{********************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{*****************************************************}
destructor TALDynamicListBoxCustomTrack.TTrack.Destroy;
begin
  {$IF defined(ALDPK)}
  ALFreeAndNil(FPrevStateStyles);
  {$ENDIF}
  ALFreeAndNil(FStateStyles);
  ALFreeAndNil(FStopIndicator);
  inherited;
end;

{****************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{************************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.CreateStopIndicator: TStopIndicatorBrush;
begin
  Result := TStopIndicatorBrush.Create;
end;

{***************************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.CreateStateStyles: TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{*********************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    StateStyles.AlignToPixel;
    StopIndicator.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{******************************************************************}
function TALDynamicListBoxCustomTrack.TTrack.HasCustomDraw: Boolean;
begin
  Result := StopIndicator.hasStopIndicator;
end;

{***************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{***********************************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.SetStopIndicator(const Value: TStopIndicatorBrush);
begin
  FStopIndicator.Assign(Value);
end;

{****************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.SetXRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if //(not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.XRadius, AToStateStyle.StateLayer.XRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.XRadius := XRadius;
    end;
    APrevStateStyle.StateLayer.XRadius := XRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
  end;
  {$ENDIF}
end;

{****************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.SetYRadius(const Value: Single);

  {~~~~~~~~~~~~~~~~~~}
  {$IF defined(ALDPK)}
  procedure _PropagateChanges(const APrevStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    if //(not (csLoading in ComponentState)) and
       (not AToStateStyle.StateLayer.HasFill) then begin
      if (SameValue(APrevStateStyle.StateLayer.YRadius, AToStateStyle.StateLayer.YRadius, TEpsilon.Vector)) then AToStateStyle.StateLayer.YRadius := YRadius;
    end;
    APrevStateStyle.StateLayer.YRadius := YRadius;
  end;
  {$ENDIF}

begin
  inherited;
  {$IF defined(ALDPK)}
  if (StateStyles <> nil) and (FPrevStateStyles <> nil) then begin
    _PropagateChanges(FPrevStateStyles.Disabled, StateStyles.Disabled);
  end;
  {$ENDIF}
end;

{********************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.StateStylesChanged(Sender: TObject);
begin
  ClearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{**********************************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.StopIndicatorChanged(Sender: TObject);
begin
  ClearBufDrawable;
  Repaint;
end;

{***********************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.PaddingChanged;
begin
  inherited;
  ClearBufDrawable;
  Repaint;
end;

{*************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.ClearBufDrawable;
begin
  if FcustomTrack.FIsAligningTracks then exit;
  {$IFDEF debug}
  if (FStateStyles <> nil) and
     (not IsDestroying) and
     (ALIsDrawableNull(FBufDrawable)) and // warn will be raise in inherited
     (not ALIsDrawableNull(FStateStyles.Disabled.FBufDrawable)) then
    ALLog(Classname + '.ClearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  if FStateStyles <> nil then
    FStateStyles.ClearBufDrawable;
  inherited ClearBufDrawable;
end;

{************************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.MakeBufDrawable;
begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    ClearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle = nil then exit;
  if LStateStyle.Inherit then exit;
  if (not ALIsDrawableNull(LStateStyle.FBufDrawable)) then exit;
  LStateStyle.SupersedeNoChanges(true{ASaveState});
  try

    {$IFDEF debug}
    ALLog(Classname + '.MakeBufDrawable', 'Name: ' + Name + ' | Style: ' + LStateStyle.ClassName + ' | Width: ' + ALFloatToStrW(Width, ALDefaultFormatSettingsW)+ ' | Height: ' + ALFloatToStrW(Height, ALDefaultFormatSettingsW));
    {$endif}

    // Create the BufDrawable
    CreateBufDrawable(
      LStateStyle.FBufDrawable, // var ABufDrawable: TALDrawable;
      LStateStyle.FBufDrawableRect, // var ABufDrawableRect: TRectF;
      ALGetScreenScale, // const AScale: Single;
      LStateStyle.Fill, // const AFill: TALBrush;
      LStateStyle.StateLayer, // const AStateLayer: TALStateLayer;
      TAlphaColors.null, // const AStateLayerContentColor: TAlphaColor;
      False, // const ADrawStateLayerOnTop: Boolean;
      LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
      LStateStyle.Shadow, // const AShadow: TALShadow
      LStateStyle.StopIndicator); // const AStopIndicator: TStopIndicator);

  finally
    LStateStyle.RestorestateNoChanges;
  end;
end;

{**************************************************************}
Procedure TALDynamicListBoxCustomTrack.TTrack.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow);
begin
  CreateBufDrawable(
    ABufDrawable, // var ABufDrawable: TALDrawable;
    ABufDrawableRect, // out ABufDrawableRect: TRectF;
    AScale, // const AScale: Single;
    AFill, // const AFill: TALBrush;
    AStateLayer, // const AStateLayer: TALStateLayer;
    AStateLayerContentColor, // const AStateLayerContentColor: TAlphaColor;
    ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
    AStroke, // const AStroke: TALStrokeBrush;
    AShadow, // const AShadow: TALShadow;
    StopIndicator); // const AStopIndicator: TStopIndicator);
end;

{**************************************************************}
Procedure TALDynamicListBoxCustomTrack.TTrack.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            out ABufDrawableRect: TRectF;
            const AScale: Single;
            const AFill: TALBrush;
            const AStateLayer: TALStateLayer;
            const AStateLayerContentColor: TAlphaColor;
            const ADrawStateLayerOnTop: Boolean;
            const AStroke: TALStrokeBrush;
            const AShadow: TALShadow;
            const AStopIndicator: TStopIndicatorBrush);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect.ReducePrecision;
  if FCustomTrack.Orientation = TOrientation.Horizontal then
    ABufDrawableRect.Width := FCustomTrack.GetTrackSize(true{AIncludeTrackPadding})
  else
    ABufDrawableRect.Height := FCustomTrack.GetTrackSize(true{AIncludeTrackPadding});
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AFill, // const AFill: TALBrush;
                        nil, // const AFillResourceStream: TStream;
                        AStateLayer, // const AStateLayer: TALStateLayer;
                        AShadow); // const AShadow: TALShadow): TRectF;
  ABufDrawableRect.Offset(-LSurfaceRect.Left, -LSurfaceRect.Top);

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    AScale, // const AScale: Single;
    LSurfaceRect.Width, // const w: integer;
    LSurfaceRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      TALDrawRectangleHelper.Create(LCanvas)
        .SetScale(AScale)
        .SetAlignToPixel(IsPixelAlignmentEnabled)
        .SetDstRect(ABufDrawableRect)
        .SetFill(AFill)
        .SetStateLayer(AStateLayer, AStateLayerContentColor)
        .SetDrawStateLayerOnTop(ADrawStateLayerOnTop)
        .SetStroke(AStroke)
        .SetShadow(AShadow)
        .SetSides(Sides)
        .SetCorners(Corners)
        .SetXRadius(XRadius)
        .SetYRadius(YRadius)
        .Draw;

      If AStopIndicator.hasStopIndicator then begin
        var LStopIndicatorCount: integer;
        if FCustomTrack.Frequency > 0 then
          LStopIndicatorCount := Ceil((FCustomTrack.Max - FCustomTrack.Min) / FCustomTrack.Frequency) + 1
        else
          LStopIndicatorCount := 2;
        if FCustomTrack.Orientation = TOrientation.Horizontal then begin
          if LStopIndicatorCount * 2{1 indicator + 1 empty space} > ABufDrawableRect.Width / AStopIndicator.Size then
            LStopIndicatorCount := 2;
        end
        else begin
          if LStopIndicatorCount * 2{1 indicator + 1 empty space} > ABufDrawableRect.Height / AStopIndicator.Size then
            LStopIndicatorCount := 2;
        end;
        LStopIndicatorCount := system.Math.Max(LStopIndicatorCount, 2);
        For var i := 0 to LStopIndicatorCount -1 do begin
          var LDstRect := TrectF.Create(0,0,AStopIndicator.Size, AStopIndicator.Size);
          LDstRect := LDstRect.CenterAt(ABufDrawableRect);
          var LPos: Single;
          if (LStopIndicatorCount = 2) and (I = 1) then
            LPos := FCustomTrack.GetThumbPos(FCustomTrack.Max)
          else
            LPos := FCustomTrack.GetThumbPos(System.math.Min(FCustomTrack.Min + (i * FCustomTrack.Frequency), FCustomTrack.Max));
          LPos := LPos - FCustomTrack.Padding.Left - (AStopIndicator.Size / 2);
          if FCustomTrack.Orientation = TOrientation.Horizontal then
            LDstRect.SetLocation(LPos, LDstRect.Top)
          else
            LDstRect.SetLocation(LDstRect.Left, Lpos);
          TALDrawRectangleHelper.Create(LCanvas)
            .SetScale(AScale)
            .SetAlignToPixel(IsPixelAlignmentEnabled)
            .SetDstRect(TRectF.Create(0, 0, 1, 1).FitInto(LDstRect))
            .SetFillColor(AStopIndicator.Color)
            .SetFillResourceName(AStopIndicator.ResourceName)
            .SetFillWrapMode(AStopIndicator.WrapMode)
            .SetXRadius(-50)
            .SetYRadius(-50)
            .Draw;
        end;
      end;

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALCreateDrawableFromSurface(LSurface);
    // The Shadow or Statelayer are not included in the dimensions of the fBufDrawableRect rectangle.
    // However, the fBufDrawableRect rectangle is offset by the dimensions of the shadow/Statelayer.
    ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);

  finally
    ALFreeAndNilSurface(LSurface, LCanvas);
  end;

end;

{**************************************************}
procedure TALDynamicListBoxCustomTrack.TTrack.Paint;
begin

  StateStyles.UpdateLastPaintedRawStyle;
  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;
  var LStateStyle := TBaseStateStyle(StateStyles.GetCurrentRawStyle);
  if LStateStyle <> nil then begin
    LDrawable := LStateStyle.FBufDrawable;
    LDrawableRect := LStateStyle.FBufDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := FBufDrawable;
      LDrawableRect := FBufDrawableRect;
    end;
  end
  else begin
    LDrawable := FBufDrawable;
    LDrawableRect := FBufDrawableRect;
  end;

  if ALIsDrawableNull(LDrawable) then begin

    var LCurrentAdjustedStateStyle := TBaseStateStyle(StateStyles.GetCurrentAdjustedStyle);
    if LCurrentAdjustedStateStyle = nil then begin
      inherited Paint;
      exit;
    end;

    {$IF DEFINED(ALSkiaCanvas)}

    TALDrawRectangleHelper.Create(TSkCanvasCustom(Canvas).Canvas.Handle)
      .SetAlignToPixel(IsPixelAlignmentEnabled)
      .SetDstRect(LocalRect.ReducePrecision)
      .SetOpacity(AbsoluteOpacity)
      .SetFill(LCurrentAdjustedStateStyle.Fill)
      .SetStateLayer(LCurrentAdjustedStateStyle.StateLayer, TalphaColors.Null)
      .SetStroke(LCurrentAdjustedStateStyle.Stroke)
      .SetShadow(LCurrentAdjustedStateStyle.Shadow)
      .SetSides(Sides)
      .SetCorners(Corners)
      .SetXRadius(XRadius)
      .SetYRadius(YRadius)
      .Draw;

    {$ELSE}

    {$IF defined(DEBUG)}
    if not doublebuffered then begin
      ALLog('TALDynamicListBoxCustomTrack.TTrack.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end;
    {$ENDIF}

    {$ENDIF}

    exit;
  end;

  var LSrcRect := GetBufDrawableSrcRect;
  var LDstRect := LSrcRect;
  LDstRect.Width := LDstRect.Width / Canvas.Scale;
  LDstRect.height := LDstRect.height / Canvas.Scale;
  LDstRect.SetLocation(0,0);
  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LSrcRect, // const ASrcRect: TrectF; // IN REAL PIXEL !
    LDstRect, // const ADstRect: TrectF; // IN Virtual pixels !
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*********************************************************************************}
function TALDynamicListBoxCustomTrack.TInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    Result.SetLocation(
      Result.width - (Width * Canvas.Scale) - 1{For AddPixelForAlignment in ALCreateSurface},
      Result.Top);
    Result.Width := (Width * Canvas.Scale) + 1{For AddPixelForAlignment in ALCreateSurface};
  end
  else begin
    Result.SetLocation(
      Result.Left,
      Result.Height - (Height * Canvas.Scale) - 1{For AddPixelForAlignment in ALCreateSurface});
    Result.Height := (Height * Canvas.Scale) + 1{For AddPixelForAlignment in ALCreateSurface};
  end;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{************************************************************************************}
function TALDynamicListBoxCustomTrack.TActiveTrack.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ff167efc;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomTrack.TActiveTrack.TDisabledStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $ff167efc;
end;

{**************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TActiveTrack.TDisabledStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{*******************************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.TActiveTrack.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TTrack.TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{**********************************************************************}
function TALDynamicListBoxCustomTrack.TActiveTrack.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{****************************************************************************************}
function TALDynamicListBoxCustomTrack.TActiveTrack.CreateStateStyles: TTrack.TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{*******************************************************************************}
function TALDynamicListBoxCustomTrack.TActiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then
    Result.Width := Width * Canvas.Scale
  else
    Result.Height := Height * Canvas.Scale;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{*********************************************************************}
constructor TALDynamicListBoxCustomTrack.Create(const AOwner: TObject);
begin
  FThumb := nil;
  FInactiveTrack := nil;
  FActiveTrack := nil;
  FValueIndicator := nil;
  //--
  inherited;
  //--
  //SetAcceptsControls(False);
  DisabledOpacity := 1;
  //CanFocus := True;
  //inherited TabStop := False;
  FTabStop := True;
  FIsAligningTracks := False;
  FOrientation := TOrientation.Horizontal;
  FOnChange := nil;
  //--
  FThumb := CreateThumb;
  FInactiveTrack := CreateInactiveTrack;
  FActiveTrack := CreateActiveTrack;
  FValueIndicator := CreateValueIndicator;
end;

{*******************************************************}
procedure TALDynamicListBoxCustomTrack.AfterConstruction;
begin
  inherited;
  realign;
end;

{**********************************************}
//procedure TALDynamicListBoxCustomTrack.Loaded;
//begin
//  if FThumb.FValueRange.IsChanged then
//    FThumb.FValueRange.Changed(True);
//  inherited;
//end;

{*********************************************************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.CreateInactiveTrack(const AInactiveTrackClass: TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TInactiveTrack;
begin
  if AInactiveTrackClass = nil then Exit(CreateInactiveTrack(TInactiveTrack, AName));
  //--
  Result := AInactiveTrackClass.Create(self);
  //Result.Parent := self;
  //Result.Stored := False;
  //Result.SetSubComponent(True);
  Result.Name := AName; // Useful at design time in the IDE
  //--
  //var LHalfHeight := GetDefaultSize.Height / 2;
  //var LMarginsChange := Result.Margins.OnChange;
  //Result.Margins.OnChange := nil;
  //Result.Margins.DefaultValue := TrectF.Create(0,LHalfHeight-1,0,LHalfHeight-1); // 2px height
  //Result.Margins.Rect := Result.Margins.DefaultValue;
  //Result.Margins.OnChange := LMarginsChange;
  //--
  if Orientation = TOrientation.Horizontal then Result.Align := TALAlignLayout.vertical
  else Result.Align := TALAlignLayout.horizontal;
end;

{***********************************************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.CreateActiveTrack(const AActiveTrackClass: TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TActiveTrack;
begin
  if AActiveTrackClass = nil then Exit(CreateActiveTrack(TActiveTrack, AName));
  //--
  Result := AActiveTrackClass.Create(self);
  //Result.Parent := self;
  //Result.Stored := False;
  //Result.SetSubComponent(True);
  Result.Name := AName; // Useful at design time in the IDE
  //--
  //var LHalfHeight := GetDefaultSize.Height / 2;
  //var LMarginsChange := Result.Margins.OnChange;
  //Result.Margins.OnChange := nil;
  //Result.Margins.DefaultValue := TrectF.Create(0,LHalfHeight-1,0,LHalfHeight-1); // 2px height
  //Result.Margins.Rect := Result.Margins.DefaultValue;
  //Result.Margins.OnChange := LMarginsChange;
  //--
  if Orientation = TOrientation.Horizontal then Result.Align := TALAlignLayout.vertical
  else Result.Align := TALAlignLayout.horizontal;
end;

{*****************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.CreateThumb(const AThumbClass: TThumbClass = nil; Const AName: String = 'Thumb'): TThumb;
begin
  if AThumbClass = nil then Exit(CreateThumb(TThumb, AName));
  //--
  Result := AThumbClass.Create(self);
  //Result.Parent := self;
  //Result.Stored := False;
  //Result.SetSubComponent(True);
  Result.Name := AName; // Useful at design time in the IDE
  Result.Width := GetDefaultSize.Height; // 32 px width
  //--
  if Orientation = TOrientation.Horizontal then Result.Align := TALAlignLayout.vertical
  else Result.Align := TALAlignLayout.horizontal;
end;

{**************************************************************************************************************************************************************************}
function TALDynamicListBoxCustomTrack.CreateValueIndicator(const AValueIndicatorClass: TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TValueIndicator;
begin
  if AValueIndicatorClass = nil then Exit(CreateValueIndicator(TValueIndicator, AName));
  //--
  Result := AValueIndicatorClass.Create(self);
  //Result.Parent := self;
  //Result.Stored := False;
  //Result.SetSubComponent(True);
  Result.Name := AName; // Useful at design time in the IDE
end;

{*****************************************************}
procedure TALDynamicListBoxCustomTrack.MakeBufDrawable;
begin
  if FInactiveTrack <> nil then FInactiveTrack.MakeBufDrawable;
  if FActiveTrack <> nil then FActiveTrack.MakeBufDrawable;
  if FThumb <> nil then FThumb.MakeBufDrawable;
  //if FValueIndicator <> nil then FValueIndicator.MakeBufDrawable;
end;

{******************************************************}
procedure TALDynamicListBoxCustomTrack.ClearBufDrawable;
begin
  if FInactiveTrack <> nil then FInactiveTrack.ClearBufDrawable;
  if FActiveTrack <> nil then FActiveTrack.ClearBufDrawable;
  if FThumb <> nil then FThumb.ClearBufDrawable;
  if FValueIndicator <> nil then FValueIndicator.ClearBufDrawable;
end;

{*********************************************************}
function TALDynamicListBoxCustomTrack.ValueStored: Boolean;
begin
  Result := not SameValue(Value, 0, Tepsilon.Vector);
end;

{****************************************************************}
function TALDynamicListBoxCustomTrack.ViewportSizeStored: Boolean;
begin
  Result := not SameValue(ViewportSize, 0, Tepsilon.Vector);
end;

{*************************************************************}
function TALDynamicListBoxCustomTrack.FrequencyStored: Boolean;
begin
  Result := not SameValue(Frequency, 0, Tepsilon.Vector);
end;

{*******************************************************}
function TALDynamicListBoxCustomTrack.MaxStored: Boolean;
begin
  Result := not SameValue(Max, FMX.StdActns.DefaultMaxValue, Tepsilon.Vector);
end;

{*******************************************************}
function TALDynamicListBoxCustomTrack.MinStored: Boolean;
begin
  Result := not SameValue(Min, 0, Tepsilon.Vector);
end;

{***************************************************}
function TALDynamicListBoxCustomTrack.GetMax: Double;
begin
  Result := FThumb.FValueRange.Max;
end;

{*****************************************************************}
procedure TALDynamicListBoxCustomTrack.SetMax(const Value: Double);
begin
  if not SameValue(GetMax, Value) then begin
    if compareValue(Value, Min) < 0 then min := Value;
    FThumb.FValueRange.Max := Value;
    ClearBufDrawable;
  end;
end;

{***************************************************}
function TALDynamicListBoxCustomTrack.GetMin: Double;
begin
  Result := FThumb.FValueRange.Min;
end;

{*****************************************************************}
procedure TALDynamicListBoxCustomTrack.SetMin(const Value: Double);
begin
  if not SameValue(GetMin, Value) then begin
    if compareValue(Value, Max) > 0 then max := Value;
    FThumb.FValueRange.Min := Value;
    ClearBufDrawable;
  end;
end;

{*********************************************************}
function TALDynamicListBoxCustomTrack.GetFrequency: Double;
begin
  Result := FThumb.FValueRange.Frequency;
end;

{***********************************************************************}
procedure TALDynamicListBoxCustomTrack.SetFrequency(const Value: Double);
begin
  if not SameValue(GetFrequency, Value) then begin
    FThumb.FValueRange.Frequency := Value;
    ClearBufDrawable;
  end;
end;

{*****************************************************}
function TALDynamicListBoxCustomTrack.GetValue: Double;
begin
  Result := FThumb.FValueRange.Value;
end;

{*************************************************************}
procedure TALDynamicListBoxCustomTrack.SetValue(Value: Double);
begin
  if not SameValue(GetValue, Value) then
    FThumb.FValueRange.Value := Value;
end;

{************************************************************}
function TALDynamicListBoxCustomTrack.GetViewportSize: Double;
begin
  Result := FThumb.FValueRange.ViewportSize;
end;

{**************************************************************************}
procedure TALDynamicListBoxCustomTrack.SetViewportSize(const Value: Double);
begin
  if not SameValue(GetViewportSize, Value) then
    FThumb.FValueRange.ViewportSize := Value;
end;

{***************************************************************}
function TALDynamicListBoxCustomTrack.GetDoubleBuffered: boolean;
begin
  result := FThumb.DoubleBuffered;
end;

{******************************************************************************}
procedure TALDynamicListBoxCustomTrack.SetDoubleBuffered(const AValue: Boolean);
begin
  FThumb.DoubleBuffered := AValue;
  if FInactiveTrack <> nil then FInactiveTrack.DoubleBuffered := AValue;
  if FActiveTrack <> nil then FActiveTrack.DoubleBuffered := AValue;
  if FValueIndicator <> nil then FValueIndicator.DoubleBuffered := AValue;
end;

{************************************************************}
//function TALDynamicListBoxCustomTrack._GetCanFocus: Boolean;
//begin
//  Result := inherited CanFocus;
//end;

{**************************************************************************}
//procedure TALDynamicListBoxCustomTrack._SetCanFocus(const Value: Boolean);
//begin
//  Inherited CanFocus := Value;
//  if FThumb <> nil then FThumb.CanFocus := Value;
//end;

{*************************************************************************}
//procedure TALDynamicListBoxCustomTrack._SetTabStop(const Value: Boolean);
//begin
//  FTabStop := Value;
//  if FThumb <> nil then FThumb.TabStop := Value;
//end;

{************************************************************}
function TALDynamicListBoxCustomTrack.GetLeadingTrack: TTrack;
begin
  Result := FActiveTrack;
end;

{*************************************************************}
function TALDynamicListBoxCustomTrack.GetTrailingTrack: TTrack;
begin
  Result := FInactiveTrack;
end;

{************************************************************************}
function TALDynamicListBoxCustomTrack.GetLeadingTrackStartPadding: Single;
begin
  var LLeadingTrack := GetLeadingTrack;
  if LLeadingTrack <> nil then begin
    If Orientation = TOrientation.Horizontal then Result := LLeadingTrack.Padding.Left
    else Result := LLeadingTrack.Padding.Top;
  end
  else
    Result := 0;
end;

{***********************************************************************}
function TALDynamicListBoxCustomTrack.GetTrailingTrackEndPadding: Single;
begin
  var LTrailingTrack := GetTrailingTrack;
  if LTrailingTrack <> nil then begin
    If Orientation = TOrientation.Horizontal then Result := LTrailingTrack.Padding.Right
    else Result := LTrailingTrack.Padding.bottom;
  end
  else
    Result := 0;
end;

{******************************************************************************************************}
function TALDynamicListBoxCustomTrack.GetTrackSize(Const AIncludeTrackPadding: Boolean = False): Single;
begin
  if Orientation = TOrientation.Horizontal then begin
    result := Width - Padding.Left - Padding.Right;
    If FThumb <> nil then
      Result := Result - FThumb.Width{FThumb.Width/2 on the left + FThumb.Width/2 on the right};
  end
  else begin
    result := Height - Padding.Top - Padding.Bottom;
    If FThumb <> nil then
      Result := Result - FThumb.Height{FThumb.Height/2 on the left + FThumb.Height/2 on the right};
  end;
  If not AIncludeTrackPadding then
    Result := Result - GetLeadingTrackStartPadding - GetTrailingTrackEndPadding;
end;

{***********************************************}
procedure TALDynamicListBoxCustomTrack.DoResized;
begin
  inherited;
  if FActiveTrack <> nil then
    FActiveTrack.ClearBufDrawable;
  if FInactiveTrack <> nil then
    FInactiveTrack.ClearBufDrawable;
end;

{***********************************************}
procedure TALDynamicListBoxCustomTrack.DoRealign;
begin
  inherited;
  var LSaveDisableAlign := FDisableAlign;
  var LSaveIsAligningTracks := FIsAligningTracks;
  FIsAligningTracks := True;
  FDisableAlign := True;
  BeginUpdate;
  Try
    AlignThumb;
    AlignTracks;
  Finally
    EndUpdate;
    FDisableAlign := LSaveDisableAlign;
    FIsAligningTracks := LSaveIsAligningTracks;
  End;
end;

{******************************************************************************}
function TALDynamicListBoxCustomTrack.GetThumbPos(const AValue: single): Single;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _ValueToPos(MinValue, MaxValue, ViewportSize: Double; TrackSize, Value: Single): Single;
  begin
    if ViewportSize < 0 then ViewportSize := 0;
    var LValRel: Double := MaxValue - MinValue - ViewportSize;
    if LValRel > 0 then begin
      LValRel := (Value - MinValue) / LValRel;
      Result := TrackSize * LValRel;
    end
    else Result := 0;
  end;

begin
  var LPos := _ValueToPos(
                Min, // MinValue
                Max, // MaxValue
                ViewportSize, // ViewportSize
                GetTrackSize, // TrackSize
                AValue); // Value
  If Orientation = TOrientation.Horizontal then Result := LPos + padding.Left
  else Result := LPos + padding.Top;
  Result := Result + GetLeadingTrackStartPadding;
  Result := ALAlignDimensionToPixelRound(Result, ALGetScreenScale, TEpsilon.Position);
end;

{************************************************}
procedure TALDynamicListBoxCustomTrack.AlignThumb;
begin
  if FThumb = nil then exit;
  var LThumbPos := GetThumbPos(Value);
  If Orientation = TOrientation.Horizontal then
    FThumb.Left := LThumbPos
  else
    FThumb.Top := LThumbPos
end;

{*************************************************}
procedure TALDynamicListBoxCustomTrack.AlignTracks;
begin
  if FThumb = nil then exit;
  if Orientation = TOrientation.Horizontal then begin
    if FInactiveTrack <> nil then begin
      FInactiveTrack.Left := FThumb.Left + Fthumb.Width + Fthumb.Margins.Right;
      FInactiveTrack.Width := Width - Padding.Right - FInactiveTrack.Left - (Fthumb.Width / 2);
    end;
    if FActiveTrack <> nil then begin
      FActiveTrack.Left := Padding.Left + (Fthumb.Width / 2);
      FActiveTrack.Width := FThumb.Left - Fthumb.Margins.Left - FActiveTrack.Left;
    end;
  end
  else begin
    if FInactiveTrack <> nil then begin
      FInactiveTrack.Top := FThumb.Top + Fthumb.Height + Fthumb.Margins.Bottom;
      FInactiveTrack.Height := Height - Padding.Bottom - FInactiveTrack.Top - (Fthumb.Height / 2);
    end;
    if FActiveTrack <> nil then begin
      FActiveTrack.Top := Padding.Top + (Fthumb.Height / 2);
      FActiveTrack.Height := FThumb.Top - Fthumb.Margins.Top - FActiveTrack.Top;
    end;
  end;
end;

{***********************************************}
procedure TALDynamicListBoxCustomTrack.DoChanged;
begin
  //if {not (csLoading in ComponentState) and} Assigned(FOnChange) then
    FOnChange(Self);
end;

{****************************************************}
procedure TALDynamicListBoxCustomTrack.EnabledChanged;
begin
  inherited;
  if FInactiveTrack <> nil then FInactiveTrack.enabled := enabled;
  if FActiveTrack <> nil then FActiveTrack.enabled := enabled;
  if FThumb <> nil then FThumb.enabled := enabled;
  if FValueIndicator <> nil then FValueIndicator.enabled := enabled;
end;

{*******************************************************************************}
procedure TALDynamicListBoxCustomTrack.SetOrientation(const Value: TOrientation);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapTopBottomWithLeftRight(Const ARect: TrectF): TRectF;
  Begin
    Result.Left := ARect.Top;
    Result.Top := ARect.Left;
    Result.Right := ARect.Bottom;
    Result.Bottom := ARect.Right;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapAlign(Const AAlign: TALAlignLayout): TALAlignLayout;
  Begin
    If AAlign = TALAlignLayout.Vertical then result := TALAlignLayout.Horizontal
    else If AAlign = TALAlignLayout.Horizontal then result := TALAlignLayout.Vertical
    else result := AAlign;
  End;

begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    //if not (csLoading in ComponentState) then begin
      BeginUpdate;
      Try
        SetBounds(Left, Top, Height, Width);
        Margins.Rect := SwapTopBottomWithLeftRight(Margins.Rect);
        Padding.Rect := SwapTopBottomWithLeftRight(Padding.Rect);
        if FActiveTrack <> nil then begin
          FActiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FActiveTrack.Margins.Rect);
          FActiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FActiveTrack.Padding.Rect);
          FActiveTrack.Align := SwapAlign(FActiveTrack.Align);
        end;
        if FInactiveTrack <> nil then begin
          FInactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FInactiveTrack.Margins.Rect);
          FInactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FInactiveTrack.Padding.Rect);
          FInactiveTrack.Align := SwapAlign(FInactiveTrack.Align);
        end;
        if FThumb <> nil then begin
          FThumb.Margins.Rect := SwapTopBottomWithLeftRight(FThumb.Margins.Rect);
          FThumb.padding.Rect := SwapTopBottomWithLeftRight(FThumb.padding.Rect);
          FThumb.TouchTargetExpansion := SwapTopBottomWithLeftRight(FThumb.TouchTargetExpansion);
          var LThumbWidth := FThumb.Width;
          FThumb.Width := FThumb.Height;
          FThumb.Height := LThumbWidth;
          FThumb.Align := SwapAlign(FThumb.Align);
        end;
      Finally
        EndUpdate;
      End;
    //end
    //else begin
    //  if FActiveTrack <> nil then
    //    FActiveTrack.Align := SwapAlign(FActiveTrack.Align);
    //  if FInactiveTrack <> nil then
    //    FInactiveTrack.Align := SwapAlign(FInactiveTrack.Align);
    //  if FThumb <> nil then
    //    FThumb.Align := SwapAlign(FThumb.Align);
    //end;
  end;
end;

{********************************************************}
function TALDynamicListBoxTrackBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 32);
end;

{*****************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.SetXRadius(const Value: Single);
begin
  Inherited;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetXRadius(Value);
end;

{*****************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.SetYRadius(const Value: Single);
begin
  Inherited;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetYRadius(Value);
end;

{*******************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.SetCorners(const Value: TCorners);
begin
  Inherited;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.SetCorners(Value);
end;

{************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.MarginsChanged;
begin
  Inherited;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Margins.Rect := Margins.Rect;
end;

{************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.PaddingChanged;
begin
  Inherited;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Padding.Rect := Padding.Rect;
end;

{***********************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.StopIndicatorChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.StopIndicator.Assign(StopIndicator);
end;

{**************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.FillChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Fill.Assign(Fill);
end;

{****************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.StrokeChanged(Sender: TObject);
begin
  Inherited;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Stroke.Assign(Stroke);
end;

{****************************************************************************}
function TALDynamicListBoxRangeTrackBar.TMinInactiveTrack._GetOpacity: Single;
begin
  Result := Inherited Opacity;
end;

{*******************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinInactiveTrack._SetOpacity(const AValue: Single);
begin
  Inherited Opacity := AValue;
  var LMaxInactiveTrack := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxInactiveTrack;
  if LMaxInactiveTrack <> nil then
    LMaxInactiveTrack.Opacity := AValue;
end;

{**********************************************************************************}
function TALDynamicListBoxRangeTrackBar.TMinInactiveTrack._IsOpacityStored: boolean;
begin
  Result := not SameValue(FOpacity, 1);
end;

{**************************************************************************************}
function TALDynamicListBoxRangeTrackBar.TMinInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then
    Result.Width := Width * Canvas.Scale
  else
    Result.Height := Height * Canvas.Scale;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{**************************************************************************************}
function TALDynamicListBoxRangeTrackBar.TMaxInactiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    Result.SetLocation(
      Result.width - (Width * Canvas.Scale) - 1{For AddPixelForAlignment in ALCreateSurface},
      Result.Top);
    Result.Width := (Width * Canvas.Scale) + 1{For AddPixelForAlignment in ALCreateSurface};
  end
  else begin
    Result.SetLocation(
      Result.Left,
      Result.Height - (Height * Canvas.Scale) - 1{For AddPixelForAlignment in ALCreateSurface});
    Result.Height := (Height * Canvas.Scale) + 1{For AddPixelForAlignment in ALCreateSurface};
  end;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{*********************************************************************************}
function TALDynamicListBoxRangeTrackBar.TActiveTrack.GetBufDrawableSrcRect: TRectF;
begin
  if ALIsDrawableNull(FBufDrawable) then Exit(TRectF.Empty);
  Result := TRectF.Create(0, 0, ALGetDrawableWidth(FBufDrawable), ALGetDrawableHeight(FBufDrawable));
  if FCustomTrack.Orientation = TOrientation.Horizontal then begin
    Var LThumbWidth: Single;
    If FcustomTrack.FThumb <> nil then LThumbWidth := FcustomTrack.FThumb.Width
    else LThumbWidth := 0;
    Result.SetLocation(
      (Left - FcustomTrack.Padding.Left - (LThumbWidth / 2)) * Canvas.Scale,
      Result.Top);
    Result.Width := Width * Canvas.Scale;
  end
  else begin
    Var LThumbHeight: Single;
    If FcustomTrack.FThumb <> nil then LThumbHeight := FcustomTrack.FThumb.Height
    else LThumbHeight := 0;
    Result.SetLocation(
      Result.Left,
      (Top - FcustomTrack.Padding.Top - (LThumbHeight / 2)) * Canvas.Scale);
    Result.Height := Height * Canvas.Scale;
  end;
  Result := ALAlignToPixelRound(Result, TMatrix.Identity, ALGetScreenScale{Scale}, TEpsilon.Position);
end;

{************************************************************************************************************}
constructor TALDynamicListBoxRangeTrackBar.TMinThumb.Create(const ACustomTrack: TALDynamicListBoxCustomTrack);
begin
  inherited;
  //FFormerTouchTargetExpansionChangedHandler := TouchTargetExpansion.OnChange;
  //TouchTargetExpansion.OnChange := TouchTargetExpansionChanged;
end;

{********************************************************************}
function TALDynamicListBoxRangeTrackBar.TMinThumb._GetOpacity: Single;
begin
  Result := Inherited Opacity;
end;

{***********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb._SetOpacity(const AValue: Single);
begin
  Inherited Opacity := AValue;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Opacity := AValue;
end;

{********************************************************************}
function TALDynamicListBoxRangeTrackBar.TMinThumb._GetCursor: TCursor;
begin
  Result := Inherited Cursor;
end;

{***********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb._SetCursor(const AValue: TCursor);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Cursor := AValue;
end;

{**************************************************************************}
function TALDynamicListBoxRangeTrackBar.TMinThumb._IsOpacityStored: boolean;
begin
  Result := not SameValue(FOpacity, 1);
end;

{*********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.SetXRadius(const Value: Single);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.XRadius := Value;
end;

{*********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.SetYRadius(const Value: Single);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.YRadius := Value;
end;

{***********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.SetCorners(const Value: TCorners);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Corners := Value;
end;

{****************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.MarginsChanged;
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Margins.rect := Margins.rect;
end;

{****************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.PaddingChanged;
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Padding.rect := Padding.rect;
end;

{******************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.FillChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Fill.Assign(Fill);
end;

{********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.StrokeChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Stroke.Assign(Stroke);
end;

{********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.ShadowChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.Shadow.Assign(Shadow);
end;

{*************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.StateStylesChanged(Sender: TObject);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.StateStyles.Assign(StateStyles);
end;

{***********************************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.SetTouchTargetExpansion(const AValue: TRectF);
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.TouchTargetExpansion := TouchTargetExpansion;
end;

{***********************************************************}
procedure TALDynamicListBoxRangeTrackBar.TMinThumb.DoResized;
begin
  Inherited;
  var LMaxThumb := TALDynamicListBoxRangeTrackBar(FCustomTrack).FMaxThumb;
  if LMaxThumb <> nil then
    LMaxThumb.SetSize(TSizeF.Create(Width, Height));
end;

{************************************************************************************************************}
constructor TALDynamicListBoxRangeTrackBar.TMaxThumb.Create(const ACustomTrack: TALDynamicListBoxCustomTrack);
begin
  inherited;
  var LValueRangeChanged := FValueRange.OnChanged;
  FValueRange.OnChanged := nil;
  FValueRange.Value := FValueRange.Max;
  FValueRange.OnChanged := LValueRangeChanged;
end;

{***********************************************************************}
constructor TALDynamicListBoxRangeTrackBar.Create(const AOwner: TObject);
begin
  //--
  FMaxInactiveTrack := nil;
  FMaxThumb := nil;
  //--
  inherited;
  //--
  FMaxInactiveTrack := CreateInactiveTrack(TMaxInactiveTrack, 'MaxInactiveTrack');
  FMaxThumb := CreateThumb(TMaxThumb, 'MaxThumb');
  //--
  //FThumb.TabOrder := 0;
  //FMaxThumb.TabOrder := 1;
end;

{************************************************}
//procedure TALDynamicListBoxRangeTrackBar.Loaded;
//begin
//  if FMaxThumb.FValueRange.IsChanged then
//    FMaxThumb.FValueRange.Changed(True);
//  inherited;
//end;

{******************************************************}
procedure TALDynamicListBoxRangeTrackBar.EnabledChanged;
begin
  inherited;
  if FMaxInactiveTrack <> nil then FMaxInactiveTrack.enabled := enabled;
  if FMaxThumb <> nil then FMaxThumb.enabled := enabled;
end;

{*******************************************************}
procedure TALDynamicListBoxRangeTrackBar.MakeBufDrawable;
begin
  inherited;
  if FMaxInactiveTrack <> nil then FMaxInactiveTrack.MakeBufDrawable;
  if FMaxThumb <> nil then FMaxThumb.MakeBufDrawable;
end;

{********************************************************}
procedure TALDynamicListBoxRangeTrackBar.ClearBufDrawable;
begin
  inherited;
  if FMaxInactiveTrack <> nil then FMaxInactiveTrack.ClearBufDrawable;
  if FMaxThumb <> nil then FMaxThumb.ClearBufDrawable;
end;

{*******************************************************************************************}
function TALDynamicListBoxRangeTrackBar.GetLeadingTrack: TALDynamicListBoxCustomTrack.TTrack;
begin
  Result := FInactiveTrack;
end;

{********************************************************************************************}
function TALDynamicListBoxRangeTrackBar.GetTrailingTrack: TALDynamicListBoxCustomTrack.TTrack;
begin
  Result := FMaxInactiveTrack;
end;

{*************************************************************}
function TALDynamicListBoxRangeTrackBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(200, 32);
end;

{**************************************************************}
function TALDynamicListBoxRangeTrackBar.MaxValueStored: Boolean;
begin
  Result := not SameValue(MaxValue, FMX.StdActns.DefaultMaxValue, Tepsilon.Vector);
end;

{*******************************************************************}
procedure TALDynamicListBoxRangeTrackBar.SetMax(const Value: Double);
begin
  if not SameValue(GetMax, Value) then begin
    inherited;
    FMaxThumb.FValueRange.Max := Value;
  end;
end;

{*******************************************************************}
procedure TALDynamicListBoxRangeTrackBar.SetMin(const Value: Double);
begin
  if not SameValue(GetMin, Value) then begin
    inherited;
    FMaxThumb.FValueRange.Min := Value;
  end;
end;

{*************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.SetFrequency(const Value: Double);
begin
  if not SameValue(GetFrequency, Value) then begin
    inherited;
    FMaxThumb.FValueRange.Frequency := Value;
  end;
end;

{***************************************************************}
procedure TALDynamicListBoxRangeTrackBar.SetValue(Value: Double);
begin
  if not SameValue(GetValue, Value) then begin
    inherited;
    if (not fThumb.Pressed) and
       (GetValue > (max - Min) / 2) then fThumb.BringToFront;
  end;
end;

{**********************************************************}
function TALDynamicListBoxRangeTrackBar.GetMaxValue: Double;
begin
  Result := FMaxThumb.FValueRange.Value;
end;

{******************************************************************}
procedure TALDynamicListBoxRangeTrackBar.SetMaxValue(Value: Double);
begin
  if not SameValue(GetMaxValue, Value) then begin
    FMaxThumb.FValueRange.Value := Value;
    if (not fMaxThumb.Pressed) and
       (GetMaxValue < (max - Min) / 2) then fMaxThumb.BringToFront;
  end;
end;

{****************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.SetViewportSize(const Value: Double);
begin
  if not SameValue(GetViewportSize, Value) then begin
    inherited;
    FMaxThumb.FValueRange.ViewportSize := Value;
  end;
end;

{********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.SetDoubleBuffered(const AValue: Boolean);
begin
  Inherited;
  if FMaxThumb <> nil then FMaxThumb.DoubleBuffered := AValue;
end;

{****************************************************************************}
//procedure TALDynamicListBoxRangeTrackBar._SetCanFocus(const Value: Boolean);
//begin
//  Inherited;
//  if FMaxThumb <> nil then FMaxThumb.CanFocus := Value;
//end;

{***************************************************************************}
//procedure TALDynamicListBoxRangeTrackBar._SetTabStop(const Value: Boolean);
//begin
//  Inherited;
//  if FMaxThumb <> nil then FMaxThumb.TabStop := Value;
//end;

{*********************************************************************************************************************************************************************************************************************************}
function TALDynamicListBoxRangeTrackBar.CreateInactiveTrack(const AInactiveTrackClass: TALDynamicListBoxCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALDynamicListBoxCustomTrack.TInactiveTrack;
begin
  if AInactiveTrackClass = nil then Exit(CreateInactiveTrack(TMinInactiveTrack, AName));
  result := Inherited;
end;

{***********************************************************************************************************************************************************************************************************************}
function TALDynamicListBoxRangeTrackBar.CreateActiveTrack(const AActiveTrackClass: TALDynamicListBoxCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALDynamicListBoxCustomTrack.TActiveTrack;
begin
  if AActiveTrackClass = nil then Exit(CreateActiveTrack(TActiveTrack, AName));
  result := Inherited;
end;

{*****************************************************************************************************************************************************************************************}
function TALDynamicListBoxRangeTrackBar.CreateThumb(const AThumbClass: TALDynamicListBoxCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALDynamicListBoxCustomTrack.TThumb;
begin
  if AThumbClass = nil then Exit(CreateThumb(TMinThumb, AName));
  result := Inherited;
end;

{*************************************************}
procedure TALDynamicListBoxRangeTrackBar.DoResized;
begin
  inherited;
  if FMaxInactiveTrack <> nil then
    FMaxInactiveTrack.ClearBufDrawable;
end;

{*************************************************}
procedure TALDynamicListBoxRangeTrackBar.DoRealign;
begin
  // Realign is called by TALValueRangeTrack.DoChanged,
  // so we can check here if minValue <= maxValue.
  if (FThumb <> nil) and (FMaxThumb <> nil) and (minValue > MaxValue) then begin
    if fThumb.Pressed then MinValue := MaxValue
    else MaxValue := MinValue;
    exit; // no need to continue, this function will be called again
  end;
  inherited DoRealign;
end;

{**************************************************}
procedure TALDynamicListBoxRangeTrackBar.AlignThumb;
begin
  Inherited;
  if FMaxThumb = nil then exit;
  var LMaxThumbPos := GetThumbPos(MaxValue);
  If Orientation = TOrientation.Horizontal then
    FMaxThumb.Left := LMaxThumbPos
  else
    FMaxThumb.Top := LMaxThumbPos
end;

{***************************************************}
procedure TALDynamicListBoxRangeTrackBar.AlignTracks;
begin
  if (FThumb = nil) or (FMaxThumb = nil) then exit;
  if Orientation = TOrientation.Horizontal then begin
    if FInactiveTrack <> nil then begin
      FInactiveTrack.Left := Padding.Left + (Fthumb.Width / 2);
      FInactiveTrack.Width := FThumb.Left - Fthumb.Margins.Left - FInactiveTrack.Left;
    end;
    if FActiveTrack <> nil then begin
      FActiveTrack.Left := FThumb.Left + Fthumb.Width + Fthumb.Margins.Right;
      FActiveTrack.Width := FMaxThumb.Left - FMaxthumb.Margins.Left - FActiveTrack.Left;
    end;
    if FMaxInactiveTrack <> nil then begin
      FMaxInactiveTrack.Left := FMaxThumb.Left + FMaxthumb.Width + FMaxthumb.Margins.Right;
      FMaxInactiveTrack.Width := Width - Padding.Right - FMaxInactiveTrack.Left - (FMaxthumb.Width / 2);
    end;
  end
  else begin
    if FInactiveTrack <> nil then begin
      FInactiveTrack.Top := Padding.Top + (Fthumb.Height / 2);
      FInactiveTrack.Height := FThumb.Top - Fthumb.Margins.Top - FInactiveTrack.Top;
    end;
    if FActiveTrack <> nil then begin
      FActiveTrack.Top := FThumb.Top + Fthumb.Height + Fthumb.Margins.Bottom;
      FActiveTrack.Height := FMaxThumb.Top - FMaxthumb.Margins.Top - FActiveTrack.Top;
    end;
    if FMaxInactiveTrack <> nil then begin
      FMaxInactiveTrack.Top := FMaxThumb.Top + FMaxthumb.Height + FMaxthumb.Margins.Bottom;
      FMaxInactiveTrack.Height := Height - Padding.Bottom - FMaxInactiveTrack.Top - (FMaxthumb.Height / 2);
    end;
  end;
end;

{*********************************************************************************}
procedure TALDynamicListBoxRangeTrackBar.SetOrientation(const Value: TOrientation);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapTopBottomWithLeftRight(Const ARect: TrectF): TRectF;
  Begin
    Result.Left := ARect.Top;
    Result.Top := ARect.Left;
    Result.Right := ARect.Bottom;
    Result.Bottom := ARect.Right;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function SwapAlign(Const AAlign: TALAlignLayout): TALAlignLayout;
  Begin
    If AAlign = TALAlignLayout.Vertical then result := TALAlignLayout.Horizontal
    else If AAlign = TALAlignLayout.Horizontal then result := TALAlignLayout.Vertical
    else result := AAlign;
  End;

begin
  if FOrientation <> Value then begin
    //if not (csLoading in ComponentState) then begin
      BeginUpdate;
      Try
        inherited;
        if FMaxInactiveTrack <> nil then begin
          FMaxInactiveTrack.Margins.Rect := SwapTopBottomWithLeftRight(FMaxInactiveTrack.Margins.Rect);
          FMaxInactiveTrack.Padding.Rect := SwapTopBottomWithLeftRight(FMaxInactiveTrack.Padding.Rect);
          FMaxInactiveTrack.Align := SwapAlign(FMaxInactiveTrack.Align);
        end;
        if FMaxThumb <> nil then begin
          FMaxThumb.Margins.Rect := SwapTopBottomWithLeftRight(FMaxThumb.Margins.Rect);
          FMaxThumb.padding.Rect := SwapTopBottomWithLeftRight(FMaxThumb.padding.Rect);
          FMaxThumb.TouchTargetExpansion := SwapTopBottomWithLeftRight(FMaxThumb.TouchTargetExpansion);
          var LMaxThumbWidth := FMaxThumb.Width;
          FMaxThumb.Width := FMaxThumb.Height;
          FMaxThumb.Height := LMaxThumbWidth;
          FMaxThumb.Align := SwapAlign(FMaxThumb.Align);
        end;
      Finally
        EndUpdate;
      End;
    //end
    //else begin
    //  inherited;
    //  if FMaxInactiveTrack <> nil then
    //    FMaxInactiveTrack.Align := SwapAlign(FMaxInactiveTrack.Align);
    //  if FMaxThumb <> nil then
    //    FMaxThumb.Align := SwapAlign(FMaxThumb.Align);
    //end;
  end;
end;

{**********************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{******************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TDisabledStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TDisabledStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{*********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TDisabledStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{*********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TDisabledStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TDisabledStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{**************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TDisabledStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{***************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TDisabledStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{*****************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.THoveredStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{*******************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.THoveredStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.THoveredStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.THoveredStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{***********************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.THoveredStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{*************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.THoveredStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{**************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.THoveredStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{*****************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TPressedStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{*******************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TPressedStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TPressedStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TPressedStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{***********************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TPressedStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{*************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TPressedStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{**************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TPressedStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{*****************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TFocusedStateStyle.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $47000000;
end;

{*******************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TFocusedStateStyle.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := Talphacolors.Null;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TFocusedStateStyle.TStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{********************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TFocusedStateStyle.TStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{***********************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TFocusedStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TFill.Create(AParent);
end;

{*************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TFocusedStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TStroke.Create(AParent);
end;

{**************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TFocusedStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TStateLayer.Create;
end;

{**********************************************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TStateStyles.CreateDisabledStateStyle(const AParent: TObject): TALDynamicListBoxCustomTrack.TThumb.TDisabledStateStyle;
begin
  Result := TDisabledStateStyle.Create(AParent);
end;

{********************************************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TStateStyles.CreateHoveredStateStyle(const AParent: TObject): TALDynamicListBoxCustomTrack.TThumb.THoveredStateStyle;
begin
  Result := THoveredStateStyle.Create(AParent);
end;

{********************************************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TStateStyles.CreatePressedStateStyle(const AParent: TObject): TALDynamicListBoxCustomTrack.TThumb.TPressedStateStyle;
begin
  Result := TPressedStateStyle.Create(AParent);
end;

{********************************************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.TStateStyles.CreateFocusedStateStyle(const AParent: TObject): TALDynamicListBoxCustomTrack.TThumb.TFocusedStateStyle;
begin
  Result := TFocusedStateStyle.Create(AParent);
end;

{********************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{****************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{*******************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.CreateStateStyles: TALDynamicListBoxCustomTrack.TThumb.TStateStyles;
begin
  Result := TStateStyles.Create(self);
end;

{*************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{*************************************************************************}
function TALDynamicListBoxCustomScrollBar.TThumb.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{*************************************************************************}
constructor TALDynamicListBoxCustomScrollBar.Create(const AOwner: TObject);
begin
  inherited;
  //CanFocus := False;
end;

{***************************************************************}
function TALDynamicListBoxCustomScrollBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 18);
end;

{****************************************************}
procedure TALDynamicListBoxCustomScrollBar.AlignThumb;
begin
  if FThumb = nil then exit;
  if ViewportSize > 0 then begin
    If Orientation = TOrientation.Horizontal then begin
      FThumb.Width := ALAlignDimensionToPixelRound(
                        System.Math.Min(
                          System.Math.MaxValue(
                            [ViewportSize / (Max - Min) * Width,
                             Height / 2,
                             5{MinThumbSize}]),
                          Width),
                        ALGetScreenScale,
                        Tepsilon.Position);
    end
    else begin
      FThumb.Height := ALAlignDimensionToPixelRound(
                         System.Math.Min(
                           System.Math.MaxValue(
                             [ViewportSize / (Max - Min) * Height,
                              Width / 2,
                              5{MinThumbSize}]),
                           Height),
                         ALGetScreenScale,
                         Tepsilon.Position);
    end;
  end;
  inherited;
end;

{*******************************************************************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.CreateThumb(const AThumbClass: TALDynamicListBoxCustomTrack.TThumbClass = nil; Const AName: String = 'Thumb'): TALDynamicListBoxCustomTrack.TThumb;
begin
  if AThumbClass = nil then Exit(CreateThumb(TThumb, AName));
  result := Inherited;
end;

{***********************************************************************************************************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.CreateInactiveTrack(const AInactiveTrackClass: TALDynamicListBoxCustomTrack.TInactiveTrackClass = nil; Const AName: String = 'InactiveTrack'): TALDynamicListBoxCustomTrack.TInactiveTrack;
begin
  Result := Nil;
end;

{*************************************************************************************************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.CreateActiveTrack(const AActiveTrackClass: TALDynamicListBoxCustomTrack.TActiveTrackClass = nil; Const AName: String = 'ActiveTrack'): TALDynamicListBoxCustomTrack.TActiveTrack;
begin
  Result := Nil;
end;

{****************************************************************************************************************************************************************************************************************************************}
function TALDynamicListBoxCustomScrollBar.CreateValueIndicator(const AValueIndicatorClass: TALDynamicListBoxCustomTrack.TValueIndicatorClass = nil; Const AName: String = 'ValueIndicator'): TALDynamicListBoxCustomTrack.TValueIndicator;
begin
  Result := Nil;
end;

{****************************************************************}
constructor TALDynamicListBoxLayout.Create(const AOwner: TObject);
begin
  inherited Create(AOwner);
  //CanParentFocus := True;
  HitTest := False;
end;

{**************************************}
procedure TALDynamicListBoxLayout.Paint;
begin
  inherited;
  //if (csDesigning in ComponentState) and not Locked then
  //  DrawDesignBorder;
end;

{$ENDREGION}

//////////////////////////////////////////////
/// THE CODE ABOVE WAS AUTO-GENERATED FROM ///
/// <ALCINOE>\Tools\CodeBuilder.           ///
//////////////////////////////////////////////

{***********************************************************************************}
constructor TALDynamicListBoxItemContent.Create(const AOwner: TALDynamicListBoxItem);
begin
  inherited create(AOwner);
  BeginUpdate;
  Autosize := True;
  IsEphemeral := True;
end;

{***************************************************************************************}
constructor TALDynamicListBoxItemMainContent.Create(const AOwner: TALDynamicListBoxItem);
begin
  inherited create(AOwner);
  Align := TALAlignLayout.Top;
end;


{***************************************************************************************************************************}
constructor TALDynamicListBoxItemLoadingContent.TSkeletonAnimation.Create(const AOwner: TALDynamicListBoxItemLoadingContent);
Begin
  FOwner := AOwner;
  FAnimation := TALFloatAnimation.Create;
  FAnimation.Loop := True;
  FAnimation.AnimationType := TAnimationType.InOut;
  FAnimation.Interpolation := TALInterpolationType.Sinusoidal;
  FAnimation.OnProcess := AnimationProcess;
  FKind := TSkeletonAnimationKind.None;
  SetKind(TSkeletonAnimationKind.Pulse);
  FWaveColor := TalphaColors.White;
  FWaveAngle := 80;
  Inherited create;
end;

{************************************************************************}
destructor TALDynamicListBoxItemLoadingContent.TSkeletonAnimation.Destroy;
begin
  ALFreeAndNil(FAnimation);
  Inherited;
end;

{*************************************************************************************************************}
procedure TALDynamicListBoxItemLoadingContent.TSkeletonAnimation.SetKind(const AValue: TSkeletonAnimationKind);
begin
  If AValue <> FKind then begin
    FAnimation.Stop;
    FKind := AValue;
    case FKind of
      TALDynamicListBoxItemLoadingContent.TSkeletonAnimationKind.None: ;
      TALDynamicListBoxItemLoadingContent.TSkeletonAnimationKind.Pulse: begin
        FAnimation.Delay := 0.5;
        FAnimation.StartValue := 1.0;
        FAnimation.StopValue := 0.4;
        FAnimation.Duration := 1;
        FAnimation.AutoReverse := True;
      end;
      TALDynamicListBoxItemLoadingContent.TSkeletonAnimationKind.Wave: begin
        FAnimation.Delay := 0;
        FAnimation.StartValue := -0.5;
        FAnimation.StopValue := 1.5;
        FAnimation.Duration := 2;
        FAnimation.AutoReverse := False;
      end;
    end;
  end;
end;

{*************************************************************************************************}
procedure TALDynamicListBoxItemLoadingContent.TSkeletonAnimation.AnimationProcess(Sender: TObject);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure UpdateGradient(const AControl: TALDynamicListBoxControl);
  begin
    for var I := Low(AControl.FControls) to High(AControl.FControls) do begin
      UpdateGradient(AControl.FControls[i]);
      if not (AControl.FControls[i] is TALDynamicListBoxShape) then continue;
      var LShape := TALDynamicListBoxShape(AControl.FControls[i]);
      if LShape.Width <= 0 then continue;
      LShape.DoubleBuffered := False;
      var LShapeLeft: Single := Fowner.AbsoluteToLocal(LShape.LocalToAbsolute(TpointF.Create(0,0))).X;
      var LGradientCenter: Single := (1/LShape.Width) * ((TALFloatAnimation(Sender).CurrentValue * Fowner.Width) - LShapeLeft);
      var LGradientRadius: Single := (0.2 * Fowner.Width) / LShape.Width;
      var LGradient := LShape.Fill.Gradient;
      LGradient.Angle := Waveangle;
      LGradient.Style := TGradientStyle.Linear;
      if length(LGradient.Colors) = 0 then begin
        LGradient.Colors := [LShape.Fill.Color, WaveColor, LShape.Fill.Color];
        LShape.Fill.Color := TALphaColors.Null;
      end;
      LGradient.Offsets := [LGradientCenter - LGradientRadius, LGradientCenter, LGradientCenter + LGradientRadius];
    end;
  end;

begin
  Case Kind of
    TSkeletonAnimationKind.Pulse: Begin
      for var I := Low(FOwner.FControls) to High(FOwner.FControls) do
        FOwner.FControls[i].Opacity := TALFloatAnimation(Sender).CurrentValue;
    End;
    TSkeletonAnimationKind.Wave: Begin
      UpdateGradient(FOwner);
    End;
    else
      Raise Exception.Create('Error 3E43ABAE-7A19-41F7-A2A0-332020C068D8')
  end;
  FOwner.Repaint;
end;

{******************************************************************************************}
constructor TALDynamicListBoxItemLoadingContent.Create(const AOwner: TALDynamicListBoxItem);
begin
  inherited;
  Align := TALAlignLayout.Client;
  Autosize := False;
  FSkeletonAnimation := TSkeletonAnimation.Create(Self);
end;

{*****************************************************}
destructor TALDynamicListBoxItemLoadingContent.Destroy;
begin
  AlFreeAndNil(FSkeletonAnimation);
  inherited;
end;

{**************************************************************}
procedure TALDynamicListBoxItemLoadingContent.BeforeDestruction;
begin
  FSkeletonAnimation.Animation.Enabled := False;
  Inherited;
end;

{**************************************************}
procedure TALDynamicListBoxItemLoadingContent.Paint;
begin
  if FSkeletonAnimation.Kind <> TSkeletonAnimationKind.None then
    FSkeletonAnimation.Animation.Start;
  inherited;
end;

{*************************************************************************************************}
constructor TALDynamicListBoxItem.TDownloadDataContext.Create(const AOwner: TALDynamicListBoxItem);
begin
  inherited Create;
  Lock := TObject.Create;
  Owner := AOwner;
  FreeByThread := True;
  CustomParams := TALStringListW.Create;
  OnDownloadData := AOwner.OnDownloadData;
end;

{************************************************************}
destructor TALDynamicListBoxItem.TDownloadDataContext.Destroy;
begin
  ALFreeAndNil(Lock);
  ALFreeAndNil(CustomParams);
  inherited Destroy;
end;

{**************************************************************************************************}
constructor TALDynamicListBoxItem.TCreateContentContext.Create(const AOwner: TALDynamicListBoxItem);
begin
  inherited Create;
  Lock := TObject.Create;
  Owner := AOwner;
  FreeByThread := True;
  CustomParams := TALStringListW.Create;
  {$IF defined(debug)}
  if AOwner.OwnerListBox = nil then
    Raise Exception.Create('Error A377D825-737D-4FB5-8EAF-3A28680DAB6B');
  {$ENDIF}
  CacheEngine := AOwner.OwnerListBox.CacheEngine.IncreaseRefCount;
  TargetRect := AOwner.GetUnalignedSpace;
  OnCreateMainContent := AOwner.OnCreateMainContent;
  OnCreateErrorContent := AOwner.CreateErrorContent;
end;

{*************************************************************}
destructor TALDynamicListBoxItem.TCreateContentContext.Destroy;
begin
  ALFreeAndNil(Lock);
  ALFreeAndNil(CustomParams);
  CacheEngine.DecreaseRefCount;
  inherited Destroy;
end;

{**************************************************************}
constructor TALDynamicListBoxItem.Create(const AOwner: TObject);
begin
  inherited create(AOwner);
  AutoSize := True;
  Align := TALAlignLayout.Top;
  //IsEphemeral := False;
  //--
  FData := nil;
  FDownloadDataContext := nil;
  FDownloadDataErrorCode := 0;
  FOnDownloadData := nil;
  //--
  FCreateContentContext := nil;
  FBackgroundContent := nil;
  FMainContent := nil;
  FLoadingContent := nil;
  FErrorContent := nil;
  FForegroundContent := Nil;
  FOnCreateMainContent := nil;
  FOnCreateLoadingContent := nil;
  FOnCreateErrorContent := nil;
end;

{***************************************}
destructor TALDynamicListBoxItem.Destroy;
begin
  CancelDownloadData;
  CancelPreloadContent;
  AlFreeAndNil(FData);
  Inherited Destroy;
end;

{**************************************************************************************}
procedure TALDynamicListBoxItem.RemoveControl(const AControl: TALDynamicListBoxControl);
begin
  if AControl.OwnerControl <> Self then exit;
  if Acontrol = FBackgroundContent then FBackgroundContent := nil
  else if Acontrol = FMainContent then FMainContent := nil
  else if Acontrol = FLoadingContent then FLoadingContent := nil
  else if Acontrol = FErrorContent then FErrorContent := nil
  else if Acontrol = FForegroundContent then FForegroundContent := nil;
  inherited;
end;

{**************************************}
procedure TALDynamicListBoxItem.Prepare;
begin
  {$IFDEF DEBUG}
  ALLog(ClassName+'.Prepare', 'Index:' + ALintToStrW(Index));
  {$ENDIF}
  // FetchContent will invoke DownloadData, and its execution
  // may potentially be deferred until DownloadData is completed.
  FetchContent;
end;

{****************************************}
procedure TALDynamicListBoxItem.Unprepare;
begin
  {$IFDEF DEBUG}
  ALLog(ClassName+'.Unprepare', 'Index:' + ALintToStrW(Index));
  {$ENDIF}
  CancelPreloadContent;
  for Var I := High(Fcontrols) downto Low(Fcontrols) do
    If Fcontrols[i].IsEphemeral then begin
      var LControl := Fcontrols[i];
      ALFreeAndNil(LControl, true{ADelayed});
    end;
end;

{*************************************************************************************************}
function TALDynamicListBoxItem.FetchContent(const APreload: boolean): TALDynamicListBoxItemContent;
begin

  // If the item is outside the preloaded range, return nil
  if (OwnerView <> nil) and
     ((Index < OwnerView.FirstPreloadedItemIndex) or
      (Index > OwnerView.LastPreloadedItemIndex)) then exit(nil);

  // If the item is not visible, return nil
  if not Visible then exit(nil);

  // If data must be downloaded first, return loading content
  if DownloadData then begin
    result := FLoadingContent;
    if (Result <> nil) or
       (not CanCreateLoadingContent) or
       (APreload) then exit;
    CancelPreloadContent;
    ShiftLoadingContent(CreateLoadingContent);
    Result := FLoadingContent;
  end

  // Error Content
  else if FDownloadDataErrorCode <> 0 then begin
    result := FErrorContent;
    if (result <> nil) then exit;
    if not CanCreateErrorContent then Exit;
    if (not APreload) then begin
      CancelPreloadContent;
      FCreateContentContext := CreateContentCreationContext;
      Try
        ShiftErrorContent(CreateErrorContent(FCreateContentContext));
        Result := FErrorContent;
      Finally
        ALFreeAndNil(FCreateContentContext);
      End;
    end
    else begin
      if IsContentPreloading then exit;
      FCreateContentContext := CreateContentCreationContext;
      Try
        TALGraphicThreadPool.Instance.ExecuteProc(
          PreloadErrorContent, // const AProc: TALWorkerThreadObjProc;
          FCreateContentContext, // const AContext: Tobject; // Context will be free by the worker thread
          GetPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
      except
        ALFreeAndNil(FDownloadDataContext);
        Raise;
      End;
    end;
  end

  // Main Content
  else begin
    result := FMainContent;
    if (result <> nil) then exit;
    if not CanCreateMainContent then Exit;
    if (not APreload) then begin
      CancelPreloadContent;
      FCreateContentContext := CreateContentCreationContext;
      Try
        ShiftMainContent(CreateMainContent(FCreateContentContext));
        Result := FMainContent;
      Finally
        ALFreeAndNil(FCreateContentContext);
      End;
    end
    else begin
      if IsContentPreloading then exit;
      FCreateContentContext := CreateContentCreationContext;
      Try
        TALGraphicThreadPool.Instance.ExecuteProc(
          PreloadMainContent, // const AProc: TALWorkerThreadObjProc;
          FCreateContentContext, // const AContext: Tobject; // Context will be free by the worker thread
          GetPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
      except
        ALFreeAndNil(FDownloadDataContext);
        Raise;
      End;
    end;
  end;

end;

{************************************************************************}
function TALDynamicListBoxItem.FetchContent: TALDynamicListBoxItemContent;
begin
  Result := FetchContent(not IsVisibleWithinListboxBounds{APreload});
end;

{*****************************************************************************************}
function  TALDynamicListBoxItem.DownloadData(const AForceReload: Boolean = False): boolean;
begin

  // Exit if the last download resulted in an error, unless AForceReload is True
  // (e.g., when triggered by a "Reload" button click)
  if (not AForceReload) and
     (FDownloadDataErrorCode <> 0) then exit(False);

  // Exit if no data is needed
  if not CanDownloadData then exit(false);

  // Exit if a thread is already performing the task
  if IsDownloadDataRunning then exit(true);

  // Before starting the background thread
  {$IFDEF DEBUG}
  ALLog(ClassName+'.DownloadData', 'ForceReload:' + ALBoolToStrW(AForceReload));
  {$ENDIF}
  FDownloadDataErrorCode := 0;

  // Load the data in a separate thread to avoid blocking the calling thread
  FDownloadDataContext := CreateDownloadDataContext;
  Try
    TALNetHttpClientPool.Instance.ExecuteProc(
      DownloadDataBackgroundProc, // const AProc: TALWorkerThreadObjProc;
      FDownloadDataContext, // const AContext: Tobject; // Context will be free by the worker thread
      GetPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  except
    ALFreeAndNil(FDownloadDataContext);
    Raise;
  End;

  // Return True
  result := true;

end;

{*****************************************************************************}
function TALDynamicListBoxItem.CreateDownloadDataContext: TDownloadDataContext;
begin
  Result := TDownloadDataContext.Create(Self);
end;

{**************************************************************************************}
class procedure TALDynamicListBoxItem.DownloadDataBackgroundProc(var AContext: Tobject);
begin
  var LContext := TDownloadDataContext(AContext);
  if LContext.owner = nil then exit;
  try

    var LFreeData := True;
    var LData: TALJSONNodeW := nil;
    Try

      var LDownloadDataErrorCode: Integer := 0;
      DownloadDataBackgroundProcFetchData(LContext, LData, LDownloadDataErrorCode);

      if LContext.owner = nil then exit;
      DownloadDataBackgroundProcInitData(LContext, LDownloadDataErrorCode, LData);

      while not DownloadDataBackgroundProcCanProcessData(LContext) do begin
        if LContext.owner = nil then exit;
        sleep(250);
      end;

      if LContext.owner = nil then exit;
      TThread.queue(nil,
        procedure
        begin
          Try
            if LContext.owner <> nil then begin
              LContext.owner.DownloadDataProcessData(LContext, LDownloadDataErrorCode, LData);
              LContext.owner.FDownloadDataContext := nil;
              LContext.owner.DownloadDataFinished;
            end;
          finally
            ALFreeAndNil(LContext);
            ALfreeAndNil(LData);
          End;
        end);
      AContext := nil; // AContext will be free by TThread.queue
      LFreeData := False; // LData will be free by TThread.queue

    finally
      if LFreeData then
        ALfreeAndNil(LData);
    end;

  Except
    On E: Exception do begin
      ALLog('TALDynamicListBoxItem.DownloadDataBackgroundProc', E);
      TMonitor.Enter(LContext.Lock);
      try
        if LContext.Owner <> nil then begin
          LContext.FreeByThread := False;
          AContext := nil; // AContext will be free by CancelResourceDownload
        end;
      finally
        TMonitor.Exit(LContext.Lock);
      end;
    end;
  end;
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBoxItem.DownloadDataBackgroundProcFetchData(
                  const AContext: TDownloadDataContext;
                  out AData: TALJSONNodeW;
                  var AErrorCode: Integer);
begin
  Tmonitor.Enter(AContext.Lock);
  Try
    if AContext.owner = nil then exit;
    if not assigned(AContext.OnDownloadData) then
      Raise Exception.Create('Error DF2328CA-BCF7-46D6-B100-AFD222FF8873');
    var LMethod: TMethod;
    LMethod.Code := TMethod(AContext.OnDownloadData).Code;
    // Set Self to nil to prevent accidental access to instance members,
    // as we are in a multithreaded context where most members are not thread-safe.
    LMethod.Data := nil;
    TDownloadDataEvent(LMethod)(
      AContext, // const AContext: TDownloadDataContext;
      AData, // Const AData: TALJSONNodeW;
      AErrorCode); // var AErrorCode: Integer
  finally
    Tmonitor.Exit(AContext.Lock);
  End;
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBoxItem.DownloadDataBackgroundProcInitData(
                  const AContext: TDownloadDataContext;
                  const AErrorCode: Integer;
                  const AData: TALJSONNodeW);
begin
  if (AErrorCode = 0) and (AData <> nil) then begin
    AData.ChildNodes.SetSorted(true{Value},true{recurse});
    AData.MultiThreadPrepare(true{aOnlyChildList});
  end;
end;

{**************}
// [MultiThread]
class function TALDynamicListBoxItem.DownloadDataBackgroundProcCanProcessData(const AContext: TDownloadDataContext): boolean; // [MultiThread]
begin
  if TThread.Current.ThreadID = MainThreadID then exit(true);
  TMonitor.Enter(AContext.Lock);
  try
    // Primarily because we want to prevent the list
    // from being updated during the bottom-bound animation.
    result := (AContext.owner = nil) or
              (AContext.Owner.OwnerListBox = nil) or
              (not AContext.Owner.OwnerListBox.HasActiveScrollEngines);
  finally
    TMonitor.Exit(AContext.Lock);
  end;
end;

{******************************************************}
procedure TALDynamicListBoxItem.DownloadDataProcessData(
            const AContext: TDownloadDataContext;
            const AErrorCode: Integer;
            var AData: TALJSONNodeW);
begin
  FDownloadDataErrorCode := AErrorCode;
  if FDownloadDataErrorCode = 0 then begin
    ALfreeAndNil(FData);
    Fdata := AData;
    AData := nil;
    FOnDownloadData := Nil;
  end
  else ALfreeAndNil(AData);
end;

{***************************************************}
procedure TALDynamicListBoxItem.DownloadDataFinished;
begin
  if FDownloadDataErrorCode <> 0 then OwnerListBox.ShowErrorMessageBanner(FDownloadDataErrorCode)
  else FetchContent;
end;

{******************************************************}
function TALDynamicListBoxItem.CanDownloadData: Boolean;
begin
  // Once the data has been downloaded,
  // FOnDownloadData will be set to nil
  Result := assigned(FOnDownloadData);
end;

{************************************************************}
function TALDynamicListBoxItem.IsDownloadDataRunning: Boolean;
begin
  result := FDownloadDataContext <> nil;
end;

{************************************************************}
Function TALDynamicListBoxItem.HasDataBeenDownloaded: Boolean;
begin
  result := (not CanDownloadData) or
            (FDownloadDataErrorCode <> 0);
end;

{*************************************************}
procedure TALDynamicListBoxItem.CancelDownloadData;
begin
  // The FDownloadDataContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FDownloadDataContext <> nil then begin
    var LContextToFree: TDownloadDataContext;
    var LLock := FDownloadDataContext.lock;
    TMonitor.Enter(LLock);
    try
      if not FDownloadDataContext.FreeByThread then LContextToFree := FDownloadDataContext
      else LContextToFree := nil;
      FDownloadDataContext.Owner := nil;
      FDownloadDataContext := nil;
    Finally
      TMonitor.Exit(LLock);
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{*********************************************************************************}
function TALDynamicListBoxItem.CreateContentCreationContext: TCreateContentContext;
begin
  Result := TCreateContentContext.Create(self);
end;

{***************************************************}
procedure TALDynamicListBoxItem.CancelPreloadContent;
begin
  // The FCreateContentContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FCreateContentContext <> nil then begin
    var LContextToFree: TCreateContentContext;
    var LLock := FCreateContentContext.lock;
    TMonitor.Enter(LLock);
    try
      if not FCreateContentContext.FreeByThread then LContextToFree := FCreateContentContext
      else LContextToFree := nil;
      FCreateContentContext.Owner := nil;
      FCreateContentContext := nil;
    Finally
      TMonitor.Exit(LLock);
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{**********************************************************}
function TALDynamicListBoxItem.IsContentPreloading: Boolean;
begin
  result := FCreateContentContext <> nil;
end;

{************************************************************************************************}
procedure TALDynamicListBoxItem.ActivateCoreContent(const AContent: TALDynamicListBoxItemContent);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ToggleContent(var AMemberContent: TALDynamicListBoxItemContent);
  begin
    if (AMemberContent <> nil) and (AMemberContent <> AContent) then begin
      if AMemberContent.IsEphemeral then ALFreeAndNil(AMemberContent, true{ADelayed})
      else AMemberContent.Visible := False;
    end;
  end;

begin
  _ToggleContent(FMainContent);
  _ToggleContent(FLoadingContent);
  _ToggleContent(FErrorContent);
  AContent.Visible := True;
end;

{***********************************************************}
function TALDynamicListBoxItem.CanCreateMainContent: Boolean;
begin
  Result := assigned(FOnCreateMainContent);
end;

{************************************************************************}
procedure TALDynamicListBoxItem.PreloadMainContent(var AContext: Tobject);
begin
  var LContext := TCreateContentContext(AContext);
  if LContext.owner = nil then exit;
  try

    var LFreeMainContent := True;
    var LMainContent: TALDynamicListBoxItemMainContent := nil;
    Try

      LMainContent := CreateMainContent(LContext);

      if LContext.owner = nil then exit;
      If (LContext.Owner.OwnerView <> nil) and (LContext.Owner.OwnerView.Orientation = TOrientation.Horizontal) then LMainContent.Align := TALAlignLayout.Left
      else LMainContent.Align := TALAlignLayout.Top;
      LMainContent.EndUpdate;

      if LContext.owner = nil then exit;
      ALDynamicListBoxMakeBufDrawables(LMainContent, true{AEnsureDoubleBuffered});

      if LContext.owner = nil then exit;
      TThread.queue(nil,
        procedure
        begin
          Try
            if LContext.owner <> nil then begin
              LContext.owner.ShiftMainContent(LMainContent);
              LContext.owner.FCreateContentContext := nil;
              LMainContent := nil;
            end;
          finally
            ALfreeAndNil(LMainContent);
            ALFreeAndNil(LContext);
          End;
        end);
      AContext := nil; // AContext will be free by TThread.queue
      LFreeMainContent := False; // LMainContent will be free by TThread.queue

    finally
      if LFreeMainContent then
        ALfreeAndNil(LMainContent);
    end;

  Except
    On E: Exception do begin
      ALLog('TALDynamicListBoxItem.PreloadMainContent', E);
      TMonitor.Enter(LContext.Lock);
      try
        if LContext.Owner <> nil then begin
          LContext.FreeByThread := False;
          AContext := nil; // AContext will be free by CancelResourceDownload
        end;
      finally
        TMonitor.Exit(LContext.Lock);
      end;
    end;
  end;
end;

{************************************************************************************************************************}
function TALDynamicListBoxItem.CreateMainContent(const AContext: TCreateContentContext): TALDynamicListBoxItemMainContent;
begin
  Tmonitor.Enter(AContext.Lock);
  Try
    if AContext.owner = nil then exit(nil);
    if not assigned(AContext.OnCreateMainContent) then
      Raise Exception.Create('Error DF2328CA-BCF7-46D6-B100-AFD222FF8873');
    var LMethod: TMethod;
    LMethod.Code := TMethod(AContext.OnCreateMainContent).Code;
    // Set Self to nil to prevent accidental access to instance members,
    // as we are in a multithreaded context where most members are not thread-safe.
    LMethod.Data := nil;
    Result := TCreateMainContentEvent(LMethod)(AContext);
  finally
    Tmonitor.Exit(AContext.Lock);
  End;
end;

{*************************************************************************************************}
procedure TALDynamicListBoxItem.ShiftMainContent(const AContent: TALDynamicListBoxItemMainContent);
begin
  if FMainContent <> nil then
    Raise Exception.Create('Error 61728AC7-EF36-42F3-8BE4-075772DDBE12');
  FMainContent := AContent;
  If (OwnerView <> nil) and (OwnerView.Orientation = TOrientation.Horizontal) then FMainContent.Align := TALAlignLayout.Left
  else FMainContent.Align := TALAlignLayout.Top;
  FMainContent.EndUpdate;
  Insertcontrol(FMainContent, 0);
  ActivateCoreContent(FMainContent);
end;

{**************************************************************}
function TALDynamicListBoxItem.CanCreateLoadingContent: Boolean;
begin
  Result := assigned(FOnCreateLoadingContent);
end;

{***************************************************************************************}
function TALDynamicListBoxItem.CreateLoadingContent: TALDynamicListBoxItemLoadingContent;
begin
  if not assigned(FOnCreateLoadingContent) then
    Raise Exception.Create('Error 0B7B97DD-4A4F-4ADB-A7B2-EAEFC53F7348');
  var LContext := CreateContentCreationContext;
  Try
    result := FOnCreateLoadingContent(LContext);
  Finally
    ALFreeAndNil(LContext);
  End;
end;

{*******************************************************************************************************}
procedure TALDynamicListBoxItem.ShiftLoadingContent(const AContent: TALDynamicListBoxItemLoadingContent);
begin
  if FLoadingContent <> nil then
    Raise Exception.Create('Error A0EB1E0F-29C4-4035-AB6D-A1647993F8F9');
  FLoadingContent := AContent;
  FLoadingContent.EndUpdate;
  Insertcontrol(FLoadingContent, 0);
  ActivateCoreContent(FLoadingContent);
end;

{************************************************************}
function TALDynamicListBoxItem.CanCreateErrorContent: Boolean;
begin
  Result := assigned(FOnCreateErrorContent);
end;

{*************************************************************************}
procedure TALDynamicListBoxItem.PreloadErrorContent(var AContext: Tobject);
begin

end;

{**************************************************************************************************************************}
function TALDynamicListBoxItem.CreateErrorContent(const AContext: TCreateContentContext): TALDynamicListBoxItemErrorContent;
begin
  (* *)
  Result := Nil;
end;

{***************************************************************************************************}
procedure TALDynamicListBoxItem.ShiftErrorContent(const AContent: TALDynamicListBoxItemErrorContent);
begin

end;

{*******************************************************************}
function TALDynamicListBoxItem.IsVisibleWithinListboxBounds: Boolean;
begin
  if (not AbsoluteVisible) or (OwnerView = nil) then exit(False)
  else Result := ((index >= OwnerView.FirstVisibleItemIndex) and
                  (index <= OwnerView.LastVisibleItemIndex)) and
                 (OwnerView.IsVisibleWithinListboxBounds);
end;

{********************************************************************}
procedure TALDynamicListBoxItem.PaintInternal(const ACanvas: TCanvas);
begin
  FetchContent;
  Inherited;
end;

{***************************************************************************************}
constructor TALDynamicListBoxViewMainContent.Create(const AOwner: TALDynamicListBoxItem);
begin
  inherited create(AOwner);
  Align := TALAlignLayout.Client;
  Autosize := True;
  IsEphemeral := False;
  FOnRealign := nil;
end;

{*************************************************************************************************************************}
procedure TALDynamicListBoxViewMainContent.InsertItems(const AItems: TArray<TALDynamicListBoxItem>; const AIndex: Integer);
begin
  if Length(AItems) = 0 then
    Raise Exception.Create('InsertItems failed: No items provided');
  //--
  var LIndex := Max(0, Min(AIndex, High(FControls) + 1));
  if LIndex <= High(FControls) then begin
    Setlength(FControls, length(FControls) + 1);
    ALMove(FControls[LIndex], FControls[LIndex+Length(AItems)], (High(FControls) - LIndex) * SizeOf(Pointer));
    For var I := LIndex+Length(AItems) to High(FControls) do
      FControls[I].FIndex := I;
  end
  else
    Setlength(FControls, length(FControls) + Length(AItems));
  //--
  for var i := 0 to Length(AItems) - 1 do begin
    var LItem := AItems[i];
    {$IF defined(debug)}
    if LItem.OwnerControl <> nil then Raise Exception.Create('InsertItems integrity check failed: LItem.OwnerControl is not nil');
    If LItem.FUpdating <> FUpdating then raise Exception.Create('InsertItems integrity check failed: LItem.FUpdating mismatch');
    if LItem.AbsoluteCursor <> AbsoluteCursor then raise Exception.Create('InsertItems integrity check failed: AbsoluteCursor mismatch');
    if (not SameValue(AbsoluteOpacity, 1, TEpsilon.Scale)) or
       (not SameValue(LItem.AbsoluteOpacity, AbsoluteOpacity, TEpsilon.Scale)) then raise Exception.Create('InsertItems integrity check failed: AbsoluteOpacity mismatch');
    if LItem.AbsoluteEnabled <> AbsoluteEnabled then raise Exception.Create('InsertItems integrity check failed: AbsoluteEnabled mismatch');
    if LItem.AbsoluteVisible <> AbsoluteVisible then raise Exception.Create('InsertItems integrity check failed: AbsoluteVisible mismatch');
    {$ENDIF}
    FControls[LIndex + i] := LItem;
    LItem.FIndex := LIndex + i;
    LItem.FOwnerControl := Self;
    If LItem.ControlsCount > 0 then LItem.ParentChanged
    else begin
      LItem.FOwnerItem := FOwnerItem;
      LItem.FOwnerView := FOwnerView;
      LItem.FOwnerListBox := FOwnerListBox;
    end;
  end;
  //--
  Realign(LIndex);
end;

{*******************************************************************************}
procedure TALDynamicListBoxViewMainContent.DoRealign(const AStartIndex: integer);
begin

  if (FIsDestroying) or
     (FDisableAlign) or
     (controlsCount = 0) then exit;

  FDisableAlign := True;
  try

    // By default, items are top-aligned. You must provide an
    // OnRealign event to change this behavior.
    if assigned(FOnRealign) then FOnRealign(Self, AStartIndex)
    else begin
      var LCurrY: double;
      if AStartIndex <= 0 then LCurrY := 0
      else LCurrY := Fcontrols[AStartIndex - 1].bottom;

      for var I := AStartIndex to ControlsCount - 1 do begin
        Controls[i].SetBounds(0{X},LCurrY{Y}, Width{AWidth}, Controls[i].Height{AHeight});
        LCurrY := LCurrY + Controls[i].Height;
      end;
    end;

  finally
    FDisableAlign := False;
  end;

  AdjustSize;

  If OwnerView <> nil then
    OwnerView.UpdateScrollEngineLimits;

end;

{***************************************************}
procedure TALDynamicListBoxViewMainContent.DoRealign;
begin
  DoRealign(0);
end;

{*****************************************************************************}
procedure TALDynamicListBoxViewMainContent.Realign(const AStartIndex: integer);
begin
  if IsDestroying then
    Exit;
  if FDisableAlign then
    Exit;
  if IsUpdating then
    Exit;
  DoRealign(AStartIndex);
end;

{****************************************************}
procedure TALDynamicListBoxViewMainContent.AdjustSize;
begin
  If OwnerView <> nil then begin
    var LLastActiveItem: TALDynamicListBoxItem := OwnerView.FindLastActiveItem;
    if LLastActiveItem <> nil then begin
      if OwnerView.Orientation = TOrientation.Horizontal then Width := LLastActiveItem.Left + LLastActiveItem.Width + LLastActiveItem.Margins.Right
      else Height := LLastActiveItem.Top + LLastActiveItem.height + LLastActiveItem.Margins.bottom
    end;
  end;
end;

{*******************************************************}
procedure TALDynamicListBoxViewMainContent.ParentChanged;
begin
  inherited;
  if OwnerView <> nil then begin
    if OwnerView.Orientation = TOrientation.Horizontal then Align := TALAlignLayout.Vertical
    else Align := TALAlignLayout.horizontal;
  end
  else
    Align := TALAlignLayout.horizontal;
end;

{****************************************************************************}
function TALDynamicListBoxViewMainContent.GetFirstVisibleObjectIndex: Integer;
begin
  if OwnerView <> nil then Result := OwnerView.FFirstVisibleItemIndex
  else Result := Inherited;
end;

{***************************************************************************}
function TALDynamicListBoxViewMainContent.GetLastVisibleObjectIndex: Integer;
begin
  if OwnerView <> nil then Result := OwnerView.FLastVisibleItemIndex
  else Result := Inherited;
end;

{*******************************************************************}
function TALDynamicListBoxViewMainContent.PaintChildrenOnly: boolean;
begin
  Result := True;
end;

{**************************************************************************************************}
constructor TALDynamicListBoxView.TDownloadItemsContext.Create(const AOwner: TALDynamicListBoxView);
begin
  inherited Create;
  Lock := TObject.Create;
  Owner := AOwner;
  FreeByThread := True;
  ItemIdType := AOwner.FItemIdType;
  ItemIdNodeName := AOwner.ItemIdNodeName;
  MaxItems := AOwner.MaxItems;
  PaginationToken := AOwner.FPaginationToken;
  CustomParams := TALStringListW.Create;
  OnDownloadItems := AOwner.OnDownloadItems;
  OnCreateItem := AOwner.OnCreateItem;
  OnCreateItemMainContent := AOwner.OnCreateItemMainContent;
  OnCreateItemLoadingContent := AOwner.OnCreateItemLoadingContent;
  OnCreateItemErrorContent := AOwner.OnCreateItemErrorContent;
  OnDownloadItemData := AOwner.OnDownloadItemData;
end;

{*************************************************************}
destructor TALDynamicListBoxView.TDownloadItemsContext.Destroy;
begin
  ALFreeAndNil(Lock);
  ALFreeAndNil(CustomParams);
  inherited Destroy;
end;

{**************************************************************************************************}
constructor TALDynamicListBoxView.TCreateContentContext.Create(const AOwner: TALDynamicListBoxView);
begin
  inherited create(AOwner);
  OnCreateNoItemsContent := AOwner.OnCreateNoItemsContent;
end;

{**************************************************************}
constructor TALDynamicListBoxView.Create(const AOwner: TObject);
begin
  inherited create(AOwner);
  AutoSize := False;
  {$IFDEF DEBUG}
  fDebugFpsStarted := 0;
  fDebugFpsCount := 0;
  //fDebugFpsStopWatch
  //fDebugFpsRenderTimeStopWatch
  fDebugAverageFpsCount := 0;
  fDebugAverageFps := 0;
  {$ENDIF}
  FOrientation := TOrientation.Vertical;
  FItemIdType := TItemIdType.Unknown;
  fScrollCapturedByMe := False;
  FHandleMouseEvents := False;
  FMouseDownPos := TPointF.Zero;
  FNoItemsContent := nil;
  FTopBarContent := nil;
  FBottomBarContent := Nil;
  FItems := nil;
  FItemIdNodeName := 'id';
  FUniqueInt64ItemIds := TDictionary<Int64, boolean>.create;
  FUniqueTextItemIds := TDictionary<String, boolean>.create;
  FMaxItems := MaxInt;
  FPreloadItemCount := 10;
  //--
  FScrollEngine := TALScrollEngine.Create;
  {$IF (not defined(DEBUG)) and (defined(MSWINDOWS) or defined(ALMacOS))}
  FScrollEngine.TouchTracking := [];
  {$ELSE}
  FScrollEngine.TouchTracking := [ttVertical];
  {$ENDIF}
  FScrollEngine.OnChanged := ScrollEngineChanged;
  FScrollEngine.OnStart := ScrollEngineStart;
  FScrollEngine.OnStop := ScrollEngineStop;
  //--
  FScrollBar := nil;
  //--
  FFirstVisibleItemIndex := 0; // = Low(FItems^) when FItems is empty
  FLastVisibleItemIndex := -1; // = High(FItems^) when FItems is empty
  FFirstPreloadedItemIndex := 0; // = Low(FItems^) when FItems is empty
  FLastPreloadedItemIndex := -1; // = High(FItems^) when FItems is empty
  FPaginationToken := #0;
  FTriggerDownloadItemsAtIndex := -maxint;
  FDownloadItemsErrorCode := 0;
  FDownloadItemsContext := nil;
  FOnDownloadItems := nil;
  FOnCreateItem := nil;
  FOnCreateItemMainContent := nil;
  FOnCreateItemLoadingContent := nil;
  FOnCreateItemErrorContent := nil;
  FOnDownloadItemData := nil;
  FOnCreateNoItemsContent := nil;
end;

{***************************************}
destructor TALDynamicListBoxView.Destroy;
begin
  CancelDownloadItems;
  ALFreeAndNil(FUniqueInt64ItemIds);
  ALFreeAndNil(FUniqueTextItemIds);
  ALFreeAndNil(FScrollEngine);
  inherited;
end;

{**************************************************************************************}
procedure TALDynamicListBoxView.RemoveControl(const AControl: TALDynamicListBoxControl);
begin
  if AControl.OwnerControl <> Self then exit;
  if Acontrol = FNoItemsContent then FNoItemsContent := nil
  else if Acontrol = FTopBarContent then FTopBarContent := nil
  else if Acontrol = FBottomBarContent then FBottomBarContent := nil;
  inherited;
end;

{**************************************}
procedure TALDynamicListBoxView.Prepare;
begin
  inherited;
  // SetViewportPosition will prepare all items
  // and also invoke DownloadItems.
  SetViewportPosition(ViewportPosition);
end;

{****************************************}
procedure TALDynamicListBoxView.Unprepare;
begin
  inherited;
  if Fitems <> nil then
    for Var I := High(FItems^) downto Low(FItems^) do
      FItems^[i].Unprepare;
end;

{*************************************************************************************************}
function TALDynamicListBoxView.FetchContent(const APreload: boolean): TALDynamicListBoxItemContent;
begin

  Result := nil;

  // If the item is outside the preloaded range, return nil
  if (OwnerView <> nil) and
     ((Index < OwnerView.FirstPreloadedItemIndex) or
      (Index > OwnerView.LastPreloadedItemIndex)) then exit(nil);

  // If the item is not visible, return nil
  if not Visible then exit;

  // If data must be downloaded first, return loading content
  if DownloadData then begin
    result := FLoadingContent;
    if (Result <> nil) or
       (not CanCreateLoadingContent) or
       (APreload) then exit;
    CancelPreloadContent;
    ShiftLoadingContent(CreateLoadingContent);
    Result := FLoadingContent;
  end

  // If the items have never been downloaded, download them
  // first and return the loading content
  else if (FMainContent = nil) and (DownloadItems) then begin
    result := FLoadingContent;
    if (Result <> nil) or
       (not CanCreateLoadingContent) or
       (APreload) then exit;
    CancelPreloadContent;
    ShiftLoadingContent(CreateLoadingContent);
    Result := FLoadingContent;
  end

  // If the items were never successfully downloaded and an
  // error occurred during the last download, return the error content
  else if (FMainContent = nil) and (FDownloadDataErrorCode <> 0) then begin
    result := FErrorContent;
    if (result <> nil) then exit;
    if not CanCreateErrorContent then Exit;
    if (not APreload) then begin
      CancelPreloadContent;
      FCreateContentContext := CreateContentCreationContext;
      Try
        ShiftErrorContent(CreateErrorContent(FCreateContentContext));
        Result := FErrorContent;
      Finally
        ALFreeAndNil(FCreateContentContext);
      End;
    end
    else begin
      if IsContentPreloading then exit;
      FCreateContentContext := CreateContentCreationContext;
      Try
        TALGraphicThreadPool.Instance.ExecuteProc(
          PreloadErrorContent, // const AProc: TALWorkerThreadObjProc;
          FCreateContentContext, // const AContext: Tobject; // Context will be free by the worker thread
          GetPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
      except
        ALFreeAndNil(FDownloadDataContext);
        Raise;
      End;
    end;
  end

  // Main Content
  else begin
    result := FMainContent;
    if (result <> nil) then exit;
    if not CanCreateMainContent then Exit;
    if (not APreload) then begin
      CancelPreloadContent;
      FCreateContentContext := CreateContentCreationContext;
      Try
        ShiftMainContent(CreateMainContent(FCreateContentContext));
        Result := FMainContent;
      Finally
        ALFreeAndNil(FCreateContentContext);
      End;
    end
    else begin
      if IsContentPreloading then exit;
      FCreateContentContext := CreateContentCreationContext;
      Try
        TALGraphicThreadPool.Instance.ExecuteProc(
          PreloadMainContent, // const AProc: TALWorkerThreadObjProc;
          FCreateContentContext, // const AContext: Tobject; // Context will be free by the worker thread
          GetPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
      except
        ALFreeAndNil(FDownloadDataContext);
        Raise;
      End;
    end;
  end;

end;

{*****************************************************************************}
function TALDynamicListBoxView.GetOnCreateMainContent: TCreateMainContentEvent;
begin
  Result := TCreateMainContentEvent(inherited OnCreateMainContent);
end;

{********************************************************************************************}
procedure TALDynamicListBoxView.SetOnCreateMainContent(const AValue: TCreateMainContentEvent);
begin
  inherited OnCreateMainContent := TALDynamicListBoxItem.TCreateMainContentEvent(AValue);
end;

{*************************************************************************}
procedure TALDynamicListBoxView.SetOrientation(const AValue: TOrientation);
begin
  if FOrientation <> AValue then begin
    FOrientation := AValue;
    If FMainContent <> nil then begin
      if FOrientation = TOrientation.Horizontal then FMainContent.Align := TALAlignLayout.Vertical
      else FMainContent.Align := TALAlignLayout.horizontal;
    end;
  end;
end;

{********************************************************************************************************}
procedure TALDynamicListBoxView.InternalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.MouseDown',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if (Button = TMouseButton.mbLeft) then begin
    FHandleMouseEvents := true;
    fMouseDownPos := TpointF.Create(X,Y);
    ScrollEngine.MouseDown(X, Y);
  end;
end;

{**********************************************************************************}
procedure TALDynamicListBoxView.InternalMouseMove(Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.InternalMouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if FHandleMouseEvents then begin
    if (not fScrollCapturedByMe) and
       (((ttHorizontal in fScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.x - x) > abs(fMouseDownPos.y - y)) and
         (abs(fMouseDownPos.x - x) > TALScrollEngine.DefaultTouchSlop)) or
        ((ttVertical in fScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.y - y) > abs(fMouseDownPos.x - x)) and
         (abs(fMouseDownPos.y - y) > TALScrollEngine.DefaultTouchSlop))) then begin
      {$IFDEF DEBUG}
      //ALLog(
      //  ClassName + '.InternalMouseMove',
      //  'ScrollCapturedByMe');
      {$ENDIF}
      fScrollCapturedByMe := True;
      TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(True));
    end;
    ScrollEngine.MouseMove(X, Y);
  end;
end;

{******************************************************************************************************}
procedure TALDynamicListBoxView.InternalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.InternalMouseUp',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if FHandleMouseEvents and (Button = TMouseButton.mbLeft) then begin
    FScrollCapturedByMe := False;
    ScrollEngine.MouseUp(X, Y);
    FHandleMouseEvents := False;
  end;
end;

{*************************************************}
procedure TALDynamicListBoxView.InternalMouseLeave;
begin
  {$IFDEF DEBUG}
  //ALLog(ClassName + '.InternalMouseLeave');
  {$ENDIF}
  if FHandleMouseEvents then begin
    FScrollCapturedByMe := False;
    ScrollEngine.MouseLeave;
    FHandleMouseEvents := False;
  end;
end;

{************************************************************************************************}
procedure TALDynamicListBoxView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  InternalMouseDown(Button, Shift, X, Y);
end;

{**************************************************************************}
procedure TALDynamicListBoxView.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  InternalMouseMove(Shift, X, Y);
  inherited;
end;

{**********************************************************************************************}
procedure TALDynamicListBoxView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  InternalMouseUp(Button, Shift, X, Y);
end;

{*****************************************}
procedure TALDynamicListBoxView.MouseLeave;
begin
  inherited;
  InternalMouseLeave;
end;

{*************************************************************************************************************************************************}
procedure TALDynamicListBoxView.ChildrenMouseDown(const AObject: TALDynamicListBoxControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not aObject.AutoCapture then AObject.capture;
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseDown(Button, Shift, P.X, P.Y);
  inherited;
end;

{***************************************************************************************************************************}
procedure TALDynamicListBoxView.ChildrenMouseMove(const AObject: TALDynamicListBoxControl; Shift: TShiftState; X, Y: Single);
begin
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseMove(Shift, P.X, P.Y);
  inherited;
end;

{***********************************************************************************************************************************************}
procedure TALDynamicListBoxView.ChildrenMouseUp(const AObject: TALDynamicListBoxControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not aObject.AutoCapture then aObject.releasecapture;
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseUp(Button, Shift, P.X, P.Y);
  inherited;
end;

{******************************************************************************************}
procedure TALDynamicListBoxView.ChildrenMouseLeave(const AObject: TALDynamicListBoxControl);
begin
  InternalMouseLeave;
  inherited;
end;

{*******************************************************************}
procedure TALDynamicListBoxView.ScrollEngineChanged(Sender: TObject);
begin
  {$IFDEF DEBUG}
  //ALLog(Classname+'.ScrollEngineChanged');
  {$ENDIF}
  SetViewportPosition(ScrollEngine.ViewportPosition);
end;

{*****************************************************************}
procedure TALDynamicListBoxView.ScrollEngineStart(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.ScrollEngineStart');
  {$ENDIF}
  If ownerListBox <> nil then
    inc(ownerListBox.FActiveScrollEnginesCount);
end;

{****************************************************************}
procedure TALDynamicListBoxView.ScrollEngineStop(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.ScrollEngineStop');
  {$ENDIF}
  If ownerListBox <> nil then begin
    dec(ownerListBox.FActiveScrollEnginesCount);
    {$IFDEF DEBUG}
    if ownerListBox.FActiveScrollEnginesCount < 0 then
      Raise Exception.Create('Error E277A8CF-210A-4C0F-8427-C0FF34C470FE');
    {$ENDIF}
  end;
end;

{*******************************************************}
procedure TALDynamicListBoxView.UpdateScrollEngineLimits;
begin
  ScrollEngine.MinScrollLimit := TALPointD.Create(0,0);
  if FMainContent = nil then ScrollEngine.MaxScrollLimit := TALPointD.Create(0,0)
  else if Orientation = TOrientation.Horizontal then ScrollEngine.MaxScrollLimit := TALPointD.Create(max(0,FMainContent.Width - Width), 0)
  else ScrollEngine.MaxScrollLimit := TALPointD.Create(0, max(0,FMainContent.Height - height));
end;

{****************************************************************}
function TALDynamicListBoxView.FindFirstVisibleItemIndex: integer;
begin

  //
  // -4 -     -
  // -3 -     -
  // -2 -     -
  // -1 -     -                     <= this pixel is NOT drawn
  //  0 +++++++ (FViewportPosition) <= this pixel is drawn
  //  1 +++++++                     <= this pixel is drawn
  //  2 +++++++                     <= this pixel is drawn
  //  3 -     - (Owner.Height)      <= this pixel is NOT drawn
  //  4 -     -
  //

  //----- 0 if FItems is empty
  Result := Max(
              Low(FItems^){0 if empty},
              Min(
                High(FItems^){-1 if empty},
                FFirstVisibleItemIndex));

  //
  //                         x   x   x
  //                           x   x
  // FirstVisibleItemIndex > x   x   x
  //                           x   x
  //                         x   x   x < LastVisibleItemIndex
  //                           x   x
  //                         x   x   x
  //
  If Orientation = TOrientation.horizontal then begin
    var LPos: Double := ViewportPosition.X;
    var LTMPIdx := Result;
    while (LTMPIdx >= Low(FItems^)) and
          (LTMPIdx <= High(FItems^)) do begin
      if FItems^[LTMPIdx].Visible then begin
        if compareValue(FItems^[LTMPIdx].right, LPos, TEpsilon.Position) >= 0 then Result := LTMPIdx
        else break;
      end;
      dec(LTMPIdx);
    end;
    //--
    while (Result < High(FItems^)) and
          ((not FItems^[Result].Visible) or
           (compareValue(FItems^[Result].right, LPos, TEpsilon.Position) < 0)) do
      inc(Result);
  end
  else begin
    var LPos: Double := ViewportPosition.Y;
    var LTMPIdx := Result;
    while (LTMPIdx >= Low(FItems^)) and
          (LTMPIdx <= High(FItems^)) do begin
      if FItems^[LTMPIdx].Visible then begin
        if compareValue(FItems^[LTMPIdx].bottom, LPos, TEpsilon.Position) >= 0 then Result := LTMPIdx
        else break;
      end;
      dec(LTMPIdx);
    end;
    //--
    while (Result < High(FItems^)) and
          ((not FItems^[Result].Visible) or
           (compareValue(FItems^[Result].bottom, LPos, TEpsilon.Position) < 0)) do
      inc(Result);
  end;

  // Consistency check only in debug
  {$IF defined(DEBUG)}
  if (Result < Low(FItems^)) then
    raise Exception.Create('Error FBAEF066-CFC7-4B63-96BA-15A1F0C8D827');
  {$ENDIF}

end;

{***************************************************************}
function TALDynamicListBoxView.FindLastVisibleItemIndex: integer;
begin

  //
  // -4 -     -
  // -3 -     -
  // -2 -     -
  // -1 -     -                     <= this pixel is NOT drawn
  //  0 +++++++ (FViewportPosition) <= this pixel is drawn
  //  1 +++++++                     <= this pixel is drawn
  //  2 +++++++                     <= this pixel is drawn
  //  3 -     - (Owner.Height)      <= this pixel is NOT drawn
  //  4 -     -
  //

  //----- -1 if FItems is empty
  Result := Min(
              High(FItems^){-1 if empty},
              Max(
                Low(FItems^){0 if empty},
                FLastVisibleItemIndex));

  //
  //                       x   x   x
  //                         x   x
  // FirstVisibleItemIdx > x   x   x
  //                         x   x
  //                       x   x   x < LastVisibleItemIdx
  //                         x   x     ------------------
  //                       x   x   x
  //
  If Orientation = TOrientation.horizontal then begin
    var LPos: Double := ViewportPosition.X + Width;
    var LTMPIdx := Result;
    while (LTMPIdx >= Low(FItems^)) and
          (LTMPIdx <= High(FItems^)) do begin
      if FItems^[LTMPIdx].Visible then begin
        if compareValue(FItems^[LTMPIdx].left, LPos, TEpsilon.Position) < 0 then Result := LTMPIdx
        else break;
      end;
      inc(LTMPIdx);
    end;
    //--
    while (Result > Low(FItems^)) and
          ((not FItems^[Result].Visible) or
           (compareValue(FItems^[Result].left, LPos, TEpsilon.Position) >= 0)) do
      dec(Result);
  end
  else begin
    var LPos: Double := ViewportPosition.Y + Height;
    var LTMPIdx := Result;
    while (LTMPIdx >= Low(FItems^)) and
          (LTMPIdx <= High(FItems^)) do begin
      if FItems^[LTMPIdx].Visible then begin
        if compareValue(FItems^[LTMPIdx].top,  LPos, TEpsilon.Position) < 0 then Result := LTMPIdx
        else break;
      end;
      inc(LTMPIdx);
    end;
    //--
    while (Result > Low(FItems^)) and
          ((not FItems^[Result].Visible) or
           (compareValue(FItems^[Result].top,  LPos, TEpsilon.Position) >= 0)) do
      dec(Result);
  end;

  // Consistency check only in debug
  {$IF defined(DEBUG)}
  if (Result > High(FItems^)) then
    raise Exception.Create('Error 399D434C-C527-476A-B49D-33CFDAB30D86');
  {$ENDIF}

end;

{***********************************************************************}
function TALDynamicListBoxView.FindLastActiveItem: TALDynamicListBoxItem;
begin
  Result := nil;
  if (Fitems <> nil) then
    For var I := high(Fitems^) downto low(Fitems^) do
      if Fitems^[i].Visible then begin
        Result := Fitems^[i];
        break;
      end;
end;

{*******************************************************************}
function TALDynamicListBoxView.IsVisibleWithinListboxBounds: Boolean;
begin
  if not AbsoluteVisible then
    exit(false)
  else if OwnerView = nil then
    result := (OwnerListBox <> nil) and
              (OwnerListBox.MainView = Self)
  else
    Result := ((index >= OwnerView.FirstVisibleItemIndex) and
               (index <= OwnerView.LastVisibleItemIndex)) and
              (OwnerView.IsVisibleWithinListboxBounds);
end;

{********************************************************************}
procedure TALDynamicListBoxView.PaintInternal(const ACanvas: TCanvas);
begin
  {$IFDEF DEBUG}
  LogFPS;
  {$ENDIF}
  inherited;
end;

{************}
{$IFDEF DEBUG}
procedure TALDynamicListBoxView.LogFPS;
begin

  if ScrollEngine.TouchTracking = [] then exit;

  fDebugFpsRenderTimeStopWatch.stop;
  if (fDebugFpsStarted = 2) and
     (not ScrollEngine.down) and
     (abs(ScrollEngine.CurrentVelocity.y) > 100) and
     (fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds > 1500{1000*1.5} / DisplayDefaultRefreshRate) then begin
    ALLog(
      classname + '.fps',
      'Drop frame detected | '  +
      ALFormatFloatW('0.00', fDebugFpsRenderTimeStopWatch.Elapsed.totalMilliseconds, ALDefaultFormatSettingsW) + ' | '+
      'Velocity: ' + ALFormatFloatW('0', ScrollEngine.CurrentVelocity.y, ALDefaultFormatSettingsW),
      TalLogType.warn);
  end;

  if (abs(ScrollEngine.CurrentVelocity.y) > 100) and (not ScrollEngine.down) then begin
    // When the user moves the mouse, the repaint is triggered by the mousemove event.
    // However, when the user stops touching the screen, the repaint is triggered by
    // the ScrollEngine Choreographer. The issue is that these two events do not occur
    // at the same time. As a result, when the user stops touching the screen and the
    // ScrollEngine Choreographer takes over to request a repaint, there may be a delay
    // of up to one frame. This is why we need to ignore the very first frame after
    // the user stops touching the screen.
    if fDebugFpsStarted = 0 then begin
      fDebugFpsStarted := 1;
    end
    else if fDebugFpsStarted = 1 then begin
      fDebugFpsStarted := 2;
      fDebugFpsCount := 0;
      fDebugFpsStopWatch := TstopWatch.StartNew;
    end
    else begin
      inc(fDebugFpsCount);
    end;
  end
  else if fDebugFpsStarted = 1 then begin
    fDebugFpsStarted := 0;
  end
  else if fDebugFpsStarted = 2 then begin
    fDebugFpsStopWatch.stop;
    inc(fDebugFpsCount);
    if fDebugFpsStopWatch.Elapsed.totalMilliseconds > 0 then begin
      fDebugAverageFps := ((fDebugAverageFps * fDebugAverageFpsCount) + ((fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000)) / (fDebugAverageFpsCount + 1);
      inc(fDebugAverageFpsCount);
      ALLog(
        classname + '.fps',
        ALFormatFloatW('0.##', (fDebugFpsCount / fDebugFpsStopWatch.Elapsed.totalMilliseconds) * 1000, ALDefaultFormatSettingsW) + ' fps' + ' | ' +
        'Average: ' + ALFormatFloatW('0.##', fDebugAverageFps, ALDefaultFormatSettingsW) + ' fps' + ' | ' +
        'System default: ' + ALFormatFloatW('0.##', DisplayDefaultRefreshRate, ALDefaultFormatSettingsW) + ' fps',
        TalLogType.verbose);
    end;
    fDebugFpsStarted := 0;
  end;
  fDebugFpsRenderTimeStopWatch := TstopWatch.StartNew;

end;
{$ENDIF}

{************************************************************}
function TALDynamicListBoxView.GetViewportPosition: TALPointD;
begin
  If FMainContent = nil then Result := TALPointD.create(0,0)
  else Result := TALPointD.create(-FMainContent.left, -FMainContent.top);
end;

{***************************************************************************}
procedure TALDynamicListBoxView.SetViewportPosition(const AValue: TALPointD);
begin

  {$REGION 'Update MainContent position'}
  if FItems = nil then exit;
  FMainContent.SetPosition(-AValue);
  {$ENDREGION}

  {$REGION 'calculate FFirstVisibleItemIndex/FLastVisibleItemIndex'}
  FFirstVisibleItemIndex := FindFirstVisibleItemIndex;
  FLastVisibleItemIndex := FindLastVisibleItemIndex;
  {$ENDREGION}

  {$REGION 'calculate FFirstPreloadedItemIndex/FLastPreloadedItemIndex'}
  var LOldFirstPreloadedItemIndex := FFirstPreloadedItemIndex;
  var LOldLastPreloadedItemIndex := FLastPreloadedItemIndex;
  FFirstPreloadedItemIndex := Max(Low(FItems^){0 if empty}, FFirstVisibleItemIndex - PreloadItemCount);
  FLastPreloadedItemIndex := Min(High(FItems^){-1 if empty}, FLastVisibleItemIndex + PreloadItemCount);

  // Consistency check only in debug
  {$IF defined(DEBUG)}
  if (LOldFirstPreloadedItemIndex < Low(FItems^)) or
     (LOldLastPreloadedItemIndex > High(FItems^)) or
     (FFirstPreloadedItemIndex < Low(FItems^)) or
     (FLastPreloadedItemIndex > High(FItems^)) then
    raise Exception.Create('Error FD682464-DBB8-4D50-B31F-F8AEFCEBBA15');
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'offset preload'}

  //
  // (A)   LOldLastPreloadedItemIndex
  // (A)   | LOldFirstPreloadedItemIndex
  //       | |
  // (B)   | |               | LOldFirstPreloadedItemIndex
  // (B)   | |               |         | LOldLastPreloadedItemIndex
  //       | |               |---------|
  //      -1 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
  // (1)                   7 |---------| 14
  // (2)                        10 |-----------| 18
  // (3)                     8 |-----| 13
  // (4)                 6 |---| 10
  // (5)           3 |-------| 9
  // (6)                            12 |---| 16
  // (7)  -1 |-| 2
  // (8)                                    16 |-----| 21
  // (9)  -1 | 1
  // (10)                                         19 | 21
  //

  //delete bitmap that get out of the scope on the left
  //----->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  //FItems is empty: for I := 0 to -1
  //(A1) for i := 0 to 7      (B1) for i := 8 to 7
  //(A2) for i := 0 to 10     (B2) for i := 8 to 10
  //(A3) for i := 0 to 8      (B3) for i := 8 to 8
  //(A4) for i := 0 to 6      (B4) for i := 8 to 6
  //(A5) for i := 0 to 3      (B5) for i := 8 to 3
  //(A6) for i := 0 to 12     (B6) for i := 8 to 12
  //(A7) for i := 0 to -1     (B7) for i := 8 to -1
  //(A8) for i := 0 to 16     (B8) for i := 8 to 16
  //(A9) for i := 0 to -1     (B9) for i := 8 to -1
  //(A10) for i := 0 to 19    (B10) for i := 8 to 19
  for var I := Max(Low(FItems^){0 if empty}, LOldFirstPreloadedItemIndex) to Min(High(FItems^){-1 if empty}, FFirstPreloadedItemIndex - 1) do
    FItems^[i].Unprepare;

  //delete bitmap that get out of the scope on the right
  //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<-----
  //FItems is empty: for I := -1 downto 0
  //(A1) for i := -1 downto 14     (B1) for i := 13 downto 14
  //(A2) for i := -1 downto 18     (B2) for i := 13 downto 18
  //(A3) for i := -1 downto 13     (B3) for i := 13 downto 13
  //(A4) for i := -1 downto 10     (B4) for i := 13 downto 10
  //(A5) for i := -1 downto 9      (B5) for i := 13 downto 9
  //(A6) for i := -1 downto 16     (B6) for i := 13 downto 16
  //(A7) for i := -1 downto 2      (B7) for i := 13 downto 2
  //(A8) for i := -1 downto 21     (B8) for i := 13 downto 21
  //(A9) for i := -1 downto 1      (B9) for i := 13 downto 1
  //(A10) for i := -1 downto 21    (B10) for i := 13 downto 21
  for var I := Min(High(FItems^){-1 if empty}, LOldLastPreloadedItemIndex) downto Max(Low(FItems^){0 if empty}, FLastPreloadedItemIndex + 1) do
    FItems^[i].Unprepare;

  //when ATriggeredByScrollEvent=false we boot of all items between
  //FFirstPreloadedItemIndex and FLastPreloadedItemIndex
  //to handle the case when we insert some items in the middle of
  //FItems
(*
  if ATriggeredByScrollEvent then begin
*)

    //add bitmap that enter in the scope on the right
    //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>+++++
    //(A1) for i := 8 to -1      (B1) for i := 8 to 7
    //(A2) for i := 11 to -1     (B2) for i := 11 to 7
    //(A3) for i := 9 to -1      (B3) for i := 9 to 7
    //(A4) for i := 7 to -1      (B4) for i := 7 to 7
    //(A5) for i := 4 to -1      (B5) for i := 4 to 7
    //(A6) for i := 13 to -1     (B6) for i := 13 to 7
    //(A7) for i := 0 to -1      (B7) for i := 0 to 1
    //(A8) for i := 17 to -1     (B8) for i := 17 to 7
    //(A9) for i := 0 to -1      (B9) for i := 0 to 0
    //(A10) for i := 20 to -1    (B10) for i := 20 to 7
    for var I := Max(Low(FItems^){0 if empty}, FFirstPreloadedItemIndex) to MinIntValue([High(FItems^){-1 if empty}, FLastPreloadedItemIndex, LoldFirstPreloadedItemIndex - 1]) do begin
(*      if FPostponedViewportPositionSet then break;   *)
      FItems^[i].prepare;
    end;

    //add bitmap that enter in the scope on the left
    //+++++<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    //(A1) for i := 13 downto 8      (B1) for i := 13 downto 14
    //(A2) for i := 17 downto 11     (B2) for i := 17 downto 14
    //(A3) for i := 12 downto 9      (B3) for i := 12 downto 14
    //(A4) for i := 9 downto 7       (B4) for i := 9 downto 14
    //(A5) for i := 8 downto 4       (B5) for i := 8 downto 14
    //(A6) for i := 15 downto 13     (B6) for i := 15 downto 14
    //(A7) for i := 1 downto 0       (B7) for i := 1 downto 14
    //(A8) for i := 20 downto 17     (B8) for i := 20 downto 17
    //(A9) for i := 0 downto 0       (B9) for i := 0 downto 14
    //(A10) for i := 20 downto 20    (B10) for i := 20 downto 20
    for var I := Min(High(FItems^){-1 if empty}, FLastPreloadedItemIndex) downto MaxIntValue([Low(FItems^){0 if empty}, FFirstPreloadedItemIndex, LoldLastPreloadedItemIndex + 1]) do begin
(*      if FPostponedViewportPositionSet then break;  *)
      FItems^[i].prepare;
    end;

(*
  end
  else begin

    //Enforce all EphemeralLayout
    //(A1) for i := 8 to 13       (B1) for i := 8 to 13
    //(A2) for i := 11 to 17      (B2) for i := 11 to 17
    //(A3) for i := 9 to 12       (B3) for i := 9 to 12
    //(A4) for i := 7 to 9        (B4) for i := 7 to 9
    //(A5) for i := 4 to 8        (B5) for i := 4 to 8
    //(A6) for i := 13 to 15      (B6) for i := 13 to 15
    //(A7) for i := 0 to 1        (B7) for i := 0 to 1
    //(A8) for i := 17 to 20      (B8) for i := 17 to 20
    //(A9) for i := 0 to 0        (B9) for i := 0 to 0
    //(A10) for i := 20 to 20     (B10) for i := 20 to 20
    for var I := Max(Low(FItems^){0 if empty}, FFirstPreloadedItemIndex) to Min(High(FItems^){-1 if empty}, FLastPreloadedItemIndex) do begin
      if FPostponedViewportPositionSet then break;
      FItems^[i].boot;
    end;

  end;
*)
  {$ENDREGION}

  {$REGION 'Log'}
  {$IFDEF DEBUG}
  {-----
  ALLog(
    Classname+'.SetViewportPosition',
    'Value: '+ ALFloatToStrW(aValue, ALDefaultFormatSettingsW) + ' | '+
    'EnforceScrollLimits:' + ALBoolToStrW(AEnforceScrollLimits) + ' | '+
    'TriggeredByScrollEvent:' + ALBoolToStrW(ATriggeredByScrollEvent) + ' | '+
    'Postponed: False');
  -----}
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'DownloadItems'}
  DownloadItems;
  {$ENDREGION}

  {$REGION 'repaint'}
  Repaint;
  {$ENDREGION}

end;

{******************************************}
procedure TALDynamicListBoxView.LockItemIds;
begin
  Tmonitor.enter(FUniqueInt64ItemIds);
end;

{********************************************}
procedure TALDynamicListBoxView.UnLockItemIds;
begin
  Tmonitor.exit(FUniqueInt64ItemIds);
end;

{******************************************************************************************}
function  TALDynamicListBoxView.DownloadItems(const AForceReload: Boolean = False): boolean;
begin

  // Exit if the last download resulted in an error, unless AForceReload is True
  // (e.g., when triggered by a "Reload" button click)
  if (not AForceReload) and
     (FDownloadItemsErrorCode <> 0) then exit(False);

  // Exit if no data is needed
  if not CanDownloadItems then exit(false);

  // Exit if a thread is already performing the task
  if IsDownloadItemsRunning then exit(true);

  // Before starting the background thread
  {$IFDEF DEBUG}
  ALLog(ClassName+'.DownloadItems', 'ForceReload:' + ALBoolToStrW(AForceReload));
  {$ENDIF}
  FDownloadItemsErrorCode := 0;

  // Load the data in a separate thread to avoid blocking the calling thread
  FDownloadItemsContext := CreateDownloadItemsContext;
  Try
    TALNetHttpClientPool.Instance.ExecuteProc(
      DownloadItemsBackgroundProc, // const AProc: TALWorkerThreadObjProc;
      FDownloadItemsContext, // const AContext: Tobject; // Context will be free by the worker thread
      GetPriority); // const AGetPriorityFunc: TALWorkerThreadGetPriorityFunc;
  except
    ALFreeAndNil(FDownloadItemsContext);
    Raise;
  End;

  // Return True
  result := true;

end;

{*******************************************************************************}
function TALDynamicListBoxView.CreateDownloadItemsContext: TDownloadItemsContext;
begin
  Result := TDownloadItemsContext.Create(Self);
end;

{***************************************************************************************}
class procedure TALDynamicListBoxView.DownloadItemsBackgroundProc(var AContext: Tobject);
begin
  var LContext := TDownloadItemsContext(AContext);
  if LContext.owner = nil then exit;
  try

    var LData: TALJSONNodeW := nil;
    var LFreeItems := True;
    var LItems: TArray<TALDynamicListBoxItem> := nil;
    Try

      var LDownloadItemsErrorCode: Integer := 0;
      DownloadItemsBackgroundProcFetchData(LContext, LData, LDownloadItemsErrorCode);

      if LContext.owner = nil then exit;
      DownloadItemsBackgroundProcCreateItems(LContext, LDownloadItemsErrorCode, LData, LItems);

      while not DownloadItemsBackgroundProcCanProcessItems(LContext) do begin
        if LContext.owner = nil then exit;
        sleep(250);
      end;

      if LContext.owner = nil then exit;
      TThread.queue(nil,
        procedure
        begin
          Try
            if LContext.owner <> nil then begin
              LContext.owner.DownloadItemsProcessItems(LContext, LDownloadItemsErrorCode, LItems);
              LContext.owner.FDownloadItemsContext := nil;
              LContext.owner.DownloadItemsFinished;
            end;
          finally
            ALFreeAndNil(LContext);
            For var I := Low(LItems) to High(LItems) do
              ALFreeAndNil(LItems[i]);
          End;
        end);
      AContext := nil; // AContext will be free by TThread.queue
      LFreeItems := False; // LItems will be free by TThread.queue

    finally
      ALfreeAndNil(LData);
      if LFreeItems then
        For var I := Low(LItems) to High(LItems) do
          ALFreeAndNil(LItems[i]);
    end;

  Except
    On E: Exception do begin
      ALLog('TALDynamicListBoxView.DownloadItemsBackgroundProc', E);
      TMonitor.Enter(LContext.Lock);
      try
        if LContext.Owner <> nil then begin
          LContext.FreeByThread := False;
          AContext := nil; // AContext will be free by CancelResourceDownload
        end;
      finally
        TMonitor.Exit(LContext.Lock);
      end;
    end;
  end;
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBoxView.DownloadItemsBackgroundProcFetchData(
                  const AContext: TDownloadItemsContext;
                  out AData: TALJSONNodeW;
                  var AErrorCode: Integer);
begin
  Tmonitor.Enter(AContext.Lock);
  Try
    if AContext.owner = nil then exit;
    if not assigned(AContext.OnDownloadItems) then
      Raise Exception.Create('Error DF2328CA-BCF7-46D6-B100-AFD222FF8873');
    var LMethod: TMethod;
    LMethod.Code := TMethod(AContext.OnDownloadItems).Code;
    // Set Self to nil to prevent accidental access to instance members,
    // as we are in a multithreaded context where most members are not thread-safe.
    LMethod.Data := nil;
    if AContext.PaginationToken = #0 then
      AContext.PaginationToken := '';
    TDownloadItemsEvent(LMethod)(
      AContext, // const AContext: TDownloadItemsContext;
      AData, // Const AData: TALJSONNodeW;
      AContext.PaginationToken, // var APaginationToken: String;
      AErrorCode); // var AErrorCode: Integer
  finally
    Tmonitor.Exit(AContext.Lock);
  End;
end;

{**************}
// [MultiThread]
class procedure TALDynamicListBoxView.DownloadItemsBackgroundProcCreateItems(
                  const AContext: TDownloadItemsContext;
                  const AErrorCode: Integer;
                  const AData: TALJSONNodeW;
                  out AItems: TArray<TALDynamicListBoxItem>);
begin
  TMonitor.Enter(AContext.Lock);
  try

    // Exit
    if (AContext.owner = nil) or
       (AErrorCode <> 0) or
       (AData = nil) or
       (AData.ChildNodes.Count = 0) then begin
      AItems := nil;
      Exit;
    end;

    // Calculate ItemIdType
    If AContext.ItemIdType = TItemIdType.Unknown then begin
      var LIdNode := AData.ChildNodes[0].getChildNode(AContext.ItemIdNodeName);
      if LIdNode.NodeSubType in [nstInt32, nstInt64] then AContext.ItemIdType := TItemIdType.Int64
      else AContext.ItemIdType := TItemIdType.Text;
    end;

    // Deduplicates items
    AContext.Owner.LockItemIds;
    try
      var Lcount: integer;
      if Acontext.ItemIdType = TItemIdType.Int64 then LCount := AContext.Owner.FUniqueInt64ItemIds.Count
      else LCount := AContext.Owner.FUniqueTextItemIds.Count;
      for var i := AData.ChildNodes.Count - 1 downto 0 do begin
        if Lcount >= Acontext.MaxItems then break;
        var LItemNode := AData.ChildNodes[i];
        if AContext.ItemIdType = TItemIdType.Int64 then begin
          var LItemId := LItemNode.getChildNodeValueInt64(AContext.ItemIdNodeName, 0);
          if (LItemID = 0) or (not AContext.Owner.FUniqueInt64ItemIds.TryAdd(LItemID, true)) then begin
            AData.ChildNodes.Delete(i);
            continue;
          end;
        end
        else begin
          var LItemId := LItemNode.getChildNodeValueText(AContext.ItemIdNodeName, '');
          if (LItemID = '') or (not AContext.Owner.FUniqueTextItemIds.TryAdd(LItemID, true)) then begin
            AData.ChildNodes.Delete(i);
            continue;
          end;
        end;
        inc(LCount);
      end;
    finally
      AContext.Owner.UnLockItemIds
    end;

    // Update AItems
    Setlength(AItems, AData.ChildNodes.Count);
    For var I := AData.ChildNodes.Count - 1 downto 0 do begin
      var LData := AData.ChildNodes.Extract(I);
      LData.ChildNodes.SetSorted(true{Value},true{recurse});
      LData.MultiThreadPrepare(true{aOnlyChildList});
      var LItem: TALDynamicListBoxItem;
      If assigned(AContext.OnCreateItem) then begin
        var LMethod: TMethod;
        LMethod.Code := TMethod(AContext.OnCreateItem).Code;
        // Set Self to nil to prevent accidental access to instance members,
        // as we are in a multithreaded context where most members are not thread-safe.
        LMethod.Data := nil;
        LItem := TCreateItemEvent(LMethod)(
                   AContext, // const AContext: TDownloadItemsContext;
                   LData); // Const AData: TALJSONNodeW;
      end
      else LItem := TALDynamicListBoxItem.Create(nil);
      if LItem.FData = nil then LItem.FData := LData
      else ALFreeAndNil(LData);
      if not assigned(LItem.OnCreateMainContent) then LItem.OnCreateMainContent := AContext.OnCreateItemMainContent;
      if not assigned(LItem.OnCreateLoadingContent) then LItem.OnCreateLoadingContent := AContext.OnCreateItemLoadingContent;
      if not assigned(LItem.OnCreateErrorContent) then LItem.OnCreateErrorContent := AContext.OnCreateItemErrorContent;
      if not assigned(LItem.OnDownloadData) then LItem.OnDownloadData := AContext.OnDownloadItemData;
      AItems[i] := LItem;
    end;

  finally
    TMonitor.Exit(AContext.Lock);
  end;
end;

{**************}
// [MultiThread]
class function TALDynamicListBoxView.DownloadItemsBackgroundProcCanProcessItems(const AContext: TDownloadItemsContext): boolean; // [MultiThread]
begin
  if TThread.Current.ThreadID = MainThreadID then exit(true);
  TMonitor.Enter(AContext.Lock);
  try
    // Primarily because we want to prevent the list
    // from being updated during the bottom-bound animation.
    result := (AContext.owner = nil) or
              (AContext.Owner.OwnerListBox = nil) or
              (not AContext.Owner.OwnerListBox.HasActiveScrollEngines);
  finally
    TMonitor.Exit(AContext.Lock);
  end;
end;

{********************************************************}
procedure TALDynamicListBoxView.DownloadItemsProcessItems(
            const AContext: TDownloadItemsContext;
            const AErrorCode: Integer;
            var AItems: TArray<TALDynamicListBoxItem>);
begin
  FDownloadItemsErrorCode := AErrorCode;
  if FDownloadItemsErrorCode = 0 then begin

    {$IFDEF DEBUG}
    var LStopWatch := TstopWatch.startNew;
    try
    {$ENDIF}

      // Create the MainContent
      if FMainContent = nil then begin
        var LContext := TCreateContentContext.Create(Self);
        Try
          if Assigned(OnCreateMainContent) then FMainContent := OnCreateMainContent(LContext)
          else FMainContent := TALDynamicListBoxViewMainContent.Create(nil);
        Finally
          ALFreeAndNil(LContext);
        End;
        {$IFDEF DEBUG}
        if FMainContent = nil then
          Raise Exception.Create('Error 6482DF15-1503-4D36-84E5-FBF560650F28');
        {$ENDIF}
        if (OwnerListBox <> nil) and
           (Self = OwnerListBox.MainView) and
           (not assigned(TALDynamicListBoxViewMainContent(FMainContent).OnRealign)) then
          TALDynamicListBoxViewMainContent(FMainContent).OnRealign := OwnerListBox.OnRealignItems;
        InsertControl(FMainContent, 0);
        ActivateCoreContent(FMainContent);
        FItems := @FMainContent.FControls;
      end;

      // Add the items
      if (length(AItems) > 0) then begin
        TALDynamicListBoxViewMainContent(FMainContent).InsertItems(AItems, Maxint);
        FTriggerDownloadItemsAtIndex := High(FItems^) - (length(AItems) div 3);
        AItems := nil;
      end;

      // Update FPaginationToken
      FPaginationToken := AContext.PaginationToken;

    {$IFDEF DEBUG}
    finally
      LStopWatch.Stop;
      ALLog(ClassName+'.DownloadItemsProcessItems', 'timeTaken: ' + ALFloatToStrW(LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW));
    end;
    {$ENDIF}

  end;
end;

{****************************************************}
procedure TALDynamicListBoxView.DownloadItemsFinished;
begin
  if FDownloadItemsErrorCode <> 0 then OwnerListBox.ShowErrorMessageBanner(FDownloadItemsErrorCode)
  else SetViewportPosition(ViewportPosition);
end;

{*******************************************************}
function TALDynamicListBoxView.CanDownloadItems: Boolean;
begin
  Result := (FLastVisibleItemIndex >= FTriggerDownloadItemsAtIndex) and
            (FPaginationToken <> '');
end;

{*************************************************************}
function TALDynamicListBoxView.IsDownloadItemsRunning: Boolean;
begin
  result := FDownloadItemsContext <> nil;
end;

{**************************************************}
procedure TALDynamicListBoxView.CancelDownloadItems;
begin
  // The FDownloadItemsContext pointer can only be
  // updated in the main thread, so there is no need
  // to lock its access for reading or updating.
  if FDownloadItemsContext <> nil then begin
    var LContextToFree: TDownloadItemsContext;
    var LLock := FDownloadItemsContext.lock;
    TMonitor.Enter(LLock);
    try
      if not FDownloadItemsContext.FreeByThread then LContextToFree := FDownloadItemsContext
      else LContextToFree := nil;
      FDownloadItemsContext.Owner := nil;
      FDownloadItemsContext := nil;
    Finally
      TMonitor.Exit(LLock);
    End;
    ALFreeAndNil(LContextToFree);
  end;
end;

{************************************************************************************************}
procedure TALDynamicListBoxView.ActivateCoreContent(const AContent: TALDynamicListBoxItemContent);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ToggleContent(var AMemberContent: TALDynamicListBoxItemContent);
  begin
    if (AMemberContent <> nil) and (AMemberContent <> AContent) then begin
      if AMemberContent.IsEphemeral then ALFreeAndNil(AMemberContent, true{ADelayed})
      else AMemberContent.Visible := False;
    end;
  end;

begin
  _ToggleContent(FNoItemsContent);
  inherited;
end;

{***********************************************************}
function TALDynamicListBoxView.CanCreateMainContent: Boolean;
begin
  Result := True;
end;

{*******************************************************}
constructor TALDynamicListBox.Create(aOwner: TComponent);
begin
  inherited create(AOwner);
  ClipChildren := True;
  FHovered := nil;
  FCaptured := nil;
  FCacheEngine := TALBufDrawableCacheEngine.Create;
  FHasBeenPrepared := False;
  FActiveScrollEnginesCount := 0;
  FOnDownloadItems := nil;
  FOnDownloadItemData := nil;
  FOnCreateMainView := nil;
  FOnCreateMainViewMainContent := nil;
  FOnCreateMainViewLoadingContent := nil;
  FOnCreateMainViewErrorContent := nil;
  FOnCreateMainViewNoItemsContent := nil;
  FOnCreateItem := nil;
  FOnCreateItemMainContent := nil;
  FOnCreateItemLoadingContent := nil;
  FOnCreateItemErrorContent := nil;
  FOnRealignItems := nil;
  FMainView := nil;
end;

{***********************************}
destructor TALDynamicListBox.Destroy;
begin
  ALFreeAndNil(FMainView);
  ALFreeAndNil(FCacheEngine);
  inherited Destroy;
end;

{**********************************}
procedure TALDynamicListBox.Prepare;
begin
  if not FHasBeenPrepared then begin
    FMainView := CreateMainView;
    {$IF defined(DEBUG)}
    If (FMainView.OwnerListBox <> self) then
      Raise Exception.Create('MainView must be created with the ListBox as its owner');
    {$ENDIF}
    FMainView.Align := TALAlignLayout.Client;
    if not assigned(FMainView.OnCreateMainContent) then FMainView.OnCreateMainContent := OnCreateMainViewMainContent;
    if not assigned(FMainView.OnCreateLoadingContent) then FMainView.OnCreateLoadingContent := OnCreateMainViewLoadingContent;
    if not assigned(FMainView.OnCreateErrorContent) then FMainView.OnCreateErrorContent := OnCreateMainViewErrorContent;
    if not assigned(FMainView.OnCreateNoItemsContent) then FMainView.OnCreateNoItemsContent := OnCreateMainViewNoItemsContent;
    if not assigned(FMainView.OnDownloadItems) then FMainView.OnDownloadItems := OnDownloadItems;
    if not assigned(FMainView.OnCreateItem) then FMainView.OnCreateItem := OnCreateItem;
    if not assigned(FMainView.OnCreateItemMainContent) then FMainView.OnCreateItemMainContent := OnCreateItemMainContent;
    if not assigned(FMainView.OnCreateItemLoadingContent) then FMainView.OnCreateItemLoadingContent := OnCreateItemLoadingContent;
    if not assigned(FMainView.OnCreateItemErrorContent) then FMainView.OnCreateItemErrorContent := OnCreateItemErrorContent;
    if not assigned(FMainView.OnDownloadItemData) then FMainView.OnDownloadItemData := OnDownloadItemData;
    FMainView.SetBounds(0,0,Width,Height);
    FMainView.Prepare;
    FHasBeenPrepared := True;
  end;
end;

{***************************************************************}
function TALDynamicListBox.CreateMainView: TALDynamicListBoxView;
begin
  If assigned(OnCreateMainView) then Result := OnCreateMainView(Self)
  else result := TALDynamicListBoxView.Create(self);
end;

{*****************************************}
function TALDynamicListBox.GetControlAtPos(
           const APos: TALPointD; // APos is local to the control
           out AControlPos: TALPointD; // AControlPos is local to the founded control
           const ACheckHitTest: Boolean = true): TALDynamicListBoxControl;
begin
  if MainView = nil then exit(nil);
  var LAbsolutePos := LocalToAbsolute(APos.ReducePrecision);

  if Hovered <> nil then
    result := Hovered.GetControlAtPos(Hovered.AbsoluteToLocal(LAbsolutePos), AControlPos, ACheckHitTest)
  else
    result := nil;

  if result = nil then
    result := MainView.GetControlAtPos(MainView.AbsoluteToLocal(LAbsolutePos), AControlPos, ACheckHitTest);
end;

{*****************************************}
function TALDynamicListBox.GetControlAtPos(
           const APos: TALPointD; // APos is local to the control
           const ACheckHitTest: Boolean = true): TALDynamicListBoxControl;
begin
  Var LControlPos: TALPointD;
  result := GetControlAtPos(APos, LControlPos, ACheckHitTest);
end;

{********************************************************************************************}
procedure TALDynamicListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LControlMousePos: TALPointD;
  var LControl := GetControlAtPos(
                    TALPointD.create(X, Y), // const APos: TPointF; // APos is local to the control
                    LControlMousePos); // out AControlPos: TPointF; // AControlPos is local to the founded control
  If LControl <> nil then
    LControl.MouseDown(Button, Shift, LControlMousePos.X, LControlMousePos.Y);
  inherited;
end;

{**********************************************************************}
procedure TALDynamicListBox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if FCaptured <> nil then begin
    Var LCapturedMousePos := FCaptured.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(X, Y)));
    FCaptured.MouseMove(Shift, LCapturedMousePos.X, LCapturedMousePos.Y);
  end
  else begin
    var LControlMousePos: TALPointD;
    var LControl := GetControlAtPos(
                      TALPointD.create(X, Y), // const APos: TPointF; // APos is local to the control
                      LControlMousePos); // out AControlPos: TPointF; // AControlPos is local to the founded control
    If LControl <> nil then begin
      SetHovered(LControl);
      LControl.MouseMove(Shift, LControlMousePos.X, LControlMousePos.Y);
    end
    else
      SetHovered(nil);
  end;
  inherited;
end;

{******************************************************************************************}
procedure TALDynamicListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
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
  //--
  inherited;
end;

{*********************************************************************************************}
procedure TALDynamicListBox.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
end;

{***************************************}
procedure TALDynamicListBox.DoMouseLeave;
begin
  SetCaptured(Nil);
  SetHovered(nil);
  inherited;
end;

{*****************************************************************************}
procedure TALDynamicListBox.SetCaptured(const Value: TALDynamicListBoxControl);
begin
  FCaptured := Value;
end;

{****************************************************************************}
procedure TALDynamicListBox.SetHovered(const Value: TALDynamicListBoxControl);
begin
  if (Value <> FHovered) then begin
    if FHovered <> nil then FHovered.MouseLeave;
    FHovered := Value;
    if FHovered <> nil then FHovered.MouseEnter;
  end;
end;

{**************}
// [MultiThread]
function TALDynamicListBox.GetPriority(const AContext: Tobject): Int64;
begin
  result := TALNetHttpClientPool.Instance.PriorityStartingPoint;
end;

{************************************************************}
function TALDynamicListBox.GetHasActiveScrollEngines: Boolean;
begin
  Result := FActiveScrollEnginesCount > 0;
end;

{************************************}
procedure TALDynamicListBox.DoResized;
begin
  inherited;
  if MainView <> nil then
    MainView.SetBounds(0,0,Width,Height);
  FCacheEngine.CLearEntries;
end;

{********************************}
procedure TALDynamicListBox.Paint;
begin
  if TALGuardianThread.HasInstance then
    TALGuardianThread.Instance.CanExecute := False;
  //--
  inherited;
  //--
  Prepare;
  //--
  if MainView = nil then
    Raise Exception.Create('Error EE6AB6E3-5988-4325-9EA4-92AC8B0D7C28');
  //--
  var LSavedMatrix := Canvas.Matrix;
  try
    var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(MainView.left,MainView.Top);
    Canvas.SetMatrix(LMatrix);
    MainView.PaintInternal(Canvas);
  finally
    Canvas.SetMatrix(LSavedMatrix);
  end;
  //--
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{****************************************************************************}
procedure TALDynamicListBox.ShowErrorMessageBanner(const AErrorCode: Integer);
begin

end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALDynamicListBox]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALDynamicListBox, 'Size');
  UnlistPublishedProperty(TALDynamicListBox, 'StyleName');
  UnlistPublishedProperty(TALDynamicListBox, 'OnTap');
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALDynamicListBox]);
  _ALDummyComponent := TComponent.create(nil);
  {$IFDEF DEBUG}
  {$If defined(Android)}
  //https://developer.android.com/media/optimize/performance/frame-rate
  TALDynamicListBoxView.DisplayDefaultRefreshRate := TAndroidHelper.Display.getRefreshRate;
  {$ELSEIf defined(IOS)}
  TALDynamicListBoxView.DisplayDefaultRefreshRate := TiOSHelper.MainScreen.maximumFramesPerSecond;
  {$ELSE}
  TALDynamicListBoxView.DisplayDefaultRefreshRate := 60
  {$ENDIF}
  {$ENDIF}

finalization
  AlFreeAndNil(_ALDummyComponent);

end.
