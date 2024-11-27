unit Alcinoe.FMX.DynamicScrollBox;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  System.SyncObjs,
  System.Messaging,
  System.Generics.Collections,
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
  Alcinoe.JSONDoc,
  Alcinoe.FMX.Ani,
  Alcinoe.Common,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.fmx.Common;

type

  (*
  // https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-2342
  TAlignLayout = (
    None,
    Top,
    Left,
    Right,
    Bottom,
    //MostTop,
    //MostBottom,
    //MostLeft,
    //MostRight,
    Client,
    //Contents,
    Center,
    VertCenter,
    HorzCenter,
    Horizontal,
    Vertical,
    //Scale,
    //Fit,
    //FitLeft,
    //FitRight,
    TopCenter, // No TAlignLayout equivalent - Aligns the control to the top of the container but centers it horizontally without stretching its width to match the container's width.
    LeftCenter,  // No TAlignLayout equivalent - Aligns the control to the left of the container but centers it vertically without stretching its height to match the container's height.
    RightCenter, // No TAlignLayout equivalent - Aligns the control to the right of the container but centers it vertically without stretching its height to match the container's height.
    BottomCenter); // No TAlignLayout equivalent - Aligns the control to the bottom of the container but centers it horizontally without stretching its width to match the container's width.
  *)



  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDownloadingStatus = (None, Downloading, Processing, Processed, Error);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicScrollBoxItem = class;
  TALDynamicScrollBoxView = class;
  TALDynamicScrollBox = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicScrollBoxDownloadMaterialExtData = Class(Tobject)
  public
    type
      TDrawMaterialFunc = function(
                            const aSender: Tobject;
                            const aContentStream: TCustomMemoryStream;
                            const aContentUrl: String;
                            const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
                            Const aContentBlurred: single;
                            const aContentCropCenter: TPointF;
                            const aCornerRadius: TPointF;
                            const aScreenScale: single): TALDrawable;
  private
    FComparekey: integer;
    FMaterialIdx: integer;
    FContentStream: TCustomMemoryStream;
    fContentUrl: String;
    fDestSize: TSizeF;
    fContentBlurred: single;
    fContentCropCenter: TpointF;
    fCornerRadius: TPointF;
    fDrawMaterialFunc: TDrawMaterialFunc;
  public
    constructor Create(
                  const AComparekey: Integer;
                  const AMaterialIdx: Integer;
                  const AContentUrl: String;
                  const ADestSize: TSizeF;
                  const AContentBlurred: single;
                  const AContentCropCenter: TpointF;
                  const aCornerRadius: TPointF;
                  const ADrawMaterialFunc: TDrawMaterialFunc); overload; virtual;
    constructor Create(
                  const AComparekey: Integer;
                  const AMaterialIdx: Integer;
                  const AContentStream: TCustomMemoryStream;
                  const ADestSize: TSizeF;
                  const AContentBlurred: single;
                  const AContentCropCenter: TpointF;
                  const aCornerRadius: TPointF;
                  const ADrawMaterialFunc: TDrawMaterialFunc); overload; virtual;
    destructor Destroy; override;
    Property Comparekey: integer read FComparekey;
    Property MaterialIdx: integer read FMaterialIdx;
    Property ContentStream: TCustomMemoryStream read FContentStream write FContentStream;
    Property ContentUrl: String read fContentUrl;
    Property DestSize: TSizeF read fDestSize;
    Property ContentBlurred: single read fContentBlurred;
    Property ContentCropCenter: TpointF read fContentCropCenter;
    Property CornerRadius: TPointF read fCornerRadius;
    property DrawMaterialFunc: TDrawMaterialFunc read fDrawMaterialFunc;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALDynamicScrollBoxControl is similar to TControl
  // but optimized for minimal overhead.
  TALDynamicScrollBoxControl = class(TObject)
  private
    FBoundsRect: TALRectD;
    FPadding: TBounds;
    FMargins: TBounds;
    FOnResized: TNotifyEvent;
    procedure SetPadding(const AValue: TBounds);
    procedure SetMargins(const AValue: TBounds);
    procedure PaddingChangedHandler(Sender: TObject);
    procedure MarginsChangedHandler(Sender: TObject);
  protected
    procedure SetBoundsRect(AValue: TALRectD); virtual;
    procedure HandleSizeChanged; virtual;
    function GetWidth: Single; inline;
    procedure SetWidth(const AValue: Single); inline;
    function GetHeight: Single; inline;
    procedure SetHeight(const AValue: Single); inline;
    function GetTop: Double; inline;
    procedure SetTop(const AValue: Double); inline;
    function GetLeft: Double; inline;
    procedure SetLeft(const AValue: Double); inline;
    procedure PaddingChanged; virtual;
    Procedure MarginsChanged; Virtual;
    procedure DoResized; virtual;
    procedure Realign;
    procedure DoRealign; virtual;
  public
    constructor Create(const AOwner: TALDynamicScrollBoxControl); overload; virtual;
    constructor Create(const AOwner: TALDynamicScrollBox); overload; virtual;
    destructor Destroy; override;
    // https://stackoverflow.com/questions/73107039/record-property-with-getter-left-side-cannot-be-assigned-to
    // While it may be possible to perform operations like BoundsRect.width := 25, this behavior is unintended and
    // can be considered a bug, as theoretically it should be disallowed. Therefore, it is crucial to avoid updating
    // BoundsRect via its individual members (e.g., BoundsRect.width := 25). Instead, modifications should only
    // be made through the writer property SetBoundsRect, such as: BoundsRect := TALRectD.Create(...)
    property BoundsRect: TALRectD read FBoundsRect write SetBoundsRect;
    property Left: Double read GetLeft write SetLeft;
    property Top: Double read GetTop write SetTop;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
    property Padding: TBounds read FPadding write SetPadding;
    property Margins: TBounds read FMargins write SetMargins;
    property OnResized: TNotifyEvent read FOnResized write FOnResized;




  private
    //**FForm: TCommonCustomForm;
    FFocusOnMouseDown: Boolean;
    FFocusOnMouseUp: Boolean;
    FControlAbsolutePosAtMouseDown: TpointF;
    FMouseDownAtLowVelocity: Boolean;
    FDisableDoubleClickHandling: Boolean;
    FIsPixelAlignmentEnabled: Boolean;
    //**FFormerMarginsChangedHandler: TNotifyEvent;
    function GetPressed: Boolean;
    procedure SetPressed(const AValue: Boolean);
    //**procedure DelayOnResize(Sender: TObject);
    //**procedure DelayOnResized(Sender: TObject);
    //**procedure MarginsChangedHandler(Sender: TObject);
  private
    FCanvas: TCanvas;
    FAlign: TALAlignLayout;
    FPressed: Boolean;
    FIsMouseOver: Boolean;
    FIsFocused: Boolean;
    FAutoCapture: Boolean;
  protected
    function GetLocalRect: TRectF; virtual;
    procedure DoEnter; virtual;
    procedure DoExit; virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); overload; virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); overload; virtual;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); overload; virtual;
    procedure Paint; overload; virtual;
    function GetAbsoluteOpacity: Single; virtual;
  protected
    function GetIsPixelAlignmentEnabled: Boolean; virtual;
    procedure SetIsPixelAlignmentEnabled(const AValue: Boolean); Virtual;
    property FocusOnMouseDown: Boolean read FFocusOnMouseDown write FFocusOnMouseDown;
    property FocusOnMouseUp: Boolean read FFocusOnMouseUp write FFocusOnMouseUp;
    //**function GetParentedVisible: Boolean; override;
    //**procedure DoRootChanged; override;
    procedure IsMouseOverChanged; virtual;
    procedure IsFocusedChanged; virtual;
    procedure PressedChanged; virtual;
    //**procedure Loaded; override;
    //**function IsOwnerLoading: Boolean;
    //**function IsSizeStored: Boolean; override;
  public
    //**procedure SetNewScene(AScene: IScene); override;
    //**function IsVisibleWithinFormBounds: Boolean;
    //**property Form: TCommonCustomForm read FForm;
    property DisableDoubleClickHandling: Boolean read FDisableDoubleClickHandling write FDisableDoubleClickHandling;
    //**{$IFNDEF ALCompilerVersionSupported122}
    //**  {$MESSAGE WARN 'Check if property FMX.Controls.TControl.Pressed still not fire a PressChanged event when it gets updated, and adjust the IFDEF'}
    //**{$ENDIF}
    property Pressed: Boolean read GetPressed write SetPressed;
    procedure AlignToPixel; virtual;
    property IsPixelAlignmentEnabled: Boolean read GetIsPixelAlignmentEnabled write SetIsPixelAlignmentEnabled;
  public
    function AbsoluteToLocal(const APoint: TPointF): TPointF; overload; virtual;
    function AbsoluteToLocal(const ARect: TRectF): TRectF; overload;
    function LocalToAbsolute(const APoint: TPointF): TPointF; overload; virtual;
    function LocalToAbsolute(const ARect: TRectF): TRectF; overload;
    procedure Repaint;
    procedure SetFocus;
    procedure BeginUpdate; virtual;
    function IsUpdating: Boolean; virtual;
    procedure EndUpdate; virtual;
    property Align: TALAlignLayout read FAlign write FAlign;
    property IsMouseOver: Boolean read FIsMouseOver;
    property IsFocused: Boolean read FIsFocused;
    property AutoCapture: Boolean read FAutoCapture write FAutoCapture;
    property LocalRect: TRectF read GetLocalRect;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property AbsoluteOpacity: Single read GetAbsoluteOpacity;
  //
  {$REGION 'OnPaint'}
  protected
    // Inside a view, the Rect and canvas matrix are adjusted
    // to match the current scroll position.
    procedure Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single); overload; virtual; // ARect is local to the control
  {$ENDREGION'}
  //
  {$REGION 'MouseEvents'}
  private
    FHitTest: Boolean;
    FIsPressed: Boolean;
    FLongPressDelay: Cardinal;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    //*FOnMouseDown: TALMouseEvent;
    //*FOnMouseUp: TALMouseEvent;
    //*FOnClick: TALMouseEvent;
    //*FOnDblClick: TALMouseEvent;
    //*FOnMouseMove: TALMouseEvent;
    //*FOnLongPress: TALMouseEvent;
    procedure SetOnMouseEnter(const AValue: TNotifyEvent);
    procedure SetOnMouseLeave(const AValue: TNotifyEvent);
    //*procedure SetOnMouseDown(const AValue: TALMouseEvent);
    //*procedure SetOnMouseUp(const AValue: TALMouseEvent);
    //*procedure SetOnClick(const AValue: TALMouseEvent);
    //*procedure SetOnDblClick(const AValue: TALMouseEvent);
    //*procedure SetOnMouseMove(const AValue: TALMouseEvent);
    //*procedure SetOnLongPress(const AValue: TALMouseEvent);
  protected
    procedure MouseEnter(const Sender: TObject); virtual;
    procedure MouseLeave(const Sender: TObject); virtual;
    procedure MouseDown(const Sender: TObject; const AMousePos: TPointF); overload; virtual; // AMousePos is local to the control
    procedure MouseUp(const Sender: TObject; const AMousePos: TPointF); overload; virtual; // AMousePos is local to the control
    procedure Click(const Sender: TObject; const AMousePos: TPointF); overload; virtual; // AMousePos is local to the control
    procedure DblClick(const Sender: TObject; const AMousePos: TPointF); virtual; // AMousePos is local to the control
    procedure MouseMove(const Sender: TObject; const AMousePos: TPointF); virtual; // AMousePos is local to the control
    procedure LongPress(const Sender: TObject; const AMousePos: TPointF); virtual; // AMousePos is local to the control
  public
    property HitTest: Boolean read FHitTest write FHitTest;
    property IsPressed: Boolean read FIsPressed write FIsPressed;
    property LongPressDelay: Cardinal read FLongPressDelay write FLongPressDelay;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetOnMouseLeave;
    //*property OnMouseDown: TALMouseEvent read FOnMouseDown write SetOnMouseDown;
    //*property OnMouseUp: TALMouseEvent read FOnMouseUp write SetOnMouseUp;
    //*property OnClick: TALMouseEvent read FOnClick write SetOnClick;
    //*property OnDblClick: TALMouseEvent read FOnDblClick write SetOnDblClick;
    //*property OnMouseMove: TALMouseEvent read FOnMouseMove write SetOnMouseMove;
    //*property OnLongPress: TALMouseEvent read FOnLongPress write SetOnLongPress;
  {$ENDREGION'}
  //
  {$REGION 'TouchEffect'}
  //*public
  //*  type
  //*    TTouchEffectStyle = (
  //*      None,
  //*      Custom,
  //*      ClickFadeOutRectangle,
  //*      ClickFadeOutCircle,
  //*      ClickFadeOutRectangleAndCircle,
  //*      LongPressFadeOutRectangle,
  //*      LongPressFadeOutCircle);
  //*    //--
  //*    TStartTouchEffectEvent = (
  //*      Click,
  //*      MouseDown);
  //*    //--
  //*    TTouchEffectBoundsConstraint = (
  //*      LocalRect,
  //*      ExpandedLocalRect,
  //*      BitmapRect);
  //*protected
  //*  procedure StartTouchEffect(Const AEvent: TStartTouchEffectEvent; Const AMousePos: TpointF); virtual; // AMousePos is local to the control
  //*  procedure FadeOutTouchEffect; virtual;
  //*  procedure InitTouchEffectBitmaps; virtual;
  //*  procedure DrawTouchEffect(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single); virtual;
  //*  function GetTouchEffectBoundsRect: TrectF; virtual;
  //*public
  //*  TouchEffectStyle: TTouchEffectStyle;
  //*  StartTouchEffectOnMouseDown: Boolean;
  //*  TouchEffectBoundsConstraint: TTouchEffectBoundsConstraint;
  //*  TouchEffectZOrder: integer;
  //*  TouchEffectRectangleFillColor: TalphaColor;
  //*  TouchEffectCircleFillColor: TalphaColor;
  {$ENDREGION'}
  //
  {$REGION 'Type-checking'}
  private
    FIsScrollBoxView: Boolean;
    FIsScrollBoxItem: Boolean;
    FIsEphemeralLayout: Boolean;
  public
    property IsScrollBoxView: Boolean read FIsScrollBoxView;
    property IsScrollBoxItem: Boolean read FIsScrollBoxItem;
    property IsEphemeralLayout: Boolean read FIsEphemeralLayout;
  {$ENDREGION'}
  //
  {$REGION 'Owner'}
  private
    FOwner: TALDynamicScrollBoxControl;
    FOwnerItem: TALDynamicScrollBoxItem;
    FOwnerView: TALDynamicScrollBoxView;
    FOwnerScrollBox: TALDynamicScrollBox;
    procedure SetOwner(const Value: TALDynamicScrollBoxControl);
  public
    Property OwnerItem: TALDynamicScrollBoxItem read FOwnerItem;
    Property OwnerView: TALDynamicScrollBoxView read FOwnerView;
    property OwnerScrollBox: TALDynamicScrollBox read FOwnerScrollBox;
    property Owner: TALDynamicScrollBoxControl read FOwner write SetOwner;
    Property Parent: TALDynamicScrollBoxControl read FOwner write SetOwner;
  {$ENDREGION'}
  //
  {$REGION 'Controls'}
  private
    // The order of the list determines the alignment priority and z-order: the
    // first control in the list aligns and paints before the others. first
    // control in the list catches touch events after others
    FControls: TArray<TALDynamicScrollBoxControl>;
    FControlIndex: integer;
    FNeedArrangement: Boolean;
    function GetControlsCount: Integer;
  protected
    procedure InsertControl(const AControl: TALDynamicScrollBoxControl); virtual;
    procedure RemoveControl(const AControl: TALDynamicScrollBoxControl); virtual;
    procedure MoveControl(const AControl: TALDynamicScrollBoxControl; const ANewIndex: Integer);
    function GetControlAtPos(
               const aPos: TPointF; // APos is local to the control
               out AControlPos: TPointF; // AControlPos is local to the founded control
               const ADirectChildOnly: Boolean = False;
               const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl; overload; virtual;
    function GetControlAtPos(
               const aPos: TPointF;
               const ADirectChildOnly: Boolean = False;
               const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl; overload; virtual; // APos is local to the control
  public
    property ControlIndex: Integer read FControlIndex;
    property ControlsCount: Integer read GetControlsCount;
    Procedure AlignControls;
  {$ENDREGION'}
  //
  {$REGION 'Coordinates conversion'}
  public
    function LocalToFrame(const APoint: TALPointD): TALPointD; overload; virtual;
    function LocalToFrame(const ARect: TALRectD): TALRectD; overload; virtual;
    function LocalToFrame(const APoint: TPointF): TALPointD; overload;
    function LocalToFrame(const ARect: TRectF): TALRectD; overload;
    function FrameToLocal(const APoint: TPointF): TALPointD; overload; virtual;
    function FrameToLocal(const ARect: TRectF): TALRectD; overload; virtual;
  {$ENDREGION}
  //
  {$REGION 'BufBitmap'}
  public
    function MakeBufBitmap: TALDrawable; virtual;
    procedure ClearBufBitmap; virtual;
  {$ENDREGION}
  //
  {$REGION 'Alignment'}
  private
    fIsAdjustingSize: Boolean;
  protected
    procedure AdjustSize; virtual;
  {$ENDREGION}
  //
  {$REGION 'Materials'}
  protected
    procedure DownloadMaterials; virtual;
    procedure DoDownloadMaterials; virtual;
    procedure DownloadMaterial(
                const AMaterialIdx: Integer;
                var AMediaStatus: TALDownloadingStatus;
                const aMediaURL: String;
                const AMediaBlurred: single;
                const AMediaCropCenter: TpointF;
                const aDestSize: TSizeF;
                const aCornerRadius: TPointF;
                const ADrawFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc;
                const ADrawEmptyNode: boolean = false); overload;
    procedure DownloadMaterial(
                const AMaterialIdx: Integer;
                var AMediaStatus: TALDownloadingStatus;
                const aMediaNode: TALJSONNodeW;
                const aCropped: Boolean;
                const aDestSize: TSizeF;
                const aCornerRadius: TPointF;
                const ADrawFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc;
                const ADrawEmptyNode: boolean = false); overload;
    function CanStartDownloadMaterial(var AExtData: TObject): boolean;
    function DoCanStartDownloadMaterial(
               const aCompareKey: Integer;
               const aMaterialIdx: Integer): boolean; virtual;
    procedure OnSuccessDownloadMaterial(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AExtData: TObject);
    procedure OnErrorDownloadMaterial(const AErrMessage: string; var AExtData: Tobject);
    function ExtractMaterialFromRefreshingEphemeralLayout(const aMaterialIdx: Integer): TALDrawable; virtual;
    procedure BuildMaterial(var AExtData: TObject);
    function DoBuildMaterial(const ADownloadMaterialExtData: TALDynamicScrollBoxDownloadMaterialExtData): TALDrawable; virtual;
    procedure ShiftMaterial(
                const aCompareKey: Integer;
                const aMaterialIdx: Integer;
                const aMaterial: TALDrawable);
    procedure DoShiftMaterial(
                const aMaterialIdx: Integer;
                const aMaterial: TALDrawable); virtual;
  {$ENDREGION}
  //
  {$REGION 'Key'}
  protected
    function GetKey: integer; Virtual;
    function IsValidKey(Const aCompareKey: integer): Boolean; virtual;
  public
    Property Key: integer read GetKey;
  {$ENDREGION}
  //
  {$REGION 'Priority'}
  protected
    function GetPriority(const AExtData: Tobject): Int64; virtual;
  {$ENDREGION}
  //
  {$REGION 'Members'}
  protected
    //*function GetLocalRect: TRectF; inline;
    function GetExpandedLocalRect: TRectF; inline;
    function GetExpandedBoundsRect: TALRectD; inline;
  public
    Visible: boolean;
    //*Align: TAlign;
    AutoSize: Boolean;
    Cursor: TCursor;
    TouchTargetExpansion: TrectF;
    Tag: int64;
    property ExpandedBoundsRect: TALRectD read GetExpandedBoundsRect;
    //*property LocalRect: TRectF read GetLocalRect;
    property ExpandedLocalRect: TRectF read GetExpandedLocalRect;
  {$ENDREGION}
  //
  private
    FIsDestroying: boolean;
  public
    procedure BeforeDestroy; virtual;
    procedure AsyncDestroy; virtual;
    Property isDestroying: boolean read fisDestroying; //[MultiThread]
  end;

//////////////////////////////////////////////
/// THE CODE BELOW WAS AUTO-GENERATED FROM ///
/// <ALCINOE>\Tools\CodeBuilder.           ///
//////////////////////////////////////////////

{$REGION 'AUTO-GENERATED'}

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicScrollBoxShape = class(TALDynamicScrollBoxControl(*, IALShapeControl*))
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
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    procedure ShadowChanged(Sender: TObject); virtual;
  public
    constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    property Fill: TALBrush read GetFill write SetFill;
    property Stroke: TALStrokeBrush read GetStroke write SetStroke;
    property Shadow: TALShadow read GetShadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicScrollBoxBaseRectangle = class(TALDynamicScrollBoxShape)
  private
    fDoubleBuffered: boolean;
    FXRadius: Single;
    FYRadius: Single;
    FCorners: TCorners;
    FSides: TSides;
    FDefaultXRadius: Single;
    FDefaultYRadius: Single;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    function HasCustomDraw: Boolean; virtual;
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
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
    procedure DoResized; override;
  public
    constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
    property DefaultXRadius: Single read FDefaultXRadius write FDefaultXRadius;
    property DefaultYRadius: Single read FDefaultYRadius write FDefaultYRadius;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicScrollBoxRectangle = class(TALDynamicScrollBoxBaseRectangle)
  private
    FAutoSize: Boolean;
  protected
    function GetAutoSize: Boolean; virtual;
    procedure SetAutoSize(const Value: Boolean); virtual;
    procedure DoRealign; override;
    procedure AdjustSize; virtual;
    function HasUnconstrainedAutosizeX: Boolean; virtual;
    function HasUnconstrainedAutosizeY: Boolean; virtual;
  public
    constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
  published
    //property Action;
    property Align;
    //**property Anchors;
    // Dynamically adjusts the dimensions to accommodate child controls,
    // considering their sizes, positions, margins, and alignments.
    property AutoSize: Boolean read GetAutoSize write SetAutoSize default False;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //**property ClipChildren;
    //property ClipParent;
    property Corners;
    //**property Cursor;
    property DoubleBuffered;
    //**property DragMode;
    //**property EnableDragHighlight;
    //**property Enabled;
    property Fill;
    //**property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    //**property HitTest;
    //**property Locked;
    property Margins;
    //**property Opacity;
    property Padding;
    //**property PopupMenu;
    //**property Position;
    //**property RotationAngle;
    //**property RotationCenter;
    //**property Scale;
    property Shadow;
    property Sides;
    //property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    //**property TouchTargetExpansion;
    //**property Visible;
    //**property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    //**property OnDragEnter;
    //**property OnDragLeave;
    //**property OnDragOver;
    //**property OnDragDrop;
    //**property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    //**property OnMouseEnter;
    //**property OnMouseLeave;
    //**property OnMouseDown;
    //**property OnMouseUp;
    //**property OnMouseMove;
    //**property OnMouseWheel;
    //**property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    //**property OnPainting;
    //**property OnPaint;
    //property OnResize;
    //**property OnResized;
  end;

{$ENDREGION}

//////////////////////////////////////////////
/// THE CODE ABOVE WAS AUTO-GENERATED FROM ///
/// <ALCINOE>\Tools\CodeBuilder.           ///
//////////////////////////////////////////////

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALDynamicScrollBoxLayout is a container for other graphical objects.
  // Use the layouts when you need to organize multiple graphical controls under the same parent.
  TALDynamicScrollBoxLayout = Class(TALDynamicScrollBoxControl)
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALDynamicScrollBoxRectangle defines 2D rectangles.
  // This class is a clone of TALRectangle
  //*TALDynamicScrollBoxRectangle = Class(TALDynamicScrollBoxControl)
  //
  //*{$REGION 'OnPaint'}
  //*protected
  //*  procedure Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single); override; // ARect is local to the content
  //*{$ENDREGION'}
  //
  //*{$REGION 'BufBitmap'}
  //*private
  //*  fBufBitmap: TALDrawable;
  //*  fBufBitmapRect: TRectF;
  //*public
  //*  function MakeBufBitmap: TALDrawable; override;
  //*  procedure ClearBufBitmap; override;
  //*  property BufBitmap: TALDrawable read fBufBitmap;
  //*{$ENDREGION}
  //
  //*{$REGION 'Members'}
  //*public
  //*  FillColor: TAlphaColor;
  //*  StrokeColor: TalphaColor;
  //*  StrokeThickness: Single;
  //*  Sides: TSides;
  //*  Corners: TCorners;
  //*  XRadius: Single;
  //*  YRadius: Single;
  //*  ShadowColor: TAlphaColor;
  //*  shadowBlur: Single;
  //*  shadowOffsetX: Single;
  //*  shadowOffsetY: Single;
  //*{$ENDREGION}
  //
  //*public
  //*  constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
  //*  destructor Destroy; override;
  //*End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALDynamicScrollBoxText defines 2D text objects.
  // This class is a clone of TALText
  TALDynamicScrollBoxText = Class(TALDynamicScrollBoxControl)
  //
  {$REGION 'OnPaint'}
  protected
    procedure Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single); override; // ARect is local to the content
  {$ENDREGION'}
  //
  {$REGION 'BufBitmap'}
  private
    fBufBitmap: TALDrawable;
    fBufBitmapRect: TRectF;
    fBufTextBroken: Boolean;
    fBufAllTextDrawn: Boolean;
  public
    function MakeBufBitmap: TALDrawable; override;
    procedure ClearBufBitmap; override;
    property BufBitmap: TALDrawable read fBufBitmap;
    // True if the text was broken into several lines.
    property TextBroken: Boolean read FBufTextBroken;
    // True if all the text was drawn (no need for any ellipsis).
    property AllTextDrawn: Boolean read FBufAllTextDrawn;
  {$ENDREGION}
  //
  {$REGION 'Alignment'}
  protected
    procedure AdjustSize; override;
  {$ENDREGION}
  //
  {$REGION 'Members'}
  public
    Text: string;
    TextIsHtml: Boolean;
    FontName: String;
    FontStyle: TfontStyles;
    FontSize: Single;
    FontColor: TalphaColor;
    LineSpacing: Single;
    Trimming: TTextTrimming;
    HorzTextAlign: TTextAlign;
    VertTextAlign: TTextAlign;
    WordWrap: Boolean;
    FillColor: TalphaColor;
    StrokeColor: TalphaColor;
    StrokeThickness: Single;
    Sides: TSides;
    Corners: TCorners;
    XRadius: Single;
    YRadius: Single;
    MaxWidth: Single;
    MaxHeight: Single;
  {$ENDREGION}
  //
  public
    constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
    destructor Destroy; override;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // TALDynamicScrollBoxImage defines 2D image components.
  // Use TALDynamicScrollBoxImage to display a graphical image on a control.
  // This class is a clone of TALImage
  TALDynamicScrollBoxImage = Class(TALDynamicScrollBoxControl)
  //
  {$REGION 'OnPaint'}
  protected
    procedure Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single); override; // ARect is local to the content
  {$ENDREGION'}
  //
  {$REGION 'TouchEffect'}
  //*protected
  //*  function GetTouchEffectBoundsRect: TrectF; override;
  {$ENDREGION'}
  //
  {$REGION 'Bitmap'}
  strict private
    FBitmap: TALDrawable;
    FOwnBitmap: Boolean;
    FBitmapRect: TRectF;
  protected
    procedure SetBitmap(const AValue: TALDrawable);
    function GetBitmap: TALDrawable; virtual;
  strict private
    FBitmapStatus: TALDownloadingStatus;
    FBitmapStatusIcon: TALDrawable;
    FOwnBitmapStatusIcon: Boolean;
    FBitmapStatusIconRect: TRectF;
  protected
    procedure SetBitmapStatus(const AValue: TALDownloadingStatus);
    procedure SetBitmapStatusIcon(const AValue: TALDrawable);
  public
    BitmapStatusIconSize: TSizeF;
    property BitmapStatus: TALDownloadingStatus read FBitmapStatus write SetBitmapStatus;
    property BitmapStatusIcon: TALDrawable read FBitmapStatusIcon write SetBitmapStatusIcon;
  public
    type
      TBitmapWrapMode = (
        Stretch,
        FitInto,
        FitIntoAndCrop,
        FitIntoAndCropAsCircle);
    type
      THorzBitmapAlign = (Center, Left, Right);
      TVertBitmapAlign = (Center, Top, Bottom);
    type
      TGetBitmapFunc = function(Const ASender: TALDynamicScrollBoxImage): TALDrawable of Object;
  public
    BitmapURL: String;
    BitmapResourceName: String;
    BitmapSize: TSizeF;
    BitmapBlurred: single;
    BitmapCropCenter: TpointF;
    HorzBitmapAlign: THorzBitmapAlign;
    VertBitmapAlign: TVertBitmapAlign;
    BitmapCornerRadius: TPointF;
    BitmapWrapMode: TBitmapWrapMode;
    AlwaysDrawBitmap: Boolean;
    GetBitmapFunc: TGetBitmapFunc;
    DrawBitmapFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc;
    property Bitmap: TALDrawable read GetBitmap write SetBitmap;
    procedure SetBitmapUrl(const aMediaNode: TALJSONNodeW; const aCropped: Boolean = true);
  {$ENDREGION}
  //
  {$REGION 'BufBitmap'}
  public
    function MakeBufBitmap: TALDrawable; override;
  {$ENDREGION}
  //
  {$REGION 'Materials'}
  protected
    procedure DoDownloadMaterials; override;
    procedure DoShiftMaterial(
                const aMaterialIdx: Integer;
                const aMaterial: TALDrawable); override;
  {$ENDREGION}
  //
  public
    constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
    destructor Destroy; override;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicScrollBoxPreloadEphemeralLayoutExtData = Class(Tobject)
  private
    FComparekey: integer;
  public
    constructor Create(const AComparekey: Integer); virtual;
    Property Comparekey: integer read FComparekey;
  End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicScrollBoxItemEphemeralLayout = class(TALDynamicScrollBoxLayout)
  public
    constructor Create(const AOwner: TALDynamicScrollBoxItem); reintroduce; virtual;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // An Item area represents a non-scrollable section, managing ephemeral
  // layout that are dynamically created or deleted as needed.
  TALDynamicScrollBoxItem = class(TALDynamicScrollBoxControl)
  //
  {$REGION 'Items'}
  private
    FItemIndex: Integer;
    procedure setItemIndex(const aValue: integer);
  public
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
  {$ENDREGION}
  //
  {$REGION 'Data'}
  protected
    type
      TGrowDataBackgroundThread = class(TThread)
      private
        fOwner: TALDynamicScrollBoxItem;
      protected
        procedure Execute; override;
      public
        constructor Create(const AOwner: TALDynamicScrollBoxItem); reintroduce; virtual;
      end;
  private
    FData: TALJSONNodeW; // never reseted - when read the owner frame must be locked via beginread or via beginwrite when write
    FCanGrowData: Boolean;
    FGrowDataExecutedAtLeastOnce: Boolean;
    FGrowDataError: Boolean;
    FShowApiErrorIfGrowDataError: Boolean;
    FGrowDataLock: Tobject;
    FGrowDataBackgroundThreadsCount: integer;
    FEnforceEphemeralLayoutWhenGrowDataFinished: Boolean;
    function GetData: TALJSONNodeW; inline;
  protected
    function GrowDataIsRunning: Boolean;
    property GrowDataExecutedAtLeastOnce: Boolean read FGrowDataExecutedAtLeastOnce write FGrowDataExecutedAtLeastOnce;
    property ShowApiErrorIfGrowDataError: Boolean read FShowApiErrorIfGrowDataError write FShowApiErrorIfGrowDataError;
    function  GrowData(const aForceGrow: Boolean = False): boolean; virtual;
    function GrowDataCreateBackgroundThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread; virtual;
    procedure GrowDataBackgroundProc(const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread); virtual;
    function  GrowDataBackgroundProcDownloadData(
                const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread;
                const aResultNode: TALJSONNodeW): Boolean; virtual;
    procedure GrowDataBackgroundProcInitData(const aDataNode: TALJSONNodeW); virtual;
    function  GrowDataBackgroundProcCanProcessData: boolean; virtual;
    procedure GrowDataProcessData(
                const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread;
                const ADataLoadedSuccessfully: Boolean;
                var ADownloadedData: TALJSONNodeW);
    procedure GrowDataFinish(
                const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread;
                const ADataLoadedSuccessfully: Boolean); virtual;
    procedure GrowDataFinished(const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread); virtual;
    function IsGrowDataFinished: Boolean;
  public
    property Data: TALJSONNodeW read GetData write Fdata;
    function HasData: boolean; inline;
    property CanGrowData: boolean read FCanGrowData write FCanGrowData;
    property GrowDataError: boolean read FGrowDataError;
  {$ENDREGION}
  //
  {$REGION 'EphemeralLayout'}
  private
    FIsRefreshingEphemeralLayout: Boolean;
    FRefreshingEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout;
    FEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout; // soft link as the EphemeralLayout is also added to the controls list
    FEphemeralLayoutStatus: TALDownloadingStatus;
  protected
    procedure PreloadEphemeralLayout(var AExtData: Tobject);
    function CanPreloadEphemeralLayout(const aCompareKey: Integer): Boolean;
    function DoCanPreloadEphemeralLayout(const aCompareKey: Integer): Boolean; virtual;
    function BuildEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout;
    function DoBuildEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout; virtual;
    procedure ShiftEphemeralLayout(
                const aCompareKey: Integer;
                const aEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout);
    procedure DoShiftEphemeralLayout(const aEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout); virtual;
  public
    function EnforceEphemeralLayout(const a4Preload: boolean): TALDynamicScrollBoxItemEphemeralLayout; overload; virtual;
    function EnforceEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout; overload; virtual;
    property EphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout read FEphemeralLayout;
    property EphemeralLayoutStatus: TALDownloadingStatus read FEphemeralLayoutStatus write FEphemeralLayoutStatus;
    procedure clearEphemeralLayout; virtual;
    procedure RefreshEphemeralLayout; virtual;
    property IsRefreshingEphemeralLayout: boolean read FIsRefreshingEphemeralLayout;
    property RefreshingEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout read FRefreshingEphemeralLayout;
  {$ENDREGION}
  //
  {$REGION 'Materials'}
  protected
    procedure DownloadMaterials; override;


    procedure DownloadMaterial(
                const AMaterialIdx: Integer;
                var AMediaStatus: TALDownloadingStatus;
                const aMediaNode: TALJSONNodeW;
                const aCropped: Boolean;
                const aDestSize: TSizeF;
                const aCornerRadius: TPointF;
                const ADrawFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc;
                const ADrawEmptyNode: boolean = false);
    function CanStartDownloadMaterial(var AExtData: TObject): boolean;
    function DoCanStartDownloadMaterial(
               const aCompareKey: Integer;
               const aMaterialIdx: Integer): boolean; virtual;
    procedure OnSuccessDownloadMaterial(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AExtData: TObject);
    procedure OnErrorDownloadMaterial(const AErrMessage: string; var AExtData: Tobject);
    function ExtractMaterialFromRefreshingEphemeralLayout(const aMaterialIdx: Integer): TALDrawable; virtual;
    procedure BuildMaterial(var AExtData: TObject);
    function DoBuildMaterial(const ADownloadMaterialExtData: TALDynamicScrollBoxDownloadMaterialExtData): TALDrawable; virtual;
    procedure ShiftMaterial(
                const aCompareKey: Integer;
                const aMaterialIdx: Integer;
                const aMaterial: TALDrawable);
    procedure DoShiftMaterial(
                const aMaterialIdx: Integer;
                const aMaterial: TALDrawable); virtual;
  {$ENDREGION}
  //
  {$REGION 'Key'}
  private
    // used to sync the thread - never reseted always
    // incremented and manipulated via Atomicxxx functions
    FKey: integer;
  protected
    function GetKey: integer; override;
    function IsValidKey(Const aCompareKey: integer): Boolean; override;
  {$ENDREGION}
  //
  {$REGION 'Priority'}
  private
    FPriority: int64; // [MultiThread]
    FPriorityFactor: Int64; // [MultiThread]
  protected
    procedure UpdatePriority; virtual;
    function GetPriority(const AExtData: Tobject): Int64; overload;
  {$ENDREGION}
  //
  {$REGION 'Coordinates conversion'}
  public
    function LocalToFrame(const APoint: TALPointD): TALPointD; overload; override;
    function LocalToFrame(const ARect: TALRectD): TALRectD; overload; override;
    function FrameToLocal(const APoint: TPointF): TALPointD; overload; override;
    function FrameToLocal(const ARect: TRectF): TALRectD; overload; override;
  {$ENDREGION}
  //
  {$REGION 'Selection'}
  private
    FDeleted: Boolean; // when true the item is ignored. We never delete an item, we only set hidden to true if we want to hide it
    FSelected: boolean;
    FCanBeSelected: boolean;
  public
    property Deleted: Boolean read FDeleted;
    property Selected: Boolean read FSelected write FSelected;
    property CanBeSelected: Boolean read FCanBeSelected write FCanBeSelected;
  {$ENDREGION'}
  //
  {$REGION 'MouseEvents'}
  protected
    procedure MouseDown(const Sender: TObject; const AMousePos: TPointF); override; // AMousePos is local to the control
    procedure Click(const Sender: TObject; const AMousePos: TPointF); override; // AMousePos is local to the control
  {$ENDREGION'}
  //
  {$REGION 'OnPaint'}
  protected
    procedure Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single); override; // ARect is local to the control
  {$ENDREGION'}
  //
  {$REGION 'Members'}
  private
    FVisibleInPaintArea: Boolean;
    function GetVisibleInPaintArea: Boolean; inline;
  protected
    procedure SetBoundsRect(AValue: TALRectD); override;
  public
    property VisibleInPaintArea: Boolean read GetVisibleInPaintArea write FVisibleInPaintArea;
  {$ENDREGION}
  //
  public
    constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Prepare; virtual;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //*TALDynamicScrollBoxViewActionBar = Class(TALDynamicScrollBoxRectangle)
  //
  //*{$REGION 'MouseEvents'}
  //*protected
  //*   procedure RefreshBtnClick(Sender: TObject; AMousePos: TPointF); virtual;
  //*   procedure PreviousBtnClick(Sender: TObject; AMousePos: TPointF); virtual;
  //*{$ENDREGION'}
  //
  //*{$REGION 'Members'}
  //*public
  //*  // Indicates whether the action bar has the capability to collapse (hide)
  //*  // when the user scrolls down. When set to True, the action bar will
  //*  // hide upon scrolling down and reappear when scrolling up. When set to
  //*  // False, the action bar remains fixed regardless of the scrolling behavior.
  //*  CanCollapse: Boolean;
  //*  Title: TALDynamicScrollBoxText;
  //*  PreviousBtn: TALDynamicScrollBoxImage;
  //*  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  //*  RefreshBtn: TALDynamicScrollBoxImage;
  //*  {$ENDIF}
  //*{$ENDREGION}
  //
  //*public
  //*  constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
  //*End;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // The creation of the view EphemeralLayout is initiated under three
  // distinct conditions:
  //  * When the view is Prepareed up: If the view isn't visible within the paint
  //    area, the EphemeralLayout creation process commences in the background;
  //    otherwise, it takes place in the main thread.
  //  * When the view's size is altered using SetBoundsRect: Again, if the view
  //    isn't visible within the paint area, the EphemeralLayout creation
  //    process begins in the background; in all other instances, it is
  //    performed in the main thread.
  //  * When painting the view: In this scenario, the creation of the
  //    EphemeralLayout always occurs in the main thread.
  TALDynamicScrollBoxViewEphemeralLayout = class(TALDynamicScrollBoxItemEphemeralLayout)
  public
    // this if the position of the waiting Animation icon that we show
    // at the bottom (or at the top if ItemsReversedOrientation) when
    // we are growing the items.
    GrowItemsAniindicatorPos: TAlPointD;
    //------
    // this if the position of the Reload Button that we show
    // at the bottom (or at the top if ItemsReversedOrientation) when
    // we get an error when growing the items to ask the user to
    // manually redo the operation.
    ItemsReloadBtnPos: TAlPointD;
    ItemsReloadBtnTouchTargetRect: TalRectD;
    //------
    NoItemsIcon: Boolean;
    NoItemsIconPos: TALPointD;
    NoItemsIconTouchTargetRect: TALRectD;
    NoItemsTitle: Boolean;
    NoItemsTitlePos: TALPointD;
    NoItemsTitleTouchTargetRect: TALRectD;
    NoItemsSubTitle: Boolean;
    NoItemsSubTitlePos: TALPointD;
    NoItemsSubTitleTouchTargetRect: TALRectD;
    NoItemsBtn: Boolean;
    NoItemsBtnPos: TALPointD;
    NoItemsBtnTouchTargetRect: TALRectD;
    //-----
    constructor Create(const AOwner: TALDynamicScrollBoxItem); override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // A view implements a scrollable area (either horizontal or vertical),
  // managing ephemeral layout that are dynamically created or deleted as needed.
  TALDynamicScrollBoxView = class(TALDynamicScrollBoxItem)
  //
  {$REGION 'Controls'}
  protected
    procedure InsertControl(const AControl: TALDynamicScrollBoxControl); override;
    procedure RemoveControl(const AControl: TALDynamicScrollBoxControl); override;
    function GetControlAtPos(
               const aPos: TPointF; // APos is local to the control
               out AControlPos: TPointF; // AControlPos is local to the founded Control
               const ADirectChildOnly: Boolean = False;
               const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl; override;
  {$ENDREGION'}
  //
  {$REGION 'Items'}
  public
    type
      TItemIDType = (Int64, Text);
  protected
    type
      TGrowItemsBackgroundThread = class(TThread)
      private
        fOwner: TALDynamicScrollBoxView;
        FInsertAt: integer;
        FItems: TALJSONNodeW;
        FMaxItems: integer;
      protected
        procedure Execute; override;
      public
        constructor Create(
                      const AOwner: TALDynamicScrollBoxView;
                      const AInsertAt: integer;
                      const AItems: TALJSONNodeW; // The responsibility of freeing the memory used by AItems lies with this function.
                      const AMaxItems: integer); reintroduce; virtual;
        destructor Destroy; override;
        property InsertAt: integer read FInsertAt;
        property Items: TALJSONNodeW read FItems;
        property MaxItems: integer read FMaxItems;
      end;
  private
    FItemsOrientation: TOrientation; // Horizontal, Vertical
    FItemsReversedOrientation: boolean; // when true it's mean that items are ordered ▲ or ▶ from the bottom (first item) to the top (last item) like for exemple a whatsapp conversation. Else items are ordered ▼ or ▶ from the top (first item) to the bottom (last item)
    FItemIDType: TItemIDType;
    FItemIDPath: string;
    FItemInt64Ids: TDictionary<Int64, boolean>; // Used to unduplicate vertical items who contain int64 ids. Must be locked via LockItemIds/UnLockItemIds before used
    FItemStrIds: TDictionary<String, boolean>; // Used to unduplicate vertical items who contain string ids. Must be locked via LockItemIds/UnLockItemIds before used
    FItems: TArray<TALDynamicScrollBoxItem>;
    FCanGrowItems: Boolean;
    FGrowItemsAtIdx: integer;
    FGrowItemsExecutedAtLeastOnce: Boolean;
    FGrowItemsError: Boolean;
    FShowApiErrorIfGrowItemsError: boolean;
    FDownloadItemsNextPageToken: String; // must be used only between Tmonitor.Enter(FGrowItemsLock) .. Tmonitor.exit(FGrowItemsLock)
    FGrowItemsLock: Tobject;
    FGrowItemsBackgroundThreadsCount: integer;
    FIsArrangingItems: Boolean;
  private
    procedure LockItemIds;
    procedure UnLockItemIds;
    function GetItem(Index: Integer): TALDynamicScrollBoxItem;
    function GetItemsCount: integer;
  public
    Type
      TArrangeItemsCause = (ItemsInsert, ItemDelete, ItemResize, Other);
  protected
    function GetMaxItems: Integer; virtual;
    property MaxItems: integer read GetMaxItems;
    function CreateItem: TALDynamicScrollBoxItem; virtual;
    procedure AddItem(const aItem: TALDynamicScrollBoxItem); virtual;
    function GetNearestItemIdx: integer; // Visually farthest in this direction ▲ or ◀ (independandly of the value of ItemsReversedOrientation)
    function GetNearestItemBottom: Double; // Visually farthest in this direction ▲ or ◀ (independandly of the value of ItemsReversedOrientation)
    function GetNearestItemRight: Double; // Visually farthest in this direction ▲ or ◀ (independandly of the value of ItemsReversedOrientation)
    function GetFarthestItemIdx: integer; // Visually farthest in this direction ▼ or ▶ (independandly of the value of ItemsReversedOrientation)
    function GetFarthestItemBottom: Double; // Visually farthest in this direction ▼ or ▶ (independandly of the value of ItemsReversedOrientation)
    function GetFarthestItemRight: Double; // Visually farthest in this direction ▼ or ▶ (independandly of the value of ItemsReversedOrientation)
    function HasVisibleItems: Boolean;
    //-----
    function GrowItemsIsRunning: Boolean;
    property GrowItemsExecutedAtLeastOnce: Boolean read FGrowItemsExecutedAtLeastOnce write FGrowItemsExecutedAtLeastOnce;
    property ShowApiErrorIfGrowItemsError: Boolean read FShowApiErrorIfGrowItemsError write FShowApiErrorIfGrowItemsError;
    function  GrowItems(
                const aForceGrow: Boolean = False;
                const AInsertAt: integer = -1;
                const AItems: TALJSONNodeW = nil;
                const AAsync: Boolean = True): boolean; virtual;
    function GrowItemsCreateBackgroundThread(
               const AInsertAt: integer;
               const AItems: TALJSONNodeW): TALDynamicScrollBoxView.TGrowItemsBackgroundThread; virtual;
    procedure GrowItemsBackgroundProc(const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread); virtual;
    function  GrowItemsBackgroundProcDownloadItems(
                const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread;
                const aResultNode: TALJSONNodeW;
                var aNextPageToken: String): Boolean; virtual;
    function  GrowItemsBackgroundProcCanAddItem(const aItemID: int64): boolean; overload; virtual;
    function  GrowItemsBackgroundProcCanAddItem(const aItemID: String): boolean; overload; virtual;
    procedure GrowItemsBackgroundProcInitItem(const aItemNode: TALJSONNodeW); virtual;
    function  GrowItemsBackgroundProcCanProcessItems: boolean; virtual;
    procedure GrowItemsProcessItems(
                const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread;
                const AItemsLoadedSuccessfully: Boolean;
                const ADownloadedItems: TALJSONNodeW); virtual;
    procedure GrowItemsFinish(
                const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread;
                const AItemsLoadedSuccessfully: Boolean); virtual;
    procedure GrowItemsFinished(const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread); virtual;
    //-----
    Procedure DoArrangeItems(
                const ACause: TALDynamicScrollBoxView.TArrangeItemsCause;
                const AFromIdx: integer;
                const AToIdx: integer); virtual;
    Procedure DoAfterItemsArranged; virtual;
  public
    procedure DeleteItem(Const aItem: TALDynamicScrollBoxItem);
    // This procedure is called by the following functions:
    // * TALDynamicScrollBoxItem.SetBoundsRect
    // * TALDynamicScrollBoxView.AddItem (ie: When we insert a single item)
    // * TALDynamicScrollBoxView.GrowItemsProcessItems (ie: When we insert items in bulk)
    // * TALDynamicScrollBoxView.DeleteItem
    // * TALDynamicScrollBoxItem.ShiftEphemeralLayout (ie: Only when the item is a view)
    procedure ArrangeItems(
                const ACause: TALDynamicScrollBoxView.TArrangeItemsCause;
                const AFromIdx: integer;
                const AToIdx: integer);
    // This is where the minimum and maximum scrolling limits should be set.
    // Additionally, specific UI controls (like GrowItemsAniIndicator,
    // ItemsReloadBtn, etc.) should be positioned within the scrollable area.
    Procedure AfterItemsArranged(
                const AAlterScrollPosValue: Boolean = False;
                const ANewScrollPosValue: Double = 0); virtual;
    procedure ArrangeNoItemsEphemeralLayout(const ARect: TRectF);
    property Items[Index: Integer]: TALDynamicScrollBoxItem read GetItem;
    property ItemsCount: Integer read GetItemsCount;
    property CanGrowItems: boolean read FCanGrowItems write FCanGrowItems;
    property GrowItemsAtIdx: integer read FGrowItemsAtIdx write FGrowItemsAtIdx;
    property GrowItemsError: boolean read FGrowItemsError;
    property ItemsOrientation: TOrientation read FItemsOrientation write FItemsOrientation;
    property ItemsReversedOrientation: boolean read FItemsReversedOrientation write FItemsReversedOrientation;
    property ItemIDType: TItemIDType read FItemIDType write FItemIDType;
    property ItemIDPath: String read FItemIDPath write FItemIDPath;
    property IsArrangingItems: Boolean read FIsArrangingItems write FIsArrangingItems;
  {$ENDREGION}
  //
  {$REGION 'NewItem'}
  private
    fNewItemQueue: TObjectlist<TALJSONNodeW>;
    fNewItemAnim: TALFloatAnimation;
    Procedure CreateNewItemAnim;
  protected
    procedure EnqueueNewItem(const AItem: TALJSONNodeW); // The responsibility of freeing the memory used by AItem lies with this function.
    procedure ProcessNewItemQueue;
    procedure NewItemAnimFinish(Sender: TObject); virtual;
    function CanDoNewItemAnim(const ANewItem: TALDynamicScrollBoxItem): boolean; virtual;
    function CanDoNewItemVibrate(const ANewItem: TALDynamicScrollBoxItem): boolean; virtual;
  public
    procedure AddNewItem(
                const AItem: TALJSONNodeW;
                const aFromQueue: boolean = False;
                const AInsertAt: integer = -1;
                const AAsync: Boolean = True); // The responsibility of freeing the memory used by AItem lies with this function.
    property NewItemAnim: TALFloatAnimation read fNewItemAnim;
  {$ENDREGION'}
  //
  {$REGION 'EphemeralLayout'}
  private
    function GetEphemeralLayout: TALDynamicScrollBoxViewEphemeralLayout; inline;
  protected
    function DoBuildEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout; override;
  public
    function EnforceEphemeralLayout(const a4Preload: boolean): TALDynamicScrollBoxViewEphemeralLayout; reintroduce; overload;
    function EnforceEphemeralLayout: TALDynamicScrollBoxViewEphemeralLayout; reintroduce; overload;
    property EphemeralLayout: TALDynamicScrollBoxViewEphemeralLayout read GetEphemeralLayout;
    procedure clearEphemeralLayout; override;
  {$ENDREGION}
  //
  {$REGION 'ActionBar'}
  //*protected
  //*  function CreateActionBar: TALDynamicScrollBoxViewActionBar; virtual;
  //*  function GetActionBarHeight: Single;
  //*public
  //*  ActionBar: TALDynamicScrollBoxViewActionBar;
  {$ENDREGION'}
  //
  {$REGION 'ScrollPos'}
  private
    FScrollPos: double;
    FScrollFirstVisibleItemIdx: integer;
    FScrollLastVisibleItemIdx: integer;
    FScrollFirstPreloadedItemIdx: Integer;
    FScrollLastPreloadedItemIdx: Integer;
    FScrollFromFirstToLast: Boolean;
    FScrollMinBound: Double;
    FScrollMaxBound: Double;
    FSetScrollPosGuard: Boolean;
    FPostponedScrollPosSet: boolean;
    FPostponedScrollPos: Double;
    function GetScrollPos: Double; inline;
  protected
    function CalculateScrollFirstVisibleItemIdx: integer;
    function CalculateScrollLastVisibleItemIdx: integer;
  public
    property ScrollPos: Double read GetScrollPos;
    procedure SetScrollPos(const AValue: Double; const AEnforceScrollLimits: boolean = True; const ATriggeredByScrollEvent: boolean = False); virtual;
    property ScrollFirstVisibleItemIdx: integer read FScrollFirstVisibleItemIdx;
    property ScrollLastVisibleItemIdx: integer read FScrollLastVisibleItemIdx;
    property ScrollFirstPreloadedItemIdx: Integer read FScrollFirstPreloadedItemIdx;
    property ScrollLastPreloadedItemIdx: Integer read FScrollLastPreloadedItemIdx;
    // When true it's mean mean we scroll (independandly of the value of
    // ItemsReversedOrientation) in this direction: ▼ or ▶ Else it's mean we
    // scroll in this direction: ▲ or ◀
    property ScrollFromFirstToLast: Boolean read FScrollFromFirstToLast;
    // ScrollMinBound represents the position of the uppermost control.
    // ScrollMaxBound represents the position of the lowest control.
    // These are not necessarily the limits of the ScrollPos value.
    property ScrollMinBound: Double read FScrollMinBound write FScrollMinBound;
    property ScrollMaxBound: Double read FScrollMaxBound write FScrollMaxBound;
  {$ENDREGION}
  //
  {$REGION 'ScrollEngine'}
  private
    FScrollEngine: TALScrollEngine;
  protected
    procedure ScrollEngineChanged(Sender: TObject); virtual;
    procedure ScrollEngineStart(Sender: TObject); virtual;
    procedure ScrollEngineStop(Sender: TObject); virtual;
    function GetScrollEnginesAtPos(const aPos: TPointF): TArray<TALScrollEngine>;
  public
    property ScrollEngine: TALScrollEngine read fScrollEngine;
  {$ENDREGION'}
  //
  {$REGION 'Priority'}
  protected
    procedure UpdatePriority; override;
  public
    function CanUpdateGlobalThreadPoolPriority: boolean;
    procedure UpdateGlobalThreadPoolPriority;
  {$ENDREGION}
  //
  {$REGION 'Coordinates conversion'}
  public
    function LocalToScroll(const APoint: TPointF): TALPointD; overload;
    function LocalToScroll(const ARect: TRectF): TALRectD; overload;
    function ScrollToLocal(const APoint: TALPointD): TALPointD; overload;
    function ScrollToLocal(const ARect: TALRectD): TALRectD; overload;
    function ScrollToFrame(const APoint: TALPointD): TALPointD; overload;
    function ScrollToFrame(const ARect: TALRectD): TALRectD; overload;
  {$ENDREGION}
  //
  {$REGION 'MouseEvents'}
  protected
    procedure ItemsReloadBtnClick(Sender: Tobject); virtual;
    procedure NoItemsBtnClick(Sender: Tobject); virtual;
    procedure Click(const Sender: TObject; const AMousePos: TPointF); override; // AMousePos is local to the view
    procedure MouseMove(const Sender: TObject; const AMousePos: TPointF); override; // AMousePos is local to the view
  {$ENDREGION'}
  //
  {$REGION 'PreloadOffsets'}
  protected
    function GetItemsPreloadOffset: integer; virtual;
  protected
    property ItemsPreloadOffset: integer read GetItemsPreloadOffset;
  {$ENDREGION'}
  //
  {$REGION 'Measures'}
  protected
    function GetMeasures(Index: Integer): Single; virtual;
  protected
    property ItemsReloadBtnWidth: single index 0 read GetMeasures;
    property ItemsReloadBtnHeight: single index 1 read GetMeasures;
    property ItemsReloadBtnMarginTop: single index 2 read GetMeasures;
    property ItemsReloadBtnMarginBottom: single index 3 read GetMeasures;
    property ItemsReloadBtnMarginRight: single index 24 read GetMeasures;
    property ItemsReloadBtnMarginLeft: single index 25 read GetMeasures;
    //-----
    property GrowItemsAniindicatorMarginTop: Single index 4 read GetMeasures;
    property GrowItemsAniindicatorMarginBottom: Single index 5 read GetMeasures;
    property GrowItemsAniindicatorMarginRight: Single index 26 read GetMeasures;
    property GrowItemsAniindicatorMarginLeft: Single index 27 read GetMeasures;
    //-----
    property NoItemsMarginTop: Single index 6 read GetMeasures;
    property NoItemsMarginBottom: Single index 7 read GetMeasures;
    property NoItemsMarginLeft: Single index 8 read GetMeasures;
    property NoItemsMarginRight: Single index 9 read GetMeasures;
    property NoItemsIconWidth: Single index 10 read GetMeasures;
    property NoItemsIconHeight: Single index 11 read GetMeasures;
    property NoItemsTitleTextSize: Single index 12 read GetMeasures;
    property NoItemsTitleLineSpacing: Single index 13 read GetMeasures;
    property NoItemsTitleMarginTop: Single index 14 read GetMeasures;
    property NoItemsSubTitleTextSize: Single index 15 read GetMeasures;
    property NoItemsSubTitleLineSpacing: Single index 16 read GetMeasures;
    property NoItemsSubTitleMarginTop: Single index 17 read GetMeasures;
    property NoItemsBtnTextSize: Single index 18 read GetMeasures;
    property NoItemsBtnLineSpacing: Single index 19 read GetMeasures;
    property NoItemsBtnMarginTop: Single index 20 read GetMeasures;
    property NoItemsBtnRadius: Single index 21 read GetMeasures;
    property NoItemsBtnPaddingX: Single index 22 read GetMeasures;
    property NoItemsBtnPaddingY: Single index 23 read GetMeasures;
  {$ENDREGION}
  //
  {$REGION 'Bitmaps'}
  protected
    function GetBitmap(Index: Integer): TALDrawable; virtual;
  protected
    property ItemsReloadBtn: TALDrawable index 0 read GetBitmap;
    property NoItemsIcon: TALDrawable index 1 read GetBitmap;
    property NoItemsTitle: TALDrawable index 2 read GetBitmap;
    property NoItemsSubTitle: TALDrawable index 3 read GetBitmap;
    property NoItemsBtn: TALDrawable index 4 read GetBitmap;
  {$ENDREGION}
  //
  {$REGION 'OnPaint'}
  protected
    procedure Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single); override; // ARect is local to the view
  {$ENDREGION'}
  //
  {$REGION 'Members'}
  protected
    procedure SetBoundsRect(AValue: TALRectD); override;
  {$ENDREGION'}
  //
  public
    constructor Create(const AOwner: TALDynamicScrollBoxControl); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestroy; override;
    procedure AsyncDestroy; override;
    procedure Prepare; override;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDynamicScrollBox = class(TALControl)
  //
  {$REGION 'MainView'}
  private
    FMainView: TALDynamicScrollBoxView;
  protected
    function CreateMainView: TALDynamicScrollBoxView; virtual;
  public
    property MainView: TALDynamicScrollBoxView read FMainView;
    function CalculateMainViewLocalRect: TALRectD;
  {$ENDREGION}
  //
  {$REGION 'PreloadOffsets'}
  protected
    ItemsPreloadOffset: integer;
    Procedure InitPreloadOffsets;
    Procedure DoInitPreloadOffsets; virtual;
  {$ENDREGION'}
  //
  {$REGION 'Measures'}
  Private
    FFrameWidth: Single;          //
    FFrameHeight: Single;         // used by FrameResized to know
    FFrameAffineRatio: Single;    // if the frame was really resized
    FFrameKeyboardHeight: Single; //
    //-----
    FMainViewWidth: Single;  // If the frame have a panel (ex conversation with send message bottom panel) then the height
    FMainViewHeight: Single; // and width of the view could be distinct from the height and width of the frame
  protected
    property MainViewWidth: Single read FMainViewWidth;
    property MainViewHeight: Single read FMainViewHeight;
  protected
    ErrorMessageBannerTextSize: Single;
    ErrorMessageBannerLineSpacing: single;
    ErrorMessageBannerPadding: single;
    //-----
    GPSRequiredMessageBannerTextSize: Single;
    GPSRequiredMessageBannerLineSpacing: single;
    GPSRequiredMessageBannerPadding: single;
    //-----
    ItemsReloadBtnWidth: single;
    ItemsReloadBtnHeight: single;
    ItemsReloadBtnMarginTop: single;
    ItemsReloadBtnMarginBottom: single;
    ItemsReloadBtnMarginRight: single;
    ItemsReloadBtnMarginLeft: single;
    //-----
    RefreshIconWidth: single;
    RefreshIconHeight: single;
    //-----
    GrowItemsAniindicatorMarginTop: Single;
    GrowItemsAniindicatorMarginBottom: Single;
    GrowItemsAniindicatorMarginRight: Single;
    GrowItemsAniindicatorMarginLeft: Single;
    //-----
    NoItemsMarginTop: Single;
    NoItemsMarginBottom: Single;
    NoItemsMarginLeft: Single;
    NoItemsMarginRight: Single;
    NoItemsIconWidth: Single;
    NoItemsIconHeight: Single;
    NoItemsTitleTextSize: Single;
    NoItemsTitleLineSpacing: Single;
    NoItemsTitleMarginTop: Single;
    NoItemsSubTitleTextSize: Single;
    NoItemsSubTitleLineSpacing: Single;
    NoItemsSubTitleMarginTop: Single;
    NoItemsBtnTextSize: Single;
    NoItemsBtnLineSpacing: Single;
    NoItemsBtnMarginTop: Single;
    NoItemsBtnRadius: Single;
    NoItemsBtnPaddingX: Single;
    NoItemsBtnPaddingY: Single;
    //------
    FilterDialogMainBtnRect: TrectF;
    //-----
    TouchTargetExpansion: single;
    //-----
    Procedure InitMeasures;
    Procedure DoInitMeasures; virtual;
  {$ENDREGION}
  //
  {$REGION 'Bitmaps'}
  strict private
    fErrorMessageBanner: TALDrawable;
    fGPSRequiredMessageBanner: TALDrawable;
    fItemsReloadBtn: TALDrawable;
    fRefreshIcon: TALDrawable;
    fRefreshingAniIcon: TALDrawable;
    fNoItemsIcon: TALDrawable;
    fNoItemsTitle: TALDrawable;
    fNoItemsSubTitle: TALDrawable;
    fNoItemsBtn: TALDrawable;
    fFilterDialogMainBtn: TALDrawable;
  protected
    procedure CreateErrorMessageBannerAnim; virtual;
    function GetErrorMessageBanner: TALDrawable; virtual;
    function DrawErrorMessageBanner: TALDrawable; virtual;
    procedure CreateGPSRequiredMessageBannerAnim; virtual;
    function GetGPSRequiredMessageBanner: TALDrawable; virtual;
    function DrawGPSRequiredMessageBanner: TALDrawable; virtual;
    function GetItemsReloadBtn: TALDrawable; virtual;
    function DrawItemsReloadBtn: TALDrawable; virtual;
    function GetRefreshIcon: TALDrawable; virtual;
    function DrawRefreshIcon: TALDrawable; virtual;
    function GetRefreshingAniIcon: TALDrawable; virtual;
    function DrawRefreshingAniIcon: TALDrawable; virtual;
    function GetNoItemsIcon: TALDrawable; virtual;
    function DrawNoItemsIcon: TALDrawable; virtual;
    function GetNoItemsTitle: TALDrawable; virtual;
    function DrawNoItemsTitle: TALDrawable; virtual;
    function GetNoItemsSubTitle: TALDrawable; virtual;
    function DrawNoItemsSubTitle: TALDrawable; virtual;
    function GetNoItemsBtn: TALDrawable; virtual;
    function DrawNoItemsBtn: TALDrawable; virtual;
    function GetFilterDialogMainBtn: TALDrawable; virtual;
    function DrawFilterDialogMainBtn: TALDrawable; virtual;
    Procedure InitBitmaps;
    Procedure DoInitBitmaps; virtual;
    Procedure FreeBitmaps; virtual;
  public
    ErrorMessageBannerAnim: TALFloatAnimation;
    GPSRequiredMessageBannerAnim: TALFloatAnimation;
    property ErrorMessageBanner: TALDrawable read GetErrorMessageBanner;
    property GPSRequiredMessageBanner: TALDrawable read GetGPSRequiredMessageBanner;
    property ItemsReloadBtn: TALDrawable read GetItemsReloadBtn;
    property RefreshIcon: TALDrawable read GetRefreshIcon;
    property RefreshingAniIcon: TALDrawable read GetRefreshingAniIcon;
    property NoItemsIcon: TALDrawable read GetNoItemsIcon;
    property NoItemsTitle: TALDrawable read GetNoItemsTitle;
    property NoItemsSubTitle: TALDrawable read GetNoItemsSubTitle;
    property NoItemsBtn: TALDrawable read GetNoItemsBtn;
    property FilterDialogMainBtn: TALDrawable read GetFilterDialogMainBtn;
    class function FitIntoAndCropAsCircleImage(
                     const aSender: Tobject;
                     const aPicStream: TCustomMemoryStream;
                     const aPicUrl: String;
                     const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
                     Const aPicBlurred: single;
                     const aPicCropCenter: TPointF;
                     const aCornerRadius: TPointF; // Useless here
                     const aScreenScale: single): TALDrawable; static;
    class function FitIntoAndCropImage(
                     const aSender: Tobject;
                     const aPicStream: TCustomMemoryStream;
                     const aPicUrl: String;
                     const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
                     Const aPicBlurred: single;
                     const aPicCropCenter: TPointF;
                     const aCornerRadius: TPointF;
                     const aScreenScale: single): TALDrawable; static;
    class function PlaceIntoAndCropImage(
                     const aSender: Tobject;
                     const aPicStream: TCustomMemoryStream;
                     const aPicUrl: String;
                     const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
                     Const aPicBlurred: single;
                     const aPicCropCenter: TPointF;
                     const aCornerRadius: TPointF;
                     const aScreenScale: single): TALDrawable; static;
  {$ENDREGION}
  //
  {$REGION 'ProgressFrame'}
  //*private
  //*  FProgressFrame: TALBaseProgressFrame;
  //*  FProgressFrameStopAnimationEvent: Tevent;
  //*protected
  //*  property ProgressFrame: TALBaseProgressFrame read FProgressFrame;
  //*  property ProgressFrameStopAnimationEvent: Tevent read FProgressFrameStopAnimationEvent;
  //*  function CreateProgressFrame: TALBaseProgressFrame; virtual;
  //*  function CreateProgressFrameStopAnimationEvent: TEvent; virtual;
  //*  procedure StartProgressFrameAnimation; virtual;
  //*  procedure StopProgressFrameAnimation(const AImmediate: boolean); overload; virtual;
  //*  procedure StopProgressFrameAnimation; overload; virtual;
  //*  function CanRemoveProgressFrame: Boolean; virtual;
  //*  procedure RemoveProgressFrame; virtual;
  {$ENDREGION'}
  //
  {$REGION 'ScrollEngine'}
  private
    FActiveScrollEngines: TList<TALScrollEngine>;
    FHasActiveScrollEngines: Boolean;
  protected
    property ActiveScrollEngines: TList<TALScrollEngine> read FActiveScrollEngines;
    property HasActiveScrollEngines: Boolean read FHasActiveScrollEngines write FHasActiveScrollEngines;
  {$ENDREGION'}
  //
  {$REGION 'ScrollBar'}
  private
    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    FScrollBar: TALScrollBar;
    {$ENDIF}
  protected
    procedure ScrollBarChange(Sender: TObject); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
  public
    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    property ScrollBar: TALScrollBar read FScrollBar;
    {$ENDIF}
  {$ENDREGION'}
  //
  {$REGION 'Refresh'}
  //*private
  //*  fCanRefreshByScrolling: Boolean;
  //*  fRefreshByScrollingDisabled: Boolean;
  //*  function GetCanRefreshByScrolling: Boolean; inline;
  //*  property CanRefreshByScrolling: Boolean read GetCanRefreshByScrolling write fCanRefreshByScrolling;
  //*protected
  //*  procedure RefreshCreateRefreshedFrameOnTimer(Sender: TObject); override;
  //*  procedure RefreshCreateRefreshedFrame; override;
  //*  procedure RefreshCheckReadyToSwapOnTimer(Sender: TObject); override;
  //*  property RefreshByScrollingDisabled: Boolean read fRefreshByScrollingDisabled write fRefreshByScrollingDisabled;
  {$ENDREGION'}
  //
  {$REGION 'AniIndicator'}
  private
    fAniIndicatorTimer: Ttimer;
    fAniIndicatorEnabled: Boolean;
    fAniindicatorImgIndex: TSmallPoint;
  protected
    procedure AniIndicatorOnTimer(Sender: TObject);
  {$ENDREGION'}
  //
  {$REGION 'GetControlAtPos'}
  function GetControlAtPos(
             const aPos: TPointF; // APos is local to the frame
             out AControlPos: TPointF; // AControlPos is local to the founded control
             const ADirectChildOnly: Boolean = False;
             const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl; overload; virtual;
  function GetControlAtPos(
             const aPos: TPointF;
             const ADirectChildOnly: Boolean = False;
             const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl; overload; virtual; // APos is local to the control
  {$ENDREGION'}
  //
  {$REGION 'Hovered'}
  private
    FHovered: TALDynamicScrollBoxControl;
    procedure SetHovered(const AValue: TALDynamicScrollBoxControl);
  public
    property Hovered: TALDynamicScrollBoxControl read FHovered;
  {$ENDREGION'}
  //
  {$REGION 'LongPress'}
  private
    FLongPressTimer: TTimer;
  protected
    procedure LongPressTimerOnTimer(Sender: TObject);
  {$ENDREGION'}
  //
  {$REGION 'TouchEffect'}
  private
    FTouchEffectOwner: TALDynamicScrollBoxControl;
    FTouchEffectMousePos: TpointF; // local to the TouchEffectOwner
    FTouchEffectClicked: Boolean;
    FTouchEffectAnim: TALFloatAnimation;
    FTouchEffectRectangleBitmap: TALDrawable;
    FOwnTouchEffectRectangleBitmap: Boolean;
    FTouchEffectCircleBitmap: TALDrawable;
    FOwnTouchEffectCircleBitmap: Boolean;
    procedure TouchEffectAnimFinish(Sender: TObject);
    procedure SetTouchEffectRectangleBitmap(const AValue: TALDrawable);
    procedure SetTouchEffectCircleBitmap(const AValue: TALDrawable);
  protected
    property TouchEffectAnim: TALFloatAnimation read FTouchEffectAnim;
    property TouchEffectOwner: TALDynamicScrollBoxControl read FTouchEffectOwner write FTouchEffectOwner;
    property TouchEffectMousePos: TpointF read FTouchEffectMousePos write FTouchEffectMousePos; // local to the TouchEffectOwner
    property TouchEffectClicked: Boolean read FTouchEffectClicked write FTouchEffectClicked;
  public
    property TouchEffectRectangleBitmap: TALDrawable read FTouchEffectRectangleBitmap write SetTouchEffectRectangleBitmap;
    property OwnTouchEffectRectangleBitmap: Boolean read FOwnTouchEffectRectangleBitmap write FOwnTouchEffectRectangleBitmap;
    property TouchEffectCircleBitmap: TALDrawable read FTouchEffectCircleBitmap write SetTouchEffectCircleBitmap;
    property OwnTouchEffectCircleBitmap: Boolean read FOwnTouchEffectCircleBitmap write FOwnTouchEffectCircleBitmap;
  {$ENDREGION'}
  //
  {$REGION 'Selection'}
  private
    fSelectionTimer: Ttimer;
    fSelectedItems: TList<TALDynamicScrollBoxItem>;
    procedure SelectionTimerOnTimer(Sender: TObject);
  protected
    procedure DoDeleteSelection; virtual;
  public
    property SelectionTimer: Ttimer read fSelectionTimer;
    property SelectedItems: TList<TALDynamicScrollBoxItem> read fSelectedItems;
    procedure ToggleSelection(Const aItem: TALDynamicScrollBoxItem);
    procedure CancelSelection; virtual;
    procedure DeleteSelection;
  {$ENDREGION'}
  //
  {$REGION 'DblClick'}
  //usage of the DblClickTimer is simple
  //In the ONCLICK event :
  // * you check if DblClickTimer is enabled, if yes then it's mean you are in a DblClick
  //   and then you fire the DblClick event and you deactivate the DblClickTimer
  // * if DblClickTimer is not enabled, then it's mean you are in the first click
  //   so you enable DblClickTimer with an event assigned to it's ontimer that simply
  //   disable the DblClickTimer when it's fire (ex: DblClickOffOnTimer) or that
  //   fire the very first onclick because DblClick was not done
  private
    fDblClickTimer: Ttimer;
    function GetDblClickTimer: TTimer;
  public
    procedure DblClickOffOnTimer(Sender: TObject);
    property DblClickTimer: Ttimer read GetDblClickTimer;
  {$ENDREGION'}
  //
  {$REGION 'MouseEvents'}
  protected
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
  {$ENDREGION'}
  //
  {$REGION 'OnResize'}
  protected
    procedure DoFrameResized(const AToCounterbalanceKeyboardSpace: boolean); virtual;
    procedure FrameResized(Sender: TObject); virtual;
  {$ENDREGION'}
  //
  {$REGION 'OnPaint'}
  protected
    //we use Painting instead of DoPaint because
    //Painting is called BEFORE to paint childreen when
    //DoPaint is called AFTER we have painted childreen.
    //this is problematic if the form have childreen like
    //on conversation frame with botton edit panel
    procedure Painting; override;
    procedure DoPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single); virtual;
  {$ENDREGION'}
  //
  {$REGION 'FilterDialog'}
  public
    type
      TFilterDialogPopupBtn = Record
        Caption: String;
        ResName: String;
        ModalResult: TModalResult;
        constructor Create(
                      const ACaption: String;
                      const AResName: String;
                      const AModalResult: TModalResult);
      End;
  private
    fFilterDialogMainBtnVisible: Boolean;
    fFilterDialogPopupBtns: Tlist<TFilterDialogPopupBtn>;
    procedure FilterDialogPopupBtnLabelResized(Sender: TObject);
    procedure FilterDialogPopupBtnLabelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FilterDialogPopupBtnImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FilterDialogMainBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FilterDialogMainBtnClick(Sender: TObject);
  protected
    procedure FilterDialogClose(Const aPopup: Tcontrol; const AResult: TModalResult); virtual;
    function GetFilterDialogMainBtnResName: String; virtual;
    function CanShowFilterDialogResetBtn: Boolean; virtual;
    property FilterDialogMainBtnVisible: Boolean read fFilterDialogMainBtnVisible write fFilterDialogMainBtnVisible;
    property FilterDialogPopupBtns: Tlist<TFilterDialogPopupBtn> read fFilterDialogPopupBtns write fFilterDialogPopupBtns;
  {$ENDREGION'}
  //
  {$REGION 'Priority'}
  protected
    function GetPriority(const AExtData: Tobject): Int64; inline;
  {$ENDREGION}
  //
  {$REGION 'ScrollToBegin'}
  private
    fScrollToBeginAnim: TALFloatAnimation;
    procedure ScrollToBeginAnimProcess(Sender: TObject);
    procedure ScrollToBeginAnimFinish(Sender: TObject);
  public
    procedure ScrollToBegin;
  {$ENDREGION}
  //
  {$REGION 'ActionBar/NavigationBar'}
  //*public
  //*  procedure InitActionBar; override;
  {$ENDREGION'}
  //
  {$REGION 'GeoPosition'}
  private
    class var CanWaitGeoPosition: boolean;
  private
    FRefreshWhenGeoPosition: Boolean;
    FNeedGeoPosition: Boolean;
    FWaitGeoPosition: boolean;
    FWaitGeoPositionEvent: Tevent;
    FWaitGeoPositionTimer: TTimer;
    procedure SetWaitGeoPosition(const AValue: boolean);
    procedure OnGeoPositionUpdateMessage(const Sender: TObject; const M: TMessage);
    procedure OnGeoPositionAuthorizationStatusMessage(const Sender: TObject; const M: TMessage);
    procedure OnWaitGeoPositionTimerEvent(Sender: TObject);
  protected
    procedure ForceStartGeoPositionUpdates;
    property WaitGeoPosition: boolean read FWaitGeoPosition write SetWaitGeoPosition;
  {$ENDREGION}
  //
  {$REGION 'Notification'}
  //*protected
  //*  procedure HandleNotificationReceivedMessage(const Sender: TObject; const M: TMessage); override;
  {$ENDREGION'}
  //
  {$REGION 'Events'}
  public
    type
      TLoadDataEvent = procedure(Const AItem: TALDynamicScrollBoxItem; const AData: TALJSONNodeW; var AIsSuccessful: Boolean) of object;
      TLoadItemsEvent = procedure(Const AView: TALDynamicScrollBoxView; const AItems: TALJSONNodeW; var APaginationToken: String; var AIsSuccessful: Boolean) of object;
      TCreateViewEvent = function(Const AOwner: TALDynamicScrollBox): TALDynamicScrollBoxView of object;
      TCreateItemEvent = function(Const AOwner: TALDynamicScrollBoxView): TALDynamicScrollBoxItem of object;
      TCreateLayoutEvent = function(Const AOwner: TALDynamicScrollBoxItem): TALDynamicScrollBoxItemEphemeralLayout of object;
      TAlignItemsEvent = procedure(Const Aview: TALDynamicScrollBoxView; const AFromIdx, AToIdx: integer) of object;
  private
    FOnLoadData: TLoadDataEvent;
    FOnLoadItems: TLoadItemsEvent;
    FOnCreateView: TCreateViewEvent;
    FOnCreateItem: TCreateItemEvent;
    FOnCreateLayout: TCreateLayoutEvent;
    FOnAlignItems: TAlignItemsEvent;
  public
    property OnLoadData: TLoadDataEvent read FOnLoadData write FOnLoadData;
    property OnLoadItems: TLoadItemsEvent read FOnLoadItems write FOnLoadItems;
    property OnCreateView: TCreateViewEvent read FOnCreateView write FOnCreateView;
    property OnCreateItem: TCreateItemEvent read FOnCreateItem write FOnCreateItem;
    property OnCreateLayout: TCreateLayoutEvent read FOnCreateLayout write FOnCreateLayout;
    property OnAlignItems: TAlignItemsEvent read FOnAlignItems write FOnAlignItems;
  {$ENDREGION}
  //
  {$REGION 'Prepare'}
  protected
    FHasBeenPrepared: Boolean;
  public
    procedure Prepare; virtual;
  {$ENDREGION}
  //
  //*protected
  //*  function GetCanPreload: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  //*  procedure BeforeDestroy; override;
  //*  procedure AsyncDestroy; override;
  //*  procedure Initialize; override;
  //*  procedure OnHide; override;
  //*  procedure OnShow; override;
  end;

procedure Register;

implementation

uses
  System.Math,
  System.SysUtils,
  System.Diagnostics,
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
  {$IF defined(ALSkiaEngine)}
  System.Skia.API,
  FMX.Skia.Canvas,
  {$ENDIF}
  FMX.forms,
  FMX.Utils,
  FMX.Platform,
  Fmx.Surfaces,
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.Cipher,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.BreakText,
  Alcinoe.HTTP.Client.Net.Pool;

{*************}
//[MultiThread]
constructor TALDynamicScrollBoxControl.Create(const AOwner: TALDynamicScrollBoxControl);
begin
  inherited create;
  FBoundsRect := TalRectD.create(0,0,50,50);
  FPadding := TBounds.Create(TRectF.Empty);
  FPadding.OnChange := PaddingChangedHandler;
  FMargins := TBounds.Create(TRectF.Empty);
  FMargins.OnChange := MarginsChangedHandler;
  //FOnResized := nil;





  FCanvas := nil;
  FAlign := TALAlignLayout.None;
  FPressed := False;
  FIsMouseOver := False;
  FIsFocused := False;
  FAutoCapture := False;

  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if MarginsChanged is not implemented in FMX.Controls.TControl and adjust the IFDEF'}
  {$ENDIF}
  //**FFormerMarginsChangedHandler := Margins.OnChange;
  //**Margins.OnChange := MarginsChangedHandler;
  //**Size.SetPlatformDefaultWithoutNotification(False);
  //**FForm := nil;
  FFocusOnMouseDown := False;
  FFocusOnMouseUp := False;
  FControlAbsolutePosAtMouseDown := TpointF.zero;
  FMouseDownAtLowVelocity := True;
  // Double-clicks, or double-taps, are rarely used in mobile design due to
  // touch screen challenges and user experience considerations. Mobile devices
  // favor simpler, more intuitive gestures like swiping and pinching, which are
  // better suited to smaller screens and prevent confusion with similar
  // actions. Consequently, functionalities often tied to double-clicks on
  // desktops are handled by different gestures or interface controls in
  // mobile apps, leading to a more user-friendly experience.
  FDisableDoubleClickHandling := True;
  FIsPixelAlignmentEnabled := True;

  FOnMouseEnter := nil;
  FOnMouseLeave := nil;
  //*FOnMouseDown := nil;
  //*FOnMouseUp := nil;
  //*FOnClick := nil;
  //*FOnDblClick := nil;
  //*FOnMouseMove := nil;
  //*FOnLongPress := nil;
  //FIsScrollBoxView (false by default)
  //FIsScrollBoxItem (false by default)
  //FIsEphemeralLayout (false by default)
  SetLength(FControls, 0);
  FNeedArrangement := True;
  fIsAdjustingSize := False;
  FControlIndex := -1;
  Visible := True;
  //*Align := TALAlignLayout.None;
  AutoSize := False;
  //*Margins := TrectF.Empty;
  //*Padding := TrectF.Empty;
  Cursor := crDefault;
  FHitTest := False;
  FIsPressed := False;
  FLongPressDelay := 250;
  //*TouchEffectStyle := TTouchEffectStyle.None;
  //*StartTouchEffectOnMouseDown := False;
  //*TouchEffectBoundsConstraint := TTouchEffectBoundsConstraint.ExpandedLocalRect;
  //*TouchEffectZOrder := Maxint;
  //*TouchEffectRectangleFillColor := TALTouchEffectBuilder.DefRectangleFillColor;
  //*TouchEffectCircleFillColor := TALTouchEffectBuilder.DefCircleFillColor;
  TouchTargetExpansion := TRectF.Empty;
  Tag := 0;
  fisDestroying := False;
  FOwner := nil;
  FOwnerItem := nil;
  FOwnerView := nil;
  //FOwnerScrollBox (nil by default)
  SetOwner(AOwner);
end;

{********************************************}
destructor TALDynamicScrollBoxControl.Destroy;
begin
  ALFreeAndNil(FPadding);
  ALFreeAndNil(FMargins);



  fisDestroying := True;
  SetOwner(nil);
  for Var I := low(FControls) to High(FControls) do
    ALFreeAndNil(FControls[i]);
  inherited;
end;

{*********************************************************************}
procedure TALDynamicScrollBoxControl.SetPadding(const AValue: TBounds);
begin
  FPadding.Assign(AValue);
end;

{*********************************************************************}
procedure TALDynamicScrollBoxControl.SetMargins(const AValue: TBounds);
begin
  FMargins.Assign(AValue);
end;

{**************************************************************************}
procedure TALDynamicScrollBoxControl.PaddingChangedHandler(Sender: TObject);
begin
  PaddingChanged;
end;

{**************************************************}
procedure TALDynamicScrollBoxControl.PaddingChanged;
begin
  Realign;
end;

{**************************************************************************}
procedure TALDynamicScrollBoxControl.MarginsChangedHandler(Sender: TObject);
begin
  MarginsChanged;
end;

{**************************************************}
procedure TALDynamicScrollBoxControl.MarginsChanged;
begin
  If Parent <> nil then
    Parent.Realign;
end;

{*******************************************************************}
procedure TALDynamicScrollBoxControl.SetBoundsRect(AValue: TALRectD);
begin
  AValue.Width := System.Math.Max(0, AValue.Width);
  AValue.Height := System.Math.Max(0, AValue.Height);
  if not AValue.EqualsTo(FboundsRect, TEpsilon.Position) then begin
    FboundsRect := AValue;
    HandleSizeChanged;
  end;
end;

{***************************************************}
function TALDynamicScrollBoxControl.GetWidth: Single;
begin
  Result := Single(FBoundsRect.Width);
end;

{****************************************************}
function TALDynamicScrollBoxControl.GetHeight: Single;
begin
  Result := Single(FBoundsRect.Height);
end;

{******************************************************************}
procedure TALDynamicScrollBoxControl.SetWidth(const AValue: Single);
begin
  var LBoundsRect := FBoundsRect;
  LBoundsRect.Width := AValue;
  SetBoundsRect(LBoundsRect);
end;

{*******************************************************************}
procedure TALDynamicScrollBoxControl.SetHeight(const AValue: Single);
begin
  var LBoundsRect := FBoundsRect;
  LBoundsRect.Height := AValue;
  SetBoundsRect(LBoundsRect);
end;

{*****************************************************}
procedure TALDynamicScrollBoxControl.HandleSizeChanged;
begin
  if (Align <> TALAlignLayout.None) and (Parent <> nil) then
    Parent.Realign;
  if ControlsCount > 0 then
    Realign;
  Repaint;
  DoResized
end;

{*********************************************}
procedure TALDynamicScrollBoxControl.DoResized;
begin
  if Assigned(FOnResized) then
    FOnResized(Self);
end;

{*******************************************}
procedure TALDynamicScrollBoxControl.Realign;
begin
  if IsDestroying then
    Exit;
  //if FDisableAlign then
  //  Exit;
  if IsUpdating then
    Exit;
  DoRealign;
end;

{*********************************************}
procedure TALDynamicScrollBoxControl.DoRealign;
begin
  //if not FNeedAlign then
  //  Exit;
  AlignControls;
end;

{*************************************************}
Procedure TALDynamicScrollBoxControl.AlignControls;
begin

  {$REGION 'exit if arrangement is not needed'}
  if FIsDestroying then exit;
  if not FNeedArrangement then exit;
  {$ENDREGION}

  var LClientRect: TALRectD;
  if AutoSize then begin

    {$REGION 'Initialize client rect based on self''s bounds and padding'}
    LClientRect := TALRectD.Create(
                     Padding.Left, {Left}
                     Padding.Top, {Top}
                     FBoundsRect.Width - Padding.Right, {Right}
                     FBoundsRect.height - Padding.Bottom); {Bottom}
    {$ENDREGION}

    {$REGION 'First Pass: BoundsRect Adjustment for Side Anchored controls (do not call AlignControls)'}
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
              LClientRect.Top + LControl.Margins.Top + LControl.FBoundsRect.Height)); {Bottom}
          LClientRect.Top := LControl.FBoundsRect.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'TopCenter'}
        TALAlignLayout.TopCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.FBoundsRect.Height)); {Bottom}
          LClientRect.Top := LControl.FBoundsRect.Bottom + LControl.Margins.Bottom;
        end;
        {$ENDREGION}

        {$REGION 'Left'}
        TALAlignLayout.Left:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.FBoundsRect.Width, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Left := LControl.FBoundsRect.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'LeftCenter'}
        TALAlignLayout.LeftCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.FBoundsRect.Width, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
          LClientRect.Left := LControl.FBoundsRect.Right + LControl.Margins.Right;
        end;
        {$ENDREGION}

        {$REGION 'Right'}
        TALAlignLayout.Right:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.FBoundsRect.Width, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Right := LControl.FBoundsRect.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'RightCenter'}
        TALAlignLayout.RightCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.FBoundsRect.Width, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
          LClientRect.Right := LControl.FBoundsRect.Left - LControl.Margins.Left;
        end;
        {$ENDREGION}

        {$REGION 'Bottom'}
        TALAlignLayout.Bottom:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.FBoundsRect.Height, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.FBoundsRect.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'BottomCenter'}
        TALAlignLayout.BottomCenter:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.FBoundsRect.Height, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
          LClientRect.Bottom := LControl.FBoundsRect.Top - LControl.Margins.Top;
        end;
        {$ENDREGION}

        {$REGION 'None,Center,VertCenter,HorzCenter,Horizontal,Vertical,Client'}
        TALAlignLayout.None,
        TALAlignLayout.Center,
        TALAlignLayout.VertCenter,
        TALAlignLayout.HorzCenter,
        TALAlignLayout.Horizontal,
        TALAlignLayout.Vertical,
        TALAlignLayout.Client:;
        {$ENDREGION}

        {$REGION 'Unknown'}
        else
          raise Exception.Create('Error EEE242BF-F67C-4987-9A24-BC660C922CB8');
        {$ENDREGION}

      end;
    end;
    {$ENDREGION}

    {$REGION 'Second pass: BoundsRect Adjustment for Central controls (do not call AlignControls)'}
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
              (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'Horizontal'}
        TALAlignLayout.Horizontal:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LControl.FBoundsRect.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LControl.FBoundsRect.Bottom)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'Vertical'}
        TALAlignLayout.Vertical:
        begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LControl.FBoundsRect.left, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LControl.FBoundsRect.right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'VertCenter'}
        TALAlignLayout.VertCenter: begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'HorzCenter'}
        TALAlignLayout.HorzCenter: begin
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
        {$ENDREGION}

        {$REGION 'None,Top,Left,Right,Bottom,TopCenter,LeftCenter,RightCenter,BottomCenter'}
        TALAlignLayout.None,
        TALAlignLayout.Top,
        TALAlignLayout.Left,
        TALAlignLayout.Right,
        TALAlignLayout.Bottom,
        TALAlignLayout.TopCenter,
        TALAlignLayout.LeftCenter,
        TALAlignLayout.RightCenter,
        TALAlignLayout.BottomCenter:;
        {$ENDREGION}

        {$REGION 'Unknown'}
        else
          raise Exception.Create('Error 94A426D7-2652-476F-ABBA-039DDC419BC4');
        {$ENDREGION}

      end;
    end;
    {$ENDREGION}

    {$REGION 'Third pass: If autosize is enabled, attempt to adjust the dimensions of anchored controls to their minimal size (Call AlignControls)'}
    if autosize then begin
      for var I := low(FControls) to High(FControls) do begin
        var LControl := FControls[i];
        if not LControl.Visible then Continue;
        case LControl.Align of

          {$REGION 'LControl.Align: Top,Bottom,VertCenter,Horizontal: Force Adjust the width'}
          TALAlignLayout.Top,
          TALAlignLayout.Bottom,
          TALAlignLayout.VertCenter,
          TALAlignLayout.Horizontal: begin
            case Align of

              {$REGION 'SELF.Align: Top,Bottom,VertCenter,Horizontal'}
              TALAlignLayout.Top,
              TALAlignLayout.Bottom,
              TALAlignLayout.VertCenter,
              TALAlignLayout.Horizontal: begin
                If LControl.AutoSize then
                  LControl.AlignControls; // autosize only the height
              end;
              {$ENDREGION}

              {$REGION 'SELF.Align: None,Left,Right,Center,TopCenter,LeftCenter,RightCenter,BottomCenter,HorzCenter,Vertical'}
              TALAlignLayout.None,
              TALAlignLayout.Left,
              TALAlignLayout.Right,
              TALAlignLayout.Center,
              TALAlignLayout.TopCenter,
              TALAlignLayout.LeftCenter,
              TALAlignLayout.RightCenter,
              TALAlignLayout.BottomCenter,
              TALAlignLayout.HorzCenter,
              TALAlignLayout.Vertical: begin
                var LSaveAutoSize := LControl.AutoSize;
                var LSaveAlign := LControl.Align;
                try
                  LControl.AutoSize := True;
                  if LSaveAutoSize then LControl.Align := TALAlignLayout.none // autosize the width and the height
                  else LControl.Align := TALAlignLayout.left; // autosize only the width
                  LControl.AlignControls;
                finally
                  LControl.AutoSize := LSaveAutoSize;
                  LControl.Align := LSaveAlign;
                end;
              end;
              {$ENDREGION}

              {$REGION 'SELF.Align: Client'}
              TALAlignLayout.Client:;
              {$ENDREGION}

              {$REGION 'Unknown'}
              else
                raise Exception.Create('Error D6DF8505-3B0E-4B51-B174-DFDEBC263D65');
              {$ENDREGION}

            end;
          end;
          {$ENDREGION}

          {$REGION 'LControl.Align: Left,Right,HorzCenter,Vertical: Force Adjust the Height'}
          TALAlignLayout.Left,
          TALAlignLayout.Right,
          TALAlignLayout.HorzCenter,
          TALAlignLayout.Vertical: begin
            case Align of

              {$REGION 'SELF.Align: Left,Right,HorzCenter,Vertical'}
              TALAlignLayout.Left,
              TALAlignLayout.Right,
              TALAlignLayout.HorzCenter,
              TALAlignLayout.Vertical: begin
                If LControl.AutoSize then
                  LControl.AlignControls; // autosize only the width
              end;
              {$ENDREGION}

              {$REGION 'SELF.Align: None,Top,Bottom,Center,TopCenter,LeftCenter,RightCenter,BottomCenter,VertCenter,Horizontal'}
              TALAlignLayout.None,
              TALAlignLayout.Top,
              TALAlignLayout.Bottom,
              TALAlignLayout.Center,
              TALAlignLayout.TopCenter,
              TALAlignLayout.LeftCenter,
              TALAlignLayout.RightCenter,
              TALAlignLayout.BottomCenter,
              TALAlignLayout.VertCenter,
              TALAlignLayout.Horizontal: begin
                var LSaveAutoSize := LControl.AutoSize;
                var LSaveAlign := LControl.Align;
                try
                  LControl.AutoSize := True;
                  if LSaveAutoSize then LControl.Align := TALAlignLayout.none // autosize the width and the height
                  else LControl.Align := TALAlignLayout.top; // autosize only the height
                  LControl.AlignControls;
                finally
                  LControl.AutoSize := LSaveAutoSize;
                  LControl.Align := LSaveAlign;
                end;
              end;
              {$ENDREGION}

              {$REGION 'SELF.Align: Client'}
              TALAlignLayout.Client:;
              {$ENDREGION}

              {$REGION 'Unknown'}
              else
                raise Exception.Create('Error 149239EB-DEAC-44D9-86EF-A78241124EB4');
              {$ENDREGION}

            end;
          end;
          {$ENDREGION}

          {$REGION 'LControl.Align: Client: Force Adjust the Width and/or the Height'}
          TALAlignLayout.Client: begin
            case Align of

              {$REGION 'SELF.Align: Top,Bottom,VertCenter,Horizontal'}
              TALAlignLayout.Top,
              TALAlignLayout.Bottom,
              TALAlignLayout.VertCenter,
              TALAlignLayout.Horizontal: begin
                var LSaveAutoSize := LControl.AutoSize;
                var LSaveAlign := LControl.Align;
                try
                  LControl.AutoSize := True;
                  if LSaveAutoSize then LControl.Align := TALAlignLayout.none // autosize the width and the height
                  else LControl.Align := TALAlignLayout.top; // autosize only the height
                  LControl.AlignControls;
                finally
                  LControl.AutoSize := LSaveAutoSize;
                  LControl.Align := LSaveAlign;
                end;
              end;
              {$ENDREGION}

              {$REGION 'SELF.Align: Left,Right,HorzCenter,Vertical'}
              TALAlignLayout.Left,
              TALAlignLayout.Right,
              TALAlignLayout.HorzCenter,
              TALAlignLayout.Vertical: begin
                var LSaveAutoSize := LControl.AutoSize;
                var LSaveAlign := LControl.Align;
                try
                  LControl.AutoSize := True;
                  if LSaveAutoSize then LControl.Align := TALAlignLayout.none // autosize the width and the height
                  else LControl.Align := TALAlignLayout.left; // autosize only the width
                  LControl.AlignControls;
                finally
                  LControl.AutoSize := LSaveAutoSize;
                  LControl.Align := LSaveAlign;
                end;
              end;
              {$ENDREGION}

              {$REGION 'SELF.Align: None,Center,TopCenter,LeftCenter,RightCenter,BottomCenter'}
              TALAlignLayout.None,
              TALAlignLayout.Center,
              TALAlignLayout.TopCenter,
              TALAlignLayout.LeftCenter,
              TALAlignLayout.RightCenter,
              TALAlignLayout.BottomCenter: begin
                var LSaveAutoSize := LControl.AutoSize;
                var LSaveAlign := LControl.Align;
                try
                  LControl.AutoSize := True;
                  LControl.Align := TALAlignLayout.none; // autosize the width and the height
                  LControl.AlignControls;
                finally
                  LControl.AutoSize := LSaveAutoSize;
                  LControl.Align := LSaveAlign;
                end;
              end;
              {$ENDREGION}

              {$REGION 'SELF.Align: Client'}
              TALAlignLayout.Client:;
              {$ENDREGION}

              {$REGION 'Unknown'}
              else
                raise Exception.Create('Error B7AFF786-AFC7-4C60-9518-CBFCCA393FEF');
              {$ENDREGION}

            end;
          end;
          {$ENDREGION}

          {$REGION 'LControl.Align: TopCenter,LeftCenter,RightCenter,BottomCenter: Force Adjust nothing'}
          TALAlignLayout.Center,
          TALAlignLayout.TopCenter,
          TALAlignLayout.LeftCenter,
          TALAlignLayout.RightCenter,
          TALAlignLayout.BottomCenter: begin
            If LControl.AutoSize then
              LControl.AlignControls;
          end;
          {$ENDREGION}

          {$REGION 'LControl.Align: None: Adjust nothing'}
          TALAlignLayout.None: ;
          {$ENDREGION}

          {$REGION 'Unknown'}
          else
            raise Exception.Create('Error 1C45A2D3-2E4A-40F4-80B4-442DCCF9A078');
          {$ENDREGION}

        end;
      end;

    end;
    {$ENDREGION'}

    {$REGION 'Adjust the size'}
    AdjustSize;
    {$ENDREGION'}

  end;

  {$REGION 'Initialize client rect based on self''s bounds and padding'}
  LClientRect := TALRectD.Create(
                   Padding.Left, {Left}
                   Padding.Top, {Top}
                   FBoundsRect.Width - Padding.Right, {Right}
                   FBoundsRect.height - Padding.Bottom); {Bottom}
  {$ENDREGION}

  {$REGION 'Fourth Pass: BoundsRect Adjustment for Side Anchored controls (call AlignControls)'}
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
            LClientRect.Top + LControl.Margins.Top + LControl.FBoundsRect.Height)); {Bottom}
        if LControl.AutoSize then
          LControl.AlignControls;
        LClientRect.Top := LControl.FBoundsRect.Bottom + LControl.Margins.Bottom;
      end;
      {$ENDREGION}

      {$REGION 'TopCenter'}
      TALAlignLayout.TopCenter:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
            LClientRect.Top + LControl.Margins.Top, {Top}
            (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
            LClientRect.Top + LControl.Margins.Top + LControl.FBoundsRect.Height)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
              LClientRect.Top + LControl.Margins.Top + LControl.FBoundsRect.Height)); {Bottom}
        end;
        LClientRect.Top := LControl.FBoundsRect.Bottom + LControl.Margins.Bottom;
      end;
      {$ENDREGION}

      {$REGION 'Left'}
      TALAlignLayout.Left:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            LClientRect.Left + LControl.Margins.Left, {Left}
            LClientRect.Top + LControl.Margins.Top, {Top}
            LClientRect.Left + LControl.Margins.Left + LControl.FBoundsRect.Width, {Right}
            LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        if LControl.AutoSize then
          LControl.AlignControls;
        LClientRect.Left := LControl.FBoundsRect.Right + LControl.Margins.Right;
      end;
      {$ENDREGION}

      {$REGION 'LeftCenter'}
      TALAlignLayout.LeftCenter:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            LClientRect.Left + LControl.Margins.Left, {Left}
            (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
            LClientRect.Left + LControl.Margins.Left + LControl.FBoundsRect.Width, {Right}
            (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
              LClientRect.Left + LControl.Margins.Left + LControl.FBoundsRect.Width, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        end;
        LClientRect.Left := LControl.FBoundsRect.Right + LControl.Margins.Right;
      end;
      {$ENDREGION}

      {$REGION 'Right'}
      TALAlignLayout.Right:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            LClientRect.Right - LControl.Margins.Right - LControl.FBoundsRect.Width, {Left}
            LClientRect.Top + LControl.Margins.Top, {Top}
            LClientRect.Right - LControl.Margins.Right, {Right}
            LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.FBoundsRect.Width, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
        LClientRect.Right := LControl.FBoundsRect.Left - LControl.Margins.Left;
      end;
      {$ENDREGION}

      {$REGION 'RightCenter'}
      TALAlignLayout.RightCenter:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            LClientRect.Right - LControl.Margins.Right - LControl.FBoundsRect.Width, {Left}
            (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
            LClientRect.Right - LControl.Margins.Right, {Right}
            (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Right - LControl.Margins.Right - LControl.FBoundsRect.Width, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        end;
        LClientRect.Right := LControl.FBoundsRect.Left - LControl.Margins.Left;
      end;
      {$ENDREGION}

      {$REGION 'Bottom'}
      TALAlignLayout.Bottom:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            LClientRect.Left + LControl.Margins.Left, {Left}
            LClientRect.Bottom - LControl.Margins.Bottom - LControl.FBoundsRect.Height, {Top}
            LClientRect.Right - LControl.Margins.Right, {Right}
            LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.FBoundsRect.Height, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
        LClientRect.Bottom := LControl.FBoundsRect.Top - LControl.Margins.Top;
      end;
      {$ENDREGION}

      {$REGION 'BottomCenter'}
      TALAlignLayout.BottomCenter:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
            LClientRect.Bottom - LControl.Margins.Bottom - LControl.FBoundsRect.Height, {Top}
            (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
            LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
              LClientRect.Bottom - LControl.Margins.Bottom - LControl.FBoundsRect.Height, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
        LClientRect.Bottom := LControl.FBoundsRect.Top - LControl.Margins.Top;
      end;
      {$ENDREGION}

      {$REGION 'None,Center,VertCenter,HorzCenter,Horizontal,Vertical,Client'}
      TALAlignLayout.None,
      TALAlignLayout.Center,
      TALAlignLayout.VertCenter,
      TALAlignLayout.HorzCenter,
      TALAlignLayout.Horizontal,
      TALAlignLayout.Vertical,
      TALAlignLayout.Client:;
      {$ENDREGION}

      {$REGION 'Unknown'}
      else
        raise Exception.Create('Error EEE242BF-F67C-4987-9A24-BC660C922CB8');
      {$ENDREGION}

    end;
  end;
  {$ENDREGION}

  {$REGION 'Fifth pass: BoundsRect Adjustment for Central controls (call AlignControls)'}
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
            (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
            (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
            (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
            (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        end;
      end;
      {$ENDREGION}

      {$REGION 'Horizontal'}
      TALAlignLayout.Horizontal:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            LClientRect.Left + LControl.Margins.Left, {Left}
            LControl.FBoundsRect.Top, {Top}
            LClientRect.Right - LControl.Margins.Right, {Right}
            LControl.FBoundsRect.Bottom)); {Bottom}
      end;
      {$ENDREGION}

      {$REGION 'Vertical'}
      TALAlignLayout.Vertical:
      begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            LControl.FBoundsRect.left, {Left}
            LClientRect.Top + LControl.Margins.Top, {Top}
            LControl.FBoundsRect.right, {Right}
            LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
      end;
      {$ENDREGION}

      {$REGION 'VertCenter'}
      TALAlignLayout.VertCenter: begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            LClientRect.Left + LControl.Margins.Left, {Left}
            (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
            LClientRect.Right - LControl.Margins.Right, {Right}
            (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              LClientRect.Left + LControl.Margins.Left, {Left}
              (LClientRect.Top + LClientRect.Bottom - LControl.FBoundsRect.Height) / 2, {Top}
              LClientRect.Right - LControl.Margins.Right, {Right}
              (LClientRect.Top + LClientRect.Bottom + LControl.FBoundsRect.Height) / 2)); {Bottom}
        end;
      end;
      {$ENDREGION}

      {$REGION 'HorzCenter'}
      TALAlignLayout.HorzCenter: begin
        LControl.SetBoundsRect(
          TALRectD.Create(
            (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
            LClientRect.Top + LControl.Margins.Top, {Top}
            (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
            LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        if LControl.AutoSize then begin
          LControl.AlignControls;
          LControl.SetBoundsRect(
            TALRectD.Create(
              (LClientRect.Left + LClientRect.Right - LControl.FBoundsRect.Width) / 2, {Left}
              LClientRect.Top + LControl.Margins.Top, {Top}
              (LClientRect.Left + LClientRect.Right + LControl.FBoundsRect.Width) / 2, {Right}
              LClientRect.Bottom - LControl.Margins.Bottom)); {Bottom}
        end;
      end;
      {$ENDREGION}

      {$REGION 'None,Top,Left,Right,Bottom,TopCenter,LeftCenter,RightCenter,BottomCenter'}
      TALAlignLayout.None,
      TALAlignLayout.Top,
      TALAlignLayout.Left,
      TALAlignLayout.Right,
      TALAlignLayout.Bottom,
      TALAlignLayout.TopCenter,
      TALAlignLayout.LeftCenter,
      TALAlignLayout.RightCenter,
      TALAlignLayout.BottomCenter:;
      {$ENDREGION}

      {$REGION 'Unknown'}
      else
        raise Exception.Create('Error 94A426D7-2652-476F-ABBA-039DDC419BC4');
      {$ENDREGION}

    end;
  end;
  {$ENDREGION}

  {$REGION 'Sixth pass: call AlignControls of each controls'}
  for var I := low(FControls) to High(FControls) do
    FControls[i].AlignControls;
  {$ENDREGION}

  {$REGION 'Clear FNeedArrangement'}
  FNeedArrangement := False;
  {$ENDREGION}

  {$REGION 'Make BufBitmap'}
  if (Owner = nil) or (Not Owner.FNeedArrangement) then
    MakeBufBitmap
  {$ENDREGION}

end;











{*******************************************************}
function TALDynamicScrollBoxControl.GetLocalRect: TRectF;
begin
  Result := TRectF.Create(0, 0, Width, Height);
end;

{*****************************************}
procedure TALDynamicScrollBoxControl.Paint;
begin

end;

{*************************************************************}
function TALDynamicScrollBoxControl.GetAbsoluteOpacity: Single;
begin
  Result := 1;
end;

{**********************************************************************************}
function TALDynamicScrollBoxControl.AbsoluteToLocal(const APoint: TPointF): TPointF;
begin

end;

{*******************************************************************************}
function TALDynamicScrollBoxControl.AbsoluteToLocal(const ARect: TRectF): TRectF;
begin

end;

{**********************************************************************************}
function TALDynamicScrollBoxControl.LocalToAbsolute(const APoint: TPointF): TPointF;
begin

end;

{*******************************************************************************}
function TALDynamicScrollBoxControl.LocalToAbsolute(const ARect: TRectF): TRectF;
begin

end;

{*******************************************}
procedure TALDynamicScrollBoxControl.Repaint;
begin

end;

{********************************************}
procedure TALDynamicScrollBoxControl.SetFocus;
begin

end;

{***********************************************}
procedure TALDynamicScrollBoxControl.BeginUpdate;
begin

end;

{******************************************************}
function TALDynamicScrollBoxControl.IsUpdating: Boolean;
begin

end;

{*********************************************}
procedure TALDynamicScrollBoxControl.EndUpdate;
begin

end;

{**********************************************}
//**procedure TALDynamicScrollBoxControl.Loaded;
//**begin
//**  {$IF not DEFINED(ALDPK)}
//**  if IsPixelAlignmentEnabled then
//**    AlignToPixel;
//**  {$ENDIF}
//**  Inherited;
//**end;

{**************************************************************}
//**function TALDynamicScrollBoxControl.IsOwnerLoading: Boolean;
//**begin
//**  result := (Owner <> nil) and
//**            (csloading in Owner.ComponentState);
//**end;

{************************************************************}
//**function TALDynamicScrollBoxControl.IsSizeStored: Boolean;
//**begin
//**  var LDefaultSize := GetDefaultSize;
//**  result := (not SameValue(LDefaultSize.cx, Size.Size.cx, TEpsilon.Position)) or
//**            (not SameValue(LDefaultSize.cy, Size.Size.cy, TEpsilon.Position));
//**end;

{************************************************}
procedure TALDynamicScrollBoxControl.AlignToPixel;
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
        TALAlignLayout.Center: begin
          Width := ALAlignDimensionToPixelRound(Width , ALGetScreenScale, TEpsilon.Position);
          Height := ALAlignDimensionToPixelRound(Height , ALGetScreenScale, TEpsilon.Position);
        end;
        //--
        TALAlignLayout.Top,
        TALAlignLayout.MostTop,
        TALAlignLayout.Bottom,
        TALAlignLayout.MostBottom,
        TALAlignLayout.Horizontal,
        TALAlignLayout.VertCenter:
          Height := ALAlignDimensionToPixelRound(Height , ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Left,
        TALAlignLayout.MostLeft,
        TALAlignLayout.Right,
        TALAlignLayout.MostRight,
        TALAlignLayout.Vertical,
        TALAlignLayout.HorzCenter:
          Width := ALAlignDimensionToPixelRound(Width , ALGetScreenScale, TEpsilon.Position);
        //--
        TALAlignLayout.Client,
        TALAlignLayout.Contents,
        TALAlignLayout.Scale,
        TALAlignLayout.Fit,
        TALAlignLayout.FitLeft,
        TALAlignLayout.FitRight:;
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

{**********************************************************************}
//**procedure TALDynamicScrollBoxControl.DelayOnResize(Sender: TObject);
//**begin
//**  Include(TALDynamicScrollBoxControlAccessPrivate(Self).FDelayedEvents, TALDynamicScrollBoxControlAccessPrivate.TDelayedEvent.Resize);
//**end;

{***********************************************************************}
//**procedure TALDynamicScrollBoxControl.DelayOnResized(Sender: TObject);
//**begin
//**  Include(TALDynamicScrollBoxControlAccessPrivate(Self).FDelayedEvents, TALDynamicScrollBoxControlAccessPrivate.TDelayedEvent.Resized);
//**end;

{**********************************************************************}
function TALDynamicScrollBoxControl.GetIsPixelAlignmentEnabled: Boolean;
begin
  Result := FIsPixelAlignmentEnabled;
end;

{*************************************************************************************}
procedure TALDynamicScrollBoxControl.SetIsPixelAlignmentEnabled(const AValue: Boolean);
begin
  FIsPixelAlignmentEnabled := AValue;
end;

{*************************************************************************}
//**function TALDynamicScrollBoxControl.IsVisibleWithinFormBounds: Boolean;
//**begin
//**  Result := GetParentedVisible;
//**  if not result then exit;
//**  if FForm <> nil then
//**    Result := FForm.ClientRect.IntersectsWith(LocalToAbsolute(LocalRect));
//**end;

{*******************************************************************}
//**procedure TALDynamicScrollBoxControl.SetNewScene(AScene: IScene);
//**begin
//**  {$IFNDEF ALCompilerVersionSupported122}
//**    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
//**    {$MESSAGE WARN 'Check if FMX.Controls.TControl.Pressed is still changed only in SetNewScene/DoMouseLeave/MouseDown/MouseUp/MouseClick'}
//**    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsFocused is still changed only in SetNewScene/DoEnter/DoExit'}
//**    {$MESSAGE WARN 'Check if FMX.Controls.TControl.IsMouseOver is still changed only in SetNewScene/DoMouseEnter/DoMouseLeave'}
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

{******************************************************}
function TALDynamicScrollBoxControl.GetPressed: Boolean;
begin
  result := FPressed;
end;

{*********************************************************************}
procedure TALDynamicScrollBoxControl.SetPressed(const AValue: Boolean);
begin
  if AValue <> GetPressed then begin
    FPressed := AValue;
    pressedChanged;
  end;
end;

{*******************************************}
procedure TALDynamicScrollBoxControl.DoEnter;
begin
  var LPrevIsFocused := IsFocused;
  inherited;
  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
end;

{******************************************}
procedure TALDynamicScrollBoxControl.DoExit;
begin
  var LPrevIsFocused := IsFocused;
  inherited;
  if LPrevIsFocused <> IsFocused then IsFocusedChanged;
end;

{************************************************}
procedure TALDynamicScrollBoxControl.DoMouseEnter;
begin
  var LPrevIsMouseOver := IsMouseOver;
  inherited;
  {$IF defined(ANDROID) or defined(IOS)}
  FIsMouseOver := False;
  {$ENDIF}
  if LPrevIsMouseOver <> IsMouseOver then IsMouseOverChanged;
end;

{************************************************}
procedure TALDynamicScrollBoxControl.DoMouseLeave;
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

{*****************************************************************************************************}
procedure TALDynamicScrollBoxControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  //--
  FControlAbsolutePosAtMouseDown := LocalToAbsolute(TPointF.Zero);
  FMouseDownAtLowVelocity := True;
  //--
  if FDisableDoubleClickHandling then Shift := Shift - [ssDouble];
  //--
  //**var LScrollableControl: IALScrollableControl;
  //**var LParent := Parent;
  //**while LParent <> nil do begin
  //**  if Supports(LParent, IALScrollableControl, LScrollableControl) then begin
  //**    if not LScrollableControl.GetScrollEngine.IsVelocityLow then begin
  //**      FMouseDownAtLowVelocity := False;
  //**      Break;
  //**    end
  //**    else LParent := LParent.Parent;
  //**  end
  //**  else LParent := LParent.Parent;
  //**end;
  //--
  if (not FFocusOnMouseDown) or (FFocusOnMouseUp) or (not FMouseDownAtLowVelocity) then begin
    Var LOldIsfocused := FIsfocused;
    FIsfocused := True;
    Try
      inherited;
    finally
      FIsfocused := LOldIsfocused;
    End;
  end
  else
    inherited;
  //--
  if LPrevPressed <> Pressed then PressedChanged;
end;

{***************************************************************************************************}
procedure TALDynamicScrollBoxControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFNDEF ALCompilerVersionSupported122}
    {$MESSAGE WARN 'Check if https://embt.atlassian.net/servicedesk/customer/portal/1/RSS-1323 have been implemented and adjust the IFDEF'}
  {$ENDIF}
  var LPrevPressed := Pressed;
  inherited;
  if LPrevPressed <> Pressed then PressedChanged;
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (FFocusOnMouseUp) and
     (FMouseDownAtLowVelocity) and
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) <= TALScrollEngine.DefaultTouchSlop) and
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) <= TALScrollEngine.DefaultTouchSlop) and
     //**(not (csDesigning in ComponentState)) and
     (not FIsFocused) then
    SetFocus;
end;

{******************************************************************************************************}
procedure TALDynamicScrollBoxControl.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  var LControlAbsolutePos := LocalToAbsolute(TPointF.Zero);
  if (not FMouseDownAtLowVelocity) or
     (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) or
     (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then begin
    {$IF defined(debug)}
    if (not FMouseDownAtLowVelocity) then
      ALLog('MouseClick', 'Skipped | Mouse Down was not made at Low Velocity')
    else if (abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x) > TALScrollEngine.DefaultTouchSlop) then
      ALLog('MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.x - LControlAbsolutePos.x), ALDefaultFormatSettingsW) + ' horizontally')
    else if (abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y) > TALScrollEngine.DefaultTouchSlop) then
      ALLog('MouseClick', 'Skipped | Control moved by '+ALFormatFloatW('0.##', abs(FControlAbsolutePosAtMouseDown.y - LControlAbsolutePos.y), ALDefaultFormatSettingsW) + ' vertically')
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
// exclusively with ParentControl.
//**function TALDynamicScrollBoxControl.GetParentedVisible: Boolean;
//**begin
//**  {$IF defined(ALDPK)}
//**  result := Inherited GetParentedVisible;
//**  {$ELSE}
//**  var P: TControl := Self;
//**  Result := False;
//**  while P <> nil do begin
//**    if not p.Visible then Exit;
//**    P := P.ParentControl;
//**  end;
//**  // We do not care about FForm.visible like in
//**  // Inherited GetParentedVisible
//**  // Result := (FForm = nil) or (FForm.visible);
//**  Result := True;
//**  {$ENDIF}
//**end;

{*****************************************************}
//**procedure TALDynamicScrollBoxControl.DoRootChanged;
//**begin
//**  inherited;
//**  if Root is TCommonCustomForm then FForm := TCommonCustomForm(Root)
//**  else FForm := Nil;
//**end;

{******************************************************}
procedure TALDynamicScrollBoxControl.IsMouseOverChanged;
begin
  // virtual
end;

{****************************************************}
procedure TALDynamicScrollBoxControl.IsFocusedChanged;
begin
  // virtual
end;

{**************************************************}
procedure TALDynamicScrollBoxControl.PressedChanged;
begin
  // virtual
end;
















//////////////////////////////////////////////
/// THE CODE BELOW WAS AUTO-GENERATED FROM ///
/// <ALCINOE>\Tools\CodeBuilder.           ///
//////////////////////////////////////////////

{$REGION 'AUTO-GENERATED'}

{************************************************************************************}
constructor TALDynamicScrollBoxShape.Create(const AOwner: TALDynamicScrollBoxControl);
begin
  inherited;
  FFill := TALBrush.Create($FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TALStrokeBrush.Create($FF000000);
  FStroke.OnChanged := StrokeChanged;
  fShadow := TALShadow.Create;
  fShadow.OnChanged := ShadowChanged;
end;

{******************************************}
destructor TALDynamicScrollBoxShape.Destroy;
begin
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FFill);
  ALFreeAndNil(fShadow);
  inherited;
end;

{**********************************************}
procedure TALDynamicScrollBoxShape.AlignToPixel;
begin
  beginUpdate;
  try
    inherited;
    Stroke.AlignToPixel;
    Fill.AlignToPixel;
    Shadow.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{**************************************************************}
procedure TALDynamicScrollBoxShape.FillChanged(Sender: TObject);
begin
  Repaint;
end;

{****************************************************************}
procedure TALDynamicScrollBoxShape.StrokeChanged(Sender: TObject);
begin
  Repaint;
end;

{****************************************************************}
procedure TALDynamicScrollBoxShape.ShadowChanged(Sender: TObject);
begin
  Repaint;
end;

{**************************************************}
function TALDynamicScrollBoxShape.GetFill: TALBrush;
begin
  Result := FFill;
end;

{****************************************************************}
procedure TALDynamicScrollBoxShape.SetFill(const Value: TALBrush);
begin
  FFill.Assign(Value);
end;

{**********************************************************}
function TALDynamicScrollBoxShape.GetStroke: TALStrokeBrush;
begin
  Result := FStroke;
end;

{************************************************************************}
procedure TALDynamicScrollBoxShape.SetStroke(const Value: TALStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{*****************************************************}
function TALDynamicScrollBoxShape.GetShadow: TALShadow;
begin
  Result := FShadow;
end;

{*******************************************************************}
procedure TALDynamicScrollBoxShape.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
end;

{********************************************************************************************}
constructor TALDynamicScrollBoxBaseRectangle.Create(const AOwner: TALDynamicScrollBoxControl);
begin
  inherited;
  fDoubleBuffered := true;
  FDefaultXRadius := 0;
  FDefaultYRadius := 0;
  FXRadius := FDefaultXRadius;
  FYRadius := FDefaultYRadius;
  FCorners := AllCorners;
  FSides := AllSides;
  fBufDrawable := ALNullDrawable;
end;

{**************************************************}
destructor TALDynamicScrollBoxBaseRectangle.Destroy;
begin
  ClearBufDrawable;
  inherited;
end;

{***************************************************}
procedure TALDynamicScrollBoxBaseRectangle.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{**********************************************************}
procedure TALDynamicScrollBoxBaseRectangle.clearBufDrawable;
begin
  {$IFDEF debug}
  if //** (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared'(* | Name: ' + Name*), TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{*****************************************************************}
function TALDynamicScrollBoxBaseRectangle.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

{***************************************************************}
function TALDynamicScrollBoxBaseRectangle.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

{*****************************************************************}
function TALDynamicScrollBoxBaseRectangle.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, FDefaultXRadius, TEpsilon.Vector);
end;

{*****************************************************************}
function TALDynamicScrollBoxBaseRectangle.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, FDefaultYRadius, TEpsilon.Vector);
end;

{***************************************************************}
function TALDynamicScrollBoxBaseRectangle.HasCustomDraw: Boolean;
begin
  Result := False;
end;

{*******************************************************************}
function TALDynamicScrollBoxBaseRectangle.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{**********************************************************************************}
procedure TALDynamicScrollBoxBaseRectangle.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then clearBufDrawable;
  end;
end;

{*************************************************************************}
procedure TALDynamicScrollBoxBaseRectangle.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  (**if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else **)NewValue := Value;
  if not SameValue(FXRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FXRadius := NewValue;
    Repaint;
  end;
end;

{*************************************************************************}
procedure TALDynamicScrollBoxBaseRectangle.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  (**if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else **)NewValue := Value;
  if not SameValue(FYRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FYRadius := NewValue;
    Repaint;
  end;
end;

{***************************************************************************}
procedure TALDynamicScrollBoxBaseRectangle.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    clearBufDrawable;
    FCorners := Value;
    Repaint;
  end;
end;

{***********************************************************************}
procedure TALDynamicScrollBoxBaseRectangle.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    clearBufDrawable;
    FSides := Value;
    Repaint;
  end;
end;

{**********************************************************************}
procedure TALDynamicScrollBoxBaseRectangle.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{************************************************************************}
procedure TALDynamicScrollBoxBaseRectangle.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{************************************************************************}
procedure TALDynamicScrollBoxBaseRectangle.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{***********************************************************}
Procedure TALDynamicScrollBoxBaseRectangle.CreateBufDrawable(
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

  ABufDrawableRect := LocalRect;
  var LSurfaceRect := ALGetShapeSurfaceRect(
                        ABufDrawableRect, // const ARect: TRectF;
                        AFill, // const AFill: TALBrush;
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

      ALDrawRectangle(
        LCanvas, // const ACanvas: TALCanvas;
        AScale, // const AScale: Single;
        IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
        ABufDrawableRect, // const Rect: TrectF;
        1, // const AOpacity: Single;
        AFill, // const Fill: TALBrush;
        AStateLayer, // const StateLayer: TALStateLayer;
        AStateLayerContentColor, // const AStateLayerContentColor: TAlphaColor;
        ADrawStateLayerOnTop, // const ADrawStateLayerOnTop: Boolean;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow, // const Shadow: TALShadow
        Sides, // const Sides: TSides;
        Corners, // const Corners: TCorners;
        XRadius, // const XRadius: Single = 0;
        YRadius); // const YRadius: Single = 0);

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

{*********************************************************}
procedure TALDynamicScrollBoxBaseRectangle.MakeBufDrawable;
begin

  if //--- Do not create BufDrawable if not DoubleBuffered
     {$IF not DEFINED(ALDPK)}(not DoubleBuffered) or{$ENDIF}
     //--- Do not create BufDrawable if the size is 0
     (BoundsRect.IsEmpty) or
     //--- Do not create BufDrawable if only fill with solid color
     ((not HasCustomDraw)
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
       (Fill.Styles = [TALBrushStyle.solid])))
  then begin
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(FBufDrawable)) then exit;

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

{***********************************************}
procedure TALDynamicScrollBoxBaseRectangle.Paint;
begin

  MakeBufDrawable;

  if ALIsDrawableNull(fBufDrawable) then begin
    {$IF DEFINED(ALSkiaCanvas)}
    ALDrawRectangle(
      TSkCanvasCustom(Canvas).Canvas.Handle, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      IsPixelAlignmentEnabled, // const AAlignToPixel: Boolean;
      LocalRect, // const Rect: TrectF;
      AbsoluteOpacity, // const AOpacity: Single;
      Fill, // const Fill: TALBrush;
      nil, // const StateLayer: TALStateLayer;
      TAlphaColors.Null, // const AStateLayerContentColor: TAlphaColor;
      True, // const ADrawStateLayerOnTop: Boolean;
      Stroke, // const Stroke: TALStrokeBrush;
      Shadow, // const Shadow: TALShadow
      Sides, // const Sides: TSides;
      Corners, // const Corners: TCorners;
      XRadius, // const XRadius: Single = 0;
      YRadius); // const YRadius: Single = 0);
    {$ELSE}
    {$IF defined(DEBUG)}
    if not doublebuffered then begin
      ALLog('TALDynamicScrollBoxBaseRectangle.Paint', 'Controls that are not double-buffered only work when SKIA is enabled', TALLogType.ERROR);
      exit;
    end;
    {$ENDIF}
    If Fill.Styles = [TALBrushStyle.Solid] then begin
      Canvas.Fill.kind := TBrushKind.solid;
      Canvas.Fill.color := Fill.color;
      Canvas.FillRect(ALAlignToPixelRound(LocalRect, Canvas.Matrix, Canvas.Scale, TEpsilon.position), XRadius, YRadius, FCorners, AbsoluteOpacity, TCornerType.Round);
    end
    else if Fill.HasFill then raise Exception.Create('Error 87B5E7C8-55AF-41C7-88D4-B840C7D0F78F');
    {$ENDIF}
    exit;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{****************************************************************************************}
constructor TALDynamicScrollBoxRectangle.Create(const AOwner: TALDynamicScrollBoxControl);
begin
  inherited Create(AOwner);
  FAutoSize := False;
end;

{*********************************************************}
function TALDynamicScrollBoxRectangle.GetAutoSize: Boolean;
begin
  result := FAutoSize;
end;

{***********************************************************************}
function TALDynamicScrollBoxRectangle.HasUnconstrainedAutosizeX: Boolean;
begin
  result := GetAutoSize;
end;

{***********************************************************************}
function TALDynamicScrollBoxRectangle.HasUnconstrainedAutosizeY: Boolean;
begin
  result := GetAutoSize;
end;

{***********************************************************************}
procedure TALDynamicScrollBoxRectangle.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
    repaint;
  end;
end;

{***********************************************}
procedure TALDynamicScrollBoxRectangle.DoRealign;
begin
  inherited DoRealign;
  AdjustSize;
end;

{************************************************}
procedure TALDynamicScrollBoxRectangle.AdjustSize;
begin
//**  if (not (csLoading in ComponentState)) and // loaded will call again AdjustSize
//**     (not (csDestroying in ComponentState)) and // if csDestroying do not do autosize
//**     (FAutoSize) then // if FAutoSize is false nothing to adjust
//**    ALAutoSize(Self);
end;

{$ENDREGION}

//////////////////////////////////////////////
/// THE CODE ABOVE WAS AUTO-GENERATED FROM ///
/// <ALCINOE>\Tools\CodeBuilder.           ///
//////////////////////////////////////////////


{*************}
//[MultiThread]
constructor TALDynamicScrollBoxDownloadMaterialExtData.Create(
              const AComparekey: Integer;
              const AMaterialIdx: Integer;
              const AContentUrl: String;
              const ADestSize: TSizeF;
              const AContentBlurred: single;
              const AContentCropCenter: TpointF;
              const aCornerRadius: TPointF;
              const ADrawMaterialFunc: TDrawMaterialFunc);
begin
  inherited create;
  FComparekey := AComparekey;
  FMaterialIdx := AMaterialIdx;
  FContentStream := nil;
  fContentUrl := AContentUrl;
  fDestSize :=  ADestSize;
  fContentBlurred := AContentBlurred;
  fContentCropCenter := AContentCropCenter;
  fCornerRadius := aCornerRadius;
  fDrawMaterialFunc := ADrawMaterialFunc;
End;

{*************}
//[MultiThread]
constructor TALDynamicScrollBoxDownloadMaterialExtData.Create(
              const AComparekey: Integer;
              const AMaterialIdx: Integer;
              const AContentStream: TCustomMemoryStream;
              const ADestSize: TSizeF;
              const AContentBlurred: single;
              const AContentCropCenter: TpointF;
              const aCornerRadius: TPointF;
              const ADrawMaterialFunc: TDrawMaterialFunc);
begin
  inherited create;
  FComparekey := AComparekey;
  FMaterialIdx := AMaterialIdx;
  FContentStream := AContentStream;
  fContentUrl := '';
  fDestSize :=  ADestSize;
  fContentBlurred := AContentBlurred;
  fContentCropCenter := AContentCropCenter;
  fCornerRadius := aCornerRadius;
  fDrawMaterialFunc := ADrawMaterialFunc;
End;

{*************}
//[MultiThread]
destructor TALDynamicScrollBoxDownloadMaterialExtData.Destroy;
begin
  ALFreeAndNil(FContentStream);
  Inherited Destroy;
end;

{*************}
//[MultiThread]
constructor TALDynamicScrollBoxControl.Create(const AOwner: TALDynamicScrollBox);
begin
  FOwnerScrollBox := AOwner;
  var LOwner: TALDynamicScrollBoxControl := Nil;
  create(LOwner);
end;

{*************************************************}
procedure TALDynamicScrollBoxControl.BeforeDestroy;
begin
  fisDestroying := True;
  SetOwner(nil);
  for Var I := low(FControls) to High(FControls) do
    FControls[i].BeforeDestroy;
end;

{************************************************}
procedure TALDynamicScrollBoxControl.AsyncDestroy;
begin
  fisDestroying := True;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxControl.MakeBufBitmap: TALDrawable;
begin
  If not visible then exit;
  for var I := low(FControls) to High(FControls) do
    FControls[i].MakeBufBitmap;
  result := ALNullDrawable;
end;

{**************************************************}
procedure TALDynamicScrollBoxControl.ClearBufBitmap;
begin
  for var I := low(FControls) to High(FControls) do
    FControls[i].ClearBufBitmap;
  FNeedArrangement := True;
end;

{***********************************************************************************}
function TALDynamicScrollBoxControl.LocalToFrame(const APoint: TALPointD): TALPointD;
begin
  Result := APoint;
  Result.Offset(BoundsRect.topleft);
  if Owner <> nil then result := Owner.LocalToFrame(Result);
end;

{********************************************************************************}
function TALDynamicScrollBoxControl.LocalToFrame(const ARect: TALRectD): TALRectD;
begin
  Result := ARect;
  Result.Offset(BoundsRect.topleft);
  if Owner <> nil then result := Owner.LocalToFrame(Result);
end;

{*********************************************************************************}
function TALDynamicScrollBoxControl.LocalToFrame(const APoint: TPointF): TALPointD;
begin
  result := LocalToFrame(TALPointD.Create(APoint));
end;

{******************************************************************************}
function TALDynamicScrollBoxControl.LocalToFrame(const ARect: TRectF): TALRectD;
begin
  result := LocalToFrame(TALRectD.Create(ARect));
end;

{*********************************************************************************}
function TALDynamicScrollBoxControl.FrameToLocal(const APoint: TPointF): TALPointD;
begin
  if Owner <> nil then Result := Owner.FrameToLocal(APoint)
  else Result := TALPointD.Create(APoint);
  Result.Offset(-BoundsRect.TopLeft);
end;

{******************************************************************************}
function TALDynamicScrollBoxControl.FrameToLocal(const ARect: TRectF): TALRectD;
begin
  if Owner <> nil then Result := Owner.FrameToLocal(ARect)
  else Result := TALRectD.Create(ARect);
  Result.Offset(-BoundsRect.TopLeft);
end;

{******************************************************************}
function TALDynamicScrollBoxControl.GetExpandedBoundsRect: TALRectD;
begin
  result := TALRectD.Create(
              BoundsRect.Left-TouchTargetExpansion.Left,
              BoundsRect.top-TouchTargetExpansion.top,
              BoundsRect.Right+TouchTargetExpansion.right,
              FboundsRect.Bottom+TouchTargetExpansion.bottom);
end;

{***************************************************************}
function TALDynamicScrollBoxControl.GetExpandedLocalRect: TRectF;
begin
  result := TRectF.Create(
              0-TouchTargetExpansion.Left,
              0-TouchTargetExpansion.top,
              FboundsRect.Width+TouchTargetExpansion.right,
              FboundsRect.Height+TouchTargetExpansion.bottom);
end;

{*************************************************}
function TALDynamicScrollBoxControl.GetTop: Double;
begin
  result := FboundsRect.Top;
end;

{****************************************************************}
procedure TALDynamicScrollBoxControl.SetTop(const AValue: Double);
begin
  var LboundsRect := FBoundsRect;
  LboundsRect.Location := TALPointD.Create(LboundsRect.Left, AValue);
  SetBoundsRect(LBoundsRect);
end;

{**************************************************}
function TALDynamicScrollBoxControl.GetLeft: Double;
begin
  result := FboundsRect.Left;
end;

{*****************************************************************}
procedure TALDynamicScrollBoxControl.SetLeft(const AValue: Double);
begin
  var LboundsRect := FBoundsRect;
  LboundsRect.Location := TALPointD.Create(AValue, LboundsRect.top);
  SetBoundsRect(LBoundsRect);
end;

{*******************************************************************************************************}
procedure TALDynamicScrollBoxControl.Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single);
begin
  if length(FControls) > 0 then begin
    var LSavedMatrix := Canvas.Matrix;
    try
      for var i := low(FControls) to High(FControls) do begin
        //*if (TouchEffectStyle <> TTouchEffectStyle.None) and (TouchEffectZorder = i) then begin
        //*  if i > low(FControls) then Canvas.SetMatrix(LSavedMatrix);
        //*  DrawTouchEffect(Canvas, ARect, aOpacity);
        //*end;
        //--
        Var LControl := FControls[i];
        if not LControl.visible then continue;
        Var LDeltaRect := LControl.BoundsRect.ReducePrecision;
        var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(LDeltaRect.left,LDeltaRect.Top);
        Canvas.SetMatrix(LMatrix);
        Var LDeltaTopLeft := LDeltaRect.TopLeft;
        LDeltaRect := TRectF.Intersect(ARect, LDeltaRect);
        LDeltaRect.Offset(-LDeltaTopLeft);
        //**
        if LControl is TALDynamicScrollBoxRectangle then begin
        //**
          TALDynamicScrollBoxRectangle(LControl).Canvas := Canvas;
          TALDynamicScrollBoxRectangle(LControl).Paint;
        end
        else
          LControl.Paint(Canvas, LDeltaRect, aOpacity);
      end;
    finally
      Canvas.SetMatrix(LSavedMatrix);
    end;
  end;
  //--
  //*if (TouchEffectStyle <> TTouchEffectStyle.None) and (TouchEffectZorder > High(FControls)) then
  //*  DrawTouchEffect(Canvas, ARect, aOpacity);
end;

{************************************************************}
function TALDynamicScrollBoxControl.GetControlsCount: Integer;
begin
  Result := Length(FControls);
end;

{*********************************************************************************************}
procedure TALDynamicScrollBoxControl.InsertControl(const AControl: TALDynamicScrollBoxControl);

Var
  LOwnerItem: TALDynamicScrollBoxItem;
  LOwnerView: TALDynamicScrollBoxView;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _SetRelativeOwners(const AControl: TALDynamicScrollBoxControl);
  begin
    AControl.FOwnerScrollBox := OwnerScrollBox;
    if AControl.FOwnerView = nil then AControl.FOwnerView := LOwnerView;
    if AControl.FOwnerItem = nil then AControl.FOwnerItem := LOwnerItem;
    for var I := low(AControl.FControls) to high(AControl.FControls) do
      _SetRelativeOwners(AControl.FControls[i]);
  end;

begin
  if AControl = nil then exit;
  if AControl.FOwner <> nil then
    AControl.FOwner.RemoveControl(AControl);
  Setlength(FControls, length(FControls) + 1);
  FControls[High(FControls)] := AControl;
  AControl.FControlIndex := High(FControls);
  AControl.FOwner := self;
  if IsScrollBoxItem then LOwnerItem := TALDynamicScrollBoxItem(Self)
  else LOwnerItem := FOwnerItem;
  if IsScrollBoxView then LOwnerView := TALDynamicScrollBoxView(Self)
  else LOwnerView := FOwnerView;
  _SetRelativeOwners(AControl);
  FNeedArrangement := True;
end;

{*********************************************************************************************}
procedure TALDynamicScrollBoxControl.RemoveControl(const AControl: TALDynamicScrollBoxControl);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ClearRelativeOwners(const AControl: TALDynamicScrollBoxControl);
  begin
    if AControl.FOwnerScrollBox <> nil then begin
      if AControl.FOwnerScrollBox.FTouchEffectOwner = self then AControl.FOwnerScrollBox.FTouchEffectOwner := nil;
      if AControl.FOwnerScrollBox.FHovered = self then AControl.FOwnerScrollBox.FHovered := nil;
      AControl.FOwnerScrollBox := nil;
      if (AControl.FOwnerView = self) or (AControl.FOwnerView = FOwnerView) then AControl.FOwnerView := nil;
      if (AControl.FOwnerItem = self) or (AControl.FOwnerItem = FOwnerItem) then AControl.FOwnerItem := nil;
      for var I := low(AControl.FControls) to high(AControl.FControls) do
        _ClearRelativeOwners(AControl.FControls[i]);
    end;
  end;

begin
  if FIsDestroying then exit;
  if AControl = nil then exit;
  for var I := low(FControls) to high(FControls) do begin
    if FControls[i] = AControl then begin
      if I < high(FControls) then
        ALMove(FControls[i + 1], FControls[i], (high(FControls) - i) * SizeOf(Pointer));
      Setlength(FControls, length(FControls) - 1);
      AControl.FOwner := nil;
      _ClearRelativeOwners(AControl);
      AControl.FControlIndex := -1;
      for var J := I to High(FControls) do
        FControls[J].FControlIndex := J;
      FNeedArrangement := True;
      break;
    end;
  end;
end;

{*********************************************************************************************************************}
procedure TALDynamicScrollBoxControl.MoveControl(const AControl: TALDynamicScrollBoxControl; const ANewIndex: Integer);
begin
  var LNewIndex := Max(Low(FControls), Min(High(FControls), ANewIndex));
  if AControl.ControlIndex = LNewIndex then exit;
  if AControl.ControlIndex < LNewIndex then begin
    Move(FControls[AControl.ControlIndex + 1], FControls[AControl.ControlIndex], (LNewIndex - AControl.ControlIndex) * SizeOf(Pointer));
    FControls[LNewIndex] := AControl;
    for var I := AControl.ControlIndex to LNewIndex do
      FControls[i].FControlIndex := I;
  end
  else begin
    Move(FControls[LNewIndex], FControls[LNewIndex + 1], (AControl.ControlIndex - LNewIndex) * SizeOf(Pointer));
    FControls[LNewIndex] := AControl;
    for var I := LNewIndex to AControl.ControlIndex do
      FControls[i].FControlIndex := I;
  end;
end;

{*************************************************************************************}
procedure TALDynamicScrollBoxControl.SetOwner(const Value: TALDynamicScrollBoxControl);
begin
  if Value <> fOwner then begin
    // this will set FOwner to nil
    if FOwner <> nil then
      FOwner.RemoveControl(self);
    // this will set FOwner = Value
    if Value <> nil then
      value.InsertControl(Self);
  end;
end;

{**************************************************}
function TALDynamicScrollBoxControl.GetControlAtPos(
           const aPos: TPointF; // APos is local to the control
           out AControlPos: TPointF; // AControlPos is local to the founded control
           const ADirectChildOnly: Boolean = False;
           const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl;
begin
  if not ExpandedLocalRect.Contains(aPos) then begin
    AControlPos := TPointF.Zero;
    exit(nil);
  end;
  //--
  for var i := High(FControls) downto low(FControls) do begin
    var LControl := FControls[i];
    if not LControl.Visible then continue;
    if LControl.ExpandedBoundsRect.Contains(aPos) then begin
      if not ADirectChildOnly then
        result := LControl.GetControlAtPos(
                    APos - LControl.BoundsRect.TopLeft.ReducePrecision,
                    AControlPos,
                    ADirectChildOnly,
                    ACheckHitTest)
      else
        Result := nil;
      if Result = nil then begin
        IF ACheckHitTest and (not LControl.HitTest) then AControlPos := TPointF.Zero
        else begin
          result := LControl;
          AControlPos := APos - LControl.BoundsRect.TopLeft.ReducePrecision;
        end;
      end;
      exit;
    end;
  end;
  //--
  IF ACheckHitTest and (not HitTest) then begin
    Result := Nil;
    AControlPos := TPointF.Zero
  end
  else begin
    result := Self;
    AControlPos := aPos;
  end;
end;

{**************************************************}
function TALDynamicScrollBoxControl.GetControlAtPos(
           const aPos: TPointF;
           const ADirectChildOnly: Boolean = False;
           const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl;
begin
  Var LControlPos: TPointF;
  result := GetControlAtPos(aPos, LControlPos, ADirectChildOnly, ACheckHitTest);
end;

{**********************************************}
procedure TALDynamicScrollBoxControl.AdjustSize;
begin
(*
  if AutoSize then begin
    fIsAdjustingSize := True;
    Try
      var LSize := TSizeF.Create(0,0);
      for var I := low(FControls) to high(FControls) do begin
        var LControl := FControls[i];
        case LControl.Align of

          {$REGION 'None'}
          TALAlignLayout.None:;
          {$ENDREGION}

          {$REGION 'Top,Bottom,TopCenter,BottomCenter,HorzCenter,Vertical'}
          TALAlignLayout.Top,
          TALAlignLayout.Bottom,
          TALAlignLayout.TopCenter,
          TALAlignLayout.BottomCenter,
          TALAlignLayout.HorzCenter,
          TALAlignLayout.Vertical: begin
            LSize.Width := Max(LSize.Width, Padding.Left + LControl.Margins.left + LControl.Width + LControl.Margins.right + padding.Right);
            if LSize.Height = 0 then LSize.height := LSize.height + Padding.top + Padding.bottom;
            LSize.height := LSize.height + LControl.Margins.top + LControl.Height + LControl.Margins.bottom;
          end;
          {$ENDREGION}

          {$REGION 'Left,Right,LeftCenter,RightCenter,VertCenter,Horizontal'}
          TALAlignLayout.Left,
          TALAlignLayout.Right,
          TALAlignLayout.LeftCenter,
          TALAlignLayout.RightCenter,
          TALAlignLayout.VertCenter,
          TALAlignLayout.Horizontal: begin
            LSize.Height := Max(LSize.Height, Padding.top + LControl.Margins.top + LControl.Height + LControl.Margins.bottom + padding.Bottom);
            if LSize.Width = 0 then LSize.Width := LSize.Width + Padding.left + Padding.right;
            LSize.Width := LSize.Width + LControl.Margins.left + LControl.Width + LControl.Margins.right;
          end;
          {$ENDREGION}

          {$REGION 'Center,Client'}
          TALAlignLayout.Center,
          TALAlignLayout.Client: begin
            LSize.Width := Max(LSize.Width, Padding.Left + LControl.Margins.left + LControl.Width + LControl.Margins.right + padding.Right);
            LSize.Height := Max(LSize.Height, Padding.top + LControl.Margins.top + LControl.Height + LControl.Margins.bottom + padding.Bottom);
          end;
          {$ENDREGION}

        end;
      end;

      // This to take care of the align constraint
      if Align in [TAlign.Top,
                   TALAlignLayout.Bottom,
                   TALAlignLayout.Horizontal,
                   TALAlignLayout.VertCenter,
                   TALAlignLayout.Client] then begin
        LSize.Width := Width;
      end;
      if Align in [TAlign.Left,
                   TALAlignLayout.Right,
                   TALAlignLayout.HorzCenter,
                   TALAlignLayout.Vertical,
                   TALAlignLayout.Client] then begin
        LSize.height := height;
      end;

      if LSize.Width = 0 then LSize.Width := Width;
      if LSize.Height = 0 then LSize.Height := Height;
      SetBoundsRect(TALRectD.Create(FBoundsRect.TopLeft, LSize.Width, LSize.Height));

    finally
      fIsAdjustingSize := False;
    End;
  end;
*)
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxControl.DownloadMaterials;
begin
  Try
    DoDownloadMaterials;
  except
    on E: Exception do
      ALLog(ALFormatW('%s.DownloadMaterials', [Classname]), E);
  End;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxControl.DoDownloadMaterials;
begin
  // * In descendant download the materials using TALNetHttpClientPool.Instance with :
  //     CanStartDownloadMaterial
  //     OnSuccessDownloadMaterial
  //     OnErrorDownloadMaterial
  // * In descendant you must also override those functions :
  //     DoBuildMaterial
  //     DoShiftMaterial
  // * do not forget to call for each download:
  //   if not OwnerScrollBox.IncrementWaitForCount then exit;
  // * for ExtData use TALDynamicScrollBoxDownloadMaterialExtData
  // * set EphemeralLayout.materialxxx.Status := TALDownloadingStatus.Downloading;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxControl.DownloadMaterial(
            const AMaterialIdx: Integer;
            var AMediaStatus: TALDownloadingStatus;
            const aMediaURL: String;
            const AMediaBlurred: single;
            const AMediaCropCenter: TpointF;
            const aDestSize: TSizeF;
            const aCornerRadius: TPointF;
            const ADrawFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc;
            const ADrawEmptyNode: boolean = false);
begin
  if AMediaStatus = TALDownloadingStatus.None then begin
    (*
    var LRefreshingMaterial := ExtractMaterialFromRefreshingEphemeralLayout(AMaterialIdx);
    if (LRefreshingMaterial <> nil) and
       (SameValue(LRefreshingMaterial.Width, aDestSize.Width * ALGetScreenScale, 1)) and           // aDestSize use float but RefreshingMaterial.Width/height use Integer
       (SameValue(LRefreshingMaterial.height, aDestSize.height * ALGetScreenScale, 1)) then begin  // so it's could be a difference of max 1 pixel
      if not OwnerScrollBox.IncrementWaitForCount then exit;
      ShiftMaterial(
        Key, // const AComparekey: Integer;
        AMaterialIdx, // const AMaterialIdx: Integer;
        LRefreshingMaterial); // const aMaterial: TALDrawable
      exit;
    end;
    *)
    if (not ADrawEmptyNode) and (AMediaUrl = '') then AMediaStatus := TALDownloadingStatus.Processed
    else begin
      {$IFDEF DEBUG}
      var LMediaURL := aMediaURL;
      if ALRandom32(10) = 0 then LMediaURL := LMediaURL + '.bad';
      {$ENDIF}
      AMediaStatus := TALDownloadingStatus.Downloading;
      //*if not OwnerScrollBox.IncrementWaitForCount then exit;
      TALNetHttpClientPool.Instance.Get(
        {$IFDEF DEBUG}LMediaURL{$ELSE}AMediaUrl{$ENDIF}, // const AUrl: String;
        CanStartDownloadMaterial, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
        OnSuccessDownloadMaterial, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
        OnErrorDownloadMaterial, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
        TALDynamicScrollBoxDownloadMaterialExtData.Create(
          Key, // const AComparekey: Integer;
          AMaterialIdx, // const AMaterialIdx: Integer;
          {$IFDEF DEBUG}LMediaURL{$ELSE}AMediaUrl{$ENDIF}, // const AContentUrl: String;
          ADestSize, // const ADestSize: TSizeF;
          AMediaBlurred, // const AContentBlurred: single;
          AMediaCropCenter, // const AContentCropCenter: TpointF;
          aCornerRadius, // const aCornerRadius: TPointF;
          ADrawFunc), // const ADrawMaterialFunc: TDrawMaterialFunc
        true, // const AUseCache: Boolean = True;
        GetPriority); // const APriority: Int64 = 0
    end;
  end;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxControl.DownloadMaterial(
            const AMaterialIdx: Integer;
            var AMediaStatus: TALDownloadingStatus;
            const aMediaNode: TALJSONNodeW;
            const aCropped: Boolean;
            const aDestSize: TSizeF;
            const aCornerRadius: TPointF;
            const ADrawFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc;
            const ADrawEmptyNode: boolean = false);
begin
//*  if AMediaStatus = TALDownloadingStatus.None then begin
//*    (*
//*    var LRefreshingMaterial := ExtractMaterialFromRefreshingEphemeralLayout(AMaterialIdx);
//*    if (LRefreshingMaterial <> nil) and
//*       (SameValue(LRefreshingMaterial.Width, aDestSize.Width * ALGetScreenScale, 1)) and           // aDestSize use float but RefreshingMaterial.Width/height use Integer
//*       (SameValue(LRefreshingMaterial.height, aDestSize.height * ALGetScreenScale, 1)) then begin  // so it's could be a difference of max 1 pixel
//*      if not OwnerScrollBox.IncrementWaitForCount then exit;
//*      ShiftMaterial(
//*        Key, // const AComparekey: Integer;
//*        AMaterialIdx, // const AMaterialIdx: Integer;
//*        LRefreshingMaterial); // const aMaterial: TALDrawable
//*      exit;
//*    end;
//*    *)
//*    var LMediaBlurred: single;
//*    var LMediaSize: TSizeF;
//*    var LMediaCropCenter: TpointF;
//*    var LMediaPicUrl := ALGetPictureUrl(
//*                          AMediaNode, // const aPicNode: TALJSONNodeW;
//*                          ADestSize.Width, // const aMinWidth: single; // virtual pixel
//*                          ADestSize.Height, // const aMinHeight: single; // virtual pixel
//*                          ALGetScreenScale, // const aScreenScale: single;
//*                          aCropped, // const aCropped: Boolean; // return if possible the url of the cropped image
//*                          LMediaBlurred, // var   aPicBlurred: single;
//*                          LMediaSize, // Var   aPicSize: TpointF; // real pixel - the original picture size of the image or if cropped the dimension of the cropped picture
//*                          LMediaCropCenter); // Var   aPicCropCenter: TpointF
//*    if (not ADrawEmptyNode) and (LMediaPicUrl = '') then AMediaStatus := TALDownloadingStatus.Processed
//*    else begin
//*      {$IFDEF DEBUG}
//*      if ALRandom32(10) = 0 then LMediaPicUrl := LMediaPicUrl + '.bad';
//*      {$ENDIF}
//*      AMediaStatus := TALDownloadingStatus.Downloading;
//*      if not OwnerScrollBox.IncrementWaitForCount then exit;
//*      TALNetHttpClientPool.Instance.Get(
//*        LMediaPicUrl, // const AUrl: String;
//*        CanStartDownloadMaterial, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
//*        OnSuccessDownloadMaterial, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
//*        OnErrorDownloadMaterial, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
//*        TALDynamicScrollBoxDownloadMaterialExtData.Create(
//*          Key, // const AComparekey: Integer;
//*          AMaterialIdx, // const AMaterialIdx: Integer;
//*          LMediaPicUrl, // const AContentUrl: String;
//*          ADestSize, // const ADestSize: TSizeF;
//*          LMediaBlurred, // const AContentBlurred: single;
//*          LMediaCropCenter, // const AContentCropCenter: TpointF;
//*          aCornerRadius, // const aCornerRadius: TPointF;
//*          ADrawFunc), // const ADrawMaterialFunc: TDrawMaterialFunc
//*        true, // const AUseCache: Boolean = True;
//*        GetPriority); // const APriority: Int64 = 0
//*    end;
//*  end;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxControl.CanStartDownloadMaterial(var AExtData: TObject): boolean;
begin
  Try
    var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(AExtData);
    result := DoCanStartDownloadMaterial(
                LDownloadMaterialExtData.Comparekey,
                LDownloadMaterialExtData.MaterialIdx);
    if OwnerScrollBox = nil then exit;
    //*if not result then OwnerScrollBox.DecrementWaitForCount;
    {$IFDEF DEBUG}
    if OwnerItem <> nil then begin
      var LLogtype: TalLogType;
      if Result then LLogtype := TalLogType.verbose
      else LLogtype := TalLogType.warn;
      ALLog(
        Classname+'.CanStartDownloadMaterial.'+ALIfThenW(result,'OK','KO'),
        'ItemIdx:'+ALIntToStrW(OwnerItem.ItemIndex)+' | '+
        'ControlIdx:'+ALIntToStrW(ControlIndex) + ' | ' +
        'MaterialIdx:'+ALIntToStrW(LDownloadMaterialExtData.MaterialIdx) + ' | ' +
        'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
        'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
        'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
        LLogtype);
    end;
    {$ENDIF}
  except
    On E: Exception do begin
      ALLog(ALFormatW('%s.CanStartDownloadMaterial', [Classname]), E);
      result := True;
    end;
  End;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxControl.DoCanStartDownloadMaterial(
           const aCompareKey: Integer;
           const aMaterialIdx: Integer): boolean;
begin
  //*result := (OwnerScrollBox <> nil) and (not OwnerScrollBox.isDestroying) and IsValidKey(aCompareKey);
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxControl.OnSuccessDownloadMaterial(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AExtData: TObject);
begin

  {$IFDEF DEBUG}
  //*TALLifecycleMonitor.Instance.AppTerminatingSignal.WaitFor(ALRandom32(3000));
  {$ENDIF}

  Try
    var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(AExtData);
    {$IFDEF DEBUG}
    //{-----
    if OwnerItem <> nil then begin
      Var LLogKO := not IsValidKey(LDownloadMaterialExtData.CompareKey);
      var LLogtype: TalLogType;
      if LLogKO then LLogtype := TalLogType.warn
      else LLogtype := TalLogType.verbose;
      ALLog(
        Classname+'.OnSuccessDownloadMaterial.'+ALIfThenW(LLogKO, 'KO', 'OK'),
        'ItemIdx:'+ALIntToStrW(OwnerItem.ItemIndex)+' | '+
        'ControlIdx:'+ALIntToStrW(ControlIndex) + ' | ' +
        'MaterialIdx:'+ALIntToStrW(LDownloadMaterialExtData.MaterialIdx) + ' | ' +
        'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
        'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
        'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
        LLogtype);
    end;
    //----}
    {$ENDIF}
    if not IsValidKey(LDownloadMaterialExtData.CompareKey) then begin
      //*OwnerScrollBox.DecrementWaitForCount;
      exit;
    end;
    LDownloadMaterialExtData.ContentStream := AContentStream;
    AContentStream := nil; // AContentStream will be free by LDownloadMaterialExtData
    TALGraphicThreadPool.Instance.ExecuteProc(
      BuildMaterial, // const AProc: TALWorkerThreadProc;
      LDownloadMaterialExtData, // const AExtData: Tobject; TALGraphicThreadPool.Instance will own and release the ExtData object
      GetPriority); // const APriority: Int64 = 0;
    AExtData := nil; // AExtData will be free by TALGraphicThreadPool.Instance
  except
    On E: Exception do begin
      ALLog(ALFormatW('%s.OnSuccessDownloadMaterial', [Classname]), E);
      //*OwnerScrollBox.DecrementWaitForCount;
    end;
  End;

end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxControl.OnErrorDownloadMaterial(const AErrMessage: string; var AExtData: Tobject);
begin
  Try
    var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(AExtData);
    var LCompareKey := LDownloadMaterialExtData.CompareKey;
    var LMaterialIdx := LDownloadMaterialExtData.MaterialIdx;
    {$IFDEF DEBUG}
    if OwnerItem <> nil then begin
      ALLog(
        Classname+'.OnErrorDownloadMaterial',
        'ErrMessage:'+AErrMessage + ' | ' +
        'ItemIdx:'+ALIntToStrW(OwnerItem.ItemIndex)+' | '+
        'ControlIdx:'+ALIntToStrW(ControlIndex) + ' | ' +
        'MaterialIdx:'+ALIntToStrW(LMaterialIdx) + ' | ' +
        'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
        'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
        'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
        TalLogType.error);
    end;
    {$ENDIF}
    if not IsValidKey(LCompareKey) then begin
      //*OwnerScrollBox.DecrementWaitForCount;
      exit;
    end;
    TThread.queue(nil,
      procedure
      begin
        ShiftMaterial(
          LCompareKey,
          LMaterialIdx,
          ALNullDrawable);
      end);
  except
    On E: Exception do begin
      ALLog(ALFormatW('%s.BuildMaterial', [Classname]), E);
      //*OwnerScrollBox.DecrementWaitForCount;
    end;
  End;
end;

{*************************************************************************************************************************}
function TALDynamicScrollBoxControl.ExtractMaterialFromRefreshingEphemeralLayout(const aMaterialIdx: Integer): TALDrawable;
begin
  result := ALNullDrawable;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxControl.BuildMaterial(var AExtData: TObject);
begin
  //*OwnerScrollBox.BeginRead;
  try
    Try
      var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(AExtData);
      var LCompareKey := LDownloadMaterialExtData.CompareKey;
      var LMaterialIdx := LDownloadMaterialExtData.MaterialIdx;
      {$IFDEF DEBUG}
      //{-----
      if OwnerItem <> nil then begin
        Var LLogKO := not IsValidKey(LCompareKey);
        var LLogtype: TalLogType;
        if LLogKO then LLogtype := TalLogType.warn
        else LLogtype := TalLogType.verbose;
        ALLog(
          Classname+'.BuildMaterial.'+ALIfThenW(LLogKO, 'KO', 'OK'),
          'ItemIdx:'+ALIntToStrW(OwnerItem.ItemIndex)+' | '+
          'ControlIdx:'+ALIntToStrW(ControlIndex) + ' | ' +
          'MaterialIdx:'+ALIntToStrW(LMaterialIdx) + ' | ' +
          'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
          'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
          'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
          LLogtype);
      end;
      //-----}
      {$ENDIF}
      if not IsValidKey(LCompareKey) then begin
        //*OwnerScrollBox.DecrementWaitForCount;
        exit;
      end;
      Var LMaterial := DoBuildMaterial(LDownloadMaterialExtData);
      TThread.queue(nil,
        procedure
        begin
          ShiftMaterial(
            LCompareKey,
            LMaterialIdx,
            LMaterial);
        end);
    except
      On E: Exception do begin
        ALLog(ALFormatW('%s.BuildMaterial', [Classname]), E);
        //*OwnerScrollBox.DecrementWaitForCount;
      end;
    End;
  finally
    //*OwnerScrollBox.EndRead;
  end;
end;

{********************************}
//[MultiThread][BeginRead/EndRead]
function TALDynamicScrollBoxControl.DoBuildMaterial(const ADownloadMaterialExtData: TALDynamicScrollBoxDownloadMaterialExtData): TALDrawable;
begin
  Var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(ADownloadMaterialExtData);
  if assigned(LDownloadMaterialExtData.DrawMaterialFunc) then
    Result := LDownloadMaterialExtData.DrawMaterialFunc(
                self,
                LDownloadMaterialExtData.ContentStream,
                LDownloadMaterialExtData.ContentUrl,
                LDownloadMaterialExtData.DestSize,
                LDownloadMaterialExtData.ContentBlurred,
                LDownloadMaterialExtData.ContentCropCenter,
                LDownloadMaterialExtData.CornerRadius,
                ALGetScreenScale)
  else
    raise Exception.Create('Error 5F9A9E7B-9D6C-4451-AF23-3AE9325EB3AB');
end;

{*************************************************}
procedure TALDynamicScrollBoxControl.ShiftMaterial(
            const aCompareKey: Integer;
            const aMaterialIdx: Integer;
            const aMaterial: TALDrawable);
begin
  {$IFDEF DEBUG}
  //{-----
  if OwnerItem <> nil then begin
    Var LLogKO := not IsValidKey(aCompareKey);
    var LLogtype: TalLogType;
    if LLogKO then LLogtype := TalLogType.warn
    else LLogtype := TalLogType.verbose;
    ALLog(
      Classname+'.ShiftMaterial.'+ALIfThenW(LLogKO, 'KO', 'OK'),
      'ItemIdx:'+ALIntToStrW(OwnerItem.ItemIndex)+' | '+
      'ControlIdx:'+ALIntToStrW(ControlIndex) + ' | ' +
      'MaterialIdx:'+ALIntToStrW(aMaterialIdx) + ' | ' +
      'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
      'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
      'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
      LLogtype);
  end;
  //-----}
  {$ENDIF}
  Try
    Try
      //*if not IsValidKey(aCompareKey) then ALFreeAndNil(aMaterial, true{aDelayed})
      //*else begin
      //*  DoShiftMaterial(aMaterialIdx, aMaterial);
      //*  OwnerScrollBox.repaint;
      //*end;
    except
      On E: Exception do
        ALLOG(ALFormatW('%s.ShiftMaterial', [Classname]), E);
    End;
  finally
    //*OwnerScrollBox.DecrementWaitForCount;
  end;
end;

{***************************************************}
procedure TALDynamicScrollBoxControl.DoShiftMaterial(
            const aMaterialIdx: Integer;
            const aMaterial: TALDrawable);
begin
  // if aMaterial = nil then set the status of the material in the EphemeralLayout to TALDownloadingStatus.Error
end;

{**************************************************}
function TALDynamicScrollBoxControl.GetKey: integer;
begin
  Result := 1;
end;

{**********************************************************************************}
function TALDynamicScrollBoxControl.IsValidKey(Const aCompareKey: integer): Boolean;
begin
  result := aCompareKey = 1;
end;

{******************************************************************************}
function TALDynamicScrollBoxControl.GetPriority(const AExtData: Tobject): Int64;
begin
  if OwnerItem <> nil then result := OwnerItem.GetPriority(AExtData)
  else if OwnerView <> nil then Result := OwnerView.GetPriority(AExtData)
  else if OwnerScrollBox <> nil then result := OwnerScrollBox.GetPriority(AExtData)
  else result := 0;
end;

{***********************************************************************************************************************}
//*procedure TALDynamicScrollBoxControl.StartTouchEffect(Const AEvent: TStartTouchEffectEvent; Const AMousePos: TpointF);
//*
//*  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
//*  function _GetAlphaColorRec: TAlphaColorRec;
//*  begin
//*    case TouchEffectStyle of
//*      TTouchEffectStyle.ClickFadeOutRectangle,
//*      TTouchEffectStyle.LongPressFadeOutRectangle: Result := TalphaColorRec.create(TouchEffectRectangleFillColor);
//*      TTouchEffectStyle.ClickFadeOutCircle,
//*      TTouchEffectStyle.LongPressFadeOutCircle: Result := TalphaColorRec.create(TouchEffectCircleFillColor);
//*      else raise Exception.Create('Error 2B59BC60-4B3A-4651-BACF-507274AA6A4A');
//*    end;
//*  end;
//*
//*begin
//*
//*  {$REGION 'ClickFadeOutRectangle/ClickFadeOutCircle'}
//*  If TouchEffectStyle in [TTouchEffectStyle.ClickFadeOutRectangle,
//*                          TTouchEffectStyle.ClickFadeOutCircle] then begin
//*
//*    if (AEvent = TStartTouchEffectEvent.MouseDown) and (not StartTouchEffectOnMouseDown) then exit;
//*    if (AEvent = TStartTouchEffectEvent.Click) and (StartTouchEffectOnMouseDown) then exit;
//*    //
//*    InitTouchEffectBitmaps;
//*    //
//*    OwnerScrollBox.TouchEffectOwner := Self;
//*    OwnerScrollBox.TouchEffectMousePos := AMousePos;
//*    OwnerScrollBox.TouchEffectClicked := False;
//*    //
//*    OwnerScrollBox.TouchEffectAnim.Enabled := false;
//*    OwnerScrollBox.TouchEffectAnim.Tag := 0;
//*    OwnerScrollBox.TouchEffectAnim.TagFloat := 0;
//*    OwnerScrollBox.TouchEffectAnim.Delay := 0.1;
//*    OwnerScrollBox.TouchEffectAnim.Duration := 0.2;
//*    OwnerScrollBox.TouchEffectAnim.StartValue := ((100 / 255) * _GetAlphaColorRec.A) / 100;
//*    OwnerScrollBox.TouchEffectAnim.StopValue := 0;
//*    OwnerScrollBox.TouchEffectAnim.start;
//*    //
//*    OwnerScrollBox.Repaint;
//*
//*  end
//*  {$ENDREGION}
//*
//*  {$REGION 'ClickFadeOutRectangleAndCircle'}
//*  else If TouchEffectStyle = TTouchEffectStyle.ClickFadeOutRectangleAndCircle then begin
//*
//*    if (AEvent = TStartTouchEffectEvent.MouseDown) and (not StartTouchEffectOnMouseDown) then exit;
//*    if (AEvent = TStartTouchEffectEvent.Click) and (StartTouchEffectOnMouseDown) then exit;
//*    //
//*    InitTouchEffectBitmaps;
//*    //
//*    OwnerScrollBox.TouchEffectOwner := Self;
//*    OwnerScrollBox.TouchEffectMousePos := AMousePos;
//*    OwnerScrollBox.TouchEffectClicked := False;
//*    //
//*    OwnerScrollBox.TouchEffectAnim.Enabled := false;
//*    OwnerScrollBox.TouchEffectAnim.Tag := 0;
//*    OwnerScrollBox.TouchEffectAnim.TagFloat := ((100 / 255) * TalphaColorRec.create(TouchEffectCircleFillColor).A) / 100;
//*    OwnerScrollBox.TouchEffectAnim.Delay := 0;
//*    OwnerScrollBox.TouchEffectAnim.Duration := 0.1;
//*    OwnerScrollBox.TouchEffectAnim.StartValue := 0;
//*    if OwnerScrollBox.TouchEffectCircleBitmap = nil then raise Exception.Create('Error 32DF1709-1BAE-49F8-8052-313C51E08663');
//*    OwnerScrollBox.TouchEffectAnim.StopValue := OwnerScrollBox.TouchEffectCircleBitmap.Width / ALGetScreenScale;
//*    OwnerScrollBox.TouchEffectAnim.start;
//*    //
//*    OwnerScrollBox.Repaint;
//*
//*  end
//*  {$ENDREGION}
//*
//*  {$REGION 'LongPressFadeOutRectangle/LongPressFadeOutCircle'}
//*  else If TouchEffectStyle in [TTouchEffectStyle.LongPressFadeOutRectangle,
//*                               TTouchEffectStyle.LongPressFadeOutCircle] then begin
//*
//*    if (AEvent = TStartTouchEffectEvent.Click) then exit;
//*    //
//*    InitTouchEffectBitmaps;
//*    //
//*    OwnerScrollBox.TouchEffectOwner := Self;
//*    OwnerScrollBox.TouchEffectMousePos := AMousePos;
//*    OwnerScrollBox.TouchEffectClicked := False;
//*    //
//*    OwnerScrollBox.TouchEffectAnim.Enabled := false;
//*    OwnerScrollBox.TouchEffectAnim.Tag := 0;
//*    OwnerScrollBox.TouchEffectAnim.TagFloat := 0;
//*    OwnerScrollBox.TouchEffectAnim.Delay := 0;
//*    OwnerScrollBox.TouchEffectAnim.Duration := 0.3;
//*    OwnerScrollBox.TouchEffectAnim.StartValue := ((100 / 255) * _GetAlphaColorRec.A) / 100;
//*    OwnerScrollBox.TouchEffectAnim.StopValue := 0;
//*    //
//*    OwnerScrollBox.Repaint;
//*
//*  end
//*  {$ENDREGION}
//*
//*  {$REGION 'Unhandled'}
//*  else If not (TouchEffectStyle in [TTouchEffectStyle.None,
//*                                    TTouchEffectStyle.Custom]) then begin
//*
//*    raise Exception.Create('Error 1D10C36E-4CE5-4182-88C0-30AD4BD86F00');
//*
//*  end;
//*  {$ENDREGION}
//*
//*end;
//*
//*{*********************************************************}
//*procedure TALDynamicScrollBoxControl.FadeOutTouchEffect;
//*begin
//*  If (OwnerScrollBox.TouchEffectOwner = self) and
//*     (TouchEffectStyle in [TTouchEffectStyle.LongPressFadeOutRectangle,
//*                           TTouchEffectStyle.LongPressFadeOutCircle]) then
//*    OwnerScrollBox.TouchEffectAnim.Enabled := True;
//*end;
//*
//*{**********************************************************************}
//*function TALDynamicScrollBoxControl.GetTouchEffectBoundsRect: TrectF;
//*begin
//*  if TouchEffectBoundsconstraint = TTouchEffectBoundsConstraint.LocalRect then result := LocalRect
//*  else if TouchEffectBoundsconstraint = TTouchEffectBoundsConstraint.ExpandedLocalRect then begin
//*    Result := LocalRect;
//*    Result.Inflate(TouchTargetExpansion.Left,TouchTargetExpansion.Top,TouchTargetExpansion.Right,TouchTargetExpansion.Bottom);
//*  end
//*  else raise Exception.Create('Error 91135F73-2BD4-42A6-8456-DE8D4B04C187');
//*  //--
//*  if TouchEffectStyle in [TTouchEffectStyle.ClickFadeOutCircle,
//*                          TTouchEffectStyle.LongPressFadeOutCircle] then begin
//*    var LTmpRect := Result;
//*    Result.Width := max(Result.Width, Result.Height);
//*    Result.Height := max(Result.Width, Result.Height);
//*    Result := Result.CenterAt(LTmpRect);
//*  end
//*  else if not (TouchEffectStyle in [TTouchEffectStyle.None,
//*                                    TTouchEffectStyle.Custom,
//*                                    TTouchEffectStyle.ClickFadeOutRectangle,
//*                                    TTouchEffectStyle.ClickFadeOutRectangleAndCircle,
//*                                    TTouchEffectStyle.LongPressFadeOutRectangle]) then begin
//*    raise Exception.Create('Error A99AA159-C7F5-4569-A7F7-63A0452B7A8F');
//*  end;
//*end;
//*
//*{*************************************************************}
//*procedure TALDynamicScrollBoxControl.InitTouchEffectBitmaps;
//*begin
//*
//*  {$REGION 'ClickFadeOutRectangleAndCircle'}
//*  if TouchEffectStyle = TTouchEffectStyle.ClickFadeOutRectangleAndCircle then begin
//*    var LTouchEffectBoundsRect := GetTouchEffectBoundsRect;
//*    OwnerScrollBox.TouchEffectRectangleBitmap := TALImageCacheEngine.Instance.RetrieveRectImage(TouchEffectRectangleFillColor, LTouchEffectBoundsRect.Width * ALGetScreenScale, LTouchEffectBoundsRect.Height * ALGetScreenScale);
//*    OwnerScrollBox.OwnTouchEffectRectangleBitmap := false;
//*    var LCircleAlphaColorRec := TalphaColorRec.create(TouchEffectCircleFillColor);
//*    LCircleAlphaColorRec.A := 255;
//*    Var LCircleSize := Min(180, max(LTouchEffectBoundsRect.Width/1.25,LTouchEffectBoundsRect.Height/1.25));
//*    OwnerScrollBox.TouchEffectCircleBitmap := TALImageCacheEngine.Instance.RetrieveCircleImage(LCircleAlphaColorRec.Color, LCircleSize * ALGetScreenScale, LCircleSize * ALGetScreenScale);
//*    OwnerScrollBox.OwnTouchEffectCircleBitmap := false;
//*  end
//*  {$ENDREGION}
//*
//*  {$REGION 'ClickFadeOutRectangle/LongPressFadeOutRectangle'}
//*  else if TouchEffectStyle in [TTouchEffectStyle.ClickFadeOutRectangle,
//*                               TTouchEffectStyle.LongPressFadeOutRectangle] then begin
//*    var LRectangleAlphaColorRec := TalphaColorRec.create(TouchEffectRectangleFillColor);
//*    LRectangleAlphaColorRec.A := 255;
//*    var LTouchEffectBoundsRect := GetTouchEffectBoundsRect;
//*    OwnerScrollBox.TouchEffectRectangleBitmap := TALImageCacheEngine.Instance.RetrieveRectImage(LRectangleAlphaColorRec.Color, LTouchEffectBoundsRect.Width * ALGetScreenScale, LTouchEffectBoundsRect.Height * ALGetScreenScale);
//*    OwnerScrollBox.OwnTouchEffectRectangleBitmap := false;
//*  end
//*  {$ENDREGION}
//*
//*  {$REGION 'ClickFadeOutCircle/LongPressFadeOutCircle'}
//*  else if TouchEffectStyle in [TTouchEffectStyle.ClickFadeOutCircle,
//*                               TTouchEffectStyle.LongPressFadeOutCircle] then begin
//*    var LCircleAlphaColorRec := TalphaColorRec.create(TouchEffectCircleFillColor);
//*    LCircleAlphaColorRec.A := 255;
//*    var LTouchEffectBoundsRect := GetTouchEffectBoundsRect;
//*    OwnerScrollBox.TouchEffectCircleBitmap := TALImageCacheEngine.Instance.RetrieveCircleImage(LCircleAlphaColorRec.Color, LTouchEffectBoundsRect.Width * ALGetScreenScale, LTouchEffectBoundsRect.Height * ALGetScreenScale);
//*    OwnerScrollBox.OwnTouchEffectCircleBitmap := false;
//*  end
//*  {$ENDREGION}
//*
//*  {$REGION 'Unknown'}
//*  else if not (TouchEffectStyle in [TTouchEffectStyle.None,
//*                                    TTouchEffectStyle.Custom]) then begin
//*    raise Exception.Create('Error E2D7684E-C434-4BD3-86B2-1821982FAFCD');
//*  end;
//*  {$ENDREGION}
//*
//*end;
//*
//*{********************************************************************************************************************}
//*procedure TALDynamicScrollBoxControl.DrawTouchEffect(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single);
//*begin
//*  if (OwnerScrollBox.TouchEffectOwner = self) then begin
//*
//*    {$REGION 'ClickFadeOutRectangle/LongPressFadeOutRectangle'}
//*    if TouchEffectStyle in [TTouchEffectStyle.ClickFadeOutRectangle,
//*                            TTouchEffectStyle.LongPressFadeOutRectangle] then begin
//*      var LOpacity: Single;
//*      if OwnerScrollBox.TouchEffectAnim.Enabled then LOpacity := OwnerScrollBox.TouchEffectAnim.CurrentValue * AOpacity
//*      else LOpacity := OwnerScrollBox.TouchEffectAnim.StartValue * AOpacity;
//*      OwnerScrollBox.drawBitmap(
//*        Canvas,
//*        ARect,
//*        OwnerScrollBox.TouchEffectRectangleBitmap, // aBitmap
//*        GetTouchEffectBoundsRect.TopLeft, // aTopLeft
//*        LOpacity); // const AOpacity: Single;
//*    end
//*    {$ENDREGION}
//*
//*    {$REGION 'ClickFadeOutCircle/LongPressFadeOutCircle'}
//*    else if TouchEffectStyle in [TTouchEffectStyle.ClickFadeOutCircle,
//*                                 TTouchEffectStyle.LongPressFadeOutCircle] then begin
//*      var LOpacity: Single;
//*      if OwnerScrollBox.TouchEffectAnim.Enabled then LOpacity := OwnerScrollBox.TouchEffectAnim.CurrentValue * AOpacity
//*      else LOpacity := OwnerScrollBox.TouchEffectAnim.StartValue * AOpacity;
//*      OwnerScrollBox.drawBitmap(
//*        Canvas,
//*        ARect,
//*        OwnerScrollBox.TouchEffectCircleBitmap, // aBitmap
//*        GetTouchEffectBoundsRect.TopLeft, // aTopLeft
//*        LOpacity); // const AOpacity: Single;
//*    end
//*    {$ENDREGION}
//*
//*    {$REGION 'ClickFadeOutRectangleAndCircle'}
//*    else if TouchEffectStyle = TTouchEffectStyle.ClickFadeOutRectangleAndCircle then begin
//*      OwnerScrollBox.drawBitmap(
//*        Canvas,
//*        ARect,
//*        OwnerScrollBox.TouchEffectRectangleBitmap, // aBitmap
//*        GetTouchEffectBoundsRect.TopLeft, // aTopLeft
//*        AOpacity); // const AOpacity: Single;
//*      var LCircleOpacity: Single;
//*      var LCircleRect: TRectF;
//*      if OwnerScrollBox.TouchEffectAnim.Tag = 0 then begin
//*        LCircleRect.Size := TsizeF.Create(OwnerScrollBox.TouchEffectAnim.CurrentValue, OwnerScrollBox.TouchEffectAnim.CurrentValue);
//*        LCircleOpacity := OwnerScrollBox.TouchEffectAnim.tagFloat * AOpacity;
//*      end
//*      else begin
//*        LCircleRect.Size := TsizeF.Create(OwnerScrollBox.TouchEffectAnim.tagFloat, OwnerScrollBox.TouchEffectAnim.tagFloat);
//*        LCircleOpacity := OwnerScrollBox.TouchEffectAnim.CurrentValue * AOpacity;
//*      end;
//*      LCircleRect.SetLocation(
//*        OwnerScrollBox.TouchEffectMousePos.x - (LCircleRect.Width / 2),
//*        OwnerScrollBox.TouchEffectMousePos.y - (LCircleRect.Height / 2));
//*      OwnerScrollBox.drawBitmap(
//*        Canvas,
//*        ARect,
//*        OwnerScrollBox.TouchEffectCircleBitmap, // aBitmap
//*        LCircleRect, // aTopLeft
//*        LCircleOpacity, // const AOpacity: Single;
//*        true); // const aClipRect: Boolean = False
//*    end;
//*    {$ENDREGION}
//*
//*  end;
//*end;

{*******************************************************************************}
procedure TALDynamicScrollBoxControl.SetOnMouseEnter(const AValue: TNotifyEvent);
begin
  HitTest := True;
  FOnMouseEnter := AValue;
end;

{*******************************************************************************}
procedure TALDynamicScrollBoxControl.SetOnMouseLeave(const AValue: TNotifyEvent);
begin
  HitTest := True;
  FOnMouseLeave := AValue;
end;

{**********************************************************************************}
//*procedure TALDynamicScrollBoxControl.SetOnMouseDown(const AValue: TALMouseEvent);
//*begin
//*  HitTest := True;
//*  FOnMouseDown := AValue;
//*end;

{********************************************************************************}
//*procedure TALDynamicScrollBoxControl.SetOnMouseUp(const AValue: TALMouseEvent);
//*begin
//*  HitTest := True;
//*  FOnMouseUp := AValue;
//*end;

{******************************************************************************}
//*procedure TALDynamicScrollBoxControl.SetOnClick(const AValue: TALMouseEvent);
//*begin
//*  HitTest := True;
//*  Cursor := crhandpoint;
//*  FOnClick := AValue;
//*end;

{*********************************************************************************}
//*procedure TALDynamicScrollBoxControl.SetOnDblClick(const AValue: TALMouseEvent);
//*begin
//*  HitTest := True;
//*  Cursor := crhandpoint;
//*  FOnDblClick := AValue;
//*end;

{**********************************************************************************}
//*procedure TALDynamicScrollBoxControl.SetOnMouseMove(const AValue: TALMouseEvent);
//*begin
//*  HitTest := True;
//*  FOnMouseMove := AValue;
//*end;

{**********************************************************************************}
//*procedure TALDynamicScrollBoxControl.SetOnLongPress(const AValue: TALMouseEvent);
//*begin
//*  HitTest := True;
//*  Cursor := crhandpoint;
//*  FOnLongPress := AValue;
//*end;

{*********************************************************************}
procedure TALDynamicScrollBoxControl.MouseEnter(const Sender: TObject);
begin
  OwnerScrollBox.cursor := Cursor;
  if Assigned(fOnMouseEnter) then
    fOnMouseEnter(Sender);
end;

{*********************************************************************}
procedure TALDynamicScrollBoxControl.MouseLeave(const Sender: TObject);
begin
  IsPressed := False;
  if Assigned(fOnMouseLeave) then
    fOnMouseLeave(Sender);
end;

{***********************************************************************************************************************************}
procedure TALDynamicScrollBoxControl.MouseDown(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the control
begin
  IsPressed := True;
//*  if Assigned(fOnMouseDown) then
//*    fOnMouseDown(Sender, AMousePos);
end;

{*********************************************************************************************************************************}
procedure TALDynamicScrollBoxControl.MouseUp(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the control
begin
  IsPressed := False;
//*  if Assigned(fOnMouseUp) then
//*    fOnMouseUp(Sender, AMousePos);
end;

{*******************************************************************************************************************************}
procedure TALDynamicScrollBoxControl.Click(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the control
begin
//*  if Assigned(fOnClick) then
//*    fOnClick(Sender, AMousePos);
end;

{**********************************************************************************************************************************}
procedure TALDynamicScrollBoxControl.DblClick(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the control
begin
//*  if Assigned(fOnDblClick) then
//*    fOnDblClick(Sender, AMousePos);
end;

{***********************************************************************************************************************************}
procedure TALDynamicScrollBoxControl.MouseMove(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the control
begin
  OwnerScrollBox.cursor := Cursor;
//*  if Assigned(fOnMouseMove) then
//*    fOnMouseMove(Sender, AMousePos);
end;

{***********************************************************************************************************************************}
procedure TALDynamicScrollBoxControl.LongPress(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the control
begin
//*  if Assigned(fOnLongPress) then
//*    fOnLongPress(Sender, AMousePos);
end;

{*******************************************************************************************}
//*constructor TALDynamicScrollBoxRectangle.Create(const AOwner: TALDynamicScrollBoxControl);
//*begin
//*  inherited Create(AOwner);
//*  fBufBitmap := ALNullDrawable;
//*  fBufBitmapRect := TRectF.Empty;
//*  FillColor := TAlphaColorRec.Null; { in TALRectangle: $FFE0E0E0 }
//*  StrokeColor := TAlphaColorRec.Null; { in TALRectangle: $FF000000 }
//*  StrokeThickness := 1;
//*  Sides := AllSides;
//*  Corners := AllCorners;
//*  XRadius := 0;
//*  YRadius := 0;
//*  ShadowColor := TAlphaColorRec.Null; { in TALRectangle: $96000000 }
//*  shadowBlur := 12;
//*  shadowOffsetX := 0;
//*  shadowOffsetY := 0;
//*end;

{*************************************************}
//*destructor TALDynamicScrollBoxRectangle.Destroy;
//*begin
//*  ALFreeAndNilDrawable(fBufBitmap);
//*  inherited Destroy;
//*end;

{*******************************************************}
//*procedure TALDynamicScrollBoxRectangle.ClearBufBitmap;
//*begin
//*  inherited ClearBufBitmap;
//*  {$IF defined(DEBUG)}
//*  if (not FIsDestroying) and (Not ALIsDrawableNull(fBufBitmap)) then
//*    ALLog(Self.className + '.ClearBufBitmap', 'BufBitmap will likely be recreated', TalLogType.warn);
//*  {$ENDIF}
//*  ALFreeAndNilDrawable(fBufBitmap);
//*end;

{******************************************************************}
//*function TALDynamicScrollBoxRectangle.MakeBufBitmap: TALDrawable;
//*begin
//*
//*  Result := inherited MakeBufBitmap;
//*  if Not ALIsDrawableNull(result) then exit;
//*
//*  if (Not ALIsDrawableNull(fBufBitmap)) or
//*     //--- Don't do bufbitmap if size=0
//*     (SameValue(Width, 0, TEpsilon.position)) or
//*     (SameValue(Height, 0, TEpsilon.position)) or
//*     //--- Don't do bufbitmap if only fill with solid color
//*     (((StrokeColor = TalphaColorRec.Null) or
//*       (sides = []))
//*      and
//*      ((SameValue(xRadius, 0, TEpsilon.Vector)) or
//*       (SameValue(yRadius, 0, TEpsilon.Vector)) or
//*       (corners=[]))
//*      and
//*      (ShadowColor = TalphaColorRec.Null)) then
//*    exit(fBufBitmap);
//*
//*  //init fBufBitmapRect / LRect
//*  var LScreenScale := ALGetScreenScale;
//*  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, LScreenScale); // to have the pixel aligned width and height
//*  var LRect := TrectF.Create(0,0,round(fBufBitmapRect.Width * LScreenScale), round(fBufBitmapRect.height * LScreenScale));
//*  if ShadowColor <> TalphaColorRec.Null then begin
//*    fBufBitmapRect.Inflate(Shadowblur, Shadowblur); // add the extra space needed to draw the shadow
//*    fBufBitmapRect := ALAlignDimensionToPixelRound(fBufBitmapRect, LScreenScale); // to have the pixel aligned width and height
//*    LRect.Offset(Shadowblur * LScreenScale, Shadowblur * LScreenScale);
//*  end;
//*
//*  {$IFDEF ANDROID}
//*
//*  //create the drawing surface
//*  var LBitmap: Jbitmap;
//*  var LCanvas: Jcanvas;
//*  ALCreateDrawingSurface(
//*    LBitmap, // Var aBitmap: Jbitmap;
//*    LCanvas, // var aCanvas: Jcanvas;
//*    round(fBufBitmapRect.Width * LScreenScale), // const w: integer;
//*    round(fBufBitmapRect.height * LScreenScale));// const h: integer)
//*  try
//*
//*     ALPaintRectangle(
//*       LCanvas, // const aBitmap: Jbitmap;
//*       LRect, // const Rect: TrectF;
//*       FillColor, // const FillColor: TAlphaColor;
//*       StrokeColor, // const StrokeColor: TalphaColor;
//*       StrokeThickness * LScreenScale, // const StrokeThickness: Single;
//*       ShadowColor, // const ShadowColor: TAlphaColor;
//*       shadowBlur * LScreenScale, //   const shadowBlur: Single;
//*       shadowOffsetX * LScreenScale, // const shadowOffsetX: Single;
//*       shadowOffsetY * LScreenScale, // const shadowOffsetY: Single;
//*       Sides, // const Sides: TSides;
//*       Corners, // const Corners: TCorners;
//*       XRadius * LScreenScale, // const XRadius: Single;
//*       YRadius * LScreenScale); // const YRadius: Single); overload;
//*
//*    fBufBitmap := ALJBitmaptoTexture(LBitmap);
//*
//*  finally
//*    ALFreeAndNilSurface(LBitmap, LCanvas);
//*  end;
//*
//*  {$ELSEIF DEFINED(IOS)}
//*
//*  //create the drawing surface
//*  var LBitmapSurface: TbitmapSurface;
//*  var LColorSpace: CGColorSpaceRef;
//*  var LContext: CGContextRef;
//*  ALCreateDrawingSurface(
//*    LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
//*    LContext, //    Var aContext: CGContextRef;
//*    LColorSpace, // Var aColorSpace: CGColorSpaceRef;
//*    round(fBufBitmapRect.Width * LScreenScale), // const w: integer;
//*    round(fBufBitmapRect.height * LScreenScale));// const h: integer)
//*  try
//*
//*     ALPaintRectangle(
//*       LContext, // const aContext: CGContextRef;
//*       LColorSpace, // const aColorSpace: CGColorSpaceRef;
//*       LBitmapSurface.Height, // const aGridHeight: Single;
//*       LRect, // const Rect: TrectF;
//*       FillColor, // const FillColor: TAlphaColor;
//*       StrokeColor, // const StrokeColor: TalphaColor;
//*       StrokeThickness * LScreenScale, // const StrokeThickness: Single;
//*       ShadowColor, // const ShadowColor: TAlphaColor;
//*       shadowBlur * LScreenScale, //   const shadowBlur: Single;
//*       shadowOffsetX * LScreenScale, // const shadowOffsetX: Single;
//*       shadowOffsetY * LScreenScale, // const shadowOffsetY: Single;
//*       Sides, // const Sides: TSides;
//*       Corners, // const Corners: TCorners;
//*       XRadius * LScreenScale, // const XRadius: Single;
//*       YRadius * LScreenScale); // const YRadius: Single); overload;
//*
//*    fBufBitmap := ALBitmapSurfacetoTexture(LBitmapSurface);
//*
//*  finally
//*    ALFreeAndNilSurface(
//*      LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
//*      LContext, // Var aContext: CGContextRef;
//*      LColorSpace); // Var aColorSpace: CGColorSpaceRef;
//*  end;
//*
//*  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
//*
//*  //create the drawing surface
//*  ALCreateSurface(
//*    fBufBitmap, // Var aBitmap: Jbitmap;
//*    true, // const aClearBitmap: boolean;
//*    round(fBufBitmapRect.Width * LScreenScale), // const w: integer;
//*    round(fBufBitmapRect.height * LScreenScale));// const h: integer)
//*  try
//*
//*    //begin the scene
//*    if fBufBitmap.Canvas.BeginScene then
//*    try
//*
//*      ALDrawRectangle(
//*        fBufBitmap.Canvas, // const aBitmap: Jbitmap;
//*        LRect, // const Rect: TrectF;
//*        FillColor, // const FillColor: TAlphaColor;
//*        StrokeColor, // const StrokeColor: TalphaColor;
//*        StrokeThickness * LScreenScale, // const StrokeThickness: Single;
//*        ShadowColor, // const ShadowColor: TAlphaColor;
//*        shadowBlur * LScreenScale, //   const shadowBlur: Single;
//*        shadowOffsetX * LScreenScale, // const shadowOffsetX: Single;
//*        shadowOffsetY * LScreenScale, // const shadowOffsetY: Single;
//*        Sides, // const Sides: TSides;
//*        Corners, // const Corners: TCorners;
//*        XRadius * LScreenScale, // const XRadius: Single;
//*        YRadius * LScreenScale); // const YRadius: Single); overload;
//*
//*    finally
//*      fBufBitmap.Canvas.EndScene;
//*    end;
//*
//*  Except
//*    ALFreeAndNilSurface(fBufBitmap);
//*    raise;
//*  end;
//*
//*  {$ENDIF}
//*
//*  //set the result
//*  result := fBufBitmap;
//*
//*end;

{************************************************************************************************************}
//*procedure TALDynamicScrollBoxRectangle.Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single);
//*begin
//*
//*  if ALIsDrawableNull(fBufBitmap) then begin
//*    Canvas.Fill.Kind := TbrushKind.Solid;
//*    Canvas.Fill.Color := FillColor;
//*    var LDestRect := canvas.AlignToPixel(LocalRect);
//*    canvas.FillRect(LDestRect, aOpacity);
//*  end
//*  else begin
//*    OwnerScrollBox.drawBitmap(
//*      Canvas,
//*      ARect,
//*      FBufBitmap, // aBitmap
//*      fBufBitmapRect.TopLeft, // aTopLeft
//*      aOpacity); // aOpacity
//*  end;
//*
//*  inherited Paint(Canvas, ARect, aOpacity);
//*
//*end;

{***********************************************************************************}
constructor TALDynamicScrollBoxText.Create(const AOwner: TALDynamicScrollBoxControl);
begin
  inherited create(AOwner);
  AutoSize := true;
  fBufBitmap := ALNullDrawable;
  fBufBitmapRect := TRectF.Empty;
  fBufTextBroken := False;
  fBufAllTextDrawn := True;
  Text := '';
  TextIsHtml := False;
//*  FontName := ALGetFontFamily('sans-serif');
  FontStyle := [];
  FontSize := 19;
  FontColor := $FF333844;
  LineSpacing := 0;
  Trimming := TTextTrimming.Character;
  HorzTextAlign := TTextAlign.Leading;
  VertTextAlign := TTextAlign.Center;
  WordWrap := false;
  FillColor := TalphaColorRec.Null;
  StrokeColor := TalphaColorRec.Null;
  StrokeThickness := 1;
  Sides := AllSides;
  Corners := AllCorners;
  XRadius := 0;
  YRadius := 0;
  MaxWidth := 65535;
  MaxHeight := 65535;
end;

{*****************************************}
destructor TALDynamicScrollBoxText.Destroy;
begin
  ALFreeAndNilDrawable(fBufBitmap);
  inherited Destroy;
end;

{***********************************************}
procedure TALDynamicScrollBoxText.ClearBufBitmap;
begin
  inherited ClearBufBitmap;
  {$IF defined(DEBUG)}
  if (not FIsDestroying) and (Not ALIsDrawableNull(fBufBitmap)) then
    ALLog(Self.className + '.ClearBufBitmap', 'BufBitmap will likely be recreated', TalLogType.warn);
  {$ENDIF}
  ALFreeAndNilDrawable(fBufBitmap);
end;

{**********************************************************}
function TALDynamicScrollBoxText.MakeBufBitmap: TALDrawable;
begin

//*  Result := inherited MakeBufBitmap;
//*  if (Not ALIsDrawableNull(result)) then exit;
//*
//*  if (Not ALIsDrawableNull(fBufBitmap)) or
//*     //--- Don't do bufbitmap if text is empty
//*     (text.IsEmpty) then
//*    exit(fBufBitmap);
//*
//*  var LScreenScale := ALGetScreenScale;
//*  if Align in [TAlign.Top,
//*               TALAlignLayout.Bottom,
//*               TALAlignLayout.VertCenter,
//*               TALAlignLayout.Horizontal] then fBufBitmapRect := TRectF.Create(0, 0, Width*LScreenScale, maxHeight*LScreenScale)
//*  else if Align in [TAlign.Left,
//*                    TALAlignLayout.Right,
//*                    TALAlignLayout.HorzCenter,
//*                    TALAlignLayout.Vertical] then fBufBitmapRect := TRectF.Create(0, 0, maxWidth*LScreenScale, Height*LScreenScale)
//*  else if Align in [TAlign.Client] then fBufBitmapRect := TRectF.Create(0, 0, Width*LScreenScale, Height*LScreenScale)
//*  else begin
//*    {$IFDEF debug}
//*    if not (Align in [TAlign.None, TALAlignLayout.Center, TALAlignLayout.TopCenter, TALAlignLayout.LeftCenter, TALAlignLayout.RightCenter, TALAlignLayout.BottomCenter]) then
//*      raise Exception.Create('Error E949A33C-1412-42CD-9782-ADA1BCDEF0C1');
//*    {$endif}
//*    fBufBitmapRect := TRectF.Create(0, 0, maxWidth*LScreenScale, maxHeight*LScreenScale);
//*  end;
//*
//*  var LOptions := TALMultiLineTextOptions.Create;
//*  Try
//*
//*    LOptions.FontName := FontName;
//*    LOptions.FontSize := FontSize * LScreenScale;
//*    LOptions.FontStyle := FontStyle;
//*    LOptions.FontColor := FontColor;
//*    //-----
//*    //LOptions.EllipsisText: String; // default = '…';
//*    //LOptions.EllipsisFontStyle: TFontStyles; // default = [];
//*    //LOptions.EllipsisFontColor: TalphaColor; // default = TAlphaColorRec.Null;
//*    //-----
//*    if ((FillColor <> TalphaColorRec.Null) or            // if we don't draw any background then
//*        (StrokeColor <> TalphaColorRec.Null)) then begin // always set autosize = true
//*      if (not Autosize) then LOptions.AutoSize := false // if we ask autosize = false then autosize to false
//*      else if (Align in [TAlign.Top,
//*                         TALAlignLayout.Bottom,
//*                         TALAlignLayout.Horizontal]) then begin
//*          LOptions.AutoSize := false;   // if we ask autosize = true and Width is aligned
//*          LOptions.AutoSizeY := True;   // then autosize only the Y
//*      end
//*      else if (Align in [TAlign.Left,
//*                         TALAlignLayout.Right,
//*                         TALAlignLayout.Vertical]) then begin
//*        LOptions.AutoSize := false; // if we ask autosize = true and Height is aligned
//*        LOptions.AutoSizeX := True; // then autosize only the X
//*      end
//*      else if (Align in [TAlign.Client]) then LOptions.AutoSize := false  // if we ask autosize = true and Width & Height are aligned then don't autosize anything
//*      else begin
//*        {$IFDEF debug}
//*        if not (Align in [TAlign.None, TALAlignLayout.Center]) then
//*          raise Exception.Create('Error E949A33C-1412-42CD-9782-ADA1BCDEF0C1');
//*        {$endif}
//*        LOptions.AutoSize := True; // // if we ask autosize = true and Width & Height are not aligned then autosize to true
//*      end;
//*    end
//*    else LOptions.AutoSize := True;
//*    //-----
//*    LOptions.WordWrap := WordWrap;
//*    //LOptions.MaxLines: integer; // default = 0;
//*    LOptions.LineSpacing := LineSpacing * LScreenScale;
//*    LOptions.Trimming := Trimming;
//*    //LOptions.FailIfTextBroken: boolean; // default = false
//*    //-----
//*    LOptions.HTextAlign := HorzTextAlign;
//*    LOptions.VTextAlign := VertTextAlign;
//*    //-----
//*    if FillColor <> TalphaColorRec.Null then begin
//*      LOptions.Fill.Kind := TBrushKind.Solid;
//*      LOptions.Fill.Color := FillColor;
//*    end;
//*    if StrokeColor <> TalphaColorRec.Null then begin
//*      LOptions.Stroke.Kind := TBrushKind.Solid;
//*      LOptions.Stroke.Color := StrokeColor;
//*      LOptions.Stroke.Thickness := LOptions.Stroke.Thickness * LScreenScale;
//*    end;
//*    LOptions.Sides := Sides;
//*    LOptions.XRadius := XRadius * LScreenScale;
//*    LOptions.YRadius := YRadius * LScreenScale;
//*    LOptions.Corners := Corners;
//*    LOptions.Padding := padding;
//*    LOptions.Padding.Top := Padding.Top * LScreenScale;
//*    LOptions.Padding.right := Padding.right * LScreenScale;
//*    LOptions.Padding.left := Padding.left * LScreenScale;
//*    LOptions.Padding.bottom := Padding.bottom * LScreenScale;
//*    //-----
//*    LOptions.TextIsHtml := TextIsHtml;
//*
//*    // init LPrevBitmapRect
//*    var LPrevBitmapRect := fBufBitmapRect;
//*
//*    // build fBufBitmap
//*    fBufBitmap := ALDrawMultiLineText(
//*                    Text, // const aText: String; // support only basic html tag like <b>...</b>, <i>...</i>, <font color="#ffffff">...</font> and <span id="xxx">...</span>
//*                    fBufBitmapRect, // var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
//*                    fBufTextBroken,
//*                    fBufAllTextDrawn,
//*                    LOptions);
//*
//*    // align fbufBitmapRect
//*    if LOptions.AutoSize and (not Autosize) then begin
//*      case LOptions.HTextAlign of
//*        TTextAlign.Center: begin
//*                             fbufBitmapRect.Offset((LPrevBitmapRect.Width - fbufBitmapRect.width) / 2, 0);
//*                           end;
//*        TTextAlign.Trailing: begin
//*                               fbufBitmapRect.Offset(LPrevBitmapRect.Width - fbufBitmapRect.width, 0);
//*                             end;
//*      end;
//*      case LOptions.VTextAlign of
//*        TTextAlign.Center: begin
//*                             fbufBitmapRect.Offset(0, (LPrevBitmapRect.Height - fbufBitmapRect.Height) / 2);
//*                           end;
//*        TTextAlign.Trailing: begin
//*                               fbufBitmapRect.Offset(0, LPrevBitmapRect.Height - fbufBitmapRect.Height);
//*                             end;
//*      end;
//*    end;
//*    if Autosize then fBufBitmapRect.Offset(-fBufBitmapRect.left, -fBufBitmapRect.top);
//*
//*    //convert fbufBitmapRect do virtual pixel
//*    fbufBitmapRect.Top := fbufBitmapRect.Top / LScreenScale;
//*    fbufBitmapRect.right := fbufBitmapRect.right / LScreenScale;
//*    fbufBitmapRect.left := fbufBitmapRect.left / LScreenScale;
//*    fbufBitmapRect.bottom := fbufBitmapRect.bottom / LScreenScale;
//*
//*  finally
//*    ALFreeAndNil(LOptions);
//*  end;
//*
//*  //update the result
//*  result := fBufBitmap;

end;

{*******************************************}
procedure TALDynamicScrollBoxText.AdjustSize;
begin
(*
  if AutoSize then begin
    fIsAdjustingSize := True;
    try

      MakeBufBitmap;
      var R := FBufBitmapRect;

      // This to take care of the align constraint
      if Align in [TAlign.Client,
                   TALAlignLayout.Top,
                   TALAlignLayout.Bottom,
                   TALAlignLayout.VertCenter,
                   TALAlignLayout.Horizontal] then begin
        r.Left := 0;
        r.Width := Width;
      end;
      if Align in [TAlign.Client,
                   TALAlignLayout.Left,
                   TALAlignLayout.Right,
                   TALAlignLayout.HorzCenter,
                   TALAlignLayout.Vertical] then begin
        r.Top := 0;
        r.height := height;
      end;

      SetboundsRect(TALRectD.Create(FBoundsRect.TopLeft, R.Width + R.Left * 2, R.Height + R.Top * 2));

    finally
      fIsAdjustingSize := False;
    end;
  end;
*)
end;

{****************************************************************************************************}
procedure TALDynamicScrollBoxText.Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single);
begin

  var LLocation: TPointF;
  if Autosize then begin
    var LDesignatedArea := localrect;
    case HorzTextAlign of
      TTextAlign.Center: LLocation.X := (LDesignatedArea.Left + LDesignatedArea.Right - fBufBitmapRect.Width) / 2;
      TTextAlign.Leading: LLocation.X := LDesignatedArea.Left;
      TTextAlign.Trailing: LLocation.X := LDesignatedArea.Right - fBufBitmapRect.Width;
      else raise Exception.Create('Error 66B0A84D-3E1E-47EE-9A04-5D122C8434F1');
    end;
    case VertTextAlign of
      TTextAlign.Center: LLocation.Y := (LDesignatedArea.Top + LDesignatedArea.Bottom - fBufBitmapRect.Height) / 2;
      TTextAlign.Leading: LLocation.Y := LDesignatedArea.Top;
      TTextAlign.Trailing: LLocation.Y := LDesignatedArea.Bottom - fBufBitmapRect.Height;
      else raise Exception.Create('Error 9B1AD602-155E-4531-9598-877138C5E86E');
    end;
  end
  else LLocation := fBufBitmapRect.TopLeft;

//*  OwnerScrollBox.drawBitmap(
//*    Canvas,
//*    ARect,
//*    FBufBitmap, // aBitmap
//*    LLocation, // aTopLeft
//*    aOpacity); // aOpacity

  inherited Paint(Canvas, ARect, aOpacity);

end;

{************************************************************************************}
constructor TALDynamicScrollBoxImage.Create(const AOwner: TALDynamicScrollBoxControl);
begin
  inherited create(AOwner);
  FBitmap := ALNullDrawable;
  FOwnBitmap := True;
  FBitmapRect := TrectF.Empty;
  FBitmapStatus := TALDownloadingStatus.None;
  FBitmapStatusIcon := ALNullDrawable;
  FOwnBitmapStatusIcon := True;
  FBitmapStatusIconRect := TRectF.Empty;
  BitmapStatusIconSize := TSizeF.Create(16, 16);
  BitmapURL := '';
  BitmapResourceName := '';
  BitmapSize := TSizeF.Create(0,0);
  BitmapBlurred := -1;
  BitmapCropCenter := TpointF.Create(-50,-50);
  HorzBitmapAlign := THorzBitmapAlign.Center;
  VertBitmapAlign := TVertBitmapAlign.Center;
  BitmapCornerRadius := TPointF.Zero;
  BitmapWrapMode := TBitmapWrapMode.FitInto;
  AlwaysDrawBitmap := False;
  GetBitmapFunc := nil;
  DrawBitmapFunc := nil;
end;

{******************************************}
destructor TALDynamicScrollBoxImage.Destroy;
begin
  SetBitmap(ALNullDrawable);
  inherited Destroy;
end;

{**************************************************************************************************************}
procedure TALDynamicScrollBoxImage.SetBitmapUrl(const aMediaNode: TALJSONNodeW; const aCropped: Boolean = true);
begin
  var LBitmapSize := BitmapSize;
  if LBitmapSize.IsZero then LBitmapSize := TSizeF.Create(Width,Height);
//*  BitmapURL := ALGetPictureUrl(
//*                 aMediaNode, // const aPicNode: TALJSONNodeW;
//*                 LBitmapSize.Width, // const aMinWidth: single; // virtual pixel
//*                 LBitmapSize.Height, // const aMinHeight: single; // virtual pixel
//*                 ALGetScreenScale, // const aScreenScale: single;
//*                 aCropped, // const aCropped: Boolean; // return if possible the url of the cropped image
//*                 BitmapBlurred, // var   aPicBlurred: single;
//*                 BitmapCropCenter); // Var   aPicCropCenter: TpointF
end;

{**********************************************************************}
procedure TALDynamicScrollBoxImage.SetBitmap(const AValue: TALDrawable);
begin
//*  if FOwnBitmap then ALFreeAndNilDrawable(FBitmap);
//*  FBitmap := AValue;
//*  FOwnBitmap := True;
//*  if FBitmap <> nil then begin
//*    FBitmapRect := TRectF.Create(
//*                       0, 0,
//*                       Bitmap.Width/ALGetScreenScale,
//*                       Bitmap.Height/ALGetScreenScale);
//*    FBitmapRect := FBitmapRect.CenterAt(LocalRect);
//*    var dx: Single;
//*    var dy: Single;
//*    case HorzBitmapAlign of
//*      THorzBitmapAlign.Center: dx := 0;
//*      THorzBitmapAlign.Left: dx := -FBitmapRect.Left + Padding.Left;
//*      THorzBitmapAlign.Right: dx := FboundsRect.Width - FBitmapRect.Right - Padding.Right;
//*      else
//*        raise Exception.Create('Error 27587919-9460-48D7-8083-A618A4657EEC');
//*    end;
//*    case VertBitmapAlign of
//*      TVertBitmapAlign.Center: dy := 0;
//*      TVertBitmapAlign.Top: dy := -FBitmapRect.Top + Padding.top;
//*      TVertBitmapAlign.Bottom: dy := FboundsRect.Height - FBitmapRect.Bottom - Padding.bottom;
//*      else
//*        raise Exception.Create('Error 65007F13-10BE-4FA8-BFB0-51D7E24EB2E8');
//*    end;
//*    FBitmapRect.Offset(dx,dy);
//*  end;
//*
end;

{********************************************************************************}
procedure TALDynamicScrollBoxImage.SetBitmapStatusIcon(const AValue: TALDrawable);
begin
//*  if FOwnBitmapStatusIcon then ALFreeAndNil(FBitmapStatusIcon);
//*  FBitmapStatusIcon := AValue;
//*  FOwnBitmapStatusIcon := True;
//*  if FBitmapStatusIcon <> nil then begin
//*    FBitmapStatusIconRect := TRectF.Create(
//*                               0, 0,
//*                               FBitmapStatusIcon.Width/ALGetScreenScale,
//*                               FBitmapStatusIcon.Height/ALGetScreenScale);
//*    case BitmapWrapMode of
//*      TBitmapWrapMode.FitIntoAndCrop:
//*        FBitmapStatusIconRect.SetLocation(
//*                                FBitmapRect.Width - FBitmapStatusIconRect.Width - (FBitmapStatusIconRect.Width / 3),
//*                                FBitmapStatusIconRect.height / 3);
//*      TBitmapWrapMode.FitIntoAndCropAsCircle:
//*        // https://fr.wikipedia.org/wiki/Cercle_trigonom%C3%A9trique#/media/File:Unit_circle_angles_color.svg
//*        FBitmapStatusIconRect.SetLocation(
//*                                ((FBitmapRect.Width / 2) + ((sqrt(2) / 2) * (FBitmapRect.Width / 2))) - (FBitmapStatusIconRect.Width / 2),
//*                                ((FBitmapRect.Height / 2) - ((sqrt(2) / 2) * (FBitmapRect.Height / 2))) - (FBitmapStatusIconRect.Height / 2));
//*
//*      else raise Exception.Create('Error EA9F8DA2-E027-45E1-92E6-2B1AC9C7016A');
//*    end;
//*  end;
//*
end;

{*******************************************************}
function TALDynamicScrollBoxImage.GetBitmap: TALDrawable;
begin
  if assigned(GetBitmapFunc) then result:= GetBitmapFunc(Self)
  else result:= FBitmap;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxImage.MakeBufBitmap: TALDrawable;
begin
  result := inherited MakeBufBitmap;
  DownloadMaterials;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxImage.SetBitmapStatus(const AValue: TALDownloadingStatus);
begin
//*  if AValue <> FBitmapStatus then begin
//*    FBitmapStatus := AValue;
//*    case FBitmapStatus of
//*      //--
//*      TALDownloadingStatus.None,
//*      TALDownloadingStatus.Processed: begin
//*        BitmapStatusIcon := nil;
//*      end;
//*      //--
//*      TALDownloadingStatus.Downloading,
//*      TALDownloadingStatus.Processing: begin
//*        BitmapStatusIcon := nil;
//*        //BitmapStatusIcon := TALImageCacheEngine.Instance.RetrieveFitIntoResourceImage('downloading_64x64', BitmapStatusIconSize.Width * ALGetScreenScale, BitmapStatusIconSize.Height * ALGetScreenScale);
//*        //FOwnBitmapStatusIcon := false;
//*      end;
//*      //--
//*      TALDownloadingStatus.Error: begin
//*        BitmapStatusIcon := TALImageCacheEngine.Instance.RetrieveFitIntoResourceImage('error_64x64', BitmapStatusIconSize.Width * ALGetScreenScale, BitmapStatusIconSize.Height * ALGetScreenScale);
//*        FOwnBitmapStatusIcon := false;
//*      end;
//*      //--
//*      else
//*        raise Exception.Create('Error 01F9F9CA-7C0F-4CD2-9ACA-7E3B6E59CA74');
//*    end;
//*  end;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxImage.DoDownloadMaterials;
begin
  inherited DoDownloadMaterials;
  if (BitmapStatus <> TALDownloadingStatus.None) then exit;
  if (BitmapURL <> '') then begin
    // Init LBitmapSize
    var LBitmapSize := BitmapSize;
    if LBitmapSize.IsZero then LBitmapSize := TSizeF.Create(Width,Height);
    // Init LDrawBitmapFunc
    var LDrawBitmapFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc := DrawBitmapFunc;
    if not assigned(LDrawBitmapFunc) then begin
      case BitmapWrapMode of
        TbitmapWrapMode.FitIntoAndCrop: LDrawBitmapFunc := TALDynamicScrollBox.FitIntoAndCropImage;
        TbitmapWrapMode.FitIntoAndCropAsCircle: LDrawBitmapFunc := TALDynamicScrollBox.FitIntoAndCropAsCircleImage;
        else raise Exception.Create('Error 2B924B0E-D644-487D-9B0C-768D520B6822');
      end;
    end;
    // Set the downloading background image
    var LBGColor: TalphaColor;
    {$if defined(ALOverflowCheckingON)}
      {$OverFlowChecks Off}
    {$endif}
    // Taken from Delphi Random implementation
    case ((UInt64(UInt32(4)) * UInt64(UInt32(self) * $08088405 + 1)) shr 32) of
      0: LBGColor := $ffe9f1ff;
      1: LBGColor := $ff91b6fc;
      2: LBGColor := $ffd4e2fd;
      3: LBGColor := $ffbdd3fe;
      else
        raise Exception.Create('Error 0D017B56-72E0-44A3-ACB7-EF8B5F74D136');
    end;
    {$if defined(ALOverflowCheckingON)}
      {$OverFlowChecks On}
    {$endif}
    //*case BitmapWrapMode of
    //*  TBitmapWrapMode.FitIntoAndCrop: Bitmap := TALImageCacheEngine.Instance.RetrieveRectImage(LBGColor, LBitmapSize.Width * ALGetScreenScale, LBitmapSize.Height * ALGetScreenScale);
    //*  TBitmapWrapMode.FitIntoAndCropAsCircle: Bitmap := TALImageCacheEngine.Instance.RetrieveCircleImage(LBGColor, LBitmapSize.Width * ALGetScreenScale, LBitmapSize.Height * ALGetScreenScale);
    //*  else raise Exception.Create('Error 2B924B0E-D644-487D-9B0C-768D520B6822');
    //*end;
    FOwnBitmap := False;
    // Initiate the download
    Var LTmpBitmapStatus := BitmapStatus;
    DownloadMaterial(
      0, // const AMaterialIdx: Integer;
      LTmpBitmapStatus, // var AMediaStatus: TALDownloadingStatus;
      BitmapURL, // const aMediaURL: String;
      BitmapBlurred, // const AMediaBlurred: single;
      BitmapCropCenter, // const AMediaCropCenter: TpointF;
      LBitmapSize, //  const aDestSize: TSizeF;
      BitmapCornerRadius, // const aCornerRadius: TPointF;
      LDrawBitmapFunc, // const ADrawFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc;
      AlwaysDrawBitmap); // const ADrawEmptyNode: boolean = false);
    BitmapStatus := LTmpBitmapStatus;
  end
  else if (BitmapResourceName <> '') then begin
    var LBitmapSize := BitmapSize;
    if LBitmapSize.IsZero then LBitmapSize := TSizeF.Create(Width,Height);
    //*case BitmapWrapMode of
    //*  TBitmapWrapMode.FitInto: Bitmap := TALImageCacheEngine.Instance.RetrieveFitIntoResourceImage(BitmapResourceName, LBitmapSize.Width * ALGetScreenScale, LBitmapSize.Height * ALGetScreenScale);
    //*  TBitmapWrapMode.Stretch: Bitmap := TALImageCacheEngine.Instance.RetrieveStretchResourceImage(BitmapResourceName, LBitmapSize.Width * ALGetScreenScale, LBitmapSize.Height * ALGetScreenScale);
    //*  TbitmapWrapMode.FitIntoAndCrop: Bitmap := TALImageCacheEngine.Instance.RetrieveFitIntoAndCropResourceImage(BitmapResourceName, LBitmapSize.Width * ALGetScreenScale, LBitmapSize.Height * ALGetScreenScale);
    //*  TbitmapWrapMode.FitIntoAndCropAsCircle: Bitmap := TALImageCacheEngine.Instance.RetrieveFitIntoAndCropResourceAsCircleImage(BitmapResourceName, LBitmapSize.Width * ALGetScreenScale, LBitmapSize.Height * ALGetScreenScale);
    //*  else raise Exception.Create('Error 2B924B0E-D644-487D-9B0C-768D520B6822');
    //*end;
    FOwnBitmap := False;
  end;
end;

{*************************************************}
procedure TALDynamicScrollBoxImage.DoShiftMaterial(
            const aMaterialIdx: Integer;
            const aMaterial: TALDrawable);
begin
  if ALIsDrawableNull(aMaterial) then BitmapStatus := TALDownloadingStatus.Error
  else begin
    Setbitmap(aMaterial);
    BitmapStatus := TALDownloadingStatus.Processed;
  end;
end;

{*****************************************************************************************************}
procedure TALDynamicScrollBoxImage.Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single);
begin

//*  OwnerScrollBox.drawBitmap(
//*    Canvas,
//*    ARect,
//*    Bitmap, // aBitmap
//*    FBitmapRect.TopLeft, // aTopLeft
//*    aOpacity); // aOpacity

//*  OwnerScrollBox.drawBitmap(
//*    Canvas,
//*    ARect,
//*    BitmapStatusIcon, // aBitmap
//*    FBitmapStatusIconRect.TopLeft, // aTopLeft
//*    aOpacity); // aOpacity

  inherited Paint(Canvas, ARect, aOpacity);

end;


{********************************************************************}
//*function TALDynamicScrollBoxImage.GetTouchEffectBoundsRect: TrectF;
//*begin
//*  case TouchEffectBoundsconstraint of
//*    TTouchEffectBoundsConstraint.LocalRect: result := inherited GetTouchEffectBoundsRect;
//*    TTouchEffectBoundsConstraint.ExpandedLocalRect: result := inherited GetTouchEffectBoundsRect;
//*    TTouchEffectBoundsConstraint.BitmapRect: result := FBitmapRect;
//*    else raise Exception.Create('Error F3C13920-416B-49A1-A5D6-FC34B1C5451B');
//*  end;
//*end;

{***********************************************************************************************}
//*constructor TALDynamicScrollBoxViewActionBar.Create(const AOwner: TALDynamicScrollBoxControl);
//*begin
//*  inherited Create(AOwner);

//*  CanCollapse := True;
//*  Height := 56;
//*  Align := TALAlignLayout.Horizontal;
//*  FillColor := TALphaColorRec.White;
//*  Sides := [TSide.Bottom];
//*  StrokeColor := $FFE9E9E9;
//*  ShadowColor := $FFE9E9E9;
//*  ShadowBlur := 1;

//*  PreviousBtn := TALDynamicScrollBoxImage.Create(Self);
//*  PreviousBtn.BitmapResourceName := 'previous_88x64';
//*  PreviousBtn.margins.Left := 16;
//*  PreviousBtn.margins.Right := 16;
//*  PreviousBtn.Width := 22;
//*  PreviousBtn.Height := 16;
//*  PreviousBtn.TouchTargetExpansion := PreviousBtn.margins;
//*  PreviousBtn.TouchTargetExpansion.Top := (Height - PreviousBtn.Height) / 2;
//*  PreviousBtn.TouchTargetExpansion.bottom := PreviousBtn.TouchTargetExpansion.Top;
//*  PreviousBtn.Align := TALAlignLayout.rightCenter;
//*  PreviousBtn.TouchEffectStyle := TTouchEffectStyle.ClickFadeOutCircle;
//*  PreviousBtn.OnClick := PreviousBtnClick;
//*  PreviousBtn.Visible := False;

//*  Title := TALDynamicScrollBoxText.Create(Self);
//*  Title.Align := TALAlignLayout.VertCenter;
//*  Title.Margins.Left := 16;
//*  Title.Margins.right := 16;
//*  Title.FontSize := 19;
//*  Title.FontStyle := [TfontStyle.fsBold];
//*  Title.TextIsHtml := True;

//*  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
//*  RefreshBtn := TALDynamicScrollBoxImage.Create(Self);
//*  RefreshBtn.BitmapResourceName := 'reload_64x64';
//*  RefreshBtn.margins.Left := 16;
//*  RefreshBtn.margins.Right := 16;
//*  RefreshBtn.Width := 16;
//*  RefreshBtn.Height := 16;
//*  RefreshBtn.TouchTargetExpansion := RefreshBtn.margins;
//*  RefreshBtn.TouchTargetExpansion.Top := (Height - RefreshBtn.Height) / 2;
//*  RefreshBtn.TouchTargetExpansion.bottom := RefreshBtn.TouchTargetExpansion.Top;
//*  RefreshBtn.Align := TALAlignLayout.RightCenter;
//*  RefreshBtn.TouchEffectStyle := TTouchEffectStyle.ClickFadeOutCircle;
//*  RefreshBtn.OnClick := RefreshBtnClick;
//*  {$ENDIF}
//*End;

{*************************************************************************************************}
//*procedure TALDynamicScrollBoxViewActionBar.RefreshBtnClick(Sender: TObject; AMousePos: TPointF);
//*begin
//*  OwnerScrollBox.Refresh(false{aShowPopupLoading}, true{aShowWhitePageLoading});
//*end;

{**************************************************************************************************}
//*procedure TALDynamicScrollBoxViewActionBar.PreviousBtnClick(Sender: TObject; AMousePos: TPointF);
//*begin
//*  TALMainForm.Instance.ShowPreviousFrame;
//*end;

{*************}
//[MultiThread]
constructor TALDynamicScrollBoxPreloadEphemeralLayoutExtData.Create(const AComparekey: Integer);
begin
  inherited create;
  FComparekey := AComparekey;
end;

{***********************************************************************************************}
constructor TALDynamicScrollBoxItemEphemeralLayout.Create(const AOwner: TALDynamicScrollBoxItem);
begin
  FIsEphemeralLayout := True;
  inherited create(AOwner.OwnerScrollBox);
  FBoundsRect := TALRectD.Create(0, 0, AOwner.FboundsRect.Width, AOwner.FboundsRect.Height);
  //*Align := TALAlignLayout.Top;
  Autosize := True;
end;

{**********************************************************************************************************}
constructor TALDynamicScrollBoxItem.TGrowDataBackgroundThread.Create(const AOwner: TALDynamicScrollBoxItem);
begin
  inherited Create(True{CreateSuspended});
  FreeOnTerminate := True;
  FOwner := AOwner;
end;

{******************************************************************}
procedure TALDynamicScrollBoxItem.TGrowDataBackgroundThread.Execute;
begin
  {$IFDEF DEBUG}
  TThread.NameThreadForDebugging(Classname);
  {$ENDIF}
  Tmonitor.Enter(FOwner.FGrowDataLock);
  try

    Try
      FOwner.GrowDataBackgroundProc(self);
    Finally
      Atomicdecrement(FOwner.FGrowDataBackgroundThreadsCount);
    End;

    TThread.Synchronize(nil,
      procedure
      begin
        FOwner.GrowDataFinished(self);
      end);

  finally
    Tmonitor.Exit(FOwner.FGrowDataLock);
//*    FOwner.OwnerScrollBox.DecrementWaitForCount;
  end;
end;

{***********************************************************************************}
constructor TALDynamicScrollBoxItem.Create(const AOwner: TALDynamicScrollBoxControl);
begin
  FIsScrollBoxItem := True;
  //-----
  inherited create(AOwner);
  //-----
  //*Align := TALAlignLayout.Top;
  AutoSize := True;
  //-----
  FData := nil;
  FCanGrowData := False;
  FGrowDataExecutedAtLeastOnce := False;
  FGrowDataError := false;
  FShowApiErrorIfGrowDataError := True;
  FGrowDataLock := Tobject.Create;
  FGrowDataBackgroundThreadsCount := 0;
  FEnforceEphemeralLayoutWhenGrowDataFinished := False;
  //-----
  FIsRefreshingEphemeralLayout := false;
  FRefreshingEphemeralLayout := Nil;
  FEphemeralLayout := nil;
  FEphemeralLayoutStatus := TALDownloadingStatus.None;
  //-----
  FItemIndex := -1;
  FKey := 1;
  FVisibleInPaintArea := False;
  FDeleted := False;
  FSelected := False;
  FCanBeSelected := False;
  //
  // 9223372036854775808
  // aaaa000000000000000
  // aaaaBBBBB0000000000
  // aaaaBBBBBccccc00000
  //               ^^^^^ priority of sub items (3rd level views) (max 99999 sub items)
  //          ^^^^^ priority of items (2nd level views) (max 99999 items)
  //     ^^^^^ priority of 1rt level views (max 99999 views)
  // ^^^^ priority of frame (max 9222 frames)
  //
  if OwnerView <> nil then begin
    if OwnerView.OwnerView <> nil then begin
      if OwnerView.OwnerView.OwnerView <> nil then begin
        {$IF defined(DEBUG)}
        if OwnerView.OwnerView.OwnerView.OwnerView <> nil then
          raise Exception.Create('Error 96DCD3F1-F1B4-40EA-A755-52866239A6D4');
        {$ENDIF}
        FPriorityFactor := 1; // 4th level views
      end
      else FPriorityFactor := 100000; // 3rd level views
    end
    else FPriorityFactor := 10000000000; // 2nd level views
  end
  else FPriorityFactor := 0; // 1th level view (MainView)
end;

{*****************************************}
destructor TALDynamicScrollBoxItem.Destroy;
begin
  ALFreeAndNil(FGrowDataLock);
  ALFreeAndNil(FData);
  inherited Destroy;
end;

{**************************************************}
procedure TALDynamicScrollBoxItem.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdatePriority;
end;

{****************************************}
procedure TALDynamicScrollBoxItem.Prepare;
begin
  //EnforceEphemeralLayout will call growData and could
  //be potentially postponed to the end of growdata
  EnforceEphemeralLayout;
end;

{***********************************************}
procedure TALDynamicScrollBoxItem.UpdatePriority;
begin

  // The UpdatePriority will be call again by the
  // SetItemIndex procedure except for the mainview
  // that is the only one to not have an Owner
  if (itemIndex = -1) and (Owner <> nil) then
    exit;

  //
  // 9223372036854775808
  // aaaa000000000000000
  // aaaaBBBBB0000000000
  // aaaaBBBBBccccc00000
  //               ^^^^^ priority of sub items (3rd level views) (max 99999 sub items)
  //          ^^^^^ priority of items (2nd level views) (max 99999 items)
  //     ^^^^^ priority of 1rt level views (max 99999 views)
  // ^^^^ priority of frame (max 9222 frames)
  //

  if OwnerView <> nil then begin
    AtomicExchange(
      FPriority,
      (OwnerView.GetPriority(nil)) + ((ItemIndex mod 99999) * fPriorityFactor));
  end
  else begin
    AtomicExchange(
      FPriority,
      OwnerScrollBox.GetPriority(nil));
  end;

end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.GetPriority(const AExtData: Tobject): Int64;
begin
//*  if VisibleInPaintArea and OwnerScrollBox.IsShown then result := TALGraphicThreadPool.Instance.PriorityStartingPoint
//*  else result := AtomicCmpExchange(FPriority{target}, 0{NewValue}, 0{Comparand});
end;

{********************************************************************************}
function TALDynamicScrollBoxItem.LocalToFrame(const APoint: TALPointD): TALPointD;
begin
  Result := APoint;
  Result.Offset(BoundsRect.TopLeft);
  if ownerView <> nil then result := ownerView.ScrollToFrame(Result);
end;

{*****************************************************************************}
function TALDynamicScrollBoxItem.LocalToFrame(const ARect: TALRectD): TALRectD;
begin
  Result := ARect;
  Result.Offset(BoundsRect.TopLeft);
  if ownerView <> nil then result := ownerView.ScrollToFrame(Result);
end;

{******************************************************************************}
function TALDynamicScrollBoxItem.FrameToLocal(const APoint: TPointF): TALPointD;
begin
  if ownerView <> nil then Result := ownerView.FrameToLocal(APoint)
  else Result := TALPointD.Create(APoint);
  Result.Offset(-BoundsRect.TopLeft);
  if ownerView <> nil then begin
    if ownerView.ItemsOrientation=Torientation.vertical then Result.Offset(0,ownerView.ScrollPos)
    else Result.Offset(ownerView.ScrollPos,0)
  end;
end;

{***************************************************************************}
function TALDynamicScrollBoxItem.FrameToLocal(const ARect: TRectF): TALRectD;
begin
  if ownerView <> nil then Result := ownerView.FrameToLocal(ARect)
  else Result := TALRectD.Create(ARect);
  Result.Offset(-BoundsRect.TopLeft);
  if ownerView <> nil then begin
    if ownerView.ItemsOrientation=Torientation.vertical then Result.Offset(0,ownerView.ScrollPos)
    else Result.Offset(ownerView.ScrollPos,0)
  end;
end;

{********************************************************************************************************************************}
procedure TALDynamicScrollBoxItem.MouseDown(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the control
begin

  {$REGION 'Init Result'}
  inherited MouseDown(Sender, AMousePos);
  {$ENDREGION}

  {$REGION 'Selection'}
(*
  if (CanBeSelected) and
     (abs(TALTouchEventTracking.OnMouseDownVelocity) < TALTouchEventTracking.MinVelocityThreshold) then begin
    OwnerScrollBox.SelectionTimer.TagObject := Self;
    OwnerScrollBox.SelectionTimer.enabled := True;
  end;
*)
  {$ENDREGION}

end;

{****************************************************************************************************************************}
procedure TALDynamicScrollBoxItem.Click(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the control
begin

  {$REGION 'Init Result'}
  inherited Click(Sender, AMousePos);
  {$ENDREGION}

  {$REGION 'Selection'}
(*
  if OwnerScrollBox.SelectedItems.Count > 0 then begin
    OwnerScrollBox.ToggleSelection(self);
    exit(true);
  end;
*)
  {$ENDREGION}

end;

{****************************************************************************************************}
procedure TALDynamicScrollBoxItem.Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single);
begin

  {$REGION 'Check that all EphemeralLayout are already created'}
  {$IF defined(debug)}
  //it's look like that in some rare cases the EphemeralLayout could be nul
  //I don't yet completely understand how it's possible, I see it on the conversation
  //frame when I scroll very fastly
  if EphemeralLayout = nil then
    ALLog(ClassName+'.Paint', 'EphemeralLayout is null', TalLogType.WARN);
  {$ENDIF}
  {$ENDREGION}

  inherited Paint(Canvas, ARect, aOpacity);

end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.GetVisibleInPaintArea: Boolean;
begin
  result := (Visible) and (FVisibleInPaintArea);
  if (result) and (OwnerView <> nil) then
    result := OwnerView.VisibleInPaintArea;
end;

{****************************************************************}
procedure TALDynamicScrollBoxItem.SetBoundsRect(AValue: TALRectD);
begin
  var LChanged := (not sameValue(FBoundsRect.Width, Avalue.Width, TEpsilon.Position)) or
                  (not sameValue(FBoundsRect.height, Avalue.height, TEpsilon.Position));
  inherited SetBoundsRect(aValue);
  if (LChanged) then begin
    if not fIsAdjustingSize then begin
      //even if EphemeralLayout is nil we need to call
      //clearEphemeralLayout to increment the key in case
      //a background thread is building the EphemeralLayout
      clearEphemeralLayout;
      EnforceEphemeralLayout;
    end;
    //--
    if (ownerView <> nil) then
      ownerView.ArrangeItems(
        TALDynamicScrollBoxView.TArrangeItemsCause.ItemResize, // const ACause: TALDynamicScrollBoxView.TArrangeItemsCause);
        ItemIndex, // const AFromIdx: integer;
        ItemIndex); // const AToIdx: integer;
  end
end;

{********************************************************************}
procedure TALDynamicScrollBoxItem.setItemIndex(const aValue: integer);
begin
  if aValue <> FItemIndex then begin
    FItemIndex := aValue;
    UpdatePriority;
  end;
end;

{*****************************************************}
function TALDynamicScrollBoxItem.GetData: TALJSONNodeW;
begin
  if not HasData then raise Exception.Create('GetData cannot return nil');
  result := FData;
end;

{************************************************}
function TALDynamicScrollBoxItem.HasData: boolean;
begin
  result := FData <> nil;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.GrowDataIsRunning: Boolean;
begin
  result := AtomiccmpExchange(FGrowDataBackgroundThreadsCount{Target}, 0{NewValue}, 0{Comparand}) <> 0;
end;

{*************************************************************************************}
function  TALDynamicScrollBoxItem.GrowData(const aForceGrow: Boolean = False): boolean;
begin

  //exit if the last download was in error except if aForceGrow = true
  //(ex: button Reload click)
  if (not aForceGrow) and
     (FGrowDataError) then exit(false);

  //exit if a thread is already doing the job
  if GrowDataIsRunning then exit(true);

  //exit if we don't need any data
  if not FCanGrowData then begin
    FGrowDataExecutedAtLeastOnce := True;
    exit(false);
  end;

  //before going in background thread
  {$IFDEF DEBUG}
  ALLog(
    ClassName+'.GrowData',
    'ForceGrow:' + ALBoolToStrW(aForceGrow),
    TalLogType.verbose);
  {$ENDIF}
  FGrowDataError := False;

  //load the data in other thread to not block the calling thread
//*  if not OwnerScrollBox.IncrementWaitForCount then exit(false);
  AtomicIncrement(FGrowDataBackgroundThreadsCount);
  var LGrowDataBackgroundThread := GrowDataCreateBackgroundThread;
  LGrowDataBackgroundThread.Start;
  result := true;

end;

{*****************************************************************************************************************}
function TALDynamicScrollBoxItem.GrowDataCreateBackgroundThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread;
begin
  result := TGrowDataBackgroundThread.create(self);
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxItem.GrowDataBackgroundProc(const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread);
begin

  {$REGION 'exit if destroying'}
//*  if OwnerScrollBox.isdestroying then exit;
  {$ENDREGION'}

  {$REGION 'Create local objects and lock the thread'}
  var LDataDoc := TALJSONDocumentW.create;
  {$ENDREGION}

  try

    {$REGION 'Download the Data'}
    var LDownloadDataIsSuccessful := GrowDataBackgroundProcDownloadData(
                                       AOwnerThread,
                                       LDataDoc);
    if not LDownloadDataIsSuccessful then
      LDataDoc.ChildNodes.Clear;
    {$ENDREGION}

    {$REGION 'init the Data'}
    GrowDataBackgroundProcInitData(LDataDoc);
    {$ENDREGION}

    {$REGION 'Wait that we can do some jobs in the main thread'}
//*    if OwnerScrollBox.isdestroying then exit;
    while not GrowDataBackgroundProcCanProcessData do
      sleep(250);
    {$ENDREGION}

    {$REGION 'call GrowDataProcessData'}
//*    if OwnerScrollBox.isdestroying then exit;
    TThread.Synchronize(nil,
      procedure
      begin
        GrowDataProcessData(
          AOwnerThread,
          LDownloadDataIsSuccessful,
          LDataDoc);
      end);
    {$ENDREGION}

    {$REGION 'Finish'}
//*    if OwnerScrollBox.isdestroying then exit;
    TThread.Synchronize(nil,
      procedure
      begin
        GrowDataFinish(
          AOwnerThread,
          LDownloadDataIsSuccessful);
      end);
    {$ENDREGION}

  finally

    {$REGION 'Free local objects and unlock the thread'}
    ALfreeAndNil(LDataDoc);
    {$ENDREGION}

  end;

end;

{*************}
//[MultiThread]
function  TALDynamicScrollBoxItem.GrowDataBackgroundProcDownloadData(
            const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread;
            const aResultNode: TALJSONNodeW): boolean;
begin
  result := False;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxItem.GrowDataBackgroundProcInitData(const aDataNode: TALJSONNodeW);
begin
  aDataNode.ChildNodes.SetSorted(true{Value},true{recurse});
  aDataNode.MultiThreadPrepare(true{aOnlyChildList});
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.GrowDataBackgroundProcCanProcessData: boolean;
begin
  //mostly because we don't want to list to be updated when we are in the
  //bottom bound animation
  result := //*(OwnerScrollBox.isdestroying) or
            (TThread.Current.ThreadID = MainThreadID) or
            ((not OwnerScrollBox.HasActiveScrollEngines)); //*and
             //*(not OwnerScrollBox.IsInTransition));
end;

{****************************************************}
procedure TALDynamicScrollBoxItem.GrowDataProcessData(
            const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread;
            const ADataLoadedSuccessfully: Boolean;
            var ADownloadedData: TALJSONNodeW);
begin

  {$REGION 'exit if destroying'}
//*  if OwnerScrollBox.isdestroying then exit;
  {$ENDREGION'}

  {$REGION 'init FGrowDataError/FCanGrowData'}
  FGrowDataError := Not ADataLoadedSuccessfully;
  if FGrowDataError then FCanGrowData := true
  else FCanGrowData := false;
  {$ENDREGION'}

  {$REGION 'update FData'}
//*  OwnerScrollBox.BeginWrite;
  try
    ALfreeAndNil(FData);
    Fdata := ADownloadedData;
    ADownloadedData := nil;
  finally
//*    OwnerScrollBox.EndWrite;
  end;
  {$ENDREGION'}

end;

{***********************************************}
procedure TALDynamicScrollBoxItem.GrowDataFinish(
            const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread;
            const ADataLoadedSuccessfully: Boolean);
begin

  //do nothing if we are destroying the form
//*  if OwnerScrollBox.isdestroying then exit;

  //If we encounter an Error AND if ShowApiErrorIfGrowDataError
  //and (but it's doesn't matter in fact) if it's the very first
  //time we call GrowData then then call ShowApiError
  if (not FGrowDataExecutedAtLeastOnce) and
     (FGrowDataError) and
     (ShowApiErrorIfGrowDataError) then begin
    FGrowDataExecutedAtLeastOnce := True;
//*    OwnerScrollBox.ShowApiError(ADownloadDataReturnCode)
  end

  //else build the Ephemeral Layout
  else begin
    FGrowDataExecutedAtLeastOnce := True;
//*    OwnerScrollBox.RemoveProgressFrame;
    if FGrowDataError then
      OwnerScrollBox.createErrorMessageBannerAnim;
  end;

end;

{************************************************************************************************************************}
procedure TALDynamicScrollBoxItem.GrowDataFinished(const AOwnerThread: TALDynamicScrollBoxItem.TGrowDataBackgroundThread);
begin
  if FEnforceEphemeralLayoutWhenGrowDataFinished then begin
    FEnforceEphemeralLayoutWhenGrowDataFinished := False;
    EnforceEphemeralLayout;
  end;
  OwnerScrollBox.repaint;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.IsGrowDataFinished: Boolean;
begin
  result := ((not FCanGrowData) or (FGrowDataError)) and
            (not GrowDataIsRunning);
end;

{*****************************************************}
procedure TALDynamicScrollBoxItem.clearEphemeralLayout;
begin
  AtomicIncrement(FKey);
  if FEphemeralLayout <> nil then
    ALFreeAndNil(FEphemeralLayout, true{ADelayed}); // must stay ADelayed for RefreshEphemeralLayout
  FEphemeralLayoutStatus := TALDownloadingStatus.None;
end;

{*******************************************************}
procedure TALDynamicScrollBoxItem.RefreshEphemeralLayout;
begin
  if FEphemeralLayout = nil then exit;
  FIsRefreshingEphemeralLayout := True;
  FRefreshingEphemeralLayout := FEphemeralLayout;
  try
    // As we do ALFreeAndNil(FEphemeralLayout, true{ADelayed}); in clearEphemeralLayout
    // FRefreshingEphemeralLayout will stay valid during this loop
    clearEphemeralLayout;
    EnforceEphemeralLayout;
  finally
    FRefreshingEphemeralLayout := nil;
    FIsRefreshingEphemeralLayout := False;
  end;
  OwnerScrollBox.Repaint;
end;

{************************************************************************************************************************}
function TALDynamicScrollBoxItem.EnforceEphemeralLayout(const a4Preload: boolean): TALDynamicScrollBoxItemEphemeralLayout;
begin

  {$REGION 'if we already have a EphemeralLayout simply return it'}
  if (OwnerView <> nil) and
     ((ItemIndex < OwnerView.ScrollFirstPreloadedItemIdx) or
      (ItemIndex > OwnerView.ScrollLastPreloadedItemIdx)) then exit(nil);
  if not Visible then exit(nil);
  if GrowData then begin
    FEnforceEphemeralLayoutWhenGrowDataFinished := True;
    Exit(nil);
  end;
  result := FEphemeralLayout;
  if (result <> nil) then exit;
  {$ENDREGION}

  {$REGION 'LOG'}
  {$IFDEF DEBUG}
  //{-------------
  if (not a4Preload) or (EphemeralLayoutStatus = TALDownloadingStatus.None) then begin
    var LPreloadedItemsStr: String := '';
    var LFirst: integer := -1;
    var LLast: integer := -1;
    var LCount: Integer := 0;
    if OwnerView <> nil then begin
      for var i := 0 to OwnerView.ItemsCount - 1 do begin
        if OwnerView.Items[i].EphemeralLayoutStatus <> TALDownloadingStatus.None then begin
          if LFirst = -1 then LFirst := i;
          LLast := i;
        end
        else begin
          if LFirst <> -1 then begin
            LPreloadedItemsStr := LPreloadedItemsStr + ALIntToStrW(LFirst) + '..' + ALIntToStrW(LLast) + ';';
            LCount := LCount + LLast - LFirst + 1;
          end;
          LFirst := -1;
        end;
      end;
    end;
    if LFirst <> -1 then begin
      LCount := LCount + LLast - LFirst + 1;
      LPreloadedItemsStr := LPreloadedItemsStr + ALIntToStrW(LFirst) + '..' + ALIntToStrW(LLast) + ';';
    end;
    if LPreloadedItemsStr <> '' then begin
      delete(LPreloadedItemsStr, length(LPreloadedItemsStr), 1);
      LPreloadedItemsStr := LPreloadedItemsStr + ' ('+ALIntToStrW(LCount)+')';
    end;
    var LLogtype: TalLogType;
    if a4Preload then LLogtype := TalLogType.verbose
    else LLogtype := TalLogType.INFO;
    ALLog(
      Classname+'.EnforceEphemeralLayout.Build',
      'Index:'+ALIntToStrW(ItemIndex)+' | '+
      'Priority:'+ALIntToStrW(GetPriority(nil))+' | '+
      'GlobalPriorityStartingPoint:'+ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint)+' | '+
      '4Preload:'+ALBoolToStrW(a4Preload) + ' | ' +
      'Preloaded items:'+LPreloadedItemsStr,
      LLogtype);
  end;
  //-------}
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'not a4Preload'}
  if (not a4Preload) then begin

    var LCompareKey := AtomicIncrement(Fkey);
//*    if not OwnerScrollBox.IncrementWaitForCount then exit;
    ShiftEphemeralLayout(
      LCompareKey, // const aCompareKey: Integer;
      BuildEphemeralLayout); // const aEphemeralLayout: Tkiskis_DynamicScrollBoxEphemeralLayout);
    Result := FEphemeralLayout;

  end
  {$ENDREGION}

  {$REGION 'a4Preload'}
  else begin

    if EphemeralLayoutStatus <> TALDownloadingStatus.None then exit;
    EphemeralLayoutStatus := TALDownloadingStatus.Processing;
    var LCompareKey := AtomicIncrement(Fkey);
//*    if not OwnerScrollBox.IncrementWaitForCount then exit;
    TALGraphicThreadPool.Instance.ExecuteProc(
      PreloadEphemeralLayout, // const AProc: TALWorkerThreadProc;
      TALDynamicScrollBoxPreloadEphemeralLayoutExtData.Create(LCompareKey), // const AExtData: Tobject; TALGraphicThreadPool.Instance will own and release the ExtData object
      GetPriority); // const APriority: Int64 = 0;

  end;
  {$ENDREGION}

end;

{**********************************************************************************************}
function TALDynamicScrollBoxItem.EnforceEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout;
begin
  result := EnforceEphemeralLayout(not VisibleInPaintArea{a4preload});
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxItem.PreloadEphemeralLayout(var AExtData: Tobject);
begin
  Try
    var LPreloadEphemeralLayoutExtData := TALDynamicScrollBoxPreloadEphemeralLayoutExtData(AExtData);
    var LCompareKey := LPreloadEphemeralLayoutExtData.Comparekey;
    {$IFDEF DEBUG}
    {-----
    Var LLogKO := not CanPreloadEphemeralLayout(LCompareKey);
    var LLogtype: TalLogType;
    if LLogKO then LLogtype := TalLogType.warn
    else LLogtype := TalLogType.verbose;
    ALLog(
      Classname+'.PreloadEphemeralLayout.'+ALIfThenW(LLogKO, 'KO', 'OK'),
      'Index:'+ALIntToStrW(Index)+' | '+
      'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
      'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
      'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
      LLogtype);
    -----}
    {$ENDIF}
    if CanPreloadEphemeralLayout(LCompareKey) then begin
      var LEphemeralLayout := BuildEphemeralLayout;
      TThread.queue(nil,
        procedure
        begin
          ShiftEphemeralLayout(
            LCompareKey, // const aCompareKey: Integer;
            LEphemeralLayout); // const aEphemeralLayout: Tkiskis_DynamicScrollBoxEphemeralLayout);
        end);
    end
//*    else OwnerScrollBox.DecrementWaitForCount;
  except
    On E: Exception do begin
      ALLog(ALFormatW('%s.PreloadEphemeralLayout', [Classname]), E);
//*      OwnerScrollBox.DecrementWaitForCount;
    end;
  End;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.CanPreloadEphemeralLayout(const aCompareKey: Integer): Boolean;
begin
  Try
    result := DoCanPreloadEphemeralLayout(aCompareKey);
  except
    On E: Exception do begin
      ALLog(ALFormatW('%s.CanPreloadEphemeralLayout', [Classname]), E);
      Result := False;
    end;
  End;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.DoCanPreloadEphemeralLayout(const aCompareKey: Integer): Boolean;
begin
  result := AtomicCmpExchange(FKey{target}, 0{NewValue}, 0{Comparand}) = aCompareKey;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.BuildEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout;
begin
//*  OwnerScrollBox.BeginRead;
  try
    Try
      result := doBuildEphemeralLayout;
      if Result <> nil then result.AlignControls;
    except
      on E: Exception do begin
        ALLog(ALFormatW('%s.BuildEphemeralLayout', [Classname]), E);
        Result := nil;
      end;
    End;
  finally
//*    OwnerScrollBox.EndRead;
  end;
end;

{********************************}
//[MultiThread][BeginRead/EndRead]
function TALDynamicScrollBoxItem.doBuildEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout;
begin
  //Normally we do not need to protect the access to primitive types (like Rect, etc.)
  //because when we change it (like Rect) then we will also increment the Key that will
  //defacto release the EphemeralLayout in ShiftEphemeralLayout
  if Assigned(OwnerScrollBox.OnCreateLayout) then
    Result := OwnerScrollBox.OnCreateLayout(Self)
  else
    result := TALDynamicScrollBoxItemEphemeralLayout.Create(self);
end;

{*****************************************************}
procedure TALDynamicScrollBoxItem.ShiftEphemeralLayout(
            const aCompareKey: Integer;
            const aEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout);
begin
  {$IFDEF DEBUG}
  {-----
  Var LLogKO := AtomicCmpExchange(FKey, 0, 0) <> aCompareKey;
  var LLogtype: TalLogType;
  if LLogKO then LLogtype := TalLogType.warn
  else LLogtype := TalLogType.verbose;
  ALLog(
    Classname+'.ShiftEphemeralLayout.'+ALIfThenW(LLogKO, 'KO', 'OK'),
    'Index:'+ALIntToStrW(Index)+' | '+
    'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
    'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
    'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
    LLogtype);
  -----}
  {$ENDIF}
  Try
    Try
      if AtomicCmpExchange(FKey{target}, 0{NewValue}, 0{Comparand}) <> aCompareKey then ALFreeAndNil(aEphemeralLayout, true{aDelayed})
      else DoShiftEphemeralLayout(aEphemeralLayout);
    except
      On E: Exception do begin
        ALLog(ALFormatW('%s.ShiftEphemeralLayout', [Classname]), E);
        EphemeralLayoutStatus := TALDownloadingStatus.Error;
      end;
    End;
  finally
//*    OwnerScrollBox.DecrementWaitForCount;
  end;
end;

{***********************************************************************************************************************}
procedure TALDynamicScrollBoxItem.DoShiftEphemeralLayout(const aEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout);
begin
  if fEphemeralLayout <> nil then
    raise Exception.Create('Error A7F1D39F-1368-45DC-BDFF-00F223DA08BF');
  fEphemeralLayout := aEphemeralLayout;
  if aEphemeralLayout = nil then EphemeralLayoutStatus := TALDownloadingStatus.Error
  else begin
    fEphemeralLayout.Owner := Self;
    MoveControl(fEphemeralLayout,0);
    EphemeralLayoutStatus := TALDownloadingStatus.Processed;
    AlignControls;
    (*
    we must call this if AlignControls didn't change the boundsRect
    maybe overload the AlignControls for ScrollBoxView
    //
    if isScrollBoxView then
      with TALDynamicScrollBoxView(self) do
        ArrangeItems(
          TALDynamicScrollBoxView.TArrangeItemsCause.Other, // const ACause: TALDynamicScrollBoxView.TArrangeItemsCause;
          0, // const AFromIdx: integer;
          itemsCount - 1); // const AToIdx: integer)
    *)
    //*DownloadMaterials;
  end;
end;

{**************************************************}
procedure TALDynamicScrollBoxItem.DownloadMaterials;
begin
  if FEphemeralLayout = nil then exit;
  inherited DownloadMaterials;
end;








{*************************************************}
procedure TALDynamicScrollBoxItem.DownloadMaterial(
            const AMaterialIdx: Integer;
            var AMediaStatus: TALDownloadingStatus;
            const aMediaNode: TALJSONNodeW;
            const aCropped: Boolean;
            const aDestSize: TSizeF;
            const aCornerRadius: TPointF;
            const ADrawFunc: TALDynamicScrollBoxDownloadMaterialExtData.TDrawMaterialFunc;
            const ADrawEmptyNode: boolean = false);
begin

//*  //-----
//*  if AMediaStatus = TALDownloadingStatus.None then begin
//*    var LRefreshingMaterial := ExtractMaterialFromRefreshingEphemeralLayout(AMaterialIdx);
//*    if (LRefreshingMaterial <> nil) and
//*       (SameValue(LRefreshingMaterial.Width, aDestSize.Width * ALGetScreenScale, 1)) and           // aDestSize use float but RefreshingMaterial.Width/height use Integer
//*       (SameValue(LRefreshingMaterial.height, aDestSize.height * ALGetScreenScale, 1)) then begin  // so it's could be a difference of max 1 pixel
//*      if not OwnerScrollBox.IncrementWaitForCount then exit;
//*      ShiftMaterial(
//*        Key, // const AComparekey: Integer;
//*        AMaterialIdx, // const AMaterialIdx: Integer;
//*        LRefreshingMaterial); // const aMaterial: TALDrawable
//*      exit;
//*    end;
//*    var LMediaBlurred: single;
//*    var LMediaSize: TSizeF;
//*    var LMediaCropCenter: TpointF;
//*    var LMediaPicUrl := ALGetPictureUrl(
//*                          AMediaNode, // const aPicNode: TALJSONNodeW;
//*                          ADestSize.Width, // const aMinWidth: single; // virtual pixel
//*                          ADestSize.Height, // const aMinHeight: single; // virtual pixel
//*                          ALGetScreenScale, // const aScreenScale: single;
//*                          aCropped, // const aCropped: Boolean; // return if possible the url of the cropped image
//*                          LMediaBlurred, // var   aPicBlurred: single;
//*                          LMediaSize, // Var   aPicSize: TpointF; // real pixel - the original picture size of the image or if cropped the dimension of the cropped picture
//*                          LMediaCropCenter); // Var   aPicCropCenter: TpointF
//*    if (not ADrawEmptyNode) and (LMediaPicUrl = '') then AMediaStatus := TALDownloadingStatus.Processed
//*    else begin
//*      {$IFDEF DEBUG}
//*      if ALRandom32(10) = 0 then LMediaPicUrl := LMediaPicUrl + '.bad';
//*      {$ENDIF}
//*      AMediaStatus := TALDownloadingStatus.Downloading;
//*      if not OwnerScrollBox.IncrementWaitForCount then exit;
//*      TALNetHttpClientPool.Instance.Get(
//*        LMediaPicUrl, // const AUrl: String;
//*        CanStartDownloadMaterial, // const ACanStartCallBack: TALNetHttpClientPoolCanStartProc;
//*        OnSuccessDownloadMaterial, // const AOnSuccessCallBack: TALNetHttpClientPoolOnSuccessProc;
//*        OnErrorDownloadMaterial, // const AOnErrorCallBack: TALNetHttpClientPoolOnErrorProc;
//*        TALDynamicScrollBoxDownloadMaterialExtData.Create(
//*          Key, // const AComparekey: Integer;
//*          AMaterialIdx, // const AMaterialIdx: Integer;
//*          LMediaPicUrl, // const AContentUrl: String;
//*          ADestSize, // const ADestSize: TSizeF;
//*          LMediaBlurred, // const AContentBlurred: single;
//*          LMediaCropCenter, // const AContentCropCenter: TpointF;
//*          aCornerRadius, // const aCornerRadius: TPointF;
//*          ADrawFunc), // const ADrawMaterialFunc: TDrawMaterialFunc
//*        true, // const AUseCache: Boolean = True;
//*        GetPriority); // const APriority: Int64 = 0
//*    end;
//*  end;

end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.CanStartDownloadMaterial(var AExtData: TObject): boolean;
begin
  Try
    var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(AExtData);
    result := DoCanStartDownloadMaterial(
                LDownloadMaterialExtData.Comparekey,
                LDownloadMaterialExtData.MaterialIdx);
//*    if not result then OwnerScrollBox.DecrementWaitForCount;
    {$IFDEF DEBUG}
    var LLogtype: TalLogType;
    if Result then LLogtype := TalLogType.verbose
    else LLogtype := TalLogType.warn;
    ALLog(
      Classname+'.CanStartDownloadMaterial.'+ALIfThenW(result,'OK','KO'),
      'Index:'+ALIntToStrW(ItemIndex)+' | '+
      'MaterialIdx:'+ALIntToStrW(LDownloadMaterialExtData.MaterialIdx) + ' | ' +
      'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
      'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
      'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
      LLogtype);
    {$ENDIF}
  except
    On E: Exception do begin
      ALLog(ALFormatW('%s.CanStartDownloadMaterial', [Classname]), E);
      result := True;
    end;
  End;
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxItem.DoCanStartDownloadMaterial(
           const aCompareKey: Integer;
           const aMaterialIdx: Integer): boolean;
begin
  result := //* (not OwnerScrollBox.isDestroying) and
            (AtomicCmpExchange(FKey{target}, 0{NewValue}, 0{Comparand}) = aCompareKey);
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxItem.OnSuccessDownloadMaterial(const AResponse: IHTTPResponse; var AContentStream: TMemoryStream; var AExtData: TObject);
begin

  {$IFDEF DEBUG}
  //*TALLifecycleMonitor.Instance.AppTerminatingSignal.WaitFor(ALRandom32(3000));
  {$ENDIF}

  Try
    var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(AExtData);
    {$IFDEF DEBUG}
    {-----
    Var LLogKO := AtomicCmpExchange(FKey, 0, 0) <> LDownloadMaterialExtData.CompareKey;
    var LLogtype: TalLogType;
    if LLogKO then LLogtype := TalLogType.warn
    else LLogtype := TalLogType.verbose;
    ALLog(
      Classname+'.OnSuccessDownloadMaterial.'+ALIfThenW(LLogKO, 'KO', 'OK'),
      'Index:'+ALIntToStrW(Index)+' | '+
      'MaterialIdx:'+ALIntToStrW(LDownloadMaterialExtData.MaterialIdx) + ' | ' +
      'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
      'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
      'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
      LLogtype);
    ----}
    {$ENDIF}
    if AtomicCmpExchange(FKey{target}, 0{NewValue}, 0{Comparand}) <> LDownloadMaterialExtData.CompareKey then begin
//*      OwnerScrollBox.DecrementWaitForCount;
      exit;
    end;
    LDownloadMaterialExtData.ContentStream := AContentStream;
    AContentStream := nil; // AContentStream will be free by LDownloadMaterialExtData
    TALGraphicThreadPool.Instance.ExecuteProc(
      BuildMaterial, // const AProc: TALWorkerThreadProc;
      LDownloadMaterialExtData, // const AExtData: Tobject; TALGraphicThreadPool.Instance will own and release the ExtData object
      GetPriority); // const APriority: Int64 = 0;
    AExtData := nil; // AExtData will be free by TALGraphicThreadPool.Instance
  except
    On E: Exception do begin
      ALLog(ALFormatW('%s.OnSuccessDownloadMaterial', [Classname]), E);
//*      OwnerScrollBox.DecrementWaitForCount;
    end;
  End;

end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxItem.OnErrorDownloadMaterial(const AErrMessage: string; var AExtData: Tobject);
begin
  Try
    var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(AExtData);
    var LCompareKey := LDownloadMaterialExtData.CompareKey;
    var LMaterialIdx := LDownloadMaterialExtData.MaterialIdx;
    {$IFDEF DEBUG}
    ALLog(
      Classname+'.OnErrorDownloadMaterial',
      'ErrMessage:'+AErrMessage + ' | ' +
      'Index:'+ALIntToStrW(ItemIndex)+' | '+
      'MaterialIdx:'+ALIntToStrW(LMaterialIdx) + ' | ' +
      'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
      'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
      'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
      TalLogType.error);
    {$ENDIF}
    if AtomicCmpExchange(FKey{target}, 0{NewValue}, 0{Comparand}) <> LCompareKey then begin
//*      OwnerScrollBox.DecrementWaitForCount;
      exit;
    end;
    TThread.queue(nil,
      procedure
      begin
        ShiftMaterial(
          LCompareKey,
          LMaterialIdx,
          ALNullDrawable);
      end);
  except
    On E: Exception do begin
      ALLog(ALFormatW('%s.BuildMaterial', [Classname]), E);
//*      OwnerScrollBox.DecrementWaitForCount;
    end;
  End;
end;

{**********************************************************************************************************************}
function TALDynamicScrollBoxItem.ExtractMaterialFromRefreshingEphemeralLayout(const aMaterialIdx: Integer): TALDrawable;
begin
  result := ALNullDrawable;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxItem.BuildMaterial(var AExtData: TObject);
begin
  //*OwnerScrollBox.BeginRead;
  try
    Try
      var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(AExtData);
      var LCompareKey := LDownloadMaterialExtData.CompareKey;
      var LMaterialIdx := LDownloadMaterialExtData.MaterialIdx;
      {$IFDEF DEBUG}
      {-----
      Var LLogKO := AtomicCmpExchange(FKey, 0, 0) <> LCompareKey;
      var LLogtype: TalLogType;
      if LLogKO then LLogtype := TalLogType.warn
      else LLogtype := TalLogType.verbose;
      ALLog(
        Classname+'.BuildMaterial.'+ALIfThenW(LLogKO, 'KO', 'OK'),
        'Index:'+ALIntToStrW(Index)+' | '+
        'MaterialIdx:'+ALIntToStrW(LMaterialIdx) + ' | ' +
        'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
        'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
        'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
        LLogtype);
      -----}
      {$ENDIF}
      if AtomicCmpExchange(FKey{target}, 0{NewValue}, 0{Comparand}) <> LCompareKey then begin
//*        OwnerScrollBox.DecrementWaitForCount;
        exit;
      end;
      Var LMaterial := DoBuildMaterial(LDownloadMaterialExtData);
      TThread.queue(nil,
        procedure
        begin
          ShiftMaterial(
            LCompareKey,
            LMaterialIdx,
            LMaterial);
        end);
    except
      On E: Exception do begin
        ALLog(ALFormatW('%s.BuildMaterial', [Classname]), E);
//*        OwnerScrollBox.DecrementWaitForCount;
      end;
    End;
  finally
  //*  OwnerScrollBox.EndRead;
  end;
end;

{********************************}
//[MultiThread][BeginRead/EndRead]
function TALDynamicScrollBoxItem.DoBuildMaterial(const ADownloadMaterialExtData: TALDynamicScrollBoxDownloadMaterialExtData): TALDrawable;
begin
  Var LDownloadMaterialExtData := TALDynamicScrollBoxDownloadMaterialExtData(ADownloadMaterialExtData);
  if assigned(LDownloadMaterialExtData.DrawMaterialFunc) then
    Result := LDownloadMaterialExtData.DrawMaterialFunc(
                self,
                LDownloadMaterialExtData.ContentStream,
                LDownloadMaterialExtData.ContentUrl,
                LDownloadMaterialExtData.DestSize,
                LDownloadMaterialExtData.ContentBlurred,
                LDownloadMaterialExtData.ContentCropCenter,
                LDownloadMaterialExtData.CornerRadius,
                ALGetScreenScale)
  else
    raise Exception.Create('Error 5F9A9E7B-9D6C-4451-AF23-3AE9325EB3AB');
end;

{**********************************************}
procedure TALDynamicScrollBoxItem.ShiftMaterial(
            const aCompareKey: Integer;
            const aMaterialIdx: Integer;
            const aMaterial: TALDrawable);
begin
  {$IFDEF DEBUG}
  {-----
  Var LLogKO := AtomicCmpExchange(FKey, 0, 0) <> aCompareKey;
  var LLogtype: TalLogType;
  if LLogKO then LLogtype := TalLogType.warn
  else LLogtype := TalLogType.verbose;
  ALLog(
    Classname+'.ShiftMaterial.'+ALIfThenW(LLogKO, 'KO', 'OK'),
    'Index:'+ALIntToStrW(Index)+' | '+
    'MaterialIdx:'+ALIntToStrW(aMaterialIdx) + ' | ' +
    'Priority:'+ALIntToStrW(GetPriority(nil)) + ' | ' +
    'GlobalPriorityStartingPoint:' + ALIntToStrW(TALGraphicThreadPool.Instance.PriorityStartingPoint) + ' | ' +
    'GlobalPriorityDirection:' + TRttiEnumerationType.GetName(TALGraphicThreadPool.Instance.PriorityDirection),
    LLogtype);
  -----}
  {$ENDIF}
  Try
    Try
      //*if AtomicCmpExchange(FKey{target}, 0{NewValue}, 0{Comparand}) <> aCompareKey then ALFreeAndNil(aMaterial, true{aDelayed})
      //*else begin
      //*  if (fEphemeralLayout <> nil) then DoShiftMaterial(aMaterialIdx, aMaterial)
      //*  else ALFreeAndNil(aMaterial, true{aDelayed});
      //*  OwnerScrollBox.repaint;
      //*end;
    except
      On E: Exception do
        ALLOG(ALFormatW('%s.ShiftMaterial', [Classname]), E);
    End;
  finally
//*    OwnerScrollBox.DecrementWaitForCount;
  end;
end;

{************************************************}
procedure TALDynamicScrollBoxItem.DoShiftMaterial(
            const aMaterialIdx: Integer;
            const aMaterial: TALDrawable);
begin
  // if aMaterial = nil then set the status of the material in the EphemeralLayout to TALDownloadingStatus.Error
end;

{***********************************************}
function TALDynamicScrollBoxItem.GetKey: integer;
begin
  result := AtomicCmpExchange(FKey{target}, 0{NewValue}, 0{Comparand});
end;

{*******************************************************************************}
function TALDynamicScrollBoxItem.IsValidKey(Const aCompareKey: integer): Boolean;
begin
  result := Key = aCompareKey;
end;

{***********************************************************************************************}
constructor TALDynamicScrollBoxViewEphemeralLayout.Create(const AOwner: TALDynamicScrollBoxItem);
begin
  inherited create(AOwner);
  //-----
  GrowItemsAniindicatorPos := TAlPointD.create(-1000,-1000);
  ItemsReloadBtnPos := TAlPointD.create(-1000,-1000);
  ItemsReloadBtnTouchTargetRect := TalRectD.create(0,0,0,0);
  //------
  NoItemsIcon := false;
  //NoItemsIconPos := TAlPointD.create(0,0);
  NoItemsIconTouchTargetRect := TALRectD.create(0,0,0,0);
  NoItemsTitle := false;
  //NoItemsTitlePos := TAlPointD.create(0,0);
  NoItemsTitleTouchTargetRect := TALRectD.create(0,0,0,0);
  NoItemsSubTitle := false;
  //NoItemsSubTitlePos := TAlPointD.create(0,0);
  NoItemsSubTitleTouchTargetRect := TALRectD.create(0,0,0,0);
  NoItemsBtn := False;
  //NoItemsBtnPos := TAlPointD.create(0,0);
  NoItemsBtnTouchTargetRect := TALRectD.create(0,0,0,0);
end;

{********************************************************************}
constructor TALDynamicScrollBoxView.TGrowItemsBackgroundThread.Create(
              const AOwner: TALDynamicScrollBoxView;
              const AInsertAt: integer;
              const AItems: TALJSONNodeW;
              const AMaxItems: integer);
begin
  inherited Create(True{CreateSuspended});
  FreeOnTerminate := True;
  FOwner := AOwner;
  FInsertAt := AInsertAt;
  FItems := AItems;
  FMaxItems := AMaxItems;
end;

{********************************************************************}
destructor TALDynamicScrollBoxView.TGrowItemsBackgroundThread.Destroy;
begin
  ALFreeAndNil(FItems);
  inherited Destroy;
end;

{*******************************************************************}
procedure TALDynamicScrollBoxView.TGrowItemsBackgroundThread.Execute;
begin

  if terminated then exit;

  try

    {$IFDEF DEBUG}
    if TThread.Current.ThreadID <> MainThreadID then
      TThread.NameThreadForDebugging(Classname);
    {$ENDIF}

    if TThread.CurrentThread.ThreadID = MainThreadID then begin
      While not TMonitor.TryEnter(FOwner.FGrowItemsLock) do begin
        Sleep(10);
        CheckSynchronize;
      end;
    end
    else
      Tmonitor.Enter(FOwner.FGrowItemsLock);

    try

      Try
        FOwner.GrowItemsBackgroundProc(self);
      Finally
        Atomicdecrement(FOwner.FGrowItemsBackgroundThreadsCount);
      End;

    finally
      Tmonitor.Exit(FOwner.FGrowItemsLock);
    end;

    TThread.Synchronize(nil,
      procedure
      begin
        FOwner.GrowItemsFinished(self);
      end);

  finally
//*    FOwner.OwnerScrollBox.DecrementWaitForCount;
  end;

end;

{***********************************************************************************}
constructor TALDynamicScrollBoxView.Create(const AOwner: TALDynamicScrollBoxControl);
begin
  FIsScrollBoxView := True;
  //-----
  inherited create(AOwner);
  //-----
  FItemsOrientation := Torientation.Vertical;
  FItemsReversedOrientation := False;
  FItemIDType := TItemIDType.Int64;
  FItemIDPath := 'id';
  FItemInt64Ids := TDictionary<Int64, boolean>.create;
  FItemStrIds := TDictionary<String, boolean>.create;
  Setlength(FItems, 0);
  FCanGrowItems := True;
  FGrowItemsAtIdx := -maxint;
  FGrowItemsExecutedAtLeastOnce := False;
  FGrowItemsError := False;
  FShowApiErrorIfGrowItemsError := True;
  FDownloadItemsNextPageToken := #0(*vWin_NullValueU*);
  FGrowItemsLock := Tobject.Create;
  FGrowItemsBackgroundThreadsCount := 0;
  FIsArrangingItems := False;
  //-----
  fNewItemAnim := nil;
  fNewItemQueue := TObjectlist<TALJSONNodeW>.create(true{AOwnsObjects});
  //-----
  //*ActionBar := CreateActionBar;
  //-----
  FScrollPos := 0;
  FScrollFirstVisibleItemIdx := 0;   // = Low(FItems) when FItems is empty
  FScrollLastVisibleItemIdx := -1;    // = High(FItems) when FItems is empty
  FScrollFirstPreloadedItemIdx := 0; // = Low(FItems) when FItems is empty
  FScrollLastPreloadedItemIdx := -1;  // = High(FItems) when FItems is empty
  FScrollFromFirstToLast := True;
  FScrollMinBound := 0;
  FScrollMaxBound := 0;
  FSetScrollPosGuard := False;
  FPostponedScrollPosSet := False;
  FPostponedScrollPos := 0;
  //------
  FScrollEngine := TALScrollEngine.Create;
  {$IF (not defined(DEBUG)) and (defined(MSWINDOWS) or defined(ALMacOS))}
  FScrollEngine.TouchTracking := [];
  {$ELSE}
  FScrollEngine.TouchTracking := [ttVertical];
  {$ENDIF}
  FScrollEngine.OnChanged := ScrollEngineChanged;
  FScrollEngine.OnStart := ScrollEngineStart;
  FScrollEngine.OnStop := ScrollEngineStop;
end;

{**************************************************}
procedure TALDynamicScrollBoxView.AfterConstruction;
begin
  inherited AfterConstruction;
  {$IF (not defined(DEBUG)) and (defined(MSWINDOWS) or defined(ALMacOS))}
  FScrollEngine.TouchTracking := [];
  {$ELSE}
  if ItemsOrientation = Torientation.Vertical then FScrollEngine.TouchTracking := [ttVertical]
  else FScrollEngine.TouchTracking := [tthorizontal];
  {$ENDIF}
  if FItemsReversedOrientation then FGrowItemsAtIdx := MaxInt
  else FGrowItemsAtIdx := -MaxInt;
  FScrollFromFirstToLast := not FItemsReversedOrientation;
end;

{*****************************************}
destructor TALDynamicScrollBoxView.Destroy;
begin
  for var I := Low(FItems) to High(FItems) do
    ALFreeAndNil(FItems[i]);
  ALFreeAndNil(FItemInt64Ids);
  ALFreeAndNil(FItemStrIds);
  ALfreeAndNil(FGrowItemsLock);
  AlFreeAndNil(fNewItemAnim);
  AlFreeAndNil(fNewItemQueue);
  ALFreeAndNil(FScrollEngine);
  inherited Destroy;
end;

{**********************************************}
procedure TALDynamicScrollBoxView.BeforeDestroy;
begin
  inherited BeforeDestroy;
  FScrollEngine.Stop(true{AAbruptly});
  if fNewItemAnim <> nil then
    fNewItemAnim.enabled := False;
  for var I := Low(FItems) to High(FItems) do
    FItems[i].BeforeDestroy;
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxView.AsyncDestroy;
begin
  inherited AsyncDestroy;
  for var I := Low(FItems) to High(FItems) do
    FItems[i].AsyncDestroy;
end;

{****************************************}
procedure TALDynamicScrollBoxView.Prepare;
begin
  inherited Prepare;
  // SetScrollPos will Prepare of all items
  // and also call growItems
  SetScrollPos(ScrollPos);
end;

{***********************************************}
procedure TALDynamicScrollBoxView.UpdatePriority;
begin
  inherited UpdatePriority;
  for var I := 0 to itemsCount - 1 do
    items[i].UpdatePriority;
end;

{**************************************************************************}
function TALDynamicScrollBoxView.CanUpdateGlobalThreadPoolPriority: boolean;
begin
  if OwnerView = nil then exit(true);
  if ScrollFromFirstToLast then result := ItemIndex = OwnerView.ScrollFirstVisibleItemIdx
  else result := ItemIndex = OwnerView.ScrollLastVisibleItemIdx;
  if result then result := OwnerView.CanUpdateGlobalThreadPoolPriority;
end;

{***************************************************************}
procedure TALDynamicScrollBoxView.UpdateGlobalThreadPoolPriority;
begin
  if (ScrollFromFirstToLast) and
     (ScrollFirstVisibleItemIdx >= 0) and
     (ScrollFirstVisibleItemIdx < ItemsCount) then begin
    Var Litem := Items[ScrollFirstVisibleItemIdx];
    if (Litem.IsScrollBoxView) then
      TALDynamicScrollBoxView(Litem).UpdateGlobalThreadPoolPriority
    else begin
      if not CanUpdateGlobalThreadPoolPriority then exit;
      //*OwnerScrollBox.SetGlobalThreadPoolPriority(
      //*  Litem.GetPriority(nil),
       //* TALWorkerThreadPool.TPriorityDirection.GreaterThan);
    end;
  end
  else if (not ScrollFromFirstToLast) and
          (ScrollLastVisibleItemIdx >= 0) and
          (ScrollLastVisibleItemIdx < ItemsCount) then begin
    Var Litem := Items[ScrollLastVisibleItemIdx];
    if (Litem.IsScrollBoxView) then
      TALDynamicScrollBoxView(Litem).UpdateGlobalThreadPoolPriority
    else begin
      if not CanUpdateGlobalThreadPoolPriority then exit;
      //*OwnerScrollBox.SetGlobalThreadPoolPriority(
      //*  Litem.GetPriority(nil),
      //*  TALWorkerThreadPool.TPriorityDirection.lessThan);
    end;
  end
  else if OwnerView <> nil then begin
    if not CanUpdateGlobalThreadPoolPriority then exit;
    if (OwnerView.ScrollFromFirstToLast) then
      //*OwnerScrollBox.SetGlobalThreadPoolPriority(
      //*  GetPriority(nil),
      //*  TALWorkerThreadPool.TPriorityDirection.GreaterThan)
    else
      //*OwnerScrollBox.SetGlobalThreadPoolPriority(
      //*  GetPriority(nil),
      //*  TALWorkerThreadPool.TPriorityDirection.lessThan)
  end
  else
    //*OwnerScrollBox.SetGlobalThreadPoolPriority(
    //*  OwnerScrollBox.GetPriority(nil),
    //*  TALWorkerThreadPool.TPriorityDirection.GreaterThan);
end;

{******************************************}
//Add the current scrollpos to the given Pos
function TALDynamicScrollBoxView.LocalToScroll(const APoint: TPointF): TALPointD;
begin
  Result := TALPointD.Create(APoint);
  if ItemsOrientation = Torientation.Vertical then Result.Offset(0, ScrollPos)
  else Result.Offset(ScrollPos, 0);
end;

{*******************************************}
//Add the current scrollpos to the given Rect
function TALDynamicScrollBoxView.LocalToScroll(const ARect: TRectF): TALRectD;
begin
  Result := TALRectD.Create(ARect);
  if ItemsOrientation = Torientation.Vertical then Result.Offset(0, ScrollPos)
  else Result.Offset(ScrollPos, 0);
end;

{*********************************************}
//remove the current scrollpos to the given Pos
function TALDynamicScrollBoxView.ScrollToLocal(const APoint: TALPointD): TALPointD;
begin
  Result := APoint;
  if ItemsOrientation = Torientation.Vertical then Result.Offset(0, -ScrollPos)
  else Result.Offset(-ScrollPos, 0);
end;

{**********************************************}
//remove the current scrollpos to the given Rect
function TALDynamicScrollBoxView.ScrollToLocal(const ARect: TALRectD): TALRectD;
begin
  Result := ARect;
  if ItemsOrientation = Torientation.Vertical then Result.Offset(0, -ScrollPos)
  else Result.Offset(-ScrollPos, 0);
end;

{*******************************************************}
//Translates a given point (minus current scrollpos) from
//client area coordinates to owner frame coordinates.
function TALDynamicScrollBoxView.ScrollToFrame(const APoint: TALPointD): TALPointD;
begin
  Result := ScrollToLocal(APoint);
  Result.Offset(BoundsRect.TopLeft);
  if ownerView <> nil then result := ownerView.ScrollToFrame(APoint);
end;

{*******************************************************}
//Translates a given point (minus current scrollpos) from
//client area coordinates to owner frame coordinates.
function TALDynamicScrollBoxView.ScrollToFrame(const ARect: TALRectD): TALRectD;
begin
  Result := ScrollToLocal(ARect);
  Result.Offset(BoundsRect.TopLeft);
  if ownerView <> nil then result := ownerView.ScrollToFrame(Arect);
end;

{**************************************************************}
function TALDynamicScrollBoxView.GetItemsPreloadOffset: integer;
begin
  result := OwnerScrollBox.ItemsPreloadOffset;
end;

{*******************************************************************}
function TALDynamicScrollBoxView.GetMeasures(Index: Integer): Single;
begin
  case index of
    0  {ItemsReloadBtnWidth}: result := OwnerScrollBox.ItemsReloadBtnWidth;
    1  {ItemsReloadBtnHeight}: result := OwnerScrollBox.ItemsReloadBtnHeight;
    2  {ItemsReloadBtnMarginTop}: result := OwnerScrollBox.ItemsReloadBtnMarginTop;
    3  {ItemsReloadBtnMarginBottom}: result := OwnerScrollBox.ItemsReloadBtnMarginBottom;
    24 {ItemsReloadBtnMarginRight}: result := OwnerScrollBox.ItemsReloadBtnMarginRight;
    25 {ItemsReloadBtnMarginLeft}: result := OwnerScrollBox.ItemsReloadBtnMarginLeft;
    4  {GrowItemsAniindicatorMarginTop}: result := OwnerScrollBox.GrowItemsAniindicatorMarginTop;
    5  {GrowItemsAniindicatorMarginBottom}: result := OwnerScrollBox.GrowItemsAniindicatorMarginBottom;
    26 {GrowItemsAniindicatorMarginRight}: result := OwnerScrollBox.GrowItemsAniindicatorMarginRight;
    27 {GrowItemsAniindicatorMarginLeft}: result := OwnerScrollBox.GrowItemsAniindicatorMarginLeft;
    6  {NoItemsMarginTop}: result := OwnerScrollBox.NoItemsMarginTop;
    7  {NoItemsMarginBottom}: result := OwnerScrollBox.NoItemsMarginBottom;
    8  {NoItemsMarginLeft}: result := OwnerScrollBox.NoItemsMarginLeft;
    9  {NoItemsMarginRight}: result := OwnerScrollBox.NoItemsMarginRight;
    10 {NoItemsIconWidth}: result := OwnerScrollBox.NoItemsIconWidth;
    11 {NoItemsIconHeight}: result := OwnerScrollBox.NoItemsIconHeight;
    12 {NoItemsTitleTextSize}: result := OwnerScrollBox.NoItemsTitleTextSize;
    13 {NoItemsTitleLineSpacing}: result := OwnerScrollBox.NoItemsTitleLineSpacing;
    14 {NoItemsTitleMarginTop}: result := OwnerScrollBox.NoItemsTitleMarginTop;
    15 {NoItemsSubTitleTextSize}: result := OwnerScrollBox.NoItemsSubTitleTextSize;
    16 {NoItemsSubTitleLineSpacing}: result := OwnerScrollBox.NoItemsSubTitleLineSpacing;
    17 {NoItemsSubTitleMarginTop}: result := OwnerScrollBox.NoItemsSubTitleMarginTop;
    18 {NoItemsBtnTextSize}: result := OwnerScrollBox.NoItemsBtnTextSize;
    19 {NoItemsBtnLineSpacing}: result := OwnerScrollBox.NoItemsBtnLineSpacing;
    20 {NoItemsBtnMarginTop}: result := OwnerScrollBox.NoItemsBtnMarginTop;
    21 {NoItemsBtnRadius}: result := OwnerScrollBox.NoItemsBtnRadius;
    22 {NoItemsBtnPaddingX}: result := OwnerScrollBox.NoItemsBtnPaddingX;
    23 {NoItemsBtnPaddingY}: result := OwnerScrollBox.NoItemsBtnPaddingY;
    else raise Exception.Create('Error E86BC3C4-DEDD-4B99-B658-76CADF3DBAAE');
  end;
end;

{**********************************************************************}
function TALDynamicScrollBoxView.GetBitmap(Index: Integer): TALDrawable;
begin
  case index of
    0 {ItemsReloadBtn}: result := OwnerScrollBox.ItemsReloadBtn;
    1 {NoItemsIcon}: result := OwnerScrollBox.NoItemsIcon;
    2 {NoItemsTitle}: result := OwnerScrollBox.NoItemsTitle;
    3 {NoItemsSubTitle}: result := OwnerScrollBox.NoItemsSubTitle;
    4 {NoItemsBtn}: result := OwnerScrollBox.NoItemsBtn;
    else raise Exception.Create('Error 1EA4FE23-DA3D-4149-A626-97EBFDE4417E');
  end;
end;

{****************************************************************************************************}
procedure TALDynamicScrollBoxView.Paint(Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single);
begin

  {$REGION 'init local vars'}
  var LEphemeralLayout := EnforceEphemeralLayout(false{a4preload});
  if LEphemeralLayout = nil then exit;
  {$ENDREGION}

  {$REGION 'draw all visible items'}
  var LSavedMatrix := Canvas.Matrix;
  try
    for var i := ScrollFirstVisibleItemIdx to ScrollLastVisibleItemIdx do begin
      Var LItem := Items[i];
      if not Litem.Visible then continue;
      Var LDeltaRect := ScrollToLocal(LItem.BoundsRect).ReducePrecision;
      //-----
      var LOpacity: single;
      if (NewItemAnim <> nil) and
         (NewItemAnim.Enabled) then begin
         if ((FItemsReversedOrientation) and
             (i < NewItemAnim.tag)) or
            ((not FItemsReversedOrientation) and
             (i > NewItemAnim.tag)) then begin
           LOpacity := 1;
           if ItemsOrientation=Torientation.vertical then begin
             if (FItemsReversedOrientation) then
               LDeltaRect.SetLocation(
                 LDeltaRect.left,
                 LDeltaRect.top +
                 Items[NewItemAnim.tag].BoundsRect.height -
                 (NewItemAnim.CurrentValue * Items[NewItemAnim.tag].BoundsRect.height))
             else
               LDeltaRect.SetLocation(
                 LDeltaRect.left,
                 LDeltaRect.top -
                 Items[NewItemAnim.tag].BoundsRect.height +
                 (NewItemAnim.CurrentValue * Items[NewItemAnim.tag].BoundsRect.height));
           end
           else begin
             if (FItemsReversedOrientation) then
               LDeltaRect.SetLocation(
                 LDeltaRect.left +
                 Items[NewItemAnim.tag].BoundsRect.width -
                 (NewItemAnim.CurrentValue * Items[NewItemAnim.tag].BoundsRect.width),
                 LDeltaRect.top)
             else
               LDeltaRect.SetLocation(
                 LDeltaRect.left -
                 Items[NewItemAnim.tag].BoundsRect.width +
                 (NewItemAnim.CurrentValue * Items[NewItemAnim.tag].BoundsRect.width),
                 LDeltaRect.top);
           end;
         end
         else if i = NewItemAnim.tag then LOpacity := (NewItemAnim.CurrentTime / NewItemAnim.Duration)
         else LOpacity := 1;
      end
      else LOpacity := 1;
      //-----
      var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(LDeltaRect.left,LDeltaRect.Top);
      Canvas.SetMatrix(LMatrix);
      Var LDeltaTopLeft := LDeltaRect.TopLeft;
      LDeltaRect := TRectF.Intersect(ARect, LDeltaRect);
      LDeltaRect.Offset(-LDeltaTopLeft);
      LItem.Paint(Canvas, LDeltaRect, aOpacity * LOpacity);
    end;
  finally
    Canvas.SetMatrix(LSavedMatrix);
  end;
  {$ENDREGION}

  {$REGION 'draw the ItemsReloadBtn'}
  if GrowItemsError then begin
    //*OwnerScrollBox.drawBitmap(
    //*  Canvas,
    //*  ARect,
    //*  ItemsReloadBtn, // aBitmap
    //*  ScrollToLocal(LEphemeralLayout.ItemsReloadBtnPos).ReducePrecision, // aTopLeft
    //*  aOpacity); // aOpacity
  end;
  {$ENDREGION}

  {$REGION 'draw NoItemsIcon'}
  if LEphemeralLayout.NoItemsIcon then begin
    //*OwnerScrollBox.drawBitmap(
    //*  Canvas,
    //*  ARect,
    //*  NoItemsIcon, // aBitmap
    //*  ScrollToLocal(LEphemeralLayout.NoItemsIconPos).ReducePrecision, // aTopLeft
    //*  aOpacity); // aOpacity
  end;
  {$ENDREGION}

  {$REGION 'draw NoItemsTitle'}
  if LEphemeralLayout.NoItemsTitle then begin
    //*OwnerScrollBox.drawBitmap(
    //*  Canvas,
    //*  ARect,
    //*  NoItemsTitle, // aBitmap
    //*  ScrollToLocal(LEphemeralLayout.NoItemsTitlePos).ReducePrecision, // aTopLeft
    //*  aOpacity); // aOpacity
  end;
  {$ENDREGION}

  {$REGION 'draw NoItemsSubTitle'}
  if LEphemeralLayout.NoItemsSubTitle then begin
    //*OwnerScrollBox.drawBitmap(
    //*  Canvas,
    //*  ARect,
    //*  NoItemsSubTitle, // aBitmap
    //*  ScrollToLocal(LEphemeralLayout.NoItemsSubTitlePos).ReducePrecision, // aTopLeft
    //*  aOpacity); // aOpacity
  end;
  {$ENDREGION}

  {$REGION 'draw NoItemsBtn'}
  if LEphemeralLayout.NoItemsBtn then begin
    //*OwnerScrollBox.drawBitmap(
    //*  Canvas,
    //*  ARect,
    //*  NoItemsBtn, // aBitmap
    //*  ScrollToLocal(LEphemeralLayout.NoItemsBtnPos).ReducePrecision, // aTopLeft
    //*  aOpacity); // aOpacity
  end;
  {$ENDREGION}

  {$REGION 'draw the GrowItemsAniindicator'}
  if CanGrowItems and
     (not GrowItemsError) then begin
    //*OwnerScrollBox.drawAniIndicator(
    //*  Canvas, // const Canvas: TCanvas;
    //*  ARect, // const ARect: TRectF;
    //*  OwnerScrollBox.fAniindicatorImgIndex, // const aAniindicatorImgIndex: TSmallPoint;
    //*  ScrollToLocal(LEphemeralLayout.GrowItemsAniindicatorPos).ReducePrecision, // const aY: single;
    //*  aOpacity, // const aOpacity: Single;
    //*  ALGetScreenScale, // const aScreenScale: single;
    //*  OwnerScrollBox.FAniIndicatorEnabled); // Var LAniIndicatorEnabled: Boolean
  end;
  {$ENDREGION}

  {$REGION 'inherited'}
  inherited Paint(Canvas, ARect, aOpacity);
  {$ENDREGION}

end;

{****************************************************************}
procedure TALDynamicScrollBoxView.SetBoundsRect(AValue: TALRectD);
begin
  var LChanged := (not sameValue(FBoundsRect.Width, Avalue.Width, TEpsilon.Position)) or
                  (not sameValue(FBoundsRect.height, Avalue.height, TEpsilon.Position));
  inherited SetBoundsRect(aValue);
  if LChanged then begin
    arrangeItems(
      TArrangeItemsCause.Other, // const ACause: TALDynamicScrollBoxView.TArrangeItemsCause;
      0, // const AFromIdx: integer;
      itemsCount - 1); // const AToIdx: integer)
  end
end;

{*************************************************************************************************************************}
procedure TALDynamicScrollBoxView.Click(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the view
begin

  {$REGION 'Inherited'}
  inherited Click(Sender, AMousePos);
  {$ENDREGION}

  {$REGION 'LScrollMousePos'}
(*
  var LScrollMousePos := LocalToScroll(AMousePos);
*)
  {$ENDREGION}

  {$REGION 'ItemsReloadBtnClick'}
(*
  if (GrowItemsError) and
     (EphemeralLayout <> nil) and
     (EphemeralLayout.ItemsReloadBtnTouchTargetRect.Contains(LScrollMousePos)) then begin
    TALTouchEffectBuilder.create(OwnerScrollBox)
      .bounds(ScrollToFrame(EphemeralLayout.ItemsReloadBtnTouchTargetRect).ReducePrecision) // const ABounds: TRectf
      .Kind(TALTouchEffectKind.Circle) // default TALTouchEffectKind.RectangleAndCircle
      .ClipChildren(false) // default True
      .OnFinishEvent(ItemsReloadBtnClick)
      .InflateBoundsBy(-OwnerScrollBox.TouchTargetExpansion)
      .start({aStartImmediatly});
    exit(true);
  end;
*)
  {$ENDREGION}

  {$REGION 'NoItemsBtnClick'}
(*
  if (EphemeralLayout <> nil) and
     (EphemeralLayout.NoItemsBtnTouchTargetRect.Contains(LScrollMousePos.ReducePrecision)) then begin
    TALTouchEffectBuilder.create(OwnerScrollBox)
      .bounds(ScrollToFrame(EphemeralLayout.NoItemsBtnTouchTargetRect).ReducePrecision)
      .mousePos(LocalToFrame(AMousePos).ReducePrecision)
      .OnFinishEvent(NoItemsBtnClick)
      .BackgroundColor(OwnerScrollBox.AbsoluteBackgroundColor)
      .RectangleXRadius(NoItemsBtnRadius)
      .RectangleYRadius(NoItemsBtnRadius)
      .InflateBoundsBy(-OwnerScrollBox.TouchTargetExpansion)
      .start({aStartImmediatly});
    exit(true);
  end;
*)
  {$ENDREGION}

end;

{*****************************************************************************************************************************}
procedure TALDynamicScrollBoxView.MouseMove(const Sender: TObject; const AMousePos: TPointF); // AMousePos is local to the view
begin

  {$REGION 'Children MouseMove'}
  inherited MouseMove(Sender, AMousePos);
  {$ENDREGION}

  {$REGION 'crhandpoint cursor'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
(*
  var LScrollMousePos := LocalToScroll(AMousePos);
  if (EphemeralLayout <> nil) and
     (EphemeralLayout.NoItemsBtnTouchTargetRect.Contains(LScrollMousePos)) then OwnerScrollBox.cursor := crhandpoint
  else if (GrowItemsError) and
          (EphemeralLayout <> nil) and
          (EphemeralLayout.ItemsReloadBtnTouchTargetRect.Contains(LScrollMousePos)) then OwnerScrollBox.cursor := crhandpoint;
*)
  {$endif}
  {$ENDREGION}

end;

{*********************************************************************}
procedure TALDynamicScrollBoxView.ItemsReloadBtnClick(Sender: Tobject);
begin
  ALFreeAndNil(OwnerScrollBox.ErrorMessageBannerAnim);
  GrowData(true{aForceGrow});
  GrowItems(true{aForceGrow});
  OwnerScrollBox.repaint;
end;

{*****************************************************************}
procedure TALDynamicScrollBoxView.NoItemsBtnClick(Sender: Tobject);
begin
  //virtual;
end;

{*********************************************************************}
procedure TALDynamicScrollBoxView.ScrollEngineChanged(Sender: TObject);
begin
  {$IFDEF DEBUG}
  //ALLog(Classname+'.ScrollEngineChanged', TalLogType.verbose);
  {$ENDIF}
  if (ScrollEngine.Down) and
     (ScrollEngine.Moved) and
     (OwnerScrollBox.ActiveScrollEngines.Count > 1) then begin
    var LActiveScrollEnginesArray := OwnerScrollBox.ActiveScrollEngines.ToArray;
    for var I := low(LActiveScrollEnginesArray) to high(LActiveScrollEnginesArray) do
      if (LActiveScrollEnginesArray[i] <> ScrollEngine) and
         (LActiveScrollEnginesArray[i].down) then
        LActiveScrollEnginesArray[i].Down := False;
  end;
  if ItemsOrientation = TOrientation.Vertical then SetScrollPos(ScrollEngine.ViewportPosition.Y, false{AEnforceScrollLimits}, True{ATriggeredByScrollEvent})
  else SetScrollPos(ScrollEngine.ViewportPosition.X, false{AEnforceScrollLimits}, True{ATriggeredByScrollEvent});
end;

{*******************************************************************}
procedure TALDynamicScrollBoxView.ScrollEngineStart(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.ScrollEngineStart', TalLogType.verbose);
  if OwnerScrollBox.ActiveScrollEngines.Contains(ScrollEngine) then
    raise Exception.Create('Error 5BB2514C-AD9C-4EA9-9294-7753FA6B8894');
  {$ENDIF}
  OwnerScrollBox.ActiveScrollEngines.Add(ScrollEngine);
  OwnerScrollBox.HasActiveScrollEngines := true;
end;

{******************************************************************}
procedure TALDynamicScrollBoxView.ScrollEngineStop(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.ScrollEngineStop', TalLogType.verbose);
  if not OwnerScrollBox.ActiveScrollEngines.Contains(ScrollEngine) then
    raise Exception.Create('Error A50AC83D-D186-437D-A186-EBFC9157E46F');
  {$ENDIF}
  OwnerScrollBox.ActiveScrollEngines.Remove(ScrollEngine);
  OwnerScrollBox.HasActiveScrollEngines := OwnerScrollBox.ActiveScrollEngines.Count > 0;
end;

{***************************************************************************************************}
function TALDynamicScrollBoxView.GetScrollEnginesAtPos(const aPos: TPointF): TArray<TALScrollEngine>;
begin
  var LControlPos: TPointF;
  var LControl := GetControlAtPos(aPos, LControlPos, True{ADirectChildOnly}, false{ACheckHitTest});
  if (LControl <> self) and (LControl.isScrollBoxView) then begin
    result := TALDynamicScrollBoxView(LControl).GetScrollEnginesAtPos(LControlPos);
    setlength(result, length(result)+1);
  end
  else begin
    setlength(result, 1);
  end;
  result[high(result)] := ScrollEngine;
end;

{******************************************************************************************}
procedure TALDynamicScrollBoxView.InsertControl(const AControl: TALDynamicScrollBoxControl);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _SetRelativeOwners(const AControl: TALDynamicScrollBoxControl);
  begin
    AControl.FOwnerScrollBox := OwnerScrollBox;
    if AControl.FOwnerView = nil then AControl.FOwnerView := Self;
    if AControl.FOwnerItem = nil then AControl.FOwnerItem := Self;
    for var I := low(AControl.FControls) to high(AControl.FControls) do
      _SetRelativeOwners(AControl.FControls[i]);
  end;

begin
  if AControl = nil then exit;
  if AControl.IsScrollBoxItem then begin
    AControl.FOwner := self;
    _SetRelativeOwners(AControl);
  end
  else inherited InsertControl(AControl);
end;

{******************************************************************************************}
procedure TALDynamicScrollBoxView.RemoveControl(const AControl: TALDynamicScrollBoxControl);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ClearRelativeOwners(const AControl: TALDynamicScrollBoxControl);
  begin
    AControl.FOwnerScrollBox := nil;
    if (AControl.FOwnerItem = self) or (AControl.FOwnerItem = FOwnerItem) then AControl.FOwnerItem := nil;
    if (AControl.FOwnerView = self) or (AControl.FOwnerView = FOwnerView) then AControl.FOwnerView := nil;
    for var I := low(AControl.FControls) to high(AControl.FControls) do
      _ClearRelativeOwners(AControl.FControls[i]);
  end;

begin
  if FIsDestroying then exit;
  if AControl = nil then exit;
  if AControl.isScrollBoxItem then begin
    AControl.FOwner := nil;
    _ClearRelativeOwners(AControl);
  end
  else inherited RemoveControl(AControl);
end;

{***********************************************}
function TALDynamicScrollBoxView.GetControlAtPos(
           const aPos: TPointF; // APos is local to the control
           out AControlPos: TPointF; // AControlPos is local to the founded control
           const ADirectChildOnly: Boolean = False;
           const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl;
begin
  Result := Inherited GetControlAtPos(aPos, AControlPos, ADirectChildOnly, ACheckHitTest);
  //--
  if result = Self then result := nil;
  if result = FephemeralLayout then result := nil; (* temp fix *)
  if result <> nil then exit;
  if not ExpandedLocalRect.Contains(aPos) then begin
    AControlPos := TPointF.Zero;
    exit(nil);
  end;
  //--
  var LScrollPos := LocalToScroll(aPos);
  for var i := ScrollFirstVisibleItemIdx to ScrollLastVisibleItemIdx do begin
    var LItem := Items[i];
    if not LItem.Visible then continue;
    if LItem.ExpandedBoundsRect.Contains(LScrollPos) then begin
      if not ADirectChildOnly then
        result := LItem.GetControlAtPos(
                    (LScrollPos - LItem.BoundsRect.TopLeft).ReducePrecision,
                    AControlPos,
                    ADirectChildOnly,
                    ACheckHitTest)
      else
        Result := nil;
      if Result = nil then begin
        IF ACheckHitTest and (not LItem.HitTest) then AControlPos := TPointF.Zero
        else begin
          result := LItem;
          AControlPos := (LScrollPos - LItem.BoundsRect.TopLeft).ReducePrecision;
        end;
      end;
      exit;
    end;
  end;
  //--
  IF ACheckHitTest and (not HitTest) then begin
    Result := Nil;
    AControlPos := TPointF.Zero
  end
  else begin
    result := Self;
    AControlPos := aPos;
  end;
end;

{****************************************************}
function TALDynamicScrollBoxView.GetMaxItems: Integer;
begin
  result := MaxInt;
end;

{*******************************************************************}
function TALDynamicScrollBoxView.CreateItem: TALDynamicScrollBoxItem;
begin
  If assigned(OwnerScrollBox.OnCreateItem) then
    Result := OwnerScrollBox.OnCreateItem(self)
  else
    result := TALDynamicScrollBoxItem.Create(self);
end;

{******************************************************************************}
procedure TALDynamicScrollBoxView.AddItem(const aItem: TALDynamicScrollBoxItem);
begin

  {$REGION 'init LInitialItemsCount'}
  var LInitialItemsCount := length(FItems);
  {$ENDREGION'}

  {$REGION 'init LInsertAt'}
  var LInsertAt: integer;
  if FItemsReversedOrientation then LInsertAt := 0
  else LInsertAt := LInitialItemsCount;
  LInsertAt := Min(LInsertAt, LInitialItemsCount);
  {$ENDREGION'}

  {$REGION 'update FItems'}
  setlength(FItems, LInitialItemsCount + 1);
  //--
  if LInsertAt < LInitialItemsCount then begin
    ALMove(FItems[LInsertAt], FItems[LInsertAt+1], (LInitialItemsCount - LInsertAt) * SizeOf(Pointer));
    for var I := LInsertAt+1 to High(FItems) do
      FItems[i].ItemIndex := FItems[i].ItemIndex + 1;
  end;
  //--
  AItem.ItemIndex := LInsertAt;
  FItems[AItem.ItemIndex] := AItem;
  //--
  if LInsertAt <= FScrollFirstVisibleItemIdx then inc(FScrollFirstVisibleItemIdx, 1);
  if LInsertAt <= FScrollLastVisibleItemIdx then inc(FScrollLastVisibleItemIdx, 1);
  if LInsertAt <= FScrollFirstPreloadedItemIdx then inc(FScrollFirstPreloadedItemIdx, 1);
  if LInsertAt <= FScrollLastPreloadedItemIdx then inc(FScrollLastPreloadedItemIdx, 1);
  {$ENDREGION'}

  {$REGION 'Arrange Items'}
  ArrangeItems(
    TArrangeItemsCause.ItemsInsert, // const ACause: TALDynamicScrollBoxView.TArrangeItemsCause
    LInsertAt, // const AFromIdx: integer;
    LInsertAt); // const AToIdx: integer;
  {$ENDREGION'}

end;

{********************************************}
procedure TALDynamicScrollBoxView.LockItemIds;
begin
  if FItemIDType = TItemIDType.Int64 then Tmonitor.enter(fItemInt64Ids)
  else Tmonitor.enter(fItemStrIds);
end;

{**********************************************}
procedure TALDynamicScrollBoxView.UnLockItemIds;
begin
  if FItemIDType = TItemIDType.Int64 then Tmonitor.exit(fItemInt64Ids)
  else Tmonitor.exit(fItemStrIds);
end;

{********************************************************************************}
function TALDynamicScrollBoxView.GetItem(Index: Integer): TALDynamicScrollBoxItem;
begin
  if (Index < 0) or (Index > High(FItems)) then raise Exception.Create('GetItem :: index is out of bounds');
  result := FItems[Index];
end;

{******************************************************}
function TALDynamicScrollBoxView.GetItemsCount: integer;
begin
  result := Length(FItems);
end;

{**********************************************************}
function TALDynamicScrollBoxView.GetNearestItemIdx: integer;
begin
  Result := -1;
  if ItemsOrientation = TOrientation.Vertical then begin
    var LFailCount := 0;
    var LHighestBottom: Double := maxint;
    for var i := Low(FItems) to High(FItems) do begin
      if Items[i].Visible then begin
        if compareValue(Items[i].BoundsRect.Bottom, LHighestBottom) < 0 then begin
          LHighestBottom := Items[i].BoundsRect.Bottom;
          result := i;
        end;
        inc(LFailCount);
        if LFailcount >= 2 then break;
      end
      else LFailcount := 0;
    end;
  end
  else begin
    for var i := Low(FItems) to High(FItems) do
      if Items[i].Visible then
        exit(i);
  end;
end;

{************************************************************}
function TALDynamicScrollBoxView.GetNearestItemBottom: Double;
begin
  var LNearestItemIdx := GetNearestItemIdx;
  if LNearestItemIdx > -1 then result := Items[LNearestItemIdx].BoundsRect.Bottom
  else result := 0;
end;

{***********************************************************}
function TALDynamicScrollBoxView.GetNearestItemRight: Double;
begin
  var LNearestItemIdx := GetNearestItemIdx;
  if LNearestItemIdx > -1 then result := Items[LNearestItemIdx].BoundsRect.Right
  else result := 0;
end;

{***********************************************************}
function TALDynamicScrollBoxView.GetFarthestItemIdx: integer;
begin
  Result := -1;
  if ItemsOrientation = TOrientation.Vertical then begin
    var LFailCount := 0;
    var LLowestBottom: Double := 0;
    for var i := High(FItems) downto Low(FItems) do begin
      if Items[i].Visible then begin
        if compareValue(Items[i].BoundsRect.Bottom, LLowestBottom) > 0 then begin
          LLowestBottom := Items[i].BoundsRect.Bottom;
          result := i;
        end;
        inc(LFailCount);
        if LFailcount >= 2 then break;
      end
      else LFailcount := 0;
    end;
  end
  else begin
    for var i := High(FItems) downto Low(FItems) do
      if Items[i].Visible then
        exit(i);
  end;
end;

{*************************************************************}
function TALDynamicScrollBoxView.GetFarthestItemBottom: Double;
begin
  var LFarthestItemIdx := GetFarthestItemIdx;
  if LFarthestItemIdx > -1 then result := Items[LFarthestItemIdx].BoundsRect.Bottom
  else result := 0;
end;

{************************************************************}
function TALDynamicScrollBoxView.GetFarthestItemRight: Double;
begin
  var LFarthestItemIdx := GetFarthestItemIdx;
  if LFarthestItemIdx > -1 then result := Items[LFarthestItemIdx].BoundsRect.Right
  else result := 0;
end;

{********************************************************}
function TALDynamicScrollBoxView.HasVisibleItems: Boolean;
begin
  Result := false;
  for var i := Low(FItems) to High(FItems) do
    if Items[i].Visible then
      exit(true);
end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxView.GrowItemsIsRunning: Boolean;
begin
  result := AtomiccmpExchange(FGrowItemsBackgroundThreadsCount{Target}, 0{NewValue}, 0{Comparand}) <> 0;
end;

{*****************************************}
function TALDynamicScrollBoxView.GrowItems(
           const aForceGrow: Boolean = False;
           const AInsertAt: integer = -1;
           const AItems: TALJSONNodeW = nil;
           const AAsync: Boolean = True): boolean;
begin

  {$IF defined(debug)}
  if (aForceGrow = false) and (AItems <> nil) then
    raise Exception.Create('In GrowItems ForceGrow can not be false when Items is not nil');
  {$ENDIF}

  //exit if the last download was in error OR we still not
  //reach the GrowItemsAtIdx except if aForceGrow = true (ex: button Reload click)
  if (not aForceGrow) and
     ((FGrowItemsError) or
      ((FItemsReversedOrientation) and
       (FScrollFirstVisibleItemIdx > FGrowItemsAtIdx)) or
      ((not FItemsReversedOrientation) and
       (FScrollLastVisibleItemIdx < FGrowItemsAtIdx))) then exit(false);

  //exit in anycase if their is no more Items available
  if (not FCanGrowItems) and (AItems = nil) then begin
    FGrowItemsExecutedAtLeastOnce := True;
    exit(false);
  end;

  //before going in background thread
  {$IFDEF DEBUG}
  ALLog(
    ClassName+'.GrowItems',
    'ForceGrow:' + ALBoolToStrW(aForceGrow) + ' | ' +
    'Items:' + ALIfThenW(AItems=nil, 'nil', '<Items>'),
    TalLogType.verbose);
  {$ENDIF}
  if AItems = nil then begin
    FGrowItemsError := False;
    if FItemsReversedOrientation then FGrowItemsAtIdx := -MaxInt
    else FGrowItemsAtIdx := MaxInt;
  end;

  //load the Items in other thread to not block the calling thread
//*  if not OwnerScrollBox.IncrementWaitForCount then exit(false);
  AtomicIncrement(FGrowItemsBackgroundThreadsCount);
  var LGrowItemsBackgroundThread := GrowItemsCreateBackgroundThread(AInsertAt, AItems);
  if AAsync then LGrowItemsBackgroundThread.Start
  else begin
    try
      LGrowItemsBackgroundThread.execute;
    finally
      LGrowItemsBackgroundThread.Terminate;
      LGrowItemsBackgroundThread.Start;
    end;
  end;
  result := true;

end;

{***************************************************************}
function TALDynamicScrollBoxView.GrowItemsCreateBackgroundThread(
           const AInsertAt: integer;
           const AItems: TALJSONNodeW): TALDynamicScrollBoxView.TGrowItemsBackgroundThread;
begin
  result := TGrowItemsBackgroundThread.create(
              self, // const AOwner: TALDynamicScrollBoxView;
              AInsertAt, // const AInsertAt: integer;
              AItems, // const AItems: TALJSONNodeW);
              MaxItems); // const AMaxItems: integer - we need this param because MaxItems is not tagged [MultiThread]
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxView.GrowItemsBackgroundProc(const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread);
begin

  {$REGION 'exit if destroying'}
  //*if OwnerScrollBox.isdestroying then exit;
  {$ENDREGION'}

  {$REGION 'wait a GeoPosition fix'}
  if OwnerScrollBox.WaitGeoPosition then
    OwnerScrollBox.FWaitGeoPositionEvent.WaitFor(INFINITE);
  {$ENDREGION}

  {$REGION 'Create local objects and lock the thread'}
  var LItemsDoc := TALJSONDocumentW.create;
  {$ENDREGION}

  try

    {$REGION 'Download the Items'}
    var LDownloadItemsIsSuccessful: Boolean := True;
    if AOwnerThread.Items <> nil then begin
      while AOwnerThread.Items.ChildNodes.Count > 0 do
        LItemsDoc.ChildNodes.Add(AOwnerThread.Items.ChildNodes.Extract(0));
    end
    else begin
      var LSavedDownloadItemsNextPageToken := FDownloadItemsNextPageToken;
      if FDownloadItemsNextPageToken = '' then raise Exception.Create('Error 4762A2BF-0ED5-439A-85C3-E8CDCD5E00F1');
      if FDownloadItemsNextPageToken = #0(*vWin_NullValueU*) then FDownloadItemsNextPageToken := '';
      LDownloadItemsIsSuccessful := GrowItemsBackgroundProcDownloadItems(
                                      AOwnerThread,
                                      LItemsDoc,
                                      FDownloadItemsNextPageToken);
      if not LDownloadItemsIsSuccessful then begin
        FDownloadItemsNextPageToken := LSavedDownloadItemsNextPageToken;
        LItemsDoc.ChildNodes.Clear;
      end;
    end;
    {$ENDREGION}

    {$REGION 'update fItemInt64Ids/fItemStrIds'}
    LockItemIds;
    try
      //*if OwnerScrollBox.isdestroying then exit;
      var Lcount: integer;
      if FItemIDType = TItemIDType.Int64 then LCount := fItemInt64Ids.Count
      else LCount := fItemStrIds.Count;
      //-----
      for var i := LItemsDoc.ChildNodes.Count - 1 downto 0 do begin
        //-----
        if Lcount >= AOwnerThread.MaxItems then break;
        //-----
        var LItemNode := LItemsDoc.ChildNodes[i];
        if FItemIDType = TItemIDType.Int64 then begin
          var LItemId := LItemNode.getChildNodeValueInt64(FItemIDPath, 0);
          if not GrowItemsBackgroundProcCanAddItem(LItemId) then begin
            LItemsDoc.ChildNodes.Delete(i);
            continue;
          end;
        end
        else begin
          var LItemId := LItemNode.getChildNodeValueText(FItemIDPath, '');
          if not GrowItemsBackgroundProcCanAddItem(LItemId) then begin
            LItemsDoc.ChildNodes.Delete(i);
            continue;
          end;
        end;
        //-----
        inc(LCount);
        GrowItemsBackgroundProcInitItem(LItemNode);
        //-----
      end;
    finally
      UnLockItemIds
    end;
    {$ENDREGION}

    {$REGION 'Wait that we can do some jobs in the main thread'}
    while not GrowItemsBackgroundProcCanProcessItems do
      sleep(250);
    {$ENDREGION}

    {$REGION 'call GrowItemsProcessItems'}
    //*if OwnerScrollBox.isdestroying then exit;
    TThread.Synchronize(nil,
      procedure
      begin
        GrowItemsProcessItems(
          AOwnerThread,
          LDownloadItemsIsSuccessful,
          LItemsDoc);
      end);
    {$ENDREGION}

    {$REGION 'Stop the Progress Animation'}
    //The ProgressFrameStopAnimationEvent is used ONLY with SINGLE view DynamicScrollBoxFrame
    //and used only in GrowItemsBackgroundProc (not in GrowDataBackgroundProc). I do
    //like this because I do not want to lock the access below.
    //*if OwnerScrollBox.isdestroying then exit;
    //*if (OwnerScrollBox.ProgressFrameStopAnimationEvent <> nil) then begin
    //*  TThread.Synchronize(nil,OwnerScrollBox.StopProgressFrameAnimation);
    //*  if OwnerScrollBox.isdestroying then exit;
    //*  OwnerScrollBox.ProgressFrameStopAnimationEvent.WaitFor(INFINITE);
    //*end;
    {$ENDREGION}

    {$REGION 'Finish'}
    //*if OwnerScrollBox.isdestroying then exit;
    TThread.Synchronize(nil,
      procedure
      begin
        GrowItemsFinish(
          AOwnerThread,
          LDownloadItemsIsSuccessful);
      end);
    {$ENDREGION}

  finally

    {$REGION 'Free local objects and unlock the thread'}
    ALfreeAndNil(LItemsDoc);
    {$ENDREGION}

  end;

end;

{*************}
//[MultiThread]
function TALDynamicScrollBoxView.GrowItemsBackgroundProcDownloadItems(
           const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread;
           const aResultNode: TALJSONNodeW;
           var aNextPageToken: String): Boolean;
begin
  result := False;
  If assigned(AOwnerThread.FOwner.OwnerScrollBox.OnLoadItems) then
    AOwnerThread.FOwner.OwnerScrollBox.OnLoadItems(
      AOwnerThread.FOwner, // Const AView: TALDynamicScrollBoxView;
      aResultNode, // const AItems: TALJSONNodeW;
      aNextPageToken, // var APaginationToken: String;
      Result); // var AIsSuccessful: Boolean)
end;

{*************}
//[MultiThread]
function  TALDynamicScrollBoxView.GrowItemsBackgroundProcCanAddItem(const aItemID: int64): boolean;
begin
  //LockItemIds is already set
  result := (aItemID <> 0) and (fItemInt64Ids.TryAdd(aItemID, true));
end;

{*************}
//[MultiThread]
function  TALDynamicScrollBoxView.GrowItemsBackgroundProcCanAddItem(const aItemID: String): boolean;
begin
  //LockItemIds is already set
  result := (aItemId <> '') and (fItemStrIds.TryAdd(aItemId, true));
end;

{*************}
//[MultiThread]
procedure TALDynamicScrollBoxView.GrowItemsBackgroundProcInitItem(const aItemNode: TALJSONNodeW);
begin
  aItemNode.ChildNodes.SetSorted(true{Value},true{recurse});
  aItemNode.MultiThreadPrepare(true{aOnlyChildList});
end;

{*************}
//[MultiThread]
function  TALDynamicScrollBoxView.GrowItemsBackgroundProcCanProcessItems: boolean;
begin
  //mostly because we don't want to list to be updated when we are in the
  //bottom bound animation
  result := //*(OwnerScrollBox.isdestroying) or
            (TThread.Current.ThreadID = MainThreadID) or
            ((not OwnerScrollBox.HasActiveScrollEngines)); //*and
            //* (not OwnerScrollBox.IsInTransition));
  //wait that the growdata is finished because later we will need to
  //create the EphemeralLayout (via ArrangeItems) to know how to
  //arrange the Items
  if result then result := IsGrowDataFinished;
end;

{******************************************************}
procedure TALDynamicScrollBoxView.GrowItemsProcessItems(
            const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread;
            const AItemsLoadedSuccessfully: Boolean;
            const ADownloadedItems: TALJSONNodeW);
begin

  {$REGION 'Add some Log'}
  {$IFDEF DEBUG}
  var LStopWatch := TstopWatch.startNew;
  try
  {$ENDIF}
  {$ENDREGION'}

  {$REGION 'exit if destroying'}
  //*if OwnerScrollBox.isdestroying then exit;
  {$ENDREGION'}

  {$REGION 'init FGrowItemsError/FCanGrowItems'}
  if AOwnerThread.Items = nil then begin
    FGrowItemsError := not AItemsLoadedSuccessfully;
    if FGrowItemsError then FCanGrowItems := true
    else begin
      LockItemIds;
      try
        if FItemIDType = TItemIDType.Int64 then FCanGrowItems := (FDownloadItemsNextPageToken <> '') and (fItemInt64Ids.Count < AOwnerThread.MaxItems)
        else FCanGrowItems := (FDownloadItemsNextPageToken <> '') and (fItemStrIds.Count < AOwnerThread.MaxItems);
      finally
        UnLockItemIds;
      end;
    end;
  end;
  {$ENDREGION'}

  {$REGION 'init LInitialItemsCount/LDownloadedItemsCount'}
  var LInitialItemsCount := length(FItems);
  var LDownloadedItemsCount := ADownloadedItems.ChildNodes.Count;
  {$ENDREGION'}

  {$REGION 'init LInsertAt'}
  var LInsertAt: integer;
  if AOwnerThread.InsertAt > -1 then LInsertAt := AOwnerThread.InsertAt
  else if FItemsReversedOrientation then LInsertAt := 0
  else LInsertAt := LInitialItemsCount;
  LInsertAt := Min(LInsertAt, LInitialItemsCount);
  {$ENDREGION'}

  {$REGION 'update FItems'}
  if LDownloadedItemsCount > 0 then begin
    setlength(FItems, LInitialItemsCount + LDownloadedItemsCount);
    //-----
    if LInsertAt < LInitialItemsCount then begin
      ALMove(FItems[LInsertAt], FItems[LInsertAt+LDownloadedItemsCount], (LInitialItemsCount - LInsertAt) * SizeOf(Pointer));
      for var I := LInsertAt+LDownloadedItemsCount to High(FItems) do
        FItems[i].ItemIndex := FItems[i].ItemIndex + LDownloadedItemsCount;
    end;
    //--
    for var i := LDownloadedItemsCount - 1 downto 0 do begin
      var LItem := CreateItem;
      if FItemsReversedOrientation then LItem.ItemIndex := LInsertAt + LDownloadedItemsCount - 1 - i
      else LItem.ItemIndex := LInsertAt + i;
      LItem.data := ADownloadedItems.ChildNodes.Extract(I);
      FItems[LItem.ItemIndex] := LItem;
    end;
    //--
    if LInsertAt <= FScrollFirstVisibleItemIdx then inc(FScrollFirstVisibleItemIdx, LDownloadedItemsCount);
    if LInsertAt <= FScrollLastVisibleItemIdx then inc(FScrollLastVisibleItemIdx, LDownloadedItemsCount);
    if LInsertAt <= FScrollFirstPreloadedItemIdx then inc(FScrollFirstPreloadedItemIdx, LDownloadedItemsCount);
    if LInsertAt <= FScrollLastPreloadedItemIdx then inc(FScrollLastPreloadedItemIdx, LDownloadedItemsCount);
  end;
  {$ENDREGION'}

  {$REGION 'Arrange Items'}
  ArrangeItems(
    TArrangeItemsCause.ItemsInsert, // const ACause: TALDynamicScrollBoxView.TArrangeItemsCause
    LInsertAt, // const AFromIdx: integer;
    LInsertAt + LDownloadedItemsCount - 1); // const AToIdx: integer;
  {$ENDREGION'}

  {$REGION 'Init FGrowItemsAtIdx'}
  if AOwnerThread.Items = nil then begin
    if FItemsReversedOrientation then begin
      if FCanGrowItems then FGrowItemsAtIdx := max(GetNearestItemIdx, round(LDownloadedItemsCount / 2))
      else FGrowItemsAtIdx := -maxint;
    end
    else begin
      if FCanGrowItems then FGrowItemsAtIdx := Min(GetFarthestItemIdx, LInsertAt + LDownloadedItemsCount - round(LDownloadedItemsCount / 2))
      else FGrowItemsAtIdx := maxint;
    end;
  end;
  {$ENDREGION'}

  {$REGION 'New Item anim/vibrate'}
  if (AOwnerThread.Items <> nil) and // << this mean we process a notification
     (LDownloadedItemsCount = 1) then begin // because all items in AOwnerThread.Items could have been filtered

    //Init the notification item
    var LItem := Items[LInsertAt];

    //start the anim
    if CanDoNewItemAnim(LItem) then begin
      //*TALMainForm.Instance.ShowPopupLoading(
      //*  False, //const StartAnimation: boolean = False;
      //*  TAlphaColorRec.null, // const OverlayColor: TalphaColor = TAlphaColorRec.null; // $64000000 for black like popupdialog
      //*  true); //const JustOverlay: Boolean = False);
      CreateNewItemAnim;
      NewItemAnim.Tag := LInsertAt;
      NewItemAnim.Enabled := true;
    end;

    //vibrate
    if CanDoNewItemVibrate(LItem) then begin
      {$IF defined(ANDROID)}
      var LVibratorServiceNative := TAndroidHelper.Context.getSystemService(TJContext.JavaClass.VIBRATOR_SERVICE);
      var LVibrator := TJVibrator.Wrap((LVibratorServiceNative as ILocalObject).GetObjectID);
      var LPattern := TJavaArray<Int64>.create(4);
      try
        LPattern[0] := 0;
        LPattern[1] := 150;
        LPattern[2] := 200;
        LPattern[3] := 300;
        LVibrator.Vibrate(LPattern, -1);
      finally
        AlFreeAndNil(LPattern);
      end;
      {$ELSEIF defined(IOS)}
      AudioServicesPlayAlertSound(kSystemSoundID_Vibrate);
      {$ENDIF}
    end;

  end;
  {$ENDREGION'}

  {$REGION 'Add some Log'}
  {$IFDEF DEBUG}
  finally
    LStopWatch.Stop;
    ALLog(ClassName+'.GrowItemsProcessItems', 'timeTaken: ' + ALFloatToStrW(LStopWatch.Elapsed.TotalMilliseconds, ALDefaultFormatSettingsW), TalLogType.verbose);
  end;
  {$ENDIF}
  {$ENDREGION'}

end;

{************************************************}
procedure TALDynamicScrollBoxView.GrowItemsFinish(
            const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread;
            const AItemsLoadedSuccessfully: Boolean);
begin

  //do nothing if we are destroying the form
  //*if OwnerScrollBox.isdestroying then exit;

  //If we encounter an Error AND if ShowApiErrorIfGrowItemsError
  //and if it's the very first time we call GrowItems then
  //then call ShowApiError
  if (not FGrowItemsExecutedAtLeastOnce) and
     (FGrowItemsError) and
     (ShowApiErrorIfGrowItemsError) then begin
    FGrowItemsExecutedAtLeastOnce := True;
    //*OwnerScrollBox.ShowApiError(ADownloadItemsReturnCode)
  end

  //call repaint
  else begin
    //if ItemsCount = 0 then it's the NoItemsBtn that must
    //show to the end user to activate it's GPS
    if (OwnerScrollBox.FNeedGeoPosition) and
       (not FGrowItemsExecutedAtLeastOnce) and
       //*(not TALGeoPositionSensor.instance.hasLastGeoPosition) and
       (ItemsCount > 0) then OwnerScrollBox.createGPSRequiredMessageBannerAnim;
    FGrowItemsExecutedAtLeastOnce := True;
    //*OwnerScrollBox.RemoveProgressFrame;
    if (FGrowItemsError) and
       (AOwnerThread.Items = nil) then
      OwnerScrollBox.createErrorMessageBannerAnim;
  end;

end;

{**************************************************************************************************************************}
procedure TALDynamicScrollBoxView.GrowItemsFinished(const AOwnerThread: TALDynamicScrollBoxView.TGrowItemsBackgroundThread);
begin
  if (not FGrowItemsError) and
     (AOwnerThread.Items = nil) then GrowItems
  else ProcessNewItemQueue;
  OwnerScrollBox.repaint;
end;

{*********************************************************************************}
procedure TALDynamicScrollBoxView.DeleteItem(Const aItem: TALDynamicScrollBoxItem);
begin
  if aItem.HasData then begin
    LockItemIds;
    try
      if FItemIDType = TItemIDType.Int64 then begin
        var LItemId := aItem.Data.getChildNodeValueInt64(FItemIDPath, 0);
        if LItemId <> 0 then fItemInt64Ids.Remove(LItemId);
      end
      else begin
        var LItemId := aItem.Data.getChildNodeValueText(FItemIDPath, '');
        if LItemId <> '' then fItemStrIds.Remove(LItemId);
      end;
    finally
      UnLockItemIds
    end;
  end;
  aItem.clearEphemeralLayout;
  aItem.Selected := False;
  aItem.FDeleted := True;
  aItem.Visible := False;
  ArrangeItems(
    TArrangeItemsCause.ItemDelete, // const ACause: TALDynamicScrollBoxView.TArrangeItemsCause
    aItem.ItemIndex, // const AFromIdx: integer;
    aItem.ItemIndex); // const AToIdx: integer;
  //*OwnerScrollBox.InitActionBar;
  OwnerScrollBox.repaint;
end;

{*********************************************}
procedure TALDynamicScrollBoxView.ArrangeItems(
            const ACause: TALDynamicScrollBoxView.TArrangeItemsCause;
            const AFromIdx: integer;
            const AToIdx: integer);
begin

  //to hide a stupid warning bug
  if IsArrangingItems then exit;
  if EphemeralLayout = nil then exit;
  var LGluedItem: TALDynamicScrollBoxItem := nil;
  Var LDeltaScrollPos: Double := 0;

  //init LGluedItem and LDeltaScrollPos
  if AToIdx >= AFromIdx then begin

    {$REGION 'init LHadVisibleItems'}
    var LHadVisibleItems := false;
    if (ACause = TArrangeItemsCause.ItemsInsert) then begin
      for var i := Low(FItems) to High(FItems) do
        if ((i < AFromIdx) or (i > AToIdx)) and
           (Items[i].Visible) then begin
          LHadVisibleItems := True;
          Break;
        end;
    end
    else if ACause in [TArrangeItemsCause.ItemDelete, TArrangeItemsCause.ItemResize, TArrangeItemsCause.Other] then
      LHadVisibleItems := HasVisibleItems
    else
      raise Exception.Create('Error B786B5F1-936A-4E1E-B157-B54E99196E14');
    {$ENDREGION}

    {$REGION 'ItemsReversedOrientation'}
    if ItemsReversedOrientation then begin

      {$REGION 'Not Had visible items'}
      if Not LHadVisibleItems then begin
        //In case of insert, if they was no visible items then move the
        //scrollpos the the very last item
        if (ACause = TArrangeItemsCause.ItemsInsert) and
           (AToIdx >= Low(FItems)) and
           (AToIdx <= High(FItems)) then begin
          LGluedItem := Fitems[AToIdx];
          LDeltaScrollPos := 0;
        end
        //else do not touch the scrollpos, their is still no visible
        //items so nothing to do
        else if ACause in [TArrangeItemsCause.ItemDelete, TArrangeItemsCause.ItemResize, TArrangeItemsCause.Other] then begin
          //LGluedItem := nil;
          //LDeltaScrollPos := 0;
        end
        else
          raise Exception.Create('Error 6C74482C-2141-4323-92D8-9342D2ECE265');
      end
      {$ENDREGION}

      {$REGION 'We are at the max target'}
      //*else if (ItemsOrientation = TOrientation.Vertical) and (CompareValue(ScrollPos, ScrollEngine.MaxScrollLimit.y, TALTouchEventTracking.MinDistanceThreshold) >= 0) or
      //*        (ItemsOrientation = TOrientation.horizontal) and (CompareValue(ScrollPos, ScrollEngine.MaxScrollLimit.x, TALTouchEventTracking.MinDistanceThreshold) >= 0) then begin
      //*  if (ACause = TArrangeItemsCause.ItemsInsert) and
      //*     (AToIdx > ScrollLastVisibleItemIdx) and
      //*     (AToIdx >= Low(FItems)) and
      //*     (AToIdx <= High(FItems)) then LGluedItem := Fitems[AToIdx]
      //*  else LGluedItem := Fitems[ScrollLastVisibleItemIdx];
      //*  LDeltaScrollPos := 0;
      //*end
      {$ENDREGION}

      {$REGION 'We are resizing items'}
      else if (ACause = TArrangeItemsCause.ItemResize) then begin
        //if we are resizing items, glue the last Visible Item so that we will see
        //any items being inserted/deleted/resized before this first visible Item
        if (ScrollLastVisibleItemIdx >= Low(FItems)) and (ScrollLastVisibleItemIdx <= High(FItems)) then begin
          LGluedItem := FItems[ScrollLastVisibleItemIdx];
          if ItemsOrientation = TOrientation.Vertical then LDeltaScrollPos := LGluedItem.BoundsRect.bottom - ScrollPos
          else LDeltaScrollPos := LGluedItem.BoundsRect.right - ScrollPos;
        end;
      end
      {$ENDREGION}

      {$REGION 'All other cases'}
      else begin

        //For all other cases, glue the first Visible Item so that we will not see
        //the items being inserted/deleted/resized before this first visible Item
        var LScrollFirstVisibleItemIdx: integer;
        //if we delete some items then we must recalculate the ScrollFirstVisibleItemIdx
        //because we could have deleted the first visible item.
        if (ACause = TArrangeItemsCause.ItemDelete) then LScrollFirstVisibleItemIdx := CalculateScrollFirstVisibleItemIdx
        else LScrollFirstVisibleItemIdx := ScrollFirstVisibleItemIdx;
        if (LScrollFirstVisibleItemIdx >= Low(FItems)) and (LScrollFirstVisibleItemIdx <= High(FItems)) then begin
          LGluedItem := FItems[LScrollFirstVisibleItemIdx];
          if (LScrollFirstVisibleItemIdx < ScrollFirstVisibleItemIdx) or (LScrollFirstVisibleItemIdx > ScrollLastVisibleItemIdx) then LDeltaScrollPos := 0
          else if ItemsOrientation = TOrientation.Vertical then LDeltaScrollPos := LGluedItem.BoundsRect.bottom - ScrollPos
          else LDeltaScrollPos := LGluedItem.BoundsRect.right - ScrollPos;
        end;

      end;
      {$ENDREGION}

    end
    {$ENDREGION}

    {$REGION 'NOT ItemsReversedOrientation'}
    else begin

      {$REGION 'Not Had visible items'}
      if Not LHadVisibleItems then begin
        //if they was no visible items then do not touch
        //the scrollpos that must be egual to 0 so that
        //we can see the new items inserted
        //LGluedItem := nil;
        //LDeltaScrollPos := 0;
      end
      {$ENDREGION}

      {$REGION 'We are at the min target'}
      //*else if (ItemsOrientation = TOrientation.Vertical) and (CompareValue(ScrollPos, ScrollEngine.MinScrollLimit.y, TALTouchEventTracking.MinDistanceThreshold) <= 0) or
      //*        (ItemsOrientation = TOrientation.horizontal) and (CompareValue(ScrollPos, ScrollEngine.MinScrollLimit.x, TALTouchEventTracking.MinDistanceThreshold) <= 0) then begin
      //*  //if we are at the top of the scrolling area then
      //*  //do not touch the scrollpos so that we can see
      //*  //the items being inserted/deleted/resized
      //*  //LGluedItem := nil;
      //*  //LDeltaScrollPos := 0;
      //*end
      {$ENDREGION}

      {$REGION 'All other cases'}
      else begin

        //For all other cases, glue the first Visible Item so that we will not see
        //the items being inserted/deleted/resized before this first visible Item
        var LScrollFirstVisibleItemIdx: integer;
        //if we delete some items then we must recalculate the ScrollFirstVisibleItemIdx
        //because we could have deleted the first visible item.
        if (ACause = TArrangeItemsCause.ItemDelete) then LScrollFirstVisibleItemIdx := CalculateScrollFirstVisibleItemIdx
        else LScrollFirstVisibleItemIdx := ScrollFirstVisibleItemIdx;
        if (LScrollFirstVisibleItemIdx >= Low(FItems)) and (LScrollFirstVisibleItemIdx <= High(FItems)) then begin
          LGluedItem := FItems[LScrollFirstVisibleItemIdx];
          if (LScrollFirstVisibleItemIdx < ScrollFirstVisibleItemIdx) or (LScrollFirstVisibleItemIdx > ScrollLastVisibleItemIdx) then LDeltaScrollPos := 0
          else if ItemsOrientation = TOrientation.Vertical then LDeltaScrollPos := LGluedItem.BoundsRect.top - ScrollPos
          else LDeltaScrollPos := LGluedItem.BoundsRect.left - ScrollPos;
        end;

      end;
      {$ENDREGION}

    end;
    {$ENDREGION}

  end;

  //DoArrangeItems
  if AToIdx >= AFromIdx then begin
    FIsArrangingItems := True;
    try
      DoArrangeItems(ACause, AFromIdx, AToIdx);
    finally
      FIsArrangingItems := False;
    end;
  end;

  //AfterItemsArranged
  if (LGluedItem <> nil) and
     (LGluedItem.Visible) then begin
    var LNewScrollPosValue: Double;
    if ItemsReversedOrientation then begin
      if ItemsOrientation = TOrientation.Vertical then LNewScrollPosValue := LGluedItem.BoundsRect.bottom - LDeltaScrollPos
      else LNewScrollPosValue := LGluedItem.BoundsRect.right - LDeltaScrollPos;
    end
    else begin
      if ItemsOrientation = TOrientation.Vertical then LNewScrollPosValue := LGluedItem.BoundsRect.top - LDeltaScrollPos
      else LNewScrollPosValue := LGluedItem.BoundsRect.left - LDeltaScrollPos;
    end;
    AfterItemsArranged(
      true, // Const AAlterScrollPosValue: Boolean = False;
      LNewScrollPosValue); // const ANewScrollPosValue: Double = 0
  end
  else
    AfterItemsArranged;

end;

{***********************************************}
procedure TALDynamicScrollBoxView.DoArrangeItems(
            const ACause: TALDynamicScrollBoxView.TArrangeItemsCause;
            const AFromIdx: integer;
            const AToIdx: integer);
begin
  {$IFDEF DEBUG}
  ALLog(
    ClassName+'.DoArrangeItems',
    'FromIdx: ' + ALIntToStrW(AFromIdx) + ' | ' +
    'ToIdx: ' + ALIntToStrW(AToIdx) + ' | ' +
    'Cause: ' + TRttiEnumerationType.GetName(ACause),
    TalLogType.verbose);
  {$ENDIF}
  If Assigned(OwnerScrollBox.OnAlignItems) then
    OwnerScrollBox.OnAlignItems(Self, AFromIdx, AToIdx)
  else begin
    var LOwnerScrollBox := OwnerScrollBox;
    var LCurrY: double;
    if AFromIdx <= 0 then LCurrY := 0 //* GetActionBarHeight + LOwnerScrollBox.FirstActivitiesItemMarginTop
    else LCurrY := Items[AFromIdx - 1].BoundsRect.bottom;
    for var I := AFromIdx to ItemsCount - 1 do begin
      Items[i].BoundsRect := TALRectD.Create(
                               TALPointD.Create(0, LCurrY),
                               LOwnerScrollBox.Width,
                               Items[i].BoundsRect.Height); //Items[i].rect.height will be set via the Items[i].shiftEphemeralLayout procedure
      LCurrY := LCurrY + Items[i].BoundsRect.Height;
    end;
  end;
end;

{********************************}
//[MultiThread][BeginRead/EndRead]
function TALDynamicScrollBoxView.doBuildEphemeralLayout: TALDynamicScrollBoxItemEphemeralLayout;
begin
  //Normally we do not need to protect the access to primitive types (like Rect, etc.)
  //because when we change it (like Rect) then we will also increment the Key that will
  //defacto release the EphemeralLayout in ShiftEphemeralLayout
  if Assigned(OwnerScrollBox.OnCreateLayout) then
    Result := OwnerScrollBox.OnCreateLayout(Self)
  else
    result := TALDynamicScrollBoxViewEphemeralLayout.Create(self);
end;

{******************************************************************************************}
function TALDynamicScrollBoxView.GetEphemeralLayout: TALDynamicScrollBoxViewEphemeralLayout;
begin
  result := TALDynamicScrollBoxViewEphemeralLayout(inherited EphemeralLayout)
end;

{************************************************************************************************************************}
function TALDynamicScrollBoxView.EnforceEphemeralLayout(const a4Preload: boolean): TALDynamicScrollBoxViewEphemeralLayout;
begin
  result := TALDynamicScrollBoxViewEphemeralLayout(inherited EnforceEphemeralLayout(a4Preload));
end;

{**********************************************************************************************}
function TALDynamicScrollBoxView.EnforceEphemeralLayout: TALDynamicScrollBoxViewEphemeralLayout;
begin
  result := TALDynamicScrollBoxViewEphemeralLayout(inherited EnforceEphemeralLayout);
end;

{*****************************************************}
procedure TALDynamicScrollBoxView.clearEphemeralLayout;
begin
  inherited clearEphemeralLayout;
  for var I := Low(FItems) to High(FItems) do
    FItems[i].clearEphemeralLayout;
end;

{***************************************************}
procedure TALDynamicScrollBoxView.AfterItemsArranged(
            const AAlterScrollPosValue: Boolean = False;
            const ANewScrollPosValue: Double = 0);
begin

  //AfterItemsArranged via the overriden function
  DoAfterItemsArranged;

  var LMaxScrollLimit: Double;
  if ItemsOrientation = TOrientation.Vertical then begin
    LMaxScrollLimit := max(0, FScrollMaxBound - BoundsRect.Height);
    ScrollEngine.SetScrollLimits(
      TALPointD.Create(0, FScrollMinBound),
      TALPointD.Create(0, LMaxScrollLimit),
      false{EnforceLimits}); // The boundaries will be applied in the subsequent SetScrollPos
    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    var LPrevOnChange := OwnerScrollBox.FscrollBar.OnChange;
    OwnerScrollBox.FscrollBar.OnChange := nil;
    Try
      OwnerScrollBox.FscrollBar.Height := BoundsRect.Height; // << this because of affineRatio if the size of the windows change
      OwnerScrollBox.FscrollBar.Position.Point := TpointF.Create(OwnerScrollBox.width - OwnerScrollBox.scrollBar.Width, 0);
      OwnerScrollBox.FscrollBar.ViewportSize := BoundsRect.Height;
      OwnerScrollBox.FscrollBar.Min := FScrollMinBound;
      OwnerScrollBox.FscrollBar.Max := LMaxScrollLimit + BoundsRect.Height;
      OwnerScrollBox.FscrollBar.Tag := compareValue(LMaxScrollLimit, 0, TEpsilon.Position);
      OwnerScrollBox.FscrollBar.visible := OwnerScrollBox.scrollBar.Tag <> 0;
    finally
      OwnerScrollBox.ScrollBar.OnChange := LPrevOnChange;
    end;
    {$ENDIF}
  end
  else begin
    LMaxScrollLimit := max(0, FScrollMaxBound - BoundsRect.Width);
    ScrollEngine.SetScrollLimits(
      TALPointD.Create(FScrollMinBound, 0),
      TALPointD.Create(LMaxScrollLimit, 0),
      false{EnforceLimits}); // The boundaries will be applied in the subsequent SetScrollPos
  end;

  // Preload items and enforce boundaries
  if AAlterScrollPosValue then
    SetScrollPos(ANewScrollPosValue, True{AEnforceScrollLimits})
  else
    SetScrollPos(ScrollPos, False{AEnforceScrollLimits});

end;

{*****************************************************}
procedure TALDynamicScrollBoxView.DoAfterItemsArranged;
begin

  if EphemeralLayout = nil then begin
    if ItemsOrientation = TOrientation.Vertical then begin
      ScrollMinBound := 0;
      ScrollMaxBound := GetFarthestItemBottom;
    end
    else begin
      ScrollMinBound := 0;
      ScrollMaxBound := GetFarthestItemRight;
    end;
    exit;
  end;
  var LEphemeralLayout := EphemeralLayout;
  //-----
  ScrollMinBound := 0;
  ScrollMaxBound := 0;
  //-----
  LEphemeralLayout.GrowItemsAniindicatorPos := TAlPointD.create(-1000,-1000);
  LEphemeralLayout.ItemsReloadBtnPos := TAlPointD.create(-1000,-1000);
  LEphemeralLayout.ItemsReloadBtnTouchTargetRect := TalRectD.create(0,0,0,0);
  //-----
  LEphemeralLayout.NoItemsIcon := false;
  LEphemeralLayout.NoItemsIconTouchTargetRect := TALRectD.create(0,0,0,0);
  LEphemeralLayout.NoItemsTitle := false;
  LEphemeralLayout.NoItemsTitleTouchTargetRect := TALRectD.create(0,0,0,0);
  LEphemeralLayout.NoItemsSubTitle := false;
  LEphemeralLayout.NoItemsSubTitleTouchTargetRect := TALRectD.create(0,0,0,0);
  LEphemeralLayout.NoItemsBtn := false;
  LEphemeralLayout.NoItemsBtnTouchTargetRect := TALRectD.create(0,0,0,0);

  if ItemsOrientation = TOrientation.Vertical then begin

    {$REGION 'GrowItemsError'}
    if GrowItemsError then begin

      {$REGION 'ItemsReversedOrientation'}
      if ItemsReversedOrientation then begin
        LEphemeralLayout.ItemsReloadBtnPos := TalPointD.Create(
                                                (BoundsRect.Width - ItemsReloadBtnWidth) / 2,
                                                0 - ItemsReloadBtnMarginBottom - ItemsReloadBtnHeight);
        LEphemeralLayout.GrowItemsAniindicatorPos := TalPointD.Create(
                                                       (BoundsRect.Width (*- TALMainForm.Instance.AniindicatorWidth*)) / 2,
                                                       0 - GrowItemsAniindicatorMarginBottom (*- TALMainForm.Instance.AniindicatorHeight*));
        ScrollMinBound := LEphemeralLayout.ItemsReloadBtnPos.y -
                          ItemsReloadBtnMarginTop;
        ScrollMaxBound := GetFarthestItemBottom;
      end
      {$ENDREGION}

      {$REGION 'NO ItemsReversedOrientation'}
      else begin
        LEphemeralLayout.ItemsReloadBtnPos := TalPointD.Create(
                                                (BoundsRect.Width - ItemsReloadBtnWidth) / 2,
                                                GetFarthestItemBottom + ItemsReloadBtnMarginTop);
        LEphemeralLayout.GrowItemsAniindicatorPos := TalPointD.Create(
                                                       (BoundsRect.Width (*- TALMainForm.Instance.AniindicatorWidth*)) / 2,
                                                       GetFarthestItemBottom + GrowItemsAniindicatorMarginTop);
        ScrollMaxBound := LEphemeralLayout.ItemsReloadBtnPos.y +
                          ItemsReloadBtnHeight +
                          ItemsReloadBtnMarginBottom;
      end;
      {$ENDREGION}

      LEphemeralLayout.ItemsReloadBtnTouchTargetRect := TalRectD.create(
                                                          LEphemeralLayout.ItemsReloadBtnPos - tAlPointD.Create(OwnerScrollBox.TouchTargetExpansion,OwnerScrollBox.TouchTargetExpansion),
                                                          ItemsReloadBtnWidth + (OwnerScrollBox.TouchTargetExpansion*2),
                                                          ItemsReloadBtnHeight + (OwnerScrollBox.TouchTargetExpansion*2));

    end
    {$ENDREGION}

    {$REGION 'CanGrowItems'}
    else if CanGrowItems then begin

      {$REGION 'ItemsReversedOrientation'}
      if ItemsReversedOrientation then begin
        LEphemeralLayout.GrowItemsAniindicatorPos := TalPointD.Create(
                                                       (BoundsRect.Width (*- TALMainForm.Instance.AniindicatorWidth*)) / 2,
                                                       0 - GrowItemsAniindicatorMarginBottom (*- TALMainForm.Instance.AniindicatorHeight*));
        ScrollMinBound := LEphemeralLayout.GrowItemsAniindicatorPos.Y -
                          GrowItemsAniindicatorMarginTop;
        ScrollMaxBound := GetFarthestItemBottom;
      end
      {$ENDREGION}

      {$REGION 'NO ItemsReversedOrientation'}
      else begin
        LEphemeralLayout.GrowItemsAniindicatorPos := TalPointD.Create(
                                                       (BoundsRect.Width (*- TALMainForm.Instance.AniindicatorWidth*)) / 2,
                                                       GetFarthestItemBottom + GrowItemsAniindicatorMarginTop);
        ScrollMaxBound := LEphemeralLayout.GrowItemsAniindicatorPos.Y +
                          //*TALMainForm.Instance.AniindicatorHeight +
                          GrowItemsAniindicatorMarginBottom;
      end;
      {$ENDREGION}

    end
    {$ENDREGION}

    {$REGION 'NoVisibleItems'}
    else if not HasVisibleItems then begin
      ArrangeNoItemsEphemeralLayout(BoundsRect.ReducePrecision);
    end
    {$ENDREGION}

    {$REGION 'Other'}
    else begin
      ScrollMaxBound := GetFarthestItemBottom;
    end;
    {$ENDREGION}

  end

  else begin

    {$REGION 'GrowItemsError'}
    if GrowItemsError then begin

      {$REGION 'ItemsReversedOrientation'}
      if ItemsReversedOrientation then begin
        LEphemeralLayout.ItemsReloadBtnPos := TalPointD.Create(
                                                0 - ItemsReloadBtnMarginRight - ItemsReloadBtnWidth,
                                                (BoundsRect.Height - ItemsReloadBtnHeight) / 2);
        LEphemeralLayout.GrowItemsAniindicatorPos := TalPointD.Create(
                                                       0 - GrowItemsAniindicatorMarginRight (*- TALMainForm.Instance.AniindicatorWidth*),
                                                       (BoundsRect.Height (*- TALMainForm.Instance.AniindicatorHeight*)) / 2);
        ScrollMinBound := LEphemeralLayout.ItemsReloadBtnPos.x -
                          ItemsReloadBtnMarginLeft;
        ScrollMaxBound := GetFarthestItemRight;
      end
      {$ENDREGION}

      {$REGION 'NO ItemsReversedOrientation'}
      else begin
        LEphemeralLayout.ItemsReloadBtnPos := TalPointD.Create(
                                                GetFarthestItemRight + ItemsReloadBtnMarginLeft,
                                                (BoundsRect.Height - ItemsReloadBtnHeight) / 2);
        LEphemeralLayout.GrowItemsAniindicatorPos := TalPointD.Create(
                                                       GetFarthestItemRight + GrowItemsAniindicatorMarginLeft,
                                                       (BoundsRect.Height (*- TALMainForm.Instance.AniindicatorHeight*)) / 2);
        ScrollMaxBound := LEphemeralLayout.ItemsReloadBtnPos.x +
                          ItemsReloadBtnWidth +
                          ItemsReloadBtnMarginRight;
      end;
      {$ENDREGION}

      LEphemeralLayout.ItemsReloadBtnTouchTargetRect := TalRectD.create(
                                                          LEphemeralLayout.ItemsReloadBtnPos - tAlPointD.Create(OwnerScrollBox.TouchTargetExpansion,OwnerScrollBox.TouchTargetExpansion),
                                                          ItemsReloadBtnWidth + (OwnerScrollBox.TouchTargetExpansion*2),
                                                          ItemsReloadBtnHeight + (OwnerScrollBox.TouchTargetExpansion*2));

    end
    {$ENDREGION}

    {$REGION 'CanGrowItems'}
    else if CanGrowItems then begin

      {$REGION 'ItemsReversedOrientation'}
      if ItemsReversedOrientation then begin
        LEphemeralLayout.GrowItemsAniindicatorPos := TalPointD.Create(
                                                       0 - GrowItemsAniindicatorMarginRight (*- TALMainForm.Instance.AniindicatorWidth*),
                                                       (BoundsRect.Height (*- TALMainForm.Instance.AniindicatorHeight*)) / 2);
        ScrollMinBound := LEphemeralLayout.GrowItemsAniindicatorPos.X -
                          GrowItemsAniindicatorMarginLeft;
        ScrollMaxBound := GetFarthestItemRight;
      end
      {$ENDREGION}

      {$REGION 'NO ItemsReversedOrientation'}
      else begin
        LEphemeralLayout.GrowItemsAniindicatorPos := TalPointD.Create(
                                                       GetFarthestItemRight + GrowItemsAniindicatorMarginLeft,
                                                       (BoundsRect.Height (*- TALMainForm.Instance.AniindicatorHeight*)) / 2);
        ScrollMaxBound := LEphemeralLayout.GrowItemsAniindicatorPos.X +
                          //*TALMainForm.Instance.AniindicatorWidth +
                          GrowItemsAniindicatorMarginRight;
      end;
      {$ENDREGION}

    end
    {$ENDREGION}

    {$REGION 'NoVisibleItems'}
    else if not HasVisibleItems then begin
      ArrangeNoItemsEphemeralLayout(BoundsRect.ReducePrecision);
    end
    {$ENDREGION}

    {$REGION 'Other'}
    else begin
      ScrollMaxBound := GetFarthestItemRight;
    end;
    {$ENDREGION}

  end;

end;

{***********************************************************************************}
procedure TALDynamicScrollBoxView.ArrangeNoItemsEphemeralLayout(const ARect: TRectF);
begin

//*  var LEphemeralLayout := EphemeralLayout;
//*  if LEphemeralLayout = nil then exit;
//*
//*  if ItemsOrientation = TOrientation.Vertical then begin
//*
//*    var LNoItemsIconBitmapSize := TpointF.Zero;
//*    if NoItemsIcon <> nil then begin
//*      LEphemeralLayout.NoItemsIcon := True;
//*      LNoItemsIconBitmapSize := TpointF.Create(
//*                                  NoItemsIcon.Width / ALGetScreenScale,
//*                                  NoItemsIcon.Height / ALGetScreenScale)
//*    end;
//*    //-----
//*    var LNoItemsTitleMarginTop: Single := 0;
//*    var LNoItemsTitleBitmapSize := TpointF.Zero;
//*    if NoItemsTitle <> nil then begin
//*      LEphemeralLayout.NoItemsTitle := True;
//*      if LEphemeralLayout.NoItemsIcon then LNoItemsTitleMarginTop := NoItemsTitleMarginTop;
//*      LNoItemsTitleBitmapSize := TpointF.Create(
//*                                   NoItemsTitle.Width / ALGetScreenScale,
//*                                   NoItemsTitle.Height / ALGetScreenScale)
//*    end;
//*    //-----
//*    var LNoItemsSubTitleMarginTop: Single := 0;
//*    var LNoItemsSubTitleBitmapSize := TpointF.Zero;
//*    if NoItemsSubTitle <> nil then begin
//*      LEphemeralLayout.NoItemsSubTitle := True;
//*      if LEphemeralLayout.NoItemsIcon or
//*         LEphemeralLayout.NoItemsTitle then LNoItemsSubTitleMarginTop := NoItemsSubTitleMarginTop;
//*      LNoItemsSubTitleBitmapSize := TpointF.Create(
//*                                      NoItemsSubTitle.Width / ALGetScreenScale,
//*                                      NoItemsSubTitle.Height / ALGetScreenScale)
//*    end;
//*    //-----
//*    var LNoItemsBtnMarginTop: Single := 0;
//*    var LNoItemsBtnBitmapSize := TpointF.Zero;
//*    if NoItemsBtn <> nil then begin
//*      LEphemeralLayout.NoItemsBtn := True;
//*      if LEphemeralLayout.NoItemsIcon or
//*         LEphemeralLayout.NoItemsTitle or
//*         LEphemeralLayout.NoItemsSubTitle then LNoItemsBtnMarginTop := NoItemsBtnMarginTop;
//*      LNoItemsBtnBitmapSize := TpointF.Create(
//*                                 NoItemsBtn.Width / ALGetScreenScale,
//*                                 NoItemsBtn.Height / ALGetScreenScale)
//*    end;
//*    //-----
//*    var LNoItemsHeight: single := LNoItemsIconBitmapSize.y +
//*                                  LNoItemsTitleMarginTop +
//*                                  LNoItemsTitleBitmapSize.y +
//*                                  LNoItemsSubTitleMarginTop +
//*                                  LNoItemsSubTitleBitmapSize.y +
//*                                  LNoItemsBtnMarginTop +
//*                                  LNoItemsBtnBitmapSize.y;
//*    LEphemeralLayout.NoItemsIconPos := TALPointD.create(
//*                                         ARect.left + ((ARect.Width - LNoItemsIconBitmapSize.x) / 2),
//*                                         Max(
//*                                           ARect.top + NoItemsMarginTop,
//*                                           ARect.top + ((ARect.Height - LNoItemsHeight) / 2)));
//*    if LEphemeralLayout.NoItemsIcon then
//*      LEphemeralLayout.NoItemsIconTouchTargetRect := TALRectD.create(
//*                                                       LEphemeralLayout.NoItemsIconPos - TALPointD.Create(OwnerScrollBox.TouchTargetExpansion,OwnerScrollBox.TouchTargetExpansion),
//*                                                       LNoItemsIconBitmapSize.x + (OwnerScrollBox.TouchTargetExpansion*2),
//*                                                       LNoItemsIconBitmapSize.y + (OwnerScrollBox.TouchTargetExpansion*2));
//*    LEphemeralLayout.NoItemsTitlePos := TALPointD.create(
//*                                          ARect.left + ((ARect.Width - LNoItemsTitleBitmapSize.x) / 2),
//*                                          LEphemeralLayout.NoItemsIconPos.y + LNoItemsIconBitmapSize.y + LNoItemsTitleMarginTop);
//*    if LEphemeralLayout.NoItemsTitle then
//*      LEphemeralLayout.NoItemsTitleTouchTargetRect := TALRectD.create(
//*                                                        LEphemeralLayout.NoItemsTitlePos - TALPointD.Create(OwnerScrollBox.TouchTargetExpansion,OwnerScrollBox.TouchTargetExpansion),
//*                                                        LNoItemsTitleBitmapSize.x + (OwnerScrollBox.TouchTargetExpansion*2),
//*                                                        LNoItemsTitleBitmapSize.y + (OwnerScrollBox.TouchTargetExpansion*2));
//*    LEphemeralLayout.NoItemsSubTitlePos := TALPointD.create(
//*                                             ARect.left + ((ARect.Width - LNoItemsSubTitleBitmapSize.x) / 2),
//*                                             LEphemeralLayout.NoItemsTitlePos.y + LNoItemsTitleBitmapSize.y + LNoItemsSubTitleMarginTop);
//*    if LEphemeralLayout.NoItemsSubTitle then
//*      LEphemeralLayout.NoItemsSubTitleTouchTargetRect := TALRectD.create(
//*                                                           LEphemeralLayout.NoItemsSubTitlePos - TALPointD.Create(OwnerScrollBox.TouchTargetExpansion,OwnerScrollBox.TouchTargetExpansion),
//*                                                           LNoItemsSubTitleBitmapSize.x + (OwnerScrollBox.TouchTargetExpansion*2),
//*                                                           LNoItemsSubTitleBitmapSize.y + (OwnerScrollBox.TouchTargetExpansion*2));
//*    LEphemeralLayout.NoItemsBtnPos := TALPointD.create(
//*                                        ARect.left + ((ARect.Width - LNoItemsBtnBitmapSize.x) / 2),
//*                                        LEphemeralLayout.NoItemsSubTitlePos.y + LNoItemsSubTitleBitmapSize.y + LNoItemsBtnMarginTop);
//*    if LEphemeralLayout.NoItemsBtn then
//*      LEphemeralLayout.NoItemsBtnTouchTargetRect := TALRectD.create(
//*                                                      LEphemeralLayout.NoItemsBtnPos - TALPointD.Create(OwnerScrollBox.TouchTargetExpansion,OwnerScrollBox.TouchTargetExpansion),
//*                                                      LNoItemsBtnBitmapSize.x + (OwnerScrollBox.TouchTargetExpansion*2),
//*                                                      LNoItemsBtnBitmapSize.y + (OwnerScrollBox.TouchTargetExpansion*2));
//*    //-----
//*    ScrollMaxBound := LEphemeralLayout.NoItemsBtnPos.y +
//*                      LNoItemsBtnBitmapSize.y +
//*                      NoItemsMarginBottom;
//*
//*  end
//*
//*  else begin
//*    //todo
//*  end;

end;

{****************************************************}
function TALDynamicScrollBoxView.GetScrollPos: Double;
begin
  if FPostponedScrollPosSet then result := FPostponedScrollPos
  else result := FScrollPos;
end;

{***************************************************************************}
function TALDynamicScrollBoxView.CalculateScrollFirstVisibleItemIdx: integer;
begin

  //
  // -4 -     -
  // -3 -     -
  // -2 -     -
  // -1 -     -                    <= this pixel is NOT drawn
  //  0 +++++++ (FScrollPos)       <= this pixel is drawn
  //  1 +++++++                    <= this pixel is drawn
  //  2 +++++++                    <= this pixel is drawn
  //  3 -     - (Owner.ViewHeight) <= this pixel is NOT drawn
  //  4 -     -
  //

  //----- 0 if FItems is empty
  Result := Max(
              Low(FItems){0 if empty},
              Min(
                High(FItems){-1 if empty},
                FScrollFirstVisibleItemIdx));

  //
  //                       x   x   x
  //                         x   x
  // FirstVisibleItemIdx > x   x   x
  // -------------------     x   x
  //                       x   x   x < LastVisibleItemIdx
  //                         x   x
  //                       x   x   x
  //
  var LTMPIdx := Result;
  var LFailCount: integer := 0;
  while (LTMPIdx >= Low(FItems)) and
        (LTMPIdx <= High(FItems)) do begin
    if FItems[LTMPIdx].Visible then begin
      if ((ItemsOrientation = TOrientation.Vertical)   and (compareValue(FItems[LTMPIdx].BoundsRect.bottom, FScrollPos, TEpsilon.Position) >= 0)) or
         ((ItemsOrientation = TOrientation.horizontal) and (compareValue(FItems[LTMPIdx].BoundsRect.right,  FScrollPos, TEpsilon.Position) >= 0)) then begin
        Result := LTMPIdx;
        LFailCount := 0;
      end
      else begin
        inc(LFailCount);
        if LFailcount >= 2 then break;
      end;
    end;
    dec(LTMPIdx);
  end;
  //-----
  while (Result < High(FItems)) and
        ((not FItems[Result].Visible) or
         (((ItemsOrientation = TOrientation.Vertical)   and (compareValue(FItems[Result].BoundsRect.bottom, FScrollPos, TEpsilon.Position) < 0)) or
          ((ItemsOrientation = TOrientation.horizontal) and (compareValue(FItems[Result].BoundsRect.right,  FScrollPos, TEpsilon.Position) < 0)))) do
    inc(Result);

  //consistency check only in debug
  {$IF defined(DEBUG)}
  if (Result < Low(FItems)) then
    raise Exception.Create('Error FBAEF066-CFC7-4B63-96BA-15A1F0C8D827');
  {$ENDIF}

end;

{**************************************************************************}
function TALDynamicScrollBoxView.CalculateScrollLastVisibleItemIdx: integer;
begin

  //
  // -4 -     -
  // -3 -     -
  // -2 -     -
  // -1 -     -                    <= this pixel is NOT drawn
  //  0 +++++++ (FScrollPos)       <= this pixel is drawn
  //  1 +++++++                    <= this pixel is drawn
  //  2 +++++++                    <= this pixel is drawn
  //  3 -     - (Owner.ViewHeight) <= this pixel is NOT drawn
  //  4 -     -
  //

  //----- -1 if FItems is empty
  Result := Min(
              High(FItems){-1 if empty},
              Max(
                Low(FItems){0 if empty},
                FScrollLastVisibleItemIdx));

  //
  //                       x   x   x
  //                         x   x
  // FirstVisibleItemIdx > x   x   x
  //                         x   x
  //                       x   x   x < LastVisibleItemIdx
  //                         x   x     ------------------
  //                       x   x   x
  //
  var LTMPIdx := Result;
  var LFailCount := 0;
  while (LTMPIdx >= Low(FItems)) and
        (LTMPIdx <= High(FItems)) do begin
    if FItems[LTMPIdx].Visible then begin
      if ((ItemsOrientation = TOrientation.vertical)   and (compareValue(FItems[LTMPIdx].BoundsRect.top,  FScrollPos + BoundsRect.Height, TEpsilon.Position) < 0)) or
         ((ItemsOrientation = TOrientation.horizontal) and (compareValue(FItems[LTMPIdx].BoundsRect.left, FScrollPos + BoundsRect.Width,  TEpsilon.Position) < 0)) then begin
        Result := LTMPIdx;
        LFailCount := 0;
      end
      else begin
        inc(LFailCount);
        if LFailcount >= 2 then break;
      end;
    end;
    inc(LTMPIdx);
  end;
  //-----
  while (Result > Low(FItems)) and
        ((not FItems[Result].Visible) or
         (((ItemsOrientation = TOrientation.Vertical)   and (compareValue(FItems[Result].BoundsRect.top,  FScrollPos + BoundsRect.Height, TEpsilon.Position) >= 0)) or
          ((ItemsOrientation = TOrientation.horizontal) and (compareValue(FItems[Result].BoundsRect.left, FScrollPos + BoundsRect.width,  TEpsilon.Position) >= 0)))) do
    dec(Result);

  //consistency check only in debug
  {$IF defined(DEBUG)}
  if (Result > High(FItems)) then
    raise Exception.Create('Error 399D434C-C527-476A-B49D-33CFDAB30D86');
  {$ENDIF}

end;

{***************************************************************************************************************************************************************}
procedure TALDynamicScrollBoxView.SetScrollPos(const AValue: Double; const AEnforceScrollLimits: boolean = True; const ATriggeredByScrollEvent: boolean = False);
begin

  {$REGION 'EnforceScrollLimits'}
  if AEnforceScrollLimits then begin
    if ItemsOrientation = TOrientation.Vertical then begin
      SetScrollPos(
        Max(
          ScrollEngine.MinScrollLimit.y,
          min(
            ScrollEngine.MaxScrollLimit.y,
            AValue)),
        false{AEnforceScrollLimits},
        ATriggeredByScrollEvent)
    end
    else begin
      SetScrollPos(
        Max(
          ScrollEngine.MinScrollLimit.X,
          min(
            ScrollEngine.MaxScrollLimit.X,
            AValue)),
        false{AEnforceScrollLimits},
        ATriggeredByScrollEvent);
    end;
    exit;
  end;
  {$ENDREGION}

  if (TNonReentrantHelper.EnterSection(FSetScrollPosGuard)) then begin
    Try

      {$REGION 'calculate FScrollFromFirstToLast'}
      if ATriggeredByScrollEvent then begin
        var LCompareValue := compareValue(FScrollPos, aValue, TEpsilon.Position);
        if LCompareValue < 0 then FScrollFromFirstToLast := True
        else if LCompareValue > 0 then FScrollFromFirstToLast := False;
      end;
      {$ENDREGION}

      {$REGION 'Update the ActionBar'}
      //*if (ActionBar <> nil) and (ActionBar.CanCollapse) then begin
      //*  if (ATriggeredByScrollEvent) and (aValue > Fscrollengine.MinScrollLimit.y) and (aValue < Fscrollengine.MaxScrollLimit.y) then begin
      //*    if FScrollFromFirstToLast then ActionBar.top := max(-ActionBar.Height, ActionBar.top - (aValue - FScrollPos))
      //*    else ActionBar.top := min(0, ActionBar.top + (FScrollPos - aValue));
      //*  end;
      //*end;
      {$ENDREGION}

      {$REGION 'Update CurrentTouchEffect'}
      //*if ItemsOrientation = TOrientation.Vertical then TALTouchEffect.OffsetCurrentTouchEffect(0, FScrollPos - aValue)
      //*else TALTouchEffect.OffsetCurrentTouchEffect(FScrollPos - aValue, 0);
      {$ENDREGION}

      {$REGION 'Update FScrollPos'}
      FScrollPos := aValue;
      {$IF defined(MSWINDOWS) or defined(ALMacOS)}
      if ItemsOrientation = TOrientation.Vertical then begin
        var LPrevOnChange := OwnerScrollBox.FscrollBar.OnChange;
        OwnerScrollBox.FscrollBar.OnChange := nil;
        Try
          OwnerScrollBox.FscrollBar.value := FScrollPos;
        finally
          OwnerScrollBox.ScrollBar.OnChange := LPrevOnChange;
        end;
      end;
      {$ENDIF}
      {$ENDREGION}

      {$REGION 'calculate FScrollFirstVisibleItemIdx/FScrollLastVisibleItemIdx'}
      var LOldScrollFirstVisibleItemIdx := FScrollFirstVisibleItemIdx;
      var LOldScrollLastVisibleItemIdx := FScrollLastVisibleItemIdx;
      FScrollFirstVisibleItemIdx := CalculateScrollFirstVisibleItemIdx;
      FScrollLastVisibleItemIdx := CalculateScrollLastVisibleItemIdx;
      {$ENDREGION}

      {$REGION 'calculate FScrollFirstPreloadedItemIdx/FScrollLastPreloadedItemIdx'}
      var LOldScrollFirstPreloadedItemIdx := FScrollFirstPreloadedItemIdx;
      var LOldScrollLastPreloadedItemIdx := FScrollLastPreloadedItemIdx;
      FScrollFirstPreloadedItemIdx := Max(Low(FItems){0 if empty}, FScrollFirstVisibleItemIdx - ItemsPreloadOffset);
      FScrollLastPreloadedItemIdx := Min(High(FItems){-1 if empty}, FScrollLastVisibleItemIdx + ItemsPreloadOffset);

      //consistency check only in debug
      {$IF defined(DEBUG)}
      if (LOldScrollFirstPreloadedItemIdx < Low(FItems)) or
         (LOldScrollLastPreloadedItemIdx > High(FItems)) or
         (FScrollFirstPreloadedItemIdx < Low(FItems)) or
         (FScrollLastPreloadedItemIdx > High(FItems)) then
        raise Exception.Create('Error FD682464-DBB8-4D50-B31F-F8AEFCEBBA15');
      {$ENDIF}
      {$ENDREGION}

      {$REGION 'UpdateGlobalThreadPoolPriority'}
      UpdateGlobalThreadPoolPriority;
      {$ENDREGION}

      {$REGION 'Owner.fCanRefreshByScrolling'}
      //*if compareValue(FScrollPos, 0, 1{Epsilon}) > 0 then OwnerScrollBox.CanRefreshByScrolling := False;
      {$ENDREGION}

      {$REGION 'Update VisibleInPaintArea'}

      //
      // the program to regenerate the result of the graph below is located
      // in WinSuite/WinDating/_Archive/kiskis/offsetpreload
      //
      // (A)   LOldScrollLastVisibleItemIdx
      // (A)   | LOldScrollFirstVisibleItemIdx
      //       | |
      // (B)   | |               | LOldScrollFirstVisibleItemIdx
      // (B)   | |               |         | LOldScrollLastVisibleItemIdx
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
      for var I := Max(Low(FItems){0 if empty}, LOldScrollFirstVisibleItemIdx) to Min(High(FItems){-1 if empty}, FScrollFirstVisibleItemIdx - 1) do
        FItems[i].VisibleInPaintArea := False;

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
      for var I := Min(High(FItems){-1 if empty}, LOldScrollLastVisibleItemIdx) downto Max(Low(FItems){0 if empty}, FScrollLastVisibleItemIdx + 1) do
        FItems[i].VisibleInPaintArea := False;

      //when ATriggeredByScrollEvent=false we Prepare of all items between
      //FScrollFirstVisibleItemIdx and FScrollLastVisibleItemIdx
      //to handle the case when we insert some items in the middle of
      //FItems
      if ATriggeredByScrollEvent then begin

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
        for var I := Max(Low(FItems){0 if empty}, FScrollFirstVisibleItemIdx) to MinIntValue([High(FItems){-1 if empty}, FScrollLastVisibleItemIdx, LoldScrollFirstVisibleItemIdx - 1]) do begin
          FItems[i].VisibleInPaintArea := True;
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
        for var I := Min(High(FItems){-1 if empty}, FScrollLastVisibleItemIdx) downto MaxIntValue([Low(FItems){0 if empty}, FScrollFirstVisibleItemIdx, LoldScrollLastVisibleItemIdx + 1]) do begin
          FItems[i].VisibleInPaintArea := True;
        end;

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
        for var I := Max(Low(FItems){0 if empty}, FScrollFirstVisibleItemIdx) to Min(High(FItems){-1 if empty}, FScrollLastVisibleItemIdx) do begin
          FItems[i].VisibleInPaintArea := True;
        end;

      end;
      {$ENDREGION}

      {$REGION 'offset preload'}

      //
      // the program to regenerate the result of the graph below is located
      // in WinSuite/WinDating/_Archive/kiskis/offsetpreload
      //
      // (A)   LOldScrollLastPreloadedItemIdx
      // (A)   | LOldScrollFirstPreloadedItemIdx
      //       | |
      // (B)   | |               | LOldScrollFirstPreloadedItemIdx
      // (B)   | |               |         | LOldScrollLastPreloadedItemIdx
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
      for var I := Max(Low(FItems){0 if empty}, LOldScrollFirstPreloadedItemIdx) to Min(High(FItems){-1 if empty}, FScrollFirstPreloadedItemIdx - 1) do
        FItems[i].clearEphemeralLayout;

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
      for var I := Min(High(FItems){-1 if empty}, LOldScrollLastPreloadedItemIdx) downto Max(Low(FItems){0 if empty}, FScrollLastPreloadedItemIdx + 1) do
        FItems[i].clearEphemeralLayout;

      //when ATriggeredByScrollEvent=false we Prepare of all items between
      //FScrollFirstPreloadedItemIdx and FScrollLastPreloadedItemIdx
      //to handle the case when we insert some items in the middle of
      //FItems
      if ATriggeredByScrollEvent then begin

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
        for var I := Max(Low(FItems){0 if empty}, FScrollFirstPreloadedItemIdx) to MinIntValue([High(FItems){-1 if empty}, FScrollLastPreloadedItemIdx, LoldScrollFirstPreloadedItemIdx - 1]) do begin
          if FPostponedScrollPosSet then break;
          FItems[i].Prepare;
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
        for var I := Min(High(FItems){-1 if empty}, FScrollLastPreloadedItemIdx) downto MaxIntValue([Low(FItems){0 if empty}, FScrollFirstPreloadedItemIdx, LoldScrollLastPreloadedItemIdx + 1]) do begin
          if FPostponedScrollPosSet then break;
          FItems[i].Prepare;
        end;

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
        for var I := Max(Low(FItems){0 if empty}, FScrollFirstPreloadedItemIdx) to Min(High(FItems){-1 if empty}, FScrollLastPreloadedItemIdx) do begin
          if FPostponedScrollPosSet then break;
          FItems[i].Prepare;
        end;

      end;
      {$ENDREGION}

      {$REGION 'Log'}
      {$IFDEF DEBUG}
      {-----
      ALLog(
        Classname+'.SetScrollPos',
        'Value: '+ ALFloatToStrW(aValue, ALDefaultFormatSettingsW) + ' | '+
        'EnforceScrollLimits:' + ALBoolToStrW(AEnforceScrollLimits) + ' | '+
        'TriggeredByScrollEvent:' + ALBoolToStrW(ATriggeredByScrollEvent) + ' | '+
        'Postponed: False',
        TalLogType.VERBOSE);
      -----}
      {$ENDIF}
      {$ENDREGION}

    finally
      TNonReentrantHelper.LeaveSection(FSetScrollPosGuard);
    End;

    {$REGION 'PostponedScrollPos'}
    if FPostponedScrollPosSet then begin
      FPostponedScrollPosSet := false;
      SetScrollPos(FPostponedScrollPos, False{AEnforceScrollLimits}, False{ATriggeredByScrollEvent});
      exit;
    end;
    {$ENDREGION}

    {$REGION 'align ScrollEngine.ViewportPosition with ScrollPos'}
    // This is also important to enforce boundaries. When we assign
    // ScrollEngine.ViewportPosition := xxx and if xxx is outside the
    // boundaries, then ScrollEngine will initiate a springback to move inside
    // the boundaries.
    if not ATriggeredByScrollEvent then begin
      var LPrevScrollEngineOnChanged := ScrollEngine.OnChanged;
      ScrollEngine.OnChanged := Nil;
      Try
        if ItemsOrientation = TOrientation.Vertical then
          ScrollEngine.ViewportPosition := TALPointD.Create(0, ScrollPos)
        else
          ScrollEngine.ViewportPosition := TALPointD.Create(ScrollPos,0);
      finally
        ScrollEngine.OnChanged := LPrevScrollEngineOnChanged;
      End;
    end;
    {$ENDREGION}

    {$REGION 'GrowItems'}
    GrowItems;
    {$ENDREGION}

    {$REGION 'repaint'}
    OwnerScrollBox.repaint;
    {$ENDREGION}

  end
  else begin
    //in SetScrollPos we call FItems[i].Prepare that will build the EphemeralLayout
    //and if the height returned in the EphemeralLayout is different from the height of
    //the Item then we will call arrangeItems that in turn will call SetScrollPos
    //to visually glue the Scroll
    FPostponedScrollPosSet := True;
    FPostponedScrollPos := aValue;
    {$IFDEF DEBUG}
    {-----
    ALLog(
      Classname+'.SetScrollPos',
      'Value: '+ ALFloatToStrW(aValue, ALDefaultFormatSettingsW) + ' | '+
      'EnforceScrollLimits:' + ALBoolToStrW(AEnforceScrollLimits) + ' | '+
      'TriggeredByScrollEvent:' + ALBoolToStrW(ATriggeredByScrollEvent) + ' | '+
      'Postponed: True',
      TalLogType.VERBOSE);
    -----}
    {$ENDIF}
  end;

end;

{*******************************************}
procedure TALDynamicScrollBoxView.AddNewItem(
            const AItem: TALJSONNodeW;
            const aFromQueue: boolean = False;
            const AInsertAt: integer = -1;
            const AAsync: Boolean = True);
begin

  //security check
  if AItem = nil then
    raise Exception.Create('Error 917F7393-7C4E-49DF-BD8C-C6F93DDB0487');

  //enqueue if we are not ready
  if (not GrowItemsExecutedAtLeastOnce) or // GrowItemsExecutedAtLeastOnce is set only after IsGrowDataFinished (ie: GrowItemsBackgroundProcCanProcessItems)
     (GrowItemsIsRunning) or // if we are still growing the Items then enqueue
     ((NewItemAnim <> nil) and
      (NewItemAnim.enabled)) then begin // if we are playing an new Activity animation enqueue also
    EnqueueNewItem(AItem);
    exit;
  end;

  //in AddNewItem the behavior of InsertAt (by default insert at the begining) is opposed to the behavior in
  //growitem (by default insert at the end)
  var LInsertAt: Integer;
  if AInsertAt > -1 then LInsertAt := AInsertAt
  else if FItemsReversedOrientation then LInsertAt := Maxint
  else LInsertAt := 0;

  //create the item node
  var Litems := TALJSONDocumentW.Create;
  try
    AItem.setChildNodeValueBool('can_do_new_item_anim', AItem.GetChildNodeValueBool('can_do_new_item_anim', True) and (not aFromQueue));
    AItem.setChildNodeValueBool('can_do_new_item_vibrate', AItem.GetChildNodeValueBool('can_do_new_item_vibrate', True) and (not aFromQueue));
    Litems.ChildNodes.Add(AItem);
    GrowItems(
      true, // const aForceGrow: Boolean = False;
      LInsertAt, // const AInsertAt: integer;
      Litems, // const AItems: TALJSONNodeW = nil): boolean;
      AAsync); // const AAsync: Boolean = True
  except
    ALFreeAndNil(Litems);
    Raise;
  end;

end;

{****************************************************}
procedure TALDynamicScrollBoxView.ProcessNewItemQueue;
begin
  Var LItems := fNewItemQueue.toarray;
  if length(LItems) = 0 then exit;
  fNewItemQueue.OwnsObjects := False;
  Try
    fNewItemQueue.Clear;
  Finally
    fNewItemQueue.OwnsObjects := True;
  End;
  for var I := low(LItems) to high(LItems) do
    AddNewItem(LItems[i], true{aFromQueue});
end;

{**************************************************************************}
procedure TALDynamicScrollBoxView.EnqueueNewItem(const AItem: TALJSONNodeW);
begin
  fnewItemQueue.Add(AItem);
end;

{**************************************************}
Procedure TALDynamicScrollBoxView.CreateNewItemAnim;
begin
  if fNewItemAnim = nil then begin
    fNewItemAnim := TALFloatAnimation.Create;
    //*fNewItemAnim.OnProcess := OwnerScrollBox.BasicAnimProcess;
    fNewItemAnim.OnFinish := NewItemAnimFinish;
    fNewItemAnim.delay := 0;
    fNewItemAnim.Duration := 0.5;
    fNewItemAnim.AnimationType := TAnimationType.out;
    fNewItemAnim.Interpolation := TALInterpolationType.back;
    fNewItemAnim.Overshoot := 2;
    fNewItemAnim.StartValue := 0;
    fNewItemAnim.StopValue := 1;
    fNewItemAnim.Tag := 0;
  end;
end;

{**************************************************************************************************}
function TALDynamicScrollBoxView.CanDoNewItemAnim(const ANewItem: TALDynamicScrollBoxItem): boolean;
begin
  result := (OwnerScrollBox.visible) and // << no animation if the frame is not visible
            //*(TALMainForm.Instance.popupdialog = nil) and // << no animation if popupdialog is present as we don't know what is going so better avoid to do anything
            //*(TALMainForm.Instance.popupLoading = nil) and // << we don't know what is going so better avoid to do anything
            //*(OwnerScrollBox.popupLoading = nil) and // << we don't know what is going so better avoid to do anything
            (not OwnerScrollBox.HasActiveScrollEngines) and // if we touch the screen or if their is some scroll no need animation that will destroy the scroll
            ((FItemsReversedOrientation and (ScrollPos > 0)) or // see why in ItemsReversedOrientation we need ScrollPos > 0 by trying to add a messages in an empty conversation.
             ((not FItemsReversedOrientation) and
              (((ItemsOrientation=Torientation.vertical) and (compareValue(ANewItem.BoundsRect.Top, ScrollPos, Tepsilon.Position) >= 0)) or
               ((ItemsOrientation=Torientation.horizontal) and (compareValue(ANewItem.BoundsRect.left, ScrollPos, Tepsilon.Position) >= 0))))) and // in not ItemsReversedOrientation the anim can work in any situation as soon as the ANewItem is fully visible
            (ANewItem.ItemIndex >= ScrollFirstVisibleItemIdx) and // << ANewItem must be visible
            (ANewItem.ItemIndex <= ScrollLastVisibleItemIdx) and // << ANewItem must be visible
            ((NewItemAnim = nil) or (not NewItemAnim.Enabled)) and
            (ANewItem.data.getChildNodeValuebool('can_do_new_item_anim', true)) //* and
            //*(not TALLifecycleMonitor.Instance.AppEnteredBackground);
end;

{*****************************************************************************************************}
function TALDynamicScrollBoxView.CanDoNewItemVibrate(const ANewItem: TALDynamicScrollBoxItem): boolean;
begin
  result := (OwnerScrollBox.visible) and
            (ANewItem.data.getChildNodeValuebool('can_do_new_item_vibrate', true)) //* and
            //* (not TALLifecycleMonitor.Instance.AppEnteredBackground);
end;

{*******************************************************************}
procedure TALDynamicScrollBoxView.NewItemAnimFinish(Sender: TObject);
begin
  //*TALMainForm.Instance.closePopupLoading;
  fNewItemAnim.Enabled := False;
  fNewItemAnim.Tag := 0;
  ProcessNewItemQueue;
  OwnerScrollBox.repaint;
end;

{************************************************************************************}
//*function TALDynamicScrollBoxView.CreateActionBar: TALDynamicScrollBoxViewActionBar;
//*begin
//*  result := nil;
//*end;

{*************************************************************}
//*function TALDynamicScrollBoxView.GetActionBarHeight: Single;
//*begin
//*  if ActionBar = nil then result := 0
//*  else result := ActionBar.Height;
//*end;

{***********************************************************}
constructor TALDynamicScrollBox.TFilterDialogPopupBtn.Create(
              const ACaption: String;
              const AResName: String;
              const AModalResult: TModalResult);
begin
  Caption := ACaption;
  ResName := AResName;
  ModalResult := AModalResult;
end;

{*********************************************************}
constructor TALDynamicScrollBox.Create(aOwner: TComponent);
begin
  inherited create(AOwner);
  //-----
  //*AsyncDestroyMandatory := True;
  OnResized := FrameResized;
  //-----
  //all PreloadOffsets will be initialized in DoInitPreloadOffsets;
  //-----
  FFrameWidth := 0;
  FFrameHeight := 0;
  FFrameAffineRatio := 1;
  FFrameKeyboardHeight := 0;
  FMainViewWidth := 0;
  FMainViewHeight := 0;
  //all others Measures will be initialized in DoInitMeasures
  //-----
  ErrorMessageBannerAnim := nil;
  GPSRequiredMessageBannerAnim := nil;
  fErrorMessageBanner := ALNullDrawable;
  fGPSRequiredMessageBanner := ALNullDrawable;
  fItemsReloadBtn := ALNullDrawable;
  fRefreshIcon := ALNullDrawable;
  fRefreshingAniIcon := ALNullDrawable;
  fNoItemsIcon := ALNullDrawable;
  fNoItemsTitle := ALNullDrawable;
  fNoItemsSubTitle := ALNullDrawable;
  fNoItemsBtn := ALNullDrawable;
  fFilterDialogMainBtn := ALNullDrawable;
  //-----
  //*FProgressFrame := CreateProgressFrame;
  //*FProgressFrameStopAnimationEvent := CreateProgressFrameStopAnimationEvent;
  //-----
  // We utilize TList<> as opposed to TObjectList<> because
  // TObjectList overloads the Notify method, which consequently
  // slows down all bulk insert operations.
  FActiveScrollEngines := TList<TALScrollEngine>.create;
  FHasActiveScrollEngines := False;
  //-----
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  FScrollBar := TALScrollBar.create(self);
  FScrollBar.Parent := self;
  FscrollBar.Orientation := Torientation.Vertical;
  FscrollBar.Width := 4;
  FscrollBar.Anchors := [TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
  FscrollBar.Tag := 0;
  FscrollBar.Visible := False;
  //*fScrollBar.Thumb.OnMouseMove := Tkiskis_ProcOfObjectWrapper.OnVertScrollBoxVScrollBarThumbMouseMove;
  //*fScrollBar.Thumb.OnMouseLeave := Tkiskis_ProcOfObjectWrapper.OnVertScrollBoxVScrollBarThumbMouseLeave;
  //*fScrollBar.Thumb.OnMouseDown := Tkiskis_ProcOfObjectWrapper.OnVertScrollBoxVScrollBarThumbMouseDown;
  //*fScrollBar.Thumb.OnMouseUp := Tkiskis_ProcOfObjectWrapper.OnVertScrollBoxVScrollBarThumbMouseUp;
  fScrollBar.OnChange := ScrollBarChange;
  {$ENDIF}
  //-----
  //*fCanRefreshByScrolling := True;
  //*fRefreshByScrollingDisabled := False;
  //------
  fAniIndicatorTimer := Ttimer.Create(nil);
  fAniIndicatorTimer.Enabled := False;
  fAniIndicatorTimer.Interval := 50;
  fAniIndicatorTimer.OnTimer := AniIndicatorOnTimer;
  FAniIndicatorEnabled := False;
  fAniindicatorImgIndex := TSmallPoint.create(0,0);
  //------
  FHovered := nil;
  //------
  FLongPressTimer := TTimer.Create(nil);
  FLongPressTimer.OnTimer := LongPressTimerOnTimer;
  FLongPressTimer.Enabled := False;
  //------
  FTouchEffectOwner := nil;
  FTouchEffectMousePos := TpointF.Zero;
  FTouchEffectClicked := False;
  FTouchEffectAnim := TALFloatAnimation.Create;
  FTouchEffectAnim.AnimationType := TAnimationType.in;
  FTouchEffectAnim.Interpolation := TALInterpolationType.linear;
  //*FTouchEffectAnim.OnProcess := BasicAnimProcess;
  FTouchEffectAnim.OnFinish := TouchEffectAnimFinish;
  FTouchEffectRectangleBitmap := ALNullDrawable;
  FOwnTouchEffectRectangleBitmap := False;
  FTouchEffectCircleBitmap := ALNullDrawable;
  FOwnTouchEffectCircleBitmap := False;
  //------
  fSelectionTimer := Ttimer.Create(nil);
  fSelectionTimer.Enabled := False;
  fSelectionTimer.Interval := 750;
  fSelectionTimer.OnTimer := SelectionTimerOnTimer;
  //------
  // We utilize TList<> as opposed to TObjectList<> because
  // TObjectList overloads the Notify method, which consequently
  // slows down all bulk insert operations.
  fSelectedItems := TList<TALDynamicScrollBoxItem>.create;
  //------
  fDblClickTimer := nil;
  //------
  fFilterDialogMainBtnVisible := false;
  FilterDialogPopupBtns := nil;
  //------
  fScrollToBeginAnim := TALFloatAnimation.Create;
  fScrollToBeginAnim.OnProcess := ScrollToBeginAnimProcess;
  fScrollToBeginAnim.OnFinish := ScrollToBeginAnimFinish;
  fScrollToBeginAnim.delay := 0;
  fScrollToBeginAnim.Duration := 0.3;
  fScrollToBeginAnim.AnimationType := TAnimationType.in;
  fScrollToBeginAnim.Interpolation := TALInterpolationType.linear;
  fScrollToBeginAnim.StartValue := 1;
  fScrollToBeginAnim.StopValue := 0;
  //-----
  FRefreshWhenGeoPosition := False;
  FNeedGeoPosition := False;
  FWaitGeoPosition := False;
  FWaitGeoPositionEvent := nil;
  FWaitGeoPositionTimer := nil;
  //-----
  FOnLoadData := nil;
  FOnLoadItems := nil;
  FOnCreateView := nil;
  FOnCreateItem := nil;
  FOnCreateLayout := nil;
  FOnAlignItems := nil;
  //-----
  FHasBeenPrepared := False;
  //-----
  FMainView := CreateMainView;
  if FMainView <> nil then
    FMainView.VisibleInPaintArea := True;
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  fScrollBar.TagObject := FMainView.ScrollEngine;
  {$ENDIF}
end;

{*****************************************************}
//*function TALDynamicScrollBox.GetCanPreload: Boolean;
//*begin
//*  result := (inherited GetCanPreload) and
//*            ((not WaitGeoPosition) or
//*             (TALGeoPositionSensor.Instance.IsListeningGeoPositionUpdates));
//*end;

{******************************************}
//*procedure TALDynamicScrollBox.Initialize;
//*begin
//*  inherited Initialize; // will set Initialized to true
//*  Resize; // to fire InitMeasures, InitBitmaps and InitPreloadOffsets
//*  doresized;
//*  FMainView.Prepare;
//*end;

{*************************************}
destructor TALDynamicScrollBox.Destroy;
begin
  ALFreeAndNil(FMainView);
//*  ALFreeAndNil(FProgressFrameStopAnimationEvent);
  ALFreeAndNil(fAniIndicatorTimer);
  ALFreeAndNil(FLongPressTimer);
  ALFreeAndNil(FTouchEffectAnim);
  AlFreeAndNil(fSelectionTimer);
  ALFreeAndNil(fSelectedItems);
  AlFreeAndNil(fDblClickTimer);
  ALFreeAndNil(FilterDialogPopupBtns);
  AlFreeAndNil(fScrollToBeginAnim);
  AlFreeAndNil(ErrorMessageBannerAnim);
  AlFreeAndNil(GPSRequiredMessageBannerAnim);
  ALFreeAndNil(FActiveScrollEngines);
  SetTouchEffectRectangleBitmap(ALNullDrawable);
  SetTouchEffectCircleBitmap(ALNullDrawable);
  SetWaitGeoPosition(false);
  FreeBitmaps;
  inherited Destroy;
end;

{************************************}
procedure TALDynamicScrollBox.Prepare;
begin
  if not FHasBeenPrepared then begin
    FMainView.Prepare;
    FHasBeenPrepared := True;
  end;
end;

{*********************************************}
//*procedure TALDynamicScrollBox.BeforeDestroy;
//*begin
//*  inherited BeforeDestroy;
//*  StopProgressFrameAnimation(True{AImmediate});
//*  if assigned(ErrorMessageBannerAnim) then
//*    ErrorMessageBannerAnim.Enabled := False;
//*  if assigned(GPSRequiredMessageBannerAnim) then
//*    GPSRequiredMessageBannerAnim.Enabled := False;
//*  fAniIndicatorTimer.Enabled := False;
//*  FLongPressTimer.Enabled := False;
//*  FTouchEffectAnim.Enabled := False;
//*  fSelectionTimer.Enabled := False;
//*  if assigned(fDblClickTimer) then fDblClickTimer.Enabled := False;
//*  fScrollToBeginAnim.enabled := False;
//*  if assigned(FWaitGeoPositionEvent) then
//*    FWaitGeoPositionEvent.SetEvent;
//*  FMainview.BeforeDestroy;
//*end;

{*************}
//[MultiThread]
//*procedure TALDynamicScrollBox.AsyncDestroy;
//*begin
//*  inherited AsyncDestroy;
//*  fMainView.AsyncDestroy;
//*end;

{**************************************}
//*procedure TALDynamicScrollBox.OnHide;
//*begin
//*  inherited OnHide;
//*  StopProgressFrameAnimation(True{AImmediate});
//*  if ActiveScrollEngines.Count = 1 then ActiveScrollEngines[0].Stop
//*  else begin
//*    var LActiveScrollEnginesArray := ActiveScrollEngines.ToArray;
//*    for var I := low(LActiveScrollEnginesArray) to high(LActiveScrollEnginesArray) do
//*      LActiveScrollEnginesArray[i].Stop;
//*  end;
//*  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
//*  FscrollBar.Visible := False;
//*  {$ENDIF}
//*end;

{**************************************}
//*procedure TALDynamicScrollBox.OnShow;
//*begin
//*  inherited;
//*  StartProgressFrameAnimation;
//*  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
//*  var LPrevOnChange := FscrollBar.OnChange;
//*  FscrollBar.OnChange := nil;
//*  Try
//*    FscrollBar.Height := MainViewHeight;
//*    FscrollBar.Position.Point := TpointF.Create(width - FscrollBar.Width, 0);
//*    FscrollBar.ViewportSize := MainViewHeight;
//*    FscrollBar.Visible := FscrollBar.Tag <> 0;
//*  finally
//*    ScrollBar.OnChange := LPrevOnChange;
//*  end;
//*  {$ENDIF}
//*  MainView.UpdateGlobalThreadPoolPriority;
//*  //----
//*  if FWaitGeoPosition and (not TALGeoPositionSensor.Instance.IsListeningGeoPositionUpdates) then
//*    TALGeoPositionSensor.Instance.StartGeoPositionUpdates
//*end;

{*********************************************************}
procedure TALDynamicScrollBox.ForceStartGeoPositionUpdates;
begin
//*  if TALGeoPositionSensor.Instance.hasLastGeoPosition then refresh
//*  else begin
    CanWaitGeoPosition := true;
    WaitGeoPosition := true;
    FRefreshWhenGeoPosition := True;
//*    TALGeoPositionSensor.Instance.StartGeoPositionUpdates(True{AForceShowRequestPermissionRationale});
//*  end;
end;

{**********************************************************************}
procedure TALDynamicScrollBox.SetWaitGeoPosition(const AValue: boolean);
begin
  FNeedGeoPosition := AValue;
  if AValue = FWaitGeoPosition then exit;
  if AValue then begin
    if not CanWaitGeoPosition then exit;
//*    if TALGeoPositionSensor.instance.hasLastGeoPosition then begin
//*      CanWaitGeoPosition := False;
//*      exit;
//*    end;
    //--
    FWaitGeoPosition := AValue;
    //--
//*    TMessageManager.DefaultManager.SubscribeToMessage(TALGeoPositionSensor.TAuthorizationStatusMessage, OnGeoPositionAuthorizationStatusMessage);
//*    TMessageManager.DefaultManager.SubscribeToMessage(TALGeoPositionSensor.TGeoPositionUpdateMessage, OnGeoPositionUpdateMessage);
    //--
    FWaitGeoPositionEvent := TEvent.Create;
    //--
    FWaitGeoPositionTimer := Ttimer.Create(nil);
    FWaitGeoPositionTimer.OnTimer := OnWaitGeoPositionTimerEvent;
    FWaitGeoPositionTimer.Interval := 30000;
//*    FWaitGeoPositionTimer.Enabled := (TALGeoPositionSensor.instance.IsListeningGeoPositionUpdates) and
//*                                     (not TALGeoPositionSensor.instance.IsActivatingGpsAndGrantingGeoPositionAccess);
  end
  else begin
    FWaitGeoPosition := AValue;
    //--
//*    TMessageManager.DefaultManager.Unsubscribe(TALGeoPositionSensor.TAuthorizationStatusMessage, OnGeoPositionAuthorizationStatusMessage);
//*    TMessageManager.DefaultManager.Unsubscribe(TALGeoPositionSensor.TGeoPositionUpdateMessage, OnGeoPositionUpdateMessage);
    //--
    ALFreeandNil(FWaitGeoPositionEvent);
    //--
    ALFreeandNil(FWaitGeoPositionTimer);
  end;
end;

{*************************************************************************************************}
procedure TALDynamicScrollBox.OnGeoPositionUpdateMessage(const Sender: TObject; const M: TMessage);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.OnGeoPositionUpdateMessage', TalLogType.verbose);
  {$ENDIF}
  AlFreeAndNil(GPSRequiredMessageBannerAnim);
  CanWaitGeoPosition := False;
  FWaitGeoPositionTimer.Enabled := False;
  FWaitGeoPositionEvent.SetEvent;
//*  if FRefreshWhenGeoPosition then refresh;
end;

{**************************************************************************************************************}
procedure TALDynamicScrollBox.OnGeoPositionAuthorizationStatusMessage(const Sender: TObject; const M: TMessage);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.OnGeoPositionAuthorizationStatusMessage', TalLogType.verbose);
  {$ENDIF}
//*  if TALGeoPositionSensor.instance.IsGpsEnabledAndGeoPositionAccessGranted then begin
//*    AlFreeAndNil(GPSRequiredMessageBannerAnim);
//*    CanWaitGeoPosition := True;
//*    FWaitGeoPositionTimer.Enabled := true;
//*    if FRefreshWhenGeoPosition then refresh;
//*  end
//*  else begin
//*    CanWaitGeoPosition := False;
//*    FWaitGeoPositionTimer.Enabled := False;
//*    FWaitGeoPositionEvent.SetEvent;
//*  end;
end;

{*************************************************************************}
procedure TALDynamicScrollBox.OnWaitGeoPositionTimerEvent(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.OnWaitGeoPositionTimerEvent', TalLogType.verbose);
  {$ENDIF}
  CanWaitGeoPosition := False;
  FWaitGeoPositionTimer.Enabled := False;
  FWaitGeoPositionEvent.SetEvent;
end;

{***********************************************************************************************************}
//*procedure TALDynamicScrollBox.HandleNotificationReceivedMessage(const Sender: TObject; const M: TMessage);
//*begin
//*  //*if IsDestroying then exit;
//*  if not HasActiveScrollEngines then Inherited HandleNotificationReceivedMessage(Sender, M)
//*  else if M is TALNotificationService.TNotificationReceivedMessage then begin
//*    var LPayload := TALStringListW.create;
//*    LPayload.Assign(TALNotificationService.TNotificationReceivedMessage(M).Payload);
//*    if not StartThread(
//*             Procedure
//*             Begin
//*
//*               Try
//*                 while True do begin
//*                   //*if IsDestroying then Break;
//*                   if not HasActiveScrollEngines then begin
//*                     var LNotificationReceivedMessage := TALNotificationService.TNotificationReceivedMessage.create(LPayload);
//*                     Try
//*                       inherited HandleNotificationReceivedMessage(self, LNotificationReceivedMessage);
//*                     finally
//*                       ALFreeAndNil(LNotificationReceivedMessage);
//*                     end;
//*                     Break;
//*                   end
//*                   else sleep(100);
//*                 end;
//*               except
//*                 On e: Exception do begin
//*                   ALLog('TALDynamicScrollBox.HandleNotificationReceivedMessage', E);
//*                 end;
//*               End;
//*
//*               ALFreeAndNil(LPayload);
//*
//*             End) then
//*      ALFreeAndNil(LPayload);
//*  end
//*  else
//*    raise Exception.Create('Error 4BE9987B-D82F-47B1-AF4E-DD9ACD6E57FC');
//*end;

{****************************************************************}
//*function TALDynamicScrollBox.GetCanRefreshByScrolling: Boolean;
//*begin
//*  result := (not fRefreshByScrollingDisabled) and fCanRefreshByScrolling;
//*end;

{***********************************************************************************}
//*procedure TALDynamicScrollBox.RefreshCreateRefreshedFrameOnTimer(Sender: TObject);
//*begin
//*  RefreshTimer.tag := RefreshTimer.tag + 1;
//*  var LScrollEngineDown := false;
//*  var LScrollEngineHasVelocity := false;
//*  for var I := 0 to ActiveScrollEngines.Count - 1 do begin
//*    LScrollEngineDown := LScrollEngineDown or ActiveScrollEngines[i].down;
//*    LScrollEngineHasVelocity := LScrollEngineHasVelocity or (not sameValue(ActiveScrollEngines[i].CurrentVelocity.y, 0, Tepsilon.Position));
//*  end;
//*  if (PopupLoading = nil) and
//*     ((LScrollEngineDown) or
//*      ((RefreshTimer.tag < 500{5 seconds}) and (LScrollEngineHasVelocity)) or // under windows I saw sometime that ScrollEngine.CurrentVelocity.y stuck on something like -15.848 :(
//*      (compareValue(MainView.ScrollEngine.ViewportPosition.y, 0, Tepsilon.Position) < 0)) then exit; // because the next create is little slow and it's can flick the anim
//*  inherited RefreshCreateRefreshedFrameOnTimer(sender);
//*end;

{***********************************************************}
//*procedure TALDynamicScrollBox.RefreshCreateRefreshedFrame;
//*begin
//*  //refreshFrame is created in descendant overriden procedure
//*  if RefreshedFrame <> nil then begin
//*    RefreshedFrame.Width := Width;
//*    RefreshedFrame.Height := Height;
//*    RefreshedFrame.AffineRatio := AffineRatio;
//*    if (not RefreshedFrame.Initialized) then RefreshedFrame.Initialize;
//*  end;
//*  inherited RefreshCreateRefreshedFrame;
//*end;

{*******************************************************************************}
//*procedure TALDynamicScrollBox.RefreshCheckReadyToSwapOnTimer(Sender: TObject);
//*begin
//*  Var LRefreshedDynamicScrollBoxFrame := TALDynamicScrollBox(RefreshedFrame);
//*  if LRefreshedDynamicScrollBoxFrame = nil then exit;
//*  if (not LRefreshedDynamicScrollBoxFrame.MainView.GrowDataExecutedAtLeastOnce) or
//*     (not LRefreshedDynamicScrollBoxFrame.MainView.GrowItemsExecutedAtLeastOnce) then exit;
//*  //-----
//*  for var I := 0 to ActiveScrollEngines.Count - 1 do
//*    if ActiveScrollEngines[i].down then exit;
//*  //-----
//*  inherited RefreshCheckReadyToSwapOnTimer(sender);
//*end;

{*****************************************}
Procedure TALDynamicScrollBox.InitMeasures;
begin
  //*beginWrite;
  try
    DoInitMeasures;
  finally
    //*endWrite;
  end;
end;

{*******************************************}
Procedure TALDynamicScrollBox.DoInitMeasures;
begin
//*  {$IFDEF DEBUG}
//*  allog(
//*    ClassName+'.DoInitMeasures',
//*    'MainViewWidth:' + ALFloatToStrW(MainViewWidth, ALDefaultFormatSettingsW) + ' | '+
//*    'MainViewHeight:' + ALFloatToStrW(MainViewHeight, ALDefaultFormatSettingsW),
//*    TalLogType.verbose);
//*  {$ENDIF}
//*  //-----
//*  ErrorMessageBannerTextSize := ALAlignDimensionToPixelRound(14 * AffineRatio, ScreenScale);
//*  ErrorMessageBannerLineSpacing := ALAlignDimensionToPixelRound(1 * AffineRatio, ScreenScale);
//*  ErrorMessageBannerPadding := ALAlignDimensionToPixelRound(12 * AffineRatio, ScreenScale);
//*  //-----
//*  GPSRequiredMessageBannerTextSize := ALAlignDimensionToPixelRound(14 * AffineRatio, ScreenScale);
//*  GPSRequiredMessageBannerLineSpacing := ALAlignDimensionToPixelRound(1 * AffineRatio, ScreenScale);
//*  GPSRequiredMessageBannerPadding := ALAlignDimensionToPixelRound(12 * AffineRatio, ScreenScale);
//*  //-----
//*  ItemsReloadBtnWidth := ALAlignDimensionToPixelRound(48 * AffineRatio, ScreenScale);
//*  ItemsReloadBtnHeight := ItemsReloadBtnWidth;
//*  ItemsReloadBtnMarginTop := ALAlignDimensionToPixelRound(20 * AffineRatio, ScreenScale);
//*  ItemsReloadBtnMarginBottom := ItemsReloadBtnMarginTop;
//*  ItemsReloadBtnMarginRight := ALAlignDimensionToPixelRound(20 * AffineRatio, ScreenScale);
//*  ItemsReloadBtnMarginLeft := ItemsReloadBtnMarginRight;
//*  //-----
//*  RefreshIconWidth := ALAlignDimensionToPixelRound(45 * AffineRatio, ScreenScale);
//*  RefreshIconHeight := RefreshIconWidth;
//*  //-----
//*  GrowItemsAniindicatorMarginTop := ALAlignDimensionToPixelRound(ItemsReloadBtnMarginTop + ((ItemsReloadBtnHeight - TALMainForm.Instance.AniindicatorHeight) / 2), ScreenScale); // 26 = 20 + (48(fItemsReloadBtnHeight) - 36(fAniindicatorHeight) / 2) => to not have a resize when we press the reload btn
//*  GrowItemsAniindicatorMarginBottom := GrowItemsAniindicatorMarginTop;
//*  GrowItemsAniindicatorMarginRight := ALAlignDimensionToPixelRound(ItemsReloadBtnMarginRight + ((ItemsReloadBtnWidth - TALMainForm.Instance.AniindicatorWidth) / 2), ScreenScale); // 26 = 20 + (48(fItemsReloadBtnWidth) - 36(fAniindicatorWidth) / 2) => to not have a resize when we press the reload btn
//*  GrowItemsAniindicatorMarginLeft := GrowItemsAniindicatorMarginTop;
//*  //-----
//*  NoItemsMarginTop := ALAlignDimensionToPixelRound(25 * AffineRatio, ScreenScale);
//*  NoItemsMarginBottom := ALAlignDimensionToPixelRound(25 * AffineRatio, ScreenScale);
//*  NoItemsMarginLeft := ALAlignDimensionToPixelRound(25 * AffineRatio, ScreenScale);
//*  NoItemsMarginRight := ALAlignDimensionToPixelRound(25 * AffineRatio, ScreenScale);
//*  NoItemsIconWidth := ALAlignDimensionToPixelRound(70 * AffineRatio, ScreenScale);
//*  NoItemsIconHeight := NoItemsIconWidth;
//*  NoItemsTitleTextSize := ALAlignDimensionToPixelRound(22 * AffineRatio, ScreenScale);
//*  NoItemsTitleLineSpacing := ALAlignDimensionToPixelRound(2 * AffineRatio, ScreenScale);
//*  NoItemsTitleMarginTop := ALAlignDimensionToPixelRound(16 * AffineRatio, ScreenScale);
//*  NoItemsSubTitleTextSize := ALAlignDimensionToPixelRound(17 * AffineRatio, ScreenScale);
//*  NoItemsSubTitleLineSpacing := ALAlignDimensionToPixelRound(2 * AffineRatio, ScreenScale);
//*  NoItemsSubTitleMarginTop := ALAlignDimensionToPixelRound(16 * AffineRatio, ScreenScale);
//*  NoItemsBtnTextSize := ALAlignDimensionToPixelRound(17 * AffineRatio, ScreenScale);
//*  NoItemsBtnLineSpacing := ALAlignDimensionToPixelRound(2 * AffineRatio, ScreenScale);
//*  NoItemsBtnMarginTop := ALAlignDimensionToPixelRound(25 * AffineRatio, ScreenScale);
//*  NoItemsBtnRadius := ALAlignDimensionToPixelRound(16 * AffineRatio, ScreenScale);
//*  NoItemsBtnPaddingX := ALAlignDimensionToPixelRound(26 * AffineRatio, ScreenScale);
//*  NoItemsBtnPaddingY := ALAlignDimensionToPixelRound(12 * AffineRatio, ScreenScale);
//*  //-----
//*  FilterDialogMainBtnRect := ALAlignDimensionToPixelRound(
//*                               TrectF.create(
//*                                 0, 0,
//*                                 87 * AffineRatio,
//*                                 87 * AffineRatio),
//*                               ScreenScale);
//*  FilterDialogMainBtnRect.Offset(
//*    MainViewWidth - FilterDialogMainBtnRect.width - ALAlignDimensionToPixelRound(5 * AffineRatio, ScreenScale),
//*    MainViewHeight - FilterDialogMainBtnRect.height - ALAlignDimensionToPixelRound(5 * AffineRatio, ScreenScale));
//*  //-----
//*  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
//*  {$IFDEF debug}
//*  TouchTargetExpansion := ALAlignDimensionToPixelRound(10 * AffineRatio, ScreenScale);
//*  {$ELSE}
//*  TouchTargetExpansion := 0;
//*  {$ENDIF}
//*  {$ELSE}
//*  TouchTargetExpansion := ALAlignDimensionToPixelRound(10 * AffineRatio, ScreenScale);
//*  {$ENDIF}
end;

{***********************************************}
Procedure TALDynamicScrollBox.InitPreloadOffsets;
begin
  //*beginWrite;
  try
    DoInitPreloadOffsets;
    {$IFDEF DEBUG}
    ALLog(
      Classname+'.InitPreloadOffsets',
      'ItemsPreloadOffset: '+ALIntToStrW(ItemsPreloadOffset),
      TalLogType.verbose);
    {$ENDIF}
  finally
    //*endWrite;
  end;
end;

{*************************************************}
Procedure TALDynamicScrollBox.DoInitPreloadOffsets;
begin
  ItemsPreloadOffset := (*0*) 50;
end;

{****************************************}
Procedure TALDynamicScrollBox.InitBitmaps;
begin
  //*beginWrite;
  try
    DoInitBitmaps;
  finally
    //*endWrite;
  end;
end;

{******************************************}
Procedure TALDynamicScrollBox.DoInitBitmaps;
begin

  //-----
  {$IFDEF DEBUG}
  allog(
    ClassName+'.DoInitBitmaps',
    'MainViewWidth:' + ALFloatToStrW(MainViewWidth, ALDefaultFormatSettingsW) + ' | '+
    'MainViewHeight:' + ALFloatToStrW(MainViewHeight, ALDefaultFormatSettingsW),
    TalLogType.verbose);
  {$ENDIF}

  //-----
  FreeBitmaps;

  //-----
  //fErrorMessageBanner
  //fGPSRequiredMessageBanner
  //fItemsReloadBtn
  //fRefreshIcon
  //fRefreshingAniIcon
  //fNoItemsIcon
  //fNoItemsTitle
  //fNoItemsSubTitle
  //fNoItemsBtn
  //FilterDialogMainBtn
  //ActionBarPreviousBtn
  //ActionBarRefreshBtn

end;

{****************************************}
Procedure TALDynamicScrollBox.FreeBitmaps;
begin
  {$IFDEF DEBUG}
  ALLog(Classname+'.FreeBitmaps', TalLogType.verbose);
  {$ENDIF}
  AlFreeAndNilDrawable(fErrorMessageBanner);
  AlFreeAndNilDrawable(fGPSRequiredMessageBanner);
  AlFreeAndNilDrawable(fItemsReloadBtn);
  AlFreeAndNilDrawable(fRefreshIcon);
  AlFreeAndNilDrawable(fRefreshingAniIcon);
  AlFreeAndNilDrawable(fNoItemsIcon);
  AlFreeAndNilDrawable(fNoItemsTitle);
  AlFreeAndNilDrawable(fNoItemsSubTitle);
  AlFreeAndNilDrawable(fNoItemsBtn);
  AlFreeAndNilDrawable(fFilterDialogMainBtn);
end;

{*************************************************************}
class function TALDynamicScrollBox.FitIntoAndCropAsCircleImage(
                 const aSender: Tobject;
                 const aPicStream: TCustomMemoryStream;
                 const aPicUrl: String;
                 const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
                 Const aPicBlurred: single;
                 const aPicCropCenter: TPointF;
                 const aCornerRadius: TPointF; // Useless here
                 const aScreenScale: single): TALDrawable;
begin
//*  result := ALFitIntoAndCropAsCircleImage(
//*              aPicStream, // const aPicStream: TCustomMemoryStream;
//*              aPicUrl, // const aPicUrl: String;
//*              aDestSize, // const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
//*              aPicBlurred, // Const aPicBlurred: single;
//*              aPicCropCenter, // const aPicCropCenter: TPointF;
//*              aScreenScale); // const aScreenScale: single)
end;

{*****************************************************}
class function TALDynamicScrollBox.FitIntoAndCropImage(
                 const aSender: Tobject;
                 const aPicStream: TCustomMemoryStream;
                 const aPicUrl: String;
                 const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
                 Const aPicBlurred: single;
                 const aPicCropCenter: TPointF;
                 const aCornerRadius: TPointF;
                 const aScreenScale: single): TALDrawable;
begin
//*  result := ALFitIntoAndCropImage(
//*              aPicStream, // const aPicStream: TCustomMemoryStream;
//*              aPicUrl, // const aPicUrl: String;
//*              aDestSize, // const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
//*              aPicBlurred, // Const aPicBlurred: single;
//*              aPicCropCenter, // const aPicCropCenter: TPointF;
//*              aCornerRadius, // const aCornerRadius: TPointF;
//*              aScreenScale); // const aScreenScale: single): TALDrawable;
end;

{*******************************************************}
class function TALDynamicScrollBox.PlaceIntoAndCropImage(
                 const aSender: Tobject;
                 const aPicStream: TCustomMemoryStream;
                 const aPicUrl: String;
                 const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
                 Const aPicBlurred: single;
                 const aPicCropCenter: TPointF;
                 const aCornerRadius: TPointF;
                 const aScreenScale: single): TALDrawable;
begin
//*  result := ALPlaceIntoAndCropImage(
//*              aPicStream, // const aPicStream: TCustomMemoryStream;
//*              aPicUrl, // const aPicUrl: String;
//*              aDestSize, // const aDestSize: TSizeF; // << the DEST size of the bitmap in virtual pixel
//*              aPicBlurred, // Const aPicBlurred: single;
//*              aPicCropCenter, // const aPicCropCenter: TPointF;
//*              aCornerRadius, // const aCornerRadius: TPointF;
//*              aScreenScale); // const aScreenScale: single): TALDrawable;
end;

{*********************************************************}
procedure TALDynamicScrollBox.CreateErrorMessageBannerAnim;
begin
  if ErrorMessageBannerAnim = nil then begin
    ErrorMessageBannerAnim := TALFloatAnimation.Create;
//*    ErrorMessageBannerAnim.OnProcess := BasicAnimProcess;
//*    ErrorMessageBannerAnim.OnFinish := BasicAnimFinish;
    ErrorMessageBannerAnim.delay := 5;
    ErrorMessageBannerAnim.Duration := 0.3;
    ErrorMessageBannerAnim.AnimationType := TAnimationType.in;
    ErrorMessageBannerAnim.Interpolation := TALInterpolationType.Linear;
    ErrorMessageBannerAnim.StartValue := 1;
    ErrorMessageBannerAnim.StopValue := 0;
  end;
  ErrorMessageBannerAnim.Enabled := true;
end;

{***************************************************************}
procedure TALDynamicScrollBox.CreateGPSRequiredMessageBannerAnim;
begin
  if GPSRequiredMessageBannerAnim = nil then begin
    GPSRequiredMessageBannerAnim := TALFloatAnimation.Create;
//*    GPSRequiredMessageBannerAnim.OnProcess := BasicAnimProcess;
//*    GPSRequiredMessageBannerAnim.OnFinish := BasicAnimFinish;
    GPSRequiredMessageBannerAnim.delay := 60;
    GPSRequiredMessageBannerAnim.Duration := 0.3;
    GPSRequiredMessageBannerAnim.AnimationType := TAnimationType.in;
    GPSRequiredMessageBannerAnim.Interpolation := TALInterpolationType.Linear;
    GPSRequiredMessageBannerAnim.StartValue := 1;
    GPSRequiredMessageBannerAnim.StopValue := 0;
  end;
  GPSRequiredMessageBannerAnim.Enabled := true;
end;

{**************************************************************}
function TALDynamicScrollBox.GetErrorMessageBanner: TALDrawable;
begin
  if ALIsDrawableNull(fErrorMessageBanner) then fErrorMessageBanner := DrawErrorMessageBanner;
  result := fErrorMessageBanner;
end;

{********************************************************************}
function TALDynamicScrollBox.GetGPSRequiredMessageBanner: TALDrawable;
begin
  if ALIsDrawableNull(fGPSRequiredMessageBanner) then fGPSRequiredMessageBanner := DrawGPSRequiredMessageBanner;
  result := fGPSRequiredMessageBanner;
end;

{***************************************************************}
function TALDynamicScrollBox.DrawErrorMessageBanner: TALDrawable;
begin
//*  var LOptions := TALMultiLineTextOptions.Create;
//*  Try
//*    LOptions.FontName := ALGetFontFamily('sans-serif');
//*    LOptions.FontSize := ErrorMessageBannerTextSize * ScreenScale;
//*    LOptions.FontStyle := [];
//*    LOptions.FontColor := $ffffffff;
//*    //-----
//*    //EllipsisText: String; // default = '…';
//*    //EllipsisFontStyle: TFontStyles; // default = [];
//*    //EllipsisFontColor: TalphaColor; // default = TAlphaColorRec.Null;
//*    //-----
//*    LOptions.AutoSize := False;
//*    LOptions.AutosizeY := True;
//*    //LOptions.AutoSizeY: Boolean; // default = False;
//*    LOptions.WordWrap := True;
//*    //LOptions.MaxLines: integer; // default = 0;
//*    LOptions.LineSpacing := ErrorMessageBannerLineSpacing * ScreenScale;
//*    //LOptions.Trimming: TTextTrimming; // default = TTextTrimming.Character;
//*    //LOptions.FailIfTextBroken: boolean; // default = false
//*    //-----
//*    LOptions.HTextAlign := tTextalign.Center;
//*    LOptions.VTextAlign := tTextalign.Center;
//*    //-----
//*    LOptions.Fill.Kind := TbrushKind.Solid;
//*    LOptions.Fill.Color := $A0000000;
//*    //LOptions.Stroke: TStrokeBrush; // default = none
//*    //LOptions.Sides: TSides; // default = AllSides
//*    //LOptions.XRadius := 8; // default = 0
//*    //LOptions.YRadius := 8; // default = 0
//*    //LOptions.Corners: TCorners; // default = AllCorners
//*    LOptions.Padding := TrectF.Create(
//*                          ErrorMessageBannerPadding,
//*                          ErrorMessageBannerPadding,
//*                          ErrorMessageBannerPadding,
//*                          ErrorMessageBannerPadding);
//*    //-----
//*    LOptions.TextIsHtml:= True;
//*    //-----
//*    var LRect := TrectF.Create(0,0,(MainViewWidth*ScreenScale),10000);
//*    Result := ALDrawMultiLineText(
//*                ALTranslate('_NoInternetConnection2'), // const aText: String; // support only basic html tag like <b>...</b>, <i>...</i>, <font color="#ffffff">...</font> and <span id="xxx">...</span>
//*                LRect, // var LRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
//*                LOptions);
//*  finally
//*    ALFreeAndNil(LOptions);
//*  end;
end;

{*********************************************************************}
function TALDynamicScrollBox.DrawGPSRequiredMessageBanner: TALDrawable;
begin
//*  var LOptions := TALDrawMultiLineTextOptions.Create;
//*  Try
//*    LOptions.FontName := ALGetFontFamily('sans-serif');
//*    LOptions.FontSize := GPSRequiredMessageBannerTextSize * ScreenScale;
//*    LOptions.FontStyle := [];
//*    LOptions.FontColor := $ffffffff;
//*    //-----
//*    //EllipsisText: String; // default = '…';
//*    //EllipsisFontStyle: TFontStyles; // default = [];
//*    //EllipsisFontColor: TalphaColor; // default = TAlphaColorRec.Null;
//*    //-----
//*    LOptions.AutoSize := False;
//*    LOptions.AutosizeY := True;
//*    //LOptions.AutoSizeY: Boolean; // default = False;
//*    LOptions.WordWrap := True;
//*    //LOptions.MaxLines: integer; // default = 0;
//*    LOptions.LineSpacing := GPSRequiredMessageBannerLineSpacing * ScreenScale;
//*    //LOptions.Trimming: TTextTrimming; // default = TTextTrimming.Character;
//*    //LOptions.FailIfTextBroken: boolean; // default = false
//*    //-----
//*    LOptions.HTextAlign := tTextalign.Center;
//*    LOptions.VTextAlign := tTextalign.Center;
//*    //-----
//*    LOptions.Fill.Kind := TbrushKind.Solid;
//*    LOptions.Fill.Color := $A0000000;
//*    //LOptions.Stroke: TStrokeBrush; // default = none
//*    //LOptions.Sides: TSides; // default = AllSides
//*    //LOptions.XRadius := 8; // default = 0
//*    //LOptions.YRadius := 8; // default = 0
//*    //LOptions.Corners: TCorners; // default = AllCorners
//*    LOptions.Padding := TrectF.Create(
//*                          GPSRequiredMessageBannerPadding,
//*                          GPSRequiredMessageBannerPadding,
//*                          GPSRequiredMessageBannerPadding,
//*                          GPSRequiredMessageBannerPadding);
//*    //-----
//*    LOptions.TextIsHtml:= True;
//*    //-----
//*    var LRect := TrectF.Create(0,0,(MainViewWidth*ScreenScale),10000);
//*    Result := ALDrawMultiLineText(
//*                ALTranslate('_LocationServicesInformationBanner'), // const aText: String; // support only basic html tag like <b>...</b>, <i>...</i>, <font color="#ffffff">...</font> and <span id="xxx">...</span>
//*                LRect, // var LRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
//*                LOptions);
//*  finally
//*    ALFreeAndNil(LOptions);
//*  end;
end;

{**********************************************************}
function TALDynamicScrollBox.GetItemsReloadBtn: TALDrawable;
begin
  if ALIsDrawableNull(fItemsReloadBtn) then fItemsReloadBtn := DrawItemsReloadBtn;
  result := fItemsReloadBtn;
end;

{***********************************************************}
function TALDynamicScrollBox.DrawItemsReloadBtn: TALDrawable;
begin
//*  Result := ALloadFitIntoResourceImageV3(
//*              'reload_192x192',
//*              Round(ItemsReloadBtnWidth * ScreenScale),
//*              Round(ItemsReloadBtnHeight * ScreenScale));
end;

{*******************************************************}
function TALDynamicScrollBox.GetRefreshIcon: TALDrawable;
begin
  if ALIsDrawableNull(fRefreshIcon) then fRefreshIcon := DrawRefreshIcon;
  result := fRefreshIcon;
end;

{********************************************************}
function TALDynamicScrollBox.DrawRefreshIcon: TALDrawable;
begin
//*  Result := ALloadFitIntoResourceImageV3(
//*              'reload_180x180',
//*              Round(RefreshIconWidth * ScreenScale),
//*              Round(RefreshIconHeight * ScreenScale));
end;

{*************************************************************}
function TALDynamicScrollBox.GetRefreshingAniIcon: TALDrawable;
begin
  if ALIsDrawableNull(FRefreshingAniIcon) then FRefreshingAniIcon := DrawRefreshingAniIcon;
  result := FRefreshingAniIcon;
end;

{**************************************************************}
function TALDynamicScrollBox.DrawRefreshingAniIcon: TALDrawable;
begin
//*  Result := ALloadFitIntoResourceImageV3(
//*              'reloadani_180x180',
//*              Round(RefreshIconWidth * ScreenScale),
//*              Round(RefreshIconHeight * ScreenScale));
end;

{*******************************************************}
function TALDynamicScrollBox.GetNoItemsIcon: TALDrawable;
begin
  if ALIsDrawableNull(fNoItemsIcon) then fNoItemsIcon := DrawNoItemsIcon;
  result := fNoItemsIcon;
end;

{********************************************************}
function TALDynamicScrollBox.DrawNoItemsIcon: TALDrawable;
begin
  result := ALNullDrawable;
end;

{********************************************************}
function TALDynamicScrollBox.GetNoItemsTitle: TALDrawable;
begin
  if ALIsDrawableNull(fNoItemsTitle) then fNoItemsTitle := DrawNoItemsTitle;
  result := fNoItemsTitle;
end;

{*********************************************************}
function TALDynamicScrollBox.DrawNoItemsTitle: TALDrawable;
begin
  result := ALNullDrawable;
end;

{***********************************************************}
function TALDynamicScrollBox.GetNoItemsSubTitle: TALDrawable;
begin
  if ALIsDrawableNull(fNoItemsSubTitle) then fNoItemsSubTitle := DrawNoItemsSubTitle;
  result := fNoItemsSubTitle;
end;

{************************************************************}
function TALDynamicScrollBox.DrawNoItemsSubTitle: TALDrawable;
begin
  result := ALNullDrawable;
end;

{******************************************************}
function TALDynamicScrollBox.GetNoItemsBtn: TALDrawable;
begin
  if ALIsDrawableNull(fNoItemsBtn) then fNoItemsBtn := DrawNoItemsBtn;
  result := fNoItemsBtn;
end;

{*******************************************************}
function TALDynamicScrollBox.DrawNoItemsBtn: TALDrawable;
begin
  result := ALNullDrawable;
end;

{***************************************************************}
function TALDynamicScrollBox.GetFilterDialogMainBtn: TALDrawable;
begin
  if ALIsDrawableNull(fFilterDialogMainBtn) then fFilterDialogMainBtn := DrawFilterDialogMainBtn;
  result := fFilterDialogMainBtn;
end;

{****************************************************************}
function TALDynamicScrollBox.DrawFilterDialogMainBtn: TALDrawable;
begin
  Var LResName := GetFilterDialogMainBtnResName;
  if LResName = '' then exit(ALNullDrawable);
  var W := Round(FilterDialogMainBtnRect.width * ALGetScreenScale);
  var H := Round(FilterDialogMainBtnRect.height * ALGetScreenScale);
//*  Result := ALloadFitIntoResourceImageV3(LResName, W, h);
end;

{*******************************************************************}
function TALDynamicScrollBox.CreateMainView: TALDynamicScrollBoxView;
begin
  If assigned(OnCreateView) then
    Result := OnCreateView(Self)
  else
    result := TALDynamicScrollBoxView.Create(self);
end;

{****************************************************************}
function TALDynamicScrollBox.CalculateMainViewLocalRect: TALRectD;
begin
  Result := TALRectD.Create(LocalRect);
(*
  for var LControl in Controls do begin
    case LControl.Align of
      //--
      TALAlignLayout.MostTop,
      TALAlignLayout.Top: Result.Top := Max(Result.Top, LControl.Position.Y + LControl.Height);
      //--
      TALAlignLayout.MostLeft,
      TALAlignLayout.Left: Result.left := Max(Result.left, LControl.Position.x + LControl.Width);
      //--
      TALAlignLayout.MostRight,
      TALAlignLayout.Right: Result.Right := min(Result.Right, LControl.Position.x);
      //--
      TALAlignLayout.MostBottom,
      TALAlignLayout.Bottom: Result.Bottom := min(Result.Bottom, LControl.Position.y);
      //--
      TALAlignLayout.None:;
      TALAlignLayout.Client:;
      TALAlignLayout.Contents:;
      TALAlignLayout.Center:;
      TALAlignLayout.VertCenter:;
      TALAlignLayout.HorzCenter:;
      TALAlignLayout.Horizontal:;
      TALAlignLayout.Vertical:;
      TALAlignLayout.Scale:;
      TALAlignLayout.Fit:;
      TALAlignLayout.FitLeft:;
      TALAlignLayout.FitRight:;
      else raise Exception.Create('Error 1886C1FB-97AD-4319-AC53-AB25D1B150F0');
    end;
  end;
*)
end;

{*****************************************************************}
procedure TALDynamicScrollBox.AniIndicatorOnTimer(Sender: TObject);
begin
  inc(fAniindicatorImgIndex.x);
  if fAniindicatorImgIndex.x >= 5 then begin
    fAniindicatorImgIndex.x := 0;
    inc(fAniindicatorImgIndex.Y);
    if fAniindicatorImgIndex.Y >= 4 then fAniindicatorImgIndex.Y := 0;
  end;
  repaint;
end;

{*************************************}
procedure TALDynamicScrollBox.Painting;
begin

  {$REGION 'ALLog'}
  {$IFDEF DEBUG}
  //ALLog(ClassName + '.FramePaint', TalLogType.verbose);
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'Prepare The MainView'}
  Prepare;
  {$ENDREGION}

  {$REGION 'init local vars'}
  var LEphemeralLayout := MainView.EnforceEphemeralLayout(false{a4preload});
  if LEphemeralLayout = nil then exit;
  var LRect := LocalRect;
  FAniIndicatorEnabled := False;
  {$ENDREGION}

  {$REGION 'LogFPS'}
  {$IFDEF DEBUG}
//*  if MainView.ScrollEngine.TouchTracking = [ttVertical] then
//*    LogFPS(MainView.ScrollEngine);
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'DoFramePaint'}
  DoPainting(Self, Canvas, LRect, 1{aOpacity});
  {$ENDREGION}

  {$REGION 'draw the ErrorMessageBanner'}
  if (assigned(ErrorMessageBannerAnim)) and
     (ErrorMessageBannerAnim.Enabled) then begin
//*    drawBitmap(
//*      Canvas,
//*      LRect,
//*      ErrorMessageBanner, // aBitmap
//*      TpointF.Create(0,0), // aTopLeft
//*      ErrorMessageBannerAnim.CurrentValue); // aOpacity
  end
  {$ENDREGION}

  {$REGION 'draw the GPSRequiredMessageBanner'}
  else if (assigned(GPSRequiredMessageBannerAnim)) and
          (GPSRequiredMessageBannerAnim.Enabled) then begin
//*    drawBitmap(
//*      Canvas,
//*      LRect,
//*      GPSRequiredMessageBanner, // aBitmap
//*      TpointF.Create(0,0), // aTopLeft
//*      GPSRequiredMessageBannerAnim.CurrentValue); // aOpacity
  end;
  {$ENDREGION}

  {$REGION 'draw the RefreshIcon'}
//*  if (IsRefreshing or CanRefreshByScrolling) and
//*     (PopupLoading = nil) then begin
//*
//*    //-----
//*    var LRotationCenter := TpointF.Create(
//*                             (MainViewWidth / 2),
//*                             (-1 * (max(-RefreshIconHeight, MainView.ScrollPos) + RefreshIconHeight + (min(0, MainView.ScrollPos + RefreshIconHeight) * 2))) + (RefreshIconHeight / 2));
//*    //-----
//*    if IsRefreshing then begin
//*
//*      if CanRefreshByScrolling then
//*        LRotationCenter.Y := max(
//*                               LRotationCenter.Y,
//*                               (RefreshIconHeight + (RefreshIconHeight / 2)))
//*      else
//*        LRotationCenter.Y := min(
//*                               (RefreshIconHeight + (RefreshIconHeight / 2)),
//*                               (RefreshIconHeight + (RefreshIconHeight / 2)) - MainView.ScrollPos);
//*      if drawRotatedBitmap(
//*           Canvas,
//*           LRect,
//*           RefreshingAniIcon, // aBitmap
//*           LRotationCenter, // aRotationCenter
//*           ((fAniindicatorImgIndex.x) + (fAniindicatorImgIndex.Y * 5)) * 18,
//*           1{aOpacity}) then FAniIndicatorEnabled := true;
//*
//*    end
//*
//*    //-----
//*    else begin
//*
//*      drawRotatedBitmap(
//*        Canvas,
//*        LRect,
//*        RefreshIcon, // aBitmap
//*        LRotationCenter, // aRotationCenter
//*        round((360 / (2*RefreshIconHeight)) * (LRotationCenter.Y)) mod 360,
//*        1); // aOpacity
//*
//*    end;
//*  end;
  {$ENDREGION}

  {$REGION 'Enable / Deactivate FAniindicatorTimer'}
//*  FAniindicatorTimer.enabled := FAniIndicatorEnabled;
  {$ENDREGION}

end;

{**********************************************************************************************************************}
procedure TALDynamicScrollBox.DoPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF; Const aOpacity: Single);
begin

  {$REGION 'draw the main View'}
  var LSavedMatrix := Canvas.Matrix;
  try
    Var LDeltaRect := MainView.BoundsRect.ReducePrecision;
    var LMatrix := LSavedMatrix * TMatrix.CreateTranslation(LDeltaRect.left,LDeltaRect.Top);
    Canvas.SetMatrix(LMatrix);
    Var LDeltaTopLeft := LDeltaRect.TopLeft;
    LDeltaRect := TRectF.Intersect(ARect, LDeltaRect);
    LDeltaRect.Offset(-LDeltaTopLeft);
    MainView.Paint(Canvas, LDeltaRect, aOpacity);
  finally
    Canvas.SetMatrix(LSavedMatrix);
  end;
  {$ENDREGION}

  {$REGION 'draw the FilterDialogMainBtnBitmap'}
  if (fFilterDialogMainBtnVisible) then begin
//*    drawBitmap(
//*      Canvas,
//*      ARect,
//*      FilterDialogMainBtn, // aBitmap
//*      FilterDialogMainBtnRect.topLeft, // aTopLeft
//*      aOpacity); // aOpacity
  end;
  {$ENDREGION}

end;

{*******************************************************************}
procedure TALDynamicScrollBox.SelectionTimerOnTimer(Sender: TObject);
begin
  FSelectionTimer.Enabled := False;
  if FSelectionTimer.TagObject = nil then exit;
  var LMousePos: TPointF;
  var LMouseService: IFMXMouseService;
//*  if TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, LMouseService) then
//*    LMousePos := TALMainForm.Instance.ScreenToClient(LMouseService.GetMousePos)
//*  else
//*    LMousePos := TPointF.Create(0,0);
//*  if (LMousePos.Distance(TALTouchEventTracking.OnMouseDownPointF) < TALTouchEventTracking.MinDistanceThreshold) then
//*    ToggleSelection(TALDynamicScrollBoxItem(FSelectionTimer.TagObject));
end;

{**********************************************************************************}
procedure TALDynamicScrollBox.ToggleSelection(Const aItem: TALDynamicScrollBoxItem);
begin
  if aItem.Selected then begin
    aItem.Selected := false;
    {$IFDEF DEBUG}
    if not fSelectedItems.Contains(aItem) then
      raise Exception.Create('Error 521E4C85-9B36-43F4-9AE4-A55E03680C4C');
    {$ENDIF}
    fSelectedItems.Remove(aItem);
  end
  else begin
    aItem.Selected := true;
    {$IFDEF DEBUG}
    if fSelectedItems.Contains(aItem) then
      raise Exception.Create('Error 07E18B0E-FF9C-4DC6-94AC-0A297A3B6A21');
    {$ENDIF}
    fSelectedItems.Add(aItem);
  end;
//*  InitActionBar;
  repaint;
end;

{********************************************}
procedure TALDynamicScrollBox.CancelSelection;
begin
  for var LItem in fSelectedItems do
    LItem.Selected := False;
  fSelectedItems.Clear;
//*  InitActionBar;
  repaint;
end;

{********************************************}
procedure TALDynamicScrollBox.DeleteSelection;
begin
  DoDeleteSelection;
  for var LItem in SelectedItems do begin
    if LItem.OwnerView = nil then
      raise Exception.Create('Error 91512BC4-6EFC-49E4-991F-F36A097B7B85');
    LItem.OwnerView.DeleteItem(LItem);
  end;
  SelectedItems.Clear;
//*  InitActionBar;
  repaint;
end;

{**********************************************}
procedure TALDynamicScrollBox.DoDeleteSelection;
begin
  //virtual
  //DoDeleteSelection must call an async dataprovider function
end;

{****************************************************}
function TALDynamicScrollBox.GetDblClickTimer: TTimer;
begin
  if fDblClickTimer = nil then begin
    fDblClickTimer := Ttimer.Create(nil);
    fDblClickTimer.Enabled := False;
    fDblClickTimer.Interval := 350;
  end;
  result := fDblClickTimer;
end;

{****************************************************************}
procedure TALDynamicScrollBox.DblClickOffOnTimer(Sender: TObject);
begin
  DblClickTimer.Enabled := False;
end;

{**********************************************************************************************}
procedure TALDynamicScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseDown(Button, Shift, X, Y);
  //--
  {$IFDEF DEBUG}
  ALLog(
    ClassName + '.MouseDown',
    'x: ' + ALFloatToStrW(X, ALDefaultFormatSettingsW) + ' | ' +
    'y: ' + ALFloatToStrW(y, ALDefaultFormatSettingsW),
    TalLogType.verbose);
  {$ENDIF}
  //--
  var LMousePos := TPointF.Create(X,Y);
  var LScrollEngineCurrentVelocity: Double := 0;
  var LScrollEngines := MainView.GetScrollEnginesAtPos(MainView.FrameToLocal(LMousePos).ReducePrecision);
  for var I := low(LScrollEngines) to high(LScrollEngines) do begin
    var LScrollEngine := LScrollEngines[i];
    if LScrollEngine.TouchTracking = [ttVertical] then begin
      if abs(LScrollEngine.currentvelocity.Y) > abs(LScrollEngineCurrentVelocity) then
        LScrollEngineCurrentVelocity := LScrollEngine.currentvelocity.Y;
    end
    else if LScrollEngine.TouchTracking = [ttHorizontal] then begin
      if abs(LScrollEngine.currentvelocity.x) > abs(LScrollEngineCurrentVelocity) then
        LScrollEngineCurrentVelocity := LScrollEngine.currentvelocity.x;
    end
    else raise Exception.Create('Error 73A0351F-EDF4-4D10-AA3B-2F37284594A4');
    LScrollEngine.MouseDown(X, Y);
  end;
  //--
  //update the TALTouchEventTracking.OnMouseDownVelocity because it's was not
  //calculated in TALMainForm.MouseDown
//*  TALTouchEventTracking.OnMouseDown(Self, Button, Shift, X, Y, LScrollEngineCurrentVelocity);
  //--
//*  if (not IsRefreshing) then begin
//*    if SameValue(MainView.ScrollEngine.viewPortPosition.y, 0, 1{Epsilon}) then CanRefreshByScrolling := true
//*    else CanRefreshByScrolling := false;
//*  end;
  //--
  //mirrored in Click
  if assigned(ErrorMessageBannerAnim) and
     (ErrorMessageBannerAnim.Enabled) and
     (Not ALisDrawableNull(FErrorMessageBanner)) and
     (LMousePos.y <= ALGetDrawableHeight(FErrorMessageBanner)) then begin
    exit; // the event will be handled in onclick
  end
  else if assigned(GPSRequiredMessageBannerAnim) and
     (GPSRequiredMessageBannerAnim.Enabled) and
     (Not ALIsDrawableNull(FGPSRequiredMessageBanner)) and
     (LMousePos.y <= ALGetDrawableHeight(FGPSRequiredMessageBanner)) then begin
    exit; // the event will be handled in onclick
  end;
  //--
  if (fFilterDialogMainBtnVisible) and
     (FilterDialogMainBtnRect.Contains(LMousePos)) then begin
//*    TALTouchEffectBuilder.create(self)
//*      .Bounds(FilterDialogMainBtnRect) // default TRectf.Empty
//*      .Kind(TALTouchEffectKind.Circle) // default TALTouchEffectKind.RectangleAndCircle
//*      .ClipChildren(false) // default True
//*      .OnFinishEvent(FilterDialogMainBtnClick) // default nil
//*      .MaxCircleDiameter(round(FilterDialogMainBtnRect.Width * (57/87))) // default 65535
//*      .start({aStartImmediatly});
    exit;
  end;
  //--
  {$IF defined(debug)}
  If FLongPressTimer.Enabled then raise Exception.Create('Error 21D2D4F4-F06A-4B82-B73D-AD47E9B4DA86');
  {$ENDIF}
  var LControlMousePos: TPointf;
  var LControl := GetControlAtPos(TPointF.Create(X,Y), LControlMousePos);
  SetHovered(LControl);
  if FHovered <> nil then begin
//*    if abs(TALTouchEventTracking.OnMouseDownVelocity) < TALTouchEventTracking.MinVelocityThreshold then begin
//*      FLongPressTimer.Interval := FHovered.LongPressDelay;
//*      FLongPressTimer.Enabled := True;
//*      FHovered.StartTouchEffect(TALDynamicScrollBoxControl.TStartTouchEffectEvent.MouseDown, LControlMousePos);
//*    end;
//*    FHovered.MouseDown(FHovered, LControlMousePos);
  end;
end;

{********************************************************************************************}
procedure TALDynamicScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseUp(Button, Shift, X, Y);
  //--
  {$IFDEF DEBUG}
  ALLog(
    ClassName + '.MouseUp',
    'x: ' + ALFloatToStrW(X, ALDefaultFormatSettingsW) + ' | ' +
    'y: ' + ALFloatToStrW(y, ALDefaultFormatSettingsW),
    TalLogType.verbose);
  {$ENDIF}
  //--
  if ActiveScrollEngines.Count = 1 then ActiveScrollEngines[0].MouseUp(X, Y)
  else begin
    var LActiveScrollEnginesArray := ActiveScrollEngines.ToArray;
    for var I := low(LActiveScrollEnginesArray) to high(LActiveScrollEnginesArray) do
      LActiveScrollEnginesArray[i].MouseUp(X, Y);
  end;
  //--
//*  if (not IsRefreshing) and
//*     (CanRefreshByScrolling) and
//*     (MainView.ScrollEngine.viewPortPosition.y <= -1 * (RefreshIconHeight + (RefreshIconHeight / 2))) then refresh(false{aShowPopupLoading});
  //--
  FSelectionTimer.enabled := False;
  FLongPressTimer.Enabled := False;
  //--
  {$IF defined(debug)}
  If GetControlAtPos(TPointF.Create(X,Y)) <> FHovered then raise Exception.Create('Error F60A8D6F-2854-4256-8E22-A785DB25EDC3');
  {$ENDIF}
  if FHovered <> nil then begin
//*    FHovered.FadeOutTouchEffect;
//*    var LControlMousePos := FHovered.FrameToLocal(TPointF.Create(X,Y));
//*    FHovered.MouseUp(FHovered, LControlMousePos.ReducePrecision);
  end;
end;

{**********************************}
procedure TALDynamicScrollBox.Click;
begin
//*  inherited Click;
//*  //--
//*  var LMousePos := TALMainForm.Instance.ScreenToClient(Screen.MousePos);
//*  LMousePos := AbsoluteToLocal(LMousePos);
//*  //--
//*  {$IFDEF DEBUG}
//*  ALLog(
//*    ClassName + '.Click',
//*    'x: ' + ALFloatToStrW(LMousePos.X, ALDefaultFormatSettingsW) + ' | ' +
//*    'y: ' + ALFloatToStrW(LMousePos.y, ALDefaultFormatSettingsW),
//*    TalLogType.verbose);
//*  {$ENDIF}
//*  //--
//*  if not TALTouchEventTracking.CanAcceptClickEvent(self, 250) then exit;
//*  //--
//*  //mirrored in MouseDown
//*  if assigned(ErrorMessageBannerAnim) and
//*     (ErrorMessageBannerAnim.Enabled) and
//*     (FErrorMessageBanner <> nil) and
//*     (LMousePos.y <= FErrorMessageBanner.Height) then begin
//*    AlFreeAndNil(ErrorMessageBannerAnim);
//*    repaint;
//*    exit;
//*  end
//*  else if assigned(GPSRequiredMessageBannerAnim) and
//*     (GPSRequiredMessageBannerAnim.Enabled) and
//*     (FGPSRequiredMessageBanner <> nil) and
//*     (LMousePos.y <= FGPSRequiredMessageBanner.Height) then begin
//*    GPSRequiredMessageBannerAnim.Enabled := False;
//*    GPSRequiredMessageBannerAnim.Enabled := True;
//*    ForceStartGeoPositionUpdates;
//*    repaint;
//*    exit;
//*  end;
//*  //--
//*  {$IF defined(debug)}
//*  If GetControlAtPos(LMousePos) <> FHovered then raise Exception.Create('Error 136FCD25-3B39-4ADB-97D1-9FBD8F4430DD');
//*  {$ENDIF}
//*  // MouseUp is fired AFTER Click so at this
//*  // step the FHovered is still assigned
//*  if FHovered <> nil then begin
//*    var LControlMousePos := FHovered.FrameToLocal(LMousePos);
//*    //TALTouchEffectBuilder.create(self)
//*    //  .bounds(FHovered.LocalToFrame(FHovered.LocalRect).ReducePrecision) // const ABounds: TRectf
//*    //  .MousePos(LControlMousePos.ReducePrecision) // default Tpointf.Zero
//*    //  .start({aStartImmediatly});
//*    //exit;
//*    FHovered.StartTouchEffect(TALDynamicScrollBoxControl.TStartTouchEffectEvent.Click, LControlMousePos.ReducePrecision);
//*    if TouchEffectOwner <> FHovered then
//*      FHovered.Click(FHovered, LControlMousePos.ReducePrecision)
//*    else
//*      TouchEffectClicked := True;
//*  end;
end;

{*************************************}
procedure TALDynamicScrollBox.DblClick;
begin
//*  inherited DblClick;
//*  //--
//*  var LMousePos := TALMainForm.Instance.ScreenToClient(Screen.MousePos);
//*  LMousePos := AbsoluteToLocal(LMousePos);
//*  //--
//*  {$IFDEF DEBUG}
//*  ALLog(
//*    ClassName + '.DblClick',
//*    'x: ' + ALFloatToStrW(LMousePos.X, ALDefaultFormatSettingsW) + ' | ' +
//*    'y: ' + ALFloatToStrW(LMousePos.y, ALDefaultFormatSettingsW),
//*    TalLogType.verbose);
//*  {$ENDIF}
//*  //--
//*  {$IF defined(debug)}
//*  If GetControlAtPos(LMousePos) <> FHovered then raise Exception.Create('Error E434E436-1342-4757-AA22-FD4C7E9A41C7');
//*  {$ENDIF}
//*  if FHovered <> nil then begin
//*    var LControlMousePos := FHovered.FrameToLocal(LMousePos);
//*    FHovered.DblClick(FHovered, LControlMousePos.ReducePrecision);
//*  end;
end;

{*****************************************}
procedure TALDynamicScrollBox.DoMouseEnter;
begin
  inherited DoMouseEnter;
  //--
  {$IFDEF DEBUG}
  ALLog(ClassName + '.DoMouseEnter', TalLogType.verbose);
  {$ENDIF}
  //--
  SetHovered(nil);
end;

{*****************************************}
procedure TALDynamicScrollBox.DoMouseLeave;
begin
  inherited DoMouseLeave;
  //--
  {$IFDEF DEBUG}
  ALLog(ClassName + '.DoMouseLeave', TalLogType.verbose);
  {$ENDIF}
  //--
  if ActiveScrollEngines.Count = 1 then ActiveScrollEngines[0].MouseLeave
  else begin
    var LActiveScrollEnginesArray := ActiveScrollEngines.ToArray;
    for var I := low(LActiveScrollEnginesArray) to high(LActiveScrollEnginesArray) do
      LActiveScrollEnginesArray[i].MouseLeave;
  end;
  //--
  FSelectionTimer.enabled := False;
  FLongPressTimer.Enabled := False;
  //--
  SetHovered(nil);
end;

{************************************************************************}
procedure TALDynamicScrollBox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited MouseMove(Shift, X, Y);
  //--
  {$IFDEF DEBUG}
  {-----
  ALLog(
    ClassName + '.MouseMove',
    'x: ' + ALFloatToStrW(X, ALDefaultFormatSettingsW) + ' | ' +
    'y: ' + ALFloatToStrW(y, ALDefaultFormatSettingsW),
    TalLogType.verbose);
  -----}
  {$ENDIF}
  //--
  if ActiveScrollEngines.Count = 1 then ActiveScrollEngines[0].MouseMove(X, Y)
  else begin
    var LActiveScrollEnginesArray := ActiveScrollEngines.ToArray;
    for var I := low(LActiveScrollEnginesArray) to high(LActiveScrollEnginesArray) do
      LActiveScrollEnginesArray[i].MouseMove(X, Y);
  end;
  //--
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  Cursor := CrDefault;
  {$endif}
  //--
  var LControlMousePos: TPointf;
  var LControl := GetControlAtPos(TPointF.Create(X,Y), LControlMousePos);
  SetHovered(LControl);
  //*if (FLongPressTimer.Enabled) and
  //*   (LocalToAbsolute(TPointF.Create(X,Y)).Distance(TALTouchEventTracking.OnMouseDownPointF) >= TALTouchEventTracking.MinDistanceThreshold) then begin
  //*  FLongPressTimer.Enabled := False;
  //*  if FHovered <> nil then
  //*    FHovered.FadeOutTouchEffect;
  //*end;
  if LControl <> nil then
    LControl.MouseMove(LControl, LControlMousePos);
  //--
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  if (FFilterDialogMainBtnVisible) and
     (FilterDialogMainBtnRect.Contains(TPointF.Create(X,Y))) then cursor := crhandpoint
  {$endif}
end;

{****************************************************************************************************************************************}
procedure TALDynamicScrollBox.FilterDialogPopupBtnLabelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
//*  TALTouchEffectBuilder.create(Sender)
//*    .MousePos(TpointF.Create(x,y)) // default Tpointf.Zero
//*    .RectangleFillColor($66ababab) // default $66EEF3FE
//*    .CircleFillColor($66000000) // default $66C4D4FD
//*    .MaxCircleDiameter(round(ALAlignDimensionToPixelRound(40 * AffineRatio, ScreenScale))) // default 65535
//*    .RectangleXRadius(TALText(sender).XRadius) // default 0
//*    .RectangleYRadius(TALText(sender).YRadius) // default 0
//*    .start({aStartImmediatly});
end;

{****************************************************************************************************************************************}
procedure TALDynamicScrollBox.FilterDialogPopupBtnImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
//*  TALTouchEffectBuilder.create(Sender)
//*    .Kind(TALTouchEffectKind.circle) // default TALTouchEffectKind.RectangleAndCircle
//*    .ClipChildren(false) // default True
//*    .MaxCircleDiameter(round(ALAlignDimensionToPixelRound(60 * AffineRatio, ScreenScale) * (57/87))) // default 65535
//*    .start({aStartImmediatly});
end;

{**********************************************************************************************************************************}
procedure TALDynamicScrollBox.FilterDialogMainBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
//*  TALTouchEffectBuilder.create(Sender)
//*    .Kind(TALTouchEffectKind.circle) // default TALTouchEffectKind.RectangleAndCircle
//*    .ClipChildren(false) // default True
//*    .MaxCircleDiameter(round(FilterDialogMainBtnRect.Width * (57/87))) // default 65535
//*    .start({aStartImmediatly});
end;

{******************************************************************************}
procedure TALDynamicScrollBox.FilterDialogPopupBtnLabelResized(Sender: TObject);
begin
  var LLabel := TALText(Sender);
  var LLayout := TALLayout(LLabel.Parent);
  //-----
  LLayout.Width := LLabel.Width + LLayout.Height{width of the circle} + LLabel.Margins.Right;
  LLayout.Position.X := FilterDialogMainBtnRect.Left - LLayout.Width + FilterDialogMainBtnRect.width - ((FilterDialogMainBtnRect.width - LLayout.Height{width of the circle}) / 2) + Position.X;
  //-----
//*  LLabel.Position.point := LLabel.AbsoluteToLocal(
//*                             ALAlignAbsolutePointToPixelRound(
//*                               LLabel.LocalToAbsolute(Tpointf.Create(0, (LLayout.Height - LLabel.height) / 2)),
//*                               ScreenScale)); // << i need this because LLabel.ClipChildren := True; so if not perfectly alligned, it's painted cropped
end;

{**********************************************************************}
procedure TALDynamicScrollBox.FilterDialogMainBtnClick(Sender: TObject);

//*var LOverlay: TALRectangle;
//*    LPosYAnimationStopValue: single;
//*    LPosYAnimations: TArray<TALFloatPropertyAnimation>;
//*    LOpacityAnimations: TArray<TALFloatPropertyAnimation>;
//*    LAnimationsIdx: integer;
//*
//*  {~~~~~~~~~~~~~~~~~~}
//*  function _createBtn(
//*             const aCaption: String;
//*             const aResName: String;
//*             const aTag: integer): TALLayout;
//*  begin
//*    //-----
//*    var LBtnHeight: Single := ALAlignDimensionToPixelRound(60 * AffineRatio, ScreenScale);
//*    if LPosYAnimationStopValue - LBtnHeight <= Position.y then exit(nil);
//*    //-----
//*    result := TALLayout.create(LOverlay);
//*    result.parent := LOverlay;
//*    result.Height := LBtnHeight;
//*    result.Position.Point := FilterDialogMainBtnRect.Location + Position.Point;
//*    result.Position.Y := result.Position.Y + ((FilterDialogMainBtnRect.height - result.Height) / 2);
//*    result.opacity := 0;
//*    //-----
//*    var LImage := TALImage.Create(result);
//*    LImage.Parent := result;
//*    LImage.Align := TALAlignLayout.MostRight;
//*    LImage.Width := LBtnHeight;
//*    LImage.height := LBtnHeight;
//*    LImage.ResourceName := aResName;
//*    LImage.Cursor := Crhandpoint;
//*    LImage.tag := aTag;
//*    LImage.OnMouseDown := FilterDialogPopupBtnImageMouseDown;
//*    LImage.OnClick := TALMainForm.Instance.PopupDialogBtnClick;
//*    //-----
//*    var LLabel := TALText.Create(result);
//*    LLabel.Parent := result;
//*    LLabel.OnResized := FilterDialogPopupBtnLabelResized;
//*    LLabel.ClipChildren := True;
//*    LLabel.Align := TALAlignLayout.none;
//*    LLabel.margins.right := ALAlignDimensionToPixelRound(4 * AffineRatio, ScreenScale);
//*    LLabel.Padding.Left := ALAlignDimensionToPixelRound(10 * AffineRatio, ScreenScale);
//*    LLabel.Padding.right := ALAlignDimensionToPixelRound(10 * AffineRatio, ScreenScale);
//*    LLabel.Padding.top := ALAlignDimensionToPixelRound(8 * AffineRatio, ScreenScale);
//*    LLabel.Padding.bottom := ALAlignDimensionToPixelRound(8 * AffineRatio, ScreenScale);
//*    LLabel.Fill.Kind := TbrushKind.Solid;
//*    LLabel.Fill.Color := $C8000000;
//*    LLabel.XRadius := ALAlignDimensionToPixelRound(4 * AffineRatio, ScreenScale);
//*    LLabel.YRadius := ALAlignDimensionToPixelRound(4 * AffineRatio, ScreenScale);
//*    LLabel.Cursor := Crhandpoint;
//*    LLabel.OnMouseDown := FilterDialogPopupBtnLabelMouseDown;
//*    LLabel.OnClick := TALMainForm.Instance.PopupDialogBtnClick;
//*    LLabel.tag := aTag;
//*    LLabel.AutoSize := True;
//*    LLabel.TextSettings.FontColor := $ffffffff;
//*    LLabel.TextSettings.Font.Style := [TfontStyle.fsBold];
//*    LLabel.TextSettings.Font.Family := ALGetFontFamily('sans-serif');
//*    LLabel.TextSettings.Font.size := ALAlignDimensionToPixelRound(12 * AffineRatio, ScreenScale);
//*    LLabel.TextSettings.WordWrap := False;
//*    LLabel.Text := aCaption;
//*    LLabel.HitTest := True;
//*    //-----
//*    inc(LAnimationsIdx);
//*    //-----
//*    var LfloatAnimation := TALFloatPropertyAnimation.Create(result);
//*    LfloatAnimation.Parent := result;
//*    LfloatAnimation.PropertyName := 'Position.Y';
//*    LfloatAnimation.Delay := 0.05;
//*    LfloatAnimation.Duration := 0.3;
//*    LfloatAnimation.AnimationType := TanimationType.Out;
//*    LfloatAnimation.Interpolation := TALInterpolationType.back;
//*    LfloatAnimation.StartFromCurrent := True;
//*    LfloatAnimation.StopValue := result.AbsoluteToLocal(
//*                                   ALAlignAbsolutePointToPixelRound(
//*                                     result.LocalToAbsolute(
//*                                       TpointF.Create(0, LPosYAnimationStopValue - result.Height)),
//*                                         ScreenScale)).Y; // << i need this because LLabel.ClipChildren := True; so if not perfectly alligned, it's painted cropped
//*    LPosYAnimationStopValue := LfloatAnimation.StopValue;
//*    LfloatAnimation.Enabled := false;
//*    LPosYAnimations[LAnimationsIdx] := LfloatAnimation;
//*    //-----
//*    LfloatAnimation := TALFloatPropertyAnimation.Create(result);
//*    LfloatAnimation.Parent := result;
//*    LfloatAnimation.PropertyName := 'Opacity';
//*    LfloatAnimation.Delay := 0.05;
//*    LfloatAnimation.Duration := 0.2;
//*    LfloatAnimation.AnimationType := TanimationType.in;
//*    LfloatAnimation.Interpolation := TALInterpolationType.Linear;
//*    LfloatAnimation.StartFromCurrent := True;
//*    LfloatAnimation.StopValue := 1;
//*    LfloatAnimation.Enabled := false;
//*    LOpacityAnimations[LAnimationsIdx] := LfloatAnimation;
//*  end;
//*
//*var LColorAnimation: TALColorPropertyAnimation;
//*    LImage: TALImage;

begin

//*  // create the LOverlay
//*  LOverlay := TALRectangle.Create(TALMainForm.Instance);
//*  LOverlay.Parent := TALMainForm.Instance;
//*  LOverlay.beginupdate;
//*  LOverlay.Fill.Color := TAlphaColorRec.null;
//*  LOverlay.Stroke.Kind := TbrushKind.none;
//*  LOverlay.Position.Point := TpointF.Create(0,0);
//*  LOverlay.Size.Size := TpointF.Create(TALMainForm.Instance.clientWidth, TALMainForm.Instance.ClientHeight);
//*  LOverlay.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
//*  LOverlay.OnClick := TALMainForm.Instance.PopupDialogCloseClick;
//*
//*  //create the LOverlay opacity animation
//*  LColorAnimation := TALColorPropertyAnimation.Create(LOverlay);
//*  LColorAnimation.Parent := LOverlay;
//*  LColorAnimation.PropertyName := 'Fill.Color';
//*  LColorAnimation.Delay := 0.05;
//*  LColorAnimation.Duration := 0.2;
//*  LColorAnimation.AnimationType := TanimationType.in;
//*  LColorAnimation.Interpolation := TALInterpolationType.linear;
//*  LColorAnimation.StartFromCurrent := True;
//*  LColorAnimation.StopValue := $AFffffff;
//*  LColorAnimation.Enabled := false;
//*
//*  //create the main button
//*  LImage := TALImage.Create(LOverlay);
//*  LImage.Parent := LOverlay;
//*  LImage.Width := FilterDialogMainBtnRect.Width;
//*  LImage.Height := FilterDialogMainBtnRect.Height;
//*  LImage.Position.Point := FilterDialogMainBtnRect.Location + Position.Point;
//*  LImage.ResourceName := GetFilterDialogMainBtnResName;
//*  LImage.Cursor := Crhandpoint;
//*  LImage.OnMouseDown := FilterDialogMainBtnMouseDown;
//*  LImage.OnClick := TALMainForm.Instance.PopupDialogCloseClick;
//*
//*  //create the cancel button
//*  if CanShowFilterDialogResetBtn then begin
//*    LImage := TALImage.Create(LOverlay);
//*    LImage.Parent := LOverlay;
//*    LImage.Width := FilterDialogMainBtnRect.Width;
//*    LImage.Height := FilterDialogMainBtnRect.Height;
//*    LImage.Position.Point := FilterDialogMainBtnRect.Location + Position.Point;
//*    LImage.Position.X := LImage.Position.x - FilterDialogMainBtnRect.Width;
//*    LImage.ResourceName := 'filter_cancel_348x348';
//*    LImage.Cursor := Crhandpoint;
//*    LImage.tag := 100;
//*    LImage.OnMouseDown := FilterDialogMainBtnMouseDown;
//*    LImage.OnClick := TALMainForm.Instance.PopupDialogBtnClick;
//*  end;
//*
//*  //init aPosYAnimation
//*  setlength(LPosYAnimations, FFilterDialogPopupBtns.Count);
//*  for LAnimationsIdx := low(LPosYAnimations) to High(LPosYAnimations) do
//*    LPosYAnimations[LAnimationsIdx] := nil;
//*  LPosYAnimationStopValue := Position.y + FilterDialogMainBtnRect.Top ;//+ 15{the padding around the circle for the shadow};
//*
//*  //init aOpacityAnimation
//*  setlength(LOpacityAnimations, FFilterDialogPopupBtns.Count);
//*  for LAnimationsIdx := low(LOpacityAnimations) to High(LOpacityAnimations) do
//*    LOpacityAnimations[LAnimationsIdx] := nil;
//*
//*  //init LAnimationsIdx
//*  LAnimationsIdx := -1;
//*
//*  //create all the buttons
//*  for var LFilterDialogPopupBtn in FFilterDialogPopupBtns do
//*    _createBtn(
//*      LFilterDialogPopupBtn.Caption,
//*      LFilterDialogPopupBtn.ResName,
//*      LFilterDialogPopupBtn.ModalResult);
//*
//*  //hide the fFilterDialogMainBtnVisible
//*  fFilterDialogMainBtnVisible := False;
//*
//*  //endupdate
//*  LOverlay.endupdate;
//*
//*  //show the dialog
//*  TALMainForm.Instance.ShowPopupDialog(
//*    LOverlay,
//*    FilterDialogClose);
//*
//*  //start all the animation
//*  LColorAnimation.Enabled := True;
//*  for LAnimationsIdx := low(LPosYAnimations) to High(LPosYAnimations) do begin
//*    if LPosYAnimations[LAnimationsIdx] <> nil then LPosYAnimations[LAnimationsIdx].enabled := True;
//*    if LOpacityAnimations[LAnimationsIdx] <> nil then LOpacityAnimations[LAnimationsIdx].enabled := True;
//*  end;

end;

{***************************************************************************************************}
procedure TALDynamicScrollBox.FilterDialogClose(Const aPopup: Tcontrol; const AResult: TModalResult);
begin
  FilterDialogMainBtnVisible := true;
  if AResult <> mrClose then ALFreeAndNilDrawable(fFilterDialogMainBtn);
end;

{*****************************************************************}
function TALDynamicScrollBox.GetFilterDialogMainBtnResName: String;
begin
  result := '';
end;

{****************************************************************}
function TALDynamicScrollBox.CanShowFilterDialogResetBtn: Boolean;
begin
  result := false;
end;

{*******************************************}
function TALDynamicScrollBox.GetControlAtPos(
           const aPos: TPointF; // APos is local to the frame
           out AControlPos: TPointF; // AControlPos is local to the founded control
           const ADirectChildOnly: Boolean = False;
           const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl;
begin
  if FHovered <> nil then result := FHovered.GetControlAtPos(FHovered.FrameToLocal(aPos).ReducePrecision, AControlPos)
  else result := nil;
  if result = nil then result := MainView.GetControlAtPos(MainView.FrameToLocal(aPos).ReducePrecision, AControlPos);
end;

{*******************************************}
function TALDynamicScrollBox.GetControlAtPos(
           const aPos: TPointF;
           const ADirectChildOnly: Boolean = False;
           const ACheckHitTest: Boolean = true): TALDynamicScrollBoxControl;
begin
  Var LControlPos: TPointF;
  result := GetControlAtPos(aPos, LControlPos, ADirectChildOnly, ACheckHitTest);
end;

{*********************************************************************************}
procedure TALDynamicScrollBox.SetHovered(const AValue: TALDynamicScrollBoxControl);
begin
  if AValue <> FHovered then begin
    FLongPressTimer.Enabled := False;
    if FHovered <> nil then begin
//*      FHovered.FadeOutTouchEffect;
//*      FHovered.MouseLeave(FHovered);
    end;
    FHovered := AValue;
    if FHovered <> nil then
      FHovered.mouseEnter(FHovered);
  end;
end;

{*******************************************************************}
procedure TALDynamicScrollBox.LongPressTimerOnTimer(Sender: TObject);
begin
  FLongPressTimer.Enabled := False;
  If FHovered = nil then exit;
//*  var LMousePos := TALMainForm.Instance.ScreenToClient(Screen.MousePos);
//*  LMousePos := AbsoluteToLocal(LMousePos);
//*  FHovered.LongPress(FHovered, FHovered.FrameToLocal(LMousePos).ReducePrecision);
end;

{*************************************************************************************}
procedure TALDynamicScrollBox.SetTouchEffectRectangleBitmap(const AValue: TALDrawable);
begin
//*  if FOwnTouchEffectRectangleBitmap then ALFreeAndNil(FTouchEffectRectangleBitmap);
  FTouchEffectRectangleBitmap := AValue;
  FOwnTouchEffectRectangleBitmap := True;
end;

{**********************************************************************************}
procedure TALDynamicScrollBox.SetTouchEffectCircleBitmap(const AValue: TALDrawable);
begin
  if FOwnTouchEffectCircleBitmap then ALFreeAndNilDrawable(FTouchEffectCircleBitmap);
  FTouchEffectCircleBitmap := AValue;
  FOwnTouchEffectCircleBitmap := True;
end;

{*******************************************************************}
procedure TALDynamicScrollBox.TouchEffectAnimFinish(Sender: TObject);
begin
  FTouchEffectAnim.Enabled := False;
  if TouchEffectOwner = nil then exit;
  //--
//*  if (TouchEffectOwner.TouchEffectStyle = TALDynamicScrollBoxControl.TTouchEffectStyle.ClickFadeOutRectangleAndCircle) and
//*     (FTouchEffectAnim.Tag = 0) then begin
//*    var LPrevTagFloat := TouchEffectAnim.TagFloat;
//*    TouchEffectAnim.Tag := 1;
//*    TouchEffectAnim.TagFloat := TouchEffectAnim.StopValue;
//*    TouchEffectAnim.Delay := 0;
//*    TouchEffectAnim.Duration := 0.2;
//*    TouchEffectAnim.StartValue := LPrevTagFloat;
//*    TouchEffectAnim.StopValue := 0;
//*    TouchEffectAnim.start;
//*    exit;
//*  end;
  //--
  var LTouchEffectOwner := TouchEffectOwner;
  TouchEffectOwner := nil;
  //--
  If TouchEffectClicked then
    LTouchEffectOwner.Click(LTouchEffectOwner, FTouchEffectMousePos);
end;

{******************************************************************************************************}
procedure TALDynamicScrollBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  if (not (Handled)) then begin
    if not (ssHorizontal in Shift) then begin
      // https://stackoverflow.com/questions/7753123/why-is-wheeldelta-120
      var Offset: Single := (FscrollBar.ViewportSize / 5) * -1 * (WheelDelta / 120);
      TALScrollEngine(FscrollBar.TagObject).MouseWheel(0, Offset);
      Handled := True;
    end;
  end;
  {$ENDIF}
end;

{**********************************************************}
procedure TALDynamicScrollBox.FrameResized(Sender: TObject);
begin
//*  if not initialized then exit; // initialize will call directly Resize/DoResized
  if ((not SameValue(FFrameWidth, Width, TEpsilon.Position))) then begin
//*      (not SameValue(FFrameHeight, Height + TALMainForm.Instance.KeyboardBounds.Height, TEpsilon.Position)) or
//*      (not SameValue(FFrameAffineRatio, AffineRatio, TEpsilon.Scale))) then begin
    FFrameWidth := Width;
    FFrameHeight := Height; //* + TALMainForm.Instance.KeyboardBounds.Height;
    FFrameAffineRatio := 1; //* AffineRatio;
    FFrameKeyboardHeight := 0; //* TALMainForm.Instance.KeyboardBounds.Height;
    DoFrameResized(false{AToCounterbalanceKeyboardSpace});
  end
//*  else if (not SameValue(FFrameKeyboardHeight, TALMainForm.Instance.KeyboardBounds.Height, TEpsilon.Position)) then begin
//*    FFrameKeyboardHeight := TALMainForm.Instance.KeyboardBounds.Height;
//*    DoFrameResized(True{AToCounterbalanceKeyboardSpace});
//*  end;
end;

{******************************************************************************************}
procedure TALDynamicScrollBox.DoFrameResized(const AToCounterbalanceKeyboardSpace: boolean);
begin
  Var LMainViewLocalRect := CalculateMainViewLocalRect;
  {$IFDEF DEBUG}
  ALLog(
    ClassName + '.DoFrameResized',
    'ToCounterbalanceKeyboardSpace: ' + ALBoolToStrW(AToCounterbalanceKeyboardSpace) + ' | ' +
    'MainView.BoundsRect.left:' + ALFormatFloatW('0.##', LMainViewLocalRect.Left, ALDefaultFormatSettingsW) + ' | ' +
    'MainView.BoundsRect.Top:' + ALFormatFloatW('0.##', LMainViewLocalRect.Top, ALDefaultFormatSettingsW) + ' | ' +
    'MainView.BoundsRect.Right:' + ALFormatFloatW('0.##', LMainViewLocalRect.Right, ALDefaultFormatSettingsW) + ' | ' +
    'MainView.BoundsRect.Bottom:' + ALFormatFloatW('0.##', LMainViewLocalRect.Bottom, ALDefaultFormatSettingsW));
  {$ENDIF}
//*  beginWrite;
  try
    FMainViewWidth := LMainViewLocalRect.Width;
    FMainViewHeight := LMainViewLocalRect.Height;
  finally
//*    endWrite;
  end;
  //----
  if not AToCounterbalanceKeyboardSpace then begin
    InitMeasures;
    InitPreloadOffsets;
    InitBitmaps;
  end;
  //----
  if AToCounterbalanceKeyboardSpace then
    FMainView.fIsAdjustingSize := True;
  try
    FMainView.SetBoundsRect(LMainViewLocalRect);
  finally
    FMainView.fIsAdjustingSize := False;
  end;
  FMainView.AlignControls;
end;

{*************************************************************}
procedure TALDynamicScrollBox.ScrollBarChange(Sender: TObject);
begin
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
  var LScrollEngine := TALScrollEngine(FscrollBar.TagObject);
  LScrollEngine.ViewportPosition := TALPointD.Create(0, fScrollBar.Value);
  {$ENDIF}
end;

{************************************************************************}
//*function TALDynamicScrollBox.CreateProgressFrame: TALBaseProgressFrame;
//*begin
//*  result := nil;
//*end;

{****************************************************************************}
//*function TALDynamicScrollBox.CreateProgressFrameStopAnimationEvent: TEvent;
//*begin
//*  result := nil;
//*end;

{***********************************************************}
//*procedure TALDynamicScrollBox.StartProgressFrameAnimation;
//*begin
//*  if ProgressFrame <> nil then
//*    ProgressFrame.StartAnimation;
//*end;

{*************************************************************************************}
//*procedure TALDynamicScrollBox.StopProgressFrameAnimation(const AImmediate: boolean);
//*begin
//*  if AImmediate or isdestroying then begin
//*    if ProgressFrame <> nil then
//*      ProgressFrame.StopAnimation(nil);
//*    if ProgressFrameStopAnimationEvent <> nil then
//*      ProgressFrameStopAnimationEvent.SetEvent;
//*  end
//*  else begin
//*    if ProgressFrame <> nil then begin
//*      if ProgressFrameStopAnimationEvent <> nil then ProgressFrameStopAnimationEvent.ResetEvent;
//*      ProgressFrame.StopAnimation(ProgressFrameStopAnimationEvent);
//*    end
//*    else if ProgressFrameStopAnimationEvent <> nil then ProgressFrameStopAnimationEvent.SetEvent;
//*  end;
//*end;

{**********************************************************}
//*procedure TALDynamicScrollBox.StopProgressFrameAnimation;
//*begin
//*  StopProgressFrameAnimation(False{AImmediate});
//*end;

{**************************************************************}
//*function TALDynamicScrollBox.CanRemoveProgressFrame: Boolean;
//*begin
//*  //by default (can be overriden) we remove the progress frame when
//*  //the current view have finished to growdata and already downloaded
//*  //the first part of the Items. descendant can override this to wait
//*  //for exemple that we also downloaded some materials (main picture
//*  //for exemple)
//*  result := (ProgressFrame <> nil) and
//*            (MainView.GrowDataExecutedAtLeastOnce) and
//*            (MainView.GrowItemsExecutedAtLeastOnce);
//*end;

{***************************************************}
//*procedure TALDynamicScrollBox.RemoveProgressFrame;
//*begin
//*  if not CanRemoveProgressFrame then exit;
//*  //-----
//*  ALFreeAndNil(ProgressFrame, true{aDelayed});
//*  ALFreeAndNil(ProgressFrameStopAnimationEvent, true{aDelayed});
//*end;

{*************}
//[MultiThread]
function TALDynamicScrollBox.GetPriority(const AExtData: Tobject): Int64;
begin
//*  result := PriorityStartingPoint;
end;

{******************************************}
procedure TALDynamicScrollBox.ScrollToBegin;
begin
  if (MainView.ItemsReversedOrientation) then begin
    if CompareValue(MainView.ScrollPos, MainView.ScrollMaxBound, Tepsilon.Position) >= 0 then exit;
  end
  else begin
    if CompareValue(MainView.ScrollPos, MainView.ScrollMinBound, Tepsilon.Position) <= 0 then exit;
  end;
  if HasActiveScrollEngines then exit;
//*  if (PopupLoading <> nil) then exit;
//*  TALMainForm.Instance.ShowPopupLoading(
//*    False, //const StartAnimation: boolean = False;
//*    TAlphaColorRec.null, // const OverlayColor: TalphaColor = TAlphaColorRec.null; // $64000000 for black like popupdialog
//*    true); //const JustOverlay: Boolean = False);
  fScrollToBeginAnim.StartValue := Opacity;
  fScrollToBeginAnim.StopValue := 0;
  fScrollToBeginAnim.Enabled := true;
end;

{**********************************************************************}
procedure TALDynamicScrollBox.ScrollToBeginAnimProcess(Sender: TObject);
begin
  Opacity := TALFloatAnimation(Sender).currentValue;
  repaint;
end;

{*********************************************************************}
procedure TALDynamicScrollBox.ScrollToBeginAnimFinish(Sender: TObject);
begin
//*  TALMainForm.Instance.closePopupLoading;
  fScrollToBeginAnim.Enabled := False;
  Opacity := 1;
 if MainView.ItemsOrientation = TOrientation.Vertical then begin
    if (MainView.ItemsReversedOrientation) then MainView.ScrollEngine.ViewportPosition := TALPointD.Create(0, MainView.ScrollEngine.MaxScrollLimit.Y)
    else MainView.ScrollEngine.ViewportPosition := TALPointD.Create(0, MainView.ScrollEngine.MinScrollLimit.Y);
  end
  else begin
    if (MainView.ItemsReversedOrientation) then MainView.ScrollEngine.ViewportPosition := TALPointD.Create(MainView.ScrollEngine.MaxScrollLimit.X, 0)
    else MainView.ScrollEngine.ViewportPosition := TALPointD.Create(MainView.ScrollEngine.MinScrollLimit.X, 0);
  end;
  MainView.ScrollEngine.Stop;
  repaint;
end;

{*********************************************}
//*procedure TALDynamicScrollBox.InitActionBar;
//*begin
//*  if not IsMainFrame then exit;
//*  TALMainForm.Instance.ActionBar.Visible := false;

(*
  TALMainForm.Instance.ActionBar.beginUpdate;
  try
    inherited InitActionBar;
    if SelectedItems.Count > 0 then begin
      TALMainForm.Instance.btnCancel.visible := True;
      TALMainForm.Instance.btnDelete.visible := True;
      TALMainForm.Instance.ActionBarLabel.Visible := True;
      if TALMainForm.Instance.language = RUS then begin
        TALMainForm.Instance.ActionBarLabel.Text := Win_RUSFormatPluralU(
                                                         ALTranslate('_RUS_xSelectedNumberEndby_1'),
                                                         ALTranslate('_RUS_xSelectedNumberEndby_2_3_4'),
                                                         ALTranslate('_RUS_xSelectedNumberEndby_0_5_6_7_8_9'),
                                                         SelectedItems.Count);
      end
      else begin
        TALMainForm.Instance.ActionBarLabel.Text := Win_ENUFormatPluralU(
                                                         ALTranslate('_xSelected'),
                                                         ALTranslate('_xSelecteds'),
                                                         SelectedItems.Count);
      end;
    end;
  finally
    TALMainForm.Instance.ActionBar.EndUpdate;
  end;
*)
//*end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALDynamicScrollBox]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALDynamicScrollBox, 'Size');
  UnlistPublishedProperty(TALDynamicScrollBox, 'StyleName');
  UnlistPublishedProperty(TALDynamicScrollBox, 'OnTap');
  {$ENDIF}
end;

initialization
  TALDynamicScrollBox.CanWaitGeoPosition := True;
  RegisterFmxClasses([TALDynamicScrollBox]);

end.
