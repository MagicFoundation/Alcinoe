unit Alcinoe.FMX.PageController;

interface

{$I Alcinoe.inc}

uses
  System.Classes,
  System.UITypes,
  System.Types,
  System.Messaging,
  FMX.Types,
  FMX.Controls,
  ALcinoe.Common,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Layouts,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.ScrollEngine;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  TALPageController = class;

  {********************************************}
  TALBasePageIndicator = class(TALBaseRectangle)
  public
    procedure AnimationProcess(Const AValue: Single); virtual; abstract;
    procedure ActivePageChanged(Const ANewActivePageIndex: Integer); virtual; abstract;
    procedure PageCountChanged(Const ANewPageCount: Integer; const ANewActivePageIndex: Integer); virtual; abstract;
  end;

  {*************************}
  [ComponentPlatforms($FFFF)]
  TALPageIndicator = class(TALBasePageIndicator)
  public
    type
      //---------------
      // TAnimationType
      TAnimationType = (None, Worm, ThinWorm, JumpingDotTopView, JumpingDotSideView, Slide, Scale, LinearSwap, SpinSwap, Color);
      //------
      // TFill
      TFill = class(TALBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      //--------
      // TStroke
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      //-----------
      // TIndicator
      TIndicator = Class(TALPersistentObserver)
      public
        type
          TMargins = class(TALBounds)
          protected
            function GetDefaultValue: TRectF; override;
          end;
          TFill = class(TALBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
          TStroke = class(TALStrokeBrush)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
      private
        FWidth: Single; // 4 bytes
        FHeight: Single; // 4 bytes
        FMargins: TALBounds; // 8 bytes
        FFill: TALBrush; // 8 bytes
        FStroke: TALStrokeBrush; // 8 bytes
        FShadow: TALShadow; // 8 bytes
        FXRadius: Single; // 4 bytes
        FYRadius: Single; // 4 bytes
        FCorners: TCorners; // 1 bytes
        FSides: TSides; // 1 bytes
        procedure SetWidth(const Value: Single);
        procedure SetHeight(const Value: Single);
        procedure SetMargins(const Value: TALBounds);
        procedure SetFill(const Value: TALBrush);
        procedure SetStroke(const Value: TALStrokeBrush);
        procedure SetShadow(const Value: TALShadow);
        procedure SetXRadius(const Value: Single);
        procedure SetYRadius(const Value: Single);
        procedure SetCorners(const Value: TCorners);
        procedure SetSides(const Value: TSides);
        procedure MarginsChanged(ASender: TObject);
        procedure FillChanged(ASender: TObject);
        procedure StrokeChanged(ASender: TObject);
        procedure ShadowChanged(ASender: TObject);
        function IsWidthStored: Boolean;
        function IsHeightStored: Boolean;
        function IsXRadiusStored: Boolean;
        function IsYRadiusStored: Boolean;
        function IsCornersStored: Boolean;
        function IsSidesStored: Boolean;
      protected
        function CreateMargins: TALBounds; virtual;
        function CreateFill: TALBrush; virtual;
        function CreateStroke: TALStrokeBrush; virtual;
        function CreateShadow: TALShadow; virtual;
        function GetDefaultWidth: Single;
        function GetDefaultHeight: Single;
        function GetDefaultXRadius: Single;
        function GetDefaultYRadius: Single;
        function GetDefaultCorners: TCorners;
        function GetDefaultSides: TSides;
        property Margins: TALBounds read FMargins write setMargins;
        property Width: Single read FWidth write setWidth stored IsWidthStored nodefault;
        property Height: Single read FHeight write setHeight stored IsHeightStored nodefault;
      public
        constructor Create; override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure AlignToPixel; virtual;
        property DefaultWidth: Single read GetDefaultWidth;
        property DefaultHeight: Single read GetDefaultHeight;
        property DefaultXRadius: Single read GetDefaultXRadius;
        property DefaultYRadius: Single read GetDefaultYRadius;
        property DefaultCorners: TCorners read GetDefaultCorners;
        property DefaultSides: TSides read GetDefaultSides;
      published
        property Fill: TALBrush read FFill write setFill;
        property Stroke: TALStrokeBrush read FStroke write setStroke;
        property Shadow: TALShadow read FShadow write setShadow;
        property XRadius: Single read FXRadius write setXRadius stored IsXRadiusStored nodefault;
        property YRadius: Single read FYRadius write setYRadius stored IsYRadiusStored nodefault;
        property Corners: TCorners read FCorners write setCorners stored IsCornersStored;
        property Sides: TSides read FSides write setSides stored IsSidesStored;
      end;
      //-----------------
      // TActiveIndicator
      TActiveIndicator = Class(TIndicator)
      public
        type
          TFill = class(TIndicator.TFill)
          protected
            function GetDefaultColor: TAlphaColor; override;
          end;
      protected
        function CreateFill: TALBrush; override;
      end;
      //-------------------
      // TInactiveIndicator
      TInactiveIndicator = Class(TIndicator)
      Published
        property Margins;
        property Width;
        property Height;
      end;
      //------------------
      // TIndicatorControl
      TIndicatorControl = Class(TALRectangle);
      //------------------------
      // TActiveIndicatorControl
      TActiveIndicatorControl = Class(TIndicatorControl)
      protected
        function GetCacheSubIndex: Integer; override;
      end;
      //--------------------------
      // TInactiveIndicatorControl
      TInactiveIndicatorControl = Class(TIndicatorControl)
      protected
        function GetCacheSubIndex: Integer; override;
      end;
  private
    FAnimationType: TAnimationType; // 1 byte
    FPageCount: Integer; // 4 bytes
    FActivePageIndex: Integer; // 4 bytes
    FAnimationValue: Single; // 4 bytes
    FActiveIndicatorControl: TActiveIndicatorControl; // 8 bytes
    FActiveIndicator: TActiveIndicator;  // 8 bytes
    FInactiveIndicator: TInactiveIndicator; // 8 bytes
    procedure SetAnimationType(const AValue: TAnimationType);
    procedure SetActiveIndicator(const AValue: TActiveIndicator);
    procedure SetInactiveIndicator(const AValue: TInactiveIndicator);
    procedure ActiveIndicatorChanged(ASender: TObject);
    procedure InactiveIndicatorChanged(ASender: TObject);
    procedure InternalDoRealign;
  protected
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    procedure SetDoubleBuffered(const AValue: Boolean); override;
    procedure Loaded; override;
    procedure DoRealign; override;
    procedure AdjustSize; override;
    Procedure UpdateActiveIndicator; Virtual;
    procedure RebuildIndicatorControls; virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AlignToPixel; override;
    procedure AnimationProcess(Const AValue: Single); override;
    procedure ActivePageChanged(const ANewActivePageIndex: Integer); override;
    procedure PageCountChanged(Const ANewPageCount: Integer; const ANewActivePageIndex: Integer); override;
    property ActiveIndicatorControl: TActiveIndicatorControl read FActiveIndicatorControl;
    property ActivePageIndex: Integer read FActivePageIndex;
    property AnimationValue: Single read FAnimationValue;
    property PageCount: integer read FPageCount;
    property CacheEngine;
    property CacheIndex;
  published
    //property Action;
    property ActiveIndicator: TActiveIndicator read FActiveIndicator write SetActiveIndicator;
    property InactiveIndicator: TInactiveIndicator read FInactiveIndicator write SetInactiveIndicator;
    property Align;
    property Anchors;
    property AnimationType: TAnimationType read FAnimationType write SetAnimationType default TanimationType.None;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Corners;
    property Cursor;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property Shadow;
    property Sides;
    property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius;
    property YRadius;
    //property OnCanFocus;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALPageView = class(TALBaseRectangle)
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
  private
    FPageController: TALPageController;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
  protected
    function CreateFill: TALBrush; override;
    function CreateStroke: TALStrokeBrush; override;
    procedure ParentChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    property PageController: TALPageController read FPageController;
    property CacheEngine;
    property CacheIndex;
  published
    //property Action;
    property Active: Boolean read GetActive write SetActive stored False;
    //property Align;
    //property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered;
    //property DragMode;
    //property EnableDragHighlight;
    property Enabled;
    property Fill;
    //property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest default false;
    property &Index stored False;
    property Locked default true;
    //property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    //property Position;
    //property RotationAngle;
    //property RotationCenter;
    //property Pivot;
    //property Scale;
    //property Size;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    //property Visible;
    //property Width;
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
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
  end;
  TALPageViewClass = class of TALPageView;

  {***********************************************}
  // IItemsContainer is required because it is used
  // in FMX.Editor.Items / FMX.Design.Items
  [ComponentPlatforms($FFFF)]
  TALPageController = class(TALBaseRectangle, IALScrollableControl, IItemsContainer)
  public
    const
      MAXIMUM_FLING_VELOCITY = 8000; // From android ViewConfiguration.MAXIMUM_FLING_VELOCITY
      MAX_SETTLE_DURATION = 600; // From android ViewPager.MAX_SETTLE_DURATION
  public
    type
      // -------
      // TStroke
      TStroke = class(TALStrokeBrush)
      protected
        function GetDefaultColor: TAlphaColor; override;
      end;
      // ---------------
      // TPageTransition
      TPageTransition = (None, Slide, DirectFadeIn, DirectFadeOut, OverlayFadeIn, RevealFadeOut, CrossFade);
      // ------------------------
      // TViewportPositionChangeEvent
      TViewportPositionChangeEvent = procedure (Sender: TObject; const OldViewportPosition, NewViewportPosition: TALPointD) of object;
      // --------
      // TContent
      // Inherits from TALContent instead of TALControl
      // because, at design time in the IDE, child components added to
      // TContent (i.e., the TALPageView) are properly assigned as children
      // of the PageView controller.
      TContent = class(TALContent)
      private
        FPageController: TALPageController;
      protected
        procedure DoAddObject(const AObject: TFmxObject); override;
        procedure DoRemoveObject(const AObject: TFmxObject); override;
        procedure DoDeleteChildren; override;
        procedure DoContentChanged; override;
        {$IFNDEF ALDPK}
        function IsVisibleObject(const AObject: TControl): Boolean; override;
        {$ENDIF}
      public
        constructor Create(AOwner: TComponent); override;
        property PageControl: TALPageController read FPageController;
      end;
      // -------------
      // TScrollEngine
      TScrollEngine = class(TALScrollEngine)
      private
        FPageController: TALPageController; // 8 bytes;
        fLastViewportPosition: TALPointD; // 16 bytes;
        FDownPageIndex: integer; // 4 bytes
      protected
        procedure DoMouseDown; override;
        procedure DoMouseUp; override;
        procedure DoStart; override;
        procedure DoStop; override;
        procedure DoChanged; override;
      public
        constructor Create(const APageControl: TALPageController); reintroduce;
        property PageControl: TALPageController read FPageController;
      end;
  private
    FViewportFraction: Single; // 4 bytes
    FActivePageIndex: Integer; // 4 bytes
    FScrollEngine: TALScrollEngine; // 8 bytes
    FContent: TContent; // 8 bytes
    FPageIndicator: TALBasePageIndicator; // 8 bytes
    fMouseDownPos: TPointF; // 8 bytes
    FFadeAnimation: TALFloatAnimation; // 8 bytes
    FFadeFromPageIndex: Integer; // 4 bytes
    FFadeToPageIndex: Integer; // 4 bytes
    FFadeOverlay: TALLayout; // 8 bytes
    FFadeTouchMode: TALScrollEngine.TTouchMode; // 1 byte
    FPageSnapping: Boolean;  // 1 byte
    FPadEnds: Boolean; // 1 byte
    FHandleMouseEvents: Boolean; // 1 byte
    fScrollCapturedByMe: boolean; // 1 bytes
    FOrientation: TOrientation; // 1 byte
    FOnViewportPositionChange: TViewportPositionChangeEvent; // 16 bytes
    FOnActivePageChanged: TNotifyEvent; // 16 bytes
    fOnAniStart: TnotifyEvent; // 16 bytes
    fOnAniStop: TnotifyEvent; // 16 bytes
    procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
    procedure RefreshActivePageIndex;
    procedure SetPageIndicator(const AValue: TALBasePageIndicator);
    function GetPageSize: Single;
    function GetEndPadding: Single;
    function GetPageCount: integer;
    function GetPage(AIndex: Integer): TALPageView;
    function GetActivePage: TALPageView;
    procedure SetOrientation(const AValue: TOrientation);
    procedure SetViewportFraction(const AValue: Single);
    function IsViewportFractionStored: Boolean;
    procedure FadeAnimationProcess(Sender: TObject);
    procedure FadeAnimationFinish(Sender: TObject);
    procedure internalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure internalMouseMove(Shift: TShiftState; X, Y: Single);
    procedure internalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure internalMouseLeave;
    { IItemContainer }
    function GetItemsCount: Integer;
    function GetItem(const AIndex: Integer): TFmxObject;
    { IALScrollableControl }
    function GetScrollEngine: TALScrollEngine;
    procedure SetScrollEngine(const Value: TALScrollEngine);
  protected
    function CreateStroke: TALStrokeBrush; override;
    function CreateContent: TContent; virtual;
    function CreateScrollEngine: TScrollEngine; virtual;
    procedure FreeNotification(AObject: TObject); override;
    procedure Loaded; override;
    function GetDefaultSize: TSizeF; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure Paint; override;
    procedure DoActivePageChanged; virtual;
    procedure DoRealign; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    {$IFNDEF ALDPK}
    procedure ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ChildrenMouseLeave(const AObject: TControl); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure SetActivePage(const AValue: TALPageView; const ATransition: TPageTransition; const AVelocity: Single = 0); overload; virtual;
    procedure SetActivePage(const AValue: TALPageView); overload; virtual;
    procedure SetActivePageIndex(const AValue: integer; const ATransition: TPageTransition; const AVelocity: Single = 0); overload; virtual;
    procedure SetActivePageIndex(const AValue: Integer); overload; virtual;
    function NextPage(ATransition: TPageTransition): Boolean;
    function PreviousPage(ATransition: TPageTransition): Boolean;
    function FirstPage(ATransition: TPageTransition): Boolean;
    function LastPage(ATransition: TPageTransition): Boolean;
    function AddPage(const APageViewClass: TALPageViewClass = nil): TALPageView;
    function InsertPage(const AIndex: Integer; const APageViewClass: TALPageViewClass = nil): TALPageView;
    procedure DeletePage(const AIndex: Integer);
    property PageCount: Integer read GetPageCount;
    function HasActivePage: Boolean;
    property Pages[AIndex: Integer]: TALPageView read GetPage;
    property ActivePage: TALPageView read GetActivePage write SetActivePage stored False;
    property Content: TContent read FContent;
    property ClipChildren default True;
    property CacheEngine;
    property CacheIndex;
  published
    //property Action;
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex default -1;
    property Align;
    property Anchors;
    //property AutoSize;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DoubleBuffered;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Fill;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Orientation: TOrientation read FOrientation write SetOrientation default TOrientation.Horizontal;
    //property Padding;
    /// <summary>
    ///   Whether to add padding to both ends of the list.
    ///   If this is set to true and PageController.viewportFraction < 1.0, padding will be
    ///   added such that the first and last child slivers will be in the center of the
    ///   viewport when scrolled all the way to the start or end, respectively.
    ///   If PageController.viewportFraction >= 1.0, this property has no effect.
    /// </summary>
    property PadEnds: Boolean read FPadEnds write FPadEnds default true;
    property PageIndicator: TALBasePageIndicator read FPageIndicator write SetPageIndicator;
    /// <summary>
    ///   Set to false to disable page snapping, useful for custom scroll behavior.
    ///   If the padEnds is false and PageController.viewportFraction < 1.0, the page
    ///   will snap to the beginning of the viewport; otherwise, the page will snap
    ///   to the center of the viewport.
    /// </summary>
    property PageSnapping: Boolean read FPageSnapping write FPageSnapping default true;
    property PopupMenu;
    property Position;
    property RotationAngle;
    //property RotationCenter;
    property Pivot;
    property Scale;
    property ScrollEngine: TALScrollEngine read GetScrollEngine write SetScrollEngine;
    property Sides;
    property Size;
    property Stroke;
    //property TabOrder;
    //property TabStop;
    //property TouchTargetExpansion;
    /// <summary>
    ///   The fraction of the viewport that each page should occupy.
    ///   Defaults to 1.0, which means each page fills the viewport in the scrolling direction.
    /// </summary>
    property ViewportFraction: Single read FViewportFraction write SetViewportFraction Stored IsViewportFractionStored nodefault;
    property Visible;
    property Width;
    property OnAniStart: TnotifyEvent read fOnAniStart write fOnAniStart;
    property OnAniStop: TnotifyEvent read fOnAniStop write fOnAniStop;
    //property OnCanFocus;
    /// <summary>
    ///   Called whenever the active page changes.
    /// </summary>
    property OnActivePageChanged: TNotifyEvent read FOnActivePageChanged write FOnActivePageChanged;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    //property OnEnter;
    //property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    property OnClick;
    //property OnDblClick;
    //property OnKeyDown;
    //property OnKeyUp;
    property OnPainting;
    property OnPaint;
    //property OnResize;
    property OnResized;
    property OnViewportPositionChange: TViewportPositionChangeEvent read FOnViewportPositionChange write FOnViewportPositionChange;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.Math,
  System.Math.Vectors,
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  FMX.Utils;

{************************}
function TALPageIndicator.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TALphaColors.Null;
end;

{************************}
function TALPageIndicator.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TALphaColors.Null;
end;

{************************}
function TALPageIndicator.TIndicator.TMargins.GetDefaultValue: TRectF;
begin
  Result := TRectF.Create(3,3,3,3);
end;

{************************}
function TALPageIndicator.TIndicator.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $FFadadad;
end;

{************************}
function TALPageIndicator.TIndicator.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{************************}
constructor TALPageIndicator.TIndicator.Create;
begin
  inherited;
  FWidth := DefaultWidth;
  FHeight := DefaultHeight;
  FMargins := CreateMargins;
  FMargins.OnChanged := MarginsChanged;
  FFill := CreateFill;
  FFill.OnChanged := FillChanged;
  FStroke := CreateStroke;
  FStroke.OnChanged := StrokeChanged;
  FShadow := CreateShadow;
  FShadow.OnChanged := ShadowChanged;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  FCorners := DefaultCorners;
  FSides := DefaultSides;
end;

{************************}
destructor TALPageIndicator.TIndicator.Destroy;
begin
  ALFreeAndNil(FMargins);
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FShadow);
  inherited;
end;

{************************}
function TALPageIndicator.TIndicator.CreateMargins: TALBounds;
begin
  Result := TMargins.Create;
end;

{************************}
function TALPageIndicator.TIndicator.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{************************}
function TALPageIndicator.TIndicator.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{************************}
function TALPageIndicator.TIndicator.CreateShadow: TALShadow;
begin
  Result := TALShadow.Create;
end;

{************************}
procedure TALPageIndicator.TIndicator.Assign(Source: TPersistent);
begin
  if Source is TIndicator then begin
    BeginUpdate;
    Try
      Width := TIndicator(Source).Width;
      Height := TIndicator(Source).Height;
      Margins.Assign(TIndicator(Source).Margins);
      Fill.Assign(TIndicator(Source).Fill);
      Stroke.Assign(TIndicator(Source).Stroke);
      Shadow.Assign(TIndicator(Source).Shadow);
      XRadius := TIndicator(Source).XRadius;
      YRadius := TIndicator(Source).YRadius;
      Corners := TIndicator(Source).Corners;
      Sides := TIndicator(Source).Sides;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{************************}
procedure TALPageIndicator.TIndicator.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Width := DefaultWidth;
    Height := DefaultHeight;
    Margins.Reset;
    Fill.Reset;
    Stroke.Reset;
    Shadow.Reset;
    XRadius := DefaultXRadius;
    YRadius := DefaultYRadius;
    Corners := DefaultCorners;
    Sides := DefaultSides;
  finally
    EndUpdate;
  end;
end;

{************************}
procedure TALPageIndicator.TIndicator.AlignToPixel;
begin
  BeginUpdate;
  try
    Width := ALAlignDimensionToPixelRound(Width, ALGetScreenScale, TEpsilon.Position);
    Height := ALAlignDimensionToPixelRound(Width, ALGetScreenScale, TEpsilon.Position);
    Margins.AlignToPixel;
    Fill.AlignToPixel;
    Stroke.AlignToPixel;
    Shadow.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{************************}
function TALPageIndicator.TIndicator.GetDefaultWidth: Single;
begin
  Result := 8;
end;

{************************}
function TALPageIndicator.TIndicator.GetDefaultHeight: Single;
begin
  Result := 8;
end;

{************************}
function TALPageIndicator.TIndicator.GetDefaultXRadius: Single;
begin
  Result := -50;
end;

{************************}
function TALPageIndicator.TIndicator.GetDefaultYRadius: Single;
begin
  Result := -50;
end;

{************************}
function TALPageIndicator.TIndicator.GetDefaultCorners: TCorners;
begin
  Result := AllCorners
end;

{************************}
function TALPageIndicator.TIndicator.GetDefaultSides: TSides;
begin
  Result := AllSides;
end;

{************************}
procedure TALPageIndicator.TIndicator.SetWidth(const Value: Single);
begin
  if not SameValue(FWidth, Value, TEpsilon.Position) then begin
    FWidth := Value;
    Change;
  end;
end;

{************************}
procedure TALPageIndicator.TIndicator.SetHeight(const Value: Single);
begin
  if not SameValue(FHeight, Value, TEpsilon.Position) then begin
    FHeight := Value;
    Change;
  end;
end;

{************************}
procedure TALPageIndicator.TIndicator.SetMargins(const Value: TALBounds);
begin
  FMargins.Assign(Value);
end;

{************************}
procedure TALPageIndicator.TIndicator.SetFill(const Value: TALBrush);
begin
  FFill.Assign(Value);
end;

{************************}
procedure TALPageIndicator.TIndicator.SetStroke(const Value: TALStrokeBrush);
begin
  FStroke.Assign(Value);
end;

{************************}
procedure TALPageIndicator.TIndicator.SetShadow(const Value: TALShadow);
begin
  FShadow.Assign(Value);
end;

{************************}
procedure TALPageIndicator.TIndicator.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    FXRadius := Value;
    Change;
  end;
end;

{************************}
procedure TALPageIndicator.TIndicator.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    FYRadius := Value;
    Change;
  end;
end;

{************************}
procedure TALPageIndicator.TIndicator.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then begin
    FCorners := Value;
    Change;
  end;
end;

{************************}
procedure TALPageIndicator.TIndicator.SetSides(const Value: TSides);
begin
  if FSides <> Value then begin
    FSides := Value;
    Change;
  end;
end;

{************************}
procedure TALPageIndicator.TIndicator.MarginsChanged(ASender: TObject);
begin
  Change;
end;

{************************}
procedure TALPageIndicator.TIndicator.FillChanged(ASender: TObject);
begin
  Change;
end;

{************************}
procedure TALPageIndicator.TIndicator.StrokeChanged(ASender: TObject);
begin
  Change;
end;

{************************}
procedure TALPageIndicator.TIndicator.ShadowChanged(ASender: TObject);
begin
  Change;
end;

{************************}
function TALPageIndicator.TIndicator.IsWidthStored: Boolean;
begin
  result := not SameValue(fWidth, DefaultWidth, Tepsilon.Position);
end;

{************************}
function TALPageIndicator.TIndicator.IsHeightStored: Boolean;
begin
  result := not SameValue(fHeight, DefaultHeight, Tepsilon.Position);
end;

{************************}
function TALPageIndicator.TIndicator.IsXRadiusStored: Boolean;
begin
  result := not SameValue(fXRadius, DefaultXRadius, Tepsilon.Vector);
end;

{************************}
function TALPageIndicator.TIndicator.IsYRadiusStored: Boolean;
begin
  result := not SameValue(fYRadius, DefaultYRadius, Tepsilon.Vector);
end;

{************************}
function TALPageIndicator.TIndicator.IsCornersStored: Boolean;
begin
  Result := FCorners <> DefaultCorners;
end;

{************************}
function TALPageIndicator.TIndicator.IsSidesStored: Boolean;
begin
  Result := FSides <> Defaultsides;
end;

{************************}
function TALPageIndicator.TActiveIndicator.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := $FF5e6ec1;
end;

{*********************************************}
function TALPageIndicator.TActiveIndicator.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{*******************************************}
function TALPageIndicator.TActiveIndicatorControl.GetCacheSubIndex: Integer;
begin
  Result := 1;
end;

{*******************************************}
function TALPageIndicator.TInactiveIndicatorControl.GetCacheSubIndex: Integer;
begin
  Result := 2;
end;

{******************************************************}
constructor TALPageIndicator.Create(AOwner: TComponent);
begin
  inherited;
  FAnimationType := TanimationType.None;
  FPageCount := 0;
  FActivePageIndex := -1;
  FAnimationValue := 0;
  FActiveIndicatorControl := nil;
  FActiveIndicator := TActiveIndicator.Create;
  FActiveIndicator.OnChanged := ActiveIndicatorChanged;
  FInactiveIndicator := TInactiveIndicator.Create;
  FInactiveIndicator.OnChanged := InactiveIndicatorChanged;
  AutoSize := True;
end;

{******************************************************}
destructor TALPageIndicator.Destroy;
begin
  ALFreeAndNil(FActiveIndicator);
  ALFreeAndNil(FInactiveIndicator);
  inherited;
end;

{******************************************************}
procedure TALPageIndicator.AlignToPixel;
begin
  BeginUpdate;
  Try
    inherited;
    FActiveIndicator.AlignToPixel;
    FInActiveIndicator.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
function TALPageIndicator.CreateFill: TALBrush;
begin
  Result := TFill.Create;
end;

{******************************************************}
function TALPageIndicator.CreateStroke: TALStrokeBrush;
begin
  Result := TStroke.Create;
end;

{******************************************************}
procedure TALPageIndicator.SetDoubleBuffered(const AValue: Boolean);
begin
  inherited;
  for var I := 0 to Controls.count - 1 do
    TIndicatorControl(Controls[i]).DoubleBuffered := AValue;
end;

{********************************}
procedure TALPageIndicator.Loaded;
begin
  inherited;
  RebuildIndicatorControls;
end;

{**************************}
procedure TALPageIndicator.InternalDoRealign;
begin
  var LWidth := Padding.Left + Padding.Right +
                ((FInactiveIndicator.Margins.Left + FInactiveIndicator.Margins.Right + FInactiveIndicator.Width) * FPageCount);
  var LHeight := Padding.Top + Padding.Bottom +
                 FInactiveIndicator.Margins.Top + FInactiveIndicator.Margins.Bottom + FInactiveIndicator.Height;
  var LCurrX := Padding.Left + ((Width - LWidth) / 2);
  var LCurrY := Padding.Top + ((Height - LHeight) / 2);
  For var I := 0 to Controls.Count - 1 do begin
    var LIndicator := Controls[i];
    LIndicator.SetBounds(
      LCurrX + FInactiveIndicator.Margins.Left,
      LCurrY + FInactiveIndicator.Margins.Top,
      FInactiveIndicator.Width,
      FInactiveIndicator.Height);
    LCurrX := LCurrX + FInactiveIndicator.Margins.Left + LIndicator.Width + FInactiveIndicator.Margins.right;
  end;
end;

{***********************************}
procedure TALPageIndicator.DoRealign;
begin
  AdjustSize;
  // UpdateActiveIndicator will call InternalDoRealign
  UpdateActiveIndicator;
end;

{**************************************}
procedure TALPageIndicator.AdjustSize;
begin
  if (not (csLoading in ComponentState)) and // Loaded will call again AdjustSize
     (not (csDestroying in ComponentState)) and // If csDestroying do not do autosize
     (ControlsCount > 0) and // If there are no controls, do not perform autosizing
     (HasUnconstrainedAutosizeX or HasUnconstrainedAutosizeY) and // If AutoSize is false nothing to adjust
     (scene <> nil) and // SetNewScene will call again AdjustSize
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

      var LWidth := Padding.Left + Padding.Right +
                    ((FInactiveIndicator.Margins.Left + FInactiveIndicator.Margins.Right + FInactiveIndicator.Width) * FPageCount);
      var LHeight := Padding.Top + Padding.Bottom +
                     FInactiveIndicator.Margins.Top + FInactiveIndicator.Margins.Bottom + FInactiveIndicator.Height;

      if (not HasUnconstrainedAutosizeX) or (SameValue(LWidth, 0, Tepsilon.Position)) then
        LWidth := Width;
      if (not HasUnconstrainedAutosizeY) or (SameValue(LHeight, 0, Tepsilon.Position)) then
        LHeight := Height;
      SetFixedSizeBounds(Position.X, Position.Y, LWidth, LHeight);

    finally
      TNonReentrantHelper.LeaveSection(FIsAdjustingSize)
    end;
  end;
end;

{***********************************************}
Procedure TALPageIndicator.UpdateActiveIndicator;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateWithoutAnimation;
  begin
    if FActiveIndicatorControl <> nil then
      FActiveIndicatorControl.SetBounds(
        Controls[FActivePageIndex].Position.x,
        Controls[FActivePageIndex].Position.y,
        Controls[FActivePageIndex].Width,
        Controls[FActivePageIndex].Height);
  end;

begin

  // Exit if csDestroying or csLoading
  If (csDestroying in componentState) or
     (csLoading in componentState) then exit;

  // Deactivate Align
  if TNonReentrantHelper.EnterSection(FDisableAlign) then begin
    try

      // Reset visibility/scale/color of all controls
      for var I := 0 to Controls.Count - 1 do begin
        Controls[I].Visible := True;
        TIndicatorControl(Controls[I]).Scale := 1;
        if (Controls[I] = FactiveIndicatorControl) then
          TIndicatorControl(Controls[I]).Fill.Color := FActiveIndicator.Fill.Color
        else
          TIndicatorControl(Controls[I]).Fill.Color := FInactiveIndicator.Fill.Color
      end;

      // Realign all controls
      InternalDoRealign;

      // Exit if no Active Page
      if FActivePageIndex = -1 then begin
        if FActiveIndicatorControl <> nil then
          FActiveIndicatorControl.Visible := False;
        exit;
      end;

      // Exit if only one page
      if FPageCount = 1 then begin
        _UpdateWithoutAnimation;
        exit;
      end;

      // Init LDistanceBetweenIndicators
      var LDistanceBetweenIndicators := abs(Controls[1].Position.x - Controls[0].Position.x);

      // Init LAnimationValue
      var LAnimationValue: Single := FAnimationValue;
      if (FActivePageIndex = 0) then LAnimationValue := Max(0,LAnimationValue);
      if (FActivePageIndex = PageCount - 1) then LAnimationValue := Min(0,LAnimationValue);

      // Handle Animation
      Case FAnimationType of

        //-------------------
        //TAnimationType.None
        TAnimationType.None: begin
          _UpdateWithoutAnimation;
        end;

        //-------------------
        //TAnimationType.Worm
        TAnimationType.Worm: begin
          If SameValue(LAnimationValue, 0, TEpsilon.Scale) then _UpdateWithoutAnimation
          else if LAnimationValue > 0 then begin
            FActiveIndicatorControl.SetBounds(
              Controls[FActivePageIndex].Position.x,
              Controls[FActivePageIndex].Position.y,
              Controls[FActivePageIndex].Width + (LDistanceBetweenIndicators * LAnimationValue * 2),
              Controls[FActivePageIndex].Height);
          end
          else begin
            var LWidth := Controls[FActivePageIndex].Width + (LDistanceBetweenIndicators * -LAnimationValue * 2);
            FActiveIndicatorControl.SetBounds(
              Controls[FActivePageIndex].Position.x + Controls[FActivePageIndex].Width - LWidth,
              Controls[FActivePageIndex].Position.y,
              LWidth,
              Controls[FActivePageIndex].Height);
          end;
        end;

        //-----------------------
        //TAnimationType.ThinWorm
        TAnimationType.ThinWorm: begin
          If SameValue(LAnimationValue, 0, TEpsilon.Scale) then _UpdateWithoutAnimation
          else if LAnimationValue > 0 then begin
            var LHeight := Controls[FActivePageIndex].Height - (Controls[FActivePageIndex].Height * LAnimationValue);
            FActiveIndicatorControl.SetBounds(
              Controls[FActivePageIndex].Position.x,
              Controls[FActivePageIndex].Position.y + ((Controls[FActivePageIndex].height - LHeight) / 2),
              Controls[FActivePageIndex].Width + (LDistanceBetweenIndicators * LAnimationValue * 2),
              LHeight);
          end
          else begin
            var LWidth := Controls[FActivePageIndex].Width + (LDistanceBetweenIndicators * -LAnimationValue * 2);
            var LHeight := Controls[FActivePageIndex].Height - (Controls[FActivePageIndex].Height * -LAnimationValue);
            FActiveIndicatorControl.SetBounds(
              Controls[FActivePageIndex].Position.x + Controls[FActivePageIndex].Width - LWidth,
              Controls[FActivePageIndex].Position.y + ((Controls[FActivePageIndex].height - LHeight) / 2),
              LWidth,
              LHeight);
          end;
        end;

        //--------------------------------
        //TAnimationType.JumpingDotTopView
        TAnimationType.JumpingDotTopView: begin
          If SameValue(LAnimationValue, 0, TEpsilon.Scale) then _UpdateWithoutAnimation
          else begin
            FActiveIndicatorControl.Position.x := Controls[FActivePageIndex].Position.x + (LDistanceBetweenIndicators * LAnimationValue);
            FActiveIndicatorControl.Scale := 1 + abs(LAnimationValue * 1.2);
          end;
        end;

        //---------------------------------
        //TAnimationType.jumpingDotSideView
        TAnimationType.jumpingDotSideView: begin
          If SameValue(LAnimationValue, 0, TEpsilon.Scale) then _UpdateWithoutAnimation
          else if LAnimationValue > 0 then begin
            var LRadius := LDistanceBetweenIndicators / 2.0;
            var LCenterX := Controls[FActivePageIndex].Position.x + LRadius;
            var LTheta := pi * (1 - LAnimationValue);
            FActiveIndicatorControl.Position.Point := TPointF.Create(
              LCenterX + LRadius * Cos(LTheta),
              Controls[FActivePageIndex].Position.y - LRadius * Sin(LTheta));
          end
          else begin
            var LRadius := LDistanceBetweenIndicators / 2.0;
            var LCenterX := Controls[FActivePageIndex].Position.x - LRadius;
            var LTheta := pi * -LAnimationValue;
            FActiveIndicatorControl.Position.Point := TPointF.Create(
              LCenterX + LRadius * Cos(LTheta),
              Controls[FActivePageIndex].Position.y - LRadius * Sin(LTheta));
          end;
        end;

        //--------------------
        //TAnimationType.Slide
        TAnimationType.Slide: begin
          If SameValue(LAnimationValue, 0, TEpsilon.Scale) then _UpdateWithoutAnimation
          else FActiveIndicatorControl.Position.x := Controls[FActivePageIndex].Position.x + (LDistanceBetweenIndicators * LAnimationValue);
        end;

        //--------------------
        //TAnimationType.Scale
        TAnimationType.Scale: begin
          const LMaxDeltaScale = 0.5;
          if (LAnimationValue > 0) and (FActivePageIndex < FPageCount - 1) then begin
            TIndicatorControl(Controls[FActivePageIndex + 1]).Scale := 1 + (LAnimationValue * LMaxDeltaScale);
            TIndicatorControl(Controls[FActivePageIndex]).Scale := 1 + LMaxDeltaScale - (LAnimationValue * LMaxDeltaScale);
          end
          else if (LAnimationValue < 0) and (FActivePageIndex > 0) then begin
            TIndicatorControl(Controls[FActivePageIndex - 1]).Scale := 1 + (-LAnimationValue * LMaxDeltaScale);
            TIndicatorControl(Controls[FActivePageIndex]).Scale := 1 + LMaxDeltaScale - (-LAnimationValue * LMaxDeltaScale);
          end
          else TIndicatorControl(Controls[FActivePageIndex ]).Scale := 1 + LMaxDeltaScale;
          FActiveIndicatorControl.Visible := False;
        end;

        //-----------------------
        //TAnimationType.SpinSwap
        TAnimationType.SpinSwap: begin
          If SameValue(LAnimationValue, 0, TEpsilon.Scale) then _UpdateWithoutAnimation
          else begin
            Controls[FActivePageIndex].Visible := False;
            if (LAnimationValue > 0) and (FActivePageIndex < FPageCount - 1) then begin
              var LRadius := LDistanceBetweenIndicators / 2.0;
              var LCenterX := Controls[FActivePageIndex].Position.x + LRadius;
              var LTheta1 := pi * (1 - LAnimationValue);
              var LTheta2 := pi * (LAnimationValue);
              FActiveIndicatorControl.Position.Point := TPointF.Create(
                LCenterX + LRadius * Cos(LTheta1),
                Controls[FActivePageIndex].Position.y + LRadius * Sin(LTheta1));
              Controls[FActivePageIndex+1].Position.Point := TPointF.Create(
                LCenterX + LRadius * Cos(LTheta2),
                Controls[FActivePageIndex].Position.y - LRadius * Sin(LTheta2));
            end
            else if (LAnimationValue < 0) and (FActivePageIndex > 0) then begin
              var LRadius := LDistanceBetweenIndicators / 2.0;
              var LCenterX := Controls[FActivePageIndex].Position.x - LRadius;
              var LTheta2 := pi * (1 + LAnimationValue);
              var LTheta1 := pi * (-LAnimationValue);
              FActiveIndicatorControl.Position.Point := TPointF.Create(
                LCenterX + LRadius * Cos(LTheta1),
                Controls[FActivePageIndex].Position.y + LRadius * Sin(LTheta1));
              Controls[FActivePageIndex-1].Position.Point := TPointF.Create(
                LCenterX + LRadius * Cos(LTheta2),
                Controls[FActivePageIndex].Position.y - LRadius * Sin(LTheta2));
            end;
          end;
        end;

        //-------------------------
        //TAnimationType.LinearSwap
        TAnimationType.LinearSwap: begin
          If SameValue(LAnimationValue, 0, TEpsilon.Scale) then _UpdateWithoutAnimation
          else begin
            FActiveIndicatorControl.Position.X := Controls[FActivePageIndex].Position.x + (LDistanceBetweenIndicators * LAnimationValue);
            Controls[FActivePageIndex].Visible := False;
            if (LAnimationValue > 0) and (FActivePageIndex < FPageCount - 1) then
              Controls[FActivePageIndex + 1].Position.x := Controls[FActivePageIndex + 1].Position.x - (LDistanceBetweenIndicators * LAnimationValue)
            else if (LAnimationValue < 0) and (FActivePageIndex > 0) then
              Controls[FActivePageIndex - 1].Position.x := Controls[FActivePageIndex - 1].Position.x + (LDistanceBetweenIndicators * -LAnimationValue);
          end;
        end;

        //--------------------
        //TAnimationType.Color
        TAnimationType.Color: begin
          If SameValue(LAnimationValue, 0, TEpsilon.Scale) then _UpdateWithoutAnimation
          else if (LAnimationValue > 0) and (FActivePageIndex < FPageCount - 1) then begin
            FActiveIndicatorControl.Visible := False;
            TIndicatorControl(Controls[FActivePageIndex]).fill.Color := Interpolatecolor(FActiveIndicatorControl.fill.Color{Start}, FInActiveIndicator.Fill.Color{Stop}, LAnimationValue);
            TIndicatorControl(Controls[FActivePageIndex + 1]).Fill.Color := Interpolatecolor(FInActiveIndicator.Fill.Color{Start}, FActiveIndicatorControl.fill.Color{Stop}, LAnimationValue);
          end
          else if (LAnimationValue < 0) and (FActivePageIndex > 0) then begin
            FActiveIndicatorControl.Visible := False;
            TIndicatorControl(Controls[FActivePageIndex]).fill.Color := Interpolatecolor(FActiveIndicatorControl.fill.Color{Start}, FInActiveIndicator.Fill.Color{Stop}, -LAnimationValue);
            TIndicatorControl(Controls[FActivePageIndex - 1]).Fill.Color := Interpolatecolor(FInActiveIndicator.Fill.Color{Start}, FActiveIndicatorControl.fill.Color{Stop}, -LAnimationValue);
          end;
        end;

        //-------
        //Unknown
        Else
          Raise Exception.Create('Error 8E8D21F9-593D-40BA-90E3-390F5EC36F18')

      end;

    finally
      TNonReentrantHelper.LeaveSection(FDisableAlign);
    end;
  end;

end;

{**************************************}
procedure TALPageIndicator.RebuildIndicatorControls;
begin
  If (csDestroying in componentState) or
     (csloading in componentState) then exit;
  BeginUpdate;
  try

    While ControlsCount > 0 do begin
      var LControl := Controls[ControlsCount - 1];
      ALFreeAndNil(LControl);
    end;

    For var I := 0 to FPageCount - 1 do begin
      var LIndicator := TInactiveIndicatorControl.Create(self);
      LIndicator.Parent := Self;
      LIndicator.Stored := False;
      LIndicator.Locked := True;
      LIndicator.HitTest := False;
      LIndicator.Fill.Assign(FInactiveIndicator.Fill);
      LIndicator.Stroke.Assign(FInactiveIndicator.Stroke);
      LIndicator.Shadow.Assign(FInactiveIndicator.Shadow);
      LIndicator.XRadius := FInactiveIndicator.XRadius;
      LIndicator.YRadius := FInactiveIndicator.YRadius;
      LIndicator.Corners := FInactiveIndicator.Corners;
      LIndicator.Sides := FInactiveIndicator.Sides;
      LIndicator.CacheEngine := CacheEngine;
      LIndicator.CacheIndex := CacheIndex;
      LIndicator.DoubleBuffered := DoubleBuffered;
    end;

    FActiveIndicatorControl := TActiveIndicatorControl.Create(self);
    FActiveIndicatorControl.Parent := Self;
    FActiveIndicatorControl.Stored := False;
    FActiveIndicatorControl.Locked := True;
    FActiveIndicatorControl.HitTest := False;
    FActiveIndicatorControl.Fill.Assign(FActiveIndicator.Fill);
    FActiveIndicatorControl.Stroke.Assign(FActiveIndicator.Stroke);
    FActiveIndicatorControl.Shadow.Assign(FActiveIndicator.Shadow);
    FActiveIndicatorControl.XRadius := FActiveIndicator.XRadius;
    FActiveIndicatorControl.YRadius := FActiveIndicator.YRadius;
    FActiveIndicatorControl.Corners := FActiveIndicator.Corners;
    FActiveIndicatorControl.Sides := FActiveIndicator.Sides;
    FActiveIndicatorControl.CacheEngine := CacheEngine;
    FActiveIndicatorControl.CacheIndex := CacheIndex;
    FActiveIndicatorControl.DoubleBuffered := DoubleBuffered;

  finally
    // This will call realign
    EndUpdate;
  end;
end;

{*******************************}
procedure TALPageIndicator.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{******************************************************}
procedure TALPageIndicator.AnimationProcess(Const AValue: Single);
begin
  if not SameValue(FAnimationValue, AValue, TEpsilon.Scale) then begin
    FAnimationValue := AValue;
    UpdateActiveIndicator;
  end;
end;

{******************************************************}
procedure TALPageIndicator.ActivePageChanged(const ANewActivePageIndex: Integer);
begin
  if ANewActivePageIndex <> FActivePageIndex then begin
    FActivePageIndex := ANewActivePageIndex;
    if SameValue(FAnimationValue, 0, TEpsilon.Scale) then
      UpdateActiveIndicator;
  end;
end;

{******************************************************}
procedure TALPageIndicator.PageCountChanged(Const ANewPageCount: Integer; const ANewActivePageIndex: Integer);
begin
  if ANewPageCount <> FPageCount then begin
    FPageCount := ANewPageCount;
    FActivePageIndex := ANewActivePageIndex;
    RebuildIndicatorControls;
  end;
end;

{******************************************************}
procedure TALPageIndicator.SetAnimationType(const AValue: TAnimationType);
begin
  If FAnimationType <> AValue then begin
    FAnimationType := AValue;
    UpdateActiveIndicator;
  end;
end;

{******************************************************}
procedure TALPageIndicator.SetActiveIndicator(const AValue: TActiveIndicator);
begin
  FActiveIndicator.Assign(AValue);
end;

{******************************************************}
procedure TALPageIndicator.SetInactiveIndicator(const AValue: TInActiveIndicator);
begin
  FInactiveIndicator.Assign(AValue);
end;

{******************************************************}
procedure TALPageIndicator.ActiveIndicatorChanged(ASender: TObject);
begin
  RebuildIndicatorControls;
end;

{******************************************************}
procedure TALPageIndicator.InactiveIndicatorChanged(ASender: TObject);
begin
  RebuildIndicatorControls;
end;

{******************************************************}
function TALPageView.TFill.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{********************************************************}
function TALPageView.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*************************************************}
constructor TALPageView.Create(AOwner: TComponent);
begin
  inherited;
  FPageController := nil;
  HitTest := False;
  Locked := True;
end;

{****************************************}
function TALPageView.CreateFill: TALBrush;
begin
  result := TFill.Create;
end;

{************************************************}
function TALPageView.CreateStroke: TALStrokeBrush;
begin
  result := TStroke.Create;
end;

{**************************************}
function TALPageView.GetActive: Boolean;
begin
  Result := (FPageController <> nil) and
            (FPageController.ActivePage = Self);
end;

{****************************************************}
procedure TALPageView.SetActive(const Value: Boolean);
begin
  if (FPageController <> nil) and (Value) then
    FPageController.SetActivePage(Self)
end;

{**********************************}
procedure TALPageView.ParentChanged;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _FindPageController: TALPageController;
  begin
    var P := ParentControl;
    while P <> nil do begin
      if P is TALPageController then begin
        Result := TALPageController(P);
        Exit;
      end;
      P := P.ParentControl;
    end;
    Result := nil;
  end;

begin
  inherited ParentChanged;
  FPageController := _FindPageController;
end;

{********************************************************}
function TALPageController.TStroke.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{*****************************************************************}
constructor TALPageController.TContent.Create(AOwner: TComponent);
begin
  ValidateInheritance(AOwner, TALPageController, False{CanBeNil});
  inherited Create(AOwner);
  FPageController := TALPageController(AOwner);
end;

{*************}
{$IFNDEF ALDPK}
function TALPageController.TContent.IsVisibleObject(const AObject: TControl): Boolean;
begin

  if AObject.Visible then begin

    if FPageController.Orientation = TOrientation.Horizontal then begin
      Result := (AObject.Position.X < -Position.X + FPageController.Width) and
                (AObject.Position.X + AObject.Width > -Position.X);
    end
    else begin
      Result := (AObject.Position.Y < -Position.Y + FPageController.Height) and
                (AObject.Position.Y + AObject.Height > -Position.Y);
    end;

  end
  else
    result := False;

end;
{$ENDIF}

{**************************************************************************}
procedure TALPageController.TContent.DoAddObject(const AObject: TFmxObject);
begin
  // At design time, the Delphi IDE may add children such as
  // TGrabHandle.TGrabHandleRectangle. We want to prevent these from
  // being added to TContent.
  If not (AObject is TALPageView) then FPageController.AddObject(AObject)
  else begin
    inherited;
    if FPageController.PageIndicator <> nil then
      FPageController.PageIndicator.PageCountChanged(ControlsCount, FPageController.ActivePageIndex);
  end;
end;

{**************************************************************************}
procedure TALPageController.TContent.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited;
  if FPageController.PageIndicator <> nil then
    FPageController.PageIndicator.PageCountChanged(ControlsCount, FPageController.ActivePageIndex);
end;

{*****************************************************}
procedure TALPageController.TContent.DoDeleteChildren;
begin
  inherited;
  if FPageController.PageIndicator <> nil then
    FPageController.PageIndicator.PageCountChanged(ControlsCount, FPageController.ActivePageIndex);
end;

{****************************************************}
procedure TALPageController.TContent.DoContentChanged;
begin
  FPageController.Realign;
end;

{*****************************************************************************}
constructor TALPageController.TScrollEngine.Create(const APageControl: TALPageController);
begin
  inherited Create;
  FPageController := APageControl;
  fLastViewportPosition := TALPointD.Create(0,0);
  FDownPageIndex := -1;
End;

{****************************************************}
procedure TALPageController.TScrollEngine.DoMouseDown;
begin
  Inherited;
  FDownPageIndex := FPageController.ActivePageIndex;
end;

{**************************************************}
procedure TALPageController.TScrollEngine.DoMouseUp;
begin

  // Record whether the control was in
  // a pressed (down) state before processing.
  var LWasDown := Down;

   // Call the inherited mouse up handler.
  inherited;

  // If the control was not pressed before
  // this event, exit early.
  if not LWasDown then exit;

  // If page snapping is disabled, exit the procedure
  // since no automatic page alignment should occur.
  if not FPageController.PageSnapping then exit;

  // If there is no active page available, exit early.
  if not FPageController.HasActivePage then exit;

  {$IFDEF DEBUG}
  //ALLog('TALPageController.TScrollEngine.DoMouseUp', 'ScrollCapturedByMe:'+ALBooltoStrW(FPageController.fScrollCapturedByMe, 'True', 'False'));
  {$ENDIF}

  // Initialize the target page index with the current active page.
  var LTargetPageIndex := FPageController.ActivePageIndex;
  if (FPageController.fScrollCapturedByMe) and (LTargetPageIndex = FDownPageIndex) then begin
    var LCmpVelocity: TValueRelationship;
    If FPageController.Orientation = TOrientation.Horizontal then LCmpVelocity := compareValue(UpVelocity.X, 0, Tepsilon.Position)
    else LCmpVelocity := compareValue(UpVelocity.Y, 0, Tepsilon.Position);
    if LCmpVelocity > 0 then begin
      if ((FPageController.Orientation = TOrientation.Horizontal) and
          (DownPosition.X > UpPosition.X)) or
         ((FPageController.Orientation = TOrientation.Vertical) and
          (DownPosition.Y > UpPosition.Y)) then
        LTargetPageIndex := Min(LTargetPageIndex+1, FPageController.PageCount-1);
    end
    else if LCmpVelocity < 0 then begin
      if ((FPageController.Orientation = TOrientation.Horizontal) and
          (DownPosition.X < UpPosition.X)) or
         ((FPageController.Orientation = TOrientation.Vertical) and
          (DownPosition.Y < UpPosition.Y)) then
        LTargetPageIndex := Max(LTargetPageIndex-1, 0);
    end;
  end;

  // Calculate the maximum allowed velocity, scaled to the current screen.
  var LMaximumVelocity := MAXIMUM_FLING_VELOCITY * ALGetScreenScale;
  var LVelocity: Single;
  If FPageController.Orientation = TOrientation.Horizontal then LVelocity := EnsureRange(CurrentVelocity.X, -LMaximumVelocity, LMaximumVelocity)
  else LVelocity := EnsureRange(CurrentVelocity.Y, -LMaximumVelocity, LMaximumVelocity);

  // Set the target page as active using
  // a slide transition with the computed velocity.
  FPageController.SetActivePage(
    FPageController.Pages[LTargetPageIndex],
    TALPageController.TPageTransition.Slide,
    LVelocity);

end;

{************************************************}
procedure TALPageController.TScrollEngine.DoStart;
begin
  inherited DoStart;

  if (FPageController.Scene <> nil) and
     (not (csDestroying in FPageController.ComponentState)) then
    FPageController.Scene.ChangeScrollingState(FPageController, True);

  if (assigned(FPageController.fOnAniStart)) and
     (not (csDestroying in FPageController.ComponentState)) then
    FPageController.fOnAniStart(FPageController);
end;

{************************************************}
procedure TALPageController.TScrollEngine.DoStop;
begin
  inherited DoStop;

  if (FPageController.Scene <> nil) and
     (not (csDestroying in FPageController.ComponentState)) then
    FPageController.Scene.ChangeScrollingState(nil, False);

  if (assigned(FPageController.fOnAniStop)) and
     (not (csDestroying in FPageController.ComponentState)) then
    FPageController.fOnAniStop(FPageController);
end;

{********************************************}
procedure TALPageController.TScrollEngine.DoChanged;
begin
  {$IF defined(debug)}
  //ALLog(ClassName + '.TALPageController.TScrollEngine.DoChanged');
  {$ENDIF}
  if (not (csDestroying in FPageController.ComponentState)) then begin

    var LSaveDisableAlign := FPageController.FDisableAlign;
    FPageController.FDisableAlign := True;
    try
      FPageController.Content.Position.Point := -TPointF.Create(
                                                  ViewportPosition.X,
                                                  ViewportPosition.Y);
    finally
      FPageController.FDisableAlign := LSaveDisableAlign;
    end;

    FpageController.RefreshActivePageIndex;

    var LNewViewportPosition := ViewportPosition;
    if (assigned(FPageController.FOnViewportPositionChange)) and
       (not fLastViewportPosition.EqualsTo(LNewViewportPosition, TEpsilon.Position)) then
      FPageController.FOnViewportPositionChange(self, fLastViewportPosition, LNewViewportPosition);
    fLastViewportPosition := LNewViewportPosition;

    If FPageController.PageIndicator <> nil then begin
      if not FPageController.HasActivePage then
        FPageController.PageIndicator.AnimationProcess(0)
      else if FPageController.Orientation = TOrientation.Horizontal then
        FPageController.PageIndicator.AnimationProcess((FPageController.GetEndPadding - FPageController.Content.Position.X - FPageController.ActivePage.Position.X) / FPageController.GetPageSize)
      else
        FPageController.PageIndicator.AnimationProcess((FPageController.GetEndPadding - FPageController.Content.Position.Y - FPageController.ActivePage.Position.Y) / FPageController.GetPageSize);
    end;

  end;
  inherited DoChanged;
end;

{***************************************************}
constructor TALPageController.Create(AOwner: TComponent);
begin
  inherited;
  //--
  FViewportFraction := 1;
  FActivePageIndex := -1;
  FScrollEngine := CreateScrollEngine;
  FContent := CreateContent;
  FPageIndicator := nil;
  fMouseDownPos := TpointF.Zero;
  FFadeAnimation := nil;
  FFadeFromPageIndex := -1;
  FFadeToPageIndex := -1;
  FFadeOverlay := nil;
  FFadeTouchMode := TALScrollEngine.TTouchMode.Auto;
  FPageSnapping := True;
  FPadEnds := True;
  FHandleMouseEvents := False;
  fScrollCapturedByMe := False;
  FOrientation := TOrientation.Horizontal;
  FOnViewportPositionChange := nil;
  FOnActivePageChanged := nil;
  fOnAniStart := nil;
  fOnAniStop := nil;
  //--
  ClipChildren := true;
  AutoCapture := True;
  SetAcceptsControls(True);
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
end;

{*******************************}
destructor TALPageController.Destroy;
begin
  SetPageIndicator(nil);
  ALFreeAndNil(FScrollEngine);
  ALFreeAndNil(FFadeAnimation);
  inherited;
end;

{************************************}
procedure TALPageController.BeforeDestruction;
begin
  if BeforeDestructionExecuted then exit;
  // Unsubscribe from TALScrollCapturedMessage to stop receiving messages.
  // This must be done in BeforeDestruction rather than in Destroy,
  // because the control might be freed in the background via ALFreeAndNil(..., delayed),
  // and BeforeDestruction is guaranteed to execute on the main thread.
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  if FFadeAnimation <> nil then FFadeAnimation.Enabled := False;
  FScrollEngine.Stop(True{AAbruptly});
  inherited;
end;

{************************************************}
function TALPageController.CreateStroke: TALStrokeBrush;
begin
  result := TStroke.Create;
end;

{*************************************************}
function TALPageController.CreateContent: TContent;
begin
  Result := TContent.Create(Self);
  Result.parent := self;
  Result.Stored := False;
  Result.Locked := True;
  Result.HitTest := False;
end;

{*************************************************}
function TALPageController.CreateScrollEngine: TScrollEngine;
begin
  Result := TScrollEngine.Create(Self);
  Result.TouchTracking := [ttHorizontal];
end;

{**************************************************************}
procedure TALPageController.FreeNotification(AObject: TObject);
begin
  inherited;
  if AObject = FPageIndicator then
    FPageIndicator := nil;
end;

{*********************************}
procedure TALPageController.Loaded;
begin
  var FOldDisableAlign := fDisableAlign;
  fDisableAlign := True;
  try
    inherited;
  finally
    fDisableAlign := FOldDisableAlign;
  end;
  DoRealign;
end;

{**********************************************}
function TALPageController.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(200, 200);
end;

{*************************************************************}
procedure TALPageController.DoAddObject(const AObject: TFmxObject);
begin
  if (AObject is TALPageView) then FContent.AddObject(AObject)
  else inherited;
end;

{***********************************************}
procedure TALPageController.RefreshActivePageIndex;
begin
  var LOldActivePageIndex := FActivePageIndex;
  var LActivePageIndex := Max(0, ActivePageIndex);
  var LCenterLine: Single := GetEndPadding + (GetPageSize / 2);
  if Orientation = TOrientation.Horizontal then begin
    For var I := LActivePageIndex to LActivePageIndex + PageCount - 1 do begin
      var LPageIndex := I mod PageCount;
      var LPageView := Pages[LPageIndex];
      If (compareValue(LPageView.position.X + Content.Position.X, LCenterLine, TEpsilon.Position) <= 0) and
         (compareValue(LPageView.position.X + Content.Position.X + LPageView.Width, LCenterLine, TEpsilon.Position) > 0) then begin
        {$IF defined(debug)}
        //if FActivePageIndex <> LPageIndex then
        //  Allog('TALPageController.RefreshActivePageIndex', AlInttostrW(I));
        {$ENDIF}
        FActivePageIndex := LPageIndex;
        Break;
      end;
    end;
  end
  else begin
    For var I := LActivePageIndex to LActivePageIndex + PageCount - 1 do begin
      var LPageIndex := I mod PageCount;
      var LPageView := Pages[LPageIndex];
      If (compareValue(LPageView.position.Y + Content.Position.Y, LCenterLine, TEpsilon.Position) <= 0) and
         (compareValue(LPageView.position.Y + Content.Position.Y + LPageView.Height, LCenterLine, TEpsilon.Position) > 0) then begin
        {$IF defined(debug)}
        //if FActivePageIndex <> LPageIndex then
        //  Allog('TALPageController.RefreshActivePageIndex', AlInttostrW(I));
        {$ENDIF}
        FActivePageIndex := LPageIndex;
        Break;
      end;
    end;
  end;
  if LOldActivePageIndex <> FActivePageIndex then
    DoActivePageChanged;
end;

{***********************************************}
procedure TALPageController.SetPageIndicator(const AValue: TALBasePageIndicator);
begin
  if FPageIndicator <> AValue then begin
    if FPageIndicator <> nil then begin
      FPageIndicator.RemoveFreeNotify(Self);
      FPageIndicator.PageCountChanged(0{ANewPageCount}, -1{ANewActivePageIndex});
    end;
    FPageIndicator := AValue;
    if FPageIndicator <> nil then Begin
      FPageIndicator.AddFreeNotify(Self);
      FPageIndicator.PageCountChanged(PageCount, ActivePageIndex);
    End;
  end;
end;

{***********************************************}
function TALPageController.GetPageSize: Single;
begin
  if Orientation = TOrientation.Horizontal then
    result := Width * viewportFraction
  else
    result := Height * viewportFraction;
end;

{***********************************************}
function TALPageController.GetEndPadding: Single;
begin
  if not FPadEnds then result := 0
  else if Orientation = TOrientation.Horizontal then
    result := (Width * (1 - viewportFraction)) / 2
  else
    result := (Height * (1 - viewportFraction)) / 2;
end;

{***********************************************}
function TALPageController.GetPageCount: integer;
begin
  Result := Content.ControlsCount;
end;

{***************************************************************}
function TALPageController.GetPage(AIndex: Integer): TALPageView;
begin
  if InRange(AIndex, 0, PageCount - 1) then begin
    {$IF defined(debug) or defined(ALDPK)}
    // At design time, the Delphi IDE may add children such as
    // TGrabHandle.TGrabHandleRectangle. We want to be certain
    // that Content.Controls[AIndex] is a TALPageView
    if not (Content.Controls[AIndex] is TALPageView) then
      Raise Exception.Create('Error AAA0A344-0B61-4280-8CD6-98E787A86844');
    {$ENDIF}
    Result := TALPageView(Content.Controls[AIndex])
  end
  else raise Exception.Createfmt('Invalid page index (%d)', [AIndex]);
end;

{********************************************}
function TALPageController.GetItemsCount: Integer;
begin
  Result := GetPageCount;
end;

{****************************************************************}
function TALPageController.GetItem(const AIndex: Integer): TFmxObject;
begin
  Result := GetPage(AIndex);
end;

{********************************************}
function TALPageController.GetScrollEngine: TALScrollEngine;
begin
  result := FScrollEngine;
end;

{********************************************}
procedure TALPageController.SetScrollEngine(const Value: TALScrollEngine);
begin
  FScrollEngine.Assign(Value);
end;

{************************************************}
function TALPageController.HasActivePage: Boolean;
begin
  Result := ActivePageIndex >= 0;
end;

{*****************************************************************************************************************************************}
procedure TALPageController.SetActivePageIndex(const AValue: integer; const ATransition: TPageTransition; const AVelocity: Single = 0);
begin
  SetActivePage(Pages[AValue], ATransition, AVelocity);
end;

{********************************************************************}
procedure TALPageController.SetActivePageIndex(const AValue: Integer);
begin
  if not (csLoading in componentState) then
    SetActivePageIndex(AValue, TPageTransition.None)
  else
    FActivePageIndex := AValue;
end;

{**********************************************}
function TALPageController.GetActivePage: TALPageView;
begin
  if ActivePageIndex >= 0 then Result := Pages[ActivePageIndex]
  else Result := nil;
end;

{************************************************************}
procedure TALPageController.SetActivePage(const AValue: TALPageView);
begin
  if AValue = nil then raise Exception.Create('AValue cannot be nil');
  {$IF defined(debug)}
  if not IsChild(AValue) then raise Exception.Create('AValue is not a child of this controller');
  {$ENDIF}
  SetActivePage(AValue, TPageTransition.None);
end;

{*********************************************************************************************************************************************************}
procedure TALPageController.SetActivePage(const AValue: TALPageView; const ATransition: TPageTransition; const AVelocity: Single = 0);

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    // Extracted from the distanceInfluenceForSnapDuration method in ViewPager.java.
    // We want the duration of the page snap animation to be influenced by the distance that
    // the screen has to travel, however, we don't want this duration to be effected in a
    // purely linear fashion. Instead, we use this method to moderate the effect that the distance
    // of travel has on the overall snap duration.
    function distanceInfluenceForSnapDuration(f: Single): single;
    begin
      f := f - 0.5; // center the values about 0.
      f := f * (0.3 * PI / 2.0);
      Result := sin(f);
    end;

begin

  if AValue = nil then raise Exception.Create('AValue cannot be nil');
  if (FFadeAnimation <> nil) and (FFadeAnimation.Running) then Exit;
  if HasActivePage then ActivePage.ResetFocus;

  {$REGION 'TPageTransition.Slide'}
  if (ATransition = TPageTransition.Slide) and
     (HasActivePage) then begin

    // The following code is extracted from
    // ViewPager.java's smoothScrollTo method.

    if Orientation = TOrientation.Horizontal then begin

      var sx: Single := -Content.Position.X;
      var sy: Single := -Content.Position.Y;
      var dx: Single := AValue.Position.X - GetEndPadding - sx;
      var dy: Single := AValue.Position.y - sy;
      if SameValue(dx, 0, TEpsilon.Position) and
         SameValue(dy, 0, TEpsilon.Position) then exit;

      var LWidth := Width - Padding.Left - Padding.Right;
      var LHalfWidth: Single := LWidth / 2;
      var LDistanceRatio: Single := Min(1.0, Abs(dx) / LWidth);
      var LDistance: Single := LHalfWidth + LHalfWidth * distanceInfluenceForSnapDuration(LDistanceRatio);

      var LDuration: integer;
      var LVelocity: Single := abs(AVelocity);
      if (Lvelocity > 0) then LDuration := 4 * round(1000 * abs(LDistance / Lvelocity))
      else begin
        //
        // in smoothScrollTo it's written
        //
        // final float pageWidth = width * mAdapter.getPageWidth(mCurItem);
        //
        // but in PagerAdapter we have
        //
        //   public float getPageWidth(int position) {
        //     return 1.f;
        //   }
        //
        var LPageWidth: Single := LWidth {* mAdapter.getPageWidth(mCurItem)};
        var LPageDelta: Single := abs(dx) / (LPageWidth {+ mPageMargin});
        LDuration := Trunc((LPageDelta + 1) * 100);
        //
        // If the scroll distance equals one full page (including margin), then:
        //   pageDelta would be 1, so duration would be (1+1)×100=200 milliseconds.
        // If the scroll distance is half of that, then:
        //   pageDelta would be 0.5, and duration would be (0.5+1)×100=150 milliseconds.
        //
        // In general, this calculation produces a duration between 100 and 150 ms, which is too fast.
        // To mitigate this, I multiply the computed duration by 2.
        //
        LDuration := LDuration * 4;
      end;
      LDuration := Min(LDuration, MAX_SETTLE_DURATION);

      ScrollEngine.startScroll(sx, sy, dx, dy, LDuration);

    end
    else begin

      var sx: Single := -Content.Position.X;
      var sy: Single := -Content.Position.Y;
      var dx: Single := AValue.Position.X - sx;
      var dy: Single := AValue.Position.y - GetEndPadding - sy;
      if SameValue(dx, 0, TEpsilon.Position) and
         SameValue(dy, 0, TEpsilon.Position) then exit;

      var LHeight := Height - Padding.Top - Padding.Bottom;
      var LHalfHeight: Single := LHeight / 2;
      var LDistanceRatio: Single := Min(1.0, Abs(dy) / LHeight);
      var LDistance: Single := LHalfHeight + LHalfHeight * distanceInfluenceForSnapDuration(LDistanceRatio);

      var LDuration: integer;
      var LVelocity: Single := abs(AVelocity);
      if (Lvelocity > 0) then LDuration := 4 * round(1000 * abs(LDistance / Lvelocity))
      else begin
        //
        // in smoothScrollTo it's written
        //
        // final float pageWidth = width * mAdapter.getPageWidth(mCurItem);
        //
        // but in PagerAdapter we have
        //
        //   public float getPageWidth(int position) {
        //     return 1.f;
        //   }
        //
        var LPageHeight: Single := LHeight {* mAdapter.getPageHeight(mCurItem)};
        var LPageDelta: Single := abs(dy) / (LPageHeight {+ mPageMargin});
        LDuration := Trunc((LPageDelta + 1) * 100);
        //
        // If the scroll distance equals one full page (including margin), then:
        //   pageDelta would be 1, so duration would be (1+1)×100=200 milliseconds.
        // If the scroll distance is half of that, then:
        //   pageDelta would be 0.5, and duration would be (0.5+1)×100=150 milliseconds.
        //
        // In general, this calculation produces a duration between 100 and 150 ms, which is too fast.
        // To mitigate this, I multiply the computed duration by 2.
        //
        LDuration := LDuration * 4;
      end;
      LDuration := Min(LDuration, MAX_SETTLE_DURATION);

      ScrollEngine.startScroll(sx, sy, dx, dy, LDuration);

    end;

  end
  {$ENDREGION}

  {$REGION 'TPageTransition.DirectFadeIn/DirectFadeOut/OverlayFadeIn/RevealFadeOut/CrossFade'}
  else if (ATransition in [TPageTransition.DirectFadeIn,
                           TPageTransition.DirectFadeOut,
                           TPageTransition.OverlayFadeIn,
                           TPageTransition.RevealFadeOut,
                           TPageTransition.CrossFade]) and
          (HasActivePage) then begin

    if ActivePage = AValue then exit;
    FScrollEngine.Stop(true{AAbruptly});
    If FViewPortFraction <> 1 then
      raise Exception.Create('The fade transition only works when the viewport fraction is set to 1.');

    // FFadeOverlay is to deactivate all mouse / touch event
    ALFreeAndNil(FFadeOverlay);
    FFadeOverlay := TalLayout.Create(self);
    FFadeOverlay.Parent := self;
    FFadeOverlay.Position.Point := TpointF.Create(0,0);
    FFadeOverlay.Size.Size := TpointF.Create(Width, Height);
    FFadeOverlay.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom];
    FFadeOverlay.HitTest := True;
    FFadeOverlay.Visible := True;
    FFadeOverlay.BringToFront;

    if FFadeAnimation = nil then begin
      FFadeAnimation := TALFloatAnimation.Create;
      FFadeAnimation.OnProcess := FadeAnimationProcess;
      FFadeAnimation.OnFinish := FadeAnimationFinish;
      FFadeAnimation.Duration := 2; // 0.3;
    end;

    FFadeTouchMode := FScrollEngine.TouchMode;
    FScrollEngine.TouchMode := TALScrollEngine.TTouchMode.Disabled;
    FFadeAnimation.Tag := integer(ATransition);
    FFadeFromPageIndex := ActivePageIndex;
    FFadeToPageIndex := AValue.Index;

    case ATransition of
      TPageTransition.DirectFadeIn: begin
        FFadeAnimation.StartValue := 0;
        FFadeAnimation.StopValue := 1;
        ActivePageIndex := FFadeToPageIndex;
      end;
      //--
      TPageTransition.DirectFadeOut: begin
        FFadeAnimation.StartValue := 1;
        FFadeAnimation.StopValue := 0;
      end;
      //--
      TPageTransition.OverlayFadeIn: begin
        FDisableAlign := True;
        AValue.Position.Point := ActivePage.Position.Point;
        ActivePage.Index := 0; // FromPageIndex
        Avalue.Index := 1; // ToPageIndex
        FFadeAnimation.StartValue := 0;
        FFadeAnimation.StopValue := 1;
      end;
      //--
      TPageTransition.RevealFadeOut: begin
        FDisableAlign := True;
        AValue.Position.Point := ActivePage.Position.Point;
        var LActivePage := ActivePage;
        Avalue.Index := 0; // ToPageIndex
        LActivePage.Index := 1; // FromPageIndex
        FFadeAnimation.StartValue := 1;
        FFadeAnimation.StopValue := 0;
      end;
      //--
      TPageTransition.CrossFade: begin
        FDisableAlign := True;
        AValue.Position.Point := ActivePage.Position.Point;
        ActivePage.Index := 0; // FromPageIndex
        Avalue.Index := 1; // ToPageIndex
        FFadeAnimation.StartValue := 0;
        FFadeAnimation.StopValue := 1;
      end;
      //--
      else raise Exception.Create('Error AF651414-E6EE-4B42-90D1-3509657FCB22');
    end;

    if (assigned(fOnAniStart)) and
       (not fScrollEngine.TimerActive) then
      fOnAniStart(Self);
    FFadeAnimation.start;

  end
  {$ENDREGION}

  {$REGION 'TPageTransition.None'}
  else begin
    FScrollEngine.Stop;
    if Orientation = TOrientation.Horizontal then
      FScrollEngine.SetViewportPosition(TALPointD.Create(AValue.Position.x - GetEndPadding, AValue.Position.y))
    else
      FScrollEngine.SetViewportPosition(TALPointD.Create(AValue.Position.y, AValue.Position.y - GetEndPadding));
  end;
  {$ENDREGION}

end;

{****************************************************************}
procedure TALPageController.FadeAnimationProcess(Sender: TObject);
begin
  case TPageTransition(FFadeAnimation.Tag) of
    TPageTransition.DirectFadeIn: begin
      //FFadeAnimation.StartValue := 0;
      //FFadeAnimation.StopValue := 1;
      //ActivePageIndex := FFadeToPageIndex;
      Pages[FFadeToPageIndex].Opacity := FFadeAnimation.CurrentValue;
    end;
    //--
    TPageTransition.DirectFadeOut: begin
      //FFadeAnimation.StartValue := 1;
      //FFadeAnimation.StopValue := 0;
      Pages[FFadeFromPageIndex].Opacity := FFadeAnimation.CurrentValue;
    end;
    //--
    TPageTransition.OverlayFadeIn: begin
      //FDisableAlign := True;
      //AValue.Position.Point := ActivePage.Position.Point;
      //ActivePage.Index := 0; // FromPageIndex
      //Avalue.Index := 1; // ToPageIndex
      //FFadeAnimation.StartValue := 0;
      //FFadeAnimation.StopValue := 1;
      Pages[1].Opacity := FFadeAnimation.CurrentValue;
    end;
    //--
    TPageTransition.RevealFadeOut: begin
      //FDisableAlign := True;
      //AValue.Position.Point := ActivePage.Position.Point;
      //var LActivePage := ActivePage;
      //Avalue.Index := 0; // ToPageIndex
      //LActivePage.Index := 1; // FromPageIndex
      //FFadeAnimation.StartValue := 1;
      //FFadeAnimation.StopValue := 0;
      Pages[1].Opacity := FFadeAnimation.CurrentValue;
    end;
    //--
    TPageTransition.CrossFade: begin
      //FDisableAlign := True;
      //AValue.Position.Point := ActivePage.Position.Point;
      //ActivePage.Index := 0; // FromPageIndex
      //Avalue.Index := 1; // ToPageIndex
      //FFadeAnimation.StartValue := 0;
      //FFadeAnimation.StopValue := 1;
      Pages[0].Opacity := 1-FFadeAnimation.CurrentValue;
      Pages[1].Opacity := FFadeAnimation.CurrentValue;
    end;
    //--
    else raise Exception.Create('Error C1EF4949-7B16-4398-BCA0-564B09EDFB3B');
  end;
  if (assigned(FOnViewportPositionChange)) then
    FOnViewportPositionChange(self, FScrollEngine.ViewportPosition, FScrollEngine.ViewportPosition);
end;

{******************************************************************}
procedure TALPageController.FadeAnimationFinish(Sender: TObject);
begin
  case TPageTransition(FFadeAnimation.Tag) of
    TPageTransition.DirectFadeIn: begin
      //FFadeAnimation.StartValue := 0;
      //FFadeAnimation.StopValue := 1;
      //ActivePageIndex := FFadeToPageIndex;
    end;
    //--
    TPageTransition.DirectFadeOut: begin
      //FFadeAnimation.StartValue := 1;
      //FFadeAnimation.StopValue := 0;
      ActivePageIndex := FFadeToPageIndex;
      Pages[FFadeFromPageIndex].Opacity := 1;
    end;
    //--
    TPageTransition.OverlayFadeIn: begin
      //FDisableAlign := True;
      //AValue.Position.Point := ActivePage.Position.Point;
      //ActivePage.Index := 0; // FromPageIndex
      //Avalue.Index := 1; // ToPageIndex
      //FFadeAnimation.StartValue := 0;
      //FFadeAnimation.StopValue := 1;
      var LPage0 := Pages[0];
      var LPage1 := Pages[1];
      if FFadeFromPageIndex{LPage0} < FFadeToPageIndex{LPage1} then begin
        LPage1.Index := FFadeToPageIndex;
        LPage0.Index := FFadeFromPageIndex;
      end
      else begin
        LPage0.Index := FFadeFromPageIndex;
        LPage1.Index := FFadeToPageIndex;
      end;
      FDisableAlign := False;
      DoRealign;
      ActivePageIndex := FFadeToPageIndex;
      LPage1.Opacity := 1;
    end;
    //--
    TPageTransition.RevealFadeOut: begin
      //FDisableAlign := True;
      //AValue.Position.Point := ActivePage.Position.Point;
      //var LActivePage := ActivePage;
      //Avalue.Index := 0; // ToPageIndex
      //LActivePage.Index := 1; // FromPageIndex
      //FFadeAnimation.StartValue := 1;
      //FFadeAnimation.StopValue := 0;
      var LPage0 := Pages[0];
      var LPage1 := Pages[1];
      if FFadeFromPageIndex{LPage0} > FFadeToPageIndex{LPage1} then begin
        LPage1.Index := FFadeFromPageIndex;
        LPage0.Index := FFadeToPageIndex;
      end
      else begin
        LPage0.Index := FFadeToPageIndex;
        LPage1.Index := FFadeFromPageIndex;
      end;
      FDisableAlign := False;
      DoRealign;
      ActivePageIndex := FFadeToPageIndex;
      LPage1.Opacity := 1;
    end;
    //--
    TPageTransition.CrossFade: begin
      //FDisableAlign := True;
      //AValue.Position.Point := ActivePage.Position.Point;
      //ActivePage.Index := 0; // FromPageIndex
      //Avalue.Index := 1; // ToPageIndex
      //FFadeAnimation.StartValue := 0;
      //FFadeAnimation.StopValue := 1;
      var LPage0 := Pages[0];
      var LPage1 := Pages[1];
      if FFadeFromPageIndex{LPage0} < FFadeToPageIndex{LPage1} then begin
        LPage1.Index := FFadeToPageIndex;
        LPage0.Index := FFadeFromPageIndex;
      end
      else begin
        LPage0.Index := FFadeFromPageIndex;
        LPage1.Index := FFadeToPageIndex;
      end;
      FDisableAlign := False;
      DoRealign;
      ActivePageIndex := FFadeToPageIndex;
      LPage0.Opacity := 1;
      LPage1.Opacity := 1;
    end;
    //--
    else raise Exception.Create('Error C1EF4949-7B16-4398-BCA0-564B09EDFB3B');
  end;

  ALFreeAndNil(FFadeOverlay);
  FScrollEngine.TouchMode := FFadeTouchMode;

  if (assigned(fOnAniStop)) and
     (not fScrollEngine.TimerActive) then
    fOnAniStop(Self);
end;

{**********************************************}
procedure TALPageController.SetOrientation(const AValue: TOrientation);
begin
  if FOrientation <> AValue then begin
    FOrientation := AValue;
    if FOrientation = Torientation.Horizontal then ScrollEngine.TouchTracking := [ttHorizontal]
    else ScrollEngine.TouchTracking := [ttVertical];
    DoRealign;
  end;
end;

{********************************************************************}
procedure TALPageController.SetViewportFraction(const AValue: Single);
begin
  If not sameValue(fViewportFraction, AValue, Tepsilon.Scale) then begin
    fViewportFraction := AValue;
    Dorealign;
  end;
end;

{***********************************************************}
function TALPageController.IsViewportFractionStored: Boolean;
begin
  result := not sameValue(fViewportFraction, 1, Tepsilon.Scale);
end;

{****************************}
procedure TALPageController.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
end;

{*******************************}
procedure TALPageController.DoActivePageChanged;
begin
  if Assigned(FOnActivePageChanged) then
    FOnActivePageChanged(Self);
  If FPageIndicator <> nil then
    FPageIndicator.ActivePageChanged(FActivePageIndex);
end;

{*******************************}
procedure TALPageController.DoRealign;
begin
  if CSLoading in componentState then exit;
  if fDisableAlign then exit;
  fDisableAlign := True;
  try

    If orientation = Torientation.Horizontal then begin
      var LPageSize: Single := GetPageSize;
      var LCurrX: Single := 0;
      For var I := 0 to Content.ControlsCount - 1 do begin
        var LPageView := Content.Controls[i];
        LPageView.SetBounds(LCurrX{X}, 0{Y}, LPageSize{AWidth}, Height{AHeight});
        LCurrX := LCurrX + LPageSize;
      end;
      Content.SetBounds(Content.Position.X{X}, Content.Position.Y{Y}, LCurrX{AWidth}, Height{AHeight});
      ScrollEngine.SetScrollLimits(
        TALPointD.Create(-GetEndPadding,0),
        TALPointD.Create(Content.Width - Width + GetEndPadding, 0),
        false{EnforceLimits});
    end
    else begin
      var LPageSize: Single := GetPageSize;
      var LCurrY: Single := 0;
      For var I := 0 to Content.ControlsCount - 1 do begin
        var LPageView := Content.Controls[i];
        LPageView.SetBounds(0{X}, LCurrY{Y}, Width{AWidth}, LPageSize{AHeight});
        LCurrY := LCurrY + LPageSize;
      end;
      Content.SetBounds(Content.Position.X{X}, Content.Position.Y{Y}, Width{AWidth}, LCurrY{AHeight});
      ScrollEngine.SetScrollLimits(
        TALPointD.Create(0,-GetEndPadding),
        TALPointD.Create(0, Content.Height - Height + GetEndPadding),
        false{EnforceLimits});
    end;

    if InRange(ActivePageIndex, 0, PageCount - 1) then
      SetActivePageIndex(ActivePageIndex)
    else begin
      FActivePageIndex := -1;
      if pageCount > 0 then SetActivePageIndex(0)
    end;

  finally
    fDisableAlign := false;
  end;
end;

{*********************************************************************************************}
procedure TALPageController.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'ScrollEngine.down: ' + ALBoolToStrW(fScrollEngine.down));
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    {$IFDEF DEBUG}
    if fScrollCapturedByMe then
      raise Exception.Create('Error 9A590A21-139E-44EC-83F5-6813453329E4');
    {$ENDIF}
    if fScrollEngine.down then begin
      fScrollEngine.Down := false;
      FHandleMouseEvents := False;
    end;
  end;
end;

{************************************************************************************************}
procedure TALPageController.internalMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.MouseDown',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if (Button = TMouseButton.mbLeft) then begin
    FHandleMouseEvents := true;
    fMouseDownPos := TPointF.Create(X,Y);
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseDown(form.Handle);
    {$ELSE}
    ScrollEngine.MouseDown(X, Y);
    {$ENDIF}
  end;
end;

{**************************************************************************}
procedure TALPageController.internalMouseMove(Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.internalMouseMove',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if FHandleMouseEvents then begin
    if (not fScrollCapturedByMe) and
       (fScrollEngine.TouchEnabled) and
       (((ttHorizontal in fScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.x - x) > abs(fMouseDownPos.y - y)) and
         (abs(fMouseDownPos.x - x) > TALScrollEngine.DefaultTouchSlop)) or
        ((ttVertical in fScrollEngine.TouchTracking) and
         (abs(fMouseDownPos.y - y) > abs(fMouseDownPos.x - x)) and
         (abs(fMouseDownPos.y - y) > TALScrollEngine.DefaultTouchSlop))) then begin
      {$IFDEF DEBUG}
      //ALLog(
      //  ClassName + '.internalMouseMove',
      //  'ScrollCapturedByMe');
      {$ENDIF}
      fScrollCapturedByMe := True;
      TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(true));
    end;
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseMove(form.Handle);
    {$ELSE}
    ScrollEngine.MouseMove(X, Y);
    {$ENDIF}
  end;
end;

{**********************************************************************************************}
procedure TALPageController.internalMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  {$IFDEF DEBUG}
  //ALLog(
  //  ClassName + '.internalMouseUp',
  //  'Position:' + ALFormatFloatW('0.##', x, ALDefaultFormatSettingsW) + ',' + ALFormatFloatW('0.##', y, ALDefaultFormatSettingsW));
  {$ENDIF}
  if FHandleMouseEvents and (Button = TMouseButton.mbLeft) then begin
    {$IF defined(ANDROID) or defined(IOS)}
    if form <> nil then
      ScrollEngine.MouseUp(form.Handle);
    {$ELSE}
    ScrollEngine.MouseUp(X, Y);
    {$ENDIF}
    FScrollCapturedByMe := False;
    FHandleMouseEvents := False;
  end;
end;

{*****************************************}
procedure TALPageController.internalMouseLeave;
begin
  {$IFDEF DEBUG}
  //ALLog(ClassName + '.internalMouseLeave');
  {$ENDIF}
  if FHandleMouseEvents then begin
    ScrollEngine.MouseLeave;
    FScrollCapturedByMe := False;
    FHandleMouseEvents := False;
  end;
end;

{****************************************************************************************}
procedure TALPageController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  internalMouseDown(Button, Shift, X, Y);
end;

{******************************************************************}
procedure TALPageController.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  // Inherited at the end because of
  // https://github.com/MagicFoundation/Alcinoe/issues/381
  internalMouseMove(Shift, X, Y);
  inherited;
end;

{**************************************************************************************}
procedure TALPageController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  internalMouseUp(Button, Shift, X, Y);
end;

{***********************************}
procedure TALPageController.DoMouseLeave;
begin
  inherited;
  internalMouseLeave;
end;

{**}
Type
  _TControlAccessProtected = class(Tcontrol);

{*************}
{$IFNDEF ALDPK}
procedure TALPageController.ChildrenMouseDown(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not aObject.AutoCapture then begin
    {$IF defined(MSWindows)}
    // On Windows, calling doCapture will invoke Winapi.Windows.SetCapture(FormToHWND(AForm));
    // This action deactivates some functionalities in the native control, such as the right-click menu.
    if not Supports(aObject, IALNativeControl) then
    {$ENDIF}
      _TControlAccessProtected(aObject).capture;
  end;
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseDown(Button, Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF}

{*************}
{$IFNDEF ALDPK}
procedure TALPageController.ChildrenMouseMove(const AObject: TControl; Shift: TShiftState; X, Y: Single);
begin
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  internalMouseMove(Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF}

{*************}
{$IFNDEF ALDPK}
procedure TALPageController.ChildrenMouseUp(const AObject: TControl; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if not aObject.AutoCapture then begin
    {$IF defined(MSWindows)}
    // On Windows, calling doCapture will invoke Winapi.Windows.SetCapture(FormToHWND(AForm));
    // This action deactivates some functionalities in the native control, such as the right-click menu.
    if not Supports(aObject, IALNativeControl) then
    {$ENDIF}
      _TControlAccessProtected(aObject).releasecapture;
  end;
  var P := AbsoluteToLocal(AObject.LocalToAbsolute(TpointF.Create(X, Y)));
  InternalMouseUp(Button, Shift, P.X, P.Y);
  inherited;
end;
{$ENDIF}

{*************}
{$IFNDEF ALDPK}
procedure TALPageController.ChildrenMouseLeave(const AObject: TControl);
begin
  internalMouseLeave;
  inherited;
end;
{$ENDIF}

{*********************************************************************}
function TALPageController.NextPage(ATransition: TPageTransition): Boolean;
begin
  Result := (PageCount > 0) and (ActivePageIndex < PageCount - 1);
  if Result then SetActivePageIndex(ActivePageIndex + 1, ATransition);
end;

{**********************************************************************}
function TALPageController.PreviousPage(ATransition: TPageTransition): Boolean;
begin
  Result := (PageCount > 0) and (ActivePageIndex > 0);
  if Result then SetActivePageIndex(ActivePageIndex - 1, ATransition);
end;

{*******************************************************************}
function TALPageController.FirstPage(ATransition: TPageTransition): Boolean;
begin
  Result := (PageCount > 0) and (ActivePageIndex > 0);
  if Result then SetActivePageIndex(0, ATransition);
end;

{******************************************************************}
function TALPageController.LastPage(ATransition: TPageTransition): Boolean;
begin
  Result := (PageCount > 0) and (ActivePageIndex < PageCount - 1);
  if Result then SetActivePageIndex(PageCount - 1, ATransition);
end;

{**********************************************************************}
function TALPageController.AddPage(const APageViewClass: TALPageViewClass): TALPageView;
begin
  Result := InsertPage(MaxInt, APageViewClass);
end;

{*****************************************************************************************************}
function TALPageController.InsertPage(const AIndex: Integer; const APageViewClass: TALPageViewClass = nil): TALPageView;
begin
  var LIndex := EnsureRange(AIndex, 0, PageCount);
  var LPageViewClass: TALPageViewClass;
  if APageViewClass = nil then LPageViewClass := TALPageView
  else LPageViewClass := APageViewClass;
  Result := LPageViewClass.Create(Self);
  try
    var LActivePageIndex := ActivePageIndex;
    if LActivePageIndex >= LIndex then Inc(LActivePageIndex);
    FContent.InsertObject(Lindex, Result);
    ActivePageIndex := LActivePageIndex;
  except
    ALFreeAndNil(Result);
    Raise;
  end;
end;

{***********************************************************}
procedure TALPageController.DeletePage(const AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex <= PageCount - 1) then begin
    var LActivePageIndex := ActivePageIndex;
    if LActivePageIndex >= AIndex then Dec(LActivePageIndex);
    var LPage := Pages[AIndex];
    FContent.RemoveObject(AIndex);
    ALFreeAndNil(LPage);
    if LActivePageIndex >= 0 then ActivePageIndex := LActivePageIndex;
  end;
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALPageController, TALPageIndicator]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALPageController, 'Size');
  UnlistPublishedProperty(TALPageController, 'StyleName');
  UnlistPublishedProperty(TALPageController, 'OnTap');
  UnlistPublishedProperty(TALPageView, 'Size');
  UnlistPublishedProperty(TALPageView, 'StyleName');
  UnlistPublishedProperty(TALPageView, 'OnTap');
  UnlistPublishedProperty(TALPageIndicator, 'Size');
  UnlistPublishedProperty(TALPageIndicator, 'StyleName');
  UnlistPublishedProperty(TALPageIndicator, 'OnTap');
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALPageController, TALPageView]);

end.
