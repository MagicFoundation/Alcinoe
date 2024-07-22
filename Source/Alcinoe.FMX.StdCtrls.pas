unit Alcinoe.FMX.StdCtrls;

interface

{$I Alcinoe.inc}

{$IFNDEF ALCompilerVersionSupported120}
  {$MESSAGE WARN 'Check if FMX.StdCtrls.pas was not updated and adjust the IFDEF'}
{$ENDIF}

uses
  System.Classes,
  System.Types,
  {$IFDEF DEBUG}
  System.Diagnostics,
  {$ENDIF}
  System.UITypes,
  System.ImageList,
  System.Math,
  System.Rtti,
  System.Messaging,
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  FMX.types3D,
  {$ENDIF}
  FMX.types,
  FMX.stdActns,
  FMX.Controls,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.actnlist,
  FMX.ImgList,
  Alcinoe.FMX.Ani,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.ScrollEngine,
  Alcinoe.FMX.Controls,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Objects;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALAniIndicator = class(TALControl, IALDoubleBufferedControl)
  public const
    DefaultEnabled = False;
  private
    fTimer: TTimer;
    finterval: integer;
    FFrameCount: Integer;
    FRowCount: Integer;
    fResourceName: String;
    fFrameIndex: TSmallPoint;
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    procedure setResourceName(const Value: String);
    procedure onTimer(sender: Tobject);
    function ResourceNameStored: Boolean;
  protected
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
    function EnabledStored: Boolean; override;
    procedure SetEnabled(const Value: Boolean); override;
    function GetDefaultSize: TSizeF; override;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
  published
    //property Action;
    property Align;
    property Anchors;
    //property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    //property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled stored EnabledStored default DefaultEnabled;
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
    property ResourceName: String read fResourceName write setResourceName stored ResourceNameStored nodefault;
    property FrameCount: Integer read FFrameCount write FFrameCount default 20;
    property RowCount: Integer read FRowCount write FRowCount default 4;
    property Interval: integer read finterval write finterval default 50;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    //property TabOrder;
    //property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
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

  {~~~~~~~~~~~~~~~~~~~~~}
  TALCustomTrack = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  (* todo move it inside TALCustomTrack *)
  TALTrackThumbGlyph = class(TALImage)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align default TalignLayout.Client;
    property HitTest default false;
    property Locked default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackThumb = class(TALBaseRectangle)
  private
    fValueRange: TValueRange;
    FTrack: TALCustomTrack;
    FGlyph: TALTrackThumbGlyph;
    fMouseDownPos: TPointF;
    fTrackMouseDownPos: TPointF;
    FPressed: Boolean;
    fScrollCapturedByMe: boolean;
    procedure ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
    function PointToValue(X, Y: Single): Double;
  public
    constructor Create(const ATrack: TALCustomTrack; const aValueRange: TValueRange; const aWithGlyphObj: boolean); reintroduce;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    function GetDefaultTouchTargetExpansion: TRectF; override;
    property Pressed: Boolean read FPressed;
  published
    property Cursor default crHandPoint;
    property Glyph: TALTrackThumbGlyph read FGlyph;
    property Locked default True;
    property Position stored false;
    property Size stored false;
    property TouchTargetExpansion;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackBackground = class(TALBaseRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HitTest default false;
    property Locked default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackHighlight = class(TALBaseRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HitTest default false;
    property Locked default True;
    property Position stored false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCustomTrack = class(TALControl, IValueRange)
  private
    FValueRange: TValueRange;
    FDefaultValueRange: TBaseValueRange;
    function GetIsTracking: Boolean;
    function GetValueRange: TCustomValueRange;
    procedure SetValueRange(const AValue: TCustomValueRange);
    procedure SetValueRange_(const Value: TValueRange);
    function FrequencyStored: Boolean;
    function MaxStored: Boolean;
    function MinStored: Boolean;
    procedure SetThumbSize(const Value: Single);
    function ThumbSizeStored: Boolean;
    function ViewportSizeStored: Boolean;
  protected
    FOnChange: TNotifyEvent;
    FOnTracking: TNotifyEvent;
    FIgnoreViewportSize: Boolean;
    FOrientation: TOrientation;
    FTracking: Boolean;
    FThumbSize: Single;
    FMinThumbSize: Single;
    FThumb: TALTrackThumb;
    FBackGround: TALTrackBackground;
    FHighlight: TALTrackHighlight;
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
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure SetOrientation(const Value: TOrientation); virtual;
    function GetThumbRect(const Value: single; const aThumb: TALTrackThumb): TRectF; overload; virtual;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    function GetDefaultTouchTargetExpansion: TRectF; override;
    function GetThumbSize(var IgnoreViewportSize: Boolean): Integer; virtual;
    procedure DoRealign; override;
    procedure Loaded; override;
    procedure DoChanged; virtual;
    procedure DoTracking; virtual;
    function CreateValueRangeTrack : TValueRange; virtual;
    property DefaultValueRange: TBaseValueRange read FDefaultValueRange;
    property ValueRange: TValueRange read FValueRange write SetValueRange_ stored ValueStored;
    property Value: Double read GetValue write SetValue stored ValueStored nodefault;
    property Thumb: TALTrackThumb read FThumb;
    procedure UpdateHighlight; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property IsTracking: Boolean read GetIsTracking;
    property Min: Double read GetMin write SetMin stored MinStored nodefault;
    property Max: Double read GetMax write SetMax stored MaxStored nodefault;
    property Frequency: Double read GetFrequency write SetFrequency stored FrequencyStored nodefault;
    property ViewportSize: Double read GetViewportSize write SetViewportSize stored ViewportSizeStored nodefault;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property Tracking: Boolean read FTracking write FTracking default True;
    property ThumbSize: Single read fThumbSize write SetThumbSize Stored ThumbSizeStored nodefault; // 0 mean the thumb will have the height of the track in horizontal or width of the track in vertical
    property BackGround: TALTrackBackground read FBackGround;
    property Highlight: TALTrackHighlight read FHighlight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTracking: TNotifyEvent read FOnTracking write FOnTracking;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALTrackBar = class(TALCustomTrack)
  protected
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ValueRange;
  published
    //property Action;
    property Align;
    property Anchors;
    property BackGround;
    property CanFocus default True;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Frequency;
    property Height;
    property Highlight;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Min;
    property Max;
    property Opacity;
    property Orientation;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Thumb;
    property ThumbSize; (* todo: delete *)
    property TouchTargetExpansion;
    property Tracking;
    property Value;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnChange;
    property OnTracking;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnEnter;
    property OnExit;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALScrollBar = class(TALCustomTrack)
  protected
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ValueRange;
  published
    //property Action;
    property Align;
    property Anchors;
    property BackGround;
    property CanFocus default False;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Min;
    property Max;
    property Opacity;
    property Orientation;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Thumb;
    property TouchTargetExpansion;
    property Value;
    property ViewportSize;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnChange;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnEnter;
    property OnExit;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALRangeTrackBar = class(TALCustomTrack)
  private
    FMaxValueRange: TValueRange;
  protected
    FMaxThumb: TALTrackThumb;
    procedure SetViewportSize(const Value: Double); override;
    procedure SetFrequency(const Value: Double); override;
    procedure SetMax(const Value: Double); override;
    procedure SetMin(const Value: Double); override;
    function MaxValueStored: Boolean; virtual;
    function GetDefaultSize: TSizeF; override;
    procedure SetValue(Value: Double); override;
    function GetMaxValue: Double; virtual;
    procedure SetMaxValue(Value: Double); virtual;
    procedure Loaded; override;
    procedure DoRealign; override;
    procedure UpdateHighlight; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //property Action;
    property Align;
    property Anchors;
    property BackGround;
    property CanFocus default True;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Frequency;
    property Height;
    property Highlight;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest;
    property Locked;
    property Margins;
    property Min;
    property Max;
    property MinValue: Double read GetValue write SetValue stored ValueStored nodefault;
    property MaxValue: Double read GetMaxValue write SetMaxValue stored MaxValueStored nodefault;
    property MinThumb: TALTrackThumb read FThumb;
    property MaxThumb: TALTrackThumb read FMaxThumb;
    property ThumbSize;  (* todo delete *)
    property Opacity;
    property Orientation;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Tracking;
    property Value;
    property Visible;
    property Width;
    property OnCanFocus;
    property OnChange;
    property OnTracking;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnEnter;
    property OnExit;
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

  {~~~~~~~~~~~~~~~~~~}
  TALCheckBox = Class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  (* todo move it inside TALCheckBox *)
  TALCheckMarkBrush = class(TALPersistentObserver)
  private
    FColor: TAlphaColor;
    FResourceName: String;
    FWrapMode: TALImageWrapMode;
    FThickness: Single;
    FPadding: TBounds;
    FDefaultColor: TAlphaColor;
    FDefaultResourceName: String;
    FDefaultWrapMode: TALImageWrapMode;
    FDefaultThickness: Single;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetResourceName(const Value: String);
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure SetThickness(const Value: Single);
    procedure SetPadding(const Value: TBounds);
    procedure PaddingChanged(Sender: TObject); virtual;
    function IsColorStored: Boolean;
    function IsResourceNameStored: Boolean;
    function IsWrapModeStored: Boolean;
    function IsThicknessStored: Boolean;
  protected
    function CreateSavedState: TALPersistentObserver; override;
  public
    constructor Create(const ADefaultColor: TAlphaColor); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALCheckMarkBrush; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALCheckMarkBrush; const ANormalizedTime: Single);
    function HasCheckMark: boolean;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
    property DefaultResourceName: String read FDefaultResourceName write FDefaultResourceName;
    property DefaultWrapMode: TALImageWrapMode read FDefaultWrapMode write FDefaultWrapMode;
    property DefaultThickness: Single read FDefaultThickness write FDefaultThickness;
  published
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
    property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored nodefault;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
    property Thickness: Single read FThickness write SetThickness stored IsThicknessStored nodefault;
    property Padding: TBounds read FPadding write SetPadding;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritCheckMarkBrush = class(TALCheckMarkBrush)
  private
    FParent: TALCheckMarkBrush;
    FInherit: Boolean;
    fSuperseded: Boolean;
    procedure SetInherit(const AValue: Boolean);
  protected
    function CreateSavedState: TALPersistentObserver; override;
    procedure DoSupersede; virtual;
  public
    constructor Create(const AParent: TALCheckMarkBrush; const ADefaultColor: TAlphaColor); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede(Const ASaveState: Boolean = False); virtual;
    procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
    property Superseded: Boolean read FSuperseded;
    property Parent: TALCheckMarkBrush read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBoxBaseStateStyle = class(TALBaseStateStyle)
  private
    FCheckMark: TALInheritCheckMarkBrush;
    function GetStateStyleParent: TALCheckBoxBaseStateStyle;
    function GetControlParent: TALCheckBox;
    procedure SetCheckMark(const AValue: TALInheritCheckMarkBrush);
    procedure CheckMarkChanged(ASender: TObject);
  protected
    function GetInherit: Boolean; override;
    procedure DoSupersede; override;
  public
    constructor Create(const AParent: TObject); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALCheckBoxBaseStateStyle; const ANormalizedTime: Single); reintroduce; virtual;
    procedure InterpolateNoChanges(const ATo: TALCheckBoxBaseStateStyle; const ANormalizedTime: Single); reintroduce;
    property StateStyleParent: TALCheckBoxBaseStateStyle read GetStateStyleParent;
    property ControlParent: TALCheckBox read GetControlParent;
  published
    property CheckMark: TALInheritCheckMarkBrush read FCheckMark write SetCheckMark;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBoxDefaultStateStyle = class(TALCheckBoxBaseStateStyle)
  published
    property Fill;
    property Shadow;
    property Stroke;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBoxDisabledStateStyle = class(TALCheckBoxBaseStateStyle)
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
  published
    property Fill;
    property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
    property Shadow;
    property Stroke;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBoxHoveredStateStyle = class(TALCheckBoxBaseStateStyle)
  published
    property Fill;
    property Shadow;
    property StateLayer;
    property Stroke;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBoxPressedStateStyle = class(TALCheckBoxBaseStateStyle)
  published
    property Fill;
    property Shadow;
    property StateLayer;
    property Stroke;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBoxFocusedStateStyle = class(TALCheckBoxBaseStateStyle)
  published
    property Fill;
    property Shadow;
    property StateLayer;
    property Stroke;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBoxCheckStateStyles = class(TALPersistentObserver)
  private
    FDefault: TALCheckBoxDefaultStateStyle;
    FDisabled: TALCheckBoxDisabledStateStyle;
    FHovered: TALCheckBoxHoveredStateStyle;
    FPressed: TALCheckBoxPressedStateStyle;
    FFocused: TALCheckBoxFocusedStateStyle;
    procedure SetDefault(const AValue: TALCheckBoxDefaultStateStyle);
    procedure SetDisabled(const AValue: TALCheckBoxDisabledStateStyle);
    procedure SetHovered(const AValue: TALCheckBoxHoveredStateStyle);
    procedure SetPressed(const AValue: TALCheckBoxPressedStateStyle);
    procedure SetFocused(const AValue: TALCheckBoxFocusedStateStyle);
    procedure DefaultChanged(ASender: TObject);
    procedure DisabledChanged(ASender: TObject);
    procedure HoveredChanged(ASender: TObject);
    procedure PressedChanged(ASender: TObject);
    procedure FocusedChanged(ASender: TObject);
  protected
    function CreateSavedState: TALPersistentObserver; override;
  public
    constructor Create(const AParent: TALCheckBox); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property &Default: TALCheckBoxDefaultStateStyle read FDefault write SetDefault;
    property Disabled: TALCheckBoxDisabledStateStyle read FDisabled write SetDisabled;
    property Hovered: TALCheckBoxHoveredStateStyle read FHovered write SetHovered;
    property Pressed: TALCheckBoxPressedStateStyle read FPressed write SetPressed;
    property Focused: TALCheckBoxFocusedStateStyle read FFocused write SetFocused;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBoxStateStyles = class(TALPersistentObserver)
  private
    FChecked: TALCheckBoxCheckStateStyles;
    FUnchecked: TALCheckBoxCheckStateStyles;
    procedure SetChecked(const AValue: TALCheckBoxCheckStateStyles);
    procedure SetUnchecked(const AValue: TALCheckBoxCheckStateStyles);
    procedure CheckedChanged(ASender: TObject);
    procedure UncheckedChanged(ASender: TObject);
  protected
    function CreateSavedState: TALPersistentObserver; override;
  public
    constructor Create(const AParent: TALCheckBox); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Checked: TALCheckBoxCheckStateStyles read FChecked write SetChecked;
    property Unchecked: TALCheckBoxCheckStateStyles read FUnchecked write SetUnchecked;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCheckBox = class(TALShape, IALDoubleBufferedControl)
  private
    FOnChange: TNotifyEvent;
    FDoubleBuffered: boolean;
    FXRadius: Single;
    FYRadius: Single;
    FChecked: Boolean;
    FCheckMark: TALCheckMarkBrush;
    FStateStyles: TALCheckBoxStateStyles;
    fBufCheckedDrawable: TALDrawable;
    fBufCheckedDrawableRect: TRectF;
    fBufCheckedDisabledDrawable: TALDrawable;
    fBufCheckedDisabledDrawableRect: TRectF;
    fBufCheckedHoveredDrawable: TALDrawable;
    fBufCheckedHoveredDrawableRect: TRectF;
    fBufCheckedPressedDrawable: TALDrawable;
    fBufCheckedPressedDrawableRect: TRectF;
    fBufCheckedFocusedDrawable: TALDrawable;
    fBufCheckedFocusedDrawableRect: TRectF;
    fBufUnCheckedDrawable: TALDrawable;
    fBufUnCheckedDrawableRect: TRectF;
    fBufUnCheckedDisabledDrawable: TALDrawable;
    fBufUnCheckedDisabledDrawableRect: TRectF;
    fBufUnCheckedHoveredDrawable: TALDrawable;
    fBufUnCheckedHoveredDrawableRect: TRectF;
    fBufUnCheckedPressedDrawable: TALDrawable;
    fBufUnCheckedPressedDrawableRect: TRectF;
    fBufUnCheckedFocusedDrawable: TALDrawable;
    fBufUnCheckedFocusedDrawableRect: TRectF;
    procedure SetCheckMark(const Value: TALCheckMarkBrush);
    procedure SetStateStyles(const AValue: TALCheckBoxStateStyles);
  protected
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure CheckMarkChanged(Sender: TObject); virtual;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure FillChanged(Sender: TObject); override;
    procedure StrokeChanged(Sender: TObject); override;
    procedure ShadowChanged(Sender: TObject); override;
    procedure IsMouseOverChanged; override;
    procedure PressedChanged; override;
    function GetDefaultSize: TSizeF; override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    procedure Click; override;
    procedure DoChanged; virtual;
    procedure DoResized; override;
    procedure DrawCheckMark(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const ADstRect: TrectF;
            const ACheckMark: TALCheckMarkBrush;
            const AChecked: Boolean); virtual;
    Procedure CreateBufDrawable(
                var ABufDrawable: TALDrawable;
                var ABufDrawableRect: TRectF;
                const AFill: TALBrush;
                const AStateLayer: TALBrush;
                const AStroke: TALStrokeBrush;
                const ACheckMark: TALCheckMarkBrush;
                const AShadow: TALShadow); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
  published
    //property Action;
    property Align;
    property Anchors;
    property CanFocus default True;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property CheckMark: TALCheckMarkBrush read FCheckMark write SetCheckMark;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property ClipChildren;
    //property ClipParent;
    property Cursor default crHandPoint;
    property DoubleBuffered: Boolean read GetDoubleBuffered write SetDoubleBuffered default true;
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
    property RotationCenter;
    property Scale;
    property Shadow;
    property Size;
    property StateStyles: TALCheckBoxStateStyles read FStateStyles write SetStateStyles;
    property Stroke;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property OnCanFocus;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnEnter;
    property OnExit;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALRadioButton = class(TALCheckBox)
  private
    FGroupName: string;
    fMandatory: boolean;
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GroupNameStored: Boolean;
    procedure GroupMessageCall(const Sender : TObject; const M : TMessage);
  protected
    procedure SetChecked(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GroupName: string read GetGroupName write SetGroupName stored GroupNameStored nodefault;
    property Mandatory: Boolean read fMandatory write fMandatory default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  (* todo move it inside TALSwitch *)
  TALSwitchThumb = class(TALBaseRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HitTest default false;
    property Locked default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSwitchBackground = class(TALBaseRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property HitTest default false;
    property Locked default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALSwitch = class(TALControl)
  public const
    DefaultSwitchAnimationDuration = 0.2;
    TrackingSensitivity = 3;
  private
    FThumb: TALSwitchThumb;
    FBackGround: TALSwitchBackground;
    FThumbRect: TrectF;
    FPressed, FTracking: Boolean;
    FPressedThumbPos, FSavedPos: TPointF;
    FAnimation: TALFloatAnimation;
    FChecked: Boolean;
    FOnChange: TNotifyEvent;
    FOnAnimationProcess: TNotifyEvent;
    FOnAnimationFinish: TNotifyEvent;
    FThumbSize: Single;
    fAnimationDuration: Single;
    procedure doAnimationProcess(Sender: TObject);
    procedure DoAnimationEnd(Sender: TObject);
    procedure SetThumbSize(const Value: Single);
    function AnimationDurationStored: Boolean;
  protected
    function GetDefaultSize: TSizeF; override;
    procedure AnimateTo(const Value: Boolean);
    function GetThumbCenter: Single;
    function GetThumbSize: Single;
    function GetValueByMousePos(const X, Y: Single): Boolean;
    function GetThumbRectByValue(const Value: Boolean): TRectF; virtual;
    procedure DoChange;
    procedure Resize; override;
    procedure DoRealign; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    property Pressed: Boolean read FPressed;
    procedure SetChecked(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetThumbValue: Single;
    procedure SetCheckedWithAnimation(const Value: Boolean);
    property Tracking: boolean read FTracking write FTracking default false;
    property Animation: TALFloatAnimation read FAnimation;
  published
    //property Action;
    property Align;
    property AnimationDuration: single read fAnimationDuration write fAnimationDuration stored AnimationDurationStored nodefault;
    property Anchors;
    property BackGround: TALSwitchBackGround read FBackGround;
    property CanFocus;
    //property CanParentFocus;
    //property DisableFocusEffect;
    property Checked: Boolean read FChecked write SetChecked default false;
    property ClipChildren;
    //property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Height;
    //property Hint;
    //property ParentShowHint;
    //property ShowHint;
    property HitTest default False;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property Thumb: TALSwitchThumb read FThumb;
    property ThumbSize: Single read FThumbSize write SetThumbSize; (* todo delete *)
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property OnAnimationProcess: TNotifyEvent read FOnAnimationProcess write FOnAnimationProcess;
    property OnAnimationFinish: TNotifyEvent read FOnAnimationFinish write FOnAnimationFinish;
    property OnCanFocus;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnEnter;
    property OnExit;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALButton = class(TALBaseText)
  public
    type
      // -----------------------
      // TStateStyleTextSettings
      TStateStyleTextSettings = class(TALInheritBaseTextSettings)
      published
        property Font;
        property Decoration;
      end;
      // ---------------
      // TBaseStateStyle
      TBaseStateStyle = class(TALBaseStateStyle)
      private
        FText: String;
        FTextSettings: TStateStyleTextSettings;
        FDefaultText: String;
        FPriorSupersedeText: String;
        function GetStateStyleParent: TBaseStateStyle;
        function GetControlParent: TALButton;
        procedure SetText(const Value: string);
        procedure SetTextSettings(const AValue: TStateStyleTextSettings);
        procedure TextSettingsChanged(ASender: TObject);
        function IsTextStored: Boolean;
      protected
        function GetInherit: Boolean; override;
        procedure DoSupersede; override;
      public
        constructor Create(const AParent: TObject); override;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
        procedure Interpolate(const ATo: TBaseStateStyle; const ANormalizedTime: Single); reintroduce; virtual;
        procedure InterpolateNoChanges(const ATo: TBaseStateStyle; const ANormalizedTime: Single); reintroduce;
        property StateStyleParent: TBaseStateStyle read GetStateStyleParent;
        property ControlParent: TALButton read GetControlParent;
        property DefaultText: String read FDefaultText write FDefaultText;
      published
        property Text: string read FText write SetText stored IsTextStored nodefault;
        property TextSettings: TStateStyleTextSettings read fTextSettings write SetTextSettings;
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
      published
        property Fill;
        property Scale;
        property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
        property Shadow;
        property Stroke;
      end;
      // ------------------
      // THoveredStateStyle
      THoveredStateStyle = class(TBaseStateStyle)
      published
        property Fill;
        property Scale;
        property Shadow;
        property StateLayer;
        property Stroke;
        property Transition;
      end;
      // ------------------
      // TPressedStateStyle
      TPressedStateStyle = class(TBaseStateStyle)
      published
        property Fill;
        property Scale;
        property Shadow;
        property StateLayer;
        property Stroke;
        property Transition;
      end;
      // ------------------
      // TFocusedStateStyle
      TFocusedStateStyle = class(TBaseStateStyle)
      published
        property Fill;
        property Scale;
        property Shadow;
        property StateLayer;
        property Stroke;
        property Transition;
      end;
      // ------------
      // TStateStyles
      TStateStyles = class(TALPersistentObserver)
      private
        FDisabled: TDisabledStateStyle;
        FHovered: THoveredStateStyle;
        FPressed: TPressedStateStyle;
        FFocused: TFocusedStateStyle;
        procedure SetDisabled(const AValue: TDisabledStateStyle);
        procedure SetHovered(const AValue: THoveredStateStyle);
        procedure SetPressed(const AValue: TPressedStateStyle);
        procedure SetFocused(const AValue: TFocusedStateStyle);
        procedure DisabledChanged(ASender: TObject);
        procedure HoveredChanged(ASender: TObject);
        procedure PressedChanged(ASender: TObject);
        procedure FocusedChanged(ASender: TObject);
      protected
        function CreateSavedState: TALPersistentObserver; override;
      public
        constructor Create(const AParent: TALButton); reintroduce; virtual;
        destructor Destroy; override;
        procedure Assign(Source: TPersistent); override;
        procedure Reset; override;
      published
        property Disabled: TDisabledStateStyle read FDisabled write SetDisabled;
        property Hovered: THoveredStateStyle read FHovered write SetHovered;
        property Pressed: TPressedStateStyle read FPressed write SetPressed;
        property Focused: TFocusedStateStyle read FFocused write SetFocused;
      end;
      // -------------
      // TTextSettings
      TTextSettings = class(TALBaseTextSettings)
      published
        property Font;
        property Decoration;
        property Trimming;
        property MaxLines;
        property Ellipsis;
        property HorzAlign;
        property VertAlign;
        property LineHeightMultiplier;
        property LetterSpacing;
        property IsHtml;
      end;
  private
    FStateStyles: TStateStyles;
    FStateTransitionAnimation: TALfloatAnimation;
    FStateTransitionFrom: TBaseStateStyle;
    FStateTransitionTo: TBaseStateStyle;
    FCurrentStateStyle: TBaseStateStyle;
    fBufDisabledDrawable: TALDrawable;
    fBufDisabledDrawableRect: TRectF;
    fBufHoveredDrawable: TALDrawable;
    fBufHoveredDrawableRect: TRectF;
    fBufPressedDrawable: TALDrawable;
    fBufPressedDrawableRect: TRectF;
    fBufFocusedDrawable: TALDrawable;
    fBufFocusedDrawableRect: TRectF;
    function GetTextSettings: TTextSettings;
    procedure SetStateStyles(const AValue: TStateStyles);
  protected
    function CreateTextSettings: TALBaseTextSettings; override;
    procedure SetTextSettings(const Value: TTextSettings); reintroduce;
    procedure SetName(const Value: TComponentName); override;
    function GetCurrentStateStyle: TBaseStateStyle; virtual;
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure IsMouseOverChanged; override;
    procedure PressedChanged; override;
    procedure startStateTransition; virtual;
    procedure StateTransitionAnimationProcess(Sender: TObject); virtual;
    procedure StateTransitionAnimationFinish(Sender: TObject); virtual;
    Procedure DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; var ARect: TrectF; var ASurfaceSize: TSizeF); override;
    Procedure DrawMultilineTextBeforeDrawParagraph(const ACanvas: TALCanvas; Const ARect: TrectF); override;
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure clearBufDrawable; override;
    procedure MakeBufDrawable; override;
  published
    property AutoSize default True;
    property CanFocus default true;
    property Cursor default crHandPoint;
    property DoubleBuffered;
    property HitTest default True;
    property StateStyles: TStateStyles read FStateStyles write SetStateStyles;
    property TabOrder;
    property TabStop;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  system.Math.Vectors,
  {$IF defined(ALSkiaEngine)}
  System.Skia.API,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  {$IF DEFINED(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  FMX.Canvas.GPU,
  Alcinoe.FMX.Types3D,
  {$ENDIF}
  {$IF DEFINED(ALAppleOS)}
  iOSapi.CoreGraphics,
  {$ENDIF}
  FMX.Platform,
  fmx.consts,
  fmx.utils,
  Alcinoe.StringUtils,
  Alcinoe.FMX.BreakText,
  Alcinoe.Common;

{*****************************************************}
constructor TALAniIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  finterval := 50;
  FFrameCount := 20;
  FRowCount := 4;
  fResourceName := 'aniindicator_540x432';
  fFrameIndex := TSmallPoint.Create(0,0);
  fTimer := TTimer.Create(self);
  fTimer.Enabled := False;
  fTimer.Interval := finterval;
  fTimer.OnTimer := onTimer;
  fBufDrawable := ALNullDrawable;
  Enabled := DefaultEnabled;
  SetAcceptsControls(False);
end;

{*********************************}
destructor TALAniIndicator.Destroy;
begin
  fTimer.Enabled := False;
  ALFreeAndNil(fTimer);
  clearBufDrawable;
  inherited;
end;

{**********************************************}
function TALAniIndicator.EnabledStored: Boolean;
begin
  result := Enabled;
end;

{**********************************************}
function TALAniIndicator.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(36, 36);
end;

{**********************************}
procedure TALAniIndicator.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{***************************************}
procedure TALAniIndicator.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{****************************************}
procedure TALAniIndicator.MakeBufDrawable;
begin

  if (Size.Size.IsZero) or // Do not create BufDrawable if the size is 0
     (fResourceName = '') // Do not create BufDrawable if fResourceName is empty
  then begin
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) then exit;

  fBufDrawableRect := LocalRect;
  {$IFDEF ALDPK}
  try
    var LFileName := ALGetResourceFilename(FResourceName);
    if LFileName <> '' then fBufDrawable := ALLoadFromFileAndFitIntoToDrawable(LFileName, Width * (fframeCount div fRowCount) * ALGetScreenScale, Height * fRowCount * ALGetScreenScale)
    else fBufDrawable := ALNullDrawable;
  except
    fBufDrawable := ALNullDrawable;
  end;
  {$ELSE}
  fBufDrawable := ALLoadFromResourceAndFitIntoToDrawable(fResourceName, Width * (fframeCount div fRowCount) * ALGetScreenScale, Height * fRowCount * ALGetScreenScale);
  {$ENDIF}

end;

{*************************************************}
procedure TALAniIndicator.onTimer(sender: Tobject);
begin
  inc(fFrameIndex.x);
  if fFrameIndex.x >= FFrameCount div FRowCount then begin
    fFrameIndex.x := 0;
    inc(fFrameIndex.Y);
    if fFrameIndex.Y >= FRowCount then fFrameIndex.Y := 0;
  end;
  repaint;
end;

{******************************}
procedure TALAniIndicator.Paint;
begin

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    var R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  MakeBufDrawable;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    fBufDrawable, // const ADrawable: TALDrawable;
    TRectF.Create(
      TPointF.Create(
        fFrameIndex.x * Width * ALGetScreenScale,
        fFrameIndex.Y * Height * ALGetScreenScale),
      Width * ALGetScreenScale,
      Height * ALGetScreenScale), // const ASrcRect: TrectF; // IN REAL PIXEL !
    fBufDrawableRect, // const ADestRect: TrectF; // IN virtual pixels !
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***********************************************}
function TALAniIndicator.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{**************************************************************}
procedure TALAniIndicator.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not yet supported
end;

{***************************************************}
function TALAniIndicator.ResourceNameStored: Boolean;
begin
  result := fResourceName <> 'aniindicator_540x432';
end;

{*********************************************************}
procedure TALAniIndicator.SetEnabled(const Value: Boolean);
begin
  if Enabled <> Value then begin
    inherited;
    fTimer.Enabled := Enabled;
  end;
end;

{*************************************************************}
procedure TALAniIndicator.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    clearBufDrawable;
    FResourceName := Value;
    Repaint;
  end;
end;

{***************************************************************************************************************************************}
function _ValueToPos(MinValue, MaxValue, ViewportSize: Double; ThumbSize, TrackSize, Value: Single; IgnoreViewportSize: Boolean): Single;
var ValRel: Double;
begin
  Result := ThumbSize / 2;
  if (ViewportSize < 0) or IgnoreViewportSize then ViewportSize := 0;
  ValRel := MaxValue - MinValue - ViewportSize;
  if ValRel > 0 then begin
    ValRel := (Value - MinValue) / ValRel;
    Result := (TrackSize - ThumbSize) * ValRel + Result;
  end;
end;

{*************************************************************************************************************************************}
function _PosToValue(MinValue, MaxValue, ViewportSize: Double; ThumbSize, TrackSize, Pos: Single; IgnoreViewportSize: Boolean): Double;
var ValRel: Double;
begin
  Result := MinValue;
  if (ViewportSize < 0) or IgnoreViewportSize then ViewportSize := 0;
  ValRel := TrackSize - ThumbSize;
  if ValRel > 0 then begin
    ValRel := (Pos - ThumbSize / 2) / ValRel;
    if ValRel < 0 then ValRel := 0;
    if ValRel > 1 then ValRel := 1;
    Result := MinValue + ValRel * (MaxValue - MinValue - ViewportSize);
  end;
end;

{********************************************************}
constructor TALTrackThumbGlyph.Create(AOwner: TComponent);
begin
  inherited;
  Align := TalignLayout.Client;
  locked := True;
  HitTest := False;
end;

{***************************************************************************************************************************}
constructor TALTrackThumb.Create(const ATrack: TALCustomTrack; const aValueRange: TValueRange; const aWithGlyphObj: boolean);
begin
  inherited create(ATrack);
  cursor := crHandPoint;
  FPressed := False;
  FTrack := ATrack;
  FValueRange := aValueRange;
  CanFocus := False;
  CanParentFocus := True;
  AutoCapture := True;
  Locked := True;
  if aWithGlyphObj then begin
    fGlyph := TALTrackThumbGlyph.Create(self);
    fGlyph.Parent := self;
    fGlyph.Stored := False;
    fGlyph.SetSubComponent(True);
    fGlyph.Name := 'Glyph';
  end
  else fGlyph := nil;
  fMouseDownPos := TpointF.Zero;
  fTrackMouseDownPos := TpointF.Zero;
  fScrollCapturedByMe := False;
  TMessageManager.DefaultManager.SubscribeToMessage(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
end;

{*******************************}
destructor TALTrackThumb.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TALScrollCapturedMessage, ScrollCapturedByOtherHandler);
  inherited;
end;

{********************************************************}
function TALTrackThumb.PointToValue(X, Y: Single): Double;
var P: TPointF;
begin
  Result := 0;
  if (Parent is TControl) then begin
    if FTrack.Orientation = TOrientation.Horizontal then begin
      P := FTrack.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(X, 0)));
      P.X := P.X - fMouseDownPos.X + Width / 2;
      Result := _PosToValue(FTrack.Min, FTrack.Max, FTrack.ViewportSize, Self.Width, FTrack.Width, P.X, FTrack.FIgnoreViewportSize);
    end
    else begin
      P := FTrack.AbsoluteToLocal(LocalToAbsolute(TPointF.Create(0, Y)));
      P.Y := P.Y - fMouseDownPos.Y + Height / 2;
      Result := _PosToValue(FTrack.Min, FTrack.Max, FTrack.ViewportSize, Self.Height, FTrack.Height, P.Y, FTrack.FIgnoreViewportSize);
    end;
  end;
end;

{************************************************************}
function TALTrackThumb.GetDefaultTouchTargetExpansion: TRectF;
var DeviceSrv: IFMXDeviceService;
begin
  if SupportsPlatformService(IFMXDeviceService, DeviceSrv) and
    (TDeviceFeature.HasTouchScreen in DeviceSrv.GetFeatures) then
    Result := TRectF.Create(
                DefaultTouchTargetExpansion,
                DefaultTouchTargetExpansion,
                DefaultTouchTargetExpansion,
                DefaultTouchTargetExpansion)
  else
    Result := inherited ;
end;

{*********************************************************************************************}
procedure TALTrackThumb.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  if (Sender = self) then exit;
  {$IFDEF DEBUG}
  //ALLog(
  //  'TALTrackThumb.ScrollCapturedByOtherHandler',
  //  'Captured: ' + ALBoolToStrW(TALScrollCapturedMessage(M).Captured)+ ' | ' +
  //  'Pressed: ' + ALBoolToStrW(FPressed),
  //  TalLogType.verbose);
  {$ENDIF}
  if TALScrollCapturedMessage(M).Captured then begin
    if FPressed then begin
      FPressed := False;
      if (not FValueRange.Tracking) then FValueRange.Tracking := True;
    end;
  end;
end;

{****************************************************************************************}
procedure TALTrackThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (Button = TMouseButton.mbLeft) and Enabled then begin
    BringToFront;
    repaint;
    FPressed := True;
    fMouseDownPos := PointF(X, Y);
    fTrackMouseDownPos := FTrack.AbsoluteToLocal(LocalToAbsolute(fMouseDownPos));
    FTrack.SetFocus;
    fValueRange.Tracking := FTrack.Tracking;
    StartTriggerAnimation(Self, 'Pressed');
    ApplyTriggerEffect(Self, 'Pressed');
  end;
end;

{******************************************************************}
procedure TALTrackThumb.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FPressed and Enabled then begin

    if (not fScrollCapturedByMe) then begin
      var LTrackMousePos := FTrack.AbsoluteToLocal(LocalToAbsolute(TpointF.Create(X,Y)));
      If (((Ftrack.Orientation = TOrientation.Horizontal) and
           (abs(FTrackMouseDownPos.x - LTrackMousePos.x) > abs(FTrackMouseDownPos.y - LTrackMousePos.y)) and
           (abs(FTrackMouseDownPos.x - LTrackMousePos.x) > TALScrollEngine.DefaultTouchSlop)) or
          ((Ftrack.Orientation = TOrientation.Vertical) and
           (abs(FTrackMouseDownPos.y - LTrackMousePos.y) > abs(FTrackMouseDownPos.x - LTrackMousePos.x)) and
           (abs(FTrackMouseDownPos.y - LTrackMousePos.y) > TALScrollEngine.DefaultTouchSlop))) then begin
        fMouseDownPos := PointF(X, Y);
        fTrackMouseDownPos := LTrackMousePos;
        fScrollCapturedByMe := true;
        TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(true), True);
      end;
    end;

    if fScrollCapturedByMe then begin
      try
        FValueRange.Value := PointToValue(X, Y);
      except
        FPressed := False;
        raise;
      end;
    end;

  end;
end;

{**************************************************************************************}
procedure TALTrackThumb.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var LValue: Single;
begin
  LValue := PointToValue(X, Y);
  inherited;
  if FPressed then begin

    FScrollCapturedByMe := False;

    FPressed := False;
    try
      if (not FValueRange.Tracking) then begin
        FValueRange.Value := LValue;
        FValueRange.Tracking := True;
      end;
    finally
      StartTriggerAnimation(Self, 'Pressed');
      ApplyTriggerEffect(Self, 'Pressed');
    end;

  end;
end;

{***********************************}
procedure TALTrackThumb.DoMouseLeave;
begin
  inherited;
  if FPressed then begin

    FScrollCapturedByMe := False;

    FPressed := False;
    try
      if (not FValueRange.Tracking) then begin
        FValueRange.Tracking := True;
      end;
    finally
      StartTriggerAnimation(Self, 'Pressed');
      ApplyTriggerEffect(Self, 'Pressed');
    end;

  end;
end;

{********************************************************}
constructor TALTrackBackground.Create(AOwner: TComponent);
begin
  inherited;
  Locked := True;
  HitTest := False;
end;

{*******************************************************}
constructor TALTrackHighlight.Create(AOwner: TComponent);
begin
  inherited;
  Locked := True;
  HitTest := False;
end;

type

  {**************************************}
  TALValueRangeTrack = class (TValueRange)
  private
    FTrack: TALCustomTrack;
    FValueChanged: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoBeforeChange; override;
    procedure DoChanged; override;
    procedure DoAfterChange; override;
    property Track: TALCustomTrack read FTrack;
  end;

{********************************************************}
constructor TALValueRangeTrack.Create(AOwner: TComponent);
begin
  ValidateInheritance(AOwner, TALCustomTrack, false{CanBeNil});
  inherited;
  FTrack := TALCustomTrack(AOwner);
end;

{******************************************}
procedure TALValueRangeTrack.DoBeforeChange;
begin
  FValueChanged := (not SameValue(Value, New.Value));
  inherited;
end;

{*************************************}
procedure TALValueRangeTrack.DoChanged;
begin
  FTrack.Realign;
  FTrack.DoTracking;
  inherited;
end;

{*****************************************}
procedure TALValueRangeTrack.DoAfterChange;
begin
  if FValueChanged then
  try
    FTrack.DoChanged;
  finally
    FValueChanged := False;
  end;
  inherited;
end;

{****************************************************}
constructor TALCustomTrack.Create(AOwner: TComponent);
begin
  inherited;
  //-----
  FValueRange := CreateValueRangeTrack;
  FDefaultValueRange := TBaseValueRange.Create;
  FOrientation := TOrientation.Horizontal;
  FIgnoreViewportSize := false;
  FTracking := True;
  FThumbSize := 0;
  FMinThumbSize := 5;
  FOnChange := nil;
  FOnTracking := nil;
  FBackGround := nil;
  FHighlight := nil;
  FThumb := nil;
end;

{********************************}
destructor TALCustomTrack.Destroy;
begin
  ALFreeAndNil(FDefaultValueRange);
  ALFreeAndNil(FValueRange);
  inherited;
end;

{*****************************************}
procedure TALCustomTrack.AfterConstruction;
begin
  inherited;
  DefaultValueRange.Assign(FValueRange.New);
  realign;
end;

{******************************}
procedure TALCustomTrack.Loaded;
begin
  if not (csDestroying in ComponentState) then begin
    if FValueRange.IsChanged then
      FValueRange.Changed(True);
  end;
  inherited;
end;

{**********************************************************}
function TALCustomTrack.CreateValueRangeTrack : TValueRange;
begin
  Result := TALValueRangeTrack.Create(Self);
end;

{**************************************}
function TALCustomTrack.GetData: TValue;
begin
  Result := Value;
end;

{*************************************************************}
function TALCustomTrack.GetDefaultTouchTargetExpansion: TRectF;
var DeviceSrv: IFMXDeviceService;
begin
  if SupportsPlatformService(IFMXDeviceService, DeviceSrv) and
    (TDeviceFeature.HasTouchScreen in DeviceSrv.GetFeatures) then
    Result := TRectF.Create(
                DefaultTouchTargetExpansion,
                DefaultTouchTargetExpansion,
                DefaultTouchTargetExpansion,
                DefaultTouchTargetExpansion)
  else
    Result := inherited ;
end;

{****************************************************}
procedure TALCustomTrack.SetData(const Value: TValue);
begin
  if Value.IsType<TNotifyEvent> then OnChange := Value.AsType<TNotifyEvent>()
  else if Value.IsOrdinal then Self.Value := Value.AsOrdinal
  else if Value.IsType<Single> then Self.Value := Value.AsType<Single>
  else Self.Value := Min
end;

{*********************************************************************************************}
function TALCustomTrack.GetThumbRect(const Value: single; const aThumb: TALTrackThumb): TRectF;
var Pos, Size: Single;
begin
  Result := LocalRect;
  Size := GetThumbSize(FIgnoreViewportSize);
  case Orientation of
    TOrientation.Horizontal:
      begin
        Pos := _ValueToPos(Min, Max, ViewportSize, Size, Width, Value, FIgnoreViewportSize);
        Size := Size / 2;
        Result := RectF(Pos - Size, 0, Pos + Size, Height);
      end;
    TOrientation.Vertical:
      begin
        Pos := _ValueToPos(Min, Max, ViewportSize, Size, Height, Value, FIgnoreViewportSize);
        Size := Size / 2;
        Result := RectF(0, Pos - Size, Width, Pos + Size);
      end;
  end;
  if (aThumb <> nil) and
     (aThumb.Parent <> nil) and
     (aThumb.Parent is TControl) then begin
   if RectWidth(Result) > TControl(aThumb.Parent).Padding.left +
                           aThumb.Margins.left +
                           TControl(aThumb.Parent).Padding.right -
                           aThumb.Margins.right then begin
      Result.left := Round(Result.left + TControl(aThumb.Parent).Padding.left + aThumb.Margins.left);
      Result.right := Round(Result.right - TControl(aThumb.Parent).Padding.right - aThumb.Margins.right);
    end;
    Result.top := Round(Result.top + TControl(aThumb.Parent).Padding.top + aThumb.Margins.top);
    Result.bottom := Round(Result.bottom - TControl(aThumb.Parent).Padding.bottom - aThumb.Margins.bottom);
  end;
end;

{*****************************************************************************}
function TALCustomTrack.GetThumbSize(var IgnoreViewportSize: Boolean): Integer;
var
  lSize: Double;
begin
  Result := 0;
  case Orientation of
    TOrientation.Horizontal:
      begin
        if ViewportSize > 0 then lSize := ViewportSize / (Max - Min) * Width
        else if SameValue(FThumbSize, 0) then lSize := Height
        else lSize := FThumbSize;
        Result := Round(System.Math.Min(System.Math.MaxValue([lSize, Height / 2, FMinThumbSize]), Width));
      end;
    TOrientation.Vertical:
      begin
        if ViewportSize > 0 then lSize := ViewportSize / (Max - Min) * Height
        else if SameValue(FThumbSize, 0) then lSize := Width
        else lSize := FThumbSize;
        Result := Round(System.Math.Min(System.Math.MaxValue([lSize, Width / 2, FMinThumbSize]), Height));
      end;
  else
    lSize := FMinThumbSize;
  end;
  if Result < FMinThumbSize then Result := 0;
  IgnoreViewportSize := Result <= (lSize - 1);
end;

{*******************************************}
function TALCustomTrack.ValueStored: Boolean;
begin
  Result := not SameValue(Value, DefaultValueRange.Value);
end;

{*********************************************************}
procedure TALCustomTrack.SetThumbSize(const Value: Single);
begin
  if not SameValue(Value, fThumbSize) then begin
    fThumbSize := Value;
    Realign;
  end;
end;

{***********************************************}
function TALCustomTrack.ThumbSizeStored: Boolean;
begin
  Result := (not SameValue(fThumbSize, 0));
end;

{**************************************************}
function TALCustomTrack.ViewportSizeStored: Boolean;
begin
  Result := not SameValue(ViewportSize, DefaultValueRange.ViewportSize);
end;

{***********************************************}
function TALCustomTrack.FrequencyStored: Boolean;
begin
  Result := not SameValue(Frequency, DefaultValueRange.Frequency);
end;

{*****************************************}
function TALCustomTrack.MaxStored: Boolean;
begin
  Result := not SameValue(Max, DefaultValueRange.Max);
end;

{*****************************************}
function TALCustomTrack.MinStored: Boolean;
begin
  Result := not SameValue(Min, DefaultValueRange.Min);
end;

{*************************************}
function TALCustomTrack.GetMax: Double;
begin
  Result := FValueRange.Max;
end;

{***************************************************}
procedure TALCustomTrack.SetMax(const Value: Double);
begin
  if compareValue(Value, Min) < 0 then min := Value;
  FValueRange.Max := Value;
end;

{***************************************************}
procedure TALCustomTrack.SetMin(const Value: Double);
begin
  if compareValue(Value, Max) > 0 then max := Value;
  FValueRange.Min := Value;
end;

{*************************************}
function TALCustomTrack.GetMin: Double;
begin
  Result := FValueRange.Min;
end;

{*********************************************************}
procedure TALCustomTrack.SetFrequency(const Value: Double);
begin
  FValueRange.Frequency := Value;
end;

{*******************************************}
function TALCustomTrack.GetFrequency: Double;
begin
  Result := FValueRange.Frequency;
end;

{***************************************}
function TALCustomTrack.GetValue: Double;
begin
  Result := FValueRange.Value;
end;

{***********************************************}
procedure TALCustomTrack.SetValue(Value: Double);
begin
  FValueRange.Value := Value;
end;

{**********************************************}
function TALCustomTrack.GetViewportSize: Double;
begin
  Result := FValueRange.ViewportSize;
end;

{************************************************************}
procedure TALCustomTrack.SetViewportSize(const Value: Double);
begin
  FValueRange.ViewportSize := Value;
end;

{*******************************************************}
function TALCustomTrack.GetValueRange: TCustomValueRange;
begin
  Result := FValueRange;
end;

{**********************************************************************}
procedure TALCustomTrack.SetValueRange(const AValue: TCustomValueRange);
begin
  FValueRange.Assign(AValue);
end;

{****************************************************************}
procedure TALCustomTrack.SetValueRange_(const Value: TValueRange);
begin
  FValueRange.Assign(Value);
end;

{*********************************}
procedure TALCustomTrack.DoRealign;
var LThumbRect: TRectF;
begin
  inherited;
  if FThumb <> nil then begin
    LThumbRect := GetThumbRect(Value, FThumb);
    FThumb.Visible := not LThumbRect.IsEmpty;
    FThumb.BoundsRect := LThumbRect;
    Repaint;
  end;
  UpdateHighlight;
end;

{***************************************}
procedure TALCustomTrack.UpdateHighlight;
var r: TRectF;
begin
  r := GetThumbRect(Value, FThumb);
  if (FbackGround <> nil) then r.Offset(-fbackground.Margins.Left, -fbackground.Margins.top);
  if FHighlight <> nil then begin
    case Orientation of
      TOrientation.Horizontal: FHighlight.Width := Round((r.Left + r.Right) / 2);
      TOrientation.Vertical: FHighlight.Height := Round((r.Top + r.Bottom) / 2);
    end;
  end;
end;

{*********************************}
procedure TALCustomTrack.DoChanged;
begin
  if not (csLoading in ComponentState) and Assigned(FOnChange) then
    FOnChange(Self);
end;

{**********************************}
procedure TALCustomTrack.DoTracking;
begin
  if not (csLoading in ComponentState) and Assigned(FOnTracking) then
    FOnTracking(Self);
end;

{************************************************************************************************}
procedure TALCustomTrack.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var inc: Double;
    LValue: Double;
begin
  inc := Frequency;
  if inc = 0 then inc := 1;
  inherited;
  case Key of
    vkHome: LValue := Min;
    vkEnd: LValue := Max;
    vkUp: LValue := Value - inc;
    vkDown: LValue := Value + inc;
    vkLeft: LValue := Value - inc;
    vkRight: LValue := Value + inc;
    else Exit;
  end;
  Key := 0;
  SetValue(LValue);
end;

{*****************************************************************}
procedure TALCustomTrack.SetOrientation(const Value: TOrientation);
begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    if not (csLoading in ComponentState) then begin
      SetBounds(Position.X, Position.Y, Size.Height, Size.Width);
      if FOrientation=TOrientation.Horizontal then begin
        if FBackGround <> nil then begin
          FBackGround.Align := TalignLayout.none;
          FBackGround.Size.Height := FBackGround.Size.Width;
          FBackGround.Margins.Left := FBackGround.Margins.Top;
          FBackGround.Margins.right := FBackGround.Margins.Bottom;
          FBackGround.Margins.Top := 0;
          FBackGround.Margins.Bottom := 0;
          FBackGround.Align := TalignLayout.VertCenter;
        end;
        //-----
        if FHighlight <> nil then begin
          FHighlight.Size.Height := FBackGround.Size.height;
          FHighlight.Size.Width := 0;
        end;
      end
      else begin
        if FBackGround <> nil then begin
          FBackGround.Align := TalignLayout.none;
          FBackGround.Size.Width := FBackGround.Size.Height;
          FBackGround.Margins.top := FBackGround.Margins.Left;
          FBackGround.Margins.Bottom := FBackGround.Margins.right;
          FBackGround.Margins.left := 0;
          FBackGround.Margins.right := 0;
          FBackGround.Align := TalignLayout.HorzCenter;
        end;
        //-----
        if FHighlight <> nil then begin
          FHighlight.Size.Width := FBackGround.Size.width;
          FHighlight.Size.Height := 0;
        end;
      end;
    end;
  end;
end;

{*********************************************}
function TALCustomTrack.GetIsTracking: Boolean;
begin
  Result := (FThumb <> nil) and FThumb.FPressed;
end;

{*************************************************}
constructor TALTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := True;
  SetAcceptsControls(False);
  //-----
  FBackGround := TALTrackBackground.Create(self);
  FBackGround.Parent := self;
  FBackGround.Stored := False;
  FBackGround.SetSubComponent(True);
  FBackGround.Name := 'BackGround';
  FBackGround.Align := TalignLayout.VertCenter;
  FBackGround.Size.Height := 2;
  FBackGround.Margins.DefaultValue := TrectF.Create(16,0,16,0);
  FBackGround.Margins.Left := 16;
  FBackGround.Margins.right := 16;
  FBackGround.Stroke.Color := TAlphaColors.Null;
  fBackGround.Fill.Color := $ffc5c5c5;
  //-----
  FHighlight := TALTrackHighlight.Create(FBackGround);
  FHighlight.Parent := FBackGround;
  FHighlight.Stored := False;
  FHighlight.SetSubComponent(True);
  FHighlight.Name := 'Highlight';
  FHighlight.Position.Point := TpointF.Create(0,0);
  FHighlight.Size.Height := 2;
  FHighlight.Size.Width := 0;
  FHighlight.Stroke.Color := TalphaColors.Null;
  FHighlight.Fill.Color := $ff167efc;
  //-----
  FThumb := TALTrackThumb.Create(self, fValueRange, true{aWithGlyphObj});
  FThumb.Parent := self;
  FThumb.Stored := False;
  FThumb.SetSubComponent(True);
  FThumb.Name := 'Thumb';
  FThumb.XRadius := 16;
  FThumb.yRadius := 16;
  fThumb.Stroke.Color := $ffd5d5d5;
  FThumb.Fill.Color := $ffffffff;
end;

{******************************************}
function TALTrackBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 32);
end;

{**************************************************}
constructor TALScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := False;
  SetAcceptsControls(False);
  //-----
  FThumb := TALTrackThumb.Create(self, FvalueRange, False{aWithGlyphObj});
  FThumb.Parent := self;
  FThumb.Stored := False;
  FThumb.SetSubComponent(True);
  FThumb.Name := 'Thumb';
  FThumb.Stroke.Color := Talphacolors.Null;
  FThumb.Fill.Color := $47000000;
end;

{*******************************************}
function TALScrollBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(150, 4);
end;

{******************************************************}
constructor TALRangeTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FMaxValueRange := CreateValueRangeTrack;
  FMaxValueRange.Value := FMaxValueRange.Max;
  CanFocus := True;
  SetAcceptsControls(False);
  //-----
  FBackGround := TALTrackBackground.Create(self);
  FBackGround.Parent := self;
  FBackGround.Stored := False;
  FBackGround.SetSubComponent(True);
  FBackGround.Name := 'BackGround';
  FBackGround.Align := TalignLayout.VertCenter;
  FBackGround.Size.Height := 2;
  FBackGround.Margins.DefaultValue := TrectF.Create(16,0,16,0);
  FBackGround.Margins.Left := 16;
  FBackGround.Margins.right := 16;
  FBackGround.Stroke.Color := Talphacolors.Null;
  fBackGround.Fill.Color := $ffc5c5c5;
  //-----
  FHighlight := TALTrackHighlight.Create(FBackGround);
  FHighlight.Parent := FBackGround;
  FHighlight.Stored := False;
  FHighlight.SetSubComponent(True);
  FHighlight.Name := 'Highlight';
  FHighlight.Position.Point := TpointF.Create(0,0);
  FHighlight.Size.Height := 2;
  FHighlight.Size.Width := 0;
  FHighlight.Stroke.Color := Talphacolors.Null;
  FHighlight.Fill.Color := $ff167efc;
  //-----
  FThumb := TALTrackThumb.Create(self, FvalueRange, true{aWithGlyphObj});
  FThumb.Parent := self;
  FThumb.Stored := False;
  FThumb.SetSubComponent(True);
  FThumb.Name := 'MinThumb';
  FThumb.XRadius := 16;
  FThumb.yRadius := 16;
  fThumb.Stroke.Color := $ffd5d5d5;
  FThumb.Fill.Color := $ffffffff;
  //-----
  FMaxThumb := TALTrackThumb.Create(self, fMaxValueRange, true{aWithGlyphObj});
  FMaxThumb.Parent := self;
  FMaxThumb.Stored := False;
  FMaxThumb.SetSubComponent(True);
  FMaxThumb.Name := 'MaxThumb';
  FMaxThumb.XRadius := 16;
  FMaxThumb.yRadius := 16;
  fMaxThumb.Stroke.Color := $ffd5d5d5;
  FMaxThumb.Fill.Color := $ffffffff;
end;

{**********************************}
destructor TALRangeTrackBar.Destroy;
begin
  ALFreeAndNil(FMaxValueRange);
  inherited;
end;

{***********************************************}
function TALRangeTrackBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(200, 32);
end;

{********************************}
procedure TALRangeTrackBar.Loaded;
begin
  if not (csDestroying in ComponentState) then begin
    if FMaxValueRange.IsChanged then
      FMaxValueRange.Changed(True);
  end;
  inherited;
end;

{***********************************}
procedure TALRangeTrackBar.DoRealign;
var R: TRectF;
begin
  //realign is call be TALValueRangeTrack.DoChanged;
  //so we can check here if minValue <= MaxValue
  if minValue > MaxValue then begin
    if fThumb.Pressed then MinValue := MaxValue
    else MaxValue := MinValue;
    exit; // no need to continue, this function will be called again
  end;
  if FMaxThumb <> nil then begin
    R := GetThumbRect(MaxValue, FMaxThumb);
    FMaxThumb.Visible := not ((R.Right <= R.Left) or (R.Bottom <= R.Top));
    FMaxThumb.BoundsRect := R;
  end;
  inherited DoRealign;
end;

{*****************************************}
procedure TALRangeTrackBar.UpdateHighlight;
var rMin, rMax: TRectF;
begin
  rMin := GetThumbRect(Value, FThumb);
  rMax := GetThumbRect(MaxValue, FMaxThumb);
  if (FbackGround <> nil) then begin
    rMin.Offset(-fbackground.Margins.Left, -fbackground.Margins.top);
    rMax.Offset(-fbackground.Margins.Left, -fbackground.Margins.top);
  end;
  if FHighlight <> nil then begin
    case Orientation of
      TOrientation.Horizontal: begin
        FHighlight.setbounds(
          Round((rMin.Left + rMin.Right) / 2),
          FHighlight.Position.y,
          Round((rMax.Left + rMax.Right) / 2) - Round((rMin.Left + rMin.Right) / 2),
          FHighlight.Height);
      end;
      TOrientation.Vertical: begin
        FHighlight.setbounds(
          FHighlight.Position.x,
          Round((rMin.Top + rMin.Bottom) / 2),
          FHighlight.width,
          Round((rMax.Top + rMax.Bottom) / 2) - Round((rMin.Top + rMin.Bottom) / 2));
      end;
    end;
  end;
end;

{*************************************************}
procedure TALRangeTrackBar.SetValue(Value: Double);
begin
  inherited SetValue(Value);
  if (not fThumb.Pressed) and
     (GetValue > (max - Min) / 2) then fThumb.BringToFront;
end;

{********************************************}
function TALRangeTrackBar.GetMaxValue: Double;
begin
  Result := FMaxValueRange.Value;
end;

{****************************************************}
procedure TALRangeTrackBar.SetMaxValue(Value: Double);
begin
  FMaxValueRange.Value := Value;
  if (not fMaxThumb.Pressed) and
     (GetMaxValue < (max - Min) / 2) then fMaxThumb.BringToFront;
end;

{************************************************}
function TALRangeTrackBar.MaxValueStored: Boolean;
begin
  Result := not SameValue(MaxValue, DefaultValueRange.Value);
end;

{***********************************************************}
procedure TALRangeTrackBar.SetFrequency(const Value: Double);
begin
  inherited;
  FMaxValueRange.Frequency := Value;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMax(const Value: Double);
begin
  if compareValue(Value, Min) < 0 then min := Value;
  inherited;
  FMaxValueRange.Max := Value;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMin(const Value: Double);
begin
  if compareValue(Value, Max) > 0 then max := Value;
  inherited;
  FMaxValueRange.Min := Value;
end;

{**************************************************************}
procedure TALRangeTrackBar.SetViewportSize(const Value: Double);
begin
  inherited;
  FMaxValueRange.ViewportSize := Value;
end;

{**********************************************************************************************}
constructor TALCheckMarkBrush.Create(const ADefaultColor: TAlphaColor);
begin
  inherited Create;
  //--
  FDefaultColor := ADefaultColor;
  FDefaultResourceName := '';
  FDefaultWrapMode := TALImageWrapMode.Fit;
  FDefaultThickness := 2;
  //--
  FColor := FDefaultColor;
  FResourceName := FDefaultResourceName;
  FWrapMode := FDefaultWrapMode;
  FThickness := FDefaultThickness;
  //--
  FPadding := TBounds.Create(TRectF.Empty);
  FPadding.OnChange := PaddingChanged;
end;

{**************************}
destructor TALCheckMarkBrush.Destroy;
begin
  ALFreeAndNil(FPadding);
  inherited;
end;

{*********************************}
function TALCheckMarkBrush.CreateSavedState: TALPersistentObserver;
type
  TALCheckMarkBrushClass = class of TALCheckMarkBrush;
begin
  result := TALCheckMarkBrushClass(classtype).Create(DefaultColor);
end;

{**********************************************}
procedure TALCheckMarkBrush.Assign(Source: TPersistent);
begin
  if Source is TALCheckMarkBrush then begin
    BeginUpdate;
    Try
      Color := TALCheckMarkBrush(Source).Color;
      ResourceName := TALCheckMarkBrush(Source).ResourceName;
      WrapMode := TALCheckMarkBrush(Source).WrapMode;
      Thickness := TALStrokeBrush(Source).Thickness;
      Padding.Assign(TALCheckMarkBrush(Source).Padding);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{************************}
procedure TALCheckMarkBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Color := DefaultColor;
    ResourceName := DefaultResourceName;
    WrapMode := DefaultWrapMode;
    Thickness := DefaultThickness;
    Padding.Rect := Padding.DefaultValue;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
procedure TALCheckMarkBrush.Interpolate(const ATo: TALCheckMarkBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Color := InterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      ResourceName := ATo.ResourceName;
      WrapMode := ATo.WrapMode;
      Thickness := InterpolateSingle(Thickness{Start}, ATo.Thickness{Stop}, ANormalizedTime);
      Padding.Left := InterpolateSingle(Padding.Left{Start}, ATo.Padding.Left{Stop}, ANormalizedTime);
      Padding.Right := InterpolateSingle(Padding.Right{Start}, ATo.Padding.Right{Stop}, ANormalizedTime);
      Padding.Top := InterpolateSingle(Padding.Top{Start}, ATo.Padding.Top{Stop}, ANormalizedTime);
      Padding.Bottom := InterpolateSingle(Padding.Bottom{Start}, ATo.Padding.Bottom{Stop}, ANormalizedTime);
    end
    else begin
      Color := InterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      ResourceName := DefaultResourceName;
      WrapMode := DefaultWrapMode;
      Thickness := InterpolateSingle(Thickness{Start}, DefaultThickness{Stop}, ANormalizedTime);
      Padding.Left := InterpolateSingle(Padding.Left{Start}, Padding.DefaultValue.Left{Stop}, ANormalizedTime);
      Padding.Right := InterpolateSingle(Padding.Right{Start}, Padding.DefaultValue.Right{Stop}, ANormalizedTime);
      Padding.Top := InterpolateSingle(Padding.Top{Start}, Padding.DefaultValue.Top{Stop}, ANormalizedTime);
      Padding.Bottom := InterpolateSingle(Padding.Bottom{Start}, Padding.DefaultValue.Bottom{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
procedure TALCheckMarkBrush.InterpolateNoChanges(const ATo: TALCheckMarkBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{************************}
function TALCheckMarkBrush.HasCheckMark: boolean;
begin
  result := ((Color <> TalphaColors.Null) and
             (CompareValue(FThickness, 0, TEpsilon.Vector) > 0)) or
            (ResourceName <> '');
end;

{***************************************}
function TALCheckMarkBrush.IsColorStored: Boolean;
begin
  result := FColor <> FDefaultColor;
end;

{**********************************************}
function TALCheckMarkBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> FDefaultResourceName;
end;

{**********************************************}
function TALCheckMarkBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> FDefaultWrapMode;
end;

{****************************************************}
function TALCheckMarkBrush.IsThicknessStored: Boolean;
begin
  result := not SameValue(FThickness, FDefaultThickness, TEpsilon.Vector);
end;

{****************************************************}
procedure TALCheckMarkBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
  end;
end;

{******************************************************}
procedure TALCheckMarkBrush.SetResourceName(const Value: String);
begin
  if fResourceName <> Value then begin
    fResourceName := Value;
    Change;
  end;
end;

{******************************************************}
procedure TALCheckMarkBrush.SetWrapMode(const Value: TALImageWrapMode);
begin
  if fWrapMode <> Value then begin
    fWrapMode := Value;
    Change;
  end;
end;

{*********************************************************}
procedure TALCheckMarkBrush.SetThickness(const Value: Single);
begin
  if not SameValue(Value, FThickness, TEpsilon.Vector) then begin
    fThickness := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALCheckMarkBrush.SetPadding(const Value: TBounds);
begin
  FPadding.Assign(Value);
end;

{*************************************************}
procedure TALCheckMarkBrush.PaddingChanged(Sender: TObject);
begin
  change;
end;

{***************************************************************************************************}
constructor TALInheritCheckMarkBrush.Create(const AParent: TALCheckMarkBrush; const ADefaultColor: TAlphaColor);
begin
  inherited create(ADefaultColor);
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{*********************************}
function TALInheritCheckMarkBrush.CreateSavedState: TALPersistentObserver;
type
  TALInheritCheckMarkBrushClass = class of TALInheritCheckMarkBrush;
begin
  result := TALInheritCheckMarkBrushClass(classtype).Create(nil{AParent}, DefaultColor);
end;

{**********************************************************}
procedure TALInheritCheckMarkBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{****************************************************}
procedure TALInheritCheckMarkBrush.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALInheritCheckMarkBrush then begin
      Inherit := TALInheritCheckMarkBrush(Source).Inherit;
      fSuperseded := TALInheritCheckMarkBrush(Source).fSuperseded;
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

{******************************}
procedure TALInheritCheckMarkBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Inherit := True;
    fSuperseded := False;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritCheckMarkBrush.DoSupersede;
begin
  Assign(FParent);
end;

{******************}
procedure TALInheritCheckMarkBrush.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  beginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TALInheritCheckMarkBrush then begin
      TALInheritCheckMarkBrush(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TALInheritCheckMarkBrush(FParent).restoreState;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{*************************}
procedure TALInheritCheckMarkBrush.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{***********************************}
constructor TALCheckBoxBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  //--
  if StateStyleParent <> nil then FCheckMark := TALInheritCheckMarkBrush.Create(StateStyleParent.CheckMark, TAlphaColors.Black{ADefaultColor})
  else if ControlParent <> nil then FCheckMark := TALInheritCheckMarkBrush.Create(ControlParent.CheckMark, TAlphaColors.Black{ADefaultColor})
  else FCheckMark := TALInheritCheckMarkBrush.Create(nil, TAlphaColors.Black{ADefaultColor});
  //--
  StateLayer.Padding.DefaultValue := TRectF.Create(-10,-10,-10,-10);
  StateLayer.Padding.Rect := StateLayer.Padding.DefaultValue;
  //--
  FCheckMark.OnChanged := CheckMarkChanged;
end;

{*************************************}
destructor TALCheckBoxBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FCheckMark);
  inherited Destroy;
end;

{******************************************************}
procedure TALCheckBoxBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TALCheckBoxBaseStateStyle then begin
    BeginUpdate;
    Try
      CheckMark.Assign(TALCheckBoxBaseStateStyle(Source).CheckMark);
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{******************************}
procedure TALCheckBoxBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    CheckMark.Reset;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALCheckBoxBaseStateStyle.Interpolate(const ATo: TALCheckBoxBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then CheckMark.Interpolate(ATo.CheckMark, ANormalizedTime)
    else if StateStyleParent <> nil then CheckMark.Interpolate(StateStyleParent.CheckMark, ANormalizedTime)
    else if ControlParent <> nil then CheckMark.Interpolate(ControlParent.CheckMark, ANormalizedTime)
    else CheckMark.Interpolate(nil, ANormalizedTime);
  Finally
    EndUpdate;
  End;
end;

{******************************************************}
procedure TALCheckBoxBaseStateStyle.InterpolateNoChanges(const ATo: TALCheckBoxBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{******************}
procedure TALCheckBoxBaseStateStyle.DoSupersede;
begin
  inherited;
  CheckMark.Supersede;
end;

{********************************************************************************}
function TALCheckBoxBaseStateStyle.GetStateStyleParent: TALCheckBoxBaseStateStyle;
begin
  {$IF defined(debug)}
  if (inherited StateStyleParent <> nil) and
     (not (inherited StateStyleParent is TALCheckBoxBaseStateStyle)) then
    raise Exception.Create('StateStyleParent must be of type TALCheckBoxBaseStateStyle');
  {$ENDIF}
  result := TALCheckBoxBaseStateStyle(inherited StateStyleParent);
end;

{***************************************************************}
function TALCheckBoxBaseStateStyle.GetControlParent: TALCheckBox;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TALCheckBox)) then
    raise Exception.Create('ControlParent must be of type TALCheckBox');
  {$ENDIF}
  result := TALCheckBox(inherited ControlParent);
end;

{********************************************************************************}
procedure TALCheckBoxBaseStateStyle.SetCheckMark(const AValue: TALInheritCheckMarkBrush);
begin
  FCheckMark.Assign(AValue);
end;

{***********************************************}
function TALCheckBoxBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            CheckMark.Inherit;
end;

{*****************************************************************}
procedure TALCheckBoxBaseStateStyle.CheckMarkChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************}
function TALCheckBoxDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{********************************************************************}
procedure TALCheckBoxDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{***********************************************************************}
constructor TALCheckBoxDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{****************************************************************}
procedure TALCheckBoxDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALCheckBoxDisabledStateStyle then
      Opacity := TALCheckBoxDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{******************************}
procedure TALCheckBoxDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{******************************}
function TALCheckBoxDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{*************************************}
constructor TALCheckBoxCheckStateStyles.Create(const AParent: TALCheckBox);
begin
  inherited Create;
  //--
  FDefault := TALCheckBoxDefaultStateStyle.Create(AParent);
  FDefault.OnChanged := DefaultChanged;
  //--
  FDisabled := TALCheckBoxDisabledStateStyle.Create(FDefault);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := TALCheckBoxHoveredStateStyle.Create(FDefault);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := TALCheckBoxPressedStateStyle.Create(FDefault);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := TALCheckBoxFocusedStateStyle.Create(FDefault);
  FFocused.OnChanged := FocusedChanged;
end;

{*************************************}
destructor TALCheckBoxCheckStateStyles.Destroy;
begin
  ALFreeAndNil(FDefault);
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{*********************************}
function TALCheckBoxCheckStateStyles.CreateSavedState: TALPersistentObserver;
type
  TALCheckBoxCheckStateStylesClass = class of TALCheckBoxCheckStateStyles;
begin
  result := TALCheckBoxCheckStateStylesClass(classtype).Create(nil{AParent});
end;

{******************************************************}
procedure TALCheckBoxCheckStateStyles.Assign(Source: TPersistent);
begin
  if Source is TALCheckBoxCheckStateStyles then begin
    BeginUpdate;
    Try
      Default.Assign(TALCheckBoxCheckStateStyles(Source).Default);
      Disabled.Assign(TALCheckBoxCheckStateStyles(Source).Disabled);
      Hovered.Assign(TALCheckBoxCheckStateStyles(Source).Hovered);
      Pressed.Assign(TALCheckBoxCheckStateStyles(Source).Pressed);
      Focused.Assign(TALCheckBoxCheckStateStyles(Source).Focused);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{******************************}
procedure TALCheckBoxCheckStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Default.Reset;
    Disabled.Reset;
    Hovered.Reset;
    Pressed.Reset;
    Focused.Reset;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************}
procedure TALCheckBoxCheckStateStyles.SetDefault(const AValue: TALCheckBoxDefaultStateStyle);
begin
  FDefault.Assign(AValue);
end;

{************************************************************************************}
procedure TALCheckBoxCheckStateStyles.SetDisabled(const AValue: TALCheckBoxDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{************************************************************************************}
procedure TALCheckBoxCheckStateStyles.SetHovered(const AValue: TALCheckBoxHoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALCheckBoxCheckStateStyles.SetPressed(const AValue: TALCheckBoxPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALCheckBoxCheckStateStyles.SetFocused(const AValue: TALCheckBoxFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{**********************************************************}
procedure TALCheckBoxCheckStateStyles.DefaultChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************}
procedure TALCheckBoxCheckStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************}
procedure TALCheckBoxCheckStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALCheckBoxCheckStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALCheckBoxCheckStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{*************************************}
constructor TALCheckBoxStateStyles.Create(const AParent: TALCheckBox);
begin
  inherited Create;
  //--
  FChecked := TALCheckBoxCheckStateStyles.Create(AParent);
  FChecked.OnChanged := CheckedChanged;
  //--
  FUnchecked := TALCheckBoxCheckStateStyles.Create(AParent);
  FUnchecked.OnChanged := UncheckedChanged;
end;

{*************************************}
destructor TALCheckBoxStateStyles.Destroy;
begin
  ALFreeAndNil(FChecked);
  ALFreeAndNil(FUnchecked);
  inherited Destroy;
end;

{*********************************}
function TALCheckBoxStateStyles.CreateSavedState: TALPersistentObserver;
type
  TALCheckBoxStateStylesClass = class of TALCheckBoxStateStyles;
begin
  result := TALCheckBoxStateStylesClass(classtype).Create(nil{AParent});
end;

{******************************************************}
procedure TALCheckBoxStateStyles.Assign(Source: TPersistent);
begin
  if Source is TALCheckBoxStateStyles then begin
    BeginUpdate;
    Try
      Checked.Assign(TALCheckBoxStateStyles(Source).Checked);
      Unchecked.Assign(TALCheckBoxStateStyles(Source).Unchecked);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{******************************}
procedure TALCheckBoxStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Checked.reset;
    Unchecked.reset;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************}
procedure TALCheckBoxStateStyles.SetChecked(const AValue: TALCheckBoxCheckStateStyles);
begin
  FChecked.Assign(AValue);
end;

{*********************************************************************************}
procedure TALCheckBoxStateStyles.SetUnchecked(const AValue: TALCheckBoxCheckStateStyles);
begin
  FUnchecked.Assign(AValue);
end;

{**********************************************************}
procedure TALCheckBoxStateStyles.CheckedChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************}
procedure TALCheckBoxStateStyles.UncheckedChanged(ASender: TObject);
begin
  Change;
end;

{*************************************************}
constructor TALCheckbox.Create(AOwner: TComponent);
begin
  inherited;
  //--
  SetAcceptsControls(False);
  CanFocus := True;
  Cursor := crHandPoint;
  //--
  Fill.DefaultColor := TAlphaColors.White;
  Fill.Color := Fill.DefaultColor;
  //--
  Stroke.DefaultColor := TAlphaColors.Black;
  Stroke.Color := Stroke.DefaultColor;
  //--
  FOnChange := nil;
  FDoubleBuffered := true;
  FXRadius := 0;
  FYRadius := 0;
  FChecked := False;
  FCheckMark := TALCheckMarkBrush.Create(TAlphaColors.Black);
  FCheckMark.OnChanged := CheckMarkChanged;
  //--
  FStateStyles := TALCheckBoxStateStyles.Create(Self);
  FStateStyles.OnChanged := StateStylesChanged;
  //--
  fBufCheckedDrawable := ALNullDrawable;
  fBufCheckedDisabledDrawable := ALNullDrawable;
  fBufCheckedHoveredDrawable := ALNullDrawable;
  fBufCheckedPressedDrawable := ALNullDrawable;
  fBufCheckedFocusedDrawable := ALNullDrawable;
  fBufUnCheckedDrawable := ALNullDrawable;
  fBufUnCheckedDisabledDrawable := ALNullDrawable;
  fBufUnCheckedHoveredDrawable := ALNullDrawable;
  fBufUnCheckedPressedDrawable := ALNullDrawable;
  fBufUnCheckedFocusedDrawable := ALNullDrawable;
end;

{*****************************}
destructor TALCheckbox.Destroy;
begin
  clearBufDrawable;
  ALFreeAndNil(FCheckMark);
  ALFreeAndNil(FStateStyles);
  inherited;
end;

{*****************************}
procedure TALCheckbox.Click;
begin
  Checked := not Checked;
  inherited;
end;

{*********************************************************************************************}
procedure TALCheckbox.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited;
  if (KeyChar = ' ') then begin
    Click; // Emulate mouse click to perform Action.OnExecute
    KeyChar := #0;
  end;
end;

{***********************************************}
function TALCheckbox.GetDoubleBuffered: boolean;
begin
  result := fDoubleBuffered;
end;

{**************************************************************}
procedure TALCheckbox.SetDoubleBuffered(const AValue: Boolean);
begin
  if AValue <> fDoubleBuffered then begin
    fDoubleBuffered := AValue;
    if not fDoubleBuffered then clearBufDrawable;
  end;
end;

{****************************************************}
procedure TALCheckbox.SetXRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FXRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FXRadius := NewValue;
    Repaint;
  end;
end;

{****************************************************}
procedure TALCheckbox.SetYRadius(const Value: Single);
var
  NewValue: Single;
begin
  if csDesigning in ComponentState then NewValue := Max(-50, Min(Value, Min(Width / 2, Height / 2)))
  else NewValue := Value;
  if not SameValue(FYRadius, NewValue, TEpsilon.Vector) then begin
    clearBufDrawable;
    FYRadius := NewValue;
    Repaint;
  end;
end;

{***************************************}
function TALCheckbox.GetChecked: Boolean;
begin
  Result := FChecked;
end;

{*****************************************************}
procedure TALCheckbox.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then begin
    FChecked := Value;
    DoChanged;
  end;
end;

{*********************************************************************}
procedure TALCheckbox.SetStateStyles(const AValue: TALCheckBoxStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{******************************************************}
procedure TALCheckbox.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
  (* *)
  if FChecked then DisabledOpacity := StateStyles.Checked.Disabled.opacity
  else DisabledOpacity := StateStyles.Unchecked.Disabled.opacity;
  Repaint;
end;

{***********************************************************}
procedure TALCheckbox.SetCheckMark(const Value: TALCheckMarkBrush);
begin
  FCheckMark.Assign(Value);
end;

{******************************************************}
procedure TALCheckbox.CheckMarkChanged(Sender: TObject);
begin
  clearBufDrawable;
  Repaint;
end;

{**************************************************}
procedure TALCheckbox.FillChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{****************************************************}
procedure TALCheckbox.StrokeChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{****************************************************}
procedure TALCheckbox.ShadowChanged(Sender: TObject);
begin
  clearBufDrawable;
  inherited;
end;

{**************************************************}
procedure TALCheckbox.IsMouseOverChanged;
begin
  inherited;
  repaint;
end;

{**************************************************}
procedure TALCheckbox.PressedChanged;
begin
  inherited;
  repaint;
end;

{******************************************}
function TALCheckbox.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(22, 22);
end;

{******************************}
procedure TALCheckbox.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;
end;

{******************************}
procedure TALCheckbox.DoResized;
begin
  ClearBufDrawable;
  inherited;
end;

{***********************************}
procedure TALCheckbox.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     ((not ALIsDrawableNull(fBufCheckedDrawable)) or
      (not ALIsDrawableNull(fBufCheckedDisabledDrawable)) or
      (not ALIsDrawableNull(fBufCheckedHoveredDrawable)) or
      (not ALIsDrawableNull(fBufCheckedPressedDrawable)) or
      (not ALIsDrawableNull(fBufCheckedFocusedDrawable)) or
      (not ALIsDrawableNull(fBufUnCheckedDrawable)) or
      (not ALIsDrawableNull(fBufUnCheckedDisabledDrawable)) or
      (not ALIsDrawableNull(fBufUnCheckedHoveredDrawable)) or
      (not ALIsDrawableNull(fBufUnCheckedPressedDrawable)) or
      (not ALIsDrawableNull(fBufUnCheckedFocusedDrawable))) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufCheckedDrawable);
  ALFreeAndNilDrawable(fBufCheckedDisabledDrawable);
  ALFreeAndNilDrawable(fBufCheckedHoveredDrawable);
  ALFreeAndNilDrawable(fBufCheckedPressedDrawable);
  ALFreeAndNilDrawable(fBufCheckedFocusedDrawable);
  ALFreeAndNilDrawable(fBufUnCheckedDrawable);
  ALFreeAndNilDrawable(fBufUnCheckedDisabledDrawable);
  ALFreeAndNilDrawable(fBufUnCheckedHoveredDrawable);
  ALFreeAndNilDrawable(fBufUnCheckedPressedDrawable);
  ALFreeAndNilDrawable(fBufUnCheckedFocusedDrawable);
end;

{**********************************}
procedure TALCheckbox.DrawCheckMark(
            const ACanvas: TALCanvas;
            const AScale: Single;
            const ADstRect: TrectF;
            const ACheckMark: TALCheckMarkBrush;
            const AChecked: Boolean);
begin
  var LRect := ADstRect;
  LRect.Top := LRect.Top * AScale;
  LRect.right := LRect.right * AScale;
  LRect.left := LRect.left * AScale;
  LRect.bottom := LRect.bottom * AScale;
  var LScaledPaddingRect := ACheckMark.padding.Rect;
  LScaledPaddingRect.Left := LScaledPaddingRect.Left * AScale;
  LScaledPaddingRect.right := LScaledPaddingRect.right * AScale;
  LScaledPaddingRect.top := LScaledPaddingRect.top * AScale;
  LScaledPaddingRect.bottom := LScaledPaddingRect.bottom * AScale;
  LRect.Top := LRect.Top + LScaledPaddingRect.top;
  LRect.right := LRect.right - LScaledPaddingRect.right;
  LRect.left := LRect.left + LScaledPaddingRect.left;
  LRect.bottom := LRect.bottom - LScaledPaddingRect.bottom;

  // Without ResourceName
  if ACheckMark.ResourceName = '' then begin

    // Exit if no color or no stroke
    var LScaledCheckMarkThickness := ACheckMark.Thickness * AScale;
    if (ACheckMark.Color = TalphaColors.Null) or (CompareValue(LScaledCheckMarkThickness, 0, TEpsilon.position) <= 0) then
      exit;

    // exit if not checked
    if not checked then
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

    ALDrawRectangle(
      ACanvas, // const ACanvas: TALCanvas;
      1, // const AScale: Single;
      LRect, // const ADstRect: TrectF;
      1, // const AOpacity: Single;
      TAlphaColors.Null, // const AFillColor: TAlphaColor;
      TGradientStyle.Linear, // const AFillGradientStyle: TGradientStyle;
      [], // const AFillGradientColors: TArray<TAlphaColor>;
      [], // const AFillGradientOffsets: TArray<Single>;
      TPointF.Zero, // const AFillGradientStartPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      TPointF.Zero, // const AFillGradientEndPoint: TPointF; // Coordinates in ADstRect space. You can use ALGetLinearGradientCoordinates to convert angle to point
      ACheckMark.ResourceName, // const AFillResourceName: String;
      ACheckMark.WrapMode, // Const AFillWrapMode: TALImageWrapMode;
      TRectF.Empty, // Const AFillPaddingRect: TRectF;
      TAlphaColors.Null, // const AStrokeColor: TalphaColor;
      0, // const AStrokeThickness: Single;
      TAlphaColors.Null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
      0, // const AShadowBlur: Single;
      0, // const AShadowOffsetX: Single;
      0, // const AShadowOffsetY: Single;
      AllSides, // const ASides: TSides;
      AllCorners, // const ACorners: TCorners;
      0, // const AXRadius: Single;
      0); // const AYRadius: Single)

  end;

end;

{**************************************}
Procedure TALCheckbox.CreateBufDrawable(
            var ABufDrawable: TALDrawable;
            var ABufDrawableRect: TRectF;
            const AFill: TALBrush;
            const AStateLayer: TALBrush;
            const AStroke: TALStrokeBrush;
            const ACheckMark: TALCheckMarkBrush;
            const AShadow: TALShadow);
begin

  if (not ALIsDrawableNull(ABufDrawable)) then exit;

  ABufDrawableRect := LocalRect;
  var LRect := ABufDrawableRect;

  if AFill.HasFill then begin
    var LFillRect := LRect;
    LfillRect.Inflate(-AFill.Padding.Left, -AFill.Padding.top, -AFill.Padding.right, -AFill.Padding.Bottom);
    ABufDrawableRect := TRectF.Union(LfillRect, ABufDrawableRect); // add the extra space needed to draw the fill
  end;

  if AStateLayer.HasFill then begin
    var LStateLayerRect := LRect;
    LStateLayerRect.Inflate(-AStateLayer.Padding.Left, -AStateLayer.Padding.top, -AStateLayer.Padding.right, -AStateLayer.Padding.Bottom);
    ABufDrawableRect := TRectF.Union(LStateLayerRect, ABufDrawableRect); // add the extra space needed to draw the StateLayer
  end;

  if ACheckMark.HasCheckMark then begin
    var LCheckMarkRect := LRect;
    LCheckMarkRect.Inflate(-ACheckMark.Padding.Left, -ACheckMark.Padding.top, -ACheckMark.Padding.right, -ACheckMark.Padding.Bottom);
    ABufDrawableRect := TRectF.Union(LCheckMarkRect, ABufDrawableRect); // add the extra space needed to draw the CheckMark
  end;

  if AShadow.HasShadow then begin
    var LShadowWidth := ALGetShadowWidth(AShadow.blur);
    var LShadowRect := LRect;
    LShadowRect.Inflate(LShadowWidth, LShadowWidth);
    LShadowRect.Offset(AShadow.OffsetX, AShadow.OffsetY);
    ABufDrawableRect := TRectF.Union(LShadowRect, ABufDrawableRect); // add the extra space needed to draw the shadow
  end;

  ABufDrawableRect := ALAlignDimensionToPixelRound(ABufDrawableRect, ALGetScreenScale); // to have the pixel aligned width and height
  LRect.Offset(Max(0, -ABufDrawableRect.Left), Max(0, -ABufDrawableRect.top));

  var LSurface: TALSurface;
  var LCanvas: TALCanvas;
  ALCreateSurface(
    LSurface, // out ASurface: TALSurface;
    LCanvas, // out ACanvas: TALCanvas;
    ALGetScreenScale, // const AScale: Single;
    ABufDrawableRect.Width, // const w: integer;
    ABufDrawableRect.height);// const h: integer)
  try

    if ALCanvasBeginScene(LCanvas) then
    try

      ALDrawRectangle(
        LCanvas, // const ACanvas: TALCanvas;
        ALGetScreenScale, // const AScale: Single;
        LRect, // const Rect: TrectF;
        1, // const AOpacity: Single;
        AFill, // const Fill: TALBrush;
        AStroke, // const Stroke: TALStrokeBrush;
        AShadow, // const Shadow: TALShadow
        ALLSides, // const Sides: TSides;
        ALLCorners, // const Corners: TCorners;
        XRadius, // const XRadius: Single = 0;
        YRadius); // const YRadius: Single = 0);

      if AStateLayer.HasFill then begin
        ALDrawRectangle(
          LCanvas, // const ACanvas: TALCanvas;
          ALGetScreenScale, // const AScale: Single;
          LRect, // const Rect: TrectF;
          1, // const AOpacity: Single;
          AStateLayer, // const Fill: TALBrush;
          nil, // const Stroke: TALStrokeBrush;
          nil, // const Shadow: TALShadow
          AllSides, // const Sides: TSides;
          AllCorners, // const Corners: TCorners;
          -50, // const XRadius: Single = 0;
          -50); // const YRadius: Single = 0);
      end;

      DrawCheckMark(
        LCanvas, // const ACanvas: TALCanvas;
        ALGetScreenScale, // const AScale: Single;
        LRect, // const ADstRect: TrectF;
        ACheckMark, // const ACheckMark: TALCheckMarkBrush;
        FChecked); // const AChecked: Boolean

    finally
      ALCanvasEndScene(LCanvas)
    end;

    ABufDrawable := ALSurfaceToDrawable(LSurface);

  finally
    ALFreeSurface(LSurface, LCanvas);
  end;

end;

{************************************}
procedure TALCheckbox.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _MakeBufDrawable(
              const AStateStyle: TALCheckBoxBaseStateStyle;
              var ABufDrawable: TALDrawable;
              var ABufDrawableRect: TRectF);
  begin
    if (AStateStyle <> StateStyles.Checked.Default) and
       (AStateStyle <> StateStyles.UnChecked.Default) and
       (AStateStyle.Inherit) then exit;
    if (not ALIsDrawableNull(ABufDrawable)) then exit;
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try
      //--
      CreateBufDrawable(
        ABufDrawable, // var ABufDrawable: TALDrawable;
        ABufDrawableRect, // var ABufDrawableRect: TRectF;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.StateLayer, // const AStateLayer: TALBrush;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.CheckMark, // const ACheckMark: TALCheckMarkBrush;
        AStateStyle.Shadow); // const AShadow: TALShadow);

(*
      // The shadow effect is not included in the fBufDrawableRect rectangle's dimensions.
      // However, the fBufDrawableRect rectangle is offset by the shadow's dx and dy values,
      // if a shadow is applied, to adjust for the visual shift caused by the shadow.
      var LMainDrawableRect := BufDrawableRect;
      LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
      var LCenteredRect := ABufDrawableRect.CenterAt(LMainDrawableRect);
      ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);
      ABufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);
*)
    finally
      AStateStyle.RestoreStateNoChanges;
    end;
  end;

begin
  if FChecked then begin
    _MakeBufDrawable(
      StateStyles.Checked.Default, // const AStateStyle: TALButtonStateStyle;
      FBufCheckedDrawable, // var ABufDrawable: TALDrawable;
      FBufCheckedDrawableRect); // var ABufDrawableRect: TRectF;
    if Not Enabled then begin
      _MakeBufDrawable(
        StateStyles.Checked.Disabled, // const AStateStyle: TALButtonStateStyle;
        FBufCheckedDisabledDrawable, // var ABufDrawable: TALDrawable;
        FBufCheckedDisabledDrawableRect); // var ABufDrawableRect: TRectF;
    end
    else if Pressed then begin
      _MakeBufDrawable(
        StateStyles.Checked.Pressed, // const AStateStyle: TALButtonStateStyle;
        FBufCheckedPressedDrawable, // var ABufDrawable: TALDrawable;
        FBufCheckedPressedDrawableRect); // var ABufDrawableRect: TRectF;
    end
    else if IsFocused then begin
      _MakeBufDrawable(
        StateStyles.Checked.Focused, // const AStateStyle: TALButtonStateStyle;
        FBufCheckedFocusedDrawable, // var ABufDrawable: TALDrawable;
        FBufCheckedFocusedDrawableRect); // var ABufDrawableRect: TRectF;
    end
    else if IsMouseOver then begin
      _MakeBufDrawable(
        StateStyles.Checked.Hovered, // const AStateStyle: TALButtonStateStyle;
        FBufCheckedHoveredDrawable, // var ABufDrawable: TALDrawable;
        FBufCheckedHoveredDrawableRect); // var ABufDrawableRect: TRectF;
    end;
  end
  else begin
    _MakeBufDrawable(
      StateStyles.UnChecked.Default, // const AStateStyle: TALButtonStateStyle;
      FBufUnCheckedDrawable, // var ABufDrawable: TALDrawable;
      FBufUnCheckedDrawableRect); // var ABufDrawableRect: TRectF;
    if Not Enabled then begin
      _MakeBufDrawable(
        StateStyles.UnChecked.Disabled, // const AStateStyle: TALButtonStateStyle;
        FBufUnCheckedDisabledDrawable, // var ABufDrawable: TALDrawable;
        FBufUnCheckedDisabledDrawableRect); // var ABufDrawableRect: TRectF;
    end
    else if Pressed then begin
      _MakeBufDrawable(
        StateStyles.UnChecked.Pressed, // const AStateStyle: TALButtonStateStyle;
        FBufUnCheckedPressedDrawable, // var ABufDrawable: TALDrawable;
        FBufUnCheckedPressedDrawableRect); // var ABufDrawableRect: TRectF;
    end
    else if IsFocused then begin
      _MakeBufDrawable(
        StateStyles.UnChecked.Focused, // const AStateStyle: TALButtonStateStyle;
        FBufUnCheckedFocusedDrawable, // var ABufDrawable: TALDrawable;
        FBufUnCheckedFocusedDrawableRect); // var ABufDrawableRect: TRectF;
    end
    else if IsMouseOver then begin
      _MakeBufDrawable(
        StateStyles.UnChecked.Hovered, // const AStateStyle: TALButtonStateStyle;
        FBufUnCheckedHoveredDrawable, // var ABufDrawable: TALDrawable;
        FBufUnCheckedHoveredDrawableRect); // var ABufDrawableRect: TRectF;
    end;
  end;
end;

{**************************}
procedure TALCheckbox.Paint;
begin

  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;

  if Fchecked then begin
    if Not Enabled then begin
      LDrawable := fBufCheckedDisabledDrawable;
      LDrawableRect := fBufCheckedDisabledDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := fBufCheckedDrawable;
        LDrawableRect := fBufCheckedDrawableRect;
      end;
    end
    //--
    else if Pressed then begin
      LDrawable := fBufCheckedPressedDrawable;
      LDrawableRect := fBufCheckedPressedDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := fBufCheckedDrawable;
        LDrawableRect := fBufCheckedDrawableRect;
      end;
    end
    //--
    else if IsFocused then begin
      LDrawable := fBufCheckedFocusedDrawable;
      LDrawableRect := fBufCheckedFocusedDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := fBufCheckedDrawable;
        LDrawableRect := fBufCheckedDrawableRect;
      end;
    end
    //--
    else if IsMouseOver then begin
      LDrawable := fBufCheckedHoveredDrawable;
      LDrawableRect := fBufCheckedHoveredDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := fBufCheckedDrawable;
        LDrawableRect := fBufCheckedDrawableRect;
      end;
    end
    //--
    else begin
      LDrawable := fBufCheckedDrawable;
      LDrawableRect := fBufCheckedDrawableRect;
    end;
  end
  else begin
    if Not Enabled then begin
      LDrawable := fBufUnCheckedDisabledDrawable;
      LDrawableRect := fBufUnCheckedDisabledDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := fBufUnCheckedDrawable;
        LDrawableRect := fBufUnCheckedDrawableRect;
      end;
    end
    //--
    else if Pressed then begin
      LDrawable := fBufUnCheckedPressedDrawable;
      LDrawableRect := fBufUnCheckedPressedDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := fBufUnCheckedDrawable;
        LDrawableRect := fBufUnCheckedDrawableRect;
      end;
    end
    //--
    else if IsFocused then begin
      LDrawable := fBufUnCheckedFocusedDrawable;
      LDrawableRect := fBufUnCheckedFocusedDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := fBufUnCheckedDrawable;
        LDrawableRect := fBufUnCheckedDrawableRect;
      end;
    end
    //--
    else if IsMouseOver then begin
      LDrawable := fBufUnCheckedHoveredDrawable;
      LDrawableRect := fBufUnCheckedHoveredDrawableRect;
      if ALIsDrawableNull(LDrawable) then begin
        LDrawable := fBufUnCheckedDrawable;
        LDrawableRect := fBufUnCheckedDrawableRect;
      end;
    end
    //--
    else begin
      LDrawable := fBufUnCheckedDrawable;
      LDrawableRect := fBufUnCheckedDrawableRect;
    end;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{****************************************************}
constructor TALRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  FGroupName := '';
  fMandatory := false;
  TMessageManager.DefaultManager.SubscribeToMessage(TRadioButtonGroupMessage, GroupMessageCall);
end;

{********************************}
destructor TALRadioButton.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TRadioButtonGroupMessage, GroupMessageCall);
  inherited;
end;

{********************************************************}
procedure TALRadioButton.SetChecked(const Value: Boolean);
var M: TRadioButtonGroupMessage;
begin
  if FChecked <> Value then begin
    if (csDesigning in ComponentState) and FChecked then FChecked := Value // allows check/uncheck in design-mode
    else begin
      if (not value) and fMandatory then exit;
      FChecked := Value;
      if Value then begin
        M := TRadioButtonGroupMessage.Create(GroupName);
        TMessageManager.DefaultManager.SendMessage(Self, M, True);
      end;
    end;
    DoChanged;
  end;
end;

{*******************************************}
function TALRadioButton.GetGroupName: string;
begin
  Result := FGroupName;
end;

{**********************************************************************************}
procedure TALRadioButton.GroupMessageCall(const Sender: TObject; const M: TMessage);
var LOldMandatory: Boolean;
begin
  if SameText(TRadioButtonGroupMessage(M).GroupName, GroupName) and (Sender <> Self) and (Scene <> nil) and
     (not (Sender is TControl) or ((Sender as TControl).Scene = Scene)) then begin
    LOldMandatory := fMandatory;
    fMandatory := False;
    try
      Checked := False;
    finally
      fMandatory := LOldMandatory;
    end;
  end;
end;

{***********************************************}
function TALRadioButton.GroupNameStored: Boolean;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _GroupNameIsSet(AGroupName: string): Boolean;
  begin
    AGroupName := AGroupName.Trim;
    Result := (not AGroupName.IsEmpty) and (AGroupName <> '0') and (AGroupName <> '-1');
  end;

begin
  Result := _GroupNameIsSet(FGroupName);
end;

{*********************************************************}
procedure TALRadioButton.SetGroupName(const Value: string);
var S: string;
begin
  S := Value.Trim;
  if FGroupName <> S then FGroupName := Value;
end;

{****************************************************}
constructor TALSwitchThumb.Create(AOwner: TComponent);
begin
  inherited;
  Locked := True;
  HitTest := False;
end;

{*********************************************************}
constructor TALSwitchBackground.Create(AOwner: TComponent);
begin
  inherited;
  Locked := True;
  HitTest := False;
end;

{***********************************************}
constructor TALSwitch.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := True;
  SetAcceptsControls(False);
  AutoCapture := True;
  //-----
  FChecked := false;
  FOnChange := nil;
  FOnAnimationProcess := nil;
  FOnAnimationFinish := nil;
  FPressed := false;
  FTracking := false;
  FPressedThumbPos := TpointF.create(0,0);
  FSavedPos := TpointF.create(0,0);
  FThumbSize := 0;
  FThumbRect := GetThumbRectByValue(FChecked);
  fAnimationDuration := DefaultSwitchAnimationDuration;
  //-----
  FAnimation := TALFloatAnimation.Create;
  FAnimation.AnimationType := TAnimationType.In;
  FAnimation.Interpolation := TALInterpolationType.Linear;
  FAnimation.OnProcess := doAnimationProcess;
  FAnimation.OnFinish := DoAnimationEnd;
  FAnimation.Enabled := False;
  //-----
  FBackGround := TALSwitchBackGround.Create(self);
  FBackGround.Parent := self;
  FBackGround.Stored := False;
  FBackGround.SetSubComponent(True);
  FBackGround.Name := 'BackGround';
  FBackGround.Align := TalignLayout.VertCenter;
  FBackGround.Size.Height := 14;
  FBackGround.Margins.DefaultValue := TrectF.Create(6,0,6,0);
  FBackGround.Margins.Left := 6;
  FBackGround.Margins.right := 6;
  FBackGround.XRadius := 7;
  FBackGround.yRadius := 7;
  FBackGround.Stroke.Color := Talphacolors.Null;
  fBackGround.Fill.Color := $ffc5c5c5;
  fBackGround.HitTest := False;
  //-----
  FThumb := TALSwitchThumb.Create(self);
  FThumb.Parent := self;
  FThumb.Stored := False;
  FThumb.SetSubComponent(True);
  FThumb.Name := 'Thumb';
  FThumb.Width := FThumbRect.Width;
  FThumb.height := FThumbRect.height;
  FThumb.XRadius := 11;
  FThumb.yRadius := 11;
  fThumb.Stroke.Color := $ffd5d5d5;
  FThumb.Fill.Color := $ffffffff;
  FThumb.HitTest := False;
end;

{***************************}
destructor TALSwitch.Destroy;
begin
  FAnimation.Enabled := False;
  ALFreeAndNil(FAnimation);
  inherited;
end;

{****************************************}
function TALSwitch.GetDefaultSize: TSizeF;
begin
    Result := TSizeF.Create(44, 22);
end;

{***************************}
procedure TALSwitch.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{*************************}
procedure TALSwitch.Resize;
begin
  inherited Resize;
  fAnimation.Enabled := False;
  FthumbRect := GetThumbRectByValue(FChecked);
  realign;
end;

{******************************************************}
procedure TALSwitch.doAnimationProcess(Sender: TObject);
begin
  FthumbRect.SetLocation(FAnimation.CurrentValue, FthumbRect.Top);
  realign;
  if Assigned(FOnAnimationProcess) then FOnAnimationProcess(Self);
end;

{**************************************************}
procedure TALSwitch.DoAnimationEnd(Sender: TObject);
begin
  FAnimation.Enabled := False;
  if Assigned(FOnAnimationFinish) then FOnAnimationFinish(Self);
end;

{**************************************************}
procedure TALSwitch.AnimateTo(const Value: Boolean);
var ElapsedDistance, AnimationDistance: Single;
    R: TrectF;
begin
  FAnimation.Enabled := False;
  if ([csLoading, csReading, csDestroying, csDesigning] * ComponentState <> []) or
     (Parent = nil) or
     (FDisablePaint) or
     (FUpdating > 0) or
     (not Visible) or
     (not ParentedVisible) then begin
    FThumbRect := GetThumbRectByValue(Value);
    Realign;
  end
  else begin

    FAnimation.StartValue := fThumbRect.left;
    R := GetThumbRectByValue(Value);
    FAnimation.StopValue := R.Left;
    //-----
    AnimationDistance := Width - R.Width;
    ElapsedDistance := Abs(FAnimation.StopValue - FAnimation.StartValue);
    //-----
    if AnimationDistance > 0 then FAnimation.Duration := AnimationDuration * (ElapsedDistance / AnimationDistance)
    else FAnimation.Duration := AnimationDuration;
    //-----
    FAnimation.Start;

  end;
end;

{****************************************}
function TALSwitch.GetThumbCenter: Single;
begin
  Result := (fThumbRect.Left + fThumbRect.Right) / 2;
end;

{**************************************}
function TALSwitch.GetThumbSize: Single;
begin
  if SameValue(FThumbSize, 0.0, Epsilon) then Result := LocalRect.Height
  else Result := FThumbSize;
end;

{*******************************************************************}
function TALSwitch.GetThumbRectByValue(const Value: Boolean): TRectF;
begin
  result := LocalRect;
  if not Value then Result.Right := result.Left + GetThumbSize
  else Result.Left := result.Right - GetThumbSize;
end;

{***************************************}
function TALSwitch.GetThumbValue: Single;
begin
  if fThumbRect.Left > 0 then Result := fThumbRect.Left / (Width - fThumbRect.Width)
  else result := 0;
end;

{*****************************************************************}
function TALSwitch.GetValueByMousePos(const X, Y: Single): Boolean;
var HalfThumbWidth: Single;
begin
  HalfThumbWidth := fThumbRect.Width / 2;
  if (X - FPressedThumbPos.X) + HalfThumbWidth < Width / 2 then Result := False
  else Result := True;
end;

{************************************************************************************}
procedure TALSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (Button = TMouseButton.mbLeft) and (not Fanimation.Running) then begin
    FPressed := True;
    FSavedPos := TPointF.Create(X, Y);
    FPressedThumbPos := FSavedPos - FThumbRect.TopLeft;
  end;
end;

{**************************************************************}
procedure TALSwitch.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FPressed then begin

    if ((ssTouch in Shift) or (ssLeft in Shift)) and (Abs(X - FSavedPos.X) < TrackingSensitivity) then Exit;
    //-----
    FTracking := True;
    //-----
    FThumbRect.Offset(X - FSavedPos.X, 0);
    if FThumbRect.Left < 0 then FThumbRect.Offset(-FThumbRect.Left, 0);
    if FThumbRect.Right > Width then FThumbRect.Offset(-(FThumbRect.Right - Width), 0);
    //-----
    FSavedPos := TPointF.Create(X, Y);
    //-----
    realign;

  end;
end;

{**********************************************************************************}
procedure TALSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var LChecked: Boolean;
begin
  inherited;
  if FPressed then begin
    FPressed := False;
    if not FTracking then begin
      LChecked := not FChecked;
      AnimateTo(LChecked);
      SetChecked(LChecked);
    end
    else begin
      LChecked := GetValueByMousePos(X, Y);
      AnimateTo(LChecked);
      SetChecked(LChecked);
    end;
    FTracking := False;
  end;
end;

{****************************}
procedure TALSwitch.DoRealign;
begin
  inherited;
  if FThumb <> nil then FThumb.BoundsRect := fThumbRect;
end;

{**************************************************}
function TALSwitch.AnimationDurationStored: Boolean;
begin
  result := not sameValue(AnimationDuration, DefaultSwitchAnimationDuration, epsilon);
end;

{****************************************************}
procedure TALSwitch.SetThumbSize(const Value: Single);
begin
  fAnimation.Enabled := False;
  FThumbSize := Value;
  FthumbRect := GetThumbRectByValue(FChecked);
  realign;
end;

{***************************************************}
procedure TALSwitch.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then begin
    FChecked := Value;
    if not Fanimation.Running then begin
      FThumbRect := GetThumbRectByValue(FChecked);
      realign;
    end;
    DoChange;
  end;
end;

{****************************************************************}
procedure TALSwitch.SetCheckedWithAnimation(const Value: Boolean);
begin
  if FChecked <> Value then begin
    AnimateTo(FChecked);
    SetChecked(FChecked);
  end;
end;

{***********************************}
constructor TALButton.TBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FDefaultText := '';
  FText := FDefaultText;
  //--
  if StateStyleParent <> nil then FTextSettings := TStateStyleTextSettings.Create(StateStyleParent.TextSettings)
  else if ControlParent <> nil then FTextSettings := TStateStyleTextSettings.Create(ControlParent.TextSettings)
  else FTextSettings := TStateStyleTextSettings.Create(nil);
  //--
  Fill.DefaultColor := $FFE1E1E1;
  Fill.Color := Fill.DefaultColor;
  //--
  Stroke.DefaultColor := $FFADADAD;
  Stroke.Color := Stroke.DefaultColor;
  //--
  FTextSettings.OnChanged := TextSettingsChanged;
  //--
  //FPriorSupersedeText
end;

{*************************************}
destructor TALButton.TBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FTextSettings);
  inherited Destroy;
end;

{******************************************************}
procedure TALButton.TBaseStateStyle.Assign(Source: TPersistent);
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

{******************************}
procedure TALButton.TBaseStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Text := DefaultText;
    TextSettings.reset;
  finally
    EndUpdate;
  end;
end;

{******************************************************}
procedure TALButton.TBaseStateStyle.Interpolate(const ATo: TBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  try
    Inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then begin
      Text := ATo.Text;
      TextSettings.Interpolate(ATo.TextSettings, ANormalizedTime);
    end
    else if StateStyleParent <> nil then begin
      Text := StateStyleParent.Text;
      TextSettings.Interpolate(StateStyleParent.TextSettings, ANormalizedTime);
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

{******************************************************}
procedure TALButton.TBaseStateStyle.InterpolateNoChanges(const ATo: TBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{******************}
procedure TALButton.TBaseStateStyle.DoSupersede;
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

{*********************************************************}
function TALButton.TBaseStateStyle.GetStateStyleParent: TBaseStateStyle;
begin
  {$IF defined(debug)}
  if (inherited StateStyleParent <> nil) and
     (not (inherited StateStyleParent is TBaseStateStyle)) then
    raise Exception.Create('StateStyleParent must be of type TBaseStateStyle');
  {$ENDIF}
  Result := TBaseStateStyle(inherited StateStyleParent);
end;

{*********************************************************}
function TALButton.TBaseStateStyle.GetControlParent: TALButton;
begin
  {$IF defined(debug)}
  if (inherited ControlParent <> nil) and
     (not (inherited ControlParent is TALButton)) then
    raise Exception.Create('ControlParent must be of type TALButton');
  {$ENDIF}
  Result := TALButton(inherited ControlParent);
end;

{*********************************************************}
procedure TALButton.TBaseStateStyle.SetText(const Value: string);
begin
  if FText <> Value then begin
    FText := Value;
    Change;
  end;
end;

{*******************************************************************************************}
procedure TALButton.TBaseStateStyle.SetTextSettings(const AValue: TStateStyleTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

{***********************************************}
function TALButton.TBaseStateStyle.GetInherit: Boolean;
begin
  Result := inherited GetInherit and
            Text.IsEmpty and
            TextSettings.Inherit;
end;

{******************************************************************}
procedure TALButton.TBaseStateStyle.TextSettingsChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
function TALButton.TBaseStateStyle.IsTextStored: Boolean;
begin
  Result := FText <> FDefaultText;
end;

{**********************************************************}
function TALButton.TDisabledStateStyle.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{********************************************************************}
procedure TALButton.TDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*********************************************}
constructor TALButton.TDisabledStateStyle.Create(const AParent: TObject);
begin
  inherited Create(AParent);
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{****************************************************************}
procedure TALButton.TDisabledStateStyle.Assign(Source: TPersistent);
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

{******************************}
procedure TALButton.TDisabledStateStyle.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Opacity := TControl.DefaultDisabledOpacity;
  finally
    EndUpdate;
  end;
end;

{******************************}
function TALButton.TDisabledStateStyle.GetInherit: Boolean;
begin
  // Opacity is not part of the GetInherit function because it updates the
  // disabledOpacity of the base control immediately every time it changes.
  // Essentially, it acts merely as a link to the disabledOpacity of the base control.
  Result := inherited GetInherit;
end;

{*************************************}
constructor TALButton.TStateStyles.Create(const AParent: TALButton);
begin
  inherited Create;
  //--
  FDisabled := TDisabledStateStyle.Create(AParent);
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := THoveredStateStyle.Create(AParent);
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := TPressedStateStyle.Create(AParent);
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := TFocusedStateStyle.Create(AParent);
  FFocused.OnChanged := FocusedChanged;
end;

{*************************************}
destructor TALButton.TStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{*********************************}
function TALButton.TStateStyles.CreateSavedState: TALPersistentObserver;
type
  TALButtonStateStylesClass = class of TStateStyles;
begin
  result := TALButtonStateStylesClass(classtype).Create(nil{AParent});
end;

{******************************************************}
procedure TALButton.TStateStyles.Assign(Source: TPersistent);
begin
  if Source is TStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TStateStyles(Source).Disabled);
      Hovered.Assign(TStateStyles(Source).Hovered);
      Pressed.Assign(TStateStyles(Source).Pressed);
      Focused.Assign(TStateStyles(Source).Focused);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{******************************}
procedure TALButton.TStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Disabled.reset;
    Hovered.reset;
    Pressed.reset;
    Focused.reset;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************}
procedure TALButton.TStateStyles.SetDisabled(const AValue: TDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{************************************************************************************}
procedure TALButton.TStateStyles.SetHovered(const AValue: THoveredStateStyle);
begin
  FHovered.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALButton.TStateStyles.SetPressed(const AValue: TPressedStateStyle);
begin
  FPressed.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALButton.TStateStyles.SetFocused(const AValue: TFocusedStateStyle);
begin
  FFocused.Assign(AValue);
end;

{**********************************************************}
procedure TALButton.TStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************}
procedure TALButton.TStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALButton.TStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALButton.TStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{***********************************************}
constructor TALButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //--
  CanFocus := True;
  HitTest := True;
  AutoSize := True;
  Cursor := crHandPoint;
  //--
  Fill.DefaultColor := $ffe1e1e1;
  Fill.Color := Fill.DefaultColor;
  //--
  Stroke.DefaultColor := $ffadadad;
  Stroke.Color := Stroke.DefaultColor;
  //--
  TextSettings.Font.DefaultWeight := TFontWeight.medium;
  TextSettings.Font.Weight := TextSettings.Font.DefaultWeight;
  TextSettings.DefaultHorzAlign := TALTextHorzAlign.center;
  TextSettings.HorzAlign := TextSettings.DefaultHorzAlign;
  //--
  Padding.DefaultValue := TRectF.create(12{Left}, 6{Top}, 12{Right}, 6{Bottom});
  Padding.Rect := Padding.DefaultValue;
  //--
  FStateStyles := TStateStyles.Create(self);
  FStateStyles.OnChanged := StateStylesChanged;
  //--
  FStateTransitionAnimation := TALFloatAnimation.Create;
  FStateTransitionAnimation.OnProcess := StateTransitionAnimationProcess;
  FStateTransitionAnimation.OnFinish := StateTransitionAnimationFinish;
  //--
  FStateTransitionFrom := nil;
  FStateTransitionTo := nil;
  FCurrentStateStyle := nil;
  //--
  fBufDisabledDrawable := ALNullDrawable;
  fBufHoveredDrawable := ALNullDrawable;
  fBufPressedDrawable := ALNullDrawable;
  fBufFocusedDrawable := ALNullDrawable;
end;

{***************************}
destructor TALButton.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  ALFreeAndNil(FStateTransitionAnimation);
  inherited Destroy;
end;

{*************************}
procedure TALButton.Loaded;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ConvertFontFamily(const AStateStyle: TBaseStateStyle);
  begin
    if (AStateStyle.TextSettings.Font.AutoConvert) and
       (AStateStyle.TextSettings.Font.Family <> '') and
       (not (csDesigning in ComponentState)) then
      AStateStyle.TextSettings.Font.Family := ALConvertFontFamily(AStateStyle.TextSettings.Font.Family);
  end;

begin
  _ConvertFontFamily(StateStyles.Disabled);
  _ConvertFontFamily(StateStyles.Hovered);
  _ConvertFontFamily(StateStyles.Pressed);
  _ConvertFontFamily(StateStyles.Focused);
  inherited Loaded;
end;

{*********************************************************}
function TALButton.CreateTextSettings: TALBaseTextSettings;
begin
  Result := TTextSettings.Create;
end;

{********************************************************}
function TALButton.GetTextSettings: TTextSettings;
begin
  Result := TTextSettings(Inherited TextSettings);
end;

{**********************************************************************}
procedure TALButton.SetTextSettings(const Value: TTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{*******************************************************}
procedure TALButton.SetName(const Value: TComponentName);
begin
  var LChangeText := not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (csLoading in TComponent(Owner).ComponentState));
  inherited SetName(Value);
  if LChangeText then
    Text := Value;
end;

{*********************************************************************}
procedure TALButton.SetStateStyles(const AValue: TStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{*******************************************************}
function TALButton.GetCurrentStateStyle: TBaseStateStyle;
begin
  if Not Enabled then Result := StateStyles.Disabled
  else if Pressed then Result := StateStyles.Pressed
  else if IsFocused then Result := StateStyles.Focused
  else if IsMouseOver then Result := StateStyles.Hovered
  else result := nil;
end;

{******************************************************}
procedure TALButton.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
  Repaint;
end;

{**************************************************}
procedure TALButton.IsMouseOverChanged;
begin
  inherited;
  startStateTransition;
  repaint;
end;

{**************************************************}
procedure TALButton.PressedChanged;
begin
  inherited;
  startStateTransition;
  repaint;
end;

{**************************************************}
procedure TALButton.startStateTransition;
begin
  var LPrevStateTransitionAnimationFrom := FStateTransitionFrom;
  var LPrevStateTransitionAnimationTo := FStateTransitionTo;
  FStateTransitionFrom := FStateTransitionTo;
  FStateTransitionTo := GetCurrentStateStyle;
  //--
  var LDuration: Single;
  var LanimationType: TAnimationType;
  var LInterpolation: TALInterpolationType;
  if FStateTransitionTo <> nil then begin
    LDuration := FStateTransitionTo.Transition.Duration;
    LanimationType := FStateTransitionTo.Transition.animationType;
    LInterpolation := FStateTransitionTo.Transition.Interpolation;
  end
  else if FStateTransitionFrom <> nil then begin
    LDuration := FStateTransitionFrom.Transition.Duration;
    LanimationType := FStateTransitionFrom.Transition.animationType;
    LInterpolation := FStateTransitionFrom.Transition.Interpolation;
  end
  else
    Raise Exception.Create('Error #B2B17EF6-4F6A-4CBF-B1D6-C880B70D2141');
  //--
  if FStateTransitionAnimation.Enabled then begin
    FStateTransitionAnimation.StopAtCurrent;
    FStateTransitionAnimation.Enabled := False;
    if (LPrevStateTransitionAnimationFrom = FStateTransitionTo) and
       (LPrevStateTransitionAnimationTo = FStateTransitionFrom) then begin
      FStateTransitionAnimation.StartValue := 1-FStateTransitionAnimation.CurrentValue;
      FStateTransitionAnimation.StopValue := 1;
      if FStateTransitionTo <> nil then
        FStateTransitionAnimation.Duration := FStateTransitionTo.Transition.Duration * FStateTransitionAnimation.CurrentValue
      else if FStateTransitionFrom <> nil then
        FStateTransitionAnimation.Duration := FStateTransitionFrom.Transition.Duration * FStateTransitionAnimation.CurrentValue
      else
        Raise Exception.Create('Error #7AF71AE4-115F-4F8C-AA36-D7BC3B246759');
      FStateTransitionAnimation.Start;
    end;
    exit;
  end;
  //--
  if SameValue(LDuration,0.0,TEpsilon.Scale) then
    Exit;
  //--
  FStateTransitionAnimation.StartValue := 0;
  FStateTransitionAnimation.StopValue := 1;
  FStateTransitionAnimation.Duration := LDuration;
  FStateTransitionAnimation.AnimationType := LAnimationType;
  FStateTransitionAnimation.Interpolation := LInterpolation;
  FStateTransitionAnimation.Start;
end;

{**************************************************}
procedure TALButton.StateTransitionAnimationProcess(Sender: TObject);
begin
  Repaint;
end;

{**************************************************}
procedure TALButton.StateTransitionAnimationFinish(Sender: TObject);
begin
  FStateTransitionAnimation.Enabled := False;
  Repaint;
end;

{***********************************}
procedure TALButton.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (ALIsDrawableNull(BufDrawable)) and // warn will be raise in inherited
     ((not ALIsDrawableNull(fBufDisabledDrawable)) or
      (not ALIsDrawableNull(fBufHoveredDrawable)) or
      (not ALIsDrawableNull(fBufPressedDrawable)) or
      (not ALIsDrawableNull(fBufFocusedDrawable))) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  inherited clearBufDrawable;
  ALFreeAndNilDrawable(fBufDisabledDrawable);
  ALFreeAndNilDrawable(fBufHoveredDrawable);
  ALFreeAndNilDrawable(fBufPressedDrawable);
  ALFreeAndNilDrawable(fBufFocusedDrawable);
end;

{**********************************}
procedure TALButton.MakeBufDrawable;

  {~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _MakeBufDrawable(
              const AStateStyle: TBaseStateStyle;
              var ABufDrawable: TALDrawable;
              var ABufDrawableRect: TRectF);
  begin
    if AStateStyle.Inherit then exit;
    if (not ALIsDrawableNull(ABufDrawable)) then exit;
    FCurrentStateStyle := AStateStyle;
    AStateStyle.SupersedeNoChanges(true{ASaveState});
    try
      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;
      var LScale: Single;
      if Abs(AStateStyle.Scale.x - 1) > Abs(AStateStyle.Scale.y - 1) then
        LScale := AStateStyle.Scale.x
      else
        LScale := AStateStyle.Scale.y;
      CreateBufDrawable(
        ABufDrawable, // var ABufDrawable: TALDrawable;
        ABufDrawableRect, // var ABufDrawableRect: TRectF;
        LTextBroken, // var ABufTextBroken: Boolean;
        LAllTextDrawn, // var ABufAllTextDrawn: Boolean;
        LElements, // var ABufElements: TALTextElements;
        ALGetScreenScale * LScale, // const AScale: Single;
        AStateStyle.Text, // const AText: String;
        AStateStyle.TextSettings.Font, // const AFont: TALFont;
        AStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
        AStateStyle.TextSettings.Font, // const AEllipsisFont: TALFont;
        AStateStyle.TextSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
        AStateStyle.Fill, // const AFill: TALBrush;
        AStateStyle.Stroke, // const AStroke: TALStrokeBrush;
        AStateStyle.Shadow); // const AShadow: TALShadow);

      // The shadow effect is not included in the fBufDrawableRect rectangle's dimensions.
      // However, the fBufDrawableRect rectangle is offset by the shadow's dx and dy values,
      // if a shadow is applied, to adjust for the visual shift caused by the shadow.
      var LMainDrawableRect := BufDrawableRect;
      LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
      var LCenteredRect := ABufDrawableRect.CenterAt(LMainDrawableRect);
      ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);
      ABufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);
    finally
      AStateStyle.RestorestateNoChanges;
      FCurrentStateStyle := nil;
    end;
  end;

begin
  //--- Do not create BufDrawable if not DoubleBuffered
  if {$IF not DEFINED(ALDPK)}(not DoubleBuffered){$ELSE}False{$ENDIF} then begin
    clearBufDrawable;
    exit;
  end;
  //--
  inherited MakeBufDrawable;
  //--
  if Not Enabled then begin
    _MakeBufDrawable(
      StateStyles.Disabled, // const AStateStyle: TALButtonStateStyle;
      FBufDisabledDrawable, // var ABufDrawable: TALDrawable;
      FBufDisabledDrawableRect); // var ABufDrawableRect: TRectF;
  end
  else if Pressed then begin
    _MakeBufDrawable(
      StateStyles.Pressed, // const AStateStyle: TALButtonStateStyle;
      FBufPressedDrawable, // var ABufDrawable: TALDrawable;
      FBufPressedDrawableRect); // var ABufDrawableRect: TRectF;
  end
  else if IsFocused then begin
    _MakeBufDrawable(
      StateStyles.Focused, // const AStateStyle: TALButtonStateStyle;
      FBufFocusedDrawable, // var ABufDrawable: TALDrawable;
      FBufFocusedDrawableRect); // var ABufDrawableRect: TRectF;
  end
  else if IsMouseOver then begin
    _MakeBufDrawable(
      StateStyles.Hovered, // const AStateStyle: TALButtonStateStyle;
      FBufHoveredDrawable, // var ABufDrawable: TALDrawable;
      FBufHoveredDrawableRect); // var ABufDrawableRect: TRectF;
  end;
end;

{************************}
Procedure TALButton.DrawMultilineTextAdjustRect(const ACanvas: TALCanvas; var ARect: TrectF; var ASurfaceSize: TSizeF);
begin
  if (ALIsCanvasNull(ACanvas)) and (FCurrentStateStyle <> nil) and (FCurrentStateStyle.StateLayer.HasFill) then begin
    var LRect := ARect;
    var LPaddingRect := FCurrentStateStyle.StateLayer.Padding.Rect;
    LRect.Inflate(-LPaddingRect.Left{DL}, -LPaddingRect.top{DT}, -LPaddingRect.Right{DR}, -LPaddingRect.Bottom{DB});
    var LSurfaceRect := TRectF.Union(TRectF.Create(0, 0, ASurfaceSize.Width, ASurfaceSize.Height), LRect);
    ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.top);
    ASurfaceSize := LSurfaceRect.Size;
  end;
  //--
  If not ALIsCanvasNull(ACanvas) then
    ARect := ARect.CenterAt(LocalRect);
end;

{************************}
Procedure TALButton.DrawMultilineTextBeforeDrawParagraph(const ACanvas: TALCanvas; Const ARect: TrectF);
begin
  if (FCurrentStateStyle = nil) or (not FCurrentStateStyle.StateLayer.HasFill) then
    exit;
  {$IF defined(DEBUG)}
  if not FCurrentStateStyle.Superseded then
    Raise Exception.Create('Error #828F2B09-D501-41BB-99D3-A06A467CC3D9');
  {$ENDIF}
  var LFillColor: TalphaColor;
  if FCurrentStateStyle.StateLayer.UseContentColor then LFillColor := FCurrentStateStyle.TextSettings.Font.Color
  else LFillColor := FCurrentStateStyle.StateLayer.Color;
  ALDrawRectangle(
    ACanvas, // const ACanvas: TALCanvas;
    1, // const AScale: Single;
    ARect, // const ADstRect: TrectF;
    FCurrentStateStyle.StateLayer.Opacity, // const AOpacity: Single;
    LFillColor, // const AFillColor: TAlphaColor;
    FCurrentStateStyle.StateLayer.Gradient.Style, // const AFillGradientStyle: TGradientStyle;
    FCurrentStateStyle.StateLayer.Gradient.Colors, // const AFillGradientColors: TArray<TAlphaColor>;
    FCurrentStateStyle.StateLayer.Gradient.Offsets, // const AFillGradientOffsets: TArray<Single>;
    FCurrentStateStyle.StateLayer.Gradient.Angle, // const AFillGradientAngle: Single;
    FCurrentStateStyle.StateLayer.ResourceName, // const AFillResourceName: String;
    FCurrentStateStyle.StateLayer.WrapMode, // Const AFillWrapMode: TALImageWrapMode;
    FCurrentStateStyle.StateLayer.Padding.Rect, // Const AFillPaddingRect: TRectF;
    TAlphaColors.Null, // const AStrokeColor: TalphaColor;
    0, // const AStrokeThickness: Single;
    TAlphaColors.Null, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, the Canvas should have adequate space to accommodate the shadow. You can use the ALGetShadowWidth function to estimate the required width.
    0, // const AShadowBlur: Single;
    0, // const AShadowOffsetX: Single;
    0, // const AShadowOffsetY: Single;
    AllSides, // const ASides: TSides;
    AllCorners, // const ACorners: TCorners;
    FCurrentStateStyle.StateLayer.XRadius, // const AXRadius: Single;
    FCurrentStateStyle.StateLayer.YRadius); // const AYRadius: Single)
end;

{************************}
procedure TALButton.Paint;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _DrawMultilineText(const AFromStateStyle: TBaseStateStyle; const AToStateStyle: TBaseStateStyle);
  begin
    {$IF DEFINED(ALSkiaCanvas)}
    var LStateStyle := AFromStateStyle;
    if LStateStyle = nil then LStateStyle := AToStateStyle;
    if LStateStyle = nil then
      raise Exception.Create('Error 45CB6D22-AB78-4857-B03F-1636E5184C12');
    FCurrentStateStyle := LStateStyle;
    LStateStyle.SupersedeNoChanges(true{ASaveState});
    try
      if FStateTransitionAnimation.Enabled then begin
        if AFromStateStyle = nil then LStateStyle{AToStateStyle}.InterpolateNoChanges(nil{AFromStateStyle}, 1-FStateTransitionAnimation.CurrentValue)
        else if AToStateStyle = nil then LStateStyle{AFromStateStyle}.InterpolateNoChanges(nil{AToStateStyle}, FStateTransitionAnimation.CurrentValue)
        else begin
          AToStateStyle.SupersedeNoChanges(true{ASaveState});
          try
            LStateStyle{AFromStateStyle}.InterpolateNoChanges(AToStateStyle{AToStateStyle}, FStateTransitionAnimation.CurrentValue);
          finally
            AToStateStyle.RestorestateNoChanges;
          end;
        end;
      end;
      // Using a matrix on the canvas results in smoother animations compared to using
      // Ascale with DrawMultilineText. This is because changes in scale affect the font size,
      // leading to rounding issues (I spent many hours looking for a way to avoid this).
      // If there is an animation, it appears jerky because the text position
      // shifts up or down with scale changes due to pixel alignment.
      var LCanvasSaveState: TCanvasSaveState := nil;
      if Not LStateStyle.Scale.Inherit then begin
        LCanvasSaveState := Canvas.SaveState;
        Var LAbsoluteRect := AbsoluteRect;
        var LMatrix := TMatrix.CreateTranslation(-LAbsoluteRect.Left, -LAbsoluteRect.Top);
        LMatrix := LMatrix * TMatrix.CreateScaling(LStateStyle.Scale.x, LStateStyle.Scale.y);
        LMatrix := LMatrix * TMatrix.CreateTranslation(
                               LAbsoluteRect.Left - (((LAbsoluteRect.Width * LStateStyle.Scale.x) - LAbsoluteRect.Width) / 2),
                               LAbsoluteRect.Top - (((LAbsoluteRect.height * LStateStyle.Scale.y) - LAbsoluteRect.Height) / 2));
        Canvas.SetMatrix(Canvas.Matrix * LMatrix);
      end;
      try
        var LTextBroken: Boolean;
        var LAllTextDrawn: Boolean;
        var LElements: TALTextElements;
        DrawMultilineText(
          LTextBroken, // out ATextBroken: Boolean;
          LAllTextDrawn, // out AAllTextDrawn: Boolean;
          LElements, // out AElements: TALTextElements;
          1{Ascale},
          LStateStyle.Text, // const AText: String;
          LStateStyle.TextSettings.Font, // const AFont: TALFont;
          LStateStyle.TextSettings.Decoration, // const ADecoration: TALTextDecoration;
          LStateStyle.TextSettings.EllipsisSettings.font, // const AEllipsisFont: TALFont;
          LStateStyle.TextSettings.EllipsisSettings.Decoration, // const AEllipsisDecoration: TALTextDecoration;
          LStateStyle.Fill, // const AFill: TALBrush;
          LStateStyle.Stroke, // const AStroke: TALStrokeBrush;
          LStateStyle.Shadow); // const AShadow: TALShadow);
      finally
        if Not LStateStyle.Scale.Inherit then
          Canvas.RestoreState(LCanvasSaveState);
      end;
    finally
      LStateStyle.RestorestateNoChanges;
      FCurrentStateStyle := nil;
    end;
    {$ELSE}
    {$IF defined(DEBUG)}
    if not doublebuffered then
      raise Exception.Create('Controls that are not double-buffered only work when SKIA is enabled.');
    {$ENDIF}
    {$ENDIF}
  end;

begin

  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;

  var LStateStyle := GetCurrentStateStyle;

  If FStateTransitionAnimation.Enabled then begin
    LDrawable := ALNullDrawable;
    LDrawableRect := TRectF.Empty;
  end
  else if Not Enabled then begin
    LDrawable := fBufDisabledDrawable;
    LDrawableRect := fBufDisabledDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
    end;
  end
  //--
  else if Pressed then begin
    LDrawable := fBufPressedDrawable;
    LDrawableRect := fBufPressedDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
    end;
  end
  //--
  else if IsFocused then begin
    LDrawable := fBufFocusedDrawable;
    LDrawableRect := fBufFocusedDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
    end;
  end
  //--
  else if IsMouseOver then begin
    LDrawable := fBufHoveredDrawable;
    LDrawableRect := fBufHoveredDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
    end;
  end
  //--
  else begin
    LDrawable := BufDrawable;
    LDrawableRect := BufDrawableRect;
  end;

  if ALIsDrawableNull(LDrawable) then begin
    if FStateTransitionAnimation.Enabled then _DrawMultilineText(FStateTransitionFrom, FStateTransitionTo)
    else if Not Enabled then _DrawMultilineText(StateStyles.Disabled, StateStyles.Disabled)
    else if Pressed then _DrawMultilineText(StateStyles.Pressed, StateStyles.Pressed)
    else if IsFocused then _DrawMultilineText(StateStyles.Focused, StateStyles.Focused)
    else if IsMouseOver then _DrawMultilineText(StateStyles.Hovered, StateStyles.Hovered)
    else inherited Paint;
    exit;
  end;

  if (LStateStyle <> nil) and (not LStateStyle.Scale.Inherit) then begin
    var LDstRect := LDrawableRect;
    LDstRect.Width := LDstRect.Width * LStateStyle.Scale.x;
    LDstRect.Height := LDstRect.Height * LStateStyle.Scale.y;
    LDstRect := LdstRect.CenterAt(LDrawableRect);
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      LDrawable, // const ADrawable: TALDrawable;
      LDstRect.TopLeft, // const ADstRect: TrectF; // IN Virtual pixels !
      AbsoluteOpacity); // const AOpacity: Single);
  end
  else begin
    ALDrawDrawable(
      Canvas, // const ACanvas: Tcanvas;
      LDrawable, // const ADrawable: TALDrawable;
      LDrawableRect.TopLeft, // const ATopLeft: TpointF;
      AbsoluteOpacity); // const AOpacity: Single);
  end;

end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALAniIndicator, TALScrollBar, TALTrackBar, TALRangeTrackBar, TALCheckBox, TALRadioButton, TALSwitch, TALButton]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALAniIndicator, 'Size');
  UnlistPublishedProperty(TALAniIndicator, 'StyleName');
  UnlistPublishedProperty(TALAniIndicator, 'OnTap');
  //--
  UnlistPublishedProperty(TALScrollBar, 'Size');
  UnlistPublishedProperty(TALScrollBar, 'StyleName');
  UnlistPublishedProperty(TALScrollBar, 'OnTap');
  //--
  UnlistPublishedProperty(TALTrackBar, 'Size');
  UnlistPublishedProperty(TALTrackBar, 'StyleName');
  UnlistPublishedProperty(TALTrackBar, 'OnTap');
  //--
  UnlistPublishedProperty(TALRangeTrackBar, 'Size');
  UnlistPublishedProperty(TALRangeTrackBar, 'StyleName');
  UnlistPublishedProperty(TALRangeTrackBar, 'OnTap');
  //--
  UnlistPublishedProperty(TALCheckBox, 'Size');
  UnlistPublishedProperty(TALCheckBox, 'StyleName');
  UnlistPublishedProperty(TALCheckBox, 'OnTap');
  //--
  UnlistPublishedProperty(TALRadioButton, 'Size');
  UnlistPublishedProperty(TALRadioButton, 'StyleName');
  UnlistPublishedProperty(TALRadioButton, 'OnTap');
  //--
  UnlistPublishedProperty(TALSwitch, 'Size');
  UnlistPublishedProperty(TALSwitch, 'StyleName');
  UnlistPublishedProperty(TALSwitch, 'OnTap');
  //--
  UnlistPublishedProperty(TALButton, 'Size');
  UnlistPublishedProperty(TALButton, 'StyleName');
  UnlistPublishedProperty(TALButton, 'OnTap');
  //--
  UnlistPublishedProperty(TALTrackThumbGlyph, 'Size');
  UnlistPublishedProperty(TALTrackThumbGlyph, 'StyleName');
  UnlistPublishedProperty(TALTrackThumbGlyph, 'OnTap');
  UnlistPublishedProperty(TALTrackThumbGlyph, 'Locked');
  //--
  UnlistPublishedProperty(TALTrackThumb, 'Size');
  UnlistPublishedProperty(TALTrackThumb, 'StyleName');
  UnlistPublishedProperty(TALTrackThumb, 'OnTap');
  UnlistPublishedProperty(TALTrackThumb, 'Locked');
  UnlistPublishedProperty(TALTrackThumb, 'Anchors'); // not work https://quality.embarcadero.com/browse/RSP-15684
  UnlistPublishedProperty(TALTrackThumb, 'Align');
  UnlistPublishedProperty(TALTrackThumb, 'Position');
  UnlistPublishedProperty(TALTrackThumb, 'PopupMenu');
  UnlistPublishedProperty(TALTrackThumb, 'DragMode');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragEnd');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragEnter');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragLeave');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragOver');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragDrop');
  UnlistPublishedProperty(TALTrackThumb, 'EnableDragHighlight');
  //--
  UnlistPublishedProperty(TALTrackBackground, 'Size');
  UnlistPublishedProperty(TALTrackBackground, 'StyleName');
  UnlistPublishedProperty(TALTrackBackground, 'OnTap');
  UnlistPublishedProperty(TALTrackBackground, 'Locked');
  UnlistPublishedProperty(TALTrackBackground, 'PopupMenu');
  UnlistPublishedProperty(TALTrackBackground, 'DragMode');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragEnd');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragEnter');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragLeave');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragOver');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragDrop');
  UnlistPublishedProperty(TALTrackBackground, 'EnableDragHighlight');
  //--
  UnlistPublishedProperty(TALTrackHighlight, 'Size');
  UnlistPublishedProperty(TALTrackHighlight, 'StyleName');
  UnlistPublishedProperty(TALTrackHighlight, 'OnTap');
  UnlistPublishedProperty(TALTrackHighlight, 'Locked');
  UnlistPublishedProperty(TALTrackHighlight, 'Anchors'); // not work https://quality.embarcadero.com/browse/RSP-15684
  UnlistPublishedProperty(TALTrackHighlight, 'Align');
  UnlistPublishedProperty(TALTrackHighlight, 'Position');
  UnlistPublishedProperty(TALTrackHighlight, 'PopupMenu');
  UnlistPublishedProperty(TALTrackHighlight, 'DragMode');
  UnlistPublishedProperty(TALTrackHighlight, 'OnDragEnd');
  UnlistPublishedProperty(TALTrackHighlight, 'OnDragEnter');
  UnlistPublishedProperty(TALTrackHighlight, 'OnDragLeave');
  UnlistPublishedProperty(TALTrackHighlight, 'OnDragOver');
  UnlistPublishedProperty(TALTrackHighlight, 'OnDragDrop');
  UnlistPublishedProperty(TALTrackHighlight, 'EnableDragHighlight');
  //--
  UnlistPublishedProperty(TALSwitchThumb, 'Size');
  UnlistPublishedProperty(TALSwitchThumb, 'StyleName');
  UnlistPublishedProperty(TALSwitchThumb, 'OnTap');
  UnlistPublishedProperty(TALSwitchThumb, 'Locked');
  UnlistPublishedProperty(TALSwitchThumb, 'Anchors'); // not work https://quality.embarcadero.com/browse/RSP-15684
  UnlistPublishedProperty(TALSwitchThumb, 'Align');
  UnlistPublishedProperty(TALSwitchThumb, 'Position');
  UnlistPublishedProperty(TALSwitchThumb, 'PopupMenu');
  UnlistPublishedProperty(TALSwitchThumb, 'DragMode');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragEnd');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragEnter');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragLeave');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragOver');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragDrop');
  UnlistPublishedProperty(TALSwitchThumb, 'EnableDragHighlight');
  //--
  UnlistPublishedProperty(TALSwitchBackground, 'Size');
  UnlistPublishedProperty(TALSwitchBackground, 'StyleName');
  UnlistPublishedProperty(TALSwitchBackground, 'OnTap');
  UnlistPublishedProperty(TALSwitchBackground, 'Locked');
  UnlistPublishedProperty(TALSwitchBackground, 'PopupMenu');
  UnlistPublishedProperty(TALSwitchBackground, 'DragMode');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragEnd');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragEnter');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragLeave');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragOver');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragDrop');
  UnlistPublishedProperty(TALSwitchBackground, 'EnableDragHighlight');
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALAniIndicator, TALScrollBar, TALTrackBar, TALRangeTrackBar, TALCheckBox, TALRadioButton, TALSwitch, TALButton]);

end.
