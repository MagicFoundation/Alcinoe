unit ALFmxStdCtrls;

{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.StdCtrls.pas was not updated and adjust the IFDEF'}
{$ENDIF}

interface

uses System.Classes,
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
     ALFmxAni,
     ALFmxInertialMovement,
     ALFmxObjects;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALAniIndicator = class(Tcontrol)
  private
    fTimer: TTimer;
    finterval: integer;
    FFrameCount: Integer;
    FRowCount: Integer;
    fResourceName: String;
    fFrameIndex: TSmallPoint;
    FScreenScale: single;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure setResourceName(const Value: String);
    procedure onTimer(sender: Tobject);
    function ResourceNameStored: Boolean;
  protected
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
    function EnabledStored: Boolean; override;
    procedure SetEnabled(const Value: Boolean); override;
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
  published
    property Align;
    property Anchors;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default False;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    property FrameCount: Integer read FFrameCount write FFrameCount default 20;
    property RowCount: Integer read FRowCount write FRowCount default 4;
    property interval: integer read finterval write finterval default 50;
    property ResourceName: String read fResourceName write setResourceName stored ResourceNameStored;
    property ParentShowHint;
    property ShowHint;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  TALCustomTrack = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackThumbGlyph = class(TALImage)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align default TalignLayout.Client;
    property Locked default True;
    property HitTest default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackThumb = class(TALRectangle)
  private
    [Weak] fValueRange: TValueRange;
    [Weak] FTrack: TALCustomTrack;
    [Weak] FGlyph: TALTrackThumbGlyph;
    FDownOffset: TPointF;
    fTrackDownOffset: single;
    FPressed: Boolean;
    FDeadZoneBeforeAcquireScrolling: Integer;
    fScrollingAcquiredByMe: boolean;
    fScrollingAcquiredByOther: boolean;
    fScrollingAcquiredByOtherMessageID: integer;
    procedure setScrollingAcquiredByMe(const Value: boolean);
    procedure ScrollingAcquiredByOtherHandler(const Sender: TObject; const M: TMessage);
    function PointToValue(X, Y: Single): Single;
  public
    constructor Create(const ATrack: TALCustomTrack; const aValueRange: TValueRange; const aWithGlyphObj: boolean); reintroduce;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    function GetDefaultTouchTargetExpansion: TRectF; override;
    property IsPressed: Boolean read FPressed;
    property DeadZoneBeforeAcquireScrolling: Integer read FDeadZoneBeforeAcquireScrolling write FDeadZoneBeforeAcquireScrolling default 5;
  published
    property TouchTargetExpansion;
    property Locked default True;
    property Position stored false;
    property Size stored false;
    property Glyph: TALTrackThumbGlyph read FGlyph;
    property Cursor default crHandPoint;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackBackground = class(TALRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked default True;
    property HitTest default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackHighlight = class(TALRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked default True;
    property Position stored false;
    property HitTest default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCustomTrack = class(TControl, IValueRange)
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
    [Weak] FThumb: TALTrackThumb;
    [Weak] FBackGround: TALTrackBackground;
    [Weak] FHighlight: TALTrackHighlight;
    procedure SetViewportSize(const Value: Single); virtual;
    function GetViewportSize: Single; virtual;
    function GetFrequency: Single; virtual;
    procedure SetFrequency(const Value: Single); virtual;
    function GetMax: Single; virtual;
    procedure SetMax(const Value: Single); virtual;
    function GetMin: Single; virtual;
    procedure SetMin(const Value: Single); virtual;
    function GetValue: Single; virtual;
    procedure SetValue(Value: Single); virtual;
    function ValueStored: Boolean; virtual;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure SetOrientation(const Value: TOrientation); virtual;
    function GetThumbRect(const Value: single; const aThumb: TALTrackThumb): TRectF; overload; virtual;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
    function GetDefaultTouchTargetExpansion: TRectF; override;
    function GetThumbSize(var IgnoreViewportSize: Boolean): Integer; virtual;
    procedure DoRealign; override;
    property IsTracking: Boolean read GetIsTracking;
    procedure Loaded; override;
    procedure DoChanged; virtual;
    procedure DoTracking; virtual;
    function CreateValueRangeTrack : TValueRange; virtual;
    property DefaultValueRange: TBaseValueRange read FDefaultValueRange;
    property ValueRange: TValueRange read FValueRange write SetValueRange_ stored ValueStored;
    property Value: Single read GetValue write SetValue stored ValueStored nodefault;
    property Thumb: TALTrackThumb read FThumb;
    procedure UpdateHighlight; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property Min: Single read GetMin write SetMin stored MinStored nodefault;
    property Max: Single read GetMax write SetMax stored MaxStored nodefault;
    property Frequency: Single read GetFrequency write SetFrequency stored FrequencyStored nodefault;
    property ViewportSize: Single read GetViewportSize write SetViewportSize stored ViewportSizeStored nodefault;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property Tracking: Boolean read FTracking write FTracking default True;
    property ThumbSize: Single read fThumbSize write SetThumbSize Stored ThumbSizeStored; // << 0 mean the thumb will have the height of the track in horizontal or width of the track in vertical
    property BackGround: TALTrackBackground read FBackGround;
    property Highlight: TALTrackHighlight read FHighlight;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTracking: TNotifyEvent read FOnTracking write FOnTracking;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackBar = class(TALCustomTrack)
  protected
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ValueRange;
  published
    property ThumbSize;
    property Thumb;
    property BackGround;
    property Highlight;
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DisableFocusEffect;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Frequency;
    property Locked;
    property Height;
    property Hint;
    property HitTest;
    property Padding;
    property Min;
    property Max;
    property Orientation;
    property Opacity;
    property Margins;
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
    property ParentShowHint;
    property ShowHint;
    {events}
    property OnChange;
    property OnTracking;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    //-----
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    //-----
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALScrollBar = class(TALCustomTrack)
  protected
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ValueRange;
  published
    property Thumb;
    property BackGround;
    property Align;
    property Anchors;
    property CanFocus default False;
    property CanParentFocus;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property HitTest;
    property Padding;
    property Min;
    property Max;
    property Orientation;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Value;
    property Visible;
    property Width;
    property ViewportSize;
    {events}
    property OnChange;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    //-----
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    //-----
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALRangeTrackBar = class(TALCustomTrack)
  private
    FMaxValueRange: TValueRange;
  protected
    [Weak] FMaxThumb: TALTrackThumb;
    procedure SetViewportSize(const Value: Single); override;
    procedure SetFrequency(const Value: Single); override;
    procedure SetMax(const Value: Single); override;
    procedure SetMin(const Value: Single); override;
    function MaxValueStored: Boolean; virtual;
    function GetDefaultSize: TSizeF; override;
    procedure SetValue(Value: Single); override;
    function GetMaxValue: Single; virtual;
    procedure SetMaxValue(Value: Single); virtual;
    procedure Loaded; override;
    procedure DoRealign; override;
    procedure UpdateHighlight; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ThumbSize;
    property MinThumb: TALTrackThumb read FThumb;
    property MaxThumb: TALTrackThumb read FMaxThumb;
    property BackGround;
    property Highlight;
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DisableFocusEffect;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Frequency;
    property Locked;
    property Height;
    property Hint;
    property HitTest;
    property Padding;
    property Min;
    property Max;
    property MinValue: Single read GetValue write SetValue stored ValueStored nodefault;
    property MaxValue: Single read GetMaxValue write SetMaxValue stored MaxValueStored nodefault;
    property Orientation;
    property Opacity;
    property Margins;
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
    property ParentShowHint;
    property ShowHint;
    {events}
    property OnChange;
    property OnTracking;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    //-----
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    //-----
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBox = class(TControl)
  private
    FScreenScale: single;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    FbufResourceName: String;
    //-----
    FPressing: Boolean;
    FOnChange: TNotifyEvent;
    FIsPressed: Boolean;
    FIsChecked: Boolean;
    fImageCheckedResourceName: String;
    fImageUncheckedResourceName: String;
    FWrapMode: TALImageWrapMode;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure setImageCheckedResourceName(const Value: String);
    procedure setImageUncheckedResourceName(const Value: String);
    procedure SetWrapMode(const Value: TALImageWrapMode);
  protected
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
    procedure DoChanged; virtual;
    function GetDefaultSize: TSizeF; override;
    function GetIsChecked: Boolean; virtual;
    procedure SetIsChecked(const Value: Boolean); virtual;
    function ImageCheckedResourceNameStored: Boolean; virtual;
    function ImageUncheckedResourceNameStored: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  published
    property Action;
    property Align;
    property Anchors;
    property CanFocus default True;
    property CanParentFocus;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DisableFocusEffect;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled;
    property Locked default False;
    property Height;
    property Hint;
    property HitTest default True;
    property IsChecked: Boolean read GetIsChecked write SetIsChecked default False;
    property ImageCheckedResourceName: String read fImageCheckedResourceName write setImageCheckedResourceName stored ImageCheckedResourceNameStored;
    property ImageUncheckedResourceName: String read fImageUncheckedResourceName write setImageUncheckedResourceName stored ImageUncheckedResourceNameStored;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode default TALImageWrapMode.Fit;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TabStop;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property ParentShowHint;
    property ShowHint;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnKeyDown;
    property OnKeyUp;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    {$IF CompilerVersion >= 32} // tokyo
    property OnResized;
    {$ENDIF}
  end;

  {*********************************}
  TALRadioButton = class(TALCheckBox)
  private
    FGroupName: string;
    fMandatory: boolean;
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GroupNameStored: Boolean;
    procedure GroupMessageCall(const Sender : TObject; const M : TMessage);
  protected
    procedure SetIsChecked(const Value: Boolean); override;
    function ImageCheckedResourceNameStored: Boolean; override;
    function ImageUncheckedResourceNameStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GroupName: string read GetGroupName write SetGroupName stored GroupNameStored nodefault;
    property Mandatory: Boolean read fMandatory write fMandatory default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSwitchThumb = class(TALRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked default True;
    property HitTest default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSwitchBackground = class(TALRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked default True;
    property HitTest default false;
  end;

  {*************************}
  TALSwitch = class(TControl)
  public const
    DefaultSwitchAnimationDuration = 0.2;
    TrackingSensitivity = 3;
  private
    [Weak] FThumb: TALSwitchThumb;
    [Weak] FBackGround: TALSwitchBackground;
    FThumbRect: TrectF;
    FPressed, FTracking: Boolean;
    FPressedThumbPos, FSavedPos: TPointF;
    FAnimation: TALFloatAnimation;
    FIsChecked: Boolean;
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
    procedure SetIsChecked(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetThumbValue: Single;
    procedure SetIsCheckedWithAnimation(const Value: Boolean);
    property Tracking: boolean read FTracking write FTracking default false;
    property Animation: TALFloatAnimation read FAnimation;
  published
    property Action;
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TabOrder;
    property TouchTargetExpansion;
    property ThumbSize: Single read FThumbSize write SetThumbSize;
    property Thumb: TALSwitchThumb read FThumb;
    property BackGround: TALSwitchBackGround read FBackGround;
    property IsChecked: Boolean read FIsChecked write SetIsChecked default false;
    property AnimationDuration: single read fAnimationDuration write fAnimationDuration stored AnimationDurationStored;
    property Visible default True;
    property Width;
    property OnApplyStyleLookup;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnKeyDown;
    property OnKeyUp;
    property OnCanFocus;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnAnimationProcess: TNotifyEvent read FOnAnimationProcess write FOnAnimationProcess;
    property OnAnimationFinish: TNotifyEvent read FOnAnimationFinish write FOnAnimationFinish;
  end;

{$IFDEF debug}
var
  AlDebugAniIndicatorMakeBufBitmapCount: integer;
  AlDebugCheckBoxMakeBufBitmapCount: integer;

  AlDebugAniIndicatorMakeBufBitmapStopWatch: TstopWatch;
  AlDebugCheckBoxMakeBufBitmapStopWatch: TstopWatch;
{$endif}

procedure Register;

implementation

uses System.SysUtils,
     system.Math.Vectors,
     {$IFDEF ALDPK}
     system.IOUtils,
     DesignIntf,
     toolsApi,
     {$ENDIF}
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.Canvas.GPU,
     ALFmxTypes3D,
     {$ENDIF}
     FMX.Platform,
     fmx.consts,
     fmx.utils,
     alGraphics,
     AlCommon,
     ALFmxCommon;

{*****************************************************}
constructor TALAniIndicator.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
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
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fBufBitmap := nil;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
  Enabled := False;
  SetAcceptsControls(False);
end;

{*********************************}
destructor TALAniIndicator.Destroy;
begin
  fTimer.Enabled := False;
  ALFreeAndNil(fTimer);
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
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

{***************************************}
procedure TALAniIndicator.clearBufBitmap;
begin
  ALFreeAndNil(fBufBitmap);
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALAniIndicator.MakeBufBitmap: TTexture;
{$ELSE}
function TALAniIndicator.MakeBufBitmap: Tbitmap;
{$ENDIF}

{$IFDEF ALDPK}
var aFileName: String;
{$ENDIF}

begin

  if (Scene = nil) or
     //--- don't do bufbitmap is size=0
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     //--- don't do bufbitmap if fResourceName is empty
     (fResourceName = '')
  then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;

  {$IFDEF debug}
  ALLog('TALAniIndicator.MakeBufBitmap', 'Name: ' + Name, TalLogType.verbose);
  inc(AlDebugAniIndicatorMakeBufBitmapCount);
  AlDebugAniIndicatorMakeBufBitmapStopWatch.Start;
  try
  {$endif}

    {$IFDEF ALDPK}
    aFileName := extractFilePath(getActiveProject.fileName) + 'resources\' + fResourceName; // by default all the resources files must be located in the sub-folder /resources/ of the project
    if not TFile.Exists(aFileName) then begin
      aFileName := aFileName + '.png';
      if not TFile.Exists(aFileName) then aFileName := '';
    end;
    {$ENDIF}

    fBufBitmapRect := LocalRect;
    {$IFDEF ALDPK}
    if aFileName <> '' then fBufBitmap := ALLoadFitIntoFileImageV3(aFileName, Width * (fframeCount div fRowCount) * FScreenScale, Height * fRowCount * FScreenScale)
    else fBufBitmap := nil;
    {$ELSE}
    fBufBitmap := ALLoadFitIntoResourceImageV3(fResourceName, Width * (fframeCount div fRowCount) * FScreenScale, Height * fRowCount * FScreenScale);
    {$ENDIF}
    result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugAniIndicatorMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

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
var R: TRectF;
begin

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(TPointF.Create(fFrameIndex.x * Width * fScreenScale,
                                                                    fFrameIndex.Y * Height * fScreenScale),
                                                     Width * fScreenScale,
                                                     Height * fScreenScale), // ARect
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(TPointF.Create(fFrameIndex.x * Width * fScreenScale,
                                                 fFrameIndex.Y * Height * fScreenScale),
                                  Width * fScreenScale,
                                  Height * fScreenScale), // SrcRect
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

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

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALAniIndicator.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
  fTimer.enabled := False;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALAniIndicator.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
  fTimer.enabled := Enabled;
end;
{$ENDIF}

{*************************************************************}
procedure TALAniIndicator.setResourceName(const Value: String);
begin
  if FResourceName <> Value then begin
    clearBufBitmap;
    FResourceName := Value;
    Repaint;
  end;
end;

{*******************************************************************************************************************************}
function _ValueToPos(MinValue, MaxValue, ViewportSize, ThumbSize, TrackSize, Value: Single; IgnoreViewportSize: Boolean): Single;
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

{*****************************************************************************************************************************}
function _PosToValue(MinValue, MaxValue, ViewportSize, ThumbSize, TrackSize, Pos: Single; IgnoreViewportSize: Boolean): Single;
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
  FDeadZoneBeforeAcquireScrolling := 5;
  FDownOffset := TpointF.Create(0, 0);
  fTrackDownOffset := 0;
  fScrollingAcquiredByMe := False;
  fScrollingAcquiredByOther := False;
  fScrollingAcquiredByOtherMessageID := TMessageManager.DefaultManager.SubscribeToMessage(TALScrollingAcquiredMessage, ScrollingAcquiredByOtherHandler);
end;

{*******************************}
destructor TALTrackThumb.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TALScrollingAcquiredMessage, fScrollingAcquiredByOtherMessageID);
  inherited;
end;

{********************************************************}
function TALTrackThumb.PointToValue(X, Y: Single): Single;
var P: TPointF;
begin
  Result := 0;
  if (Parent is TControl) then begin
    if FTrack.Orientation = TOrientation.Horizontal then begin
      P := FTrack.ScreenToLocal(LocalToScreen(TPointF.Create(X, 0)));
      P.X := P.X - FDownOffset.X + Width / 2;
      Result := _PosToValue(FTrack.Min, FTrack.Max, FTrack.ViewportSize, Self.Width, FTrack.Width, P.X, FTrack.FIgnoreViewportSize);
    end
    else begin
      P := FTrack.ScreenToLocal(LocalToScreen(TPointF.Create(0, Y)));
      P.Y := P.Y - FDownOffset.Y + Height / 2;
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
    Result := TRectF.Create(DefaultTouchTargetExpansion,
                            DefaultTouchTargetExpansion,
                            DefaultTouchTargetExpansion,
                            DefaultTouchTargetExpansion)
  else
    Result := inherited ;
end;

{*********************************************************************}
procedure TALTrackThumb.setScrollingAcquiredByMe(const Value: boolean);
begin
  if Value <> fScrollingAcquiredByMe  then begin
    fScrollingAcquiredByMe := Value;
    TMessageManager.DefaultManager.SendMessage(self, TALScrollingAcquiredMessage.Create(Value), True);
  end;
end;

{************************************************************************************************}
procedure TALTrackThumb.ScrollingAcquiredByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  //the scrolling was acquired or released by another control (like a scrollbox for exemple)
  //the problem is that the scrolling could be acquired BEFORE the mousedown is fired in parent control (baah yes)
  //so we need the var fScrollingAcquiredByOther to handle this
  if (Sender = self) then exit;
  if TALScrollingAcquiredMessage(M).Acquired then begin
    if FPressed then begin
      FPressed := False;
      if (not FValueRange.Tracking) then FValueRange.Tracking := True;
    end;
    fScrollingAcquiredByOther := True;
  end
  else fScrollingAcquiredByOther := False;
end;

{****************************************************************************************}
procedure TALTrackThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (not fScrollingAcquiredByOther) and (Button = TMouseButton.mbLeft) and Enabled then begin
    BringToFront;
    repaint;
    FPressed := True;
    setScrollingAcquiredByMe(False);
    FDownOffset := PointF(X, Y);
    fTrackDownOffset := FTrack.ScreenToLocal(LocalToScreen(TPointF.Create(X, 0))).x;
    FTrack.SetFocus;
    fValueRange.Tracking := FTrack.Tracking;
    StartTriggerAnimation(Self, 'IsPressed');
    ApplyTriggerEffect(Self, 'IsPressed');
  end;
end;

{******************************************************************}
procedure TALTrackThumb.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FPressed and Enabled then begin

    if (not fScrollingAcquiredByMe) and
       (abs(FTrack.ScreenToLocal(LocalToScreen(TPointF.Create(x, 0))).x - fTrackDownOffset) > fDeadZoneBeforeAcquireScrolling) then setScrollingAcquiredByMe(True);

    try
      FValueRange.Value := PointToValue(X, Y);
    except
      FPressed := False;
      raise;
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

    setScrollingAcquiredByMe(False);

    FPressed := False;
    try
      if (not FValueRange.Tracking) then begin
        FValueRange.Value := LValue;
        FValueRange.Tracking := True;
      end;
    finally
      StartTriggerAnimation(Self, 'IsPressed');
      ApplyTriggerEffect(Self, 'IsPressed');
    end;

  end;
end;

{***********************************}
procedure TALTrackThumb.DoMouseLeave;
begin
  inherited;
  if FPressed then begin

    setScrollingAcquiredByMe(False);

    FPressed := False;
    try
      if (not FValueRange.Tracking) then begin
        FValueRange.Tracking := True;
      end;
    finally
      StartTriggerAnimation(Self, 'IsPressed');
      ApplyTriggerEffect(Self, 'IsPressed');
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
    [Weak] FTrack: TALCustomTrack;
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
    Result := TRectF.Create(DefaultTouchTargetExpansion,
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
function TALCustomTrack.GetMax: Single;
begin
  Result := FValueRange.Max;
end;

{***************************************************}
procedure TALCustomTrack.SetMax(const Value: Single);
begin
  if compareValue(Value, Min) < 0 then min := Value;
  FValueRange.Max := Value;
end;

{***************************************************}
procedure TALCustomTrack.SetMin(const Value: Single);
begin
  if compareValue(Value, Max) > 0 then max := Value;
  FValueRange.Min := Value;
end;

{*************************************}
function TALCustomTrack.GetMin: Single;
begin
  Result := FValueRange.Min;
end;

{*********************************************************}
procedure TALCustomTrack.SetFrequency(const Value: Single);
begin
  FValueRange.Frequency := Value;
end;

{*******************************************}
function TALCustomTrack.GetFrequency: Single;
begin
  Result := FValueRange.Frequency;
end;

{***************************************}
function TALCustomTrack.GetValue: Single;
begin
  Result := FValueRange.Value;
end;

{***********************************************}
procedure TALCustomTrack.SetValue(Value: Single);
begin
  FValueRange.Value := Value;
end;

{**********************************************}
function TALCustomTrack.GetViewportSize: Single;
begin
  Result := FValueRange.ViewportSize;
end;

{************************************************************}
procedure TALCustomTrack.SetViewportSize(const Value: Single);
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
var R: TRectF;
begin
  inherited;
  if FThumb <> nil then begin
    R := GetThumbRect(Value, FThumb);
    FThumb.Visible := not ((R.Right <= R.Left) or (R.Bottom <= R.Top));
    FThumb.BoundsRect := R;
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
var inc: Single;
    LValue: Single;
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
  FBackGround.Stroke.Kind := TBrushKind.None;
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
  FHighlight.Stroke.Kind := TBrushKind.None;
  FHighlight.Fill.Color := $ff167efc;
  //-----
  FThumb := TALTrackThumb.Create(self, fValueRange, true{aWithGlyphObj});
  FThumb.Parent := self;
  FThumb.Stored := False;
  FThumb.SetSubComponent(True);
  FThumb.Name := 'Thumb';
  FThumb.XRadius := 16;
  FThumb.yRadius := 16;
  FThumb.Stroke.Kind := TBrushKind.solid;
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
  FThumb.Stroke.Kind := TBrushKind.none;
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
  FBackGround.Stroke.Kind := TBrushKind.None;
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
  FHighlight.Stroke.Kind := TBrushKind.None;
  FHighlight.Fill.Color := $ff167efc;
  //-----
  FThumb := TALTrackThumb.Create(self, FvalueRange, true{aWithGlyphObj});
  FThumb.Parent := self;
  FThumb.Stored := False;
  FThumb.SetSubComponent(True);
  FThumb.Name := 'MinThumb';
  FThumb.XRadius := 16;
  FThumb.yRadius := 16;
  FThumb.Stroke.Kind := TBrushKind.solid;
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
  FMaxThumb.Stroke.Kind := TBrushKind.solid;
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
    if fThumb.IsPressed then MinValue := MaxValue
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
        FHighlight.setbounds(Round((rMin.Left + rMin.Right) / 2),
                             FHighlight.Position.y,
                             Round((rMax.Left + rMax.Right) / 2) - Round((rMin.Left + rMin.Right) / 2),
                             FHighlight.Height);
      end;
      TOrientation.Vertical: begin
        FHighlight.setbounds(FHighlight.Position.x,
                             Round((rMin.Top + rMin.Bottom) / 2),
                             FHighlight.width,
                             Round((rMax.Top + rMax.Bottom) / 2) - Round((rMin.Top + rMin.Bottom) / 2));
      end;
    end;
  end;
end;

{*************************************************}
procedure TALRangeTrackBar.SetValue(Value: Single);
begin
  inherited SetValue(Value);
  if (not fThumb.IsPressed) and
     (GetValue > (max - Min) / 2) then fThumb.BringToFront;
end;

{********************************************}
function TALRangeTrackBar.GetMaxValue: Single;
begin
  Result := FMaxValueRange.Value;
end;

{****************************************************}
procedure TALRangeTrackBar.SetMaxValue(Value: Single);
begin
  FMaxValueRange.Value := Value;
  if (not fMaxThumb.IsPressed) and
     (GetMaxValue < (max - Min) / 2) then fMaxThumb.BringToFront;
end;

{************************************************}
function TALRangeTrackBar.MaxValueStored: Boolean;
begin
  Result := not SameValue(MaxValue, DefaultValueRange.Value);
end;

{***********************************************************}
procedure TALRangeTrackBar.SetFrequency(const Value: Single);
begin
  inherited;
  FMaxValueRange.Frequency := Value;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMax(const Value: Single);
begin
  if compareValue(Value, Min) < 0 then min := Value;
  inherited;
  FMaxValueRange.Max := Value;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMin(const Value: Single);
begin
  if compareValue(Value, Max) > 0 then max := Value;
  inherited;
  FMaxValueRange.Min := Value;
end;

{**************************************************************}
procedure TALRangeTrackBar.SetViewportSize(const Value: Single);
begin
  inherited;
  FMaxValueRange.ViewportSize := Value;
end;

{*************************************************}
constructor TALCheckbox.Create(AOwner: TComponent);
var aScreenSrv: IFMXScreenService;
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, aScreenSrv) then FScreenScale := aScreenSrv.GetScreenScale
  else FScreenScale := 1;
  fBufBitmap := nil;
  SetAcceptsControls(False);
  CanFocus := True;
  AutoCapture := True;
  FPressing:= false;
  FOnChange := nil;
  FIsPressed := False;
  FIsChecked := False;
  fImageCheckedResourceName := 'checkbox_checked_88x88';
  fImageUncheckedResourceName := 'checkbox_unchecked_88x88';
  FWrapMode := TALImageWrapMode.Fit;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{*****************************}
destructor TALCheckbox.Destroy;
begin
  clearBufBitmap;
  {$IF defined(ANDROID) or defined(IOS)}
  TMessageManager.DefaultManager.Unsubscribe(TContextLostMessage, FOpenGLContextLostId);
  TMessageManager.DefaultManager.Unsubscribe(TContextResetMessage, FOpenGLContextResetId);
  {$ENDIF}
  inherited;
end;

{***********************************}
procedure TALCheckbox.clearBufBitmap;
begin
  ALFreeAndNil(fBufBitmap);
end;

{******************************}
procedure TALCheckbox.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;
end;

{**************************************************************************************}
procedure TALCheckbox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    FPressing := True;
    FIsPressed := True;
    StartTriggerAnimation(Self, 'IsPressed');
  end;
end;

{****************************************************************}
procedure TALCheckbox.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (ssLeft in Shift) and (FPressing) then
  begin
    if FIsPressed <> LocalRect.Contains(PointF(X, Y)) then
    begin
      FIsPressed := LocalRect.Contains(PointF(X, Y));
      StartTriggerAnimation(Self, 'IsPressed');
    end;
  end;
end;

{************************************************************************************}
procedure TALCheckbox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FPressing then
  begin
    inherited;
    FPressing := False;
    FIsPressed := False;

    if LocalRect.Contains(PointF(X, Y)) then
    begin
      IsChecked := not IsChecked;
    end
  end;
end;

{*********************************************************************************************}
procedure TALCheckbox.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
begin
  inherited;
  if (KeyChar = ' ') then
  begin
    Click; // Emulate mouse click to perform Action.OnExecute
    IsChecked := not IsChecked;
    KeyChar := #0;
  end;
end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
function TALCheckbox.MakeBufBitmap: TTexture;
{$ELSE}
function TALCheckbox.MakeBufBitmap: Tbitmap;
{$ENDIF}

var aResourceName: String;
    {$IFDEF ALDPK}
    aFileName: String;
    {$ENDIF}

begin

  if IsChecked then aResourceName := ImageCheckedResourceName
  else aResourceName := ImageUncheckedResourceName;

  if (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     (aResourceName = '') then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) and
     (FbufResourceName = aResourceName) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;
  FbufResourceName := aResourceName;

  {$IFDEF debug}
  ALLog('TALCheckbox.MakeBufBitmap', 'Name: ' + Name, TalLogType.verbose);
  inc(AlDebugCheckBoxMakeBufBitmapCount);
  AlDebugCheckBoxMakeBufBitmapStopWatch.Start;
  try
  {$endif}

    {$IFDEF ALDPK}
    aFileName := extractFilePath(getActiveProject.fileName) + 'resources\' + aResourceName; // by default all the resources files must be located in the sub-folder /resources/ of the project
    if not TFile.Exists(aFileName) then begin
      aFileName := aFileName + '.png';
      if not TFile.Exists(aFileName) then aFileName := '';
    end;
    {$ENDIF}

    case FWrapMode of

      //Display the image with its original dimensions:
      //* The image is placed in the upper-left corner of the rectangle of the control.
      //* If the image is larger than the control's rectangle, then only the upper-left part of the image,
      //  which fits in the rectangle of the control, is shown. The image is not resized.
      TALImageWrapMode.Original:
        begin
          Result := nil; // todo
        end;

      //Best fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
      //  (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
      //  of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
      //  or the height of the resized image is equal to the height of the rectangle of the control. The whole image
      //  should be displayed. The image is displayed centered in the rectangle of the control.
      // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
      //  the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      TALImageWrapMode.Fit:
        begin
          fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
          {$IFDEF ALDPK}
          if aFileName <> '' then fBufBitmap := ALLoadFitIntoFileImageV3(aFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := nil;
          {$ELSE}
          fBufBitmap := ALLoadFitIntoResourceImageV3(aResourceName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale);
          {$ENDIF}
          result := fBufBitmap;
          if result <> nil then fBufBitmapRect := TrectF.Create(0,0, result.Width/FScreenScale, result.Height/FScreenScale).
                                                    CenterAt(fBufBitmapRect);
        end;

      //Stretch the image to fill the entire rectangle of the control.
      TALImageWrapMode.Stretch:
        begin
          Result := nil; // todo
        end;

      //Tile (multiply) the image to cover the entire rectangle of the control:
      //* If the image is larger than the rectangle of the control, then only the
      //  upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
      //* If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
      //  (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
      //  the upper-left corner of the rectangle of the control.
      TALImageWrapMode.Tile:
        begin
          Result := nil; // todo
        end;

      //Center the image to the rectangle of the control:
      //* The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
      TALImageWrapMode.Center:
        begin
          Result := nil; // todo
        end;

      //Fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
      //  to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
      //  resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
      //* If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
      TALImageWrapMode.Place:
        begin
          Result := nil; // todo
        end;

      //Best fit the image in the rectangle of the control:
      //* If any dimension of the image is larger than the rectangle of the control, then scales down the image
      //  (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
      //  of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
      //  AND the height of the resized image is equal to the height of the rectangle of the control.
      // * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
      //  the rectangle of the control. Whole the image should be displayed.
      TALImageWrapMode.FitAndCrop:
        begin
          fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
          {$IFDEF ALDPK}
          if aFileName <> '' then fBufBitmap := ALLoadFitIntoAndCropFileImageV3(aFileName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale)
          else fBufBitmap := nil;
          {$ELSE}
          fBufBitmap := ALLoadFitIntoAndCropResourceImageV3(aResourceName, fBufBitmapRect.Width * FScreenScale, fBufBitmapRect.Height * FScreenScale);
          {$ENDIF}
          result := fBufBitmap;
          if result <> nil then fBufBitmapRect := TrectF.Create(0,0, result.Width/FScreenScale, result.Height/FScreenScale).
                                                    CenterAt(fBufBitmapRect);
        end;

      //to hide a stupid warning else
      else Result := nil;

    end;

  {$IFDEF debug}
  finally
    AlDebugCheckBoxMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}

end;

{**************************}
procedure TALCheckbox.Paint;
var R: TRectF;
begin

  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    inherited paint;
    exit;
  end;

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}

  TCustomCanvasGpu(Canvas).DrawTexture(canvas.AlignToPixel(fBufBitmapRect), // ATexRect (destRec)
                                       TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), // ARect (srcRec)
                                       ALPrepareColor(TCustomCanvasGpu.ModulateColor, AbsoluteOpacity), // https://quality.embarcadero.com/browse/RSP-15432
                                       fBufBitmap);

  {$ELSE}

  canvas.DrawBitmap(fBufBitmap,
                    TRectF.Create(0, 0, fBufBitmap.Width, fBufBitmap.Height), {SrcRect}
                    canvas.AlignToPixel(fBufBitmapRect), {DestRect}
                    AbsoluteOpacity, {opacity}
                    true{highSpeed});

  {$ENDIF}

end;

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALCheckbox.OpenGLContextLostHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{************************************}
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
procedure TALCheckbox.OpenGLContextResetHandler(const Sender: TObject; const Msg: TMessage);
begin
  clearBufBitmap;
end;
{$ENDIF}

{******************************************}
function TALCheckbox.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(22, 22);
end;

{***********************************************************}
function TALCheckBox.ImageCheckedResourceNameStored: Boolean;
begin
  result := fImageCheckedResourceName <> 'checkbox_checked_88x88';
end;

{*************************************************************}
function TALCheckBox.ImageUncheckedResourceNameStored: Boolean;
begin
  result := fImageUnCheckedResourceName <> 'checkbox_unchecked_88x88';
end;

{***************************************************************}
procedure TALCheckBox.SetWrapMode(const Value: TALImageWrapMode);
begin
  if FWrapMode <> Value then begin
    clearBufBitmap;
    FWrapMode := Value;
    Repaint;
  end;
end;

{*********************************************************************}
procedure TALCheckbox.setImageCheckedResourceName(const Value: String);
begin
  if fImageCheckedResourceName <> Value then begin
    clearBufBitmap;
    fImageCheckedResourceName := Value;
    Repaint;
  end;
end;

{***********************************************************************}
procedure TALCheckbox.setImageUncheckedResourceName(const Value: String);
begin
  if fImageUncheckedResourceName <> Value then begin
    clearBufBitmap;
    fImageUncheckedResourceName := Value;
    Repaint;
  end;
end;

{*****************************************}
function TALCheckbox.GetIsChecked: Boolean;
begin
  Result := FIsChecked;
end;

{*******************************************************}
procedure TALCheckbox.SetIsChecked(const Value: Boolean);
begin
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    StartTriggerAnimation(Self, 'IsChecked');
    DoChanged;
  end;
end;

{****************************************************}
constructor TALRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  fImageCheckedResourceName := 'radio_checked_88x88';
  fImageUncheckedResourceName := 'radio_unchecked_88x88';
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

{**********************************************************}
procedure TALRadioButton.SetIsChecked(const Value: Boolean);
var M: TRadioButtonGroupMessage;
begin
  if FIsChecked <> Value then begin
    if (csDesigning in ComponentState) and FIsChecked then FIsChecked := Value // allows check/uncheck in design-mode
    else begin
      if (not value) and fMandatory then exit;
      FIsChecked := Value;
      if Value then begin
        M := TRadioButtonGroupMessage.Create(GroupName);
        TMessageManager.DefaultManager.SendMessage(Self, M, True);
      end;
    end;
    StartTriggerAnimation(Self, 'IsChecked');
    DoChanged;
  end;
end;

{**************************************************************}
function TALRadioButton.ImageCheckedResourceNameStored: Boolean;
begin
  result := fImageCheckedResourceName <> 'radio_checked_88x88';
end;

{****************************************************************}
function TALRadioButton.ImageUncheckedResourceNameStored: Boolean;
begin
  result := fImageUnCheckedResourceName <> 'radio_unchecked_88x88';
end;

{*******************************************}
function TALRadioButton.GetGroupName: string;
begin
  Result := FGroupName;
end;

{**********************************************************************************}
procedure TALRadioButton.GroupMessageCall(const Sender: TObject; const M: TMessage);
var aOldMandatory: Boolean;
begin
  if SameText(TRadioButtonGroupMessage(M).GroupName, GroupName) and (Sender <> Self) and (Scene <> nil) and
     (not (Sender is TControl) or ((Sender as TControl).Scene = Scene)) then begin
    aOldMandatory := fMandatory;
    fMandatory := False;
    try
      IsChecked := False;
    finally
      fMandatory := aOldMandatory;
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
  FIsChecked := false;
  FOnChange := nil;
  FOnAnimationProcess := nil;
  FOnAnimationFinish := nil;
  FPressed := false;
  FTracking := false;
  FPressedThumbPos := TpointF.create(0,0);
  FSavedPos := TpointF.create(0,0);
  FThumbSize := 0;
  FThumbRect := GetThumbRectByValue(FIsChecked);
  fAnimationDuration := DefaultSwitchAnimationDuration;
  //-----
  FAnimation := TALFloatAnimation.Create;
  FAnimation.AnimationType := TAnimationType.In;
  FAnimation.Interpolation := TInterpolationType.Linear;
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
  FBackGround.Stroke.Kind := TBrushKind.None;
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
  FThumb.Stroke.Kind := TBrushKind.solid;
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
  FthumbRect := GetThumbRectByValue(FIsChecked);
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
var LIsChecked: Boolean;
begin
  inherited;
  if FPressed then begin
    FPressed := False;
    if not FTracking then begin
      LIsChecked := not FIsChecked;
      AnimateTo(LIsChecked);
      SetIsChecked(LIsChecked);
    end
    else begin
      LIsChecked := GetValueByMousePos(X, Y);
      AnimateTo(LIsChecked);
      SetIsChecked(LIsChecked);
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
  FthumbRect := GetThumbRectByValue(FIsChecked);
  realign;
end;

{*****************************************************}
procedure TALSwitch.SetIsChecked(const Value: Boolean);
begin
  if FIsChecked <> Value then begin
    FIsChecked := Value;
    if not Fanimation.Running then begin
      FThumbRect := GetThumbRectByValue(FIsChecked);
      realign;
    end;
    DoChange;
  end;
end;

{******************************************************************}
procedure TALSwitch.SetIsCheckedWithAnimation(const Value: Boolean);
begin
  if FIsChecked <> Value then begin
    AnimateTo(FIsChecked);
    SetIsChecked(FIsChecked);
  end;
end;

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALAniIndicator, TALScrollBar, TALTrackBar, TALRangeTrackBar, TALCheckBox, TALRadioButton, TALSwitch]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALTrackThumbGlyph, 'Locked');
  UnlistPublishedProperty(TALTrackThumbGlyph, 'StyleName');
  //-----
  UnlistPublishedProperty(TALTrackThumb, 'Locked');
  UnlistPublishedProperty(TALTrackThumb, 'StyleName');
  UnlistPublishedProperty(TALTrackThumb, 'Anchors'); // not work https://quality.embarcadero.com/browse/RSP-15684
  UnlistPublishedProperty(TALTrackThumb, 'Align');
  UnlistPublishedProperty(TALTrackThumb, 'Position');
  UnlistPublishedProperty(TALTrackThumb, 'Size');
  UnlistPublishedProperty(TALTrackThumb, 'PopupMenu');
  UnlistPublishedProperty(TALTrackThumb, 'DragMode');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragEnd');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragEnter');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragLeave');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragOver');
  UnlistPublishedProperty(TALTrackThumb, 'OnDragDrop');
  UnlistPublishedProperty(TALTrackThumb, 'EnableDragHighlight');
  //-----
  UnlistPublishedProperty(TALTrackBackground, 'Locked');
  UnlistPublishedProperty(TALTrackBackground, 'StyleName');
  UnlistPublishedProperty(TALTrackBackground, 'PopupMenu');
  UnlistPublishedProperty(TALTrackBackground, 'DragMode');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragEnd');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragEnter');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragLeave');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragOver');
  UnlistPublishedProperty(TALTrackBackground, 'OnDragDrop');
  UnlistPublishedProperty(TALTrackBackground, 'EnableDragHighlight');
  //-----
  UnlistPublishedProperty(TALTrackHighlight, 'Locked');
  UnlistPublishedProperty(TALTrackHighlight, 'StyleName');
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
  //-----
  UnlistPublishedProperty(TALSwitchThumb, 'Locked');
  UnlistPublishedProperty(TALSwitchThumb, 'StyleName');
  UnlistPublishedProperty(TALSwitchThumb, 'Anchors'); // not work https://quality.embarcadero.com/browse/RSP-15684
  UnlistPublishedProperty(TALSwitchThumb, 'Align');
  UnlistPublishedProperty(TALSwitchThumb, 'Position');
  UnlistPublishedProperty(TALSwitchThumb, 'Size');
  UnlistPublishedProperty(TALSwitchThumb, 'PopupMenu');
  UnlistPublishedProperty(TALSwitchThumb, 'DragMode');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragEnd');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragEnter');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragLeave');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragOver');
  UnlistPublishedProperty(TALSwitchThumb, 'OnDragDrop');
  UnlistPublishedProperty(TALSwitchThumb, 'EnableDragHighlight');
  //-----
  UnlistPublishedProperty(TALSwitchBackground, 'Locked');
  UnlistPublishedProperty(TALSwitchBackground, 'StyleName');
  UnlistPublishedProperty(TALSwitchBackground, 'PopupMenu');
  UnlistPublishedProperty(TALSwitchBackground, 'DragMode');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragEnd');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragEnter');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragLeave');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragOver');
  UnlistPublishedProperty(TALSwitchBackground, 'OnDragDrop');
  UnlistPublishedProperty(TALSwitchBackground, 'EnableDragHighlight');
  //-----
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALAniIndicator, TALCheckBox, TALRadioButton, TALScrollBar, TALTrackBar, TALRangeTrackBar, TALSwitch]);
  {$IFDEF debug}
  AlDebugAniIndicatorMakeBufBitmapStopWatch := TstopWatch.Create;
  AlDebugCheckBoxMakeBufBitmapStopWatch := TstopWatch.Create;
  {$endif}

end.
