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
    property Align;
    property Anchors;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled stored EnabledStored default DefaultEnabled;
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
    property OnResized;
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
    fScrollCapturedByOther: boolean;
    procedure setScrollCapturedByMe(const Value: boolean);
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
    property TouchTargetExpansion;
    property Locked default True;
    property Position stored false;
    property Size stored false;
    property Glyph: TALTrackThumbGlyph read FGlyph;
    property Cursor default crHandPoint;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackBackground = class(TALBaseRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked default True;
    property HitTest default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackHighlight = class(TALBaseRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked default True;
    property Position stored false;
    property HitTest default false;
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
    property ThumbSize: Single read fThumbSize write SetThumbSize Stored ThumbSizeStored; // << 0 mean the thumb will have the height of the track in horizontal or width of the track in vertical
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
    property Value;
    property Visible;
    property Width;
    property ParentShowHint;
    property ShowHint;
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
    property MinValue: Double read GetValue write SetValue stored ValueStored nodefault;
    property MaxValue: Double read GetMaxValue write SetMaxValue stored MaxValueStored nodefault;
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
    property OnResized;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALCheckBox = class(TALControl, IALDoubleBufferedControl)
  private
    fBufDrawable: TALDrawable;
    fBufDrawableRect: TRectF;
    FbufResourceName: String;
    //-----
    FPressing: Boolean;
    FOnChange: TNotifyEvent;
    FPressed: Boolean;
    FChecked: Boolean;
    FIsPan: Boolean;
    fImageCheckedResourceName: String;
    fImageUncheckedResourceName: String;
    FWrapMode: TALImageWrapMode;
    procedure setImageCheckedResourceName(const Value: String);
    procedure setImageUncheckedResourceName(const Value: String);
    procedure SetWrapMode(const Value: TALImageWrapMode);
  protected
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure Paint; override;
    property BufDrawable: TALDrawable read fBufDrawable;
    property BufDrawableRect: TRectF read fBufDrawableRect;
    procedure DoChanged; virtual;
    function GetDefaultSize: TSizeF; override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked(const Value: Boolean); virtual;
    function ImageCheckedResourceNameStored: Boolean; virtual;
    function ImageUncheckedResourceNameStored: Boolean; virtual;
    procedure DoResized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeBufDrawable; virtual;
    procedure clearBufDrawable; virtual;
    procedure SetNewScene(AScene: IScene); override;
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
    property Checked: Boolean read GetChecked write SetChecked default False;
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
    function ImageCheckedResourceNameStored: Boolean; override;
    function ImageUncheckedResourceNameStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GroupName: string read GetGroupName write SetGroupName stored GroupNameStored nodefault;
    property Mandatory: Boolean read fMandatory write fMandatory default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSwitchThumb = class(TALBaseRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked default True;
    property HitTest default false;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALSwitchBackground = class(TALBaseRectangle)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Locked default True;
    property HitTest default false;
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
    property TabStop;
    property TouchTargetExpansion;
    property ThumbSize: Single read FThumbSize write SetThumbSize;
    property Thumb: TALSwitchThumb read FThumb;
    property BackGround: TALSwitchBackGround read FBackGround;
    property Checked: Boolean read FChecked write SetChecked default false;
    property AnimationDuration: single read fAnimationDuration write fAnimationDuration stored AnimationDurationStored;
    property Visible default True;
    property Width;
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALButtonStateStyleTextSettings = class(TALInheritBaseTextSettings)
  published
    property Font;
    property Decoration;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALButtonStateStyle = class(TALPersistentObserver)
  private
    FText: String;
    FFill: TALInheritBrush;
    FStroke: TALInheritStrokeBrush;
    FTextSettings: TALButtonStateStyleTextSettings;
    FShadow: TALInheritShadow;
    procedure SetText(const Value: string);
    procedure SetFill(const AValue: TALInheritBrush);
    procedure SetStroke(const AValue: TALInheritStrokeBrush);
    procedure SetTextSettings(const AValue: TALButtonStateStyleTextSettings);
    procedure SetShadow(const AValue: TALInheritShadow);
    procedure FillChanged(ASender: TObject);
    procedure StrokeChanged(ASender: TObject);
    procedure TextSettingsChanged(ASender: TObject);
    procedure ShadowChanged(ASender: TObject);
  protected
    function GetInherit: Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    Property Inherit: Boolean read GetInherit;
  published
    property Text: string read FText write SetText;
    property Fill: TALInheritBrush read FFill write SetFill;
    property Stroke: TALInheritStrokeBrush read FStroke write SetStroke;
    property TextSettings: TALButtonStateStyleTextSettings read fTextSettings write SetTextSettings;
    property Shadow: TALInheritShadow read FShadow write SetShadow;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALButtonDisabledStateStyle = class(TALButtonStateStyle)
  private
    FOpacity: Single;
    function OpacityStored: Boolean;
    procedure SetOpacity(const Value: Single);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Opacity: Single read FOpacity write SetOpacity stored OpacityStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALButtonStateStyles = class(TALPersistentObserver)
  private
    FDisabled: TALButtonDisabledStateStyle;
    FHovered: TALButtonStateStyle;
    FPressed: TALButtonStateStyle;
    FFocused: TALButtonStateStyle;
    procedure SetDisabled(const AValue: TALButtonDisabledStateStyle);
    procedure SetHovered(const AValue: TALButtonStateStyle);
    procedure SetPressed(const AValue: TALButtonStateStyle);
    procedure SetFocused(const AValue: TALButtonStateStyle);
    procedure DisabledChanged(ASender: TObject);
    procedure HoveredChanged(ASender: TObject);
    procedure PressedChanged(ASender: TObject);
    procedure FocusedChanged(ASender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Disabled: TALButtonDisabledStateStyle read FDisabled write SetDisabled;
    property Hovered: TALButtonStateStyle read FHovered write SetHovered;
    property Pressed: TALButtonStateStyle read FPressed write SetPressed;
    property Focused: TALButtonStateStyle read FFocused write SetFocused;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALButtonTextSettings = class(TALBaseTextSettings)
  private
  public
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~}
  [ComponentPlatforms($FFFF)]
  TALButton = class(TALBaseText)
  private
    FHovered: Boolean;
    FPressed: Boolean;
    FStateStyles: TALButtonStateStyles;
    fBufDisabledDrawable: TALDrawable;
    fBufDisabledDrawableRect: TRectF;
    fBufHoveredDrawable: TALDrawable;
    fBufHoveredDrawableRect: TRectF;
    fBufPressedDrawable: TALDrawable;
    fBufPressedDrawableRect: TRectF;
    fBufFocusedDrawable: TALDrawable;
    fBufFocusedDrawableRect: TRectF;
    function GetTextSettings: TALButtonTextSettings;
  protected
    function CreateTextSettings: TALBaseTextSettings; override;
    procedure SetTextSettings(const Value: TALButtonTextSettings); reintroduce;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure SetName(const Value: TComponentName); override;
    procedure SetStateStyles(const AValue: TALButtonStateStyles);
    procedure StateStylesChanged(Sender: TObject); virtual;
    procedure Paint; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure clearBufDrawable; override;
    procedure MakeBufDrawable; override;
  published
    property CanFocus default False;
    property HitTest default True;
    property AutoSize default True;
    property Cursor default crHandPoint;
    property TabOrder;
    property TabStop;
    property StateStyles: TALButtonStateStyles read FStateStyles write SetStateStyles;
    property TextSettings: TALButtonTextSettings read GetTextSettings write SetTextSettings;
  end;

procedure ALApplyThemeToButton(const AButton: TALButton; const ATheme: String);

procedure Register;

implementation

uses
  System.SysUtils,
  system.Math.Vectors,
  {$IF defined(ALSkiaCanvas)}
  System.Skia.API,
  {$ENDIF}
  {$IFDEF ALDPK}
  DesignIntf,
  {$ENDIF}
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  FMX.Canvas.GPU,
  Alcinoe.FMX.Types3D,
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
  var LFileName := ALDPKGetResourceFilename(FResourceName);
  if LFileName <> '' then fBufDrawable := ALLoadFromFileAndFitIntoToDrawable(LFileName, Width * (fframeCount div fRowCount) * ALGetScreenScale, Height * fRowCount * ALGetScreenScale)
  else fBufDrawable := ALNullDrawable;
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
  fScrollCapturedByOther := False;
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

{******************************************************************}
procedure TALTrackThumb.setScrollCapturedByMe(const Value: boolean);
begin
  if Value <> fScrollCapturedByMe  then begin
    {$IFDEF DEBUG}
    //ALLog('TALTrackThumb.setScrollCapturedByMe', 'Value: ' + ALBoolToStrW(Value), TalLogType.verbose);
    {$ENDIF}
    fScrollCapturedByMe := Value;
    TMessageManager.DefaultManager.SendMessage(self, TALScrollCapturedMessage.Create(Value), True);
  end;
end;

{*********************************************************************************************}
procedure TALTrackThumb.ScrollCapturedByOtherHandler(const Sender: TObject; const M: TMessage);
begin
  //the scrolling was Captured or released by another control (like a scrollbox for exemple)
  //the problem is that the scrolling could be Captured BEFORE the mousedown is fired in parent control (baah yes)
  //so we need the var fScrollCapturedByOther to handle this
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
    fScrollCapturedByOther := True;
  end
  else fScrollCapturedByOther := False;
end;

{****************************************************************************************}
procedure TALTrackThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (not fScrollCapturedByOther) and (Button = TMouseButton.mbLeft) and Enabled then begin
    BringToFront;
    repaint;
    FPressed := True;
    setScrollCapturedByMe(False);
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
        setScrollCapturedByMe(True);
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

    setScrollCapturedByMe(False);

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

    setScrollCapturedByMe(False);

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

{*************************************************}
constructor TALCheckbox.Create(AOwner: TComponent);
begin
  inherited;
  fBufDrawable := ALNullDrawable;
  SetAcceptsControls(False);
  CanFocus := True;
  AutoCapture := True;
  FPressing:= false;
  FOnChange := nil;
  FPressed := False;
  FChecked := False;
  FIsPan := False;
  fImageCheckedResourceName := 'checkbox_checked_88x88';
  fImageUncheckedResourceName := 'checkbox_unchecked_88x88';
  FWrapMode := TALImageWrapMode.Fit;
end;

{*****************************}
destructor TALCheckbox.Destroy;
begin
  clearBufDrawable;
  inherited;
end;

{***********************************}
procedure TALCheckbox.clearBufDrawable;
begin
  {$IFDEF debug}
  if (not (csDestroying in ComponentState)) and
     (not ALIsDrawableNull(fBufDrawable)) then
    ALLog(Classname + '.clearBufDrawable', 'BufDrawable has been cleared | Name: ' + Name, TalLogType.warn);
  {$endif}
  ALFreeAndNilDrawable(fBufDrawable);
end;

{******************************}
procedure TALCheckbox.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  Repaint;
end;

{************************************************}
procedure TALCheckbox.SetNewScene(AScene: IScene);
begin
  if FPressed and (Scene <> nil) then
  begin
    FPressed := False;
    if AScene <> nil then
      StartTriggerAnimation(Self, 'Pressed');
  end;
  inherited;
end;

{**************************************************************************************}
procedure TALCheckbox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
  begin
    FPressing := True;
    FPressed := True;
    FIsPan := False;
    StartTriggerAnimation(Self, 'Pressed');
  end;
end;

{****************************************************************}
procedure TALCheckbox.MouseMove(Shift: TShiftState; X, Y: Single);
const
  ThresholdMouseTap = 5;
var
  Distance: Single;
begin
  inherited;
  if (ssLeft in Shift) and FPressing then
  begin
    if FPressed <> LocalRect.Contains(PointF(X, Y)) then
    begin
      FPressed := LocalRect.Contains(PointF(X, Y));
      StartTriggerAnimation(Self, 'Pressed');
    end;
    if not FIsPan then
    begin
      Distance := LocalToScreen(PressedPosition).Distance(LocalToScreen(TPointF.Create(X, Y)));
      if Distance > ThresholdMouseTap then
        FIsPan := True;
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
    FPressed := False;

    if LocalRect.Contains(PointF(X, Y)) then
    begin
      // Avoiding a changing "Checked" state if user moves cursor of finger through the control.
      case TOSVersion.Platform of
        TOSVersion.TPlatform.pfiOS,
        TOSVersion.TPlatform.pfAndroid:
          if not FIsPan then
            Checked := not Checked;
      else
        Checked := not Checked;
      end;
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
    Checked := not Checked;
    KeyChar := #0;
  end;
end;

{************************************}
procedure TALCheckbox.MakeBufDrawable;
begin

  var LResourceName: String;
  if Checked then LResourceName := ImageCheckedResourceName
  else LResourceName := ImageUncheckedResourceName;

  if (Size.Size.IsZero) or // Do not create BufDrawable if the size is 0
     (LResourceName = '') then begin // Do not create BufDrawable if LResourceName is empty
    clearBufDrawable;
    exit;
  end;

  if (not ALIsDrawableNull(fBufDrawable)) and
     (FbufResourceName = LResourceName) then exit;
  clearBufDrawable;
  FbufResourceName := LResourceName;

  {$IFDEF ALDPK}
  var LFileName := ALDPKGetResourceFilename(LResourceName);
  {$ENDIF}

  fBufDrawableRect := ALAlignDimensionToPixelRound(LocalRect, ALGetScreenScale); // to have the pixel aligned width and height

  case FWrapMode of

    // Display the image with its original dimensions:
    // * The image is placed in the upper-left corner of the rectangle of the control.
    // * If the image is larger than the control's rectangle, then only the upper-left part of the image,
    //   which fits in the rectangle of the control, is shown. The image is not resized.
    TALImageWrapMode.Original:
      begin
        // todo
      end;

    // Best fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image
    //   (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
    //   of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
    //   or the height of the resized image is equal to the height of the rectangle of the control. The whole image
    //   should be displayed. The image is displayed centered in the rectangle of the control.
    //  * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    //   the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    TALImageWrapMode.Fit:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufDrawable := ALLoadFromFileAndFitIntoToDrawable(LFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALNullDrawable;
        {$ELSE}
        fBufDrawable := ALLoadFromResourceAndFitIntoToDrawable(LResourceName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale);
        {$ENDIF}
      end;

    // Stretch the image to fill the entire rectangle of the control.
    TALImageWrapMode.Stretch:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufDrawable := ALLoadFromFileAndStretchToDrawable(LFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALNullDrawable;
        {$ELSE}
        fBufDrawable := ALLoadFromResourceAndStretchToDrawable(LResourceName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale);
        {$ENDIF}
      end;

    // Tile (multiply) the image to cover the entire rectangle of the control:
    // * If the image is larger than the rectangle of the control, then only the
    //   upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
    // * If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
    //   (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
    //   the upper-left corner of the rectangle of the control.
    TALImageWrapMode.Tile:
      begin
        // todo
      end;

    // Center the image to the rectangle of the control:
    // * The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
    TALImageWrapMode.Center:
      begin
        // todo
      end;

    // Fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
    //   to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
    //   resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    // * If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
    TALImageWrapMode.Place:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufDrawable := ALLoadFromFileAndPlaceIntoToDrawable(LFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALNullDrawable;
        {$ELSE}
        fBufDrawable := ALLoadFromResourceAndPlaceIntoToDrawable(LResourceName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale);
        {$ENDIF}
      end;

    // Best fit the image in the rectangle of the control:
    // * If any dimension of the image is larger than the rectangle of the control, then scales down the image
    //   (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
    //   of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
    //   AND the height of the resized image is equal to the height of the rectangle of the control.
    //  * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    //   the rectangle of the control. Whole the image should be displayed.
    TALImageWrapMode.FitAndCrop:
      begin
        {$IFDEF ALDPK}
        if LFileName <> '' then fBufDrawable := ALLoadFromFileAndFitIntoAndCropToDrawable(LFileName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale)
        else fBufDrawable := ALNullDrawable;
        {$ELSE}
        fBufDrawable := ALLoadFromResourceAndFitIntoAndCropToDrawable(LResourceName, fBufDrawableRect.Width * ALGetScreenScale, fBufDrawableRect.Height * ALGetScreenScale);
        {$ENDIF}
      end;

    // Error
    else
      Raise Exception.Create('Error 9D7CF8BF-AB6B-432F-8C2B-474E23B5535D')

  end;

  if not ALIsDrawableNull(fBufDrawable) then
    fBufDrawableRect := TrectF.Create(0,0, ALGetDrawableWidth(fBufDrawable)/ALGetScreenScale, ALGetDrawableHeight(fBufDrawable)/ALGetScreenScale).
                          CenterAt(LocalRect);
end;

{**************************}
procedure TALCheckbox.Paint;
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
    fBufDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{***********************************************}
function TALCheckbox.GetDoubleBuffered: boolean;
begin
  result := True;
end;

{**************************************************************}
procedure TALCheckbox.SetDoubleBuffered(const AValue: Boolean);
begin
  // Not yet supported
end;

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
    clearBufDrawable;
    FWrapMode := Value;
    Repaint;
  end;
end;

{*********************************************************************}
procedure TALCheckbox.setImageCheckedResourceName(const Value: String);
begin
  if fImageCheckedResourceName <> Value then begin
    clearBufDrawable;
    fImageCheckedResourceName := Value;
    Repaint;
  end;
end;

{***********************************************************************}
procedure TALCheckbox.setImageUncheckedResourceName(const Value: String);
begin
  if fImageUncheckedResourceName <> Value then begin
    clearBufDrawable;
    fImageUncheckedResourceName := Value;
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
  if FChecked <> Value then
  begin
    FChecked := Value;
    StartTriggerAnimation(Self, 'Checked');
    DoChanged;
  end;
end;

{******************************}
procedure TALCheckbox.DoResized;
begin
  ClearBufDrawable;
  inherited;
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
    StartTriggerAnimation(Self, 'Checked');
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
constructor TALButtonStateStyle.Create;
begin
  inherited Create;
  FText := '';
  //--
  FFill := TALInheritBrush.Create(TBrushKind.Solid{ADefaultKind}, $FFE1E1E1{ADefaultColor});
  FFill.OnChanged := FillChanged;
  //--
  FStroke := TALInheritStrokeBrush.Create(TBrushKind.Solid{ADefaultKind}, $FFADADAD{ADefaultColor});
  FStroke.OnChanged := StrokeChanged;
  //--
  FTextSettings := TALButtonStateStyleTextSettings.Create;
  FTextSettings.OnChanged := TextSettingsChanged;
  //--
  FShadow := TALInheritShadow.Create;
  FShadow.OnChanged := ShadowChanged;
end;

{*************************************}
destructor TALButtonStateStyle.Destroy;
begin
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FTextSettings);
  ALFreeAndNil(FShadow);
  inherited Destroy;
end;

{******************************************************}
procedure TALButtonStateStyle.Assign(Source: TPersistent);
begin
  if Source is TALButtonStateStyle then begin
    BeginUpdate;
    Try
      Fill.Assign(TALButtonStateStyle(Source).Fill);
      Stroke.Assign(TALButtonStateStyle(Source).Stroke);
      TextSettings.Assign(TALButtonStateStyle(Source).TextSettings);
      Shadow.Assign(TALButtonStateStyle(Source).Shadow);
    Finally
      EndUpdate;
    End;
  end
  else
    inherited Assign(Source);
end;

{*********************************************************}
procedure TALButtonStateStyle.SetText(const Value: string);
begin
  if FText <> Value then begin
    FText := Value;
    Change;
  end;
end;

{****************************************************************************}
procedure TALButtonStateStyle.SetFill(const AValue: TALInheritBrush);
begin
  FFill.Assign(AValue);
end;

{************************************************************************************}
procedure TALButtonStateStyle.SetStroke(const AValue: TALInheritStrokeBrush);
begin
  FStroke.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALButtonStateStyle.SetTextSettings(const AValue: TALButtonStateStyleTextSettings);
begin
  FTextSettings.Assign(AValue);
end;

{*******************************************************************************}
procedure TALButtonStateStyle.SetShadow(const AValue: TALInheritShadow);
begin
  FShadow.Assign(AValue);
end;

{***********************************************}
function TALButtonStateStyle.GetInherit: Boolean;
begin
  Result := Fill.Inherit and
            Stroke.Inherit and
            TextSettings.Inherit and
            Shadow.Inherit;
end;

{**********************************************************}
procedure TALButtonStateStyle.FillChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************}
procedure TALButtonStateStyle.StrokeChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALButtonStateStyle.TextSettingsChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************}
procedure TALButtonStateStyle.ShadowChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************}
function TALButtonDisabledStateStyle.OpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, TControl.DefaultDisabledOpacity, TEpsilon.Scale);
end;

{********************************************************************}
procedure TALButtonDisabledStateStyle.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*********************************************}
constructor TALButtonDisabledStateStyle.Create;
begin
  inherited Create;
  FOpacity := TControl.DefaultDisabledOpacity;
end;

{****************************************************************}
procedure TALButtonDisabledStateStyle.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALButtonDisabledStateStyle then
      Opacity := TALButtonDisabledStateStyle(Source).Opacity
    else
      Opacity := TControl.DefaultDisabledOpacity;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*************************************}
constructor TALButtonStateStyles.Create;
begin
  inherited Create;
  //--
  FDisabled := TALButtonDisabledStateStyle.Create;
  FDisabled.OnChanged := DisabledChanged;
  //--
  FHovered := TALButtonStateStyle.Create;
  FHovered.OnChanged := HoveredChanged;
  //--
  FPressed := TALButtonStateStyle.Create;
  FPressed.OnChanged := PressedChanged;
  //--
  FFocused := TALButtonStateStyle.Create;
  FFocused.OnChanged := FocusedChanged;
end;

{*************************************}
destructor TALButtonStateStyles.Destroy;
begin
  ALFreeAndNil(FDisabled);
  ALFreeAndNil(FHovered);
  ALFreeAndNil(FPressed);
  ALFreeAndNil(FFocused);
  inherited Destroy;
end;

{******************************************************}
procedure TALButtonStateStyles.Assign(Source: TPersistent);
begin
  if Source is TALButtonStateStyles then begin
    BeginUpdate;
    Try
      Disabled.Assign(TALButtonStateStyles(Source).Disabled);
      Hovered.Assign(TALButtonStateStyles(Source).Hovered);
      Pressed.Assign(TALButtonStateStyles(Source).Pressed);
      Focused.Assign(TALButtonStateStyles(Source).Focused);
    Finally
      EndUpdate;
    End;
  end
  else
    inherited Assign(Source);
end;

{************************************************************************************}
procedure TALButtonStateStyles.SetDisabled(const AValue: TALButtonDisabledStateStyle);
begin
  FDisabled.Assign(AValue);
end;

{************************************************************************************}
procedure TALButtonStateStyles.SetHovered(const AValue: TALButtonStateStyle);
begin
  FHovered.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALButtonStateStyles.SetPressed(const AValue: TALButtonStateStyle);
begin
  FPressed.Assign(AValue);
end;

{*******************************************************************************************}
procedure TALButtonStateStyles.SetFocused(const AValue: TALButtonStateStyle);
begin
  FFocused.Assign(AValue);
end;

{**********************************************************}
procedure TALButtonStateStyles.DisabledChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************}
procedure TALButtonStateStyles.HoveredChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALButtonStateStyles.PressedChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALButtonStateStyles.FocusedChanged(ASender: TObject);
begin
  Change;
end;

{***********************************************}
constructor TALButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //--
  FHovered := False;
  FPressed := False;
  //--
  CanFocus := False;
  HitTest := True;
  AutoSize := True;
  Cursor := crHandPoint;
  //--
  FStateStyles := TALButtonStateStyles.Create;
  FStateStyles.OnChanged := StateStylesChanged;
  //--
  fBufDisabledDrawable := ALNullDrawable;
  fBufHoveredDrawable := ALNullDrawable;
  fBufPressedDrawable := ALNullDrawable;
  fBufFocusedDrawable := ALNullDrawable;
  //--
  Fill.DefaultKind := TBrushKind.Solid;
  Fill.DefaultColor := $ffe1e1e1;
  Fill.Kind := Fill.DefaultKind;
  Fill.Color := Fill.DefaultColor;
  //--
  Stroke.DefaultKind := TBrushKind.Solid;
  Stroke.DefaultColor := $ffadadad;
  Stroke.Kind := Stroke.DefaultKind;
  Stroke.Color := Stroke.DefaultColor;
  //--
  TextSettings.Font.DefaultWeight := TFontWeight.medium;
  TextSettings.Font.Weight := TextSettings.Font.DefaultWeight;
  TextSettings.DefaultHorzAlign := TALTextHorzAlign.center;
  TextSettings.HorzAlign := TextSettings.DefaultHorzAlign;
  //--
  Padding.DefaultValue := TRectF.create(12{Left}, 6{Top}, 12{Right}, 6{Bottom});
  Padding.Rect := Padding.DefaultValue;
end;

{***************************}
destructor TALButton.Destroy;
begin
  ALFreeAndNil(FStateStyles);
  inherited Destroy;
end;

{*************************}
procedure TALButton.Loaded;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _ConvertFontFamily(const AStateStyle: TALButtonStateStyle);
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
  Result := TALButtonTextSettings.Create;
end;

{********************************************************}
function TALButton.GetTextSettings: TALButtonTextSettings;
begin
  Result := TALButtonTextSettings(Inherited TextSettings);
end;

{**********************************************************************}
procedure TALButton.SetTextSettings(const Value: TALButtonTextSettings);
begin
  Inherited SetTextSettings(Value);
end;

{**********************************************************************************}
procedure TALButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FPressed := True;
  Repaint;
end;

{************************************************************}
procedure TALButton.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited MouseMove(Shift, X, Y);
  if not FHovered then
    Repaint;
  FHovered := True;
end;

{********************************************************************************}
procedure TALButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FPressed := False;
  Repaint;
end;

{*****************************}
procedure TALButton.DoMouseEnter;
begin
  inherited DoMouseEnter;
  FHovered := True;
  Repaint;
end;

{*****************************}
procedure TALButton.DoMouseLeave;
begin
  inherited DoMouseLeave;
  FHovered := False;
  Repaint;
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
procedure TALButton.SetStateStyles(const AValue: TALButtonStateStyles);
begin
  FStateStyles.Assign(AValue);
end;

{******************************************************}
procedure TALButton.StateStylesChanged(Sender: TObject);
begin
  clearBufDrawable;
  DisabledOpacity := StateStyles.Disabled.opacity;
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
              const AStateStyle: TALButtonStateStyle;
              var ABufDrawable: TALDrawable;
              var ABufDrawableRect: TRectF);
  begin
    if AStateStyle.Inherit then exit;
    if (not ALIsDrawableNull(ABufDrawable)) then exit;
    //--
    var LFont: TALFont;
    var LDecoration: TALTextDecoration;
    var LPrevFontSize: Single := 0;
    var LPrevFontFamily: String := '';
    var LPrevFontColor: TAlphaColor := TalphaColors.Null;
    var LPrevFontOnchanged: TNotifyEvent := nil;
    if not AStateStyle.TextSettings.inherit then begin
      LFont := AStateStyle.TextSettings.Font;
      LPrevFontOnchanged := LFont.OnChanged;
      LPrevFontSize := LFont.Size;
      LPrevFontFamily := LFont.Family;
      LPrevFontColor := Lfont.Color;
      LFont.OnChanged := nil;
      if SameValue(LFont.Size, 0, TEpsilon.FontSize) then LFont.Size := TextSettings.Font.Size;
      if LFont.Family = '' then LFont.Family := TextSettings.Font.family;
      if LFont.Color = TalphaColors.Null then LFont.Color := TextSettings.Font.Color;
      LDecoration := AStateStyle.TextSettings.Decoration;
    end
    else begin
      LFont := TextSettings.Font;
      LDecoration := TextSettings.Decoration;
    end;
    try
      var LFill: TBrush;
      if not AStateStyle.Fill.inherit then LFill := AStateStyle.Fill
      else LFill := Fill;
      //--
      var LStroke: TStrokeBrush;
      if not AStateStyle.Stroke.inherit then LStroke := AStateStyle.Stroke
      else LStroke := Stroke;
      //--
      var LShadow: TALShadow;
      if not AStateStyle.Shadow.inherit then LShadow := AStateStyle.Shadow
      else LShadow := Shadow;
      //--
      var LText: String;
      if AStateStyle.Text = '' then LText := Text
      else LText := AStateStyle.Text;
      //--
      var LTextBroken: Boolean;
      var LAllTextDrawn: Boolean;
      var LElements: TALTextElements;
      CreateBufDrawable(
        ABufDrawable, // var ABufDrawable: TALDrawable;
        ABufDrawableRect, // var ABufDrawableRect: TRectF;
        LTextBroken, // var ABufTextBroken: Boolean;
        LAllTextDrawn, // var ABufAllTextDrawn: Boolean;
        LElements, // var ABufElements: TALTextElements;
        LText, // const AText: String;
        LFont, // const AFont: TALFont;
        LDecoration, // const ADecoration: TALTextDecoration;
        LFont, // const AEllipsisFont: TALFont;
        LDecoration, // const AEllipsisDecoration: TALTextDecoration;
        LFill, // const AFill: TBrush;
        LStroke, // const AStroke: TStrokeBrush;
        LShadow); // const AShadow: TALShadow);

      // The shadow effect is not included in the fBufDrawableRect rectangle's dimensions.
      // However, the fBufDrawableRect rectangle is offset by the shadow's dx and dy values,
      // if a shadow is applied, to adjust for the visual shift caused by the shadow.
      var LMainDrawableRect := BufDrawableRect;
      LMainDrawableRect.Offset(-LMainDrawableRect.Left, -LMainDrawableRect.Top);
      var LCenteredRect := ABufDrawableRect.CenterAt(LMainDrawableRect);
      ABufDrawableRect.Offset(-2*ABufDrawableRect.Left, -2*ABufDrawableRect.Top);
      ABufDrawableRect.Offset(LCenteredRect.Left, LCenteredRect.top);
    finally
      if not AStateStyle.TextSettings.inherit then begin
        LFont.Size := LPrevFontSize;
        LFont.Family := LPrevFontFamily;
        Lfont.Color := LPrevFontColor;
        LFont.OnChanged := LPrevFontOnchanged;
      end;
    end;
  end;

begin
  inherited MakeBufDrawable;
  //--
  if Not Enabled then begin
    _MakeBufDrawable(
      StateStyles.Disabled, // const AStateStyle: TALButtonStateStyle;
      FBufDisabledDrawable, // var ABufDrawable: TALDrawable;
      FBufDisabledDrawableRect); // var ABufDrawableRect: TRectF;
  end
  else if FPressed then begin
    _MakeBufDrawable(
      StateStyles.Pressed, // const AStateStyle: TALButtonStateStyle;
      FBufPressedDrawable, // var ABufDrawable: TALDrawable;
      FBufPressedDrawableRect); // var ABufDrawableRect: TRectF;
  end
  else if FHovered then begin
    _MakeBufDrawable(
      StateStyles.Hovered, // const AStateStyle: TALButtonStateStyle;
      FBufHoveredDrawable, // var ABufDrawable: TALDrawable;
      FBufHoveredDrawableRect); // var ABufDrawableRect: TRectF;
  end
  else if IsFocused then begin
    _MakeBufDrawable(
      StateStyles.Focused, // const AStateStyle: TALButtonStateStyle;
      FBufFocusedDrawable, // var ABufDrawable: TALDrawable;
      FBufFocusedDrawableRect); // var ABufDrawableRect: TRectF;
  end;
end;

{************************}
procedure TALButton.Paint;
begin

  MakeBufDrawable;

  var LDrawable: TALDrawable;
  var LDrawableRect: TRectF;

  if Not Enabled then begin
    LDrawable := fBufDisabledDrawable;
    LDrawableRect := fBufDisabledDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
    end;
  end
  //--
  else if FPressed then begin
    LDrawable := fBufPressedDrawable;
    LDrawableRect := fBufPressedDrawableRect;
    if ALIsDrawableNull(LDrawable) then begin
      LDrawable := BufDrawable;
      LDrawableRect := BufDrawableRect;
    end;
  end
  //--
  else if FHovered then begin
    LDrawable := fBufHoveredDrawable;
    LDrawableRect := fBufHoveredDrawableRect;
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
  else begin
    LDrawable := BufDrawable;
    LDrawableRect := BufDrawableRect;
  end;

  ALDrawDrawable(
    Canvas, // const ACanvas: Tcanvas;
    LDrawable, // const ADrawable: TALDrawable;
    LDrawableRect.TopLeft, // const ATopLeft: TpointF;
    AbsoluteOpacity); // const AOpacity: Single);

end;

{*****************************************************************************}
procedure ALApplyThemeToButton(const AButton: TALButton; const ATheme: String);
begin
  With AButton do begin

    {$REGION 'Default'}
    if ATheme = 'Default' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := padding.DefaultValue;
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := 0;
      YRadius := 0;
      Fill.Kind := Fill.DefaultKind;
      Fill.Color := Fill.DefaultColor;
      Stroke.Kind := Stroke.DefaultKind;
      Stroke.Color := Stroke.DefaultColor;
      Stroke.Thickness := 1;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14;
      TextSettings.Font.Weight := TFontWeight.medium;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := TControl.DefaultDisabledOpacity;
      StateStyles.Disabled.Fill.reset;
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.reset;
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.reset;
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;

      //--Pressed--
      StateStyles.Pressed.Fill.reset;
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.reset;
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Windows'}
    else if ATheme = 'Windows' then begin
      //--Enabled (default)--
      Canfocus := True;
      AutoSize := True;
      padding.Rect := padding.DefaultValue;
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := 0;
      YRadius := 0;
      Fill.Kind := Fill.DefaultKind;
      Fill.Color := Fill.DefaultColor;
      Stroke.Kind := Stroke.DefaultKind;
      Stroke.Color := Stroke.DefaultColor;
      Stroke.Thickness := 1;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14;
      TextSettings.Font.Weight := TFontWeight.medium;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := TControl.DefaultDisabledOpacity;
      StateStyles.Disabled.Fill.reset;
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.reset;
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Color := $FFe5f1fb;
      StateStyles.Hovered.Stroke.Assign(Stroke);
      StateStyles.Hovered.Stroke.Inherit := False;
      StateStyles.Hovered.Stroke.Color := $FF0078d7;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;

      //--Pressed--
      StateStyles.Pressed.Fill.assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Color := $FFcce4f7;
      StateStyles.Pressed.Stroke.Assign(Stroke);
      StateStyles.Pressed.Stroke.Inherit := False;
      StateStyles.Pressed.Stroke.Color := $FF005499;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.reset;
      StateStyles.Focused.Stroke.Assign(Stroke);
      StateStyles.focused.Stroke.Inherit := False;
      StateStyles.focused.Stroke.Color := $FF0078d7;
      StateStyles.focused.Stroke.Thickness := 2;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Light.Filled'}
    //https://m3.material.io/components/buttons/specs#cbfd91a6-d688-4be7-9a69-672549de3ea9
    else if ATheme = 'Material3.Light.Filled' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      Stroke.Kind := TBrushKind.none;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FFFFFFFF; // md.sys.color.on-primary // md.ref.palette.primary100
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorOpacity($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.Assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFFFFFFF, 0.08); // md.sys.color.on-primary / md.ref.palette.primary100
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.enabled := True;
      StateStyles.Hovered.Shadow.Color := ALSetColorOpacity($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := 2;
      StateStyles.Hovered.Shadow.OffsetY := 1;

      //--Pressed--
      StateStyles.Pressed.Fill.Assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFFFFFFF, 0.12); // md.sys.color.on-primary / md.ref.palette.primary100
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Assign(Fill);
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFFFFFFF, 0.12); // md.sys.color.on-primary / md.ref.palette.primary100
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Light.Outlined'}
    //https://m3.material.io/components/buttons/specs#4a0c06da-0b2f-47de-a583-97e0ae80b5a5
    else if ATheme = 'Material3.Light.Outlined' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.none;
      Stroke.Kind := TBrushKind.Solid;
      Stroke.Color := $FF79747E; // md.sys.color.outline / md.ref.palette.neutral-variant50
      Stroke.Thickness := 1;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary / md.ref.palette.primary40
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Reset;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALSetColorOpacity($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Kind := TBrushKind.Solid;
      StateStyles.Hovered.Fill.Color := ALSetColorOpacity($FF6750A4, 0.08); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;

      //--Pressed--
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Kind := TBrushKind.Solid;
      StateStyles.Pressed.Fill.Color := ALSetColorOpacity($FF6750A4, 0.12); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.Kind := TBrushKind.Solid;
      StateStyles.Focused.Fill.Color := ALSetColorOpacity($FF6750A4, 0.12); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.inherit := False;
      StateStyles.Focused.Stroke.Color := $FF6750A4;  // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Light.Text'}
    //https://m3.material.io/components/buttons/specs#398d84eb-fc8a-4c8a-bfb4-82d2e85dee4d
    else if ATheme = 'Material3.Light.Text' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(12{Left}, 12{Top}, 12{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.none;
      Stroke.Kind := TBrushKind.none;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary // md.ref.palette.primary40
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Reset;
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.Assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.kind := TBrushKind.Solid;
      StateStyles.Hovered.Fill.Color := ALSetColorOpacity($FF6750A4, 0.08); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;

      //--Pressed--
      StateStyles.Pressed.Fill.Assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.kind := TBrushKind.Solid;
      StateStyles.Pressed.Fill.Color := ALSetColorOpacity($FF6750A4, 0.12); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Assign(Fill);
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.kind := TBrushKind.Solid;
      StateStyles.Focused.Fill.Color := ALSetColorOpacity($FF6750A4, 0.12); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Light.Elevated'}
    //https://m3.material.io/components/buttons/specs#c75be779-5a59-4748-98d4-e47fc888d0b1
    else if ATheme = 'Material3.Light.Elevated' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := $FFF7F2FA; // md.sys.color.surface-container-low / md.ref.palette.neutral96
      Stroke.Kind := TBrushKind.none;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FF6750A4; // md.sys.color.primary // md.ref.palette.primary40
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;
      Shadow.enabled := True;
      Shadow.Color := ALSetColorOpacity($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      Shadow.blur := 2;
      Shadow.OffsetY := 1;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorOpacity($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Shadow.Reset;
      StateStyles.Disabled.Shadow.inherit := False;

      //--Hovered--
      StateStyles.Hovered.Fill.Assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF6750A4, 0.08); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.enabled := True;
      StateStyles.Hovered.Shadow.Color := ALSetColorOpacity($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := 3;
      StateStyles.Hovered.Shadow.OffsetY := 1;

      //--Pressed--
      StateStyles.Pressed.Fill.Assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF6750A4, 0.12); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Assign(Fill);
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF6750A4, 0.12); // md.sys.color.primary / md.ref.palette.primary40
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Light.Tonal'}
    //https://m3.material.io/components/buttons/specs#6ce8b926-87c4-4600-9bec-5deb4aaa65d8
    else if ATheme = 'Material3.Light.Tonal' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := $FFE8DEF8; // md.sys.color.secondary-container / md.ref.palette.secondary90
      Stroke.Kind := TBrushKind.none;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FF1D192B; // md.sys.color.on-secondary-container // md.ref.palette.secondary10
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorOpacity($FF1D1B20, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FF1D1B20, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral10
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.Assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF1D192B, 0.08); // md.sys.color.on-secondary-container / md.ref.palette.secondary10
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.enabled := True;
      StateStyles.Hovered.Shadow.Color := ALSetColorOpacity($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := 2;
      StateStyles.Hovered.Shadow.OffsetY := 1;

      //--Pressed--
      StateStyles.Pressed.Fill.Assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF1D192B, 0.12); // md.sys.color.on-secondary-container / md.ref.palette.secondary10
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Assign(Fill);
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF1D192B, 0.12); // md.sys.color.on-secondary-container / md.ref.palette.secondary10
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Dark.Filled'}
    //https://m3.material.io/components/buttons/specs#cbfd91a6-d688-4be7-9a69-672549de3ea9
    else if ATheme = 'Material3.Dark.Filled' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      Stroke.Kind := TBrushKind.none;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FF381E72; // md.sys.color.on-primary / md.ref.palette.primary20
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorOpacity($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.Assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF381E72, 0.08); // md.sys.color.on-primary / md.ref.palette.primary20
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.enabled := True;
      StateStyles.Hovered.Shadow.Color := ALSetColorOpacity($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := 2;
      StateStyles.Hovered.Shadow.OffsetY := 1;

      //--Pressed--
      StateStyles.Pressed.Fill.Assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF381E72, 0.12); // md.sys.color.on-primary / md.ref.palette.primary20
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Assign(Fill);
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FF381E72, 0.12); // md.sys.color.on-primary / md.ref.palette.primary20
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Dark.Outlined'}
    //https://m3.material.io/components/buttons/specs#4a0c06da-0b2f-47de-a583-97e0ae80b5a5
    else if ATheme = 'Material3.Dark.Outlined' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.none;
      Stroke.Kind := TBrushKind.Solid;
      Stroke.Color := $FF938F99; // md.sys.color.outline / md.ref.palette.neutral-variant60
      Stroke.Thickness := 1;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary / md.ref.palette.primary80
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Reset;
      StateStyles.Disabled.Stroke.Assign(Stroke);
      StateStyles.Disabled.Stroke.Inherit := False;
      StateStyles.Disabled.Stroke.Color := ALSetColorOpacity($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Kind := TBrushKind.Solid;
      StateStyles.Hovered.Fill.Color := ALSetColorOpacity($FFD0BCFF, 0.08); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;

      //--Pressed--
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Kind := TBrushKind.Solid;
      StateStyles.Pressed.Fill.Color := ALSetColorOpacity($FFD0BCFF, 0.12); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.Kind := TBrushKind.Solid;
      StateStyles.Focused.Fill.Color := ALSetColorOpacity($FFD0BCFF, 0.12); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Focused.Stroke.assign(Stroke);
      StateStyles.Focused.Stroke.inherit := False;
      StateStyles.Focused.Stroke.Color := $FFD0BCFF;  // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Dark.Text'}
    //https://m3.material.io/components/buttons/specs#398d84eb-fc8a-4c8a-bfb4-82d2e85dee4d
    else if ATheme = 'Material3.Dark.Text' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(12{Left}, 12{Top}, 12{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.none;
      Stroke.Kind := TBrushKind.none;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary // md.ref.palette.primary80
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Reset;
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.Assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.kind := TBrushKind.Solid;
      StateStyles.Hovered.Fill.Color := ALSetColorOpacity($FFD0BCFF, 0.08); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;

      //--Pressed--
      StateStyles.Pressed.Fill.Assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.kind := TBrushKind.Solid;
      StateStyles.Pressed.Fill.Color := ALSetColorOpacity($FFD0BCFF, 0.12); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Assign(Fill);
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.kind := TBrushKind.Solid;
      StateStyles.Focused.Fill.Color := ALSetColorOpacity($FFD0BCFF, 0.12); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Dark.Elevated'}
    //https://m3.material.io/components/buttons/specs#c75be779-5a59-4748-98d4-e47fc888d0b1
    else if ATheme = 'Material3.Dark.Elevated' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := $FF1D1B20; // md.sys.color.surface-container-low / md.ref.palette.neutral10
      Stroke.Kind := TBrushKind.none;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FFD0BCFF; // md.sys.color.primary // md.ref.palette.primary80
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;
      Shadow.enabled := True;
      Shadow.Color := ALSetColorOpacity($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      Shadow.blur := 2;
      Shadow.OffsetY := 1;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorOpacity($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Shadow.Reset;
      StateStyles.Disabled.Shadow.inherit := False;

      //--Hovered--
      StateStyles.Hovered.Fill.Assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFD0BCFF, 0.08); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.enabled := True;
      StateStyles.Hovered.Shadow.Color := ALSetColorOpacity($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := 3;
      StateStyles.Hovered.Shadow.OffsetY := 1;

      //--Pressed--
      StateStyles.Pressed.Fill.Assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFD0BCFF, 0.12); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Assign(Fill);
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFD0BCFF, 0.12); // md.sys.color.primary / md.ref.palette.primary80
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    {$REGION 'Material3.Dark.Tonal'}
    //https://m3.material.io/components/buttons/specs#6ce8b926-87c4-4600-9bec-5deb4aaa65d8
    else if ATheme = 'Material3.Dark.Tonal' then begin
      //--Enabled (default)--
      Canfocus := False;
      AutoSize := True;
      padding.Rect := TRectF.Create(24{Left}, 12{Top}, 24{Right}, 12{Bottom});
      Corners := AllCorners;
      Sides := AllSides;
      XRadius := -50;
      YRadius := -50;
      Fill.Kind := TBrushKind.Solid;
      Fill.Color := $FF4A4458; // md.sys.color.secondary-container / md.ref.palette.secondary30
      Stroke.Kind := TBrushKind.none;
      var LPrevIsHtml := TextSettings.IsHtml;
      TextSettings.Reset;
      TextSettings.IsHtml := LPrevIsHtml;
      TextSettings.Font.Size := 14; // icon size: 18dp
      TextSettings.Font.Weight := TFontWeight.medium;
      TextSettings.Font.Color := $FFE8DEF8; // md.sys.color.on-secondary-container // md.ref.palette.secondary90
      TextSettings.LetterSpacing := 0.1;
      Shadow.Reset;

      //--Disabled--
      StateStyles.Disabled.Opacity := 1;
      StateStyles.Disabled.Fill.Assign(Fill);
      StateStyles.Disabled.Fill.Inherit := False;
      StateStyles.Disabled.Fill.Color := ALSetColorOpacity($FFE6E0E9, 0.12); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Stroke.Reset;
      StateStyles.Disabled.TextSettings.Assign(TextSettings);
      StateStyles.Disabled.TextSettings.Inherit := False;
      StateStyles.Disabled.TextSettings.Font.Color := ALSetColorOpacity($FFE6E0E9, 0.38); // md.sys.color.on-surface / md.ref.palette.neutral90
      StateStyles.Disabled.Shadow.Reset;

      //--Hovered--
      StateStyles.Hovered.Fill.Assign(Fill);
      StateStyles.Hovered.Fill.Inherit := False;
      StateStyles.Hovered.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFE8DEF8, 0.08); // md.sys.color.on-secondary-container / md.ref.palette.secondary90
      StateStyles.Hovered.Stroke.Reset;
      StateStyles.Hovered.TextSettings.reset;
      StateStyles.Hovered.Shadow.Reset;
      StateStyles.Hovered.Shadow.Inherit := False;
      StateStyles.Hovered.Shadow.enabled := True;
      StateStyles.Hovered.Shadow.Color := ALSetColorOpacity($FF000000, 0.50); // md.sys.color.shadow / md.ref.palette.neutral0
      StateStyles.Hovered.Shadow.blur := 2;
      StateStyles.Hovered.Shadow.OffsetY := 1;

      //--Pressed--
      StateStyles.Pressed.Fill.Assign(Fill);
      StateStyles.Pressed.Fill.Inherit := False;
      StateStyles.Pressed.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFE8DEF8, 0.12); // md.sys.color.on-secondary-container / md.ref.palette.secondary90
      StateStyles.Pressed.Stroke.Reset;
      StateStyles.Pressed.TextSettings.reset;
      StateStyles.Pressed.Shadow.Reset;

      //--Focused--
      StateStyles.Focused.Fill.Assign(Fill);
      StateStyles.Focused.Fill.Inherit := False;
      StateStyles.Focused.Fill.Color := ALBlendColorWithOpacity(Fill.Color, $FFE8DEF8, 0.12); // md.sys.color.on-secondary-container / md.ref.palette.secondary90
      StateStyles.Focused.Stroke.Reset;
      StateStyles.Focused.TextSettings.reset;
      StateStyles.Focused.Shadow.Reset;
    end
    {$ENDREGION}

    else
      raise Exception.Create('Error 9E2163E6-1342-4F02-B571-FC0276AD5BED');

  end;
end;

{*****************}
procedure Register;
begin
  RegisterComponents('Alcinoe', [TALAniIndicator, TALScrollBar, TALTrackBar, TALRangeTrackBar, TALCheckBox, TALRadioButton, TALSwitch, TALButton]);
  {$IFDEF ALDPK}
  UnlistPublishedProperty(TALTrackThumbGlyph, 'Locked');
  UnlistPublishedProperty(TALTrackThumbGlyph, 'StyleName');
  //--
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
  //--
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
  //--
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
  //--
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
  //--
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
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALAniIndicator, TALScrollBar, TALTrackBar, TALRangeTrackBar, TALCheckBox, TALRadioButton, TALSwitch, TALButton]);

end.
