unit ALFmxStdCtrls;

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
     ALFmxInertialMovement,
     ALFmxImgList,
     ALFmxObjects;

type

  {~~~~~~~~~~~~~~~~~~~~~}
  TALCustomTrack = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTrackThumbGlyph = class(TALGlyph)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align default TalignLayout.Client;
    property autohide default true;
    property Locked default True;
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
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALCheckBox = class(TControl, IGlyph)
  public const
    DesignBorderColor = $A080D080;
  private
    FScreenScale: single;
    fdoubleBuffered: boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    fBufBitmap: TTexture;
    {$ELSE}
    fBufBitmap: Tbitmap;
    {$ENDIF}
    fBufBitmapRect: TRectF;
    fBufSize: TsizeF;
    [weak] fBufImages: TCustomImageList;
    FbufImageIndex: TImageIndex;
    //-----
    FPressing: Boolean;
    FOnChange: TNotifyEvent;
    FIsPressed: Boolean;
    FIsChecked: Boolean;
    FImageCheckedLink: TImageLink;
    FImageUncheckedLink: TImageLink;
    FisImagesChanged: boolean;
    FStretch: Boolean;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    FOpenGLContextLostId: integer;
    FOpenGLContextResetId: Integer;
    procedure OpenGLContextLostHandler(const Sender : TObject; const Msg : TMessage);
    procedure OpenGLContextResetHandler(const Sender : TObject; const Msg : TMessage); // << because of https://quality.embarcadero.com/browse/RSP-16142
    {$ENDIF}
    procedure SetdoubleBuffered(const Value: Boolean);
    function GetImages: TCustomImageList;
    procedure SetImages(const Value: TCustomImageList);
    { IGlyph }
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
    function GetImageUncheckedIndex: TImageIndex;
    procedure SetImageUncheckedIndex(const Value: TImageIndex);
    function GetImageList: TBaseImageList; inline;
    procedure SetImageList(const Value: TBaseImageList);
    function IGlyph.GetImages = GetImageList;
    procedure IGlyph.SetImages = SetImageList;
    procedure NonBufferedPaint;
    procedure SetStretch(const Value: Boolean);
  protected
    procedure Paint; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    property BufBitmap: TTexture read fBufBitmap;
    {$ELSE}
    property BufBitmap: Tbitmap read fBufBitmap;
    {$ENDIF}
    procedure DoEndUpdate; override;
    procedure DoChanged; virtual;
    function ImageCheckedIndexStored: Boolean; virtual;
    function ImageUncheckedIndexStored: Boolean; virtual;
    function ImagesStored: Boolean; virtual;
    function GetDefaultSize: TSizeF; override;
    function GetIsChecked: Boolean; virtual;
    procedure SetIsChecked(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    function MakeBufBitmap: TTexture; virtual;
    {$ELSE}
    function MakeBufBitmap: Tbitmap; virtual;
    {$ENDIF}
    procedure clearBufBitmap; virtual;
    procedure ImagesChanged;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  published
    property doubleBuffered: Boolean read fdoubleBuffered write setdoubleBuffered default true;
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
    property ImageCheckedIndex: TImageIndex read GetImageIndex write SetImageIndex stored ImageCheckedIndexStored;
    property ImageUncheckedIndex: TImageIndex read GetImageUncheckedIndex write SetImageUncheckedIndex stored ImageUncheckedIndexStored;
    property Images: TCustomImageList read GetImages write SetImages stored ImagesStored;
    property Stretch: Boolean read FStretch write SetStretch default True;
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
  end;

  {*********************************}
  TAlRadioButton = class(TALCheckBox)
  private
    FGroupName: string;
    fMandatory: boolean;
    function GetGroupName: string;
    procedure SetGroupName(const Value: string);
    function GroupNameStored: Boolean;
    procedure GroupMessageCall(const Sender : TObject; const M : TMessage);
  protected
    procedure SetIsChecked(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property GroupName: string read GetGroupName write SetGroupName stored GroupNameStored nodefault;
    property Mandatory: Boolean read fMandatory write fMandatory default false;
  end;

{$IFDEF debug}
var
  AlDebugCheckBoxMakeBufBitmapCount: integer;
  AlDebugCheckBoxMakeBufBitmapStopWatch: TstopWatch;
{$endif}

procedure Register;

implementation

uses System.SysUtils,
     system.Math.Vectors,
     {$IFDEF ALDPK}
     DesignIntf,
     {$ENDIF}
     {$IF DEFINED(IOS) or DEFINED(ANDROID)}
     FMX.Canvas.GPU,
     ALFmxTypes3D,
     {$ENDIF}
     FMX.Platform,
     fmx.consts,
     fmx.utils,
     AlCommon,
     ALFmxCommon;

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
  autohide := true;
  locked := True;
end;

{***************************************************************************************************************************}
constructor TALTrackThumb.Create(const ATrack: TALCustomTrack; const aValueRange: TValueRange; const aWithGlyphObj: boolean);
begin
  inherited create(ATrack);
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
  lSize := FMinThumbSize;
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
  FValueRange.Max := Value;
end;

{***************************************************}
procedure TALCustomTrack.SetMin(const Value: Single);
begin
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

{********************************************}
function TALRangeTrackBar.GetMaxValue: Single;
begin
  Result := FMaxValueRange.Value;
end;

{****************************************************}
procedure TALRangeTrackBar.SetMaxValue(Value: Single);
begin
  FMaxValueRange.Value := Value;
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
  inherited;
  FMaxValueRange.Max := Value;
end;

{*****************************************************}
procedure TALRangeTrackBar.SetMin(const Value: Single);
begin
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
  fdoubleBuffered := true;
  fBufBitmap := nil;
  SetAcceptsControls(False);
  CanFocus := True;
  AutoCapture := True;
  FPressing:= false;
  FOnChange := nil;
  FIsPressed := False;
  FIsChecked := False;
  FImageCheckedLink := TGlyphImageLink.Create(Self);
  FImageUncheckedLink := TGlyphImageLink.Create(Self);
  FisImagesChanged := False;
  FStretch := True;
  {$IF defined(ANDROID) or defined(IOS)}
  FOpenGLContextLostId := TMessageManager.DefaultManager.SubscribeToMessage(TContextLostMessage, OpenGLContextLostHandler);
  FOpenGLContextResetId := TMessageManager.DefaultManager.SubscribeToMessage(TContextResetMessage, OpenGLContextResetHandler);
  {$ENDIF}
end;

{*****************************}
destructor TALCheckbox.Destroy;
begin
  clearBufBitmap;
  AlFreeAndNil(FImageCheckedLink);   // >> will call disposeOf if necessary
  AlFreeAndNil(FImageUncheckedLink); // >> will call disposeOf if necessary
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

{********************************}
procedure TALCheckbox.DoEndUpdate;
begin
  inherited;
  if FisImagesChanged then
    repaint;
end;

{**********************************}
procedure TALCheckbox.ImagesChanged;
begin
  if ([csLoading, csDestroying, csUpdating] * ComponentState = []) and not IsUpdating then begin
    repaint;
    FisImagesChanged := False;
  end
  else FisImagesChanged := True;
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

var aImageIndex: TimageIndex;
    {$IF defined(ANDROID) or defined(IOS)}
    aBitmap: TBitmap;
    aBitmapSize: TSize;
    {$ENDIF}

begin

  if IsChecked then aImageIndex := ImageCheckedIndex
  else aImageIndex := ImageUncheckedIndex;

  if ([csLoading, csDestroying, csDesigning] * ComponentState <> []) or
     (not fdoubleBuffered) or
     (Scene = nil) or
     (SameValue(Size.Size.cx, 0, TEpsilon.position)) or
     (SameValue(Size.Size.cy, 0, TEpsilon.position)) or
     (Images = nil) or
     (aImageIndex = -1) then begin
    clearBufBitmap;
    exit(nil);
  end;

  if (fBufBitmap <> nil) and
     (SameValue(fBufSize.cx, Size.Size.cx, TEpsilon.position)) and
     (SameValue(fBufSize.cy, Size.Size.cy, TEpsilon.position)) and
     (fBufImages = Images) and
     (FbufImageIndex = aImageIndex) then exit(fBufBitmap);

  clearBufBitmap;
  fBufSize := Size.Size;
  fBufImages := Images;
  FbufImageIndex := aImageIndex;

  {$IFDEF debug}
  ALLog('TALCheckbox.MakeBufBitmap', 'TALCheckbox.MakeBufBitmap', TalLogType.verbose);
  inc(AlDebugCheckBoxMakeBufBitmapCount);
  AlDebugCheckBoxMakeBufBitmapStopWatch.Start;
  try
  {$endif}

  {$IF defined(ANDROID) or defined(IOS)}

  //init aBitmapSize / aBitmap / fBufBitmapRect
  aBitmapSize := TSize.Create(0, 0);
  aBitmap := nil;
  fBufBitmapRect := ALAlignDimensionToPixelRound(LocalRect, FScreenScale); // to have the pixel aligned width and height
  if (Images <> nil) and
     (fBufBitmapRect.Width >= 1) and
     (fBufBitmapRect.Height >= 1) and
     (aImageIndex <> -1) and
     ([csLoading, csUpdating, csDestroying] * Images.ComponentState = []) then begin
    aBitmapSize := TSize.Create(Round(fBufBitmapRect.Width * FScreenScale), Round(fBufBitmapRect.Height * FScreenScale));
    if not Stretch then Images.BestSize(aImageIndex, aBitmapSize);
    aBitmap := Images.Bitmap(aBitmapSize, aImageIndex)
  end;

  if aBitmap <> nil then begin

    //init fBufBitmapRect
    fBufBitmapRect := TRectF.Create(0,
                                    0,
                                    aBitmap.Width / FScreenScale,
                                    aBitmap.Height/ FScreenScale).CenterAt(fBufBitmapRect);

    //convert the aBitmapSurface to texture
    //it's important to make a copy of the aBitmap because it's could be destroyed by the TimageList if
    //their is not anymore enalf of place in it's own caching system
    fBufBitmap := TALTexture.Create(True{aVolatile});
    try
      fBufBitmap.Assign(aBitmap);
    except
      ALFreeAndNil(fBufBitmap);
      raise;
    end;

  end;

  {$ENDIF}

  result := fBufBitmap;

  {$IFDEF debug}
  finally
    AlDebugCheckBoxMakeBufBitmapStopWatch.Stop;
  end;
  {$endif}
end;

{*************************************}
procedure TALCheckbox.NonBufferedPaint;
const
  MinCrossSize = 3;
  MaxCrossSize = 13;
var
  TextRect, ImgRect, BitmapRect: TRectF;
  CrossSize: Single;
  Bitmap: TBitmap;
  BitmapSize: TSize;
  aImageIndex: TimageIndex;
begin
  if IsChecked then aImageIndex := ImageCheckedIndex
  else aImageIndex := ImageUncheckedIndex;
  if [csLoading, csDestroying] * ComponentState = [] then
  begin
    BitmapSize := TSize.Create(0, 0);
    Bitmap := nil;
    ImgRect := LocalRect;
    if (Images <> nil) and (ImgRect.Width >= 1) and (ImgRect.Height >= 1) and (aImageIndex <> -1) and
      ([csLoading, csUpdating, csDestroying] * Images.ComponentState = []) then
    begin
      BitmapSize := TSize.Create(Round(ImgRect.Width * FScreenScale), Round(ImgRect.Height * FScreenScale));
      Images.BestSize(aImageIndex, BitmapSize);
      Bitmap := Images.Bitmap(BitmapSize, aImageIndex)
    end;
    if (csDesigning in ComponentState) and not Locked then
      DrawDesignBorder(DesignBorderColor, DesignBorderColor);
    if Bitmap <> nil then
    begin
      BitmapRect := TRectF.Create(0, 0, Bitmap.Width, Bitmap.Height);
      ImgRect := TRectF.Create(CenteredRect(ImgRect.Round, TRectF.Create(0, 0, Bitmap.Width / FScreenScale,
        Bitmap.Height/ FScreenScale).Round));
      Canvas.DrawBitmap(Bitmap, BitmapRect, ImgRect, AbsoluteOpacity, True);
    end;
    if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
    begin
      TextRect := LocalRect;
      TextRect.Inflate(0.5, 0.5);
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.Stroke.Color := TAlphaColorRec.Darkgray;
      Canvas.Stroke.Dash := TStrokeDash.Solid;
      CrossSize := Trunc(Min(MaxCrossSize, Min(TextRect.Width, TextRect.Height) / 6)) + 1;
      if CrossSize >= MinCrossSize then
      begin
        TextRect.TopLeft.Offset(2, 2);
        TextRect.BottomRight := TextRect.TopLeft;
        TextRect.BottomRight.Offset(CrossSize, CrossSize);
        if Bitmap = nil then
        begin
          if Images = nil then
            Canvas.Stroke.Color := TAlphaColorRec.Red;
          Canvas.DrawLine(TextRect.TopLeft, TextRect.BottomRight, AbsoluteOpacity);
          Canvas.DrawLine(TPointF.Create(TextRect.Right, TextRect.Top), TPointF.Create(TextRect.Left, TextRect.Bottom),
            AbsoluteOpacity);
          TextRect := TRectF.Create(TextRect.Left, TextRect.Bottom, Width, Height);
        end;
        if aImageIndex <> -1 then
        begin
          Canvas.Font.Family := 'Small Font';
          Canvas.Font.Size := 7;
          TextRect.Bottom := TextRect.Top + Canvas.TextHeight(Inttostr(aImageIndex));
          if TextRect.Bottom <= Height then
          begin
            Canvas.Fill.Color := TAlphaColorRec.Darkgray;
            Canvas.FillText(TextRect, Inttostr(aImageIndex), False, AbsoluteOpacity, [], TTextAlign.Leading,
              TTextAlign.Leading);
          end;
        end;
      end;
    end;
  end;
end;

{**************************}
procedure TALCheckbox.Paint;
begin

  MakeBufBitmap;

  if fBufBitmap = nil then begin
    NonBufferedPaint;
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

{************************************************************}
procedure TALCheckbox.SetdoubleBuffered(const Value: Boolean);
begin
  if Value <> fDoubleBuffered then begin
    fDoubleBuffered := value;
    if not fDoubleBuffered then clearbufBitmap;
  end;
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

{************************************************}
function TALCheckbox.GetImageList: TBaseImageList;
begin
  Result := GetImages;
end;

{**************************************************************}
procedure TALCheckbox.SetImageList(const Value: TBaseImageList);
begin
  ValidateInheritance(Value, TCustomImageList);
  SetImages(TCustomImageList(Value));
end;

{******************************************}
function TALCheckbox.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(22, 22);
end;

{**********************************************}
function TALCheckbox.GetImageIndex: TImageIndex;
begin
  Result := FImageCheckedLink.ImageIndex;
end;

{************************************************************}
procedure TALCheckbox.SetImageIndex(const Value: TImageIndex);
begin
  if FImageCheckedLink.ImageIndex <> Value then
    FImageCheckedLink.ImageIndex := Value;
end;

{*******************************************************}
function TALCheckbox.GetImageUncheckedIndex: TImageIndex;
begin
  Result := FImageUncheckedLink.ImageIndex;
end;

{*********************************************************************}
procedure TALCheckbox.SetImageUncheckedIndex(const Value: TImageIndex);
begin
  if FImageUncheckedLink.ImageIndex <> Value then
    FImageUncheckedLink.ImageIndex := Value;
end;

{****************************************************}
function TALCheckbox.ImagecheckedIndexStored: Boolean;
begin
  Result := (ImageCheckedIndex <> -1);
end;

{******************************************************}
function TALCheckbox.ImageUncheckedIndexStored: Boolean;
begin
  Result := (ImageUncheckedIndex <> -1);
end;

{***********************************************}
function TALCheckbox.GetImages: TCustomImageList;
begin
  Result := TCustomImageList(FImageCheckedLink.Images);
end;

{*************************************************************}
procedure TALCheckbox.SetImages(const Value: TCustomImageList);
begin
  FImageCheckedLink.Images := Value;
  FImageUncheckedLink.Images := Value;
end;

{*****************************************}
function TALCheckbox.ImagesStored: Boolean;
begin
  Result := Images <> nil;
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

{*****************************************************}
procedure TALCheckbox.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Repaint;
  end;
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

procedure Register;
begin
  RegisterComponents('Alcinoe', [TALScrollBar, TALTrackBar, TALRangeTrackBar, TALCheckBox, TALRadioButton]);
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
  {$ENDIF}
end;

initialization
  RegisterFmxClasses([TALCheckBox, TALRadioButton, TALScrollBar, TALTrackBar, TALRangeTrackBar]);
  {$IFDEF debug}
  AlDebugCheckBoxMakeBufBitmapStopWatch := TstopWatch.Create;
  {$endif}

end.
