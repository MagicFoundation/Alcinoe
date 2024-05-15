unit Alcinoe.FMX.Common;

interface

{$I Alcinoe.inc}

uses
  System.classes,
  System.UITypes,
  System.Types,
  System.Generics.Collections,
  System.Math.Vectors,
  {$IF defined(ALMacOS)}
  Macapi.CocoaTypes,
  Macapi.Foundation,
  Macapi.CoreGraphics,
  Macapi.CoreText,
  Alcinoe.FMX.NativeView.Mac,
  {$ENDIF}
  {$IF defined(ios)}
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.CoreGraphics,
  iOSapi.CoreText,
  IOSApi.UIKit,
  Alcinoe.FMX.NativeView.iOS,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.Common,
  Alcinoe.FMX.NativeView.Android,
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  Fmx.types,
  FMX.TextLayout,
  FMX.graphics,
  FMX.Filter,
  FMX.Effects,
  FMX.controls;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALDoubleBufferedControl = interface
    ['{26A0A593-D483-4AE2-881B-6CB930B5E863}']
    procedure MakeBufDrawable;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALAutosizeControl = interface
    ['{464CDB70-9F76-4334-8774-5DD98605D6C1}']
    function HasUnconstrainedAutosizeX: boolean;
    function HasUnconstrainedAutosizeY: boolean;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALNativeControl = interface
    ['{EB2063C4-CA1F-4415-97C3-4C161907F244}']
    {$IF defined(android)}
    function GetNativeView: TALAndroidNativeView;
    {$ELSEIF defined(IOS)}
    function GetNativeView: TALIosNativeView;
    {$ELSEIF defined(ALMacOS)}
    function GetNativeView: TALMacNativeView;
    {$ELSEIF defined(MSWindows)}
    function GetNativeView: TALWinNativeView;
    {$ENDIF}
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALPersistentObserver = class(TPersistent)
  private
    FUpdateCount: Integer;
    FIsChanged: Boolean;
    FOnChanged: TNotifyEvent;
  private
    procedure DoChanged; virtual;
  public
    constructor Create; virtual;
    procedure Reset; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure Change; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALShadow = class(TALPersistentObserver)
  private
    FEnabled: boolean;
    FBlur: Single;
    FOffsetX: Single;
    FOffsetY: Single;
    FColor: TAlphaColor;
    FDefaultBlur: Single;
    FDefaultOffsetX: Single;
    FDefaultOffsetY: Single;
    FDefaultColor: TAlphaColor;
    procedure SetEnabled(const Value: boolean);
    procedure setblur(const Value: Single);
    procedure setOffsetX(const Value: Single);
    procedure setOffsetY(const Value: Single);
    procedure setColor(const Value: TAlphaColor);
    function IsblurStored: Boolean;
    function IsOffsetXStored: Boolean;
    function IsOffsetYStored: Boolean;
    function IsColorStored: Boolean;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    property Defaultblur: Single read fDefaultblur write fDefaultblur;
    property DefaultOffsetX: Single read fDefaultOffsetX write fDefaultOffsetX;
    property DefaultOffsetY: Single read fDefaultOffsetY write fDefaultOffsetY;
    property DefaultColor: TAlphaColor read fDefaultColor write fDefaultColor;
  published
    property enabled: boolean read fEnabled Write SetEnabled default false;
    property blur: Single read fblur write setblur stored IsblurStored;
    property OffsetX: Single read fOffsetX write setOffsetX stored IsOffsetXStored;
    property OffsetY: Single read fOffsetY write setOffsetY stored IsOffsetYStored;
    property Color: TAlphaColor read fColor write setColor stored IsColorStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTextDecorationKind = (Underline, Overline, LineThrough);
  TALTextDecorationKinds = set of TALTextDecorationKind;
  TALTextDecorationStyle = (Solid, Double, Dotted, Dashed, Wavy);
  TALTextHorzAlign = (Center, Leading, Trailing, Justify);
  TALTextVertAlign = (Center, Leading, Trailing);
  TALTextDirection = (RightToLeft, LeftToRight);
  TALTextTrimming = (Character, Word);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFont = class(TALPersistentObserver)
  private
    FFamily: TFontName;
    FSize: Single;
    FWeight: TFontWeight;
    FSlant: TFontSlant;
    FStretch: TFontStretch;
    FColor: TAlphaColor;
    FAutoConvert: Boolean;
    FDefaultFamily: TFontName;
    FDefaultSize: Single;
    FDefaultWeight: TFontWeight;
    FDefaultSlant: TFontSlant;
    FDefaultStretch: TFontStretch;
    FDefaultColor: TAlphaColor;
    FDefaultAutoConvert: Boolean;
    procedure SetFamily(const AValue: TFontName);
    procedure SetSize(const AValue: Single);
    procedure SetWeight(const AValue: TFontWeight);
    procedure SetSlant(const AValue: TFontSlant);
    procedure SetStretch(const AValue: TFontStretch);
    procedure SetColor(const AValue: TAlphaColor);
    procedure SetAutoConvert(const AValue: Boolean);
    function IsFamilyStored: Boolean;
    function IsSizeStored: Boolean;
    function IsWeightStored: Boolean;
    function IsSlantStored: Boolean;
    function IsStretchStored: Boolean;
    function IsColorStored: Boolean;
    function IsAutoConvertStored: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    property DefaultFamily: TFontName read FDefaultFamily write FDefaultFamily;
    property DefaultSize: Single read FDefaultSize write FDefaultSize;
    property DefaultWeight: TFontWeight read FDefaultWeight write FDefaultWeight;
    property DefaultSlant: TFontSlant read FDefaultSlant write FDefaultSlant;
    property DefaultStretch: TFontStretch read FDefaultStretch write FDefaultStretch;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
    property DefaultAutoConvert: Boolean read FDefaultAutoConvert write FDefaultAutoConvert;
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored;
    property Size: Single read FSize write SetSize stored IsSizeStored;
    property Weight: TFontWeight read FWeight write SetWeight stored IsWeightStored;
    property Slant: TFontSlant read FSlant write SetSlant stored IsSlantStored;
    property Stretch: TFontStretch read FStretch write SetStretch stored IsStretchStored;
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
    property AutoConvert: Boolean read FAutoConvert write SetAutoConvert stored IsAutoConvertStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTextDecoration = class(TALPersistentObserver)
  private
    FKinds: TALTextDecorationKinds;
    FStyle: TALTextDecorationStyle;
    FThicknessMultiplier: Single;
    FColor: TAlphaColor;
    FDefaultKinds: TALTextDecorationKinds;
    FDefaultStyle: TALTextDecorationStyle;
    FDefaultThicknessMultiplier: Single;
    FDefaultColor: TAlphaColor;
    procedure SetKinds(const AValue: TALTextDecorationKinds);
    procedure SetStyle(const AValue: TALTextDecorationStyle);
    procedure SetThicknessMultiplier(const AValue: Single);
    procedure SetColor(const AValue: TAlphaColor);
    function IsKindsStored: Boolean;
    function IsStyleStored: Boolean;
    function IsThicknessMultiplierStored: Boolean;
    function IsColorStored: Boolean;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    property DefaultKinds: TALTextDecorationKinds read FDefaultKinds write FDefaultKinds;
    property DefaultStyle: TALTextDecorationStyle read FDefaultStyle write FDefaultStyle;
    property DefaultThicknessMultiplier: Single read FDefaultThicknessMultiplier write FDefaultThicknessMultiplier;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
  published
    property Kinds: TALTextDecorationKinds read FKinds write SetKinds Stored IsKindsStored;
    property Style: TALTextDecorationStyle read FStyle write SetStyle Stored IsStyleStored;
    property ThicknessMultiplier: Single read FThicknessMultiplier write SetThicknessMultiplier Stored IsThicknessMultiplierStored;
    property Color: TAlphaColor read FColor write SetColor Stored IsColorStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALEllipsisSettings = class(TALPersistentObserver)
  private
    FInherit: Boolean;
    FFont: TALFont;
    FDecoration: TALTextDecoration;
    procedure SetInherit(const AValue: Boolean);
    procedure SetFont(const AValue: TALFont);
    procedure SetDecoration(const AValue: TALTextDecoration);
    procedure FontChanged(ASender: TObject);
    procedure DecorationChanged(ASender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
    property Font: TALFont read FFont write SetFont;
    property Decoration: TALTextDecoration read FDecoration write SetDecoration;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseTextSettings = class(TALPersistentObserver)
  private
    FFont: TALFont;
    FDecoration: TALTextDecoration;
    FEllipsis: String;
    FEllipsisSettings: TALEllipsisSettings;
    FTrimming: TALTextTrimming;
    FMaxLines: integer;
    FHorzAlign: TALTextHorzAlign;
    FVertAlign: TALTextVertAlign;
    FLineHeightMultiplier: Single;
    FLetterSpacing: Single;
    FIsHtml: Boolean;
    FDefaultEllipsis: String;
    FDefaultTrimming: TALTextTrimming;
    FDefaultMaxLines: integer;
    FDefaultHorzAlign: TALTextHorzAlign;
    FDefaultVertAlign: TALTextVertAlign;
    FDefaultLineHeightMultiplier: Single;
    FDefaultLetterSpacing: Single;
    FDefaultIsHtml: Boolean;
    procedure SetFont(const AValue: TALFont);
    procedure SetDecoration(const AValue: TALTextDecoration);
    procedure SetEllipsis(const AValue: String);
    procedure SetEllipsisSettings(const AValue: TALEllipsisSettings);
    procedure SetTrimming(const AValue: TALTextTrimming);
    procedure SetMaxLines(const AValue: Integer);
    procedure SetHorzAlign(const AValue: TALTextHorzAlign);
    procedure SetVertAlign(const AValue: TALTextVertAlign);
    procedure SetLineHeightMultiplier(const AValue: Single);
    procedure SetLetterSpacing(const AValue: Single);
    procedure SetIsHtml(const AValue: Boolean);
    procedure FontChanged(ASender: TObject);
    procedure DecorationChanged(ASender: TObject);
    procedure EllipsisSettingsChanged(ASender: TObject);
    function IsEllipsisStored: Boolean;
    function IsTrimmingStored: Boolean;
    function IsMaxLinesStored: Boolean;
    function IsHorzAlignStored: Boolean;
    function IsVertAlignStored: Boolean;
    function IsLineHeightMultiplierStored: Boolean;
    function IsLetterSpacingStored: Boolean;
    function IsIsHtmlStored: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    //--
    property DefaultEllipsis: String read FDefaultEllipsis write FDefaultEllipsis;
    property DefaultTrimming: TALTextTrimming read FDefaultTrimming write FDefaultTrimming;
    property DefaultMaxLines: Integer read FDefaultMaxLines write FDefaultMaxLines;
    property DefaultHorzAlign: TALTextHorzAlign read FDefaultHorzAlign write FDefaultHorzAlign;
    property DefaultVertAlign: TALTextVertAlign read FDefaultVertAlign write FDefaultVertAlign;
    property DefaultLineHeightMultiplier: Single read FDefaultLineHeightMultiplier write FDefaultLineHeightMultiplier;
    property DefaultLetterSpacing: Single read FDefaultLetterSpacing write FDefaultLetterSpacing;
    property DefaultIsHtml: Boolean read FDefaultIsHtml write FDefaultIsHtml;
    //--
    property Font: TALFont read FFont write SetFont;
    property Decoration: TALTextDecoration read FDecoration write SetDecoration;
    property Ellipsis: String read FEllipsis write SetEllipsis stored IsEllipsisStored;
    property EllipsisSettings: TALEllipsisSettings read FEllipsisSettings write SetEllipsisSettings;
    property Trimming: TALTextTrimming read FTrimming write SetTrimming stored IsTrimmingStored;
    property MaxLines: Integer read FMaxLines write SetMaxLines stored IsMaxLinesStored;
    property HorzAlign: TALTextHorzAlign read FHorzAlign write SetHorzAlign stored IsHorzAlignStored;
    property VertAlign: TALTextVertAlign read FVertAlign write SetVertAlign stored IsVertAlignStored;
    property LineHeightMultiplier: Single read FLineHeightMultiplier write SetLineHeightMultiplier stored IsLineHeightMultiplierStored;
    property LetterSpacing: Single read FLetterSpacing write SetLetterSpacing stored IsLetterSpacingStored;
    property IsHtml: Boolean read FIsHtml write SetIsHtml stored IsIsHtmlStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTextSettings = class(TALBaseTextSettings)
  private
  public
  published
    property Font;
    property Decoration;
    property Trimming;
    property MaxLines;
    property Ellipsis;
    property EllipsisSettings;
    property HorzAlign;
    property VertAlign;
    property LineHeightMultiplier;
    property LetterSpacing;
    property IsHtml;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritBrush = class(TBrush)
  private
    FInherit: Boolean;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritStrokeBrush = class(TStrokeBrush)
  private
    FInherit: Boolean;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritBaseTextSettings = class(TALBaseTextSettings)
  private
    FInherit: Boolean;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritShadow = class(TALShadow)
  private
    FInherit: Boolean;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  TALFontMetrics = record
    Ascent: Single; // The recommended distance above the baseline for singled spaced text.
    Descent: Single; // The recommended distance below the baseline for singled spaced text.
    Leading: Single; // The recommended additional space to add between lines of text.
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  TALFontManager = class
  public
    class procedure RegisterTypefaceFromResource(const AResourceName: string); static;
  end;

type

  TALCustomConvertFontFamilyProc = function(const AFontFamily: TFontName): TFontName;

var

  ALCustomConvertFontFamilyProc: TALCustomConvertFontFamilyProc;

{*********************************************************************}
function  ALConvertFontFamily(const AFontFamily: TFontName): TFontName;
function  ALExtractPrimaryFontFamily(const AFontFamilies: String): String;
function  ALGetFontMetrics(
            const AFontFamily: String;
            const AFontSize: single;
            const AFontWeight: TFontWeight;
            const AFontSlant: TFontSlant;
            const AFontColor: TalphaColor;
            const ADecorationKinds: TALTextDecorationKinds): TALFontMetrics;
function  ALTranslate(const AText: string): string;
Procedure ALMakeBufDrawables(const AControl: TControl);
procedure ALAutoSize(const AControl: TControl);
function  ALAlignDimensionToPixelRound(const Rect: TRectF; const Scale: single): TRectF; overload;
function  ALAlignDimensionToPixelRound(const Dimension: single; const Scale: single): single; overload;
function  ALAlignDimensionToPixelCeil(const Rect: TRectF; const Scale: single): TRectF; overload;
function  ALAlignDimensionToPixelCeil(const Dimension: single; const Scale: single): single; overload;
function  ALAlignToPixelRound(const Point: TPointF; const Scale: single): TpointF; overload;
function  ALAlignToPixelRound(const Rect: TRectF; const Scale: single): TRectF; overload;

{$IF defined(ALAppleOS)}
type
  PAlphaColorCGFloat = ^TAlphaColorCGFloat;
  TAlphaColorCGFloat = record
  public
    R, G, B, A: CGFloat;
    class function Create(const R, G, B: CGFloat; const A: CGFloat = 1): TAlphaColorCGFloat; overload; static; inline;
    class function Create(const Color: TAlphaColor): TAlphaColorCGFloat; overload; static; inline;
    class function Create(const Color: TAlphaColorF): TAlphaColorCGFloat; overload; static; inline;
  end;

function ALLowerLeftCGRect(const aUpperLeftOrigin: TPointF; const aWidth, aHeight: single; const aGridHeight: Single): CGRect;
function ALCreateCTFontRef(const AFontFamily: String; const AFontSize: single; const AFontWeight: TFontWeight; const AFontSlant: TFontSlant): CTFontRef;
{$ENDIF}

{$IF defined(IOS)}
function ALTextHorzAlignToUITextAlignment(const ATextHorzAlign: TALTextHorzAlign): UITextAlignment;
{$ENDIF}

{$IF defined(ALMacOS)}
function ALTextHorzAlignToNSTextAlignment(const ATextHorzAlign: TALTextHorzAlign): NSTextAlignment;
{$ENDIF}

{$IF defined(ANDROID)}
function ALfontStyleToAndroidStyle(const afontStyle: TfontStyles): integer;
{$ENDIF}

{$IF defined(ANDROID)}
function ALStringsToJArrayList(const AStrings: TArray<String>): JArrayList;
function ALJSetToStrings(const ASet: JSet): TArray<String>;
{$ENDIF}

{$IF defined(ALAppleOS)}
function ALStringsToNSArray(const AStrings: TArray<String>): NSMutableArray;
function ALNSSetToStrings(const ANSSet: NSSet): TArray<String>;
{$ENDIF}

{$IFDEF ALDPK}
function ALDPKGetResourceFilename(const AResourceName: String): String;
{$ENDIF}

Type

  {$IFNDEF ALCompilerVersionSupported120}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl still has the exact same fields and adjust the IFDEF'}
  {$ENDIF}
  TALControlAccessPrivate = class(TFmxObject)
  {$IF CompilerVersion >= 32}  // Tokyo
  private type
    TDelayedEvent = (Resize, Resized);
  {$ENDIF}
  private const
    InitialControlsCapacity = 10;
  public const
    DefaultTouchTargetExpansion = 6;
    DefaultDisabledOpacity = 0.6;
    DesignBorderColor = $A0909090;
  protected class var
    FPaintStage: TPaintStage;
  public
    FOnMouseUp: TMouseEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FHitTest: Boolean;
    FClipChildren: Boolean;
    FAutoCapture: Boolean;
    FPadding: TBounds;
    FMargins: TBounds;
    FTempCanvas: TCanvas;
    FRotationAngle: Single;
    FPosition: TPosition;
    FScale: TPosition;
    FSkew: TPosition;
    FRotationCenter: TPosition;
    FCanFocus: Boolean;
    FOnCanFocus: TCanFocusEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FClipParent: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnPaint: TOnPaintEvent;
    FOnPainting: TOnPaintEvent;
    FCursor: TCursor;
    FInheritedCursor: TCursor;
    FDragMode: TDragMode;
    FEnableDragHighlight: Boolean;
    FOnDragEnter: TDragEnterEvent;
    FOnDragDrop: TDragDropEvent;
    FOnDragLeave: TNotifyEvent;
    FOnDragOver: TDragOverEvent;
    FOnDragEnd: TNotifyEvent;
    FIsDragOver: Boolean;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;
    FOnTap: TTapEvent;
    FHint: string;
    FActionHint: string;
    FShowHint: Boolean;
    FPopupMenu: TCustomPopupMenu;
    FRecalcEnabled, FEnabled, FAbsoluteEnabled: Boolean;
    FTabList: TTabList;
    FOnResize: TNotifyEvent;
    {$IF CompilerVersion >= 32}  // Tokyo
    FOnResized: TNotifyEvent;
    {$ENDIF}
    FDisableEffect: Boolean;
    FAcceptsControls: Boolean;
    FControls: TControlList;
    FEnableExecuteAction: Boolean;
    FCanParentFocus: Boolean;
    FMinClipHeight: Single;
    FMinClipWidth: Single;
    FSmallSizeControl: Boolean;
    FTouchTargetExpansion: TBounds;
    FOnDeactivate: TNotifyEvent;
    FOnActivate: TNotifyEvent;
    FSimpleTransform: Boolean;
    FFixedSize: TSize;
    FEffects: TList<TEffect>;
    FDisabledOpacity: Single;
    [Weak] FParentControl: TControl;
    FParentContent: IContent;
    {$IF CompilerVersion >= 36}  // Athens
    FParentContentObserver: IContentObserver;
    {$ENDIF}
    FUpdateRect: TRectF;
    FTabStop: Boolean;
    FDisableDisappear: Integer;
    FAnchorMove: Boolean;
    FApplyingEffect: Boolean;
    {$IF CompilerVersion >= 32}  // Tokyo
    FExitingOrEntering: Boolean;
    FDelayedEvents: set of TDelayedEvent;
    {$ENDIF}
    {$IF CompilerVersion >= 34}  // Sydney
    FTabOrder: TTabOrder;
    {$ENDIF}
    FInflated: Boolean;
    {$IF CompilerVersion >= 36}  // Athens
    FOnApplyStyle: TNotifyEvent;
    FOnFreeStyle: TNotifyEvent;
    {$ELSE}
    FOnApplyStyleLookup: TNotifyEvent;
    {$ENDIF}
    FAlign: TAlignLayout;
    FAnchors: TAnchors;
    {$IF CompilerVersion < 36}  // Athens
    FUpdateEffects: Boolean;
    {$ENDIF}
    FDisableFocusEffect: Boolean;
    FTouchManager: TTouchManager;
    FOnGesture: TGestureEvent;
    FVisible: Boolean;
    FPressed: Boolean;
    FPressedPosition: TPointF;
    FDoubleClick: Boolean;
    FParentShowHint: Boolean;
    {$IF CompilerVersion >= 33}  // Rio
    FCustomSceneAddRect: TCustomSceneAddRectEvent;
    {$ENDIF}
    FScene: IScene;
    FLastHeight: Single;
    FLastWidth: Single;
    FSize: TControlSize;
    FLocalMatrix: TMatrix;
    FAbsoluteMatrix: TMatrix;
    FInvAbsoluteMatrix: TMatrix;
    {$IF CompilerVersion >= 36}  // Athens
    FEffectCache: IFilterCacheLayer;
    {$ELSE}
    FEffectBitmap: TBitmap;
    {$ENDIF}
    FLocked: Boolean;
    FOpacity, FAbsoluteOpacity: Single;
    FInPaintTo: Boolean;
    FInPaintToAbsMatrix, FInPaintToInvMatrix: TMatrix;
    FAbsoluteHasEffect: Boolean;
    FAbsoluteHasDisablePaintEffect: Boolean;
    FAbsoluteHasAfterPaintEffect: Boolean;
    FUpdating: Integer;
    FNeedAlign: Boolean;
    FDisablePaint: Boolean;
    FDisableAlign: Boolean;
    FRecalcOpacity: Boolean;
    FRecalcUpdateRect: Boolean;
    FRecalcAbsolute: Boolean;
    FRecalcHasEffect: Boolean;
    FHasClipParent: TControl;
    FRecalcHasClipParent: Boolean;
    FDesignInteractive: Boolean;
    FDesignSelectionMarks: Boolean;
    FIsMouseOver: Boolean;
    FIsFocused: Boolean;
    FAnchorRules: TPointF;
    FAnchorOrigin: TPointF;
    FOriginalParentSize: TPointF;
    FLeft: Single;
    FTop: Single;
    FExplicitLeft: Single;
    FExplicitTop: Single;
    FExplicitWidth: Single;
    FExplicitHeight: Single;
  end;

{$IFDEF ANDROID}
var ALViewStackCount: integer;
{$ENDIF}

{$IFDEF ANDROID}
function getRenderScript: JRenderScript;
{$ENDIF}

var
  ALScreenScale: Single;
procedure ALInitScreenScale;
function ALGetScreenScale: Single; Inline;

implementation

uses
  system.SysUtils,
  System.Math,
  System.SyncObjs,
  Fmx.Platform,
  {$IF defined(ALSkiaEngine)}
  FMX.Skia,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  FMX.forms,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Macapi.AppKit,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  Winapi.Windows,
  FMX.Helpers.Win,
  {$ENDIF}
  {$IFDEF ALDPK}
  System.IOutils,
  System.Hash,
  ToolsAPI,
  {$ENDIF}
  {$IF not defined(ALDPK)}
  Alcinoe.Cipher,
  {$ENDIF}
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.Common,
  ALcinoe.StringUtils;

{***************************************}
constructor TALPersistentObserver.Create;
begin
  inherited create;
  FUpdateCount := 0;
  FIsChanged := False;
  FOnChanged := nil;
end;

{************************************}
procedure TALPersistentObserver.Reset;
begin
  // Virtual
end;

{***************************************}
procedure TALPersistentObserver.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

{***************************************}
procedure TALPersistentObserver.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and (FIsChanged) then
      try
        DoChanged;
      finally
        FIsChanged := False;
      end;
  end;
end;

{***************************************}
procedure TALPersistentObserver.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

{***************************************}
procedure TALPersistentObserver.Change;
begin
  FIsChanged := True;
  if (FUpdateCount = 0) then
  begin
    try
      DoChanged;
    finally
      FIsChanged := False;
    end;
  end;
end;

{***************************}
constructor TALShadow.Create;
begin
  inherited Create;
  FEnabled := False;

  FDefaultblur := 12;
  FDefaultOffsetX := 0;
  FDefaultOffsetY := 0;
  FDefaultColor := $96000000;

  Fblur := FDefaultBlur;
  FOffsetX := FDefaultOffsetX;
  FOffsetY := FDefaultOffsetY;
  FColor := FDefaultColor;
end;

{**********************************************}
procedure TALShadow.Assign(Source: TPersistent);
begin
  if Source is TALShadow then begin
    BeginUpdate;
    Try
      Enabled := TALShadow(Source).Enabled;
      Blur    := TALShadow(Source).Blur;
      OffsetX := TALShadow(Source).OffsetX;
      OffsetY := TALShadow(Source).OffsetY;
      Color   := TALShadow(Source).Color;
    Finally
      EndUpdate;
    End;
  end
  else
    inherited Assign(Source);
end;

{************************}
procedure TALShadow.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Enabled := False;
    blur := DefaultBlur;
    OffsetX := DefaultOffsetX;
    OffsetY := DefaultOffsetY;
    Color := DefaultColor;
  finally
    EndUpdate;
  end;
end;

{***************************************}
function TALShadow.IsblurStored: Boolean;
begin
  result := not SameValue(fBlur, FDefaultBlur, Tepsilon.Position);
end;

{******************************************}
function TALShadow.IsOffsetXStored: Boolean;
begin
  result := not SameValue(fOffsetX, FDefaultOffsetX, Tepsilon.Position);
end;

{******************************************}
function TALShadow.IsOffsetYStored: Boolean;
begin
  result := not SameValue(fOffsetY, FDefaultOffsetY, Tepsilon.Position);
end;

{****************************************}
function TALShadow.IsColorStored: Boolean;
begin
  result := FColor <> FDefaultColor;
end;

{***************************************************}
procedure TALShadow.SetEnabled(const Value: boolean);
begin
  if fEnabled <> Value then begin
    fEnabled := Value;
    Change;
  end;
end;

{***********************************************}
procedure TALShadow.setblur(const Value: Single);
begin
  if not SameValue(fBlur, Value, Tepsilon.Position) then begin
    Fblur := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALShadow.setOffsetX(const Value: Single);
begin
  if not SameValue(fOffsetX, Value, Tepsilon.Position) then begin
    fOffsetX := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALShadow.setOffsetY(const Value: Single);
begin
  if not SameValue(fOffsetY, Value, Tepsilon.Position) then begin
    fOffsetY := Value;
    Change;
  end;
end;

{*****************************************************}
procedure TALShadow.setColor(const Value: TAlphaColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Change;
  end;
end;

{*************************}
constructor TALFont.Create;
begin
  inherited Create;

  FDefaultFamily := 'sans-serif';
  FDefaultSize := 14;
  FDefaultWeight := TFontWeight.Regular;
  FDefaultSlant := TFontSlant.Regular;
  FDefaultStretch := TFontStretch.Regular;
  FDefaultColor := TAlphaColorRec.Black;
  FDefaultAutoConvert := True;

  FFamily := FDefaultFamily;
  FSize := FDefaultSize;
  FWeight := FDefaultWeight;
  FSlant := FDefaultSlant;
  FStretch := FDefaultStretch;
  FColor := FDefaultColor;
  FAutoConvert := FDefaultAutoConvert;
end;

{********************************************}
procedure TALFont.AssignTo(Dest: TPersistent);
begin
  if Dest is TALFont then begin
    TALFont(Dest).Assign(self);
  end
  else if Dest is TFont then begin
    TFont(Dest).SetSettings(
      Family,
      Size,
      TFontStyleExt.Create(
        Weight, // const AWeight: TFontWeight = TFontWeight.Regular;
        Slant, // const AStant: TFontSlant = TFontSlant.Regular;
        Stretch, // const AStretch: TFontStretch = TFontStretch.Regular;
        [])); // const AOtherStyles: TFontStyles = []
  end
  else
    inherited AssignTo(Dest);
end;

{********************************************}
procedure TALFont.Assign(Source: TPersistent);
begin
  if Source is TALFont then begin
    BeginUpdate;
    Try
      Family      := TALFont(Source).Family;
      Size        := TALFont(Source).Size;
      Weight      := TALFont(Source).Weight;
      Slant       := TALFont(Source).Slant;
      Stretch     := TALFont(Source).Stretch;
      Color       := TALFont(Source).Color;
      AutoConvert := TALFont(Source).AutoConvert;
    Finally
      EndUpdate;
    End;
  end
  else if Source is TFont then begin
    BeginUpdate;
    Try
      Family      := TFont(Source).Family;
      Size        := TFont(Source).Size;
      Weight      := TFont(Source).StyleExt.Weight;
      Slant       := TFont(Source).StyleExt.Slant;
      Stretch     := TFont(Source).StyleExt.Stretch;
      Color       := DefaultColor;
      AutoConvert := DefaultAutoConvert;
    Finally
      EndUpdate;
    End;
  end
  else
    inherited Assign(Source);
end;

{**********************}
procedure TALFont.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Family := DefaultFamily;
    Size := DefaultSize;
    Weight := DefaultWeight;
    Slant := DefaultSlant;
    Stretch := DefaultStretch;
    Color := DefaultColor;
    AutoConvert := DefaultAutoConvert;
  finally
    EndUpdate;
  end;
end;

{***************************************}
function TALFont.IsFamilyStored: Boolean;
begin
  Result := FFamily <> FDefaultFamily;
end;

{*************************************}
function TALFont.IsSizeStored: Boolean;
begin
  Result := not SameValue(FSize, FDefaultSize, TEpsilon.FontSize);
end;

{***************************************}
function TALFont.IsWeightStored: Boolean;
begin
  result := FWeight <> FDefaultWeight;
end;

{**************************************}
function TALFont.IsSlantStored: Boolean;
begin
  result := FSlant <> FDefaultSlant;
end;

{****************************************}
function TALFont.IsStretchStored: Boolean;
begin
  result := FStretch <> FDefaultStretch;
end;

{**************************************}
function TALFont.IsColorStored: Boolean;
begin
  result := FColor <> FDefaultColor;
end;

{********************************************}
function TALFont.IsAutoConvertStored: Boolean;
begin
  result := FAutoConvert <> FDefaultAutoConvert;
end;

{***************************************************}
procedure TALFont.SetFamily(const AValue: TFontName);

  function NormalizeFamily(const AValue: string): string;
  var
    LSplitted: TArray<string>;
    LFamilies: TArray<string>;
    I: Integer;
  begin
    LSplitted := AValue.Split([',', #13, #10], TStringSplitOptions.ExcludeEmpty);
    LFamilies := [];
    for I := 0 to Length(LSplitted) - 1 do
    begin
      LSplitted[I] := LSplitted[I].Trim;
      if LSplitted[I] <> '' then
        LFamilies := LFamilies + [LSplitted[I]];
    end;
    if LFamilies = nil then
      Exit('');
    Result := string.Join(', ', LFamilies);
  end;

begin
  Var LFamily := NormalizeFamily(AValue);
  if FFamily <> LFamily then begin
    FFamily := LFamily;
    Change;
  end;
end;

{**********************************************}
procedure TALFont.SetSize(const AValue: Single);
begin
  if not SameValue(FSize, AValue, TEpsilon.FontSize) then begin
    FSize := AValue;
    Change;
  end;
end;

{*****************************************************}
procedure TALFont.SetWeight(const AValue: TFontWeight);
begin
  if FWeight <> AValue then begin
    FWeight := AValue;
    change;
  end;
end;

{***************************************************}
procedure TALFont.SetSlant(const AValue: TFontSlant);
begin
  If FSlant <> AValue then begin
    FSlant := AValue;
    Change;
  end;
end;

{*******************************************************}
procedure TALFont.SetStretch(const AValue: TFontStretch);
begin
  If FStretch <> AValue then begin
    FStretch := AValue;
    Change;
  end;
end;

{****************************************************}
procedure TALFont.SetColor(const AValue: TAlphaColor);
begin
  if FColor <> AValue then begin
    FColor := AValue;
    change;
  end;
end;

{******************************************************}
procedure TALFont.SetAutoConvert(const AValue: Boolean);
begin
  if FAutoConvert <> AValue then begin
    FAutoConvert := AValue;
    change;
  end;
end;

{***********************************}
constructor TALTextDecoration.Create;
begin
  inherited Create;

  FDefaultKinds := [];
  FDefaultStyle := TALTextDecorationStyle.Solid;
  FDefaultThicknessMultiplier := 1;
  FDefaultColor := TAlphaColors.Null;

  FKinds := FDefaultKinds;
  FStyle := FDefaultStyle;
  FThicknessMultiplier := FDefaultThicknessMultiplier;
  FColor := FDefaultColor;
end;

{******************************************************}
procedure TALTextDecoration.Assign(Source: TPersistent);
begin
  if Source is TALTextDecoration then begin
    BeginUpdate;
    Try
      Kinds := TALTextDecoration(Source).Kinds;
      Style := TALTextDecoration(Source).Style;
      ThicknessMultiplier := TALTextDecoration(Source).ThicknessMultiplier;
      Color  := TALTextDecoration(Source).Color;
    Finally
      EndUpdate;
    End;
  end
  else
    inherited Assign(Source);
end;

{********************************}
procedure TALTextDecoration.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Kinds := DefaultKinds;
    Style := DefaultStyle;
    ThicknessMultiplier := DefaultThicknessMultiplier;
    Color := DefaultColor;
  finally
    EndUpdate;
  end;
end;

{************************************************}
function TALTextDecoration.IsKindsStored: Boolean;
begin
  result := FKinds <> FDefaultKinds
end;

{************************************************}
function TALTextDecoration.IsStyleStored: Boolean;
begin
  result := FStyle <> FDefaultStyle
end;

{**************************************************************}
function TALTextDecoration.IsThicknessMultiplierStored: Boolean;
begin
  Result := not SameValue(FThicknessMultiplier, FDefaultThicknessMultiplier, TEpsilon.Vector);
end;

{************************************************}
function TALTextDecoration.IsColorStored: Boolean;
begin
  result := FColor <> FDefaultColor
end;

{*************************************************************************}
procedure TALTextDecoration.SetKinds(const AValue: TALTextDecorationKinds);
begin
  If FKinds <> AValue then begin
    FKinds := AValue;
    Change;
  end;
end;

{*************************************************************************}
procedure TALTextDecoration.SetStyle(const AValue: TALTextDecorationStyle);
begin
  If FStyle <> AValue then begin
    FStyle := AValue;
    Change;
  end;
end;

{***********************************************************************}
procedure TALTextDecoration.SetThicknessMultiplier(const AValue: Single);
begin
  if not SameValue(FThicknessMultiplier, AValue, TEpsilon.Vector) then begin
    FThicknessMultiplier := AValue;
    Change;
  end;
end;

{**************************************************************}
procedure TALTextDecoration.SetColor(const AValue: TAlphaColor);
begin
  If FColor <> AValue then begin
    FColor := AValue;
    Change;
  end;
end;

{*************************************}
constructor TALEllipsisSettings.Create;
begin
  inherited Create;
  FInherit := True;
  FFont := TALFont.Create;
  FFont.OnChanged := FontChanged;
  FDecoration := TALTextDecoration.Create;
  FDecoration.OnChanged := DecorationChanged;
end;

{*************************************}
destructor TALEllipsisSettings.Destroy;
begin
  ALFreeAndNil(FFont);
  ALFreeAndNil(FDecoration);
  inherited Destroy;
end;

{********************************************************}
procedure TALEllipsisSettings.Assign(Source: TPersistent);
begin
  if Source is TALEllipsisSettings then begin
    BeginUpdate;
    Try
      Inherit := TALEllipsisSettings(Source).Inherit;
      Font.Assign(TALEllipsisSettings(Source).Font);
      Decoration.Assign(TALEllipsisSettings(Source).Decoration);
    Finally
      EndUpdate;
    End;
  end
  else if Source is TTextSettings then begin
    BeginUpdate;
    Try
      Inherit := False;
      Font.assign(TTextSettings(Source).Font);
      Font.color := TTextSettings(Source).FontColor;
      Decoration.reset;
    Finally
      EndUpdate;
    End;
  end
  else
    inherited Assign(Source);
end;

{**********************************}
procedure TALEllipsisSettings.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Inherit := True;
    Font.reset;
    Decoration.reset;
  finally
    EndUpdate;
  end;
end;

{**********************************************************}
procedure TALEllipsisSettings.FontChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************}
procedure TALEllipsisSettings.DecorationChanged(ASender: TObject);
begin
  Change;
end;

{**************************************************************}
procedure TALEllipsisSettings.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{***********************************************************}
procedure TALEllipsisSettings.SetFont(const AValue: TALFont);
begin
  FFont.Assign(AValue);
end;

{***************************************************************************}
procedure TALEllipsisSettings.SetDecoration(const AValue: TALTextDecoration);
begin
  FDecoration.Assign(AValue);
end;

{*************************************}
constructor TALBaseTextSettings.Create;
begin
  inherited Create;
  FFont := TALFont.Create;
  FFont.OnChanged := FontChanged;
  FDecoration := TALTextDecoration.Create;
  FDecoration.OnChanged := DecorationChanged;
  FEllipsisSettings := TALEllipsisSettings.create;
  FEllipsisSettings.OnChanged := EllipsisSettingsChanged;

  FDefaultEllipsis := '…';
  FDefaultTrimming := TALTextTrimming.Word;
  FDefaultMaxLines := 65535;
  FDefaultHorzAlign := TALTextHorzAlign.Leading;
  FDefaultVertAlign := TALTextVertAlign.Center;
  FDefaultLineHeightMultiplier := 0;
  FDefaultLetterSpacing := 0;
  FDefaultIsHtml := False;

  FEllipsis := FDefaultEllipsis;
  FTrimming := FDefaultTrimming;
  FMaxLines := FDefaultMaxLines;
  FHorzAlign := FDefaultHorzAlign;
  FVertAlign := FDefaultVertAlign;
  FLineHeightMultiplier := FDefaultLineHeightMultiplier;
  FLetterSpacing := FDefaultLetterSpacing;
  FIsHtml := FDefaultIsHtml;
end;

{*************************************}
destructor TALBaseTextSettings.Destroy;
begin
  ALFreeAndNil(FFont);
  ALFreeAndNil(FDecoration);
  ALFreeAndNil(FEllipsisSettings);
  inherited Destroy;
end;

{********************************************************}
procedure TALBaseTextSettings.AssignTo(Dest: TPersistent);
begin
  if Dest is TALBaseTextSettings then begin
    TALBaseTextSettings(Dest).Assign(self);
  end
  else if Dest is TTextSettings then begin
    TTextSettings(Dest).BeginUpdate;
    try
      TTextSettings(Dest).Font.Assign(Font);
      if TALTextDecorationKind.Underline in Decoration.Kinds then TTextSettings(Dest).Font.Style := TTextSettings(Dest).Font.Style + [TfontStyle.fsUnderline];
      if TALTextDecorationKind.LineThrough in Decoration.Kinds then TTextSettings(Dest).Font.Style := TTextSettings(Dest).Font.Style + [TfontStyle.fsStrikeOut];
      TTextSettings(Dest).FontColor := Font.color;
      if Ellipsis = '' then TTextSettings(Dest).Trimming := TTextTrimming.None
      else begin
        case Trimming of
          TALTextTrimming.Character: TTextSettings(Dest).Trimming := TTextTrimming.Character;
          TALTextTrimming.Word:      TTextSettings(Dest).Trimming := TTextTrimming.Word;
          else raise Exception.Create('Error 66A0235E-FC1D-4FD9-82B1-1C58AF792AE6');
        end;
      end;
      TTextSettings(Dest).WordWrap := MaxLines > 1;
      case HorzAlign of
        TALTextHorzAlign.Center:   TTextSettings(Dest).HorzAlign := TTextAlign.Center;
        TALTextHorzAlign.Leading:  TTextSettings(Dest).HorzAlign := TTextAlign.Leading;
        TALTextHorzAlign.Trailing: TTextSettings(Dest).HorzAlign := TTextAlign.Trailing;
        TALTextHorzAlign.Justify:  TTextSettings(Dest).HorzAlign := TTextAlign.Leading;
        else raise Exception.Create('Error 1D7E69B8-1EA0-4DB1-8A6D-A69712D6BB00');
      end;
      case VertAlign of
        TALTextVertAlign.Center:   TTextSettings(Dest).VertAlign := TTextAlign.Center;
        TALTextVertAlign.Leading:  TTextSettings(Dest).VertAlign := TTextAlign.Leading;
        TALTextVertAlign.Trailing: TTextSettings(Dest).VertAlign := TTextAlign.Trailing;
        else raise Exception.Create('Error 09066A04-9DF9-492B-8836-5C16027E9CDF');
      end;
    finally
      TTextSettings(Dest).EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

{********************************************************}
procedure TALBaseTextSettings.Assign(Source: TPersistent);
begin
  if Source is TALBaseTextSettings then begin
    BeginUpdate;
    Try
      Font.Assign(TALBaseTextSettings(Source).Font);
      Decoration.Assign(TALBaseTextSettings(Source).Decoration);
      EllipsisSettings.Assign(TALBaseTextSettings(Source).EllipsisSettings);
      Ellipsis             := TALBaseTextSettings(Source).Ellipsis;
      Trimming             := TALBaseTextSettings(Source).Trimming;
      MaxLines             := TALBaseTextSettings(Source).MaxLines;
      HorzAlign            := TALBaseTextSettings(Source).HorzAlign;
      VertAlign            := TALBaseTextSettings(Source).VertAlign;
      LineHeightMultiplier := TALBaseTextSettings(Source).LineHeightMultiplier;
      LetterSpacing        := TALBaseTextSettings(Source).LetterSpacing;
      IsHtml               := TALBaseTextSettings(Source).IsHtml;
    Finally
      EndUpdate;
    End;
  end
  else if Source is TTextSettings then begin
    BeginUpdate;
    Try
      Font.assign(TTextSettings(Source).Font);
      Font.color := TTextSettings(Source).FontColor;
      Decoration.reset;
      if TfontStyle.fsUnderline in TTextSettings(Source).Font.Style then Decoration.Kinds := Decoration.Kinds + [TALTextDecorationKind.Underline];
      if TfontStyle.fsStrikeOut in TTextSettings(Source).Font.Style then Decoration.Kinds := Decoration.Kinds + [TALTextDecorationKind.LineThrough];
      EllipsisSettings.reset;
      Ellipsis := DefaultEllipsis;
      case TTextSettings(Source).Trimming of
        TTextTrimming.None:      Trimming := DefaultTrimming;
        TTextTrimming.Character: Trimming := TALTextTrimming.Character;
        TTextTrimming.Word:      Trimming := TALTextTrimming.Word;
        else raise Exception.Create('Error FAFCFC8A-6C5F-464C-B2F3-0283C0D6072E');
      end;
      if not TTextSettings(Source).WordWrap then MaxLines := 1
      else                                       MaxLines := DefaultMaxLines;
      case TTextSettings(Source).HorzAlign of
        TTextAlign.Center:   HorzAlign := TALTextHorzAlign.Center;
        TTextAlign.Leading:  HorzAlign := TALTextHorzAlign.Leading;
        TTextAlign.Trailing: HorzAlign := TALTextHorzAlign.Trailing;
        else raise Exception.Create('Error AB8E03CB-3C96-4052-8BF1-1906836BD90B');
      end;
      case TTextSettings(Source).VertAlign of
        TTextAlign.Center:   VertAlign := TALTextVertAlign.Center;
        TTextAlign.Leading:  VertAlign := TALTextVertAlign.Leading;
        TTextAlign.Trailing: VertAlign := TALTextVertAlign.Trailing;
        else raise Exception.Create('Error F25E554C-CF12-4686-A99B-19927FC7E18D');
      end;
      LineHeightMultiplier := DefaultLineHeightMultiplier;
      LetterSpacing        := DefaultLetterSpacing;
      IsHtml               := DefaultIsHtml;
    Finally
      EndUpdate;
    End;
  end
  else
    inherited Assign(Source);
end;

{**********************************}
procedure TALBaseTextSettings.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Font.reset;
    Decoration.reset;
    EllipsisSettings.reset;
    Ellipsis := DefaultEllipsis;
    Trimming := DefaultTrimming;
    MaxLines := DefaultMaxLines;
    HorzAlign := DefaultHorzAlign;
    VertAlign := DefaultVertAlign;
    LineHeightMultiplier := DefaultLineHeightMultiplier;
    LetterSpacing := DefaultLetterSpacing;
    IsHtml := DefaultIsHtml;
  finally
    EndUpdate;
  end;
end;

{**********************************************************}
procedure TALBaseTextSettings.FontChanged(ASender: TObject);
begin
  Change;
end;

{****************************************************************}
procedure TALBaseTextSettings.DecorationChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************}
procedure TALBaseTextSettings.EllipsisSettingsChanged(ASender: TObject);
begin
  Change;
end;

{*************************************************}
function TALBaseTextSettings.IsEllipsisStored: Boolean;
begin
  Result := FEllipsis <> FDefaultEllipsis;
end;

{*****************************************************}
function TALBaseTextSettings.IsTrimmingStored: Boolean;
begin
  Result := FTrimming <> FDefaultTrimming;
end;

{*****************************************************}
function TALBaseTextSettings.IsMaxLinesStored: Boolean;
begin
  Result := FMaxLines <> FDefaultMaxLines;
end;

{******************************************************}
function TALBaseTextSettings.IsHorzAlignStored: Boolean;
begin
  Result := FHorzAlign <> FDefaultHorzAlign;
end;

{******************************************************}
function TALBaseTextSettings.IsVertAlignStored: Boolean;
begin
  Result := FVertAlign <> FDefaultVertAlign;
end;

{*****************************************************************}
function TALBaseTextSettings.IsLineHeightMultiplierStored: Boolean;
begin
  Result := not SameValue(FLineHeightMultiplier, FDefaultLineHeightMultiplier, TEpsilon.Scale);
end;

{**********************************************************}
function TALBaseTextSettings.IsLetterSpacingStored: Boolean;
begin
  Result := not SameValue(FLetterSpacing, FDefaultLetterSpacing, TEpsilon.FontSize);
end;

{***************************************************}
function TALBaseTextSettings.IsIsHtmlStored: Boolean;
begin
  Result := FIsHtml <> FDefaultIsHtml;
end;

{***********************************************************}
procedure TALBaseTextSettings.SetFont(const AValue: TALFont);
begin
  FFont.Assign(AValue);
end;

{***************************************************************************}
procedure TALBaseTextSettings.SetDecoration(const AValue: TALTextDecoration);
begin
  FDecoration.Assign(AValue);
end;

{*******************************************************************************}
procedure TALBaseTextSettings.SetEllipsisSettings(const AValue: TALEllipsisSettings);
begin
  FEllipsisSettings.Assign(AValue);
end;

{**************************************************************}
procedure TALBaseTextSettings.SetEllipsis(const AValue: String);
begin
  If FEllipsis <> AValue then begin
    FEllipsis := AValue;
    Change;
  end;
end;

{***********************************************************************}
procedure TALBaseTextSettings.SetTrimming(const AValue: TALTextTrimming);
begin
  If FTrimming <> AValue then begin
    FTrimming := AValue;
    Change;
  end;
end;

{***************************************************************}
procedure TALBaseTextSettings.SetMaxLines(const AValue: Integer);
begin
  If FMaxLines <> AValue then begin
    FMaxLines := AValue;
    Change;
  end;
end;

{*************************************************************************}
procedure TALBaseTextSettings.SetHorzAlign(const AValue: TALTextHorzAlign);
begin
  If FHorzAlign <> AValue then begin
    FHorzAlign := AValue;
    Change;
  end;
end;

{*************************************************************************}
procedure TALBaseTextSettings.SetVertAlign(const AValue: TALTextVertAlign);
begin
  If FVertAlign <> AValue then begin
    FVertAlign := AValue;
    Change;
  end;
end;

{**************************************************************************}
procedure TALBaseTextSettings.SetLineHeightMultiplier(const AValue: Single);
begin
  if not SameValue(FLineHeightMultiplier, AValue, TEpsilon.Scale) then begin
    FLineHeightMultiplier := AValue;
    Change;
  end;
end;

{*******************************************************************}
procedure TALBaseTextSettings.SetLetterSpacing(const AValue: Single);
begin
  if not SameValue(FLetterSpacing, AValue, TEpsilon.FontSize) then begin
    FLetterSpacing := AValue;
    Change;
  end;
end;

{*************************************************************}
procedure TALBaseTextSettings.SetIsHtml(const AValue: Boolean);
begin
  If FIsHtml <> AValue then begin
    FIsHtml := AValue;
    Change;
  end;
end;

{***************************************************************************************************}
constructor TALInheritBrush.Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor);
begin
  inherited create(ADefaultKind, ADefaultColor);
  FInherit := True;
end;

{**********************************************************}
procedure TALInheritBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    if Assigned(OnChanged) then
      OnChanged(Self);
  end;
end;

{****************************************************}
procedure TALInheritBrush.Assign(Source: TPersistent);
begin
  if Source is TALInheritBrush then
    Inherit := TALInheritBrush(Source).Inherit
  else
    Inherit := False;
  inherited Assign(Source);
end;

{******************************}
procedure TALInheritBrush.Reset;
begin
  Inherit := True;
  Color := DefaultColor;
  Kind := DefaultKind;
end;

{*************************************}
constructor TALInheritStrokeBrush.Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor);
begin
  inherited create(ADefaultKind, ADefaultColor);
  FInherit := True;
end;

{**************************************************************}
procedure TALInheritStrokeBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    if Assigned(OnChanged) then
      OnChanged(Self);
  end;
end;

{********************************************************}
procedure TALInheritStrokeBrush.Assign(Source: TPersistent);
begin
  if Source is TALInheritStrokeBrush then
    Inherit := TALInheritStrokeBrush(Source).Inherit
  else
    Inherit := False;
  inherited Assign(Source);
end;

{*********************************************}
procedure TALInheritStrokeBrush.Reset;
begin
  Inherit := True;
  Color := DefaultColor;
  Kind := DefaultKind;
end;

{*************************************}
constructor TALInheritBaseTextSettings.Create;
begin
  inherited create;
  FInherit := True;
  // Font size = 0 mean inherit the font size
  Font.DefaultSize := 0;
  Font.Size := Font.DefaultSize;
  // Font family = '' mean inherit the font family
  Font.DefaultFamily := '';
  Font.Family := Font.DefaultFamily;
  // Font Color = Null mean inherit the font Color
  Font.DefaultColor := TAlphaColors.Null;
  Font.Color := Font.DefaultColor;
end;

{**************************************************************}
procedure TALInheritBaseTextSettings.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{********************************************************}
procedure TALInheritBaseTextSettings.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALInheritBaseTextSettings then
      Inherit := TALInheritBaseTextSettings(Source).Inherit
    else
      Inherit := False;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*********************************************}
procedure TALInheritBaseTextSettings.Reset;
begin
  BeginUpdate;
  Try
    Inherit := True;
    inherited reset;
  finally
    EndUpdate;
  end;
end;

{*************************************}
constructor TALInheritShadow.Create;
begin
  inherited create;
  FInherit := True;
end;

{**************************************************************}
procedure TALInheritShadow.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{********************************************************}
procedure TALInheritShadow.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALInheritShadow then
      Inherit := TALInheritShadow(Source).Inherit
    else
      Inherit := False;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{*********************************************}
procedure TALInheritShadow.Reset;
begin
  BeginUpdate;
  Try
    Inherit := True;
    inherited reset;
  finally
    EndUpdate;
  end;
end;

{***************************************************************************************}
class procedure TALFontManager.RegisterTypefaceFromResource(const AResourceName: string);
begin

  {$IF defined(ALSkiaEngine)}
  var LStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    TSkDefaultProviders.RegisterTypeface(LStream);
  finally
    ALfreeandNil(LStream);
  end;
  {$ENDIF}

  {$IF not defined(ALSkiaEngine) and (defined(AppleOS))}
  var LStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    var LDataProviderRef := CGDataProviderCreateWithData(nil, LStream.Memory, LStream.Size, nil);
    if LDataProviderRef = nil then raise Exception.Create('Failed to create data provider from resource stream');
    Try
      var LFontRef := CGFontCreateWithDataProvider(LDataProviderRef);
      if LFontRef = nil then raise Exception.Create('Failed to create CGFontRef from data provider');
      try
        var LErrorRef: PCFErrorRef;
        if CTFontManagerRegisterGraphicsFont(LFontRef, @LErrorRef) = 0 then begin
          var LNSError: NSError := TNSError.Wrap(LErrorRef);
          Raise Exception.createFmt('Cannot register font: code="%d", description="%s"', [LNSError.code, NSStrToStr(LNSError.localizedDescription)]);
        end;
        {$IF defined(DEBUG)}
        ALLog(
          'TALFontManager.RegisterTypefaceFromResource',
          'ResourceName: '+ AResourceName + ' | '+
          'FullName: '+ NSStrToStr(TNSString.Wrap(CGFontCopyFullName(LFontRef))));
        {$ENDIF}
      finally
         CGFontRelease(LFontRef);
      end;
    Finally
      CGDataProviderRelease(LDataProviderRef);
    end;
  finally
    ALfreeandNil(LStream);
  end;
  {$ENDIF}

end;

{********************************************************************}
function ALConvertFontFamily(const AFontFamily: TFontName): TFontName;
begin
  var LFontFamily := ALTrim(AFontFamily);
  If AlposW(',', LFontFamily) > 0 then begin
    Result := '';
    var LFontFamilies := LFontFamily.Split([',', #13, #10], TStringSplitOptions.ExcludeEmpty);
    for var I := low(LFontFamilies) to high(LFontFamilies) do begin
      LFontFamily := ALConvertFontFamily(ALTrim(LFontFamilies[I]));
      if LFontFamily <> '' then Result := Result + ALIfthenW(Result <> '', ',') + LFontFamily;
    end;
  end
  else if Assigned(ALCustomConvertFontFamilyProc) then begin
    Result := ALCustomConvertFontFamilyProc(LFontFamily)
  end
  else begin
    {$if defined(ANDROID)}
    // In Android, when you want to use the default system font, you should
    // specify "sans-serif" as the font name. Roboto has been the default font
    // for Android since Android 4.0 (Ice Cream Sandwich), and specifying
    // "sans-serif" in your Java/Kotlin code will use Roboto or whichever font
    // is the system default on the user's device. This approach ensures that
    // your application uses the default system font, which provides a consistent
    // user experience across different devices and versions of Android.
    //   sans-serif
    //   sans-serif-thin
    //   sans-serif-light
    //   sans-serif-medium
    //   sans-serif-black
    //   sans-serif-condensed
    //   sans-serif-smallcaps
    result := LFontFamily;
    {$ELSEif defined(ALAppleOS)}
    // https://developer.apple.com/fonts/system-fonts/
    if ALSametextW(LFontFamily, 'sans-serif') then result := 'Helvetica Neue'
    else if ALSametextW(LFontFamily, 'sans-serif-thin') then result := 'Helvetica Neue Thin'
    else if ALSametextW(LFontFamily, 'sans-serif-light') then result := 'Helvetica Neue Light'
    else if ALSametextW(LFontFamily, 'sans-serif-medium') then result := 'Helvetica Neue Medium'
    else if ALSametextW(LFontFamily, 'sans-serif-black') then result := 'Helvetica Neue Bold'
    else result := LFontFamily;
    {$ELSEIF defined(MSWINDOWS)}
    if ALSametextW(LFontFamily, 'sans-serif') then result := 'Segoe UI'
    else if ALSametextW(LFontFamily, 'sans-serif-thin') then result := 'Segoe UI Light'
    else if ALSametextW(LFontFamily, 'sans-serif-light') then result := 'Segoe UI Light'
    else if ALSametextW(LFontFamily, 'sans-serif-medium') then result := 'Segoe UI Semibold'
    else if ALSametextW(LFontFamily, 'sans-serif-black') then result := 'Segoe UI Black'
    else result := LFontFamily;
    {$ELSE}
    result := LFontFamily;
    {$endif}
  end;
end;

{***********************************************************************}
function ALExtractPrimaryFontFamily(const AFontFamilies: String): String;
begin
  Result := '';
  var LFontFamilies := AFontFamilies.Split([',', #13, #10], TStringSplitOptions.ExcludeEmpty);
  for var I := low(LFontFamilies) to high(LFontFamilies) do begin
    Result := ALTrim(LFontFamilies[I]);
    if Result <> '' then break;
  end;
end;

{**}
type
  // packed because of https://stackoverflow.com/questions/61731462/is-this-declaration-good-tdictionarytpairint32-int64-bool
  TALFontMetricsKey = packed record
    FontFamily: {$IF not defined(ALDPK)}TALSHA1Digest{$ELSE}int64{$ENDIF};
    FontSize: single;
    FontWeight: TFontWeight;
    FontSlant: TFontSlant;
    FontColor: TalphaColor;
    DecorationKinds: TALTextDecorationKinds;
  end;

{*}
var
  ALFontMetricsCache: TDictionary<TALFontMetricsKey, TALFontMetrics>;
  ALFontMetricsCacheLock: TLightweightMREW;

{************************}
function ALGetFontMetrics(
           const AFontFamily: String;
           const AFontSize: single;
           const AFontWeight: TFontWeight;
           const AFontSlant: TFontSlant;
           const AFontColor: TalphaColor;
           const ADecorationKinds: TALTextDecorationKinds): TALFontMetrics;
begin

  var LFontMetricsKey: TALFontMetricsKey;
  {$IF not defined(ALDPK)}
  ALStringHashSHA1(LFontMetricsKey.FontFamily, AFontFamily, TEncoding.Utf8);
  {$ELSE}
  LFontMetricsKey.FontFamily := THashFNV1a64.GetHashValue(AFontFamily);
  {$ENDIF}
  LFontMetricsKey.FontSize:= AFontSize;
  LFontMetricsKey.FontWeight:= AFontWeight;
  LFontMetricsKey.FontSlant:= AFontSlant;
  LFontMetricsKey.FontColor:= AFontColor;
  LFontMetricsKey.DecorationKinds:= ADecorationKinds;
  ALFontMetricsCacheLock.beginRead;
  Try
    if ALFontMetricsCache.TryGetValue(LFontMetricsKey, Result) then exit;
  finally
    ALFontMetricsCacheLock.endRead;
  end;

  {$IF defined(ANDROID)}
  var LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true);
  LPaint.setSubpixelText(true);
  LPaint.setFilterBitmap(True);
  LPaint.setDither(true);

  var LFontFamily := ALExtractPrimaryFontFamily(AFontFamily);
  var LFontStyles: TFontStyles := [];
  if AFontWeight in [TFontWeight.Bold,
                     TFontWeight.UltraBold,
                     TFontWeight.Black,
                     TFontWeight.UltraBlack] then LFontStyles := LFontStyles + [TFontStyle.fsBold];
  if AFontSlant in [TFontSlant.Italic, TFontSlant.Oblique] then LFontStyles := LFontStyles + [TFontStyle.fsItalic];
  var LTypeface := TJTypeface.JavaClass.create(StringToJString(LFontFamily), ALfontStyleToAndroidStyle(LFontStyles));
  if TOSVersion.Check(28, 0) then begin
    var LfontWeightInt: Integer;
    case AFontWeight of
      TFontWeight.Thin: LfontWeightInt := 100; //	Thin;
      TFontWeight.UltraLight: LfontWeightInt := 200; //	Extra Light
      TFontWeight.Light: LfontWeightInt := 300; //	Light
      TFontWeight.SemiLight: LfontWeightInt := 350;
      TFontWeight.Regular: LfontWeightInt := 400; //	Normal
      TFontWeight.Medium: LfontWeightInt := 500; //	Medium
      TFontWeight.Semibold: LfontWeightInt := 600; //	Semi Bold
      TFontWeight.Bold: LfontWeightInt := 700; //	Bold
      TFontWeight.UltraBold: LfontWeightInt := 800; //	Extra Bold
      TFontWeight.Black: LfontWeightInt := 900; //	Black
      TFontWeight.UltraBlack: LfontWeightInt := 1000;
      else raise Exception.Create('Error B088F38F-341E-44E4-A844-16A51E35E8A1');
    end;
    LTypeface := TJTypeface.JavaClass.create(
                   LTypeface, {family}
                   LfontWeightInt, {weight}
                   AFontSlant in [TFontSlant.Italic, TFontSlant.Oblique]{italic});
  end;
  LPaint.setTypeface(LTypeface);
  LPaint.setTextSize(AFontSize);
  LPaint.setColor(integer(AFontColor));
  LPaint.setUnderlineText(TALTextDecorationKind.Underline in ADecorationKinds);
  LPaint.setStrikeThruText(TALTextDecorationKind.LineThrough in ADecorationKinds);

  var LFontMetrics := LPaint.getFontMetrics;
  Result.Ascent := LFontMetrics.Ascent;
  Result.Descent := LFontMetrics.Descent;
  Result.Leading := LFontMetrics.Leading;

  LPaint := nil;
  {$ENDIF}

  {$IF defined(ALAppleOS)}

  var Lfont := ALCreateCTFontRef(AFontFamily, AFontSize, AFontWeight, AFontSlant);
  try
    Result.Ascent := -CTFontGetAscent(Lfont);
    Result.Descent := CTFontGetDescent(Lfont);
    Result.Leading := CTFontGetLeading(Lfont);
    {$IF defined(DEBUG)}
    //ALLog(
    //  'ALGetFontMetrics',
    //  'FontFamily: '+ AFontFamily + ' | '+
    //  'FontSize: '+ ALFloatToStrW(AFontSize, ALDefaultFormatSettingsW) + ' | '+
    //  'CTFontGetAscent: ' + ALFloatToStrW(CTFontGetAscent(Lfont), ALDefaultFormatSettingsW) + ' | '+
    //  'CTFontGetDescent: ' + ALFloatToStrW(CTFontGetDescent(Lfont), ALDefaultFormatSettingsW) + ' | '+
    //  'CTFontGetLeading: ' + ALFloatToStrW(CTFontGetLeading(Lfont), ALDefaultFormatSettingsW) + ' | '+
    //  'CTFontGetBoundingBox.height: ' + ALFloatToStrW(CTFontGetBoundingBox(Lfont).size.height, ALDefaultFormatSettingsW));
    {$ENDIF}
  finally
    CFRelease(LFont);
  end;

  {$ENDIF}

  {$IF defined(MSWINDOWS)}

  // Since the Windows API only works with integers, I multiply the font size by 100
  // and later divide the result by 100 to achieve better precision.

  var LFontFamily := ALExtractPrimaryFontFamily(AFontFamily);
  var LFont := CreateFont(
                 -Round(AFontSize*100), // nHeight: Integer;
                 0, // nWidth: Integer;
                 0, // nEscapement: Integer;
                 0, // nOrientaion: Integer;
                 FontWeightToWinapi(AFontWeight), // fnWeight: Integer;
                 Cardinal(not AFontSlant.IsRegular), // fdwItalic: DWORD
                 cardinal(TALTextDecorationKind.Underline in ADecorationKinds), // fdwUnderline: DWORD
                 cardinal(TALTextDecorationKind.LineThrough in ADecorationKinds), // fdwStrikeOut: DWORD
                 DEFAULT_CHARSET, // fdwCharSet: DWORD
                 OUT_DEFAULT_PRECIS, // fdwOutputPrecision: DWORD
                 CLIP_DEFAULT_PRECIS, // fdwClipPrecision: DWORD
                 DEFAULT_QUALITY, // fdwQuality: DWORD
                 DEFAULT_PITCH or FF_DONTCARE, // fdwPitchAndFamily: DWORD
                 PChar(LFontFamily)); // lpszFace: LPCWSTR
  if LFont = 0 then raiseLastOsError;
  try
    var LDC := CreateCompatibleDC(0);
    if LDC = 0 then raiseLastOsError;
    try
      if SelectObject(LDC, LFont) = 0 then raiseLastOsError;
      var LMetrics: TTextMetric;
      if not GetTextMetrics(LDC, LMetrics) then raiseLastOsError;
      Result.Ascent := -LMetrics.tmAscent/100;
      Result.Descent := LMetrics.tmDescent/100;
      Result.Leading := LMetrics.tmExternalLeading/100;
    finally
      if not DeleteDC(LDC) then
        raiseLastOsError
    end;
  finally
    if not DeleteObject(LFont) then raiseLastOsError;
  end;

  {$ENDIF}

  ALFontMetricsCacheLock.beginWrite;
  Try
    ALFontMetricsCache.TryAdd(LFontMetricsKey, Result);
  finally
    ALFontMetricsCacheLock.endWrite;
  end;

end;

{*************************************************}
function  ALTranslate(const AText: string): string;
begin
  if AText = '' then Exit('');
  if Assigned(CustomTranslateProc) then begin
    result := CustomTranslateProc(AText);
    if result = '' then Result := AText;
    Exit;
  end;
  Result := translate(AText);
end;

{**********************************************************}
Procedure ALMakeBufDrawables(const AControl: TControl);
begin
  // This ensures the style is retained when the control exits the visible area.
  // Otherwise, the style will be released and reapplied shortly after.
  AControl.DisableDisappear := true;

  var LDoubleBufferedControl: IALDoubleBufferedControl;
  if Supports(AControl, IALDoubleBufferedControl, LDoubleBufferedControl) then
    LDoubleBufferedControl.MakeBufDrawable;

  for var LChild in aControl.Controls do
    ALMakeBufDrawables(LChild);
end;

{*********************************************}
procedure ALAutoSize(const AControl: TControl);
begin
  var LSize := TSizeF.Create(0,0);
  for var LChildControl in AControl.Controls do begin
    if (csDesigning in AControl.ComponentState) and (LChildControl.ClassName = 'TGrabHandle.TGrabHandleRectangle') then
      continue;
    case LChildControl.Align of

      //--
      TAlignLayout.None: begin
        LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + AControl.padding.right);
        LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + AControl.padding.bottom);
      end;

      //--
      TAlignLayout.Center:;

      //--
      TAlignLayout.Top,
      TAlignLayout.MostTop,
      TAlignLayout.Bottom,
      TAlignLayout.MostBottom: begin
        var LAutosizeControl: IALAutosizeControl;
        if Supports(LChildControl, IALAutosizeControl, LAutosizeControl) and LAutosizeControl.HasUnconstrainedAutosizeX then
          LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + AControl.padding.right)
        else
          LSize.Width := Max(LSize.Width, AControl.Width);
        LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + AControl.padding.bottom);
      end;

      //--
      TAlignLayout.Left,
      TAlignLayout.MostLeft,
      TAlignLayout.Right,
      TAlignLayout.MostRight: Begin
        LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + AControl.padding.right);
        var LAutosizeControl: IALAutosizeControl;
        if Supports(LChildControl, IALAutosizeControl, LAutosizeControl) and LAutosizeControl.HasUnconstrainedAutosizeY then
          LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + AControl.padding.bottom)
        else
          LSize.height := Max(LSize.Height, AControl.Height);
      End;

      //--
      TAlignLayout.Client,
      TAlignLayout.Contents,
      TAlignLayout.Scale,
      TAlignLayout.Fit,
      TAlignLayout.FitLeft,
      TAlignLayout.FitRight: Begin
        var LAutosizeControl: IALAutosizeControl;
        if Supports(LChildControl, IALAutosizeControl, LAutosizeControl) then begin
          if LAutosizeControl.HasUnconstrainedAutosizeX then LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + AControl.padding.right)
          else LSize.Width := Max(LSize.Width, AControl.Width);
          if LAutosizeControl.HasUnconstrainedAutosizeY then LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + AControl.padding.bottom)
          else LSize.height := Max(LSize.Height, AControl.Height);
        end
        else begin
          LSize.Width := Max(LSize.Width, AControl.Width);
          LSize.height := Max(LSize.Height, AControl.Height);
        end;
      End;

      //--
      TAlignLayout.Horizontal,
      TAlignLayout.VertCenter: Begin
        var LAutosizeControl: IALAutosizeControl;
        if Supports(LChildControl, IALAutosizeControl, LAutosizeControl) and LAutosizeControl.HasUnconstrainedAutosizeX then
          LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + AControl.padding.right)
        else
          LSize.Width := Max(LSize.Width, AControl.Width);
      End;

      //--
      TAlignLayout.Vertical,
      TAlignLayout.HorzCenter: Begin
        var LAutosizeControl: IALAutosizeControl;
        if Supports(LChildControl, IALAutosizeControl, LAutosizeControl) and LAutosizeControl.HasUnconstrainedAutosizeY then
          LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + AControl.padding.bottom)
        else
          LSize.height := Max(LSize.Height, AControl.Height);
      End;

    end;
  end;

  if LSize.Width = 0 then LSize.Width := AControl.Width;
  if LSize.Height = 0 then LSize.Height := AControl.Height;
  AControl.SetBounds(AControl.Position.X, AControl.Position.Y, LSize.Width, LSize.Height);
end;

{**************************************************************************************}
function  ALAlignDimensionToPixelRound(const Rect: TRectF; const Scale: single): TRectF;
begin
  result := Rect;
  result.Width := Round(Rect.Width * Scale) / Scale;
  result.height := Round(Rect.height * Scale) / Scale;
end;

{*******************************************************************************************}
function  ALAlignDimensionToPixelRound(const Dimension: single; const Scale: single): single;
begin
  result := Round(Dimension * Scale) / Scale;
end;

{*************************************************************************************}
function  ALAlignDimensionToPixelCeil(const Rect: TRectF; const Scale: single): TRectF;
begin
  result := Rect;
  result.Width := ceil(Rect.Width * Scale - TEpsilon.Vector) / Scale;
  result.height := ceil(Rect.height * Scale - TEpsilon.Vector) / Scale;
end;

{******************************************************************************************}
function  ALAlignDimensionToPixelCeil(const Dimension: single; const Scale: single): single;
begin
  result := ceil(Dimension * Scale - TEpsilon.Vector) / Scale;
end;

{********************************************************************************}
function  ALAlignToPixelRound(const Point: TPointF; const Scale: single): TpointF;
begin
  result.x := round(Point.x * Scale) / Scale;
  result.y := round(Point.y * Scale) / Scale;
end;

{*****************************************************************************}
function  ALAlignToPixelRound(const Rect: TRectF; const Scale: single): TRectF;
begin
  Result.Left := round(Rect.Left * Scale) / Scale;
  Result.Top := round(Rect.Top * Scale) / Scale;
  Result.Right := Result.Left + (Round(Rect.Width * Scale) / Scale); // keep ratio horizontally
  Result.Bottom := Result.Top + (Round(Rect.Height * Scale) / Scale); // keep ratio vertically
end;

{**********************}
{$IF defined(ALAppleOS)}
class function TAlphaColorCGFloat.Create(const R, G, B: CGFloat; const A: CGFloat = 1): TAlphaColorCGFloat;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
class function TAlphaColorCGFloat.Create(const Color: TAlphaColor): TAlphaColorCGFloat;
begin
  Result.R := TAlphaColorRec(Color).R / 255;
  Result.G := TAlphaColorRec(Color).G / 255;
  Result.B := TAlphaColorRec(Color).B / 255;
  Result.A := TAlphaColorRec(Color).A / 255;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
class function TAlphaColorCGFloat.Create(const Color: TAlphaColorf): TAlphaColorCGFloat;
begin
  Result.R := Color.R;
  Result.G := Color.G;
  Result.B := Color.B;
  Result.A := Color.A;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALLowerLeftCGRect(const aUpperLeftOrigin: TPointF; const aWidth, aHeight: single; const aGridHeight: Single): CGRect;
begin
  Result.origin.x := aUpperLeftOrigin.x;
  Result.origin.Y := aGridHeight - aUpperLeftOrigin.y - aHeight;
  Result.size.Width := aWidth;
  Result.size.Height := aHeight;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function  ALCreateCTFontRef(const AFontFamily: String; const AFontSize: single; const AFontWeight: TFontWeight; const AFontSlant: TFontSlant): CTFontRef;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _UpdateFontTraitsAttribute(Const AAttributes: NSMutableDictionary);
  begin
    if (AFontWeight <> TFontWeight.Regular) or (AFontSlant = TFontSlant.Italic) then begin
      var LFontTraits := TNSMutableDictionary.Create;
      try

        if (AFontWeight <> TFontWeight.Regular) then begin
          var LCTFontWeight: Single;
          case AFontWeight of
            TFontWeight.Thin: LCTFontWeight := -0.80; // 100 - NSFontWeightUltraLight
            TFontWeight.UltraLight: LCTFontWeight := -0.60; // 200 - NSFontWeightThin
            TFontWeight.Light: LCTFontWeight := -0.40; // 300 - NSFontWeightLight
            TFontWeight.SemiLight: LCTFontWeight := -0.20; // 350
            TFontWeight.Regular: LCTFontWeight := 0.0; // 400 - NSFontWeightRegular
            TFontWeight.Medium: LCTFontWeight := 0.23; // 500 - NSFontWeightMedium
            TFontWeight.Semibold: LCTFontWeight := 0.30; // 600 - NSFontWeightSemibold
            TFontWeight.Bold: LCTFontWeight := 0.40; // 700 - NSFontWeightBold
            TFontWeight.UltraBold: LCTFontWeight := 0.56; // 800 - NSFontWeightHeavy
            TFontWeight.Black: LCTFontWeight := 0.62; // 900 - NSFontWeightBlack
            TFontWeight.UltraBlack: LCTFontWeight := 1.00; // 1000
            else raise Exception.Create('Error 9F8E0D0B-78A0-4EBE-A4C6-B4098DEE7EFF');
          end;
          LFontTraits.setValue(TNSNumber.OCClass.numberWithFloat(LCTFontWeight), TNSString.Wrap(kCTFontWeightTrait));
        end;

        if (AFontSlant = TFontSlant.Italic) then
          LFontTraits.setValue(TNSNumber.OCClass.numberWithUnsignedInt(kCTFontItalicTrait), TNSString.Wrap(kCTFontSymbolicTrait));

        AAttributes.setValue(NSObjectToID(LFontTraits), TNSString.Wrap(kCTFontTraitsAttribute));

      finally
        LFontTraits.release;
      end;
    end;
  end;

begin
  Result := nil; // To handle a warning bug
  var LAttributes := TNSMutableDictionary.Create;
  try

    var LFallbackFontDescriptorRefs: Tarray<CTFontDescriptorRef>;
    setlength(LFallbackFontDescriptorRefs, 0);
    try

      Var LMainFontFamilySet: Boolean := False;
      var LFontFamilies := String(AFontFamily).Split([',', #13, #10], TStringSplitOptions.ExcludeEmpty);
      for var I := low(LFontFamilies) to high(LFontFamilies) do begin
        var LFontFamily := ALTrim(LFontFamilies[i]);
        if LFontFamily = '' then Continue;
        //--
        If not LMainFontFamilySet then begin
          LAttributes.setValue(StringToID(LFontFamily), TNSString.Wrap(kCTFontFamilyNameAttribute));
          _UpdateFontTraitsAttribute(LAttributes);
          LMainFontFamilySet := True;
        end
        //--
        else begin
          var LFallbackAttributes := TNSMutableDictionary.Create;
          try
            LFallbackAttributes.setValue(StringToID(LFontFamily), TNSString.Wrap(kCTFontFamilyNameAttribute));
            _UpdateFontTraitsAttribute(LFallbackAttributes);
            LFallbackAttributes.setValue(TNSNumber.OCClass.numberWithFloat(AFontSize), TNSString.Wrap(kCTFontSizeAttribute));
            var LFallbackFontDescriptorRef := CTFontDescriptorCreateWithAttributes(CFDictionaryRef(NSObjectToID(LFallbackAttributes)));
            if LFallbackFontDescriptorRef = nil then raise Exception.Create('Error creating fallback font descriptor');
            setlength(LFallbackFontDescriptorRefs, length(LFallbackFontDescriptorRefs) + 1);
            LFallbackFontDescriptorRefs[high(LFallbackFontDescriptorRefs)] := LFallbackFontDescriptorRef;
          finally
            LFallbackAttributes.release;
          end;
        end;
      end;

      if length(LFallbackFontDescriptorRefs) > 0 then begin
        var LCascadeListArray := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LFallbackFontDescriptorRefs[0], Length(LFallbackFontDescriptorRefs)));
        LAttributes.setValue(NSObjectToID(LCascadeListArray), TNSString.Wrap(kCTFontCascadeListAttribute));
      end;

      var LFontDescriptorRef := CTFontDescriptorCreateWithAttributes(CFDictionaryRef(NSObjectToID(LAttributes)));
      if LFontDescriptorRef = nil then raise Exception.Create('Error creating font descriptor');
      Try
        Result := CTFontCreateWithFontDescriptor(LFontDescriptorRef, AFontSize, nil);
        if Result = nil then raise Exception.Create('Error creating font');
      finally
        CFRelease(LFontDescriptorRef);
      end;

    finally
      for var I := Low(LFallbackFontDescriptorRefs) to High(LFallbackFontDescriptorRefs) do
        CFRelease(LFallbackFontDescriptorRefs[i]);
    end;
  finally
    LAttributes.release;
  end;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function ALTextHorzAlignToUITextAlignment(const ATextHorzAlign: TALTextHorzAlign): UITextAlignment;
begin
  case ATextHorzAlign of
    TALTextHorzAlign.Center:   Result := UITextAlignmentCenter;
    TALTextHorzAlign.Leading:  Result := UITextAlignmentLeft;
    TALTextHorzAlign.Trailing: Result := UITextAlignmentRight;
    TALTextHorzAlign.Justify:  Result := UITextAlignmentLeft;
    else Raise Exception.Create('Error 1F3C2FAF-354E-4584-A269-DEFD7E626A4A');
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ALMacOS)}
function ALTextHorzAlignToNSTextAlignment(const ATextHorzAlign: TALTextHorzAlign): NSTextAlignment;
begin
  case ATextHorzAlign of
    TALTextHorzAlign.Center:   Result := NSCenterTextAlignment;
    TALTextHorzAlign.Leading:  Result := NSLeftTextAlignment;
    TALTextHorzAlign.Trailing: Result := NSRightTextAlignment;
    TALTextHorzAlign.Justify:  Result := NSJustifiedTextAlignment;
    else Raise Exception.Create('Error 8E1D2DC2-33BA-4A53-9CE4-977748C1CAE0');
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALfontStyleToAndroidStyle(const afontStyle: TfontStyles): integer;
begin
  if (TFontStyle.fsBold in afontStyle) and
     (TFontStyle.fsItalic in afontStyle) then result := TJTypeface.JavaClass.BOLD_ITALIC
  else if (TFontStyle.fsBold in afontStyle) then result := TJTypeface.JavaClass.BOLD
  else if (TFontStyle.fsItalic in afontStyle) then result := TJTypeface.JavaClass.ITALIC
  else result := TJTypeface.JavaClass.NORMAL;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALStringsToJArrayList(const AStrings: TArray<String>): JArrayList;
var S: JString;
    LString: String;
begin
  Result := TJArrayList.JavaClass.init(Length(AStrings));
  for LString in AStrings do begin
    S := StringToJString(LString);
    Result.add(S);
  end;
end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALJSetToStrings(const ASet: JSet): TArray<String>;
var Iterator: JIterator;
    Index: Integer;
    S: JString;
begin
  SetLength(Result, ASet.size);
  if ASet.size > 0 then begin
    Index := 0;
    Iterator := ASet.iterator;
    while Iterator.hasNext do begin
      S := TJString.Wrap((Iterator.next as ILocalObject).GetObjectID);
      if S <> nil then begin
        Result[Index] := JStringToString(S);
        Inc(Index);
      end;
    end;
    SetLength(Result, Index);
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALStringsToNSArray(const AStrings: TArray<String>): NSMutableArray;
var S: NSString;
    LString: String;
begin
  Result := TNSMutableArray.Create;
  for LString in AStrings do begin
    S := StrToNSStr(LString);
    Result.addObject(NSObjectToID(S));
  end;
end;
{$ENDIF}

{**********************}
{$IF defined(ALAppleOS)}
function ALNSSetToStrings(const ANSSet: NSSet): TArray<String>;
var LStringArray: NSArray;
    LString: String;
    I: Integer;
begin
  if ANSSet <> nil then begin
    SetLength(Result, ANSSet.count);
    if ANSSet.count > 0 then begin
      LStringArray := ANSSet.allObjects;
      for I := 0 to LStringArray.Count - 1 do begin
        LString := NSStrToStr(TNSString.Wrap(LStringArray.objectAtIndex(I)));
        Result[I] := LString;
      end;
    end;
  end;
end;
{$ENDIF}

{**************}
{$IFDEF ANDROID}

// https://developer.android.com/guide/topics/renderscript/compute.html
// You should consider context creation to be a potentially long-running operation, since it
// may create resources on different pieces of hardware; it should not be in an application's
// critical path if at all possible. Typically, an application will have only a single
// RenderScript context at a time.

var _RenderScript: JRenderScript;

{**************************************}
function getRenderScript: JRenderScript;
begin
  if _RenderScript = nil then begin
    Tmonitor.Enter(Application);
    try
      if _RenderScript = nil then
        _RenderScript := TJRenderScript.JavaClass.create(TandroidHelper.Context);
    finally
      Tmonitor.Exit(Application);
    end;
  end;
  result := _RenderScript;
end;

{$ENDIF}

{$IFDEF ALDPK}
function ALDPKGetResourceFilename(const AResourceName: String): String;
begin
  Result := '';
  if TFile.Exists(getActiveProject.fileName) then begin
    var LDProjSrc := ALGetStringFromFile(getActiveProject.fileName, TEncoding.utf8);
    //<RcItem Include="resources\account_100x100.png">
    //    <ResourceType>RCDATA</ResourceType>
    //    <ResourceId>account_100x100</ResourceId>
    //</RcItem>
    Var P1: Integer := ALposIgnoreCaseW('<ResourceId>'+AResourceName+'</ResourceId>', LDProjSrc);
    While (P1 > 1) and ((LDProjSrc[P1-1] <> '=') or (LDProjSrc[P1] <> '"')) do dec(P1);
    if (P1 > 0) then begin
      var P2: Integer := ALPosW('"', LDProjSrc, P1+1);
      if P2 > P1 then begin
        Result := extractFilePath(getActiveProject.fileName) + ALcopyStr(LDProjSrc, P1+1, P2-P1-1);
        if not TFile.Exists(Result) then Result := '';
      end;
    end;
  end;
  if Result = '' then begin
    Result := extractFilePath(getActiveProject.fileName) + 'Resources\' + AResourceName; // by default all the resources files must be located in the sub-folder /Resources/ of the project
    if not TFile.Exists(Result) then begin
      if TFile.Exists(Result + '.png') then Result := Result + '.png'
      else if TFile.Exists(Result + '.jpg') then Result := Result + '.jpg'
      else Result := '';
    end;
  end;
end;
{$ENDIF}

{**************************}
procedure ALInitScreenScale;
begin
  if ALScreenScale = 0 then begin
    var LScreenService: IFMXScreenService;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, LScreenService) then
      ALScreenScale := LScreenService.GetScreenScale
    else
      ALScreenScale := 1;
    {$IF defined(debug)}
    ALLog('Screen Scale', ALFloatToStrW(ALScreenScale, ALDefaultFormatSettingsW));
    {$ENDIF}
  end;
end;

{********************************}
function ALGetScreenScale: Single;
begin
  result := ALScreenScale;
  if Result = 0 then begin
    ALInitScreenScale;
    result := ALScreenScale;
  end;
end;

initialization
  ALScreenScale := 0;
  ALCustomConvertFontFamilyProc := nil;
  {$IFDEF ANDROID}
  ALViewStackCount := 0;
  _RenderScript := nil;
  {$ENDIF}
  ALFontMetricsCache := TDictionary<TALFontMetricsKey, TALFontMetrics>.Create;
  //_FontMetricsCacheLock := ??; their is no TLightweightMREW.create but instead an ugly class operator TLightweightMREW.Initialize :(

finalization
  AlFreeAndNil(ALFontMetricsCache);

end.
