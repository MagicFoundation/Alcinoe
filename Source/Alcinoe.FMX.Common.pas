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
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Alcinoe.AndroidApi.Common,
  Alcinoe.FMX.NativeView.Android,
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  Alcinoe.FMX.NativeView.Win,
  {$ENDIF}
  {$IF defined(ALSkiaEngine)}
  System.Skia.API,
  {$ENDIF}
  Fmx.types3D,
  Fmx.types,
  FMX.TextLayout,
  FMX.graphics,
  FMX.Filter,
  FMX.Effects,
  FMX.controls,
  Alcinoe.FMX.Ani,
  ALcinoe.FMX.Controls,
  Alcinoe.FMX.ScrollEngine;

var
  // The ALBrokenImageResourceName variable specifies the
  // name of the local image resource to be used as a fallback
  // when the remote image assigned to a TImage is unreachable.
  // The ALBrokenImageWidth and ALBrokenImageHeight variables
  // define the dimensions (in pixels) of this fallback image,
  // ensuring a consistent display when the intended remote image
  // cannot be loaded.
  ALBrokenImageResourceName: String;
  ALBrokenImageWidth: Integer;
  ALBrokenImageHeight: Integer;

type

  // !! Workaround to avoid a circular unit reference. The declaration of TALSurface/TALCanvas/TALBitmap/TALDrawable from Alcinoe.FMX.Graphics is duplicated here.
  TALSurface =  {$IF defined(ALSkiaEngine)}sk_surface_t{$ELSEIF defined(ANDROID)}Jbitmap{$ELSEIF defined(ALAppleOS)}CGContextRef{$ELSE}Tbitmap{$ENDIF};
  TALCanvas =   {$IF defined(ALSkiaEngine)}sk_canvas_t{$ELSEIF defined(ANDROID)}Jcanvas{$ELSEIF defined(ALAppleOS)}CGContextRef{$ELSE}Tcanvas{$ENDIF};
  TALBitmap =   {$IF defined(ALSkiaEngine)}sk_image_t{$ELSEIF defined(ANDROID)}JBitmap{$ELSEIF defined(ALAppleOS)}CGImageRef{$ELSE}Tbitmap{$ENDIF};
  TALDrawable = {$IF defined(ALSkiaCanvas)}sk_image_t{$ELSEIF defined(ALGpuCanvas)}TTexture{$ELSE}Tbitmap{$ENDIF};

  {~~~~~~~~~~~~~~~~~~}
  TALImageWrapMode = (
    // Display the image with its original dimensions:
    // * The image is placed in the upper-left corner of the rectangle of the control.
    // * If the image is larger than the control's rectangle, then only the upper-left part of the image,
    //   which fits in the rectangle of the control, is shown. The image is not resized.
    //Original,

    /// <summary>
    ///   Best fit the image in the rectangle of the control.
    /// </summary>
    /// <remarks><para>
    ///   * If any dimension of the image is larger than the rectangle of the control, then scales down the image
    ///     (keeping image proportions – the ratio between the width and height) to fit the whole image in the rectangle
    ///     of the control. That is, either the width of the resized image is equal to the width of the control's rectangle
    ///     or the height of the resized image is equal to the height of the rectangle of the control. The whole image
    ///     should be displayed. The image is displayed centered in the rectangle of the control.
    /// </para><para>
    ///   * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    ///     the rectangle of the control. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    /// </para></remarks>
    Fit,

    /// <summary>
    ///   Stretch the image to fill the entire rectangle of the control
    /// </summary>
    Stretch,

    // Tile (multiply) the image to cover the entire rectangle of the control:
    // * If the image is larger than the rectangle of the control, then only the
    //   upper-left part of the image, which fits in the rectangle of the control, is shown. The image is not resized.
    // * If the image (original size) is smaller than the rectangle of the control, then the multiple images are tiled
    //   (placed one next to another) to fill the entire rectangle of the control. The images are placed beginning from
    //   the upper-left corner of the rectangle of the control.
    //Tile,

    /// <summary>
    ///   Fit the image in the rectangle of the control:
    /// </summary>
    /// <remarks><para>
    ///   * If any dimension of the image is larger than the rectangle of the control, then scales down the image (keeping image proportions--the ratio between the width and height)
    ///     to fit the whole image in the rectangle of the control. That is, either the width of the resized image is equal to the width of the control's rectangle or the height of the
    ///     resized image is equal to the height of the control's rectangle. Whole the image should be displayed. The image is displayed centered in the rectangle of the control.
    /// </para><para>
    ///   * If the original image is smaller than the rectangle of the control, then the image is not resized. The image is displayed centered in the rectangle of the control.
    /// </para></remarks>
    Place,

    /// <summary>
    ///   Best fit the image in the rectangle of the control:
    /// </summary>
    /// <remarks><para>
    ///   * If any dimension of the image is larger than the rectangle of the control, then scales down the image
    ///     (keeping image proportions – the ratio between the width and height) to fit the height or the width of the image in the rectangle
    ///     of the control and crop the extra part of the image. That is, the width of the resized image is equal to the width of the control's rectangle
    ///     AND the height of the resized image is equal to the height of the rectangle of the control.
    /// </para><para>
    ///   * If the original image is smaller than the rectangle of the control, then the image is stretched to best fit in
    ///     the rectangle of the control. Whole the image should be displayed.
    /// </para></remarks>
    FitAndCrop);

type

  {~~~~~~~~~~~~~~~}
  TALBrush = Class;
  TALStateLayer = Class;
  TALStrokeBrush = class;
  TALShadow = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALScrollableControl = interface
    ['{6750E04D-8DB6-4F27-898A-B20AD55CAAF4}']
    function GetScrollEngine: TALScrollEngine;
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
    FUpdateCount: Integer; // 4 Bytes
    FIsChanged: Boolean; // 1 Bytes
    FOnChanged: TNotifyEvent; // 16 Bytes
    FSavedStates: TObjectQueue<TALPersistentObserver>; // 8 Bytes
    procedure DoChanged; virtual;
  protected
    function CreateSavedState: TALPersistentObserver; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure EndUpdateNoChanges; virtual;
    procedure SaveState; virtual;
    procedure RestoreState; virtual;
    procedure RestoreStateNoChanges; virtual;
    procedure Change; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Types.TBounds was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALBounds = class(TPersistent)
  private
    FLeft: Single; // 4 Bytes
    FTop: Single; // 4 Bytes
    FRight: Single; // 4 Bytes
    FBottom: Single; // 4 Bytes
    FOnChange: TNotifyEvent; // 16 Bytes
    function GetRect: TRectF;
    procedure SetRect(const Value: TRectF);
    procedure SetBottom(const Value: Single);
    procedure SetLeft(const Value: Single);
    procedure SetRight(const Value: Single);
    procedure SetTop(const Value: Single);
    function IsBottomStored: Boolean;
    function IsLeftStored: Boolean;
    function IsRightStored: Boolean;
    function IsTopStored: Boolean;
  protected
    function GetDefaultValue: TRectF; virtual;
    procedure DoChange; virtual;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    function Equals(Obj: TObject): Boolean; override;
    function PaddingRect(const R: TRectF): TRectF;
    function MarginRect(const R: TRectF): TRectF;
    function Width: Single;
    function Height: Single;
    property Rect: TRectF read GetRect write SetRect;
    property DefaultValue: TRectF read GetDefaultValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function Empty: Boolean;
    function MarginEmpty: Boolean;
    function ToString: string; override;
  published
    property Left: Single read FLeft write SetLeft stored IsLeftStored nodefault;
    property Top: Single read FTop write SetTop stored IsTopStored nodefault;
    property Right: Single read FRight write SetRight stored IsRightStored nodefault;
    property Bottom: Single read FBottom write SetBottom stored IsBottomStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Types.TPosition was not updated and adjust the IFDEF'}
  {$ENDIF}
  TALPosition = class(TPersistent)
  private
    FY: Single; // 4 Bytes
    FX: Single; // 4 Bytes
    FOnChange: TNotifyEvent; // 16 Bytes
    procedure SetPoint(const Value: TPointF);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    function GetPoint: TPointF;
    function IsXStored: Boolean;
    function IsYStored: Boolean;
  protected
    function GetDefaultValue: TPointF; virtual;
    procedure DoChange; virtual;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure SetPointNoChange(const P: TPointF);
    function Empty: Boolean;
    procedure Reflect(const Normal: TPointF);
    property Point: TPointF read GetPoint write SetPoint;
    property DefaultValue: TPointF read GetDefaultValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property X: Single read FX write SetX stored IsXStored nodefault;
    property Y: Single read FY write SetY stored IsYStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALShadow = class(TALPersistentObserver)
  private
    FBlur: Single; // 4 bytes
    FOffsetX: Single; // 4 bytes
    FOffsetY: Single; // 4 bytes
    FColor: TAlphaColor; // 4 bytes
    procedure setblur(const Value: Single);
    procedure setOffsetX(const Value: Single);
    procedure setOffsetY(const Value: Single);
    procedure setColor(const Value: TAlphaColor);
    function IsblurStored: Boolean;
    function IsOffsetXStored: Boolean;
    function IsOffsetYStored: Boolean;
    function IsColorStored: Boolean;
  {$IF defined(ALBackwardCompatible)}
  private
    procedure ReadEnabled(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  {$ENDIF}
  protected
    function GetDefaultblur: Single; virtual;
    function GetDefaultOffsetX: Single; virtual;
    function GetDefaultOffsetY: Single; virtual;
    function GetDefaultColor: TAlphaColor; virtual;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure Interpolate(const ATo: TALShadow; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALShadow; const ANormalizedTime: Single);
    function HasShadow: boolean; virtual;
    property Defaultblur: Single read GetDefaultblur;
    property DefaultOffsetX: Single read GetDefaultOffsetX;
    property DefaultOffsetY: Single read GetDefaultOffsetY;
    property DefaultColor: TAlphaColor read GetDefaultColor;
  published
    property blur: Single read fblur write setblur stored IsblurStored nodefault;
    property OffsetX: Single read fOffsetX write setOffsetX stored IsOffsetXStored nodefault;
    property OffsetY: Single read fOffsetY write setOffsetY stored IsOffsetYStored nodefault;
    property Color: TAlphaColor read fColor write setColor stored IsColorStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritShadow = class(TALShadow)
  private
    FParent: TALShadow; // 8 bytes
    FInherit: Boolean; // 1 byte
    fSuperseded: Boolean; // 1 byte
    procedure SetInherit(const AValue: Boolean);
  protected
    function CreateSavedState: TALPersistentObserver; override;
    procedure DoSupersede; virtual;
  public
    constructor Create(const AParent: TALShadow); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede(Const ASaveState: Boolean = False); virtual;
    procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
    property Superseded: Boolean read FSuperseded;
    property Parent: TALShadow read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTextDecorationKind = (Underline, Overline, LineThrough);
  TALTextDecorationKinds = set of TALTextDecorationKind;
  TALTextDecorationStyle = (Solid, Double, Dotted, Dashed, Wavy);
  TALTextHorzAlign = (Center, Leading, Trailing, Justify);
  TALTextVertAlign = (Center, Leading, Trailing);
  TALTextDirection = (RightToLeft, LeftToRight);
  TALTextTrimming = (Character, Word);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALFont = class(TALPersistentObserver)
  public
    class var SansSerifFamily: String;
  private
    FFamily: TFontName; // 8 bytes
    FSize: Single; // 4 bytes
    FWeight: TFontWeight; // 4 bytes (because FMX.Graphics use {$MINENUMSIZE 4})
    FSlant: TFontSlant; // 4 bytes (because FMX.Graphics use {$MINENUMSIZE 4})
    FStretch: TFontStretch; // 4 bytes (because FMX.Graphics use {$MINENUMSIZE 4})
    FColor: TAlphaColor; // 4 bytes
    procedure SetFamily(const AValue: TFontName);
    procedure SetSize(const AValue: Single);
    procedure SetWeight(const AValue: TFontWeight);
    procedure SetSlant(const AValue: TFontSlant);
    procedure SetStretch(const AValue: TFontStretch);
    procedure SetColor(const AValue: TAlphaColor);
    function IsFamilyStored: Boolean;
    function IsSizeStored: Boolean;
    function IsWeightStored: Boolean;
    function IsSlantStored: Boolean;
    function IsStretchStored: Boolean;
    function IsColorStored: Boolean;
  {$IF defined(ALBackwardCompatible)}
  private
    procedure ReadStyleExt(AStream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  {$ENDIF}
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDefaultFamily: TFontName; virtual;
    function GetDefaultSize: Single; virtual;
    function GetDefaultWeight: TFontWeight; virtual;
    function GetDefaultSlant: TFontSlant; virtual;
    function GetDefaultStretch: TFontStretch; virtual;
    function GetDefaultColor: TAlphaColor; virtual;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure Interpolate(const ATo: TALFont; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALFont; const ANormalizedTime: Single);
    property DefaultFamily: TFontName read GetDefaultFamily;
    property DefaultSize: Single read GetDefaultSize;
    property DefaultWeight: TFontWeight read GetDefaultWeight;
    property DefaultSlant: TFontSlant read GetDefaultSlant;
    property DefaultStretch: TFontStretch read GetDefaultStretch;
    property DefaultColor: TAlphaColor read GetDefaultColor;
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored nodefault;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
    property Weight: TFontWeight read FWeight write SetWeight stored IsWeightStored;
    property Slant: TFontSlant read FSlant write SetSlant stored IsSlantStored;
    property Stretch: TFontStretch read FStretch write SetStretch stored IsStretchStored;
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTextDecoration = class(TALPersistentObserver)
  private
    FKinds: TALTextDecorationKinds; // 1 byte
    FStyle: TALTextDecorationStyle; // 1 byte
    FThicknessMultiplier: Single; // 4 bytes
    FColor: TAlphaColor; // 4 bytes
    procedure SetKinds(const AValue: TALTextDecorationKinds);
    procedure SetStyle(const AValue: TALTextDecorationStyle);
    procedure SetThicknessMultiplier(const AValue: Single);
    procedure SetColor(const AValue: TAlphaColor);
    function IsKindsStored: Boolean;
    function IsStyleStored: Boolean;
    function IsThicknessMultiplierStored: Boolean;
    function IsColorStored: Boolean;
  protected
    function GetDefaultKinds: TALTextDecorationKinds; virtual;
    function GetDefaultStyle: TALTextDecorationStyle; virtual;
    function GetDefaultThicknessMultiplier: Single; virtual;
    function GetDefaultColor: TAlphaColor; virtual;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALTextDecoration; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALTextDecoration; const ANormalizedTime: Single);
    property DefaultKinds: TALTextDecorationKinds read GetDefaultKinds;
    property DefaultStyle: TALTextDecorationStyle read GetDefaultStyle;
    property DefaultThicknessMultiplier: Single read GetDefaultThicknessMultiplier;
    property DefaultColor: TAlphaColor read GetDefaultColor;
  published
    property Kinds: TALTextDecorationKinds read FKinds write SetKinds Stored IsKindsStored;
    property Style: TALTextDecorationStyle read FStyle write SetStyle Stored IsStyleStored;
    property ThicknessMultiplier: Single read FThicknessMultiplier write SetThicknessMultiplier Stored IsThicknessMultiplierStored nodefault;
    property Color: TAlphaColor read FColor write SetColor Stored IsColorStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALEllipsisSettings = class(TALPersistentObserver)
  private
    FInherit: Boolean; // 1 byte
    FFont: TALFont; // 8 bytes
    FDecoration: TALTextDecoration; // 8 bytes
    procedure SetInherit(const AValue: Boolean);
    procedure SetFont(const AValue: TALFont);
    procedure SetDecoration(const AValue: TALTextDecoration);
    procedure FontChanged(ASender: TObject);
    procedure DecorationChanged(ASender: TObject);
    function IsInheritStored: Boolean;
  protected
    function CreateFont: TALFont; virtual;
    function CreateDecoration: TALTextDecoration; virtual;
    function GetDefaultInherit: Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure Interpolate(const ATo: TALEllipsisSettings; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALEllipsisSettings; const ANormalizedTime: Single);
    property DefaultInherit: Boolean read GetDefaultInherit;
  published
    property Inherit: Boolean read FInherit write SetInherit stored IsInheritStored;
    property Font: TALFont read FFont write SetFont;
    property Decoration: TALTextDecoration read FDecoration write SetDecoration;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseTextSettings = class(TALPersistentObserver)
  public
    class var HorizontalEllipsis: String;
  private
    FFont: TALFont; // 8 bytes
    FDecoration: TALTextDecoration; // 8 bytes
    FEllipsis: String; // 8 bytes
    FEllipsisSettings: TALEllipsisSettings; // 8 bytes
    FMaxLines: integer; // 4 bytes
    FIsHtml: Boolean; // 1 byte
    FTrimming: TALTextTrimming; // 1 byte
    FHorzAlign: TALTextHorzAlign; // 1 byte
    FVertAlign: TALTextVertAlign; // 1 byte
    FLineHeightMultiplier: Single; // 4 bytes
    FLetterSpacing: Single; // 4 bytes
    procedure SetFont(const AValue: TALFont);
    procedure SetDecoration(const AValue: TALTextDecoration);
    procedure SetEllipsis(const AValue: String);
    procedure SetEllipsisSettings(const AValue: TALEllipsisSettings);
    procedure SetMaxLines(const AValue: Integer);
    procedure SetIsHtml(const AValue: Boolean);
    procedure SetTrimming(const AValue: TALTextTrimming);
    procedure SetHorzAlign(const AValue: TALTextHorzAlign);
    procedure SetVertAlign(const AValue: TALTextVertAlign);
    procedure SetLineHeightMultiplier(const AValue: Single);
    procedure SetLetterSpacing(const AValue: Single);
    procedure FontChanged(ASender: TObject);
    procedure DecorationChanged(ASender: TObject);
    procedure EllipsisSettingsChanged(ASender: TObject);
    function IsEllipsisStored: Boolean;
    function IsMaxLinesStored: Boolean;
    function IsIsHtmlStored: Boolean;
    function IsTrimmingStored: Boolean;
    function IsHorzAlignStored: Boolean;
    function IsVertAlignStored: Boolean;
    function IsLineHeightMultiplierStored: Boolean;
    function IsLetterSpacingStored: Boolean;
  {$IF defined(ALBackwardCompatible)}
  private
    procedure ReadFontColor(Reader: TReader);
    procedure ReadWordWrap(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  {$ENDIF}
  protected
    function CreateFont: TALFont; virtual;
    function CreateDecoration: TALTextDecoration; virtual;
    function CreateEllipsisSettings: TALEllipsisSettings; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    function GetDefaultEllipsis: String; virtual;
    function GetDefaultMaxLines: Integer; virtual;
    function GetDefaultIsHtml: Boolean; virtual;
    function GetDefaultTrimming: TALTextTrimming; virtual;
    function GetDefaultHorzAlign: TALTextHorzAlign; virtual;
    function GetDefaultVertAlign: TALTextVertAlign; virtual;
    function GetDefaultLineHeightMultiplier: Single; virtual;
    function GetDefaultLetterSpacing: Single; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure Interpolate(const ATo: TALBaseTextSettings; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALBaseTextSettings; const ANormalizedTime: Single);
    //--
    property DefaultEllipsis: String read GetDefaultEllipsis;
    property DefaultMaxLines: Integer read GetDefaultMaxLines;
    property DefaultIsHtml: Boolean read GetDefaultIsHtml;
    property DefaultTrimming: TALTextTrimming read GetDefaultTrimming;
    property DefaultHorzAlign: TALTextHorzAlign read GetDefaultHorzAlign;
    property DefaultVertAlign: TALTextVertAlign read GetDefaultVertAlign;
    property DefaultLineHeightMultiplier: Single read GetDefaultLineHeightMultiplier;
    property DefaultLetterSpacing: Single read GetDefaultLetterSpacing;
    //--
    property Font: TALFont read FFont write SetFont;
    property Decoration: TALTextDecoration read FDecoration write SetDecoration;
    property Ellipsis: String read FEllipsis write SetEllipsis stored IsEllipsisStored nodefault;
    property EllipsisSettings: TALEllipsisSettings read FEllipsisSettings write SetEllipsisSettings;
    property MaxLines: Integer read FMaxLines write SetMaxLines stored IsMaxLinesStored;
    property IsHtml: Boolean read FIsHtml write SetIsHtml stored IsIsHtmlStored;
    property Trimming: TALTextTrimming read FTrimming write SetTrimming stored IsTrimmingStored;
    property HorzAlign: TALTextHorzAlign read FHorzAlign write SetHorzAlign stored IsHorzAlignStored;
    property VertAlign: TALTextVertAlign read FVertAlign write SetVertAlign stored IsVertAlignStored;
    property LineHeightMultiplier: Single read FLineHeightMultiplier write SetLineHeightMultiplier stored IsLineHeightMultiplierStored nodefault;
    property LetterSpacing: Single read FLetterSpacing write SetLetterSpacing stored IsLetterSpacingStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTextSettings = class(TALBaseTextSettings)
  published
    property Font;
    property Decoration;
    property Ellipsis;
    property EllipsisSettings;
    property MaxLines;
    property IsHtml;
    property Trimming;
    property HorzAlign;
    property VertAlign;
    property LineHeightMultiplier;
    property LetterSpacing;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritBaseTextSettings = class(TALBaseTextSettings)
  private
    FParent: TALBaseTextSettings; // 8 bytes
    FInherit: Boolean; // 1 byte
    fSuperseded: Boolean; // 1 byte
    procedure SetInherit(const AValue: Boolean);
  protected
    function CreateSavedState: TALPersistentObserver; override;
    procedure DoSupersede; virtual;
  public
    constructor Create(const AParent: TALBaseTextSettings); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede(Const ASaveState: Boolean = False); virtual;
    procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
    property Superseded: Boolean read FSuperseded;
    property Parent: TALBaseTextSettings read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALGradient = class(TALPersistentObserver)
  private
    FStyle: TGradientStyle; // 4 bytes (because FMX.Graphics use {$MINENUMSIZE 4})
    FAngle: Single; // 4 bytes
    FColors: TArray<TAlphaColor>; // 8 bytes
    FOffsets: TArray<Single>; // 8 bytes
    function GetCSSFormat: String;
    procedure SetStyle(const Value: TGradientStyle);
    procedure SetAngle(const Value: Single);
    procedure SetColors(const Value: TArray<TAlphaColor>);
    procedure SetOffsets(const Value: TArray<Single>);
    procedure SetCSSFormat(const Value: String);
    function IsStyleStored: Boolean;
    function IsAngleStored: Boolean;
    procedure ReadColors(Reader: TReader);
    procedure WriteColors(Writer: TWriter);
    procedure ReadOffsets(Reader: TReader);
    procedure WriteOffsets(Writer: TWriter);
  protected
    function GetDefaultStyle: TGradientStyle; virtual;
    function GetDefaultAngle: Single; virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALGradient; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALGradient; const ANormalizedTime: Single);
    property DefaultStyle: TGradientStyle read GetDefaultStyle;
    property DefaultAngle: Single read GetDefaultAngle;
    property Colors: TArray<TAlphaColor> read FColors write SetColors;
    property Offsets: TArray<Single> read FOffsets write SetOffsets;
    property CSSFormat: String read GetCSSFormat Write SetCSSFormat;
  published
    property Style: TGradientStyle read FStyle write SetStyle stored IsStyleStored;
    property Angle: Single read FAngle write SetAngle stored IsAngleStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBrushStyle = (Solid, Gradient, Image);
  TALBrushStyles = set of TALBrushStyle;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBrush = class(TALPersistentObserver)
  private
    {--- Logical order ---
    FColor: TAlphaColor;
    FGradient: TALGradient;
    FResourceName: String;
    FBackgroundMargins: TALBounds;
    FImageMargins: TALBounds;
    FImageNoRadius: Boolean;
    FWrapMode: TALImageWrapMode;
    --- Memory Optimization ---}
    FResourceName: String; // 8 bytes
    FGradient: TALGradient; // 8 bytes
    FBackgroundMargins: TALBounds; // 8 bytes
    FImageMargins: TALBounds; // 8 bytes
    FImageNoRadius: Boolean; // 1 byte
    FWrapMode: TALImageWrapMode; // 1 byte
    FColor: TAlphaColor; // 4 bytes
    procedure SetColor(const Value: TAlphaColor);
    procedure SetGradient(const Value: TALGradient);
    procedure SetResourceName(const Value: String);
    procedure SetBackgroundMargins(const Value: TALBounds);
    procedure SetImageMargins(const Value: TALBounds);
    procedure SetImageNoRadius(const Value: Boolean);
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure GradientChanged(Sender: TObject); virtual;
    procedure BackgroundMarginsChanged(Sender: TObject); virtual;
    procedure ImageMarginsChanged(Sender: TObject); virtual;
    function IsColorStored: Boolean;
    function IsResourceNameStored: Boolean;
    function IsImageNoRadiusStored: Boolean;
    function IsWrapModeStored: Boolean;
  {$IF defined(ALBackwardCompatible)}
  private
    procedure ReadKind(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  {$ENDIF}
  protected
    function CreateBackgroundMargins: TALBounds; virtual;
    function CreateImageMargins: TALBounds; virtual;
    function GetDefaultColor: TAlphaColor; virtual;
    function GetDefaultResourceName: String; virtual;
    function GetDefaultImageNoRadius: Boolean; virtual;
    function GetDefaultWrapMode: TALImageWrapMode; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure Interpolate(const ATo: TALBrush; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALBrush; const ANormalizedTime: Single);
    function HasFill: boolean; virtual;
    function Styles: TALBrushStyles; virtual;
    function IsRemoteResource: Boolean;
    property DefaultColor: TAlphaColor read GetDefaultColor;
    property DefaultResourceName: String read GetDefaultResourceName;
    property DefaultImageNoRadius: Boolean read GetDefaultImageNoRadius;
    property DefaultWrapMode: TALImageWrapMode read GetDefaultWrapMode;
  published
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
    property Gradient: TALGradient read FGradient write SetGradient;
    property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored nodefault;
    property BackgroundMargins: TALBounds read FBackgroundMargins write SetBackgroundMargins;
    property ImageMargins: TALBounds read FImageMargins write SetImageMargins;
    property ImageNoRadius: Boolean read FImageNoRadius write SetImageNoRadius stored IsImageNoRadiusStored;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritBrush = class(TALBrush)
  private
    FParent: TALBrush; // 8 bytes
    FInherit: Boolean; // 1 byte
    fSuperseded: Boolean; // 1 byte
    procedure SetInherit(const AValue: Boolean);
  protected
    function CreateSavedState: TALPersistentObserver; override;
    procedure DoSupersede; virtual;
  public
    constructor Create(const AParent: TALBrush); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede(Const ASaveState: Boolean = False); virtual;
    procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
    property Superseded: Boolean read FSuperseded;
    property Parent: TALBrush read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALStrokeBrush = class(TALPersistentObserver)
  private
    FColor: TAlphaColor; // 4 bytes
    FThickness: Single; // 4 bytes
    procedure SetColor(const Value: TAlphaColor);
    procedure SetThickness(const Value: Single);
    function IsColorStored: Boolean;
    function IsThicknessStored: Boolean;
  {$IF defined(ALBackwardCompatible)}
  private
    procedure ReadKind(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  {$ENDIF}
  protected
    function GetDefaultColor: TAlphaColor; virtual;
    function GetDefaultThickness: Single; virtual;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure Interpolate(const ATo: TALStrokeBrush; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALStrokeBrush; const ANormalizedTime: Single);
    function HasStroke: boolean; virtual;
    property DefaultColor: TAlphaColor read GetDefaultColor;
    property DefaultThickness: Single read GetDefaultThickness;
  published
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
    property Thickness: Single read FThickness write SetThickness stored IsThicknessStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritStrokeBrush = class(TALStrokeBrush)
  private
    FParent: TALStrokeBrush; // 8 bytes
    FInherit: Boolean; // 1 byte
    fSuperseded: Boolean; // 1 byte
    procedure SetInherit(const AValue: Boolean);
  protected
    function CreateSavedState: TALPersistentObserver; override;
    procedure DoSupersede; virtual;
  public
    constructor Create(const AParent: TALStrokeBrush); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede(Const ASaveState: Boolean = False); virtual;
    procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
    property Superseded: Boolean read FSuperseded;
    property Parent: TALStrokeBrush read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  // https://m3.material.io/foundations/interaction/states/state-layers
  // A state layer is a semi-transparent covering on an element that indicates
  // its state. State layers provide a systematic approach to visualizing states
  // by using opacity. A layer can be applied to an entire element or in a
  // circular shape and only one state layer can be applied at a given time.
  TALStateLayer = class(TALPersistentObserver)
  private
    FOpacity: Single; // 4 bytes
    FColor: TAlphaColor; // 4 bytes
    FUseContentColor: Boolean; // 1 byte
    FMargins: TALBounds; // 8 bytes
    FXRadius: Single; // 4 bytes
    FYRadius: Single; // 4 bytes
    procedure SetOpacity(const Value: Single);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetUseContentColor(const Value: Boolean);
    procedure SetMargins(const Value: TALBounds);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    procedure MarginsChanged(Sender: TObject); virtual;
    function IsOpacityStored: Boolean;
    function IsColorStored: Boolean;
    function IsUseContentColorStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  protected
    function CreateMargins: TALBounds; virtual;
    function GetDefaultOpacity: Single; virtual;
    function GetDefaultColor: TAlphaColor; virtual;
    function GetDefaultUseContentColor: Boolean; virtual;
    function GetDefaultXRadius: Single; virtual;
    function GetDefaultYRadius: Single; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure AlignToPixel; virtual;
    procedure Interpolate(const ATo: TALStateLayer; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALStateLayer; const ANormalizedTime: Single);
    function HasFill: boolean; virtual;
    property DefaultOpacity: Single read GetDefaultOpacity;
    property DefaultColor: TAlphaColor read GetDefaultColor;
    property DefaultUseContentColor: Boolean read GetDefaultUseContentColor;
    property DefaultXRadius: Single read GetDefaultXRadius;
    property DefaultYRadius: Single read GetDefaultYRadius;
  published
    property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
    /// <summary>
    ///   When UseContentColor is true, the color property is ignored, and the
    ///   color is derived from the component content, such as the color of a
    ///   label's text.
    /// </summary>
    property UseContentColor: Boolean read FUseContentColor write SetUseContentColor stored IsUseContentColorStored;
    property Margins: TALBounds read FMargins write SetMargins;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALStateTransition = class(TALPersistentObserver)
  private
    FAnimationType: TAnimationType; // 4 bytes (because FMX.Types use {$MINENUMSIZE 4})
    FDuration: Single; // 4 bytes
    FInterpolation: TALInterpolationType; // 1 byte
    FDelayClick: Boolean; // 1 byte
    procedure SetAnimationType(const Value: TAnimationType);
    procedure SetDuration(const Value: Single);
    procedure SetInterpolation(const Value: TALInterpolationType);
    procedure SetDelayClick(const Value: Boolean);
    function IsAnimationTypeStored: Boolean;
    function IsDurationStored: Boolean;
    function IsInterpolationStored: Boolean;
    function IsDelayClickStored: Boolean;
  protected
    function GetDefaultAnimationType: TAnimationType; virtual;
    function GetDefaultDuration: Single; virtual;
    function GetDefaultInterpolation: TALInterpolationType; virtual;
    function GetDefaultDelayClick: Boolean; virtual;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    property DefaultAnimationType: TAnimationType read GetDefaultAnimationType;
    property DefaultDuration: Single read GetDefaultDuration;
    property DefaultInterpolation: TALInterpolationType read GetDefaultInterpolation;
    property DefaultDelayClick: Boolean read GetDefaultDelayClick;
  published
    property AnimationType: TAnimationType read FAnimationType write SetAnimationType stored IsAnimationTypeStored;
    property Duration: Single read FDuration write SetDuration stored IsDurationStored nodefault;
    property Interpolation: TALInterpolationType read FInterpolation write SetInterpolation stored IsInterpolationStored;
    property DelayClick: Boolean read FDelayClick write SetDelayClick stored IsDelayClickStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseStateStyle = class(TALPersistentObserver)
  private
    FParent: Tobject; // 8 bytes
    FStateStyleParent: TALBaseStateStyle; // 8 bytes
    FControlParent: TALControl; // 8 bytes
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
    procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
    procedure Supersede(Const ASaveState: Boolean = False); virtual;
    procedure SupersedeNoChanges(Const ASaveState: Boolean = False);
    property Superseded: Boolean read FSuperseded;
    property Parent: TObject read FParent;
    property StateStyleParent: TALBaseStateStyle read FStateStyleParent;
    property ControlParent: TALControl read FControlParent;
    property DefaultScale: Single read GetDefaultScale;
    property CacheSubIndex: integer read GetCacheSubIndex;
  end;

  // ---------------------------------------------------------------------------------------------------------------------------------- //
  // CHECKBOX      |    BUTTON      |    TOGGLEBUTTON    |    EDIT        |   SWITCH           |    TRACKBAR         |    RANGETRACKBAR //
  // --------------|----------------|--------------------|----------------|--------------------|---------------------|----------------- //
  // Checked       |    Disabled    |    Checked         |    Disabled    |   Track            |    ActiveTrack      |    ActiveTrack   //
  //   Default     |    Hovered     |      Default       |    Hovered     |     Checked        |      Disabled       |      Disabled    //
  //   Disabled    |    Pressed     |      Disabled      |    Focused     |       default      |    InactiveTrack    |    InactiveTrack //
  //   Hovered     |    Focused     |      Hovered       |                |       Disabled     |      Disabled       |      Disabled    //
  //   Pressed     |    *dragged    |      Pressed       |                |       Hovered      |    Thumb            |    MinThumb      //
  //   Focused     |                |      Focused       |                |       Pressed      |      Disabled       |      Disabled    //
  // UnChecked     |                |    UnChecked       |                |       Focused      |      Hovered        |      Hovered     //
  //   Default     |                |      Default       |                |     UnChecked      |      Pressed        |      Pressed     //
  //   Disabled    |                |      Disabled      |                |       default      |      Focused        |      Focused     //
  //   Hovered     |                |      Hovered       |                |       Disabled     |                     |    MaxThumb      //
  //   Pressed     |                |      Pressed       |                |       Hovered      |                     |      Disabled    //
  //   Focused     |                |      Focused       |                |       Pressed      |                     |      Hovered     //
  //                                                                      |       Focused      |                     |      Pressed     //
  //                                                                      |   Thumb            |                     |      Focused     //
  //                                                                      |     Checked        |                                        //
  //                                                                      |       Default      |                                        //
  //                                                                      |       Disabled     |                                        //
  //                                                                      |       Hovered      |                                        //
  //                                                                      |       Pressed      |                                        //
  //                                                                      |       Focused      |                                        //
  //                                                                      |     UnChecked      |                                        //
  //                                                                      |       Default      |                                        //
  //                                                                      |       Disabled     |                                        //
  //                                                                      |       Hovered      |                                        //
  //                                                                      |       Pressed      |                                        //
  //                                                                      |       Focused      |                                        //
  //------------------------------------------------------------------------------------------------------------------------------------//

  {***********************************************}
  TALBaseStateStyles = class(TALPersistentObserver)
  private
    FParent: TALControl; // 8 bytes
    FTransition: TALStateTransition; // 8 bytes
    FTransitionAnimation: TALfloatAnimation; // 8 bytes
    FTransitionFrom: TALBaseStateStyle; // 8 bytes
    FTransitionTo: TALBaseStateStyle; // 8 bytes
    FTransitionClickDelayed: Boolean; // 1 byte
    FLastPaintedRawStyle: TALBaseStateStyle; // 8 bytes
    FCurrentAdjustedStyle: TALBaseStateStyle; // 8 bytes
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
    constructor Create(const AParent: TALControl); reintroduce; virtual;
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
    function GetCurrentRawStyle: TALBaseStateStyle; virtual;
    /// <summary>
    ///   Determines and returns the current state style of the control,
    ///   applying any necessary adjustments and handling state transition
    ///   animations. This function provides the fully adjusted style that
    ///   reflects the control's current visual appearance, including
    ///   interpolations during transitions.
    /// </summary>
    function GetCurrentAdjustedStyle: TALBaseStateStyle; virtual;
    function IsTransitionAnimationRunning: Boolean; virtual;
    property TransitionFrom: TALBaseStateStyle read FTransitionFrom;
    property TransitionTo: TALBaseStateStyle read FTransitionTo;
    procedure UpdateLastPaintedRawStyle; virtual;
    Property Parent: TALControl read FParent;
  end;

  {~~~~~~~~~~~~~~~~~~~~~}
  TALFontMetrics = record
    /// <summary> The recommended distance above the baseline for singled spaced text </summary>
    Ascent: Single;
    /// <summary> The recommended distance below the baseline for singled spaced text </summary>
    Descent: Single;
    /// <summary> The recommended additional space to add between lines of text </summary>
    Leading: Single;
  end;

  {~~~~~~~~~~~~~~~~~~~~}
  TALFontManager = class
  {$IF (not defined(ALSkiaEngine)) and (defined(Android))}
  private
    class var FCustomTypeFaces: TDictionary<String, JTypeFace>;
  public
    class function GetCustomTypeFace(const AFamilyName: string): JTypeFace; static;
  {$ENDIF}
  public
    class procedure RegisterTypefaceFromResource(const AResourceName: string; const AFamilyName: string); static;
  end;

type
  TALCustomConvertFontFamilyProc = function(const AFontFamily: TFontName): TFontName;
  TALCustomGetResourceFilenameProc = function(const AResourceName: String): String;

var
  ALCustomConvertFontFamilyProc: TALCustomConvertFontFamilyProc;
  ALCustomGetResourceFilenameProc: TALCustomGetResourceFilenameProc;

{*********************************************************************}
function  ALConvertFontFamily(const AFontFamily: TFontName): TFontName;
function  ALExtractPrimaryFontFamily(const AFontFamilies: String): String;
{$IF defined(ALSkiaEngine)}
Function ALGetSkFontStyle(
           const AFontWeight: TFontWeight;
           const AFontSlant: TFontSlant;
           const AFontStretch: TFontStretch): sk_fontstyle_t;
{$ENDIF}
function  ALGetFontMetrics(
            const AFontFamily: String;
            const AFontSize: single;
            const AFontWeight: TFontWeight;
            const AFontSlant: TFontSlant): TALFontMetrics;
function  ALGetResourceFilename(const AResourceName: String): String;
function  ALTranslate(const AText: string): string;
Procedure ALMakeBufDrawables(const AControl: TControl; const AEnsureDoubleBuffered: Boolean = True);
function  ALAlignEdgesToPixelRound(const Rect: TRectF; const Scale: single; const Epsilon: Single = 0): TRectF; overload;
function  ALAlignDimensionToPixelRound(const Size: TSizeF; const Scale: single; const Epsilon: Single = 0): TSizeF; overload;
function  ALAlignDimensionToPixelRound(const Rect: TRectF; const Scale: single; const Epsilon: Single = 0): TRectF; overload;
function  ALAlignDimensionToPixelRound(const Dimension: single; const Scale: single; const Epsilon: Single = 0): single; overload;
function  ALAlignDimensionToPixelCeil(const Rect: TRectF; const Scale: single; const Epsilon: Single = 0): TRectF; overload;
function  ALAlignDimensionToPixelCeil(const Dimension: single; const Scale: single; const Epsilon: Single = 0): single; overload;
function  ALAlignDimensionToPixelFloor(const Rect: TRectF; const Scale: single; const Epsilon: Single = 0): TRectF; overload;
function  ALAlignDimensionToPixelFloor(const Dimension: single; const Scale: single; const Epsilon: Single = 0): single; overload;
function  ALAlignToPixelRound(const Point: TPointF; const Matrix: TMatrix; const Scale: single; const Epsilon: Single = 0): TpointF; overload;
function  ALAlignToPixelRound(const Rect: TRectF; const Matrix: TMatrix; const Scale: single; const Epsilon: Single = 0): TRectF; overload;
function  ALTextAlignToTextHorzAlign(const ATextAlign: TTextAlign): TALTextHorzAlign;
function  ALTextAlignToTextVertAlign(const ATextAlign: TTextAlign): TALTextVertAlign;

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

Type

  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Controls.TControl still has the exact same fields and adjust the IFDEF'}
  {$ENDIF}
  TALControlAccessPrivate = class(TFmxObject)
  {$IF CompilerVersion >= 32}  // Tokyo
  public type
    TDelayedEvent = (Resize, Resized);
  {$ENDIF}
  public const
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
  System.IOutils,
  System.UIConsts,
  FMX.Utils,
  Fmx.Platform,
  {$IF defined(ALSkiaEngine)}
  FMX.Skia,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  FMX.forms,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Macapi.AppKit,
  Alcinoe.Macapi.CoreText,
  {$ENDIF}
  {$IF defined(IOS)}
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.Helpers,
  Alcinoe.iOSapi.CoreText,
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  Winapi.Windows,
  FMX.Helpers.Win,
  {$ENDIF}
  {$IF defined(ALDPK)}
  System.Hash,
  ToolsAPI,
  DesignEditors,
  {$ENDIF}
  {$IF not defined(ALDPK)}
  Alcinoe.Cipher,
  {$ENDIF}
  Alcinoe.FMX.Types3D,
  Alcinoe.FMX.Graphics,
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.Common,
  Alcinoe.files,
  Alcinoe.HTTP.Client,
  Alcinoe.stringList,
  ALcinoe.StringUtils;

{***************************************}
constructor TALPersistentObserver.Create;
begin
  inherited create;
  FUpdateCount := 0;
  FIsChanged := False;
  FOnChanged := nil;
  FSavedStates := nil;
end;

{***************************************}
destructor TALPersistentObserver.Destroy;
begin
  ALFreeAndNil(FSavedStates);
  Inherited;
end;

{*********************************************************************}
function TALPersistentObserver.CreateSavedState: TALPersistentObserver;
begin
  result := TALPersistentObserver(classtype.Create);
end;

{************************************}
procedure TALPersistentObserver.Reset;
begin
  // Virtual
end;

{******************************************}
procedure TALPersistentObserver.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

{****************************************}
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

{*************************************************}
procedure TALPersistentObserver.EndUpdateNoChanges;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and (FIsChanged) then
      // If execution reaches this point, it means there was no previously unclosed
      // beginUpdate since FUpdateCount is 0. Therefore, ignoring the doChanged
      // call here will not affect any prior beginUpdate operations.
      FIsChanged := False;
  end;
end;

{****************************************}
procedure TALPersistentObserver.SaveState;
begin
  if FSavedStates = nil then
    FSavedStates := TObjectQueue<TALPersistentObserver>.Create(True{AOwnsObjects});
  var LSavedState := CreateSavedState;
  if LSavedState.classtype <> classtype then
    Raise Exception.create('The saved state type returned by "CreateSavedState" does not match the current object type');
  LSavedState.Assign(self);
  FSavedStates.Enqueue(LSavedState);
end;

{*******************************************}
procedure TALPersistentObserver.RestoreState;
begin
  if (FSavedStates = nil) or
     (FSavedStates.Count = 0) then
    raise Exception.Create('No saved state available');
  var LSavedState := FSavedStates.extract;
  try
    Assign(LSavedState);
  finally
    ALFreeAndNil(LSavedState);
  end;
end;

{****************************************************}
procedure TALPersistentObserver.RestoreStateNoChanges;
begin
  BeginUpdate;
  try
    RestoreState;
  finally
    EndUpdateNoChanges;
  end;
end;

{****************************************}
procedure TALPersistentObserver.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

{*************************************}
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
constructor TALBounds.Create;
begin
  inherited Create;
  var LDefaultValue := DefaultValue;
  FLeft := LDefaultValue.Left;
  FTop := LDefaultValue.Top;
  FRight := LDefaultValue.Right;
  FBottom := LDefaultValue.Bottom;
end;

{**********************************************}
procedure TALBounds.Assign(Source: TPersistent);
begin
  if Source is TALBounds then
    Rect := TALBounds(Source).Rect
  else if Source is TBounds then
    Rect := TBounds(Source).Rect
  else if Source = nil then
    Rect := DefaultValue
  else
    inherited
end;

{*****************************************}
function TALBounds.GetDefaultValue: TRectF;
begin
  Result := TRectF.Empty;
end;

{*********************************}
function TALBounds.GetRect: TRectF;
begin
  Result := TRectF.Create(FLeft, FTop, FRight, FBottom);
end;

{***********************************************}
procedure TALBounds.SetRect(const Value: TRectF);
begin
  if Rect <> Value then
  begin
    FLeft := Value.Left;
    FTop := Value.Top;
    FRight := Value.Right;
    FBottom := Value.Bottom;
    DoChange;
  end;
end;

{******************************************************}
function TALBounds.PaddingRect(const R: TRectF): TRectF;
begin
  Result := TRectF.Create(R.Left + FLeft, R.Top + FTop, R.Right - FRight, R.Bottom - FBottom);
end;

{*****************************************************}
function TALBounds.MarginRect(const R: TRectF): TRectF;
begin
  Result := TRectF.Create(R.Left - FLeft, R.Top - FTop, R.Right + FRight, R.Bottom + FBottom);
end;

{*******************************}
function TALBounds.Width: Single;
begin
  Result := Rect.Width;
end;

{********************************}
function TALBounds.Height: Single;
begin
  Result := Rect.Height;
end;

{*****************************************}
function TALBounds.IsBottomStored: Boolean;
begin
  Result := not SameValue(FBottom, DefaultValue.Bottom, Epsilon);
end;

{***************************************}
function TALBounds.IsLeftStored: Boolean;
begin
  Result := not SameValue(FLeft, DefaultValue.Left, Epsilon);
end;

{****************************************}
function TALBounds.IsRightStored: Boolean;
begin
  Result := not SameValue(FRight, DefaultValue.Right, Epsilon);
end;

{**************************************}
function TALBounds.IsTopStored: Boolean;
begin
  Result := not SameValue(FTop, DefaultValue.Top, Epsilon);
end;

{**************************************}
function TALBounds.MarginEmpty: Boolean;
begin
  Result := SameValue(FLeft, 0, Epsilon) and
            SameValue(FTop, 0, Epsilon) and
            SameValue(FRight, 0, Epsilon) and
            SameValue(FBottom, 0, Epsilon);
end;

{********************************}
function TALBounds.Empty: Boolean;
begin
  Result := System.Types.IsRectEmpty(Rect)
end;

{***********************************************}
function TALBounds.Equals(Obj: TObject): Boolean;
begin
  if (Obj is TALBounds) then
    Result := TALBounds(Obj).Rect = Rect
  else if (Obj is TBounds) then
    Result := TBounds(Obj).Rect = Rect
  else
    Result := inherited;
end;

{*************************************************}
procedure TALBounds.SetBottom(const Value: Single);
begin
  if not SameValue(FBottom, Value, Epsilon) then
  begin
    FBottom := Value;
    DoChange;
  end;
end;

{***********************************************}
procedure TALBounds.SetLeft(const Value: Single);
begin
  if not SameValue(FLeft, Value, Epsilon) then
  begin
    FLeft := Value;
    DoChange;
  end;
end;

{************************************************}
procedure TALBounds.SetRight(const Value: Single);
begin
  if not SameValue(FRight, Value, Epsilon) then
  begin
    FRight := Value;
    DoChange;
  end;
end;

{**********************************************}
procedure TALBounds.SetTop(const Value: Single);
begin
  if not SameValue(FTop, Value, Epsilon) then
  begin
    FTop := Value;
    DoChange;
  end;
end;

{**********************************}
function TALBounds.ToString: string;
begin
  Result := Format('%s (%4.2f,%4.2f)-(%4.2f,%4.2f)', [inherited ToString, FLeft, FTop, FRight, FBottom]);
end;

{***************************}
procedure TALBounds.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

{*****************************}
constructor TALPosition.Create;
begin
  inherited Create;
  var LDefaultValue := DefaultValue;
  FX := LDefaultValue.X;
  FY := LDefaultValue.Y;
end;

{************************************************}
procedure TALPosition.Assign(Source: TPersistent);
begin
  if Source is TALPosition then
    Point := TALPosition(Source).Point
  else if Source is TPosition then
    Point := TPosition(Source).Point
  else
    inherited
end;

{*******************************************************}
procedure TALPosition.SetPointNoChange(const P: TPointF);
begin
  FX := P.X;
  FY := P.Y;
end;

{**********************************}
function TALPosition.Empty: Boolean;
begin
  Result := Point.IsZero;
end;

{*****************************}
procedure TALPosition.DoChange;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

{**************************************}
function TALPosition.IsXStored: Boolean;
begin
  Result := not SameValue(FX, DefaultValue.X, Epsilon);
end;

{**************************************}
function TALPosition.IsYStored: Boolean;
begin
  Result := not SameValue(FY, DefaultValue.Y, Epsilon);
end;

{***************************************************}
procedure TALPosition.Reflect(const Normal: TPointF);
begin
  Point := Point.Reflect(Normal);
end;

{********************************************}
function TALPosition.GetDefaultValue: TPointF;
begin
  Result := TpointF.Zero;
end;

{*************************************}
function TALPosition.GetPoint: TPointF;
begin
  Result := TPointF.Create(FX, FY);
end;

{***************************************************}
procedure TALPosition.SetPoint(const Value: TPointF);
begin
  var LChange := not (SameValue(FX, Value.X, Epsilon) and SameValue(FY, Value.Y, Epsilon));
  FX := Value.X;
  FY := Value.Y;
  if LChange then
    DoChange;
end;

{**********************************************}
procedure TALPosition.SetX(const Value: Single);
begin
  var LChange := not SameValue(FX, Value, Epsilon);
  FX := Value;
  if LChange then
    DoChange;
end;

{**********************************************}
procedure TALPosition.SetY(const Value: Single);
begin
  var LChange := not SameValue(FY, Value, Epsilon);
  FY := Value;
  if LChange then
    DoChange;
end;

{***************************}
constructor TALShadow.Create;
begin
  inherited Create;
  Fblur := DefaultBlur;
  FOffsetX := DefaultOffsetX;
  FOffsetY := DefaultOffsetY;
  FColor := DefaultColor;
end;

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALShadow.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Enabled', ReadEnabled{ReadData}, nil{WriteData}, false{hasdata});
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALShadow.ReadEnabled(Reader: TReader);
begin
  if not Reader.ReadBoolean then Color := TAlphaColors.Null;
end;
{$ENDIF}

{**********************************************}
procedure TALShadow.Assign(Source: TPersistent);
begin
  if Source is TALShadow then begin
    BeginUpdate;
    Try
      Blur    := TALShadow(Source).Blur;
      OffsetX := TALShadow(Source).OffsetX;
      OffsetY := TALShadow(Source).OffsetY;
      Color   := TALShadow(Source).Color;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{************************}
procedure TALShadow.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    blur := DefaultBlur;
    OffsetX := DefaultOffsetX;
    OffsetY := DefaultOffsetY;
    Color := DefaultColor;
  finally
    EndUpdate;
  end;
end;

{*******************************}
procedure TALShadow.AlignToPixel;
begin
  BeginUpdate;
  try
    blur := ALAlignDimensionToPixelRound(blur, ALGetScreenScale, Tepsilon.Vector);
    OffsetX := ALAlignDimensionToPixelRound(OffsetX, ALGetScreenScale, TEpsilon.Position);
    OffsetY := ALAlignDimensionToPixelRound(OffsetY, ALGetScreenScale, TEpsilon.Position);
  finally
    EndUpdate;
  end;
end;

{***********************************************************************************}
procedure TALShadow.Interpolate(const ATo: TALShadow; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      blur := InterpolateSingle(blur{Start}, ATo.blur{Stop}, ANormalizedTime);
      OffsetX := InterpolateSingle(OffsetX{Start}, ATo.OffsetX{Stop}, ANormalizedTime);
      OffsetY := InterpolateSingle(OffsetY{Start}, ATo.OffsetY{Stop}, ANormalizedTime);
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
    end
    else begin
      blur := InterpolateSingle(blur{Start}, Defaultblur{Stop}, ANormalizedTime);
      OffsetX := InterpolateSingle(OffsetX{Start}, DefaultOffsetX{Stop}, ANormalizedTime);
      OffsetY := InterpolateSingle(OffsetY{Start}, DefaultOffsetY{Stop}, ANormalizedTime);
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{********************************************************************************************}
procedure TALShadow.InterpolateNoChanges(const ATo: TALShadow; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{************************************}
function TALShadow.HasShadow: boolean;
begin
  result := (Color <> TalphaColors.Null) and
            (CompareValue(Blur, 0, Tepsilon.vector) > 0);
end;

{***************************************}
function TALShadow.IsblurStored: Boolean;
begin
  result := not SameValue(fBlur, DefaultBlur, Tepsilon.vector);
end;

{******************************************}
function TALShadow.IsOffsetXStored: Boolean;
begin
  result := not SameValue(fOffsetX, DefaultOffsetX, Tepsilon.Position);
end;

{******************************************}
function TALShadow.IsOffsetYStored: Boolean;
begin
  result := not SameValue(fOffsetY, DefaultOffsetY, Tepsilon.Position);
end;

{****************************************}
function TALShadow.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{****************************************}
function TALShadow.GetDefaultblur: Single;
begin
  Result := 0; // 12
end;

{*******************************************}
function TALShadow.GetDefaultOffsetX: Single;
begin
  Result := 0;
end;

{*******************************************}
function TALShadow.GetDefaultOffsetY: Single;
begin
  Result := 0;
end;

{**********************************************}
function TALShadow.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.null; // $96000000;
end;

{***********************************************}
procedure TALShadow.setblur(const Value: Single);
begin
  if not SameValue(fBlur, Value, Tepsilon.vector) then begin
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

{************************************************************}
constructor TALInheritShadow.Create(const AParent: TALShadow);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{****************************************************************}
function TALInheritShadow.CreateSavedState: TALPersistentObserver;
type
  TALInheritShadowClass = class of TALInheritShadow;
begin
  result := TALInheritShadowClass(classtype).Create(nil{AParent});
end;

{***********************************************************}
procedure TALInheritShadow.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{*****************************************************}
procedure TALInheritShadow.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALInheritShadow then begin
      Inherit := TALInheritShadow(Source).Inherit;
      fSuperseded := TALInheritShadow(Source).fSuperseded;
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

{*******************************}
procedure TALInheritShadow.Reset;
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

{*************************************}
procedure TALInheritShadow.DoSupersede;
begin
  Assign(FParent);
end;

{**********************************************************************}
procedure TALInheritShadow.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  BeginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TALInheritShadow then begin
      TALInheritShadow(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TALInheritShadow(FParent).restoreStateNoChanges;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{*******************************************************************************}
procedure TALInheritShadow.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{*************************}
constructor TALFont.Create;
begin
  inherited Create;
  FFamily := DefaultFamily;
  FSize := DefaultSize;
  FWeight := DefaultWeight;
  FSlant := DefaultSlant;
  FStretch := DefaultStretch;
  FColor := DefaultColor;
end;

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALFont.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('StyleExt', ReadStyleExt, nil{WriteData}, false{hasdata});
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALFont.ReadStyleExt(AStream: TStream);
begin
  var LFontStyles: TFontStyles; // fsBold, fsItalic, fsUnderline, fsStrikeOut
  AStream.Read(LFontStyles, SizeOf(TFontStyles));
  AStream.Read(FWeight, SizeOf(TFontWeight)); // fsBold
  AStream.Read(FSlant, SizeOf(TFontSlant)); // fsItalic
  AStream.Read(FStretch, SizeOf(TFontStretch));
end;
{$ENDIF}

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
    ALAssignError(Self{ASource}, Dest{ADest});
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
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************}
procedure TALFont.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Family := DefaultFamily;
    Size := DefaultSize;
    Weight := DefaultWeight;
    Slant := DefaultSlant;
    Stretch := DefaultStretch;
    Color := DefaultColor;
  finally
    EndUpdate;
  end;
end;

{*****************************}
procedure TALFont.AlignToPixel;
begin
  BeginUpdate;
  try
    // I'm not sure if doing this will impact anything
    Size := ALAlignDimensionToPixelRound(Size, ALGetScreenScale, TEpsilon.FontSize);
  finally
    EndUpdate;
  end;
end;

{*******************************************************************************}
procedure TALFont.Interpolate(const ATo: TALFont; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Family := ATo.Family;
      Size := InterpolateSingle(Size{Start}, ATo.Size{Stop}, ANormalizedTime);
      //TFontWeight = (Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack)
      Weight := TFontWeight(round(InterpolateSingle(integer(Weight), integer(ATo.Weight), ANormalizedTime)));
      Slant := ATo.Slant;
      //TFontStretch = (UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, Regular, SemiExpanded, Expanded, ExtraExpanded, UltraExpanded)
      Stretch := TFontStretch(round(InterpolateSingle(integer(Stretch), integer(ATo.Stretch), ANormalizedTime)));
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
    end
    else begin
      Family := ALConvertFontFamily(DefaultFamily);
      Size := InterpolateSingle(Size{Start}, DefaultSize{Stop}, ANormalizedTime);
      //TFontWeight = (Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack)
      Weight := TFontWeight(round(InterpolateSingle(integer(Weight), integer(DefaultWeight), ANormalizedTime)));
      Slant := DefaultSlant;
      //TFontStretch = (UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, Regular, SemiExpanded, Expanded, ExtraExpanded, UltraExpanded)
      Stretch := TFontStretch(round(InterpolateSingle(integer(Stretch), integer(DefaultStretch), ANormalizedTime)));
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{****************************************************************************************}
procedure TALFont.InterpolateNoChanges(const ATo: TALFont; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{***************************************}
function TALFont.IsFamilyStored: Boolean;
begin
  Result := FFamily <> DefaultFamily;
end;

{*************************************}
function TALFont.IsSizeStored: Boolean;
begin
  Result := not SameValue(FSize, DefaultSize, TEpsilon.FontSize);
end;

{***************************************}
function TALFont.IsWeightStored: Boolean;
begin
  result := FWeight <> DefaultWeight;
end;

{**************************************}
function TALFont.IsSlantStored: Boolean;
begin
  result := FSlant <> DefaultSlant;
end;

{****************************************}
function TALFont.IsStretchStored: Boolean;
begin
  result := FStretch <> DefaultStretch;
end;

{**************************************}
function TALFont.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{*******************************************}
function TALFont.GetDefaultFamily: TFontName;
begin
  result := SansSerifFamily;
end;

{**************************************}
function TALFont.GetDefaultSize: Single;
begin
  result := 14;
end;

{*********************************************}
function TALFont.GetDefaultWeight: TFontWeight;
begin
  result := TFontWeight.Regular;
end;

{*******************************************}
function TALFont.GetDefaultSlant: TFontSlant;
begin
  result := TFontSlant.Regular;
end;

{***********************************************}
function TALFont.GetDefaultStretch: TFontStretch;
begin
  result := TFontStretch.Regular;
end;

{********************************************}
function TALFont.GetDefaultColor: TAlphaColor;
begin
  result := TAlphaColorRec.Black;
end;

{***************************************************}
procedure TALFont.SetFamily(const AValue: TFontName);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
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

{***********************************}
constructor TALTextDecoration.Create;
begin
  inherited Create;
  FKinds := DefaultKinds;
  FStyle := DefaultStyle;
  FThicknessMultiplier := DefaultThicknessMultiplier;
  FColor := DefaultColor;
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
    ALAssignError(Source{ASource}, Self{ADest});
end;

{********************************}
procedure TALTextDecoration.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Kinds := DefaultKinds;
    Style := DefaultStyle;
    ThicknessMultiplier := DefaultThicknessMultiplier;
    Color := DefaultColor;
  finally
    EndUpdate;
  end;
end;

{***************************************************************************************************}
procedure TALTextDecoration.Interpolate(const ATo: TALTextDecoration; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Kinds := ATo.Kinds;
      Style := ATo.Style;
      ThicknessMultiplier := InterpolateSingle(ThicknessMultiplier{Start}, ATo.ThicknessMultiplier{Stop}, ANormalizedTime);
      if (ATo.Color = TalphaColors.Null) or
         (Color = TalphaColors.Null) then
        Color := ATo.Color
      else
        Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
    end
    else begin
      Kinds := DefaultKinds;
      Style := DefaultStyle;
      ThicknessMultiplier := InterpolateSingle(ThicknessMultiplier{Start}, DefaultThicknessMultiplier{Stop}, ANormalizedTime);
      if (DefaultColor = TalphaColors.Null) or
         (Color = TalphaColors.Null) then
        Color := DefaultColor
      else
        Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************************************}
procedure TALTextDecoration.InterpolateNoChanges(const ATo: TALTextDecoration; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{************************************************}
function TALTextDecoration.IsKindsStored: Boolean;
begin
  result := FKinds <> DefaultKinds
end;

{************************************************}
function TALTextDecoration.IsStyleStored: Boolean;
begin
  result := FStyle <> DefaultStyle
end;

{**************************************************************}
function TALTextDecoration.IsThicknessMultiplierStored: Boolean;
begin
  Result := not SameValue(FThicknessMultiplier, DefaultThicknessMultiplier, TEpsilon.Scale);
end;

{************************************************}
function TALTextDecoration.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor
end;

{*****************************************************************}
function TALTextDecoration.GetDefaultKinds: TALTextDecorationKinds;
begin
  Result := [];
end;

{*****************************************************************}
function TALTextDecoration.GetDefaultStyle: TALTextDecorationStyle;
begin
  Result := TALTextDecorationStyle.Solid;
end;

{***************************************************************}
function TALTextDecoration.GetDefaultThicknessMultiplier: Single;
begin
  Result := 1;
end;

{******************************************************}
function TALTextDecoration.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
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
  if not SameValue(FThicknessMultiplier, AValue, TEpsilon.Scale) then begin
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
  FInherit := DefaultInherit;
  FFont := CreateFont;
  FFont.OnChanged := FontChanged;
  FDecoration := CreateDecoration;
  FDecoration.OnChanged := DecorationChanged;
end;

{*************************************}
destructor TALEllipsisSettings.Destroy;
begin
  ALFreeAndNil(FFont);
  ALFreeAndNil(FDecoration);
  inherited Destroy;
end;

{***********************************************}
function TALEllipsisSettings.CreateFont: TALFont;
begin
  Result := TALFont.Create;
end;

{***************************************************************}
function TALEllipsisSettings.CreateDecoration: TALTextDecoration;
begin
  Result := TALTextDecoration.Create;
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
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************}
procedure TALEllipsisSettings.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Inherit := DefaultInherit;
    Font.reset;
    Decoration.reset;
  finally
    EndUpdate;
  end;
end;

{*****************************************}
procedure TALEllipsisSettings.AlignToPixel;
begin
  BeginUpdate;
  try
    // I'm not sure if doing this will impact anything
    Font.AlignToPixel;
  finally
    EndUpdate;
  end;
end;

{*******************************************************************************************************}
procedure TALEllipsisSettings.Interpolate(const ATo: TALEllipsisSettings; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      if ATo.Inherit or Inherit then begin
        Font.assign(ATo.Font);
        Decoration.assign(ATo.Decoration);
        Inherit := ATo.Inherit; // True or False
      end
      else begin
        Font.Interpolate(ATo.Font, ANormalizedTime);
        Decoration.Interpolate(ATo.Decoration, ANormalizedTime);
        Inherit := ATo.Inherit; // False
      end;
    end
    else begin
      if DefaultInherit or Inherit then begin
        Font.Reset;
        Decoration.reset;
        Inherit := DefaultInherit; // True or False
      end
      else begin
        Font.Interpolate(nil, ANormalizedTime);
        Decoration.Interpolate(nil, ANormalizedTime);
        Inherit := DefaultInherit; // False
      end;
    end;
  finally
    EndUpdate;
  end;
end;

{****************************************************************************************************************}
procedure TALEllipsisSettings.InterpolateNoChanges(const ATo: TALEllipsisSettings; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
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

{****************************************************}
function TALEllipsisSettings.IsInheritStored: Boolean;
begin
  Result := FInherit <> DefaultInherit;
end;

{******************************************************}
function TALEllipsisSettings.GetDefaultInherit: Boolean;
begin
  Result := True;
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
  FFont := CreateFont;
  FFont.OnChanged := FontChanged;
  FDecoration := CreateDecoration;
  FDecoration.OnChanged := DecorationChanged;
  FEllipsis := DefaultEllipsis;
  FEllipsisSettings := CreateEllipsisSettings;
  FEllipsisSettings.OnChanged := EllipsisSettingsChanged;
  FMaxLines := DefaultMaxLines;
  FIsHtml := DefaultIsHtml;
  FTrimming := DefaultTrimming;
  FHorzAlign := DefaultHorzAlign;
  FVertAlign := DefaultVertAlign;
  FLineHeightMultiplier := DefaultLineHeightMultiplier;
  FLetterSpacing := DefaultLetterSpacing;
end;

{*************************************}
destructor TALBaseTextSettings.Destroy;
begin
  ALFreeAndNil(FFont);
  ALFreeAndNil(FDecoration);
  ALFreeAndNil(FEllipsisSettings);
  inherited Destroy;
end;

{***********************************************}
function TALBaseTextSettings.CreateFont: TALFont;
begin
  Result := TALFont.Create;
end;

{***************************************************************}
function TALBaseTextSettings.CreateDecoration: TALTextDecoration;
begin
  Result := TALTextDecoration.Create;
end;

{***********************************************************************}
function TALBaseTextSettings.CreateEllipsisSettings: TALEllipsisSettings;
begin
  Result := TALEllipsisSettings.create;
end;

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALBaseTextSettings.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('FontColor', ReadFontColor{ReadData}, nil{WriteData}, false{hasdata});
  Filer.DefineProperty('WordWrap', ReadWordWrap{ReadData}, nil{WriteData}, false{hasdata});
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALBaseTextSettings.ReadFontColor(Reader: TReader);
begin
  Var LColor: Integer;
  if IdentToAlphaColor(Reader.ReadIdent, Lcolor) then Font.Color := LColor;
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALBaseTextSettings.ReadWordWrap(Reader: TReader);
begin
  if not Reader.ReadBoolean then MaxLines := 1;
end;
{$ENDIF}

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
    ALAssignError(Self{ASource}, Dest{ADest});
end;

{********************************************************}
procedure TALBaseTextSettings.Assign(Source: TPersistent);
begin
  if Source is TALBaseTextSettings then begin
    BeginUpdate;
    Try
      Font.Assign(TALBaseTextSettings(Source).Font);
      Decoration.Assign(TALBaseTextSettings(Source).Decoration);
      Ellipsis             := TALBaseTextSettings(Source).Ellipsis;
      EllipsisSettings.Assign(TALBaseTextSettings(Source).EllipsisSettings);
      MaxLines             := TALBaseTextSettings(Source).MaxLines;
      IsHtml               := TALBaseTextSettings(Source).IsHtml;
      Trimming             := TALBaseTextSettings(Source).Trimming;
      HorzAlign            := TALBaseTextSettings(Source).HorzAlign;
      VertAlign            := TALBaseTextSettings(Source).VertAlign;
      LineHeightMultiplier := TALBaseTextSettings(Source).LineHeightMultiplier;
      LetterSpacing        := TALBaseTextSettings(Source).LetterSpacing;
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
      Ellipsis := DefaultEllipsis;
      EllipsisSettings.reset;
      if not TTextSettings(Source).WordWrap then MaxLines := 1
      else MaxLines := DefaultMaxLines;
      IsHtml := DefaultIsHtml;
      case TTextSettings(Source).Trimming of
        TTextTrimming.None:      Trimming := DefaultTrimming;
        TTextTrimming.Character: Trimming := TALTextTrimming.Character;
        TTextTrimming.Word:      Trimming := TALTextTrimming.Word;
        else raise Exception.Create('Error FAFCFC8A-6C5F-464C-B2F3-0283C0D6072E');
      end;
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
      LetterSpacing := DefaultLetterSpacing;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************}
procedure TALBaseTextSettings.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Font.reset;
    Decoration.reset;
    Ellipsis := DefaultEllipsis;
    EllipsisSettings.reset;
    MaxLines := DefaultMaxLines;
    IsHtml := DefaultIsHtml;
    Trimming := DefaultTrimming;
    HorzAlign := DefaultHorzAlign;
    VertAlign := DefaultVertAlign;
    LineHeightMultiplier := DefaultLineHeightMultiplier;
    LetterSpacing := DefaultLetterSpacing;
  finally
    EndUpdate;
  end;
end;

{*****************************************}
procedure TALBaseTextSettings.AlignToPixel;
begin
  BeginUpdate;
  try
    // I'm not sure if doing this will impact anything
    Font.AlignToPixel;
    EllipsisSettings.AlignToPixel;
    LetterSpacing := ALAlignDimensionToPixelRound(LetterSpacing, ALGetScreenScale, TEpsilon.FontSize);
  finally
    EndUpdate;
  end;
end;

{*******************************************************************************************************}
procedure TALBaseTextSettings.Interpolate(const ATo: TALBaseTextSettings; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Font.Interpolate(ATo.Font, ANormalizedTime);
      Decoration.Interpolate(ATo.Decoration, ANormalizedTime);
      Ellipsis := ATo.Ellipsis;
      EllipsisSettings.Interpolate(ATo.EllipsisSettings, ANormalizedTime);
      MaxLines := ATo.MaxLines;
      IsHtml := ATo.IsHtml;
      Trimming := ATo.Trimming;
      HorzAlign := ATo.HorzAlign;
      VertAlign := ATo.VertAlign;
      LineHeightMultiplier := InterpolateSingle(LineHeightMultiplier{Start}, ATo.LineHeightMultiplier{Stop}, ANormalizedTime);
      LetterSpacing := InterpolateSingle(LetterSpacing{Start}, ATo.LetterSpacing{Stop}, ANormalizedTime);
    end
    else begin
      Font.Interpolate(nil, ANormalizedTime);
      Decoration.Interpolate(nil, ANormalizedTime);
      Ellipsis := DefaultEllipsis;
      EllipsisSettings.Interpolate(nil, ANormalizedTime);
      MaxLines := DefaultMaxLines;
      IsHtml := DefaultIsHtml;
      Trimming := DefaultTrimming;
      HorzAlign := DefaultHorzAlign;
      VertAlign := DefaultVertAlign;
      LineHeightMultiplier := InterpolateSingle(LineHeightMultiplier{Start}, DefaultLineHeightMultiplier{Stop}, ANormalizedTime);
      LetterSpacing := InterpolateSingle(LetterSpacing{Start}, DefaultLetterSpacing{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{****************************************************************************************************************}
procedure TALBaseTextSettings.InterpolateNoChanges(const ATo: TALBaseTextSettings; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
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

{**********************************************************************}
procedure TALBaseTextSettings.EllipsisSettingsChanged(ASender: TObject);
begin
  Change;
end;

{*****************************************************}
function TALBaseTextSettings.IsEllipsisStored: Boolean;
begin
  Result := FEllipsis <> DefaultEllipsis;
end;

{*****************************************************}
function TALBaseTextSettings.IsMaxLinesStored: Boolean;
begin
  Result := FMaxLines <> DefaultMaxLines;
end;

{***************************************************}
function TALBaseTextSettings.IsIsHtmlStored: Boolean;
begin
  Result := FIsHtml <> DefaultIsHtml;
end;

{*****************************************************}
function TALBaseTextSettings.IsTrimmingStored: Boolean;
begin
  Result := FTrimming <> DefaultTrimming;
end;

{******************************************************}
function TALBaseTextSettings.IsHorzAlignStored: Boolean;
begin
  Result := FHorzAlign <> DefaultHorzAlign;
end;

{******************************************************}
function TALBaseTextSettings.IsVertAlignStored: Boolean;
begin
  Result := FVertAlign <> DefaultVertAlign;
end;

{*****************************************************************}
function TALBaseTextSettings.IsLineHeightMultiplierStored: Boolean;
begin
  Result := not SameValue(FLineHeightMultiplier, DefaultLineHeightMultiplier, TEpsilon.Scale);
end;

{**********************************************************}
function TALBaseTextSettings.IsLetterSpacingStored: Boolean;
begin
  Result := not SameValue(FLetterSpacing, DefaultLetterSpacing, TEpsilon.FontSize);
end;

{******************************************************}
function TALBaseTextSettings.GetDefaultEllipsis: String;
begin
  Result := HorizontalEllipsis;
end;

{*******************************************************}
function TALBaseTextSettings.GetDefaultMaxLines: Integer;
begin
  Result := 65535;
end;

{*****************************************************}
function TALBaseTextSettings.GetDefaultIsHtml: Boolean;
begin
  Result := False;
end;

{***************************************************************}
function TALBaseTextSettings.GetDefaultTrimming: TALTextTrimming;
begin
  Result := TALTextTrimming.Word;
end;

{*****************************************************************}
function TALBaseTextSettings.GetDefaultHorzAlign: TALTextHorzAlign;
begin
  Result := TALTextHorzAlign.Leading;
end;

{*****************************************************************}
function TALBaseTextSettings.GetDefaultVertAlign: TALTextVertAlign;
begin
  Result := TALTextVertAlign.Center;
end;

{******************************************************************}
function TALBaseTextSettings.GetDefaultLineHeightMultiplier: Single;
begin
  Result := 0;
end;

{***********************************************************}
function TALBaseTextSettings.GetDefaultLetterSpacing: Single;
begin
  Result := 0;
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

{**************************************************************}
procedure TALBaseTextSettings.SetEllipsis(const AValue: String);
begin
  If FEllipsis <> AValue then begin
    FEllipsis := AValue;
    Change;
  end;
end;

{***********************************************************************************}
procedure TALBaseTextSettings.SetEllipsisSettings(const AValue: TALEllipsisSettings);
begin
  FEllipsisSettings.Assign(AValue);
end;

{***************************************************************}
procedure TALBaseTextSettings.SetMaxLines(const AValue: Integer);
begin
  If FMaxLines <> AValue then begin
    FMaxLines := AValue;
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

{***********************************************************************}
procedure TALBaseTextSettings.SetTrimming(const AValue: TALTextTrimming);
begin
  If FTrimming <> AValue then begin
    FTrimming := AValue;
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

{********************************************************************************}
constructor TALInheritBaseTextSettings.Create(const AParent: TALBaseTextSettings);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{**************************************************************************}
function TALInheritBaseTextSettings.CreateSavedState: TALPersistentObserver;
type
  TALInheritBaseTextSettingsClass = class of TALInheritBaseTextSettings;
begin
  result := TALInheritBaseTextSettingsClass(classtype).Create(nil{AParent});
end;

{*********************************************************************}
procedure TALInheritBaseTextSettings.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{***************************************************************}
procedure TALInheritBaseTextSettings.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALInheritBaseTextSettings then begin
      Inherit := TALInheritBaseTextSettings(Source).Inherit;
      fSuperseded := TALInheritBaseTextSettings(Source).fSuperseded;
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

{*****************************************}
procedure TALInheritBaseTextSettings.Reset;
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

{***********************************************}
procedure TALInheritBaseTextSettings.DoSupersede;
begin
  Assign(FParent);
end;

{********************************************************************************}
procedure TALInheritBaseTextSettings.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  BeginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TALInheritBaseTextSettings then begin
      TALInheritBaseTextSettings(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TALInheritBaseTextSettings(FParent).restoreStateNoChanges;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{*****************************************************************************************}
procedure TALInheritBaseTextSettings.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{*****************************}
constructor TALGradient.Create;
begin
  inherited Create;
  FStyle := DefaultStyle;
  FAngle := DefaultAngle;
  FColors := [];
  FOffsets := [];
end;

{************************************************}
procedure TALGradient.Assign(Source: TPersistent);
begin
  if Source is TALGradient then begin
    BeginUpdate;
    Try
      Style := TALGradient(Source).Style;
      Angle := TALGradient(Source).Angle;
      Colors := TALGradient(Source).Colors;
      Offsets := TALGradient(Source).Offsets;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**************************}
procedure TALGradient.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Style := DefaultStyle;
    Angle := DefaultAngle;
    Colors := [];
    Offsets := [];
  finally
    EndUpdate;
  end;
end;

{***************************************************************************************}
procedure TALGradient.Interpolate(const ATo: TALGradient; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      if (Style = ATo.Style) and
         (length(Colors) = length(ATo.Colors)) and
         (length(Offsets) = length(ATo.Offsets)) then begin
        Style := ATo.Style;
        Angle := InterpolateSingle(Angle{Start}, ATo.Angle{Stop}, ANormalizedTime);
        for var I := Low(Colors) to High(Colors) do
          Colors[i] := ALInterpolateColor(Colors[i]{Start}, ATo.Colors[i]{Stop}, ANormalizedTime);
        for var I := Low(Offsets) to High(Offsets) do
          Offsets[i] := InterpolateSingle(Offsets[i]{Start}, ATo.Offsets[i]{Stop}, ANormalizedTime);
      end
      else begin
        Style := ATo.Style;
        Angle := ATo.Angle;
        Colors := ATo.Colors;
        Offsets := ATo.Offsets;
      end;
    end
    else begin
      Style := DefaultStyle;
      Angle := DefaultAngle;
      Colors := [];
      Offsets := [];
    end;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************************}
procedure TALGradient.InterpolateNoChanges(const ATo: TALGradient; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{****************************************************}
procedure TALGradient.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Colors', ReadColors, WriteColors, Length(FColors) > 0);
  Filer.DefineProperty('Offsets', ReadOffsets, WriteOffsets, Length(FOffsets) > 0);
end;

{************************************************}
procedure TALGradient.ReadColors(Reader: TReader);
begin
  SetLength(FColors, 0);
  Reader.ReadListBegin;
  try
    while not Reader.EndOfList do begin
      SetLength(FColors, length(FColors)+1);
      FColors[high(FColors)] := Cardinal(Reader.ReadInt64);
    end;
  finally
    Reader.ReadListEnd;
  end;
end;

{*************************************************}
procedure TALGradient.WriteColors(Writer: TWriter);
begin
  Writer.WriteListBegin;
  try
    for var I := Low(FColors) to High(FColors) do
      Writer.WriteInteger(FColors[I]);
  finally
    Writer.WriteListEnd;
  end;
end;

{*************************************************}
procedure TALGradient.ReadOffsets(Reader: TReader);
begin
  SetLength(FOffsets, 0);
  Reader.ReadListBegin;
  try
    while not Reader.EndOfList do begin
      SetLength(FOffsets, length(FOffsets)+1);
      FOffsets[high(FOffsets)] := Reader.ReadSingle;
    end;
  finally
    Reader.ReadListEnd;
  end;
end;

{**************************************************}
procedure TALGradient.WriteOffsets(Writer: TWriter);
begin
  Writer.WriteListBegin;
  try
    for var I := Low(FOffsets) to High(FOffsets) do
      Writer.WriteSingle(FOffsets[I]);
  finally
    Writer.WriteListEnd;
  end;
end;

{******************************************}
function TALGradient.IsStyleStored: Boolean;
begin
  result := FStyle <> DefaultStyle;
end;

{******************************************}
function TALGradient.IsAngleStored: Boolean;
begin
  result := Not sameValue(FAngle, DefaultAngle, TEpsilon.Angle);
end;

{***************************************************}
function TALGradient.GetDefaultStyle: TGradientStyle;
begin
  Result := TGradientStyle.Linear;
end;

{*******************************************}
function TALGradient.GetDefaultAngle: Single;
begin
  Result := 180;
end;

{****************************************}
function TALGradient.GetCSSFormat: String;
begin
  Result := '';
  If (length(Colors) = 0) then exit;
  If (length(Colors) = 1) then begin
    {$IF defined(ALDPK)}
    Exit;
    {$ELSE}
    raise Exception.Create('A gradient requires at least two colors');
    {$ENDIF}
  end;
  if ((length(Offsets) > 0) and
      (length(Offsets) <> length(Colors))) then begin
    {$IF defined(ALDPK)}
    Exit;
    {$ELSE}
    raise Exception.Create('The number of gradient offsets does not match the number of gradient colors');
    {$ENDIF}
  end;
  case Style of
    TGradientStyle.linear: result := ALFormatFloatW('linear-gradient(0.#####deg', Angle, ALDefaultFormatSettingsW);
    TGradientStyle.radial: Result := 'radial-gradient(circle';
    else raise Exception.Create('Error 591CBC8C-50A7-4444-A0D6-584EB45AD56A');
  end;
  For var I := Low(Colors) to high(Colors) do begin
    var LAphaColorRec := TAlphaColorRec.Create(Colors[i]);
    result := result + ', #' + ALInttoHexW(LAphaColorRec.R, 2) + ALInttoHexW(LAphaColorRec.G, 2) + ALInttoHexW(LAphaColorRec.B, 2) + ALInttoHexW(LAphaColorRec.A, 2);
    if length(Offsets) > 0 then
      result := result + ALFormatFloatW(' 0.#####%', Offsets[i] * 100, ALDefaultFormatSettingsW);
  end;
  result := result + ')';
end;

{**********************************************************}
procedure TALGradient.SetStyle(const Value: TGradientStyle);
begin
  if fStyle <> Value then begin
    fStyle := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALGradient.SetAngle(const Value: Single);
begin
  if fAngle <> Value then begin
    fAngle := Value;
    Change;
  end;
end;

{****************************************************************}
procedure TALGradient.SetColors(const Value: TArray<TAlphaColor>);
begin
  if fColors <> Value then begin
    fColors := Value;
    Change;
  end;
end;

{************************************************************}
procedure TALGradient.SetOffsets(const Value: TArray<Single>);
begin
  if fOffsets <> Value then begin
    fOffsets := Value;
    Change;
  end;
end;

{******************************************************}
procedure TALGradient.SetCSSFormat(const Value: String);

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _RaiseException(const AMsg: String);
  begin
    {$IF defined(ALDPK)}
    raise EDesignPropertyError.Create(AMsg);
    {$ELSE}
    raise Exception.Create(AMsg);
    {$ENDIF}
  end;

begin

  BeginUpdate;
  try

    Reset;

    Try

      //linear-gradient(44deg, rgba(198,27,27,1) 0%, rgba(134,113,255,1) 100%);
      //radial-gradient(circle, rgba(198,27,27,1) 0%, rgba(134,113,255,1) 100%);
      var LValue := ALTrim(ALLowerCase(Value));
      if LValue = '' then exit;
      LValue := ALStringReplaceW(LValue, #13, ' ', [RfReplaceALL]);
      LValue := ALStringReplaceW(LValue, #10, ' ', [RfReplaceALL]);
      LValue := ALStringReplaceW(LValue, #9, ' ', [RfReplaceALL]);
      while AlposW(' (', LValue) > 0 do LValue := ALStringReplaceW(LValue, ' (', '(', [rfReplaceALL]); // linear-gradient(44deg, rgba(198,27,27,1) 0%, rgba(134,113,255,1) 100%);
      while AlposW(' ,', LValue) > 0 do LValue := ALStringReplaceW(LValue, ' ,', ',', [rfReplaceALL]); // linear-gradient(44deg,rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%);
      while AlposW(', ', LValue) > 0 do LValue := ALStringReplaceW(LValue, ', ', ',', [rfReplaceALL]); // linear-gradient(44deg,rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%);
      While (LValue <> '') and (LValue[length(LValue)] = ';') do begin
        delete(LValue, length(LValue), 1); //linear-gradient(44deg,rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%)
        LValue := ALTrim(LValue);
      end;
      if (LValue <> '') and (LValue[length(LValue)] = ')') then begin
        delete(LValue, length(LValue), 1); //linear-gradient(44deg,rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%
        LValue := ALTrim(LValue);
      end
      else
        _RaiseException('Invalid CSS gradient');

      var P1: integer := 1;
      // linear-gradient
      If Alposw('linear-gradient(', LValue, P1) = P1 then begin
        FStyle := TGradientStyle.linear;
        P1 := P1 + 16{length('linear-gradient(')}; // 44deg,rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%
        var P2 := P1;
        While (P2 <= Length(LValue)) and (CharInSet(LValue[P2], ['0'..'9','.'])) do inc(p2);
        if P2 = P1 then _RaiseException('CSS gradient format error: angle specification missing');
        If not ALTryStrToFloat(ALCopyStr(LValue, P1, P2-P1), FAngle, ALDefaultFormatSettingsW) then
          _RaiseException('CSS gradient format error: invalid angle value');
        P1 := P2; // deg,rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%
        if AlposW('deg,', LValue, P1) = P1 then
          P1 := P1 + 4{length('deg,')} // rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%
        else
          _RaiseException('CSS gradient format error: "deg," expected after angle value');
      end
      // radial-gradient
      else If Alposw('radial-gradient(', LValue, P1) = P1 then begin
        FStyle := TGradientStyle.Radial;
        P1 := P1 + 16{length('radial-gradient(')}; // circle,rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%
        if AlposW('circle,', LValue, P1) = P1 then
          P1 := P1 + 7{length('circle,')} // rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%
        else
          _RaiseException('CSS gradient support error: only "circle" shape is supported for radial gradients');
      end
      else
        _RaiseException('CSS gradient format error: unrecognized gradient type');

      If P1 > Length(LValue) then
        _RaiseException('CSS gradient format error: incomplete gradient definition');

      var LWithOffset: integer := -1;
      While P1 <= Length(LValue) do begin

        var LColor: TAlphacolor;

        // rgb(198,27,27) 0%,rgba(134,113,255,1) 100%
        if Alposw('rgb(', LValue, P1) = P1 then begin
          P1 := P1 + 4{length('rgb(')};
          var P2 := ALPosW(')', LValue, P1);
          if P2 <= P1 then _RaiseException('CSS gradient format error: Expected closing parenthesis for rgb');
          var LLst := TALStringListW.create;
          Try
            LLst.LineBreak := ',';
            LLst.Text := ALCopyStr(LValue, P1, P2-P1);
            If LLst.Count <> 3 then _RaiseException('CSS gradient format error: RGB values count mismatch');
            var R, G, B: integer;
            if (not ALTryStrToInt(LLst[0], R)) or (R < low(Byte)) or (R > high(Byte)) then _RaiseException('CSS gradient format error: Invalid red component');
            if (not ALTryStrToInt(LLst[1], G)) or (G < low(Byte)) or (G > high(Byte)) then _RaiseException('CSS gradient format error: Invalid green component');
            if (not ALTryStrToInt(LLst[2], B)) or (B < low(Byte)) or (B > high(Byte)) then _RaiseException('CSS gradient format error: Invalid blue component');
            LColor := MakeColor(R, G, B);
          finally
            ALFreeAndNil(LLst);
          end;
          P1 := P2 + 1; //  0%,rgba(134,113,255,1) 100%
        end

        // rgba(198,27,27,1) 0%,rgba(134,113,255,1) 100%
        else if Alposw('rgba(', LValue, P1) = P1 then begin
          P1 := P1 + 5{length('rgba(')};
          var P2 := ALPosW(')', LValue, P1);
          if P2 <= P1 then _RaiseException('CSS gradient format error: Expected closing parenthesis for rgba');
          var LLst := TALStringListW.create;
          Try
            LLst.LineBreak := ',';
            LLst.Text := ALCopyStr(LValue, P1, P2-P1);
            If LLst.Count <> 4 then _RaiseException('CSS gradient format error: RGBA values count mismatch');
            var R, G, B: integer;
            var A: Single;
            if (not ALTryStrToInt(LLst[0], R)) or (R < low(Byte)) or (R > high(Byte)) then _RaiseException('CSS gradient format error: Invalid red component');
            if (not ALTryStrToInt(LLst[1], G)) or (G < low(Byte)) or (G > high(Byte)) then _RaiseException('CSS gradient format error: Invalid green component');
            if (not ALTryStrToInt(LLst[2], B)) or (B < low(Byte)) or (B > high(Byte)) then _RaiseException('CSS gradient format error: Invalid blue component');
            if (not ALTryStrToFloat(LLst[3], A, ALDefaultFormatSettingsW)) or (compareValue(A, 0, Tepsilon.Vector) < 0) or (compareValue(A, 1, Tepsilon.Vector) > 0) then _RaiseException('CSS gradient format error: Invalid alpha component');
            LColor := MakeColor(R, G, B, byte(round(A * High(Byte))));
          finally
            ALFreeAndNil(LLst);
          end;
          P1 := P2 + 1; //  0%,rgba(134,113,255,1) 100%
        end

        // #ff000080 0%,rgba(134,113,255,1) 100%
        else if Alposw('#', LValue, P1) = P1 then begin
          P1 := P1 + 1{length('#')};
          var P2 := P1;
          While (P2 <= Length(LValue)) and (CharInSet(LValue[P2], ['0'..'9','a'..'f'])) do inc(P2);
          If not ALTryRGBAHexToAlphaColor(AlcopyStr(LValue, P1, P2-P1), LColor) then _RaiseException('CSS gradient format error: Invalid hexadecimal color');
          P1 := P2 + 1; //  0%,rgba(134,113,255,1) 100%
        end

       // Unknown
        else
          _RaiseException('CSS gradient format error: Unknown color format');

        var LOffset: Single;
        While (P1 <= Length(LValue)) and (LValue[P1] = ' ') do inc(p1);
        var P2 := P1;
        While (P2 <= Length(LValue)) and (CharInSet(LValue[P2], ['0'..'9','.'])) do inc(p2);
        if P2 = P1 then begin
          if LWithOffset = 1 then _RaiseException('CSS gradient format error: Inconsistent color stop definitions');
          LWithOffset := 0;
        end
        else begin
          if LWithOffset = 0 then _RaiseException('CSS gradient format error: Inconsistent color stop definitions');
          LWithOffset := 1;
          If not ALTryStrToFloat(ALCopyStr(LValue, P1, P2-P1), LOffset, ALDefaultFormatSettingsW) then
            _RaiseException('CSS gradient format error: Invalid offset value');
          P1 := P2; // %,rgba(134,113,255,1) 100%
          if AlPosW('%', LValue, P1) = P1 then
            P1 := P1 + 1{length('%')} // ,rgba(134,113,255,1) 100%
          else
            _RaiseException('CSS gradient format error: Missing percentage sign after offset value');
        end;

        Setlength(Fcolors, length(Fcolors) + 1);
        Fcolors[high(Fcolors)] := Lcolor;
        if LWithOffset = 1 then begin
          Setlength(FOffsets, length(FOffsets) + 1);
          FOffsets[high(FOffsets)] := LOffset / 100;
        end;

        if AlPosW(',', LValue, P1) = P1 then
          P1 := P1 + 1{length(',')}; // rgba(134,113,255,1) 100%

      end;

      Change;

    Except
      On E: Exception do begin
        Reset;
        raise;
      end;
    end;

  finally
    EndUpdate;
  end;
end;

{**************************}
constructor TALBrush.Create;
begin
  inherited Create;
  FColor := DefaultColor;
  FGradient := TALGradient.Create;
  FGradient.OnChanged := GradientChanged;
  FResourceName := DefaultResourceName;
  FBackgroundMargins := CreateBackgroundMargins;
  FBackgroundMargins.OnChange := BackgroundMarginsChanged;
  FImageMargins := CreateImageMargins;
  FImageMargins.OnChange := ImageMarginsChanged;
  FImageNoRadius := DefaultImageNoRadius;
  FWrapMode := DefaultWrapMode;
end;

{**************************}
destructor TALBrush.Destroy;
begin
  ALFreeAndNil(FGradient);
  ALFreeAndNil(FBackgroundMargins);
  ALFreeAndNil(FImageMargins);
  inherited;
end;

{***************************************************}
function TALBrush.CreateBackgroundMargins: TALBounds;
begin
  Result := TALBounds.Create;
end;

{**********************************************}
function TALBrush.CreateImageMargins: TALBounds;
begin
  Result := TALBounds.Create;
end;

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALBrush.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Kind', ReadKind{ReadData}, nil{WriteData}, false{hasdata});
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALBrush.ReadKind(Reader: TReader);
begin
  var LKindStr := Reader.ReadIdent;
  If ALSameTextW(LKindStr, 'None') then Color := TAlphaColors.Null;
end;
{$ENDIF}

{*********************************************}
procedure TALBrush.Assign(Source: TPersistent);
begin
  if Source is TALBrush then begin
    BeginUpdate;
    Try
      Color := TALBrush(Source).Color;
      Gradient.Assign(TALBrush(Source).Gradient);
      ResourceName := TALBrush(Source).ResourceName;
      BackgroundMargins.Assign(TALBrush(Source).BackgroundMargins);
      ImageMargins.Assign(TALBrush(Source).ImageMargins);
      ImageNoRadius := TALBrush(Source).ImageNoRadius;
      WrapMode := TALBrush(Source).WrapMode;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{***********************}
procedure TALBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Color := DefaultColor;
    Gradient.Reset;
    ResourceName := DefaultResourceName;
    BackgroundMargins.Rect := BackgroundMargins.DefaultValue;
    ImageMargins.Rect := ImageMargins.DefaultValue;
    ImageNoRadius := DefaultImageNoRadius;
    WrapMode := DefaultWrapMode;
  finally
    EndUpdate;
  end;
end;

{******************************}
procedure TALBrush.AlignToPixel;
begin
  BeginUpdate;
  try
    BackgroundMargins.Rect := ALAlignEdgesToPixelRound(BackgroundMargins.Rect, ALGetScreenScale, TEpsilon.Position);
    ImageMargins.Rect := ALAlignEdgesToPixelRound(ImageMargins.Rect, ALGetScreenScale, TEpsilon.Position);
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
procedure TALBrush.Interpolate(const ATo: TALBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      Gradient.Interpolate(aTo.Gradient, ANormalizedTime);
      ResourceName := ATo.ResourceName;
      BackgroundMargins.Left := InterpolateSingle(BackgroundMargins.Left{Start}, ATo.BackgroundMargins.Left{Stop}, ANormalizedTime);
      BackgroundMargins.Right := InterpolateSingle(BackgroundMargins.Right{Start}, ATo.BackgroundMargins.Right{Stop}, ANormalizedTime);
      BackgroundMargins.Top := InterpolateSingle(BackgroundMargins.Top{Start}, ATo.BackgroundMargins.Top{Stop}, ANormalizedTime);
      BackgroundMargins.Bottom := InterpolateSingle(BackgroundMargins.Bottom{Start}, ATo.BackgroundMargins.Bottom{Stop}, ANormalizedTime);
      ImageMargins.Left := InterpolateSingle(ImageMargins.Left{Start}, ATo.ImageMargins.Left{Stop}, ANormalizedTime);
      ImageMargins.Right := InterpolateSingle(ImageMargins.Right{Start}, ATo.ImageMargins.Right{Stop}, ANormalizedTime);
      ImageMargins.Top := InterpolateSingle(ImageMargins.Top{Start}, ATo.ImageMargins.Top{Stop}, ANormalizedTime);
      ImageMargins.Bottom := InterpolateSingle(ImageMargins.Bottom{Start}, ATo.ImageMargins.Bottom{Stop}, ANormalizedTime);
      ImageNoRadius := ATo.ImageNoRadius;
      WrapMode := ATo.WrapMode;
    end
    else begin
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      Gradient.Interpolate(nil, ANormalizedTime);
      ResourceName := DefaultResourceName;
      BackgroundMargins.Left := InterpolateSingle(BackgroundMargins.Left{Start}, BackgroundMargins.DefaultValue.Left{Stop}, ANormalizedTime);
      BackgroundMargins.Right := InterpolateSingle(BackgroundMargins.Right{Start}, BackgroundMargins.DefaultValue.Right{Stop}, ANormalizedTime);
      BackgroundMargins.Top := InterpolateSingle(BackgroundMargins.Top{Start}, BackgroundMargins.DefaultValue.Top{Stop}, ANormalizedTime);
      BackgroundMargins.Bottom := InterpolateSingle(BackgroundMargins.Bottom{Start}, BackgroundMargins.DefaultValue.Bottom{Stop}, ANormalizedTime);
      ImageMargins.Left := InterpolateSingle(ImageMargins.Left{Start}, ImageMargins.DefaultValue.Left{Stop}, ANormalizedTime);
      ImageMargins.Right := InterpolateSingle(ImageMargins.Right{Start}, ImageMargins.DefaultValue.Right{Stop}, ANormalizedTime);
      ImageMargins.Top := InterpolateSingle(ImageMargins.Top{Start}, ImageMargins.DefaultValue.Top{Stop}, ANormalizedTime);
      ImageMargins.Bottom := InterpolateSingle(ImageMargins.Bottom{Start}, ImageMargins.DefaultValue.Bottom{Stop}, ANormalizedTime);
      ImageNoRadius := DefaultImageNoRadius;
      WrapMode := DefaultWrapMode;
    end;
  finally
    EndUpdate;
  end;
end;

{******************************************************************************************}
procedure TALBrush.InterpolateNoChanges(const ATo: TALBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{*********************************}
function TALBrush.HasFill: boolean;
begin
  result := Styles <> [];
end;

{***************************************}
function TALBrush.Styles: TALBrushStyles;
begin
  Result := [];
  if (Color <> TalphaColors.Null) then result := result + [TALBrushStyle.Solid];
  if (length(Gradient.Colors) > 0) then result := result + [TALBrushStyle.Gradient];
  if (ResourceName <> '') then result := result + [TALBrushStyle.Image];
end;

{******************************************}
function TALBrush.IsRemoteResource: Boolean;
begin
  Result := AlIsHttpOrHttpsUrl(ResourceName);
end;

{***************************************}
function TALBrush.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{**********************************************}
function TALBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> DefaultResourceName;
end;

{***********************************************}
function TALBrush.IsImageNoRadiusStored: Boolean;
begin
  result := FImageNoRadius <> DefaultImageNoRadius;
end;

{******************************************}
function TALBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> DefaultWrapMode;
end;

{*********************************************}
function TALBrush.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.white; // $FFE0E0E0;
end;

{***********************************************}
function TALBrush.GetDefaultResourceName: String;
begin
  Result := '';
end;

{*************************************************}
function TALBrush.GetDefaultImageNoRadius: Boolean;
begin
  Result := False;
end;

{*****************************************************}
function TALBrush.GetDefaultWrapMode: TALImageWrapMode;
begin
  Result := TALImageWrapMode.Fit;
end;

{****************************************************}
procedure TALBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
  end;
end;

{*******************************************************}
procedure TALBrush.SetGradient(const Value: TAlGradient);
begin
  FGradient.Assign(Value);
end;

{******************************************************}
procedure TALBrush.SetResourceName(const Value: String);
begin
  if fResourceName <> Value then begin
    fResourceName := Value;
    Change;
  end;
end;

{**************************************************************}
procedure TALBrush.SetBackgroundMargins(const Value: TALBounds);
begin
  FBackgroundMargins.Assign(Value);
end;

{*********************************************************}
procedure TALBrush.SetImageMargins(const Value: TALBounds);
begin
  FImageMargins.Assign(Value);
end;

{********************************************************}
procedure TALBrush.SetImageNoRadius(const Value: Boolean);
begin
  if fImageNoRadius <> Value then begin
    fImageNoRadius := Value;
    Change;
  end;
end;

{************************************************************}
procedure TALBrush.SetWrapMode(const Value: TALImageWrapMode);
begin
  if fWrapMode <> Value then begin
    fWrapMode := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALBrush.GradientChanged(Sender: TObject);
begin
  change;
end;

{***********************************************************}
procedure TALBrush.BackgroundMarginsChanged(Sender: TObject);
begin
  change;
end;

{******************************************************}
procedure TALBrush.ImageMarginsChanged(Sender: TObject);
begin
  change;
end;

{**********************************************************}
constructor TALInheritBrush.Create(const AParent: TALBrush);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{***************************************************************}
function TALInheritBrush.CreateSavedState: TALPersistentObserver;
type
  TALInheritBrushClass = class of TALInheritBrush;
begin
  result := TALInheritBrushClass(classtype).Create(nil{AParent});
end;

{**********************************************************}
procedure TALInheritBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{****************************************************}
procedure TALInheritBrush.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALInheritBrush then begin
      Inherit := TALInheritBrush(Source).Inherit;
      fSuperseded := TALInheritBrush(Source).fSuperseded;
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
procedure TALInheritBrush.Reset;
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

{************************************}
procedure TALInheritBrush.DoSupersede;
begin
  Assign(FParent);
end;

{*********************************************************************}
procedure TALInheritBrush.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  BeginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TALInheritBrush then begin
      TALInheritBrush(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TALInheritBrush(FParent).restoreStateNoChanges;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{******************************************************************************}
procedure TALInheritBrush.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{********************************}
constructor TALStrokeBrush.Create;
begin
  inherited Create;
  FColor := DefaultColor;
  FThickness := DefaultThickness;
end;

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALStrokeBrush.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Kind', ReadKind{ReadData}, nil{WriteData}, false{hasdata});
end;
{$ENDIF}

{*********************************}
{$IF defined(ALBackwardCompatible)}
procedure TALStrokeBrush.ReadKind(Reader: TReader);
begin
  var LKindStr := Reader.ReadIdent;
  If ALSameTextW(LKindStr, 'None') then Color := TAlphaColors.Null;
end;
{$ENDIF}

{***************************************************}
procedure TALStrokeBrush.Assign(Source: TPersistent);
begin
  if Source is TALStrokeBrush then begin
    BeginUpdate;
    Try
      Color := TALStrokeBrush(Source).Color;
      Thickness := TALStrokeBrush(Source).Thickness;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*****************************}
procedure TALStrokeBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Color := DefaultColor;
    Thickness := DefaultThickness;
  finally
    EndUpdate;
  end;
end;

{************************************}
procedure TALStrokeBrush.AlignToPixel;
begin
  BeginUpdate;
  try
    Thickness := ALAlignDimensionToPixelRound(Thickness, ALGetScreenScale, TEpsilon.Vector);
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************************}
procedure TALStrokeBrush.Interpolate(const ATo: TALStrokeBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      Thickness := InterpolateSingle(Thickness{Start}, ATo.Thickness{Stop}, ANormalizedTime);
    end
    else begin
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      Thickness := InterpolateSingle(Thickness{Start}, DefaultThickness{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{******************************************************************************************************}
procedure TALStrokeBrush.InterpolateNoChanges(const ATo: TALStrokeBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{*****************************************}
function TALStrokeBrush.HasStroke: boolean;
begin
  result := (Color <> TalphaColors.Null) and
            (CompareValue(Thickness, 0, TEpsilon.Vector) > 0);
end;

{*********************************************}
function TALStrokeBrush.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{*************************************************}
function TALStrokeBrush.IsThicknessStored: Boolean;
begin
  result := not SameValue(FThickness, DefaultThickness, TEpsilon.Vector);
end;

{***************************************************}
function TALStrokeBrush.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Black;
end;

{**************************************************}
function TALStrokeBrush.GetDefaultThickness: Single;
begin
  Result := 1;
end;

{**********************************************************}
procedure TALStrokeBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
  end;
end;

{*********************************************************}
procedure TALStrokeBrush.SetThickness(const Value: Single);
begin
  if not SameValue(Value, FThickness, TEpsilon.Vector) then begin
    fThickness := Value;
    Change;
  end;
end;

{**********************************************************************}
constructor TALInheritStrokeBrush.Create(const AParent: TALStrokeBrush);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := False;
end;

{*********************************************************************}
function TALInheritStrokeBrush.CreateSavedState: TALPersistentObserver;
type
  TALInheritStrokeBrushClass = class of TALInheritStrokeBrush;
begin
  result := TALInheritStrokeBrushClass(classtype).Create(nil{AParent});
end;

{****************************************************************}
procedure TALInheritStrokeBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{**********************************************************}
procedure TALInheritStrokeBrush.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALInheritStrokeBrush then begin
      Inherit := TALInheritStrokeBrush(Source).Inherit;
      fSuperseded := TALInheritStrokeBrush(Source).fSuperseded;
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

{************************************}
procedure TALInheritStrokeBrush.Reset;
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

{******************************************}
procedure TALInheritStrokeBrush.DoSupersede;
begin
  Assign(FParent);
end;

{***************************************************************************}
procedure TALInheritStrokeBrush.Supersede(Const ASaveState: Boolean = False);
begin
  if ASaveState then SaveState;
  if (FSuperseded) or
     (not inherit) or
     (FParent = nil) then exit;
  BeginUpdate;
  try
    var LParentSuperseded := False;
    if FParent is TALInheritStrokeBrush then begin
      TALInheritStrokeBrush(FParent).SupersedeNoChanges(true{ASaveState});
      LParentSuperseded := True;
    end;
    try
      DoSupersede;
    finally
      if LParentSuperseded then
        TALInheritStrokeBrush(FParent).restoreStateNoChanges;
    end;
    Inherit := False;
    FSuperseded := True;
  finally
    EndUpdate;
  end;
end;

{************************************************************************************}
procedure TALInheritStrokeBrush.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{*******************************}
constructor TALStateLayer.Create;
begin
  inherited Create;
  //--
  FOpacity := DefaultOpacity;
  FColor := DefaultColor;
  FUseContentColor := DefaultUseContentColor;
  FXRadius := DefaultXRadius;
  FYRadius := DefaultYRadius;
  //--
  FMargins := CreateMargins;
  FMargins.OnChange := MarginsChanged;
end;

{*******************************}
destructor TALStateLayer.Destroy;
begin
  ALFreeAndNil(FMargins);
  inherited;
end;

{**********************************************}
function TALStateLayer.CreateMargins: TALBounds;
begin
  Result := TALBounds.Create;
end;

{**************************************************}
procedure TALStateLayer.Assign(Source: TPersistent);
begin
  if Source is TALStateLayer then begin
    BeginUpdate;
    Try
      Opacity := TALStateLayer(Source).Opacity;
      Color := TALStateLayer(Source).Color;
      UseContentColor := TALStateLayer(Source).UseContentColor;
      Margins.Assign(TALStateLayer(Source).Margins);
      XRadius := TALStateLayer(Source).XRadius;
      YRadius := TALStateLayer(Source).YRadius;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{****************************}
procedure TALStateLayer.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Opacity := DefaultOpacity;
    Color := DefaultColor;
    UseContentColor := DefaultUseContentColor;
    Margins.Rect := Margins.DefaultValue;
    XRadius := DefaultXRadius;
    YRadius := DefaultYRadius;
  finally
    EndUpdate;
  end;
end;

{***********************************}
procedure TALStateLayer.AlignToPixel;
begin
  BeginUpdate;
  try
    Margins.Rect := ALAlignEdgesToPixelRound(Margins.Rect, ALGetScreenScale, TEpsilon.Position);
  finally
    EndUpdate;
  end;
end;

{*******************************************************************************************}
procedure TALStateLayer.Interpolate(const ATo: TALStateLayer; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Opacity := InterpolateSingle(Opacity{Start}, ATo.Opacity{Stop}, ANormalizedTime);
      Color := ALInterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      UseContentColor := ATo.UseContentColor;
      Margins.Left := InterpolateSingle(Margins.Left{Start}, ATo.Margins.Left{Stop}, ANormalizedTime);
      Margins.Right := InterpolateSingle(Margins.Right{Start}, ATo.Margins.Right{Stop}, ANormalizedTime);
      Margins.Top := InterpolateSingle(Margins.Top{Start}, ATo.Margins.Top{Stop}, ANormalizedTime);
      Margins.Bottom := InterpolateSingle(Margins.Bottom{Start}, ATo.Margins.Bottom{Stop}, ANormalizedTime);
      XRadius := InterpolateSingle(XRadius{Start}, ATo.XRadius{Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, ATo.YRadius{Stop}, ANormalizedTime);
    end
    else begin
      Opacity := InterpolateSingle(Opacity{Start}, DefaultOpacity{Stop}, ANormalizedTime);
      Color := ALInterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      UseContentColor := DefaultUseContentColor;
      Margins.Left := InterpolateSingle(Margins.Left{Start}, Margins.DefaultValue.Left{Stop}, ANormalizedTime);
      Margins.Right := InterpolateSingle(Margins.Right{Start}, Margins.DefaultValue.Right{Stop}, ANormalizedTime);
      Margins.Top := InterpolateSingle(Margins.Top{Start}, Margins.DefaultValue.Top{Stop}, ANormalizedTime);
      Margins.Bottom := InterpolateSingle(Margins.Bottom{Start}, Margins.DefaultValue.Bottom{Stop}, ANormalizedTime);
      XRadius := InterpolateSingle(XRadius{Start}, DefaultXRadius{Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, DefaultYRadius{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{****************************************************************************************************}
procedure TALStateLayer.InterpolateNoChanges(const ATo: TALStateLayer; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{**************************************}
function TALStateLayer.HasFill: boolean;
begin
  result := ((Color <> TalphaColors.Null) or (UseContentColor)) and
            (CompareValue(Opacity, 0, TEpsilon.Scale) > 0);
end;

{**********************************************}
function TALStateLayer.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, DefaultOpacity, TEpsilon.Scale);
end;

{********************************************}
function TALStateLayer.IsColorStored: Boolean;
begin
  result := FColor <> DefaultColor;
end;

{******************************************************}
function TALStateLayer.IsUseContentColorStored: Boolean;
begin
  Result := FUseContentColor <> DefaultUseContentColor;
end;

{**********************************************}
function TALStateLayer.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, DefaultXRadius, TEpsilon.Vector);
end;

{**********************************************}
function TALStateLayer.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, DefaultYRadius, TEpsilon.Vector);
end;

{***********************************************}
function TALStateLayer.GetDefaultOpacity: Single;
begin
  Result := 0;
end;

{**************************************************}
function TALStateLayer.GetDefaultColor: TAlphaColor;
begin
  Result := TAlphaColors.Null;
end;

{********************************************************}
function TALStateLayer.GetDefaultUseContentColor: Boolean;
begin
  Result := False;
end;

{***********************************************}
function TALStateLayer.GetDefaultXRadius: Single;
begin
  Result := 0;
end;

{***********************************************}
function TALStateLayer.GetDefaultYRadius: Single;
begin
  Result := 0;
end;

{******************************************************}
procedure TALStateLayer.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{*********************************************************}
procedure TALStateLayer.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
  end;
end;

{***************************************************************}
procedure TALStateLayer.SetUseContentColor(const Value: Boolean);
begin
  if FUseContentColor <> Value then begin
    FUseContentColor := Value;
    Change;
  end;
end;

{*********************************************************}
procedure TALStateLayer.SetMargins(const Value: TALBounds);
begin
  FMargins.Assign(Value);
end;

{******************************************************}
procedure TALStateLayer.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    FXRadius := Value;
    Change;
  end;
end;

{******************************************************}
procedure TALStateLayer.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    FYRadius := Value;
    Change;
  end;
end;

{******************************************************}
procedure TALStateLayer.MarginsChanged(Sender: TObject);
begin
  change;
end;

{************************************}
constructor TALStateTransition.Create;
begin
  inherited Create;
  FAnimationType := DefaultAnimationType;
  FDuration := DefaultDuration;
  FInterpolation := DefaultInterpolation;
  FDelayClick := DefaultDelayClick;
end;

{*******************************************************}
procedure TALStateTransition.Assign(Source: TPersistent);
begin
  if Source is TALStateTransition then begin
    BeginUpdate;
    Try
      AnimationType := TALStateTransition(Source).AnimationType;
      Duration := TALStateTransition(Source).Duration;
      Interpolation := TALStateTransition(Source).Interpolation;
      DelayClick := TALStateTransition(Source).DelayClick;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*********************************}
procedure TALStateTransition.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    AnimationType := DefaultAnimationType;
    Duration := DefaultDuration;
    Interpolation := DefaultInterpolation;
    DelayClick := DefaultDelayClick;
  finally
    EndUpdate;
  end;
end;

{*********************************************************}
function TALStateTransition.IsAnimationTypeStored: Boolean;
begin
  result := FAnimationType <> DefaultAnimationType;
end;

{****************************************************}
function TALStateTransition.IsDurationStored: Boolean;
begin
  result := not SameValue(fDuration, DefaultDuration, Tepsilon.Scale);
end;

{*********************************************************}
function TALStateTransition.IsInterpolationStored: Boolean;
begin
  result := FInterpolation <> DefaultInterpolation;
end;

{******************************************************}
function TALStateTransition.IsDelayClickStored: Boolean;
begin
  result := FDelayClick <> DefaultDelayClick;
end;

{******************************************************************}
function TALStateTransition.GetDefaultAnimationType: TAnimationType;
begin
  Result := TAnimationType.Out;
end;

{*****************************************************}
function TALStateTransition.GetDefaultDuration: Single;
begin
  Result := 0.16;
end;

{************************************************************************}
function TALStateTransition.GetDefaultInterpolation: TALInterpolationType;
begin
  Result := TALInterpolationType.Cubic;
end;

{********************************************************}
function TALStateTransition.GetDefaultDelayClick: Boolean;
begin
  Result := False;
end;

{*************************************************************************}
procedure TALStateTransition.SetAnimationType(const Value: TAnimationType);
begin
  if fAnimationType <> Value then begin
    fAnimationType := Value;
    Change;
  end;
end;

{************************************************************}
procedure TALStateTransition.SetDuration(const Value: Single);
begin
  if Not SameValue(fDuration, Value, Tepsilon.Scale) then begin
    fDuration := Value;
    Change;
  end;
end;

{*******************************************************************************}
procedure TALStateTransition.SetInterpolation(const Value: TALInterpolationType);
begin
  if fInterpolation <> Value then begin
    fInterpolation := Value;
    Change;
  end;
end;

{***************************************************************}
procedure TALStateTransition.SetDelayClick(const Value: Boolean);
begin
  if fDelayClick <> Value then begin
    fDelayClick := Value;
    Change;
  end;
end;

{**}
Type
  _TControlAccessProtected = class(Tcontrol);

{***********************************************************}
constructor TALBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create;
  //--
  FParent := AParent;
  if (AParent is TALShape) then begin
    var LShapeControl := TALShape(AParent);
    FStateStyleParent := nil;
    FControlParent := TALControl(AParent);
    FFill := CreateFill(LShapeControl.fill);
    FStateLayer := CreateStateLayer;
    FStroke := CreateStroke(LShapeControl.Stroke);
    FShadow := CreateShadow(LShapeControl.Shadow);
  end
  else if (AParent is TALBaseStateStyle) then begin
    FStateStyleParent := TALBaseStateStyle(AParent);
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

{***********************************}
destructor TALBaseStateStyle.Destroy;
begin
  ClearBufDrawable;
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStateLayer);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FShadow);
  inherited Destroy;
end;

{*****************************************************************}
function TALBaseStateStyle.CreateSavedState: TALPersistentObserver;
type
  TALBaseStateStyleClass = class of TALBaseStateStyle;
begin
  result := TALBaseStateStyleClass(classtype).Create(nil{AParent});
end;

{******************************************************************************}
function TALBaseStateStyle.CreateFill(const AParent: TALBrush): TALInheritBrush;
begin
  Result := TALInheritBrush.Create(AParent)
end;

{*********************************************************}
function TALBaseStateStyle.CreateStateLayer: TALStateLayer;
begin
  Result := TALStateLayer.Create;
end;

{********************************************************************************************}
function TALBaseStateStyle.CreateStroke(const AParent: TALStrokeBrush): TALInheritStrokeBrush;
begin
  Result := TALInheritStrokeBrush.Create(AParent);
end;

{**********************************************************************************}
function TALBaseStateStyle.CreateShadow(const AParent: TALShadow): TALInheritShadow;
begin
  Result := TALInheritShadow.Create(AParent);
end;

{******************************************************}
procedure TALBaseStateStyle.Assign(Source: TPersistent);
begin
  if Source is TALBaseStateStyle then begin
    BeginUpdate;
    Try
      Fill.Assign(TALBaseStateStyle(Source).Fill);
      StateLayer.Assign(TALBaseStateStyle(Source).StateLayer);
      Stroke.Assign(TALBaseStateStyle(Source).Stroke);
      Shadow.Assign(TALBaseStateStyle(Source).Shadow);
      Scale := TALBaseStateStyle(Source).Scale;
      fSuperseded := TALBaseStateStyle(Source).fSuperseded;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{********************************}
procedure TALBaseStateStyle.Reset;
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

{***************************************}
procedure TALBaseStateStyle.AlignToPixel;
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

{*******************************************}
procedure TALBaseStateStyle.ClearBufDrawable;
begin
  ALFreeAndNilDrawable(FBufDrawable);
end;

{***************************************************************************************************}
procedure TALBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
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
    else if (FControlParent is TALShape) then begin
      var LShapeControl := TALShape(FControlParent);
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

{************************************************************************************************************}
procedure TALBaseStateStyle.InterpolateNoChanges(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{**************************************}
procedure TALBaseStateStyle.DoSupersede;
begin
  Fill.Supersede;
  Stroke.Supersede;
  Shadow.Supersede;
  // Do not supersede the scale
end;

{***********************************************************************}
procedure TALBaseStateStyle.Supersede(Const ASaveState: Boolean = False);
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

{********************************************************************************}
procedure TALBaseStateStyle.SupersedeNoChanges(Const ASaveState: Boolean = False);
begin
  BeginUpdate;
  try
    Supersede(ASaveState);
  finally
    EndUpdateNoChanges;
  end;
end;

{************************************************}
function TALBaseStateStyle.IsScaleStored: Boolean;
begin
  result := not SameValue(fScale, DefaultScale, Tepsilon.Scale);
end;

{*************************************************}
function TALBaseStateStyle.GetDefaultScale: Single;
begin
  Result := 1;
end;

{*****************************************************************}
procedure TALBaseStateStyle.SetFill(const AValue: TALInheritBrush);
begin
  FFill.Assign(AValue);
end;

{*********************************************************************}
procedure TALBaseStateStyle.SetStateLayer(const AValue: TALStateLayer);
begin
  FStateLayer.Assign(AValue);
end;

{*************************************************************************}
procedure TALBaseStateStyle.SetStroke(const AValue: TALInheritStrokeBrush);
begin
  FStroke.Assign(AValue);
end;

{********************************************************************}
procedure TALBaseStateStyle.SetShadow(const AValue: TALInheritShadow);
begin
  FShadow.Assign(AValue);
end;

{********************************************************}
procedure TALBaseStateStyle.SetScale(const Value: Single);
begin
  if not SameValue(FScale, Value, TEpsilon.Scale) then begin
    FScale := Value;
    Change;
  end;
end;

{*********************************************}
function TALBaseStateStyle.GetInherit: Boolean;
begin
  Result := Fill.Inherit and
            (not StateLayer.HasFill) and
            Stroke.Inherit and
            Shadow.Inherit and
            Samevalue(Scale, DefaultScale, TEpsilon.Scale);
end;

{***************************************************}
function TALBaseStateStyle.GetCacheSubIndex: Integer;
begin
  Result := 0;
end;

{********************************************************}
procedure TALBaseStateStyle.FillChanged(ASender: TObject);
begin
  Change;
end;

{**************************************************************}
procedure TALBaseStateStyle.StateLayerChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************}
procedure TALBaseStateStyle.StrokeChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************}
procedure TALBaseStateStyle.ShadowChanged(ASender: TObject);
begin
  Change;
end;

{***************************************************************}
constructor TALBaseStateStyles.Create(const AParent: TALControl);
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

{************************************}
destructor TALBaseStateStyles.Destroy;
begin
  ALFreeAndNil(FTransitionAnimation);
  ALfreeandNil(FTransitionFrom);
  ALfreeandNil(FTransitionTo);
  //FLastPaintedRawStyle
  ALfreeandNil(FCurrentAdjustedStyle);
  ALFreeAndNil(FTransition);
  inherited Destroy;
end;

{******************************************************************}
function TALBaseStateStyles.CreateSavedState: TALPersistentObserver;
type
  TALBaseStateStylesClass = class of TALBaseStateStyles;
begin
  result := TALBaseStateStylesClass(classtype).Create(nil{AParent});
end;

{***************************************************************}
function TALBaseStateStyles.CreateTransition: TALStateTransition;
begin
  result := TALStateTransition.Create;
end;

{*******************************************}
procedure TALBaseStateStyles.StartTransition;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  function _IsSameStateStyleClass(const AStateStyleA, AStateStyleB: TALBaseStateStyle): boolean;
  begin
    result := AStateStyleA = AStateStyleB;
    if (not result) and
       (AStateStyleA <> nil) and
       (AStateStyleB <> nil) then begin
      result := AStateStyleA.ClassType = AStateStyleB.ClassType;
    end;
  end;

type
  TALBaseStateStyleClass = class of TALBaseStateStyle;

begin

  If ([csLoading, csDestroying, csDesigning] * parent.ComponentState <> []) then Exit;
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
      FTransitionFrom := TALBaseStateStyleClass(FCurrentAdjustedStyle.classtype).Create(FCurrentAdjustedStyle.Parent{AParent});
      FTransitionFrom.Assign(FCurrentAdjustedStyle);
    end
    else begin
      ALFreeAndNil(FTransitionFrom);
      if FLastPaintedRawStyle = nil then FTransitionFrom := nil
      else begin
        FTransitionFrom := TALBaseStateStyleClass(FLastPaintedRawStyle.classtype).Create(FLastPaintedRawStyle.Parent{AParent});
        FTransitionFrom.Assign(FLastPaintedRawStyle);
      end;
    end;
    //--
    ALFreeAndNil(FTransitionTo);
    if LCurrentRawStyle = nil then FTransitionTo := nil
    else begin
      FTransitionTo := TALBaseStateStyleClass(LCurrentRawStyle.classtype).Create(LCurrentRawStyle.parent{AParent});
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
    //  'TALBaseStateStyles.StartTransition',
    //  'From: '+LTransitionFromClassName + ' | ' +
    //  'To: '+LTransitionToClassName);
    {$ENDIF}

  finally
    if FTransitionAnimation.Running then
      FTransitionClickDelayed := LPrevTransitionClickDelayed
    else if LPrevTransitionClickDelayed then
      _TControlAccessProtected(FParent).click;
  end;
end;

{***********************************************************************}
procedure TALBaseStateStyles.TransitionAnimationProcess(Sender: TObject);
begin
  {$IF defined(debug)}
  //ALLog('TALBaseStateStyles.TransitionAnimationProcess');
  {$ENDIF}
  FParent.Repaint;
end;

{**********************************************************************}
procedure TALBaseStateStyles.TransitionAnimationFinish(Sender: TObject);
begin
  {$IF defined(debug)}
  //ALLog('TALBaseStateStyles.TransitionAnimationFinish');
  {$ENDIF}
  FTransitionAnimation.Enabled := False;
  if FTransitionClickDelayed then begin
    FTransitionClickDelayed := False;
    _TControlAccessProtected(FParent).Click;
  end;
  FParent.Repaint;
end;

{*******************************************************}
procedure TALBaseStateStyles.Assign(Source: TPersistent);
begin
  if Source is TALBaseStateStyles then begin
    BeginUpdate;
    Try
      Transition.Assign(TALBaseStateStyles(Source).Transition);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{*********************************}
procedure TALBaseStateStyles.Reset;
begin
  BeginUpdate;
  Try
    inherited;
    Transition.Reset;
  finally
    EndUpdate;
  end;
end;

{****************************************}
procedure TALBaseStateStyles.AlignToPixel;
begin
  // Virtual
end;

{********************************************}
procedure TALBaseStateStyles.ClearBufDrawable;
begin
  // Virtual
end;

{****************************************************************}
function TALBaseStateStyles.GetCurrentRawStyle: TALBaseStateStyle;
begin
  Raise Exception.Create('Not implemented')
end;

{*********************************************************************}
function TALBaseStateStyles.GetCurrentAdjustedStyle: TALBaseStateStyle;
type
  TALBaseStateStyleClass = class of TALBaseStateStyle;
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
      FCurrentAdjustedStyle := TALBaseStateStyleClass(LStateStyle.classtype).Create(LStateStyle.parent{AParent});
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
        FCurrentAdjustedStyle := TALBaseStateStyleClass(LStateStyle.classtype).Create(LStateStyle.parent{AParent});
      end;
      FCurrentAdjustedStyle.Assign(LStateStyle);
      FCurrentAdjustedStyle.SupersedeNoChanges(false{ASaveState});
    end;
  end;
  Result := FCurrentAdjustedStyle;
end;

{****************************************************************}
function TALBaseStateStyles.IsTransitionAnimationRunning: Boolean;
begin
  Result := FTransitionAnimation.Enabled and
            FTransitionAnimation.Running;
end;

{*****************************************************}
procedure TALBaseStateStyles.UpdateLastPaintedRawStyle;
begin
  FLastPaintedRawStyle := GetCurrentRawStyle;
end;

{**************************************************************************}
procedure TALBaseStateStyles.SetTransition(const Value: TALStateTransition);
begin
  FTransition.Assign(Value);
end;

{***************************************************************}
procedure TALBaseStateStyles.TransitionChanged(ASender: TObject);
begin
  Change;
end;

{******************************************************************************************************************}
class procedure TALFontManager.RegisterTypefaceFromResource(const AResourceName: string; const AFamilyName: string);
begin

  {$IF defined(ALSkiaEngine)}
  var LStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    TSkDefaultProviders.RegisterTypeface(LStream);
  finally
    ALfreeandNil(LStream);
  end;
  {$ENDIF}

  {$IF (not defined(ALSkiaEngine)) and (defined(Android))}
  var LStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    Var LfileName := TPath.GetTempFileName;
    Lstream.SaveToFile(LfileName);
    try
      var LtypeFace := TJtypeFace.JavaClass.createFromFile(StringToJstring(Lfilename));
      If LtypeFace = nil then raise Exception.CreateFmt('Failed to create Typeface from the resource: %s', [AResourceName]);
      FCustomTypefaces.Add(AFamilyName.ToLower, LtypeFace);
    finally
      TFile.Delete(LfileName);
    end;
  finally
    ALfreeandNil(LStream);
  end;
  {$ENDIF}

  {$IF (not defined(ALSkiaEngine)) and (defined(ALAppleOS))}
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

{******************************************************}
{$IF (not defined(ALSkiaEngine)) and (defined(Android))}
class function TALFontManager.GetCustomTypeFace(const AFamilyName: string): JTypeFace;
begin
  if not FCustomTypefaces.TryGetValue(AFamilyName.ToLower, Result) then result := Nil;
end;
{$ENDIF}

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
    //else if ALSametextW(LFontFamily, 'sans-serif-thin') then result := 'Helvetica Neue Thin'
    //else if ALSametextW(LFontFamily, 'sans-serif-light') then result := 'Helvetica Neue Light'
    //else if ALSametextW(LFontFamily, 'sans-serif-medium') then result := 'Helvetica Neue Medium'
    //else if ALSametextW(LFontFamily, 'sans-serif-black') then result := 'Helvetica Neue Bold'
    else result := LFontFamily;
    {$ELSEIF defined(MSWINDOWS)}
    if ALSametextW(LFontFamily, 'sans-serif') then result := 'Segoe UI'
    //else if ALSametextW(LFontFamily, 'sans-serif-thin') then result := 'Segoe UI Light'
    //else if ALSametextW(LFontFamily, 'sans-serif-light') then result := 'Segoe UI Light'
    //else if ALSametextW(LFontFamily, 'sans-serif-medium') then result := 'Segoe UI Semibold'
    //else if ALSametextW(LFontFamily, 'sans-serif-black') then result := 'Segoe UI Black'
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

{*************************}
{$IF defined(ALSkiaEngine)}
Function ALGetSkFontStyle(
           const AFontWeight: TFontWeight;
           const AFontSlant: TFontSlant;
           const AFontStretch: TFontStretch): sk_fontstyle_t;
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if declaration of System.Skia.API.sk_fontstyle_t didn''t changed'}
  {$ENDIF}
  //--
  // Defined in SkFontStyle.h
  //
  //  enum Weight {
  //      kInvisible_Weight   =    0,
  //      kThin_Weight        =  100,
  //      kExtraLight_Weight  =  200,
  //      kLight_Weight       =  300,
  //      kNormal_Weight      =  400,
  //      kMedium_Weight      =  500,
  //      kSemiBold_Weight    =  600,
  //      kBold_Weight        =  700,
  //      kExtraBold_Weight   =  800,
  //      kBlack_Weight       =  900,
  //      kExtraBlack_Weight  = 1000,
  //  };
  case AFontWeight of
    TFontWeight.Thin: Result.weight := 100; // kThin_Weight        =  100,
    TFontWeight.UltraLight: Result.weight := 200; // kExtraLight_Weight  =  200,
    TFontWeight.Light: Result.weight := 300; // kLight_Weight       =  300,
    TFontWeight.SemiLight: Result.weight := 350;
    TFontWeight.Regular: Result.weight := 400; // kNormal_Weight      =  400,
    TFontWeight.Medium: Result.weight := 500; // kMedium_Weight      =  500,
    TFontWeight.Semibold: Result.weight := 600; // kSemiBold_Weight    =  600,
    TFontWeight.Bold: Result.weight := 700; // kBold_Weight        =  700,
    TFontWeight.UltraBold: Result.weight := 800; // kExtraBold_Weight   =  800,
    TFontWeight.Black: Result.weight := 900; // kBlack_Weight       =  900,
    TFontWeight.UltraBlack: Result.weight := 1000; // kExtraBlack_Weight  = 1000,
    else raise Exception.Create('Error F5486A8A-8CE8-415D-A845-29AB360C82DE');
  end;
  //--
  case AFontSlant of
    TFontSlant.Regular: Result.slant := sk_fontslant_t.UPRIGHT_SK_FONTSLANT;
    TFontSlant.Italic: Result.slant := sk_fontslant_t.ITALIC_SK_FONTSLANT;
    TFontSlant.Oblique: Result.slant := sk_fontslant_t.OBLIQUE_SK_FONTSLANT;
    else raise Exception.Create('Error 035F6EC0-7AAD-4AC8-BED6-3594DB666E62');
  end;
  //--
  // Defined in SkFontStyle.h
  //
  //  enum Width {
  //      kUltraCondensed_Width   = 1,
  //      kExtraCondensed_Width   = 2,
  //      kCondensed_Width        = 3,
  //      kSemiCondensed_Width    = 4,
  //      kNormal_Width           = 5,
  //      kSemiExpanded_Width     = 6,
  //      kExpanded_Width         = 7,
  //      kExtraExpanded_Width    = 8,
  //      kUltraExpanded_Width    = 9,
  //  };
  case AFontStretch of
    TFontStretch.UltraCondensed: Result.width := 1; // kUltraCondensed_Width   = 1,
    TFontStretch.ExtraCondensed: Result.width := 2; // kExtraCondensed_Width   = 2,
    TFontStretch.Condensed     : Result.width := 3; // kCondensed_Width        = 3,
    TFontStretch.SemiCondensed : Result.width := 4; // kSemiCondensed_Width    = 4,
    TFontStretch.Regular       : Result.width := 5; // kNormal_Width           = 5,
    TFontStretch.SemiExpanded  : Result.width := 6; // kSemiExpanded_Width     = 6,
    TFontStretch.Expanded      : Result.width := 7; // kExpanded_Width         = 7,
    TFontStretch.ExtraExpanded : Result.width := 8; // kExtraExpanded_Width    = 8,
    TFontStretch.UltraExpanded : Result.width := 9; // kUltraExpanded_Width    = 9,
    else raise Exception.Create('Error 05D911E1-D8A4-4692-AF95-DE878BE3838D');
  end;
end;
{$ENDIF}

{**}
type
  // packed because of https://stackoverflow.com/questions/61731462/is-this-declaration-good-tdictionarytpairint32-int64-bool
  TALFontMetricsKey = packed record
    FontFamily: {$IF not defined(ALDPK)}TALSHA1Digest{$ELSE}int64{$ENDIF};
    FontSize: single;
    FontWeight: TFontWeight;
    FontSlant: TFontSlant;
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
           const AFontSlant: TFontSlant): TALFontMetrics;
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
  ALFontMetricsCacheLock.beginRead;
  Try
    if ALFontMetricsCache.TryGetValue(LFontMetricsKey, Result) then exit;
  finally
    ALFontMetricsCacheLock.endRead;
  end;

  {$IF defined(ALSkiaEngine)}
  var LFontFamily := ALExtractPrimaryFontFamily(AFontFamily);
  var LSkFontStyle := ALGetSkFontStyle(AFontWeight, AFontSlant, TFontStretch.Regular);
  var LSkTypeface := ALSkCheckHandle(
                       sk4d_typeface_make_from_name(
                         MarshaledAString(UTF8String(LFontFamily)), // const family_name: MarshaledAString;
                         @LSkFontStyle)); // const style: psk_fontstyle_t);
  try
    var LSkFont := sk4d_font_create(
                     LSkTypeface, // typeface: sk_typeface_t;
                     AFontSize, // size,
                     1.0, // sx,
                     0.0); // kx: float): sk_font_t;
    try
      var LSkFontMetrics: sk_fontmetrics_t;
      sk4d_font_get_metrics(LSkFont, @LSkFontMetrics);
      Result.Ascent := LSkFontMetrics.ascent;
      Result.Descent := LSkFontMetrics.descent;
      Result.Leading := LSkFontMetrics.leading;
    finally
      sk4d_font_destroy(LSkFont);
    end;
  finally
    sk4d_refcnt_unref(LSktypeface);
  end;
  {$ENDIF}

  {$IF defined(ALAppleOS) and (not defined(ALSkiaEngine))}
  var Lfont := ALCreateCTFontRef(AFontFamily, AFontSize, AFontWeight, AFontSlant);
  try
    Result.Ascent := -CTFontGetAscent(Lfont);
    Result.Descent := CTFontGetDescent(Lfont);
    Result.Leading := CTFontGetLeading(Lfont);
  finally
    CFRelease(LFont);
  end;
  {$ENDIF}

  {$IF defined(ANDROID) and (not defined(ALSkiaEngine))}
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
  if TOSVersion.Check(9, 0) then begin
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

  var LFontMetrics := LPaint.getFontMetrics;
  Result.Ascent := LFontMetrics.Ascent;
  Result.Descent := LFontMetrics.Descent;
  Result.Leading := LFontMetrics.Leading;

  LPaint := nil;
  {$ENDIF}

  {$IF defined(MSWINDOWS) and (not defined(ALSkiaEngine))}

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
                 0, // fdwUnderline: DWORD
                 0, // fdwStrikeOut: DWORD
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
    if ALFontMetricsCache.TryAdd(LFontMetricsKey, Result) then begin
      {$IF defined(DEBUG)}
      ALLog(
        'ALGetFontMetrics',
        'FontFamily: '+ AFontFamily + ' | '+
        'FontSize: '+ ALFloatToStrW(AFontSize, ALDefaultFormatSettingsW) + ' | '+
        'Ascent: ' + ALFloatToStrW(Result.Ascent, ALDefaultFormatSettingsW) + ' | '+
        'Descent: ' + ALFloatToStrW(Result.Descent, ALDefaultFormatSettingsW) + ' | '+
        'Leading: ' + ALFloatToStrW(Result.Leading, ALDefaultFormatSettingsW));
      {$ENDIF}
    end;
  finally
    ALFontMetricsCacheLock.endWrite;
  end;

end;

{******************************************************************}
function ALGetResourceFilename(const AResourceName: String): String;
begin

  if (AResourceName = '') or (ALIsHttpOrHttpsUrl(AResourceName)) then
    exit('');

  {$IFDEF ALDPK}

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
      else if TFile.Exists(Result + '.json') then Result := Result + '.json' // Skottie - Lottie Animation
      else Result := '';
    end;
  end;

  {$ELSE}

  if Assigned(ALCustomGetResourceFilenameProc) then Result := ALCustomGetResourceFilenameProc(AResourceName)
  else If ALposW(PathDelim,AResourceName) > 0 then Result := AResourceName
  else result := '';

  {$ENDIF}

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

{**************************************************************************************************}
Procedure ALMakeBufDrawables(const AControl: TControl; const AEnsureDoubleBuffered: Boolean = True);
begin
  // This ensures the style is retained when the control exits the visible area.
  // Otherwise, the style will be released and reapplied shortly after.
  AControl.DisableDisappear := true;

  if AControl is TALControl then begin
    var LControl := TALControl(AControl);
    if AEnsureDoubleBuffered then
      LControl.DoubleBuffered := true;
    LControl.MakeBufDrawable;
  end;

  for var LChild in aControl.Controls do
    ALMakeBufDrawables(LChild, AEnsureDoubleBuffered);
end;

{*************************************************************************************************************}
function  ALAlignEdgesToPixelRound(const Rect: TRectF; const Scale: single; const Epsilon: Single = 0): TRectF;
begin
  result.left := Round(Rect.left * Scale + Epsilon) / Scale;
  result.right := Round(Rect.right * Scale + Epsilon) / Scale;
  result.top := Round(Rect.top * Scale + Epsilon) / Scale;
  result.bottom := Round(Rect.bottom * Scale + Epsilon) / Scale;
end;

{*****************************************************************************************************************}
function  ALAlignDimensionToPixelRound(const Size: TSizeF; const Scale: single; const Epsilon: Single = 0): TSizeF;
begin
  result.Width := Round(Size.Width * Scale + Epsilon) / Scale;
  result.height := Round(Size.height * Scale + Epsilon) / Scale;
end;

{*****************************************************************************************************************}
function  ALAlignDimensionToPixelRound(const Rect: TRectF; const Scale: single; const Epsilon: Single = 0): TRectF;
begin
  result := Rect;
  result.Width := Round(Rect.Width * Scale + Epsilon) / Scale;
  result.height := Round(Rect.height * Scale + Epsilon) / Scale;
end;

{**********************************************************************************************************************}
function  ALAlignDimensionToPixelRound(const Dimension: single; const Scale: single; const Epsilon: Single = 0): single;
begin
  result := Round(Dimension * Scale + Epsilon) / Scale;
end;

{****************************************************************************************************************}
function  ALAlignDimensionToPixelCeil(const Rect: TRectF; const Scale: single; const Epsilon: Single = 0): TRectF;
begin
  result := Rect;
  result.Width := ceil(Rect.Width * Scale - Epsilon) / Scale;
  result.height := ceil(Rect.height * Scale - Epsilon) / Scale;
end;

{*********************************************************************************************************************}
function  ALAlignDimensionToPixelCeil(const Dimension: single; const Scale: single; const Epsilon: Single = 0): single;
begin
  result := ceil(Dimension * Scale - Epsilon) / Scale;
end;

{*****************************************************************************************************************}
function  ALAlignDimensionToPixelFloor(const Rect: TRectF; const Scale: single; const Epsilon: Single = 0): TRectF;
begin
  result := Rect;
  result.Width := Floor(Rect.Width * Scale + Epsilon) / Scale;
  result.height := Floor(Rect.height * Scale + Epsilon) / Scale;
end;

{**********************************************************************************************************************}
function  ALAlignDimensionToPixelFloor(const Dimension: single; const Scale: single; const Epsilon: Single = 0): single;
begin
  result := Floor(Dimension * Scale + Epsilon) / Scale;
end;

{***********************************************************************************}
// Note: The matrix must be in virtual pixels (i.e., without the scale applied to it)
// This is what is returned by canvas.matrix by default.
function  ALAlignToPixelRound(const Point: TPointF; const Matrix: TMatrix; const Scale: single; const Epsilon: Single = 0): TpointF; overload;
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Graphics.TCanvas.SetMatrix was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if FMX.Graphics.TCanvas.AlignToPixelHorizontally was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if FMX.Graphics.TCanvas.AlignToPixelVertically was not updated and adjust the IFDEF'}
  {$ENDIF}
  // Taken from TCanvas.SetMatrix
  if (not SameValue(Matrix.m11, 1, TEpsilon.Matrix)) or
     (not SameValue(Matrix.m22, 1, TEpsilon.Matrix)) or
     (not SameValue(Matrix.m33, 1, TEpsilon.Matrix)) then begin
    result := Point;
    Exit;
  end;
  // Taken from function TCanvas.AlignToPixelHorizontally(const Value: Single): Single;
  result.x := Round((Matrix.m31 + Point.x) * Scale + Epsilon) / Scale - Matrix.m31;
  // Taken from function TCanvas.AlignToPixelVertically(const Value: Single): Single;
  result.y := Round((Matrix.m32 + Point.y) * Scale + Epsilon) / Scale - Matrix.m32;
end;

{***********************************************************************************}
// Note: The matrix must be in virtual pixels (i.e., without the scale applied to it)
// This is what is returned by canvas.matrix by default.
function  ALAlignToPixelRound(const Rect: TRectF; const Matrix: TMatrix; const Scale: single; const Epsilon: Single = 0): TRectF; overload;
begin
  {$IFNDEF ALCompilerVersionSupported123}
    {$MESSAGE WARN 'Check if FMX.Graphics.TCanvas.SetMatrix was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if FMX.Graphics.TCanvas.AlignToPixelHorizontally was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if FMX.Graphics.TCanvas.AlignToPixelVertically was not updated and adjust the IFDEF'}
    {$MESSAGE WARN 'Check if FMX.Graphics.TCanvas.AlignToPixel was not updated and adjust the IFDEF'}
  {$ENDIF}
  // Taken from TCanvas.SetMatrix
  if (not SameValue(Matrix.m11, 1, TEpsilon.Matrix)) or
     (not SameValue(Matrix.m22, 1, TEpsilon.Matrix)) or
     (not SameValue(Matrix.m33, 1, TEpsilon.Matrix)) then begin
    result := Rect;
    Exit;
  end;
  // Taken from function TCanvas.AlignToPixelHorizontally(const Value: Single): Single;
  Result.Left := Round((Matrix.m31 + Rect.Left) * Scale + Epsilon) / Scale - Matrix.m31;
  // Taken from function TCanvas.AlignToPixelVertically(const Value: Single): Single;
  Result.Top := Round((Matrix.m32 + Rect.Top) * Scale + Epsilon) / Scale - Matrix.m32;
  // Taken from function TCanvas.AlignToPixel(const Rect: TRectF): TRectF;
  Result.Right := Result.Left + Round(Rect.Width * Scale + Epsilon) / Scale; // keep ratio horizontally
  Result.Bottom := Result.Top + Round(Rect.Height * Scale + Epsilon) / Scale; // keep ratio vertically
end;

{***********************************************************************************}
function  ALTextAlignToTextHorzAlign(const ATextAlign: TTextAlign): TALTextHorzAlign;
begin
  case ATextAlign of
    TTextAlign.Center: result := TALTextHorzAlign.Center;
    TTextAlign.Leading: result := TALTextHorzAlign.Leading;
    TTextAlign.Trailing: result := TALTextHorzAlign.Trailing;
    else Raise Exception.Create('Error #9123711A-62FC-47E2-A041-1D7727198CD2')
  end;
end;

{***********************************************************************************}
function  ALTextAlignToTextVertAlign(const ATextAlign: TTextAlign): TALTextVertAlign;
begin
  case ATextAlign of
    TTextAlign.Center: result := TALTextVertAlign.Center;
    TTextAlign.Leading: result := TALTextVertAlign.Leading;
    TTextAlign.Trailing: result := TALTextVertAlign.Trailing;
    else Raise Exception.Create('Error #9123711A-62FC-47E2-A041-1D7727198CD2')
  end;
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
  // The Alcinoe Framework assumes that the screen scale is fixed and uniform,
  // meaning the scale is the same for both width and height, and consistent
  // across all forms.
  //
  // On windows you can change screenscale via
  //   FMX.Platform.Win.TWinWindowHandle.SetForcedScale
  // --
  // On Android you can change screenscale via
  //   FMX.Platform.Screen.Android.SetScreenScaleOverrideHook
  result := ALScreenScale;
  if Result = 0 then begin
    ALInitScreenScale;
    result := ALScreenScale;
  end;
end;

initialization
  ALBrokenImageResourceName := 'broken_image';
  ALBrokenImageWidth := 16;
  ALBrokenImageHeight := 16;
  ALScreenScale := 0;
  ALCustomConvertFontFamilyProc := nil;
  ALCustomGetResourceFilenameProc := nil;
  {$IFDEF ANDROID}
  ALViewStackCount := 0;
  _RenderScript := nil;
  {$ENDIF}
  ALFontMetricsCache := TDictionary<TALFontMetricsKey, TALFontMetrics>.Create;
  //ALFontMetricsCacheLock := ??; their is no TLightweightMREW.create but instead an ugly class operator TLightweightMREW.Initialize :(
  {$IF (not defined(ALSkiaEngine)) and (defined(Android))}
  TALFontManager.FCustomTypeFaces := TDictionary<String, JTypeFace>.Create;
  {$ENDIF}
  TALFont.SansSerifFamily := 'sans-serif';
  TALBaseTextSettings.HorizontalEllipsis := '…';

finalization
  AlFreeAndNil(ALFontMetricsCache);
  {$IF (not defined(ALSkiaEngine)) and (defined(Android))}
  ALFreeAndNil(TALFontManager.FCustomTypeFaces);
  {$ENDIF}

end.
