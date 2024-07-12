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
  FMX.controls,
  ALcinoe.FMX.Controls,
  Alcinoe.FMX.ScrollEngine;

type

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

    // Center the image to the rectangle of the control:
    // * The image is always displayed at its original size (regardless whether the rectangle of the control is larger or smaller than the image size).
    //Center,

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
  TALStrokeBrush = class;
  TALShadow = class;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALScrollableControl = interface
    ['{6750E04D-8DB6-4F27-898A-B20AD55CAAF4}']
    function GetScrollEngine: TALScrollEngine;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALDoubleBufferedControl = interface
    ['{26A0A593-D483-4AE2-881B-6CB930B5E863}']
    function GetDoubleBuffered: boolean;
    procedure SetDoubleBuffered(const AValue: Boolean);
    procedure MakeBufDrawable;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALAutosizeControl = interface
    ['{464CDB70-9F76-4334-8774-5DD98605D6C1}']
    function HasUnconstrainedAutosizeX: boolean;
    function HasUnconstrainedAutosizeY: boolean;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  IALShapeControl = interface
    ['{7F0C732D-88CF-4FE4-ABCB-A990D103D7A8}']
    function GetFill: TALBrush;
    procedure SetFill(const Value: TALBrush);
    function GetStroke: TALStrokeBrush;
    procedure SetStroke(const Value: TALStrokeBrush);
    function GetShadow: TALShadow;
    procedure SetShadow(const Value: TALShadow);
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
    procedure DoChanged; virtual;
  public
    constructor Create; virtual;
    procedure Reset; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure EndUpdateNoChanges; virtual;
    procedure Change; virtual;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALShadow = class(TALPersistentObserver)
  private
    FBlur: Single;
    FOffsetX: Single;
    FOffsetY: Single;
    FColor: TAlphaColor;
    FDefaultBlur: Single;
    FDefaultOffsetX: Single;
    FDefaultOffsetY: Single;
    FDefaultColor: TAlphaColor;
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
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALShadow; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALShadow; const ANormalizedTime: Single);
    function HasShadow: boolean; virtual;
    property Defaultblur: Single read fDefaultblur write fDefaultblur;
    property DefaultOffsetX: Single read fDefaultOffsetX write fDefaultOffsetX;
    property DefaultOffsetY: Single read fDefaultOffsetY write fDefaultOffsetY;
    property DefaultColor: TAlphaColor read fDefaultColor write fDefaultColor;
  published
    property blur: Single read fblur write setblur stored IsblurStored nodefault;
    property OffsetX: Single read fOffsetX write setOffsetX stored IsOffsetXStored nodefault;
    property OffsetY: Single read fOffsetY write setOffsetY stored IsOffsetYStored nodefault;
    property Color: TAlphaColor read fColor write setColor stored IsColorStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritShadow = class(TALShadow)
  private
    FParent: TALShadow;
    FInherit: Boolean;
    fSuperseded: Integer;
    FPriorSupersedeBlur: Single;
    FPriorSupersedeOffsetX: Single;
    FPriorSupersedeOffsetY: Single;
    FPriorSupersedeColor: TAlphaColor;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create(const AParent: TALShadow); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede; virtual;
    procedure Reinstate; virtual;
    procedure SupersedeNoChanges;
    procedure ReinstateNoChanges;
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
    procedure Interpolate(const ATo: TALFont; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALFont; const ANormalizedTime: Single);
    property DefaultFamily: TFontName read FDefaultFamily write FDefaultFamily;
    property DefaultSize: Single read FDefaultSize write FDefaultSize;
    property DefaultWeight: TFontWeight read FDefaultWeight write FDefaultWeight;
    property DefaultSlant: TFontSlant read FDefaultSlant write FDefaultSlant;
    property DefaultStretch: TFontStretch read FDefaultStretch write FDefaultStretch;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
    property DefaultAutoConvert: Boolean read FDefaultAutoConvert write FDefaultAutoConvert;
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored nodefault;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
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
    constructor Create; reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALTextDecoration; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALTextDecoration; const ANormalizedTime: Single);
    property DefaultKinds: TALTextDecorationKinds read FDefaultKinds write FDefaultKinds;
    property DefaultStyle: TALTextDecorationStyle read FDefaultStyle write FDefaultStyle;
    property DefaultThicknessMultiplier: Single read FDefaultThicknessMultiplier write FDefaultThicknessMultiplier;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
  published
    property Kinds: TALTextDecorationKinds read FKinds write SetKinds Stored IsKindsStored;
    property Style: TALTextDecorationStyle read FStyle write SetStyle Stored IsStyleStored;
    property ThicknessMultiplier: Single read FThicknessMultiplier write SetThicknessMultiplier Stored IsThicknessMultiplierStored nodefault;
    property Color: TAlphaColor read FColor write SetColor Stored IsColorStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALEllipsisSettings = class(TALPersistentObserver)
  private
    FInherit: Boolean;
    FFont: TALFont;
    FDecoration: TALTextDecoration;
    FDefaultInherit: Boolean;
    procedure SetInherit(const AValue: Boolean);
    procedure SetFont(const AValue: TALFont);
    procedure SetDecoration(const AValue: TALTextDecoration);
    procedure FontChanged(ASender: TObject);
    procedure DecorationChanged(ASender: TObject);
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALEllipsisSettings; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALEllipsisSettings; const ANormalizedTime: Single);
    property DefaultInherit: Boolean read FDefaultInherit write FDefaultInherit;
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
    procedure Interpolate(const ATo: TALBaseTextSettings; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALBaseTextSettings; const ANormalizedTime: Single);
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
    property Ellipsis: String read FEllipsis write SetEllipsis stored IsEllipsisStored nodefault;
    property EllipsisSettings: TALEllipsisSettings read FEllipsisSettings write SetEllipsisSettings;
    property Trimming: TALTextTrimming read FTrimming write SetTrimming stored IsTrimmingStored;
    property MaxLines: Integer read FMaxLines write SetMaxLines stored IsMaxLinesStored;
    property HorzAlign: TALTextHorzAlign read FHorzAlign write SetHorzAlign stored IsHorzAlignStored;
    property VertAlign: TALTextVertAlign read FVertAlign write SetVertAlign stored IsVertAlignStored;
    property LineHeightMultiplier: Single read FLineHeightMultiplier write SetLineHeightMultiplier stored IsLineHeightMultiplierStored nodefault;
    property LetterSpacing: Single read FLetterSpacing write SetLetterSpacing stored IsLetterSpacingStored nodefault;
    property IsHtml: Boolean read FIsHtml write SetIsHtml stored IsIsHtmlStored;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALTextSettings = class(TALBaseTextSettings)
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

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritBaseTextSettings = class(TALBaseTextSettings)
  private
    FParent: TALBaseTextSettings;
    FInherit: Boolean;
    fSuperseded: Integer;
    FPriorSupersedeFontFamily: TFontName;
    FPriorSupersedeFontSize: Single;
    FPriorSupersedeFontWeight: TFontWeight;
    FPriorSupersedeFontSlant: TFontSlant;
    FPriorSupersedeFontStretch: TFontStretch;
    FPriorSupersedeFontColor: TAlphaColor;
    FPriorSupersedeFontAutoConvert: Boolean;
    FPriorSupersedeDecorationKinds: TALTextDecorationKinds;
    FPriorSupersedeDecorationStyle: TALTextDecorationStyle;
    FPriorSupersedeDecorationThicknessMultiplier: Single;
    FPriorSupersedeDecorationColor: TAlphaColor;
    FPriorSupersedeEllipsis: String;
    FPriorSupersedeEllipsisSettingsInherit: Boolean;
    FPriorSupersedeEllipsisSettingsFontFamily: TFontName;
    FPriorSupersedeEllipsisSettingsFontSize: Single;
    FPriorSupersedeEllipsisSettingsFontWeight: TFontWeight;
    FPriorSupersedeEllipsisSettingsFontSlant: TFontSlant;
    FPriorSupersedeEllipsisSettingsFontStretch: TFontStretch;
    FPriorSupersedeEllipsisSettingsFontColor: TAlphaColor;
    FPriorSupersedeEllipsisSettingsFontAutoConvert: Boolean;
    FPriorSupersedeEllipsisSettingsDecorationKinds: TALTextDecorationKinds;
    FPriorSupersedeEllipsisSettingsDecorationStyle: TALTextDecorationStyle;
    FPriorSupersedeEllipsisSettingsDecorationThicknessMultiplier: Single;
    FPriorSupersedeEllipsisSettingsDecorationColor: TAlphaColor;
    FPriorSupersedeTrimming: TALTextTrimming;
    FPriorSupersedeMaxLines: integer;
    FPriorSupersedeHorzAlign: TALTextHorzAlign;
    FPriorSupersedeVertAlign: TALTextVertAlign;
    FPriorSupersedeLineHeightMultiplier: Single;
    FPriorSupersedeLetterSpacing: Single;
    FPriorSupersedeIsHtml: Boolean;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create(const AParent: TALBaseTextSettings); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede; virtual;
    procedure Reinstate; virtual;
    procedure SupersedeNoChanges;
    procedure ReinstateNoChanges;
    property Parent: TALBaseTextSettings read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALGradient = class(TALPersistentObserver)
  private
    FStyle: TGradientStyle;
    FColors: TArray<TAlphaColor>;
    FOffsets: TArray<Single>;
    FAngle: Single;
    FDefaultStyle: TGradientStyle;
    FDefaultAngle: Single;
    function GetCSSFormat: String;
    procedure SetStyle(const Value: TGradientStyle);
    procedure SetColors(const Value: TArray<TAlphaColor>);
    procedure SetOffsets(const Value: TArray<Single>);
    procedure SetAngle(const Value: Single);
    procedure SetCSSFormat(const Value: String);
    function IsStyleStored: Boolean;
    function IsAngleStored: Boolean;
    procedure ReadColors(Reader: TReader);
    procedure WriteColors(Writer: TWriter);
    procedure ReadOffsets(Reader: TReader);
    procedure WriteOffsets(Writer: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALGradient; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALGradient; const ANormalizedTime: Single);
    property DefaultStyle: TGradientStyle read FDefaultStyle write FDefaultStyle;
    property DefaultAngle: Single read FDefaultAngle write FDefaultAngle;
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
    FColor: TAlphaColor;
    FGradient: TALGradient;
    FResourceName: String;
    FWrapMode: TALImageWrapMode;
    FPadding: TBounds;
    FDefaultColor: TAlphaColor;
    FDefaultResourceName: String;
    FDefaultWrapMode: TALImageWrapMode;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetGradient(const Value: TALGradient);
    procedure SetResourceName(const Value: String);
    procedure SetWrapMode(const Value: TALImageWrapMode);
    procedure SetPadding(const Value: TBounds);
    procedure GradientChanged(Sender: TObject); virtual;
    procedure PaddingChanged(Sender: TObject); virtual;
    function IsColorStored: Boolean;
    function IsResourceNameStored: Boolean;
    function IsWrapModeStored: Boolean;
  {$IF defined(ALBackwardCompatible)}
  private
    procedure ReadKind(Reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  {$ENDIF}
  public
    constructor Create(const ADefaultColor: TAlphaColor); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALBrush; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALBrush; const ANormalizedTime: Single);
    function HasFill: boolean; virtual;
    function Styles: TALBrushStyles; virtual;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
    property DefaultResourceName: String read FDefaultResourceName write FDefaultResourceName;
    property DefaultWrapMode: TALImageWrapMode read FDefaultWrapMode write FDefaultWrapMode;
  published
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
    property Gradient: TALGradient read FGradient write SetGradient;
    property ResourceName: String read FResourceName write SetResourceName stored IsResourceNameStored nodefault;
    property WrapMode: TALImageWrapMode read FWrapMode write SetWrapMode stored IsWrapModeStored;
    property Padding: TBounds read FPadding write SetPadding;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritBrush = class(TALBrush)
  private
    FParent: TALBrush;
    FInherit: Boolean;
    fSuperseded: Integer;
    FPriorSupersedeColor: TAlphaColor;
    FPriorSupersedeGradientStyle: TGradientStyle;
    FPriorSupersedeGradientColors: TArray<TAlphaColor>;
    FPriorSupersedeGradientOffsets: TArray<Single>;
    FPriorSupersedeGradientAngle: Single;
    FPriorSupersedeResourceName: String;
    FPriorSupersedeWrapMode: TALImageWrapMode;
    FPriorSupersedePaddingRect: TRectF;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create(const AParent: TALBrush; const ADefaultColor: TAlphaColor); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede; virtual;
    procedure Reinstate; virtual;
    procedure SupersedeNoChanges;
    procedure ReinstateNoChanges;
    property Parent: TALBrush read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALStrokeBrush = class(TALPersistentObserver)
  private
    FColor: TAlphaColor;
    FThickness: Single;
    FDefaultColor: TAlphaColor;
    FDefaultThickness: Single;
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
  public
    constructor Create(const ADefaultColor: TAlphaColor); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALStrokeBrush; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALStrokeBrush; const ANormalizedTime: Single);
    function HasStroke: boolean; virtual;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
    property DefaultThickness: Single read FDefaultThickness write FDefaultThickness;
  published
    property Color: TAlphaColor read FColor write SetColor stored IsColorStored;
    property Thickness: Single read FThickness write SetThickness stored IsThicknessStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritStrokeBrush = class(TALStrokeBrush)
  private
    FParent: TALStrokeBrush;
    FInherit: Boolean;
    FSuperseded: Integer;
    FPriorSupersedeColor: TAlphaColor;
    FPriorSupersedeThickness: Single;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create(const AParent: TALStrokeBrush; const ADefaultColor: TAlphaColor); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Supersede; virtual;
    procedure Reinstate; virtual;
    procedure SupersedeNoChanges;
    procedure ReinstateNoChanges;
    property Parent: TALStrokeBrush read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALStateLayer = class(TALBrush)
  private
    FOpacity: Single;
    FUseContentColor: Boolean;
    FXRadius: Single;
    FYRadius: Single;
    FDefaultOpacity: Single;
    FDefaultUseContentColor: Boolean;
    FDefaultXRadius: Single;
    FDefaultYRadius: Single;
    procedure SetOpacity(const Value: Single);
    procedure SetUseContentColor(const Value: Boolean);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    function IsOpacityStored: Boolean;
    function IsUseContentColorStored: Boolean;
    function IsXRadiusStored: Boolean;
    function IsYRadiusStored: Boolean;
  public
    constructor Create(const ADefaultColor: TAlphaColor); override;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; override;
    procedure Interpolate(const ATo: TALStateLayer; const ANormalizedTime: Single); reintroduce; virtual;
    procedure InterpolateNoChanges(const ATo: TALStateLayer; const ANormalizedTime: Single);
    function HasFill: boolean; override;
    function Styles: TALBrushStyles; override;
    property DefaultOpacity: Single read FDefaultOpacity write FDefaultOpacity;
    property DefaultUseContentColor: Boolean read FDefaultUseContentColor write FDefaultUseContentColor;
    property DefaultXRadius: Single read FDefaultXRadius write FDefaultXRadius;
    property DefaultYRadius: Single read FDefaultYRadius write FDefaultYRadius;
  published
    property Opacity: Single read FOpacity write SetOpacity stored IsOpacityStored nodefault;
    /// <summary>
    ///   When UseContentColor is true, the color property is ignored, and the
    ///   color is derived from the component content, such as the color of a
    ///   label's text.
    /// </summary>
    property UseContentColor: Boolean read FUseContentColor write SetUseContentColor stored IsUseContentColorStored;
    property XRadius: Single read FXRadius write SetXRadius stored IsXRadiusStored nodefault;
    property YRadius: Single read FYRadius write SetYRadius stored IsYRadiusStored nodefault;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALPosition = class(TPosition)
  public
    procedure Interpolate(const ATo: TPosition; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TPosition; const ANormalizedTime: Single);
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALInheritPosition = class(TALPosition)
  private
    FParent: TPosition;
    FInherit: Boolean;
    FSuperseded: Integer;
    FPriorSupersedeX: Single;
    FPriorSupersedeY: Single;
    procedure SetInherit(const AValue: Boolean);
  public
    constructor Create(const AParent: TPosition; const ADefaultValue: TPointF); reintroduce; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Reset; virtual;
    procedure Supersede; virtual;
    procedure Reinstate; virtual;
    procedure SupersedeNoChanges;
    procedure ReinstateNoChanges;
    property Parent: TPosition read FParent;
  published
    property Inherit: Boolean read FInherit write SetInherit Default True;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBaseStateStyle = class(TALPersistentObserver)
  private
    FStateStyleParent: TALBaseStateStyle;
    FControlParent: TALControl;
    FFill: TALInheritBrush;
    FStateLayer: TALStateLayer;
    FStroke: TALInheritStrokeBrush;
    FShadow: TALInheritShadow;
    FScale: TALInheritPosition;
    FTransitionDuration: Single;
    FDefaultTransitionDuration: Single;
    fSuperseded: Integer;
    procedure SetFill(const AValue: TALInheritBrush);
    procedure SetStateLayer(const AValue: TALStateLayer);
    procedure SetStroke(const AValue: TALInheritStrokeBrush);
    procedure SetShadow(const AValue: TALInheritShadow);
    procedure SetScale(const Value: TALInheritPosition);
    procedure SetTransitionDuration(const Value: Single);
    procedure FillChanged(ASender: TObject);
    procedure StateLayerChanged(ASender: TObject);
    procedure StrokeChanged(ASender: TObject);
    procedure ShadowChanged(ASender: TObject);
    procedure ScaleChanged(ASender: TObject);
    function IsTransitionDurationStored: Boolean;
  protected
    function GetInherit: Boolean; virtual;
    property Fill: TALInheritBrush read FFill write SetFill;
    property Shadow: TALInheritShadow read FShadow write SetShadow;
    property StateLayer: TALStateLayer read FStateLayer write SetStateLayer;
    property Stroke: TALInheritStrokeBrush read FStroke write SetStroke;
    property StateStyleParent: TALBaseStateStyle read FStateStyleParent;
    property ControlParent: TALControl read FControlParent;
    procedure DoSupersede; virtual;
    procedure DoReinstate; virtual;
    property Scale: TALInheritPosition read FScale write SetScale;
    property TransitionDuration: Single read FTransitionDuration write SetTransitionDuration stored IsTransitionDurationStored nodefault;
  public
    constructor Create(const AParent: TObject); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    Property Inherit: Boolean read GetInherit;
    procedure Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single); virtual;
    procedure InterpolateNoChanges(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
    procedure Supersede; virtual;
    procedure Reinstate; virtual;
    procedure SupersedeNoChanges;
    procedure ReinstateNoChanges;
    property DefaultTransitionDuration: Single read FDefaultTransitionDuration write FDefaultTransitionDuration;
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
  public
    class procedure RegisterTypefaceFromResource(const AResourceName: string); static;
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
function  ALGetFontMetrics(
            const AFontFamily: String;
            const AFontSize: single;
            const AFontWeight: TFontWeight;
            const AFontSlant: TFontSlant;
            const AFontColor: TalphaColor;
            const ADecorationKinds: TALTextDecorationKinds): TALFontMetrics;
function  ALGetResourceDirectory: String;
function  ALGetResourceFilename(const AResourceName: String): String;
function  ALTranslate(const AText: string): string;
Procedure ALMakeBufDrawables(const AControl: TControl; const AEnsureDoubleBuffered: Boolean = True);
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
  System.IOutils,
  System.UIConsts,
  FMX.Utils,
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
  System.Hash,
  ToolsAPI,
  {$ENDIF}
  {$IF not defined(ALDPK)}
  Alcinoe.Cipher,
  {$ENDIF}
  {$IF defined(ALDPK)}
  DesignEditors,
  {$ENDIF}
  Alcinoe.FMX.Objects,
  Alcinoe.FMX.StdCtrls,
  Alcinoe.Common,
  Alcinoe.files,
  Alcinoe.stringList,
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
  //--
  FDefaultblur := 12;
  FDefaultOffsetX := 0;
  FDefaultOffsetY := 0;
  FDefaultColor := TAlphaColors.null; // $96000000;
  //--
  Fblur := FDefaultBlur;
  FOffsetX := FDefaultOffsetX;
  FOffsetY := FDefaultOffsetY;
  FColor := FDefaultColor;
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

{*********************************************************************************}
procedure TALShadow.Interpolate(const ATo: TALShadow; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      blur := InterpolateSingle(blur{Start}, ATo.blur{Stop}, ANormalizedTime);
      OffsetX := InterpolateSingle(OffsetX{Start}, ATo.OffsetX{Stop}, ANormalizedTime);
      OffsetY := InterpolateSingle(OffsetY{Start}, ATo.OffsetY{Stop}, ANormalizedTime);
      Color := InterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
    end
    else begin
      blur := InterpolateSingle(blur{Start}, Defaultblur{Stop}, ANormalizedTime);
      OffsetX := InterpolateSingle(OffsetX{Start}, DefaultOffsetX{Stop}, ANormalizedTime);
      OffsetY := InterpolateSingle(OffsetY{Start}, DefaultOffsetY{Stop}, ANormalizedTime);
      Color := InterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
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
            (CompareValue(fBlur, 0, Tepsilon.vector) > 0);
end;

{***************************************}
function TALShadow.IsblurStored: Boolean;
begin
  result := not SameValue(fBlur, FDefaultBlur, Tepsilon.vector);
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

{*************************************}
constructor TALInheritShadow.Create(const AParent: TALShadow);
begin
  inherited create;
  FParent := AParent;
  FInherit := True;
  fSuperseded := 0;
  //FPriorSupersedeBlur
  //FPriorSupersedeOffsetX
  //FPriorSupersedeOffsetY
  //FPriorSupersedeColor
end;

{**********************************************************}
procedure TALInheritShadow.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{****************************************************}
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

{******************************}
procedure TALInheritShadow.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Inherit := True;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritShadow.Supersede;
begin
  beginUpdate;
  try
    if (not inherit) or
       (FParent = nil) then exit;
    inc(fSuperseded);
    if (FSuperseded <> 1) then exit;

    FPriorSupersedeBlur := Blur;
    FPriorSupersedeOffsetX := OffsetX;
    FPriorSupersedeOffsetY := OffsetY;
    FPriorSupersedeColor := Color;
    //--
    var LParentSuperseded := False;
    if FParent is TALInheritShadow then begin
      TALInheritShadow(FParent).SupersedeNoChanges;
      LParentSuperseded := True;
    end;
    try
      Blur := FParent.blur;
      OffsetX := FParent.OffsetX;
      OffsetY := FParent.OffsetY;
      Color := FParent.Color;
    finally
      if LParentSuperseded then
        TALInheritShadow(FParent).ReinstateNoChanges;
    end;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritShadow.Reinstate;
begin
  beginUpdate;
  try
    if fSuperseded <= 0 then exit;
    dec(fSuperseded);
    if fSuperseded = 0 then begin
      Blur := FPriorSupersedeblur;
      OffsetX := FPriorSupersedeOffsetX;
      OffsetY := FPriorSupersedeOffsetY;
      Color := FPriorSupersedeColor;
    end;
  finally
    EndUpdate;
  end;
end;

{*************************}
procedure TALInheritShadow.SupersedeNoChanges;
begin
  BeginUpdate;
  try
    Supersede;
  finally
    EndUpdateNoChanges;
  end;
end;

{*************************}
procedure TALInheritShadow.ReinstateNoChanges;
begin
  BeginUpdate;
  try
    Reinstate;
  finally
    EndUpdateNoChanges;
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
    ALAssignError(Source{ASource}, Self{ADest});
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
      Color := InterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      AutoConvert := ATo.AutoConvert;
    end
    else begin
      Family := DefaultFamily;
      Size := InterpolateSingle(Size{Start}, DefaultSize{Stop}, ANormalizedTime);
      //TFontWeight = (Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack)
      Weight := TFontWeight(round(InterpolateSingle(integer(Weight), integer(DefaultWeight), ANormalizedTime)));
      Slant := DefaultSlant;
      //TFontStretch = (UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, Regular, SemiExpanded, Expanded, ExtraExpanded, UltraExpanded)
      Stretch := TFontStretch(round(InterpolateSingle(integer(Stretch), integer(DefaultStretch), ANormalizedTime)));
      Color := InterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      AutoConvert := DefaultAutoConvert;
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
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
    ALAssignError(Source{ASource}, Self{ADest});
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
        Color := InterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
    end
    else begin
      Kinds := DefaultKinds;
      Style := DefaultStyle;
      ThicknessMultiplier := InterpolateSingle(ThicknessMultiplier{Start}, DefaultThicknessMultiplier{Stop}, ANormalizedTime);
      if (DefaultColor = TalphaColors.Null) or
         (Color = TalphaColors.Null) then
        Color := DefaultColor
      else
        Color := InterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
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
  Result := not SameValue(FThicknessMultiplier, FDefaultThicknessMultiplier, TEpsilon.Scale);
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
  FDefaultInherit := True;
  FInherit := FDefaultInherit;
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
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**********************************}
procedure TALEllipsisSettings.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Inherit := DefaultInherit;
    Font.reset;
    Decoration.reset;
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

{*********************************************************************************}
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
  //--
  FFont := TALFont.Create;
  FFont.OnChanged := FontChanged;
  FDecoration := TALTextDecoration.Create;
  FDecoration.OnChanged := DecorationChanged;
  FEllipsisSettings := TALEllipsisSettings.create;
  FEllipsisSettings.OnChanged := EllipsisSettingsChanged;
  //--
  FDefaultEllipsis := '…';
  FDefaultTrimming := TALTextTrimming.Word;
  FDefaultMaxLines := 65535;
  FDefaultHorzAlign := TALTextHorzAlign.Leading;
  FDefaultVertAlign := TALTextVertAlign.Center;
  FDefaultLineHeightMultiplier := 0;
  FDefaultLetterSpacing := 0;
  FDefaultIsHtml := False;
  //--
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
    ALAssignError(Source{ASource}, Self{ADest});
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

{*********************************************************************************}
procedure TALBaseTextSettings.Interpolate(const ATo: TALBaseTextSettings; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Font.Interpolate(ATo.Font, ANormalizedTime);
      Decoration.Interpolate(ATo.Decoration, ANormalizedTime);
      EllipsisSettings.Interpolate(ATo.EllipsisSettings, ANormalizedTime);
      Ellipsis := ATo.Ellipsis;
      Trimming := ATo.Trimming;
      MaxLines := DefaultMaxLines;
      HorzAlign := ATo.HorzAlign;
      VertAlign := ATo.VertAlign;
      LineHeightMultiplier := InterpolateSingle(LineHeightMultiplier{Start}, ATo.LineHeightMultiplier{Stop}, ANormalizedTime);
      LetterSpacing := InterpolateSingle(LetterSpacing{Start}, ATo.LetterSpacing{Stop}, ANormalizedTime);
      IsHtml := ATo.IsHtml;
    end
    else begin
      Font.Interpolate(nil, ANormalizedTime);
      Decoration.Interpolate(nil, ANormalizedTime);
      EllipsisSettings.Interpolate(nil, ANormalizedTime);
      Ellipsis := DefaultEllipsis;
      Trimming := DefaultTrimming;
      MaxLines := DefaultMaxLines;
      HorzAlign := DefaultHorzAlign;
      VertAlign := DefaultVertAlign;
      LineHeightMultiplier := InterpolateSingle(LineHeightMultiplier{Start}, DefaultLineHeightMultiplier{Stop}, ANormalizedTime);
      LetterSpacing := InterpolateSingle(LetterSpacing{Start}, DefaultLetterSpacing{Stop}, ANormalizedTime);
      IsHtml := DefaultIsHtml;
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
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

{*************************************}
constructor TALInheritBaseTextSettings.Create(const AParent: TALBaseTextSettings);
begin
  inherited create;
  FParent := AParent;
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

  fSuperseded := 0;
  //FPriorSupersedeFontFamily
  //FPriorSupersedeFontSize
  //FPriorSupersedeFontWeight
  //FPriorSupersedeFontSlant
  //FPriorSupersedeFontStretch
  //FPriorSupersedeFontColor
  //FPriorSupersedeFontAutoConvert
  //FPriorSupersedeDecorationKinds
  //FPriorSupersedeDecorationStyle
  //FPriorSupersedeDecorationThicknessMultiplier
  //FPriorSupersedeDecorationColor
  //FPriorSupersedeEllipsis: String;
  //FPriorSupersedeEllipsisSettingsInherit
  //FPriorSupersedeEllipsisSettingsFontFamily
  //FPriorSupersedeEllipsisSettingsFontSize
  //FPriorSupersedeEllipsisSettingsFontWeight
  //FPriorSupersedeEllipsisSettingsFontSlant
  //FPriorSupersedeEllipsisSettingsFontStretch
  //FPriorSupersedeEllipsisSettingsFontColor
  //FPriorSupersedeEllipsisSettingsFontAutoConvert
  //FPriorSupersedeEllipsisSettingsDecorationKinds
  //FPriorSupersedeEllipsisSettingsDecorationStyle
  //FPriorSupersedeEllipsisSettingsDecorationThicknessMultiplier
  //FPriorSupersedeEllipsisSettingsDecorationColor
  //FPriorSupersedeTrimming: TALTextTrimming;
  //FPriorSupersedeMaxLines: integer;
  //FPriorSupersedeHorzAlign: TALTextHorzAlign;
  //FPriorSupersedeVertAlign: TALTextVertAlign;
  //FPriorSupersedeLineHeightMultiplier
  //FPriorSupersedeLetterSpacing
  //FPriorSupersedeIsHtml
end;

{**********************************************************}
procedure TALInheritBaseTextSettings.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{****************************************************}
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

{******************************}
procedure TALInheritBaseTextSettings.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Inherit := True;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritBaseTextSettings.Supersede;
begin
  beginUpdate;
  try
    if (FParent = nil) then exit;
    inc(fSuperseded);
    if (FSuperseded <> 1) then exit;

    FPriorSupersedeFontFamily := Font.Family;
    FPriorSupersedeFontSize := Font.Size;
    FPriorSupersedeFontWeight := Font.Weight;
    FPriorSupersedeFontSlant := Font.Slant;
    FPriorSupersedeFontStretch := Font.Stretch;
    FPriorSupersedeFontColor := Font.Color;
    FPriorSupersedeFontAutoConvert := Font.AutoConvert;
    FPriorSupersedeDecorationKinds := Decoration.Kinds;
    FPriorSupersedeDecorationStyle := Decoration.Style;
    FPriorSupersedeDecorationThicknessMultiplier := Decoration.ThicknessMultiplier;
    FPriorSupersedeDecorationColor := Decoration.Color;
    FPriorSupersedeEllipsis := Ellipsis;
    FPriorSupersedeEllipsisSettingsInherit := EllipsisSettings.Inherit;
    FPriorSupersedeEllipsisSettingsFontFamily := EllipsisSettings.Font.Family;
    FPriorSupersedeEllipsisSettingsFontSize := EllipsisSettings.Font.Size;
    FPriorSupersedeEllipsisSettingsFontWeight := EllipsisSettings.Font.Weight;
    FPriorSupersedeEllipsisSettingsFontSlant := EllipsisSettings.Font.Slant;
    FPriorSupersedeEllipsisSettingsFontStretch := EllipsisSettings.Font.Stretch;
    FPriorSupersedeEllipsisSettingsFontColor := EllipsisSettings.Font.Color;
    FPriorSupersedeEllipsisSettingsFontAutoConvert := EllipsisSettings.Font.AutoConvert;
    FPriorSupersedeEllipsisSettingsDecorationKinds := EllipsisSettings.Decoration.Kinds;
    FPriorSupersedeEllipsisSettingsDecorationStyle := EllipsisSettings.Decoration.Style;
    FPriorSupersedeEllipsisSettingsDecorationThicknessMultiplier := EllipsisSettings.Decoration.ThicknessMultiplier;
    FPriorSupersedeEllipsisSettingsDecorationColor := EllipsisSettings.Decoration.Color;
    FPriorSupersedeTrimming := Trimming;
    FPriorSupersedeMaxLines := MaxLines;
    FPriorSupersedeHorzAlign := HorzAlign;
    FPriorSupersedeVertAlign := VertAlign;
    FPriorSupersedeLineHeightMultiplier := LineHeightMultiplier;
    FPriorSupersedeLetterSpacing := LetterSpacing;
    FPriorSupersedeIsHtml := IsHtml;
    //--
    var LParentSuperseded := False;
    if FParent is TALInheritBaseTextSettings then begin
      TALInheritBaseTextSettings(FParent).SupersedeNoChanges;
      LParentSuperseded := True;
    end;
    try
      if inherit then begin
        Font.Family := FParent.Font.Family;
        Font.Size := FParent.Font.Size;
        Font.Weight := FParent.Font.Weight;
        Font.Slant := FParent.Font.Slant;
        Font.Stretch := FParent.Font.Stretch;
        Font.Color := FParent.Font.Color;
        Font.AutoConvert := FParent.Font.AutoConvert;
        Decoration.Kinds := FParent.Decoration.Kinds;
        Decoration.Style := FParent.Decoration.Style;
        Decoration.ThicknessMultiplier := FParent.Decoration.ThicknessMultiplier;
        Decoration.Color := FParent.Decoration.Color;
        Ellipsis := FParent.Ellipsis;
        EllipsisSettings.Inherit := FParent.EllipsisSettings.Inherit;
        EllipsisSettings.Font.Family := FParent.EllipsisSettings.Font.Family;
        EllipsisSettings.Font.Size := FParent.EllipsisSettings.Font.Size;
        EllipsisSettings.Font.Weight := FParent.EllipsisSettings.Font.Weight;
        EllipsisSettings.Font.Slant := FParent.EllipsisSettings.Font.Slant;
        EllipsisSettings.Font.Stretch := FParent.EllipsisSettings.Font.Stretch;
        EllipsisSettings.Font.Color := FParent.EllipsisSettings.Font.Color;
        EllipsisSettings.Font.AutoConvert := FParent.EllipsisSettings.Font.AutoConvert;
        EllipsisSettings.Decoration.Kinds := FParent.EllipsisSettings.Decoration.Kinds;
        EllipsisSettings.Decoration.Style := FParent.EllipsisSettings.Decoration.Style;
        EllipsisSettings.Decoration.ThicknessMultiplier := FParent.EllipsisSettings.Decoration.ThicknessMultiplier;
        EllipsisSettings.Decoration.Color := FParent.EllipsisSettings.Decoration.Color;
        Trimming := FParent.Trimming;
        MaxLines := FParent.MaxLines;
        HorzAlign := FParent.HorzAlign;
        VertAlign := FParent.VertAlign;
        LineHeightMultiplier := FParent.LineHeightMultiplier;
        LetterSpacing := FParent.LetterSpacing;
        IsHtml := FParent.IsHtml;
      end
      else begin
        (* todo delete *)
        if Font.Family = '' then Font.Family := FParent.Font.Family;
        if SameValue(Font.Size, 0, TEpsilon.FontSize) then Font.Size := FParent.Font.Size;
        if Font.Color = TAlphaColors.Null then Font.Color := FParent.Font.Color;
      end;
    finally
      if LParentSuperseded then
        TALInheritBaseTextSettings(FParent).ReinstateNoChanges;
    end;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritBaseTextSettings.Reinstate;
begin
  beginUpdate;
  try
    if fSuperseded <= 0 then exit;
    dec(fSuperseded);
    if fSuperseded = 0 then begin
      Font.Family := FPriorSupersedeFontFamily;
      Font.Size := FPriorSupersedeFontSize;
      Font.Weight := FPriorSupersedeFontWeight;
      Font.Slant := FPriorSupersedeFontSlant;
      Font.Stretch := FPriorSupersedeFontStretch;
      Font.Color := FPriorSupersedeFontColor;
      Font.AutoConvert := FPriorSupersedeFontAutoConvert;
      Decoration.Kinds := FPriorSupersedeDecorationKinds;
      Decoration.Style := FPriorSupersedeDecorationStyle;
      Decoration.ThicknessMultiplier := FPriorSupersedeDecorationThicknessMultiplier;
      Decoration.Color := FPriorSupersedeDecorationColor;
      Ellipsis := FPriorSupersedeEllipsis;
      EllipsisSettings.Inherit := FPriorSupersedeEllipsisSettingsInherit;
      EllipsisSettings.Font.Family := FPriorSupersedeEllipsisSettingsFontFamily;
      EllipsisSettings.Font.Size := FPriorSupersedeEllipsisSettingsFontSize;
      EllipsisSettings.Font.Weight := FPriorSupersedeEllipsisSettingsFontWeight;
      EllipsisSettings.Font.Slant := FPriorSupersedeEllipsisSettingsFontSlant;
      EllipsisSettings.Font.Stretch := FPriorSupersedeEllipsisSettingsFontStretch;
      EllipsisSettings.Font.Color := FPriorSupersedeEllipsisSettingsFontColor;
      EllipsisSettings.Font.AutoConvert := FPriorSupersedeEllipsisSettingsFontAutoConvert;
      EllipsisSettings.Decoration.Kinds := FPriorSupersedeEllipsisSettingsDecorationKinds;
      EllipsisSettings.Decoration.Style := FPriorSupersedeEllipsisSettingsDecorationStyle;
      EllipsisSettings.Decoration.ThicknessMultiplier := FPriorSupersedeEllipsisSettingsDecorationThicknessMultiplier;
      EllipsisSettings.Decoration.Color := FPriorSupersedeEllipsisSettingsDecorationColor;
      Trimming := FPriorSupersedeTrimming;
      MaxLines := FPriorSupersedeMaxLines;
      HorzAlign := FPriorSupersedeHorzAlign;
      VertAlign := FPriorSupersedeVertAlign;
      LineHeightMultiplier := FPriorSupersedeLineHeightMultiplier;
      LetterSpacing := FPriorSupersedeLetterSpacing;
      IsHtml := FPriorSupersedeIsHtml;
    end;
  finally
    EndUpdate;
  end;
end;

{*************************}
procedure TALInheritBaseTextSettings.SupersedeNoChanges;
begin
  BeginUpdate;
  try
    Supersede;
  finally
    EndUpdateNoChanges;
  end;
end;

{*************************}
procedure TALInheritBaseTextSettings.ReinstateNoChanges;
begin
  BeginUpdate;
  try
    Reinstate;
  finally
    EndUpdateNoChanges;
  end;
end;

{*****************************}
constructor TALGradient.Create;
begin
  inherited Create;
  //--
  FDefaultStyle := TGradientStyle.Linear;
  FDefaultAngle := 180;
  //--
  FStyle := FDefaultStyle;
  FColors := [];
  FOffsets := [];
  FAngle := FDefaultAngle;
end;

{**********************************************}
procedure TALGradient.Assign(Source: TPersistent);
begin
  if Source is TALGradient then begin
    BeginUpdate;
    Try
      Style := TALGradient(Source).Style;
      Colors := TALGradient(Source).Colors;
      Offsets := TALGradient(Source).Offsets;
      Angle := TALGradient(Source).Angle;
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
    inherited Reset;
    Style := DefaultStyle;
    Colors := [];
    Offsets := [];
    Angle := DefaultAngle;
  finally
    EndUpdate;
  end;
end;

{**************************}
procedure TALGradient.Interpolate(const ATo: TALGradient; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      if (Style = ATo.Style) and
         (length(Colors) = length(ATo.Colors)) and
         (length(Offsets) = length(ATo.Offsets)) then begin
        Style := ATo.Style;
        for var I := Low(Colors) to High(Colors) do
          Colors[i] := InterpolateColor(Colors[i]{Start}, ATo.Colors[i]{Stop}, ANormalizedTime);
        for var I := Low(Offsets) to High(Offsets) do
          Offsets[i] := InterpolateSingle(Offsets[i]{Start}, ATo.Offsets[i]{Stop}, ANormalizedTime);
        Angle := InterpolateSingle(Angle{Start}, ATo.Angle{Stop}, ANormalizedTime);
      end
      else begin
        Style := ATo.Style;
        Colors := ATo.Colors;
        Offsets := ATo.Offsets;
        Angle := ATo.Angle;
      end;
    end
    else begin
      Style := DefaultStyle;
      Colors := [];
      Offsets := [];
      Angle := DefaultAngle;
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
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

{*************************************************}
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

{*****************************************}
function TALGradient.IsStyleStored: Boolean;
begin
  result := FStyle <> FDefaultStyle;
end;

{******************************************}
function TALGradient.IsAngleStored: Boolean;
begin
  result := Not sameValue(FAngle, FDefaultAngle, TEpsilon.Angle);
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

{****************************************************}
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

{**************************************************}
procedure TALGradient.SetAngle(const Value: Single);
begin
  if fAngle <> Value then begin
    fAngle := Value;
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

{**********************************************************************************************}
constructor TALBrush.Create(const ADefaultColor: TAlphaColor);
begin
  inherited Create;
  //--
  FDefaultColor := ADefaultColor;
  FDefaultResourceName := '';
  FDefaultWrapMode := TALImageWrapMode.Fit;
  //--
  FColor := FDefaultColor;
  FResourceName := FDefaultResourceName;
  FWrapMode := FDefaultWrapMode;
  //--
  FGradient := TALGradient.Create;
  FGradient.OnChanged := GradientChanged;
  //--
  FPadding := TBounds.Create(TRectF.Empty);
  FPadding.OnChange := PaddingChanged;
end;

{**************************}
destructor TALBrush.Destroy;
begin
  ALFreeAndNil(FGradient);
  ALFreeAndNil(FPadding);
  inherited;
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

{**********************************************}
procedure TALBrush.Assign(Source: TPersistent);
begin
  if Source is TALBrush then begin
    BeginUpdate;
    Try
      Color := TALBrush(Source).Color;
      Gradient.Assign(TALBrush(Source).Gradient);
      ResourceName := TALBrush(Source).ResourceName;
      WrapMode := TALBrush(Source).WrapMode;
      Padding.Assign(TALBrush(Source).Padding);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{************************}
procedure TALBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Color := DefaultColor;
    Gradient.Reset;
    ResourceName := DefaultResourceName;
    WrapMode := DefaultWrapMode;
    Padding.Rect := Padding.DefaultValue;
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
      Color := InterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      Gradient.Interpolate(aTo.Gradient, ANormalizedTime);
      ResourceName := ATo.ResourceName;
      WrapMode := ATo.WrapMode;
      Padding.Left := InterpolateSingle(Padding.Left{Start}, ATo.Padding.Left{Stop}, ANormalizedTime);
      Padding.Right := InterpolateSingle(Padding.Right{Start}, ATo.Padding.Right{Stop}, ANormalizedTime);
      Padding.Top := InterpolateSingle(Padding.Top{Start}, ATo.Padding.Top{Stop}, ANormalizedTime);
      Padding.Bottom := InterpolateSingle(Padding.Bottom{Start}, ATo.Padding.Bottom{Stop}, ANormalizedTime);
    end
    else begin
      Color := InterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      Gradient.Interpolate(nil, ANormalizedTime);
      ResourceName := DefaultResourceName;
      WrapMode := DefaultWrapMode;
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

{***************************************}
function TALBrush.IsColorStored: Boolean;
begin
  result := FColor <> FDefaultColor;
end;

{**********************************************}
function TALBrush.IsResourceNameStored: Boolean;
begin
  result := FResourceName <> FDefaultResourceName;
end;

{**********************************************}
function TALBrush.IsWrapModeStored: Boolean;
begin
  result := FWrapMode <> FDefaultWrapMode;
end;

{****************************************************}
procedure TALBrush.SetColor(const Value: TAlphaColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    Change;
  end;
end;

{****************************************************}
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

{******************************************************}
procedure TALBrush.SetWrapMode(const Value: TALImageWrapMode);
begin
  if fWrapMode <> Value then begin
    fWrapMode := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALBrush.SetPadding(const Value: TBounds);
begin
  FPadding.Assign(Value);
end;

{**************************************************}
procedure TALBrush.GradientChanged(Sender: TObject);
begin
  change;
end;

{*************************************************}
procedure TALBrush.PaddingChanged(Sender: TObject);
begin
  change;
end;

{***************************************************************************************************}
constructor TALInheritBrush.Create(const AParent: TALBrush; const ADefaultColor: TAlphaColor);
begin
  inherited create(ADefaultColor);
  FParent := AParent;
  FInherit := True;
  fSuperseded := 0;
  //FPriorSupersedeColor
  //FPriorSupersedeGradientStyle
  //FPriorSupersedeGradientColors
  //FPriorSupersedeGradientOffsets
  //FPriorSupersedeGradientAngle
  //FPriorSupersedeResourceName
  //FPriorSupersedeWrapMode
  //FPriorSupersedePaddingRect
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
    if Source is TALInheritBrush then
      Inherit := TALInheritBrush(Source).Inherit
    else
      Inherit := False;
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
    inherited Reset;
    Inherit := True;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritBrush.Supersede;
begin
  beginUpdate;
  try
    if (not inherit) or
       (FParent = nil) then exit;
    inc(fSuperseded);
    if (FSuperseded <> 1) then exit;

    FPriorSupersedeColor := Color;
    FPriorSupersedeGradientStyle := Gradient.Style;
    FPriorSupersedeGradientColors := Gradient.Colors;
    FPriorSupersedeGradientOffsets := Gradient.Offsets;
    FPriorSupersedeGradientAngle := Gradient.Angle;
    FPriorSupersedeResourceName := ResourceName;
    FPriorSupersedeWrapMode := WrapMode;
    FPriorSupersedePaddingRect := Padding.Rect;
    //--
    var LParentSuperseded := False;
    if FParent is TALInheritBrush then begin
      TALInheritBrush(FParent).SupersedeNoChanges;
      LParentSuperseded := True;
    end;
    try
      Color := FParent.Color;
      Gradient.Style := FParent.Gradient.Style;
      Gradient.Colors := FParent.Gradient.Colors;
      Gradient.Offsets := FParent.Gradient.Offsets;
      Gradient.Angle := FParent.Gradient.Angle;
      ResourceName := FParent.ResourceName;
      WrapMode := FParent.WrapMode;
      Padding.Rect := FParent.Padding.Rect;
    finally
      if LParentSuperseded then
        TALInheritBrush(FParent).ReinstateNoChanges;
    end;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritBrush.Reinstate;
begin
  beginUpdate;
  try
    if fSuperseded <= 0 then exit;
    dec(fSuperseded);
    if fSuperseded = 0 then begin
      Color := FPriorSupersedeColor;
      Gradient.Style := FPriorSupersedeGradientStyle;
      Gradient.Colors := FPriorSupersedeGradientColors;
      Gradient.Offsets := FPriorSupersedeGradientOffsets;
      Gradient.Angle := FPriorSupersedeGradientAngle;
      ResourceName := FPriorSupersedeResourceName;
      WrapMode := FPriorSupersedeWrapMode;
      Padding.Rect := FPriorSupersedePaddingRect;
    end;
  finally
    EndUpdate;
  end;
end;

{*************************}
procedure TALInheritBrush.SupersedeNoChanges;
begin
  BeginUpdate;
  try
    Supersede;
  finally
    EndUpdateNoChanges;
  end;
end;

{*************************}
procedure TALInheritBrush.ReinstateNoChanges;
begin
  BeginUpdate;
  try
    Reinstate;
  finally
    EndUpdateNoChanges;
  end;
end;

{**********************************************************************************************}
constructor TALStrokeBrush.Create(const ADefaultColor: TAlphaColor);
begin
  inherited Create;
  //--
  FDefaultColor := ADefaultColor;
  FDefaultThickness := 1;
  //--
  FColor := FDefaultColor;
  FThickness := FDefaultThickness;
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

{**********************************************}
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

{************************}
procedure TALStrokeBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Color := DefaultColor;
    Thickness := DefaultThickness;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
procedure TALStrokeBrush.Interpolate(const ATo: TALStrokeBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Color := InterpolateColor(Color{Start}, ATo.Color{Stop}, ANormalizedTime);
      Thickness := InterpolateSingle(Thickness{Start}, ATo.Thickness{Stop}, ANormalizedTime);
    end
    else begin
      Color := InterpolateColor(Color{Start}, DefaultColor{Stop}, ANormalizedTime);
      Thickness := InterpolateSingle(Thickness{Start}, DefaultThickness{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
procedure TALStrokeBrush.InterpolateNoChanges(const ATo: TALStrokeBrush; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{************************}
function TALStrokeBrush.HasStroke: boolean;
begin
  result := (Color <> TalphaColors.Null) and
            (CompareValue(FThickness, 0, TEpsilon.Vector) > 0);
end;

{***************************************}
function TALStrokeBrush.IsColorStored: Boolean;
begin
  result := FColor <> FDefaultColor;
end;

{**********************************************}
function TALStrokeBrush.IsThicknessStored: Boolean;
begin
  result := not SameValue(FThickness, FDefaultThickness, TEpsilon.Vector);
end;

{****************************************************}
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

{*************************************}
constructor TALInheritStrokeBrush.Create(const AParent: TALStrokeBrush; const ADefaultColor: TAlphaColor);
begin
  inherited create(ADefaultColor);
  FParent := AParent;
  FInherit := True;
  FSuperseded := 0;
  //FPriorSupersedeColor
  //FPriorSupersedeThickness
end;

{**********************************************************}
procedure TALInheritStrokeBrush.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    Change;
  end;
end;

{****************************************************}
procedure TALInheritStrokeBrush.Assign(Source: TPersistent);
begin
  BeginUpdate;
  Try
    if Source is TALInheritStrokeBrush then
      Inherit := TALInheritStrokeBrush(Source).Inherit
    else
      Inherit := False;
    inherited Assign(Source);
  Finally
    EndUpdate;
  End;
end;

{******************************}
procedure TALInheritStrokeBrush.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Inherit := True;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritStrokeBrush.Supersede;
begin
  beginUpdate;
  try
    if (not inherit) or
       (FParent = nil) then exit;
    inc(fSuperseded);
    if (FSuperseded <> 1) then exit;

    FPriorSupersedeColor := Color;
    FPriorSupersedeThickness := Thickness;
    //--
    var LParentSuperseded := False;
    if FParent is TALInheritStrokeBrush then begin
      TALInheritStrokeBrush(FParent).SupersedeNoChanges;
      LParentSuperseded := True;
    end;
    try
      Color := FParent.Color;
      Thickness := FParent.Thickness;
    finally
      if LParentSuperseded then
        TALInheritStrokeBrush(FParent).ReinstateNoChanges;
    end;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALInheritStrokeBrush.Reinstate;
begin
  beginUpdate;
  try
    if fSuperseded <= 0 then exit;
    dec(fSuperseded);
    if fSuperseded = 0 then begin
      Color := FPriorSupersedeColor;
      Thickness := FPriorSupersedeThickness;
    end;
  finally
    EndUpdate;
  end;
end;

{*************************}
procedure TALInheritStrokeBrush.SupersedeNoChanges;
begin
  BeginUpdate;
  try
    Supersede;
  finally
    EndUpdateNoChanges;
  end;
end;

{*************************}
procedure TALInheritStrokeBrush.ReinstateNoChanges;
begin
  BeginUpdate;
  try
    Reinstate;
  finally
    EndUpdateNoChanges;
  end;
end;

{**************************************************}
constructor TALStateLayer.Create(const ADefaultColor: TAlphaColor);
begin
  inherited Create(ADefaultColor);
  //--
  FDefaultOpacity := 0;
  FDefaultUseContentColor := True;
  FDefaultXRadius := 0;
  FDefaultYRadius := 0;
  //--
  FOpacity := FDefaultOpacity;
  FUseContentColor := FDefaultUseContentColor;
  FXRadius := FDefaultXRadius;
  FYRadius := FDefaultYRadius;
end;

{**************************************************}
procedure TALStateLayer.Assign(Source: TPersistent);
begin
  if Source is TALStateLayer then begin
    BeginUpdate;
    Try
      Opacity := TALStateLayer(Source).Opacity;
      UseContentColor := TALStateLayer(Source).UseContentColor;
      XRadius := TALStateLayer(Source).XRadius;
      YRadius := TALStateLayer(Source).YRadius;
      inherited Assign(Source);
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{**************************************************}
procedure TALStateLayer.Reset;
begin
  BeginUpdate;
  Try
    inherited Reset;
    Opacity := DefaultOpacity;
    UseContentColor := DefaultUseContentColor;
    XRadius := DefaultXRadius;
    YRadius := DefaultYRadius;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
procedure TALStateLayer.Interpolate(const ATo: TALStateLayer; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    inherited Interpolate(ATo, ANormalizedTime);
    if ATo <> nil then begin
      Opacity := InterpolateSingle(Opacity{Start}, ATo.Opacity{Stop}, ANormalizedTime);
      UseContentColor := ATo.UseContentColor;
      XRadius := InterpolateSingle(XRadius{Start}, ATo.XRadius{Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, ATo.YRadius{Stop}, ANormalizedTime);
    end
    else begin
      Opacity := InterpolateSingle(Opacity{Start}, DefaultOpacity{Stop}, ANormalizedTime);
      UseContentColor := DefaultUseContentColor;
      XRadius := InterpolateSingle(XRadius{Start}, DefaultXRadius{Stop}, ANormalizedTime);
      YRadius := InterpolateSingle(YRadius{Start}, DefaultYRadius{Stop}, ANormalizedTime);
    end;
  finally
    EndUpdate;
  end;
end;

{*********************************************************************************}
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
  Result := (Inherited HasFill) and
            (CompareValue(Opacity, 0, TEpsilon.Scale) > 0);
end;

{********************************************}
function TALStateLayer.Styles: TALBrushStyles;
begin
  Result := inherited Styles;
  if UseContentColor then result := result + [TALBrushStyle.Solid];
end;

{**************************************************}
procedure TALStateLayer.SetOpacity(const Value: Single);
begin
  if not SameValue(FOpacity, Value, TEpsilon.Scale) then begin
    FOpacity := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALStateLayer.SetUseContentColor(const Value: Boolean);
begin
  if FUseContentColor <> Value then begin
    FUseContentColor := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALStateLayer.SetXRadius(const Value: Single);
begin
  if not SameValue(FXRadius, Value, TEpsilon.Vector) then begin
    FXRadius := Value;
    Change;
  end;
end;

{**************************************************}
procedure TALStateLayer.SetYRadius(const Value: Single);
begin
  if not SameValue(FYRadius, Value, TEpsilon.Vector) then begin
    FYRadius := Value;
    Change;
  end;
end;

{**************************************************}
function TALStateLayer.IsOpacityStored: Boolean;
begin
  Result := not SameValue(FOpacity, FDefaultOpacity, TEpsilon.Scale);
end;

{**************************************************}
function TALStateLayer.IsUseContentColorStored: Boolean;
begin
  Result := FUseContentColor <> FDefaultUseContentColor;
end;

{**************************************************}
function TALStateLayer.IsXRadiusStored: Boolean;
begin
  Result := not SameValue(FXRadius, FDefaultXRadius, TEpsilon.Vector);
end;

{**************************************************}
function TALStateLayer.IsYRadiusStored: Boolean;
begin
  Result := not SameValue(FYRadius, FDefaultYRadius, TEpsilon.Vector);
end;

{*********************************************************************************}
procedure TALPosition.Interpolate(const ATo: TPosition; const ANormalizedTime: Single);
begin
  if ATo <> nil then
    Point := TpointF.Create(
               InterpolateSingle(X{Start}, ATo.X{Stop}, ANormalizedTime),
               InterpolateSingle(Y{Start}, ATo.Y{Stop}, ANormalizedTime))
  else
    Point := TpointF.Create(
               InterpolateSingle(X{Start}, DefaultValue.X{Stop}, ANormalizedTime),
               InterpolateSingle(Y{Start}, DefaultValue.Y{Stop}, ANormalizedTime));
end;

{*********************************************************************************}
procedure TALPosition.InterpolateNoChanges(const ATo: TPosition; const ANormalizedTime: Single);
begin
  if ATo <> nil then
    SetPointNoChange(
      TpointF.Create(
        InterpolateSingle(X{Start}, ATo.X{Stop}, ANormalizedTime),
        InterpolateSingle(Y{Start}, ATo.Y{Stop}, ANormalizedTime)))
  else
    SetPointNoChange(
      TpointF.Create(
        InterpolateSingle(X{Start}, DefaultValue.X{Stop}, ANormalizedTime),
        InterpolateSingle(Y{Start}, DefaultValue.Y{Stop}, ANormalizedTime)));
end;

{*************************************}
constructor TALInheritPosition.Create(const AParent: TPosition; const ADefaultValue: TPointF);
begin
  inherited create(ADefaultValue);
  FParent := AParent;
  FInherit := True;
  FSuperseded := 0;
  //FPriorSupersedeX
  //FPriorSupersedeY
end;

{**********************************************************}
procedure TALInheritPosition.SetInherit(const AValue: Boolean);
begin
  If FInherit <> AValue then begin
    FInherit := AValue;
    DoChange;
  end;
end;

{****************************************************}
procedure TALInheritPosition.Assign(Source: TPersistent);
begin
  if Source is TALInheritPosition then
    Inherit := TALInheritPosition(Source).Inherit
  else
    Inherit := False;
  inherited Assign(Source);
end;

{******************************}
procedure TALInheritPosition.Reset;
begin
  Point := DefaultValue;
  Inherit := True;
end;

{******************}
procedure TALInheritPosition.Supersede;
begin
  if (not inherit) or
     (FParent = nil) then exit;
  inc(fSuperseded);
  if (FSuperseded <> 1) then exit;

  FPriorSupersedeX := X;
  FPriorSupersedeY := Y;
  //--
  var LParentSuperseded := False;
  if FParent is TALInheritPosition then begin
    TALInheritPosition(FParent).SupersedeNoChanges;
    LParentSuperseded := True;
  end;
  try
    Point := FParent.Point;
  finally
    if LParentSuperseded then
      TALInheritPosition(FParent).ReinstateNoChanges;
  end;
end;

{******************}
procedure TALInheritPosition.Reinstate;
begin
  if fSuperseded <= 0 then exit;
  dec(fSuperseded);
  if fSuperseded = 0 then
    Point := TpointF.Create(FPriorSupersedeX, FPriorSupersedeY);
end;

{*************************}
procedure TALInheritPosition.SupersedeNoChanges;
begin
  if (not inherit) or
     (FParent = nil) then exit;
  inc(fSuperseded);
  if (FSuperseded <> 1) then exit;

  FPriorSupersedeX := X;
  FPriorSupersedeY := Y;
  //--
  var LParentSuperseded := False;
  if FParent is TALInheritPosition then begin
    TALInheritPosition(FParent).SupersedeNoChanges;
    LParentSuperseded := True;
  end;
  try
    SetPointNoChange(FParent.Point);
  finally
    if LParentSuperseded then
      TALInheritPosition(FParent).ReinstateNoChanges;
  end;
end;

{*************************}
procedure TALInheritPosition.ReinstateNoChanges;
begin
  if fSuperseded <= 0 then exit;
  dec(fSuperseded);
  if fSuperseded = 0 then
    SetPointNoChange(TpointF.Create(FPriorSupersedeX, FPriorSupersedeY));
end;

{**}
Type
  _TControlAccessProtected = class(Tcontrol);

{***********************************}
constructor TALBaseStateStyle.Create(const AParent: TObject);
begin
  inherited Create;
  //--
  FDefaultTransitionDuration := 0.2;
  //--
  var LShapeControl: IALShapeControl;
  if Supports(AParent, IALShapeControl, LShapeControl) then begin
    {$IF defined(debug)}
    if not (AParent is TALControl) then
      raise Exception.Create('Error AFF4CD13-6544-4914-AC4E-B3AAB4D6DB43');
    {$ENDIF}
    FControlParent := TALControl(AParent);
    FFill := TALInheritBrush.Create(LShapeControl.Getfill, TAlphaColors.White{ADefaultColor});
    FStateLayer := TALStateLayer.Create(TAlphaColors.Null{ADefaultColor});
    FStroke := TALInheritStrokeBrush.Create(LShapeControl.GetStroke, TAlphaColors.Black{ADefaultColor});
    FShadow := TALInheritShadow.Create(LShapeControl.GetShadow);
    FScale := TALInheritPosition.Create(_TControlAccessProtected(AParent).Scale,TPointF.Create(1,1));
  end
  else begin
    {$IF defined(debug)}
    if not (AParent is TALBaseStateStyle) then
      raise Exception.Create('Error 54F5A9EA-9BD8-447E-B99C-B288950FF234');
    {$ENDIF}
    FStateStyleParent := TALBaseStateStyle(AParent);
    FFill := TALInheritBrush.Create(FStateStyleParent.fill, TAlphaColors.White{ADefaultColor});
    FStateLayer := TALStateLayer.Create(TAlphaColors.Null{ADefaultColor});
    FStroke := TALInheritStrokeBrush.Create(FStateStyleParent.Stroke, TAlphaColors.Black{ADefaultColor});
    FShadow := TALInheritShadow.Create(FStateStyleParent.Shadow);
    FScale := TALInheritPosition.Create(FStateStyleParent.Scale, TPointF.Create(1,1));
  end;
  FFill.OnChanged := FillChanged;
  FStateLayer.OnChanged := StateLayerChanged;
  FStroke.OnChanged := StrokeChanged;
  FShadow.OnChanged := ShadowChanged;
  FScale.OnChange := ScaleChanged;
  //--
  FTransitionDuration := FDefaultTransitionDuration;
  //--
  fSuperseded := 0;
end;

{*************************************}
destructor TALBaseStateStyle.Destroy;
begin
  ALFreeAndNil(FFill);
  ALFreeAndNil(FStateLayer);
  ALFreeAndNil(FStroke);
  ALFreeAndNil(FShadow);
  ALFreeAndNil(FScale);
  inherited Destroy;
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
      Scale.Assign(TALBaseStateStyle(Source).Scale);
      TransitionDuration := TALBaseStateStyle(Source).TransitionDuration;
    Finally
      EndUpdate;
    End;
  end
  else
    ALAssignError(Source{ASource}, Self{ADest});
end;

{******************************************************}
procedure TALBaseStateStyle.Interpolate(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    if ATo <> nil then begin
      Fill.Interpolate(ATo.Fill, ANormalizedTime);
      StateLayer.Interpolate(ATo.StateLayer, ANormalizedTime);
      Stroke.Interpolate(ATo.Stroke, ANormalizedTime);
      Shadow.Interpolate(ATo.Shadow, ANormalizedTime);
      Scale.Interpolate(ATo.Scale, ANormalizedTime);
      //TransitionDuration
    end
    else if FStateStyleParent <> nil then begin
      Fill.Interpolate(FStateStyleParent.Fill, ANormalizedTime);
      StateLayer.Interpolate(FStateStyleParent.StateLayer, ANormalizedTime);
      Stroke.Interpolate(FStateStyleParent.Stroke, ANormalizedTime);
      Shadow.Interpolate(FStateStyleParent.Shadow, ANormalizedTime);
      Scale.Interpolate(FStateStyleParent.Scale, ANormalizedTime);
      //TransitionDuration
    end
    else if (FControlParent <> nil) and (FControlParent is TALShape) then begin
      Fill.Interpolate(TALShape(FControlParent).Fill, ANormalizedTime);
      StateLayer.Interpolate(nil, ANormalizedTime);
      Stroke.Interpolate(TALShape(FControlParent).Stroke, ANormalizedTime);
      Shadow.Interpolate(TALShape(FControlParent).Shadow, ANormalizedTime);
      Scale.Interpolate(_TControlAccessProtected(FControlParent).Scale, ANormalizedTime);
      //TransitionDuration
    end
    else begin
      Fill.Interpolate(nil, ANormalizedTime);
      StateLayer.Interpolate(nil, ANormalizedTime);
      Stroke.Interpolate(nil, ANormalizedTime);
      Shadow.Interpolate(nil, ANormalizedTime);
      Scale.Interpolate(nil, ANormalizedTime);
      //TransitionDuration
    end;
  Finally
    EndUpdate;
  End;
end;

{*********************************************************************************}
procedure TALBaseStateStyle.InterpolateNoChanges(const ATo: TALBaseStateStyle; const ANormalizedTime: Single);
begin
  BeginUpdate;
  Try
    Interpolate(ATo, ANormalizedTime);
  Finally
    EndUpdateNoChanges;
  end;
end;

{******************}
procedure TALBaseStateStyle.DoSupersede;
begin
  Fill.Supersede;
  Stroke.Supersede;
  Shadow.Supersede;
  Scale.Supersede;
end;

{******************}
procedure TALBaseStateStyle.Supersede;
begin
  beginUpdate;
  try
    inc(fSuperseded);
    if (FSuperseded <> 1) then exit;

    if StateStyleParent <> nil then
      StateStyleParent.SupersedeNoChanges;
    try
      DoSupersede;
    finally
      if StateStyleParent <> nil then
        StateStyleParent.ReinstateNoChanges;
    end;
  finally
    EndUpdate;
  end;
end;

{******************}
procedure TALBaseStateStyle.DoReinstate;
begin
  Fill.Reinstate;
  Stroke.Reinstate;
  Shadow.Reinstate;
  Scale.Reinstate;
end;

{******************}
procedure TALBaseStateStyle.Reinstate;
begin
  beginUpdate;
  try
    if fSuperseded <= 0 then exit;
    dec(fSuperseded);
    if fSuperseded = 0 then
      DoReinstate;
  finally
    EndUpdate;
  end;
end;

{*************************}
procedure TALBaseStateStyle.SupersedeNoChanges;
begin
  BeginUpdate;
  try
    Supersede;
  finally
    EndUpdateNoChanges;
  end;
end;

{*************************}
procedure TALBaseStateStyle.ReinstateNoChanges;
begin
  BeginUpdate;
  try
    Reinstate;
  finally
    EndUpdateNoChanges;
  end;
end;

{****************************************************************************}
procedure TALBaseStateStyle.SetFill(const AValue: TALInheritBrush);
begin
  FFill.Assign(AValue);
end;

{********************************************************************}
procedure TALBaseStateStyle.SetStateLayer(const AValue: TALStateLayer);
begin
  FStateLayer.Assign(AValue);
end;

{************************************************************************************}
procedure TALBaseStateStyle.SetStroke(const AValue: TALInheritStrokeBrush);
begin
  FStroke.Assign(AValue);
end;

{*******************************************************************************}
procedure TALBaseStateStyle.SetShadow(const AValue: TALInheritShadow);
begin
  FShadow.Assign(AValue);
end;

{***********************************************}
procedure TALBaseStateStyle.SetScale(const Value: TALInheritPosition);
begin
  FScale.Assign(Value);
end;

{***********************************************}
procedure TALBaseStateStyle.SetTransitionDuration(const Value: Single);
begin
  if not SameValue(FTransitionDuration, Value, TEpsilon.Vector) then begin
    FTransitionDuration := Value;
    Change;
  end;
end;

{***********************************************}
function TALBaseStateStyle.GetInherit: Boolean;
begin
  Result := Fill.Inherit and
            (not StateLayer.HasFill) and
            Stroke.Inherit and
            Shadow.Inherit and
            Scale.Inherit;
end;

{**********************************************************}
procedure TALBaseStateStyle.FillChanged(ASender: TObject);
begin
  Change;
end;

{**********************************************************}
procedure TALBaseStateStyle.StateLayerChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************}
procedure TALBaseStateStyle.StrokeChanged(ASender: TObject);
begin
  Change;
end;

{************************************************************}
procedure TALBaseStateStyle.ShadowChanged(ASender: TObject);
begin
  Change;
end;

{*********************************************************}
procedure TALBaseStateStyle.ScaleChanged(ASender: TObject);
begin
  Change;
end;

{**************************************************}
function TALBaseStateStyle.IsTransitionDurationStored: Boolean;
begin
  Result := not SameValue(FTransitionDuration, FDefaultTransitionDuration, TEpsilon.Vector);
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

{*}
var
  ALResourceDirectory: String;
  ALResourceDirectoryLock: TLightweightMREW;

{**************************************}
function ALGetResourceDirectory: String;
begin
  ALResourceDirectoryLock.beginRead;
  Try
    result := ALResourceDirectory;
  finally
    ALResourceDirectoryLock.endRead;
  end;

  if result = '' then begin
    ALResourceDirectoryLock.beginWrite;
    Try

      {$IF Defined(ANDROID)}
      // asset:///resources
      ALResourceDirectory := 'asset:///resources';
      {$ELSEIF Defined(IOS)}
      // /private/var/containers/Bundle/Application/1A22111C-63ED-2255-8899-33554D5FA659/myapp.app/Resources
      var LBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
      ALResourceDirectory := TPath.combine(UTF8ToString(LBundle.resourcePath.UTF8String), 'Resources');
      {$ELSEIF Defined(ALMacOS)}
      // /Users/xxx/PAServer/scratch-dir/xxx-VMWare/myapp.app/Contents/Resources
      var LBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
      ALResourceDirectory := UTF8ToString(LBundle.resourcePath.UTF8String);
      {$ELSEIF Defined(MSWINDOWS)}
      // c:\Program Files\myapp\Resources
      ALResourceDirectory := TPath.combine(ALGetModulePathW, 'Resources');
      {$ELSE}
      raise Exception.Create('Error 0F41B748-DFAD-48A8-957C-07C175AE5633');
      {$ENDIF}

      result := ALResourceDirectory;

    finally
      ALResourceDirectoryLock.endWrite;
    end;
  end;
end;

{******************************************************************}
function ALGetResourceFilename(const AResourceName: String): String;
begin

  if AResourceName = '' then
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
      else Result := '';
    end;
  end;

  {$ELSE}

  if Assigned(ALCustomGetResourceFilenameProc) then Result := ALCustomGetResourceFilenameProc(AResourceName)
  else If ALposW('.',AResourceName) > 0 then Result := TPath.combine(ALGetResourceDirectory, AResourceName)
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

{*************************************************************************************************}
Procedure ALMakeBufDrawables(const AControl: TControl; const AEnsureDoubleBuffered: Boolean = True);
begin
  // This ensures the style is retained when the control exits the visible area.
  // Otherwise, the style will be released and reapplied shortly after.
  AControl.DisableDisappear := true;

  var LDoubleBufferedControl: IALDoubleBufferedControl;
  if Supports(AControl, IALDoubleBufferedControl, LDoubleBufferedControl) then begin
    if AEnsureDoubleBuffered then
      LDoubleBufferedControl.SetDoubleBuffered(true);
    LDoubleBufferedControl.MakeBufDrawable;
  end;

  for var LChild in aControl.Controls do
    ALMakeBufDrawables(LChild, AEnsureDoubleBuffered);
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
      TAlignLayout.Center: begin
        LSize.Width := Max(LSize.Width, LChildControl.Position.X + LChildControl.width + LChildControl.Margins.right + AControl.padding.right);
        LSize.height := Max(LSize.height, LChildControl.Position.Y + LChildControl.Height + LChildControl.Margins.bottom + AControl.padding.bottom);
      end;

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
  ALCustomGetResourceFilenameProc := nil;
  ALResourceDirectory := '';
  //ALResourceDirectoryLock := ??; their is no TLightweightMREW.create but instead an ugly class operator TLightweightMREW.Initialize :(
  {$IFDEF ANDROID}
  ALViewStackCount := 0;
  _RenderScript := nil;
  {$ENDIF}
  ALFontMetricsCache := TDictionary<TALFontMetricsKey, TALFontMetrics>.Create;
  //ALFontMetricsCacheLock := ??; their is no TLightweightMREW.create but instead an ugly class operator TLightweightMREW.Initialize :(

finalization
  AlFreeAndNil(ALFontMetricsCache);

end.
