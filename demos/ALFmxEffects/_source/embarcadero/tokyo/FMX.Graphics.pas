{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2017 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.Graphics;

{$MINENUMSIZE 4}
{$H+}

interface

{$SCOPEDENUMS ON}

uses
  System.Types, System.UITypes, System.Classes, System.Messaging, System.Generics.Collections, System.SysUtils,
  FMX.Types, FMX.Surfaces, System.Math.Vectors;

type
  TCanvas = class;
  TCanvasClass = class of TCanvas;
  TBitmap = class;
  TBitmapImage = class;
  TTextSettings = class;

{ TGradientPoint }

  TGradientPoint = class(TCollectionItem)
  private
    FColor: TAlphaColor;
    FOffset: Single;
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
  public
    procedure Assign(Source: TPersistent); override;
    property IntColor: TAlphaColor read FColor write FColor;
  published
    property Color: TAlphaColor read GetColor write SetColor;
    property Offset: Single read FOffset write FOffset nodefault;
  end;

{ TGradientPoints }

  TGradientPoints = class(TCollection)
  private
    function GetPoint(Index: Integer): TGradientPoint;
  public
    property Points[Index: Integer]: TGradientPoint read GetPoint; default;
  end;

{ TGradient }

  TGradientStyle = (Linear, Radial);

  TGradientStyleHelper = record helper for TGradientStyle
  const
    gsLinear = TGradientStyle.Linear deprecated 'Use TGradientStyle.Linear';
    gsRadial = TGradientStyle.Radial deprecated 'Use TGradientStyle.Radial';
  end;

  TGradient = class(TPersistent)
  private
    FPoints: TGradientPoints;
    FOnChanged: TNotifyEvent;
    FStartPosition: TPosition;
    FStopPosition: TPosition;
    FStyle: TGradientStyle;
    FRadialTransform: TTransform;
    procedure SetStartPosition(const Value: TPosition);
    procedure SetStopPosition(const Value: TPosition);
    procedure PositionChanged(Sender: TObject);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetColor1(const Value: TAlphaColor);
    function IsLinearStored: Boolean;
    procedure SetStyle(const Value: TGradientStyle);
    function IsRadialStored: Boolean;
    procedure SetRadialTransform(const Value: TTransform);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Change;
    procedure ApplyOpacity(const AOpacity: Single);
    function InterpolateColor(Offset: Single): TAlphaColor; overload;
    function InterpolateColor(X, Y: Single): TAlphaColor; overload;
    function Equal(const AGradient: TGradient): Boolean;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    { fast access }
    property Color: TAlphaColor write SetColor;
    property Color1: TAlphaColor write SetColor1;
  published
    property Points: TGradientPoints read FPoints write FPoints;
    property Style: TGradientStyle read FStyle write SetStyle default TGradientStyle.Linear;
    { linear }
    property StartPosition: TPosition read FStartPosition write SetStartPosition stored IsLinearStored;
    property StopPosition: TPosition read FStopPosition write SetStopPosition stored IsLinearStored;
    { radial }
    property RadialTransform: TTransform read FRadialTransform write SetRadialTransform stored IsRadialStored;
  end;

{ TBrushResource }

  TBrush = class;
  TBrushObject = class;

  TBrushResource = class(TInterfacedPersistent, IFreeNotification)
  private
    FStyleResource: TBrushObject;
    FStyleLookup: string;
    FOnChanged: TNotifyEvent;
    function GetBrush: TBrush;
    procedure SetStyleResource(const Value: TBrushObject);
    function GetStyleLookup: string;
    procedure SetStyleLookup(const Value: string);
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
  public
    destructor Destroy; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Assign(Source: TPersistent); override;
    property Brush: TBrush read GetBrush;
  published
    property StyleResource: TBrushObject read FStyleResource write SetStyleResource stored False;
    property StyleLookup: string read GetStyleLookup write SetStyleLookup;
  end;

{ TBrushBitmap }

  TWrapMode = (Tile, TileOriginal, TileStretch);

  TWrapModeHelper = record helper for TWrapMode
  const
    wmTile = TWrapMode.Tile deprecated 'Use TWrapMode.Tile';
    wmTileOriginal = TWrapMode.TileOriginal deprecated 'Use TWrapMode.TileOriginal';
    wmTileStretch = TWrapMode.TileStretch deprecated 'Use TWrapMode.TileStretch';
  end;

  TBrushBitmap = class(TInterfacedPersistent)
  private
    FOnChanged: TNotifyEvent;
    FBitmap: TBitmap;
    FWrapMode: TWrapMode;
    procedure SetWrapMode(const Value: TWrapMode);
    procedure SetBitmap(const Value: TBitmap);
    function GetBitmapImage: TBitmapImage;
  protected
    procedure DoChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Assign(Source: TPersistent); override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    /// <summary>Image to be used by the brush.</summary>
    property Image: TBitmapImage read GetBitmapImage;
    property WrapMode: TWrapMode read FWrapMode write SetWrapMode;
  end;

{ TBrush }

  TBrushKind = (None, Solid, Gradient, Bitmap, Resource);

  TBrushKindHelper = record helper for TBrushKind
  const
    bkNone = TBrushKind.None deprecated 'Use TBrushKind.None';
    bkSolid = TBrushKind.Solid deprecated 'Use TBrushKind.Solid';
    bkGradient = TBrushKind.Gradient deprecated 'Use TBrushKind.Gradient';
    bkBitmap = TBrushKind.Bitmap deprecated 'Use TBrushKind.Bitmap';
    bkResource = TBrushKind.Resource deprecated 'Use TBrushKind.Resource';
  end;

  TBrush = class(TPersistent)
  private
    FColor: TAlphaColor;
    FKind: TBrushKind;
    FOnChanged: TNotifyEvent;
    FGradient: TGradient;
    FDefaultKind: TBrushKind;
    FDefaultColor: TAlphaColor;
    FResource: TBrushResource;
    FBitmap: TBrushBitmap;
    FOnGradientChanged: TNotifyEvent;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetKind(const Value: TBrushKind);
    procedure SetGradient(const Value: TGradient);
    function IsColorStored: Boolean;
    function IsGradientStored: Boolean;
    function GetColor: TAlphaColor;
    function IsKindStored: Boolean;
    procedure SetResource(const Value: TBrushResource);
    function IsResourceStored: Boolean;
    function IsBitmapStored: Boolean;
  protected
    procedure GradientChanged(Sender: TObject);
    procedure ResourceChanged(Sender: TObject);
    procedure BitmapChanged(Sender: TObject);
  public
    constructor Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property OnGradientChanged: TNotifyEvent read FOnGradientChanged write FOnGradientChanged;
    property DefaultColor: TAlphaColor read FDefaultColor write FDefaultColor;
    property DefaultKind: TBrushKind read FDefaultKind write FDefaultKind;
  published
    property Color: TAlphaColor read GetColor write SetColor stored IsColorStored;
    property Bitmap: TBrushBitmap read FBitmap write FBitmap stored IsBitmapStored;
    property Kind: TBrushKind read FKind write SetKind stored IsKindStored;
    property Gradient: TGradient read FGradient write SetGradient stored IsGradientStored;
    property Resource: TBrushResource read FResource write SetResource stored IsResourceStored;
  end;

  TStrokeCap = (Flat, Round);

  TStrokeCapHelper = record helper for TStrokeCap
  const
    scFlat = TStrokeCap.Flat deprecated 'Use TStrokeCap.Flat';
    scRound = TStrokeCap.Round deprecated 'Use TStrokeCap.Round';
  end;

  TStrokeJoin = (Miter, Round, Bevel);

  TStrokeJoinHelper = record helper for TStrokeJoin
  const
    sjMiter = TStrokeJoin.Miter deprecated 'Use TStrokeJoin.Miter';
    sjRound = TStrokeJoin.Round deprecated 'Use TStrokeJoin.Round';
    sjBevel = TStrokeJoin.Bevel deprecated 'Use TStrokeJoin.Bevel';
  end;

  TStrokeDash = (Solid, Dash, Dot, DashDot, DashDotDot, Custom);

  TStrokeDashHelper = record helper for TStrokeDash
  const
    sdSolid = TStrokeDash.Solid deprecated 'Use TStrokeDash.Solid';
    sdDash = TStrokeDash.Dash deprecated 'Use TStrokeDash.Dash';
    sdDot = TStrokeDash.Dot deprecated 'Use TStrokeDash.Dot';
    sdDashDot = TStrokeDash.DashDot deprecated 'Use TStrokeDash.DashDot';
    sdDashDotDot = TStrokeDash.DashDotDot deprecated 'Use TStrokeDash.DashDotDot';
    sdCustom = TStrokeDash.Custom deprecated 'Use TStrokeDash.Custom';
  end;

  TDashArray = TArray<Single>;

  TStrokeBrush = class(TBrush)
  public type
    TDashData = record
      DashArray: TDashArray;
      DashOffset: Single;
      constructor Create(const ADashArray: TDashArray; ADashOffset: Single);
    end;
    TDashDevice = (Screen, Printer);
    TDashDeviceHelper = record helper for TDashDevice
    const
      ddScreen = TDashDevice.Screen deprecated 'Use TDashDevice.Screen';
      ddPrinter = TDashDevice.Printer deprecated 'Use TDashDevice.Printer';
    end;
    TStdDashes = array[TDashDevice, TStrokeDash] of TDashData;
  private class var
    FStdDash: TStdDashes;
    FStdDashCreated: Boolean;
  private
    FJoin: TStrokeJoin;
    FThickness: Single;
    FCap: TStrokeCap;
    FDash: TStrokeDash;
    FDashArray: TDashArray;
    FDashOffset: Single;
    function IsThicknessStored: Boolean;
    procedure SetCap(const Value: TStrokeCap);
    procedure SetDash(const Value: TStrokeDash);
    procedure SetJoin(const Value: TStrokeJoin);
    procedure SetThickness(const Value: Single);
    function GetDashArray: TDashArray;
    class function GetStdDash(const Device: TDashDevice; const Dash: TStrokeDash): TDashData; static;
    procedure ReadCustomDash(AStream: TStream);
    procedure WriteCustomDash(AStream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(const ADefaultKind: TBrushKind; const ADefaultColor: TAlphaColor); reintroduce;
    procedure Assign(Source: TPersistent); override;
    procedure SetCustomDash(const Dash: array of Single; Offset: Single);
    property DashArray: TDashArray read GetDashArray;
    property DashOffset: Single read FDashOffset;
    class property StdDash[const Device: TDashDevice; const Dash: TStrokeDash]: TDashData read GetStdDash;
  published
    property Thickness: Single read FThickness write SetThickness stored IsThicknessStored nodefault;
    property Cap: TStrokeCap read FCap write SetCap default TStrokeCap.Flat;
    property Dash: TStrokeDash read FDash write SetDash default TStrokeDash.Solid;
    property Join: TStrokeJoin read FJoin write SetJoin default TStrokeJoin.Miter;
  end;

  IFMXSystemFontService = interface(IInterface)
    ['{62017F22-ADF1-44D9-A21D-796D8C7F3CF0}']
    function GetDefaultFontFamilyName: string;
    function GetDefaultFontSize: Single;
  end;

{ TFont }

  TFontClass = class of TFont;

  TFontWeight = (Thin, UltraLight, Light, SemiLight, Regular, Medium, Semibold, Bold, UltraBold, Black, UltraBlack);

  /// <summary>
  ///   Font weight type helper
  /// </summary>
  TFontWeightHelper = record helper for TFontWeight
    /// <summary>
    ///   Checks wherever current weight value is TFontWeight.Regular
    /// </summary>
    function IsRegular: Boolean;
  end;

  TFontSlant = (Regular, Italic, Oblique);

  /// <summary>
  ///   Font slant type helper
  /// </summary>
  TFontSlantHelper = record helper for TFontSlant
    /// <summary>
    ///   Checks wherever current slant value is TFontSlant.Regular
    /// </summary>
    function IsRegular: Boolean;
  end;

  TFontStretch = (UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, Regular, SemiExpanded, Expanded,
    ExtraExpanded, UltraExpanded);

  /// <summary>
  ///   Font stretch type helper
  /// </summary>
  TFontStretchHelper = record helper for TFontStretch
    /// <summary>
    ///   Checks wherever current stretch value is TFontStretch.Regular
    /// </summary>
    function IsRegular: Boolean;
  end;

  /// <summary>
  ///   Extended font style based on TFontStyles. Support multi-weight and multi-stretch fonts
  /// </summary>
  TFontStyleExt = record
    /// <summary>
    ///   Set of regular <c>TFontStyle</c>. May contains any <c>TFontStyle</c> value but TFont will process only
    ///  fsOutline and fsStrikeOut values
    /// </summary>
    SimpleStyle: TFontStyles;
    /// <summary>
    ///   Default font weight value
    /// </summary>
    Weight: TFontWeight;
    /// <summary>
    ///   Default font slant value
    /// </summary>
    Slant: TFontSlant;
    /// <summary>
    ///   Default font stretch value
    /// </summary>
    Stretch: TFontStretch;

    /// <summary>
    ///   Common extended font style constructor.
    ///  Initialy new style is initializing using <c>AOtherStyles</c> values. After that <c>AWeight</c>, <c>ASlant</c>
    ///  and <c>AStretch</c> values are applying
    /// </summary>
    /// <remarks>
    ///   Basicaly bsBold or fsItalic values in AOtherStyles are ignoring. Because after creating result from the set of
    ///  <c>TFontStyle</c> values system will set styles from <c>AWeight</c>, <c>ASlant</c> and <c>AStretch</c> which
    ///  will replace style values.
    /// </remarks>
    class function Create(const AWeight: TFontWeight = TFontWeight.Regular;
      const AStant: TFontSlant = TFontSlant.Regular; const AStretch: TFontStretch = TFontStretch.Regular;
      const AOtherStyles: TFontStyles = []): TFontStyleExt; overload; static; inline;
    ///<summary>Constructor that allows to create extended style from the regular <c>TFontStyles</c></summary>
    class function Create(const AStyle: TFontStyles): TFontStyleExt; overload; static; inline;
    ///<summary>Default style with regular parameters without any decorations</summary>
    class function Default: TFontStyleExt; static; inline;
    //Operators
    ///<summary>Overriding equality check operator</summary>
    class operator Equal(const A, B: TFontStyleExt): Boolean;
    ///<summary>Overriding inequality check operator</summary>
    class operator NotEqual(const A, B: TFontStyleExt): Boolean;
    ///<summary>Overriding implicit conversion to TFontStyles</summary>
    class operator Implicit(const AStyle: TFontStyleExt): TFontStyles;
    ///<summary>Overriding addition operator with both extended styles</summary>
    class operator Add(const A, B: TFontStyleExt): TFontStyleExt;
    ///<summary>Overriding addition operator with single font style item</summary>
    class operator Add(const A: TFontStyleExt; const B: TFontStyle): TFontStyleExt;
    ///<summary>Overriding addition operator with set of font styles</summary>
    class operator Add(const A: TFontStyleExt; const B: TFontStyles): TFontStyleExt;
    ///<summary>Overriding subtraction operator with single font style item</summary>
    class operator Subtract(const A: TFontStyleExt; const B: TFontStyle): TFontStyleExt;
    ///<summary>Overriding subtraction operator with set of font styles</summary>
    class operator Subtract(const A: TFontStyleExt; const B: TFontStyles): TFontStyleExt;
    ///<summary>Overriding check whether TFontStyle contains in TFontStyleExt</summary>
    class operator In(const A: TFontStyle; const B: TFontStyleExt): Boolean;
    ///<summary>Overriding Multiply set of TFontStyle to the TFontStyleExt</summary>
    class operator Multiply(const A: TFontStyles; const B: TFontStyleExt): TFontStyles;

    /// <summary>
    ///   Overriding Multiply set of TFontStyle to the TFontStyleExt
    /// </summary>
    function IsRegular: Boolean;
  end;

  TFont = class(TPersistent)
  private const
    DefaultFontSize: Single = 12.0;
    DefaultFontFamily = 'Tahoma';
    MaxFontSize: Single = 512.0;
  private
    FSize: Single;
    FFamily: TFontName;
    FStyleExt: TFontStyleExt;
    FUpdating: Boolean;
    FChanged: Boolean;
    FOnChanged: TNotifyEvent;
    procedure SetFamily(const Value: TFontName);
    procedure SetSize(const Value: Single);
    function GetStyle: TFontStyles;
    procedure SetStyle(const Value: TFontStyles);
    procedure SetStyleExt(const Value: TFontStyleExt);
    procedure ReadStyleExt(AStream: TStream);
    procedure WriteStyleExt(AStream: TStream);
  private class var
    FFontSvc: IFMXSystemFontService;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function DefaultFamily: string; virtual;
    function DefaultSize: Single; virtual;
    procedure DoChanged; virtual;
  public
    constructor Create;
    procedure AfterConstruction; override;
    procedure Change;
    procedure Assign(Source: TPersistent); override;
    procedure SetSettings(const AFamily: string; const ASize: Single; const AStyle: TFontStyleExt);
    function Equals(Obj: TObject): Boolean; override;
    function IsFamilyStored: Boolean;
    function IsSizeStored: Boolean;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    /// <summary>
    ///   Refrects current font style, including underline and strikeout
    /// </summary>
    property StyleExt: TFontStyleExt read FStyleExt write SetStyleExt;
  published
    property Family: TFontName read FFamily write SetFamily stored IsFamilyStored;
    property Size: Single read FSize write SetSize stored IsSizeStored nodefault;
    property Style: TFontStyles read GetStyle write SetStyle stored False;
  end;

{ TBitmapCodec }

  PBitmapCodecSaveParams = ^TBitmapCodecSaveParams;
  TBitmapCodecSaveParams = record
    // encode quality 0..100
    Quality: Integer;
  end;

  TCustomBitmapCodec = class abstract
  public
    class function GetImageSize(const AFileName: string): TPointF; virtual; abstract;
    class function IsValid(const AStream: TStream): Boolean; virtual; abstract;
    function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal = 0): Boolean; virtual; abstract;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean; virtual; abstract;
    function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal = 0): Boolean; virtual; abstract;
    function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface; const SaveParams: PBitmapCodecSaveParams = nil): Boolean; virtual; abstract;
    function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
      const SaveParams: PBitmapCodecSaveParams = nil): Boolean; virtual; abstract;
  end;
  TCustomBitmapCodecClass = class of TCustomBitmapCodec;

{ TBitmapCodecManager }

  EBitmapCodecManagerException = class(Exception);

  TBitmapCodecManager = class sealed
  public type
    TBitmapCodecClassDescriptor = record
      Extension: string;
      Description: string;
      BitmapCodecClass: TCustomBitmapCodecClass;
      CanSave: Boolean;
    end;
  strict private type
    TBitmapCodecDescriptorField = (Extension, Description);
  strict private
    class var FBitmapCodecClassDescriptors: TList<TBitmapCodecClassDescriptor>;
    class function FindBitmapCodecDescriptor(const Name: string; const Field: TBitmapCodecDescriptorField): TBitmapCodecClassDescriptor;
    class function GuessCodecClass(const Name: string; const Field: TBitmapCodecDescriptorField):
      TCustomBitmapCodecClass;
  private
  public
    // Reserved for internal use only - do not call directly!
    class procedure UnInitialize;
    // Register a bitmap codec class with a file extension, description
    class procedure RegisterBitmapCodecClass(const Extension, Description: string; const CanSave: Boolean;
      const BitmapCodecClass: TCustomBitmapCodecClass);
    class procedure UnregisterBitmapCodecClass(const Extension: string);
    // Helpful function
    class function GetFileTypes: string;
    class function GetFilterString: string;
    class function CodecExists(const AFileName: string): Boolean; overload;
    class function GetImageSize(const AFileName: string): TPointF;
    class function LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal = 0): Boolean;
    class function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean;
    class function LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
      const MaxSizeLimit: Cardinal = 0): Boolean;
    class function SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
      SaveParams: PBitmapCodecSaveParams = nil): Boolean; overload;
    class function SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface; const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
  end;

{ TImageTypeChecker }

  /// <summary>Helper class for BitmapCodec</summary>
  TImageTypeChecker = class
  private type
    TImageData = record
      DataType: String;
      Length: Integer;
      Header: array[0..3] of Byte;
    end;
  public
    /// <summary>Analizes the header to guess the image format of he given file</summary>
    class function GetType(AFileName: String): String; overload;
    /// <summary>Analizes the header to guess the image format of he given stream</summary>
    class function GetType(AData: TStream): String; overload;
  end;

{ TBitmap }

  IBitmapObject = interface(IFreeNotificationBehavior)
    ['{5C17D001-47C1-462F-856D-8358B7B2C842}']
    function GetBitmap: TBitmap;
    property Bitmap: TBitmap read GetBitmap;
  end;

  TBitmapData = record
  private
    FPixelFormat: TPixelFormat;
    FWidth: Integer;
    FHeight: Integer;
    function GetBytesPerPixel: Integer;
    function GetBytesPerLine: Integer;
  public
    Data: Pointer;
    Pitch: Integer;
    constructor Create(const AWidth, AHeight: Integer; const APixelFormat: TPixelFormat);
    function GetPixel(const X, Y: Integer): TAlphaColor;
    procedure SetPixel(const X, Y: Integer; const AColor: TAlphaColor);
    procedure Copy(const Source: TBitmapData);
    // Access to scanline in PixelFormat
    function GetScanline(const I: Integer): Pointer;
    function GetPixelAddr(const I, J: Integer): Pointer;
    property PixelFormat: TPixelFormat read FPixelFormat;
    property BytesPerPixel: Integer read GetBytesPerPixel;
    property BytesPerLine: Integer read GetBytesPerLine;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  TMapAccess = (Read, Write, ReadWrite);

  TMapAccessHelper = record helper for TMapAccess
  const
    maRead = TMapAccess.Read deprecated 'Use TMapAccess.Read';
    maWrite = TMapAccess.Write deprecated 'Use TMapAccess.Write';
    maReadWrite = TMapAccess.ReadWrite deprecated 'Use TMapAccess.ReadWrite';
  end;

  TBitmapImage = class
  private
    FRefCount: Integer;
    FHandle: THandle;
    FCanvasClass: TCanvasClass;
    FWidth: Integer;
    FHeight: Integer;
    FBitmapScale: Single;
    FPixelFormat: TPixelFormat;
    procedure CreateHandle;
    procedure FreeHandle;
    function GetCanvasClass: TCanvasClass;
  public
    constructor Create;
    procedure IncreaseRefCount; inline;
    procedure DecreaseRefCount;
    property RefCount: Integer read FRefCount;
    property CanvasClass: TCanvasClass read GetCanvasClass;
    property Handle: THandle read FHandle;
    property BitmapScale: Single read FBitmapScale;
    property PixelFormat: TPixelFormat read FPixelFormat;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

  TBitmap = class(TInterfacedPersistent, IStreamPersist)
  private
    FImage: TBitmapImage;
    FCanvas: TCanvas;
    FMapped: Boolean;
    FMapAccess: TMapAccess;
    FOnChange: TNotifyEvent;
    function GetBytesPerLine: Integer;
    function GetBytesPerPixel: Integer;
    function GetCanvas: TCanvas;
    function GetPixelFormat: TPixelFormat;
    function GetImage: TBitmapImage;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function GetBitmapScale: Single;
    function GetHandle: THandle;
    procedure SetWidth(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetBitmapScale(const Scale: Single);
    procedure ReadBitmap(Stream: TStream);
    procedure WriteBitmap(Stream: TStream);
    function GetCanvasClass: TCanvasClass;
    function GetBounds: TRect;
    function GetSize: TSize;
    function GetBoundsF: TRectF;
  protected
    procedure CreateNewReference;
    procedure CopyToNewReference;
    procedure DestroyResources; virtual;
    procedure BitmapChanged; virtual;
    procedure AssignFromSurface(const Source: TBitmapSurface);
    procedure DoChange; virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadStyleLookup(Reader: TReader); virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const AWidth, AHeight: Integer); overload; virtual;
    constructor CreateFromStream(const AStream: TStream); virtual;
    constructor CreateFromFile(const AFileName: string); virtual;
    constructor CreateFromBitmapAndMask(const Bitmap, Mask: TBitmap);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function EqualsBitmap(const Bitmap: TBitmap): Boolean;
    procedure SetSize(const ASize: TSize); overload;
    procedure SetSize(const AWidth, AHeight: Integer); overload;
    procedure CopyFromBitmap(const Source: TBitmap); overload;
    procedure CopyFromBitmap(const Source: TBitmap; SrcRect: TRect; DestX, DestY: Integer); overload;

    function IsEmpty: Boolean;
    function HandleAllocated: Boolean;
    procedure FreeHandle;

    procedure Clear(const AColor: TAlphaColor); virtual;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0);
    procedure Rotate(const Angle: Single);
    procedure Resize(const AWidth, AHeight: Integer);
    procedure FlipHorizontal;
    procedure FlipVertical;
    procedure InvertAlpha;
    procedure ReplaceOpaqueColor(const Color: TAlphaColor);

    function CreateMask: PByteArray;
    procedure ApplyMask(const Mask: PByteArray; const DstX: Integer = 0; const DstY: Integer = 0);

    function CreateThumbnail(const AWidth, AHeight: Integer): TBitmap;

    function Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
    procedure Unmap(var Data: TBitmapData);

    procedure LoadFromFile(const AFileName: string);
    procedure LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
      const UseEmbedded: Boolean = True);
    procedure SaveToFile(const AFileName: string; const SaveParams: PBitmapCodecSaveParams = nil);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property Canvas: TCanvas read GetCanvas;
    property CanvasClass: TCanvasClass read GetCanvasClass;
    property Image: TBitmapImage read GetImage;
    property Handle: THandle read GetHandle;
    property PixelFormat: TPixelFormat read GetPixelFormat;
    property BytesPerPixel: Integer read GetBytesPerPixel;
    property BytesPerLine: Integer read GetBytesPerLine;
    /// <summary>Helper property that returns bitmap bounds in TRect</summary>
    property Bounds: TRect read GetBounds;
    /// <summary>Helper property that returns bitmap bounds in TRectF</summary>
    property BoundsF: TRectF read GetBoundsF;
    property BitmapScale: Single read GetBitmapScale write SetBitmapScale;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    /// <summary>Helper property that returns dimention of bitmap as TSize</summary>
    property Size: TSize read GetSize write SetSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TPathData }

  TPathData = class;

  IPathObject = interface(IFreeNotificationBehavior)
    ['{8C014863-4F69-48F2-9CF7-E336BFD3F06B}']
    function GetPath: TPathData;
    property Path: TPathData read GetPath;
  end;

  TPathPointKind = (MoveTo, LineTo, CurveTo, Close);

  TPathPointKindHelper = record helper for TPathPointKind
  const
    ppMoveTo = TPathPointKind.MoveTo deprecated 'Use TPathPointKind.MoveTo';
    ppLineTo = TPathPointKind.LineTo deprecated 'Use TPathPointKind.LineTo';
    ppCurveTo = TPathPointKind.CurveTo deprecated 'Use TPathPointKind.CurveTo';
    ppClose = TPathPointKind.Close deprecated 'Use TPathPointKind.Close';
  end;

  TPathPoint = packed record
    Kind: TPathPointKind;
    Point: TPointF;
    class function Create(const AKind: TPathPointKind; const APoint: TPointF): TPathPoint; static; inline;
    class operator Equal(const APoint1, APoint2: TPathPoint): Boolean;
    class operator NotEqual(const APoint1, APoint2: TPathPoint): Boolean;
  end;

  TPathObject = class;

  TPathData = class(TInterfacedPersistent, IFreeNotification)
  private const
    MinFlatness = 0.05;
  public const
    DefaultFlatness = 0.25;
  private
    FOnChanged: TNotifyEvent;
    FStyleResource: TObject;
    FStyleLookup: string;
    FStartPoint: TPointF;
    FPathData: TList<TPathPoint>;
    FRecalcBounds: Boolean;
    FBounds: TRectF;
    function GetPathString: string;
    procedure SetPathString(const Value: string);
    procedure CalculateBezierCoefficients(const Bezier: TCubicBezier; out AX, BX, CX, AY, BY, CY: Single);
    function PointOnBezier(const StartPoint: TPointF; const AX, BX, CX, AY, BY, CY, T: Single): TPointF;
    function CreateBezier(const Bezier: TCubicBezier; const PointCount: Integer): TPolygon;
    procedure AddArcSvgPart(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
    procedure AddArcSvg(const P1, Radius: TPointF; Angle: Single; const LargeFlag, SweepFlag: Boolean; const P2: TPointF);
    function GetStyleLookup: string;
    procedure SetStyleLookup(const Value: string);
    function GetPath: TPathData;
    function GetCount: Integer; inline;
    function GetPoint(AIndex: Integer): TPathPoint; inline;
    function GetTokensFromString(const PathString: string; var Pos: Integer): string;
    function GetNumberFromString(const PathString: string; var Pos: Integer): string;
    function GetPointFromString(const PathString: string; var Pos: Integer): TPointF;
    function HasRelativeOffset(const PathString: string; const Pos: Integer): Boolean;
  protected
    { rtl }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPath(Stream: TStream);
    procedure WritePath(Stream: TStream);
    { IFreeNotification }
    procedure FreeNotification(AObject: TObject);
    procedure DoChanged(NeedRecalcBounds: Boolean = True); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function EqualsPath(const Path: TPathData): Boolean;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    { creation }
    function LastPoint: TPointF;
    procedure MoveTo(const P: TPointF);
    procedure MoveToRel(const P: TPointF);
    procedure LineTo(const P: TPointF);
    procedure LineToRel(const P: TPointF);
    procedure HLineTo(const X: Single);
    procedure HLineToRel(const X: Single);
    procedure VLineTo(const Y: Single);
    procedure VLineToRel(const Y: Single);
    procedure CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
    procedure CurveToRel(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
    procedure SmoothCurveTo(const ControlPoint2, EndPoint: TPointF);
    procedure SmoothCurveToRel(const ControlPoint2, EndPoint: TPointF);
    procedure QuadCurveTo(const ControlPoint, EndPoint: TPointF);
    procedure ClosePath;
    { shapes }
    procedure AddEllipse(const ARect: TRectF);
    procedure AddRectangle(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const ACornerType: TCornerType = TCornerType.Round);
    procedure AddArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
    { modification }
    procedure AddPath(APath: TPathData);
    procedure Clear;
    procedure Flatten(const Flatness: Single = DefaultFlatness);
    procedure Scale(const ScaleX, ScaleY: Single); overload;
    procedure Scale(const AScale: TPointF); overload; inline;
    procedure Translate(const DX, DY: Single); overload;
    procedure Translate(const Delta: TPointF); overload; inline;
    procedure FitToRect(const ARect: TRectF);
    procedure ApplyMatrix(const M: TMatrix);
    { params }
    function GetBounds: TRectF;
    { convert }
    function FlattenToPolygon(var Polygon: TPolygon; const Flatness: Single = DefaultFlatness): TPointF;
    function IsEmpty: Boolean;
    { access }
    property Count: Integer read GetCount;
    property Points[AIndex: Integer]: TPathPoint read GetPoint; default;
    { resoruces }
    property ResourcePath: TPathData read GetPath;
  published
    property Data: string read GetPathString write SetPathString stored False;
    { This property allow to link path with PathObject by name. }
    property StyleLookup: string read GetStyleLookup write SetStyleLookup;
  end;

{ TCanvasSaveState }

  TCanvasSaveState = class(TPersistent)
  private
  protected
    FAssigned: Boolean;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    FDash: TDashArray;
    FDashOffset: Single;
    FFont: TFont;
    FMatrix: TMatrix;
    FOffset: TPointF;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Assigned: Boolean read FAssigned;
  end;

  TRegion = array of TRectF;
  TRegionArray = array of TRegion;

{ TCanvas }

  ECanvasException = class(Exception);

  TFillTextFlag = (RightToLeft);


  TFillTextFlagHelper = record helper for TFillTextFlag
  const
    ftRightToLeft = TFillTextFlag.RightToLeft deprecated 'Use TFillTextFlag.RightToLeft';
  end;

  TFillTextFlags = set of TFillTextFlag;

  TAbstractPrinter = class(TPersistent);

  PClipRects = ^TClipRects;
  TClipRects = array of TRectF;

  ICanvasObject = interface
  ['{61166E3B-9BC3-41E3-9D9A-5C6AB6460950}']
    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;
  end;

  IModulateCanvas = interface
  ['{B7CFFA1B-FBCF-4B36-AA32-93856F621F28}']
    function GetModulateColor: TAlphaColor;
    procedure SetModulateColor(const AColor: TAlphaColor);
    property ModulateColor: TAlphaColor read GetModulateColor write SetModulateColor;
  end;

  TCanvasStyle = (NeedGPUSurface, SupportClipRects, SupportModulation);
  TCanvasStyles = set of TCanvasStyle;

  TCanvasAttribute = (MaxBitmapSize);

  TCanvasQuality = (SystemDefault, HighPerformance, HighQuality);

  TCanvasQualityHelper = record helper for TCanvasQuality
  const
    ccSystemDefault = TCanvasQuality.SystemDefault deprecated 'Use TCanvasQuality.SystemDefault';
    ccHighPerformance = TCanvasQuality.HighPerformance deprecated 'Use TCanvasQuality.HighPerformance';
    ccHighQuality = TCanvasQuality.HighQuality deprecated 'Use TCanvasQuality.HighQuality';
  end;

  TCanvas = class abstract(TInterfacedPersistent)
  public const
    MaxAllowedBitmapSize = $FFFF;
    DefaultScale = 1;
  protected class var
    FLock: TObject;
  private
    FBeginSceneCount: Integer;
    FFill: TBrush;
    FStroke: TStrokeBrush;
    FParent: TWindowHandle;
    [Weak] FBitmap: TBitmap;
    FScale: Single;
    FQuality: TCanvasQuality;
    FMatrix: TMatrix;
    procedure SetFill(const Value: TBrush);
  protected type
    TMatrixMeaning = (Unknown, Identity, Translate);
    TMatrixMeaningHelper = record helper for TMatrixMeaning
    const
      mmUnknown = TMatrixMeaning.Unknown deprecated 'Use TMatrixMeaning.Unknown';
      mmIdentity = TMatrixMeaning.Identity deprecated 'Use TMatrixMeaning.Identity';
      mmTranslate = TMatrixMeaning.Translate deprecated 'Use TMatrixMeaning.Translate';
    end;
    TCustomMetaBrush = class
    private
      FValid: Boolean;
    public
      property Valid: Boolean read FValid write FValid;
    end;
    TMetaBrush = class(TCustomMetaBrush)
    private
      FKind: TBrushKind;
      FColor: TAlphaColor;
      FOpacity: Single;
      FGradient: TGradient;
      FRect: TRectF;
      [Weak] FBitmapImage: TBitmapImage;
      FWrapMode: TWrapMode;
      function GetGradient: TGradient;
    public
      destructor Destroy; override;
      property Kind: TBrushKind read FKind write FKind;
      property Color: TAlphaColor read FColor write FColor;
      property Opacity: Single read FOpacity write FOpacity;
      property Rect: TRectF read FRect write FRect;
      property WrapMode: TWrapMode read FWrapMode write FWrapMode;
      property Gradient: TGradient read GetGradient;
      property Image: TBitmapImage read FBitmapImage write FBitmapImage;
    end;
    TMetaStrokeBrush = class(TCustomMetaBrush)
    private
      FCap: TStrokeCap;
      FDash: TStrokeDash;
      FJoin: TStrokeJoin;
      FDashArray: TDashArray;
      FDashOffset: Single;
    public
      property Cap: TStrokeCap read FCap write FCap;
      property Dash: TStrokeDash read FDash write FDash;
      property Join: TStrokeJoin read FJoin write FJoin;
      property DashArray: TDashArray read FDashArray write FDashArray;
      property DashOffset: Single read FDashOffset write FDashOffset;
    end;
  private
    FMatrixMeaning: TMatrixMeaning;
    FMatrixTranslate: TPointF;
    FBlending: Boolean;
    /// <summary>Indicates offset of drawing area</summary>
    FOffset: TPointF;
    procedure SetBlending(const Value: Boolean);
    class constructor Create;
    class destructor Destroy;
  type
    TCanvasSaveStateList = TObjectList<TCanvasSaveState>;
  protected
    FClippingChangeCount: Integer;
    FSavingStateCount: Integer;
    FWidth, FHeight: Integer;
    FFont: TFont;
    FCanvasSaveData: TCanvasSaveStateList;
    FPrinter: TAbstractPrinter;
    procedure FontChanged(Sender: TObject); virtual;
    { Window }
    function CreateSaveState: TCanvasSaveState; virtual;
    procedure Initialize;
    procedure UnInitialize;
    function GetCanvasScale: Single; virtual;
    { scene }
    function DoBeginScene(const AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; virtual;
    procedure DoEndScene; virtual;
    procedure DoFlush; virtual;
    { constructors }
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); virtual;
    constructor CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); virtual;
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); virtual;
    { bitmap }
    class function DoInitializeBitmap(const Width, Height: Integer; const Scale: Single; var PixelFormat: TPixelFormat): THandle; virtual; abstract;
    class procedure DoFinalizeBitmap(var Bitmap: THandle); virtual; abstract;
    class function DoMapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean; virtual; abstract;
    class procedure DoUnmapBitmap(const Bitmap: THandle; var Data: TBitmapData); virtual; abstract;
    class procedure DoCopyBitmap(const Source, Dest: TBitmap); virtual;
    { states }
    procedure DoBlendingChanged; virtual;
    { drawing }
    /// <summary>Apply a new matrix transformations to the canvas.</summary>
    procedure DoSetMatrix(const M: TMatrix); virtual;
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); virtual; abstract;
    procedure DoFillRoundRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.Round); virtual;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); virtual; abstract;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); virtual; abstract;
    function DoFillPolygon(const Points: TPolygon; const AOpacity: Single; const ABrush: TBrush): Boolean; virtual;
    procedure DoDrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single; const HighSpeed: Boolean); virtual; abstract;
    procedure DoDrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); virtual; abstract;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); virtual; abstract;
    function DoDrawPolygon(const Points: TPolygon; const AOpacity: Single; const ABrush: TStrokeBrush): Boolean; virtual;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); virtual; abstract;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); virtual; abstract;
    property Parent: TWindowHandle read FParent;
  protected
    function TransformPoint(const P: TPointF): TPointF; inline;
    function TransformRect(const Rect: TRectF): TRectF; inline;
    property MatrixMeaning: TMatrixMeaning read FMatrixMeaning;
    property MatrixTranslate: TPointF read FMatrixTranslate;
  public
    destructor Destroy; override;
    { lock }
    class procedure Lock;
    class procedure Unlock;
    { caps }
    class function GetCanvasStyle: TCanvasStyles; virtual;
    class function GetAttribute(const Value: TCanvasAttribute): Integer; virtual;
    { scene }
    procedure SetSize(const AWidth, AHeight: Integer); virtual;
    function BeginScene(AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
    procedure EndScene;
    property BeginSceneCount: integer read FBeginSceneCount;
    procedure Flush;
    { buffer }
    procedure Clear(const Color: TAlphaColor); virtual; abstract;
    procedure ClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); virtual; abstract;
    /// <summary>Return True is Scale is integer value</summary>
    function IsScaleInteger: Boolean;
    { matrix }
    procedure SetMatrix(const M: TMatrix);
    procedure MultiplyMatrix(const M: TMatrix);
    { state }
    function SaveState: TCanvasSaveState;
    procedure RestoreState(const State: TCanvasSaveState);
    { bitmap }
    class function InitializeBitmap(const Width, Height: Integer; const Scale: Single; var PixelFormat: TPixelFormat): THandle;
    class procedure FinalizeBitmap(var Bitmap: THandle);
    class function MapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean;
    class procedure UnmapBitmap(const Bitmap: THandle; var Data: TBitmapData);
    class procedure CopyBitmap(const Source, Dest: TBitmap);
    { aligning }
    function AlignToPixel(const Value: TPointF): TPointF; overload; inline;
    function AlignToPixel(const Rect: TRectF): TRectF; overload; inline;
    function AlignToPixelVertically(const Value: Single): Single; inline;
    function AlignToPixelHorizontally(const Value: Single): Single; inline;
    { clipping }
    procedure IntersectClipRect(const ARect: TRectF); virtual; abstract;
    procedure ExcludeClipRect(const ARect: TRectF); virtual; abstract;
    { drawing }
    procedure FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single); overload;
    procedure FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single;
      const AOpacity: Single; const ABrush: TBrush); overload;
    procedure FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure FillPath(const APath: TPathData; const AOpacity: Single); overload;
    procedure FillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); overload;
    procedure FillEllipse(const ARect: TRectF; const AOpacity: Single); overload;
    procedure FillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); overload;
    procedure DrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single;
      const HighSpeed: Boolean = False);
    procedure DrawLine(const APt1, APt2: TPointF; const AOpacity: Single); overload;
    procedure DrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); overload;
    procedure DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ABrush: TStrokeBrush; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure DrawPath(const APath: TPathData; const AOpacity: Single); overload;
    procedure DrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); overload;
    procedure DrawEllipse(const ARect: TRectF; const AOpacity: Single); overload;
    procedure DrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); overload;
    procedure DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single); overload;
    procedure DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single; const ABrush: TStrokeBrush); overload;
    { mesauring }
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; virtual; abstract;
    { helpers }
    procedure DrawRectSides(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ASides: TSides; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure DrawRectSides(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const ASides: TSides; const ABrush: TStrokeBrush; const ACornerType: TCornerType = TCornerType.Round); overload;
    procedure DrawDashRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const AColor: TAlphaColor);
    { linear polygon }
    procedure FillPolygon(const Points: TPolygon; const AOpacity: Single); virtual;
    procedure DrawPolygon(const Points: TPolygon; const AOpacity: Single); virtual;
    { text }
    function LoadFontFromStream(const AStream: TStream): Boolean; virtual;
    { deprecated, use TTextLayout }
    procedure FillText(const ARect: TRectF; const AText: string; const WordWrap: Boolean; const AOpacity: Single;
      const Flags: TFillTextFlags; const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center); virtual;
    procedure MeasureText(var ARect: TRectF; const AText: string; const WordWrap: Boolean; const Flags: TFillTextFlags;
      const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center); virtual;
    procedure MeasureLines(const ALines: TLineMetricInfo; const ARect: TRectF; const AText: string; const WordWrap: Boolean; const Flags: TFillTextFlags;
      const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center); virtual;
    function TextToPath(Path: TPathData; const ARect: TRectF; const AText: string; const WordWrap: Boolean;
      const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center): Boolean; virtual;
    function TextWidth(const AText: string): Single;
    function TextHeight(const AText: string): Single;
    { properties }
    property Blending: Boolean read FBlending write SetBlending;
    property Quality: TCanvasQuality read FQuality;
    property Stroke: TStrokeBrush read FStroke;
    property Fill: TBrush read FFill write SetFill;
    property Font: TFont read FFont;
    property Matrix: TMatrix read FMatrix;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Bitmap: TBitmap read FBitmap;
    property Scale: Single read FScale;
    /// <summary>Allows to offset drawing area</summary>
    property Offset: TPointF read FOffset write FOffset;
    { statistics }
    property ClippingChangeCount: Integer read FClippingChangeCount;
    property SavingStateCount: Integer read FSavingStateCount;
  end;

  ECanvasManagerException = class(Exception);

  TCanvasDestroyMessage = class(TMessage);

  TCanvasManager = class sealed
  private type
    TCanvasClassRec = record
      CanvasClass: TCanvasClass;
      Default: Boolean;
      PrinterCanvas: Boolean;
    end;
  strict private
    class var FCanvasList: TList<TCanvasClassRec>;
    class var FDefaultCanvasClass: TCanvasClass;
    class var FDefaultPrinterCanvasClass: TCanvasClass;
    class var FMeasureBitmap: TBitmap;
    class var FEnableSoftwareCanvas: Boolean;
  private
    class function GetDefaultCanvas: TCanvasClass; static;
    class function GetMeasureCanvas: TCanvas; static;
    class function GetDefaultPrinterCanvas: TCanvasClass; static;
  public
    // Reserved for internal use only - do not call directly!
    class procedure UnInitialize;
    // Register a rendering Canvas class
    class procedure RegisterCanvas(const CanvasClass: TCanvasClass; const ADefault: Boolean; const APrinterCanvas: Boolean);
    // Return default Canvas
    class property DefaultCanvas: TCanvasClass read GetDefaultCanvas;
    // Return default Canvas
    class property DefaultPrinterCanvas: TCanvasClass read GetDefaultPrinterCanvas;
    // Return canvas instance used for text measuring for example
    class property MeasureCanvas: TCanvas read GetMeasureCanvas;
    // Creation helper
    class function CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
      const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault): TCanvas;
    class function CreateFromBitmap(const ABitmap: TBitmap;
      const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault): TCanvas;
    class function CreateFromPrinter(const APrinter: TAbstractPrinter): TCanvas;
    class procedure RecreateFromPrinter(const Canvas: TCanvas; const APrinter: TAbstractPrinter);
    class procedure EnableSoftwareCanvas(const Enable: Boolean);
  end;

  TPrinterCanvas = class(TCanvas)
  end;

  TPrinterCanvasClass = class of TPrinterCanvas;

{ TBrushObject }

  IBrushObject = interface(IFreeNotificationBehavior)
    ['{BB870DB6-0228-4165-9906-CF75BFF8C7CA}']
    function GetBrush: TBrush;
    property Brush: TBrush read GetBrush;
  end;

  TBrushObject = class(TFmxObject, IBrushObject)
  private
    FBrush: TBrush;
    { IBrushObject }
    function GetBrush: TBrush;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Brush: TBrush read FBrush write FBrush;
  end;

{ TFontObject }

  IFontObject = interface(IFreeNotificationBehavior)
    ['{F87FBCFE-CE5F-430C-8F46-B20B2E395C1B}']
    function GetFont: TFont;
    property Font: TFont read GetFont;
  end;

  TFontObject = class(TFmxObject, IFontObject)
  private
    FFont: TFont;
    { IFontObject }
    function GetFont: TFont;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font: TFont read FFont write FFont;
  end;

{ TPathObject }

  TPathObject = class(TFmxObject, IPathObject)
  private
    FPath: TPathData;
    { IPathObject }
    function GetPath: TPathData;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Path: TPathData read FPath write FPath;
  end;

{ TBitmapObject }

  TBitmapObject = class(TFmxObject, IBitmapObject)
  private
    FBitmap: TBitmap;
    { IBitmapObject }
    function GetBitmap: TBitmap;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap read FBitmap write FBitmap;
  end;

{ TColorObject }

  TColorObject = class(TFmxObject)
  private
    FColor: TAlphaColor;
  protected
    procedure SetName(const NewName: TComponentName); override;
  published
    property Color: TAlphaColor read FColor write FColor;
  end;

  TFontColorForStateClass = class of TFontColorForState;

  TFontColorForState = class (TPersistent)
  public type
    TIndex = (Normal, Hot, Pressed, Focused, Active);
  private
    [Weak] FOwner: TTextSettings;
    FColor: array [TIndex] of TAlphaColor;
    FUpdateCount: Integer;
    FChanged: Boolean;
    function GetColor(const Index: TIndex): TAlphaColor;
    procedure SetColor(const Index: TIndex; const Value: TAlphaColor);
  protected
    function GetOwner: TPersistent; override;
    function GetCurrentColor(const Index: TIndex): TAlphaColor; virtual;
    procedure DoChanged; virtual;
  public
    constructor Create(const AOwner: TTextSettings); virtual;
    procedure AfterConstruction; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Change;
    property Owner: TTextSettings read FOwner;
    procedure Assign(Source: TPersistent); override;
    function Equals(Obj: TObject): Boolean; override;
    property CurrentColor[const Index: TIndex]: TAlphaColor read GetCurrentColor;
    property Color[const Index: TIndex]: TAlphaColor read GetColor write SetColor; default;
    property Normal: TAlphaColor index TIndex.Normal read GetColor write SetColor default TAlphaColorRec.Null;
    property Hot: TAlphaColor index TIndex.Hot read GetColor write SetColor default TAlphaColorRec.Null;
    property Pressed: TAlphaColor index TIndex.Pressed read GetColor write SetColor default TAlphaColorRec.Null;
    property Focused: TAlphaColor index TIndex.Focused read GetColor write SetColor default TAlphaColorRec.Null;
    property Active: TAlphaColor index TIndex.Active read GetColor write SetColor default TAlphaColorRec.Null;
  end;

  TTextSettingsClass = class of TTextSettings;

  /// <summary>
  ///   This class combines some of properties that relate to the text
  /// </summary>
  TTextSettings = class(TPersistent)
  private
    [Weak] FOwner: TPersistent;
    FFont: TFont;
    FUpdateCount: Integer;
    FHorzAlign: TTextAlign;
    FVertAlign: TTextAlign;
    FWordWrap: Boolean;
    FFontColor: TAlphaColor;
    FIsChanged: Boolean;
    FIsAdjustChanged: Boolean;
    FOnChanged: TNotifyEvent;
    FTrimming: TTextTrimming;
    FFontColorForState: TFontColorForState;
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetHorzAlign(const Value: TTextAlign);
    procedure SetVertAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetTrimming(const Value: TTextTrimming);
    procedure SetFontColorForState(const Value: TFontColorForState);
    function StoreFontColorForState: Boolean;
    function CreateFontColorForState: TFontColorForState;
  protected
    procedure DoChanged; virtual;
    procedure SetFont(const Value: TFont); virtual;
    function GetOwner: TPersistent; override;
    function GetTextColorsClass: TFontColorForStateClass; virtual;
    procedure DoAssign(const Source: TTextSettings); virtual;
    procedure DoAssignNotStyled(const TextSettings: TTextSettings; const StyledSettings: TStyledSettings); virtual;
  public
    constructor Create(const AOwner: TPersistent); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignNotStyled(const TextSettings: TTextSettings; const StyledSettings: TStyledSettings);
    function Equals(Obj: TObject): Boolean; override;
    procedure Change;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateStyledSettings(const OldTextSettings, DefaultTextSettings: TTextSettings;
      var StyledSettings: TStyledSettings); virtual;
    property UpdateCount: integer read FUpdateCount;
    property IsChanged: boolean read FIsChanged write FIsChanged;
    property IsAdjustChanged: boolean read FIsAdjustChanged write FIsAdjustChanged;
    property Font: TFont read FFont write SetFont;
    property FontColor: TAlphaColor read FFontColor write SetFontColor default TAlphaColorRec.Black;
    property FontColorForState: TFontColorForState read FFontColorForState write SetFontColorForState
      stored StoreFontColorForState;
    property HorzAlign: TTextAlign read FHorzAlign write SetHorzAlign default TTextAlign.Leading;
    property VertAlign: TTextAlign read FVertAlign write SetVertAlign default TTextAlign.Center;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property Trimming: TTextTrimming read FTrimming write SetTrimming default TTextTrimming.None;
    property Owner: TPersistent read FOwner;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  ITextSettings = interface
  ['{FD99635D-D8DB-4E26-B36F-97D3AABBCCB3}']
    function GetDefaultTextSettings: TTextSettings;
    function GetTextSettings: TTextSettings;
    procedure SetTextSettings(const Value: TTextSettings);
    function GetResultingTextSettings: TTextSettings;
    function GetStyledSettings: TStyledSettings;
    procedure SetStyledSettings(const Value: TStyledSettings);

    property DefaultTextSettings: TTextSettings read GetDefaultTextSettings;
    property TextSettings: TTextSettings read GetTextSettings write SetTextSettings;
    property ResultingTextSettings: TTextSettings read GetResultingTextSettings;
    property StyledSettings: TStyledSettings read GetStyledSettings write SetStyledSettings;
  end;

implementation

uses
  System.UIConsts, System.Math, System.TypInfo, System.Character, FMX.Consts, FMX.Platform, FMX.TextLayout, FMX.Utils;

{ TImageTypeChecker }

class function TImageTypeChecker.GetType(AFileName: String): String;
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := GetType(LStream);
  finally
    LStream.Free;
  end;
end;

class function TImageTypeChecker.GetType(AData: TStream): String;
var
  LBuffer : TBytes;
  I: Integer;
  LOldPos: Int64;
const
  MaxImageDataLength = 4;
  ImageData: array[0..6] of TImageData = (
    (DataType: SGIFImageExtension;  Length: 3; Header: (71,73,70,0)),      // gif
    (DataType: SBMPImageExtension;  Length: 2; Header: (66,77,0,0)),       // bmp
    (DataType: SPNGImageExtension;  Length: 4; Header: (137,80,78,71)),    // png
    (DataType: STIFFImageExtension; Length: 3; Header: (73,73,42,0)),      // tiff
    (DataType: STIFFImageExtension; Length: 3; Header: (77,77,42,0)),      // tiff 2
    (DataType: SJPGImageExtension;  Length: 4; Header: (255,216,255,224)), // jpg
    (DataType: SJPGImageExtension;  Length: 4; Header: (255,216,255,225))  // jpg (canon)
  );
begin
  Result := String.Empty;
  SetLength(LBuffer, MaxImageDataLength);
  LOldPos := AData.Position;
  try
    if AData.Read(LBuffer, MaxImageDataLength) = MaxImageDataLength then
    begin
      for I := Low(ImageData) to High(ImageData) do
      begin
        if (CompareMem(@ImageData[I].Header[0], LBuffer, ImageData[i].Length) ) then
        begin
          Result := ImageData[I].DataType;
          break;
        end;
      end;
    end;
  finally
    AData.Position := LOldPos;
  end;
end;

{ TGradientPoint }

procedure TGradientPoint.Assign(Source: TPersistent);
begin
  if Source is TGradientPoint then
  begin
    FColor := TGradientPoint(Source).FColor;
    FOffset := TGradientPoint(Source).FOffset;
  end
  else
    inherited;
end;

function TGradientPoint.GetColor: TAlphaColor;
begin
  Result := FColor;
end;

procedure TGradientPoint.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

{ TGradientPoints }

function TGradientPoints.GetPoint(Index: Integer): TGradientPoint;
begin
  Result := TGradientPoint(Items[Index]);
end;

{ TGradient }

constructor TGradient.Create;
var
  P: TGradientPoint;
begin
  inherited;
  FStartPosition := TPosition.Create(PointF(0, 0));
  FStartPosition.OnChange := PositionChanged;
  FStopPosition := TPosition.Create(PointF(0, 1));
  FStopPosition.OnChange := PositionChanged;
  FRadialTransform := TTransform.Create;
  FRadialTransform.OnChanged := PositionChanged;
  FPoints := TGradientPoints.Create(TGradientPoint);
  P := TGradientPoint(FPoints.Add);
  P.IntColor := $FF000000;
  P := TGradientPoint(FPoints.Add);
  P.IntColor := $FFFFFFFF;
  P.Offset := 1;
end;

procedure TGradient.ApplyOpacity(const AOpacity: Single);
var
  I: Integer;
begin
  if AOpacity < 1.0 then
    for I := 0 to FPoints.Count - 1 do
      FPoints[I].Color := MakeColor(FPoints[I].Color, AOpacity);
end;

procedure TGradient.Assign(Source: TPersistent);
var
  SaveChanged: TNotifyEvent;
begin
  if Source is TGradient then
  begin
    SaveChanged := FOnChanged;
    FOnChanged := nil;
    FPoints.Clear;
    FPoints.Assign(TGradient(Source).FPoints);
    FStyle := TGradient(Source).Style;
    if FStyle = TGradientStyle.Linear then
    begin
      FStopPosition.Assign(TGradient(Source).StopPosition);
      FStartPosition.Assign(TGradient(Source).StartPosition);
    end
    else
    begin
      FRadialTransform.Assign(TGradient(Source).RadialTransform);
    end;
    FOnChanged := SaveChanged;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    inherited;
end;

destructor TGradient.Destroy;
begin
  FStartPosition.Free;
  FStopPosition.Free;
  FRadialTransform.Free;
  FPoints.Free;
  inherited;
end;

function TGradient.Equal(const AGradient: TGradient): Boolean;
var
  I: Integer;
begin
  Result := True;
  if FPoints.Count <> AGradient.FPoints.Count then Exit(False);
  if not SameValue(FStartPosition.X, AGradient.FStartPosition.X, TEpsilon.Position) then Exit(False);
  if not SameValue(FStartPosition.Y, AGradient.FStartPosition.Y, TEpsilon.Position) then Exit(False);
  if not SameValue(FStopPosition.X, AGradient.FStopPosition.X, TEpsilon.Position) then Exit(False);
  if not SameValue(FStopPosition.Y, AGradient.FStopPosition.Y, TEpsilon.Position) then Exit(False);
  for I := 0 to FPoints.Count - 1 do
  begin
    if FPoints[I].Color <> AGradient.FPoints[I].Color then Exit(False);
    if not SameValue(FPoints[I].Offset, AGradient.FPoints[I].Offset, TEpsilon.Position) then Exit(False);
  end;
end;

procedure TGradient.Change;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TGradient.InterpolateColor(X, Y: Single): TAlphaColor;
var
  A, B: TPointF;
  Projection: Single;
begin
  case Style of
    TGradientStyle.Linear: begin
      A := StopPosition.Point - StartPosition.Point;
      B := TPointF.Create(X, Y) - StartPosition.Point;
      Projection := A.Normalize.DotProduct(B) / A.Length;
      Result := InterpolateColor(Projection);
    end;
    TGradientStyle.Radial: begin
      A := TPointF.Create(X, Y) - RadialTransform.RotationCenter.Point;
      Result := InterpolateColor(1 - (A.Length * 2));
    end;
  else
    Result := 0;
  end;
end;

function TGradient.InterpolateColor(Offset: Single): TAlphaColor;
var
  I: Integer;
begin
  Result := 0;
  if FPoints.Count > 1 then
  begin
    if Offset < 0 then
      Offset := 0;
    if Offset > 1 then
      Offset := 1;
    if Offset < FPoints[0].Offset then
    begin
      Result := Points[0].IntColor;
      Exit;
    end;
    if Offset > FPoints[FPoints.Count - 1].Offset then
    begin
      Result := FPoints[FPoints.Count - 1].IntColor;
      Exit;
    end;
    for I := 0 to FPoints.Count - 2 do
    begin
      if (Offset < Points[I].Offset) then
        Continue;
      if Offset > Points[I + 1].Offset then
        Continue;
      if Points[I + 1].Offset - Points[I].Offset <= 0 then
        Result := Points[I].IntColor
      else if (I = FPoints.Count - 2) and (Offset > Points[Points.Count - 1].Offset) then // last
        Result := Points[Points.Count - 1].IntColor
      else
        Result := FMX.Utils.InterpolateColor(Points[I].IntColor, Points[I + 1].IntColor,
          (Offset - Points[I].Offset) / (Points[I + 1].Offset - Points[I].Offset));
    end;
  end;
end;

procedure TGradient.PositionChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TGradient.IsLinearStored: Boolean;
begin
  Result := FStyle = TGradientStyle.Linear;
end;

function TGradient.IsRadialStored: Boolean;
begin
  Result := FStyle = TGradientStyle.Radial;
end;

procedure TGradient.SetRadialTransform(const Value: TTransform);
begin
  FRadialTransform.Assign(Value);
end;

procedure TGradient.SetStartPosition(const Value: TPosition);
begin
  FStartPosition.Assign(Value);
end;

procedure TGradient.SetStopPosition(const Value: TPosition);
begin
  FStopPosition.Assign(Value);
end;

procedure TGradient.SetColor(const Value: TAlphaColor);
begin
  if (FPoints.Count > 0) and (Points[0].Color <> Value) then
  begin
    Points[0].Color := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TGradient.SetColor1(const Value: TAlphaColor);
begin
  if (FPoints.Count > 1) and (Points[1].Color <> Value) then
  begin
    Points[1].Color := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TGradient.SetStyle(const Value: TGradientStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

{ TBrushResource }

destructor TBrushResource.Destroy;
begin
  if FStyleResource <> nil then
  begin
    FStyleResource.RemoveFreeNotify(Self);
    FStyleResource := nil;
  end;
  inherited;
end;

procedure TBrushResource.FreeNotification(AObject: TObject);
begin
  if AObject = FStyleResource then
    FStyleResource := nil;
end;

procedure TBrushResource.Assign(Source: TPersistent);
begin
  if Source is TBrushResource then
  begin
    StyleResource := TBrushResource(Source).StyleResource;
    FStyleLookup := TBrushResource(Source).StyleLookup;
  end
  else
    inherited;
end;

procedure TBrushResource.SetStyleResource(const Value: TBrushObject);
begin
  if FStyleResource <> Value then
  begin
    if FStyleResource <> nil then
      FStyleResource.RemoveFreeNotify(Self);
    FStyleResource := Value;
    if FStyleResource <> nil then
    begin
      FStyleLookup := FStyleResource.StyleName;
      FStyleResource.AddFreeNotify(Self);
    end;
  end;
end;

function TBrushResource.GetStyleLookup: string;
begin
  Result := FStyleLookup;
end;

procedure TBrushResource.SetStyleLookup(const Value: string);
begin
  if Value <> FStyleLookup then
  begin
    FStyleLookup := Value;
  end;
end;

function TBrushResource.GetBrush: TBrush;
var
  O: TFmxObject;
begin
  Result := nil;
  if FStyleResource <> nil then
    Result := TBrushObject(FStyleResource).Brush
  else if FStyleLookup <> '' then
  begin
    O := FindStyleResource(FStyleLookup);
    if O is TBrushObject then
      StyleResource := TBrushObject(O);
    if FStyleResource <> nil then
      Result := TBrushObject(FStyleResource).Brush;
  end;
end;

{ TBrushBitmap }

constructor TBrushBitmap.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create(0, 0);
end;

destructor TBrushBitmap.Destroy;
begin
  FBitmap.DisposeOf;
  inherited;
end;

procedure TBrushBitmap.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TBrushBitmap.GetBitmapImage: TBitmapImage;
begin
  if FBitmap <> nil then
    Result := FBitmap.Image
  else
    Result := nil;
end;

procedure TBrushBitmap.Assign(Source: TPersistent);
begin
  if Source is TBrushBitmap then
  begin
    FWrapMode := TBrushBitmap(Source).FWrapMode;
    FBitmap.Assign(TBrushBitmap(Source).FBitmap);
    DoChanged;
  end
  else
    inherited;
end;

procedure TBrushBitmap.SetWrapMode(const Value: TWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    DoChanged;
  end;
end;

procedure TBrushBitmap.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
  DoChanged;
end;

{ TBrush }

constructor TBrush.Create;
begin
  inherited Create;
  FDefaultKind := ADefaultKind;
  FDefaultColor := ADefaultColor;
  FColor := ADefaultColor;
  FKind := FDefaultKind;
  FGradient := TGradient.Create;
  FGradient.OnChanged := GradientChanged;
  FResource := TBrushResource.Create;
  FResource.OnChanged := ResourceChanged;
  FBitmap := TBrushBitmap.Create;
  FBitmap.OnChanged := BitmapChanged;
  FBitmap.Bitmap.OnChange := BitmapChanged;
end;

destructor TBrush.Destroy;
begin
  FBitmap.DisposeOf;
  FResource.DisposeOf;
  FGradient.DisposeOf;
  inherited;
end;

procedure TBrush.Assign(Source: TPersistent);
var
  SaveChange: TNotifyEvent;
begin
  if Source is TBrush then
  begin
    SaveChange := FOnChanged;
    FOnChanged := nil;
    FDefaultKind := TBrush(Source).FDefaultKind;
    FDefaultColor := TBrush(Source).FDefaultColor;
    FColor := TBrush(Source).FColor;
    FKind := TBrush(Source).FKind;
    case FKind of
      TBrushKind.Gradient:
        FGradient.Assign(TBrush(Source).Gradient);
      TBrushKind.Resource:
        FResource.Assign(TBrush(Source).Resource);
      TBrushKind.Bitmap:
        FBitmap.Assign(TBrush(Source).Bitmap);
    end;
    FOnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    inherited;
end;

procedure TBrush.GradientChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
  if Assigned(FOnGradientChanged) then
    FOnGradientChanged(Self);
end;

procedure TBrush.ResourceChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TBrush.BitmapChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TBrush.IsBitmapStored: Boolean;
begin
  Result := (FKind = TBrushKind.Bitmap);
end;

function TBrush.IsColorStored: Boolean;
begin
  Result := (FKind = TBrushKind.Solid) and (FColor <> FDefaultColor);
end;

function TBrush.IsGradientStored: Boolean;
begin
  Result := FKind = TBrushKind.Gradient;
end;

function TBrush.IsKindStored: Boolean;
begin
  Result := FKind <> FDefaultKind;
end;

function TBrush.IsResourceStored: Boolean;
begin
  Result := FKind = TBrushKind.Resource;
end;

procedure TBrush.SetResource(const Value: TBrushResource);
begin
  FResource.Assign(Value);
end;

procedure TBrush.SetGradient(const Value: TGradient);
begin
  FGradient.Assign(Value);
end;

function TBrush.GetColor: TAlphaColor;
begin
  Result := FColor;
end;

procedure TBrush.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if FKind = TBrushKind.Gradient then
      FGradient.Color := Value
    else if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TBrush.SetKind(const Value: TBrushKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

{ TStrokeBrush.TDashData }

constructor TStrokeBrush.TDashData.Create(const ADashArray: TDashArray;
  ADashOffset: Single);
begin
  DashArray := ADashArray;
  DashOffset := ADashOffset;
end;

{ TStrokeBrush }

constructor TStrokeBrush.Create(const ADefaultKind: TBrushKind;
  const ADefaultColor: TAlphaColor);
begin
  inherited;
  FThickness := 1;
end;

procedure TStrokeBrush.ReadCustomDash(AStream: TStream);
var
  Len: Integer;
begin
  AStream.Read(Len, SizeOf(Len));
  SetLength(FDashArray, Len);
  if Len > 0 then
    AStream.Read(FDashArray[0], SizeOf(Single) * Len);
  AStream.Read(FDashOffset, SizeOf(FDashOffset));
end;

procedure TStrokeBrush.WriteCustomDash(AStream: TStream);
var
  Len: Integer;
begin
  Len := Length(FDashArray);
  AStream.Write(Len, SizeOf(Len));
  if Len > 0 then
    AStream.Write(FDashArray[0], SizeOf(Single) * Len);
  AStream.Write(FDashOffset, SizeOf(FDashOffset));
end;

procedure TStrokeBrush.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CustomDash', ReadCustomDash, WriteCustomDash, Dash = TStrokeDash.Custom);
end;

function TStrokeBrush.GetDashArray: TDashArray;
begin
  Result := Copy(FDashArray);
end;

class function TStrokeBrush.GetStdDash(const Device: TDashDevice;
  const Dash: TStrokeDash): TDashData;
begin
  if not FStdDashCreated then
  begin
    // create the screen line dashes
    FStdDash[TDashDevice.Screen, TStrokeDash.Solid] :=
      TDashData.Create(nil, 0);
    FStdDash[TDashDevice.Screen, TStrokeDash.Dash] :=
      TDashData.Create(TDashArray.Create(3, 1), 0);
    FStdDash[TDashDevice.Screen, TStrokeDash.Dot] :=
      TDashData.Create(TDashArray.Create(1, 1), 0);
    FStdDash[TDashDevice.Screen, TStrokeDash.DashDot] :=
      TDashData.Create(TDashArray.Create(3, 1, 1, 1), 0);
    FStdDash[TDashDevice.Screen, TStrokeDash.DashDotDot] :=
      TDashData.Create(TDashArray.Create(3, 1, 1, 1, 1, 1), 0);
    FStdDash[TDashDevice.Screen, TStrokeDash.Custom] :=
      TDashData.Create(nil, 0);

    // create the printer line dashes
  {$IFDEF MACOS}
    // MacOS dashes work strange; these values are experimental values that
    // seem to work correctly
    FStdDash[TDashDevice.Printer, TStrokeDash.Solid] :=
      TDashData.Create(nil, 0);
    FStdDash[TDashDevice.Printer, TStrokeDash.Dash] :=
      TDashData.Create(TDashArray.Create(3 * 2, 6 * 2), 0);
    FStdDash[TDashDevice.Printer, TStrokeDash.Dot] :=
      TDashData.Create(TDashArray.Create(1 * 2, 3 * 2), 0);
    FStdDash[TDashDevice.Printer, TStrokeDash.DashDot] :=
      TDashData.Create(TDashArray.Create(3 * 3, 6 * 3, 1 * 3, 3 * 3), 0);
    FStdDash[TDashDevice.Printer, TStrokeDash.DashDotDot] :=
      TDashData.Create(TDashArray.Create(3 * 2, 9 * 2, 1 * 2, 3 * 2, 1 * 2, 3 * 2), 0);
    FStdDash[TDashDevice.Printer, TStrokeDash.Custom] :=
      TDashData.Create(nil, 0);
  {$ELSE}
    FStdDash[TDashDevice.Printer] := FStdDash[TDashDevice.Screen];
  {$ENDIF}

    FStdDashCreated := True;
  end;

  Result.DashArray := Copy(FStdDash[Device, Dash].DashArray);
  Result.DashOffset := FStdDash[Device, Dash].DashOffset;
end;

procedure TStrokeBrush.Assign(Source: TPersistent);
var
  SaveChange: TNotifyEvent;
begin
  if Source is TStrokeBrush then
  begin
    SaveChange := FOnChanged;
    FOnChanged := nil;

    FDefaultKind := TStrokeBrush(Source).FDefaultKind;
    FDefaultColor := TStrokeBrush(Source).FDefaultColor;
    FColor := TStrokeBrush(Source).Color;
    FKind := TstrokeBrush(Source).Kind;
    case FKind of
      TBrushKind.Gradient:
        FGradient.Assign(TStrokeBrush(Source).Gradient);
      TBrushKind.Resource:
        FResource.Assign(TStrokeBrush(Source).Resource);
      TBrushKind.Bitmap:
        FBitmap.Assign(TStrokeBrush(Source).Bitmap);
    end;

    FThickness := TStrokeBrush(Source).Thickness;
    FCap := TStrokeBrush(Source).Cap;
    FDash := TStrokeBrush(Source).Dash;
    FJoin := TStrokeBrush(Source).Join;
    FDashArray := Copy(TStrokeBrush(Source).FDashArray);
    FDashOffset := TStrokeBrush(Source).FDashOffset;
    FOnChanged := SaveChange;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    inherited;
end;

procedure TStrokeBrush.SetCustomDash(const Dash: array of Single; Offset: Single);
var
  I: Integer;
begin
  FDash := TStrokeDash.Custom;
  SetLength(FDashArray, Length(Dash));
  for I := 0 to High(Dash) do
    FDashArray[I] := Dash[I];
  FDashOffset := Offset;
end;

function TStrokeBrush.IsThicknessStored: Boolean;
begin
  Result := Thickness <> 1;
end;

procedure TStrokeBrush.SetCap(const Value: TStrokeCap);
begin
  if FCap <> Value then
  begin
    FCap := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TStrokeBrush.SetDash(const Value: TStrokeDash);
begin
  if FDash <> Value then
  begin
    FDash := Value;
    case FDash of
      TStrokeDash.Solid:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 0);
        end;
      TStrokeDash.Dash:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 2);
          FDashArray[0] := 1 * 3;
          FDashArray[1] := 1;
        end;
      TStrokeDash.Dot:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 2);
          FDashArray[0] := 1;
          FDashArray[1] := 1;
        end;
      TStrokeDash.DashDot:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 4);
          FDashArray[0] := 1 * 3;
          FDashArray[1] := 1;
          FDashArray[2] := 1;
          FDashArray[3] := 1;
        end;
      TStrokeDash.DashDotDot:
        begin
          FDashOffset := 0;
          SetLength(FDashArray, 6);
          FDashArray[0] := 1 * 3;
          FDashArray[1] := 1;
          FDashArray[2] := 1;
          FDashArray[3] := 1;
          FDashArray[4] := 1;
          FDashArray[5] := 1;
        end;
      TStrokeDash.Custom:
        ;
    else
      FDashOffset := 0;
      SetLength(FDashArray, 0);
    end;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TStrokeBrush.SetJoin(const Value: TStrokeJoin);
begin
  if FJoin <> Value then
  begin
    FJoin := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

procedure TStrokeBrush.SetThickness(const Value: Single);
var
  NewValue: Single;
begin
  NewValue := Max(Value, 0);
  if not SameValue(FThickness, NewValue, TEpsilon.Vector) then
  begin
    FThickness := NewValue;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

{ TFontWeightHelper }

function TFontWeightHelper.IsRegular: Boolean;
begin
  Result := Self = TFontWeight.Regular;
end;

{ TFontSlantHelper }

function TFontSlantHelper.IsRegular: Boolean;
begin
  Result := Self = TFontSlant.Regular;
end;

{ TFontStretchHelper }

function TFontStretchHelper.IsRegular: Boolean;
begin
  Result := Self = TFontStretch.Regular;
end;

{ TFontStyleExt }

class function TFontStyleExt.Create(const AWeight: TFontWeight; const AStant: TFontSlant; const AStretch: TFontStretch;
  const AOtherStyles: TFontStyles): TFontStyleExt;
begin
  Result := TFontStyleExt.Create(AOtherStyles);
  Result.Weight := AWeight;
  Result.Slant := AStant;
  Result.Stretch := AStretch;
end;

class function TFontStyleExt.Create(const AStyle: TFontStyles): TFontStyleExt;
begin
  Result.Weight := TFontWeight.Regular;
  Result.Slant := TFontSlant.Regular;
  Result.Stretch := TFontStretch.Regular;
  if AStyle <> [] then
  begin
    if TFontStyle.fsBold in AStyle then
      Result.Weight := TFontWeight.Bold;
    if TFontStyle.fsItalic in AStyle then
      Result.Slant := TFontSlant.Oblique;
  end;
  Result.SimpleStyle := AStyle - [TFontStyle.fsBold, TFontStyle.fsItalic];
end;

class function TFontStyleExt.Default: TFontStyleExt;
begin
  Result := TFontStyleExt.Create([]);
end;

class operator TFontStyleExt.Equal(const A, B: TFontStyleExt): Boolean;
begin
  Result := (A.SimpleStyle = B.SimpleStyle) and (A.Weight = B.Weight) and (A.Slant = B.Slant)
    and (A.Stretch = B.Stretch);
end;

class operator TFontStyleExt.NotEqual(const A, B: TFontStyleExt): Boolean;
begin
  Result := (A.SimpleStyle <> B.SimpleStyle) or (A.Weight <> B.Weight) or (A.Slant <> B.Slant)
    or (A.Stretch <> B.Stretch);
end;

class operator TFontStyleExt.Implicit(const AStyle: TFontStyleExt): TFontStyles;
begin
  Result := AStyle.SimpleStyle;
  if not AStyle.Weight.IsRegular then
    Include(Result, TFontStyle.fsBold);
  if not AStyle.Slant.IsRegular then
    Include(Result, TFontStyle.fsItalic);
end;

class operator TFontStyleExt.Add(const A, B: TFontStyleExt): TFontStyleExt;
begin
  Result := A;
  Result.SimpleStyle := Result.SimpleStyle + B.SimpleStyle;
end;

class operator TFontStyleExt.Add(const A: TFontStyleExt; const B: TFontStyle): TFontStyleExt;
begin
  Result := A;
  Result.SimpleStyle := Result.SimpleStyle + [B] - [TFontStyle.fsBold, TFontStyle.fsItalic];
  if B = TFontStyle.fsBold then
    Result.Weight := TFontWeight.Bold;
  if B = TFontStyle.fsItalic then
    Result.Slant := TFontSlant.Oblique;
end;

class operator TFontStyleExt.Add(const A: TFontStyleExt; const B: TFontStyles): TFontStyleExt;
begin
  Result := A;
  Result.SimpleStyle := Result.SimpleStyle + B - [TFontStyle.fsBold, TFontStyle.fsItalic];
  if TFontStyle.fsBold in B then
    Result.Weight := TFontWeight.Bold;
  if TFontStyle.fsItalic in B then
    Result.Slant := TFontSlant.Oblique;
end;

class operator TFontStyleExt.Subtract(const A: TFontStyleExt; const B: TFontStyle): TFontStyleExt;
begin
  Result := A;
  Result.SimpleStyle := Result.SimpleStyle - [B] - [TFontStyle.fsBold, TFontStyle.fsItalic];
  if B = TFontStyle.fsBold then
    Result.Weight := TFontWeight.Regular;
  if B = TFontStyle.fsItalic then
    Result.Slant := TFontSlant.Regular;
end;

class operator TFontStyleExt.Subtract(const A: TFontStyleExt; const B: TFontStyles): TFontStyleExt;
begin
  Result := A;
  Result.SimpleStyle := Result.SimpleStyle - B - [TFontStyle.fsBold, TFontStyle.fsItalic];
  if TFontStyle.fsBold in B then
    Result.Weight := TFontWeight.Regular;
  if TFontStyle.fsItalic in B then
    Result.Slant := TFontSlant.Regular;
end;

class operator TFontStyleExt.In(const A: TFontStyle; const B: TFontStyleExt): Boolean;
begin
  if A in [TFontStyle.fsUnderline, TFontStyle.fsStrikeOut] then
    Result := A in B.SimpleStyle
  else if A = TFontStyle.fsBold then
    Result := B.Weight > TFontWeight.Regular
  else if A = TFontStyle.fsItalic then
    Result := not B.Slant.IsRegular
  else
    Result := False;
end;

class operator TFontStyleExt.Multiply(const A: TFontStyles; const B: TFontStyleExt): TFontStyles;
begin
  Result := A * B;
end;

function TFontStyleExt.IsRegular: Boolean;
begin
  Result := (Weight = TFontWeight.Regular) and (Stretch = TFontStretch.Regular) and (Slant = TFontSlant.Regular);
end;

{ TFont }

procedure TFont.SetFamily(const Value: TFontName);
begin
  if FFamily <> Value then
  begin
    FFamily := Value;
    Change;
  end;
end;

procedure TFont.SetSize(const Value: Single);
var
  LSize: Single;
begin
  LSize := EnsureRange(Value, 1, MaxFontSize);
  if not SameValue(FSize, LSize, TEpsilon.FontSize) then
  begin
    FSize := LSize;
    Change;
  end;
end;

function TFont.GetStyle: TFontStyles;
begin
  Result := FStyleExt;
end;

procedure TFont.SetStyle(const Value: TFontStyles);
var
  LStyle: TFontStyleExt;
begin
  LStyle := TFontStyleExt.Create(Value);
  if FStyleExt <> LStyle then
  begin
    FStyleExt := LStyle;
    Change;
  end;
end;

procedure TFont.SetStyleExt(const Value: TFontStyleExt);
begin
  if FStyleExt <> Value then
  begin
    FStyleExt := Value;
    Change;
  end;
end;

procedure TFont.ReadStyleExt(AStream: TStream);
begin
  AStream.Read(FStyleExt.SimpleStyle, SizeOf(TFontStyles));
  AStream.Read(FStyleExt.Weight, SizeOf(TFontWeight));
  AStream.Read(FStyleExt.Slant, SizeOf(TFontSlant));
  AStream.Read(FStyleExt.Stretch, SizeOf(TFontStretch));
end;

procedure TFont.WriteStyleExt(AStream: TStream);
begin
  AStream.Write(FStyleExt.SimpleStyle, SizeOf(TFontStyles));
  AStream.Write(FStyleExt.Weight, SizeOf(TFontWeight));
  AStream.Write(FStyleExt.Slant, SizeOf(TFontSlant));
  AStream.Write(FStyleExt.Stretch, SizeOf(TFontStretch));
end;

procedure TFont.DefineProperties(Filer: TFiler);
begin
  Filer.DefineBinaryProperty('StyleExt', ReadStyleExt, WriteStyleExt, not FStyleExt.IsRegular or (FStyleExt.SimpleStyle <> []));
  inherited;
end;

function TFont.DefaultFamily: string;
begin
  if FFontSvc = nil then
    TPlatformServices.Current.SupportsPlatformService(IFMXSystemFontService, FFontSvc);
  if FFontSvc <> nil then
    Result := FFontSvc.GetDefaultFontFamilyName
  else
    Result := DefaultFontFamily;
end;

function TFont.DefaultSize: Single;
begin
  if FFontSvc = nil then
    TPlatformServices.Current.SupportsPlatformService(IFMXSystemFontService, FFontSvc);
  if FFontSvc <> nil then
    Result := FFontSvc.GetDefaultFontSize
  else
    Result := DefaultFontSize;
end;

procedure TFont.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

constructor TFont.Create;
begin
  inherited;
  FUpdating := True;
  SetSettings(DefaultFamily, DefaultSize, TFontStyleExt.Default);
end;

procedure TFont.AfterConstruction;
begin
  inherited;
  FChanged := False;
  FUpdating := False;
end;

procedure TFont.Change;
begin
  if not FUpdating then
  begin
    FChanged := False;
    DoChanged;
  end
  else
    FChanged := True;
end;

procedure TFont.Assign(Source: TPersistent);
var
  LFont: TFont;
begin
  if (Source = nil) or (Source is TFont) then
  begin
    if Source = nil then
      LFont := TFontClass(ClassType).Create
    else
      LFont := TFont(Source);
    try
      SetSettings(LFont.Family, LFont.Size, LFont.StyleExt);
    finally
      if Source = nil then
        LFont.Free;
    end;
  end
  else
    inherited;
end;

procedure TFont.SetSettings(const AFamily: string; const ASize: Single; const AStyle: TFontStyleExt);
var
  LUpdating: Boolean;
begin
  LUpdating := FUpdating;
  try
    FUpdating := True;
    Family := AFamily;
    Size := ASize;
    StyleExt := AStyle;
  finally
    FUpdating := LUpdating;
  end;
  if not FUpdating and FChanged then
    Change;
end;

function TFont.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj is TFont) and SameValue(Size, TFont(Obj).Size, TEpsilon.FontSize) and (Family = TFont(Obj).Family) and
    (StyleExt = TFont(Obj).StyleExt);
end;

function TFont.IsFamilyStored: Boolean;
begin
  Result := FFamily <> DefaultFamily;
end;

function TFont.IsSizeStored: Boolean;
begin
  Result := not SameValue(FSize, DefaultSize, TEpsilon.FontSize);
end;

{ TFontColorForState }

constructor TFontColorForState.Create(const AOwner: TTextSettings);
begin
  inherited Create;
  FOwner := AOwner;
  BeginUpdate;
end;

procedure TFontColorForState.AfterConstruction;
begin
  inherited;
  FChanged := False;
  EndUpdate;
end;

procedure TFontColorForState.Assign(Source: TPersistent);
var
  LFontColors: TFontColorForState;
  I: TIndex;
begin
  if (Source is TFontColorForState) or (Source = nil) then
  begin
    if Source = nil then
      LFontColors := TFontColorForStateClass(ClassType).Create(Owner)
    else
      LFontColors := TFontColorForState(Source);
    try
      BeginUpdate;
      try
        for I := Low(TIndex) to High(TIndex) do
          Color[I] := LFontColors.Color[I];
      finally
        EndUpdate;
      end;
    finally
      if Source = nil then
        LFontColors.Free;
    end;
  end
  else
    inherited;
end;

function TFontColorForState.Equals(Obj: TObject): Boolean;
var
  I: TIndex;
begin
  Result := Obj is TFontColorForState;
  if Result then
    for I := Low(TIndex) to High(TIndex) do
      if FColor[I] <> TFontColorForState(Obj).FColor[I] then
        Exit(False);
end;

procedure TFontColorForState.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TFontColorForState.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and FChanged then
    try
      DoChanged;
    finally
      FChanged := False;
    end;
  end;
end;

procedure TFontColorForState.Change;
begin
  if FUpdateCount = 0 then
  begin
    try
      DoChanged;
    finally
      FChanged := False;
    end;
  end
  else
    FChanged := True;
end;

procedure TFontColorForState.DoChanged;
begin
  if FOwner <> nil then
    FOwner.Change;
end;

function TFontColorForState.GetColor(const Index: TIndex): TAlphaColor;
begin
  Result := FColor[Index];
end;

procedure TFontColorForState.SetColor(const Index: TIndex; const Value: TAlphaColor);
begin
  if FColor[Index] <> Value then
  begin
    FColor[Index] := Value;
    Change;
  end;
end;

function TFontColorForState.GetCurrentColor(const Index: TIndex): TAlphaColor;
begin
  Result := FColor[Index];
  if (Result = claNull) and (Owner <> nil) then
    Result := Owner.FontColor;
end;

function TFontColorForState.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TTextSettings }

type
  TSettingsFont = class (TFont)
  private
    [weak] FTextSettings: TTextSettings;
  protected
    procedure DoChanged; override;
  public
    constructor Create(const ATextSettings: TTextSettings);
    property TextSettings: TTextSettings read FTextSettings;
  end;

{ TSettingsFont }

constructor TSettingsFont.Create(const ATextSettings: TTextSettings);
begin
  inherited Create;
  FTextSettings := ATextSettings;
end;

procedure TSettingsFont.DoChanged;
begin
  if FTextSettings <> nil then
  begin
    FTextSettings.IsAdjustChanged := True;
    FTextSettings.Change;
  end;
  inherited;
end;

constructor TTextSettings.Create(const AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  BeginUpdate;
  FFontColorForState := CreateFontColorForState;
  FFont := TSettingsFont.Create(Self);
  FontColor := TAlphaColorRec.Black;
  HorzAlign := TTextAlign.Leading;
  VertAlign := TTextAlign.Center;
  Trimming := TTextTrimming.None;
  WordWrap := False;
end;

procedure TTextSettings.AfterConstruction;
begin
  inherited;
  FIsChanged := False;
  FIsAdjustChanged := False;
  EndUpdate;
end;

destructor TTextSettings.Destroy;
begin
  FFont.Free;
  FFontColorForState.Free;
  inherited;
end;

function TTextSettings.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TTextSettings.DoAssign(const Source: TTextSettings);
begin
  Font.Assign(Source.Font);
  FontColor := Source.FontColor;
  FontColorForState.Assign(Source.FontColorForState);
  HorzAlign := Source.HorzAlign;
  VertAlign := Source.VertAlign;
  WordWrap := Source.WordWrap;
  Trimming := Source.Trimming;
end;

procedure TTextSettings.Assign(Source: TPersistent);
var
  LTextSettings: TTextSettings;
begin
  if (Source = nil) or (Source is TTextSettings) then
  begin
    if Source = nil then
      LTextSettings := TTextSettingsClass(ClassType).Create(Owner)
    else
      LTextSettings := TTextSettings(Source);
    try
      BeginUpdate;
      try
        DoAssign(LTextSettings);
      finally
        EndUpdate;
      end;
    finally
      if Source = nil then
        LTextSettings.Free;
    end;
  end
  else
    inherited;
end;

function TTextSettings.Equals(Obj: TObject): Boolean;
var
  Source: TTextSettings;
begin
  Result := Obj is TTextSettings;
  if Result then
  begin
    Source := TTextSettings(Obj);
    Result := (HorzAlign = Source.HorzAlign) and (VertAlign = Source.VertAlign) and  (WordWrap = Source.WordWrap) and
      (FontColor = Source.FontColor) and (Trimming = Source.Trimming) and (Font.Equals(Source.Font)) and
      (FontColorForState.Equals(Source.FontColorForState));
  end;
end;

procedure TTextSettings.DoAssignNotStyled(const TextSettings: TTextSettings; const StyledSettings: TStyledSettings);
begin
  if not (TStyledSetting.Family in StyledSettings) then
    Font.Family := TextSettings.Font.FFamily;
  if not (TStyledSetting.Size in StyledSettings) then
    Font.Size := TextSettings.Font.Size;
  if not (TStyledSetting.Style in StyledSettings) then
    Font.StyleExt := TextSettings.Font.StyleExt;
  if not (TStyledSetting.FontColor in StyledSettings) then
  begin
    FontColor := TextSettings.FontColor;
    FontColorForState := TextSettings.FontColorForState;
  end;
  if not (TStyledSetting.Other in StyledSettings) then
  begin
    HorzAlign := TextSettings.HorzAlign;
    VertAlign := TextSettings.VertAlign;
    WordWrap := TextSettings.WordWrap;
    Trimming := TextSettings.Trimming;
  end;
end;

procedure TTextSettings.AssignNotStyled(const TextSettings: TTextSettings; const StyledSettings: TStyledSettings);
var
  LTextSettings: TTextSettings;
begin
  if StyledSettings <> AllStyledSettings then
  begin
    if StyledSettings = [] then
      Assign(TextSettings)
    else
    begin
      if TextSettings = nil then
        LTextSettings := TTextSettingsClass(ClassType).Create(Owner)
      else
        LTextSettings := TextSettings;
      try
        BeginUpdate;
        try
          DoAssignNotStyled(LTextSettings, StyledSettings);
        finally
          EndUpdate;
        end;
      finally
        if TextSettings = nil then
          LTextSettings.Free;
      end;
    end;
  end;
end;

procedure TTextSettings.UpdateStyledSettings(const OldTextSettings, DefaultTextSettings: TTextSettings; var StyledSettings: TStyledSettings);
begin
  // If the user changed the value of the property, and it differs from the default,
  // then delete the corresponding value from StyledSettings
  if (not SameText(OldTextSettings.Font.Family, Font.Family)) and
    (not SameText(DefaultTextSettings.Font.Family, Font.Family)) then
    Exclude(StyledSettings, TStyledSetting.Family);

  if (not SameValue(OldTextSettings.Font.Size, Font.Size, TEpsilon.FontSize)) and
    (not SameValue(DefaultTextSettings.Font.Size, Font.Size, TEpsilon.FontSize)) then
    Exclude(StyledSettings, TStyledSetting.Size);

  if (OldTextSettings.Font.StyleExt <> Font.StyleExt) and (DefaultTextSettings.Font.StyleExt <> Font.StyleExt) then
    Exclude(StyledSettings, TStyledSetting.Style);

  if ((OldTextSettings.FontColor <> FontColor) and (DefaultTextSettings.FontColor <> FontColor)) then
    Exclude(StyledSettings, TStyledSetting.FontColor);

  if ((OldTextSettings.HorzAlign <> HorzAlign) and (DefaultTextSettings.HorzAlign <> HorzAlign)) or
    ((OldTextSettings.VertAlign <> VertAlign) and (DefaultTextSettings.VertAlign <> VertAlign)) or
    ((OldTextSettings.Trimming <> Trimming) and (DefaultTextSettings.Trimming <> Trimming)) or
    ((OldTextSettings.WordWrap <> WordWrap) and (DefaultTextSettings.WordWrap <> WordWrap)) then
    Exclude(StyledSettings, TStyledSetting.Other);

  if (not OldTextSettings.FontColorForState.Equals(FontColorForState)) and
    (not DefaultTextSettings.FontColorForState.Equals(FontColorForState)) then
    Exclude(StyledSettings, TStyledSetting.FontColor);
end;

procedure TTextSettings.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TTextSettings.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and (FIsChanged or FIsAdjustChanged) then
    try
      DoChanged;
    finally
      FIsChanged := False;
      FIsAdjustChanged := False;
    end;
  end;
end;

procedure TTextSettings.Change;
begin
  FIsChanged := True;
  if (FUpdateCount = 0) then
  begin
    try
      DoChanged;
    finally
      FIsChanged := False;
      FIsAdjustChanged := False;
    end;
  end;
end;

procedure TTextSettings.DoChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TTextSettings.SetFont(const Value: TFont);
begin
  if not (((Font = nil) and (Value = nil)) or ((Font <> nil) and Font.Equals(Value))) then
  begin
    Font.Assign(Value);
    IsAdjustChanged := True;
    Change;
  end;
end;

procedure TTextSettings.SetFontColor(const Value: TAlphaColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    {$IF defined(IOS)}        //
    IsAdjustChanged := True;  // << https://quality.embarcadero.com/browse/RSP-20676
    {$ENDIF}                  //
    Change;
  end;
end;

procedure TTextSettings.SetHorzAlign(const Value: TTextAlign);
begin
  if FHorzAlign <> Value then
  begin
    FHorzAlign := Value;
    IsAdjustChanged := True;
    Change;
  end;
end;

procedure TTextSettings.SetVertAlign(const Value: TTextAlign);
begin
  if FVertAlign <> Value then
  begin
    FVertAlign := Value;
    IsAdjustChanged := True;
    Change;
  end;
end;

procedure TTextSettings.SetTrimming(const Value: TTextTrimming);
begin
  if FTrimming <> Value then
  begin
    FTrimming := Value;
    IsAdjustChanged := True;
    Change;
  end;
end;

procedure TTextSettings.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    IsAdjustChanged := True;
    Change;
  end;
end;

function TTextSettings.GetTextColorsClass: TFontColorForStateClass;
begin
  Result := nil;
end;

function TTextSettings.CreateFontColorForState: TFontColorForState;
var
  LClass: TFontColorForStateClass;
begin
  LClass := GetTextColorsClass;
  if LClass = nil then
    LClass := TFontColorForState;
  Result := LClass.Create(Self);
end;

procedure TTextSettings.SetFontColorForState(const Value: TFontColorForState);
begin
  FFontColorForState.Assign(Value);
end;

function TTextSettings.StoreFontColorForState: Boolean;
var
  LFontColors: TFontColorForState;
begin
  LFontColors := CreateFontColorForState;
  try
    Result := not FFontColorForState.Equals(LFontColors);
  finally
    LFontColors.Free;
  end;
end;

{ TBitmapCodecManager }

class procedure TBitmapCodecManager.UnInitialize;
begin
  FreeAndNil(FBitmapCodecClassDescriptors);
end;

class function TBitmapCodecManager.FindBitmapCodecDescriptor(const Name: string;
  const Field: TBitmapCodecDescriptorField): TBitmapCodecClassDescriptor;
var
  LResult: Boolean;
  LDescriptor: TBitmapCodecClassDescriptor;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FBitmapCodecClassDescriptors <> nil then
    for LDescriptor in FBitmapCodecClassDescriptors do
    begin
      case Field of
        TBitmapCodecDescriptorField.Extension: LResult := SameText(Name, LDescriptor.Extension, loUserLocale);
        TBitmapCodecDescriptorField.Description: LResult := SameText(Name, LDescriptor.Description, loUserLocale);
      else
        LResult := False;
      end;
      if LResult then
        Result := LDescriptor;
    end;
end;

class function TBitmapCodecManager.GuessCodecClass(const Name: string; const Field: TBitmapCodecDescriptorField):
  TCustomBitmapCodecClass;
begin
  Result := FindBitmapCodecDescriptor(name, field).BitmapCodecClass;
  //If none found, fallback to the first one.
  if (Result = nil) and (FBitmapCodecClassDescriptors.Count > 0) then
    Result := FBitmapCodecClassDescriptors[0].BitmapCodecClass;
end;

class procedure TBitmapCodecManager.RegisterBitmapCodecClass(const Extension, Description: string; const CanSave: Boolean;
  const BitmapCodecClass: TCustomBitmapCodecClass);
var
  LDescriptor: TBitmapCodecClassDescriptor;
begin
  if FBitmapCodecClassDescriptors = nil then
    FBitmapCodecClassDescriptors := TList<TBitmapCodecClassDescriptor>.Create;

  LDescriptor.Extension := Extension;
  LDescriptor.Description := Description;
  LDescriptor.BitmapCodecClass := BitmapCodecClass;
  LDescriptor.CanSave := CanSave;
  FBitmapCodecClassDescriptors.Add(LDescriptor);
end;

class procedure TBitmapCodecManager.UnregisterBitmapCodecClass(const Extension: string);
var
  I: Integer;
begin
  if FBitmapCodecClassDescriptors <> nil then
    for I := FBitmapCodecClassDescriptors.Count - 1 downto 0 do
      if SameText(Extension, FBitmapCodecClassDescriptors[I].Extension) then
        FBitmapCodecClassDescriptors.Delete(I);
end;

class function TBitmapCodecManager.CodecExists(const AFileName: string): Boolean;
begin
  Result := FindBitmapCodecDescriptor(ExtractFileExt(AFileName),
    TBitmapCodecDescriptorField.Extension).BitmapCodecClass <> nil;
end;

class function TBitmapCodecManager.GetFileTypes: string;
var
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := '';
  if FBitmapCodecClassDescriptors <> nil then
    for Descriptor in FBitmapCodecClassDescriptors do
      if Result = '' then
        Result := '*' + Descriptor.Extension
      else
        Result := Result + ';' + '*' + Descriptor.Extension;
end;

class function TBitmapCodecManager.GetFilterString: string;
var
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := '';
  if FBitmapCodecClassDescriptors <> nil then
  begin
    for Descriptor in FBitmapCodecClassDescriptors do
      if Result = '' then
        Result := Descriptor.Description + ' (' + '*' + Descriptor.Extension + ')|' + '*' + Descriptor.Extension
      else
        Result := Result + '|' + Descriptor.Description + ' (' + '*' + Descriptor.Extension + ')|' + '*' +
          Descriptor.Extension;
    // all files
    Result := SVAllFiles + ' (' + GetFileTypes + ')|' + GetFileTypes + '|' + Result;
  end;
end;

class function TBitmapCodecManager.GetImageSize(const AFileName: string): TPointF;
var
  CodecClass: TCustomBitmapCodecClass;
  DataType: String;
begin
  DataType := TImageTypeChecker.GetType(AFileName);
  CodecClass := GuessCodecClass(DataType, TBitmapCodecDescriptorField.Extension);
  if CodecClass <> nil then
    Result := CodecClass.GetImageSize(AFileName)
  else
    Result := TPointF.Zero;
end;

class function TBitmapCodecManager.LoadFromFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal = 0): Boolean;
var
  CodecClass: TCustomBitmapCodecClass;
  Codec: TCustomBitmapCodec;
  DataType: String;
begin
  DataType := TImageTypeChecker.GetType(AFileName);
  CodecClass := GuessCodecClass(DataType, TBitmapCodecDescriptorField.Extension);
  if CodecClass <> nil then
  begin
    Codec := CodecClass.Create;
    try
      Result := Codec.LoadFromFile(AFileName, Bitmap, MaxSizeLimit);
    finally
      Codec.Free;
    end;
  end
  else
    Result := False;
end;

class function TBitmapCodecManager.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
  const UseEmbedded: Boolean; const Bitmap: TBitmapSurface): Boolean;
var
  CodecClass: TCustomBitmapCodecClass;
  Codec: TCustomBitmapCodec;
  DataType: String;
begin
  DataType := TImageTypeChecker.GetType(AFileName);
  CodecClass := GuessCodecClass(DataType, TBitmapCodecDescriptorField.Extension);
  if CodecClass <> nil then
  begin
    Codec := CodecClass.Create;
    try
      Result := Codec.LoadThumbnailFromFile(AFileName, AFitWidth, AFitHeight, UseEmbedded, Bitmap);
    finally
      Codec.Free;
    end;
  end
  else
    Result := False;
end;

class function TBitmapCodecManager.LoadFromStream(const AStream: TStream; const Bitmap: TBitmapSurface;
  const MaxSizeLimit: Cardinal = 0): Boolean;
var
  CodecClass: TCustomBitmapCodecClass;
  Codec: TCustomBitmapCodec;
  DataType: String;
begin
  Result := False;
  DataType := TImageTypeChecker.GetType(AStream);
  CodecClass := GuessCodecClass(DataType, TBitmapCodecDescriptorField.Extension);
  if CodecClass <> nil then
  begin
    Codec := CodecClass.Create;
    try
      Result := Codec.LoadFromStream(AStream, Bitmap, MaxSizeLimit);
    finally
      Codec.Free;
    end;
  end
end;

class function TBitmapCodecManager.SaveToFile(const AFileName: string; const Bitmap: TBitmapSurface;
  const SaveParams: PBitmapCodecSaveParams = nil): Boolean;
var
  Codec: TCustomBitmapCodec;
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := False;
  if FBitmapCodecClassDescriptors <> nil then
    for Descriptor in FBitmapCodecClassDescriptors do
      if SameText(ExtractFileExt(AFileName), Descriptor.Extension, loUserLocale) and Descriptor.CanSave then
      begin
        Codec := Descriptor.BitmapCodecClass.Create;
        try
          Result := Codec.SaveToFile(AFileName, Bitmap, SaveParams);
        finally
          Codec.Free;
        end;
      end;
end;

class function TBitmapCodecManager.SaveToStream(const AStream: TStream; const Bitmap: TBitmapSurface; const Extension: string;
  SaveParams: PBitmapCodecSaveParams = nil): Boolean;
var
  Codec: TCustomBitmapCodec;
  Descriptor: TBitmapCodecClassDescriptor;
begin
  Result := False;
  if FBitmapCodecClassDescriptors <> nil then
    for Descriptor in FBitmapCodecClassDescriptors do
      if (SameText(Extension, Descriptor.Extension, loUserLocale) or SameText('.' + Extension, Descriptor.Extension,
        loUserLocale)) and Descriptor.CanSave then
      begin
        Codec := Descriptor.BitmapCodecClass.Create;
        try
          Result := Codec.SaveToStream(AStream, Bitmap, Descriptor.Extension, SaveParams);
        finally
          Codec.Free;
        end;
      end;
end;

{ TBitmapData }

constructor TBitmapData.Create(const AWidth, AHeight: Integer;
  const APixelFormat: TPixelFormat);
begin
  Self.FWidth := AWidth;
  Self.FHeight := AHeight;
  Self.FPixelFormat := APixelFormat;
end;

function TBitmapData.GetBytesPerLine: Integer;
begin
  Result := Width * BytesPerPixel;
end;

function TBitmapData.GetBytesPerPixel: Integer;
begin
  Result := PixelFormatBytes[PixelFormat];
end;

function TBitmapData.GetPixel(const X, Y: Integer): TAlphaColor;
begin
  Result := PixelToAlphaColor(GetPixelAddr(X, Y), PixelFormat);
end;

function TBitmapData.GetPixelAddr(const I, J: Integer): Pointer;
begin
  Result := Pointer(NativeInt(GetScanline(J)) + I * BytesPerPixel);
end;

function TBitmapData.GetScanline(const I: Integer): Pointer;
begin
  Result := Pointer(NativeInt(Data) + I * Pitch);
end;

procedure TBitmapData.Copy(const Source: TBitmapData);
var
  I: Integer;
begin
  for I := 0 to Height - 1 do
    Move(Source.GetScanline(I)^, GetScanline(I)^, BytesPerLine);
end;

procedure TBitmapData.SetPixel(const X, Y: Integer; const AColor: TAlphaColor);
begin
  AlphaColorToPixel(AColor, GetPixelAddr(X, Y), PixelFormat);
end;

{ TBitmapImage }

constructor TBitmapImage.Create;
begin
  inherited;
  FBitmapScale := 1;
end;

procedure TBitmapImage.CreateHandle;
begin
  FHandle := CanvasClass.InitializeBitmap(Width, Height, BitmapScale, FPixelFormat);
end;

procedure TBitmapImage.FreeHandle;
begin
  FCanvasClass.FinalizeBitmap(FHandle);
  FHandle := 0;
end;

function TBitmapImage.GetCanvasClass: TCanvasClass;
begin
  if FCanvasClass = nil then
    FCanvasClass := TCanvasManager.GetDefaultCanvas;
  Result := FCanvasClass;
end;

procedure TBitmapImage.IncreaseRefCount;
begin
  AtomicIncrement(FRefCount);
end;

procedure TBitmapImage.DecreaseRefCount;
begin
  if Self <> nil then
  begin
    AtomicDecrement(FRefCount);
    if FRefCount = 0 then
    begin
      if FHandle <> 0 then
        FreeHandle;
      DisposeOf;
    end;
  end;
end;

{ TBitmap }

constructor TBitmap.Create;
begin
  inherited;
  FImage := TBitmapImage.Create;
  FImage.IncreaseRefCount;
end;

constructor TBitmap.Create(const AWidth, AHeight: Integer);
begin
  Create;
  SetSize(AWidth, AHeight);
end;

constructor TBitmap.CreateFromStream(const AStream: TStream);
begin
  Create;
  LoadFromStream(AStream);
end;

constructor TBitmap.CreateFromFile(const AFileName: string);
begin
  Create;
  LoadFromFile(AFileName);
end;

constructor TBitmap.CreateFromBitmapAndMask(const Bitmap, Mask: TBitmap);

  function GetBrightness(const Color: TAlphaColor): Integer;
  begin
    { Faster integer variant of formula:
      Result = R * 0.2126 + G * 0.7152 + B * 0.0722 }
    Result := ((Integer(TAlphaColorRec(Color).R) * 54) + (Integer(TAlphaColorRec(Color).G) * 183) +
      (Integer(TAlphaColorRec(Color).R) * 19)) div 256;
  end;

var
  I, J: Integer;
  D, B, M: TBitmapData;
  C: TAlphaColor;
begin
  Create(Bitmap.Width, Bitmap.Height);
  if (Bitmap.Width <> Mask.Width) or (Bitmap.Height <> Mask.Height) then
    raise EBitmapIncorrectSize.Create(SBitmapIncorrectSize);
  if Map(TMapAccess.Write, D) then
  try
    if Bitmap.Map(TMapAccess.Read, B) then
    try
      if Mask.Map(TMapAccess.Read, M) then
      try
        for J := 0 to Height - 1 do
          for I := 0 to Width - 1 do
          begin
            C := B.GetPixel(I, J);
            TAlphaColorRec(C).A := GetBrightness(M.GetPixel(I, J));
            D.SetPixel(I, J, C);
          end;
      finally
        Mask.Unmap(M);
      end;
    finally
      Bitmap.Unmap(B);
    end;
  finally
    Unmap(D);
  end;
end;

destructor TBitmap.Destroy;
begin
  DestroyResources;
  FImage.DecreaseRefCount;
  inherited;
end;

function TBitmap.GetImage: TBitmapImage;
begin
  if FImage.Handle = 0 then
    FImage.CreateHandle;
  Result := FImage;
end;

function TBitmap.GetHandle: THandle;
begin
  Result := Image.Handle;
end;

function TBitmap.GetHeight: Integer;
begin
  Result := FImage.Height;
end;

function TBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := Image.PixelFormat;
end;

function TBitmap.GetSize: TSize;
begin
  Result := TSize.Create(Width, Height);
end;

function TBitmap.GetWidth: Integer;
begin
  Result := FImage.Width;
end;

function TBitmap.HandleAllocated: Boolean;
begin
  Result := (FImage <> nil) and (Image.Handle <> 0);
end;

procedure TBitmap.SetWidth(const Value: Integer);
begin
  SetSize(Value, Height);
end;

function TBitmap.GetBitmapScale: Single;
begin
  Result := FImage.BitmapScale;
end;

function TBitmap.GetBounds: TRect;
begin
  Result := TRect.Create(0, 0, Width, Height);
end;

function TBitmap.GetBoundsF: TRectF;
begin
  Result := TRectF.Create(0, 0, Width, Height);
end;

function TBitmap.GetBytesPerLine: Integer;
begin
  Result := BytesPerPixel * Width;
end;

function TBitmap.GetBytesPerPixel: Integer;
begin
  Result := PixelFormatBytes[PixelFormat];
end;

procedure TBitmap.SetBitmapScale(const Scale: Single);
begin
  if BitmapScale <> Scale then
  begin
    TMonitor.Enter(Self);
    try
      CopyToNewReference;
      FImage.FBitmapScale := Scale;
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

procedure TBitmap.SetHeight(const Value: Integer);
begin
  SetSize(Width, Value);
end;

procedure TBitmap.SetSize(const ASize: TSize);
begin
  SetSize(ASize.Width, ASize.Height);
end;

procedure TBitmap.SetSize(const AWidth, AHeight: Integer);
var
  SaveBitmapScale: Single;
begin
  if (FImage.FWidth <> AWidth) or (FImage.FHeight <> AHeight) then
  begin
    if (AWidth > CanvasClass.GetAttribute(TCanvasAttribute.MaxBitmapSize)) or
       (AHeight > CanvasClass.GetAttribute(TCanvasAttribute.MaxBitmapSize))
    then
      raise EBitmapSizeTooBig.CreateRes(@SBitmapSizeTooBig);

    TMonitor.Enter(Self);
    try
      SaveBitmapScale := BitmapScale;
      CreateNewReference;
      FImage.FWidth := Max(0, AWidth);
      FImage.FHeight := Max(0, AHeight);
      FImage.FBitmapScale := SaveBitmapScale;
      BitmapChanged;
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

procedure TBitmap.DestroyResources;
begin
  TMonitor.Enter(Self);
  try
    if FCanvas <> nil then
      FCanvas.DisposeOf;
    FCanvas := nil;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.FreeHandle;
begin
  TMonitor.Enter(Self);
  try
    CreateNewReference;
    BitmapChanged;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.CreateNewReference;
begin
  TMonitor.Enter(Self);
  try
    DestroyResources;
    FImage.DecreaseRefCount;
    FImage := TBitmapImage.Create;
    FImage.IncreaseRefCount;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.CopyToNewReference;
var
  OldHandle: TBitmapImage;
  Source, Dest: TBitmapData;
begin
  if FImage.RefCount > 1 then
  begin
    TMonitor.Enter(Self);
    try
      OldHandle := FImage;
      OldHandle.IncreaseRefCount;
      try
        CreateNewReference;

        FImage.FWidth := OldHandle.Width;
        FImage.FHeight := OldHandle.Height;
        FImage.FPixelFormat := OldHandle.PixelFormat;
        FImage.FBitmapScale := OldHandle.BitmapScale;

        if CanvasClass.MapBitmap(Handle, TMapAccess.Write, Dest) then
        begin
          if CanvasClass.MapBitmap(OldHandle.Handle, TMapAccess.Read, Source) then
          begin
            Move(Source.Data^, Dest.Data^, Source.Pitch * FImage.Height);
            CanvasClass.UnmapBitmap(OldHandle.Handle, Source);
          end;
          CanvasClass.UnmapBitmap(Handle, Dest);
        end;
      finally
        OldHandle.DecreaseRefCount;
      end;
    finally
      TMonitor.Exit(Self);
    end;
  end;
end;

function TBitmap.GetCanvasClass: TCanvasClass;
begin
  Result := Image.CanvasClass;
end;

procedure TBitmap.Clear(const AColor: TAlphaColor);
begin
  ClearRect(TRectF.Create(0, 0, Width, Height), AColor);
end;

procedure TBitmap.ClearRect(const ARect: TRectF; const AColor: TAlphaColor);
var
  R: TRectF;
  M: TBitmapData;
  C: Cardinal;
begin
  TMonitor.Enter(Self);
  try
    R := ARect;
    if R.Left < 0 then
      R.Left := 0;
    if R.Top < 0 then
      R.Top := 0;
    if R.Right > Width then
      R.Right := Width;
    if R.Bottom > Height then
      R.Bottom := Height;
    if R.Bottom < R.Top then
      R.Bottom := R.Top;
    if R.Right < R.Left then
      R.Right := R.Left;
    if (R.Right < 0) or (R.Top < 0) or (R.Left > Width) or (R.Top > Height) then
      Exit;
    if not R.IsEmpty and Map(TMapAccess.Write, M) then
    try
      AlphaColorToPixel(PremultiplyAlpha(AColor), @C, PixelFormat);
      FillAlphaColorRect(PAlphaColorArray(M.Data), M.Pitch div 4, Height, Trunc(R.Left), Trunc(R.Top), Trunc(R.Right),
        Trunc(R.Bottom), C);
    finally
      Unmap(M);
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.CopyFromBitmap(const Source: TBitmap);
begin
  TMonitor.Enter(Self);
  try
    TCanvas.CopyBitmap(Source, Self);
    BitmapChanged;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.CopyFromBitmap(const Source: TBitmap; SrcRect: TRect; DestX, DestY: Integer);
var
  I, MoveBytes: Integer;
  SrcData, DestData: TBitmapData;
begin
  if Map(TMapAccess.Write, DestData) then
  try
    if Source.Map(TMapAccess.Read, SrcData) then
    try
      if SrcRect.Left < 0 then
      begin
        Dec(DestX, SrcRect.Left);
        SrcRect.Left := 0;
      end;
      if SrcRect.Top < 0 then
      begin
        Dec(DestY, SrcRect.Top);
        SrcRect.Top := 0;
      end;
      SrcRect.Right := Min(SrcRect.Right, Source.Width);
      SrcRect.Bottom := Min(SrcRect.Bottom, Source.Height);
      if DestX < 0 then
      begin
        Dec(SrcRect.Left, DestX);
        DestX := 0;
      end;
      if DestY < 0 then
      begin
        Dec(SrcRect.Top, DestY);
        DestY := 0;
      end;
      if DestX + SrcRect.Width > Width then
        SrcRect.Width := Width - DestX;
      if DestY + SrcRect.Height > Height then
        SrcRect.Height := Height - DestY;

      if (SrcRect.Left < SrcRect.Right) and (SrcRect.Top < SrcRect.Bottom) then
      begin
        MoveBytes := SrcRect.Width * SrcData.BytesPerPixel;
        for I := 0 to SrcRect.Height - 1 do
          Move(SrcData.GetPixelAddr(SrcRect.Left, SrcRect.Top + I)^,
            DestData.GetPixelAddr(DestX, DestY + I)^, MoveBytes);
      end;
    finally
      Source.Unmap(SrcData);
    end;
  finally
    Unmap(DestData);
  end;
end;

procedure TBitmap.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TBitmap.EqualsBitmap(const Bitmap: TBitmap): Boolean;
var
  MyMap, BitmapMap: TBitmapData;
  I: Integer;
begin
  if IsEmpty or Bitmap.IsEmpty then
  begin
    Result := IsEmpty and Bitmap.IsEmpty;
    Exit;
  end;
  Result := (Width = Bitmap.Width) and (Height = Bitmap.Height) and (PixelFormat = Bitmap.PixelFormat);
  if Result then
  begin
    if Map(TMapAccess.Read, MyMap) then
    try
      if Bitmap.Map(TMapAccess.Read, BitmapMap) then
      try
        for I := 0 to Height - 1 do
          if not CompareMem(MyMap.GetScanline(I), BitmapMap.GetScanline(I), MyMap.BytesPerLine) then
          begin
            Result := False;
            Exit;
          end;
      finally
        Bitmap.Unmap(BitmapMap);
      end;
    finally
      Unmap(MyMap);
    end;
  end;
end;

procedure TBitmap.BitmapChanged;
begin
  DoChange;
end;

function TBitmap.IsEmpty: Boolean;
begin
  Result := (Width = 0) or (Height = 0);
end;

procedure TBitmap.Assign(Source: TPersistent);
begin
  TMonitor.Enter(Self);
  try
    if Source = nil then
      SetSize(0, 0)
    else if Source is TBitmap then
    begin
      DestroyResources;
      TBitmap(Source).FImage.IncreaseRefCount;
      FImage.DecreaseRefCount;
      FImage := TBitmap(Source).FImage;
      BitmapChanged;
    end
    else if Source is TBitmapSurface then
      AssignFromSurface(TBitmapSurface(Source))
    else
      inherited;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.AssignFromSurface(const Source: TBitmapSurface);
var
  BitmapData: TBitmapData;
  MaxSize: Integer;
  ResampledSurface: TBitmapSurface;
  I: Integer;
  SourceRect: TRectF;
begin
  TMonitor.Enter(Self);
  try
    MaxSize := CanvasClass.GetAttribute(TCanvasAttribute.MaxBitmapSize);
    if (Source.Width > MaxSize) or (Source.Height > MaxSize) then
    begin
      SourceRect := TRectF.Create(0, 0, Source.Width, Source.Height);
      SourceRect.Fit(TRectF.Create(0, 0, MaxSize, MaxSize));
      ResampledSurface := TBitmapSurface.Create;
      try
        ResampledSurface.StretchFrom(Source, Trunc(SourceRect.Width), Trunc(SourceRect.Height), PixelFormat);
        AssignFromSurface(ResampledSurface);
      finally
        ResampledSurface.Free;
      end;
    end
    else
    begin
      SetSize(Source.Width, Source.Height);
      if Map(TMapAccess.Write, BitmapData) then
      try
        for I := 0 to TBitmapSurface(Source).Height - 1 do
          Move(TBitmapSurface(Source).Scanline[I]^, BitmapData.GetScanline(I)^, BitmapData.BytesPerLine);
      finally
        Unmap(BitmapData);
      end;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.AssignTo(Dest: TPersistent);
var
  I: Integer;
  BitmapData: TBitmapData;
begin
  if Dest is TBitmapSurface then
  begin
    TMonitor.Enter(Self);
    try
      TBitmapSurface(Dest).SetSize(Width, Height, PixelFormat);
      if Map(TMapAccess.Read, BitmapData) then
      try
        for I := 0 to Height - 1 do
          Move(BitmapData.GetScanline(I)^, TBitmapSurface(Dest).Scanline[I]^, BitmapData.BytesPerLine);
      finally
        Unmap(BitmapData);
      end;
    finally
      TMonitor.Exit(Self);
    end;
  end
  else
    inherited;
end;

procedure TBitmap.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TBitmap) or not EqualsBitmap(TBitmap(Filer.Ancestor))
    else
      Result := not IsEmpty;
  end;

begin
  inherited;
  Filer.DefineBinaryProperty('PNG', ReadBitmap, WriteBitmap, DoWrite);
  Filer.DefineProperty('StyleLookup', ReadStyleLookup, nil, False);
end;

procedure TBitmap.ReadStyleLookup(Reader: TReader);
begin
  Reader.ReadString;
end;

procedure TBitmap.ReadBitmap(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TBitmap.WriteBitmap(Stream: TStream);
begin
  SaveToStream(Stream);
end;

procedure TBitmap.Rotate(const Angle: Single);
var
  Temp: TBitmap;
  M, M2: TMatrix;
  Pts: array of TPointF;
  R: TRectF;
begin
  if Angle = 0 then
    Exit;

  TMonitor.Enter(Self);
  try
    M := TMatrix.Identity;
    M.m31 := -Width / 2;
    M.m32 := -Height / 2;
    M := M * TMatrix.CreateRotation(DegToRad(Angle));
    { calc new size }
    SetLength(Pts, 4);
    Pts[0] := TPointF.Zero * M;
    Pts[1] := TPointF.Create(Width, 0) * M;
    Pts[2] := TPointF.Create(Width, Height) * M;
    Pts[3] := TPointF.Create(0, Height) * M;
    R := NormalizeRectF(Pts);
    { translate }
    M2 := TMatrix.Identity;
    M2.m31 := R.Width / 2;
    M2.m32 := R.Height / 2;
    M := M * M2;

    Temp := TBitmap.Create(Trunc(R.Width), Trunc(R.Height));
    try
      if Temp.Canvas.BeginScene then
      try
        Temp.Canvas.Clear(0);
        Temp.Canvas.SetMatrix(M);
        temp.Canvas.DrawBitmap(Self, TRectF.Create(0, 0, Width, Height), TRectF.Create(0, 0, Width, Height), 1);
      finally
        Temp.Canvas.EndScene;
      end;
      Assign(Temp);
    finally
      Temp.DisposeOf;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.FlipHorizontal;
var
  I, J: Integer;
  Tmp: TAlphaColor;
  M: TBitmapData;
begin
  if Map(TMapAccess.ReadWrite, M) then
  try
    for J := 0 to Height - 1 do
      for I := 0 to (Width - 1) div 2 do
      begin
        Tmp := PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + Width - 1 - I];
        PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + Width - 1 - I] := PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
        PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I] := Tmp;
      end;
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.FlipVertical;
var
  I: Integer;
  Tmp: PAlphaColorArray;
  M: TBitmapData;
begin
  GetMem(Tmp, Width * 4);
  if Map(TMapAccess.ReadWrite, M) then
  try
    for I := 0 to (Height - 1) div 2 do
    begin
      System.Move(PAlphaColorArray(M.Data)[(Height - 1 - I) * (M.Pitch div 4)], Tmp[0], M.Pitch);
      System.Move(PAlphaColorArray(M.Data)[I * (M.Pitch div 4)], PAlphaColorArray(M.Data)[(Height - 1 - I) * (M.Pitch div 4)], M.Pitch);
      System.Move(Tmp[0], PAlphaColorArray(M.Data)[I * (M.Pitch div 4)], M.Pitch);
    end;
  finally
    Unmap(M);
  end;
  FreeMem(Tmp, Width * 4);
end;

procedure TBitmap.InvertAlpha;
var
  I, J: Integer;
  M: TBitmapData;
  C: PAlphaColorRec;
begin
  if Map(TMapAccess.ReadWrite, M) then
  try
    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        C := @PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
        C^.Color := UnpremultiplyAlpha(C^.Color);
        C^.A := $FF - C^.A;
        C^.Color := PremultiplyAlpha(C^.Color);
      end;
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.ReplaceOpaqueColor(const Color: TAlphaColor);
var
  I, J: Integer;
  M: TBitmapData;
  C: PAlphaColorRec;
begin
  if Map(TMapAccess.ReadWrite, M) then
  try
    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        C := @PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
        if C^.A > 0 then
          C^.Color := PremultiplyAlpha(MakeColor(Color, C^.A / $FF));
      end;
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.Resize(const AWidth, AHeight: Integer);
var
  BufferTmp: TBitmap;
begin
  if not IsEmpty then
  begin
    TMonitor.Enter(Self);
    try
      BufferTmp := CreateThumbnail(AWidth, AHeight);
      try
        Assign(BufferTmp);
      finally
        BufferTmp.Free;
      end;
    finally
      TMonitor.Exit(Self);
    end;
  end
  else
    SetSize(AWidth, AHeight);
end;

function TBitmap.CreateMask: PByteArray;
var
  I, J: Integer;
  M: TBitmapData;
  C: PAlphaColorRec;
begin
  GetMem(Result, Width * Height);
  if Map(TMapAccess.ReadWrite, M) then
  try
    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        C := @PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
        Result[I + (J * Width)] := C^.A;
      end;
  finally
    Unmap(M);
  end;
end;

procedure TBitmap.ApplyMask(const Mask: PByteArray; const DstX: Integer = 0; const DstY: Integer = 0);
var
  I, J: Integer;
  M: TBitmapData;
  C: PAlphaColorRec;
begin
  if Map(TMapAccess.ReadWrite, M) then
  try
    for J := 0 to Height - 1 do
      for I := 0 to Width - 1 do
      begin
        if (I - DstX < 0) or (I - DstX > Width - 1) or (J - DstY < 0) or (J - DstY > Height - 1) then
          Continue;
        if Mask[I - DstX + ((J - DstY) * Width)] > 0 then
        begin
          C := @PAlphaColorArray(M.Data)[J * (M.Pitch div 4) + I];
          C^.Color := PremultiplyAlpha(MakeColor(UnpremultiplyAlpha(C^.Color), ($FF - Mask[I - DstX + ((j - DstY) * Width)]) / $FF));
        end;
      end;
  finally
    Unmap(M);
  end;
end;

function TBitmap.CreateThumbnail(const AWidth, AHeight: Integer): TBitmap;
var
  FitRect: TRectF;
begin
  TMonitor.Enter(Self);
  try
    Result := TBitmap.Create(AWidth, AHeight);
    if not IsEmpty and Result.Canvas.BeginScene then
    try
      FitRect := TRectF.Create(0, 0, Width, Height);
      FitRect := FitRect.FitInto(TRectF.Create(0, 0, AWidth, AHeight));
      Result.Canvas.DrawBitmap(Self, TRectF.Create(0, 0, Width, Height), FitRect, 1);
    finally
      Result.Canvas.EndScene;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TBitmap.Map(const Access: TMapAccess; var Data: TBitmapData): Boolean;
begin
  TMonitor.Enter(Self);

  if Access in [TMapAccess.Write, TMapAccess.ReadWrite] then
    CopyToNewReference;

  if CanvasClass.MapBitmap(Handle, Access, Data) then
  begin
    Data.Create(Width, Height, PixelFormat);
    FMapped := True;
    FMapAccess := Access;
    Result := True;
  end
  else
    Result := False;
end;

procedure TBitmap.Unmap(var Data: TBitmapData);
begin
  if FMapped then
  begin
    CanvasClass.UnmapBitmap(Handle, Data);
    FMapped := False;
    if FMapAccess in [TMapAccess.Write, TMapAccess.ReadWrite] then
      BitmapChanged;

    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.LoadFromFile(const AFileName: string);
var
  Surf: TBitmapSurface;
begin
  TMonitor.Enter(Self);
  try
    Surf := TBitmapSurface.Create;
    try
      if TBitmapCodecManager.LoadFromFile(AFileName, Surf, CanvasClass.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
        Assign(Surf)
      else
        raise EBitmapLoadingFailed.CreateFMT(SBitmapLoadingFailedNamed, [AFileName]);
    finally
      Surf.Free;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: Single;
  const UseEmbedded: Boolean = True);
var
  Surf: TBitmapSurface;
begin
  TMonitor.Enter(Self);
  try
    Surf := TBitmapSurface.Create;
    try
      if TBitmapCodecManager.LoadThumbnailFromFile(AFileName, AFitWidth, AFitHeight, UseEmbedded, Surf) then
        Assign(Surf)
      else
        raise EThumbnailLoadingFailed.CreateFMT(SThumbnailLoadingFailedNamed, [AFileName]);
    finally
      Surf.Free;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.LoadFromStream(Stream: TStream);
var
  S: TStream;
  Surf: TBitmapSurface;
begin
  TMonitor.Enter(Self);
  try
    if Stream.Position > 0 then
    begin
      // need to create temp stream
      S := TMemoryStream.Create;
      try
        S.CopyFrom(Stream, Stream.Size - Stream.Position);
        S.Position := 0;
        Surf := TBitmapSurface.Create;
        try
          if TBitmapCodecManager.LoadFromStream(S, Surf, CanvasClass.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
            Assign(Surf)
          else
            raise EBitmapLoadingFailed.Create(SBitmapLoadingFailed);
        finally
          Surf.Free;
        end;
      finally
        S.Free;
      end;
    end
    else
      if Stream.Size = 0 then
        Clear(0)
      else begin
        Surf := TBitmapSurface.Create;
        try
          if TBitmapCodecManager.LoadFromStream(Stream, Surf, CanvasClass.GetAttribute(TCanvasAttribute.MaxBitmapSize)) then
            Assign(Surf)
          else
            raise EBitmapLoadingFailed.Create(SBitmapLoadingFailed);
        finally
          Surf.Free;
        end;
      end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.SaveToFile(const AFileName: string; const SaveParams: PBitmapCodecSaveParams = nil);
var
  Surf: TBitmapSurface;
begin
  TMonitor.Enter(Self);
  try
    Surf := TBitmapSurface.Create;
    try
      Surf.Assign(Self);
      if not TBitmapCodecManager.SaveToFile(AFileName, Surf, SaveParams) then
        raise EBitmapSavingFailed.CreateFMT(SBitmapSavingFailed, [AFileName]);
    finally
      Surf.Free;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TBitmap.SaveToStream(Stream: TStream);
var
  Surf: TBitmapSurface;
begin
  TMonitor.Enter(Self);
  try
    Surf := TBitmapSurface.Create;
    try
      Surf.Assign(Self);
      if not TBitmapCodecManager.SaveToStream(Stream, Surf, SPNGImageExtension) then
        raise EBitmapSavingFailed.Create(SBitmapSavingFailed);
    finally
      Surf.Free;
    end;
  finally
    TMonitor.Exit(Self);
  end;
end;

function TBitmap.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
    TMonitor.Enter(Self);
    try
      CopyToNewReference;
      FCanvas := CanvasClass.CreateFromBitmap(Self);
    finally
      TMonitor.Exit(Self);
    end;
  end;
  Result := FCanvas;
end;

{ TPathPoint }

class function TPathPoint.Create(const AKind: TPathPointKind; const APoint: TPointF): TPathPoint;
begin
  Result.Kind := AKind;
  Result.Point := APoint;
end;

class operator TPathPoint.Equal(const APoint1, APoint2: TPathPoint): Boolean;
begin
  Result := (APoint1.Kind = APoint2.Kind) and (APoint1.Point = APoint2.Point);
end;

class operator TPathPoint.NotEqual(const APoint1, APoint2: TPathPoint): Boolean;
begin
  Result := not (APoint1 = APoint2);
end;

{ TPath }

constructor TPathData.Create;
begin
  inherited Create;
  FPathData := TList<TPathPoint>.Create;
  FRecalcBounds := True;
end;

destructor TPathData.Destroy;
var
  PathObject: IPathObject;
begin
  if FStyleResource <> nil then
  begin
    if Supports(FStyleResource, IPathObject, PathObject) then
      PathObject.RemoveFreeNotify(Self);
    FStyleResource := nil;
  end;
  FPathData.Free;
  inherited;
end;

function TPathData.EqualsPath(const Path: TPathData): Boolean;
var
  I: Integer;
begin
  if IsEmpty or Path.IsEmpty then
    Exit(True);
  Result := Count = Path.Count;
  if Result then
    for I := 0 to Count - 1 do
      if Points[I] <> Path.Points[I] then
        Exit(False);
end;

procedure TPathData.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TPathData then
  begin
    if TPathData(Source).ResourcePath <> nil then
    begin
      FStyleLookup := TPathData(Source).StyleLookup;
      DoChanged(False);
    end
    else
    begin
      FPathData.Count := TPathData(Source).Count;
      for I := 0 to TPathData(Source).Count - 1 do
        FPathData[I] := TPathData(Source)[I];
      DoChanged;
    end;
  end
  else
    inherited
end;

function TPathData.GetStyleLookup: string;
begin
  Result := FStyleLookup;
end;

procedure TPathData.SetStyleLookup(const Value: string);
begin
  if Value <> FStyleLookup then
  begin
    FStyleLookup := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

function TPathData.GetPath: TPathData;
var
  O: TFmxObject;
  PO: IPathObject;
begin
  Result := nil;
  if (FStyleResource <> nil) and Supports(FStyleResource, IPathObject, PO) then
    Result := PO.Path
  else if FStyleLookup <> '' then
  begin
    O := FindStyleResource(FStyleLookup);
    if Supports(O, IPathObject, PO) then
    begin
      if FStyleResource <> nil then
        PO.RemoveFreeNotify(Self);
      FStyleResource := O;
      if FStyleResource <> nil then
        PO.AddFreeNotify(Self);
      Result := PO.Path;
    end;
  end;
end;

function TPathData.GetCount: Integer;
begin
  Result := FPathData.Count;
end;

function TPathData.GetPoint(AIndex: Integer): TPathPoint;
begin
  Result := FPathData[AIndex];
end;

procedure TPathData.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TPathData) or not EqualsPath(TPathData(Filer.Ancestor))
    else
      Result := Count > 0;
  end;

begin
  inherited;
  Filer.DefineBinaryProperty('Path', ReadPath, WritePath, DoWrite);
end;

procedure TPathData.DoChanged(NeedRecalcBounds: Boolean);
begin
  if NeedRecalcBounds then
    FRecalcBounds := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TPathData.ReadPath(Stream: TStream);
var
  ReadCount: Cardinal;
  I: Integer;
  ByteKind: Byte;
  Point: TPointF;
  PathPoint: TPathPoint;
begin
  Stream.Read(ReadCount, SizeOf(ReadCount));
  FPathData.Count := ReadCount;
  if ReadCount > 0 then
    if (Stream.Size - 4) div ReadCount = 9 then
      for I := 0 to ReadCount - 1 do
      begin
        Stream.Read(ByteKind, SizeOf(Byte));
        Stream.Read(Point, SizeOf(TPointF));
        FPathData[I] := TPathPoint.Create(TPathPointKind(ByteKind), Point);
      end
    else
      for I := 0 to ReadCount - 1 do
      begin
        Stream.Read(PathPoint, SizeOf(PathPoint));
        FPathData[I] := PathPoint;
      end;
  DoChanged;
end;

procedure TPathData.WritePath(Stream: TStream);
var
  WriteCount: Cardinal;
  PathPoint: TPathPoint;
  I: Integer;
begin
  WriteCount := Count;
  Stream.Write(WriteCount, SizeOf(WriteCount));
  for I := 0 to WriteCount - 1 do
  begin
    PathPoint := FPathData[I];
    Stream.Write(PathPoint, SizeOf(PathPoint));
  end;
end;

function TPathData.LastPoint: TPointF;
begin
  if Count > 0 then
    Result := FPathData[FPathData.Count - 1].Point
  else
    Result := TPointF.Zero;
end;

procedure TPathData.MoveTo(const P: TPointF);
begin
  FPathData.Add(TPathPoint.Create(TPathPointKind.MoveTo, P));
  FStartPoint := FPathData[FPathData.Count - 1].Point;
  DoChanged;
end;

procedure TPathData.MoveToRel(const P: TPointF);
begin
  FPathData.Add(TPathPoint.Create(TPathPointKind.MoveTo, LastPoint + P));
  FStartPoint := FPathData[FPathData.Count - 1].Point;
  DoChanged;
end;

procedure TPathData.LineTo(const P: TPointF);
begin
  FPathData.Add(TPathPoint.Create(TPathPointKind.LineTo, P));
  DoChanged;
end;

procedure TPathData.LineToRel(const P: TPointF);
begin
  FPathData.Add(TPathPoint.Create(TPathPointKind.LineTo, LastPoint + P));
  DoChanged;
end;

procedure TPathData.HLineTo(const X: Single);
begin
  FPathData.Add(TPathPoint.Create(TPathPointKind.LineTo, TPointF.Create(X, LastPoint.Y)));
  DoChanged;
end;

procedure TPathData.HLineToRel(const X: Single);
begin
  LineToRel(TPointF.Create(X, 0));
end;

procedure TPathData.VLineTo(const Y: Single);
begin
  FPathData.Add(TPathPoint.Create(TPathPointKind.LineTo, TPointF.Create(LastPoint.X, Y)));
  DoChanged;
end;

procedure TPathData.VLineToRel(const Y: Single);
begin
  LineToRel(TPointF.Create(0, Y));
end;

procedure TPathData.QuadCurveTo(const ControlPoint, EndPoint: TPointF);
const
  OneThird = 1 / 3;
  TwoThirds = 2 / 3;
var
  LP, CP1, CP2: TPointF;
begin
  LP := LastPoint;
  CP1.X := OneThird * LP.X + TwoThirds * ControlPoint.X;
  CP1.Y := OneThird * LP.Y + TwoThirds * ControlPoint.Y;
  CP2.X := TwoThirds * ControlPoint.X + OneThird * EndPoint.X;
  CP2.Y := TwoThirds * ControlPoint.Y + OneThird * EndPoint.Y;
  CurveTo(CP1, CP2, EndPoint);
end;

procedure TPathData.CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
begin
  FPathData.Add(TPathPoint.Create(TPathPointKind.CurveTo, ControlPoint1));
  FPathData.Add(TPathPoint.Create(TPathPointKind.CurveTo, ControlPoint2));
  FPathData.Add(TPathPoint.Create(TPathPointKind.CurveTo, EndPoint));
  DoChanged;
end;

procedure TPathData.CurveToRel(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
var
  LP: TPointF;
begin
  LP := LastPoint;
  CurveTo(LP + ControlPoint1, LP + ControlPoint2, LP + EndPoint);
end;

procedure TPathData.SmoothCurveTo(const ControlPoint2, EndPoint: TPointF);
var
  ControlPoint1: TPointF;
begin
  if Count > 2 then
    ControlPoint1 := LastPoint + (LastPoint - FPathData[FPathData.Count - 2].Point)
  else
    ControlPoint1 := ControlPoint2;
  CurveTo(ControlPoint1, ControlPoint2, EndPoint);
end;

procedure TPathData.SmoothCurveToRel(const ControlPoint2, EndPoint: TPointF);
var
  ControlPoint1: TPointF;
begin
  if Count > 2 then
    ControlPoint1 := LastPoint - FPathData[FPathData.Count - 2].Point
  else
    ControlPoint1 := ControlPoint2;
  CurveToRel(ControlPoint1, ControlPoint2, EndPoint);
end;

procedure TPathData.ClosePath;
begin
  FPathData.Add(TPathPoint.Create(TPathPointKind.Close, FStartPoint));
  DoChanged;
end;

procedure TPathData.AddPath(APath: TPathData);
var
  I: Integer;
begin
  FPathData.Capacity := FPathData.Count + APath.Count;
  for I := 0 to APath.Count - 1 do
    FPathData.Add(APath.Points[I]);
  DoChanged;
end;

procedure TPathData.Clear;
begin
  FPathData.Clear;
  DoChanged;
end;

function TPathData.GetBounds: TRectF;
const
  SmallAmount = 0.001;
var
  I: Integer;
begin
  if FPathData.Count < 1 then
    Exit(TRectF.Create(0, 0, 0, 0));
  if FRecalcBounds then
  begin
    Result := TRectF.Create($FFFF, $FFFF, -$FFFF, -$FFFF);
    for I := 0 to FPathData.Count - 1 do
    begin
      if FPathData[I].Kind = TPathPointKind.Close then
        Continue;

      if FPathData[I].Point.X < Result.Left then
        Result.Left := FPathData[I].Point.X;
      if FPathData[I].Point.X > Result.Right then
        Result.Right := FPathData[I].Point.X;
      if FPathData[I].Point.Y < Result.Top then
        Result.Top := FPathData[I].Point.Y;
      if FPathData[I].Point.Y > Result.Bottom then
        Result.Bottom := FPathData[I].Point.Y;
    end;
    // add small amount
    if SameValue(Result.Width, 0, TEpsilon.Position) then
      Result.Right := Result.Left + SmallAmount;
    if SameValue(Result.Height, 0, TEpsilon.Position) then
      Result.Bottom := Result.Top + SmallAmount;
    FBounds := Result;
    FRecalcBounds := False;
  end
  else
    Result := FBounds;
end;

procedure TPathData.Scale(const ScaleX, ScaleY: Single);
var
  I: Integer;
  ScalePoint: TPointF;
begin
  if FPathData.Count > 0 then
  begin
    ScalePoint := TPointF.Create(ScaleX, ScaleY);
    for I := 0 to FPathData.Count - 1 do
      if FPathData[I].Kind in [TPathPointKind.MoveTo, TPathPointKind.LineTo, TPathPointKind.CurveTo] then
        FPathData[I] := TPathPoint.Create(FPathData[I].Kind, FPathData[I].Point * ScalePoint);
    DoChanged;
  end;
end;

procedure TPathData.Scale(const AScale: TPointF);
begin
  Scale(AScale.X, AScale.Y);
end;

procedure TPathData.Translate(const DX, DY: Single);
var
  I: Integer;
  TranslatePoint: TPointF;
begin
  if FPathData.Count > 0 then
  begin
    TranslatePoint := TPointF.Create(DX, DY);
    for I := 0 to FPathData.Count - 1 do
      if FPathData[I].Kind in [TPathPointKind.MoveTo, TPathPointKind.LineTo, TPathPointKind.CurveTo] then
        FPathData[I] := TPathPoint.Create(FPathData[I].Kind, FPathData[I].Point + TranslatePoint);
    DoChanged;
  end;
end;

procedure TPathData.Translate(const Delta: TPointF);
begin
  Translate(Delta.X, Delta.Y);
end;

procedure TPathData.FitToRect(const ARect: TRectF);
var
  FitBounds, Bounds: TRectF;
  Ratio: Single;
begin
  Bounds := GetBounds;
  FitBounds := Bounds.FitInto(ARect, Ratio);
  if not SameValue(Ratio, 0, TEpsilon.Scale) then
  begin
    Ratio := 1 / Ratio;
    Translate(-Bounds.Left, -Bounds.Top);
    Scale(Ratio, Ratio);
    Translate(FitBounds.Left, FitBounds.Top);
  end;
end;

procedure TPathData.ApplyMatrix(const M: TMatrix);
var
  I: Integer;
begin
  if FPathData.Count > 0 then
  begin
    for I := 0 to FPathData.Count - 1 do
      if FPathData[I].Kind in [TPathPointKind.MoveTo, TPathPointKind.LineTo, TPathPointKind.CurveTo] then
        FPathData[I] := TPathPoint.Create(FPathData[I].Kind, FPathData[I].Point * M);
    DoChanged;
  end;
end;

procedure TPathData.CalculateBezierCoefficients(const Bezier: TCubicBezier; out AX, BX, CX, AY, BY, CY: Single);
begin
  CX := 3 * (Bezier[1].X - Bezier[0].X);
  CY := 3 * (Bezier[1].Y - Bezier[0].Y);
  BX := 3 * (Bezier[2].X - Bezier[1].X) - CX;
  BY := 3 * (Bezier[2].Y - Bezier[1].Y) - CY;
  AX := Bezier[3].X - Bezier[0].X - CX - BX;
  AY := Bezier[3].Y - Bezier[0].Y - CY - BY;
end;

function TPathData.PointOnBezier(const StartPoint: TPointF; const AX, BX, CX, AY, BY, CY, T: Single): TPointF;
var
  SquareT, CubeT: Single;
begin
  SquareT := T * T;
  CubeT := SquareT * T;
  Result.X := (AX * CubeT) + (BX * SquareT) + (CX * T) + StartPoint.X;
  Result.Y := (AY * CubeT) + (BY * SquareT) + (CY * T) + StartPoint.Y;
end;

function TPathData.CreateBezier(const Bezier: TCubicBezier; const PointCount: Integer): TPolygon;
var
  AX, BX, CX, AY, BY, CY, DT, T: Single;
  I: Integer;
begin
  if PointCount = 0 then
    Exit;
  DT := 1 / (1 * PointCount - 1);
  T := 0;
  SetLength(Result, PointCount);
  CalculateBezierCoefficients(Bezier, AX, BX, CX, AY, BY, CY);
  for I := 0 to PointCount - 1 do
  begin
    Result[I] := PointOnBezier(Bezier[0], AX, BX, CX, AY, BY, CY, T);
    T := T + DT;
  end;
end;

procedure TPathData.Flatten(const Flatness: Single);
var
  J, I: Integer;
  BPts: TPolygon;
  B: TCubicBezier;
  F, Len: Single;
  SegCount: Integer;
  OldPathData: TList<TPathPoint>;
  CurPoint: TPointF;
begin
  if FPathData.Count > 0 then
  begin
    F := Max(Flatness, MinFlatness);
    OldPathData := TList<TPathPoint>.Create;
    try
      OldPathData.Count := FPathData.Count;
      for J := 0 to FPathData.Count - 1 do
        OldPathData.Add(FPathData[J]);
      FPathData.Clear;
      J := 0;
      while J < OldPathData.Count do
      begin
        case OldPathData[J].Kind of
          TPathPointKind.MoveTo:
            begin
              MoveTo(OldPathData[J].Point);
              CurPoint := OldPathData[J].Point;
            end;
          TPathPointKind.LineTo:
            begin
              LineTo(OldPathData[J].Point);
              CurPoint := OldPathData[J].Point;
            end;
          TPathPointKind.CurveTo:
            begin
              B[0] := CurPoint;
              B[1] := OldPathData[J].Point;
              Inc(J);
              B[2] := OldPathData[J].Point;
              Inc(J);
              B[3] := OldPathData[J].Point;
              BPts := CreateBezier(B, 6);
              Len := 0;
              for I := 0 to High(BPts) - 1 do
                Len := Len + (BPts[I] - BPts[I + 1]).Length;
              SegCount := Round(Len / F);
              if SegCount < 2 then
                LineTo(B[3])
              else
              begin
                BPts := CreateBezier(B, SegCount);
                for I := 0 to High(BPts) do
                  LineTo(BPts[I]);
                CurPoint := OldPathData[J].Point;
              end;
            end;
          TPathPointKind.Close:
            ClosePath;
        end;
        Inc(J);
      end;
      DoChanged(False);
    finally
      OldPathData.Free;
    end;
  end;
end;

function TPathData.FlattenToPolygon(var Polygon: TPolygon; const Flatness: Single = 0.25): TPointF;

  procedure AddPoint(const P: TPointF);
  begin
    if (Length(Polygon) > 0) and (SameValue(P.X, Polygon[High(Polygon)].X, TEpsilon.Position) and SameValue(P.Y,
      Polygon[High(Polygon)].Y, TEpsilon.Position)) then
      Exit;
    SetLength(Polygon, Length(Polygon) + 1);
    Polygon[High(Polygon)] := P;
  end;

var
  J, I: Integer;
  BPts: TPolygon;
  B: TCubicBezier;
  SP, CurPoint: TPointF;
  F, Len: Single;
  SegCount: Integer;
  CurBounds: TRectF;
begin
  Result := TPointF.Zero;
  SetLength(Polygon, 0);
  if FPathData.Count > 0 then
  begin
    F := Max(Flatness, MinFlatness);
    J := 0;
    while J < FPathData.Count do
    begin
      case FPathData[J].Kind of
        TPathPointKind.MoveTo:
          begin
            if Length(Polygon) > 0 then
              AddPoint(PolygonPointBreak);
            AddPoint(FPathData[J].Point);
            CurPoint := FPathData[J].Point;
            SP := CurPoint;
          end;
        TPathPointKind.LineTo:
          begin
            AddPoint(FPathData[J].Point);
            CurPoint := FPathData[J].Point;
          end;
        TPathPointKind.CurveTo:
          begin
            B[0] := CurPoint;
            B[1] := FPathData[J].Point;
            Inc(J);
            B[2] := FPathData[J].Point;
            Inc(J);
            B[3] := FPathData[J].Point;
            BPts := CreateBezier(B, 6);
            Len := 0;
            for I := 0 to High(BPts) - 1 do
              Len := Len + (BPts[I] - BPts[I + 1]).Length;
            SegCount := Round(Len / F);
            if SegCount < 2 then
            begin
              AddPoint(B[0]);
              AddPoint(B[3]);
            end
            else
            begin
              BPts := CreateBezier(B, SegCount);
              for I := 0 to High(BPts) do
                AddPoint(BPts[I]);
            end;
            CurPoint := FPathData[J].Point;
          end;
        TPathPointKind.Close:
          begin
            AddPoint(SP);
            AddPoint(PolygonPointBreak);
          end;
      end;
      Inc(J);
    end;
    CurBounds := GetBounds;
    Result := TPointF.Create(Abs(CurBounds.Width), Abs(CurBounds.Height));
  end;
end;

procedure TPathData.FreeNotification(AObject: TObject);
begin
  if FStyleResource = AObject then
    FStyleResource := nil;
end;

procedure TPathData.AddEllipse(const ARect: TRectF);
var
  CX, CY, PX, PY: Single;
begin
  CX := (ARect.Left + ARect.Right) / 2;
  CY := (ARect.Top + ARect.Bottom) / 2;
  PX := CurveKappa * (ARect.Width / 2);
  PY := CurveKappa * (ARect.Height / 2);
  MoveTo(TPointF.Create(ARect.Left, CY));
  CurveTo(TPointF.Create(ARect.Left, CY - PY), TPointF.Create(CX - PX, ARect.Top), TPointF.Create(CX, ARect.Top));
  CurveTo(TPointF.Create(CX + PX, ARect.Top), TPointF.Create(ARect.Right, CY - PY), TPointF.Create(ARect.Right, CY));
  CurveTo(TPointF.Create(ARect.Right, CY + PY), TPointF.Create(CX + PX, ARect.Bottom), TPointF.Create(CX, ARect.Bottom));
  CurveTo(TPointF.Create(CX - PX, ARect.Bottom), TPointF.Create(ARect.Left, CY + PY), TPointF.Create(ARect.Left, CY));
end;

procedure TPathData.AddRectangle(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const ACornerType: TCornerType = TCornerType.Round);
var
  X1, X2, Y1, Y2: Single;
begin
  if SameValue(XRadius, 0, TEpsilon.Vector) or SameValue(YRadius, 0, TEpsilon.Vector) then
  begin
    MoveTo(ARect.TopLeft);
    LineTo(TPointF.Create(ARect.Right, ARect.Top));
    LineTo(ARect.BottomRight);
    LineTo(TPointF.Create(ARect.Left, ARect.Bottom));
  end
  else
  begin
    X1 := XRadius;
    if ARect.Width - (X1 * 2) < 0 then
      X1 := XRadius * (ARect.Width / (X1 * 2));
    X2 := X1 / 2;
    Y1 := YRadius;
    if ARect.Height - (Y1 * 2) < 0 then
      Y1 := YRadius * (ARect.Height / (Y1 * 2));
    Y2 := Y1 / 2;
    MoveTo(TPointF.Create(ARect.Left, ARect.Top + Y1));
    if TCorner.TopLeft in ACorners then
    begin
      case ACornerType of
        TCornerType.Bevel:
          LineTo(TPointF.Create(ARect.Left + X1, ARect.Top));
        TCornerType.InnerRound:
          CurveTo(TPointF.Create(ARect.Left + X2, ARect.Top + Y1), TPointF.Create(ARect.Left + X1, ARect.Top + Y2),
            TPointF.Create(ARect.Left + X1, ARect.Top));
        TCornerType.InnerLine:
          begin
            LineTo(TPointF.Create(ARect.Left + X2, ARect.Top + Y1));
            LineTo(TPointF.Create(ARect.Left + X1, ARect.Top + Y2));
            LineTo(TPointF.Create(ARect.Left + X1, ARect.Top));
          end;
      else
        CurveTo(TPointF.Create(ARect.Left, ARect.Top + Y2), TPointF.Create(ARect.Left + X2, ARect.Top),
          TPointF.Create(ARect.Left + X1, ARect.Top));
      end;
    end
    else
    begin
      LineTo(TPointF.Create(ARect.Left, ARect.Top));
      LineTo(TPointF.Create(ARect.Left + X1, ARect.Top));
    end;
    LineTo(TPointF.Create(ARect.Right - X1, ARect.Top));
    if TCorner.TopRight in ACorners then
    begin
      case ACornerType of
        TCornerType.Bevel:
          LineTo(TPointF.Create(ARect.Right, ARect.Top + Y1));
        TCornerType.InnerRound:
          CurveTo(TPointF.Create(ARect.Right - X1, ARect.Top + Y2), TPointF.Create(ARect.Right - X2, ARect.Top + Y1),
            TPointF.Create(ARect.Right, ARect.Top + Y1));
        TCornerType.InnerLine:
          begin
            LineTo(TPointF.Create(ARect.Right - X1, ARect.Top + Y2));
            LineTo(TPointF.Create(ARect.Right - X2, ARect.Top + Y1));
            LineTo(TPointF.Create(ARect.Right, ARect.Top + Y1));
          end;
      else
        CurveTo(TPointF.Create(ARect.Right - X2, ARect.Top), TPointF.Create(ARect.Right, ARect.Top + Y2),
          TPointF.Create(ARect.Right, ARect.Top + Y1));
      end;
    end
    else
    begin
      LineTo(TPointF.Create(ARect.Right, ARect.Top));
      LineTo(TPointF.Create(ARect.Right, ARect.Top + Y1));
    end;
    LineTo(TPointF.Create(ARect.Right, ARect.Bottom - Y1));
    if TCorner.BottomRight in ACorners then
    begin
      case ACornerType of
        TCornerType.Bevel:
          LineTo(TPointF.Create(ARect.Right - X1, ARect.Bottom));
        TCornerType.InnerRound:
          CurveTo(TPointF.Create(ARect.Right - X2, ARect.Bottom - Y1), TPointF.Create(ARect.Right - X1, ARect.Bottom - Y2),
            TPointF.Create(ARect.Right - X1, ARect.Bottom));
        TCornerType.InnerLine:
          begin
            LineTo(TPointF.Create(ARect.Right - X2, ARect.Bottom - Y1));
            LineTo(TPointF.Create(ARect.Right - X1, ARect.Bottom - Y2));
            LineTo(TPointF.Create(ARect.Right - X1, ARect.Bottom));
          end;
      else
        CurveTo(TPointF.Create(ARect.Right, ARect.Bottom - Y2), TPointF.Create(ARect.Right - X2, ARect.Bottom),
          TPointF.Create(ARect.Right - X1, ARect.Bottom));
      end;
    end
    else
    begin
      LineTo(TPointF.Create(ARect.Right, ARect.Bottom));
      LineTo(TPointF.Create(ARect.Right - X1, ARect.Bottom));
    end;
    LineTo(TPointF.Create(ARect.Left + X1, ARect.Bottom));
    if TCorner.BottomLeft in ACorners then
    begin
      case ACornerType of
        TCornerType.Bevel:
          LineTo(TPointF.Create(ARect.Left, ARect.Bottom - Y1));
        TCornerType.InnerRound:
          CurveTo(TPointF.Create(ARect.Left + X1, ARect.Bottom - Y2), TPointF.Create(ARect.Left + X2, ARect.Bottom - Y1),
            TPointF.Create(ARect.Left, ARect.Bottom - Y1));
        TCornerType.InnerLine:
          begin
            LineTo(TPointF.Create(ARect.Left + X1, ARect.Bottom - Y2));
            LineTo(TPointF.Create(ARect.Left + X2, ARect.Bottom - Y1));
            LineTo(TPointF.Create(ARect.Left, ARect.Bottom - Y1));
          end;
      else
        CurveTo(TPointF.Create(ARect.Left + X2, ARect.Bottom), TPointF.Create(ARect.Left, ARect.Bottom - (Y2)),
          TPointF.Create(ARect.Left, ARect.Bottom - Y1));
      end;
    end
    else
    begin
      LineTo(TPointF.Create(ARect.Left, ARect.Bottom));
      LineTo(TPointF.Create(ARect.Left, ARect.Bottom - Y1));
    end;
  end;
  ClosePath;
end;

procedure DrawArcWithBezier(Path: TPathData; CenterX, CenterY, RadiusX, RadiusY, StartAngle, SweepRange: Single;
  UseMoveTo: Boolean);
var
  Coord: array of TPointF;
  Pts: array of TPointF;
  A, B, C, X, Y: Single;
  SS, CC: Single;
  I: Integer;
begin
  if SweepRange = 0 then
  begin
    if UseMoveTo then
    begin
      if Path.FPathData.Count < 1 then
        Path.MoveTo(TPointF.Create(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)))
      else
        Path.LineTo(TPointF.Create(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)));
    end;
    Path.LineTo(TPointF.Create(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)));
    Exit;
  end;
  SinCos(SweepRange / 2, B, C);
  A := 1 - C;
  X := A * 4 / 3;
  Y := B - X * C / B;
  SinCos(StartAngle + SweepRange / 2, SS, CC);
  SetLength(Coord, 4);
  Coord[0] := TPointF.Create(C, -B);
  Coord[1] := TPointF.Create(C + X, -Y);
  Coord[2] := TPointF.Create(C + X, Y);
  Coord[3] := TPointF.Create(C, B);
  SetLength(Pts, 4);
  for I := 0 to 3 do
  begin
    Pts[I] := TPointF.Create(CenterX + RadiusX * (Coord[I].X * CC - Coord[I].Y * SS), CenterY + RadiusY * (Coord[I].X *
      SS + Coord[I].Y * CC));
  end;
  if UseMoveTo then
  begin
    if Path.FPathData.Count < 1 then
      Path.MoveTo(Pts[0])
    else
      Path.LineTo(Pts[0]);
  end;
  Path.CurveTo(Pts[1], Pts[2], Pts[3]);
end;

procedure TPathData.AddArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
const
  BezierArcAngleEpsilon = 0.01;
  MinSweepAngle = 1E-10;
var
  UseMoveTo: Boolean;
  I: Integer;
  F: Single;
  TotalSweep, LocalSweep, PrevSweep: Single;
  Done: Boolean;
begin
  StartAngle := DegToRad(StartAngle);
  SweepAngle := DegToRad(SweepAngle);
  I := Trunc(StartAngle / (2 * Pi));
  F := StartAngle - (I * 2 * Pi);
  StartAngle := F;
  if SweepAngle >= 2 * Pi then
    SweepAngle := 2 * Pi;
  if SweepAngle <= -2 * Pi then
    SweepAngle := -2 * Pi;
  if Abs(SweepAngle) < MinSweepAngle then
    Exit;
  TotalSweep := 0;
  Done := False;
  UseMoveTo := True;
  repeat
    if SweepAngle < 0 then
    begin
      PrevSweep := TotalSweep;
      LocalSweep := -Pi / 2;
      TotalSweep := TotalSweep - (Pi / 2);
      if TotalSweep <= SweepAngle + BezierArcAngleEpsilon then
      begin
        LocalSweep := SweepAngle - PrevSweep;
        Done := True;
      end;
    end
    else
    begin
      PrevSweep := TotalSweep;
      LocalSweep := Pi / 2;
      TotalSweep := TotalSweep + (Pi / 2);
      if TotalSweep >= SweepAngle - BezierArcAngleEpsilon then
      begin
        LocalSweep := SweepAngle - PrevSweep;
        Done := True;
      end;
    end;
    DrawArcWithBezier(Self, Center.X, Center.Y, Radius.X, Radius.Y, StartAngle, LocalSweep, UseMoveTo);
    UseMoveTo := False;
    StartAngle := StartAngle + LocalSweep;
  until Done;
end;

procedure TPathData.AddArcSvgPart(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
const
  BezierArcAngleEpsilon = 0.01;
  MinSweepAngle = 1E-10;
var
  UseMoveTo: Boolean;
  I: Integer;
  F: Single;
  TotalSweep, LocalSweep, PrevSweep: Single;
  Done: Boolean;
begin
  StartAngle := DegToRad(StartAngle);
  SweepAngle := DegToRad(SweepAngle);
  I := Trunc(StartAngle / (2 * Pi));
  F := StartAngle - (I * 2 * Pi);
  StartAngle := F;
  if SweepAngle >= 2 * Pi then
    SweepAngle := 2 * Pi;
  if SweepAngle <= -2 * Pi then
    SweepAngle := -2 * Pi;
  if Abs(SweepAngle) < MinSweepAngle then
    Exit;
  TotalSweep := 0;
  Done := False;
  UseMoveTo := False;
  repeat
    if SweepAngle < 0 then
    begin
      PrevSweep := TotalSweep;
      LocalSweep := -Pi / 2;
      TotalSweep := TotalSweep - (Pi / 2);
      if TotalSweep <= SweepAngle + BezierArcAngleEpsilon then
      begin
        LocalSweep := SweepAngle - PrevSweep;
        Done := True;
      end;
    end
    else
    begin
      PrevSweep := TotalSweep;
      LocalSweep := Pi / 2;
      TotalSweep := TotalSweep + (Pi / 2);
      if TotalSweep >= SweepAngle - BezierArcAngleEpsilon then
      begin
        LocalSweep := SweepAngle - PrevSweep;
        Done := True;
      end;
    end;
    DrawArcWithBezier(Self, Center.X, Center.Y, Radius.X, Radius.Y, StartAngle, LocalSweep, UseMoveTo);
    UseMoveTo := False;
    StartAngle := StartAngle + LocalSweep;
  until Done;
end;

procedure TPathData.AddArcSvg(const P1, Radius: TPointF; Angle: Single; const LargeFlag, SweepFlag: Boolean;
  const P2: TPointF);
var
  I: Integer;
  RadOk: Boolean;
  V, P, N, Sq, Rx, Ry, X0, Y0, X1, Y1, X2, Y2, CX, CY, Ux, Uy, Vx, Vy: Single;
  Dx2, Dy2, Prx, Pry, Px1, Py1, Cx1, Cy1, Sx2, Sy2, Sign, Coef: Single;
  RadCheck, StartAngle, SweepAngle, CosA, SinA: Single;
  M: TMatrix;
  Len: Integer;
begin
  // Trivial case: Arc transformate to point
  if P1 = P2 then
    Exit;
  Rx := Radius.X;
  Ry := Radius.Y;
  X0 := P1.X;
  Y0 := P1.Y;
  X2 := P2.X;
  Y2 := P2.Y;
  Angle := DegToRad(Angle);
  RadOk := True;
  if Rx < 0 then
    Rx := -Rx;
  if Ry < 0 then
    Ry := -Rx;
  // Calculate the middle point between the current and the final points
  Dx2 := (X0 - X2) / 2;
  Dy2 := (Y0 - Y2) / 2;
  // Convert angle from degrees to radians
  SinCos(Angle, SinA, CosA);
  // Calculate (x1, y1)
  X1 := CosA * Dx2 + SinA * Dy2;
  Y1 := -SinA * Dx2 + CosA * Dy2;
  // Ensure radii are large enough
  Prx := Rx * Rx;
  Pry := Ry * Ry;
  Px1 := X1 * X1;
  Py1 := Y1 * Y1;
  // Check that radii are large enough
  RadCheck := Px1 / Prx + Py1 / Pry;
  if RadCheck > 1 then
  begin
    Rx := Sqrt(RadCheck) * Rx;
    Ry := Sqrt(RadCheck) * Ry;
    Prx := Rx * Rx;
    Pry := Ry * Ry;
    if RadCheck > 10 then
      RadOk := False;
  end;
  // Calculate (Cx1, Cy1)
  if LargeFlag = SweepFlag then
    Sign := -1
  else
    Sign := 1;
  Sq := (Prx * Pry - Prx * Py1 - Pry * Px1) / (Prx * Py1 + Pry * Px1);
  if Sq < 0 then
    Coef := Sign * Sqrt(0)
  else
    Coef := Sign * Sqrt(Sq);
  Cx1 := Coef * ((Rx * Y1) / Ry);
  Cy1 := Coef * -((Ry * X1) / Rx);
  // Calculate (CX, CY) from (Cx1, Cy1)
  Sx2 := (X0 + X2) / 2;
  Sy2 := (Y0 + Y2) / 2;
  CX := Sx2 + (CosA * Cx1 - SinA * Cy1);
  CY := Sy2 + (SinA * Cx1 + CosA * Cy1);
  // Calculate the StartAngle (angle1) and the SweepAngle (dangle)
  Ux := (X1 - Cx1) / Rx;
  Uy := (Y1 - Cy1) / Ry;
  Vx := (-X1 - Cx1) / Rx;
  Vy := (-Y1 - Cy1) / Ry;
  // Calculate the angle start
  N := Sqrt(Ux * Ux + Uy * Uy);
  P := Ux; // (1 * Ux ) + (0 * Uy )
  if Uy < 0 then
    Sign := -1
  else
    Sign := 1;
  V := P / N;
  if V < -1 then
    V := -1;
  if V > 1 then
    V := 1;
  StartAngle := Sign * ArcCos(V);
  // Calculate the sweep angle
  N := Sqrt((Ux * Ux + Uy * Uy) * (Vx * Vx + Vy * Vy));
  P := Ux * Vx + Uy * Vy;
  if Ux * Vy - Uy * Vx < 0 then
    Sign := -1
  else
    Sign := 1;
  V := P / N;
  if V < -1 then
    V := -1;
  if V > 1 then
    V := 1;
  SweepAngle := Sign * ArcCos(V);
  if (not SweepFlag) and (SweepAngle > 0) then
    SweepAngle := SweepAngle - Pi * 2
  else if SweepFlag and (SweepAngle < 0) then
    SweepAngle := SweepAngle + Pi * 2;
  Len := Count;
  AddArcSvgPart(TPointF.Zero, TPointF.Create(Rx, Ry), RadToDeg(StartAngle), RadToDeg(SweepAngle));
  M := TMatrix.Identity;
  M.m31 := CX;
  M.m32 := CY;
  M := TMatrix.CreateRotation(Angle) * M;
  I := Len;
  while I < Count do
  begin
    FPathData[I] := TPathPoint.Create(FPathData[I].Kind, FPathData[I].Point * M);
    Inc(I);
  end;
end;

function TPathData.IsEmpty: Boolean;
begin
  Result := (FPathData.Count < 1) or (GetBounds.Width * GetBounds.Height = 0);
end;

function TPathData.GetPathString: string;
var
  I: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  Result := Builder.ToString;
  try
    I := 0;
    while I < Count do
    begin
      case FPathData[I].Kind of
        TPathPointKind.MoveTo:
          Builder.Append('M')
                 .Append(FloatToStr(FPathData[I].Point.X, USFormatSettings))
                 .Append(',')
                 .Append(FloatToStr(FPathData[I].Point.Y, USFormatSettings))
                 .Append(' ');
        TPathPointKind.LineTo:
          Builder.Append('L')
                 .Append(FloatToStr(FPathData[I].Point.X, USFormatSettings))
                 .Append(',')
                 .Append(FloatToStr(FPathData[I].Point.Y, USFormatSettings))
                 .Append(' ');
        TPathPointKind.CurveTo:
          begin
            Builder.Append('C')
                   .Append(FloatToStr(FPathData[I].Point.X, USFormatSettings))
                   .Append(',')
                   .Append(FloatToStr(FPathData[I].Point.Y, USFormatSettings))
                   .Append(' ');

            Builder.Append(FloatToStr(FPathData[I + 1].Point.X, USFormatSettings))
                   .Append(',')
                   .Append(FloatToStr(FPathData[I + 1].Point.Y, USFormatSettings))
                   .Append(' ');

            Builder.Append(FloatToStr(FPathData[I + 2].Point.X, USFormatSettings))
                   .Append(',')
                   .Append(FloatToStr(FPathData[I + 2].Point.Y, USFormatSettings))
                   .Append(' ');

            Inc(I, 2);
          end;
        TPathPointKind.Close:
          Builder.Append('Z ');
      end;
      Inc(I);
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function TPathData.GetTokensFromString(const PathString: string; var Pos: Integer): string;
var
  StringBuilder: TStringBuilder;
begin
  if Pos < PathString.Length then
  begin
    StringBuilder := TStringBuilder.Create;
    try
      while (Pos < PathString.Length) and (PathString.Chars[Pos] = ' ') do
        Inc(Pos);
      while (Pos < PathString.Length) and ('zmlchvsqtaZMLCHVSQTA'.Contains(PathString.Chars[Pos])) do
      begin
        StringBuilder.Append(PathString.Chars[Pos]);
        Inc(Pos);
      end;
      Result := StringBuilder.ToString;
    finally
      StringBuilder.Free;
    end;
  end
  else
    Result := string.Empty;
end;

function TPathData.GetNumberFromString(const PathString: string; var Pos: Integer): string;
var
  StringBuilder: TStringBuilder;
begin
  if Pos < PathString.Length then
  begin
    StringBuilder := TStringBuilder.Create;
    try
      while (Pos < PathString.Length) and (PathString.Chars[Pos] = ' ') do
        Inc(Pos);
      while Pos < PathString.Length do
      begin
        if PathString.Chars[Pos] = 'e' then
        begin
          StringBuilder.Append(PathString.Chars[Pos]);
          Inc(Pos);
          Continue;
        end;
        if (PathString.Chars[Pos] = '-') and (StringBuilder.Length > 0) and
          (StringBuilder.Chars[StringBuilder.Length - 1] = 'e') then
        begin
          StringBuilder.Append(PathString.Chars[Pos]);
          Inc(Pos);
          Continue;
        end;
        if (StringBuilder.Length > 0) and (PathString.Chars[Pos] = '-') then
          Break;
       if not '0123456789.'.Contains(PathString.Chars[Pos]) and not (PathString.Chars[Pos] = '-') then
          Break;
        StringBuilder.Append(PathString.Chars[Pos]);
        Inc(Pos);
      end;
      while PathString.Chars[Pos] = ' ' do
        Inc(Pos);
      Result := StringBuilder.ToString;
    finally
      StringBuilder.Free;
    end;
  end
  else
    Result := string.Empty;
end;

function TPathData.GetPointFromString(const PathString: string; var Pos: Integer): TPointF;
var
  X, Y: string;
begin
  Result := TPointF.Zero;
  if Pos < PathString.Length then
  begin
    while (Pos < PathString.Length) and PathString.Chars[Pos].IsInArray([',', ' ']) do
      Inc(Pos);
    X := GetNumberFromString(PathString, Pos);
    while (Pos < PathString.Length) and PathString.Chars[Pos].IsInArray([',', ' ']) do
      Inc(Pos);
    Y := GetNumberFromString(PathString, Pos);
    while (Pos < PathString.Length) and PathString.Chars[Pos].IsInArray([',', ' ']) do
      Inc(Pos);
    Result := TPointF.Create(StrToFloat(X, USFormatSettings), StrToFloat(Y, USFormatSettings));
  end;
end;

function TPathData.HasRelativeOffset(const PathString: string; const Pos: Integer): Boolean;
begin
  Result := PathString.Chars[Pos].IsDigit or (PathString.Chars[Pos] = '-');
end;

procedure TPathData.SetPathString(const Value: string);
var
  Builder, TokenBuilder: TStringBuilder;
  PathString, Tokens: string;
  Radius, CurvePoint1, CurvePoint2, TempPoint: TPointF;
  Large, Sweet: Boolean;
  Pos, I, LastLength: Integer;
  Angle: Single;
  Token: Char;
begin
  Builder := TStringBuilder.Create;
  TokenBuilder := TStringBuilder.Create;
  try
    for I := 0 to Value.Length - 1 do
    begin
      if Value.Chars[I].IsInArray([#9, #10, #13]) then
        Builder.Append(' ')
      else
        Builder.Append(Value.Chars[I]);
    end;
    PathString := Builder.ToString;
    FPathData.Clear;
    Pos := 0;
    LastLength := -1;
    while (Builder.Length > Pos) and (LastLength <> Pos) do
    begin
      LastLength := Pos;
      Tokens := GetTokensFromString(PathString, Pos);
      TokenBuilder.Clear;
      TokenBuilder.Append(Tokens);
      while TokenBuilder.Length > 0 do
      begin
        Token := TokenBuilder.Chars[0];
        TokenBuilder.Remove(0, 1);
        case Token of
          'z', 'Z': ClosePath;
          'M':
            begin
              MoveTo(GetPointFromString(PathString, Pos));
              while HasRelativeOffset(PathString, Pos) do
                LineTo(GetPointFromString(PathString, Pos));
            end;
          'm':
            begin
              MoveToRel(GetPointFromString(PathString, Pos));
              while HasRelativeOffset(PathString, Pos) do
                LineToRel(GetPointFromString(PathString, Pos));
            end;
          'L':
            begin
              LineTo(GetPointFromString(PathString, Pos));
              while HasRelativeOffset(PathString, Pos) do
                LineTo(GetPointFromString(PathString, Pos));
            end;
          'l':
            begin
              LineToRel(GetPointFromString(PathString, Pos));
              while HasRelativeOffset(PathString, Pos) do
                LineToRel(GetPointFromString(PathString, Pos));
            end;
          'C':
            begin
              CurvePoint1 := GetPointFromString(PathString, Pos);
              CurvePoint2 := GetPointFromString(PathString, Pos);
              CurveTo(CurvePoint1, CurvePoint2, GetPointFromString(PathString, Pos));
              while HasRelativeOffset(PathString, Pos) do
              begin
                CurvePoint1 := GetPointFromString(PathString, Pos);
                CurvePoint2 := GetPointFromString(PathString, Pos);
                CurveTo(CurvePoint1, CurvePoint2, GetPointFromString(PathString, Pos));
              end;
            end;
          'c':
            begin
              CurvePoint1 := GetPointFromString(PathString, Pos);
              CurvePoint2 := GetPointFromString(PathString, Pos);
              CurveToRel(CurvePoint1, CurvePoint2, GetPointFromString(PathString, Pos));
              while HasRelativeOffset(PathString, Pos) do
              begin
                CurvePoint1 := GetPointFromString(PathString, Pos);
                CurvePoint2 := GetPointFromString(PathString, Pos);
                CurveToRel(CurvePoint1, CurvePoint2, GetPointFromString(PathString, Pos));
              end;
            end;
          'S':
            begin
              CurvePoint2 := GetPointFromString(PathString, Pos);
              SmoothCurveTo(CurvePoint2, GetPointFromString(PathString, Pos));
              while HasRelativeOffset(PathString, Pos) do
              begin
                CurvePoint2 := GetPointFromString(PathString, Pos);
                SmoothCurveTo(CurvePoint2, GetPointFromString(PathString, Pos));
              end;
            end;
          's':
            begin
              CurvePoint2 := GetPointFromString(PathString, Pos);
              SmoothCurveToRel(CurvePoint2, GetPointFromString(PathString, Pos));
              while HasRelativeOffset(PathString, Pos) do
              begin
                CurvePoint2 := GetPointFromString(PathString, Pos);
                SmoothCurveToRel(CurvePoint2, GetPointFromString(PathString, Pos));
              end;
            end;
          'H': HLineTo(StrToFloat(GetNumberFromString(PathString, Pos), USFormatSettings));
          'h': HLineToRel(StrToFloat(GetNumberFromString(PathString, Pos), USFormatSettings));
          'V': VLineTo(StrToFloat(GetNumberFromString(PathString, Pos), USFormatSettings));
          'v': VLineToRel(StrToFloat(GetNumberFromString(PathString, Pos), USFormatSettings));
          'Q', 'q':
            begin
              GetPointFromString(PathString, Pos);
              GetPointFromString(PathString, Pos);
            end;
          'T', 't': GetPointFromString(PathString, Pos);
          'A', 'a':
            begin
              if Count > 0 then
                CurvePoint1 := FPathData[FPathData.Count - 1].Point
              else
                CurvePoint1 := TPointF.Zero;
              Radius := GetPointFromString(PathString, Pos);
              Angle := StrToFloat(GetNumberFromString(PathString, Pos), USFormatSettings);
              TempPoint := GetPointFromString(PathString, Pos);
              Large := TempPoint.X = 1;
              Sweet := TempPoint.Y = 1;
              CurvePoint2 := GetPointFromString(PathString, Pos);
              if Token = 'a' then
                CurvePoint2 := CurvePoint1 + CurvePoint2;
              AddArcSvg(CurvePoint1, Radius, Angle, Large, Sweet, CurvePoint2);
            end;
        end;
      end;
    end;
    DoChanged;
  finally
    TokenBuilder.Free;
    Builder.Free;
  end;
end;

{ TCanvasManager }

class procedure TCanvasManager.UnInitialize;
var
  CanvasSrv: IFMXCanvasService;
begin
  FreeAndNil(FMeasureBitmap);
  FreeAndNil(FCanvasList);
  if (TPlatformServices.Current <> nil) and TPlatformServices.Current.SupportsPlatformService(IFMXCanvasService,
    CanvasSrv) then
    CanvasSrv.UnregisterCanvasClasses;
end;

class function TCanvasManager.CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault): TCanvas;
begin
  Result := DefaultCanvas.CreateFromBitmap(ABitmap, AQuality);
end;

class function TCanvasManager.CreateFromPrinter(const APrinter: TAbstractPrinter): TCanvas;
begin
  Result := DefaultPrinterCanvas.CreateFromPrinter(APrinter);
end;

class function TCanvasManager.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault): TCanvas;
begin
  Result := DefaultCanvas.CreateFromWindow(AParent, AWidth, AHeight, AQuality);
end;

class function TCanvasManager.GetDefaultCanvas: TCanvasClass;
var
  CanvasSrv: IFMXCanvasService;
  CanvasClassRec: TCanvasClassRec;
begin
  if FDefaultCanvasClass = nil then
  begin
    Result := nil;
    if (FCanvasList = nil) and TPlatformServices.Current.SupportsPlatformService(IFMXCanvasService, CanvasSrv) then
      CanvasSrv.RegisterCanvasClasses;
    if (FCanvasList <> nil) and (FCanvasList.Count > 0) then
    begin
      for CanvasClassRec in FCanvasList do
      begin
        if CanvasClassRec.Default and
          (not FEnableSoftwareCanvas and (TCanvasStyle.NeedGPUSurface in CanvasClassRec.CanvasClass.GetCanvasStyle)) then
        begin
          Result := CanvasClassRec.CanvasClass;
          Break;
        end;
        if CanvasClassRec.Default and
          (FEnableSoftwareCanvas and not (TCanvasStyle.NeedGPUSurface in CanvasClassRec.CanvasClass.GetCanvasStyle)) then
        begin
          Result := CanvasClassRec.CanvasClass;
          Break;
        end;
      end;
      if (Result = nil) and FEnableSoftwareCanvas then
      begin
        for CanvasClassRec in FCanvasList do
        begin
          if not (TCanvasStyle.NeedGPUSurface in CanvasClassRec.CanvasClass.GetCanvasStyle) then
          begin
            Result := CanvasClassRec.CanvasClass;
            Break;
          end;
        end;
      end;
      if Result = nil then
        Result := FCanvasList[0].CanvasClass;
      FDefaultCanvasClass := Result;
    end
    else
      raise ECanvasManagerException.Create('No TCanvas implementation found');
  end
  else
    Result := FDefaultCanvasClass;
end;

class function TCanvasManager.GetDefaultPrinterCanvas: TCanvasClass;
var
  CanvasSrv: IFMXCanvasService;
  CanvasClassRec: TCanvasClassRec;
begin
  if FDefaultPrinterCanvasClass = nil then
  begin
    Result := nil;
    if FCanvasList = nil then
      if TPlatformServices.Current.SupportsPlatformService(IFMXCanvasService, CanvasSrv) then
        CanvasSrv.RegisterCanvasClasses;
    if (FCanvasList <> nil) and (FCanvasList.Count > 0) then
    begin
      for CanvasClassRec in FCanvasList do
        if CanvasClassRec.PrinterCanvas then
        begin
          Result := CanvasClassRec.CanvasClass;
          Break;
        end;
      FDefaultPrinterCanvasClass := Result;
    end
    else
      raise ECanvasManagerException.Create('No TCanvas for printer implementation found');
  end
  else
    Result := FDefaultPrinterCanvasClass;
end;

class function TCanvasManager.GetMeasureCanvas: TCanvas;
begin
  if FMeasureBitmap = nil then
    FMeasureBitmap := TBitmap.Create(1, 1);
  Result := FMeasureBitmap.Canvas
end;

class procedure TCanvasManager.RecreateFromPrinter(const Canvas: TCanvas; const APrinter: TAbstractPrinter);
begin
  Canvas.UnInitialize;
  Canvas.CreateFromPrinter(APrinter);
end;

class procedure TCanvasManager.EnableSoftwareCanvas(const Enable: Boolean);
begin
  FEnableSoftwareCanvas := Enable;
  FDefaultCanvasClass := nil;
end;

class procedure TCanvasManager.RegisterCanvas(const CanvasClass: TCanvasClass; const ADefault: Boolean; const APrinterCanvas: Boolean);
var
  Rec: TCanvasClassRec;
begin
  if FCanvasList = nil then
    FCanvasList := TList<TCanvasClassRec>.Create;
  Rec.CanvasClass := CanvasClass;
  Rec.Default := ADefault;
  Rec.PrinterCanvas := APrinterCanvas;
  FCanvasList.Add(Rec);
end;

{ TCanvas.TMetaBrush }

destructor TCanvas.TMetaBrush.Destroy;
begin
  FGradient.Free;
  inherited;
end;

function TCanvas.TMetaBrush.GetGradient: TGradient;
begin
  if FGradient = nil then
  begin
    FGradient := TGradient.Create;
    FValid := False;
  end;

  Result := FGradient;
end;

{ TCanvas }

constructor TCanvas.CreateFromWindow(const AParent: TWindowHandle; const AWidth, AHeight: Integer;
  const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault);
begin
  inherited Create;
  FQuality := AQuality;
  FParent := AParent;
  FWidth := AWidth;
  FHeight := AHeight;
  Initialize;
end;

class procedure TCanvas.CopyBitmap(const Source, Dest: TBitmap);
var
  S, D: TBitmapData;
begin
  if (Source.Width = Dest.Width) and (Source.Height = Dest.Height) then
  begin
    if not Source.IsEmpty then
    begin
      if (Source.CanvasClass = Dest.CanvasClass) then
        Source.CanvasClass.DoCopyBitmap(Source, Dest)
      else begin
        if Source.Map(TMapAccess.Read, S) and Dest.Map(TMapAccess.Write, D) then
        try
          D.Copy(S);
        finally
          Source.Unmap(S);
          Dest.Unmap(D);
        end;
      end;
    end;
  end else
    raise ECanvasException.Create(SBitmapSizeNotEqual);
end;

class constructor TCanvas.Create;
begin
  FLock := TObject.Create;
end;

constructor TCanvas.CreateFromBitmap(const ABitmap: TBitmap; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault);
begin
  inherited Create;
  FQuality := AQuality;
  FBitmap := ABitmap;
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  Initialize;
end;

constructor TCanvas.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  inherited Create;
  Initialize;
  FPrinter := APrinter;
end;

procedure TCanvas.Initialize;
begin
  FScale := GetCanvasScale;
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, $FF000000);
  FFill := TBrush.Create(TBrushKind.Solid, $FFFFFFFF);
  FFont := TFont.Create;
  FFont.OnChanged := FontChanged;
  FCanvasSaveData := TCanvasSaveStateList.Create;
  FMatrixMeaning := TMatrixMeaning.Identity;
  FBlending := True;
end;

procedure TCanvas.UnInitialize;
begin
  FCanvasSaveData.DisposeOf;
  FFont.DisposeOf;
  FStroke.DisposeOf;
  FFill.DisposeOf;
end;

destructor TCanvas.Destroy;
begin
  if TThread.Current.ThreadID = MainThreadID then  // << https://quality.embarcadero.com/browse/RSP-19673
    TMessageManager.DefaultManager.SendMessage(Self, TCanvasDestroyMessage.Create); // TCanvasDestroyMessage seam to be used only in FMX.TextLayout
  UnInitialize;
  inherited;
end;

class destructor TCanvas.Destroy;
begin
  FLock.Free;
end;

function TCanvas.DoBeginScene(const AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
begin
  FClippingChangeCount := 0;
  FSavingStateCount := 0;

  Stroke.Thickness := 1;
  Stroke.Cap := TStrokeCap.Flat;
  Stroke.Join := TStrokeJoin.Miter;
  Stroke.Dash := TStrokeDash.Solid;
  Stroke.Kind := TBrushKind.Solid;
  Fill.Kind := TBrushKind.Solid;
  SetMatrix(TMatrix.Identity);
  Result := True;
end;

procedure TCanvas.DoBlendingChanged;
begin

end;

class procedure TCanvas.DoCopyBitmap(const Source, Dest: TBitmap);
var
  S, D: TBitmapData;
begin
  if Source.Map(TMapAccess.Read, S) and Dest.Map(TMapAccess.Write, D) then
  try
    D.Copy(S);
  finally
    Source.Unmap(S);
    Dest.Unmap(D);
  end;
end;

procedure TCanvas.DoEndScene;
begin
  if FBitmap <> nil then
    FBitmap.BitmapChanged;
end;

function TCanvas.AlignToPixel(const Value: TPointF): TPointF;
begin
  case FMatrixMeaning of
    TMatrixMeaning.Identity:
      Result := TPointF.Create(Round(Value.X * FScale + TEpsilon.Vector) / FScale, Round(Value.Y * FScale + TEpsilon.Vector) / FScale);
    TMatrixMeaning.Translate:
      Result := TPointF.Create(Round((Matrix.m31 + Value.X) * FScale + TEpsilon.Vector) / FScale - Matrix.m31,
        Round((Matrix.m32 + Value.Y) * FScale + TEpsilon.Vector) / FScale - Matrix.m32);
  else
    Result := Value;
  end;
end;

function TCanvas.AlignToPixel(const Rect: TRectF): TRectF;
begin
  Result.Left := AlignToPixelHorizontally(Rect.Left);
  Result.Top := AlignToPixelVertically(Rect.Top);
  Result.Right := Result.Left + Round(Rect.Width * Scale) / Scale; // keep ratio horizontally
  Result.Bottom := Result.Top + Round(Rect.Height * Scale) / Scale; // keep ratio vertically
end;

function TCanvas.AlignToPixelHorizontally(const Value: Single): Single;
begin
  case FMatrixMeaning of
    TMatrixMeaning.Identity: Result := Round(Value * FScale + TEpsilon.Vector) / FScale;
    TMatrixMeaning.Translate: Result := Round((Matrix.m31 + Value) * FScale + TEpsilon.Vector) / FScale - Matrix.m31;
  else
    Result := Value;
  end;
end;

function TCanvas.AlignToPixelVertically(const Value: Single): Single;
begin
  case FMatrixMeaning of
    TMatrixMeaning.Identity: Result := Round(Value * FScale + TEpsilon.Vector) / FScale;
    TMatrixMeaning.Translate: Result := Round((Matrix.m32 + Value) * FScale + TEpsilon.Vector) / FScale - Matrix.m32;
  else
    Result := Value;
  end;
end;

class procedure TCanvas.Lock;
begin
  TMonitor.Enter(FLock);
end;

class procedure TCanvas.Unlock;
begin
  TMonitor.Exit(FLock);
end;

function TCanvas.BeginScene(AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean;
begin
  Lock;
  try
    if FBeginSceneCount = 0 then
    begin
      Result := (Width > 0) and (Height > 0) and DoBeginScene(AClipRects, AContextHandle);
      if not Result then
      begin
        Unlock;
        Exit;
      end;
    end
    else
      Result := FBeginSceneCount > 0;
    Inc(FBeginSceneCount);
  except
    Unlock;
    raise;
  end;
end;

procedure TCanvas.EndScene;
begin
  try
    if FBeginSceneCount = 1 then
      DoEndScene;
    Dec(FBeginSceneCount);
  finally
    Unlock;
  end;
end;

procedure TCanvas.DoSetMatrix(const M: TMatrix);
begin
end;

procedure TCanvas.SetMatrix(const M: TMatrix);
begin
  FMatrixMeaning := TMatrixMeaning.Unknown;
  if not SameValue(FOffset.X, 0, TEpsilon.Matrix) or not SameValue(FOffset.Y, 0, TEpsilon.Matrix) then
    FMatrix := TMatrix.CreateTranslation(FOffset.X, FOffset.Y) * M
  else
    FMatrix := M;
  { Check for identity matrix values. It is assumed that the matrix is composed of
    three vectors of unit length, so by comparing some specific values with one,
    we discard any other possibility of other vectors. }
  if SameValue(FMatrix.m11, 1, TEpsilon.Matrix) and SameValue(FMatrix.m22, 1, TEpsilon.Matrix) and
    SameValue(FMatrix.m33, 1, TEpsilon.Matrix) then
  begin
    if SameValue(FMatrix.m31, 0, TEpsilon.Matrix) and SameValue(FMatrix.m32, 0, TEpsilon.Matrix) then
    begin // If no translation is present, we have an identity matrix.
      FMatrixMeaning := TMatrixMeaning.Identity;
    end
    else
    begin // Translation information is present in the matrix.
      FMatrixMeaning := TMatrixMeaning.Translate;

      FMatrixTranslate.X := FMatrix.m31;
      FMatrixTranslate.Y := FMatrix.m32;
    end;
  end;
  DoSetMatrix(FMatrix);
end;

procedure TCanvas.MultiplyMatrix(const M: TMatrix);
var
  MulMatrix: TMatrix;
begin
  MulMatrix := M * FMatrix;
  SetMatrix(MulMatrix);
end;

function TCanvas.CreateSaveState: TCanvasSaveState;
begin
  Result := TCanvasSaveState.Create;
end;

procedure TCanvas.RestoreState(const State: TCanvasSaveState);
begin
  if FCanvasSaveData.IndexOf(State) >= 0 then
    Assign(State);
end;

procedure TCanvas.FontChanged(Sender: TObject);
begin
end;

class function TCanvas.InitializeBitmap(const Width, Height: Integer; const Scale: Single; var PixelFormat: TPixelFormat): THandle;
begin
  if (Width > 0) and (Height > 0) then
    Result := DoInitializeBitmap(Width, Height, Scale, PixelFormat)
  else
    Result := 0;
end;

function TCanvas.IsScaleInteger: Boolean;
begin
  Result := SameValue(Frac(Scale), 0, TEpsilon.Scale);
end;

class procedure TCanvas.FinalizeBitmap(var Bitmap: THandle);
begin
  if Bitmap <> 0 then
    DoFinalizeBitmap(Bitmap);
end;

procedure TCanvas.Flush;
begin
  DoFlush;
end;

class function TCanvas.MapBitmap(const Bitmap: THandle; const Access: TMapAccess; var Data: TBitmapData): Boolean;
begin
  if Bitmap <> 0 then
    Result := DoMapBitmap(Bitmap, Access, Data)
  else
    Result := False;
end;

class procedure TCanvas.UnmapBitmap(const Bitmap: THandle; var Data: TBitmapData);
begin
  if Bitmap <> 0 then
    DoUnmapBitmap(Bitmap, Data);
end;

function TCanvas.LoadFontFromStream(const AStream: TStream): Boolean;
begin
  Result := False;
                                                       
end;

procedure TCanvas.MeasureLines(const ALines: TLineMetricInfo; const ARect: TRectF; const AText: string; const WordWrap: Boolean; const Flags: TFillTextFlags;
  const ATextAlign: TTextAlign; const AVTextAlign: TTextAlign = TTextAlign.Center);
var
  WStartChar, WSaveChar, WCurChar, WCutOffChar: Integer;
  LCurChar: Integer;
  TmpS: string;
  WWidth: Single;
  LEditRectWidth: Single;
  Tok, LText: string;

  function _IsSurrogate(Surrogate: WideChar): Boolean;
  begin
    Result := (Integer(Surrogate) >= $D800) and (Integer(Surrogate) <= $DFFF);
  end;

  procedure _SkipSeparators(var Pos: Integer; const S: string);
  const
    // #$0020   SPACE
    // #$0021 ! EXCLAMATION MARK
    // #$002C , COMMA
    // #$002D - HYPHEN-MINUS
    // #$002E . FULL STOP
    // #$003A : COLON
    // #$003B ; SEMICOLON
    // #$003F ? QUESTION MARK
    BasicSeparatos: string = #$0020#$0021#$002C#$002D#$002E#$003A#$003B#$003F;
    MaxBasicSeparators: WideChar = #$003F;
  var
    ch: WideChar;
  begin
    while Pos < S.Length do
    begin
      ch := S.Chars[Pos];
      if (ch > MaxBasicSeparators) or not BasicSeparatos.Contains(ch) then
        Break;
      if _IsSurrogate(ch) then
        Inc(Pos, 2)
      else
        Inc(Pos);
    end;
  end;

  function _WideGetToken(var Pos: Integer; const S: string): string;
  const
  //#$0020   SPACE
  //#$0021 ! EXCLAMATION MARK
  //#$002C , COMMA
  //#$002D - HYPHEN-MINUS
  //#$002E . FULL STOP
  //#$003A : COLON
  //#$003B ; SEMICOLON
  //#$003F ? QUESTION MARK
    BasicSeparatos: string = #$0020#$0021#$002C#$002D#$002E#$003A#$003B#$003F;
    MaxBasicSeparators: WideChar = #$003F;
  var
    ch: WideChar;
  begin
    Result := '';
    { skip first separators }
    _SkipSeparators(Pos, S);

    { get }
    while Pos < S.Length do
    begin
      ch := S.Chars[Pos];
      if (ch <= MaxBasicSeparators) and BasicSeparatos.Contains(ch) then
        Break;
      if _IsSurrogate(ch) then
      begin
        Result := Result + S.Substring(Pos, 2);
        Inc(Pos, 2)
      end
      else
      begin
        Result := Result + S.Chars[Pos];
        Inc(Pos);
      end;
    end;

    { skip separators }
    _SkipSeparators(Pos, S);
  end;

  function RoundToPowerOf2(I: Integer): Integer;
  begin
    I := I or (I shr 1);
    I := I or (I shr 2);
    I := I or (I shr 4);
    I := I or (I shr 8);
    I := I or (I shr 16);
    Result := I + 1;
  end;

  function CutOffPoint(TmpS: string; Width: Single): integer;
  var
    W : Single;
    Delta: Integer;
  begin
    Delta := RoundToPowerOf2(Tmps.Length) div 2;
    Result := Delta;

    while Delta > 0 do
    begin
      W := TextWidth(TmpS.Substring(0, Result));
      if W > Width then
        Result := Result - Delta;
      Delta := Delta div 2;
      Result := Result + Delta;
    end;
  end;

begin
  ALines.Count := 0;
  if AText = '' then
    Exit;

  ALines.Count := 1;
  LEditRectWidth := ARect.Width;

  // first check linecreaks
  LText := AText;
  TmpS := '';

  LCurChar := 0;

  ALines.Count := 1;
  ALines.Metrics[0].Index := 1;
  while LCurChar < LText.Length do
  begin
    if (LText.Chars[LCurChar] = #13) or (LText.Chars[LCurChar] = #10) then
    begin
      if (LText.Chars[LCurChar] = #13) and (LCurChar + 1 < LText.Length) then
        if LText.Chars[LCurChar + 1] = #10 then
          Inc(LCurChar);

      if WordWrap and (TextWidth(TmpS) > LEditRectWidth) then
      begin
        WCurChar := 0;
        WStartChar := 0;
        WSaveChar := 0;
        Tok := _WideGetToken(WCurChar, TmpS);
        while Tok <> '' do
        begin
          WWidth := TextWidth(TmpS.Substring(WStartChar, WCurChar - WStartChar));
          if WWidth > LEditRectWidth then
          begin
            if WSaveChar = WStartChar then
            begin
              WCutOffChar := CutOffPoint(TmpS.Substring(WStartChar, WCurChar - WStartChar), LEditRectWidth);
              ALines.Metrics[ALines.Count - 1].Len := WCutoffChar;
              WCurChar := WStartChar + WCutOffChar;
              WStartChar := WStartChar + WCutOffChar;
            end
            else
            begin
              ALines.Metrics[ALines.Count - 1].Len := WSaveChar - WStartChar;
              WStartChar := WSaveChar;
            end;
            ALines.Count := ALines.Count + 1;
            ALines.Metrics[ALines.Count - 1].Index :=
              ALines.Metrics[ALines.Count - 2].Index + ALines.Metrics[ALines.Count - 2].Len;
          end;
          WSaveChar := WCurChar;
          Tok := _WideGetToken(WCurChar, TmpS);
          if WSaveChar = WCurChar then
            Break; { !!! - error }
        end;

        ALines.Metrics[ALines.Count - 1].Len := WCurChar - WStartChar;
      end
      else
        ALines.Metrics[ALines.Count - 1].Len := Length(Tmps);

      ALines.Count := ALines.Count + 1;
      ALines.Metrics[ALines.Count - 1].Index := LCurChar + 2;

      TmpS := '';
    end
    else
      TmpS := TmpS + LText.Chars[LCurChar];
    Inc(LCurChar);
  end;

// last line
  if WordWrap and (TextWidth(TmpS) > LEditRectWidth) then
  begin
    WCurChar := 0;
    WStartChar := 0;
    WSaveChar := 0;
    Tok := _WideGetToken(WCurChar, TmpS);
    while Tok <> '' do
    begin
      Tok := TmpS.Substring(WStartChar, WCurChar - WStartChar);
      WWidth := TextWidth(TmpS.Substring(WStartChar, WCurChar - WStartChar));
      if WWidth > LEditRectWidth then
      begin
        if WSaveChar = WStartChar then
        begin
          WCutOffChar := CutOffPoint(TmpS.Substring(WStartChar, WCurChar - WStartChar), LEditRectWidth);
          ALines.Metrics[ALines.Count - 1].Len := WCutoffChar;
          WCurChar := WStartChar + WCutOffChar;
          WStartChar := WStartChar + WCutOffChar;
        end
        else
        begin
          ALines.Metrics[ALines.Count - 1].Len := WSaveChar - WStartChar;
          WStartChar := WSaveChar;
        end;
        ALines.Count := ALines.Count + 1;
        ALines.Metrics[ALines.Count - 1].Index :=
          ALines.Metrics[ALines.Count - 2].Index + ALines.Metrics[ALines.Count - 2].Len;
      end;

      WSaveChar := WCurChar;
      Tok := _WideGetToken(WCurChar, TmpS);
      if WSaveChar = WCurChar then
        Break; { !!! - error }
    end;
    ALines.Metrics[ALines.Count - 1].Len := WCurChar - WStartChar;
  end
  else
    ALines.Metrics[ALines.Count - 1].Len := Length(Tmps);
end;

procedure TCanvas.MeasureText(var ARect: TRectF; const AText: string;
  const WordWrap: Boolean; const Flags: TFillTextFlags; const ATextAlign,
  AVTextAlign: TTextAlign);
var
  Layout: TTextLayout;
begin
  if AText.IsEmpty then
  begin
    ARect.Right := ARect.Left;
    ARect.Bottom := ARect.Top;
    Exit;
  end;

  Layout := TTextLayoutManager.TextLayoutByCanvas(Self.ClassType).Create(Self);
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.Text := AText;
    Layout.WordWrap := WordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Self.Font;
    Layout.Color := Self.Fill.Color;
    Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
    Layout.EndUpdate;
    ARect := Layout.TextRect;
  finally
    FreeAndNil(Layout);
  end;
end;

function TCanvas.TextHeight(const AText: string): Single;
var
  R: TRectF;
begin
  R := RectF(0, 0, 10000, 10000);
  MeasureText(R, AText, False, [], TTextAlign.Leading, TTextAlign.Leading);
  Result := R.Bottom;
end;

function TCanvas.TextToPath(Path: TPathData; const ARect: TRectF;
  const AText: string; const WordWrap: Boolean; const ATextAlign,
  AVTextAlign: TTextAlign): Boolean;
var
  Layout: TTextLayout;
begin
  if AText.IsEmpty then
    Exit(False);

  Layout := TTextLayoutManager.TextLayoutByCanvas(Self.ClassType).Create;
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.Text := AText;
    Layout.WordWrap := WordWrap;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Self.Font;
    Layout.Color := Self.Fill.Color;
    Layout.EndUpdate;
    Layout.ConvertToPath(Path);
    Result := True;
  finally
    FreeAndNil(Layout);
  end;
end;

function TCanvas.TextWidth(const AText: string): Single;
var
  R: TRectF;
begin
  R := RectF(0, 0, 10000, 20);
  MeasureText(R, AText, False, [], TTextAlign.Leading, TTextAlign.Center);
  Result := R.Right;
end;

function TCanvas.TransformPoint(const P: TPointF): TPointF;
begin
  case FMatrixMeaning of
    TMatrixMeaning.Unknown:
      Result := P * FMatrix;
    TMatrixMeaning.Identity:
      Result := P;
    TMatrixMeaning.Translate:
      begin
        Result.X := P.X + FMatrixTranslate.X;
        Result.Y := P.Y + FMatrixTranslate.Y;
      end;
  end;
end;

function TCanvas.TransformRect(const Rect: TRectF): TRectF;
var
  V: TPointF;
begin
  case FMatrixMeaning of
    TMatrixMeaning.Unknown:
      begin
        Result.TopLeft := Rect.TopLeft * FMatrix;

        V := TPointF.Create(Rect.Right, Rect.Top) * FMatrix;
        Result.Right := V.X;
        Result.Bottom := V.Y;

        V := Rect.BottomRight * FMatrix;
        Result.Right := V.X;
        Result.Bottom := V.Y;

        V := TPointF.Create(Rect.Left, Rect.Bottom) * FMatrix;
        Result.Left := V.X;
        Result.Bottom := V.Y;
      end;
    TMatrixMeaning.Identity:
      Result := Rect;
    TMatrixMeaning.Translate:
      begin
        Result := Rect;
        Result.Offset(FMatrixTranslate);
      end;
  end;
end;

procedure TCanvas.FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single);
begin
  FillArc(Center, Radius, StartAngle, SweepAngle, AOpacity, FFill);
end;

procedure TCanvas.FillArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single; const ABrush: TBrush);
var
  P: TPathData;
begin
  P := TPathData.Create;
  try
    P.AddArc(Center, Radius, StartAngle, SweepAngle);
    FillPath(P, AOpacity, ABrush);
  finally
    P.Free;
  end;
end;

procedure TCanvas.DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single);
begin
  DrawArc(Center, Radius, StartAngle, SweepAngle, AOpacity, FStroke);
end;

procedure TCanvas.DrawArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single; const AOpacity: Single; const ABrush: TStrokeBrush);
var
  P: TPathData;
begin
  P := TPathData.Create;
  try
    P.AddArc(Center, Radius, StartAngle, SweepAngle);
    DrawPath(P, AOpacity, ABrush);
  finally
    P.Free;
  end;
end;

procedure TCanvas.DrawBitmap(const ABitmap: TBitmap; const SrcRect, DstRect: TRectF; const AOpacity: Single; const HighSpeed: Boolean);
begin
  DoDrawBitmap(ABitmap, SrcRect, DstRect, AOpacity, HighSpeed);
end;

procedure TCanvas.DrawEllipse(const ARect: TRectF; const AOpacity: Single);
begin
  DrawEllipse(ARect, AOpacity, FStroke);
end;

procedure TCanvas.DrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if ABrush.Kind <> TBrushKind.None then
    DoDrawEllipse(ARect, AOpacity, ABrush);
end;

procedure TCanvas.DrawLine(const APt1, APt2: TPointF; const AOpacity: Single);
begin
  DrawLine(APt1, APt2, AOpacity, FStroke);
end;

procedure TCanvas.DrawLine(const APt1, APt2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if ABrush.Kind <> TBrushKind.None then
    DoDrawLine(APt1, APt2, AOpacity, ABrush);
end;

function TCanvas.SaveState: TCanvasSaveState;
var
  SaveData : TCanvasSaveState;
begin
  FSavingStateCount := FSavingStateCount + 1;
  for SaveData in FCanvasSaveData do
  begin
    if not SaveData.Assigned then
    begin
      SaveData.Assign(Self);
      Result := SaveData;
      Exit;
    end;
  end;
  Result := CreateSaveState;
  try
    Result.Assign(Self);
    FCanvasSaveData.Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TCanvas.SetBlending(const Value: Boolean);
begin
  if FBlending <> Value then
  begin
    FBlending := Value;
    DoBlendingChanged;
  end;
end;

procedure TCanvas.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TCanvas.FillPath(const APath: TPathData; const AOpacity: Single);
begin
  FillPath(APath, AOpacity, FFill);
end;

procedure TCanvas.FillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush);
begin
  if (ABrush.Kind <> TBrushKind.None) and not APath.IsEmpty then
    DoFillPath(APath, AOpacity, ABrush);
end;

function TCanvas.DoFillPolygon(const Points: TPolygon; const AOpacity: Single; const ABrush: TBrush): Boolean;
begin
  Result := False;
end;

procedure TCanvas.FillPolygon(const Points: TPolygon; const AOpacity: Single);
var
  I: Integer;
  Path: TPathData;
  PathBreakFound: Boolean;
begin
  if not DoFillPolygon(Points, AOpacity, FFill) then
  begin
    Path := TPathData.Create;
    try
      PathBreakFound := False;
      for I := 0 to High(Points) do
      begin
        if I = 0 then
          Path.MoveTo(Points[I])
        else
        if (Points[I].X = PolygonPointBreak.X) and (Points[I].Y = PolygonPointBreak.Y) then
        begin
          Path.ClosePath;
          PathBreakFound := True;
        end
        else
          Path.LineTo(Points[I]);
      end;
      if not PathBreakFound then
        Path.ClosePath;
      FillPath(Path, AOpacity);
    finally
      Path.Free;
    end;
  end;
end;

procedure TCanvas.DoFillRoundRect(const ARect: TRectF; const XRadius,
  YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ABrush: TBrush; const ACornerType: TCornerType = TCornerType.Round);
var
  Path: TPathData;
  x1, x2, y1, y2: Single;
  R: TRectF;
begin
  R := ARect;
  x1 := XRadius;
  if RectWidth(R) - (x1 * 2) < 0 then
    x1 := RectWidth(R) / 2;
  x2 := XRadius * CurveKappaInv;
  y1 := YRadius;
  if RectHeight(R) - (y1 * 2) < 0 then
    y1 := RectHeight(R) / 2;
  y2 := YRadius * CurveKappaInv;
  Path := TPathData.Create;
  Path.MoveTo(PointF(R.Left, R.Top + y1));
  if TCorner.TopLeft in ACorners then
  begin
    case ACornerType of
      // TCornerType.Round - default
      TCornerType.Bevel: Path.LineTo(PointF(R.Left + x1, R.Top));
      TCornerType.InnerRound: Path.CurveTo(PointF(R.Left + x2, R.Top + y1), PointF(R.Left + x1, R.Top + y2), PointF(R.Left + x1, R.Top));
      TCornerType.InnerLine:
        begin
          Path.LineTo(PointF(R.Left + x2, R.Top + y1));
          Path.LineTo(PointF(R.Left + x1, R.Top + y2));
          Path.LineTo(PointF(R.Left + x1, R.Top));
        end;
    else
      Path.CurveTo(PointF(R.Left, R.Top + (y2)), PointF(R.Left + x2, R.Top), PointF(R.Left + x1, R.Top))
    end;
  end
  else
  begin
    Path.LineTo(PointF(R.Left, R.Top));
    Path.LineTo(PointF(R.Left + x1, R.Top));
  end;
  Path.LineTo(PointF(R.Right - x1, R.Top));
  if TCorner.TopRight in ACorners then
  begin
    case ACornerType of
      // TCornerType.Round - default
      TCornerType.Bevel: Path.LineTo(PointF(R.Right, R.Top + y1));
      TCornerType.InnerRound: Path.CurveTo(PointF(R.Right - x1, R.Top + y2), PointF(R.Right - x2, R.Top + y1), PointF(R.Right, R.Top + y1));
      TCornerType.InnerLine:
        begin
          Path.LineTo(PointF(R.Right - x1, R.Top + y2));
          Path.LineTo(PointF(R.Right - x2, R.Top + y1));
          Path.LineTo(PointF(R.Right, R.Top + y1));
        end;
    else
      Path.CurveTo(PointF(R.Right - x2, R.Top), PointF(R.Right, R.Top + (y2)), PointF(R.Right, R.Top + y1))
    end;
  end
  else
  begin
    Path.LineTo(PointF(R.Right, R.Top));
    Path.LineTo(PointF(R.Right, R.Top + y1));
  end;
  Path.LineTo(PointF(R.Right, R.Bottom - y1));
  if TCorner.BottomRight in ACorners then
  begin
    case ACornerType of
      // TCornerType.Round - default
      TCornerType.Bevel: Path.LineTo(PointF(R.Right - x1, R.Bottom));
      TCornerType.InnerRound: Path.CurveTo(PointF(R.Right - x2, R.Bottom - y1), PointF(R.Right - x1, R.Bottom - y2), PointF(R.Right - x1, R.Bottom));
      TCornerType.InnerLine:
        begin
          Path.LineTo(PointF(R.Right - x2, R.Bottom - y1));
          Path.LineTo(PointF(R.Right - x1, R.Bottom - y2));
          Path.LineTo(PointF(R.Right - x1, R.Bottom));
        end;
    else
      Path.CurveTo(PointF(R.Right, R.Bottom - (y2)), PointF(R.Right - x2, R.Bottom), PointF(R.Right - x1, R.Bottom))
    end;
  end
  else
  begin
    Path.LineTo(PointF(R.Right, R.Bottom));
    Path.LineTo(PointF(R.Right - x1, R.Bottom));
  end;
  Path.LineTo(PointF(R.Left + x1, R.Bottom));
  if TCorner.BottomLeft in ACorners then
  begin
    case ACornerType of
      // TCornerType.Round - default
      TCornerType.Bevel: Path.LineTo(PointF(R.Left, R.Bottom - y1));
      TCornerType.InnerRound: Path.CurveTo(PointF(R.Left + x1, R.Bottom - y2), PointF(R.Left + x2, R.Bottom - y1), PointF(R.Left, R.Bottom - y1));
      TCornerType.InnerLine:
        begin
          Path.LineTo(PointF(R.Left + x1, R.Bottom - y2));
          Path.LineTo(PointF(R.Left + x2, R.Bottom - y1));
          Path.LineTo(PointF(R.Left, R.Bottom - y1));
        end;
    else
      Path.CurveTo(PointF(R.Left + x2, R.Bottom), PointF(R.Left, R.Bottom - (y2)), PointF(R.Left, R.Bottom - y1))
    end;
  end
  else
  begin
    Path.LineTo(PointF(R.Left, R.Bottom));
    Path.LineTo(PointF(R.Left, R.Bottom - y1));
  end;
  Path.ClosePath;
  DoFillPath(Path, AOpacity, ABrush);
  Path.Free;
end;

procedure TCanvas.DoFlush;
begin

end;

procedure TCanvas.FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ACornerType: TCornerType);
begin
  FillRect(ARect, XRadius, YRadius, ACorners, AOpacity, FFill, ACornerType);
end;

procedure TCanvas.FillRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ABrush: TBrush; const ACornerType: TCornerType);
begin
  if ABrush.Kind <> TBrushKind.None then
  begin
    if ((XRadius = 0) and (YRadius = 0)) or (ACorners = []) then
      DoFillRect(ARect, AOpacity, ABrush)
    else
      DoFillRoundRect(ARect, XRadius, YRadius, ACorners, AOpacity, ABrush, ACornerType)
  end;
end;

procedure TCanvas.FillEllipse(const ARect: TRectF; const AOpacity: Single);
begin
  FillEllipse(ARect, AOpacity, FFill);
end;

procedure TCanvas.FillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush);
begin
  if ABrush.Kind <> TBrushKind.None then
    DoFillEllipse(ARect, AOpacity, ABrush);
end;

procedure TCanvas.FillText(const ARect: TRectF; const AText: string;
  const WordWrap: Boolean; const AOpacity: Single; const Flags: TFillTextFlags;
  const ATextAlign, AVTextAlign: TTextAlign);
var
  Layout: TTextLayout;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(Self.ClassType).Create(Self);
  try
    Layout.BeginUpdate;
    Layout.TopLeft := ARect.TopLeft;
    Layout.MaxSize := PointF(ARect.Width, ARect.Height);
    Layout.Text := AText;
    Layout.WordWrap := WordWrap;
    Layout.Opacity := AOpacity;
    Layout.HorizontalAlign := ATextAlign;
    Layout.VerticalAlign := AVTextAlign;
    Layout.Font := Self.Font;
    Layout.Color := Self.Fill.Color;
    Layout.RightToLeft := TFillTextFlag.RightToLeft in Flags;
    Layout.EndUpdate;
    Layout.RenderLayout(Self);
  finally
    FreeAndNil(Layout);
  end;
end;

procedure TCanvas.DrawPath(const APath: TPathData; const AOpacity: Single);
begin
  DrawPath(APath, AOpacity, FStroke);
end;

procedure TCanvas.DrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  if (ABrush.Kind <> TBrushKind.None) and not APath.IsEmpty then
    DoDrawPath(APath, AOpacity, ABrush);
end;

function TCanvas.DoDrawPolygon(const Points: TPolygon; const AOpacity: Single; const ABrush: TStrokeBrush): Boolean;
begin
  Result := False;
end;

procedure TCanvas.DrawPolygon(const Points: TPolygon; const AOpacity: Single);
var
  I: Integer;
  Path: TPathData;
  PathBreakFound: Boolean;
begin
  if not DoDrawPolygon(Points, AOpacity, FStroke) then
  begin
    Path := TPathData.Create;
    try
      PathBreakFound := False;
      for I := 0 to High(Points) do
      begin
        if I = 0 then
          Path.MoveTo(Points[I])
        else
        if (Points[I].X = PolygonPointBreak.X) and (Points[I].Y = PolygonPointBreak.Y) then
        begin
          Path.ClosePath;
          PathBreakFound := True;
        end
        else
          Path.LineTo(Points[I]);
      end;
      if not PathBreakFound then
        Path.ClosePath;
      DrawPath(Path, AOpacity);
    finally
      Path.Free;
    end;
  end;
end;

procedure TCanvas.DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ACornerType: TCornerType);
begin
  DrawRect(ARect, XRadius, YRadius, ACorners, AOpacity, FStroke, ACornerType);
end;

procedure TCanvas.DrawRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners; const AOpacity: Single;
  const ABrush: TStrokeBrush; const ACornerType: TCornerType);
var
  Path: TPathData;
  x1, x2, y1, y2: Single;
  R: TRectF;
begin
  if ABrush.Kind <> TBrushKind.None then
  begin
    R := ARect;
    if ((XRadius = 0) and (YRadius = 0)) or (ACorners = []) then
      DoDrawRect(ARect, AOpacity, ABrush)
    else begin
      R := ARect;
      x1 := XRadius;
      if RectWidth(R) - (x1 * 2) < 0 then
        x1 := RectWidth(R) / 2;
      x2 := XRadius * CurveKappaInv;
      y1 := YRadius;
      if RectHeight(R) - (y1 * 2) < 0 then
        y1 := RectHeight(R) / 2;
      y2 := YRadius * CurveKappaInv;
      Path := TPathData.Create;
      Path.MoveTo(PointF(R.Left, R.Top + y1));
      if TCorner.TopLeft in ACorners then
      begin
        case ACornerType of
          // TCornerType.Round - default
          TCornerType.Bevel: Path.LineTo(PointF(R.Left + x1, R.Top));
          TCornerType.InnerRound: Path.CurveTo(PointF(R.Left + x2, R.Top + y1), PointF(R.Left + x1, R.Top + y2), PointF(R.Left + x1, R.Top));
          TCornerType.InnerLine:
            begin
              Path.LineTo(PointF(R.Left + x2, R.Top + y1));
              Path.LineTo(PointF(R.Left + x1, R.Top + y2));
              Path.LineTo(PointF(R.Left + x1, R.Top));
            end;
        else
          Path.CurveTo(PointF(R.Left, R.Top + (y2)), PointF(R.Left + x2, R.Top), PointF(R.Left + x1, R.Top))
        end;
      end
      else
      begin
        Path.LineTo(PointF(R.Left, R.Top));
        Path.LineTo(PointF(R.Left + x1, R.Top));
      end;
      Path.LineTo(PointF(R.Right - x1, R.Top));
      if TCorner.TopRight in ACorners then
      begin
        case ACornerType of
          // TCornerType.Round - default
          TCornerType.Bevel: Path.LineTo(PointF(R.Right, R.Top + y1));
          TCornerType.InnerRound: Path.CurveTo(PointF(R.Right - x1, R.Top + y2), PointF(R.Right - x2, R.Top + y1), PointF(R.Right, R.Top + y1));
          TCornerType.InnerLine:
            begin
              Path.LineTo(PointF(R.Right - x1, R.Top + y2));
              Path.LineTo(PointF(R.Right - x2, R.Top + y1));
              Path.LineTo(PointF(R.Right, R.Top + y1));
            end;
        else
          Path.CurveTo(PointF(R.Right - x2, R.Top), PointF(R.Right, R.Top + (y2)), PointF(R.Right, R.Top + y1))
        end;
      end
      else
      begin
        Path.LineTo(PointF(R.Right, R.Top));
        Path.LineTo(PointF(R.Right, R.Top + y1));
      end;
      Path.LineTo(PointF(R.Right, R.Bottom - y1));
      if TCorner.BottomRight in ACorners then
      begin
        case ACornerType of
          // TCornerType.Round - default
          TCornerType.Bevel: Path.LineTo(PointF(R.Right - x1, R.Bottom));
          TCornerType.InnerRound: Path.CurveTo(PointF(R.Right - x2, R.Bottom - y1), PointF(R.Right - x1, R.Bottom - y2), PointF(R.Right - x1, R.Bottom));
          TCornerType.InnerLine:
            begin
              Path.LineTo(PointF(R.Right - x2, R.Bottom - y1));
              Path.LineTo(PointF(R.Right - x1, R.Bottom - y2));
              Path.LineTo(PointF(R.Right - x1, R.Bottom));
            end;
        else
          Path.CurveTo(PointF(R.Right, R.Bottom - (y2)), PointF(R.Right - x2, R.Bottom), PointF(R.Right - x1, R.Bottom))
        end;
      end
      else
      begin
        Path.LineTo(PointF(R.Right, R.Bottom));
        Path.LineTo(PointF(R.Right - x1, R.Bottom));
      end;
      Path.LineTo(PointF(R.Left + x1, R.Bottom));
      if TCorner.BottomLeft in ACorners then
      begin
        case ACornerType of
          // TCornerType.Round - default
          TCornerType.Bevel: Path.LineTo(PointF(R.Left, R.Bottom - y1));
          TCornerType.InnerRound: Path.CurveTo(PointF(R.Left + x1, R.Bottom - y2), PointF(R.Left + x2, R.Bottom - y1), PointF(R.Left, R.Bottom - y1));
          TCornerType.InnerLine:
            begin
              Path.LineTo(PointF(R.Left + x1, R.Bottom - y2));
              Path.LineTo(PointF(R.Left + x2, R.Bottom - y1));
              Path.LineTo(PointF(R.Left, R.Bottom - y1));
            end;
        else
          Path.CurveTo(PointF(R.Left + x2, R.Bottom), PointF(R.Left, R.Bottom - (y2)), PointF(R.Left, R.Bottom - y1))
        end;
      end
      else
      begin
        Path.LineTo(PointF(R.Left, R.Bottom));
        Path.LineTo(PointF(R.Left, R.Bottom - y1));
      end;
      Path.ClosePath;
      DoDrawPath(Path, AOpacity, ABrush);
      Path.Free;
    end;
  end;
end;

procedure TCanvas.DrawDashRect(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const AOpacity: Single; const AColor: TAlphaColor);
var
  Brush: TStrokeBrush;
begin
  Brush := TStrokeBrush.Create(TBrushKind.Solid, AColor);
  Brush.Thickness := 1;
  Brush.Dash := TStrokeDash.Dash;
  Brush.Kind := TBrushKind.Solid;
  DrawRect(ARect, XRadius, YRadius, ACorners, AOpacity, Brush);
  Brush.Free;
end;

procedure TCanvas.DrawRectSides(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ASides: TSides; const ACornerType: TCornerType = TCornerType.Round);
begin
  DrawRectSides(ARect, XRadius, YRadius, ACorners, AOpacity, ASides, FStroke, ACornerType);
end;

procedure TCanvas.DrawRectSides(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const AOpacity: Single; const ASides: TSides; const ABrush: TStrokeBrush; const ACornerType: TCornerType = TCornerType.Round);
var
  Path: TPathData;
  X1, X2, Y1, Y2: Single;
  DrawingRect: TRectF;
begin
  DrawingRect := ARect;
  X1 := XRadius;
  if DrawingRect.Width - (X1 * 2) < 0 then
    if X1 <> 0 then // guard divide by zero
      X1 := (XRadius * (DrawingRect.Width / (X1 * 2)));
  X2 := X1 / 2;
  Y1 := YRadius;
  if DrawingRect.Height - (Y1 * 2) < 0 then
    if Y1 <> 0 then // guard divide by zero
      Y1 := (YRadius * (DrawingRect.Height / (Y1 * 2)));
  Y2 := Y1 / 2;
  Path := TPathData.Create;
  try
    Path.MoveTo(PointF(DrawingRect.Left, DrawingRect.Top + Y1));
    if TCorner.TopLeft in ACorners then
    begin
      if (TSide.Top in ASides) or (TSide.Left in ASides) or (XRadius > 0) or (YRadius > 0) then
      begin
        case ACornerType of
          // TCornerType.Round - default
          TCornerType.Bevel:
            Path.LineTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Top));
          TCornerType.InnerRound:
            Path.CurveTo(TPointF.Create(DrawingRect.Left + X2, DrawingRect.Top + Y1),
              TPointF.Create(DrawingRect.Left + X1, DrawingRect.Top + Y2),
              TPointF.Create(DrawingRect.Left + X1, DrawingRect.Top));
          TCornerType.InnerLine:
            begin
              Path.LineTo(TPointF.Create(DrawingRect.Left + X2, DrawingRect.Top + Y1));
              Path.LineTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Top + Y2));
              Path.LineTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Top));
            end;
        else
          Path.CurveTo(TPointF.Create(DrawingRect.Left, DrawingRect.Top + (Y2)),
            TPointF.Create(DrawingRect.Left + X2, DrawingRect.Top), TPointF.Create(DrawingRect.Left + X1,
            DrawingRect.Top))
        end;
      end
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Top));
    end
    else
    begin
      if TSide.Left in ASides then
        Path.LineTo(TPointF.Create(DrawingRect.Left, DrawingRect.Top))
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Left, DrawingRect.Top));
      if TSide.Top in ASides then
        Path.LineTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Top))
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Top));
    end;
    if not(TSide.Top in ASides) then
      Path.MoveTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Top))
    else
      Path.LineTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Top));
    if TCorner.TopRight in ACorners then
    begin
      if (TSide.Top in ASides) or (TSide.Right in ASides) or (XRadius > 0) or (YRadius > 0) then
      begin
        case ACornerType of
          // TCornerType.Round - default
          TCornerType.Bevel:
            Path.LineTo(TPointF.Create(DrawingRect.Right, DrawingRect.Top + Y1));
          TCornerType.InnerRound:
            Path.CurveTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Top + Y2),
              TPointF.Create(DrawingRect.Right - X2, DrawingRect.Top + Y1),
              TPointF.Create(DrawingRect.Right, DrawingRect.Top + Y1));
          TCornerType.InnerLine:
            begin
              Path.LineTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Top + Y2));
              Path.LineTo(TPointF.Create(DrawingRect.Right - X2, DrawingRect.Top + Y1));
              Path.LineTo(TPointF.Create(DrawingRect.Right, DrawingRect.Top + Y1));
            end;
        else
          Path.CurveTo(TPointF.Create(DrawingRect.Right - X2, DrawingRect.Top),
            TPointF.Create(DrawingRect.Right, DrawingRect.Top + (Y2)),
            TPointF.Create(DrawingRect.Right, DrawingRect.Top + Y1))
        end;
      end
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Right, DrawingRect.Top + Y1));
    end
    else
    begin
      if TSide.Top in ASides then
        Path.LineTo(TPointF.Create(DrawingRect.Right, DrawingRect.Top))
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Right, DrawingRect.Top));
      if TSide.Right in ASides then
        Path.LineTo(TPointF.Create(DrawingRect.Right, DrawingRect.Top + Y1))
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Right, DrawingRect.Top + Y1));
    end;
    if not(TSide.Right in ASides) then
      Path.MoveTo(TPointF.Create(DrawingRect.Right, DrawingRect.Bottom - Y1))
    else
      Path.LineTo(TPointF.Create(DrawingRect.Right, DrawingRect.Bottom - Y1));
    if TCorner.BottomRight in ACorners then
    begin
      if (TSide.Bottom in ASides) or (TSide.Right in ASides) or (XRadius > 0) or (YRadius > 0) then
      begin
        case ACornerType of
          // TCornerType.Round - default
          TCornerType.Bevel:
            Path.LineTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Bottom));
          TCornerType.InnerRound:
            Path.CurveTo(TPointF.Create(DrawingRect.Right - X2, DrawingRect.Bottom - Y1),
              TPointF.Create(DrawingRect.Right - X1, DrawingRect.Bottom - Y2),
              TPointF.Create(DrawingRect.Right - X1, DrawingRect.Bottom));
          TCornerType.InnerLine:
            begin
              Path.LineTo(TPointF.Create(DrawingRect.Right - X2, DrawingRect.Bottom - Y1));
              Path.LineTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Bottom - Y2));
              Path.LineTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Bottom));
            end;
        else
          Path.CurveTo(TPointF.Create(DrawingRect.Right, DrawingRect.Bottom - (Y2)),
            TPointF.Create(DrawingRect.Right - X2, DrawingRect.Bottom), TPointF.Create(DrawingRect.Right - X1,
            DrawingRect.Bottom))
        end;
      end
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Bottom));
    end
    else
    begin
      if TSide.Right in ASides then
        Path.LineTo(TPointF.Create(DrawingRect.Right, DrawingRect.Bottom))
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Right, DrawingRect.Bottom));
      if TSide.Bottom in ASides then
        Path.LineTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Bottom))
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Right - X1, DrawingRect.Bottom));
    end;
    if not(TSide.Bottom in ASides) then
      Path.MoveTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Bottom))
    else
      Path.LineTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Bottom));
    if TCorner.BottomLeft in ACorners then
    begin
      if (TSide.Bottom in ASides) or (TSide.Left in ASides) or (XRadius > 0) or (YRadius > 0) then
      begin
        case ACornerType of
          // TCornerType.Round - default
          TCornerType.Bevel:
            Path.LineTo(TPointF.Create(DrawingRect.Left, DrawingRect.Bottom - Y1));
          TCornerType.InnerRound:
            Path.CurveTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Bottom - Y2),
              TPointF.Create(DrawingRect.Left + X2, DrawingRect.Bottom - Y1),
              TPointF.Create(DrawingRect.Left, DrawingRect.Bottom - Y1));
          TCornerType.InnerLine:
            begin
              Path.LineTo(TPointF.Create(DrawingRect.Left + X1, DrawingRect.Bottom - Y2));
              Path.LineTo(TPointF.Create(DrawingRect.Left + X2, DrawingRect.Bottom - Y1));
              Path.LineTo(TPointF.Create(DrawingRect.Left, DrawingRect.Bottom - Y1));
            end;
        else
          Path.CurveTo(TPointF.Create(DrawingRect.Left + X2, DrawingRect.Bottom),
            TPointF.Create(DrawingRect.Left, DrawingRect.Bottom - (Y2)),
            TPointF.Create(DrawingRect.Left, DrawingRect.Bottom - Y1))
        end;
      end
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Left, DrawingRect.Bottom - Y1));
    end
    else
    begin
      if TSide.Bottom in ASides then
        Path.LineTo(TPointF.Create(DrawingRect.Left, DrawingRect.Bottom))
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Left, DrawingRect.Bottom));
      if TSide.Left in ASides then
        Path.LineTo(TPointF.Create(DrawingRect.Left, DrawingRect.Bottom - Y1))
      else
        Path.MoveTo(TPointF.Create(DrawingRect.Left, DrawingRect.Bottom - Y1));
    end;
    if (TSide.Left in ASides) then
    begin
      Path.LineTo(TPointF.Create(DrawingRect.Left, DrawingRect.Top + Y1));
    end;
    DrawPath(Path, AOpacity, ABrush);
  finally
    Path.Free;
  end;
end;

function TCanvas.GetCanvasScale: Single;
begin
  if Parent <> nil then
    Result := Parent.Scale
  else if Bitmap <> nil then
    Result := Bitmap.BitmapScale
  else
    Result := DefaultScale;
end;

class function TCanvas.GetCanvasStyle: TCanvasStyles;
begin
  Result := [TCanvasStyle.SupportClipRects];
end;

class function TCanvas.GetAttribute(const Value: TCanvasAttribute): Integer;
begin
  case Value of
    TCanvasAttribute.MaxBitmapSize:
      Result := MaxAllowedBitmapSize;
  else
    raise EArgumentException.CreateRes(@SInvalidCanvasParameter);
  end;
end;

procedure TCanvas.SetSize(const AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

{ TBrushObject }

constructor TBrushObject.Create(AOwner: TComponent);
begin
  inherited;
  FBrush := TBrush.Create(TBrushKind.Solid, $FFFFFFFF);
end;

destructor TBrushObject.Destroy;
begin
  FreeAndNil(FBrush);
  inherited;
end;

function TBrushObject.GetBrush: TBrush;
begin
  Result := FBrush;
end;

procedure TBrushObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

{ TFontObject }

constructor TFontObject.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
end;

destructor TFontObject.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

function TFontObject.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TFontObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

{ TPathObject }

constructor TPathObject.Create(AOwner: TComponent);
begin
  inherited;
  FPath := TPathData.Create();
end;

destructor TPathObject.Destroy;
begin
  FreeAndNil(FPath);
  inherited;
end;

function TPathObject.GetPath: TPathData;
begin
  Result := FPath;
end;

procedure TPathObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

{ TBitmapObject }

constructor TBitmapObject.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap.Create(1, 1);
end;

destructor TBitmapObject.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TBitmapObject.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

procedure TBitmapObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FStyleName = '' then
    FStyleName := Name;
end;

{ TCanvasSaveState }

procedure TCanvasSaveState.Assign(Source: TPersistent);
var
  LCanvas: TCanvas;
begin
  if Source is TCanvas then
  begin
    LCanvas := TCanvas(Source);
    Self.FAssigned := True;
    Self.FOffset := LCanvas.FOffset;
    Self.FMatrix := LCanvas.FMatrix;
    Self.FFill.Assign(LCanvas.Fill);
    Self.FStroke.Assign(LCanvas.Stroke);
    Self.FFont.Assign(LCanvas.Font);
  end else
    inherited;
end;

procedure TCanvasSaveState.AssignTo(Dest: TPersistent);
var
  LCanvas: TCanvas;
begin
  if Dest is TCanvas then
  begin
    LCanvas := TCanvas(Dest);
    Self.FAssigned := False;
    LCanvas.Offset := FOffset;
    LCanvas.SetMatrix(Self.FMatrix);
    LCanvas.Fill.Assign(Self.FFill);
    LCanvas.Stroke.Assign(Self.FStroke);
    LCanvas.Font.Assign(Self.FFont);
  end else
    inherited;
end;

constructor TCanvasSaveState.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFill := TBrush.Create(TBrushKind.Solid, TAlphaColors.Black);
  FStroke := TStrokeBrush.Create(TBrushKind.Solid, TAlphaColors.White);
end;

destructor TCanvasSaveState.Destroy;
begin
  FFont.Free;
  FFill.Free;
  FStroke.Free;
  inherited;
end;

{ TColorObject }

procedure TColorObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if StyleName = '' then
    StyleName := Name;
end;

procedure RegisterAliases;
begin
  AddEnumElementAliases(TypeInfo(TGradientStyle), ['gsLinear', 'gsRadial']);
  AddEnumElementAliases(TypeInfo(TWrapMode), ['wmTile', 'wmTileOriginal', 'wmTileStretch']);
  AddEnumElementAliases(TypeInfo(TBrushKind), ['bkNone', 'bkSolid', 'bkGradient', 'bkBitmap', 'bkResource']);
  AddEnumElementAliases(TypeInfo(TStrokeCap), ['scFlat', 'scRound']);
  AddEnumElementAliases(TypeInfo(TStrokeJoin), ['sjMiter', 'sjRound', 'sjBevel']);
  AddEnumElementAliases(TypeInfo(TStrokeDash), ['sdSolid', 'sdDash', 'sdDot', 'sdDashDot', 'sdDashDotDot', 'sdCustom']);
  AddEnumElementAliases(TypeInfo(TStrokeBrush.TDashDevice), ['ddScreen', 'ddPrinter']);
  AddEnumElementAliases(TypeInfo(TMapAccess), ['maRead', 'maWrite', 'maReadWrite']);
  AddEnumElementAliases(TypeInfo(TPathPointKind), ['ppMoveTo', 'ppLineTo', 'ppCurveTo', 'ppClose']);
  AddEnumElementAliases(TypeInfo(TFillTextFlag), ['ftRightToLeft']);
  AddEnumElementAliases(TypeInfo(TCanvasQuality), ['ccSystemDefault', 'ccHighPerformance', 'ccHighQuality']);
  AddEnumElementAliases(TypeInfo(TCanvas.TMatrixMeaning), ['mmUnknown', 'mmIdentity', 'mmTranslate']);
end;

procedure UnregisterAliases;
begin
  RemoveEnumElementAliases(TypeInfo(TGradientStyle));
  RemoveEnumElementAliases(TypeInfo(TWrapMode));
  RemoveEnumElementAliases(TypeInfo(TBrushKind));
  RemoveEnumElementAliases(TypeInfo(TStrokeCap));
  RemoveEnumElementAliases(TypeInfo(TStrokeJoin));
  RemoveEnumElementAliases(TypeInfo(TStrokeDash));
  RemoveEnumElementAliases(TypeInfo(TStrokeBrush.TDashDevice));
  RemoveEnumElementAliases(TypeInfo(TMapAccess));
  RemoveEnumElementAliases(TypeInfo(TPathPointKind));
  RemoveEnumElementAliases(TypeInfo(TFillTextFlag));
  RemoveEnumElementAliases(TypeInfo(TCanvasQuality));
  RemoveEnumElementAliases(TypeInfo(TCanvas.TMatrixMeaning));
end;

initialization
  RegisterAliases;
  RegisterFmxClasses([TBrushObject, TFontObject, TPathObject, TBitmapObject, TColorObject]);
finalization
  UnregisterAliases;
end.
