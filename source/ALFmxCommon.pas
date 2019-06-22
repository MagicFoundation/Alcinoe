unit ALFmxCommon;

interface

{$IF Low(string) = 0}
  {$DEFINE _ZEROBASEDSTRINGS_ON}
{$ENDIF}

{$IF Defined(ANDROID) or defined(IOS)}
  {$DEFINE _USE_TEXTURE}
{$ENDIF}

{$IF defined(MACOS) and not defined(IOS)}
  {$DEFINE _MACOS}
{$ENDIF}

uses System.classes,
     System.UITypes,
     System.Types,
     System.Generics.Collections,
     System.Math.Vectors,
     {$IF defined(ios)}
     iOSapi.Foundation,
     iOSapi.CoreGraphics,
     iOSapi.CocoaTypes,
     iOSapi.CoreText,
     fmx.surfaces,
     fmx.types3D,
     {$ENDIF}
     {$IF defined(ANDROID)}
     Androidapi.JNI.GraphicsContentViewText,
     Androidapi.JNI.JavaTypes,
     fmx.types3D,
     ALAndroidApi,
     {$ENDIF}
     Fmx.types,
     FMX.TextLayout,
     FMX.graphics,
     FMX.Effects,
     FMX.controls;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALShadow = class(TPersistent)
  private
    fenabled: boolean;
    fblur: Single;
    fOffsetX: Single;
    fOffsetY: Single;
    fShadowColor: TAlphaColor;
    FOnChanged: TNotifyEvent;
    procedure SetEnabled(const Value: boolean);
    procedure setblur(const Value: Single);
    procedure setOffsetX(const Value: Single);
    procedure setOffsetY(const Value: Single);
    procedure setShadowColor(const Value: TAlphaColor);
    function IsblurStored: Boolean;
    function IsOffsetXStored: Boolean;
    function IsOffsetYStored: Boolean;
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property enabled: boolean read fEnabled Write SetEnabled default false;
    property blur: Single read fblur write setblur stored IsblurStored;
    property OffsetX: Single read fOffsetX write setOffsetX stored IsOffsetXStored;
    property OffsetY: Single read fOffsetY write setOffsetY stored IsOffsetYStored;
    property ShadowColor: TAlphaColor read fShadowColor write setShadowColor default $96000000;
  end;

type

  TALCustomConvertFontFamilyProc = function(const AFamily: TFontName; const aFontStyles: TfontStyles): TFontName;

var

  ALCustomConvertFontFamilyProc: TALCustomConvertFontFamilyProc;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function  ALConvertFontFamily(const AFamily: TFontName; const aFontStyles: TfontStyles): TFontName;
function  ALTranslate(const AText: string): string;
Procedure ALFmxMakeBufBitmaps(const aControl: TControl);
function  ALAlignAbsolutePointToPixelRound(const Point: TPointF; const Scale: single): TpointF;
function  ALAlignDimensionToPixelRound(const Rect: TRectF; const Scale: single): TRectF; overload;
function  ALAlignDimensionToPixelRound(const Dimension: single; const Scale: single): single; overload;
function  ALAlignDimensionToPixelRound(const Rect: TRectF): TRectF; overload;
function  ALAlignDimensionToPixelCeil(const Rect: TRectF; const Scale: single): TRectF; overload;
function  ALAlignDimensionToPixelCeil(const Dimension: single; const Scale: single): single; overload;
function  ALAlignDimensionToPixelCeil(const Rect: TRectF): TRectF; overload;
function  ALAlignToPixelRound(const Rect: TRectF): TRectF;

{$IF defined(IOS)}
function  ALLowerLeftCGRect(const aUpperLeftOrigin: TPointF; const aWidth, aHeight: single; const aGridHeight: Single): CGRect;
function  ALGetCTFontRef(const AFontFamily: String; const aFontSize: single; const aFontStyle: TFontStyles): CTFontRef;
{$ENDIF}

{$IF defined(ANDROID)}

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBreakTextItem = class(Tobject)
  public
    line: JString;
    pos: TpointF; // << pos of the bottom on the text (without descent)
    rect: TrectF;
    fontColor: TalphaColor; // << not initialised by ALBreakText
    fontStyle: integer; // << not initialised by ALBreakText
    id: string; // << not initialised by ALBreakText
    imgSrc: string; // << not initialised by ALBreakText
    isEllipsis: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;
  TALBreakTextItems = class(TobjectList<TALBreakTextItem>);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function ALBreakText(const aPaint: JPaint;
                     var ARect: TRectF;
                     const AText: JString;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming;
                     const aBreakTextItems: TALBreakTextItems;
                     var aTotalLines: integer;
                     var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                     const aFirstLineIndent: TpointF;
                     const aLineSpacing: single = 0;
                     const aEllipsisText: JString = nil;
                     const aEllipsisFontName: String = '';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; overload; // return true if text was breaked in several lines (truncated or not)
function ALBreakText(const aPaint: JPaint;
                     var ARect: TRectF;
                     const AText: JString;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming;
                     const aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: TpointF;
                     const aLineSpacing: single = 0;
                     const aEllipsisText: JString = nil;
                     const aEllipsisFontName: String = '';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; inline; overload; // return true if text was breaked in several lines (truncated or not)
{$ENDIF}

{$IF defined(IOS)}

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBreakTextItem = class(Tobject)
  public
    Line: CTLineRef;
    text: String;
    pos: TpointF; // << pos of the bottom on the text (without descent)
    rect: TrectF;
    fontColor: TalphaColor; // << not initialised by ALBreakText
    fontStyle: TFontStyles; // << not initialised by ALBreakText
    id: string; // << not initialised by ALBreakText
    imgSrc: string; // << not initialised by ALBreakText
    isEllipsis: Boolean;
    constructor Create;
    destructor Destroy; override;
  end;
  TALBreakTextItems = class(TobjectList<TALBreakTextItem>);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function ALBreakText(const aColorSpace: CGColorSpaceRef;
                     const aFontColor: TalphaColor;
                     const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     var ARect: TRectF;
                     const AText: string;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
                     const aBreakTextItems: TALBreakTextItems;
                     var aTotalLines: integer;
                     var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                     const aFirstLineIndent: TpointF;// kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent.x in aTextAttr
                     const aLineSpacing: single = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                     const aEllipsisText: string = '…';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; overload; // // return true if text was breaked in several lines (truncated or not)
function ALBreakText(const aColorSpace: CGColorSpaceRef;
                     const aFontColor: TalphaColor;
                     const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     var ARect: TRectF;
                     const AText: string;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
                     const aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: TpointF;// kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent.x in aTextAttr
                     const aLineSpacing: single = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                     const aEllipsisText: string = '…';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; inline; overload; // // return true if text was breaked in several lines (truncated or not)

{$ENDIF}

{$IF defined(MSWINDOWS) or defined(_MACOS)}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ALGetTextMetrics(const aFontSize: single;
                           const aFontStyle: TFontStyles;
                           const aFontName: String;
                           var aAscent:Single; // << return aAscent in negative (like in android)
                           var aDescent:Single);

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALBreakTextItem = class(Tobject)
  public
    Line: String;
    pos: TpointF; // << pos of the bottom on the text (without descent)
    rect: TrectF;
    fontColor: TalphaColor; // << not initialised by ALBreakText
    fontStyle: TFontStyles; // << not initialised by ALBreakText
    id: string; // << not initialised by ALBreakText
    imgSrc: string; // << not initialised by ALBreakText
    isEllipsis: Boolean;
    constructor Create;
  end;
  TALBreakTextItems = class(TobjectList<TALBreakTextItem>);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
function ALbreakText(const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     const atext: String;
                     const aMaxWidth: Single;
                     var aMeasuredWidth: Single): integer; overload;
function ALBreakText(const aFontColor: TalphaColor;
                     const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     var ARect: TRectF;
                     const AText: string;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
                     const aBreakTextItems: TALBreakTextItems;
                     var aTotalLines: integer;
                     var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                     const aFirstLineIndent: TpointF;// kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent.x in aTextAttr
                     const aLineSpacing: single = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                     const aEllipsisText: string = '…';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; overload; // // return true if text was breaked in several lines (truncated or not)

{$ENDIF}

type

  {~~~~~~~~~~~~~~~~~~~~~}
  TAlTextElement = record
    Id: string;
    rect: TrectF;
  end;
  TalTextElements = array of TalTextElement;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALDrawMultiLineTextOptions = class(Tobject)
  public
    //-----
    FontName: String;
    FontSize: single;
    FontStyle: TFontStyles;
    FontColor: TalphaColor;
    //-----
    EllipsisText: String; // default = '…';
    EllipsisFontStyle: TFontStyles; // default = [];
    EllipsisFontColor: TalphaColor; // default = TAlphaColorRec.Null;
    //-----
    AutoSize: Boolean; // default = True;
    AutoSizeX: Boolean; // default = False;
    AutoSizeY: Boolean; // default = False;
    WordWrap: Boolean; // default = True;
    MaxLines: integer; // default = 0;
    LineSpacing: single; // default = 0;
    Trimming: TTextTrimming; // default = TTextTrimming.Character;
    FirstLineIndent: TpointF; // default = Tpointf.create(0,0);
    FailIfTextBreaked: boolean; // default = false
    //-----
    HTextAlign: TTextAlign; // default = TTextAlign.Leading;
    VTextAlign: TTextAlign; // default = TTextAlign.Leading;
    //-----
    Fill: TBrush;  // default = none
    Stroke: TStrokeBrush; // default = none
    Sides: TSides; // default = AllSides
    XRadius: Single; // default = 0
    YRadius: Single; // default = 0
    Corners: TCorners; // default = AllCorners
    Padding: TRectF;  // default = 0
    //-----
    TextIsHtml: boolean; // default = false;
                         // NOTE: it's a partial html implementation, just for styling like <b>, <font color="">, <img src="xxx">, etc.
                         //       For exemple #13#10 are handle like breakline and not like space
    //-----
    constructor Create;
    destructor Destroy; override;
  End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// their is a bug, if you write something like: abcd#13#10<b>abcd</b>
// then you will obtain
//     abcd(<b>)abcd(</b>)
// instead of
//     abcd(#13#10)
//     (<b>)abcd(</b>)
// the workaround is to write instead abcd<b>#13#10abcd</b>
// not look very hard to correct but i have no time to do it right now
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              var aTextBreaked: boolean; // out => true if the text was "breaked" in several lines
                              var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                              var aAscent: single; // out => the Ascent of the last element (in real pixel)
                              var aDescent: Single; // out => the Descent of the last element (in real pixel)
                              var aFirstPos: TpointF; // out => the point of the start of the text
                              var aLastPos: TpointF; // out => the point of the end of the text
                              var aElements: TalTextElements; // out => the list of rect describing all span elements
                              var aEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              var aTextBreaked: boolean; // true is the text was "breaked" in several lines
                              var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              var aTextBreaked: boolean; // out => true is the text was "breaked" in several lines
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; inline; overload;
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF}; inline; overload;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure ALPaintRectangle({$IF defined(ANDROID)}
                           const aCanvas: Jcanvas;
                           {$ELSEIF defined(IOS)}
                           const aContext: CGContextRef;
                           const aColorSpace: CGColorSpaceRef;
                           const aGridHeight: Single;
                           {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
                           const aCanvas: Tcanvas;
                           {$ENDIF}
                           const dstRect: TrectF;
                           const Fill: TBrush;
                           const Stroke: TStrokeBrush;
                           const Shadow: TALShadow = nil; // if shadow then the Canvas must contain enalf space to draw the shadow (around Shadow.blur on each side of the rectangle)
                           const Sides: TSides = [TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]; // default = AllSides
                           const Corners: TCorners = [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]; // default = AllCorners
                           const XRadius: Single = 0;
                           const YRadius: Single = 0);

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure ALPaintCircle({$IF defined(ANDROID)}
                        const aCanvas: Jcanvas;
                        {$ELSEIF defined(IOS)}
                        const aContext: CGContextRef;
                        const aColorSpace: CGColorSpaceRef;
                        const aGridHeight: Single;
                        {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
                        const aCanvas: Tcanvas;
                        {$ENDIF}
                        const dstRect: TrectF;
                        const Fill: TBrush;
                        const Stroke: TStrokeBrush;
                        const Shadow: TALShadow = nil); // if shadow then the Canvas must contain enalf space to draw the shadow (around Shadow.blur on each side of the rectangle)

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ALCreateDrawingSurface({$IF defined(ANDROID)}
                                 Var aBitmap: Jbitmap;
                                 var aCanvas: Jcanvas;
                                 {$ELSEIF defined(IOS)}
                                 var aBitmapSurface: TbitmapSurface;
                                 Var aContext: CGContextRef;
                                 Var aColorSpace: CGColorSpaceRef;
                                 {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
                                 Var aBitmap: Tbitmap;
                                 const aClearBitmap: boolean;
                                 {$ENDIF}
                                 const w: integer;
                                 const h: integer);
procedure ALFreeDrawingSurface({$IF defined(ANDROID)}
                               Var aBitmap: Jbitmap;
                               var aCanvas: Jcanvas
                               {$ELSEIF defined(IOS)}
                               var aBitmapSurface: TbitmapSurface;
                               Var aContext: CGContextRef;
                               Var aColorSpace: CGColorSpaceRef
                               {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
                               Var aBitmap: Tbitmap
                               {$ENDIF});
{$IF defined(IOS)}
Procedure ALCreateDrawingSurfaceV2(var aBitmapSurface: TbitmapSurface;
                                   Var aContext: CGContextRef;
                                   Var aColorSpace: CGColorSpaceRef;
                                   const w: integer;
                                   const h: integer);
procedure ALFreeDrawingSurfaceV2(var aBitmapSurface: TbitmapSurface;
                                 Var aContext: CGContextRef);
{$ENDIF}


{$IF defined(ANDROID)}
function ALfontStyleToAndroidStyle(const afontStyle: TfontStyles): integer;
{$ENDIF}

{$IF defined(ANDROID)}
function ALStringsToJArrayList(const AStrings: TArray<String>): JArrayList;
function ALJSetToStrings(const ASet: JSet): TArray<String>;
{$ENDIF}

{$IF defined(IOS)}
function ALStringsToNSArray(const AStrings: TArray<String>): NSMutableArray;
function ALNSSetToStrings(const ANSSet: NSSet): TArray<String>;
{$ENDIF}

Type

  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if FMX.Controls.TControl still has the exact same fields and adjust the IFDEF'}
  {$ENDIF}
  TALControlAccessPrivate = class(TFmxObject)
  {$IF CompilerVersion >= 32}  // tokyo
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
    {$IF CompilerVersion >= 32}  // tokyo
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
    FUpdateRect: TRectF;
    FTabStop: Boolean;
    FDisableDisappear: Integer;
    FAnchorMove: Boolean;
    FApplyingEffect: Boolean;
    {$IF CompilerVersion >= 32}  // tokyo
    FExitingOrEntering: Boolean;
    FDelayedEvents: set of TDelayedEvent;
    {$ENDIF}
    FInflated: Boolean;
    FOnApplyStyleLookup: TNotifyEvent;
    FAlign: TAlignLayout;
    FAnchors: TAnchors;
    FUpdateEffects: Boolean; // << i personnally need to access this private field
    FDisableFocusEffect: Boolean;
    FTouchManager: TTouchManager;
    FOnGesture: TGestureEvent;
    FVisible: Boolean;
    FPressed: Boolean;
    FPressedPosition: TPointF;
    FDoubleClick: Boolean;
    FParentShowHint: Boolean;
    {$IF CompilerVersion >= 33}  // rio
    FCustomSceneAddRect: TCustomSceneAddRectEvent;
    {$ENDIF}
    FScene: IScene;
    FLastHeight: Single;
    FLastWidth: Single;
    FSize: TControlSize;
    FLocalMatrix: TMatrix;
    FAbsoluteMatrix: TMatrix;
    FInvAbsoluteMatrix: TMatrix;
    FEffectBitmap: TBitmap;
    FLocked: Boolean;
    FOpacity, FAbsoluteOpacity: Single;
    FInPaintTo: Boolean;
    FInPaintToAbsMatrix, FInPaintToInvMatrix: TMatrix;
    FAbsoluteHasEffect: Boolean;
    FAbsoluteHasDisablePaintEffect: Boolean;
    FAbsoluteHasAfterPaintEffect: Boolean;
    FUpdating: Integer; // << i personnally need to access this protected field
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

  {$IF CompilerVersion > 33} // rio
    {$MESSAGE WARN 'Check if FMX.TextLayout.TTextLayout still has the exact same fields and adjust the IFDEF'}
  {$ENDIF}
  TALTextLayoutAccessPrivate = class abstract
  public const
    MaxLayoutSize: TPointF = (X: $FFFF; Y: $FFFF);
  public
    FAttributes: TList<TTextAttributedRange>;
    FFont: TFont;
    FColor: TAlphaColor;
    FText: string;
    FWordWrap : Boolean;
    FHorizontalAlign: TTextAlign;
    FVerticalAlign: TTextAlign;
    FPadding: TBounds;
    FNeedUpdate: Boolean;
    FMaxSize: TPointF;
    FTopLeft: TPointF;
    FUpdating: Integer; // << i personnally need to access this protected field
    FOpacity: Single;
    FTrimming: TTextTrimming;
    FRightToLeft: Boolean;
    [weak] FCanvas: TCanvas;
    FMessageId: Integer;
  end;

{$IFDEF ANDROID}
var ALViewStackCount: integer;
{$ENDIF}

{$IFDEF ANDROID}
function getRenderScript: JRenderScript;
{$ENDIF}

implementation

uses system.SysUtils,
     System.Character,
     System.Math,
     {$IF defined(ANDROID)}
     Androidapi.JNIBridge,
     Androidapi.Helpers,
     Androidapi.JNI.Os,
     Androidapi.Bitmap,
     FMX.platForm.android,
     FMX.Platform.UI.Android,
     FMX.forms,
     alFmxEdit,
     {$ENDIF}
     {$IF defined(IOS)}
     iOSapi.UIKit,
     Macapi.ObjectiveC,
     Macapi.CoreFoundation,
     Macapi.Helpers,
     {$ENDIF}
     fmx.consts,
     ALFmxObjects,
     AlFmxStdCtrls,
     ALStringList,
     ALGraphics,
     ALString,
     AlCommon;

{***************************}
constructor TALShadow.Create;
begin
  inherited Create;
  fenabled := False;
  fblur := 12;
  fOffsetX := 0;
  fOffsetY := 0;
  fShadowColor := $96000000;
  FOnChanged := nil;
end;

{**********************************************}
procedure TALShadow.Assign(Source: TPersistent);
var aSaveChange: TNotifyEvent;
begin
  if Source is TALShadow then begin
    aSaveChange := FOnChanged;
    FOnChanged := nil;
    fenabled := TALShadow(Source).fenabled;
    fblur := TALShadow(Source).fblur;
    fOffsetX := TALShadow(Source).fOffsetX;
    fOffsetY := TALShadow(Source).fOffsetY;
    fShadowColor := TALShadow(Source).fShadowColor;
    FOnChanged := aSaveChange;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end
  else inherited;
end;

{***************************************}
function TALShadow.IsblurStored: Boolean;
begin
  result := not SameValue(fBlur, 12, Tepsilon.Position);
end;

{******************************************}
function TALShadow.IsOffsetXStored: Boolean;
begin
  result := not SameValue(fBlur, 0, Tepsilon.Position);
end;

{******************************************}
function TALShadow.IsOffsetYStored: Boolean;
begin
  result := not SameValue(fBlur, 0, Tepsilon.Position);
end;

{***************************************************}
procedure TALShadow.SetEnabled(const Value: boolean);
begin
  if fEnabled <> Value then begin
    fEnabled := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

{***********************************************}
procedure TALShadow.setblur(const Value: Single);
begin
  if Fblur <> Value then begin
    Fblur := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

{**************************************************}
procedure TALShadow.setOffsetX(const Value: Single);
begin
  if fOffsetX <> Value then begin
    fOffsetX := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

{**************************************************}
procedure TALShadow.setOffsetY(const Value: Single);
begin
  if fOffsetY <> Value then begin
    fOffsetY := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

{***********************************************************}
procedure TALShadow.setShadowColor(const Value: TAlphaColor);
begin
  if FShadowColor <> Value then begin
    FShadowColor := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

{************************************************************************************************}
function ALConvertFontFamily(const AFamily: TFontName; const aFontStyles: TfontStyles): TFontName;
begin
  if AFamily = '' then Exit('');
  if Assigned(ALCustomConvertFontFamilyProc) then begin
    Result := ALCustomConvertFontFamilyProc(AFamily, aFontStyles);
    if Result = '' then Result := AFamily;
    Exit;
  end;
  Result := AFamily;
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

{******************************************************}
Procedure ALFmxMakeBufBitmaps(const aControl: TControl);
var aChild: TControl;
begin

  //
  //if aControl is TPresentedControl then TPresentedControl(aControl).ApplyStyleLookup; // this to generate child controls
  //                                                                                    // that can be TALText for exemple
  //                                                                                    // (for the Tlabel)
  acontrol.DisableDisappear := true; // this to keep the style when the control get out of the visible are
                                     // else the style will be freed to be reaplied a little later

  if (aControl is TALText) then begin
    TALText(aControl).doubleBuffered := True;
    TALText(aControl).MakeBufBitmap;
  end
  else if (aControl is TALRectangle) then begin
    TALRectangle(aControl).doubleBuffered := True;
    TALRectangle(aControl).MakeBufBitmap;
  end
  else if (aControl is TALCircle) then begin
    TALCircle(aControl).doubleBuffered := True;
    TALCircle(aControl).MakeBufBitmap;
  end
  else if (aControl is TALImage) then TALImage(aControl).MakeBufBitmap
  else if (aControl is TALAniIndicator) then TALAniIndicator(aControl).MakeBufBitmap
  else if (aControl is TALCheckBox) then TALCheckBox(aControl).MakeBufBitmap
  else if (aControl is TALLine) then begin
    TALLine(aControl).doubleBuffered := True;
    TALLine(aControl).MakeBufBitmap;
  end;

  for aChild in aControl.Controls do
    ALFmxMakeBufBitmaps(aChild);

end;

{*********************************************************************************************}
function  ALAlignAbsolutePointToPixelRound(const Point: TPointF; const Scale: single): TpointF;
begin
  result.x := round(Point.x * Scale) / Scale;
  result.y := round(Point.y * Scale) / Scale;
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

{*****************************************************************}
function  ALAlignDimensionToPixelRound(const Rect: TRectF): TRectF;
begin
  result := Rect;
  result.Width := Round(Rect.Width);
  result.height := Round(Rect.height);
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

{****************************************************************}
function  ALAlignDimensionToPixelCeil(const Rect: TRectF): TRectF;
begin
  result := Rect;
  result.Width := ceil(Rect.Width - TEpsilon.Vector);
  result.height := ceil(Rect.height - TEpsilon.Vector);
end;

{********************************************************}
function  ALAlignToPixelRound(const Rect: TRectF): TRectF;
begin
  Result.Left := round(Rect.Left);
  Result.Top := round(Rect.Top);
  Result.Right := Result.Left + Round(Rect.Width); // keep ratio horizontally
  Result.Bottom := Result.Top + Round(Rect.Height); // keep ratio vertically
end;

{****************}
{$IF defined(IOS)}
function ALLowerLeftCGRect(const aUpperLeftOrigin: TPointF; const aWidth, aHeight: single; const aGridHeight: Single): CGRect;
begin
  Result.origin.x := aUpperLeftOrigin.x;
  Result.origin.Y := aGridHeight - aUpperLeftOrigin.y - aHeight;
  Result.size.Width := aWidth;
  Result.size.Height := aHeight;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if  FMX.Canvas.Mac.TTextLayoutCT.GetCTFontRef is still the same as below and adjust the IFDEF'}
{$ENDIF}
function  ALGetCTFontRef(const AFontFamily: String; const aFontSize: single; const aFontStyle: TFontStyles): CTFontRef;

const
  /// <summary> Rotating matrix to simulate Italic font attribute </summary>
  ItalicMatrix: CGAffineTransform = (
    a: 1;
    b: 0;
    c: 0.176326981; //~tan(10 degrees)
    d: 1;
    tx: 0;
    ty: 0
  );

var
  LFontRef, NewFontRef: CTFontRef;
  Matrix: PCGAffineTransform;

begin

  Result := nil;
  Matrix := nil;
  LFontRef := CTFontCreateWithName(CFSTR(AFontFamily){name}, AFontSize{size}, nil{matrix}); // Returns a CTFontRef that best matches the name provided with size and matrix attributes.
  try

    if TFontStyle.fsItalic in AFontStyle then begin
      NewFontRef := CTFontCreateCopyWithSymbolicTraits(LFontRef, 0, nil, kCTFontItalicTrait, kCTFontItalicTrait);  // Return a new font reference in the same family with the given symbolic traits. or NULL if none is found in the system.
      if NewFontRef <> nil then begin
        CFRelease(LFontRef);
        LFontRef := NewFontRef;
      end
      else begin
        // Font has no Italic version, applying transform matrix
        Matrix := @ItalicMatrix;
        NewFontRef := CTFontCreateWithName(CFSTR(AFontFamily), AFontSize, @ItalicMatrix);
        if NewFontRef <> nil then begin
          CFRelease(LFontRef);
          LFontRef := NewFontRef;
        end;
      end;
    end;

    if TFontStyle.fsBold in AFontStyle then begin
      NewFontRef := CTFontCreateCopyWithSymbolicTraits(LFontRef, 0, Matrix, kCTFontBoldTrait, kCTFontBoldTrait);
      if NewFontRef <> nil then begin
        CFRelease(LFontRef);
        LFontRef := NewFontRef;
      end;
    end;

    Result := LFontRef;

  except
    CFRelease(LFontRef);
    // don't raise any exception, return simply nil
  end;

end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
constructor TALBreakTextItem.Create;
begin
  inherited;
  Line := nil;
  isEllipsis := False;
end;
{$ENDIF}

{*********************}
{$IF defined(ANDROID)}
destructor TALBreakTextItem.Destroy;
begin
  line := Nil;
  inherited;
end;
{$ENDIF}

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
// Draw text in a given rectangle and automatically wrap lines.
// about emoticons i decide that if the font emoticons are not good enalf
// I will simply replace the font ! i will not add custom image
// (ie: emoticons) in the middle of the text !!
{$IF defined(ANDROID)}
function ALBreakText(const aPaint: JPaint;
                     var ARect: TRectF;
                     const AText: JString;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming;
                     const aBreakTextItems: TALBreakTextItems;
                     var aTotalLines: integer;
                     var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                     const aFirstLineIndent: TpointF;
                     const aLineSpacing: single = 0;
                     const aEllipsisText: JString = nil;
                     const aEllipsisFontName: String = '';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; // return true if text was breaked in several lines or truncated

var aBreakTextItemsStartCount: integer;
    aBreakTextItem: TALBreakTextItem;
    aNumberOfChars: integer;
    aSaveNumberOfChars: integer;
    aSaveNumberOfCharsIsAccurate: Boolean;
    aLine: jString;
    aLineIndent: Single;
    aEllipsisLine: Jstring;
    aEllipsisLineLn: single;
    aEllipsisLinePos: TpointF;
    aEllipsisLineRect: TrectF;
    aMaxWidth: single;
    aMaxHeight: single;
    aMaxLineWidth: single;
    aLineHeight: single;
    aTotalLinesHeight: single;
    aChar: Char;
    ATextLn: integer;
    ATextIdx: integer;
    aCurrLineY: single;
    aMetrics: JPaint_FontMetricsInt;
    ameasuredWidth: TJavaArray<Single>;
    aOffset: single;
    aLineEndWithBreakLine: Boolean;
    i, j: integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //their is a fucking bug on android 4.4.2 that aNumberOfChars
  //is the number of Glyph and not of char, so ligature like 'fi' are
  //counted like one glyph :(
  //very few comments about this on the internet
  //http://stackoverflow.com/questions/39891726/android-paint-breaktext-not-work-on-kitkat
  procedure _splitLigature(const _MaxWidth: single);
  var aTmpMeasuredWidth: Single;
  begin
    if (aNumberOfChars < aLine.length) and
       (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
      while aNumberOfChars < aLine.length  do begin
        aTmpMeasuredWidth := aPaint.measureText(aLine{text},
                                                0,
                                                aNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
        if compareValue(aTmpMeasuredWidth, _MaxWidth, TEpsilon.Position) > 0 then break
        else begin
          inc(aNumberOfChars);
          ameasuredWidth[0] := aTmpMeasuredWidth;
        end;
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _initEllipsis;
  var aSavedColor: TalphaColor;
      aSavedTypeFace: JTypeface;
      aTypeFace: JTypeface;
      JStr1: Jstring;
  begin
    if aEllipsisLine = nil then begin
      //-----
      if aEllipsisText = nil then aEllipsisLine := StringtoJString(string('…'))
      else aEllipsisLine := aEllipsisText;
      //-----
      aSavedTypeFace := nil; // stupid warning
      if aEllipsisFontName <> '' then begin
        aSavedTypeFace := aPaint.getTypeface;
        JStr1 := StringToJString(aEllipsisFontName); // << https://quality.embarcadero.com/browse/RSP-14187
        aTypeface := TJTypeface.JavaClass.create(JStr1, ALfontStyleToAndroidStyle(aEllipsisFontStyle));
        aPaint.setTypeface(aTypeface);
        aTypeface := nil;
        JStr1 := nil;
      end;
      //-----
      aSavedColor := TAlphaColorRec.Null; // stupid warning
      if aEllipsisFontColor <> TAlphaColorRec.Null then begin
        aSavedColor := Cardinal(aPaint.getColor);
        aPaint.setColor(integer(aEllipsisFontColor));
      end;
      //-----
      aEllipsisLineLn := aPaint.measureText(aEllipsisLine);
      if aEllipsisFontName <> '' then aPaint.setTypeface(aSavedTypeFace);
      if aEllipsisFontColor <> TAlphaColorRec.Null then aPaint.setColor(integer(aSavedColor));
      //-----
      case AHTextAlign of
        TTextAlign.Center: begin
                             aEllipsisLinePos := TpointF.create((aMaxWidth - aEllipsisLineLn - aLineIndent) / 2, aCurrLineY);
                           end;
        TTextAlign.Leading: begin
                              aEllipsisLinePos := TpointF.create(aLineIndent, aCurrLineY);
                            end;
        TTextAlign.Trailing: begin
                               aEllipsisLinePos := TpointF.create(aMaxWidth - aEllipsisLineLn, aCurrLineY);
                             end;
      end;
      aEllipsisLinerect := Trectf.Create(TPointF.Create(aEllipsisLinePos.x,
                                                        aEllipsisLinePos.Y - (-1*aMetrics.ascent)),
                                         aEllipsisLineLn,
                                         (-1*aMetrics.ascent) + aMetrics.descent);
      //-----
    end;
  end;

begin

  //init result
  result := false;
  aAllTextDrawed := true;

  //init aBreakTextItemsStartCount
  aBreakTextItemsStartCount := aBreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / aTotalLinesHeight
  if aRect.Width > 16384 then aRect.Width := 16384;  // << because on android kitkat (4.4.2) it's look like that aPaint.breakText with maxWidth > 16384 return 0 :(
  if aRect.height > 16384 then aRect.height := 16384;
  aMaxWidth := ARect.width;
  aMaxHeight := ARect.Height;
  aMaxLineWidth := 0;
  aTotalLinesHeight := 0;

  //init ATextIdx / ATextLn
  ATextIdx := 0;
  ATextLn := AText.length;

  //init metics / aCurrLineY / aLineHeight
  aMetrics := aPaint.getFontMetricsInt; // aMetrics.top       => The maximum distance above the baseline for the tallest glyph in the font at a given text size.
                                        // aMetrics.ascent    => The recommended distance above the baseline for singled spaced text.
                                        // aMetrics.descent   => The recommended distance below the baseline for singled spaced text.
                                        // aMetrics.bottom    => The maximum distance below the baseline for the lowest glyph in the font at a given text size
                                        // aMetrics.leading   => The recommended additional space to add between lines of text
  aCurrLineY := aFirstLineIndent.y + (-1*aMetrics.ascent); // aMetrics.top and aMetrics.ascent are always returned in negative value
  aTotalLines := 0;
  aLineHeight := aMetrics.descent + aLineSpacing + (-1*aMetrics.ascent);

  //init aEllipsisLine
  aEllipsisLine := nil;
  aEllipsisLineLn := 0;

  //init aLineIndent
  aLineIndent := aFirstLineIndent.x;

  //if we have at least enalf of height to write the 1rt row
  if comparevalue(aFirstLineIndent.y + aMetrics.descent + (-1*aMetrics.Ascent),aMaxHeight,Tepsilon.position) <= 0 then begin

    //create ameasuredWidth
    ameasuredWidth := TJavaArray<Single>.Create(1);
    try

      //loop still their is some chars
      while ATextIdx < ATextLn do begin

        // init aline
        aLine := nil; // << https://quality.embarcadero.com/browse/RSP-14187
        i := aText.indexOf($0D {c}, ATextIdx{start}); // find if their is some #13 (MSWINDOWS linebreak = #13#10)
        j := aText.indexOf($0A {c}, ATextIdx{start}); // find if their is some #10 (UNIX linebreak = #10)
        if (i >= 0) and (j >= 0) then I := min(i,j)
        else I := max(I, J);
        if i = ATextIdx then begin
          aLine := StringtoJString(string(''));
          aLineEndWithBreakLine := True;
          result := true;
        end
        else if i > 0 then begin
          aLine := aText.substring(ATextIdx{start}, i{end_}); // skip the $0D/$0A
          aLineEndWithBreakLine := True;
          result := true;
        end
        else begin
          aLine := aText.substring(ATextIdx{start});
          aLineEndWithBreakLine := False;
        end;

        //calculate the number of char in the current line (this work good also if aline is empty)
        aNumberOfChars := aPaint.breakText(aLine {text},
                                           true {measureForwards},
                                           aMaxWidth - aLineIndent, {maxWidth}
                                           ameasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                            // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                            // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                            //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                            //   it's return for exemple 9 when height = 11
       _splitLigature(aMaxWidth - aLineIndent);

        //init result
        if aNumberOfChars < aLine.length then result := true;

        //if we need to break the text
        if (aNumberOfChars < aLine.length) or // << if aNumberOfChars < aLine.length it's evident we will need to break the text
           (                                                                                                    // <<
            (aLineEndWithBreakLine) and                                                                         // <<
            (aTrimming <> TTextTrimming.None) and                                                               // <<
            (                                                                                                   // << we need this check to add the ellipsis on the last line
             (not aWordWrap) or                                                                                 // << when the last line finish by a break line (#13#10)
             ((compareValue(aCurrLineY + aLineHeight + aMetrics.descent, aMaxHeight, Tepsilon.position) > 0) or // <<
              ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1)))                                             // <<
            )                                                                                                   // <<
           )                                                                                                    // <<
        then begin

          //if not aWordWrap
          if not aWordWrap then begin
            aAllTextDrawed := False; // aNumberOfChars < aLine.length so in anycase we will not draw all the text
            case aTrimming of
              TTextTrimming.None: begin
                                    if aNumberOfChars > 0 then
                                      aLine := aLine.substring(0, aNumberOfChars);
                                  end;
              TTextTrimming.Character: begin
                                         //-----
                                         _initEllipsis;
                                         //-----
                                         if (aNumberOfChars < aLine.length) then dec(aNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                      // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                      // we considere that aEllipsisText is only one char
                                         while aNumberOfChars > 0 do begin
                                           aLine := aLine.substring(0, aNumberOfChars);
                                           aNumberOfChars := aPaint.breakText(aLine {text},
                                                                              true {measureForwards},
                                                                              aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                                              ameasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                                               // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                                               // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                                               //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                                               //   it's return for exemple 9 when height = 11
                                           _splitLigature(aMaxWidth - aEllipsisLineLn - aLineIndent);
                                           if aNumberOfChars >= aLine.length then break;
                                         end;
                                         //-----
                                       end;
              TTextTrimming.Word: begin
                                    //-----
                                    _initEllipsis;
                                    //-----
                                    aSaveNumberOfChars := aNumberOfChars;
                                    aSaveNumberOfCharsIsAccurate := False;
                                    while aNumberOfChars > 0 do begin
                                      //----
                                      if (aNumberOfChars >= aLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                                        aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                                      end
                                      //----
                                      else if aNumberOfChars >= 2 then begin
                                        aChar := aLine.charAt(aNumberOfChars-2);
                                        if (not aChar.IsWhiteSpace) or (Achar.ToUCS4Char = $00A0{No-break Space}) then begin
                                          dec(aNumberOfChars);
                                          continue;
                                        end;
                                        aLine := aLine.substring(0, aNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                                      end
                                      //----
                                      else begin
                                        aNumberOfChars := aSaveNumberOfChars;
                                        if (not aSaveNumberOfCharsIsAccurate) and (aNumberOfChars < aLine.length) then dec(aNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                                                            // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                                                            // we considere that aEllipsisText is only one char
                                        while aNumberOfChars > 0 do begin
                                          aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                                          aNumberOfChars := aPaint.breakText(aLine {text},
                                                                             true {measureForwards},
                                                                             aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                                             ameasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                                              // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                                              // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                                              //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                                              //   it's return for exemple 9 when height = 11
                                          _splitLigature(aMaxWidth - aEllipsisLineLn - aLineIndent);
                                          if aNumberOfChars >= aLine.length then break;
                                        end;
                                        break;
                                      end;
                                      //----
                                      aNumberOfChars := aPaint.breakText(aLine {text},
                                                                         true {measureForwards},
                                                                         aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                                         ameasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                                          // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                                          // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                                          //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                                          //   it's return for exemple 9 when height = 11
                                      _splitLigature(aMaxWidth - aEllipsisLineLn - aLineIndent);
                                      if aNumberOfChars >= aLine.length then break
                                      else begin
                                        aSaveNumberOfChars:= aNumberOfChars;
                                        aSaveNumberOfCharsIsAccurate := True;
                                      end;
                                      //----
                                    end;
                                  end;
            end;
          end

          //if aWordWrap
          else begin

            //We are at the last line and aTrimming <> TTextTrimming.None
            if ((compareValue(aCurrLineY + aLineHeight + aMetrics.descent, aMaxHeight, Tepsilon.position) > 0) or
                ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1))) and
               (aTrimming <> TTextTrimming.None) then begin

              //-----
              aAllTextDrawed := False; // if we are at the last line then in anycase we will not draw all the text
              _initEllipsis;
              //-----
              aSaveNumberOfChars := aNumberOfChars;
              aSaveNumberOfCharsIsAccurate := False;
              while aNumberOfChars > 0 do begin
                //----
                if (aNumberOfChars >= aLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                  aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                end
                //----
                else if (aTrimming = TTextTrimming.Word) and (aNumberOfChars >= 2) then begin
                  aChar := aLine.charAt(aNumberOfChars-2);
                  if (not aChar.IsWhiteSpace) or (Achar.ToUCS4Char = $00A0{No-break Space}) then begin
                    dec(aNumberOfChars);
                    continue;
                  end;
                  aLine := aLine.substring(0, aNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                end
                //----
                else begin
                  aNumberOfChars := aSaveNumberOfChars;
                  if (not aSaveNumberOfCharsIsAccurate) and (aNumberOfChars < aLine.length) then dec(aNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                                      // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                                      // we considere that aEllipsisText is only one char
                  while aNumberOfChars > 0 do begin
                    aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                    aNumberOfChars := aPaint.breakText(aLine {text},
                                                       true {measureForwards},
                                                       aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                       ameasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                        // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                        // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                        //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                        //   it's return for exemple 9 when height = 11
                    _splitLigature(aMaxWidth - aEllipsisLineLn - aLineIndent);
                    if aNumberOfChars >= aLine.length then break;
                  end;
                  break;
                end;
                //----
                aNumberOfChars := aPaint.breakText(aLine {text},
                                                   true {measureForwards},
                                                   aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                   ameasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                    // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                    // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                    //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                    //   it's return for exemple 9 when height = 11
                _splitLigature(aMaxWidth - aEllipsisLineLn - aLineIndent);
                if aNumberOfChars >= aLine.length then break
                else begin
                  aSaveNumberOfChars:= aNumberOfChars;
                  aSaveNumberOfCharsIsAccurate := True;
                end;
                //----
              end;

            end

            //We are not at the last line or aTrimming = TTextTrimming.None
            else begin

              //We are at the last line and aTrimming = TTextTrimming.None and more line available
              if (aTrimming <> TTextTrimming.None) and
                 ((compareValue(aCurrLineY + aLineHeight + aMetrics.descent, aMaxHeight, Tepsilon.position) > 0) or
                  ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1))) then aAllTextDrawed := False;

              //cut the line
              aSaveNumberOfChars := aNumberOfChars;
              if aNumberOfChars < aLine.length then inc(aNumberOfChars); // in case the space separator is just after aNumberOfChars
              while aNumberOfChars > 0 do begin
                //-----
                if aNumberOfChars >= 2 then begin
                  aChar := aLine.charAt(aNumberOfChars-1);
                  if (not aChar.IsWhiteSpace) or (Achar.ToUCS4Char = $00A0{No-break Space}) then begin
                    dec(aNumberOfChars);
                    continue;
                  end;
                  aLine := aLine.substring(0, aNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish just before the space
                end
                //-----
                else begin
                  aNumberOfChars := aSaveNumberOfChars;
                  if compareValue(aLineIndent, 0, TEpsilon.position) > 0 then aNumberOfChars := 0;
                  while aNumberOfChars > 0 do begin
                    aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                    aNumberOfChars := aPaint.breakText(aLine {text},
                                                       true {measureForwards},
                                                       aMaxWidth - aLineIndent, {maxWidth}
                                                       ameasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                        // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                        // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                        //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                        //   it's return for exemple 9 when height = 11
                    _splitLigature(aMaxWidth - aLineIndent);
                    if aNumberOfChars >= aLine.length then break;
                  end;
                  break;
                end;
                //-----
                aNumberOfChars := aPaint.breakText(aLine {text},
                                                   true {measureForwards},
                                                   aMaxWidth - aLineIndent, {maxWidth}
                                                   ameasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                    // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                    // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                    //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                    //   it's return for exemple 9 when height = 11
                _splitLigature(aMaxWidth - aLineIndent);
                if aNumberOfChars >= aLine.length then begin
                  inc(aNumberOfChars); // to skip the separator
                  break;
                end
                else aSaveNumberOfChars:= aNumberOfChars;
                //-----
              end;

            end;

          end;

        end;

        //init aMaxLineWidth
        aMaxLineWidth := max(aMaxLineWidth, ameasuredWidth[0] + aEllipsisLineLn + aLineIndent);

        // update aTotalLinesHeight
        aTotalLinesHeight := aCurrLineY + aMetrics.descent;

        // their is not enalf of place to write at least one char or
        // we are on the #13/#10
        // NOTE: we need to remove the breakline because for exemple we have
        // coco sur le cocotier#13#10
        // then aline will be equal to
        // coco sur le cocotier
        // we draw this line, and then we increate aCurrLineY := aCurrLineY + aLineHeight;
        // so it's mean the #13#10 was already taking in account, so we must delete it
        if aNumberOfChars <= 0 then begin
          if (ATextIdx + 1 < ATextLn) and
             (aText.codePointAt(ATextIdx) = $0D) and
             (aText.codePointAt(ATextIdx + 1) = $0A) then ATextIdx := ATextIdx + 2 // (MSWINDOWS linebreak = #13#10)
          else if (ATextIdx < ATextLn) and
                  (aText.codePointAt(ATextIdx) = $0A) then ATextIdx := ATextIdx + 1 // (UNIX linebreak = #10)
          else if (ATextIdx < ATextLn) and
                  ((aText.charAT(ATextIdx).IsWhiteSpace) and
                   (aText.charAT(ATextIdx).ToUCS4Char <> $00A0{No-break Space})) then ATextIdx := ATextIdx + 1 // (white space) if we don't have place to write
                                                                                                               // ' blabla' then break it in
                                                                                                               //
                                                                                                               //  |yoyoyoyo|
                                                                                                               //  |blabla  |
                                                                                                               //
                                                                                                               //  and not in
                                                                                                               //
                                                                                                               //  |yoyoyoyo|
                                                                                                               //  | blabla |
                                                                                                               //
          else if compareValue(aLineIndent, 0, Tepsilon.Position) <= 0 then begin // if aLineIndent > 0 then maybe we don't have enalf of place to write one char because of the aLineIndent.
            ATextIdx := ATextIdx + 1; // skip the current char
            if (ATextIdx < ATextLn) and
               (aText.CharAT(ATextIdx).IsLowSurrogate) then inc(ATextIdx);
          end;
          aCurrLineY := aCurrLineY + aLineHeight; // go to next line
          inc(aTotalLines);
          aLineIndent := 0;
          if (not aWordWrap) or
             ((aMaxLines > 0) and (aTotalLines >= aMaxLines)) or
             (compareValue(aCurrLineY + aMetrics.descent, aMaxHeight, TEpsilon.position) > 0) then break
          else continue;
        end
        else begin
          aTextIdx := ATextIdx + aNumberOfChars;
          if (ATextIdx + 1 < ATextLn) and
             (aText.codePointAt(ATextIdx) = $0D) and
             (aText.codePointAt(ATextIdx + 1) = $0A) then ATextIdx := ATextIdx + 2 // (MSWINDOWS linebreak = #13#10)
          else if (ATextIdx < ATextLn) and
                  (aText.codePointAt(ATextIdx) = $0A) then ATextIdx := ATextIdx + 1;
        end;

        //init aBreakedText
        aBreakTextItem := TalBreakTextItem.Create;
        try

          //update aBreakTextItem
          aBreakTextItem.line := aLine;
          case AHTextAlign of
            TTextAlign.Center: begin
                                 aBreakTextItem.pos := TpointF.create((aMaxWidth - ameasuredWidth[0] - aEllipsisLineLn - aLineIndent) / 2, aCurrLineY);
                               end;
            TTextAlign.Leading: begin
                                  aBreakTextItem.pos := TpointF.create(aLineIndent, aCurrLineY);
                                end;
            TTextAlign.Trailing: begin
                                   aBreakTextItem.pos := TpointF.create(aMaxWidth - ameasuredWidth[0] - aEllipsisLineLn, aCurrLineY);
                                 end;
          end;
          aBreakTextItem.rect := Trectf.Create(TPointF.Create(aBreakTextItem.pos.x,
                                                              aBreakTextItem.pos.Y - (-1*aMetrics.ascent)),
                                               ameasuredWidth[0],
                                               (-1*aMetrics.ascent) + aMetrics.descent);

          //update aEllipsisLinePos / aEllipsisLinerect
          if aEllipsisLine <> nil then begin
            aEllipsisLinePos := TpointF.Create(aBreakTextItem.pos.x + ameasuredWidth[0], aCurrLineY);
            aEllipsisLinerect := Trectf.Create(TPointF.Create(aBreakTextItem.pos.x + ameasuredWidth[0],
                                                              aBreakTextItem.pos.Y - (-1*aMetrics.ascent)),
                                               aEllipsisLineLn,
                                               (-1*aMetrics.ascent) + aMetrics.descent);
          end;

          // update aBreakTextItems
          aBreakTextItems.Add(aBreakTextItem);

        except
          ALFreeAndNil(aBreakTextItem);
          raise;
        end;

        //update aCurrLineY
        aCurrLineY := aCurrLineY + aLineHeight;
        inc(aTotalLines);

        //update aLineIndent
        aLineIndent := 0;

        // stop if not aWordWrap or after maxheight
        if (not aWordWrap) or
           ((aMaxLines > 0) and (aTotalLines >= aMaxLines)) or
           (compareValue(aCurrLineY + aMetrics.descent, aMaxHeight, TEpsilon.position) > 0) then break;

        //add the last empty row if their is one
        if (ATextIdx >= ATextLn) and aLineEndWithBreakLine and (aEllipsisLine = nil) then begin

          //init aBreakedText
          aBreakTextItem := TalBreakTextItem.Create;
          try

            //update aBreakTextItem
            aBreakTextItem.line := StringToJstring('');
            case AHTextAlign of
              TTextAlign.Center: begin
                                   aBreakTextItem.pos := TpointF.create((aMaxWidth - aEllipsisLineLn - aLineIndent) / 2, aCurrLineY);
                                 end;
              TTextAlign.Leading: begin
                                    aBreakTextItem.pos := TpointF.create(aLineIndent, aCurrLineY);
                                  end;
              TTextAlign.Trailing: begin
                                     aBreakTextItem.pos := TpointF.create(aMaxWidth - aEllipsisLineLn, aCurrLineY);
                                   end;
            end;
            aBreakTextItem.rect := Trectf.Create(TPointF.Create(aBreakTextItem.pos.x,
                                                                aBreakTextItem.pos.Y - (-1*aMetrics.Ascent)),
                                                 0,
                                                 (-1*aMetrics.Ascent) + aMetrics.Descent);

            // update aBreakTextItems
            aBreakTextItems.Add(aBreakTextItem);

          except
            ALFreeAndNil(aBreakTextItem);
            raise;
          end;

          //update aCurrLineY
          aCurrLineY := aCurrLineY + aLineHeight;
          inc(aTotalLines);

        end;

      end;

    finally
      ALFreeAndNil(ameasuredWidth);
      aLine := nil; // << https://quality.embarcadero.com/browse/RSP-14187
    end;

  end
  else result := true;

  //add the end ellipsis
  if aEllipsisLine <> nil then begin
    aBreakTextItem := TalBreakTextItem.Create;
    try
      aBreakTextItem.line := aEllipsisLine;
      aEllipsisLine := nil;
      aBreakTextItem.pos := aEllipsisLinePos;
      aBreakTextItem.rect := aEllipsisLineRect;
      aBreakTextItem.isEllipsis := True;
      aBreakTextItems.Add(aBreakTextItem);
    except
      ALFreeAndNil(aBreakTextItem);
      raise;
    end;
  end;

  //initialise ARect
  if compareValue(aMaxLineWidth, aMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            aOffset := Floor((aRect.Right - aMaxLineWidth - arect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            aRect.Left := aRect.Left + aOffset;
                            aRect.right := aRect.right - aOffset;
                            for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                              aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
                              aBreakTextItems[I].rect.Offset(-aOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             aRect.Right := min(aRect.Right, aRect.Left + aMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              aOffset := Floor(aRect.Right - aMaxLineWidth - arect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              aRect.Left := aRect.Left + aOffset;
                              for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                                aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
                                aBreakTextItems[I].rect.Offset(-aOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(aTotalLinesHeight, aMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            aOffset := (aRect.bottom - aTotalLinesHeight - arect.top) / 2;
                            aRect.top := aRect.top + aOffset;
                            aRect.bottom := aRect.bottom - aOffset;
                          end;
       TTextAlign.Leading: begin
                             aRect.bottom := min(aRect.bottom, aRect.top + aTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              aOffset := aRect.bottom - aTotalLinesHeight - arect.top;
                              aRect.top := aRect.top + aOffset;
                            end;
    end;
  end;

end;
{$ENDIF}

{********************}
{$IF defined(ANDROID)}
function ALBreakText(const aPaint: JPaint;
                     var ARect: TRectF;
                     const AText: JString;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming;
                     const aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: TpointF;
                     const aLineSpacing: single = 0;
                     const aEllipsisText: JString = nil;
                     const aEllipsisFontName: String = '';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; // return true if text was breaked in several lines (truncated or not)
var aTotalLines: integer;
    aAllTextDrawed: boolean;
begin
  result := ALBreakText(aPaint, // const aPaint: JPaint;
                        ARect, // var ARect: TRectF;
                        AText, // const AText: JString;
                        aWordWrap, // const aWordWrap: Boolean;
                        AHTextAlign, AVTextAlign, // const AHTextAlign, AVTextAlign: TTextAlign;
                        aTrimming, // const aTrimming: TTextTrimming;
                        aBreakTextItems, // const aBreakTextItems: TALBreakTextItems;
                        aTotalLines, // var aTotalLines: integer;
                        aAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                        aFirstLineIndent, // const aFirstLineIndent: TpointF;
                        aLineSpacing, // const aLineSpacing: single = 0;
                        aEllipsisText, // const aEllipsisText: JString = nil;
                        aEllipsisFontName, // const aEllipsisFontName: String = '';
                        aEllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                        aEllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                        aMaxlines); //const aMaxlines: integer = 0): boolean;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
constructor TALBreakTextItem.Create;
begin
  inherited;
  Line := nil;
  isEllipsis := False;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
destructor TALBreakTextItem.Destroy;
begin
  if line <> nil then begin
    cfRelease(Line);
    line := Nil;
  end;
  inherited;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function ALBreakText(const aColorSpace: CGColorSpaceRef;
                     const aFontColor: TalphaColor;
                     const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     var ARect: TRectF;
                     const AText: string;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
                     const aBreakTextItems: TALBreakTextItems;
                     var aTotalLines: integer;
                     var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                     const aFirstLineIndent: TpointF;// kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent.x in aTextAttr
                     const aLineSpacing: single = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                     const aEllipsisText: string = '…';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; // // return true if text was breaked in several lines (truncated or not)

var aBreakTextItemsStartCount: integer;
    aBreakTextItem: TALBreakTextItem;
    aEllipsisBreakTextItem: TALBreakTextItem;
    aEllipsisLine: CtLineRef;
    aEllipsisWidth: Double;
    aEllipsisFont: CTFontRef;
    aEllipsisString: CFStringRef;
    aEllipsisAttr: CFMutableAttributedStringRef;
    aEllipsisColor: CGColorRef;
    aFramePath: CGMutablePathRef;
    aFrameSetter: CTFramesetterRef;
    aFrame: CTFrameRef;
    alines: CFArrayRef;
    aline: CTLineRef;
    aLinesCount: CFIndex;
    aLineIndent: single;
    aTextAttr: CFMutableAttributedStringRef;
    aTmpTextAttr: CFAttributedStringRef;
    aMaxWidth: single;
    aMaxHeight: single;
    aPrevMaxLineWidth: Single;
    aMaxLineWidth: single;
    aCurrLineY: single;
    aTotalLinesHeight: single;
    aAscent, aDescent: CGFloat;
    aMeasuredWidth: Double;
    aSettings: array of CTParagraphStyleSetting;
    aParagraphStyle: CTParagraphStyleRef;
    aCGColor: CGColorRef;
    aTextString: CFStringRef;
    aFont: CTFontRef;
    aOffset: single;
    aStringRange: CFRange;
    aFirstLineHeadIndent: CGFloat;
    aLineSpacingAdjustment: CGFloat;
    aLineBreakMode: Byte;
    aAlphaColor: TAlphaColorCGFloat;
    i: CFIndex;

begin

  //init aAllTextDrawed
  aAllTextDrawed := True;

  //init aBreakTextItemsStartCount
  aBreakTextItemsStartCount := aBreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / aTotalLinesHeight / etc.
  if aRect.Width > 65535 then aRect.Width := 65535;
  if aRect.height > 65535 then aRect.height := 65535;
  aMaxWidth := ARect.width;
  aMaxHeight := ARect.Height;
  aPrevMaxLineWidth := 0; // << need this vars because we must recalculate the maxlineWidth for the last lines after the truncation is maded
  aMaxLineWidth := 0;
  aTotalLinesHeight := 0;
  aTotalLines := 0;
  aLineIndent := aFirstLineIndent.x;

  //create aCGColor
  aAlphaColor := TAlphaColorCGFloat.Create(aFontColor);
  aCGColor := CGColorCreate(aColorSpace, @aAlphaColor);
  try

    //create aFont
    aFont := ALGetCTFontRef(aFontName, aFontSize, aFontStyle); // Returns a new font reference for the given name.
    if aFont = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;
    try

      //create aTextString
      aTextString := CFStringCreateWithCharacters(kCFAllocatorDefault, @AText[Low(string)], Length(AText));
      if aTextString = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;
      try

        //create aTextAttr
        aTextAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
        try

          CFAttributedStringReplaceString(aTextAttr, CFRangeMake(0, 0), aTextString); // Modifies the string of an attributed string.
          CFAttributedStringBeginEditing(aTextAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
          try
            CFAttributedStringSetAttribute(aTextAttr, CFRangeMake(0, CFStringGetLength(aTextString)), kCTFontAttributeName, aFont);
            CFAttributedStringSetAttribute(aTextAttr, CFRangeMake(0, CFStringGetLength(aTextString)), kCTForegroundColorAttributeName, aCGColor);
            //-----
            SetLength(aSettings, 0);
            //-----
            //kCTParagraphStyleSpecifierAlignment
            //The text alignment. Natural text alignment is realized as left or right alignment, depending on the line sweep direction
            //of the first script contained in the paragraph. Type: CTTextAlignment. Default: kCTNaturalTextAlignment.
            //* kCTTextAlignmentCenter
            //* kCTTextAlignmentJustified
            //* kCTTextAlignmentLeft
            //* kCTTextAlignmentNatural
            //* kCTTextAlignmentRight
            //-----
            //kCTParagraphStyleSpecifierBaseWritingDirection
            //The base writing direction of the lines. Type: CTWritingDirection. Default: kCTWritingDirectionNatural. Application: CTFramesetter, CTTypesetter.
            //* kCTWritingDirectionNatural: The writing direction is algorithmically determined using the Unicode Bidirectional Algorithm rules P2 and P3.
            //* kCTWritingDirectionLeftToRight: The writing direction is left to right.
            //* kCTWritingDirectionRightToLeft: The writing direction is right to left.
            //-----
            //kCTParagraphStyleSpecifierCount
            //The number of style specifiers. The purpose is to simplify validation of style specifiers
            //-----
            //kCTParagraphStyleSpecifierDefaultTabInterval
            //The documentwide default tab interval. Tabs after the last specified by kCTParagraphStyleSpecifierTabStops are placed at
            //integer multiples of this distance (if positive). Type: CGFloat. Default: 0.0. Application: CTFramesetter, CTTypesetter.
            //-----
            //kCTParagraphStyleSpecifierFirstLineHeadIndent
            //The distance, in points, from the leading margin of a frame to the beginning of the paragraph's first line. This value
            //is always nonnegative. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
            if (compareValue(aFirstLineIndent.x, 0, TEpsilon.position) > 0) then begin
              SetLength(aSettings, length(aSettings) + 1);
              aFirstLineHeadIndent := aFirstLineIndent.x;
              aSettings[high(aSettings)].spec := kCTParagraphStyleSpecifierFirstLineHeadIndent;
              aSettings[high(aSettings)].valueSize := SizeOf(aFirstLineHeadIndent);
              aSettings[high(aSettings)].value := @aFirstLineHeadIndent;
            end;
            //-----
            //kCTParagraphStyleSpecifierHeadIndent
            //The distance, in points, from the leading margin of a text container to the beginning of lines other than the first.
            //This value is always nonnegative. Type: CGFloat Default: 0.0 Application: CTFramesetter
            //-----
            //kCTParagraphStyleSpecifierLineBreakMode
            //The mode that should be used to break lines when laying out the paragraph's text. Type: CTLineBreakMode.
            //Default: kCTLineBreakByWordWrapping. Application: CTFramesetter
            //* kCTLineBreakByWordWrapping: Wrapping occurs at word boundaries unless the word itself doesn't fit on a single line.
            //* kCTLineBreakByCharWrapping: Wrapping occurs before the first character that doesn't fit.
            //* kCTLineBreakByClipping: Lines are simply not drawn past the edge of the frame.
            //* kCTLineBreakByTruncatingHead: Each line is displayed so that the end fits in the frame and the missing text is indicated by an ellipsis glyph.
            //* kCTLineBreakByTruncatingTail: Each line is displayed so that the beginning fits in the container and the missing text is indicated by an ellipsis glyph.
            //* kCTLineBreakByTruncatingMiddle: Each line is displayed so that the beginning and end fit in the container and the missing text is indicated by an ellipsis glyph in the middle.
            if not aWordWrap then begin
              SetLength(aSettings, length(aSettings) + 1);
              case aTrimming of
                TTextTrimming.None: aLineBreakMode := kCTLineBreakByClipping;
                TTextTrimming.Character: aLineBreakMode := kCTLineBreakByCharWrapping;
                TTextTrimming.Word: aLineBreakMode := kCTLineBreakByWordWrapping;
              end;
              aSettings[high(aSettings)].spec := kCTParagraphStyleSpecifierLineBreakMode;
              aSettings[high(aSettings)].valueSize := SizeOf(aLineBreakMode);
              aSettings[high(aSettings)].value := @aLineBreakMode;
            end;
            //-----
            //kCTParagraphStyleSpecifierLineHeightMultiple
            //The line height multiple. The natural line height of the receiver is multiplied by this factor (if positive) before
            //being constrained by minimum and maximum line height. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
            //-----
            //kCTParagraphStyleSpecifierLineSpacing
            //Deprecated. Use kCTParagraphStyleSpecifierMaximumLineSpacing, kCTParagraphStyleSpecifierMinimumLineSpacing, and
            //kCTParagraphStyleSpecifierLineSpaceAdjustment to control space between lines. The space in points added between
            //lines within the paragraph (commonly known as leading). This value is always nonnegative. Type: CGFloat.
            //Default: 0.0. Application: CTFramesetter.
            //-----
            //kCTParagraphStyleSpecifierLineSpacingAdjustment
            //The space in points added between lines within the paragraph (commonly known as leading).
            if (compareValue(aLineSpacing, 0, TEpsilon.position) > 0) then begin
              SetLength(aSettings, length(aSettings) + 1);
              aLineSpacingAdjustment := aLineSpacing;
              aSettings[high(aSettings)].spec := kCTParagraphStyleSpecifierLineSpacingAdjustment;
              aSettings[high(aSettings)].valueSize := SizeOf(aLineSpacingAdjustment);
              aSettings[high(aSettings)].value := @aLineSpacingAdjustment;
            end;
            //-----
            //kCTParagraphStyleSpecifierMaximumLineHeight
            //The maximum height that any line in the frame will occupy, regardless of the font size or size of any
            //attached graphic. Glyphs and graphics exceeding this height will overlap neighboring lines.
            //A maximum height of 0 implies no line height limit. This value is always nonnegative.
            //Type: CGFloat. Default: 0.0. Application: CTFramesetter.
            //-----
            //kCTParagraphStyleSpecifierMaximumLineSpacing
            //The maximum space in points between lines within the paragraph (commonly known as leading). This value is always nonnegative.
            //-----
            //kCTParagraphStyleSpecifierMinimumLineHeight
            //The minimum height that any line in the frame will occupy, regardless of the font size or size of any attached graphic.
            //This value is always nonnegative. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
            //-----
            //kCTParagraphStyleSpecifierMinimumLineSpacing
            //The minimum space in points between lines within the paragraph (commonly known as leading). This value is always nonnegative.
            //-----
            //kCTParagraphStyleSpecifierParagraphSpacing
            //The space added at the end of the paragraph to separate it from the following paragraph. This value is always nonnegative
            //and is determined by adding the previous paragraph's kCTParagraphStyleSpecifierParagraphSpacing setting and the current paragraph's
            //kCTParagraphStyleSpecifierParagraphSpacingBefore setting. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
            //-----
            //kCTParagraphStyleSpecifierParagraphSpacingBefore
            //The distance between the paragraph's top and the beginning of its text content. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
            //-----
            //kCTParagraphStyleSpecifierTabStops
            //The CTTextTab objects, sorted by location, that define the tab stops for the paragraph style. Type: CFArray of CTTextTabRef.
            //Default: 12 left-aligned tabs, spaced by 28.0 points. Application: CTFramesetter, CTTypesetter.
            //-----
            //kCTParagraphStyleSpecifierTailIndent
            //The distance, in points, from the margin of a frame to the end of lines. If positive, this value is the distance from the leading margin
            //(for example, the left margin in left-to-right text). If 0 or negative, it's the distance from the trailing margin. Type: CGFloat.
            //Default: 0.0. Application: CTFramesetter.
            //-----
            if length(aSettings) > 0 then begin
              aParagraphStyle := CTParagraphStyleCreate(@aSettings[0], Length(aSettings));
              try
                CFAttributedStringSetAttribute(aTextAttr, CFRangeMake(0, CFStringGetLength(aTextString)), kCTParagraphStyleAttributeName, aParagraphStyle);
              finally
                CFRelease(aParagraphStyle);
              end;
            end;
          finally
            CFAttributedStringEndEditing(aTextAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
          end;



          /////////////////////////////
          //Break the text in line(s)//
          /////////////////////////////

          //Create an immutable path of a rectangle.
          aFramePath := CGPathCreateWithRect(ALLowerLeftCGRect(tpointf.create(0,0){aUpperLeftOrigin},
                                                               ARect.Width{aWidth},
                                                               ARect.Height - aFirstLineIndent.y{aHeight},
                                                               ARect.Height - aFirstLineIndent.y{aGridHeight}),
                                                               nil{transform});
          try

            //Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
            //create and fill text frames with the CTFramesetterCreateFrame call.
            aFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(aTextAttr));
            if aFrameSetter = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;  // CTFramesetterCreateWithAttributedString return NULL if unsuccessful.
            try

              //Creates an immutable frame using a framesetter.
              aFrame := CTFramesetterCreateFrame(aframesetter, // framesetter: The framesetter used to create the frame.
                                                 CFRangeMake(0, 0), // stringRange: The range, of the attributed string that was used to create the framesetter,
                                                                    // that is to be typeset in lines fitted into the frame. If the length portion of the range is
                                                                    // set to 0, then the framesetter continues to add lines until it runs out of text or space.
                                                 aframePath, // path: A CGPath object that specifies the shape of the frame. The path may be non-rectangular
                                                             // in versions of OS X v10.7 or later and versions of iOS 4.2 or later.
                                                 nil); // frameAttributes: Additional attributes that control the frame filling process can be specified here,
                                                       // or NULL if there are no such attributes.
              if aFrame = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;  // CTFramesetterCreateFrame return NULL if unsuccessful.
              try

                //init alines / aLinesCount
                alines := CTFrameGetLines(aFrame); // Return a CFArray object containing the CTLine objects that make up the frame, or, if there are no lines in the frame, an array with no elements.
                aLinesCount := CFArrayGetCount(aLines);

                //init result
                result := aLinesCount > 1;

                //update aBreakTextItems - loop on all lines
                for I := 0 to aLinesCount - 1 do begin

                  //break if maxline reach
                  if (aMaxlines > 0) and (aTotalLines >= aMaxlines) then break; // << no need to set the result to true because aLinesCount > 1

                  //break if not wordwrap
                  if (not aWordwrap) and (aTotalLines >= 1) then break; // << no need to set the result to true because aLinesCount > 1

                  //init aline / aMeasuredWidth
                  aline := CFArrayGetValueAtIndex(alines, I);
                  aMeasuredWidth := CTLineGetTypographicBounds(aline, // line: The line whose typographic bounds are calculated.
                                                               @aAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                               @aDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                               nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                     // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                  //unfortunatly the lines that are wrapped are not trimed with the last space
                  //so aMeasuredWidth is inacurate. so i must trim the trailling char
                  if i < aLinesCount - 1 then aMeasuredWidth := aMeasuredWidth - CTLineGetTrailingWhitespaceWidth(aline);

                  //update aCurrLineY
                  if i = 0 then aCurrLineY := aFirstLineIndent.y + aAscent
                  else aCurrLineY := aTotalLinesHeight + aLineSpacing + aAscent;

                  // stop if after maxheight
                  if (compareValue(aCurrLineY + adescent, aMaxHeight, TEpsilon.position) > 0) then begin
                    result := True;
                    aAllTextDrawed := False;
                    break;
                  end;

                  // update aTotalLinesHeight and aTotalLines
                  aTotalLinesHeight := aCurrLineY + aDescent;
                  inc(aTotalLines);

                  //alineindent must be init here (after the break) because alineindent must
                  //correspond to the last item in aBreakTextItems
                  if i > 0 then aLineIndent := 0;

                  // calculate aMaxLineWidth
                  aPrevMaxLineWidth := aMaxLineWidth;
                  aMaxLineWidth := max(aMaxLineWidth, aMeasuredWidth + aLineIndent);

                  // init aStringRange
                  aStringRange := CTLineGetStringRange(aline); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

                  // update aBreakTextItems
                  aBreakTextItem := TalBreakTextItem.Create;
                  try

                    // aBreakTextItem.Line
                    aBreakTextItem.Line := CFRetain(aline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line

                    // aBreakTextItem.text
                    if aStringRange.length > 0 then begin
                      aTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, CFAttributedStringRef(aTextAttr), aStringRange); // return A new attributed string whose string and attributes are copied from the specified range of the supplied attributed string. Returns NULL if there was a problem copying the object.
                      if aTmpTextAttr <> nil then begin
                        try
                          aBreakTextItem.text := CFStringRefToStr(CFAttributedStringGetString(aTmpTextAttr));  // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object.
                        finally
                          cfRelease(aTmpTextAttr);
                        end;
                      end
                      else aBreakTextItem.text := '';
                    end
                    else aBreakTextItem.text := '';

                    // aBreakTextItem.pos / aBreakTextItem.rect
                    case AHTextAlign of
                      TTextAlign.Center: begin
                                           aBreakTextItem.pos := TpointF.create((aMaxWidth - ameasuredWidth - aLineIndent) / 2, aCurrLineY);
                                         end;
                      TTextAlign.Leading: begin
                                            aBreakTextItem.pos := TpointF.create(aLineIndent, aCurrLineY);
                                          end;
                      TTextAlign.Trailing: begin
                                             aBreakTextItem.pos := TpointF.create(aMaxWidth - ameasuredWidth, aCurrLineY);
                                           end;
                    end;
                    aBreakTextItem.rect := Trectf.Create(TPointF.Create(aBreakTextItem.pos.x,
                                                                        aBreakTextItem.pos.Y - aAscent),
                                                         ameasuredWidth,
                                                         aAscent + adescent);

                    // add aBreakTextItem to aBreakTextItems
                    aBreakTextItems.Add(aBreakTextItem);

                  except
                    ALFreeAndNil(aBreakTextItem);
                    raise;
                  end;

                end;

              finally
                CFRelease(aFrame);
              end;

            finally
              CFRelease(aFrameSetter);
            end;

          finally
            CFRelease(aFramePath);
          end;



          //////////////////////////
          //truncate the last line//
          //////////////////////////

          if (aBreakTextItems.Count > aBreakTextItemsStartCount) and  //if the text was at least breaked in one line
             (aStringRange.length > 0) and  // aStringRange was initialised previously
             (aStringRange.location + aStringRange.length < CFAttributedStringGetLength(CFAttributedStringRef(aTextAttr))) then begin // if the last line do not contain all the chars

            //init result
            result := True;
            aAllTextDrawed := False;

            //if aTrimming = TTextTrimming.None or aEllipsisAttr = nil then nothing todo
            if (aTrimming <> TTextTrimming.None) and
               (aEllipsisText <> '') then begin

              //create aEllipsisColor
              if aEllipsisFontColor <> TAlphaColorRec.Null then aAlphaColor := TAlphaColorCGFloat.Create(aEllipsisFontColor)
              else aAlphaColor := TAlphaColorCGFloat.Create(aFontColor);
              aEllipsisColor := CGColorCreate(aColorSpace, @aAlphaColor);
              try

                //create aEllipsisFont
                aEllipsisFont := ALGetCTFontRef(aFontName, aFontSize, aEllipsisFontStyle); // Returns a new font reference for the given name.
                if aEllipsisFont <> nil then begin
                  try

                    //create aEllipsisString
                    aEllipsisString := CFStringCreateWithCharacters(kCFAllocatorDefault, @aEllipsisText[Low(string)], Length(aEllipsisText));
                    if aEllipsisString <> nil then begin
                      try

                        //create aEllipsisAttr
                        aEllipsisAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
                        try

                          CFAttributedStringReplaceString(aEllipsisAttr, CFRangeMake(0, 0), aEllipsisString); // Modifies the string of an attributed string.
                          CFAttributedStringBeginEditing(aEllipsisAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                          try
                            CFAttributedStringSetAttribute(aEllipsisAttr, CFRangeMake(0, CFStringGetLength(aEllipsisString)), kCTFontAttributeName, aEllipsisFont);
                            CFAttributedStringSetAttribute(aEllipsisAttr, CFRangeMake(0, CFStringGetLength(aEllipsisString)), kCTForegroundColorAttributeName, aEllipsisColor);
                          finally
                            CFAttributedStringEndEditing(aEllipsisAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
                          end;

                          //create the aEllipsisLine
                          aEllipsisLine := CTLineCreateWithAttributedString(CFAttributedStringRef(aEllipsisAttr)); // Creates a single immutable line object directly from an attributed string.
                          if aEllipsisLine <> nil then begin                                                       // Return Value: A reference to a CTLine object if the call was successful; otherwise, NULL.
                            try

                              CFAttributedStringBeginEditing(aTextAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                              try
                                //-----
                                SetLength(aSettings, 0);
                                //-----
                                //kCTParagraphStyleSpecifierAlignment
                                //The text alignment. Natural text alignment is realized as left or right alignment, depending on the line sweep direction
                                //of the first script contained in the paragraph. Type: CTTextAlignment. Default: kCTNaturalTextAlignment.
                                //* kCTTextAlignmentCenter
                                //* kCTTextAlignmentJustified
                                //* kCTTextAlignmentLeft
                                //* kCTTextAlignmentNatural
                                //* kCTTextAlignmentRight
                                //-----
                                //kCTParagraphStyleSpecifierBaseWritingDirection
                                //The base writing direction of the lines. Type: CTWritingDirection. Default: kCTWritingDirectionNatural. Application: CTFramesetter, CTTypesetter.
                                //* kCTWritingDirectionNatural: The writing direction is algorithmically determined using the Unicode Bidirectional Algorithm rules P2 and P3.
                                //* kCTWritingDirectionLeftToRight: The writing direction is left to right.
                                //* kCTWritingDirectionRightToLeft: The writing direction is right to left.
                                //-----
                                //kCTParagraphStyleSpecifierCount
                                //The number of style specifiers. The purpose is to simplify validation of style specifiers
                                //-----
                                //kCTParagraphStyleSpecifierDefaultTabInterval
                                //The documentwide default tab interval. Tabs after the last specified by kCTParagraphStyleSpecifierTabStops are placed at
                                //integer multiples of this distance (if positive). Type: CGFloat. Default: 0.0. Application: CTFramesetter, CTTypesetter.
                                //-----
                                //kCTParagraphStyleSpecifierFirstLineHeadIndent
                                //The distance, in points, from the leading margin of a frame to the beginning of the paragraph's first line. This value
                                //is always nonnegative. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
                                //-----
                                //kCTParagraphStyleSpecifierHeadIndent
                                //The distance, in points, from the leading margin of a text container to the beginning of lines other than the first.
                                //This value is always nonnegative. Type: CGFloat Default: 0.0 Application: CTFramesetter
                                //-----
                                //kCTParagraphStyleSpecifierLineBreakMode
                                //The mode that should be used to break lines when laying out the paragraph's text. Type: CTLineBreakMode.
                                //Default: kCTLineBreakByWordWrapping. Application: CTFramesetter
                                //* kCTLineBreakByWordWrapping: Wrapping occurs at word boundaries unless the word itself doesn't fit on a single line.
                                //* kCTLineBreakByCharWrapping: Wrapping occurs before the first character that doesn't fit.
                                //* kCTLineBreakByClipping: Lines are simply not drawn past the edge of the frame.
                                //* kCTLineBreakByTruncatingHead: Each line is displayed so that the end fits in the frame and the missing text is indicated by an ellipsis glyph.
                                //* kCTLineBreakByTruncatingTail: Each line is displayed so that the beginning fits in the container and the missing text is indicated by an ellipsis glyph.
                                //* kCTLineBreakByTruncatingMiddle: Each line is displayed so that the beginning and end fit in the container and the missing text is indicated by an ellipsis glyph in the middle.
                                SetLength(aSettings, length(aSettings) + 1);
                                case aTrimming of
                                  TTextTrimming.None: aLineBreakMode := kCTLineBreakByClipping;
                                  TTextTrimming.Character: aLineBreakMode := kCTLineBreakByCharWrapping;
                                  TTextTrimming.Word: aLineBreakMode := kCTLineBreakByWordWrapping;
                                end;
                                aSettings[high(aSettings)].spec := kCTParagraphStyleSpecifierLineBreakMode;
                                aSettings[high(aSettings)].valueSize := SizeOf(aLineBreakMode);
                                aSettings[high(aSettings)].value := @aLineBreakMode;
                                //-----
                                //kCTParagraphStyleSpecifierLineHeightMultiple
                                //The line height multiple. The natural line height of the receiver is multiplied by this factor (if positive) before
                                //being constrained by minimum and maximum line height. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
                                //-----
                                //kCTParagraphStyleSpecifierLineSpacing
                                //Deprecated. Use kCTParagraphStyleSpecifierMaximumLineSpacing, kCTParagraphStyleSpecifierMinimumLineSpacing, and
                                //kCTParagraphStyleSpecifierLineSpaceAdjustment to control space between lines. The space in points added between
                                //lines within the paragraph (commonly known as leading). This value is always nonnegative. Type: CGFloat.
                                //Default: 0.0. Application: CTFramesetter.
                                //-----
                                //kCTParagraphStyleSpecifierLineSpacingAdjustment
                                //The space in points added between lines within the paragraph (commonly known as leading).
                                //-----
                                //kCTParagraphStyleSpecifierMaximumLineHeight
                                //The maximum height that any line in the frame will occupy, regardless of the font size or size of any
                                //attached graphic. Glyphs and graphics exceeding this height will overlap neighboring lines.
                                //A maximum height of 0 implies no line height limit. This value is always nonnegative.
                                //Type: CGFloat. Default: 0.0. Application: CTFramesetter.
                                //-----
                                //kCTParagraphStyleSpecifierMaximumLineSpacing
                                //The maximum space in points between lines within the paragraph (commonly known as leading). This value is always nonnegative.
                                //-----
                                //kCTParagraphStyleSpecifierMinimumLineHeight
                                //The minimum height that any line in the frame will occupy, regardless of the font size or size of any attached graphic.
                                //This value is always nonnegative. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
                                //-----
                                //kCTParagraphStyleSpecifierMinimumLineSpacing
                                //The minimum space in points between lines within the paragraph (commonly known as leading). This value is always nonnegative.
                                //-----
                                //kCTParagraphStyleSpecifierParagraphSpacing
                                //The space added at the end of the paragraph to separate it from the following paragraph. This value is always nonnegative
                                //and is determined by adding the previous paragraph's kCTParagraphStyleSpecifierParagraphSpacing setting and the current paragraph's
                                //kCTParagraphStyleSpecifierParagraphSpacingBefore setting. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
                                //-----
                                //kCTParagraphStyleSpecifierParagraphSpacingBefore
                                //The distance between the paragraph's top and the beginning of its text content. Type: CGFloat. Default: 0.0. Application: CTFramesetter.
                                //-----
                                //kCTParagraphStyleSpecifierTabStops
                                //The CTTextTab objects, sorted by location, that define the tab stops for the paragraph style. Type: CFArray of CTTextTabRef.
                                //Default: 12 left-aligned tabs, spaced by 28.0 points. Application: CTFramesetter, CTTypesetter.
                                //-----
                                //kCTParagraphStyleSpecifierTailIndent
                                //The distance, in points, from the margin of a frame to the end of lines. If positive, this value is the distance from the leading margin
                                //(for example, the left margin in left-to-right text). If 0 or negative, it's the distance from the trailing margin. Type: CGFloat.
                                //Default: 0.0. Application: CTFramesetter.
                                //-----
                                if length(aSettings) > 0 then begin
                                  aParagraphStyle := CTParagraphStyleCreate(@aSettings[0], Length(aSettings));
                                  try
                                    CFAttributedStringSetAttribute(aTextAttr, CFRangeMake(0, CFStringGetLength(aTextString)), kCTParagraphStyleAttributeName, aParagraphStyle);
                                  finally
                                    CFRelease(aParagraphStyle);
                                  end;
                                end;
                                //-----
                              finally
                                CFAttributedStringEndEditing(aTextAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
                              end;

                              //init aEllipsisWidth
                              aEllipsisWidth := CTLineGetTypographicBounds(aEllipsisLine, // line: The line whose typographic bounds are calculated.
                                                                           @aAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                                           @aDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                                           nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                                 // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                              //Create an immutable path of a rectangle.
                              aFramePath := CGPathCreateWithRect(ALLowerLeftCGRect(tpointf.create(0,0){aUpperLeftOrigin},
                                                                                   aMaxWidth - aEllipsisWidth - aLineIndent{aWidth},
                                                                                   ceil(aAscent+aDescent){aHeight}, // +1 because it's seam when height is exact then it's not work
                                                                                   ceil(aAscent+aDescent){aGridHeight}),
                                                                                   nil{transform});

                              try

                                //Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
                                //create and fill text frames with the CTFramesetterCreateFrame call.
                                aFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(aTextAttr));
                                if aFrameSetter <> nil then begin  // CTFramesetterCreateWithAttributedString return NULL if unsuccessful.
                                  try

                                    //Creates an immutable frame using a framesetter.
                                    aFrame := CTFramesetterCreateFrame(aframesetter, // framesetter: The framesetter used to create the frame.
                                                                       CFRangeMake(aStringRange.location, 0), // stringRange: The range, of the attributed string that was used to create the framesetter,
                                                                                                              // that is to be typeset in lines fitted into the frame. If the length portion of the range is
                                                                                                              // set to 0, then the framesetter continues to add lines until it runs out of text or space.
                                                                       aframePath, // path: A CGPath object that specifies the shape of the frame. The path may be non-rectangular
                                                                                   // in versions of OS X v10.7 or later and versions of iOS 4.2 or later.
                                                                       nil); // frameAttributes: Additional attributes that control the frame filling process can be specified here,
                                                                             // or NULL if there are no such attributes.
                                    if aFrame <> nil then begin // CTFramesetterCreateFrame return NULL if unsuccessful.
                                      try

                                        //init aBreakTextItem
                                        aBreakTextItem := aBreakTextItems[aBreakTextItems.count - 1];
                                        cfRelease(aBreakTextItem.Line);
                                        aBreakTextItem.Line := nil; // << i use this as a flag

                                        //init alines / aLinesCount
                                        alines := CTFrameGetLines(aFrame); // Return a CFArray object containing the CTLine objects that make up the frame, or, if there are no lines in the frame, an array with no elements.
                                        aLinesCount := CFArrayGetCount(aLines);
                                        if aLinesCount > 0 then begin

                                          //init aline / aMeasuredWidth
                                          aline := CFArrayGetValueAtIndex(alines, 0);
                                          aMeasuredWidth := CTLineGetTypographicBounds(aline, // line: The line whose typographic bounds are calculated.
                                                                                       @aAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                                                       @aDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                                                       nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                                            // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                                          //init aStringRange
                                          aStringRange := CTLineGetStringRange(aline); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

                                          //if their is enalf of place for the text + ellipssis
                                          if (aStringRange.length > 0) and
                                             (compareValue(aMeasuredWidth - CTLineGetTrailingWhitespaceWidth(aline), 0, TEpsilon.Position) > 0) and
                                             (compareValue(aMeasuredWidth + aEllipsisWidth, aMaxWidth - aLineIndent, TEpsilon.Position) <= 0) then begin

                                            //calculate aMaxLineWidth
                                            aMaxLineWidth := max(aPrevMaxLineWidth, aMeasuredWidth + aLineIndent + aEllipsisWidth);

                                            //update aBreakTextItems.Line
                                            aBreakTextItem.Line := CFRetain(aline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line

                                            //update aBreakTextItems.text
                                            if aStringRange.length > 0 then begin
                                              aTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, CFAttributedStringRef(aTextAttr), aStringRange); // return A new attributed string whose string and attributes are copied from the specified range of the supplied attributed string. Returns NULL if there was a problem copying the object.
                                              if aTmpTextAttr <> nil then begin
                                                try
                                                  aBreakTextItem.text := CFStringRefToStr(CFAttributedStringGetString(aTmpTextAttr));  // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object.
                                                finally
                                                  cfRelease(aTmpTextAttr);
                                                end;
                                              end
                                              else aBreakTextItem.text := '';
                                            end
                                            else aBreakTextItem.text := '';

                                            //update aBreakTextItems.pos & aBreakTextItems.rect
                                            case AHTextAlign of
                                              TTextAlign.Center: begin
                                                                   aBreakTextItem.pos := TpointF.create((aMaxWidth - ameasuredWidth - aEllipsisWidth - aLineIndent) / 2, aBreakTextItem.pos.y);
                                                                 end;
                                              TTextAlign.Leading: begin
                                                                    aBreakTextItem.pos := TpointF.create(aLineIndent, aBreakTextItem.pos.y);
                                                                  end;
                                              TTextAlign.Trailing: begin
                                                                     aBreakTextItem.pos := TpointF.create(aMaxWidth - ameasuredWidth - aEllipsisWidth, aBreakTextItem.pos.y);
                                                                   end;
                                            end;
                                            aBreakTextItem.rect := Trectf.Create(TPointF.Create(aBreakTextItem.pos.x,
                                                                                                aBreakTextItem.pos.Y - aAscent),
                                                                                 ameasuredWidth,
                                                                                 aAscent + adescent);

                                          end;

                                        end;

                                        //update aBreakTextItem.rect.Width
                                        if aBreakTextItem.Line = nil then aBreakTextItem.rect.Width := 0;

                                        //add the ellipsis line
                                        aEllipsisBreakTextItem := TalBreakTextItem.Create;
                                        try
                                          aEllipsisBreakTextItem.Line := CFRetain(aEllipsisLine); // Retains a Core Foundation object.
                                          aEllipsisBreakTextItem.text := aEllipsisText;
                                          aEllipsisBreakTextItem.isEllipsis := true;
                                          aEllipsisBreakTextItem.pos := TpointF.create(aBreakTextItem.pos.x + aBreakTextItem.rect.Width, aBreakTextItem.pos.y);
                                          aEllipsisBreakTextItem.rect := Trectf.Create(TPointF.Create(aEllipsisBreakTextItem.pos.x,
                                                                                                      aEllipsisBreakTextItem.pos.Y - aAscent), // if aBreakTextItem.Line = nil then aAscent = aAscent of the ellipsis
                                                                                       aEllipsisWidth,
                                                                                       aAscent + adescent); // if aBreakTextItem.Line = nil then aAscent/aDescent = aAscent/aDescent of the ellipsis
                                          aBreakTextItems.Add(aEllipsisBreakTextItem);
                                        except
                                          ALFreeAndNil(aEllipsisBreakTextItem);
                                          raise;
                                        end;

                                        //delete the last line if not enalf of place
                                        if aBreakTextItem.Line = nil then begin
                                          aMaxLineWidth := max(aPrevMaxLineWidth, aLineIndent + aEllipsisWidth);
                                          aBreakTextItems.Delete(aBreakTextItems.count - 2);
                                        end;

                                      finally
                                        CFRelease(aFrame);
                                      end;
                                    end;

                                  finally
                                    CFRelease(aFrameSetter);
                                  end;
                                end;

                              finally
                                CFRelease(aFramePath);
                              end;

                            finally
                              cfRelease(aEllipsisLine);
                            end;
                          end;

                        finally
                          CFRelease(aEllipsisAttr);
                        end;

                      finally
                        CFRelease(aEllipsisString);
                      end;
                    end;

                  finally
                    CFRelease(aEllipsisFont);
                  end;
                end;

              finally
                CGColorRelease(aEllipsisColor);
              end;

            end;

          end
          else if (aBreakTextItems.Count = aBreakTextItemsStartCount) and  // If no line was added
                  (not AText.IsEmpty) then begin // and the text was not empty

            //init result
            result := True;
            aAllTextDrawed := False;

          end

        finally
          CFRelease(aTextAttr);
        end;

      finally
        CFRelease(aTextString);
      end;

    finally
      CFRelease(aFont);
    end;

  finally
    CGColorRelease(aCGColor);
  end;


  //initialise ARect
  if compareValue(aMaxLineWidth, aMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            aOffset := Floor((aRect.Right - aMaxLineWidth - arect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            aRect.Left := aRect.Left + aOffset;
                            aRect.right := aRect.right - aOffset;
                            for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                              aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
                              aBreakTextItems[I].rect.Offset(-aOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             aRect.Right := min(aRect.Right, aRect.Left + aMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              aOffset := Floor(aRect.Right - aMaxLineWidth - arect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              aRect.Left := aRect.Left + aOffset;
                              for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                                aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
                                aBreakTextItems[I].rect.Offset(-aOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(aTotalLinesHeight, aMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            aOffset := (aRect.bottom - aTotalLinesHeight - arect.top) / 2;
                            aRect.top := aRect.top + aOffset;
                            aRect.bottom := aRect.bottom - aOffset;
                          end;
       TTextAlign.Leading: begin
                             aRect.bottom := min(aRect.bottom, aRect.top + aTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              aOffset := aRect.bottom - aTotalLinesHeight - arect.top;
                              aRect.top := aRect.top + aOffset;
                            end;
    end;
  end;

end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function ALBreakText(const aColorSpace: CGColorSpaceRef;
                     const aFontColor: TalphaColor;
                     const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     var ARect: TRectF;
                     const AText: string;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
                     const aBreakTextItems: TALBreakTextItems;
                     const aFirstLineIndent: TpointF;// kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent.x in aTextAttr
                     const aLineSpacing: single = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                     const aEllipsisText: string = '…';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; inline; overload; // // return true if text was breaked in several lines (truncated or not)
var aTotalLines: integer;
    aAllTextDrawed: boolean;
begin
  result := ALBreakText(aColorSpace, // const aColorSpace: CGColorSpaceRef;
                        aFontColor, // const aFontColor: TalphaColor;
                        aFontSize, // const aFontSize: single;
                        aFontStyle, // const aFontStyle: TFontStyles;
                        aFontName, // const aFontName: String;
                        ARect, // var ARect: TRectF;
                        AText, // const AText: string;
                        aWordWrap, // const aWordWrap: Boolean;
                        AHTextAlign, AVTextAlign, // const AHTextAlign, AVTextAlign: TTextAlign;
                        aTrimming, // const aTrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
                        aBreakTextItems, // const aBreakTextItems: TALBreakTextItems;
                        aTotalLines, // var aTotalLines: integer;
                        aAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                        aFirstLineIndent, // const aFirstLineIndent: TpointF;// kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent.x in aTextAttr
                        aLineSpacing, // const aLineSpacing: single = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                        aEllipsisText, // const aEllipsisText: string = '…';
                        aEllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                        aEllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                        aMaxlines); // const aMaxlines: integer = 0): boolean; // // return true if text was breaked in several lines (truncated or not)
end;
{$ENDIF}


{*****************************************}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
Procedure ALGetTextMetrics(const aFontSize: single;
                           const aFontStyle: TFontStyles;
                           const aFontName: String;
                           var aAscent:Single; // << return aAscent in negative (like in android)
                           var aDescent:Single);
var aLayout: TTextLayout;
begin
  aLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    aLayout.BeginUpdate;
    aLayout.Text := '^_'; // << seam that aLayout.TextHeight will be the same for all characters so doesn't matter what we set here
    aLayout.Font.Family := aFontName;
    aLayout.Font.Style := aFontStyle;
    aLayout.Font.Size := aFontSize;
    aLayout.EndUpdate;
    aAscent := 0;  // << unfortunatly TTextlayout don't gave any ascent/descent ( https://quality.embarcadero.com/browse/RSP-16645
                   // << also the canvas.FillText don't ask the base line of the text but the top/left corner so it's better to say
                   // << that ascent = 0 and descent = height of the font
    aDescent := aLayout.TextHeight;
  finally
    aLayout.Free;
  end;
end;
{$ENDIF}

{*****************************************}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
function ALbreakText(const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     const atext: String;
                     const aMaxWidth: Single;
                     var aMeasuredWidth: Single): integer;
var aLayout: TTextLayout;
begin
  // this is true on macos and on windows
  // if aText = 'A' (only 1 char)
  // then aLayout.PositionAtPoint(TpointF.Create(aMeasuredWidth - Tepsilon.Position,0))
  // will return 1 BUT
  // aLayout.PositionAtPoint(TpointF.Create(0,0))
  // will return 0
  // so the conclusion is that aLayout.PositionAtPoint return at the right border
  // the position of the NEXT character (it's good it's what we need)
  // -------
  // also it's seam than aLayout.PositionAtPoint never return an index on a LOW surrogate
  // (remember that aLayout.PositionAtPoint return at the right border the index of the
  // NEXT charactere, so it's index-1 that is matter for us and if index <> LOW surrogate then
  // index-1 <> HIGH surrogate). this because in the delphi source code of PositionAtPoint they do at the end:
  //  if (Result >= 0) and (Result < Text.Length) and Text.Chars[Result].IsLowSurrogate then Inc(Result);
  aLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    aLayout.BeginUpdate;
    aLayout.Font.Family := aFontName;
    aLayout.Font.Style := aFontStyle;
    aLayout.Font.Size := aFontSize;
    aLayout.MaxSize := Tpointf.Create(aMaxWidth, 65535);
    aLayout.Trimming := TTextTrimming.Character;
    aLayout.VerticalAlign := TTextAlign.Leading;
    aLayout.HorizontalAlign := TTextAlign.Leading;
    aLayout.WordWrap := False;
    if (atext <> '') and (atext.Chars[atext.Length - 1].IsLowSurrogate) then aLayout.Text := atext + ' '  // << https://quality.embarcadero.com/browse/RSP-16649
    else aLayout.Text := atext;
    aLayout.EndUpdate;
    aMeasuredWidth := aLayout.TextWidth;
    result := aLayout.PositionAtPoint(TpointF.Create(aMeasuredWidth - Tepsilon.Position,0)); // << on macos this function is buggy and you need to update fmx.canvas.mac (see https://quality.embarcadero.com/browse/RSP-16648 and https://quality.embarcadero.com/browse/RSP-16649)
                                                                                             // << - Tepsilon.Position because if PositionAtPoint = exactly aMeasuredWidth then it's return -1
    result := min(atext.Length, result); // remove the extra space we added because of https://quality.embarcadero.com/browse/RSP-16649
    if result < 0 then result := 0;
  finally
    aLayout.Free;
  end;
end;
{$ENDIF}

{*****************************************}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
constructor TALBreakTextItem.Create;
begin
  inherited;
  Line := '';
  isEllipsis := False;
end;
{$ENDIF}

{*****************************************}
{$IF defined(MSWINDOWS) or defined(_MACOS)}
function ALBreakText(const aFontColor: TalphaColor;
                     const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     var ARect: TRectF;
                     const AText: string;
                     const aWordWrap: Boolean;
                     const AHTextAlign, AVTextAlign: TTextAlign;
                     const aTrimming: TTextTrimming; // TTextTrimming.word not yet supported - TTextTrimming.character will be used instead (if someone need, it's not really hard to implement)
                     const aBreakTextItems: TALBreakTextItems;
                     var aTotalLines: integer;
                     var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                     const aFirstLineIndent: TpointF;// kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent.x in aTextAttr
                     const aLineSpacing: single = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                     const aEllipsisText: string = '…';
                     const aEllipsisFontStyle: TFontStyles = [];
                     const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                     const aMaxlines: integer = 0): boolean; // // return true if text was breaked in several lines (truncated or not)

var aBreakTextItemsStartCount: integer;
    aBreakTextItem: TALBreakTextItem;
    aNumberOfChars: integer;
    aSaveNumberOfChars: integer;
    aSaveNumberOfCharsIsAccurate: Boolean;
    aLine: String;
    aLineIndent: Single;
    aEllipsisLine: String;
    aEllipsisLineLn: single;
    aEllipsisLinePos: TpointF;
    aEllipsisLineRect: TrectF;
    aMaxWidth: single;
    aMaxHeight: single;
    aMaxLineWidth: single;
    aLineHeight: single;
    aTotalLinesHeight: single;
    aChar: Char;
    ATextLn: integer;
    ATextIdx: integer;
    aCurrLineY: single;
    aAscent, aDescent: Single;
    ameasuredWidth: Single;
    aOffset: single;
    aLineEndWithBreakLine: Boolean;
    i, j: integer;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _initEllipsis;
  begin
    if aEllipsisLine = '' then begin
      //-----
      if aEllipsisText = '' then aEllipsisLine := '…'
      else aEllipsisLine := aEllipsisText;
      //-----
      ALbreakText(aFontSize, // const aFontSize: single;
                  aEllipsisFontStyle, // const aFontStyle: TFontStyles;
                  aFontName, // const aFontName: String;
                  aEllipsisLine, // const atext: String;
                  65535, // const aMaxWidth: Single;
                  aEllipsisLineLn); // var aMeasuredWidth: Single)
      //-----
      case AHTextAlign of
        TTextAlign.Center: begin
                             aEllipsisLinePos := TpointF.create((aMaxWidth - aEllipsisLineLn - aLineIndent) / 2, aCurrLineY);
                           end;
        TTextAlign.Leading: begin
                              aEllipsisLinePos := TpointF.create(aLineIndent, aCurrLineY);
                            end;
        TTextAlign.Trailing: begin
                               aEllipsisLinePos := TpointF.create(aMaxWidth - aEllipsisLineLn, aCurrLineY);
                             end;
      end;
      aEllipsisLinerect := Trectf.Create(TPointF.Create(aEllipsisLinePos.x,
                                                        aEllipsisLinePos.Y - (-1*aAscent)),
                                         aEllipsisLineLn,
                                         (-1*aAscent) + aDescent);
      //-----
    end;
  end;

begin

  //init result
  result := false;
  aAllTextDrawed := True;

  //init aBreakTextItemsStartCount
  aBreakTextItemsStartCount := aBreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / aTotalLinesHeight
  if aRect.Width > 65535 then aRect.Width := 65535;
  if aRect.height > 65535 then aRect.height := 65535;
  aMaxWidth := ARect.width;
  aMaxHeight := ARect.Height;
  aMaxLineWidth := 0;
  aTotalLinesHeight := 0;

  //init ATextIdx / ATextLn
  ATextIdx := 0;
  ATextLn := AText.length;

  //init metics / aCurrLineY / aLineHeight
  ALGetTextMetrics(aFontSize,
                   aFontStyle,
                   aFontName,
                   aAscent,
                   aDescent);
  aCurrLineY := aFirstLineIndent.y + (-1*aAscent); // aMetrics.top and aMetrics.ascent are always returned in negative value
  aTotalLines := 0;
  aLineHeight := Adescent + aLineSpacing + (-1*aAscent);

  //init aEllipsisLine
  aEllipsisLine := '';
  aEllipsisLineLn := 0;

  //init aLineIndent
  aLineIndent := aFirstLineIndent.x;

  //if we have at least enalf of height to write the 1rt row
  if comparevalue(aFirstLineIndent.y + Adescent + (-1*aAscent),aMaxHeight,Tepsilon.position) <= 0 then begin

    //loop still their is some chars
    while ATextIdx < ATextLn do begin

      // init aline
      i := aText.indexOf(#13 {c}, ATextIdx{start}); // find if their is some #13 (MSWINDOWS linebreak = #13#10)
      j := aText.indexOf(#10 {c}, ATextIdx{start}); // find if their is some #10 (UNIX linebreak = #10)
      if (i >= 0) and (j >= 0) then I := min(i,j)
      else I := max(I, J);
      if i = ATextIdx then begin
        aLine := '';
        aLineEndWithBreakLine := True;
        result := true;
      end
      else if i > 0 then begin
        aLine := aText.substring(ATextIdx{startIndex}, i - ATextIdx{length}); // skip the $0D/$0A
        aLineEndWithBreakLine := True;
        result := true;
      end
      else begin
        aLine := aText.substring(ATextIdx{startIndex});
        aLineEndWithBreakLine := False;
      end;

      //calculate the number of char in the current line (this work good also if aline is empty)
      aNumberOfChars := ALbreakText(aFontSize,
                                    aFontStyle,
                                    aFontName,
                                    aLine {text},
                                    aMaxWidth - aLineIndent, {amaxWidth}
                                    ameasuredWidth {measuredWidth});

      //init result
      if aNumberOfChars < aLine.length then result := true;

      //if we need to break the text
      if (aNumberOfChars < aLine.length) or // << if aNumberOfChars < aLine.length it's evident we will need to break the text
         (                                                                                                // <<
          (aLineEndWithBreakLine) and                                                                     // <<
          (aTrimming <> TTextTrimming.None) and                                                           // <<
          (                                                                                               // << we need this check to add the ellipsis on the last line
           (not aWordWrap) or                                                                             // << when the last line finish by a break line (#13#10)
           ((compareValue(aCurrLineY + aLineHeight + adescent, aMaxHeight, Tepsilon.position) > 0) or     // <<
            ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1)))                                         // <<
          )                                                                                               // <<
         )                                                                                                // <<
      then begin

        //if not aWordWrap
        if not aWordWrap then begin
          aAllTextDrawed := False; // aNumberOfChars < aLine.length so in anycase we will not draw all the text
          case aTrimming of
            TTextTrimming.None: begin
                                  if aNumberOfChars > 0 then
                                    aLine := aLine.substring(0, aNumberOfChars);
                                end;
            TTextTrimming.Character: begin
                                       //-----
                                       _initEllipsis;
                                       //-----
                                       if (aNumberOfChars < aLine.length) then dec(aNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                    // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                    // we considere that aEllipsisText is only one char
                                       while aNumberOfChars > 0 do begin
                                         aLine := aLine.substring(0, aNumberOfChars);
                                         aNumberOfChars := ALbreakText(aFontSize,
                                                                       aFontStyle,
                                                                       aFontName,
                                                                       aLine {text},
                                                                       aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                                       ameasuredWidth {measuredWidth});
                                         if aNumberOfChars >= aLine.length then break;
                                       end;
                                       //-----
                                     end;
            TTextTrimming.Word: begin
                                  //-----
                                  _initEllipsis;
                                  //-----
                                  aSaveNumberOfChars := aNumberOfChars;
                                  aSaveNumberOfCharsIsAccurate := False;
                                  while aNumberOfChars > 0 do begin
                                    //----
                                    if (aNumberOfChars >= aLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                                      aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                                    end
                                    //----
                                    else if aNumberOfChars >= 2 then begin
                                      aChar := aLine.chars[aNumberOfChars-2];
                                      if (not aChar.IsWhiteSpace) or (Achar.ToUCS4Char = $00A0{No-break Space}) then begin
                                        dec(aNumberOfChars);
                                        continue;
                                      end;
                                      aLine := aLine.substring(0, aNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                                    end
                                    //----
                                    else begin
                                      aNumberOfChars := aSaveNumberOfChars;
                                      if (not aSaveNumberOfCharsIsAccurate) and (aNumberOfChars < aLine.length) then dec(aNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                                                          // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                                                          // we considere that aEllipsisText is only one char
                                      while aNumberOfChars > 0 do begin
                                        aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                                        aNumberOfChars := ALbreakText(aFontSize,
                                                                      aFontStyle,
                                                                      aFontName,
                                                                      aLine {text},
                                                                      aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                                      ameasuredWidth {measuredWidth});
                                        if aNumberOfChars >= aLine.length then break;
                                      end;
                                      break;
                                    end;
                                    //----
                                    aNumberOfChars := ALbreakText(aFontSize,
                                                                  aFontStyle,
                                                                  aFontName,
                                                                  aLine {text},
                                                                  aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                                  ameasuredWidth {measuredWidth});
                                    if aNumberOfChars >= aLine.length then break
                                    else begin
                                      aSaveNumberOfChars:= aNumberOfChars;
                                      aSaveNumberOfCharsIsAccurate := True;
                                    end;
                                    //----
                                  end;
                                end;
          end;
        end

        //if aWordWrap
        else begin

          //We are at the last line and aTrimming <> TTextTrimming.None
          if ((compareValue(aCurrLineY + aLineHeight + adescent, aMaxHeight, Tepsilon.position) > 0) or
              ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1))) and
             (aTrimming <> TTextTrimming.None) then begin

            //-----
            aAllTextDrawed := False; // if we are at the last line then in anycase we will not draw all the text
            _initEllipsis;
            //-----
            aSaveNumberOfChars := aNumberOfChars;
            aSaveNumberOfCharsIsAccurate := False;
            while aNumberOfChars > 0 do begin
              //----
              if (aNumberOfChars >= aLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
              end
              //----
              else if (aTrimming = TTextTrimming.Word) and (aNumberOfChars >= 2) then begin
                aChar := aLine.chars[aNumberOfChars-2];
                if (not aChar.IsWhiteSpace) or (Achar.ToUCS4Char = $00A0{No-break Space}) then begin
                  dec(aNumberOfChars);
                  continue;
                end;
                aLine := aLine.substring(0, aNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
              end
              //----
              else begin
                aNumberOfChars := aSaveNumberOfChars;
                if (not aSaveNumberOfCharsIsAccurate) and (aNumberOfChars < aLine.length) then dec(aNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                                    // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                                    // we considere that aEllipsisText is only one char
                while aNumberOfChars > 0 do begin
                  aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                  aNumberOfChars := ALbreakText(aFontSize,
                                                aFontStyle,
                                                aFontName,
                                                aLine {text},
                                                aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                                ameasuredWidth {measuredWidth});
                  if aNumberOfChars >= aLine.length then break;
                end;
                break;
              end;
              //----
              aNumberOfChars := ALbreakText(aFontSize,
                                            aFontStyle,
                                            aFontName,
                                            aLine {text},
                                            aMaxWidth - aEllipsisLineLn - aLineIndent, {maxWidth}
                                            ameasuredWidth {measuredWidth});
              if aNumberOfChars >= aLine.length then break
              else begin
                aSaveNumberOfChars:= aNumberOfChars;
                aSaveNumberOfCharsIsAccurate := True;
              end;
              //----
            end;

          end

          //We are not at the last line or aTrimming = TTextTrimming.None
          else begin

            //We are at the last line and aTrimming = TTextTrimming.None and more line available
            if (aTrimming = TTextTrimming.None) and
               ((compareValue(aCurrLineY + aLineHeight + adescent, aMaxHeight, Tepsilon.position) > 0) or
                ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1))) then aAllTextDrawed := False;

            //Cut the line
            aSaveNumberOfChars := aNumberOfChars;
            if aNumberOfChars < aLine.length then inc(aNumberOfChars); // in case the space separator is just after aNumberOfChars
            while aNumberOfChars > 0 do begin
              //-----
              if aNumberOfChars >= 2 then begin
                aChar := aLine.chars[aNumberOfChars-1];
                if (not aChar.IsWhiteSpace) or (Achar.ToUCS4Char = $00A0{No-break Space}) then begin
                  dec(aNumberOfChars);
                  continue;
                end;
                aLine := aLine.substring(0, aNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish just before the space
              end
              //-----
              else begin
                aNumberOfChars := aSaveNumberOfChars;
                if compareValue(aLineIndent, 0, TEpsilon.position) > 0 then aNumberOfChars := 0;
                while aNumberOfChars > 0 do begin
                  aLine := aLine.substring(0, aNumberOfChars); // length of aLine is now aNumberOfChars
                  aNumberOfChars := ALbreakText(aFontSize,
                                                aFontStyle,
                                                aFontName,
                                                aLine {text},
                                                aMaxWidth - aLineIndent, {maxWidth}
                                                ameasuredWidth {measuredWidth});
                  if aNumberOfChars >= aLine.length then break;
                end;
                break;
              end;
              //-----
              aNumberOfChars := ALbreakText(aFontSize,
                                            aFontStyle,
                                            aFontName,
                                            aLine {text},
                                            aMaxWidth - aLineIndent, {maxWidth}
                                            ameasuredWidth {measuredWidth});
              if aNumberOfChars >= aLine.length then begin
                inc(aNumberOfChars); // to skip the separator
                break;
              end
              else aSaveNumberOfChars:= aNumberOfChars;
              //-----
            end;

          end;

        end;

      end;

      //init aMaxLineWidth
      aMaxLineWidth := max(aMaxLineWidth, ameasuredWidth + aEllipsisLineLn + aLineIndent);

      // update aTotalLinesHeight
      aTotalLinesHeight := aCurrLineY + aDescent;

      // their is not enalf of place to write at least one char or
      // we are on the #13/#10
      // NOTE: we need to remove the breakline because for exemple we have
      // coco sur le cocotier#13#10
      // then aline will be equal to
      // coco sur le cocotier
      // we draw this line, and then we increate aCurrLineY := aCurrLineY + aLineHeight;
      // so it's mean the #13#10 was already taking in account, so we must delete it
      if aNumberOfChars <= 0 then begin
        if (ATextIdx + 1 < ATextLn) and
           (aText.Chars[ATextIdx] = #13) and
           (aText.Chars[ATextIdx + 1] = #10) then ATextIdx := ATextIdx + 2 // (MSWINDOWS linebreak = #13#10)
        else if (ATextIdx < ATextLn) and
                (aText.Chars[ATextIdx] = #10) then ATextIdx := ATextIdx + 1 // (UNIX linebreak = #10)
        else if (ATextIdx < ATextLn) and
                ((aText.Chars[ATextIdx].IsWhiteSpace) and
                 (aText.Chars[ATextIdx].ToUCS4Char <> $00A0{No-break Space})) then ATextIdx := ATextIdx + 1 // (white space) if we don't have place to write
                                                                                                            // ' blabla' then break it in
                                                                                                            //
                                                                                                            //  |yoyoyoyo|
                                                                                                            //  |blabla  |
                                                                                                            //
                                                                                                            //  and not in
                                                                                                            //
                                                                                                            //  |yoyoyoyo|
                                                                                                            //  | blabla |
                                                                                                            //
        else if compareValue(aLineIndent, 0, Tepsilon.Position) <= 0 then begin // if aLineIndent > 0 then maybe we don't have enalf of place to write one char because of the aLineIndent.
          ATextIdx := ATextIdx + 1; // skip the current char
          if (ATextIdx < ATextLn) and
             (aText.Chars[ATextIdx].IsLowSurrogate) then inc(ATextIdx);
        end;
        aCurrLineY := aCurrLineY + aLineHeight; // go to next line
        inc(aTotalLines);
        aLineIndent := 0;
        if (not aWordWrap) or
           ((aMaxLines > 0) and (aTotalLines >= aMaxLines)) or
           (compareValue(aCurrLineY + aDescent, aMaxHeight, TEpsilon.position) > 0) then break
        else continue;
      end
      else begin
        aTextIdx := ATextIdx + aNumberOfChars;
        if (ATextIdx + 1 < ATextLn) and
           (aText.Chars[ATextIdx] = #13) and
           (aText.Chars[ATextIdx + 1] = #10) then ATextIdx := ATextIdx + 2 // (MSWINDOWS linebreak = #13#10)
        else if (ATextIdx < ATextLn) and
                (aText.Chars[ATextIdx] = #10) then ATextIdx := ATextIdx + 1;
      end;

      //init aBreakedText
      aBreakTextItem := TalBreakTextItem.Create;
      try

        //update aBreakTextItem
        aBreakTextItem.line := aLine;
        case AHTextAlign of
          TTextAlign.Center: begin
                               aBreakTextItem.pos := TpointF.create((aMaxWidth - ameasuredWidth - aEllipsisLineLn - aLineIndent) / 2, aCurrLineY);
                             end;
          TTextAlign.Leading: begin
                                aBreakTextItem.pos := TpointF.create(aLineIndent, aCurrLineY);
                              end;
          TTextAlign.Trailing: begin
                                 aBreakTextItem.pos := TpointF.create(aMaxWidth - ameasuredWidth - aEllipsisLineLn, aCurrLineY);
                               end;
        end;
        aBreakTextItem.rect := Trectf.Create(TPointF.Create(aBreakTextItem.pos.x,
                                                            aBreakTextItem.pos.Y - (-1*aAscent)),
                                             ameasuredWidth,
                                             (-1*aAscent) + aDescent);

        //update aEllipsisLinePos / aEllipsisLinerect
        if aEllipsisLine <> '' then begin
          aEllipsisLinePos := TpointF.Create(aBreakTextItem.pos.x + ameasuredWidth, aCurrLineY);
          aEllipsisLinerect := Trectf.Create(TPointF.Create(aBreakTextItem.pos.x + ameasuredWidth,
                                                            aBreakTextItem.pos.Y - (-1*aAscent)),
                                             aEllipsisLineLn,
                                             (-1*aAscent) + aDescent);
        end;

        // update aBreakTextItems
        aBreakTextItems.Add(aBreakTextItem);

      except
        ALFreeAndNil(aBreakTextItem);
        raise;
      end;

      //update aCurrLineY
      aCurrLineY := aCurrLineY + aLineHeight;
      inc(aTotalLines);

      //update aLineIndent
      aLineIndent := 0;

      // stop if not aWordWrap or after maxheight
      if (not aWordWrap) or
         ((aMaxLines > 0) and (aTotalLines >= aMaxLines)) or
         (compareValue(aCurrLineY + aDescent, aMaxHeight, TEpsilon.position) > 0) then break;

      //add the last empty row if their is one
      if (ATextIdx >= ATextLn) and aLineEndWithBreakLine and (aEllipsisLine = '') then begin

        //init aBreakedText
        aBreakTextItem := TalBreakTextItem.Create;
        try

          //update aBreakTextItem
          aBreakTextItem.line := '';
          case AHTextAlign of
            TTextAlign.Center: begin
                                 aBreakTextItem.pos := TpointF.create((aMaxWidth - aEllipsisLineLn - aLineIndent) / 2, aCurrLineY);
                               end;
            TTextAlign.Leading: begin
                                  aBreakTextItem.pos := TpointF.create(aLineIndent, aCurrLineY);
                                end;
            TTextAlign.Trailing: begin
                                   aBreakTextItem.pos := TpointF.create(aMaxWidth - aEllipsisLineLn, aCurrLineY);
                                 end;
          end;
          aBreakTextItem.rect := Trectf.Create(TPointF.Create(aBreakTextItem.pos.x,
                                                              aBreakTextItem.pos.Y - (-1*aAscent)),
                                               0,
                                               (-1*aAscent) + aDescent);

          // update aBreakTextItems
          aBreakTextItems.Add(aBreakTextItem);

        except
          ALFreeAndNil(aBreakTextItem);
          raise;
        end;

        //update aCurrLineY
        aCurrLineY := aCurrLineY + aLineHeight;
        inc(aTotalLines);

      end;

    end;

  end
  else result := true;

  //add the end ellipsis
  if aEllipsisLine <> '' then begin
    aBreakTextItem := TalBreakTextItem.Create;
    try
      aBreakTextItem.line := aEllipsisLine;
      aBreakTextItem.pos := aEllipsisLinePos;
      aBreakTextItem.rect := aEllipsisLineRect;
      aBreakTextItem.isEllipsis := True;
      aBreakTextItems.Add(aBreakTextItem);
    except
      ALFreeAndNil(aBreakTextItem);
      raise;
    end;
  end;

  //initialise ARect
  if compareValue(aMaxLineWidth, aMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            aOffset := Floor((aRect.Right - aMaxLineWidth - arect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            aRect.Left := aRect.Left + aOffset;
                            aRect.right := aRect.right - aOffset;
                            for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                              aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
                              aBreakTextItems[I].rect.Offset(-aOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             aRect.Right := min(aRect.Right, aRect.Left + aMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              aOffset := Floor(aRect.Right - aMaxLineWidth - arect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              aRect.Left := aRect.Left + aOffset;
                              for I := aBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                                aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - aOffset;
                                aBreakTextItems[I].rect.Offset(-aOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(aTotalLinesHeight, aMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            aOffset := (aRect.bottom - aTotalLinesHeight - arect.top) / 2;
                            aRect.top := aRect.top + aOffset;
                            aRect.bottom := aRect.bottom - aOffset;
                          end;
       TTextAlign.Leading: begin
                             aRect.bottom := min(aRect.bottom, aRect.top + aTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              aOffset := aRect.bottom - aTotalLinesHeight - arect.top;
                              aRect.top := aRect.top + aOffset;
                            end;
    end;
  end;

end;
{$ENDIF}

{*********************************************}
constructor TALDrawMultiLineTextOptions.Create;
begin
  //-----
  FontName := '';
  FontSize := 12;
  FontStyle := [];
  FontColor := $ff000000;
  //-----
  EllipsisText := '…';
  EllipsisFontStyle := [];
  EllipsisFontColor := TAlphaColorRec.Null;
  //-----
  AutoSize := True;
  AutoSizeX := False;
  AutoSizeY := False;
  WordWrap := True;
  MaxLines := 0;
  LineSpacing := 0;
  Trimming := TTextTrimming.Character;
  FirstLineIndent := Tpointf.create(0,0);
  FailIfTextBreaked := false;
  //-----
  HTextAlign := TTextAlign.Leading;
  VTextAlign := TTextAlign.Leading;
  //-----
  Fill := TBrush.Create(TbrushKind.None, $FFE0E0E0);
  Stroke := TStrokeBrush.Create(TbrushKind.None,  $FF000000);
  Sides := AllSides;
  XRadius := 0;
  YRadius := 0;
  Corners := AllCorners;
  Padding := TRectF.Create(0,0,0,0);
  //-----
  TextIsHtml := false;
  //-----
end;

{*********************************************}
destructor TALDrawMultiLineTextOptions.Destroy;
begin
  ALFreeAndNil(Fill);
  ALFreeAndNil(Stroke);
  inherited destroy;
end;

{*********************}
{$ZEROBASEDSTRINGS OFF} // << the guy who introduce zero base string in delphi is just a mix of a Monkey and a Donkey !
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              var aTextBreaked: boolean; // out => true if the text was "breaked" in several lines
                              var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                              var aAscent: single; // out => the Ascent of the last element (in real pixel)
                              var aDescent: Single; // out => the Descent of the last element (in real pixel)
                              var aFirstPos: TpointF; // out => the point of the start of the text
                              var aLastPos: TpointF; // out => the point of the end of the text
                              var aElements: TalTextElements; // out => the list of rect describing all span elements
                              var aEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _getInfosFromTag(const aTag: String; // color="#ffffff" id="xxx"
                             const aSpanIds: TalStringlistU;
                             const aFontColors: Tlist<TalphaColor>);
  var aParamList: TAlStringListU;
      acolorInt: integer;
      S1: String;
  begin

    if aTag = '' then begin
      aSpanIds.Add('');
      if aFontColors.Count > 0 then aFontColors.Add(aFontColors[aFontColors.Count - 1])
       else aFontColors.Add(aOptions.FontColor);
      exit;
    end;

    aParamList := TALStringListU.Create;
    try

      ALExtractHeaderFieldsWithQuoteEscapedU([' ', #9, #13, #10],
                                             [' ', #9, #13, #10],
                                             ['"', ''''],
                                             PChar(aTag),
                                             aParamList,
                                             False,
                                             True{StripQuotes});

      aSpanIds.Add(aParamList.Values['id']);

      S1 := aParamList.Values['color'];
      if S1 <> '' then begin

        if S1[low(S1)] = '#' then begin
          S1[low(S1)] := '$';
          if length(S1) = 7 then insert('ff', S1, 2); // $ffffffff
          if not ALTryStrTointU(S1, acolorInt) then begin
            if aFontColors.Count > 0 then aFontColors.Add(aFontColors[aFontColors.Count - 1])
            else aFontColors.Add(aOptions.FontColor);
          end
          else aFontColors.Add(TalphaColor(acolorInt));
        end
        else begin
          if aFontColors.Count > 0 then aFontColors.Add(aFontColors[aFontColors.Count - 1])
          else aFontColors.Add(aOptions.FontColor);
        end;

      end
      else begin
        if aFontColors.Count > 0 then aFontColors.Add(aFontColors[aFontColors.Count - 1])
        else aFontColors.Add(aOptions.FontColor);
      end;

    finally
      aParamList.Free;
    end;

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _getInfosFromImg(const aTag: String; // src="xxx"
                             var aSrc: String);
  var aParamList: TAlStringListU;
  begin

    if aTag = '' then begin
      aSrc := '';
      exit;
    end;

    aParamList := TALStringListU.Create;
    try

      ALExtractHeaderFieldsWithQuoteEscapedU([' ', #9, #13, #10],
                                             [' ', #9, #13, #10],
                                             ['"', ''''],
                                             PChar(aTag),
                                             aParamList,
                                             False,
                                             True{StripQuotes});

      aSrc := aParamList.Values['src'];

    finally
      aParamList.Free;
    end;

  end;

var {$IF defined(ANDROID)}
    aBitmap: Jbitmap;
    aImg: Jbitmap;
    aPaint: JPaint;
    aTypeface: JTypeface;
    aCanvas: Jcanvas;
    aStyle: integer;
    JStr1, JStr2: JString;
    {$ENDIF}
    {$IF defined(IOS)}
    aBitmapSurface: TbitmapSurface;
    aImg: CGImageRef;
    aColorSpace: CGColorSpaceRef;
    aContext: CGContextRef;
    aStyle: TfontStyles;
    aWhiteSpace: Boolean;
    {$ENDIF}
    {$IF defined(MSWINDOWS) or defined(_MACOS)}
    aStyle: TfontStyles;
    aImg: Tbitmap;
    {$ENDIF}
    aBreakedTextItems: TALBreakTextItems;
    aBreakedTextItem: TALBreakTextItem;
    aBreakedTextItemsCount: integer;
    aCurrText: String;
    aCurrImgSrc: String;
    aTag: String;
    aBold: integer;
    aItalic: Integer;
    aMaxWidth: single;
    aMaxHeight: Single;
    aFontColors: Tlist<TalphaColor>;
    aFontColor: TalphaColor;
    aSpanIds: TalStringlistU;
    aSpanID: String;
    aFirstLineIndent: TpointF;
    aTmpRect: TrectF;
    aTotalLines: integer;
    aTmpTotalLines: integer;
    aTmpTextBreaked: Boolean;
    aTmpAllTextDrawed: Boolean;
    aCurrentLineY: single;
    aOffset: single;
    P1, P2: integer;
    i, j: integer;

begin

  {$IF defined(_USE_TEXTURE)}
  i := TContextManager.DefaultContextClass.MaxTextureSize;
  if i > 0 then begin
    aRect.Width := min(aRect.Width, i);
    aRect.height := min(aRect.height, i);
  end;
  {$ENDIF}

  //init out var
  aTextBreaked := false;
  aAllTextDrawed := True;
  aAscent := 0;
  aDescent := 0;
  aFirstPos := TpointF.Create(0,0);
  aLastPos := TpointF.Create(0,0);
  setlength(aElements, 0);
  aEllipsisRect := TRectF.Create(0,0,0,0);

  {$IF defined(ANDROID)}
  //create aPaint
  aPaint := TJPaint.JavaClass.init;
  aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  aPaint.setSubpixelText(true); // Enabling this flag causes glyph advances to be computed with subpixel accuracy.
  aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
  aPaint.setTextSize(aOptions.FontSize);
  {$ENDIF}

  {$IF defined(IOS)}
  //init the color space
  aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
  if aColorSpace = nil then begin ARect.Width := 0; ARect.Height := 0; exit(nil); end;  // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
  try
  {$ENDIF}

    //init local var
    {$IFDEF IOS}
    aWhiteSpace := False;
    {$ENDIF}
    aFirstLineIndent := aOptions.FirstLineIndent;
    aMaxWidth := 0;
    aMaxHeight := 0;
    aTotalLines := 0;
    aBold := 0;
    aItalic := 0;
    aFontColors := Tlist<TalphaColor>.create;
    aSpanIds := TalStringlistU.create;
    aBreakedTextItems := TalBreakTextItems.Create(true{aOwnsObjects});
    try

      //loop on all the html elements
      P1 := 1;
      while P1 <= length(aText) do begin


        /////////////////////////////////////
        //if the text contain html elements//
        /////////////////////////////////////
        if aOptions.TextIsHtml then begin

          //extract aTag / aCurrText
          if aText[P1] = '<' then begin

            //-----
            aCurrImgSrc := '';
            aCurrText := '';
            P2 := AlposExU('>', aText, P1+1); // blablabla <font color="#ffffff">bliblibli</font> blobloblo
                                              //           ^P1                  ^P2
            if P2 <= 0 then break;
            aTag := AlCopyStrU(aText, P1, P2 - P1 + 1); // <font color="#ffffff">
            P1 := P2 + 1; // blablabla <font color="#ffffff">bliblibli</font> blobloblo
                          //                                 ^P1

            //-----
            if alposU('<b', aTag) = 1 then begin
              _getInfosFromTag(AlcopyStrU(aTag, 4, length(aTag) - 4), aSpanIds, aFontColors);
              inc(aBold);
            end
            else if aTag = '</b>' then begin
              if aSpanIds.count > 0 then aSpanIds.Delete(aSpanIds.Count - 1);
              if aFontColors.count > 0 then aFontColors.Delete(aFontColors.Count - 1);
              dec(aBold);
            end

            //-----
            else if alposU('<img', aTag) = 1 then begin // <img src="xxx">
              _getInfosFromImg(AlcopyStrU(aTag, 6, length(aTag) - 6), aCurrImgSrc);
              aCurrText := '⬛';
            end

            //-----
            else if (alposU('<i', aTag) = 1) then begin
              _getInfosFromTag(AlcopyStrU(aTag, 4, length(aTag) - 4), aSpanIds, aFontColors);
              inc(aItalic)
            end
            else if aTag = '</i>' then begin
              if aSpanIds.count > 0 then aSpanIds.Delete(aSpanIds.Count - 1);
              if aFontColors.count > 0 then aFontColors.Delete(aFontColors.Count - 1);
              dec(aItalic)
            end

            //-----
            else if alposU('<font', aTag) = 1 then begin   // <font color="#ffffff">
              _getInfosFromTag(AlcopyStrU(aTag, 7, length(aTag) - 7), aSpanIds, aFontColors);
            end
            else if aTag = '</font>' then begin
              if aSpanIds.count > 0 then aSpanIds.Delete(aSpanIds.Count - 1);
              if aFontColors.count > 0 then aFontColors.Delete(aFontColors.Count - 1);
            end

            //-----
            else if alposU('<span', aTag) = 1 then begin // <span id="xxx">
              _getInfosFromTag(AlcopyStrU(aTag, 7, length(aTag) - 7), aSpanIds, aFontColors);
            end
            else if aTag = '</span>' then begin
              if aSpanIds.count > 0 then aSpanIds.Delete(aSpanIds.Count - 1);
              if aFontColors.count > 0 then aFontColors.Delete(aFontColors.Count - 1);
            end;

            //-----
            if aCurrImgSrc = '' then continue;

          end
          else begin

            aCurrImgSrc := '';
            P2 := AlposExU('<', aText, P1);  // blablabla <font color="#ffffff">bliblibli</font> blobloblo
                                             //                                 ^P1      ^P2
            if P2 <= 0 then P2 := Maxint;
            aCurrText := AlcopyStrU(aText, P1, P2 - P1);  // blablabla
            aCurrText := ALStringReplaceU(aCurrText, '&gt;', '>', [rfReplaceALL]);
            aCurrText := ALStringReplaceU(aCurrText, '&lt;', '<', [rfReplaceALL]);
            {$IFDEF IOS}
            //because of this http://stackoverflow.com/questions/41334425/ctframesettercreateframe-and-kctparagraphstylespecifierfirstlineheadindent
            if aWhiteSpace then aCurrText := ' ' + aCurrText;
            if (P2 <= length(aText) - 3) and
               (aText[P2 + 1] = 'i') and
               (aText[P2 + 2] = 'm') and
               (aText[P2 + 3] = 'g') then begin
              aWhiteSpace := False;
            end
            else if (P2 > 1) and
                    (P2 <= length(aText)) and
                    (aText[P2 - 1].IsWhiteSpace) then begin
              setlength(aCurrText, length(aCurrText) - 1);
              aWhiteSpace := True;
            end
            else aWhiteSpace := False;
            {$ENDIF}

            P1 := P2; // blablabla <font color="#ffffff">bliblibli</font> blobloblo
                      //                                          ^P1
          end;

        end


        ///////////////////////////
        //if the text is NOT html//
        ///////////////////////////
        else begin
          aCurrText := aText;
          P1 := Maxint;
        end;


        //////////////////////
        //draw the curr text//
        //////////////////////
        if aCurrText <> '' then begin

          //aFontColor
          if aFontColors.Count > 0 then aFontColor := aFontColors[aFontColors.Count - 1]
          else aFontColor := aOptions.FontColor;

          //aSpanID
          if aSpanIds.Count > 0 then aSpanId := aSpanIds[aSpanIds.Count - 1]
          else aSpanId := '';

          //aStyle
          {$IF defined(ANDROID)}
          if ((TFontStyle.fsBold in aOptions.FontStyle) or (aBold > 0)) and
             ((TFontStyle.fsItalic in aOptions.FontStyle) or (aItalic > 0)) then aStyle := TJTypeface.JavaClass.BOLD_ITALIC
          else if ((TFontStyle.fsBold in aOptions.FontStyle) or (aBold > 0)) then aStyle := TJTypeface.JavaClass.BOLD
          else if ((TFontStyle.fsItalic in aOptions.FontStyle) or (aItalic > 0)) then aStyle := TJTypeface.JavaClass.ITALIC
          else aStyle := TJTypeface.JavaClass.NORMAL;
          {$ELSE}
          if ((TFontStyle.fsBold in aOptions.FontStyle) or (aBold > 0)) and
             ((TFontStyle.fsItalic in aOptions.FontStyle) or (aItalic > 0)) then aStyle := [TFontStyle.fsBold, TFontStyle.fsItalic]
          else if ((TFontStyle.fsBold in aOptions.FontStyle) or (aBold > 0)) then aStyle := [TFontStyle.fsBold]
          else if ((TFontStyle.fsItalic in aOptions.FontStyle) or (aItalic > 0)) then aStyle := [TFontStyle.fsItalic]
          else aStyle := [];
          {$ENDIF}

          //loop style we draw all the text or at least the ellipsis
          while True do begin

            //init aPaint
            {$IF defined(ANDROID)}
            aPaint.setColor(integer(aFontColor));
            JStr1 := StringToJString(aOptions.FontName); // << https://quality.embarcadero.com/browse/RSP-14187
            aTypeface := TJTypeface.JavaClass.create(JStr1, aStyle);
            aPaint.setTypeface(aTypeface);
            aTypeface := nil;
            JStr1 := nil;
            {$ENDIF}

            //init aTmpRect / aBreakedTextItemsCount
            aTmpRect := ARect;
            aTmpRect.Width := aTmpRect.Width - aOptions.Padding.Left - aOptions.Padding.right;
            aTmpRect.Height := aTmpRect.Height - aOptions.Padding.top - aOptions.Padding.bottom;
            aBreakedTextItemsCount := aBreakedTextItems.Count;

            //break the text
            {$IF defined(ANDROID)}
            JStr1 := StringtoJString(aCurrText); // << https://quality.embarcadero.com/browse/RSP-14187
            JStr2 := StringtoJString(aOptions.EllipsisText); // << https://quality.embarcadero.com/browse/RSP-14187
            aTmpTextBreaked := ALBreakText(aPaint, // const aPaint: JPaint;
                                           ATmpRect, // var ARect: TRectF;
                                           JStr1, // const AText: JString;
                                           aOptions.WordWrap, //const aWordWrap: Boolean;
                                           TTextAlign.Leading, TTextAlign.Leading, //const AHTextAlign, AVTextAlign: TTextAlign;
                                           aOptions.Trimming, // const aTrimming: TTextTrimming;
                                           aBreakedTextItems, // var aBreakedTexts: Tarray<Tpair<JString, TpointF>>);
                                           aTmpTotalLines, // var aTotalLines: integer
                                           aTmpAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                           aFirstLineIndent, // const aFirstLineIndent: TpointF;
                                           aOptions.LineSpacing, // const aLineSpacing: single = 0;
                                           JStr2, //  const aEllipsisText: JString = nil;
                                           aOptions.FontName, // const aEllipsisFontName: String = '';
                                           aOptions.EllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                                           aOptions.EllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null
                                           aOptions.MaxLines - aTotalLines + AlifThen(aTotalLines > 0, 1, 0)); // const aMaxlines: integer = 0
            JStr1 := nil;
            JStr2 := nil;
            {$ELSEIF defined(IOS)}
            aTmpTextBreaked := ALBreakText(aColorSpace, // const aColorSpace: CGColorSpaceRef;
                                           aFontColor, // const aFontColor: TalphaColor;
                                           aOptions.FontSize, // const aFontSize: single;
                                           aStyle, // const aFontStyle: TFontStyles;
                                           aOptions.FontName, // const aFontName: String;
                                           ATmpRect, // var ARect: TRectF;
                                           aCurrText, // const AText: string;
                                           aOptions.WordWrap, // const aWordWrap: Boolean;
                                           TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                                           aOptions.Trimming, // const aTrimming: TTextTrimming;
                                           aBreakedTextItems, // const aBreakTextItems: TALBreakTextItems;
                                           aTmpTotalLines, // var aTotalLines: integer;
                                           aTmpAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                           aFirstLineIndent, // const aFirstLineIndent: TpointF;
                                           aOptions.LineSpacing, // const aLineSpacing: single = 0;
                                           aOptions.EllipsisText, // const aEllipsisText: string = '…';
                                           aOptions.EllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                                           aOptions.EllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                                           aOptions.MaxLines - aTotalLines + AlifThen(aTotalLines > 0, 1, 0)); // const aMaxlines: integer = 0
            {$ELSE}
            aTmpTextBreaked := ALBreakText(aFontColor, // const aFontColor: TalphaColor;
                                           aOptions.FontSize, // const aFontSize: single;
                                           aStyle, // const aFontStyle: TFontStyles;
                                           aOptions.FontName, // const aFontName: String;
                                           ATmpRect, // var ARect: TRectF;
                                           aCurrText, // const AText: string;
                                           aOptions.WordWrap, // const aWordWrap: Boolean;
                                           TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                                           aOptions.Trimming, // const aTrimming: TTextTrimming;
                                           aBreakedTextItems, // const aBreakTextItems: TALBreakTextItems;
                                           aTmpTotalLines, // var aTotalLines: integer;
                                           aTmpAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                           aFirstLineIndent, // const aFirstLineIndent: TpointF;
                                           aOptions.LineSpacing, // const aLineSpacing: single = 0;
                                           aOptions.EllipsisText, // const aEllipsisText: string = '…';
                                           aOptions.EllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                                           aOptions.EllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                                           aOptions.MaxLines - aTotalLines + AlifThen(aTotalLines > 0, 1, 0)); // const aMaxlines: integer = 0
            {$ENDIF}

            //handle FailIfTextBreaked
            if aTmpTextBreaked and aOptions.FailIfTextBreaked then begin ARect.Width := 0; ARect.Height := 0; exit(nil); end;

            //update the img
            if (aCurrImgSrc <> '') and
               (aBreakedTextItems.Count - aBreakedTextItemsCount = 1) then begin
              aBreakedTextItem := aBreakedTextItems[aBreakedTextItems.count - 1];
              aBreakedTextItem.imgsrc := aCurrImgSrc;
            end;

            //if their was not enalf of place to write the ellipsis
            if (aBreakedTextItems.Count >= 2) and                                                                                          // << more than 2 items
               (aBreakedTextItems.Count - aBreakedTextItemsCount = 1) and                                                                  // << only 1 item (the ellipsis) was added
               (aBreakedTextItems[aBreakedTextItems.count - 1].isEllipsis) and                                                             // << last item is an elipsis
               (comparevalue(aBreakedTextItems[aBreakedTextItems.count - 1].rect.right, ATmpRect.Right, Tepsilon.Position) > 0) then begin // << ellipsis is not inside ATmpRect

              //init aBreakedTextItem
              // -1 = the ellipsis '...' (the last item)
              // their is no item for the text that generate the break line
              // because their is even not enalf of place to write the text himself (all the place was used by the ellipsis)
              // -2 = the previous text
              aBreakedTextItem := aBreakedTextItems[aBreakedTextItems.count - 2];

              //if the ellipsis is not on the same line of the aBreakedTextItem then it's mean
              //we don't have enalf of place on one full row to draw the ellipsis so break the loop
              if compareValue(aBreakedTextItem.rect.Top,  // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                              aBreakedTextItems[aBreakedTextItems.count - 1].rect.top,
                              Tepsilon.Position) <> 0 then break;

              //get the params from aBreakedTextItem
              aFontColor := aBreakedTextItem.fontColor;
              aSpanId := aBreakedTextItem.id;
              aStyle := aBreakedTextItem.fontStyle;
              setlength(aCurrText, 2 * length(aCurrText));                         // << i put some space in the end of the previous text to force
              for I := Low(aCurrText) to High(aCurrText) do aCurrText[i] := ' ';   // << the draw of the ellipsis
              {$IF defined(ANDROID)}
              aCurrText := JStringtoString(aBreakedTextItem.line) + aCurrText + '_';
              {$ELSEIF defined(IOS)}
              aCurrText := aBreakedTextItem.text + aCurrText + '_';
              {$ELSE}
              aCurrText := aBreakedTextItem.line + aCurrText + '_';
              {$ENDIF}
              aFirstLineIndent := TpointF.Create(aBreakedTextItem.rect.left, aBreakedTextItem.rect.Top);

              //clean the aBreakedTextItems
              for I := aBreakedTextItems.Count - 1 downto aBreakedTextItems.Count - 2 do
                aBreakedTextItems.Delete(i);

              //try again
              P1 := maxint;
              continue;

            end;

            //stop the loop
            break;

          end;

          //update aTotalLines
          if aTotalLines = 0 then aTotalLines := aTmpTotalLines
          else aTotalLines := aTotalLines + aTmpTotalLines - 1;

          //update aMaxWidth/aMaxHeight
          aMaxWidth := max(aMaxWidth, ATmpRect.Width);
          aMaxHeight := max(aMaxHeight, ATmpRect.height);

          //update aTextBreaked
          aTextBreaked := aTextBreaked or aTmpTextBreaked;
          aAllTextDrawed := aAllTextDrawed and aTmpAllTextDrawed;

          //update all the aBreakedTextItem
          for I := aBreakedTextItemsCount to aBreakedTextItems.Count - 1 do begin
            aBreakedTextItem := aBreakedTextItems[i];
            //-----
            if (not aBreakedTextItem.isEllipsis) or (aOptions.EllipsisFontColor = TAlphaColorRec.Null) then aBreakedTextItem.fontColor := aFontColor
            else aBreakedTextItem.fontColor := aOptions.EllipsisFontColor;
            //-----
            {$IF defined(ANDROID)}
            if (not aBreakedTextItem.isEllipsis) then aBreakedTextItem.FontStyle := aStyle
            else aBreakedTextItem.FontStyle := ALfontStyleToAndroidStyle(aOptions.EllipsisFontStyle);
            {$ELSE}
            if (not aBreakedTextItem.isEllipsis) then aBreakedTextItem.FontStyle := aStyle
            else aBreakedTextItem.FontStyle := aOptions.EllipsisFontStyle;
            {$ENDIF}
            //-----
            if (not aBreakedTextItem.isEllipsis) then aBreakedTextItem.Id := aSpanID
            else aBreakedTextItem.Id := '';
          end;

          //Update aFirstLineIndent
          if aBreakedTextItems.count > aBreakedTextItemsCount then begin
            aBreakedTextItem := aBreakedTextItems[aBreakedTextItems.count - 1];
            aFirstLineIndent := TpointF.Create(aBreakedTextItem.rect.Right, aBreakedTextItem.rect.Top);
            if aBreakedTextItem.isEllipsis then break;
          end;
          // else break; << we can't break here, it's maybe juste a ' ' we try to write at the end of the line that was deleted by ALBreakText

        end;

      end;

      //initialise ARect
      if aOptions.Autosize or (aOptions.AutosizeX and aOptions.AutosizeY) then begin
        aRect.Width := aMaxWidth + aOptions.Padding.Left + aOptions.Padding.right;
        aRect.Height := aMaxHeight + aOptions.Padding.top + aOptions.Padding.bottom;
      end
      else if aOptions.AutosizeX then aRect.Width := aMaxWidth + aOptions.Padding.Left + aOptions.Padding.right
      else if aOptions.AutosizeY then aRect.Height := aMaxHeight + aOptions.Padding.top + aOptions.Padding.bottom;
      case aOptions.HTextAlign of
        TTextAlign.Center: begin
                             if aBreakedTextItems.Count > 0 then begin
                               aCurrentLineY := aBreakedTextItems[0].rect.top; // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                               J := 0;
                               for I := 1 to aBreakedTextItems.Count do begin
                                 if (I = aBreakedTextItems.Count) or
                                    (compareValue(aCurrentLineY, aBreakedTextItems[I].rect.top, Tepsilon.Position) <> 0) then begin
                                   aOffset := Floor((aRect.width -
                                                       aBreakedTextItems[I-1].rect.Right -
                                                         aOptions.Padding.Left -
                                                           aOptions.Padding.right) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                                   while J < I do begin
                                     aBreakedTextItems[j].pos.X := aBreakedTextItems[j].pos.X + aOptions.Padding.Left + aOffset;
                                     aBreakedTextItems[j].rect.Offset(aOptions.Padding.Left + aOffset, 0);
                                     inc(J);
                                   end;
                                   if (I <> aBreakedTextItems.Count) then aCurrentLineY := aBreakedTextItems[I].rect.top;
                                 end;
                               end;
                             end;
                           end;
        TTextAlign.Leading: begin
                             for I := 0 to aBreakedTextItems.Count - 1 do begin
                               aBreakedTextItems[i].pos.X := aBreakedTextItems[i].pos.X + aOptions.Padding.Left;
                               aBreakedTextItems[i].rect.Offset(aOptions.Padding.Left, 0);
                             end;
                           end;
        TTextAlign.Trailing: begin
                               if aBreakedTextItems.Count > 0 then begin
                                 aCurrentLineY := aBreakedTextItems[0].rect.top; // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                                 J := 0;
                                 for I := 1 to aBreakedTextItems.Count do begin
                                   if (I = aBreakedTextItems.Count) or
                                      (compareValue(aCurrentLineY, aBreakedTextItems[I].rect.top, Tepsilon.Position) <> 0) then begin
                                     aOffset := Floor((aRect.width -
                                                         aBreakedTextItems[I-1].rect.Right -
                                                           aOptions.Padding.Left -
                                                             aOptions.Padding.right)); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                                     while J < I do begin
                                       aBreakedTextItems[j].pos.X := aBreakedTextItems[j].pos.X + aOptions.Padding.Left + aOffset;
                                       aBreakedTextItems[j].rect.Offset(aOptions.Padding.Left + aOffset, 0);
                                       inc(J);
                                     end;
                                     if (I <> aBreakedTextItems.Count) then aCurrentLineY := aBreakedTextItems[I].rect.top;
                                   end;
                                 end;
                               end;
                             end;
      end;
      case aOptions.VTextAlign of
        TTextAlign.Center: begin
                             aOffset := Floor((aRect.height -
                                                 aMaxHeight -
                                                   aOptions.Padding.top -
                                                     aOptions.Padding.bottom) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                             for I := 0 to aBreakedTextItems.Count - 1 do begin
                               aBreakedTextItems[I].pos.y := aBreakedTextItems[i].pos.y + aOptions.Padding.top + aOffset;
                               aBreakedTextItems[I].rect.Offset(0, aOptions.Padding.top + aOffset);
                             end;
                           end;
        TTextAlign.Leading: begin
                             for I := 0 to aBreakedTextItems.Count - 1 do begin
                               aBreakedTextItems[i].pos.Y := aBreakedTextItems[i].pos.Y + aOptions.Padding.top;
                               aBreakedTextItems[i].rect.Offset(0, aOptions.Padding.top);
                             end;
                           end;
        TTextAlign.Trailing: begin
                               aOffset := Floor((aRect.height -
                                                   aMaxHeight -
                                                     aOptions.Padding.top -
                                                       aOptions.Padding.bottom)); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                               for I := 0 to aBreakedTextItems.Count - 1 do begin
                                 aBreakedTextItems[I].pos.y := aBreakedTextItems[i].pos.y + aOptions.Padding.top + aOffset;
                                 aBreakedTextItems[I].rect.Offset(0, aOptions.Padding.top + aOffset);
                               end;
                             end;
      end;
      aRect := ALAlignDimensionToPixelCeil(aRect);

      // init out vars
      if aBreakedTextItems.count > 0 then begin
        aFirstPos := aBreakedTextItems[0].pos;
        aBreakedTextItem := aBreakedTextItems[aBreakedTextItems.count - 1];
        aLastPos := aBreakedTextItem.pos;
        aLastPos.offset(aBreakedTextItem.rect.width, 0);
        aAscent := aLastPos.y - aBreakedTextItem.rect.top;
        aDescent := aBreakedTextItem.rect.bottom - aLastPos.y;
        if aBreakedTextItem.isEllipsis then aEllipsisRect := aBreakedTextItem.rect
        else aEllipsisRect := Trectf.Create(0,0,0,0);
      end;

      //update aElements
      J := 0;
      setlength(aElements, aBreakedTextItems.count);
      for i := 0 to aBreakedTextItems.count - 1 do begin
        if (aBreakedTextItems[i].id <> '') then begin
          aElements[j].Id := aBreakedTextItems[i].id;
          aElements[j].rect := aBreakedTextItems[i].rect;
          inc(j);
        end;
      end;
      setlength(aElements, J);

      {$IF defined(ANDROID)}

      //create the drawing surface
      ALCreateDrawingSurface(aBitmap, // Var aBitmap: Jbitmap;
                             aCanvas, // Var aCanvas: Jcanvas;
                             round(max(1, aRect.Width)), // const w: integer;
                             round(max(1, aRect.Height)));// const h: integer)
      try

        //draw the background
        if (aOptions.Fill.Kind <> TbrushKind.None) or
           (aOptions.stroke.Kind <> TbrushKind.None) then begin
          ALPaintRectangle(aCanvas, // const aBitmap: Jbitmap;
                           aRect, // const aRect: TrectF;
                           aOptions.Fill, // const Fill: TBrush;
                           aOptions.Stroke, // const Stroke: TStrokeBrush;
                           nil, // const Shadow: TALShadow
                           aOptions.Sides, // const Sides: TSides;
                           aOptions.Corners, // const Corners: TCorners;
                           aOptions.XRadius, // const XRadius: Single = 0;
                           aOptions.YRadius); // const YRadius: Single = 0);
        end;

        //draw all texts
        for i := 0 to aBreakedTextItems.count - 1 do begin
          aBreakedTextItem := aBreakedTextItems[i];
          if aBreakedTextItem.imgSrc <> '' then begin
            aMaxWidth := min(aBreakedTextItem.rect.Width, aBreakedTextItem.rect.Height);
            aTmpRect := ALAlignToPixelRound(
                          TrectF.Create(0,0,aMaxWidth,aMaxWidth).
                            CenterAt(aBreakedTextItem.rect));
            aImg := ALLoadFitIntoResourceImageV2(aBreakedTextItem.imgSrc, aTmpRect.Width, aTmpRect.Height);
            if aImg <> nil then begin
              try
                aCanvas.drawBitmap(aImg, aTmpRect.left {left}, aTmpRect.top {top}, apaint {paint});
              finally
                aImg.recycle;
                aImg := nil;
              end;
            end;
          end
          else begin
            aPaint.setColor(integer(aBreakedTextItem.fontColor));
            //-----
            JStr1 := StringToJString(aOptions.FontName); // << https://quality.embarcadero.com/browse/RSP-14187
            aTypeface := TJTypeface.JavaClass.create(JStr1, aBreakedTextItem.fontStyle);
            aPaint.setTypeface(aTypeface);
            aTypeface := nil;
            JStr1 := nil;
            //-----
            aCanvas.drawText(aBreakedTextItem.line{text},
                             aBreakedTextItem.pos.x {x},
                             aBreakedTextItem.pos.y {y},
                             apaint {paint});
          end;
          //-----
        end;

        //free the paint and the canvas
        aPaint := nil;

        //create the result
        result := ALJBitmaptoTexture(aBitmap);

      finally
        ALFreeDrawingSurface(aBitmap, aCanvas);
      end;
      {$ENDIF}

      {$IF defined(IOS)}

      //create the drawing surface
      ALCreateDrawingSurfaceV2(aBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                               aContext, //    Var aContext: CGContextRef;
                               aColorSpace, // const aColorSpace: CGColorSpaceRef;
                               round(max(1, aRect.Width)), // const w: integer;
                               round(max(1, aRect.Height)));// const h: integer)
      try

        //draw the background
        if (aOptions.Fill.Kind <> TbrushKind.None) or
           (aOptions.stroke.Kind <> TbrushKind.None) then begin
          ALPaintRectangle(aContext, // const aContext: CGContextRef;
                           aColorSpace, // const aColorSpace: CGColorSpaceRef;
                           aBitmapSurface.Height, // const aGridHeight: Single;
                           aRect, // const aRect: TrectF;
                           aOptions.Fill, // const Fill: TBrush;
                           aOptions.Stroke, // const Stroke: TStrokeBrush;
                           nil, // const Shadow: TALShadow
                           aOptions.Sides, // const Sides: TSides;
                           aOptions.Corners, // const Corners: TCorners;
                           aOptions.XRadius, // const XRadius: Single = 0;
                           aOptions.YRadius); // const YRadius: Single = 0);
        end;

        //draw all texts
        for i := 0 to aBreakedTextItems.count - 1 do begin
          aBreakedTextItem := aBreakedTextItems[i];
          if aBreakedTextItem.imgSrc <> '' then begin
            aMaxWidth := min(aBreakedTextItem.rect.Width, aBreakedTextItem.rect.Height);
            aTmpRect := ALAlignToPixelRound(
                          TrectF.Create(0,0,aMaxWidth,aMaxWidth).
                            CenterAt(aBreakedTextItem.rect));
            aImg := ALLoadFitIntoResourceImageV2(aBreakedTextItem.imgSrc, aTmpRect.Width, aTmpRect.Height);
            if aImg <> nil then begin
              Try
                CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TPointF.Create(aTmpRect.left, aTmpRect.top),
                                                     aTmpRect.Width,
                                                     aTmpRect.Height,
                                                     aBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                   aImg); // image The image to draw.
              finally
                CGImageRelease(aImg);
              End;
            end;
          end
          else begin
            CGContextSetTextPosition(acontext,
                                     aBreakedTextItem.pos.x {x},
                                     aBitmapSurface.Height - aBreakedTextItem.pos.Y);{y}
            CTLineDraw(aBreakedTextItem.Line, acontext); // Draws a complete line.
          end;
        end;

        //convert the aBitmapSurface to texture
        result := ALBitmapSurfacetoTexture(aBitmapSurface);

      finally
        ALFreeDrawingSurfaceV2(aBitmapSurface, aContext);
      end;

      {$ENDIF}

      {$IF defined(MSWINDOWS) or defined(_MACOS)}

      //create the drawing surface
      ALCreateDrawingSurface(result, // Var aBitmap: Jbitmap;
                             true, // const aClearBitmap: boolean;
                             round(max(1, aRect.Width)), // const w: integer;
                             round(max(1, aRect.Height)));// const h: integer)
      try

        //begin the scene
        if result.Canvas.BeginScene then
        try

          //draw the background
          if (aOptions.Fill.Kind <> TbrushKind.None) or
             (aOptions.stroke.Kind <> TbrushKind.None) then begin
            ALPaintRectangle(result.Canvas, // const aBitmap: Jbitmap;
                             aRect, // const aRect: TrectF;
                             aOptions.Fill, // const Fill: TBrush;
                             aOptions.Stroke, // const Stroke: TStrokeBrush;
                             nil, // const Shadow: TALShadow
                             aOptions.Sides, // const Sides: TSides;
                             aOptions.Corners, // const Corners: TCorners;
                             aOptions.XRadius, // const XRadius: Single = 0;
                             aOptions.YRadius); // const YRadius: Single = 0);
          end;

          //draw all texts
          result.Canvas.Fill.Kind := TbrushKind.Solid;
          result.Canvas.Font.Family := aOptions.FontName;
          result.Canvas.Font.size := aOptions.FontSize;
          for i := 0 to aBreakedTextItems.count - 1 do begin
            aBreakedTextItem := aBreakedTextItems[i];
            if aBreakedTextItem.imgSrc <> '' then begin
              aMaxWidth := min(aBreakedTextItem.rect.Width, aBreakedTextItem.rect.Height) * 1.15;
              aTmpRect := ALAlignToPixelRound(
                            TrectF.Create(0,0,aMaxWidth,aMaxWidth).
                              CenterAt(aBreakedTextItem.rect));
              aImg := ALLoadFitIntoResourceImageV2(aBreakedTextItem.imgSrc, aTmpRect.Width, aTmpRect.Height);
              if aImg <> nil then begin
                try
                  result.Canvas.drawBitmap(aImg, TrectF.Create(0,0,aTmpRect.Width,aTmpRect.Height), aTmpRect{DstRect}, 1{AOpacity}, false{HighSpeed});
                finally
                  ALFreeAndNil(aImg);
                end;
              end;
            end
            else begin
              result.Canvas.Fill.Color := aBreakedTextItem.fontColor;
              result.Canvas.Font.style := aBreakedTextItem.fontStyle;
              //-----
              result.Canvas.FillText(aBreakedTextItem.rect, // const ARect: TRectF;
                                     aBreakedTextItem.line, // const AText: string;
                                     False, // const WordWrap: Boolean;
                                     1, // const AOpacity: Single;
                                     [], // const Flags: TFillTextFlags;
                                     TTextAlign.Leading, TTextAlign.Leading);// const ATextAlign, AVTextAlign: TTextAlign
            end;
          end;

        finally
          result.Canvas.EndScene;
        end;

      except
        ALFreeDrawingSurface(result);
        raise;
      end;
      {$ENDIF}

    finally
      ALFreeAndNil(aBreakedTextItems);
      alfreeandnil(aFontColors);
      alfreeandnil(aSpanIds);
    end;

  {$IF defined(IOS)}
  finally
    CGColorSpaceRelease(aColorSpace);
  end;
  {$ENDIF}

end;
{$IF defined(_ZEROBASEDSTRINGS_ON)}
  {$ZEROBASEDSTRINGS ON}
{$ENDIF}

{************************************************}
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              var aTextBreaked: boolean; // true is the text was "breaked" in several lines
                              var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aAscent: single;
    aDescent: Single;
    aFirstPos: TpointF;
    aLastPos: TpointF;
    aElements: TalTextElements;
    aEllipsisRect: TRectF;
begin
  result := ALDrawMultiLineText(aText,
                                aRect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                                aTextBreaked, // out => true is the text was "breaked" in several lines
                                aAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                aAscent, // var aAscent: single; // out => the Ascent of the last element (in real pixel)
                                aDescent, // var aDescent: Single; // out => the Descent of the last element (in real pixel)
                                aFirstPos, // var aFirstPos: TpointF; // out => the point of the start of the text
                                aLastPos, // var aLastPos: TpointF; // out => the point of the end of the text
                                aElements, // var aElements: TalTextElements; // out => the list of rect describing all span elements
                                aEllipsisRect, // var aEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
                                aOptions);
end;

{************************************************}
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              var aTextBreaked: boolean; // true is the text was "breaked" in several lines
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aAscent: single;
    aDescent: Single;
    aFirstPos: TpointF;
    aLastPos: TpointF;
    aElements: TalTextElements;
    aEllipsisRect: TRectF;
    aAllTextDrawed: boolean;
begin
  result := ALDrawMultiLineText(aText,
                                aRect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                                aTextBreaked, // out => true is the text was "breaked" in several lines
                                aAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                aAscent, // var aAscent: single; // out => the Ascent of the last element (in real pixel)
                                aDescent, // var aDescent: Single; // out => the Descent of the last element (in real pixel)
                                aFirstPos, // var aFirstPos: TpointF; // out => the point of the start of the text
                                aLastPos, // var aLastPos: TpointF; // out => the point of the end of the text
                                aElements, // var aElements: TalTextElements; // out => the list of rect describing all span elements
                                aEllipsisRect, // var aEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
                                aOptions);
end;

{************************************************}
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF _USE_TEXTURE}TTexture{$ELSE}Tbitmap{$ENDIF};
var aAscent: single;
    aDescent: Single;
    aFirstPos: TpointF;
    aLastPos: TpointF;
    aElements: TalTextElements;
    aEllipsisRect: TRectF;
    aTextBreaked: boolean;
    aAllTextDrawed: boolean;
begin
  result := ALDrawMultiLineText(aText,
                                aRect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                                aTextBreaked, // out => true is the text was "breaked" in several lines
                                aAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                aAscent, // var aAscent: single; // out => the Ascent of the last element (in real pixel)
                                aDescent, // var aDescent: Single; // out => the Descent of the last element (in real pixel)
                                aFirstPos, // var aFirstPos: TpointF; // out => the point of the start of the text
                                aLastPos, // var aLastPos: TpointF; // out => the point of the end of the text
                                aElements, // var aElements: TalTextElements; // out => the list of rect describing all span elements
                                aEllipsisRect, // var aEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
                                aOptions);
end;

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
    AString: String;
begin
  Result := TJArrayList.JavaClass.init(Length(AStrings));
  for AString in AStrings do begin
    S := StringToJString(AString);
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
{$ENDIF}

{****************}
{$IF defined(IOS)}
function ALStringsToNSArray(const AStrings: TArray<String>): NSMutableArray;
var S: NSString;
    AString: String;
begin
  Result := TNSMutableArray.Create;
  for AString in AStrings do begin
    S := StrToNSStr(AString);
    Result.addObject((S as ILocalObject).GetObjectID);
  end;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
function ALNSSetToStrings(const ANSSet: NSSet): TArray<String>;
var StringArray: NSArray;
    AString: String;
    I: Integer;
begin
  if ANSSet <> nil then begin
    SetLength(Result, ANSSet.count);
    StringArray := ANSSet.allObjects;
    for I := 0 to StringArray.Count - 1 do begin
      AString := NSStrToStr(TNSString.Wrap(StringArray.objectAtIndex(I)));
      Result[I] := AString;
    end;
  end;
end;
{$ENDIF}

{************************}
{$IF CompilerVersion > 33} // rio
  {$MESSAGE WARN 'Check if FMX.Objects.GetDrawingShapeRectAndSetThickness still have the same implementation and adjust the IFDEF'}
{$ENDIF}
//duplicate of the private delphi function GetDrawingShapeRectAndSetThickness in FMX.Objects
function ALGetDrawingShapeRectAndSetThickness(const Rect: TrectF;
                                              const Fill: TBrush;
                                              const Stroke: TStrokeBrush;
                                              const Fit: Boolean;
                                              var FillShape, DrawShape: Boolean;
                                              var StrokeThicknessRestoreValue: Single): TRectF;
const
  MinRectAreaSize = 0.01;
begin
  FillShape := (Fill <> nil) and (Fill.Kind <> TBrushKind.None);
  DrawShape := (Stroke <> nil) and (Stroke.Kind <> TBrushKind.None);

  if Fit then
    Result := TRectF.Create(0, 0, 1, 1).FitInto(Rect)
  else
    Result := Rect;

  if DrawShape then
  begin
    if Result.Width < Stroke.Thickness then
    begin
      StrokeThicknessRestoreValue := Stroke.Thickness;
      FillShape := False;
      Stroke.Thickness := Min(Result.Width, Result.Height);
      Result.Left := (Result.Right + Result.Left) * 0.5;
      Result.Right := Result.Left + MinRectAreaSize;
    end
    else
      Result.Inflate(-Stroke.Thickness * 0.5, 0);

    if Result.Height < Stroke.Thickness then
    begin
      if StrokeThicknessRestoreValue < 0.0 then
        StrokeThicknessRestoreValue := Stroke.Thickness;
      FillShape := False;
      Stroke.Thickness := Min(Result.Width, Result.Height);
      Result.Top := (Result.Bottom + Result.Top) * 0.5;
      Result.Bottom := Result.Top + MinRectAreaSize;
    end
    else
      Result.Inflate(0, -Stroke.Thickness * 0.5);
  end;
end;

{***********************************************}
procedure ALPaintRectangle({$IF defined(ANDROID)}
                           const aCanvas: Jcanvas;
                           {$ELSEIF defined(IOS)}
                           const aContext: CGContextRef;
                           const aColorSpace: CGColorSpaceRef;
                           const aGridHeight: Single;
                           {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
                           const aCanvas: Tcanvas;
                           {$ENDIF}
                           const dstRect: TrectF;
                           const Fill: TBrush;
                           const Stroke: TStrokeBrush;
                           const Shadow: TALShadow = nil; // if shadow then the Canvas must contain enalf space to draw the shadow (around Shadow.blur on each side of the rectangle)
                           const Sides: TSides = [TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]; // default = AllSides
                           const Corners: TCorners = [TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]; // default = AllCorners
                           const XRadius: Single = 0;
                           const YRadius: Single = 0);

  {$REGION ' _drawRect (ANDROID)'}
  {$IF defined(ANDROID)}
  procedure _drawRect(const aCanvas: Jcanvas;
                      const aPaint: JPaint;
                      const aRect: TrectF;
                      Const aDrawOnlyBorder: Boolean);

  var aJRect: JRectF;
      aPath: JPath;
      aXRadius: single;
      aYradius: Single;
      aWidthMinusCorners: single;
      aHeightMinusCorners: Single;
      aCorners: TCorners;
      aHalfStrokeWidth: Single;
  begin

    // use drawRoundRect
    if ((compareValue(xRadius, 0, TEpsilon.position) > 0) and
        (compareValue(YRadius, 0, TEpsilon.position) > 0)) and
       (corners=[TCorner.TopLeft, TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight]) and
       (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
      //-----
      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});

      aJRect := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
      aCanvas.drawRoundRect(aJRect{rect},
                            xRadius {rx},
                            yRadius {ry},
                            apaint);
      aJRect := nil;

      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.clearShadowLayer;
      //-----
    end

    // use drawRect
    else if ((compareValue(xRadius, 0, TEpsilon.position) = 0) or
             (compareValue(YRadius, 0, TEpsilon.position) = 0) or
             (corners=[])) and
            (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
      //-----
      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});

      aCanvas.drawRect(aRect.left{left},
                       aRect.top{top},
                       aRect.right{right},
                       aRect.bottom{bottom},
                       apaint);

      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.clearShadowLayer;
      //-----
    end

    // use drawPath
    else begin

      aPath := TJPath.Create;
      //----
      aXRadius := xRadius;
      aYradius := yRadius;
      if (aXRadius > aRect.width / 2) then aXRadius := aRect.width / 2;
      if (aYradius > aRect.height / 2) then aYradius := aRect.height / 2;
      //----
      if (compareValue(aXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(aYRadius, 0, TEpsilon.position) > 0) then aCorners := corners
      else aCorners := [];
      //----
      aWidthMinusCorners := (aRect.width - (2 * aXRadius));
      aHeightMinusCorners := (aRect.height - (2 * aYradius));
      //----
      if (Stroke.Kind <> TBrushKind.None) then aHalfStrokeWidth := (Stroke.Thickness) / 2
      else aHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in aCorners) then begin
        aPath.moveTo(aRect.right, aRect.top + aYradius);
        aPath.rQuadTo(0, -aYradius, -aXRadius, -aYradius);
        if not aDrawOnlyBorder then aPath.rlineTo(0, -aHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then aPath.moveTo(aRect.right + aHalfStrokeWidth, aRect.top + aYradius)
        else aPath.moveTo(aRect.right, aRect.top + aYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           aPath.rLineTo(0, -aYradius -aHalfStrokeWidth);
           if aDrawOnlyBorder then aPath.rMoveTo(0, aHalfStrokeWidth);
        end
        else aPath.rMoveTo(0, -aYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then aPath.rLineTo(-aXRadius -aHalfStrokeWidth,0)
          else begin
            aPath.rMoveTo(+aHalfStrokeWidth,0);
            aPath.rLineTo(-aXRadius -aHalfStrokeWidth,0);
          end;
        end
        else aPath.rMoveTo(-aXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then aPath.rLineTo(-awidthMinusCorners, 0)
      else aPath.rMoveTo(-awidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in aCorners) then begin
        if not aDrawOnlyBorder then aPath.rlineTo(0, +aHalfStrokeWidth);
        aPath.rQuadTo(-aXRadius, 0, -aXRadius, aYradius);
        if not aDrawOnlyBorder then aPath.rlineTo(-aHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          aPath.rLineTo(-aXRadius -aHalfStrokeWidth, 0);
          if aDrawOnlyBorder then aPath.rMoveTo(aHalfStrokeWidth, 0);
        end
        else aPath.rMoveTo(-aXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then aPath.rLineTo(0,aYradius +aHalfStrokeWidth)
          else begin
            aPath.rMoveTo(0,-aHalfStrokeWidth);
            aPath.rLineTo(0,+aYradius +aHalfStrokeWidth);
          end;
        end
        else aPath.rMoveTo(0,aYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then aPath.rLineTo(0, aheightMinusCorners)
      else aPath.rMoveTo(0, aheightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in aCorners) then begin
        if not aDrawOnlyBorder then aPath.rlineTo(aHalfStrokeWidth, 0);
        aPath.rQuadTo(0, aYradius, aXRadius, aYradius);
        if not aDrawOnlyBorder then aPath.rlineTo(0, aHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          aPath.rLineTo(0, aYradius +aHalfStrokeWidth);
          if aDrawOnlyBorder then aPath.rMoveTo(0, -aHalfStrokeWidth);
        end
        else aPath.rMoveTo(0, aYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then aPath.rLineTo(aXRadius +aHalfStrokeWidth,0)
          else begin
            aPath.rMoveTo(-aHalfStrokeWidth,0);
            aPath.rLineTo(+aXRadius +aHalfStrokeWidth,0);
          end;
        end
        else aPath.rMoveTo(aXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then aPath.rLineTo(awidthMinusCorners, 0)
      else aPath.rMoveTo(awidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in aCorners) then begin
        if not aDrawOnlyBorder then aPath.rlineTo(0, -aHalfStrokeWidth);
        aPath.rQuadTo(aXRadius, 0, aXRadius, -aYradius);
        if not aDrawOnlyBorder then aPath.rlineTo(aHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          aPath.rLineTo(aXRadius +aHalfStrokeWidth,0);
          if aDrawOnlyBorder then aPath.rMoveTo(-aHalfStrokeWidth, 0);
        end
        else aPath.rMoveTo(aXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then aPath.rLineTo(0, -aYradius -aHalfStrokeWidth)
          else begin
            aPath.rMoveTo(0,+aHalfStrokeWidth);
            aPath.rLineTo(0,-aYradius -aHalfStrokeWidth);
          end;
        end
        else aPath.rMoveTo(0, -aYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then aPath.rLineTo(0, -aheightMinusCorners)
      else aPath.rMoveTo(0, -aheightMinusCorners);

      //-----
      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});

      aCanvas.drawPath(apath,aPaint);
      aPath := nil;

      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.clearShadowLayer;
      //-----

    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' _DrawPath (IOS)'}
  {$IF defined(IOS)}
  procedure _DrawPath(const aRect: TrectF;
                      Const aDrawOnlyBorder: Boolean);

  var aXRadius: single;
      aYradius: Single;
      aWidthMinusCorners: single;
      aHeightMinusCorners: Single;
      aCorners: TCorners;
      aHalfStrokeWidth: Single;
      aCurPoint: TpointF;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(aContext, X, aGridHeight - Y);
      aCurPoint.X := x;
      aCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(aContext,
                                   aCurPoint.X + dx1{cpx},
                                   aGridHeight - (aCurPoint.Y + dy1){cpy},
                                   aCurPoint.X + dx2{x},
                                   aGridHeight - (aCurPoint.Y + dy2){y});
      aCurPoint.X := aCurPoint.X + dx2;
      aCurPoint.Y := aCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(aContext, aCurPoint.X + dx{x}, aGridHeight - (aCurPoint.Y + dy{y}));
      aCurPoint.X := aCurPoint.X + dx;
      aCurPoint.Y := aCurPoint.Y + dy;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rMoveTo(dx: Single; dy: Single);
    begin
      CGContextMoveToPoint(aContext, aCurPoint.X + dx{x}, aGridHeight - (aCurPoint.Y + dy{y}));
      aCurPoint.X := aCurPoint.X + dx;
      aCurPoint.Y := aCurPoint.Y + dy;
    end;

  begin

    // Creates a new empty path in a graphics context.
    CGContextBeginPath(aContext);

    // use drawRect
    if ((compareValue(xRadius, 0, TEpsilon.position) = 0) or
        (compareValue(YRadius, 0, TEpsilon.position) = 0) or
        (corners=[])) and
       (sides=[TSide.Top, TSide.Left, TSide.Bottom, TSide.Right]) then begin
     //-----
     CGContextAddRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                  aRect.Width,
                                                  aRect.Height,
                                                  aGridHeight));
     //-----
    end

    // use drawPath
    else begin

      aXRadius := xRadius;
      aYradius := yRadius;
      if (aXRadius > aRect.width / 2) then aXRadius := aRect.width / 2;
      if (aYradius > aRect.height / 2) then aYradius := aRect.height / 2;
      //----
      if (compareValue(aXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(aYRadius, 0, TEpsilon.position) > 0) then aCorners := corners
      else aCorners := [];
      //----
      aWidthMinusCorners := (aRect.width - (2 * aXRadius));
      aHeightMinusCorners := (aRect.height - (2 * aYradius));
      //----
      if (Stroke.Kind <> TBrushKind.None) then aHalfStrokeWidth := (Stroke.Thickness) / 2
      else aHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in aCorners) then begin
        _moveTo(aRect.right, aRect.top + aYradius);
        _rQuadTo(0, -aYradius, -aXRadius, -aYradius);
        if not aDrawOnlyBorder then _rlineTo(0, -aHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then _moveTo(aRect.right + aHalfStrokeWidth, aRect.top + aYradius)
        else _moveTo(aRect.right, aRect.top + aYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           _rLineTo(0, -aYradius -aHalfStrokeWidth);
           if aDrawOnlyBorder then _rMoveTo(0, aHalfStrokeWidth);
        end
        else _rMoveTo(0, -aYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(-aXRadius -aHalfStrokeWidth,0)
          else begin
            _rMoveTo(+aHalfStrokeWidth,0);
            _rLineTo(-aXRadius -aHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(-aXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then _rLineTo(-awidthMinusCorners, 0)
      else _rMoveTo(-awidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in aCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, +aHalfStrokeWidth);
        _rQuadTo(-aXRadius, 0, -aXRadius, aYradius);
        if not aDrawOnlyBorder then _rlineTo(-aHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          _rLineTo(-aXRadius -aHalfStrokeWidth, 0);
          if aDrawOnlyBorder then _rMoveTo(aHalfStrokeWidth, 0);
        end
        else _rMoveTo(-aXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0,aYradius +aHalfStrokeWidth)
          else begin
            _rMoveTo(0,-aHalfStrokeWidth);
            _rLineTo(0,+aYradius +aHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0,aYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then _rLineTo(0, aheightMinusCorners)
      else _rMoveTo(0, aheightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in aCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(aHalfStrokeWidth, 0);
        _rQuadTo(0, aYradius, aXRadius, aYradius);
        if not aDrawOnlyBorder then _rlineTo(0, aHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          _rLineTo(0, aYradius +aHalfStrokeWidth);
          if aDrawOnlyBorder then _rMoveTo(0, -aHalfStrokeWidth);
        end
        else _rMoveTo(0, aYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(aXRadius +aHalfStrokeWidth,0)
          else begin
            _rMoveTo(-aHalfStrokeWidth,0);
            _rLineTo(+aXRadius +aHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(aXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then _rLineTo(awidthMinusCorners, 0)
      else _rMoveTo(awidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in aCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, -aHalfStrokeWidth);
        _rQuadTo(aXRadius, 0, aXRadius, -aYradius);
        if not aDrawOnlyBorder then _rlineTo(aHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          _rLineTo(aXRadius +aHalfStrokeWidth,0);
          if aDrawOnlyBorder then _rMoveTo(-aHalfStrokeWidth, 0);
        end
        else _rMoveTo(aXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0, -aYradius -aHalfStrokeWidth)
          else begin
            _rMoveTo(0,+aHalfStrokeWidth);
            _rLineTo(0,-aYradius -aHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0, -aYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then _rLineTo(0, -aheightMinusCorners)
      else _rMoveTo(0, -aheightMinusCorners);

    end;

  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' _GetShapeRect (MSWINDOWS / _MACOS)'}
  {$IF defined(MSWINDOWS) or defined(_MACOS)}
  function _GetShapeRect: TRectF;
  begin
    Result := DstRect;
    if Stroke.Kind <> TBrushKind.None then
      InflateRect(Result, -(Stroke.Thickness / 2), -(Stroke.Thickness / 2));
  end;
  {$ENDIF}
  {$ENDREGION}

{$IF defined(IOS)}
const aDefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var aRect: TrectF;
    aTmpBitmap: Jbitmap;
    aShader: JRadialGradient;
    aPaint: JPaint;
    aColors: TJavaArray<Integer>;
    aStops: TJavaArray<Single>;
    aPorterDuffXfermode: jPorterDuffXfermode;
    aBitmapInfo: AndroidBitmapInfo;
    aPixelBuffer: Pointer;
    aBitmapData: TBitmapData;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    i: integer;
{$ELSEIF defined(IOS)}
var aRect: TrectF;
    aAlphaColor: TAlphaColorCGFloat;
    aColor: CGColorRef;
    aCallback: CGFunctionCallbacks;
    aShading: CGShadingRef;
    aFunc: CGFunctionRef;
    aBitmapData: TBitmapData;
    aTMPContext: CGContextRef;
    aImageRef: CGImageRef;
    aImage: UIImage;
{$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
var LShapeRect: TRectF;
    Off: Single;
    StrokeThicknessRestoreValue: Single;
    FillShape, DrawShape: Boolean;
{$ENDIF}

begin

  {$IFDEF ANDROID}

  //create the canvas and the paint
  aPaint := TJPaint.JavaClass.init;
  aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  //init aRect
  if Stroke.Kind <> TBrushKind.None then begin
    aRect := TrectF.Create(dstRect.Left + (Stroke.Thickness / 2),
                           dstRect.Top + (Stroke.Thickness / 2),
                           dstRect.right - (Stroke.Thickness / 2),
                           dstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else aRect := dstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the rectangle
  if Fill.Kind <> TBrushKind.None then begin

    //init aPaint
    aPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full aRect + Stroke.Thickness :( this result&ing in border if the fill is for exemple black and border white

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        aColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
        aStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
        for i := 0 to Fill.Gradient.Points.Count - 1 do begin
          aColors[Fill.Gradient.Points.Count - 1 - i] := integer(Fill.Gradient.Points[i].Color);
          aStops[Fill.Gradient.Points.Count - 1 - i] := 1 - Fill.Gradient.Points[i].Offset;
        end;
        aShader := TJRadialGradient.JavaClass.init(aRect.CenterPoint.x{x}, aRect.CenterPoint.y{y}, aRect.width / 2{radius},  aColors, aStops, TJShader_TileMode.JavaClass.CLAMP{tile});
        aPaint.setShader(aShader);
        _drawRect(aCanvas, aPaint, aRect, false{aDrawOnlyBorder});
        aPaint.setShader(nil);
        aShader := nil;
        ALfreeandNil(aColors);
        ALfreeandNil(aStops);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          //-----
          aTmpBitmap := TJBitmap.JavaClass.createBitmap(fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888);
          //-----
          FillChar(aBitmapInfo, SizeOf(aBitmapInfo), 0);
          if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID, @aBitmapInfo) = 0) and
             (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID, @aPixelBuffer) = 0) then
          try
            if fill.Bitmap.Bitmap.Map(TMapAccess.Read, aBitmapData) then
            try
              System.Move(aBitmapData.Data^, aPixelBuffer^, aBitmapData.Pitch * aBitmapData.Height);
            finally
              fill.Bitmap.Bitmap.Unmap(aBitmapData);
            end;
          finally
            AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID);
          end;
          //-----
          _drawRect(aCanvas, aPaint, aRect, false{aDrawOnlyBorder});
          aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
          aJDestRectf := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
          aJSrcRect := TJRect.JavaClass.init(0, 0, fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height);
          aPaint.setXfermode(aPorterDuffXfermode);
          aCanvas.drawBitmap(aTMPBitmap, aJSrcRect, aJDestRectf, apaint);
          aPaint.setXfermode(nil);
          aPorterDuffXfermode := nil;
          aJSrcRect := nil;
          aJDestRectf := nil;
          //-----
          aTmpBitmap.recycle;
          aTmpBitmap := nil;
          //-----
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      aPaint.setColor(integer(Fill.Color));
      _drawRect(aCanvas, aPaint, aRect, false{aDrawOnlyBorder});
    end;

  end;

  //stroke the rectangle
  if Stroke.Kind <> TBrushKind.None then begin

    //init aPaint
    aPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    aPaint.setStrokeWidth(Stroke.Thickness);

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      aPaint.setColor(integer(Stroke.Color));
      _drawRect(aCanvas, aPaint, aRect, true{aDrawOnlyBorder});
    end;

  end;

  //free the paint and the canvas
  aPaint := nil;

  {$ELSEIF DEFINED(IOS)}

  //set the paint default properties
  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
  //-----
  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.

  //init aRect
  if Stroke.Kind <> TBrushKind.None then begin
    aRect := TrectF.Create(DstRect.Left + (Stroke.Thickness / 2),
                           DstRect.Top + (Stroke.Thickness / 2),
                           DstRect.right - (Stroke.Thickness / 2),
                           DstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else aRect := DstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the rectangle
  if Fill.Kind <> TBrushKind.None then begin

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        CGContextSaveGState(aContext);
        //-----
        aCallback.version := 0;
        aCallback.evaluate := @ALGradientEvaluateCallback;
        aCallback.releaseInfo:= nil;
        aFunc := CGFunctionCreate(fill.Gradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                                  1, // domainDimension - The number of inputs.
                                  @aDefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                                  4, // rangeDimension - The number of outputs.
                                  nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                                  @aCallback); // callbacks - A pointer to a callback function table.
        try
          aShading := CGShadingCreateRadial(aColorSpace, // colorspace
                                            CGPoint.Create(TPointF.Create(aRect.Width / 2, aRect.height / 2)), // start - The center of the starting circle, in the shading's target coordinate space.
                                            aRect.Width / 2, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                                            CGPoint.Create(TPointF.Create(aRect.Width / 2, aRect.Height / 2)), // end - The center of the ending circle, in the shading's target coordinate space.
                                            0, // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                                            aFunc, // function
                                            1, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                                            1); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
          try
            _DrawPath(aRect, false{aDrawOnlyBorder});
            CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                     // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                     // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                     // save the graphics state before you clip and restore the graphics state after you’ve completed
                                     // any clipped drawing.
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              aAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
              aColor := CGColorCreate(aColorSpace, @aAlphaColor);
              try
                CGContextSetShadowWithColor(aContext,
                                            CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                            Shadow.blur, // blur
                                            aColor); // color
              finally
                CGColorRelease(aColor);
              end;
            end;
            //-----
            CGContextDrawShading(aContext, aShading);
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              CGContextSetShadowWithColor(aContext,
                                          CGSizeMake(0, 0), // offset
                                          0, // blur
                                          nil); // color
            end;
            //-----
          finally
            CGShadingRelease(aShading);
          end;
        finally
          CGFunctionRelease(aFunc);
        end;
        //-----
        CGContextRestoreGState(aContext);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          if fill.Bitmap.Bitmap.Map(TMapAccess.Read, aBitmapData) then
          try
            aTMPContext := CGBitmapContextCreate(aBitmapData.Data, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                 aBitmapData.Width, // width: The width, in pixels, of the required bitmap.
                                                 aBitmapData.Height, // height: The height, in pixels, of the required bitmap.
                                                 8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                    //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                    //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                    //                   chapter of Quartz 2D Programming Guide.
                                                 aBitmapData.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                               //              a value of 0 causes the value to be calculated automatically.
                                                 aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                              //             bitmap graphics contexts.
                                                 kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                                   // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                                   // kCGImageAlphaPremultipliedNone =  For example, RGB
                                                 kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                           // kCGBitmapByteOrder32Little = Little-endian
                                                                           // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                           //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                           //             values. The constants for specifying the alpha channel information are declared with the
                                                                           //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                           //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                           //             and CGImageAlphaInfo constants.)
                                                                           //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                           //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                           //             Graphics Contexts chapter of Quartz 2D Programming Guide.
            if aContext <> nil then begin
              try
                aImageRef := CGBitmapContextCreateImage(aTMPContext);
                if aImageRef <> nil then
                try
                  aImage := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(aImageRef));
                  if aImage <> nil then
                  try
                    CGContextSaveGState(aContext);
                    //-----
                    _DrawPath(aRect, false{aDrawOnlyBorder});
                    CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      aAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
                      aColor := CGColorCreate(aColorSpace, @aAlphaColor);
                      try
                        CGContextSetShadowWithColor(aContext,
                                                    CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                                    Shadow.blur, // blur
                                                    aColor); // color
                      finally
                        CGColorRelease(aColor);
                      end;
                    end;
                    //-----
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(aRect.TopLeft,
                                                         aRect.Width,
                                                         aRect.Height,
                                                         aGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      CGContextSetShadowWithColor(aContext,
                                                  CGSizeMake(0, 0), // offset
                                                  0, // blur
                                                  nil); // color
                    end;
                    //-----
                    CGContextRestoreGState(aContext);
                  finally
                    aImage.release;
                  end;
                finally
                  CGImageRelease(aImageRef);
                end;
              finally
                CGContextRelease(aTMPContext);
              end;
            end;
          finally
            fill.Bitmap.Bitmap.Unmap(aBitmapData);
          end;
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      aAlphaColor := TAlphaColorCGFloat.Create(Fill.Color);
      CGContextSetRGBFillColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
      _DrawPath(aRect, false{aDrawOnlyBorder});
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        aAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
        aColor := CGColorCreate(aColorSpace, @aAlphaColor);
        try
          CGContextSetShadowWithColor(aContext,
                                      CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                      Shadow.blur, // blur
                                      aColor); // color
        finally
          CGColorRelease(aColor);
        end;
      end;
      //-----
      CGContextFillPath(aContext);
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        CGContextSetShadowWithColor(aContext,
                                    CGSizeMake(0, 0), // offset
                                    0, // blur
                                    nil); // color
      end;
    end;

  end;

  //stroke the rectangle
  if Stroke.Kind <> TBrushKind.None then begin

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      aAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
      CGContextSetRGBStrokeColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
      CGContextSetLineWidth(aContext, Stroke.Thickness);
      _DrawPath(aRect, True{aDrawOnlyBorder});
      CGContextStrokePath(aContext);
    end;

  end;

  {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}

  StrokeThicknessRestoreValue := Stroke.Thickness;
  try
    LShapeRect := ALGetDrawingShapeRectAndSetThickness(DstRect, Fill, Stroke, False, FillShape, DrawShape, StrokeThicknessRestoreValue);

    if Sides <> AllSides then
    begin
      Off := LShapeRect.Left;
      if not(TSide.Top in Sides) then
        LShapeRect.Top := LShapeRect.Top - Off;
      if not(TSide.Left in Sides) then
        LShapeRect.Left := LShapeRect.Left - Off;
      if not(TSide.Bottom in Sides) then
        LShapeRect.Bottom := LShapeRect.Bottom + Off;
      if not(TSide.Right in Sides) then
        LShapeRect.Right := LShapeRect.Right + Off;
      if FillShape then
        aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Fill, TCornerType.Round{CornerType});
      if DrawShape then
        aCanvas.DrawRectSides(_GetShapeRect, XRadius, YRadius, Corners,  1{AbsoluteOpacity}, Sides, Stroke, TCornerType.Round{CornerType});
    end
    else
    begin
      if FillShape then
        aCanvas.FillRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Fill, TCornerType.Round{CornerType});
      if DrawShape then
        aCanvas.DrawRect(LShapeRect, XRadius, YRadius, Corners, 1{AbsoluteOpacity}, Stroke, TCornerType.Round{CornerType});
    end;
  finally
    if StrokeThicknessRestoreValue <> Stroke.Thickness then
      Stroke.Thickness := StrokeThicknessRestoreValue;
  end;

  {$ENDIF}

end;

{********************************************}
procedure ALPaintCircle({$IF defined(ANDROID)}
                        const aCanvas: Jcanvas;
                        {$ELSEIF defined(IOS)}
                        const aContext: CGContextRef;
                        const aColorSpace: CGColorSpaceRef;
                        const aGridHeight: Single;
                        {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
                        const aCanvas: Tcanvas;
                        {$ENDIF}
                        const dstRect: TrectF;
                        const Fill: TBrush;
                        const Stroke: TStrokeBrush;
                        const Shadow: TALShadow = nil); // if shadow then the Canvas must contain enalf space to draw the shadow (around Shadow.blur on each side of the rectangle)

{$IF defined(IOS)}
const aDefaultInputRange: array[0..1] of CGFloat = (0, 1);
{$ENDIF}

{$IF defined(ANDROID)}
var aTmpBitmap: Jbitmap;
    aShader: JRadialGradient;
    aPaint: JPaint;
    aRect: TRectf;
    aColors: TJavaArray<Integer>;
    aStops: TJavaArray<Single>;
    aPorterDuffXfermode: jPorterDuffXfermode;
    aBitmapInfo: AndroidBitmapInfo;
    aPixelBuffer: Pointer;
    aBitmapData: TBitmapData;
    aJDestRectf: JrectF;
    aJSrcRect: Jrect;
    i: integer;
{$ELSEIF defined(IOS)}
var aAlphaColor: TAlphaColorCGFloat;
    aColor: CGColorRef;
    aCallback: CGFunctionCallbacks;
    aShading: CGShadingRef;
    aFunc: CGFunctionRef;
    aRect: TRectf;
    aBitmapData: TBitmapData;
    aTMPContext: CGContextRef;
    aImageRef: CGImageRef;
    aImage: UIImage;
{$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
var LShapeRect: TRectF;
    StrokeThicknessRestoreValue: Single;
    FillShape, DrawShape: Boolean;
{$ENDIF}

begin

  {$IFDEF ANDROID}

  //create the canvas and the paint
  aPaint := TJPaint.JavaClass.init;
  aPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  aPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  apaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  //init aRect
  if Stroke.Kind <> TBrushKind.None then begin
    aRect := TrectF.Create(dstRect.Left + (Stroke.Thickness / 2),
                           dstRect.Top + (Stroke.Thickness / 2),
                           dstRect.right - (Stroke.Thickness / 2),
                           dstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else aRect := dstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the circle
  if Fill.Kind <> TBrushKind.None then begin

    //init aPaint
    aPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full aRect + Stroke.Thickness :( this result&ing in border if the fill is for exemple black and border white

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        aColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
        aStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
        for i := 0 to Fill.Gradient.Points.Count - 1 do begin
          aColors[Fill.Gradient.Points.Count - 1 - i] := integer(Fill.Gradient.Points[i].Color);
          aStops[Fill.Gradient.Points.Count - 1 - i] := 1 - Fill.Gradient.Points[i].Offset;
        end;
        aShader := TJRadialGradient.JavaClass.init(aRect.CenterPoint.x{x}, aRect.CenterPoint.y{y}, aRect.width / 2{radius},  aColors, aStops, TJShader_TileMode.JavaClass.CLAMP{tile});
        aPaint.setShader(aShader);
        if (Shadow <> nil) and
           (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});
        aCanvas.drawCircle(aRect.CenterPoint.x{cx}, aRect.CenterPoint.y{cy}, aRect.width / 2{radius}, apaint);
        if (Shadow <> nil) and
           (Shadow.enabled) then aPaint.clearShadowLayer;
        aPaint.setShader(nil);
        aShader := nil;
        alfreeandNil(aColors);
        alfreeandNil(aStops);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          //-----
          aTmpBitmap := TJBitmap.JavaClass.createBitmap(fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888);
          //-----
          FillChar(aBitmapInfo, SizeOf(aBitmapInfo), 0);
          if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID, @aBitmapInfo) = 0) and
             (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID, @aPixelBuffer) = 0) then
          try
            if fill.Bitmap.Bitmap.Map(TMapAccess.Read, aBitmapData) then
            try
              System.Move(aBitmapData.Data^, aPixelBuffer^, aBitmapData.Pitch * aBitmapData.Height);
            finally
              fill.Bitmap.Bitmap.Unmap(aBitmapData);
            end;
          finally
            AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (aTmpBitmap as ILocalObject).GetObjectID);
          end;
          //-----
          aCanvas.drawCircle(aRect.CenterPoint.x{cx}, aRect.CenterPoint.y{cy}, aRect.width / 2{radius}, apaint);
          aPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
          aJDestRectf := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
          aJSrcRect := TJRect.JavaClass.init(0, 0, fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height);
          aPaint.setXfermode(aPorterDuffXfermode);
          if (Shadow <> nil) and
             (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});
          aCanvas.drawBitmap(aTMPBitmap, aJSrcRect, aJDestRectf, apaint);
          if (Shadow <> nil) and
             (Shadow.enabled) then aPaint.clearShadowLayer;
          aPaint.setXfermode(nil);
          aPorterDuffXfermode := nil;
          aJSrcRect := nil;
          aJDestRectf := nil;
          //-----
          aTmpBitmap.recycle;
          aTmpBitmap := nil;
          //-----
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      aPaint.setColor(integer(Fill.Color));
      if (Shadow <> nil) and
         (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});
      aCanvas.drawCircle(aRect.CenterPoint.x{cx}, aRect.CenterPoint.y{cy}, aRect.width / 2{radius}, apaint);
      if (Shadow <> nil) and
         (Shadow.enabled) then aPaint.clearShadowLayer;
    end;

  end;

  //stroke the circle
  if Stroke.Kind <> TBrushKind.None then begin

    //init aPaint
    aPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    aPaint.setStrokeWidth(Stroke.Thickness);

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      aPaint.setColor(integer(Stroke.Color));
      aCanvas.drawCircle(aRect.CenterPoint.x{cx}, aRect.CenterPoint.y{cy}, aRect.width / 2{radius}, apaint);
    end;

  end;

  //free the paint and the canvas
  aPaint := nil;

  {$ELSEIF DEFINED(IOS)}

  //set the paint default properties
  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
  //-----
  CGContextSetShouldAntialias(aContext, 1); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.

  //init aRect
  if Stroke.Kind <> TBrushKind.None then begin
    aRect := TrectF.Create(DstRect.Left + (Stroke.Thickness / 2),
                           DstRect.Top + (Stroke.Thickness / 2),
                           DstRect.right - (Stroke.Thickness / 2),
                           DstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else aRect := DstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the circle
  if Fill.Kind <> TBrushKind.None then begin

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        CGContextSaveGState(aContext);
        //-----
        aCallback.version := 0;
        aCallback.evaluate := @ALGradientEvaluateCallback;
        aCallback.releaseInfo:= nil;
        aFunc := CGFunctionCreate(fill.Gradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                                  1, // domainDimension - The number of inputs.
                                  @aDefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                                  4, // rangeDimension - The number of outputs.
                                  nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                                  @aCallback); // callbacks - A pointer to a callback function table.
        try
          aShading := CGShadingCreateRadial(aColorSpace, // colorspace
                                            CGPoint.Create(TPointF.Create(aRect.Width / 2, aRect.height / 2)), // start - The center of the starting circle, in the shading's target coordinate space.
                                            aRect.Width / 2, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                                            CGPoint.Create(TPointF.Create(aRect.Width / 2, aRect.Height / 2)), // end - The center of the ending circle, in the shading's target coordinate space.
                                            0, // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                                            aFunc, // function
                                            1, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                                            1); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
          try
            CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
            CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                                  aRect.Width,
                                                                  aRect.Height,
                                                                  aGridHeight));
            CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
            CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                     // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                     // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                     // save the graphics state before you clip and restore the graphics state after you’ve completed
                                     // any clipped drawing.
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              aAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
              aColor := CGColorCreate(aColorSpace, @aAlphaColor);
              try
                CGContextSetShadowWithColor(aContext,
                                            CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                            Shadow.blur, // blur
                                            aColor); // color
              finally
                CGColorRelease(aColor);
              end;
            end;
            //-----
            CGContextDrawShading(aContext, aShading);
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              CGContextSetShadowWithColor(aContext,
                                          CGSizeMake(0, 0), // offset
                                          0, // blur
                                          nil); // color
            end;
            //-----
          finally
            CGShadingRelease(aShading);
          end;
        finally
          CGFunctionRelease(aFunc);
        end;
        //-----
        CGContextRestoreGState(aContext);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          if fill.Bitmap.Bitmap.Map(TMapAccess.Read, aBitmapData) then
          try
            aTMPContext := CGBitmapContextCreate(aBitmapData.Data, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                 aBitmapData.Width, // width: The width, in pixels, of the required bitmap.
                                                 aBitmapData.Height, // height: The height, in pixels, of the required bitmap.
                                                 8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                    //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                    //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                    //                   chapter of Quartz 2D Programming Guide.
                                                 aBitmapData.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                                               //              a value of 0 causes the value to be calculated automatically.
                                                 aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                              //             bitmap graphics contexts.
                                                 kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                                   // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                                   // kCGImageAlphaPremultipliedNone =  For example, RGB
                                                 kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                           // kCGBitmapByteOrder32Little = Little-endian
                                                                           // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                           //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                           //             values. The constants for specifying the alpha channel information are declared with the
                                                                           //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                           //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                           //             and CGImageAlphaInfo constants.)
                                                                           //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                           //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                           //             Graphics Contexts chapter of Quartz 2D Programming Guide.
            if aContext <> nil then begin
              try
                aImageRef := CGBitmapContextCreateImage(aTMPContext);
                if aImageRef <> nil then
                try
                  aImage := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(aImageRef));
                  if aImage <> nil then
                  try
                    CGContextSaveGState(aContext);
                    //-----
                    CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
                    CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                                          aRect.Width,
                                                                          aRect.Height,
                                                                          aGridHeight)); // Adds an ellipse that fits inside the specified rectangle.
                    CGContextClosePath(aContext); // Closes and terminates the current path’s subpath.
                    CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      aAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
                      aColor := CGColorCreate(aColorSpace, @aAlphaColor);
                      try
                        CGContextSetShadowWithColor(aContext,
                                                    CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                                    Shadow.blur, // blur
                                                    aColor); // color
                      finally
                        CGColorRelease(aColor);
                      end;
                    end;
                    //-----
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(aRect.TopLeft,
                                                         aRect.Width,
                                                         aRect.Height,
                                                         aGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       aImage.CGImage); // image The image to draw.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      CGContextSetShadowWithColor(aContext,
                                                  CGSizeMake(0, 0), // offset
                                                  0, // blur
                                                  nil); // color
                    end;
                    //-----
                    CGContextRestoreGState(aContext);
                  finally
                    aImage.release;
                  end;
                finally
                  CGImageRelease(aImageRef);
                end;
              finally
                CGContextRelease(aTMPContext);
              end;
            end;
          finally
            fill.Bitmap.Bitmap.Unmap(aBitmapData);
          end;
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      aAlphaColor := TAlphaColorCGFloat.Create(Fill.Color);
      CGContextSetRGBFillColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        aAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
        aColor := CGColorCreate(aColorSpace, @aAlphaColor);
        try
          CGContextSetShadowWithColor(aContext,
                                      CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                      Shadow.blur, // blur
                                      aColor); // color
        finally
          CGColorRelease(aColor);
        end;
      end;
      //-----
      CGContextFillEllipseInRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                             aRect.Width,
                                                             aRect.Height,
                                                             aGridHeight));
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        CGContextSetShadowWithColor(aContext,
                                    CGSizeMake(0, 0), // offset
                                    0, // blur
                                    nil); // color
      end;
      //-----
    end;

  end;

  //stroke the circle
  if Stroke.Kind <> TBrushKind.None then begin

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      aAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
      CGContextSetRGBStrokeColor(aContext, aAlphaColor.R, aAlphaColor.G, aAlphaColor.B, aAlphaColor.A);
      CGContextSetLineWidth(aContext, Stroke.Thickness);
      CGContextStrokeEllipseInRect(aContext, ALLowerLeftCGRect(aRect.TopLeft,
                                                               aRect.Width,
                                                               aRect.Height,
                                                               aGridHeight));
    end;

  end;

  {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}

  StrokeThicknessRestoreValue := Stroke.Thickness;
  try
    LShapeRect := ALGetDrawingShapeRectAndSetThickness(DstRect, Fill, Stroke, True, FillShape, DrawShape, StrokeThicknessRestoreValue);
    if FillShape then
      aCanvas.FillEllipse(LShapeRect, 1{AbsoluteOpacity}, Fill);
    if DrawShape then
      aCanvas.DrawEllipse(LShapeRect, 1{AbsoluteOpacity}, Stroke);
  finally
    if StrokeThicknessRestoreValue <> Stroke.Thickness then
      Stroke.Thickness := StrokeThicknessRestoreValue;
  end;

  {$ENDIF}

end;

{*****************************************************}
Procedure ALCreateDrawingSurface({$IF defined(ANDROID)}
                                 Var aBitmap: Jbitmap;
                                 var aCanvas: Jcanvas;
                                 {$ELSEIF defined(IOS)}
                                 var aBitmapSurface: TbitmapSurface;
                                 Var aContext: CGContextRef;
                                 Var aColorSpace: CGColorSpaceRef;
                                 {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
                                 Var aBitmap: Tbitmap;
                                 const aClearBitmap: boolean;
                                 {$ENDIF}
                                 const w: integer;
                                 const h: integer);
begin

  {$IFDEF ANDROID}

  //create the main bitmap on with we will draw
  aBitmap := TJBitmap.JavaClass.createBitmap(W, H, TJBitmap_Config.JavaClass.ARGB_8888);

  //create the canvas and the paint
  aCanvas := TJCanvas.JavaClass.init(aBitmap);

  {$ELSEIF DEFINED(IOS)}

  //create the bitmapSurface
  aBitmapSurface := TbitmapSurface.Create;
  try

    //init aBitmapSurface
    aBitmapSurface.SetSize(W,H);

    //init the color space
    aColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
    if aColorSpace = nil then Raise Exception.Create('Call to CGColorSpaceCreateDeviceRGB failed'); // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
    try

      //create the context
      aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                             //       memory block should be at least (bytesPerRow*height) bytes.
                                                             //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                             //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                        aBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                                        aBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                                        8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                           //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                           //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                           //                   chapter of Quartz 2D Programming Guide.
                                        aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                              //              a value of 0 causes the value to be calculated automatically.
                                        aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                     //             bitmap graphics contexts.
                                        kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                          // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                          // kCGImageAlphaPremultipliedNone =  For example, RGB
                                        kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                  // kCGBitmapByteOrder32Little = Little-endian
                                                                  // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                  //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                  //             values. The constants for specifying the alpha channel information are declared with the
                                                                  //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                  //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                  //             and CGImageAlphaInfo constants.)
                                                                  //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                  //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                  //             Graphics Contexts chapter of Quartz 2D Programming Guide.
      if aContext = nil then Raise Exception.Create('Call to CGBitmapContextCreate failed');
      try

        //set the paint default properties
        CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
        //-----
        CGContextSetShouldAntialias(aContext, 1); // default: ON
                                                  // Sets anti-aliasing on or off for a graphics context.
        CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
        //-----
        //CGContextSetShouldSmoothFonts(aContext, 1); // There are cases, such as rendering to a bitmap, when font smoothing is not appropriate and should be disabled.
                                                      // Note that some contexts (such as PostScript contexts) do not support font smoothing.
                                                      // -----
                                                      // Enables or disables font smoothing in a graphics context.
                                                      // When drawing text on a context attached to a color LCD display, Quartz takes advantage of the nature of
                                                      // LCD monitors to improve the legibility of text. This technique is called Font Smoothing. The pixels
                                                      // of an LCD monitor are made up of red, green, and blue sub-pixels. If you take these sub-pixels into
                                                      // account the screen appears to have three times the resolution commonly attributed to it, at least in
                                                      // one dimension. Font smoothing takes advantage of this increased resolution to improve the rendering of
                                                      // text. Quartz turns different sub-pixels off and on by changing the color of a pixels along the edge of
                                                      // letter shapes. Because your eye expects to see a hard line at the edge of the glyphs, the computer tricks
                                                      // it into ignoring the color in favor of perceiving a smooth edge. One disadvantage of font smoothing is
                                                      // that it relies on the fixed ordering of the sub-pixels of an LCD display. That makes the technique of
                                                      // limited use on other types of monitors. Font smoothing is also of limited use on offscreen bitmaps.
        //CGContextSetAllowsFontSmoothing(aContext, 1); // Sets whether or not to allow font smoothing for a graphics context.
        //-----
        CGContextSetShouldSubpixelPositionFonts(aContext, 1); // default: ON
                                                              // When enabled, the graphics context may position glyphs on nonintegral pixel boundaries. When disabled,
                                                              // the position of glyphs are always forced to integral pixel boundaries.
                                                              // -----
                                                              // Enables or disables subpixel positioning in a graphics context.
                                                              // Subpixel positioning concerns whether or not the glyphs in a line of
                                                              // text will be aligned to pixel boundaries or not. If subpixel positioning is
                                                              // off then when glyphs are drawn their positions might be shifted slightly to
                                                              // take pixel boundaries in account. This can improve the visual definition of
                                                              // the glyphs (making them slightly less "blurry") at the expense of honoring
                                                              // the font metrics.
        CGContextSetAllowsFontSubpixelPositioning(aContext, 1); // Sets whether or not to allow subpixel positioning for a graphics context
        //-----
        CGContextSetShouldSubpixelQuantizeFonts(aContext, 1); // default: ON
                                                              // Enables or disables subpixel quantization in a graphics context.
                                                              // -----
                                                              // Subpixel quantization is only enabled if subpixel positioning is enabled. Subpixel
                                                              // quantization improves the rendering of fonts whose glyphs are at subpixel positions
                                                              // by more closely examining how the shapes that make up the glyphs cover an individual pixel.
                                                              // This improvement, requires additional processing so changing this value can affect text
                                                              // drawing performance.
        CGContextSetAllowsFontSubpixelQuantization(aContext, 1);  // Sets whether or not to allow subpixel quantization for a graphics context

      except
        CGContextRelease(aContext);
        raise;
      end;

    Except
      CGColorSpaceRelease(aColorSpace);
      raise;
    end;

  except
    ALFreeAndNil(aBitmapSurface);
    raise;
  end;

  {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}

  aBitmap := Tbitmap.Create(w, H);
  if aClearBitmap then aBitmap.Clear(TAlphaColorRec.Null);

  {$ENDIF}

end;

{***************************************************}
procedure ALFreeDrawingSurface({$IF defined(ANDROID)}
                               Var aBitmap: Jbitmap;
                               var aCanvas: Jcanvas
                               {$ELSEIF defined(IOS)}
                               var aBitmapSurface: TbitmapSurface;
                               Var aContext: CGContextRef;
                               Var aColorSpace: CGColorSpaceRef
                               {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}
                               Var aBitmap: Tbitmap
                               {$ENDIF});
begin

  {$IFDEF ANDROID}

  aCanvas := nil;
  aBitmap.recycle;
  aBitmap := nil;

  {$ELSEIF DEFINED(IOS)}

  CGContextRelease(aContext);
  CGColorSpaceRelease(aColorSpace);
  ALFreeAndNil(aBitmapSurface);

  {$ELSEIF defined(MSWINDOWS) or defined(_MACOS)}

  ALFreeAndNil(aBitmap);

  {$ENDIF}

end;

{****************}
{$IF defined(IOS)}
Procedure ALCreateDrawingSurfaceV2(var aBitmapSurface: TbitmapSurface;
                                   Var aContext: CGContextRef;
                                   Var aColorSpace: CGColorSpaceRef;
                                   const w: integer;
                                   const h: integer);
begin

  //create the bitmapSurface
  aBitmapSurface := TbitmapSurface.Create;
  try

    //init aBitmapSurface
    aBitmapSurface.SetSize(W,H);

    //create the context
    aContext := CGBitmapContextCreate(aBitmapSurface.Bits, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                           //       memory block should be at least (bytesPerRow*height) bytes.
                                                           //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                           //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                      aBitmapSurface.Width, // width: The width, in pixels, of the required bitmap.
                                      aBitmapSurface.Height, // height: The height, in pixels, of the required bitmap.
                                      8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                         //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                         //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                         //                   chapter of Quartz 2D Programming Guide.
                                      aBitmapSurface.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
                                                            //              a value of 0 causes the value to be calculated automatically.
                                      aColorSpace, // colorspace: The color space to use for the bi1tmap context. Note that indexed color spaces are not supported for
                                                   //             bitmap graphics contexts.
                                      kCGImageAlphaPremultipliedLast or // kCGImageAlphaPremultipliedLast =  For example, premultiplied RGBA
                                                                        // kCGImageAlphaPremultipliedFirst =  For example, premultiplied ARGB
                                                                        // kCGImageAlphaPremultipliedNone =  For example, RGB
                                      kCGBitmapByteOrder32Big); // kCGBitmapByteOrder32Big = Big-endian
                                                                // kCGBitmapByteOrder32Little = Little-endian
                                                                // bitmapInfo: Constants that specify whether the bitmap should contain an alpha channel, the alpha channel’s relative
                                                                //             location in a pixel, and information about whether the pixel components are floating-point or integer
                                                                //             values. The constants for specifying the alpha channel information are declared with the
                                                                //             CGImageAlphaInfo type but can be passed to this parameter safely. You can also pass the other constants
                                                                //             associated with the CGBitmapInfo type. (See CGImage Reference for a description of the CGBitmapInfo
                                                                //             and CGImageAlphaInfo constants.)
                                                                //             For an example of how to specify the color space, bits per pixel, bits per pixel component, and bitmap
                                                                //             information using the CGBitmapContextCreate function, see “Creating a Bitmap Graphics Context” in the
                                                                //             Graphics Contexts chapter of Quartz 2D Programming Guide.
    if aContext = nil then Raise Exception.Create('Call to CGBitmapContextCreate failed');
    try

      //set the paint default properties
      CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
      //-----
      CGContextSetShouldAntialias(aContext, 1); // default: ON
                                                // Sets anti-aliasing on or off for a graphics context.
      CGContextSetAllowsAntialiasing(aContext, 1); // Sets whether or not to allow anti-aliasing for a graphics context.
      //-----
      //CGContextSetShouldSmoothFonts(aContext, 1); // There are cases, such as rendering to a bitmap, when font smoothing is not appropriate and should be disabled.
                                                    // Note that some contexts (such as PostScript contexts) do not support font smoothing.
                                                    // -----
                                                    // Enables or disables font smoothing in a graphics context.
                                                    // When drawing text on a context attached to a color LCD display, Quartz takes advantage of the nature of
                                                    // LCD monitors to improve the legibility of text. This technique is called Font Smoothing. The pixels
                                                    // of an LCD monitor are made up of red, green, and blue sub-pixels. If you take these sub-pixels into
                                                    // account the screen appears to have three times the resolution commonly attributed to it, at least in
                                                    // one dimension. Font smoothing takes advantage of this increased resolution to improve the rendering of
                                                    // text. Quartz turns different sub-pixels off and on by changing the color of a pixels along the edge of
                                                    // letter shapes. Because your eye expects to see a hard line at the edge of the glyphs, the computer tricks
                                                    // it into ignoring the color in favor of perceiving a smooth edge. One disadvantage of font smoothing is
                                                    // that it relies on the fixed ordering of the sub-pixels of an LCD display. That makes the technique of
                                                    // limited use on other types of monitors. Font smoothing is also of limited use on offscreen bitmaps.
      //CGContextSetAllowsFontSmoothing(aContext, 1); // Sets whether or not to allow font smoothing for a graphics context.
      //-----
      CGContextSetShouldSubpixelPositionFonts(aContext, 1); // default: ON
                                                            // When enabled, the graphics context may position glyphs on nonintegral pixel boundaries. When disabled,
                                                            // the position of glyphs are always forced to integral pixel boundaries.
                                                            // -----
                                                            // Enables or disables subpixel positioning in a graphics context.
                                                            // Subpixel positioning concerns whether or not the glyphs in a line of
                                                            // text will be aligned to pixel boundaries or not. If subpixel positioning is
                                                            // off then when glyphs are drawn their positions might be shifted slightly to
                                                            // take pixel boundaries in account. This can improve the visual definition of
                                                            // the glyphs (making them slightly less "blurry") at the expense of honoring
                                                            // the font metrics.
      CGContextSetAllowsFontSubpixelPositioning(aContext, 1); // Sets whether or not to allow subpixel positioning for a graphics context
      //-----
      CGContextSetShouldSubpixelQuantizeFonts(aContext, 1); // default: ON
                                                            // Enables or disables subpixel quantization in a graphics context.
                                                            // -----
                                                            // Subpixel quantization is only enabled if subpixel positioning is enabled. Subpixel
                                                            // quantization improves the rendering of fonts whose glyphs are at subpixel positions
                                                            // by more closely examining how the shapes that make up the glyphs cover an individual pixel.
                                                            // This improvement, requires additional processing so changing this value can affect text
                                                            // drawing performance.
      CGContextSetAllowsFontSubpixelQuantization(aContext, 1);  // Sets whether or not to allow subpixel quantization for a graphics context

    except
      CGContextRelease(aContext);
      raise;
    end;

  except
    ALFreeAndNil(aBitmapSurface);
    raise;
  end;

end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
procedure ALFreeDrawingSurfaceV2(var aBitmapSurface: TbitmapSurface;
                                 Var aContext: CGContextRef);
begin

  CGContextRelease(aContext);
  ALFreeAndNil(aBitmapSurface);

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

initialization
  ALCustomConvertFontFamilyProc := nil;
  {$IFDEF ANDROID}
  ALViewStackCount := 0;
  _RenderScript := nil;
  {$ENDIF}

end.
