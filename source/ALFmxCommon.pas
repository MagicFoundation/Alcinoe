unit ALFmxCommon;

interface

{$I Alcinoe.inc}

uses
  System.classes,
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

{$IF defined(MSWINDOWS) or defined(ALMacOS)}

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
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
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
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; overload;
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              var aTextBreaked: boolean; // out => true is the text was "breaked" in several lines
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; inline; overload;
function  ALDrawMultiLineText(const aText: String; // support only theses EXACT html tag :
                                                   //   <b>...</b>
                                                   //   <i>...</i>
                                                   //   <font color="#xxxxxx">...</font>
                                                   //   <span id="xxx">...</span>
                                                   //   <img src="xxx">
                                                   // other < > must be encoded with &lt; and &gt;
                              var aRect: TRectF; // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF}; inline; overload;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
procedure ALPaintRectangle({$IF defined(ANDROID)}
                           const aCanvas: Jcanvas;
                           {$ELSEIF defined(IOS)}
                           const aContext: CGContextRef;
                           const aColorSpace: CGColorSpaceRef;
                           const aGridHeight: Single;
                           {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
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
                        {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
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
                                 {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
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
                               {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
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

  {$IF CompilerVersion > 34} // sydney
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
    {$IF CompilerVersion >= 34}  // sydney
    FTabOrder: TTabOrder;
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

  {$IF CompilerVersion > 34} // sydney
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

uses
  system.SysUtils,
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
var LSaveChange: TNotifyEvent;
begin
  if Source is TALShadow then begin
    LSaveChange := FOnChanged;
    FOnChanged := nil;
    fenabled := TALShadow(Source).fenabled;
    fblur := TALShadow(Source).fblur;
    fOffsetX := TALShadow(Source).fOffsetX;
    fOffsetY := TALShadow(Source).fOffsetY;
    fShadowColor := TALShadow(Source).fShadowColor;
    FOnChanged := LSaveChange;
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
var LChild: TControl;
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

  for LChild in aControl.Controls do
    ALFmxMakeBufBitmaps(LChild);

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
{$IF CompilerVersion > 34} // sydney
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

var LBreakTextItemsStartCount: integer;
    LBreakTextItem: TALBreakTextItem;
    LNumberOfChars: integer;
    LSaveNumberOfChars: integer;
    LSaveNumberOfCharsIsAccurate: Boolean;
    LLine: jString;
    LLineIndent: Single;
    LEllipsisLine: Jstring;
    LEllipsisLineLn: single;
    LEllipsisLinePos: TpointF;
    LEllipsisLineRect: TrectF;
    LMaxWidth: single;
    LMaxHeight: single;
    LMaxLineWidth: single;
    LLineHeight: single;
    LTotalLinesHeight: single;
    LChar: Char;
    LTextLn: integer;
    LTextIdx: integer;
    LCurrLineY: single;
    LMetrics: JPaint_FontMetricsInt;
    LMeasuredWidth: TJavaArray<Single>;
    LOffset: single;
    LLineEndWithBreakLine: Boolean;
    I, J: integer;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  //their is a fucking bug on android 4.4.2 that aNumberOfChars
  //is the number of Glyph and not of char, so ligature like 'fi' are
  //counted like one glyph :(
  //very few comments about this on the internet
  //http://stackoverflow.com/questions/39891726/android-paint-breaktext-not-work-on-kitkat
  procedure _splitLigature(const _MaxWidth: single);
  var LTmpMeasuredWidth: Single;
  begin
    if (LNumberOfChars < LLine.length) and
       (TJBuild_VERSION.JavaClass.SDK_INT < 22 {lollipop}) then begin
      while LNumberOfChars < LLine.length  do begin
        LTmpMeasuredWidth := aPaint.measureText(LLine{text},
                                                0,
                                                LNumberOfChars + 1);  // measureText seam to be not soo much accurate as breakText unfortunatly (round up)
        if compareValue(LTmpMeasuredWidth, _MaxWidth, TEpsilon.Position) > 0 then break
        else begin
          inc(LNumberOfChars);
          LMeasuredWidth[0] := LTmpMeasuredWidth;
        end;
      end;
    end;
  end;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _initEllipsis;
  var LSavedColor: TalphaColor;
      LSavedTypeFace: JTypeface;
      LTypeFace: JTypeface;
      LJStr1: Jstring;
  begin
    if LEllipsisLine = nil then begin
      //-----
      if aEllipsisText = nil then LEllipsisLine := StringtoJString(string('…'))
      else LEllipsisLine := aEllipsisText;
      //-----
      LSavedTypeFace := nil; // stupid warning
      if aEllipsisFontName <> '' then begin
        LSavedTypeFace := aPaint.getTypeface;
        LJStr1 := StringToJString(aEllipsisFontName); // << https://quality.embarcadero.com/browse/RSP-14187
        LTypeFace := TJTypeface.JavaClass.create(LJStr1, ALfontStyleToAndroidStyle(aEllipsisFontStyle));
        aPaint.setTypeface(LTypeFace);
        LTypeFace := nil;
        LJStr1 := nil;
      end;
      //-----
      LSavedColor := TAlphaColorRec.Null; // stupid warning
      if aEllipsisFontColor <> TAlphaColorRec.Null then begin
        LSavedColor := Cardinal(aPaint.getColor);
        aPaint.setColor(integer(aEllipsisFontColor));
      end;
      //-----
      LEllipsisLineLn := aPaint.measureText(LEllipsisLine);
      if aEllipsisFontName <> '' then aPaint.setTypeface(LSavedTypeFace);
      if aEllipsisFontColor <> TAlphaColorRec.Null then aPaint.setColor(integer(LSavedColor));
      //-----
      case AHTextAlign of
        TTextAlign.Center: begin
                             LEllipsisLinePos := TpointF.create((LMaxWidth - LEllipsisLineLn - LLineIndent) / 2, LCurrLineY);
                           end;
        TTextAlign.Leading: begin
                              LEllipsisLinePos := TpointF.create(LLineIndent, LCurrLineY);
                            end;
        TTextAlign.Trailing: begin
                               LEllipsisLinePos := TpointF.create(LMaxWidth - LEllipsisLineLn, LCurrLineY);
                             end;
      end;
      LEllipsisLineRect := Trectf.Create(TPointF.Create(LEllipsisLinePos.x,
                                                        LEllipsisLinePos.Y - (-1*LMetrics.ascent)),
                                         LEllipsisLineLn,
                                         (-1*LMetrics.ascent) + LMetrics.descent);
      //-----
    end;
  end;

begin

  //init result
  result := false;
  aAllTextDrawed := true;

  //init aBreakTextItemsStartCount
  LBreakTextItemsStartCount := aBreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / aTotalLinesHeight
  if aRect.Width > 16384 then aRect.Width := 16384;  // << because on android kitkat (4.4.2) it's look like that aPaint.breakText with maxWidth > 16384 return 0 :(
  if aRect.height > 16384 then aRect.height := 16384;
  LMaxWidth := ARect.width;
  LMaxHeight := ARect.Height;
  LMaxLineWidth := 0;
  LTotalLinesHeight := 0;

  //init ATextIdx / ATextLn
  LTextIdx := 0;
  LTextLn := AText.length;

  //init metics / aCurrLineY / aLineHeight
  LMetrics := aPaint.getFontMetricsInt; // aMetrics.top       => The maximum distance above the baseline for the tallest glyph in the font at a given text size.
                                        // aMetrics.ascent    => The recommended distance above the baseline for singled spaced text.
                                        // aMetrics.descent   => The recommended distance below the baseline for singled spaced text.
                                        // aMetrics.bottom    => The maximum distance below the baseline for the lowest glyph in the font at a given text size
                                        // aMetrics.leading   => The recommended additional space to add between lines of text
  LCurrLineY := aFirstLineIndent.y + (-1*LMetrics.ascent); // aMetrics.top and aMetrics.ascent are always returned in negative value
  aTotalLines := 0;
  LLineHeight := LMetrics.descent + aLineSpacing + (-1*LMetrics.ascent);

  //init aEllipsisLine
  LEllipsisLine := nil;
  LEllipsisLineLn := 0;

  //init aLineIndent
  LLineIndent := aFirstLineIndent.x;

  //if we have at least enalf of height to write the 1rt row
  if comparevalue(aFirstLineIndent.y + LMetrics.descent + (-1*LMetrics.Ascent),LMaxHeight,Tepsilon.position) <= 0 then begin

    //create ameasuredWidth
    LMeasuredWidth := TJavaArray<Single>.Create(1);
    try

      //loop still their is some chars
      while LTextIdx < LTextLn do begin

        // init aline
        LLine := nil; // << https://quality.embarcadero.com/browse/RSP-14187
        I := aText.indexOf($0D {c}, LTextIdx{start}); // find if their is some #13 (MSWINDOWS linebreak = #13#10)
        J := aText.indexOf($0A {c}, LTextIdx{start}); // find if their is some #10 (UNIX linebreak = #10)
        if (I >= 0) and (J >= 0) then I := min(I,J)
        else I := max(I, J);
        if I = LTextIdx then begin
          LLine := StringtoJString(string(''));
          LLineEndWithBreakLine := True;
          result := true;
        end
        else if I > 0 then begin
          LLine := aText.substring(LTextIdx{start}, I{end_}); // skip the $0D/$0A
          LLineEndWithBreakLine := True;
          result := true;
        end
        else begin
          LLine := aText.substring(LTextIdx{start});
          LLineEndWithBreakLine := False;
        end;

        //calculate the number of char in the current line (this work good also if aline is empty)
        LNumberOfChars := aPaint.breakText(LLine {text},
                                           true {measureForwards},
                                           LMaxWidth - LLineIndent, {maxWidth}
                                           LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                            // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                            // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                            //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                            //   it's return for exemple 9 when height = 11
       _splitLigature(LMaxWidth - LLineIndent);

        //init result
        if LNumberOfChars < LLine.length then result := true;

        //if we need to break the text
        if (LNumberOfChars < LLine.length) or // << if aNumberOfChars < aLine.length it's evident we will need to break the text
           (                                                                                                    // <<
            (LLineEndWithBreakLine) and                                                                         // <<
            (aTrimming <> TTextTrimming.None) and                                                               // <<
            (                                                                                                   // << we need this check to add the ellipsis on the last line
             (not aWordWrap) or                                                                                 // << when the last line finish by a break line (#13#10)
             ((compareValue(LCurrLineY + LLineHeight + LMetrics.descent, LMaxHeight, Tepsilon.position) > 0) or // <<
              ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1)))                                             // <<
            )                                                                                                   // <<
           )                                                                                                    // <<
        then begin

          //if not aWordWrap
          if not aWordWrap then begin
            aAllTextDrawed := False; // aNumberOfChars < aLine.length so in anycase we will not draw all the text
            case aTrimming of
              TTextTrimming.None: begin
                                    if LNumberOfChars > 0 then
                                      LLine := LLine.substring(0, LNumberOfChars);
                                  end;
              TTextTrimming.Character: begin
                                         //-----
                                         _initEllipsis;
                                         //-----
                                         if (LNumberOfChars < LLine.length) then dec(LNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                      // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                      // we considere that aEllipsisText is only one char
                                         while LNumberOfChars > 0 do begin
                                           LLine := LLine.substring(0, LNumberOfChars);
                                           LNumberOfChars := aPaint.breakText(LLine {text},
                                                                              true {measureForwards},
                                                                              LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                                              LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                                               // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                                               // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                                               //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                                               //   it's return for exemple 9 when height = 11
                                           _splitLigature(LMaxWidth - LEllipsisLineLn - LLineIndent);
                                           if LNumberOfChars >= LLine.length then break;
                                         end;
                                         //-----
                                       end;
              TTextTrimming.Word: begin
                                    //-----
                                    _initEllipsis;
                                    //-----
                                    LSaveNumberOfChars := LNumberOfChars;
                                    LSaveNumberOfCharsIsAccurate := False;
                                    while LNumberOfChars > 0 do begin
                                      //----
                                      if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                                        LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                                      end
                                      //----
                                      else if LNumberOfChars >= 2 then begin
                                        LChar := LLine.charAt(LNumberOfChars-2);
                                        if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                                          dec(LNumberOfChars);
                                          continue;
                                        end;
                                        LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                                      end
                                      //----
                                      else begin
                                        LNumberOfChars := LSaveNumberOfChars;
                                        if (not LSaveNumberOfCharsIsAccurate) and (LNumberOfChars < LLine.length) then dec(LNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                                                            // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                                                            // we considere that aEllipsisText is only one char
                                        while LNumberOfChars > 0 do begin
                                          LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                                          LNumberOfChars := aPaint.breakText(LLine {text},
                                                                             true {measureForwards},
                                                                             LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                                             LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                                              // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                                              // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                                              //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                                              //   it's return for exemple 9 when height = 11
                                          _splitLigature(LMaxWidth - LEllipsisLineLn - LLineIndent);
                                          if LNumberOfChars >= LLine.length then break;
                                        end;
                                        break;
                                      end;
                                      //----
                                      LNumberOfChars := aPaint.breakText(LLine {text},
                                                                         true {measureForwards},
                                                                         LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                                         LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                                          // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                                          // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                                          //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                                          //   it's return for exemple 9 when height = 11
                                      _splitLigature(LMaxWidth - LEllipsisLineLn - LLineIndent);
                                      if LNumberOfChars >= LLine.length then break
                                      else begin
                                        LSaveNumberOfChars:= LNumberOfChars;
                                        LSaveNumberOfCharsIsAccurate := True;
                                      end;
                                      //----
                                    end;
                                  end;
            end;
          end

          //if aWordWrap
          else begin

            //We are at the last line and aTrimming <> TTextTrimming.None
            if ((compareValue(LCurrLineY + LLineHeight + LMetrics.descent, LMaxHeight, Tepsilon.position) > 0) or
                ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1))) and
               (aTrimming <> TTextTrimming.None) then begin

              //-----
              aAllTextDrawed := False; // if we are at the last line then in anycase we will not draw all the text
              _initEllipsis;
              //-----
              LSaveNumberOfChars := LNumberOfChars;
              LSaveNumberOfCharsIsAccurate := False;
              while LNumberOfChars > 0 do begin
                //----
                if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                  LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                end
                //----
                else if (aTrimming = TTextTrimming.Word) and (LNumberOfChars >= 2) then begin
                  LChar := LLine.charAt(LNumberOfChars-2);
                  if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                    dec(LNumberOfChars);
                    continue;
                  end;
                  LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                end
                //----
                else begin
                  LNumberOfChars := LSaveNumberOfChars;
                  if (not LSaveNumberOfCharsIsAccurate) and (LNumberOfChars < LLine.length) then dec(LNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                                      // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                                      // we considere that aEllipsisText is only one char
                  while LNumberOfChars > 0 do begin
                    LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                    LNumberOfChars := aPaint.breakText(LLine {text},
                                                       true {measureForwards},
                                                       LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                       LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                        // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                        // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                        //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                        //   it's return for exemple 9 when height = 11
                    _splitLigature(LMaxWidth - LEllipsisLineLn - LLineIndent);
                    if LNumberOfChars >= LLine.length then break;
                  end;
                  break;
                end;
                //----
                LNumberOfChars := aPaint.breakText(LLine {text},
                                                   true {measureForwards},
                                                   LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                   LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                    // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                    // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                    //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                    //   it's return for exemple 9 when height = 11
                _splitLigature(LMaxWidth - LEllipsisLineLn - LLineIndent);
                if LNumberOfChars >= LLine.length then break
                else begin
                  LSaveNumberOfChars:= LNumberOfChars;
                  LSaveNumberOfCharsIsAccurate := True;
                end;
                //----
              end;

            end

            //We are not at the last line or aTrimming = TTextTrimming.None
            else begin

              //We are at the last line and aTrimming = TTextTrimming.None and more line available
              if (aTrimming <> TTextTrimming.None) and
                 ((compareValue(LCurrLineY + LLineHeight + LMetrics.descent, LMaxHeight, Tepsilon.position) > 0) or
                  ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1))) then aAllTextDrawed := False;

              //cut the line
              LSaveNumberOfChars := LNumberOfChars;
              if LNumberOfChars < LLine.length then inc(LNumberOfChars); // in case the space separator is just after aNumberOfChars
              while LNumberOfChars > 0 do begin
                //-----
                if LNumberOfChars >= 2 then begin
                  LChar := LLine.charAt(LNumberOfChars-1);
                  if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                    dec(LNumberOfChars);
                    continue;
                  end;
                  LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish just before the space
                end
                //-----
                else begin
                  LNumberOfChars := LSaveNumberOfChars;
                  if compareValue(LLineIndent, 0, TEpsilon.position) > 0 then LNumberOfChars := 0;
                  while LNumberOfChars > 0 do begin
                    LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                    LNumberOfChars := aPaint.breakText(LLine {text},
                                                       true {measureForwards},
                                                       LMaxWidth - LLineIndent, {maxWidth}
                                                       LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                        // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                        // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                        //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                        //   it's return for exemple 9 when height = 11
                    _splitLigature(LMaxWidth - LLineIndent);
                    if LNumberOfChars >= LLine.length then break;
                  end;
                  break;
                end;
                //-----
                LNumberOfChars := aPaint.breakText(LLine {text},
                                                   true {measureForwards},
                                                   LMaxWidth - LLineIndent, {maxWidth}
                                                   LMeasuredWidth {measuredWidth}); // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
                                                                                    // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
                                                                                    // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
                                                                                    //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
                                                                                    //   it's return for exemple 9 when height = 11
                _splitLigature(LMaxWidth - LLineIndent);
                if LNumberOfChars >= LLine.length then begin
                  inc(LNumberOfChars); // to skip the separator
                  break;
                end
                else LSaveNumberOfChars:= LNumberOfChars;
                //-----
              end;

            end;

          end;

        end;

        //init aMaxLineWidth
        LMaxLineWidth := max(LMaxLineWidth, LMeasuredWidth[0] + LEllipsisLineLn + LLineIndent);

        // update aTotalLinesHeight
        LTotalLinesHeight := LCurrLineY + LMetrics.descent;

        // their is not enalf of place to write at least one char or
        // we are on the #13/#10
        // NOTE: we need to remove the breakline because for exemple we have
        // coco sur le cocotier#13#10
        // then aline will be equal to
        // coco sur le cocotier
        // we draw this line, and then we increate aCurrLineY := aCurrLineY + aLineHeight;
        // so it's mean the #13#10 was already taking in account, so we must delete it
        if LNumberOfChars <= 0 then begin
          if (LTextIdx + 1 < LTextLn) and
             (aText.codePointAt(LTextIdx) = $0D) and
             (aText.codePointAt(LTextIdx + 1) = $0A) then LTextIdx := LTextIdx + 2 // (MSWINDOWS linebreak = #13#10)
          else if (LTextIdx < LTextLn) and
                  (aText.codePointAt(LTextIdx) = $0A) then LTextIdx := LTextIdx + 1 // (UNIX linebreak = #10)
          else if (LTextIdx < LTextLn) and
                  ((aText.charAT(LTextIdx).IsWhiteSpace) and
                   (aText.charAT(LTextIdx).ToUCS4Char <> $00A0{No-break Space})) then LTextIdx := LTextIdx + 1 // (white space) if we don't have place to write
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
          else if compareValue(LLineIndent, 0, Tepsilon.Position) <= 0 then begin // if aLineIndent > 0 then maybe we don't have enalf of place to write one char because of the aLineIndent.
            LTextIdx := LTextIdx + 1; // skip the current char
            if (LTextIdx < LTextLn) and
               (aText.CharAT(LTextIdx).IsLowSurrogate) then inc(LTextIdx);
          end;
          LCurrLineY := LCurrLineY + LLineHeight; // go to next line
          inc(aTotalLines);
          LLineIndent := 0;
          if (not aWordWrap) or
             ((aMaxLines > 0) and (aTotalLines >= aMaxLines)) or
             (compareValue(LCurrLineY + LMetrics.descent, LMaxHeight, TEpsilon.position) > 0) then break
          else continue;
        end
        else begin
          LTextIdx := LTextIdx + LNumberOfChars;
          if (LTextIdx + 1 < LTextLn) and
             (aText.codePointAt(LTextIdx) = $0D) and
             (aText.codePointAt(LTextIdx + 1) = $0A) then LTextIdx := LTextIdx + 2 // (MSWINDOWS linebreak = #13#10)
          else if (LTextIdx < LTextLn) and
                  (aText.codePointAt(LTextIdx) = $0A) then LTextIdx := LTextIdx + 1;
        end;

        //init aBreakedText
        LBreakTextItem := TalBreakTextItem.Create;
        try

          //update aBreakTextItem
          LBreakTextItem.line := LLine;
          case AHTextAlign of
            TTextAlign.Center: begin
                                 LBreakTextItem.pos := TpointF.create((LMaxWidth - LMeasuredWidth[0] - LEllipsisLineLn - LLineIndent) / 2, LCurrLineY);
                               end;
            TTextAlign.Leading: begin
                                  LBreakTextItem.pos := TpointF.create(LLineIndent, LCurrLineY);
                                end;
            TTextAlign.Trailing: begin
                                   LBreakTextItem.pos := TpointF.create(LMaxWidth - LMeasuredWidth[0] - LEllipsisLineLn, LCurrLineY);
                                 end;
          end;
          LBreakTextItem.rect := Trectf.Create(TPointF.Create(LBreakTextItem.pos.x,
                                                              LBreakTextItem.pos.Y - (-1*LMetrics.ascent)),
                                               LMeasuredWidth[0],
                                               (-1*LMetrics.ascent) + LMetrics.descent);

          //update aEllipsisLinePos / aEllipsisLinerect
          if LEllipsisLine <> nil then begin
            LEllipsisLinePos := TpointF.Create(LBreakTextItem.pos.x + LMeasuredWidth[0], LCurrLineY);
            LEllipsisLineRect := Trectf.Create(TPointF.Create(LBreakTextItem.pos.x + LMeasuredWidth[0],
                                                              LBreakTextItem.pos.Y - (-1*LMetrics.ascent)),
                                               LEllipsisLineLn,
                                               (-1*LMetrics.ascent) + LMetrics.descent);
          end;

          // update aBreakTextItems
          aBreakTextItems.Add(LBreakTextItem);

        except
          ALFreeAndNil(LBreakTextItem);
          raise;
        end;

        //update aCurrLineY
        LCurrLineY := LCurrLineY + LLineHeight;
        inc(aTotalLines);

        //update aLineIndent
        LLineIndent := 0;

        // stop if not aWordWrap or after maxheight
        if (not aWordWrap) or
           ((aMaxLines > 0) and (aTotalLines >= aMaxLines)) or
           (compareValue(LCurrLineY + LMetrics.descent, LMaxHeight, TEpsilon.position) > 0) then break;

        //add the last empty row if their is one
        if (LTextIdx >= LTextLn) and LLineEndWithBreakLine and (LEllipsisLine = nil) then begin

          //init aBreakedText
          LBreakTextItem := TalBreakTextItem.Create;
          try

            //update aBreakTextItem
            LBreakTextItem.line := StringToJstring('');
            case AHTextAlign of
              TTextAlign.Center: begin
                                   LBreakTextItem.pos := TpointF.create((LMaxWidth - LEllipsisLineLn - LLineIndent) / 2, LCurrLineY);
                                 end;
              TTextAlign.Leading: begin
                                    LBreakTextItem.pos := TpointF.create(LLineIndent, LCurrLineY);
                                  end;
              TTextAlign.Trailing: begin
                                     LBreakTextItem.pos := TpointF.create(LMaxWidth - LEllipsisLineLn, LCurrLineY);
                                   end;
            end;
            LBreakTextItem.rect := Trectf.Create(TPointF.Create(LBreakTextItem.pos.x,
                                                                LBreakTextItem.pos.Y - (-1*LMetrics.Ascent)),
                                                 0,
                                                 (-1*LMetrics.Ascent) + LMetrics.Descent);

            // update aBreakTextItems
            aBreakTextItems.Add(LBreakTextItem);

          except
            ALFreeAndNil(LBreakTextItem);
            raise;
          end;

          //update aCurrLineY
          LCurrLineY := LCurrLineY + LLineHeight;
          inc(aTotalLines);

        end;

      end;

    finally
      ALFreeAndNil(LMeasuredWidth);
      LLine := nil; // << https://quality.embarcadero.com/browse/RSP-14187
    end;

  end
  else result := true;

  //add the end ellipsis
  if LEllipsisLine <> nil then begin
    LBreakTextItem := TalBreakTextItem.Create;
    try
      LBreakTextItem.line := LEllipsisLine;
      LEllipsisLine := nil;
      LBreakTextItem.pos := LEllipsisLinePos;
      LBreakTextItem.rect := LEllipsisLineRect;
      LBreakTextItem.isEllipsis := True;
      aBreakTextItems.Add(LBreakTextItem);
    except
      ALFreeAndNil(LBreakTextItem);
      raise;
    end;
  end;

  //initialise ARect
  if compareValue(LMaxLineWidth, LMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            LOffset := Floor((aRect.Right - LMaxLineWidth - arect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            aRect.Left := aRect.Left + LOffset;
                            aRect.right := aRect.right - LOffset;
                            for I := LBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                              aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - LOffset;
                              aBreakTextItems[I].rect.Offset(-LOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             aRect.Right := min(aRect.Right, aRect.Left + LMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              LOffset := Floor(aRect.Right - LMaxLineWidth - arect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              aRect.Left := aRect.Left + LOffset;
                              for I := LBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                                aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - LOffset;
                                aBreakTextItems[I].rect.Offset(-LOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(LTotalLinesHeight, LMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            LOffset := (aRect.bottom - LTotalLinesHeight - arect.top) / 2;
                            aRect.top := aRect.top + LOffset;
                            aRect.bottom := aRect.bottom - LOffset;
                          end;
       TTextAlign.Leading: begin
                             aRect.bottom := min(aRect.bottom, aRect.top + LTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              LOffset := aRect.bottom - LTotalLinesHeight - arect.top;
                              aRect.top := aRect.top + LOffset;
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
var LTotalLines: integer;
    LAllTextDrawed: boolean;
begin
  result := ALBreakText(aPaint, // const aPaint: JPaint;
                        ARect, // var ARect: TRectF;
                        AText, // const AText: JString;
                        aWordWrap, // const aWordWrap: Boolean;
                        AHTextAlign, AVTextAlign, // const AHTextAlign, AVTextAlign: TTextAlign;
                        aTrimming, // const aTrimming: TTextTrimming;
                        aBreakTextItems, // const aBreakTextItems: TALBreakTextItems;
                        LTotalLines, // var aTotalLines: integer;
                        LAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
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

var LBreakTextItemsStartCount: integer;
    LBreakTextItem: TALBreakTextItem;
    LEllipsisBreakTextItem: TALBreakTextItem;
    LEllipsisLine: CtLineRef;
    LEllipsisWidth: Double;
    LEllipsisFont: CTFontRef;
    LEllipsisString: CFStringRef;
    LEllipsisAttr: CFMutableAttributedStringRef;
    LEllipsisColor: CGColorRef;
    LFramePath: CGMutablePathRef;
    LFrameSetter: CTFramesetterRef;
    LFrame: CTFrameRef;
    Llines: CFArrayRef;
    Lline: CTLineRef;
    LLinesCount: CFIndex;
    LLineIndent: single;
    LTextAttr: CFMutableAttributedStringRef;
    LTmpTextAttr: CFAttributedStringRef;
    LMaxWidth: single;
    LMaxHeight: single;
    LPrevMaxLineWidth: Single;
    LMaxLineWidth: single;
    LCurrLineY: single;
    LTotalLinesHeight: single;
    LAscent, LDescent: CGFloat;
    LMeasuredWidth: Double;
    LSettings: array of CTParagraphStyleSetting;
    LParagraphStyle: CTParagraphStyleRef;
    LCGColor: CGColorRef;
    LTextString: CFStringRef;
    LFont: CTFontRef;
    LOffset: single;
    LStringRange: CFRange;
    LFirstLineHeadIndent: CGFloat;
    LLineSpacingAdjustment: CGFloat;
    LLineBreakMode: Byte;
    LAlphaColor: TAlphaColorCGFloat;
    I: CFIndex;

begin

  //init aAllTextDrawed
  aAllTextDrawed := True;

  //init aBreakTextItemsStartCount
  LBreakTextItemsStartCount := aBreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / aTotalLinesHeight / etc.
  if aRect.Width > 65535 then aRect.Width := 65535;
  if aRect.height > 65535 then aRect.height := 65535;
  LMaxWidth := ARect.width;
  LMaxHeight := ARect.Height;
  LPrevMaxLineWidth := 0; // << need this vars because we must recalculate the maxlineWidth for the last lines after the truncation is maded
  LMaxLineWidth := 0;
  LTotalLinesHeight := 0;
  aTotalLines := 0;
  LLineIndent := aFirstLineIndent.x;

  //create aCGColor
  LAlphaColor := TAlphaColorCGFloat.Create(aFontColor);
  LCGColor := CGColorCreate(aColorSpace, @LAlphaColor);
  try

    //create aFont
    LFont := ALGetCTFontRef(aFontName, aFontSize, aFontStyle); // Returns a new font reference for the given name.
    if LFont = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;
    try

      //create aTextString
      LTextString := CFStringCreateWithCharacters(kCFAllocatorDefault, @AText[Low(string)], Length(AText));
      if LTextString = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;
      try

        //create aTextAttr
        LTextAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
        try

          CFAttributedStringReplaceString(LTextAttr, CFRangeMake(0, 0), LTextString); // Modifies the string of an attributed string.
          CFAttributedStringBeginEditing(LTextAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
          try
            CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTFontAttributeName, LFont);
            CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTForegroundColorAttributeName, LCGColor);
            //-----
            SetLength(LSettings, 0);
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
              SetLength(LSettings, length(LSettings) + 1);
              LFirstLineHeadIndent := aFirstLineIndent.x;
              LSettings[high(LSettings)].spec := kCTParagraphStyleSpecifierFirstLineHeadIndent;
              LSettings[high(LSettings)].valueSize := SizeOf(LFirstLineHeadIndent);
              LSettings[high(LSettings)].value := @LFirstLineHeadIndent;
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
              SetLength(LSettings, length(LSettings) + 1);
              case aTrimming of
                TTextTrimming.None: LLineBreakMode := kCTLineBreakByClipping;
                TTextTrimming.Character: LLineBreakMode := kCTLineBreakByCharWrapping;
                TTextTrimming.Word: LLineBreakMode := kCTLineBreakByWordWrapping;
              end;
              LSettings[high(LSettings)].spec := kCTParagraphStyleSpecifierLineBreakMode;
              LSettings[high(LSettings)].valueSize := SizeOf(LLineBreakMode);
              LSettings[high(LSettings)].value := @LLineBreakMode;
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
              SetLength(LSettings, length(LSettings) + 1);
              LLineSpacingAdjustment := aLineSpacing;
              LSettings[high(LSettings)].spec := kCTParagraphStyleSpecifierLineSpacingAdjustment;
              LSettings[high(LSettings)].valueSize := SizeOf(LLineSpacingAdjustment);
              LSettings[high(LSettings)].value := @LLineSpacingAdjustment;
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
            if length(LSettings) > 0 then begin
              LParagraphStyle := CTParagraphStyleCreate(@LSettings[0], Length(LSettings));
              try
                CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTParagraphStyleAttributeName, LParagraphStyle);
              finally
                CFRelease(LParagraphStyle);
              end;
            end;
          finally
            CFAttributedStringEndEditing(LTextAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
          end;



          /////////////////////////////
          //Break the text in line(s)//
          /////////////////////////////

          //Create an immutable path of a rectangle.
          LFramePath := CGPathCreateWithRect(ALLowerLeftCGRect(tpointf.create(0,0){aUpperLeftOrigin},
                                                               ARect.Width{aWidth},
                                                               ARect.Height - aFirstLineIndent.y{aHeight},
                                                               ARect.Height - aFirstLineIndent.y{aGridHeight}),
                                                               nil{transform});
          try

            //Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
            //create and fill text frames with the CTFramesetterCreateFrame call.
            LFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(LTextAttr));
            if LFrameSetter = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;  // CTFramesetterCreateWithAttributedString return NULL if unsuccessful.
            try

              //Creates an immutable frame using a framesetter.
              LFrame := CTFramesetterCreateFrame(LFrameSetter, // framesetter: The framesetter used to create the frame.
                                                 CFRangeMake(0, 0), // stringRange: The range, of the attributed string that was used to create the framesetter,
                                                                    // that is to be typeset in lines fitted into the frame. If the length portion of the range is
                                                                    // set to 0, then the framesetter continues to add lines until it runs out of text or space.
                                                 LFramePath, // path: A CGPath object that specifies the shape of the frame. The path may be non-rectangular
                                                             // in versions of OS X v10.7 or later and versions of iOS 4.2 or later.
                                                 nil); // frameAttributes: Additional attributes that control the frame filling process can be specified here,
                                                       // or NULL if there are no such attributes.
              if LFrame = nil then begin ARect.Width := 0; ARect.Height := 0; exit(False); end;  // CTFramesetterCreateFrame return NULL if unsuccessful.
              try

                //init alines / aLinesCount
                Llines := CTFrameGetLines(LFrame); // Return a CFArray object containing the CTLine objects that make up the frame, or, if there are no lines in the frame, an array with no elements.
                LLinesCount := CFArrayGetCount(Llines);

                //init result
                result := LLinesCount > 1;

                //update aBreakTextItems - loop on all lines
                for I := 0 to LLinesCount - 1 do begin

                  //break if maxline reach
                  if (aMaxlines > 0) and (aTotalLines >= aMaxlines) then break; // << no need to set the result to true because aLinesCount > 1

                  //break if not wordwrap
                  if (not aWordwrap) and (aTotalLines >= 1) then break; // << no need to set the result to true because aLinesCount > 1

                  //init aline / aMeasuredWidth
                  Lline := CFArrayGetValueAtIndex(Llines, I);
                  LMeasuredWidth := CTLineGetTypographicBounds(Lline, // line: The line whose typographic bounds are calculated.
                                                               @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                               @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                               nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                     // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                  //unfortunatly the lines that are wrapped are not trimed with the last space
                  //so aMeasuredWidth is inacurate. so i must trim the trailling char
                  if I < LLinesCount - 1 then LMeasuredWidth := LMeasuredWidth - CTLineGetTrailingWhitespaceWidth(Lline);

                  //update aCurrLineY
                  if I = 0 then LCurrLineY := aFirstLineIndent.y + LAscent
                  else LCurrLineY := LTotalLinesHeight + aLineSpacing + LAscent;

                  // stop if after maxheight
                  if (compareValue(LCurrLineY + LDescent, LMaxHeight, TEpsilon.position) > 0) then begin
                    result := True;
                    aAllTextDrawed := False;
                    break;
                  end;

                  // update aTotalLinesHeight and aTotalLines
                  LTotalLinesHeight := LCurrLineY + LDescent;
                  inc(aTotalLines);

                  //alineindent must be init here (after the break) because alineindent must
                  //correspond to the last item in aBreakTextItems
                  if I > 0 then LLineIndent := 0;

                  // calculate aMaxLineWidth
                  LPrevMaxLineWidth := LMaxLineWidth;
                  LMaxLineWidth := max(LMaxLineWidth, LMeasuredWidth + LLineIndent);

                  // init aStringRange
                  LStringRange := CTLineGetStringRange(Lline); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

                  // update aBreakTextItems
                  LBreakTextItem := TalBreakTextItem.Create;
                  try

                    // aBreakTextItem.Line
                    LBreakTextItem.Line := CFRetain(Lline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line

                    // aBreakTextItem.text
                    if LStringRange.length > 0 then begin
                      LTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, CFAttributedStringRef(LTextAttr), LStringRange); // return A new attributed string whose string and attributes are copied from the specified range of the supplied attributed string. Returns NULL if there was a problem copying the object.
                      if LTmpTextAttr <> nil then begin
                        try
                          LBreakTextItem.text := CFStringRefToStr(CFAttributedStringGetString(LTmpTextAttr));  // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object.
                        finally
                          cfRelease(LTmpTextAttr);
                        end;
                      end
                      else LBreakTextItem.text := '';
                    end
                    else LBreakTextItem.text := '';

                    // aBreakTextItem.pos / aBreakTextItem.rect
                    case AHTextAlign of
                      TTextAlign.Center: begin
                                           LBreakTextItem.pos := TpointF.create((LMaxWidth - LMeasuredWidth - LLineIndent) / 2, LCurrLineY);
                                         end;
                      TTextAlign.Leading: begin
                                            LBreakTextItem.pos := TpointF.create(LLineIndent, LCurrLineY);
                                          end;
                      TTextAlign.Trailing: begin
                                             LBreakTextItem.pos := TpointF.create(LMaxWidth - LMeasuredWidth, LCurrLineY);
                                           end;
                    end;
                    LBreakTextItem.rect := Trectf.Create(TPointF.Create(LBreakTextItem.pos.x,
                                                                        LBreakTextItem.pos.Y - LAscent),
                                                         LMeasuredWidth,
                                                         LAscent + LDescent);

                    // add aBreakTextItem to aBreakTextItems
                    aBreakTextItems.Add(LBreakTextItem);

                  except
                    ALFreeAndNil(LBreakTextItem);
                    raise;
                  end;

                end;

              finally
                CFRelease(LFrame);
              end;

            finally
              CFRelease(LFrameSetter);
            end;

          finally
            CFRelease(LFramePath);
          end;



          //////////////////////////
          //truncate the last line//
          //////////////////////////

          if (aBreakTextItems.Count > LBreakTextItemsStartCount) and  //if the text was at least breaked in one line
             (LStringRange.length > 0) and  // aStringRange was initialised previously
             (LStringRange.location + LStringRange.length < CFAttributedStringGetLength(CFAttributedStringRef(LTextAttr))) then begin // if the last line do not contain all the chars

            //init result
            result := True;
            aAllTextDrawed := False;

            //if aTrimming = TTextTrimming.None or aEllipsisAttr = nil then nothing todo
            if (aTrimming <> TTextTrimming.None) and
               (aEllipsisText <> '') then begin

              //create aEllipsisColor
              if aEllipsisFontColor <> TAlphaColorRec.Null then LAlphaColor := TAlphaColorCGFloat.Create(aEllipsisFontColor)
              else LAlphaColor := TAlphaColorCGFloat.Create(aFontColor);
              LEllipsisColor := CGColorCreate(aColorSpace, @LAlphaColor);
              try

                //create aEllipsisFont
                LEllipsisFont := ALGetCTFontRef(aFontName, aFontSize, aEllipsisFontStyle); // Returns a new font reference for the given name.
                if LEllipsisFont <> nil then begin
                  try

                    //create aEllipsisString
                    LEllipsisString := CFStringCreateWithCharacters(kCFAllocatorDefault, @aEllipsisText[Low(string)], Length(aEllipsisText));
                    if LEllipsisString <> nil then begin
                      try

                        //create aEllipsisAttr
                        LEllipsisAttr := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength}); // Creates a mutable attributed string.
                        try

                          CFAttributedStringReplaceString(LEllipsisAttr, CFRangeMake(0, 0), LEllipsisString); // Modifies the string of an attributed string.
                          CFAttributedStringBeginEditing(LEllipsisAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                          try
                            CFAttributedStringSetAttribute(LEllipsisAttr, CFRangeMake(0, CFStringGetLength(LEllipsisString)), kCTFontAttributeName, LEllipsisFont);
                            CFAttributedStringSetAttribute(LEllipsisAttr, CFRangeMake(0, CFStringGetLength(LEllipsisString)), kCTForegroundColorAttributeName, LEllipsisColor);
                          finally
                            CFAttributedStringEndEditing(LEllipsisAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
                          end;

                          //create the aEllipsisLine
                          LEllipsisLine := CTLineCreateWithAttributedString(CFAttributedStringRef(LEllipsisAttr)); // Creates a single immutable line object directly from an attributed string.
                          if LEllipsisLine <> nil then begin                                                       // Return Value: A reference to a CTLine object if the call was successful; otherwise, NULL.
                            try

                              CFAttributedStringBeginEditing(LTextAttr); // Defers internal consistency-checking and coalescing for a mutable attributed string.
                              try
                                //-----
                                SetLength(LSettings, 0);
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
                                SetLength(LSettings, length(LSettings) + 1);
                                case aTrimming of
                                  TTextTrimming.None: LLineBreakMode := kCTLineBreakByClipping;
                                  TTextTrimming.Character: LLineBreakMode := kCTLineBreakByCharWrapping;
                                  TTextTrimming.Word: LLineBreakMode := kCTLineBreakByWordWrapping;
                                end;
                                LSettings[high(LSettings)].spec := kCTParagraphStyleSpecifierLineBreakMode;
                                LSettings[high(LSettings)].valueSize := SizeOf(LLineBreakMode);
                                LSettings[high(LSettings)].value := @LLineBreakMode;
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
                                if length(LSettings) > 0 then begin
                                  LParagraphStyle := CTParagraphStyleCreate(@LSettings[0], Length(LSettings));
                                  try
                                    CFAttributedStringSetAttribute(LTextAttr, CFRangeMake(0, CFStringGetLength(LTextString)), kCTParagraphStyleAttributeName, LParagraphStyle);
                                  finally
                                    CFRelease(LParagraphStyle);
                                  end;
                                end;
                                //-----
                              finally
                                CFAttributedStringEndEditing(LTextAttr); // Re-enables internal consistency-checking and coalescing for a mutable attributed string.
                              end;

                              //init aEllipsisWidth
                              LEllipsisWidth := CTLineGetTypographicBounds(LEllipsisLine, // line: The line whose typographic bounds are calculated.
                                                                           @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                                           @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                                           nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                                 // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                              //Create an immutable path of a rectangle.
                              LFramePath := CGPathCreateWithRect(ALLowerLeftCGRect(tpointf.create(0,0){aUpperLeftOrigin},
                                                                                   LMaxWidth - LEllipsisWidth - LLineIndent{aWidth},
                                                                                   ceil(LAscent+LDescent){aHeight}, // +1 because it's seam when height is exact then it's not work
                                                                                   ceil(LAscent+LDescent){aGridHeight}),
                                                                                   nil{transform});

                              try

                                //Creates an immutable framesetter object from an attributed string. The resultant framesetter object can be used to
                                //create and fill text frames with the CTFramesetterCreateFrame call.
                                LFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(LTextAttr));
                                if LFrameSetter <> nil then begin  // CTFramesetterCreateWithAttributedString return NULL if unsuccessful.
                                  try

                                    //Creates an immutable frame using a framesetter.
                                    LFrame := CTFramesetterCreateFrame(LFrameSetter, // framesetter: The framesetter used to create the frame.
                                                                       CFRangeMake(LStringRange.location, 0), // stringRange: The range, of the attributed string that was used to create the framesetter,
                                                                                                              // that is to be typeset in lines fitted into the frame. If the length portion of the range is
                                                                                                              // set to 0, then the framesetter continues to add lines until it runs out of text or space.
                                                                       LFramePath, // path: A CGPath object that specifies the shape of the frame. The path may be non-rectangular
                                                                                   // in versions of OS X v10.7 or later and versions of iOS 4.2 or later.
                                                                       nil); // frameAttributes: Additional attributes that control the frame filling process can be specified here,
                                                                             // or NULL if there are no such attributes.
                                    if LFrame <> nil then begin // CTFramesetterCreateFrame return NULL if unsuccessful.
                                      try

                                        //init aBreakTextItem
                                        LBreakTextItem := aBreakTextItems[aBreakTextItems.count - 1];
                                        cfRelease(LBreakTextItem.Line);
                                        LBreakTextItem.Line := nil; // << i use this as a flag

                                        //init alines / aLinesCount
                                        Llines := CTFrameGetLines(LFrame); // Return a CFArray object containing the CTLine objects that make up the frame, or, if there are no lines in the frame, an array with no elements.
                                        LLinesCount := CFArrayGetCount(Llines);
                                        if LLinesCount > 0 then begin

                                          //init aline / aMeasuredWidth
                                          Lline := CFArrayGetValueAtIndex(Llines, 0);
                                          LMeasuredWidth := CTLineGetTypographicBounds(Lline, // line: The line whose typographic bounds are calculated.
                                                                                       @LAscent, // ascent: On output, the ascent of the line. This parameter can be set to NULL if not needed.
                                                                                       @LDescent, // descent: On output, the descent of the line. This parameter can be set to NULL if not needed.
                                                                                       nil); // leading: On output, the leading of the line. This parameter can be set to NULL if not needed. (it's look like to be always 0)
                                                                                            // >> return the typographic width of the line. If the line is invalid, this function returns 0.

                                          //init aStringRange
                                          LStringRange := CTLineGetStringRange(Lline); // return a CFRange structure that contains the range over the backing store string that spawned the glyphs

                                          //if their is enalf of place for the text + ellipssis
                                          if (LStringRange.length > 0) and
                                             (compareValue(LMeasuredWidth - CTLineGetTrailingWhitespaceWidth(Lline), 0, TEpsilon.Position) > 0) and
                                             (compareValue(LMeasuredWidth + LEllipsisWidth, LMaxWidth - LLineIndent, TEpsilon.Position) <= 0) then begin

                                            //calculate aMaxLineWidth
                                            LMaxLineWidth := max(LPrevMaxLineWidth, LMeasuredWidth + LLineIndent + LEllipsisWidth);

                                            //update aBreakTextItems.Line
                                            LBreakTextItem.Line := CFRetain(Lline); // Retains a Core Foundation object. we need this because we will later free the aFrame but still need to access the Line

                                            //update aBreakTextItems.text
                                            if LStringRange.length > 0 then begin
                                              LTmpTextAttr := CFAttributedStringCreateWithSubstring(kCFAllocatorDefault, CFAttributedStringRef(LTextAttr), LStringRange); // return A new attributed string whose string and attributes are copied from the specified range of the supplied attributed string. Returns NULL if there was a problem copying the object.
                                              if LTmpTextAttr <> nil then begin
                                                try
                                                  LBreakTextItem.text := CFStringRefToStr(CFAttributedStringGetString(LTmpTextAttr));  // Return An immutable string containing the characters from aStr, or NULL if there was a problem creating the object.
                                                finally
                                                  cfRelease(LTmpTextAttr);
                                                end;
                                              end
                                              else LBreakTextItem.text := '';
                                            end
                                            else LBreakTextItem.text := '';

                                            //update aBreakTextItems.pos & aBreakTextItems.rect
                                            case AHTextAlign of
                                              TTextAlign.Center: begin
                                                                   LBreakTextItem.pos := TpointF.create((LMaxWidth - LMeasuredWidth - LEllipsisWidth - LLineIndent) / 2, LBreakTextItem.pos.y);
                                                                 end;
                                              TTextAlign.Leading: begin
                                                                    LBreakTextItem.pos := TpointF.create(LLineIndent, LBreakTextItem.pos.y);
                                                                  end;
                                              TTextAlign.Trailing: begin
                                                                     LBreakTextItem.pos := TpointF.create(LMaxWidth - LMeasuredWidth - LEllipsisWidth, LBreakTextItem.pos.y);
                                                                   end;
                                            end;
                                            LBreakTextItem.rect := Trectf.Create(TPointF.Create(LBreakTextItem.pos.x,
                                                                                                LBreakTextItem.pos.Y - LAscent),
                                                                                 LMeasuredWidth,
                                                                                 LAscent + LDescent);

                                          end;

                                        end;

                                        //update aBreakTextItem.rect.Width
                                        if LBreakTextItem.Line = nil then LBreakTextItem.rect.Width := 0;

                                        //add the ellipsis line
                                        LEllipsisBreakTextItem := TalBreakTextItem.Create;
                                        try
                                          LEllipsisBreakTextItem.Line := CFRetain(LEllipsisLine); // Retains a Core Foundation object.
                                          LEllipsisBreakTextItem.text := aEllipsisText;
                                          LEllipsisBreakTextItem.isEllipsis := true;
                                          LEllipsisBreakTextItem.pos := TpointF.create(LBreakTextItem.pos.x + LBreakTextItem.rect.Width, LBreakTextItem.pos.y);
                                          LEllipsisBreakTextItem.rect := Trectf.Create(TPointF.Create(LEllipsisBreakTextItem.pos.x,
                                                                                                      LEllipsisBreakTextItem.pos.Y - LAscent), // if aBreakTextItem.Line = nil then aAscent = aAscent of the ellipsis
                                                                                       LEllipsisWidth,
                                                                                       LAscent + LDescent); // if aBreakTextItem.Line = nil then aAscent/aDescent = aAscent/aDescent of the ellipsis
                                          aBreakTextItems.Add(LEllipsisBreakTextItem);
                                        except
                                          ALFreeAndNil(LEllipsisBreakTextItem);
                                          raise;
                                        end;

                                        //delete the last line if not enalf of place
                                        if LBreakTextItem.Line = nil then begin
                                          LMaxLineWidth := max(LPrevMaxLineWidth, LLineIndent + LEllipsisWidth);
                                          aBreakTextItems.Delete(aBreakTextItems.count - 2);
                                        end;

                                      finally
                                        CFRelease(LFrame);
                                      end;
                                    end;

                                  finally
                                    CFRelease(LFrameSetter);
                                  end;
                                end;

                              finally
                                CFRelease(LFramePath);
                              end;

                            finally
                              cfRelease(LEllipsisLine);
                            end;
                          end;

                        finally
                          CFRelease(LEllipsisAttr);
                        end;

                      finally
                        CFRelease(LEllipsisString);
                      end;
                    end;

                  finally
                    CFRelease(LEllipsisFont);
                  end;
                end;

              finally
                CGColorRelease(LEllipsisColor);
              end;

            end;

          end
          else if (aBreakTextItems.Count = LBreakTextItemsStartCount) and  // If no line was added
                  (not AText.IsEmpty) then begin // and the text was not empty

            //init result
            result := True;
            aAllTextDrawed := False;

          end

        finally
          CFRelease(LTextAttr);
        end;

      finally
        CFRelease(LTextString);
      end;

    finally
      CFRelease(LFont);
    end;

  finally
    CGColorRelease(LCGColor);
  end;


  //initialise ARect
  if compareValue(LMaxLineWidth, LMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            LOffset := Floor((aRect.Right - LMaxLineWidth - arect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            aRect.Left := aRect.Left + LOffset;
                            aRect.right := aRect.right - LOffset;
                            for I := LBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                              aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - LOffset;
                              aBreakTextItems[I].rect.Offset(-LOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             aRect.Right := min(aRect.Right, aRect.Left + LMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              LOffset := Floor(aRect.Right - LMaxLineWidth - arect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              aRect.Left := aRect.Left + LOffset;
                              for I := LBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                                aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - LOffset;
                                aBreakTextItems[I].rect.Offset(-LOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(LTotalLinesHeight, LMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            LOffset := (aRect.bottom - LTotalLinesHeight - arect.top) / 2;
                            aRect.top := aRect.top + LOffset;
                            aRect.bottom := aRect.bottom - LOffset;
                          end;
       TTextAlign.Leading: begin
                             aRect.bottom := min(aRect.bottom, aRect.top + LTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              LOffset := aRect.bottom - LTotalLinesHeight - arect.top;
                              aRect.top := aRect.top + LOffset;
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
var LTotalLines: integer;
    LAllTextDrawed: boolean;
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
                        LTotalLines, // var aTotalLines: integer;
                        LAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                        aFirstLineIndent, // const aFirstLineIndent: TpointF;// kCTParagraphStyleSpecifierFirstLineHeadIndent must also have been set with aFirstLineIndent.x in aTextAttr
                        aLineSpacing, // const aLineSpacing: single = 0; // kCTParagraphStyleSpecifierLineSpacingAdjustment must also have been set with aLineSpacing in aTextAttr
                        aEllipsisText, // const aEllipsisText: string = '…';
                        aEllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                        aEllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                        aMaxlines); // const aMaxlines: integer = 0): boolean; // // return true if text was breaked in several lines (truncated or not)
end;
{$ENDIF}


{*****************************************}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
Procedure ALGetTextMetrics(const aFontSize: single;
                           const aFontStyle: TFontStyles;
                           const aFontName: String;
                           var aAscent:Single; // << return aAscent in negative (like in android)
                           var aDescent:Single);
var LLayout: TTextLayout;
begin
  LLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    LLayout.BeginUpdate;
    LLayout.Text := '^_'; // << seam that aLayout.TextHeight will be the same for all characters so doesn't matter what we set here
    LLayout.Font.Family := aFontName;
    LLayout.Font.Style := aFontStyle;
    LLayout.Font.Size := aFontSize;
    LLayout.EndUpdate;
    aAscent := 0;  // << unfortunatly TTextlayout don't gave any ascent/descent ( https://quality.embarcadero.com/browse/RSP-16645
                   // << also the canvas.FillText don't ask the base line of the text but the top/left corner so it's better to say
                   // << that ascent = 0 and descent = height of the font
    aDescent := LLayout.TextHeight;
  finally
    LLayout.Free;
  end;
end;
{$ENDIF}

{*****************************************}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
function ALbreakText(const aFontSize: single;
                     const aFontStyle: TFontStyles;
                     const aFontName: String;
                     const atext: String;
                     const aMaxWidth: Single;
                     var aMeasuredWidth: Single): integer;
var LLayout: TTextLayout;
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
  LLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    LLayout.BeginUpdate;
    LLayout.Font.Family := aFontName;
    LLayout.Font.Style := aFontStyle;
    LLayout.Font.Size := aFontSize;
    LLayout.MaxSize := Tpointf.Create(aMaxWidth, 65535);
    LLayout.Trimming := TTextTrimming.Character;
    LLayout.VerticalAlign := TTextAlign.Leading;
    LLayout.HorizontalAlign := TTextAlign.Leading;
    LLayout.WordWrap := False;
    if (atext <> '') and (atext.Chars[atext.Length - 1].IsLowSurrogate) then LLayout.Text := atext + ' '  // << https://quality.embarcadero.com/browse/RSP-16649
    else LLayout.Text := atext;
    LLayout.EndUpdate;
    aMeasuredWidth := LLayout.TextWidth;
    result := LLayout.PositionAtPoint(TpointF.Create(aMeasuredWidth - Tepsilon.Position,0)); // << on macos this function is buggy and you need to update fmx.canvas.mac (see https://quality.embarcadero.com/browse/RSP-16648 and https://quality.embarcadero.com/browse/RSP-16649)
                                                                                             // << - Tepsilon.Position because if PositionAtPoint = exactly aMeasuredWidth then it's return -1
    result := min(atext.Length, result); // remove the extra space we added because of https://quality.embarcadero.com/browse/RSP-16649
    if result < 0 then result := 0;
  finally
    LLayout.Free;
  end;
end;
{$ENDIF}

{*****************************************}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
constructor TALBreakTextItem.Create;
begin
  inherited;
  Line := '';
  isEllipsis := False;
end;
{$ENDIF}

{*****************************************}
{$IF defined(MSWINDOWS) or defined(ALMacOS)}
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

var LBreakTextItemsStartCount: integer;
    LBreakTextItem: TALBreakTextItem;
    LNumberOfChars: integer;
    LSaveNumberOfChars: integer;
    LSaveNumberOfCharsIsAccurate: Boolean;
    LLine: String;
    LLineIndent: Single;
    LEllipsisLine: String;
    LEllipsisLineLn: single;
    LEllipsisLinePos: TpointF;
    LEllipsisLineRect: TrectF;
    LMaxWidth: single;
    LMaxHeight: single;
    LMaxLineWidth: single;
    LLineHeight: single;
    LTotalLinesHeight: single;
    LChar: Char;
    LTextLn: integer;
    LTextIdx: integer;
    LCurrLineY: single;
    LAscent, LDescent: Single;
    LMeasuredWidth: Single;
    LOffset: single;
    LLineEndWithBreakLine: Boolean;
    I, J: integer;

  {~~~~~~~~~~~~~~~~~~~~~~}
  procedure _initEllipsis;
  begin
    if LEllipsisLine = '' then begin
      //-----
      if aEllipsisText = '' then LEllipsisLine := '…'
      else LEllipsisLine := aEllipsisText;
      //-----
      ALbreakText(aFontSize, // const aFontSize: single;
                  aEllipsisFontStyle, // const aFontStyle: TFontStyles;
                  aFontName, // const aFontName: String;
                  LEllipsisLine, // const atext: String;
                  65535, // const aMaxWidth: Single;
                  LEllipsisLineLn); // var aMeasuredWidth: Single)
      //-----
      case AHTextAlign of
        TTextAlign.Center: begin
                             LEllipsisLinePos := TpointF.create((LMaxWidth - LEllipsisLineLn - LLineIndent) / 2, LCurrLineY);
                           end;
        TTextAlign.Leading: begin
                              LEllipsisLinePos := TpointF.create(LLineIndent, LCurrLineY);
                            end;
        TTextAlign.Trailing: begin
                               LEllipsisLinePos := TpointF.create(LMaxWidth - LEllipsisLineLn, LCurrLineY);
                             end;
      end;
      LEllipsisLineRect := Trectf.Create(TPointF.Create(LEllipsisLinePos.x,
                                                        LEllipsisLinePos.Y - (-1*LAscent)),
                                         LEllipsisLineLn,
                                         (-1*LAscent) + LDescent);
      //-----
    end;
  end;

begin

  //init result
  result := false;
  aAllTextDrawed := True;

  //init aBreakTextItemsStartCount
  LBreakTextItemsStartCount := aBreakTextItems.Count;

  //init aMaxWidth / aMaxHeight / aMaxLineWidth / aTotalLinesHeight
  if aRect.Width > 65535 then aRect.Width := 65535;
  if aRect.height > 65535 then aRect.height := 65535;
  LMaxWidth := ARect.width;
  LMaxHeight := ARect.Height;
  LMaxLineWidth := 0;
  LTotalLinesHeight := 0;

  //init ATextIdx / ATextLn
  LTextIdx := 0;
  LTextLn := AText.length;

  //init metics / aCurrLineY / aLineHeight
  ALGetTextMetrics(aFontSize,
                   aFontStyle,
                   aFontName,
                   LAscent,
                   LDescent);
  LCurrLineY := aFirstLineIndent.y + (-1*LAscent); // aMetrics.top and aMetrics.ascent are always returned in negative value
  aTotalLines := 0;
  LLineHeight := LDescent + aLineSpacing + (-1*LAscent);

  //init aEllipsisLine
  LEllipsisLine := '';
  LEllipsisLineLn := 0;

  //init aLineIndent
  LLineIndent := aFirstLineIndent.x;

  //if we have at least enalf of height to write the 1rt row
  if comparevalue(aFirstLineIndent.y + LDescent + (-1*LAscent),LMaxHeight,Tepsilon.position) <= 0 then begin

    //loop still their is some chars
    while LTextIdx < LTextLn do begin

      // init aline
      I := aText.indexOf(#13 {c}, LTextIdx{start}); // find if their is some #13 (MSWINDOWS linebreak = #13#10)
      J := aText.indexOf(#10 {c}, LTextIdx{start}); // find if their is some #10 (UNIX linebreak = #10)
      if (I >= 0) and (J >= 0) then I := min(I,J)
      else I := max(I, J);
      if I = LTextIdx then begin
        LLine := '';
        LLineEndWithBreakLine := True;
        result := true;
      end
      else if I > 0 then begin
        LLine := aText.substring(LTextIdx{startIndex}, I - LTextIdx{length}); // skip the $0D/$0A
        LLineEndWithBreakLine := True;
        result := true;
      end
      else begin
        LLine := aText.substring(LTextIdx{startIndex});
        LLineEndWithBreakLine := False;
      end;

      //calculate the number of char in the current line (this work good also if aline is empty)
      LNumberOfChars := ALbreakText(aFontSize,
                                    aFontStyle,
                                    aFontName,
                                    LLine {text},
                                    LMaxWidth - LLineIndent, {amaxWidth}
                                    LMeasuredWidth {measuredWidth});

      //init result
      if LNumberOfChars < LLine.length then result := true;

      //if we need to break the text
      if (LNumberOfChars < LLine.length) or // << if aNumberOfChars < aLine.length it's evident we will need to break the text
         (                                                                                                // <<
          (LLineEndWithBreakLine) and                                                                     // <<
          (aTrimming <> TTextTrimming.None) and                                                           // <<
          (                                                                                               // << we need this check to add the ellipsis on the last line
           (not aWordWrap) or                                                                             // << when the last line finish by a break line (#13#10)
           ((compareValue(LCurrLineY + LLineHeight + LDescent, LMaxHeight, Tepsilon.position) > 0) or     // <<
            ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1)))                                         // <<
          )                                                                                               // <<
         )                                                                                                // <<
      then begin

        //if not aWordWrap
        if not aWordWrap then begin
          aAllTextDrawed := False; // aNumberOfChars < aLine.length so in anycase we will not draw all the text
          case aTrimming of
            TTextTrimming.None: begin
                                  if LNumberOfChars > 0 then
                                    LLine := LLine.substring(0, LNumberOfChars);
                                end;
            TTextTrimming.Character: begin
                                       //-----
                                       _initEllipsis;
                                       //-----
                                       if (LNumberOfChars < LLine.length) then dec(LNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                    // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                    // we considere that aEllipsisText is only one char
                                       while LNumberOfChars > 0 do begin
                                         LLine := LLine.substring(0, LNumberOfChars);
                                         LNumberOfChars := ALbreakText(aFontSize,
                                                                       aFontStyle,
                                                                       aFontName,
                                                                       LLine {text},
                                                                       LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                                       LMeasuredWidth {measuredWidth});
                                         if LNumberOfChars >= LLine.length then break;
                                       end;
                                       //-----
                                     end;
            TTextTrimming.Word: begin
                                  //-----
                                  _initEllipsis;
                                  //-----
                                  LSaveNumberOfChars := LNumberOfChars;
                                  LSaveNumberOfCharsIsAccurate := False;
                                  while LNumberOfChars > 0 do begin
                                    //----
                                    if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                                      LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                                    end
                                    //----
                                    else if LNumberOfChars >= 2 then begin
                                      LChar := LLine.chars[LNumberOfChars-2];
                                      if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                                        dec(LNumberOfChars);
                                        continue;
                                      end;
                                      LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
                                    end
                                    //----
                                    else begin
                                      LNumberOfChars := LSaveNumberOfChars;
                                      if (not LSaveNumberOfCharsIsAccurate) and (LNumberOfChars < LLine.length) then dec(LNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                                                          // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                                                          // we considere that aEllipsisText is only one char
                                      while LNumberOfChars > 0 do begin
                                        LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                                        LNumberOfChars := ALbreakText(aFontSize,
                                                                      aFontStyle,
                                                                      aFontName,
                                                                      LLine {text},
                                                                      LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                                      LMeasuredWidth {measuredWidth});
                                        if LNumberOfChars >= LLine.length then break;
                                      end;
                                      break;
                                    end;
                                    //----
                                    LNumberOfChars := ALbreakText(aFontSize,
                                                                  aFontStyle,
                                                                  aFontName,
                                                                  LLine {text},
                                                                  LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                                  LMeasuredWidth {measuredWidth});
                                    if LNumberOfChars >= LLine.length then break
                                    else begin
                                      LSaveNumberOfChars:= LNumberOfChars;
                                      LSaveNumberOfCharsIsAccurate := True;
                                    end;
                                    //----
                                  end;
                                end;
          end;
        end

        //if aWordWrap
        else begin

          //We are at the last line and aTrimming <> TTextTrimming.None
          if ((compareValue(LCurrLineY + LLineHeight + LDescent, LMaxHeight, Tepsilon.position) > 0) or
              ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1))) and
             (aTrimming <> TTextTrimming.None) then begin

            //-----
            aAllTextDrawed := False; // if we are at the last line then in anycase we will not draw all the text
            _initEllipsis;
            //-----
            LSaveNumberOfChars := LNumberOfChars;
            LSaveNumberOfCharsIsAccurate := False;
            while LNumberOfChars > 0 do begin
              //----
              if (LNumberOfChars >= LLine.length) then begin // if (aNumberOfChars >= aLine.length) then we are here because of manual linebreak
                LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
              end
              //----
              else if (aTrimming = TTextTrimming.Word) and (LNumberOfChars >= 2) then begin
                LChar := LLine.chars[LNumberOfChars-2];
                if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                  dec(LNumberOfChars);
                  continue;
                end;
                LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish with space
              end
              //----
              else begin
                LNumberOfChars := LSaveNumberOfChars;
                if (not LSaveNumberOfCharsIsAccurate) and (LNumberOfChars < LLine.length) then dec(LNumberOfChars); // (aNumberOfChars < aLine.length) to know that we are not here
                                                                                                                    // because of manual linebreak and dec(aNumberOfChars) because initialy
                                                                                                                    // we considere that aEllipsisText is only one char
                while LNumberOfChars > 0 do begin
                  LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                  LNumberOfChars := ALbreakText(aFontSize,
                                                aFontStyle,
                                                aFontName,
                                                LLine {text},
                                                LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                                LMeasuredWidth {measuredWidth});
                  if LNumberOfChars >= LLine.length then break;
                end;
                break;
              end;
              //----
              LNumberOfChars := ALbreakText(aFontSize,
                                            aFontStyle,
                                            aFontName,
                                            LLine {text},
                                            LMaxWidth - LEllipsisLineLn - LLineIndent, {maxWidth}
                                            LMeasuredWidth {measuredWidth});
              if LNumberOfChars >= LLine.length then break
              else begin
                LSaveNumberOfChars:= LNumberOfChars;
                LSaveNumberOfCharsIsAccurate := True;
              end;
              //----
            end;

          end

          //We are not at the last line or aTrimming = TTextTrimming.None
          else begin

            //We are at the last line and aTrimming = TTextTrimming.None and more line available
            if (aTrimming = TTextTrimming.None) and
               ((compareValue(LCurrLineY + LLineHeight + LDescent, LMaxHeight, Tepsilon.position) > 0) or
                ((aMaxLines > 0) and (aTotalLines >= aMaxLines - 1))) then aAllTextDrawed := False;

            //Cut the line
            LSaveNumberOfChars := LNumberOfChars;
            if LNumberOfChars < LLine.length then inc(LNumberOfChars); // in case the space separator is just after aNumberOfChars
            while LNumberOfChars > 0 do begin
              //-----
              if LNumberOfChars >= 2 then begin
                LChar := LLine.chars[LNumberOfChars-1];
                if (not LChar.IsWhiteSpace) or (LChar.ToUCS4Char = $00A0{No-break Space}) then begin
                  dec(LNumberOfChars);
                  continue;
                end;
                LLine := LLine.substring(0, LNumberOfChars-1); // length of aLine is now aNumberOfChars - 1 and finish just before the space
              end
              //-----
              else begin
                LNumberOfChars := LSaveNumberOfChars;
                if compareValue(LLineIndent, 0, TEpsilon.position) > 0 then LNumberOfChars := 0;
                while LNumberOfChars > 0 do begin
                  LLine := LLine.substring(0, LNumberOfChars); // length of aLine is now aNumberOfChars
                  LNumberOfChars := ALbreakText(aFontSize,
                                                aFontStyle,
                                                aFontName,
                                                LLine {text},
                                                LMaxWidth - LLineIndent, {maxWidth}
                                                LMeasuredWidth {measuredWidth});
                  if LNumberOfChars >= LLine.length then break;
                end;
                break;
              end;
              //-----
              LNumberOfChars := ALbreakText(aFontSize,
                                            aFontStyle,
                                            aFontName,
                                            LLine {text},
                                            LMaxWidth - LLineIndent, {maxWidth}
                                            LMeasuredWidth {measuredWidth});
              if LNumberOfChars >= LLine.length then begin
                inc(LNumberOfChars); // to skip the separator
                break;
              end
              else LSaveNumberOfChars:= LNumberOfChars;
              //-----
            end;

          end;

        end;

      end;

      //init aMaxLineWidth
      LMaxLineWidth := max(LMaxLineWidth, LMeasuredWidth + LEllipsisLineLn + LLineIndent);

      // update aTotalLinesHeight
      LTotalLinesHeight := LCurrLineY + LDescent;

      // their is not enalf of place to write at least one char or
      // we are on the #13/#10
      // NOTE: we need to remove the breakline because for exemple we have
      // coco sur le cocotier#13#10
      // then aline will be equal to
      // coco sur le cocotier
      // we draw this line, and then we increate aCurrLineY := aCurrLineY + aLineHeight;
      // so it's mean the #13#10 was already taking in account, so we must delete it
      if LNumberOfChars <= 0 then begin
        if (LTextIdx + 1 < LTextLn) and
           (aText.Chars[LTextIdx] = #13) and
           (aText.Chars[LTextIdx + 1] = #10) then LTextIdx := LTextIdx + 2 // (MSWINDOWS linebreak = #13#10)
        else if (LTextIdx < LTextLn) and
                (aText.Chars[LTextIdx] = #10) then LTextIdx := LTextIdx + 1 // (UNIX linebreak = #10)
        else if (LTextIdx < LTextLn) and
                ((aText.Chars[LTextIdx].IsWhiteSpace) and
                 (aText.Chars[LTextIdx].ToUCS4Char <> $00A0{No-break Space})) then LTextIdx := LTextIdx + 1 // (white space) if we don't have place to write
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
        else if compareValue(LLineIndent, 0, Tepsilon.Position) <= 0 then begin // if aLineIndent > 0 then maybe we don't have enalf of place to write one char because of the aLineIndent.
          LTextIdx := LTextIdx + 1; // skip the current char
          if (LTextIdx < LTextLn) and
             (aText.Chars[LTextIdx].IsLowSurrogate) then inc(LTextIdx);
        end;
        LCurrLineY := LCurrLineY + LLineHeight; // go to next line
        inc(aTotalLines);
        LLineIndent := 0;
        if (not aWordWrap) or
           ((aMaxLines > 0) and (aTotalLines >= aMaxLines)) or
           (compareValue(LCurrLineY + LDescent, LMaxHeight, TEpsilon.position) > 0) then break
        else continue;
      end
      else begin
        LTextIdx := LTextIdx + LNumberOfChars;
        if (LTextIdx + 1 < LTextLn) and
           (aText.Chars[LTextIdx] = #13) and
           (aText.Chars[LTextIdx + 1] = #10) then LTextIdx := LTextIdx + 2 // (MSWINDOWS linebreak = #13#10)
        else if (LTextIdx < LTextLn) and
                (aText.Chars[LTextIdx] = #10) then LTextIdx := LTextIdx + 1;
      end;

      //init aBreakedText
      LBreakTextItem := TalBreakTextItem.Create;
      try

        //update aBreakTextItem
        LBreakTextItem.line := LLine;
        case AHTextAlign of
          TTextAlign.Center: begin
                               LBreakTextItem.pos := TpointF.create((LMaxWidth - LMeasuredWidth - LEllipsisLineLn - LLineIndent) / 2, LCurrLineY);
                             end;
          TTextAlign.Leading: begin
                                LBreakTextItem.pos := TpointF.create(LLineIndent, LCurrLineY);
                              end;
          TTextAlign.Trailing: begin
                                 LBreakTextItem.pos := TpointF.create(LMaxWidth - LMeasuredWidth - LEllipsisLineLn, LCurrLineY);
                               end;
        end;
        LBreakTextItem.rect := Trectf.Create(TPointF.Create(LBreakTextItem.pos.x,
                                                            LBreakTextItem.pos.Y - (-1*LAscent)),
                                             LMeasuredWidth,
                                             (-1*LAscent) + LDescent);

        //update aEllipsisLinePos / aEllipsisLinerect
        if LEllipsisLine <> '' then begin
          LEllipsisLinePos := TpointF.Create(LBreakTextItem.pos.x + LMeasuredWidth, LCurrLineY);
          LEllipsisLineRect := Trectf.Create(TPointF.Create(LBreakTextItem.pos.x + LMeasuredWidth,
                                                            LBreakTextItem.pos.Y - (-1*LAscent)),
                                             LEllipsisLineLn,
                                             (-1*LAscent) + LDescent);
        end;

        // update aBreakTextItems
        aBreakTextItems.Add(LBreakTextItem);

      except
        ALFreeAndNil(LBreakTextItem);
        raise;
      end;

      //update aCurrLineY
      LCurrLineY := LCurrLineY + LLineHeight;
      inc(aTotalLines);

      //update aLineIndent
      LLineIndent := 0;

      // stop if not aWordWrap or after maxheight
      if (not aWordWrap) or
         ((aMaxLines > 0) and (aTotalLines >= aMaxLines)) or
         (compareValue(LCurrLineY + LDescent, LMaxHeight, TEpsilon.position) > 0) then break;

      //add the last empty row if their is one
      if (LTextIdx >= LTextLn) and LLineEndWithBreakLine and (LEllipsisLine = '') then begin

        //init aBreakedText
        LBreakTextItem := TalBreakTextItem.Create;
        try

          //update aBreakTextItem
          LBreakTextItem.line := '';
          case AHTextAlign of
            TTextAlign.Center: begin
                                 LBreakTextItem.pos := TpointF.create((LMaxWidth - LEllipsisLineLn - LLineIndent) / 2, LCurrLineY);
                               end;
            TTextAlign.Leading: begin
                                  LBreakTextItem.pos := TpointF.create(LLineIndent, LCurrLineY);
                                end;
            TTextAlign.Trailing: begin
                                   LBreakTextItem.pos := TpointF.create(LMaxWidth - LEllipsisLineLn, LCurrLineY);
                                 end;
          end;
          LBreakTextItem.rect := Trectf.Create(TPointF.Create(LBreakTextItem.pos.x,
                                                              LBreakTextItem.pos.Y - (-1*LAscent)),
                                               0,
                                               (-1*LAscent) + LDescent);

          // update aBreakTextItems
          aBreakTextItems.Add(LBreakTextItem);

        except
          ALFreeAndNil(LBreakTextItem);
          raise;
        end;

        //update aCurrLineY
        LCurrLineY := LCurrLineY + LLineHeight;
        inc(aTotalLines);

      end;

    end;

  end
  else result := true;

  //add the end ellipsis
  if LEllipsisLine <> '' then begin
    LBreakTextItem := TalBreakTextItem.Create;
    try
      LBreakTextItem.line := LEllipsisLine;
      LBreakTextItem.pos := LEllipsisLinePos;
      LBreakTextItem.rect := LEllipsisLineRect;
      LBreakTextItem.isEllipsis := True;
      aBreakTextItems.Add(LBreakTextItem);
    except
      ALFreeAndNil(LBreakTextItem);
      raise;
    end;
  end;

  //initialise ARect
  if compareValue(LMaxLineWidth, LMaxWidth, Tepsilon.Position) < 0 then begin
    case AHTextAlign of
       TTextAlign.Center: begin
                            LOffset := Floor((aRect.Right - LMaxLineWidth - arect.Left) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                            aRect.Left := aRect.Left + LOffset;
                            aRect.right := aRect.right - LOffset;
                            for I := LBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                              aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - LOffset;
                              aBreakTextItems[I].rect.Offset(-LOffset, 0);
                            end;
                          end;
       TTextAlign.Leading: begin
                             aRect.Right := min(aRect.Right, aRect.Left + LMaxLineWidth);
                           end;
       TTextAlign.Trailing: begin
                              LOffset := Floor(aRect.Right - LMaxLineWidth - arect.Left); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                              aRect.Left := aRect.Left + LOffset;
                              for I := LBreakTextItemsStartCount to aBreakTextItems.Count - 1 do begin
                                aBreakTextItems[I].pos.X := aBreakTextItems[I].pos.X - LOffset;
                                aBreakTextItems[I].rect.Offset(-LOffset, 0);
                              end;
                            end;
    end;
  end;
  if compareValue(LTotalLinesHeight, LMaxHeight, Tepsilon.Position) < 0 then begin
    case AVTextAlign of
       TTextAlign.Center: begin
                            LOffset := (aRect.bottom - LTotalLinesHeight - arect.top) / 2;
                            aRect.top := aRect.top + LOffset;
                            aRect.bottom := aRect.bottom - LOffset;
                          end;
       TTextAlign.Leading: begin
                             aRect.bottom := min(aRect.bottom, aRect.top + LTotalLinesHeight);
                           end;
       TTextAlign.Trailing: begin
                              LOffset := aRect.bottom - LTotalLinesHeight - arect.top;
                              aRect.top := aRect.top + LOffset;
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
{$ZEROBASEDSTRINGS OFF}
function  ALDrawMultiLineText(const aText: String; // support only those html tags :
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
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _getInfosFromTag(const aTag: String; // color="#ffffff" id="xxx"
                             const aSpanIds: TalStringlistU;
                             const aFontColors: Tlist<TalphaColor>);
  var LParamList: TAlStringListU;
      LcolorInt: integer;
      S1: String;
  begin

    if aTag = '' then begin
      aSpanIds.Add('');
      if aFontColors.Count > 0 then aFontColors.Add(aFontColors[aFontColors.Count - 1])
       else aFontColors.Add(aOptions.FontColor);
      exit;
    end;

    LParamList := TALStringListU.Create;
    try

      ALExtractHeaderFieldsWithQuoteEscapedU([' ', #9, #13, #10],
                                             [' ', #9, #13, #10],
                                             ['"', ''''],
                                             PChar(aTag),
                                             LParamList,
                                             False,
                                             True{StripQuotes});

      aSpanIds.Add(LParamList.Values['id']);

      S1 := LParamList.Values['color'];
      if S1 <> '' then begin

        if S1[low(S1)] = '#' then begin
          S1[low(S1)] := '$';
          if length(S1) = 7 then insert('ff', S1, 2); // $ffffffff
          if not ALTryStrTointU(S1, LcolorInt) then begin
            if aFontColors.Count > 0 then aFontColors.Add(aFontColors[aFontColors.Count - 1])
            else aFontColors.Add(aOptions.FontColor);
          end
          else aFontColors.Add(TalphaColor(LcolorInt));
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
      LParamList.Free;
    end;

  end;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  procedure _getInfosFromImg(const aTag: String; // src="xxx"
                             var aSrc: String);
  var LParamList: TAlStringListU;
  begin

    if aTag = '' then begin
      aSrc := '';
      exit;
    end;

    LParamList := TALStringListU.Create;
    try

      ALExtractHeaderFieldsWithQuoteEscapedU([' ', #9, #13, #10],
                                             [' ', #9, #13, #10],
                                             ['"', ''''],
                                             PChar(aTag),
                                             LParamList,
                                             False,
                                             True{StripQuotes});

      aSrc := LParamList.Values['src'];

    finally
      LParamList.Free;
    end;

  end;

var {$IF defined(ANDROID)}
    LBitmap: Jbitmap;
    LImg: Jbitmap;
    LPaint: JPaint;
    LTypeface: JTypeface;
    LCanvas: Jcanvas;
    LStyle: integer;
    JStr1, JStr2: JString;
    {$ENDIF}
    {$IF defined(IOS)}
    LBitmapSurface: TbitmapSurface;
    LImg: CGImageRef;
    LColorSpace: CGColorSpaceRef;
    LContext: CGContextRef;
    LStyle: TfontStyles;
    LWhiteSpace: Boolean;
    {$ENDIF}
    {$IF defined(MSWINDOWS) or defined(ALMacOS)}
    LStyle: TfontStyles;
    LImg: Tbitmap;
    {$ENDIF}
    LBreakedTextItems: TALBreakTextItems;
    LBreakedTextItem: TALBreakTextItem;
    LBreakedTextItemsCount: integer;
    LCurrText: String;
    LCurrImgSrc: String;
    LTag: String;
    LBold: integer;
    LItalic: Integer;
    LMaxWidth: single;
    LMaxHeight: Single;
    LFontColors: Tlist<TalphaColor>;
    LFontColor: TalphaColor;
    LSpanIDs: TalStringlistU;
    LSpanID: String;
    LFirstLineIndent: TpointF;
    LTmpRect: TrectF;
    LTotalLines: integer;
    LTmpTotalLines: integer;
    LTmpTextBreaked: Boolean;
    LTmpAllTextDrawed: Boolean;
    LCurrentLineY: single;
    LOffset: single;
    P1, P2: integer;
    i, j: integer;

begin

  {$IF defined(ALUseTexture)}
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
  //create LPaint
  LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setSubpixelText(true); // Enabling this flag causes glyph advances to be computed with subpixel accuracy.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.
  LPaint.setTextSize(aOptions.FontSize);
  {$ENDIF}

  {$IF defined(IOS)}
  //init the color space
  LColorSpace := CGColorSpaceCreateDeviceRGB;  // Return Value: A device-dependent RGB color space. You are responsible for releasing this object by
  if LColorSpace = nil then begin ARect.Width := 0; ARect.Height := 0; exit(nil); end;  // calling CGColorSpaceRelease. If unsuccessful, returns NULL.
  try
  {$ENDIF}

    //init local var
    {$IFDEF IOS}
    LWhiteSpace := False;
    {$ENDIF}
    LFirstLineIndent := aOptions.FirstLineIndent;
    LMaxWidth := 0;
    LMaxHeight := 0;
    LTotalLines := 0;
    LBold := 0;
    LItalic := 0;
    LFontColors := Tlist<TalphaColor>.create;
    LSpanIDs := TalStringlistU.create;
    LBreakedTextItems := TalBreakTextItems.Create(true{aOwnsObjects});
    try

      //loop on all the html elements
      P1 := 1;
      while P1 <= length(aText) do begin


        /////////////////////////////////////
        //if the text contain html elements//
        /////////////////////////////////////
        if aOptions.TextIsHtml then begin

          //extract LTag / LCurrText
          if aText[P1] = '<' then begin

            //-----
            LCurrImgSrc := '';
            LCurrText := '';
            P2 := AlposExU('>', aText, P1+1); // blablabla <font color="#ffffff">bliblibli</font> blobloblo
                                              //           ^P1                  ^P2
            if P2 <= 0 then break;
            LTag := AlCopyStrU(aText, P1, P2 - P1 + 1); // <font color="#ffffff">
            P1 := P2 + 1; // blablabla <font color="#ffffff">bliblibli</font> blobloblo
                          //                                 ^P1

            //-----
            if (alposU('<b ', LTag) = 1) or
               (LTag = '<b>') then begin
              _getInfosFromTag(AlcopyStrU(LTag, 4, length(LTag) - 4), LSpanIDs, LFontColors);
              inc(LBold);
            end
            else if LTag = '</b>' then begin
              if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
              if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
              dec(LBold);
            end

            //-----
            else if (alposU('<img ', LTag) = 1) or
                    (LTag = '<img/>') then begin // <img src="xxx">
              _getInfosFromImg(AlcopyStrU(LTag, 6, length(LTag) - 6), LCurrImgSrc);
              LCurrText := '⬛';
            end

            //-----
            else if (alposU('<i ', LTag) = 1) or
                    (LTag = '<i>') then begin
              _getInfosFromTag(AlcopyStrU(LTag, 4, length(LTag) - 4), LSpanIDs, LFontColors);
              inc(LItalic)
            end
            else if LTag = '</i>' then begin
              if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
              if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
              dec(LItalic)
            end

            //-----
            else if (alposU('<font ', LTag) = 1) or
                    (LTag = '<font>')  then begin   // <font color="#ffffff">
              _getInfosFromTag(AlcopyStrU(LTag, 7, length(LTag) - 7), LSpanIDs, LFontColors);
            end
            else if LTag = '</font>' then begin
              if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
              if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
            end

            //-----
            else if (alposU('<span ', LTag) = 1) or
                    (LTag = '<span>') then begin // <span id="xxx">
              _getInfosFromTag(AlcopyStrU(LTag, 7, length(LTag) - 7), LSpanIDs, LFontColors);
            end
            else if LTag = '</span>' then begin
              if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
              if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
            end;

            //-----
            if LCurrImgSrc = '' then continue;

          end
          else begin

            LCurrImgSrc := '';
            P2 := AlposExU('<', aText, P1);  // blablabla <font color="#ffffff">bliblibli</font> blobloblo
                                             //                                 ^P1      ^P2
            if P2 <= 0 then P2 := Maxint;
            LCurrText := AlcopyStrU(aText, P1, P2 - P1);  // blablabla
            LCurrText := ALStringReplaceU(LCurrText, '&gt;', '>', [rfReplaceALL]);
            LCurrText := ALStringReplaceU(LCurrText, '&lt;', '<', [rfReplaceALL]);
            {$IFDEF IOS}
            //because of this http://stackoverflow.com/questions/41334425/ctframesettercreateframe-and-kctparagraphstylespecifierfirstlineheadindent
            if LWhiteSpace then LCurrText := ' ' + LCurrText;
            if (P2 <= length(aText) - 3) and
               (aText[P2 + 1] = 'i') and
               (aText[P2 + 2] = 'm') and
               (aText[P2 + 3] = 'g') then begin
              LWhiteSpace := False;
            end
            else if (P2 > 1) and
                    (P2 <= length(aText)) and
                    (aText[P2 - 1].IsWhiteSpace) then begin
              setlength(LCurrText, length(LCurrText) - 1);
              LWhiteSpace := True;
            end
            else LWhiteSpace := False;
            {$ENDIF}

            P1 := P2; // blablabla <font color="#ffffff">bliblibli</font> blobloblo
                      //                                          ^P1
          end;

        end


        ///////////////////////////
        //if the text is NOT html//
        ///////////////////////////
        else begin
          LCurrText := aText;
          P1 := Maxint;
        end;


        //////////////////////
        //draw the curr text//
        //////////////////////
        if LCurrText <> '' then begin

          //LFontColor
          if LFontColors.Count > 0 then LFontColor := LFontColors[LFontColors.Count - 1]
          else LFontColor := aOptions.FontColor;

          //LSpanID
          if LSpanIDs.Count > 0 then LSpanID := LSpanIDs[LSpanIDs.Count - 1]
          else LSpanID := '';

          //LStyle
          {$IF defined(ANDROID)}
          if ((TFontStyle.fsBold in aOptions.FontStyle) or (LBold > 0)) and
             ((TFontStyle.fsItalic in aOptions.FontStyle) or (LItalic > 0)) then LStyle := TJTypeface.JavaClass.BOLD_ITALIC
          else if ((TFontStyle.fsBold in aOptions.FontStyle) or (LBold > 0)) then LStyle := TJTypeface.JavaClass.BOLD
          else if ((TFontStyle.fsItalic in aOptions.FontStyle) or (LItalic > 0)) then LStyle := TJTypeface.JavaClass.ITALIC
          else LStyle := TJTypeface.JavaClass.NORMAL;
          {$ELSE}
          if ((TFontStyle.fsBold in aOptions.FontStyle) or (LBold > 0)) and
             ((TFontStyle.fsItalic in aOptions.FontStyle) or (LItalic > 0)) then LStyle := [TFontStyle.fsBold, TFontStyle.fsItalic]
          else if ((TFontStyle.fsBold in aOptions.FontStyle) or (LBold > 0)) then LStyle := [TFontStyle.fsBold]
          else if ((TFontStyle.fsItalic in aOptions.FontStyle) or (LItalic > 0)) then LStyle := [TFontStyle.fsItalic]
          else LStyle := [];
          {$ENDIF}

          //loop style we draw all the text or at least the ellipsis
          while True do begin

            //init LPaint
            {$IF defined(ANDROID)}
            LPaint.setColor(integer(LFontColor));
            JStr1 := StringToJString(aOptions.FontName); // << https://quality.embarcadero.com/browse/RSP-14187
            LTypeface := TJTypeface.JavaClass.create(JStr1, LStyle);
            LPaint.setTypeface(LTypeface);
            LTypeface := nil;
            JStr1 := nil;
            {$ENDIF}

            //init LTmpRect / LBreakedTextItemsCount
            LTmpRect := ARect;
            LTmpRect.Width := LTmpRect.Width - aOptions.Padding.Left - aOptions.Padding.right;
            LTmpRect.Height := LTmpRect.Height - aOptions.Padding.top - aOptions.Padding.bottom;
            LBreakedTextItemsCount := LBreakedTextItems.Count;

            //break the text
            {$IF defined(ANDROID)}
            JStr1 := StringtoJString(LCurrText); // << https://quality.embarcadero.com/browse/RSP-14187
            JStr2 := StringtoJString(aOptions.EllipsisText); // << https://quality.embarcadero.com/browse/RSP-14187
            LTmpTextBreaked := ALBreakText(LPaint, // const aPaint: JPaint;
                                           LTmpRect, // var ARect: TRectF;
                                           JStr1, // const AText: JString;
                                           aOptions.WordWrap, //const aWordWrap: Boolean;
                                           TTextAlign.Leading, TTextAlign.Leading, //const AHTextAlign, AVTextAlign: TTextAlign;
                                           aOptions.Trimming, // const aTrimming: TTextTrimming;
                                           LBreakedTextItems, // var aBreakedTexts: Tarray<Tpair<JString, TpointF>>);
                                           LTmpTotalLines, // var aTotalLines: integer
                                           LTmpAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                           LFirstLineIndent, // const aFirstLineIndent: TpointF;
                                           aOptions.LineSpacing, // const aLineSpacing: single = 0;
                                           JStr2, //  const aEllipsisText: JString = nil;
                                           aOptions.FontName, // const aEllipsisFontName: String = '';
                                           aOptions.EllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                                           aOptions.EllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null
                                           aOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const aMaxlines: integer = 0
            JStr1 := nil;
            JStr2 := nil;
            {$ELSEIF defined(IOS)}
            LTmpTextBreaked := ALBreakText(LColorSpace, // const aColorSpace: CGColorSpaceRef;
                                           LFontColor, // const aFontColor: TalphaColor;
                                           aOptions.FontSize, // const aFontSize: single;
                                           LStyle, // const aFontStyle: TFontStyles;
                                           aOptions.FontName, // const aFontName: String;
                                           LTmpRect, // var ARect: TRectF;
                                           LCurrText, // const AText: string;
                                           aOptions.WordWrap, // const aWordWrap: Boolean;
                                           TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                                           aOptions.Trimming, // const aTrimming: TTextTrimming;
                                           LBreakedTextItems, // const aBreakTextItems: TALBreakTextItems;
                                           LTmpTotalLines, // var aTotalLines: integer;
                                           LTmpAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                           LFirstLineIndent, // const aFirstLineIndent: TpointF;
                                           aOptions.LineSpacing, // const aLineSpacing: single = 0;
                                           aOptions.EllipsisText, // const aEllipsisText: string = '…';
                                           aOptions.EllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                                           aOptions.EllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                                           aOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const aMaxlines: integer = 0
            {$ELSE}
            LTmpTextBreaked := ALBreakText(LFontColor, // const aFontColor: TalphaColor;
                                           aOptions.FontSize, // const aFontSize: single;
                                           LStyle, // const aFontStyle: TFontStyles;
                                           aOptions.FontName, // const aFontName: String;
                                           LTmpRect, // var ARect: TRectF;
                                           LCurrText, // const AText: string;
                                           aOptions.WordWrap, // const aWordWrap: Boolean;
                                           TTextAlign.Leading, TTextAlign.Leading, // const AHTextAlign, AVTextAlign: TTextAlign;
                                           aOptions.Trimming, // const aTrimming: TTextTrimming;
                                           LBreakedTextItems, // const aBreakTextItems: TALBreakTextItems;
                                           LTmpTotalLines, // var aTotalLines: integer;
                                           LTmpAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                           LFirstLineIndent, // const aFirstLineIndent: TpointF;
                                           aOptions.LineSpacing, // const aLineSpacing: single = 0;
                                           aOptions.EllipsisText, // const aEllipsisText: string = '…';
                                           aOptions.EllipsisFontStyle, // const aEllipsisFontStyle: TFontStyles = [];
                                           aOptions.EllipsisFontColor, // const aEllipsisFontColor: TalphaColor = TAlphaColorRec.Null;
                                           aOptions.MaxLines - LTotalLines + AlifThen(LTotalLines > 0, 1, 0)); // const aMaxlines: integer = 0
            {$ENDIF}

            //handle FailIfTextBreaked
            if LTmpTextBreaked and aOptions.FailIfTextBreaked then begin ARect.Width := 0; ARect.Height := 0; exit(nil); end;

            //update the img
            if (LCurrImgSrc <> '') and
               (LBreakedTextItems.Count - LBreakedTextItemsCount = 1) then begin
              LBreakedTextItem := LBreakedTextItems[LBreakedTextItems.count - 1];
              LBreakedTextItem.imgsrc := LCurrImgSrc;
            end;

            //if their was not enalf of place to write the ellipsis
            if (LBreakedTextItems.Count >= 2) and                                                                                          // << more than 2 items
               (LBreakedTextItems.Count - LBreakedTextItemsCount = 1) and                                                                  // << only 1 item (the ellipsis) was added
               (LBreakedTextItems[LBreakedTextItems.count - 1].isEllipsis) and                                                             // << last item is an elipsis
               (comparevalue(LBreakedTextItems[LBreakedTextItems.count - 1].rect.right, LTmpRect.Right, Tepsilon.Position) > 0) then begin // << ellipsis is not inside LTmpRect

              //init LBreakedTextItem
              // -1 = the ellipsis '...' (the last item)
              // their is no item for the text that generate the break line
              // because their is even not enalf of place to write the text himself (all the place was used by the ellipsis)
              // -2 = the previous text
              LBreakedTextItem := LBreakedTextItems[LBreakedTextItems.count - 2];

              //if the ellipsis is not on the same line of the LBreakedTextItem then it's mean
              //we don't have enalf of place on one full row to draw the ellipsis so break the loop
              if compareValue(LBreakedTextItem.rect.Top,  // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                              LBreakedTextItems[LBreakedTextItems.count - 1].rect.top,
                              Tepsilon.Position) <> 0 then break;

              //get the params from LBreakedTextItem
              LFontColor := LBreakedTextItem.fontColor;
              LSpanID := LBreakedTextItem.id;
              LStyle := LBreakedTextItem.fontStyle;
              setlength(LCurrText, 2 * length(LCurrText));                         // << i put some space in the end of the previous text to force
              for I := Low(LCurrText) to High(LCurrText) do LCurrText[i] := ' ';   // << the draw of the ellipsis
              {$IF defined(ANDROID)}
              LCurrText := JStringtoString(LBreakedTextItem.line) + LCurrText + '_';
              {$ELSEIF defined(IOS)}
              LCurrText := LBreakedTextItem.text + LCurrText + '_';
              {$ELSE}
              LCurrText := LBreakedTextItem.line + LCurrText + '_';
              {$ENDIF}
              LFirstLineIndent := TpointF.Create(LBreakedTextItem.rect.left, LBreakedTextItem.rect.Top);

              //clean the LBreakedTextItems
              for I := LBreakedTextItems.Count - 1 downto LBreakedTextItems.Count - 2 do
                LBreakedTextItems.Delete(i);

              //try again
              P1 := maxint;
              continue;

            end;

            //stop the loop
            break;

          end;

          //update LTotalLines
          if LTotalLines = 0 then LTotalLines := LTmpTotalLines
          else LTotalLines := LTotalLines + LTmpTotalLines - 1;

          //update LMaxWidth/LMaxHeight
          LMaxWidth := max(LMaxWidth, LTmpRect.Width);
          LMaxHeight := max(LMaxHeight, LTmpRect.height);

          //update aTextBreaked
          aTextBreaked := aTextBreaked or LTmpTextBreaked;
          aAllTextDrawed := aAllTextDrawed and LTmpAllTextDrawed;

          //update all the LBreakedTextItem
          for I := LBreakedTextItemsCount to LBreakedTextItems.Count - 1 do begin
            LBreakedTextItem := LBreakedTextItems[i];
            //-----
            if (not LBreakedTextItem.isEllipsis) or (aOptions.EllipsisFontColor = TAlphaColorRec.Null) then LBreakedTextItem.fontColor := LFontColor
            else LBreakedTextItem.fontColor := aOptions.EllipsisFontColor;
            //-----
            {$IF defined(ANDROID)}
            if (not LBreakedTextItem.isEllipsis) then LBreakedTextItem.FontStyle := LStyle
            else LBreakedTextItem.FontStyle := ALfontStyleToAndroidStyle(aOptions.EllipsisFontStyle);
            {$ELSE}
            if (not LBreakedTextItem.isEllipsis) then LBreakedTextItem.FontStyle := LStyle
            else LBreakedTextItem.FontStyle := aOptions.EllipsisFontStyle;
            {$ENDIF}
            //-----
            if (not LBreakedTextItem.isEllipsis) then LBreakedTextItem.Id := LSpanID
            else LBreakedTextItem.Id := '';
          end;

          //Update LFirstLineIndent
          if LBreakedTextItems.count > LBreakedTextItemsCount then begin
            LBreakedTextItem := LBreakedTextItems[LBreakedTextItems.count - 1];
            LFirstLineIndent := TpointF.Create(LBreakedTextItem.rect.Right, LBreakedTextItem.rect.Top);
            if LBreakedTextItem.isEllipsis then break;
          end;
          // else break; << we can't break here, it's maybe juste a ' ' we try to write at the end of the line that was deleted by ALBreakText

        end;

      end;

      //initialise ARect
      if aOptions.Autosize or (aOptions.AutosizeX and aOptions.AutosizeY) then begin
        aRect.Width := LMaxWidth + aOptions.Padding.Left + aOptions.Padding.right;
        aRect.Height := LMaxHeight + aOptions.Padding.top + aOptions.Padding.bottom;
      end
      else if aOptions.AutosizeX then aRect.Width := LMaxWidth + aOptions.Padding.Left + aOptions.Padding.right
      else if aOptions.AutosizeY then aRect.Height := LMaxHeight + aOptions.Padding.top + aOptions.Padding.bottom;
      case aOptions.HTextAlign of
        TTextAlign.Center: begin
                             if LBreakedTextItems.Count > 0 then begin
                               LCurrentLineY := LBreakedTextItems[0].rect.top; // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                               J := 0;
                               for I := 1 to LBreakedTextItems.Count do begin
                                 if (I = LBreakedTextItems.Count) or
                                    (compareValue(LCurrentLineY, LBreakedTextItems[I].rect.top, Tepsilon.Position) <> 0) then begin
                                   LOffset := Floor((aRect.width -
                                                       LBreakedTextItems[I-1].rect.Right -
                                                         aOptions.Padding.Left -
                                                           aOptions.Padding.right) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                                   while J < I do begin
                                     LBreakedTextItems[j].pos.X := LBreakedTextItems[j].pos.X + aOptions.Padding.Left + LOffset;
                                     LBreakedTextItems[j].rect.Offset(aOptions.Padding.Left + LOffset, 0);
                                     inc(J);
                                   end;
                                   if (I <> LBreakedTextItems.Count) then LCurrentLineY := LBreakedTextItems[I].rect.top;
                                 end;
                               end;
                             end;
                           end;
        TTextAlign.Leading: begin
                             for I := 0 to LBreakedTextItems.Count - 1 do begin
                               LBreakedTextItems[i].pos.X := LBreakedTextItems[i].pos.X + aOptions.Padding.Left;
                               LBreakedTextItems[i].rect.Offset(aOptions.Padding.Left, 0);
                             end;
                           end;
        TTextAlign.Trailing: begin
                               if LBreakedTextItems.Count > 0 then begin
                                 LCurrentLineY := LBreakedTextItems[0].rect.top; // << we can not use pos.y because on ios bold text can be 1 or 2 pixel more high the normal text :(
                                 J := 0;
                                 for I := 1 to LBreakedTextItems.Count do begin
                                   if (I = LBreakedTextItems.Count) or
                                      (compareValue(LCurrentLineY, LBreakedTextItems[I].rect.top, Tepsilon.Position) <> 0) then begin
                                     LOffset := Floor((aRect.width -
                                                         LBreakedTextItems[I-1].rect.Right -
                                                           aOptions.Padding.Left -
                                                             aOptions.Padding.right)); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                                     while J < I do begin
                                       LBreakedTextItems[j].pos.X := LBreakedTextItems[j].pos.X + aOptions.Padding.Left + LOffset;
                                       LBreakedTextItems[j].rect.Offset(aOptions.Padding.Left + LOffset, 0);
                                       inc(J);
                                     end;
                                     if (I <> LBreakedTextItems.Count) then LCurrentLineY := LBreakedTextItems[I].rect.top;
                                   end;
                                 end;
                               end;
                             end;
      end;
      case aOptions.VTextAlign of
        TTextAlign.Center: begin
                             LOffset := Floor((aRect.height -
                                                 LMaxHeight -
                                                   aOptions.Padding.top -
                                                     aOptions.Padding.bottom) / 2); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                             for I := 0 to LBreakedTextItems.Count - 1 do begin
                               LBreakedTextItems[I].pos.y := LBreakedTextItems[i].pos.y + aOptions.Padding.top + LOffset;
                               LBreakedTextItems[I].rect.Offset(0, aOptions.Padding.top + LOffset);
                             end;
                           end;
        TTextAlign.Leading: begin
                             for I := 0 to LBreakedTextItems.Count - 1 do begin
                               LBreakedTextItems[i].pos.Y := LBreakedTextItems[i].pos.Y + aOptions.Padding.top;
                               LBreakedTextItems[i].rect.Offset(0, aOptions.Padding.top);
                             end;
                           end;
        TTextAlign.Trailing: begin
                               LOffset := Floor((aRect.height -
                                                   LMaxHeight -
                                                     aOptions.Padding.top -
                                                       aOptions.Padding.bottom)); // Floor to stay perfectly pixel aligned (but i don't really know if it's really matter, because visually hard to see the difference)
                               for I := 0 to LBreakedTextItems.Count - 1 do begin
                                 LBreakedTextItems[I].pos.y := LBreakedTextItems[i].pos.y + aOptions.Padding.top + LOffset;
                                 LBreakedTextItems[I].rect.Offset(0, aOptions.Padding.top + LOffset);
                               end;
                             end;
      end;
      aRect := ALAlignDimensionToPixelCeil(aRect);

      // init out vars
      if LBreakedTextItems.count > 0 then begin
        aFirstPos := LBreakedTextItems[0].pos;
        LBreakedTextItem := LBreakedTextItems[LBreakedTextItems.count - 1];
        aLastPos := LBreakedTextItem.pos;
        aLastPos.offset(LBreakedTextItem.rect.width, 0);
        aAscent := aLastPos.y - LBreakedTextItem.rect.top;
        aDescent := LBreakedTextItem.rect.bottom - aLastPos.y;
        if LBreakedTextItem.isEllipsis then aEllipsisRect := LBreakedTextItem.rect
        else aEllipsisRect := Trectf.Create(0,0,0,0);
      end;

      //update aElements
      J := 0;
      setlength(aElements, LBreakedTextItems.count);
      for i := 0 to LBreakedTextItems.count - 1 do begin
        if (LBreakedTextItems[i].id <> '') then begin
          aElements[j].Id := LBreakedTextItems[i].id;
          aElements[j].rect := LBreakedTextItems[i].rect;
          inc(j);
        end;
      end;
      setlength(aElements, J);

      {$IF defined(ANDROID)}

      //create the drawing surface
      ALCreateDrawingSurface(LBitmap, // Var aBitmap: Jbitmap;
                             LCanvas, // Var aCanvas: Jcanvas;
                             round(max(1, aRect.Width)), // const w: integer;
                             round(max(1, aRect.Height)));// const h: integer)
      try

        //draw the background
        if (aOptions.Fill.Kind <> TbrushKind.None) or
           (aOptions.stroke.Kind <> TbrushKind.None) then begin
          ALPaintRectangle(LCanvas, // const aBitmap: Jbitmap;
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
        for i := 0 to LBreakedTextItems.count - 1 do begin
          LBreakedTextItem := LBreakedTextItems[i];
          if LBreakedTextItem.imgSrc <> '' then begin
            LMaxWidth := min(LBreakedTextItem.rect.Width, LBreakedTextItem.rect.Height);
            LTmpRect := ALAlignToPixelRound(
                          TrectF.Create(0,0,LMaxWidth,LMaxWidth).
                            CenterAt(LBreakedTextItem.rect));
            LImg := ALLoadFitIntoResourceImageV2(LBreakedTextItem.imgSrc, LTmpRect.Width, LTmpRect.Height);
            if LImg <> nil then begin
              try
                LPaint.setColor(integer(LBreakedTextItem.fontColor)); // sean that the bitmap is paint with the alpha value set via setColor
                                                                      // ideally I would prefer to draw bitmap with alpha = 1 but drawText
                                                                      // don't draw emoji with alpha 1 (that is not the case under iOS) and we
                                                                      // we need do work the same way as LCanvas.drawText work :(
                LCanvas.drawBitmap(LImg, LTmpRect.left {left}, LTmpRect.top {top}, LPaint {paint});
              finally
                LImg.recycle;
                LImg := nil;
              end;
            end;
          end
          else begin
            LPaint.setColor(integer(LBreakedTextItem.fontColor));
            //-----
            JStr1 := StringToJString(aOptions.FontName); // << https://quality.embarcadero.com/browse/RSP-14187
            LTypeface := TJTypeface.JavaClass.create(JStr1, LBreakedTextItem.fontStyle);
            LPaint.setTypeface(LTypeface);
            LTypeface := nil;
            JStr1 := nil;
            //-----
            LCanvas.drawText(LBreakedTextItem.line{text},
                             LBreakedTextItem.pos.x {x},
                             LBreakedTextItem.pos.y {y},
                             LPaint {paint});
          end;
          //-----
        end;

        //free the paint and the canvas
        LPaint := nil;

        //create the result
        result := ALJBitmaptoTexture(LBitmap);

      finally
        ALFreeDrawingSurface(LBitmap, LCanvas);
      end;
      {$ENDIF}

      {$IF defined(IOS)}

      //create the drawing surface
      ALCreateDrawingSurfaceV2(LBitmapSurface, // var aBitmapSurface: TbitmapSurface;
                               LContext, //    Var aContext: CGContextRef;
                               LColorSpace, // const aColorSpace: CGColorSpaceRef;
                               round(max(1, aRect.Width)), // const w: integer;
                               round(max(1, aRect.Height)));// const h: integer)
      try

        //draw the background
        if (aOptions.Fill.Kind <> TbrushKind.None) or
           (aOptions.stroke.Kind <> TbrushKind.None) then begin
          ALPaintRectangle(LContext, // const aContext: CGContextRef;
                           LColorSpace, // const aColorSpace: CGColorSpaceRef;
                           LBitmapSurface.Height, // const aGridHeight: Single;
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
        for i := 0 to LBreakedTextItems.count - 1 do begin
          LBreakedTextItem := LBreakedTextItems[i];
          if LBreakedTextItem.imgSrc <> '' then begin
            LMaxWidth := min(LBreakedTextItem.rect.Width, LBreakedTextItem.rect.Height);
            LTmpRect := ALAlignToPixelRound(
                          TrectF.Create(0,0,LMaxWidth,LMaxWidth).
                            CenterAt(LBreakedTextItem.rect));
            LImg := ALLoadFitIntoResourceImageV2(LBreakedTextItem.imgSrc, LTmpRect.Width, LTmpRect.Height);
            if LImg <> nil then begin
              Try
                CGContextSetAlpha(LContext, TAlphaColorF.create(LBreakedTextItem.fontColor).A); // to work the same way as with android
                CGContextDrawImage(LContext, // c: The graphics context in which to draw the image.
                                   ALLowerLeftCGRect(TPointF.Create(LTmpRect.left, LTmpRect.top),
                                                     LTmpRect.Width,
                                                     LTmpRect.Height,
                                                     LBitmapSurface.Height), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                   LImg); // image The image to draw.
              finally
                CGImageRelease(LImg);
              End;
            end;
          end
          else begin
            CGContextSetAlpha(LContext, 1);
            CGContextSetTextPosition(LContext,
                                     LBreakedTextItem.pos.x {x},
                                     LBitmapSurface.Height - LBreakedTextItem.pos.Y);{y}
            CTLineDraw(LBreakedTextItem.Line, LContext); // Draws a complete line.
          end;
        end;

        //convert the LBitmapSurface to texture
        result := ALBitmapSurfacetoTexture(LBitmapSurface);

      finally
        ALFreeDrawingSurfaceV2(LBitmapSurface, LContext);
      end;

      {$ENDIF}

      {$IF defined(MSWINDOWS) or defined(ALMacOS)}

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
          for i := 0 to LBreakedTextItems.count - 1 do begin
            LBreakedTextItem := LBreakedTextItems[i];
            if LBreakedTextItem.imgSrc <> '' then begin
              LMaxWidth := min(LBreakedTextItem.rect.Width, LBreakedTextItem.rect.Height) * 1.15;
              LTmpRect := ALAlignToPixelRound(
                            TrectF.Create(0,0,LMaxWidth,LMaxWidth).
                              CenterAt(LBreakedTextItem.rect));
              LImg := ALLoadFitIntoResourceImageV2(LBreakedTextItem.imgSrc, LTmpRect.Width, LTmpRect.Height);
              if LImg <> nil then begin
                try
                  result.Canvas.drawBitmap(
                                  LImg,
                                  TrectF.Create(0,0,LTmpRect.Width,LTmpRect.Height),
                                  LTmpRect{DstRect},
                                  TAlphaColorF.create(LBreakedTextItem.fontColor).A{AOpacity}, // to work the same way as with android
                                  false{HighSpeed});
                finally
                  ALFreeAndNil(LImg);
                end;
              end;
            end
            else begin
              result.Canvas.Fill.Color := LBreakedTextItem.fontColor;
              result.Canvas.Font.style := LBreakedTextItem.fontStyle;
              //-----
              result.Canvas.FillText(LBreakedTextItem.rect, // const ARect: TRectF;
                                     LBreakedTextItem.line, // const AText: string;
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
      ALFreeAndNil(LBreakedTextItems);
      alfreeandnil(LFontColors);
      alfreeandnil(LSpanIDs);
    end;

  {$IF defined(IOS)}
  finally
    CGColorSpaceRelease(LColorSpace);
  end;
  {$ENDIF}

end;
{$IF defined(ALZeroBasedStringsON)}
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
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
var LAscent: single;
    LDescent: Single;
    LFirstPos: TpointF;
    LLastPos: TpointF;
    LElements: TalTextElements;
    LEllipsisRect: TRectF;
begin
  result := ALDrawMultiLineText(aText,
                                aRect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                                aTextBreaked, // out => true is the text was "breaked" in several lines
                                aAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                LAscent, // var aAscent: single; // out => the Ascent of the last element (in real pixel)
                                LDescent, // var aDescent: Single; // out => the Descent of the last element (in real pixel)
                                LFirstPos, // var aFirstPos: TpointF; // out => the point of the start of the text
                                LLastPos, // var aLastPos: TpointF; // out => the point of the end of the text
                                LElements, // var aElements: TalTextElements; // out => the list of rect describing all span elements
                                LEllipsisRect, // var aEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
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
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
var LAscent: single;
    LDescent: Single;
    LFirstPos: TpointF;
    LLastPos: TpointF;
    LElements: TalTextElements;
    LEllipsisRect: TRectF;
    LAllTextDrawed: boolean;
begin
  result := ALDrawMultiLineText(aText,
                                aRect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                                aTextBreaked, // out => true is the text was "breaked" in several lines
                                LAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                LAscent, // var aAscent: single; // out => the Ascent of the last element (in real pixel)
                                LDescent, // var aDescent: Single; // out => the Descent of the last element (in real pixel)
                                LFirstPos, // var aFirstPos: TpointF; // out => the point of the start of the text
                                LLastPos, // var aLastPos: TpointF; // out => the point of the end of the text
                                LElements, // var aElements: TalTextElements; // out => the list of rect describing all span elements
                                LEllipsisRect, // var aEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
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
                              const aOptions: TALDrawMultiLineTextOptions): {$IFDEF ALUseTexture}TTexture{$ELSE}Tbitmap{$ENDIF};
var LAscent: single;
    LDescent: Single;
    LFirstPos: TpointF;
    LLastPos: TpointF;
    LElements: TalTextElements;
    LEllipsisRect: TRectF;
    LTextBreaked: boolean;
    LAllTextDrawed: boolean;
begin
  result := ALDrawMultiLineText(aText,
                                aRect, // in => the constraint boundaries in real pixel. out => the calculated rect that contain the html in real pixel
                                LTextBreaked, // out => true is the text was "breaked" in several lines
                                LAllTextDrawed, // var aAllTextDrawed: boolean; // out => true if all the text was drawed (no need of any Ellipsis)
                                LAscent, // var aAscent: single; // out => the Ascent of the last element (in real pixel)
                                LDescent, // var aDescent: Single; // out => the Descent of the last element (in real pixel)
                                LFirstPos, // var aFirstPos: TpointF; // out => the point of the start of the text
                                LLastPos, // var aLastPos: TpointF; // out => the point of the end of the text
                                LElements, // var aElements: TalTextElements; // out => the list of rect describing all span elements
                                LEllipsisRect, // var aEllipsisRect: TRectF; // out => the rect of the Ellipsis (if present)
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

{****************}
{$IF defined(IOS)}
function ALStringsToNSArray(const AStrings: TArray<String>): NSMutableArray;
var S: NSString;
    LString: String;
begin
  Result := TNSMutableArray.Create;
  for LString in AStrings do begin
    S := StrToNSStr(LString);
    Result.addObject((S as ILocalObject).GetObjectID);
  end;
end;
{$ENDIF}

{****************}
{$IF defined(IOS)}
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

{************************}
{$IF CompilerVersion > 34} // sydney
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
                           {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
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

  var LJRect: JRectF;
      LPath: JPath;
      LXRadius: single;
      LYradius: Single;
      LWidthMinusCorners: single;
      LHeightMinusCorners: Single;
      LCorners: TCorners;
      LHalfStrokeWidth: Single;
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

      LJRect := TJRectf.JavaClass.init(aRect.left, aRect.top, aRect.right, aRect.bottom);
      aCanvas.drawRoundRect(LJRect{rect},
                            xRadius {rx},
                            yRadius {ry},
                            apaint);
      LJRect := nil;

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

      LPath := TJPath.Create;
      //----
      LXRadius := xRadius;
      LYradius := yRadius;
      if (LXRadius > aRect.width / 2) then LXRadius := aRect.width / 2;
      if (LYradius > aRect.height / 2) then LYradius := aRect.height / 2;
      //----
      if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := corners
      else LCorners := [];
      //----
      LWidthMinusCorners := (aRect.width - (2 * LXRadius));
      LHeightMinusCorners := (aRect.height - (2 * LYradius));
      //----
      if (Stroke.Kind <> TBrushKind.None) then LHalfStrokeWidth := (Stroke.Thickness) / 2
      else LHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in LCorners) then begin
        LPath.moveTo(aRect.right, aRect.top + LYradius);
        LPath.rQuadTo(0, -LYradius, -LXRadius, -LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(0, -LHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then LPath.moveTo(aRect.right + LHalfStrokeWidth, aRect.top + LYradius)
        else LPath.moveTo(aRect.right, aRect.top + LYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           LPath.rLineTo(0, -LYradius -LHalfStrokeWidth);
           if aDrawOnlyBorder then LPath.rMoveTo(0, LHalfStrokeWidth);
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(-LXRadius -LHalfStrokeWidth,0)
          else begin
            LPath.rMoveTo(+LHalfStrokeWidth,0);
            LPath.rLineTo(-LXRadius -LHalfStrokeWidth,0);
          end;
        end
        else LPath.rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then LPath.rLineTo(-LWidthMinusCorners, 0)
      else LPath.rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(0, +LHalfStrokeWidth);
        LPath.rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(-LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          LPath.rLineTo(-LXRadius -LHalfStrokeWidth, 0);
          if aDrawOnlyBorder then LPath.rMoveTo(LHalfStrokeWidth, 0);
        end
        else LPath.rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(0,LYradius +LHalfStrokeWidth)
          else begin
            LPath.rMoveTo(0,-LHalfStrokeWidth);
            LPath.rLineTo(0,+LYradius +LHalfStrokeWidth);
          end;
        end
        else LPath.rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then LPath.rLineTo(0, LHeightMinusCorners)
      else LPath.rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(LHalfStrokeWidth, 0);
        LPath.rQuadTo(0, LYradius, LXRadius, LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(0, LHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          LPath.rLineTo(0, LYradius +LHalfStrokeWidth);
          if aDrawOnlyBorder then LPath.rMoveTo(0, -LHalfStrokeWidth);
        end
        else LPath.rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(LXRadius +LHalfStrokeWidth,0)
          else begin
            LPath.rMoveTo(-LHalfStrokeWidth,0);
            LPath.rLineTo(+LXRadius +LHalfStrokeWidth,0);
          end;
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then LPath.rLineTo(LWidthMinusCorners, 0)
      else LPath.rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        if not aDrawOnlyBorder then LPath.rlineTo(0, -LHalfStrokeWidth);
        LPath.rQuadTo(LXRadius, 0, LXRadius, -LYradius);
        if not aDrawOnlyBorder then LPath.rlineTo(LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          LPath.rLineTo(LXRadius +LHalfStrokeWidth,0);
          if aDrawOnlyBorder then LPath.rMoveTo(-LHalfStrokeWidth, 0);
        end
        else LPath.rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then LPath.rLineTo(0, -LYradius -LHalfStrokeWidth)
          else begin
            LPath.rMoveTo(0,+LHalfStrokeWidth);
            LPath.rLineTo(0,-LYradius -LHalfStrokeWidth);
          end;
        end
        else LPath.rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then LPath.rLineTo(0, -LHeightMinusCorners)
      else LPath.rMoveTo(0, -LHeightMinusCorners);

      //-----
      if (not aDrawOnlyBorder) and
         (Shadow <> nil) and
         (Shadow.enabled) then aPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});

      aCanvas.drawPath(LPath,aPaint);
      LPath := nil;

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

  var LXRadius: single;
      LYradius: Single;
      LWidthMinusCorners: single;
      LHeightMinusCorners: Single;
      LCorners: TCorners;
      LHalfStrokeWidth: Single;
      LCurPoint: TpointF;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _moveTo(x: Single; y: Single);
    begin
      CGContextMoveToPoint(aContext, X, aGridHeight - Y);
      LCurPoint.X := x;
      LCurPoint.Y := Y;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rQuadTo(dx1: Single; dy1: Single; dx2: Single; dy2: Single);
    begin
      CGContextAddQuadCurveToPoint(aContext,
                                   LCurPoint.X + dx1{cpx},
                                   aGridHeight - (LCurPoint.Y + dy1){cpy},
                                   LCurPoint.X + dx2{x},
                                   aGridHeight - (LCurPoint.Y + dy2){y});
      LCurPoint.X := LCurPoint.X + dx2;
      LCurPoint.Y := LCurPoint.Y + dy2;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rLineTo(dx: Single; dy: Single);
    begin
      CGContextAddLineToPoint(aContext, LCurPoint.X + dx{x}, aGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
    end;

    {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
    procedure _rMoveTo(dx: Single; dy: Single);
    begin
      CGContextMoveToPoint(aContext, LCurPoint.X + dx{x}, aGridHeight - (LCurPoint.Y + dy{y}));
      LCurPoint.X := LCurPoint.X + dx;
      LCurPoint.Y := LCurPoint.Y + dy;
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

      LXRadius := xRadius;
      LYradius := yRadius;
      if (LXRadius > aRect.width / 2) then LXRadius := aRect.width / 2;
      if (LYradius > aRect.height / 2) then LYradius := aRect.height / 2;
      //----
      if (compareValue(LXRadius, 0, TEpsilon.position) > 0) and
         (compareValue(LYradius, 0, TEpsilon.position) > 0) then LCorners := corners
      else LCorners := [];
      //----
      LWidthMinusCorners := (aRect.width - (2 * LXRadius));
      LHeightMinusCorners := (aRect.height - (2 * LYradius));
      //----
      if (Stroke.Kind <> TBrushKind.None) then LHalfStrokeWidth := (Stroke.Thickness) / 2
      else LHalfStrokeWidth := 0;


      //----- TopRight
      if (TCorner.TopRight in LCorners) then begin
        _moveTo(aRect.right, aRect.top + LYradius);
        _rQuadTo(0, -LYradius, -LXRadius, -LYradius);
        if not aDrawOnlyBorder then _rlineTo(0, -LHalfStrokeWidth);
      end
      else begin
        if not aDrawOnlyBorder then _moveTo(aRect.right + LHalfStrokeWidth, aRect.top + LYradius)
        else _moveTo(aRect.right, aRect.top + LYradius);
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
           _rLineTo(0, -LYradius -LHalfStrokeWidth);
           if aDrawOnlyBorder then _rMoveTo(0, LHalfStrokeWidth);
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
        //----
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(-LXRadius -LHalfStrokeWidth,0)
          else begin
            _rMoveTo(+LHalfStrokeWidth,0);
            _rLineTo(-LXRadius -LHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(-LXRadius,0); // aDrawOnlyBorder AND not TSide.top
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.Top in sides) then _rLineTo(-LWidthMinusCorners, 0)
      else _rMoveTo(-LWidthMinusCorners, 0);

      //----- TopLeft
      if (TCorner.TopLeft in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, +LHalfStrokeWidth);
        _rQuadTo(-LXRadius, 0, -LXRadius, LYradius);
        if not aDrawOnlyBorder then _rlineTo(-LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.top in sides) then begin
          _rLineTo(-LXRadius -LHalfStrokeWidth, 0);
          if aDrawOnlyBorder then _rMoveTo(LHalfStrokeWidth, 0);
        end
        else _rMoveTo(-LXRadius, 0); // aDrawOnlyBorder AND not TSide.top
        //----
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0,LYradius +LHalfStrokeWidth)
          else begin
            _rMoveTo(0,-LHalfStrokeWidth);
            _rLineTo(0,+LYradius +LHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0,LYradius); // aDrawOnlyBorder AND not TSide.left
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.left in sides) then _rLineTo(0, LHeightMinusCorners)
      else _rMoveTo(0, LHeightMinusCorners);

      //----- BottomLeft
      if (TCorner.BottomLeft in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(LHalfStrokeWidth, 0);
        _rQuadTo(0, LYradius, LXRadius, LYradius);
        if not aDrawOnlyBorder then _rlineTo(0, LHalfStrokeWidth);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.left in sides) then begin
          _rLineTo(0, LYradius +LHalfStrokeWidth);
          if aDrawOnlyBorder then _rMoveTo(0, -LHalfStrokeWidth);
        end
        else _rMoveTo(0, LYradius); // aDrawOnlyBorder AND not TSide.left
        //----
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(LXRadius +LHalfStrokeWidth,0)
          else begin
            _rMoveTo(-LHalfStrokeWidth,0);
            _rLineTo(+LXRadius +LHalfStrokeWidth,0);
          end;
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.bottom in sides) then _rLineTo(LWidthMinusCorners, 0)
      else _rMoveTo(LWidthMinusCorners, 0);

      //----- BottomRight
      if (TCorner.BottomRight in LCorners) then begin
        if not aDrawOnlyBorder then _rlineTo(0, -LHalfStrokeWidth);
        _rQuadTo(LXRadius, 0, LXRadius, -LYradius);
        if not aDrawOnlyBorder then _rlineTo(LHalfStrokeWidth, 0);
      end
      else begin
        if (not aDrawOnlyBorder) or
           (TSide.bottom in sides) then begin
          _rLineTo(LXRadius +LHalfStrokeWidth,0);
          if aDrawOnlyBorder then _rMoveTo(-LHalfStrokeWidth, 0);
        end
        else _rMoveTo(LXRadius,0); // aDrawOnlyBorder AND not TSide.bottom
        //----
        if (not aDrawOnlyBorder) or
           (TSide.right in sides) then begin
          if not aDrawOnlyBorder then _rLineTo(0, -LYradius -LHalfStrokeWidth)
          else begin
            _rMoveTo(0,+LHalfStrokeWidth);
            _rLineTo(0,-LYradius -LHalfStrokeWidth);
          end;
        end
        else _rMoveTo(0, -LYradius); // aDrawOnlyBorder AND not TSide.right
      end;
      //-----
      if (not aDrawOnlyBorder) or
         (TSide.right in sides) then _rLineTo(0, -LHeightMinusCorners)
      else _rMoveTo(0, -LHeightMinusCorners);

    end;

  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION ' _GetShapeRect (MSWINDOWS / ALMacOS)'}
  {$IF defined(MSWINDOWS) or defined(ALMacOS)}
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
var LRect: TrectF;
    LTmpBitmap: Jbitmap;
    LRadialShader: JRadialGradient;
    LLinearShader: JLinearGradient;
    LPaint: JPaint;
    LColors: TJavaArray<Integer>;
    LStops: TJavaArray<Single>;
    LPorterDuffXfermode: jPorterDuffXfermode;
    LBitmapInfo: AndroidBitmapInfo;
    LPixelBuffer: Pointer;
    LBitmapData: TBitmapData;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    i: integer;
{$ELSEIF defined(IOS)}
var LRect: TrectF;
    LAlphaColor: TAlphaColorCGFloat;
    LColor: CGColorRef;
    LCallback: CGFunctionCallbacks;
    LShading: CGShadingRef;
    LFunc: CGFunctionRef;
    LBitmapData: TBitmapData;
    LTmpContext: CGContextRef;
    LImageRef: CGImageRef;
    LImage: UIImage;
{$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
var LShapeRect: TRectF;
    Off: Single;
    StrokeThicknessRestoreValue: Single;
    FillShape, DrawShape: Boolean;
{$ENDIF}

begin

  {$IFDEF ANDROID}

  //create the canvas and the paint
  LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  //init LRect
  if Stroke.Kind <> TBrushKind.None then begin
    LRect := TrectF.Create(dstRect.Left + (Stroke.Thickness / 2),
                           dstRect.Top + (Stroke.Thickness / 2),
                           dstRect.right - (Stroke.Thickness / 2),
                           dstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := dstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the rectangle
  if Fill.Kind <> TBrushKind.None then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LRect + Stroke.Thickness :( this result&ing in border if the fill is for exemple black and border white

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        LColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
        LStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
        for i := 0 to Fill.Gradient.Points.Count - 1 do begin
          LColors[Fill.Gradient.Points.Count - 1 - i] := integer(Fill.Gradient.Points[i].Color);
          LStops[Fill.Gradient.Points.Count - 1 - i] := 1 - Fill.Gradient.Points[i].Offset;
        end;
        LRadialShader := TJRadialGradient.JavaClass.init(
                           LRect.CenterPoint.x{x},
                           LRect.CenterPoint.y{y},
                           LRect.width / 2{radius},
                           LColors,
                           LStops,
                           TJShader_TileMode.JavaClass.CLAMP{tile});
        LPaint.setShader(LRadialShader);
        _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});
        LPaint.setShader(nil);
        LRadialShader := nil;
        ALfreeandNil(LColors);
        ALfreeandNil(LStops);
      end
      else if Fill.Gradient.Style = TGradientStyle.Linear then begin
        LColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
        LStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
        for i := 0 to Fill.Gradient.Points.Count - 1 do begin
          LColors[i] := integer(Fill.Gradient.Points[i].Color);
          LStops[i] := Fill.Gradient.Points[i].Offset;
        end;
        LLinearShader := TJLinearGradient.JavaClass.init(
                           LRect.TopLeft.x + (Fill.Gradient.StartPosition.X * LRect.Width){x0},
                           LRect.TopLeft.y + (Fill.Gradient.StartPosition.Y * LRect.Height){y0},
                           LRect.BottomRight.x + (Fill.Gradient.StopPosition.X * LRect.Width){x1},
                           LRect.BottomRight.y + (Fill.Gradient.StopPosition.Y * LRect.Height){y1},
                           LColors,
                           LStops,
                           TJShader_TileMode.JavaClass.CLAMP{tile});
        LPaint.setShader(LLinearShader);
        _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});
        LPaint.setShader(nil);
        LLinearShader := nil;
        ALfreeandNil(LColors);
        ALfreeandNil(LStops);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          //-----
          LTmpBitmap := TJBitmap.JavaClass.createBitmap(fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888);
          //-----
          FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
          if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LBitmapInfo) = 0) and
             (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LPixelBuffer) = 0) then
          try
            if fill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
            try
              System.Move(LBitmapData.Data^, LPixelBuffer^, LBitmapData.Pitch * LBitmapData.Height);
            finally
              fill.Bitmap.Bitmap.Unmap(LBitmapData);
            end;
          finally
            AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID);
          end;
          //-----
          _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});
          LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
          LJDestRectf := TJRectf.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);
          LJSrcRect := TJRect.JavaClass.init(0, 0, fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height);
          LPaint.setXfermode(LPorterDuffXfermode);
          aCanvas.drawBitmap(LTmpBitmap, LJSrcRect, LJDestRectf, LPaint);
          LPaint.setXfermode(nil);
          LPorterDuffXfermode := nil;
          LJSrcRect := nil;
          LJDestRectf := nil;
          //-----
          LTmpBitmap.recycle;
          LTmpBitmap := nil;
          //-----
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      LPaint.setColor(integer(Fill.Color));
      _drawRect(aCanvas, LPaint, LRect, false{aDrawOnlyBorder});
    end;

  end;

  //stroke the rectangle
  if Stroke.Kind <> TBrushKind.None then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    LPaint.setStrokeWidth(Stroke.Thickness);

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      LPaint.setColor(integer(Stroke.Color));
      _drawRect(aCanvas, LPaint, LRect, true{aDrawOnlyBorder});
    end;

  end;

  //free the paint and the canvas
  LPaint := nil;

  {$ELSEIF DEFINED(IOS)}

  //set the paint default properties
  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
  //-----
  CGContextSetShouldAntialias(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.

  //init LRect
  if Stroke.Kind <> TBrushKind.None then begin
    LRect := TrectF.Create(DstRect.Left + (Stroke.Thickness / 2),
                           DstRect.Top + (Stroke.Thickness / 2),
                           DstRect.right - (Stroke.Thickness / 2),
                           DstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := DstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the rectangle
  if Fill.Kind <> TBrushKind.None then begin

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style in [TGradientStyle.Radial,
                                 TGradientStyle.Linear] then begin
        CGContextSaveGState(aContext);
        //-----
        LCallback.version := 0;
        LCallback.evaluate := @ALGradientEvaluateCallback;
        LCallback.releaseInfo:= nil;
        LFunc := CGFunctionCreate(fill.Gradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                                  1, // domainDimension - The number of inputs.
                                  @aDefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                                  4, // rangeDimension - The number of outputs.
                                  nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                                  @LCallback); // callbacks - A pointer to a callback function table.
        try
          if Fill.Gradient.Style = TGradientStyle.Radial then begin
            LShading := CGShadingCreateRadial(aColorSpace, // colorspace
                                              CGPoint.Create(TPointF.Create(LRect.Width / 2, aGridHeight - (LRect.height / 2))), // start - The center of the starting circle, in the shading's target coordinate space.
                                              LRect.Width / 2, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                                              CGPoint.Create(TPointF.Create(LRect.Width / 2, aGridHeight - (LRect.Height / 2))), // end - The center of the ending circle, in the shading's target coordinate space.
                                              0, // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                                              LFunc, // function
                                              {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                                              {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
          end
          else begin
            LShading := CGShadingCreateAxial(aColorSpace, // colorspace
                                             CGPointMake(LRect.Left + (Fill.Gradient.StartPosition.X * LRect.Width),
                                                         aGridHeight - LRect.top - (Fill.Gradient.StartPosition.Y * LRect.Height)), // start - The starting point of the axis, in the shading's target coordinate space.
                                             CGPointMake(LRect.Left + (Fill.Gradient.StopPosition.X * LRect.Width),
                                                         aGridHeight - LRect.top - (Fill.Gradient.StopPosition.Y * LRect.Height)), // end - The ending point of the axis, in the shading's target coordinate space.
                                             LFunc, // function
                                             {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting point of the axis.
                                             {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending point of the axis.
          end;
          try
            _DrawPath(LRect, false{aDrawOnlyBorder});
            CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                     // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                     // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                     // save the graphics state before you clip and restore the graphics state after you’ve completed
                                     // any clipped drawing.
            //-----
            if (Shadow <> nil) and
               (Shadow.enabled) then begin
              LAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
              LColor := CGColorCreate(aColorSpace, @LAlphaColor);
              try
                CGContextSetShadowWithColor(aContext,
                                            CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                            Shadow.blur, // blur
                                            LColor); // color
              finally
                CGColorRelease(LColor);
              end;
            end;
            //-----
            CGContextDrawShading(aContext, LShading);
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
            CGShadingRelease(LShading);
          end;
        finally
          CGFunctionRelease(LFunc);
        end;
        //-----
        CGContextRestoreGState(aContext);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          if fill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
          try
            LTmpContext := CGBitmapContextCreate(LBitmapData.Data, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                 LBitmapData.Width, // width: The width, in pixels, of the required bitmap.
                                                 LBitmapData.Height, // height: The height, in pixels, of the required bitmap.
                                                 8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                    //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                    //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                    //                   chapter of Quartz 2D Programming Guide.
                                                 LBitmapData.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
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
                LImageRef := CGBitmapContextCreateImage(LTmpContext);
                if LImageRef <> nil then
                try
                  LImage := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(LImageRef));
                  if LImage <> nil then
                  try
                    CGContextSaveGState(aContext);
                    //-----
                    _DrawPath(LRect, false{aDrawOnlyBorder});
                    CGContextClip(aContext); // Modifies the current clipping path, using the nonzero winding number rule.
                                             // Unlike the current path, the current clipping path is part of the graphics state. Therefore,
                                             // to re-enlarge the paintable area by restoring the clipping path to a prior state, you must
                                             // save the graphics state before you clip and restore the graphics state after you’ve completed
                                             // any clipped drawing.
                    //-----
                    if (Shadow <> nil) and
                       (Shadow.enabled) then begin
                      LAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
                      LColor := CGColorCreate(aColorSpace, @LAlphaColor);
                      try
                        CGContextSetShadowWithColor(aContext,
                                                    CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                                    Shadow.blur, // blur
                                                    LColor); // color
                      finally
                        CGColorRelease(LColor);
                      end;
                    end;
                    //-----
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(LRect.TopLeft,
                                                         LRect.Width,
                                                         LRect.Height,
                                                         aGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       LImage.CGImage); // image The image to draw.
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
                    LImage.release;
                  end;
                finally
                  CGImageRelease(LImageRef);
                end;
              finally
                CGContextRelease(LTmpContext);
              end;
            end;
          finally
            fill.Bitmap.Bitmap.Unmap(LBitmapData);
          end;
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      LAlphaColor := TAlphaColorCGFloat.Create(Fill.Color);
      CGContextSetRGBFillColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
      _DrawPath(LRect, false{aDrawOnlyBorder});
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        LAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
        LColor := CGColorCreate(aColorSpace, @LAlphaColor);
        try
          CGContextSetShadowWithColor(aContext,
                                      CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                      Shadow.blur, // blur
                                      LColor); // color
        finally
          CGColorRelease(LColor);
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
      LAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
      CGContextSetRGBStrokeColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
      CGContextSetLineWidth(aContext, Stroke.Thickness);
      _DrawPath(LRect, True{aDrawOnlyBorder});
      CGContextStrokePath(aContext);
    end;

  end;

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

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
                        {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
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
var LTmpBitmap: Jbitmap;
    LShader: JRadialGradient;
    LPaint: JPaint;
    LRect: TRectf;
    LColors: TJavaArray<Integer>;
    LStops: TJavaArray<Single>;
    LPorterDuffXfermode: jPorterDuffXfermode;
    LBitmapInfo: AndroidBitmapInfo;
    LPixelBuffer: Pointer;
    LBitmapData: TBitmapData;
    LJDestRectf: JrectF;
    LJSrcRect: Jrect;
    i: integer;
{$ELSEIF defined(IOS)}
var LAlphaColor: TAlphaColorCGFloat;
    LColor: CGColorRef;
    LCallback: CGFunctionCallbacks;
    LShading: CGShadingRef;
    LFunc: CGFunctionRef;
    LRect: TRectf;
    LBitmapData: TBitmapData;
    LTmpContext: CGContextRef;
    LImageRef: CGImageRef;
    LImage: UIImage;
{$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
var LShapeRect: TRectF;
    StrokeThicknessRestoreValue: Single;
    FillShape, DrawShape: Boolean;
{$ENDIF}

begin

  {$IFDEF ANDROID}

  //create the canvas and the paint
  LPaint := TJPaint.JavaClass.init;
  LPaint.setAntiAlias(true); // Enabling this flag will cause all draw operations that support antialiasing to use it.
  LPaint.setFilterBitmap(True); // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn with nearest neighbor sampling, likely resulting in artifacts.
  LPaint.setDither(true); // Enabling this flag applies a dither to any blit operation where the target's colour space is more constrained than the source.

  //init LRect
  if Stroke.Kind <> TBrushKind.None then begin
    LRect := TrectF.Create(dstRect.Left + (Stroke.Thickness / 2),
                           dstRect.Top + (Stroke.Thickness / 2),
                           dstRect.right - (Stroke.Thickness / 2),
                           dstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := dstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the circle
  if Fill.Kind <> TBrushKind.None then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.FILL); // FILL_AND_STROCK it's absolutely useless, because it's will fill on the full LRect + Stroke.Thickness :( this result&ing in border if the fill is for exemple black and border white

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        LColors := TJavaArray<Integer>.Create(Fill.Gradient.Points.Count);
        LStops := TJavaArray<Single>.Create(Fill.Gradient.Points.Count);
        for i := 0 to Fill.Gradient.Points.Count - 1 do begin
          LColors[Fill.Gradient.Points.Count - 1 - i] := integer(Fill.Gradient.Points[i].Color);
          LStops[Fill.Gradient.Points.Count - 1 - i] := 1 - Fill.Gradient.Points[i].Offset;
        end;
        LShader := TJRadialGradient.JavaClass.init(LRect.CenterPoint.x{x}, LRect.CenterPoint.y{y}, LRect.width / 2{radius},  LColors, LStops, TJShader_TileMode.JavaClass.CLAMP{tile});
        LPaint.setShader(LShader);
        if (Shadow <> nil) and
           (Shadow.enabled) then LPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});
        aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
        if (Shadow <> nil) and
           (Shadow.enabled) then LPaint.clearShadowLayer;
        LPaint.setShader(nil);
        LShader := nil;
        alfreeandNil(LColors);
        alfreeandNil(LStops);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          //-----
          LTmpBitmap := TJBitmap.JavaClass.createBitmap(fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height, TJBitmap_Config.JavaClass.ARGB_8888);
          //-----
          FillChar(LBitmapInfo, SizeOf(LBitmapInfo), 0);
          if (AndroidBitmap_getInfo(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LBitmapInfo) = 0) and
             (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID, @LPixelBuffer) = 0) then
          try
            if fill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
            try
              System.Move(LBitmapData.Data^, LPixelBuffer^, LBitmapData.Pitch * LBitmapData.Height);
            finally
              fill.Bitmap.Bitmap.Unmap(LBitmapData);
            end;
          finally
            AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, (LTmpBitmap as ILocalObject).GetObjectID);
          end;
          //-----
          aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
          LPorterDuffXfermode := TJPorterDuffXfermode.JavaClass.init(TJPorterDuff_Mode.JavaClass.SRC_IN);
          LJDestRectf := TJRectf.JavaClass.init(LRect.left, LRect.top, LRect.right, LRect.bottom);
          LJSrcRect := TJRect.JavaClass.init(0, 0, fill.Bitmap.Bitmap.Width, fill.Bitmap.Bitmap.height);
          LPaint.setXfermode(LPorterDuffXfermode);
          if (Shadow <> nil) and
             (Shadow.enabled) then LPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});
          aCanvas.drawBitmap(LTmpBitmap, LJSrcRect, LJDestRectf, LPaint);
          if (Shadow <> nil) and
             (Shadow.enabled) then LPaint.clearShadowLayer;
          LPaint.setXfermode(nil);
          LPorterDuffXfermode := nil;
          LJSrcRect := nil;
          LJDestRectf := nil;
          //-----
          LTmpBitmap.recycle;
          LTmpBitmap := nil;
          //-----
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      LPaint.setColor(integer(Fill.Color));
      if (Shadow <> nil) and
         (Shadow.enabled) then LPaint.setShadowLayer(Shadow.blur{radius}, Shadow.OffsetX{dx}, Shadow.OffsetY{dy}, integer(Shadow.ShadowColor){shadowColor});
      aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
      if (Shadow <> nil) and
         (Shadow.enabled) then LPaint.clearShadowLayer;
    end;

  end;

  //stroke the circle
  if Stroke.Kind <> TBrushKind.None then begin

    //init LPaint
    LPaint.setStyle(TJPaint_Style.JavaClass.STROKE);
    LPaint.setStrokeWidth(Stroke.Thickness);

    //stroke with solid color
    if Stroke.Kind = TBrushKind.Solid then begin
      LPaint.setColor(integer(Stroke.Color));
      aCanvas.drawCircle(LRect.CenterPoint.x{cx}, LRect.CenterPoint.y{cy}, LRect.width / 2{radius}, LPaint);
    end;

  end;

  //free the paint and the canvas
  LPaint := nil;

  {$ELSEIF DEFINED(IOS)}

  //set the paint default properties
  CGContextSetInterpolationQuality(aContext, kCGInterpolationHigh); // Sets the level of interpolation quality for a graphics context. http://stackoverflow.com/questions/5685884/imagequality-with-cgcontextsetinterpolationquality
  //-----
  CGContextSetShouldAntialias(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets anti-aliasing on or off for a graphics context.
  CGContextSetAllowsAntialiasing(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.

  //init LRect
  if Stroke.Kind <> TBrushKind.None then begin
    LRect := TrectF.Create(DstRect.Left + (Stroke.Thickness / 2),
                           DstRect.Top + (Stroke.Thickness / 2),
                           DstRect.right - (Stroke.Thickness / 2),
                           DstRect.bottom - (Stroke.Thickness / 2)); // http://stackoverflow.com/questions/17038017/ios-draw-filled-circles
  end
  else LRect := DstRect; // << stupid bug https://quality.embarcadero.com/browse/RSP-16607

  //fill the circle
  if Fill.Kind <> TBrushKind.None then begin

    //fill with gradient
    if Fill.Kind = TBrushKind.Gradient then begin
      if Fill.Gradient.Style = TGradientStyle.Radial then begin
        CGContextSaveGState(aContext);
        //-----
        LCallback.version := 0;
        LCallback.evaluate := @ALGradientEvaluateCallback;
        LCallback.releaseInfo:= nil;
        LFunc := CGFunctionCreate(fill.Gradient, // info - A pointer to user-defined storage for data that you want to pass to your callbacks.
                                  1, // domainDimension - The number of inputs.
                                  @aDefaultInputRange, // domain - An array of (2*domainDimension) floats used to specify the valid intervals of input values
                                  4, // rangeDimension - The number of outputs.
                                  nil, // range - An array of (2*rangeDimension) floats that specifies the valid intervals of output values
                                  @LCallback); // callbacks - A pointer to a callback function table.
        try
          LShading := CGShadingCreateRadial(aColorSpace, // colorspace
                                            CGPoint.Create(TPointF.Create(LRect.Width / 2, LRect.height / 2)), // start - The center of the starting circle, in the shading's target coordinate space.
                                            LRect.Width / 2, // startRadius - The radius of the starting circle, in the shading's target coordinate space.
                                            CGPoint.Create(TPointF.Create(LRect.Width / 2, LRect.Height / 2)), // end - The center of the ending circle, in the shading's target coordinate space.
                                            0, // endRadius - The radius of the ending circle, in the shading's target coordinate space.
                                            LFunc, // function
                                            {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}, // extendStart - A Boolean value that specifies whether to extend the shading beyond the starting circle.
                                            {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // extendEnd - A Boolean value that specifies whether to extend the shading beyond the ending circle.
          try
            CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
            CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(LRect.TopLeft,
                                                                  LRect.Width,
                                                                  LRect.Height,
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
              LAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
              LColor := CGColorCreate(aColorSpace, @LAlphaColor);
              try
                CGContextSetShadowWithColor(aContext,
                                            CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                            Shadow.blur, // blur
                                            LColor); // color
              finally
                CGColorRelease(LColor);
              end;
            end;
            //-----
            CGContextDrawShading(aContext, LShading);
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
            CGShadingRelease(LShading);
          end;
        finally
          CGFunctionRelease(LFunc);
        end;
        //-----
        CGContextRestoreGState(aContext);
      end;
    end

    //fill with bitmap
    else if Fill.Kind = TBrushKind.Bitmap then begin
      if not fill.Bitmap.Bitmap.IsEmpty then begin
        if fill.Bitmap.WrapMode = TWrapMode.TileStretch then begin
          if fill.Bitmap.Bitmap.Map(TMapAccess.Read, LBitmapData) then
          try
            LTmpContext := CGBitmapContextCreate(LBitmapData.Data, // data: A pointer to the destination in memory where the drawing is to be rendered. The size of this
                                                                   //       memory block should be at least (bytesPerRow*height) bytes.
                                                                   //       In iOS 4.0 and later, and OS X v10.6 and later, you can pass NULL if you want Quartz to allocate
                                                                   //       memory for the bitmap. This frees you from managing your own memory, which reduces memory leak issues.
                                                 LBitmapData.Width, // width: The width, in pixels, of the required bitmap.
                                                 LBitmapData.Height, // height: The height, in pixels, of the required bitmap.
                                                 8, // bitsPerComponent: The number of bits to use for each component of a pixel in memory. For example, for a 32-bit
                                                    //                   pixel format and an RGB color space, you would specify a value of 8 bits per component. For
                                                    //                   the list of supported pixel formats, see “Supported Pixel Formats” in the Graphics Contexts
                                                    //                   chapter of Quartz 2D Programming Guide.
                                                 LBitmapData.Pitch, // bytesPerRow: The number of bytes of memory to use per row of the bitmap. If the data parameter is NULL, passing
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
                LImageRef := CGBitmapContextCreateImage(LTmpContext);
                if LImageRef <> nil then
                try
                  LImage := TUIImage.Wrap(TUIImage.alloc.initWithCGImage(LImageRef));
                  if LImage <> nil then
                  try
                    CGContextSaveGState(aContext);
                    //-----
                    CGContextBeginPath(aContext);  // Creates a new empty path in a graphics context.
                    CGContextAddEllipseInRect(aContext, ALLowerLeftCGRect(LRect.TopLeft,
                                                                          LRect.Width,
                                                                          LRect.Height,
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
                      LAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
                      LColor := CGColorCreate(aColorSpace, @LAlphaColor);
                      try
                        CGContextSetShadowWithColor(aContext,
                                                    CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                                    Shadow.blur, // blur
                                                    LColor); // color
                      finally
                        CGColorRelease(LColor);
                      end;
                    end;
                    //-----
                    CGContextDrawImage(aContext, // c: The graphics context in which to draw the image.
                                       ALLowerLeftCGRect(LRect.TopLeft,
                                                         LRect.Width,
                                                         LRect.Height,
                                                         aGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                                       LImage.CGImage); // image The image to draw.
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
                    LImage.release;
                  end;
                finally
                  CGImageRelease(LImageRef);
                end;
              finally
                CGContextRelease(LTmpContext);
              end;
            end;
          finally
            fill.Bitmap.Bitmap.Unmap(LBitmapData);
          end;
        end;
      end;
    end

    //fill with solid color
    else if Fill.Kind = TBrushKind.Solid then begin
      LAlphaColor := TAlphaColorCGFloat.Create(Fill.Color);
      CGContextSetRGBFillColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
      //-----
      if (Shadow <> nil) and
         (Shadow.enabled) then begin
        LAlphaColor := TAlphaColorCGFloat.Create(Shadow.ShadowColor);
        LColor := CGColorCreate(aColorSpace, @LAlphaColor);
        try
          CGContextSetShadowWithColor(aContext,
                                      CGSizeMake(Shadow.OffsetX, Shadow.OffsetY), // offset
                                      Shadow.blur, // blur
                                      LColor); // color
        finally
          CGColorRelease(LColor);
        end;
      end;
      //-----
      CGContextFillEllipseInRect(aContext, ALLowerLeftCGRect(LRect.TopLeft,
                                                             LRect.Width,
                                                             LRect.Height,
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
      LAlphaColor := TAlphaColorCGFloat.Create(Stroke.Color);
      CGContextSetRGBStrokeColor(aContext, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
      CGContextSetLineWidth(aContext, Stroke.Thickness);
      CGContextStrokeEllipseInRect(aContext, ALLowerLeftCGRect(LRect.TopLeft,
                                                               LRect.Width,
                                                               LRect.Height,
                                                               aGridHeight));
    end;

  end;

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

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
                                 {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
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
        CGContextSetShouldAntialias(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // default: ON
                                                                                                        // Sets anti-aliasing on or off for a graphics context.
        CGContextSetAllowsAntialiasing(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
        //-----
        //CGContextSetShouldSmoothFonts(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // There are cases, such as rendering to a bitmap, when font smoothing is not appropriate and should be disabled.
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
        //CGContextSetAllowsFontSmoothing(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow font smoothing for a graphics context.
        //-----
        CGContextSetShouldSubpixelPositionFonts(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // default: ON
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
        CGContextSetAllowsFontSubpixelPositioning(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow subpixel positioning for a graphics context
        //-----
        CGContextSetShouldSubpixelQuantizeFonts(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // default: ON
                                                                                                                    // Enables or disables subpixel quantization in a graphics context.
                                                                                                                    // -----
                                                                                                                    // Subpixel quantization is only enabled if subpixel positioning is enabled. Subpixel
                                                                                                                    // quantization improves the rendering of fonts whose glyphs are at subpixel positions
                                                                                                                    // by more closely examining how the shapes that make up the glyphs cover an individual pixel.
                                                                                                                    // This improvement, requires additional processing so changing this value can affect text
                                                                                                                    // drawing performance.
        CGContextSetAllowsFontSubpixelQuantization(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF});  // Sets whether or not to allow subpixel quantization for a graphics context

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

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

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
                               {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}
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

  {$ELSEIF defined(MSWINDOWS) or defined(ALMacOS)}

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
      CGContextSetShouldAntialias(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // default: ON
                                                                                                      // Sets anti-aliasing on or off for a graphics context.
      CGContextSetAllowsAntialiasing(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow anti-aliasing for a graphics context.
      //-----
      //CGContextSetShouldSmoothFonts(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // There are cases, such as rendering to a bitmap, when font smoothing is not appropriate and should be disabled.
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
      //CGContextSetAllowsFontSmoothing(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow font smoothing for a graphics context.
      //-----
      CGContextSetShouldSubpixelPositionFonts(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // default: ON
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
      CGContextSetAllowsFontSubpixelPositioning(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // Sets whether or not to allow subpixel positioning for a graphics context
      //-----
      CGContextSetShouldSubpixelQuantizeFonts(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF}); // default: ON
                                                                                                                  // Enables or disables subpixel quantization in a graphics context.
                                                                                                                  // -----
                                                                                                                  // Subpixel quantization is only enabled if subpixel positioning is enabled. Subpixel
                                                                                                                  // quantization improves the rendering of fonts whose glyphs are at subpixel positions
                                                                                                                  // by more closely examining how the shapes that make up the glyphs cover an individual pixel.
                                                                                                                  // This improvement, requires additional processing so changing this value can affect text
                                                                                                                  // drawing performance.
      CGContextSetAllowsFontSubpixelQuantization(aContext, {$IF CompilerVersion >= 34}{sydney}true{$ELSE}1{$ENDIF});  // Sets whether or not to allow subpixel quantization for a graphics context

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
