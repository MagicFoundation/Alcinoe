unit Alcinoe.FMX.BreakText;

interface

{$I Alcinoe.inc}

uses
  System.UITypes,
  System.Types,
  Fmx.types,
  FMX.graphics,
  Alcinoe.FMX.Common,
  Alcinoe.FMX.Graphics;

type

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMultiLineTextOptions = class;

  {~~~~~~~~~~~~~~~~~~~~~}
  TALTextElement = record
    Id: string;
    Rect: TrectF;
    class function Empty: TALTextElement; inline; static;
  end;
  TALTextElements = array of TALTextElement;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMultiLineTextAdjustRectProc = procedure(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; var ARect: TrectF; var ASurfaceSize: TSizeF) of object;
  TALMultiLineTextBeforeDrawBackgroundProc = procedure(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF) of object;
  TALMultiLineTextBeforeDrawParagraphProc = procedure(const ACanvas: TALCanvas; const AOptions: TALMultiLineTextOptions; Const ARect: TrectF) of object;

  {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
  TALMultiLineTextOptions = class(Tobject)
  private
    FOnAdjustRect: TALMultiLineTextAdjustRectProc;
    FOnBeforeDrawBackground: TALMultiLineTextBeforeDrawBackgroundProc;
    FOnBeforeDrawParagraph: TALMultiLineTextBeforeDrawParagraphProc;
  public
    Scale: Single; // default = 1
    AlignToPixel: Boolean; // default = true
    Opacity: Single; // Default = 1
    //--
    FontFamily: String; // default = ''
    FontSize: single; // default = 14
    FontWeight: TFontWeight; // default = TFontWeight.Regular
    FontSlant: TFontSlant; // default = TFontSlant.Regular
    FontStretch: TFontStretch; // default = TFontStretch.Regular
    FontColor: TalphaColor; // default = TAlphaColors.Black
    //--
    DecorationKinds: TALTextDecorationKinds; // default = []
    DecorationStyle: TALTextDecorationStyle; // default = TALTextDecorationStyle.Solid
    DecorationThicknessMultiplier: Single; // default = 1
    DecorationColor: TAlphaColor; // default = TAlphaColors.Null
    //--
    EllipsisText: String; // default = '…';
    EllipsisInheritSettings: Boolean; // default = True;
    //--
    EllipsisFontFamily: String; // default = ''
    EllipsisFontSize: single; // default = 14
    EllipsisFontWeight: TFontWeight; // default = TFontWeight.Regular
    EllipsisFontSlant: TFontSlant; // default = TFontSlant.Regular
    EllipsisFontStretch: TFontStretch; // default = TFontStretch.Regular
    EllipsisFontColor: TalphaColor; // default = TAlphaColors.Black
    //--
    EllipsisDecorationKinds: TALTextDecorationKinds; // default = []
    EllipsisDecorationStyle: TALTextDecorationStyle; // default = TALTextDecorationStyle.Solid
    EllipsisDecorationThicknessMultiplier: Single; // default = 1
    EllipsisDecorationColor: TAlphaColor; // default = TAlphaColors.Null
    //--
    AutoSize: Boolean; // default = True
    AutoSizeX: Boolean; // default = False
    AutoSizeY: Boolean; // default = False
    MaxLines: integer; // default = 65535
    // When LineHeightMultiplier = 0 the line height will be the sum of the font ascent + font descent + font leading.
    // When LineHeightMultiplier is non-null, the line height of the span of text will be a multiple of fontSize and be exactly fontSize * height logical pixels tall
    LineHeightMultiplier: single; // default = 0
    LetterSpacing: Single; // default = 0
    Trimming: TALTextTrimming; // default = TALTextTrimming.Word
    FailIfTextBroken: boolean; // default = false
    //--
    Direction: TALTextDirection; // default = TALTextDirection.LeftToRight
    HTextAlign: TALTextHorzAlign; // default = TALTextHorzAlign.Leading
    VTextAlign: TALTextVertAlign; // default = TALTextVertAlign.Leading
    //--
    FillColor: TAlphaColor; // default = TAlphaColors.null
    FillGradientStyle: TGradientStyle; // Default = TGradientStyle.Linear;
    FillGradientColors: TArray<TAlphaColor>; // Default = [];
    FillGradientOffsets: TArray<Single>; // Default = [];
    FillGradientAngle: Single; // Default = 180;
    FillResourceName: String; // default = ''
    FillWrapMode: TALImageWrapMode; // default = TALImageWrapMode.Fit
    FillBackgroundMargins: TRectF; // default = TRectF.Empty
    FillImageMargins: TRectF; // default = TRectF.Empty
    //--
    StateLayerOpacity: Single; // Default = 0
    StateLayerColor: TAlphaColor; // Default = TAlphaColors.null
    StateLayerMargins: TRectF; // default = TRectF.Empty
    StateLayerXRadius: Single; // default = 0
    StateLayerYRadius: Single; // default = 0
    //--
    StrokeColor: TalphaColor; // default = TAlphaColors.null
    StrokeThickness: Single; // default = 1
    //--
    ShadowColor: TAlphaColor; // default = TAlphaColors.null
    ShadowBlur: Single; // default = 12
    ShadowOffsetX: Single; // default = 0
    ShadowOffsetY: Single; // default = 0
    //--
    Sides: TSides; // default = AllSides
    XRadius: Single; // default = 0
    YRadius: Single; // default = 0
    Corners: TCorners; // default = AllCorners
    Padding: TRectF;  // default = 0
    //--
    // We support only the following HTML tags:
    //   * <br>
    //   * <b>...</b>
    //   * <i>...</i>
    //   * <font color="#FFFFFF"
    //           face="Roboto">...</font>
    //   * <span id="xxx"
    //           color="#FFFFFF"
    //           font-family="Roboto"
    //           font-size="14px"
    //           font-weight="bold"
    //           font-style="italic"
    //           font-stretch="condensed"
    //           text-decoration-line="underline overline"
    //           text-decoration-style="solid"
    //           text-decoration-thickness="3"
    //           text-decoration-color="#FFFFFF"
    //           line-height="1.6"
    //           letter-spacing="2px"
    //           background-color="#FFFFFF">...</span>
    //   * <img src="{ResourceName}"
    //          width="xxx"
    //          height="xxx">
    //   * Other "<" and ">" must be encoded with "&lt;" and "&gt;"
    //
    // Note: You can also use the "style" attribute for inline styling
    // Ex: <span style="font-size:14px;font-style:italic">
    TextIsHtml: boolean; // default = false;
    //--
    property OnAdjustRect: TALMultiLineTextAdjustRectProc read FOnAdjustRect write FOnAdjustRect; // default = nil
    property OnBeforeDrawBackground: TALMultiLineTextBeforeDrawBackgroundProc read FOnBeforeDrawBackground write FOnBeforeDrawBackground; // default = nil
    property OnBeforeDrawParagraph: TALMultiLineTextBeforeDrawParagraphProc read FOnBeforeDrawParagraph write FOnBeforeDrawParagraph; // default = nil
    //--
    constructor Create;
    //--
    procedure Assign(Source: TALMultiLineTextOptions);
    procedure ScaleAndAlignProperties(const ACanvasScale: Single);
  End;

procedure ALDrawMultiLineText(
            var ASurface: TALSurface; // If nil and AOnlyMeasure is false, a new surface will be created
            var ACanvas: TALCanvas; // If nil and AOnlyMeasure is false, a new canvas will be created
            const AText: String; // When AOptions.TextIsHtml is set to true, the HTML tags supported are described in the TALMultiLineTextOptions.TextIsHtml field declaration.
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
            out AElements: TALTextElements; // out => The list of rectangles describing all span elements.
            const AOptions: TALMultiLineTextOptions;
            const AOnlyMeasure: Boolean = False); overload;
procedure ALDrawMultiLineText(
            var ASurface: TALSurface; // If nil and AOnlyMeasure is false, a new surface will be created
            var ACanvas: TALCanvas; // If nil and AOnlyMeasure is false, a new canvas will be created
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
            const AOptions: TALMultiLineTextOptions;
            const AOnlyMeasure: Boolean = False); inline; overload;
procedure ALDrawMultiLineText(
            var ASurface: TALSurface; // If nil and AOnlyMeasure is false, a new surface will be created
            var ACanvas: TALCanvas; // If nil and AOnlyMeasure is false, a new canvas will be created
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            const AOptions: TALMultiLineTextOptions;
            const AOnlyMeasure: Boolean = False); inline; overload;
procedure ALDrawMultiLineText(
            var ASurface: TALSurface; // If nil and AOnlyMeasure is false, a new surface will be created
            var ACanvas: TALCanvas; // If nil and AOnlyMeasure is false, a new canvas will be created
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            const AOptions: TALMultiLineTextOptions;
            const AOnlyMeasure: Boolean = False); inline; overload;


procedure ALMeasureMultiLineText(
            const AText: String; // When AOptions.TextIsHtml is set to true, the HTML tags supported are described in the TALMultiLineTextOptions.TextIsHtml field declaration.
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
            out AElements: TALTextElements; // out => The list of rectangles describing all span elements.
            const AOptions: TALMultiLineTextOptions); inline; overload;
procedure ALMeasureMultiLineText(
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
            const AOptions: TALMultiLineTextOptions); inline; overload;
procedure ALMeasureMultiLineText(
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            const AOptions: TALMultiLineTextOptions); inline; overload;
procedure ALMeasureMultiLineText(
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            const AOptions: TALMultiLineTextOptions); inline; overload;


function ALCreateMultiLineTextDrawable(
           const AText: String; // When AOptions.TextIsHtml is set to true, the HTML tags supported are described in the TALMultiLineTextOptions.TextIsHtml field declaration.
           var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                              // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                              // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                              // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
           out AElements: TALTextElements; // out => The list of rectangles describing all span elements.
           const AOptions: TALMultiLineTextOptions): TALDrawable; inline; overload;
function ALCreateMultiLineTextDrawable(
           const AText: String;
           var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                              // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                              // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                              // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
           const AOptions: TALMultiLineTextOptions): TALDrawable; inline; overload;
function ALCreateMultiLineTextDrawable(
           const AText: String;
           var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                              // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                              // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                              // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           const AOptions: TALMultiLineTextOptions): TALDrawable; inline; overload;
function ALCreateMultiLineTextDrawable(
           const AText: String;
           var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                              // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                              // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                              // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
           const AOptions: TALMultiLineTextOptions): TALDrawable; inline; overload;

function ALGetTextElementsByID(Const ATextElements: TALTextElements; Const AId: String): TALTextElements;

implementation

uses
  System.Math.Vectors,
  system.SysUtils,
  System.Character,
  System.Math,
  System.Generics.Collections,
  {$IF defined(ALSkiaEngine)}
  System.Skia.API,
  Fmx.skia,
  {$ENDIF}
  {$IF not defined(ALSkiaEngine)}
  System.SyncObjs,
  Alcinoe.Cipher,
  {$ENDIF}
  {$IF defined(ANDROID)}
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  Androidapi.Helpers,
  {$ENDIF}
  {$IF defined(ios)}
  iOSapi.CoreGraphics,
  iOSapi.CoreText,
  Macapi.CoreFoundation,
  {$ENDIF}
  {$IF defined(ALMacOS)}
  Macapi.CocoaTypes,
  Macapi.CoreGraphics,
  Macapi.CoreText,
  Macapi.CoreFoundation,
  {$ENDIF}
  {$IF defined(MSWINDOWS)}
  Winapi.Windows,
  FMX.TextLayout,
  FMX.Helpers.Win,
  FMX.Utils,
  {$ENDIF}
  Alcinoe.StringList,
  Alcinoe.StringUtils,
  Alcinoe.Common;

{**************************************************}
class function TALTextElement.Empty: TALTextElement;
begin
  Result.ID := '';
  Result.Rect := TRectF.Empty;
end;

{*****************************************}
constructor TALMultiLineTextOptions.Create;
begin
  FOnAdjustRect := nil;
  FOnBeforeDrawBackground := nil;
  FOnBeforeDrawParagraph := nil;
  //--
  Scale := 1;
  AlignToPixel := true;
  Opacity := 1;
  //--
  FontFamily := '';
  FontSize := 14;
  FontWeight := TFontWeight.Regular;
  FontSlant := TFontSlant.Regular;
  FontStretch := TFontStretch.Regular;
  FontColor := TAlphaColors.Black;
  //--
  DecorationKinds := [];
  DecorationStyle := TALTextDecorationStyle.Solid;
  DecorationThicknessMultiplier := 1;
  DecorationColor := TAlphaColors.Null;
  //--
  EllipsisText := '…';
  EllipsisInheritSettings := True;
  //--
  EllipsisFontFamily := '';
  EllipsisFontSize := 14;
  EllipsisFontWeight := TFontWeight.Regular;
  EllipsisFontSlant := TFontSlant.Regular;
  EllipsisFontStretch := TFontStretch.Regular;
  EllipsisFontColor := TAlphaColors.Black;
  //--
  EllipsisDecorationKinds := [];
  EllipsisDecorationStyle := TALTextDecorationStyle.Solid;
  EllipsisDecorationThicknessMultiplier := 1;
  EllipsisDecorationColor := TAlphaColors.Null;
  //--
  AutoSize := True;
  AutoSizeX := False;
  AutoSizeY := False;
  MaxLines := 65535;
  LineHeightMultiplier := 0;
  LetterSpacing := 0;
  Trimming := TALTextTrimming.Word;
  FailIfTextBroken := false;
  //--
  Direction := TALTextDirection.LeftToRight;
  HTextAlign := TALTextHorzAlign.Leading;
  VTextAlign := TALTextVertAlign.Leading;
  //--
  FillColor := TAlphaColors.Null;
  FillGradientStyle := TGradientStyle.Linear;
  FillGradientColors := [];
  FillGradientOffsets := [];
  FillGradientAngle := 180;
  FillResourceName := '';
  FillWrapMode := TALImageWrapMode.Fit;
  FillBackgroundMargins := TRectF.Empty;
  FillImageMargins := TRectF.Empty;
  //--
  StateLayerOpacity := 0;
  StateLayerColor := TAlphaColors.null;
  StateLayerMargins := TRectF.Empty;
  StateLayerXRadius := 0;
  StateLayerYRadius := 0;
  //--
  StrokeColor := TalphaColors.Null;
  StrokeThickness := 1;
  //--
  ShadowColor := TAlphaColors.Null;
  ShadowBlur := 12;
  ShadowOffsetX := 0;
  ShadowOffsetY := 0;
  //--
  Sides := AllSides;
  XRadius := 0;
  YRadius := 0;
  Corners := AllCorners;
  Padding := TRectF.Create(0,0,0,0);
  //--
  TextIsHtml := false;
end;

{************************************************************************}
procedure TALMultiLineTextOptions.Assign(Source: TALMultiLineTextOptions);
begin
  FOnAdjustRect := Source.FOnAdjustRect;
  FOnBeforeDrawBackground := Source.FOnBeforeDrawBackground;
  FOnBeforeDrawParagraph := Source.FOnBeforeDrawParagraph;
  //--
  Scale := Source.Scale;
  AlignToPixel := Source.AlignToPixel;
  Opacity := Source.Opacity;
  //--
  FontFamily := Source.FontFamily;
  FontSize := Source.FontSize;
  FontWeight := Source.FontWeight;
  FontSlant := Source.FontSlant;
  FontStretch := Source.FontStretch;
  FontColor := Source.FontColor;
  //--
  DecorationKinds := Source.DecorationKinds;
  DecorationStyle := Source.DecorationStyle;
  DecorationThicknessMultiplier := Source.DecorationThicknessMultiplier;
  DecorationColor := Source.DecorationColor;
  //--
  EllipsisText := Source.EllipsisText;
  EllipsisInheritSettings := Source.EllipsisInheritSettings;
  //--
  EllipsisFontFamily := Source.EllipsisFontFamily;
  EllipsisFontSize := Source.EllipsisFontSize;
  EllipsisFontWeight := Source.EllipsisFontWeight;
  EllipsisFontSlant := Source.EllipsisFontSlant;
  EllipsisFontStretch := Source.EllipsisFontStretch;
  EllipsisFontColor := Source.EllipsisFontColor;
  //--
  EllipsisDecorationKinds := Source.EllipsisDecorationKinds;
  EllipsisDecorationStyle := Source.EllipsisDecorationStyle;
  EllipsisDecorationThicknessMultiplier := Source.EllipsisDecorationThicknessMultiplier;
  EllipsisDecorationColor := Source.EllipsisDecorationColor;
  //--
  AutoSize := Source.AutoSize;
  AutoSizeX := Source.AutoSizeX;
  AutoSizeY := Source.AutoSizeY;
  MaxLines := Source.MaxLines;
  LineHeightMultiplier := Source.LineHeightMultiplier;
  LetterSpacing := Source.LetterSpacing;
  Trimming := Source.Trimming;
  FailIfTextBroken := Source.FailIfTextBroken;
  //--
  Direction := Source.Direction;
  HTextAlign := Source.HTextAlign;
  VTextAlign := Source.VTextAlign;
  //--
  FillColor := Source.FillColor;
  FillGradientStyle := Source.FillGradientStyle;
  FillGradientColors := Source.FillGradientColors;
  FillGradientOffsets := Source.FillGradientOffsets;
  FillGradientAngle := Source.FillGradientAngle;
  FillResourceName := Source.FillResourceName;
  FillWrapMode := Source.FillWrapMode;
  FillBackgroundMargins := Source.FillBackgroundMargins;
  FillImageMargins := Source.FillImageMargins;
  //--
  StateLayerOpacity := Source.StateLayerOpacity;
  StateLayerColor := Source.StateLayerColor;
  StateLayerMargins := Source.StateLayerMargins;
  StateLayerXRadius := Source.StateLayerXRadius;
  StateLayerYRadius := Source.StateLayerYRadius;
  //--
  StrokeColor := Source.StrokeColor;
  StrokeThickness := Source.StrokeThickness;
  //--
  ShadowColor := Source.ShadowColor;
  ShadowBlur := Source.ShadowBlur;
  ShadowOffsetX := Source.ShadowOffsetX;
  ShadowOffsetY := Source.ShadowOffsetY;
  //--
  Sides := Source.Sides;
  XRadius := Source.XRadius;
  YRadius := Source.YRadius;
  Corners := Source.Corners;
  Padding := Source.Padding;
  //--
  TextIsHtml := Source.TextIsHtml;
end;

{************************************************************************************}
procedure TALMultiLineTextOptions.ScaleAndAlignProperties(const ACanvasScale: Single);
begin
  if Scale <> 1 then begin
    FontSize := FontSize * Scale;
    EllipsisFontSize := EllipsisFontSize * Scale;
    LetterSpacing := LetterSpacing * Scale;
    FillBackgroundMargins.Top := FillBackgroundMargins.Top * Scale;
    FillBackgroundMargins.Right := FillBackgroundMargins.Right * Scale;
    FillBackgroundMargins.Left := FillBackgroundMargins.Left * Scale;
    FillBackgroundMargins.Bottom := FillBackgroundMargins.Bottom * Scale;
    FillImageMargins.Top := FillImageMargins.Top * Scale;
    FillImageMargins.Right := FillImageMargins.Right * Scale;
    FillImageMargins.Left := FillImageMargins.Left * Scale;
    FillImageMargins.Bottom := FillImageMargins.Bottom * Scale;
    StateLayerMargins.Top := StateLayerMargins.Top * Scale;
    StateLayerMargins.Right := StateLayerMargins.Right * Scale;
    StateLayerMargins.Left := StateLayerMargins.Left * Scale;
    StateLayerMargins.Bottom := StateLayerMargins.Bottom * Scale;
    if compareValue(StateLayerXRadius, 0, TEpsilon.Vector) > 0 then StateLayerXRadius := StateLayerXRadius * Scale;
    if compareValue(StateLayerYRadius, 0, TEpsilon.Vector) > 0 then StateLayerYRadius := StateLayerYRadius * Scale;
    StrokeThickness := StrokeThickness * Scale;
    ShadowBlur := ShadowBlur * Scale;
    ShadowOffsetX := ShadowOffsetX * Scale;
    ShadowOffsetY := ShadowOffsetY * Scale;
    if compareValue(XRadius, 0, TEpsilon.Vector) > 0 then XRadius := XRadius * Scale;
    if compareValue(YRadius, 0, TEpsilon.Vector) > 0 then YRadius := YRadius * Scale;
    Padding.Top := Padding.Top * Scale;
    Padding.Right := Padding.Right * Scale;
    Padding.Left := Padding.Left * Scale;
    Padding.Bottom := Padding.Bottom * Scale;
    Scale := 1;
  end;
  if AlignToPixel then begin
    FontSize := ALAlignDimensionToPixelRound(FontSize, ACanvasScale, TEpsilon.FontSize);
    EllipsisFontSize := ALAlignDimensionToPixelRound(EllipsisFontSize, ACanvasScale, TEpsilon.FontSize);
    LetterSpacing := ALAlignDimensionToPixelRound(LetterSpacing, ACanvasScale, TEpsilon.FontSize);
    FillBackgroundMargins := ALAlignEdgesToPixelRound(FillBackgroundMargins, ACanvasScale, TEpsilon.Position);
    FillImageMargins := ALAlignEdgesToPixelRound(FillImageMargins, ACanvasScale, TEpsilon.Position);
    StateLayerMargins := ALAlignEdgesToPixelRound(StateLayerMargins, ACanvasScale, TEpsilon.Position);
    StrokeThickness := ALAlignDimensionToPixelRound(StrokeThickness, ACanvasScale, TEpsilon.Vector);
    Shadowblur := ALAlignDimensionToPixelRound(Shadowblur, ACanvasScale, Tepsilon.Vector);
    ShadowOffsetX := ALAlignDimensionToPixelRound(ShadowOffsetX, ACanvasScale, TEpsilon.Position);
    ShadowOffsetY := ALAlignDimensionToPixelRound(ShadowOffsetY, ACanvasScale, TEpsilon.Position);
    Padding := ALAlignEdgesToPixelRound(Padding, ACanvasScale, TEpsilon.Position);
  end;
end;

{****************************}
procedure ALDrawMultiLineText(
            var ASurface: TALSurface; // If nil and AOnlyMeasure is false, a new surface will be created
            var ACanvas: TALCanvas; // If nil and AOnlyMeasure is false, a new canvas will be created
            const AText: String; // When AOptions.TextIsHtml is set to true, the HTML tags supported are described in the TALMultiLineTextOptions.TextIsHtml field declaration.
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
            out AElements: TALTextElements; // out => The list of rectangles describing all span elements.
            const AOptions: TALMultiLineTextOptions;
            const AOnlyMeasure: Boolean = False);

  var LScale: Single;
  var LOptions: TALMultiLineTextOptions;

  {$REGION 'TRangeIndex'}
  {$IF defined(ALSkiaEngine)}
  type
    TRangeIndex = record
      SpanID: String;
      start: Integer;
      &end: Integer;
    end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'TExtendedTextElement'}
  {$IF not defined(ALSkiaEngine)}
  Type
    TExtendedTextElement = record
      Id: string;
      LineIndex: Integer;
      Rect: TrectF;
      DrawTextOffsetY: Single;
      Ascent: Single;
      Descent: Single;
      LineHeightMultiplier: Single;
      LetterSpacing: Single;
      Text: String;
      IsEllipsis: Boolean;
      IsBreakLine: Boolean;
      BackgroundColor: TAlphaColor;
      FontFamily: String;
      FontSize: single;
      FontWeight: TFontWeight;
      FontSlant: TFontSlant;
      FontStretch: TFontStretch;
      FontColor: TalphaColor;
      DecorationKinds: TALTextDecorationKinds;
      DecorationStyle: TALTextDecorationStyle;
      DecorationThicknessMultiplier: Single;
      DecorationColor: TAlphaColor;
      ImgSrc: String;
    end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_IsWhiteSpace'}
  function _IsWhiteSpace(const AChar: Char): Boolean; inline;
  begin
    // SPACE, TAB, LF, LINE-TAB, FF, CR, NEL
    Result := (AChar.IsWhiteSpace) and (AChar <> #$00A0{No-break Space});
  end;
  {$ENDREGION}

  {$REGION '_IsHyphen'}
  function _IsHyphen(const AChar: Char): Boolean; inline;
  begin
    Result := AChar.IsInArray(['-'{Hyphen-Minus}, #$00AD{Soft Hyphen}]);
  end;
  {$ENDREGION}

  {$REGION '_findLastBreakPosition'}
  // Returns zero-based index or, if you prefer, the number of characters
  // If ANumberOfChars <= 0 returns ANumberOfChars
  function _findLastBreakPosition(
             const AText: String;
             Const ANumberOfChars: Integer;
             const AHardBreak: Boolean = False;
             const ASkipEndOfTextPunctuation: Boolean = False): integer;
  begin
    if ANumberOfChars <= 0 then exit(ANumberOfChars);
    Var Ln := AText.Length;
    // Break on Hyphen or WhiteSpace
    Result := Min(ANumberOfChars, Ln);
    While Result > 0 do begin
      if (_IsHyphen(AText.Chars[Result-1])) or
         ((Result < Ln) and
          (_IsWhiteSpace(AText.Chars[Result])) and
          (not _IsWhiteSpace(AText.Chars[Result-1]))) then
        break
      else
        dec(Result);
    end;
    // Break on \ or /
    If (AHardBreak) and (Result <= 0) then begin
      Result := Min(ANumberOfChars, Ln);
      While Result > 0 do begin
        if (Result < Ln) and (CharInSet(AText.Chars[Result-1], ['\','/'])) then break
        else dec(Result);
      end;
    end;
    // Skip end of text punctuation
    While (ASkipEndOfTextPunctuation) and
          (Result > 0) and
          ((AText.Chars[Result-1].IsPunctuation) or // ., ; : ? ! - – — ' " ( ) [ ] { } < > / \ … ‘ ’ “ ” * & @ # % _ | ~ ^ + = « » ¡ ¿
           (AText.Chars[Result-1].IsWhiteSpace) or // SPACE, TAB,LF,LINE-TAB,FF,CR, No-break Space, NEL
           (AText.Chars[Result-1].IsSeparator)) do // Space / No-Break Space
      Dec(Result);
  end;
  {$ENDREGION}

  {$REGION '_TryStrColorToInt'}
  Function _TryStrColorToInt(const AColorStr: String; out AColorInt: Cardinal): boolean;
  begin
    Result := False;
    if (AColorStr <> '') and (AColorStr[low(AColorStr)] = '#') then begin
      var LAlphaColor: TAlphaColor;
      if ALTryRGBAHexToAlphaColor(AlcopyStr(AColorStr, 2, MaxInt), LAlphaColor) then begin
        Result := True;
        AColorInt := Cardinal(LAlphaColor);
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION '_TryStrPxValueToFloat'}
  Function _TryStrPxValueToFloat(const AValueStr: String; out AValueFloat: Single): boolean;
  begin
    if AlPosW('px', AValueStr) <= 0 then exit(False)
    else result := ALTryStrToFloat(ALStringReplaceW(AValueStr, 'px', '', []), AValueFloat, ALDefaultFormatSettingsW);
  end;
  {$ENDREGION}

  {$REGION '_getInfosFromTag'}
  procedure _getInfosFromTag(
              const ATag: String; // color="#ffffff" id="xxx"
              const ASpanIds: TALStringListW;
              const AFontFamilies: TALStringListW;
              const AFontSizes: Tlist<Single>;
              const AFontWeights: Tlist<TFontWeight>;
              const AFontSlants: TList<TFontSlant>;
              const AFontStretchs: TList<TFontStretch>;
              const AFontColors: Tlist<TalphaColor>;
              const ADecorationKinds: TList<TALTextDecorationKinds>;
              const ADecorationStyles: TList<TALTextDecorationStyle>;
              const ADecorationThicknessMultipliers: TList<Single>;
              const ADecorationColors: TList<TAlphaColor>;
              const ABackgroundColors: TList<TAlphaColor>;
              const ALineHeightMultipliers: TList<Single>;
              const ALetterSpacings: TList<Single>;
              out AImgSrc: String;
              out AImgWidth: Single;
              out AImgHeight: Single); overload;
  begin
    var LParamList := TALStringListW.Create;
    try
      ALExtractHeaderFieldsWithQuoteEscaped(
        [' ', #9, #13, #10]{Separators},
        [' ', #9, #13, #10]{WhiteSpace},
        ['"', '''']{Quotes},
        PChar(ATag){Content},
        LParamList{Strings},
        False{HttpDecode},
        True{StripQuotes});
      //--
      var LStyle := LParamList.Values['style'];
      if LStyle <> '' then begin
        var LStyleParamList := TALStringListW.Create;
        try
          LStyleParamList.NameValueSeparator := ':';
          ALExtractHeaderFieldsWithQuoteEscaped(
            [';']{Separators},
            [' ', #9, #13, #10]{WhiteSpace},
            ['"', '''']{Quotes},
            PChar(LStyle){Content},
            LStyleParamList{Strings},
            False{HttpDecode},
            True{StripQuotes});
          //--
          var LValue := ALLowerCase(LStyleParamList.Values['width']);
          If ALPosW('px', LValue) > 0 then LStyleParamList.Values['width'] := ALStringReplaceW(LValue, 'px', '', [])
          else LStyleParamList.Values['width'] := '';
          //--
          LValue := ALLowerCase(LStyleParamList.Values['height']);
          If ALPosW('px', LValue) > 0 then LStyleParamList.Values['height'] := ALStringReplaceW(LValue, 'px', '', [])
          else LStyleParamList.Values['height'] := '';
          //--
          for var I := 0 to LStyleParamList.Count - 1 do
            LParamList.Values[LStyleParamList.Names[i]] := ALTrim(LStyleParamList.ValueFromIndex[i]);
        finally
          ALFreeAndNil(LStyleParamList);
        end;
      end;
      //--
      if ASpanIds <> nil then begin
        var LSpanID := LParamList.Values['id'];
        if LSpanID <> '' then ASpanIds.Add(LParamList.Values['id'])
        else if ASpanIds.Count > 0 then ASpanIds.Add(ASpanIds[ASpanIds.Count - 1])
        else ASpanIds.Add('');
      end;
      //--
      if AFontFamilies <> nil then begin
        var LFontFamily := LParamList.Values['face']; // <font face="Arial">
        if LFontFamily = '' then LFontFamily := LParamList.Values['font-family']; // <span font-family="Arial">
        if LFontFamily <> '' then AFontFamilies.Add(LFontFamily)
        else if AFontFamilies.Count > 0 then AFontFamilies.Add(AFontFamilies[AFontFamilies.Count - 1])
        else AFontFamilies.Add(LOptions.FontFamily);
      end;
      //--
      if AFontSizes <> nil then begin
        var LFontSizeFloat: Single;
        if _TryStrPxValueToFloat(ALLowerCase(LParamList.Values['font-size']), LFontSizeFloat) then AFontSizes.Add(LFontSizeFloat * LScale) // <span font-size="14px">
        else if AFontSizes.Count > 0 then AFontSizes.Add(AFontSizes[AFontSizes.Count - 1])
        else AFontSizes.Add(LOptions.FontSize);
      end;
      //--
      if AFontWeights <> nil then begin
        var LFontWeight := ALLowerCase(LParamList.Values['font-weight']); // <span font-weight="bold">
             If (LFontWeight = '100') then AFontWeights.Add(TFontWeight.Thin) // Thin (Hairline)
        else if (LFontWeight = '200') then AFontWeights.Add(TFontWeight.UltraLight) // Extra Light (Ultra Light)
        else if (LFontWeight = '300') then AFontWeights.Add(TFontWeight.Light) // Light
        else if (LFontWeight = '350') then AFontWeights.Add(TFontWeight.SemiLight)
        else if (LFontWeight = '400') or (LFontWeight = 'normal') then AFontWeights.Add(TFontWeight.Regular) // Normal (Regular)
        else if (LFontWeight = '500') then AFontWeights.Add(TFontWeight.Medium) // Medium
        else if (LFontWeight = '600') then AFontWeights.Add(TFontWeight.Semibold) // Semi Bold (Demi Bold)
        else if (LFontWeight = '700') or (LFontWeight = 'bold') then AFontWeights.Add(TFontWeight.Bold) // Bold
        else if (LFontWeight = '800') then AFontWeights.Add(TFontWeight.UltraBold) // Extra Bold (Ultra Bold)
        else if (LFontWeight = '900') then AFontWeights.Add(TFontWeight.Black) // Black (Heavy)
        else if (LFontWeight = '1000') then AFontWeights.Add(TFontWeight.UltraBlack) // Extra Black (Ultra Black)
        else if AFontWeights.Count > 0 then AFontWeights.Add(AFontWeights[AFontWeights.Count - 1])
        else AFontWeights.Add(LOptions.FontWeight);
      end;
      //--
      if AFontSlants <> nil then begin
        var LFontStyle := ALLowerCase(LParamList.Values['font-style']); // <span font-style="italic">
             if (LFontStyle = 'normal') then AFontSlants.Add(TFontSlant.Regular)
        else if (LFontStyle = 'italic') then AFontSlants.Add(TFontSlant.Italic)
        else if (LFontStyle = 'oblique') then AFontSlants.Add(TFontSlant.Oblique)
        else if AFontSlants.Count > 0 then AFontSlants.Add(AFontSlants[AFontSlants.Count - 1])
        else AFontSlants.Add(LOptions.FontSlant);
      end;
      //--
      if AFontStretchs <> nil then begin
        var LFontStretch := ALLowerCase(LParamList.Values['font-Stretch']); // <span font-stretch="condensed">
             if (LFontStretch = 'ultra-condensed') then AFontStretchs.Add(TFontStretch.UltraCondensed)
        else if (LFontStretch = 'extra-condensed') then AFontStretchs.Add(TFontStretch.ExtraCondensed)
        else if (LFontStretch = 'condensed') then AFontStretchs.Add(TFontStretch.Condensed)
        else if (LFontStretch = 'semi-condensed') then AFontStretchs.Add(TFontStretch.SemiCondensed)
        else if (LFontStretch = 'normal') then AFontStretchs.Add(TFontStretch.Regular)
        else if (LFontStretch = 'semi-expanded') then AFontStretchs.Add(TFontStretch.SemiExpanded)
        else if (LFontStretch = 'expanded') then AFontStretchs.Add(TFontStretch.Expanded)
        else if (LFontStretch = 'extra-expanded') then AFontStretchs.Add(TFontStretch.ExtraExpanded)
        else if (LFontStretch = 'ultra-expanded') then AFontStretchs.Add(TFontStretch.UltraExpanded)
        else if AFontStretchs.Count > 0 then AFontStretchs.Add(AFontStretchs[AFontStretchs.Count - 1])
        else AFontStretchs.Add(LOptions.FontStretch);
      end;
      //--
      if AFontColors <> nil then begin
        var LColorInt: Cardinal;
        if _TryStrColorToInt(LParamList.Values['color'], LColorInt) then AFontColors.Add(TalphaColor(LcolorInt)) // <span color="#xxxxxx">
        else if AFontColors.Count > 0 then AFontColors.Add(AFontColors[AFontColors.Count - 1])
        else AFontColors.Add(LOptions.FontColor);
      end;
      //--
      If ADecorationKinds <> nil then begin
        var LIdx := LParamList.IndexOfName('text-decoration-line');
        if LIdx >= 0 then begin
          var LTextDecorationKinds: TALTextDecorationKinds := [];
          var Ltextdecorationline := ' ' + ALLowerCase(LParamList.ValueFromIndex[LIdx]) + ' '; // <span text-decoration-line="underline overline">
          if AlposW(' underline ',Ltextdecorationline) > 0 then LTextDecorationKinds := LTextDecorationKinds + [TALTextDecorationKind.Underline];
          if AlposW(' overline ',Ltextdecorationline) > 0 then LTextDecorationKinds := LTextDecorationKinds + [TALTextDecorationKind.overline];
          if AlposW(' line-through ',Ltextdecorationline) > 0 then LTextDecorationKinds := LTextDecorationKinds + [TALTextDecorationKind.LineThrough];
          ADecorationKinds.Add(LTextDecorationKinds);
        end
        else if ADecorationKinds.Count > 0 then ADecorationKinds.Add(ADecorationKinds[ADecorationKinds.Count - 1])
        else ADecorationKinds.Add(LOptions.DecorationKinds);
      end;
      //--
      if ADecorationStyles <> nil then begin
        var LDecorationStyle := ALLowerCase(LParamList.Values['text-decoration-style']); // <span text-decoration-style="solid">
             if (LDecorationStyle = 'solid') then ADecorationStyles.Add(TALTextDecorationStyle.Solid)
        else if (LDecorationStyle = 'double') then ADecorationStyles.Add(TALTextDecorationStyle.Double)
        else if (LDecorationStyle = 'dotted') then ADecorationStyles.Add(TALTextDecorationStyle.Dotted)
        else if (LDecorationStyle = 'dashed') then ADecorationStyles.Add(TALTextDecorationStyle.Dashed)
        else if (LDecorationStyle = 'wavy') then ADecorationStyles.Add(TALTextDecorationStyle.Wavy)
        else if ADecorationStyles.Count > 0 then ADecorationStyles.Add(ADecorationStyles[ADecorationStyles.Count - 1])
        else ADecorationStyles.Add(LOptions.DecorationStyle);
      end;
      //--
      If ADecorationThicknessMultipliers <> nil then begin
        var LDecorationThicknessMultiplierFloat: Single;
        if ALTryStrToFloat(LParamList.Values['text-decoration-thickness'], LDecorationThicknessMultiplierFloat, ALDefaultFormatSettingsW) then ADecorationThicknessMultipliers.Add(LDecorationThicknessMultiplierFloat) // <span text-decoration-thickness="3">
        else if ADecorationThicknessMultipliers.Count > 0 then ADecorationThicknessMultipliers.Add(ADecorationThicknessMultipliers[ADecorationThicknessMultipliers.Count - 1])
        else ADecorationThicknessMultipliers.Add(LOptions.DecorationThicknessMultiplier);
      end;
      //--
      If ADecorationColors <> nil then begin
        var LColorInt: Cardinal;
        if _TryStrColorToInt(LParamList.Values['text-decoration-color'], LColorInt) then ADecorationColors.Add(TalphaColor(LcolorInt)) // <span text-decoration-color="#xxxxxx">
        else if ADecorationColors.Count > 0 then ADecorationColors.Add(ADecorationColors[ADecorationColors.Count - 1])
        else ADecorationColors.Add(LOptions.DecorationColor);
      end;
      //--
      If ABackgroundColors <> nil then begin
        var LColorInt: Cardinal;
        if _TryStrColorToInt(LParamList.Values['background-color'], LColorInt) then ABackgroundColors.Add(TalphaColor(LcolorInt)) // <span background-color="#xxxxxx">
        else if ABackgroundColors.Count > 0 then ABackgroundColors.Add(ABackgroundColors[ABackgroundColors.Count - 1])
        else ABackgroundColors.Add(TalphaColors.Null);
      end;
      //--
      If ALineHeightMultipliers <> nil then begin
        var LLineHeightMultiplierFloat: Single;
        if ALTryStrToFloat(LParamList.Values['line-height'], LLineHeightMultiplierFloat, ALDefaultFormatSettingsW) then ALineHeightMultipliers.Add(LLineHeightMultiplierFloat) // <span line-height="1.6">
        else if ALineHeightMultipliers.Count > 0 then ALineHeightMultipliers.Add(ALineHeightMultipliers[ALineHeightMultipliers.Count - 1])
        else ALineHeightMultipliers.Add(LOptions.LineHeightMultiplier);
      end;
      //--
      If ALetterSpacings <> nil then begin
        var LLetterSpacingFloat: Single;
        if _TryStrPxValueToFloat(ALLowerCase(LParamList.Values['letter-spacing']), LLetterSpacingFloat) then ALetterSpacings.Add(LLetterSpacingFloat * LScale) // <span letter-spacing="3px">
        else if ALetterSpacings.Count > 0 then ALetterSpacings.Add(ALetterSpacings[ALetterSpacings.Count - 1])
        else ALetterSpacings.Add(LOptions.LetterSpacing);
      end;
      //--
      AImgSrc := LParamList.Values['src'];
      //--
      AImgWidth := ALStrToFloatDef(LParamList.Values['width'], 0, ALDefaultFormatSettingsW) * LScale;
      //--
      AImgHeight := ALStrToFloatDef(LParamList.Values['height'], 0, ALDefaultFormatSettingsW) * LScale;
    finally
      ALFreeAndNil(LParamList);
    end;
  end;
  {$ENDREGION}

  {$REGION '_getInfosFromTag'}
  procedure _getInfosFromTag(
              const ATag: String; // color="#ffffff" id="xxx"
              const ASpanIds: TALStringListW;
              const AFontFamilies: TALStringListW;
              const AFontSizes: Tlist<Single>;
              const AFontWeights: Tlist<TFontWeight>;
              const AFontSlants: TList<TFontSlant>;
              const AFontStretchs: TList<TFontStretch>;
              const AFontColors: Tlist<TalphaColor>;
              const ADecorationKinds: TList<TALTextDecorationKinds>;
              const ADecorationStyles: TList<TALTextDecorationStyle>;
              const ADecorationThicknessMultipliers: TList<Single>;
              const ADecorationColors: TList<TAlphaColor>;
              const ABackgroundColors: TList<TAlphaColor>;
              const ALineHeightMultipliers: TList<Single>;
              const ALetterSpacings: TList<Single>); overload;
  begin
    Var LImgSrc: String;
    Var LImgWidth: Single;
    Var LImgHeight: Single;
    _getInfosFromTag(
      ATag, // const ATag: String; // color="#ffffff" id="xxx"
      ASpanIds, // const ASpanIds: TALStringListW;
      AFontFamilies, // const AFontFamilies: TALStringListW;
      AFontSizes, // const AFontSizes: Tlist<Single>;
      AFontWeights, // const AFontWeights: Tlist<TFontWeight>;
      AFontSlants, // const AFontSlants: TList<TFontSlant>;
      AFontStretchs, // const AFontStretchs: TList<TFontStretch>;
      AFontColors, // const AFontColors: Tlist<TalphaColor>;
      ADecorationKinds, // const ADecorationKinds: TList<TALTextDecorationKinds>;
      ADecorationStyles, // const ADecorationStyles: TList<TALTextDecorationStyle>;
      ADecorationThicknessMultipliers, // const ADecorationThicknessMultipliers: TList<Single>;
      ADecorationColors, // const ADecorationColors: TList<TAlphaColor>;
      ABackgroundColors, // const ABackgroundColors: TList<TAlphaColor>;
      ALineHeightMultipliers, // const ALineHeightMultipliers: TList<Single>;
      ALetterSpacings, // const ALetterSpacings: TList<Single>;
      LImgSrc, // out AImgSrc: String;
      LImgWidth, // out AImgWidth: Single;
      LImgHeight); // out AImgHeight: Single)
  end;
  {$ENDREGION}

  {$REGION '_getInfosFromTag'}
  procedure _getInfosFromTag(
              const ATag: String; // color="#ffffff" id="xxx"
              const ASpanIds: TALStringListW;
              const AFontFamilies: TALStringListW;
              const AFontSizes: Tlist<Single>;
              const AFontColors: Tlist<TalphaColor>); overload;
  begin
    Var LImgSrc: String;
    Var LImgWidth: Single;
    Var LImgHeight: Single;
    _getInfosFromTag(
      ATag, // const ATag: String; // color="#ffffff" id="xxx"
      ASpanIds, // const ASpanIds: TALStringListW;
      AFontFamilies, // const AFontFamilies: TALStringListW;
      AFontSizes, // const AFontSizes: Tlist<Single>;
      nil, // const AFontWeights: Tlist<TFontWeight>;
      nil, // const AFontSlants: TList<TFontSlant>;
      nil, // const AFontStretchs: TList<TFontStretch>;
      AFontColors, // const AFontColors: Tlist<TalphaColor>;
      nil, // const ADecorationKinds: TList<TALTextDecorationKinds>;
      nil, // const ADecorationStyles: TList<TALTextDecorationStyle>;
      nil, // const ADecorationThicknessMultipliers: TList<Single>;
      nil, // const ADecorationColors: TList<TAlphaColor>;
      nil, // const ABackgroundColors: TList<TAlphaColor>;
      nil, // const ALineHeightMultipliers: TList<Single>;
      nil, // const ALetterSpacings: TList<Single>;
      LImgSrc, // out AImgSrc: String;
      LImgWidth, // out AImgWidth: Single;
      LImgHeight); // out AImgHeight: Single)
  end;
  {$ENDREGION}

  {$REGION '_getInfosFromTag'}
  procedure _getInfosFromTag(
              const ATag: String; // color="#ffffff" id="xxx"
              const ASpanIds: TALStringListW); overload;
  begin
    Var LImgSrc: String;
    Var LImgWidth: Single;
    Var LImgHeight: Single;
    _getInfosFromTag(
      ATag, // const ATag: String; // color="#ffffff" id="xxx"
      ASpanIds, // const ASpanIds: TALStringListW;
      nil, // const AFontFamilies: TALStringListW;
      nil, // const AFontSizes: Tlist<Single>;
      nil, // const AFontWeights: Tlist<TFontWeight>;
      nil, // const AFontSlants: TList<TFontSlant>;
      nil, // const AFontStretchs: TList<TFontStretch>;
      nil, // const AFontColors: Tlist<TalphaColor>;
      nil, // const ADecorationKinds: TList<TALTextDecorationKinds>;
      nil, // const ADecorationStyles: TList<TALTextDecorationStyle>;
      nil, // const ADecorationThicknessMultipliers: TList<Single>;
      nil, // const ADecorationColors: TList<TAlphaColor>;
      nil, // const ABackgroundColors: TList<TAlphaColor>;
      nil, // const ALineHeightMultipliers: TList<Single>;
      nil, // const ALetterSpacings: TList<Single>;
      LImgSrc, // out AImgSrc: String;
      LImgWidth, // out AImgWidth: Single;
      LImgHeight); // out AImgHeight: Single)
  end;
  {$ENDREGION}

  {$REGION '_getInfosFromTag'}
  procedure _getInfosFromTag(
              const ATag: String; // color="#ffffff" id="xxx"
              out AImgSrc: String;
              out AImgWidth: Single;
              out AImgHeight: Single); overload;
  begin
    _getInfosFromTag(
      ATag, // const ATag: String; // color="#ffffff" id="xxx"
      nil, // const ASpanIds: TALStringListW;
      nil, // const AFontFamilies: TALStringListW;
      nil, // const AFontSizes: Tlist<Single>;
      nil, // const AFontWeights: Tlist<TFontWeight>;
      nil, // const AFontSlants: TList<TFontSlant>;
      nil, // const AFontStretchs: TList<TFontStretch>;
      nil, // const AFontColors: Tlist<TalphaColor>;
      nil, // const ADecorationKinds: TList<TALTextDecorationKinds>;
      nil, // const ADecorationStyles: TList<TALTextDecorationStyle>;
      nil, // const ADecorationThicknessMultipliers: TList<Single>;
      nil, // const ADecorationColors: TList<TAlphaColor>;
      nil, // const ABackgroundColors: TList<TAlphaColor>;
      nil, // const ALineHeightMultipliers: TList<Single>;
      nil, // const ALetterSpacings: TList<Single>;
      AImgSrc, // out AImgSrc: String;
      AImgWidth, // out AImgWidth: Single;
      AImgHeight); // out AImgHeight: Single)
  end;
  {$ENDREGION}

  {$REGION '_getFontFamily'}
  {$IF not defined(ALSkiaEngine)}
  function _getFontFamily(const AFontFamilies: String): String;
  begin
    Result := '';
    var LFontFamilies := AFontFamilies.Split([',', #13, #10], TStringSplitOptions.ExcludeEmpty);
    for var I := low(LFontFamilies) to high(LFontFamilies) do begin
      Result := ALTrim(LFontFamilies[I]);
      if Result <> '' then break;
    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_getFontStyleExt'}
  {$IF not defined(ALSkiaEngine)}
  function _getFontStyleExt(const AFontWeight: TFontWeight; const AFontSlant: TFontSlant; const AFontStretch: TFontStretch; const ADecorationKinds: TALTextDecorationKinds): TFontStyleExt;
  begin
    var LOtherStyles: TFontStyles := [];
    if TALTextDecorationKind.Underline in ADecorationKinds then LOtherStyles := LOtherStyles + [TFontStyle.fsUnderline];
    if TALTextDecorationKind.LineThrough in ADecorationKinds then LOtherStyles := LOtherStyles + [TFontStyle.fsStrikeOut];
    Result := TFontStyleExt.Create(AFontWeight, AFontSlant, AFontStretch, LOtherStyles);
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_Paint'}
  {$IF not defined(ALSkiaEngine) and defined(ANDROID)}
  var
    _Paint: JPaint;
    _CurrFontFamily: String;
    _CurrFontSize: single;
    _CurrFontWeight: TFontWeight;
    _CurrFontSlant: TFontSlant;
    _CurrFontStretch: TFontStretch;
    _CurrFontColor: TalphaColor;
    _CurrDecorationKinds: TALTextDecorationKinds;
    _CurrDecorationStyle: TALTextDecorationStyle;
    _CurrDecorationThicknessMultiplier: Single;
    _CurrDecorationColor: TAlphaColor;
    _CurrLetterSpacing: Single;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_UpdatePaint'}
  {$IF not defined(ALSkiaEngine) and defined(ANDROID)}
  procedure _UpdatePaint(
              const AFontFamily: String;
              const AFontSize: single;
              const AFontWeight: TFontWeight;
              const AFontSlant: TFontSlant;
              const AFontStretch: TFontStretch;
              const AFontColor: TalphaColor;
              const ADecorationKinds: TALTextDecorationKinds;
              const ADecorationStyle: TALTextDecorationStyle;
              const ADecorationThicknessMultiplier: Single;
              const ADecorationColor: TAlphaColor;
              const ALetterSpacing: Single);
  begin

    if (AFontFamily = _CurrFontFamily) and
       (SameValue(AFontSize, _CurrFontSize, TEpsilon.FontSize)) and
       (AFontWeight = _CurrFontWeight) and
       (AFontSlant = _CurrFontSlant) and
       (AFontStretch = _CurrFontStretch) and
       (AFontColor = _CurrFontColor) and
       (ADecorationKinds = _CurrDecorationKinds) and
       (ADecorationStyle = _CurrDecorationStyle) and
       (SameValue(ADecorationThicknessMultiplier, _CurrDecorationThicknessMultiplier, TEpsilon.Scale)) and
       (ADecorationColor = _CurrDecorationColor) and
       (SameValue(ALetterSpacing, _CurrLetterSpacing, TEpsilon.FontSize)) then exit;

    _CurrFontFamily := AFontFamily;
    _CurrFontSize := AFontSize;
    _CurrFontWeight := AFontWeight;
    _CurrFontSlant := AFontSlant;
    _CurrFontStretch := AFontStretch;
    _CurrFontColor := AFontColor;
    _CurrDecorationKinds := ADecorationKinds;
    _CurrDecorationStyle := ADecorationStyle;
    _CurrDecorationThicknessMultiplier := ADecorationThicknessMultiplier;
    _CurrDecorationColor := ADecorationColor;
    _CurrLetterSpacing := ALetterSpacing;

    var LFontFamily := _getFontFamily(AFontFamily);
    var LTypeface: JTypeFace := TALFontManager.GetCustomTypeFace(LFontFamily);
    if LTypeface = nil then begin
      var LFontStyles: TFontStyles := [];
      if AFontWeight in [TFontWeight.Bold,
                         TFontWeight.UltraBold,
                         TFontWeight.Black,
                         TFontWeight.UltraBlack] then LFontStyles := LFontStyles + [TFontStyle.fsBold];
      if AFontSlant in [TFontSlant.Italic, TFontSlant.Oblique] then LFontStyles := LFontStyles + [TFontStyle.fsItalic];
      LTypeface := TJTypeface.JavaClass.create(StringToJString(LFontFamily), ALfontStyleToAndroidStyle(LFontStyles));
    end;
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
    _Paint.setTypeface(LTypeface);
    _Paint.setTextSize(AFontSize);
    if (Not SameValue(AfontSize, 0, TEpsilon.FontSize)) then
      _Paint.setLetterSpacing(ALetterSpacing / AfontSize)
    else
      _Paint.setLetterSpacing(0);
    _Paint.setColor(integer(AFontColor));
    _Paint.setUnderlineText(TALTextDecorationKind.Underline in ADecorationKinds);
    _Paint.setStrikeThruText(TALTextDecorationKind.LineThrough in ADecorationKinds);

  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_CreateAttributedString'}
  {$IF not defined(ALSkiaEngine) and (defined(ALAppleOS))}
  function _CreateAttributedString(
             const AText: String;
             const AFontFamily: String;
             const AFontSize: single;
             const AFontWeight: TFontWeight;
             const AFontSlant: TFontSlant;
             const AFontStretch: TFontStretch;
             const AFontColor: TalphaColor;
             const ADecorationKinds: TALTextDecorationKinds;
             const ADecorationStyle: TALTextDecorationStyle;
             const ADecorationThicknessMultiplier: Single;
             const ADecorationColor: TAlphaColor;
             const ALetterSpacing: Single;
             const ADirection: TALTextDirection): CFMutableAttributedStringRef;
  begin
    var LFontAlphaColor := TAlphaColorCGFloat.Create(AFontColor);
    var LFontCGColor := CGColorCreate(ALGetGlobalCGColorSpace, @LFontAlphaColor);
    if LFontCGColor = nil then raise Exception.Create('Failed to create CGColor');
    try
      var LCTFontRef := ALCreateCTFontRef(AFontFamily, AFontSize, AFontWeight, AFontSlant);
      try
        var LTextString := CFStringCreateWithCharacters(kCFAllocatorDefault, @AText[Low(string)], Length(AText));
        if LTextString = nil then raise Exception.Create('Failed to create CFString');
        try
          var LTextRange := CFRangeMake(0, CFStringGetLength(LTextString));
          Result := CFAttributedStringCreateMutable(kCFAllocatorDefault{alloc}, 0{maxLength});
          if Result = nil then raise Exception.Create('Failed to create CFMutableAttributedStringRef');
          try
            CFAttributedStringReplaceString(Result, CFRangeMake(0, 0), LTextString);
            CFAttributedStringBeginEditing(Result);
            Try
              CFAttributedStringSetAttribute(Result, LTextRange, kCTFontAttributeName, LCTFontRef);
              //--
              CFAttributedStringSetAttribute(Result, LTextRange, kCTForegroundColorAttributeName, LFontCGColor);
              //--
              if ADirection = TALTextDirection.RightToLeft then begin
                var LSettings: array of CTParagraphStyleSetting;
                SetLength(LSettings, 1);
                //--
                //kCTParagraphStyleSpecifierBaseWritingDirection
                //The base writing direction of the lines. Type: CTWritingDirection. Default: kCTWritingDirectionNatural. Application: CTFramesetter, CTTypesetter.
                //* kCTWritingDirectionNatural: The writing direction is algorithmically determined using the Unicode Bidirectional Algorithm rules P2 and P3.
                //* kCTWritingDirectionLeftToRight: The writing direction is left to right.
                //* kCTWritingDirectionRightToLeft: The writing direction is right to left.
                var LWritingDirection: Byte := kCTWritingDirectionRightToLeft;
                SetLength(LSettings, length(LSettings) + 1);
                LSettings[high(LSettings)].spec := kCTParagraphStyleSpecifierBaseWritingDirection;
                LSettings[high(LSettings)].valueSize := SizeOf(LWritingDirection);
                LSettings[high(LSettings)].value := @LWritingDirection;
                //--
                var LParagraphStyle := CTParagraphStyleCreate(@LSettings[0], Length(LSettings));
                try
                  CFAttributedStringSetAttribute(Result, LTextRange, kCTParagraphStyleAttributeName, LParagraphStyle);
                finally
                  CFRelease(LParagraphStyle);
                end;
              end;
              //--
              if TALTextDecorationKind.Underline in ADecorationKinds then begin
                var LValue: Cardinal;
                case ADecorationStyle of
                  TALTextDecorationStyle.Double: LValue := kCTUnderlineStyleDouble;
                  TALTextDecorationStyle.Dotted: LValue := kCTUnderlineStyleSingle or kCTUnderlinePatternDot;
                  TALTextDecorationStyle.Dashed: LValue := kCTUnderlineStyleSingle or kCTUnderlinePatternDash;
                  else {Solid, Wavy} LValue := kCTUnderlineStyleSingle;
                end;
                var LUnderlineStyleAttributeValue := CFNumberCreate(nil, kCFNumberSInt32Type, @LValue);
                if LUnderlineStyleAttributeValue = nil then raise Exception.Create('Failed to create CFNumber');
                try
                  CFAttributedStringSetAttribute(Result, LTextRange, kCTUnderlineStyleAttributeName, LUnderlineStyleAttributeValue);
                finally
                  CFRelease(LUnderlineStyleAttributeValue);
                end;
                //--
                if ADecorationColor <> TAlphaColors.Null then begin
                  var LDecorationAlphaColor := TAlphaColorCGFloat.Create(ADecorationColor);
                  var LDecorationCGColor := CGColorCreate(ALGetGlobalCGColorSpace, @LDecorationAlphaColor);
                  if LDecorationCGColor = nil then raise Exception.Create('Failed to create CGColor');
                  try
                    CFAttributedStringSetAttribute(Result, LTextRange, kCTUnderlineColorAttributeName, LDecorationCGColor);
                  finally
                    CGColorRelease(LDecorationCGColor);
                  end;
                end;
              end;
              //--
              if not sameValue(ALetterSpacing, 0, TEpsilon.FontSize) then begin
                var LValue: Single := ALetterSpacing;
                var LKernAttributeValue := CFNumberCreate(nil, kCFNumberFloat32Type, @LValue);
                if LKernAttributeValue = nil then raise Exception.Create('Failed to create CFNumber');
                try
                  CFAttributedStringSetAttribute(Result, LTextRange, kCTKernAttributeName, LKernAttributeValue);
                finally
                  CFRelease(LKernAttributeValue);
                end;
              end;
            finally
              CFAttributedStringEndEditing(Result);
            end;
          Except
            CFRelease(Result);
            Raise;
          end;
        finally
          CFRelease(LTextString);
        end;
      finally
        CFRelease(LCTFontRef);
      end;
    finally
      CGColorRelease(LFontCGColor);
    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_GetFontMetrics'}
  {$IF not defined(ALSkiaEngine)}
  function _GetFontMetrics(
             const AFontFamily: String;
             const AFontSize: single;
             const AFontWeight: TFontWeight;
             const AFontSlant: TFontSlant): TALFontMetrics;
  begin
    Result := ALGetFontMetrics(
                AFontFamily, // const AFontFamily: String;
                AFontSize, // const AFontSize: single;
                AFontWeight, // const AFontWeight: TFontWeight;
                AFontSlant); // const AFontSlant: TFontSlant;
    if Result.Leading > 0 then begin
      Result.Ascent := Result.Ascent - (Result.Leading/2);
      Result.Descent := Result.Descent + (Result.Leading/2);
      Result.leading := 0;
    end;
  end;
  {$ENDIF}
  {$ENDREGION}

  {$REGION '_BreakText'}
  {$IF not defined(ALSkiaEngine)}
  function _BreakText(
             const AText: String;
             const AFontFamily: String;
             const AFontSize: single;
             const AFontWeight: TFontWeight;
             const AFontSlant: TFontSlant;
             const AFontStretch: TFontStretch;
             const AFontColor: TalphaColor;
             const ADecorationKinds: TALTextDecorationKinds;
             const ADecorationStyle: TALTextDecorationStyle;
             const ADecorationThicknessMultiplier: Single;
             const ADecorationColor: TAlphaColor;
             const ALetterSpacing: Single;
             const ADirection: TALTextDirection;
             const AMaxWidth: Single;
             const AHardBreak: Boolean;
             out AMeasuredWidth: Single;
             out AMeasuredHeight: Single): integer;
  begin

    If CompareValue(AMaxWidth, 0, TEpsilon.Position) <= 0 then begin
      Result := 0;
      AMeasuredWidth := 0;
      AMeasuredHeight := 0;
      Exit;
    end;

    {$IF defined(ANDROID)}

    _UpdatePaint(
      AFontFamily,
      AFontSize,
      AFontWeight,
      AFontSlant,
      AFontStretch,
      AFontColor,
      ADecorationKinds,
      ADecorationStyle,
      ADecorationThicknessMultiplier,
      ADecorationColor,
      ALetterSpacing);

    // http://stackoverflow.com/questions/7549182/android-paint-measuretext-vs-gettextbounds
    // * getTextBounds will return the absolute (ie integer rounded) minimal bounding rect
    // * measureText adds some advance value to the text on both sides, while getTextBounds computes minimal
    //   bounds where given text will fit - getTextBounds is also not accurate at all regarding the height,
    //   it's return for exemple 9 when height = 11
    var LMeasuredWidth := TJavaArray<Single>.Create(1);
    try

      // breakText
      Result := _Paint.breakText(
                  StringToJString(AText) {text},
                  true {measureForwards},
                  AMaxWidth, {maxWidth}
                  LMeasuredWidth {measuredWidth});

      {$IF defined(debug)}
      if (Result < 0) or (Result > AText.Length) then
        Raise Exception.create('Error B8C38471-D5C6-43E9-8A2D-4E1BDDB3713A');
      {$ENDIF}

      // Not enough space to write at least one character
      if Result = 0 then begin
        AMeasuredWidth := 0;
        AMeasuredHeight := 0;
      end

      // Enough space to write all characters
      else If Result = AText.Length then begin
        AMeasuredWidth := LMeasuredWidth[0];
        AMeasuredHeight := 0;
      end

      // The text must be broken
      else begin

        // Calculate the correct position where the text should break
        var LBreakPosition := _findLastBreakPosition(AText, Result, AHardBreak);

        // No good position found
        if LBreakPosition = 0 then begin
          if AHardBreak then begin
            AMeasuredWidth := LMeasuredWidth[0];
            AMeasuredHeight := 0;
          end
          else begin
            Result := 0;
            AMeasuredWidth := 0;
            AMeasuredHeight := 0;
          end;
        end

        // Result already fell within a good position
        else if LBreakPosition = result then begin
          AMeasuredWidth := LMeasuredWidth[0];
          AMeasuredHeight := 0;
        end

        // Result did not fall within a good position
        else begin
          var LText := AlCopyStr(AText,1,LBreakPosition);
          Result := _Paint.breakText(
                      StringToJString(LText) {text},
                      true {measureForwards},
                      65535, {maxWidth}
                      LMeasuredWidth {measuredWidth});
          {$IF defined(debug)}
          if result <> LBreakPosition then
            Raise Exception.Create('Error 195DA1C6-5054-45C2-824C-B2C1C3ED3FE0');
          {$ENDIF}
          AMeasuredWidth := LMeasuredWidth[0];
          AMeasuredHeight := 0;
        end;
      end;

    finally
      ALFreeAndNil(LMeasuredWidth);
    end;

    {$ENDIF}

    {$IF defined(ALAppleOS)}

    var LAttributedString := _CreateAttributedString(
                               AText, // const AText: String;
                               AFontFamily, // const AFontFamily: String;
                               AFontSize, // const AFontSize: single;
                               AFontWeight, // const AFontWeight: TFontWeight;
                               AFontSlant, // const AFontSlant: TFontSlant;
                               AFontStretch, // const AFontStretch: TFontStretch;
                               AFontColor, // const AFontColor: TalphaColor;
                               ADecorationKinds, // const ADecorationKinds: TALTextDecorationKinds;
                               ADecorationStyle, // const ADecorationStyle: TALTextDecorationStyle;
                               ADecorationThicknessMultiplier, // const ADecorationThicknessMultiplier: Single;
                               ADecorationColor, // const ADecorationColor: TAlphaColor;
                               ALetterSpacing, // const ALetterSpacing: Single;
                               ADirection); // const ADirection: TALTextDirection
    try
      var LFrameSetter := CTFramesetterCreateWithAttributedString(CFAttributedStringRef(LAttributedString));
      if LFrameSetter = nil then raise Exception.Create('Failed to create CTFramesetter');
      try

        // Break the text using CTFramesetterSuggestFrameSizeWithConstraints
        var Lmetrics := _GetFontMetrics(
                          AFontFamily, // const AFontFamily: String;
                          AFontSize, // const AFontSize: single;
                          AFontWeight, // const AFontWeight: TFontWeight;
                          AFontSlant); // const AFontSlant: TFontSlant;
        var LfitRange: CFRange;
        var LSize := CTFramesetterSuggestFrameSizeWithConstraints(
                       LFrameSetter, //framesetter: CTFramesetterRef;
                       CFRangeMake(0, Atext.Length), // stringRange: CFRange;
                       nil, // frameAttributes: CFDictionaryRef;
                       // Multiplying by 1.8 to be on the safe side, since in the event of a font fallback,
                       // the fallback font could have a larger ascent/descent than the original font.
                       CGSizeMake(AMaxWidth, ((-1 *Lmetrics.ascent) + Lmetrics.descent) * 1.8), // constraints: CGSize;
                       @LfitRange); // fitRange: PCFRange
        Result := LfitRange.length;
        {$IF defined(debug)}
        if (Result < 0) or (Result > AText.Length) then
          Raise Exception.create('Error B8C38471-D5C6-43E9-8A2D-4E1BDDB3713A');
        {$ENDIF}

        // https://stackoverflow.com/questions/78144915/ctframesettercreateframe-and-kctparagraphstylespecifierfirstlineheadindent
        if (Result < AText.Length) and (not AHardBreak) then begin
          var LBreakPosition := _findLastBreakPosition(AText, Result, AHardBreak);
          if LBreakPosition <= 0 then begin
            Result := 0;
            AMeasuredWidth := 0;
            AMeasuredHeight := 0;
            exit;
          end;
        end;

        // Get the trailing whitespace
        var LTrailingWhitespaceWidth: double := 0;
        If Result = Atext.Length then begin
          Var LLineAttributedString := _CreateAttributedString(
                                         ALCopyStr(Atext,1,Result), // const AText: String;
                                         AFontFamily, // const AFontFamily: String;
                                         AFontSize, // const AFontSize: single;
                                         AFontWeight, // const AFontWeight: TFontWeight;
                                         AFontSlant, // const AFontSlant: TFontSlant;
                                         AFontStretch, // const AFontStretch: TFontStretch;
                                         AFontColor, // const AFontColor: TalphaColor;
                                         ADecorationKinds, // const ADecorationKinds: TALTextDecorationKinds;
                                         ADecorationStyle, // const ADecorationStyle: TALTextDecorationStyle;
                                         ADecorationThicknessMultiplier, // const ADecorationThicknessMultiplier: Single;
                                         ADecorationColor, // const ADecorationColor: TAlphaColor;
                                         ALetterSpacing, // const ALetterSpacing: Single;
                                         ADirection); // const ADirection: TALTextDirection
          try
            var LLine := CTLineCreateWithAttributedString(CFAttributedStringRef(LLineAttributedString));
            try
              LTrailingWhitespaceWidth := CTLineGetTrailingWhitespaceWidth(Lline);
            finally
              CFRelease(LLine);
            end;
          finally
            CFRelease(LLineAttributedString);
          end;
        end;

        // Update AMeasuredWidth
        AMeasuredWidth := LSize.width + LTrailingWhitespaceWidth;
        AMeasuredHeight := LSize.height;

        {$IF defined(DEBUG)}
        //ALLog(
        //  'ALCreateMultiLineTextDrawable._BreakText',
        //  'Text: '+ Atext + ' | '+
        //  'MaxWidth: '+ ALFloatToStrW(AMaxWidth, ALDefaultFormatSettingsW) + ' | '+
        //  'Result: ' + ALInttoStrW(Result) + ' | '+
        //  'MeasuredWidth: ' + ALFloatToStrW(AMeasuredWidth, ALDefaultFormatSettingsW) + ' | '+
        //  'MeasuredHeight: ' + ALFloatToStrW(AMeasuredHeight, ALDefaultFormatSettingsW) + ' | '+
        //  'TrailingWhitespaceWidth: ' + ALFloatToStrW(LTrailingWhitespaceWidth, ALDefaultFormatSettingsW) + ' | '+
        //  'Metrics.ascent: ' + ALFloatToStrW(Lmetrics.Ascent, ALDefaultFormatSettingsW) + ' | '+
        //  'Metrics.descent: ' + ALFloatToStrW(Lmetrics.descent, ALDefaultFormatSettingsW));
        {$ENDIF}

      finally
        CFRelease(LFrameSetter);
      end;
    finally
      CFRelease(LAttributedString);
    end;

    {$ENDIF}

    {$IF defined(MSWINDOWS)}

    // Initially, I wanted to use the Windows API function GetTextExtentPoint32 to retrieve
    // text dimensions. However, this function does not return accurate results for fonts
    // with different weights. For example, when using the 'Segoe UI' font with a medium weight,
    // it returns dimensions as if the font were set to regular weight, which is incorrect.
    //
    //{$REGION 'TWinBitmap'}
    //{$IF not defined(ALSkiaEngine) and defined(MSWINDOWS)}
    //Type
    //  TWinBitmap = record
    //    BitmapInfo: TBitmapInfo;
    //    DC: HDC;
    //    Bitmap: HBITMAP;
    //    Bits: PAlphaColorRecArray;
    //    Width, Height: Integer;
    //  end;
    //{$ENDIF}
    //{$ENDREGION}
    //
    //{$REGION '_MeasureBitmap'}
    //{$IF not defined(ALSkiaEngine) and defined(MSWINDOWS)}
    //var
    //  _MeasureBitmap: TWinBitmap;
    //{$ENDIF}
    //{$ENDREGION}
    //
    //{$REGION '_CreateMeasureBitmap'}
    //{$IF not defined(ALSkiaEngine) and defined(MSWINDOWS)}
    //procedure _CreateMeasureBitmap;
    //begin
    //  _MeasureBitmap.Width := 1;
    //  _MeasureBitmap.Height := 1;
    //  _MeasureBitmap.DC := CreateCompatibleDC(0);
    //  if _MeasureBitmap.DC = 0 then raiseLastOsError;
    //  ZeroMemory(@(_MeasureBitmap.BitmapInfo.bmiHeader), SizeOf(TBitmapInfoHeader));
    //  _MeasureBitmap.BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    //  _MeasureBitmap.BitmapInfo.bmiHeader.biWidth := 1;
    //  _MeasureBitmap.BitmapInfo.bmiHeader.biHeight := -1;
    //  _MeasureBitmap.BitmapInfo.bmiHeader.biPlanes := 1;
    //  _MeasureBitmap.BitmapInfo.bmiHeader.biCompression := BI_RGB;
    //  _MeasureBitmap.BitmapInfo.bmiHeader.biBitCount := 32;
    //  _MeasureBitmap.Bitmap := CreateDIBSection(_MeasureBitmap.DC, _MeasureBitmap.BitmapInfo, DIB_RGB_COLORS, Pointer(_MeasureBitmap.Bits), 0, 0);
    //  if _MeasureBitmap.Bitmap = 0 then raiseLastOsError;
    //  if SetMapMode(_MeasureBitmap.DC, MM_TEXT) = 0 then raiseLastOsError;
    //  if SelectObject(_MeasureBitmap.DC, _MeasureBitmap.Bitmap) = 0 then raiseLastOsError;
    //end;
    //{$ENDIF}
    //{$ENDREGION}
    //
    //{$REGION '_FreeMeasureBitmap'}
    //{$IF not defined(ALSkiaEngine) and defined(MSWINDOWS)}
    //procedure _FreeMeasureBitmap;
    //begin
    //  _MeasureBitmap.Bits := nil;
    //  if not DeleteObject(_MeasureBitmap.Bitmap) then raiseLastOsError;
    //  if not DeleteDC(_MeasureBitmap.DC) then raiseLastOsError;
    //end;
    //{$ENDIF}
    //{$ENDREGION}
    //
    //{$IF defined(MSWINDOWS)}
    //_CreateMeasureBitmap;
    //{$ENDIF}
    //
    //{$IF defined(MSWINDOWS)}
    //_FreeMeasureBitmap;
    //{$ENDIF}
    //
    // Since the Windows API only works with integers, I multiply the font size by 100
    // and later divide the result by 100 to achieve better precision.
    //
    //var LFontFamily := _getFontFamily(AFontFamily);
    //var LFont := CreateFont(
    //               -Round(AFontSize*100), // nHeight: Integer;
    //               0, // nWidth: Integer;
    //               0, // nEscapement: Integer;
    //               0, // nOrientaion: Integer;
    //               FontWeightToWinapi(AFontWeight), // fnWeight: Integer;
    //               Cardinal(not AFontSlant.IsRegular), // fdwItalic: DWORD
    //               cardinal(TALTextDecorationKind.Underline in ADecorationKinds), // fdwUnderline: DWORD
    //               cardinal(TALTextDecorationKind.LineThrough in ADecorationKinds), // fdwStrikeOut: DWORD
    //               DEFAULT_CHARSET, // fdwCharSet: DWORD
    //               OUT_DEFAULT_PRECIS, // fdwOutputPrecision: DWORD
    //               CLIP_DEFAULT_PRECIS, // fdwClipPrecision: DWORD
    //               DEFAULT_QUALITY, // fdwQuality: DWORD
    //               DEFAULT_PITCH or FF_DONTCARE, // fdwPitchAndFamily: DWORD
    //               PChar(LFontFamily)); // lpszFace: LPCWSTR
    //if LFont = 0 then raiseLastOsError;
    //try
    //  if SelectObject(_MeasureBitmap.DC, LFont) = 0 then raiseLastOsError;
    //  var LText: String := Atext;
    //  Result := LText.Length;
    //  While Result > 0 do begin
    //    var LSize: TSize;
    //    if not GetTextExtentPoint32(_MeasureBitmap.DC, LText, LText.Length, LSize) then raiseLastOsError;
    //    If CompareValue(LSize.Width, AMaxWidth*100, Tepsilon.Position) <= 0 then begin
    //      AMeasuredWidth := LSize.Width/100;
    //      AMeasuredHeight := LSize.Height/100;
    //      exit;
    //    end;
    //    Result := _findLastBreakPosition(LText, Result-1, AHardBreak);
    //    If Result > 0 then
    //      LText := LText.Remove(Result);
    //  end;
    //  AMeasuredWidth := 0;
    //  AMeasuredHeight := 0;
    //finally
    //  if not DeleteObject(LFont) then raiseLastOsError;
    //end;

    var LLayout := TTextLayoutManager.DefaultTextLayout.Create;
    try
      var LText: String := Atext;
      Result := LText.Length;
      While Result > 0 do begin
        LLayout.BeginUpdate;
        LLayout.Font.Family := _getFontFamily(AFontFamily);
        LLayout.Font.StyleExt := _getFontStyleExt(AFontWeight, AFontSlant, AFontStretch, ADecorationKinds);
        LLayout.Font.Size := aFontSize;
        LLayout.MaxSize := Tpointf.Create(65535, 65535);
        LLayout.Trimming := TTextTrimming.Character;
        LLayout.VerticalAlign := TTextAlign.Leading;
        LLayout.HorizontalAlign := TTextAlign.Leading;
        LLayout.WordWrap := false;
        LLayout.Text := Ltext;
        LLayout.EndUpdate;
        AMeasuredWidth := LLayout.TextWidth;
        AMeasuredHeight := LLayout.TextHeight;
        If CompareValue(AMeasuredWidth, AMaxWidth, Tepsilon.Position) <= 0 then exit;
        var LPrevResult: Integer := Result;
        Result := _findLastBreakPosition(LText, Result-1, AHardBreak);
        If Result > 0 then LText := LText.Remove(Result)
        else if AHardBreak then begin
          Result := LPrevResult - 1;
          if LText[Result].IsHighSurrogate then dec(result);
          If Result > 0 then LText := LText.Remove(Result);
        end;
      end;
      AMeasuredWidth := 0;
      AMeasuredHeight := 0;
    finally
      ALFreeAndNil(LLayout);
    end;

    {$ENDIF}

  end;
  {$ENDIF}
  {$ENDREGION}

begin
  if AText = '' then begin
    ARect.Width := 0;
    ARect.Height := 0;
    Exit;
  end;

  var LCanvasMatrix: TMatrix;
  var LCanvasScale: Single;
  if AOptions.AlignToPixel then ALExtractMatrixFromCanvas(Acanvas, LCanvasMatrix, LCanvasScale)
  else begin
    LCanvasMatrix := TMatrix.Identity;
    LCanvasScale := 1;
  end;
  LScale := AOptions.Scale;
  LOptions := TALMultiLineTextOptions.Create;
  LOptions.Assign(AOptions);
  LOptions.ScaleAndAlignProperties(LCanvasScale);
  ARect.Top := ARect.Top * LScale;
  ARect.right := ARect.right * LScale;
  ARect.left := ARect.left * LScale;
  ARect.bottom := ARect.bottom * LScale;
  if LOptions.AlignToPixel then
    ARect := ALAlignToPixelRound(ARect, LCanvasMatrix, LCanvasScale, TEpsilon.Position);
  Try

    {$REGION 'SKIA'}
    {$IF defined(ALSkiaEngine)}

    // Init out var
    ATextBroken := false;
    AAllTextDrawn := True;
    setlength(AElements, 0);

    // Init local var
    var LPrevInsertEllipsisAt := MaxInt;
    var LInsertEllipsisAt := MaxInt;
    var LMaxLines: Integer;
    if LOptions.MaxLines > 0 then LMaxLines := LOptions.MaxLines
    else LMaxLines := MaxInt;
    var LFontFamilies := TALStringListW.create;
    var LFontSizes := Tlist<Single>.Create;
    var LFontWeights:= Tlist<TFontWeight>.Create;
    var LFontSlants := TList<TFontSlant>.Create;
    var LFontStretchs := TList<TFontStretch>.Create;
    var LFontColors := Tlist<TAlphaColor>.create;
    var LDecorationKinds := TList<TALTextDecorationKinds>.Create;
    var LDecorationStyles := TList<TALTextDecorationStyle>.Create;
    var LDecorationThicknessMultipliers := TList<Single>.Create;
    var LDecorationColors := TList<TAlphaColor>.Create;
    var LBackgroundColors := TList<TAlphaColor>.Create;
    var LLineHeightMultipliers := TList<Single>.Create;
    var LLetterSpacings := TList<Single>.Create;
    var LSpanIDs := TALStringListW.create;
    var LPlaceHolders := TALStringListW.create;
    var LRangeIndexes := Tlist<TRangeIndex>.create;
    var LPaint := ALSkCheckHandle(sk4d_paint_create);
    try

      // Init LPaint
      sk4d_paint_set_antialias(LPaint, true);
      sk4d_paint_set_dither(LPaint, true);

      // Format LText
      var Ltext: String := ALTrimRight(AText);
      if LOptions.TextIsHtml then begin
        LText := ALStringReplaceW(Ltext, #13, ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, #10, ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, #9, ' ', [RfReplaceALL]);
        While ALPosW('  ', LText) > 0 do
          LText := ALStringReplaceW(Ltext, '  ', ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, '<br>', #10, [RfReplaceALL, RfIgnoreCase]);
        LText := ALStringReplaceW(Ltext, '<br/>', #10, [RfReplaceALL, RfIgnoreCase]);
        While ALPosW(' '#10, LText) > 0 do
          LText := ALStringReplaceW(Ltext, ' '#10, #10, [RfReplaceALL]);
        While ALPosW(#10' ', LText) > 0 do
          LText := ALStringReplaceW(Ltext, #10' ', #10, [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, '&nbsp;', Chr($A0){0x00A0}, [RfReplaceALL, RfIgnoreCase]);
      end
      else
        LText := ALStringReplaceW(Ltext, #13#10, #10, [RfReplaceALL]);

      // In Skia, there is no direct way to set the maximum height for a SkParagraph in advance.
      // We can primarily control the paragraph height by adjusting the 'maxLines' property.
      // Therefore, we must first layout the SkParagraph. If its height exceeds ARect.height,
      // then we need to adjust the 'maxLines' accordingly.
      // https://groups.google.com/g/skia-discuss/c/zS4wAwmCkLE
      While true do begin

        // Clear all accumulators
        LFontFamilies.Clear;
        LFontSizes.Clear;
        LFontWeights.Clear;
        LFontSlants.Clear;
        LFontStretchs.Clear;
        LFontColors.Clear;
        LDecorationKinds.clear;
        LDecorationStyles.clear;
        LDecorationThicknessMultipliers.clear;
        LDecorationColors.clear;
        LBackgroundColors.Clear;
        LLineHeightMultipliers.clear;
        LLetterSpacings.Clear;
        LSpanIDs.Clear;
        LPlaceHolders.Clear;
        LRangeIndexes.Clear;

        // Declare LTextForRange
        var LTextForRange: String := '';

        // Init LParagraphstyle
        var LParagraphstyle := sk4d_paragraphstyle_create;
        Try

          // https://api.flutter.dev/flutter/dart-ui/TextHeightBehavior-class.html
          // Defines how to apply TextStyle.height over and under text.
          // TextHeightBehavior.applyHeightToFirstAscent and TextHeightBehavior.applyHeightToLastDescent
          // represent whether the TextStyle.height modifier will be applied to the corresponding metric.
          // By default both properties are true, and TextStyle.height is applied as normal. When set to
          // false, the font's default ascent will be used.
          // applyHeightToFirstAscent → bool
          // Whether to apply the TextStyle.height modifier to the ascent of the first line in the paragraph.
          // applyHeightToLastDescent → bool
          // Whether to apply the TextStyle.height modifier to the descent of the last line in the paragraph.
          // var LSkTextHeightBehaviors: TSkTextHeightBehaviors := [TSkTextHeightBehavior.DisableFirstAscent, TSkTextHeightBehavior.DisableLastDescent];
          // sk4d_paragraphstyle_set_text_height_behaviors(LParagraphstyle, Byte(LSkTextHeightBehaviors));

          // Direction
          if LOptions.Direction = TALTextDirection.RightToLeft then
            sk4d_paragraphstyle_set_text_direction(LParagraphstyle, sk_textdirection_t.RIGHT_TO_LEFT_SK_TEXTDIRECTION);

          // HTextAlign
          // https://api.flutter.dev/flutter/dart-ui/TextAlign.html
          // Whether and how to align text horizontally.
          // * left → const TextAlign
          //     Align the text on the left edge of the container.
          // * right → const TextAlign
          //     Align the text on the right edge of the container.
          // * center → const TextAlign
          //     Align the text in the center of the container.
          // * justify → const TextAlign
          //     Stretch lines of text that end with a soft line break to fill the width of the container.
          //     Lines that end with hard line breaks are aligned towards the start edge.
          // * start → const TextAlign
          //     Align the text on the leading edge of the container.
          //     For left-to-right text (TextDirection.ltr), this is the left edge.
          //     For right-to-left text (TextDirection.rtl), this is the right edge.
          // * end → const TextAlign
          //     Align the text on the trailing edge of the container.
          //     For left-to-right text (TextDirection.ltr), this is the right edge.
          //     For right-to-left text (TextDirection.rtl), this is the left edge.
          case LOptions.HTextAlign of
            TALTextHorzAlign.Center: sk4d_paragraphstyle_set_text_align(LParagraphstyle, sk_textalign_t.CENTER_SK_TEXTALIGN);
            TALTextHorzAlign.Leading: sk4d_paragraphstyle_set_text_align(LParagraphstyle, sk_textalign_t.START_SK_TEXTALIGN);
            TALTextHorzAlign.Trailing: sk4d_paragraphstyle_set_text_align(LParagraphstyle, sk_textalign_t.TERMINATE_SK_TEXTALIGN);
            TALTextHorzAlign.Justify: sk4d_paragraphstyle_set_text_align(LParagraphstyle, sk_textalign_t.JUSTIFY_SK_TEXTALIGN);
            Else raise Exception.Create('Error C3E64ECA-147F-4EE8-A4CA-EBCB48ACD08C');
          end;

          // https://en.wikipedia.org/wiki/Font_hinting
          // With the advent of high-DPI displays (generally considered to be displays with
          // more than 300 pixels per inch), font hinting has become less relevant, as
          // aliasing effects become un-noticeable to the human eye. As a result
          // Apple's Quartz text renderer, which is targeted for Apple's Retina
          // displays, now ignores font hint information completely.
          // Note: I tried to disable the font hinting, but it seems to change nothing, so
          // I need a practical example to show what exactly font hinting does.
          // In the meantime, I prefer not to touch these settings.
          // sk4d_paragraphstyle_disable_hinting(LParagraphstyle);

          // MaxLines
          // https://api.flutter.dev/flutter/dart-ui/ParagraphStyle/ParagraphStyle.html
          // The maximum number of lines painted. Lines beyond this number are silently dropped.
          // For example, if maxLines is 1, then only one line is rendered. If maxLines is null,
          // but ellipsis is not null, then lines after the first one that overflows the width
          // constraints are dropped. The width constraints are those set in the ParagraphConstraints
          // object passed to the Paragraph.layout method.
          sk4d_paragraphstyle_set_max_lines(LParagraphstyle, LMaxLines);

          // Create LParagraphBuilder
          var LParagraphBuilder := sk4d_paragraphbuilder_create2(
                                     LParagraphstyle, // const paragraph_style: sk_paragraphstyle_t;
                                     TSkDefaultProviders.TypefaceFont.Handle, // font_provider: sk_fontmgr_t;
                                     true); // enable_font_fallback: _bool): sk_paragraphbuilder_t; cdecl;
          try


            ///////////////////////////////////
            // loop on all the html elements //
            ///////////////////////////////////

            var LCurrIndex := 0;
            var P1 := low(Ltext);
            while P1 <= high(Ltext) do begin

              var LCurrText: String;
              var LCurrImgSrc: String;
              var LcurrImgWidth: Single;
              var LcurrImgHeight: Single;


              /////////////////////////
              // Insert the ellipsis //
              /////////////////////////

              if LCurrIndex = LInsertEllipsisAt then begin
                if not LOptions.EllipsisInheritSettings then begin
                  LFontFamilies.Add(LOptions.EllipsisFontFamily);
                  LFontSizes.Add(LOptions.EllipsisFontSize);
                  LFontWeights.Add(LOptions.EllipsisFontWeight);
                  LFontSlants.Add(LOptions.EllipsisFontSlant);
                  LFontStretchs.Add(LOptions.EllipsisFontStretch);
                  LFontColors.Add(LOptions.EllipsisFontColor);
                  LDecorationKinds.Add(LOptions.EllipsisDecorationKinds);
                  LDecorationStyles.Add(LOptions.EllipsisDecorationStyle);
                  LDecorationThicknessMultipliers.Add(LOptions.EllipsisDecorationThicknessMultiplier);
                  LDecorationColors.Add(LOptions.EllipsisDecorationColor);
                  LBackgroundColors.Add(TALphaColors.Null);
                  LLineHeightMultipliers.Add(LOptions.LineHeightMultiplier);
                  LLetterSpacings.Add(LOptions.LetterSpacing);
                end;
                LSpanIDs.Add('ellipsis');
                if (length(LOptions.EllipsisText) > 2) and (ALPosW('… ', LOptions.EllipsisText) = 1) then
                  LCurrText := ALCopyStr(LOptions.EllipsisText, 3, maxint)
                else
                  LCurrText := LOptions.EllipsisText;
                LCurrImgSrc := '';
                LcurrImgWidth := 0;
                LcurrImgHeight := 0;
                LInsertEllipsisAt := Maxint;
                P1 := Maxint;
              end

              /////////////////////////////////////
              // The text contains HTML elements //
              /////////////////////////////////////

              else if LOptions.TextIsHtml then begin

                // Extract LCurrText / LCurrImgSrc
                if Ltext[P1] = '<' then begin

                  // Proper HTML requires that elements are correctly nested and
                  // closed in the order they are opened.

                  //-----
                  LCurrImgSrc := '';
                  LCurrText := '';
                  LcurrImgWidth := 0;
                  LcurrImgHeight := 0;
                  var P2 := ALPosW('>', Ltext, P1+1); // blablabla <font color="#ffffff">blablabla</font> blablabla
                                                      //           ^P1                  ^P2
                  if P2 <= 0 then break;
                  var LTag: String := ALCopyStr(Ltext, P1, P2 - P1 + 1); // <font color="#ffffff">
                  P1 := P2 + 1; // blablabla <font color="#ffffff">blablabla</font> blablabla
                                //                                 ^P1

                  //-----
                  if (ALPosW('<b ', LTag) = 1) or
                     (LTag = '<b>') then begin
                    _getInfosFromTag(ALCopyStr(LTag, 4, length(LTag) - 4), LSpanIDs);
                    LFontWeights.Add(TFontWeight.Bold);
                  end
                  else if LTag = '</b>' then begin
                    if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
                    if LFontWeights.count > 0 then LFontWeights.Delete(LFontWeights.Count - 1);
                  end

                  //-----
                  else if (ALPosW('<i ', LTag) = 1) or
                          (LTag = '<i>') then begin
                    _getInfosFromTag(ALCopyStr(LTag, 4, length(LTag) - 4), LSpanIDs);
                    LFontSlants.Add(TFontSlant.Italic);
                  end
                  else if LTag = '</i>' then begin
                    if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
                    if LFontSlants.count > 0 then LFontSlants.Delete(LFontSlants.Count - 1);
                  end

                  //-----
                  else if (ALPosW('<img ', LTag) = 1) or
                          (LTag = '<img/>') then begin // <img src="xxx">
                    _getInfosFromTag(ALCopyStr(LTag, 6, length(LTag) - 6), LCurrImgSrc, LcurrImgWidth, LcurrImgHeight);
                  end

                  //-----
                  else if (ALPosW('<font ', LTag) = 1) or
                          (LTag = '<font>')  then begin   // <font color="#ffffff">
                    _getInfosFromTag(ALCopyStr(LTag, 7, length(LTag) - 7), LSpanIDs, LFontFamilies, LFontSizes, LFontColors);
                  end
                  else if LTag = '</font>' then begin
                    if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
                    if LFontFamilies.count > 0 then LFontFamilies.Delete(LFontFamilies.Count - 1);
                    if LFontSizes.count > 0 then LFontSizes.Delete(LFontSizes.Count - 1);
                    if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
                  end

                  //-----
                  else if (ALPosW('<span ', LTag) = 1) or
                          (LTag = '<span>') then begin // <span id="xxx">
                    _getInfosFromTag(ALCopyStr(LTag, 7, length(LTag) - 7), LSpanIDs, LFontFamilies, LFontSizes, LFontWeights, LFontSlants, LFontStretchs, LFontColors, LDecorationKinds, LDecorationStyles, LDecorationThicknessMultipliers, LDecorationColors, LBackgroundColors, LLineHeightMultipliers, LLetterSpacings);
                  end
                  else if LTag = '</span>' then begin
                    if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
                    if LFontFamilies.count > 0 then LFontFamilies.Delete(LFontFamilies.Count - 1);
                    if LFontSizes.count > 0 then LFontSizes.Delete(LFontSizes.Count - 1);
                    if LFontWeights.count > 0 then LFontWeights.Delete(LFontWeights.Count - 1);
                    if LFontSlants.count > 0 then LFontSlants.Delete(LFontSlants.Count - 1);
                    if LFontStretchs.count > 0 then LFontStretchs.Delete(LFontStretchs.Count - 1);
                    if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
                    if LDecorationKinds.count > 0 then LDecorationKinds.Delete(LDecorationKinds.Count - 1);
                    if LDecorationStyles.count > 0 then LDecorationStyles.Delete(LDecorationStyles.Count - 1);
                    if LDecorationThicknessMultipliers.count > 0 then LDecorationThicknessMultipliers.Delete(LDecorationThicknessMultipliers.Count - 1);
                    if LDecorationColors.count > 0 then LDecorationColors.Delete(LDecorationColors.Count - 1);
                    if LBackgroundColors.count > 0 then LBackgroundColors.Delete(LBackgroundColors.Count - 1);
                    if LLineHeightMultipliers.count > 0 then LLineHeightMultipliers.Delete(LLineHeightMultipliers.Count - 1);
                    if LLetterSpacings.count > 0 then LLetterSpacings.Delete(LLetterSpacings.Count - 1);
                  end;

                end
                else begin

                  LCurrImgSrc := '';
                  LcurrImgWidth := 0;
                  LcurrImgHeight := 0;
                  var P2 := ALPosW('<', Ltext, P1);  // blablabla <font color="#ffffff">blablabla</font> blablabla
                                                     //                                 ^P1      ^P2
                  if P2 <= 0 then P2 := Maxint;
                  LCurrText := ALCopyStr(Ltext, P1, P2 - P1);  // blablabla
                  LCurrText := ALStringReplaceW(LCurrText, '&gt;', '>', [rfReplaceALL]);
                  LCurrText := ALStringReplaceW(LCurrText, '&lt;', '<', [rfReplaceALL]);
                  P1 := P2; // blablabla <font color="#ffffff">blablabla</font> blablabla
                            //                                          ^P1
                end;

              end

              /////////////////////////////////////////////
              // The text does NOT contain HTML elements //
              /////////////////////////////////////////////

              else begin
                LCurrText := Ltext;
                LCurrImgSrc := '';
                LcurrImgWidth := 0;
                LcurrImgHeight := 0;
                P1 := Maxint;
              end;


              //////////////////////////////////////////////////
              // Add the current text to the ParagraphBuilder //
              //////////////////////////////////////////////////

              if (LCurrText <> '') or (LCurrImgSrc <> '') then begin

                // Init LFontFamily
                var LFontFamily: String;
                if LFontFamilies.Count > 0 then LFontFamily := LFontFamilies[LFontFamilies.Count - 1]
                else LFontFamily := LOptions.FontFamily;

                // Init LFontSize
                var LFontSize: Single;
                if LFontSizes.Count > 0 then LFontSize := LFontSizes[LFontSizes.Count - 1]
                else LFontSize := LOptions.FontSize;

                // Init LFontWeight
                var LFontWeight: TFontWeight;
                if LFontWeights.Count > 0 then LFontWeight := LFontWeights[LFontWeights.Count - 1]
                else LFontWeight := LOptions.FontWeight;

                // Init LFontSlant
                var LFontSlant: TFontSlant;
                if LFontSlants.Count > 0 then LFontSlant := LFontSlants[LFontSlants.Count - 1]
                else LFontSlant := LOptions.FontSlant;

                // Init LFontStretch
                var LFontStretch: TFontStretch;
                if LFontStretchs.Count > 0 then LFontStretch := LFontStretchs[LFontStretchs.Count - 1]
                else LFontStretch := LOptions.FontStretch;

                // Init LFontColor
                var LFontColor: TalphaColor;
                if LFontColors.Count > 0 then LFontColor := LFontColors[LFontColors.Count - 1]
                else LFontColor := LOptions.FontColor;

                // Init LDecorationKind
                var LDecorationKind: TALTextDecorationKinds;
                if LDecorationKinds.Count > 0 then LDecorationKind := LDecorationKinds[LDecorationKinds.Count - 1]
                else LDecorationKind := LOptions.DecorationKinds;

                // Init LDecorationStyle
                var LDecorationStyle: TALTextDecorationStyle;
                if LDecorationStyles.Count > 0 then LDecorationStyle := LDecorationStyles[LDecorationStyles.Count - 1]
                else LDecorationStyle := LOptions.DecorationStyle;

                // Init LDecorationThicknessMultiplier
                var LDecorationThicknessMultiplier: Single;
                if LDecorationThicknessMultipliers.Count > 0 then LDecorationThicknessMultiplier := LDecorationThicknessMultipliers[LDecorationThicknessMultipliers.Count - 1]
                else LDecorationThicknessMultiplier := LOptions.DecorationThicknessMultiplier;

                // Init LDecorationColor
                var LDecorationColor: TalphaColor;
                if LDecorationColors.Count > 0 then LDecorationColor := LDecorationColors[LDecorationColors.Count - 1]
                else LDecorationColor := LOptions.DecorationColor;

                // Init LBackgroundColor
                var LBackgroundColor: TalphaColor;
                if LBackgroundColors.Count > 0 then LBackgroundColor := LBackgroundColors[LBackgroundColors.Count - 1]
                else LBackgroundColor := TalphaColors.Null;

                // Init LLineHeightMultiplier
                var LLineHeightMultiplier: Single;
                if LLineHeightMultipliers.Count > 0 then LLineHeightMultiplier := LLineHeightMultipliers[LLineHeightMultipliers.Count - 1]
                else LLineHeightMultiplier := LOptions.LineHeightMultiplier;

                // Init LLetterSpacing
                var LLetterSpacing: Single;
                if LLetterSpacings.Count > 0 then LLetterSpacing := LLetterSpacings[LLetterSpacings.Count - 1]
                else LLetterSpacing := LOptions.LetterSpacing;

                // Init LSpanID
                var LSpanID: String;
                if LSpanIDs.Count > 0 then LSpanID := LSpanIDs[LSpanIDs.Count - 1]
                else LSpanID := '';

                // Init LLength
                var LLength: Integer;
                if LCurrImgSrc <> '' then LLength := 1
                else LLength := LCurrText.Length;

                // Init LPrevCurrIndex / LCurrIndex
                var LPrevCurrIndex := LCurrIndex;
                LCurrIndex := LCurrIndex + LLength;

                // Handle LInsertEllipsisAt
                if LCurrIndex > LInsertEllipsisAt then begin
                  // It is not possible to have a non-empty LCurrImgSrc here. When LCurrImgSrc is not empty (''),
                  // LLength is set to 1. Therefore, L(Prev)CurrIndex + 1 cannot exceed LInsertEllipsisAt because to reach the condition
                  // where L(Prev)CurrIndex = LInsertEllipsisAt (the only way to have  L(Prev)CurrIndex + 1 > LInsertEllipsisAt), we
                  // would have already entered the "Insert the ellipsis" block (i.e., if LCurrIndex = LInsertEllipsisAt then ...). This
                  // block would insert text and reset LCurrImgSrc to an empty string ('').
                  if LCurrImgSrc <> '' then
                    raise Exception.Create('Error 90AD8D31-4CEA-4AE3-8D84-51B5EB6E171B');
                  LCurrText := ALCopyStr(LCurrText, 1, LCurrText.Length - (LCurrIndex - LInsertEllipsisAt));
                  if (length(LOptions.EllipsisText) > 2) and (ALPosW('… ', LOptions.EllipsisText) = 1) then begin
                    LCurrText := LCurrText + '… ';
                    LInsertEllipsisAt := LInsertEllipsisAt + 2;
                  end;
                  LLength := LCurrText.Length;
                  LCurrIndex := LInsertEllipsisAt;
                  P1 := high(Ltext);
                end;

                // Update LRangeIndexes
                if LSpanID <> '' then begin
                  Var LRangeIndex: TRangeIndex;
                  LRangeIndex.SpanID := LSpanID;
                  LRangeIndex.start := LPrevCurrIndex;
                  LRangeIndex.&end := LPrevCurrIndex + LLength;
                  LRangeIndexes.Add(LRangeIndex);
                end;

                //break the text
                var LTextStyle := sk4d_textstyle_create;
                try

                  // Set the font families
                  // https://api.flutter.dev/flutter/painting/TextStyle/fontFamily.html
                  // The name of the font to use when painting the text (e.g., Roboto).
                  var LUTF16FontFamilies: TArray<String>;
                  var LUTF8FontFamilies: TArray<UTF8String>;
                  var LMarshaledFontFamilies: TArray<MarshaledAString>;
                  LUTF16FontFamilies := LFontFamily.Split([',', #13, #10], TStringSplitOptions.ExcludeEmpty);
                  if Length(LUTF16FontFamilies) > 0 then begin
                    SetLength(LUTF8FontFamilies, Length(LUTF16FontFamilies));
                    SetLength(LMarshaledFontFamilies, Length(LUTF16FontFamilies));
                    var I: Integer := 0;
                    for var J := low(LUTF16FontFamilies) to high(LUTF16FontFamilies) do begin
                      var LUTF16FontFamily := ALTrim(LUTF16FontFamilies[J]);
                      if LUTF16FontFamily <> '' then begin
                        LUTF8FontFamilies[I] := UTF8String(LUTF16FontFamily);
                        LMarshaledFontFamilies[I] := MarshaledAString(LUTF8FontFamilies[I]);
                        inc(I);
                      end;
                    end;
                    if I > 0 then
                      sk4d_textstyle_set_font_families(LTextStyle, @LMarshaledFontFamilies[0], I);
                  end;

                  // Set the font size
                  // https://api.flutter.dev/flutter/painting/TextStyle/fontSize.html
                  // The size of fonts (in logical pixels) to use when painting the text.
                  // The getParagraphStyle method defaults to 14 logical pixels if fontSize is set to null.
                  sk4d_textstyle_set_font_size(LTextStyle, LFontSize);

                  // Set the font Weight, Slant and Stretch
                  var LSkfontstyle := ALGetSkFontStyle(LFontWeight, LFontSlant, LFontStretch);
                  sk4d_textstyle_set_font_style(LTextStyle, @LSkfontstyle);

                  // Set the font color
                  sk4d_textstyle_set_color(LTextStyle, LFontColor);

                  // Set decoration kinds
                  // https://api.flutter.dev/flutter/painting/TextStyle/decoration.html
                  // The decorations to paint near the text (e.g., an underline).
                  {$IFNDEF ALCompilerVersionSupported122}
                    {$MESSAGE WARN 'Check if declaration of System.Skia.TSkTextDecoration didn''t changed'}
                  {$ENDIF}
                  sk4d_textstyle_set_decorations(LTextStyle, Byte(LDecorationKind));

                  // Set decoration style
                  // https://api.flutter.dev/flutter/painting/TextStyle/decorationStyle.html
                  // The style in which to paint the text decorations (e.g., dashed).
                  if LDecorationKind <> [] then begin
                    var Ltextdecorationstyle: sk_textdecorationstyle_t;
                    case LDecorationStyle of
                      TALTextDecorationStyle.Solid: Ltextdecorationstyle := sk_textdecorationstyle_t.SOLID_SK_TEXTDECORATIONSTYLE;
                      TALTextDecorationStyle.Double: Ltextdecorationstyle := sk_textdecorationstyle_t.DOUBLE_SK_TEXTDECORATIONSTYLE;
                      TALTextDecorationStyle.Dotted: Ltextdecorationstyle := sk_textdecorationstyle_t.DOTTED_SK_TEXTDECORATIONSTYLE;
                      TALTextDecorationStyle.Dashed: Ltextdecorationstyle := sk_textdecorationstyle_t.DASHED_SK_TEXTDECORATIONSTYLE;
                      TALTextDecorationStyle.Wavy: Ltextdecorationstyle := sk_textdecorationstyle_t.WAVY_SK_TEXTDECORATIONSTYLE;
                      else raise Exception.Create('Error 4ECD41C9-4D7B-4BDE-AA1B-BA27FB57C106');
                    end;
                    sk4d_textstyle_set_decoration_style(LTextStyle, Ltextdecorationstyle);
                  end;

                  // Set decoration thickness
                  // https://api.flutter.dev/flutter/painting/TextStyle/decorationThickness.html
                  // The thickness of the decoration stroke as a multiplier of the thickness defined by the font.
                  // The font provides a base stroke width for decorations which scales off of the fontSize.
                  // This property may be used to achieve a thinner or thicker decoration stroke, without
                  // changing the fontSize. For example, a decorationThickness of 2.0 will draw a decoration
                  // twice as thick as the font defined decoration thickness.
                  if (LDecorationKind <> []) and
                     (not samevalue(LDecorationThicknessMultiplier, 0, Tepsilon.Scale)) then
                    sk4d_textstyle_set_decoration_thickness(LTextStyle, LDecorationThicknessMultiplier);

                  // Set decoration color
                  // https://api.flutter.dev/flutter/painting/TextStyle/decorationColor.html
                  // The color in which to paint the text decorations.
                  if (LDecorationKind <> []) and
                     (LDecorationColor <> Talphacolors.Null) then
                    sk4d_textstyle_set_decoration_color(LTextStyle, LDecorationColor);

                  // set background color
                  // https://api.flutter.dev/flutter/painting/TextStyle/backgroundColor.html
                  // The color to use as the background for the text.
                  if LBackgroundColor <> TalphaColors.Null then begin
                    sk4d_paint_set_color(LPaint, LBackgroundColor);
                    sk4d_textstyle_set_background_color(LTextStyle, Lpaint);
                  end;

                  // set letter spacing
                  // https://api.flutter.dev/flutter/painting/TextStyle/letterSpacing.html
                  // The amount of space (in logical pixels) to add between each letter. A negative value
                  // can be used to bring the letters closer.
                  if not SameValue(LLetterSpacing, 0, TEpsilon.FontSize) then
                    sk4d_textstyle_set_letter_spacing(LTextStyle, LLetterSpacing);

                  // set half_leading
                  // https://api.flutter.dev/flutter/painting/TextStyle/leadingDistribution.html
                  // How the vertical space added by the height multiplier should be distributed over
                  // and under the text. When a non-null height is specified, after accommodating the
                  // glyphs of the text, the remaining vertical space from the allotted line height will
                  // be distributed over and under the text, according to the leadingDistribution property.
                  // When height is null, leadingDistribution does not affect the text layout.
                  // Defaults to null, which defers to the paragraph's
                  // ParagraphStyle.textHeightBehavior's leadingDistribution (IE: TextLeadingDistribution.proportional)
                  sk4d_textstyle_set_half_leading(LTextStyle, true);

                  // set height multiplier
                  // https://api.flutter.dev/flutter/painting/TextStyle/height.html
                  // By default, text will layout with line height as defined by the font. Font-metrics defined line height may be
                  // taller or shorter than the font size. The height property allows manual adjustment of the height of the line
                  // as a multiple of fontSize. For most fonts, setting height to 1.0 is not the same as omitting or setting
                  // height to null.
                  if not SameValue(LLineHeightMultiplier, 0, TEpsilon.Scale) then
                    sk4d_textstyle_set_height_multiplier(LTextStyle, LLineHeightMultiplier);

                  // Push the style
                  // https://api.flutter.dev/flutter/dart-ui/ParagraphBuilder/pushStyle.html
                  // Applies the given style to the added text until pop is called.
                  sk4d_paragraphbuilder_push_style(LParagraphBuilder, LTextStyle);

                  // Add the text or PlaceHolder
                  if LCurrImgSrc <> '' then begin
                    {$IFNDEF ALCompilerVersionSupported122}
                      {$MESSAGE WARN 'Check if declaration of System.Skia.API.sk_placeholderstyle_t didn''t changed'}
                    {$ENDIF}
                    var LPlaceholderStyle: sk_placeholderstyle_t;
                    if CompareValue(LCurrImgWidth, 0, TEpsilon.FontSize) <= 0 then LCurrImgWidth := LOptions.FontSize;
                    if CompareValue(LCurrImgHeight, 0, TEpsilon.FontSize) <= 0 then LCurrImgHeight := LOptions.FontSize;
                    LPlaceholderStyle.width := LcurrImgWidth;
                    LPlaceholderStyle.height := LcurrImgHeight;
                    LPlaceholderStyle.alignment := sk_placeholderalignment_t.MIDDLE_SK_PLACEHOLDERALIGNMENT;
                    LPlaceholderStyle.baseline := sk_textbaseline_t.ALPHABETIC_SK_TEXTBASELINE;
                    LPlaceholderStyle.baseline_offset := 0.0;
                    sk4d_paragraphbuilder_add_placeholder(LParagraphBuilder, @LPlaceholderStyle);
                    LTextForRange := LTextForRange + '_';
                    LPlaceHolders.Add(LCurrImgSrc);
                  end
                  else begin
                    sk4d_paragraphbuilder_add_text(LParagraphBuilder, MarshaledAString(UTF8String(LCurrText)));
                    LTextForRange := LTextForRange + LCurrText;
                  end;

                  // Pop the style
                  // https://api.flutter.dev/flutter/dart-ui/ParagraphBuilder/pop.html
                  // Ends the effect of the most recent call to pushStyle.
                  sk4d_paragraphbuilder_pop(LParagraphBuilder);

                finally
                  sk4d_textstyle_destroy(LTextStyle);
                end;

              end;

            end;


            /////////////////////////
            // Paint the paragraph //
            /////////////////////////

            // https://api.flutter.dev/flutter/dart-ui/ParagraphBuilder/build.html
            // Applies the given paragraph style and returns a Paragraph containing the added text and associated styling.
            // After calling this function, the paragraph builder object is invalid and cannot be used further.
            var LParagraph := sk4d_paragraphbuilder_build(LParagraphBuilder);
            try

              // Layout the paragraph
              // https://api.flutter.dev/flutter/dart-ui/Paragraph/layout.html
              // Computes the size and position of each glyph in the paragraph.
              var LMaxWidth := ARect.Width - LOptions.Padding.Left - LOptions.Padding.Right;
              sk4d_paragraph_layout(Lparagraph, LMaxWidth);

              // Get the Metrics of each lines
              var LMetrics: Tarray<sk_metrics_t>;
              SetLength(LMetrics, sk4d_paragraph_get_line_metrics(Lparagraph, nil));
              if length(LMetrics) = 0 then begin
                ARect.Width := 0;
                ARect.Height := 0;
                exit;
              end;
              sk4d_paragraph_get_line_metrics(Lparagraph, @LMetrics[0]);

              // Update AAllTextDrawn
              // https://api.flutter.dev/flutter/dart-ui/Paragraph/didExceedMaxLines.html
              // True if there is more vertical content, but the text was truncated, either because
              /// we reached maxLines lines of text or because the maxLines was null, ellipsis was
              // not null, and one of the lines exceeded the width constraint.
              var LDidExceedMaxLines := sk4d_paragraph_did_exceed_max_lines(Lparagraph);
              if AAllTextDrawn then
                AAllTextDrawn := not LDidExceedMaxLines;

              // Update ATextBroken
              ATextBroken := ATextBroken or (length(LMetrics) > 1) or LDidExceedMaxLines;
              if LOptions.FailIfTextBroken and ATextBroken then begin
                ARect.Width := 0;
                ARect.Height := 0;
                exit;
              end;

              // Update ARect
              var LReLayout: Boolean := False;
              var LMaxHeight := ARect.Height - LOptions.Padding.Top - LOptions.Padding.Bottom;
              Var LParagraphRect := TrectF.Empty;
              for var I := Low(LMetrics) to High(LMetrics) do begin
                LParagraphRect.Width := Max(LParagraphRect.Width, LMetrics[I].width);
                LParagraphRect.Height := LParagraphRect.Height + LMetrics[I].height;
                // We have too many lines to fit within the maxHeight constraint.
                // Note: There is a bug in Skia related to setting a very large font size for the
                // ellipsis. This issue occurs when the last line is significantly short,
                // containing only 2 characters (e.g., when rect.width = 35). Under these circumstances,
                // the last line may exceed the maximum width constraint specified in sk4d_paragraph_layout.
                if (CompareValue(LParagraphRect.Height, LMaxHeight, TEpsilon.Position) > 0) or
                   (CompareValue(LParagraphRect.Width, LMaxWidth, TEpsilon.Position) > 0) then begin
                  if I = 0 then begin
                    ARect.Width := 0;
                    ARect.Height := 0;
                    exit;
                  end;
                  // Although it's impossible to have LMaxLines = I, I prefer to crash rather
                  // than enter an infinite loop.
                  if LMaxLines <= I then
                    Raise exception.Create('Error 47772843-A4B1-4133-9FD2-3DA7D4DBB2ED');
                  LMaxLines := I {0 based};
                  AAllTextDrawn := False;
                  LInsertEllipsisAt := LMetrics[i-1].end_excluding_whitespaces;
                  LPrevInsertEllipsisAt := LInsertEllipsisAt;
                  LReLayout := True;
                  Break;
                end
                // We have exceeded the maxLines, so add the EllipsisText.
                else if ((I = High(LMetrics)) and (LDidExceedMaxLines) and (LOptions.EllipsisText <> '')) then begin
                  AAllTextDrawn := False;
                  // Note: With Skia, only TALTextTrimming.Word is supported.
                  // Normally, LMetrics[i].end_excluding_whitespaces, as determined by sk4d_paragraph_layout,
                  // indicates the correct position for inserting the ellipsis. However, in certain scenarios,
                  // such as when processing "The house of WOLFESCHLEGELSTEIN", the engine suggests a break
                  // after "of" at position 12. In the next iteration, when attempting to break "The house of… more[+]",
                  // the engine now advises a break after the ellipsis at position 13 (where 13 > 12). This is why
                  // the management of LPrevInsertEllipsisAt is crucial.
                  LInsertEllipsisAt := LMetrics[i].end_excluding_whitespaces;
                  if LInsertEllipsisAt >= LPrevInsertEllipsisAt then
                    LInsertEllipsisAt := _findLastBreakPosition(
                                           LTextForRange, // const AText: String;
                                           LPrevInsertEllipsisAt-1, // Const ANumberOfChars: Integer;
                                           false, // const AHardBreak: Boolean = False;
                                           true); // const ASkipEndOfTextPunctuation: Boolean = False)
                  // _findLastBreakPosition return -1 when LPrevInsertEllipsisAt = 0
                  if LInsertEllipsisAt < 0 then begin
                    ARect.Width := 0;
                    ARect.Height := 0;
                    exit;
                  end;
                  LPrevInsertEllipsisAt := LInsertEllipsisAt;
                  LReLayout := True;
                  Break;
                end;
              end;
              if LReLayout then continue;
              //
              // We must round up (ceil) the value of LMetrics[I].width. For example, if we return a width
              // of 21.82, and later attempt to layout the same paragraph with a constraint
              // width of 21.82, it will fail. This issue seems to arise because Skia requires
              // at least a full pixel's space to determine if it can break at 21.82. Otherwise, it will
              // break the text at the previous word, thinking there isn't enough space
              // to accommodate the last 0.82. Internally it's round 21.82 down to 21 like you can
              // see in ParagraphImpl.cpp :
              //
              //   void ParagraphImpl::layout(SkScalar rawWidth) {
              //       TODO: This rounding is done to match Flutter tests. Must be removed...
              //       auto floorWidth = SkScalarFloorToScalar(rawWidth);
              //
              var LOriginalRectWidth: Single := ARect.Width;
              if LOptions.Autosize or (LOptions.AutosizeX and LOptions.AutosizeY) then begin
                ARect.Width := Min(ARect.Width, Ceil(LParagraphRect.Width) + LOptions.Padding.Left + LOptions.Padding.Right);
                ARect.Height := Min(ARect.Height, LParagraphRect.Height + LOptions.Padding.Top + LOptions.Padding.Bottom);
              end
              else if LOptions.AutosizeX then ARect.Width := Min(ARect.Width, Ceil(LParagraphRect.Width) + LOptions.Padding.Left + LOptions.Padding.Right)
              else if LOptions.AutosizeY then ARect.Height := Min(ARect.Height, LParagraphRect.Height + LOptions.Padding.Top + LOptions.Padding.Bottom);

              // init LParagraphRect.topleft
              case LOptions.VTextAlign of
                TALTextVertAlign.Center: LParagraphRect.Offset(LOptions.Padding.Left, LOptions.Padding.Top + ((ARect.Height - LParagraphRect.Height - LOptions.Padding.Top - LOptions.Padding.Bottom) / 2));
                TALTextVertAlign.Leading: LParagraphRect.Offset(LOptions.Padding.Left, LOptions.Padding.Top);
                TALTextVertAlign.Trailing: LParagraphRect.Offset(LOptions.Padding.Left, ARect.Height - LOptions.Padding.Bottom - LParagraphRect.Height);
                Else raise Exception.Create('Error 6DBA0B08-4F9E-4D89-9998-6B054D527F1F');
              end;

              // init LSk4dParagraphOffsetX
              Var LSk4dParagraphOffsetX: Single;
              If (LOptions.hTextAlign = TALTextHorzAlign.Center) then LSk4dParagraphOffsetX := (LOriginalRectWidth - ARect.Width) / 2
              else if ((LOptions.Direction = TALTextDirection.RightToLeft) and (LOptions.hTextAlign = TALTextHorzAlign.Leading)) or
                      ((LOptions.Direction <> TALTextDirection.RightToLeft) and (LOptions.hTextAlign = TALTextHorzAlign.Trailing)) then LSk4dParagraphOffsetX := (LOriginalRectWidth - ARect.Width)
              else LSk4dParagraphOffsetX := 0;

              // Update AElements
              var Ltextboxes: TArray<sk_textbox_t>;
              for var I := 0 to LRangeIndexes.Count - 1 do begin
                SetLength(Ltextboxes, sk4d_paragraph_get_rects_for_range(LParagraph, LRangeIndexes[I].start{start}, LRangeIndexes[I].&end{&end}, sk_rectheightstyle_t.MAX_SK_RECTHEIGHTSTYLE{rect_height_style}, sk_rectwidthstyle_t.TIGHT_SK_RECTWIDTHSTYLE{rect_width_style}, nil{result}));
                if length(Ltextboxes) > 0 then begin
                  sk4d_paragraph_get_rects_for_range(LParagraph, LRangeIndexes[I].start{start}, LRangeIndexes[I].&end{&end}, sk_rectheightstyle_t.MAX_SK_RECTHEIGHTSTYLE{rect_height_style}, sk_rectwidthstyle_t.TIGHT_SK_RECTWIDTHSTYLE{rect_width_style}, @Ltextboxes[0]{result});
                  var J := high(AElements) + 1;
                  setlength(AElements, length(AElements) + length(Ltextboxes));
                  For var K := low(Ltextboxes) to high(Ltextboxes) do begin
                    var Ltextbox := Ltextboxes[K];
                    AElements[J+K].Id := LRangeIndexes[I].SpanID;
                    AElements[J+K].rect := TRectF.Create(
                                             (LParagraphRect.Left + Ltextbox.rect.Left - LSk4dParagraphOffsetX) / LScale,
                                             (LParagraphRect.Top + Ltextbox.rect.Top) / LScale,
                                             (LParagraphRect.Left + Ltextbox.rect.Right - LSk4dParagraphOffsetX) / LScale,
                                             (LParagraphRect.Top + Ltextbox.rect.Bottom) / LScale);
                  end;
                end;
              end;

              // Calculate the SurfaceRect
              var LSurfaceRect := ALGetShapeSurfaceRect(
                                    ARect, // const ARect: TRectF;
                                    LOptions.FillColor, // const AFillColor: TAlphaColor;
                                    LOptions.FillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                                    LOptions.FillResourceName, // const AFillResourceName: String;
                                    LOptions.FillBackgroundMargins, // Const AFillBackgroundMarginsRect: TRectF;
                                    LOptions.FillImageMargins, // Const AFillImageMarginsRect: TRectF;
                                    LOptions.StateLayerOpacity, // const AStateLayerOpacity: Single;
                                    LOptions.StateLayerColor, // const AStateLayerColor: TAlphaColor;
                                    false, // const AStateLayerUseContentColor: Boolean;
                                    LOptions.StateLayerMargins, // Const AStateLayerMarginsRect: TRectF;
                                    LOptions.ShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
                                    LOptions.ShadowBlur, // const AShadowBlur: Single;
                                    LOptions.ShadowOffsetX, // const AShadowOffsetX: Single;
                                    LOptions.ShadowOffsetY); // const AShadowOffsetY: Single;
              if ALIsCanvasNull(ACanvas) then
                ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.top);

              // Adjust the rect
              if Assigned(LOptions.OnAdjustRect) then begin
                var LSurfaceSize := LSurfaceRect.Size;
                LOptions.OnAdjustRect(ACanvas, LOptions, ARect, LSurfaceSize);
                LSurfaceRect.Size := LSurfaceSize;
              end;

              // Offset the ParagraphRect and AElements according to the Arect coordinates
              LParagraphRect.Offset(ARect.Left, ARect.top);
              for var I := low(AElements) to high(AElements) do
                AElements[I].rect.Offset(ARect.Left, ARect.top);

              // Though it's an unlikely scenario, this ensures avoidance of a crash in
              // the subsequent ALCreateSurface call.
              If (ALCeil(ARect.Width, TEpsilon.Position) = 0) or
                 (ALCeil(ARect.Height, TEpsilon.Position) = 0) then begin
                ARect.Width := 0;
                ARect.Height := 0;
                exit;
              end;

              // 4096 x 4096 pixels is generally considered a safe maximum size
              // for surfaces across many systems
              If (LSurfaceRect.Width > 4096) or
                 (LSurfaceRect.Height > 4096) then begin
                ARect.Width := 0;
                ARect.Height := 0;
                exit;
              end;

              // Exit if AOnlyMeasure
              if AOnlyMeasure then exit;

              // Create the drawing surface
              if ALIsCanvasNull(ACanvas) then begin
                if not ALIsSurfaceNull(ASurface) then
                  raise Exception.Create('ASurface must also be null when ACanvas is null');
                // Use Ceil instead of Round because if the height is, for example, 75.4, it's better to have
                // 76 px in height than 75 px. If there is a border, then it will look better in such cases.
                ALCreateSurface(
                  ASurface, // out ASurface: TALSurface;
                  ACanvas, // out ACanvas: TALCanvas;
                  ALCeil(LSurfaceRect.Width, TEpsilon.Position), // const w: integer;
                  ALCeil(LSurfaceRect.Height, TEpsilon.Position));// const h: integer)
              end;
              if ALCanvasBeginScene(ACanvas) then
              try

                // Create the alpha layer
                if compareValue(LOptions.Opacity, 1, Tepsilon.Scale) < 0 then
                  sk4d_canvas_save_layer_alpha(ACanvas, @LSurfaceRect, round(255 * LOptions.Opacity));
                try

                  // Handle custom event
                  if Assigned(LOptions.OnBeforeDrawBackground) then
                    LOptions.OnBeforeDrawBackground(ACanvas, LOptions, ARect);

                  // Draw the background
                  if (LOptions.FillColor <> TalphaColors.Null) or
                     (length(LOptions.FillGradientColors) > 0) or
                     (LOptions.FillResourceName <> '') or
                     (LOptions.StateLayerColor <> TalphaColors.Null) or
                     (LOptions.StrokeColor <> TalphaColors.Null) or
                     (LOptions.ShadowColor <> TalphaColors.Null) then begin
                    ALDrawRectangle(
                      ACanvas, // const ACanvas: TALCanvas;
                      LOptions.Scale, // const AScale: Single;
                      LOptions.AlignToPixel, // const AAlignToPixel: Boolean;
                      ARect, // const ARect: TrectF;
                      1, //const AOpacity: Single;
                      LOptions.FillColor, // const AFillColor: TAlphaColor;
                      LOptions.FillGradientStyle, // const AFillGradientStyle: TGradientStyle;
                      LOptions.FillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                      LOptions.FillGradientOffsets, // const AFillGradientOffsets: TArray<Single>;
                      LOptions.FillGradientAngle, // const AFillGradientAngle: Single;
                      LOptions.FillResourceName, // const AFillResourceName: String;
                      LOptions.FillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
                      LOptions.FillBackgroundMargins, // Const AFillBackgroundMarginsRect: TRectF;
                      LOptions.FillImageMargins, // Const AFillImageMarginsRect: TRectF;
                      LOptions.StateLayerOpacity, // const AStateLayerOpacity: Single;
                      LOptions.StateLayerColor, // const AStateLayerColor: TAlphaColor;
                      LOptions.StateLayerMargins, // Const AStateLayerMarginsRect: TRectF;
                      LOptions.StateLayerXRadius, // const AStateLayerXRadius: Single;
                      LOptions.StateLayerYRadius, // const AStateLayerYRadius: Single;
                      True, // const ADrawStateLayerOnTop: Boolean;
                      LOptions.StrokeColor, // const AStrokeColor: TalphaColor;
                      LOptions.StrokeThickness, // const AStrokeThickness: Single;
                      LOptions.ShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
                      LOptions.ShadowBlur, // const AShadowBlur: Single;
                      LOptions.ShadowOffsetX, // const AShadowOffsetX: Single;
                      LOptions.ShadowOffsetY, // const AShadowOffsetY: Single;
                      LOptions.Sides, // const Sides: TSides;
                      LOptions.Corners, // const Corners: TCorners;
                      LOptions.XRadius, // const XRadius: Single = 0;
                      LOptions.YRadius); // const YRadius: Single = 0);
                  end;

                  // Handle custom event
                  if Assigned(LOptions.OnBeforeDrawParagraph) then
                    LOptions.OnBeforeDrawParagraph(ACanvas, LOptions, ARect);

                  // Paint the paragraph
                  sk4d_paragraph_paint(
                    LParagraph, // self: sk_paragraph_t;
                    ACanvas, // canvas: sk_canvas_t;
                    LParagraphRect.left - LSk4dParagraphOffsetX, // x: float;
                    LParagraphRect.top); // y: float

                  // retrieve the rect of all placeholders
                  SetLength(LTextBoxes, sk4d_paragraph_get_rects_for_placeholders(LParagraph, nil));
                  if length(LTextBoxes) > 0 then
                    sk4d_paragraph_get_rects_for_placeholders(LParagraph, @LTextBoxes[0]);

                  // Paint all images
                  if Length(LTextBoxes) > LPlaceHolders.Count then
                    raise Exception.Create('Error 155FE121-9CA2-46F8-A51C-0CB72EADC7EC');
                  For var i := low(LTextBoxes) to high(LTextBoxes) do begin
                    Var LImgSrc := LPlaceHolders[i];
                    If LImgSrc <> '' then begin
                      Var LDstRect := TRectF.Create(
                                        LTextBoxes[i].rect.left - LSk4dParagraphOffsetX,
                                        LTextBoxes[i].rect.Top,
                                        LTextBoxes[i].rect.Right - LSk4dParagraphOffsetX,
                                        LTextBoxes[i].rect.Bottom);
                      LDstRect.Offset(LParagraphRect.TopLeft);
                      var LSrcRect := TRectF.Create(0,0,LDstRect.Width, LDstRect.Height);
                      {$IFDEF ALDPK}
                      var LImg: sk_image_t;
                      var LFileName := ALGetResourceFilename(LImgSrc);
                      if LFileName <> '' then begin
                        try
                          LImg := ALLoadFromFileAndStretchToSkImage(LFileName, LDstRect.Width, LDstRect.Height)
                        except
                          LImg := 0;
                        end
                      end
                      else
                        LImg := 0;
                      {$ELSE}
                      var LImg := ALLoadFromResourceAndStretchToSkImage(LImgSrc, LDstRect.Width, LDstRect.Height);
                      {$ENDIF}
                      If LImg <> 0 then begin
                        try
                          var LSamplingoptions := ALGetNearestSkSamplingoptions;
                          sk4d_canvas_draw_image_rect(
                            ACanvas, // self: sk_canvas_t;
                            LImg, // const image: sk_image_t;
                            @LSrcRect, // const src: psk_rect_t;
                            @LDstRect,  // const dest: psk_rect_t;
                            @LSamplingoptions, // const sampling: psk_samplingoptions_t;
                            LPaint, // const paint: sk_paint_t;
                            FAST_SK_SRCRECTCONSTRAINT); // constraint: sk_srcrectconstraint_t)
                        finally
                          sk4d_refcnt_unref(LImg);
                        end;
                      end;
                    end;
                  end;

                finally
                  // Remove the alpha layer
                  if compareValue(LOptions.Opacity, 1, Tepsilon.Scale) < 0 then
                    sk4d_canvas_restore(ACanvas);
                end;

              finally
                ALCanvasEndScene(ACanvas);
              end;

            finally
              sk4d_paragraph_destroy(LParagraph);
            end;

          finally
            sk4d_paragraphbuilder_destroy(LParagraphBuilder);
          end;

        finally
          sk4d_paragraphstyle_destroy(LParagraphstyle);
        end;

        // break the loop
        Break;

      end;

    finally
      sk4d_paint_destroy(LPaint);
      ALFreeAndNil(LFontFamilies);
      ALFreeAndNil(LFontSizes);
      ALFreeAndNil(LFontWeights);
      ALFreeAndNil(LFontSlants);
      ALFreeAndNil(LFontStretchs);
      ALFreeAndNil(LFontColors);
      ALFreeAndNil(LDecorationKinds);
      ALFreeAndNil(LDecorationStyles);
      ALFreeAndNil(LDecorationThicknessMultipliers);
      ALFreeAndNil(LDecorationColors);
      ALFreeAndNil(LBackGroundColors);
      ALFreeAndNil(LLineHeightMultipliers);
      ALFreeAndNil(LLetterSpacings);
      ALFreeAndNil(LSpanIDs);
      ALFreeAndNil(LPlaceHolders);
      ALFreeAndNil(LRangeIndexes);
    end;

    {$ENDIF}
    {$ENDREGION}

    {$REGION 'ANDROID/IOS/MACOS/MSWINDOWS'}
    {$IF not defined(ALSkiaEngine)}

    // Init out var
    ATextBroken := false;
    AAllTextDrawn := True;
    setlength(AElements, 0);

    // Init local var
    var LAddEllipsis: Integer := 0;
    var LCurrLineIndex: Integer := 0;
    var LCurrLineWidth: Single := 0;
    var LCurrLineHeight: Single := 0;
    var LCurrParagraphHeight: Single := 0;
    var LMaxLines: Integer;
    if LOptions.MaxLines > 0 then LMaxLines := LOptions.MaxLines
    else LMaxLines := MaxInt;
    var LFontFamilies := TALStringListW.create;
    var LFontSizes := Tlist<Single>.Create;
    var LFontWeights:= Tlist<TFontWeight>.Create;
    var LFontSlants := TList<TFontSlant>.Create;
    var LFontStretchs := TList<TFontStretch>.Create;
    var LFontColors := Tlist<TAlphaColor>.create;
    var LDecorationKinds := TList<TALTextDecorationKinds>.Create;
    var LDecorationStyles := TList<TALTextDecorationStyle>.Create;
    var LDecorationThicknessMultipliers := TList<Single>.Create;
    var LDecorationColors := TList<TAlphaColor>.Create;
    var LBackgroundColors := TList<TAlphaColor>.Create;
    var LLineHeightMultipliers := TList<Single>.Create;
    var LLetterSpacings := TList<Single>.Create;
    var LSpanIDs := TALStringListW.create;
    var LExtendedTextElements := TList<TExtendedTextElement>.Create;
    {$IF defined(ANDROID)}
    _Paint := TJPaint.JavaClass.init;
    {$ENDIF}
    try

      {$IF defined(ANDROID)}
      // Enabling this flag will cause all draw operations that support antialiasing to use it.
      _Paint.setAntiAlias(true);
      // Enabling this flag causes glyph advances to be computed with subpixel accuracy.
      _Paint.setSubpixelText(true);
      // enable bilinear sampling on scaled bitmaps. If cleared, scaled bitmaps will be drawn
      // with nearest neighbor sampling, likely resulting in artifacts.
      _Paint.setFilterBitmap(True);
      // Enabling this flag applies a dither to any blit operation where the target's
      // colour space is more constrained than the source.
      _Paint.setDither(true);

      _CurrFontFamily := '';
      _CurrFontSize := 0;
      _CurrFontWeight := TFontWeight.Regular;
      _CurrFontSlant := TFontSlant.Regular;
      _CurrFontStretch := TFontStretch.Regular;
      _CurrFontColor := TAlphaColors.Null;
      _CurrDecorationKinds := [];
      _CurrDecorationStyle := TALTextDecorationStyle.Solid;
      _CurrDecorationThicknessMultiplier := 1;
      _CurrDecorationColor := TAlphaColors.Null;
      _CurrLetterSpacing := 0;
      {$ENDIF}

      // Format LText
      var Ltext: String := ALTrimRight(AText);
      if LOptions.TextIsHtml then begin
        LText := ALStringReplaceW(Ltext, #13, ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, #10, ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, #9, ' ', [RfReplaceALL]);
        While ALPosW('  ', LText) > 0 do
          LText := ALStringReplaceW(Ltext, '  ', ' ', [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, '<br>', #10, [RfReplaceALL, RfIgnoreCase]);
        LText := ALStringReplaceW(Ltext, '<br/>', #10, [RfReplaceALL, RfIgnoreCase]);
        While ALPosW(' '#10, LText) > 0 do
          LText := ALStringReplaceW(Ltext, ' '#10, #10, [RfReplaceALL]);
        While ALPosW(#10' ', LText) > 0 do
          LText := ALStringReplaceW(Ltext, #10' ', #10, [RfReplaceALL]);
        LText := ALStringReplaceW(Ltext, '&nbsp;', Chr($A0){0x00A0}, [RfReplaceALL, RfIgnoreCase]);
      end
      else
        LText := ALStringReplaceW(Ltext, #13#10, #10, [RfReplaceALL]);


      ///////////////////////////////////
      // loop on all the html elements //
      ///////////////////////////////////

      var P1 := low(Ltext);
      while P1 <= high(Ltext) do begin

        var LIsEllipsis := False;
        var LCurrText: String;
        var LCurrImgSrc: String;
        var LcurrImgWidth: Single;
        var LcurrImgHeight: Single;


        /////////////////////////
        // Insert the ellipsis //
        /////////////////////////

        if LAddEllipsis > 0 then begin

          // Update AAllTextDrawn
          AAllTextDrawn := False;

          // Clear all accumulators
          LFontFamilies.Clear;
          LFontSizes.Clear;
          LFontWeights.Clear;
          LFontSlants.Clear;
          LFontStretchs.Clear;
          LFontColors.Clear;
          LDecorationKinds.clear;
          LDecorationStyles.clear;
          LDecorationThicknessMultipliers.clear;
          LDecorationColors.clear;
          LBackgroundColors.Clear;
          LLineHeightMultipliers.clear;
          LLetterSpacings.Clear;
          LSpanIDs.Clear;

          // Remove the ellipsis element if present
          While (LExtendedTextElements.Count > 0) and
                (LExtendedTextElements[LExtendedTextElements.Count-1].IsEllipsis) do
            LExtendedTextElements.Delete(LExtendedTextElements.Count-1);

          // Init LInteractiveEllipsis
          var LInteractiveEllipsis := (length(LOptions.EllipsisText) > 2) and (ALPosW('… ', LOptions.EllipsisText) = 1);

          // Internal Loop
          While True do begin

            // LAddEllipsis = 1 indicates it's the first time an ellipsis is being
            // added. In this scenario, we do not truncate the previous element.
            // Instead, we append the ellipsis '…' to its end if the ellispis is
            // like "… more[+]". In the subsequent loop, we will add the ellipsis span.
            if LAddEllipsis = 1 then begin

              // On the next loop add the ellipsis
              LAddEllipsis := 2;

              // Handle special case where ellispis is like "… more[+]"
              if LInteractiveEllipsis then begin
                While true do begin
                  if LExtendedTextElements.Count > 0 then begin
                    Var LExtendedTextElement := LExtendedTextElements[LExtendedTextElements.Count-1];
                    //--
                    if LExtendedTextElement.ImgSrc <> '' then begin
                      LCurrText := '… ';
                    end
                    else begin
                      LExtendedTextElements.Delete(LExtendedTextElements.Count-1);
                      LCurrText := ALTrimRight(LExtendedTextElement.Text);
                      While (LCurrText <> '') and
                            ((LCurrText.Chars[LCurrText.Length-1].IsPunctuation) or // ., ; : ? ! - – — ' " ( ) [ ] { } < > / \ … ‘ ’ “ ” * & @ # % _ | ~ ^ + = « » ¡ ¿
                             (LCurrText.Chars[LCurrText.Length-1].IsWhiteSpace) or // SPACE, TAB,LF,LINE-TAB,FF,CR, No-break Space, NEL
                             (LCurrText.Chars[LCurrText.Length-1].IsSeparator)) do // Space / No-Break Space
                        LCurrText := ALTrimRight(LCurrText.Remove(LCurrText.Length-1));
                      if LCurrText = '' then continue;
                      LCurrText := LCurrText + '… ';
                    end;
                    LCurrImgSrc := '';
                    LcurrImgWidth := 0;
                    LcurrImgHeight := 0;
                    P1 := -1;
                    //--
                    LFontFamilies.Add(LExtendedTextElement.FontFamily);
                    LFontSizes.Add(LExtendedTextElement.FontSize);
                    LFontWeights.Add(LExtendedTextElement.FontWeight);
                    LFontSlants.Add(LExtendedTextElement.FontSlant);
                    LFontStretchs.Add(LExtendedTextElement.FontStretch);
                    LFontColors.Add(LExtendedTextElement.FontColor);
                    LDecorationKinds.Add(LExtendedTextElement.DecorationKinds);
                    LDecorationStyles.Add(LExtendedTextElement.DecorationStyle);
                    LDecorationThicknessMultipliers.Add(LExtendedTextElement.DecorationThicknessMultiplier);
                    LDecorationColors.Add(LExtendedTextElement.DecorationColor);
                    LBackgroundColors.Add(LExtendedTextElement.BackgroundColor);
                    LLineHeightMultipliers.Add(LExtendedTextElement.LineHeightMultiplier);
                    LLetterSpacings.Add(LExtendedTextElement.LetterSpacing);
                    LSpanIDs.Add(LExtendedTextElement.Id);
                    //--
                    break; // While true do (2)
                  end
                  else begin
                    LCurrText := '';
                    break; // While true do (2)
                  end;
                end;
                IF LCurrText = '' then
                  Continue; // => Go to => else if LAddEllipsis = 2 then
                break; // While true do (1)
              end

              // => Go to => else if LAddEllipsis = 2 then
              else
                Continue;

            end

            // If LAddEllipsis = 2, it means we must add the ellipsis element.
            else if LAddEllipsis = 2 then begin
              if LInteractiveEllipsis then
                LCurrText := ALCopyStr(LOptions.EllipsisText, 3, maxint)
              else
                LCurrText := LOptions.EllipsisText;
              LCurrImgSrc := '';
              LcurrImgWidth := 0;
              LcurrImgHeight := 0;
              P1 := Maxint;
              //--
              if not LOptions.EllipsisInheritSettings then begin
                LFontFamilies.Add(LOptions.EllipsisFontFamily);
                LFontSizes.Add(LOptions.EllipsisFontSize);
                LFontWeights.Add(LOptions.EllipsisFontWeight);
                LFontSlants.Add(LOptions.EllipsisFontSlant);
                LFontStretchs.Add(LOptions.EllipsisFontStretch);
                LFontColors.Add(LOptions.EllipsisFontColor);
                LDecorationKinds.Add(LOptions.EllipsisDecorationKinds);
                LDecorationStyles.Add(LOptions.EllipsisDecorationStyle);
                LDecorationThicknessMultipliers.Add(LOptions.EllipsisDecorationThicknessMultiplier);
                LDecorationColors.Add(LOptions.EllipsisDecorationColor);
                LBackgroundColors.Add(TALphaColors.Null);
                LLineHeightMultipliers.Add(LOptions.LineHeightMultiplier);
                LLetterSpacings.Add(LOptions.LetterSpacing);
                LSpanIDs.Add('ellipsis');
              end
              else if LExtendedTextElements.Count > 0 then begin
                Var LExtendedTextElement := LExtendedTextElements[LExtendedTextElements.Count-1];
                LFontFamilies.Add(LExtendedTextElement.FontFamily);
                LFontSizes.Add(LExtendedTextElement.FontSize);
                LFontWeights.Add(LExtendedTextElement.FontWeight);
                LFontSlants.Add(LExtendedTextElement.FontSlant);
                LFontStretchs.Add(LExtendedTextElement.FontStretch);
                LFontColors.Add(LExtendedTextElement.FontColor);
                LDecorationKinds.Add(LExtendedTextElement.DecorationKinds);
                LDecorationStyles.Add(LExtendedTextElement.DecorationStyle);
                LDecorationThicknessMultipliers.Add(LExtendedTextElement.DecorationThicknessMultiplier);
                LDecorationColors.Add(LExtendedTextElement.DecorationColor);
                LBackgroundColors.Add(LExtendedTextElement.BackgroundColor);
                LLineHeightMultipliers.Add(LExtendedTextElement.LineHeightMultiplier);
                LLetterSpacings.Add(LExtendedTextElement.LetterSpacing);
                LSpanIDs.Add('ellipsis');
              end;
              // Else we have previously cleared all accumulators so setting will
              // be taken from the LOptions
              //--
              LIsEllipsis := True;
              //--
              break; // While true do (1)
            end

            // If LAddEllipsis = 3, it means we must remove the ellipsis element
            // and trunc the last element.
            else if LAddEllipsis > 2 then begin

              LAddEllipsis := Maxint;
              Var LExtendedTextElement: TExtendedTextElement;
              While LAddEllipsis = Maxint do begin

                // Remove the breaklines located at the end
                While (LExtendedTextElements.Count > 0) and (LExtendedTextElements[LExtendedTextElements.Count-1].IsBreakLine) do
                  LExtendedTextElements.Delete(LExtendedTextElements.Count-1);

                // If LExtendedTextElements.Count equals 0, we cannot truncate further. Since
                // LAddEllipsis is greater than or equal to 2, it means we have already attempted
                // to add just an ellipsis, and it failed. Therefore, we have no other choice but
                // to return an empty result.
                if LExtendedTextElements.Count = 0 then begin
                  ARect.Width := 0;
                  ARect.Height := 0;
                  exit;
                end;

                // Get the previous element
                LExtendedTextElement := LExtendedTextElements[LExtendedTextElements.Count-1];
                LExtendedTextElements.Delete(LExtendedTextElements.Count-1);

                // The element is an Image
                if LExtendedTextElement.ImgSrc <> '' then begin
                  LAddEllipsis := 1;
                  continue; // => Go to => else if LAddEllipsis = 1 then
                end

                // The element is a Text
                else begin
                  LCurrText := ALTrimRight(LExtendedTextElement.Text);
                  // Special case LInteractiveEllipsis = true and LCurrText = '…'
                  // Remove this element and redo the loop to trunk the previous
                  // element instead of this one
                  if LInteractiveEllipsis and (LCurrText = '…') then
                    continue; // => Go to => While LAddEllipsis = Maxint do begin
                  var LNumberOfChars: Integer;
                  if LInteractiveEllipsis then LNumberOfChars := LCurrText.Length - 2 // skip the '…'
                  else LNumberOfChars := LCurrText.Length - 1;
                  var LBreakPos := _findLastBreakPosition(
                                     LCurrText, // const AText: String;
                                     LNumberOfChars, // Const ANumberOfChars: Integer;
                                     (LExtendedTextElements.Count > 0) and (LExtendedTextElements[LExtendedTextElements.Count-1].IsBreakLine), // const AHardBreak: Boolean = False;
                                     true); // const ASkipEndOfTextPunctuation: Boolean = False
                  if LBreakPos <= 0 then begin
                    LAddEllipsis := 1;
                    continue; // => Go to => else if LAddEllipsis = 1 then
                  end;
                  LCurrText := LCurrText.Remove(LBreakPos);
                  if LInteractiveEllipsis then
                    LCurrText := LCurrText + '… ';
                  // On the next loop add the ellipsis
                  LAddEllipsis := 2;
                end;

              end;

              //--
              if LAddEllipsis <> 2 then
                continue; // => Go to => else if LAddEllipsis = xxx then
              //--
              LCurrImgSrc := '';
              LcurrImgWidth := 0;
              LcurrImgHeight := 0;
              P1 := -1;
              //--
              LFontFamilies.Add(LExtendedTextElement.FontFamily);
              LFontSizes.Add(LExtendedTextElement.FontSize);
              LFontWeights.Add(LExtendedTextElement.FontWeight);
              LFontSlants.Add(LExtendedTextElement.FontSlant);
              LFontStretchs.Add(LExtendedTextElement.FontStretch);
              LFontColors.Add(LExtendedTextElement.FontColor);
              LDecorationKinds.Add(LExtendedTextElement.DecorationKinds);
              LDecorationStyles.Add(LExtendedTextElement.DecorationStyle);
              LDecorationThicknessMultipliers.Add(LExtendedTextElement.DecorationThicknessMultiplier);
              LDecorationColors.Add(LExtendedTextElement.DecorationColor);
              LBackgroundColors.Add(LExtendedTextElement.BackgroundColor);
              LLineHeightMultipliers.Add(LExtendedTextElement.LineHeightMultiplier);
              LLetterSpacings.Add(LExtendedTextElement.LetterSpacing);
              LSpanIDs.Add(LExtendedTextElement.Id);
              //--
              break; // While true do (2)

            end;

          end;

          // Update LCurrLineIndex
          If LExtendedTextElements.Count > 0 then begin
            LCurrLineIndex := 0;
            LCurrLineHeight := 0;
            LCurrParagraphHeight := 0;
            for var I := 0 to LExtendedTextElements.Count - 1 do begin
              Var LExtendedTextElement := LExtendedTextElements[I];
              if LExtendedTextElement.LineIndex <> LCurrLineIndex then begin
                LCurrLineIndex := LExtendedTextElement.LineIndex;
                LCurrParagraphHeight := LCurrParagraphHeight + LCurrLineHeight;
                LCurrLineHeight := -LExtendedTextElement.Ascent + LExtendedTextElement.Descent;
              end
              else begin
                LCurrLineHeight := max(LCurrLineHeight, -LExtendedTextElement.Ascent + LExtendedTextElement.Descent);
              end;
            end;
            Var LExtendedTextElement := LExtendedTextElements[LExtendedTextElements.Count-1];
            LCurrLineIndex := LExtendedTextElement.LineIndex;
            LCurrLineWidth := LExtendedTextElement.Rect.Right;
          end
          else begin
            LCurrLineIndex := 0;
            LCurrLineHeight := 0;
            LCurrParagraphHeight := 0;
            LCurrLineWidth := 0;
          end;

        end

        ////////////////////////////
        // We are on a Break Line //
        ////////////////////////////

        else if Ltext[P1] = #10 then begin

          LCurrText := #10;
          LCurrImgSrc := '';
          LcurrImgWidth := 0;
          LcurrImgHeight := 0;
          P1 := P1 + 1;

        end

        /////////////////////////////////////
        // The text contains HTML elements //
        /////////////////////////////////////

        else if LOptions.TextIsHtml then begin

          // Extract LCurrText / LCurrImgSrc
          if Ltext[P1] = '<' then begin

            // Proper HTML requires that elements are correctly nested and
            // closed in the order they are opened.

            //-----
            LCurrImgSrc := '';
            LCurrText := '';
            LcurrImgWidth := 0;
            LcurrImgHeight := 0;
            var P2 := ALPosW('>', Ltext, P1+1); // blablabla <font color="#ffffff">blablabla</font> blablabla
                                                //           ^P1                  ^P2
            if P2 <= 0 then break;
            var LTag: String := ALCopyStr(Ltext, P1, P2 - P1 + 1); // <font color="#ffffff">
            P1 := P2 + 1; // blablabla <font color="#ffffff">blablabla</font> blablabla
                          //                                 ^P1

            //-----
            if (ALPosW('<b ', LTag) = 1) or
               (LTag = '<b>') then begin
              _getInfosFromTag(ALCopyStr(LTag, 4, length(LTag) - 4), LSpanIDs);
              LFontWeights.Add(TFontWeight.Bold);
            end
            else if LTag = '</b>' then begin
              if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
              if LFontWeights.count > 0 then LFontWeights.Delete(LFontWeights.Count - 1);
            end

            //-----
            else if (ALPosW('<i ', LTag) = 1) or
                    (LTag = '<i>') then begin
              _getInfosFromTag(ALCopyStr(LTag, 4, length(LTag) - 4), LSpanIDs);
              LFontSlants.Add(TFontSlant.Italic);
            end
            else if LTag = '</i>' then begin
              if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
              if LFontSlants.count > 0 then LFontSlants.Delete(LFontSlants.Count - 1);
            end

            //-----
            else if (ALPosW('<img ', LTag) = 1) or
                    (LTag = '<img/>') then begin // <img src="xxx">
              _getInfosFromTag(ALCopyStr(LTag, 6, length(LTag) - 6), LCurrImgSrc, LcurrImgWidth, LcurrImgHeight);
            end

            //-----
            else if (ALPosW('<font ', LTag) = 1) or
                    (LTag = '<font>')  then begin   // <font color="#ffffff">
              _getInfosFromTag(ALCopyStr(LTag, 7, length(LTag) - 7), LSpanIDs, LFontFamilies, LFontSizes, LFontColors);
            end
            else if LTag = '</font>' then begin
              if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
              if LFontFamilies.count > 0 then LFontFamilies.Delete(LFontFamilies.Count - 1);
              if LFontSizes.count > 0 then LFontSizes.Delete(LFontSizes.Count - 1);
              if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
            end

            //-----
            else if (ALPosW('<span ', LTag) = 1) or
                    (LTag = '<span>') then begin // <span id="xxx">
              _getInfosFromTag(ALCopyStr(LTag, 7, length(LTag) - 7), LSpanIDs, LFontFamilies, LFontSizes, LFontWeights, LFontSlants, LFontStretchs, LFontColors, LDecorationKinds, LDecorationStyles, LDecorationThicknessMultipliers, LDecorationColors, LBackgroundColors, LLineHeightMultipliers, LLetterSpacings);
            end
            else if LTag = '</span>' then begin
              if LSpanIDs.count > 0 then LSpanIDs.Delete(LSpanIDs.Count - 1);
              if LFontFamilies.count > 0 then LFontFamilies.Delete(LFontFamilies.Count - 1);
              if LFontSizes.count > 0 then LFontSizes.Delete(LFontSizes.Count - 1);
              if LFontWeights.count > 0 then LFontWeights.Delete(LFontWeights.Count - 1);
              if LFontSlants.count > 0 then LFontSlants.Delete(LFontSlants.Count - 1);
              if LFontStretchs.count > 0 then LFontStretchs.Delete(LFontStretchs.Count - 1);
              if LFontColors.count > 0 then LFontColors.Delete(LFontColors.Count - 1);
              if LDecorationKinds.count > 0 then LDecorationKinds.Delete(LDecorationKinds.Count - 1);
              if LDecorationStyles.count > 0 then LDecorationStyles.Delete(LDecorationStyles.Count - 1);
              if LDecorationThicknessMultipliers.count > 0 then LDecorationThicknessMultipliers.Delete(LDecorationThicknessMultipliers.Count - 1);
              if LDecorationColors.count > 0 then LDecorationColors.Delete(LDecorationColors.Count - 1);
              if LBackgroundColors.count > 0 then LBackgroundColors.Delete(LBackgroundColors.Count - 1);
              if LLineHeightMultipliers.count > 0 then LLineHeightMultipliers.Delete(LLineHeightMultipliers.Count - 1);
              if LLetterSpacings.count > 0 then LLetterSpacings.Delete(LLetterSpacings.Count - 1);
            end;

          end
          else begin

            LCurrImgSrc := '';
            LcurrImgWidth := 0;
            LcurrImgHeight := 0;
            var P2 := ALPosW('<', Ltext, P1);  // blablabla <font color="#ffffff">blablabla</font> blablabla
                                               //                                 ^P1      ^P2
            if P2 <= 0 then P2 := Maxint;
            var P3 := ALPosW(#10, Ltext, P1);
            if P3 <= 0 then P3 := Maxint;
            P2 := Min(P2,P3);
            LCurrText := ALCopyStr(Ltext, P1, P2 - P1);  // blablabla
            LCurrText := ALStringReplaceW(LCurrText, '&gt;', '>', [rfReplaceALL]);
            LCurrText := ALStringReplaceW(LCurrText, '&lt;', '<', [rfReplaceALL]);
            P1 := P2; // blablabla <font color="#ffffff">blablabla</font> blablabla
                      //                                          ^P1

          end;

        end

        /////////////////////////////////////////////
        // The text does NOT contain HTML elements //
        /////////////////////////////////////////////

        else begin

          LCurrImgSrc := '';
          LcurrImgWidth := 0;
          LcurrImgHeight := 0;
          var P2 := ALPosW(#10, Ltext, P1);  // blablabla #10blablabla#10 blablabla
                                             // ^P1       ^P2
          if P2 <= 0 then P2 := Maxint;
          LCurrText := ALCopyStr(Ltext, P1, P2 - P1);  // blablabla
          P1 := P2; // blablabla #10blablabla#10 blablabla
                    //           ^P1

        end;


        //////////////////////////////////////////////////
        // Add the current text to the ParagraphBuilder //
        //////////////////////////////////////////////////

        if (LCurrText <> '') or (LCurrImgSrc <> '') then begin

          // Init LFontFamily
          var LFontFamily: String;
          if LFontFamilies.Count > 0 then LFontFamily := LFontFamilies[LFontFamilies.Count - 1]
          else LFontFamily := LOptions.FontFamily;

          // Init LFontSize
          var LFontSize: Single;
          if LFontSizes.Count > 0 then LFontSize := LFontSizes[LFontSizes.Count - 1]
          else LFontSize := LOptions.FontSize;

          // Init LFontWeight
          var LFontWeight: TFontWeight;
          if LFontWeights.Count > 0 then LFontWeight := LFontWeights[LFontWeights.Count - 1]
          else LFontWeight := LOptions.FontWeight;

          // Init LFontSlant
          var LFontSlant: TFontSlant;
          if LFontSlants.Count > 0 then LFontSlant := LFontSlants[LFontSlants.Count - 1]
          else LFontSlant := LOptions.FontSlant;

          // Init LFontStretch
          var LFontStretch: TFontStretch;
          if LFontStretchs.Count > 0 then LFontStretch := LFontStretchs[LFontStretchs.Count - 1]
          else LFontStretch := LOptions.FontStretch;

          // Init LFontColor
          var LFontColor: TalphaColor;
          if LFontColors.Count > 0 then LFontColor := LFontColors[LFontColors.Count - 1]
          else LFontColor := LOptions.FontColor;

          // Init LDecorationKind
          var LDecorationKind: TALTextDecorationKinds;
          if LDecorationKinds.Count > 0 then LDecorationKind := LDecorationKinds[LDecorationKinds.Count - 1]
          else LDecorationKind := LOptions.DecorationKinds;

          // Init LDecorationStyle
          var LDecorationStyle: TALTextDecorationStyle;
          if LDecorationStyles.Count > 0 then LDecorationStyle := LDecorationStyles[LDecorationStyles.Count - 1]
          else LDecorationStyle := LOptions.DecorationStyle;

          // Init LDecorationThicknessMultiplier
          var LDecorationThicknessMultiplier: Single;
          if LDecorationThicknessMultipliers.Count > 0 then LDecorationThicknessMultiplier := LDecorationThicknessMultipliers[LDecorationThicknessMultipliers.Count - 1]
          else LDecorationThicknessMultiplier := LOptions.DecorationThicknessMultiplier;

          // Init LDecorationColor
          var LDecorationColor: TalphaColor;
          if LDecorationColors.Count > 0 then LDecorationColor := LDecorationColors[LDecorationColors.Count - 1]
          else LDecorationColor := LOptions.DecorationColor;

          // Init LBackgroundColor
          var LBackgroundColor: TalphaColor;
          if LBackgroundColors.Count > 0 then LBackgroundColor := LBackgroundColors[LBackgroundColors.Count - 1]
          else LBackgroundColor := TalphaColors.Null;

          // Init LLineHeightMultiplier
          var LLineHeightMultiplier: Single;
          if LLineHeightMultipliers.Count > 0 then LLineHeightMultiplier := LLineHeightMultipliers[LLineHeightMultipliers.Count - 1]
          else LLineHeightMultiplier := LOptions.LineHeightMultiplier;

          // Init LLetterSpacing
          var LLetterSpacing: Single;
          if LLetterSpacings.Count > 0 then LLetterSpacing := LLetterSpacings[LLetterSpacings.Count - 1]
          else LLetterSpacing := LOptions.LetterSpacing;

          // Init LSpanID
          var LSpanID: String;
          if LSpanIDs.Count > 0 then LSpanID := LSpanIDs[LSpanIDs.Count - 1]
          else LSpanID := '';

          // It's a text
          if LCurrText <> '' then begin

            // First get the Font Metrics
            var LFontMetrics := _GetFontMetrics(
                                  LFontFamily, // const AFontFamily: String;
                                  LFontSize, // const AFontSize: single;
                                  LFontWeight, // const AFontWeight: TFontWeight;
                                  LFontSlant); // const AFontSlant: TFontSlant;

            // Handle ALineHeightMultiplier
            // To be in pair with skia, by default, text will layout with line height as defined by the font. Font-metrics defined line height may be
            // taller or shorter than the font size. The height property allows manual adjustment of the height of the line
            // as a multiple of fontSize. For most fonts, setting height to 1.0 is not the same as omitting or setting
            // height to null.
            var LDrawTextOffsetY: Single;
            if not sameValue(LLineHeightMultiplier, 0, TEpsilon.Scale) then begin
              var LOldAscent := LFontMetrics.Ascent;
              var LRatio: Single := (LFontSize / (-LFontMetrics.Ascent + LFontMetrics.Descent));
              LFontMetrics.Ascent := -1 * LRatio * -LFontMetrics.Ascent * LLineHeightMultiplier;
              LFontMetrics.Descent := LRatio * LFontMetrics.Descent * LLineHeightMultiplier;
              LDrawTextOffsetY :=  (-1 * LFontMetrics.Ascent) - (-1 * LOldAscent);
            end
            else LDrawTextOffsetY := 0;


            // Now break the line
            While LCurrText <> '' do begin

              // No line left to add the text
              if LCurrLineIndex >= LMaxLines then begin
                inc(LAddEllipsis);
                P1 := -1;
                Break; // => break the loop => While LCurrText <> '' do begin => Go to the loop => while P1 <= high(Ltext) do begin
              end;

              // Do some measurements
              var LMeasuredWidth: Single;
              var LMeasuredHeight: Single;
              var LNumberOfChars: Integer;
              if LCurrText <> #10 then
                LNumberOfChars := _BreakText(
                                    LCurrText, // const AText: String;
                                    LFontFamily, // const AFontFamily: String;
                                    LFontSize, // const AFontSize: single;
                                    LFontWeight, // const AFontWeight: TFontWeight;
                                    LFontSlant, // const AFontSlant: TFontSlant;
                                    LFontStretch, // const AFontStretch: TFontStretch;
                                    LFontColor, // const AFontColor: TalphaColor;
                                    LDecorationKind, // const ADecorationKinds: TALTextDecorationKinds;
                                    LDecorationStyle, // const ADecorationStyle: TALTextDecorationStyle;
                                    LDecorationThicknessMultiplier, // const ADecorationThicknessMultiplier: Single;
                                    LDecorationColor, // const ADecorationColor: TAlphaColor;
                                    LLetterSpacing, // const ALetterSpacing: Single;
                                    LOptions.Direction, // const ADirection: TALTextDirection
                                    ARect.Width - LOptions.Padding.Left - LOptions.Padding.Right - LCurrLineWidth, // const AMaxWidth: Single;
                                    samevalue(LCurrLineWidth, 0, TEpsilon.Position), // const AHardBreak: Boolean;
                                    LMeasuredWidth, // out AMeasuredWidth: Single): integer;
                                    LMeasuredHeight) // out AMeasuredHeight: Single): integer;
              else begin
                LMeasuredWidth := 0;
                LMeasuredHeight := 0;
                LNumberOfChars := 0;
              end;

              // No horizontal space left to add the text
              If LNumberOfChars = 0 then begin
                if (LCurrText <> #10) and samevalue(LCurrLineWidth, 0, TEpsilon.Position) then begin
                  inc(LAddEllipsis);
                  P1 := -1;
                  Break; // => break the loop => While LCurrText <> '' do begin => Go to the loop => while P1 <= high(Ltext) do begin
                end;
                //--
                inc(LCurrLineIndex);
                LCurrParagraphHeight := LCurrParagraphHeight + LCurrLineHeight;
                LCurrLineWidth := 0;
                LCurrLineHeight := 0;
                ATextBroken := True;
                if LOptions.FailIfTextBroken then begin
                  ARect.Width := 0;
                  ARect.Height := 0;
                  exit;
                end;
                //--
                var LExtendedTextElement: TExtendedTextElement;
                LExtendedTextElement.Id := '';
                LExtendedTextElement.LineIndex := LCurrLineIndex;
                LExtendedTextElement.Rect := TrectF.Create(TpointF.Create(0, 0), 0, -LFontMetrics.Ascent + LFontMetrics.Descent);
                LExtendedTextElement.DrawTextOffsetY := 0;
                LExtendedTextElement.Ascent := LFontMetrics.Ascent;
                LExtendedTextElement.Descent := LFontMetrics.Descent;
                LExtendedTextElement.LineHeightMultiplier := LLineHeightMultiplier;
                LExtendedTextElement.LetterSpacing := LLetterSpacing;
                LExtendedTextElement.Text := '';
                LExtendedTextElement.IsEllipsis := LIsEllipsis;
                LExtendedTextElement.IsBreakLine := True;
                LExtendedTextElement.BackgroundColor := LBackgroundColor;
                LExtendedTextElement.FontFamily := LFontFamily;
                LExtendedTextElement.FontSize := LFontSize;
                LExtendedTextElement.FontWeight := LFontWeight;
                LExtendedTextElement.FontSlant := LFontSlant;
                LExtendedTextElement.FontStretch := LFontStretch;
                LExtendedTextElement.FontColor := LFontColor;
                LExtendedTextElement.DecorationKinds := LDecorationKind;
                LExtendedTextElement.DecorationStyle := LDecorationStyle;
                LExtendedTextElement.DecorationThicknessMultiplier := LDecorationThicknessMultiplier;
                LExtendedTextElement.DecorationColor := LDecorationColor;
                LExtendedTextElement.ImgSrc := '';
                LExtendedTextElements.add(LExtendedTextElement);
                //--
                if LCurrText = #10 then break // => break the loop => While LCurrText <> '' do begin => Go to the loop => while P1 <= high(Ltext) do begin
                else Continue; // => redo the loop => While LCurrText <> '' do begin
              end;

              // No Vertical space left to add the Image
              If CompareValue(LCurrParagraphHeight + (-1 * LFontMetrics.Ascent) + LFontMetrics.Descent, ARect.Height - LOptions.Padding.Top - LOptions.Padding.Bottom, TEpsilon.Position) > 0 then begin
                inc(LAddEllipsis);
                P1 := -1;
                Break; // => break the loop => While LCurrText <> '' do begin => Go to the loop => while P1 <= high(Ltext) do begin
              end;

              // Everything is fine, add the Element
              var LExtendedTextElement: TExtendedTextElement;
              LExtendedTextElement.Id := LspanID;
              LExtendedTextElement.LineIndex := LCurrLineIndex;
              if CompareValue(LMeasuredHeight, ceil(-LFontMetrics.Ascent + LFontMetrics.Descent), Tepsilon.Position) > 0 then begin
                var LOldLineHeight := -LFontMetrics.Ascent + LFontMetrics.Descent;
                LExtendedTextElement.Ascent := (LFontMetrics.Ascent / LOldLineHeight) * LMeasuredHeight;
                LExtendedTextElement.Descent := (LFontMetrics.Descent / LOldLineHeight) * LMeasuredHeight;
              end
              else begin
                LExtendedTextElement.Ascent := LFontMetrics.Ascent;
                LExtendedTextElement.Descent := LFontMetrics.Descent;
              end;
              LExtendedTextElement.DrawTextOffsetY := LDrawTextOffsetY;
              LExtendedTextElement.Rect := TrectF.Create(TpointF.Create(LCurrLineWidth, 0), LMeasuredWidth, -LExtendedTextElement.Ascent + LExtendedTextElement.Descent);
              LCurrLineWidth := LCurrLineWidth + LMeasuredWidth;
              LCurrLineHeight := Max(LCurrLineHeight, -LExtendedTextElement.Ascent + LExtendedTextElement.Descent);
              LExtendedTextElement.LineHeightMultiplier := LLineHeightMultiplier;
              LExtendedTextElement.LetterSpacing := LLetterSpacing;
              LExtendedTextElement.Text := LCurrText.Substring(0, LNumberOfChars);
              LExtendedTextElement.IsEllipsis := LIsEllipsis;
              LExtendedTextElement.IsBreakLine := False;
              LExtendedTextElement.BackgroundColor := LBackgroundColor;
              LExtendedTextElement.FontFamily := LFontFamily;
              LExtendedTextElement.FontSize := LFontSize;
              LExtendedTextElement.FontWeight := LFontWeight;
              LExtendedTextElement.FontSlant := LFontSlant;
              LExtendedTextElement.FontStretch := LFontStretch;
              LExtendedTextElement.FontColor := LFontColor;
              LExtendedTextElement.DecorationKinds := LDecorationKind;
              LExtendedTextElement.DecorationStyle := LDecorationStyle;
              LExtendedTextElement.DecorationThicknessMultiplier := LDecorationThicknessMultiplier;
              LExtendedTextElement.DecorationColor := LDecorationColor;
              LExtendedTextElement.ImgSrc := '';
              LExtendedTextElements.add(LExtendedTextElement);

              // Increase the number of lines
              if LNumberOfChars < LCurrText.Length then begin
                inc(LCurrLineIndex);
                LCurrParagraphHeight := LCurrParagraphHeight + LCurrLineHeight;
                LCurrLineWidth := 0;
                LCurrLineHeight := 0;
                ATextBroken := True;
                if LOptions.FailIfTextBroken then begin
                  ARect.Width := 0;
                  ARect.Height := 0;
                  exit;
                end;
                //--
                //var LExtendedTextElement: TExtendedTextElement;
                LExtendedTextElement.Id := '';
                LExtendedTextElement.LineIndex := LCurrLineIndex;
                LExtendedTextElement.Rect := TrectF.Create(TpointF.Create(0, 0), 0, -LFontMetrics.Ascent + LFontMetrics.Descent);
                LExtendedTextElement.DrawTextOffsetY := 0;
                LExtendedTextElement.Ascent := LFontMetrics.Ascent;
                LExtendedTextElement.Descent := LFontMetrics.Descent;
                LExtendedTextElement.LineHeightMultiplier := LLineHeightMultiplier;
                LExtendedTextElement.LetterSpacing := LLetterSpacing;
                LExtendedTextElement.Text := '';
                LExtendedTextElement.IsEllipsis := LIsEllipsis;
                LExtendedTextElement.IsBreakLine := True;
                LExtendedTextElement.BackgroundColor := LBackgroundColor;
                LExtendedTextElement.FontFamily := LFontFamily;
                LExtendedTextElement.FontSize := LFontSize;
                LExtendedTextElement.FontWeight := LFontWeight;
                LExtendedTextElement.FontSlant := LFontSlant;
                LExtendedTextElement.FontStretch := LFontStretch;
                LExtendedTextElement.FontColor := LFontColor;
                LExtendedTextElement.DecorationKinds := LDecorationKind;
                LExtendedTextElement.DecorationStyle := LDecorationStyle;
                LExtendedTextElement.DecorationThicknessMultiplier := LDecorationThicknessMultiplier;
                LExtendedTextElement.DecorationColor := LDecorationColor;
                LExtendedTextElement.ImgSrc := '';
                LExtendedTextElements.add(LExtendedTextElement);
              end;

              // Remove white space between lines
              While (LNumberOfChars < LCurrText.Length) and (_IsWhiteSpace(LCurrText.Chars[LNumberOfChars])) do
                inc(LNumberOfChars, 1);

              // Update LCurrText
              LCurrText := LCurrText.remove(0, LNumberOfChars);

            end;

          end

          // It's an Image
          else if LCurrImgSrc <> '' then begin

            // Update LCurrImgWidth / LCurrImgHeight
            if CompareValue(LCurrImgWidth, 0, TEpsilon.FontSize) <= 0 then LCurrImgWidth := LOptions.FontSize;
            if CompareValue(LCurrImgHeight, 0, TEpsilon.FontSize) <= 0 then LCurrImgHeight := LOptions.FontSize;

            // No horizontal space left to add the Image
            If CompareValue(LCurrLineWidth + LCurrImgWidth, ARect.Width - LOptions.Padding.Left - LOptions.Padding.Right, TEpsilon.Position) > 0 then begin
              if samevalue(LCurrLineWidth, 0, TEpsilon.Position) then begin
                inc(LAddEllipsis);
                P1 := -1;
                continue; // => => Go to the loop => while P1 <= high(Ltext) do begin
              end;
              //--
              inc(LCurrLineIndex);
              LCurrParagraphHeight := LCurrParagraphHeight + LCurrLineHeight;
              LCurrLineWidth := 0;
              LCurrLineHeight := 0;
              ATextBroken := True;
              if LOptions.FailIfTextBroken then begin
                ARect.Width := 0;
                ARect.Height := 0;
                exit;
              end;
              //--
              var LFontMetrics := _GetFontMetrics(
                                    LFontFamily, // const AFontFamily: String;
                                    LFontSize, // const AFontSize: single;
                                    LFontWeight, // const AFontWeight: TFontWeight;
                                    LFontSlant); // const AFontSlant: TFontSlant;
              //--
              var LExtendedTextElement: TExtendedTextElement;
              LExtendedTextElement.Id := '';
              LExtendedTextElement.LineIndex := LCurrLineIndex;
              LExtendedTextElement.Rect := TrectF.Create(TpointF.Create(0, 0), 0, -LFontMetrics.Ascent + LFontMetrics.Descent);
              LExtendedTextElement.DrawTextOffsetY := 0;
              LExtendedTextElement.Ascent := LFontMetrics.Ascent;
              LExtendedTextElement.Descent := LFontMetrics.Descent;
              LExtendedTextElement.LineHeightMultiplier := LLineHeightMultiplier;
              LExtendedTextElement.LetterSpacing := LLetterSpacing;
              LExtendedTextElement.Text := '';
              LExtendedTextElement.IsEllipsis := LIsEllipsis;
              LExtendedTextElement.IsBreakLine := True;
              LExtendedTextElement.BackgroundColor := LBackgroundColor;
              LExtendedTextElement.FontFamily := LFontFamily;
              LExtendedTextElement.FontSize := LFontSize;
              LExtendedTextElement.FontWeight := LFontWeight;
              LExtendedTextElement.FontSlant := LFontSlant;
              LExtendedTextElement.FontStretch := LFontStretch;
              LExtendedTextElement.FontColor := LFontColor;
              LExtendedTextElement.DecorationKinds := LDecorationKind;
              LExtendedTextElement.DecorationStyle := LDecorationStyle;
              LExtendedTextElement.DecorationThicknessMultiplier := LDecorationThicknessMultiplier;
              LExtendedTextElement.DecorationColor := LDecorationColor;
              LExtendedTextElement.ImgSrc := '';
              LExtendedTextElements.add(LExtendedTextElement);
            end;

            // No Vertical space left to add the Image
            If CompareValue(LCurrParagraphHeight + LCurrImgHeight, ARect.Height - LOptions.Padding.Top - LOptions.Padding.Bottom, TEpsilon.Position) > 0 then begin
              inc(LAddEllipsis);
              P1 := -1;
              continue; // => => Go to the loop => while P1 <= high(Ltext) do begin
            end;

            // No line left to add the Image
            if LCurrLineIndex >= LMaxLines then begin
              inc(LAddEllipsis);
              P1 := -1;
              continue; // => => Go to the loop => while P1 <= high(Ltext) do begin
            end;

            // Everything is fine, add the Element
            var LExtendedTextElement: TExtendedTextElement;
            LExtendedTextElement.Id := LspanID;
            LExtendedTextElement.LineIndex := LCurrLineIndex;
            LExtendedTextElement.Rect := TrectF.Create(TpointF.Create(LCurrLineWidth, 0), LcurrImgWidth, LcurrImgHeight);
            LCurrLineWidth := LCurrLineWidth + LCurrImgWidth;
            LCurrLineHeight := Max(LCurrLineHeight, LCurrImgHeight);
            LExtendedTextElement.DrawTextOffsetY := 0;
            LExtendedTextElement.Ascent := 0;
            LExtendedTextElement.Descent := 0;
            LExtendedTextElement.LineHeightMultiplier := LLineHeightMultiplier;
            LExtendedTextElement.LetterSpacing := LLetterSpacing;
            LExtendedTextElement.Text := '';
            LExtendedTextElement.IsEllipsis := LIsEllipsis;
            LExtendedTextElement.IsBreakLine := False;
            LExtendedTextElement.BackgroundColor := LBackgroundColor;
            LExtendedTextElement.FontFamily := LFontFamily;
            LExtendedTextElement.FontSize := LFontSize;
            LExtendedTextElement.FontWeight := LFontWeight;
            LExtendedTextElement.FontSlant := LFontSlant;
            LExtendedTextElement.FontStretch := LFontStretch;
            LExtendedTextElement.FontColor := LFontColor;
            LExtendedTextElement.DecorationKinds := LDecorationKind;
            LExtendedTextElement.DecorationStyle := LDecorationStyle;
            LExtendedTextElement.DecorationThicknessMultiplier := LDecorationThicknessMultiplier;
            LExtendedTextElement.DecorationColor := LDecorationColor;
            LExtendedTextElement.ImgSrc := LCurrImgSrc;
            LExtendedTextElements.add(LExtendedTextElement);

          end;

        end;

      end;


      /////////////////////////
      // Paint the paragraph //
      /////////////////////////

      // Order elements inside LExtendedTextElements
      var LPrevParagraphHeight: single := 0;
      var LParagraphRect := TrectF.Empty;
      var LBaseLine: Single := 0;
      var LTextLineHeight: Single := 0;
      var LImgLineHeight: Single := 0;
      LCurrLineIndex := -1;
      For var I := 0 to LExtendedTextElements.count - 1 do begin
        var LExtendedTextElement := LExtendedTextElements[i];
        //--
        if (LExtendedTextElement.isbreakline) and
           (I < LExtendedTextElements.count - 1) and
           (LExtendedTextElement.LineIndex = LExtendedTextElements[I + 1].LineIndex) then
          continue;
        //--
        If LExtendedTextElement.LineIndex <> LCurrLineIndex then begin
          LCurrLineIndex := LExtendedTextElement.LineIndex;
          //--
          var LMaxAscent: Single := -LExtendedTextElement.Ascent;
          var LMaxDescent: Single := LExtendedTextElement.Descent;
          if LExtendedTextElement.ImgSrc <> '' then begin
            LTextLineHeight := 0;
            LImgLineHeight := LExtendedTextElement.Rect.Height;
          end
          else begin
            LTextLineHeight := LMaxAscent + LMaxDescent;
            LImgLineHeight := 0;
          end;
          //--
          for var J := I + 1 to LExtendedTextElements.count - 1 do begin
            var L_TMP_ExtendedTextElement := LExtendedTextElements[j];
            if L_TMP_ExtendedTextElement.LineIndex <> LExtendedTextElement.LineIndex then break;
            LMaxAscent := max(LMaxAscent, -L_TMP_ExtendedTextElement.Ascent);
            LMaxDescent := max(LMaxDescent, L_TMP_ExtendedTextElement.Descent);
            if L_TMP_ExtendedTextElement.ImgSrc <> '' then begin
              var LPrevImgLineHeight := LImgLineHeight;
              LImgLineHeight := max(LImgLineHeight, L_TMP_ExtendedTextElement.Rect.Height);
            end
            else begin
              var LPrevTextLineHeight := LTextLineHeight;
              LTextLineHeight := max(LTextLineHeight, LMaxAscent + LMaxDescent);
            end;
          end;
          //--
          LPrevParagraphHeight := LParagraphRect.height;
          LParagraphRect.height := LParagraphRect.height + max(LImgLineHeight, LTextLineHeight);
          LBaseLine := LMaxAscent;
        end;
        //--
        LParagraphRect.Width := Max(LParagraphRect.Width, LExtendedTextElement.Rect.Right);
        //--
        if LExtendedTextElement.ImgSrc <> '' then
          LExtendedTextElement.Rect.Offset(0, LPrevParagraphHeight - LExtendedTextElement.Rect.Top + max(0, (LTextLineHeight - LImgLineHeight) / 2))
        else
          LExtendedTextElement.Rect.Offset(0, LBaseLine - (-1 * LExtendedTextElement.Ascent) + LPrevParagraphHeight - LExtendedTextElement.Rect.Top + max(0, (LImgLineHeight - LTextLineHeight) / 2));
        LExtendedTextElements[i] := LExtendedTextElement;
      end;
      LParagraphRect.height := LParagraphRect.height;

      // Autosize
      if LOptions.Autosize or (LOptions.AutosizeX and LOptions.AutosizeY) then begin
        ARect.Width := Min(ARect.Width, LParagraphRect.Width + LOptions.Padding.Left + LOptions.Padding.Right);
        ARect.Height := Min(ARect.Height, LParagraphRect.Height + LOptions.Padding.Top + LOptions.Padding.Bottom);
      end
      else if LOptions.AutosizeX then ARect.Width := Min(ARect.Width, LParagraphRect.Width + LOptions.Padding.Left + LOptions.Padding.Right)
      else if LOptions.AutosizeY then ARect.Height := Min(ARect.Height, LParagraphRect.Height + LOptions.Padding.Top + LOptions.Padding.Bottom);

      // HTextAlign/VTextAlign
      // TALTextHorzAlign.Justify is not yet supported
      var LHTextAlign := LOptions.HTextAlign;
      if LOptions.Direction = TALTextDirection.RightToLeft then begin
        case LHTextAlign of
          TALTextHorzAlign.Center:;
          TALTextHorzAlign.Leading: LHTextAlign := TALTextHorzAlign.Trailing;
          TALTextHorzAlign.Trailing: LHTextAlign := TALTextHorzAlign.Leading;
          TALTextHorzAlign.Justify:;
          else raise Exception.Create('Error A8C0EAAF-8B9F-4941-A533-5A5A0C4B4709');
        end;
      end;
      case LHTextAlign of
        TALTextHorzAlign.Center: begin
          LCurrLineIndex := -1;
          LCurrLineWidth := 0;
          for var I := LExtendedTextElements.count - 1 downto 0 do begin
            var LExtendedTextElement := LExtendedTextElements[i];
            If LExtendedTextElement.LineIndex <> LCurrLineIndex then begin
              LCurrLineIndex := LExtendedTextElement.LineIndex;
              LCurrLineWidth := LExtendedTextElement.Rect.Right;
            end;
            LExtendedTextElement.Rect.Offset((LParagraphRect.Width - LCurrLineWidth) / 2, 0);
            LExtendedTextElements[i] := LExtendedTextElement;
          end;
          LParagraphRect.Offset(LOptions.Padding.Left + ((ARect.Width-LOptions.Padding.Left-LOptions.Padding.Right-LParagraphRect.Width) / 2), 0);
        end;
        //--
        TALTextHorzAlign.Leading,
        TALTextHorzAlign.Justify: begin
          LParagraphRect.Offset(LOptions.Padding.Left, 0);
        end;
        //--
        TALTextHorzAlign.Trailing: begin
          LCurrLineIndex := -1;
          LCurrLineWidth := 0;
          for var I := LExtendedTextElements.count - 1 downto 0 do begin
            var LExtendedTextElement := LExtendedTextElements[i];
            If LExtendedTextElement.LineIndex <> LCurrLineIndex then begin
              LCurrLineIndex := LExtendedTextElement.LineIndex;
              LCurrLineWidth := LExtendedTextElement.Rect.Right;
            end;
            LExtendedTextElement.Rect.Offset(LParagraphRect.Width - LCurrLineWidth, 0);
            LExtendedTextElements[i] := LExtendedTextElement;
          end;
          LParagraphRect.Offset((ARect.Width-LParagraphRect.Width-LOptions.Padding.Right), 0);
        end;
        //--
        else raise Exception.Create('Error 768AE40A-1C99-47BD-BD6E-1F7AABFB018C');
      end;
      case LOptions.VTextAlign of
        TALTextVertAlign.Center: LParagraphRect.Offset(0, LOptions.Padding.Top + ((ARect.Height - LParagraphRect.Height - LOptions.Padding.Top - LOptions.Padding.Bottom) / 2));
        TALTextVertAlign.Leading: LParagraphRect.Offset(0, LOptions.Padding.Top);
        TALTextVertAlign.Trailing: LParagraphRect.Offset(0, ARect.Height - LOptions.Padding.Bottom - LParagraphRect.Height);
        Else raise Exception.Create('Error 6DBA0B08-4F9E-4D89-9998-6B054D527F1F');
      end;

      // Update AElements
      Var I := 0;
      Setlength(AElements, LExtendedTextElements.Count);
      for var J := 0 to LExtendedTextElements.Count - 1 do begin
        var LExtendedTextElement := LExtendedTextElements[J];
        If LExtendedTextElement.id <> '' then begin
          AElements[I].Id := LExtendedTextElement.ID;
          AElements[I].rect := TRectF.Create(
                                 (LParagraphRect.Left + LExtendedTextElement.Rect.Left) / LScale,
                                 (LParagraphRect.Top + LExtendedTextElement.Rect.Top) / LScale,
                                 (LParagraphRect.Left + LExtendedTextElement.Rect.Right) / LScale,
                                 (LParagraphRect.Top + LExtendedTextElement.Rect.Bottom) / LScale);
          inc(I);
        end;
      end;
      setlength(AElements, I);

      // Calculate the SurfaceRect
      var LSurfaceRect := ALGetShapeSurfaceRect(
                            ARect, // const ARect: TRectF;
                            LOptions.FillColor, // const AFillColor: TAlphaColor;
                            LOptions.FillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
                            LOptions.FillResourceName, // const AFillResourceName: String;
                            LOptions.FillBackgroundMargins, // Const AFillBackgroundMarginsRect: TRectF;
                            LOptions.FillImageMargins, // Const AFillImageMarginsRect: TRectF;
                            LOptions.StateLayerOpacity, // const AStateLayerOpacity: Single;
                            LOptions.StateLayerColor, // const AStateLayerColor: TAlphaColor;
                            false, // const AStateLayerUseContentColor: Boolean;
                            LOptions.StateLayerMargins, // Const AStateLayerMarginsRect: TRectF;
                            LOptions.ShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
                            LOptions.ShadowBlur, // const AShadowBlur: Single;
                            LOptions.ShadowOffsetX, // const AShadowOffsetX: Single;
                            LOptions.ShadowOffsetY); // const AShadowOffsetY: Single;
      if ALIsCanvasNull(ACanvas) then
        ARect.Offset(-LSurfaceRect.Left, -LSurfaceRect.top);

      // Adjust the rect
      if Assigned(LOptions.OnAdjustRect) then begin
        var LSurfaceSize := LSurfaceRect.Size;
        LOptions.OnAdjustRect(ACanvas, LOptions, ARect, LSurfaceSize);
        LSurfaceRect.Size := LSurfaceSize;
      end;

      // Offset the ParagraphRect and AElements according to the Arect coordinates
      LParagraphRect.Offset(ARect.Left, ARect.top);
      for I := low(AElements) to high(AElements) do
        AElements[I].rect.Offset(ARect.Left, ARect.top);

      // Though it's an unlikely scenario, this ensures avoidance of a crash in
      // the subsequent ALCreateSurface call.
      If (ALCeil(ARect.Width, TEpsilon.Position) = 0) or
         (ALCeil(ARect.Height, TEpsilon.Position) = 0) then begin
        ARect.Width := 0;
        ARect.Height := 0;
        exit;
      end;

      // 4096 x 4096 pixels is generally considered a safe maximum size
      // for surfaces across many systems
      If (LSurfaceRect.Width > 4096) or
         (LSurfaceRect.Height > 4096) then begin
        ARect.Width := 0;
        ARect.Height := 0;
        exit;
      end;

      // Exit if AOnlyMeasure
      if AOnlyMeasure then exit;

      // Create the drawing surface
      if ALIsCanvasNull(ACanvas) then begin
        if not ALIsSurfaceNull(ASurface) then
          raise Exception.Create('ASurface must also be null when ACanvas is null');
        // Use Ceil instead of Round because if the height is, for example, 75.4, it's better to have
        // 76 px in height than 75 px. If there is a border, then it will look better in such cases.
        ALCreateSurface(
          ASurface, // out ASurface: TALSurface;
          ACanvas, // out ACanvas: TALCanvas;
          ALCeil(LSurfaceRect.Width, TEpsilon.Position), // const w: integer;
          ALCeil(LSurfaceRect.Height, TEpsilon.Position));// const h: integer)
      end;
      if ALCanvasBeginScene(ACanvas) then
      try

        // Create the alpha layer
        if compareValue(LOptions.Opacity, 1, Tepsilon.Scale) < 0 then begin

          {$REGION 'ANDROID'}
          {$IF defined(ANDROID)}
          var LJLayerRect := TJRectF.JavaClass.init(LSurfaceRect.left, LSurfaceRect.top, LSurfaceRect.right, LSurfaceRect.bottom);
          aCanvas.saveLayerAlpha(LJLayerRect, round(255 * LOptions.Opacity));
          LJLayerRect := nil;
          {$ENDIF}
          {$ENDREGION}

          {$REGION 'IOS/MACOS'}
          {$IF defined(ALAppleOS)}
          CGContextSaveGState(ACanvas);
          CGContextSetAlpha(ACanvas, LOptions.Opacity);
          CGContextBeginTransparencyLayerWithRect(
            ACanvas,
            ALLowerLeftCGRect(
              LSurfaceRect.TopLeft,
              LSurfaceRect.Width,
              LSurfaceRect.Height,
              CGBitmapContextGetHeight(ACanvas)),
              nil{auxiliaryInfo});
          {$ENDIF}
          {$ENDREGION}

        end;
        try

          // Handle custom event
          if Assigned(LOptions.OnBeforeDrawBackground) then
            LOptions.OnBeforeDrawBackground(ACanvas, LOptions, ARect);

          // Draw the background
          if (LOptions.FillColor <> TalphaColors.Null) or
             (length(LOptions.FillGradientColors) > 0) or
             (LOptions.FillResourceName <> '') or
             (LOptions.StateLayerColor <> TalphaColors.Null) or
             (LOptions.StrokeColor <> TalphaColors.Null) or
             (LOptions.ShadowColor <> TalphaColors.Null) then begin
            ALDrawRectangle(
              ACanvas, // const ACanvas: TALCanvas;
              LOptions.Scale, // const AScale: Single;
              LOptions.AlignToPixel, // const AAlignToPixel: Boolean;
              ARect, // const ARect: TrectF;
              1, //const AOpacity: Single;
              LOptions.FillColor, // const AFillColor: TAlphaColor;
              LOptions.FillGradientStyle, // const AFillGradientStyle: TGradientStyle;
              LOptions.FillGradientColors, // const AFillGradientColors: TArray<TAlphaColor>;
              LOptions.FillGradientOffsets, // const AFillGradientOffsets: TArray<Single>;
              LOptions.FillGradientAngle, // const AFillGradientAngle: Single;
              LOptions.FillResourceName, // const AFillResourceName: String;
              LOptions.FillWrapMode, // Const AFillWrapMode: TALImageWrapMode;
              LOptions.FillBackgroundMargins, // Const AFillBackgroundMarginsRect: TRectF;
              LOptions.FillImageMargins, // Const AFillImageMarginsRect: TRectF;
              LOptions.StateLayerOpacity, // const AStateLayerOpacity: Single;
              LOptions.StateLayerColor, // const AStateLayerColor: TAlphaColor;
              LOptions.StateLayerMargins, // Const AStateLayerMarginsRect: TRectF;
              LOptions.StateLayerXRadius, // const AStateLayerXRadius: Single;
              LOptions.StateLayerYRadius, // const AStateLayerYRadius: Single;
              True, // const ADrawStateLayerOnTop: Boolean;
              LOptions.StrokeColor, // const AStrokeColor: TalphaColor;
              LOptions.StrokeThickness, // const AStrokeThickness: Single;
              LOptions.ShadowColor, // const AShadowColor: TAlphaColor; // If ShadowColor is not null, then the Canvas must have enough space to draw the shadow (approximately ShadowBlur on each side of the rectangle)
              LOptions.ShadowBlur, // const AShadowBlur: Single;
              LOptions.ShadowOffsetX, // const AShadowOffsetX: Single;
              LOptions.ShadowOffsetY, // const AShadowOffsetY: Single;
              LOptions.Sides, // const Sides: TSides;
              LOptions.Corners, // const Corners: TCorners;
              LOptions.XRadius, // const XRadius: Single = 0;
              LOptions.YRadius); // const YRadius: Single = 0);
          end;

          // Handle custom event
          if Assigned(LOptions.OnBeforeDrawParagraph) then
            LOptions.OnBeforeDrawParagraph(ACanvas, LOptions, ARect);

          {$REGION 'ANDROID'}
          {$IF defined(ANDROID)}

          for i := 0 to LExtendedTextElements.count - 1 do begin
            var LExtendedTextElement := LExtendedTextElements[i];
            if LExtendedTextElement.imgSrc <> '' then begin
              Var LDstRect := LExtendedTextElement.Rect;
              LDstRect.Offset(LParagraphRect.TopLeft);
              var LSrcRect := TRectF.Create(0,0,LDstRect.Width, LDstRect.Height);
              var LImg := ALLoadFromResourceAndStretchToJBitmap(LExtendedTextElement.imgSrc, LDstRect.Width, LDstRect.Height);
              try
                ACanvas.drawBitmap(LImg, LDstRect.left {left}, LDstRect.top {top}, _Paint {paint});
              finally
                LImg.recycle;
                LImg := nil;
              end;
            end
            else begin
              Var LDstRect := LExtendedTextElement.Rect;
              LDstRect.Offset(LParagraphRect.TopLeft);
              if LExtendedTextElement.BackgroundColor <> TAlphaColors.Null then begin
                _CurrFontColor := LExtendedTextElement.BackgroundColor;
                _Paint.setColor(integer(LExtendedTextElement.BackgroundColor));
                ACanvas.drawRect(
                  LDstRect.left, // left: Single;
                  LDstRect.top, // top: Single;
                  LDstRect.right, // right: Single;
                  LDstRect.bottom, // bottom: Single,
                  _Paint);
              end;
              _UpdatePaint(
                LExtendedTextElement.FontFamily,
                LExtendedTextElement.FontSize,
                LExtendedTextElement.FontWeight,
                LExtendedTextElement.FontSlant,
                LExtendedTextElement.FontStretch,
                LExtendedTextElement.FontColor,
                LExtendedTextElement.DecorationKinds,
                LExtendedTextElement.DecorationStyle,
                LExtendedTextElement.DecorationThicknessMultiplier,
                LExtendedTextElement.DecorationColor,
                LExtendedTextElement.LetterSpacing);
              ACanvas.drawText(
                StringToJString(LExtendedTextElement.text){text},
                LDstRect.left {x},
                LDstRect.Top + (-1 * LExtendedTextElement.Ascent){y},
                _Paint {paint});
            end;
          end;

          {$ENDIF}
          {$ENDREGION}

          {$REGION 'IOS/MACOS'}
          {$IF defined(ALAppleOS)}

          var LGridHeight := CGBitmapContextGetHeight(ACanvas);
          for i := 0 to LExtendedTextElements.count - 1 do begin
            var LExtendedTextElement := LExtendedTextElements[i];
            if LExtendedTextElement.imgSrc <> '' then begin
              Var LDstRect := LExtendedTextElement.Rect;
              LDstRect.Offset(LParagraphRect.TopLeft);
              var LSrcRect := TRectF.Create(0,0,LDstRect.Width, LDstRect.Height);
              var LImg := ALLoadFromResourceAndStretchToCGImageRef(LExtendedTextElement.imgSrc, LDstRect.Width, LDstRect.Height);
              try
                CGContextDrawImage(
                  ACanvas, // c: The graphics context in which to draw the image.
                  ALLowerLeftCGRect(
                    TPointF.Create(LDstRect.left, LDstRect.top),
                    LDstRect.Width,
                    LDstRect.Height,
                    LGridHeight), // rect The location and dimensions in user space of the bounding box in which to draw the image.
                  LImg); // image The image to draw.
              finally
                CGImageRelease(LImg);
              end;
            end
            else begin
              Var LDstRect := LExtendedTextElement.Rect;
              LDstRect.Offset(LParagraphRect.TopLeft);
              If LExtendedTextElement.BackgroundColor <> Talphacolors.Null then begin
                var LAlphaColor := TAlphaColorCGFloat.Create(LExtendedTextElement.BackgroundColor);
                CGContextSetRGBFillColor(ACanvas, LAlphaColor.R, LAlphaColor.G, LAlphaColor.B, LAlphaColor.A);
                CGContextFillRect(
                  ACanvas,
                  ALLowerLeftCGRect(
                    LDstRect.TopLeft,
                    LDstRect.Width,
                    LDstRect.Height,
                    LGridHeight));
              end;
              var LAttributedString := _CreateAttributedString(
                                         LExtendedTextElement.Text, //const AText: String;
                                         LExtendedTextElement.FontFamily, //const AFontFamily: String;
                                         LExtendedTextElement.FontSize, //const AFontSize: single;
                                         LExtendedTextElement.FontWeight, //const AFontWeight: TFontWeight;
                                         LExtendedTextElement.FontSlant, //const AFontSlant: TFontSlant;
                                         LExtendedTextElement.FontStretch, //const AFontStretch: TFontStretch;
                                         LExtendedTextElement.FontColor, //const AFontColor: TalphaColor;
                                         LExtendedTextElement.DecorationKinds, //const ADecorationKinds: TALTextDecorationKinds;
                                         LExtendedTextElement.DecorationStyle, //const ADecorationStyle: TALTextDecorationStyle;
                                         LExtendedTextElement.DecorationThicknessMultiplier, //const ADecorationThicknessMultiplier: Single;
                                         LExtendedTextElement.DecorationColor, //const ADecorationColor: TAlphaColor;
                                         LExtendedTextElement.LetterSpacing, //const ALetterSpacing: Single
                                         LOptions.Direction); // const ADirection: TALTextDirection
              try
                var LLine := CTLineCreateWithAttributedString(CFAttributedStringRef(LAttributedString));
                try
                  CGContextSetTextPosition(
                    ACanvas,
                    LDstRect.left {x},
                    LGridHeight - (LDstRect.Top + (-1 * LExtendedTextElement.Ascent)));{y}
                  CTLineDraw(LLine, ACanvas); // Draws a complete line.
                finally
                  CFRelease(LLine);
                end;
              finally
                CFRelease(LAttributedString);
              end;
            end;
          end;

          {$ENDIF}
          {$ENDREGION}

          {$REGION 'MSWINDOWS'}
          {$IF defined(MSWINDOWS)}

          ACanvas.Fill.Kind := TBrushKind.Solid;
          for i := 0 to LExtendedTextElements.count - 1 do begin
            var LExtendedTextElement := LExtendedTextElements[i];
            if LExtendedTextElement.imgSrc <> '' then begin
              Var LDstRect := LExtendedTextElement.Rect;
              LDstRect.Offset(LParagraphRect.TopLeft);
              var LSrcRect := TRectF.Create(0,0,LDstRect.Width, LDstRect.Height);
              var LImg := ALLoadFromResourceAndStretchToBitmap(LExtendedTextElement.imgSrc, LDstRect.Width, LDstRect.Height);
              try
                ACanvas.drawBitmap(
                  LImg,
                  LSrcRect,
                  LDstRect,
                  1{AOpacity},
                  false{HighSpeed});
              finally
                ALFreeAndNil(LImg);
              end;
            end
            else begin
              Var LDstRect := LExtendedTextElement.Rect;
              LDstRect.Offset(LParagraphRect.TopLeft);
              ACanvas.Font.Family := _getFontFamily(LExtendedTextElement.FontFamily);
              ACanvas.Font.Size := LExtendedTextElement.FontSize;
              ACanvas.Font.StyleExt := _getFontStyleExt(LExtendedTextElement.FontWeight, LExtendedTextElement.FontSlant, LExtendedTextElement.FontStretch, LExtendedTextElement.DecorationKinds);
              If LExtendedTextElement.BackgroundColor <> Talphacolors.Null then begin
                ACanvas.Fill.Color := LExtendedTextElement.BackgroundColor;
                ACanvas.FillRect(
                  LDstRect,
                  1{AOpacity});
              end;
              ACanvas.Fill.Color := LExtendedTextElement.FontColor;
              var LFlags: TFillTextFlags;
              if LOptions.Direction = TALTextDirection.RightToLeft then
                LFlags := [TFillTextFlag.RightToLeft]
              else
                LFlags := [];
              LDstRect.Offset(0, LExtendedTextElement.DrawTextOffsetY);
              // It appears that FillText includes the default font leading,
              // which is not included in LDstRect.
              ACanvas.FillText(
                LDstRect, // const ARect: TRectF;
                LExtendedTextElement.Text, // const AText: string;
                False, // const WordWrap: Boolean;
                1, // const AOpacity: Single;
                LFlags, // const Flags: TFillTextFlags;
                TTextAlign.Leading, TTextAlign.Leading);// const ATextAlign, AVTextAlign: TTextAlign
            end;
          end;

          {$ENDIF}
          {$ENDREGION}

        finally
          // Remove the alpha layer
          if compareValue(LOptions.Opacity, 1, Tepsilon.Scale) < 0 then begin

            {$REGION 'ANDROID'}
            {$IF defined(ANDROID)}
            ACanvas.restore;
            {$ENDIF}
            {$ENDREGION}

            {$REGION 'IOS/MACOS'}
            {$IF defined(ALAppleOS)}
            CGContextEndTransparencyLayer(ACanvas);
            CGContextRestoreGState(ACanvas)
            {$ENDIF}
            {$ENDREGION}

          end;
        end;

      finally
        ALCanvasEndScene(ACanvas);
      end;

    finally
      ALFreeAndNil(LFontFamilies);
      ALFreeAndNil(LFontSizes);
      ALFreeAndNil(LFontWeights);
      ALFreeAndNil(LFontSlants);
      ALFreeAndNil(LFontStretchs);
      ALFreeAndNil(LFontColors);
      ALFreeAndNil(LDecorationKinds);
      ALFreeAndNil(LDecorationStyles);
      ALFreeAndNil(LDecorationThicknessMultipliers);
      ALFreeAndNil(LDecorationColors);
      ALFreeAndNil(LBackGroundColors);
      ALFreeAndNil(LLineHeightMultipliers);
      ALFreeAndNil(LLetterSpacings);
      ALFreeAndNil(LSpanIDs);
      ALFreeAndNil(LExtendedTextElements);
      {$IF defined(ANDROID)}
      _Paint := nil;
      {$ENDIF}
    end;

    {$ENDIF}
    {$ENDREGION}

  finally
    ALFreeAndNil(LOptions);
    ARect.Top := ARect.Top / LScale;
    ARect.right := ARect.right / LScale;
    ARect.left := ARect.left / LScale;
    ARect.bottom := ARect.bottom / LScale;
  end;
end;

{****************************}
procedure ALDrawMultiLineText(
            var ASurface: TALSurface; // If nil and AOnlyMeasure is false, a new surface will be created
            var ACanvas: TALCanvas; // If nil and AOnlyMeasure is false, a new canvas will be created
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
            const AOptions: TALMultiLineTextOptions;
            const AOnlyMeasure: Boolean = False);
begin
  var LElements: TALTextElements;
  ALDrawMultiLineText(
    ASurface,
    ACanvas,
    AText,
    ARect,
    ATextBroken,
    AAllTextDrawn,
    LElements,
    AOptions,
    AOnlyMeasure);
end;

{****************************}
procedure ALDrawMultiLineText(
            var ASurface: TALSurface; // If nil and AOnlyMeasure is false, a new surface will be created
            var ACanvas: TALCanvas; // If nil and AOnlyMeasure is false, a new canvas will be created
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            const AOptions: TALMultiLineTextOptions;
            const AOnlyMeasure: Boolean = False);
begin
  var LElements: TALTextElements;
  var LAllTextDrawn: boolean;
  ALDrawMultiLineText(
    ASurface,
    ACanvas,
    AText,
    ARect,
    ATextBroken,
    LAllTextDrawn,
    LElements,
    AOptions,
    AOnlyMeasure);
end;

{****************************}
procedure ALDrawMultiLineText(
            var ASurface: TALSurface; // If nil and AOnlyMeasure is false, a new surface will be created
            var ACanvas: TALCanvas; // If nil and AOnlyMeasure is false, a new canvas will be created
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            const AOptions: TALMultiLineTextOptions;
            const AOnlyMeasure: Boolean = False);
begin
  var LElements: TALTextElements;
  var LTextBroken: boolean;
  var LAllTextDrawn: boolean;
  ALDrawMultiLineText(
    ASurface,
    ACanvas,
    AText,
    ARect,
    LTextBroken,
    LAllTextDrawn,
    LElements,
    AOptions,
    AOnlyMeasure);
end;

{*******************************}
procedure ALMeasureMultiLineText(
            const AText: String; // When AOptions.TextIsHtml is set to true, the HTML tags supported are described in the TALMultiLineTextOptions.TextIsHtml field declaration.
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
            out AElements: TALTextElements; // out => The list of rectangles describing all span elements.
            const AOptions: TALMultiLineTextOptions);
begin
  var LSurface: TALSurface := ALNullSurface;
  var LCanvas: TALCanvas := ALNullCanvas;
  ALDrawMultiLineText(
    LSurface,
    LCanvas,
    AText,
    ARect,
    ATextBroken,
    AAllTextDrawn,
    AElements,
    AOptions,
    true{AOnlyMeasure});
end;

{*******************************}
procedure ALMeasureMultiLineText(
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
            const AOptions: TALMultiLineTextOptions);
begin
  var LElements: TALTextElements;
  ALMeasureMultiLineText(
    AText,
    ARect,
    ATextBroken,
    AAllTextDrawn,
    LElements,
    AOptions);
end;

{*******************************}
procedure ALMeasureMultiLineText(
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
            const AOptions: TALMultiLineTextOptions);
begin
  var LElements: TALTextElements;
  var LAllTextDrawn: boolean;
  ALMeasureMultiLineText(
    AText,
    ARect,
    ATextBroken,
    LAllTextDrawn,
    LElements,
    AOptions);
end;

{*******************************}
procedure ALMeasureMultiLineText(
            const AText: String;
            var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                               // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                               // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                               // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
            const AOptions: TALMultiLineTextOptions);
begin
  var LElements: TALTextElements;
  var LTextBroken: boolean;
  var LAllTextDrawn: boolean;
  ALMeasureMultiLineText(
    AText,
    ARect,
    LTextBroken,
    LAllTextDrawn,
    LElements,
    AOptions);
end;

{*************************************}
function ALCreateMultiLineTextDrawable(
           const AText: String; // When AOptions.TextIsHtml is set to true, the HTML tags supported are described in the TALMultiLineTextOptions.TextIsHtml field declaration.
           var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                              // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                              // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                              // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
           out AElements: TALTextElements; // out => The list of rectangles describing all span elements.
           const AOptions: TALMultiLineTextOptions): TALDrawable;
begin
  var LSurface: TALSurface := ALNullSurface;
  var LCanvas: TALCanvas := ALNullCanvas;
  ALDrawMultiLineText(
    LSurface,
    LCanvas,
    AText,
    ARect,
    ATextBroken,
    AAllTextDrawn,
    AElements,
    AOptions);
  if not ALIsSurfaceNull(LSurface) then begin
    try
      result := ALCreateDrawableFromSurface(LSurface);
    finally
      ALFreeAndNilSurface(LSurface, LCanvas);
    end;
  end
  else result := ALNullDrawable;
end;

{*************************************}
function ALCreateMultiLineTextDrawable(
           const AText: String;
           var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                              // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                              // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                              // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           out AAllTextDrawn: boolean; // out => Returns true if all the text was drawn.
           const AOptions: TALMultiLineTextOptions): TALDrawable;
begin
  var LElements: TALTextElements;
  result := ALCreateMultiLineTextDrawable(
              AText,
              ARect,
              ATextBroken,
              AAllTextDrawn,
              LElements,
              AOptions);
end;

{*************************************}
function ALCreateMultiLineTextDrawable(
           const AText: String;
           var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                              // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                              // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                              // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
           out ATextBroken: boolean; // out => Returns true if the text has been broken into several lines.
           const AOptions: TALMultiLineTextOptions): TALDrawable;
begin
  var LElements: TALTextElements;
  var LAllTextDrawn: boolean;
  result := ALCreateMultiLineTextDrawable(
              AText,
              ARect,
              ATextBroken,
              LAllTextDrawn,
              LElements,
              AOptions);
end;

{*************************************}
function ALCreateMultiLineTextDrawable(
           const AText: String;
           var ARect: TRectF; // In: Defines the constraint boundaries in real pixels within which the text must fit.
                              // Out: Updated to the calculated rectangle that snugly contains the text, also in real pixels.
                              // Note: The shadow effect is not considered within the constraint boundaries and is also not included in the calculated rectangle's dimensions.
                              // However, the calculated rectangle is offset by the shadow's dx and dy values, if a shadow is applied, to adjust for the visual shift caused by the shadow.
           const AOptions: TALMultiLineTextOptions): TALDrawable;
begin
  var LElements: TALTextElements;
  var LTextBroken: boolean;
  var LAllTextDrawn: boolean;
  result := ALCreateMultiLineTextDrawable(
              AText,
              ARect,
              LTextBroken,
              LAllTextDrawn,
              LElements,
              AOptions);
end;

{*******************************************************************************************************}
function ALGetTextElementsByID(Const ATextElements: TALTextElements; Const AId: String): TALTextElements;
begin
  Setlength(Result, length(ATextElements));
  Var I := 0;
  For var J := Low(ATextElements) to high(ATextElements) do
    if ATextElements[J].Id = AId then begin
      Result[I] := ATextElements[J];
      inc(I);
    end;
  Setlength(Result, I);
end;

end.
